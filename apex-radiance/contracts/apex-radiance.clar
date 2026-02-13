;; ApexRadiance DAO Governance Contract
;; A skill-weighted governance system with dynamic committees

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-proposal-closed (err u104))
(define-constant err-insufficient-reputation (err u105))

;; Data Variables
(define-data-var proposal-count uint u0)
(define-data-var min-reputation-to-propose uint u100)
(define-data-var voting-period uint u1440) ;; blocks (~10 days)

;; Data Maps

;; Member reputation and expertise tracking
(define-map members
    principal
    {
        reputation-score: uint,
        expertise-areas: (list 5 (string-ascii 50)),
        total-votes: uint,
        successful-votes: uint,
        joined-height: uint
    }
)

;; Committee structure
(define-map committees
    uint ;; committee-id
    {
        committee-type: (string-ascii 20), ;; "expert", "community", "oversight"
        members: (list 20 principal),
        active: bool,
        created-height: uint
    }
)

;; Proposals
(define-map proposals
    uint ;; proposal-id
    {
        proposer: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        proposal-type: (string-ascii 20),
        committee-id: uint,
        start-height: uint,
        end-height: uint,
        yes-votes: uint,
        no-votes: uint,
        executed: bool,
        passed: bool
    }
)

;; Vote tracking
(define-map votes
    {proposal-id: uint, voter: principal}
    {
        vote-weight: uint,
        vote-choice: bool, ;; true = yes, false = no
        voted-height: uint
    }
)

;; Delegation tracking
(define-map delegations
    {delegator: principal, expertise-area: (string-ascii 50)}
    {
        delegate: principal,
        active: bool
    }
)

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-member (member principal))
    (map-get? members member)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-committee (committee-id uint))
    (map-get? committees committee-id)
)

(define-read-only (calculate-vote-weight (voter principal) (proposal-type (string-ascii 20)))
    (let
        (
            (member-data (unwrap! (map-get? members voter) u0))
            (base-reputation (get reputation-score member-data))
            (total-votes (get total-votes member-data))
            (successful-votes (get successful-votes member-data))
        )
        ;; Weight = reputation * (successful-votes / total-votes ratio)
        ;; Minimum weight of 1 for any registered member
        (if (is-eq total-votes u0)
            base-reputation
            (/ (* base-reputation (+ successful-votes u1)) (+ total-votes u1))
        )
    )
)

(define-read-only (get-proposal-status (proposal-id uint))
    (let
        (
            (proposal-data (unwrap! (map-get? proposals proposal-id) (err err-not-found)))
        )
        (ok {
            proposal-id: proposal-id,
            active: (and 
                (>= block-height (get start-height proposal-data))
                (<= block-height (get end-height proposal-data))
                (not (get executed proposal-data))
            ),
            yes-votes: (get yes-votes proposal-data),
            no-votes: (get no-votes proposal-data),
            passed: (get passed proposal-data),
            executed: (get executed proposal-data)
        })
    )
)

;; Public functions

;; Register as a member
(define-public (register-member (expertise-areas (list 5 (string-ascii 50))))
    (begin
        (asserts! (is-none (map-get? members tx-sender)) (err u106))
        (ok (map-set members tx-sender {
            reputation-score: u50, ;; Starting reputation
            expertise-areas: expertise-areas,
            total-votes: u0,
            successful-votes: u0,
            joined-height: block-height
        }))
    )
)

;; Create a proposal
(define-public (create-proposal 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (proposal-type (string-ascii 20))
    (committee-id uint))
    (let
        (
            (proposal-id (+ (var-get proposal-count) u1))
            (member-data (unwrap! (map-get? members tx-sender) err-unauthorized))
        )
        (asserts! (>= (get reputation-score member-data) (var-get min-reputation-to-propose)) err-insufficient-reputation)
        (var-set proposal-count proposal-id)
        (ok (map-set proposals proposal-id {
            proposer: tx-sender,
            title: title,
            description: description,
            proposal-type: proposal-type,
            committee-id: committee-id,
            start-height: block-height,
            end-height: (+ block-height (var-get voting-period)),
            yes-votes: u0,
            no-votes: u0,
            executed: false,
            passed: false
        }))
    )
)

;; Cast a vote
(define-public (vote (proposal-id uint) (vote-choice bool))
    (let
        (
            (proposal-data (unwrap! (map-get? proposals proposal-id) err-not-found))
            (member-data (unwrap! (map-get? members tx-sender) err-unauthorized))
            (vote-weight (calculate-vote-weight tx-sender (get proposal-type proposal-data)))
            (current-yes (get yes-votes proposal-data))
            (current-no (get no-votes proposal-data))
        )
        ;; Verify voting period is active
        (asserts! (>= block-height (get start-height proposal-data)) err-proposal-closed)
        (asserts! (<= block-height (get end-height proposal-data)) err-proposal-closed)
        (asserts! (not (get executed proposal-data)) err-proposal-closed)
        
        ;; Check if already voted
        (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: tx-sender})) err-already-voted)
        
        ;; Record vote
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} {
            vote-weight: vote-weight,
            vote-choice: vote-choice,
            voted-height: block-height
        })
        
        ;; Update proposal vote counts
        (map-set proposals proposal-id (merge proposal-data {
            yes-votes: (if vote-choice (+ current-yes vote-weight) current-yes),
            no-votes: (if vote-choice current-no (+ current-no vote-weight))
        }))
        
        ;; Update member vote count
        (map-set members tx-sender (merge member-data {
            total-votes: (+ (get total-votes member-data) u1)
        }))
        
        (ok true)
    )
)

;; Execute proposal after voting period
(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal-data (unwrap! (map-get? proposals proposal-id) err-not-found))
            (yes-votes (get yes-votes proposal-data))
            (no-votes (get no-votes proposal-data))
            (passed (> yes-votes no-votes))
        )
        ;; Verify voting period ended
        (asserts! (> block-height (get end-height proposal-data)) (err u107))
        (asserts! (not (get executed proposal-data)) (err u108))
        
        ;; Mark as executed
        (map-set proposals proposal-id (merge proposal-data {
            executed: true,
            passed: passed
        }))
        
        (ok passed)
    )
)

;; Delegate voting power
(define-public (delegate-vote (delegate principal) (expertise-area (string-ascii 50)))
    (begin
        (asserts! (is-some (map-get? members tx-sender)) err-unauthorized)
        (asserts! (is-some (map-get? members delegate)) err-not-found)
        (ok (map-set delegations 
            {delegator: tx-sender, expertise-area: expertise-area}
            {delegate: delegate, active: true}
        ))
    )
)

;; Update member reputation (owner only for now)
(define-public (update-reputation (member principal) (new-reputation uint))
    (let
        (
            (member-data (unwrap! (map-get? members member) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set members member (merge member-data {
            reputation-score: new-reputation
        })))
    )
)

;; Create committee
(define-public (create-committee 
    (committee-id uint)
    (committee-type (string-ascii 20))
    (committee-members (list 20 principal)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set committees committee-id {
            committee-type: committee-type,
            members: committee-members,
            active: true,
            created-height: block-height
        }))
    )
)