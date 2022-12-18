;;; Storage

;; Hashmap for keeping track of events
(define-map events-lib uint {
    name: (string-ascii 100),
    description: (string-ascii 1000),
    type: (string-ascii 100),
    deadline: uint, ;; unix timestamp
    for-criteria: (string-ascii 100),
    against-criteria: (string-ascii 100),
    for-total-amount: uint,
    against-total-amount: uint,
    for-stakers-count: uint,
    against-stakers-count: uint
})

;; Hashmap for stakers and their stakes
(define-map stakers-lib principal {
    total-amount-in-stake: uint,
    staked-events: (tuple (uint bool uint)) ;; should use a map instead
    ;; event_id, choice (true-for, false-against), amount in stake
})

(define-data-var next-event-id uint u0)

;;; Constants
;; Error messages to be defined here

;;; Internals

(define-public (create-event
    (name (string-ascii 100))
    (description (string-ascii 1000))
    (type (string-ascii 100))
    (deadline uint)
    (for-criteria (string-ascii 100))
    (against-criteria (string-ascii 100))
    (for-total-amount uint)
    (against-total-amount uint)
    (for-stakers-count uint)
    (against-stakers-count uint))
    (begin ;; need to include if statements to check inputs or make private and call from other public function
        (map-insert events-lib (var-get next-event-id) {
            name: name,
            description: description,
            type: type,
            deadline: deadline,
            for-criteria: for-criteria,
            against-criteria: against-criteria,
            for-total-amount: for-total-amount,
            against-total-amount: against-total-amount,
            for-stakers-count: for-stakers-count,
            against-stakers-count: against-stakers-count
        })
        (ok "Success")
    )
)

(define-public (stake-event (event-id uint) (choice bool) (amount uint))
    (let staker-details (map-get? stakers-lib tx-sender) ;;tx-sender is the principal address as key
        ;;check if this event has already been staked by user
            ;; true -> add to the stake amount
            ;; false -> insert new event into staked-events
        ;;add new stake to total-amount-in-stake
)
