;;; Storage

;; Hashmap for keeping track of events
(define-map events uint {
    name: (string-ascii 100),
    description: (string-ascii 1000),
    type: (string-ascii 100),
    deadline: uint, ;; unix timestamp
    for-criteria: (string-ascii 100),
    against-criteria: (string-ascii 100),
})

;; Hashmap for stakes
(define-map stakes uint {staker: principal, event-id: uint, choice: bool, amount: uint})

;;; Constants
(define-data-var last-event-id uint u0)
(define-data-var last-stake-id uint u0)

;; Error messages to be defined here

;;; Externals

(define-public (create-event
    (name (string-ascii 100))
    (description (string-ascii 1000))
    (type (string-ascii 100))
    (deadline uint)
    (for-criteria (string-ascii 100))
    (against-criteria (string-ascii 100)))
    ;; need to include if statements to check inputs or make private and call from other public function
    (begin
        (map-insert events (new-event-id) {
            name: name,
            description: description,
            type: type,
            deadline: deadline,
            for-criteria: for-criteria,
            against-criteria: against-criteria,
        })
        (ok "Success")
    )
)

;; is tx-sender ok or is as-contract needed here
(define-public (create-stake (event-id uint) (choice bool) (amount uint))
    (if (is-some (map-get? events event-id)) ;; check if event exists
        ;; check that the staker is valid person and check that this event has already been staked by user
        (begin
            (map-insert stakes (new-stake-id)
                {staker: tx-sender, event-id: event-id, choice: choice, amount: amount})
            (ok "Success")
        )
        (err "Invalid event")
    )
)

;; private functions to calculate
            ;; for-total-amount: for-total-amount,
            ;; against-total-amount: against-total-amount,
            ;; for-stakers-count: for-stakers-count,
            ;; against-stakers-count: against-stakers-count

;; CRUD for events and stakes

;;; Internals

;; Return new event id after incrementing it
(define-private (new-event-id)
  (let ((event-id (+ u1 (var-get last-event-id))))
     (var-set last-event-id event-id)
     event-id))

;; Return new stake id after incrementing it
(define-private (new-stake-id)
  (let ((stake-id (+ u1 (var-get last-stake-id))))
     (var-set last-stake-id stake-id)
     stake-id))