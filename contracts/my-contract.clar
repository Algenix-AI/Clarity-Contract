;;; Storage

;; Hashmap for keeping track of events
(define-map events uint {
    name: (string-ascii 100),
    description: (string-ascii 1000),
    type: (string-ascii 100),
    deadline: uint, ;; unix timestamp
    for-criteria: (string-ascii 100),
    against-criteria: (string-ascii 100),
    for-total-amount: uint,
    against-total-amount: uint,
    for-stakers-count: uint,
    against-stakers-count: uint,
    is-closed: bool,
    result: (optional bool)
})

;; A user can only make one stake per event
;; Hashmap for stakes
(define-map stakes {event-id: uint, principal-address: principal} {choice: bool, amount: uint, paid: bool})

;;; Constants
(define-constant admin 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM) ;; replace this with the deployed wallet value
(define-data-var last-event-id uint u0) 

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
            for-total-amount: u0,
            against-total-amount: u0,
            for-stakers-count: u0,
            against-stakers-count: u0,
            is-closed: false,
            result: none
        })
        (ok "Successfully created event")
    )
)
(define-public (double-down (event-id uint) (choice bool) (amount uint))
    (begin
        (asserts! (not (is-eq none (map-get? stakes {event-id: event-id, principal-address: tx-sender}))) (err u33)) ;;check that user has indeed have had a stake previously
        (let ((stakes-data (unwrap-panic (map-get? stakes {event-id: event-id, principal-address: tx-sender}))) )
            (begin
                (asserts! (is-eq choice (get choice stakes-data)) (err u34))
                (try! (stx-transfer?
                    amount
                    tx-sender
                    admin
                ))
                (merge stakes-data {amount: (+ amount (get amount stakes-data))})
            )
        )
    (ok "Succesfully double-downed"))
)

(define-public (create-stake (event-id uint) (choice bool) (amount uint) (current-timestamp uint))
    (begin
        (asserts! (is-some (map-get? events event-id)) (err u99)) ;; check if event exists
        (asserts! (not (>= current-timestamp (get deadline (unwrap-panic (map-get? events event-id))))) (err u5))
        (asserts! (not (get is-closed (unwrap-panic (map-get? events event-id)))) (err u101))
        ;; check if deadline has passed or if event is already closed
        (begin
            ;; check that the staker is valid person and check that this event has already been staked by user
            ;; (as-contract tx-sender) ;; is tx-sender ok or is as-contract needed here
            (asserts! (is-eq true (map-insert stakes {event-id: event-id, principal-address: tx-sender} {choice: choice, amount: amount, paid: false})) (err u32))
            (let ((updated-event-values (unwrap-panic (map-get? events event-id))))
                (if choice
                    (map-set events event-id 
                        (begin
                            (try! (stx-transfer?
                                amount
                                tx-sender
                                admin
                            ))
                            (merge (merge updated-event-values { for-stakers-count: (+ u1 (get for-stakers-count updated-event-values))}) { for-total-amount: (+ amount (get for-total-amount updated-event-values))})
                        )
                    )
                    (map-set events event-id
                        (begin
                            (try! (stx-transfer?
                                    amount
                                    tx-sender
                                    admin
                            ))
                            (merge (merge updated-event-values { against-stakers-count: (+ u1 (get against-stakers-count updated-event-values))}) { against-total-amount: (+ amount (get against-total-amount updated-event-values))})
                        )
                    )
                )
            )
            (ok "Success")
        )
    )
)

;; return event stakes
(define-read-only (get-event-info (event-id uint))
    (if (is-some (map-get? events event-id)) ;; check if event exists
        (ok (map-get? events event-id))
        (err u1)
))

;; admin pays out a principal and marks the principal as paid
(define-public (make-payouts (event-id uint) (current-timestamp uint) (current-principal principal))
    (let ((staker-values (unwrap-panic (map-get? stakes {event-id: event-id, principal-address: current-principal}))))
        (let ((event-values (unwrap-panic (map-get? events event-id))))
            (begin
                (asserts! (is-some (map-get? stakes {event-id: event-id, principal-address: current-principal})) (err u1))
                ;; check if deadline has passed or if event is already closed
                (asserts! (>= current-timestamp (get deadline event-values)) (err u9))
                (asserts! (get is-closed (unwrap-panic (map-get? events event-id))) (err u5))
                (asserts! (is-eq (unwrap-panic (get result (unwrap-panic (map-get? events event-id)))) (get choice staker-values)) (err u6)) ;; checks if staker has won the event
                (asserts! (not (get paid staker-values)) (err u7)) ;; checks if staker has already been paid
                
                (map-set stakes {event-id: event-id, principal-address: current-principal} (merge staker-values {paid: true})) ;; update staker as paid
                (if (unwrap-panic (get result (unwrap-panic (map-get? events event-id))));; if the result was for, then we payout from against-total-amount
                    (try!
                        (begin 
                        (stx-transfer?
                            (unwrap-panic (calculate-staker-wins (get amount staker-values) (get for-total-amount event-values) (get against-total-amount event-values)))
                            admin
                            current-principal
                        )
                    ))
                    (try!
                        (begin 
                        (stx-transfer?
                            (unwrap-panic (calculate-staker-wins (get amount staker-values) (get against-total-amount event-values) (get for-total-amount event-values)))
                            admin
                            current-principal
                        )
                    ))
                )
                (ok 1)
            )
        )
    )
)

(define-public (admin-close-event (event-id uint) (result bool))
    (begin
        (asserts! (is-eq tx-sender admin) (err u1))
        (close-event event-id result)
        (ok u0)
))

(define-public (print-details)
    (begin
        (print admin)
        (print tx-sender)
        (print contract-caller)
        (ok "!")
    )
)

;;; Internals

;; Closes event
(define-private (close-event (event-id uint) (result bool))
    (begin 
        (let ((event-values (unwrap-panic (map-get? events event-id)))) 
        (map-set events event-id (merge (merge event-values {is-closed: true}) {result: (some result)}))
        )
    )
)

;; unused
;; Checks timestamp of block as a proxy for the current time
(define-private (check-time (deadline uint)) 
    (>= (unwrap-panic (get-block-info? time u0)) deadline)
)

;; Return new event id after incrementing it
(define-private (new-event-id)
  (let ((event-id (+ u1 (var-get last-event-id))))
     (var-set last-event-id event-id)
     event-id))

;; calculate winning proportions 
(define-private (calculate-staker-wins (staker-amount uint) (winning-total-amount uint) (losing-total-amount uint))
    (begin
    (asserts! (>= losing-total-amount u0) (err u1))
    (ok (+ staker-amount (* (/ staker-amount winning-total-amount) losing-total-amount))))
)
