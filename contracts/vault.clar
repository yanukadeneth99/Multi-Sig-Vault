;; Owner
(define-constant contract-owner tx-sender)

;; Errors
(define-constant err-owner-only (err u100))
(define-constant err-already-locked (err u101))
(define-constant err-more-votes-than-members-required (err u102))
(define-constant err-not-a-member (err u103))
(define-constant err-votes-required-not-met (err u104))

;; Variables
(define-data-var members (list 100 principal) (list))
(define-data-var votes-required uint u1)
(define-map votes { member: principal, recipient: principal } { decision: bool })

;; Initialise the Vault
(define-public (start (new-members (list 100 principal)) (new-votes-required uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (len (var-get members)) u0) err-already-locked)
    (asserts! (>= (len new-members) new-votes-required) err-more-votes-than-members-required)
    (var-set members new-members)
    (var-set votes-required new-votes-required)
    (ok true)
  )
)