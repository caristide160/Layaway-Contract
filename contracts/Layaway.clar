(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ITEM_NOT_FOUND (err u101))
(define-constant ERR_ITEM_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_PAYMENT_TOO_LARGE (err u104))
(define-constant ERR_ITEM_NOT_PAID_IN_FULL (err u105))
(define-constant ERR_ITEM_ALREADY_CLAIMED (err u106))
(define-constant ERR_PAYMENT_DEADLINE_PASSED (err u107))
(define-constant ERR_ITEM_STILL_ACTIVE (err u108))

(define-data-var next-item-id uint u1)

(define-map layaway-items
  { item-id: uint }
  {
    seller: principal,
    buyer: principal,
    item-name: (string-ascii 50),
    total-price: uint,
    amount-paid: uint,
    payment-deadline: uint,
    claimed: bool,
    created-at: uint
  }
)

(define-map user-items
  { user: principal, item-id: uint }
  { exists: bool }
)

(define-public (create-layaway-item (buyer principal) (item-name (string-ascii 50)) (total-price uint) (payment-deadline uint))
  (let
    (
      (item-id (var-get next-item-id))
      (current-block stacks-block-height)
    )
    (asserts! (> total-price u0) ERR_INVALID_AMOUNT)
    (asserts! (> payment-deadline current-block) ERR_PAYMENT_DEADLINE_PASSED)
    (asserts! (is-none (map-get? layaway-items { item-id: item-id })) ERR_ITEM_ALREADY_EXISTS)
    
    (map-set layaway-items
      { item-id: item-id }
      {
        seller: tx-sender,
        buyer: buyer,
        item-name: item-name,
        total-price: total-price,
        amount-paid: u0,
        payment-deadline: payment-deadline,
        claimed: false,
        created-at: current-block
      }
    )
    
    (map-set user-items { user: buyer, item-id: item-id } { exists: true })
    (map-set user-items { user: tx-sender, item-id: item-id } { exists: true })
    
    (var-set next-item-id (+ item-id u1))
    (ok item-id)
  )
)

(define-public (make-payment (item-id uint) (payment-amount uint))
  (let
    (
      (item-data (unwrap! (map-get? layaway-items { item-id: item-id }) ERR_ITEM_NOT_FOUND))
      (current-block stacks-block-height)
      (new-amount-paid (+ (get amount-paid item-data) payment-amount))
    )
    (asserts! (is-eq tx-sender (get buyer item-data)) ERR_NOT_AUTHORIZED)
    (asserts! (> payment-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= current-block (get payment-deadline item-data)) ERR_PAYMENT_DEADLINE_PASSED)
    (asserts! (<= new-amount-paid (get total-price item-data)) ERR_PAYMENT_TOO_LARGE)
    (asserts! (not (get claimed item-data)) ERR_ITEM_ALREADY_CLAIMED)
    
    (try! (stx-transfer? payment-amount tx-sender (get seller item-data)))
    
    (map-set layaway-items
      { item-id: item-id }
      (merge item-data { amount-paid: new-amount-paid })
    )
    
    (ok new-amount-paid)
  )
)

(define-public (claim-item (item-id uint))
  (let
    (
      (item-data (unwrap! (map-get? layaway-items { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get buyer item-data)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get amount-paid item-data) (get total-price item-data)) ERR_ITEM_NOT_PAID_IN_FULL)
    (asserts! (not (get claimed item-data)) ERR_ITEM_ALREADY_CLAIMED)
    
    (map-set layaway-items
      { item-id: item-id }
      (merge item-data { claimed: true })
    )
    
    (ok true)
  )
)

(define-public (cancel-layaway (item-id uint))
  (let
    (
      (item-data (unwrap! (map-get? layaway-items { item-id: item-id }) ERR_ITEM_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender (get seller item-data)) (is-eq tx-sender (get buyer item-data))) ERR_NOT_AUTHORIZED)
    (asserts! (> current-block (get payment-deadline item-data)) ERR_ITEM_STILL_ACTIVE)
    (asserts! (not (get claimed item-data)) ERR_ITEM_ALREADY_CLAIMED)
    (asserts! (< (get amount-paid item-data) (get total-price item-data)) ERR_ITEM_NOT_PAID_IN_FULL)
    
    (if (> (get amount-paid item-data) u0)
      (try! (stx-transfer? (get amount-paid item-data) (get seller item-data) (get buyer item-data)))
      true
    )
    
    (map-delete layaway-items { item-id: item-id })
    (map-delete user-items { user: (get buyer item-data), item-id: item-id })
    (map-delete user-items { user: (get seller item-data), item-id: item-id })
    
    (ok true)
  )
)

(define-read-only (get-layaway-item (item-id uint))
  (map-get? layaway-items { item-id: item-id })
)

(define-read-only (get-remaining-balance (item-id uint))
  (match (map-get? layaway-items { item-id: item-id })
    item-data (ok (- (get total-price item-data) (get amount-paid item-data)))
    ERR_ITEM_NOT_FOUND
  )
)

(define-read-only (is-fully-paid (item-id uint))
  (match (map-get? layaway-items { item-id: item-id })
    item-data (ok (is-eq (get amount-paid item-data) (get total-price item-data)))
    ERR_ITEM_NOT_FOUND
  )
)

(define-read-only (get-payment-progress (item-id uint))
  (match (map-get? layaway-items { item-id: item-id })
    item-data 
      (let
        (
          (progress (/ (* (get amount-paid item-data) u100) (get total-price item-data)))
        )
        (ok progress)
      )
    ERR_ITEM_NOT_FOUND
  )
)

(define-read-only (is-payment-deadline-passed (item-id uint))
  (match (map-get? layaway-items { item-id: item-id })
    item-data (ok (> stacks-block-height (get payment-deadline item-data)))
    ERR_ITEM_NOT_FOUND
  )
)

(define-read-only (can-claim-item (item-id uint))
  (match (map-get? layaway-items { item-id: item-id })
    item-data 
      (ok (and 
        (is-eq (get amount-paid item-data) (get total-price item-data))
        (not (get claimed item-data))
      ))
    ERR_ITEM_NOT_FOUND
  )
)

(define-read-only (get-next-item-id)
  (ok (var-get next-item-id))
)

(define-read-only (has-user-item (user principal) (item-id uint))
  (is-some (map-get? user-items { user: user, item-id: item-id }))
)