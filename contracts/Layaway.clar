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
(define-constant ERR_DISPUTE_NOT_FOUND (err u109))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u110))
(define-constant ERR_DISPUTE_ALREADY_RESOLVED (err u111))
(define-constant ERR_NOT_DISPUTE_PARTICIPANT (err u112))
(define-constant ERR_INVALID_ARBITRATOR (err u113))
(define-constant ERR_DISPUTE_PERIOD_EXPIRED (err u114))
(define-constant ERR_ESCROW_NOT_FOUND (err u115))
(define-constant ERR_ESCROW_ALREADY_RELEASED (err u116))
(define-constant ERR_INSUFFICIENT_ESCROW_BALANCE (err u117))

(define-data-var next-item-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var dispute-period uint u144)
(define-data-var arbitration-fee uint u1000000)

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

(define-map disputes
  { dispute-id: uint }
  {
    item-id: uint,
    initiator: principal,
    respondent: principal,
    dispute-type: (string-ascii 20),
    description: (string-ascii 500),
    status: (string-ascii 20),
    created-at: uint,
    resolved-at: (optional uint),
    resolution: (optional (string-ascii 500)),
    arbitrator: (optional principal),
    escrow-amount: uint
  }
)

(define-map escrow-balances
  { dispute-id: uint }
  {
    total-amount: uint,
    released: bool,
    release-to: (optional principal)
  }
)

(define-map dispute-votes
  { dispute-id: uint, voter: principal }
  {
    vote: (string-ascii 10),
    voted-at: uint
  }
)

(define-map arbitrator-registry
  { arbitrator: principal }
  {
    active: bool,
    cases-handled: uint,
    reputation-score: uint,
    registered-at: uint
  }
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

(define-public (register-arbitrator)
  (let
    (
      (current-block stacks-block-height)
    )
    (asserts! (is-none (map-get? arbitrator-registry { arbitrator: tx-sender })) ERR_ITEM_ALREADY_EXISTS)
    
    (map-set arbitrator-registry
      { arbitrator: tx-sender }
      {
        active: true,
        cases-handled: u0,
        reputation-score: u100,
        registered-at: current-block
      }
    )
    
    (ok true)
  )
)

(define-public (create-dispute (item-id uint) (dispute-type (string-ascii 20)) (description (string-ascii 500)))
  (let
    (
      (item-data (unwrap! (map-get? layaway-items { item-id: item-id }) ERR_ITEM_NOT_FOUND))
      (dispute-id (var-get next-dispute-id))
      (current-block stacks-block-height)
      (respondent (if (is-eq tx-sender (get seller item-data)) (get buyer item-data) (get seller item-data)))
      (escrow-amount (+ (get amount-paid item-data) (var-get arbitration-fee)))
    )
    (asserts! (or (is-eq tx-sender (get seller item-data)) (is-eq tx-sender (get buyer item-data))) ERR_NOT_AUTHORIZED)
    (asserts! (> (get amount-paid item-data) u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get claimed item-data)) ERR_ITEM_ALREADY_CLAIMED)
    
    (try! (stx-transfer? (var-get arbitration-fee) tx-sender (as-contract tx-sender)))
    
    (map-set disputes
      { dispute-id: dispute-id }
      {
        item-id: item-id,
        initiator: tx-sender,
        respondent: respondent,
        dispute-type: dispute-type,
        description: description,
        status: "open",
        created-at: current-block,
        resolved-at: none,
        resolution: none,
        arbitrator: none,
        escrow-amount: escrow-amount
      }
    )
    
    (map-set escrow-balances
      { dispute-id: dispute-id }
      {
        total-amount: (get amount-paid item-data),
        released: false,
        release-to: none
      }
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (assign-arbitrator (dispute-id uint) (arbitrator principal))
  (let
    (
      (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (arbitrator-data (unwrap! (map-get? arbitrator-registry { arbitrator: arbitrator }) ERR_INVALID_ARBITRATOR))
      (current-block stacks-block-height)
    )
    (asserts! (or 
      (is-eq tx-sender (get initiator dispute-data)) 
      (is-eq tx-sender (get respondent dispute-data))
    ) ERR_NOT_DISPUTE_PARTICIPANT)
    (asserts! (is-eq (get status dispute-data) "open") ERR_DISPUTE_ALREADY_RESOLVED)
    (asserts! (get active arbitrator-data) ERR_INVALID_ARBITRATOR)
    (asserts! (<= current-block (+ (get created-at dispute-data) (var-get dispute-period))) ERR_DISPUTE_PERIOD_EXPIRED)
    
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data { 
        arbitrator: (some arbitrator),
        status: "in-arbitration"
      })
    )
    
    (ok true)
  )
)

(define-public (submit-evidence (dispute-id uint) (evidence (string-ascii 500)))
  (let
    (
      (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender (get initiator dispute-data)) 
      (is-eq tx-sender (get respondent dispute-data))
    ) ERR_NOT_DISPUTE_PARTICIPANT)
    (asserts! (not (is-eq (get status dispute-data) "resolved")) ERR_DISPUTE_ALREADY_RESOLVED)
    
    (map-set dispute-votes
      { dispute-id: dispute-id, voter: tx-sender }
      {
        vote: "evidence",
        voted-at: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (resolve-dispute (dispute-id uint) (resolution (string-ascii 500)) (winner principal))
  (let
    (
      (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (escrow-data (unwrap! (map-get? escrow-balances { dispute-id: dispute-id }) ERR_ESCROW_NOT_FOUND))
      (arbitrator (unwrap! (get arbitrator dispute-data) ERR_INVALID_ARBITRATOR))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender arbitrator) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq (get status dispute-data) "resolved")) ERR_DISPUTE_ALREADY_RESOLVED)
    (asserts! (not (get released escrow-data)) ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (or 
      (is-eq winner (get initiator dispute-data)) 
      (is-eq winner (get respondent dispute-data))
    ) ERR_NOT_DISPUTE_PARTICIPANT)
    
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data {
        status: "resolved",
        resolved-at: (some current-block),
        resolution: (some resolution)
      })
    )
    
    (map-set escrow-balances
      { dispute-id: dispute-id }
      (merge escrow-data {
        released: true,
        release-to: (some winner)
      })
    )
    
    (map-set arbitrator-registry
      { arbitrator: arbitrator }
      (merge (unwrap-panic (map-get? arbitrator-registry { arbitrator: arbitrator })) {
        cases-handled: (+ (get cases-handled (unwrap-panic (map-get? arbitrator-registry { arbitrator: arbitrator }))) u1),
        reputation-score: (+ (get reputation-score (unwrap-panic (map-get? arbitrator-registry { arbitrator: arbitrator }))) u10)
      })
    )
    
    (try! (as-contract (stx-transfer? (get total-amount escrow-data) tx-sender winner)))
    
    (ok true)
  )
)

(define-public (release-escrow (dispute-id uint))
  (let
    (
      (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (escrow-data (unwrap! (map-get? escrow-balances { dispute-id: dispute-id }) ERR_ESCROW_NOT_FOUND))
      (release-to (unwrap! (get release-to escrow-data) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (is-eq (get status dispute-data) "resolved") ERR_DISPUTE_NOT_FOUND)
    (asserts! (get released escrow-data) ERR_ESCROW_NOT_FOUND)
    (asserts! (or 
      (is-eq tx-sender (get initiator dispute-data)) 
      (is-eq tx-sender (get respondent dispute-data))
      (is-eq tx-sender release-to)
    ) ERR_NOT_AUTHORIZED)
    
    (ok true)
  )
)

(define-public (withdraw-arbitration-fee (dispute-id uint))
  (let
    (
      (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (arbitrator (unwrap! (get arbitrator dispute-data) ERR_INVALID_ARBITRATOR))
    )
    (asserts! (is-eq tx-sender arbitrator) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status dispute-data) "resolved") ERR_DISPUTE_NOT_FOUND)
    
    (try! (as-contract (stx-transfer? (var-get arbitration-fee) tx-sender arbitrator)))
    
    (ok true)
  )
)

(define-public (set-dispute-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set dispute-period new-period)
    (ok true)
  )
)

(define-public (set-arbitration-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set arbitration-fee new-fee)
    (ok true)
  )
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-escrow-balance (dispute-id uint))
  (map-get? escrow-balances { dispute-id: dispute-id })
)

(define-read-only (get-arbitrator-info (arbitrator principal))
  (map-get? arbitrator-registry { arbitrator: arbitrator })
)

(define-read-only (get-dispute-vote (dispute-id uint) (voter principal))
  (map-get? dispute-votes { dispute-id: dispute-id, voter: voter })
)

(define-read-only (get-dispute-period)
  (ok (var-get dispute-period))
)

(define-read-only (get-arbitration-fee)
  (ok (var-get arbitration-fee))
)

(define-read-only (get-next-dispute-id)
  (ok (var-get next-dispute-id))
)

(define-read-only (is-dispute-expired (dispute-id uint))
  (match (map-get? disputes { dispute-id: dispute-id })
    dispute-data (ok (> stacks-block-height (+ (get created-at dispute-data) (var-get dispute-period))))
    ERR_DISPUTE_NOT_FOUND
  )
)

(define-read-only (can-resolve-dispute (dispute-id uint))
  (match (map-get? disputes { dispute-id: dispute-id })
    dispute-data 
      (ok (and 
        (is-some (get arbitrator dispute-data))
        (not (is-eq (get status dispute-data) "resolved"))
        (<= stacks-block-height (+ (get created-at dispute-data) (var-get dispute-period)))
      ))
    ERR_DISPUTE_NOT_FOUND
  )
)