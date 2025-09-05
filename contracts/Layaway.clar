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
(define-constant ERR_TEMPLATE_NOT_FOUND (err u118))
(define-constant ERR_INSTALLMENT_NOT_FOUND (err u119))
(define-constant ERR_INSTALLMENT_ALREADY_PAID (err u120))
(define-constant ERR_WRONG_INSTALLMENT_AMOUNT (err u121))
(define-constant ERR_INSTALLMENT_NOT_DUE (err u122))
(define-constant ERR_TEMPLATE_ALREADY_EXISTS (err u123))
(define-constant ERR_INVALID_INSTALLMENT_COUNT (err u124))
(define-constant ERR_INSTALLMENT_OVERDUE (err u125))
(define-constant ERR_GIFT_CARD_NOT_FOUND (err u126))
(define-constant ERR_GIFT_CARD_EXPIRED (err u127))
(define-constant ERR_GIFT_CARD_INSUFFICIENT_BALANCE (err u128))
(define-constant ERR_GIFT_CARD_ALREADY_USED (err u129))
(define-constant ERR_GIFT_CARD_NOT_TRANSFERABLE (err u130))
(define-constant ERR_VOUCHER_NOT_FOUND (err u131))
(define-constant ERR_VOUCHER_ITEM_MISMATCH (err u132))
(define-constant ERR_VOUCHER_ALREADY_REDEEMED (err u133))
(define-constant GIFT_CARD_MAX_VALIDITY_PERIOD u52560)  ;; ~1 year in blocks

(define-data-var next-item-id uint u1)
(define-data-var next-gift-card-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var dispute-period uint u144)
(define-data-var arbitration-fee uint u1000000)
(define-data-var next-template-id uint u1)
(define-data-var late-payment-penalty-rate uint u5)
(define-data-var early-payment-bonus-rate uint u2)

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

(define-map payment-templates
  { template-id: uint }
  {
    seller: principal,
    template-name: (string-ascii 50),
    total-price: uint,
    installment-count: uint,
    installment-amount: uint,
    first-payment-due: uint,
    payment-interval: uint,
    late-penalty-enabled: bool,
    early-bonus-enabled: bool,
    active: bool,
    created-at: uint
  }
)

(define-map layaway-schedules
  { item-id: uint }
  {
    template-id: uint,
    current-installment: uint,
    completed-installments: uint,
    next-due-date: uint,
    total-penalties: uint,
    total-bonuses: uint
  }
)

(define-map installment-payments
  { item-id: uint, installment-number: uint }
  {
    amount-due: uint,
    amount-paid: uint,
    due-date: uint,
    paid-date: (optional uint),
    penalty-applied: uint,
    bonus-applied: uint,
    status: (string-ascii 20)
  }
)

(define-map template-usage
  { template-id: uint }
  {
    times-used: uint,
    total-value: uint,
    completion-rate: uint
  }
)

;; Gift Card System Maps
(define-map gift-cards
  { gift-card-id: uint }
  {
    creator: principal,
    holder: principal,
    balance: uint,
    original-amount: uint,
    created-at: uint,
    expires-at: uint,
    transferable: bool,
    message: (string-ascii 100),
    active: bool
  }
)

(define-map item-vouchers
  { voucher-id: uint }
  {
    creator: principal,
    item-id: uint,
    voucher-amount: uint,
    redeemed: bool,
    redeemed-by: (optional principal),
    redeemed-at: (optional uint),
    created-at: uint,
    expires-at: uint,
    message: (string-ascii 100)
  }
)

(define-map gift-card-usage
  { gift-card-id: uint, usage-id: uint }
  {
    item-id: uint,
    amount-used: uint,
    used-by: principal,
    used-at: uint
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

(define-public (create-payment-template (template-name (string-ascii 50)) (total-price uint) (installment-count uint) (first-payment-due uint) (payment-interval uint) (late-penalty-enabled bool) (early-bonus-enabled bool))
  (let
    (
      (template-id (var-get next-template-id))
      (current-block stacks-block-height)
      (installment-amount (/ total-price installment-count))
    )
    (asserts! (> total-price u0) ERR_INVALID_AMOUNT)
    (asserts! (and (> installment-count u0) (<= installment-count u12)) ERR_INVALID_INSTALLMENT_COUNT)
    (asserts! (> first-payment-due current-block) ERR_PAYMENT_DEADLINE_PASSED)
    (asserts! (> payment-interval u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? payment-templates { template-id: template-id })) ERR_TEMPLATE_ALREADY_EXISTS)
    
    (map-set payment-templates
      { template-id: template-id }
      {
        seller: tx-sender,
        template-name: template-name,
        total-price: total-price,
        installment-count: installment-count,
        installment-amount: installment-amount,
        first-payment-due: first-payment-due,
        payment-interval: payment-interval,
        late-penalty-enabled: late-penalty-enabled,
        early-bonus-enabled: early-bonus-enabled,
        active: true,
        created-at: current-block
      }
    )
    
    (map-set template-usage
      { template-id: template-id }
      {
        times-used: u0,
        total-value: u0,
        completion-rate: u0
      }
    )
    
    (var-set next-template-id (+ template-id u1))
    (ok template-id)
  )
)

(define-public (create-layaway-with-template (buyer principal) (item-name (string-ascii 50)) (template-id uint))
  (let
    (
      (template-data (unwrap! (map-get? payment-templates { template-id: template-id }) ERR_TEMPLATE_NOT_FOUND))
      (item-id (var-get next-item-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get seller template-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get active template-data) ERR_TEMPLATE_NOT_FOUND)
    (asserts! (is-none (map-get? layaway-items { item-id: item-id })) ERR_ITEM_ALREADY_EXISTS)
    
    (map-set layaway-items
      { item-id: item-id }
      {
        seller: tx-sender,
        buyer: buyer,
        item-name: item-name,
        total-price: (get total-price template-data),
        amount-paid: u0,
        payment-deadline: (+ (get first-payment-due template-data) (* (get payment-interval template-data) (get installment-count template-data))),
        claimed: false,
        created-at: current-block
      }
    )
    
    (map-set layaway-schedules
      { item-id: item-id }
      {
        template-id: template-id,
        current-installment: u1,
        completed-installments: u0,
        next-due-date: (get first-payment-due template-data),
        total-penalties: u0,
        total-bonuses: u0
      }
    )
    
    (map-set installment-payments { item-id: item-id, installment-number: u1 } {
      amount-due: (get installment-amount template-data),
      amount-paid: u0,
      due-date: (get first-payment-due template-data),
      paid-date: none,
      penalty-applied: u0,
      bonus-applied: u0,
      status: "pending"
    })
    
    (map-set user-items { user: buyer, item-id: item-id } { exists: true })
    (map-set user-items { user: tx-sender, item-id: item-id } { exists: true })
    
    (map-set template-usage
      { template-id: template-id }
      (merge (unwrap-panic (map-get? template-usage { template-id: template-id })) {
        times-used: (+ (get times-used (unwrap-panic (map-get? template-usage { template-id: template-id }))) u1),
        total-value: (+ (get total-value (unwrap-panic (map-get? template-usage { template-id: template-id }))) (get total-price template-data))
      })
    )
    
    (var-set next-item-id (+ item-id u1))
    (ok item-id)
  )
)

(define-public (make-installment-payment (item-id uint) (installment-number uint))
  (let
    (
      (item-data (unwrap! (map-get? layaway-items { item-id: item-id }) ERR_ITEM_NOT_FOUND))
      (schedule-data (unwrap! (map-get? layaway-schedules { item-id: item-id }) ERR_ITEM_NOT_FOUND))
      (installment-data (unwrap! (map-get? installment-payments { item-id: item-id, installment-number: installment-number }) ERR_INSTALLMENT_NOT_FOUND))
      (template-data (unwrap! (map-get? payment-templates { template-id: (get template-id schedule-data) }) ERR_TEMPLATE_NOT_FOUND))
      (current-block stacks-block-height)
      (is-overdue (> current-block (get due-date installment-data)))
      (is-early (< current-block (get due-date installment-data)))
      (penalty-amount (if (and is-overdue (get late-penalty-enabled template-data)) 
        (/ (* (get amount-due installment-data) (var-get late-payment-penalty-rate)) u100) 
        u0))
      (bonus-amount (if (and is-early (get early-bonus-enabled template-data)) 
        (/ (* (get amount-due installment-data) (var-get early-payment-bonus-rate)) u100) 
        u0))
      (total-payment (+ (get amount-due installment-data) penalty-amount))
      (effective-payment (- total-payment bonus-amount))
    )
    (asserts! (is-eq tx-sender (get buyer item-data)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get claimed item-data)) ERR_ITEM_ALREADY_CLAIMED)
    (asserts! (is-eq (get status installment-data) "pending") ERR_INSTALLMENT_ALREADY_PAID)
    (asserts! (is-eq installment-number (get current-installment schedule-data)) ERR_INSTALLMENT_NOT_DUE)
    
    (try! (stx-transfer? effective-payment tx-sender (get seller item-data)))
    
    (map-set installment-payments
      { item-id: item-id, installment-number: installment-number }
      (merge installment-data {
        amount-paid: effective-payment,
        paid-date: (some current-block),
        penalty-applied: penalty-amount,
        bonus-applied: bonus-amount,
        status: "paid"
      })
    )
    
    (map-set layaway-schedules
      { item-id: item-id }
      (merge schedule-data {
        current-installment: (+ (get current-installment schedule-data) u1),
        completed-installments: (+ (get completed-installments schedule-data) u1),
        next-due-date: (if (< (+ installment-number u1) (get installment-count template-data))
          (+ (get due-date installment-data) (get payment-interval template-data))
          (get due-date installment-data)),
        total-penalties: (+ (get total-penalties schedule-data) penalty-amount),
        total-bonuses: (+ (get total-bonuses schedule-data) bonus-amount)
      })
    )
    
    (map-set layaway-items
      { item-id: item-id }
      (merge item-data {
        amount-paid: (+ (get amount-paid item-data) effective-payment)
      })
    )
    
    (ok effective-payment)
  )
)

(define-public (toggle-template-status (template-id uint))
  (let
    (
      (template-data (unwrap! (map-get? payment-templates { template-id: template-id }) ERR_TEMPLATE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get seller template-data)) ERR_NOT_AUTHORIZED)
    
    (map-set payment-templates
      { template-id: template-id }
      (merge template-data { active: (not (get active template-data)) })
    )
    
    (ok (not (get active template-data)))
  )
)

(define-public (set-penalty-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-rate u20) ERR_INVALID_AMOUNT)
    (var-set late-payment-penalty-rate new-rate)
    (ok true)
  )
)

(define-public (set-bonus-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-rate u10) ERR_INVALID_AMOUNT)
    (var-set early-payment-bonus-rate new-rate)
    (ok true)
  )
)

(define-read-only (get-payment-template (template-id uint))
  (map-get? payment-templates { template-id: template-id })
)

(define-read-only (get-layaway-schedule (item-id uint))
  (map-get? layaway-schedules { item-id: item-id })
)

(define-read-only (get-installment-payment (item-id uint) (installment-number uint))
  (map-get? installment-payments { item-id: item-id, installment-number: installment-number })
)

(define-read-only (get-template-usage (template-id uint))
  (map-get? template-usage { template-id: template-id })
)

(define-read-only (get-next-template-id)
  (ok (var-get next-template-id))
)

(define-read-only (get-penalty-rate)
  (ok (var-get late-payment-penalty-rate))
)

(define-read-only (get-bonus-rate)
  (ok (var-get early-payment-bonus-rate))
)

(define-read-only (is-installment-overdue (item-id uint) (installment-number uint))
  (match (map-get? installment-payments { item-id: item-id, installment-number: installment-number })
    installment-data 
      (ok (and 
        (> stacks-block-height (get due-date installment-data))
        (is-eq (get status installment-data) "pending")
      ))
    ERR_INSTALLMENT_NOT_FOUND
  )
)

(define-read-only (get-payment-schedule-summary (item-id uint))
  (match (map-get? layaway-schedules { item-id: item-id })
    schedule-data
      (let
        (
          (template-data (unwrap! (map-get? payment-templates { template-id: (get template-id schedule-data) }) ERR_TEMPLATE_NOT_FOUND))
          (completion-percentage (/ (* (get completed-installments schedule-data) u100) (get installment-count template-data)))
        )
        (ok {
          total-installments: (get installment-count template-data),
          completed: (get completed-installments schedule-data),
          remaining: (- (get installment-count template-data) (get completed-installments schedule-data)),
          completion-percentage: completion-percentage,
          next-due: (get next-due-date schedule-data),
          total-penalties: (get total-penalties schedule-data),
          total-bonuses: (get total-bonuses schedule-data)
        })
      )
    ERR_ITEM_NOT_FOUND
  )
)

(define-read-only (can-make-installment-payment (item-id uint) (installment-number uint))
  (match (map-get? installment-payments { item-id: item-id, installment-number: installment-number })
    installment-data
      (match (map-get? layaway-schedules { item-id: item-id })
        schedule-data
          (ok (and
            (is-eq (get status installment-data) "pending")
            (is-eq installment-number (get current-installment schedule-data))
          ))
        ERR_ITEM_NOT_FOUND
      )
    ERR_INSTALLMENT_NOT_FOUND
  )
)

;; ===== GIFT CARD & VOUCHER SYSTEM =====

;; Create gift card that can be used for layaway payments
(define-public (create-gift-card 
  (recipient principal) 
  (amount uint) 
  (validity-period uint) 
  (transferable bool) 
  (message (string-ascii 100)))
  (let (
    (gift-card-id (var-get next-gift-card-id))
    (current-block stacks-block-height)
    (expires-at (+ current-block (if (<= validity-period GIFT_CARD_MAX_VALIDITY_PERIOD) validity-period GIFT_CARD_MAX_VALIDITY_PERIOD)))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> validity-period u0) ERR_INVALID_AMOUNT)
    
    ;; Transfer STX to contract for gift card funding
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set gift-cards
      { gift-card-id: gift-card-id }
      {
        creator: tx-sender,
        holder: recipient,
        balance: amount,
        original-amount: amount,
        created-at: current-block,
        expires-at: expires-at,
        transferable: transferable,
        message: message,
        active: true
      }
    )
    
    (var-set next-gift-card-id (+ gift-card-id u1))
    (ok gift-card-id)
  )
)

;; Transfer gift card to another user (if transferable)
(define-public (transfer-gift-card (gift-card-id uint) (new-recipient principal))
  (let (
    (gift-card-data (unwrap! (map-get? gift-cards { gift-card-id: gift-card-id }) ERR_GIFT_CARD_NOT_FOUND))
    (current-block stacks-block-height)
  )
    (asserts! (is-eq tx-sender (get holder gift-card-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get transferable gift-card-data) ERR_GIFT_CARD_NOT_TRANSFERABLE)
    (asserts! (get active gift-card-data) ERR_GIFT_CARD_ALREADY_USED)
    (asserts! (<= current-block (get expires-at gift-card-data)) ERR_GIFT_CARD_EXPIRED)
    (asserts! (> (get balance gift-card-data) u0) ERR_GIFT_CARD_INSUFFICIENT_BALANCE)
    
    (map-set gift-cards
      { gift-card-id: gift-card-id }
      (merge gift-card-data { holder: new-recipient })
    )
    
    (ok true)
  )
)

;; Use gift card for layaway payment
(define-public (redeem-gift-card (gift-card-id uint) (item-id uint) (amount uint))
  (let (
    (gift-card-data (unwrap! (map-get? gift-cards { gift-card-id: gift-card-id }) ERR_GIFT_CARD_NOT_FOUND))
    (item-data (unwrap! (map-get? layaway-items { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    (current-block stacks-block-height)
    (new-balance (- (get balance gift-card-data) amount))
    (new-amount-paid (+ (get amount-paid item-data) amount))
  )
    (asserts! (or 
      (is-eq tx-sender (get holder gift-card-data)) 
      (is-eq tx-sender (get buyer item-data))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (get active gift-card-data) ERR_GIFT_CARD_ALREADY_USED)
    (asserts! (<= current-block (get expires-at gift-card-data)) ERR_GIFT_CARD_EXPIRED)
    (asserts! (>= (get balance gift-card-data) amount) ERR_GIFT_CARD_INSUFFICIENT_BALANCE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= new-amount-paid (get total-price item-data)) ERR_PAYMENT_TOO_LARGE)
    (asserts! (not (get claimed item-data)) ERR_ITEM_ALREADY_CLAIMED)
    (asserts! (<= current-block (get payment-deadline item-data)) ERR_PAYMENT_DEADLINE_PASSED)
    
    ;; Transfer STX from contract to seller
    (try! (as-contract (stx-transfer? amount tx-sender (get seller item-data))))
    
    ;; Update gift card balance
    (map-set gift-cards
      { gift-card-id: gift-card-id }
      (merge gift-card-data {
        balance: new-balance,
        active: (> new-balance u0)
      })
    )
    
    ;; Update layaway item payment
    (map-set layaway-items
      { item-id: item-id }
      (merge item-data { amount-paid: new-amount-paid })
    )
    
    ;; Record gift card usage
    (map-set gift-card-usage
      { gift-card-id: gift-card-id, usage-id: current-block }
      {
        item-id: item-id,
        amount-used: amount,
        used-by: tx-sender,
        used-at: current-block
      }
    )
    
    (ok new-amount-paid)
  )
)

;; Create voucher for specific layaway item
(define-public (create-item-voucher 
  (item-id uint) 
  (voucher-amount uint) 
  (validity-period uint) 
  (message (string-ascii 100)))
  (let (
    (voucher-id (var-get next-gift-card-id))  ;; Reuse gift card ID counter
    (item-data (unwrap! (map-get? layaway-items { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    (current-block stacks-block-height)
    (expires-at (+ current-block (if (<= validity-period GIFT_CARD_MAX_VALIDITY_PERIOD) validity-period GIFT_CARD_MAX_VALIDITY_PERIOD)))
  )
    (asserts! (or 
      (is-eq tx-sender (get seller item-data)) 
      (is-eq tx-sender (get buyer item-data))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (> voucher-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= voucher-amount (- (get total-price item-data) (get amount-paid item-data))) ERR_PAYMENT_TOO_LARGE)
    (asserts! (> validity-period u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get claimed item-data)) ERR_ITEM_ALREADY_CLAIMED)
    
    ;; Transfer STX to contract for voucher funding
    (try! (stx-transfer? voucher-amount tx-sender (as-contract tx-sender)))
    
    (map-set item-vouchers
      { voucher-id: voucher-id }
      {
        creator: tx-sender,
        item-id: item-id,
        voucher-amount: voucher-amount,
        redeemed: false,
        redeemed-by: none,
        redeemed-at: none,
        created-at: current-block,
        expires-at: expires-at,
        message: message
      }
    )
    
    (var-set next-gift-card-id (+ voucher-id u1))
    (ok voucher-id)
  )
)

;; Redeem voucher for specific item
(define-public (redeem-voucher (voucher-id uint))
  (let (
    (voucher-data (unwrap! (map-get? item-vouchers { voucher-id: voucher-id }) ERR_VOUCHER_NOT_FOUND))
    (item-data (unwrap! (map-get? layaway-items { item-id: (get item-id voucher-data) }) ERR_ITEM_NOT_FOUND))
    (current-block stacks-block-height)
    (new-amount-paid (+ (get amount-paid item-data) (get voucher-amount voucher-data)))
  )
    (asserts! (is-eq tx-sender (get buyer item-data)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get redeemed voucher-data)) ERR_VOUCHER_ALREADY_REDEEMED)
    (asserts! (<= current-block (get expires-at voucher-data)) ERR_GIFT_CARD_EXPIRED)
    (asserts! (not (get claimed item-data)) ERR_ITEM_ALREADY_CLAIMED)
    (asserts! (<= current-block (get payment-deadline item-data)) ERR_PAYMENT_DEADLINE_PASSED)
    
    ;; Transfer STX from contract to seller
    (try! (as-contract (stx-transfer? (get voucher-amount voucher-data) tx-sender (get seller item-data))))
    
    ;; Mark voucher as redeemed
    (map-set item-vouchers
      { voucher-id: voucher-id }
      (merge voucher-data {
        redeemed: true,
        redeemed-by: (some tx-sender),
        redeemed-at: (some current-block)
      })
    )
    
    ;; Update layaway item payment
    (map-set layaway-items
      { item-id: (get item-id voucher-data) }
      (merge item-data { amount-paid: new-amount-paid })
    )
    
    (ok new-amount-paid)
  )
)

;; Cash out remaining gift card balance (emergency function)
(define-public (cash-out-gift-card (gift-card-id uint))
  (let (
    (gift-card-data (unwrap! (map-get? gift-cards { gift-card-id: gift-card-id }) ERR_GIFT_CARD_NOT_FOUND))
    (current-block stacks-block-height)
  )
    (asserts! (is-eq tx-sender (get holder gift-card-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get active gift-card-data) ERR_GIFT_CARD_ALREADY_USED)
    (asserts! (> (get balance gift-card-data) u0) ERR_GIFT_CARD_INSUFFICIENT_BALANCE)
    
    ;; Return remaining balance to holder
    (try! (as-contract (stx-transfer? (get balance gift-card-data) tx-sender (get holder gift-card-data))))
    
    ;; Deactivate gift card
    (map-set gift-cards
      { gift-card-id: gift-card-id }
      (merge gift-card-data {
        balance: u0,
        active: false
      })
    )
    
    (ok (get balance gift-card-data))
  )
)

;; ===== GIFT CARD & VOUCHER READ-ONLY FUNCTIONS =====

;; Get gift card details
(define-read-only (get-gift-card (gift-card-id uint))
  (map-get? gift-cards { gift-card-id: gift-card-id })
)

;; Get voucher details
(define-read-only (get-voucher (voucher-id uint))
  (map-get? item-vouchers { voucher-id: voucher-id })
)

;; Check if gift card is valid and usable
(define-read-only (is-gift-card-valid (gift-card-id uint))
  (match (map-get? gift-cards { gift-card-id: gift-card-id })
    gift-card-data
      (ok (and 
        (get active gift-card-data)
        (<= stacks-block-height (get expires-at gift-card-data))
        (> (get balance gift-card-data) u0)
      ))
    ERR_GIFT_CARD_NOT_FOUND
  )
)

;; Check if voucher is valid and redeemable
(define-read-only (is-voucher-valid (voucher-id uint))
  (match (map-get? item-vouchers { voucher-id: voucher-id })
    voucher-data
      (ok (and 
        (not (get redeemed voucher-data))
        (<= stacks-block-height (get expires-at voucher-data))
      ))
    ERR_VOUCHER_NOT_FOUND
  )
)

;; Get gift card balance and status
(define-read-only (get-gift-card-balance (gift-card-id uint))
  (match (map-get? gift-cards { gift-card-id: gift-card-id })
    gift-card-data
      (ok {
        balance: (get balance gift-card-data),
        original-amount: (get original-amount gift-card-data),
        active: (get active gift-card-data),
        expired: (> stacks-block-height (get expires-at gift-card-data))
      })
    ERR_GIFT_CARD_NOT_FOUND
  )
)

;; Get gift card usage history
(define-read-only (get-gift-card-usage (gift-card-id uint) (usage-id uint))
  (map-get? gift-card-usage { gift-card-id: gift-card-id, usage-id: usage-id })
)

;; Get next gift card ID
(define-read-only (get-next-gift-card-id)
  (ok (var-get next-gift-card-id))
)

;; Calculate how much of layaway item can be paid with gift card
(define-read-only (calculate-max-gift-card-payment (gift-card-id uint) (item-id uint))
  (match (map-get? gift-cards { gift-card-id: gift-card-id })
    gift-card-data
      (match (map-get? layaway-items { item-id: item-id })
        item-data
          (let (
            (remaining-balance (- (get total-price item-data) (get amount-paid item-data)))
            (gift-card-balance (get balance gift-card-data))
          )
            (ok (if (<= remaining-balance gift-card-balance) remaining-balance gift-card-balance))
          )
        ERR_ITEM_NOT_FOUND
      )
    ERR_GIFT_CARD_NOT_FOUND
  )
)

;; Check if gift card can be used for specific item
(define-read-only (can-use-gift-card-for-item (gift-card-id uint) (item-id uint) (amount uint))
  (match (map-get? gift-cards { gift-card-id: gift-card-id })
    gift-card-data
      (match (map-get? layaway-items { item-id: item-id })
        item-data
          (let (
            (current-block stacks-block-height)
            (remaining-balance (- (get total-price item-data) (get amount-paid item-data)))
          )
            (ok (and
              (get active gift-card-data)
              (<= current-block (get expires-at gift-card-data))
              (<= current-block (get payment-deadline item-data))
              (not (get claimed item-data))
              (>= (get balance gift-card-data) amount)
              (<= amount remaining-balance)
              (> amount u0)
            ))
          )
        ERR_ITEM_NOT_FOUND
      )
    ERR_GIFT_CARD_NOT_FOUND
  )
)



