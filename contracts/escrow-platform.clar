;; Simple Escrow Service Contract
;; A secure escrow system for transactions between buyers and sellers
;; with dispute resolution and automatic release conditions

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ESCROW-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATE (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-EXPIRED (err u104))
(define-constant ERR-NOT-EXPIRED (err u105))
(define-constant ERR-ALREADY-RESOLVED (err u106))
(define-constant ERR-INVALID-AMOUNT (err u107))
(define-constant ERR-SELF-TRANSACTION (err u108))

;; Escrow states
(define-constant STATE-FUNDED u1)
(define-constant STATE-COMPLETED u2)
(define-constant STATE-DISPUTED u3)
(define-constant STATE-CANCELLED u4)
(define-constant STATE-ARBITRATED u5)

;; Contract owner (for administrative functions)
(define-data-var contract-owner principal tx-sender)

;; Arbitrator settings
(define-data-var default-arbitrator principal tx-sender)
(define-data-var arbitration-fee uint u1000000) ;; 1 STX in microSTX

;; Escrow counter for unique IDs
(define-data-var escrow-counter uint u0)

;; Escrow data structure
(define-map escrows
  uint
  {
    buyer: principal,
    seller: principal,
    arbitrator: principal,
    amount: uint,
    state: uint,
    created-at: uint,
    expiry-block: uint,
    description: (string-ascii 256),
    buyer-confirmed: bool,
    seller-confirmed: bool
  }
)

;; Dispute information
(define-map disputes
  uint
  {
    raised-by: principal,
    raised-at: uint,
    reason: (string-ascii 512),
    arbitrator-decision: (optional bool) ;; true = buyer wins, false = seller wins, none = pending
  }
)

;; Events for off-chain tracking
(define-data-var last-event-id uint u0)

;; Public functions

;; Create a new escrow
(define-public (create-escrow
  (seller principal)
  (amount uint)
  (expiry-blocks uint)
  (description (string-ascii 256))
  (custom-arbitrator (optional principal)))
  (let (
    (escrow-id (+ (var-get escrow-counter) u1))
    (current-block burn-block-height)
    (arbitrator (default-to (var-get default-arbitrator) custom-arbitrator))
  )
    ;; Validation
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (not (is-eq tx-sender seller)) ERR-SELF-TRANSACTION)
    (asserts! (> expiry-blocks u0) ERR-INVALID-STATE)

    ;; Check if buyer has sufficient funds
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)

    ;; Transfer funds to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Create escrow record
    (map-set escrows escrow-id
      {
        buyer: tx-sender,
        seller: seller,
        arbitrator: arbitrator,
        amount: amount,
        state: STATE-FUNDED,
        created-at: current-block,
        expiry-block: (+ current-block expiry-blocks),
        description: description,
        buyer-confirmed: false,
        seller-confirmed: false
      }
    )

    ;; Update counter
    (var-set escrow-counter escrow-id)

    ;; Emit event
    (print {
      event: "escrow-created",
      escrow-id: escrow-id,
      buyer: tx-sender,
      seller: seller,
      amount: amount,
      expiry-block: (+ current-block expiry-blocks)
    })

    (ok escrow-id)
  )
)

;; Buyer confirms receipt of goods/services
(define-public (confirm-delivery (escrow-id uint))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND))
  )
    ;; Only buyer can confirm
    (asserts! (is-eq tx-sender (get buyer escrow)) ERR-NOT-AUTHORIZED)
    ;; Must be in funded state
    (asserts! (is-eq (get state escrow) STATE-FUNDED) ERR-INVALID-STATE)

    ;; Update escrow with buyer confirmation
    (map-set escrows escrow-id
      (merge escrow { buyer-confirmed: true })
    )

    ;; If seller already confirmed, complete the escrow
    (if (get seller-confirmed escrow)
      (complete-escrow escrow-id)
      (ok true)
    )
  )
)

;; Seller confirms they've delivered goods/services
(define-public (seller-confirm-delivery (escrow-id uint))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND))
  )
    ;; Only seller can confirm
    (asserts! (is-eq tx-sender (get seller escrow)) ERR-NOT-AUTHORIZED)
    ;; Must be in funded state
    (asserts! (is-eq (get state escrow) STATE-FUNDED) ERR-INVALID-STATE)

    ;; Update escrow with seller confirmation
    (map-set escrows escrow-id
      (merge escrow { seller-confirmed: true })
    )

    ;; If buyer already confirmed, complete the escrow
    (if (get buyer-confirmed escrow)
      (complete-escrow escrow-id)
      (ok true)
    )
  )
)

;; Automatic release after expiry (seller can claim)
(define-public (claim-expired-escrow (escrow-id uint))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND))
    (current-block burn-block-height)
  )
    ;; Only seller can claim expired escrow
    (asserts! (is-eq tx-sender (get seller escrow)) ERR-NOT-AUTHORIZED)
    ;; Must be in funded state
    (asserts! (is-eq (get state escrow) STATE-FUNDED) ERR-INVALID-STATE)
    ;; Must be expired
    (asserts! (>= current-block (get expiry-block escrow)) ERR-NOT-EXPIRED)
    ;; Must not be disputed
    (asserts! (is-none (map-get? disputes escrow-id)) ERR-INVALID-STATE)

    ;; Transfer funds to seller
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get seller escrow))))

    ;; Update state
    (map-set escrows escrow-id
      (merge escrow { state: STATE-COMPLETED })
    )

    ;; Emit event
    (print {
      event: "escrow-expired-claimed",
      escrow-id: escrow-id,
      seller: (get seller escrow),
      amount: (get amount escrow)
    })

    (ok true)
  )
)

;; Raise a dispute
(define-public (raise-dispute (escrow-id uint) (reason (string-ascii 512)))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND))
    (current-block burn-block-height)
  )
    ;; Only buyer or seller can raise dispute
    (asserts! (or
      (is-eq tx-sender (get buyer escrow))
      (is-eq tx-sender (get seller escrow))
    ) ERR-NOT-AUTHORIZED)
    ;; Must be in funded state
    (asserts! (is-eq (get state escrow) STATE-FUNDED) ERR-INVALID-STATE)
    ;; Must not be expired
    (asserts! (< current-block (get expiry-block escrow)) ERR-EXPIRED)
    ;; Must not already be disputed
    (asserts! (is-none (map-get? disputes escrow-id)) ERR-ALREADY-RESOLVED)

    ;; Create dispute record
    (map-set disputes escrow-id
      {
        raised-by: tx-sender,
        raised-at: current-block,
        reason: reason,
        arbitrator-decision: none
      }
    )

    ;; Update escrow state
    (map-set escrows escrow-id
      (merge escrow { state: STATE-DISPUTED })
    )

    ;; Emit event
    (print {
      event: "dispute-raised",
      escrow-id: escrow-id,
      raised-by: tx-sender,
      reason: reason
    })

    (ok true)
  )
)

;; Arbitrator resolves dispute
(define-public (resolve-dispute (escrow-id uint) (buyer-wins bool))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND))
    (dispute (unwrap! (map-get? disputes escrow-id) ERR-ESCROW-NOT-FOUND))
  )
    ;; Only arbitrator can resolve
    (asserts! (is-eq tx-sender (get arbitrator escrow)) ERR-NOT-AUTHORIZED)
    ;; Must be in disputed state
    (asserts! (is-eq (get state escrow) STATE-DISPUTED) ERR-INVALID-STATE)
    ;; Must not already be resolved
    (asserts! (is-none (get arbitrator-decision dispute)) ERR-ALREADY-RESOLVED)

    ;; Update dispute with decision
    (map-set disputes escrow-id
      (merge dispute { arbitrator-decision: (some buyer-wins) })
    )

    ;; Transfer funds based on decision
    (if buyer-wins
      ;; Refund to buyer (minus arbitration fee)
      (begin
        (try! (as-contract (stx-transfer? (var-get arbitration-fee) tx-sender (get arbitrator escrow))))
        (try! (as-contract (stx-transfer? (- (get amount escrow) (var-get arbitration-fee)) tx-sender (get buyer escrow))))
      )
      ;; Pay to seller (minus arbitration fee)
      (begin
        (try! (as-contract (stx-transfer? (var-get arbitration-fee) tx-sender (get arbitrator escrow))))
        (try! (as-contract (stx-transfer? (- (get amount escrow) (var-get arbitration-fee)) tx-sender (get seller escrow))))
      )
    )

    ;; Update escrow state
    (map-set escrows escrow-id
      (merge escrow { state: STATE-ARBITRATED })
    )

    ;; Emit event
    (print {
      event: "dispute-resolved",
      escrow-id: escrow-id,
      buyer-wins: buyer-wins,
      arbitrator: (get arbitrator escrow)
    })

    (ok true)
  )
)

;; Cancel escrow (only before expiry and if both parties agree or buyer cancels immediately)
(define-public (cancel-escrow (escrow-id uint))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND))
    (current-block burn-block-height)
  )
    ;; Must be in funded state
    (asserts! (is-eq (get state escrow) STATE-FUNDED) ERR-INVALID-STATE)
    ;; Must not be disputed
    (asserts! (is-none (map-get? disputes escrow-id)) ERR-INVALID-STATE)

    ;; Buyer can cancel within first 10 blocks, or both parties can agree anytime
    (asserts! (or
      (and (is-eq tx-sender (get buyer escrow)) (< current-block (+ (get created-at escrow) u10)))
      (and (is-eq tx-sender (get buyer escrow)) (get seller-confirmed escrow))
      (and (is-eq tx-sender (get seller escrow)) (get buyer-confirmed escrow))
    ) ERR-NOT-AUTHORIZED)

    ;; Refund to buyer
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get buyer escrow))))

    ;; Update state
    (map-set escrows escrow-id
      (merge escrow { state: STATE-CANCELLED })
    )

    ;; Emit event
    (print {
      event: "escrow-cancelled",
      escrow-id: escrow-id,
      cancelled-by: tx-sender
    })

    (ok true)
  )
)

;; Private function to complete escrow
(define-private (complete-escrow (escrow-id uint))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND))
  )
    ;; Transfer funds to seller
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get seller escrow))))

    ;; Update state
    (map-set escrows escrow-id
      (merge escrow { state: STATE-COMPLETED })
    )

    ;; Emit event
    (print {
      event: "escrow-completed",
      escrow-id: escrow-id,
      seller: (get seller escrow),
      amount: (get amount escrow)
    })

    (ok true)
  )
)

;; Read-only functions

;; Get escrow details
(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id)
)

;; Get dispute details
(define-read-only (get-dispute (escrow-id uint))
  (map-get? disputes escrow-id)
)

;; Get escrow state
(define-read-only (get-escrow-state (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow (some (get state escrow))
    none
  )
)

;; Check if escrow is expired
(define-read-only (is-escrow-expired (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow (>= burn-block-height (get expiry-block escrow))
    false
  )
)

;; Get current escrow counter
(define-read-only (get-escrow-counter)
  (var-get escrow-counter)
)

;; Get contract settings
(define-read-only (get-contract-settings)
  {
    owner: (var-get contract-owner),
    default-arbitrator: (var-get default-arbitrator),
    arbitration-fee: (var-get arbitration-fee)
  }
)

;; Administrative functions (only contract owner)

;; Update default arbitrator
(define-public (set-default-arbitrator (new-arbitrator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set default-arbitrator new-arbitrator)
    (ok true)
  )
)

;; Update arbitration fee
(define-public (set-arbitration-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set arbitration-fee new-fee)
    (ok true)
  )
)

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)
