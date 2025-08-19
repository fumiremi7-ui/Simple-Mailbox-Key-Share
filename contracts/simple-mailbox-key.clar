;; Simple Mailbox Key Share
;; Temporary mail collection system for neighbors during vacations

;; Error constants
(define-constant err-not-authorized (err u100))
(define-constant err-already-exists (err u101))
(define-constant err-not-found (err u102))
(define-constant err-invalid-duration (err u103))

;; Data structures
(define-map key-shares
  { owner: principal }
  {
    helper: principal,
    start-block: uint,
    end-block: uint,
    active: bool
  }
)

(define-map package-holds
  { id: uint }
  {
    owner: principal,
    helper: principal,
    description: (string-ascii 256),
    held-at-block: uint,
    retrieved: bool
  }
)

(define-map mail-forwards
  { owner: principal, forward-id: uint }
  {
    helper: principal,
    recipient: principal,
    instructions: (string-ascii 512),
    created-at-block: uint,
    completed: bool
  }
)

;; Data variables
(define-data-var next-package-id uint u1)
(define-data-var next-forward-id uint u1)

;; Create key share arrangement
(define-public (create-key-share (helper principal) (duration-blocks uint))
  (let ((current-block stacks-block-height)
        (end-block (+ stacks-block-height duration-blocks)))
    (asserts! (> duration-blocks u0) err-invalid-duration)
    (asserts! (is-none (map-get? key-shares { owner: tx-sender })) err-already-exists)
    (ok (map-set key-shares
      { owner: tx-sender }
      {
        helper: helper,
        start-block: current-block,
        end-block: end-block,
        active: true
      }))))

;; Deactivate key share
(define-public (deactivate-key-share)
  (let ((share (unwrap! (map-get? key-shares { owner: tx-sender }) err-not-found)))
    (ok (map-set key-shares
      { owner: tx-sender }
      (merge share { active: false })))))

;; Record package hold
(define-public (hold-package (owner principal) (description (string-ascii 256)))
  (let ((package-id (var-get next-package-id))
        (share (unwrap! (map-get? key-shares { owner: owner }) err-not-found)))
    (asserts! (get active share) err-not-authorized)
    (asserts! (is-eq tx-sender (get helper share)) err-not-authorized)
    (asserts! (<= stacks-block-height (get end-block share)) err-not-authorized)
    (map-set package-holds
      { id: package-id }
      {
        owner: owner,
        helper: tx-sender,
        description: description,
        held-at-block: stacks-block-height,
        retrieved: false
      })
    (var-set next-package-id (+ package-id u1))
    (ok package-id)))

;; Mark package as retrieved
(define-public (retrieve-package (package-id uint))
  (let ((package (unwrap! (map-get? package-holds { id: package-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get owner package)) err-not-authorized)
    (ok (map-set package-holds
      { id: package-id }
      (merge package { retrieved: true })))))

;; Create mail forward instruction
(define-public (create-mail-forward (recipient principal) (instructions (string-ascii 512)))
  (let ((forward-id (var-get next-forward-id))
        (share (unwrap! (map-get? key-shares { owner: tx-sender }) err-not-found)))
    (asserts! (get active share) err-not-authorized)
    (map-set mail-forwards
      { owner: tx-sender, forward-id: forward-id }
      {
        helper: (get helper share),
        recipient: recipient,
        instructions: instructions,
        created-at-block: stacks-block-height,
        completed: false
      })
    (var-set next-forward-id (+ forward-id u1))
    (ok forward-id)))

;; Mark mail forward as completed
(define-public (complete-mail-forward (owner principal) (forward-id uint))
  (let ((forward (unwrap! (map-get? mail-forwards { owner: owner, forward-id: forward-id }) err-not-found))
        (share (unwrap! (map-get? key-shares { owner: owner }) err-not-found)))
    (asserts! (is-eq tx-sender (get helper share)) err-not-authorized)
    (ok (map-set mail-forwards
      { owner: owner, forward-id: forward-id }
      (merge forward { completed: true })))))

;; Read-only functions
(define-read-only (get-key-share (owner principal))
  (map-get? key-shares { owner: owner }))

(define-read-only (get-package (package-id uint))
  (map-get? package-holds { id: package-id }))

(define-read-only (get-mail-forward (owner principal) (forward-id uint))
  (map-get? mail-forwards { owner: owner, forward-id: forward-id }))

(define-read-only (is-active-helper (owner principal) (helper principal))
  (match (map-get? key-shares { owner: owner })
    share (and (get active share)
               (is-eq helper (get helper share))
               (<= stacks-block-height (get end-block share)))
    false))
