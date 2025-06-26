;; ========================================
;; ACADEMIC RESEARCH COLLABORATION SYSTEM
;; Comprehensive Research Project Management with Funding
;; ========================================

(define-constant CONTRACT_OWNER tx-sender)

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROJECT_NOT_FOUND (err u101))
(define-constant ERR_INVALID_PARTICIPANT (err u102))
(define-constant ERR_PROJECT_CLOSED (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_INVALID_ALLOCATION (err u105))
(define-constant ERR_ALREADY_EXISTS (err u106))
(define-constant ERR_INVALID_STATUS (err u107))
(define-constant ERR_INVALID_AMOUNT (err u200))
(define-constant ERR_PAYMENT_FAILED (err u201))
(define-constant ERR_GRANT_NOT_FOUND (err u202))
(define-constant ERR_GRANT_CLOSED (err u203))
(define-constant ERR_INSUFFICIENT_BALANCE (err u204))

;; Project status constants
(define-constant STATUS_PROPOSED u0)
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_SUSPENDED u2)
(define-constant STATUS_COMPLETED u3)
(define-constant STATUS_TERMINATED u4)

;; Grant application statuses
(define-constant GRANT_SUBMITTED u0)
(define-constant GRANT_UNDER_REVIEW u1)
(define-constant GRANT_APPROVED u2)
(define-constant GRANT_REJECTED u3)
(define-constant GRANT_FUNDED u4)

;; ========================================
;; DATA STRUCTURES - RESEARCH MANAGEMENT
;; ========================================

(define-map projects
  { project-id: uint }
  {
    title: (string-ascii 256),
    description: (string-ascii 1024),
    lead-institution: principal,
    total-funding: uint,
    allocated-funding: uint,
    status: uint,
    start-block: uint,
    end-block: uint,
    created-at: uint,
    data-sharing-hash: (buff 32),
    ip-agreement-hash: (buff 32)
  }
)

(define-map project-participants
  { project-id: uint, participant: principal }
  {
    institution-name: (string-ascii 128),
    role: (string-ascii 64),
    funding-allocation: uint,
    voting-weight: uint,
    joined-at: uint,
    is-active: bool
  }
)

(define-map project-milestones
  { project-id: uint, milestone-id: uint }
  {
    description: (string-ascii 512),
    funding-amount: uint,
    target-block: uint,
    completed: bool,
    completion-block: (optional uint),
    deliverable-hash: (optional (buff 32))
  }
)

(define-map project-publications
  { project-id: uint, publication-id: uint }
  {
    title: (string-ascii 256),
    authors: (list 10 principal),
    journal: (string-ascii 128),
    doi: (string-ascii 128),
    publication-date: uint,
    ip-share: (list 10 { participant: principal, percentage: uint })
  }
)

;; ========================================
;; DATA STRUCTURES - FUNDING MANAGEMENT
;; ========================================

(define-map grant-applications
  { grant-id: uint }
  {
    project-id: uint,
    applicant: principal,
    funding-agency: (string-ascii 128),
    requested-amount: uint,
    approved-amount: uint,
    application-date: uint,
    decision-date: (optional uint),
    status: uint,
    proposal-hash: (buff 32),
    review-comments: (optional (string-ascii 512))
  }
)

(define-map funding-pools
  { project-id: uint }
  {
    total-pool: uint,
    distributed: uint,
    locked: uint,
    emergency-reserve: uint
  }
)

(define-map milestone-payments
  { project-id: uint, milestone-id: uint }
  {
    amount: uint,
    recipients: (list 10 { participant: principal, amount: uint }),
    paid: bool,
    payment-date: (optional uint)
  }
)

(define-map participant-balances
  { project-id: uint, participant: principal }
  { balance: uint, withdrawn: uint }
)

;; ========================================
;; GLOBAL COUNTERS AND AUTHORIZATION
;; ========================================

(define-data-var next-project-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var next-publication-id uint u1)
(define-data-var next-grant-id uint u1)

;; Authorization maps
(define-map authorized-institutions principal bool)
(define-map funding-agencies principal bool)

;; ========================================
;; ADMINISTRATIVE FUNCTIONS
;; ========================================

(define-public (authorize-institution (institution principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (map-set authorized-institutions institution true))
  )
)

(define-public (register-funding-agency (agency principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (map-set funding-agencies agency true))
  )
)

(define-read-only (is-authorized-institution (institution principal))
  (default-to false (map-get? authorized-institutions institution))
)

(define-read-only (is-funding-agency (agency principal))
  (default-to false (map-get? funding-agencies agency))
)

;; ========================================
;; PROJECT CREATION AND MANAGEMENT
;; ========================================

(define-public (create-project
  (title (string-ascii 256))
  (description (string-ascii 1024))
  (total-funding uint)
  (duration-blocks uint)
  (data-sharing-hash (buff 32))
  (ip-agreement-hash (buff 32))
)
  (let ((project-id (var-get next-project-id)))
    (asserts! (is-authorized-institution tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> total-funding u0) ERR_INVALID_ALLOCATION)
    (asserts! (> duration-blocks u0) ERR_INVALID_ALLOCATION)

    (map-set projects
      { project-id: project-id }
      {
        title: title,
        description: description,
        lead-institution: tx-sender,
        total-funding: total-funding,
        allocated-funding: u0,
        status: STATUS_PROPOSED,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration-blocks),
        created-at: stacks-block-height,
        data-sharing-hash: data-sharing-hash,
        ip-agreement-hash: ip-agreement-hash
      }
    )

    ;; Add lead institution as first participant
    (map-set project-participants
      { project-id: project-id, participant: tx-sender }
      {
        institution-name: "Lead Institution",
        role: "Principal Investigator",
        funding-allocation: u0,
        voting-weight: u100,
        joined-at: stacks-block-height,
        is-active: true
      }
    )

    ;; Initialize funding pool
    (map-set funding-pools
      { project-id: project-id }
      {
        total-pool: u0,
        distributed: u0,
        locked: u0,
        emergency-reserve: u0
      }
    )

    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (add-participant
  (project-id uint)
  (participant principal)
  (institution-name (string-ascii 128))
  (role (string-ascii 64))
  (funding-allocation uint)
  (voting-weight uint)
)
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get lead-institution project)) ERR_UNAUTHORIZED)
    (asserts! (is-authorized-institution participant) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? project-participants { project-id: project-id, participant: participant })) ERR_ALREADY_EXISTS)
    (asserts! (<= (+ (get allocated-funding project) funding-allocation) (get total-funding project)) ERR_INSUFFICIENT_FUNDS)

    (map-set project-participants
      { project-id: project-id, participant: participant }
      {
        institution-name: institution-name,
        role: role,
        funding-allocation: funding-allocation,
        voting-weight: voting-weight,
        joined-at: stacks-block-height,
        is-active: true
      }
    )

    (map-set projects
      { project-id: project-id }
      (merge project { allocated-funding: (+ (get allocated-funding project) funding-allocation) })
    )

    (ok true)
  )
)

(define-public (activate-project (project-id uint))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get lead-institution project)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_PROPOSED) ERR_INVALID_STATUS)

    (map-set projects
      { project-id: project-id }
      (merge project {
        status: STATUS_ACTIVE,
        start-block: stacks-block-height
      })
    )

    (ok true)
  )
)

(define-public (update-project-status (project-id uint) (new-status uint))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get lead-institution project)) ERR_UNAUTHORIZED)
    (asserts! (<= new-status STATUS_TERMINATED) ERR_INVALID_STATUS)

    (map-set projects
      { project-id: project-id }
      (merge project { status: new-status })
    )

    (ok true)
  )
)

;; ========================================
;; MILESTONE MANAGEMENT
;; ========================================

(define-public (create-milestone
  (project-id uint)
  (description (string-ascii 512))
  (funding-amount uint)
  (blocks-from-now uint)
)
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (milestone-id (var-get next-milestone-id))
    )
    (asserts! (is-eq tx-sender (get lead-institution project)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_ACTIVE) ERR_PROJECT_CLOSED)

    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      {
        description: description,
        funding-amount: funding-amount,
        target-block: (+ stacks-block-height blocks-from-now),
        completed: false,
        completion-block: none,
        deliverable-hash: none
      }
    )

    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (complete-milestone
  (project-id uint)
  (milestone-id uint)
  (deliverable-hash (buff 32))
)
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }) ERR_PROJECT_NOT_FOUND))
      (participant (unwrap! (map-get? project-participants { project-id: project-id, participant: tx-sender }) ERR_INVALID_PARTICIPANT))
    )
    (asserts! (get is-active participant) ERR_UNAUTHORIZED)
    (asserts! (not (get completed milestone)) ERR_INVALID_STATUS)

    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone {
        completed: true,
        completion-block: (some stacks-block-height),
        deliverable-hash: (some deliverable-hash)
      })
    )

    (ok true)
  )
)

;; ========================================
;; PUBLICATION MANAGEMENT
;; ========================================

(define-public (register-publication
  (project-id uint)
  (title (string-ascii 256))
  (authors (list 10 principal))
  (journal (string-ascii 128))
  (doi (string-ascii 128))
  (ip-shares (list 10 { participant: principal, percentage: uint }))
)
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (publication-id (var-get next-publication-id))
      (participant (unwrap! (map-get? project-participants { project-id: project-id, participant: tx-sender }) ERR_INVALID_PARTICIPANT))
    )
    (asserts! (get is-active participant) ERR_UNAUTHORIZED)

    (map-set project-publications
      { project-id: project-id, publication-id: publication-id }
      {
        title: title,
        authors: authors,
        journal: journal,
        doi: doi,
        publication-date: stacks-block-height,
        ip-share: ip-shares
      }
    )

    (var-set next-publication-id (+ publication-id u1))
    (ok publication-id)
  )
)

;; ========================================
;; GRANT APPLICATION MANAGEMENT
;; ========================================

(define-public (submit-grant-application
  (project-id uint)
  (funding-agency (string-ascii 128))
  (requested-amount uint)
  (proposal-hash (buff 32))
)
  (let
    (
      (grant-id (var-get next-grant-id))
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (> requested-amount u0) ERR_INVALID_AMOUNT)

    (map-set grant-applications
      { grant-id: grant-id }
      {
        project-id: project-id,
        applicant: tx-sender,
        funding-agency: funding-agency,
        requested-amount: requested-amount,
        approved-amount: u0,
        application-date: stacks-block-height,
        decision-date: none,
        status: GRANT_SUBMITTED,
        proposal-hash: proposal-hash,
        review-comments: none
      }
    )

    (var-set next-grant-id (+ grant-id u1))
    (ok grant-id)
  )
)

(define-public (review-grant-application
  (grant-id uint)
  (decision uint)
  (approved-amount uint)
  (comments (string-ascii 512))
)
  (let ((grant (unwrap! (map-get? grant-applications { grant-id: grant-id }) ERR_GRANT_NOT_FOUND)))
    (asserts! (is-funding-agency tx-sender) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq (get status grant) GRANT_SUBMITTED) (is-eq (get status grant) GRANT_UNDER_REVIEW)) ERR_GRANT_CLOSED)

    (map-set grant-applications
      { grant-id: grant-id }
      (merge grant {
        status: decision,
        approved-amount: approved-amount,
        decision-date: (some stacks-block-height),
        review-comments: (some comments)
      })
    )

    ;; If approved, add funds to project pool
    (if (is-eq decision GRANT_APPROVED)
      (add-grant-funds (get project-id grant) approved-amount)
      (ok true)
    )
  )
)

;; ========================================
;; FUNDING POOL MANAGEMENT
;; ========================================

(define-public (deposit-funds (project-id uint) (amount uint))
  (let
    (
      (pool (unwrap! (map-get? funding-pools { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    ;; In a real implementation, this would handle STX transfers

    (map-set funding-pools
      { project-id: project-id }
      (merge pool {
        total-pool: (+ (get total-pool pool) amount),
        emergency-reserve: (+ (get emergency-reserve pool) (/ amount u10))
      })
    )

    (ok true)
  )
)

(define-private (add-grant-funds (project-id uint) (amount uint))
  (let ((pool (unwrap! (map-get? funding-pools { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    (map-set funding-pools
      { project-id: project-id }
      (merge pool {
        total-pool: (+ (get total-pool pool) amount),
        emergency-reserve: (+ (get emergency-reserve pool) (/ amount u10))
      })
    )
    (ok true)
  )
)

;; ========================================
;; MILESTONE-BASED PAYMENT DISTRIBUTION
;; ========================================

(define-public (setup-milestone-payment
  (project-id uint)
  (milestone-id uint)
  (amount uint)
  (recipients (list 10 { participant: principal, amount: uint }))
)
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }) ERR_PROJECT_NOT_FOUND))
      (pool (unwrap! (map-get? funding-pools { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get lead-institution project)) ERR_UNAUTHORIZED)
    (asserts! (<= amount (- (get total-pool pool) (get distributed pool) (get emergency-reserve pool))) ERR_INSUFFICIENT_BALANCE)

    (map-set milestone-payments
      { project-id: project-id, milestone-id: milestone-id }
      {
        amount: amount,
        recipients: recipients,
        paid: false,
        payment-date: none
      }
    )

    ;; Lock the funds
    (map-set funding-pools
      { project-id: project-id }
      (merge pool { locked: (+ (get locked pool) amount) })
    )

    (ok true)
  )
)

(define-public (execute-milestone-payment (project-id uint) (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }) ERR_PROJECT_NOT_FOUND))
      (payment (unwrap! (map-get? milestone-payments { project-id: project-id, milestone-id: milestone-id }) ERR_PROJECT_NOT_FOUND))
      (pool (unwrap! (map-get? funding-pools { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (get completed milestone) ERR_INVALID_STATUS)
    (asserts! (not (get paid payment)) ERR_INVALID_STATUS)

    ;; Update payment record
    (map-set milestone-payments
      { project-id: project-id, milestone-id: milestone-id }
      (merge payment {
        paid: true,
        payment-date: (some stacks-block-height)
      })
    )

    ;; Update funding pool
    (map-set funding-pools
      { project-id: project-id }
      (merge pool {
        distributed: (+ (get distributed pool) (get amount payment)),
        locked: (- (get locked pool) (get amount payment))
      })
    )

    ;; Update participant balances
    (fold update-participant-balance (get recipients payment) project-id)

    (ok true)
  )
)

(define-private (update-participant-balance
  (recipient { participant: principal, amount: uint })
  (project-id uint)
)
  (let
    (
      (current-balance (default-to { balance: u0, withdrawn: u0 }
        (map-get? participant-balances { project-id: project-id, participant: (get participant recipient) })))
    )
    (map-set participant-balances
      { project-id: project-id, participant: (get participant recipient) }
      (merge current-balance { balance: (+ (get balance current-balance) (get amount recipient)) })
    )
    project-id ;; Return project-id to continue fold
  )
)

;; ========================================
;; WITHDRAWAL FUNCTIONS
;; ========================================

(define-public (withdraw-funds (project-id uint) (amount uint))
  (let
    (
      (balance (unwrap! (map-get? participant-balances { project-id: project-id, participant: tx-sender }) ERR_INSUFFICIENT_BALANCE))
    )
    (asserts! (<= amount (get balance balance)) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)

    ;; In a real implementation, this would transfer STX to tx-sender

    (map-set participant-balances
      { project-id: project-id, participant: tx-sender }
      {
        balance: (- (get balance balance) amount),
        withdrawn: (+ (get withdrawn balance) amount)
      }
    )

    (ok true)
  )
)

;; ========================================
;; READ-ONLY FUNCTIONS
;; ========================================

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-participant (project-id uint) (participant principal))
  (map-get? project-participants { project-id: project-id, participant: participant })
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
  (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-publication (project-id uint) (publication-id uint))
  (map-get? project-publications { project-id: project-id, publication-id: publication-id })
)

(define-read-only (get-grant-application (grant-id uint))
  (map-get? grant-applications { grant-id: grant-id })
)

(define-read-only (get-funding-pool (project-id uint))
  (map-get? funding-pools { project-id: project-id })
)

(define-read-only (get-participant-balance (project-id uint) (participant principal))
  (map-get? participant-balances { project-id: project-id, participant: participant })
)

(define-read-only (get-milestone-payment (project-id uint) (milestone-id uint))
  (map-get? milestone-payments { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-project-status (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project (ok (get status project))
    ERR_PROJECT_NOT_FOUND
  )
)

(define-read-only (calculate-available-funds (project-id uint))
  (match (map-get? funding-pools { project-id: project-id })
    pool (ok (- (get total-pool pool) (get distributed pool) (get locked pool) (get emergency-reserve pool)))
    ERR_PROJECT_NOT_FOUND
  )
)

(define-read-only (get-next-project-id)
  (var-get next-project-id)
)

(define-read-only (get-next-grant-id)
  (var-get next-grant-id)
)

(define-read-only (get-project-funding-summary (project-id uint))
  (let
    (
      (project (map-get? projects { project-id: project-id }))
      (pool (map-get? funding-pools { project-id: project-id }))
    )
    (match project
      proj (match pool
        p (ok {
          total-funding: (get total-funding proj),
          allocated-funding: (get allocated-funding proj),
          pool-total: (get total-pool p),
          distributed: (get distributed p),
          locked: (get locked p),
          available: (- (get total-pool p) (get distributed p) (get locked p) (get emergency-reserve p))
        })
        ERR_PROJECT_NOT_FOUND
      )
      ERR_PROJECT_NOT_FOUND
    )
  )
)
