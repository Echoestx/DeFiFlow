;; Decentralized Liquidity Mining Protocol - v3.0
;; Final implementation with advanced features, analytics, and governance

(define-constant protocol-manager tx-sender)
(define-constant err-manager-only (err u100))
(define-constant err-invalid-signature (err u101))
(define-constant err-invalid-nonce (err u102))
(define-constant err-protocol-locked (err u103))
(define-constant err-unauthorized-pool (err u104))
(define-constant err-deposit-expired (err u105))
(define-constant err-insufficient-liquidity (err u106))
(define-constant err-invalid-parameter (err u107))
(define-constant err-governance-rejected (err u108))
(define-constant err-invalid-reward-rate (err u109))

;; Data Variables
(define-data-var protocol-locked bool false)
(define-map nonce-registry principal uint)
(define-map liquidity-pools principal {
    performance: uint, 
    total-deposits: uint, 
    active: bool, 
    min-deposit: uint,
    reward-rate: uint,
    last-update-time: uint
})

(define-map deposit-registry 
    uint 
    {provider: principal, 
     pool-id: (string-ascii 64),
     nonce: uint,
     timestamp: uint,
     signature: (buff 65),
     amount: uint,
     processed: bool,
     rewards-claimed: uint})

(define-map governance-votes 
    uint 
    {proposal-id: uint,
     proposer: principal,
     description: (string-ascii 256),
     proposal-type: (string-ascii 64),
     value: uint,
     votes-for: uint,
     votes-against: uint,
     start-block: uint,
     end-block: uint,
     executed: bool})

(define-map user-analytics
    principal
    {total-deposits: uint,
     active-deposits: uint,
     total-rewards: uint,
     last-active: uint})

(define-map vote-registry
    {proposal-id: uint, voter: principal}
    {vote: bool})

(define-data-var registry-counter uint u0)
(define-data-var governance-counter uint u0)
(define-data-var lock-period uint u144) ;; Default 24 hours (144 blocks)
(define-data-var max-min-deposit uint u1000000) 
(define-data-var global-reward-rate uint u100) ;; Base reward rate (x100 for precision)
(define-data-var governance-threshold uint u5) ;; Minimum votes required
(define-data-var vote-duration uint u720) ;; Default 5 days (720 blocks)

;; Read-only functions
(define-read-only (get-nonce (user principal))
    (default-to u0 (map-get? nonce-registry user)))

(define-read-only (is-locked)
    (var-get protocol-locked))

(define-read-only (get-pool-profile (pool principal))
    (map-get? liquidity-pools pool))

(define-read-only (get-deposit-details (deposit-id uint))
    (map-get? deposit-registry deposit-id))

(define-read-only (get-governance-proposal (proposal-id uint))
    (map-get? governance-votes proposal-id))

(define-read-only (get-user-stats (user principal))
    (default-to 
        {total-deposits: u0, active-deposits: u0, total-rewards: u0, last-active: u0}
        (map-get? user-analytics user)))

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? vote-registry {proposal-id: proposal-id, voter: voter}))

(define-read-only (get-protocol-metrics)
    {total-deposits: (var-get registry-counter),
     total-proposals: (var-get governance-counter),
     is-locked: (var-get protocol-locked),
     lock-period: (var-get lock-period),
     max-min-deposit: (var-get max-min-deposit),
     global-reward-rate: (var-get global-reward-rate),
     governance-threshold: (var-get governance-threshold),
     vote-duration: (var-get vote-duration)})

(define-read-only (calculate-rewards (deposit-id uint))
    (let ((deposit (unwrap-panic (map-get? deposit-registry deposit-id)))
          (current-block block-height)
          (pool-data (unwrap-panic (get-pool-profile tx-sender))))
        (if (get processed deposit)
            (let ((time-staked (- current-block (get timestamp deposit)))
                  (base-reward (* (get amount deposit) (get reward-rate pool-data))))
                (/ (* base-reward time-staked) u10000))
            u0)))

;; Read-only functions for signature verification
(define-read-only (verify-signature (message (buff 32)) (signature (buff 65)) (provider principal))
    (let ((recovered-public-key (unwrap! (secp256k1-recover? message signature) false)))
        (is-eq (unwrap! (principal-of? recovered-public-key) false) provider)))

;; Private functions
(define-private (increment-nonce (user principal))
    (let ((current-nonce (get-nonce user)))
        (map-set nonce-registry 
            user 
            (+ current-nonce u1))))

(define-private (update-pool-metrics (pool principal) (deposit-amount uint))
    (let ((current-metrics (unwrap-panic (get-pool-profile pool))))
        (map-set liquidity-pools
            pool
            (merge current-metrics 
                  {performance: (+ (get performance current-metrics) u1),
                   total-deposits: (+ (get total-deposits current-metrics) deposit-amount),
                   last-update-time: block-height}))))

(define-private (update-user-analytics (user principal) (amount uint) (is-deposit bool))
    (let ((current-stats (get-user-stats user)))
        (map-set user-analytics
            user
            (merge current-stats
                  {total-deposits: (+ (get total-deposits current-stats) (if is-deposit amount u0)),
                   active-deposits: (+ (get active-deposits current-stats) (if is-deposit amount (- u0 amount))),
                   last-active: block-height}))))

(define-private (update-user-rewards (user principal) (rewards uint))
    (let ((current-stats (get-user-stats user)))
        (map-set user-analytics
            user
            (merge current-stats
                  {total-rewards: (+ (get total-rewards current-stats) rewards),
                   last-active: block-height}))))

(define-private (validate-principal (principal-to-check principal))
    (is-some (get-pool-profile principal-to-check)))

(define-private (validate-min-deposit (min-deposit uint))
    (<= min-deposit (var-get max-min-deposit)))

(define-private (validate-period (period uint))
    (and (> period u0) (<= period u1000)))

(define-private (validate-reward-rate (rate uint))
    (and (> rate u0) (<= rate u1000)))

(define-private (execute-governance-proposal (proposal-id uint))
    (let ((proposal (unwrap-panic (map-get? governance-votes proposal-id))))
        (if (and (>= (get votes-for proposal) (var-get governance-threshold))
                 (> (get votes-for proposal) (get votes-against proposal)))
            (let ((proposal-type (get proposal-type proposal))
                  (value (get value proposal)))
                (begin
                    (if (is-eq proposal-type "lock-period")
                        (var-set lock-period value)
                        (if (is-eq proposal-type "max-min-deposit")
                            (var-set max-min-deposit value)
                            (if (is-eq proposal-type "global-reward-rate")
                                (var-set global-reward-rate value)
                                (if (is-eq proposal-type "governance-threshold")
                                    (var-set governance-threshold value)
                                    (if (is-eq proposal-type "vote-duration")
                                        (var-set vote-duration value)
                                        false)))))
                    true)) ;; Always return true if we executed successfully
            false))) ;; Not enough votes

;; Public functions
(define-public (register-pool (new-pool principal) (minimum-deposit uint) (reward-rate uint))
    (begin
        (asserts! (is-eq protocol-manager tx-sender) err-manager-only)
        (asserts! (not (validate-principal new-pool)) err-invalid-parameter)
        (asserts! (validate-min-deposit minimum-deposit) err-invalid-parameter)
        (asserts! (validate-reward-rate reward-rate) err-invalid-reward-rate)
        (ok (map-set liquidity-pools
            new-pool
            {performance: u0,
             total-deposits: u0,
             active: true,
             min-deposit: minimum-deposit,
             reward-rate: reward-rate,
             last-update-time: block-height}))))

(define-public (toggle-lock)
    (begin
        (asserts! (is-eq protocol-manager tx-sender) err-manager-only)
        (ok (var-set protocol-locked (not (var-get protocol-locked))))))

(define-public (set-lock-period (new-period uint))
    (begin
        (asserts! (is-eq protocol-manager tx-sender) err-manager-only)
        (asserts! (validate-period new-period) err-invalid-parameter)
        (ok (var-set lock-period new-period))))

(define-public (set-max-min-deposit (new-max-min-deposit uint))
    (begin
        (asserts! (is-eq protocol-manager tx-sender) err-manager-only)
        (asserts! (> new-max-min-deposit u0) err-invalid-parameter)
        (ok (var-set max-min-deposit new-max-min-deposit))))

(define-public (update-pool-status (target-pool principal) (active-status bool) (minimum-deposit uint) (reward-rate uint))
    (begin
        (asserts! (is-eq protocol-manager tx-sender) err-manager-only)
        (asserts! (validate-principal target-pool) err-invalid-parameter)
        (asserts! (validate-min-deposit minimum-deposit) err-invalid-parameter)
        (asserts! (validate-reward-rate reward-rate) err-invalid-reward-rate)
        (let ((pool-data (unwrap-panic (get-pool-profile target-pool))))
            (ok (map-set liquidity-pools
                target-pool
                (merge pool-data 
                       {active: active-status,
                        min-deposit: minimum-deposit,
                        reward-rate: reward-rate,
                        last-update-time: block-height}))))))

(define-public (submit-deposit 
    (pool-id (string-ascii 64))
    (signature (buff 65))
    (amount uint))
    (let
        ((provider tx-sender)
         (current-nonce (get-nonce provider))
         (message-hash (sha256 (concat (unwrap-panic (to-consensus-buff? pool-id))
                                     (concat (unwrap-panic (to-consensus-buff? current-nonce))
                                             (unwrap-panic (to-consensus-buff? amount)))))))
        (asserts! (not (var-get protocol-locked)) err-protocol-locked)
        (asserts! (> amount u0) err-invalid-parameter)
        (asserts! (verify-signature message-hash signature provider) err-invalid-signature)
        (map-set deposit-registry
            (var-get registry-counter)
            {provider: provider,
             pool-id: pool-id,
             nonce: current-nonce,
             timestamp: block-height,
             signature: signature,
             amount: amount,
             processed: false,
             rewards-claimed: u0})
        
        ;; Update user analytics
        (update-user-analytics provider amount true)
        
        ;; Increment registry counter
        (var-set registry-counter (+ (var-get registry-counter) u1))
        (ok true)))

(define-public (process-deposit (registry-id uint))
    (let ((deposit (unwrap-panic (map-get? deposit-registry registry-id)))
          (pool tx-sender)
          (pool-data (unwrap! (get-pool-profile pool) err-unauthorized-pool))
          (current-height block-height))
        (asserts! (not (var-get protocol-locked)) err-protocol-locked)
        (asserts! (get active pool-data) err-unauthorized-pool)
        (asserts! (not (get processed deposit)) err-invalid-nonce)
        (asserts! (<= (- current-height (get timestamp deposit)) (var-get lock-period)) err-deposit-expired)
        (asserts! (>= (get amount deposit) (get min-deposit pool-data)) err-insufficient-liquidity)
        
        ;; Process the deposit
        (map-set deposit-registry
            registry-id
            (merge deposit {processed: true}))
        
        ;; Update nonce and pool stats
        (increment-nonce (get provider deposit))
        (update-pool-metrics pool (get amount deposit))
        (ok true)))

(define-public (cancel-deposit (registry-id uint))
    (let ((deposit (unwrap-panic (map-get? deposit-registry registry-id)))
          (provider tx-sender))
        (asserts! (is-eq provider (get provider deposit)) err-manager-only)
        (asserts! (not (get processed deposit)) err-invalid-nonce)
        
        ;; Cancel the deposit
        (map-set deposit-registry
            registry-id
            (merge deposit {processed: true}))
        
        ;; Update user analytics
        (update-user-analytics provider (get amount deposit) false)
        
        ;; Update nonce
        (increment-nonce provider)
        (ok true)))

(define-public (claim-rewards (deposit-id uint))
    (let ((deposit (unwrap-panic (map-get? deposit-registry deposit-id)))
          (provider tx-sender)
          (rewards (calculate-rewards deposit-id)))
        (asserts! (is-eq provider (get provider deposit)) err-manager-only)
        (asserts! (get processed deposit) err-invalid-parameter)
        
        ;; Update rewards claimed
        (map-set deposit-registry
            deposit-id
            (merge deposit {rewards-claimed: (+ (get rewards-claimed deposit) rewards)}))
        
        ;; Update user analytics
        (update-user-rewards provider rewards)
        
        (ok rewards)))

(define-public (submit-governance-proposal 
    (description (string-ascii 256))
    (proposal-type (string-ascii 64))
    (value uint))
    (begin
        (asserts! (or (is-eq proposal-type "lock-period")
                    (is-eq proposal-type "max-min-deposit")
                    (is-eq proposal-type "global-reward-rate")
                    (is-eq proposal-type "governance-threshold")
                    (is-eq proposal-type "vote-duration"))
                err-invalid-parameter)
        
        ;; Validate the value based on proposal type
        (asserts! 
            (if (is-eq proposal-type "lock-period")
                (validate-period value)
                (if (is-eq proposal-type "reward-rate")
                    (validate-reward-rate value)
                    true)) ;; Other parameters have fewer restrictions
            err-invalid-parameter)
        
        ;; Create the proposal
        (map-set governance-votes
            (var-get governance-counter)
            {proposal-id: (var-get governance-counter),
             proposer: tx-sender,
             description: description,
             proposal-type: proposal-type,
             value: value,
             votes-for: u0,
             votes-against: u0,
             start-block: block-height,
             end-block: (+ block-height (var-get vote-duration)),
             executed: false})
        
        ;; Increment the governance counter
        (var-set governance-counter (+ (var-get governance-counter) u1))
        (ok (- (var-get governance-counter) u1)))) ;; Return the proposal ID

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
    (let ((proposal (unwrap! (map-get? governance-votes proposal-id) err-invalid-parameter))
          (voter tx-sender)
          (current-height block-height))
        
        ;; Check that voting is still open
        (asserts! (< current-height (get end-block proposal)) err-deposit-expired)
        
        ;; Check that the voter hasn't already voted on this proposal
        (asserts! (is-none (get-vote proposal-id voter)) err-invalid-nonce)
        
        ;; Record the vote
        (map-set vote-registry
            {proposal-id: proposal-id, voter: voter}
            {vote: vote-for})
        
        ;; Update the vote count
        (map-set governance-votes
            proposal-id
            (merge proposal
                  {votes-for: (+ (get votes-for proposal) (if vote-for u1 u0)),
                   votes-against: (+ (get votes-against proposal) (if vote-for u0 u1))}))
        
        (ok true)))

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? governance-votes proposal-id) err-invalid-parameter))
          (current-height block-height))
        
        ;; Check that voting is closed
        (asserts! (>= current-height (get end-block proposal)) err-invalid-parameter)
        
        ;; Check that the proposal hasn't already been executed
        (asserts! (not (get executed proposal)) err-invalid-parameter)
        
        ;; Try to execute the proposal
        (asserts! (execute-governance-proposal proposal-id) err-governance-rejected)
        
        ;; Mark the proposal as executed
        (map-set governance-votes
            proposal-id
            (merge proposal {executed: true}))
        
        (ok true)))