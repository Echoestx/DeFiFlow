;; Decentralized Liquidity Mining Protocol 

(define-constant protocol-manager tx-sender)
(define-constant err-manager-only (err u100))
(define-constant err-invalid-signature (err u101))
(define-constant err-invalid-nonce (err u102))
(define-constant err-protocol-locked (err u103))
(define-constant err-unauthorized-pool (err u104))
(define-constant err-deposit-expired (err u105))
(define-constant err-insufficient-liquidity (err u106))
(define-constant err-invalid-parameter (err u107))

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

(define-data-var registry-counter uint u0)
(define-data-var lock-period uint u144) ;; Default 24 hours (144 blocks)
(define-data-var max-min-deposit uint u1000000) 
(define-data-var global-reward-rate uint u100) ;; Base reward rate (x100 for precision)

;; Read-only functions
(define-read-only (get-nonce (user principal))
    (default-to u0 (map-get? nonce-registry user)))

(define-read-only (is-locked)
    (var-get protocol-locked))

(define-read-only (get-pool-profile (pool principal))
    (map-get? liquidity-pools pool))

(define-read-only (get-deposit-details (deposit-id uint))
    (map-get? deposit-registry deposit-id))

(define-read-only (get-protocol-metrics)
    {total-deposits: (var-get registry-counter),
     is-locked: (var-get protocol-locked),
     lock-period: (var-get lock-period),
     max-min-deposit: (var-get max-min-deposit),
     global-reward-rate: (var-get global-reward-rate)})

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

(define-private (validate-principal (principal-to-check principal))
    (is-some (get-pool-profile principal-to-check)))

(define-private (validate-min-deposit (min-deposit uint))
    (<= min-deposit (var-get max-min-deposit)))

(define-private (validate-period (period uint))
    (and (> period u0) (<= period u1000)))

(define-private (validate-reward-rate (rate uint))
    (and (> rate u0) (<= rate u1000)))

;; Public functions
(define-public (register-pool (new-pool principal) (minimum-deposit uint) (reward-rate uint))
    (begin
        (asserts! (is-eq protocol-manager tx-sender) err-manager-only)
        (asserts! (not (validate-principal new-pool)) err-invalid-parameter)
        (asserts! (validate-min-deposit minimum-deposit) err-invalid-parameter)
        (asserts! (validate-reward-rate reward-rate) err-invalid-parameter)
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
        (asserts! (validate-reward-rate reward-rate) err-invalid-parameter)
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
        
        (ok rewards)))