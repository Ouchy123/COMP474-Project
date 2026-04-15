;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F.clp  - Factbase (Templates + Initial Facts)
;; Domain: Small Business Network (Wi-Fi) Troubleshooting
;; D2 TODO 3: Possibilistic Uncertainty (Fuzzy Logic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------
;; Templates (Base Facts)
;; ---------------------------

(deftemplate case
  (slot id (type INTEGER) (default 1))
  (slot status (type SYMBOL) (default open)))

(deftemplate scope
  (slot affected-devices (type SYMBOL) (default unknown))) ; one/some/all/unknown

(deftemplate environment
  (slot business-type (type SYMBOL) (default unknown))      ; cafe/office/store/unknown
  (slot number-of-users (type SYMBOL) (default unknown))    ; low/medium/high/unknown
  (slot critical-service (type SYMBOL) (default unknown)))  ; none/pos/cloud-email/unknown

(deftemplate connectivity
  (slot wifi-connected (type SYMBOL) (default unknown))     ; yes/no/unknown
  (slot internet-working (type SYMBOL) (default unknown))   ; yes/no/unknown
  (slot lan-working (type SYMBOL) (default unknown))        ; yes/no/unknown
  (slot slow-speed (type SYMBOL) (default unknown))         ; yes/no/unknown
  (slot intermittent (type SYMBOL) (default unknown)))      ; yes/no/unknown

(deftemplate infrastructure
  (slot router-status (type SYMBOL) (default unknown))              ; normal/off/unknown
  (slot modem-status (type SYMBOL) (default unknown))               ; online/offline/unknown
  (slot access-point-count (type SYMBOL) (default unknown))         ; single/multiple/unknown
  (slot signal-strength (type SYMBOL) (default unknown))            ; strong/weak/unknown
  (slot recent-config-change (type SYMBOL) (default unknown))       ; yes/no/unknown
  (slot password-recently-changed (type SYMBOL) (default unknown))) ; yes/no/unknown

(deftemplate symptoms
  (slot websites-not-loading (type SYMBOL) (default unknown))  ; yes/no/unknown
  (slot dns-error (type SYMBOL) (default unknown))             ; yes/no/unknown
  (slot only-some-apps-bad (type SYMBOL) (default unknown)))   ; yes/no/unknown

(deftemplate meta
  (slot priority (type SYMBOL) (default medium))            ; low/medium/high
  (slot escalated (type SYMBOL) (default no))               ; yes/no
  (slot attempts (type INTEGER) (default 0))
  (slot printed-diagnosis (type SYMBOL) (default no))       ; yes/no
  (slot printed-actions (type SYMBOL) (default no)))        ; yes/no

(deftemplate security
  (slot auth-fail (type SYMBOL) (default unknown))          ; yes/no/unknown
  (slot captive-portal (type SYMBOL) (default unknown))     ; yes/no/unknown
  (slot vpn-required (type SYMBOL) (default unknown)))      ; yes/no/unknown

(deftemplate services
  (slot dhcp-ok (type SYMBOL) (default unknown))            ; yes/no/unknown
  (slot dns-ok (type SYMBOL) (default unknown))             ; yes/no/unknown
  (slot cloud-service-up (type SYMBOL) (default unknown)))  ; yes/no/unknown

(deftemplate performance
  (slot latency-high (type SYMBOL) (default unknown))       ; yes/no/unknown
  (slot packet-loss (type SYMBOL) (default unknown))        ; yes/no/unknown
  (slot jitter-high (type SYMBOL) (default unknown)))       ; yes/no/unknown

;; ---------------------------
;; Derived facts (asserted by rules)
;; ---------------------------

(deftemplate diagnosis
  (slot text (type STRING))
  (slot confidence (type SYMBOL))) ; low/medium/high

(deftemplate action
  (slot text (type STRING))
  (slot priority (type SYMBOL)))   ; low/medium/high

;; ---------------------------
;; D2 TODO 3: Fuzzy templates (NEW)
;; Native FuzzyCLIPS deftemplate syntax
;; ---------------------------

;; signal-quality: 0 = no signal, 100 = perfect signal
(deftemplate signal-quality
  0 100
  ((poor     (0 1)  (30 1)  (50 0))
   (marginal (30 0) (50 1)  (70 0))
   (good     (50 0) (80 1)  (100 1))))

;; network-load: 0 = no load, 100 = fully saturated
(deftemplate network-load
  0 100
  ((light    (0 1)  (30 1)  (50 0))
   (moderate (30 0) (50 1)  (70 0))
   (heavy    (50 0) (80 1)  (100 1))))

;; connection-stability: 0 = completely unstable, 100 = perfectly stable
(deftemplate connection-stability
  0 100
  ((unstable (0 1)  (30 1)  (50 0))
   (flaky    (30 0) (50 1)  (70 0))
   (stable   (50 0) (80 1)  (100 1))))

;; response-time: 0ms = instant, 500ms = very slow
(deftemplate response-time
  0 500
  ((fast       (0 1)   (50 1)   (150 0))
   (acceptable (50 0)  (150 1)  (300 0))
   (slow       (150 0) (300 1)  (500 1))))

;; fuzzy-diagnosis: output fact storing graded possibility
(deftemplate fuzzy-diagnosis
  (slot text (type STRING))
  (slot possibility (type FLOAT)))

;; fuzzy-inputs: stores raw crisp values entered by user
;; used by RHS of fuzzy rules to compute membership degrees manually
(deftemplate fuzzy-inputs
  (slot signal-quality-val    (type FLOAT) (default -1.0))
  (slot network-load-val      (type FLOAT) (default -1.0))
  (slot connection-stability-val (type FLOAT) (default -1.0))
  (slot response-time-val     (type FLOAT) (default -1.0)))

;; ---------------------------
;; Initial Facts (Defaults)
;; ---------------------------

(deffacts initial-facts
  (case (id 1) (status open))
  (scope (affected-devices unknown))
  (environment (business-type unknown) (number-of-users unknown) (critical-service unknown))
  (connectivity (wifi-connected unknown) (internet-working unknown) (lan-working unknown)
                (slow-speed unknown) (intermittent unknown))
  (infrastructure (router-status unknown) (modem-status unknown) (access-point-count unknown)
                  (signal-strength unknown) (recent-config-change unknown)
                  (password-recently-changed unknown))
  (symptoms (websites-not-loading unknown) (dns-error unknown) (only-some-apps-bad unknown))
  (meta (priority medium) (escalated no) (attempts 0) (printed-diagnosis no) (printed-actions no))
  (security (auth-fail unknown) (captive-portal unknown) (vpn-required unknown))
  (services (dhcp-ok unknown) (dns-ok unknown) (cloud-service-up unknown))
  (performance (latency-high unknown) (packet-loss unknown) (jitter-high unknown))
  (fuzzy-inputs)
)