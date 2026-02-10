;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F.clp  - Factbase (Templates + Initial Facts)
;; Domain: Small Business Network (Wi-Fi) Troubleshooting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------
;; Templates (Base Facts)
;; ---------------------------

(deftemplate case
  (slot id (type INTEGER) (default 1))
  (slot status (type SYMBOL) (default open)))  ; open/closed

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

;; ---------------------------
;; NEW: Meaningful Base Facts (used by rules)
;; ---------------------------

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
;; Initial Facts (Defaults)
;; IMPORTANT: Now we have 10 meaningful base fact instances after (reset)
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

  ;; --- New meaningful base facts (used in rules) ---
  (security (auth-fail unknown) (captive-portal unknown) (vpn-required unknown))
  (services (dhcp-ok unknown) (dns-ok unknown) (cloud-service-up unknown))
  (performance (latency-high unknown) (packet-loss unknown) (jitter-high unknown))
)
