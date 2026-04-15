;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F.clp  - Factbase (Templates + Initial Facts)
;; Domain: Small Business Network (Wi-Fi) Troubleshooting
;; Deliverable 2 - Uncertainty Reasoning Version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------
;; Core templates from D1
;; ---------------------------

(deftemplate case
  (slot id (type INTEGER) (default 1))
  (slot status (type SYMBOL) (default open)))

(deftemplate scope
  (slot affected-devices (type SYMBOL) (default unknown))) ; one/some/all/unknown

(deftemplate environment
  (slot business-type (type SYMBOL) (default unknown))      ; cafe/office/store/other/unknown
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
  (slot websites-not-loading (type SYMBOL) (default unknown))
  (slot dns-error (type SYMBOL) (default unknown))
  (slot only-some-apps-bad (type SYMBOL) (default unknown)))

(deftemplate meta
  (slot priority (type SYMBOL) (default medium))
  (slot escalated (type SYMBOL) (default no))
  (slot attempts (type INTEGER) (default 0))
  (slot printed-diagnosis (type SYMBOL) (default no))
  (slot printed-actions (type SYMBOL) (default no)))

(deftemplate security
  (slot auth-fail (type SYMBOL) (default unknown))
  (slot captive-portal (type SYMBOL) (default unknown))
  (slot vpn-required (type SYMBOL) (default unknown)))

(deftemplate services
  (slot dhcp-ok (type SYMBOL) (default unknown))
  (slot dns-ok (type SYMBOL) (default unknown))
  (slot cloud-service-up (type SYMBOL) (default unknown)))

(deftemplate performance
  (slot latency-high (type SYMBOL) (default unknown))
  (slot packet-loss (type SYMBOL) (default unknown))
  (slot jitter-high (type SYMBOL) (default unknown)))

;; ---------------------------
;; NEW D2 useful domain facts (5+ additions)
;; These are still about the domain, not just implementation.
;; ---------------------------

(deftemplate radio-environment
  (slot neighbor-interference (type SYMBOL) (default unknown)) ; low/medium/high/unknown
  (slot obstruction-level (type SYMBOL) (default unknown)))    ; low/medium/high/unknown

(deftemplate addressing
  (slot valid-ip (type SYMBOL) (default unknown))              ; yes/no/unknown
  (slot ip-conflict (type SYMBOL) (default unknown)))          ; yes/no/unknown

(deftemplate wan-observation
  (slot isp-status-page (type SYMBOL) (default unknown))       ; normal/outage/unknown
  (slot modem-lights-normal (type SYMBOL) (default unknown)))  ; yes/no/unknown

(deftemplate device-health
  (slot adapter-enabled (type SYMBOL) (default unknown))       ; yes/no/unknown
  (slot driver-ok (type SYMBOL) (default unknown)))            ; yes/no/unknown

(deftemplate traffic-profile
  (slot large-downloads (type SYMBOL) (default unknown))       ; yes/no/unknown
  (slot guest-traffic-heavy (type SYMBOL) (default unknown)))  ; yes/no/unknown

;; ---------------------------
;; D2 uncertainty templates
;; ---------------------------

(deftemplate evidence
  (slot name (type SYMBOL))
  (slot value (type SYMBOL))
  (slot cf (type FLOAT)))

(deftemplate diagnosis
  (slot text (type STRING))
  (slot cf (type FLOAT)))

(deftemplate action
  (slot text (type STRING))
  (slot priority (type SYMBOL)))

;; ---------------------------
;; Initial facts
;; Keep original order for quick-mode modify stability
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

  ;; NEW D2 facts
  (radio-environment (neighbor-interference unknown) (obstruction-level unknown))
  (addressing (valid-ip unknown) (ip-conflict unknown))
  (wan-observation (isp-status-page unknown) (modem-lights-normal unknown))
  (device-health (adapter-enabled unknown) (driver-ok unknown))
  (traffic-profile (large-downloads unknown) (guest-traffic-heavy unknown))
)