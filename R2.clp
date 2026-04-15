;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R.clp  - Rulebase (Uncertainty Reasoning Version)
;; Domain: Small Business Network (Wi-Fi) Troubleshooting
;; D2: Certainty Factors with evidence combination
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ============================================================================
;; Helper functions
;; ============================================================================

(deffunction min2 (?a ?b)
  (if (< ?a ?b) then (return ?a) else (return ?b)))

(deffunction min3 (?a ?b ?c)
  (return (min2 (min2 ?a ?b) ?c)))

(deffunction min4 (?a ?b ?c ?d)
  (return (min2 (min3 ?a ?b ?c) ?d)))

(deffunction combine-positive (?a ?b)
  ; Standard positive CF combination:
  ; CFnew = CF1 + CF2(1 - CF1)
  (return (+ ?a (* ?b (- 1 ?a)))))

(deffunction max2 (?a ?b)
  (if (> ?a ?b) then (return ?a) else (return ?b)))

(deffunction add-evidence (?name ?value ?cf)
  (bind ?found FALSE)
  (do-for-fact ((?e evidence)) TRUE
    (if (and (eq (fact-slot-value ?e name) ?name)
             (eq (fact-slot-value ?e value) ?value))
      then
        (bind ?found ?e)))
  (if (eq ?found FALSE)
    then
      (assert (evidence (name ?name) (value ?value) (cf ?cf)))
    else
      (modify ?found (cf (max2 ?cf (fact-slot-value ?found cf))))))

(deffunction add-diagnosis (?text ?cf)
  (if (<= ?cf 0.0) then (return FALSE))
  (bind ?found FALSE)
  (do-for-fact ((?d diagnosis)) TRUE
    (if (eq (fact-slot-value ?d text) ?text)
      then
        (bind ?found ?d)))
  (if (eq ?found FALSE)
    then
      (assert (diagnosis (text ?text) (cf ?cf)))
    else
      (modify ?found (cf (combine-positive (fact-slot-value ?found cf) ?cf)))))

(deffunction add-action (?text ?priority)
  (if (not (any-factp ((?a action))
            (and (eq (fact-slot-value ?a text) ?text)
                 (eq (fact-slot-value ?a priority) ?priority))))
    then
      (assert (action (text ?text) (priority ?priority)))))

(deffunction print-diagnoses ()
  (printout t crlf "=== Likely Causes (Diagnosis) ===" crlf)
  (bind ?ds (find-all-facts ((?d diagnosis)) TRUE))
  (bind ?printed 0)
  (if (= (length$ ?ds) 0)
    then
      (printout t "- (none)" crlf)
    else
      (foreach ?d ?ds
        (if (> (fact-slot-value ?d cf) 0.10)
          then
            (printout t "- "
                      (fact-slot-value ?d text)
                      "  [cf: "
                      (fact-slot-value ?d cf)
                      "]" crlf)
            (bind ?printed (+ ?printed 1)))))
  (if (= ?printed 0)
    then
      (printout t "- (none)" crlf)))

(deffunction print-actions ()
  (printout t crlf "=== Suggested Actions ===" crlf)
  (bind ?as (find-all-facts ((?a action)) TRUE))
  (if (= (length$ ?as) 0)
    then
      (printout t "- (none)" crlf)
    else
      (foreach ?a ?as
        (printout t "- ("
                  (fact-slot-value ?a priority)
                  ") "
                  (fact-slot-value ?a text)
                  crlf)))
  (printout t crlf "=== End of recommendations ===" crlf))

(deffunction ask-symbol (?prompt ?allowed)
  (printout t ?prompt crlf "Options: " ?allowed crlf "> ")
  (bind ?ans (read))
  (while (not (member$ ?ans ?allowed)) do
    (printout t "Invalid. Please enter one of: " ?allowed crlf "> ")
    (bind ?ans (read)))
  (return ?ans))

;; ============================================================================
;; EVIDENCE EXTRACTION RULES
;; Convert observed facts into uncertain evidence with CF values
;; ============================================================================

(defrule E1-scope-all
  (scope (affected-devices all))
  =>
  (add-evidence scope-all yes 0.95))

(defrule E2-scope-some
  (scope (affected-devices some))
  =>
  (add-evidence scope-some yes 0.85))

(defrule E3-scope-one
  (scope (affected-devices one))
  =>
  (add-evidence scope-one yes 0.90))

(defrule E4-wifi-up
  (connectivity (wifi-connected yes))
  =>
  (add-evidence wifi-up yes 0.95))

(defrule E5-wifi-down
  (connectivity (wifi-connected no))
  =>
  (add-evidence wifi-down yes 0.95))

(defrule E6-internet-up
  (connectivity (internet-working yes))
  =>
  (add-evidence internet-up yes 0.95))

(defrule E7-internet-down
  (connectivity (internet-working no))
  =>
  (add-evidence internet-down yes 0.95))

(defrule E8-slow-speed
  (connectivity (slow-speed yes))
  =>
  (add-evidence slow-speed yes 0.85))

(defrule E9-intermittent
  (connectivity (intermittent yes))
  =>
  (add-evidence intermittent yes 0.80))

(defrule E10-router-off
  (infrastructure (router-status off))
  =>
  (add-evidence router-off yes 0.98))

(defrule E11-modem-offline
  (infrastructure (modem-status offline))
  =>
  (add-evidence modem-offline yes 0.95))

(defrule E12-weak-signal
  (infrastructure (signal-strength weak))
  =>
  (add-evidence weak-signal yes 0.85))

(defrule E13-password-change
  (infrastructure (password-recently-changed yes))
  =>
  (add-evidence password-change yes 0.85))

(defrule E14-dns-symptom
  (symptoms (dns-error yes))
  =>
  (add-evidence dns-symptom yes 0.90))

(defrule E15-some-apps-bad
  (symptoms (only-some-apps-bad yes))
  =>
  (add-evidence some-apps-bad yes 0.80))

(defrule E16-auth-fail
  (security (auth-fail yes))
  =>
  (add-evidence auth-fail yes 0.90))

(defrule E17-vpn-required
  (security (vpn-required yes))
  =>
  (add-evidence vpn-required yes 0.85))

(defrule E18-dhcp-failure
  (services (dhcp-ok no))
  =>
  (add-evidence dhcp-failure yes 0.85))

(defrule E19-dns-service-ok
  (services (dns-ok yes))
  =>
  (add-evidence dns-service-ok yes 0.90))

(defrule E20-cloud-down
  (services (cloud-service-up no))
  =>
  (add-evidence cloud-down yes 0.90))

(defrule E21-latency-high
  (performance (latency-high yes))
  =>
  (add-evidence latency-high yes 0.80))

(defrule E22-packet-loss
  (performance (packet-loss yes))
  =>
  (add-evidence packet-loss yes 0.85))

(defrule E23-jitter-high
  (performance (jitter-high yes))
  =>
  (add-evidence jitter-high yes 0.75))

(defrule E24-high-users
  (environment (number-of-users high))
  =>
  (add-evidence high-users yes 0.85))

(defrule E25-cafe-context
  (environment (business-type cafe))
  =>
  (add-evidence cafe-context yes 0.75))

(defrule E26-pos-critical
  (environment (critical-service pos))
  =>
  (add-evidence pos-critical yes 0.80))

(defrule E27-interference-high
  (radio-environment (neighbor-interference high))
  =>
  (add-evidence interference-high yes 0.90))

(defrule E28-obstruction-high
  (radio-environment (obstruction-level high))
  =>
  (add-evidence obstruction-high yes 0.75))

(defrule E29-valid-ip-no
  (addressing (valid-ip no))
  =>
  (add-evidence invalid-ip yes 0.90))

(defrule E30-ip-conflict
  (addressing (ip-conflict yes))
  =>
  (add-evidence ip-conflict yes 0.90))

(defrule E31-isp-status-outage
  (wan-observation (isp-status-page outage))
  =>
  (add-evidence isp-status-outage yes 0.95))

(defrule E32-modem-lights-bad
  (wan-observation (modem-lights-normal no))
  =>
  (add-evidence modem-lights-bad yes 0.85))

(defrule E33-adapter-disabled
  (device-health (adapter-enabled no))
  =>
  (add-evidence adapter-disabled yes 0.95))

(defrule E34-driver-bad
  (device-health (driver-ok no))
  =>
  (add-evidence driver-bad yes 0.90))

(defrule E35-large-downloads
  (traffic-profile (large-downloads yes))
  =>
  (add-evidence large-downloads yes 0.80))

(defrule E36-guest-traffic-heavy
  (traffic-profile (guest-traffic-heavy yes))
  =>
  (add-evidence guest-heavy yes 0.85))

;; ============================================================================
;; HYPOTHESIS RULES (Uncertainty reasoning with evidence combination)
;; Use rule strength × min(evidence CFs), plus some conflict handling
;; ============================================================================

;; H1: Router power failure
(defrule H1-router-power-failure
  (evidence (name scope-all) (value yes) (cf ?cf1))
  (evidence (name wifi-down) (value yes) (cf ?cf2))
  (evidence (name router-off) (value yes) (cf ?cf3))
  =>
  (bind ?final (* 0.98 (min3 ?cf1 ?cf2 ?cf3)))
  (add-diagnosis "Router power failure" ?final))

;; H2: ISP outage / modem-to-ISP failure (strong version)
(defrule H2-isp-outage-strong
  (evidence (name scope-all) (value yes) (cf ?cf1))
  (evidence (name wifi-up) (value yes) (cf ?cf2))
  (evidence (name internet-down) (value yes) (cf ?cf3))
  (evidence (name modem-offline) (value yes) (cf ?cf4))
  =>
  (bind ?final (* 0.95 (min4 ?cf1 ?cf2 ?cf3 ?cf4)))
  (add-diagnosis "ISP outage or modem-to-ISP link failure" ?final))

;; H3: ISP outage confirmed by status page
(defrule H3-isp-outage-status-page
  (evidence (name isp-status-outage) (value yes) (cf ?cf1))
  =>
  (bind ?final (* 0.98 ?cf1))
  (add-diagnosis "ISP outage or modem-to-ISP link failure" ?final))

;; H4: ISP instability / intermittent WAN problem
(defrule H4-isp-instability
  (evidence (name intermittent) (value yes) (cf ?cf1))
  (or (evidence (name modem-offline) (value yes) (cf ?cf2))
      (evidence (name modem-lights-bad) (value yes) (cf ?cf2)))
  =>
  (bind ?final (* 0.88 (min2 ?cf1 ?cf2)))
  (add-diagnosis "ISP line instability" ?final))

;; H5: Device-side adapter/driver issue
(defrule H5-device-adapter-driver
  (evidence (name scope-one) (value yes) (cf ?cf1))
  (evidence (name wifi-down) (value yes) (cf ?cf2))
  (or (evidence (name adapter-disabled) (value yes) (cf ?cf3))
      (evidence (name driver-bad) (value yes) (cf ?cf3)))
  =>
  (bind ?final (* 0.92 (min3 ?cf1 ?cf2 ?cf3)))
  (add-diagnosis "Single-device adapter or driver issue" ?final))

;; H6: Credential mismatch after password change
(defrule H6-credential-mismatch
  (or (evidence (name scope-one) (value yes) (cf ?cf1))
      (evidence (name scope-some) (value yes) (cf ?cf1)))
  (evidence (name wifi-down) (value yes) (cf ?cf2))
  (or (evidence (name password-change) (value yes) (cf ?cf3))
      (evidence (name auth-fail) (value yes) (cf ?cf3)))
  =>
  (bind ?final (* 0.90 (min3 ?cf1 ?cf2 ?cf3)))
  (add-diagnosis "Credential mismatch after password change" ?final))

;; H7: Credential mismatch weakened if ALL devices are affected
(defrule H7-credential-mismatch-weakened
  (evidence (name scope-all) (value yes) (cf ?cf1))
  (evidence (name wifi-down) (value yes) (cf ?cf2))
  (or (evidence (name password-change) (value yes) (cf ?cf3))
      (evidence (name auth-fail) (value yes) (cf ?cf3)))
  =>
  (bind ?support (* 0.90 (min3 ?cf1 ?cf2 ?cf3)))
  (bind ?final (* ?support 0.45))
  (add-diagnosis "Credential mismatch after password change" ?final))

;; H8: DNS failure (normal support)
(defrule H8-dns-failure
  (evidence (name dns-symptom) (value yes) (cf ?cf1))
  (evidence (name internet-up) (value yes) (cf ?cf2))
  (not (evidence (name dns-service-ok) (value yes) (cf ?)))
  =>
  (bind ?final (* 0.95 (min2 ?cf1 ?cf2)))
  (add-diagnosis "DNS resolution failure" ?final))

;; H9: DNS failure but conflicting evidence exists
(defrule H9-dns-failure-conflicted
  (evidence (name dns-symptom) (value yes) (cf ?cf1))
  (evidence (name internet-up) (value yes) (cf ?cf2))
  (evidence (name dns-service-ok) (value yes) (cf ?cf3))
  =>
  (bind ?support (* 0.95 (min2 ?cf1 ?cf2)))
  (bind ?final (* ?support (- 1 ?cf3)))
  (add-diagnosis "DNS resolution failure" ?final))

;; H10: DHCP / IP assignment issue
(defrule H10-dhcp-ip-issue
  (evidence (name wifi-up) (value yes) (cf ?cf1))
  (evidence (name internet-down) (value yes) (cf ?cf2))
  (or (evidence (name dhcp-failure) (value yes) (cf ?cf3))
      (evidence (name invalid-ip) (value yes) (cf ?cf3))
      (evidence (name ip-conflict) (value yes) (cf ?cf3)))
  =>
  (bind ?final (* 0.90 (min3 ?cf1 ?cf2 ?cf3)))
  (add-diagnosis "DHCP/IP assignment issue" ?final))

;; H11: Bandwidth congestion
(defrule H11-bandwidth-congestion
  (evidence (name slow-speed) (value yes) (cf ?cf1))
  (evidence (name high-users) (value yes) (cf ?cf2))
  (or (evidence (name large-downloads) (value yes) (cf ?cf3))
      (evidence (name latency-high) (value yes) (cf ?cf3))
      (evidence (name jitter-high) (value yes) (cf ?cf3)))
  =>
  (bind ?final (* 0.88 (min3 ?cf1 ?cf2 ?cf3)))
  (add-diagnosis "Bandwidth congestion" ?final))

;; H12: General congestion (lighter support if only slow + many users)
(defrule H12-bandwidth-congestion-light
  (evidence (name slow-speed) (value yes) (cf ?cf1))
  (evidence (name high-users) (value yes) (cf ?cf2))
  =>
  (bind ?final (* 0.75 (min2 ?cf1 ?cf2)))
  (add-diagnosis "Bandwidth congestion" ?final))

;; H13: Guest overload in cafe
(defrule H13-guest-overload
  (evidence (name cafe-context) (value yes) (cf ?cf1))
  (evidence (name slow-speed) (value yes) (cf ?cf2))
  (or (evidence (name guest-heavy) (value yes) (cf ?cf3))
      (evidence (name high-users) (value yes) (cf ?cf3)))
  =>
  (bind ?final (* 0.82 (min3 ?cf1 ?cf2 ?cf3)))
  (add-diagnosis "Guest Wi-Fi overload" ?final))

;; H14: Wireless interference / RF issue
(defrule H14-wireless-interference
  (evidence (name packet-loss) (value yes) (cf ?cf1))
  (evidence (name weak-signal) (value yes) (cf ?cf2))
  (or (evidence (name interference-high) (value yes) (cf ?cf3))
      (evidence (name obstruction-high) (value yes) (cf ?cf3)))
  =>
  (bind ?final (* 0.92 (min3 ?cf1 ?cf2 ?cf3)))
  (add-diagnosis "Wireless interference / poor RF conditions" ?final))

;; H15: External cloud service outage
(defrule H15-cloud-outage
  (evidence (name some-apps-bad) (value yes) (cf ?cf1))
  (evidence (name internet-up) (value yes) (cf ?cf2))
  (evidence (name cloud-down) (value yes) (cf ?cf3))
  =>
  (bind ?final (* 0.93 (min3 ?cf1 ?cf2 ?cf3)))
  (add-diagnosis "External cloud service outage" ?final))

;; H16: VPN dependency
(defrule H16-vpn-dependency
  (evidence (name some-apps-bad) (value yes) (cf ?cf1))
  (evidence (name internet-up) (value yes) (cf ?cf2))
  (evidence (name vpn-required) (value yes) (cf ?cf3))
  =>
  (bind ?final (* 0.86 (min3 ?cf1 ?cf2 ?cf3)))
  (add-diagnosis "VPN dependency for affected apps" ?final))

;; ============================================================================
;; ACTION RULES
;; Add actions only when a diagnosis reaches a meaningful threshold
;; ============================================================================

(defrule A1-router-power
  (diagnosis (text "Router power failure") (cf ?cf&:(>= ?cf 0.55)))
  =>
  (add-action "Check router power adapter, outlet, and power switch." high)
  (add-action "Power-cycle the router and verify that the SSID is broadcasting." high))

(defrule A2-isp-outage
  (diagnosis (text "ISP outage or modem-to-ISP link failure") (cf ?cf&:(>= ?cf 0.55)))
  =>
  (add-action "Check modem lights and ISP cabling, then power-cycle the modem." high)
  (add-action "If the modem stays offline, contact the ISP and report the outage." high))

(defrule A3-isp-instability
  (diagnosis (text "ISP line instability") (cf ?cf&:(>= ?cf 0.50)))
  =>
  (add-action "Monitor modem status over time and note recurring disconnects." medium)
  (add-action "Contact the ISP if intermittent drops continue." medium))

(defrule A4-device-adapter
  (diagnosis (text "Single-device adapter or driver issue") (cf ?cf&:(>= ?cf 0.55)))
  =>
  (add-action "Check whether the device Wi-Fi adapter is enabled." high)
  (add-action "Update or reinstall the Wi-Fi adapter driver on the affected device." medium))

(defrule A5-credential-mismatch
  (diagnosis (text "Credential mismatch after password change") (cf ?cf&:(>= ?cf 0.50)))
  =>
  (add-action "Forget the Wi-Fi network and reconnect using the updated password." high)
  (add-action "Verify WPA2/WPA3 compatibility on the affected device." medium))

(defrule A6-dns-failure
  (diagnosis (text "DNS resolution failure") (cf ?cf&:(>= ?cf 0.50)))
  =>
  (add-action "Switch DNS to a public resolver such as 1.1.1.1 or 8.8.8.8." high)
  (add-action "Flush the DNS cache and test name resolution again." medium))

(defrule A7-dhcp-ip
  (diagnosis (text "DHCP/IP assignment issue") (cf ?cf&:(>= ?cf 0.50)))
  =>
  (add-action "Reconnect the device to renew the IP address." high)
  (add-action "Check the DHCP scope and make sure the address pool is not exhausted." medium))

(defrule A8-bandwidth-congestion
  (diagnosis (text "Bandwidth congestion") (cf ?cf&:(>= ?cf 0.50)))
  =>
  (add-action "Limit heavy traffic such as backups, updates, and streaming." high)
  (add-action "Enable QoS or upgrade available bandwidth if congestion is frequent." medium))

(defrule A9-guest-overload
  (diagnosis (text "Guest Wi-Fi overload") (cf ?cf&:(>= ?cf 0.45)))
  =>
  (add-action "Separate guest traffic from business traffic." medium)
  (add-action "Apply per-user bandwidth limits to the guest network." medium))

(defrule A10-wireless-interference
  (diagnosis (text "Wireless interference / poor RF conditions") (cf ?cf&:(>= ?cf 0.50)))
  =>
  (add-action "Change the Wi-Fi channel and reduce nearby interference sources." high)
  (add-action "Reposition the access point or add another AP for better coverage." medium))

(defrule A11-cloud-outage
  (diagnosis (text "External cloud service outage") (cf ?cf&:(>= ?cf 0.50)))
  =>
  (add-action "Check the cloud provider status page and wait for service restoration." medium)
  (add-action "Use an alternate workflow or escalate to the vendor if the outage persists." medium))

(defrule A12-vpn-dependency
  (diagnosis (text "VPN dependency for affected apps") (cf ?cf&:(>= ?cf 0.50)))
  =>
  (add-action "Connect to the business VPN and retest the affected applications." medium))

;; ============================================================================
;; PRINT RULES
;; ============================================================================

(defrule PRINT-diagnosis
  (declare (salience -1000))
  ?m <- (meta (printed-diagnosis no))
  =>
  (print-diagnoses)
  (modify ?m (printed-diagnosis yes)))

(defrule PRINT-actions
  (declare (salience -1001))
  ?m <- (meta (printed-actions no))
  =>
  (print-actions)
  (modify ?m (printed-actions yes)))

;; ============================================================================
;; Utility setters
;; ============================================================================

(deffunction set-scope (?val)
  (bind ?f (find-fact ((?s scope)) TRUE))
  (if (neq ?f FALSE) then (modify ?f (affected-devices ?val))))

(deffunction set-environment (?bt ?users ?critical)
  (bind ?f (find-fact ((?e environment)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (business-type ?bt) (number-of-users ?users) (critical-service ?critical))))

(deffunction set-connectivity (?wifi ?internet ?lan ?slow ?inter)
  (bind ?f (find-fact ((?c connectivity)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (wifi-connected ?wifi)
              (internet-working ?internet)
              (lan-working ?lan)
              (slow-speed ?slow)
              (intermittent ?inter))))

(deffunction set-infra (?router ?modem ?signal ?apcount ?recentcfg ?pwchanged)
  (bind ?f (find-fact ((?i infrastructure)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (router-status ?router)
              (modem-status ?modem)
              (signal-strength ?signal)
              (access-point-count ?apcount)
              (recent-config-change ?recentcfg)
              (password-recently-changed ?pwchanged))))

(deffunction set-symptoms (?web ?dns ?someapps)
  (bind ?f (find-fact ((?s symptoms)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (websites-not-loading ?web)
              (dns-error ?dns)
              (only-some-apps-bad ?someapps))))

(deffunction set-security (?auth ?portal ?vpn)
  (bind ?f (find-fact ((?s security)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (auth-fail ?auth)
              (captive-portal ?portal)
              (vpn-required ?vpn))))

(deffunction set-services (?dhcp ?dnsok ?cloud)
  (bind ?f (find-fact ((?s services)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (dhcp-ok ?dhcp)
              (dns-ok ?dnsok)
              (cloud-service-up ?cloud))))

(deffunction set-performance (?lat ?loss ?jit)
  (bind ?f (find-fact ((?p performance)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (latency-high ?lat)
              (packet-loss ?loss)
              (jitter-high ?jit))))

(deffunction set-radio-env (?interf ?obs)
  (bind ?f (find-fact ((?r radio-environment)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (neighbor-interference ?interf)
              (obstruction-level ?obs))))

(deffunction set-addressing (?valid ?conflict)
  (bind ?f (find-fact ((?a addressing)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (valid-ip ?valid)
              (ip-conflict ?conflict))))

(deffunction set-wan-observation (?status ?lights)
  (bind ?f (find-fact ((?w wan-observation)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (isp-status-page ?status)
              (modem-lights-normal ?lights))))

(deffunction set-device-health (?adapter ?driver)
  (bind ?f (find-fact ((?d device-health)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (adapter-enabled ?adapter)
              (driver-ok ?driver))))

(deffunction set-traffic-profile (?downloads ?guest)
  (bind ?f (find-fact ((?t traffic-profile)) TRUE))
  (if (neq ?f FALSE) then
    (modify ?f (large-downloads ?downloads)
              (guest-traffic-heavy ?guest))))

;; ============================================================================
;; QUICK MODE
;; Expanded so uncertainty reasoning has better evidence to work with
;; Fact IDs 2..10 remain stable from F.clp, new ones continue after that
;; ============================================================================

(deffunction user-input-and-run ()
  (reset)

  (printout t crlf "=== Small Business Wi-Fi Troubleshooting (Uncertainty Mode) ===" crlf)

  ;; f-2 scope
  (bind ?scope
        (ask-symbol "1) How many devices are affected? (one/some/all)"
                    (create$ one some all)))
  (modify 2 (affected-devices ?scope))

  ;; f-4 connectivity
  (bind ?wifi
        (ask-symbol "2) Is Wi-Fi connected? (yes/no)"
                    (create$ yes no)))
  (bind ?internet
        (ask-symbol "3) Is internet working? (yes/no)"
                    (create$ yes no)))
  (bind ?slow
        (ask-symbol "4) Is it slow? (yes/no)"
                    (create$ yes no)))
  (bind ?inter
        (ask-symbol "5) Is the connection intermittent? (yes/no)"
                    (create$ yes no)))
  (modify 4 (wifi-connected ?wifi)
            (internet-working ?internet)
            (slow-speed ?slow)
            (intermittent ?inter))

  ;; f-3 environment
  (bind ?bt
        (ask-symbol "6) Business type? (cafe/office/store/other)"
                    (create$ cafe office store other)))
  (bind ?users
        (ask-symbol "7) Number of users right now? (low/medium/high)"
                    (create$ low medium high)))
  (bind ?critical
        (ask-symbol "8) Critical service? (none/pos/cloud-email)"
                    (create$ none pos cloud-email)))
  (modify 3 (business-type ?bt)
            (number-of-users ?users)
            (critical-service ?critical))

  ;; f-5 infrastructure
  (bind ?router
        (ask-symbol "9) Router status? (normal/off/unknown)"
                    (create$ normal off unknown)))
  (bind ?modem
        (ask-symbol "10) Modem status? (online/offline/unknown)"
                    (create$ online offline unknown)))
  (bind ?signal
        (ask-symbol "11) Signal strength? (strong/weak/unknown)"
                    (create$ strong weak unknown)))
  (bind ?pw
        (ask-symbol "12) Password recently changed? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 5 (router-status ?router)
            (modem-status ?modem)
            (signal-strength ?signal)
            (password-recently-changed ?pw))

  ;; f-6 symptoms
  (bind ?dns
        (ask-symbol "13) DNS error shown? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (bind ?apps
        (ask-symbol "14) Only some apps failing? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 6 (dns-error ?dns)
            (only-some-apps-bad ?apps))

  ;; f-8 security
  (bind ?auth
        (ask-symbol "15) Authentication failure? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (bind ?vpn
        (ask-symbol "16) Do affected apps require VPN? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 8 (auth-fail ?auth)
            (vpn-required ?vpn))

  ;; f-9 services
  (bind ?dhcp
        (ask-symbol "17) Is DHCP working correctly? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (bind ?dnsok
        (ask-symbol "18) Is DNS service known to be OK? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (bind ?cloud
        (ask-symbol "19) Is cloud service up? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 9 (dhcp-ok ?dhcp)
            (dns-ok ?dnsok)
            (cloud-service-up ?cloud))

  ;; f-10 performance
  (bind ?lat
        (ask-symbol "20) High latency? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (bind ?loss
        (ask-symbol "21) Packet loss? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (bind ?jit
        (ask-symbol "22) High jitter? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 10 (latency-high ?lat)
             (packet-loss ?loss)
             (jitter-high ?jit))

  ;; new D2 fact instances start after f-10
  ;; f-11 radio-environment
  (bind ?interf
        (ask-symbol "23) Neighbor interference level? (low/medium/high/unknown)"
                    (create$ low medium high unknown)))
  (bind ?obs
        (ask-symbol "24) Physical obstruction level? (low/medium/high/unknown)"
                    (create$ low medium high unknown)))
  (modify 11 (neighbor-interference ?interf)
             (obstruction-level ?obs))

  ;; f-12 addressing
  (bind ?valid
        (ask-symbol "25) Does the device have a valid IP? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (bind ?ipconf
        (ask-symbol "26) Is there an IP conflict? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 12 (valid-ip ?valid)
             (ip-conflict ?ipconf))

  ;; f-13 wan-observation
  (bind ?ispstatus
        (ask-symbol "27) ISP status page? (normal/outage/unknown)"
                    (create$ normal outage unknown)))
  (bind ?lights
        (ask-symbol "28) Are modem lights normal? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 13 (isp-status-page ?ispstatus)
             (modem-lights-normal ?lights))

  ;; f-14 device-health
  (bind ?adapter
        (ask-symbol "29) Is the Wi-Fi adapter enabled? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (bind ?driver
        (ask-symbol "30) Is the Wi-Fi driver OK? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 14 (adapter-enabled ?adapter)
             (driver-ok ?driver))

  ;; f-15 traffic-profile
  (bind ?downloads
        (ask-symbol "31) Are there large downloads/backups happening? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (bind ?guestheavy
        (ask-symbol "32) Is guest traffic heavy? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 15 (large-downloads ?downloads)
             (guest-traffic-heavy ?guestheavy))

  (printout t crlf "=== Running inference... ===" crlf)
  (run))