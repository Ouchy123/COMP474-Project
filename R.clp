;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R.clp  - Rulebase (IF–THEN rules)
;; Domain: Small Business Network (Wi-Fi) Troubleshooting
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------
;; Helper functions
;; ---------------------------

(deffunction add-diagnosis (?t ?c)
  (if (not (any-factp ((?d diagnosis))
            (and (eq (fact-slot-value ?d text) ?t)
                 (eq (fact-slot-value ?d confidence) ?c))))
    then (assert (diagnosis (text ?t) (confidence ?c)))))

(deffunction add-action (?t ?p)
  (if (not (any-factp ((?a action))
            (and (eq (fact-slot-value ?a text) ?t)
                 (eq (fact-slot-value ?a priority) ?p))))
    then (assert (action (text ?t) (priority ?p)))))

(deffunction print-diagnoses ()
  (printout t crlf "=== Likely Causes (Diagnosis) ===" crlf)
  (bind ?ds (find-all-facts ((?d diagnosis)) TRUE))
  (if (= (length$ ?ds) 0)
    then
      (printout t "- (none)" crlf)
    else
      (foreach ?d ?ds
        (printout t "- "
                  (fact-slot-value ?d text)
                  "  [confidence: "
                  (fact-slot-value ?d confidence)
                  "]" crlf))))

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


;; ---------------------------
;; Core rules (same logic, now stable)
;; ---------------------------

(defrule R1-router-no-power-full-outage
  (scope (affected-devices all))
  (connectivity (wifi-connected no) (internet-working no))
  (infrastructure (router-status off))
  =>
  (add-diagnosis "Router appears powered off (no Wi-Fi for everyone)." high)
  (add-action "Check router power cable/outlet and power it on." high))

(defrule R2-modem-offline-full-outage
  (scope (affected-devices all))
  (connectivity (wifi-connected yes) (internet-working no))
  (infrastructure (modem-status offline))
  =>
  (add-diagnosis "ISP link down or modem not connected to ISP." high)
  (add-action "Restart modem (power cycle 30 seconds) and check ISP line/cable." high)
  (add-action "If modem stays offline after reboot, contact ISP (possible outage/line issue)." high)
  (add-action "Check modem lights and restart modem first, then router." high))

(defrule R3-wifi-down-all-devices
  (scope (affected-devices all))
  (connectivity (wifi-connected no))
  =>
  (add-diagnosis "Wi-Fi is down for everyone (router/AP issue likely)." high)
  (add-action "Restart router/access point and verify SSID is broadcasting." high))

(defrule R4-wifi-ok-internet-down
  (scope (affected-devices all))
  (connectivity (wifi-connected yes) (internet-working no))
  =>
  (add-diagnosis "WAN/ISP or modem issue (Wi-Fi works but no internet)." high)
  (add-action "Check modem status and WAN cable; reboot modem then router." high))

(defrule R5-single-device-cant-connect
  (scope (affected-devices one))
  (connectivity (wifi-connected no))
  =>
  (add-diagnosis "Single device Wi-Fi issue (device-side problem)." high)
  (add-action "On the device: toggle Wi-Fi, forget network, re-enter password." high))

(defrule R6-coverage-weak-signal
  (scope (affected-devices some))
  (infrastructure (signal-strength weak))
  =>
  (add-diagnosis "Coverage issue (weak signal for some devices)." high)
  (add-action "Move closer to AP, reposition AP, or add an additional access point." high))

(defrule R7-slow-high-users
  (connectivity (slow-speed yes))
  (environment (number-of-users high))
  =>
  (add-diagnosis "Bandwidth saturation (too many users on limited internet)." high)
  (add-action "Limit heavy traffic, schedule updates, or upgrade ISP plan." high))

(defrule R8-slow-guest-overload-cafe
  (connectivity (slow-speed yes))
  (environment (business-type cafe))
  =>
  (add-diagnosis "Guest Wi-Fi overload (common in cafes)." medium)
  (add-action "Separate guest Wi-Fi and apply bandwidth limits for guest network." medium))

(defrule R9-password-changed-cant-connect
  (connectivity (wifi-connected no))
  (infrastructure (password-recently-changed yes))
  =>
  (add-diagnosis "Wi-Fi password changed; device has outdated credentials." high)
  (add-action "Forget Wi-Fi network and re-enter the updated password." high))

(defrule R10-dns-error-websites-not-loading
  (symptoms (websites-not-loading yes) (dns-error yes))
  (connectivity (internet-working yes))
  =>
  (add-diagnosis "DNS issue (internet up but DNS failing)." high)
  (add-action "Switch DNS to public DNS (1.1.1.1 / 8.8.8.8) and flush DNS cache." high))


;; ---------------------------
;; 
;; ---------------------------

(defrule R11-auth-failure
  (security (auth-fail yes))
  =>
  (add-diagnosis "Wi-Fi authentication failure (wrong password or security mismatch)." high)
  (add-action "Verify Wi-Fi password and security type (WPA2/WPA3); re-join network." high))

(defrule R12-captive-portal
  (security (captive-portal yes))
  (connectivity (wifi-connected yes))
  (connectivity (internet-working no))
  =>
  (add-diagnosis "Captive portal may be blocking internet access (login required)." medium)
  (add-action "Open a browser and complete captive portal login/terms page." medium))

(defrule R13-vpn-required-only-some-apps
  (security (vpn-required yes))
  (symptoms (only-some-apps-bad yes))
  =>
  (add-diagnosis "Some business apps may require VPN access." medium)
  (add-action "Connect to company VPN, then retry the affected apps/services." medium))

(defrule R14-dhcp-problem
  (services (dhcp-ok no))
  (connectivity (wifi-connected yes))
  =>
  (add-diagnosis "DHCP issue (clients may not be getting valid IP configuration)." high)
  (add-action "Renew IP address (release/renew) and restart DHCP service/router." high)
  (add-action "Check router DHCP scope/pool and ensure it is not exhausted." medium))

(defrule R15-dns-service-down
  (services (dns-ok no))
  (connectivity (wifi-connected yes))
  =>
  (add-diagnosis "DNS service issue (name resolution failing)." high)
  (add-action "Change DNS to public DNS and verify router DNS settings." high))

(defrule R16-cloud-service-down
  (services (cloud-service-up no))
  (symptoms (only-some-apps-bad yes))
  =>
  (add-diagnosis "Cloud service outage (problem is likely external to Wi-Fi)." medium)
  (add-action "Check vendor status page and wait/contact provider." medium))

(defrule R17-packet-loss-interference
  (performance (packet-loss yes))
  (infrastructure (signal-strength weak))
  =>
  (add-diagnosis "Packet loss likely due to interference or poor wireless link." high)
  (add-action "Change Wi-Fi channel, reduce interference, or reposition AP." high))

(defrule R18-high-latency-congestion
  (performance (latency-high yes))
  (environment (number-of-users high))
  =>
  (add-diagnosis "High latency likely due to network congestion." medium)
  (add-action "Limit heavy traffic, enable QoS, or upgrade bandwidth." medium))

(defrule R19-jitter-critical-traffic
  (performance (jitter-high yes))
  (environment (critical-service pos))
  =>
  (add-diagnosis "High jitter can disrupt real-time services (POS/voice/video)." medium)
  (add-action "Enable QoS for critical traffic and reduce background uploads." medium))


;; ---------------------------
;; Printing rules
;; ---------------------------

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
