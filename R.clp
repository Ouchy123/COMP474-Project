;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R.clp  - Rulebase (IF–THEN rules)
;; Domain: Small Business Network (Wi-Fi) Troubleshooting
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
;; Single ask helper (ONLY ONE - removed duplicate)
;; ---------------------------

(deffunction ask-symbol (?prompt ?allowed)
  (printout t ?prompt crlf "Options: " ?allowed crlf "> ")
  (bind ?ans (read))
  (while (not (member$ ?ans ?allowed)) do
    (printout t "Invalid. Please enter one of: " ?allowed crlf "> ")
    (bind ?ans (read)))
  (return ?ans))

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

;; NOTE: relaxed trigger so Quick Mode (only asks dns-error) can still fire
(defrule R10-dns-error-websites-not-loading
  (symptoms (dns-error yes))
  (connectivity (internet-working yes))
  =>
  (add-diagnosis "DNS issue (internet up but DNS failing)." high)
  (add-action "Switch DNS to public DNS (1.1.1.1 / 8.8.8.8) and flush DNS cache." high))

;; ---------------------------
;; Additional rules
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

;; =========================================================
;; 5 coverage rules (R20 ~ R24)
;; reduce "(none)" in common Quick Mode situations.
;; =========================================================

;; R20: Internet works but slow (not necessarily high users, office/store)
(defrule R20-slow-only-general
  (connectivity (wifi-connected yes) (internet-working yes) (slow-speed yes))
  (environment (number-of-users low|medium))
  =>
  (add-diagnosis "Slow internet with Wi-Fi connected (possible ISP throttling, background updates, or limited bandwidth)." medium)
  (add-action "Run a speed test and check if backups/updates/streams are consuming bandwidth." high)
  (add-action "Enable QoS or limit heavy traffic; consider upgrading ISP plan if consistently slow." medium))

;; R21: Only some apps fail while internet is OK (service-specific / filtering)
(defrule R21-only-some-apps-service-specific
  (connectivity (wifi-connected yes) (internet-working yes))
  (symptoms (only-some-apps-bad yes))
  (symptoms (dns-error no|unknown))
  =>
  (add-diagnosis "Only some apps fail (service-specific issue, firewall/content filter, or vendor outage)." medium)
  (add-action "Test the same app on another network (phone hotspot) to confirm service-side issue." high)
  (add-action "Check firewall/content filtering rules or proxy settings that might block the app." medium))

;; R22: Password recently changed + affected one/some devices (more precise than R9)
(defrule R22-password-changed-some-devices
  (scope (affected-devices one|some))
  (connectivity (wifi-connected no))
  (infrastructure (password-recently-changed yes))
  =>
  (add-diagnosis "Wi-Fi password was changed; some devices likely still have old saved credentials." high)
  (add-action "On affected devices: Forget the Wi-Fi network and reconnect with the updated password." high)
  (add-action "If it still fails, verify WPA2/WPA3 compatibility and reboot the device." medium))

;; R23: Intermittent dropping + modem offline/unknown (ISP instability)
(defrule R23-intermittent-isp-instability
  (connectivity (intermittent yes))
  (infrastructure (modem-status offline|unknown))
  =>
  (add-diagnosis "Intermittent connection likely due to ISP line/modem instability." high)
  (add-action "Power-cycle modem (unplug 30s) and check ISP cables/line connections." high)
  (add-action "If modem frequently goes offline, contact ISP and report intermittent drops." medium))

;; R24: DHCP unknown/no + internet not working (stronger DHCP/IP assignment hint)
(defrule R24-dhcp-unknown-or-no-internet-down
  (connectivity (wifi-connected yes) (internet-working no))
  (services (dhcp-ok no|unknown))
  =>
  (add-diagnosis "Possible DHCP/IP assignment issue (connected to Wi-Fi but no internet)." medium)
  (add-action "Reconnect device to renew IP; restart router DHCP if multiple devices affected." high)
  (add-action "Check DHCP pool size (IP exhaustion) and expand leases if needed." medium))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple User Input Functions (CLIPS console Q/A)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUICK MODE: User Input + Run 
;; Uses fixed fact numbers (f-2..f-10) for stability.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction user-input-and-run ()
  (reset)

  (printout t crlf "=== Small Business Wi-Fi Troubleshooting (Quick Mode) ===" crlf)

  ;; 1) Scope (f-2)
  (bind ?scope
        (ask-symbol "1) How many devices are affected? (one/some/all)"
                    (create$ one some all)))
  (modify 2 (affected-devices ?scope))

  ;; 2-4) Connectivity core (f-4)
  (bind ?wifi
        (ask-symbol "2) Is Wi-Fi connected? (yes/no)"
                    (create$ yes no)))
  (bind ?internet
        (ask-symbol "3) Is internet working? (yes/no)"
                    (create$ yes no)))
  (bind ?slow
        (ask-symbol "4) Is it slow? (yes/no)"
                    (create$ yes no)))
  (modify 4 (wifi-connected ?wifi)
            (internet-working ?internet)
            (slow-speed ?slow))

  ;; 5-6) Environment (f-3)
  (bind ?bt
        (ask-symbol "5) Business type? (cafe/office/store/other)"
                    (create$ cafe office store other)))
  (bind ?users
        (ask-symbol "6) Number of users right now? (low/medium/high)"
                    (create$ low medium high)))
  (modify 3 (business-type ?bt)
            (number-of-users ?users))

  ;; 7-8) Infrastructure core (f-5)
  (bind ?router
        (ask-symbol "7) Router status? (normal/off/unknown)"
                    (create$ normal off unknown)))
  (bind ?modem
        (ask-symbol "8) Modem status? (online/offline/unknown)"
                    (create$ online offline unknown)))
  (modify 5 (router-status ?router)
            (modem-status ?modem))

  ;; 9) Password changed? (f-5)
  (bind ?pw
        (ask-symbol "9) Password recently changed? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 5 (password-recently-changed ?pw))

  ;; 10) DNS error? (f-6)  (R10 now triggers on dns-error yes)
  (bind ?dns
        (ask-symbol "10) DNS error? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (modify 6 (dns-error ?dns))

  (printout t crlf "=== Running inference... ===" crlf)
  (run)
)
