;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R.clp - Rulebase (IF-THEN rules)
;; Domain: Small Business Network (Wi-Fi) Troubleshooting
;; Compatible with FuzzyCLIPS V6.10d
;; D2 TODO 3: Possibilistic Uncertainty (Fuzzy Logic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------
;; Helper functions
;; ---------------------------

(deffunction add-diagnosis (?t ?c)
  (assert (diagnosis (text ?t) (confidence ?c))))

(deffunction add-action (?t ?p)
  (assert (action (text ?t) (priority ?p))))

(deffunction print-diagnoses ()
  (printout t crlf "=== Likely Causes (Diagnosis) ===" crlf)
  (bind ?found FALSE)
  (bind ?fl (get-fact-list))
  (bind ?len (length$ ?fl))
  (loop-for-count (?i 1 ?len)
    (bind ?f (nth$ ?i ?fl))
    (if (eq (fact-relation ?f) diagnosis) then
      (bind ?found TRUE)
      (printout t "- "
                (fact-slot-value ?f text)
                "  [confidence: "
                (fact-slot-value ?f confidence)
                "]" crlf)))
  (if (eq ?found FALSE) then
    (printout t "- (none)" crlf)))

(deffunction print-fuzzy-diagnoses ()
  (printout t crlf "=== Fuzzy Diagnoses (Possibilistic) ===" crlf)
  (bind ?found FALSE)
  (bind ?fl (get-fact-list))
  (bind ?len (length$ ?fl))
  (loop-for-count (?i 1 ?len)
    (bind ?f (nth$ ?i ?fl))
    (if (eq (fact-relation ?f) fuzzy-diagnosis) then
      (bind ?found TRUE)
      (bind ?p (fact-slot-value ?f possibility))
      (bind ?rounded (/ (round (* ?p 100.0)) 100.0))
      (printout t "- "
                (fact-slot-value ?f text)
                "  [possibility: "
                ?rounded
                "]" crlf)))
  (if (eq ?found FALSE) then
    (printout t "- (none)" crlf)))

(deffunction print-actions ()
  (printout t crlf "=== Suggested Actions ===" crlf)
  (bind ?found FALSE)
  (bind ?fl (get-fact-list))
  (bind ?len (length$ ?fl))
  (loop-for-count (?i 1 ?len)
    (bind ?f (nth$ ?i ?fl))
    (if (eq (fact-relation ?f) action) then
      (bind ?found TRUE)
      (printout t "- ("
                (fact-slot-value ?f priority)
                ") "
                (fact-slot-value ?f text)
                crlf)))
  (if (eq ?found FALSE) then
    (printout t "- (none)" crlf))
  (printout t crlf "=== End of recommendations ===" crlf))

;; ---------------------------
;; Ask helpers
;; ---------------------------

(deffunction ask-symbol (?prompt ?allowed)
  (printout t ?prompt crlf "Options: " ?allowed crlf "> ")
  (bind ?ans (read))
  (while (not (member ?ans ?allowed)) do
    (printout t "Invalid. Please enter one of: " ?allowed crlf "> ")
    (bind ?ans (read)))
  (return ?ans))

(deffunction ask-1-to-5 (?prompt)
  (printout t ?prompt crlf "> ")
  (bind ?ans (read))
  (while (or (< ?ans 1) (> ?ans 5)) do
    (printout t "Invalid. Please enter a number between 1 and 5." crlf "> ")
    (bind ?ans (read)))
  (return ?ans))

;; ---------------------------
;; Fuzzy input mapping functions
;; Maps descriptive 1-5 scale to crisp domain values
;; ---------------------------

(deffunction map-signal-quality (?level)
  ;; 1=no signal(10), 2=very weak(25), 3=weak(40), 4=moderate(65), 5=strong(90)
  (if (eq ?level 1) then (return 10.0))
  (if (eq ?level 2) then (return 25.0))
  (if (eq ?level 3) then (return 40.0))
  (if (eq ?level 4) then (return 65.0))
  (return 90.0))

(deffunction map-network-load (?level)
  ;; 1=just me(10), 2=few(30), 3=several(55), 4=many(75), 5=very crowded(95)
  (if (eq ?level 1) then (return 10.0))
  (if (eq ?level 2) then (return 30.0))
  (if (eq ?level 3) then (return 55.0))
  (if (eq ?level 4) then (return 75.0))
  (return 95.0))

(deffunction map-connection-stability (?level)
  ;; 1=constantly disconnecting(10), 2=drops every few min(25),
  ;; 3=drops occasionally(45), 4=mostly stable(70), 5=perfectly stable(95)
  (if (eq ?level 1) then (return 10.0))
  (if (eq ?level 2) then (return 25.0))
  (if (eq ?level 3) then (return 45.0))
  (if (eq ?level 4) then (return 70.0))
  (return 95.0))

(deffunction map-response-time (?level)
  ;; 1=instant(25), 2=slight delay(120), 3=noticeably slow(220),
  ;; 4=very slow(350), 5=barely loading(470)
  (if (eq ?level 1) then (return 25.0))
  (if (eq ?level 2) then (return 120.0))
  (if (eq ?level 3) then (return 220.0))
  (if (eq ?level 4) then (return 350.0))
  (return 470.0))

;; ---------------------------
;; Fact finder helper
;; ---------------------------

(deffunction find-fact-by-relation (?rel)
  (bind ?fl (get-fact-list))
  (bind ?len (length$ ?fl))
  (bind ?result FALSE)
  (loop-for-count (?i 1 ?len)
    (bind ?f (nth$ ?i ?fl))
    (if (eq (fact-relation ?f) ?rel) then
      (bind ?result ?f)))
  (return ?result))

;; ---------------------------
;; Fuzzy membership function
;; Trapezoidal: rises a->b, flat b->c, falls c->d
;; All inputs cast to float to ensure decimal precision
;; ---------------------------

(deffunction membership-trapezoid (?x ?a ?b ?c ?d)
  (bind ?x (float ?x))
  (bind ?a (float ?a))
  (bind ?b (float ?b))
  (bind ?c (float ?c))
  (bind ?d (float ?d))
  (if (or (< ?x ?a) (> ?x ?d)) then (return 0.0))
  (if (and (>= ?x ?b) (<= ?x ?c)) then (return 1.0))
  (if (< ?x ?b) then (return (/ (- ?x ?a) (- ?b ?a))))
  (return (/ (- ?d ?x) (- ?d ?c))))

;; Fuzzy AND: minimum of two degrees (t-norm)
(deffunction fz-min (?a ?b)
  (if (< ?a ?b) then (return ?a) else (return ?b)))

;; ---------------------------
;; Core rules (D1 - original)
;; ---------------------------

(defrule R1-router-no-power-full-outage
  (scope (affected-devices all))
  (connectivity (wifi-connected no) (internet-working no))
  (infrastructure (router-status off))
  (not (diagnosis (text "Router appears powered off (no Wi-Fi for everyone).")))
  =>
  (add-diagnosis "Router appears powered off (no Wi-Fi for everyone)." high)
  (add-action "Check router power cable/outlet and power it on." high))

(defrule R2-modem-offline-full-outage
  (scope (affected-devices all))
  (connectivity (wifi-connected yes) (internet-working no))
  (infrastructure (modem-status offline))
  (not (diagnosis (text "ISP link down or modem not connected to ISP.")))
  =>
  (add-diagnosis "ISP link down or modem not connected to ISP." high)
  (add-action "Restart modem (power cycle 30 seconds) and check ISP line/cable." high)
  (add-action "If modem stays offline after reboot, contact ISP." high)
  (add-action "Check modem lights and restart modem first, then router." high))

(defrule R3-wifi-down-all-devices
  (scope (affected-devices all))
  (connectivity (wifi-connected no))
  (not (diagnosis (text "Wi-Fi is down for everyone (router/AP issue likely).")))
  =>
  (add-diagnosis "Wi-Fi is down for everyone (router/AP issue likely)." high)
  (add-action "Restart router/access point and verify SSID is broadcasting." high))

(defrule R4-wifi-ok-internet-down
  (scope (affected-devices all))
  (connectivity (wifi-connected yes) (internet-working no))
  (not (diagnosis (text "WAN/ISP or modem issue (Wi-Fi works but no internet).")))
  =>
  (add-diagnosis "WAN/ISP or modem issue (Wi-Fi works but no internet)." high)
  (add-action "Check modem status and WAN cable; reboot modem then router." high))

(defrule R5-single-device-cant-connect
  (scope (affected-devices one))
  (connectivity (wifi-connected no))
  (not (diagnosis (text "Single device Wi-Fi issue (device-side problem).")))
  =>
  (add-diagnosis "Single device Wi-Fi issue (device-side problem)." high)
  (add-action "On the device: toggle Wi-Fi, forget network, re-enter password." high))

(defrule R6-coverage-weak-signal
  (scope (affected-devices some))
  (infrastructure (signal-strength weak))
  (not (diagnosis (text "Coverage issue (weak signal for some devices).")))
  =>
  (add-diagnosis "Coverage issue (weak signal for some devices)." high)
  (add-action "Move closer to AP, reposition AP, or add an additional access point." high))

(defrule R7-slow-high-users
  (connectivity (slow-speed yes))
  (environment (number-of-users high))
  (not (diagnosis (text "Bandwidth saturation (too many users on limited internet).")))
  =>
  (add-diagnosis "Bandwidth saturation (too many users on limited internet)." high)
  (add-action "Limit heavy traffic, schedule updates, or upgrade ISP plan." high))

(defrule R8-slow-guest-overload-cafe
  (connectivity (slow-speed yes))
  (environment (business-type cafe))
  (not (diagnosis (text "Guest Wi-Fi overload (common in cafes).")))
  =>
  (add-diagnosis "Guest Wi-Fi overload (common in cafes)." medium)
  (add-action "Separate guest Wi-Fi and apply bandwidth limits for guest network." medium))

(defrule R9-password-changed-cant-connect
  (connectivity (wifi-connected no))
  (infrastructure (password-recently-changed yes))
  (not (diagnosis (text "Wi-Fi password changed; device has outdated credentials.")))
  =>
  (add-diagnosis "Wi-Fi password changed; device has outdated credentials." high)
  (add-action "Forget Wi-Fi network and re-enter the updated password." high))

(defrule R10-dns-error-websites-not-loading
  (symptoms (dns-error yes))
  (connectivity (internet-working yes))
  (not (diagnosis (text "DNS issue (internet up but DNS failing).")))
  =>
  (add-diagnosis "DNS issue (internet up but DNS failing)." high)
  (add-action "Switch DNS to public DNS (1.1.1.1 / 8.8.8.8) and flush DNS cache." high))

(defrule R11-auth-failure
  (security (auth-fail yes))
  (not (diagnosis (text "Wi-Fi authentication failure (wrong password or security mismatch).")))
  =>
  (add-diagnosis "Wi-Fi authentication failure (wrong password or security mismatch)." high)
  (add-action "Verify Wi-Fi password and security type (WPA2/WPA3); re-join network." high))

(defrule R12-captive-portal
  (security (captive-portal yes))
  (connectivity (wifi-connected yes) (internet-working no))
  (not (diagnosis (text "Captive portal may be blocking internet access (login required).")))
  =>
  (add-diagnosis "Captive portal may be blocking internet access (login required)." medium)
  (add-action "Open a browser and complete captive portal login/terms page." medium))

(defrule R13-vpn-required-only-some-apps
  (security (vpn-required yes))
  (symptoms (only-some-apps-bad yes))
  (not (diagnosis (text "Some business apps may require VPN access.")))
  =>
  (add-diagnosis "Some business apps may require VPN access." medium)
  (add-action "Connect to company VPN, then retry the affected apps/services." medium))

(defrule R14-dhcp-problem
  (services (dhcp-ok no))
  (connectivity (wifi-connected yes))
  (not (diagnosis (text "DHCP issue (clients may not be getting valid IP configuration).")))
  =>
  (add-diagnosis "DHCP issue (clients may not be getting valid IP configuration)." high)
  (add-action "Renew IP address (release/renew) and restart DHCP service/router." high)
  (add-action "Check router DHCP scope/pool and ensure it is not exhausted." medium))

(defrule R15-dns-service-down
  (services (dns-ok no))
  (connectivity (wifi-connected yes))
  (not (diagnosis (text "DNS service issue (name resolution failing).")))
  =>
  (add-diagnosis "DNS service issue (name resolution failing)." high)
  (add-action "Change DNS to public DNS and verify router DNS settings." high))

(defrule R16-cloud-service-down
  (services (cloud-service-up no))
  (symptoms (only-some-apps-bad yes))
  (not (diagnosis (text "Cloud service outage (problem is likely external to Wi-Fi).")))
  =>
  (add-diagnosis "Cloud service outage (problem is likely external to Wi-Fi)." medium)
  (add-action "Check vendor status page and wait/contact provider." medium))

(defrule R17-packet-loss-interference
  (performance (packet-loss yes))
  (infrastructure (signal-strength weak))
  (not (diagnosis (text "Packet loss likely due to interference or poor wireless link.")))
  =>
  (add-diagnosis "Packet loss likely due to interference or poor wireless link." high)
  (add-action "Change Wi-Fi channel, reduce interference, or reposition AP." high))

(defrule R18-high-latency-congestion
  (performance (latency-high yes))
  (environment (number-of-users high))
  (not (diagnosis (text "High latency due to network congestion.")))
  =>
  (add-diagnosis "High latency due to network congestion." high)
  (add-action "Enable QoS, prioritize critical traffic, and reduce background load." high))

(defrule R19-jitter-critical-traffic
  (performance (jitter-high yes))
  (environment (critical-service pos))
  (not (diagnosis (text "High jitter can disrupt real-time services (POS/voice/video).")))
  =>
  (add-diagnosis "High jitter can disrupt real-time services (POS/voice/video)." medium)
  (add-action "Enable QoS for critical traffic and reduce background uploads." medium))

(defrule R20-slow-only-general
  (connectivity (wifi-connected yes) (internet-working yes) (slow-speed yes))
  (environment (number-of-users low|medium))
  (not (diagnosis (text "Slow internet despite Wi-Fi connected (possible ISP throttling or background traffic).")))
  =>
  (add-diagnosis "Slow internet despite Wi-Fi connected (possible ISP throttling or background traffic)." medium)
  (add-action "Run a speed test and check if backups/updates/streams are consuming bandwidth." high)
  (add-action "Enable QoS or limit heavy traffic; consider upgrading ISP plan if consistently slow." medium))

(defrule R21-only-some-apps-service-specific
  (connectivity (wifi-connected yes) (internet-working yes))
  (symptoms (only-some-apps-bad yes) (dns-error no|unknown))
  (not (diagnosis (text "Only some apps fail (service-specific issue or firewall/content filter).")))
  =>
  (add-diagnosis "Only some apps fail (service-specific issue or firewall/content filter)." medium)
  (add-action "Test the same app on another network (phone hotspot) to confirm service-side issue." high)
  (add-action "Check firewall/content filtering rules or proxy settings that might block the app." medium))

(defrule R22-password-changed-some-devices
  (scope (affected-devices one|some))
  (connectivity (wifi-connected no))
  (infrastructure (password-recently-changed yes))
  (not (diagnosis (text "Wi-Fi password was changed; some devices still have old saved credentials.")))
  =>
  (add-diagnosis "Wi-Fi password was changed; some devices still have old saved credentials." high)
  (add-action "On affected devices: Forget the Wi-Fi network and reconnect with the updated password." high)
  (add-action "If it still fails, verify WPA2/WPA3 compatibility and reboot the device." medium))

(defrule R23-intermittent-isp-instability
  (connectivity (intermittent yes))
  (infrastructure (modem-status offline|unknown))
  (not (diagnosis (text "Intermittent connection likely due to ISP line/modem instability.")))
  =>
  (add-diagnosis "Intermittent connection likely due to ISP line/modem instability." high)
  (add-action "Power-cycle modem (unplug 30s) and check ISP cables/line connections." high)
  (add-action "If modem frequently goes offline, contact ISP and report intermittent drops." medium))

(defrule R24-dhcp-unknown-or-no-internet-down
  (connectivity (wifi-connected yes) (internet-working no))
  (services (dhcp-ok no|unknown))
  (not (diagnosis (text "Possible DHCP/IP assignment issue (connected to Wi-Fi but no internet).")))
  =>
  (add-diagnosis "Possible DHCP/IP assignment issue (connected to Wi-Fi but no internet)." medium)
  (add-action "Reconnect device to renew IP; restart router DHCP if multiple devices affected." high)
  (add-action "Check DHCP pool size (IP exhaustion) and expand leases if needed." medium))

;; ---------------------------
;; D2 TODO 3: Fuzzy rules (RF1 to RF10)
;; Membership degrees computed manually using trapezoid function
;; Fuzzy AND (RF8, RF9, RF10) uses fz-min (t-norm)
;; ---------------------------

;; RF1: Poor signal quality strongly suggests a coverage gap
;; poor: trapezoid(0, 0, 30, 50)
(defrule RF1-poor-signal-coverage-high
  (signal-quality poor)
  (fuzzy-inputs (signal-quality-val ?sq))
  (not (fuzzy-diagnosis (text "Coverage gap detected: signal quality is poor.")))
  =>
  (bind ?degree (membership-trapezoid ?sq 0.0 0.0 30.0 50.0))
  (assert (fuzzy-diagnosis
    (text "Coverage gap detected: signal quality is poor.")
    (possibility ?degree)))
  (add-action "Reposition or add access points to eliminate coverage gaps." high))

;; RF2: Marginal signal suggests intermittent coverage issues
;; marginal: trapezoid(30, 50, 50, 70)
(defrule RF2-marginal-signal-coverage-medium
  (signal-quality marginal)
  (fuzzy-inputs (signal-quality-val ?sq))
  (not (fuzzy-diagnosis (text "Marginal signal: intermittent coverage issues possible.")))
  =>
  (bind ?degree (membership-trapezoid ?sq 30.0 50.0 50.0 70.0))
  (assert (fuzzy-diagnosis
    (text "Marginal signal: intermittent coverage issues possible.")
    (possibility ?degree)))
  (add-action "Monitor signal strength and consider repositioning the access point." medium))

;; RF3: Heavy network load strongly suggests bandwidth saturation
;; heavy: trapezoid(50, 80, 100, 100)
(defrule RF3-heavy-load-saturation-high
  (network-load heavy)
  (fuzzy-inputs (network-load-val ?nl))
  (not (fuzzy-diagnosis (text "Bandwidth saturation: network load is heavy.")))
  =>
  (bind ?degree (membership-trapezoid ?nl 50.0 80.0 100.0 100.0))
  (assert (fuzzy-diagnosis
    (text "Bandwidth saturation: network load is heavy.")
    (possibility ?degree)))
  (add-action "Apply QoS policies and limit non-critical bandwidth usage." high))

;; RF4: Moderate network load may cause slowness
;; moderate: trapezoid(30, 50, 50, 70)
(defrule RF4-moderate-load-saturation-medium
  (network-load moderate)
  (fuzzy-inputs (network-load-val ?nl))
  (not (fuzzy-diagnosis (text "Moderate network load: some users may experience slowness.")))
  =>
  (bind ?degree (membership-trapezoid ?nl 30.0 50.0 50.0 70.0))
  (assert (fuzzy-diagnosis
    (text "Moderate network load: some users may experience slowness.")
    (possibility ?degree)))
  (add-action "Monitor bandwidth usage and consider scheduling large transfers off-peak." medium))

;; RF5: Unstable connection strongly suggests ISP or modem instability
;; unstable: trapezoid(0, 0, 30, 50)
(defrule RF5-unstable-connection-isp
  (connection-stability unstable)
  (fuzzy-inputs (connection-stability-val ?cs))
  (not (fuzzy-diagnosis (text "ISP or modem instability: connection is highly unstable.")))
  =>
  (bind ?degree (membership-trapezoid ?cs 0.0 0.0 30.0 50.0))
  (assert (fuzzy-diagnosis
    (text "ISP or modem instability: connection is highly unstable.")
    (possibility ?degree)))
  (add-action "Power-cycle modem and contact ISP if instability persists." high))

;; RF6: Flaky connection may indicate line or modem issues
;; flaky: trapezoid(30, 50, 50, 70)
(defrule RF6-flaky-connection-isp
  (connection-stability flaky)
  (fuzzy-inputs (connection-stability-val ?cs))
  (not (fuzzy-diagnosis (text "Flaky connection: possible line or modem degradation.")))
  =>
  (bind ?degree (membership-trapezoid ?cs 30.0 50.0 50.0 70.0))
  (assert (fuzzy-diagnosis
    (text "Flaky connection: possible line or modem degradation.")
    (possibility ?degree)))
  (add-action "Check physical modem/cable connections and run a line quality test." medium))

;; RF7: Slow response time strongly suggests congestion or routing issue
;; slow: trapezoid(150, 300, 500, 500)
(defrule RF7-slow-response-congestion
  (response-time slow)
  (fuzzy-inputs (response-time-val ?rt))
  (not (fuzzy-diagnosis (text "Congestion or routing issue: response time is slow.")))
  =>
  (bind ?degree (membership-trapezoid ?rt 150.0 300.0 500.0 500.0))
  (assert (fuzzy-diagnosis
    (text "Congestion or routing issue: response time is slow.")
    (possibility ?degree)))
  (add-action "Check for congested channels, enable QoS, and verify routing table." high))

;; RF8: Acceptable response time with heavy load = borderline congestion risk
;; Fuzzy AND = fz-min of acceptable(rt) and heavy(nl)
;; acceptable: trapezoid(50, 150, 150, 300)
;; heavy:      trapezoid(50, 80, 100, 100)
(defrule RF8-acceptable-response-heavy-load-risk
  (response-time acceptable)
  (network-load heavy)
  (fuzzy-inputs (response-time-val ?rt) (network-load-val ?nl))
  (not (fuzzy-diagnosis (text "Borderline congestion risk: load is heavy despite acceptable response time.")))
  =>
  (bind ?d-rt (membership-trapezoid ?rt 50.0 150.0 150.0 300.0))
  (bind ?d-nl (membership-trapezoid ?nl 50.0 80.0 100.0 100.0))
  (bind ?degree (fz-min ?d-rt ?d-nl))
  (assert (fuzzy-diagnosis
    (text "Borderline congestion risk: load is heavy despite acceptable response time.")
    (possibility ?degree)))
  (add-action "Proactively apply bandwidth limits before congestion worsens." low))

;; RF9: Poor signal AND unstable connection suggest RF interference
;; Fuzzy AND = fz-min of poor(sq) and unstable(cs)
;; poor:     trapezoid(0, 0, 30, 50)
;; unstable: trapezoid(0, 0, 30, 50)
(defrule RF9-poor-signal-unstable-interference
  (signal-quality poor)
  (connection-stability unstable)
  (fuzzy-inputs (signal-quality-val ?sq) (connection-stability-val ?cs))
  (not (fuzzy-diagnosis (text "RF interference likely: poor signal combined with high instability.")))
  =>
  (bind ?d-sq (membership-trapezoid ?sq 0.0 0.0 30.0 50.0))
  (bind ?d-cs (membership-trapezoid ?cs 0.0 0.0 30.0 50.0))
  (bind ?degree (fz-min ?d-sq ?d-cs))
  (assert (fuzzy-diagnosis
    (text "RF interference likely: poor signal combined with high instability.")
    (possibility ?degree)))
  (add-action "Scan for interfering networks, change Wi-Fi channel, and check for physical obstructions." high))

;; RF10: Slow response time AND heavy load = severe network degradation
;; Fuzzy AND = fz-min of slow(rt) and heavy(nl)
;; slow:  trapezoid(150, 300, 500, 500)
;; heavy: trapezoid(50, 80, 100, 100)
(defrule RF10-slow-response-heavy-load-severe
  (response-time slow)
  (network-load heavy)
  (fuzzy-inputs (response-time-val ?rt) (network-load-val ?nl))
  (not (fuzzy-diagnosis (text "Severe network degradation: high load and slow response time combined.")))
  =>
  (bind ?d-rt (membership-trapezoid ?rt 150.0 300.0 500.0 500.0))
  (bind ?d-nl (membership-trapezoid ?nl 50.0 80.0 100.0 100.0))
  (bind ?degree (fz-min ?d-rt ?d-nl))
  (assert (fuzzy-diagnosis
    (text "Severe network degradation: high load and slow response time combined.")
    (possibility ?degree)))
  (add-action "Immediately reduce network load, prioritize critical services, and consider hardware upgrade." high))

;; ---------------------------
;; Printing rules
;; ---------------------------

(defrule PRINT-diagnosis
  (declare (salience -1000))
  ?m <- (meta (printed-diagnosis no))
  =>
  (print-diagnoses)
  (modify ?m (printed-diagnosis yes)))

(defrule PRINT-fuzzy-diagnosis
  (declare (salience -1001))
  ?m <- (meta (printed-diagnosis yes) (printed-actions no))
  =>
  (print-fuzzy-diagnoses))

(defrule PRINT-actions
  (declare (salience -1002))
  ?m <- (meta (printed-actions no))
  =>
  (print-actions)
  (modify ?m (printed-actions yes)))

;; ---------------------------
;; Set functions
;; ---------------------------

(deffunction set-scope (?val)
  (bind ?f (find-fact-by-relation scope))
  (if (neq ?f FALSE) then (modify ?f (affected-devices ?val))))

(deffunction set-environment (?bt ?users ?critical)
  (bind ?f (find-fact-by-relation environment))
  (if (neq ?f FALSE) then
    (modify ?f (business-type ?bt) (number-of-users ?users) (critical-service ?critical))))

(deffunction set-connectivity (?wifi ?internet ?lan ?slow ?inter)
  (bind ?f (find-fact-by-relation connectivity))
  (if (neq ?f FALSE) then
    (modify ?f (wifi-connected ?wifi) (internet-working ?internet)
              (lan-working ?lan) (slow-speed ?slow) (intermittent ?inter))))

(deffunction set-infra (?router ?modem ?signal ?apcount ?recentcfg ?pwchanged)
  (bind ?f (find-fact-by-relation infrastructure))
  (if (neq ?f FALSE) then
    (modify ?f (router-status ?router) (modem-status ?modem)
              (signal-strength ?signal) (access-point-count ?apcount)
              (recent-config-change ?recentcfg) (password-recently-changed ?pwchanged))))

(deffunction set-symptoms (?web ?dns ?someapps)
  (bind ?f (find-fact-by-relation symptoms))
  (if (neq ?f FALSE) then
    (modify ?f (websites-not-loading ?web) (dns-error ?dns) (only-some-apps-bad ?someapps))))

(deffunction set-security (?auth ?portal ?vpn)
  (bind ?f (find-fact-by-relation security))
  (if (neq ?f FALSE) then
    (modify ?f (auth-fail ?auth) (captive-portal ?portal) (vpn-required ?vpn))))

(deffunction set-services (?dhcp ?dnsok ?cloud)
  (bind ?f (find-fact-by-relation services))
  (if (neq ?f FALSE) then
    (modify ?f (dhcp-ok ?dhcp) (dns-ok ?dnsok) (cloud-service-up ?cloud))))

(deffunction set-performance (?lat ?loss ?jit)
  (bind ?f (find-fact-by-relation performance))
  (if (neq ?f FALSE) then
    (modify ?f (latency-high ?lat) (packet-loss ?loss) (jitter-high ?jit))))

;; ---------------------------
;; Quick Mode
;; ---------------------------

(deffunction user-input-and-run ()
  (reset)
  (printout t crlf "=== Small Business Wi-Fi Troubleshooting (Fuzzy Mode) ===" crlf)

  (bind ?scope
        (ask-symbol "1) How many devices are affected? (one/some/all)"
                    (create$ one some all)))
  (set-scope ?scope)

  (bind ?wifi
        (ask-symbol "2) Is Wi-Fi connected? (yes/no)"
                    (create$ yes no)))
  (bind ?internet
        (ask-symbol "3) Is internet working? (yes/no)"
                    (create$ yes no)))
  (bind ?slow
        (ask-symbol "4) Is it slow? (yes/no)"
                    (create$ yes no)))
  (set-connectivity ?wifi ?internet unknown ?slow unknown)

  (bind ?bt
        (ask-symbol "5) Business type? (cafe/office/store/other)"
                    (create$ cafe office store other)))
  (bind ?users
        (ask-symbol "6) Number of users right now? (low/medium/high)"
                    (create$ low medium high)))
  (set-environment ?bt ?users unknown)

  (bind ?router
        (ask-symbol "7) Router status? (normal/off/unknown)"
                    (create$ normal off unknown)))
  (bind ?modem
        (ask-symbol "8) Modem status? (online/offline/unknown)"
                    (create$ online offline unknown)))
  (bind ?pw
        (ask-symbol "9) Password recently changed? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (set-infra ?router ?modem unknown unknown unknown ?pw)

  (bind ?dns
        (ask-symbol "10) DNS error? (yes/no/unknown)"
                    (create$ yes no unknown)))
  (set-symptoms unknown ?dns unknown)

  ;; Fuzzy inputs - descriptive 1-5 scale
  (printout t crlf "--- Fuzzy Assessment (rate 1 to 5) ---" crlf)

  (bind ?sq-level
        (ask-1-to-5
          "11) Signal quality?
  1 = No signal / constantly dropping
  2 = Very weak (1-2 bars)
  3 = Weak (2-3 bars)
  4 = Moderate (3-4 bars)
  5 = Strong (4-5 bars / full bars)"))
  (bind ?sq (map-signal-quality ?sq-level))

  (bind ?nl-level
        (ask-1-to-5
          "12) How many devices/people actively using the network?
  1 = Just me / almost nobody
  2 = A few people (2-5)
  3 = Several people (6-15)
  4 = Many people (16-30)
  5 = Very crowded (30+)"))
  (bind ?nl (map-network-load ?nl-level))

  (bind ?cs-level
        (ask-1-to-5
          "13) How stable is your connection?
  1 = Constantly disconnecting
  2 = Drops every few minutes
  3 = Drops occasionally
  4 = Mostly stable with rare drops
  5 = Perfectly stable"))
  (bind ?cs (map-connection-stability ?cs-level))

  (bind ?rt-level
        (ask-1-to-5
          "14) How would you describe page/app loading speed?
  1 = Instant / no delay
  2 = Slight delay but acceptable
  3 = Noticeably slow
  4 = Very slow (takes several seconds)
  5 = Barely loading / timing out"))
  (bind ?rt (map-response-time ?rt-level))

  ;; Store crisp values for membership computation in rule RHS
  (bind ?fi (find-fact-by-relation fuzzy-inputs))
  (modify ?fi
    (signal-quality-val ?sq)
    (network-load-val ?nl)
    (connection-stability-val ?cs)
    (response-time-val ?rt))

  ;; Assert singleton fuzzy facts for LHS pattern matching
  (assert (signal-quality (?sq 1)))
  (assert (network-load (?nl 1)))
  (assert (connection-stability (?cs 1)))
  (assert (response-time (?rt 1)))

  (printout t crlf "=== Running inference... ===" crlf)
  (run))