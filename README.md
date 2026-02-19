# COMP474-Project
COMP474 Project - Option C

📌 Project Overview

This project implements a rule-based expert system for diagnosing and resolving common network and Wi-Fi issues in small-to-medium sized businesses.

The system is developed for COMP 474 – Intelligent Systems (Winter 2026) and is implemented using the CLIPS declarative programming language.

The expert system assists non-technical users in systematically identifying the root causes of connectivity and performance problems and provides prioritized corrective actions.


🎯 Project Goal

The goal of this project is to design and implement a knowledge-based expert system that:

Diagnoses common network issues (Wi-Fi failure, slow speeds, DNS problems, ISP outages, etc.)

Provides explainable diagnoses with confidence levels

Suggests prioritized corrective actions

Helps users determine when to escalate to professional IT support

The system focuses on small businesses (10–50 employees) without dedicated IT staff.


🧠 Knowledge Base Structure (K = F + R)

The knowledge base is divided into:

📂 Factbase (F.clp)

Defines structured templates representing:

Scope of issue (one/some/all devices)

Connectivity state

Infrastructure status

Environment context

Security conditions

Services (DHCP, DNS, cloud)

Performance metrics (latency, packet loss, jitter)

Facts are expressed using structured English and natural naming conventions.

📂 Rulebase (R.clp)

Contains 21+ IF–THEN rules that:

Analyze combinations of facts

Infer likely diagnoses

Assign confidence levels (low/medium/high)

Generate prioritized corrective actions

The system uses forward chaining inference in CLIPS.
