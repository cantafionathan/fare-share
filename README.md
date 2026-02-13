# Disentangling Regulatory and Pandemic Effects

## Project Overview

In January 2021, Seattle implemented a minimum compensation ordinance for ride-share drivers. This study utilizes a **Bayesian synthetic control framework** to estimate the "lift" in transit ridership following the policy, using Portland, Oregon, as the primary donor city.

## Technical Stack

-   **Probabilistic Programming:** Models implemented in **Stan** via the **`cmdstanr`** interface.
-   **Diagnostics:** Performance evaluated using **PSIS-LOO (ELPD)**, **Pareto** $\hat k$, and MCMC convergence metrics ($\hat{R}$ and ESS).

------------------------------------------------------------------------

*This project was completed for **STAT 547D: Bayesian Workflow** at the University of British Columbia.*
