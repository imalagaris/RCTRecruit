---
title: 'RCTRecruit: An R Package for Predicting Clinical Trial Recruitment Using a Flexible, Nonparametric Approach'
tags:
  - R
  - clinical trials
  - recruitment prediction
  - non-parametric
  - resampling
  - biostatistics
authors:
  - name: Alejandro Villasante-Tezanos
    affiliation: 1
    corresponding: true
  - name: Chris K. Kurinec
    affiliation: 1
  - name: Alex Tan
    affiliation: 1
  - name: Ioannis Malagaris
    affiliation: 1
  - name: Yong-Fang Kuo
    affiliation: 1
  - name: Xiaoying Yu
    affiliation: 1
affiliations:
  - name: Department of Biostatistics & Data Science, University of Texas Medical Branch, Galveston, Texas, United States
    index: 1
date: 12 May 2025
bibliography: paper.bib
editor_options: 
  markdown: 
    wrap: 72
---

# Summary

Accurate prediction of subject recruitment is essential for clinical
trial success. `RCTRecruit` is an R package that employs a novel,
flexible, non-parametric weighted resampling approach to predict
clinical trial recruitment. Unlike traditional models relying on strict
parametric or Bayesian assumptions, `RCTRecruit` utilizes empirical
historical data to simulate future enrollment. It assigns higher
selection probabilities to historical data from calendar weeks analogous
to the target prediction window, capturing seasonal enrollment patterns
and temporal trends. Using these weighted resampling-based techniques,
`RCTRecruit` generates simulated distributions and confidence intervals
for recruitment projections. Requiring one year of historical data as
the input - without requiring users to have existing statistical or
programming expertise - the package accounts for anomalies (e.g.,
enrollment gaps due to holiday breaks, staff shortage, clinic closure,
etc.) and external enrollment factors, providing a practical and
intuitive solution for investigators seeking reliable recruitment
forecasts. The flexibility and simplicity of `RCTRecruit` make it a
valuable tool for clinical trial monitoring, improving enrollment
predictions, and overall trial efficiency. For investigators, having a
transparent, open-source tool directly within the R ecosystem bridges
the gap between complex statistical theory and daily trial operations.
By standardizing these non-parametric methods into accessible
computational functions, `RCTRecruit` ensures that trial monitoring
reports are both statistically rigorous and fully reproducible across
diverse clinical research environments.

# Statement of Need

The recruitment process remains a major bottleneck in trial execution.
Up to 60% of trials experience delays or termination due to insufficient
enrollment [@huang_clinical_2018]. Investigators must accurately
forecast whether target enrollment numbers can be achieved within the
funded period, and determine if recruitment strategies require
adjustment [@healy_identifying_2018; @kasenda_prediction_2020]. Data and
Safety Monitoring Boards (DSMBs) rely heavily on these longitudinal
forecasts to evaluate the ongoing viability of public health
interventions. When trials are underpowered due to recruitment
shortfalls, the statistical validity of the entire study is jeopardized,
potentially delaying the delivery of critical therapeutic interventions
to vulnerable patient populations. Furthermore, grant funding agencies
increasingly require detailed, statistically sound recruitment
justifications before allocating resources to large-scale clinical
trials.

Inaccurate estimates of recruitment rates carry measurable consequences,
leading to missed targets, increased participant dropout rates,
premature trial termination, and compromised data integrity
[@sathian_impact_2020; @gkioni_statistical_2020]. `RCTRecruit` addresses
this requirement. It provides a data-driven solution for generating
long-term recruitment forecasts without requiring advanced programming
expertise or rigid statistical assumptions that frequently fail in
clinical environments. By equipping investigators with simple, reliable,
data-driven forecasting tools, `RCTRecruit` actively mitigates the
uncertainty of trial recruitment, and ensures that resources are
utilized with maximum efficiency.

# State of the Field

Several models exist to predict trial recruitment
[@barnard_systematic_2010; @gkioni_systematic_2019], but they often
introduce assumptions that do not hold in clinical conditions.
Parametric models rely on specific distributional assumptions. Poisson
models are commonly utilized for fixed-interval predictions
[@lee_interim_1983; @carter_practical_2005], while Gamma distributions
are applied to model varying recruitment rates across active trial
centers [@anisimov_predictive_2011]. Bayesian models offer predictive
power but require the specification of prior knowledge regarding
recruitment rates, which can be difficult to define accurately and is
not always available at the start of a trial [@jiang_modeling_2015;
@jiang_bayesian_2016]. The rigid nature of these parametric frameworks
severely limits their practical utility when analyzing the inherently
noisy, longitudinal data generated by real-world initiatives.

Actual recruitment patterns frequently violate these traditional models
due to predictable seasonal adjustments, such as decreased staff and
patient availability during holidays [@moffat_factors_2023], or
unpredictable disruptions like the COVID-19 pandemic
[@medidata_solutions_covid-19_2020]. Unweighted empirical methods are
sometimes used for their ease of implementation, but they lack the
flexibility to capture real-world complexities
[@gkioni_statistical_2020]. `RCTRecruit` offers a non-parametric
framework [@villasante-tezanos_non-parametric_2024] that allows
empirical, historical recruitment data to directly inform predictions.
This fills a distinct methodological gap in the current open-source
software landscape, providing investigators with a robust computational
alternative that inherently respects the natural variance, clustering,
and longitudinal idiosyncrasies intrinsic to clinical trial enrollment
data.

# Software Design and Methodology

The core architecture of `RCTRecruit` relies on a weighted resampling
methodology utilizing empirical enrollment data from a preceding year.
To predict recruitment for a specific future week, the algorithm samples
from the historical dataset, assigning higher selection probabilities to
historical weeks that are temporally analogous to the target week.

*Weighting Functions*: Users customize the temporal sensitivity of the
simulations using two primary probability mass functions (PMF). By
default, weight functions are generated using a Binomial distribution
with parameters $n=51$ and $p=0.5$. This anchors the peak of the
probability curve exactly at the calendar week in the empirical data
corresponding to the week of interest. Alternatively, users can select a
Cauchy distribution centered at $0$ with a scale parameter of $1$. The
Cauchy PMF possesses heavier tails, accommodating scenarios where
enrollment patterns exhibit longer-range temporal correlations.

*Adjusting for Anomalies and Efficiency*: To prevent simulations from
being skewed by unrepeatable historical events, users can designate gap
weeks, prolonged periods with zero enrollment that are not expected to
recur. The software replaces sampled subjects from these weeks with
their expected statistical value. Furthermore, the package allows users
to apply predetermined efficiency factors (e.g., practical adjustments
reflecting changes in recruitment resources or staffing levels) to
simulate anticipated increases or decreases in recruitment resources.

*Computational Implementation*: The package is explicitly designed to
integrate seamlessly into standard biostatistical reporting workflows.
It leverages foundational R capabilities to drive its core resampling
engine, ensuring high computational efficiency even when executing
massive permutation loops for extensive simulation scenarios.
Furthermore, the output objects generated by the predictive functions
are purposefully structured to facilitate straightforward integration
with popular data visualization libraries and enable seamless
incorporation into automated DSMB reporting pipelines.

*Core Outputs and Validation*: By performing resampling with replacement
(defaulting to 10,000 iterations), `RCTRecruit` outputs critical metrics
for trial management:

1.  Cumulative Projections: Total accumulated weekly recruitment over
    time, providing the median trajectory and a 95% projection band
    (`GetWeekPredCI`).
2.  Timeline Estimations: The projected time required to enroll a target
    number ($N$) of subjects (`Time2Nsubjects`).
3.  Model Validation: A Euclidean Distance (ED) metric (`GetDistance`)
    that assesses similarity between target ongoing enrollment data and
    simulated cumulative enrollment sequences, assessing predictive
    accuracy and validating the chosen historical baseline.

# Research Impact Statement

The integration of `RCTRecruit` into clinical trial planning facilitates
realistic operational timelines and optimized resource allocation. By
relaxing the need for parametric and Bayesian assumptions, the package
allows investigators to model diverse enrollment patterns accurately.
This framework empowers research committees to assess ongoing
feasibility and adapt recruitment strategies proactively, contributing
to the efficient execution of medical research. Furthermore, the
availability of this methodology as an open-source R package
democratizes access to advanced predictive analytics for research teams
operating with limited funding or statistical and/or programming
support. Ultimately, by providing a robust, transparent computational
tool for trial forecasting - and leveraging actual recruitment data -
`RCTRecruit` provides a data-driven framework that increases the
successful execution of clinical studies and advancements in medical
research.

# AI Usage Disclosure

No artificial intelligence tools were used in the development of the
software code.
