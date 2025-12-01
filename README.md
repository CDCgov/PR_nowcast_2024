---
editor_options: 
  markdown: 
    wrap: 72
---

# **Real-time dengue nowcasting: Lessons learned from the 2024 outbreak in Puerto Rico**

**General disclaimer:** This repository was created for use by CDC
programs to collaborate on public health–related projects in support of
the [CDC
mission](https://www.cdc.gov/about/cdc/#cdc_about_cio_mission-our-mission).
GitHub is not hosted by the CDC, but is a third-party website used by
CDC and its partners to share information and collaborate on software.
CDC use of GitHub does not imply an endorsement of any one particular
service, product, or enterprise.

------------------------------------------------------------------------

## **Overview**

This repository supports the manuscript *“Lessons learned from real-time
nowcasting: The 2024 dengue outbreak in Puerto Rico”* (see citation
below). It provides code, documentation, and reproducible workflows used
to perform real-time dengue nowcasting during the 2024 outbreak and
retrospective analyses that informed operational decision-making.

The materials here demonstrate implementation of **Nowcasting by
Bayesian Smoothing (NobBS)** using a generated sample case-level data.

------------------------------------------------------------------------

## **Background**

Delays in case reporting complicated situational awareness and
operational response. We addressed this by applying NobBS to generate
weekly nowcasts of case incidence for PR dengue outbreak in 2024,
accounting for changing reporting delays.

The analyses revealed three key lessons for real-time dengue nowcasting:

1.  **Stable reporting patterns and ongoing evaluation** are essential
    for reliable estimates.
2.  **Adaptive modeling strategies** (e.g., adjusting delay
    distributions, shared-parameter modeling) are needed under changing
    reporting or low-case-count setting.
3.  **Close collaboration with local public health partners** is
    critical for interpreting outputs and translating them into timely
    action.

The repository includes both analyses performed on a generated sample of
national and subpopulations data.

------------------------------------------------------------------------

## **Repository Contents**

-   **`/data/denv_sample_dat.csv`** – Example (de-identified) inputs or
    data templates for structuring nowcasting data.

-   **`Run_nowcast_codes.R`** – Primary R workflow for running NobBS and
    baseline nowcasts.

    -   Fits NobBS models (independent- and shared-parameter models).
    -   Generates results from baseline model.
    -   Generates estimates and uncertainty intervals.

-   **`NobBS_nation_nowcast.R`**, **`NobBS_subnation_nowcast.R`**,
    **`baseline_nation_nowcast.R`**, **`baseline_subnation_nowcast.R`**
    – Scripts for running NobBS and baseline models in nation or
    subpopulation dataset.

-   **`nowcast_functions.R`** – Scripts for running NobBS model in stan.

-   **`model_evaluation.R`** – Scripts for model evaluation (50%, 95%
    intervals coverage and WIS)

-   **`Plot.R`** – Scripts for plotting, comparing nowcasts from
    different models.

-   **`/output/`** – Folder to store results from NobBS and baseline.

------------------------------------------------------------------------

## **Usage Instructions**

1.  **Prepare your data** Format line-listed dengue case data with
    fields required for NobBS (e.g., onset date, report date). Store
    de-identified files in the `/data` directory.

2.  **Run the nowcast workflow** Execute **`Run_nowcast_codes`**`.R` to:

    -   fit NobBS models,
    -   generate results from base line model,
    -   produce posterior distribution of nowcasts.

3.  **Evaluate model performance** Use scripts in `/model_evaluation.R`
    to compute 50%, 95% coverage and WIS (decompose scoring components,
    and compare independent vs. shared-parameter models).

------------------------------------------------------------------------

## **Citation**

If you use these materials or build on the modeling approach, please
cite:

**Tran Q et al. Lessons learned from real-time nowcasting: The 2024
dengue outbreak in Puerto Rico.** *[Journal name, year – to be
updated]*.

------------------------------------------------------------------------

## **Public Domain Standard Notice**

This repository constitutes a work of the United States Government and
is not subject to domestic copyright protection under 17 USC §105. This
repository is in the public domain within the United States, and
copyright and related rights in the work worldwide are waived through
the [CC0 1.0 Universal public domain
dedication](https://creativecommons.org/publicdomain/zero/1.0/). All
contributions to this repository will be released under the CC0
dedication. By submitting a pull request you agree to this waiver of
copyright interest.

------------------------------------------------------------------------

## **License Standard Notice**

The repository utilizes code licensed under the terms of the Apache
Software License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it
and/or modify it under the terms of the Apache Software License version
2, or (at your option) any later version.

This source code in this repository is distributed in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
Apache Software License for more details.

You should have received a copy of the Apache Software License along
with this program. If not, see
<http://www.apache.org/licenses/LICENSE-2.0.html>

The source code forked from other open source projects will inherit its
license.

------------------------------------------------------------------------

## **Privacy Standard Notice**

Anyone is encouraged to contribute to the repository by
[forking](https://help.github.com/articles/fork-a-repo) and submitting a
pull request. (If you are new to GitHub, you might start with a [basic
tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual,
irrevocable, non-exclusive, transferable license to all users under the
terms of the [Apache Software License
v2](http://www.apache.org/licenses/LICENSE-2.0.html) or later.

All comments, messages, pull requests, and other submissions received
through CDC including this GitHub page may be subject to applicable
federal law, including but not limited to the Federal Records Act, and
may be archived. Learn more at <http://www.cdc.gov/other/privacy.html>.

------------------------------------------------------------------------

## **Contributing Standard Notice**

Contributions are welcome through forks and pull requests. By
contributing, you grant a world-wide, royalty-free, perpetual,
irrevocable, non-exclusive, transferable license to all users under the
terms of the Apache Software License v2 or later.

All comments, messages, and submissions may be subject to applicable
federal law and recordkeeping requirements.

------------------------------------------------------------------------

## **Records Management Standard Notice**

This repository is not a source of government records, but is a copy to
increase collaboration and collaborative potential. All government
records will be published through the [CDC web
site](http://www.cdc.gov/).

## **Additional Standard Notices**

Please refer to [CDC's Template
Repository](https://github.com/CDCgov/template) for more information
about [contributing to this
repository](https://github.com/CDCgov/template/blob/main/CONTRIBUTING.md),
[public domain notices and
disclaimers](https://github.com/CDCgov/template/blob/main/DISCLAIMER.md),
and [code of
conduct](https://github.com/CDCgov/template/blob/main/code-of-conduct.md).
