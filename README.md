# Earthquake activity in the North-East of the Netherlands

This repository contains the code and documentation that can be used to achieve the following to tasks:
* Generating a monthly report, containing an estimate for the magnitude of completeness, $m_c$, as well as visualisations of the earthquake activity in the most recent month
* Producing a Markov chain Monte Carlo (MCMC) storage guide based on a quantitative simulation study on different pre-allocation strategies

Table of contents:
<!-- vim-markdown-toc GFM -->

* [Repository overview](#repository-overview)
* [Requirements](#requirements)
* [Producing the monthly summary](#producing-the-monthly-summary)
* [Producing the Markov chain Monte Carlo storage guide](#producing-the-markov-chain-monte-carlo-storage-guide)
* [Configuring the monthly summary](#configuring-the-monthly-summary)
* [Configuring the storage guide](#configuring-the-storage-guide)

<!-- vim-markdown-toc -->

## Repository overview

* `config.yaml`:
  * Configuration file for controlling various aspects of the report generation. More details in [Configuring the monthly summary](#configuring-the-monthly-summary) and [Configuring the storage guide](#configuring-the-storage-guide)
* `data/`:
  * `dervied/`:
    * Folder for storing temporary generated data files
  * `raw/`:
    * The cleaned induced earthquake data csv files should be put in this folder
* `install-requirements.R`:
  * Utility scripts for installing all the R packages required
* `Makefile`:
  * Used to compile the monthly summary and the MCMC storage guide
* `outputs/figures/`:
  * Target location of plots
* `reports/`:
  * `mcmc-storage-guide/`:
    * Directory for storing latex files associated with building the 1-page MCMC storage guide
  * `monthly-summary/`
    * Directory for storing latex files associated with building the 2-page monthly summary document
* `src/`:
  * `helper-functions/`:
    * Utility R functions used for the storage guide and monthly summary scripts
  * `mcmc-storage-guide/`:
    * Scripts used to perform the simulation study
  * `monthly-summary/`
    * Scripts used to perform the magnitude of completeness estimation, as well as producing the plots

## Requirements

Before attempting to reproduce the monthly summary and the MCMC storage guide, please make sure you have all the following on your system:

* The R programming language
* The `R` packages found in [`install_requirements.R`](install_requirements.R).
  You can either run this script on your system or install them manually.
* The following LaTeX packages: `amsmath`, `amssymb`, `caption`, `cleveref`, `datatool`, `geometry`, `graphicx`, `natbib`, `parskip`, and `subcaption`.
* If you want to compile the [`monthly-summary.tex`](reports/monthly-summary/monthly-summary.tex) or the [`mcmc-storage-guide.tex`](reports/mcmc-storage-guide/mcmc-storage-guide.tex) using the [`Makefile`](Makefile), you will also need the following:
  * GNU Make
  * `latexmk` with PDFLatex support

You will also need to put a earthquake data file in the `data/raw/` directory and configure the `date` and `data_suffix` parameters in
the configuration file, [`config.yaml`](config.yaml), before attempting to reproduce the documents.

## Producing the monthly summary

Various aspects of the $m_c$ estimation, as well as setting which datafile to use, can be configured in [`config.yaml`](config.yaml). See [Configuring the monthly summary](#configuring-the-monthly-summary) for more details.

When producing the monthly summary, both the plots and the parameters and estimates mentioned in text are updated automatically.
However, the qualitative comments made might not be applicable for all instances of the data, so
you can consider changing those in
the [`reports/monthly-summary/monthly-summary.tex`](reports/monthly-summary/monthly-summary.tex) 
after producing the document, and then rerun the makefile command.

**Using the Makefile (recommended)**

Simply navigate to the root of the repository, and run `make monthly_summary`. This will run the relevant R scripts to produce the plots, compile the LaTeX file [`reports/monthly-summary/monthly-summary.tex`](reports/monthly-summary/monthly-summary.tex) and copy the resulting monthly summary PDF file into the project root directory.

**Manually**

* Navigate to the project root directory
* Run [`src/monthly-summary/00_minimum-magnitude-of-completeness.R`](src/monthly-summary/00_minimum-magnitude-of-completeness.R) to create the plots for the first page
* Then run [`src/monthly-summary/01_earthquake-visualisations.R`](src/monthly-summary/00_earthquake-visualisations.R) to create the plots for the second page
* Compile the LaTeX document [`reports/monthly-summary/monthly-summary.tex`](reports/monthly-summary/monthly-summary.tex)

## Producing the Markov chain Monte Carlo storage guide

Various aspects of the simulation study can be configured in [`config.yaml`](config.yaml). See [Configuring the storage guide](#configuring-the-storage-guide) for more details.

**Using the Makefile (recommended)**

Simply navigate to the root of the repository, and run `make mcmc_storage_guide`. This will run the relevant R scripts to produce the plots, compile the LaTeX file [`reports/mcmc-storage-guide/mcmc-storage-guide.tex`](mcmc-storage-guide.tex) and copy the resulting storage guide  PDF file into the project root directory.

**Manually**

* Navigate to the project root directory
* Run [`src/monthly-summary/00_mcmc-storage-guide-simulation-study.R`](src/mcmc-storage-guide/00_mcmc-storage-guide-simulation-study.R) to create the plots for the report
* Then compile the LaTeX document [`reports/monthly-summary/monthly-summary.tex`](reports/monthly-summary/monthly-summary.tex)

## Configuring the monthly summary

All configuration options can be changed in [`config.yaml`](config.yaml).

| configuration parameter             | description                                                                                                                                                                                                                                                      |
|-------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `date` and `data_suffix`            | The data file that will be used for all the analyses and visualisations in the monthly summary is expected to be located in `data/raw/` with a filename `<date><date_suffix>.csv`.                                                                               |
| `log_frequency_smoothing_parameter` | As part of the $m_c$ estimation, we apply a log-transformation to the frequency counts for the magnitudes. To avoid numerical errors when these counts are zero, a constant is added to all the counts before taking logs. This is controlled by this parameter. |
| `magnitude_bin_width`               | This parameter controls the width of the magnitude bins to bin the data when both performing $m_c$ estimation and when visualising the magnitude distribution in a histogram.                                                                                    |
| `min_mc` and `max_mc`               | When estimating $m_c$, we only consider candidate values in the range specified by these two parameters, separated by intervals of size `bin_width`                                                                                                              |
| `explained_variance_min`            | When estimating $m_c$, we pick the smallest candidate $m_c$-value that achieves a goodness of fit above this value (out of 100)                                                                                                                                  |
| `report-summary-data.csv`           | The name of the file to store the temporary numerical summaries and other data that will be used when generating the monthly summary and MCMC storage guide reports.                                                                                             |

## Configuring the storage guide

All configuration options can be changed in [`config.yaml`](config.yaml).

| Configuration parameter    | description                                                                                                                     |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------|
| `num_model_parameters`     | The value for $p$, the dimension of the model parameter vector $\boldsymbol{\theta}$                                            |
| `max_num_samples`          | The largest value for $m$, the number of samples. $m$-values logarthimically spaced between 1 and `max_num_samples` are tested. |
| `num_repeated_simulations` | Number of times to repeat each simulation run in order to build a 95% asymptotic normal confidence interval                     |


