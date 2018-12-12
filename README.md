# Scenarios

This repository contains IPCC scenario data from the AR5 and SR15 and a set of R scripts to process and plot data.
The data include Model Intercomparison Projects such as AMPERE, LIMITS, RoSE, SSP.

## Getting started
The file main.R provides a template to analyse scenario data.
First one must define a few user options in the file scripts/user_section.R.
These include options to process the scenario databases, definitions of file paths and the definition of the function to select a subset of variables from the scenario databases. 
Second the scenario databases can be processed to generate a new dataset containing all requested variables or one can load a file containing a dataset generated previously.
Third statistics can be computed and plots created by using the available functions (see functions/functions_computePathwayStats.R and functions/functions_plot.R).

## Computing statistics
The following functions are available:

* compute_stats_tempcat: compute quantiles by temperature ceiling categories
* compute_stats_tempcat_reg: compute quantiles by temperature ceiling categories and regions
* compute_stats_allcat: compute quantiles by policy timing and technology availability categories
* compute_cumulate: compute cumulative values of a variable over a specified period 
* compute_cumulate_allcat: compute cumulative values of a variable over a specified period, split by policy timing and technology availability categories
* compute_maxDecadalRate: compute the maximum decadal deployment rate of a variable over a specified period
* compute_maxDeployRate: compute the maximum deployment rate of a variable over a specified period
* compute_avgDeployRate: compute the average deployment rate of a variable over a specified period
* compute_avgDeployRate2030250_relative: compute the average deployment rate of a variable between 2030 and 2050 relative to

See file functions/functions_computePathwayStats.R for details.

## Plotting graphs
The following functions are available:

* plot_ribbons_tempcat: generate funnel plot by temperature categories
* plot_ribbons_allcat_grid: generate funnel grid plot by temperature categories, policy timing and technology availability categories
* plot_ribbons_allcat_byTemp: generate funnel grid plot by temperature categories, policy timing and technology availability categories
* plot_ribbons_allcat_2C: generate funnel plot by policy timing and technology availability categories. Non 2C scenarios must be filtered out first.
* plot_cumulative_boxplots: generate boxplots of cumulative values by temperature categories
* plot_cumulative_boxplots_allcat: generate boxplots of cumulative values by policy timing and technology availability categories
* plot_deployment_boxplots: generate boxplots of deployment rates by temperature categories
* plot_deployment_boxplots_allcat: generate boxplots of deployment rates by policy timing and technology availability categories
* plot_avgDeployRate20302050_boxplots: generate boxplots of deployment rates by temperature categories
* plot_avgDeployRate20302050_boxplots_allcat: generate boxplots of deployment rates by policy timing and technology availability categories

See file functions/functions_plot.R for details.


