# Transportation Alternatives – MultipleRegressionScript
**An R script for scatterplots, correlation statistics, ordinary least squares (OLS) regression and diagnostics, generalized linear model (GLM) testing, and negative binomial regression and diagnostics.**

*This repo is a result of data analysis completed as part of the 2016 Summer of Maps Program. This work was completed in support of a pro bono project with [Transportation Alternatives](https://www.transalt.org/).*

*[Summer of Maps](http://www.summerofmaps.com/) is a fellowship program organized and facilitated by [Azavea](https://www.azavea.com/). Azavea is a B Corporation that specializes in civic-minded GIS software development and spatial analysis.*
*Summer of Maps offers stipends to student spatial analysts to perform data analysis and visualization for non-profit organizations. Every year we match up non-profit organizations that have spatial analysis needs with talented students to implement projects over a three-month period during the summer.*

### Purpose of the Work
[Transportation Alternatives](https://www.transalt.org/) expressed interest in exploring statistical relationships between traffic crashes and poverty in New York City. To perform this analysis, Parker – the Summer of Maps Fellow for this project – developed a script using the [R statistical programming language](https://www.r-project.org/).

The script automates a handleful of useful processes for examining statistical correlation and regression, including:

- Loading data from a CSV
- Removing erroneous results using [dplyr](https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html)
- Generating scatterplots of each variable’s relationship with crash density
- Generating correlation and significance values
- Creating a correlation matrix of all variables
- Performing ordinary least squares (OLS) regression
- Assessing the OLS model with a variety of diagnostics
- Performing stepwise and best subsets regression
- Assessing the distribution of crash data against assumed distributions of different generalized linear models (GLMs)
- Performing negative binomial regression
- Generating diagnostics for negative binomial regression

### Intended Use for this Work
This script is intended for use by folks looking for a simple and lightweight method for testing their data on OLS and negative binomial regression models. The data used for this particular analysis is included in Regression_Data.csv.

The code for this script is open and free to be forked, modified, or repurposed.

### Acknowledgements
[Parker Ziegler](http://parkerziegler.com/) was the analyst for this project. Special to Daniel McGlone, Senior GIS Analyst at Azavea, for his mentorship on this project, to Esther Needham, Data Analytics Project Manager at Azavea, for her guidance with R, and to Jeremy Heffner, Senior Data Scientist at Azavea, for his statistical wisdom.

Data for this project comes from Transportation Alternatives, the U.S. Census Bureau's American Community Survey, and NYC Open Data.
