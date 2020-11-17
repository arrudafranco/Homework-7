# hw07

## Gustavo Arruda

This repository is part of a University of Chicago course called "Computation for the Social Sciences" taught in the fall of 2020. It contains models for median college student debt data and also data with a subset of variables concerning attitudes towards racist professors from the General Social Survey, both from the US.

 - [scorecard.Rmd](scorecard.Rmd) and [gss_colrac.Rmd](gss_colrac.Rmd) are Markdown files that renders written analysis of the data.
 - [scorecard.md](scorecard.md) contains charts and details about linear models of student debt data in the US.
 - [gss_colrac.md](gss_colrac.md) contains charts and details about classification models of attitutes towards racist professors in the US.
  
Used Libraries:

- To run the code in this repository, the libraries used were:
 - library(tidyverse)
 - library(rcfss)
 - library(leaps)
 - library(caret)
 - library(ggplot2)
 - library(knitr)
 - library(broom)
 - library(randomForest)
 - library(partykit)