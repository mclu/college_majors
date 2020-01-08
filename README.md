# Introductory Analyses of College Majors Dataset

## Overview
This is a short data analysis for application of the Center for Academic Innovation's Data Science Fellowship.  

## Navigation


## Data
The data can be downloaded from FiveThirtyEight College Majors git [repo](https://github.com/fivethirtyeight/data/tree/master/college-majors).

## Installation
To knit the `results.Rmd` file, the following packages should be installed in the IDE.

```r
install.packages(tidyverse)
install.packages(data.table)
install.packages(shiny)
install.packages(knitr)
install.packages(DT)
```
To run the file locally, you can install the shiny package in R and use the `runGitHub()` function.
```r
shiny::runGithub("college_majors", "mclu", subdir = " ") 
# or runUrl("")
```
Or you can clone the git repository, then use `runApp()`.
```r
# First clone the repository with git. If you have cloned it into
# ~/shiny_example, first go to that directory, then use runApp().
setwd("~/shiny_example")
runApp()
```

