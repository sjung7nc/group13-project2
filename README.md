# This is *Kera* and *Soohee*'s group project.  
 
## Purpose of the repo  
We will read in and summarize the [*bike sharing*](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset) data set and then try to predict the number of users using predictive models.  
......

## R packages required  
library(tidyverse)
library(caret)
library(gbm)

## Links to the generated analysis  
The analysis for [Monday is available here](MondayAnalysis.md).
The analysis for [Tuesday is available here](MondayAnalysis.md).
The analysis for [Wednesday is available here](MondayAnalysis.md).
The analysis for [Thursday is available here](MondayAnalysis.md).
The analysis for [Friday is available here](MondayAnalysis.md).
The analysis for [Saturday is available here](MondayAnalysis.md).
The analysis for [Sunday is available here](MondayAnalysis.md).

## Code used to automate the process  
### Create filenames  
output_file <- paste0(weekdays,".md")  

### Create a list for each day with just the day parameter  
params = lapply(weekdays, FUN=function(x){list(Day=x)})  

### Out into a data frame  
reports <- tibble(output_file,params)  

### Need to use x[[1]] to get at elements since tibble doesn't simplify  
apply(reports, MARGIN =1,  
      FUN = function(x){  
        rmarkdown::render(input="Bike Share Project.Rmd", output_file=x[[1]],  
                          params=x[[2]])  
      })
