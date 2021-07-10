# This is *Kera* and *Soohee*'s group project.  
 
## | Purpose of the repo |  
We will read in and summarize the [*bike sharing*](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset) data set and then try to predict the number of users using predictive models.  

The total number of riders is of interest because it can give a feel for the total number of bikes that need to be available at certain times, and not just cater to the number of registered riders or the number of casual riders. Different types of riders use the bikes on different days. For instance, the number of casual riders tends to increase on holidays, while the registered rider numbers are decreased. In this same example though, there are still more registered riders than casual. Knowing the overall trend helps to ensure that the bikeshare will have enough bikes to support all the users.
......

## | R packages required |  
library(tidyverse)  
library(caret)  
library(corrplot)  

## | Links to the generated analysis |  
The analysis for [Monday is available here](https://github.com/sjung7nc/group13-project2/blob/main/Monday.md).  
The analysis for [Tuesday is available here](https://github.com/sjung7nc/group13-project2/blob/main/Tuesday.md).  
The analysis for [Wednesday is available here](https://github.com/sjung7nc/group13-project2/blob/main/Wednesday.md).  
The analysis for [Thursday is available here](https://github.com/sjung7nc/group13-project2/blob/main/Thursday.md).  
The analysis for [Friday is available here](https://github.com/sjung7nc/group13-project2/blob/main/Friday.md).  
The analysis for [Saturday is available here](https://github.com/sjung7nc/group13-project2/blob/main/Saturday.md).  
The analysis for [Sunday is available here](https://github.com/sjung7nc/group13-project2/blob/main/Sunday.md).  

## | Code used to automate the process |  

In the below code that was used to automate the reports, the 'weekdays' vector was created in the main analysis file and holds the different day names.
```{r}
# Create filenames  
output_file <- paste0(weekdays,".md")  

# Create a list for each day with just the day parameter  
params = lapply(weekdays, FUN=function(x){list(Day=x)})  

# Out into a data frame  
reports <- tibble(output_file,params)  

# Need to use x[[1]] to get at elements since tibble doesn't simplify  
apply(reports, MARGIN =1,  
      FUN = function(x){  
        rmarkdown::render(input="Bike Share Project.Rmd", output_file=x[[1]],  
                          params=x[[2]])  
      })
```
