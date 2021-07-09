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