#get unique teams
days <- unique(day)
#create filenames
output_file <- paste0(days, ".md")
#create a list for each team with just the team name parameter
#params = lapply(days, FUN = function(x){list( = x)})