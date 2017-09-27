corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length l indicating
	## the location of the CSV files

	## "threshold' is a numeric vector of length l indicating the 
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and suflate; the default is 0

	## Return a numeric vector of correlations
	## NOTE: do not reound the result!

        ## y_array <- length(list.files(directory)) ## how many files in directory?
        files <- list.files(directory) ## build an array of the file names
        y_array <- length(files)
        results_table <- data.frame()
        my_cor = numeric()
        
        ## read all files, return a table of how many complete cses in each file.
        complete_cases <- complete("specdata")
        
        ## read those files that meet the "treshold" test. For each file, compute the correlation of 
        ## nitrate and sulfate and store in a table called my_cor
        j <- 1
        setwd(directory)
        while (j <= y_array) {
                if (complete_cases$nobs[j] >= threshold) {
                        results_table <- read.csv(files[j])
                        my_cor = c(my_cor, cor(results_table$sulfate, results_table$nitrate, use = 'pairwise.complete.obs')) }
                j <- j + 1
              }

        return(my_cor)
        
}