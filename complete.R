complete <- function(directory, id = 1:332) {
        ## 'diretory' is a character vector or length i indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID and 'nobs' is the
        ## number of complete cases
        
        y_array <- id  ## load an array of file ids
        y_array
        i <- 1         ## setup an index
        results_table <- data.frame()
        
        while (i <= length(y_array)) { ## build a file name and read it in
                id_current <- id[i]
                if (id_current <= 9) {
                        id_char <- paste("00", toString(id[i]), sep = "")
                }
                else if (id_current <= 99) {
                        id_char <- paste("0", toString(id[i]), sep = "")
                }
                else if (id_current <= 332) {
                        id_char <- toString(id[i])
                }
                
                file_name <- paste(directory, "/", id_char, ".csv", sep = "")
                file_name
                data <- read.csv(file_name)
                
                x <- 0 ## setup counter of valid samples
                j <- 1
                
                while (j <= nrow(data)) {
                        j
                        if (!is.na(data$sulfate[j])) {
                                x <- x + 1}
                        j <- j + 1
                }
                ## add the file id and count to results_table
                results <- data.frame(id=id_current, nobs=x)
                results_table <- rbind(results_table, results)
                
                
                i <- i+1 ## move to next file in directory
        }
        colnames(results_table) <- c("id", "nobs")
        results_table ## Print the results table.
}

