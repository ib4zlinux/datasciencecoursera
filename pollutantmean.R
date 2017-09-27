pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length l indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calcluate the 
        ## meanl either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: do not round the result!
        
        y_array <- id
        i <- 1
        nums <- 0
        
        while (i <= length(y_array)) {
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
                
                if (pollutant == 'nitrate') {
                        nums <- c(nums, as.numeric(as.character(data$nitrate)))
                }
                else {nums <- c(nums, as.numeric(as.character(data$sulfate)))}
                i <- i + 1
        } ## end while loop
        
        mean(nums, na.rm = TRUE)

}