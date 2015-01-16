pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)

    rf=paste(directory,"/",sprintf("%03d.csv",id),sep="")    # Get the list of files to read

    ## Now loop through all of the files and accumulate the values
    ## read.csv will fail if we've specified a nonexistent file

    vals<-NULL

    for (i in 1:length(rf)) {
        dat<-read.csv(rf[i])     # get the data frame
        p <- if(pollutant == "sulfate") {
            dat$sulfate
        }
        else if (pollutant == "nitrate") {
            dat$nitrate
        } else {
            stop('Pollutant must be either "nitrate" or "sulfate"')
        }

        vals<-c(vals,p[!is.na(p)])

    }
    mean(vals)
}
