corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations

    ## First, get the number of complete observations in each file

    cobs<-complete(directory)

    ids<-cobs$id[cobs$nobs>threshold]

    retval <- numeric(0)       # Initialize our return value

    for (i in ids) {
        rf=paste(directory,"/",sprintf("%03d.csv",i),sep="")    # Get the file to read
        t<-read.csv(rf)                                         # Read the data
        retval<-c(retval,cor(t$sulfate,t$nitrate,use="complete.obs"))
    }

    retval
}
