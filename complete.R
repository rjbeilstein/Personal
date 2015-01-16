complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    rf=paste(directory,"/",sprintf("%03d.csv",id),sep="")    # Get the list of files to read

    ##  Start out empty

    ids<-NULL
    obs<-NULL

    ## Read 'em and weep

    for (i in 1:length(rf)) {
        p<-read.csv(rf[i])
        ids<-c(ids,id[i])
        obs<-c(obs,sum(!(is.na(p$sulfate) | is.na(p$nitrate))))
    }
    data.frame(id=ids,nobs=obs)
}
