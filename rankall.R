#  Copyright (C) Robert J. Beilstein, 2015
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# 
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate
#
# Author: rjb
###############################################################################

rankall <- function(outcome, num = "best") {
    source("getHospitalData.R",local=TRUE)                            # get data function
    hd<-tryCatch(getHospitalData(outcome),error=function(e) e)  # get the data, trap errors
    if (inherits(hd,"error")) stop(conditionMessage(hd))    # If getHospitalData signals an error, pass it along
    
    rslt=data.frame()
    
    states=sort(unique(hd$State))        # get list of states
    for (i in states) {
        hstate<-hd[hd$State==i,]
        if (num=="best") rnum=1                                 # "best" is first row
        else if (num=="worst") rnum=nrow(hstate)                    # worst is last row
        else {
            rnum=tryCatch(as.numeric(num),warning=function(e) e)    # decode number
            if (inherits(rnum,"warning")) stop("invalid number")    # complain if number is wrong
        }    
        if (rnum < 1 || rnum > nrow(hstate)) { rslt<-rbind(rslt,data.frame(hospital=NA,state=i)) }
        else rslt<-rbind(rslt,data.frame(hospital=hstate$Hospital.Name[rnum],state=i))
    }
    rslt
}
