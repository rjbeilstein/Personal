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
#  R Programming Assignment 3.  Utility function to return subset of hospital data
#  by condition and optionally by state
#  Result is a 3-element data frame containing hospital, state and selected condition (as numeric)
#  with NA elements elided, sorted into ascending 
# 
# Author: rjb
###############################################################################

getHospitalData<-function(condition,state=NULL) {           # decode condition to column
    if (condition == "heart attack") ccol=11
    else if (condition == "heart failure") ccol=17
    else if (condition == "pneumonia") ccol=23
    else stop("invalid outcome")
    
    HospitalData=read.csv("outcome-of-care-measures.csv",header=TRUE,colClasses="character")   # get raw data
    
    if (!is.null(state)) HospitalData=HospitalData[HospitalData$State==state,]
    if (0==nrow(HospitalData)) stop("invalid state")
    rv<-data.frame(HospitalData[,c(1,2,7)],outcome=suppressWarnings(as.numeric(HospitalData[,ccol])))
    rv[order(rv$outcome,rv$Hospital.Name,na.last=NA),]   # Sort by outcome (mortality), Hospital Name, return it
}

