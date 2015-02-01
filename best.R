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
#  Return best hospital in a specified state for one of the three specified conditions
#
# Author: rjb
###############################################################################

best<-function(state,outcome) {
    if (is.null(state)) stop("invalid state")               # getHospitalData allows NULL, we don't
    source("getHospitalData.R",local=TRUE)                  # source data fetcher as local function
    hd<-tryCatch(getHospitalData(outcome,state),error=function(e) e)
   if (inherits(hd,"error")) stop(conditionMessage(hd))    # If getHospitalData signals an error, pass it along
   hd[1,2]
}    

