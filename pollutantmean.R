pollutantmean <- function(directory, pollutant, id = 1:332) {
	
	## Copyright (C) Bob Beilstein, 2015
	#	This program is free software: you can redistribute it and/or modify
	#	it under the terms of the GNU General Public License as published by
	#	the Free Software Foundation, either version 3 of the License, or
	#	(at your option) any later version.
	#	
	#	This program is distributed in the hope that it will be useful,
	#	but WITHOUT ANY WARRANTY; without even the implied warranty of
	#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	#	GNU General Public License for more details.
	#	
	#	You should have received a copy of the GNU General Public License
	#	along with this program.  If not, see <http://www.gnu.org/licenses/>.
	
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
