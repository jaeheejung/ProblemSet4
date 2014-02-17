#Problem Set 4
#Jae Hee Jung


library(plyr)
#Load library plyr, which will be useful.

Organize <- function(filename){
#This function takes as input a generic NetLogo file.

namedatetime <- scan(file=filename,what="",skip=1,nlines=2,quiet=TRUE)
#Read in the second and third lines, which contain the name of the original NetLogo file and the date and the time the simulation was run.

namedatetime <-gsub(",","",namedatetime[-4])
#Simplify the vector namedatetime by dropping the fourth element and dropping the unnecessary commas.

namedatetime <-gsub("/","-",namedatetime)
#You cannot have slashes in a folder name, so substitute the slashes with hyphens.

namedatetime <- gsub(":",".",namedatetime)
#You cannot have colons in a folder name, so substitute the colons with periods.

namedatetime <- paste(namedatetime,collapse=" ")
#Collapse the vector namedatetime into a single element.

dir.create(file.path(namedatetime))
#Create the top level directory with the character vector namedatetime as the name of the directory.

sapply(X=c("Globals","Turtles","Plots"),FUN=function(X){dir.create(file.path(namedatetime,X))})
#Create 3 sub-directories named Globals, Turtles, and Plots using the sapply function.

sapply(X=c("PositionPlot","WinnersPlot","PolarizationPlot","IncumbentPercentagePlot"),FUN=function(X){dir.create(file.path(namedatetime,"Plots",X))})
#Create 4 sub-directories under the sub-directory Plots using the sapply function.

globals.names <- scan(file=filename,what="",sep=",",skip=8,nlines=1,quiet=TRUE)
#Read in the 9th row, which consists of characters separated by commas.

globals.values <- scan(file=filename,what=c(0,""),sep=",",skip=9,nlines=1,quiet=TRUE)
#Read in the 10th row consisting of both numbers and characters.

globals.values <- gsub("\\[|\\]","",globals.values)
#Get rid of the square brackets.

globals.values <- strsplit(globals.values," ")
#Split the elements of globals.values, resulting in a list.

names(globals.values) <- globals.names
#Assign the values of globals.names as the names of the elements in globals.values, thereby creating a single combined list.

globals.list <- globals.values
#Rename the list into globals.list for the sake of clarity.

dump("globals.list",file=file.path(namedatetime,"Globals","Globals.R"))
#Dump globals.list as a file named Globals.R into the Globals sub-directory. 

turtles.names <- scan(file=filename,what=c(0,""),sep=",",skip=12,nlines=1,quiet=TRUE)
#Read in the column names of the section TURTLES.

turtles.all <- scan(file=filename,what=c(0,""),sep=",",skip=13,nlines=4786,quiet=TRUE)
#Read in the data under TURTLES. From the Excel file, we can see that the rows continue until the 4799th line.

turtles.all <- matrix(turtles.all,nrow=4786,byrow=TRUE)
#Convert the incredibly long vector into a 4786 by 84 matrix .

turtles.all <- turtles.all[,c(1:38)]
#Drop all columns from the 39th to the 84th since they are empty columns created because of the commas in the original csv file.

turtles.names <- turtles.names[c(1:38)]
#Drop all elements from the 39th to the 84th since they are empty quotation marks created because of the commas in the original csv file.

colnames(turtles.all) <- turtles.names
#Assign turtles.names as the column headings of turtles.all.

districts.index <- which(turtles.all[,"breed"]=="{breed districts}")
#Index the row numbers of all that have "{breed districts}" in the breed column.

districts <- turtles.all[districts.index, ]
#Subset the matrix turtles.all using districts.index.

districts <- districts[,colSums(apply(districts,2,function(X){X==""}))==0]
#Remove columns that are empty (i.e. have only quotation marks).

write.csv(districts,file.path(namedatetime,"Turtles","Districts.csv"))
#Write out Districts.csv to the sub-directory Turtles.

voters.index <- which(turtles.all[,"breed"]=="{breed voters}")
#Index the row numbers of all that have "{breed voters}" in the breed column.

voters <- turtles.all[voters.index, ]
#Subset the matrix turtles.all using voters.index.

voters <- voters[,colSums(apply(voters,2,function(X){X==""}))==0]
#Remove columns that are empty (i.e. have only quotation marks).

write.csv(voters,file.path(namedatetime,"Turtles","Voters.csv"))
#Write out Voters.csv to the sub-directory Turtles.

activists.index <- which(turtles.all[,"breed"]=="{breed activists}")
#Index the row numbers of all that have "{breed activists}" in the breed column.

activists <- turtles.all[activists.index, ]
#Subset the matrix turtles.all using activists.index.

activists <- activists[,colSums(apply(activists,2,function(X){X==""}))==0]
#Remove columns that are empty (i.e. have only quotation marks).

write.csv(activists,file.path(namedatetime,"Turtles","Activists.csv"))
#Write out Activists.csv to the sub-directory Turtles.

parties.index <- which(turtles.all[,"breed"]=="{breed parties}")
#Index the row numbers of all that have "{breed parties}" in the breed column.

parties <- turtles.all[parties.index, ]
#Subset the matrix turtles.all using parties.index.

parties <- parties[,colSums(apply(parties,2,function(X){X==""}))==0]
#Remove columns that are empty (i.e. have only quotation marks).

write.csv(parties,file.path(namedatetime,"Turtles","Parties.csv"))
#Write out Parties.csv to the sub-directory Turtles.

candidates.index <- which(turtles.all[,"breed"]=="{breed cands}")
#Index the row numbers of all that have "{breed cands}" in the breed column.

candidates <- turtles.all[candidates.index, ]
#Subset the matrix turtles.all using candidates.index.

candidates <- candidates[,colSums(apply(candidates,2,function(X){X==""}))==0]
#Remove columns that are empty (i.e. have only quotation marks).

write.csv(candidates,file.path(namedatetime,"Turtles","Candidates.csv"))
#Write out Candidates.csv to the sub-directory Turtles.

}










