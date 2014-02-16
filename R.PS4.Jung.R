#Problem Set 4
#Jae Hee Jung


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
#Split the elements of globals.values, resulting in a list of 84 elements.

names(globals.values) <- globals.names
#Assign the values of globals.names as the names of the elements in globals.values, thereby creating a single combined list.

globals.list <- globals.values
#Rename the list into globals.list for the sake of clarity.

dump("globals.list",file=file.path(namedatetime,"Globals","Globals.R"))
#Dump globals.list as a file named Globals.R into the Globals sub-directory.

}
