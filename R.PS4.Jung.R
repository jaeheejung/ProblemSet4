#Problem Set 4
#Jae Hee Jung


Organize <- function(filename){
#This function takes as input a generic NetLogo file.

namedatetime <- scan(file=filename,what="",skip=1,nlines=2)
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
}


