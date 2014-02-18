#Problem Set 4
#Jae Hee Jung


library(plyr)
#Load the plyr package, just in case.

#######NetLogo Exercise#######
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


#######JMR Ch.4 Problem 3#######
sink("SquareCube")
#Create a program/script named "SquareCube" in the working directory.

cat("number square cube")
#These are the headings of the table.

for(i in 1:n){
		squared <- i^2
		cubed <- i^3
		cat(format(i,width=8),
		format(squared,width=10),
		format(cubed,width=10),
		"\n",sep="")
}
#The for loop creates the table of numbers, squares, and cubes.

sink()
#The sink function terminates and saves the program/script "SquareCube", which can be read later on using the source function.


#######JMR Ch.4 Problem 4#######
mult.table <- matrix(0,9,9)
#Create a temporary 9 by 9 matrix filled with 0s.

for(i in 1:9){
	mult.table[i,] <- i*(1:9)
}
#For loop mult.table by row.

print(mult.table)
#Check the output.


#######JMR Ch.7 Problem 3#######
pop <- data.frame(m=rnorm(100,160,20),f=rnorm(100,160,20))
#This is code from the JMR book question.

next.gen <- function(pop){
	pop$m <- sample(pop$m)
	pop$m <- apply(pop,1,mean)
	pop$f <- pop$m
	return(pop)
}
#This is code from the JMR book question.

library(lattice)
#Load the lattice package.

height.data <- data.frame(NULL)
#Make an empty data.frame to be filled through for loop.

for(i in 1:9){
	one.gen.height <- data.frame(next.gen(pop)[,1],rep(i,100))
	height.data <- rbind(height.data,one.gen.height)
}
#Fill the data.frame height.data by for looping the male heights of each generation simulated.

colnames(height.data) <- c("height","generation")
#Assign proper column names to the data.

histogram(~height|generation,data=height.data,main="Distribution of male height by generation",xlab="ht")
#Create a panel of 9 plots as in the book.


#######JMR Ch.7 Problem 4#######
treeg <- read.csv("/Users/jaeheejung/Desktop/Spring 2014/Applied Statistical Programming/inst/resources/data/treegrowth.csv")
#Read in the data.

library(lattice)
#Load the lattice package.

#What follows below is the code in the JMR book used to draw the graph. I utilize this code to make the same plot using the lattice package. I mark below where this replicated code ends.
trees <- list()
n <- 0
current.ID <- treeg$tree.ID[1]
current.age <- treeg$age[1]
current.dbh <- treeg$dbh.in[1]
current.height <- treeg$height.ft[1]
for(i in 2:dim(treeg)[1]){
	if(treeg$tree.ID[i]==current.ID){
		current.age <- c(treeg$age[i],current.age)
		current.dbh <- c(treeg$dbh.in[i],current.dbh)
		current.height <- c(treeg$height.ft[i],current.height)
	}else{
		n <- n+1
		trees[[n]] <- list(tree.ID=current.ID,forest=treeg$forest[i-1],habitat=treeg$habitat[i-1],age=current.age,dbh.in=current.dbh,height.ft=current.height)
		current.ID <- treeg$tree.ID[i]
		current.age <- treeg$age[i]
		current.dbh <- treeg$dbh.in[i]
		current.height <- treeg$height.ft[i]
	}
}
n <- n+1
trees[[n]] <- list(tree.ID=current.ID,forest=treeg$forest[i],habitat=treeg$habitat[i],age=current.age,dbh.in=current.dbh,height.ft=current.height)

my.max <- function(x,i)max(x[[i]])
max.age <- max(sapply(trees,my.max,"age"))
max.height <- max(sapply(trees,my.max,"height.ft"))
#This is the end of the code from the JMR book.

xyplot(height.ft~age,data=treeg,xlab="age (years)",ylab="height (feet)",xlim=c(0,max.age),ylim=c(0,max.height),type="l")
#Draw the plot using the lattice function xyplot.