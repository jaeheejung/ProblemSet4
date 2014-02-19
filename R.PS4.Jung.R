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

unique.districts <- apply(districts,2,function(X){length(unique(X))})
#Calculate the number of unique values in each column of districts.

unique.index.districts <- which(unique.districts==1)
#Index the columns that have a single unique value (i.e. that have constant values).

districts <- districts[,-unique.index.voters]
#Drop the columns with constant values.

voters.index <- which(turtles.all[,"breed"]=="{breed voters}")
#Index the row numbers of all that have "{breed voters}" in the breed column.

voters <- turtles.all[voters.index, ]
#Subset the matrix turtles.all using voters.index.

voters <- voters[,colSums(apply(voters,2,function(X){X==""}))==0]
#Remove columns that are empty (i.e. have only quotation marks).

unique.voters <- apply(voters,2,function(X){length(unique(X))})
#Calculate the number of unique values in each column of voters.

unique.index.voters <- which(unique.voters==1)
#Index the columns that have a single unique value (i.e. that have constant values).

voters <- voters[,-unique.index.voters]
#Drop the columns with constant values.

activists.index <- which(turtles.all[,"breed"]=="{breed activists}")
#Index the row numbers of all that have "{breed activists}" in the breed column.

activists <- turtles.all[activists.index, ]
#Subset the matrix turtles.all using activists.index.

activists <- activists[,colSums(apply(activists,2,function(X){X==""}))==0]
#Remove columns that are empty (i.e. have only quotation marks).

unique.activists <- apply(activists,2,function(X){length(unique(X))})
#Calculate the number of unique values in each column of activists.

unique.index.activists <- which(unique.activists==1)
#Index the columns that have a single unique value (i.e. that have constant values).

activists <- activists[,-unique.index.activists]
#Drop the columns with constant values.

parties.index <- which(turtles.all[,"breed"]=="{breed parties}")
#Index the row numbers of all that have "{breed parties}" in the breed column.

parties <- turtles.all[parties.index, ]
#Subset the matrix turtles.all using parties.index.

parties <- parties[,colSums(apply(parties,2,function(X){X==""}))==0]
#Remove columns that are empty (i.e. have only quotation marks).

unique.parties <- apply(parties,2,function(X){length(unique(X))})
#Calculate the number of unique values in each column of parties.

unique.index.parties <- which(unique.parties==1)
#Index the columns that have a single unique value (i.e. that have constant values).

parties <- parties[,-unique.index.parties]
#Drop the columns with constant values.

candidates.index <- which(turtles.all[,"breed"]=="{breed cands}")
#Index the row numbers of all that have "{breed cands}" in the breed column.

candidates <- turtles.all[candidates.index, ]
#Subset the matrix turtles.all using candidates.index.

candidates <- candidates[,colSums(apply(candidates,2,function(X){X==""}))==0]
#Remove columns that are empty (i.e. have only quotation marks).

unique.candidates <- apply(candidates,2,function(X){length(unique(X))})
#Calculate the number of unique values in each column of candidates.

unique.index.candidates <- which(unique.candidates==1)
#Index the columns that have a single unique value (i.e. that have constant values).

candidates <- candidates[,-unique.index.candidates]
#Drop the columns with constant values.

write.csv(districts,file.path(namedatetime,"Turtles","Districts.csv"))
#Write out Districts.csv to the sub-directory Turtles.

write.csv(voters,file.path(namedatetime,"Turtles","Voters.csv"))
#Write out Voters.csv to the sub-directory Turtles.

write.csv(activists,file.path(namedatetime,"Turtles","Activists.csv"))
#Write out Activists.csv to the sub-directory Turtles.

write.csv(parties,file.path(namedatetime,"Turtles","Parties.csv"))
#Write out Parties.csv to the sub-directory Turtles.

write.csv(candidates,file.path(namedatetime,"Turtles","Candidates.csv"))
#Write out Candidates.csv to the sub-directory Turtles.

D1.headings <- scan(file=filename,what=c(0,""),sep=",",quiet=TRUE,skip=8544,nlines=1)
#Read the headings (Red, Blue, etc.) of the D1 section of the file.

D1.headings <- D1.headings[c(1,5,9,13,17,21)]
#Remove the meaningless quotation markets, getting a vector of relevant headings.

D1.col.names <- scan(file=filename,what=c(0,""),sep=",",quiet=TRUE,skip=8545,nlines=1)
#Read in the vector of column names.

D1.data <- scan(file=filename,what=c(0,""),sep=",",quiet=TRUE,skip=8546,nlines=169)
#Read the D1 section of the file, which starts at line 8547 and ends at line 8715.

D1.data <- matrix(D1.data,nrow=169,byrow=TRUE)
#Convert D1.data into a 169 by 84 matrix.

colnames(D1.data) <- D1.col.names
#Assign D1.col.names as the column names of D1.data.

D1.data <- D1.data[,c(2,6,10,14,18,22)]
#Leave only y columns, which are the positions of candidates, activists, and voters.

colnames(D1.data) <- D1.headings
#Substitute the column names of D1.data with D1.headings

write.csv(D1.data,file.path(namedatetime,"Plots","PositionPlot","D1.csv"))
#Write out D1.data as a csv file.

#Below, I start repeating the code I wrote to get D1.data to get D2.data and D3.data. Since the process is the same except for the different object names and row numbers, I will not document what I did.
D2.headings <- scan(file=filename,what=c(0,""),sep=",",quiet=TRUE,skip=8728,nlines=1)

D2.headings <- D2.headings[c(1,5,9,13,17,21)]

D2.col.names <- scan(file=filename,what=c(0,""),sep=",",quiet=TRUE,skip=8729,nlines=1)

D2.data <- scan(file=filename,what=c(0,""),sep=",",quiet=TRUE,skip=8730,nlines=169)

D2.data <- matrix(D2.data,nrow=169,byrow=TRUE)

colnames(D2.data) <- D2.col.names

D2.data <- D2.data[,c(2,6,10,14,18,22)]

colnames(D2.data) <- D2.headings

write.csv(D2.data,file.path(namedatetime,"Plots","PositionPlot","D2.csv"))

D3.headings <- scan(file=filename,what=c(0,""),sep=",",quiet=TRUE,skip=8912,nlines=1)

D3.headings <- D3.headings[c(1,5,9,13,17,21)]

D3.col.names <- scan(file=filename,what=c(0,""),sep=",",quiet=TRUE,skip=8913,nlines=1)

D3.data <- scan(file=filename,what=c(0,""),sep=",",quiet=TRUE,skip=8914,nlines=169)

D3.data <- matrix(D3.data,nrow=169,byrow=TRUE)

colnames(D3.data) <- D3.col.names

D3.data <- D3.data[,c(2,6,10,14,18,22)]

colnames(D3.data) <- D3.headings

write.csv(D3.data,file.path(namedatetime,"Plots","PositionPlot","D3.csv"))
#This ends the code for creating D2.csv and D3.csv.

mode(D1.data) <- "numeric"
mode(D2.data) <- "numeric"
mode(D3.data) <- "numeric"
#Change the three data, which are composed of character values, to numeric values in order to calculate the mean.

D1 <- apply(D1.data,2,mean)
D2 <- apply(D2.data,2,mean)
D3 <- apply(D3.data,2,mean)
#Get the average position of each agent in each dimension.

names(D1) <- NULL
names(D2) <- NULL
names(D3) <- NULL
#Make the names of D1, D2, and D3 NULL so that we can directly plot them.

pdf(file=file.path(namedatetime,"Plots","PositionPlot","Positions.pdf"))
#Open a pdf file to put my plot in.

plot(NULL,xlab="",ylab="",xlim=c(1,6),ylim=c(-10,10),axes=FALSE)
#Create a NULL plot with invisible x and y limits.

box(lwd=1)
#Add a contour box.

axis(side=2)
#Add the y axis.

mtext(side=2,"Average Positions",padj=-5)
#Label the y axis.

axis(side=1, at=c(1:6), labels=c("RCand","BCand","RAct","RVote","BVote","BAct"))
#Label the x axis according to agent type.

points(D1,pch="1",col=c("red","blue","red","red","blue","blue"))
#Add average positions in dimension 1 with different colors for Democrats and Republicans.

points(D2,pch="2",col=c("red","blue","red","red","blue","blue"))
#Add average positions in dimension 2 with different colors for Democrats and Republicans.

points(D3,pch="3",col=c("red","blue","red","red","blue","blue"))
#Add average positions in dimension 3 with different colors for Democrats and Republicans.

title("Average positions of candidates, activists,\n and voters along three dimensions")
#Add the main title of the plot.

legend("topleft",legend=c("1: D1","2: D2","3: D3"))
#Add a legend specifying what the 1, 2, 3 points refer to.

dev.off()
#Close the pdf plotting device.

winners.col.names <- scan(filename,what=c(0,""),sep=",",skip=9139,nlines=1,quiet=TRUE)
#Read in the column names of the WINNERS section of the data file.

winners.data <- scan(filename,what=c(0,""),sep=",",skip=9140,nlines=169,quiet=TRUE)
#Read in the WINNERS section of the data file.

winners.data <- matrix(winners.data,nrow=169,byrow=TRUE)
#Tranform the vector into a matrix of data.

colnames(winners.data) <- winners.col.names
#Assign winners.col.names as the column names of winners.data.

winners.data <- winners.data[,1:12]
#Only include the first twelve columns of winners.data, because other columns are empty.

winners.data <- winners.data[,c(1,2,5,6,9,10)]
#Only include the columns signifying the cycle, the party affiliation, and the half dividing line used in the original NetLogo program plot. This drops columns such as "color","pen down?"

colnames(winners.data) <- c("Cycle","Dem","Cycle","Fifty","Cycle","Rep")
#Change the colunm names of winners.data from x's and y's to clearly idenfiable headings.

write.csv(winners.data,file.path(namedatetime,"Plots","WinnersPlot","Winner.csv"))
#Write out Winner.csv.

pdf(file=file.path(namedatetime,"Plots","WinnersPlot","Winner.pdf"))
#Create a pdf file to plot.

plot(NULL,xlab="Cycle",ylab="% of candidates won",xlim=c(0,168),ylim=c(30,60),axes=FALSE)
#Create an empty plot with the axis labels showing, but the axis ranges hidden.

box(lwd=1)
#Add the contour of the plotting box.

axis(side=2)
#Add the y axis.

axis(side=1,at=c(0:168),labels=c(0:168))
#Add the x axis. The labels specify cycles from 0 to 168.

lines(winners.data[,2],lty=1,col="blue")
#The second column of winners.data is data on the percentage of Democratic candidates. Plot a type 1 line in blue.

lines(winners.data[,6],lty=1,col="red")
#The sixth column of winners.data is data on the percentage of Republican candidates. Plot a type 1 line in red.

lines(winners.data[,4],lty=2,col="grey")
#The fourth column of winners.data is data on the percentage that divides in half. For this dataset, the dividing line is 45%. Plot a type 2 line in grey.

title("Percentage of candidates who won in each cycle")
#Add the main title of the plot.

legend("topleft",legend=c("Republicans","Democrats"),lty=c(1,1),col=c("red","blue"))
#Add a legend specifying what the lines mean.

dev.off()
#Turn off the plotting device.

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


###Changes for Jacob's office hour###
1+1