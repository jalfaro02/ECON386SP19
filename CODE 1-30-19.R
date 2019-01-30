##CODE FROM 1/30/19

##comments look like this  

##installs the swirl package - only needs to be done once
install.packages("swirl") 

##activates/points R to the swirl library of commands = nees to be done every time you start a new session if you want to use this package
library("swirl")  

##installs a particular swirl course - only needs to be done once for the particular course (in this case, R Programming E)
install_course_github("swirldev", "R_Programming_E")  

##ALTERNATIVELY - you can use the following command to install a course (in this case, Exploratory Data Analysis):
swirl::install_course("R_Programming_E")

swirl()  ##runs the program "swirl" and you and the AI take it from there