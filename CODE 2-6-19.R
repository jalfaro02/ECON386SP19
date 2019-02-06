###CODE FROM 2-6-19 LECTURE#####
#########IMPORTING DATA#########

filepath<-"C:/Users/Steve L/Desktop/ECON386SP19/NHIS_RAW.csv" ##saves name of filepath as a string variable
NHIS_0<-read.csv(filepath)  ##uses the read.csv function to import the data from the filepath location

###########################
##some exploratory analysis
###########################

View(NHIS_0)  #view the structure of the dataset in spreadsheet view
##summary(NHIS_0)
##pairs(NHIS_0, col=factor(NHIS_0$SEX))
dim(NHIS_0)  ##returns a vector decribing the dimensions of the dataset (rowsxcols)
plot(NHIS_0$weight, NHIS_0$height)  ##plots height vs. weight
plot(NHIS_0$weight, NHIS_0$height, col=factor(NHIS_0$SEX))  ##colors same plot above by the sex variable
summary(NHIS_0$weight)
summary(NHIS_0$height)
hist(NHIS_0$weight)
hist(NHIS_0$height)
hist(NHIS_0$SLEEP)
summary(NHIS_0$SLEEP)
hist(NHIS_0$educ)
summary(NHIS_0$educ)
hist(NHIS_0$BMI)
summary(NHIS_0$BMI)

#####################
##a bit of cleaning##
#####################
NHIS_1<-NHIS_0  ##creates a new dataset
NHIS_1$HHX<-NULL  ##removes this column variable from the dataset
NHIS_1$FPX<-NULL  ##removes this column variable from the dataset
NHIS_1$FMX<-NULL  ##removes this column variable from the dataset
dim(NHIS_1)  ##verifies that we've lost some dimensions in the column space
dim(NHIS_0)  ##compares to the previous iteration of the raw dataset prior to the cleaning step
pairs(NHIS_1, col=factor(NHIS_0$SEX))  ##pairwise plot of remaining variables is cleaner now

############################
##a quick regression model##
MODEL1<-lm(NHIS_1$height ~ NHIS_1$weight)  ##builds the model
summary(MODEL1)  ##returns the model output summary
############################
