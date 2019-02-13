#####CODE FROM 2-13-19 CLASS ON CLEANING DATA###
################################################

##FIRST WE MUST IMPORT THE DATA###
NHIS<-read.csv('C:/Users/Steve L/Desktop/ECON386SP19/NHIS_RAW.csv')

##CONDUCT SOME EXPLORATORY ANALYSIS##
View(NHIS)  ##View data in spreadsheet format
head(NHIS)  ##display first 6 rows of data
tail(NHIS)  ##display last 6 rows of data
summary(NHIS)  ##generates summary statistics for all variables
plot(NHIS$weight, NHIS$height, col=factor(NHIS$SEX))  ##plot of weight vs height colored by the sex variable
hist(NHIS$SLEEP)  ##generates a histogram for the SLEEP variable
summary(NHIS$SLEEP) ##summarizes sleep variable
hist(NHIS$educ)  ##generates histogram of education variable
summary(NHIS$educ)  ##generates summary statistic for the education variable
hist(NHIS$BMI)  ##generates histogram for the BMI variable
summary(NHIS$BMI)  ##summary statistics for BMI variable

##Here we entertain two methodologies for cleaning data: 
##1) deletion; vs. 2) deletetion only when necessary.
##The latter method results in less lost information from
##trimming outlier data.

########################
###METHOD 1: Deletion###
########################

##First Step##
NHIS_1<-subset(NHIS, SLEEP<=24) ##deletes observations with invalid sleep response
dim(NHIS_1)  ##dimensions of the data after 1st step
(dim(NHIS)[1]-dim(NHIS_1)[1])/dim(NHIS)[1]  ##computes fraction of data lost in first step of cleaning

##Second Step##
NHIS_2<-subset(NHIS_1, height<90)
dim(NHIS_2)  ##dimensions of the data after 2nd step

##Third Step##
NHIS_3<-subset(NHIS_2, weight<450)
dim(NHIS_3)  ##dimensions of the data after 3rd step

##Fourth Step##
NHIS_4<-subset(NHIS_3, BMI<60)
dim(NHIS_4)  ##dimensions of the data after 4th step

##Fifth Step##
NHIS_5<-subset(NHIS_4, educ<35)
dim(NHIS_5)  ##dimensions of the data after 5th step

##Alternatively, we could do it all in one step:
NHIS_A<-subset(NHIS, SLEEP<=24 & BMI<60 & educ<35 & height<90 & weight<450)
dim(NHIS_A)
(dim(NHIS)[1]-dim(NHIS_A)[1])/dim(NHIS)[1]  ##percentage of total observations lost from trimming outliers

##Recall that we discussed how to delete columns of data as well:
NHIS_A$HHX<-NULL  ##removes this column variable
NHIS_A$FPX<-NULL  ##removes this column variable
NHIS_A$FMX<-NULL  ##removes this column variable
dim(NHIS_A)

#######################################
###METHOD 2: Deletion when necessary###
#######################################
NHIS_B<-NHIS  ##create new dataset from starting point
NHIS_B$SLEEP[NHIS_B$SLEEP>24]<-NA ##assigns NA values for invalid responses
mean(NHIS_B$SLEEP)  ##R can't handle the computation because of the NA
mean(NHIS_B$SLEEP, na.rm=TRUE) ##tells R to ignore the NA 

##assigns NA to the invalid/outlier responses
NHIS_B$height[NHIS_B$height>90]<-NA 
NHIS_B$weight[NHIS_B$weight>450]<-NA
NHIS_B$BMI[NHIS_B$BMI>60]<-NA
NHIS_B$educ[NHIS_B$educ>35]<-NA
dim(NHIS_B)  ##no data lost - same as dim(NHIS)

MODEL1<-lm(NHIS_B$height~NHIS_B$weight)
summary(MODEL1)