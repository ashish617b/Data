# remove objects and free memory after large object is removed and get report
rm(list = ls())
gc()
#read the data file
diabetic<-read.csv(file="C:/Users/Vineeth Adithya/Desktop/project.csv",stringsAsFactors=FALSE,header=TRUE)
#view the dataset that we have choosen for analysis
View(diabetic)
names(diabetic)
dim(diabetic)
str(diabetic)
attributes(diabetic)
diabetic[1:9,]
head(diabetic)
tail(diabetic)
# Get summary of all the variables in the dataset
summary(diabetic)
#priliminary exploration of the variable Insulin in the diabetes dataset
diabetic[1:10, "Insulin"]
diabetic$Insulin[1:10]
# Inter quartile range of the variable Insulin
quantile(diabetic$Insulin)
quantile(diabetic$Insulin, c(.1, .3, .65))
#variance of the variable Insulin
var(diabetic$Insulin)
#plot a histogram for the distribution of the variable Insulin in the dataset
hist(diabetic$Insulin)
# density graph for the variable Insulin
plot(density(diabetic$Insulin))
# finding the mean value for the variable Insulin
mean_insulin<-mean(diabetic$Insulin)
# finding the median for the variable Insulin
median_insulin<-median(diabetic$Insulin)
#priliminary exploration of the variable Insulin in the diabetes dataset
diabetic[1:10, "Insulin"]
diabetic$Insulin[1:10]
# Inter quartile range of the variable Insulin
quantile(diabetic$Insulin)
quantile(diabetic$Insulin, c(.1, .3, .65))
#variance of the variable Insulin
var(diabetic$Insulin)
#plot a histogram for the distribution of the variable Insulin in the dataset
hist(diabetic$Insulin)
# density graph for the variable Insulin
plot(density(diabetic$Insulin))
# finding the mean value for the variable Insulin
mean_insulin<-mean(diabetic$Insulin)
# finding the median for the variable Insulin
median_insulin<-median(diabetic$Insulin)
#finding outliers
boxplot.stats(diabetic$Insulin)$out
boxplot(diabetic$Insulin)
#priliminary exploration of the variable glucose in the diabetes dataset
diabetic[1:10, "Glucose"]
diabetic$Glucose[1:10]
# Inter quartile range of the variable  Glucose
quantile(diabetic$Glucose)
quantile(diabetic$Glucose, c(.1, .3, .65))
#variance of the variable Glucose
var(diabetic$Glucose)
#plot a histogram for the distribution of the variable Glucose in the dataset
hist(diabetic$Glucose)
# density graph for the variable Glucose
plot(density(diabetic$Glucose))
# finding the mean value for the variable Glucose
mean_glucose<-mean(diabetic$Glucose)
# finding the median for the variable Glucose
median_glucose<-median(diabetic$Glucose)
#finding outliers
boxplot.stats(diabetic$Glucose)$out
boxplot(diabetic$Glucose)
#priliminary exploration of the variable Bloodpressure in the diabetes dataset
diabetic[1:10, "Bloodpressure"]
diabetic$BloodPressure[1:10]
# Inter quartile range of the variable Bloodpressure
quantile(diabetic$BloodPressure)
quantile(diabetic$BloodPressure, c(.1, .3, .65))
#variance of the variable Bloodpressure
var(diabetic$BloodPressure)
#plot a histogram for the distribution of the variable Bloodpressure in the dataset
hist(diabetic$BloodPressure)
# density graph for the variable Bloodpressure
plot(density(diabetic$BloodPressure))
# finding the mean value for the variable Bloodpressure
mean_Bloodpressure<-mean(diabetic$BloodPressure)
# finding the median for the variable Bloodpressure
median_Bloodpressure<-median(diabetic$BloodPressure)
#finding outliers
boxplot.stats(diabetic$BloodPressure)$out
boxplot(diabetic$BloodPressure)
#priliminary exploration of the variable skinthickness in the diabetes dataset
diabetic[1:10, "Skinthickness"]
diabetic$SkinThickness[1:10]
# Inter quartile range of the variable skinthickness
quantile(diabetic$SkinThickness)
quantile(diabetic$SkinThickness, c(.1, .3, .65))
#variance of the variable skinthickness
var(diabetic$SkinThickness)
#plot a histogram for the distribution of the variable skinthickness in the dataset
hist(diabetic$SkinThickness)
# density graph for the variable skinthickness
plot(density(diabetic$SkinThickness))
# finding the mean value for the variable skinthickness
mean_skinthickness<-mean(diabetic$SkinThickness)
# finding the median for the variable skinthickness
median_SKINTHICKNESS<-median(diabetic$SkinThickness)
#finsing outliers
boxplot.stats(diabetic$SkinThickness)$out
boxplot(diabetic$SkinThickness)
#priliminary exploration of the variable BMI in the diabetes dataset
diabetic[1:10, "BMI"]
diabetic$BMI[1:10]
# Inter quartile range of the variable BMI
quantile(diabetic$BMI)
quantile(diabetic$BMI, c(.1, .3, .65))
#variance of the variable BMI
var(diabetic$BMI)
#plot a histogram for the distribution of the variable BMI in the dataset
hist(diabetic$BMI)
# density graph for the variable BMI
plot(density(diabetic$BMI))
# finding the mean value for the variable BMI
mean_BMI<-mean(diabetic$BMI)
# finding the median for the variable BMI
median_BMI<-median(diabetic$BMI)
#finding outliers
boxplot.stats(diabetic$BMI)$out
boxplot(diabetic$BMI)
#priliminary exploration of the variable Diabetespedigreefunction in the diabetes dataset
diabetic[1:10, "Diabetespedigreefunction"]
diabetic$DiabetesPedigreeFunction[1:10]
# Inter quartile range of the variable Diabeticpedigree function
quantile(diabetic$DiabetesPedigreeFunction)
quantile(diabetic$DiabetesPedigreeFunction, c(.1, .3, .65))
#variance of the variable Diabeticpedigree function
var(diabetic$DiabetesPedigreeFunction)
#plot a histogram for the distribution of the variable Diabeticpedigree function in the dataset
hist(diabetic$DiabetesPedigreeFunction)
# density graph for the variable Diabeticpedigree function
plot(density(diabetic$DiabetesPedigreeFunction))
# finding the mean value for the variable Diabeticpedigree function
mean_diabeticpedigreefunction<-mean(diabetic$DiabetesPedigreeFunction)
# finding the median for the variable diabeticpedigreefunction
median_diabetespedigreefunction<-median(diabetic$DiabetesPedigreeFunction)
# FINDING outliers
boxplot.stats(diabetic$DiabetesPedigreeFunction)$out
boxplot(diabetic$DiabetesPedigreeFunction)
#priliminary exploration of the variable age in the diabetes dataset
diabetic[1:10, "Age"]
diabetic$Age[1:10]
# Inter quartile range of the variable Age
quantile(diabetic$Age)
quantile(diabetic$Age, c(.1, .3, .65))
#variance of the variable age
var(diabetic$Age)
#plot a histogram for the distribution of the variable age in the dataset
hist(diabetic$Age)
# density graph for the variable age
plot(density(diabetic$Age))
# finding the mean value for the variable Age
mean_age<-mean(diabetic$Age)
# finding the median for the variable age
median_age<-median(diabetic$Age)
#finding outliers
boxplot.stats(diabetic$Age)$out
boxplot(diabetic$Age)
#priliminary exploration of the variable Pregnancies in the diabetes dataset
diabetic[1:10, "Pregnancies"]
diabetic$Pregnancies[1:10]
# Inter quartile range of the variable Pregnancies
quantile(diabetic$Pregnancies)
quantile(diabetic$Pregnancies, c(.1, .3, .65))
#variance of the variable Pregnancies
var(diabetic$Pregnancies)
#plot a histogram for the distribution of the variable Pregnancies in the dataset
hist(diabetic$Pregnancies)
# density graph for the variable pregnancies
plot(density(diabetic$Pregnancies))
# finding the mean value for the variable Pregnancies
mean_Pregnancies<-mean(diabetic$Pregnancies)
# finding the median for the variable pregnancies
median_Pregnancies<-median(diabetic$Pregnancies)
#findinvg outliers
boxplot.stats(diabetic$Pregnancies)$out
boxplot(diabetic$Pregnancies)

