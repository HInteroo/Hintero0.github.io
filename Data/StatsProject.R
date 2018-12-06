#First, trying to load all-ages.csv to RStudio
#Step 1:

AllAges <-read.csv("/Users/admin/Desktop/Data/all-ages.csv")
library(e1071) 
#-------------------First numerical data columns--------------------------#
## Mean and standard Deviation for AllAges$Total
#Step 2: Using AllAges$Total to find the mean and standard
#Deviation for the column total
meanTotal<-mean(AllAges$Total)
meanTotal
SdTotal <-sd(AllAges$Total)
SdTotal
skewness(AllAges$Total)
kurtosis(AllAges$Total)

#First Graph Boxplot:
boxplot(AllAges$Total, horizontal = TRUE, xlab = "Number of Students", 
        main = "Total Amount of Students in Majors")
boxplot.stats(AllAges$Total)$out

#Second Graph Histogram:
hist(AllAges$Total,breaks = 150,xlab = "Number of Students",main = "Total Amount of Students in Majors")

#-------------------Second numerical data column--------------------------#
## Mean and standard Deviation for AllAges$Employedl
#Step 3: Using AllAges$Employed to find the mean and standard
#Deviation for the column Employed
meanEmployed<-mean(AllAges$Employed)
SdEmployed <-sd(AllAges$Employed)
meanEmployed
SdEmployed
skewness(AllAges$Employed)
kurtosis(AllAges$Employed)

#First Graph Boxplot:
boxplot(AllAges$Employed, horizontal = TRUE, xlab = "Number of Employed", 
        main = "Total Amount of Employed Students per Majors")
boxplot.stats(AllAges$Employed)$out

#Second Graph Histogram:
hist(AllAges$Employed,breaks = 150,xlab = "Number of Employed Students",main = "Total Amount of Employed Students in Majors")

#-------------------Third numerical data column--------------------------#
## Mean and standard Deviation for AllAges$Unemployment_rate
#Step 3: Using AllAges$Unemployment_rate to find the mean and standard
#Deviation for the column Unemployment_rate
MeanUnemploymentR<-mean(AllAges$Unemployment_rate)
SdUnemploymentR <-sd(AllAges$Unemployment_rate)
MeanUnemploymentR
SdUnemploymentR
skewness(AllAges$Unemployment_rate)
kurtosis(AllAges$Unemployment_rate)

#First Graph Boxplot:
boxplot(AllAges$Unemployment_rate, horizontal = TRUE, xlab = "Percentage of Unemployment rate", 
        main = "Unemployment Rate per Majors")
boxplot.stats(AllAges$Unemployment_rate)$out

#Second Graph Histogram:
hist(AllAges$Unemployment_rate,breaks = 50,xlab = "Percentage of Unemployment rate", 
     main = "Unemployment Rate per Majors")

#PROJECT MILESTONE 5:
install.packages("qcc") #To use pareto.chart()
library(qcc)
pareto.chart(table(AllAges[3]),main = "Pareto Char for Major Categories")
#______________________________________________________________-
#Thursday 1 November: Compute the confidence intervals for the means of the three numerical data columns
#Confidence Interval for the "Total":
mean <- 230256.6
StandardDeviation <- 422068.5
n <- length(AllAges$Total)
n
qnorm(0.975)
Error <- qnorm(0.975)*StandardDeviation/sqrt(n)
left <- mean-Error
right <- mean+Error

#Confidence Interval for the "Employed":
meanEmployed<-mean(AllAges$Employed)
SdEmployed <-sd(AllAges$Employed)
n <- length(AllAges$Employed)
Error <- qnorm(0.975)*SdEmployed/sqrt(n)
left <- meanEmployed-Error
right <- meanEmployed+Error
left
right
#Confidence Interval for the "Unemployement_rate":
MeanUnemploymentR<-mean(AllAges$Unemployment_rate)
SdUnemploymentR <-sd(AllAges$Unemployment_rate)
n <- length(AllAges$Unemployment_rate)
Error <- qnorm(0.975)*SdUnemploymentR/sqrt(n)
left <- MeanUnemploymentR-Error
right <- MeanUnemploymentR+Error
right
left
#--------------------------------------------------------#
#Thursday 15 29 November: Decide on and test 3 hypotheses about your data. 
#Hypothesis test, true population average is different from 300,000.0
#True population average of students in each Major is 300,000.0
#Is there any evidence that the average total of students
#is 300,000.0? Has it changed?
#H0: No change. 300,000.0 is true population
#P-value is low, reject the null hypothesis. 1-5% stong evidence that the alternative is true:
#The average is different from 300,000.0 students in major.
#Not only is it different, but had decreased.
head(AllAges)
TotalPerMajors <- xtabs(Total ~ Major_category, data = AllAges)
t.test(TotalPerMajors,mu = 300000.0)
#-----------------------------------------------
UnemploymentR <- AllAges$Unemployment_rate
UnemploymentR
class(Unemployment_rate)
t.test(UnemploymentR,mu = 0.055)
#-----------------------------------------------
#H0: mean of Employed students = mean of Unemployed Students of each majors
#Two-sided test
#Assume non-equal variances
attach(AllAges)
boxplot(Employed,Unemployed)
t.test(Employed,Unemployed, mu = 0 , alt ="two.sided")
class(AllAges$Total)
