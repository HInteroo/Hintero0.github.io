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

