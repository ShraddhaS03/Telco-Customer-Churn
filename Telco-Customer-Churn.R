#Telco Customer Churn
#install.packages("Hotelling")
library(tidyverse)
library(MASS)
library(DMwR)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)
library(lattice)
library(sm)
library(Hmisc)
library(asbio)
library(MVA)
library(Hotelling)

#Reading the dataset
telco_churn <- read.csv("C:\\Users\\sshah\\Desktop\\MVA\\project\\Telco-Customer-Churn.csv")
class(telco_churn)

# Showing the structure of the data frame.
str(telco_churn)
#size of dataset - output is first # of rows aka points, then columns aka variables
dim(telco_churn)
#list variables in dataset
names(telco_churn)
#print first 10 rows 
head(telco_churn, n=10)
#summary of data
summary(telco_churn)

#Data Cleaning
#Converting SeniorCitizen variable into a factor variable
telco_churn$SeniorCitizen <- as.factor(ifelse(telco_churn$SeniorCitizen==0,'Yes','No'))
head(telco_churn, n=10)

#Converting tenure values into ranges of 12 months

telco_churn <- mutate(telco_churn,tenure_range = tenure)
telco_churn_tenure <- cut(telco_churn$tenure_range,6,labels = c('0-1 Years','1-2 Years','2-3 Years','4-5 Years','5-6 Years','6-7 Years'))
head(telco_churn_tenure)

#Replacing 'No Internet Service' value in Streaming Movies, Online Security, Device Prtection,
#Tech Support and Streaming TV with No'
telco_churn$StreamingTV[telco_churn$StreamingTV=='No internet service'] <- 'No'
telco_churn$StreamingMovies[telco_churn$StreamingMovies=='No internet service'] <- 'No'
telco_churn$OnlineSecurity[telco_churn$OnlineSecurity=='No internet service'] <- 'No'
telco_churn$OnlineBackup[telco_churn$OnlineBackup=='No internet service'] <- 'No'
telco_churn$DeviceProtection[telco_churn$DeviceProtection=='No internet service'] <- 'No'
telco_churn$TechSupport[telco_churn$TechSupport=='No internet service'] <- 'No'

#Deleting the unused levels from the factor variables
telco_churn$StreamingMovies <- factor(telco_churn$StreamingMovies)
telco_churn$StreamingTV <- factor(telco_churn$StreamingTV)
telco_churn$OnlineSecurity <- factor(telco_churn$OnlineSecurity)
telco_churn$OnlineBackup <- factor(telco_churn$OnlineBackup)
telco_churn$DeviceProtection <- factor(telco_churn$DeviceProtection)
telco_churn$TechSupport <- factor(telco_churn$TechSupport)

#Calculating the number of null values in each of the columns
nullvalues <- colSums(is.na(telco_churn))
nullvalues <- (nullvalues/nrow(telco_churn))*100
nullvalues

#Removing the rows containing null values as there are just 11 rows out of 7043 rows
#in total which is 0.15% and hence we can afford dropping those
telco_churn <- telco_churn[complete.cases(telco_churn), ]

#Exploratory Data Analysis

#Checking for distributions in numerical columns
#The qqplotd show a few extreme outliers which break the assumption of 95% confidence
#normal distribution
par(mfrow = c(1,2))
hist(telco_churn$TotalCharges,xlab='',main = 'Histogram of TotalCharges',freq = FALSE)
lines(density(telco_churn$TotalCharges,na.rm = T))
rug(jitter(telco_churn$TotalCharges))
qqPlot(telco_churn$TotalCharges,main='Normal QQ plot of TotalCharges')
par(mfrow=c(1,1))

ggplot(telco_churn, aes(x = Churn))+geom_histogram(stat = "count", fill = c("sky blue", "orange"))

telco_churn %>% filter(telco_churn$Churn == "Yes") %>%  ggplot( aes(x=  tenure))+geom_bar(fill = "orange" )

par(mfrow = c(1,2))
hist(telco_churn$MonthlyCharges,xlab='',main = 'Histogram of MonthlyCharges',freq = FALSE)
lines(density(telco_churn$MonthlyCharges,na.rm = T))
rug(jitter(telco_churn$MonthlyCharges))
qqPlot(telco_churn$MonthlyCharges,main='Normal QQ plot of MonthlyCharges')
par(mfrow=c(1,1))

#Boxplot distributions for our numeric columns
#The dashed line shows the mean and the dark center line shows the median
#Difference between these two lines depict the deviation from the central limit theorem
boxplot(telco_churn$TotalCharges, ylab = "TotalCharges")
rug(jitter(telco_churn$TotalCharges), side = 2)
abline(h = mean(telco_churn$TotalCharges, na.rm = T), lty = 2)

boxplot(telco_churn$MonthlyCharges, ylab = "MonthlyCharges",outline = TRUE)
rug(jitter(telco_churn$MonthlyCharges), side = 2)
abline(h = mean(telco_churn$MonthlyCharges, na.rm = T), lty = 2)

#Plotting the TotalCharges and Monthl Charges with 3 lines for mean, median and mean+std
plot(telco_churn$TotalCharges, xlab = "")
abline(h = mean(telco_churn$TotalCharges, na.rm = T), lty = 1)
abline(h = mean(telco_churn$TotalCharges, na.rm = T) + sd(telco_churn$TotalCharges, na.rm = T),lty = 2)
abline(h = median(telco_churn$TotalCharges, na.rm = T), lty = 3)


#Plotting the TotalCharges and Monthl Charges with 3 lines for mean, median and mean+std
plot(telco_churn$MonthlyCharges, xlab = "")
abline(h = mean(telco_churn$MonthlyCharges, na.rm = T), lty = 1)
abline(h = mean(telco_churn$MonthCharges, na.rm = T) + sd(telco_churn$MonthlyCharges, na.rm = T),lty = 2)
abline(h = median(telco_churn$MonthlyCharges, na.rm = T), lty = 3)


#Scatterplot to understand the relationship between monthly and yearly charges
#This shows us that both of them are highly corelated which is kind of obvious
#For other categorical varibles, we compare only on one numeric column which is TotalCharges
#Checking for outliers using bvplot
plot(telco_churn$TotalCharges~telco_churn$MonthlyCharges,data=telco_churn,xlab="MonthlyCharges",ylab="TotalCharges")
with(telco_churn,text(telco_churn$MonthlyCharges,telco_churn$TotalCharges,cex=0.6,labels=abbreviate(row.names(telco_churn))))
x<-telco_churn[,c(19,20)]
bvbox(x,ylab = "TotalCharges",xlab="MonthlyCharges")


#Plotting joint boxplots for various categories wrt numerical column TotalCharges
bwplot(telco_churn$tenure_range ~ telco_churn$TotalCharges, data=telco_churn, ylab='Tenure',xlab='TotalCharges')
bwplot(telco_churn$gender ~ telco_churn$TotalCharges, data=telco_churn, ylab='Gender',xlab='TotalCharges')
bwplot(telco_churn$SeniorCitizen ~ telco_churn$TotalCharges, data=telco_churn, ylab='SeniorCitizen',xlab='TotalCharges')
bwplot(telco_churn$Partner ~ telco_churn$TotalCharges, data=telco_churn, ylab='Partner',xlab='TotalCharges')
bwplot(telco_churn$Dependents ~ telco_churn$TotalCharges, data=telco_churn, ylab='Dependents',xlab='TotalCharges')
bwplot(telco_churn$Contract ~ telco_churn$TotalCharges, data=telco_churn, ylab='Contract',xlab='TotalCharges')

#Plotting stripplots for various categories wrt numerical column TotalCharges
bwplot(telco_churn$tenure_range ~ telco_churn$TotalCharges, data=telco_churn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='Tenure',xlab='TotalCharges')
bwplot(telco_churn$gender ~ telco_churn$TotalCharges, data=telco_churn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='gender',xlab='TotalCharges')
bwplot(telco_churn$SeniorCitizen ~ telco_churn$TotalCharges, data=telco_churn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='SeniorCitizen',xlab='TotalCharges')
bwplot(telco_churn$Partner ~ telco_churn$TotalCharges, data=telco_churn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='Partner',xlab='TotalCharges')
bwplot(telco_churn$Dependents ~ telco_churn$TotalCharges, data=telco_churn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='Dependents',xlab='TotalCharges')
bwplot(telco_churn$Contract ~ telco_churn$TotalCharges, data=telco_churn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='Contract',xlab='TotalCharges')

##Creating Dummy Variables

#Converting double/int columns to numeric
numeric_col <- c("tenure","MonthlyCharges", "TotalCharges")
telco_churn[numeric_col] <- sapply(telco_churn[numeric_col], as.numeric)

#Segregating the numeric columns from categorical columns and storing them as a seperate dataframe
telco_churn_int <- telco_churn[,c("tenure","MonthlyCharges", "TotalCharges")]
telco_churn_int <- data.frame(scale(telco_churn_int))

#Creating dummy variables for the categorical data
telco_churn_cat <- telco_churn[,-c(1,6,19,20)]
dummy<- data.frame(sapply(telco_churn_cat,function(x) data.frame(model.matrix(~x-1,data =telco_churn_cat))[,-1]))
head(dummy)

#Combining the dummy and the numeric columns to form the final dataset
telco_churn_final <- cbind(telco_churn_int,dummy)
head(telco_churn_final)

##Matrix Plots, Covariance and Corelations Plots

#Next 4 lines were used to solve the error "Figure margins too large"
par("mar")
par(mar=c(1,1,1,1))
graphics.off()

#ScatterPlot matrix
pairs(telco_churn_final[,1:3],pch=".",cex=1.5)

#CorrelationMatrix
cormatrix <- round(cor(telco_churn_final),4)
#Heatmap for correlation matrix
#Negative correlations are shown in blue and positive in red
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cormatrix, col=col, symm=TRUE)

#Covariance Matrix
covmatrix <- round(cov(telco_churn_final),4)
#Heatmap for covariance matrix
#Negative correlations are shown in blue and positive in red
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(covmatrix, col=col, symm=TRUE)

#------------------------------------------------------------------------------------
##Test of Significance
#------------------------------------------------------------------------------------
#T-Test
#Null Hypothesis - The two means are equal
#Alternate Hypothesis - Difference in the two means is not zero
#pvalue >= 0.05, accept null hypothesis
#Or else accept the alternate hypothesis

#Univariate mean comparison using t test

#Totalcharges and Churn
with(data=telco_churn,t.test(telco_churn$TotalCharges[telco_churn$Churn=="Yes"],telco_churn$TotalCharges[telco_churn$Churn=="No"],var.equal=TRUE))

#MonthlyCharges and Churn
with(data=telco_churn,t.test(telco_churn$MonthlyCharges[telco_churn$Churn=="Yes"],telco_churn$MonthlyCharges[telco_churn$Churn=="No"],var.equal=TRUE))

#Totalcharges and Churn
with(data=telco_churn,t.test(telco_churn$TotalCharges[telco_churn$gender=="Male"],telco_churn$TotalCharges[telco_churn$gender=="Female"],var.equal=TRUE))

#MonthlyCharges and Churn
with(data=telco_churn,t.test(telco_churn$MonthlyCharges[telco_churn$gender=="Male"],telco_churn$MonthlyCharges[telco_churn$gender=="Female"],var.equal=TRUE))

#Multivariate mean comparison using Hotelling test

#Charges and gender
t2testgender <- hotelling.test(telco_churn$TotalCharges + telco_churn$MonthlyCharges ~ telco_churn$gender, data=telco_churn)
cat("T2 statistic =",t2testgender$stat[[1]],"\n")
print(t2testgender)

#Charges and Churn
t2testelco_churn <- hotelling.test(telco_churn$TotalCharges + telco_churn$MonthlyCharges ~ telco_churn$Churn, data=telco_churn)
cat("T2 statistic =",t2testelco_churn$stat[[1]],"\n")
print(t2testelco_churn)

#F Test
#Null Hypothesis - The two samples have same variance
#Alternate Hypothesis - Difference in the variance of two samples
#pvalue >= 0.05, accept null hypothesis
#Or else accept the alternate hypothesis

#The numerical columns we have do not have a normal distribution. Therefore we skip the F test

