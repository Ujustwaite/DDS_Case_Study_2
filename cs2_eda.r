library(readxl)
cs2Raw <- read_excel("~/SMU/Doing Data Science/CaseStudy2/CaseStudy2-data.xlsx")

groupYearsAtCompany = function(YearsAtCompany){
  group = character()
  if (YearsAtCompany >= 0 & YearsAtCompany <= 5) {group = "0-5"}
  if (YearsAtCompany > 5 & YearsAtCompany <= 10) {group = "5-10"}
  if (YearsAtCompany > 10 & YearsAtCompany <= 20) {group = "10-20"}
  if (YearsAtCompany > 20 & YearsAtCompany <= 30) {group = "20-30"}
  if (YearsAtCompany > 30 & YearsAtCompany <= 40) {group = "30-40"}
  if (YearsAtCompany > 40) {group = "40+"}
  return(group)
}

cs2Raw$YearsAtCompanyGroup = lapply(cs2Raw$YearsAtCompany, groupYearsAtCompany)
summary(cs2Raw)
#lots to see here

hist(cs2Raw$Age)
#Age looks normal, no "wave" of retirees coming. 

hist(cs2Raw$PerformanceRating)
#Performance rating system may not be functioning as desired. Unable to distinguish variations in performance. Lower scores?
#Need to check definitions. 

hist(cs2Raw$MonthlyIncome)
#just curious

hist(cs2Raw$DistanceFromHome)
#Significant number of employees live more than 10 miles away. Commuting incentives? 

hist(cs2Raw$YearsSinceLastPromotion)

#Look at Job Satisfaction Stats in different departments
boxplot(cs2Raw$JobSatisfaction~cs2Raw$Department)


#now by work role
boxplot(cs2Raw$JobSatisfaction~cs2Raw$JobRole, las = 2)
#ooohhhhhh There is something interesting! Human Resources Job Satisfaction below average! 
#Lots of below 3.0s here. 
boxplot(cs2Raw[which(cs2Raw$JobRole == "Human Resources"),]$JobSatisfaction~cs2Raw[which(cs2Raw$JobRole == "Human Resources"),]$EmployeeNumber)
#Mean human resources person
mean(cs2Raw[which(cs2Raw$JobRole == "Human Resources"),]$JobSatisfaction)
#Mean

#Just exploring something....but it doesn't appear to have born fruit
boxplot(cs2Raw$JobSatisfaction~as.character(cs2Raw$YearsAtCompanyGroup))

boxplot(cs2Raw$MonthlyIncome~cs2Raw$JobRole, las = 2)

#Looks like people's job satisfaction has nothing to do with how much they are being paid
boxplot(log(cs2Raw$MonthlyIncome)~cs2Raw$JobSatisfaction)

#by department
cs2Raw$EmployeeMonthlyProfit = cs2Raw$MonthlyRate - cs2Raw$MonthlyIncome
barplot(table(cs2Raw[which(cs2Raw$EmployeeMonthlyProfit < 0),5]))

#by job role
barplot(table(cs2Raw[which(cs2Raw$EmployeeMonthlyProfit < 0),16]), las = 2)

#attrition by

#Department



#Job satisfaction vs. attrition shows that happy people are not leaving, unhappy people are
boxplot(cs2Raw$JobSatisfaction~cs2Raw$Attrition)
#Maybe we can build a predictor to determine whether people will quit? 
#Candidate feature 
boxplot(cs2Raw$JobSatisfaction~cs2Raw$Attrition)
boxplot(log(cs2Raw$YearsSinceLastPromotion)~cs2Raw$Attrition)
boxplot(log(cs2Raw$YearsAtCompany)~cs2Raw$Attrition)
boxplot(cs2Raw$JobLevel~cs2Raw$Attrition)
boxplot(log(cs2Raw$MonthlyIncome)~cs2Raw$Attrition)
boxplot(cs2Raw$DistanceFromHome~cs2Raw$Attrition)

cs2Raw$AttritionCode = 0; 
cs2Raw[which(cs2Raw$Attrition == "Yes"),]$AttritionCode = 1
cs2Raw[which(cs2Raw$Attrition == "No"), ]$AttritionCode = 0

############Model Attrition#########################

trainset = cs2Raw[1:985,]
testset = cs2Raw[986:1470,]

modeltest = glm(data = trainset, AttritionCode~JobSatisfaction + factor(Department) + factor(JobRole) + DistanceFromHome + YearsAtCompany + YearsSinceLastPromotion + DistanceFromHome*JobSatisfaction, family = "binomial")
predictions = predict(modeltest, newdata = testset, type = "response")

testset$PredictedAttrition = predictions
testset[which(testset$PredictedAttrition >= 0.34), ]$PredictedAttrition = 1
testset[which(testset$PredictedAttrition < 0.34), ]$PredictedAttrition = 0
results = as.data.frame(cbind(testset$AttritionCode, testset$PredictedAttrition))
View(results)

results$match = results$V1 == results$V2
library(MLmetrics)
Precision(results$V1, results$V2)
Recall(results$V1, results$V2)
Accuracy(results$V1, results$V2)

####################################################