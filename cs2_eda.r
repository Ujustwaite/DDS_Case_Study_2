library(readxl)
cs2Raw <- read_excel("~/SMU/Doing Data Science/CaseStudy2/CaseStudy2-data.xlsx")

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

boxplot(cs2Raw$MonthlyIncome~cs2Raw$JobRole, las = 2)

#Looks like people's job satisfaction has nothing to do with how much they are being paid
boxplot(log(cs2Raw$MonthlyIncome)~cs2Raw$JobSatisfaction)

#by department
cs2Raw$EmployeeMonthlyProfit = cs2Raw$MonthlyRate - cs2Raw$MonthlyIncome
barplot(table(cs2Raw[which(cs2Raw$EmployeeMonthlyProfit < 0),5]))

#by job role
barplot(table(cs2Raw[which(cs2Raw$EmployeeMonthlyProfit < 0),16]), las = 2)


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

modeltest = glm(data = trainset, AttritionCode~JobSatisfaction + factor(Department) + factor(JobRole) + JobLevel + DistanceFromHome + YearsAtCompany + YearsSinceLastPromotion + MonthlyIncome + DistanceFromHome*JobSatisfaction, family = "binomial")
predictions = predict(modeltest, newdata = testset, type = "response")

testset$PredictedAttrition = predictions
testset[which(testset$PredictedAttrition >= 0.33), ]$PredictedAttrition = 1
testset[which(testset$PredictedAttrition < 0.33), ]$PredictedAttrition = 0
results = as.data.frame(cbind(testset$AttritionCode, testset$PredictedAttrition))
View(results)

results$match = results$V1 == results$V2
library(MLmetrics)
Precision(results$V1, results$V2)
Recall(results$V1, results$V2)
Accuracy(results$V1, results$V2)

####################################################