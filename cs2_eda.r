library(readxl)
library(caret)
library(dplyr)
cs2Raw <- read_excel("~/SMU/Doing Data Science/CaseStudy2/CaseStudy2-data.xlsx")
#cs2Raw <-read_xlsx("C:/Users/garapati/Desktop/data/CaseStudy2-data.xlsx",sheet=1)

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

fill <- "#4271AE"
line <- "#1F3552"
ggplot(cs2Raw, aes(x = Age)) + 
  geom_histogram(binwidth = 1.0, color=line, fill=fill) + 
  ggtitle("DDSAnalytics Employee Age Distribution")+
  theme_minimal()

#Number of Departments
length(unique(cs2Raw$Department))

#Number of employees
length(unique(cs2Raw$EmployeeNumber))

#Number of work roles
length(unique(cs2Raw$JobRole))

#GenderStats
male = dim(cs2Raw[which(cs2Raw$Gender == "Male"),])[1]
maleperc = male / 1470 * 100
female = dim(cs2Raw[which(cs2Raw$Gender == "Female"),])[1]
femaleperc = female / 1470 * 100
genderDF = as.data.frame(c("Male","Female"))
genderDF = cbind(genderDF, c(maleperc, femaleperc))
names(genderDF) = c("Gender","Percent")
genderplot = ggplot(data = genderDF, aes(x = "", y = Percent, fill = Gender)) + geom_bar(width = 1, stat = "identity")
pie = genderplot + coord_polar("y", start = 0)+ scale_fill_brewer(palette="Blues")+
  geom_text(aes(y = Percent/2 + c(0, cumsum(Percent)[-length(Percent)]), label = c("Male", "Female")), size=5)+
  theme_minimal()+
  ggtitle("Workforce Gender Distribution")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x=element_blank())
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

fill <- "#4271AE"
line <- "#1F3552"
ggplot(cs2Raw, aes(x = Department, y = JobSatisfaction)) + 
  geom_boxplot(fill = fill, colour = line, alpha = 0.7)+ 
  scale_x_discrete(name = "Department")+
  scale_y_continuous(name = "Job Satisfaction Score Distribution", limits=c(0.5, 4.5))+
  ggtitle("Job Satisfaction Survey Score by Department")+
  theme_minimal()


#now by work role
boxplot(cs2Raw$JobSatisfaction~cs2Raw$JobRole, las = 2)

ggplot(cs2Raw, aes(x = JobRole, y = JobSatisfaction)) + 
  geom_boxplot(fill = fill, colour = line, alpha = 0.7)+ 
  scale_x_discrete(name = "Job Role")+
  scale_y_continuous(name = "Job Satisfaction Score Distribution", limits=c(0.5, 4.5))+
  ggtitle("Job Satisfaction Survey Score by Job Role")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Get mean for non-HR roles
mean_non_HR = mean(cs2Raw[which(cs2Raw$JobRole != "Human Resources"),]$JobSatisfaction)
#Get mean for HR role
mean_HR = mean(cs2Raw[which(cs2Raw$JobRole == "Human Resources"),]$JobSatisfaction)
#Find the difference

diff = 100-(mean_HR / mean_non_HR)*100


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

cs2Raw$OverTimeCode = 0; 
cs2Raw[which(cs2Raw$OverTime == "Yes"),]$OverTimeCode = 1
cs2Raw[which(cs2Raw$OverTime == "No"), ]$OverTimeCode = 0

#attrition
denominator = dim(cs2Raw[which(cs2Raw$JobRole == "Human Resources"),])[1]
numerator = sum(cs2Raw[which(cs2Raw$JobRole == "Human Resources"),]$AttritionCode)
percentAttrition = numerator / denominator * 100
jobrolelist = unique(cs2Raw$JobRole)
jobroleAttritionRates = data.frame()
for (jobrole in jobrolelist) { 
  denominator = dim(cs2Raw[which(cs2Raw$JobRole == jobrole),])
  numerator = sum(cs2Raw[which(cs2Raw$JobRole == jobrole),]$AttritionCode)
  percentAttrition = round(numerator/denominator[1]*100, 2)
  jobroleAttritionRates = rbind(percentAttrition,jobroleAttritionRates)
}
jobroleAttritionRates = cbind(jobroleAttritionRates, jobrolelist)
jobroleAttritionRates = as.data.frame(jobroleAttritionRates)
names(jobroleAttritionRates) = c("Rate","JobRole")
ggplot(jobroleAttritionRates, aes(x = factor(JobRole), y = Rate)) + 
  xlab("Job Role")+ 
  ylab("Attrition Rate")+
  geom_bar(stat = "identity") +
  coord_flip()

###Addit QOL data by Aditya
library(plotly)
newdata_attrition<- subset(cs2Raw, Attrition=="Yes" )
newdata_stay <- subset(cs2Raw,Attrition=="No" )
qol_col <- c('Attrition','BusinessTravel','DistanceFromHome','OverTime','MaritalStatus')
qol_attrition_data <- newdata_attrition[,qol_col]
qol_stay_data <- newdata_stay[,qol_col]
plot_ly(qol_stay_data,labels=~BusinessTravel,type = 'pie')%>%layout(title = "Business Travel (people who stayed) ")
plot_ly(qol_attrition_data,labels=~BusinessTravel,type = 'pie')%>%layout(title = "Business Travel (people who left) ")

plot_ly(qol_stay_data,labels=~OverTime,type = 'pie')%>%layout(title = "OverTime (people who stayed) ")
plot_ly(qol_attrition_data,labels=~OverTime,type = 'pie')%>%layout(title = "OverTime (people who left) ")





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


###################################################
#Model 2

model = glm(data = trainset, formula = AttritionCode~Age + DailyRate + Education + HourlyRate+ JobRole + MonthlyIncome + PerformanceRating + StockOptionLevel + WorkLifeBalance + YearsSinceLastPromotion + Department + EducationField + EnvironmentSatisfaction + JobInvolvement + MonthlyRate + OverTime + RelationshipSatisfaction + TotalWorkingYears + YearsAtCompany + YearsWithCurrManager + BusinessTravel + DistanceFromHome + EmployeeCount + Gender + JobLevel + MaritalStatus + NumCompaniesWorked + PercentSalaryHike + StandardHours + TrainingTimesLastYear + YearsInCurrentRole, family = binomial(logit))

model = glm(data = trainset, formula = AttritionCode ~ Age + JobRole + StockOptionLevel + WorkLifeBalance + 
              YearsSinceLastPromotion + EnvironmentSatisfaction + JobInvolvement + 
              OverTime + RelationshipSatisfaction + YearsAtCompany + YearsWithCurrManager + 
              BusinessTravel + DistanceFromHome + Gender + MaritalStatus + 
              NumCompaniesWorked + YearsInCurrentRole, family = binomial(logit))

importance = varImp(model)
storeNames = row.names(importance)
storeNames = cbind(storeNames,importance)
storeNames = arrange(storeNames, -Overall)

names(storeNames) = c("Variable","Importance")
summary(model)

predictions = predict(model, newdata = testset, type = "response")

testset$PredictedAttrition = predictions
testset[which(testset$PredictedAttrition >= 0.5), ]$PredictedAttrition = 1
testset[which(testset$PredictedAttrition < 0.5), ]$PredictedAttrition = 0
results = as.data.frame(cbind(testset$AttritionCode, testset$PredictedAttrition))
View(results)

Precision(results$V1, results$V2)
Recall(results$V1, results$V2)
Accuracy(results$V1, results$V2)


