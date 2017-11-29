#TEAM 9: Vatsal Sanghavi, Annabelle Nguyen, Sophia Rowland, Alper Sayiner
#IBM HR Analytics Dataset from Kaggle 
#at : https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset

#Set the working directory to source file location
#Also save the files deviance_2015, fdr, FPR_TPR, installpackages, lambda_theory
#and lasso_aux and the dataset in the working directory

#Data Preparation Step

#Load the source files and libraries
source("installpackages.R")
source("FPR_TPR.R")
installpkg("tree")
library(tree)
installpkg("partykit")
library(partykit)
source("deviance_2015.R")
source("fdr.R")

#Load the data in the dataframe
df <- read.csv("Data.csv", header=TRUE)
#The "Age" column name is not displayed correctly so we correct it
colnames(df)[1] <- "Age"
summary(df)

#Employee Count is all 1's
#Employee number likely not related to attrition
#Standard hours is all 80
#Over18 is all Y's
#So we remove these columns
drop <- c("EmployeeCount","EmployeeNumber","StandardHours", "Over18")
df <- df[,!(names(df) %in% drop)]

#Creating binary for Attrition which is our predictor variable
df$Attrition <- ifelse(df$Attrition == "Yes", 1, 0)

#Some of the variables are not correctly identified as categorical variables
#So we code them as factors now
df$Education <- factor(df$Education)
df$EnvironmentSatisfaction <- factor(df$EnvironmentSatisfaction)
df$JobInvolvement <- factor(df$JobInvolvement)
df$JobLevel <- factor(df$JobLevel)
df$JobSatisfaction <- factor(df$JobSatisfaction)
df$PerformanceRating <- factor(df$PerformanceRating)
df$RelationshipSatisfaction <- factor(df$RelationshipSatisfaction)
df$StockOptionLevel <- factor(df$StockOptionLevel)
df$WorkLifeBalance <- factor(df$WorkLifeBalance)

#Plotting a correlation matrix to test our intitial intuition
M<-cor(df[sapply(df, function(x) !is.factor(x))])
library(corrplot)
corrplot(M, method="circle")

#Converting attrition into a factor variable
df$Attrition <- factor(df$Attrition)

summary(df)

#Lets look at the distribution of varaibles
hist(df$Age) #Normal distribution with center around 30-35
barplot(table(df$Attrition)) #More people in this set stayed than left
barplot(table(df$BusinessTravel)) #Most people traveled rarely
hist(df$DailyRate) #Uniform distribution 
barplot(table(df$Department)) #Most people work in R&D
hist(df$DistanceFromHome) #Distribution skewed right
barplot(table(df$Education)) #Most people have their bachelors or masters degree
barplot(table(df$EducationField)) #Most people have their degrees in life sciences
barplot(table(df$EnvironmentSatisfaction)) #People are satisfied with their environments
barplot(table(df$Gender)) #More males than females
hist(df$HourlyRate) #Uniform distribution
barplot(table(df$JobInvolvement)) #Most people are involved highly in their jobs
barplot(table(df$JobLevel)) #Most people are lower leveled
barplot(table(df$JobRole)) #Wide variety or roles
barplot(table(df$JobSatisfaction)) #People are satified with their jobs
barplot(table(df$MaritalStatus)) #Most people are married
hist(df$MonthlyIncome) #Distribution skewed right
hist(df$MonthlyRate) #Uniform distribution
barplot(table(df$NumCompaniesWorked)) #Most people worked for one other company beforehand
barplot(table(df$OverTime)) #Most people dont recive overtime
hist(df$PercentSalaryHike) #Distribution skewed right
barplot(table(df$PerformanceRating)) #Most people do their jobs well
barplot(table(df$RelationshipSatisfaction)) #People are satisfied with their jobs
barplot(table(df$StockOptionLevel)) #Most people have no or litte stock option
hist(df$TotalWorkingYears) #Most people have worked 10 or fewer years
hist(df$TrainingTimesLastYear) #Most people had two or three trainings
barplot(table(df$WorkLifeBalance)) #Most people said their work life balance was better
hist(df$YearsAtCompany) #Distribution right skewed
hist(df$YearsInCurrentRole) #Distribution right skewed
hist(df$YearsSinceLastPromotion) #Distribution right skewed
hist(df$YearsWithCurrManager) #Distribution right skewed

#Lets explore gender and attriction now
counts <- table(df$Attrition, df$Gender) 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Gender and Attrition",
        names.arg=c("Female", "Male"), col=c("blue","red"),
        legend = c("No", "Yes"))
#Attrition doesnt look very differnt across gender 

#Lets explore number of companies and attrition 
counts <- table(df$Attrition, df$NumCompaniesWorked) 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Number of Companies Worked and Attrition",
        col=c("blue","red"), legend = c("No", "Yes"))
#Interesting. So working at 5, 6, 7, or 9 companies make you more likley leave. 
#Possibly because they have been moving around a lot. 

#Lets explore stock options and attrition
counts <- table(df$Attrition, df$StockOptionLevel) 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Stock Options and Attrition",
        names.arg=c("No Stock", "Some Stock", "More Stock", "Most Stock"), col=c("blue","red"), 
        legend = c("No", "Yes"))
#So people like the stock options.  Possibly because they are more invested in the company. 

#Lets explore work-balance and attrition
counts <- table(df$Attrition, df$WorkLifeBalance) 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Work Life Balance and Attrition",
        names.arg=c("Bad", "Good", "Better", "Best"),col=c("blue","red"), 
        legend = c("No", "Yes"))
#So people who feel like their job is taking too much of their time is more likely to leave.
#Makes sense. 

#Lets explore work-life balance and job satisfaction
counts <- table(df$JobSatisfaction, df$WorkLifeBalance) 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Work Life Balance and Job Satisfacion",
        names.arg=c("Bad", "Good", "Better", "Best"), col=c("blue","red", "green", "yellow"), 
        legend = c("Low", "Medium", "High", "Very High"))
#Interesting.  People are faily consistent about their job satisfaction 
#regardless of work-life balance

#Lets explore job satisfaction and attrition 
counts <- table(df$Attrition, df$JobSatisfaction) 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Job Satisfaction and Attrition",
        names.arg = c("Low", "Medium", "High", "Very High"), col=c("blue","red"), 
        legend = c("No", "Yes"))
#So people who like their job more are least likely to leave. 
#Makes sense.

#Lets explore work-life balance and marital status 
counts <- table(df$MaritalStatus, df$WorkLifeBalance) 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Work Life Balance and Job Satisfacion",
        names.arg=c("Bad", "Good", "Better", "Best"), col=c("blue","red", "green"), 
        legend = c("Divorced", "Married", "Single"))
#So married people have worse work-life balance. Possibly because their life part 
#is more demanding.

#We will now explore the unsupervised learning method of clustering

#Load the necessary source file packages
installpkg("ElemStatLearn")
library(ElemStatLearn)
installpkg("class")
library(class)

#Converting all to numeric variables 
numeric_matrix<- model.matrix(Attrition ~ ., data=df)
numeric_data <- as.data.frame(numeric_matrix)

#Random seed for replication purposes
set.seed(1)

#Here we set the radius so that the area of the bubble reflects the MonthlyIncome 
radius <- sqrt(numeric_data$MonthlyIncome/pi)

#Cluster 1: Between NumCompaniesWorked and JobSatisfaction4

#Selecting the variables of interest
Performance <-numeric_data[c("NumCompaniesWorked", "JobSatisfaction4")]
x <- c("NumCompaniesWorked")
y <- c("JobSatisfaction4")

#Set the number of centers for k-means 
num_centers <- 2

### we need normalization of the data
Performance[,1] <- ( Performance[,1] - mean(Performance[,1]))/ sd(Performance[,1])
Performance[,2] <- ( Performance[,2] - mean(Performance[,2]))/ sd(Performance[,2])

#Computing kmeans
numeric_data_kmeans <- kmeans(Performance,num_centers)

colorcluster <- 1+numeric_data_kmeans$cluster

#Plotting the cluster
plot(Performance, xlab=x, ylab=y, col = colorcluster)
symbols(Performance,circles=radius, xlab=x, ylab=y, bg = colorcluster)
points(numeric_data_kmeans$centers, col = 2:4, pch = 8, cex = 2)

#Cluster 2: Between Age and NumCompaniesWorked

#Selecting the variables of interest
Performance <-numeric_data[c("Age", "NumCompaniesWorked")]
x <- c("Age")
y <- c("NumCompaniesWorked")

#Set the number of centers for k-means 
num_centers <- 2

### we need normalization of the data
Performance[,1] <- ( Performance[,1] - mean(Performance[,1]))/ sd(Performance[,1])
Performance[,2] <- ( Performance[,2] - mean(Performance[,2]))/ sd(Performance[,2])

#Computing kmeans
numeric_data_kmeans <- kmeans(Performance,num_centers)

colorcluster <- 1+numeric_data_kmeans$cluster

#Plotting the cluster
plot(Performance, xlab=x, ylab=y, col = colorcluster)
symbols(Performance,circles=radius, xlab=x, ylab=y, bg = colorcluster)
points(numeric_data_kmeans$centers, col = 2:4, pch = 8, cex = 2)

#End of unsupervised learning

#Installing required files for Lasso
source("lasso_aux.R")
installpkg("glmnet")
library(glmnet)

#Now Running Lasso without interactions
#This defines the features we will use the matrix Mx_interaction (X) and the target My (Y)
Mx<- model.matrix(Attrition ~ ., data=df)
My<- df$Attrition == "1"

#Finding Lambda for lasso without interactions
num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.Attrition <- sum(My)
w <- (num.Attrition/num.n)*(1-(num.Attrition/num.n))
# For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)

lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
summary(lassoTheory)
support(lassoTheory$beta)
colnames(Mx)[support(lassoTheory$beta)]
length(support(lassoTheory$beta))
#So Lasso without interaction gives 16 variables that explain attrition

dfa<-df

#Next, we test the relationship between attrition and the 16 variables
#by using chi sq test
#And find the ones that are significant and create a dataframe df1 with them and common with lasso

#We create new dummies for factors that were significant based on lasso
dfa$DepartmentResearch.Development <- ifelse(dfa$Department == "Research & Development", 1, 0)
dfa$JobLevel2 <- ifelse(dfa$JobLevel == "2", 1, 0)
dfa$JobRoleSales.Representative<- ifelse(dfa$JobRole == "Sales Representative", 1, 0)
dfa$MaritalStatusSingle <- ifelse(dfa$MaritalStatus == "Single", 1, 0)
dfa$StockOptionLevel1 <- ifelse(dfa$StockOptionLevel == "1", 1, 0)
dfa$BusinessTravelTravel_Frequently <- ifelse(dfa$BusinessTravel == "Travel_Frequently", 1, 0)
dfa$JobRoleLaboratory.Technician <- ifelse(dfa$JobRole == "Laboratory Technician", 1, 0)
dfa$JobSatisfaction4 <- ifelse(dfa$JobSatisfaction == "4", 1, 0)
dfa$OverTimeYes <- ifelse(dfa$OverTime == "Yes", 1, 0)

#Converting those dummies to factors
dfa$DepartmentResearch.Development <- factor(dfa$DepartmentResearch.Development)
dfa$JobLevel2 <- factor(dfa$JobLevel2)
dfa$JobRoleSales.Representative<-factor(dfa$JobRoleSales.Representative)
dfa$MaritalStatusSingle <-factor(dfa$MaritalStatusSingle)
dfa$StockOptionLevel1 <- factor(dfa$StockOptionLevel1)
dfa$BusinessTravelTravel_Frequently <-factor(dfa$BusinessTravelTravel_Frequently)
dfa$JobRoleLaboratory.Technician <- factor(dfa$JobRoleLaboratory.Technician)
dfa$JobSatisfaction4 <- factor(dfa$JobSatisfaction4)
dfa$OverTimeYes <- factor(dfa$OverTimeYes) 

#Keeping only the 16 variables that lasso gives us (along with attrition of course)
keep_dfa <- c("Attrition","DepartmentResearch.Development","JobLevel2","JobRoleSales.Representative","MaritalStatusSingle","StockOptionLevel1","BusinessTravelTravel_Frequently","JobRoleLaboratory.Technician","JobSatisfaction4","OverTimeYes", "Age","TotalWorkingYears","YearsInCurrentRole","DistanceFromHome","MonthlyIncome","YearsWithCurrManager","NumCompaniesWorked")
dfa <- dfa[,(names(dfa) %in% keep_dfa)]

#Running chisq test for attrition against each of 16 variables
chisq.test(dfa$Attrition,dfa$DepartmentResearch.Development) #sig
chisq.test(dfa$Attrition,dfa$JobLevel2) #sig
chisq.test(dfa$Attrition,dfa$JobRoleSales.Representative) #sig
chisq.test(dfa$Attrition,dfa$MaritalStatusSingle) #sig
chisq.test(dfa$Attrition,dfa$StockOptionLevel1) #sig
chisq.test(dfa$Attrition,dfa$BusinessTravelTravel_Frequently) #sig
chisq.test(dfa$Attrition,dfa$JobRoleLaboratory.Technician) #sig
chisq.test(dfa$Attrition,dfa$JobSatisfaction4) #sig
chisq.test(dfa$Attrition,dfa$OverTimeYes) #sig
chisq.test(dfa$Attrition,dfa$Age) #sig
chisq.test(dfa$Attrition,dfa$NumCompaniesWorked) #sig
chisq.test(dfa$Attrition,dfa$TotalWorkingYears) #sig
chisq.test(dfa$Attrition,dfa$YearsInCurrentRole) #sig
chisq.test(dfa$Attrition,dfa$DistanceFromHome) #insig
chisq.test(dfa$Attrition,dfa$MonthlyIncome) #insig
chisq.test(dfa$Attrition,dfa$YearsWithCurrManager) #sig

#So we find that all are significant except Distancefromhome and monthlyincome so we drop them
#from our final dataframe df1

drop_final <- c("DistanceFromHome", "MonthlyIncome")
df1 <- dfa[, !names(dfa) %in% drop_final]

#Running Lasso with interactions
#This defines the features we will use the matrix Mx_interaction (X) and the target My_interaction (Y)
Mx_interaction<- model.matrix(Attrition ~ .^2, data=df)
My_interaction<- df$Attrition == "1"

#Finding Lambda for interactions
num.features_interaction <- ncol(Mx_interaction)
num.n_interaction <- nrow(Mx_interaction)
num.Attrition_interaction <- sum(My_interaction)
w_interaction <- (num.Attrition_interaction/num.n_interaction)*(1-(num.Attrition_interaction/num.n_interaction))
#For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w_interaction*log(num.features_interaction/0.05)/num.n_interaction)

lassoTheory_interaction <- glmnet(Mx_interaction,My_interaction, family="binomial",lambda = lambda.theory)
summary(lassoTheory_interaction)
support(lassoTheory_interaction$beta)
colnames(Mx_interaction)[support(lassoTheory_interaction$beta)]
length(support(lassoTheory_interaction$beta))
#So Lasso without interactions gives 30 variables that explain attrition

#Next, we test the relationship between attrition and the 30 variables 
#by using a chi sq test
#And find the ones that are significant and create a dataframe df2 with them and common with lasso

#Creating a dataframe for variables with lasso interactions
df_interaction <- model.matrix(~.^2, df)
df_interaction <- data.frame(df_interaction)

#Keeping the variables that were shown by lasso without interactions 
keep_names <- c("Age", 
                "Attrition",
                "TotalWorkingYears", 
                "Age.StockOptionLevel1",
                "BusinessTravelTravel_Frequently.DistanceFromHome",
                "BusinessTravelTravel_Frequently.JobRoleSales.Representative",
                "BusinessTravelTravel_Frequently.MaritalStatusSingle",
                "DailyRate.StockOptionLevel1", 
                "DepartmentResearch.&.Development.JobLevel2", 
                "DepartmentSales.MaritalStatusSingle", 
                "DepartmentResearch.&.Development.MonthlyIncome",
                "DistanceFromHome.MaritalStatusSingle", 
                "DistanceFromHome.OverTimeYes",
                "Education3.JobRoleSales.Representative", 
                "EducationFieldTechnical.Degree.MaritalStatusSingle",
                "GenderMale.OverTimeYes", 
                "JobLevel2.YearsAtCompany",
                "JobLevel2.YearsInCurrentRole", 
                "JobRoleLaboratory.Technician.MaritalStatusSingle",
                "JobRoleSales.Representative.MaritalStatusSingle",
                "JobRoleLaboratory.Technician.OverTimeYes", 
                "JobRoleSales.Representative.OverTimeYes",
                "JobRoleSales.Representative.WorkLifeBalance4",
                "JobSatisfaction4.WorkLifeBalance3",
                "MaritalStatusSingle.OverTimeYes",
                "MaritalStatusSingle.PercentSalaryHike", 
                "NumCompaniesWorked.OverTimeYes",
                "OverTimeYes.PercentSalaryHike", 
                "PercentSalaryHike.TotalWorkingYears",                       
                "TrainingTimesLastYear.YearsInCurrentRole", 
                "WorkLifeBalance3:YearsWithCurrManager"
)
df_interaction <- df_interaction[,(names(df_interaction) %in% keep_names)]

#But some columns were not picked up because model.matrix drops the
#first category in every factor variable
#So the missing columns are [8], [10] and [30] from the lasso interaction model
#So we create new columns that were not picked up by model.matrix but we still need to run test on
df_interaction$DepartmentResearch.Development <- ifelse(df$Department == "Research & Development",1,0)
df_interaction$JobLevel2 <- ifelse(df$JobLevel == 2, 1,0)
df_interaction$MonthlyIncome <- df$MonthlyIncome
df_interaction$WorkLifeBalance3 <- ifelse(df$WorkLifeBalance == 3, 1, 0)
df_interaction$YearsWithCurrManager <- df$YearsWithCurrManager
df_interaction$Attrition <- df$Attrition

#Center the input variables to create interaction terms
df_interaction$DepartmentResearch.Developmentc <- df_interaction$DepartmentResearch.Development - mean(df_interaction$DepartmentResearch.Development)
df_interaction$JobLevel2c <- df_interaction$JobLevel2 - mean(df_interaction$JobLevel2)
df_interaction$MonthlyIncomec <- df_interaction$MonthlyIncome - mean(df_interaction$MonthlyIncome)
df_interaction$WorkLifeBalance3c <- df_interaction$WorkLifeBalance3 - mean(df_interaction$WorkLifeBalance3)
df_interaction$YearsWithCurrManagerc <- df_interaction$YearsWithCurrManager - mean(df_interaction$YearsWithCurrManager)

#Create the interaction variables
df_interaction$DepartmentResearch.Development.JobLevel2 <- df_interaction$DepartmentResearch.Developmentc * df_interaction$JobLevel2c
df_interaction$DepartmentResearch.Development.MonthlyIncome <- df_interaction$DepartmentResearch.Developmentc * df_interaction$MonthlyIncomec
df_interaction$WorkLifeBalance3.YearsWithCurrManager <- df_interaction$WorkLifeBalance3c * df_interaction$YearsWithCurrManagerc

#Drop the intermediate variables that were used to create the interaction
summary(df_interaction)
drop_df_interaction <- c("DepartmentResearch.Development", 
                         "JobLevel2", 
                         "MonthlyIncome", 
                         "WorkLifeBalance3", 
                         "YearsWithCurrManager", 
                         "DepartmentResearch.Developmentc",
                         "JobLevel2c",
                         "MonthlyIncomec",
                         "WorkLifeBalance3c",
                         "YearsWithCurrManagerc",
                         "DepartmentResearch.Development.JobLevel2c")
df_interaction <- df_interaction[, !names(df_interaction) %in% drop_df_interaction]

#Test the relationship between each of the 30 variables and attrition
names(df_interaction)
chisq.test(df_interaction$Attrition, df_interaction$Age)#sig
chisq.test(df_interaction$Attrition, df_interaction$TotalWorkingYears)#sig
chisq.test(df_interaction$Attrition, df_interaction$Age.StockOptionLevel1) #sig
chisq.test(df_interaction$Attrition, 
           df_interaction$BusinessTravelTravel_Frequently.DistanceFromHome) #sig
chisq.test(df_interaction$Attrition, 
           df_interaction$BusinessTravelTravel_Frequently.JobRoleSales.Representative) #sig
chisq.test(df_interaction$Attrition, 
           df_interaction$BusinessTravelTravel_Frequently.MaritalStatusSingle) #sig
chisq.test(df_interaction$Attrition, df_interaction$DailyRate.StockOptionLevel1) #insig
chisq.test(df_interaction$Attrition, df_interaction$DepartmentSales.MaritalStatusSingle) #sig
chisq.test(df_interaction$Attrition, df_interaction$DistanceFromHome.MaritalStatusSingle) #sig
chisq.test(df_interaction$Attrition, df_interaction$DistanceFromHome.OverTimeYes) #sig
chisq.test(df_interaction$Attrition, df_interaction$Education3.JobRoleSales.Representative) #sig
chisq.test(df_interaction$Attrition, 
           df_interaction$EducationFieldTechnical.Degree.MaritalStatusSingle) #sig
chisq.test(df_interaction$Attrition, df_interaction$GenderMale.OverTimeYes) #sig
chisq.test(df_interaction$Attrition, df_interaction$JobLevel2.YearsAtCompany) #sig
chisq.test(df_interaction$Attrition, df_interaction$JobLevel2.YearsInCurrentRole) #sig
chisq.test(df_interaction$Attrition, df_interaction$JobRoleLaboratory.Technician.MaritalStatusSingle) #sig
chisq.test(df_interaction$Attrition, df_interaction$JobRoleSales.Representative.MaritalStatusSingle) #sig
chisq.test(df_interaction$Attrition, df_interaction$JobRoleLaboratory.Technician.OverTimeYes) #sig
chisq.test(df_interaction$Attrition, df_interaction$JobRoleSales.Representative.OverTimeYes) #sig
chisq.test(df_interaction$Attrition, df_interaction$JobRoleSales.Representative.WorkLifeBalance4) #sig
chisq.test(df_interaction$Attrition, df_interaction$JobSatisfaction4.WorkLifeBalance3) #sig
chisq.test(df_interaction$Attrition, df_interaction$MaritalStatusSingle.OverTimeYes) #sig
chisq.test(df_interaction$Attrition, df_interaction$MaritalStatusSingle.PercentSalaryHike) #sig
chisq.test(df_interaction$Attrition, df_interaction$NumCompaniesWorked.OverTimeYes) #sig
chisq.test(df_interaction$Attrition, df_interaction$OverTimeYes.PercentSalaryHike) #sig
chisq.test(df_interaction$Attrition, df_interaction$PercentSalaryHike.TotalWorkingYears) #sig
chisq.test(df_interaction$Attrition, df_interaction$TrainingTimesLastYear.YearsInCurrentRole) #sig
chisq.test(df_interaction$Attrition, df_interaction$DepartmentResearch.Development.JobLevel2) #sig
chisq.test(df_interaction$Attrition, df_interaction$DepartmentResearch.Development.MonthlyIncome) #insig
chisq.test(df_interaction$Attrition, df_interaction$WorkLifeBalance3.YearsWithCurrManager) #sig
#All variables are significant except for DepartmentResearch.Development.MonthlyIncome and 
#DailyRate.StockOptionLevel1 and so drop them in our final dataframe df2

drop_final_interaction <- c("DepartmentResearch.Development.MonthlyIncome", "DailyRate.StockOptionLevel1")
df2 <- df_interaction[, !names(df_interaction) %in% drop_final_interaction]

#So finally we will be building our models on df1 and df2 and choosing the best model
#df1 are the variables without interactions and df2 are with interactions
summary(df1)
summary(df2)

### This will turn off warning messages
options(warn=-1)
### We are setting the seed to be 1; so it is easier to replicate.
set.seed(1)

#Calculating the ROC curve 
#Creating the folds for validation
nfold <- 2
n1 <- nrow(df1)  # the number of observations in df1 (without interactions)
n2 <- nrow(df2)  # the number of observations in df2 (with interactions)

#Create a vector of fold memberships (random order)
foldid1 <- rep(1:nfold,each=ceiling(n1/nfold))[sample(1:n1)]
foldid2 <- rep(1:nfold,each=ceiling(n2/nfold))[sample(1:n2)]

#Now lets add our models
model.logistic.interaction <- glm(Attrition~., data=df2, subset=which(foldid2==1), family="binomial")
model.logistic <- glm(Attrition~., data=df1, subset=which(foldid1==1), family="binomial")
model.tree <- tree(Attrition~ ., data=df1, subset=which(foldid1==1)) 
model.tree.interaction <- tree(Attrition~ ., data=df2, subset=which(foldid2==1)) 
model.null <- glm(Attrition~1, data=df1, subset=which(foldid1==1), family="binomial")

#Lets plot FPR and TPR
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
val<- .5
#Logistic Interaction
values <- FPR_TPR( (model.logistic.interaction$fitted >= val) , model.logistic.interaction$y )
points( values$FPR , values$TPR )
ACC.model.logistic.interaction <- values$ACC
text( values$FPR+.15, values$TPR+.30, labels=c("LR with int."))
#Logistic
values <- FPR_TPR( (model.logistic$fitted >= val) , model.logistic$y )
points( values$FPR , values$TPR)    
ACC.model.logistic <- values$ACC
text( values$FPR+.30, values$TPR+.50, labels=c("LR"))
#Tree
values <- FPR_TPR( (predict(model.tree,type="class") == "Yes") , model.logistic.interaction$y )
points( values$FPR , values$TPR )    
ACC.model.tree <- values$ACC
text( values$FPR+.10, values$TPR+.15, labels=c("tree"))
#Tree Interaction
values <- FPR_TPR( (predict(model.tree.interaction,type="class") == "Yes") , model.logistic.interaction$y )
points( values$FPR , values$TPR )    
ACC.model.tree.interaction <- values$ACC
text( values$FPR+.10, values$TPR+.80, labels=c("tree with int."))
for( val in seq(from=0,to=1,by=0.05)){
  #Logistic Interaction
  values <- FPR_TPR( (model.logistic.interaction$fitted >= val) , model.logistic.interaction$y )
  points( values$FPR , values$TPR, pch = 21, bg="red" )
  #Logistic
  values <- FPR_TPR( (model.logistic$fitted >= val) , model.logistic$y )
  points( values$FPR , values$TPR, pch = 22, bg="blue" )    
  #Tree
  values <- FPR_TPR( (predict(model.tree,type="vector")[,2] >= val) , model.logistic.interaction$y )
  points( values$FPR , values$TPR, pch = 23, bg="green" ) 
  #Tree  Interactions
  values <- FPR_TPR( (predict(model.tree.interaction,type="vector")[,2] >= val) , model.logistic.interaction$y )
  points( values$FPR , values$TPR, pch = 24, bg="yellow" )
}
#Accuracy of the model
barplot(c(ACC.model.logistic.interaction, ACC.model.logistic, ACC.model.tree, ACC.model.tree.interaction), xpd=FALSE, ylim=c(.75,.85), xlab="Method", names = c("\n logistic \n interaction", "\n logistic \n", "\n classif. \n tree", "\n inter. \n tree"), ylab = "Accuracy")

#Using K-Fold Cross Validation

#Setting things up
nfold <- 10
n1 <- nrow(df1)   # the number of observations in df1 (without interactions)
n2 <- nrow(df2) # the number of observations in df2 (with interactions)
foldid1 <- rep(1:nfold,each=ceiling(n1/nfold))[sample(1:n1)]
foldid2 <- rep(1:nfold,each=ceiling(n2/nfold))[sample(1:n2)]
#Create an empty dataframe of results
OOS <- data.frame(logistic.interaction=rep(NA,nfold), logistic=rep(NA,nfold), tree=rep(NA,nfold), tree.interaction=rep(NA,nfold),null=rep(NA,nfold))

#Use a for loop to run through the nfold trails
#Begin with the df1 data
for(k in 1:nfold){ 
  train <- which(foldid1!=k)  # train on all but fold `k'
  
  #Fit the logistic, tree, and null models
  model.logistic <- glm(Attrition~., data=df1, subset=train, family="binomial")
  model.tree     <- tree(Attrition~ ., data=df1, subset=train) 
  model.nulll    <-glm(Attrition~1, data=df1, subset=train,family="binomial")
   
  #Get predictions: type=response so we have probabilities
  pred.logistic <- predict(model.logistic, newdata=df1[-train,], type="response")
  pred.tree     <- predict(model.tree, newdata=df1[-train,], type="vector")
  pred.tree     <- pred.tree[,2]
  pred.null     <- predict(model.nulll, newdata=df1[-train,], type="response")
  
  #Calculate and log R2
  #Logistic
  OOS$logistic[k] <- R2(y=df1$Attrition[-train], pred=pred.logistic, family="binomial")
  OOS$logistic[k]
  #Tree
  OOS$tree[k] <- R2(y=df1$Attrition[-train], pred=pred.tree, family="binomial")
  OOS$tree[k]
  #Null
  OOS$null[k] <- R2(y=df1$Attrition[-train], pred=pred.null, family="binomial")
  OOS$null[k]
  #Null Model guess
  sum(df1$Attrtion[train]=="Yes")/length(train)
  
  #We will loop this nfold times (I setup for 10)
  #this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

#Next for the df2 data
for(k in 1:nfold){ 
  train <- which(foldid2!=k)  # train on all but fold `k'
  
  #Fit the two logistic interaction and tree interaction models
  model.logistic.interaction <- glm(Attrition~., data=df2, subset=train, family="binomial")
  model.tree.interaction     <- tree(Attrition~ ., data=df2, subset=train) 
    
  #Get predictions: type=response so we have probabilities
  pred.logistic.interaction <- predict(model.logistic.interaction, newdata=df2[-train,], type="response")
  pred.tree.interaction     <- predict(model.tree.interaction, newdata=df2[-train,], type="vector")
  pred.tree.interaction     <- pred.tree.interaction[,2]
  
  #calculate and log R2
  #Logistic Interaction
  OOS$logistic.interaction[k] <- R2(y=df2$Attrition[-train], pred=pred.logistic.interaction, family="binomial")
  OOS$logistic.interaction[k]
  #Tree with Interaction
  OOS$tree.interaction[k] <- R2(y=df2$Attrition[-train], pred=pred.tree.interaction, family="binomial")
  OOS$tree.interaction[k]
  
  #We will loop this nfold times (I setup for 10)
  #this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}
#Compiling performance and making box chart 
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)

if (nfold >= 10){
  names(OOS)[1] <-"logistic\ninteraction"
  names(OOS)[4] <-"tree\ninterion"
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
}

#After comaparing ROC, 10-fold cross validations and OOS R2 performances, 
#we choose the logistic model
#We will run the logistic regression on df1 now for the train data
model.logistic <- glm(Attrition~., data=df1, family="binomial")
summary(model.logistic)

#We see the following variables are signifcant at the 0.05 level
sig05 <- data.frame(summary(model.logistic)$coef[summary(model.logistic)$coef[,4] < .05, 4])
sig05

#Keeping only significant variables in our dataframe
keep_df_final <- c("Attrition", "Age", "NumCompaniesWorked","DepartmentResearch.Development","JobLevel2","MaritalStatusSingle","StockOptionLevel1","BusinessTravelTravel_Frequently","JobRoleLaboratory.Technician","JobSatisfaction4","OverTimeYes")
df_final <- df1[,(names(df1) %in% keep_df_final)]

#Now we will predict the churn probabilities of the current nonchurners
#Using the regression on all employees to predict churn probabilities will be overfitting 
#Hence we slice the data at various parts so that no data point in test and train datasets overlap as explained below

#Adding an index to final dataframe so that we can match this index with the probability of churning for final deliverable
df_final$ID <- seq.int(nrow(df_final))

#Set the seed to get consistent results after randomization
set.seed(1)

#Now we are going to build train and test data
#Train data is all of the employees that have left and 88% of those that haven't left
#We come up with n=0.88 because we want the test data to be 10% = 148 of the main data
#So our train data should be 1470-148 rows = 1322 rows out of which 237 are churners
#So 1322 - 237 = 1085 nonchurners should be there out of a total of 1233 nonchurners
#So 1085/1233 = 0.88
n <- 0.88

#Our test data is going to the 12% of nonchurners = 148 people

#Diving the final dataframe into churners and nochurners
churner <- df_final[df_final$Attrition == 1, ]
nonchurner <- df_final[df_final$Attrition == 0, ]

#Randomizing the nonchurner data rows to shuffle the data
sample <- sample.int(n=nrow(nonchurner), size = floor(nrow(nonchurner)), replace = F)
random_nonchurner <- nonchurner[sample,]

rows <- nrow(random_nonchurner)

#Choosing the 148 nonchurners for every time we run regression for every subset
skip <- ceiling((1-n)*rows)

#And number of susbets should be
subsets <- rows/skip
subsets
#So we will have total 9 subsets where the lowerbound for l9 below will be the rows of random_nonchurner (=1233)

#Now we build 9 different train and test datasets

#Test1 is are the first 148 rows of nonchurners
#Train1 is the rbind of the remaining nonchurners and all churners

u1 <- 1
l1 <- skip
test1 <- random_nonchurner[u1:l1, ]
sample1 <- subset(random_nonchurner, !random_nonchurner$ID %in% test1$ID)
train1 <- rbind(churner, sample1)

#Test2 is the next set of 148 rows of nonchurners
#Train2 is the rbind of the remaining nonchurners and all churners

u2 <- 1 + skip
l2 <- skip*2
test2 <- random_nonchurner[u2:l2, ]
sample2 <- subset(random_nonchurner, !random_nonchurner$ID %in% test2$ID)
train2 <- rbind(churner, sample2)

#And we continue so on until the last subset where we reach the last data point of churners
#Breaking down in this way ensures there is no overfitting
#Since we are not training the model and testing it on the same data point

u3 <- 1 + (skip*2)
l3 <- skip*3
test3 <- random_nonchurner[u3:l3, ]
sample3 <- subset(random_nonchurner, !random_nonchurner$ID %in% test3$ID)
train3 <- rbind(churner, sample3)

u4 <- 1 + (skip*3)
l4 <- skip*4
test4 <- random_nonchurner[u4:l4, ]
sample4 <- subset(random_nonchurner, !random_nonchurner$ID %in% test4$ID)
train4 <- rbind(churner, sample4)

u5 <- 1 + (skip*4)
l5 <- skip*5
test5 <- random_nonchurner[u5:l5, ]
sample5 <- subset(random_nonchurner, !random_nonchurner$ID %in% test5$ID)
train5 <- rbind(churner, sample5)

u6 <- 1 + (skip*5)
l6 <- skip*6
test6 <- random_nonchurner[u6:l6, ]
sample6 <- subset(random_nonchurner, !random_nonchurner$ID %in% test6$ID)
train6 <- rbind(churner, sample6)

u7 <- 1 + (skip*6)
l7 <- skip*7
test7 <- random_nonchurner[u7:l7, ]
sample7 <- subset(random_nonchurner, !random_nonchurner$ID %in% test7$ID)
train7 <- rbind(churner, sample7)

u8 <- 1 + (skip*7)
l8 <- skip*8
test8 <- random_nonchurner[u8:l8, ]
sample8 <- subset(random_nonchurner, !random_nonchurner$ID %in% test8$ID)
train8 <- rbind(churner, sample8)

u9 <- 1 + (skip*8)
l9 <- rows
test9 <- random_nonchurner[u9:l9, ]
sample9 <- subset(random_nonchurner, !random_nonchurner$ID %in% test9$ID)
train9 <- rbind(churner, sample9)

#Creating the 9 regression models for the different train and test datasets created above

model.final1 <- glm(Attrition~.-ID, data=train1, family="binomial")
summary(model.final1)

model.final2 <- glm(Attrition~.-ID, data=train2, family="binomial")
summary(model.final2)

model.final3 <- glm(Attrition~.-ID, data=train3, family="binomial")
summary(model.final3)

model.final4 <- glm(Attrition~.-ID, data=train4, family="binomial")
summary(model.final4)

model.final5 <- glm(Attrition~.-ID, data=train5, family="binomial")
summary(model.final5)

model.final6 <- glm(Attrition~.-ID, data=train6, family="binomial")
summary(model.final6)

model.final7 <- glm(Attrition~.-ID, data=train7, family="binomial")
summary(model.final7)

model.final8 <- glm(Attrition~.-ID, data=train8, family="binomial")
summary(model.final8)

model.final9 <- glm(Attrition~.-ID, data=train9, family="binomial")
summary(model.final9)

#Predicting the probabilites of churning on the 9 test datasets 
#so that together they contain all nonchurned employees

pred.final1 <- predict(model.final1, newdata=test1, type="response")
predictions1 <- round(pred.final1,2)

pred.final2 <- predict(model.final2, newdata=test2, type="response")
predictions2 <- round(pred.final2,2)

pred.final3 <- predict(model.final3, newdata=test3, type="response")
predictions3 <- round(pred.final3,2)

pred.final4 <- predict(model.final4, newdata=test4, type="response")
predictions4 <- round(pred.final4,2)

pred.final5 <- predict(model.final5, newdata=test5, type="response")
predictions5 <- round(pred.final5,2)

pred.final6 <- predict(model.final6, newdata=test6, type="response")
predictions6 <- round(pred.final6,2)

pred.final7 <- predict(model.final7, newdata=test7, type="response")
predictions7 <- round(pred.final7,2)

pred.final8 <- predict(model.final8, newdata=test8, type="response")
predictions8 <- round(pred.final8,2)

pred.final9 <- predict(model.final9, newdata=test9, type="response")
predictions9 <- round(pred.final9,2)

#Adding a column of predictions in each test data
test1$Predicted_Attrition <- predictions1
test2$Predicted_Attrition <- predictions2
test3$Predicted_Attrition <- predictions3
test4$Predicted_Attrition <- predictions4
test5$Predicted_Attrition <- predictions5
test6$Predicted_Attrition <- predictions6
test7$Predicted_Attrition <- predictions7
test8$Predicted_Attrition <- predictions8
test9$Predicted_Attrition <- predictions9

#Overlaying all the predictions so that we have all nonchurned employee data together with their
#predicted attrition rates in a single dataframe
test_final <- rbind(test1, test2, test3, test4, test5, test6, test7, test8, test9)

#But of interest is only the ID and predicted attrition rates
keep_test <- c("ID", "Predicted_Attrition")
test_final <- test_final[,(names(test_final) %in% keep_test)]

#Reloading the original dataset and filtering only the nonchurners
output <- read.csv("Data.csv", header=TRUE)

#The "Age" column name is not displayed correctly so we correct it
colnames(output)[1] <- "Age"

#Creating the ID column as earlier so that we can merge the predicted attritions to the
#correct employee
output$ID <- seq.int(nrow(output))

#The final deliverable should only include the probabilities of nonchurned employees
output <- output[output$Attrition == "No", ]

#Merging the original dataset (filtered for nonchurners) and merging on their predictions
deliverable <- merge(output, test_final, by.x = "ID", by.y = "ID", all.x = TRUE)

#Dropping the created ID column because it was just for reference
drop <- c("ID")
deliverable <- deliverable[,!(names(deliverable) %in% drop)]
deliverable <- deliverable[order(-deliverable$Predicted_Attrition),]
summary(deliverable)

#Having a CSV file with original data and predicted probabilities for final deployment to IBM
write.csv(deliverable, file = "Deliverable.csv")

#The deliverable to IBM also includes the following regression equation that they
#can use to predict future churn probabilties
final_reg <- glm(Attrition~.-ID, data=df_final, family="binomial")
summary(final_reg)
