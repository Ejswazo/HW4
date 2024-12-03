setwd("/cloud/project")

sleepdata <- read.csv("Sleep_health_and_lifestyle_dataset.csv", header = TRUE)

attach(sleepdata)
#attaches category names so you dont have to rename them when referencing 
#categrories
names(sleepdata)
#names all main catgeories on graph

shapiro.test(Sleep.Duration)
#If P value is lower than .05 then the data set is normal for shapiro test
#Could be an indication that skeep duration could be different in different 
#groups

hist(Sleep.Duration)
#Perhaps sleep duration is varied depending on gender.

is.character(Blood.Pressure)

sleepdata$systolic = substr(Blood.Pressure, 1, 3) 
#creates a subset for first 3 numbers in data set , makes category "systolic"
sleepdata$systolic = as.numeric(sleepdata$systolic)

sleepdata$diastolic = substr(Blood.Pressure, 5, 6) 

sleepdata$diastolic = as.numeric(sleepdata$diastolic)

install.packages("leaps")
library(leaps)

output <- regsubsets(Sleep.Duration ~ Gender + Age + Occupation + 
                       Quality.of.Sleep + Physical.Activity.Level +
                       Stress.Level + BMI.Category + Heart.Rate + Daily.Steps +
                       Sleep.Disorder + systolic + diastolic, data=sleepdata, 
                     nvmax=12)
#Runs models in the background to determine which catgeory is a good indicator 
#of quality.

summOut1 <- summary(output)

summOut1

n1 <- length(Sleep.Duration)

n1

p1 <- apply(summOut1$which, 1, sum)
#creates plot which determines which category is the best predictor.

p1

aic1 <- summOut1$bic - log(n1) * p1 + 2 * p1

plot(p1, aic1, ylab = "AIC1")
#Indicates which of the categories is the best indicator of quality according to 
#The AIC, The best model is the one that has all 12 predictors


model1 <- lm(Sleep.Duration ~ Gender + Age + Occupation + Quality.of.Sleep + 
               Physical.Activity.Level +
               Stress.Level + BMI.Category + Heart.Rate + Daily.Steps +
               Sleep.Disorder + systolic + diastolic, data=sleepdata)
#Storing all results for the predictor as model1 


summary(model1)
#Gender, nor occupation ma is not important when deciding sleep duration.

table(Occupation)

table(BMI.Category)

table(Sleep.Disorder)

