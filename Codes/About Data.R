library(tidyverse)


Data <- read.csv("D:\\NFHS\\NFHS5\\Weight_Predication\\weight_adult[23-06-2025].csv")

summary(Data)

Data_1 <- Data %>% select(statename,district,type_of_residence,gender,age,religion,community,
                          educational_level,currently_working,occupation,occupation_grouped,wealthindex,
                          smokes_cigarettes,smoke_bidis,drink_alcohol,diabetes,hypertension,
                          waistcurcumference,hipcurcumference,armcircumference,
                          measurement_reason,height, weight,bmi)
table(Data_1$measurement_reason,useNA = "ifany")
table(is.na(Data_1$bmi))
table(is.na(Data_1$weight))
table(is.na(Data_1$height))
## Data Cleaning 
Data_1$weight <- ifelse(!is.na(Data_1$bmi),Data_1$weight,NA)
Data_1$height <- ifelse(!is.na(Data_1$bmi),Data_1$height,NA)
Data_1$bmi <- ifelse(!is.na(Data_1$weight) & !is.na(Data_1$height),Data_1$bmi,NA)


