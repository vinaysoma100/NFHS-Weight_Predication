library(haven)
library(tidyverse)
library(readxl)

#************************************************* Adults ************************************************************#
#NFHS5
Household <- read_dta("D:\\NFHS\\NFHS5\\IAPR7EDT Household Member Recode\\IAPR7EFL.DTA")
Household$shdist <- as.character(Household$shdist)


Districts <- read_xlsx("D:\\NFHS\\NFHS5\\NFHS_District_Occupation.xlsx",sheet = "NFHS_District")
Districts$`District number` <- as.character(Districts$`District number`)
Household <- Household %>% left_join(Districts, by = c("shdist" = "District number"))

#Women
Women_req_cols <-c("hv024","shdist","district","hv001","hv002","hvidx","hv104","hv025","sh47", "sh49","hv270","ha1","ha2","ha3","ha40","sh305","sh306","shb15","ha13","hv117")
Women_req <- Household %>% select(all_of(Women_req_cols))

#Men
Men_req_cols <- c("hv024","shdist","district","hv001","hv002","hvidx","hv104","hv025","sh47", "sh49","hv270","hb1","hb2","hb3","hb40","sh305","sh306","shb15","hb13","hv118")
Men_req <- Household %>% select(all_of(Men_req_cols))


#Rename the variables
Women_req <- Women_req %>% rename("state" = hv024,
                                  "cluster_number" = hv001,
                                  "household_number" = hv002,
                                  "line_number" = hvidx,
                                  "Gender" = hv104,
                                  "type_of_residence" = hv025,
                                  "religion" =  sh47,
                                  "community" = sh49,
                                  "wealthindex" = hv270,
                                  "age" = ha1,
                                  "weight" = ha2,
                                  "height" = ha3,
                                  "bmi" =  ha40,
                                  "waistcurcumference" = sh305,
                                  "hipcurcumference" = sh306,
                                  "armcircumference" = shb15,
                                  "measurement_reason"= ha13,
                                  "eligibility" = hv117)


#Rename the variables
Men_req <- Men_req %>% rename("state" = hv024,
                              "cluster_number" = hv001,
                              "household_number" = hv002,
                              "line_number" = hvidx,
                              "Gender" = hv104,
                              "type_of_residence" = hv025,
                              "religion" =  sh47,
                              "community" = sh49,
                              "wealthindex" = hv270,
                              "age" = hb1,
                              "weight" = hb2,
                              "height" = hb3,
                              "bmi" =  hb40,
                              "waistcurcumference" = sh305,
                              "hipcurcumference" = sh306,
                              "armcircumference" = shb15,
                              "measurement_reason" = hb13,
                              "eligibility" = hv118
)


#combine
Household_req <- rbind(Men_req,Women_req)


#********  State
Household_req <- Household_req %>%
  mutate(statename = case_when(
    state == 1  ~ "Jammu & Kashmir",
    state == 2  ~ "Himachal Pradesh",
    state == 3  ~ "Punjab",
    state == 4  ~ "Chandigarh",
    state == 5  ~ "Uttarakhand",
    state == 6  ~ "Haryana",
    state == 7  ~ "NCT of Delhi",
    state == 8  ~ "Rajasthan",
    state == 9  ~ "Uttar Pradesh",
    state == 10 ~ "Bihar",
    state == 11 ~ "Sikkim",
    state == 12 ~ "Arunachal Pradesh",
    state == 13 ~ "Nagaland",
    state == 14 ~ "Manipur",
    state == 15 ~ "Mizoram",
    state == 16 ~ "Tripura",
    state == 17 ~ "Meghalaya",
    state == 18 ~ "Assam",
    state == 19 ~ "West Bengal",
    state == 20 ~ "Jharkhand",
    state == 21 ~ "Odisha",
    state == 22 ~ "Chhattisgarh",
    state == 23 ~ "Madhya Pradesh",
    state == 24 ~ "Gujarat",
    state == 25 ~ "Dadra & Nagar Haveli and Daman & Diu",
    state == 27 ~ "Maharashtra",
    state == 28 ~ "Andhra Pradesh",
    state == 29 ~ "Karnataka",
    state == 30 ~ "Goa",
    state == 31 ~ "Lakshadweep",
    state == 32 ~ "Kerala",
    state == 33 ~ "Tamil Nadu",
    state == 34 ~ "Puducherry",
    state == 35 ~ "Andaman & Nicobar Islands",
    state == 36 ~ "Telangana",
    state == 37 ~ "Ladakh",
    TRUE        ~ NA_character_
  ))

#********  Gender
Household_req <- Household_req %>%
  mutate(gender = case_when(
    Gender == 1  ~ "Male",
    Gender == 2  ~ "Female",
    Gender == 3  ~ "Transgender",
    TRUE        ~ NA_character_))

table(Household_req$Gender)


#********  Typeofresidence
Household_req <- Household_req %>%
  mutate(type_of_residence = case_when(
    type_of_residence == 1  ~ "Urban",
    type_of_residence == 2  ~ "Rural",
    TRUE        ~ NA_character_))
table(Household_req$type_of_residence,useNA = "ifany")

#********  Religion
Household_req <- Household_req %>%
  mutate(religion = case_when(
    religion == 1 ~ "Hindu",
    religion ==2 ~ "Muslim",
    religion ==3 ~ "Christian",
    religion ==4 ~"Sikh",
    religion ==5 ~ "Buddhist / Neo-Buddhist",
    religion ==6 ~ "Jain",
    religion ==7 ~ "Jewish",
    religion ==8 ~ "Parsi / Zoroastrian",
    religion ==9 ~ "No religion",
    religion == 96 ~ "Other",
    TRUE        ~ NA_character_))

table(Household_req$religion,useNA = "ifany")

#********  Community
Household_req <- Household_req %>%
  mutate(community = case_when(
    community == 1 ~ "Scheduled caste",
    community == 2 ~ "Scheduled tribe",
    community == 3 ~ "Other backward class",
    community == 4 ~ "None of them",
    community == 8 ~ "Don't know",
    TRUE        ~ NA_character_))


table(Household_req$community,useNA = "ifany")
#********  wealthindex
Household_req <- Household_req %>%
  mutate(wealthindex = case_when(
    wealthindex == 1 ~ "Poorest",
    wealthindex == 2 ~ "Poorer",
    wealthindex == 3 ~ "Middle",
    wealthindex == 4 ~ "Richer",
    wealthindex == 5 ~ "Richest",
    TRUE        ~ NA_character_))


table(Household_req$wealthindex,useNA = "ifany")

#********  "Eligibility for  interview"
Household_req <- Household_req %>%
  mutate(eligibility = case_when(
    eligibility == 0 ~ "Not eligible",
    eligibility == 1 ~  "Eligible",
        TRUE        ~ NA_character_))

table(Household_req$eligibility,useNA = "ifany")

#********  Result of measurement - height/weight"
Household_req <- Household_req %>%
  mutate(measurement_reason = case_when(
    measurement_reason == 0 ~ "Measured",
    measurement_reason == 3 ~  "Not present",
    measurement_reason == 4 ~  "Refused",
    measurement_reason == 6 ~  "Other",
    TRUE        ~ NA_character_))

table(Household_req$measurement_reason,useNA = "ifany")



#Drop the age not available
Household_req <- Household_req %>% drop_na(age)

#drop not eligible
Household_req <- Household_req %>%
  filter(eligibility != "Not eligible")

#******** weight cleaning
# 9994 "Not present"
# 9995 "Refused"
# 9996 "Other"
Household_req$weight <- ifelse(Household_req$weight %in% c(9994,9995,9996) , NA, Household_req$weight)
Household_req$weight <- round(Household_req$weight/10,1)
summary(Household_req$weight)

#******** height cleaning
# 9994 "Not present"
# 9995 "Refused"
# 9996 "Other"
Household_req$height <- ifelse(Household_req$height %in% c(9994,9995,9996) , NA, Household_req$height)
Household_req$height <- round(Household_req$height/10,1)
summary(Household_req$height)


#******** BMI cleaning
# 9998 "Flagged cases"
Household_req$bmi <- ifelse(Household_req$bmi == 9998 , NA, Household_req$bmi)
Household_req$bmi <- round(Household_req$bmi/100,2)
summary(Household_req$bmi)


#Select variables
Household_req_f <- Household_req %>% select(cluster_number,household_number,line_number,statename,district,type_of_residence,gender,age,
                                            religion,community,wealthindex,eligibility,
                                            weight,height,bmi,measurement_reason,waistcurcumference,hipcurcumference,armcircumference)


Men <-read_dta("D:\\NFHS\\NFHS5\\IAMR7EDT Men Recode\\IAMR7EFL.DTA",col_select = c("mv001","mv002","mv003","mv106","mv714","mv716","mv717","sm604","sm607","sm619","sm627a","sm627b"))
Women <- read_dta("D:/NFHS/NFHS5/IAIR7EDT Individual Recode/IAIR7EFL.DTA", col_select = c("v001","v002","v003","v106","v714","v716","v717","v463a","s708","s720","s728a","s728b"))

#Rename the variables
Men <- Men %>% rename("cluster_number" = mv001,
                          "household_number"   = mv002,
                          "line_number"  = mv003,
                          "educational_level" = mv106,
                          "currently_working" = mv714,
                          "occupation_code" =  mv716,
                          "occupation_grouped" = mv717,
                          "smokes_cigarettes" = sm604,
                          "smoke_bidis" = sm607,
                          "drink_alcohol" = sm619,
                          "diabetes" = sm627a,
                          "hypertension" =  sm627b)


#Rename the variables
Women <- Women %>% rename("cluster_number" = v001,
                             "household_number"   = v002,
                             "line_number"  = v003,
                             "educational_level" = v106,
                             "currently_working" = v714,
                             "occupation_code" =  v716,
                             "occupation_grouped" = v717,
                             "smokes_cigarettes" = v463a,
                             "smoke_bidis" = s708,
                             "drink_alcohol" = s720,
                             "diabetes" = s728a,
                             "hypertension" =  s728b)




Men_Women <- bind_rows(Men,Women)

occupation <- read_xlsx("D:\\NFHS\\NFHS5\\NFHS_District_Occupation.xlsx",sheet = "NFHS_Occupation")

Men_Women <- Men_Women %>% left_join(occupation, by = c("occupation_code" ))



#********  educational_level
Men_Women <- Men_Women %>%
  mutate(educational_level = case_when(
    educational_level == 0 ~ "No education",
    educational_level == 1 ~ "Primary",
    educational_level == 2 ~ "Secondary",
    educational_level == 3 ~ "Higher",
    TRUE        ~ NA_character_))


#********  currently_working
Men_Women <- Men_Women %>%
  mutate(currently_working = case_when(
    currently_working == 0 ~ "No",
    currently_working == 1 ~ "Yes",
    TRUE        ~ NA_character_))

#********  occupation_grouped

Men_Women <- Men_Women %>%
  mutate(occupation_grouped = case_when(
    occupation_grouped == 0  ~ "Not working",
    occupation_grouped == 1  ~ "Professional / technical / managerial",
    occupation_grouped == 3  ~ "Clerical",
    occupation_grouped == 4  ~ "Sales",
    occupation_grouped == 5  ~ "Services / household and domestic",
    occupation_grouped == 6  ~ "Agricultural",
    occupation_grouped == 7  ~ "Skilled and unskilled manual",
    occupation_grouped == 9  ~ "Other",
    occupation_grouped == 98 ~ "Don't know",
    TRUE                     ~ NA_character_
  ))





#********  smokes_cigarettes
Men_Women <- Men_Women %>%
  mutate(smokes_cigarettes = case_when(
    smokes_cigarettes == 0 ~ "No",
    smokes_cigarettes == 1 ~ "Yes",
    TRUE        ~ NA_character_))

#********  smoke_bidis
Men_Women <- Men_Women %>%
  mutate(smoke_bidis = case_when(
    smoke_bidis == 1 ~ "Every day",
    smoke_bidis == 2 ~ "Some days",
    smoke_bidis == 3 ~ "Not at all",
    TRUE        ~ NA_character_))


#********  drink_alcohol
Men_Women <- Men_Women %>%
  mutate(drink_alcohol = case_when(
    drink_alcohol == 0 ~ "No",
    drink_alcohol == 1 ~ "Yes",
    TRUE        ~ NA_character_))

#********  diabetes
Men_Women <- Men_Women %>%
  mutate(diabetes = case_when(
    diabetes == 0 ~ "No",
    diabetes == 1 ~ "Yes",
    diabetes == 8 ~ "Don't know",
    TRUE        ~ NA_character_))

#********  hypertension
Men_Women <- Men_Women %>%
  mutate(hypertension = case_when(
    hypertension == 0 ~ "No",
    hypertension == 1 ~ "Yes",
    hypertension == 8 ~ "Don't know",
    TRUE        ~ NA_character_))


Men_Women <- Men_Women %>% select(cluster_number,household_number,line_number,educational_level,currently_working,occupation,everything(),-occupation_code)
Household_req_f1 <- Household_req_f %>% left_join(Men_Women, by = c("cluster_number","household_number","line_number"))



write.csv(Household_req_f1,"D:\\NFHS\\NFHS5\\weight_adult[23-06-2025].csv")













