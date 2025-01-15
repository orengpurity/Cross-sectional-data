###Load the data

h1 <- read.csv("C:/Users/Purity/Downloads/clean_data1.csv")

p_load(tidyverse)
p_load(arsenal)
#library(readxl)
#install.packages("officer")
library(officer)
#install.packages("nutrition")
#library(nutrition)
library(dplyr)
library(janitor)
library(pacman)
view (h1)
table (h1$mother_occptn)
table (h1$mother_occptn, h1$farming_systems)
table (h1$farming_systems, h1$mother_occptn)
purity <- data.frame(h1$mother_occptn, h1$farming_systems)
write.csv(purity, "C:/Users/Purity/Downloads/purity")

######################################################################################### #EXTRACT NECESSARY VARIABLES FOR THIS PAPER

h1.1 <- subset(h1, select = c("household_id", "child_id", "child_sex", "farming_systems", "educational_hhead", "mother_occptn", "educational_mother", "hhpple"	, "hh_occptn", "hh_land_ownrshp.x", "hhincome", "savingac" , "mother_status"	, "ASF_mother", "MDD_W", "ASF_child", "MDD_C", "lack_food", "knowledge_score", "attitude_score", "practices_score", "child_age_months", "organ_meat_mthr", "flesh_meats_mthr", "eggs_mthr", "fish_seafood_mthr", "milk_products_mthr", "organ_meat_ch", "flesh_meats_ch", "eggs_ch", "fish_seafood_ch", "milk_products_ch", "milk_mother_ml", "milk_child_ml", "cattle_number", "goat_number", "sheep_number", "chicken_number", "gps_coordinates.x"))
view (h1.1)
table(h1.1$gps_coordinates.x)

###################################################################################
##Separate GPS coordinates into individual lines

h1.2 <- h1.1 |>
  separate(gps_coordinates.x ,c("Latitude", "Longitude", "Altitude", "Accuracy"), sep=" ")
view(h1.2)
table(h1.2$Latitude)

######################################################################################################################################################################
### Recode Data: 
#knowledge_score"        
h1.2$knowledge_score <- ifelse(h1.2$knowledge_score<=5, "Poor", ifelse(h1.2$knowledge_score>5 & h1.2$knowledge_score <=7, "Fair", ifelse(h1.2$knowledge_score >7, "Good", NA)))
view(h1.2$knowledge_score)
#check
table(h1.2$knowledge_score)

#Order
h1.2$knowledge_score <- factor(h1.2$knowledge_score, levels=c("Poor", "Fair", "Good"))

#check
table(h1.2$knowledge_score)

#### attitude_score" 

h1.2$attitude_score <- ifelse(h1.2$attitude_score<=5, "Poor", ifelse(h1.2$attitude_score>5 & h1.2$attitude_score <=7, "Fair", ifelse(h1.2$attitude_score >7, "Good", NA)))

#check
table(h1.2$attitude_score)

#Order
h1.2$attitude_score <- factor(h1.2$attitude_score, levels=c("Poor", "Fair", "Good"))

#check
table(h1.2$attitude_score)

#practices_score
h1.2$practices_score <- ifelse(h1.2$practices_score<=5, "Poor", ifelse(h1.2$practices_score>5 & h1.2$practices_score <=7, "Fair", ifelse(h1.2$practices_score >7, "Good", NA)))

#check
table(h1.2$practices_score)

#Order
h1.2$practices_score <- factor(h1.2$practices_score, levels=c("Poor", "Fair", "Good"))

#check
table(h1.2$practices_score)

#################################################################################### Got to your data
view(h1.2)
names(h1.2)

# What is the challenge?

h1.3 <- h1.2 %>%
  mutate(across(23:32, ~ recode(.x, "0" = "No", "1" = "Yes")))

###################################################################################
h1.3$child_age_months <- ifelse(h1.3$child_age_months <=5, "0-5", ifelse(h1.3$child_age_months >5 & h1.3$child_age_months<=23, "6-23", ifelse(h1.3$child_age_months >23 & h1.3$child_age_months <=35, "24-35", ifelse(h1.3$child_age_months >35 & h1.3$child_age_months <=48, "36-48", "Over48"))))


#Order
h1.3$child_age_months <- factor(h1.3$child_age_months, levels=c("0-5", "6-23", "24-35", "36-48", "Over48"))

###Savings account
h1.3$savingac <- ifelse(h1.3$savingac == "0", "No", ifelse(h1.3$savingac == "2", "No", ifelse(h1.3$savingac == "1", "Yes", NA)))

####Education
#Household Head 

h1.3$educational_hhead <- ifelse(h1.3$educational_hhead == "Adult Education", "None", ifelse(h1.3$educational_hhead == "None", "None", ifelse(h1.3$educational_hhead == "Pre-School", "None", ifelse(h1.3$educational_hhead == "Primary", "Primary", ifelse(h1.3$educational_hhead == "Secondary", "Secondary", ifelse(h1.3$educational_hhead == "Technical", "Tertiary", ifelse(h1.3$educational_hhead == "Tertiary", "Tertiary", ifelse(h1.3$educational_hhead == "Tertiary Not Completed", "Tertiary", NA))))))))

table(h1.3$educational_hhead)

##Mother

h1.3$educational_mother <- ifelse(h1.3$educational_mother == "Adult Education", "None", ifelse(h1.3$educational_mother == "None", "None", ifelse(h1.3$educational_mother == "Pre-School", "None", ifelse(h1.3$educational_mother == "Primary", "Primary", ifelse(h1.3$educational_mother == "Secondary", "Secondary", ifelse(h1.3$educational_mother == "Technical", "Tertiary", ifelse(h1.3$educational_mother == "Tertiary", "Tertiary", ifelse(h1.3$educational_mother == "Tertiary Not Completed", "Tertiary", NA))))))))

table(h1.3$educational_mother)

###Creat a different dataset for further manipulation

h1.4 <- h1.3

h1.5 <- subset(h1.4, select = c("household_id", "child_id", "child_sex", "child_age_months","farming_systems","educational_hhead","mother_occptn", "educational_mother", "hh_occptn","hhpple", "hh_land_ownrshp.x", "hhincome", "savingac", "mother_status", "ASF_mother", "MDD_W", "ASF_child", "MDD_C", "lack_food", "knowledge_score", "attitude_score", "practices_score", "organ_meat_mthr", "flesh_meats_mthr", "eggs_mthr", "fish_seafood_mthr", "milk_products_mthr", "organ_meat_ch", "flesh_meats_ch", "eggs_ch", "fish_seafood_ch", "milk_products_ch", "cattle_number", "goat_number", "sheep_number", "chicken_number", "Latitude", "Longitude", "Altitude", "Accuracy"))

####################################################################################################################################################################

####We are only interested in kids over the age of 6 months and under the age of 48 months

h1.5x <- subset(h1.5, child_age_months %in% c("6-23",  "24-35",  "36-48"))

###########################################################################################
table2_data <- h1.5x |>
  pivot_longer(c("MDD_W", "MDD_C"), names_to="MDD_name", values_to = "MDD") |>
  pivot_longer(c("ASF_mother", "ASF_child" ), names_to = "ASF_name", values_to="ASF")


table2 <-tableby(MDD_name~MDD+ASF, data=table2_data)
summary(table2, text = T)
###############################################################################################RESULTS

h1.5x$child_age_months <- as.numeric(h1.5x$child_age_months)
data <- h1.5x$child_age_months
result <- t.test(data)

mean(h1.5x$child_age_months)

# Extract the confidence interval
confidence_interval <- result$conf.int

# Print the confidence interval
confidence_interval

###No of pple in a household
data2 <- h1.5x$hhpple

result2 <- t.test(data2)

mean(h1.5x$hhpple)

# Extract the confidence interval
confidence_interval1 <- result2$conf.int

# Print the confidence interval
confidence_interval1
###############################################
#Cattle number
data3 <- h1.5$cattle_number
result3 <- t.test(data3)
mean(h1.5$cattle_number)
median(h1.5$cattle_number)
range(h1.5$cattle_number)


##Sheep
median(h1.5$sheep_number)
range(h1.5$sheep_number)

#Goats
median(h1.5$goat_number)
range(h1.5$goat_number)


#Chicken
median(h1.5$chicken_number)
range(h1.5$chicken_number)

##Income
median(h1.5$hhincome)
range(h1.5$hhincome)

###################################
##Create table
h1.6 <- subset(h1.5x, select = c("ASF_child","farming_systems", "child_sex", "child_age_months", "mother_occptn", "educational_mother", "hhpple"	,  "hh_land_ownrshp.x", "hhincome", "savingac" , "mother_status","lack_food", "knowledge_score", "attitude_score", "practices_score",  "cattle_number", "goat_number", "sheep_number", "chicken_number"))

h1.6$farming_systems <- factor(h1.6$farming_systems, levels = c("Pastoral", "Agro-Pastoral", "Mixed Farming"))

h1.6$knowledge_score <- factor(h1.6$knowledge_score, levels = c("Poor", "Fair", "Good"))
h1.6$attitude_score <- factor(h1.6$attitude_score, levels = c("Poor", "Fair", "Good"))
h1.6$practices_score <- factor(h1.6$practices_score, levels = c("Poor", "Fair", "Good"))


h1.6$mother_occptn <- factor(h1.6$mother_occptn, levels = c("Livestock Herding","Crop production", "Salaried Worker", "Petty Trading", "Fishing",  "Student", "Other")) 

require(knitr)

table1 <-tableby(farming_systems~child_sex+child_age_months+mother_occptn+educational_mother+hhpple+hh_land_ownrshp.x+hhincome+savingac+mother_status+lack_food+knowledge_score+attitude_score+practices_score+cattle_number+goat_number+sheep_number+chicken_number ,data= h1.6)

summary(table1, text = T)

#####################################################################################################################################################################################Livestock numbers 
h1.6a <- subset(h1.5x, select = c("farming_systems", "cattle_number", "goat_number", "sheep_number", "chicken_number",  "hhincome", "savingac"))

h1.6a$farming_systems <- factor(h1.6a$farming_systems, levels = c("Pastoral", "Agro-Pastoral", "Mixed Farming"))
########################################################################################################################################################################################


table1.1 <-tableby(farming_systems~cattle_number+goat_number+sheep_number+chicken_number+hhincome+savingac,data= h1.6a)

summary(table1.1, text = T)

##############################################################################################################################################################################
##median
#Cattle
median(h1.6a$cattle_number [h1.6a$farming_systems=="Pastoral"])
#18
range(h1.6a$cattle_number [h1.6a$farming_systems=="Pastoral"])
#0 528
median(h1.6a$cattle_number [h1.6a$farming_systems=="Agro-Pastoral"])
#24
range(h1.6a$cattle_number [h1.6a$farming_systems=="Agro-Pastoral"])
# 0 400
median(h1.6a$cattle_number [h1.6a$farming_systems=="Mixed Farming"])
#24

###########################################################################################
#Goat

median(h1.6a$goat_number [h1.6a$farming_systems=="Pastoral"])

range(h1.6a$goat_number [h1.6a$farming_systems=="Pastoral"])

median(h1.6a$goat_number [h1.6a$farming_systems=="Agro-Pastoral"])

range(h1.6a$goat_number [h1.6a$farming_systems=="Agro-Pastoral"])

median(h1.6a$goat_number [h1.6a$farming_systems=="Mixed Farming"])

range(h1.6a$goat_number [h1.6a$farming_systems=="Mixed Farming"])
###########################################################################################
#Sheep
median(h1.6a$sheep_number [h1.6a$farming_systems=="Pastoral"])

range(h1.6a$sheep_number [h1.6a$farming_systems=="Pastoral"])

median(h1.6a$sheep_number [h1.6a$farming_systems=="Agro-Pastoral"])

range(h1.6a$sheep_number [h1.6a$farming_systems=="Agro-Pastoral"])

median(h1.6a$sheep_number [h1.6a$farming_systems=="Mixed Farming"])

range(h1.6a$sheep_number [h1.6a$farming_systems=="Mixed Farming"])

###########################################################################################

#Chicken
median(h1.6a$chicken_number [h1.6a$farming_systems=="Pastoral"])

range(h1.6a$chicken_number [h1.6a$farming_systems=="Pastoral"])

median(h1.6a$chicken_number [h1.6a$farming_systems=="Agro-Pastoral"])

range(h1.6a$chicken_number [h1.6a$farming_systems=="Agro-Pastoral"])

median(h1.6a$chicken_number [h1.6a$farming_systems=="Mixed Farming"])

range(h1.6a$chicken_number [h1.6a$farming_systems=="Mixed Farming"])

############################################################################################
d1 <- h1.5x$hhpple[h1.5x$farming_systems=="Pastoral"]
mean(h1.5x$hhpple[h1.5x$farming_systems=="Pastoral"])
r1 <- t.test(d1)
conf1 <- r1$conf.int
conf1


d2 <- (h1.5x$hhpple[h1.5x$farming_systems=="Agro-Pastoral"])
mean(h1.5x$hhpple[h1.5x$farming_systems=="Agro-Pastoral"])
r2 <- t.test(d2)
conf2 <- r2$conf.int
conf2

d3 <- (h1.5x$hhpple[h1.5x$farming_systems=="Mixed Farming"])
mean(h1.5x$hhpple[h1.5x$farming_systems=="Mixed Farming"])
r3 <- t.test(d3)
conf3 <- r3$conf.int
conf3


###Household Income

median(h1.5a$hhincome[h1.5a$farming_systems=="Pastoral"])
range(h1.5a$hhincome[h1.5a$farming_systems=="Pastoral"])

median(h1.5a$hhincome[h1.5a$farming_systems=="Agro-Pastoral"])
range(h1.5a$hhincome[h1.5a$farming_systems=="Agro-Pastoral"])

median(h1.5a$hhincome[h1.5a$farming_systems=="Mixed Farming"])
range(h1.5a$hhincome[h1.5a$farming_systems=="Mixed Farming"])


######################################################################################
#Animal Source Food Consumption
require(knitr)

h1.7 <- subset(h1.5x, select = c("farming_systems" , "ASF_mother", "MDD_W", "ASF_child", "MDD_C", "lack_food","hhincome", "savingac" , "knowledge_score", "attitude_score", "practices_score"))

table1.2 <-tableby(farming_systems~ASF_mother+MDD_W+ASF_child+MDD_C+lack_food+hhincome+savingac+knowledge_score+attitude_score+practices_score, data= h1.7)

summary(table1.2, text = T)

##################################################################################
table1.3 <- tableby(farming_systems~organ_meat_mthr+flesh_meats_mthr+eggs_mthr+fish_seafood_mthr+milk_products_mthr+organ_meat_ch+flesh_meats_ch+eggs_ch+fish_seafood_ch+milk_products_ch, data = h1.8)

summary(table1.3, text = T)

#####################################################################################Household characteristics
#######################################################################################################################################################################################
##Associations

#We use data 1.5 which has all observations.
#Outcome = ASF_Child

#Organize the data
#converting chr variables to as.factor in preparation for glm fitting in the univariate analysis

##Remember
h1.5 <- subset(h1.4, child_age_months %in% c("6-23",  "24-35",  "36-48"))
################################################################################
library(forcats)
h1.5.glm <- h1.5x |>
  mutate(child_sex = factor(child_sex, levels = c("Male", "Female"))) |>
  mutate(farming_systems = factor(farming_systems, levels = c("Pastoral","Agro-Pastoral", "Mixed Farming"))) |>
  mutate(educational_hhead = factor(educational_hhead, levels = c("None", "Primary", "Secondary", "Tertiary"))) |>
  mutate(mother_occptn = factor(mother_occptn, levels = c("Livestock Herding", "Crop production", "Livestock Marketing", "Salaried Worker", "Petty Trading", "Student", "Fishing", "Other"))) |>
  mutate(educational_mother = factor(educational_mother, levels= c("None", "Secondary", "Primary", "Tertiary"))) |>
  mutate(hh_occptn = factor(hh_occptn, levels= c("Pastoralist", "Mixed Farmer","Business person", "Employed full time", "Employed Part time", "Other"))) |>
  mutate(mother_status, factor(mother_status, levels = c("Not pregant and not lactating", "Lactating", "Pregnant"))) |>
  mutate(ASF_mother = factor(ASF_mother, levels = c("Did not Consume ASF", "Consumed ASF")))|> 
  mutate(knowledge_score = factor (knowledge_score, levels= c("Poor", "Fair", "Good"))) |>
  mutate(attitude_score = factor (attitude_score, levels= c("Poor", "Fair", "Good"))) |>
  mutate(practices_score = factor (practices_score, levels= c("Poor", "Fair", "Good"))) |>
  mutate(child_age_months = factor (child_age_months, levels= c("6-23", "24-35", "36-48")))

h1.5.glm

##################################################################################
h1.5.glm$ASF_child <- factor(h1.5.glm$ASF_child, levels = c("Did not consume ASF", "Consumed ASF"))
table(h1.5.glm$ASF_child)

h1.5.glm$ASF_mother <- factor(h1.5.glm$ASF_mother, levels = c("Did not Consume ASF", "Consumed ASF"))
table(h1.5.glm$ASF_mother)

####################
h1.5x <- subset(h1.5, child_age_months %in% c("6-23",  "24-35",  "36-48"))

##################################################################################

model1 <- glm(ASF_child ~ farming_systems, data = h1.5.glm, family = binomial())
model2 <- glm(ASF_child ~ child_sex, data = h1.5.glm, family = binomial())
model3<- glm(ASF_child ~ educational_hhead, data = h1.5.glm, family = binomial())
model4<- glm(ASF_child ~ mother_occptn, data = h1.5.glm, family = binomial())
model5<- glm(ASF_child ~ educational_mother, data = h1.5.glm, family = binomial())
model6<- glm(ASF_child ~ lack_food, data = h1.5.glm, family = binomial())
model7<- glm(ASF_child ~ hhpple, data = h1.5.glm, family = binomial())
model8<- glm(ASF_child ~ hh_occptn, data = h1.5.glm, family = binomial())
model9<- glm(ASF_child ~ hh_land_ownrshp.x, data = h1.5.glm, family = binomial())
model10<- glm(ASF_child ~ hhincome, data = h1.5.glm, family = binomial())
model11<- glm(ASF_child ~ savingac, data = h1.5.glm, family = binomial())
model12<- glm(ASF_child ~ mother_status, data = h1.5.glm, family = binomial())
model13<- glm(ASF_child ~ ASF_mother, data = h1.5.glm, family = binomial())
model14<- glm(ASF_child ~ MDD_W, data = h1.5.glm, family = binomial())

model15<- glm(ASF_child ~ lack_food, data = h1.5.glm, family = binomial())
model16<- glm(ASF_child ~ knowledge_score, data = h1.5.glm, family = binomial())
model17<- glm(ASF_child ~ attitude_score, data = h1.5.glm, family = binomial())
model18<- glm(ASF_child ~ practices_score, data = h1.5.glm, family = binomial())
#model19<- glm(ASF_child ~ child_age_months, data = h1.5.glm, family = binomial())

model20<- glm(ASF_child ~ cattle_number, data = h1.5.glm, family = binomial())
model21<- glm(ASF_child ~ goat_number, data = h1.5.glm, family = binomial())
model22<- glm(ASF_child ~ sheep_number, data = h1.5.glm, family = binomial())
model23<- glm(ASF_child ~ chicken_number, data = h1.5.glm, family = binomial())

# Calculate AIC and compare model1 to model37 and compare their AIC values
models <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12, model13, model14, model16, model17, model18, model20, model21, model22, model23)
# Calculate AIC for each model
AIC_values <- sapply(models, AIC)
# Identify the model with the lowest AIC
best_model_index <- which.min(AIC_values)
best_model <- models[[best_model_index]]
best_model

##################################################################################
#Univariate using h1.5x subset

uni.h1.5x <- subset(h1.5a, select = c("ASF_child" , "MDD_C", "ASF_mother", "MDD_W", "child_sex", "child_age_months", "farming_systems", "hh_occptn","educational_hhead", "mother_occptn", "educational_mother", "hhpple", "hh_land_ownrshp.x", "hhincome", "savingac", "mother_status", "lack_food", "knowledge_score", "attitude_score", "practices_score", "cattle_number", "goat_number", "sheep_number", "chicken_number"))

uni.h1.5a_2 <- subset(uni.h1.5a, child_age_months %in% c("6-23",  "24-35",  "36-48"))
library(arsenal)
table_uni.h1.5a_2 <- tableby(ASF_child~MDD_C+ASF_mother+MDD_W+child_sex+child_age_months+farming_systems+hh_occptn+educational_hhead+mother_occptn+educational_mother+hhpple+hh_land_ownrshp.x+hhincome+savingac+mother_status+lack_food+knowledge_score+attitude_score+practices_score+cattle_number+goat_number+sheep_number+chicken_number, data = uni.h1.5x)


table_uni.h1.5a_2 <- tableby(ASF_child~MDD_C+ASF_mother+MDD_W+child_sex+child_age_months+farming_systems+hh_occptn+educational_hhead+mother_occptn+educational_mother+hhpple+hh_land_ownrshp.x+hhincome+savingac+mother_status+lack_food+knowledge_score+attitude_score+practices_score+cattle_number+goat_number+sheep_number+chicken_number, data = uni.h1.5x_2, numeric.stats=c("median","q1q3"), numeric.test="kwt")

summary(table_uni.h1.5a_2, text = T, digits=1, digits.p=3, digits.pct=1, numeric.simplify=TRUE, numeric.stats=c("meansd"))



####################################################################################Reorganize some of the data  - reference variables
library(dplyr)
hill <- subset(h1.5.glm, select = c("household_id","ASF_child", "child_sex", "farming_systems", "educational_hhead", "mother_occptn", "educational_mother", "hhpple", "hh_occptn", "hh_land_ownrshp.x", "hhincome", "savingac", "mother_status", "ASF_mother", "MDD_W","MDD_C", "lack_food", "knowledge_score", "attitude_score", "practices_score", "child_age_months", "cattle_number", "goat_number", "sheep_number", "chicken_number"))

hill.1 <- hill |>
  mutate(child_sex = factor(child_sex, levels = c("Male", "Female"))) |>
  mutate(farming_systems = factor(farming_systems, levels = c("Pastoral","Agro-Pastoral", "Mixed Farming"))) |>
  mutate(MDD_C = factor(MDD_C, levels = c("Not met MDD", "Met MDD"))) |>
  mutate(educational_hhead = factor(educational_hhead, levels = c("None", "Primary", "Secondary", "Tertiary"))) |>
  mutate(mother_occptn = factor(mother_occptn, levels = c("Livestock Herding", "Crop production", "Livestock Marketing", "Salaried Worker", "Petty Trading", "Student", "Fishing", "Other"))) |>
  mutate(educational_mother = factor(educational_mother, levels= c("None", "Secondary", "Primary", "Tertiary"))) |>
  mutate(hh_occptn = factor(hh_occptn, levels= c("Pastoralist", "Mixed Farmer","Business person", "Employed full time", "Employed Part time", "Other"))) |>
  mutate(mother_status = factor(mother_status, levels = c("Not pregant and not lactating", "Lactating", "Pregnant"))) |>
  mutate(ASF_mother = factor(ASF_mother, levels = c("Did not Consume ASF", "Consumed ASF")))|> 
  mutate(knowledge_score = factor (knowledge_score, levels= c("Poor", "Fair", "Good"))) |>
  mutate(attitude_score = factor (attitude_score, levels= c("Poor", "Fair", "Good"))) |>
  mutate(practices_score = factor (practices_score, levels= c("Poor", "Fair", "Good"))) |>
  mutate(child_age_months = factor (child_age_months, levels= c("6-23", "24-35", "36-48")))

############################################################################

hcomp <- hill.1

hcomp2 <- hcomp |>
  mutate(child_sex = factor(child_sex, levels = c("Male", "Female"))) |>
  mutate(hh_land_ownrshp.x = factor(hh_land_ownrshp.x, levels = c("No", "Yes"))) |>
  mutate(savingac = factor(savingac, levels = c("No", "Yes"))) |>
  mutate(MDD_W = factor(MDD_W, levels = c("Not met MDD", "Met MDD")))


hcomp2$lack_food <- factor(hcomp2$lack_food, levels = c("Lacked Food", "Food Secure"))



##################################################################################
final.modx.1 <- glm(ASF_child~child_sex+farming_systems+educational_hhead+hhincome+savingac+mother_status+ASF_mother+lack_food+cattle_number+goat_number+sheep_number+chicken_number, data = hcomp2, family = binomial)
summary(final.modx.1)

final.modx.1
backward = step(final.modx,trace=0)

final.modx.2 <- 
  
formula
formula(backward)
install.packages(jtools)
library(jtools)

summ(backward, confint = TRUE, exp = TRUE, digits = 3)

install.packages("questionr")
summary(final.modx)
exp(coef(final.modx))
library(questionr)
odds.ratio(final.modx)

exp(coefficients())