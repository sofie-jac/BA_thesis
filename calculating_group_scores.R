#calculate group means
data_cwt_agregated_angry <- data_cwt_agregated %>% 
  filter(Valence == "Angry")
#reaction time
mean(data_cwt_agregated_angry$Reaction_Time)
sd(data_cwt_agregated_angry$Reaction_Time)
#accuracy
mean(data_cwt_agregated_angry$Accuracy)
sd(data_cwt_agregated_angry$Accuracy)
#confidence
mean(data_cwt_agregated_angry$Confidence)
sd(data_cwt_agregated_angry$Confidence)

#calculate group means
data_cwt_agregated_happy <- data_cwt_agregated %>% 
  filter(Valence == "Happy")
#reaction time
mean(data_cwt_agregated_happy$Reaction_Time)
sd(data_cwt_agregated_happy$Reaction_Time)
#accuracy
mean(data_cwt_agregated_happy$Accuracy)
sd(data_cwt_agregated_happy$Accuracy)
#confidence
mean(data_cwt_agregated_happy$Confidence)
sd(data_cwt_agregated_happy$Confidence)

#calculate group means
data_cwt_agregated_pred <- data_cwt_agregated %>% 
  filter(Trial_type == "Predictive")
#reaction time
mean(data_cwt_agregated_pred$Reaction_Time)
sd(data_cwt_agregated_pred$Reaction_Time)
#accuracy
mean(data_cwt_agregated_pred$Accuracy)
sd(data_cwt_agregated_pred$Accuracy)
#confidence
mean(data_cwt_agregated_pred$Confidence)
sd(data_cwt_agregated_pred$Confidence)

data_cwt_agregated_non_pred <- data_cwt_agregated %>% 
  filter(Trial_type == "Non-predictive")
#reaction time
mean(data_cwt_agregated_non_pred$Reaction_Time)
sd(data_cwt_agregated_non_pred$Reaction_Time)
#accuracy
mean(data_cwt_agregated_non_pred$Accuracy)
sd(data_cwt_agregated_non_pred$Accuracy)
#confidence
mean(data_cwt_agregated_non_pred$Confidence)
sd(data_cwt_agregated_non_pred$Confidence)

data_cwt_agregated_anti_pred <- data_cwt_agregated %>% 
  filter(Trial_type == "Anti-Predictive")
#reaction time
mean(data_cwt_agregated_anti_pred$Reaction_Time)
sd(data_cwt_agregated_anti_pred$Reaction_Time)
#accuracy
mean(data_cwt_agregated_anti_pred$Accuracy)
sd(data_cwt_agregated_anti_pred$Accuracy)
#confidence
mean(data_cwt_agregated_anti_pred$Confidence)
sd(data_cwt_agregated_anti_pred$Confidence)

mean(data_cwt_agregated$SIAS_center)
sd(data_cwt_agregated$SIAS_center)

#############
#hypothesis testing

mean(data_sias$SIAS)
sd(data_sias$SIAS)
mean(data_sias$Threshold, na.rm = TRUE)
sd(data_sias$Threshold, na.rm = TRUE)
mean(data_sias$Age, na.rm = TRUE)
sd(data_sias$Age, na.rm = TRUE)

############
#getting age ranges
summary(data_sias$Age)
sd(data_sias$Age)

summary(data_cwt_agregated$Age)
sd(data_cwt_agregated$Age)

test <- merge(data_cwt_agregated, data_sias, by = "record_id")
min(test$Age)
max(test$Age)
mean(test$Age)
sd(test$Age)

#get gender distribution
summary(data_cwt_agregated$Gender)
summary(data_sias$Gender)

#get sias scores 
summary(data_sias$SIAS)
sd(data_sias$SIAS)
summary(data_cwt_agregated$SIAS)
sd(data_cwt_agregated$SIAS)

#### statistics table
sum_full <- select(data_cwt_full, record_id, Age, Gender, Accuracy, SIAS, SIAS_scale)
sum_full$predictive <- ifelse(data_cwt_full$Trial_type == "Predictive", 1, 0)
sum_full$non_predictive <- ifelse(data_cwt_full$Trial_type == "Non-predictive", 1, 0)
sum_full$anti_predictive <- ifelse(data_cwt_full$Trial_type == "Anti-Predictive", 1, 0)
sum_full$angry <- ifelse(data_cwt_full$Valence == "Angry", 1, 0)
sum_full$female <- ifelse(data_cwt_full$Gender == "Female", 1, 0)
sum_full$male <- ifelse(data_cwt_full$Gender == "Male", 1, 0)
sum_full$other <- ifelse(data_cwt_full$Gender == "Other", 1, 0)
describe(sum_full)
sum(sum_full$angry)

sum_thresh <- select(data_sias, record_id, Age, Gender, SIAS, Threshold)
describe(sum_thresh)

sum_agr <- select(data_cwt_agregated, record_id, Age, Gender, Accuracy, SIAS_center, Reaction_Time)
sum_agr$predictive <- ifelse(data_cwt_agregated$Trial_type == "Predictive", 1, 0)
sum_agr$non_predictive <- ifelse(data_cwt_agregated$Trial_type == "Non-predictive", 1, 0)
sum_agr$anti_predictive <- ifelse(data_cwt_agregated$Trial_type == "Anti-Predictive", 1, 0)
sum_agr$angry <- ifelse(data_cwt_agregated$Valence == "Angry", 1, 0)
sum_agr$female <- ifelse(data_cwt_agregated$Gender == "Female", 1, 0)
sum_agr$male <- ifelse(data_cwt_agregated$Gender == "Male", 1, 0)
sum_agr$other <- ifelse(data_cwt_agregated$Gender == "Other", 1, 0)
describe(sum_agr)
sum(sum_agr$angry)
data_cwt_agregated
summary(data_cwt_agregated$Gender)
sum(sum_agr$male)/6
