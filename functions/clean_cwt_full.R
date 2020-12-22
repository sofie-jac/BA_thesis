clean_cwt_full <- function(data_cwt_full){
  data_cwt_full <- rename(data_cwt_full, replace = c("sID_vec" = "record_id"))
  data_cwt_full <- rename(data_cwt_full, replace = c("trialN" = "trial"))
  data_cwt_full <- rename(data_cwt_full, replace = c("noiseLevel" = "Ambiguity"))
  data_cwt_full$Ambiguity <- revalue(data_cwt_full$Ambiguity, c("Low_Noise"="Low", "High_Noise"="High"))
  data_cwt_full <- rename(data_cwt_full, replace = c("predictionLevel" = "Cue_congruency"))
  data_cwt_full$Cue_congruency <- revalue(data_cwt_full$Cue_congruency, c("pred"="Predictive", "nonpred"="Non-predictive", "antipred"="Anti-Predictive"))
  data_cwt_full <- rename(data_cwt_full, replace = c("stim" = "Valence"))
  data_cwt_full$Valence <- as.factor(data_cwt_full$Valence)
  data_cwt_full$Valence <- revalue(data_cwt_full$Valence, c("0"="Angry", "1"="Happy"))
  data_cwt_full$response <- as.factor(data_cwt_full$response)
  data_cwt_full$response <- revalue(data_cwt_full$response, c("0"="Angry", "1"="Happy"))
  data_cwt_full <- rename(data_cwt_full, replace = c("ACC" = "Accuracy"))
  data_cwt_full <- rename(data_cwt_full, replace = c("RT" = "Reaction_Time"))
  data_cwt_full <- rename(data_cwt_full, replace = c("conf" = "Confidence"))
  
  data_cwt_full$Cue_congruency <- as.factor(data_cwt_full$Cue_congruency)
  data_cwt_full$Ambiguity <- as.factor(data_cwt_full$Ambiguity)
  data_cwt_full$record_id <- as.factor(data_cwt_full$record_id)
  data_cwt_full$Cue_congruency<- factor(data_cwt_full$Cue_congruency, levels=rev(levels(data_cwt_full$Cue_congruency)))
  data_cwt_full$Ambiguity <- factor(data_cwt_full$Ambiguity, levels=rev(levels(data_cwt_full$Ambiguity)))
  data_cwt_full <- dplyr::select(data_cwt_full, -c(confRT))
  data_cwt_full <- merge(data_cwt_full, data_sias, by = "record_id")
  
  #remove outliers
  data_cwt_full <- filter(data_cwt_full, response == "Angry"|response == "Happy")
  data_cwt_full <- filter(data_cwt_full, Reaction_Time >= 0.1)
  data_cwt_full <- filter(data_cwt_full, Confidence <= 100)
}
