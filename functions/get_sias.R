get_SIAS <- function(data){
  # create scores and set the classes right for further analysis
  scores <- data
  scores$age <- as.integer(scores$age)
  scores$gender <- as.factor(scores$gender)
  
  #change gender into factor with names
  scores$gender<- revalue(scores$gender, c("1"="Female", "2"="Male", "3"="Other"))
  
  #Reversescore SIAS
  columnsToReverse <- c('sias_5', 'sias_9', 'sias_11')
  scores[ ,columnsToReverse] = 4 - scores[ ,columnsToReverse]
  #Calculate SIAS
  scores <- scores %>% 
    dplyr::select(num_range("sias_", 1:20),"record_id", "age", "gender") %>%
    mutate(SIAS = rowSums(.[1:20])) %>%
    drop_na()
  
  #remove un-needed variables
  scores <- dplyr::select(scores, record_id, gender, age, SIAS)
  
  #calculate above threshold inidividuals
  scores$SIAS_above_clin <- ifelse(scores$SIAS > 42, 1, 0) #a score of 1 means diagnosis
  
  #remove 0's from record_id
  scores$record_id = str_remove(scores$record_id, "^0+")
  
  #Change names of variables
  scores <- rename(scores, c("age" = "Age", "gender" = "Gender"))
  return(scores)
}
