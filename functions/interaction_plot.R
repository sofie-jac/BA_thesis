normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  # Remove this subject mean column
  data$subjMean <- NULL
  return(data)
}

summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

interaction_plot_rt <- function(data) {
  #normalize RT and confidence intervals based on standards for repeated measures
  data_sum_rt <- summarySEwithin(data=data, measurevar="Reaction_Time", betweenvars=NULL, withinvars=c("Cue_congruency", "Valence"), idvar="record_id", na.rm=TRUE, conf.interval=.95, .drop=TRUE)

  #RT plot
  p1 <- ggplot(data_sum_rt, aes(x=Cue_congruency, y=Reaction_Time, group = Valence, color=Valence)) + 
    geom_errorbar(aes(ymin=Reaction_Time-ci, ymax=Reaction_Time+ci), width=.1) +
    geom_line() + geom_point()+
    scale_color_brewer(palette="Paired") +
    theme_minimal(base_size = 14, base_family = "Times New Roman") +
    xlab('Cue Congruency') +
    ylab('Reaction Time') +
    labs(color='Valence') 

  return(p1)
}

interaction_plot_acc <- function(data) {
  
  #normalize RT and confidence intervals based on standards for repeated measures
  data_sum_acc <- summarySEwithin(data=data, measurevar="Accuracy", betweenvars=NULL, withinvars=c("Cue_congruency", "Valence"), idvar="record_id", na.rm=TRUE, conf.interval=.95, .drop=TRUE)
  
  #Acc plot
  p2 <- ggplot(data_sum_acc, aes(x=Cue_congruency, y=Accuracy, group = Valence, color=Valence)) + 
    geom_errorbar(aes(ymin=Accuracy-ci, ymax=Accuracy+ci), width=.1) +
    geom_line() + geom_point()+
    scale_color_brewer(palette="Paired") +
    theme_minimal(base_size = 14, base_family = "Times New Roman") +
    xlab('Cue Congruency') +
    ylab('Accuracy') +
    labs(color='Valence') 
  p2
  
  return(p2)
}
