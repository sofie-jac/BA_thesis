#checking assumptions on manipulation checks

#source that you can plot residuals to ensure the family is correct: p. 391: http://www.utstat.toronto.edu/~brunner/oldclass/2201s11/readings/glmbook.pdf
pacman::p_load(car, rstatix, gvlma, lme4, dplyr, tidyverse, corrplot, lattice, arm, languageR, boot,GGally, reshape2, compiler, parallel, RobPer, cbpp)

######################### h1 model testing
#source: http://r-statistics.co/Assumptions-of-Linear-Regression.html
#source that you can plot residuals to ensure the family is correct: p. 391: http://www.utstat.toronto.edu/~brunner/oldclass/2201s11/readings/glmbook.pdf

#normality for Threshold is met
ggqqplot(scale(data_sias$Threshold), color = '#1F78B4') + labs( y = "Density", x = 'Threshold') +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#Linearity - seems linear
ggscatter(
  data_sias, x = "SIAS", y = "Threshold",
  short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9, color = '#A6CFE3')+
  ylab("Threshold")+
  xlab("SIAS (centered)")+
  ggtitle("Linearity") + 
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#identifying outliers - only 1 extreme
outliers_thresh <- data_sias %>%
  group_by(SIAS) %>%
  identify_outliers(Threshold)
sum(outliers_thresh$is.extreme)

#remove outliers - method used when testing if there is a difference between the final model and this one
Q <- quantile(data_sias$Threshold, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data_sias$Threshold)
eliminated_thresh<- subset(data_sias, data_sias$Threshold > (Q[1] - 1.5*iqr) & data_sias$Threshold < (Q[2]+1.5*iqr))

#ensure Homoscedasticity of residuals - we see an unequal distribution, which is not good
plot(m_h1, resid(., scaled=TRUE) ~ fitted(.), abline = 0, xlab = "Fitted Values", ylab = "Scaled Residuals")

#residuals are close to 0
mean(residuals(m_h1))

#variability within predictor is positive
var(data_sias$SIAS) 

#normality of residuals suggeted by plot no. 2
ggdensity(scale(residuals(m_h1)), color = '#1F78B4') + labs( y = 'Density', x = "Standardized residuals") +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#predictor and residuals are not correlated
cor.test(residuals(m_h1), data_sias$SIAS, method = "pearson", na.rm = T)

######################################## Ancova assumptions
############# testing ANCOVA for RT
data_cwt_agregated$Cue_congruency <- data_cwt_agregated$Cue_congruency

### Test results were the same without the covarriate
m_acc_cwt <- aov_car(Accuracy ~ Cue_congruency * Valence + Error(record_id/Cue_congruency*Valence), data=data_cwt_agregated)
summary(m_acc_cwt)
get_anova_table(m_acc_cwt, correction = c("auto", "GG", "HF", "none"))

#Linearity - seems linear
ggscatter(
  data_cwt_agregated, x = "SIAS_center", y = "Reaction_Time",
  facet.by  = c("Cue_congruency", "Valence"), 
  short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9, color = '#A6CFE3')+
  ylab("Reaction time")+
  xlab("SIAS (centered)")+
  ggtitle("Linearity") + 
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#homogenity of regession slopes - assumption is not violated
ggscatter(
  data_cwt_agregated, x = "SIAS_center", y = "Reaction_Time",
  color = "Cue_congruency", add = "reg.line",
  xlab = "SIAS (centered)", ylab = "Reaction time") +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  scale_color_brewer(palette="Blues")+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = "Cue_congruency")
  )
ggscatter(
  data_cwt_agregated, x = "SIAS_center", y = "Reaction_Time",
  color = "Valence", add = "reg.line",
  xlab = "SIAS (centered)", ylab = "Reaction time")+
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  scale_color_brewer(palette="Paired")+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Valence)
  )

#identifying outliers - only 12 extreme
outliers_rt <- data_cwt_agregated %>%
  group_by(Cue_congruency, Valence, SIAS) %>%
  identify_outliers(Reaction_Time)
sum(outliers_rt$is.extreme)

#remove outliers - method used when testing if there is a difference between the final model and this one
Q <- quantile(data_cwt_agregated$Reaction_Time, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data_cwt_agregated$Reaction_Time)
eliminated_rt<- subset(data_cwt_agregated, data_cwt_agregated$Reaction_Time > (Q[1] - 1.5*iqr) & data_cwt_agregated$Reaction_Time < (Q[2]+1.5*iqr))
ancova_rt <- aov_car(Reaction_Time ~ Cue_congruency*Valence*SIAS_center + Error(record_id/Cue_congruency*Valence), data=eliminated_rt, factorize = FALSE)
get_anova_table(ancova_rt, correction = c("auto", "GG", "HF", "none"))

#checking normality - great
ggqqplot(data_cwt_agregated, "Reaction_Time", ggtheme = theme_minimal(base_size = 14, base_family = "Times New Roman"), color = '#1F78B4') +
  facet_grid(Cue_congruency ~ Valence) 

#No perfect multicollinearity
temp <- dplyr::select(data_cwt_agregated, Accuracy, Cue_congruency, Valence, SIAS_center, record_id)
temp$Cue_congruency <- as.numeric(temp$Cue_congruency)
temp$Valence <- as.numeric(temp$Valence)
temp$record_id <- as.numeric(temp$record_id)
cor(temp, method = "spearman")

#homogenity of variance in residuals - we see no pattern, which suggests it's good
plot(scale(ancova_rt_cwt$aov$record_id$residuals), ylab = 'Subject ID residuals (scaled)', xlab = 'Index for Reaction Time')
plot(scale(ancova_rt_cwt$aov$`record_id:Cue_congruency`$residuals), ylab = 'Subject ID : Cue Congruency residuals (scaled)', xlab = 'Index for Reaction Time')
plot(scale(ancova_rt_cwt$aov$`record_id:Valence`$residuals), ylab = 'Subject ID : Valence residuals (scaled)', xlab = 'Index for Reaction Time')
plot(scale(ancova_rt_cwt$aov$`record_id:Cue_congruency:Valence`$residuals), ylab = 'Cue Congruency * Valence * Subject ID residuals (scaled)', xlab = 'Index for Reaction Time')

#residuals are close to 0
mean(ancova_rt_cwt$aov$record_id$residuals)
mean(ancova_rt_cwt$aov$`record_id:Cue_congruency`$residuals)
mean(ancova_rt_cwt$aov$`record_id:Valence`$residuals)
mean(ancova_rt_cwt$aov$`record_id:Cue_congruency:Valence`$residuals)
ancova_resid <- ancova_rt_cwt$aov$record_id$residuals + ancova_rt_cwt$aov$`record_id:Cue_congruency`$residuals

#variability within predictor is positive
var(data_cwt_agregated$SIAS_center) 

#normality of residuals
pp_1 <-ggdensity(scale(ancova_rt_cwt$aov$record_id$residuals), color = '#1F78B4') + labs( y = "Density", x = 'Standardized residuals for Subject ID') +
  theme_minimal(base_size = 12, base_family = "Times New Roman")
pp_2 <- ggdensity(scale(ancova_rt_cwt$aov$`record_id:Cue_congruency`$residuals), color = '#1F78B4') + labs( y = "Density", x = 'Standardized residuals for Subject ID : Cue Accuracy') +
  theme_minimal(base_size = 12, base_family = "Times New Roman")
pp_3 <- ggdensity(scale(ancova_rt_cwt$aov$`record_id:Valence`$residuals), color = '#1F78B4') + labs( y = "Density", x = 'Standardized residuals for Subject ID : Valence') +
  theme_minimal(base_size = 12, base_family = "Times New Roman")
pp_4 <- ggdensity(scale(ancova_rt_cwt$aov$`record_id:Cue_congruency:Valence`$residuals), color = '#1F78B4') + labs( y = "Density", x = 'Standardized residuals for Subject ID : Cue Accuracy : Valence') +
  theme_minimal(base_size = 12, base_family = "Times New Roman")
grid.arrange(pp_1, pp_2, pp_3, pp_4, ncol = 2)

#covariate and residuals are not correlated
t.test(ancova_rt_cwt$aov$record_id$residuals, data_cwt_agregated$SIAS_center)
t.test(ancova_rt_cwt$aov$`record_id:Cue_congruency`$residuals, data_cwt_agregated$SIAS_center)
t.test(ancova_rt_cwt$aov$`record_id:Valence`$residuals, data_cwt_agregated$SIAS_center)
t.test(ancova_rt_cwt$aov$`record_id:Cue_congruency:Valence`$residuals, data_cwt_agregated$SIAS_center)



###########################
#check linearity
ggscatter(
  data_cwt_full, x = "SIAS_scale", y = "Accuracy",
  facet.by  = c("Cue_congruency", "Valence"), 
  short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9, color = '#A6CFE3')+
  ylab("Accuracy")+
  xlab("SIAS (scaled and centered)")+
  ggtitle("Linearity") + 
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#No perfect multicollinearity - values are below 2
temp <- select(data_cwt_full, Accuracy, Cue_congruency, Valence, SIAS_scale, record_id)
temp$Cue_congruency <- as.numeric(temp$Cue_congruency)
temp$Valence <- as.numeric(temp$Valence)
temp$record_id <- as.numeric(temp$record_id)
cor(temp, method = "spearman")

#residuals are close to 0
mean(residuals(m7))

#variability within continious predictor is positive
var(data_cwt_full$SIAS_scale)

#covariate and residuals are not correlated
cor.test(residuals(m7), data_cwt_full$SIAS_scale, method = "kendal")

#checking that the random effects distribution is somewhat normal:
pairscor.fnc(ranef(m7)$record_id)

#chekcing residuals
binnedplot(predict(m7), resid(Acc_glmer))

#proving that the model improves by adding predictors
Acc_glmer_mixed <- glmer(Accuracy ~ 1 + (1+Valence|record_id), data=data_cwt_full, family = 'binomial')
anova(m7, Acc_glmer_mixed)

#
ggplot(data_cwt_full, aes(Cue_congruency, Accuracy)) + geom_count() + theme_minimal(base_size = 14, base_family = "Times New Roman")+ facet_wrap(~Valence) + xlab('Cue Congruency')







car:::summary.boot(m7_boot)
#m7_boot<-bootMer(x=m7,FUN=,nsim=1000, use.u = FALSE, type="parametric")


#Bootstrapped CI for the intercept
boot.ci(m7_boot,type="perc",index=1)




