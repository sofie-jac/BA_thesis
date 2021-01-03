
#Load library
library(pacman)
pacman::p_load(tidyverse, brms, dplyr, Hmisc, ggpubr, ggplot2, ggthemes, plyr, ggpubr, cowplot, lme4, RColorBrewer, Rmisc, functools, reshape2, afex, emmeans, psych, here, lmerTest, compare, rstatix, ggExtra, gridExtra, ggeffects, cowplot, modelr, boot, stats)

#Read Data
data_sias_raw <- read_csv(here::here('./data/anxiety_data.csv'))
data_threshold <- read_csv(here::here('./data/all_threshold_data.csv'))
data_cwt_full <- read_csv(here::here('./data/CWT_results_full.csv'))
data_weibull <- read_csv(here::here('./data/threshold_slope_data.csv'))
data_weibull <- na.omit(data_weibull)

#load functions
source(here::here('functions', 'get_SIAS.R'))
source(here::here('functions', 'clean_cwt_full.R'))
source(here::here('functions', 'interaction_plot.R'))
source(here::here('functions', 'clean_weibull_estimates.R'))

#transform to sum scores
data_sias <- get_SIAS(data_sias_raw)
#merge with thresholds
data_sias <- merge(data_sias, data_threshold, by = "record_id")
#ommit NA's
data_sias <- na.omit(data_sias)
#subset to only sumscores and record_id
data_sias_only <- dplyr::select(data_sias, record_id, SIAS, Gender)
#summary data cwt long format and add cleaned SIAS scores
data_cwt_full <- clean_cwt_full(data_cwt_full) #RT shorter than 100 ms removed + where no response was given
data_cwt_full$SIAS_scale <- scale(data_cwt_full$SIAS)

#Calculate outliers for slope/threshold standard errors and estimates and remove them from dataframe
data_weibull_clean <- clean_weibull_estimates(data_weibull)
data_weibull_clean_id <- dplyr::select(data_weibull_clean, record_id)

### remove outliers - some standard errors for the thresholds were too big
data_sias <- merge(data_sias, data_weibull_clean_id)
data_cwt_full <- merge(data_cwt_full, data_weibull_clean_id)

#create aggregated data
data_cwt_agregated <- data_cwt_full %>%
  group_by(Trial_type, Valence, record_id) %>%
  dplyr::summarize(Accuracy = mean(Accuracy), Reaction_Time = mean(Reaction_Time), Confidence = mean(Confidence))
data_cwt_agregated <- merge(data_cwt_agregated, data_sias, by = "record_id")
data_cwt_agregated <- filter(data_cwt_agregated, Accuracy > 0)
data_cwt_agregated$SIAS_center <- data_cwt_agregated$SIAS - mean(data_cwt_agregated$SIAS)

#calculate cronbachs alpha
data_sias_items <- data_sias_raw %>% 
  dplyr::select(num_range("sias_", 1:20), record_id)
data_sias_items$record_id = str_remove(data_sias_items$record_id, "^0+")

data_sias_items_erlt <- merge(data_sias_items, data_cwt_agregated, by = 'record_id')
data_sias_items_erlt <- data_sias_items_erlt %>% 
  dplyr::select(num_range("sias_", 1:20))
alpha(data_sias_items_erlt, check.keys=TRUE)

data_sias_items_thresh <- merge(data_sias_items, data_sias, by = 'record_id')
data_sias_items_thresh <- data_sias_items_thresh %>% 
  dplyr::select(num_range("sias_", 1:20))
alpha(data_sias_items_thresh, check.keys=TRUE)

###plot data

# build histogram for SIAS and treshold
p <- ggplot(data_sias, aes(SIAS, Threshold)) + geom_point(color = '#1F78B4')  +
  theme_minimal(base_size = 14, base_family = "Times New Roman") + 
  scale_color_brewer(palette="Paired") + xlab('SIAS Scores') + 
  geom_smooth(method = 'lm', color = '#1F78B4')
p1 <- ggExtra::ggMarginal(p, type = "histogram", fill = '#A6CFE3', color = '#1F78B4')

#plot the interaction between Trial_type and stimulus precision for rt, accuracy and confidence
p2 <- interaction_plot_rt(data_cwt_agregated)
p3 <- interaction_plot_acc(data_cwt_full)

#bulid glmer plot
mydf <- ggpredict(m7, terms="SIAS_scale [all]")
p4 <- ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(x, predicted), color = '#A6CFE3', size = 1.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, fill = '#1F78B4') +
  scale_color_brewer(palette="Paired") +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  xlab('SIAS Scores (scaled)') +
  ylab('Predicted Accuracy')

#plot in grid
plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), label_fontfamily = "Times New Roman")

############ appendix plots
#plot historgram to show weibull function fitted correctl

p <- ggplot(data_weibull_clean, aes(Threshold_SE, Slope_SE)) + geom_point(color = '#1F78B4')  +
  theme_minimal(base_size = 16, base_family = "Times New Roman") + 
  scale_color_brewer(palette="Paired") + xlab('Threshold (SEs)')+ ylab('Slope (SEs)')
p2 <- ggExtra::ggMarginal(p, type = "histogram", fill = '#A6CFE3', color = '#1F78B4')

p <- ggplot(data_weibull, aes(Threshold_SE, Slope_SE)) + geom_point(color = '#1F78B4')  +
  theme_minimal(base_size = 16, base_family = "Times New Roman") + 
  scale_color_brewer(palette="Paired") + xlab('Threshold (SEs)')+ ylab('Slope (SEs)')
p3 <- ggExtra::ggMarginal(p, type = "histogram", fill = '#A6CFE3', color = '#1F78B4')
(max(data_weibull$Threshold_SE)-mean(data_weibull$Threshold_SE))/sd(data_weibull$Threshold_SE)
(max(data_weibull$Slope_SE)-mean(data_weibull$Slope_SE))/sd(data_weibull$Slope_SE)

plot_grid(p3, p2, labels = c('A', 'B'), label_fontfamily = "Times New Roman")

##### build mixed effects plots for valence and accuracy
plot <- ggplot(aes(x=Valence, y=Accuracy, color = record_id), data=data_cwt_full) + 
  stat_summary(fun.data="mean_cl_boot", geom='line', aes(group=record_id)) +
  coord_cartesian(ylim=c(0,1)) + theme_minimal(base_size = 14, base_family = "Times New Roman") + 
  theme(legend.position = "none") + 
  ylab("Accuracy")
plot1 <- ggplot(aes(x=Trial_type, y=Accuracy, color = record_id), data=data_cwt_full) + 
  stat_summary(fun.data="mean_cl_boot", geom='line', aes(group=record_id)) + 
  coord_cartesian(ylim=c(0,1)) + 
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(legend.position = "none") +
  ylab("Accuracy") +
  xlab("Trial Type")
grid.arrange(plot, plot1, ncol = 2)

### plot celling effect
interaction_plot_acc_stim(data_cwt_full)
########################

### modeling

#####     h1 - violating normality is not problematic for bigger datasets
m_h1 <- lmer(formula = Threshold ~  SIAS + (1|Age), data = data_sias)
isSingular(m_h1)
summary(m_h1)
describe(data_sias)

######### ANCOVA
#were previously anovas, but as results remained the same when running ancovas and the estimates did not change, they were converted to ancovas

##RT ANCOVA
ancova_rt_cwt <- aov_car(Reaction_Time ~ Trial_type*Valence*SIAS_center + Error(record_id/Trial_type*Valence), data=data_cwt_agregated, factorize = FALSE)
get_anova_table(ancova_rt_cwt, correction = c("auto", "GG", "HF", "none"))

#get p-values for post hoc tests
pairs(emmeans(ancova_rt_cwt, ~Valence), adjust="bon")
pairs(emmeans(ancova_rt_cwt, ~Trial_type), adjust="bon")

########### glmer ACC
data_cwt_full$Trial_type <- data_cwt_full$Trial_type
#make multi level model

m1 <- glmer(Accuracy ~ Trial_type * Valence * SIAS_scale + (1+Trial_type+Valence|record_id), data=data_cwt_full, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m2 <- glmer(Accuracy ~ Trial_type * Valence * SIAS_scale + (1+Valence|record_id), data=data_cwt_full, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m3 <- glmer(Accuracy ~ Trial_type * Valence * SIAS_scale + (1|record_id), data=data_cwt_full, family = 'binomial')
m4 <- glmer(Accuracy ~ Trial_type * Valence + SIAS_scale + (1+Valence|record_id), data=data_cwt_full, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m5 <- glmer(Accuracy ~ Trial_type + Valence * SIAS_scale + (1+Valence|record_id), data=data_cwt_full, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m6 <- glmer(Accuracy ~ Trial_type * SIAS_scale + Valence  + (1+Valence|record_id), data=data_cwt_full, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m7 <- glmer(Accuracy ~ Trial_type + Valence + SIAS_scale + (1+Valence|record_id), data=data_cwt_full, family = 'binomial')

anova(m1, m2) #m2 wins
anova(m2, m3) # m2 wins
anova(m2, m4) #m4 wins
anova(m4, m5) #m5 wins
anova(m5, m6) #m5 wins
anova(m5, m7) #m7 wins

summary(m7)
inv.logit(2.20315)
inv.logit(2.20315-0.13468)
inv.logit(2.20315-0.22886)
inv.logit(2.20315+0.07814)

# calculate CI's
se <- sqrt(diag(vcov(m7)))
(tab <- cbind(Est = fixef(m7), LL = fixef(m7) - 1.96 * se, UL = fixef(m7) + 1.96 *
                se))
#intercept
inv.logit(2.037224849)
inv.logit(2.36907834)
#non-predictive
inv.logit(2.037224849-0.206092358)
inv.logit(2.36907834-0.06325905)
#anti-predictive
inv.logit(2.037224849-0.304189168)
inv.logit(2.36907834-0.15353755)
#sias
inv.logit(2.037224849+0.009923037)
inv.logit(2.36907834+0.14635972)
#use inv.logit() to see effects from the model
inv.logit(0.08456+0.03828)

############# bootstrap
#With 2000 bootstrapped replicates
FUN <- function(fit) {
  fit <- glmer(Accuracy ~ Trial_type + Valence + SIAS_scale + (1+Valence|record_id), data=data_cwt_full, family = 'binomial')
  return(fixef(fit))
}
m7_boot_2000_real <- bootMer(m7, FUN, nsim = 2000, .progress = "txt")
summary(m7_boot_2000)
