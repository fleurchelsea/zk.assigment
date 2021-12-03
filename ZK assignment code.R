# Load data
sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

# Check data
summary(sample_1)
str(sample_1)

#Load packages
library(psych)
library(lm.beta)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(lmtest)
library(sandwich)
library(car)
library(caret)
library(leaps)

# DO NOT LOAD THIS ONE NOW (you will need it later when running backwards regression)
library(MASS)

# Custom Functions provided in class

error_plotter <- function(mod, col = "black", x_var = NULL){	
  mod_vars = as.character(mod$call[2])	
  data = as.data.frame(eval(parse(text = as.character(mod$call[3]))))	
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)	
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))	
  
  data$pred = predict(mod)	
  
  if(x == "1" & is.null(x_var)){x = "response_ID"	
  data$response_ID = 1:nrow(data)} else if(x == "1"){x = x_var}	
  
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)	
  abline(mod)	
  
  for(i in 1:nrow(data)){	
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))	
    abline(v = data[i,x], lty = 2, col = col)	
  }	
  
}	

# Coefficients table function from Zoltan
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

# More checks on the data
sample_1 %>% 
  summary()

describe(sample_1)

# Some plots to check for outliers
ggplot(sample_1, aes(x = pain))+
  geom_histogram()

ggplot(sample_1, aes(x = sex, y = pain))+
  geom_boxplot()

sample_1 %>%
  mutate(rownum = row.names(sample_1)) %>%
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) +
  geom_point() + geom_text()

sample_1 %>%
  mutate(rownum = row.names(sample_1)) %>%
  ggplot() + aes(x = pain_cat, y = pain, label = rownum) +
  geom_point() + geom_text()

sample_1 %>%
  mutate(rownum = row.names(sample_1)) %>%
  ggplot() + aes(x = cortisol_serum, y = pain, label = rownum) +
  geom_point() + geom_text()

sample_1 %>%
  mutate(rownum = row.names(sample_1)) %>%
  ggplot() + aes(x = cortisol_saliva, y = pain, label = rownum) +
  geom_point() + geom_text()

sample_1 %>%
  mutate(rownum = row.names(sample_1)) %>%
  ggplot() + aes(x = mindfulness, y = pain, label = rownum) +
  geom_point() + geom_text()

# Scatterplot for age and pain
sample_1 %>%
  ggplot() + aes(x = age, y = pain) + geom_point()

# Distribution plots
ggplot(sample_1, aes(x = pain))+
  geom_density()

ggplot(sample_1, aes(x = pain))+
  geom_histogram()+
  facet_wrap(~sex)

# Identify outlier in PAIN
# **NOTE: YOU CANNOT RUN THIS WITH BOTH MASS & DPLYR PACKAGES AT THE SAME TIME**
sample_1 %>% 
  filter(pain > 10) %>% 
  select(ID, pain, sex)

# New data set without outlier (ID 88)
sample1_no_out <- sample_1 %>%
  filter (pain < 10)
sample1_no_out

# Identify outlier in STAI
sample1_no_out %>%
  mutate(rownum = row.names(sample1_no_out)) %>%
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) +
  geom_label()

sample1_no_out %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() +
  geom_smooth(method = "lm")
  
sample1_no_out %>% 
  filter(STAI_trait < 20) %>% 
  select(ID, pain, sex, age, STAI_trait)

# Remove STAI outlier (ID 34) -update same data set-
sample1_no_out <- sample1_no_out %>%
  filter (STAI_trait > 20)
sample1_no_out

# Turn sex into a factor
sample1_no_out = sample1_no_out %>%
  mutate(sex = factor(sex))

# Plots with reduced data
ggplot(sample1_no_out, aes(x = sex, y = pain))+
  geom_boxplot()

ggplot(sample1_no_out, aes(x = STAI_trait, y = pain))+
  geom_point()

# Creating model 1
model_1 <- lm(pain ~ age + sex, data = sample1_no_out)
model_1

# Build MODEL 2
model_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = sample1_no_out)
model_2

# Model comparison
# Model 1 summary
summary(model_1)
coef_table(model_1)

# Check variance explained by each model (spoiler alert, you won't need model 2)
summary(model_1)$adj.r.squared
summary(model_2)$adj.r.squared

# Checking fitness of model through AIC
AIC(model_1)
AIC(model_2)

# Checking residual error through ANOVA since predictors used in model 1 are 
# a subset of the predictors used in model 2.
anova(model_1, model_2) 

# other unnecessary ANOVA I ran that is probably not necessary
mod_mean <- lm(pain ~ 1, data = sample1_no_out)
anova(mod_mean, model_1)

# Confidence interval model 1
confint(model_1)

# std betas model 1
lm.beta(model_1)

# Coefficients regression line
sample1_no_out %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() +
  geom_smooth(method = "lm")

# Linearity line
model_2 %>%
  residualPlots()

# Linearity model 1
model_1 %>%
  residualPlots()

# Model 2 appears to be better, checking data, model diagnoctics again and assumptions
model_2 %>%
  plot(which = 5)

model_2 %>%
  plot(which = 4)

sample1_no_out %>%
  slice(c(46, 73, 85))

# Checking NORMALITY ----  QQ plot
model_2 %>%
  plot(which = 2)

# Histogram
residuals_model_2 = enframe(residuals(model_2))
residuals_model_2 %>%
  ggplot() + aes(x = value) + geom_histogram()

# Checking skew and kurtosis
describe(residuals(model_2))

# Checking LINEARITY
model_2 %>%
  residualPlots()

# Checking HOMOSCEDASTICITY
model_2 %>%
  plot(which = 3)

# NCVtest
model_2 %>%
  ncvTest()

# Breush-Pagan test
model_2 %>%
  bptest() 

#Checking No MULTICOLLINEARITY
model_2 %>% 
  vif()

# Checking correlation because of the issue with Multicollinearity
sample1_no_out %>%
  select(pain, cortisol_serum, cortisol_saliva) %>%
  pairs.panels(col = "red", lm = T)

# Trying to fix issue with MULTICOLLINEARITY by centering variables
sample1_no_out = sample1_no_out %>%
  mutate(cortisol_serum_c = cortisol_serum - mean(cortisol_serum), cortisol_saliva_c = cortisol_saliva -
           mean(cortisol_saliva))
model_3 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum_c * cortisol_saliva_c, data = sample1_no_out)

model_3 %>%
  vif()

# Rebuilding a 4th model without cortisol saliva as centering the variables did not help
model_4 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = sample1_no_out)

# Checking multicollinearity for model 4
model_4 %>%
  vif()

# Redoing checks for model 4
model_4 %>%
  plot(which = 5)

model_4 %>%
  plot(which = 4)

sample1_no_out %>%
  slice(c(46, 64, 85))

# Checking NORMALITY ----  QQ plot
model_4 %>%
  plot(which = 2)

# Histogram
residuals_model_4 = enframe(residuals(model_4))
residuals_model_4 %>%
  ggplot() + aes(x = value) + geom_histogram()

# Checking skew and kurtosis
describe(residuals(model_4))

# Checking LINEARITY
model_4 %>%
  residualPlots()

# Checking HOMOSCEDASTICITY
model_4 %>%
  plot(which = 3)

# NCVtest
model_4 %>%
  ncvTest()

# Breush-Pagan test
model_4 %>%
  bptest() 

# Checking No MULTICOLLINEARITY -again- just for keeping the structure
model_4 %>% 
  vif()

# Model summary
model.summary = summary(model_4)
model.summary
AIC(model_4)
confint(model_4)
lm.beta(model_4)
anova(model_1, model_4)

# Model comparison (preferably check the assumptions first this time around)
# Model 1 summary
summary(model_1)

# Check variance explained by each model
summary(model_1)$adj.r.squared
summary(model_4)$adj.r.squared
summary(model_2)$adj.r.squared

# Checking fitness of model through AIC
AIC(model_1)
AIC(model_4)
AIC(model_2)

# Checking residual error through ANOVA
anova(model_1, model_4) 
anova(model_2, model_4)

# Confidence interval models
confint(model_1)
confint(model_4)

# std betas models
confint(model_1)
confint(model_4)

# Coefficients regression line
sample1_no_out %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() +
  geom_smooth(method = "lm")

# Linearity line
model_4 %>%
  residualPlots()

# Linearity models
model_4 %>%
  residualPlots()

model_2 %>% 
  residualPlots()

model_1 %>% 
  residualPlots()


#Assignment part 2, new reversed model
#new data to be used LATER
sample_2 = read.csv("https://tinyurl.com/87v6emky")
summary(sample_2)
sample_2 = sample_2 %>%
  mutate(sex = factor(sex))

#Check new variables (first sample set)
sample1_no_out %>%
  mutate(rownum = row.names(sample1_no_out)) %>%
  ggplot() + aes(x = weight, y = pain, label = rownum) +
  geom_point() + geom_text()

sample1_no_out %>%
  mutate(rownum = row.names(sample1_no_out)) %>%
  ggplot() + aes(x = IQ, y = pain, label = rownum) +
  geom_point() + geom_text()

sample1_no_out %>%
  mutate(rownum = row.names(sample1_no_out)) %>%
  ggplot() + aes(x = household_income, y = pain, label = rownum) +
  geom_point() + geom_text()

# create initial "full" model
initial.model <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = sample1_no_out)
initial.model

# Identify outliers with cook distance, but there is nothing wrong with them

# Data checks and Model diagnostics for new "full" model
initial.model %>%
  plot(which = 5)

initial.model %>%
  plot(which = 4)

sample1_no_out %>%
  slice(c(46, 84, 85))

#Checking NORMALITY ----  QQ plot
initial.model %>%
  plot(which = 2)

#Histogram
residuals_initial_model = enframe(residuals(initial.model))
residuals_initial_model %>%
  ggplot() + aes(x = value) + geom_histogram()

#Checking skew and kurtosis
describe(residuals(initial.model))

#Checking LINEARITY
initial.model %>%
  residualPlots()

#Checking HOMOSCEDASTICITY
initial.model %>%
  plot(which = 3)

#NCVtest
initial.model %>%
  ncvTest()

# Breush-Pagan test
initial.model %>%
  bptest() 

#Checking No MULTICOLLINEARITY 
initial.model %>% 
  vif()

# Create a reversed model (remember to activate MASS)
library(MASS)
backward.model <- stepAIC(initial.model, direction = "backward", 
                          trace = FALSE)
backward.model
summary(backward.model)

# Remember to uncheck MASS package so that it doesn't cause any issues later on

# Old model as new object
theory_based_model<- model_4
theory_based_model
summary(theory_based_model)

# Model comparison
# R ^ 2
summary(theory_based_model)$adj.r.squared
summary(backward.model)$adj.r.squared
summary(initial.model)$adj.r.squared

# AIC and ANOVA - Checking fitness of model 
# Note: you still need anova as the backward model is nested in the other
# BUT no anova for the initial and backward model because they are not really nested
anova(theory_based_model, backward.model) 

AIC(theory_based_model)
AIC(backward.model)
AIC(initial.model)

# Note AIC diff should be higher than 2

# Predict data file 1 both models
pred_sample1_theory_mod = predict(theory_based_model)
pred_sample1_backward.model = predict(backward.model)

RSS_sample1_theory_mod = sum((sample1_no_out$pain - pred_sample1_theory_mod)^2)
RSS_sample1_backward_mod = sum((sample1_no_out$pain - pred_sample1_backward.model)^2)

RSS_sample1_theory_mod

RSS_sample1_backward_mod

# Other checks
coef_table(backward.model)
coef_table(initial.model)

# Predict new data with both models

pred_sample2_theory_mod = predict(theory_based_model, sample_2)
pred_sample2_backward_mod = predict(backward.model, sample_2)

# Calculate sum of squared residuals sample 2
RSS_test_theory_model = sum((sample_2[, "pain"] - pred_sample2_theory_mod)^2)
RSS_test_backward_model = sum((sample_2[, "pain"] - pred_sample2_backward_mod)^2)

RSS_test_theory_model

RSS_test_backward_model

# calculating total sum of squared differences (TSS) for sample 1
model_mean_sample_1 <- lm(pain ~ 1, data = sample1_no_out) 

TSS = sum((sample1_no_out$pain - predict(model_mean_sample_1))^2)	
TSS	

R2 = 1-(RSS_sample1_theory_mod/TSS)	
R2	

R2 = 1-(RSS_sample1_backward_mod/TSS)
R2

# Sample 2 theory based model, remember the models are trained in sample 1
model_mean_sample_2 <- lm(pain ~ 1, data = sample_2) 

TSS = sum((sample_2$pain - predict(model_mean_sample_2))^2)	
TSS	

R2 = 1-(RSS_test_backward_model/TSS)	
R2

R2 = 1-(RSS_test_theory_model/TSS)	
R2

# Sample 2 backward model, remember the models are trained in sample 1

RAD = sum(abs(sample1_no_out$pain - predict(backward.model)))
RAD

RSS = sum((sample1_no_out$pain - predict(backward.model))^2)	
RSS	

TSS = sum((sample_2$pain - predict(model_mean_sample_2))^2)	
TSS	

R2 = 1-(RSS/TSS)	
R2	


# NOT SURE IF I SHOULD CHECK ASSUMPTIONS OR NOT
# Model diagnostics / Assumptions if the backward model were better BUT ITS NOT
backward.model %>%
  plot(which = 5)

backward.model %>%
  plot(which = 4)

sample1_no_out %>%
  slice(c(46, 102, 115))

# Checking NORMALITY ----  QQ plot
backward.model %>%
  plot(which = 2)

# Histogram
residuals_backward_model = enframe(residuals(backward.model))
residuals_backward_model %>%
  ggplot() + aes(x = value) + geom_histogram()

# Checking skew and kurtosis
describe(residuals(backward.model))

# Checking LINEARITY
backward.model %>%
  residualPlots()

# Checking HOMOSCEDASTICITY
backward.model %>%
  plot(which = 3)

# NCVtest
backward.model %>%
  ncvTest()

# Breush-Pagan test
backward.model %>%
  bptest() 

# Checking No MULTICOLLINEARITY
backward.model %>% 
  vif()



# ASSIGNMENT PART 3 !!


# Loading data for 3rd assignment
sample_3 = read.csv("https://tinyurl.com/b385chpu")
sample_4 = read.csv("https://tinyurl.com/4f8thztv")

# Load packages
library(lm.beta)
library(lme4)
library(r2glmm)
library(lmerTest)
library(MuMIn)
library(cAIC4)

# Custom function from class
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

# Check the data
summary(sample_3)
view(sample_3)

# Converting some data into new dataframe
sample_3_mod = sample_3 %>%
  mutate(sex = factor(sex), hospital = factor(hospital))

# There was also a probable mistake with household income
sample_3_mod$household_income[2] <- 7884
view(sample_3_mod)
summary(sample_3_mod)

# Fixing another issue with entering "woman" instead of "female" in sex
sample_3_mod$sex[25] <- "female"
view(sample_3_mod)
summary(sample_3_mod)
sample_3_mod = sample_3_mod %>%
  mutate(sex = factor(sex), hospital = factor(hospital))

# Checking clustering of the data
sample_3_mod %>%
  ggplot() + aes(y = pain, x = cortisol_serum) + geom_point(aes(color = hospital),
                                                            size = 4) + geom_smooth(method = "lm", se = F)

int_plot_1 = sample_3_mod %>%
  ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot_1

int_plot_1 + xlim(-1, 50) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)

# Checking second data set
summary(sample_4)
sample_4_mod = sample_4 %>%
  mutate(sex = factor(sex), hospital = factor(hospital))
summary(sample_4_mod)
view(sample_4_mod)

# Building the mixed model
mod_rdm_int = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1 | hospital), data = sample_3_mod)

# Visualize NOT SURE IF I SHOULD KEEP THIS
sample_3_mod = sample_3_mod %>%
  mutate(pred_rdm_int = predict(mod_rdm_int))

sample_3_mod %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
    aes(y = pred_rdm_int, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)

# Model fitness
sum(residuals(mod_rdm_int)^2)	

cAIC(mod_rdm_int)$caic
      
# Describe methods (maybe because it is a random int but double check)
      
cAIC(mod_rdm_int)$caic
      
# Model coefficients (assigment part 1)
coef_table(theory_based_model)
summary(mod_rdm_int)

# Marginal R squared with confidence intervals
r2beta(mod_rdm_int, method = "nsj", data = sample_3_mod)
r2beta(model_4, method = "nsj", data = sample_1)
      
# marginal and conditional R squared values	
r.squaredGLMM(mod_rdm_int)	
r.squaredGLMM(model_4)

# Model coefficients and p-values
summary(mod_rdm_int)
summary(model_4)
      
# Confidence interval for the model coefficients
confint(mod_rdm_int)
confint(model_4)
      
# Standardized beta for each predictor
stdCoef.merMod(mod_rdm_int)
      
# Predicting pain using new data (sample 4)
pred_sample4_rdm_int_mod = predict(mod_rdm_int, sample_4_mod, allow.new.levels = TRUE)
      
#@ Calculate sum of squared residuals !!!! double check this!!!!
RSS_mod_rdm_int = sum((sample_4_mod[, "pain"] - pred_sample4_rdm_int_mod)^2)
RSS_mod_rdm_int

# Calculate TSS
sample4_mod_mean <- lm(pain ~ 1, data = sample_4_mod)

TSS_mod_rdm_int = sum((sample_4_mod$pain - predict(sample4_mod_mean))^2)	
TSS_mod_rdm_int	
      
R2_mod_rdm_int = 1-(RSS_mod_rdm_int/TSS_mod_rdm_int)	
R2_mod_rdm_int

# Compare this R2 to the marginal & conditional R2 values computed for the model on sample 3.
r.squaredGLMM(mod_rdm_int)
      
# Decide which predictors to keep on to the next one (use confidence intervals)
r2beta(mod_rdm_int, method = "nsj", data = sample_3_mod)
r2beta(mod_rndm_slope, method = "nsj", data = sample_3_mod)

## The random intercept takes into account the hospitals
## The R2 predicted on new hospitals performs worse
      
# Random slope model
mod_rndm_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital),
                            data = sample_3_mod)
      
# Visualizing mixed model with only 1 influential variable (last model)
sample_3_mod = sample_3_mod %>%
  mutate(pred_int = predict(mod_rdm_int), pred_slope = predict(mod_rndm_slope))
      
sample_3_mod %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
    aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)
      
# Remember to mention the 'boundary (singular) fit: see ?isSingular' 
# do not try to fix it!!

# Anova
anova(mod_rdm_int, mod_rndm_slope)

# cAIC
cAIC(mod_rdm_int)$caic
cAIC(mod_rndm_slope)$caic
