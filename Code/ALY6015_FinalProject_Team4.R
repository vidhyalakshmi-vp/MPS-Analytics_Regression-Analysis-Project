library(tidyverse)
library(tidyr)
library(skimr)
library(magrittr)
library(flextable)
library(rrtable)
library(officer)
library(readxl)
library(DescTools)
library(janitor)
library(Metrics)
library(knitr)
library(kableExtra)
library(Hmisc)
library(glmnet)
library(formatters)

##### My function to save an object as table #####
saveMyTable <- function(f_obj, f_Name, r_name){
  ft <- df2flextable(f_obj,
                     fontname = "Consolas",
                     fontsize = 10,
                     add.rownames = r_name,
                     align_body = 'center')
  ft <- autofit(ft)
  theme_box(ft)
  save_as_docx(ft, path = f_Name)
}

##### All imports #####
#Importing and merging the dataset
file1 <- read.csv("Datasets/FARS_crash_level_2010-2014.csv")
file2 <- read.csv("Datasets/FARS_crash_level_2015.csv")
FARS_DS <- bind_rows(file1, file2)
rm(file1, file2) #cleaning environment

# Importing the codebook
FARS_Codebook <- read_excel("Datasets/Codebook_FARS.xlsx", 
                            sheet = "Variable List")

#Categorical variable names - 24 vars
cat_Vars <-  FARS_Codebook %>%
  dplyr::filter(`Type (ordinal, categorical, etc)` == 'categorical') %>%
  select(`Variable Code Name`)  
cat_Vars <- cat_Vars$`Variable Code Name`
cat_Vars <- cat_Vars[-c(1,2,3,4,5,6,8,9,10,12,13)]
cat_Vars


##### EDA #####
dim(FARS_DS)
str(FARS_DS)

#Overview of the data set
head(FARS_DS)
tail(FARS_DS)
summary(FARS_DS)

#Checking for missing and NA values
nrow(na.omit((FARS_DS)))
nrow(FARS_DS) - sum(complete.cases(FARS_DS))

#Summary of all varaibles in the dataset
#code reference: https://cran.r-project.org/web/packages/skimr/vignettes/skimr.html
summaryall <- skim(FARS_DS) %>%
  dplyr::select(skim_variable,
                numeric.mean,
                numeric.sd,
                n_missing,
                complete_rate) %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  set_colnames(c("Variable", "Mean", "Std. Deviation", "N missing", "Completion Rate"))

# Exporting as table to Word doc
saveMyTable(summaryall, "Finalproject_Table_1.docx", FALSE)


##### Level 1 filter : FARS_DS_1 #####
# Removing rows with NA in fatals column
FARS_DS_1 <- FARS_DS %>% drop_na(fatals)
dim(FARS_DS_1) # 183,593 obs. 58 var.
nrow(FARS_DS) - nrow(FARS_DS_1) #1833 rows removed

##### My function to check NA % in DS #####
checkNA <- function(obj_dataset){
  #Checking for missing data(NA) percentage in each column
  missing_NA <- round((colMeans(is.na(obj_dataset)))*100,2)
  order <- order(missing_NA,decreasing = TRUE)
  missing_NA_ordered <- missing_NA[order]
  #converting to data frame
  tmp <- data.frame(missing_NA_ordered)
  tmp_c <- row.names(tmp)
  row.names(tmp) <- NULL
  missing_NA_df <- cbind(tmp_c, tmp)
  good <- missing_NA_df %>% filter(missing_NA_df$missing_NA_ordered == 0)
  gdCols <- good$tmp_c
  my_list <- list("miss" = missing_NA_df, "gud" = gdCols)
  return(my_list) 
}


##### Level 2 filter : FARS_DS_2 #####
# Finding the unique values in the columns
unique_count <- sapply(lapply(FARS_DS_1, unique), length, simplify=FALSE)
singleValueCols <- data.frame(unique_count[!(unique_count > 1)])
colnames(singleValueCols) #there is no column with only one unique values. All columns can be used

#Checking for missing data(NA) percentage in each column
colAnalysis_1 <- checkNA(FARS_DS_1)

#Dropping column drugs_inv with more than 60% NA values
FARS_DS_2 <- dplyr::select(FARS_DS_1, -drugs_inv)
dim(FARS_DS_2)
nrow(FARS_DS_1) - nrow(FARS_DS_2) #0 rows and 1 column removed

##### FARS_final - omitting all NA #####
#Removing all incomplete cases
FARS_final <- na.omit(FARS_DS_2)
dim(FARS_final)   #87,754 obs. over 57 variables


##### Level 3 filter : FARS_DS_3 - latitude and city #####
#Dropping rows with NA values in column -latitude
FARS_DS_3 <- FARS_DS_2[complete.cases(FARS_DS_2$latitude),]
FARS_DS_3 <- FARS_DS_3[complete.cases(FARS_DS_3$city),]
nrow(FARS_DS_2) - nrow(FARS_DS_3) #1910 rows and 1 column removed

#Checking for missing data(NA) percentage in each column
colAnalysis_2 <- checkNA(FARS_DS_3)

##### Converting Categorical variables into factors #####
#Removing removed column name in cat_Vars
cat_Vars <- cat_Vars[-8]

#Converting all the categorical variables into factors
FARS_DS_3[cat_Vars] <- lapply(FARS_DS_3[cat_Vars], as.factor)

##### Code for imputing : FARS_imputed ######
FARS_imputed <- FARS_DS_3 %>% 
  mutate_if(is.factor, 
            function(n) ifelse(is.na(n), Mode(n,na.rm=TRUE),n)) %>% 
  mutate_if(is.integer, 
            function(n) ifelse(is.na(n), round(mean(n,na.rm=TRUE),0),n)) 

#Checking for incomplete rows
nrow(FARS_imputed) - sum(complete.cases(FARS_imputed))
dim(FARS_imputed) #181683 obs.  57 var




######### Model Regression ############

##### Data splitting for prediction models #####
#Data splicing
set.seed(123)
train_index <- sample(1:nrow(FARS_final),
                      size = ceiling(0.8*nrow(FARS_final)),
                      replace = FALSE)

# Creating the training set
training_data <- FARS_final[train_index,]
dim(training_data) #70,204 obs. 57 variables

# Creating the test set
testing_data <- FARS_final[-train_index,]
dim(testing_data) #17,550 obs. 57 variables

#Converting the data into matrix
train_x <- model.matrix(fatals~., training_data)[,-1]
train_y <- training_data$fatals
test_x <- model.matrix(fatals~., testing_data)[,-1]
test_y <- testing_data$fatals


##### Data splicing - type 2 #####
set.seed(123)
train_index <- sample(1:nrow(FARS_imputed),
                      size = 16000,
                      replace = FALSE)

# Creating the training set
training_data <- FARS_imputed[train_index,]
dim(training_data) 

# Creating the temp test set
testing_data <- FARS_imputed[-train_index,]
dim(testing_data) 

test_index <- sample(1:nrow(testing_data),
                     size = 4000,
                     replace = FALSE)

# Creating the test set
testing_data <- testing_data[test_index,]
dim(testing_data)

#Converting the data into matrix
train_x <- model.matrix(fatals~., training_data)[,-1]
train_y <- training_data$fatals
test_x <- model.matrix(fatals~., testing_data)[,-1]
test_y <- testing_data$fatals



##### Simple Linear Regression using all variables #####
slm_1 <- lm(fatals~., data = training_data)
summary(slm_1)
#coeff matrix
slm_1_coe <- coef(slm_1)
tmp_names <- names(slm_1_coe)
names(slm_1_coe) <- NULL
slm_1_coe_df <- cbind(tmp_names, slm_1_coe)
#model evaluation
AIC(slm_1)
BIC(slm_1)


#Predicting the training dataset
pred_train_slm_1 <- predict(slm_1, new = training_data)
rmse_train_slm_1 <- rmse(training_data$fatals, pred_train_slm_1)
rmse_train_slm_1

#Predicting the testing dataset
pred_test_slm_1 <- predict(slm_1, new = testing_data)
rmse_test_slm_1 <- rmse(testing_data$fatals, pred_test_slm_1)
rmse_test_slm_1

#comparing the values
slm_1_mat <- c(round(AIC(slm_1), 2),
               round(BIC(slm_1), 2), 
               round(rmse_train_slm_1, 4),
               round(rmse_test_slm_1, 4))


##### Linear Regression using high significant variables #####
slm_2 <- lm(fatals ~ nhs + drunk_dr + total_fire_exp + 
              no_prev_dwi + one_prev_dwi + dr_alcohol_drug_med +
              total_moving_violations + pernotmvit + permvit,
            data = training_data)
#coeff matrix
slm_2_coe <- coef(slm_2)
tmp_names <- names(slm_2_coe)
names(slm_2_coe) <- NULL
slm_2_coe_df <- cbind(tmp_names, slm_2_coe)

summary(slm_2)
AIC(slm_2)
BIC(slm_2)

#Predicting the training dataset
pred_train_slm_2 <- predict(slm_2, new = training_data)
rmse_train_slm_2 <- rmse(training_data$fatals, pred_train_slm_2)
rmse_train_slm_2

#Predicting the testing dataset
pred_test_slm_2 <- predict(slm_2, new = testing_data)
rmse_test_slm_2 <- rmse(testing_data$fatals, pred_test_slm_2)
rmse_test_slm_2

#comparing the values
slm_2_mat <- c(round(AIC(slm_2), 2),
               round(BIC(slm_2), 2), 
               round(rmse_train_slm_2, 4),
               round(rmse_test_slm_2, 4))




##### Simple Linear Regression using 10 variables #####
slm_3 <- lm(fatals ~ total_moving_violations+ drunk_dr+ total_not_registered+
              total_invalid_license+
              two_prev_sus+ one_prev_oth+ speed_related+ dr_age_lower65+ 
              dr_other_impair+ dr_alcohol_drug_med,
            data = training_data)
#coeff matrix
slm_3_coe <- coef(slm_3)
tmp_names <- names(slm_3_coe)
names(slm_3_coe) <- NULL
slm_3_coe_df <- cbind(tmp_names, slm_3_coe)

summary(slm_3)
AIC(slm_3)
BIC(slm_3)

#Predicting the training dataset
pred_train_slm_3 <- predict(slm_3, new = training_data)
rmse_train_slm_3 <- rmse(training_data$fatals, pred_train_slm_3)
rmse_train_slm_3

#Predicting the testing dataset
pred_test_slm_3 <- predict(slm_3, new = testing_data)
rmse_test_slm_3 <- rmse(testing_data$fatals, pred_test_slm_3)
rmse_test_slm_3

#comparing the values
slm_3_mat <- c(round(AIC(slm_3), 2),
               round(BIC(slm_3), 2), 
               round(rmse_train_slm_3, 4),
               round(rmse_test_slm_3, 4))



##### Model stepwise selection #####
model_step <- step(lm(fatals~., data = training_data),
                   direction = 'both')
#model evaluation
AIC(model_step)
BIC(model_step)
#no of pred
model_step$rank


#Predicting the training dataset
pred_train_step <- predict(model_step, new = training_data)
rmse_train_step <- rmse(training_data$fatals, pred_train_step)
rmse_train_step

#Predicting the testing dataset
pred_test_step <- predict(model_step, new = testing_data)
rmse_test_step <- rmse(testing_data$fatals, pred_test_step)
rmse_test_step

#comparing the values
step_mat <- c(round(AIC(model_step), 2),
              round(BIC(model_step), 2), 
              round(rmse_train_step, 4),
              round(rmse_test_step, 4))



##### LASSO Regression Model #####
#alpha = 1
#Finding lambda value
set.seed(123)
cv_lasso <- cv.glmnet(train_x, train_y,
                      alpha = 1,
                      nlambda = 100) #cross validate
plot(cv_lasso)
#Viewing the values
cv_lasso
summary(cv_lasso)
log(cv_lasso$lambda.min)
log(cv_lasso$lambda.1se)
cv_lasso$index


#Fitting the model using lambda min
model_l1 <- glmnet(train_x, train_y,
                   alpha = 1,
                   lambda = cv_lasso$lambda.min)
model_l1$beta
model_l1$df

#Predictions using Training dataset
pred_train_l1 <- predict(model_l1, newx = train_x)
rmse_train_l1 <- rmse(train_y, pred_train_l1)
rmse_train_l1

#Predictions using Testing dataset
pred_test_l1 <- predict(model_l1, newx = test_x)
rmse_test_l1 <- rmse(test_y, pred_test_l1)
rmse_test_l1

#comparing the values
lasso_mat <- c("-","-",
               round(rmse_train_l1,4),
               round(rmse_test_l1,4))

lasso_mat


##### comparing the models #####
compare <- rbind(slm_1_mat, slm_2_mat, slm_3_mat,step_mat, lasso_mat)
row.names(compare) <- c("LRM with all variables",
                        "LRM with sig. variables",
                        "LRM with 10 variables",
                        "LRM with stepwise selection",
                        "LASSO Regression Model")
colnames(compare) <- c("AIC", "BIC", "RMSE Training Data", "RMSE Testing Data")
saveMyTable(data.frame(compare),"CompareModel_final_smallsample.docx", TRUE)



########## Answering Research Questions ########## 
##### Summary of cases fatalities exist by Year ##### 

pct_fatals_year <- FARS_final %>% tabyl(year, fatals) %>% 
  adorn_totals(c("row","col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>% knitr::kable()
pct_fatals_year

#Summary of cases fatalities exist by State ##

fatals_state1 = FARS_final %>% group_by(state) %>%
  summarize(high_fatals = sum(fatals>1)) %>% knitr::kable()  
fatals_state1

fatals_state2 = FARS_final %>% tabyl(state)
fatals_state2

fatals_state2$n_rank = rank(desc(fatals_state2$n)) 
fatals_state2

##### Summary of cases fatalities exist by Weather condition #####
pct_fatals_weather <- FARS_final %>% tabyl(fatals, weather) %>% 
  adorn_totals(c("row","col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined")%>%
  knitr::kable()

pct_fatals_weather

##### Summary of cases fatalities exist by light condition ####
pct_fatals_light <- FARS_final %>% tabyl(fatals, lgt_cond) %>% 
  adorn_totals(c("row","col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined")%>%
  knitr::kable()

pct_fatals_light

##### Selected variable correlation ##### 
dfnew <- FARS_final[c(3:4, 19:21,27:31,42, 47:49, 56:57)]

plot1<-corrplot(cor(dfnew, method = 'spearman', use = "pairwise.complete.obs"))

plot2<-corrplot(cor(dfnew, method = 'number', use = "pairwise.complete.obs"))

## Scatter plot
library(ggplot2)
dfnew1 <- FARS_final[c(19,21)]
ggplot(dfnew1,aes(total_numoccs,fatals))+geom_point()

#-------------------------------------------------------------------------------
#test mean assumption difference categorical variable
# Speed related
#number of fatalities with speed
# State the hypotheses
# H0: There is no difference in means of number of fatalities in cases with different speed group
# H1: There is difference in means of number of fatalities in cases with different speed group

# Set significance level 
alpha = 0.05

# Create a dataframe for speed

FARS_speed_1 <- FARS_final %>% filter(speed_related == 1)
FARS_speed <- FARS_final %>% filter(speed_related == 0)

speed <- data.frame(number_fatals = FARS_speed_1$fatals, group = rep("speed", 19985))

# Create a dataframe for no_speed
no_speed <- data.frame(number_fatals = FARS_speed$fatals, group = rep("no_speed", 67769))

#Combine the dataframes into one
data <- rbind(speed,no_speed)

# Run the test and save the results to the result variable 
result <- kruskal.test(number_fatals ~ group, data = data) 
result

# View the p-value 
result$p.value

#determine if we should reject the null hypothesis 
ifelse(result$p.value > alpha, "fail to reject the null", "reject the null")

#-------------------------------------------------------------------------------
#number of fatalities with Drugs_MED Involvement 
# State the hypotheses
# H0: There is no difference in means of number of fatalities in cases with different Drugs_MED group
# H1: There is difference in means of number of fatalities in cases with different Drugs_MED group

# Set significance level 
alpha = 0.05

# Create a dataframe for drugs

FARS_drugs_1 <- FARS_final %>% filter(dr_alcohol_drug_med == 1)
FARS_drugs <- FARS_final %>% filter(dr_alcohol_drug_med == 0)

drugs <- data.frame(number_fatals = FARS_drugs_1$fatals, group = rep("drugs", 23966))

# Create a dataframe for no_drugs
no_drugs <- data.frame(number_fatals = FARS_drugs$fatals, group = rep("no_drugs", 63788))

#Combine the dataframes into one
data <- rbind(drugs,no_drugs)

# Run the test and save the results to the result variable 
result <- kruskal.test(number_fatals ~ group, data = data) 
result

# View the p-value 
result$p.value

#determine if we should reject the null hypothesis 
ifelse(result$p.value > alpha, "fail to reject the null", "reject the null")

#-------------------------------------------------------------------------------
#number of fatalities with other impairment 
# State the hypotheses
# H0: There is no difference in means of number of fatalities in cases with different other impairment group
# H1: There is difference in means of number of fatalities in cases with different other impairment group

# Set significance level 
alpha = 0.05

# Create a dataframe for impair

FARS_impair_1 <- FARS_final %>% filter(dr_other_impair == 1)
FARS_impair <- FARS_final %>% filter(dr_other_impair == 0)

impair <- data.frame(number_fatals = FARS_impair_1$fatals, group = rep("impair", 7304))

# Create a dataframe for no_impair
no_impair <- data.frame(number_fatals = FARS_impair$fatals, group = rep("no_impair", 80450))

#Combine the dataframes into one
data <- rbind(impair,no_impair)

# Run the test and save the results to the result variable 
result <- kruskal.test(number_fatals ~ group, data = data) 
result

# View the p-value 
result$p.value

#determine if we should reject the null hypothesis 
ifelse(result$p.value > alpha, "fail to reject the null", "reject the null")

#-------------------------------------------------------------------------------
#number of fatalities with national highway system 
# State the hypotheses
# H0: There is no difference in means of number of fatalities in cases with different highway group
# H1: There is difference in means of number of fatalities in cases with different highway group

# Set significance level 
alpha = 0.05

# Create a dataframe for highway

FARS_highway_1 <- FARS_final %>% filter(nhs == 1)
FARS_highway <- FARS_final %>% filter(nhs == 0)

highway <- data.frame(number_fatals = FARS_highway_1$fatals, group = rep("highway", 29642))

# Create a dataframe for no_highway
no_highway <- data.frame(number_fatals = FARS_highway$fatals, group = rep("no_highway", 58112))

#Combine the dataframes into one
data <- rbind(highway,no_highway)

# Run the test and save the results to the result variable 
result <- kruskal.test(number_fatals ~ group, data = data) 
result

# View the p-value 
result$p.value

#determine if we should reject the null hypothesis 
ifelse(result$p.value > alpha, "fail to reject the null", "reject the null")

#-------------------------------------------------------------------------------
#number of fatalities with Total hit and run
# State the hypotheses
# H0: There is no difference in means of number of fatalities in cases with different hit and run group
# H1: There is difference in means of number of fatalities in cases with different hit and run group

# Set significance level 
alpha = 0.05

# Create a dataframe for hit and run

FARS_hitnrun_1 <- FARS_final %>% filter(total_hit_run == 1)
FARS_hitnrun <- FARS_final %>% filter(total_hit_run == 0)


hitnrun <- data.frame(number_fatals = FARS_hitnrun_1$fatals, group = rep("hitnrun", 2096))

# Create a dataframe for hit and run
no_hitnrun <- data.frame(number_fatals = FARS_hitnrun$fatals, group = rep("no_hitnrun", 85658))

#Combine the dataframes into one
data <- rbind(hitnrun,no_hitnrun)

# Run the test and save the results to the result variable 
result <- kruskal.test(number_fatals ~ group, data = data) 
result

# View the p-value 
result$p.value

#determine if we should reject the null hypothesis 
ifelse(result$p.value > alpha, "fail to reject the null", "reject the null")

#-------------------------------------------------------------------------------
#number of fatalities with Total fire exp
# State the hypotheses
# H0: There is no difference in means of number of fatalities in cases with different fire occurance group
# H1: There is difference in means of number of fatalities in cases with different fire occurance group

# Set significance level 
alpha = 0.05

# Create a dataframe for fire_exp

FARS_fire_1 <- FARS_final %>% filter(total_fire_exp == 1)
FARS_fire <- FARS_final %>% filter(total_fire_exp == 0)

fire_exp <- data.frame(number_fatals = FARS_fire_1$fatals, group = rep("fire_exp", 3416))

# Create a dataframe for fire_exp
no_fire_exp <- data.frame(number_fatals = FARS_fire$fatals, group = rep("no_fire_exp", 84338))

#Combine the dataframes into one
data <- rbind(fire_exp,no_fire_exp)

# Run the test and save the results to the result variable 
result <- kruskal.test(number_fatals ~ group, data = data) 
result

# View the p-value 
result$p.value

#determine if we should reject the null hypothesis 
ifelse(result$p.value > alpha, "fail to reject the null", "reject the null")
#-------------------------------------------------------------------------------

# test mean assumption quantitative variable

#focusing only serious accident
FARS_serious_acc <- FARS_final %>% filter(fatals > 1)

#the number of serious in male and female
# State the hypotheses
# H0: There is no difference in the number of drivers by each gender
# H1: There is difference in the number of drivers by each gender 

# Set significance level 
alpha = 0.05

# Create vectors of the values
male <- FARS_serious_acc$dr_male
female <- FARS_serious_acc$dr_female

# Run the test and save the results to the result variable 
result <- wilcox.test(x= male, y= female, alternative = "two.sided", correct = TRUE) 
result

# View the p-value 
result$p.value

#determine if we should reject the null hypothesis 
ifelse(result$p.value > alpha, "fail to reject the null", "reject the null")

#-------------------------------------------------------------------------------
#the number fatalities with the number of occupancy 
# State the hypotheses
# H0: There is no difference in means of number of fatalities in cases with different occupancy group
# H1: There is difference in means of number of fatalities in cases with different occupancy group

# Set significance level 
alpha = 0.05

# Create a dataframe for the number of occupancy
FARS_total_numoccs_1 <- FARS_final %>% filter(total_numoccs >= 1.5)
FARS_total_numoccs <- FARS_final %>% filter(total_numoccs < 1.5)

high_numoccs <- data.frame(number_fatals = FARS_total_numoccs_1$fatals, group = rep("high_numoccs", 54702))

# Create a dataframe for numocc
low_numoccs <- data.frame(number_fatals = FARS_total_numoccs$fatals, group = rep("low_numoccs", 33052))

#Combine the dataframes into one
data <- rbind(high_numoccs,low_numoccs)

# Run the test and save the results to the result variable 
result <- kruskal.test(number_fatals ~ group, data = data) 
result

# View the p-value 
result$p.value

#determine if we should reject the null hypothesis 
ifelse(result$p.value > alpha, "fail to reject the null", "reject the null")

#-------------------------------------------------------------------------------
############  ANOVA ########################
# Use One-Way ANOVA test with a 0.05 level of significance to 
# determine if there is a difference in  
# mean of the number of drunk drivers, number of drivers with invalid licenses,
# and number of drivers under age 16 

# State the hypotheses
# H0 : µ1 = µ2 = µ3  
# H1 : At least one mean is different from the others

# Create a data.frame for drunk_dr
drunk_dr <- data.frame('number_driver' = FARS_final$drunk_dr ,
                       'driver_type' = rep('drunk driver',87754),
                       stringsAsFactors = FALSE)

# Create a data.frame for total_invalid_license
invalid_license <- data.frame('number_driver' = FARS_final$total_invalid_license ,
                              'driver_type' = rep('invalid licenses',87754),
                              stringsAsFactors = FALSE)

# Create a data.frame for dr_age_lower16
age_lower_16 <- data.frame('number_driver' = FARS_final$dr_age_lower16 ,
                           'driver_type' = rep('age lower 16',87754),
                           stringsAsFactors = FALSE)

# Combine into one dataframe
drivers <- rbind(drunk_dr, invalid_license, age_lower_16)
drivers$driver_type <- as.factor(drivers$driver_type)

# Run the ANOVA test
anova <- aov(number_driver ~ driver_type, data = drivers)

# View the model summary
summary(anova)

# Save summary to an object
a.summary <- summary(anova)

# Degrees of freedom 
# k-1 : between group variance - numerator
df.numerator <- a.summary[[1]][1,"Df"]
df.numerator

# N - k: within group variance - denominator
df.denominator <- a.summary[[1]][2, "Df"]
df.denominator

# Extract the F test value from the summary
F.value <- a.summary[[1]][[1, "F value"]]
F.value

options(scipen = 999)
# Extract the p-value from the summary
p.value <- a.summary[[1]][1, "Pr(>F)"]
p.value

#Critical value
anova_cv <- qf(p=1-alpha, df.numerator, df.denominator)

#Make the decision
# Compare the test statistic and critical value
ifelse( F.value > anova_cv,
        "The test value is greater than the critical value",
        "The test value is lesser than the critical value")
ifelse(p.value < alpha,
       "Reject the null hypothesis",
       "Failed to reject the null hypothesis")

# See differences
TukeyHSD(anova)

