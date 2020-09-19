######## Analysis script - P1 Social Norms, Incentives, and Prosociality ######## 

##### Overview:

# Preprocessing
# Descriptives
# Tests for country-level variation

### 0) Intercept-only models

### 1) Demographics

### 2) Financial incentives - Models with predictors of interest: Full sample

### 3) Financial incentives - Models with predictors of interest: Excluding participants not willing to donate (i.e., who didn't answer IM and EM_get_return))

### 4) Time incentives - Models with predictors of interest: Full sample

### 5) Time incentives - Models with predictors of interest: Excluding participants not willing to donate (i.e., who didn't answer IM and EM_get_return))

### 6) Predicted probabilities and plotting (scatter and bar plots)

### 7) Map plots


####################################################################

##### Load packages
library("readxl")
library("tidyverse")
library("lme4")
library("haven")
library("sjlabelled")
library("data.table")
library("scales")
library("eurostat")
library("RColorBrewer")
library("psych")

##### Set WD

setwd("/Users/caroline/Desktop/PhD/P1_incentives/analysis")

##### Load data sets

# Eurobarometer (2014 wave)
eurobarometer_data_raw = read_dta("../data/Eurobarometer/ZA5931_v3-0-0.dta")

# Incentive data
incentive_data_raw = read.csv("../data/expert_survey/incentive_data.csv")

################################################################

##### Process and combine data sets

# Select relevant variables
eurobarometer_data = select(eurobarometer_data_raw, uniqid, nuts, nutslvl, isocntry, qe1_1, qe1_2, qe3_2, qe3_3, qe3_4, qe3_5, d15a, d60, qe5a_7, qe5a_8, d40b, d25, d11, d8r2, d10, d7)
incentive_data = select(incentive_data_raw, -WB_incentive_financial_comment, -plasma_incentive_financial_comment, -WB_incentive_time_comment, -plasma_incentive_time_comment)

# Remove labels
eurobarometer_data = remove_all_labels(eurobarometer_data)
eurobarometer_data = zap_formats(eurobarometer_data)

# Rename columns
eurobarometer_data = rename(eurobarometer_data,
                            IM_help_people = qe3_2,
                            IM_alleviate_shortages = qe3_3,
                            IM_medical_research = qe3_4,
                            EM_get_return_raw = qe3_5,
                            EM_occupation = d15a, 
                            EM_diff_paying_bills = d60, 
                            social_norm_financial = qe5a_7,
                            social_norm_time = qe5a_8,
                            cost_children_in_household = d40b, 
                            cost_type_of_community = d25,
                            age_raw = d11, 
                            education = d8r2, 
                            gender = d10, 
                            marital_status = d7)

# Recode country code
eurobarometer_data$country_code = ifelse(eurobarometer_data$isocntry == "DE-W", "DE",
                                         ifelse(eurobarometer_data$isocntry == "DE-E", "DE",
                                                ifelse(eurobarometer_data$isocntry == "GB-GBN", "GB",
                                                       ifelse(eurobarometer_data$isocntry == "GB-NIR", "GB", eurobarometer_data$isocntry))))

# Change country_code format for merging
incentive_data$country_code = as.character(incentive_data$country_code)

# Merge datasets based on country
data = left_join(eurobarometer_data, incentive_data, by="country_code")

##### Preprocess combined dataset for analysis

# Variable transformations

data = data %>%
  mutate(donate_blood = ifelse(qe1_1 == 1 | qe1_1 == 2, 1, 0),
         donate_plasma = ifelse(qe1_2 == 1 | qe1_2 == 2, 1, 0),
         IM_raw = ifelse(IM_help_people == 1 | IM_alleviate_shortages == 1 | IM_medical_research == 1, 1, 0),
         EM_not_employed = ifelse(EM_occupation == 1 | EM_occupation == 2 | EM_occupation == 3 | EM_occupation == 4, 1, 0),
         EM_employed = ifelse(EM_occupation == 1 | EM_occupation == 2 | EM_occupation == 3 | EM_occupation == 4, 0, 1),
         education = ifelse(education == 11, 0, ifelse(education == 10 & age_raw>19, 3, ifelse(education == 10, NA, education))),
         living_with_partner = ifelse(marital_status > 0 & marital_status < 9, 1, 0),
         EM_diff_paying_bills = ifelse(EM_diff_paying_bills == 1, 2, ifelse(EM_diff_paying_bills == 2, 1, ifelse(EM_diff_paying_bills == 3, 0, NA))),
         cost_type_of_community = ifelse(cost_type_of_community == 1, 2, ifelse(cost_type_of_community == 2, 1, ifelse(cost_type_of_community == 3, 0, NA))),
         gender = gender - 1)%>%
  mutate(IM = ifelse(is.na(IM_raw), 0, IM_raw),
         EM_get_return = ifelse(is.na(EM_get_return_raw), 0, EM_get_return_raw))%>%
  select(-IM_help_people, -IM_alleviate_shortages, -IM_medical_research, -EM_occupation, -marital_status) #, -qe1_1, -qe1_2)

# Aggregate social norms from individual responses at country level
social_norms = data%>%
  group_by(country_code)%>%
  summarize(soc_financial_raw = mean(social_norm_financial),
            soc_time_raw = mean(social_norm_time))%>%
  #mutate(soc_financial_biased = scales::rescale(soc_financial_raw, to = c(-1, 1), from = c(0,0.5)))%>%
  mutate(soc_financial = scales::rescale(soc_financial_raw, to = c(-1, 1), from = c(0,1)),
         soc_time = scales::rescale(soc_time_raw, to = c(-1, 1), from = c(0,1)))

# Add social norm data to dataset
data_full = left_join(data, social_norms, by="country_code")

# Convert variables to right format
data_full = data_full%>%
  mutate(donate_blood = as.factor(donate_blood),
         donate_plasma = as.factor(donate_plasma),
         cost_type_of_community = as.factor(cost_type_of_community),
         education = as.factor(education),
         gender = as.factor(gender),
         country_code = as.factor(country_code),
         IM = as.factor(IM),
         living_with_partner = as.factor(living_with_partner),
         EM_get_return = as.factor(EM_get_return),
         EM_diff_paying_bills = as.factor(EM_diff_paying_bills),
         EM_not_employed = as.factor(EM_not_employed),
         EM_employed = as.factor(EM_employed),
         WB_incentive_financial = as.factor(WB_incentive_financial),
         plasma_incentive_financial = as.factor(plasma_incentive_financial),
         WB_incentive_time = as.factor(WB_incentive_time),
         plasma_incentive_time = as.factor(plasma_incentive_time))


################################################################

##### Descriptives: number of respondents

## Raw data: number of respondents
nrow(data_full) # 27868

# Remove invalid data
data_WB = filter(data_full, !is.na(donate_blood))
nrow(data_WB) # 27082
(nrow(data_full) - nrow(data_WB)) / nrow(data_full) # 2.8%

# Remove respondents under 18
data_full = filter(data_full, age_raw > 17)

data_WB = filter(data_WB, age_raw > 17)
nrow(data_WB) # 26532
(27082 - 26532) / 27868 # 2.0 % additionally excluded due to age (WB)

################

##### Descriptives: summary statistics (individual-level)

# WB
summary(data_WB$donate_blood) # 0: 16337; 1: 10195
10195 / 26532 # 38.4%

describe(data_WB$age) # mean 51.3; median 52; range 18 - 99
summary(data_WB$gender) # 0: 11653; 1: 14879
summary(data_WB$living_with_partner) # 0: 9260 ; 1: 17216; NA: 56
summary(data_WB$education) # 0: 263; 1: 4388; 2: 11454; 3: 8681; 4: 1289; NA: 457
summary(data_WB$IM) # 0: 9840; 1: 16692
summary(as.factor(data_WB$IM_raw)) # 0: 4269; 1: 16692; NA: 5571
summary(data_WB$EM_get_return) # 0: 25017; 1: 1515
summary(as.factor(data_WB$EM_get_return_raw)) # 0: 19446; 1: 1515; NA: 5571 
summary(data_WB$EM_diff_paying_bills) # 0: 16291; 1: 7135; 2: 2766; NA: 340
summary(data_WB$EM_not_employed) # 0: 12894; 1: 13638
describe(data_WB$soc_financial) # mean: -0.69; sd: 0.2; median: -0.75 
describe(data_WB$soc_financial_raw) # mean: 0.15; sd: 0.1; median: 0.13; range: 0.02 - 0.39
describe(data_WB$cost_children_in_household) # mean: 0.29; sd: 0.7; median: 0
summary(data_WB$cost_type_of_community) # 0: 7228; 1: 11153; 2: 8137; NA: 14
summary(data_WB$WB_incentive_financial) # 0: 21240; 0.5: 1464; 1: 3828

##### Descriptives: summary statistics

descriptives_indiv_level_WB = data_WB%>%
  summarize(percent_donate_blood = round(mean(as.numeric(as.character(donate_blood)), na.rm = T), 2),
            sd_donate_blood = round(sd(as.numeric(as.character(donate_blood)), na.rm = T), 2),
            mean_age = round(mean(age_raw, na.rm = T), 2),
            sd_age = round(sd(age_raw, na.rm = T), 2),
            min_age = round(min(age_raw, na.rm = T), 2),
            max_age = round(max(age_raw, na.rm = T), 2),
            sd_age = round(sd(age_raw, na.rm = T), 2),
            mean_gender = round(mean(as.numeric(as.character(gender)), na.rm = T), 2),
            sd_gender = round(sd(as.numeric(as.character(gender)), na.rm = T), 2),
            mean_living_with_partner = round(mean(as.numeric(as.character(living_with_partner)), na.rm = T), 2),
            sd_living_with_partner = round(sd(as.numeric(as.character(living_with_partner)), na.rm = T), 2),
            mean_education = round(mean(as.numeric(as.character(education)), na.rm = T), 2),
            sd_education = round(sd(as.numeric(as.character(education)), na.rm = T), 2),
            min_education = round(min(as.numeric(as.character(education)), na.rm = T), 2),
            max_education = round(max(as.numeric(as.character(education)), na.rm = T), 2),
            percent_1_IM = round(mean(as.numeric(as.character(IM)), na.rm = T), 2),
            sd_IM = round(sd(as.numeric(as.character(IM)), na.rm = T), 2),
            percent_1_IM_raw = round(mean(IM_raw, na.rm = T), 2),
            sd_IM_raw = round(sd(IM_raw, na.rm = T), 2),
            mean_cost_children_in_household = round(mean(cost_children_in_household, na.rm = T), 2),
            sd_cost_children_in_household = round(sd(cost_children_in_household, na.rm = T), 2),
            min_cost_children_in_household = round(min(cost_children_in_household, na.rm = T), 2),
            max_cost_children_in_household = round(max(cost_children_in_household, na.rm = T), 2),
            mean_cost_type_of_community = round(mean(as.numeric(as.character(cost_type_of_community)), na.rm = T), 2),
            sd_cost_type_of_community = round(sd(as.numeric(as.character(cost_type_of_community)), na.rm = T), 2),
            percent_1_EM_get_return = round(mean(as.numeric(as.character(EM_get_return)), na.rm = T), 2),
            sd_EM_get_return = round(sd(as.numeric(as.character(EM_get_return)), na.rm = T), 2),
            percent_1_EM_get_return_raw = round(mean(as.numeric(as.character(EM_get_return_raw)), na.rm = T), 2),
            sd_EM_get_return_raw = round(sd(as.numeric(as.character(EM_get_return_raw)), na.rm = T), 2),
            mean_EM_diff_paying_bills = round(mean(as.numeric(as.character(EM_diff_paying_bills)), na.rm = T), 2),
            sd_EM_diff_paying_bills = round(sd(as.numeric(as.character(EM_diff_paying_bills)), na.rm = T), 2),
            percent_1_EM_employed = round(mean(as.numeric(as.character(EM_employed)), na.rm = T), 2),
            sd_EM_employed = round(sd(as.numeric(as.character(EM_employed)), na.rm = T), 2))

colSums(!is.na(data_WB))

descriptives_country_level = data_WB%>%
  group_by(country)%>%
  summarize(WB_incentive_financial = round(mean(as.numeric(as.character(WB_incentive_financial))), 2),
            WB_incentive_time = round(mean(as.numeric(as.character(WB_incentive_time))), 2),
            soc_financial = round(mean(soc_financial), 2),
            soc_time = round(mean(soc_time), 2),
            soc_financial_raw = round(mean(soc_financial_raw), 3),
            soc_time_raw = round(mean(soc_time_raw), 3),
            num_resp = n())

###### Test for country-level variation (using proportions test (see https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R6_CategoricalDataAnalysis/R6_CategoricalDataAnalysis6.html))

# calculate sums
country_level_sums = data_WB%>%
  group_by(country)%>%
  summarize(sum_IM = round(sum(as.numeric(as.character(IM))), 2),
            sum_IM_raw = round(sum(as.numeric(as.character(IM_raw)), na.rm=T), 2),
            sum_EM_get_return = round(sum(as.numeric(as.character(EM_get_return))), 2),
            sum_EM_get_return_raw = round(sum(as.numeric(as.character(EM_get_return_raw)), na.rm=T), 2),
            num_resp = n())

num_resp_excl_na = data_WB%>% filter(!is.na(IM_raw))%>%group_by(country)%>%summarize(num_resp = n())

prop.test(country_level_sums$sum_IM, country_level_sums$num_resp)
# X-squared(27) = 1501.5, p < 0.001
prop.test(country_level_sums$sum_IM_raw, num_resp_excl_na$num_resp)
# X-squared(27) = 751.23, p < 0.001
prop.test(country_level_sums$sum_EM_get_return, country_level_sums$num_resp)
# X-squared(27) = 408.58, p < 0.001
prop.test(country_level_sums$sum_EM_get_return_raw, num_resp_excl_na$num_resp)
# X-squared(27) = 428.2, p < 0.001

# SOC sums
prop_test_soc = descriptives_country_level%>%
  mutate(sum_soc_financial = round(soc_financial_raw * num_resp, 0),
         sum_soc_time = round(soc_time_raw * num_resp, 0))

prop.test(prop_test_soc$sum_soc_financial, prop_test_soc$num_resp)
# X-squared(27) = 1970.7, p < 0.001
prop.test(prop_test_soc$sum_soc_time, prop_test_soc$num_resp)
# X-squared(27) = 2436.6, p < 0.001


# B) Are SOC for financial and non-financial incentives different from one another

wilcox.test(descriptives_country_level$soc_financial_raw, descriptives_country_level$soc_time_raw) 
# Mann-Whitney U(n1 = n2 = 28) = 48, p < 0.001)

wilcox.test(descriptives_country_level$soc_time_raw, descriptives_country_level$soc_financial_raw) 
# Mann-Whitney U(n1 = n2 = 28) = 736, p < 0.001


################################################################

##### Mixed-effects models (full sample)

# In some models there was a problem with convergence: 
# error arose only when including age (and random effect, as problem doesn't arise with standard glm)
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# "Rescale and center continuous parameters: It doesn’t necessarily mess up the fit, but large differences 
# in the scales of parameters often lead to problems (especially with calculating standard deviations of 
# fixed effects)"

data_WB$age = scale(data_WB$age_raw)
data_WB$cost_children_in_household = scale(data_WB$cost_children_in_household)
data_WB$soc_financial_normalized = scale(data_WB$soc_financial_raw)
data_WB$soc_time_normalized = scale(data_WB$soc_time_raw)

# Construct data frames for all subsequent models on subsets of data
data_excl_not_willing = filter(data_WB, !is.na(IM_raw))
data_excl_countries_no_fin_inc_wb = filter(data_WB, WB_incentive_financial != 0)
data_excl_not_willing_countries_no_fin_inc_wb = filter(data_excl_not_willing, WB_incentive_financial != 0)

#### 0) Empty models

# 0A) Whole blood
m0_wb = glmer(donate_blood ~ (1|country), data = data_WB, family = "binomial")
summary(m0_wb)
# AIC = 34848.6

# Significance of random effect of country

m0_wb_constrained = glm(donate_blood ~ 1, data = data_WB, family = "binomial") # ~ 1 specifies that we want the intercept-only model (usually the 1 is implicit, e.g. when running y ~ x, this is equivalent to y ~ x + 1)
summary(m0_wb_constrained)
# AIC = 35348

anova(m0_wb, m0_wb_constrained)
# LL-test significant: country random effect improves model fit


#### 1) Demographics

m1_wb = glmer(donate_blood ~ age + gender + living_with_partner + education + (1|country), data = data_WB, family = "binomial")
summary(m1_wb)
# AIC = 33097.4 (< 34848.6 --> demographics improve model fit)
# significant effects: age (older more), gender (male more), living with partner (with partner more), education (higher education more)

### 2) Models with predictors of interest (and interactions): Full sample

m2_wb = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_financial + soc_financial_normalized*WB_incentive_financial + (1|country), data = data_WB, family = "binomial")
summary(m2_wb)
# AIC = 30650.6
# demographics effects same
# significant effects as predicted: IM (higher IM more), soc_financial (more positive SOC more) (marginally significant: EM_get_return more, rural less),
# marginally significant: EM_get_return1:WB_incentive_financial1
# significant effects not predicted: EM_not_employed (higher EM LESS), incentives (incentives = 1 LESS) 

### 3) Models with predictors of interest (and interactions): excluding participants not willing to donate (i.e., who didn't answer IM and EM_get_return))

m3_wb = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_financial + soc_financial_normalized*WB_incentive_financial + (1|country), data = data_excl_not_willing, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_wb)
# convergence issue: used different optimizer
# AIC = 26555.4
# living_with_partner becomes non-significant, o.w. demographics effects same
# significant effects as predicted: IM (higher IM more), soc_financial (more positive SOC more) (marginally: more rural less)
# significant effects not predicted: EM_not_employed (higher EM LESS), incentives (incentives = 1 LESS) 


################################################################

##### Supplementary analysis: Time incentives

### 4) Models with predictors of interest (and interactions): Full sample

m4_wb = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_time + soc_time_normalized*WB_incentive_time + (1|country), data = data_WB, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_wb)
# convergence issue: used different optimizer
# demographics effects same
# significant effects as predicted: IM (higher IM more), EM_employed (higher EM more), type_of_community (more rural less), incentive x SOC: higher if incentive and positive SOC
# marginally significant: EM_get_return1:WB_incentive_time1

### 5) Models with predictors of interest (and interactions): excluding participants not willing to donate (i.e., who didn't answer IM and EM_get_return))

m5_wb = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_time + soc_time_normalized*WB_incentive_time + (1|country), data = data_excl_not_willing, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_wb)
# convergence issue: used different optimizer
# living_with_partner becomes non-significant, o.w. demographics effects same
# significant effects as predicted: IM (higher IM more), EM_employed (higher EM more), incentive x SOC: higher if incentive and positive SOC (marginally significant: more rural less)


################################################################

### Predicted probabilities and plotting

# Convert continuous (scaled) variables to numeric as to prevent "Error: Variable 'X' has been fitted with type "nmatrix.1"
data_WB$age = as.numeric(data_WB$age)
data_WB$soc_time_normalized = as.numeric(data_WB$soc_time_normalized)
data_WB$soc_financial_normalized = as.numeric(data_WB$soc_financial_normalized)
data_WB$cost_children_in_household = as.numeric(data_WB$cost_children_in_household)

#### 1) SOC and incentives interaction predicted probabilities - FINANCIAL

# A) normalized SOC

m_SOC_incentives_financial_normalized <- glmer(donate_blood ~ age + gender + living_with_partner + education + soc_financial_normalized*WB_incentive_financial + (1|country), family="binomial", data=data_WB,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# convergence issue: used different optimizer

newdata <- expand.grid(age = mean(data_WB$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       WB_incentive_financial = c("0", "0.5", "1"),
                       soc_financial_normalized = sort(unique(data_WB$soc_financial_normalized)))

pred_SOC_incentives_financial_normalized = newdata%>%
  mutate(predictions = predict(m_SOC_incentives_financial_normalized, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_financial_normalized = pred_SOC_incentives_financial_normalized%>%
  mutate(WB_incentive_financial = recode(WB_incentive_financial, "0"="no blood operators", "0.5"="some blood operators", "1"="all blood operators"))

# make scatter plot
ggplot(pred_SOC_incentives_financial_normalized, aes(soc_financial_normalized, predictions, color=WB_incentive_financial)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=WB_incentive_financial)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of financial incentive\n (normalized)", color = "financial incentive", title = "(A) Financial incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  ggsave("plots/scatter/pred_SOC_incentives_financial_normalized.png", width = 8, height = 5)

# B) raw SOC

m_SOC_incentives_financial_raw <- glmer(donate_blood ~ age + gender + living_with_partner + education + soc_financial_raw*WB_incentive_financial + (1|country), family="binomial", data=data_WB)

newdata <- expand.grid(age = mean(data_WB$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       WB_incentive_financial = c("0", "0.5", "1"),
                       soc_financial_raw = sort(unique(data_WB$soc_financial_raw)))

pred_SOC_incentives_financial_raw = newdata%>%
  mutate(predictions = predict(m_SOC_incentives_financial_raw, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_financial_raw = pred_SOC_incentives_financial_raw%>%
  mutate(WB_incentive_financial = recode(WB_incentive_financial, "0"="no blood operators", "0.5"="some blood operators", "1"="all blood operators"))

# make scatter plot

ggplot(pred_SOC_incentives_financial_raw, aes(soc_financial_raw, predictions, color=WB_incentive_financial)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=WB_incentive_financial)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of financial incentive", color = "financial incentive", title = "(A) Financial incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  ggsave("plots/scatter/pred_SOC_incentives_financial_raw.png", width = 8, height = 5)


#### 2) SOC and incentives interaction predicted probabilities - TIME

# A) normalized SOC

m_SOC_incentives_time_normalized <- glmer(donate_blood ~ age + gender + living_with_partner + education + soc_time_normalized*WB_incentive_time + (1|country), family="binomial", data=data_WB)

newdata <- expand.grid(age = mean(data_WB$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       WB_incentive_time = c("0", "0.5", "1"),
                       soc_time_normalized = sort(unique(data_WB$soc_time_normalized)))

pred_SOC_incentives_time_normalized = newdata%>%
  mutate(predictions = predict(m_SOC_incentives_time_normalized, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_time_normalized = pred_SOC_incentives_time_normalized%>%
  mutate(WB_incentive_time = recode(WB_incentive_time, "0"="no time incentives", "0.5"="time incentives dependent\non employer", "1"="time incentives independent\nof employer"))

# make scatter plot
ggplot(pred_SOC_incentives_time_normalized, aes(soc_time_normalized, predictions, color=WB_incentive_time)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=WB_incentive_time)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of time incentive\n (normalized)", color = "time incentive", title = "(B) Time incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  ggsave("plots/scatter/pred_SOC_incentives_time_normalized.png", width = 8, height = 5)

# B) raw SOC

m_SOC_incentives_time_raw <- glmer(donate_blood ~ education + age + gender + living_with_partner  + soc_time_raw*WB_incentive_time + (1|country), family="binomial", data=data_WB,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# convergence issue: used different optimizer

newdata <- expand.grid(age = mean(data_WB$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       WB_incentive_time = c("0", "0.5", "1"),
                       soc_time_raw = sort(unique(data_WB$soc_time_raw)))

pred_SOC_incentives_time_raw = newdata%>%
  mutate(predictions = predict(m_SOC_incentives_time_raw, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_time_raw = pred_SOC_incentives_time_raw%>%
  mutate(WB_incentive_time = recode(WB_incentive_time, "0"="no time incentives", "0.5"="time incentives dependent\non employer", "1"="time incentives independent\nof employer"))

# make scatter plot
ggplot(pred_SOC_incentives_time_raw, aes(soc_time_raw, predictions, color=WB_incentive_time)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=WB_incentive_time)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of time incentive", color = "time incentive", title = "(B) Time incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  ggsave("plots/scatter/pred_SOC_incentives_time_raw.png", width = 8, height = 5)


#### 3) EM and incentives interaction predicted probabilities - FINANCIAL

# A) Full sample

m_EM_incentives_financial_full_sample <- glmer(donate_blood ~ age + gender + living_with_partner + education + EM_get_return*WB_incentive_financial + (1|country), family="binomial", data=data_WB,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# convergence issue: used different optimizer

newdata <- expand.grid(age = mean(data_WB$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       WB_incentive_financial = c("0", "0.5", "1"),
                       EM_get_return = c("0", "1"))

pred_EM_incentives_financial_full_sample = newdata%>%
  mutate(predictions = predict(m_EM_incentives_financial_full_sample, newdata, type="response", re.form=NA))

# rename factor levels
pred_EM_incentives_financial_full_sample = pred_EM_incentives_financial_full_sample%>%
  mutate(WB_incentive_financial = recode(WB_incentive_financial, "0"="no blood operators", "0.5"="some blood operators", "1"="all blood operators"))

# make bar plot
ggplot(data=pred_EM_incentives_financial_full_sample, aes(x=EM_get_return, y=predictions, fill=WB_incentive_financial)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = "predicted probability of blood donation", x = "extrinsic motivation (getting sth. in return)", fill = "financial incentive") +
  theme(legend.position="bottom") +
  ggsave("plots/bar/pred_EM_incentives_financial_full_sample.png", width = 6, height = 7)

# B) Sample excluding not willing

m_EM_incentives_financial_excl_not_willing <- glmer(donate_blood ~ age + gender + living_with_partner + education + EM_get_return*WB_incentive_financial + (1|country), family="binomial", data=data_excl_not_willing) #,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

newdata <- expand.grid(age = mean(data_excl_not_willing$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       WB_incentive_financial = c("0", "0.5", "1"),
                       EM_get_return = c("0", "1"))

pred_EM_incentives_financial_excl_not_willing = newdata%>%
  mutate(predictions = predict(m_EM_incentives_financial_excl_not_willing, newdata, type="response", re.form=NA))

# rename factor levels
pred_EM_incentives_financial_excl_not_willing = pred_EM_incentives_financial_excl_not_willing%>%
  mutate(WB_incentive_financial = recode(WB_incentive_financial, "0"="no blood operators", "0.5"="some blood operators", "1"="all blood operators"))

# make bar plot
ggplot(data=pred_EM_incentives_financial_excl_not_willing, aes(x=EM_get_return, y=predictions, fill=WB_incentive_financial)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = "predicted probability of blood donation", x = "extrinsic motivation (getting sth. in return)", fill = "financial incentive") +
  theme(legend.position="bottom") +
  ggsave("plots/bar/pred_EM_incentives_financial_excl_not_willing.png", width = 6, height = 7)


#### 4) EM and incentives interaction predicted probabilities - TIME

# A) Full sample

m_EM_incentives_time_full_sample <- glmer(donate_blood ~ age + gender + living_with_partner + education + EM_get_return*WB_incentive_time + (1|country), family="binomial", data=data_WB,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# convergence issue: used different optimizer

newdata <- expand.grid(age = mean(data_WB$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       WB_incentive_time = c("0", "0.5", "1"),
                       EM_get_return = c("0", "1"))

pred_EM_incentives_time_full_sample = newdata%>%
  mutate(predictions = predict(m_EM_incentives_time_full_sample, newdata, type="response", re.form=NA))

# rename factor levels
pred_EM_incentives_time_full_sample = pred_EM_incentives_time_full_sample%>%
  mutate(WB_incentive_time = recode(WB_incentive_time, "0"="no time incentives", "0.5"="time incentives dependent\non employer", "1"="time incentives independent\nof employer"))

# make bar plot
ggplot(data=pred_EM_incentives_time_full_sample, aes(x=EM_get_return, y=predictions, fill=WB_incentive_time)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = "predicted probability of blood donation", x = "extrinsic motivation (getting sth. in return)", fill = "time incentive") +
  theme(legend.position="bottom") +
  ggsave("plots/bar/pred_EM_incentives_time_full_sample.png", width = 6.5, height = 7)

# B) Sample excluding not willing

m_EM_incentives_time_excl_not_willing <- glmer(donate_blood ~ age + gender + living_with_partner + education + EM_get_return*WB_incentive_time + (1|country), family="binomial", data=data_excl_not_willing) #,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

newdata <- expand.grid(age = mean(data_excl_not_willing$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       WB_incentive_time = c("0", "0.5", "1"),
                       EM_get_return = c("0", "1"))

pred_EM_incentives_time_excl_not_willing = newdata%>%
  mutate(predictions = predict(m_EM_incentives_time_excl_not_willing, newdata, type="response", re.form=NA))

# rename factor levels
pred_EM_incentives_time_excl_not_willing = pred_EM_incentives_time_excl_not_willing%>%
  mutate(WB_incentive_time = recode(WB_incentive_time, "0"="no time incentives", "0.5"="time incentives dependent\non employer", "1"="time incentives independent\nof employer"))

# make bar plot
ggplot(data=pred_EM_incentives_time_excl_not_willing, aes(x=EM_get_return, y=predictions, fill=WB_incentive_time)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = "predicted probability of blood donation", x = "extrinsic motivation (getting sth. in return)", fill = "time incentive") +
  theme(legend.position="bottom") +
  ggsave("plots/bar/pred_EM_incentives_time_excl_not_willing.png", width = 6.5, height = 7)


#### 5) IM predicted probabilities

# A) Full sample

m_IM <- glmer(donate_blood ~ age + gender + living_with_partner + education + IM + (1|country), family="binomial", data=data_WB)
newdata <- with(data_WB, data.frame(gender = "1", age=mean(age, na.rm=TRUE), living_with_partner="1", education = "2", IM = c("0", "1")))

pred_IM_full_sample = newdata%>%
  mutate(predictions = predict(m_IM, newdata, type="response", re.form=NA))

# make bar plot
ggplot(data=pred_IM_full_sample, aes(x=IM, y=predictions)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = "predicted probability of blood donation", x = "intrinsic motivation") +
  ggsave("plots/bar/pred_IM_full_sample.png", width = 4, height = 5)

# B) excluding not willing

m_IM_excl_not_willing <- glmer(donate_blood ~ age + gender + living_with_partner + education + IM + (1|country), family="binomial", data=data_excl_not_willing)
newdata <- with(data_excl_not_willing, data.frame(gender = "1", age=mean(age, na.rm=TRUE), living_with_partner="1", education = "2", IM = c("0", "1")))

pred_IM_excl_not_willing = newdata%>%
  mutate(predictions = predict(m_IM_excl_not_willing, newdata, type="response", re.form=NA))

# make bar plot
ggplot(data=pred_IM_excl_not_willing, aes(x=IM, y=predictions)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = "predicted probability of blood donation", x = "intrinsic motivation") +
  ggsave("plots/bar/pred_IM_excl_not_willing.png", width = 4, height = 5)


################################################################

##### Map plots: 

# Remove NAs for plotting
data_WB_valid_WB= filter(data_WB, !is.na(donate_blood))
data_excl_not_willing_valid = filter(data_excl_not_willing, !is.na(donate_blood))

# Calculate region-level and country-level variables

data_WB_valid_WB_agg_country = data_WB_valid_WB%>%
  mutate(nutslvl= as.double(nutslvl),
         WB_incentive_financial = as.numeric(as.character(WB_incentive_financial)),
         WB_incentive_time = as.numeric(as.character(WB_incentive_time)))%>%
  rename(NUTS_ID = nuts,
         LEVL_CODE = nutslvl)%>%
  group_by(country_code)%>%
  summarize(donate_blood_agg = mean(as.numeric(as.character(donate_blood)), na.rm=T),
            EM_get_return_agg = mean(as.numeric(as.character(EM_get_return)), na.rm=T),
            IM_agg = mean(as.numeric(as.character(IM)), na.rm=T),
            WB_incentive_financial = mean(WB_incentive_financial),
            WB_incentive_time = mean(WB_incentive_time),
            soc_financial_raw = mean(soc_financial_raw),
            soc_time_raw = mean(soc_time_raw))%>%
  ungroup()%>%
  rename(CNTR_CODE = country_code)%>%
  mutate(CNTR_CODE = recode(CNTR_CODE,"GB" = "UK", "GR" = "EL"))


# Extract NUTS-ID for later country plotting (because mapdata$NUTS_ID excludes far out territories that we don't want to plot)
data_for_IDs = data_WB_valid_WB%>%
  mutate(nutslvl= as.double(nutslvl))%>%
  rename(NUTS_ID = nuts, LEVL_CODE = nutslvl)%>%
  group_by(NUTS_ID, LEVL_CODE)%>%
  summarize(n = n())

mapdata_IDs = get_eurostat_geospatial(nuts_level = "all", year = "2010")%>% #important: NUTS labels changed, eurobarometer 2014 uses 2010 nuts labels
  inner_join(data_for_IDs, by= c("NUTS_ID", "LEVL_CODE"))

valid_IDs = select(mapdata_IDs, id)

# Plot values on map

mapdata = get_eurostat_geospatial(nuts_level = "all", year = "2010")%>% #important: NUTS labels changed, eurobarometer 2014 uses 2010 nuts labels
  inner_join(data_WB_valid_WB_agg_country, by= "CNTR_CODE")

mapdata = mapdata%>%
  mutate(donate_blood_agg_buckets = cut_to_classes(donate_blood_agg, n = 7, decimals = 2),
         EM_get_return_agg_buckets = cut_to_classes(EM_get_return_agg, n = 5, decimals = 2),
         IM_agg_buckets = cut_to_classes(IM_agg, n = 5, decimals = 2),
         WB_incentive_financial_buckets = cut_to_classes(WB_incentive_financial, n = 3, decimals = 2),
         WB_incentive_time_buckets = cut_to_classes(WB_incentive_time, n = 3, decimals = 2),
         soc_financial_raw_buckets = cut_to_classes(soc_financial_raw, n = 5, decimals = 2),
         soc_time_raw_buckets = cut_to_classes(soc_time_raw, n = 5, decimals = 2))%>%
  filter(id %in% valid_IDs$id)%>%
  mutate(WB_incentive_financial_buckets = recode(WB_incentive_financial_buckets, "-0.25 ~< 0.25"="no blood operators", "0.25 ~< 0.75"="some blood operators", "0.75 ~< 1.25"="all blood operators"),
         WB_incentive_time_buckets = recode(WB_incentive_time_buckets, "-0.25 ~< 0.25"="no time incentives", "0.25 ~< 0.75"="time incentives dependent\non employer", "0.75 ~< 1.25"="time incentives independent\nof employer"),
         donate_blood_agg_buckets = recode(donate_blood_agg_buckets, "0.23 ~< 0.27"="23% to 27%", "0.27 ~< 0.31"="27% to 31%", "0.31 ~< 0.36"="31% to 36%", "0.36 ~< 0.4"="36% to 40%", "0.4 ~< 0.44"="40% to 44%", "0.44 ~< 0.49"="44% to 49%", "0.49 ~< 0.53"="49% to 53%"),
         EM_get_return_agg_buckets = recode(EM_get_return_agg_buckets, "0.01 ~< 0.04"="1% to 4%", "0.04 ~< 0.06"="4% to 6%", "0.06 ~< 0.08"="6% to 8%", "0.08 ~< 0.1"="8% to 10%", "0.1 ~< 0.13"="10% to 13%"),
         IM_agg_buckets = recode(IM_agg_buckets, "0.41 ~< 0.51"="41% to 51%", "0.51 ~< 0.6"="51% to 60%", "0.6 ~< 0.69"="60% to 69%", "0.69 ~< 0.79"="69% to 79%", "0.79 ~< 0.88"="79% to 88%"),
         soc_financial_raw_buckets = recode(soc_financial_raw_buckets, "0.02 ~< 0.1"="2% to 10%", "0.1 ~< 0.17"="10% to 17%", "0.17 ~< 0.24"="17% to 24%", "0.24 ~< 0.32"="24% to 32%", "0.32 ~< 0.39"="32% to 39%"),
         soc_time_raw_buckets = recode(soc_time_raw_buckets, "0.12 ~< 0.23"="12% to 23%", "0.23 ~< 0.35"="23% to 35%", "0.35 ~< 0.47"="35% to 47%", "0.47 ~< 0.59"="47% to 59%", "0.59 ~< 0.7"="59% to 70%"))

ggplot(mapdata, aes(fill = donate_blood_agg_buckets)) +
  scale_fill_brewer(palette = "Reds") +
  geom_sf(mapping = aes()) +
  labs(fill = "Country-level mean\nblood donation") +
  ggsave("plots/maps/blood_donation_countries.png", width = 7, height = 5.7, units = "in")

ggplot(mapdata, aes(fill = EM_get_return_agg_buckets)) +
  scale_fill_brewer(palette = "Blues") +
  geom_sf(mapping = aes()) +
  labs(fill = "", title= "(B) Country-level mean extrinsic motivation (getting sth. in return)") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  ggsave("plots/maps/EM_get_return_countries.png", width = 6, height = 5.7, units = "in")

ggplot(mapdata, aes(fill = IM_agg_buckets)) +
  scale_fill_brewer(palette = "Blues") +
  geom_sf(mapping = aes()) +
  labs(fill = "", title= "(A) Country-level mean intrinsic motivation") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  ggsave("plots/maps/IM_countries.png", width = 6, height = 5.7, units = "in")

ggplot(mapdata, aes(fill = WB_incentive_financial_buckets)) +
  scale_fill_brewer(palette = "Blues") +
  geom_sf(mapping = aes()) +
  labs(fill = "", title= "(D) Financial incentive offered") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  ggsave("plots/maps/WB_incentive_countries.png", width = 6, height = 5.7, units = "in")

ggplot(mapdata, aes(fill = WB_incentive_time_buckets)) +
  scale_fill_brewer(palette = "Blues") +
  geom_sf(mapping = aes()) +
  labs(fill = "", title= "(A) Time incentive") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  ggsave("plots/maps/WB_incentive_time_countries.png", width = 6, height = 5.7, units = "in")

ggplot(mapdata, aes(fill = soc_financial_raw_buckets)) +
  scale_fill_brewer(palette = "Blues") +
  geom_sf(mapping = aes()) +
  labs(fill = "", title= "(C) Social norm for financial incentives") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  ggsave("plots/maps/soc_financial_countries.png", width = 6, height = 5.7, units = "in")

ggplot(mapdata, aes(fill = soc_time_raw_buckets)) +
  scale_fill_brewer(palette = "Blues") +
  geom_sf(mapping = aes()) +
  labs(fill = "", title= "(B) Social norm for time incentives") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  ggsave("plots/maps/soc_time_countries.png", width = 6, height = 5.7, units = "in")

