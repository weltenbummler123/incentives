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

### 8) Additional exploratory analyses
#       I Time incentives only for employed individuals
#       II nuts-1 region-level analysis



####################################################################

##### Load packages
library("readxl")
library("tidyverse")
library("lme4")
library("haven")
library("sjlabelled")
library("data.table")
library("scales")
library("RColorBrewer")
library("psych")
library("ggrepel")
library("sf")
library("rnaturalearth")
#library("merTools") # not loaded because problems with tidyverse select function

##### Set WD

setwd("/Users/caroline/Desktop/PhD/P1_incentives/analysis")

##### Set seed for reproducibility
set.seed(101)

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
                            social_norm_financial_indiv = qe5a_7,
                            social_norm_time_indiv = qe5a_8,
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
  summarize(soc_financial_raw = mean(social_norm_financial_indiv),
            soc_time_raw = mean(social_norm_time_indiv))%>%
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
         social_norm_financial_indiv = as.factor(social_norm_financial_indiv),
         social_norm_time_indiv = as.factor(social_norm_time_indiv),
         WB_incentive_financial = as.factor(WB_incentive_financial),
         plasma_incentive_financial = as.factor(plasma_incentive_financial),
         WB_incentive_time = as.factor(WB_incentive_time),
         plasma_incentive_time = as.factor(plasma_incentive_time),
         country = as.factor(country))


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
describe(data_WB$soc_time) # mean: -0.15; sd: 0.3; median: -0.25 
describe(data_WB$soc_time_raw) # mean: 0.42; sd: 0.15; median: 0.38; range: 0.12 - 0.7
describe(data_WB$cost_children_in_household) # mean: 0.29; sd: 0.7; median: 0
summary(data_WB$cost_type_of_community) # 0: 7228; 1: 11153; 2: 8137; NA: 14
summary(data_WB$WB_incentive_financial) # 0: 21240; 0.5: 1464; 1: 3828

colSums(!is.na(data_WB))

##### Descriptives: summary statistics (country-level)

descriptives_country_level = data_WB%>%
  group_by(country)%>%
  summarize(donate_blood = round(mean(as.numeric(as.character(donate_blood))), 3),
            IM = round(mean(as.numeric(as.character(IM))), 3),
            EM = round(mean(as.numeric(as.character(EM_get_return))), 3),
            WB_incentive_financial = round(mean(as.numeric(as.character(WB_incentive_financial))), 2),
            WB_incentive_time = round(mean(as.numeric(as.character(WB_incentive_time))), 2),
            soc_financial = round(mean(soc_financial), 2),
            soc_time = round(mean(soc_time), 2),
            soc_financial_raw = round(mean(soc_financial_raw), 3),
            soc_time_raw = round(mean(soc_time_raw), 3),
            num_resp = n())

################################################################

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


# C) Are incentives and social norms correlated

# a) financial incentives and social norms for financial incentives

corr_inc_soc_financial = filter(descriptives_country_level, country != "Germany")

# For a dichotomous categorical variable and a continuous variable you can calculate a Pearson correlation if the categorical variable has a 0/1-coding for the categories. This correlation is then also known as a point-biserial correlation coefficient.
cor.test(corr_inc_soc_financial$soc_financial_raw, corr_inc_soc_financial$WB_incentive_financial, method = "pearson")
# 0.278, p = 0.1598 --> weak/moderate correlation

# Alternative: Calculate ANOVA
corr_inc_soc_financial = mutate(corr_inc_soc_financial, WB_incentive_financial = as.factor(WB_incentive_financial))
summary(aov(corr_inc_soc_financial$soc_financial_raw ~ corr_inc_soc_financial$WB_incentive_financial))
# Same as point-biserial correlation: p = 0.16 --> no significant correlation

# b) time incentives and social norms for time incentives

# Calculate ANOVA (time incentives variable has three levels, therefore not possible to use point-biserial correlation)
corr_inc_soc_time = mutate(descriptives_country_level, WB_incentive_time = as.factor(WB_incentive_time))
summary(aov(corr_inc_soc_time$soc_time_raw ~ corr_inc_soc_time$WB_incentive_time))
# p = 0.041 --> significant correlation

# Calculate correlation efficient by excluding middle level of time incentives (i.e., exclude countries where only some donors receive time incentives), --> then able to use point-biserial correlation and get a coefficient 
corr_inc_soc_time = filter(descriptives_country_level, WB_incentive_time != 0.5)
cor.test(corr_inc_soc_time$soc_time_raw, corr_inc_soc_time$WB_incentive_time, method = "pearson")
# corr = 0.4258, p = 0.0542 --> moderate/strong correlation


################################################################

##### Mixed-effects models (full sample)

# In some models there was a problem with convergence: 
# error arose only when including age (and random effect, as problem doesn't arise with standard glm)
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# "Rescale and center continuous parameters: [...] large differences 
# in the scales of parameters often lead to problems (especially with calculating standard deviations of 
# fixed effects)" --> Thus, we normalize age, number of children in household and the two social norm variables --> This fixed error

data_WB$age = as.numeric(scale(data_WB$age_raw))
data_WB$cost_children_in_household = as.numeric(scale(data_WB$cost_children_in_household))
data_WB$soc_financial_normalized = as.numeric(scale(data_WB$soc_financial_raw))
data_WB$soc_time_normalized = as.numeric(scale(data_WB$soc_time_raw))

# FOR FINANCIAL ANALYSES: Construct data frame without Germany, because it is the only country where incentives = 0.5 (i.e., incentives provided by some blood operators)
# Excluding Germany, regrouping it to incentive = 0 or regrouping it to incentive = 1 yields the same qualitative and quantitative results
data_WB_excl_Germany = filter(data_WB, country != "Germany")

####### I Financial incentives

#### 0) Empty models

# 0A) Whole blood
m0_wb = glmer(donate_blood ~ (1|country), data = data_WB, family = "binomial")
summary(m0_wb)
# AIC = 34848.6

# Significance of random effect of country

m0_wb_constrained = glm(donate_blood ~ 1, data = data_WB, family = "binomial")
summary(m0_wb_constrained)
# AIC = 35348

anova(m0_wb, m0_wb_constrained)
# LL-test significant: country random effect improves model fit
# chi^2(1) = 501.79, p < 0.001

#### 1) Demographics

m1_wb = glmer(donate_blood ~ age + gender + living_with_partner + education + (1|country), data = data_WB, family = "binomial")
summary(m1_wb)
# AIC = 33097.4 (< 34848.6 --> demographics improve model fit)
# significant effects: age (older more), gender (male more), living with partner (with partner more), education (higher education more)

### 2) Models with predictors of interest: Full sample
m2_wb = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_financial + soc_financial_normalized*WB_incentive_financial + (1|country), data = data_WB_excl_Germany, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m2_wb)
# demographics effects are the same as in demographics-only model

# significant effects as predicted: IM (higher IM more), soc_financial (more positive SOC more), cost_type_of_community2 (rural less) (marginally significant: EM_get_return, also in interaction with incentives),
# significant effects not predicted: EM_not_employed (higher EM LESS), incentives (incentives = 1 LESS) 

### 3) Robustness check: Models with predictors of interest, but excluding participants not willing to donate (i.e., who didn't answer IM and EM_get_return))

# Construct data frame excluding respondents who did not answer question regarding motivational factors
data_excl_not_willing = filter(data_WB, !is.na(IM_raw))
data_excl_not_willing_excl_Germany = filter(data_excl_not_willing, country != "Germany")

m3_wb = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_financial + soc_financial_normalized*WB_incentive_financial + (1|country), data = data_excl_not_willing_excl_Germany, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_wb)
# living_with_partner becomes non-significant
# EM_get_return1 becomes non-significant
# EM_diff_paying_bills2 becomes marginally significant (in expected direction)
# o.w. all effects remain the same as in m2


####### II Time incentives

### 4) Models with predictors of interest: Full sample

m4_wb = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_time + soc_time_normalized*WB_incentive_time + (1|country), data = data_WB, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_wb)
# demographics effects same
# significant effects as predicted: IM (higher IM more), EM_employed (higher EM more), incentive x SOC (higher if incentive and positive SOC) (marginally significant: EM_get_return1:WB_incentive_time1; type_of_community (more rural less))

### 5) Models with predictors of interest: excluding participants not willing to donate (i.e., who didn't answer IM and EM_get_return))

m5_wb = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_time + soc_time_normalized*WB_incentive_time + (1|country), data = data_excl_not_willing, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_wb)
# living_with_partner becomes non-significant
# EM_get_return1:WB_incentive_time1 becomes non-significant
# o.w. all effects remain the same as in m4


################################################################

### Predicted probabilities and plotting

#### 1) SOC and incentives interaction predicted probabilities - FINANCIAL

# A) normalized SOC (using m2_wb model above)
newdata <- expand.grid(age = mean(data_WB_excl_Germany$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_WB_excl_Germany$cost_children_in_household, na.rm=TRUE),
                       EM_not_employed = "0",
                       EM_diff_paying_bills = "0",
                       EM_get_return = "0",
                       IM = "1",
                       WB_incentive_financial = c("0", "1"),
                       soc_financial_normalized = sort(unique(data_WB_excl_Germany$soc_financial_normalized)))

pred_SOC_incentives_financial_normalized = newdata%>%
  mutate(predictions = predict(m2_wb, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_financial_normalized = pred_SOC_incentives_financial_normalized%>%
  mutate(WB_incentive_financial = recode(WB_incentive_financial, "0"="no incentive offered", "1"="incentive offered"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
ggplot(pred_SOC_incentives_financial_normalized, aes(soc_financial_normalized, predictions, color=WB_incentive_financial)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=WB_incentive_financial)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of financial incentive\n (normalized)", color = "financial incentive", title = "(A) Financial incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))+
  #facet_grid(. ~ IM)+
  ggsave("plots/scatter/pred_SOC_incentives_financial_normalized.png", width = 8, height = 5)

# B) raw SOC (raw values of social norms, for better interpretability/readability)
m2_wb_raw = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_financial + soc_financial_raw*WB_incentive_financial + (1|country), data = data_WB_excl_Germany, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

newdata <- expand.grid(age = mean(data_WB_excl_Germany$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_WB_excl_Germany$cost_children_in_household, na.rm=TRUE),
                       EM_not_employed = "0",
                       EM_diff_paying_bills = "0",
                       EM_get_return = "0",
                       IM = "1",
                       WB_incentive_financial = c("0", "1"),
                       soc_financial_raw = sort(unique(data_WB_excl_Germany$soc_financial_raw)))

pred_SOC_incentives_financial_raw = newdata%>%
  mutate(predictions = predict(m2_wb_raw, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_financial_raw = pred_SOC_incentives_financial_raw%>%
  mutate(WB_incentive_financial = recode(WB_incentive_financial, "0"="no incentive offered", "1"="incentive offered"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
ggplot(pred_SOC_incentives_financial_raw, aes(soc_financial_raw, predictions, color=WB_incentive_financial)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=WB_incentive_financial)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of financial incentive", color = "financial incentive", title = "(A) Financial incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  #facet_grid(. ~ IM) +
  ggsave("plots/scatter/pred_SOC_incentives_financial_raw.png", width = 8, height = 5)

# Make simplified bar plot with two levels of SOC
pred_SOC_incentives_financial_raw_bar = pred_SOC_incentives_financial_raw%>%
  mutate(soc_financial_raw = ifelse(soc_financial_raw < mean(soc_financial_raw), "low acceptability", "high acceptability"),
         soc_financial_raw = factor(soc_financial_raw, levels = c("low acceptability", "high acceptability")),
         WB_incentive_financial = recode(WB_incentive_financial, "no blood operators" = "not offered", "all blood operators" = "offered"))

ggplot(data=pred_SOC_incentives_financial_raw_bar, aes(x=WB_incentive_financial, y=predictions, fill=soc_financial_raw)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = "predicted probability of blood donation", x = "financial reward", fill = "social norm for acceptability of financial reward") +
  theme(legend.position="bottom")+
  ggsave("plots/bar/pred_SOC_incentives_financial_raw_BAR.png", width = 6.5, height = 6)


#### 2) SOC and incentives interaction predicted probabilities - TIME

# A) normalized SOC (using m4_wb model above)
newdata <- expand.grid(age = mean(data_WB$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_WB$cost_children_in_household, na.rm=TRUE),
                       EM_employed = "1",
                       EM_get_return = "0",
                       IM = "1",
                       WB_incentive_time = c("0", "0.5", "1"),
                       soc_time_normalized = sort(unique(data_WB$soc_time_normalized)))

pred_SOC_incentives_time_normalized = newdata%>%
  mutate(predictions = predict(m4_wb, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_time_normalized = pred_SOC_incentives_time_normalized%>%
  mutate(WB_incentive_time = recode(WB_incentive_time, "0"="no incentive offered", "0.5"="incentive dependent\non employer", "1"="incentive offered"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
ggplot(pred_SOC_incentives_time_normalized, aes(soc_time_normalized, predictions, color=WB_incentive_time)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=WB_incentive_time)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of time incentive\n (normalized)", color = "time incentive", title = "(B) Time incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))+
  #facet_grid(. ~ IM) +
  ggsave("plots/scatter/pred_SOC_incentives_time_normalized.png", width = 8, height = 5)

# B) raw SOC
m4_wb_raw = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_time + soc_time_raw*WB_incentive_time + (1|country), data = data_WB, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

newdata <- expand.grid(age = mean(data_WB$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_WB$cost_children_in_household, na.rm=TRUE),
                       EM_employed = "1",
                       EM_get_return = "0",
                       IM = "1",
                       WB_incentive_time = c("0", "0.5", "1"),
                       soc_time_raw = sort(unique(data_WB$soc_time_raw)))

pred_SOC_incentives_time_raw = newdata%>%
  mutate(predictions = predict(m4_wb_raw, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_time_raw = pred_SOC_incentives_time_raw%>%
  mutate(WB_incentive_time = recode(WB_incentive_time, "0"="no incentive offered", "0.5"="incentive dependent\non employer", "1"="incentive offered"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
ggplot(pred_SOC_incentives_time_raw, aes(soc_time_raw, predictions, color=WB_incentive_time)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=WB_incentive_time)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of time incentive", color = "time incentive", title = "(B) Time incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  #facet_grid(. ~ IM) +
  ggsave("plots/scatter/pred_SOC_incentives_time_raw.png", width = 8, height = 5)

# Make simplified bar plot with two levels of SOC
pred_SOC_incentives_time_raw_bar = pred_SOC_incentives_time_raw%>%
  mutate(soc_time_raw = ifelse(soc_time_raw < mean(soc_time_raw), "low acceptability", "high acceptability"),
         soc_time_raw = factor(soc_time_raw, levels = c("low acceptability", "high acceptability")))

ggplot(data=pred_SOC_incentives_time_raw_bar, aes(x=WB_incentive_time, y=predictions, fill=soc_time_raw)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = "predicted probability of blood donation", x = " ", fill = "social norm for acceptability of time reward") +
  theme(legend.position="bottom")+
  ggsave("plots/bar/pred_SOC_incentives_time_raw_BAR.png", width = 6.5, height = 6)


################################################################

##### Map plots: 

# Map plotting based on tutorial here: https://bhaskarvk.github.io/user2017.geodataviz/notebooks/02-Static-Maps.nb.html#

### Initiation of map data

# Get map data
world <- st_as_sf(rnaturalearth::countries110)
europe <- dplyr::filter(world, region_un=="Europe" & name!='Russia')

# Filter polygons that are part of continental Europe with the help of a bounding box
europe.bbox <- st_polygon(list(
  matrix(c(-25,29,45,29,45,75,-25,75,-25,29),byrow = T,ncol = 2)))

europe.clipped <- suppressWarnings(st_intersection(europe, st_sfc(europe.bbox, crs=st_crs(europe))))

### Calculate country-level data and add to map data 

# Calculate country-level variables
data_WB_maps = data_WB%>%
  group_by(country_code)%>%
  summarize(donate_blood_agg = mean(as.numeric(as.character(donate_blood)), na.rm=T),
            EM_get_return_agg = mean(as.numeric(as.character(EM_get_return)), na.rm=T),
            IM_agg = mean(as.numeric(as.character(IM)), na.rm=T),
            WB_incentive_financial = mean(as.numeric(as.character(WB_incentive_financial))),
            WB_incentive_time = mean(as.numeric(as.character(WB_incentive_time))),
            soc_financial_raw = mean(soc_financial_raw),
            soc_time_raw = mean(soc_time_raw))%>%
  rename(iso_a2 = country_code)%>%
  mutate(iso_a2 = as.character(iso_a2),
         WB_incentive_financial = as.factor(WB_incentive_financial),
         WB_incentive_time = as.factor(WB_incentive_time))%>%
  mutate(WB_incentive_financial = recode(WB_incentive_financial, "0"="no blood operators", "0.5"="some blood operators", "1"="all blood operators"),
         WB_incentive_time = recode(WB_incentive_time, "0"="not offered", "0.5"="offered dependent\non employer", "1"="offered independent\nof employer"))

# Add to map data
map_data = left_join(europe.clipped, data_WB_maps, by="iso_a2")

### Make maps

ggplot(map_data, aes(fill=donate_blood_agg)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name='Country-level mean\nblood donation', direction = -1, labels=scales::percent, option="plasma") +
  labs(x=NULL, y=NULL, title=NULL)+
  ggsave("plots/maps/blood_donation.png", width = 7, height = 5.7, units = "in")

ggplot(map_data, aes(fill=EM_get_return_agg)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name='', direction = -1, labels=scales::percent) +
  labs(x=NULL, y=NULL, title="(B) Country-level mean extrinsic motivation (getting sth. in return)")+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.width = unit(1,"cm"), plot.title = element_text(hjust = 0.5))+
  ggsave("plots/maps/EM_get_return.png", width = 5.7, height = 5.5, units = "in")

ggplot(map_data, aes(fill=IM_agg)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name='', direction = -1, labels=scales::percent) +
  labs(x=NULL, y=NULL, title="(A) Country-level mean intrinsic motivation")+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.width = unit(1,"cm"), plot.title = element_text(hjust = 0.5))+
  ggsave("plots/maps/IM.png", width = 5.7, height = 5.5, units = "in")

ggplot(map_data, aes(fill=soc_financial_raw)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name='', direction = -1, labels=scales::percent) +
  labs(x=NULL, y=NULL, title="(C) Social norm for financial incentives")+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.width = unit(1,"cm"), plot.title = element_text(hjust = 0.5))+
  ggsave("plots/maps/soc_financial.png", width = 5.7, height = 5.5, units = "in")

ggplot(data=subset(map_data, !is.na(soc_time_raw)), aes(fill=soc_time_raw)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name='', direction = -1, labels=scales::percent) +
  labs(x=NULL, y=NULL, title="(B) Social norm for time incentives")+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.width = unit(1.5,"cm"), plot.title = element_text(hjust = 0.5))+
  ggsave("plots/maps/soc_time.png", width = 5.7, height = 5.5, units = "in")

ggplot(data=map_data, aes(fill=WB_incentive_financial)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name= "", discrete=TRUE, na.value = "grey50", direction = -1) +
  labs(x=NULL, y=NULL, title="(D) Financial incentive offered")+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.width = unit(0.7,"cm"), plot.title = element_text(hjust = 0.5))+
  ggsave("plots/maps/incentive_financial.png", width = 5.7, height = 5.5, units = "in")

ggplot(data=map_data, aes(fill=WB_incentive_time)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name= "", discrete=TRUE, na.value = "grey50", direction = -1) +
  labs(x=NULL, y=NULL, title="(A) Time incentive offered")+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.width = unit(0.7,"cm"), plot.title = element_text(hjust = 0.5))+
  ggsave("plots/maps/incentive_time.png", width = 5.7, height = 5.5, units = "in")


##############################################################################

####### Additional exploratory analyses

#### I Time incentives only for employed individuals

data_WB_employed = filter(data_WB, EM_employed == "1")
data_WB_unemployed = filter(data_WB, EM_employed == "0")

m_time_only_employed = glmer(donate_blood ~ age + gender + living_with_partner + education + cost_children_in_household + cost_type_of_community + IM + soc_time_normalized*WB_incentive_time + EM_get_return*WB_incentive_time + (1|country), data = data_WB_employed, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m_time_only_employed)
# significant effects: IM (higher IM more), incentive x SOC interaction

m_time_unemployed = glmer(donate_blood ~ age + gender + living_with_partner + education + cost_children_in_household + cost_type_of_community + IM + soc_time_normalized*WB_incentive_time + EM_get_return*WB_incentive_time + (1|country), data = data_WB_unemployed, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m_time_unemployed)
# significant effects: IM (higher IM more), incentive x SOC interaction

## Same SOC-incentive interaction irrespective of employment status. Likely due to temporal discrepancy.


#### II nuts-1 region-level analysis

# Note: To run region-level analyses, please clear all objects from the workspace, 
# run the code from line 0 to 122 and then continue running code below

# Get nuts level 1 names
nuts_level1_names = read.csv("../data/supp_data/nuts_level1_names.csv")

# Add nuts 1 level to every respondent
data$nuts_level1_code = substr(data$nuts, 1, 3)
data = left_join(data, nuts_level1_names, by = "nuts_level1_code")

# Aggregate social norms from individual responses at nuts-1 level
social_norms_nuts1 = data%>%
  group_by(nuts_level1_code_alt)%>%
  summarize(soc_financial_raw_regions = mean(social_norm_financial_indiv),
            soc_time_raw_regions = mean(social_norm_time_indiv),
            num_resp = n(),
            nuts_level1_code = unique(nuts_level1_code))

# Add social norm data to dataset
data_full = left_join(data, social_norms_nuts1, by=c("nuts_level1_code_alt", "nuts_level1_code"))

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
         social_norm_financial_indiv = as.factor(social_norm_financial_indiv),
         social_norm_time_indiv = as.factor(social_norm_time_indiv),
         WB_incentive_financial = as.factor(WB_incentive_financial),
         plasma_incentive_financial = as.factor(plasma_incentive_financial),
         WB_incentive_time = as.factor(WB_incentive_time),
         plasma_incentive_time = as.factor(plasma_incentive_time),
         country = as.factor(country),
         nuts_level1_code_alt = as.factor(nuts_level1_code_alt),
         nuts_level1_code = as.factor(nuts_level1_code))

# Remove invalid data
data_WB = filter(data_full, !is.na(donate_blood))
data_WB = filter(data_WB, age_raw > 17)
data_WB = filter(data_WB, num_resp > 99) # remove regions with less than 100 respondents

data_WB$age = as.numeric(scale(data_WB$age_raw))
data_WB$cost_children_in_household = as.numeric(scale(data_WB$cost_children_in_household))
data_WB$soc_financial_normalized_regions = as.numeric(scale(data_WB$soc_financial_raw_regions))
data_WB$soc_time_normalized_regions = as.numeric(scale(data_WB$soc_time_raw_regions))
data_WB_excl_Germany = filter(data_WB, country != "Germany") # for analyses regarding financial rewards, exclude Germany, because only country where "some blood operators" offer financial incentives

# Run models

# A) Financial incentives
m_region_level_norms_financial = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_financial + WB_incentive_financial*soc_financial_normalized_regions + (1|country) + (1|nuts_level1_code_alt), data = data_WB_excl_Germany, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m_region_level_norms_financial)
# SOC_region is marginally significant (p = 0.08) with positive effect; WB_incentive_financial1 has significant negative effect 

# m_region_level_norms_financial_with_gender_interactions = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_financial + soc_financial_normalized_regions*WB_incentive_financial + gender*soc_financial_normalized_regions + gender*WB_incentive_financial + (1|country) + (1|nuts_level1_code_alt), data = data_WB_excl_Germany, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# if SOCxgender included, SOCxgender and SOC_region are significant

# B) Time incentives
m_region_level_norms_time = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_time + soc_time_normalized_regions*WB_incentive_time + (1|country) + (1|nuts_level1_code_alt), data = data_WB, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m_region_level_norms_time)
# No effect of SOC_region

# m_region_level_norms_time_with_gender_interactions = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*WB_incentive_time + soc_time_normalized_regions*WB_incentive_time + gender*soc_time_normalized_regions + gender*WB_incentive_time + (1|country) + (1|nuts_level1_code_alt), data = data_WB, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# if SOCxgender included, SOCxgender is significant


## Predicted probabilities and plotting

# 1) Financial incentives

newdata <- expand.grid(age = mean(data_WB_excl_Germany$age, na.rm=TRUE),
                       gender = "1",  # Use 'c("0", "1")' if using 'facet_grid( ~ gender)' below
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_WB_excl_Germany$cost_children_in_household, na.rm=TRUE),
                       EM_not_employed = "0",
                       EM_diff_paying_bills = "0",
                       EM_get_return = "0",
                       IM = "1", 
                       WB_incentive_financial = c("0", "1"),
                       soc_financial_normalized_regions = sort(unique(data_WB_excl_Germany$soc_financial_normalized_regions)))

pred_SOC_incentives_financial_normalized = newdata%>%
  mutate(predictions = predict(m_region_level_norms_financial, newdata, type="response", re.form=NA)) # choose here as model either (a) m_region_level_norms_financial or (b) m_region_level_norms_financial_with_gender_interactions

# rename factor levels
pred_SOC_incentives_financial_normalized = pred_SOC_incentives_financial_normalized%>%
  mutate(WB_incentive_financial = recode(WB_incentive_financial, "0"="no incentive offered", "1"="incentive offered"),
         gender = recode(gender, "0"="male", "1"="female"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
ggplot(pred_SOC_incentives_financial_normalized, aes(soc_financial_normalized_regions, predictions, color=WB_incentive_financial)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=WB_incentive_financial)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of financial incentive\n (normalized; region-level)", color = "financial incentive", title = "(A) Financial incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))+
  #facet_grid(. ~ gender)+
  ggsave("plots/scatter/pred_region_level_norms_financial_w_out_gender_interaction.png", width = 8, height = 6.5)


# 2) Time incentives

newdata <- expand.grid(age = mean(data_WB$age, na.rm=TRUE),
                       gender = "1", # Use 'c("0", "1")' if using 'facet_grid( ~ gender)' below
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_WB$cost_children_in_household, na.rm=TRUE),
                       EM_employed = "1",
                       EM_get_return = "0",
                       IM = "1", 
                       WB_incentive_time = c("0", "0.5", "1"),
                       soc_time_normalized_regions = sort(unique(data_WB$soc_time_normalized_regions)))

pred_region_level_norms_time = newdata%>%
  mutate(predictions = predict(m_region_level_norms_time, newdata, type="response", re.form=NA)) # choose here as model either (a) m_region_level_norms_time or (b) m_region_level_norms_time_with_gender_interactions

# rename factor levels
pred_region_level_norms_time = pred_region_level_norms_time%>%
  mutate(WB_incentive_time = recode(WB_incentive_time, "0"="no incentive offered", "0.5"="incentive dependent\non employer", "1"="incentive offered"),
         gender = recode(gender, "0"="male", "1"="female"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
ggplot(pred_region_level_norms_time, aes(soc_time_normalized_regions, predictions, color=WB_incentive_time)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=WB_incentive_time)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of time incentive (normalized; region-level)", color = "time incentive", title = "(B) Time incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  #facet_grid( ~ gender)+
  ggsave("plots/scatter/pred_region_level_norms_time_w_out_gender_interaction.png", width = 8, height = 6.5)

