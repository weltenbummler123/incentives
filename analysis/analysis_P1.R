######## Analysis script - P1 Social Norms, Incentives, and Prosociality ######## 

##### Overview:

# Preprocessing
# Descriptives
# Tests for country-level variation

### 0) Intercept-only models

### 1) Demographics

### 2) Financial incentives - Models with predictors of interest: Full sample

### 3) Financial incentives - Robustness check: Excluding participants not willing to donate (i.e., who didn't answer IM and EM_get_return))

### 4) Time incentives - Models with predictors of interest: Full sample

### 5) Time incentives - Robustness check: Excluding participants not willing to donate (i.e., who didn't answer IM and EM_get_return))

### 6) Predicted probabilities and plotting (scatter and bar plots)

### 7) Map plots

### 8) Prediction intervals for plots

### 9) Additional exploratory analyses
#       I Time incentives only for employed individuals
#       II nuts-1 region-level analysis


####################################################################

# NOTE: We make use of the Eurobarometer as a secondary data source
# In order to run the script, Eurobarometer 82.2 must be downloaded from 
# doi.org/10.4232/1.12999 (file name ZA5931_v3-0-0.dta) and saved in data/Eurobarometer/

# Full reference: European Commission, Brussels (2018). Eurobarometer 82.2 (2014). GESIS Data Archive, Cologne. ZA5931 Data file Version 3.0.0, https://doi.org/10.4232/1.12999

####################################################################

##### Load packages
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
library("ggpubr")
library("corrplot")

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
eurobarometer_data = dplyr::select(eurobarometer_data_raw, uniqid, nuts, nutslvl, isocntry, qe1_1, qe1_2, qe3_2, qe3_3, qe3_4, qe3_5, d15a, d60, qe5a_7, qe5a_8, d40b, d25, d11, d8r2, d10, d7)
incentive_data = dplyr::select(incentive_data_raw, -WB_incentive_financial_comment, -plasma_incentive_financial_comment, -WB_incentive_time_comment, -plasma_incentive_time_comment, -plasma_incentive_financial, -plasma_incentive_time)%>%
  rename(incentive_financial = WB_incentive_financial, incentive_time = WB_incentive_time)

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
  dplyr::select(-IM_help_people, -IM_alleviate_shortages, -IM_medical_research, -EM_occupation, -marital_status) #, -qe1_1, -qe1_2)

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
         incentive_financial = as.factor(incentive_financial),
         incentive_time = as.factor(incentive_time),
         country = as.factor(country))


################################################################

##### Descriptives: number of respondents

## Raw data: number of respondents
nrow(data_full) # 27868

# Remove invalid data
data = filter(data_full, !is.na(donate_blood))
nrow(data) # 27082
(nrow(data_full) - nrow(data)) / nrow(data_full) # 2.8%

# Remove respondents under 18
data_full = filter(data_full, age_raw > 17)

data = filter(data, age_raw > 17)
nrow(data) # 26532
(27082 - 26532) / 27868 # 2.0 % additionally excluded due to age

##### Descriptives: summary statistics (individual-level)

summary(data$donate_blood) # 0: 16337; 1: 10195
10195 / 26532 # 38.4%

describe(data$age) # mean 51.3; median 52; range 18 - 99
summary(data$gender) # 0: 11653; 1: 14879
summary(data$living_with_partner) # 0: 9260 ; 1: 17216; NA: 56
summary(data$education) # 0: 263; 1: 4388; 2: 11454; 3: 8681; 4: 1289; NA: 457
summary(data$IM) # 0: 9840; 1: 16692
summary(as.factor(data$IM_raw)) # 0: 4269; 1: 16692; NA: 5571
summary(data$EM_get_return) # 0: 25017; 1: 1515
summary(as.factor(data$EM_get_return_raw)) # 0: 19446; 1: 1515; NA: 5571 
summary(data$EM_diff_paying_bills) # 0: 16291; 1: 7135; 2: 2766; NA: 340
summary(data$EM_not_employed) # 0: 12894; 1: 13638
describe(data$soc_financial) # mean: -0.69; sd: 0.2; median: -0.75 
describe(data$soc_financial_raw) # mean: 0.15; sd: 0.1; median: 0.13; range: 0.02 - 0.39
describe(data$soc_time) # mean: -0.15; sd: 0.3; median: -0.25 
describe(data$soc_time_raw) # mean: 0.42; sd: 0.15; median: 0.38; range: 0.12 - 0.7
describe(data$cost_children_in_household) # mean: 0.29; sd: 0.7; median: 0
summary(data$cost_type_of_community) # 0: 7228; 1: 11153; 2: 8137; NA: 14
summary(data$incentive_financial) # 0: 21240; 0.5: 1464; 1: 3828

colSums(!is.na(data))

### Plot correlations

data_variables_of_interest = select(data, donate_blood, gender, age_raw, living_with_partner, education, EM_employed, EM_diff_paying_bills, cost_children_in_household, IM, EM_get_return, cost_type_of_community, incentive_time, soc_financial_raw, soc_time_raw, incentive_financial)%>%
  # delete incentive_financial = 0.5 (only Germany belongs to that level)
  mutate(incentive_financial = ifelse(incentive_financial == "0.5", NA, incentive_financial))%>%
  # make incentive_time binary variable
  mutate(incentive_time_all = as.factor(ifelse(incentive_time == "1", "1", "0")))%>%
  # delete categorical variables with more than two levels
  select(-education, -EM_diff_paying_bills, -cost_type_of_community, -incentive_time)%>%
  # convert all (binary) factor variables to numeric (Note that we calculate a Pearson correlation if a categorical variable has a 0/1-coding (--> point-biserial correlation coefficient))
  mutate_if(is.factor, as.character)%>%
  mutate_if(is.character, as.numeric)%>%
  # Rename as in model
  rename(`Blood donation` = donate_blood,
         `Gender (female = 1)` = gender,
         `Age (years)` = age_raw,
         `Cohabiting` = living_with_partner,
         `Employed` = EM_employed,
         `Number of children in household` = cost_children_in_household,
         `Intrinsic motivation` = IM,
         `Extrinsic motivation (getting sth. in return` = EM_get_return,
         `Financial incentive (all blood operators)` = incentive_financial,
         `Time incentive (all blood operators)` = incentive_time_all,
         `Social norm for financial incentives` = soc_financial_raw,
         `Social norm for time incentives` = soc_time_raw)

correlations = cor(data_variables_of_interest, use = "complete.obs", method = "pearson")
corrplot(correlations, type="lower", col=brewer.pal(n=8, name="RdYlBu"))


##### Descriptives: summary statistics (country-level)

descriptives_country_level = data%>%
  group_by(country)%>%
  summarize(donate_blood = round(mean(as.numeric(as.character(donate_blood))), 3),
            IM = round(mean(as.numeric(as.character(IM))), 3),
            EM = round(mean(as.numeric(as.character(EM_get_return))), 3),
            incentive_financial = round(mean(as.numeric(as.character(incentive_financial))), 2),
            incentive_time = round(mean(as.numeric(as.character(incentive_time))), 2),
            soc_financial = round(mean(soc_financial), 2),
            soc_time = round(mean(soc_time), 2),
            soc_financial_raw = round(mean(soc_financial_raw), 3),
            soc_time_raw = round(mean(soc_time_raw), 3),
            num_resp = n())

#####

# Plot observed country-level means as a function of social norms and incentives

# rename factor levels
descriptives_country_level = descriptives_country_level%>%
  mutate(incentive_financial = recode(incentive_financial, "0"="no blood operators", "0.5"="dependent on blood operator", "1"="all blood operators"),
                          incentive_time = recode(incentive_time, "0"="no blood operators", "0.5"="dependent on employer", "1"="all blood operators"))

descriptives_country_level_fin = filter(descriptives_country_level, country!="Germany")%>%
  mutate(country = ifelse(incentive_financial == "all blood operators", as.character(country), ""),
         incentive_financial = as.factor(as.character(incentive_financial)))

descriptives_country_level_time = mutate(descriptives_country_level, country = ifelse(incentive_time == "all blood operators", as.character(country), ""),
                                         incentive_time  = as.factor(as.character(incentive_time)))

# make scatter plot: financial incentives
descr_fin = ggplot(descriptives_country_level_fin, aes(soc_financial_raw, donate_blood, color=incentive_financial)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=incentive_financial)) +
  geom_point() +
  geom_text(aes(label=country), color = "black", hjust=-0.08, vjust=0) +
  labs(y = "observed mean levels of blood donation", x = "social norm regarding financial incentives", color = "financial incentives offered") + 
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))
  
# make scatter plot: time incentives
descr_time = ggplot(descriptives_country_level_time, aes(soc_time_raw, donate_blood, color=incentive_time)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=incentive_time)) +
  geom_point() +
  geom_text(aes(label=country), color = "black", hjust=-0.08, vjust=0) +
  labs(y = "observed mean levels of blood donation", x = "social norm regarding time incentives", color = "time incentives offered") + 
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

# make combined descriptves plot for paper SI
ggarrange(descr_fin, descr_time, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1) +
  ggsave("plots/scatter/combined_descr.png", width = 13, height = 6, units = "in")


################################################################

###### Test for country-level variation (using proportions test (see https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R6_CategoricalDataAnalysis/R6_CategoricalDataAnalysis6.html))

# calculate sums
country_level_sums = data%>%
  group_by(country)%>%
  summarize(sum_IM = round(sum(as.numeric(as.character(IM))), 2),
            sum_IM_raw = round(sum(as.numeric(as.character(IM_raw)), na.rm=T), 2),
            sum_EM_get_return = round(sum(as.numeric(as.character(EM_get_return))), 2),
            sum_EM_get_return_raw = round(sum(as.numeric(as.character(EM_get_return_raw)), na.rm=T), 2),
            num_resp = n())

num_resp_excl_na = data%>% filter(!is.na(IM_raw))%>%group_by(country)%>%summarize(num_resp = n())

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

corr_inc_soc_financial = filter(descriptives_country_level, country != "Germany")%>%
  mutate(incentive_financial = ifelse(incentive_financial == "all blood operators", 1, 0))

# For a dichotomous categorical variable and a continuous variable you can calculate a Pearson correlation if the categorical variable has a 0/1-coding for the categories. This correlation is then also known as a point-biserial correlation coefficient.
cor.test(corr_inc_soc_financial$soc_financial_raw, corr_inc_soc_financial$incentive_financial, method = "pearson")
# 0.278, p = 0.1598 --> weak/moderate correlation

# Alternative: Calculate ANOVA
corr_inc_soc_financial = mutate(corr_inc_soc_financial, incentive_financial = as.factor(incentive_financial))
summary(aov(corr_inc_soc_financial$soc_financial_raw ~ corr_inc_soc_financial$incentive_financial))
# Same as point-biserial correlation: p = 0.16 --> no significant correlation

# b) time incentives and social norms for time incentives

# Calculate ANOVA (time incentives variable has three levels, therefore not possible to use point-biserial correlation)
corr_inc_soc_time = mutate(descriptives_country_level, incentive_time = as.factor(incentive_time))
summary(aov(corr_inc_soc_time$soc_time_raw ~ corr_inc_soc_time$incentive_time))
# p = 0.041 --> significant correlation

# Calculate correlation efficient by excluding middle level of time incentives (i.e., exclude countries where only some donors receive time incentives), --> then able to use point-biserial correlation and get a coefficient 
corr_inc_soc_time = filter(descriptives_country_level, incentive_time != "dependent on employer")%>%
  mutate(incentive_time = ifelse(incentive_time == "all blood operators", 1, 0))
cor.test(corr_inc_soc_time$soc_time_raw, corr_inc_soc_time$incentive_time, method = "pearson")
# corr = 0.4258, p = 0.0542 --> moderate/strong correlation


################################################################

##### Mixed-effects models (full sample)

# In some models there was a problem with convergence: 
# error arose only when including age (and random effect, as problem doesn't arise with standard glm)
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# "Rescale and center continuous parameters: [...] large differences 
# in the scales of parameters often lead to problems (especially with calculating standard deviations of 
# fixed effects)" --> Thus, we normalize age, number of children in household and the two social norm variables --> This fixed error

data$age = as.numeric(scale(data$age_raw))
data$cost_children_in_household = as.numeric(scale(data$cost_children_in_household))
data$soc_financial_normalized = as.numeric(scale(data$soc_financial_raw))
data$soc_time_normalized = as.numeric(scale(data$soc_time_raw))

# FOR FINANCIAL ANALYSES: Construct data frame without Germany, because it is the only country where incentives = 0.5 (i.e., incentives provided by some blood operators)
# Excluding Germany, regrouping it to incentive = 0 or regrouping it to incentive = 1 yields the same qualitative and quantitative results
data_excl_Germany = filter(data, country != "Germany")

####### I Financial incentives

#### 0) Empty models

# 0A) Only including random effeect
m0 = glmer(donate_blood ~ (1|country), data = data, family = "binomial")
summary(m0)
# AIC = 34848.6

# Significance of random effect of country

m0_constrained = glm(donate_blood ~ 1, data = data, family = "binomial")
summary(m0_constrained)
# AIC = 35348

anova(m0, m0_constrained)
# LL-test significant: country random effect improves model fit
# chi^2(1) = 501.79, p < 0.001

#### 1) Demographics

m1 = glmer(donate_blood ~ age + gender + living_with_partner + education + (1|country), data = data, family = "binomial")
summary(m1)
# AIC = 33097.4 (< 34848.6 --> demographics improve model fit)
# significant effects: age (older more), gender (male more), living with partner (with partner more), education (higher education more)

####### I Financial incentives


### 2) Models with predictors of interest: Full sample
m2 = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*incentive_financial + soc_financial_normalized*incentive_financial + (1|country), data = data_excl_Germany, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m2)
# demographics effects are the same as in demographics-only model

# significant effects as predicted: IM (higher IM more), soc_financial (more positive SOC more), cost_type_of_community2 (rural less) (marginally significant: EM_get_return, also in interaction with incentives),
# significant effects not predicted: EM_not_employed (higher EM LESS), incentives (incentives = 1 LESS) 

### 3) Robustness check: Excluding participants not willing to donate (i.e., who didn't answer IM and EM_get_return))

# Construct data frame excluding respondents who did not answer question regarding motivational factors
data_excl_not_willing = filter(data, !is.na(IM_raw))
data_excl_not_willing_excl_Germany = filter(data_excl_not_willing, country != "Germany")

m3 = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*incentive_financial + soc_financial_normalized*incentive_financial + (1|country), data = data_excl_not_willing_excl_Germany, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3)
# living_with_partner becomes non-significant
# EM_get_return1 becomes non-significant
# EM_diff_paying_bills2 becomes marginally significant (in expected direction)
# o.w. all effects remain the same as in m2


####### II Time incentives

### 4) Models with predictors of interest: Full sample

m4 = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*incentive_time + soc_time_normalized*incentive_time + (1|country), data = data, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4)
# demographics effects same
# significant effects as predicted: IM (higher IM more), EM_employed (higher EM more), incentive x SOC (higher if incentive and positive SOC) (marginally significant: EM_get_return1:incentive_time1; type_of_community (more rural less))

### 5) Robustness check: Excluding participants not willing to donate (i.e., who didn't answer IM and EM_get_return))

m5 = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*incentive_time + soc_time_normalized*incentive_time + (1|country), data = data_excl_not_willing, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5)
# living_with_partner becomes non-significant
# EM_get_return1:incentive_time1 becomes non-significant
# o.w. all effects remain the same as in m4


################################################################

### Predicted probabilities and plotting

#### 1) SOC and incentives interaction predicted probabilities - FINANCIAL

# A) normalized SOC (using m2 model above)
newdata <- expand.grid(age = mean(data_excl_Germany$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_excl_Germany$cost_children_in_household, na.rm=TRUE),
                       EM_not_employed = "0",
                       EM_diff_paying_bills = "0",
                       EM_get_return = "0",
                       IM = "1",
                       incentive_financial = c("0", "1"),
                       soc_financial_normalized = sort(unique(data_excl_Germany$soc_financial_normalized)))

pred_SOC_incentives_financial_normalized = newdata%>%
  mutate(predictions = predict(m2, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_financial_normalized = pred_SOC_incentives_financial_normalized%>%
  mutate(incentive_financial = recode(incentive_financial, "0"="no incentive offered", "1"="incentive offered"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
ggplot(pred_SOC_incentives_financial_normalized, aes(soc_financial_normalized, predictions, color=incentive_financial)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=incentive_financial)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of financial incentive\n (normalized)", color = "financial incentive", title = "(A) Financial incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))# +
  #facet_grid(. ~ IM)+
  #ggsave("plots/scatter/pred_SOC_incentives_financial_normalized.png", width = 8, height = 5)

# B) raw SOC (raw values of social norms, for better interpretability/readability)
m2_raw = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*incentive_financial + soc_financial_raw*incentive_financial + (1|country), data = data_excl_Germany, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

newdata <- expand.grid(age = mean(data_excl_Germany$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_excl_Germany$cost_children_in_household, na.rm=TRUE),
                       EM_not_employed = "0",
                       EM_diff_paying_bills = "0",
                       EM_get_return = "0",
                       IM = "1",
                       incentive_financial = c("0", "1"),
                       soc_financial_raw = sort(unique(data_excl_Germany$soc_financial_raw)))

pred_SOC_incentives_financial_raw = newdata%>%
  mutate(predictions = predict(m2_raw, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_financial_raw = pred_SOC_incentives_financial_raw%>%
  mutate(incentive_financial = recode(incentive_financial, "0"="no blood operators", "1"="all blood operators"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
a = ggplot(pred_SOC_incentives_financial_raw, aes(soc_financial_raw, predictions, color=incentive_financial)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=incentive_financial)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm regarding financial incentives", color = "financial incentives offered") + #, title = "(A) Financial incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) #+
  #facet_grid(. ~ IM) +
  #ggsave("plots/scatter/pred_SOC_incentives_financial_raw.png", width = 8, height = 5)

# Make simplified bar plot with two levels of SOC
pred_SOC_incentives_financial_raw_bar = pred_SOC_incentives_financial_raw%>%
  mutate(soc_financial_raw = ifelse(soc_financial_raw < mean(soc_financial_raw), "low acceptability", "high acceptability"),
         soc_financial_raw = factor(soc_financial_raw, levels = c("low acceptability", "high acceptability")),
         incentive_financial = recode(incentive_financial, "no blood operators" = "not offered", "all blood operators" = "offered"))

ggplot(data=pred_SOC_incentives_financial_raw_bar, aes(x=incentive_financial, y=predictions, fill=soc_financial_raw)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = "predicted probability of blood donation", x = "financial reward", fill = "social norm for acceptability of financial reward") +
  theme(legend.position="bottom")#+
  #ggsave("plots/bar/pred_SOC_incentives_financial_raw_BAR.png", width = 6.5, height = 6)


#### 2) SOC and incentives interaction predicted probabilities - TIME

# A) normalized SOC (using m4 model above)
newdata <- expand.grid(age = mean(data$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data$cost_children_in_household, na.rm=TRUE),
                       EM_employed = "1",
                       EM_get_return = "0",
                       IM = "1",
                       incentive_time = c("0", "0.5", "1"),
                       soc_time_normalized = sort(unique(data$soc_time_normalized)))

pred_SOC_incentives_time_normalized = newdata%>%
  mutate(predictions = predict(m4, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_time_normalized = pred_SOC_incentives_time_normalized%>%
  mutate(incentive_time = recode(incentive_time, "0"="no incentive offered", "0.5"="incentive dependent\non employer", "1"="incentive offered"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
ggplot(pred_SOC_incentives_time_normalized, aes(soc_time_normalized, predictions, color=incentive_time)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=incentive_time)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of time incentive\n (normalized)", color = "time incentive", title = "(B) Time incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) #+
  #facet_grid(. ~ IM) +
  #ggsave("plots/scatter/pred_SOC_incentives_time_normalized.png", width = 8, height = 5)

# B) raw SOC
m4_raw = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*incentive_time + soc_time_raw*incentive_time + (1|country), data = data, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

newdata <- expand.grid(age = mean(data$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data$cost_children_in_household, na.rm=TRUE),
                       EM_employed = "1",
                       EM_get_return = "0",
                       IM = "1",
                       incentive_time = c("0", "0.5", "1"),
                       soc_time_raw = sort(unique(data$soc_time_raw)))

pred_SOC_incentives_time_raw = newdata%>%
  mutate(predictions = predict(m4_raw, newdata, type="response", re.form=NA))

# rename factor levels
pred_SOC_incentives_time_raw = pred_SOC_incentives_time_raw%>%
  mutate(incentive_time = recode(incentive_time, "0"="no blood operators", "0.5"="dependent on employer", "1"="all blood operators"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
b = ggplot(pred_SOC_incentives_time_raw, aes(soc_time_raw, predictions, color=incentive_time)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=incentive_time)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm regarding time incentives", color = "time incentives offered") + #, title = "(B) Time incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) #+
  #facet_grid(. ~ IM) +
  #ggsave("plots/scatter/pred_SOC_incentives_time_raw.png", width = 8, height = 5)

# Make simplified bar plot with two levels of SOC
pred_SOC_incentives_time_raw_bar = pred_SOC_incentives_time_raw%>%
  mutate(soc_time_raw = ifelse(soc_time_raw < mean(soc_time_raw), "low acceptability", "high acceptability"),
         soc_time_raw = factor(soc_time_raw, levels = c("low acceptability", "high acceptability")))

ggplot(data=pred_SOC_incentives_time_raw_bar, aes(x=incentive_time, y=predictions, fill=soc_time_raw)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = "predicted probability of blood donation", x = " ", fill = "social norm for acceptability of time reward") +
  theme(legend.position="bottom")#+
  #ggsave("plots/bar/pred_SOC_incentives_time_raw_BAR.png", width = 6.5, height = 6)


####

# make combined plot
ggarrange(a, b, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2) # +
  #ggsave("plots/scatter/combined_pred_SOC.png", width = 6.8, height = 8.7, units = "in") +
  #ggsave(file="plots/scatter/combined_pred_SOC.pdf", width = 6.8, height = 8.7, units = "in")

# make combined plot
ggarrange(a, b, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1) #+
  #ggsave("plots/scatter/combined_pred_SOC_horizontal.png", width = 13, height = 5.7, units = "in") +
  #ggsave(file="plots/scatter/combined_pred_SOC_horizontal.pdf", width = 13, height = 5.7, units = "in")

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
data_maps = data%>%
  group_by(country_code)%>%
  summarize(donate_blood_agg = mean(as.numeric(as.character(donate_blood)), na.rm=T),
            EM_get_return_agg = mean(as.numeric(as.character(EM_get_return)), na.rm=T),
            IM_agg = mean(as.numeric(as.character(IM)), na.rm=T),
            incentive_financial = mean(as.numeric(as.character(incentive_financial))),
            incentive_time = mean(as.numeric(as.character(incentive_time))),
            soc_financial_raw = mean(soc_financial_raw),
            soc_time_raw = mean(soc_time_raw))%>%
  rename(iso_a2 = country_code)%>%
  mutate(iso_a2 = as.character(iso_a2),
         incentive_financial = as.factor(incentive_financial),
         incentive_time = as.factor(incentive_time))%>%
  mutate(incentive_financial = recode(incentive_financial, "0"="no blood\noperators", "0.5"="some blood\noperators", "1"="all blood\noperators"),
         incentive_time = recode(incentive_time, "0"="no blood\noperators", "0.5"="dependent\non employer", "1"="all blood\noperators"))

# Add to map data
map_data = left_join(europe.clipped, data_maps, by="iso_a2")

### Make maps

ggplot(map_data, aes(fill=donate_blood_agg)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name='Country-level mean\nblood donation', direction = -1, labels=scales::percent, option="plasma") +
  labs(x=NULL, y=NULL, title=NULL)+
  ggsave("plots/maps/blood_donation.png", width = 7, height = 5.7, units = "in")#+
  #ggsave(file="plots/maps/blood_donation.pdf")

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

a = ggplot(data=subset(map_data, sovereignt != "Belarus" & sovereignt != "Ukraine" & sovereignt != "Moldova"), aes(fill=soc_financial_raw)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name='', direction = 1, na.value = "grey92", labels=scales::percent) +
  labs(x=NULL, y=NULL, title="Social norm\nregarding financial incentives")+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.width = unit(1.3,"cm"), plot.title = element_text(hjust = 0.5))#+
  #ggsave("plots/maps/soc_financial.png", width = 4.5, height = 5.5, units = "in")+
  #ggsave(file="plots/maps/soc_financial.pdf", width = 4.5, height = 5.5, units = "in")

b = ggplot(data=subset(map_data, sovereignt != "Belarus" & sovereignt != "Ukraine" & sovereignt != "Moldova"), aes(fill=soc_time_raw)) + 
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name='', direction = 1, na.value = "grey92", labels=scales::percent) +
  labs(x=NULL, y=NULL, title="Social norm\nregarding time incentives")+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.width = unit(1.3,"cm"), plot.title = element_text(hjust = 0.5))#+
  #ggsave("plots/maps/soc_time.png", width = 4.5, height = 5.5, units = "in")+
  #ggsave(file="plots/maps/soc_time.pdf", width = 4.5, height = 5.5, units = "in")

c = ggplot(data=subset(map_data, sovereignt != "Belarus" & sovereignt != "Ukraine" & sovereignt != "Moldova"), aes(fill=incentive_financial)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name= "", discrete=TRUE, direction = 1, na.translate = F) +
  labs(x=NULL, y=NULL, title="Financial incentives offered")+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.width = unit(0.7,"cm"), plot.title = element_text(hjust = 0.5))#+
  #guides(fill = FALSE) +
  #ggsave("plots/maps/incentive_financial.png", width = 4.5, height = 5.5, units = "in")+
  #ggsave(file="plots/maps/incentive_financial.pdf", width = 4.5, height = 5.5, units = "in")

d = ggplot(data=subset(map_data, sovereignt != "Belarus" & sovereignt != "Ukraine" & sovereignt != "Moldova"), aes(fill=incentive_time)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  viridis::scale_fill_viridis(name= "", discrete=TRUE, direction = 1, na.translate = F) +
  labs(x=NULL, y=NULL, title="Time incentives offered")+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.width = unit(0.7,"cm"), plot.title = element_text(hjust = 0.5))#+
  #guides(fill = FALSE) +
  #ggsave("plots/maps/incentive_time.png", width = 4.5, height = 5.5, units = "in")+
  #ggsave(file="plots/maps/incentive_time.pdf", width = 4.5, height = 5.5, units = "in")

# Make combined plot for paper
ggarrange(a, b, c, d, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2) +
  #ggsave("plots/maps/combined_maps.png", width = 6.8, height = 8, units = "in") +
  ggsave(file="plots/maps/combined_maps.pdf", width = 6.8, height = 8, units = "in")


####################

### Generate prediction intervals

# Based on https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html

library("merTools")

# 1) m2_raw

newdata <- expand.grid(age = mean(data_excl_Germany$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_excl_Germany$cost_children_in_household, na.rm=TRUE),
                       EM_not_employed = "0",
                       EM_diff_paying_bills = "0",
                       EM_get_return = "0",
                       IM = "1",
                       incentive_financial = c("0", "1"),
                       soc_financial_raw = sort(unique(data_excl_Germany$soc_financial_raw)),
                       country = "new country")

PI <- predictInterval(merMod = m2_raw, newdata = newdata, level = 0.8, n.sims = 1000, stat = "mean", type="probability", include.resid.var = F)

data_with_PI = bind_cols(newdata, PI)

# rename factor levels
data_with_PI = data_with_PI%>%
  mutate(incentive_financial = recode(incentive_financial, "0"="no blood operators", "1"="all blood operators"),
         gender = recode(gender, "0"="male", "1"="female"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

a_with_CI = ggplot(data_with_PI, aes(x = soc_financial_raw, y=fit, ymin=lwr, ymax=upr, color=incentive_financial)) +
  #stat_smooth(method="glm", formula=y~x, alpha=0.2, size=1.5, aes(fill=incentive_financial, ymin=lwr, ymax=upr)) +
  #geom_point() +
  #geom_errorbar(width=.005)+
  geom_smooth(aes(ymin = lwr, ymax = upr,fill = incentive_financial), stat = "identity") +
  labs(y = "predicted probability of blood donation", x = "social norm regarding financial incentives", color = "financial incentives offered") + #, title = "(A) Financial incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  expand_limits(y = 0)#+
  #ggsave("plots/scatter/pred_SOC_incentives_financial_raw_with_PI_smooth.png", width = 8, height = 5)

# 2) m4_raw

newdata <- expand.grid(age = mean(data$age, na.rm=TRUE),
                       gender = "1", 
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data$cost_children_in_household, na.rm=TRUE),
                       EM_employed = "1",
                       EM_get_return = "0",
                       IM = "1",
                       incentive_time = c("0", "0.5", "1"),
                       soc_time_raw = sort(unique(data$soc_time_raw)),
                       country = "new country")

PI <- predictInterval(merMod = m4_raw, newdata = newdata, level = 0.8, n.sims = 1000, stat = "mean", type="probability", include.resid.var = F)

data_with_PI = bind_cols(newdata, PI)

# rename factor levels
data_with_PI = data_with_PI%>%
  mutate(incentive_time = recode(incentive_time, "0"="no blood operators", "0.5"="dependent on employer", "1"="all blood operators"),
         gender = recode(gender, "0"="male", "1"="female"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

b_with_CI = ggplot(data_with_PI, aes(x = soc_time_raw, y=fit, ymin=lwr, ymax=upr, color=incentive_time)) +
  #stat_smooth(method="glm", formula=y~x, alpha=0.2, size=1.5, aes(fill=incentive_time)) +
  #geom_point() +
  #geom_errorbar(width=.005)+
  geom_smooth(aes(ymin = lwr, ymax = upr, fill = incentive_time), stat = "identity") +
  labs(y = "predicted probability of blood donation", x = "social norm regarding time incentives", color = "time incentives offered") + #, title = "(B) Time incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))+
  expand_limits(y = 0)#+
  #ggsave("plots/scatter/pred_SOC_incentives_time_raw_with_PI_smooth.png", width = 8, height = 5)


# make combined plot for paper
ggarrange(a_with_CI, b_with_CI, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1) +
  #ggsave("plots/scatter/combined_pred_SOC_horizontal_with_PI_smooth.png", width = 13, height = 5.7, units = "in") +
  ggsave(file="plots/scatter/combined_pred_SOC_horizontal_with_PI_smooth.pdf", width = 13, height = 5.7, units = "in")




##############################################################################

####### Additional exploratory analyses

#### I Time incentives only for employed individuals

data_employed = filter(data, EM_employed == "1")
data_unemployed = filter(data, EM_employed == "0")

m_time_only_employed = glmer(donate_blood ~ age + gender + living_with_partner + education + cost_children_in_household + cost_type_of_community + IM + soc_time_normalized*incentive_time + EM_get_return*incentive_time + (1|country), data = data_employed, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m_time_only_employed)
# significant effects: IM (higher IM more), incentive x SOC interaction

m_time_unemployed = glmer(donate_blood ~ age + gender + living_with_partner + education + cost_children_in_household + cost_type_of_community + IM + soc_time_normalized*incentive_time + EM_get_return*incentive_time + (1|country), data = data_unemployed, family = "binomial",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m_time_unemployed)
# significant effects: IM (higher IM more), incentive x SOC interaction

## Same SOC-incentive interaction irrespective of employment status. Likely due to temporal discrepancy.


#### II nuts-1 region-level analysis

warning("Careful! In order to run these region-level analyses, you need to clear the workspace and re-run only lines 0 - 130")
# Note: To run region-level analyses, please clear all objects from the workspace, 
# run the code from line 0 to 130 and then continue running code below

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
         incentive_financial = as.factor(incentive_financial),
         incentive_time = as.factor(incentive_time),
         country = as.factor(country),
         nuts_level1_code_alt = as.factor(nuts_level1_code_alt),
         nuts_level1_code = as.factor(nuts_level1_code))

# Remove invalid data
data = filter(data_full, !is.na(donate_blood))
data = filter(data, age_raw > 17)
data = filter(data, num_resp > 99) # remove regions with less than 100 respondents

data$age = as.numeric(scale(data$age_raw))
data$cost_children_in_household = as.numeric(scale(data$cost_children_in_household))
data$soc_financial_normalized_regions = as.numeric(scale(data$soc_financial_raw_regions))
data$soc_time_normalized_regions = as.numeric(scale(data$soc_time_raw_regions))
data_excl_Germany = filter(data, country != "Germany") # for analyses regarding financial rewards, exclude Germany, because only country where "some blood operators" offer financial incentives

# Run models

# A) Financial incentives
m_region_level_norms_financial = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*incentive_financial + incentive_financial*soc_financial_normalized_regions + (1|country) + (1|nuts_level1_code_alt), data = data_excl_Germany, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m_region_level_norms_financial)
# SOC_region is marginally significant (p = 0.08) with positive effect; incentive_financial1 has significant negative effect 

# m_region_level_norms_financial_with_gender_interactions = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_not_employed + EM_diff_paying_bills + cost_children_in_household + cost_type_of_community + IM + EM_get_return*incentive_financial + soc_financial_normalized_regions*incentive_financial + gender*soc_financial_normalized_regions + gender*incentive_financial + (1|country) + (1|nuts_level1_code_alt), data = data_excl_Germany, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# if SOCxgender included, SOCxgender and SOC_region are significant

# B) Time incentives
m_region_level_norms_time = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*incentive_time + soc_time_normalized_regions*incentive_time + (1|country) + (1|nuts_level1_code_alt), data = data, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m_region_level_norms_time)
# No effect of SOC_region

# m_region_level_norms_time_with_gender_interactions = glmer(donate_blood ~ age + gender + living_with_partner + education + EM_employed + cost_children_in_household + cost_type_of_community + IM + EM_get_return*incentive_time + soc_time_normalized_regions*incentive_time + gender*soc_time_normalized_regions + gender*incentive_time + (1|country) + (1|nuts_level1_code_alt), data = data, family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# if SOCxgender included, SOCxgender is significant


## Predicted probabilities and plotting

# 1) Financial incentives

newdata <- expand.grid(age = mean(data_excl_Germany$age, na.rm=TRUE),
                       gender = "1",  # Use 'c("0", "1")' if using 'facet_grid( ~ gender)' below
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_excl_Germany$cost_children_in_household, na.rm=TRUE),
                       EM_not_employed = "0",
                       EM_diff_paying_bills = "0",
                       EM_get_return = "0",
                       IM = "1", 
                       incentive_financial = c("0", "1"),
                       soc_financial_normalized_regions = sort(unique(data_excl_Germany$soc_financial_normalized_regions)))

pred_SOC_incentives_financial_normalized = newdata%>%
  mutate(predictions = predict(m_region_level_norms_financial, newdata, type="response", re.form=NA)) # choose here as model either (a) m_region_level_norms_financial or (b) m_region_level_norms_financial_with_gender_interactions

# rename factor levels
pred_SOC_incentives_financial_normalized = pred_SOC_incentives_financial_normalized%>%
  mutate(incentive_financial = recode(incentive_financial, "0"="no incentive offered", "1"="incentive offered"),
         gender = recode(gender, "0"="male", "1"="female"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
ggplot(pred_SOC_incentives_financial_normalized, aes(soc_financial_normalized_regions, predictions, color=incentive_financial)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=incentive_financial)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of financial incentive\n (normalized; region-level)", color = "financial incentive", title = "(A) Financial incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))#+
#facet_grid(. ~ gender)+
#ggsave("plots/scatter/pred_region_level_norms_financial_w_out_gender_interaction.png", width = 8, height = 6.5)


# 2) Time incentives

newdata <- expand.grid(age = mean(data$age, na.rm=TRUE),
                       gender = "1", # Use 'c("0", "1")' if using 'facet_grid( ~ gender)' below
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data$cost_children_in_household, na.rm=TRUE),
                       EM_employed = "1",
                       EM_get_return = "0",
                       IM = "1", 
                       incentive_time = c("0", "0.5", "1"),
                       soc_time_normalized_regions = sort(unique(data$soc_time_normalized_regions)))

pred_region_level_norms_time = newdata%>%
  mutate(predictions = predict(m_region_level_norms_time, newdata, type="response", re.form=NA)) # choose here as model either (a) m_region_level_norms_time or (b) m_region_level_norms_time_with_gender_interactions

# rename factor levels
pred_region_level_norms_time = pred_region_level_norms_time%>%
  mutate(incentive_time = recode(incentive_time, "0"="no incentive offered", "0.5"="incentive dependent\non employer", "1"="incentive offered"),
         gender = recode(gender, "0"="male", "1"="female"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))

# make scatter plot
ggplot(pred_region_level_norms_time, aes(soc_time_normalized_regions, predictions, color=incentive_time)) +
  stat_smooth(method="glm", formula=y~x, alpha=0.2, size=2, aes(fill=incentive_time)) +
  geom_point(position=position_jitter(height=0.03, width=0)) + 
  labs(y = "predicted probability of blood donation", x = "social norm for acceptability of time incentive (normalized; region-level)", color = "time incentive", title = "(B) Time incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) #+
#facet_grid( ~ gender)+
#ggsave("plots/scatter/pred_region_level_norms_time_w_out_gender_interaction.png", width = 8, height = 6.5)



### Generate prediction intervals

# 1) m_region_level_norms_financial

newdata <- expand.grid(age = mean(data_excl_Germany$age, na.rm=TRUE),
                       gender = "1",  # Use 'c("0", "1")' if using 'facet_grid( ~ gender)' below
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data_excl_Germany$cost_children_in_household, na.rm=TRUE),
                       EM_not_employed = "0",
                       EM_diff_paying_bills = "0",
                       EM_get_return = "0",
                       IM = "1", 
                       incentive_financial = c("0", "1"),
                       soc_financial_normalized_regions = sort(unique(data_excl_Germany$soc_financial_normalized_regions)),
                       country = "new country",
                       nuts_level1_code_alt = "new region")

PI <- predictInterval(merMod = m_region_level_norms_financial, newdata = newdata, level = 0.8, n.sims = 1000, stat = "mean", type="probability", include.resid.var = F)

data_with_PI = bind_cols(newdata, PI)

# rename factor levels
data_with_PI = data_with_PI%>%
  mutate(incentive_financial = recode(incentive_financial, "0"="no incentive offered", "1"="incentive offered"),
         gender = recode(gender, "0"="male", "1"="female"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))


c_with_CI = ggplot(data_with_PI, aes(x = soc_financial_normalized_regions, y=fit, ymin=lwr, ymax=upr, color=incentive_financial)) +
  #stat_smooth(method="glm", formula=y~x, alpha=0.2, size=1.5, aes(fill=incentive_financial, ymin=lwr, ymax=upr)) +
  #geom_point() +
  #geom_errorbar(width=.005)+
  geom_smooth(aes(ymin = lwr, ymax = upr,fill = incentive_financial), stat = "identity") +
  labs(y = "predicted probability of blood donation", x = "social norm regarding financial incentives", color = "financial incentives offered") + #, title = "(A) Financial incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) #+
  #ggsave("plots/scatter/pred_region_level_norms_financial_w_out_gender_interaction_with_PI_smooth.png", width = 6, height = 5)


# 2) m_region_level_norms_time

newdata <- expand.grid(age = mean(data$age, na.rm=TRUE),
                       gender = "1", # Use 'c("0", "1")' if using 'facet_grid( ~ gender)' below
                       living_with_partner="1", 
                       education = "2", 
                       cost_type_of_community = "1",
                       cost_children_in_household = mean(data$cost_children_in_household, na.rm=TRUE),
                       EM_employed = "1",
                       EM_get_return = "0",
                       IM = "1", 
                       incentive_time = c("0", "0.5", "1"),
                       soc_time_normalized_regions = sort(unique(data$soc_time_normalized_regions)),
                       country = "new country",
                       nuts_level1_code_alt = "new region")

PI <- predictInterval(merMod = m_region_level_norms_time, newdata = newdata, level = 0.8, n.sims = 1000, stat = "mean", type="probability", include.resid.var = F)

data_with_PI = bind_cols(newdata, PI)

# rename factor levels
data_with_PI = data_with_PI%>%
  mutate(incentive_time = recode(incentive_time, "0"="no incentive offered", "0.5"="incentive dependent\non employer", "1"="incentive offered"),
         gender = recode(gender, "0"="male", "1"="female"),
         IM = recode(IM, "0"="not intrinsically motivated", "1"="intrinsically motivated"))


d_with_CI = ggplot(data_with_PI, aes(x = soc_time_normalized_regions, y=fit, ymin=lwr, ymax=upr, color=incentive_time)) +
  #stat_smooth(method="glm", formula=y~x, alpha=0.2, size=1.5, aes(fill=incentive_time)) +
  #geom_point() +
  #geom_errorbar(width=.005)+
  geom_smooth(aes(ymin = lwr, ymax = upr, fill = incentive_time), stat = "identity") +
  labs(y = "predicted probability of blood donation", x = "social norm regarding time incentives", color = "time incentives offered") + #, title = "(B) Time incentives") +
  guides(fill = FALSE) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))#+
  #ggsave("plots/scatter/pred_region_level_norms_time_w_out_gender_interaction_with_PI_smooth.png", width = 6, height = 5)


# make combined plot for paper
ggarrange(c_with_CI, d_with_CI, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2) +
  ggsave("plots/scatter/combined_pred_REGION_SOC_vertical_with_PI_smooth.png", width = 8, height = 12, units = "in") #+
  #ggsave(file="plots/scatter/combined_pred_REGION_SOC_vertical_with_PI_smooth.pdf", width = 8, height = 12, units = "in")



