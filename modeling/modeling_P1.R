######## Modeling script - P1 Social Norms, Incentives, and Prosociality ######## 

##### Load packages
library("tidyverse")

##### Set WD

setwd("/Users/caroline/Desktop/PhD/P1_incentives/modeling")

##### Set seed for reproducibility
set.seed(101)

################################################################

##### I Direct benefit

### Simulating data

IM = seq(0, 1, 0.005)
EM = seq(0, 1, 0.005)
cost = c(0.2, 0.4, 0.6, 0.8)
R = c(0,1)

d = expand.grid(IM, EM, cost, R)
names(d) = c("IM", "EM", "cost", "R")

### Model implementation: Direct benefit

d$direct_benefit = d$IM + (d$R * d$EM) - d$cost

### Predicting behavior

d$B_if_direct_benefit = ifelse(d$direct_benefit > 0, 1, 0)

### Formatting

# Convert to factor and rename factor levels
d = d%>%
  mutate(B_if_direct_benefit = as.factor(B_if_direct_benefit),
         cost = as.factor(cost),
         R = as.factor(R))%>%
  mutate(B_if_direct_benefit = recode(B_if_direct_benefit, "0"="not performed", "1"="performed"),
         cost = recode(cost, "0.2"="cost = 0.2", "0.4"="cost = 0.4", "0.6"="cost = 0.6", "0.8"="cost = 0.8"),
         R = recode(R, "0"="incentive = 0", "1"="incentive = 1"))

# Format data for threshold lines

data_vline <- data.frame(cost = c("cost = 0.2", "cost = 0.4", "cost = 0.6", "cost = 0.8" ), c_levels = c(0.2, 0.4, 0.6, 0.8))
data_abline <- data.frame(R = "incentive = 1", cost = c("cost = 0.2", "cost = 0.4", "cost = 0.6", "cost = 0.8" ), c_levels = c(0.2, 0.4, 0.6, 0.8))

### Plotting

ggplot(d, aes(IM, EM, color=B_if_direct_benefit)) +
  geom_point() + 
  labs(y = "Extrinsic motivation", x = "Intrinsic motivation", color = "Prosocial behavior") +
  facet_grid(R ~ cost) +
  geom_vline(data=data_vline, aes(xintercept=c_levels-0.01), color="yellow", size = 0.8) +
  geom_abline(data=data_abline, aes(intercept=c_levels-0.01, slope=-1), color="green", size = 1) + 
  scale_color_manual(values=c("grey", "#B80000")) +
  ggsave("plots/direct_benefit.png", width = 12, height = 6)

################################################################

##### II Reputational benefit

### For each level of R, cost and B: calculate expected intrinsic and extrinsic motivation
expected_motivation_EM = d%>%
  group_by(cost, R, B_if_direct_benefit)%>%
  summarize(E_EM = mean(EM))

expected_motivation_IM = d%>%
  group_by(cost, R, B_if_direct_benefit)%>%
  summarize(E_IM = mean(IM))

### Calculate net reputational benefit:
# [SOC_EM * E(EM|B=1) + SOC_IM * E(IM|B=1)] - [SOC_EM * E(EM|B=0) + SOC_IM * E(IM|B=0)] 
# (holding VIS, pref_v_v and pref_v_a constant at 1)
# Note: We calculate the NET reputational benefit, because agents perform the behavior if the utility of performing the behavior (B=1) outweighs the utility of not performing the behavior (B=0). Thus, if utility(B=1) - utility(B=0) > 0, the behavior B is performed. For the direct benefit we did not have to calculate the net benefit, because the direct benefit of not performing the behavior is always exactly 0 (thus followed: if direct_benefit(B=1) > 0, the behavior is performed) 

# Spread expected motivation
expected_motivation_EM = spread(expected_motivation_EM, B_if_direct_benefit, E_EM)
expected_motivation_IM = spread(expected_motivation_IM, B_if_direct_benefit, E_IM)

expected_motivation_EM = rename(expected_motivation_EM, E_EM_if_B_0 = `not performed`, E_EM_if_B_1 = `performed`)
expected_motivation_IM = rename(expected_motivation_IM, E_IM_if_B_0 = `not performed`, E_IM_if_B_1 = `performed`)

expected_motivation = left_join(expected_motivation_EM, expected_motivation_IM, by = c("cost", "R"))

# Add SOC_EM (here with three levels for plotting: -1, 0, 1)
expected_motivation = expand_grid(expected_motivation, SOC_EM = c(-1, 0, 1))

# Add SOC_IM: We assume here that there is a moderately positive norm regarding being perceived as intrinsically motivated: SOC_IM = 0.5
expected_motivation$SOC_IM = 0.3

# Calculate hypothetical reputational benefits if behavior were (B=1) or were not (B=0) performed
expected_motivation = expected_motivation%>%
  mutate(rep_benefit_B_0 = E_EM_if_B_0 * SOC_EM + E_IM_if_B_0 * SOC_IM,
         rep_benefit_B_1 = E_EM_if_B_1 * SOC_EM + E_IM_if_B_1 * SOC_IM)

# Calculate NET hypothetical reputational benefits
expected_motivation$net_rep_benefit = expected_motivation$rep_benefit_B_1 - expected_motivation$rep_benefit_B_0

### Integrate net reputational benefit and predict behavior

# Select relevant columns, rename SOC levels and add reputational benefit info to main data frame
expected_motivation = expected_motivation%>%
  select(cost, R, SOC_EM, net_rep_benefit)%>%
  mutate(SOC_EM = as.factor(SOC_EM))%>%
  mutate(SOC_EM = recode(SOC_EM, "-1"="social norm = − 1", "0"="social norm = 0", "1"="social norm = 1"))
  
d = left_join(d, expected_motivation, by = c("cost", "R"))

# Calculate total benefit
d$total_benefit = d$direct_benefit + d$net_rep_benefit

# Predict behavior
d$B = ifelse(d$total_benefit > 0, 1, 0)

### Formatting

# Convert to factor and rename factor levels
d = d%>%
  mutate(B = as.factor(B))%>%
  mutate(B = recode(B, "0"="not performed", "1"="performed"))

# Format data for threshold lines
data_vline <- data.frame(cost = c("cost = 0.2", "cost = 0.4", "cost = 0.6", "cost = 0.8" ), c_levels = c(0.2, 0.4, 0.6, 0.8))
data_abline <- filter(expected_motivation, R =="incentive = 1")%>%
  mutate(cost_num = c(0.2, 0.2, 0.2, 0.4, 0.4, 0.4, 0.6, 0.6, 0.6, 0.8, 0.8, 0.8))%>%
  mutate(intercept_abline = cost_num - net_rep_benefit)%>%
  select(-R, -cost_num, -net_rep_benefit)

### Plotting (only for incentive = 1)
ggplot(d, aes(IM, EM, color=B)) +
  geom_point() + 
  labs(y = "Extrinsic motivation", x = "Intrinsic motivation", color = "Prosocial behavior") +
  facet_grid(SOC_EM ~ cost) +
  geom_vline(data=data_vline, aes(xintercept=c_levels-0.01), color="yellow", size = 0.8) +
  geom_abline(data=data_abline, aes(intercept=intercept_abline-0.01, slope=-1), color="green", size = 1) + 
  scale_color_manual(values=c("grey", "#B80000")) +
  ggsave("plots/total_benefit_SOC.png", width = 12, height = 8)


