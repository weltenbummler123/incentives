######## Modeling script - P1 Social Norms, Incentives, and Prosocial Behavior ######## 

##### Load packages
library("tidyverse")

##### Set WD

setwd("/Users/caroline/Desktop/PhD/P1_incentives/modeling")

##### Set seed for reproducibility
set.seed(101)

################################################################

##### I Direct benefit

### Simulating data

IM = seq(0, 1, 0.01)
EM = seq(0, 1, 0.01)
#IM = abs(rnorm(100, mean = 0, sd = 0.5)) # uncomment this and the following line to create predictions based on normally (but strictly positive) distributed values of IM and EM
#EM = abs(rnorm(100, mean = 0, sd = 0.5))
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
  #ggsave("plots/direct_benefit.eps", device="eps", width = 12, height = 6)+
  ggsave("plots/direct_benefit.png", width = 12, height = 6)
  

################################################################

##### II Reputational benefit (according to Benabou and Tirole)

### A) Calculate expected intrinsic and extrinsic motivation assuming uniform distribution of IM and EM

### For each level of R, cost and B: calculate expected intrinsic and extrinsic motivation
expected_motivation_EM = d%>%
  group_by(cost, R, B_if_direct_benefit)%>%
  summarize(E_EM = mean(EM))

expected_motivation_IM = d%>%
  group_by(cost, R, B_if_direct_benefit)%>%
  summarize(E_IM = mean(IM))

# Spread expected motivation
expected_motivation_EM = spread(expected_motivation_EM, B_if_direct_benefit, E_EM)
expected_motivation_IM = spread(expected_motivation_IM, B_if_direct_benefit, E_IM)

expected_motivation_EM = rename(expected_motivation_EM, E_EM_if_B_0 = `not performed`, E_EM_if_B_1 = `performed`)
expected_motivation_IM = rename(expected_motivation_IM, E_IM_if_B_0 = `not performed`, E_IM_if_B_1 = `performed`)

expected_motivation = left_join(expected_motivation_EM, expected_motivation_IM, by = c("cost", "R"))


### B) Implement Benabou & Tiroles's reputational motivation

# Calculate net reputational benefit that results from performing behavior (B=1) vs not (B=0)
expected_motivation_BenTir = expected_motivation%>%
  mutate(rep_benefit_B_0 = E_IM_if_B_0 - E_EM_if_B_0,
         rep_benefit_B_1 = E_IM_if_B_1 - E_EM_if_B_1,
         net_rep_benefit = rep_benefit_B_1 - rep_benefit_B_0)%>%
  select(cost, R, net_rep_benefit)

d_BenTir = left_join(d, expected_motivation_BenTir, by = c("cost", "R"))%>%
  mutate(total_benefit = direct_benefit + net_rep_benefit,
         B = ifelse(total_benefit > 0, 1, 0),
         B = as.factor(B),
         B = recode(B, "0"="not performed", "1"="performed"))


### Plotting
ggplot(d_BenTir, aes(IM, EM, color=B)) +
  geom_point() + 
  labs(y = "Extrinsic motivation", x = "Intrinsic motivation", color = "Prosocial behavior") +
  facet_grid(cost ~ R)+
  scale_color_manual(values=c("grey", "#B80000")) # +
  #ggsave("plots/pred_total_benefit_BenTir.eps", device="eps", width = 12, height = 8)+
  #ggsave("plots/pred_total_benefit_BenTir.png", width = 7, height = 8)
  

################################################################

##### III Reputational benefit modification

### Integration of social norms in calculating net reputational benefit:
# [SOC_EM * E(EM|B=1) + SOC_IM * E(IM|B=1)] - [SOC_EM * E(EM|B=0) + SOC_IM * E(IM|B=0)] 
# (holding VIS, pref_v_v and pref_v_a constant at 1)
# Note: We calculate the NET reputational benefit, because agents perform the behavior if the utility of performing the behavior (B=1) outweighs the utility of not performing the behavior (B=0). Thus, if utility(B=1) - utility(B=0) > 0, the behavior B is performed. For the direct benefit we did not have to calculate the net benefit, because the direct benefit of not performing the behavior is always exactly 0 (thus followed: if direct_benefit(B=1) > 0, the behavior is performed) 

# Add SOC_EM (here with three levels for plotting: -1, 0, 1)
expected_motivation = expand_grid(expected_motivation, SOC_EM = c(-1, 0, 1))

# Add SOC_IM: We assume here that SOC_IM is negligible (indeed, differences in expected intrinsic motivation play a minor role, see below)
expected_motivation$SOC_IM = 0

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
  mutate(SOC_EM = recode(SOC_EM, "-1"="social norm = - 1", "0"="social norm = 0", "1"="social norm = 1"))

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
  ggsave("plots/total_benefit_SOC.png", width = 12, height = 8) +
  ggsave("plots/total_benefit_SOC.eps", device="eps", width = 12, height = 8)


################################################################

##### IV Explore effects of expected intrinsic and extrinsic motivation

## A) Costs and benefits associated with expected intrinsic and extrinsic motivation

# assuming that expected intrinsic motivation is a benefit and expected extrinsic motivation is a cost

explore_benefit_E_IM = expected_motivation_IM%>%
  mutate(benefit_B_1 = E_IM_if_B_1 - E_IM_if_B_0)%>%
  select(cost, R, benefit_B_1)%>%
  spread(R, benefit_B_1)%>%
  rename(benefit_B_1_given_R_0 = `incentive = 0`, benefit_B_1_given_R_1 = `incentive = 1`)%>%
  mutate(cost_R_1 = benefit_B_1_given_R_1 - benefit_B_1_given_R_0)

explore_benefit_E_EM = expected_motivation_EM%>%
  mutate(cost_B_1 = E_EM_if_B_0 - E_EM_if_B_1)%>%
  select(cost, R, cost_B_1)%>%
  spread(R, cost_B_1)%>%
  rename(cost_B_1_given_R_0 = `incentive = 0`, cost_B_1_given_R_1 = `incentive = 1`)%>%
  mutate(cost_R_1 = cost_B_1_given_R_0 + cost_B_1_given_R_1)


### Simulate data with more values of cost (i.e., rerun lines 19 - 85 with different cost parameters)
cost_extended = seq(0.01, 0.99, 0.02)

d_extended = expand.grid(IM, EM, cost_extended, R)
names(d_extended) = c("IM", "EM", "cost_extended", "R")

d_extended = d_extended%>%
  mutate(direct_benefit = IM + (R * EM) - cost_extended,
         B_if_direct_benefit = ifelse(direct_benefit > 0, 1, 0))%>%
  mutate(B_if_direct_benefit = as.factor(B_if_direct_benefit),
         #cost_extended = as.factor(cost_extended),
         R = as.factor(R))%>%
  mutate(B_if_direct_benefit = recode(B_if_direct_benefit, "0"="not performed", "1"="performed"),
         #cost_extended = recode(cost_extended, "0.2"="cost_extended = 0.2", "0.4"="cost_extended = 0.4", "0.6"="cost_extended = 0.6", "0.8"="cost_extended = 0.8"),
         R = recode(R, "0"="incentive = 0", "1"="incentive = 1"))

expected_motivation_EM_extended = d_extended%>%
  group_by(cost_extended, R, B_if_direct_benefit)%>%
  summarize(E_EM = mean(EM))%>%
  spread(B_if_direct_benefit, E_EM)%>%
  rename(E_EM_if_B_0 = `not performed`, E_EM_if_B_1 = `performed`)

expected_motivation_IM_extended = d_extended%>%
  group_by(cost_extended, R, B_if_direct_benefit)%>%
  summarize(E_IM = mean(IM))%>%
  spread(B_if_direct_benefit, E_IM)%>%
  rename(E_IM_if_B_0 = `not performed`, E_IM_if_B_1 = `performed`)

explore_benefit_E_IM_extended = expected_motivation_IM_extended%>%
  mutate(benefit_B_1 = E_IM_if_B_1 - E_IM_if_B_0)%>%
  select(cost_extended, R, benefit_B_1)%>%
  spread(R, benefit_B_1)%>%
  rename(benefit_B_1_given_R_0 = `incentive = 0`, benefit_B_1_given_R_1 = `incentive = 1`)%>%
  mutate(cost_R_1_IM = benefit_B_1_given_R_1 - benefit_B_1_given_R_0)

explore_benefit_E_EM_extended = expected_motivation_EM_extended%>%
  mutate(cost_B_1 = E_EM_if_B_0 - E_EM_if_B_1)%>%
  select(cost_extended, R, cost_B_1)%>%
  spread(R, cost_B_1)%>%
  rename(cost_B_1_given_R_0 = `incentive = 0`, cost_B_1_given_R_1 = `incentive = 1`)%>%
  mutate(cost_R_1_EM = cost_B_1_given_R_0 + cost_B_1_given_R_1)

explore_benefit_E_IM_EM_extended = left_join(explore_benefit_E_IM_extended, explore_benefit_E_EM_extended, by = "cost_extended")%>%
  select(cost_extended, cost_R_1_IM, cost_R_1_EM)%>%
  gather("motivation", "cost_R_1", cost_R_1_IM, cost_R_1_EM)%>%
  mutate(motivation = ifelse(motivation == "cost_R_1_IM", "Intrinsic motivation", "Extrinsic motivation"))

# Plot these differences in reputational motivation 
# (i.e. rep_benefit given B=0 compared to rep_benefit given B=0, for different levels of incentives)

ggplot(explore_benefit_E_IM_EM_extended, aes(cost_extended, cost_R_1, color=motivation)) +
  geom_point() + 
  labs(y = "Reputational costs resulting from offering incentives\nif a prosocial behavior is performed", x = "cost of performing prosocial behavior", color = "Type of motivation") +
  #ggsave("plots/rep_benefit_IM_EM_given_R.eps", device="eps", width = 8, height = 6)+
  ggsave("plots/rep_benefit_IM_EM_given_R.png", width = 8, height = 6)


## B)  Explore effects of expected intrinsic and extrinsic motivation (by themselves, without cost)

expected_motivation_IM_extended_mod = expected_motivation_IM_extended%>%
  mutate(motivation = "Intrinsic motivation",
         diff_E_motivation_B1_B0 = E_IM_if_B_1 - E_IM_if_B_0)%>%
  select(-E_IM_if_B_1, -E_IM_if_B_0)

expected_motivation_EM_extended_mod = expected_motivation_EM_extended%>%
  mutate(motivation = "Extrinsic motivation",
         diff_E_motivation_B1_B0 = E_EM_if_B_1 - E_EM_if_B_0)%>%
  select(-E_EM_if_B_1, -E_EM_if_B_0)

expected_motivation_IM_EM_extended = bind_rows(expected_motivation_IM_extended_mod, expected_motivation_EM_extended_mod)%>%
  mutate(scenario = ifelse(R == "incentive = 1" & motivation == "Intrinsic motivation", "Intrinsic motivation, incentive present",
                           ifelse(R == "incentive = 1" & motivation == "Extrinsic motivation", "Extrinsic motivation, incentive present",
                                  ifelse(R == "incentive = 0" & motivation == "Intrinsic motivation", "Intrinsic motivation, incentive absent",
                                         ifelse(R == "incentive = 0" & motivation == "Extrinsic motivation", "Extrinsic motivation, incentive abstent", NA)))),
         R = ifelse(R == "incentive = 0", "incentive absent", "incentive present"))

ggplot(expected_motivation_IM_EM_extended, aes(cost_extended, diff_E_motivation_B1_B0, color=scenario)) +
  geom_jitter() #+ 
  labs(y = "Difference in expected motivation when a prosocial behavior is performed", x = "cost of performing prosocial behavior", color = "Type of scenario")+
  #ggsave("plots/expected_IM_EM_given_R.eps", device="eps", width = 8, height = 6)+
  #ggsave("plots/expected_IM_EM_given_R.png", width = 8, height = 6)

ggplot(expected_motivation_IM_EM_extended, aes(cost_extended, diff_E_motivation_B1_B0, color=R)) +
  geom_jitter() + 
  facet_grid(. ~ motivation)+
  labs(y = "Difference in expected motivation when a prosocial behavior is performed", x = "cost of performing prosocial behavior", color = "Incentive") #+
  #ggsave("plots/expected_IM_EM_given_R_facet.eps", device="eps", width = 8, height = 6)+
  #ggsave("plots/expected_IM_EM_given_R_facet.png", width = 8, height = 6)


## C) Explore difference in expected IM - expected EM given R

expected_motivation_IM_extended_mod2 = expected_motivation_IM_extended%>%
  mutate(diff_E_IM_B1_B0 = E_IM_if_B_1 - E_IM_if_B_0)%>%
  select(-E_IM_if_B_1, -E_IM_if_B_0)

expected_motivation_EM_extended_mod2 = expected_motivation_EM_extended%>%
  mutate(diff_E_EM_B1_B0 = E_EM_if_B_1 - E_EM_if_B_0)%>%
  select(-E_EM_if_B_1, -E_EM_if_B_0)

expected_motivation_IM_EM_extended2 = left_join(expected_motivation_IM_extended_mod2, expected_motivation_EM_extended_mod2)%>%
  mutate(diff_E_IM_B1_B0_minus_diff_E_EM_B1_B0 = diff_E_IM_B1_B0 - diff_E_EM_B1_B0,
         R = ifelse(R == "incentive = 0", "incentive absent", "incentive present"))

ggplot(expected_motivation_IM_EM_extended2, aes(cost_extended, diff_E_IM_B1_B0_minus_diff_E_EM_B1_B0, color=R)) +
  geom_point() + 
  labs(y = "Difference in expected intrinsic motivation and expected extrinsic motivation \n when a prosocial behavior is performed", x = "cost of performing prosocial behavior", color = "Incentive") #+
  #ggsave("plots/diff_expected_IM_EM_given_R.eps", device="eps", width = 8, height = 6) +
  #ggsave("plots/diff_expected_IM_EM_given_R.png", width = 8, height = 6)
