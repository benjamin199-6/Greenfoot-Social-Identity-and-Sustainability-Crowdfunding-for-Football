# Clear existing workspace to start fresh
rm(list = ls())

setwd("/Users/rapha/Library/CloudStorage/OneDrive-WUWien/MY DOCS/RESEARCH/Greenfoot")

# Load necessary libraries
library(haven)
library(dplyr)
library(tidyr)
library("wesanderson")
library(ggplot2)

data <- read_sav("Greenfoot_Survey_FullSample (1).sav")

filter = data$PROLIFIC_PID

data <- data %>% rename(Last_desicion = Round)

# Convert investment columns to numeric and replace NA with 0
data$Investment1 <- as.numeric(data$Investment1)
data$Investment1[is.na(data$Investment1)] <- 0

data$Investment2 <- as.numeric(data$Investment2)
data$Investment2[is.na(data$Investment2)] <- 0

data$Investment3 <- as.numeric(data$Investment3)
data$Investment3[is.na(data$Investment3)] <- 0

data$Investment4 <- as.numeric(data$Investment4)
data$Investment4[is.na(data$Investment4)] <- 0

data <- data %>%
  mutate(ID = row_number())

data <- data %>%
  mutate(Comprehension_Combined = coalesce(Comprehension_1, Comprehension_2, Q220, Q218, Q241, Q262))

data <- data %>%
  mutate(Understanding_Instructions_Combined = coalesce(Q92, Q223, Q244, Q265))

data <- data %>%
  mutate(Group_Members_Combined = coalesce(Q292, Q297, Q300, Q304))

data <- data %>%
  mutate(Payoff_Combined = coalesce(Comprehension_1, Q220, Q241, Q262))

data <- data %>%
  mutate(CO2_Offset_Combined = coalesce(Neutral_Random_Instr, Neutral_Club_Instruc, Q218, Q223))

data <- data %>%
  select(-Comprehension_1, -Comprehension_2, -Q220, -Q218, -Q241, -Q262,
         -Q92, -Q223, -Q244, -Q265,
         -Q292, -Q297, -Q300, -Q304,
         -Neutral_Random_Instr, -Neutral_Club_Instruc, -Q218, -Q223)

# Rename the average guess column
data <- data %>% rename(AG = averageGuess)

# Read in the Order data
Order <- read_sav("Greenfoot Survey Pence_May_Order.sav")
Order$id <- Order$PROLIFIC_PID
Order = Order %>% filter(id %in% filter)


# Select the relevant columns for order
order_columns <- Order %>%
  select(id, starts_with("FL_60_DO_FootballClubTreatment"),
         starts_with("FL_43_DO_FootballRandom"),
         starts_with("FL_70_DO_NeutralRandom"),
         starts_with("FL_84_DO_NeutralClub"),
         starts_with("FL_87_DO_NeutralRandom"))

# Pivot the order data into long format
order_long <- order_columns %>%
  pivot_longer(
    cols = -id,
    names_to = "OrderType",
    values_to = "Order"
  )

# Extract the order number from the column names
order_long <- order_long %>%
  mutate(For_Investment = case_when(
    grepl("1$", OrderType) ~ 1,
    grepl("2$", OrderType) ~ 2,
    grepl("3$", OrderType) ~ 3,
    grepl("4$", OrderType) ~ 4
  ))

# Filter out NA values
order_long <- order_long %>%
  filter(!is.na(Order))

# Separate belief columns into individual columns
separate_belief_columns <- function(df, belief_col, new_col_prefix) {
  df %>%
    separate(
      col = belief_col,
      into = paste0(new_col_prefix, "_", c("0", "1_99", "100_199", "200_299", "300")),
      sep = ",",
      convert = TRUE
    )
}

# Apply the function to Belief1, Belief2, Belief3, and Belief4
data <- data %>%
  separate_belief_columns("Belief1", "Belief1") %>%
  separate_belief_columns("Belief2", "Belief2") %>%
  separate_belief_columns("Belief3", "Belief3") %>%
  separate_belief_columns("Belief4", "Belief4")

# Make data long
data_long <- data %>%
  pivot_longer(
    cols = starts_with("Investment") | starts_with("AverageGuess"),
    names_to = c(".value", "Round"),
    names_pattern = "(Investment|AverageGuess)(\\d+)"
  )

table(data_long$Round)

# Create the Bonus(1if25%) and Threshold_high(1=3000pence) variables
data_long <- data_long %>%
  mutate(
    Bonus = case_when(
      Round == 1 ~ 0,
      Round == 2 ~ 0,
      Round == 3 ~ 1,
      Round == 4 ~ 1
    ),
    Threshold_high = case_when(
      Round == 1 ~ 0,
      Round == 2 ~ 1,
      Round == 3 ~ 0,
      Round == 4 ~ 1
    )
  )

# Combine belief columns 
data_long <- data_long %>%
  mutate(
    B_0 = coalesce(Belief1_0, Belief2_0, Belief3_0, Belief4_0),
    B_1_99 = coalesce(Belief1_1_99, Belief2_1_99, Belief3_1_99, Belief4_1_99),
    B_100_199 = coalesce(Belief1_100_199, Belief2_100_199, Belief3_100_199, Belief4_100_199),
    B_200_299 = coalesce(Belief1_200_299, Belief2_200_299, Belief3_200_299, Belief4_200_299),
    B_300 = coalesce(Belief1_300, Belief2_300, Belief3_300, Belief4_300)
  )

# Calculate the total sum of all B columns
data_long$Total_B <- data_long$B_0 + data_long$B_1_99 + data_long$B_100_199 + data_long$B_200_299 + data_long$B_300

# Calculate the sum of B_200_299 and B_300
data_long$B_high_sum <- data_long$B_200_299 + data_long$B_300

# Calculate the relative frequency of B_200_299 and B_300
data_long$B_high <- data_long$B_high_sum / data_long$Total_B

filter = data_long$PROLIFIC_PID
order_long = order_long %>% filter(id %in% filter)

# Check for NA values in key columns
data_long <- data_long %>%
  filter(!is.na(PROLIFIC_PID) & !is.na(Round))

order_long <- order_long %>%
  filter(!is.na(id) & !is.na(For_Investment))

# Merge
data_long <- merge(data_long, order_long, by.x = c("PROLIFIC_PID", "Round"), by.y = c("id", "For_Investment"), all = TRUE)

# Check the number of observations after merge
print(nrow(data_long))


table(is.na(Order$PROLIFIC_PID))


data_long <- data_long %>%
  select(-c(EndDate, StartDate, Status, IPAddress, Progress, Duration__in_seconds_, Finished,
            RecordedDate, ResponseId, RecipientLastName, RecipientFirstName, RecipientEmail,
            ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, 
            UserLanguage, Threshold1, Threshold2, Threshold3, Threshold4,
            Bonus1, Bonus2, Bonus3, Bonus4, EmpiricalBelief1, NormBelief1, Submissionid, Customstudytncsacceptedat,
            Startedat, Completedat, Reviewedat, Archivedat, Completioncode, Totalapprovals,
            Watchingsports,Ethnicitysimplified, Countryofbirth, Countryofresidence, 
            Nationality, Language, Studentstatus, Employmentstatus))


data_long <- data_long %>%
  rename(                                       
    
    
    HouseholdIncome = Household_Income,                      # Household Income
    AreaOfResidence = Q75,                                   # Area of Residence
    EducationLevel = Q285,                                   # Highest Level of Education
    InterestInFootball = Interest_in_Football,               # Interest in Football
    FavoriteFootballTeam = Q1,                               # Favorite Football Team
    SimilarityToGroupMembers = Q50,                          # Similarity to Group Members
    WillingnessToTakeRisks = Q196,                           # Willingness to Take Risks
    WillingnessToContributeIncome = Q96,                     # Willingness to Contribute Income
    Concern_ClimateChange = Q86,                             # Concern about Climate Change
    Importance_SavingEnergy = Q276_1,                        # Importance of Saving Energy
    Importance_FootballClubsEnergyEfficiency = Q276_2,       # Importance of Football Clubs' Energy Efficiency
    Importance_ClubLeadershipInSustainability = Q276_3,      # Importance of Club Leadership in Sustainability
    Awareness_ClubEnergyEfficiencyInitiatives = Q277_1,      # Awareness of Club's Energy Efficiency Initiatives
    Awareness_ClubConsideringEnergyMeasures = Q277_2,        # Awareness of Club Considering Energy Measures
    ParticipationInCrowdfunding = Q278,                      # Participation in Crowdfunding
    LikelihoodToSupport_CrowdfundingEnergyProjects = Q279_1, # Likelihood to Support Crowdfunding Energy Projects
    LikelihoodToInvest_ClubEnergyProjects = Q279_2,          # Likelihood to Invest in Club Energy Projects
    LikelihoodToParticipate_CrowdfundingCampaign = Q279_3,   # Likelihood to Participate in Crowdfunding Campaign
    Concern_MismanagementOfFunds = Q280_1,                   # Concern about Mismanagement of Funds
    Concern_LackOfTransparency = Q280_2,                     # Concern about Lack of Transparency
    Concern_InsufficientProjectInformation = Q280_3,         # Concern about Insufficient Project Information
    Concern_FinancialRisk = Q280_4,                          # Concern about Financial Risk
    Concern_Other = Q280_5,                                  # Other Concerns
    OtherConcerns_Text = Q280_5_TEXT,                        # Other Concerns Text
    Agreement_SenseOfCommunity = Q281_1,                     # Agreement: Sense of Community
    Agreement_FanEngagementCrowdfunding = Q281_2,            # Agreement: Fan Engagement in Crowdfunding
    Score = SC0,                                             # Score
    PROLIFIC_PID = PROLIFIC_PID,                             # PROLIFIC_PID
    Keep = Keep,                                             # Keep,
    Comprehension_Combined = Comprehension_Combined,         # Comprehension Combined
    Understanding_Instructions_Combined = Understanding_Instructions_Combined, # Understanding Instructions Combined
    Group_Members_Combined = Group_Members_Combined,         # Group Members Combined
    Payoff_Combined = Payoff_Combined,                       # Payoff Combined
    CO2_Offset_Combined = CO2_Offset_Combined                # CO2 Offset Combined
  )



data_long <- data_long %>% 
  mutate(
    Treatment_new = ifelse(Treatment == "Control-Small", "Neutral-Random", Treatment),
    Treatment_new = as.factor(Treatment_new),
    GroupComposition = ifelse(grepl("Club", Treatment_new), 1, ifelse(grepl("Random", Treatment_new), 0, NA)),
    ProjectType = ifelse(grepl("Football", Treatment_new), 1, ifelse(grepl("Neutral", Treatment_new), 0, NA)),
    GroupComposition = as.factor(GroupComposition),
    ProjectType = as.factor(ProjectType)
  )

table(data_long$Treatment_new,data_long$Round)

# Set the reference level for Treatment_new
data_long$Treatment_new <- relevel(factor(data_long$Treatment_new), ref = "Neutral-Random")

data_long_filter=data_long %>% filter(Treatment_new=="Football-Club")




summary_table <- data_long %>%
  group_by(GroupComposition, ProjectType, Bonus, Threshold_high) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE),
  ) %>%
  ungroup()

# View the summary table
print(summary_table)

# Load necessary libraries
library(dplyr)
library(tidyr)

names(data_long)

data_long$GroupComposition
# Create variables for thresholds, bonuses, and themes
data_long <- data_long %>%
  mutate(
    Threshold_high = ifelse(Threshold_high == "1", "yes", "no"),
    Threshold_low = ifelse(Threshold_high == "0", 1, 0),
    Bonus_yes = ifelse(Bonus == "1", 1, 0),
    Bonus_no = ifelse(Bonus == "0", 1, 0),
    GroupType = ifelse(GroupComposition == "1", "High Social", "Low Social"),
    Theme = ifelse(ProjectType == "1", "Football", "Neutral"),
    Bonus_ = ifelse(Bonus == "1", "yes", "no"))

# Create the summary table
summary_table <- data_long %>%
  group_by(GroupType, Theme, Threshold_high, Threshold_low, Bonus_yes, Bonus_no) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Mean_SD = paste0(round(Mean_Investment, 2), " (", round(SD_Investment, 2), ")")
  ) %>%
  select(GroupType, Theme, Threshold_high, Threshold_low, Bonus_yes, Bonus_no, Mean_SD)

# Combine the grouping variables into a single column to ensure unique column names in pivot_wider
summary_table <- summary_table %>%
  mutate(
    Grouping = paste0("Threshold_", Threshold_high, "_", Threshold_low, "_Bonus_", Bonus_yes, "_", Bonus_no)
  ) %>%
  select(-Threshold_high, -Threshold_low, -Bonus_yes, -Bonus_no)

# Pivot the table to the desired format
pivot_table <- summary_table %>%
  pivot_wider(
    names_from = Grouping,
    values_from = Mean_SD
  )

# Print the pivot table
print(pivot_table)




# library(writexl)
# write_xlsx(pivot_table, path = "summary_table.xlsx")

data_long$WillingnessToTakeRisks=as.numeric(data_long$WillingnessToTakeRisks)
data_long$AverageGuess=as.numeric(data_long$AverageGuess)
data_long$Bonus=as.factor(data_long$Bonus)
data_long$Bonus_=as.factor(data_long$Bonus_)
data_long$Threshold_high=as.factor(data_long$Threshold_high)
data_long$SimilarityToGroupMembers<-as.numeric(data_long$SimilarityToGroupMembers)
data_long <- data_long %>%
  dplyr::mutate(SimilarityToGroupMembers = dplyr::recode(SimilarityToGroupMembers,
                                                         `1` = "1",
                                                         `3` = "2",
                                                         `4` = "3",
                                                         `5` = "4",
                                                         `28` = "very similar"))
data_long$SimilarityToGroupMembers<-as.factor(data_long$SimilarityToGroupMembers)
data_long$InterestInFootball=as.numeric(data_long$InterestInFootball)
names(data_long)
data_long$Group_Members_Combined=as.factor(data_long$Group_Members_Combined)

data_long$FavoriteFootballTeam=as.factor(data_long$FavoriteFootballTeam)
data_long$WillingnessToTakeRisks
data_long=data_long%>% filter(Sex %in% c("Male","Female"))
data_long <- data_long %>%
  mutate(
    Concern_ClimateChange = ifelse(Concern_ClimateChange < 3, "high", "low"),
    Concern_ClimateChange = as.factor(Concern_ClimateChange)
  )

table(data_long$WillingnessToContributeIncome)

data_long$EducationLevel = as.numeric(data_long$EducationLevel)

data_long$HouseholdIncome = as.numeric(data_long$HouseholdIncome )
#data_long <- data_long %>%
#  dplyr::mutate(HouseholdIncome = dplyr::recode(HouseholdIncome,
#                                  `1` = "Below 10,000",
#                                  `2` = "10,000 - 14,999",
#                                  `3` = "15,000 - 24,999",
#                                  `4` = "25,000 - 34,999",
#                                  `5` = "35,000 - 44,999"))



library(expss)
data_long = apply_labels(data_long,
                         HouseholdIncome = "Annual Household Income After Tax")

data_long = apply_labels(data_long,
                         EducationLevel = "Higher Education")


### Regression model 
data_long_big=data_long%>% filter(Treatment !="Control-Small")

#Regression with all

library(sjPlot)

###BASELINE:USED
data_long$GroupType <- as.factor(data_long$GroupType)
data_long$GroupType <- relevel(data_long$GroupType, ref = "Low Social")

regression_model1 <- lm(Investment ~ GroupType + Theme + Bonus_ + Threshold_high , data = data_long)
tab_model(regression_model1)

####BASELINE+CONTROLS:USED

regression_model2 <- lm(Investment ~ GroupType + Theme + Bonus_yes + Threshold_high+WillingnessToTakeRisks+ Sex+HouseholdIncome+EducationLevel+InterestInFootball +
                         +Concern_ClimateChange , data = data_long)
tab_model(regression_model2)

stargazer(regression_model2, type = "latex", 
          title = "Regression Results", 
          out = "regression_output.tex")

system("pdflatex regression_output.tex")


##
library(stargazer)
stargazer(regression_model1, regression_model2,
          type = "latex",                 # Use "text" for console output; "html" or "latex" for other formats
          dep.var.labels = "Investment", # Label for the dependent variable
          covariate.labels = c("Group Type", "Theme", "Bonus", "Threshold High"), # Only show main variables
          omit = c("WillingnessToTakeRisks", "Sex", "HouseholdIncome", 
                   "EducationLevel", "InterestInFootball", "Concern_ClimateChange"), # Omit control variables
          add.lines = list(
            c("Controls Included", "No", "Yes")  # Custom row to indicate controls inclusion
          ),
          column.labels = c("Baseline", "Baseline + Controls"),  # Column labels
          keep.stat = c("n", "rsq", "adj.rsq")   # Include relevant statistics
)




data_beliefs1$category <- ifelse(data_beliefs1$variable == "B_0" | data_beliefs1$variable == "B_9_100", "0 to 100",
                                 ifelse(data_beliefs1$variable == "B_100_199" , "100 to 199",
                                        "200 to 300"))

library(afex)

# Summary of the RMANOVA model: NEW WINNER MODEL???

# Log transformation example
data_long$Investment_log <- log(data_long$Investment + 1)  # Adding 1 to handle zeros

data_long$Investment_2 <- (data_long$Investment)^2  # Adding 1 to handle zeros

model1 <- aov(Investment ~ GroupType +Theme +Bonus + Threshold_high +WillingnessToTakeRisks + Sex + as.factor(HouseholdIncome) + as.factor(EducationLevel) + as.factor(InterestInFootball) + as.factor(Concern_ClimateChange) + Error(ID/Round), data = data_long)

model_log <- aov(Investment_log ~ GroupType +Theme +Bonus + Threshold_high +WillingnessToTakeRisks + Sex + as.factor(HouseholdIncome) + as.factor(EducationLevel) + as.factor(InterestInFootball) + as.factor(Concern_ClimateChange) + Error(ID/Round), data = data_long)

model_squared <- aov(Investment_2 ~ GroupType +Theme +Bonus + Threshold_high +WillingnessToTakeRisks + Sex + as.factor(HouseholdIncome) + as.factor(EducationLevel) + as.factor(InterestInFootball) + as.factor(Concern_ClimateChange) + Error(ID/Round), data = data_long)


# Summary of the model
summary(model_log)

sum_test = unlist(model)
names(sum_test)

# Extract residuals and fitted values from the log-transformed model
residuals_log <- residuals(model_log)
fitted_log <- fitted(model_log)

# Extract residuals and fitted values from the squared-transformed model
residuals_squared <- residuals(model_squared)
fitted_squared <- fitted(model_squared)

# Q-Q plot for log-transformed model
qqnorm(residuals_log, main = "Q-Q Plot (Log Transformation)")
qqline(residuals_log, col = "red")

# Q-Q plot for squared-transformed model
qqnorm(residuals_squared, main = "Q-Q Plot (Squared Transformation)")
qqline(residuals_squared, col = "red")


##
anova_test_summary <- summary(model)
print(anova_test_summary)

# Extract the p-values for within-subject effects
within_p_values <- sum_test["Error: Within.Pr(>F)2"]
print(within_p_values)

# Extract the F-values for within-subject effects
within_f_values <- sum_test["Error: Within.F value1"]
print(within_f_values)

sum_test = unlist(summary(model))
names(sum_test)

sum_test["Error: Within.Pr(>F)2"]



# Summary of the RMANOVA model: NEW WINNER MODEL??? FOR BELIEFS: BONUS RELEVANT NOT GROUP TYPE?

model2 <- aov(B_high ~ GroupType +Theme +Bonus_yes + Threshold_high +WillingnessToTakeRisks + Sex + HouseholdIncome + EducationLevel + InterestInFootball + as.factor(Concern_ClimateChange) + Error(ID/Round), data = data_long)

# Summary of the model
summary(model2)

# Summary of the RMANOVA model: NEW WINNER MODEL??? FOR BELIEFS: BONUS RELEVANT NOT GROUP TYPE?

model3 <- aov(AverageGuess ~ GroupType +Theme +Bonus_yes + Threshold_high +WillingnessToTakeRisks + Sex + HouseholdIncome + EducationLevel + InterestInFootball + Concern_ClimateChange + Error(ID/Round), data = data_long)

# Summary of the model
summary(model3)

library(ggplot2)

# Plotting the distribution with smooth lines
ggplot(data_long, aes(x = AverageGuess, fill = GroupType, color = GroupType)) +
  geom_density(alpha = 0.4, size = 1.2) +  # Density plot with smooth lines
  labs(
    title = "Distribution of Average Guess Across Treatments",
    x = "Average Guess",
    y = "Density"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +  # Optional: Change the color palette
  scale_color_brewer(palette = "Set2")

wilcox.test(AverageGuess ~ GroupType, data = data_long)


##Beliefs
library(dplyr)
library(tidyr)
# Reshape data for repeated measures ANOVA
long_data <- data_long %>%
  pivot_longer(cols = starts_with("B_"), names_to = "BeliefType", values_to = "BeliefValue") %>%
  mutate(BeliefType = factor(BeliefType, levels = c("B_0", "B_1_99", "B_100_199", "B_200_299", "B_300")))

data_long$Belief

wilcox.test(data_long$B_high ~ data_long$GroupType)

data_long$B_high

##BEST WITH REPEATED
library(nlme)
library(lme4)
library(lmerTest)
library(sjPlot)

# Fit a mixed effects model
regression_model_R1 <- lmer(Investment_2 ~ GroupType + Theme + Bonus_yes * Threshold_high+(1 | ID) , data = data_long)
tab_model(regression_model_R1)

# Extract residuals and fitted values
residuals_lm <- residuals(regression_model_R1)
fitted_lm <- fitted(regression_model_R1)

# Q-Q plot for residuals
qqnorm(residuals_lm, main = "Q-Q Plot (Log Transformation)")
qqline(residuals_lm, col = "red")



# Bootstrap the model
set.seed(123)  # For reproducibility
boot_model <- bootMer(regression_model_R1, FUN = fixef, nsim = 1000)

# Summary of bootstrapped fixed effects
summary(boot_model)

# Extract bootstrapped confidence intervals
boot_ci <- apply(boot_model$t, 2, function(x) boot.ci(boot_model, type = "perc", index = which(colnames(boot_model$t) == names(x)))$percent[4:5])

# Convert the results into a data frame for easier interpretation
boot_results <- data.frame(
  Estimate = fixef(regression_model_R1),
  CI_lower = sapply(boot_ci, `[`, 1),
  CI_upper = sapply(boot_ci, `[`, 2)
)

# Check if the confidence intervals include zero
boot_results$Significance <- ifelse(boot_results$CI_lower > 0 | boot_results$CI_upper < 0, "Significant", "Not Significant")

print(boot_results)


# Install and load the glmmTMB package for mixed models with truncation
library(glmmTMB)

# Load minqa
library(minqa)
# Normalize data to (0, 1) if applicable
data_long$Investment_norm <- data_long$Investment / 300


# Fit the GLS model with an autoregressive correlation structure
model_gls <- gls(
  Investment ~ GroupType + Theme + Bonus_yes * Threshold_high,
  data = data_long,
  correlation = corAR1(form = ~ 1 | ID)  # Autoregressive correlation structure
)
tab_model(model_gls)

rm_anova_mixed <- lme(
  Investment ~ Bonus_yes * Threshold_high + GroupType + Theme,
  random = ~ 1 | ID,
  data = data_long
)

# View results
tab_model(rm_anova_mixed)



##NOT NORMAL RESIDUALS:
shapiro.test(residuals(lm(Investment ~ GroupType + Theme + Bonus_yes * Threshold_high, data = data_long)))


###Bootstrappin' 
library(boot)
library(lme4)
 #Function to fit the model
fit_lmer <- function(data, indices) {
  d <- data[indices, ]  # Resample the data
  model <- lmer(Investment_2 ~ GroupType + Theme + Bonus_yes + Threshold_high+(1|ID), data = d)
  return(fixef(model))  # Return fixed effects
}

# Perform bootstrapping
set.seed(123)
results <- boot::boot(data = data_long, statistic = fit_lmer, R = 1000)  # 1000 resamples

# Ensure that the result has the same structure as the original model coefficients
coef_names <- names(fixef(regression_model_R1))
bootstrap_estimates <- t(sapply(1:nrow(results$t), function(i) {
  res <- results$t[i, ]
  if (length(res) < length(coef_names)) {
    # Pad with NA if the result has fewer coefficients
    res <- c(res, rep(NA, length(coef_names) - length(res)))
  }
  return(res)
}))

colnames(bootstrap_estimates) <- coef_names

# Calculate the summary statistics
bootstrap_summary <- data.frame(
  Term = coef_names,
  Estimate = apply(bootstrap_estimates, 2, mean, na.rm = TRUE),
  SE = apply(bootstrap_estimates, 2, sd, na.rm = TRUE),
  `2.5%` = apply(bootstrap_estimates, 2, quantile, 0.025, na.rm = TRUE),
  `97.5%` = apply(bootstrap_estimates, 2, quantile, 0.975, na.rm = TRUE)
)

# Get p-values from lmerTest
original_summary <- summary(regression_model_R1)
bootstrap_summary$p.value <- original_summary$coefficients[, "Pr(>|t|)"]

#USED: ###Final table with p-values
sjPlot::tab_df(
  bootstrap_summary,
  title = "Bootstrapped Estimates for Mixed Effects Model",
  digits = 3,
  show.ci = FALSE,
  show.p = TRUE
)

# Create a data frame with bootstrapped estimates
boot_model <- lm(Estimate ~ Term, data = bootstrap_summary)

# Load kableExtra for LaTeX table generation
library(kableExtra)

# Create the LaTeX table
kable_table <- bootstrap_summary %>%
  dplyr::select(Term, Estimate, SE, `X2.5.`, `X97.5.`) %>%
  kable(
    format = "latex",
    digits = 3,
    col.names = c("Term", "Estimate", "SE", "2.5 %", "97.5 %"),
    caption = "Bootstrapped Mixed-Effects Model"
  ) %>%
  kable_styling(latex_options = c("hold_position"))

# Print the kable table for LaTeX
cat(kable_table)



##

##Für beliefs
regression_model <- lm(AverageGuess ~ GroupType + Theme + Bonus_yes + Threshold_high, data = data_long)
tab_model(regression_model)


###MIT OHNE CONTROLS MACHEN! STARGATE!
####das schaut so aus als würde man da nicht viel sehen:
kruskal.test(SimilarityToGroupMembers ~ GroupType, data = data_long)

# Create the ggplot with relative frequencies
ggplot(data_long, aes(x = as.numeric(SimilarityToGroupMembers), fill = Treatment_new)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, position = "dodge") +  # Use density for relative frequencies
  facet_wrap(~ Treatment_new, scales = "free_y") +
  labs(
    title = "Distribution of Similarity to Group Members by Group Type",
    x = "Similarity to Group Members",
    y = "Relative Frequency",
    fill = "Group Type"
  ) +
  theme_minimal()




data_long$Order
first=data_long %>% filter(Order %in% c(1,2))
second=data_long %>% filter(Order==4)
regression_model <- lm(Investment ~ GroupComposition *ProjectType + Bonus * Threshold_high+AverageGuess+Sex+HouseholdIncome+WillingnessToTakeRisks, data = first
                       )
tab_model(regression_model)

test=data_long %>% filter(ProjectType==0)
test2=data_long%>% filter(ProjectType==1)

regression_model <- lm(Investment ~ GroupComposition + Bonus * Threshold_high+AverageGuess+Sex+HouseholdIncome, data = test2)
tab_model(regression_model)





sjPlot::view_df(data_long)
control_vars <- data_long %>%
  select(Investment,HouseholdIncome, EducationLevel, InterestInFootball, WillingnessToTakeRisks, 
         Concern_ClimateChange, WillingnessToContributeIncome, AverageGuess, ParticipationInCrowdfunding, 
         LikelihoodToParticipate_CrowdfundingCampaign, SimilarityToGroupMembers)

control_vars <- control_vars %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))


# Calculate the correlation matrix
cor_matrix <- cor(control_vars, use = "complete.obs")




# Print the correlation matrix
print(cor_matrix)
library(corrplot)
# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         diag = FALSE)


names(data_long)



###########RAPHAELS PLOT ANALYSIS WITH MANN WHITNEY U AS WELL#####################

##########Beliefs##########

library(reshape)

# Melt the data into long format
data_beliefs1 <- melt(data_long, id.vars = "Treatment_new", measure.vars = c("B_0","B_1_99", "B_100_199", "B_200_299", "B_300"))


# Combine the data and calculate frequency
data_beliefs_combined <- data_beliefs1 %>%
  group_by(Treatment_new, variable) %>%
  summarise(
    total_value = sum(value), 
    count = n(), 
    .groups = 'drop'
  ) %>%
  group_by(Treatment_new) %>%
  mutate(
    Total = sum(total_value),
    freq = total_value / Total,
    SE = sqrt((freq * (1 - freq)) / count), # Standard error calculation
    CI_95 = qt(0.975, df = count - 1) * SE) # 95% confidence interval calculation)

# BELIEF ALL TREATMENTS
ggplot(data_beliefs_combined, aes(x = variable, y = freq, fill = Treatment_new)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Beliefs across Interaction of Offsetting Reason and Group Composition", 
       x = "Beliefs of group-members investments", 
       y = "Frequency",
       fill= "Campaign-Composition") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))



data_beliefs1$category <- ifelse(data_beliefs1$variable == "B_0" | data_beliefs1$variable == "B_9_100", "0 to 100",
                                  ifelse(data_beliefs1$variable == "B_100_199" , "100 to 199",
                                         "200 to 300"))


##
data_beliefs_combined_1<- data_beliefs1 %>%
  group_by(Treatment_new, category) %>%
  summarise(
    total_value = sum(value), 
    count = n(), 
    .groups = 'drop'
  ) %>%
  group_by(Treatment_new) %>%
  mutate(
    Total = sum(total_value),
    freq = total_value / Total,
    SE = sqrt((freq * (1 - freq)) / count), # Standard error calculation
    CI_95 = qt(0.975, df = count - 1) * SE # 95% confidence interval calculation
  )

# BELIEFS ALL TREATMENT WITH 3 CATEGORIES
ggplot(data_beliefs_combined_1, aes(x = category, y = freq, fill = Treatment_new)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = freq - CI_95, ymax = freq + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Investment Beliefs by Group Composition", 
       x = "Beliefs", 
       y = "Frequency",
       fill= "Treatment Conditions") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))



##FOR GROUP COMPOSITION: 
# Melt the data into long format
data_beliefs_GT <- melt(data_long, id.vars = "GroupType", measure.vars = c("B_0","B_1_99", "B_100_199", "B_200_299", "B_300"))


# Combine the data and calculate frequency
data_beliefs_combined_GT <- data_beliefs_GT %>%
  group_by(GroupType, variable) %>%
  summarise(
    total_value = sum(value), 
    count = n(), 
    .groups = 'drop'
  ) %>%
  group_by(GroupType) %>%
  mutate(
    Total = sum(total_value),
    freq = total_value / Total,
    SE = sqrt((freq * (1 - freq)) / count), # Standard error calculation
    CI_95 = qt(0.975, df = count - 1) * SE # 95% confidence interval calculation
  )

data_beliefs_combined_GT <- data_beliefs_combined_GT %>%
  dplyr::mutate(variable = dplyr::recode(variable, 
                           "B_0" = "0", 
                           "B_1_99" = "1-99", 
                           "B_100_199" = "100-199", 
                           "B_200_299" = "200-299", 
                           "B_300" = "300"))


# Distribution of Investment Beliefs by Group Composition 
ggplot(data_beliefs_combined_GT, aes(x = variable, y = freq, fill = GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = freq - CI_95, ymax = freq + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Investment Beliefs by Group Composition", 
       x = "Beliefs", 
       y = "Frequency",
       fill = "Group Composition") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))



# Bootstrapping for Confidence Intervals
bootstrap_ci <- function(data, index) {
  mean(data[index])
}
# Convert the data to long format for easier plotting
xdata_long_melt <- data_long %>%
  pivot_longer(cols = starts_with("B_"), names_to = "BeliefCategory", values_to = "Value")

library(boot)

# Bootstrap CI calculation for each belief category by group
bootstrap_results <- xdata_long_melt %>%
  group_by(GroupType, BeliefCategory) %>%
  summarise(
    Mean = mean(Value),
    CI_Lower = boot(Value, bootstrap_ci, R = 1000)$t0 - 1.96 * sd(boot(Value, bootstrap_ci, R = 1000)$t),
    CI_Upper = boot(Value, bootstrap_ci, R = 1000)$t0 + 1.96 * sd(boot(Value, bootstrap_ci, R = 1000)$t)
  )

# Plotting means with non-parametric confidence intervals
ggplot(bootstrap_results, aes(x = BeliefCategory, y = Mean, fill = GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Mean Beliefs by Group with Non-Parametric 95% Confidence Intervals",
       x = "Belief Category",
       y = "Mean Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Bootstrap CI calculation for each belief category by group
bootstrap_results <- data_long_melt %>%
  group_by(GroupType, BeliefCategory) %>%
  summarise(
    Mean = mean(Value),
    CI_Lower = boot(Value, bootstrap_ci, R = 1000)$t0 - 1.96 * sd(boot(Value, bootstrap_ci, R = 1000)$t),
    CI_Upper = boot(Value, bootstrap_ci, R = 1000)$t0 + 1.96 * sd(boot(Value, bootstrap_ci, R = 1000)$t)
  )

# Plotting means with non-parametric confidence intervals
ggplot(bootstrap_results, aes(x = BeliefCategory, y = Mean, fill = GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Mean Beliefs by Group with Non-Parametric 95% Confidence Intervals",
       x = "Belief Category",
       y = "Mean Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")



# Calculate mean and standard error for each belief category by GroupType
summary_stats <- data_long %>%
  group_by(GroupType) %>%
  summarise(
    B_0_mean = mean(B_0),
    B_1_99_mean = mean(B_1_99),
    B_100_199_mean = mean(B_100_199),
    B_200_299_mean = mean(B_200_299),
    B_300_mean = mean(B_300),
    B_0_se = sd(B_0) / sqrt(n()),
    B_1_99_se = sd(B_1_99) / sqrt(n()),
    B_100_199_se = sd(B_100_199) / sqrt(n()),
    B_200_299_se = sd(B_200_299) / sqrt(n()),
    B_300_se = sd(B_300) / sqrt(n())
  )

# Reshape the data for plotting
summary_stats_melted <- summary_stats %>%
  pivot_longer(cols = ends_with("_mean"), names_to = "BeliefCategory", values_to = "Mean") %>%
  pivot_longer(cols = ends_with("_se"), names_to = "SECategory", values_to = "SE") %>%
  filter(gsub("_mean", "", BeliefCategory) == gsub("_se", "", SECategory)) %>%
  mutate(BeliefCategory = gsub("_mean", "", BeliefCategory))

# Plotting the histogram with confidence intervals
ggplot(summary_stats_melted, aes(x = BeliefCategory, y = Mean, fill = GroupType)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Mean - 1.96 * SE, ymax = Mean + 1.96 * SE), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(
    title = "Comparison of Beliefs (B_0 to B_300) Across Group Types with 95% Confidence Intervals",
    x = "Belief Category",
    y = "Mean Relative Frequency"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  # Optional

##Lower 100 Higher 100

# Create a new column "category" based on the "value" column
data_beliefs_GT$category <- ifelse(data_beliefs_GT$variable == "B_0" | data_beliefs_GT$variable == "B_9_100", "0 to 100",
                                   ifelse(data_beliefs_GT$variable == "B_100_199" , "100 to 199",
                                          "200 to 300"))
# Group by "GroupType" and "category" and summarize
data_beliefs_combined_GT <- data_beliefs_GT %>%
  group_by(GroupType, category) %>%
  summarise(
    total_value = sum(value), 
    count = n(), 
    .groups = 'drop'
  ) %>%
  group_by(GroupType) %>%
  mutate(
    Total = sum(total_value),
    freq = total_value / Total,
    SE = sqrt((freq * (1 - freq)) / count), # Standard error calculation
    CI_95 = qt(0.975, df = count - 1) * SE # 95% confidence interval calculation
  )

########Create the bar plot with error bars
# Perform Wilcoxon tests and get p-values
wilcox_test_results <- data_beliefs_GT %>%
  group_by(category) %>%
  summarize(p_value = wilcox.test(value ~ GroupType)$p.value, .groups = 'drop') %>%
  mutate(
    significance = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    ),
    x = ifelse(category == "0 to 100", 1.5, ifelse(category == "100 to 199", 2.5, 3.5)),
    y = 30  # Position above the bars
  )

#wilcox.test(value ~ GroupType, data = data_beliefs_GT %>% filter(category == "100 to 199"))

#wilcox.test(value ~ GroupType, data = data_beliefs_GT %>% filter(category == "200 to 300"))


# Merge significance results with the data for plotting
data_beliefs_combined_GT <- data_beliefs_combined_GT %>%
  left_join(wilcox_test_results, by = "category")

####USED###Distribution of Investment Beliefs by Group Composition
ggplot(data_beliefs_combined_GT, aes(x = category, y = freq, fill = GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = freq - CI_95, ymax = freq + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "", 
       x = "Beliefs", 
       y = "Frequency",
       fill= "Group Composition") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))+
  geom_text(
    aes(label = significance, 
        y = freq + CI_95 +0.01),  # Adjust this value to place the stars above the error bars
    position = position_dodge(width = 0.9),
    vjust = 0,
    size=7
  ) +
  theme(
    axis.text = element_text(size = 15),  # Font size of axis text
    axis.title.x = element_text(size = 17), # Font size of axis titles
    axis.title.y = element_text(size = 15), # Font size of axis titles
    plot.title = element_text(size = 15), # Font size of plot title
    legend.text = element_text(size = 15), # Font size of legend text
    legend.title = element_text(size = 17) # Font size of legend title
  )



#####FOR TYPE COMPOSITION: 
# Melt the data into long format
data_beliefs_T <- melt(data_long, id.vars = "Theme", measure.vars = c("B_0","B_1_99", "B_100_199", "B_200_299", "B_300"))


# Combine the data and calculate frequency
data_beliefs_combined_T <- data_beliefs_T %>%
  group_by(Theme, variable) %>%
  summarise(
    total_value = sum(value), 
    count = n(), 
    .groups = 'drop'
  ) %>%
  group_by(Theme) %>%
  mutate(
    Total = sum(total_value),
    freq = total_value / Total,
    SE = sqrt((freq * (1 - freq)) / count), # Standard error calculation
    CI_95 = qt(0.975, df = count - 1) * SE # 95% confidence interval calculation
  )

data_beliefs_combined_T <- data_beliefs_combined_T %>%
  dplyr::mutate(variable =  dplyr::recode(variable, 
                           "B_0" = "0", 
                           "B_1_99" = "1-99", 
                           "B_100_199" = "100-199", 
                           "B_200_299" = "200-299", 
                           "B_300" = "300"))


# Create the bar plot with error bars
ggplot(data_beliefs_combined_T, aes(x = variable, y = freq, fill = Theme)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = freq - CI_95, ymax = freq + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Investment Beliefs by Group Theme", 
       x = "Beliefs", 
       y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))



data_beliefs_T$category <- ifelse(data_beliefs_T$variable == "B_0" | data_beliefs_T$variable == "B_9_100", "0 to 100",
                                   ifelse(data_beliefs_T$variable == "B_100_199" , "100 to 199",
                                          "200 to 300"))


# Combine the data and calculate frequency
data_beliefs_combined_T <- data_beliefs_T %>%
  group_by(Theme, category) %>%
  summarise(
    total_value = sum(value), 
    count = n(), 
    .groups = 'drop'
  ) %>%
  group_by(Theme) %>%
  mutate(
    Total = sum(total_value),
    freq = total_value / Total,
    SE = sqrt((freq * (1 - freq)) / count), # Standard error calculation
    CI_95 = qt(0.975, df = count - 1) * SE # 95% confidence interval calculation
)

# Create the bar plot with error bars
ggplot(data_beliefs_combined_T, aes(x = category, y = freq, fill = Theme)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = freq - CI_95, ymax = freq + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Investment Beliefs by Campaign Focus", 
       x = "Beliefs", 
       y = "Frequency",
       fill="Campaign Focus") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))



##Bonus Beliefs
# Melt the data into long format
data_beliefs_B <- melt(data_long, id.vars = "Bonus_yes", measure.vars = c("B_0","B_1_99", "B_100_199", "B_200_299", "B_300"))

data_beliefs_B$category <- ifelse(data_beliefs_B$variable == "B_0" | data_beliefs_B$variable == "B_9_100", "0 to 100",
                                  ifelse(data_beliefs_B$variable == "B_100_199" , "100 to 199",
                                         "200 to 300"))

library(reshape2)

# Combine the data and calculate frequency
data_beliefs_combined_B <- data_beliefs_B %>%
  group_by(Bonus_yes, category) %>%
  summarise(
    total_value = sum(value), 
    count = n(), 
    .groups = 'drop'
  ) %>%
  group_by(Bonus_yes) %>%
  mutate(
    Total = sum(total_value),
    freq = total_value / Total,
    SE = sqrt((freq * (1 - freq)) / count), # Standard error calculation
    CI_95 = qt(0.975, df = count - 1) * SE # 95% confidence interval calculation
  )

# Create the bar plot with error bars
ggplot(data_beliefs_combined_B, aes(x = category, y = freq, fill = as.factor(Bonus_yes)) )+
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = freq - CI_95, ymax = freq + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Investment Beliefs by Offsetting Reason", 
       x = "Beliefs", 
       y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))


#####FOR TYPE COMPOSITION: 
# Melt the data into long format
data_beliefs_B <- melt(data_long, id.vars = "Bonus", measure.vars = c("B_0","B_1_99", "B_100_199", "B_200_299", "B_300"))


# Combine the data and calculate frequency
data_beliefs_combined_B <- data_beliefs_B %>%
  group_by(Bonus, variable) %>%
  summarise(
    total_value = sum(value), 
    count = n(), 
    .groups = 'drop'
  ) %>%
  group_by(Bonus) %>%
  mutate(
    Total = sum(total_value),
    freq = total_value / Total,
    SE = sqrt((freq * (1 - freq)) / count), # Standard error calculation
    CI_95 = qt(0.975, df = count - 1) * SE # 95% confidence interval calculation
  )

data_beliefs_combined_B <- data_beliefs_combined_B %>%
  dplyr::mutate(variable =  dplyr::recode(variable, 
                           "B_0" = "0", 
                           "B_1_99" = "1-99", 
                           "B_100_199" = "100-199", 
                           "B_200_299" = "200-299", 
                           "B_300" = "300"))


# Create the bar plot with error bars
ggplot(data_beliefs_combined_T, aes(x = category, y = freq, fill = Theme)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = freq - CI_95, ymax = freq + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Investment Beliefs by Group Theme", 
       x = "Beliefs", 
       y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))




### Create the summary table
summary_table_B <- data_long %>%
  group_by(Bonus) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Mean_SD = paste0(round(Mean_Investment, 2), " (", round(SD_Investment, 2), ")")
  ) %>%
  select(Bonus, Mean_SD)



##Braplot only Group Type
# Combine the data and calculate frequency
summary_table11 <- data_long %>%
  group_by(GroupType,Theme) %>%
  summarize(
    Mean_AverageGuess = mean(AverageGuess, na.rm = TRUE),
    SD_AverageGuess = sd(AverageGuess, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_AverageGuess =  SD_AverageGuess / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_AverageGuess,
    .groups = 'drop'
  )

# Create the ggplot with error bars
ggplot(summary_table11, aes(y = Mean_AverageGuess, x= Theme, fill=GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_AverageGuess - CI_95, ymax = Mean_AverageGuess + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Average Guess by Group Composition", 
       x = "", 
       y = "Average Guess",
       fill="Group Composition") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))



###FOR THEME SIEHT MAN NICHT VIEL ALSO NICHT GEMACHT


####Barblot for group type and theme#

library(ggsignif)
library(ggpubr)


# Combine the data and calculate frequency
summary_table2 <- data_long %>%
  group_by(GroupType, Theme) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_Investment,
    .groups = 'drop'
  )


library(dplyr)
library(ggplot2)
library(ggpubr)

# Perform Wilcoxon tests and get p-values
wilcox_test_results <- data_long %>%
  group_by(Theme) %>%
  summarize(p_value = wilcox.test(Investment ~ GroupType)$p.value, .groups = 'drop') %>%
  mutate(
    significance = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    ),
    x = ifelse(Theme == "Football", 1.5, 2.5),  # Position for each Theme
    y = 30  # Position above the bars
  )

# Merge the significance stars back into the summary table
summary_table2 <- summary_table2 %>%
  left_join(wilcox_test_results, by = "Theme")
  
#####USED##### Mean Investment by Group Composition and Campaign Focus
ggplot(summary_table2, aes(x = Theme, y = Mean_Investment, fill = GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = significance, 
        y = Mean_Investment + CI_95 + 3),  # Adjust this value to place the stars above the error bars
    position = position_dodge(width = 0.9),
    vjust = 0,
    size=7
  ) +
  labs(title = "", 
       x = "Campaign Focus", 
       y = "Mean Investment",
       fill = "Group Composition") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))



Football_data <- data_long[data_long$Theme == "Football", ]
wilcox.test(Football_data$Investment ~ Football_data$GroupType)

Neutral_data <- data_long[data_long$Theme == "Neutral", ]
wilcox.test(Neutral_data$Investment ~ Neutral_data$GroupType)


##Braplot only Group Type
# Combine the data and calculate frequency
summary_table11 <- data_long %>%
  group_by(GroupType) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_Investment,
    .groups = 'drop'
  )


# Perform Wilcoxon tests and get p-values
wilcox_test_results <- data_long %>%
  summarize(p_value = wilcox.test(Investment ~ GroupType)$p.value, .groups = 'drop') %>%
  mutate(
    significance = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    ),
    y = 30  # Position above the bars
  )

summary_table11 <- summary_table11 %>%
  mutate(
    significance = wilcox_test_results$significance,
    y = Mean_Investment + CI_95 + 3  # Position for significance stars above the bars
  )

#####USED#####Mean Investment by Group Composition
# Create the ggplot with error bars and significance stars
ggplot(summary_table11, aes(y = Mean_Investment, x = GroupType, fill = GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = significance, 
        y = y),
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 7.5 
  ) +
  labs(title = "", 
       x = "", 
       y = "",
       fill = NULL) +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))+
 theme(axis.text=element_text(size=15), 
       plot.title=element_text(size=15), #change font size of axis text
       axis.title=element_text(size=15), #change font size of axis titles
        #change font size of plot title
     legend.position = "none") #change font size of all text and remove legend

# Combine the data and calculate frequency
summary_table11 <- data_long %>%
  group_by(Theme) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_Investment,
    .groups = 'drop'
  )

# Perform Wilcoxon tests and get p-values
wilcox_test_results <- data_long  %>%
  summarize(p_value = wilcox.test(Investment ~ Theme)$p.value, .groups = 'drop') %>%
  mutate(
    significance = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    ),
    y = 30  # Position above the bars
  )

# Add significance data to summary_table11
summary_table11 <- summary_table11 %>%
  left_join(wilcox_test_results) %>%
  mutate(
    y = Mean_Investment + CI_95 + 3  # Position for significance stars above the bars
  )

#####USED### Mean Investment by Offsetting Reason
ggplot(summary_table11, aes(y = Mean_Investment, x = Theme, fill = Theme)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9))  +
  labs(title = "", 
       x = "", 
       y = "Mean Investment",
       fill = "Campaign Focus") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))+
  theme(axis.text=element_text(size=15), 
        plot.title=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        #change font size of plot title
        legend.position = "none") #change font size of all text and remove legend



wilcox.test(data_long$Investment ~ data_long$Theme)


####Barblot for New_Treatment

# Summarize the data
All_Treatment_Investment <- data_long %>%
  group_by(Treatment_new) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n), 
    CI_95 = qt(0.975, df = n - 1) * SE_Investment,# Calculate standard error
    .groups = 'drop' # Drop grouping structure
  ) %>%
  # Order the Treatment_new factor based on Mean_Investment
  mutate(Treatment_new = factor(Treatment_new, levels = Treatment_new[order(Mean_Investment)]))

# Create the ggplot with error bars
ggplot(All_Treatment_Investment, aes(x = Treatment_new, y = Mean_Investment, fill = Treatment_new)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "", 
       x = "", 
       y = "Mean Investment",
       fill = NULL) +  # Remove legend description
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  theme(axis.text=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        plot.title=element_text(size=20), #change font size of plot title
        legend.position = "none") #change font size of all text and remove legend
#title = "Mean Investment Group Composition x Offsetting Reason", 



# Perform Mann-Whitney U test (pairwise comparisons)
pairwise_test <- pairwise.wilcox.test(data_long$Investment, data_long$Treatment_new, p.adjust.method = "BH")

# Print summary statistics and pairwise test results
print(pairwise_test)





# Assuming 'data_long' is your dataset
data_long$Round <- as.factor(data_long$Round)

# Create a new dataframe for the minimum thresholds
thresholds <- data.frame(
  Round = factor(c(1, 2, 3, 4)),
  Threshold = c(1500, 2000, 1500, 2000)
)

# Plot
#library(Hmisc)  # For mean_cl_normal - mean_cl_normal instead of mean_se??

ggplot(data_long, aes(x = Round, y = AverageGuess, color = GroupType, group = GroupType)) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2) +
  geom_hline(data = thresholds, aes(yintercept = Threshold), linetype = "dashed", color = "black") +
  scale_colour_manual(values = wes_palette("Darjeeling2")) +
  labs(title = "Average Investment Belief by Group Type across Rounds",
       x = "Round",
       y = "Average Belief (AverageGuess)",
       color = "Group Type") +
  theme_minimal()


ggplot(data_long, aes(x = Round, y = Investment*10, color = GroupType, group = GroupType)) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2) +
  geom_hline(data = thresholds, aes(yintercept = Threshold), linetype = "dashed", color = "black") +
  scale_colour_manual(values = wes_palette("Darjeeling2")) +
  labs(title = "",
       x = "Round",
       y = "Mean Investment",
       color = "Group Composition") +
  theme_minimal()

##Group Investment by Group Composition across Rounds
ggplot(data_long, aes(x = Round, y = Investment*10, color = GroupType, group = GroupType)) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.2) +
  geom_hline(data = thresholds, aes(yintercept = Threshold), linetype = "dashed", color = "black") +
  scale_colour_manual(values = wes_palette("Darjeeling2")) +
  labs(title = "",
       x = "Round",
       y = "Mean Group Investment",
       color = "Group Composition") +
  theme_minimal()



# Create the Bonus(1if25%) and Threshold_high(1=3000pence) variables
data_long <- data_long %>%
  mutate(
    Bonus = case_when(
      Round == 1 ~ 0,
      Round == 2 ~ 0,
      Round == 3 ~ 1,
      Round == 4 ~ 1
    ),
    Threshold_high = case_when(
      Round == 1 ~ 0,
      Round == 2 ~ 1,
      Round == 3 ~ 0,
      Round == 4 ~ 1
    )
  )


##BONUS Section

# Step 1: Calculate the overall mean for each Treatment_new
overall_means <- data_long %>%
  group_by(Treatment_new) %>%
  summarize(Overall_Mean_Investment = mean(Investment, na.rm = TRUE)) %>%
  arrange(Overall_Mean_Investment)

# Step 2: Use the overall means to order the Treatment_new factor
summary_table5 <- data_long %>%
  group_by(Treatment_new, Bonus) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n), # Calculate standard error
    CI_95 = qt(0.975, df = n - 1) * SE_Investment, # Calculate 95% confidence interval
    .groups = 'drop' # Drop grouping structure
  ) %>%
  # Convert Bonus to "No" and "Yes"
  mutate(Bonus = ifelse(Bonus == 0, "No", "Yes")) %>%
  # Order the Treatment_new factor based on Overall_Mean_Investment
  mutate(Treatment_new = factor(Treatment_new, levels = overall_means$Treatment_new)) %>%
  # Order by Treatment_new and Mean_Investment for plotting
  arrange(Treatment_new, Mean_Investment)

# Create the ggplot with error bars
ggplot(summary_table5, aes(x = Treatment_new, y = Mean_Investment, fill = Bonus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by GroupType and Theme with 95% Confidence Interval", 
       x = "Group Type", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))


##BONUS: Aggregate Section

# Calculate the overall means for ordering
overall_means <- data_long %>%
  group_by(GroupType) %>%
  summarize(Overall_Mean_Investment = mean(Investment, na.rm = TRUE)) %>%
  arrange(Overall_Mean_Investment)

# Step 2: Use the overall means to order the Treatment_new factor
summary_table7 <- data_long %>%
  group_by(GroupType, Bonus) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n), # Calculate standard error
    CI_95 = qt(0.975, df = n - 1) * SE_Investment, # Calculate 95% confidence interval
    .groups = 'drop' # Drop grouping structure
  ) %>%
  # Convert Bonus to "No" and "Yes"
  mutate(Bonus = ifelse(Bonus == 0, "No", "Yes")) %>%
  # Order the Treatment_new factor based on Overall_Mean_Investment
  mutate(Treatment_new = factor(GroupType, levels = overall_means$GroupType)) %>%
  # Order by Treatment_new and Mean_Investment for plotting
  arrange(GroupType, Mean_Investment)

# Step 3: Perform Wilcoxon tests and get p-values
wilcox_test_results <- data_long %>%
  group_by(GroupType) %>%
  summarize(p_value = wilcox.test(Investment ~ Bonus)$p.value, .groups = 'drop') %>%
  mutate(
    significance = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    ),
    x = 1.5,  # Adjust the x-position of stars if necessary
    y = 30    # Adjust the y-position of stars if necessary
  )

# Step 4: Merge the significance stars back into the summary table
summary_table7 <- summary_table7 %>%
  left_join(wilcox_test_results, by = "GroupType")

######USED#### Mean Investment by Group Composition and Bonus
ggplot(summary_table7, aes(x = Treatment_new, y = Mean_Investment, fill = Bonus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = significance, 
        y = Mean_Investment + CI_95 + 3),  # Adjust this value to place the stars above the error bars
    position = position_dodge(width = 0.9),
    vjust = 0,
    size=8
  ) +
  labs(title = "", 
       x = "", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))+
  theme(axis.text=element_text(size=15), 
        plot.title=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        legend.text = element_text(size = 15), # Font size of legend text
        legend.title = element_text(size = 15) #change font size of plot title
       
      ) #change font size of all text and remove legend
###

##BONUS: Aggregate Section

# Calculate the overall means for ordering
overall_means <- data_long %>%
  group_by(GroupType) %>%
  summarize(Overall_Mean_Investment = mean(Investment, na.rm = TRUE)) %>%
  arrange(Overall_Mean_Investment)

# Step 2: Use the overall means to order the Treatment_new factor
summary_table7 <- data_long %>%
  group_by(Bonus) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n), # Calculate standard error
    CI_95 = qt(0.975, df = n - 1) * SE_Investment, # Calculate 95% confidence interval
    .groups = 'drop' # Drop grouping structure
  ) %>%
  # Convert Bonus to "No" and "Yes"
  mutate(Bonus = ifelse(Bonus == 0, "No", "Yes")) 
  # Order the Treatment_new factor based on Overall_Mean_Investment

# Create the ggplot with error bars
ggplot(summary_table7, aes(x = Bonus, y = Mean_Investment, fill=Bonus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by Bonus", 
       x = "", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))

wilcox.test(data_long$Investment ~ data_long$Bonus)


heterogen_data <- data_long[data_long$GroupType == "Random", ]
wilcox.test(heterogen_data$Investment ~ heterogen_data$Bonus)



homogen_data <- data_long[data_long$GroupType == "Same Club", ]
wilcox.test(homogen_data$Investment ~ homogen_data$Bonus)

Round_4_data <- data_long[data_long$Round == "4", ]
wilcox.test(Round_4_data$Investment ~ Round_4_data$GroupType)

Round_3_data <- data_long[data_long$Round == "3", ]
wilcox.test(Round_3_data$Investment ~ Round_3_data$GroupType)

Round_2_data <- data_long[data_long$Round == "2", ]
wilcox.test(Round_2_data$Investment ~ Round_2_data$GroupType)

Round_1_data <- data_long[data_long$Round == "1", ]
wilcox.test(Round_1_data$Investment ~ Round_1_data$GroupType)


##THRESHOLD Section

# Step 1: Calculate the overall mean for each Treatment_new
overall_means <- data_long %>%
  group_by(Treatment_new) %>%
  summarize(Overall_Mean_Investment = mean(Investment, na.rm = TRUE)) %>%
  arrange(Overall_Mean_Investment)

# Step 2: Use the overall means to order the Treatment_new factor
summary_table4 <- data_long %>%
  group_by(Treatment_new, Threshold_high) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n), # Calculate standard error
    CI_95 = qt(0.975, df = n - 1) * SE_Investment, # Calculate 95% confidence interval
    .groups = 'drop' # Drop grouping structure
  ) %>%
  # Order the Treatment_new factor based on Overall_Mean_Investment
  mutate(Treatment_new = factor(Treatment_new, levels = overall_means$Treatment_new)) %>%
  # Order by Treatment_new and Mean_Investment for plotting
  arrange(Treatment_new, Mean_Investment)

# Create the ggplot with error bars
ggplot(summary_table4, aes(x = Treatment_new, y = Mean_Investment, fill = as.factor(Threshold_high))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by GroupType and Theme with 95% Confidence Interval", 
       x = "Group Type", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))


###

##THRESHOLD: Aggregate Section

# Calculate the overall means for ordering
overall_means <- data_long %>%
  group_by(GroupType) %>%
  summarize(Overall_Mean_Investment = mean(Investment, na.rm = TRUE)) %>%
  arrange(Overall_Mean_Investment)

# Step 2: Use the overall means to order the Treatment_new factor
summary_table7 <- data_long %>%
  group_by(GroupType,Threshold_high) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n), # Calculate standard error
    CI_95 = qt(0.975, df = n - 1) * SE_Investment, # Calculate 95% confidence interval
    .groups = 'drop' # Drop grouping structure
  ) 

# Create the ggplot with error bars
ggplot(summary_table7, aes(x = GroupType, y = Mean_Investment, fill=as.factor(Threshold_high))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by Group Composition and Threshold", 
       x = "", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))



wilcox.test(data_long$Investment ~ data_long$Threshold_high)


heterogen_data <- data_long[data_long$GroupType == "Random", ]
wilcox.test(heterogen_data$Investment ~ heterogen_data$Threshold_high)



homogen_data <- data_long[data_long$GroupType == "Same Club", ]
wilcox.test(homogen_data$Investment ~ homogen_data$Threshold_high)


###

##THRESHOLD: Aggregate Section

# Calculate the overall means for ordering
overall_means <- data_long %>%
  group_by(GroupType) %>%
  summarize(Overall_Mean_Investment = mean(Investment, na.rm = TRUE)) %>%
  arrange(Overall_Mean_Investment)

# Step 2: Use the overall means to order the Treatment_new factor
summary_table7 <- data_long %>%
  group_by(Threshold_high) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n), # Calculate standard error
    CI_95 = qt(0.975, df = n - 1) * SE_Investment, # Calculate 95% confidence interval
    .groups = 'drop' # Drop grouping structure
  ) 

# Create the ggplot with error bars
ggplot(summary_table7, aes(x = as.factor(Threshold_high), y = Mean_Investment, fill=as.factor(Threshold_high))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by Threshold", 
       x = "", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))


#Cloud Analayis
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)


install.packages("tm")
library(tm)

#Create a vector containing only the text
docs <-  data_long[!(is.na(data_long$OtherConcerns_Text) | data_long$OtherConcerns_Text==""), ]

text <-  docs$OtherConcerns_Text

#Create a vector containing only the text
text <- data_text$text
# Create a corpus  

# Create a corpus  
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))



wordcloud(words = df$word, freq = df$freq, min.freq = 1,           max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))
