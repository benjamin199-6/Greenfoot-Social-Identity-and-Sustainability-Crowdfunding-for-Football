# Clear existing workspace to start fresh
rm(list = ls())

setwd("C:/Users/rgottwei/OneDrive - WU Wien/MY DOCS/RESEARCH/Greenfoot/old")
setwd("/Users/rapha/Library/CloudStorage/OneDrive-WUWien/MY DOCS/RESEARCH/Greenfoot")

# Load necessary libraries
library(dplyr)
library(haven)
library(tidyr)
library("wesanderson")
library(ggplot2)

data <- read_sav("Greenfoot_Survey_FullSample (1).sav")

filter = data$PROLIFIC_PID

data <- data %>% dplyr::rename(Last_desicion = Round)



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


names(Order) <- str_replace(names(Order), "_$", "")


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


library(stringr)

order_long <- order_columns %>%
  pivot_longer(
    cols = -id,
    names_to = "OrderType",
    values_to = "Order"
  ) %>%
  mutate(For_Investment = str_extract(OrderType, "\\d+$")) %>% # Extract the number at the end of the string
  mutate(For_Investment = as.numeric(For_Investment)) # Convert to numeric

# Filter out NA values
order_long <- order_long %>%
  filter(!is.na(Order))


# Make data long
data_long <- data %>%
  pivot_longer(
    cols = starts_with("Investment") | starts_with("AverageGuess")| starts_with("Belief"),
    names_to = c(".value", "Round"),
    names_pattern = "(Investment|AverageGuess|Belief)(\\d+)"
  )


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
data_long <- data_long %>%
  separate_belief_columns("Belief", "B") 



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


data_long$Order

data_long$`Success Belief` <- ifelse(
  (data_long$AverageGuess > 1500 & (data_long$Round == 1 | data_long$Round == 3)) |
    (data_long$AverageGuess > 2000 & (data_long$Round == 2 | data_long$Round == 4)),
  "larger",
  "smaller"
)



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
  dplyr::rename(                                       
    
    
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
library(sjPlot)
data_long = apply_labels(data_long,
                         HouseholdIncome = "Annual Household Income After Tax")

data_long = apply_labels(data_long,
                         EducationLevel = "Higher Education")


### Regression model 
data_long_big=data_long%>% filter(Treatment !="Control-Small")


data_filtered <- subset(data_long, Treatment %in% c("Control-Small", "Neutral-Random"))

wilcox.test(Investment ~ Treatment, data = data_filtered)



library(miceadds)
library(sandwich)
#Regression with al

### Regression model 
data_long_1=data_long%>% filter(Round =="1")
data_long_2=data_long%>% filter(Round =="2")
data_long_3=data_long%>% filter(Round =="3")
data_long_4=data_long%>% filter(Round =="4")

data_long_O1=data_long%>% filter(Order =="1")


###BASELINE:USED
data_long$GroupType <- as.factor(data_long$GroupType)
data_long$GroupType <- relevel(data_long$GroupType, ref = "Low Social")

regression_model1 <- lm.cluster(Investment ~ GroupType + Theme + Bonus_ + Threshold_high , data = data_long,, cluster=data_long$ID)

tab_model(regression_model1)

####BASELINE+CONTROLS:USED

regression_model2 <- lm.cluster(Investment ~ GroupType + Theme + Bonus_yes + Threshold_high+WillingnessToTakeRisks+ Sex+HouseholdIncome+EducationLevel+InterestInFootball +
                          +Concern_ClimateChange , data = data_long, cluster=data_long$ID)

summary(regression_model2)

tab_model(regression_model2)

stargazer(regression_model2, type = "latex", 
          title = "Regression Results", 
          out = "regression_output.tex")

system("pdflatex regression_output.tex")


####For all different situations:

# List of datasets and their names
datasets <- list(data_long_1, data_long_2, data_long_3, data_long_4, data_long_O1)
dataset_names <- c("data_long", "data_long_1", "data_long_2", "data_long_3", "data_long_4", "data_long_O1")

# Loop through datasets and run the regression
for (i in 1:length(datasets)) {
  cat("\nRunning regression for:", dataset_names[i], "\n")
  
  # Run the regression
  model <- lm(Investment ~ GroupType + Theme +
                WillingnessToTakeRisks + Sex + HouseholdIncome + 
                EducationLevel + InterestInFootball + Concern_ClimateChange, 
              data = datasets[[i]])
  
  # Print the summary of the model
  print(summary(model))
}











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




library(haven)
###RMANOVA:USED
model1 <- aov(Investment ~ GroupType +Theme +Bonus + Threshold_high+ Error(ID/Round), data = data_long)

# Summary of the model
summary(model1)

library(kableExtra)
library(broom)

model1_tidy <- tidy(model1)
# Create a tidy summary and print it as a LaTeX table
model1_tidy %>%
  kable(format = "latex", booktabs = TRUE, caption = "ANOVA Results") %>%
  kable_styling(latex_options = c("hold_position"))





# Summary of the RMANOVA model: NEW WINNER MODEL??? FOR BELIEFS: BONUS RELEVANT NOT GROUP TYPE?

model2 <- aov(Investment ~ GroupType +Theme +Bonus_yes + Threshold_high +WillingnessToTakeRisks + Sex + HouseholdIncome + EducationLevel + InterestInFootball + as.factor(Concern_ClimateChange) + Error(ID/Round), data = data_long)

# Summary of the model
summary(model2)

model2_tidy <- tidy(model2)
# Create a tidy summary and print it as a LaTeX table
model2_tidy %>%
  kable(format = "latex", booktabs = TRUE, caption = "ANOVA Results") %>%
  kable_styling(latex_options = c("hold_position"))




# Summary of the RMANOVA model: NEW WINNER MODEL??? FOR BELIEFS: BONUS RELEVANT NOT GROUP TYPE?

model3 <- aov(AverageGuess ~ GroupType +Theme +Bonus_yes + Threshold_high +WillingnessToTakeRisks + Sex + HouseholdIncome + EducationLevel + InterestInFootball + Concern_ClimateChange + Error(ID/Round), data = data_long)

# Summary of the model
summary(model3)




library(ggplot2)

# Plotting the distribution with smooth lines
ggplot(data_long, aes(x = AverageGuess, fill = Treatment_new, color = Treatment_new)) +
  geom_density(alpha = 0.4, size = 1.2) +  # Density plot with smooth lines
  labs(
    title = "Distribution of Average Guess Across Treatments",
    x = "Average Guess",
    y = "Density"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +  # Optional: Change the color palette
  scale_color_brewer(palette = "Set2")



##BEST WITH REPEATED
library(nlme)
library(lme4)
library(lmerTest)
library(sjPlot)

# Fit a mixed effects model
regression_model_R1 <- lmer(Investment ~ GroupType + Theme + Bonus_yes * Threshold_high+(1 | ID) , data = data_long)
tab_model(regression_model_R1)


# Fit a mixed effects model
regression_model_R2 <- lmer(Investment ~ GroupType + Theme + Bonus_yes + Threshold_high+WillingnessToTakeRisks+ Age+Sex+HouseholdIncome+EducationLevel+InterestInFootball +
                              +Concern_ClimateChange+(1 | ID) , data = data_long)
tab_model(regression_model_R2)

# Fit a mixed effects model
regression_model_R3 <- lmer(Investment ~ GroupType + Theme + Bonus_yes + Threshold_high+WillingnessToTakeRisks+ Age+Sex+HouseholdIncome+EducationLevel+InterestInFootball +
                              +Concern_ClimateChange+(1 | ID) , data = data_long)
tab_model(regression_model_R2)

# Extract residuals and fitted values
residuals_lm <- residuals(regression_model_R1)
fitted_lm <- fitted(regression_model_R1)

# Q-Q plot for residuals
qqnorm(residuals_lm, main = "Q-Q Plot (Log Transformation)")
qqline(residuals_lm, col = "red")

# Bootstrap the model
set.seed(123)  # For reproducibility
boot_model <- bootMer(regression_model_R1, FUN = fixef, nsim = 10000)

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

#data_long_larger <- data_long %>%
 # filter(`Success Belief` == "larger")


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


##FOR GROUP COMPOSITION: 

#data_long_larger <- data_long %>%
 # filter(`Success Belief` == "larger")

# Melt the data into long format
data_beliefs_GT <- melt(data_long, id.vars = "GroupType", measure.vars = c("B_0","B_1_99", "B_100_199", "B_200_299", "B_300"))


data_beliefs_GT <- melt(data_long_larger, id.vars = "GroupType", measure.vars = c("B_0","B_1_99", "B_100_199", "B_200_299", "B_300"))



# Create a new column "category" based on the "value" column
data_beliefs_GT$category <- ifelse(data_beliefs_GT$variable == "B_0" | data_beliefs_GT$variable == "B_1_99", "0 to 100",
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

wilcox.test(AverageGuess ~ GroupType, data= data_long)

kruskal.test(AverageGuess ~ Condition, data= data_long)


###################Hypothesis 6.1###############

# Use a specific Wes Anderson color palette for the plot
ggplot(data_long, aes(x = AverageGuess, y = Investment * 10)) +
  geom_point(color = wes_palette("Darjeeling2")[2], alpha = 0.6) +  # Points with a Wes Anderson color
  geom_smooth(method = "lm", color = wes_palette("Darjeeling2")[4], se = TRUE) +  # Regression line with confidence interval
  labs(title = "",
       x = "Average Guess",
       y = "Group Investment") +
  theme_minimal()



# Calculate correlation between InvestmentBelief and IndividualInvestment
cor(data_long$AverageGuess, data_long$Investment, use = "complete.obs", method = "spearman")

# Run a simple linear regression model
model <- lm(Investment ~ AverageGuess, data = data_long)
summary(model)


################H6.3###########################

##VARIANCES DIFFER
fligner.test(Investment ~ interaction(GroupType, Theme), data = data_long)

#no difference here
fligner.test(Investment ~ GroupType, data = data_long)

#no difference here
fligner.test(Investment ~ Theme, data = data_long)

# Box plot to visualize the variance of investments by GroupType and CampaignFocus
ggplot(data_long, aes(x = interaction(GroupType, Theme), y = Investment, fill = GroupType)) +
  geom_boxplot() +
  labs(title = "Investment Variance by Group Type and Campaign Focus",
       x = "Group Type and Campaign Focus",
       y = "Investment Amount") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))



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
data_beliefs_B <- melt(data_long, id.vars = "Theme", measure.vars = c("B_0","B_1_99", "B_100_199", "B_200_299", "B_300"))


# Combine the data and calculate frequency
data_beliefs_combined_B <- data_beliefs_B %>%
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
  group_by(Bonus,B_0) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Mean_SD = paste0(round(Mean_Investment, 2), " (", round(SD_Investment, 2), ")")
  )

##Braplot only Group Type
# Combine the data and calculate frequency
summary_table11 <- data_long %>%
  group_by(GroupType,Threshold_high) %>%
  summarize(
    Mean_AverageGuess = mean(AverageGuess, na.rm = TRUE),
    SD_AverageGuess = sd(AverageGuess, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_AverageGuess =  SD_AverageGuess / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_AverageGuess,
    .groups = 'drop'
  )

# Create the ggplot with error bars
ggplot(summary_table11, aes(y = Mean_AverageGuess, x= Threshold_high, fill=GroupType)) +
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


wilcox.test(AverageGuess ~ GroupType, data = data_long)

data_long_1 <- data_long %>% filter(Round == 1 | Round == 2)
data_long_1_1 <- data_long %>% filter(Round == 1)
data_long_1_2 <- data_long %>% filter(Round == 2)

data_long_3 <- data_long %>% filter(Round == 3 | Round == 4)
data_long_3_1 <- data_long %>% filter(Round == 3)
data_long_3_2 <- data_long %>% filter(Round == 4)

wilcox.test(data_long_1$AverageGuess ~ data_long_1$GroupType)

wilcox.test(data_long_3$AverageGuess ~ data_long_3$GroupType)

mean(data_long_3$Investment[data_long_3$GroupType == "Low Social"])


##HERE

##Braplot only Group Type
# Combine the data and calculate frequency
summary_table11 <- data_long %>%
  group_by(Treatment_new) %>%
  summarize(
    Mean_AverageGuess = mean(AverageGuess, na.rm = TRUE),
    SD_AverageGuess = sd(AverageGuess, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_AverageGuess =  SD_AverageGuess / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_AverageGuess,
    .groups = 'drop'
  )

# Create the ggplot with error bars
ggplot(summary_table11, aes(y = Mean_AverageGuess, x= Treatment_new, fill=Treatment_new)) +
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


###Averague Guess: USED

# Load necessary libraries
library(ggplot2)
library(wesanderson)
library(gridExtra)

# Step 1: Summarize data and perform Wilcoxon rank-sum tests for each factor

# Summarize and Wilcoxon test for Bonus
summary_bonus <- data_long %>%
  group_by(Bonus) %>%
  summarize(
    Mean_AverageGuess = mean(AverageGuess, na.rm = TRUE),
    SD_AverageGuess = sd(AverageGuess, na.rm = TRUE),
    n = n(),
    SE_AverageGuess = SD_AverageGuess / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_AverageGuess,
    .groups = 'drop'
  )

wilcox_bonus <- wilcox.test(AverageGuess ~ Bonus, data = data_long)
summary_bonus <- summary_bonus %>%
  mutate(
    significance = case_when(
      wilcox_bonus$p.value < 0.001 ~ "***",
      wilcox_bonus$p.value < 0.01 ~ "**",
      wilcox_bonus$p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Summarize and Wilcoxon test for Threshold_high
summary_threshold <- data_long %>%
  group_by(Threshold_high) %>%
  summarize(
    Mean_AverageGuess = mean(AverageGuess, na.rm = TRUE),
    SD_AverageGuess = sd(AverageGuess, na.rm = TRUE),
    n = n(),
    SE_AverageGuess = SD_AverageGuess / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_AverageGuess,
    .groups = 'drop'
  )

wilcox_threshold <- wilcox.test(AverageGuess ~ Threshold_high, data = data_long)
summary_threshold <- summary_threshold %>%
  mutate(
    significance = case_when(
      wilcox_threshold$p.value < 0.001 ~ "***",
      wilcox_threshold$p.value < 0.01 ~ "**",
      wilcox_threshold$p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Summarize and Wilcoxon test for GroupComposition
summary_group <- data_long %>%
  group_by(GroupComposition) %>%
  summarize(
    Mean_AverageGuess = mean(AverageGuess, na.rm = TRUE),
    SD_AverageGuess = sd(AverageGuess, na.rm = TRUE),
    n = n(),
    SE_AverageGuess = SD_AverageGuess / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_AverageGuess,
    .groups = 'drop'
  )

wilcox_group <- wilcox.test(AverageGuess ~ GroupComposition, data = data_long)
summary_group <- summary_group %>%
  mutate(
    significance = case_when(
      wilcox_group$p.value < 0.001 ~ "***",
      wilcox_group$p.value < 0.01 ~ "**",
      wilcox_group$p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Summarize and Wilcoxon test for CampaignFocus
summary_campaign <- data_long %>%
  group_by(Theme) %>%
  summarize(
    Mean_AverageGuess = mean(AverageGuess, na.rm = TRUE),
    SD_AverageGuess = sd(AverageGuess, na.rm = TRUE),
    n = n(),
    SE_AverageGuess = SD_AverageGuess / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_AverageGuess,
    .groups = 'drop'
  )

wilcox_campaign <- wilcox.test(AverageGuess ~ Theme, data = data_long)
summary_campaign <- summary_campaign %>%
  mutate(
    significance = case_when(
      wilcox_campaign$p.value < 0.001 ~ "***",
      wilcox_campaign$p.value < 0.01 ~ "**",
      wilcox_campaign$p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Step 2: Plot the comparisons for each factor with custom color palette and significance stars

p1 <- ggplot(summary_bonus, aes(x = as.factor(Bonus), y = Mean_AverageGuess, color = as.factor(Bonus))) +
  geom_point(position = position_dodge(width = 0.9), size = 4) +
  geom_errorbar(aes(ymin = Mean_AverageGuess - CI_95, ymax = Mean_AverageGuess + CI_95), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = significance, y = Mean_AverageGuess + CI_95 + 0.5), size = 6, vjust = 0) +  # Add stars
  labs(title = "",
       x = "Bonus",
       y = "Mean Group Belief") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +  # Custom labels for Bonus
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Custom color palette for points
  theme(legend.position = "none",
        axis.title.y = element_text(size = 10))

# Plot for Threshold
p2 <- ggplot(summary_threshold, aes(x = as.factor(Threshold_high), y = Mean_AverageGuess, color = as.factor(Threshold_high))) +
  geom_point(position = position_dodge(width = 0.9), size = 4) +
  geom_errorbar(aes(ymin = Mean_AverageGuess - CI_95, ymax = Mean_AverageGuess + CI_95), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = significance, y = Mean_AverageGuess + CI_95 + 0.5), size = 6, vjust = 0) +  # Add stars
  labs(title = "",
       x = "Threshold High",
       y = "Mean Group Belief") +
  scale_x_discrete(labels = c("no" = "No", "yes" = "Yes")) +
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Custom color palette for points
  theme(legend.position = "none",
        axis.title.y = element_text(size = 10))

# Plot for GroupComposition with custom x-axis labels
p3 <- ggplot(summary_group, aes(x = as.factor(GroupComposition), y = Mean_AverageGuess, color = as.factor(GroupComposition))) +
  geom_point(position = position_dodge(width = 0.9), size = 4) +
  geom_errorbar(aes(ymin = Mean_AverageGuess - CI_95, ymax = Mean_AverageGuess + CI_95), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = significance, y = Mean_AverageGuess + CI_95 + 0.5), size = 6, vjust = 0) +  # Add stars
  labs(title = "",
       x = "Group Composition",
       y = "Mean Group Belief") +
  scale_x_discrete(labels = c("0" = "Low Social", "1" = "High Social")) +  # Custom labels for GroupComposition
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Custom color palette for points
  theme(legend.position = "none",
        axis.title.y = element_text(size = 10))

# Plot for CampaignFocus
p4 <- ggplot(summary_campaign, aes(x = as.factor(Theme), y = Mean_AverageGuess, color = as.factor(Theme))) +
  geom_point(position = position_dodge(width = 0.9), size = 4) +
  geom_errorbar(aes(ymin = Mean_AverageGuess - CI_95, ymax = Mean_AverageGuess + CI_95), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = significance, y = Mean_AverageGuess + CI_95 + 0.5), size = 6, vjust = 0) +  # Add stars
  labs(title = "",
       x = "Campaign Focus",
       y = "Mean Group Belief") +
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Custom color palette for points
  theme(legend.position = "none",
        axis.title.y = element_text(size = 10))


# Step 3: Arrange the plots in a grid
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Step 3: Arrange the plots in a grid
grid.arrange(p1, p2, ncol = 2)

# Step 3: Arrange the plots in a grid
grid.arrange(p3, p4, ncol = 2)




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
ggplot(summary_table11, aes(y = Mean_Investment, x = GroupType, color = GroupType)) +
  geom_point(position = position_dodge(width = 0.9), size = 6) +  # Change to point plot
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = significance, 
        y = Mean_Investment + CI_95 ),  # Position text above error bars
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 8 
  ) +
  labs(title = "", 
       x = "", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Use color for points
  theme(axis.text = element_text(size = 15), 
        plot.title = element_text(size = 15),  # Adjust font size for axis text
        axis.title = element_text(size = 15),  # Adjust font size for axis titles
        legend.position = "none") 
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

plot2<- ggplot(summary_table11, aes(y = Mean_Investment, x = Theme, color = Theme)) +
  geom_point(position = position_dodge(width = 0.9), size = 6) +  # Use point plot with larger points
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "", 
       x = "", 
       y = "Mean Investment",
       color = "Campaign Focus") +  # Change fill to color for points
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Apply color palette for points
  theme(axis.text = element_text(size = 15), 
        plot.title = element_text(size = 15),  # Adjust font size of plot title
        axis.title = element_text(size = 15),  # Adjust font size of axis titles
        legend.position = "none")  # Remove legend if not needed

plot2
wilcox.test(data_long$Investment ~ data_long$Theme)


# Step 3: Arrange the plots in a grid
grid.arrange(plot1,plot2, ncol = 2)


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


# Perform Mann-Whitney U test (pairwise comparisons)
pairwise_test <- pairwise.wilcox.test(data_long$AverageGuess, data_long$Treatment_new, p.adjust.method = "BH")

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

##USED
ggplot(data_long, aes(x = Round, y = Investment * 10, color = Round, group = Round)) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  geom_point(stat = "summary", fun = mean, size = 4) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.2) +
  geom_hline(data = thresholds, aes(yintercept = Threshold), linetype = "dashed", color = "black") +
  scale_colour_manual(values = wes_palette("Darjeeling2")) +
  labs(title = "",
       x = "",
       y = "Mean Group Investment") +
  # Rename x-axis labels
  scale_x_discrete(labels = c("1" = "Low T, No Bonus",
                              "2" = "High T, No Bonus",
                              "3" = "Low T,  Bonus",
                              "4" = "High T, Bonus")) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend



# Perform Mann-Whitney U test (pairwise comparisons)
pairwise_test <- pairwise.wilcox.test(data_long$Investment, data_long$Round, p.adjust.method = "none")

# Print summary statistics and pairwise test results
print(pairwise_test)

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



#Combine the two variables into a long format for easier plotting with ggplot2
data_long_combined <- data_long %>%
  mutate(InvestmentScaled = Investment * 10) %>%
  pivot_longer(cols = c(AverageGuess, InvestmentScaled), 
               names_to = "Measure", 
               values_to = "Value")

# Plot both AverageGuess and InvestmentScaled in the same plot
ggplot(data_long_combined, aes(x = Round, y = Value, color = GroupType, linetype = Measure, group = interaction(GroupType, Measure))) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2) +
  geom_hline(data = thresholds, aes(yintercept = Threshold), linetype = "dashed", color = "black") +
  scale_colour_manual(values = wes_palette("Darjeeling2")) +
  labs(title = "Average Belief and Investment by Group Type across Rounds",
       x = "Round",
       y = "Value (Average Belief or Investment * 10)",
       color = "Group Type",
       linetype = "Measure") +
  theme_minimal()

##95% confidence intervall:

# Function to calculate the 95% confidence interval
mean_ci <- function(x) {
  m <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
  ci_lower <- m - 1.96 * se
  ci_upper <- m + 1.96 * se
  return(c(y = m, ymin = ci_lower, ymax = ci_upper))
}

# Combine the two variables into a long format for easier plotting with ggplot2
data_long_combined <- data_long %>%
  mutate(InvestmentScaled = Investment * 10) %>%
  pivot_longer(cols = c(AverageGuess, InvestmentScaled), 
               names_to = "Measure", 
               values_to = "Value")

# Plot both AverageGuess and InvestmentScaled in the same plot with 95% CI error bars
ggplot(data_long_combined, aes(x = Round, y = Value, color = GroupType, linetype = Measure, group = interaction(GroupType, Measure))) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  geom_errorbar(stat = "summary", fun.data = mean_ci, width = 0.2) +  # 95% confidence intervals
  geom_hline(data = thresholds, aes(yintercept = Threshold), linetype = "dashed", color = "black") +
  scale_colour_manual(values = wes_palette("Darjeeling2")) +
  labs(title = "Average Belief and Investment by Group Type across Rounds",
       x = "Round",
       y = "Value (Average Belief or Investment * 10)",
       color = "Group Type",
       linetype = "Measure") +
  theme_minimal()



###Perception Guess

# Assuming data_long contains the columns 'Round', 'AverageGuess', 'Investment', and 'GroupType'

# Create a new column for the difference
data_long$Difference <-   (data_long$Investment * 10) - data_long$AverageGuess

# Plot the difference
ggplot(data_long, aes(x = Threshold_high, y = Difference, color = GroupType, group = GroupType)) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_manual(values = wes_palette("Darjeeling2")) +
  labs(title = "Difference between Investment and Average Guess by Group Type across Rounds",
       x = "Round",
       y = "Difference (AverageGuess - Investment*10)",
       color = "Group Type") +
  theme_minimal()

wilcox.test(data_long$Difference ~ data_long$GroupType)

wilcox.test(Difference ~ GroupType, data = subset(data_long, Threshold_high == "yes"))

wilcox.test(Difference ~ GroupType, data = subset(data_long, Threshold_high == "no"))



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
ggplot(summary_table7, aes(x = Treatment_new, y = Mean_Investment, color = Bonus)) +
  geom_point(position = position_dodge(width = 0.9), size = 5) +  # Use larger points in the plot
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = significance, 
        y = Mean_Investment + CI_95 -0.5),  # Adjust this value to place the stars above the error bars
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 8
  ) +
  # Add a vertical line between Low Social and High Social groups
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", size = 0.8) +  # Adjust position if necessary
  labs(title = "", 
       x = "", 
       y = "Mean Investment",
       color = "Bonus") +  # Add legend title for Bonus
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Use color for points
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Adjust point size in the legend
  theme(axis.text = element_text(size = 15), 
        plot.title = element_text(size = 15),  # Adjust font size of plot title
        axis.title = element_text(size = 15),  # Adjust font size of axis titles
        legend.text = element_text(size = 15),  # Font size of legend text
        legend.title = element_text(size = 15))  # Font size of legend title

###Seperate 


##BONUS: Aggregate Section: USED
# Step 1: Calculate the overall means for ordering
overall_means <- data_long %>%
  group_by(GroupType) %>%
  summarize(Overall_Mean_Investment = mean(Investment, na.rm = TRUE)) %>%
  arrange(Overall_Mean_Investment)

# Step 2: Summarize data by Bonus
summary_table7 <- data_long %>%
  group_by(Bonus) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE),
    n = n(),
    SE_Investment = SD_Investment / sqrt(n),
    CI_95 = qt(0.975, df = n - 1) * SE_Investment,
    .groups = 'drop'
  ) %>%
  mutate(Bonus = ifelse(Bonus == 0, "No", "Yes"))

# Step 3: Perform Wilcoxon test for Bonus
wilcox_test <- wilcox.test(Investment ~ Bonus, data = data_long)
p_value <- wilcox_test$p.value

# Step 4: Determine significance level
significance <- ifelse(p_value < 0.01, "***", ifelse(p_value < 0.05, "**", ifelse(p_value < 0.1, "*", "")))

# Step 5: Create the ggplot with error bars and significance stars

# Create the ggplot with error bars, significance stars, and Bonus labels
ggplot(summary_table7, aes(x = Bonus, y = Mean_Investment, color = Bonus)) +
  geom_point(position = position_dodge(width = 0.9), size = 6) +  # Use points instead of bars
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2) +  # No need for position_dodge() for error bars
  geom_text(
    aes(label = significance, y = Mean_Investment + CI_95 + 3),  # Significance stars above points
    vjust = 0,
    size = 8
  ) +
  labs(title = "", y = "Mean Investment") +
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Use color for points
  ylim(min(summary_table7$Mean_Investment - summary_table7$CI_95) - 5, NA) +  # Adjust y-axis limits
  theme(axis.text = element_text(size = 15), 
        plot.title = element_text(size = 15),  # Adjust font size of plot title
        axis.title = element_text(size = 15),  # Adjust font size of axis titles
        legend.position = "none")  # Remove the legend





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


###Bonus and Beliefs:here

#Melt the data into long format for Bonus_yes
data_beliefs_bonus <- melt(data_long, id.vars = "Bonus_yes", 
                           measure.vars = c("B_0", "B_1_99", "B_100_199", "B_200_299", "B_300"))

# Create a new column "category" based on the "value" column
data_beliefs_bonus$category <- ifelse(data_beliefs_bonus$variable == "B_0" | data_beliefs_bonus$variable == "B_1_99", "0 to 100",
                                      ifelse(data_beliefs_bonus$variable == "B_100_199", "100 to 199", "200 to 300"))


# Group by "Bonus_yes" and "category" and summarize
data_beliefs_combined_bonus <- data_beliefs_bonus %>%
  group_by(Bonus_yes, variable) %>%
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

print(data_beliefs_combined_bonus)

wilcox.test(AverageGuess~Bonus_yes,data=data_long)

wilcox.test(B_100_199~Bonus_yes,data=data_long)


# Perform Wilcoxon tests and get p-values for Bonus_yes
wilcox_test_results_bonus <- data_beliefs_bonus %>%
  group_by(category) %>%
  summarize(p_value = wilcox.test(value ~ Bonus_yes)$p.value, .groups = 'drop') %>%
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

# Merge significance results with the data for plotting
data_beliefs_combined_bonus <- data_beliefs_combined_bonus %>%
  left_join(wilcox_test_results_bonus, by = "variable")

# Plot: Distribution of Investment Beliefs by Bonus (Bonus_yes)
ggplot(data_beliefs_combined_bonus, aes(x = variable, y = freq, fill = as.character(Bonus_yes))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = freq - CI_95, ymax = freq + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Investment Beliefs by Bonus",
       x = "Belief Categories", 
       y = "Frequency",
       fill= "Bonus") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  geom_text(
    aes(label = significance, 
        y = freq + CI_95 + 0.01),  # Adjust this value to place the stars above the error bars
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 7
  ) +
  theme(
    axis.text = element_text(size = 15),  # Font size of axis text
    axis.title.x = element_text(size = 17), # Font size of axis titles
    axis.title.y = element_text(size = 15), # Font size of axis titles
    plot.title = element_text(size = 15),   # Font size of plot title
    legend.text = element_text(size = 15),  # Font size of legend text
    legend.title = element_text(size = 17)  # Font size of legend title
  )




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
  group_by(Bonus,Threshold_high) %>%
  summarize(
    Mean_Investment = mean(Investment, na.rm = TRUE),
    SD_Investment = sd(Investment, na.rm = TRUE), # Calculate standard deviation
    n = n(), # Count of observations
    SE_Investment = SD_Investment / sqrt(n), # Calculate standard error
    CI_95 = qt(0.975, df = n - 1) * SE_Investment, # Calculate 95% confidence interval
    .groups = 'drop' # Drop grouping structure
  ) 

# Step 3: Perform Wilcoxon test for each Threshold_high group
wilcox_test_results <- data_long %>%
  group_by(Threshold_high) %>%
  summarize(p_value = wilcox.test(Investment ~ Bonus)$p.value, .groups = 'drop') %>%
  mutate(
    significance = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    )
  )

# Step 4: Merge significance stars into the summary table
summary_table7 <- summary_table7 %>%
  left_join(wilcox_test_results, by = "Threshold_high")

# Step 5: Create the ggplot with error bars and significance stars: USED
ggplot(summary_table7, aes(x = as.factor(Threshold_high), y = Mean_Investment, color = Bonus)) +
  geom_point(position = position_dodge(width = 0.9), size = 5) +  # Larger points in the plot
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = significance, y = Mean_Investment + CI_95 + 2),  # Significance stars
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 5
  ) +
  labs(title = "", 
       x = "Threshold High", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Custom color palette for points
  scale_x_discrete(labels = c("no" = "No", "yes" = "Yes")) +  # Rename x-axis labels
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Smaller points in the legend
  theme(axis.text = element_text(size = 12), 
        plot.title = element_text(size = 15),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 13),  # Legend text size
        legend.title = element_text(size = 13))  # Legend title size


###GROUP COMPOSITION

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

# Step 3: Perform Wilcoxon test for each Threshold_high group
wilcox_test_results <- data_long %>%
  group_by(Threshold_high) %>%
  summarize(p_value = wilcox.test(Investment ~ GroupType)$p.value, .groups = 'drop') %>%
  mutate(
    significance = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    )
  )

# Step 4: Merge significance stars into the summary table
summary_table7 <- summary_table7 %>%
  left_join(wilcox_test_results, by = "Threshold_high")

# Step 5: Create the ggplot with error bars and significance stars: USED
ggplot(summary_table7, aes(x = as.factor(Threshold_high), y = Mean_Investment, color = GroupType)) +
  geom_point(position = position_dodge(width = 0.9), size = 5) +  # Larger points in the plot
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = significance, y = Mean_Investment + CI_95 + 2),  # Significance stars
    position = position_dodge(width = 0.9),
    vjust = 0,
    size = 5
  ) +
  labs(title = "", 
       x = "Threshold High", 
       y = "Mean Investment",
       color = "Group Composition") +
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Custom color palette for points
  scale_x_discrete(labels = c("no" = "No", "yes" = "Yes")) +  # Rename x-axis labels
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Smaller points in the legend
  theme(axis.text = element_text(size = 12), 
        plot.title = element_text(size = 15),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 13),  # Legend text size
        legend.title = element_text(size = 13))  # Legend title size



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
ggplot(summary_table7, aes(x = as.factor(Threshold_high), y = Mean_Investment, color = as.factor(Threshold_high))) +
  geom_point(position = position_dodge(width = 0.9), size = 5) +  # Use points instead of bars
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "", 
       x = "", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_x_discrete(labels = c("no" = "No", "yes" = "Yes")) +  # Rename x-axis labels
  scale_color_manual(values = wes_palette("Darjeeling2")) +  # Custom color palette for points
  theme(axis.text = element_text(size = 15), 
        plot.title = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none")  # Remove the legend


###Qualtiative Analysis

library(tm)
library(wordcloud)
library(RColorBrewer)

# Remove NA values from the text variable
text_data <- na.omit(data$Q280_5_TEXT)

# Create a text corpus
text_corpus <- Corpus(VectorSource(text_data))

# Clean the text: convert to lower case, remove punctuation, numbers, stopwords, etc.
text_corpus <- text_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

dtm <- TermDocumentMatrix(text_corpus)
matrix <- as.matrix(dtm)

# Get word frequencies
word_freq <- sort(rowSums(matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

# Define your custom color palette (Darjeeling2 from wesanderson, for example)
custom_palette <- wes_palette("Darjeeling2", n = 12, type = "continuous")


set.seed(12345)  # For reproducibility
wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, min.freq = 2,
          max.words = 200, random.order = FALSE, rot.per = 0.35, 
          colors = custom_palette)


data$Q280_5_TEXT
