# Clear existing workspace to start fresh
rm(list = ls())

setwd("/Users/rapha/Downloads")

# Load necessary libraries
library(haven)
library(dplyr)
library(tidyr)

require("wesanderson")
library("wesanderson")


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
    Threshold_high = ifelse(Threshold_high == "1", "Yes", "No"),
    Threshold_low = ifelse(Threshold_high == "0", 1, 0),
    Bonus_yes = ifelse(Bonus == "1", 1, 0),
    Bonus_no = ifelse(Bonus == "0", 1, 0),
    GroupType = ifelse(GroupComposition == "1", "Same Club", "Random"),
    Theme = ifelse(ProjectType == "1", "Football", "Neutral"),
  )

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



library(ggplot2)

# library(writexl)
# write_xlsx(pivot_table, path = "summary_table.xlsx")

data_long$WillingnessToTakeRisks=as.numeric(data_long$WillingnessToTakeRisks)
data_long$AverageGuess=as.numeric(data_long$AverageGuess)
data_long$Bonus=as.factor(data_long$Bonus)
data_long$Threshold_high=as.factor(data_long$Threshold_high)
data_long <- data_long %>%
  mutate(SimilarityToGroupMembers = recode(SimilarityToGroupMembers,
                                           `1` = 1,
                                           `3` = 2,
                                           `4` = 3,
                                           `5` = 4,
                                           `28` = 5))

data_long$SimilarityToGroupMembers=as.numeric(data_long$SimilarityToGroupMembers)
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
data_long <- data_long %>%
  mutate(HouseholdIncome = recode(HouseholdIncome,
                                 `1` = "Below 10,000",
                                 `2` = "10,000 - 14,999",
                                 `3` = "15,000 - 24,999",
                                 `4` = "25,000 - 34,999",
                                 `5` = "35,000 - 44,999"))


data_long$EducationLevel = as.numeric(data_long$EducationLevel)

data_long$HouseholdIncome = as.numeric(data_long$HouseholdIncome )

library(expss)
data_long = apply_labels(data_long,
                      HouseholdIncome = "Annual Household Income After Tax")

data_long = apply_labels(data_long,
                         EducationLevel = "Higher Education")

library(sjPlot)
### Regression model 
data_long_big=data_long%>% filter(Treatment !="Control-Small")





#Regression with all

##Baseline
regression_model <- lm(Investment ~ GroupType + Theme + Bonus_yes + Threshold_high, data = data_long)
tab_model(regression_model)


#####Further

regression_model <- lm(Investment ~ GroupType + Theme +AverageGuess+ Bonus_yes + Threshold_high +WillingnessToTakeRisks+ Sex+HouseholdIncome + EducationLevel+InterestInFootball +
                    +SimilarityToGroupMembers, data = data_long)
tab_model(regression_model)

####

library(lme4)

regression_model <- lmer(Investment ~ GroupType + Theme +GroupType *Theme +AverageGuess+ Bonus_yes + Threshold_high +WillingnessToTakeRisks+ Concern_ClimateChange+ Sex+HouseholdIncome + EducationLevel+InterestInFootball +
                         +SimilarityToGroupMembers, data = data_long)
tab_model(regression_model)

####

regression_model <- lm(Investment ~ GroupType + Theme + Bonus_yes + Threshold_high+WillingnessToTakeRisks+ Concern_ClimateChange+ Sex+HouseholdIncome + EducationLevel+InterestInFootball +
                         +SimilarityToGroupMembers +Concern_ClimateChange , data = data_long)
tab_model(regression_model)


###


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


###Beliefs
library(ggplot2)
library(reshape2)

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

# Create the bar plot with error bars
ggplot(data_beliefs_combined, aes(x = variable, y = freq, fill = Treatment_new)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Beliefs across Interaction of Campaign Focus and Group Composition", 
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

# Create the bar plot with error bars
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



##




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
  mutate(variable = recode(variable, 
                           "B_0" = "0", 
                           "B_1_99" = "1-99", 
                           "B_100_199" = "100-199", 
                           "B_200_299" = "200-299", 
                           "B_300" = "300"))


# Create the bar plot with error bars
ggplot(data_beliefs_combined_GT, aes(x = variable, y = freq, fill = GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = freq - CI_95, ymax = freq + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Investment Beliefs by Group Type", 
       x = "Beliefs", 
       y = "Frequency",
       fill="Group Composition") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))


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

# Create the bar plot with error bars
ggplot(data_beliefs_combined_GT, aes(x = category, y = freq, fill = GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = freq - CI_95, ymax = freq + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Investment Beliefs by Group Composition", 
       x = "Beliefs", 
       y = "Frequency",
       fill= "Group Composition") +
  theme_minimal()+
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
  mutate(variable = recode(variable, 
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
       y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))



##



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

##





wilcox.test(data_long$Investment ~ data_long$GroupType)

wilcox.test(data_long$Investment ~ data_long$Theme)





###FOR THEME SIEHT MAN NICHT VIEL ALSO NICHT GEMACHT


####Barblot for group type and theme


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

# Create the ggplot with error bars
ggplot(summary_table2, aes(x = Theme, y = Mean_Investment, fill = GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by Group Composition and Campaign Focus", 
       x = "Campaign Focus", 
       y = "Mean Investment",
       fill="Group Composition") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))


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

# Create the ggplot with error bars
ggplot(summary_table11, aes(y = Mean_Investment, x= GroupType, fill=GroupType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by Group Composition", 
       x = "", 
       y = "Mean Investment",
       fill="Group Composition") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))




##Braplot only Group Theme
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

# Create the ggplot with error bars
ggplot(summary_table11, aes(y = Mean_Investment, x= Theme, fill=Theme)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by Campaign Focus", 
       x = "", 
       y = "Mean Investment",
       fill= "Campaign Focus") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling2"))



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
  labs(title = "Mean Investment by Interaction of Group Composition and Campaign Focus", 
       x = "", 
       y = "Mean Investment",
       fill = "Focus-Composistion") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))



# Perform Mann-Whitney U test (pairwise comparisons)
pairwise_test <- pairwise.wilcox.test(data_long$Investment, data_long$Treatment_new, p.adjust.method = "BH")

# Print summary statistics and pairwise test results
print(summary_stats)
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
  labs(title = "Average Investment by Group Composition across Rounds",
       x = "Round",
       y = "Mean Investment",
       color = "Group Composition") +
  theme_minimal()

ggplot(data_long, aes(x = Round, y = Investment*10, color = GroupType, group = GroupType)) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.2) +
  geom_hline(data = thresholds, aes(yintercept = Threshold), linetype = "dashed", color = "black") +
  scale_colour_manual(values = wes_palette("Darjeeling2")) +
  labs(title = "Group Investment by Group Composition across Rounds",
       x = "Round",
       y = "Mean Group Investment",
       color = "Group Composition") +
  theme_minimal()




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

# Create the ggplot with error bars
ggplot(summary_table7, aes(x = Treatment_new, y = Mean_Investment, fill = Bonus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by Group Composition and Bonus", 
       x = "", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))


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


heterogen_data <- data_long[data_long$GroupType == "Heterogen", ]
wilcox.test(heterogen_data$Investment ~ heterogen_data$Bonus_yes)



homogen_data <- data_long[data_long$GroupType == "Homogen", ]
wilcox.test(homogen_data$Investment ~ homogen_data$Bonus_yes)


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
ggplot(summary_table7, aes(x = GroupType, y = Mean_Investment, fill=Threshold_high)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by Group Composition and Threshold", 
       x = "", 
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
summary_table_8 <- data_long %>%
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
ggplot(summary_table_8, aes(x = Threshold_high, y = Mean_Investment, fill=Threshold_high)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Investment by Threshold", 
       x = "", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Darjeeling2"))


##Combine Bonus and Threshold in one plot


# Assuming summary_table7 and summary_table_8 are already created

# Add a new column to each table to identify the type
summary_table7 <- summary_table7 %>%
  mutate(Type = "Bonus")

summary_table_8 <- summary_table_8 %>%
  mutate(Type = "Threshold_high")

# Rename the columns to be consistent for merging
summary_table_8 <- summary_table_8 %>%
  rename(Value = Threshold_high)

summary_table7 <- summary_table7 %>%
  rename(Value = Bonus)

# Combine the tables
combined_summary <- bind_rows(summary_table7, summary_table_8)

# Extract colors from the Wes Anderson palette
wes_colors <- wes_palette("Darjeeling2", 4)

# Assign custom colors for each group
custom_colors <- c("Bonus_No" = wes_colors[1], "Bonus_Yes" = wes_colors[2],
                   "Threshold_high_No" = wes_colors[1], "Threshold_high_Yes" = wes_colors[2])

# Create a combined group variable for color mapping
combined_summary <- combined_summary %>%
  mutate(Group_Type = paste(Type, Value, sep = "_"))

# Create the combined ggplot
ggplot(combined_summary, aes(x = Value, y = Mean_Investment, fill = Group_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Investment - CI_95, ymax = Mean_Investment + CI_95), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~Type, scales = "free_x") +
  labs(title = "Mean Investment by Bonus and Threshold", 
       x = "", 
       y = "Mean Investment") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  theme(legend.position = "none")

