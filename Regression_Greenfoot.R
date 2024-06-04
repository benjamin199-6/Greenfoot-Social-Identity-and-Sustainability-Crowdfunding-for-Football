# Clear existing workspace to start fresh
rm(list = ls())

setwd("/Users/benjamin/Downloads")

# Load necessary libraries
library(haven)
library(dplyr)
library(tidyr)

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
    Threshold_high = ifelse(Threshold_high == "1", 1, 0),
    Threshold_low = ifelse(Threshold_high == "0", 1, 0),
    Bonus_yes = ifelse(Bonus == "1", 1, 0),
    Bonus_no = ifelse(Bonus == "0", 1, 0),
    GroupType = ifelse(GroupComposition == "1", "Homogen", "Heterogen"),
    Theme = ifelse(ProjectType == "1", "Football", "Neutral")
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

data_long$SimilarityToGroupMembers=as.factor(data_long$SimilarityToGroupMembers)
data_long$InterestInFootball=as.numeric(data_long$InterestInFootball)
names(data_long)
data_long$Group_Members_Combined=as.factor(data_long$Group_Members_Combined)

data_long$FavoriteFootballTeam=as.factor(data_long$FavoriteFootballTeam)
data_long$WillingnessToTakeRisks
data_long=data_long%>% filter(Sex %in% c("Male","Female"))
data_long <- data_long %>%
  mutate(
    Concern_ClimateChange = ifelse(Concern_ClimateChange > 3, 1, 0),
    Concern_ClimateChange = as.factor(Concern_ClimateChange)
  )








library(sjPlot)
### Regression model 
data_long_big=data_long%>% filter(Treatment !="Control-Small")

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
