# 0_read_data.R
# read the video survey data
# July 2021

pretest1 <- read_csv("data/Payment Survey + RIRT + PGG + US Vaccine Incentives Master_July 9, 2021_04.47.csv") %>%
  filter(!Q1.2 == "No", !Q1.3 == "No")

#pretest2 <- read_csv("Matze 5 Juillet 2021/RIRT  + US Vaccine Incentives Master Copy CR 2_July #5, 2021_02.32.csv")[-(1:2),] %>%
#  filter(!Q1.2 == "No", !Q1.3 == "No")

pretest3 <- read_csv("data/RIRT  + US Vaccine Incentives Master Copy CR_July 5, 2021_02.30.csv")[-(1:2),] %>%
  filter(!Q1.2 == "No", !Q1.3 == "No")

pretest4 <- read_csv("data/RIRT  + US Vaccine Incentives Master Copy LUCID - 2nd testing_July 5, 2021_02.31.csv")[-(1:2),] %>%
  filter(!Q1.2 == "No", !Q1.3 == "No")

pretest5 <- read_csv("data/RIRT  + US Vaccine Incentives Master Copy LUCID_July 5, 2021_02.30.csv")[-(1:2),] %>%
  filter(!Q1.2 == "No", !Q1.3 == "No")

pretest6 <- read_csv("data/RIRT  + US Vaccine Incentives Master Copy CR 2_July 12, 2021_01.00.csv")[-(1:2),] %>%
  filter(!Q1.2 == "No", !Q1.3 == "No")

#pretest7 <- read_csv("CR2/Payment Survey + RIRT + PGG + US Vaccine Incentives Master_July 3, 2021_05.11.csv")[-(1:2),] %>%
#  filter(!Q1.2 == "No", !Q1.3 == "No")

#pretest8 <- read_csv("CR2/RIRT  + US Vaccine Incentives Master Copy CR 2_July 4, 2021_10.csv")[-(1:2),] %>%
#  filter(!Q1.2 == "No", !Q1.3 == "No")

pretest1$Treatment <- pretest1$Treatment %>%
  factor(levels = c("treatment1", "treatment2", "treatment3"))

pretest1 <- pretest1 %>%
  mutate(clicked = if_else(Q3.1 == "No, I haven't", clicked, NA_character_)) %>%
  mutate(pool="Facebook")

pretest1$clicked <- as.numeric(pretest1$clicked)

##pretest2$Treatment <- pretest2$Treatment %>%
#  factor(levels = c("treatment1", "treatment2", "treatment3"))

#pretest2 <- pretest2 %>%
#  mutate(clicked = if_else(Q3.1 == "No, I haven't", clicked, NA_character_))

#pretest2$clicked <- as.numeric(pretest2$clicked)

pretest3$Treatment <- pretest3$Treatment %>%
  factor(levels = c("treatment1", "treatment2", "treatment3"))

pretest3 <- pretest3 %>%
  mutate(clicked = if_else(Q3.1 == "No, I haven't", clicked, NA_character_))  %>%
  mutate(pool="CloudResearch")

pretest3$clicked <- as.numeric(pretest3$clicked)

pretest4 <- pretest4 %>%
  mutate(clicked = if_else(Q3.1 == "No, I haven't", clicked, NA_character_)) %>%
  mutate(pool="Lucid")

pretest4$clicked <- as.numeric(pretest4$clicked)

pretest5 <- pretest5 %>%
  mutate(clicked = if_else(Q3.1 == "No, I haven't", clicked, NA_character_)) %>%
  mutate(pool="Lucid")

pretest5$clicked <- as.numeric(pretest5$clicked)

pretest6 <- pretest6 %>%
  mutate(clicked = if_else(Q3.1 == "No, I haven't", clicked, NA_character_)) %>%
  mutate(pool="CloudResearch")

pretest6$clicked <- as.numeric(pretest6$clicked)

#pretest7 <- pretest7 %>%
#  mutate(clicked = if_else(Q3.1 == "No, I haven't", clicked, NA_character_))

#pretest7$clicked <- as.numeric(pretest7$clicked)

#pretest8 <- pretest8 %>%
#  mutate(clicked = if_else(Q3.1 == "No, I haven't", clicked, NA_character_))

#pretest8$clicked <- as.numeric(pretest8$clicked)

# Read in Presidential Vote Data
# Adrian: here i create a dummy variable for states with Trump vote > 50 

Trump <- read_csv("data/1976-2020-president.csv") %>%
  filter(year == "2020", candidate == "TRUMP, DONALD J.")

Trump$Trumppercent <- Trump$candidatevotes/Trump$totalvotes
Trump$state <- tolower(Trump$state)
Trump$state <- str_to_title(Trump$state)

Trump <- Trump %>%
  select(c(year, state, candidatevotes, totalvotes, Trumppercent))

Trump <- Trump %>%
  mutate(Trumphi = if_else(Trumppercent>.50, 1, 0))


# Merge and Rename Variables #

pretest1to6 <- bind_rows(pretest1, pretest3, pretest4, pretest5, pretest6, .id='file') %>%
  filter(Q3.1 == "No, I haven't") # only eligible are those who have not had vaccine
# table(table(pretest1to6$ResponseId)) # check for duplicates
# with(pretest1to6, table(file, is.na(Treatment))) # check missing
# with(pretest1to6, table(file, Treatment))

pretest1to6 <- pretest1to6 %>%
  rename(age = Q2.2,
         gender = Q2.3,
         person1_ans = "1_Q11.2",
         person2_ans = "2_Q11.2",
         person3_ans = "3_Q11.2",
         person4_ans = "4_Q11.2",
         person5_ans = "5_Q11.2",
         person6_ans = "6_Q11.2",
         person7_ans = "7_Q11.2",
         person8_ans = "8_Q11.2",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         state = Q2.5_1)


# Merge in Trump vote data

pretest1to6 <- merge(pretest1to6, Trump, by.x = "state", by.y="state")


# Education
pretest1to6 <- pretest1to6 %>%
  #  mutate(Education = if_else(Q2.4 == "Doctorate", "High", 
  #                       if_else(Q2.4 == "Associate degree", "Medium",
  #                        if_else(Q2.4 == "Some college education, no degree", "Medium",
  #                        if_else(Q2.4 == "Master's degree (including professional degrees, or equivalent)", "High", 
  #                        if_else(Q2.4 == "Some high school", "Low", 
  #                        if_else(Q2.4 == "High school graduate, diploma (or equivalent)", "Low", 
  #                        if_else(Q2.4 == "Training/vocational college", "Medium",
  #                        if_else(Q2.4 == "Nursery to 8th Grade", "Low",
  #                        if_else(Q2.4 == "None", "Low",
  #                         if_else(Q2.4 == "Bachelor's degree","High", NA_character_)))))))))))
  mutate(Education = 
           case_when(Q2.4 == "Doctorate" ~ "High", 
                     Q2.4 == "Associate degree" ~ "Medium",
                     Q2.4 == "Some college education, no degree" ~ "Medium",
                     Q2.4 == "Master's degree (including professional degrees, or equivalent)" ~ "High", 
                     Q2.4 == "Some high school" ~ "Low", 
                     Q2.4 == "High school graduate, diploma (or equivalent)" ~ "Low", 
                     Q2.4 == "Training/vocational college"~  "Medium",
                     Q2.4 == "Nursery to 8th Grade" ~ "Low",
                     Q2.4 == "None" ~ "Low",
                     Q2.4 == "Bachelor's degree" ~ "High"))

# Race
pretest1to6 <- pretest1to6 %>%
  mutate(Race = if_else(Q2.6 == "Black or African American", "Black", 
                        if_else(Q2.6 == "White", "White", "Other")))

pretest1to6 <- pretest1to6 %>%
  mutate(White = if_else(Q2.6 == "White", "White", "Non-white"))

# Age
pretest1to6 <- mutate(
  age = as.numeric(age),
  pretest1to6, Age_cat = if_else(age < 40, "Young", 
                                 if_else(age > 39, "Old", NA_character_)))

# Here I create a version of the data set with Treatments with proper labels

pretest_final <- pretest1to6 %>%
  mutate(Treatment2 = if_else(Treatment == "treatment1", "CDC Health Information", 
                              if_else(Treatment == "treatment2", "Lottery Incentive",
                                      if_else(Treatment == "treatment3", "Cash Voucher Incentive", NA_character_))))
# make reference group:
pretest_final = mutate(pretest_final, 
                       Treatment2 = factor(Treatment2),
                       Treatment2 = relevel(Treatment2, ref='CDC Health Information')) # set reference level


save(pretest_final, file='data/analysis_ready.RData')