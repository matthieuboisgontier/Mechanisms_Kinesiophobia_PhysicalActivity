# load packages -----
library("languageR")
library("here")
library("tidyverse")
library("skimr")
library("janitor")
library("ggbeeswarm")
library ("psych")
library("dplyr")
library("ggExtra")
## To estimate models
library("lmerTest")
## For figures
library("effects")
library("ggplot2")
# For testing LMM assumptions
library("lattice")
# For mediation analysis
library("lavaan")

# read in data ------
data1 <- read.csv(here("datasets_arthritis", "2024-05-09_data_arthritis.csv"))

# tidying data  ------
# Renaming factors ----- 
names(data1)
cleandata1 <- data1 %>%
  clean_names() %>%
  rename(c8 = chronic8_arthritis)  %>%
  rename(c16 = chronic16_rheumatoid_arthritis) %>%
  rename(exptime = total_elapsed_time) %>%
  rename(down = responsekey_down) %>%
  rename(language = subj) %>%
  rename(up = responsekey_up) %>%
  rename(daytime = time)
names(cleandata1)
unique(data1$chronic8_arthritis)
unique(data1$chronic12_parkinson)
unique(data1$id)
unique(data1$gender)
# adding column with trial number of the whole study
cleandata1 <- cleandata1 %>%
  group_by(id) %>%
  mutate(trial_number_study = 1:n()) %>%
  ungroup()

# adding column with mean attitude (attitude1 + attitude2)/2
#unique(cleandata1$attitude1)
#unique(cleandata1$attitude2)
cleandata1 <- cleandata1 %>%
  mutate(attitude = (attitude1 + attitude2))
#unique(cleandata1$attitude)
#hist(cleandata1$attitude)

#creating bmi variable
cleandata1 <- cleandata1 %>%
  mutate(height_m = (height/100))
cleandata1 <- cleandata1 %>%
  mutate(bmi = (weight/I(height_m^2)))
#plot(cleandata1$bmi)
describe(cleandata1$weight)



cleanlightdata1 <- cleandata1[cleandata1$trialcode != "fixation" & 
                                cleandata1$trialcode != "reminder" & 
                                cleandata1$trialcode != "too_slow" &
                                cleandata1$trialcode != "instructionimages"&
                                cleandata1$trialcode != "error" 
                              , ] 


# removing useless conditions from the "trialcode" column ------
# removing first 10 trials of each block ------
unique (cleandata1$age)
cleanlightdata1 <- cleandata1[cleandata1$trialcode != "fixation" & 
                                      cleandata1$trialcode != "reminder" & 
                                      cleandata1$trialcode != "too_slow" &
                                      cleandata1$trialcode != "instructionimages"&
                                      cleandata1$trialcode != "error" &
                                      cleandata1$latency < 3000 &
                                      cleandata1$latency > 150 &
                                      cleandata1$height < 250 &
                                      cleandata1$height > 50 &
                                      cleandata1$weight < 300 &
                                      cleandata1$weight > 30 &

                                # remove 3 first trial of each condition
                                      cleandata1$trial_num != "1" & 
                                      cleandata1$trial_num != "2" & 
                                      cleandata1$trial_num != "3" &
                                      cleandata1$trial_num != "4" &
                                      cleandata1$trial_num != "5" &
                                      cleandata1$trial_num != "6" &
                                # remove 10 first trials of the study
                                      cleandata1$trial_number_study != "7" &
                                      cleandata1$trial_number_study != "8" & 
                                      cleandata1$trial_number_study != "9" &
                                      cleandata1$trial_number_study != "10" &
                                      cleandata1$trial_number_study != "11" &
                                      cleandata1$trial_number_study != "12" &
                                      cleandata1$trial_number_study != "13" &
                                      cleandata1$trial_number_study != "14" &
                                      cleandata1$trial_number_study != "15" &
                                      cleandata1$trial_number_study != "16" &
                                      cleandata1$trial_number_study != "17" &
                                      cleandata1$trial_number_study != "18" &
                                      cleandata1$trial_number_study != "19" &
                                      cleandata1$trial_number_study != "20" &
                                      cleandata1$trial_number_study != "21" &
                                      cleandata1$trial_number_study != "22" &
                                      cleandata1$trial_number_study != "23" &
                                      cleandata1$trial_number_study != "24" &
                                      cleandata1$trial_number_study != "25" &
                                      cleandata1$trial_number_study != "26" &
                                      cleandata1$trial_number_study != "27" &
                                      cleandata1$trial_number_study != "28" &
                                      cleandata1$trial_number_study != "29" &
                                      cleandata1$trial_number_study != "30" 
                                      , ] 
unique (cleanlightdata1$sex)

# Renaming block numbers -----
class(cleanlightdata1$block_num)
cleanlightdata1$block_num <- as.factor(as.numeric(cleanlightdata1$block_num))
cleanlightdata1$block_num <- recode_factor(cleanlightdata1$block_num, "4" = "1", "7" = "2", "10" = "3", "12" = "4")

# Creating new variables --------
cleanlightdata1$manikintop = (cleanlightdata1$trialcode!="squareApproach_ManikinBottom" & 
                                cleanlightdata1$trialcode!="circleAvoid_ManikinBottom" &
                                  cleanlightdata1$trialcode!="ApAvoid_ManikinBottom" &
                                  cleanlightdata1$trialcode!="SedenApproach_ManikinBottom" &
                                  cleanlightdata1$trialcode!="ApApproach_ManikinBottom" &
                                  cleanlightdata1$trialcode!="SedenAvoid_ManikinBottom" &
                                  cleanlightdata1$trialcode!="squareAvoid_ManikinBottom" &
                                  cleanlightdata1$trialcode!="circleApproach_ManikinBottom")+0
select(cleanlightdata1, manikintop)


cleanlightdata1$approach = (cleanlightdata1$trialcode!="circleAvoid_ManikinBottom" & 
                                      cleanlightdata1$trialcode!="circleAvoid_ManikinTop" &
                                      cleanlightdata1$trialcode!="ApAvoid_ManikinBottom" &
                                      cleanlightdata1$trialcode!="ApAvoid_ManikinTop" &
                                      cleanlightdata1$trialcode!="SedenAvoid_ManikinBottom" &
                                      cleanlightdata1$trialcode!="SedenAvoid_ManikinTop" &
                                      cleanlightdata1$trialcode!="squareAvoid_ManikinTop" &
                                      cleanlightdata1$trialcode!="squareAvoid_ManikinBottom")+0
select(cleanlightdata1, trialcode, approach) 

cleanlightdata1$geomfigure = (cleanlightdata1$trialcode!="SedenApproach_ManikinTop" & 
                              cleanlightdata1$trialcode!="ApAvoid_ManikinBottom" &
                              cleanlightdata1$trialcode!="ApAvoid_ManikinTop" &
                              cleanlightdata1$trialcode!="SedenApproach_ManikinBottom" &
                              cleanlightdata1$trialcode!="ApApproach_ManikinBottom" &
                              cleanlightdata1$trialcode!="SedenAvoid_ManikinBottom" &
                              cleanlightdata1$trialcode!="ApApproach_ManikinTop" &
                              cleanlightdata1$trialcode!="SedenAvoid_ManikinTop")+0
select(cleanlightdata1, trialcode, geomfigure)

cleanlightdata1 <- cleanlightdata1 %>% mutate(stimulus = recode(trialcode, 
                                                "circleAvoid_ManikinBottom" = "circle",
                                                "squareApproach_ManikinBottom" = "square",
                                                "squareApproach_ManikinTop" = "square",
                                                "circleAvoid_ManikinTop" = "circle",
                                                "ApAvoid_ManikinBottom" = "ap",
                                                "SedenApproach_ManikinBottom" = "sed",
                                                "ApAvoid_ManikinTop" = "ap",
                                                "SedenApproach_ManikinTop" = "sed",
                                                "ApApproach_ManikinTop" = "ap",
                                                "ApApproach_ManikinBottom" = "ap",
                                                "SedenAvoid_ManikinBottom" = "sed",
                                                "SedenAvoid_ManikinTop" = "sed",
                                                "squareAvoid_ManikinTop" = "square",
                                                "squareAvoid_ManikinBottom" = "square",
                                                "circleApproach_ManikinBottom" = "circle",
                                                "circleApproach_ManikinTop" = "circle"
                                                ))  
#select(cleanlightdata1, trialcode, stimulus)
#names(cleanlightdata1)

# Inverse codage approach (=1, vs avoid=0)
# Reverse coding appraoch /avoid
cleanlightdata1$avoid.1.approach.0<- NA
cleanlightdata1$avoid.1.approach.0[is.element(cleanlightdata1$approach  ,c("1"))] <- "0"
cleanlightdata1$avoid.1.approach.0[is.element(cleanlightdata1$approach  ,c("0"))] <- "1"

# reversing coding of correct trial = 1 and error = 0 to correct trial = 0 and error = 1
cleanlightdata1$errorcleanlightdata1$error <- NA
cleanlightdata1$error[is.element(cleanlightdata1$correct  ,c("1"))] <- "0"
cleanlightdata1$error[is.element(cleanlightdata1$correct  ,c("0"))] <- "1"
cleanlightdata1$error <- as.numeric(as.character(cleanlightdata1$error))

# Creating binary variable for sex
unique(cleanlightdata1$sex)
cleanlightdata1$sex01<- NA
cleanlightdata1$sex01[is.element(cleanlightdata1$sex ,c("Male"))] <- "1"
cleanlightdata1$sex01[is.element(cleanlightdata1$sex ,c("Mâle"))] <- "1"
cleanlightdata1$sex01[is.element(cleanlightdata1$sex ,c("Female"))] <- "0"
cleanlightdata1$sex01[is.element(cleanlightdata1$sex ,c("Femelle"))] <- "0"
unique(cleanlightdata1$sex01)

# Creating binary variable for gender
unique(cleanlightdata1$gender)
cleanlightdata1$gender01<- NA
cleanlightdata1$gender01[is.element(cleanlightdata1$gender ,c("Homme"))] <- "1"
cleanlightdata1$gender01[is.element(cleanlightdata1$gender ,c("Man"))] <- "1"
cleanlightdata1$gender01[is.element(cleanlightdata1$gender ,c("Femme"))] <- "0"
cleanlightdata1$gender01[is.element(cleanlightdata1$gender ,c("Woman"))] <- "0"
unique(cleanlightdata1$gender01)
plot (cleanlightdata1$sex01 ~ cleanlightdata1$gender01)

# Reversing code of Kinesiophobia 
cleanlightdata1$kp3r <- NA
cleanlightdata1$kp3r[is.element(cleanlightdata1$kp3 ,c("1"))] <- "7"
cleanlightdata1$kp3r[is.element(cleanlightdata1$kp3 ,c("2"))] <- "6"
cleanlightdata1$kp3r[is.element(cleanlightdata1$kp3 ,c("3"))] <- "5"
cleanlightdata1$kp3r[is.element(cleanlightdata1$kp3 ,c("4"))] <- "4"
cleanlightdata1$kp3r[is.element(cleanlightdata1$kp3 ,c("5"))] <- "3"
cleanlightdata1$kp3r[is.element(cleanlightdata1$kp3 ,c("6"))] <- "2"
cleanlightdata1$kp3r[is.element(cleanlightdata1$kp3 ,c("7"))] <- "1"

cleanlightdata1$kp4r <- NA
cleanlightdata1$kp4r[is.element(cleanlightdata1$kp4 ,c("1"))] <- "7"
cleanlightdata1$kp4r[is.element(cleanlightdata1$kp4 ,c("2"))] <- "6"
cleanlightdata1$kp4r[is.element(cleanlightdata1$kp4 ,c("3"))] <- "5"
cleanlightdata1$kp4r[is.element(cleanlightdata1$kp4 ,c("4"))] <- "4"
cleanlightdata1$kp4r[is.element(cleanlightdata1$kp4 ,c("5"))] <- "3"
cleanlightdata1$kp4r[is.element(cleanlightdata1$kp4 ,c("6"))] <- "2"
cleanlightdata1$kp4r[is.element(cleanlightdata1$kp4 ,c("7"))] <- "1"

cleanlightdata1$kp7r <- NA
cleanlightdata1$kp7r[is.element(cleanlightdata1$kp7 ,c("1"))] <- "7"
cleanlightdata1$kp7r[is.element(cleanlightdata1$kp7 ,c("2"))] <- "6"
cleanlightdata1$kp7r[is.element(cleanlightdata1$kp7 ,c("3"))] <- "5"
cleanlightdata1$kp7r[is.element(cleanlightdata1$kp7 ,c("4"))] <- "4"
cleanlightdata1$kp7r[is.element(cleanlightdata1$kp7 ,c("5"))] <- "3"
cleanlightdata1$kp7r[is.element(cleanlightdata1$kp7 ,c("6"))] <- "2"
cleanlightdata1$kp7r[is.element(cleanlightdata1$kp7 ,c("7"))] <- "1"

cleanlightdata1$kp11r <- NA
cleanlightdata1$kp11r[is.element(cleanlightdata1$kp11 ,c("1"))] <- "7"
cleanlightdata1$kp11r[is.element(cleanlightdata1$kp11 ,c("2"))] <- "6"
cleanlightdata1$kp11r[is.element(cleanlightdata1$kp11 ,c("3"))] <- "5"
cleanlightdata1$kp11r[is.element(cleanlightdata1$kp11 ,c("4"))] <- "4"
cleanlightdata1$kp11r[is.element(cleanlightdata1$kp11 ,c("5"))] <- "3"
cleanlightdata1$kp11r[is.element(cleanlightdata1$kp11 ,c("6"))] <- "2"
cleanlightdata1$kp11r[is.element(cleanlightdata1$kp11 ,c("7"))] <- "1"


# Creating binary variable for mobile phone vs. PC or laptop
unique(cleanlightdata1$computer_platform)
cleanlightdata1$computer01<- NA
cleanlightdata1$computer01[is.element(cleanlightdata1$computer_platform ,c("and"))] <- "0"
cleanlightdata1$computer01[is.element(cleanlightdata1$computer_platform ,c("ios"))] <- "0"
cleanlightdata1$computer01[is.element(cleanlightdata1$computer_platform ,c("mac"))] <- "1"
cleanlightdata1$computer01[is.element(cleanlightdata1$computer_platform ,c("win"))] <- "1"
unique(cleanlightdata1$computer01)

# creating a columns of the different pictograms appearing on the screen for each trial
cleanlightdata1 <- cleanlightdata1 %>%
  mutate(pictograms = case_when(stimulus=="square" ~ square_pic, 
                                stimulus=="circle" ~ circle_pic,
                                stimulus=="ap" ~ ap_pic, 
                                TRUE ~ sed_pic))  
#select(cleanlightdata1, pictograms)
#class(cleanlightdata1$pictograms)
#write.csv(cleanlightdata1, "original_data.csv", row.names = FALSE)

### Per-subject means (reaction time) / counts (errors)
cleanlightdata1_mean_by_cond <-
  cleanlightdata1 %>%
  mutate(trialcode_no_top_bot = gsub("(.*)(Bottom|Top)", "\\1", trialcode)) %>%
  group_by(id, trialcode_no_top_bot) %>%
  summarise(mean = mean(latency, na.rm = TRUE),
            num_error = sum(error, na.rm = TRUE),
            error_ratio = mean(error, na.rm = TRUE),
            group_size = n(), .groups = "keep") %>%
  ungroup() %>%
  pivot_wider(id_cols = id, names_from = trialcode_no_top_bot,
              values_from = c(mean, num_error, group_size, error_ratio)) %>%
  group_by(id) %>%
  mutate(mean_geomAvoid = weighted.mean(c(mean_circleAvoid_Manikin, mean_squareAvoid_Manikin),
                                        w = c(group_size_circleAvoid_Manikin, group_size_squareAvoid_Manikin),
                                        na.rm = TRUE),
         mean_geomApproach = weighted.mean(c(mean_circleApproach_Manikin, mean_squareApproach_Manikin),
                                           w = c(group_size_circleApproach_Manikin, group_size_squareApproach_Manikin),
                                           na.rm = TRUE),
         sum_error_geomAvoid = sum(num_error_circleAvoid_Manikin,
                                   num_error_squareAvoid_Manikin, na.rm = TRUE),
         sum_error_geomApproach = sum(num_error_circleApproach_Manikin,
                                      num_error_squareApproach_Manikin, na.rm = TRUE),
         error_ratio_geomAvoid = sum_error_geomAvoid /
           sum(group_size_circleAvoid_Manikin,
               group_size_squareAvoid_Manikin, na.rm = TRUE),
         error_ratio_geomApproach = sum_error_geomApproach /
           sum(group_size_circleApproach_Manikin,
               group_size_squareApproach_Manikin, na.rm = TRUE)) %>%
  ungroup() %>%
  select(!starts_with("group_size")) %>%
  mutate(diff_ApAvoid = mean_ApAvoid_Manikin - mean_geomAvoid,
         diff_SedenAvoid = mean_SedenAvoid_Manikin - mean_geomAvoid,
         diff_ApApproach = mean_ApApproach_Manikin - mean_geomApproach,
         diff_SedenApproach = mean_SedenApproach_Manikin - mean_geomApproach)

cleanlightdata1 <- left_join(cleanlightdata1, cleanlightdata1_mean_by_cond, by = "id")
#limpse(cleanlightdata1)

cleanlightdata1_error_approach_avoid <-
  cleanlightdata1_mean_by_cond %>%
  select(id, sum_error_geomAvoid, sum_error_geomApproach,
         error_ratio_geomAvoid, error_ratio_geomApproach) %>%
  pivot_longer(!id,
               names_to = c(".value", "approach_or_avoid"),
               names_pattern = "(.*)(Avoid|Approach)$") %>%
  mutate(approach_or_avoid = tolower(approach_or_avoid))

cleanlightdata1 <- cleanlightdata1 %>%
  mutate(approach_or_avoid =
           case_when(
             str_starts(block_code, "approach_") ~ "approach",
             str_starts(block_code, "avoid_") ~ "avoid",
             TRUE ~ NA_character_
           ))

cleanlightdata1 <- left_join(cleanlightdata1, cleanlightdata1_error_approach_avoid,
                             by = c("id", "approach_or_avoid"))

## Adding columns with means per subject -----
# per level of the factors stimulus + approach -----
# and per level of geomfigure (1,0) -----
cleanlightmeansdata1 <- cleanlightdata1 %>%
  group_by(id, stimulus, approach) %>%
  mutate(mean_latency = mean(latency, na.rm=T)) %>% 
  mutate(mean_error = mean(error, na.rm=T)) %>% 
  ungroup() %>%
  
  group_by(id, geomfigure) %>%
  mutate(mean_latency_geom = mean(latency, na.rm=T)) %>%
  mutate(mean_error_geom = mean(error, na.rm=T)) %>%
  ungroup() %>%
  
  group_by(id, approach, geomfigure) %>%
  mutate(mean_geom_direction = mean(latency, na.rm=T)) %>% 
  mutate(mean_error_geom_direction = mean(error, na.rm=T)) %>% 
  ungroup() %>% 
  
  group_by(id, geomfigure) %>%
  mutate(mean_only_geom = mean(latency, na.rm=T)) %>% 
  mutate(mean_error_only_geom = mean(error, na.rm=T)) %>% 
  ungroup() 

#names(cleanlightmeansdata1)

# adding column with trial number of the whole study
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  group_by(id) %>%
  mutate(trial_number_study = 1:n()) %>%
  ungroup()
#unique(cleanlightmeansdata1$trial_number_study)

# Adding column with mean for geomfigure for each subject
# Use fill from tidyr after changing the 'mean_only_geom' values to NA that corresponds to 0 in 'geomfigure'
# By default, the .direction (argument in fill) is "down", but it can also take "up" (mean_only_geom, .direction="up")
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(mean_only_geom = NA^(!geomfigure)*mean_only_geom) %>%
  fill(mean_only_geom)

cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(mean_error_only_geom = NA^(!geomfigure)*mean_error_only_geom) %>%
  fill(mean_error_only_geom)

# Adding column with mean for geomfigure and direction for each subject
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(mean_geom_direction = NA^(!geomfigure)*mean_geom_direction) %>%
  group_by(id, approach) %>%
  mutate(mean_geom_direction = mean(mean_geom_direction, na.rm=T)) 

cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(mean_error_geom_direction = NA^(!geomfigure)*mean_error_geom_direction) %>%
  group_by(id, approach) %>%
  mutate(mean_error_geom_direction = mean(mean_error_geom_direction, na.rm=T)) 

# # Change variable class
class(cleanlightmeansdata1$c8)
cleanlightmeansdata1$manikintop <- as.factor(as.numeric(cleanlightmeansdata1$manikintop))
cleanlightmeansdata1$approach <- as.factor(as.numeric(cleanlightmeansdata1$approach))
cleanlightmeansdata1$stimulus <- as.factor(as.character(cleanlightmeansdata1$stimulus))
cleanlightmeansdata1$trialcode <- as.factor(as.character(cleanlightmeansdata1$trialcode))
cleanlightmeansdata1$geomfigure <- as.factor(as.numeric(cleanlightmeansdata1$geomfigure))
cleanlightmeansdata1$id <- as.factor(as.character(cleanlightmeansdata1$id))
cleanlightmeansdata1$c8 <- as.factor(as.integer(cleanlightmeansdata1$c8))
cleanlightmeansdata1$c16 <- as.factor(as.integer(cleanlightmeansdata1$c16))
cleanlightmeansdata1$age <- as.numeric(as.integer(cleanlightmeansdata1$age))
cleanlightmeansdata1$sex01 <- as.factor(as.character(cleanlightmeansdata1$sex01))
cleanlightmeansdata1$gender01 <- as.factor(as.character(cleanlightmeansdata1$gender01))
cleanlightmeansdata1$pictograms <- as.character(as.integer(cleanlightmeansdata1$pictograms))
cleanlightmeansdata1$sum_chronic <- as.numeric(as.integer(cleanlightmeansdata1$sum_chronic))
cleanlightmeansdata1$computerO1 <- as.factor(as.character(cleanlightmeansdata1$computer01))
#unique(cleanlightmeansdata1$attitudeO1)
#unique(cleanlightmeansdata1$c8)
#unique(cleanlightmeansdata1$c16)
#unique(cleanlightmeansdata1$stimulus)
#unique(cleanlightmeansdata1$pictograms)
cleanlightmeansdata1$kp1 <- as.numeric(as.integer(cleanlightmeansdata1$kp1))
cleanlightmeansdata1$kp2 <- as.numeric(as.integer(cleanlightmeansdata1$kp2))
cleanlightmeansdata1$kp3r <- as.numeric(as.character(cleanlightmeansdata1$kp3r))
cleanlightmeansdata1$kp4r <- as.numeric(as.character(cleanlightmeansdata1$kp4r))
cleanlightmeansdata1$kp5 <- as.numeric(as.integer(cleanlightmeansdata1$kp5))
cleanlightmeansdata1$kp6 <- as.numeric(as.integer(cleanlightmeansdata1$kp6))
cleanlightmeansdata1$kp7r <- as.numeric(as.character(cleanlightmeansdata1$kp7r))
cleanlightmeansdata1$kp8 <- as.numeric(as.integer(cleanlightmeansdata1$kp8))
cleanlightmeansdata1$kp9 <- as.numeric(as.integer(cleanlightmeansdata1$kp9))
cleanlightmeansdata1$kp10 <- as.numeric(as.integer(cleanlightmeansdata1$kp10))
cleanlightmeansdata1$kp11r <- as.numeric(as.character(cleanlightmeansdata1$kp11r))

# Adding column latency minus the mean latency for geometrical figures 
# irrespective of the type of figure (circle, square) and the type of movement (approach, avoid)
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(relativelatencygeom = latency - mean_only_geom)

# irrespective of the type of figure (circle, square)
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(relativelatencygeomdirection = latency - mean_geom_direction)

# creating column with means of relative latency
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  group_by(id, geomfigure, approach) %>%
  mutate(relativelatency = mean(relativelatencygeomdirection, na.rm=T)) %>% 
  ungroup() 

# creating column with number of minutes per week in moderate and vigorous physical activity (mvpa)
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(mvpa = (moderate_d*moderate_m + vigorous_d*vigorous_m)) 
hist(cleanlightmeansdata1$mvpa)

# Identify participants who report physical activity > 12h per day (5040 min per week)
participants_high_mvpa <- subset(cleanlightmeansdata1, mvpa > 5040)
print(participants_high_mvpa$id)

# creating column with number of minutes per week in moderate  physical activity 
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(moderatepa = (moderate_d*moderate_m))
hist(cleanlightmeansdata1$moderatepa)

# creating column with number of minutes per week in vigorous physical activity 
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(vigorouspa = (vigorous_d*vigorous_m))
hist(cleanlightmeansdata1$vigorouspa)

# creating column with raw ap bias
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(biasapraw = (mean_ApAvoid_Manikin-mean_ApApproach_Manikin))
hist(cleanlightmeansdata1$biasapraw)

# creating column with raw sed bias
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(biassedraw = (mean_SedenAvoid_Manikin-mean_SedenApproach_Manikin ))
hist(cleanlightmeansdata1$biassedraw)

# creating column with corrected ap bias 
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(biasapcorr = (diff_ApAvoid - diff_ApApproach))
hist(cleanlightmeansdata1$biasapcorr)

# creating column with corrected sed bias
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(biassedcorr = (diff_SedenAvoid - diff_SedenApproach))
hist(cleanlightmeansdata1$biassedcorr)

# creating column with mean kinesiophobia
cleanlightmeansdata1 <- cleanlightmeansdata1 %>%
  mutate(kpsum = rowSums(cleanlightmeansdata1[,c('kp1', 'kp2', 'kp3r', 'kp4r', 'kp5', 'kp6','kp7r', 'kp8', 'kp9','kp10', 'kp11r')], na.rm=TRUE))
hist(cleanlightmeansdata1$kpsum)
class(cleanlightmeansdata1$kpsum)


# Creating new file -----
data2 <- cleanlightmeansdata1
#write.csv(data2, "data2.csv")
plot(data2$kpsum)
describe(data2$age)
plot(data2$height)
unique(data2$id) # n =197

# exploring data -------
glimpse(data2)
# skim(data2)
hist(data2$age)
hist(data2$age, breaks=50)
describe (data2$relativelatency)
unique(data2$stimulus)


# Reorder stimulus
unique(data2$stimulus)
data2$stimulus_ap0_sed1<- NA
data2$stimulus_ap0_sed1[is.element(data2$stimulus ,c("ap"))] <- "0"
data2$stimulus_ap0_sed1[is.element(data2$stimulus  ,c("sed"))] <- "1"
data2$stimulus_ap0_sed1[is.element(data2$stimulus ,c("circle"))] <- "2"
data2$stimulus_ap0_sed1[is.element(data2$stimulus  ,c("square"))] <- "3"
unique(data2$stimulus_ap0_sed1)

# Reode stimulus
unique(data2$stimulus)
data2$stimulus_ap1_sed0<- NA
data2$stimulus_ap1_sed0[is.element(data2$stimulus ,c("ap"))] <- "1"
data2$stimulus_ap1_sed0[is.element(data2$stimulus  ,c("sed"))] <- "0"
data2$stimulus_ap1_sed0[is.element(data2$stimulus ,c("circle"))] <- "2"
data2$stimulus_ap1_sed0[is.element(data2$stimulus  ,c("square"))] <- "3"
unique(data2$stimulus_ap1_sed0)



# centration  -----
data2$age_c <- scale (data2$age, center = TRUE, scale = TRUE)
data2$mvpa_c <- scale (data2$mvpa, center = TRUE, scale = TRUE)
data2$mvpa_c <- scale (data2$mvpa, center = TRUE, scale = TRUE)
data2$bmi_c <- scale (data2$bmi, center = TRUE, scale = TRUE)
data2$diff_ApAvoid_c <- scale (data2$diff_ApAvoid, center = TRUE, scale = TRUE)                                         
data2$diff_SedenAvoid_c <- scale (data2$diff_SedenAvoid, center = TRUE, scale = TRUE)
data2$diff_ApApproach_c <- scale (data2$diff_ApApproach, center = TRUE, scale = TRUE)
data2$diff_SedenApproach_c <- scale (data2$diff_SedenApproach, center = TRUE, scale = TRUE)
data2$kpsum_c <- scale (data2$kpsum, center = TRUE, scale = TRUE)
data2$pain_c <- scale (data2$pain, center = TRUE, scale = TRUE)
data2$biasapraw_c <- scale (data2$biasapraw, center = TRUE, scale = TRUE)
data2$biassedraw_c <- scale (data2$biassedraw, center = TRUE, scale = TRUE)
data2$attitude_c <- scale (data2$attitude, center = TRUE, scale = TRUE)

# aggrefate data per subject
DataAggreg = aggregate (cbind(c8, c16, kpsum, attitude, pain, age, sex01, mvpa, gender01,
                              bmi, sum_chronic, moderatepa, vigorouspa, biassedcorr, biasapcorr)
                        ~id, data=data2, FUN=mean)

# centration
DataAggreg$age_c <- scale (DataAggreg$age, center = TRUE, scale = TRUE)
DataAggreg$bmi_c <- scale (DataAggreg$bmi, center = TRUE, scale = TRUE)
DataAggreg$mvpa_c <- scale (DataAggreg$mvpa, center = TRUE, scale = TRUE)
DataAggreg$pain_c <- scale (DataAggreg$pain, center = TRUE, scale = TRUE)
DataAggreg$kpsum_c <- scale (DataAggreg$kpsum, center = TRUE, scale = TRUE)
DataAggreg$attitude_c <- scale (DataAggreg$attitude, center = TRUE, scale = TRUE)
DataAggreg$biassedcorr_c <- scale (DataAggreg$biassedcorr, center = TRUE, scale = TRUE)
DataAggreg$biasapcorr_c <- scale (DataAggreg$biasapcorr, center = TRUE, scale = TRUE)

# Reverse Rhumathoid Arthritis no = 1 yest = 2 to no = 1 yes = 0
unique(DataAggreg$c16)
DataAggreg$c16.1.0<- NA
DataAggreg$c16.1.0[is.element(DataAggreg$c16 ,c("1"))] <- "1"
DataAggreg$c16.1.0[is.element(DataAggreg$c16  ,c("2"))] <- "0"
unique(DataAggreg$c16.1.0)

# Reverse Osteo Arthritis no = 1 yes = 2 to no = 1 yes = 0
unique(DataAggreg$c8)
DataAggreg$c8.1.0<- NA
DataAggreg$c8.1.0[is.element(DataAggreg$c8 ,c("1"))] <- "1"
DataAggreg$c8.1.0[is.element(DataAggreg$c8  ,c("2"))] <- "0"
unique(DataAggreg$c8.1.0)

# Change variable class
DataAggreg$sex01 <- as.factor(as.numeric(DataAggreg$sex01))
DataAggreg$c8 <- as.factor(as.numeric(DataAggreg$c8))
DataAggreg$c16 <- as.factor(as.numeric(DataAggreg$c16))
DataAggreg$c16.1.0 <- as.factor(as.numeric(DataAggreg$c16.1.0))

# histograms
hist (DataAggreg$kpsum)
hist (DataAggreg$mvpa)
hist (DataAggreg$kpsum)
hist (DataAggreg$age)
hist (DataAggreg$age, breaks = c(19.9,29.9,39.9,49.9,59.9,69.9,79.9,89.9), ylim=c(0, 60), xlab = "Age (years)", ylab = "Number of particiants")
hist (DataAggreg$bmi)


#Number of ...
sum(with(DataAggreg,sex01 == "1"))
sum(with(DataAggreg,sex01 == "2"))
sum(with(DataAggreg,age < "30"))
sum(with(DataAggreg,age < "40"))
sum(with(DataAggreg,age < "50"))
sum(with(DataAggreg,age < "60"))
sum(with(DataAggreg,age < "70"))
sum(with(DataAggreg,age > "80" & (c16 == "2" | c8 == "2")))
sum(with(DataAggreg,age < "90"))
sum(with(DataAggreg,c8 == "0"))
sum(with(DataAggreg,c8 == "1"))
sum(with(DataAggreg,c8 == "2"))
sum(with(DataAggreg,c16 == "0"))
sum(with(DataAggreg,c16 == "1"))
sum(with(DataAggreg,c16 == "2"))
sum(with(DataAggreg,c16 == "2" | c8 == "2"))
sum(with(DataAggreg,c16 == "1" & c8 == "2"))

# unique
unique(DataAggreg$c8)
unique(DataAggreg$c16)
unique(DataAggreg$id) # n = 196
#write.csv(DataAggreg, "DataAggreg.csv")


# DESCRIPTIVE RESULTS
data_all <- DataAggreg # all participants
unique(data_all$id) 
describe (data_all$age)
describe (data_all$kpsum)
describe (data_all$mvpa)
describe (data_all$pain)
describe (data_all$bmi)
describe (data_all$sum_chronic)
sum(with(data_all,sex01 == "1"))
sum(with(data_all,sex01 == "2"))
unique (data_all$gender)
plot (data_all$gender01~data_all$sex01)
data_sex_gender <- data_all %>% filter((sex01 %in% "2") & (gender01 %in% "1"))
unique(data_sex_gender$id) 

data_OA <- DataAggreg %>% filter((c8 %in% "2") & (c16 %in% "1")) # participants with Osteoarthritis
unique(data_OA$id) 
describe (data_OA$age)
describe (data_OA$mvpa)
describe (data_OA$kpsum)
describe (data_OA$pain)
describe (data_OA$bmi)
describe (data_OA$sum_chronic)
sum(with(data_OA,sex01 == "1"))
sum(with(data_OA,sex01 == "2"))

data_RA <- DataAggreg %>% filter(c16 %in% "2") # participants with Rheumatoid Arthritis
unique(data_RA$id) 
describe (data_RA$age)
describe (data_RA$mvpa)
describe (data_RA$kpsum)
describe (data_RA$pain)
describe (data_RA$bmi)
describe (data_RA$sum_chronic)
sum(with(data_RA,sex01 == "1"))
sum(with(data_RA,sex01 == "2"))


### STATISTICAL MODELS

#Effect of kinesiophobia on MVPA in OA (osteo arthritis)
lm1.1 <- lm (mvpa ~ kpsum_c*c8.1.0 + pain_c  + age_c + sex01 + bmi_c + sum_chronic, data=DataAggreg, na.action=na.omit) # no Rheumatoid Arthritis
summary(lm1.1)
confint(lm1.1) #95% confidence interval

#Effect of kinesiophobia on MVPA in OA (osteo arthritis)
lm1.2 <- lm (mvpa ~ kpsum_c*c8.1.0 + pain_c  + age_c + sex01 + bmi_c + sum_chronic, data=DataAggreg, subset = (c16 == 1), na.action=na.omit) # no Rheumatoid Arthritis
summary(lm1.2)
confint(lm1.2) #95% confidence interval
#plot(allEffects(lm1.1))
plot(allEffects(lm1.1), select = 6)

# Effect of kinesiophobia on MVPA in RA (rheumatoid arthritis)
lm1.3 <- lm (mvpa ~ kpsum_c*c16.1.0 + pain_c  + age_c + sex01 + bmi_c + sum_chronic, data=DataAggreg, na.action=na.omit) 
summary(lm1.3)
confint(lm1.3)


# Mediation analysis by attitude in Osteo Arthritis 
mediation.1 <- lm (mvpa ~ kpsum_c + pain_c  + biasapcorr_c + biassedcorr_c + age_c + sex01 + bmi_c + sum_chronic, data=DataAggreg, subset = (c8 == 2) & (c16 == 1), na.action=na.omit) 
summary(mediation.1)
confint(mediation.1)
plot(allEffects(mediation.1 ), select = 1)

mediation.2 <- lm (attitude ~ kpsum_c + pain_c + biasapcorr_c + biassedcorr_c + age_c + sex01 + bmi_c + sum_chronic, data=DataAggreg, subset = (c8 == 2) & (c16 == 1), na.action=na.omit) 
summary(mediation.2)
confint(mediation.2)
plot(allEffects(mediation.2 ), select = 1)

mediation.3 <- lm (mvpa ~ attitude_c + pain_c  + biasapcorr_c + biassedcorr_c  + age_c + sex01 + bmi_c + sum_chronic, data=DataAggreg, subset = (c8 == 2) & (c16 == 1), na.action=na.omit) 
summary(mediation.3)
confint(mediation.3)
plot(allEffects(mediation.3 ), select = 1)


data3  <- DataAggreg %>% filter((c8 %in% "2") & (c16 %in% "1"))
unique(data3$id) 
print(data3)
sum(with(data3,c8 == "2"))


model <- ' # direct effect
             mvpa_c ~ p*kpsum_c + c1*pain_c + c2*biasapcorr_c + c3*biassedcorr_c+ c4*age_c + c5*sex01 + c6*bmi_c + c7*sum_chronic
           # mediator
             attitude ~ a*kpsum_c + c8*pain_c + c9*biasapcorr_c + c10*biassedcorr_c+ c11*age_c + c12*sex01 + c13*bmi_c + c14*sum_chronic
             mvpa_c ~ b*attitude 
           # indirect effect (a*b)
             ab := a*b 
           # total effect
             total := p + (a*b)
         '
fit <- sem(model, data = data_OA)
summary(fit)
parameterEstimates(fit) #to extract 95 CI


# Linear mixed effects model testing the effect of kinesiophobia on corrected Reaction Time as a function of stimulus (physical activity, sedentary behavior)
# and action (approach, avoid)

lmm1.1 <- lmer(relativelatencygeomdirection  ~ 1 + approach*stimulus_ap0_sed1*kpsum_c + attitude_c + pain_c + mvpa_c + age_c + sex01 + bmi_c + computer01 + sum_chronic + (1|id) + (1|pictograms), 
               data=data2, subset = (stimulus == "sed"| stimulus == "ap") & (c16 == 0) & (c8 == 1) & error == 0 , REML=T, na.action=na.omit)
# We tried the model with a more complex random structure including  (1|stimulus_ap0_sed1) but it failed to converge
summary(lmm1.1) # kpsum NS
confint(lmm1.1)


plot(allEffects(lmm1.1))
plot(allEffects(lmm1.1), select = 9)
plot(Effect(c("approach", "stimulus_ap0_sed1"),mod = lmm1.1))

lmm1.2 <- lmer(relativelatencygeomdirection  ~ 1 + avoid.1.approach.0*kpsum_c*stimulus_ap0_sed1 + attitude_c + pain_c + mvpa_c + age_c + sex01 + bmi_c + computer01 +sum_chronic +  (1|id) + (1|pictograms), 
             data=data2, subset = (stimulus == "sed"| stimulus == "ap") & error == 0 & c8 == 1 & c16 != 1, REML=T, na.action=na.omit)
summary(lmm1.2) 
confint(lmm1.2)
plot(allEffects(lmm1.2))
plot(allEffects(lmm1.2), select = 9)

lmm1.3 <- lmer(relativelatencygeomdirection  ~ 1 + approach*kpsum_c*stimulus_ap1_sed0 + attitude_c + pain_c + mvpa_c + age_c + sex01 + bmi_c + computer01 + (1|id) +sum_chronic +  (1|pictograms), 
               data=data2, subset = (stimulus == "sed"| stimulus == "ap")  & error == 0 & c8 == 1 & c16 != 1, REML=T, na.action=na.omit)
summary(lmm1.3)
confint(lmm1.3)

lmm1.4 <- lmer(relativelatencygeomdirection  ~ 1 + avoid.1.approach.0*kpsum_c*stimulus_ap1_sed0 + attitude_c + pain_c + mvpa_c + age_c + sex01 + bmi_c + computer01 + sum_chronic + (1|id) + (1|pictograms), 
               data=data2, subset = (stimulus == "sed"| stimulus == "ap")  & error == 0 & c8 == 1 & c16 != 1, REML=T, na.action=na.omit)
summary(lmm1.4) # kpsum NS
confint(lmm1.4)


glm1<- glmer(error ~ approach*kpsum_c*stimulus + (1|id), family="binomial", 
              data=data2, subset = (stimulus == "sed"| stimulus == "ap") & (c8 == "1" & c16 != "1"), na.action=na.omit)
summary(glm1)



## Assumptions for linear mixed-effects models A
## https://ademos.people.uic.edu/Chapter18.html#611_how_do_you_test_this_assumption

#### ASSUMPTIONS LMM
# residuals
plot(lmm1.1, type = c("p", "smooth"))

# scale-location
plot(lmm1.1, sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"))

# qq plot
qqmath(lmm1.1, id = NULL)

#### Multicollinearity 
vif.mer <- function (fit) {
  ## adapted from rms::vif (variance inflation factor)
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
vif.mer(lmm1.1)
