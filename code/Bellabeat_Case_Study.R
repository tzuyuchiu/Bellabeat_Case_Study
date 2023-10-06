# Loading packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

# Importing datasets
## For the case study, I use one month Fitbit dataset between 12th April to 12th May 2016
daily_activity <- read_csv("/Users/tzuyuchiu/Desktop/Google Data Analystic/case_study/Bellabeat/project/Bellabeat_Case_Study/fitabase_data/dailyActivity_merged.csv")
daily_sleep <- read_csv("/Users/tzuyuchiu/Desktop/Google Data Analystic/case_study/Bellabeat/project/Bellabeat_Case_Study/fitabase_data/sleepDay_merged.csv")
weight_log <-read_csv("/Users/tzuyuchiu/Desktop/Google Data Analystic/case_study/Bellabeat/project/Bellabeat_Case_Study/fitabase_data/weightLogInfo_merged.csv")

# Explore the datasets
head(daily_activity)
head(daily_sleep)
head(weight_log)

# Convert datatype from string to date 
# activity
daily_activity$ActivityDate <- mdy(daily_activity$ActivityDate)
head(daily_activity)

# sleep
daily_sleep_split<- separate(daily_sleep, SleepDay, into=c('ActivityDate','time', 'am_pm'), sep=" ", remove=TRUE)
daily_sleep_split$ActivityDate <- mdy(daily_sleep_split$ActivityDate)
head(daily_sleep_split)

# weight_log
weight_log_split<- separate(weight_log, Date, into=c('ActivityDate','time', 'am_pm'), sep=" ", remove=TRUE)
weight_log_split$ActivityDate <- mdy(weight_log_split$ActivityDate)
head(weight_log_split)

# Check how many userid in each datasets
n_distinct(daily_activity$Id)
n_distinct(daily_sleep_split$Id)
n_distinct(weight_log_split$Id)

# Explore and summarize data
daily_activity %>% 
  dplyr::select(ActivityDate,TotalSteps,TotalDistance,Calories,VeryActiveMinutes,FairlyActiveMinutes,LightlyActiveMinutes,SedentaryMinutes) %>%
  summary()

daily_sleep_split %>% 
  dplyr::select(ActivityDate,TotalMinutesAsleep,TotalTimeInBed) %>% 
  summary()

weight_log_split %>%
  dplyr::select(ActivityDate,WeightKg,BMI) %>% 
  summary()

# Merge Daily activity and sleep dataset by ActivityDate and Id

jointdataset <- daily_activity %>% full_join(daily_sleep_split, by=c('Id'='Id', 'ActivityDate'='ActivityDate'))
n_distinct(jointdataset$Id)

#Count the total NA values in the TotalTimenBed column, grouped by the Id column:
jointdataset %>%
  group_by(Id) %>%
  summarise(total_na = sum(is.na(TotalTimeInBed)))

# To check if all the activities sum up is equal to 24 hours (day)
jointdataset <-jointdataset %>% 
  mutate(TotalTime=(VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+SedentaryMinutes+TotalTimeInBed)/60)
#filter on days that is not 24 hours in total. 
jointdataset %>% 
  filter(TotalTime > 24) %>% 
  glimpse()

jointdataset %>% 
  filter(TotalTime < 24) %>% 
  glimpse()

# It seems that the sedentary minutes column already included the TotolBedTime since after recalculated the TotalTime we have
# zero rows more than 24 hours

jointdataset_check1 <-jointdataset %>%
  mutate(TotalTime1=(VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+SedentaryMinutes)/60)

jointdataset_check1 %>% 
  filter(TotalTime1 > 24) %>% 
  glimpse()

jointdataset_check1 %>% 
  filter(TotalTime1 < 24) %>% 
  glimpse()
# Replace on Totalminsalseep and total time in bed NA to zero 
jointdataset_check1["TotalTimeInBed"][is.na(jointdataset_check1["TotalTimeInBed"])] <- 0
jointdataset_check1["TotalMinutesAsleep"][is.na(jointdataset_check1["TotalMinutesAsleep"])] <- 0
# Analysis
# Does weekday or weekend impact the user activities?
# Added a column to convert date to day
jointdataset_check1$WeekDay <- weekdays(jointdataset_check1$ActivityDate)
jointdataset_check1$WeekDay <- factor(jointdataset_check1$WeekDay, level=c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# summarize average total steps, total distance, and calories by day
weekday_avg<-jointdataset_check1 %>% 
  group_by(WeekDay) %>% 
  summarise(avg_steps = mean(TotalSteps),avg_distance=mean(TotalDistance),avg_calories=mean(Calories)) %>% 
  arrange(WeekDay)
weekday_avg

# Trend between number of steps and distance

ggplot(weekday_avg, aes(x=WeekDay)) +
  geom_line( aes(y=avg_steps), linewidth=2,group=1, color="blue") + 
  geom_line( aes(y=avg_distance/0.001), linewidth=2,group=1, color="purple") +
    scale_y_continuous(
  # Features of the first axis
    name = "Number of steps",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.0005, name="Distance (km)"))+ theme_bw() + 
    labs(title="Fitbit Activity: Number of steps vs. Distance (km)",x="Days")

# Trend between Total steps and calorie burn

ggplot(weekday_avg, aes(x=WeekDay)) +
  geom_line( aes(y=avg_steps), linewidth=1.5,group=1, color="blue") + 
  geom_line( aes(y=avg_calories/0.3), linewidth=1.5,group=1, color="orange") +
  scale_y_continuous(
    # Features of the first axis
    name = "Total Steps",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.3, name="Calories (kcal)")) +
  theme_bw()+ theme(axis.text.x = element_text(angle = 30, size=6.5)) +
  labs(title="Trend between TotalSteps and Calories burned",
       x="Days")

# Add columns of activities percentage of total distance
jointdataset_check1<-jointdataset_check1 %>% 
  mutate(TotalDistance_calc=(VeryActiveDistance+ModeratelyActiveDistance+LightActiveDistance+SedentaryActiveDistance),
         per_veryactive_distance=ifelse(VeryActiveDistance!=0,VeryActiveDistance/TotalDistance_calc,0),
         per_moderateactive_distance=ifelse(ModeratelyActiveDistance!=0,ModeratelyActiveDistance/TotalDistance_calc,0),
         per_lightactive_distance=ifelse(LightActiveDistance!=0,LightActiveDistance/TotalDistance_calc,0),
         per_sedentaryactive_distance=ifelse(SedentaryActiveDistance!=0,SedentaryActiveDistance/TotalDistance_calc,0))

jointdataset_check1 %>% 
  group_by(WeekDay) %>%
  summary() 


# Add a column: category to categorize the activity level into four group - Active, Moderate, Light, and Barely moving 
jointdataset_check1<-jointdataset_check1 %>% 
  mutate(category=case_when(
    per_veryactive_distance>=0.5 ~ "Active",
    per_moderateactive_distance>=0.5 ~ "Moderate",
    per_lightactive_distance>=0.5  ~ "Light",
    per_veryactive_distance+per_moderateactive_distance+per_lightactive_distance<=0.05  ~ "Barely move",
    .default = "Light"))

ggplot(data=jointdataset_check1, aes(x=TotalSteps, y= Calories)) +
  geom_point(aes(color=category)) +
  facet_wrap(~category) + theme_bw()+ labs(title="Total steps vs. Calorie burned by Activity level", x="TotalSteps")


ggplot(jointdataset_check1, aes(x = WeekDay, fill = category)) + 
  geom_bar(position = 'dodge')+ theme_bw()+ labs(title="Number of records by Activity level",x="Days", y="Number of records")

# Summary by category and average calorie burn
category_by_calories <- jointdataset_check1 %>% 
  group_by(category) %>%
  summarise(avg_calories = mean(Calories)) %>%
  ungroup()

ggplot(category_by_calories, aes(x = category, y = avg_calories)) + 
  geom_bar(stat = 'identity', width = 0.5, fill = 'firebrick')+ theme_bw()+ labs(title="Average Calorie Burn vs Activity level",x="Active Level", y="Average Calorie Burn")

# Filter category : Barely moving
barely_move_details <-jointdataset_check1 %>% 
  filter(category=='Barely move')  

# group by Id and show summary for some columns
barely_move_details %>% 
  group_by(ActivityDate) %>% 
  dplyr::select(ActivityDate,TotalSteps,Calories,SedentaryActiveDistance,SedentaryMinutes)

barely_move_details %>% 
  filter(TotalSteps>5000) %>% 
  group_by(ActivityDate) %>% 
  dplyr::select(TotalSteps,Calories,SedentaryActiveDistance,SedentaryMinutes)
