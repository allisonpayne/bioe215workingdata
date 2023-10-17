library(tidyverse)

# Read and inspect 
surveys <- read_csv("data/portal_data_joined.csv") #use underscore for the tidyverse version
head(surveys)
summary(surveys)

#Type of column species_id and hindfoot_length?
class(surveys$species_id) #character
class(surveys$hindfoot_length) #numeric

#class can be dangerous, use typeof instead. Same in atomic vector but not in others

#Q2 how many rows and columns in survey?
nrow(surveys) #34786
ncol(surveys) #13

#select & mutate affects columns 
#filter & arrange affects rows 

select(surveys, plot_id, species_id, weight)
#select to rename things
select(surveys, plot_id, weight_g = weight)
#select to remove rows
select(surveys, -record_id, -species_id)

#filters work on conditions
filter(surveys, year == 1995)
filter(surveys, year == 1995, plot_id == 7)
filter(surveys, month == 2 | day == 20)

#Q3 filter survyes to records in november, hindfoot length is greater than 36

filter(surveys, month == 11, hindfoot_length > 36)

#Q4 fix these errors 

filter(surveys, year == 1995)
filter(surveys, plot_id == 2)

#Pipes 

surveys_psw <- surveys %>% 
  filter(year == 1995) %>% 
  select(plot_id, weight)

#Subset surveys to animals collected before 1995 with columns year, sex, weight

surveys_ysw <- surveys %>% 
  filter(year == 1995) %>% 
  select(year, sex, weight)

#mutate
surveys %>% 
  mutate(weight_kg = weight / 1000) %>% 
  view()

surveys %>% 
  mutate(weight_kg = weight / 1000, 
         weight_lb = weight_kg * 2.2)

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight / 1000, 
         weight_lb = weight_kg * 2.2) %>% 
  view()

#contains only species id column, and a new column hindfoot cm with no NAs and all less than 3

surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  mutate(hindfoot_cm = hindfoot_length / 10) %>% 
  filter(hindfoot_cm < 3) %>% 
  select(hindfoot_cm, species_id)

#group by and summarize 

surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys %>% 
  drop_na(weight) %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight), 
            .groups = "drop") %>% 
  arrange(desc(mean_weight))

#arrange sorts it 

#Q7 

surveys %>% 
  group_by(plot_type) %>% 
  summarize(n_animals = n())

#Q8

surveys %>% 
  group_by(species_id) %>% 
  drop_na(hindfoot_length) %>% 
  summarize(hind_mean = mean(hindfoot_length), 
            hind_min = min(hindfoot_length), 
            hind_max = max(hindfoot_length), 
            n_obsv = n())

#Q9

surveys %>% 
  group_by(year) %>% 
  drop_na(weight) %>% 
  filter(weight == max(weight)) %>% 
  select(year, genus, species_id, weight) %>% 
  arrange(year)
