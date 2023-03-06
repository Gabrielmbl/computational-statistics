library(tidyverse)

df <- read.csv('ws03_gun_violence.csv')

# This code is taken from the worksheet #3. It creates various date columns
# and filters the data so that we have only entries betwee Jan 1, 2014
# and December 31, 2017. We then use the separate_rows function to disaggregate
# the various incident descriptions. Should there be more than one such
# description for a given, this code then creates duplications of that row
# and assigns exactly one description to each such row. We then rename
# the incident_characteristics column to the incident column.
df <- df %>% 
  mutate(nice_date = as.Date(date,"%Y-%m-%d")) %>%
  mutate(year_month = format(nice_date,"%Y-%m")) %>%
  mutate(year = format(nice_date,"%Y")) %>%
  filter(nice_date >= as.Date('2014-01-01') & 
           nice_date <= as.Date('2017-12-31')) %>% 
  separate_rows(incident_characteristics, sep="\\|\\|") %>% 
  separate_rows(incident_characteristics, sep="\\|") %>% 
  rename(incident=incident_characteristics)


my_incidents <- c("Shot - Wounded/Injured",
                  "Shot - Dead (murder, accidental, suicide)",
                  "Non-Shooting Incident",
                  "Shots Fired - No Injuries",
                  "Officer Involved Incident",
                  "Possession of gun by felon or prohibited person",
                  "Accidental Shooting",
                  "Defensive Use",
                  "Assault weapon (AR-15, AK-47, and ALL variants defined by law enforcement)",
                  "Terrorism Involvement",
                  "Gang involvement",
                  "Drug involvement")

short_my_incidents <- c("shot_injured",
                        "shot_dead",
                        "non_shooting",
                        "shots_no_injuries",
                        "officer_involved",
                        "felon",
                        "accidental",
                        "defensive",
                        "assault_weapon",
                        "terrorism",
                        "gang",
                        "drugs")

# This renames incident descriptions so that they are more concise.
for(i in 1:length(my_incidents)){
  df$incident[df$incident == my_incidents[i]] <- short_my_incidents[i]
}  

# We filter out incidents that aren't within our lists of incidents.
# We then create a column for each type of incident. The column takes on
# TRUE/FALSE values depending on whether it was described as a characteristic.
# This basically removes the aforementioned duplicated rows.
df <- df %>% filter(incident %in% short_my_incidents) %>% 
  mutate(val=TRUE) %>%
  pivot_wider(names_from=incident,
              values_from=val,
              values_fill=list(val=FALSE))

# View(df)

#1) Give a bar chart for the number of 'defensive' incidents by year

df %>% group_by(year) %>% 
  summarize(def = sum(defensive)) %>% 
  ggplot() + geom_col(mapping=aes(x=year, y=def))



#2) Give a stacked bar chart for the number killed by year with sub-bars colored according to
# whether the weapon was an ‘assault’ vs. ‘non-assault’ weapon.


df %>% ggplot() + geom_col(mapping = aes(x=year, y = n_killed, fill = assault_weapon))

# Another way to do it:
df %>% group_by(year, assault_weapon) %>% 
  summarize(num_killed= sum(n_killed)) %>% 
  ggplot() + geom_col(mapping=aes(x= year, y = num_killed, fill = assault_weapon))


#3) Give a stacked bar chart for the number killed in a mass shootings (four or more killed) by year
# with sub-bars colored according to whether the weapon was an ‘assault’ vs ‘non-assault’ weapon.

df %>% filter(n_killed >= 4) %>%  group_by(year, assault_weapon) %>% 
  summarize(num_killed= sum(n_killed)) %>% 
  ggplot() + geom_col(mapping=aes(x= year, y = num_killed, fill = assault_weapon)) # why doesnt this work????

# Another way to do it:

dfmass <- df %>% filter(n_killed>= 4)
View(dfmass)  

dfmass %>% ggplot() + geom_col(mapping=aes(x=year, y = n_killed, fill= assault_weapon))


#4) Give a dodge bar chart by year for the number of incidents for each incident category we’re
# considering. This means that for each year, there should be a bar chart that counts the number of
# incidents in the following categories: officer involved, felon, accidental, defensive, assault weapon,
# terrorism, gang, drugs.



df2 <- df %>% pivot_longer(cols=c("officer_involved", "accidental", "drugs", "gang","felon", "defensive", 
                                  "assault_weapon", "terrorism"),
                           names_to = "incident",
                           values_to = "happened")

View(df2)




df2 %>% filter(happened) %>% 
  ggplot() + 
  geom_bar(mapping=aes(x=year,fill = incident), position = "dodge") +
             guides(x = guide_axis(angle = 60)) +
             labs(x = "year", y="Count", fill="Incident")




#5) In #4, the plot was a bit deceptive in that it implicitly assumed there was no overlap between
# these different types of incidents. Count the number of incidents, by year, that fell within 2 or more
# incident categories.

#??????????????????????????????????????????????????????????????????????????????????????????????????????????????????????

df2happened <- df2 %>% filter(happened)
df2happenedduplicated <- df2happened[duplicated(df2happened$incident_id),]

View(df2happenedduplicated)

df2happenedduplicated %>% ggplot() +
  geom_bar(mapping=aes(x=year,fill=incident), position = "dodge") +
  guides(x = guide_axis(angle = 60)) +
  labs(x = "Year", y = "Count", fill = "Incident")

df2happenedduplicated %>% group_by(year) %>%  #answer
  summarize(tot_incidents= n())

df %>% mutate(in_group = (accidental | assault_weapon | defensive | drugs | felon | gang | officer_involved | terrorism)) %>% View()

df2 %>% mutate(in_group = (accidental | 
                            assault_weapon | defensive |
                            drugs | felon | gang |
                            officer_involved | terrorism)) %>% 
  
  
#df %>% mutate(in_group = (accidental | 
#                            assault_weapon | defensive |
#                            drugs | felon | gang |
#                            officer_involved | terrorism)) %>% 
#  select(in_group)    
#dftst <- df %>% mutate(in_group = (accidental | assault_weapon | defensive | drugs | felon | gang | officer_involved | terrorism))
View(dftst)
# nrow(df2happenedduplicated)

#6)  Give a dodge bar chart by year for the number killed in incidents that were either officer
# involved, felon, accidental, defensive, assault weapon, terrorism, gang, drugs. Each such incident
# bar should give the number killed in such incidents. This problem is similar to #4, except of simply
# counting how many incidents occurred, we count how many were killed in such incidents.


df2 %>% filter(happened) %>% select(year, incident, n_killed) %>% group_by(year, incident) %>%
  summarize(num_killed = sum(n_killed)) %>% 
  ggplot() +
  geom_col(aes(x=year, y= num_killed, fill = incident), position = 'dodge')



#7) For lack of a better term, let’s say that an incident is ‘of interest’ if it falls into one of the
# following categories: officer involved, felon, accidental, defensive, assault weapon, terrorism, gang or
# drugs. Give a dodge bar chart by year for the number killed in ‘of interest’ incidents versus those
# that are ‘not of interest’.

df %>%  
  mutate(interest= 
           case_when(
             officer_involved | felon | accidental |
               defensive | assault_weapon | terrorism | 
               gang | drugs ~ "of interest",
             shots_no_injuries | shot_dead | shot_injured |
               non_shooting ~ "not of interest"
           )) %>% select(year,interest, n_killed) %>% group_by(year, interest) %>% 
  summarize(num_killed = sum(n_killed)) %>% 
  ggplot() +
  geom_col(aes(x=year,y=num_killed, fill=interest),position = 'dodge')





#8) Give a dodge bar chart for the number of ‘defensive’ incidents by year along with the total
# number killed (by year) by mass shootings (4 or more killed).

#????????????????????????????????????????????????????????????????????????????????????????????????????????????

View(df2)

df2 %>% filter(happened) %>%  
  mutate(category = 
           case_when(incident == "defensive" & n_killed < 4 ~ "defensive",
                     incident == "defensive" & n_killed >= 4 ~ "defensive mass shooting",
                     incident != "defensive" & n_killed >= 4 ~ "mass shootings"
           )
  ) %>% drop_na(category) %>% 
  ggplot() + geom_bar(mapping =aes(x=year,fill = category), position = 'dodge')


df2 %>% filter(happened) %>%  
  mutate(category = 
           case_when(incident == "defensive" ~ "defensive",
                     n_killed >= 4 ~ "mass shootings"
           )
  ) %>% drop_na(category) %>%
  group_by(year, category) %>% 
  summarize(num_killed= sum(n_killed)) %>%  
  ggplot() + geom_col(mapping =aes(x=year, y = num_killed ,fill = category), position = 'dodge')






 
  

#9) 

dfstatus <- df %>% 
  separate_rows(participant_status, sep="\\|\\|") %>% 
  separate(participant_status,sep='::',into=c('person_id','status'))

View(dfstatus)

dftype <- df %>% 
  separate_rows(participant_type, sep="\\|\\|") %>% 
  separate(participant_type, sep='::', into=c('person_id', 'type'))

View(dftype)


dfstatustype <- merge(dfstatus, dftype) %>% select(incident_id, person_id, status, type, everything())
View(dfstatustype)
