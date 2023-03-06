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
