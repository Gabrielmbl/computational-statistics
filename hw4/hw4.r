library(tidyverse)

df <- read_csv('ws03_gun_violence.csv')
# This code is taken from the worksheet #3. It creates various date columns
# and filters the data s that we have only entries between Jan 1, 2014
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


df <- df %>% separate_rows(participant_status, sep="\\|\\|") %>% 
  separate_rows(participant_status, sep="\\|") %>% 
  separate_rows(participant_type, sep="\\|\\|") %>% 
  separate_rows(participant_type, sep="\\|") %>%
  separate(col = participant_status,
           into = c("participant_status_num",'participant_status_val'),
           sep="::") %>% 
  separate(col= participant_type,
           into = c("participant_type_num",'participant_type_val'),
           sep="::") %>% 
  filter(participant_status_num == participant_type_num) %>% 
  select(c(-participant_status_num,-participant_type_num)) %>% 
  rename(participant_status = participant_status_val,
         participant_type = participant_type_val)

# View(df)




#1) There are a variety of participant status designators in df. Some of them are quite curious,
# such as “Killed, Unharmed”. To make our analysis simpler, rename these values in df as follows:

df2 <- df %>% mutate(participant_status = 
                case_when(participant_status == 'Unharmed, Arrested' ~ 'Unharmed',
                  participant_status == 'Killed, Unharmed' ~ 'Killed',
                  participant_status == 'Killed, Arrested' ~ 'Killed',
                  participant_status == 'Injured, Unharmed' ~ 'Injured',
                  participant_status == 'Arrested' ~ 'Unharmed',
                  participant_status == 'Killed, Injured' ~ 'Killed',
                  participant_status == 'Injured, Unharmed, Arrested' ~ 'Injured',
                  participant_status == 'Killed, Unharmed, Arrested' ~ 'Killed'
                )
    )

# View(df2)





old_status <- c('Unharmed, Arrested',
                  'Killed, Unharmed',
                  'Killed, Arrested',
                  'Injured, Unharmed',
                  'Injured, Arrested',
                  'Arrested',
                  'Killed, Injured',
                  'Injured, Unharmed, Arrested',
                  'Killed, Unharmed, Arrested')

new_status <- c('Unharmed',
                        'Killed',
                        'Killed',
                        'Injured',
                        'Injured',
                        'Unharmed',
                        'Killed',
                        'Injured',
                        'Killed')

# This renames incident descriptions so that they are more concise.
for(i in 1:length(old_status)){
  df$participant_status[df$participant_status == old_status[i]] <- new_status[i]
}  

# View(df)





#2) If we filter df to rows with incident id == 92734, we will obtain three rows. In the first
# two rows, the participant status and participant type values are “Injured” and “Victim” (respectively). The other row has values of “Unharmed” and “Subject-Suspect”. Consolidate df
# by adding a new column called participant count that counts the number of times each pair
# of values within participant status and participant type occur for a given incident. This
# change will result in there only being two rows with incident id == 92734. The first row will
# have values “Injured”, “Victim” and 2 within the participant status, participant type and
# participant count columns (respectively). The second row will have values ‘Unharmed”, “SubjectSuspect’ and 1 within the participant status, participant type and participant count columns
# (respectively). I recommend using the group by, across and summarize commands to create
# participant count.


df2 <- df %>% 
  group_by(across(everything())) %>% 
  summarize(participant_count = n())

# View(df2)





#3) The participant type and participant status entries can be merged together into a single
# column using the unite function. Let’s call this column participants. This column will contain
# the following values:

df3 <- df2 %>% unite(col = 'participants', c('participant_type', 'participant_status'),sep = '_')  
# View(df3)


df4 <- df3 %>% 
  pivot_wider(names_from=participants,
              values_from=participant_count,
              values_fill = 0)

# View(df4)


#4) Give a stacked bar chart by year with sub-bars colored for each of the six types of participants
# with with heights given by the number participants within that category during that year.


# df3 %>% filter(participants != 'NA_NA') %>% ggplot() +
#   geom_col(aes(x=year, y=participant_count, fill = participants))

df3 %>% filter(participants != 'NA_NA') %>% group_by(year, participants) %>% 
  summarize(p_count = sum(participant_count)) %>% ggplot() +
  geom_col(aes(x=year, y= p_count, fill = participants))


#5) 
library(USAboundaries)
library(sf)

# Non - contiguous territories of the United States
non_cont <- c("Hawaii", "Alaska","Virgin Islands","American Samoa",
              "Puerto Rico","Guam","Northern Mariana Islands")

# We get a map of the contiguous United States
us_map <- us_congressional(resolution = "high") %>%
  filter(!(state_name %in% non_cont))


# We create a column called my_ random for displaying random values on the
# map. I use the dim command to find out how many rows are in us_ map so
# that my_ random has the correct number of entries .
us_map$my_random <- runif(dim(us_map)[1])

ggplot(us_map) +
  geom_sf(aes(fill = my_random), color ="black") +
  scale_fill_gradient(low ="yellow", high ="red") +
  theme_void()


# Create a map that shows the number killed by congressional district

# View(us_map)

# View(df3)

df3_nk <- df3 %>% group_by(congressional_district, state) %>% 
  summarize(num_killed = sum(n_killed))

# View(df3_nk)

us_map$cd116fp <- as.numeric(us_map$cd116fp)

new_us_map <- us_map %>% 
       left_join(df3_nk, by = c('cd116fp' = 'congressional_district', 'state_name' = 'state'))


# View(new_us_map)

ggplot(new_us_map) +
  geom_sf(aes(fill = num_killed), color ="black") +
  scale_fill_gradient(low ="yellow", high ="red", trans = "log2") +
  theme_void()




#6) Restrict the map in #5 to states in the north east (New York, Pennsylvania, New Jersey,
# Connecticut, Rhode Island, Massachusetts, New Hampshire, Vermont, Maine).

northeast <- c('New York', 'Pennsylvania', 'New Jersey',
                 'Connecticut', 'Rhode Island', 'Massachusetts', 'New Hampshire',
                'Vermont', 'Maine')
  
new_us_map %>% filter(state_name %in% northeast) %>% 
  ggplot() +
  geom_sf(aes(fill = num_killed), color ="black") +
  scale_fill_gradient(low ="yellow", high ="red", trans = "log2") +
  theme_void()



#7) The victim survival rate is given by:
# 1 − number of victims killed / number of victims .
# Compute the victim survival rate by congressional district and display the
# results on the congressional district map of the contiguous United States.

# df3 %>% group_by(congressional_district) %>% 
#  summarize(survival_rate = (1 - n_killed) / )


# View(df3)

df3 %>% group_by(participants) %>% # Finding out what kinds of participants are in the data
  summarize(count = n())

victims <- c('Victim_Injured', 'Victim_Killed', 'Victim_Unharmed')

df_victims <- df3 %>% filter(participants %in% victims) %>% 
  mutate(val = TRUE) %>% 
  pivot_wider(names_from = participants,
              values_from = val,
              values_fill = list(val=FALSE))

View(df_victims)

df_victims <- df_victims %>% group_by(congressional_district, state) %>% 
  summarize(survival_rate = (1- (sum(Victim_Killed)/sum(Victim_Killed, Victim_Injured, Victim_Unharmed))))

# df_victims

new_us_map2 <- us_map %>% 
  left_join(df_victims,
            by = c('cd116fp' = 'congressional_district', 'state_name' = 'state'))

new_us_map2 %>% ggplot() +
  geom_sf(aes(fill = survival_rate), color ="black") +
  scale_fill_gradient(low ="yellow", high ="red") +
  theme_void()




#8) Compute the suspect survival rate during defensive incidents by congressional district. Display
# the results on the congressional district map of the contiguous United States.

suspects <- c('Subject-Suspect_Injured', 'Subject-Suspect_Killed', 'Subject-Suspect_Unharmed')

df_suspects <- df3 %>% filter(participants %in% suspects) %>% 
  mutate(val = TRUE) %>% 
  pivot_wider(names_from = participants,
              values_from = val,
              values_fill = list(val=FALSE))

df_suspects <- df_suspects %>% rename_at('Subject-Suspect_Injured', ~'Subject_Suspect_Injured') # The original name with "-" was giving me trouble
df_suspects <- df_suspects %>% rename_at('Subject-Suspect_Killed', ~'Subject_Suspect_Killed')
df_suspects <- df_suspects %>% rename_at('Subject-Suspect_Unharmed', ~'Subject_Suspect_Unharmed')

# View(df_suspects)

df_suspects <- df_suspects %>% filter(defensive)

df_suspects <- df_suspects %>% group_by(congressional_district, state) %>% 
  summarize(survival_rate = (1- (sum(Subject_Suspect_Killed)/sum(Subject_Suspect_Injured, Subject_Suspect_Killed, Subject_Suspect_Unharmed))))

# df_suspects

new_us_map3 <- us_map %>% 
  left_join(df_suspects,
            by = c('cd116fp' = 'congressional_district', 'state_name' = 'state'))

new_us_map3 %>% ggplot() +
  geom_sf(aes(fill = survival_rate), color ="black") +
  scale_fill_gradient(low ="yellow", high ="red") +
  theme_void()
