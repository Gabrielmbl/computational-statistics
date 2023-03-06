#1) Give a scatter plot where each point represents a player that played in the 2000 season,
# with x-coordinate corresponding to the number of “at-bats” (abbreviated AB) and the y-coordinate
# corresponding to the number of “home runs” (abbreviated HR).

library(tidyverse)
df1 <- read_csv('hw02_batting.csv')
View(df1)
twoK <- df1 %>% filter(yearID == 2000)
View(twoK)

ggplot(data = twoK) +
  geom_point(mapping=aes(x=AB, y=HR))

#2) Major League Baseball actually consists of two separate leagues: the American League (lgID=AL)
# and the National League (lgID=NL). Give the same scatter plot as in #1, but now color the points
# according to which league the player played in.

ggplot(data = twoK) +
  geom_point(mapping=aes(x=AB, y=HR, color=lgID))


# 3) Repeat the plot in #2, but give a facet plot by year from 2000 to 2009
twoK9 <- df1 %>% filter(yearID >= 2000 & yearID <= 2009)
view(twoK9)

ggplot(data = twoK9) +
  geom_point(mapping=aes(x=AB, y=HR, color = lgID)) +
  facet_wrap(~ yearID, nrow = 2)


#4) Give box plots for each league that represents the number of hits (abbreviated H) by players
# that had over 100 hits.


df1 %>% filter(yearID == 2000, H > 100) %>% ggplot() + geom_boxplot(mapping=aes(x=lgID, y=H))



#5) Give a bar chart for the value of wheat exported to each continent.
df2 <- read_csv("hw02_exports.csv")
View(df2)

wheat <- df2 %>% filter(commodity == "wheat")
View(wheat)

ggplot(data=wheat) +
  geom_bar(mapping=aes(x=continent,y=value),stat="summary",fun=sum) +
  labs(y="Avg Value Exported")


df2 %>% filter(commodity == 'wheat') %>% ggplot() +
  geom_col(aes(x = continent, y = value))



#6) The end use code column in this data set is used to identify commodities. An end use code
# of the form 11*** gives energy sector commodities. We restrict our attention to these commodities.
# Give a stacked bar chart where each bar corresponds to a continent, the height of the bar corresponds
# to the total value of these commodities, and each sub-bar is colored according to the value of
# commodity (i.e. the bar is broken up into sub-bars where each sub-bar corresponds to the value of
#           commodities such as “fuel oil” and “natural gas”).


nrg <- df2 %>% filter(substr(df2$end_use_code, 1, 2) == "11")
View(nrg)

ggplot(data = nrg) +
  geom_col(mapping=aes(x=continent, y = value,fill=commodity)) + # geom_col does the summarize for us
  guides(x = guide_axis(angle=60)) +
  labs(x="Continent", y= "Value", fill="Commodity")
 



#7) We restrict our attention to commodities with end use codes of the form 41***. Give a fill bar
# chart where each bar corresponds to such a commodity with the bar colored by continent.

house <- df2 %>% filter(substr(df2$end_use_code,1,2) == "41") %>% 
  group_by(commodity, continent) %>% 
  summarize(total_value=sum(value))

View(house)

ggplot(data=house) +
  geom_col(mapping=aes(x=commodity, y = total_value, fill=continent),position="fill") + # GEOM_BAR JUST DOES COUNT. With GEOM_COL YOU CAN GIVE AN Y FOR VALUE
  guides(x=guide_axis(angle=60)) +
  labs(x="Commodities", fill="Continent")


#8)Create a map plot that shows the total value of exports sent to each country. You will need to use
# the summarize function to get this correct.

world <- as_tibble(map_data("world")) 

uniq_regions <- unique(df2$map_country)
uniq_regions

my_data <- tibble(region=uniq_regions, df2 %>% group_by(map_country) %>% summarize(val= mean(value,na.rm = TRUE)))
my_data

ggplot(data=my_data) +
  geom_map(mapping=aes(map_id=map_country,fill= val), map=world) +
  expand_limits(x=world$long,y=world$lat)



