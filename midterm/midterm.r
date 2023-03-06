library(tidyverse)

df <- read_csv('midterm_olympics.csv')
View(df)

#1a)
mean(df$height[(1:nrow(df))[!is.na(df$height)]])

dfhw <- df %>% filter(height != 'NA', weight != 'NA') 
dfh <- df %>% filter(height != 'NA') 
dfw <- df %>% filter(weight != 'NA')

# View(dfhw)
mean(dfh$height)


#1b)
median(df$weight[(1:nrow(df))[!is.na(df$weight)]])

median(dfw$weight)

#1c)
sd(df$height[(1:nrow(df))[!is.na(df$height)]])

sd(dfh$height)

#1d)
var(df$height[(1:nrow(df))[!is.na(df$height)]])

var(dfh$height)


#1e)
# cov(df$weight[(1:nrow(df))[!is.na(df$weight)]], df$height[(1:nrow(df))[!is.na(df$height)]])

# length(df$height[(1:nrow(df))[!is.na(df$height)]])
# length(df$weight[(1:nrow(df))[!is.na(df$weight)]])

cov(df$weight, df$height, use = 'complete.obs')
    

#1f)
cor(df$weight, df$height, use = 'complete.obs')

#2)

df %>% ggplot() +
  geom_boxplot(aes(x=sex, y = weight))

#3)
# install.packages('hexbin')
library(hexbin)
bin <- hexbin(df$weight, df$height)
plot(bin)

df %>% ggplot()+
  geom_hex(aes(x=df$weight, df$height))

#4)

df %>% group_by(nationality) %>% 
  summarize(g= sum(gold)) %>% filter(g >= 7) %>% 
  ggplot() +
  geom_col(aes(x=nationality, y=g))

#5)
df %>% mutate(n_medals = gold + silver + bronze) %>% 
  group_by(nationality) %>% 
  summarize(num_medals = sum(n_medals), g = sum(gold), s = sum(silver), b = sum(bronze)) %>%
  pivot_longer(cols = c(g,s,b), names_to ='type', values_to='value') %>% 
  filter(num_medals >= 24) %>%  ggplot() +
  geom_col(aes(x=nationality, y = num_medals/3, fill = type))



#6a)
dbinom(30, 45, 0.6)

#6b)
pbinom(25, 45,0.6)

#6c)
pbinom(32, 45, 0.6, lower.tail = FALSE)

#6d)
qbinom(0.7, 45, 0.6)


#7a) 
df %>% mutate(medal_counts = gold + silver + bronze) %>% 
  group_by(medal_counts) %>% 
  summarize(count=n())

#7b)
df %>% mutate(medal_counts = gold + silver + bronze) %>% 
  group_by(medal_counts) %>% 
  summarize(count=n()) %>% 
  mutate(prop = count/11538)


#7c)

df %>% drop_na(c("gold", "silver", "bronze")) %>%
  mutate(medal_count = gold + silver + bronze) %>% 
  mean(df$medal_count)
  
  
with_na <- df %>% mutate(medal_count = gold + silver + bronze)
  
without_na <- df %>% mutate(medal_count = gold + silver + bronze) %>% 
  filter(medal_count != 'NA')

nrow(with_na) == nrow(without_na) # TRUE, so no NAs ??


df %>% mutate(medal_count = gold + silver + bronze) %>% 
  mean(df$medal_count) #RESULTS IN NA

df %>% mutate(medal_count = gold + silver + bronze) %>% 
  filter(medal_count != 'NA') %>% 
  mean(df$medal_count) #RESULTS IN NA 


#7d)


