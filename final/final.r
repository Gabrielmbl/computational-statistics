library(tidyverse)

#1
df <- read_csv('final_college_majors.csv')
#View(df)

df %>% ggplot() +
  geom_point(aes(x= Median, y=Unemployment_rate, color= Major_category))

#2
df1 <- df %>% filter(Major == 'ACTUARIAL SCIENCE')
#df %>% filter(Major == 'ACTUARIAL SCIENCE') %>% View()

df1 <- df1 %>% pivot_longer(cols=c(P25th, Median, P75th), names_to = 'type', values_to = 'value')
# View(df1)

df1 %>% ggplot() + 
  geom_col(aes(x=type, y=value))

#3

# df %>% group_by(Major) %>% 
#   summarize(avg= mean(Median)) %>% View()

df %>% group_by(Major) %>% 
  summarize(avg= mean(Median)) %>%
  ggplot() +
  geom_col(aes(x = Major, y = avg)) + coord_flip()


#4

df %>% group_by(Major) %>% 
  summarize(avg25= mean(P25th), avgMed=mean(Median), avg75=mean(P75th)) %>%
  pivot_longer(cols=c(avg25, avgMed, avg75), names_to='type',values_to='value') %>% 
  ggplot() +
  geom_col(aes(x=Major, y= value, fill = type), position ='dodge') + coord_flip()

#5a
pnorm(60, mean= 51, sd=9 , lower.tail=FALSE)

#5b
pnorm(30, mean=51,sd=9)

#5c
qnorm(0.85, mean=51,sd=9)

#6a
tbl <- tibble(i = 1:5000) %>% rowwise() %>% 
  mutate(X = list(runif(n=10, min=0, max=1))) %>% 
  mutate(Y = sum(X))
# View(tbl)

#6b
tbl %>% ggplot()+
  geom_histogram(aes(x=Y, y =..density..))

#6c
my <- mean(tbl$Y)
sdy <- sd(tbl$Y)

tbl %>% ggplot() +
  geom_histogram(aes(x=Y, y =..density..)) +
  geom_function(fun=dnorm, args=c(mean=my,sd=sdy), color='red')


#7
n <- 40
xbar <- 42
s <- 12
alpha <- 0.01

xL <- xbar + s/sqrt(n)*pnorm(1-alpha/2,lower.tail=FALSE)
xL
xU <- xbar + s/sqrt(n)*pnorm(alpha/2,lower.tail=FALSE)
xU
#[42.30333, 42.9449]

#8
n <- 20
xbar <- 76
s1 <- 21
m <- 20
ybar <- 64
s2 <- 20
alpha <- 0.05
# ho: xbar-ybar = Do
# ha: xbar-ybar > Do

s_p <- sqrt( ((n-1)* s1^2 + (m-1)*s2^2)/(n+m-2) )
t <- (xbar-ybar)/(s_p*sqrt(1/n + 1/m))
t
talpha <- qt(alpha,df = n+m-2,lower.tail=FALSE)
talpha

talpha < t # TRUE
# Since t is in the RR, we fail to accept the null hypothesis. Thus, we conclude
# that there is sufficient evidence to claim that weekend shoppers spend more on average
# than weekday shoppers.

#9a
df3 <- read_csv('final_steel.csv')
df3 %>% View()
df3 %>% ggplot() +
  geom_point(aes(x= elongation, y= 'tensile strength'))

#9b
lin_mod <- lm(elongation~`tensile strength`, data=df3)
summary(lin_mod) # y= -0.0090357x + 28.8064452. You can see through (Intercept) and `tensile strength`




xbar <- mean(df3$elongation, na.rm=TRUE)
xbar
ybar <- mean(df3$`tensile strength`)
ybar
Sxy<- sum((df3$elongation - xbar) * (df3$`tensile strength` - ybar), na.rm=TRUE)
Sxy
Sxx <- sum((df3$elongation - xbar)^2, na.rm=TRUE)
Sxx

beta1hat <- Sxy / Sxx
beta1hat
beta0hat <- ybar - beta1hat*xbar
beta0hat


df3 %>% ggplot() +
  geom_point(aes(x=elongation, y='tensile strength')) +
  geom_smooth(aes(x=elongation, y='tensile strength'), method='lm')
