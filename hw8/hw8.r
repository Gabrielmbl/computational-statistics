library(tidyverse)

#1
n <- 130
xbar <- 98.25
s <- 0.73
alpha <- 0.01

xL <- xbar + s/sqrt(n) * qt(1-alpha/2, df = n-1, lower.tail=FALSE)
xL
xU <- xbar + s/sqrt(n) * qt(alpha/2, df = n-1, lower.tail=FALSE)
xU
#[98.08261, 98.41739]


#2
# Ho = 280
# Ha > 280
s <- 54
n <- 10
mu <- 280
xbar <- 358
alpha <- 0.01

t <- (xbar-mu)/(s/sqrt(n))
t
talpha <- qt(alpha,df = n-1, lower.tail=FALSE)
talpha
#(infinity, 2.821438]
talpha < t # TRUE
# We reject Ho since t is in the RR.



#3
n <- 6
alpha <- 0.9
sample <- c(85.4, 86.8, 86.1, 85.3, 84.8, 86)
s <- sd(sample)
alpha <- 0.1

s2U <- (n-1)*s^2/qchisq(1-alpha, df = n-1, lower.tail=FALSE)
s2U
#(-infinity, 1.560778]


#4a
n <- 20
xbar <- 505
s <- 57
alpha <- 0.1

xL <- xbar + s/sqrt(n)*qt(1-alpha/2,df=n-1,lower.tail=FALSE)
xL
xU <- xbar + s/sqrt(n)*qt(alpha/2,df=n-1,lower.tail=FALSE)
xU
#[482.9612, 527.0388]


#4b
n <- 20
xbar <- 495
s <- 69
alpha <- 0.1

xL <- xbar + s/sqrt(n)*qt(1-alpha/2,df=n-1,lower.tail=FALSE)
xL
xU <- xbar + s/sqrt(n)*qt(alpha/2,df=n-1,lower.tail=FALSE)
xU
#[468.3214, 521.6786]


#5 
s1 <- 10
n <- 20
s2 <- 12
alpha <- 0.01

# ho: s1 = s2
# ha: s1 < s2

q <- (n-1)*s^2/s2^2
q

qchisq_alpha <- qchisq(alpha,df=n-1)
qchisq_alpha < q # TRUE
# Therefore, we reject the null hypothesis and accept the hypothesis that the scores from the new
# test set significantly more variable than scores from the standard

#6a

n1 <- 10
ybar1 <- 0.041
s1 <- 0.017
n2 <- 13
ybar2 <- 0.026
s2 <- 0.006
alpha <- 0.05

#h0: ybar1 - ybar2 = do
#ha: ybar1 - ybar2 > do

s_p <- sqrt( ((n1-1)* s1^2 + (n2-1)*s2^2)/(n1+n2-2) )

t <- (ybar1-ybar2)/(s_p*sqrt(1/n1 + 1/n2))
t

talpha <- qt(alpha, df = n1 + n2-2, lower.tail=FALSE)
talpha < t # TRUE
# Therefore we reject the null hypothesis and accept the alternative one since
# t falls into RR. Therefore, their means differ.


#6b
pt(t, df= n1+n2 -2, lower.tail=FALSE)

#7a
df <- read_csv('ws02_weather.csv')
df %>% ggplot() +
  geom_point(aes(x=humidity, y=cloudcover))

#7b
lin_mod <- lm(humidity~cloudcover, data=df)
summary(linear_model)

df %>% ggplot() +
  geom_point(aes(x=humidity, y=cloudcover)) +
  geom_smooth(aes(x=humidity, y=cloudcover), method='lm')

#7c
# I believe so, since humidity and cloudcover follow a positive correlation, which is
# what the linear regression line displays as well.

#7d
xbar <- mean(df$humidity)
ybar <- mean(df$cloudcover)
varx <- var(df$humidity)
vary <- var(df$cloudcover)

n <- nrow(df[!is.na(df$humidity),]) # 365
m <- nrow(df[!is.na(df$cloudcover),]) # 365

z <- (xbar-ybar)/sqrt(varx/n + vary/m)
