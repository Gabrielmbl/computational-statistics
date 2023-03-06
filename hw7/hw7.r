library(tidyverse)

#1a
?dgamma

ggplot()+ xlim(0,10) +
  geom_function(fun=dgamma, args=c(shape=1, scale=1), color='blue') +
  geom_function(fun=dgamma, args=c(shape=2, scale=1), color='red') +
  geom_function(fun=dgamma, args=c(shape=3, scale=1), color='green') +
  geom_function(fun=dgamma, args=c(shape=1, scale=2), color='orange') +
  geom_function(fun=dgamma, args=c(shape=2, scale=2), color='purple') +
  geom_function(fun=dgamma, args=c(shape=3, scale=2), color='black') 


#1b
n <- 25
tbl <- tibble(index = 1:10000) %>% rowwise() %>%
  mutate(X = list(rgamma(n, shape = 2, scale = 1))) %>% 
  mutate(xbar = 1/n*(sum(X))) %>% 
  mutate(lnx = 1/n*(sum(log(X)))) %>% 
  mutate(xlnx = 1/n*(sum(X*log(X)))) %>% 
  mutate(that = xlnx - xbar*lnx)


#1c

tbl %>% ggplot()+
  geom_histogram(aes(x=that))

mean(tbl$that)

# I believe theta hat is a biased estimator since its mean is 0.9598385, whereas theta is 1.

#1d

tbl <- tbl %>% mutate(khat= xbar/that) 

tbl %>% ggplot() +
  geom_histogram(aes(x=khat))

mean(tbl$khat)

# Khat is a biased estimator since its mean is 2.252948, whereas k is 2.


#1e

tbl <- tbl %>% mutate(ttilde= (n/(n-1))*that) %>% 
  mutate(ktilde= khat- (1/n)*(3*khat-2/3*khat/(1+khat)-4/5*khat/((1+khat)^2)))


#1f

tbl %>% ggplot() +
  geom_histogram(aes(x=that), fill = 'green', alpha= 0.5)+
  geom_histogram(aes(x=ttilde),fill='yellow',alpha=0.5)

mean(tbl$ttilde)

# Theta tilde is an unbiased estimator since its mean is 0.9998318, which is really close to theta that is 1.

#1g

tbl %>% ggplot() +
  geom_histogram(aes(x=khat), fill = 'blue', alpha= 0.5)+
  geom_histogram(aes(x=ktilde),fill='red',alpha=0.5)

mean(tbl$ktilde)

# K tilde is a bias estimator since its mean is 2.007593, which is really close to theta that is 2.

#2a
pnorm(q= 4.1, mean=4, sd = sqrt(2/40)) - pnorm(q=3.9,mean = 4, sd = sqrt(2))


#2b

tbl2 <- tibble(index=1:10000) %>% rowwise() %>% 
  mutate(X = list(rnorm(n=40,mean=4,sd=sqrt(2/40)))) %>% 
  mutate(mean= mean(X))

# View(tbl2)

tbl2 %>% filter(3.9 < mean, mean < 4.1) %>% nrow()/10000*100



#2c

xLc <- 4 + sqrt(2)/sqrt(40) * qnorm(1-0.01/2,lower.tail=FALSE)
xLc

xUc <- 4 + sqrt(2)/sqrt(40) * qnorm(0.01/2, lower.tail=FALSE)
xUc  


# [3.424027, 4.575973]


#2d

tbl2 %>% filter(xLc < mean, mean < xUc) %>% nrow()/10000*100

#2e
1-0.95
xUe <- 4 + sqrt(2)/sqrt(40) * qnorm(0.05, lower.tail=FALSE)
xUe

#(-infinity,4.3678]

#2f

xLf <- 4 + sqrt(2)/sqrt(40) * qnorm(1-0.05, lower.tail=FALSE)
xLf

#[3.6322, infinity)


#3
# n = 20 samples, sample variance s^2 = 5
# 1 - alpha = 0.99 two-sided confidence interval for sigma sqrd

sL <- (20-1)*5/qchisq(0.01/2, df = 20-1,lower.tail=FALSE)
sL
sU <- (20-1)*5/qchisq(1-0.01/2, df=20-1, lower.tail=FALSE)
sU

# [2.462272, 13.88083]


#4a
# install.packages("haven")
library (haven)
df <- read_sav('ATP W44.sav')
#View(df)

# unique(df$WRLDRELCLASS_W44) GAVE ME THE LABELS 

df1 <- df %>% mutate(X= case_when(WRLDRELCLASS_W44 == 1 ~ 1,
                           WRLDRELCLASS_W44 == 2 ~ 0,
                           WRLDRELCLASS_W44 == 99 ~ 0))

smean <- mean(df1$X)
smean

svar <- var(df1$X)
svar


#4b
n <- nrow(df1)

xLb <- smean + sqrt(svar)/sqrt(n) * qnorm(1-0.01/2,lower.tail=FALSE)
xLb

xUb <- smean + sqrt(svar)/sqrt(n) * qnorm(0.01/2, lower.tail=FALSE)
xUb  


# [0.4443172, 0.4688174]


#4c

# The fact that the data is weighted.
