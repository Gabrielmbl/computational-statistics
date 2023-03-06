library(tidyverse)

# 1a) Consider X ∼ U[0,3]. Research the runif command. Use it to generate 1000 values for X.
# Store these values in a tibble. Display the results using a histogram.

tbl <- tibble(i = 1:1000) %>% rowwise() %>% 
  mutate(X = runif(n=1, min = 0, max = 3))

# tbl

tbl %>% ggplot() +
  geom_histogram(aes(x=X))


# 1b) Modify the plot from part (a) so that the histogram and probability density function (dunif)
# are displayed together.

# ????????????????????????????????????????????? VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV

tbl

tbl %>% ggplot() + 
  geom_histogram(aes(x=X,y=..density..)) +
  geom_function(color='red', fun=dunif,args=c(min=0,max=3))


# 1c)Let’s denote the random values generated in part (a) by xi. Compute the following quantities
# from the tibble in part (a): max(xi), 1001/1000 max(xi) and 2x.

max(tbl$X)
(1001/1000)*max(tbl$X)
# 2*(1/1000*sum(tbl$X))
2*(mean(tbl$X))

# 1d) Let’s repeat the experiment in part (a) 1000 times. For each such experiment, compute
# max(xi), 1001/1000 max(xi) and 2x. Store these results in a tibble.


tbl2 <- tibble(i = 1:1000) %>% rowwise() %>% 
  mutate(X = list(runif(n=1000, min = 0, max = 3)))
  
tbl2 <- tbl2 %>% mutate(max = max(X)) %>% 
  mutate(thmax = max*(1001/1000)) %>% 
  mutate(twoxbar = 2*(mean(X)))

# View(tbl2)




# 1e) Display the results of part (d) in overlapping histograms for max(xi), 1001
# 1000 max(xi) and 2x. Be
# sure to set alpha = 0.5 for each histogram so that one histogram doesn’t obscure another.
# Also set binwidth=0.01. Also be sure to choose different colors for each histogram.


tbl2 %>% ggplot() +
  geom_histogram(color='red',alpha=0.5, aes(x=max), binwidth=0.01) +
  geom_histogram(color='blue', alpha = 0.5, aes(x= thmax), binwidth=0.01) +
  geom_histogram(color='green', alpha=0.5, aes(x=twoxbar), binwidth=0.01)

# 1f) Compute the (sample) mean and standard deviation for the max(xi), 1001
# 1000 max(xi) and 2x.
# Display these results in a tibble (hint: pivot longer and summarize can do this nicely).


tbl3 <- tbl2 %>% pivot_longer(cols= c(max,thmax,twoxbar),names_to='type',values_to='val')
# View(tbl3)

tbl4 <- tbl3 %>% group_by(type) %>% summarize(samplemean = mean(val), std = sd(val))

# View(tbl4)
tbl4 


# 1g) 



tbl2 %>% ggplot() +
  geom_histogram(color = 'green', aes(x = twoxbar, y=..density..), binwidth=0.01) +
  geom_function(color='red', fun= dnorm, args=c(mean=3,sd=sqrt(3/nrow(tbl2)))) # Xbar ~ N(1.5, 0.75/n) --> E(2Xbar) = 2(1.5) THIS IS MEAN.
                                                                               # Var(2Xbar) = 4(0.75/n) == 3/n



# 1h)
mu <- 1.5*2
mu
sigmasq <- sqrt(3/nrow(tbl2))
sigmasq

pnorm(mu+0.01,mean=mu,sd=sigmasq) - pnorm(mu-0.01,mean=mu,sd=std)


# 1i)
? dbeta

tbl2 <- tbl2 %>% rowwise() %>% mutate(Z = max/3)
# View(tbl2)

tbl2 %>% 
  ggplot() + # xlim(0.991,1) +
  geom_histogram(aes(x=Z, y=..density..)) +
  geom_function(color='red',fun=dbeta,args=c(shape1=1000,shape2=1))

# 1j)
?pbeta

pbeta((mu + 0.01)/3, shape1 = 1000, shape2 = 1) - pbeta((mu - 0.01)/3, shape1 = 1000, shape2 = 1)


# 1k)

pbeta(((mu + 0.01)*(1000/1001))/3, shape1 = 1000, shape2 = 1) - pbeta(((mu - 0.01)*(1000/1001))/3, shape1 = 1000, shape2 = 1)

# 2a)

df <- read_csv('hw06_creditcard.csv')
df <- df %>% select(Amount, Class)
# View(df)
df1 <- df %>% filter(Amount < 2000)

df1leg <- df1 %>% filter(Class == 0)
df1fra <- df1 %>% filter(Class == 1)

ggplot() +
  geom_histogram(data=df1fra, fill ='red',alpha=0.5,aes(x=Amount,y=..density..)) +
  geom_histogram(data=df1leg, fill = 'green', alpha=0.5, aes(x=Amount,y=..density..))
  

# 2b) 
# View(df1)

lmean <- df1 %>% filter(Class == 0) %>% pull(Amount) %>% mean()
lstd <- df1 %>% filter(Class == 0) %>% pull(Amount) %>% sd()

# 2c)

df1 %>% filter(Class == 1) %>% pull(Amount) %>% mean()
df1 %>% filter(Class == 1) %>% pull(Amount) %>% sd()

# 2d) 
df2 <- tibble(i = 1:2000) %>% rowwise() %>%  mutate(sm = (df1 %>% filter(Class == 0) %>% 
  select(Amount) %>% pull %>% sample(size =491) %>% mean))
# View(df2)

df2 %>% ggplot() +
  geom_histogram(aes(x=sm, y=..density..))

# 2e) 

num_legitimate <- df1 %>% filter(Class == 0) %>% nrow()
num_legitimate

num_fraudulent <- df1 %>% filter(Class == 1) %>% nrow()
num_fraudulent



df2 %>% ggplot() +
  geom_histogram(aes(x=sm, y=..density..))+
  geom_function(color='red', fun=dnorm ,args =c(mean=lmean,sd=lstd/sqrt(491) ))



# 2f)

fmean <- mean(df1fra$Amount)

pnorm(fmean, mean = lmean, sd = lstd/sqrt(491), lower.tail = FALSE)


# 2g)

qnorm(0.00001, mean = lmean, sd = lstd/sqrt(491), lower.tail = FALSE)
