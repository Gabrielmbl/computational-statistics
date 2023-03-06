library(tidyverse)

#1) A fair 6-sided die is rolled repeatedly until it gives 3. Let X count the number of rolls it takes
# to get a 3.

# (a) Write a function in R that performs this experiment and returns the corresponding value for
# X. (Hint: sample(1:6,1) replicates a dice roll.)

dieroll <- function() {
  roll <- 10 # Out of bounds arbitrary number that will be changed to 1-6 later in the loop
  X <- 0 # count
  while(roll != 3){
    roll <- sample(1:6, 1) # roll it
    # cat(sprintf('Roll is: %s \n', roll))
    X <- X + 1
  }
  return(X)
}

# dieroll()


# (b) Repeat this experiment 10,000 times. Store your results in a tibble. Display your results using
# a bar chart.
results <- tibble(index = 0:10000) %>% rowwise() %>% 
  mutate(X = dieroll())

# View(results)

# ANSWER 
results %>% ggplot() +
  geom_histogram(aes(x=X),binwidth=1)


# (c) Notice P(X = 1) = 1/6
# and P(X = 2) = 5/6^2
# Find a general formula for P(X = k) (i.e.
# find the probability mass function for X). Then display these probabilities in a bar chart for
# X = 1 . . . 40.

probs <- tibble(X = 1:40) %>% rowwise() %>% 
  mutate(prob = ((5^(X-1))/(6^X)))

# View(probs)

probs %>% ggplot() + geom_col(aes(x = X, y = prob))







# (d) Modify the plot in (b) to display the proportion of times each value of X occurs. Then plot
# this alongside the bar chart from (c) in a “dodge” format.

results1 <- results %>% group_by(X) %>% 
  summarize(prop = n()/10000)

results2 <- results1 %>% 
  left_join(probs)

# View(results1)
# View(results2)

propprob <- results2 %>% pivot_longer(cols=c(prob,prop),names_to='type',values_to='value')

# View(propprob)

propprob %>% ggplot() +
  geom_col(aes(x= X, y= value, fill= type), position = 'dodge')



#e) Technically, X can take on any positive integer. Let’s assume the largest X can be is 100.
# Compute E(X) under this assumption.


X <- 1:100
E <- sum(((5^(X-1))/(6^X))*X)
E

# f) Compute the sample mean from the data generated in part (b)

# View(results)

mean(results$X)



# (g) Compute Var(X) under the assumptions in (e).
# Var(X) = E((X − µ)^2)
# E <- sum((5^(X-1))/(6^X)*X)

X <- 1:100

probs %>% mutate(var_term = (X-6)^2*prob) %>% pull %>% sum




# (h) Compute the sample variance from the data generated in part (b).

var(results$X)




# 2a) Write a function longest run that receives as input a vector of integers that are either 0 or 1
# and returns the length of the longest run of 1’s in the vector. Test your function against the
# following to ensure it performs as expected

longest_run <- function(vector) {
  run <- 0
  longest <- 0
  for (val in vector){
    if (val == 1){
      run <- run + 1
      #print(run)
    }
    else{ # when val is 0
      if (run > longest) {
        longest <- run
      }
      run <- 0 # restart run when val is 0
    }
  }
  if(run > longest){ # for cases like if the last number is not a 0 
    longest <- run
  }
  return(longest)
}


longest_run(c(0 ,1 ,1 ,1 ,0 ,1) ) # Should give 3
longest_run(c(1 ,0 ,1 ,0 ,0 ,1) ) # Should give 1
longest_run(c(1 ,1 ,1 ,0 ,1 ,1) ) # Should give 3
longest_run(c(1 ,1 ,1 ,1 ,1 ,1) ) # Should give 6
longest_run(c(0 ,0 ,0 ,0 ,0 ,0) ) # Should give 0


# 2b) The following code creates a tibble with 10,000 rows. Each row will contain a vector of 0’s
# and 1’s of length 15 under the coin flips column. The probability of success (i.e. “heads”)
# is θ = 0.5.

library(Rlab)
df_random <- tibble(index = 1:10000) %>% rowwise() %>%
  mutate(coin_flips = lst(rbern(20,0.6)))

# View(df_random)

# Let X count the number of 1’s in each vector. Let Y count the longest run of 1’s in each
# vector. Create columns in this tibble for X and Y .

df_random1 <- df_random %>% rowwise() %>% 
  mutate(X = sum(coin_flips)) %>% 
  mutate(Y = longest_run(coin_flips))

# View(df_random1)

#2c) Give a bar graph that counts the number of times each Y occurs.

# df_random2 <- df_random1 %>% group_by(Y) %>% 
#   summarize(count= n())
# View(df_random2)

df_random1 %>% group_by(Y) %>% 
  summarize(count= n()) %>% 
  ggplot() +
  geom_col(aes(x = Y, y=count))


# 2d) Use geom tile to display the number of times each pair of values for X and Y occurs in the
# tibble.
library(ggplot2)

df_random2 <- df_random1 %>% group_by(X,Y) %>% 
  summarize(count=n())

#View(df_random2)

df_random2 %>% ggplot() +
  geom_tile(aes(x=X,y=Y, filled.contour()=count))


# 2e) Consider a data set where each row contains a measurement of X and a measurement of Y .
# When considering the i-th row, we denote the measurement of X by xi and the measurement
# of Y by yi
# . Suppose there are n rows. The sample covariance for these measurements is ...
# where x-hat is the sample mean of the xi and y-hat is the sample mean of the yi.
# Compute the sample covariance for our tibble.

mx <- mean(df_random1$X)
my <- mean(df_random1$Y)

df_random_cov <- df_random1 %>% mutate(Xi_Yi = (X-mx)*(Y-my))
# View(df_random_cov)
cov_XY <- sum(df_random_cov$Xi_Yi)/9999
cov_XY


cov(df_random1$X, df_random1$Y)



# 2f) Compute the Pearson correlation coefficient for our tibble.

View(df_random_cov)

df_random_sxsy <- df_random_cov %>% mutate(Xi_mx = (X - mx)^2) %>% 
  mutate(Yi_my = (Y-my)^2)

View(df_random_sxsy)

s_X <- sqrt(sum(df_random_sxsy$Xi_mx)/9999) 
s_X

s_Y <- sqrt(sum(df_random_sxsy$Yi_my)/9999) 
s_Y

r_xy <- cov_XY/(s_X*s_Y)
r_xy # By hand

cor(df_random1$X, df_random1$Y) # R function



# 2g) The following code snippet lists every possible outcome in this experiment.
df_prob <- tibble(ind =1:2^15) %>% rowwise() %>%
  mutate(coin_flips = lst(as.integer(intToBits(ind))[1:15]))

# View(df_prob)

# Create columns for the random variables X and Y defined in part (b).

df_prob1 <- df_prob %>% rowwise() %>% 
  mutate(X=sum(coin_flips)) %>% 
  mutate(Y=longest_run(coin_flips))

# View(df_prob1)


# 2h) Use group by and summarize to give counts for the number of times each pair of values for X
# and Y occur in df prob. Then add a column prob to this tibble that gives the probability for
# each pair of values for X and Y (recall that each outcome listed in df prob has a probability
# of 1/2^15 ). I highly recommend running ungroup after summarize since failing to do this will
# cause error in the subsequent calculations

# 2^15 == nrow(df_prob1)

df_prob2 <- df_prob1 %>% group_by(X,Y) %>% 
  summarize(count=n()) %>% 
  mutate(prob = count/(2^15)) %>% 
  ungroup()

# View(df_prob2)



# 2i) Compute E(X) and E(Y).

E_X <- sum(df_prob2$X*df_prob2$prob)
E_X

E_Y <- sum(df_prob2$Y*df_prob2$prob)
E_Y

# 2j) The covariance of random variables X and Y is
# Cov(X,Y) = E((X − E(X))(Y − E(Y)))
# Compute this value using the tibble in part (h). Your answer should be similar to what you
# computed in part (e).

df_prob3 <- df_prob2 %>% 
  mutate(XY = (X-E_X)*(Y-E_Y)) %>%  # (X − E(X))(Y − E(Y)) 
  mutate(XY_prob = XY*prob) # Expected value is sum of all val*prob

# View(df_prob3)

cov_XY <- sum(df_prob3$XY_prob)
cov_XY



# 2k) Compute σX = SD(X) and σY = SD(Y).
# SD(X) = sqrt(E((X - mu)^2))

df_prob4 <- df_prob3 %>% 
  mutate(X_mu_2 = (X - E_X)^2) %>% 
  mutate(X_mu_2_prob = X_mu_2*prob) %>% 
  mutate(Y_mu_2 = (Y - E_Y)^2) %>% 
  mutate(Y_mu_2_prob = Y_mu_2*prob)

# View(df_prob4)

E_X_mu_2_prob <- sum(df_prob4$X_mu_2_prob) 
E_X_mu_2_prob

sd_X <- sqrt(E_X_mu_2_prob)
sd_X

# sd_X2 <- sd(df_prob3$X)????
# sd_X2 ????

E_Y_mu_2_prob <- sum(df_prob4$Y_mu_2_prob) 
E_Y_mu_2_prob

sd_Y <- sqrt(E_Y_mu_2_prob)
sd_Y

# sd_Y2 <- sd(df_prob3$Y) ???
# sd_Y2 ???



# 2l)  The Pearson correlation coefficient for random variables X and Y is ...
# Compute this value using the tibble in part (h). Your answer should be similar to what you
# computed in part (e).

p <- cov_XY / (sd_X * sd_Y)
p
