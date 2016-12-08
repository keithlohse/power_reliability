
set.seed(123) # Set the seed number for consistent, replicable simulations.

# Setting the size of our treatment effect
d<-0.5 # This is a Cohen's d (standardized effect size) that we will add 
# into our data to create the difference between groups

Tc<-rnorm(10000,0,1)
Te<-rnorm(10000,d,1)

## Reliability
# Below we are going to create two different X observations with different levels of 
# reliability (i.e., correlations with the true X).

# To establish reliability, we need to calculate the appropriate error variance
# based on classical test theory:
# X = T + e; where T is the True values as a random variable, e is measurement
# error as a random variable, and X is the resulting score. 
# It also follows that the variances are:
# var(X) = var(T)+var(e) as the variance of the sums is equal to the sum of the
# variances
# To simulate correlated variables, we know that we want r^2 amount of shared 
# variance between T and the different Xs. Thus, we need E variances that will
# yield the approriate correlation:
var5<-(1/0.5^2)-1 # var(X) - var(T) = 4 - 1 = 3
var6<-(1/0.6^2)-1
var7<-(1/0.7^2)-1
var8<-(1/0.8^2)-1
var9<-(1/0.9^2)-1

# and the error terms are thus:
e_5c<-rnorm(10000,0,sqrt(var5)) # for the control group
e_5e<-rnorm(10000,0,sqrt(var5)) # for the experiment group
e_6c<-rnorm(10000,0,sqrt(var6)) 
e_6e<-rnorm(10000,0,sqrt(var6))
e_7c<-rnorm(10000,0,sqrt(var7)) 
e_7e<-rnorm(10000,0,sqrt(var7))
e_8c<-rnorm(10000,0,sqrt(var8)) 
e_8e<-rnorm(10000,0,sqrt(var8))
e_9c<-rnorm(10000,0,sqrt(var9)) 
e_9e<-rnorm(10000,0,sqrt(var9))

# And from the true score and the measurement error, we create the observed
# values, X. 
X_5c<-Tc+e_5c
X_5e<-Te+e_5e
X_6c<-Tc+e_6c
X_6e<-Te+e_6e
X_7c<-Tc+e_7c
X_7e<-Te+e_7e
X_8c<-Tc+e_8c
X_8e<-Te+e_8e
X_9c<-Tc+e_9c
X_9e<-Te+e_9e

cor(Te,X_5e) # Should be r = 0.50
cor(Te,X_9e) # Should be r = 0.90

# We will also create a group variable for inclusion in the Population dataframe
POP<-data.frame(Tc, Te, X_5c, X_5e, X_6c, X_6e, X_7c, X_7e, X_8c, X_8e, X_9c, X_9e) 
head(POP)

## Simulated Samples
# Now we need to get ready to sample our population a bunch of times.
index<-c(1:10000) # set the number of samples that you want to take

DATA<-data.frame(index) #Create a dataframe to store the statisticcal tests 
# from our sample data

# Statistics to extract from the t-test function in the for-loop
m<-t.test(Tc, Te, paired=FALSE, var.equal=TRUE)
m
m$statistic
m$p.value
?sample()

# This for loop will randomly samples of size "n" for each of the correlated
for (i in 1:length(DATA$index)) {
    # For the first test, sample size is 10.
    n <- 300
    
    # n = 10, r = 1.0 (True Effects)
    ScT <- sample(POP$Tc, n)
    SeT <- sample(POP$Te, n)
    indT <- t.test(ScT,SeT,paired=FALSE,var.equal=TRUE) 
    
    # n = 10, r = 0.5
    Sc5 <- sample(POP$X_5c, n)
    Se5 <- sample(POP$X_5e, n)
    ind5 <- t.test(Sc5,Se5,paired=FALSE,var.equal=TRUE) 
    
    # n = 10, r = 0.6
    Sc6 <- sample(POP$X_6c, n)
    Se6 <- sample(POP$X_6e, n)
    ind6 <- t.test(Sc6,Se6,paired=FALSE,var.equal=TRUE) 
    
    # n = 10, r = 0.7
    Sc7 <- sample(POP$X_7c, n)
    Se7 <- sample(POP$X_7e, n)
    ind7 <- t.test(Sc7,Se7,paired=FALSE,var.equal=TRUE) 
    
    # n = 10, r = 0.8
    Sc8 <- sample(POP$X_8c, n)
    Se8 <- sample(POP$X_8e, n)
    ind8 <- t.test(Sc8,Se8,paired=FALSE,var.equal=TRUE) 
    
    # n = 10, r = 0.9
    Sc9 <- sample(POP$X_9c, n)
    Se9 <- sample(POP$X_9e, n)
    ind9 <- t.test(Sc9,Se9,paired=FALSE,var.equal=TRUE) 
    
    # Inferential Statistics
    DATA$size[i]<-n
    
    DATA$t_ind[i]<-indT$statistic # Saves the t-value
    DATA$p_ind[i]<-indT$p.value # Saves the p-value
    DATA$t5_ind[i]<-ind5$statistic # Saves the t-value
    DATA$p5_ind[i]<-ind5$p.value # Saves the p-value
    DATA$t6_ind[i]<-ind6$statistic 
    DATA$p6_ind[i]<-ind6$p.value 
    DATA$t7_ind[i]<-ind7$statistic 
    DATA$p7_ind[i]<-ind7$p.value 
    DATA$t8_ind[i]<-ind8$statistic 
    DATA$p8_ind[i]<-ind8$p.value
    DATA$t9_ind[i]<-ind9$statistic 
    DATA$p9_ind[i]<-ind9$p.value     
    
}

write.csv(DATA, file="d05_n300.csv")


