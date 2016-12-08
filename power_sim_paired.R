
set.seed(123) # Set the seed number for consistent, replicable simulations.

# Setting the size of our treatment effect
d<-0.80 # This is a Cohen's d (standardized effect size) 
# for the change from pre-test to post-test.
r<-1.0 # This the "true" correlation between the pre- and post-tests
# If set to 1.0 then the difference in true scores will have zero error.
# Alternatively, we can set this to 0.9999 assuming essentially perfect 
# measurement (e.g., no error from time one to time two) to analyze true scores.

Tpre<-rnorm(10000,0,1)
Tpost<-d+r*(Tpre)+rnorm(10000,0,sqrt((1-r^2))) 
cor(Tpre,Tpost)

## Reliability -----------------------------------------------------------------
# Below we are going to create two different X observations with different levels 
# of reliability (i.e., correlations with the true X).
# To establish reliability, we need to calculate the appropriate error variance
# based on classical test theory:
# X = T + e; where T is the True values as a random variable, e is measurement
# error as a random variable, and X is the resulting random variable/ 
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

# We can then use the variances to create "noisey" pre nad post test scores.
Xpre_5<-Tpre+rnorm(10000,0,sqrt(var5)) 
Xpost_5<-Tpost+rnorm(10000,0,sqrt(var5)) 
Xpre_6<-Tpre+rnorm(10000,0,sqrt(var6)) 
Xpost_6<-Tpost+rnorm(10000,0,sqrt(var6))
Xpre_7<-Tpre+rnorm(10000,0,sqrt(var7)) 
Xpost_7<-Tpost+rnorm(10000,0,sqrt(var7))
Xpre_8<-Tpre+rnorm(10000,0,sqrt(var8)) 
Xpost_8<-Tpost+rnorm(10000,0,sqrt(var8))
Xpre_9<-Tpre+rnorm(10000,0,sqrt(var9)) 
Xpost_9<-Tpost+rnorm(10000,0,sqrt(var9)) 


# The standardized effect size, Cohen's d, is whatever we defined it to be above
# Many power calculators, however, use dz, which is the mean difference divided
# by the standard deviation of the differences (rather than being divided by the
# pooled standard deviation).
# Cohen's d
d
# Cohen's dz
(mean(Tpost) - mean(Tpre))/sd(Tpost-Tpre)
(mean(Xpost_9) - mean(Xpre_9))/sd(Xpost_9-Xpre_9)
(mean(Xpost_8) - mean(Xpre_8))/sd(Xpost_8-Xpre_8)
(mean(Xpost_7) - mean(Xpre_7))/sd(Xpost_7-Xpre_7)
(mean(Xpost_6) - mean(Xpre_6))/sd(Xpost_6-Xpre_6)
(mean(Xpost_5) - mean(Xpre_5))/sd(Xpost_5-Xpre_5)


# We will combine all of these variables together into a dataframe
POP<-data.frame(Tpre, Tpost, Xpre_5, Xpost_5, Xpre_6, Xpost_6, Xpre_7, Xpost_7, 
                Xpre_8, Xpost_8, Xpre_9, Xpost_9) 
head(POP)
# Correlation between true scores and less reliable scores
cor(POP$Tpre,POP$Xpre_9) 
cor(POP$Tpre,POP$Xpre_8)
cor(POP$Tpre,POP$Xpre_7)
cor(POP$Tpre,POP$Xpre_6)
cor(POP$Tpre,POP$Xpre_5) 

# Correlation between pre-test scores for different reliabilities
cor(POP$Tpre,POP$Tpost)
cor(POP$Xpre_9,POP$Xpost_9)
cor(POP$Xpre_8,POP$Xpost_8)
cor(POP$Xpre_7,POP$Xpost_7)
cor(POP$Xpre_6,POP$Xpost_6)
cor(POP$Xpre_5,POP$Xpost_5)


## Simulated Samples
# Now we need to get ready to sample our population a bunch of times.
index<-c(1:10000) # set the number of samples that you want to take

DATA<-data.frame(index) #Create a dataframe to store the statisticcal tests 
# from our sample data

# Statistics to extract from the t-test function in the for-loop
m<-t.test(Tpre, Tpost, paired=TRUE, var.equal=TRUE)
m
m$statistic
m$p.value

## We can also use the sample function to take random rows of paired observations
## This is example code to illustrate what is happening inside the for-loop
# SAMP<-POP[sample(nrow(POP),10),]
# SAMP
# t.test(SAMP$Xpre,SAMP$Xpost,paired=TRUE,var.equal=TRUE)
# t.test(SAMP$Xpre_5,SAMP$Xpost_5,paired=TRUE,var.equal=TRUE)
# 
# mean(SAMP$Xpre_5-SAMP$Xpost_5)
# sd(SAMP$Xpre_5-SAMP$Xpost_5)/sqrt(10)
# mean(SAMP$Xpre_5-SAMP$Xpost_5)/(sd(SAMP$Xpre_5-SAMP$Xpost_5)/sqrt(10))
# 
# mean(SAMP$Xpre-SAMP$Xpost)
# sd(SAMP$Xpre-SAMP$Xpost)/sqrt(10)
# mean(SAMP$Xpre-SAMP$Xpost)/(sd(SAMP$Xpre-SAMP$Xpost)/sqrt(10))

## Simulation ------------------------------------------------------------------
# This for loop will randomly samples of size "n" for each of the correlated
for (i in 1:length(DATA$index)) {
    # For the first test, sample size is 10.
    n <- 300
    
    SAMP<-POP[sample(nrow(POP),n),]

    # r = 1.0 (True Effects)
    pairT <- t.test(SAMP$Tpre,SAMP$Tpost,paired=TRUE,var.equal=TRUE)
     
    # r = 0.5
    pair5 <- t.test(SAMP$Xpre_5,SAMP$Xpost_5,paired=TRUE,var.equal=TRUE) 
    
    # r = 0.6
    pair6 <- t.test(SAMP$Xpre_6,SAMP$Xpost_6,paired=TRUE,var.equal=TRUE) 
    
    # r = 0.7
    pair7 <- t.test(SAMP$Xpre_7,SAMP$Xpost_7,paired=TRUE,var.equal=TRUE) 
    
    # r = 0.8
    pair8 <- t.test(SAMP$Xpre_8,SAMP$Xpost_8,paired=TRUE,var.equal=TRUE) 
    
    # r = 0.9
    pair9 <- t.test(SAMP$Xpre_9,SAMP$Xpost_9,paired=TRUE,var.equal=TRUE) 
    
    # Inferential Statistics
    DATA$size[i]<-n
    
    DATA$tT_pair[i]<-pairT$statistic # Saves the t-value
    DATA$pT_pair[i]<-pairT$p.value # Saves the p-value
    DATA$t9_pair[i]<-pair9$statistic 
    DATA$p9_pair[i]<-pair9$p.value  
    DATA$t8_pair[i]<-pair8$statistic 
    DATA$p8_pair[i]<-pair8$p.value
    DATA$t7_pair[i]<-pair7$statistic 
    DATA$p7_pair[i]<-pair7$p.value
    DATA$t6_pair[i]<-pair6$statistic 
    DATA$p6_pair[i]<-pair6$p.value
    DATA$t5_pair[i]<-pair5$statistic # Saves the t-value
    DATA$p5_pair[i]<-pair5$p.value # Saves the p-value
    
}

write.csv(DATA, file="paired_d08_n300.csv")
