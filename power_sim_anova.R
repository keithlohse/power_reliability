
set.seed(123) # Set the seed number for consistent, replicable simulations.

# Setting the size of our treatment effect
d<-0.50 # This is a Cohen's d (standardized effect size) 
# for the change from pre-test to post-test.
r<-1.0 # This the "true" correlation between the pre- and post-tests
# If set to 1.0 then the difference in true scores will have zero error.
# Alternatively, we can set this to 0.9999 assuming essentially perfect 
# measurement (e.g., no error from time one to time two) to analyze true scores.

T_ctrl_pre<-rnorm(10000,0,1)
T_exp_pre<-rnorm(10000,0,1)
Tpre<-c(T_ctrl_pre,T_exp_pre)

T_ctrl_post<-r*(T_ctrl_pre)+rnorm(10000,0,sqrt((1-r^2))) 
T_exp_post<-d+r*(T_exp_pre)+rnorm(10000,0,sqrt((1-r^2))) 
Tpost<-c(T_ctrl_post,T_exp_post)

Group<-c(rep("ctrl",10000), rep("exp",10000))

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
Xpre_5<-Tpre+rnorm(20000,0,sqrt(var5)) 
Xpost_5<-Tpost+rnorm(20000,0,sqrt(var5)) 
Xpre_6<-Tpre+rnorm(20000,0,sqrt(var6)) 
Xpost_6<-Tpost+rnorm(20000,0,sqrt(var6))
Xpre_7<-Tpre+rnorm(20000,0,sqrt(var7)) 
Xpost_7<-Tpost+rnorm(20000,0,sqrt(var7))
Xpre_8<-Tpre+rnorm(20000,0,sqrt(var8)) 
Xpost_8<-Tpost+rnorm(20000,0,sqrt(var8))
Xpre_9<-Tpre+rnorm(20000,0,sqrt(var9)) 
Xpost_9<-Tpost+rnorm(20000,0,sqrt(var9)) 


# We will combine all of these variables together into a dataframe
POP<-data.frame(Group, Tpre, Tpost, Xpre_5, Xpost_5, Xpre_6, Xpost_6, Xpre_7, Xpost_7, 
                Xpre_8, Xpost_8, Xpre_9, Xpost_9) 
head(POP)
# Correlation between true scores and less reliable scores
cor(POP$Tpre,POP$Xpre_9) 
cor(POP$Tpre,POP$Xpre_8)
cor(POP$Tpre,POP$Xpre_7)
cor(POP$Tpre,POP$Xpre_6)
cor(POP$Tpre,POP$Xpre_5) 

cor(POP$Xpre_5,POP$Xpost_5)
cor(POP$Xpre_6,POP$Xpost_6)
cor(POP$Xpre_7,POP$Xpost_7)
cor(POP$Xpre_8,POP$Xpost_8)
cor(POP$Xpre_9,POP$Xpost_9)



## Simulated Samples
# Now we need to get ready to sample our population a bunch of times.
index<-c(1:10000) # set the number of samples that you want to take

DATA<-data.frame(index) #Create a dataframe to store the statisticcal tests 
# from our sample data

## We can also use the sample function to take random rows of paired observations
## This is example code to illustrate what is happening inside the for-loop
CTRL<-subset(POP, Group=="ctrl")
EXP<-subset(POP, Group=="exp")

# C_SAMP<-CTRL[sample(nrow(CTRL), 20), ]
# E_SAMP<-EXP[sample(nrow(EXP), 20), ]
#  
# SAMP<-rbind(C_SAMP, E_SAMP)
# SAMP
# write.csv(SAMP, file="anova_example_SAMP_d05.csv")
# t.test((SAMP$Xpost_9-SAMP$Xpre_9)~SAMP$Group, var.equal=TRUE)
# t.test((SAMP$Xpost_8-SAMP$Xpre_8)~SAMP$Group, var.equal=TRUE)
# t.test((SAMP$Xpost_7-SAMP$Xpre_7)~SAMP$Group, var.equal=TRUE)
# t.test((SAMP$Xpost_6-SAMP$Xpre_6)~SAMP$Group, var.equal=TRUE)
# t.test((SAMP$Xpost_5-SAMP$Xpre_5)~SAMP$Group, var.equal=TRUE)
#  
# m<-t.test((SAMP$Xpost_9-SAMP$Xpre_9)~SAMP$Group, var.equal=TRUE)
# m
# m$statistic
# m$parameter
# m$p.value
# r_sq<-(m$statistic^2)/(m$statistic^2+m$parameter)
# r_sq

## Simulation ------------------------------------------------------------------
# This for loop will randomly samples of size "n" for each of the correlated
for (i in 1:length(DATA$index)) {
    # For the first test, sample size is 10.
    n <- 300
    
    C_SAMP<-CTRL[sample(nrow(CTRL), n), ]
    E_SAMP<-EXP[sample(nrow(EXP), n), ]
    
    SAMP<-rbind(C_SAMP, E_SAMP)

    # r = 1.0 (True Effects)
    intT <- t.test((SAMP$Tpost-SAMP$Tpre)~SAMP$Group,var.equal=TRUE)
    # r = 0.9
    int9 <- t.test((SAMP$Xpost_9-SAMP$Xpre_9)~SAMP$Group,var.equal=TRUE)
    # r = 0.8
    int8 <- t.test((SAMP$Xpost_8-SAMP$Xpre_8)~SAMP$Group,var.equal=TRUE)
    # r = 0.7
    int7 <- t.test((SAMP$Xpost_7-SAMP$Xpre_7)~SAMP$Group,var.equal=TRUE)
    # r = 0.6
    int6 <- t.test((SAMP$Xpost_6-SAMP$Xpre_6)~SAMP$Group,var.equal=TRUE)
    # r = 0.5
    int5 <- t.test((SAMP$Xpost_5-SAMP$Xpre_5)~SAMP$Group,var.equal=TRUE)
     
    # Inferential Statistics
    DATA$size[i]<-n
    
    DATA$intT_t[i]<-intT$statistic # Saves the t-value
    DATA$intT_p[i]<-intT$p.value # Saves the p-value
    DATA$intT_r2[i]<-(intT$statistic^2)/(intT$statistic^2+(n-2)) # computes the r-squared value
    DATA$int9_t[i]<-int9$statistic
    DATA$int9_p[i]<-int9$p.value
    DATA$int9_r2[i]<-(int9$statistic^2)/(int9$statistic^2+(n-2))
    DATA$int8_t[i]<-int8$statistic
    DATA$int8_p[i]<-int8$p.value
    DATA$int8_r2[i]<-(int8$statistic^2)/(int8$statistic^2+(n-2))
    DATA$int7_t[i]<-int7$statistic
    DATA$int7_p[i]<-int7$p.value
    DATA$int7_r2[i]<-(int7$statistic^2)/(int7$statistic^2+(n-2))
    DATA$int6_t[i]<-int6$statistic
    DATA$int6_p[i]<-int6$p.value
    DATA$int6_r2[i]<-(int6$statistic^2)/(int6$statistic^2+(n-2))
    DATA$int5_t[i]<-int5$statistic
    DATA$int5_p[i]<-int5$p.value
    DATA$int5_r2[i]<-(int5$statistic^2)/(int5$statistic^2+(n-2))
    
}

write.csv(DATA, file="anova_d05_n300.csv")
