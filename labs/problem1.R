rm(list=ls()) # clear all prev stored data
n=600 # n throws of the die
set.seed(1) # fix random generator seed (same random simulator each time)
throw=sample(1:6, n, replace=TRUE) # simulate n throws of the die
for(i in 1:6){ # die head i = 1,2,..,6
str1<-sprintf("frequency of %d : %d, total throws = %d",i,sum(throw==i),n)
print(str1)
} # number of n throws, how many of the same die head
sample_mean = mean(throw) # sample mean
str2<-sprintf("sample mean = %f",sample_mean)
print(str2)
sample_standard_deviation = sd(throw) # sample standard deviation
str3<-sprintf("sample std deviation = %f",sample_standard_deviation)
print(str3)
