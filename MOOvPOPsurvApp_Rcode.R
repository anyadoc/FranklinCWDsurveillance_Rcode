# Belsare A, Gompper M, Keller B, Sumners J, Hansen L, Millspaugh J.
# An agent-based framework for improving wildlife disease surveillance:
# A case study of chronic wasting disease in Missouri white-tailed deer.
# Ecol Modell. 2020;417. doi:http://dx.doi.org/10.1101/478610

# (Co-submission) Belsare A, Gompper M, Keller B, Sumners J, Hansen L, Millspaugh J.
# Size Matters: Sample size assessments for chronic wasting disease
# surveillance using an agent-based modeling framework. MethodsX (in review)

library(ggplot2)
library(tidyr)
library(reshape2)
library(gridExtra)
library(grid)
library(ggpubr)
library(plotly)
library(magick)
library(dplyr)
library(ggrepel)
library(matrixStats)
require(tibble)

# model application scenario baseline 1
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl1.csv"
data_ma_bl1 <- read_csv(data_github)  
as_tibble(data_ma_bl1)
mabl1_dp <- add_column(data_ma_bl1, scenario = rep("baseline", 1000))
mabl1dp <- mabl1_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested)  #dplyr
mabl1dp <- add_column(mabl1dp, samplesize = mabl1dp$AdultMaleTested + mabl1dp$AdultFemaleTested)
mabl1dp <- add_column(mabl1dp, mrun = 1:1000)
mabl1dp$mrun <- ifelse(mabl1dp$mrun %% 100 == 0, 1, 0)
mabl1dp <- subset(mabl1dp, mrun == 1)
meanmabl1dp <- by(data_ma_bl1$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl1), 100, nrow(data_ma_bl1)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
mabl1dp <- add_column(mabl1dp, meandp = meanmabl1dp)
mabl1dp_final <- mabl1dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl1dp_final$samplesize <- "10% harvest \n ~300 sample size"
mabl1dp_final$TotalCWD <- "0.2% CWD prevalence"

# model application scenario baseline 2
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl2.csv"
data_ma_bl2 <- read_csv(data_github)  
as_tibble(data_ma_bl2)
mabl2_dp <- add_column(data_ma_bl2, scenario = rep("baseline", 1000))
mabl2dp <- mabl2_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl2dp <- add_column(mabl2dp, samplesize = mabl2dp$AdultMaleTested + mabl2dp$AdultFemaleTested)
mabl2dp <- add_column(mabl2dp, mrun = 1:1000)
mabl2dp$mrun <- ifelse(mabl2dp$mrun %% 100 == 0, 1, 0)
mabl2dp <- subset(mabl2dp, mrun == 1)
meanmabl2dp <- by(data_ma_bl2$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl2), 100, nrow(data_ma_bl2)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
mabl2dp <- add_column(mabl2dp, meandp = meanmabl2dp)
mabl2dp_final <- mabl2dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl2dp_final$samplesize <- "20% harvest \n ~600 sample size"
mabl2dp_final$TotalCWD <- "0.2% CWD prevalence"

# model application scenario baseline 3
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl3.csv"
data_ma_bl3 <- read_csv(data_github)  
as_tibble(data_ma_bl3)
mabl3_dp <- add_column(data_ma_bl3, scenario = rep("baseline", 1000))
mabl3dp <- mabl3_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl3dp <- add_column(mabl3dp, samplesize = mabl3dp$AdultMaleTested + mabl3dp$AdultFemaleTested)
mabl3dp <- add_column(mabl3dp, mrun = 1:1000)
mabl3dp$mrun <- ifelse(mabl3dp$mrun %% 100 == 0, 1, 0)
mabl3dp <- subset(mabl3dp, mrun == 1)
meanmabl3dp <- by(data_ma_bl3$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl3), 100, nrow(data_ma_bl3)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
mabl3dp <- add_column(mabl3dp, meandp = meanmabl3dp)
mabl3dp_final <- mabl3dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl3dp_final$samplesize <- "30% harvest \n ~900 sample size"
mabl3dp_final$TotalCWD <- "0.2% CWD prevalence"

# model application scenario baseline 4
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl4.csv"
data_ma_bl4 <- read_csv(data_github) 
as_tibble(data_ma_bl4)
mabl4_dp <- add_column(data_ma_bl4, scenario = rep("baseline", 1000))
mabl4dp <- mabl4_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl4dp <- add_column(mabl4dp, samplesize = mabl4dp$AdultMaleTested + mabl4dp$AdultFemaleTested)
mabl4dp <- add_column(mabl4dp, mrun = 1:1000)
mabl4dp$mrun <- ifelse(mabl4dp$mrun %% 100 == 0, 1, 0)
mabl4dp <- subset(mabl4dp, mrun == 1)
meanmabl4dp <- by(data_ma_bl4$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl4), 100, nrow(data_ma_bl4)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
mabl4dp <- add_column(mabl4dp, meandp = meanmabl4dp)
mabl4dp_final <- mabl4dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl4dp_final$samplesize <- "40% harvest \n ~1200 sample size"
mabl4dp_final$TotalCWD <- "0.2% CWD prevalence"

# model application scenario baseline 5
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl5.csv"
data_ma_bl5 <- read_csv(data_github)  
as_tibble(data_ma_bl5)
mabl5_dp <- add_column(data_ma_bl5, scenario = rep("baseline", 1000))
mabl5dp <- mabl5_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl5dp <- add_column(mabl5dp, samplesize = mabl5dp$AdultMaleTested + mabl5dp$AdultFemaleTested)
mabl5dp <- add_column(mabl5dp, mrun = 1:1000)
mabl5dp$mrun <- ifelse(mabl5dp$mrun %% 100 == 0, 1, 0)
mabl5dp <- subset(mabl5dp, mrun == 1)
meanmabl5dp <- by(data_ma_bl5$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl5), 100, nrow(data_ma_bl5)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
mabl5dp <- add_column(mabl5dp, meandp = meanmabl5dp)
mabl5dp_final <- mabl5dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl5dp_final$samplesize <- "50% harvest \n ~1500 sample size"
mabl5dp_final$TotalCWD <- "0.2% CWD prevalence"

# model application scenario baseline 6
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl6.csv"
data_ma_bl6 <- read_csv(data_github)  
as_tibble(data_ma_bl6)
mabl6_dp <- add_column(data_ma_bl6, scenario = rep("baseline", 1000))
mabl6dp <- mabl6_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl6dp <- add_column(mabl6dp, samplesize = mabl6dp$AdultMaleTested + mabl6dp$AdultFemaleTested)
mabl6dp <- add_column(mabl6dp, mrun = 1:1000)
mabl6dp$mrun <- ifelse(mabl6dp$mrun %% 100 == 0, 1, 0)
mabl6dp <- subset(mabl6dp, mrun == 1)
meanmabl6dp <- by(data_ma_bl6$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl6), 100, nrow(data_ma_bl6)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
mabl6dp <- add_column(mabl6dp, meandp = meanmabl6dp)
mabl6dp_final <- mabl6dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl6dp_final$samplesize <- "10% harvest \n ~300 sample size"
mabl6dp_final$TotalCWD <- "0.4% CWD prevalence"

# model application scenario baseline 7
data_github <-"https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl7.csv"
data_ma_bl7 <- read_csv(data_github)  
as_tibble(data_ma_bl7)
mabl7_dp <- add_column(data_ma_bl7, scenario = rep("baseline", 1000))
mabl7dp <- mabl7_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl7dp <- add_column(mabl7dp, samplesize = mabl7dp$AdultMaleTested + mabl7dp$AdultFemaleTested)
mabl7dp <- add_column(mabl7dp, mrun = 1:1000)
mabl7dp$mrun <- ifelse(mabl7dp$mrun %% 100 == 0, 1, 0)
mabl7dp <- subset(mabl7dp, mrun == 1)
meanmabl7dp <- by(data_ma_bl7$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl7), 100, nrow(data_ma_bl7)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
mabl7dp <- add_column(mabl7dp, meandp = meanmabl7dp)
mabl7dp_final <- mabl7dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl7dp_final$samplesize <- "20% harvest \n ~600 sample size"
mabl7dp_final$TotalCWD <- "0.4% CWD prevalence"

# model application scenario baseline 8
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl8.csv"
data_ma_bl8 <- read_csv(data_github)  
as_tibble(data_ma_bl8)
mabl8_dp <- add_column(data_ma_bl8, scenario = rep("baseline", 1000))
mabl8dp <- mabl8_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl8dp <- add_column(mabl8dp, samplesize = mabl8dp$AdultMaleTested + mabl8dp$AdultFemaleTested)
mabl8dp <- add_column(mabl8dp, mrun = 1:1000)
mabl8dp$mrun <- ifelse(mabl8dp$mrun %% 100 == 0, 1, 0)
mabl8dp <- subset(mabl8dp, mrun == 1)
meanmabl8dp <- by(data_ma_bl8$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl8), 100, nrow(data_ma_bl8)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
mabl8dp <- add_column(mabl8dp, meandp = meanmabl8dp)
mabl8dp_final <- mabl8dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl8dp_final$samplesize <- "30% harvest \n ~900 sample size"
mabl8dp_final$TotalCWD <- "0.4% CWD prevalence"

# model application scenario baseline 9
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl9.csv"
data_ma_bl9 <- read_csv(data_github)  
as_tibble(data_ma_bl9)
mabl9_dp <- add_column(data_ma_bl9, scenario = rep("baseline", 1000))
mabl9dp <- mabl9_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl9dp <- add_column(mabl9dp, samplesize = mabl9dp$AdultMaleTested + mabl9dp$AdultFemaleTested)
mabl9dp <- add_column(mabl9dp, mrun = 1:1000)
mabl9dp$mrun <- ifelse(mabl9dp$mrun %% 100 == 0, 1, 0)
mabl9dp <- subset(mabl9dp, mrun == 1)
meanmabl9dp <- by(data_ma_bl9$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl9), 100, nrow(data_ma_bl9)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
mabl9dp <- add_column(mabl9dp, meandp = meanmabl9dp)
mabl9dp_final <- mabl9dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl9dp_final$samplesize <- "40% harvest \n ~1200 sample size"
mabl9dp_final$TotalCWD <- "0.4% CWD prevalence"

# model application scenario baseline 10
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl10.csv"
data_ma_bl10 <- read_csv(data_github)  
as_tibble(data_ma_bl10)
mabl10_dp <- add_column(data_ma_bl10, scenario = rep("baseline", 1000))
mabl10dp <- mabl10_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl10dp <- add_column(mabl10dp, samplesize = mabl10dp$AdultMaleTested + mabl10dp$AdultFemaleTested)
mabl10dp <- add_column(mabl10dp, mrun = 1:1000)
mabl10dp$mrun <- ifelse(mabl10dp$mrun %% 100 == 0, 1, 0)
mabl10dp <- subset(mabl10dp, mrun == 1)
meanmabl10dp <- by(data_ma_bl10$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl10), 100, nrow(data_ma_bl10)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl10dp <- add_column(mabl10dp, meandp = meanmabl10dp)
mabl10dp_final <- mabl10dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl10dp_final$samplesize <- "50% harvest \n ~1500 sample size"
mabl10dp_final$TotalCWD <- "0.4% CWD prevalence"

# model application scenario baseline 11
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl11.csv"
data_ma_bl11 <- read_csv(data_github)  
as_tibble(data_ma_bl11)
mabl11_dp <- add_column(data_ma_bl11, scenario = rep("baseline", 1000))
mabl11dp <- mabl11_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl11dp <- add_column(mabl11dp, samplesize = mabl11dp$AdultMaleTested + mabl11dp$AdultFemaleTested)
mabl11dp <- add_column(mabl11dp, mrun = 1:1000)
mabl11dp$mrun <- ifelse(mabl11dp$mrun %% 100 == 0, 1, 0)
mabl11dp <- subset(mabl11dp, mrun == 1)
meanmabl11dp <- by(data_ma_bl11$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl11), 100, nrow(data_ma_bl11)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl11dp <- add_column(mabl11dp, meandp = meanmabl11dp)
mabl11dp_final <- mabl11dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl11dp_final$samplesize <- "10% harvest \n ~300 sample size"
mabl11dp_final$TotalCWD <- "0.6% CWD prevalence"

# model application scenario baseline 12
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl12.csv"
data_ma_bl12 <- read_csv(data_github)  
as_tibble(data_ma_bl12)
mabl12_dp <- add_column(data_ma_bl12, scenario = rep("baseline", 1000))
mabl12dp <- mabl12_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl12dp <- add_column(mabl12dp, samplesize = mabl12dp$AdultMaleTested + mabl12dp$AdultFemaleTested)
mabl12dp <- add_column(mabl12dp, mrun = 1:1000)
mabl12dp$mrun <- ifelse(mabl12dp$mrun %% 100 == 0, 1, 0)
mabl12dp <- subset(mabl12dp, mrun == 1)
meanmabl12dp <- by(data_ma_bl12$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl12), 100, nrow(data_ma_bl12)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl12dp <- add_column(mabl12dp, meandp = meanmabl12dp)
mabl12dp_final <- mabl12dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl12dp_final$samplesize <- "20% harvest \n ~600 sample size"
mabl12dp_final$TotalCWD <- "0.6% CWD prevalence"

# model application scenario baseline 13
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl13.csv"
data_ma_bl13 <- read_csv(data_github)  
as_tibble(data_ma_bl13)
mabl13_dp <- add_column(data_ma_bl13, scenario = rep("baseline", 1000))
mabl13dp <- mabl13_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl13dp <- add_column(mabl13dp, samplesize = mabl13dp$AdultMaleTested + mabl13dp$AdultFemaleTested)
mabl13dp <- add_column(mabl13dp, mrun = 1:1000)
mabl13dp$mrun <- ifelse(mabl13dp$mrun %% 100 == 0, 1, 0)
mabl13dp <- subset(mabl13dp, mrun == 1)
meanmabl13dp <- by(data_ma_bl13$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl13), 100, nrow(data_ma_bl13)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl13dp <- add_column(mabl13dp, meandp = meanmabl13dp)
mabl13dp_final <- mabl13dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl13dp_final$samplesize <- "30% harvest \n ~900 sample size"
mabl13dp_final$TotalCWD <- "0.6% CWD prevalence"

# model application scenario baseline 14
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl14.csv"
data_ma_bl14 <- read_csv(data_github)  
as_tibble(data_ma_bl14)
mabl14_dp <- add_column(data_ma_bl14, scenario = rep("baseline", 1000))
mabl14dp <- mabl14_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl14dp <- add_column(mabl14dp, samplesize = mabl14dp$AdultMaleTested + mabl14dp$AdultFemaleTested)
mabl14dp <- add_column(mabl14dp, mrun = 1:1000)
mabl14dp$mrun <- ifelse(mabl14dp$mrun %% 100 == 0, 1, 0)
mabl14dp <- subset(mabl14dp, mrun == 1)
meanmabl14dp <- by(data_ma_bl14$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl14), 100, nrow(data_ma_bl14)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl14dp <- add_column(mabl14dp, meandp = meanmabl14dp)
mabl14dp_final <- mabl14dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl14dp_final$samplesize <- "40% harvest \n ~1200 sample size"
mabl14dp_final$TotalCWD <- "0.6% CWD prevalence"

# model application scenario baseline 15
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl15.csv"
data_ma_bl15 <- read_csv(data_github)  
as_tibble(data_ma_bl15)
mabl15_dp <- add_column(data_ma_bl15, scenario = rep("baseline", 1000))
mabl15dp <- mabl15_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl15dp <- add_column(mabl15dp, samplesize = mabl15dp$AdultMaleTested + mabl15dp$AdultFemaleTested)
mabl15dp <- add_column(mabl15dp, mrun = 1:1000)
mabl15dp$mrun <- ifelse(mabl15dp$mrun %% 100 == 0, 1, 0)
mabl15dp <- subset(mabl15dp, mrun == 1)
meanmabl15dp <- by(data_ma_bl15$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl15), 100, nrow(data_ma_bl15)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl15dp <- add_column(mabl15dp, meandp = meanmabl15dp)
mabl15dp_final <- mabl15dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl15dp_final$samplesize <- "50% harvest \n ~1500 sample size"
mabl15dp_final$TotalCWD <- "0.6% CWD prevalence"

# model application scenario baseline 16
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl16.csv"
data_ma_bl16 <- read_csv(data_github)  
as_tibble(data_ma_bl16)
mabl16_dp <- add_column(data_ma_bl16, scenario = rep("baseline", 1000))
mabl16dp <- mabl16_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl16dp <- add_column(mabl16dp, samplesize = mabl16dp$AdultMaleTested + mabl16dp$AdultFemaleTested)
mabl16dp <- add_column(mabl16dp, mrun = 1:1000)
mabl16dp$mrun <- ifelse(mabl16dp$mrun %% 100 == 0, 1, 0)
mabl16dp <- subset(mabl16dp, mrun == 1)
meanmabl16dp <- by(data_ma_bl16$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl16), 100, nrow(data_ma_bl16)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl16dp <- add_column(mabl16dp, meandp = meanmabl16dp)
mabl16dp_final <- mabl16dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl16dp_final$samplesize <- "10% harvest \n ~300 sample size"
mabl16dp_final$TotalCWD <- "0.8% CWD prevalence"

# model application scenario baseline 17
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl17.csv"
data_ma_bl17 <- read_csv(data_github)  
as_tibble(data_ma_bl17)
mabl17_dp <- add_column(data_ma_bl17, scenario = rep("baseline", 1000))
mabl17dp <- mabl17_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl17dp <- add_column(mabl17dp, samplesize = mabl17dp$AdultMaleTested + mabl17dp$AdultFemaleTested)
mabl17dp <- add_column(mabl17dp, mrun = 1:1000)
mabl17dp$mrun <- ifelse(mabl17dp$mrun %% 100 == 0, 1, 0)
mabl17dp <- subset(mabl17dp, mrun == 1)
meanmabl17dp <- by(data_ma_bl17$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl17), 100, nrow(data_ma_bl17)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl17dp <- add_column(mabl17dp, meandp = meanmabl17dp)
mabl17dp_final <- mabl17dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl17dp_final$samplesize <- "20% harvest \n ~600 sample size"
mabl17dp_final$TotalCWD <- "0.8% CWD prevalence"

# model application scenario baseline 18
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl18.csv"
data_ma_bl18 <- read_csv(data_github)  
as_tibble(data_ma_bl18)
mabl18_dp <- add_column(data_ma_bl18, scenario = rep("baseline", 1000))
mabl18dp <- mabl18_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl18dp <- add_column(mabl18dp, samplesize = mabl18dp$AdultMaleTested + mabl18dp$AdultFemaleTested)
mabl18dp <- add_column(mabl18dp, mrun = 1:1000)
mabl18dp$mrun <- ifelse(mabl18dp$mrun %% 100 == 0, 1, 0)
mabl18dp <- subset(mabl18dp, mrun == 1)
meanmabl18dp <- by(data_ma_bl18$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl18), 100, nrow(data_ma_bl18)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl18dp <- add_column(mabl18dp, meandp = meanmabl18dp)
mabl18dp_final <- mabl18dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl18dp_final$samplesize <- "30% harvest \n ~900 sample size"
mabl18dp_final$TotalCWD <- "0.8% CWD prevalence"

# model application scenario baseline 19
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl19.csv"
data_ma_bl19 <- read_csv(data_github)  
as_tibble(data_ma_bl19)
mabl19_dp <- add_column(data_ma_bl19, scenario = rep("baseline", 1000))
mabl19dp <- mabl19_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl19dp <- add_column(mabl19dp, samplesize = mabl19dp$AdultMaleTested + mabl19dp$AdultFemaleTested)
mabl19dp <- add_column(mabl19dp, mrun = 1:1000)
mabl19dp$mrun <- ifelse(mabl19dp$mrun %% 100 == 0, 1, 0)
mabl19dp <- subset(mabl19dp, mrun == 1)
meanmabl19dp <- by(data_ma_bl19$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl19), 100, nrow(data_ma_bl19)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl19dp <- add_column(mabl19dp, meandp = meanmabl19dp)
mabl19dp_final <- mabl19dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl19dp_final$samplesize <- "40% harvest \n ~1200 sample size"
mabl19dp_final$TotalCWD <- "0.8% CWD prevalence"

# model application scenario baseline 20
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl20.csv"
data_ma_bl20 <- read_csv(data_github) 
as_tibble(data_ma_bl20)
mabl20_dp <- add_column(data_ma_bl20, scenario = rep("baseline", 1000))
mabl20dp <- mabl20_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl20dp <- add_column(mabl20dp, samplesize = mabl20dp$AdultMaleTested + mabl20dp$AdultFemaleTested)
mabl20dp <- add_column(mabl20dp, mrun = 1:1000)
mabl20dp$mrun <- ifelse(mabl20dp$mrun %% 100 == 0, 1, 0)
mabl20dp <- subset(mabl20dp, mrun == 1)
meanmabl20dp <- by(data_ma_bl20$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl20), 100, nrow(data_ma_bl20)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl20dp <- add_column(mabl20dp, meandp = meanmabl20dp)
mabl20dp_final <- mabl20dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl20dp_final$samplesize <- "50% harvest \n ~1500 sample size"
mabl20dp_final$TotalCWD <- "0.8% CWD prevalence"

# model application scenario baseline 21
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl21.csv"
data_ma_bl21 <- read_csv(data_github)  
as_tibble(data_ma_bl21)
mabl21_dp <- add_column(data_ma_bl21, scenario = rep("baseline", 1000))
mabl21dp <- mabl21_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl21dp <- add_column(mabl21dp, samplesize = mabl21dp$AdultMaleTested + mabl21dp$AdultFemaleTested)
mabl21dp <- add_column(mabl21dp, mrun = 1:1000)
mabl21dp$mrun <- ifelse(mabl21dp$mrun %% 100 == 0, 1, 0)
mabl21dp <- subset(mabl21dp, mrun == 1)
meanmabl21dp <- by(data_ma_bl21$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl21), 100, nrow(data_ma_bl21)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl21dp <- add_column(mabl21dp, meandp = meanmabl21dp)
mabl21dp_final <- mabl21dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl21dp_final$samplesize <- "10% harvest \n ~300 sample size"
mabl21dp_final$TotalCWD <- "1% CWD prevalence"

# model application scenario baseline 22
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl22.csv"
data_ma_bl22 <- read_csv(data_github)  
as_tibble(data_ma_bl22)
mabl22_dp <- add_column(data_ma_bl22, scenario = rep("baseline", 1000))
mabl22dp <- mabl22_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl22dp <- add_column(mabl22dp, samplesize = mabl22dp$AdultMaleTested + mabl22dp$AdultFemaleTested)
mabl22dp <- add_column(mabl22dp, mrun = 1:1000)
mabl22dp$mrun <- ifelse(mabl22dp$mrun %% 100 == 0, 1, 0)
mabl22dp <- subset(mabl22dp, mrun == 1)
meanmabl22dp <- by(data_ma_bl22$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl22), 100, nrow(data_ma_bl22)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl22dp <- add_column(mabl22dp, meandp = meanmabl22dp)
mabl22dp_final <- mabl22dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl22dp_final$samplesize <- "20% harvest \n ~600 sample size"
mabl22dp_final$TotalCWD <- "1% CWD prevalence"

# model application scenario baseline 23
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl23.csv"
data_ma_bl23 <- read_csv(data_github)  
as_tibble(data_ma_bl23)
mabl23_dp <- add_column(data_ma_bl23, scenario = rep("baseline", 1000))
mabl23dp <- mabl23_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl23dp <- add_column(mabl23dp, samplesize = mabl23dp$AdultMaleTested + mabl23dp$AdultFemaleTested)
mabl23dp <- add_column(mabl23dp, mrun = 1:1000)
mabl23dp$mrun <- ifelse(mabl23dp$mrun %% 100 == 0, 1, 0)
mabl23dp <- subset(mabl23dp, mrun == 1)
meanmabl23dp <- by(data_ma_bl23$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl23), 100, nrow(data_ma_bl23)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl23dp <- add_column(mabl23dp, meandp = meanmabl23dp)
mabl23dp_final <- mabl23dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl23dp_final$samplesize <- "30% harvest \n ~900 sample size"
mabl23dp_final$TotalCWD <- "1% CWD prevalence"

# model application scenario baseline 24
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl24.csv"
data_ma_bl24 <- read_csv(data_github)  
as_tibble(data_ma_bl24)
mabl24_dp <- add_column(data_ma_bl24, scenario = rep("baseline", 1000))
mabl24dp <- mabl24_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl24dp <- add_column(mabl24dp, samplesize = mabl24dp$AdultMaleTested + mabl24dp$AdultFemaleTested)
mabl24dp <- add_column(mabl24dp, mrun = 1:1000)
mabl24dp$mrun <- ifelse(mabl24dp$mrun %% 100 == 0, 1, 0)
mabl24dp <- subset(mabl24dp, mrun == 1)
meanmabl24dp <- by(data_ma_bl24$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl24), 100, nrow(data_ma_bl24)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl24dp <- add_column(mabl24dp, meandp = meanmabl24dp)
mabl24dp_final <- mabl24dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl24dp_final$samplesize <- "40% harvest \n ~1200 sample size"
mabl24dp_final$TotalCWD <- "1% CWD prevalence"

# model application scenario baseline 25
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl25.csv"
data_ma_bl25 <- read_csv(data_github)  
as_tibble(data_ma_bl25)
mabl25_dp <- add_column(data_ma_bl25, scenario = rep("baseline", 1000))
mabl25dp <- mabl25_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
mabl25dp <- add_column(mabl25dp, samplesize = mabl25dp$AdultMaleTested + mabl25dp$AdultFemaleTested)
mabl25dp <- add_column(mabl25dp, mrun = 1:1000)
mabl25dp$mrun <- ifelse(mabl25dp$mrun %% 100 == 0, 1, 0)
mabl25dp <- subset(mabl25dp, mrun == 1)
meanmabl25dp <- by(data_ma_bl25$DetProb, list(gr=as.numeric(gl(nrow(data_ma_bl25), 100, nrow(data_ma_bl25)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
mabl25dp <- add_column(mabl25dp, meandp = meanmabl25dp)
mabl25dp_final <- mabl25dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
mabl25dp_final$samplesize <- "50% harvest \n ~1500 sample size"
mabl25dp_final$TotalCWD <- "1% CWD prevalence"

# model application scenario alternate 1
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt1.csv"
data_ma_alt1 <- read_csv(data_github)  
as_tibble(data_ma_alt1)
maalt1_dp <- add_column(data_ma_alt1, scenario = rep("alternate", 1000))
maalt1dp <- maalt1_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested)  #dplyr
maalt1dp <- add_column(maalt1dp, samplesize = maalt1dp$AdultMaleTested + maalt1dp$AdultFemaleTested)
maalt1dp <- add_column(maalt1dp, mrun = 1:1000)
maalt1dp$mrun <- ifelse(maalt1dp$mrun %% 100 == 0, 1, 0)
maalt1dp <- subset(maalt1dp, mrun == 1)
meanmaalt1dp <- by(data_ma_alt1$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt1), 100, nrow(data_ma_alt1)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
maalt1dp <- add_column(maalt1dp, meandp = meanmaalt1dp)
maalt1dp_final <- maalt1dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt1dp_final$samplesize <- "10% harvest \n ~300 sample size"
maalt1dp_final$TotalCWD <- "0.2% CWD prevalence"

# model application scenario alternate 2
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt2.csv"
data_ma_alt2 <- read_csv(data_github)  
as_tibble(data_ma_alt2)
maalt2_dp <- add_column(data_ma_alt2, scenario = rep("alternate", 1000))
maalt2dp <- maalt2_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt2dp <- add_column(maalt2dp, samplesize = maalt2dp$AdultMaleTested + maalt2dp$AdultFemaleTested)
maalt2dp <- add_column(maalt2dp, mrun = 1:1000)
maalt2dp$mrun <- ifelse(maalt2dp$mrun %% 100 == 0, 1, 0)
maalt2dp <- subset(maalt2dp, mrun == 1)
meanmaalt2dp <- by(data_ma_alt2$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt2), 100, nrow(data_ma_alt2)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
maalt2dp <- add_column(maalt2dp, meandp = meanmaalt2dp)
maalt2dp_final <- maalt2dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt2dp_final$samplesize <- "20% harvest \n ~600 sample size"
maalt2dp_final$TotalCWD <- "0.2% CWD prevalence"

# model application scenario alternate 3
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt3.csv"
data_ma_alt3 <- read_csv(data_github)  
as_tibble(data_ma_alt3)
maalt3_dp <- add_column(data_ma_alt3, scenario = rep("alternate", 1000))
maalt3dp <- maalt3_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt3dp <- add_column(maalt3dp, samplesize = maalt3dp$AdultMaleTested + maalt3dp$AdultFemaleTested)
maalt3dp <- add_column(maalt3dp, mrun = 1:1000)
maalt3dp$mrun <- ifelse(maalt3dp$mrun %% 100 == 0, 1, 0)
maalt3dp <- subset(maalt3dp, mrun == 1)
meanmaalt3dp <- by(data_ma_alt3$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt3), 100, nrow(data_ma_alt3)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
maalt3dp <- add_column(maalt3dp, meandp = meanmaalt3dp)
maalt3dp_final <- maalt3dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt3dp_final$samplesize <- "30% harvest \n ~900 sample size"
maalt3dp_final$TotalCWD <- "0.2% CWD prevalence"

# model application scenario alternate 4
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt4.csv"
data_ma_alt4 <- read_csv(data_github)  
as_tibble(data_ma_alt4)
maalt4_dp <- add_column(data_ma_alt4, scenario = rep("alternate", 1000))
maalt4dp <- maalt4_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt4dp <- add_column(maalt4dp, samplesize = maalt4dp$AdultMaleTested + maalt4dp$AdultFemaleTested)
maalt4dp <- add_column(maalt4dp, mrun = 1:1000)
maalt4dp$mrun <- ifelse(maalt4dp$mrun %% 100 == 0, 1, 0)
maalt4dp <- subset(maalt4dp, mrun == 1)
meanmaalt4dp <- by(data_ma_alt4$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt4), 100, nrow(data_ma_alt4)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
maalt4dp <- add_column(maalt4dp, meandp = meanmaalt4dp)
maalt4dp_final <- maalt4dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt4dp_final$samplesize <- "40% harvest \n ~1200 sample size"
maalt4dp_final$TotalCWD <- "0.2% CWD prevalence"

# model application scenario alternate 5
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt5.csv"
data_ma_alt5 <- read_csv(data_github)
as_tibble(data_ma_alt5)
maalt5_dp <- add_column(data_ma_alt5, scenario = rep("alternate", 1000))
maalt5dp <- maalt5_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt5dp <- add_column(maalt5dp, samplesize = maalt5dp$AdultMaleTested + maalt5dp$AdultFemaleTested)
maalt5dp <- add_column(maalt5dp, mrun = 1:1000)
maalt5dp$mrun <- ifelse(maalt5dp$mrun %% 100 == 0, 1, 0)
maalt5dp <- subset(maalt5dp, mrun == 1)
meanmaalt5dp <- by(data_ma_alt5$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt5), 100, nrow(data_ma_alt5)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
maalt5dp <- add_column(maalt5dp, meandp = meanmaalt5dp)
maalt5dp_final <- maalt5dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt5dp_final$samplesize <- "50% harvest \n ~1500 sample size"
maalt5dp_final$TotalCWD <- "0.2% CWD prevalence"

# model application scenario alternate 6
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt6.csv"
data_ma_alt6 <- read_csv(data_github)  
as_tibble(data_ma_alt6)
maalt6_dp <- add_column(data_ma_alt6, scenario = rep("alternate", 1000))
maalt6dp <- maalt6_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt6dp <- add_column(maalt6dp, samplesize = maalt6dp$AdultMaleTested + maalt6dp$AdultFemaleTested)
maalt6dp <- add_column(maalt6dp, mrun = 1:1000)
maalt6dp$mrun <- ifelse(maalt6dp$mrun %% 100 == 0, 1, 0)
maalt6dp <- subset(maalt6dp, mrun == 1)
meanmaalt6dp <- by(data_ma_alt6$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt6), 100, nrow(data_ma_alt6)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
maalt6dp <- add_column(maalt6dp, meandp = meanmaalt6dp)
maalt6dp_final <- maalt6dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt6dp_final$samplesize <- "10% harvest \n ~300 sample size"
maalt6dp_final$TotalCWD <- "0.4% CWD prevalence"

# model application scenario alternate 7
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt7.csv"
data_ma_alt7 <- read_csv(data_github)  
as_tibble(data_ma_alt7)
maalt7_dp <- add_column(data_ma_alt7, scenario = rep("alternate", 1000))
maalt7dp <- maalt7_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt7dp <- add_column(maalt7dp, samplesize = maalt7dp$AdultMaleTested + maalt7dp$AdultFemaleTested)
maalt7dp <- add_column(maalt7dp, mrun = 1:1000)
maalt7dp$mrun <- ifelse(maalt7dp$mrun %% 100 == 0, 1, 0)
maalt7dp <- subset(maalt7dp, mrun == 1)
meanmaalt7dp <- by(data_ma_alt7$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt7), 100, nrow(data_ma_alt7)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
maalt7dp <- add_column(maalt7dp, meandp = meanmaalt7dp)
maalt7dp_final <- maalt7dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt7dp_final$samplesize <- "20% harvest \n ~600 sample size"
maalt7dp_final$TotalCWD <- "0.4% CWD prevalence"

# model application scenario alternate 8
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt8.csv"
data_ma_alt8 <- read_csv(data_github)
as_tibble(data_ma_alt8)
maalt8_dp <- add_column(data_ma_alt8, scenario = rep("alternate", 1000))
maalt8dp <- maalt8_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt8dp <- add_column(maalt8dp, samplesize = maalt8dp$AdultMaleTested + maalt8dp$AdultFemaleTested)
maalt8dp <- add_column(maalt8dp, mrun = 1:1000)
maalt8dp$mrun <- ifelse(maalt8dp$mrun %% 100 == 0, 1, 0)
maalt8dp <- subset(maalt8dp, mrun == 1)
meanmaalt8dp <- by(data_ma_alt8$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt8), 100, nrow(data_ma_alt8)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
maalt8dp <- add_column(maalt8dp, meandp = meanmaalt8dp)
maalt8dp_final <- maalt8dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt8dp_final$samplesize <- "30% harvest \n ~900 sample size"
maalt8dp_final$TotalCWD <- "0.4% CWD prevalence"

# model application scenario alternate 9
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt9.csv"
data_ma_alt9 <- read_csv(data_github)
as_tibble(data_ma_alt9)
maalt9_dp <- add_column(data_ma_alt9, scenario = rep("alternate", 1000))
maalt9dp <- maalt9_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt9dp <- add_column(maalt9dp, samplesize = maalt9dp$AdultMaleTested + maalt9dp$AdultFemaleTested)
maalt9dp <- add_column(maalt9dp, mrun = 1:1000)
maalt9dp$mrun <- ifelse(maalt9dp$mrun %% 100 == 0, 1, 0)
maalt9dp <- subset(maalt9dp, mrun == 1)
meanmaalt9dp <- by(data_ma_alt9$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt9), 100, nrow(data_ma_alt9)))), 
                   FUN = function(x) colMeans(as.matrix(x)))
maalt9dp <- add_column(maalt9dp, meandp = meanmaalt9dp)
maalt9dp_final <- maalt9dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt9dp_final$samplesize <- "40% harvest \n ~1200 sample size"
maalt9dp_final$TotalCWD <- "0.4% CWD prevalence"

# model application scenario alternate 10
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt10.csv"
data_ma_alt10 <- read_csv(data_github)
as_tibble(data_ma_alt10)
maalt10_dp <- add_column(data_ma_alt10, scenario = rep("alternate", 1000))
maalt10dp <- maalt10_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt10dp <- add_column(maalt10dp, samplesize = maalt10dp$AdultMaleTested + maalt10dp$AdultFemaleTested)
maalt10dp <- add_column(maalt10dp, mrun = 1:1000)
maalt10dp$mrun <- ifelse(maalt10dp$mrun %% 100 == 0, 1, 0)
maalt10dp <- subset(maalt10dp, mrun == 1)
meanmaalt10dp <- by(data_ma_alt10$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt10), 100, nrow(data_ma_alt10)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt10dp <- add_column(maalt10dp, meandp = meanmaalt10dp)
maalt10dp_final <- maalt10dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt10dp_final$samplesize <- "50% harvest \n ~1500 sample size"
maalt10dp_final$TotalCWD <- "0.4% CWD prevalence"

# model application scenario alternate 11
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt11.csv"
data_ma_alt11 <- read_csv(data_github)
as_tibble(data_ma_alt11)
maalt11_dp <- add_column(data_ma_alt11, scenario = rep("alternate", 1000))
maalt11dp <- maalt11_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt11dp <- add_column(maalt11dp, samplesize = maalt11dp$AdultMaleTested + maalt11dp$AdultFemaleTested)
maalt11dp <- add_column(maalt11dp, mrun = 1:1000)
maalt11dp$mrun <- ifelse(maalt11dp$mrun %% 100 == 0, 1, 0)
maalt11dp <- subset(maalt11dp, mrun == 1)
meanmaalt11dp <- by(data_ma_alt11$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt11), 100, nrow(data_ma_alt11)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt11dp <- add_column(maalt11dp, meandp = meanmaalt11dp)
maalt11dp_final <- maalt11dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt11dp_final$samplesize <- "10% harvest \n ~300 sample size"
maalt11dp_final$TotalCWD <- "0.6% CWD prevalence"

# model application scenario alternate 12
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt12.csv"
data_ma_alt12 <- read_csv(data_github)
as_tibble(data_ma_alt12)
maalt12_dp <- add_column(data_ma_alt12, scenario = rep("alternate", 1000))
maalt12dp <- maalt12_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt12dp <- add_column(maalt12dp, samplesize = maalt12dp$AdultMaleTested + maalt12dp$AdultFemaleTested)
maalt12dp <- add_column(maalt12dp, mrun = 1:1000)
maalt12dp$mrun <- ifelse(maalt12dp$mrun %% 100 == 0, 1, 0)
maalt12dp <- subset(maalt12dp, mrun == 1)
meanmaalt12dp <- by(data_ma_alt12$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt12), 100, nrow(data_ma_alt12)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt12dp <- add_column(maalt12dp, meandp = meanmaalt12dp)
maalt12dp_final <- maalt12dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt12dp_final$samplesize <- "20% harvest \n ~600 sample size"
maalt12dp_final$TotalCWD <- "0.6% CWD prevalence"

# model application scenario alternate 13
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt13.csv"
data_ma_alt13 <- read_csv(data_github)
as_tibble(data_ma_alt13)
maalt13_dp <- add_column(data_ma_alt13, scenario = rep("alternate", 1000))
maalt13dp <- maalt13_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt13dp <- add_column(maalt13dp, samplesize = maalt13dp$AdultMaleTested + maalt13dp$AdultFemaleTested)
maalt13dp <- add_column(maalt13dp, mrun = 1:1000)
maalt13dp$mrun <- ifelse(maalt13dp$mrun %% 100 == 0, 1, 0)
maalt13dp <- subset(maalt13dp, mrun == 1)
meanmaalt13dp <- by(data_ma_alt13$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt13), 100, nrow(data_ma_alt13)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt13dp <- add_column(maalt13dp, meandp = meanmaalt13dp)
maalt13dp_final <- maalt13dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt13dp_final$samplesize <- "30% harvest \n ~900 sample size"
maalt13dp_final$TotalCWD <- "0.6% CWD prevalence"

# model application scenario alternate 14
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt14.csv"
data_ma_alt14 <- read_csv(data_github)
as_tibble(data_ma_alt14)
maalt14_dp <- add_column(data_ma_alt14, scenario = rep("alternate", 1000))
maalt14dp <- maalt14_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt14dp <- add_column(maalt14dp, samplesize = maalt14dp$AdultMaleTested + maalt14dp$AdultFemaleTested)
maalt14dp <- add_column(maalt14dp, mrun = 1:1000)
maalt14dp$mrun <- ifelse(maalt14dp$mrun %% 100 == 0, 1, 0)
maalt14dp <- subset(maalt14dp, mrun == 1)
meanmaalt14dp <- by(data_ma_alt14$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt14), 100, nrow(data_ma_alt14)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt14dp <- add_column(maalt14dp, meandp = meanmaalt14dp)
maalt14dp_final <- maalt14dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt14dp_final$samplesize <- "40% harvest \n ~1200 sample size"
maalt14dp_final$TotalCWD <- "0.6% CWD prevalence"

# model application scenario alternate 15
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt15.csv"
data_ma_alt15 <- read_csv(data_github)
as_tibble(data_ma_alt15)
maalt15_dp <- add_column(data_ma_alt15, scenario = rep("alternate", 1000))
maalt15dp <- maalt15_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt15dp <- add_column(maalt15dp, samplesize = maalt15dp$AdultMaleTested + maalt15dp$AdultFemaleTested)
maalt15dp <- add_column(maalt15dp, mrun = 1:1000)
maalt15dp$mrun <- ifelse(maalt15dp$mrun %% 100 == 0, 1, 0)
maalt15dp <- subset(maalt15dp, mrun == 1)
meanmaalt15dp <- by(data_ma_alt15$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt15), 100, nrow(data_ma_alt15)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt15dp <- add_column(maalt15dp, meandp = meanmaalt15dp)
maalt15dp_final <- maalt15dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt15dp_final$samplesize <- "50% harvest \n ~1500 sample size"
maalt15dp_final$TotalCWD <- "0.6% CWD prevalence"

# model application scenario alternate 16
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt16.csv"
data_ma_alt16 <- read_csv(data_github)
as_tibble(data_ma_alt16)
maalt16_dp <- add_column(data_ma_alt16, scenario = rep("alternate", 1000))
maalt16dp <- maalt16_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt16dp <- add_column(maalt16dp, samplesize = maalt16dp$AdultMaleTested + maalt16dp$AdultFemaleTested)
maalt16dp <- add_column(maalt16dp, mrun = 1:1000)
maalt16dp$mrun <- ifelse(maalt16dp$mrun %% 100 == 0, 1, 0)
maalt16dp <- subset(maalt16dp, mrun == 1)
meanmaalt16dp <- by(data_ma_alt16$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt16), 100, nrow(data_ma_alt16)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt16dp <- add_column(maalt16dp, meandp = meanmaalt16dp)
maalt16dp_final <- maalt16dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt16dp_final$samplesize <- "10% harvest \n ~300 sample size"
maalt16dp_final$TotalCWD <- "0.8% CWD prevalence"

# model application scenario alternate 17
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt17.csv"
data_ma_alt17 <- read_csv(data_github)
as_tibble(data_ma_alt17)
maalt17_dp <- add_column(data_ma_alt17, scenario = rep("alternate", 1000))
maalt17dp <- maalt17_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt17dp <- add_column(maalt17dp, samplesize = maalt17dp$AdultMaleTested + maalt17dp$AdultFemaleTested)
maalt17dp <- add_column(maalt17dp, mrun = 1:1000)
maalt17dp$mrun <- ifelse(maalt17dp$mrun %% 100 == 0, 1, 0)
maalt17dp <- subset(maalt17dp, mrun == 1)
meanmaalt17dp <- by(data_ma_alt17$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt17), 100, nrow(data_ma_alt17)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt17dp <- add_column(maalt17dp, meandp = meanmaalt17dp)
maalt17dp_final <- maalt17dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt17dp_final$samplesize <- "20% harvest \n ~600 sample size"
maalt17dp_final$TotalCWD <- "0.8% CWD prevalence"

# model application scenario alternate 18
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt18.csv"
data_ma_alt18 <- read_csv(data_github)
as_tibble(data_ma_alt18)
maalt18_dp <- add_column(data_ma_alt18, scenario = rep("alternate", 1000))
maalt18dp <- maalt18_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt18dp <- add_column(maalt18dp, samplesize = maalt18dp$AdultMaleTested + maalt18dp$AdultFemaleTested)
maalt18dp <- add_column(maalt18dp, mrun = 1:1000)
maalt18dp$mrun <- ifelse(maalt18dp$mrun %% 100 == 0, 1, 0)
maalt18dp <- subset(maalt18dp, mrun == 1)
meanmaalt18dp <- by(data_ma_alt18$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt18), 100, nrow(data_ma_alt18)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt18dp <- add_column(maalt18dp, meandp = meanmaalt18dp)
maalt18dp_final <- maalt18dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt18dp_final$samplesize <- "30% harvest \n ~900 sample size"
maalt18dp_final$TotalCWD <- "0.8% CWD prevalence"

# model application scenario alternate 19
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt19.csv"
data_ma_alt19 <- read_csv(data_github)
as_tibble(data_ma_alt19)
maalt19_dp <- add_column(data_ma_alt19, scenario = rep("alternate", 1000))
maalt19dp <- maalt19_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt19dp <- add_column(maalt19dp, samplesize = maalt19dp$AdultMaleTested + maalt19dp$AdultFemaleTested)
maalt19dp <- add_column(maalt19dp, mrun = 1:1000)
maalt19dp$mrun <- ifelse(maalt19dp$mrun %% 100 == 0, 1, 0)
maalt19dp <- subset(maalt19dp, mrun == 1)
meanmaalt19dp <- by(data_ma_alt19$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt19), 100, nrow(data_ma_alt19)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt19dp <- add_column(maalt19dp, meandp = meanmaalt19dp)
maalt19dp_final <- maalt19dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt19dp_final$samplesize <- "40% harvest \n ~1200 sample size"
maalt19dp_final$TotalCWD <- "0.8% CWD prevalence"

# model application scenario alternate 20
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt20.csv"
data_ma_alt20 <- read_csv(data_github)
as_tibble(data_ma_alt20)
maalt20_dp <- add_column(data_ma_alt20, scenario = rep("alternate", 1000))
maalt20dp <- maalt20_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt20dp <- add_column(maalt20dp, samplesize = maalt20dp$AdultMaleTested + maalt20dp$AdultFemaleTested)
maalt20dp <- add_column(maalt20dp, mrun = 1:1000)
maalt20dp$mrun <- ifelse(maalt20dp$mrun %% 100 == 0, 1, 0)
maalt20dp <- subset(maalt20dp, mrun == 1)
meanmaalt20dp <- by(data_ma_alt20$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt20), 100, nrow(data_ma_alt20)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt20dp <- add_column(maalt20dp, meandp = meanmaalt20dp)
maalt20dp_final <- maalt20dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt20dp_final$samplesize <- "50% harvest \n ~1500 sample size"
maalt20dp_final$TotalCWD <- "0.8% CWD prevalence"

# model application scenario alternate 21
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt21.csv"
data_ma_alt21 <- read_csv(data_github)
as_tibble(data_ma_alt21)
maalt21_dp <- add_column(data_ma_alt21, scenario = rep("alternate", 1000))
maalt21dp <- maalt21_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt21dp <- add_column(maalt21dp, samplesize = maalt21dp$AdultMaleTested + maalt21dp$AdultFemaleTested)
maalt21dp <- add_column(maalt21dp, mrun = 1:1000)
maalt21dp$mrun <- ifelse(maalt21dp$mrun %% 100 == 0, 1, 0)
maalt21dp <- subset(maalt21dp, mrun == 1)
meanmaalt21dp <- by(data_ma_alt21$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt21), 100, nrow(data_ma_alt21)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt21dp <- add_column(maalt21dp, meandp = meanmaalt21dp)
maalt21dp_final <- maalt21dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt21dp_final$samplesize <- "10% harvest \n ~300 sample size"
maalt21dp_final$TotalCWD <- "1% CWD prevalence"

# model application scenario alternate 22
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt22.csv"
data_ma_alt22 <- read_csv(data_github)
as_tibble(data_ma_alt22)
maalt22_dp <- add_column(data_ma_alt22, scenario = rep("alternate", 1000))
maalt22dp <- maalt22_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt22dp <- add_column(maalt22dp, samplesize = maalt22dp$AdultMaleTested + maalt22dp$AdultFemaleTested)
maalt22dp <- add_column(maalt22dp, mrun = 1:1000)
maalt22dp$mrun <- ifelse(maalt22dp$mrun %% 100 == 0, 1, 0)
maalt22dp <- subset(maalt22dp, mrun == 1)
meanmaalt22dp <- by(data_ma_alt22$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt22), 100, nrow(data_ma_alt22)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt22dp <- add_column(maalt22dp, meandp = meanmaalt22dp)
maalt22dp_final <- maalt22dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt22dp_final$samplesize <- "20% harvest \n ~600 sample size"
maalt22dp_final$TotalCWD <- "1% CWD prevalence"

# model application scenario alternate 23
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt23.csv"
data_ma_alt23 <- read_csv(data_github)
as_tibble(data_ma_alt23)
maalt23_dp <- add_column(data_ma_alt23, scenario = rep("alternate", 1000))
maalt23dp <- maalt23_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt23dp <- add_column(maalt23dp, samplesize = maalt23dp$AdultMaleTested + maalt23dp$AdultFemaleTested)
maalt23dp <- add_column(maalt23dp, mrun = 1:1000)
maalt23dp$mrun <- ifelse(maalt23dp$mrun %% 100 == 0, 1, 0)
maalt23dp <- subset(maalt23dp, mrun == 1)
meanmaalt23dp <- by(data_ma_alt23$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt23), 100, nrow(data_ma_alt23)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt23dp <- add_column(maalt23dp, meandp = meanmaalt23dp)
maalt23dp_final <- maalt23dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt23dp_final$samplesize <- "30% harvest \n ~900 sample size"
maalt23dp_final$TotalCWD <- "1% CWD prevalence"

# model application scenario alternate 24
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt24.csv"
data_ma_alt24 <- read_csv(data_github)
as_tibble(data_ma_alt24)
maalt24_dp <- add_column(data_ma_alt24, scenario = rep("alternate", 1000))
maalt24dp <- maalt24_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt24dp <- add_column(maalt24dp, samplesize = maalt24dp$AdultMaleTested + maalt24dp$AdultFemaleTested)
maalt24dp <- add_column(maalt24dp, mrun = 1:1000)
maalt24dp$mrun <- ifelse(maalt24dp$mrun %% 100 == 0, 1, 0)
maalt24dp <- subset(maalt24dp, mrun == 1)
meanmaalt24dp <- by(data_ma_alt24$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt24), 100, nrow(data_ma_alt24)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt24dp <- add_column(maalt24dp, meandp = meanmaalt24dp)
maalt24dp_final <- maalt24dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt24dp_final$samplesize <- "40% harvest \n ~1200 sample size"
maalt24dp_final$TotalCWD <- "1% CWD prevalence"

# model application scenario alternate 25
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt25.csv"
data_ma_alt25 <- read_csv(data_github)
as_tibble(data_ma_alt25)
maalt25_dp <- add_column(data_ma_alt25, scenario = rep("alternate", 1000))
maalt25dp <- maalt25_dp %>% select(scenario, DetProb, TotalCWD, AdultMaleTested, AdultFemaleTested) #dplyr
maalt25dp <- add_column(maalt25dp, samplesize = maalt25dp$AdultMaleTested + maalt25dp$AdultFemaleTested)
maalt25dp <- add_column(maalt25dp, mrun = 1:1000)
maalt25dp$mrun <- ifelse(maalt25dp$mrun %% 100 == 0, 1, 0)
maalt25dp <- subset(maalt25dp, mrun == 1)
meanmaalt25dp <- by(data_ma_alt25$DetProb, list(gr=as.numeric(gl(nrow(data_ma_alt25), 100, nrow(data_ma_alt25)))), 
                    FUN = function(x) colMeans(as.matrix(x)))
maalt25dp <- add_column(maalt25dp, meandp = meanmaalt25dp)
maalt25dp_final <- maalt25dp %>% select(scenario, TotalCWD, samplesize, meandp) #dplyr
maalt25dp_final$samplesize <- "50% harvest \n ~1500 sample size" #
maalt25dp_final$TotalCWD <- "1% CWD prevalence"
#-----------------------------------------------------------------
#-----------------------------------------------------------------
ModApp <- rbind(mabl1dp_final, mabl2dp_final, mabl3dp_final, mabl4dp_final, mabl5dp_final, mabl6dp_final, mabl7dp_final, mabl8dp_final, mabl8dp_final, mabl9dp_final, mabl10dp_final, mabl11dp_final, mabl12dp_final, mabl13dp_final, mabl14dp_final, mabl15dp_final, mabl16dp_final, mabl17dp_final, mabl18dp_final, mabl19dp_final, mabl20dp_final, mabl21dp_final, mabl22dp_final, mabl23dp_final, mabl24dp_final, mabl25dp_final, maalt1dp_final, maalt2dp_final, maalt3dp_final, maalt4dp_final, maalt5dp_final, maalt6dp_final, maalt7dp_final, maalt8dp_final, maalt9dp_final, maalt10dp_final, maalt11dp_final, maalt12dp_final, maalt13dp_final, maalt14dp_final, maalt15dp_final, maalt16dp_final, maalt17dp_final, maalt18dp_final, maalt19dp_final, maalt20dp_final, maalt21dp_final, maalt22dp_final, maalt23dp_final, maalt24dp_final, maalt25dp_final)
ModApp$TotalCWD <- factor(ModApp$TotalCWD)
ModApp$scenario <-factor(ModApp$scenario,levels=c("baseline","alternate"))
p1 <- ggplot(data = ModApp, mapping = aes(x = TotalCWD, y = meandp, fill = scenario)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2, alpha = 0.2) + scale_y_continuous(limits = c(0,1), breaks=c(0, 0.25, 0.50, 0.75, 1)) + annotate("rect", xmin = 0, xmax = 6, ymin = 0.95, ymax = 1, alpha = .4) + facet_wrap(~samplesize, ncol = 5, labeller = labeller(sampleisize = labels)) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) #+ theme(legend.position = c(0.92, 0.2),legend.background = element_rect(fill = "white", colour = NA))#aes(fill = si)+ theme(strip.background = element_blank(), strip.text.x = element_blank())labeller(si = labels), scales="free"
p2 <- p1  + scale_fill_manual(values=c("grey50","grey100"), labels = c("baseline", "alternate"), name = "Scenario") + scale_x_discrete(labels = c("0.2","0.4","0.6","0.8","1")) + theme_bw() + theme(panel.grid.minor=element_blank())
p3 <- p2 + ylab("detection probability") + xlab("\n CWD prevalence (%)") + theme(legend.position = c(0.92, 0.2),legend.background = element_rect(fill = "white", colour = NA))
modelappanalysis <- p3
modelappanalysis