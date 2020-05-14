# Belsare A, Gompper M, Keller B, Sumners J, Hansen L, Millspaugh J. 
# An agent-based framework for improving wildlife disease surveillance:
# A case study of chronic wasting disease in Missouri white-tailed deer.
# Ecol Modell. 2020;417. doi:http://dx.doi.org/10.1101/478610

# (Co-submission) Belsare A, Gompper M, Keller B, Sumners J, Hansen L, Millspaugh J.
# Size Matters: Sample size assessments for chronic wasting disease
# surveillance using an agent-based modeling framework. MethodsX (in review)

require(ggplot2)
require(tidyr)
require(reshape2)
require(gridExtra)
require(grid)
require(ggpubr)
library(matrixStats)
require(dplyr)
require (tibble)

#-------------------------------------------------------------------------------
# Code for MOOvPOPsurveillance evaluation: model-derived detection probabilities 
# for 12 prevalence-sample size scenarios compared with detection probabilities
# derived from hypergeometric approximation (standardized sample size tables).
# We use one-sample t-test for comparisons.
#-------------------------------------------------------------------------------
# Scenario 1 (sc1): baseline assumptions, 0.5% prevalence in adult males, sample size ~435

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc1.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.9     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
# Scenario 2 (sc2): baseline assumptions, 0.5% prevalence in adult males, sample size ~556

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc2.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.95     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
# Scenario 3 (sc3): baseline assumptions, 0.5% prevalence in adult males, sample size ~822

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc3.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.99     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
# Scenario 4 (sc4): baseline assumptions, 1% prevalence in adult males, sample size ~222

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc4.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.9     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
#Scenario 5 (sc5): baseline assumptions, 1% prevalence in adult males, sample size ~288

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc5.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.95     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
# Scenario 6 (sc6): baseline assumptions, 1% prevalence in adult males, sample size ~434

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc6.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.99     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
# Scenario 7 (sc7): baseline assumptions, 2% prevalence in adult males, sample size ~113

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc7.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.9     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
# Scenario 8 (sc8): baseline assumptions, 2% prevalence in adult males, sample size ~147

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc8.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.95     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
# Scenario 9 (sc9): baseline assumptions, 2% prevalence in adult males, sample size ~223

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc9.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.99     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
# Scenario 10 (sc10): baseline assumptions, 5% prevalence in adult males, sample size ~46

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc10.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.9     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
# Scenario 11 (sc11): baseline assumptions, 5% prevalence in adult males, sample size ~59

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc11.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.95     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------
# Scenario 12 (sc12): baseline assumptions, 5% prevalence in adult males, sample size ~90

data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_sc12.csv"
data <- read_csv(data_github)
obsmean <- by(data$DetProb, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.99     #confidence in detecting disease or detection probability
t.test(obsmean, mu = theoretical, conf.int=0.95)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Code for sensitivity analysis MOOvPOPsurveillance and Figure 6 MethodsX article,
# also Figure 8 Ecological Modelling article.
#-------------------------------------------------------------------------------
# sensitivity analysis scenario baseline
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_bl1000.csv"
data_sa_bl <- read_csv(data_github)
as_tibble(data_sa_bl)
bl_dp <- add_column(data_sa_bl, scenario = rep("baseline", 1000))
bldp <- bl_dp %>% select(scenario, DetProb)   #dplyr
bldp <- add_column(bldp, mrun = 1:1000)
bldp$mrun <- ifelse(bldp$mrun %% 100 == 0, 1, 0)
bldp1 <- subset(bldp, mrun == 1)
meanbldp <- by(data_sa_bl$DetProb, list(gr=as.numeric(gl(nrow(data_sa_bl), 100, nrow(data_sa_bl)))), 
               FUN = function(x) colMeans(as.matrix(x)))
bldp1 <- add_column(bldp1, meandp = meanbldp)
bldp2 <- bldp1 %>% select(scenario, meandp)   #dplyr
bldp2

# sensitivity analysis alternate scenario
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_alt1000.csv"
data_sa_alt <- read_csv(data_github)
as_tibble(data_sa_alt)
alt_dp <- add_column(data_sa_alt, scenario = rep("alternate", 1000))
altdp <- alt_dp %>% select(scenario, DetProb)   #dplyr
altdp <- add_column(altdp, mrun = 1:1000)
altdp$mrun <- ifelse(altdp$mrun %% 100 == 0, 1, 0)
altdp1 <- subset(altdp, mrun == 1)
meanaltdp <- by(data_sa_alt$DetProb, list(gr=as.numeric(gl(nrow(data_sa_alt), 100, nrow(data_sa_alt)))), 
                FUN = function(x) colMeans(as.matrix(x)))
altdp1 <- add_column(altdp1, meandp = meanaltdp)
altdp2 <- altdp1 %>% select(scenario, meandp)
altdp2

# sensitivity analysis scenario 98% clustering
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_cl981000.csv"
data_sa_cl98 <- read_csv(data_github)
as_tibble(data_sa_cl98)
cl98_dp <- add_column(data_sa_cl98, scenario = rep("cluster98", 1000))
cl98dp <- cl98_dp %>% select(scenario, DetProb)   #dplyr
cl98dp <- add_column(cl98dp, mrun = 1:1000)
cl98dp$mrun <- ifelse(cl98dp$mrun %% 100 == 0, 1, 0)
cl98dp1 <- subset(cl98dp, mrun == 1)
meancl98dp <- by(data_sa_cl98$DetProb, list(gr=as.numeric(gl(nrow(data_sa_cl98), 100, nrow(data_sa_cl98)))), 
                 FUN = function(x) colMeans(as.matrix(x)))
cl98dp1 <- add_column(cl98dp1, meandp = meancl98dp)
cl98dp2 <- cl98dp1 %>% select(scenario, meandp)
cl98dp2

# sensitivity analysis scenario 96% clustering
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_cl961000.csv"
data_sa_cl96 <- read_csv(data_github)
as_tibble(data_sa_cl96)
cl96_dp <- add_column(data_sa_cl96, scenario = rep("cluster96", 1000))
cl96dp <- cl96_dp %>% select(scenario, DetProb)   #dplyr
cl96dp <- add_column(cl96dp, mrun = 1:1000)
cl96dp$mrun <- ifelse(cl96dp$mrun %% 100 == 0, 1, 0)
cl96dp1 <- subset(cl96dp, mrun == 1)
meancl96dp <- by(data_sa_cl96$DetProb, list(gr=as.numeric(gl(nrow(data_sa_cl96), 100, nrow(data_sa_cl96)))), 
                 FUN = function(x) colMeans(as.matrix(x)))
cl96dp1 <- add_column(cl96dp1, meandp = meancl96dp)
cl96dp2 <- cl96dp1 %>% select(scenario, meandp)
cl96dp2

# sensitivity analysis scenario 94% clustering
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_cl941000.csv"
data_sa_cl94 <- read_csv(data_github)
as_tibble(data_sa_cl94)
cl94_dp <- add_column(data_sa_cl94, scenario = rep("cluster94", 1000))
cl94dp <- cl94_dp %>% select(scenario, DetProb)   #dplyr
cl94dp <- add_column(cl94dp, mrun = 1:1000)
cl94dp$mrun <- ifelse(cl94dp$mrun %% 100 == 0, 1, 0)
cl94dp1 <- subset(cl94dp, mrun == 1)
meancl94dp <- by(data_sa_cl94$DetProb, list(gr=as.numeric(gl(nrow(data_sa_cl94), 100, nrow(data_sa_cl94)))), 
                 FUN = function(x) colMeans(as.matrix(x)))
cl94dp1 <- add_column(cl94dp1, meandp = meancl94dp)
cl94dp2 <- cl94dp1 %>% select(scenario, meandp)
cl94dp2

# sensitivity analysis scenario 92% clustering
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_cl921000.csv"
data_sa_cl92 <- read_csv(data_github)
as_tibble(data_sa_cl92)
cl92_dp <- add_column(data_sa_cl92, scenario = rep("cluster92", 1000))
cl92dp <- cl92_dp %>% select(scenario, DetProb)   #dplyr
cl92dp <- add_column(cl92dp, mrun = 1:1000)
cl92dp$mrun <- ifelse(cl92dp$mrun %% 100 == 0, 1, 0)
cl92dp1 <- subset(cl92dp, mrun == 1)
meancl92dp <- by(data_sa_cl92$DetProb, list(gr=as.numeric(gl(nrow(data_sa_cl92), 100, nrow(data_sa_cl92)))), 
                 FUN = function(x) colMeans(as.matrix(x)))
cl92dp1 <- add_column(cl92dp1, meandp = meancl92dp)
cl92dp2 <- cl92dp1 %>% select(scenario, meandp)
cl92dp2

# sensitivity analysis scenario 90% clustering
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_cl901000.csv"
data_sa_cl90 <- read_csv(data_github)
as_tibble(data_sa_cl90)
cl90_dp <- add_column(data_sa_cl90, scenario = rep("cluster90", 1000))
cl90dp <- cl90_dp %>% select(scenario, DetProb)   #dplyr
cl90dp <- add_column(cl90dp, mrun = 1:1000)
cl90dp$mrun <- ifelse(cl90dp$mrun %% 100 == 0, 1, 0)
cl90dp1 <- subset(cl90dp, mrun == 1)
meancl90dp <- by(data_sa_cl90$DetProb, list(gr=as.numeric(gl(nrow(data_sa_cl90), 100, nrow(data_sa_cl90)))), 
                 FUN = function(x) colMeans(as.matrix(x)))
cl90dp1 <- add_column(cl90dp1, meandp = meancl90dp)
cl90dp2 <- cl90dp1 %>% select(scenario, meandp)
cl90dp2

# sensitivity analysis scenario non-random sampling 18
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_nrs18.csv"
data_sa_nrs18 <- read_csv(data_github)
as_tibble(data_sa_nrs18)
nrs18_dp <- add_column(data_sa_nrs18, scenario = rep("nrs18", 1000))
nrs18dp <- nrs18_dp %>% select(scenario, DetProb)   #dplyr
nrs18dp <- add_column(nrs18dp, mrun = 1:1000)
nrs18dp$mrun <- ifelse(nrs18dp$mrun %% 100 == 0, 1, 0)
nrs18dp1 <- subset(nrs18dp, mrun == 1)
meannrs18dp <- by(data_sa_nrs18$DetProb, list(gr=as.numeric(gl(nrow(data_sa_nrs18), 100, nrow(data_sa_nrs18)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
nrs18dp1 <- add_column(nrs18dp1, meandp = meannrs18dp)
nrs18dp2 <- nrs18dp1 %>% select(scenario, meandp)
nrs18dp2

# sensitivity analysis scenario non-random sampling 21
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_nrs21.csv"
data_sa_nrs21 <- read_csv(data_github)
as_tibble(data_sa_nrs21)
nrs21_dp <- add_column(data_sa_nrs21, scenario = rep("nrs21", 1000))
nrs21dp <- nrs21_dp %>% select(scenario, DetProb)   #dplyr
nrs21dp <- add_column(nrs21dp, mrun = 1:1000)
nrs21dp$mrun <- ifelse(nrs21dp$mrun %% 100 == 0, 1, 0)
nrs21dp1 <- subset(nrs21dp, mrun == 1)
meannrs21dp <- by(data_sa_nrs21$DetProb, list(gr=as.numeric(gl(nrow(data_sa_nrs21), 100, nrow(data_sa_nrs21)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
nrs21dp1 <- add_column(nrs21dp1, meandp = meannrs21dp)
nrs21dp2 <- nrs21dp1 %>% select(scenario, meandp)
nrs21dp2

# sensitivity analysis scenario non-random sampling 24
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_nrs24.csv"
data_sa_nrs24 <- read_csv(data_github)
as_tibble(data_sa_nrs24)
nrs24_dp <- add_column(data_sa_nrs24, scenario = rep("nrs24", 1000))
nrs24dp <- nrs24_dp %>% select(scenario, DetProb)   #dplyr
nrs24dp <- add_column(nrs24dp, mrun = 1:1000)
nrs24dp$mrun <- ifelse(nrs24dp$mrun %% 100 == 0, 1, 0)
nrs24dp1 <- subset(nrs24dp, mrun == 1)
meannrs24dp <- by(data_sa_nrs24$DetProb, list(gr=as.numeric(gl(nrow(data_sa_nrs24), 100, nrow(data_sa_nrs24)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
nrs24dp1 <- add_column(nrs24dp1, meandp = meannrs24dp)
nrs24dp2 <- nrs24dp1 %>% select(scenario, meandp)
nrs24dp2

# sensitivity analysis scenario non-random sampling 27
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_nrs27.csv"
data_sa_nrs27 <- read_csv(data_github)
as_tibble(data_sa_nrs27)
nrs27_dp <- add_column(data_sa_nrs27, scenario = rep("nrs27", 1000))
nrs27dp <- nrs27_dp %>% select(scenario, DetProb)   #dplyr
nrs27dp <- add_column(nrs27dp, mrun = 1:1000)
nrs27dp$mrun <- ifelse(nrs27dp$mrun %% 100 == 0, 1, 0)
nrs27dp1 <- subset(nrs27dp, mrun == 1)
meannrs27dp <- by(data_sa_nrs27$DetProb, list(gr=as.numeric(gl(nrow(data_sa_nrs27), 100, nrow(data_sa_nrs27)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
nrs27dp1 <- add_column(nrs27dp1, meandp = meannrs27dp)
nrs27dp2 <- nrs27dp1 %>% select(scenario, meandp)
nrs27dp2

# sensitivity analysis scenario non-random sampling 30
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/CWDsurveillanceMO_nrs30.csv"
data_sa_nrs30 <- read_csv(data_github)
as_tibble(data_sa_nrs30)
nrs30_dp <- add_column(data_sa_nrs30, scenario = rep("nrs30", 1000))
nrs30dp <- nrs30_dp %>% select(scenario, DetProb)   #dplyr
nrs30dp <- add_column(nrs30dp, mrun = 1:1000)
nrs30dp$mrun <- ifelse(nrs30dp$mrun %% 100 == 0, 1, 0)
nrs30dp1 <- subset(nrs30dp, mrun == 1)
meannrs30dp <- by(data_sa_nrs30$DetProb, list(gr=as.numeric(gl(nrow(data_sa_nrs30), 100, nrow(data_sa_nrs30)))), 
                  FUN = function(x) colMeans(as.matrix(x)))
nrs30dp1 <- add_column(nrs30dp1, meandp = meannrs27dp)
nrs30dp2 <- nrs30dp1 %>% select(scenario, meandp)
nrs30dp2
#-------------------------------------------------------------------------------
sana <- rbind(bldp2, altdp2, cl98dp2, cl96dp2, cl94dp2, cl92dp2, cl90dp2, nrs18dp2, nrs21dp2, nrs24dp2, nrs27dp2, nrs30dp2)
sana$scenario <- ordered(sana$scenario, levels=c("cluster90", "cluster92", "cluster94", "cluster96", "cluster98", "alternate", "nrs18", "nrs21", "nrs24", "nrs27", "nrs30", "baseline"))
p1 <- ggplot(sana, aes(scenario,meandp)) + geom_boxplot(outlier.shape = NA) + geom_boxplot(data=sana[sana$scenario=="baseline",],aes(x = scenario, y = meandp),fill="green") + geom_jitter(width = 0.2, alpha = 0.2) + stat_summary(fun.y=mean, geom="point",shape=23,size=4) + annotate("rect", xmin = 0, xmax = 14, ymin = .95, ymax = 1, alpha = .4) + scale_y_continuous(limits = c(0.7,1), breaks=c(0.7,0.8,0.9,1))
p2 <- p1 + scale_x_discrete(labels=c("cluster 90%","cluster 92%","cluster 94%","cluster 96%","cluster 98%","Alternate","non-random sampling 18","non-random sampling 21","non-random sampling 24","non-random sampling 27","non-random sampling 30","Baseline"))
p3 <- p2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("detection probability") + xlab("Scenarios\n") #, face = c('bold','plain','plain','plain','plain','plain','bold','plain','plain','plain','plain','plain')
p4 <- p3 + coord_flip() + theme_bw() + theme(panel.grid.minor=element_blank()) + theme(axis.text.x = element_text(size=13),axis.title.x = element_text(size= 16)) + theme(axis.text.y = element_text(size = 13),axis.title.y = element_text(size= 16))
p5 <- p4 + theme(axis.text.y = element_text(face = c('plain','plain','plain','plain','plain','bold','plain','plain','plain','plain','plain','bold')))
sen.ana <- p5
sen.ana