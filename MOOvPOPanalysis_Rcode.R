# Belsare A, Gompper M, Keller B, Sumners J, Hansen L, Millspaugh J.
# An agent-based framework for improving wildlife disease surveillance:
# A case study of chronic wasting disease in Missouri white-tailed deer.
# Ecol Modell. 2020;417. doi:http://dx.doi.org/10.1101/478610

# (Co-submission) Belsare A, Gompper M, Keller B, Sumners J, Hansen L, Millspaugh J.
# Size Matters: Sample size assessments for chronic wasting disease
# surveillance using an agent-based modeling framework. MethodsX (in review) 

require (dplyr)
require (ggplot2)
require (tidyverse)
require(tidyr)
require (tibble)
require (cowplot)
require (gridExtra)

#-------------------------------------------------------------------------------
# Data from five MOOvPOP iterations is used to assess finite population
# growth rate (lambda) and age structure of the model-generated populations.
#-------------------------------------------------------------------------------
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/deerpopdyFranklinCounty_5iterations.csv"
data_5ite <- read_csv(data_github)
data_5ite = data_5ite[-1,] # deletes the very 1st row, which is the user provided information for setup
as_tibble(data_5ite)                                                
new_data <- add_column(data_5ite, iteration = rep(1:5, each = 26))  # number of model iterations 5, 26 years per iteration #tibble
new_data1 <- add_column(new_data, year = rep(c(1:26), 5 ))  # 26 years per iteration

#-------------------------------------------------------------------------------
# Plot lambda (Figure 3 MethodsX article; Figure 4 Ecological Modelling article)
#-------------------------------------------------------------------------------
p1 <- ggplot(new_data1, aes(x = year, y = lambda, col = factor(iteration))) + geom_smooth() + scale_y_continuous(breaks=seq(0.95, 1.05, 0.05), limits=c(0.9, 1.10))
p2 <- p1 + scale_x_continuous(limits = c(2,26), breaks = c(5,15,25)) + theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+ theme(axis.text.x = element_text(size=14),axis.title.x = element_text(size= 16)) + theme(axis.text.y = element_text(size = 14),axis.title.y = element_text(size= 16))
p3 <- p2 + geom_vline(xintercept = 15, color="black", linetype = 3, size = .1)
p4 <- p3 + theme(plot.title = element_text(size=12,face="bold")) + theme(text=element_text(face="bold", size=12)) + theme(legend.position = "none")
p4

#-------------------------------------------------------------------------------
# AgeSexComposition using data from one MOOvPOP iteration
# (Figure 4 MethodsX article; Figure 3 Ecological Modelling article)
#-------------------------------------------------------------------------------
agesexdata_1 <- subset(new_data1, iteration == 5)              # choose which iteration
agesexdata_1$ffp <- agesexdata_1$posth_ff / agesexdata_1$posth_total
agesexdata_1$mfp <- agesexdata_1$posth_mf / agesexdata_1$posth_total
agesexdata_1$fyp <- agesexdata_1$posth_fy / agesexdata_1$posth_total
agesexdata_1$myp <- agesexdata_1$posth_my / agesexdata_1$posth_total
agesexdata_1$fap <- agesexdata_1$posth_fa / agesexdata_1$posth_total
agesexdata_1$map <- agesexdata_1$posth_ma / agesexdata_1$posth_total
agesex_data <- subset(agesexdata_1, select = year: map)
data_long <- gather(agesex_data, age_sex, prop, ffp:map, factor_key=TRUE)
data_long <- data_long[order(data_long$year, data_long$age_sex), ]
p5 <- ggplot(data=data_long,aes(x = year, y = prop, fill = age_sex))
p6 <- p5 + geom_area(color="black", size=.2, alpha=.8) + theme(legend.title=element_blank())
p7 <- p6 + scale_fill_discrete(name = "Age-sex class",labels = c(" Fawn females"," Fawn males"," Yearling females"," Yearling males"," Adult females"," Adult males"))
p8 <- p7 + ylab("Proportion\n")  + xlab("Year") + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + theme(axis.title.y = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 12),axis.text.x = element_text(size = 12))
p8

#-------------------------------------------------------------------------------
# Data from 100 MOOvPOP iterations
#-------------------------------------------------------------------------------
data_github <- "https://raw.githubusercontent.com/anyadoc/FranklinCWDsurveillance_Rcode/master/saFranklinCounty_100.csv"
model_data <- read_csv(data_github)
as_tibble(model_data)
new_data2 <- add_column(model_data, year = rep(c(1:26), 100))  # number of years per iteration 26, model iterations 100 #tibble
new_data3 <- add_column(new_data2, iteration = rep(c(1:100), each = 26))
new_data4 <- subset(new_data3, year == 26)

#-------------------------------------------------------------------------------
# Compare MOOvPOP derived abundance with MDC estimate (Figure 5 MethodsX article)
#-------------------------------------------------------------------------------
MDC_est <- rnorm(100,26502,1325)  #abundance data generated based on MDC's estimate +- 5%SD
MOOvPOP <- new_data4$`pre-harvest_total` 
iteration <- c(1:100)
mydata3 <- cbind(MDC_est,MOOvPOP,iteration)
mydata_df <- as.data.frame(mydata3)
head(mydata_df)
mydata3_long <- gather(mydata_df, data_source, deer_abundance, MDC_est:MOOvPOP, factor_key=TRUE)
head(mydata3_long)
comp_N_Frank <- ggplot(mydata3_long, aes(x=iteration, y=deer_abundance, color=data_source)) + geom_point(size=2, alpha=0.5, position = "jitter") + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + stat_ellipse(geom = "polygon", alpha = 0.1, type = "t", aes(fill = data_source)) + scale_x_continuous(breaks=c(100)) + xlab("iterations") + ylab("deer abundance\n") + theme_cowplot(12)
comp_N_Frank

#-------------------------------------------------------------------------------
# Assess age-structure and sex ratio for MOOvPOP derived populations
#-------------------------------------------------------------------------------

# Age structure using data(new_data4)
# Adult proportion
mean(new_data4[["adult_prop"]])
min(new_data4[["adult_prop"]])
max(new_data4[["adult_prop"]])
# Yearling proportion
mean(new_data4[["yearling_prop"]])
min(new_data4[["yearling_prop"]])
max(new_data4[["yearling_prop"]])
# Fawn proportion
mean(new_data4[["fawn_prop"]])
min(new_data4[["fawn_prop"]])
max(new_data4[["fawn_prop"]])
# female: male ratio
mean(new_data4[["f:m_ratio"]])
min(new_data4[["f:m_ratio"]])
max(new_data4[["f:m_ratio"]])
#-------------------------------------------------------------------------------

# Age class proportion
Ageclass = c("Fawns", "Yearlings", "Adults")
Proportion = c(round(mean(new_data4[["fawn_prop"]]),digits=2),round(mean(new_data4[["yearling_prop"]]),digits=2),round(mean(new_data4[["adult_prop"]]),digits=2))
min = c(min(new_data4[["fawn_prop"]]),min(new_data4[["yearling_prop"]]),min(new_data4[["adult_prop"]]))
max = c(max(new_data4[["fawn_prop"]]),max(new_data4[["yearling_prop"]]),max(new_data4[["adult_prop"]]))
age.prop.df = data.frame(Ageclass,Proportion,min,max)
grid.table(age.prop.df)