#setwd ("C:/Users/belsare1/OneDrive/Documents/Research_AVB/Postdoc_MU/CWD/CWD_MS_Oct2016/Data for plots and analysis/use in MS/Final_MS/CWD MS June18/ABelsare_22June19")
require (dplyr)
require (ggplot2)
require (tidyverse)
require(tidyr)
data <- read.csv("deerpopdyFranklinCounty_5iterations.csv", header = TRUE)
data = data[-1,]
as_tibble(data)                                                # deletes the very 1st row, which is the user provided information for setup
new_data <- add_column(data, iteration = rep(1:5, each = 26))  # number of model iterations 5, number of years per iteration - 26 #tibble
new_data1 <- add_column(new_data, year = rep(c(1:26), 5 ))     # 26 years per iteration

#AgeSexComposition using one iteration
agesexdata_1 <- subset(new_data1, iteration == 5)              # choose which iteration
agesexdata_1$mfp <- agesexdata_1$posth_mf / agesexdata_1$posth_total
agesexdata_1$ffp <- agesexdata_1$posth_ff / agesexdata_1$posth_total
agesexdata_1$myp <- agesexdata_1$posth_my / agesexdata_1$posth_total
agesexdata_1$fyp <- agesexdata_1$posth_fy / agesexdata_1$posth_total
agesexdata_1$map <- agesexdata_1$posth_ma / agesexdata_1$posth_total
agesexdata_1$fap <- agesexdata_1$posth_fa / agesexdata_1$posth_total
agesex_data <- subset(agesexdata_1, select = year: fap)
data_long <- gather(agesex_data, age_sex, prop, mfp:fap, factor_key=TRUE)
data_long <- data_long[order(data_long$year, data_long$age_sex), ]
p1 <- ggplot(data=data_long,aes(x = year, y = prop, fill = age_sex))
p2 <- p1 + geom_area(color="black", size=.2, alpha=.8) + theme(legend.title=element_blank())
p3 <- p2 + scale_fill_discrete(name = "Age-sex class",labels = c(" Fawn females"," Fawn males"," Yearling females"," Yearling males"," Adult females"," Adult males"))
p4 <- p3 + ylab("Proportion\n")  + xlab("Year") + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + theme(axis.title.y = element_text(size = 12),axis.text.y = element_text(size = 12))+ theme(axis.title.x = element_text(size = 12),axis.text.x = element_text(size = 12))
FranklinAgeSex <- p4
#ggsave("FranklinAgeSex.png")                                  #uncomment to save the plot as a .png file. 

##lambda
p1 <- ggplot(new_data1, aes(x = year, y = lambda, col = factor(iteration))) + geom_smooth() + scale_y_continuous(breaks=seq(0.95, 1.05, 0.05), limits=c(0.9, 1.10))
p2 <- p1 + scale_x_continuous(limits = c(2,26), breaks = c(5,15,25)) + theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+ theme(axis.text.x = element_text(size=14),axis.title.x = element_text(size= 16)) + theme(axis.text.y = element_text(size = 14),axis.title.y = element_text(size= 16))
p3 <- p2 + geom_vline(xintercept = 15, color="black", linetype = 3, size = .1)
p4 <- p3 + theme(plot.title = element_text(size=12,face="bold")) + theme(text=element_text(face="bold", size=12)) + theme(legend.position = "none")
Franklinlambda <- p4
#ggsave("Franklinlambda.png")                                  #uncomment to save the plot as a .png file. 