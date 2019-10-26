require (tidyr)
require (tidyverse)
require (dplyr)
require (cowplot)
require (tibble)
require (gridExtra)
model_data <- read.csv("saFranklinCounty_100.csv", header = TRUE)
as_tibble(model_data)
new_data <- add_column(model_data, year = rep(c(1:26), 100))  # number of years per iteration 26, model iterations 100 #tibble
new_data1 <- add_column(new_data, iteration = rep(c(1:100), each = 26))
new_data2 <- subset(new_data1, year == 26)

#Compare abundance
MDC_est <- rnorm(100,26502,1325)                           #abundance data generated based on MDC's estimate +- 5%SD
MOOvPOP <- new_data2$pre.harvest_total #model_data$pre.harvest_total
iteration <- c(1:100)
mydata3 <- cbind(MDC_est,MOOvPOP,iteration)
mydata_df <- as.data.frame(mydata3)
head(mydata_df)
mydata3_long <- gather(mydata_df, data_source, deer_abundance, MDC_est:MOOvPOP, factor_key=TRUE)
head(mydata3_long)
comp_N_Frank <- ggplot(mydata3_long, aes(x=iteration, y=deer_abundance, color=data_source)) + geom_point(size=2, alpha=0.5, position = "jitter") + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + stat_ellipse(geom = "polygon", alpha = 0.1, type = "t", aes(fill = data_source)) + scale_x_continuous(breaks=c(100)) + xlab("iterations") + ylab("deer abundance\n") + theme_cowplot(12)
ggsave("comp_N_Frank.png")                                   #uncomment to save the plot as a .png file. 

#Age structure 
#data(new_data2)
#Adult proportion
mean(new_data2[["adult_prop"]])
min(new_data2[["adult_prop"]])
max(new_data2[["adult_prop"]])
#Yearling proportion
mean(new_data2[["yearling_prop"]])
min(new_data2[["yearling_prop"]])
max(new_data2[["yearling_prop"]])
#Fawn proportion
mean(new_data2[["fawn_prop"]])
min(new_data2[["fawn_prop"]])
max(new_data2[["fawn_prop"]])
#sex ratio
mean(new_data2[["f.m_ratio"]])
min(new_data2[["f.m_ratio"]])
max(new_data2[["f.m_ratio"]])

#barchart age class proportion
Ageclass = c("Fawns", "Yearlings", "Adults")
Proportion = c(round(mean(new_data2[["fawn_prop"]]),digits=2),round(mean(new_data2[["yearling_prop"]]),digits=2),round(mean(new_data2[["adult_prop"]]),digits=2))
min = c(min(new_data2[["fawn_prop"]]),min(new_data2[["yearling_prop"]]),min(new_data2[["adult_prop"]]))
max = c(max(new_data2[["fawn_prop"]]),max(new_data2[["yearling_prop"]]),max(new_data2[["adult_prop"]]))
age.prop.df = data.frame(Ageclass,Proportion,min,max)
grid.table(age.prop.df)