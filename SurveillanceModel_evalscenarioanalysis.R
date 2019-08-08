require(matrixStats)
data <- read.csv("CWDsurveillanceMO_sc12.csv", header = TRUE)   #file name for the scenario to be analyzed
obsmean <- by(observed, list(gr=as.numeric(gl(nrow(data), 100, nrow(data)))), 
              FUN = function(x) colMeans(as.matrix(x)))
theoretical = 0.99                                             #confidence in detecting disease or detection probability: 0.9/0.95/0.99
t.test(obsmean,
       mu = theoretical,
       conf.int=0.95)