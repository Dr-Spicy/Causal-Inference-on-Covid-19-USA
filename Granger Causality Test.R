library(data.table)
library(lubridate)
library(tidyverse)
library(dplyr)
library(doParallel)
library(foreach)
library(timeSeries)
library(NlinTS)
library(vars)
library(stargazer)


# ## to unregister the cl
# ncore = 8
# registerDoSEQ()
# cl <- makeCluster(ncore, type = "SOCK")
# registerDoParallel(cl, cores = ncore)

# rbfdot(sigma = 1)

#### Get the Prepared Data ####
# response = "new.case"
# response = "cumsum.new.cases"
response = "New.case.1000.people"
# response = "cumsum.new.cases.1000.people"
source("Temporal bivariate test prep.R")

set.seed(12)
M = ncol(USA)

#### linear Granger Test ####

# p = 1 #(lag)
# 
# GT.l.p1 = matrix(0, nrow = 51, ncol = M-1)
# rownames(GT.l.p1) = unique(Google.mob$sub_region_1)[-c(1,10)]
# colnames(GT.l.p1) = colnames(AL)[-1]
# 
# for (i in 1:51) {
#     for (j in 2:M) {
#         data = temporal[[i]]
#         # Construct the causality model from the second column to the first one,
#         # with a lag equal to 2, and without taking into account stationarity
#         model = causality.test (data[,1], data[,j], p, T) # last para is logical for making stationary or not
#         GT.l.p1[i,j-1] = model$pvalue
#     }
# }
# GT.l.p1.sig = GT.l.p1<0.00341

p = 2 #(lag)

GT.l.p2.x2y = matrix(0, nrow = 51, ncol = M-1)
rownames(GT.l.p2.x2y) = c("USA",unique(Google.mob$sub_region_1)[-c(1,10)])
colnames(GT.l.p2.x2y) = colnames(AL)[-1]

for (i in 1:51) {
    for (j in 2:M) {
        data = temporal[[i]]
        # Construct the causality model from the second column to the first one,
        # with a lag equal to 2, and without taking into account stationarity
        model = causality.test (data[,1], data[,j], p, F) # last para is logical for making stationary or not
        GT.l.p2.x2y[i,j-1] = model$pvalue
    }
}

GT.l.p2.y2x = matrix(0, nrow = 51, ncol = M-1)
rownames(GT.l.p2.y2x) = c("USA",unique(Google.mob$sub_region_1)[-c(1,10)])
colnames(GT.l.p2.y2x) = colnames(AL)[-1]

for (i in 1:51) {
    for (j in 2:M) {
        data = temporal[[i]]
        # Construct the causality model from the second column to the first one,
        # with a lag equal to 2, and without taking into account stationarity
        model = causality.test (data[,j], data[,1], p, F) # last para is logical for making stationary or not
        GT.l.p2.y2x[i,j-1] = model$pvalue
    }
}

stargazer(GT.l.p2.x2y, type = 'text')
stargazer(GT.l.p2.y2x, type = 'text')