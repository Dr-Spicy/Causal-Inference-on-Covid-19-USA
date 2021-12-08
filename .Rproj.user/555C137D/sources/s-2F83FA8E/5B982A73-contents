library(data.table)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(doParallel)
library(foreach)
library(timeSeries)
library(vars)
library(RTransferEntropy)



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


#### ET ####
future::plan("multisession")
# detach(package:data.table, unload = T)

shannon.p2.x2y = matrix(0, nrow = 51, ncol = M-1)
rownames(shannon.p2.x2y) = c("USA",unique(Google.mob$sub_region_1)[-c(1,10)])
colnames(shannon.p2.x2y) = colnames(AL)[-1]

shannon.p2.y2x = matrix(0, nrow = 51, ncol = M-1)
rownames(shannon.p2.y2x) = c("USA",unique(Google.mob$sub_region_1)[-c(1,10)])
colnames(shannon.p2.y2x) = colnames(AL)[-1]

for (i in 1:51) {
    for (j in 2:M) {
        data = temporal[[i]]
        # Construct the causality model from the second column to the first one,
        # with a lag equal to 2, and without taking into account stationarity
        if (i == 1){
            if (j %in% c(3,9,10,12)){
                model = transfer_entropy(x= data[,j], y=data[,1], lx = 2, ly = 2, shuffles = 200, quantiles = c(7.5, 92.5), seed = 127, burn = 50, nboot= 1000)
                shannon.p2.x2y[i,j-1] = model$coef[1,4]
                shannon.p2.y2x[i,j-1] = model$coef[2,4]
            } else {
                model = transfer_entropy(x= data[,j], y=data[,1], lx = 2, ly = 2, shuffles = 200, quantiles = c(5, 95), seed = 127, burn = 50, nboot= 1000)
                shannon.p2.x2y[i,j-1] = model$coef[1,4]
                shannon.p2.y2x[i,j-1] = model$coef[2,4]
            }
        } else  {
           
            model = transfer_entropy(x= data[,j], y=data[,1], lx = 2, ly = 2, shuffles = 200, quantiles = c(5, 95), seed = 127, burn = 50, nboot= 1000)
            shannon.p2.x2y[i,j-1] = model$coef[1,4]
            shannon.p2.y2x[i,j-1] = model$coef[2,4]
            
        }
        

    }
}


renyi.p2.x2y = matrix(0, nrow = 51, ncol = M-1)
rownames(renyi.p2.x2y) = c("USA",unique(Google.mob$sub_region_1)[-c(1,10)])
colnames(renyi.p2.x2y) = colnames(AL)[-1]

renyi.p2.y2x = matrix(0, nrow = 51, ncol = M-1)
rownames(renyi.p2.y2x) = c("USA",unique(Google.mob$sub_region_1)[-c(1,10)])
colnames(renyi.p2.y2x) = colnames(AL)[-1]

for (i in 1:51) {
    for (j in 2:M) {
        data = temporal[[i]]
        if (i == 1){
            if (j %in% c(3,9,10,12)){
                model = transfer_entropy(x= data[,j], y=data[,1], lx = 2, ly = 2, shuffles = 200,entropy = 'Renyi', q=0.1, quantiles = c(7.5, 92.5), seed = 127, burn = 50, nboot= 1000)
                renyi.p2.x2y[i,j-1] = model$coef[1,4]
                renyi.p2.y2x[i,j-1] = model$coef[2,4]
            } else {
                # Construct the causality model from the second column to the first one,
                # with a lag equal to 2, and without taking into account stationarity
                model = transfer_entropy(x= data[,j], y=data[,1], lx = 2, ly = 2, shuffles = 200, entropy = 'Renyi', q=0.1,quantiles = c(5, 95), seed = 127, burn = 50, nboot= 1000)
                renyi.p2.x2y[i,j-1] = model$coef[1,4]
                renyi.p2.y2x[i,j-1] = model$coef[2,4]
            }
        } else {
            # Construct the causality model from the second column to the first one,
            # with a lag equal to 2, and without taking into account stationarity
            model = transfer_entropy(x= data[,j], y=data[,1], lx = 2, ly = 2, shuffles = 200, entropy = 'Renyi', q=0.1,quantiles = c(7.5, 92.5), seed = 127, burn = 50, nboot= 1000)
            renyi.p2.x2y[i,j-1] = model$coef[1,4]
            renyi.p2.y2x[i,j-1] = model$coef[2,4]
        }    
    }
}
