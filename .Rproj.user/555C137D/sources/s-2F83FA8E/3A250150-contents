# install.packages("data.table")
# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("doParallel")
# install.packages("foreach")
# install.packages("kernlab")
# install.packages("npreg")
# install.packages("psych")
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(doParallel)
library(foreach)
library(kernlab)
library(npreg)
library(psych)


## to unregister the cl
ncore = 8
registerDoSEQ()
cl <- makeCluster(ncore, type = "SOCK")
registerDoParallel(cl, cores = ncore)

# rbfdot(sigma = 1)

#### Get the Prepared Data ####
response = 'New.case'
source("ANM DATA PREP.R")
source("ANM.causal.test.R")
# set.seed(12)

#### Define the p-value vectors of the Hypo tests ####
quarter.1.pvalue = rep(0,6)
quarter.2.pvalue = rep(0,6)
quarter.3.pvalue = rep(0,6)
quarter.4.pvalue = rep(0,6)


#### run the causal test X -> Y on based ANM for 1st quarter ####
# X is Pop density, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM1[-c(2,11),2]; X = ANM1[-c(2,11),8];
quarter.1.pvalue[1] = ANM.causal.test(X,Y)

# X is % of male, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM1[-c(2,11),2]; X = ANM1[-c(2,11),7];
quarter.1.pvalue[2] = ANM.causal.test(X,Y)

# X is median income, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM1[-c(2,11),2]; X = ANM1[-c(2,11),4];
quarter.1.pvalue[3] = ANM.causal.test(X,Y)

# X is % of seniors, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM1[-c(2,11),2]; X = ANM1[-c(2,11),3];
quarter.1.pvalue[4] = ANM.causal.test(X,Y)

# X is % of Blacks, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM1[-c(2,11),2]; X = ANM1[-c(2,11),5];
quarter.1.pvalue[5] = ANM.causal.test(X,Y)

# X is % of Latinos, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM1[-c(2,11),2]; X = ANM1[-c(2,11),6];
quarter.1.pvalue[6] = ANM.causal.test(X,Y)


#### run the causal test X -> Y on based ANM for 2nd quarter ####
# X is Pop density, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM2[-c(2,11),2]; X = ANM2[-c(2,11),8];
quarter.2.pvalue[1] = ANM.causal.test(X,Y)

# X is % of male, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM2[-c(2,11),2]; X = ANM2[-c(2,11),7];
quarter.2.pvalue[2] = ANM.causal.test(X,Y)

# X is median income, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM2[-c(2,11),2]; X = ANM2[-c(2,11),4];
quarter.2.pvalue[3] = ANM.causal.test(X,Y)

# X is % of seniors, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM2[-c(2,11),2]; X = ANM2[-c(2,11),3];
quarter.2.pvalue[4] = ANM.causal.test(X,Y)

# X is % of Blacks, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM2[-c(2,11),2]; X = ANM2[-c(2,11),5];
quarter.2.pvalue[5] = ANM.causal.test(X,Y)

# X is % of Latinos, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM2[-c(2,11),2]; X = ANM2[-c(2,11),6];
quarter.2.pvalue[6] = ANM.causal.test(X,Y)


#### run the causal test X -> Y on based ANM for 3rd quarter ####
# X is Pop density, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM3[-c(2,11),2]; X = ANM3[-c(2,11),8];
quarter.3.pvalue[1] = ANM.causal.test(X,Y)

# X is % of male, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM3[-c(2,11),2]; X = ANM3[-c(2,11),7];
quarter.3.pvalue[2] = ANM.causal.test(X,Y)

# X is median income, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM3[-c(2,11),2]; X = ANM3[-c(2,11),4];
quarter.3.pvalue[3] = ANM.causal.test(X,Y)

# X is % of seniors, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM3[-c(2,11),2]; X = ANM3[-c(2,11),3];
quarter.3.pvalue[4] = ANM.causal.test(X,Y)

# X is % of Blacks, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM3[-c(2,11),2]; X = ANM3[-c(2,11),5];
quarter.3.pvalue[5] = ANM.causal.test(X,Y)

# X is % of Latinos, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM3[-c(2,11),2]; X = ANM3[-c(2,11),6];
quarter.3.pvalue[6] = ANM.causal.test(X,Y)

#### run the causal test X -> Y on based ANM for 4th quarter ####
# X is Pop density, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM4[-c(2,11),2]; X = ANM4[-c(2,11),8];
quarter.4.pvalue[1] = ANM.causal.test(X,Y)

# X is % of male, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM4[-c(2,11),2]; X = ANM4[-c(2,11),7];
quarter.4.pvalue[2] = ANM.causal.test(X,Y)

# X is median income, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM4[-c(2,11),2]; X = ANM4[-c(2,11),4];
quarter.4.pvalue[3] = ANM.causal.test(X,Y)

# X is % of seniors, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM4[-c(2,11),2]; X = ANM4[-c(2,11),3];
quarter.4.pvalue[4] = ANM.causal.test(X,Y)

# X is % of Blacks, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM4[-c(2,11),2]; X = ANM4[-c(2,11),5];
quarter.4.pvalue[5] = ANM.causal.test(X,Y)

# X is % of Latinos, Y is avg new cases
# Only consider the continental 48 states, remove HI and AK
Y = ANM4[-c(2,11),2]; X = ANM4[-c(2,11),6];
quarter.4.pvalue[6] = ANM.causal.test(X,Y)

#### put the final results together ####
ANM.results.pvalue = cbind(quarter.1.pvalue, quarter.2.pvalue, quarter.3.pvalue, quarter.4.pvalue)
rownames(ANM.results.pvalue) = c('Population Density','Percent of Males','Median Income',
                                 'Percent of Seniors','Percent of African Americans','Percent of Hispanic Americans')
colnames(ANM.results.pvalue) = c('quarter 1','quarter 2', 'quarter 3', 'quarter 4')
print(ANM.results.pvalue)
