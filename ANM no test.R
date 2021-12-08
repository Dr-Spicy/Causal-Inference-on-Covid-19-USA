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



#### Get the Prepared Data ####
source("ANM DATA PREP.R")



set.seed(1)

# Only consider the continental 48 states, remove HI and AK
Y = ANM1[-c(2,11),2]; X = ANM1[-c(2,11),8];
# Y = ANM1[,2]; X = ANM1[,7];
B = 100; p.value = rep(0,B);

p.value <- foreach(j = 1:B, .packages = c('kernlab','npreg','psych'), .combine = 'c') %dopar%{
    ## Step 1: Partion the train and test 
    N = length(Y); n = 1*N; m = 0*N
    id_train = sample(seq(1,N), n)
    id_test = setdiff(seq(1,N), id_train)
    
    ## Step 2: NP regression on train one by one the X variable
    
    #2.1a. regress Y on X(pop dens)
    #2.1b. regress X(pop dens) on Y
    Y.train = Y[id_train]; Y.test = Y[id_test];
    X.train = X[id_train]; X.test = X[id_test];
    
    #2.2a. regress Y on X(income)
    #2.2b. regress X(income) on Y
    
    #2.3a. regress Y on X(black)
    #2.3b. regress X(black) on Y
    
    #2.4a. regress Y on X(latin)
    #2.4b. regress X(latin) on Y
    
    #2.5a. regress Y on X(male)
    #2.5b. regress X(male) on Y
    
    #2.6a. regress Y on X(senior)
    #2.6b. regress X(senior) on Y
    mdl.yxpop = ss(y = Y.train, x = X.train, method = 'GCV')
    mdl.xypop = ss(y = X.train, x = Y.train, method = 'GCV')
    # rsdl.yxpop = Y.train - predict(mdl.yxpop, X.train)$y
    # rsdl.xypop = X.train - predict(mdl.xypop, Y.train)$y
    
    ## Step 3: Fits on the test and obtain the residual hat
    rsdl.yxpop = Y.train - predict(mdl.yxpop, X.train)$y
    rsdl.xypop = X.train - predict(mdl.xypop, Y.train)$y
    
    
    ## Step 4: Calculate the dependence measure HSICs with guassian kernel
    # H.n = diag(n) - matrix(1/n, n, n)
    H.n = diag(n) - matrix(1/n, n, n)
    
    HSIC.yxpop = 1/(n^2)*tr(
        kernelMatrix(kernel = rbfdot(sigma = 1), x = X.train) %*%
            H.n %*%
            kernelMatrix(kernel = rbfdot(sigma = 1), x = rsdl.yxpop)
    )
    HSIC.xypop = 1/(n^2)*tr(
        kernelMatrix(kernel = rbfdot(sigma = 1), x = Y.train) %*%
            H.n %*%
            kernelMatrix(kernel = rbfdot(sigma = 1), x = rsdl.xypop)
    )
    Tc.choose = abs(HSIC.yxpop - HSIC.xypop)
    
    ## Step 5: Get the emperical dist of Tc by permutation
    
    N.p = 200; 
    Tc.perm = rep(0,N.p); 
    # i = 1
    for (i in 1:N.p) {
        id_train.prime = sample(id_train)
        Y.train.prime = Y[id_train.prime]
        rsdl.yxpop = Y.train.prime - predict(mdl.yxpop, X.train)$y
        rsdl.xypop = X.train - predict(mdl.xypop, Y.train.prime)$y
        HSIC.yxpop = 1/(n^2)*tr(
            kernelMatrix(kernel = rbfdot(sigma = 1), x = X.train) %*%
                H.n %*%
                kernelMatrix(kernel = rbfdot(sigma = 1), x = rsdl.yxpop)
        )
        HSIC.xypop = 1/(n^2)*tr(
            kernelMatrix(kernel = rbfdot(sigma = 1), x = Y.train.prime) %*%
                H.n %*%
                kernelMatrix(kernel = rbfdot(sigma = 1), x = rsdl.xypop)
        )
        Tc.perm[i] = abs(HSIC.yxpop - HSIC.xypop)
    }
    
    p.value = sum(Tc.perm > Tc.choose)/N.p
    p.value
}

hist(p.value)
Q = quantile(p.value) 
IQR = Q[4]- Q[2]
up = Q[4] + 1.5*IQR; low = Q[2] - 1.5*IQR;
no_outlier = p.value[(p.value >= low)&(p.value <= up)]
mean(no_outlier)
