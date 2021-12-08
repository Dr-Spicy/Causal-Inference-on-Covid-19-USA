


ANM.causal.test <- function(X, Y){
    # B is the number of repetition, N.p is the bootstrap number to construct the emperical distribution of test stat
    B = 100; p.value = rep(0,B);    N.p = 200; 
    p.value <- foreach(j = 1:B, .packages = c('kernlab','npreg','psych'), .combine = 'c') %dopar%{
        ## Step 1: Partion the train and test 
        N = length(Y); n = 45; m = 3
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
        mdl.yxpop = ss(y = Y.train, x = X.train, method = 'GCV', penalty = 5)
        mdl.xypop = ss(y = X.train, x = Y.train, method = 'GCV', penalty = 5)
        # rsdl.yxpop = Y.train - predict(mdl.yxpop, X.train)$y
        # rsdl.xypop = X.train - predict(mdl.xypop, Y.train)$y
        
        ## Step 3: Fits on the test and obtain the residual hat
        rsdlhat.yxpop = Y.test - predict(mdl.yxpop, X.test)$y
        rsdlhat.xypop = X.test - predict(mdl.xypop, Y.test)$y
        
        
        ## Step 4: Calculate the dependence measure HSICs with guassian kernel
        # H.n = diag(n) - matrix(1/n, n, n)
        H.m = diag(m) - matrix(1/m, m, m)
        
        HSIC.yxpop = 1/(m^2)*tr(
            kernelMatrix(kernel = rbfdot(sigma =1), x = X.test) %*%
                H.m %*%
                kernelMatrix(kernel = rbfdot(sigma =1), x = rsdlhat.yxpop)
        )
        HSIC.xypop = 1/(m^2)*tr(
            kernelMatrix(kernel = rbfdot(sigma =1), x = Y.test) %*%
                H.m %*%
                kernelMatrix(kernel = rbfdot(sigma =1), x = rsdlhat.xypop)
        )
        Tc.choose = abs(HSIC.yxpop - HSIC.xypop)
        
        ## Step 5: Get the emperical dist of Tc by permutation
        
        Tc.perm = rep(0,N.p); 
        # i = 1
        for (i in 1:N.p) {
            id_test.prime = sample(id_test)
            Y.test.prime = Y[id_test.prime]
            rsdlhat.yxpop = Y.test.prime - predict(mdl.yxpop, X.test)$y
            rsdlhat.xypop = X.test - predict(mdl.xypop, Y.test.prime)$y
            HSIC.yxpop = 1/(m^2)*tr(
                kernelMatrix(kernel = rbfdot(sigma =1), x = X.test) %*%
                    H.m %*%
                    kernelMatrix(kernel = rbfdot(sigma =1), x = rsdlhat.yxpop)
            )
            HSIC.xypop = 1/(m^2)*tr(
                kernelMatrix(kernel = rbfdot(sigma =1), x = Y.test.prime) %*%
                    H.m %*%
                    kernelMatrix(kernel = rbfdot(sigma =1), x = rsdlhat.xypop)
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
    mean(p.value)
    return(mean(no_outlier))
}



# Only consider the continental 48 states, remove HI and AK
Y = ANM1[-c(2,11),2]; X = ANM1[-c(2,11),8];



