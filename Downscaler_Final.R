########################################################################################
#
#  Space-Time Statistical Downscaler for Gridded Model Ouputs
#               and Space-time Predictors
#
#                   Howard H. Chang
#                 howard.chang@emory.edu
#
#                   July 10, 2014
#
# Reference:
# Calibrating MODIS Aerosol Optical Depth for Predicting Daily
# Concentrations via Statistical Downscaling. 
# Chang HH, Hu X, and Liu Y (2014), 
# Journal of Exposure Science & Environmental Epidemiology 24, 398-404
#
#
#
# MODEL:
#
# ### Note that multiple monitors may share the same gridded value
# Y(s,t) = PM2.5 level on day t at monitor s
# X(s,t) = AOD value on day t linked to monitor s
# Z(s,t) = design matrix for fixed effects. First column = 1 (intercpet), Second column = AOD
#
# Index s = 1, 2, ..., S and t= 1, 2, ..., T
#
# Y(s,t) = alpha1(s) + alpha2(s)X(s,t) + beta(t) + beta(t)X(s,t) + gamma*Z(s,t) + epsilon (s,t)
#
# Spatially varying intercpets:
# alpha1(s) = a11w1(s)
#
# Spatially varying slopes:
# alpha2(s) = a21w1(s) + a22w2(s)
#
# w1 and w2 are mean-zero Gaussian spatial processes with tapered covariance matrices.
#  cov [w1(s), w1(s')] = exp(-d(s,s')/rho1)*Wendland(range1)
#  cov [w2(s), w2(s')] = exp(-d(s,s')/rho2)*Wendland(range2)
#
# beta1 and beta2 are independent first-order random walk proceses:
# beta1(t) ~ Normal ( psi1*(beta1(t-1)+beta1(t+1))/2, tau1)
# beta2(t) ~ Normal ( psi2*(beta2(t-1)+beta2(t+1))/2, tau2)
#
# gamma is a vector of land use and meteorology covariates.
#
# epsilon(s,t) ~iid~ Normal (0, sigma2)
#
#
# DEFAULT PRIORS:
# These are chosen to be rather uninformative. You might consider changing the priors
# on tau1, tau2, and A
#
#
# gamma ~ flat
# A[1,1], A[2,2] ~  inv-gamma(0.001, 0.001)
# A[1,2] ~ N( 0, 1000)
# rho1, rho2 ~ gamma (0.5, 0.05)
# tau1, tau2 ~ inv-gamma (0.5, 0.005)
# psi1, psi2 ~ discretized over [0,1]
# sigma2 ~ inv-gamma (0.001, 0.001)
#
########################################################

##################################################
### Function to fit the Bivariate  downscaler
##################################################
###
###############
### INPUTS  ###
###############
### DATA
### Y = n x 1 PM2.5 vector where n is the total number of linked pm2.5 and AOD pairs
### X = n x 1 AOD vector (gridded proxy data to be calibrated)
### Z = n x p matrix where p is the number of land use and met predictors

### SPACE-TIME INDEX
### Space.ID = n x 1 vector of integers (index for locations); must be from 1,...,S
### Day.ID = n x 1 vector of integers (index fo days); must be from 1,...,N and skip index on days with no linked data pairs
### Dist.mat = S x S matrix of pairwise Euclidean distance where S is the total number of locations
### Mon.coord = S x 2 matrix for the (x,y) coordinates of monitor locations (to be used in Euclidean distance computation)

### MCMC ITERATIONS
### n.iter = number of "total" MCMC interations
### burn = number of initial burn "discarded" interations
### thin = i; save only the ith MCMC interation after burning
### save.alpha = should samples of spatial random effects be saved? This can be very large.
###  *** Must have save.alpha == TRUE if you want to do spatial interpolation ***
### save.beta = should samples of temporal random effects be saved? This can be very large.


### SPATIAL COVARIANCE FUNCTION
### taper = use tapered exponential spatial distance correltion function?
### range1, range2 = range of the tapered function for w1(s) and w2(s)
### Find range by first running the model with taper = FALSE

### TUNING PARAMETERS
### A21.tune = tuning parameter for Metropolis-Hastings for A[2,1]
### rho1.tune, rho2.tune = tuning parameter for Metropolis-Hastings for the spatial decay parameter
### The above parameters should be tuned to have acceptance rates of 50%.

### PRIORS
###
### A[1,1] ~ Gamma (A1.a, A1.b)
### A[2,2] ~ Gamma (A2.a, A2.b)
### A[2,1] ~ N (0, A.sd^2)
### rho1 ~ Gamma (rho1.a, rho1.b)
### rho2 ~ Gamma (rho2.a, rho2.b)
### tau1 ~ Gamma (t1.a, t1.b)
### tau2 ~ Gamma (t2.a, t2.b )
### sigma2 ~ Gamma (sigma.a, sigma.b)

###############
### OUTPUT  ###
###############
###
### use the "$" to extract outputs
### K = (n.iter - burn)/thin = total number of posterior samples

### A = 2 by 2 by K array for A
### alpha = S by 2 matrix; Point estimates of the spatial random effects [intercept, AOD].
### beta = T by 2 matrix; Point estimates of the temporal random effects [intercept, AOD].
### alpha.sd = S by 2 matrix; Standard errors for alpha estimates.
### beta.sd = T by 2 matrix; Standard errors for beta estimates.
### sigma = K by 1 vector of sigma2 posterior samples.
### gamma = K by (p + 2) matrix of posterior samples for fixed effects [intercept, AOD, LU+Met].

### Y = N by 1 vector of fitted values
### Y.sd = N by 1 vector of the standard errors of the fitted values

### tau = K by 2 matrix; posterior samples of [tau1, tau2]
### rho = K by 2 matrix; posterior samples of [rho1, rho2]
### psi = K by 2 matrix; posterior samples of [psi1, psi2]

### dev = K by 1 vector of deviance
### pD = effective degrees of freedom
### DIC = deviance information criterion

### A.acc = accpetance rates for A[2,1]
### rho.acc = acceptance rates for rho1, and rho2

## alpha.keep = S by 2 by K array of the posterior samples of spatial random effects
## beta.keep = T by 2 by K array of the posterior samples of temporal random effects
## Mon.coord = save spatial locations used
## taper.info = save taper ranges

########################################################
########################################################
########################################################

### Load the packages below
library (sp)
library (gstat)
library (splines)
library (lme4)
library (spam)
library (mvtnorm)
library (fields)

DownScaler = function(Y, X, Z, Dist.mat, Space.ID, Day.ID, Mon.coord,
                      n.iter = 50000, burn = 5000, thin = 5,
                      A.sd = 1000,
                      A21.tune = 0.4,  rho1.tune=0.4, rho2.tune = 0.3,
                      range1 = 100, range2 = 250,
                      rho1.a = 5, rho1.b = 0.05, rho2.a = 5, rho2.b = 0.05,
                      t1.a = 0.5, t1.b = 0.005,
                      t2.a = 0.5, t2.b = 0.005,
                      A1.a = 0.001, A1.b = 0.001,
                      A2.a = 0.001, A2.b = 0.001,
                      sigma.a = 0.001, sigma.b = 0.001,
                      taper = TRUE,
                      save.alpha = TRUE, save.beta = TRUE){

	print ("Preparing for MCMC")
	# 
	if(TRUE){
		Day.ID = Time.ID;taper=TRUE;range1 = 100;range2 = 250
		n.iter = 500; burn = 10; thin = 10000
		A.sd = 1000; A21.tune = 0.4; rho1.tune=0.4; rho2.tune = 0.3
		range1 = 100; range2 = 250
		rho1.a = 5; rho1.b = 0.05;rho2.a = 5; rho2.b = 0.05
		t1.a = 0.5; t1.b = 0.005
		t2.a = 0.5; t2.b = 0.005
		A1.a = 0.001; A1.b = 0.001
		A2.a = 0.001; A2.b = 0.001
		sigma.a = 0.001; sigma.b = 0.001
		taper = TRUE;save.alpha = TRUE; save.beta = TRUE
		
	}
	################################
	### Get some summary statistics
	## Total number of observations
	X = as.numeric (X)
	Y = as.numeric (Y)

	N = length (Y)
	N.space = nrow(Dist.mat)  # Total number of space location
	N.day = max(Day.ID)       # Total number of days
	Space.labels = 1:N.space
	Day.labels = 1:N.day

	################################
	### Check
	if (!is.null(Z)){	
	if ( all (Z[,1] == 1 ) ) { stop ("Fist column of Z should not be the intercept. ") }
	if ( length(Y) != length (X) ) { stop ("Length of Y and X mismatch.") }
	if ( length(Y) != nrow (Z) ) { stop ("Different sample size for Y and Z.") }
	if ( N.space != max (Space.ID) ) { stop ("Space ID must go from 1 to S.") }
	}

	#########################
	## Create fixed-effect design matrix
	Z = cbind (1, X, Z)

	########################################
	### Create full temporal distance matrix
	Day.mat = as.spam(matrix(0, ncol=N.day, N.day))
	for (i in 1:(N.day-1) ){ Day.mat[i,i+1] = 1 }
	for (i in 2:(N.day) ){ Day.mat[i,i-1] = 1 }
	Day.obs =  Day.labels %in% Day.ID ## Check which days are observed
	D = diag.spam ( apply (Day.mat, 2, sum))

	print (paste0("Total numberber of locations = ", N.space))
	print (paste0("Total numberber of days = ", N.day, "(", sum (Day.obs), " observed)" ))

	### Save some computation
	Space.perm = NULL ## Keep tract of permutation
	for (mon.i in Space.labels ){
			Space.perm = c(Space.perm, which (Space.ID == mon.i) )
			 X.i = Z[ Space.ID == mon.i, 1:2]
			if (mon.i == 1){
					XtX.space = t(X.i)%*%X.i
					Xt.space = t(X.i)
			} else {
					XtX.space = bdiag.spam (XtX.space, t(X.i)%*%(X.i) )
					Xt.space = bdiag.spam (Xt.space, t(X.i) )
			}
	}

	ZtZ = t(Z)%*%Z

	XtX.int.time = diag.spam (N.day)
	XtX.slope.time = rep(0, N.day)
	for (i in 1:N.day){
			day.i = Day.labels[i]
			X.i = X[Day.ID == day.i]
			XtX.int.time[i,i] = length (X.i)
			XtX.slope.time[i] = sum(X.i^2)
	}

	###################################
	### Discretize CAR parameters
	nrho = 2000
	junk = 1/sqrt(D); dt = eigen(junk%*%Day.mat%*%junk)$values
	canrho = detpart = qbeta(seq(0, 0.9999, length = nrho), 1, 1)
	for (j in 1:nrho){ detpart[j] = 1/2*sum(log(1-canrho[j]*dt)) }

	###################################
	### Initial Value from mixed model:
	print ("Obtaining Initial Values")
	fit = lmer (Y~Z -1 +  (X|Space.ID) + (X|Day.ID) )

	#gamma = matrix(fit@fixef, ncol = 1)
	gamma = matrix (fixef(fit), ncol=1)	
	sigma.prec = 1/mean (resid(fit)^2)
	#alpha = as.matrix(ranef (fit)[[2]])
	#beta = matrix(NA, ncol=2, nrow = N.day); beta[Day.obs,]= as.matrix(ranef (fit)[[1]])
	alpha = as.matrix(ranef (fit)$Space.ID)
	beta = matrix(NA, ncol=2, nrow = N.day); beta[Day.obs,]= as.matrix(ranef (fit)$Day.ID)
	
	
	tau = 1/apply (beta, 2, var, na.rm = T)
	if ( is.na(beta[1,1]) ){ beta[,1]= 0}
	for (i in 2:nrow(beta)){
			if ( is.na(beta[i,1]) ){ beta[i,] = beta[i-1,] }}

	rho1 = 100
	rho2 = 100
	psi = c(.999, .999)

	if (taper == TRUE){
			H1 = stationary.taper.cov(Mon.coord,theta =rho1,Taper.args=list(k=2,theta=range1,dimension=2))
			H2 = stationary.taper.cov(Mon.coord,theta =rho2,Taper.args=list(k=2,theta=range2,dimension=2))
	} else {
			H1 = round(exp(-Dist.mat/rho1), 5)
			H2 = round(exp(-Dist.mat/rho2), 5)
	}
	
	A = cov (alpha) ; A[1,2] = 0

	MMM = alpha[Space.ID,1]+alpha[Space.ID,2]*X+beta[Day.ID,1]+beta[Day.ID,2]*X+Z%*%gamma

	### MCMC saved parameters
	K = (n.iter - burn)/thin
	A.save = array (NA, c(2,2,K))
	rho.save = array(NA, c(K, 2))
	if (save.alpha == TRUE){ alpha.keep = array (NA, c(N.space, 2, K)) } else {alpha.keep = NULL}
	if (save.beta == TRUE){ beta.keep = array (NA, c(N.day, 2, K)) } else {beta.keep = NULL}
	sigma.save = rep (NA, K)
	gamma.save = array (NA, c(K, ncol(Z)))
	tau.save = array (NA, c(K, 2))
	psi.save = array (NA, c(K, 2))
	alpha.save = alpha2.save = matrix (0, nrow=N.space, ncol = 2)
	beta.save = beta2.save = matrix( 0, nrow = N.day, ncol = 2)
	Y.hat = rep(0, length (Y))
	Y2.hat = rep(0, length (Y))
	dev = rep(NA, K)
	A.acc = matrix(0,2,2)
	rho.acc = c(0,0)

	### MCMC begins
	print ("#############################")
	print ("MCMC Begins!")
	
	i <- 1
	
	while (i <= n.iter){

		if (TRUE){ #(i %% 250) == 0  ){ 
		  print (paste("Iteration", i, "of", n.iter)) 
		}
	  

		#Update fixed effects "gamma"
		MMM = MMM - Z%*%gamma
		RRR =  Y - MMM
		XXX = t(Z)%*%RRR
		VVV = solve(ZtZ)
		gamma = matrix(rmvnorm (1, VVV%*%XXX, 1/sigma.prec*VVV), ncol = 1)
		MMM = MMM + Z%*%gamma

		#Update residual error "sigma"
		RRR = Y - MMM
		sigma.prec = rgamma (1,  length(RRR)/2 + sigma.a, sum(RRR^2)/2 + sigma.b)

		#Update spatial random effects
		MMM = MMM - alpha[Space.ID,1] - alpha[Space.ID,2]*X
		RRR = Y - MMM
		XXX = sigma.prec*Xt.space %*% RRR[Space.perm]
		T1 = A[,1]%*%t(A[,1]); T2 = matrix (0,2,2); T2[2,2] = A[2,2]^2
		VVV = sigma.prec*XtX.space + solve(kronecker(H1,T1)+kronecker(H2,T2) )
		VVV = solve (VVV)
		alpha = matrix(VVV%*%XXX + t(chol(VVV))%*%rnorm(N.space*2), ncol = 2, byrow = T)
		MMM = MMM + alpha[Space.ID,1] + alpha[Space.ID,2]*X

		#Update temporal intercept
		MMM = MMM - beta[Day.ID,1]
		RRR = Y - MMM
		XXX = rep(0, N.day)
		XXX[Day.obs] = sigma.prec*tapply (RRR, Day.ID, sum)
		VVV = sigma.prec*XtX.int.time + tau[1]*(D - psi[1]*Day.mat)
		beta[,1] = rmvnorm.canonical (1, XXX, VVV)[1,]
		MMM = MMM + beta[Day.ID,1]

		#Update temporal AOD slope
		MMM = MMM - beta[Day.ID,2]*X
		RRR =  Y - MMM
		XXX = rep(0, N.day)
		XXX[Day.obs] = sigma.prec*tapply (RRR*X, Day.ID, sum)
		VVV = sigma.prec*diag.spam(XtX.slope.time) + tau[2]*(D - psi[2]*Day.mat)
		beta[,2] = rmvnorm.canonical (1, XXX, VVV)[1,]
		MMM = MMM + beta[Day.ID,2]*X

		######## UPDATE TEMPORAL RANDOM EFFECT PARAMETERS  #########
		#update intercept
		SS1 = as.numeric(t(beta[,1])%*%D%*%beta[,1])
		SS2 = as.numeric(t(beta[,1])%*%Day.mat%*%beta[,1])
		tau[1] = rgamma (1, N.day/2+t1.a, (SS1-psi[1]*SS2)/2 + t1.b)
		R = detpart + 0.5*tau[1]*canrho*SS2
		psi[1] = sample(canrho, 1, prob = exp(R-max(R)))

		#update AOD slope
		SS1 = as.numeric(t(beta[,2])%*%D%*%beta[,2])
		SS2 = as.numeric(t(beta[,2])%*%Day.mat%*%beta[,2])

		tau[2] = rgamma (1, N.day/2 + t2.a, (SS1-psi[2]*SS2)/2 + t2.b)
		R = detpart + 0.5*tau[2]*canrho*SS2
		psi[2] = sample(canrho, 1, prob = exp(R-max(R)))

		######## UPDATE SPATIAL RANDOM EFFECT PARAMETERS via M-H   #########
		#Update A[1,1]
		RRR = t(alpha[,1]) %*% t(chol (solve(as.matrix(H1))) )
		A[1,1] =  sqrt(1/rgamma (1, length(RRR)/2 + A1.a, sum(RRR^2)/2 + A1.b) )

		#Update A[2,2]
		RRR = t(alpha[,2] - A[2,1]/A[1,1]*alpha[,1]) %*% t(chol (solve(H2)))
		A[2,2] =  sqrt(1/rgamma (1, length(RRR)/2 + A2.a, sum(RRR^2)/2 + A2.b) )

		#Update A[2,1]
		RRR = (alpha[,2] - A[2,1]/A[1,1]*alpha[,1])/A[2,2]
		A.prop = A; A.prop[2,1] = rnorm (1, A[2,1], A21.tune)
		RRR.prop = (alpha[,2] - A.prop[2,1]/A[1,1]*alpha[,1])/A[2,2]
		lik.prop =  dmvnorm ( RRR.prop, rep(0,N.space), as.matrix(H2),  log = T)
		lik.curr =  dmvnorm ( RRR, rep(0,N.space), as.matrix(H2), log = T)
		ratio = lik.prop - lik.curr + dnorm(A.prop[2,1],0,A.sd,log=T) - dnorm(A[2,1],0,A.sd,log=T)
		if ( log(runif(1)) < ratio){ A = A.prop; alpha.lik = lik.prop; A.acc[2,1] = A.acc[2,1] + 1 }

		#Update rho1
		rho.prop = rlnorm(1, log(rho1), rho1.tune)
		if (taper == TRUE){
			 H1.prop=stationary.taper.cov(Mon.coord,theta=rho.prop,Taper.args=list(k=2,theta=range1,dimension=2))
		} else {  H1.prop = exp(-Dist.mat/rho.prop) }
		RRR = alpha[,1]/A[1,1]
		lik.prop =  dmvnorm ( RRR, rep(0,N.space), as.matrix(H1.prop), log = T)
		lik.curr =  dmvnorm ( RRR, rep(0,N.space), as.matrix(H1), log = T)
		ratio = lik.prop + dgamma (rho.prop, rho1.a, rho1.b, log = T) + log (rho.prop) -
			lik.curr - dgamma (rho1, rho1.a, rho1.b, log = T) - log(rho1)
		if ( log(runif(1)) < ratio){
			 rho1 = rho.prop; H1 = H1.prop; W1.lik=lik.prop; rho.acc[1] = rho.acc[1] + 1 }

		#Update rho2
		rho.prop = rlnorm(1, log(rho2), rho2.tune)
		if (taper == TRUE){
			 H2.prop=stationary.taper.cov(Mon.coord,theta=rho.prop,Taper.args=list(k=2,theta=range2,dimension=2))
		} else { H2.prop = exp(-Dist.mat/rho.prop) }
		
		RRR = (alpha[,2] - A[2,1]/A[1,1]*alpha[,1])/A[2,2]
		lik.prop =  dmvnorm ( RRR, rep(0,N.space), as.matrix(H2.prop), log = T)
		lik.curr =  dmvnorm ( RRR, rep(0,N.space), as.matrix(H2), log = T)
		ratio = lik.prop + dgamma (rho.prop, rho2.a, rho2.b, log = T) + log (rho.prop) -
			 lik.curr - dgamma (rho2, rho2.a, rho2.b, log = T) - log(rho2)
		if ( log(runif(1)) < ratio){
			 rho2 = rho.prop; H2 = H2.prop; W2.lik = lik.prop; rho.acc[2] = rho.acc[2] + 1 }

		if (i > burn & i %%thin == 0){
			k = (i - burn)/thin

			#Save statistics
			dev[k] = sum(-2*dnorm(Y, MMM, sqrt(1/sigma.prec), log=T))

			A.save[,,k] = A
			rho.save[k,] = c(rho1, rho2)
			if (save.alpha == TRUE){ alpha.keep[,,k] = alpha }
			if (save.beta == TRUE){ beta.keep[,,k] = beta }
			alpha.save = alpha.save + alpha/K
			alpha2.save = alpha2.save + alpha^2/K
			beta.save = beta.save + beta/K
			beta2.save = beta2.save + beta^2/K
			sigma.save[k] = sigma.prec
			gamma.save[k,] = gamma
			tau.save[k,] = tau
			psi.save[k,] = psi

			pred.samp =  (rnorm (N, MMM, sqrt(1/sigma.prec)) )
			Y.hat = Y.hat + pred.samp/K
			Y2.hat = Y2.hat + pred.samp^2/K
		}
			
		i <- i + 1
	} ## End of MCMC iterations

	alpha.MMM = alpha.save
	beta.MMM = beta.save
	gamma.MMM = apply (gamma.save, 2, mean)
	MMM = alpha.MMM[Space.ID, 1] + alpha.MMM[Space.ID, 2]*X +  beta.MMM[Day.ID,1] +
			beta.MMM[Day.ID,2]*X  + Z%*%gamma.MMM

	dhat = sum(-2*dnorm(Y, MMM, sqrt(1/mean(sigma.save)), log = T) )
	dbar = mean (dev)
	pD = dbar-dhat
	DIC = dbar + pD

	list (A = A.save, alpha = alpha.save, beta = beta.save, sigma2 = 1/sigma.save, gamma = gamma.save,
				alpha.sd = sqrt(alpha2.save - alpha.save^2),
				beta.sd = sqrt(beta2.save - beta.save^2),
				tau = tau.save, psi = psi.save,  rho = rho.save,
				Y = Y.hat, Y.sd = sqrt(Y2.hat-Y.hat^2),
				DIC = DIC, pD = pD, dev = dev, dbar=dbar,
				A.acc = A.acc/n.iter, rho.acc = rho.acc/n.iter,
				alpha.keep = alpha.keep, beta.keep = beta.keep,
				Mon.coord = Mon.coord, taper.info = c(range1, range2) )
}

############################################
## Function to perform pointwise predictions
############################################

################
###  INPUTS  ###
################
#
# obj = fitted MCMC object
#
# Z.pred = land use and weather covariates
# X.pred = AOD predictor
# Rred.coord = coordinates for the prediction
#  ** The Z for prediction must match (i.e. in the same column order) as the fitted object.
#
# n.iter = number Monte Carlo realizations
#
# save.samp = save all posterior samples?
#  ** This will take a lot of memory. Only need it if you are interested in Monte Carlo samples of "maps"
#
# Space.ID = location index rows of Z and X;1, 2, ..., S
#  ** Ex. AOD grid centroids
#
# Day.ID = day index for rows of Z and X; 1, 2, ..., T
#  ** Must be within the range of days that the model was fitted
#
################
### OUTPUTS  ###
################
#
#
#Est = estimate of the PM2.5 levels for each prediction record
#SD = standard error for each Est
#SpatialRE = map of the interpolated spatial bias at each location
#SpatialRE.sd = standard error for each SpatialRE
#Samp = Monte Carlo simulation of each mpa. Est (SD) is the mean (SD) of each simulation at each locatio.

options(spam.nearestdistnnz=c(2348949,400))

#spam::spam.options(nearestdistnnz=c(2348949,400))

pred.downscaler = function (obj, X.pred, Z.pred, Space.ID, Day.ID, Pred.coord,
                            n.iter = 100, save.samp = TRUE){
    # obj=fit;Space.ID=Space.ID.pred;Day.ID=Time.ID.pred;Pred.coord=Coord.pred;n.iter=100
    ################################
    ### Get some summary statistics
    ## Total number of observations
    X.pred = as.numeric (X.pred)
    Z = cbind (1, X.pred, Z.pred)

    N = length (X.pred)
    N.space = length (unique (Space.ID))
    N.mon = nrow(obj$Mon.coord)
    N.day = max(Day.ID)
    Space.labels = sort (unique (Space.ID))
    Day.labels = sort (unique (Day.ID))

    print (paste("Total number of predictions:", N))
    print (paste("Predictions over", N.space, "unique locations"))
    print (paste("Predictions over", N.day, "unique days"))

    ### Extracts info from previous model fit
    eff.dist1 = obj$taper.info[1]
    eff.dist2 = obj$taper.info[2]

    iter.use = sort( sample (1:length(obj$sigma2), n.iter, replace = T) )

    ################################################
    ## Interpolate Spatial Random Effects
    ################################################
    print ("Interpolating Spatial Random Effects")
    print ("This may take some time")
    post.int = matrix (0, ncol = n.iter, nrow = N.space)
    post.slope = matrix (0, ncol = n.iter, nrow = N.space)
    post = post2 = matrix (0, ncol = 2, nrow = N.space)

    ##Sample JOINT   spatial random effects
    Dist.list = rbind ( as.matrix(obj$Mon.coord), as.matrix(Pred.coord) )
    A = as.spam( matrix(0, nrow = N.mon*2, ncol = nrow (Dist.list)*2) )
    A[ 1:(N.mon*2), 1:(N.mon*2) ] = diag(N.mon*2)

    for (j in 1:n.iter){
       if (j %% 10 == 0){ print (paste ("    Iteration", j, "of", n.iter))}

        i = iter.use[j]
        H1 = stationary.taper.cov(Dist.list, theta = obj$rho[i,1],Taper.args=list(k=2,theta=eff.dist1,dimension=2))
        H2 = stationary.taper.cov(Dist.list, theta = obj$rho[i,2],Taper.args=list(k=2,theta=eff.dist2,dimension=2))

        ### Perform conditional kriging
        T1 = obj$A[,1,i]%*%t(obj$A[,1,i])
        T2 = matrix(0,2,2); T2[2,2] = obj$A[2,2,i]^2
        Sigma = kronecker(H1,T1)+kronecker(H2,T2)
        x = rmvnorm.spam(1, Sigma = Sigma)[1,]
        e = c(t(obj$alpha.keep[,,i]))

        y =  x-Sigma %*% t(A)%*%solve ( A%*%Sigma%*%t(A) ) %*% (A%*%x - e)

        post.samp = matrix (y, ncol = 2, byrow = TRUE)
        post.samp = post.samp[-c(1:N.mon),]

				if ( !is.matrix (post.samp) == TRUE) { post.samp = matrix (post.samp, ncol = 2) } 

        post.int[,j]= post.samp[,1]
        post.slope[,j] = post.samp[,2]

        post = post + post.samp/n.iter
        post2 = post2 + post.samp^2/n.iter
    }

    ### Predictions
    ### Loop through
    print ("Performing Predictions")
    Results = matrix (NA, ncol = n.iter, nrow = N)
    for (j in 1:n.iter){

        i = iter.use[j]

        #Effects components
        fix = Z %*% obj$gamma[i,]
        time.int = obj$beta.keep[Day.ID,1,i]
        time.slope = obj$beta.keep[Day.ID,2,i]
        space.int = post.int[Space.ID,j]
        space.slope = post.slope[Space.ID,j]

        resids = rnorm (N, rep(0, N), sqrt(obj$sigma[i]))
        post.samp = fix + time.int + time.slope*X.pred + space.int + space.slope*X.pred + resids
        Results[,j] = post.samp
    }

    Est = rowMeans (Results)
    SD =  sqrt( rowMeans(Results^2) - Est^2)

    if (save.samp == FALSE){ Results = NULL}
    list (Est = Est, SD = SD,
          SpatialRE = post, SpatialRE.sd = sqrt( post2 - post^2),
          Samp = Results)


}





