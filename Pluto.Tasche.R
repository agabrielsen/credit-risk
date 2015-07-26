# ---------------------------------------------------------------
# PURPOSE:
# Pluto and Tasche, 2005, Estimating Probabilities of Default for 
# Low Default Portfolios
# Basel Committee on Banking Supervision 
# Working Paper No. 14 
# Studies on the  Validation of Internal Rating Systems 
# ---------------------------------------------------------------
# INPUTS:
# n					 	number of obligors for a particular grade
# i						number of defaults for a particular grade
# theta 				this is the average of the first year correlations for the number of years considered. 30% is the assumed autocorrelation in the systemic/single factor in the Basel II single factor model.
# rho					asset correlation
# T						number of years
# Conf					Likelihood of having the number of defaults exceeding the observed given an assumed PD
# N						The number of simulations
#
# OUTPUTS:
# pd					upper bound pd 
# 
# FUNCTIONS:
# plutotashe			Estimates the Upper Pound PD given the number of obligors and defaults
# error					Error function used in the optimization
# sim					Simulation Run
# binom_cum				Cumulative Binomial
# ptdata				Formats a data vector
# plutotascheportfolio	Estimate the Upper Pound PD & Scaling Factors for a Portfolio
# ---------------------------------------------------------------
# Author: 	Alexandros Gabrielsen
# Date:		August 2013
# ---------------------------------------------------------------

# Estimates the Upper Pound PD given the number of obligors and defaults
plutotashe <- function(n, i, theta, rho, T, Conf, N) {
hh = uniroot(error, interval=c(0,1), lower=0.000000001, upper=0.999999999, tol = 1e-8, maxiter=5000,
	n=n, i=i, theta=theta, rho=rho, T=T, Conf=Conf, N=N)
names(hh) = c("UpperBoundPD", "Function", "iter", "estim.prec")
return(hh)	
}

# Error Function
error <- function(pd, n, i, theta, rho, T, Conf, N) {
	AverageSim = 1-mean(replicate(N, sim(pd,n,i,theta,rho, T)))
	error = AverageSim-Conf
	return(error)
}

# Simulation Run
sim = function(pd, n, i, theta, rho, T) {
# Generate a state of the economy
Z=array(0,c(T,1))
Z[1] = rnorm(1)
if (T > 1) {
	for (j in 2:T) {
		Z[j] = theta*Z[j-1]+sqrt(1-theta^2)*rnorm(1)
	}
	rm(j)
}
# pnotd = 1 - pnorm((qnorm(pd)-Z*sqrt(rho))/sqrt(1-rho)) #Prob Not Defaulting
p = 1-prod(1 - pnorm((qnorm(pd)-Z*sqrt(rho))/sqrt(1-rho))) # Prob of Defaulting
(tt=binom_cum(n,p,i))
return(tt)
}

# Cumulative Binomial
binom_cum = function (n,p,i) {
bc = 0
for (j in 0:i) {
	bc =  bc + choose(n,j)*(p^j)*((1-p)^(n-j))
}
return(bc)
}

createptdata <- function(data) {
# PURPOSE
# Create the Pluto & Tasche PD Table
# 
# INPUTS
# data		vector with three column names: Grades, Obligors & Defaults
#
# OUPUTS
# ptdata	vector formatted according to remaining defaulted obligors including portfolio PD
#
# Author: 	Alexandros Gabrielsen
# Date:		August 2013

ptdata = as.matrix(t(sapply(1:NROW(data), function(X) {
	a=sum(data[X:NROW(data),"Obligors"])
	b=sum(data[X:NROW(data),"Defaults"])
	return(list(a,b))
})))
colnames(ptdata) = c("Obligors", "Defaults")
ptdata = rbind(ptdata, c(sum(data[,"Obligors"]), sum(data[,"Defaults"]))) # append the portfolio level (BCR Test)
rownames(ptdata) = c(as.matrix(data[,"Grades"]), "Portfolio")
return(ptdata)
}

plutotascheportfolio <- function(ptdata, Ndefaults, theta, rho, T, Conf,N) {
# PURPOSE
# Estimate the Pluto & Tasche Upper Bound PD for a Portfolio
#
# INPUTS
# ptdata		a vector of data with column names: Grades, Obligors, Defaults
# Ndefaults		a vector containing the number of defaults per rating
# ...
# 
# OUTPUTS
# PD			Portfolio upper bound PDs including various optimization statistics
# APD			Adjusted PD based on the Portfolio Scaling Factor
# PortfolioPD	Benjamin Cathcart Ryan Statistic
#
# Author: 	Alexandros Gabriesen
# Date:		August 2013

	PD = t(sapply(1:NROW(ptdata), function(X, ptdata, theta, rho, T, Conf,N) {
	print(paste("Progress: ",round((X/NROW(ptdata))*100,0),"%", sep="")) # Progress Bar
	plutotashe(as.numeric(ptdata[X,"Obligors"]), as.numeric(ptdata[X,"Defaults"]), theta, rho, T, Conf, N)
	}, ptdata = ptdata, theta = theta, rho = rho, T=T, Conf=Conf, N=N))
	rownames(PD) = rownames(ptdata)
	

	# Estimate the Scalling Factor
	sf = as.numeric(PD["Portfolio","UpperBoundPD"])	/(sum(Ndefaults*as.numeric(PD[1:(NROW(PD)-1),"UpperBoundPD"]))/sum(Ndefaults))

	PD = cbind(as.numeric(PD[,"UpperBoundPD"]), as.numeric(PD[,"UpperBoundPD"])*sf)
	colnames(PD) = c("UpperBoundPD", "AdjustedPD")
	rownames(PD) = rownames(ptdata)
	PD["Portfolio","AdjustedPD"] = NA
	
	return(PD)
}
