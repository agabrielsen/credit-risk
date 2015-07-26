bintest = function(ci, pds, freq, obspds, statistic) {
# ---------------------------------------------------------------
# PURPOSE: 		
# Binomial Test: Upper One/Two Tailed Upper Bounds Assuming Independence
# ---------------------------------------------------------------
# INPUS:
# ci			Confidence Interval
# pds 			Expected PDs
# freq			Frequency of ratings
# obspds		Observed PDs
# statistic		Upper One/Two Tail test
#
# OUTPUTS:
# One tailed upper bounds statistic
# Pass/Failure
# ---------------------------------------------------------------
# Author: 	Alexandros Gabrielsen
# Date:		August 2013
# ---------------------------------------------------------------

	Alower = pds - qnorm(ci) * sqrt((1/freq)* pds * (1 - pds))
	Alower[Alower<0] =0
	Aupper = pds + qnorm(ci) * sqrt((1/freq)* pds * (1 - pds))
	Blower = obspds < Alower
	Blower[Blower==FALSE] = "OK"
	Blower[Blower==TRUE] = "Under Estimate"
	Bupper = obspds > Aupper
	Bupper[Bupper==FALSE] = "OK"
	Bupper[Bupper==TRUE] = "Under Estimate"

if ( statistic == "onetail" | statistic == "one tail" | statistic == "one") {
	C=data.frame(Aupper,Bupper)
	colnames(C) = c(paste("One Tail Upper Bound (", ci,")", sep=""), "Pass / Fail")
} else {
	C=data.frame(Alower,Blower, Aupper,Bupper)
	colnames(C) = c(paste("Lower Bound (", ci,")", sep=""), "Pass / Fail", paste("Upper Bound(", ci, ")", sep=""), "Pass / Fail")
}
	C[C== Inf] = NA
	C[is.na(C)]=NA
	return(C)
}


bintestcor = function(ci, pds, obspds, p,statistic) {
# ---------------------------------------------------------------
# PURPOSE: 		Binomial Test: One Tailed Upper Bounds Assuming Asset Correlation
# 
# INPUS:
# ci			Confidence Interval
# pds 			Expected PDs
# obspds		Observed PDs
# p				Asset Correlation
# statistic		Upper One/Two Tail test
#
# OUTPUTS:
# One tailed upper bounds statistic
# Pass/Failure
#
# ---------------------------------------------------------------
# Author: 	Alexandros Gabrielsen
# Date:		August 2013
# ---------------------------------------------------------------

Alower= pnorm((qnorm(pds) + qnorm(1-ci) * sqrt(p))/sqrt(1-p))
Alower[Alower<0] =0
Aupper= pnorm((qnorm(pds) + qnorm(ci) * sqrt(p))/sqrt(1-p))
Blower = obspds < Alower
Blower[Blower==FALSE] = "OK"
Blower[Blower==TRUE] = "Under Estimate"
Bupper = obspds > Aupper
Bupper[Bupper==FALSE] = "OK"
Bupper[Bupper==TRUE] = "Under Estimate"

if ( statistic == "onetail" | statistic == "one tail" | statistic == "one") {
	C=data.frame(Aupper,Bupper)
	colnames(C) = c(paste("One Tail Upper Bound (", ci,")", sep=""), "Pass / Fail")
} else {
	C=data.frame(Alower,Blower, Aupper,Bupper)
	colnames(C) = c(paste("Lower Bound (", ci,")", sep=""), "Pass / Fail", paste("Upper Bound(", ci, ")", sep=""), "Pass / Fail")
}
	C[C== Inf] = NA
	C[is.na(C)]=NA
	return(C)
}

