HosmerLemeshow = function(freq,obspds,ci) {
# ---------------------------------------------------------------
# PURPOSE: 
# Hosmer Lemeshow Test
# Basel Committee on Banking Supervision 
# Working Paper No. 14 
# Studies on the  Validation of Internal Rating Systems 
# ---------------------------------------------------------------
# INPUTS:
# freq			Frequency
# obspds		Observed PD
# ci			Confidence Interval
#
# Outputs
# HL			Hosmer Lemeshow Test
# HLstat		Hosmer Lemeshow Statistic
# Critical		Critical Values chisquared with N-2 degrees of freedom
# Pass/Fail
#
# ---------------------------------------------------------------
# Author: 	Alexandros Gabrielsen
# Date:		August 2013
# ---------------------------------------------------------------

N=NROW(freq)

HL = as.matrix(((freq*obspds-freq*pds)^2)/(freq*(pds)*(1-pds)))
colnames(HL) ="Hosmer-Lemeshow"
HLstat = sum(HL,na.rm=TRUE)


Critical = t(as.matrix(qchisq(1-ci, (N-2), lower.tail = FALSE)))
colnames(Critical) = ci

A=HLstat>Critical
A[A==FALSE] = "Pass"
A[A==TRUE] = "Fail"
colnames(A) = ci

C = list(HL, HLstat, Critical,A)
names(C) = c("HL Test", "HL Stat", "Critical", "Pass/Fail")
return(C)
}
