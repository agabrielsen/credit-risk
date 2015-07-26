KendallTau = function(X,Y,type) {
# ---------------------------------------------------------------
# PURPOSE: 
# Kendall Tau
# Basel Committee on Banking Supervision  
# Working Paper No. 14  
# Studies on the  Validation of Internal Rating Systems  
#
# ---------------------------------------------------------------
# INPUTS: 
# X			Array of data
# Y			Array of data
# type		a or b
#
# OUTPUTS:
# Tau, Tau Calculation,  Concordant, Discordant and Ties
# ---------------------------------------------------------------
# Author: 	Alexandros Gabrielsen
# Date:		August 2013
# ---------------------------------------------------------------

if (NROW(X) != NROW(Y)) stop( "size of X and Y is different")

M = t(combn(N,2))# Set up all the combination of pairs
XY = t(sapply(1:NROW(M), function(i,M,X,Y) {
	xx = sign(X[M[i,1]]-X[M[i,2]])
	yy = sign(Y[M[i,1]]-Y[M[i,2]])
	return(cbind(xx, yy, xx*yy))
},M,X,Y)) 
colnames(XY) = c("Sign(X)", "Sign(Y)", "Comparison")

xx = sum(XY[,"Comparison"]==1) 			# Concordant 
yy = sum(XY[,"Comparison"]==-1) 	 	# Discordant 	
zz = sum(XY[,"Comparison"]==0)			# Ties

if (type=="a") {
	TAU=(xx-yy)/(N*(N-1)/2)
}
if (type=="b") {
	TAU=(xx-yy)/(xx+yy+zz)
}

A=list(TAU,XY,xx,yy,zz)
names(A) = c("Tau", "Tau Calculation", "Concordant", "Discordant", "Ties")
return(A)

}

