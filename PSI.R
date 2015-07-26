PSI = function(X,Y,saveplot,showplot,ratings,psinames) {
# ---------------------------------------------------------------
# PURPOSE:  
# Population Distribution Index (PSI)
# Basel Committee on Banking Supervision  
# Working Paper No. 14  
# Studies on the  Validation of Internal Rating Systems  
# ---------------------------------------------------------------
# INPUTS:
# X				Array of (%) of Frequency
# Y				Array of (%) of Frequency
# saveplot		Save the plot
# showplot		Plots PSI (default=Yes)
# filepath		Path to save the plot
# psinames		Legend Names  (Plot)
# ratings		Ratings names (Plot)
#
# OUTPUTS:
# PSI Calculation per Rating
# PSI Statistic
# ---------------------------------------------------------------
# Author: 	Alexandros Gabrielsen
# Date:		August 2013
# ---------------------------------------------------------------

if (missing(saveplot)) {saveplot=0}
if (missing(showplot)) {showplot=1}
if (missing(ratings))  {ratings=1:NROW(X)}
if (missing(psinames)) {psinames=c("X","Y")}

PSICalc = log(X/Y)*(X-Y)
PSICalc[PSICalc == Inf] = NA
PSICalc[is.na(PSICalc)]=NA
PSIStat=sum(PSICalc,na.rm=TRUE)

if (saveplot==1) {
	graphics.off()
	win.metafile(paste(filepath,"PSI.png",sep=""))
}

if (showplot==1) {
matplot(cbind(X,Y), type=c('l','l'), lty=c(1,1), xaxt = "n", main="Population Stability Index", xlab="Risk Rating", ylab="Percentage of Total Accounts", col=c("blue","green"),lwd=1.2)
axis(1, at = 1:length(ratings), labels = paste(ratings), cex.axis = 1)
legend("topright", c(psinames), col=c("blue","green"), pch=c(1,1,2), cex=0.8,bty="n")
}
A = list(PSIStat,as.matrix(PSICalc))
names(A) = c("PSI Stat","PSI Calculation")
return(A)
}
PSIMulti = function(X, saveplot,showplot,ratings,psinames) {
# ---------------------------------------------------------------
# PURPOSE:  Population Distribution Index (PSI) - Multi Version
# ---------------------------------------------------------------
# INPUTS:
# X				Array of (%) of Frequency
# saveplot		Save the plot
# showplot		Plots PSI (default=Yes)
# filepath		Path to save the plot
# psinames		Legend Names  (Plot)
# ratings		Ratings names (Plot)
#
# OUTPUTS:
# PSI Calculation per Rating
# PSI Statistic
# ---------------------------------------------------------------
# Author: 	Alexandros Gabrielsen
# Date:		August 2013
# ---------------------------------------------------------------

if (missing(saveplot)) {saveplot=0}
if (missing(showplot)) {showplot=1}
if (missing(ratings))  {ratings=1:NROW(X)}
if (missing(psinames)) {psinames=colnames(X)}

N=NCOL(X)

B = lapply(1:(N-1), function(i,X) { PSI(X[,i], X[,c(i+1)], showplot=0) },X)
Names = sapply(1:(N-1), function(i,X) { paste(colnames(X)[i]," vs ", colnames(X)[c(i+1)],sep="") },X)

PSIStat = t(as.matrix(sapply(B,"[[",1))) # Extract PSI Statistic
colnames(PSIStat) = Names

PSICalc=lapply(B,"[[",2) # Extract PSI Calculations
names(PSICalc) = Names

if (saveplot==1) {
	graphics.off()
	win.metafile(paste(filepath,"PSI.png",sep=""))
}

if (showplot==1) {
	matplot(X, type=c('l'), lty=c(1), xaxt = "n", main="Population Stability Index", xlab="Risk Rating", ylab="Percentage of Total Accounts",lwd=1.2, col=rainbow(N))
	axis(1, at = 1:length(ratings), labels = paste(ratings), cex.axis = 1)
	legend("topright", c(psinames), pch=c(1), cex=0.8,bty="n",col=rainbow(N))
}
A = list(PSIStat,PSICalc)
names(A) = c("PSI Stat","PSI Calculation")
return(A)
}
