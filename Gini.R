Gini = function(freq, default, saveplot, showplot) {
# ---------------------------------------------------------------
# PURPOSE:  
# PD Accuracy Ration (Gini) 
# Basel Committee on Banking Supervision 
# Working Paper No. 14 
# Studies on the  Validation of Internal Rating Systems 
# ---------------------------------------------------------------
# INPUTS:
# freq			Frequency
# default		Number of Defaults
# saveplots		Save the plot
# filepath		Path to save the plot
# showplot		Plots Gini (default=Yes)
#
# OUTPUTS:
# Gini Calculation per Rating
# Gini Statistic
# ---------------------------------------------------------------
# Author: 	Alexandros Gabrielsen
# Date:		August 2013
# ---------------------------------------------------------------
if (missing(saveplot)) {saveplot=0}
if (missing(showplot)) {showplot=1}

if (NROW(freq) != NROW(default)) stop( "size of Freq and Default arrays is different")

N=NROW(freq)

a = cumsum(default/sum(default)) 				# Cumulative Defaults (% of Total Defaults)
b = cumsum((freq-default)/sum(freq-default)) 	# Cumulative Non-Defaults (% of Total Survivals)

GiniCalc = c(0, (b[2]*a[2])/2,((b[3:N]+b[2:(N-1)])*(a[3:N]-a[2:(N-1)]))/2)
Gini = (sum(GiniCalc)-0.5)*2

if (saveplot==1) {
	graphics.off()
	win.metafile(paste(filepath,"Gini.png",sep=""))
}
if (showplot==1) {
	plot(a*100,b*100, type=c('b'), xlim=c(0,100), ylim=c(0,100),
	main="Gini", xlab="Cumulative Bads (%)", ylab="Cumulative Goods (%)", col="blue",lwd=1.2)
	abline(a=0, b=1, col="red",lwd=1.2)
	legend("bottomright", c("Model", "Random Model", paste("Gini ",round(Gini*100,0), "%", sep="")), col=c("blue","red","black"), pch=c(1,1,2), cex=0.8,bty="n")
}
A = list(Gini,as.matrix(GiniCalc))
names(A) = c("Gini Stat","Gini Calculation")
return(A)
}
