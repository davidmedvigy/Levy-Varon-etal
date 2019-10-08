# Set the working directory that contains the experiment results.
setwd("~/Desktop/SFigure4")

#############################################################
#START: Load in data used to create Supplemental Figure 4
#############################################################
# load in the Results file
A<-read.csv("Results",header=TRUE,sep="")
#############################################################
#END: Load in data used to create Supplemental Figure 4
#############################################################

########################################################
#START: Graph Supplemental Figure 4
##########################################################
# FIXER ENHANCEMENT OF SOIL N

# Create an eps file for the graph
postscript(file="~/Desktop/Supplemental_Fig4.eps",width=10,height=8,horizontal = FALSE, onefile = FALSE, paper = "special")

#_______________________________________
# Start: Format graph parameters
#_______________________________________

par(mfrow=c(1,1))
par(mar=c(7,9,3,1))  #c(bottom, left, top, right)
par(mgp=c(5.5,2,0))

#Define tickmark locations and labels
Ticks1<-c(0,50,100,150,200,250,300) 
xlabels<- c("0","50","100","150","200","250","300")
#_______________________________________
# END: Format graph parameters
#_______________________________________
#calcualte % increase in soil N. ifile 5 identifies teh Fixer simulation and ifile 6 identifies the Null Fixer simulation
#(Fixer - Null_Fixer/Null_Fixer) * 100
Percent_Increase_SoilN<-(A$SoilN[A$ifile==5][1:300]-A$SoilN[A$ifile==6][1:300])/A$SoilN[A$ifile==6][1:300] *100

# plot % change in soil N. ifile X =XXX and ifile x = XXXX
plot(c(1:300),Percent_Increase_SoilN, type = 'l',lwd = 3,xlab = "Forest age (years)",ylab = "Increase in soil N (%)", axes = FALSE, cex.lab = 2.5, ylim=c(0,70))

#add axis
axis(2, at = c(0,10,20,30,40,50, 60,70), col = "black", labels = c("0","10","20","30","40","50","60","70"), las = 2,cex.axis = 2.5)        
axis(1,at= xlabels,col = "black", labels = xlabels, cex.axis = 2.5,tcl = -0.5)

#finish creating eps file
dev.off()

########################################################
#END: Graph Supplemental Figure 4
##########################################################
