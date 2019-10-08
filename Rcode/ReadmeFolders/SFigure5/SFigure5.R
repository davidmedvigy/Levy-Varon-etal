# Set the working directory that contains the experiment results.
setwd("~/Desktop/SFigure5")

#############################################################
#START: Load in data used to create Supplemental Figure 5
#############################################################
# load in the Results file
T<-read.csv("Results",header=TRUE,sep="")
#############################################################
#END: Load in data used to create Supplemental Figure 5
#############################################################

########################################################
#START: Graph Supplemental Figure 5
##########################################################

# Create an eps file for the graph
postscript(file="~/Desktop/Supplemental_Fig5.eps",width=10,height=8,horizontal = FALSE, onefile = FALSE, paper = "special")

#_______________________________________
# Start: Format graph parameters
#_______________________________________
par(mfrow=c(1,1))
par(mar=c(7,8,1,1))  #c(bottom, left, top, right)
par(mgp=c(5,2,0))

#Define tickmark locations and labels
Ticks1<-c(0,50,100,150,200,250,300) 
xlabels<- c("0","50","100","150","200","250","300")
#_______________________________________
# Start: Format graph parameters
#_______________________________________

# plot aboveground biomass for the Fixer PFT in the Individual level fixation simulaiton that includes disturbance
plot (c(1:300),T$AGB30[T$ifile==2], type = "l", col = "black", xlab = "Forest age (years)", ylab="", ylim = c(0,6),cex.axis = 2, cex.lab = 2.5,axes=FALSE,lwd=4)
#add the aboveground biomass for the fixer PFT in the simulation that does not include disturbance
points(c(1:300),T$AGB30[T$ifile==7][1:300], lwd = 4, col = "brown", type = "l")

#add Y axis label
mtext(expression(paste("Fixer Biomass C (kg C",sep = " ", m^-2,sep = " ",")")), side = 2,  line = 4,cex=2.5) 

#add legend
legend(0,6,c("Disturbance","No Disturbance"),col = c("black","brown"), bty = "n", lty = c(1,1), lwd = c(4,4), cex = 2)

#add axis
axis(2, at = c(0,2,4,6), col = "black", labels = c(0,"2", "4","6"), las = 2,cex.axis = 2.5)        
axis(1,at= Ticks1,col = "black", labels = xlabels, cex.axis = 2.5,tcl = -0.5)

#finish creating eps file
dev.off()

########################################################
#END: Graph Supplemental Figure 5
##########################################################