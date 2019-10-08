# Set the working directory that contains the experiment results.

setwd("~/Desktop/SFigure1")

#############################################################
#START: Load in data used to create Supplemental Figure 1
#############################################################
# load in the Results file
T<-read.csv("Results",header=TRUE,sep="")

#############################################################
#END: Load in data used to create Supplemental Figure 1
#############################################################

########################################################
#START: Graph Supplemental Figure 1
##########################################################

# Create an eps file for the graph
postscript(file="~/Desktop/Supplemental_Fig1.eps",width=14,height=8,horizontal = FALSE, onefile = FALSE, paper = "special")

#__________________________________________________________________________________________
#START: Create plot Supplemental Fig 1a -PFT succession for Ecosystem level fixation
#__________________________________________________________________________________________

#_______________________________________
# Start: Format graph parameters
#_______________________________________
par(mfrow=c(1,2))
par(mar=c(7,8,3,1))  #c(bottom, left, top, right)
par(mgp=c(4,2,1))

#Define x axis and axis labels
Ticks1<-c(0,25,50,75,100) 
xlabels<- c("0","","50","","100")
#_______________________________________
# End: Format graph parameters
#_______________________________________

#Identify simulation to be used. year = 4 identifies the ecosystem levelfixation simulation
year=4

#plot aboveground biomass for late-successional PFT
plot (c(1:length(T$AGB4[T$ifile==year])),T$AGB4[T$ifile==year], type = "l", col = "black", xlab = "Forest age (years)", ylab=expression(paste("Plant C (kg C",sep = " ", m^-2,sep = " ",")")), ylim = c(0,8),cex.axis = 2, cex.lab = 2.5,axes=FALSE,lty=2,lwd=4, xlim = c(0,100))
#add aboveground biomass for the mid-successional PFT
points(c(1:length(T$AGB3[T$ifile==year])),T$AGB3[T$ifile==year],type = "l", col = "black", lwd = 6,lty=3)
#add aboveground biomass for the late-successional PFT
points(c(1:length(T$AGB4[T$ifile==year])),T$AGB4[T$ifile==year],type = "l", col = "black", lwd = 4,lty=2)
#add aboveground biomass for the fixer PFT
points(c(1:length(T$AGB30[T$ifile==year])),T$AGB30[T$ifile==year],type = "l", col = "black", lwd = 4)
#add aboveground biomass for the early-successional PFT
points(c(1:length(T$AGB2[T$ifile==year])),T$AGB2[T$ifile==year],type = "l", col = "black", lwd = 2, lty = 1)

#add a legend
legend("topleft",c("Early- successional","Mid- successional","Late- successional","Fixer"),col = c("black","black","black","black"), bty = "n", lty = c(1,3,2,1), lwd = c(3,3,3,6), cex = 2)
#add axis
axis(2, at = c(0,2,4,6), col = "black", labels = c(0,"2", "4","6"), las = 2,cex.axis = 2.5)        
axis(1,at= xlabels,col = "black", labels = xlabels, cex.axis = 2.5,tcl = -0.5, line = -1)

#label panel a
text(100,7,"a", cex = 2.5)

#__________________________________________________________________________________________
#END: Create plot Supplemental Fig 1a -PFT succession for Ecosystem level fixation
#__________________________________________________________________________________________
#__________________________________________________________________________________________
#START: Create plot Supplemental Fig 1b -PFT succession for individual level simulation
#__________________________________________________________________________________________

#Identify simulation to be used. year = 2 identifies the individual level fixation simulation
year=2

#plot aboveground biomass for late-successional PFT
plot (c(1:length(T$AGB4[T$ifile==year])),T$AGB4[T$ifile==year], type = "l", col = "black", xlab = "Forest age (years)", ylab=expression(paste("Plant C (kg C",sep = " ", m^-2,sep = " ",")")), ylim = c(0,8),cex.axis = 2.5, cex.lab = 2.5,axes=FALSE,lty=2,lwd=4, xlim = c(0,100))
#add aboveground biomass for the mid-successional PFT
points(c(1:length(T$AGB3[T$ifile==year])),T$AGB3[T$ifile==year],type = "l", col = "black", lwd = 6,lty=3)
#add aboveground biomass for the late-successional PFT
points(c(1:length(T$AGB4[T$ifile==year])),T$AGB4[T$ifile==year],type = "l", col = "black", lwd = 4,lty=2)
#add aboveground biomass for the fixer PFT
points(c(1:length(T$AGB30[T$ifile==year])),T$AGB30[T$ifile==year],type = "l", col = "black", lwd = 4)
#add aboveground biomass for the early-successional PFT
points(c(1:length(T$AGB2[T$ifile==year])),T$AGB2[T$ifile==year],type = "l", col = "black", lwd = 2, lty = 1)

#add a legend
legend("topleft",c("Early- successional","Mid- successional","Late- successional","Fixer"),col = c("black","black","black","black"), bty = "n", lty = c(1,3,2,1), lwd = c(3,3,3,6), cex = 2)

#add axis
axis(2, at = c(0,2,4,6), col = "black", labels = c(0,"2", "4","6"), las = 2,cex.axis = 2.5)        
axis(1,at= xlabels,col = "black", labels = xlabels, cex.axis = 2.5,tcl = -0.5, line = -1)

#label panel b
text(100,7,"b", cex = 2.5)
#__________________________________________________________________________________________
#END: Create plot Supplemental Fig 1b -PFT succession for individual level simulation
#__________________________________________________________________________________________

#finish creating eps file
dev.off()

########################################################
#END: Graph Supplemental Figure 1
##########################################################
