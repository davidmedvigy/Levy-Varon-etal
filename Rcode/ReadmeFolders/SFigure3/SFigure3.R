# Set the working directory that contains the experiment results.
setwd("~/Desktop/SFigure3")

#############################################################
#START: Load in data used to create Supplemental Figure 3
#############################################################
# load in the Results file
T<-read.csv("Results",header=TRUE,sep="")
#############################################################
#END: Load in data used to create Supplemental Figure 3
#############################################################

########################################################
#START: Graph Supplemental Figure 3
##########################################################

# Create an eps file for the graph
postscript(file="~/Desktop/Supplemental_Fig3.eps",width=14,height=12,horizontal = FALSE, onefile = FALSE, paper = "special")

#__________________________________________________________________________________________
#START: Create plot Supplemental Fig 3a -PFT succession for Individual level fixation
#__________________________________________________________________________________________

#_______________________________________
# Start: Format graph parameters
#_______________________________________

par(mfrow=c(2,2))
par(mar=c(7,8,3,1))  #c(bottom, left, top, right)
par(mgp=c(4,2,1))

#Define x axis and axis labels
xticks<- c("0","50","100","150","200","250","300")
xlabels<- c("0","","100","","200","","300")
#_______________________________________
# End: Format graph parameters
#_______________________________________
#Identify simulation to be used. year = 2 identifies the individual level fixation simulation
year=2

#plot aboveground biomass for late-successional PFT
plot (c(1:300),T$AGB4[T$ifile==year], type = "l", col = "black", xlab = "Forest age (years)", ylab=expression(paste("Plant C (kg C",sep = " ", m^-2,sep = " ",")")), ylim = c(0,8),cex.axis = 2.5, cex.lab = 2.5,axes=FALSE,lty=2,lwd=4, xlim = c(0,300))
#add aboveground biomass for the mid-successional PFT
points(c(1:300),T$AGB3[T$ifile==year],type = "l", col = "black", lwd = 6,lty=3)
#add aboveground biomass for the late-successional PFT
points(c(1:300),T$AGB4[T$ifile==year],type = "l", col = "black", lwd = 4,lty=2)
#add aboveground biomass for the fixer PFT
points(c(1:300),T$AGB30[T$ifile==year],type = "l", col = "black", lwd = 4)
#add aboveground biomass for the early-successional PFT
points(c(1:300),T$AGB2[T$ifile==year],type = "l", col = "black", lwd = 2, lty = 1)

#add a legend
legend("topleft",c("Early- successional","Mid- successional","Late- successional","Fixer"),col = c("black","black","black","black"), bty = "n", lty = c(1,3,2,1), lwd = c(3,3,3,6), cex = 2)
#add axis
axis(2, at = c(0,2,4,6,8,10,12), col = "black", labels = c(0,"2", "4","6","8","10","12"), las = 2,cex.axis = 2.5)        
axis(1,at= xlabels,col = "black", labels = xlabels, cex.axis = 2.5,tcl = -0.5, line = -1)

#label panel a
text(290,8,"a", cex = 2.5)
#__________________________________________________________________________________________
#END: Create plot Supplemental Fig 3a -PFT succession for Individual level fixation simulation
#__________________________________________________________________________________________
#__________________________________________________________________________________________
#START: Create plot Supplemental Fig 3b -PFT succession for No fixation simulation
#__________________________________________________________________________________________

#Identify simulation to be used. year = 3 identifies the No fixation simulation
year=3

#plot aboveground biomass for late-successional PFT
plot (c(1:300),T$AGB4[T$ifile==year], type = "l", col = "black", xlab = "Forest age (years)", ylab=expression(paste("Plant C (kg C",sep = " ", m^-2,sep = " ",")")), ylim = c(0,8),cex.axis = 2.5, cex.lab = 2.5,axes=FALSE,lty=2,lwd=4, xlim = c(0,300))
#add aboveground biomass for the mid-successional PFT
points(c(1:300),T$AGB3[T$ifile==year],type = "l", col = "black", lwd = 6,lty=3)
#add aboveground biomass for the late-successional PFT
points(c(1:300),T$AGB4[T$ifile==year],type = "l", col = "black", lwd = 4,lty=2)
#add aboveground biomass for the fixer PFT
points(c(1:300),T$AGB30[T$ifile==year],type = "l", col = "black", lwd = 4)
#add aboveground biomass for the early-successional PFT
points(c(1:300),T$AGB2[T$ifile==year],type = "l", col = "black", lwd = 2, lty = 1)

#add axis
axis(2, at = c(0,2,4,6,8,10,12), col = "black", labels = c(0,"2", "4","6","8","10","12"), las = 2,cex.axis = 2.5)        
axis(1,at= xlabels,col = "black", labels = xlabels, cex.axis = 2.5,tcl = -0.5, line = -1)

#add legend
legend("topleft",c("Early- successional","Mid- successional","Late- successional","Null-Fixer"),col = c("black","black","black","black"), bty = "n", lty = c(1,3,2,1), lwd = c(3,3,3,6), cex = 2)

#label panel b
text(290,8, "b", cex = 2.5)
#__________________________________________________________________________________________
#END: Create plot Supplemental Fig 3b -PFT succession for No fixation simulation
#__________________________________________________________________________________________
#__________________________________________________________________________________________
#START: Create plot Supplemental Fig 3c -PFT succession for Fixer simulation
#__________________________________________________________________________________________

#Identify simulation to be used. year = 5 identifies the Fixer simulation
year=5

#plot aboveground biomass for the Fixer PFT
plot (c(1:300),T$AGB30[T$ifile==year], type = "l", col = "black", xlab = "Forest age (years)", ylab=expression(paste("Plant C (kg C",sep = " ", m^-2, sep = " ",")")), ylim = c(0,8),cex.axis = 2.5, cex.lab = 2.5,axes=FALSE, lwd = 6)
#add aboveground biomas for the Null Fixer PFT
points(c(1:300),T$AGB35[T$ifile==year],type = "l", col = "black", lwd = 4,lty=6)

#add axis
axis(2, at = c(0,2,4,6,8,10,12), col = "black", labels = c(0,"2", "4","6","8","10","12"), las = 2,cex.axis = 2.5)        
axis(1,at= xlabels,col = "black", labels = xlabels, cex.axis = 2.5,tcl = -0.5, line = -1)

#add legend
legend("topleft",c("Fixer ","Null-Fixer"),col = c("black","black"), bty = "n", lty = c(1,6,1,1), lwd = c(6,2.2), cex = 2)

#label panel c
text(290,7.8,"c", cex = 2.5)

#__________________________________________________________________________________________
#END: Create plot Supplemental Fig 3b -PFT succession for No fixation simulation
#__________________________________________________________________________________________
#__________________________________________________________________________________________
#START: Create plot Supplemental Fig 3d -PFT succession for Null Fixer simulation
#__________________________________________________________________________________________
#Identify simulation to be used. year = 6 identifies the Null Fixer simulation
year=6

# Note: the null fixer simulation is plotting AGB for pft 30 because the simulation was run with PFT 30 and a fixation rate = 0. 
plot (c(1:300),T$AGB30[T$ifile==year ], type = "l", col = "black", xlab = "Forest age (years)",ylab=expression(paste("Plant C (kg C",sep = " ", m^-2, sep = " ",")")), ylim = c(0,8),cex.axis = 2.5, cex.lab = 2.5,axes=FALSE, lwd = 4,lty=6)

#add axis
axis(2, at = c(0,2,4,6,8,10,12), col = "black", labels = c(0,"2", "4","6","8","10","12"), las = 2,cex.axis = 2.5)        
axis(1,at= xlabels,col = "black", labels = xlabels, cex.axis = 2.5,tcl = -0.5, line = -1)

#add a legend
legend("topleft",c("Null-Fixer"),col = c("black"), bty = "n", lty = c(6,1,1,1), lwd = c(2.2,2.5,2.5,2.5), cex = 2)

#label panel d
text(290,8, "d", cex = 2.5)
#__________________________________________________________________________________________
#START: Create plot Supplemental Fig 3d -PFT succession for Null Fixer simulation
#__________________________________________________________________________________________
#finish creating eps file
dev.off()

########################################################
#END: Graph Supplemental Figure 3
##########################################################
