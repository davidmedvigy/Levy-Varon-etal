
# Set the working directory that contains the experiment results.
setwd("~/Desktop/SFigure2")
#############################################################
#START: Load in data used to create Supplemental Figure 2
#############################################################
# load in the Results file
A<-read.csv("Results",header=TRUE,sep="")

# load in the observed C values from Batterman et al. (2014)
observed<-read.csv("AS_PlantC.csv",header = TRUE)
ObsCarbon<-observed$Carbon/10000 # Divide by 10000 to convert units from ha to m2
#############################################################
#END: Load in data used to create Supplemental Figure 2
#############################################################

########################################################
#START: Graph Supplemental Figure 2
##########################################################

# Create an eps file for the graph
postscript(file="~/Desktop/Supplemental_Fig2.eps",width=10,height=8,horizontal = FALSE, onefile = FALSE, paper = "special")

#_______________________________________
# Start: Format graph parameters
#_______________________________________
par(mfrow=c(1,1))
par(mar=c(7,8,3,1))  #c(bottom, left, top, right)
par(mgp=c(5,2,0))

#Define tickmark locations and labels
Ticks1<-c(0,50,100,150,200,250,300) 
xlabels<- c("0","50","100","150","200","250","300")
#_______________________________________
# END: Format graph parameters
#_______________________________________

#Plot observed C curve
plot (observed$Year,ObsCarbon, type = "l", col = "gray", xlab = "Forest age (years)",  ylab=expression(paste("Plant C (kg C",sep = " ", m^-2, sep = " ",yr^-1,sep = " ",")")),ylim=c(0,14), xlim=c(1,300), lwd = 5, ,axes=FALSE, cex.lab = 2.5)

#add axis
axis(1,at= Ticks1,col = "black", labels = xlabels, cex.axis = 2.5,tcl = -0.5)
axis(2, at = c(0,2,4,6,8,10,12,14), col = "black", labels = c(0,"2", "4","6","8","10","12","14"), las = 2,cex.axis = 2.5)        

# add individual level fixation predictions (ifile = 2)
points (c(1:300),A$PlantC[A$ifile==2], type = "l", col = "orange", lwd = 5)

#add individual level fixation + N predictions (Fertilization experiment) (ifile=8)
points (c(1:300),A$PlantC[A$ifile==8], type = "l", col = "darkcyan", lwd = 5)

#add legend
legend("right",c("Individual level fixation + N","Individual level fixation"),col = c("darkcyan","orange"), bty = "n", lty = c(1,1,1,1), lwd = c(3,3,3), cex = 2.5)

#finish creating eps file
dev.off()
########################################################
#END: Graph Supplemental Figure 2
##########################################################