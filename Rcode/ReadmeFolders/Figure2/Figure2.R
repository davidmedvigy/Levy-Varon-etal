# Set the working directory that contains the experiment results.
setwd("~/Desktop/Figure2")


############################################
#START: Load in data used to create Figure 2
############################################
A<-read.csv("Results",header=TRUE,sep="")

# load in the observed C values 
observed<-read.csv("AS_PlantC.csv",header = TRUE)
ObsCarbon<-observed$Carbon/10000 # Divide by 10000 toconvert units from ha to m2

# load in a vector created from observed fixation values
c<-read.csv("ControlFixation",header=TRUE,sep="")
ObsFixation<-c$x

############################################
#END: Load in data used to create Figure 2
############################################

########################################################
#START:Graph Fig 2
##########################################################

# Create an eps file with the graph
postscript(file="~/Desktop/Fig2.eps",width=7.5,height=11,horizontal = FALSE, onefile = FALSE, paper = "special")

#_______________________________________
#Start:Format parameters
#_______________________________________

par(mfrow=c(3,1))
par(mar=c(2,8.3,3,1))  #c(bottom, left, top, right)
par(mgp=c(5,2,0))

Ticks1<-c(0,50,100,150,200,250,300) 
xlabels<- c("0","50","100","150","200","250","300")
#_______________________________________
#End:Format parameters
#_______________________________________

#_____________________________________________
#START:Create plot Fig 2a - Plant C accumulation
#_____________________________________________

#Plot observed curve
plot (observed$Year,ObsCarbon, type = "l", col = "gray",  ylab=expression(paste("Plant C (kg C",sep = " ", m^-2, sep = " ",")")),ylim=c(0,15), xlim=c(1,300), lwd = 5, ,axes=FALSE, cex = 2.8,xlab = "",cex.lab=2.8)

#add axis
axis(1,at= Ticks1,col = "black", labels = xlabels, cex.axis = 2.8,tcl = -0.5)
axis(2, at = c(0,2,4,6,8,10,12,14), col = "black", labels = c(0,"", "4","","8","","12",""), las = 2,cex.axis = 2.8)        

#add ecosystem fixation predictions
points(c(1:length(A$PlantC[A$ifile==4])),A$PlantC[A$ifile==4], type = "l", col = "blue", lwd = 4)

# add individual fixation predictions
points(c(1:length(A$PlantC[A$ifile==2])),A$PlantC[A$ifile==2], type = "l", col = "darkorange", lwd = 4)

#add no fixation predictions
points(c(1:length(A$PlantC[A$ifile==3])),A$PlantC[A$ifile==3], type = "l", col = "cyan3", lwd = 4)

#add a legend
legend(150,8,c( "Ecosystem level fixation", "Individual level fixation", "No fixation","Observed" ),col = c("blue","darkorange","cyan3", "gray"), bty = "n", lty = c(1,1,1,1), lwd = c(3,3,3), cex = 1.8)

#label panel a
text(305,14.7,"a", cex = 2.5)
#_____________________________________________
#END:Create plot Fig 2a - Plant C accumulation
#_____________________________________________

#_____________________________________________
#START:Create plot Fig 2b - Difference between observed and predicted Plant C accumulation
#_____________________________________________

# Calculate change in absolute C from observed C
Fixation<-A$PlantC[A$ifile==2] - ObsCarbon[1:300]
Nofixation<-A$PlantC[A$ifile==3]- ObsCarbon[1:300]
Ecosystem<-A$PlantC[A$ifile==4]- ObsCarbon[1:300]
observed<-ObsCarbon-ObsCarbon
#############

#plot difference for individual fixation
plot (0:300,c(0,Fixation), type = "l", col = "darkorange", xlab = "", , ylab=expression(paste(Delta ~ "Plant C (kg C",sep = " ", m^-2, sep = " ",")")),ylim=c(-4,2), xlim=c(1,300), lwd = 4, ,axes=FALSE, cex.lab = 2.8)

#add axis
axis(1,at= Ticks1,col = "black", labels = xlabels, cex.axis = 2.8,tcl = -0.5)
axis(2, at = c(-6,-4,-2,0,2), col = "black", labels = c("","-4","-2","0","2"), las = 2,cex.axis= 2.8)        

#plot difference for no fixation, observed, and ecosystem fixation
points (c(0:300),c(0,Nofixation), type = "l", col = "cyan3", lwd = 4)
points (c(0:300),observed, type = "l", col = "gray", lwd = 4)
points (c(0:300),c(0,Ecosystem), type = "l", col = "blue", lwd = 4)
points (c(0:300),c(0,Fixation), type = "l", col = "darkorange", lwd = 4)
 
text(305,1.7,"b",cex=2.5)

#_____________________________________________
#END:Create plot Fig 2b - Difference between observed and predicted Plant C accumulation
#_____________________________________________
#_____________________________________________
#START:Create plot Fig 2c - Difference between observed and predicted Fixed N
#_____________________________________________


#____________________________
# Calculate change in N fixation from observed. Multiply by 10000 to convert units from m2 to ha

Fixation<-A$fixation[A$ifile==2]*10000 - ObsFixation[1:300]
Nofixation<-A$fixation[A$ifile==3]*10000- ObsFixation[1:300]
observed<-ObsFixation-ObsFixation
Ecosystemfixation<-A$fixation[A$ifile==4]*10000- ObsFixation[1:300]

#____________________________

#format plot
par(mar=c(7,8.3,4,1))  #c(bottom, left, top, right)
Ticks1<-c(0,50,100,150,200,250,300) 
xlabels<- c("0","50","100","150","200","250","300")

#plot difference for fixation, no fixation, observed, and ecosystem fixation
plot (c(0:300),c(0,Fixation), type = "l", col = "black", xlab = "", , ylab=expression(paste(Delta ~ "N fixed (kg N",sep = " ", ha^-1, sep = " ",")")),ylim=c(-40,90), xlim=c(1,300), lwd = 4, ,axes=FALSE, cex.lab = 2.8)
points (0:300,observed, type = "l", col = "gray", lwd = 4)
points (c(0:300),c(0,Fixation), type = "l", col = "darkorange", lwd = 4)
points (c(0:300),c(0,Nofixation), type = "l", col = "cyan3", lwd = 4)
points (c(0:300),c(0,Ecosystemfixation), type = "l", col = "blue", lwd = 4)

#plot axis
axis(1,at= Ticks1,col = "black", labels = xlabels, cex.axis = 2.8,tcl = -0.5)
axis(2, at = c(-40,"",-0,"",40,"",80), col = "black", labels = c(-40, -20,-0,20,40,60,80), las = 2,cex.axis = 2.3)        
#add labels 
text(305,85,"c",cex=2.5)
mtext("Forest age (years)"  , side=1, line=5, cex=2.2)


#_____________________________________________
#END:Create plot Fig 2c - Difference between observed and predicted Fixed N
#_____________________________________________


#finish creating eps file
dev.off()


########################################################
#END:Graph Fig 2
##########################################################

