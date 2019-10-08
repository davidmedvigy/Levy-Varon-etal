# Set the working directory that contains the experiment results.
setwd("~/Desktop/Figure3")

############################################
#START: Load in data used to create Figure 3
############################################

A<-read.csv("Results",header=TRUE,sep="")

############################################
#END: Load in data used to create Figure 3
############################################

########################################################################
# START: DO CALCUALTIONS FOR Figure 3a
########################################################################
#__________________________________________________
# Percent change in carbon due to fixation
#__________________________________________________
# ((Fixer - Null_fixer)/Null_Fixer)*100. ifile 5 = Fixer simulation and ifile 6 = Null_fixer simulation
 perc_due_to_fixation<-((A$PlantC[A$ifile==5][1:300]-A$PlantC[A$ifile==6][1:300])/A$PlantC[A$ifile==6][1:300])*100
 
#_____________________________________
# Percent change in carbon due to biodiveristy
#_____________________________________
# ((No Fixation - Null_fixer)/Null_fixer)*100.  ifile 3 = No Fixation simulation and ifile 6 = Null_fixer simulation
perc_due_to_diversity<-((A$PlantC[A$ifile==3]-A$PlantC[A$ifile==6][1:300])/A$PlantC[A$ifile==6][1:300])*100

#There is a break in the y axis by 20 units(The break is from -20 to -40) so the values in perc_due_to_diversity that are less then the break (< -20) need to be rescaled before they are graphed 
# For example at tick mark location -40 we are labeling it -60.. so we loose 20 units on the yaxis
perc_due_to_diversity[2]<-perc_due_to_diversity[2]+20
perc_due_to_diversity[3]<-perc_due_to_diversity[3]+20

#_____________________________________
# Percent change in C due to PFT-fixation interaction
#_____________________________________

#% change in C due to interaction =  (Individual level fixation - Fixer - No Fixation + Null_fixer)/Null_fixer *100  
#ifile 2 = Individual level fixation,ifile 5 = Fixer simulation, ifile 3 = No Fixation, ifile 6 = Null_fixer simulation

FixEffect<- ((A$PlantC[A$ifile==2][1:300] - A$PlantC[A$ifile==5][1:300] - A$PlantC[A$ifile==3][1:300] + A$PlantC[A$ifile==6][1:300])/A$PlantC[A$ifile==6][1:300]) *100



########################################################################
# END: DO CALCUALTIONS FOR Figure 3a
########################################################################

########################################################
#START:Graph Fig 3
##########################################################

# Create an eps file for the graph
postscript(file="~/Desktop/Fig3.eps",width=14,height=8,horizontal = FALSE, onefile = FALSE, paper = "special")

#_______________________________________
#Start:Format parameters
#_______________________________________
par(mar=c(7,8,3,0))  #c(bottom, left, top, right)
par(mgp=c(5.2,2,0))
par(fig=c(0,0.6,0,1), new=TRUE)

#define x axis tick locations and labels
Ticks1<-c(0,50,100,150,200,250,300) 
xlabels<- c("0","50","100","150","200","250","300")

#_______________________________________
#END:Format parameters
#_______________________________________

#____________________________________________
#START:Create plot Fig 3a - % changein C storage 
#_____________________________________________

#Plot percent change due to biodiversity
plot (1:300,perc_due_to_diversity, type = "l", col = "black", xlab = "Forest age (years)", , ylab=expression(paste( "Increase in plant C (%)")),ylim=c(-45,100), xlim=c(1,300), lwd = 4, ,axes=FALSE, cex.lab = 2.5)

# add percent change due to N2 fixation
points(1:300,perc_due_to_fixation, type = "l", col = "red",lwd = 4)
#add percent change due Fixation-biodiversity interaction 
points(1:300,FixEffect,type="l",col="royalblue1", lwd=4)

# Add axis. Note,we create a break on the y axis from from -20 to -40.
axis(2, at = c(-60,-40,-25,0,25,50,75,100), col = "black", labels = c("-80","-60","-25","0","25","50","75","100"), las = 2,cex.axis = 2.5)        
axis(1,at= Ticks1,col = "black", labels = xlabels, cex.axis = 2.5,tcl = -0.5)

# add a horizontal line at 0
abline(h=0)

# add slash marks to y axis of the graph for the break. 
library(plotrix)
axis.break(2,-32,style="slash")

#add legend
legend("topright",c("Fixation effect","Biodiversity effect","Fixation-biodiversity
interaction"),col = c("red","black","royalblue1"), bty = "n", lty = c(1,1,1,1), lwd = c(3,3,3), cex = 1.7)
#label panel a
text(290,100,"a", cex = 2)
#____________________________________________
#END:Create plot Fig 3a - % changein C storage 
#_____________________________________________
#____________________________________________
#START:Create plot Fig 3b 
#_____________________________________________
#_______________________________________
#START: Format parameters for fig 3b
#_______________________________________
par(mar=c(7,8,3,2))  #c(bottom, left, top, right)
par(mgp=c(4,2,0))
par(fig=c(0.6,1,0.5,1), new=TRUE) # x,y coordinates.. x and y ranges are 0-1 

#define x axis tick locations and labels
xticks<- c("0","50","100","150","200","250","300")
xlabels<- c("0","","100","","200","","300")
#_______________________________________
#END:Format parameters for fig 3b 
#Successional dynamics for the individual level fixation simulation
#_______________________________________

# plot aboveground biomass for late-successional PFT
plot (c(1:length(A$AGB4[A$ifile==2])),A$AGB4[A$ifile==2], type = "l", col = "black", xlab = "Forest age (years)", ylab="", ylim = c(0,9.5),cex.axis = 2, cex.lab = 2,axes=FALSE,lty=2,lwd=4)
#add aboveground biomass for mid-successional PFT
points(c(1:length(A$AGB3[A$ifile==2])),A$AGB3[A$ifile==2],type = "l", col = "black", lwd = 6,lty=3)
#add aboveground biomass for late-successional PFT
points(c(1:length(A$AGB4[A$ifile==2])),A$AGB4[A$ifile==2],type = "l", col = "black", lwd = 4,lty=2)
#add aboveground biomass for the fixer PFT
points(c(1:length(A$AGB30[A$ifile==2])),A$AGB30[A$ifile==2],type = "l", col = "black", lwd = 4)
#add aboveground biomass for early-successional PFT
points(c(1:length(A$AGB2[A$ifile==2])),A$AGB2[A$ifile==2],type = "l", col = "black", lwd = 2, lty = 1)

#label y axis
mtext(expression(paste("Plant C (kg C",sep = " ", m^-2,sep = " ",")")), side = 2,  line = 3,cex=2) 

#add legend
legend(0,10,c("Early- successional","Mid- successional","Late- successional","Fixer"),col = c("black","black","black","black"), bty = "n", lty = c(1,3,2,1), lwd = c(2,3,3,4))
#add axis
axis(2, at = c(0,2,4,6,8), col = "black", labels = c(0,"", "4","","8"), las = 2,cex.axis = 2)        
axis(1,at= xticks,col = "black", labels = xlabels, cex.axis = 1.8,tcl = -0.5)

#label panel b
text(290,8.8,"b", cex = 2)
#____________________________________________
#END:Create plot Fig 3b 
#_____________________________________________
#_______________________________________
#START: Format paramaters for fig 3c 
#Successional dynamics for the Fixer simulation
#_______________________________________
#_______________________________________
#START: Format paramaters for fig 3c
#_______________________________________
par(fig=c(0.6,1,0,0.5), new=TRUE) # x,y cordinates.. x and y ranges are 0-1 
par(mar=c(7,8,3,2))  #c(bottom, left, top, right)
par(mgp=c(4,2,0))
#_______________________________________
#END: Format parameters for fig 3b
#_______________________________________

# plot aboveground biomass for the fixer PFT
plot (c(1:length(A$AGB30[A$ifile==5])),A$AGB30[A$ifile==5], type = "l", col = "black", xlab = "Forest age (years)", ylab="", ylim = c(0,9),cex.axis = 2, cex.lab = 2,axes=FALSE, lwd = 4)
# plot aboveground biomass for the Null fixer PFT
points(c(1:length(A$AGB35[A$ifile==5])),A$AGB35[A$ifile==5],type = "l", col = "black", lwd = 4,lty=6)


#label y axis
mtext(expression(paste("Plant C (kg C",sep = " ", m^-2,sep = " ",")")), side = 2,  line = 3,cex=2) 
#add axis
axis(2, at = c(0,2,4,6,8), col = "black", labels = c(0,"", "4","","8"), las = 2,cex.axis = 2)        
axis(1,at= Ticks1,col = "black", labels = xlabels, cex.axis = 1.8,tcl = -0.5)

#label panel c
text(290,7.8,"c", cex = 2)

#add legend
legend(0,9,c("Fixer","Null-Fixer"),col = c("black","black"), bty = "n", lty = c(1,6,1,1), lwd = c(2.5,2.2,2.5,2.5))

#_______________________________________
#END: Format parameters for fig 3c 
#Successional dynamics for the Fixer simulation
#_______________________________________

#finish creating eps file
dev.off()
########################################################
#START:Graph Fig 3
##########################################################
