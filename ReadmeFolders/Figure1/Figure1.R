# Set the working directory that contains the experiment results.

setwd("~/Desktop/Figure1")

############################################
#START: Load in data used to create Figure 1
############################################
# load in the Results file
a<-read.csv("Results",header=TRUE,sep="")


# load in observed successional data using observations from Sarah Batterman at Agua Salud 
Relative_BasalArea_observed_earlys<-c(99.23,0.77,0.00)
Relative_BasalArea_observed_mids<-c(0.00,97.83,2.17)
Relative_BasalArea_observed_lates<-c(0.00,14.78,85.22)
Relative_BasalArea_observed_fixers<-c(14.50,54.14,31.36)

# format observed successional data for barplot
Relative_BasalArea_observed<-cbind(Relative_BasalArea_observed_earlys,Relative_BasalArea_observed_mids,Relative_BasalArea_observed_lates,Relative_BasalArea_observed_fixers)
Relative_BasalArea_observed<-as.matrix(Relative_BasalArea_observed)
colnames(Relative_BasalArea_observed) <- c("","","","")

# load in the observed C values from Batterman et al. (2014)
observed<-read.csv("AS_PlantC.csv",header = TRUE)
ObsCarbon<-observed$Carbon/10000 # Divide by 10000 to convert units from ha to m2

#Load in forest basal area observations from Agua Salud
BA<-read.csv("AS_ForestBA.csv")
############################################
#END: Load in data used to create Figure 1
############################################

##########################################################
#START:Calculate predicted relative basal area for Fig 1d 
##########################################################

################
# START- YRS 5,30,80
###############

# Define the simulation to use for the results. 2 corresponds to the individual fixation simulation
year=2

#Calcualate total basal area for each pft at years 5,30,300
BA2_TOTAL<-a$BA2[a$ifile==year][5] +a$BA2[a$ifile==year][30] +a$BA2[a$ifile==year][300] 
BA3_TOTAL<-a$BA3[a$ifile==year][5] +a$BA3[a$ifile==year][30] +a$BA3[a$ifile==year][300] 
BA4_TOTAL<-a$BA4[a$ifile==year][5] +a$BA4[a$ifile==year][30] +a$BA4[a$ifile==year][300] 
BA30_TOTAL<-a$BA30[a$ifile==year][5] +a$BA30[a$ifile==year][30] +a$BA30[a$ifile==year][300] 

#Calculate relative abundance of each pft at year 5
BA2_Relative_age5<-(a$BA2[a$ifile==year][5]/BA2_TOTAL)*100
BA3_Relative_age5<-(a$BA3[a$ifile==year][5]/BA3_TOTAL)*100
BA4_Relative_age5<-(a$BA4[a$ifile==year][5]/BA4_TOTAL)*100
BA30_Relative_age5<-(a$BA30[a$ifile==year][5]/BA30_TOTAL)*100

#Calculate relative abundance of each pft at year 30
BA2_Relative_age30<-(a$BA2[a$ifile==year][30]/BA2_TOTAL)*100
BA3_Relative_age30<-(a$BA3[a$ifile==year][30]/BA3_TOTAL)*100
BA4_Relative_age30<-(a$BA4[a$ifile==year][30]/BA4_TOTAL)*100
BA30_Relative_age30<-(a$BA30[a$ifile==year][30]/BA30_TOTAL)*100

#Calculate relative abundance of each pft at year 300
BA2_Relative_age300<-(a$BA2[a$ifile==year][300]/BA2_TOTAL)*100
BA3_Relative_age300<-(a$BA3[a$ifile==year][300]/BA3_TOTAL)*100
BA4_Relative_age300<-(a$BA4[a$ifile==year][300]/BA4_TOTAL)*100
BA30_Relative_age300<-(a$BA30[a$ifile==year][300]/BA30_TOTAL)*100

# Format data for bargraph 
Col_Early<-c(BA2_Relative_age5,BA2_Relative_age30,BA2_Relative_age300)
Col_Mid  <-c(BA3_Relative_age5,BA3_Relative_age30,BA3_Relative_age300)
Col_late <-c(BA4_Relative_age5,BA4_Relative_age30,BA4_Relative_age300)
Col_fixer <-c(BA30_Relative_age5,BA30_Relative_age30,BA30_Relative_age300)

Relative_BasalArea_Predicted<-matrix(c(Col_Early,Col_Mid ,Col_late,Col_fixer),ncol=4)
Relative_BasalArea_Predicted

Relative_BasalArea_PREDICTED<-as.matrix(Relative_BasalArea_Predicted)
colnames(Relative_BasalArea_PREDICTED) <- c("","","","")
Relative_BasalArea_PREDICTED
########################################################
#END:Calculate predicted relative basal area for Fig 1d 
##########################################################

########################################################
#START:Graph Fig 1
##########################################################

#_______________________________________
# Start: Format graph parameters
#_______________________________________

# Create an eps file for the graph
postscript(file="~/Desktop/Fig1.eps",width=12,height=10,horizontal = FALSE, onefile = FALSE, paper = "special")

#Set graph paramaters
par(mfrow=c(2,2))
par(mar=c(7,8.3,3,2))  #c(bottom, left, top, right)
par(mgp=c(5,2,0))

#Define tickmark locations and labels
Ticks1<-c(0,50,100,150,200,250,300) 
xlabels<- c("0","50","100","150","200","250","300")
#_______________________________________
#End:Format graph parameters
#_______________________________________

#_____________________________________________
#START:Create plot Fig 1a - Plant C accumulation
#_____________________________________________

#Plot observed C curve
plot (observed$Year,ObsCarbon, type = "l", col = "gray", xlab = "Forest age (years)",  ylab=expression(paste("Plant C (kg C",sep = " ", m^-2, sep = " ",")")),ylim=c(0,15), xlim=c(1,300), lwd = 5, ,axes=FALSE, cex.lab = 3)

#Add axis to the graph
axis(1,at= Ticks1,col = "black", labels = xlabels, cex.axis = 3,tcl = -0.5)
axis(2, at = c(0,2,4,6,8,10,12,14), col = "black", labels = c(0,"", "4","","8","","12",""), las = 2,cex.axis = 3)        


# Add Individual level fixation predictions
points (c(1:length(a$PlantC[a$ifile==2])),a$PlantC[a$ifile==2], type = "l", col = "darkorange", lwd = 5)

#label panel a
text(290,14,"a", cex = 3)

# Add observed carbon points with error bars to the graph. Values are divided by 10000 to convert units from ha to m2
points(0,0, pch = 16, cex = 2)
points(5,16285/10000, pch = 16, cex = 2)
points(12,45630/10000, pch = 16, cex = 2)
points(30,104384/10000, pch = 16, cex = 2)
points(80,81257/10000, pch = 16, cex = 2)
points(300,127806/10000, pch = 16, cex = 2)
arrows(5,16285/10000-3247/10000,5,16285/10000+3247/10000,length=0.1,angle=90,code=3)
arrows(12,45630/10000-2872/10000,12,45630/10000+2872/10000,length=0.1,angle=90,code=3)
arrows(30,104384/10000-12721/10000,30,104384/10000+12721/10000,length=0.1,angle=90,code=3)
arrows(80,81257/10000-2437/10000,80,81257/10000+2437/10000,length=0.1,angle=90,code=3)

#_____________________________________________
#End:Create plot Fig 1a - Plant C accumulation
#_____________________________________________
#_____________________________________________
#START:Create plot Fig 1b-Ecosytem N2 fixation
#_____________________________________________
#Plot formatting
par(mar=c(7,8.3,3,2))  #c(bottom, left, top, right)
par(mgp=c(5,2,0))

#Plot N2 fixation for individual simulation. The units are converted from m2 to ha my multiplying by 10000
plot (c(1:length(a$fixation[a$ifile==2])),a$fixation[a$ifile==2]*10000, type = "l", col = "darkorange", xlab = "Forest age (years)", ylab=expression(paste(N[2],sep = " ", "fixed (kg N",sep = " ", ha^-1,, sep = " ",")")),ylim=c(0,85), xlim=c(1,300), lwd = 3, ,axes=FALSE, cex.lab = 3)

#plot N2 fixation for ecoysystem simulation
points(c(1:length(a$fixation[a$ifile==4])),a$fixation[a$ifile==4]*10000, col = "blue",type = "l", lwd = 4)

#plot axis
axis(1,at= Ticks1,col = "black", labels = xlabels, cex.axis = 3,tcl = -0.5)
axis(2, at = c(0,10,20,30,40,50,60,70,80), col = "black", labels = c(0,"","20","","40","","60","","80"), las = 2,cex.axis = 3)  

#label panel b
text(290,82,"b", cex = 3)

#plot N2 Fixation( in ha) observations and error bars
points(0,0, pch = 16, cex = 2)
points(5,10, pch = 16, cex = 2)
points(12,29, pch = 16, cex = 2)
points(30,11, pch = 16, cex = 2)
points(80,0.3, pch = 16, cex = 2)
points(300,2.2, pch = 16, cex = 2)
arrows(5,9.7-4,5,9.7+4,length=0.1,angle=90,code=3)
arrows(12,29.1-11.8,12,29.1+11.8,length=0.1,angle=90,code=3)
arrows(30,11.1-5.9,30,11.1+5.9,length=0.1,angle=90,code=3)
arrows(80,0.3-0.1,80,0.3+0.1,length=0.1,angle=90,code=3)

#_____________________________________________
#END:Create plot Fig 1b-Ecosytem N2 fixation
#_____________________________________________
#_____________________________________________
#START:Create plot Fig 1c -Forest Basal area
#_____________________________________________

# Format plot
par(mar=c(7,8.3,3,2))  #c(bottom, left, top, right)
par(mgp=c(5,2,0))

#Plot predicted basal are for individual fixation simulation
plot (1:length(a$basal_area[a$ifile==2]),a$basal_area[a$ifile==2], type = "l", col = "darkorange", xlab = "Forest age (years)", ,ylab=expression(paste("Basal area (",sep = " ", m^2, sep = " ", ha^-1,")")),ylim=c(0,40), xlim=c(1,300), lwd = 4, ,axes=FALSE, cex.lab = 3)

#add in axis
axis(1,at= Ticks1,col = "black", labels = xlabels, cex.axis = 3,tcl = -0.5)
axis(2, at = c(0,5,10,15,20,25,30,35,40), col = "black", labels = c(0,"","10","", "20","","30","","40"), las = 2,cex.axis = 3)    
    
#label panel c
text(300,39,"c", cex = 3)

#Plot observations
points(BA$age,BA$BA,pch = 16, cex = 2)
#_____________________________________________
#END:Create plot Fig 1c -Forest Basal area
#_____________________________________________

#_____________________________________________
#START: Create plot Fig 1d -PFT relative basal area
#_____________________________________________

#format graph
par(mar=c(7,9,3,2))  #c(bottom, left, top, right)
par(mgp=c(5,2,0))
par(lwd=2.5)

# creat barplot of observed data 
barplot(Relative_BasalArea_observed,beside= TRUE,ylim = c(0,113), col =c("gray91","gray91","gray91","gray91"), ylab = "", cex.lab = 2.8, axes = FALSE, cex.names = 3,xlab = "")


# Plot Predicted data by overlaying shading lies on top of the observations
barplot(Relative_BasalArea_PREDICTED,beside=TRUE,ylim =c(0,113), , ylab = "", cex.lab = 3, axes = FALSE, ,add = TRUE, angle=c(45),density=5,
xlab = "",col=c("darkorange","darkorange","darkorange","darkorange"))

# add in axis and labels
axis(2, at = c(0,10,20,30,40,50,60,70,80,90,100), col = "black", labels = c(0,"",20,"",40,"",60,"",80,"",100), las = 2,cex.axis = 3)  
text(27,102,"d", cex = 3)     
mtext("Relative basal area (%)", side = 2,  line = 6.5,cex=2.3) 
mtext("Early", side = 1,  line =4.5, cex = 1.8, adj=0) 
mtext("Mid", side = 1,  line = 4.5, cex = 1.8, adj=0.35)
mtext("Late", side = 1,  line = 4.5, cex = 1.8,adj = 0.65)
mtext("Fixer", side = 1,  line = 4.5, cex = 1.8,adj = 0.95)

# Add legend. 
legend(7.5,120, legend=c("observed","predicted"), cex=1.8, fill = c("gray91","white"),bty = "n")
legend(7.5,120, legend=c("observed","predicted"), cex=1.8, angle=c(100,45), density=c(0,15), fill = c( "gray91","darkorange"),bty= "n")

# add labels to each bar
lablist<-rep(c("5 yrs  ","30 yrs ","300 yrs "),4)
y<-rep(-3,12)
x<-c(1:3,5:7,9:11,13:15)
text(x,y, labels=lablist, srt=45, pos=1, xpd=TRUE, cex = 1.2)  
#label panel d
text(16,105,"d", cex = 3)
#_____________________________________________
#END: Create plot Fig 1d -PFT relative basal area
#_____________________________________________

#finish creating eps file
 dev.off()
 
########################################################
#END:Graph Fig 1
##########################################################
