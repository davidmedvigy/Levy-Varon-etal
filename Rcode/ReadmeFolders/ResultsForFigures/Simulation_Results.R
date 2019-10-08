#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
#THIS DOCUMENT Creates a dataframe containing results from all model simulations used in Levy et al. (201?). 
#This dataframe will be used to create figures in both the main paper and the supplementary sections
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

################################################################################################################
#START: SETUP information needed to extract model results
################################################################################################################
# Set the working directory that contains the model result files.
setwd("~/Desktop/Final_simulations")
# Define the number of number of years for each simulation. 
nyearz<-300
#ID will be used to create a unique identifier for each simulation in the model. 
ID= 1

# Create a vector of simulation names. This will be used to extract results from all simulations in the same loop (below).
filename<-c("IndividualFixation-Y-","NoFixation-Y-","EcosystemFixation-Y-","Fixer-Y-","Null_Fixer-Y-","NoDisturbance_IndividualFixation-Y-","Fertilization_Experiment-Y-") 

##############################################################################################################
END: SETUP information needed to extract model results
##############################################################################################################
##############################################################################################################
#START: Create empty vectors that will be filled in with results
##############################################################################################################
PlantC<-rep(0,nyearz*length(filename))
SoilN<- rep(0,nyearz*length(filename))
fixation<-rep(0,nyearz*length(filename))
AGB_pft2<-rep(0,nyearz)
AGB_pft3<- rep(0,nyearz)
AGB_pft4<- rep(0,nyearz)
AGB_pft30<-rep(0,nyearz)
AGB_pft35<-rep(0,nyearz)
basal_area<-rep(0,nyearz)
BA_pft30<-rep(0,nyearz)
BA_pft2<-rep(0,nyearz)
BA_pft3<-rep(0,nyearz)
BA_pft4<-rep(0,nyearz)
MineralizedSoilN<-rep(0,nyearz)
balive<-rep(0,nyearz*length(filename))
bdead<-rep(0,nyearz*length(filename))
bstorage<-rep(0,nyearz*length(filename))

#Create a dataframe that will be filled with results
Results<-NULL 

##############################################################################################################
#START: Extract relevant results from each simulation file for each of the 300 years and compile all 
#results into one dataframe
##############################################################################################################

# for each of the 7 simulations and for each year in each simulation:
  for(ifile in 1:length(filename)){
     for (iyear in 1:nyearz){
# extract the file name using the filename vector created above
	 fname<-paste(filename[ifile],1999+iyear,"-00-00-000000-g01.h5",sep="")		
#print the file name and use the hdf5 library to read the file and temporarily save the file as x 
   	 print(fname)
	 library(hdf5)
	 x<-hdf5load(fname,load=F,tidy=T)

#Each time a new simulation is read, set all variables to 0.	
	if(ifile>1){	
	PlantC[iyear]<- 0
	SoilN[iyear]<- 0
	fixation[iyear]<- 0
	AGB_pft2[iyear]<- 0
	AGB_pft3[iyear]<- 0
	AGB_pft4[iyear]<- 0
	AGB_pft30[iyear]<- 0
	AGB_pft35[iyear]<- 0
	basal_area[iyear]<- 0
	BA_pft30[iyear]<- 0
	BA_pft2[iyear]<- 0
	BA_pft3[iyear]<- 0
	BA_pft4[iyear]<- 0
	MineralizedSoilN[iyear]<- 0
	balive[iyear]<- 0
    bdead[iyear]<- 0
    bstorage[iyear]<- 0
               }


#Identify each patch in the model using x$area   
	patches<-length(x$AREA)
    library(car)
	require(car)
#For each patch extract the mineralized soil N
	for(i in 1:patches){
		MineralizedSoilN[iyear]<- MineralizedSoilN[iyear] + x$MINERALIZED.SOIL.N[i] *x$AREA[i] 
#For each cohort in each patch sum the biomass (comprised of balive,bdead,and bstorage)			
		if(x$PACO.N[i]>0){
			for(j in x$PACO.ID[i]:(x$PACO.ID[i]+x$PACO.N[i]-1)) {
				balive[iyear]<-balive[iyear]+(x$BALIVE[j] * x$AREA[i] *x$NPLANT[j])
				bdead[iyear]<-bdead[iyear]+(x$BDEAD[j] * x$AREA[i]	*x$NPLANT[j])
				bstorage[iyear]<-bstorage[iyear]+(x$BSTORAGE[j] * x$AREA[i]	*x$NPLANT[j])

				                                                } #closes the cohort loop	

	                      }
	                     }# closes the patch loop

	  
PlantC[iyear] <-(balive[iyear] + bdead[iyear] +bstorage[iyear])
SoilN[iyear]<-x$SOILN
fixation[iyear]<-x$total.N.Fixation
AGB_pft2[iyear]<-sum(x$AGB[,,2])
AGB_pft3[iyear]<-sum(x$AGB[,,3])
AGB_pft4[iyear]<-sum(x$AGB[,,4])
AGB_pft30[iyear]<-sum(x$AGB[,,30])
AGB_pft35[iyear]<-sum(x$AGB[,,35])

BA_pft30[iyear]<-sum(x$BASAL.AREA[,,30])
BA_pft2[iyear]<-sum(x$BASAL.AREA[,,2])
BA_pft3[iyear]<-sum(x$BASAL.AREA[,,3])
BA_pft4[iyear]<-sum(x$BASAL.AREA[,,4])

basal_area[iyear]<-sum(x$BASAL.AREA)


data<-data.frame(ifile=ifile+ID,iyear = iyear, SoilN[iyear], PlantC[iyear],AGB_pft2[iyear],AGB_pft3[iyear],AGB_pft4[iyear],AGB_pft30[iyear],AGB_pft35[iyear],fixation[iyear],basal_area[iyear],
BA_pft30[iyear],BA_pft2[iyear],BA_pft3[iyear],BA_pft4[iyear],MineralizedSoilN[iyear]) 

Results<-rbind(Results,data) 

                     }
 	}
 	
##############################################################################################################
#END: Extract relevant results from each simulation for each of the 300 years and compile all results 
#into one dataframe
##############################################################################################################

##############################################################################################################
#START: Define names for each simulation based on the unique ID and attach them to the results  #############################################################################################################
#add an extra column in the dataset copying the unique file ID (Results$ifile). 
Results$SimulationName<-Results$ifile
#Assign simulation name based on the unique ID
Results$SimulationName[Results$SimulationName==2] <- " Individual Fixation"
Results$SimulationName[Results$SimulationName==3] <- " No Fixation"
Results$SimulationName[Results$SimulationName==4] <- " Ecosystem Fixation"
Results$SimulationName[Results$SimulationName==5] <- " Fixer"
Results$SimulationName[Results$SimulationName==6] <- " Null Fixer"
Results$SimulationName[Results$SimulationName==7] <- " No Disturbance"
Results$SimulationName[Results$SimulationName==8] <- " Fertilization Experiment"

#Rename all columns in the result file	
names(Results)<-c("ifile","iyear","SoilN","PlantC","AGB2","AGB3","AGB4","AGB30","AGB35","fixation","basal_area","BA30","BA2","BA3","BA4","MSN","SimulationName")


#Define the location to save the result file. This file will be used to create the graphs in he paper and supplemental documents
setwd("~/Desktop/ResultsForFigures") 
#write the result file
write.table(Results,file = "Results")

##############################################################################################################
#END: Define names for each set of results and save the results to the computer
##############################################################################################################

