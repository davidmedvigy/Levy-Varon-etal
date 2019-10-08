#_______________________________________________________
#This file assigns a wood density to each tree observed in
#The Agua Salud 5 yr old plots
#_______________________________________________________

#_______________________________________________________
#START: Load in relevant datasets. Agua Salud, BCI, and the global wood density database
#***The global wood density database is from Zanne, A.E., Lopez-Gonzalez, G.*, Coomes, D.A., Ilic, J., Jansen, S., Lewis, S.L., Miller, R.B., Swenson, N.G., Wiemann, M.C., and Chave, J. 2009. Global wood density database. Dryad. Identifier: http://hdl.handle.net/10255/dryad.235. 
#***The Barro Colorado Island(BCI) dataset is from  Wright et al. (2010) 
#_______________________________________________________
#set directory where files are located
setwd("~/Desktop/PFTinitialization")

# Open the Agua Salud Dataset
WD_AG_a<- read.csv("5yr_UniqueTrees.csv", header=TRUE, sep = ",")

#load in global wood density database
WDdata<- read.csv("WDdatabase.csv", header=TRUE, sep = ",")
names(WDdata)

# load in the BCI dataset
BCI<- read.csv("BCIWD.csv", header=TRUE, sep = ",")

#  Agua Salud Dataset on 5 year old plots
W<- read.csv("AS_age5_init.csv", header=TRUE, sep = ",") 

#_______________________________________________________
#END:Load in relevant datasets
#_______________________________________________________

#_________________________________________________________________
#START:Format and clean up datasets to use for analysis
#_________________________________________________________________

# Agua Salud dataset : remove rows that are missing family/genus/species information, These 2 trees cannot be identified.
#identify plants that do not have information about the family they belong to
noinfo<-WD_AG_a[is.na(WD_AG_a$Family),]
noinfo
#save relevant columns for the end 
noinfo<-data.frame(noinfo$code,noinfo$Family)
noinfo

#remove these trees from the Agua Salud dataset
WD_AG<- WD_AG_a[!is.na(WD_AG_a$Family),]

length(WD_AG$code)

# BCI dataset:
#Remove plants that do not have a wood density values

# First check to see if all trees have wood density values. They do not based on -99 values 
BCI$WSG

#Remove plants from the dataset that do not have wood density observations
BCI<-subset(BCI,BCI$WSG != -99 )
BCI$WSG

# keep only species information and wood density from the BCI dataset 
BCI<-BCI[,3:7]
#give the columns easily identifiable names
names(BCI)<-c("Genus","Species", "Binomial" ,"Family","WD")

#_________________________________________________________________
#END:Format and clean up datasets to use for analysis
#_________________________________________________________________


#_________________________________________________________________
#START: First assign trees from Agua Salud wood density values observed at BCI. 
#Then assign wood density values using the global wood density database
#_________________________________________________________________

#_________________________________________________________________
#START: Assign trees from Agua Salud wood density values observed at BCI. 
#_________________________________________________________________

#create a new dataset(z) that merges the Agua Salud dataset with the BCI observations if the binomial name is the same

z<-merge(WD_AG,BCI,by= c("Binomial"), all.x = TRUE)
names(WD_AG)
names(z) # This contains all columns fo WD_AG and BCI datasets
length(z$code) # this is the same length of the WD_AG dataset

 # How many trees have Wood density values from the BCI/Wright 2010 database?
#24Trees
# identify the lines in z where wood density values are provided (or from a coding perspective, where wood density values do not = na). The wood density values from BCI are WD.y
BCI_BinomailMatch<- z[!is.na(z$WD),]
length(BCI_BinomailMatch$WD) # 24 trees
# check that all of the values look reasonable - Looks good
BCI_BinomailMatch$WD
 # look at the dataset - Looks good
BCI_BinomailMatch
#restrict dataset to columns of interest
BCI_BinomailMatch<-data.frame(BCI_BinomailMatch$code,BCI_BinomailMatch$WD)
names(BCI_BinomailMatch)<-c("code","wood density")
BCI_BinomailMatch
#_________________________________________________________________
#END:Assign trees from Agua Salud wood density values observed at BCI. 
#_________________________________________________________________

#_________________________________________________________________
#START:Identify trees that still need wood density values assigned
#_________________________________________________________________

# now isolate the dataframe that still does not have WD values
#93 trees

# identify the lines in z where wood density values  = NA. The wood density values from BCI are WD.y
WDneeded<-z[is.na(z$WD),]
length(WDneeded$WD) #93 trees
# check that these trees do not have wood density values
WDneeded$WD
WDneeded<-WDneeded[,1:3]
length(WDneeded$code)
#_________________________________________________________________
#END:Identify trees that still need wood density values assigned
#_________________________________________________________________

#_____________________________________________________________________________________________
#START:Assign the remaining trees wood density values using the global wood density database. #_____________________________________________________________________________________________
#_____________________________________________________________________________________________
#START: MATCH BASED ON GENUS AND SPECIES 
#_____________________________________________________________________________________________
#merge the new data set containing trees that still need a wood density value (WDneeded)  with the global wood database (WDdata)
#all.x = TRUE means extra rows will be added to the output, one for each row in WDneeded that has no matching row in WDdata then 
GlobalMerged<-merge(WDneeded,WDdata,by= "Binomial", all.x = TRUE)
#check merge, looks good
GlobalMerged[1:10,]

# isolate columns of interest
GlobalMerged2<- data.frame(GlobalMerged$Binomial, GlobalMerged$code,GlobalMerged$Wood_density)

# Check to see if merging  WDneeded with WDdata added duplicate trees to the list. Yes it did. This means that in the global wood database, there were multiple wood density observations for the specific tree. Each observation was added to the merged data frame. 
length(GlobalMerged2$GlobalMerged.code) 
unique(GlobalMerged2$GlobalMerged.code)  

# CHECK:There are three observations for Alchornea latifolia (lines 6,7,8) with different wood densities.
GlobalMerged2[1:10,]

# Take the average wood density for each species with more then one observation.
# Define a list (called q1) of the unique trees
q1<-unique(GlobalMerged2$GlobalMerged.code)
length(q1) # 93


# ~~~~~~~~~~~~~~~~~~~~~ 
# START: Create a loop that goes through all of the trees and calculates the average wood density for trees that have multiple observations.  Create a new dataframe called avgWD that contains the average wood density and the tree code. This is based on matching the Binomial which includes the Genus and Species.Note:This loop takes a little while to run.
#1. MATCH BASED ON GENUS AND SPECIES 
# ~~~~~~~~~~~~~~~~~~~~~ 
avgWD <- NULL
total<-rep(0,length(q1))
number<-rep(0,length(q1))
meanWD<-rep(0,length(q1))

#for each unique code
for(i in 1:length(q1)){  
   #for each code in the Global merged dataset
	for (p in 1:length(GlobalMerged2$GlobalMerged.code)){
        # if the global code = the unique code
		if(GlobalMerged2$GlobalMerged.code[p] == q1[i]){
 
            #document how many times the code is replicated 
	    	number[i]<- number[i] + 1
            #add up the wood densities for each replicated tree
			total[i]<- total[i] + GlobalMerged2$GlobalMerged.Wood_density[p]
            #calculate the average wood density
			meanWD[i]<-total[i]/number[i]

		}
 	}	
 	    # save the code and mean wood density value and then put that in a data frame called avgWD
 		data<-data.frame(code=q1[i],meanWD[i])
    	avgWD<-rbind(avgWD, data)
}
    #Check how many lines are in the data frame and assign names to the columns
    avgWD # 93 lines
    names(avgWD)<-c("code","meanWD")  

 # avgWD  now contains an average wood density from the world database for for each unique ID 
 # How many trees have WD values from the database? (40 trees)
  newdata<-na.omit(avgWD) 
  length(newdata$meanWD) # 40 trees

  GlobalBinomialMatch<-avgWD[complete.cases(avgWD),] 
  length(GlobalBinomialMatch$code) 
 
  # isolate trees that do not have wood densities This leaves 53 trees that still need a wood density value.
  WDneeded2<-avgWD[!complete.cases(avgWD),]
  length(WDneeded2$meanWD) #53
  #Check - looks good
  WDneeded2
# ~~~~~~~~~~~~~~~~~~~~~ 
# END: Create a loop that goes through all of the trees and calculates the average wood density for trees that have multiple observations.  Create a new dataframe called avgWD that contains the average wood density and the tree code. This is based on matching the Binomial which includes the Genus and Species.Note:This loop takes a little while to run.
#1. MATCH BASED ON GENUS AND SPECIES 
# ~~~~~~~~~~~~~~~~~~~~~ 
#_____________________________________________________________________________________________
#END:MATCH BASED ON GENUS AND SPECIES 
#_____________________________________________________________________________________________

#_____________________________________________________________________________________________
#START:MATCH BASED ON GENUS 
#_____________________________________________________________________________________________

#Merge trees that still need wood density information with the initial agua salud dataset to identify the genus they belong to
WDneeded3<-merge(WDneeded2,WD_AG,by= "code", all.x= TRUE) 

#isolate columns of interest and simplify column names
WDneeded3<-data.frame(WDneeded3$code,WDneeded3$Genus)
names(WDneeded3)<-c("code","Genus")

#merge WDneeded3 with the global wood database (WDdata) by genus
GlobalMerged_Genus<-merge(WDneeded3,WDdata,by= "Genus", all.x = TRUE)
#check merge, looks good
GlobalMerged_Genus[1:10,]

# isolate columns of interest and simplify column names
GlobalMerged_Genus<- data.frame(GlobalMerged_Genus$code,GlobalMerged_Genus$Genus,GlobalMerged_Genus$Wood_density)
names(GlobalMerged_Genus)<-c("code","Genus","Wood_density")

# Define a list (called q2) of the unique trees
q2<-unique(GlobalMerged_Genus$code)
length(q2) # 53

# Take the average wood density for each species with more then one observation.
# ~~~~~~~~~~~~~~~~~~~~~ 
# START: Create another loop to assign the remaining 191 trees (in the WDneeded2 dataset) an average wood density using the BCI and global wood density database. This time, make an average wood density calculation for plants from the same genus.  ~~~~~~~~~~~~~~~~~~~~~ 

avgWD2 <- NULL
total<-rep(0,length(q2))
number<-rep(0,length(q2))
meanWD<-rep(0,length(q2))

#for each unique code/tree
for(i in 1:length(WDneeded3$code)){
	#for each code/tree in the GlobalMerged_Genus dataset
	for (p in 1:length(GlobalMerged_Genus$code)){
		#if the genus in the global database matches the genus in the WDneeded3 list
		if(GlobalMerged_Genus$Genus[p] == WDneeded3$Genus[i]){	
			#calculate the average wood density for all trees in the genus
	    	number[i]<- number[i] + 1
			total[i]<- total[i] + GlobalMerged_Genus$Wood_density[p]
			meanWD[i]<-total[i]/number[i]	
		}
 	}	
 	
 	 	# save the code/tree ID and mean wood density value and then put that in a data frame called avgWD2
 		data<-data.frame(code=WDneeded3$code[i],meanWD[i])
    	avgWD2<-rbind(avgWD2, data)
}
    
    avgWD2
    names(avgWD2)<-c("code","meanWD")

# ~~~~~~~~~~~~~~~~~~~~~ 
# END: Create another loop to assign the remaining 191 trees (in the WDneeded2 dataset) an average wood density using the global database. This time, make an average wood density calculation for plants from the same genus. 
# ~~~~~~~~~~~~~~~~~~~~~ 

# How many additional trees have WD values based on genus averages from the database? 39

#create a list of the trees assigned wood density values based on Genus  
 GlobalGenusMatch<-avgWD2[!is.na(avgWD2$meanWD),]
length(GlobalGenusMatch$meanWD) #39

# isolate trees that do not have wood densities #14 trees
WDneeded4<-avgWD2[!complete.cases(avgWD2),]
length(WDneeded4$code) 
#_____________________________________________________________________________________________
#END:MATCH BASED ON GENUS 
#_____________________________________________________________________________________________

#_____________________________________________________________________________________________
#START:MATCH BASED ON FAMILY 
#_____________________________________________________________________________________________

#Merge trees that still need wood density information with the initial agua salud dataset to identify the Family they belong to
WDneeded4<-merge(WDneeded4,WD_AG,by= "code", all.x= TRUE) 

#isolate columns of interest and simplify column names
WDneeded4<-data.frame(WDneeded4$code,WDneeded4$Family)
names(WDneeded4)<-c("code","Family")

#make sure family information is available for all of the trees. Looks good 
WDneeded4[is.na(WDneeded4$Family),]

#merge WDneeded4 with the global wood database (WDdata) by Family
GlobalMerged_Family<-merge(WDneeded4,WDdata,by= "Family", all.x = TRUE)
#check merge, looks good
GlobalMerged_Family[1:10,]

# isolate columns of interest and simplify column names
GlobalMerged_Family<- data.frame(GlobalMerged_Family$code,GlobalMerged_Family$Family,GlobalMerged_Family$Wood_density)
names(GlobalMerged_Family)<-c("code","Family","Wood_density")


# ~~~~~~~~~~~~~~~~~~~~~ 
# START: Create another loop to assign the remaining 43 trees (in the WDneeded4 dataset) an average wood density using the global wood density database. This time, make an average wood density calculation for plants from the same Family. NOTE: this takes a while to run
# ~~~~~~~~~~~~~~~~~~~~~ 

avgWD3 <- NULL
total<-rep(0,length(WDneeded4$code))
number<-rep(0,length(WDneeded4$code))
meanWD<-rep(0,length(WDneeded4$code))

#for each unique code/tree
for(i in 1:length(WDneeded4$code)){
	#for each code/tree in the GlobalMerged_Family dataset
	for (p in 1:length(GlobalMerged_Family$code)){
		#if the Family in the global database matches the Family in the WDneeded4 list
		if(GlobalMerged_Family$Family[p] == WDneeded4$Family[i]){	
		
			#calculate the average wood density for all trees in the Family
	    	number[i]<- number[i] + 1
			total[i]<- total[i] + GlobalMerged_Family$Wood_density[p]
			meanWD[i]<-total[i]/number[i]	
		}
 	}	
 	 	# save the code/tree ID and mean wood density value and then put that in a data frame called avgWD2
 	    data<-data.frame(code=WDneeded4$code[i],meanWD[i])
    	avgWD3<-rbind(avgWD3, data)
}
    
    avgWD3
    names(avgWD3)<-c("code","meanWD")
# ~~~~~~~~~~~~~~~~~~~~~ 
# END: Create another loop to assign the remaining 43 trees (in the WDneeded4 dataset) an average wood density using the global wood density database. This time, make an average wood density calculation for plants from the same Family. NOTE: this takes a while to run
# ~~~~~~~~~~~~~~~~~~~~~ 

# How many additional trees have WD values based on Family averages from the database? 11
#create a list of the trees assigned wood density values based on Famliy
 GlobalFamilyMatch<-avgWD3[!is.na(avgWD3$meanWD),]
length( GlobalFamilyMatch$meanWD) #11

# isolate trees that do not have wood densities #3 trees
WDneeded5<-avgWD3[!complete.cases(avgWD3),]
length(WDneeded5$code) 
#_____________________________________________________________________________________________
#END:MATCH BASED ON FAMILY 
#_____________________________________________________________________________________________

#_____________________________________________________________________________________________
#START:Merge all datasets to get final Wood density information 
#_____________________________________________________________________________________________
#identify datasets of interest
BCI_BinomailMatch
GlobalBinomialMatch
GlobalGenusMatch
GlobalFamilyMatch
WDneeded5
noinfo

#make the names of the datasets the same
names(BCI_BinomailMatch)<-c("code","wood_density")
names(GlobalBinomialMatch)<-c("code","wood_density")
names(GlobalGenusMatch)<-c("code","wood_density")
names(GlobalFamilyMatch)<-c("code","wood_density")
names(WDneeded5)<-c("code","wood_density")
names(noinfo)<-c("code","wood_density")

#Merge all datasets to get final WD info
AS_data<-rbind(BCI_BinomailMatch,GlobalBinomialMatch,GlobalGenusMatch,GlobalFamilyMatch,WDneeded5,noinfo)
length(AS_data$code) # 119 trees #the number of trees that I initilly started with is conserved

#assign the average wood density for all trees to the 5 trees that do not have a WD assigned
#convert wood density values to a numeric type
AS_data$wood_density<-as.numeric(AS_data$wood_density)

#take the mean wood density for trees in the dataset
meanWD<-mean(AS_data$wood_density[1:114])

#assign the trees that do not have wood density the average wood density value
AS_data$wood_density[115:119]<-c(meanWD,meanWD,meanWD,meanWD,meanWD,meanWD,meanWD,meanWD,meanWD)
AS_data$wood_density[115:119]<-c(0.4,0.4,0.4,0.4,0.4)

AS_data$wood_density

#round the wood density values to two decimal places
AS_data$wood_density<-round(AS_data$wood_density,2)
#_____________________________________________________________________________________________
#END:Merge all datasets to get final Wood density information 
#_____________________________________________________________________________________________
#_____________________________________________________________________________________________
#START: Assign each plant to a pft
#_____________________________________________________________________________________________
# First identify all of the fixers on the 5 year old plots at Agua Salud
#Reduce dataset to the variables of interest and edit column names
names(W)
W<-data.frame(W$SITE,W$SPECid,W$Fixer, W$Number_trees)
names(W)<-c("plot","code","fixer","Number_trees")

#Combine Agua Salud species list for 5 year old plots with the wood density assignments
z<-merge(W,AS_data,all.x=TRUE)

#Check that no trees were lost from W ( looks good)
length(W$code)
length(AS_data$code)
length(z$code)

#Add a column to assign pft values to trees in the 5 yr old plots 
z$pft<-rep(0, length(z$code))
	                       
#assign a pft
PFT<- NULL

for(i in 1:length(z$code)){
	if(z$wood_density[i]< 0.5){
		z$pft[i] <-2 }		
	if(z$wood_density[i]>= 0.5 & z$wood_density[i]< 0.7){
		z$pft[i] <- 3}
	if(z$wood_density[i]>=0.7){
		z$pft[i] <- 4}
	if(z$fixer[i] == "1"){
	z$pft[i] = 30}
	
		data<-data.frame(z$code[i],z$wood_density[i],z$pft[i])
	PFT<-rbind(PFT,data)
	                       }

PFT

#Check to make sure that all trees have a PFT assignment
# looks good
names(PFT)<-c("code","WD","PFT")
PFT$PFT
	
#add the PFT assignments into the z data frame                       
z$pft<-PFT$PFT

#_____________________________________________________________________________________________
#END: Assign each plant to a pft
#_____________________________________________________________________________________________
#_____________________________________________________________________________________________
#START: Calculate the density of plants in each pft in the 5 year old Agua Salud plots
#_____________________________________________________________________________________________

# Calculate the total number of trees and the total number fo trees in each PFT
Total_trees<-sum(z$Number_trees)	
#11045 trees total
total_pft2<-sum(z$Number_trees[z$pft==2])
total_pft3<-sum(z$Number_trees[z$pft==3])
total_pft4<-sum(z$Number_trees[z$pft==4])
total_pft30<-sum(z$Number_trees[z$pft==30])

# total number of plants in each pft
total_pft2 #2086
total_pft3  #6986 
total_pft4  #1845
total_pft30 # 128

#average wood density for fixers 
fixers_avgWD<-weighted.mean(z$wood_density[z$pft==30],z$Number_trees[z$pft==30])
fixers_avgWD # 0.597 new value 0.60
#_____________________________________________________________________________________________
#END: Calculate the density of plants in each pft in the 5 year old Agua Salud plots
#_____________________________________________________________________________________________

#_____________________________________________________________________________________________
#START: Calculate the settings for ED2 init_density based on the 5 year old Agua Salud plots. 
#This is the initial density of plants in units of [# of individuals/m2]
#_____________________________________________________________________________________________

##  Batterman et al reported 13806 stems/ha in her 5-year old plots.  Convert this to stems/m2
tot.dens <- 13806/10000.

## # PFT distributions stems/ha:
#NEW
Fixer_tot<- 128/0.8
pft2_tot <- 2086/0.8
pft3_tot <- 6986 /0.8
pft4_tot <-1845/0.8

# proportion of each pft to total:
fix_stems<-Fixer_tot/13806
pft2_stems<-pft2_tot/13806
pft3_stems<-pft3_tot/13806
pft4_stems<-pft4_tot/13806

#The densities of pfts and fixers are:
init.dens <- c(fix_stems * tot.dens,pft2_stems*tot.dens,pft3_stems*tot.dens,pft4_stems*tot.dens)
init.dens
0.016000 0.260750 0.873250 0.230625
#Ed_params.f90 settings for the Individual level fixation, No fixation,  and Ecosystem Level fixation simulations:     
#init_density(2)     = 0.260750 
#init_density(3)     = 0.873250 
#init_density(4)     = 0.230625
#init_density(30)    = 0.016 
##########
#_____________________________________________________________________________________________
#END: Calculate the settings for ED2 init_density based on the 5 year old Agua Salud plots. 
#This is the initial density of plants in units of [# of individuals/m2]
#_____________________________________________________________________________________________


