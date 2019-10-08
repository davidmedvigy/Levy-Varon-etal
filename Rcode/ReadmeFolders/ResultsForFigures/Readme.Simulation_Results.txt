Simulation_Results.R creates a data frame called Results.csv. Results.csv contains results from all of the model simulations in Levy et al. (201?). Results.csv includes simulations in the main text and supplemental section and is used to create all of the figures. 

To use this file, the yearly result files from all simulations (Individual fixation, No Fixation, Ecosystem Fixation, Fixer, Null Fixer, No disturbance, Fertilization) need to be moved to one folder. The location of this folder is then identified in the first line of Simulation_Results.R  by setting the working directory. 

This file (Simulation_Results.R) contains a loop that extracts results needed to create all figures.

Note that you will have to change 
1) the working directory in the first line of the code to identify where the  yearly simulation result files i are located. 
2) the file pathway to designate where the Results.csv file will be written to.  This occurs at the end of the code