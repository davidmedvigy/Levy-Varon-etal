Figure1.R contains the R code used to create figure 1. Figure 1 shows results from the Individual level fixation simulation and observations from Agua Salud. 

To run this code, you will need the Results.csv file described in the "Extract results needed to make figures" folder. You will also need the observations  of carbon accumulation (AS_PlantC.csv) and  forest basal area (AS_ForestBA.csv). The observations for figure 1b and 1d are added manually in  the Figure1.R code.

Note that you will have to change 
1) the working directory in the first line of the code to identify where the Results.csv file is located. 
2) the file pathway to designate where the figure image will be written to.  This occurs on line that starts with "postscript" in the section before before plot 1a is created.
