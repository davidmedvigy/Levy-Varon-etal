Figure2.R contains the R code used to create figure 2. Figure 2 shows results from the Individual level fixation, No Fixation, and Ecosystem level simulations.  

To run this code, you will need the Results.csv file described in the "Extract results needed to make figures" folder. You will also need the observations  of carbon accumulation (AS_PlantC.csv) and a vector of values created from the observed N2 fixation ( ControlFixation.csv). To create ControlFixation.csv, yearly values of N2 fixation were calculated by linear interpolating between N2 fixation observations. 

Note that you will have to change 
1) the working directory in the first line of the code to identify where the Results.csv file is located. 
2) the file pathway to designate where the figure image will be written to.  This occurs on line that starts with "postscript" in the section before before plot a is created.
