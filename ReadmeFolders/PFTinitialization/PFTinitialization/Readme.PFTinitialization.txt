The initial PFT composition for the Individual level fixation, No fixation,  and Ecosystem Level fixation simulations was determined by analyzing Agua Salud Project (ASP) demography data from the 5-year-old plot censuses.  We assigned a wood density and plant functional type for every tree in the 5 year old ASP plots using two databases of wood density observations. If wood density information was not available for a species, we assigned an average wood density based on the plant’s Genus and then Family.  For PFT composition, early successional trees had a wood density of <0.5, mid successional trees had a wood density value of 0.5-0.7 and late successional trees had a wood density value >0.7 g cm-3. 

The wood density and PFT assignments as well as the initial PFT composition calculation are in WD_AguaSalud.R

This document uses the files :
1)5yr_UniqueTrees.csv : A list of the unique trees in the 5 year plots that will be assigned a wood density value
2)WDdatabase.csv         : A copy of the data from the Global Wood Density Database
Zanne, A.E., Lopez-Gonzalez, G.*, Coomes, D.A., Ilic, J., Jansen, S., Lewis, S.L., Miller, R.B., Swenson, N.G., Wiemann, M.C., and Chave, J. 2009. Global wood density database. Dryad. Identifier: http://hdl.handle.net/10255/dryad.235. 
3)BCIWD.csv 		   : Wood density observations from Barro Colorado Island
Wright, S. J. et al. Functional traits and the growth-mortality trade-off in tropical trees. Ecology 91, 3664–3674 (2010). (Supplemental dataset)
4)AS_age5_init.csv       : A list of all species in each of the four replicated plots (5 years old) and the number of trees for each species in each plot

Note 1: a copy of the wood density and PFT assignments for each tree in each Agua Salud plot is saved in AguaSalud_5yrplots_Wood_Density.csv

Note 2: For all other simulations, we kept constant the total tree density (1.381 individuals m-2). In the Fixer simulation, fixers were initiated at 0.016 individuals m-2 and the remaining 1.365 individuals m-2 were designated as null-fixers that were identical to fixers except they did not have the ability to fix N2.  
