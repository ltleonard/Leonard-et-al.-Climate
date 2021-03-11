#This code is written with the libraries and mapping file Compiled2019MappingFile.txt are already loaded from the Compiled2019_Prerocessing.Rmd and are recognized within the R environment. 

#____________________________ Ordination for 16S Elevations___________________________________________#
#Object to ordinate 16S DNA samples across all elevations
ordu_we_16Paper2 = ordinate(CB16Paper2, "PCoA", "unifrac", weighted = TRUE)
#We analyzed Weighted and Unweighted Unifrac distances, so be sure to change this accordingly in the code below:
#Let's create a PCoA plot for DNA samples across elevation
Elevation16S=plot_ordination(CB16Paper2,  ordu_we_16Paper2, color = "Elevation") + 
  geom_point(size = 5) + 
  ggtitle(" P=0.001*, R=0.174") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #Assign colors to the plot
  scale_color_manual(values=c("dodgerblue4","goldenrod","darkorange3","black","gray47","gray37","gray27","gray17"), name="Level of Impact") + 
  theme(axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16), strip.text.x = element_text(size=16))
plot_ordination(CB16Paper2,  ordu_we_16Paper2, type="scree") 
#ADONIS test based on Location: Elevation. 
groupsig = transform_sample_counts(CB16Paper2  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(CB16Paper2)) 
adonis<-adonis(q~Location, data=d,permutations=999)
adonis

#____________________________ Ordination for 18S Elevations___________________________________________#

#Object to ordinate 18S DNA samples across all elevations
ordu_we_18Paper2 = ordinate(CB18Paper2, "PCoA", "unifrac", weighted = TRUE)
#We analyzed Weighted and Unweighted Unifrac distances, so be sure to change this accordingly in the code below:
#Let's create a PCoA plot for DNA samples across elevation
Elevation18S=plot_ordination(CB18Paper2,  ordu_we_18Paper2, color = "Elevation") + 
  geom_point(size = 5) + 
  ggtitle(" P=0.001*, R=0.174") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #Assign colors to the plot
  scale_color_manual(values=c("dodgerblue4","goldenrod","darkorange3","black","gray47","gray37","gray27","gray17"), name="Level of Impact") + 
  theme(axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16), strip.text.x = element_text(size=16))
plot_ordination(CB18Paper2,  ordu_we_18Paper2, type="scree") 
#ADONIS test based on Location: Elevation. 
groupsig = transform_sample_counts(CB18Paper2  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(CB18Paper2)) 
adonis<-adonis(q~Elevation, data=d,permutations=999)
adonis

#____________________________ Ordination for 16S Dates___________________________________________#
#Objects to ordinate 16S DNA for each sample type and Year at the Lower
ordu_we_LS16_LM_Control_2017 = ordinate(LM16_Control_2017, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LM_Control_2018 = ordinate(LM16_Control_2018, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LM_Control_2019 = ordinate(LM16_Control_2019, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LM_Green_2017 = ordinate(LM16_Green_2017, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LM_Green_2018 = ordinate(LM16_Green_2018, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LM_Green_2019 = ordinate(LM16_Green_2019, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LM_Lodge_2017 = ordinate(LM16_Lodge_2017, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LM_Lodge_2018 = ordinate(LM16_Lodge_2018, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LM_Lodge_2019 = ordinate(LM16_Lodge_2019, "PCoA", "unifrac", weighted = TRUE)
#Objects to ordinate 16S DNA for each sample type and Year at the Upper
ordu_we_LS16_US_Control_2017 = ordinate(US16_Control_2017, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_US_Control_2018 = ordinate(US16_Control_2018, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_US_Control_2019 = ordinate(US16_Control_2019, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_US_Green_2017 = ordinate(US16_Green_2017, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_US_Green_2018 = ordinate(US16_Green_2018, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_US_Green_2019 = ordinate(US16_Green_2019, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_US_Lodge_2017 = ordinate(US16_Lodge_2017, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_US_Lodge_2018 = ordinate(US16_Lodge_2018, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_US_Lodge_2019 = ordinate(US16_Lodge_2019, "PCoA", "unifrac", weighted = TRUE)
#Objects to ordinate 16S DNA for each sample type and Year at the Middle
ordu_we_LS16_LS_Control_2017 = ordinate(LS16_Control_2017, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LS_Control_2018 = ordinate(LS16_Control_2018, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LS_Control_2019 = ordinate(LS16_Control_2019, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LS_Green_2017 = ordinate(LS16_Green_2017, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LS_Green_2018 = ordinate(LS16_Green_2018, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LS_Green_2019 = ordinate(LS16_Green_2019, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LS_Lodge_2017 = ordinate(LS16_Lodge_2017, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LS_Lodge_2018 = ordinate(LS16_Lodge_2018, "PCoA", "unifrac", weighted = TRUE)
ordu_we_LS16_LS_Lodge_2019 = ordinate(LS16_Lodge_2019, "PCoA", "unifrac", weighted = TRUE)
#_______________Lower_______________#
#ADONIS test LM_Control2017
groupsig = transform_sample_counts(LM16_Control_2017  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LM16_Control_2017)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LM_Control2018
groupsig = transform_sample_counts(LM16_Control_2018  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LM16_Control_2018)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LM_Control2019
groupsig = transform_sample_counts(LM16_Control_2019  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LM16_Control_2019)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LM_Green2017
groupsig = transform_sample_counts(LM16_Green_2017  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LM16_Green_2017)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LM_Green2018
groupsig = transform_sample_counts(LM16_Green_2018  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LM16_Green_2018)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LM_Green2019
groupsig = transform_sample_counts(LM16_Green_2019  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LM16_Green_2019)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LM_Lodge2017
groupsig = transform_sample_counts(LM16_Lodge_2017  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LM16_Lodge_2017)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LM_Lodge2018
groupsig = transform_sample_counts(LM16_Lodge_2018  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LM16_Lodge_2018)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LM_Lodge2019
groupsig = transform_sample_counts(LM16_Lodge_2019  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LM16_Lodge_2019)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#_______________Upper_______________#
#ADONIS test US_Control2017
groupsig = transform_sample_counts(US16_Control_2017  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(US16_Control_2017)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test US_Control2018
groupsig = transform_sample_counts(US16_Control_2018  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(US16_Control_2018)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test US_Control2019
groupsig = transform_sample_counts(US16_Control_2019  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(US16_Control_2019)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test US_Green2017
groupsig = transform_sample_counts(US16_Green_2017  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(US16_Green_2017)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test US_Green2018
groupsig = transform_sample_counts(US16_Green_2018  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(US16_Green_2018)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test US_Green2019
groupsig = transform_sample_counts(US16_Green_2019  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(US16_Green_2019)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test US_Lodge2017
groupsig = transform_sample_counts(US16_Lodge_2017  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(US16_Lodge_2017)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test US_Lodge2018
groupsig = transform_sample_counts(US16_Lodge_2018  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(US16_Lodge_2018)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test US_Lodge2019
groupsig = transform_sample_counts(US16_Lodge_2019  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(US16_Lodge_2019)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#_______________Middle______________#
#ADONIS test LS_Control2017
groupsig = transform_sample_counts(LS16_Control_2017  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LS16_Control_2017)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LS_Control2018
groupsig = transform_sample_counts(LS16_Control_2018  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LS16_Control_2018)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LS_Control2019
groupsig = transform_sample_counts(LS16_Control_2019  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LS16_Control_2019)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LS_Green2017
groupsig = transform_sample_counts(LS16_Green_2017  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LS16_Green_2017)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LS_Green2018
groupsig = transform_sample_counts(LS16_Green_2018  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LS16_Green_2018)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LS_Green2019
groupsig = transform_sample_counts(LS16_Green_2019  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LS16_Green_2019)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LS_Lodge2017
groupsig = transform_sample_counts(LS16_Lodge_2017  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LS16_Lodge_2017)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LS_Lodge2018
groupsig = transform_sample_counts(LS16_Lodge_2018  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LS16_Lodge_2018)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
#ADONIS test LS_Lodge2019
groupsig = transform_sample_counts(LS16_Lodge_2019  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(LS16_Lodge_2019)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
