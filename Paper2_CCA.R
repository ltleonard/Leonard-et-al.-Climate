#In order to visualize Canonical Correspondence Analysis (CCA) analyses, ordinations need to be determined for the matrices:
#_______________Lower 16S_______________#
#ADONIS test 
groupsig = transform_sample_counts(CB16_LM  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=FALSE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(CB16_LM)) 
adonis<-adonis(q~Year, data=d,permutations=999)
adonis
#_______________Middle 16S_______________#
write.csv(q, "LM.csv")
#ADONIS test 
groupsig = transform_sample_counts(CB16_LS_regular  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(CB16_LS_regular)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
write.csv(q, "LS.csv")
#########################################################################################################
#Let's do 18S as well
#_______________Middle-ES 16S_______________#
#ADONIS test
groupsig = transform_sample_counts(CB16_LS_induced  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(CB16_LS_induced)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
write.csv(q, "ES.csv")
#_______________Lower 18S_______________#
#ADONIS test 
groupsig = transform_sample_counts(CB18_LM  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=FALSE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(CB18_LM)) 
adonis<-adonis(q~Year, data=d,permutations=999)
adonis
#_______________Middle 18S_______________#
write.csv(q, "LM.csv")
#ADONIS test 
groupsig = transform_sample_counts(CB18_LS_regular  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(CB18_LS_regular)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
write.csv(q, "LS.csv")
#_______________Middle-ES 18S_______________#
#ADONIS test
groupsig = transform_sample_counts(CB18_LS_induced  , function(x) 100* x/ sum(x))
ufrac<-UniFrac(groupsig, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
q=as.matrix(ufrac)
d<-data.frame(sample_data(CB18_LS_induced)) 
adonis<-adonis(q~Date, data=d,permutations=999)
adonis
write.csv(q, "ES.csv")