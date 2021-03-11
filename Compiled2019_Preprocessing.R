##This initial code for Filtering, Sequence variants, and Merging/ remove chimeras, taxonomic assignments is modified from Benjamin Callahan, "A DADA2 workflow for Big Data: Paired-end" https://benjjneb.github.io/dada2/bigdata_paired.html to work with my data. 
#Load the necessary libraries for preprocessing and analysis:
library(ggplot2)
library(phyloseq); packageVersion("phyloseq")
library(ShortRead)
library(dada2)
library(ape); packageVersion('ape') #library for creating  tree
library(dplyr)
library(vegan)
library(ampvis2)
library(DESeq2)
library(cowplot)
library(grid)
library(Rmisc)
theme_set(theme_bw())

#Filtering raw files
pathF <- "/FilePath/raw_data_F/" #set path for forward reads
pathR <- "/FilePath/raw_data_R/" #set path for reverse reads
filtpathF <- file.path(pathF, "filtered") # Filtered forward files go into the pathF/filtered/ subdirectory
filtpathR <- file.path(pathR, "filtered") # ...
fastqFs <- sort(list.files(pathF, pattern="fastq"))
fastqRs <- sort(list.files(pathR, pattern="fastq"))
if(length(fastqFs) != length(fastqRs)) stop("Forward and reverse files do not match.")
#filterAndTrim(fwd=file.path(pathF, fastqFs), filt=file.path(filtpathF, fastqFs),
             # rev=file.path(pathR, fastqRs), filt.rev=file.path(filtpathR, fastqRs))
filterAndTrim(fwd=file.path(pathF, fastqFs), filt=file.path(filtpathF, fastqFs), rev=file.path(pathR, fastqRs), filt.rev=file.path(filtpathR, fastqRs), trimLeft = c(40,20), truncLen=c(239,250), maxEE=2, truncQ=2, maxN=0, compress=TRUE, verbose=TRUE)

#Analyze quality scores:
#These show your quality scores. Score ideally is above 30 (and is in my case for most samples). 20 is the minimum. 
plotQualityProfile(fnFs[[50]])
plotQualityProfile(fnFs[[100]])
plotQualityProfile(fnFs[[150]])
plotQualityProfile(fnFs[[200]])
plotQualityProfile(fnRs[[50]])
plotQualityProfile(fnRs[[100]])
plotQualityProfile(fnRs[[150]])
plotQualityProfile(fnRs[[200]])

#Infer sequence variants
library(dada2); packageVersion("dada2")
# File parsing
filtpathF <- "/Users/lleonard/Documents/R/Compiled2019/Raw_F/filtered" 
filtpathR <- "/Users/lleonard/Documents/R/Compiled2019/Raw_R/filtered" 
filtFs <- list.files(filtpathF, pattern="fastq", full.names = TRUE)
filtRs <- list.files(filtpathR, pattern="fastq", full.names = TRUE)
sample.names <- sapply(strsplit(basename(filtFs), "_"), `[`, 1) # Assumes filename = samplename_XXX.fastq.gz
sample.namesR <- sapply(strsplit(basename(filtRs), "_"), `[`, 1) # Assumes filename = samplename_XXX.fastq.gz
if(!identical(sample.names, sample.namesR)) stop("Forward and reverse files do not match.")
names(filtFs) <- sample.names
names(filtRs) <- sample.names
set.seed(100)
# Learn forward error rates
errF <- learnErrors(filtFs, multithread=TRUE)
# Learn reverse error rates
errR <- learnErrors(filtRs, multithread=TRUE)
# Sample inference and merger of paired-end reads
mergers <- vector("list", length(sample.names))
names(mergers) <- sample.names

#Sample inference and merger of paired-end reads
#Here we will merge as 16S first
for(sam in sample.names) {
  cat("Processing:", sam, "\n")
  derepF <- derepFastq(filtFs[[sam]])
  ddF <- dada(derepF, err=errF, multithread=TRUE)
  derepR <- derepFastq(filtRs[[sam]])
  ddR <- dada(derepR, err=errR, multithread=TRUE)
  merger <- mergePairs(ddF, derepF, ddR, derepR,maxMismatch = 0, minOverlap = 5, justConcatenate = FALSE, verbose=TRUE, returnRejects = FALSE)
  mergers[[sam]] <- merger
  }

#Sample inference and merger of paired-end reads
#Here we will merge as 18S 
mergers18 <- vector("list", length(sample.names))
names(mergers18) <- sample.names
for(sam in sample.names) {
  cat("Processing:", sam, "\n")
  derepF <- derepFastq(filtFs[[sam]])
  ddF <- dada(derepF, err=errF, multithread=TRUE)
  derepR <- derepFastq(filtRs[[sam]])
  ddR <- dada(derepR, err=errR, multithread=TRUE)
  EukMerger <- mergePairs(ddF, derepF, ddR, derepR,maxMismatch = 0, minOverlap = 5, justConcatenate = TRUE, verbose=TRUE, returnRejects = FALSE)
  mergers18[[sam]] <- EukMerger

#16S sequence table with chimeras removed
seqtab <- makeSequenceTable(mergers)
#18S sequence table with chimeras removed
seqtab18 <- makeSequenceTable(mergers18)

#Save files
#saveRDS(seqtab, "/FilePath/seqtab.rds") 
#saveRDS(seqtab18, "/FilePath/seqtab18.rds") 

#Remove chimeras, assign taxonomy
library(dada2); packageVersion("dada2")
# Merge multiple runs (if necessary)
# Remove chimeras for 16S
seqtab <- removeBimeraDenovo(seqtab, method="consensus", multithread=TRUE)
#Remove chimeras for 18S
seqtab18 <- removeBimeraDenovo(seqtab18, method="consensus", multithread=TRUE)
# Assign taxonomy for 16S
tax <- assignTaxonomy(seqtab, "/Users/lleonard/Documents/R/Compiled2019/silva_nr_v128_train_set.fa", multithread=TRUE)
#Assign taxonomy for 18S
tax18 <- assignTaxonomy(seqtab18, "/Users/lleonard/Documents/R/Compiled2019/silva_nr_v128_train_set.fa", multithread=TRUE)
# Write to disk
#saveRDS(seqtab, "/FilePath/seqtab_final.rds") 
#saveRDS(tax, "FilePath/tax_final.rds")


##write taxonomy assignments to file...16S
write.csv(tax, file = "taxa_silva_16S.csv")
##write taxonomy assignments to file...18S
write.csv(tax18, file = "taxa_silva_18S.csv")

##save unique sequences to a file for use in alignment. 18S
a<-colnames(otu_table(seqtab18, taxa_are_rows=FALSE))
uniquesToFasta(seqtab18, "18S_uniques.fasta", ids = a)

##save unique sequences to a file for use in alignment.16S
a<-colnames(otu_table(seqtab, taxa_are_rows=FALSE))
uniquesToFasta(seqtab, "16S_uniques.fasta", ids = a)
##save unique sequences to a file for use in alignment. 18S
a<-colnames(otu_table(seqtab18, taxa_are_rows=FALSE))
uniquesToFasta(seqtab18, "18S_uniques.fasta", ids = a)

#RUN QIIME CODE IN BETWEEN HERE TO GENERATE TRE FILE
#Load into phyloseq for 18S
meta = ("/FilePath/2019MappingFileFinal.txt")
meta = import_qiime_sample_data(meta)
ps_18S <- phyloseq(otu_table(seqtab18, taxa_are_rows=FALSE), 
                   sample_data(meta), 
                   tax_table(tax18))
#read the tree into R and add to the phyloseq object ps.  It's now called ps_t_silva (for phyloseq_tree_silva).**
tree_Q2 = read.tree("/Users/lleonard/Documents/R/Compiled2019/aligned/18S_uniques_aligned_pfiltered.tre")
tree_Q2 = root(tree_Q2, 1, resolve.root = T)
ps_t_18S = merge_phyloseq(ps_18S,tree_Q2) 

unrare_18S = ps_t_18S
#Run, check all numbers are different.
sample_counts18S = sample_sums(unrare_18S)

#Load into phyloseq FOR 16S
meta = ("/FilePath/2019MappingFileFinal.txt")
meta = import_qiime_sample_data(meta)
ps_16S <- phyloseq(otu_table(seqtab, taxa_are_rows=FALSE), 
                   sample_data(meta), 
                   tax_table(tax))

#read the tree into R and add to the phyloseq object ps.  It's now called ps_t_silva (for phyloseq_tree_silva).**
tree_Q2 = read.tree("/Users/lleonard/Documents/R/Compiled2019/aligned/16S_uniques_aligned_pfiltered.tre")
tree_Q2 = root(tree_Q2, 1, resolve.root = T)
ps_t_16S = merge_phyloseq(ps_16S,tree_Q2) 

unrare_16S = ps_t_16S
#Run, check all numbers are different.
sample_counts16 = sample_sums(unrare_16S)

#Rarify to 4299. Be sure to make a CSV and sort from smallest to largest, and just see if there is a clear drop where you can cut off
#Run CB_all to make sure numbers make sense. 
#Filter out 18S, 16S, Mitochondria and Chloroplasts.
CB16S = subset_taxa(unrare_16S, (Kingdom != "Eukaryota")&(Class != "Chloroplast")&(Family != "Mitochondria"))
sums<-sample_sums(CB16S)
write.csv(sums, "16Ssums.csv")

set.seed(58373)
CB_16S = rarefy_even_depth(CB16S, sample.size = 4299, replace = FALSE, trimOTUs = TRUE)
#Make sure here all the numbers are the same,67 samples have been removed, and 1729 OTUs have been removed
sample_sums(CB_16S)
write.csv(otu_table(CB16S), "16Sotu.csv")

#Rarify to 200. Not ideal, but the best option to have some samples to analyze.
CB_18S = subset_taxa(unrare_18S, Kingdom == "Eukaryota")
sums <-sample_sums(CB_18S)
write.csv(sums, "18Ssums.csv")
set.seed(58373)
#219 samples have been removed. And 1476 OTUs were removed. 
CB_18S = rarefy_even_depth(CB_18S, sample.size = 200, replace = FALSE, trimOTUs = TRUE)
sample_sums(CB_18S)
write.csv(otu_table(CB16S), "18Sotu.csv")

#FILTER OTU TABLES
#16S, filter out positive, negative, and excess samples we do not need
CB16_Pos=subset_samples(CB_16S, Needle =="Positive")
CB16_NoPos=subset_samples(CB_16S, (Needle !="Positive")&(Needle != "Blank")&(Needle != "Spruce"))
CB16Paper2= subset_samples(CB16_NoPos, (Needle !="Red")&(Needle != "2Red")&(Needle != "Shade")&(Horizon !="Bot")&(Elevation !="NO"))
#Filter samples by site location
CB16_LM = subset_samples(CB_16S, Location == "Lower Montane")
CB16_LM_Paper2=subset_samples(CB16_LM, (Needle !="Red")&(Needle !="Green")&(Needle !="Lodge")&(Horizon !="Bot"))
CB16_LS = subset_samples(CB_16S, Location == "Lower Subalpine")
CB16_LS_Paper2=subset_samples(CB16_LS, (Needle !="Red")&(Needle !="Shade")&(Needle !="Lodge")&(Needle !="Control")&(Horizon !="Bot"))
CB16_US = subset_samples(CB_16S, Location == "Upper Subalpine")
CB16_US_Paper2=subset_samples(CB16_US_nobotordubs, (Needle !="Red")&(Needle !="Shade")&(Needle !="Lodge")&(Needle !="Control")&(Horizon !="Bot"))
#_______________Lower_______________#
#Specifiy sample types for Lower
CB16LM_Green= subset_samples(CB16_LM, (Needle=="Green")&(Horizon !="Bot"))
CB16LM_Lodge= subset_samples(CB16_LM, (Needle=="Lodge")&(Horizon !="Bot"))
CB16LM_Control= subset_samples(CB16_LM, (Needle !="Red")&(Needle !="Green")&(Needle !="Lodge")&(Horizon !="Bot"))
#Specify year for each sample type at the Lower site
LM16_Control_2017=subset_samples(CB16LM_Control, Year=="a_2017")
LM16_Control_2018=subset_samples(CB16LM_Control, Year=="b_2018")
LM16_Control_2019=subset_samples(CB16LM_Control, Year=="c_2019")
LM16_Green_2017=subset_samples(CB16LM_Green, Year=="a_2017")
LM16_Green_2018=subset_samples(CB16LM_Green, Year=="b_2018")
LM16_Green_2019=subset_samples(CB16LM_Green, Year=="c_2019")
LM16_Lodge_2017=subset_samples(CB16LM_Lodge, Year=="a_2017")
LM16_Lodge_2018=subset_samples(CB16LM_Lodge, Year=="b_2018")
LM16_Lodge_2019=subset_samples(CB16LM_Lodge, Year=="c_2019")
#Filter by each sampling date.
LMOne=subset_samples(CB16_LM_Paper2, Date=="a_One")
LMTwo=subset_samples(CB16_LM_Paper2, Date=="b_Two")
LMThree=subset_samples(CB16_LM_Paper2, Date=="c_Three")
LMFour=subset_samples(CB16_LM_Paper2, Date=="d_Four")
LMFive=subset_samples(CB16_LM_Paper2, Date=="e_Five")
LMSix=subset_samples(CB16_LM_Paper2, Date=="f_Six")
LMSeven=subset_samples(CB16_LM_Paper2, Date=="g_Seven")
#_______________Middle_______________#
#Specify snowmelt plots at the Middle site
CB16_LS_induced = subset_samples(CB16_LS, Snowmelt == "Induced")
CB16_LS_regular = subset_samples(CB16_LS, Snowmelt == "Regular")
#Specifiy sample types for Middle
CB16LS_Green= subset_samples(CB16_LS_regular, (Needle=="Green")&(Horizon !="Bot")&(Needle !="2Red"))
CB16LS_Lodge= subset_samples(CB16_LS_regular, (Needle=="Lodge")&(Horizon !="Bot")&(Needle !="2Red"))
CB16LS_Control= subset_samples(CB16_LS_regular, (Needle !="Red")&(Needle !="Green")&(Needle !="Lodge")&(Horizon !="Bot")&(Needle !="2Red"))
#Specify year for each sample type at the Middle site
LS16_Control_2017=subset_samples(CB16LS_Control, Year=="a_2017")
LS16_Control_2018=subset_samples(CB16LS_Control, Year=="b_2018")
LS16_Control_2019=subset_samples(CB16LS_Control, Year=="c_2019")
LS16_Green_2017=subset_samples(CB16LS_Green, Year=="a_2017")
LS16_Green_2018=subset_samples(CB16LS_Green, Year=="b_2018")
LS16_Green_2019=subset_samples(CB16LS_Green, Year=="c_2019")
LS16_Lodge_2017=subset_samples(CB16LS_Lodge, Year=="a_2017")
LS16_Lodge_2018=subset_samples(CB16LS_Lodge, Year=="b_2018")
LS16_Lodge_2019=subset_samples(CB16LS_Lodge, Year=="c_2019")
#Filter by each sampling date.
LSOne=subset_samples(CB16_LS_Paper2, Date=="a_One")
LSTwo=subset_samples(CB16_LS_Paper2, Date=="b_Two")
LSThree=subset_samples(CB16_LS_Paper2, Date=="c_Three")
LSFour=subset_samples(CB16_LS_Paper2, Date=="d_Four")
LSFive=subset_samples(CB16_LS_Paper2, Date=="e_Five")
LSSix=subset_samples(CB16_LS_Paper2, Date=="f_Six")
LSSeven=subset_samples(CB16_LS_Paper2, Date=="g_Seven")
#_______________Upper_______________#
#Specifiy sample types for Upper
CB16US_Green= subset_samples(CB16_US, (Needle=="Green")&(Horizon !="Bot"))
CB16US_Lodge= subset_samples(CB16_US, (Needle=="Lodge")&(Horizon !="Bot"))
CB16US_Control= subset_samples(CB16_US, (Needle !="Red")&(Needle !="Green")&(Needle !="Lodge")&(Horizon !="Bot"))
#Specify year for each sample type at the Upper site
US16_Control_2017=subset_samples(CB16US_Control, Year=="a_2017")
US16_Control_2018=subset_samples(CB16US_Control, Year=="b_2018")
US16_Control_2019=subset_samples(CB16US_Control, Year=="c_2019")
US16_Green_2017=subset_samples(CB16US_Green, Year=="a_2017")
US16_Green_2018=subset_samples(CB16US_Green, Year=="b_2018")
US16_Green_2019=subset_samples(CB16US_Green, Year=="c_2019")
US16_Lodge_2017=subset_samples(CB16US_Lodge, Year=="a_2017")
US16_Lodge_2018=subset_samples(CB16US_Lodge, Year=="b_2018")
US16_Lodge_2019=subset_samples(CB16US_Lodge, Year=="c_2019")
#Filter by each sampling date.
USOne=subset_samples(CB16_US_Paper2, Date=="a_One")
USTwo=subset_samples(CB16_US_Paper2, Date=="b_Two")
USThree=subset_samples(CB16_US_Paper2, Date=="c_Three")
USFour=subset_samples(CB16_US_Paper2, Date=="d_Four")
USFive=subset_samples(CB16_US_Paper2, Date=="e_Five")
USSix=subset_samples(CB16_US_Paper2, Date=="f_Six")
USSeven=subset_samples(CB16_US_Paper2, Date=="g_Seven")

#18S, filter out positive, negative, and excess samples we do not need
CB18_NoPos=subset_samples(CB_18S, (Needle !="Positive")&(Needle != "Blank")&(Needle != "Spruce")&(Sample !="LM.G2.2.09.20.2019")&(Sample!="LS.2XR3.SM.06.14.2019"))
CB18_LM = subset_samples(CB_18S, Location == "Lower Montane") 
CB18_LS = subset_samples(CB_18S, Location == "Lower Subalpine")
CB18_US = subset_samples(CB_18S, Location == "Upper Subalpine")
CB18Paper2= subset_samples(CB18_NoPos, (Needle !="Red")&(Needle != "2Red")&(Needle != "Shade")&(Horizon !="Bot")&(Elevation !="NO"))
CB18_LS_induced = subset_samples(CB18_LS, Snowmelt == "Induced")
CB18_LS_regular = subset_samples(CB18_LS, Snowmelt == "Regular")
#_______________Lower_______________#
CB18_LM_Paper2=subset_samples(CB18Paper2, (Needle !="Red")&(Needle !="Shade")&(Needle !="Control")&(Horizon !="Bot"))
LMOne_18S=subset_samples(CB18_LM_Paper2, Date=="a_One")
LMTwo_18S=subset_samples(CB18_LM_Paper2, Date=="b_Two")
LMThree_18S=subset_samples(CB18_LM_Paper2, Date=="c_Three")
LMFour_18S=subset_samples(CB18_LM_Paper2, Date=="d_Four")
LMFive_18S=subset_samples(CB18_LM_Paper2, Date=="e_Five")
LMSix_18S=subset_samples(CB18_LM_Paper2, Date=="f_Six")
LMSeven_18S=subset_samples(CB18_LM_Paper2, Date=="g_Seven")
#_______________Middle_______________#  
CB18_LS_Paper2=subset_samples(CB18Paper2, (Needle !="Red")&(Needle !="Shade")&(Needle !="Control")&(Horizon !="Bot"))
LSOne_18S=subset_samples(CB18_LS_Paper2, Date=="a_One")
LSTwo_18S=subset_samples(CB18_LS_Paper2, Date=="b_Two")
LSThree_18S=subset_samples(CB18_LS_Paper2, Date=="c_Three")
LSFour_18S=subset_samples(CB18_LS_Paper2, Date=="d_Four")
LSFive_18S=subset_samples(CB18_LS_Paper2, Date=="e_Five")
LSSix_18S=subset_samples(CB18_LS_Paper2, Date=="f_Six")
LSSeven_18S=subset_samples(CB18_LS_Paper2, Date=="g_Seven")
#_______________Upper_______________#  
CB18_US_Paper2=subset_samples(CB18Paper2, (Needle !="Red")&(Needle !="Shade")&(Needle !="Control")&(Horizon !="Bot"))
USOne_18S=subset_samples(CB18_US_Paper2, Date=="a_One")
USTwo_18S=subset_samples(CB18_US_Paper2, Date=="b_Two")
USThree_18S=subset_samples(CB18_US_Paper2, Date=="c_Three")
USFour_18S=subset_samples(CB18_US_Paper2, Date=="d_Four")
USFive_18S=subset_samples(CB18_US_Paper2, Date=="e_Five")
USSix_18S=subset_samples(CB18_US_Paper2, Date=="f_Six")
USSeven_18S=subset_samples(CB18_US_Paper2, Date=="g_Seven")

