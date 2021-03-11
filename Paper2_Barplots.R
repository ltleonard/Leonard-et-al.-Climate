#This code has been modified by the tutorial written by Paul J. McMurdie and Susan Holmes, "Vignette for phyloseq: Analysis of high-throughput microbiome census data": https://www.bioconductor.org/packages/release/bioc/vignettes/phyloseq/inst/doc/phyloseq-analysis.html
#Let's compare the barplots of the relative abundances for each sample, elevation, and date. 

#_______________________Lower 16S___________________#
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMOne), TRUE)[1:200])
GP1 <- prune_taxa(topsp, LMOne)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Genus %in% names(top5ph))
#Plot it
One=plot_bar(GP1, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMTwo), TRUE)[1:200])
GP2 <- prune_taxa(topsp, LMTwo)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Genus %in% names(top5ph))
#Plot it
Two=plot_bar(GP2, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMThree), TRUE)[1:200])
GP3 <- prune_taxa(topsp, LMThree)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Genus %in% names(top5ph))
#Plot it
Three=plot_bar(GP3, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMFour), TRUE)[1:200])
GP4 <- prune_taxa(topsp, LMFour)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Genus %in% names(top5ph))
#Plot it
Four=plot_bar(GP4, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#############################################################################################
## Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMFive), TRUE)[1:200])
GP5 <- prune_taxa(topsp, LMFive)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Genus %in% names(top5ph))
#Plot it
Five=plot_bar(GP5, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMSix), TRUE)[1:200])
GP6 <- prune_taxa(topsp, LMSix)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMSeven), TRUE)[1:200])
GP7 <- prune_taxa(topsp, LMSeven)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))

plot_grid(One, Two, Three, Four, Five, Six, Seven)
#############################################################################################
#______________________Middle 16S___________________#
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSOne), TRUE)[1:200])
GP1 <- prune_taxa(topsp, LSOne)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Genus %in% names(top5ph))
#Plot it
One=plot_bar(GP1, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSTwo), TRUE)[1:200])
GP2 <- prune_taxa(topsp, LSTwo)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Genus %in% names(top5ph))
#Plot it
Two=plot_bar(GP2, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSThree), TRUE)[1:200])
GP3 <- prune_taxa(topsp, LSThree)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Genus %in% names(top5ph))
#Plot it
Three=plot_bar(GP3, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSFour), TRUE)[1:200])
GP4 <- prune_taxa(topsp, LSFour)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Genus %in% names(top5ph))
#Plot it
Four=plot_bar(GP4, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
theme(legend.position="none")
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSFive), TRUE)[1:200])
GP5 <- prune_taxa(topsp, LSFive)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Genus %in% names(top5ph))
#Plot it
Five=plot_bar(GP5, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
theme(legend.position="none")
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSSix), TRUE)[1:200])
GP6 <- prune_taxa(topsp, LSSix)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
theme(legend.position="none")
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSSeven), TRUE)[1:200])
GP7 <- prune_taxa(topsp, LSSeven)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
theme(legend.position="none")
plot_grid(One, Two, Three, Four, Five, Six, Seven, align = "h")
#############################################################################################
#_______________________Upper 16S___________________#
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USOne), TRUE)[1:200])
GP1 <- prune_taxa(topsp, USOne)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Genus %in% names(top5ph))
#Plot it
One=plot_bar(GP1, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USTwo), TRUE)[1:200])
GP2 <- prune_taxa(topsp, USTwo)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Genus %in% names(top5ph))
#Plot it
Two=plot_bar(GP2, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USThree), TRUE)[1:200])
GP3 <- prune_taxa(topsp, USThree)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Genus %in% names(top5ph))
#Plot it
Three=plot_bar(GP3, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USFour), TRUE)[1:200])
GP4 <- prune_taxa(topsp, USFour)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Genus %in% names(top5ph))
#Plot it
Four=plot_bar(GP4, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USFive), TRUE)[1:200])
GP5 <- prune_taxa(topsp, USFive)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Genus %in% names(top5ph))
#Plot it
Five=plot_bar(GP5, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USSix), TRUE)[1:200])
GP6 <- prune_taxa(topsp, USSix)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USSeven), TRUE)[1:200])
GP7 <- prune_taxa(topsp, USSeven)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
#_______________________Lower 18S___________________#
topsp <- names(sort(taxa_sums(LMOne_18S), TRUE)[1:200])
GP1 <- prune_taxa(topsp, LMOne_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Phylum %in% names(top5ph))
#Plot it
One=plot_bar(GP1, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMTwo_18S), TRUE)[1:200])
GP2 <- prune_taxa(topsp, LMTwo_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Phylum %in% names(top5ph))
#Plot it
Two=plot_bar(GP2, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMThree_18S), TRUE)[1:200])
GP3 <- prune_taxa(topsp, LMThree_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Phylum %in% names(top5ph))
#Plot it
Three=plot_bar(GP3, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMFour_18S), TRUE)[1:200])
GP4 <- prune_taxa(topsp, LMFour_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Phylum %in% names(top5ph))
#Plot it
Four=plot_bar(GP4, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMFive_18S), TRUE)[1:200])
GP5 <- prune_taxa(topsp, LMFive_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Phylum %in% names(top5ph))
#Plot it
Five=plot_bar(GP5, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMSix_18S), TRUE)[1:200])
GP6 <- prune_taxa(topsp, LMSix_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LMSeven_18S), TRUE)[1:200])
GP7 <- prune_taxa(topsp, LMSeven_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))

plot_grid(One, Two, Three, Four, Five, Six, Seven, align = "h")
#############################################################################################
#_______________________Middle 18S___________________#
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSOne_18S), TRUE)[1:200])
GP1 <- prune_taxa(topsp, LSOne_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Phylum %in% names(top5ph))
#Plot it
One=plot_bar(GP1, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSTwo_18S), TRUE)[1:200])
GP2 <- prune_taxa(topsp, LSTwo_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Phylum %in% names(top5ph))
#Plot it
Two=plot_bar(GP2, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSThree_18S), TRUE)[1:200])
GP3 <- prune_taxa(topsp, LSThree_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Phylum %in% names(top5ph))
#Plot it
Three=plot_bar(GP3, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSFour_18S), TRUE)[1:200])
GP4 <- prune_taxa(topsp, LSFour_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Phylum %in% names(top5ph))
#Plot it
Four=plot_bar(GP4, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSFive_18S), TRUE)[1:200])
GP5 <- prune_taxa(topsp, LSFive_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Phylum %in% names(top5ph))
#Plot it
Five=plot_bar(GP5, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSSix_18S), TRUE)[1:200])
GP6 <- prune_taxa(topsp, LSSix_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(LSSeven_18S), TRUE)[1:200])
GP7 <- prune_taxa(topsp, LSSeven_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")

plot_grid(One, Two, Three, Four, Five, Six, Seven, align = "h")
#############################################################################################
#_______________________Upper 18S___________________#
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USOne_18S), TRUE)[1:200])
GP1 <- prune_taxa(topsp, USOne_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Genus %in% names(top5ph))
#Plot it
One=plot_bar(GP1, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USTwo_18S), TRUE)[1:200])
GP2 <- prune_taxa(topsp, USTwo_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Genus %in% names(top5ph))
#Plot it
Two=plot_bar(GP2, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USThree_18S), TRUE)[1:200])
GP3 <- prune_taxa(topsp, USThree_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Genus %in% names(top5ph))
#Plot it
Three=plot_bar(GP3, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USFour_18S), TRUE)[1:200])
GP4 <- prune_taxa(topsp, USFour_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Genus %in% names(top5ph))
#Plot it
Four=plot_bar(GP4, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USFive_18S), TRUE)[1:200])
GP5 <- prune_taxa(topsp, USFive_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Genus %in% names(top5ph))
#Plot it
Five=plot_bar(GP5, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USSix_18S), TRUE)[1:200])
GP6 <- prune_taxa(topsp, USSix_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#############################################################################################
# Subset the data by the top 200 species
topsp <- names(sort(taxa_sums(USSeven_18S), TRUE)[1:200])
GP7 <- prune_taxa(topsp, USSeven_18S)
# Subset the data to the top 15 from the top 200 OTUs
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
