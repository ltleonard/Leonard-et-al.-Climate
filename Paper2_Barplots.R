# Take a subset of the GP dataset, top 200 species
topsp <- names(sort(taxa_sums(LMOne), TRUE)[1:200])
GP1 <- prune_taxa(topsp, LMOne)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP1, x="Needle", fill="Phylum", facet_grid= ~ Date)
One=plot_bar(GP1, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP2 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMTwo), TRUE)[1:200])
GP2 <- prune_taxa(topsp, LMTwo)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP2, x="Needle", fill="Phylum", facet_grid= ~ Date)
Two=plot_bar(GP2, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP3 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMThree), TRUE)[1:200])
GP3 <- prune_taxa(topsp, LMThree)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP3, x="Needle", fill="Phylum", facet_grid= ~ Date)
Three=plot_bar(GP3, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP4 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMFour), TRUE)[1:200])
GP4 <- prune_taxa(topsp, LMFour)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP4, x="Needle", fill="Phylum", facet_grid= ~ Date)
Four=plot_bar(GP4, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP5 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMFive), TRUE)[1:200])
GP5 <- prune_taxa(topsp, LMFive)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP5, x="Needle", fill="Phylum", facet_grid= ~ Date)
Five=plot_bar(GP5, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP6 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMSix), TRUE)[1:200])
GP6 <- prune_taxa(topsp, LMSix)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP6, x="Needle", fill="Phylum", facet_grid= ~ Date)
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP7 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMSeven), TRUE)[1:200])
GP7 <- prune_taxa(topsp, LMSeven)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP7, x="Needle", fill="Phylum", facet_grid= ~ Date)
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#+theme(legend.position="none")

plot_grid(One, Two, Three, Four, Five, Six, Seven)
#############################################################################################
#############################################################################################
#############################################################################################

# Take a subset of the GP dataset, top 200 species
topsp <- names(sort(taxa_sums(LSOne), TRUE)[1:200])
GP1 <- prune_taxa(topsp, LSOne)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP1, x="Needle", fill="Phylum", facet_grid= ~ Date)
One=plot_bar(GP1, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP2 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSTwo), TRUE)[1:200])
GP2 <- prune_taxa(topsp, LSTwo)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP2, x="Needle", fill="Phylum", facet_grid= ~ Date)
Two=plot_bar(GP2, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP3 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSThree), TRUE)[1:200])
GP3 <- prune_taxa(topsp, LSThree)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP3, x="Needle", fill="Phylum", facet_grid= ~ Date)
Three=plot_bar(GP3, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP4 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSFour), TRUE)[1:200])
GP4 <- prune_taxa(topsp, LSFour)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP4, x="Needle", fill="Phylum", facet_grid= ~ Date)
Four=plot_bar(GP4, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
theme(legend.position="none")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#############################################################################################
# Take a subset of the GP5 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSFive), TRUE)[1:200])
GP5 <- prune_taxa(topsp, LSFive)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP5, x="Needle", fill="Phylum", facet_grid= ~ Date)
Five=plot_bar(GP5, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
theme(legend.position="none")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#############################################################################################
# Take a subset of the GP6 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSSix), TRUE)[1:200])
GP6 <- prune_taxa(topsp, LSSix)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP6, x="Needle", fill="Phylum", facet_grid= ~ Date)
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
theme(legend.position="none")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#############################################################################################
# Take a subset of the GP7 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSSeven), TRUE)[1:200])
GP7 <- prune_taxa(topsp, LSSeven)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP7, x="Needle", fill="Phylum", facet_grid= ~ Date)
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,6000), breaks=c(0,1000,2000,3000,4000,5000,6000))
theme(legend.position="none")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))

plot_grid(Four, Five, Six, Seven, align = "h",nrow = 1)
#plot_grid(One, Two, Three, Four, Five, Six, Seven, align = "h")

#############################################################################################
#############################################################################################
#############################################################################################
topsp <- names(sort(taxa_sums(LMOne), TRUE)[1:200])
GP1 <- prune_taxa(topsp, LMOne)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP1, x="Needle", fill="Phylum", facet_grid= ~ Date)
One=plot_bar(GP1, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP2 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMTwo), TRUE)[1:200])
GP2 <- prune_taxa(topsp, LMTwo)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP2, x="Needle", fill="Phylum", facet_grid= ~ Date)
Two=plot_bar(GP2, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP3 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMThree), TRUE)[1:200])
GP3 <- prune_taxa(topsp, LMThree)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP3, x="Needle", fill="Phylum", facet_grid= ~ Date)
Three=plot_bar(GP3, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP4 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMFour), TRUE)[1:200])
GP4 <- prune_taxa(topsp, LMFour)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP4, x="Needle", fill="Phylum", facet_grid= ~ Date)
Four=plot_bar(GP4, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP5 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMFive), TRUE)[1:200])
GP5 <- prune_taxa(topsp, LMFive)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP5, x="Needle", fill="Phylum", facet_grid= ~ Date)
Five=plot_bar(GP5, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP6 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMSix), TRUE)[1:200])
GP6 <- prune_taxa(topsp, LMSix)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP6, x="Needle", fill="Phylum", facet_grid= ~ Date)
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP7 dataset, top 200 species
topsp <- names(sort(taxa_sums(LMSeven), TRUE)[1:200])
GP7 <- prune_taxa(topsp, LMSeven)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP7, x="Needle", fill="Phylum", facet_grid= ~ Date)
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")

plot_grid(One, Two, Three, Four, Five, Six, Seven, align = "h")
#############################################################################################
#############################################################################################
#############################################################################################

# Take a subset of the GP dataset, top 200 species
topsp <- names(sort(taxa_sums(LSOne), TRUE)[1:200])
GP1 <- prune_taxa(topsp, LSOne)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP1, x="Needle", fill="Phylum", facet_grid= ~ Date)
One=plot_bar(GP1, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP2 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSTwo), TRUE)[1:200])
GP2 <- prune_taxa(topsp, LSTwo)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP2, x="Needle", fill="Phylum", facet_grid= ~ Date)
Two=plot_bar(GP2, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP3 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSThree), TRUE)[1:200])
GP3 <- prune_taxa(topsp, LSThree)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP3, x="Needle", fill="Phylum", facet_grid= ~ Date)
Three=plot_bar(GP3, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP4 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSFour), TRUE)[1:200])
GP4 <- prune_taxa(topsp, LSFour)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP4, x="Needle", fill="Phylum", facet_grid= ~ Date)
Four=plot_bar(GP4, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP5 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSFive), TRUE)[1:200])
GP5 <- prune_taxa(topsp, LSFive)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP5, x="Needle", fill="Phylum", facet_grid= ~ Date)
Five=plot_bar(GP5, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP6 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSSix), TRUE)[1:200])
GP6 <- prune_taxa(topsp, LSSix)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP6, x="Needle", fill="Phylum", facet_grid= ~ Date)
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP7 dataset, top 200 species
topsp <- names(sort(taxa_sums(LSSeven), TRUE)[1:200])
GP7 <- prune_taxa(topsp, LSSeven)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP7, x="Needle", fill="Phylum", facet_grid= ~ Date)
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")
#+scale_y_continuous(limit=c(0,8000), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))
#+theme(legend.position="none")

plot_grid(One, Two, Three, Four, Five, Six, Seven, align = "h")
#############################################################################################
#############################################################################################
#############################################################################################

# Take a subset of the GP dataset, top 200 species
topsp <- names(sort(taxa_sums(USOne), TRUE)[1:200])
GP1 <- prune_taxa(topsp, USOne)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP1), tax_table(GP1)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP1 <- subset_taxa(GP1, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP1, x="Needle", fill="Phylum", facet_grid= ~ Date)
One=plot_bar(GP1, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP2 dataset, top 200 species
topsp <- names(sort(taxa_sums(USTwo), TRUE)[1:200])
GP2 <- prune_taxa(topsp, USTwo)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP2), tax_table(GP2)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP2 <- subset_taxa(GP2, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP2, x="Needle", fill="Phylum", facet_grid= ~ Date)
Two=plot_bar(GP2, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP3 dataset, top 200 species
topsp <- names(sort(taxa_sums(USThree), TRUE)[1:200])
GP3 <- prune_taxa(topsp, USThree)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP3), tax_table(GP3)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP3 <- subset_taxa(GP3, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP3, x="Needle", fill="Phylum", facet_grid= ~ Date)
Three=plot_bar(GP3, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP4 dataset, top 200 species
topsp <- names(sort(taxa_sums(USFour), TRUE)[1:200])
GP4 <- prune_taxa(topsp, USFour)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP4), tax_table(GP4)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP4 <- subset_taxa(GP4, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP4, x="Needle", fill="Phylum", facet_grid= ~ Date)
Four=plot_bar(GP4, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP5 dataset, top 200 species
topsp <- names(sort(taxa_sums(USFive), TRUE)[1:200])
GP5 <- prune_taxa(topsp, USFive)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP5), tax_table(GP5)[, "Genus"], sum), 
               decreasing = TRUE)[1:15]
GP5 <- subset_taxa(GP5, Genus %in% names(top5ph))
#Plot it
#plot_bar(GP5, x="Needle", fill="Phylum", facet_grid= ~ Date)
Five=plot_bar(GP5, x="Needle", fill ="Genus")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP6 dataset, top 200 species
topsp <- names(sort(taxa_sums(USSix), TRUE)[1:200])
GP6 <- prune_taxa(topsp, USSix)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP6), tax_table(GP6)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP6 <- subset_taxa(GP6, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP6, x="Needle", fill="Phylum", facet_grid= ~ Date)
Six=plot_bar(GP6, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#+theme(legend.position="none")
#############################################################################################
# Take a subset of the GP7 dataset, top 200 species
topsp <- names(sort(taxa_sums(USSeven), TRUE)[1:200])
GP7 <- prune_taxa(topsp, USSeven)
# Subset further to top 5 phyla, among the top 200 OTUs.
top5ph <- sort(tapply(taxa_sums(GP7), tax_table(GP7)[, "Phylum"], sum), 
               decreasing = TRUE)[1:15]
GP7 <- subset_taxa(GP7, Phylum %in% names(top5ph))
#Plot it
#plot_bar(GP7, x="Needle", fill="Phylum", facet_grid= ~ Date)
Seven=plot_bar(GP7, x="Needle", fill ="Phylum")+geom_bar(stat="identity")+scale_y_continuous(limit=c(0,4000), breaks=c(0,1000,2000,3000,4000))
#+theme(legend.position="none")
