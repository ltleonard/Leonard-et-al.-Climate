#This script converts phyloseq objects to ampvis2 data, readable by  commands we will be making to create the heatmaps for Figure 5. 
#The following heatmap script is modified from the Mads Albersten and Kasper Skytte Anderson tutorial: https://madsalbertsen.github.io/ampvis2/reference/amp_heatmap.html#author

#_________________________________Lower Montane 16S______________________________________________________________
#Convert Phyloseq objects to Ampvis for heatmaps
#Combine OTU abundance table and taxonomy table from the 16S phyloseq Lower object:
LM_Paper <- CB16_LM_Paper2
t_otu <- t(data.frame(otu_table(LM_Paper)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LM_Paper)@.Data,
                                check.names = FALSE
)
#We need to see the dimensions of the OTU table so the following code knows which column to add from without over-writing the table
dim(otutable_4ampvis2)
#Now that we know the number of columns, add +1 to that number, in this case it is 68+1=69
otutable_4ampvis2[69] <- "NA"
colnames(otutable_4ampvis2)[69] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LM_Paper), 
                                check.names = FALSE
)
#Load the data with amp_load:
LM_ampvis<- amp_load(otutable_4ampvis2, metadata_4ampvis2)
###########################################################
#Let's now plot the heatmap of the LS_ampvis object. We chose to focus on the Family level for taxa aaggregation. 
mappedLM <- amp_heatmap(data = LM_ampvis,
                        tax_aggregate = "Family", 
                        tax_add = "Phylum", 
                        group_by = "Date",
                        tax_show = 15, 
                        tax_empty = "remove", 
                        plot_values = TRUE, 
                        plot_values_size = 7,
                        plot_legendbreaks = c(0.1,1.0,10.0,50.0), 
                        max_abundance = 100, 
                        min_abundance = .1) +
  theme(axis.text.x = element_text(size = 20, color = "black", hjust = 0.5, angle = 0)) + 
  theme(axis.text.y = element_text(size = 20, color = "black", angle = 0))
#theme(legend.text = element_text(size = 13, colour = 'black')) + 
#theme(legend.title = element_text(size = 15, color = 'black')) + 
theme(strip.text.x = element_text(size = 20, color = 'Black')) + 
  theme(strip.background.x = element_rect(fill = "White")) + 
  #theme(legend.key.size = unit(1.2, "cm"))+
  #scale_x_discrete("Dates", labels = c("Control", "Lodgepole", "Spruce Combined")) 
  mapped$labels$x <- "Date of Sampling" #change x axis label
mapped$labels$fill <- "% Relative\nAbundance" #change legend label
print(mappedLM)

#_________________________________Lower Subalpine 16S______________________________________________________________
#Convert Phyloseq objects to Ampvis for heatmaps
#Combine OTU abundance table and taxonomy table from the 16S phyloseq Middle object:
LS_Paper <- CB16_LS_Paper2
t_otu <- t(data.frame(otu_table(LS_Paper)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LS_Paper)@.Data,
                                check.names = FALSE
)
#We need to see the dimensions of the OTU table so the following code knows which column to add from without over-writing the table
dim(otutable_4ampvis2)
#Now that we know the number of columns, add +1 to that number, in this case it is 65+1=66
otutable_4ampvis2[66] <- "NA"
colnames(otutable_4ampvis2)[66] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LS_Paper), 
                                check.names = FALSE
)
#Load the data with amp_load:
LS_ampvis<- amp_load(otutable_4ampvis2, metadata_4ampvis2)
###########################################################
#Let's now plot the heatmap of the LS_ampvis object. We chose to focus on the Family level for taxa aaggregation. 
mappedLS <- amp_heatmap(data = LS_ampvis,
                        tax_aggregate = "Family", 
                        tax_add = "Phylum", 
                        group_by = "Date",
                        tax_show = 15, 
                        tax_empty = "remove", 
                        plot_values = TRUE, 
                        plot_values_size = 7,
                        plot_legendbreaks = c(0.1,1.0,10.0,50.0), 
                        max_abundance = 100, 
                        min_abundance = .1) +
  theme(axis.text.x = element_text(size = 20, color = "black", hjust = 0.5, angle = 0)) + 
  theme(axis.text.y = element_text(size = 20, color = "black", angle = 0))
#theme(legend.text = element_text(size = 13, colour = 'black')) + 
#theme(legend.title = element_text(size = 15, color = 'black')) + 
theme(strip.text.x = element_text(size = 20, color = 'Black')) + 
  theme(strip.background.x = element_rect(fill = "White")) + 
  #theme(legend.key.size = unit(1.2, "cm"))+
  #scale_x_discrete("Dates", labels = c("Control", "Lodgepole", "Spruce Combined")) 
  mapped$labels$x <- "Date of Sampling" #change x axis label
mapped$labels$fill <- "% Relative\nAbundance" #change legend label
print(mappedLS)

#_________________________________Upper Subalpine 16S______________________________________________________________
#Convert Phyloseq objects to Ampvis for heatmaps
#Combine OTU abundance table and taxonomy table from the 16S phyloseq Upper object:
US_Paper <- CB16_US_Paper2
t_otu <- t(data.frame(otu_table(US_Paper)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(US_Paper)@.Data,
                                check.names = FALSE
)
#We need to see the dimensions of the OTU table so the following code knows which column to add from without over-writing the table
dim(otutable_4ampvis2)
#Now that we know the number of columns, add +1 to that number, in this case it is 54+1=55
otutable_4ampvis2[55] <- "NA"
colnames(otutable_4ampvis2)[55] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_Paper), 
                                check.names = FALSE
)
#Load the data with amp_load:
US_ampvis<- amp_load(otutable_4ampvis2, metadata_4ampvis2)
###########################################################
#Let's now plot the heatmap of the US_ampvis object. We chose to focus on the Family level for taxa aaggregation. 
mappedUS <- amp_heatmap(data = US_ampvis,
                      tax_aggregate = "Family", 
                      tax_add = "Phylum", 
                      group_by = "Date",
                      tax_show = 15, 
                      tax_empty = "remove", 
                      plot_values = TRUE, 
                      plot_values_size = 7,
                      plot_legendbreaks = c(0.1,1.0,10.0,50.0), 
                      max_abundance = 100, 
                      min_abundance = .1) +
  theme(axis.text.x = element_text(size = 20, color = "black", hjust = 0.5, angle = 0)) + 
  theme(axis.text.y = element_text(size = 20, color = "black", angle = 0))
#theme(legend.text = element_text(size = 13, colour = 'black')) + 
#theme(legend.title = element_text(size = 15, color = 'black')) + 
theme(strip.text.x = element_text(size = 20, color = 'Black')) + 
  theme(strip.background.x = element_rect(fill = "White")) + 
  #theme(legend.key.size = unit(1.2, "cm"))+
  #scale_x_discrete("Dates", labels = c("Control", "Lodgepole", "Spruce Combined")) 
  mapped$labels$x <- "Date of Sampling" #change x axis label
mapped$labels$fill <- "% Relative\nAbundance" #change legend label
print(mappedUS)

##############################################################################
##########################Next up: 18S########################################
##############################################################################
#_________________________________Lower Montane 18S______________________________________________________________
#Convert Phyloseq objects to Ampvis for heatmaps
#Combine OTU abundance table and taxonomy table from the 18S phyloseq Lower object:
LM_Paper18S <- CB18_LM_Paper2
t_otu <- t(data.frame(otu_table(LM_Paper18S)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LM_Paper18S)@.Data,
                                check.names = FALSE
)
#We need to see the dimensions of the OTU table so the following code knows which column to add from without over-writing the table
dim(otutable_4ampvis2)
#Now that we know the number of columns, add +1 to that number, in this case it is 41+1=42
otutable_4ampvis2[42] <- "NA"
colnames(otutable_4ampvis2)[42] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LM_Paper18S), 
                                check.names = FALSE
)
#Load the data with amp_load:
LM_ampvis18S<- amp_load(otutable_4ampvis2, metadata_4ampvis2)
###########################################################
#Let's now plot the heatmap of the LS_ampvis object. We chose to focus on the Family level for taxa aaggregation. 
mappedLM18 <- amp_heatmap(data = LM_ampvis18S,
                        tax_aggregate = "Family", 
                        tax_add = "Phylum", 
                        group_by = "Date",
                        tax_show = 15, 
                        tax_empty = "remove", 
                        plot_values = TRUE, 
                        plot_values_size = 7,
                        plot_legendbreaks = c(0.1,1.0,10.0,50.0), 
                        max_abundance = 100, 
                        min_abundance = .1) +
  theme(axis.text.x = element_text(size = 20, color = "black", hjust = 0.5, angle = 0)) + 
  theme(axis.text.y = element_text(size = 20, color = "black", angle = 0))
#theme(legend.text = element_text(size = 13, colour = 'black')) + 
#theme(legend.title = element_text(size = 15, color = 'black')) + 
theme(strip.text.x = element_text(size = 20, color = 'Black')) + 
  theme(strip.background.x = element_rect(fill = "White")) + 
  #theme(legend.key.size = unit(1.2, "cm"))+
  #scale_x_discrete("Dates", labels = c("Control", "Lodgepole", "Spruce Combined")) 
  mapped$labels$x <- "Date of Sampling" #change x axis label
mapped$labels$fill <- "% Relative\nAbundance" #change legend label
print(mappedLM18)

#_________________________________Lower Subalpine 18S______________________________________________________________
#Convert Phyloseq objects to Ampvis for heatmaps
#Combine OTU abundance table and taxonomy table from the 18S phyloseq Middle object:
LS_Paper18S <- CB18_LS_Paper2
t_otu <- t(data.frame(otu_table(LS_Paper18S)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LS_Paper18S)@.Data,
                                check.names = FALSE
)
#We need to see the dimensions of the OTU table so the following code knows which column to add from without over-writing the table
dim(otutable_4ampvis2)
#Now that we know the number of columns, add +1 to that number, in this case it is 52+1=53
otutable_4ampvis2[53] <- "NA"
colnames(otutable_4ampvis2)[53] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LS_Paper18S), 
                                check.names = FALSE
)
#Load the data with amp_load:
LS_ampvis18S<- amp_load(otutable_4ampvis2, metadata_4ampvis2)
###########################################################
#Let's now plot the heatmap of the LS_ampvis object. We chose to focus on the Family level for taxa aaggregation. 
mappedLS18 <- amp_heatmap(data = LS_ampvis18S,
                        tax_aggregate = "Family", 
                        tax_add = "Phylum", 
                        group_by = "Date",
                        tax_show = 15, 
                        tax_empty = "remove", 
                        plot_values = TRUE, 
                        plot_values_size = 7,
                        plot_legendbreaks = c(0.1,1.0,10.0,50.0), 
                        max_abundance = 100, 
                        min_abundance = .1) +
  theme(axis.text.x = element_text(size = 20, color = "black", hjust = 0.5, angle = 0)) + 
  theme(axis.text.y = element_text(size = 20, color = "black", angle = 0))
#theme(legend.text = element_text(size = 13, colour = 'black')) + 
#theme(legend.title = element_text(size = 15, color = 'black')) + 
theme(strip.text.x = element_text(size = 20, color = 'Black')) + 
  theme(strip.background.x = element_rect(fill = "White")) + 
  #theme(legend.key.size = unit(1.2, "cm"))+
  #scale_x_discrete("Dates", labels = c("Control", "Lodgepole", "Spruce Combined")) 
  mapped$labels$x <- "Date of Sampling" #change x axis label
mapped$labels$fill <- "% Relative\nAbundance" #change legend label
print(mappedLS18)

#_________________________________Upper Subalpine 18S______________________________________________________________
#Convert Phyloseq objects to Ampvis for heatmaps
#Combine OTU abundance table and taxonomy table from the 18S phyloseq Upper object:
US_Paper18S <- CB18_US_Paper2
t_otu <- t(data.frame(otu_table(US_Paper18S)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(US_Paper18S)@.Data,
                                check.names = FALSE
)
#We need to see the dimensions of the OTU table so the following code knows which column to add from without over-writing the table
dim(otutable_4ampvis2)
#Now that we know the number of columns, add +1 to that number, in this case it is 35+1=36
otutable_4ampvis2[36] <- "NA"
colnames(otutable_4ampvis2)[36] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_Paper18S), 
                                check.names = FALSE
)
#Load the data with amp_load:
US_ampvis18S<- amp_load(otutable_4ampvis2, metadata_4ampvis2)
###########################################################
#Let's now plot the heatmap of the US_ampvis object. We chose to focus on the Family level for taxa aaggregation. 
mappedUS18 <- amp_heatmap(data = US_ampvis18S,
                        tax_aggregate = "Family", 
                        tax_add = "Phylum", 
                        group_by = "Date",
                        tax_show = 15, 
                        tax_empty = "remove", 
                        plot_values = TRUE, 
                        plot_values_size = 7,
                        plot_legendbreaks = c(0.1,1.0,10.0,50.0), 
                        max_abundance = 100, 
                        min_abundance = .1) +
  theme(axis.text.x = element_text(size = 20, color = "black", hjust = 0.5, angle = 0)) + 
  theme(axis.text.y = element_text(size = 20, color = "black", angle = 0))
#theme(legend.text = element_text(size = 13, colour = 'black')) + 
#theme(legend.title = element_text(size = 15, color = 'black')) + 
theme(strip.text.x = element_text(size = 20, color = 'Black')) + 
  theme(strip.background.x = element_rect(fill = "White")) + 
  #theme(legend.key.size = unit(1.2, "cm"))+
  #scale_x_discrete("Dates", labels = c("Control", "Lodgepole", "Spruce Combined")) 
  mapped$labels$x <- "Date of Sampling" #change x axis label
mapped$labels$fill <- "% Relative\nAbundance" #change legend label
print(mappedUS18)
######################################################################################################################
########################## Next up: Analysis at each elevation and sample type for 16S################################
######################################################################################################################
#Continue the above workflow for each sample type and elevation using the below ampvis conversions:
#_______________________________________________________________________________________________
#Lower Montane lodgepole for all dates
LM_Lodge16 <- CB16LM_Lodge
t_otu <- t(data.frame(otu_table(LM_Lodge16)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LM_Lodge16)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[28] <- "NA"
colnames(otutable_4ampvis2)[28] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LM_Lodge16), 
                                check.names = FALSE
)
#Load the data with amp_load:
LM_Lodge_ampvis <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Upper Subalpine lodgepole for all dates
US_Lodge16 <- CB16US_Lodge
t_otu <- t(data.frame(otu_table(US_Lodge16)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(US_Lodge16)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[24] <- "NA"
colnames(otutable_4ampvis2)[24] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_Lodge16), 
                                check.names = FALSE
)
#Load the data with amp_load:
US_Lodge_ampvis <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Lower Subalpine lodgepole for all dates
LS_Lodge16 <- CB16LS_Lodge
t_otu <- t(data.frame(otu_table(LS_Lodge16)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LS_Lodge16)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[28] <- "NA"
colnames(otutable_4ampvis2)[28] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LS_Lodge16), 
                                check.names = FALSE
)
#Load the data with amp_load:
LS_Lodge_ampvis <- amp_load(otutable_4ampvis2, metadata_4ampvis2)

#_______________________________________________________________________________________________
#Lower Montane control for all dates
LM_Control16 <- CB16LM_Control
t_otu <- t(data.frame(otu_table(LM_Control16)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LM_Control16)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[41] <- "NA"
colnames(otutable_4ampvis2)[41] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LM_Control16), 
                                check.names = FALSE
)
#Load the data with amp_load:
LM_Control_ampvis <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Lower Subalpine control for all dates
LS_Control16 <- CB16LS_Control 
t_otu <- t(data.frame(otu_table(LS_Control16)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LS_Control16)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[38] <- "NA"
colnames(otutable_4ampvis2)[38] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LS_Control16), 
                                check.names = FALSE
)
#Load the data with amp_load:
LS_Control_ampvis <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Upper Subalpine control for all dates
US_Control16 <- CB16US_Control
t_otu <- t(data.frame(otu_table(US_Control16)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(US_Control16)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[25] <- "NA"
colnames(otutable_4ampvis2)[25] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_Control16), 
                                check.names = FALSE
)
#Load the data with amp_load:
US_Control_ampvis <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Lower Montane Green for all dates
LM_Green16 <- CB16LM_Green 
t_otu <- t(data.frame(otu_table(US_Green16)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(US_Green16)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[29] <- "NA"
colnames(otutable_4ampvis2)[29] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_Green16), 
                                check.names = FALSE
)
#Load the data with amp_load:
LM_Green_ampvis <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Upper Subalpine Green for all dates
US_Green16 <- CB16US_Green 
t_otu <- t(data.frame(otu_table(US_Green16)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(US_Green16)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[24] <- "NA"
colnames(otutable_4ampvis2)[24] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_Green16), 
                                check.names = FALSE
)
#Load the data with amp_load:
US_Green_ampvis <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Lower Subalpine Green for all dates
LS_Green16 <- CB16LS_Green 
t_otu <- t(data.frame(otu_table(LS_Green16)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LS_Green16)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[26] <- "NA"
colnames(otutable_4ampvis2)[26] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_16), 
                                check.names = LS_Green16
)
#Load the data with amp_load:
LS_Green_ampvis <- amp_load(otutable_4ampvis2, metadata_4ampvis2)

######################################################################################################################
########################## Next up: Analysis at each elevation and sample type for 18S ###############################
######################################################################################################################
#Continue the above workflow for each sample type and elevation using the below ampvis conversions:
#_______________________________________________________________________________________________
#Lower Montane lodgepole for all dates
LM_Lodge18 <- CB18LM_Lodge
t_otu <- t(data.frame(otu_table(LM_Lodge18)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LM_Lodge18)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[28] <- "NA"
colnames(otutable_4ampvis2)[28] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LM_Lodge18), 
                                check.names = FALSE
)
#Load the data with amp_load:
LM_Lodge_ampvis18 <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Upper Subalpine lodgepole for all dates
US_Lodge18 <- CB18US_Lodge
t_otu <- t(data.frame(otu_table(US_Lodge18)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(US_Lodge18)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[24] <- "NA"
colnames(otutable_4ampvis2)[24] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_Lodge18), 
                                check.names = FALSE
)
#Load the data with amp_load:
US_Lodge_ampvis18 <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Lower Subalpine lodgepole for all dates
LS_Lodge18 <- CB18LS_Lodge
t_otu <- t(data.frame(otu_table(LS_Lodge18)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LS_Lodge18)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[28] <- "NA"
colnames(otutable_4ampvis2)[28] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LS_Lodge18), 
                                check.names = FALSE
)
#Load the data with amp_load:
LS_Lodge_ampvis18 <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Lower Montane control for all dates
LM_Control18 <- CB18LM_Control
t_otu <- t(data.frame(otu_table(LM_Control18)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LM_Control18)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[41] <- "NA"
colnames(otutable_4ampvis2)[41] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LM_Control18), 
                                check.names = FALSE
)
#Load the data with amp_load:
LM_Control_ampvis18 <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Lower Subalpine control for all dates
LS_Control18 <- CB18LS_Control 
t_otu <- t(data.frame(otu_table(LS_Control18)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LS_Control18)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[38] <- "NA"
colnames(otutable_4ampvis2)[38] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(LS_Control18), 
                                check.names = FALSE
)
#Load the data with amp_load:
LS_Control_ampvis18 <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Upper Subalpine control for all dates
US_Control18 <- CB18US_Control
t_otu <- t(data.frame(otu_table(US_Control18)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(US_Control18)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[25] <- "NA"
colnames(otutable_4ampvis2)[25] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_Control18), 
                                check.names = FALSE
)
#Load the data with amp_load:
US_Control_ampvis18 <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Lower Montane Green for all dates
LM_Green18 <- CB18LM_Green 
t_otu <- t(data.frame(otu_table(US_Green18)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(US_Green18)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[29] <- "NA"
colnames(otutable_4ampvis2)[29] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_Green18), 
                                check.names = FALSE
)
#Load the data with amp_load:
LM_Green_ampvis18 <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Upper Subalpine Green for all dates
US_Green18 <- CB18US_Green 
t_otu <- t(data.frame(otu_table(US_Green18)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(US_Green18)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[24] <- "NA"
colnames(otutable_4ampvis2)[24] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_Green18), 
                                check.names = FALSE
)
#Load the data with amp_load:
US_Green_ampvis18 <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
#_______________________________________________________________________________________________
#Lower Subalpine Green for all dates
LS_Green18 <- CB18LS_Green 
t_otu <- t(data.frame(otu_table(LS_Green18)))
otutable_4ampvis2 <- data.frame(OTU = rownames(t_otu@.Data),
                                t_otu@.Data,
                                phyloseq::tax_table(LS_Green18)@.Data,
                                check.names = FALSE
)
dim(otutable_4ampvis2)
otutable_4ampvis2[26] <- "NA"
colnames(otutable_4ampvis2)[26] <- "Species"
#Extract metadata from the phyloseq object:
metadata_4ampvis2 <- data.frame(phyloseq::sample_data(US_18), 
                                check.names = LS_Green18
)
#Load the data with amp_load:
LS_Green_ampvis18 <- amp_load(otutable_4ampvis2, metadata_4ampvis2)
