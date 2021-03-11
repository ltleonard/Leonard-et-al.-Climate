#Code modified from tutorial written by Michelle Berry at http://deneflab.github.io/MicrobeMiseq/demos/mothur_2_phyloseq.html
#Let's first determine the alpha diversity metrics for each elevation
#_______________Upper 16S_______________#
min_lib <- min(sample_sums(CB16_US))
nsamp = nsamples(CB16_US)
trials = 100
richness <- matrix(nrow = nsamp, ncol = trials)
row.names(richness) <- sample_names(CB16_US)
evenness <- matrix(nrow = nsamp, ncol = trials)
row.names(evenness) <- sample_names(CB16_US)
#Set Seed
set.seed(3)
for (i in 1:100) {
  # Subsample
  r <- rarefy_even_depth(CB16_US, sample.size = min_lib, verbose = FALSE, replace = TRUE)
  sample_sums(r)
  sample_sums(CB16_US)
  # Calculate richness
  rich <- as.numeric(as.matrix(estimate_richness(r, measures = "Observed")))
  richness[ ,i] <- rich
  # Calculate evenness
  even <- as.numeric(as.matrix(estimate_richness(r, measures = "InvSimpson")))
  evenness[ ,i] <- even
}
# Create a new dataframe to hold the means and standard deviations of richness estimates
SampleID <- row.names(richness)
mean <- apply(richness, 1, mean)
sd <- apply(richness, 1, sd)
measure <- rep("Richness", nsamp)
rich_stats <- data.frame(SampleID, mean, sd, measure)
# Create a new dataframe to hold the means and standard deviations of evenness estimates
SampleID <- row.names(evenness)
mean <- apply(evenness, 1, mean)
sd <- apply(evenness, 1, sd)
measure <- rep("Inverse Simpson", nsamp)
even_stats <- data.frame(SampleID, mean, sd, measure)
alpha <- rbind(rich_stats, even_stats)
#Write csv of data to use for line plots
write.csv(alpha, "alpha_US.csv")
#_______________Lower 16S_______________#
min_lib <- min(sample_sums(CB16_LM))
nsamp = nsamples(CB16_LM)
trials = 100
richness <- matrix(nrow = nsamp, ncol = trials)
row.names(richness) <- sample_names(CB16_LM)
evenness <- matrix(nrow = nsamp, ncol = trials)
row.names(evenness) <- sample_names(CB16_LM)
#Set Seed
set.seed(3)
for (i in 1:100) {
  # Subsample
  r <- rarefy_even_depth(CB16_LM, sample.size = min_lib, verbose = FALSE, replace = TRUE)
  sample_sums(r)
  sample_sums(CB16_LM)
  # Calculate richness
  rich <- as.numeric(as.matrix(estimate_richness(r, measures = "Observed")))
  richness[ ,i] <- rich
  # Calculate evenness
  even <- as.numeric(as.matrix(estimate_richness(r, measures = "InvSimpson")))
  evenness[ ,i] <- even
}
# Create a new dataframe to hold the means and standard deviations of richness estimates
SampleID <- row.names(richness)
mean <- apply(richness, 1, mean)
sd <- apply(richness, 1, sd)
measure <- rep("Richness", nsamp)
rich_stats <- data.frame(SampleID, mean, sd, measure)
# Create a new dataframe to hold the means and standard deviations of evenness estimates
SampleID <- row.names(evenness)
mean <- apply(evenness, 1, mean)
sd <- apply(evenness, 1, sd)
measure <- rep("Inverse Simpson", nsamp)
even_stats <- data.frame(SampleID, mean, sd, measure)
alpha <- rbind(rich_stats, even_stats)
#Write csv of data to use for line plots
write.csv(alpha, "alpha_LM.csv")
#_______________Midddle 16S_______________#
min_lib <- min(sample_sums(CB16_LS_regular))
nsamp = nsamples(CB16_LS_regular)
trials = 100
richness <- matrix(nrow = nsamp, ncol = trials)
row.names(richness) <- sample_names(CB16_LS_regular)
evenness <- matrix(nrow = nsamp, ncol = trials)
row.names(evenness) <- sample_names(CB16_LS_regular)
#Set Seed
set.seed(3)
for (i in 1:100) {
  # Subsample
  r <- rarefy_even_depth(CB16_LS_regular, sample.size = min_lib, verbose = FALSE, replace = TRUE)
  sample_sums(r)
  sample_sums(CB16_LS_regular)
  # Calculate richness
  rich <- as.numeric(as.matrix(estimate_richness(r, measures = "Observed")))
  richness[ ,i] <- rich
  # Calculate evenness
  even <- as.numeric(as.matrix(estimate_richness(r, measures = "InvSimpson")))
  evenness[ ,i] <- even
}
# Create a new dataframe to hold the means and standard deviations of richness estimates
SampleID <- row.names(richness)
mean <- apply(richness, 1, mean)
sd <- apply(richness, 1, sd)
measure <- rep("Richness", nsamp)
rich_stats <- data.frame(SampleID, mean, sd, measure)
# Create a new dataframe to hold the means and standard deviations of evenness estimates
SampleID <- row.names(evenness)
mean <- apply(evenness, 1, mean)
sd <- apply(evenness, 1, sd)
measure <- rep("Inverse Simpson", nsamp)
even_stats <- data.frame(SampleID, mean, sd, measure)
alpha <- rbind(rich_stats, even_stats)
#Write csv of data to use for line plots
write.csv(alpha, "alpha_LS.csv")
#_______________Midddle-ES 16S_______________#
min_lib <- min(sample_sums(CB16_LS_induced))
nsamp = nsamples(CB16_LS_induced)
trials = 100
richness <- matrix(nrow = nsamp, ncol = trials)
row.names(richness) <- sample_names(CB16_LS_induced)
evenness <- matrix(nrow = nsamp, ncol = trials)
row.names(evenness) <- sample_names(CB16_LS_induced)
#Set Seed
set.seed(3)
for (i in 1:100) {
  # Subsample
  r <- rarefy_even_depth(CB16_LS_induced, sample.size = min_lib, verbose = FALSE, replace = TRUE)
  sample_sums(r)
  sample_sums(CB16_LS_induced)
  # Calculate richness
  rich <- as.numeric(as.matrix(estimate_richness(r, measures = "Observed")))
  richness[ ,i] <- rich
  # Calculate evenness
  even <- as.numeric(as.matrix(estimate_richness(r, measures = "InvSimpson")))
  evenness[ ,i] <- even
}
# Create a new dataframe to hold the means and standard deviations of richness estimates
SampleID <- row.names(richness)
mean <- apply(richness, 1, mean)
sd <- apply(richness, 1, sd)
measure <- rep("Richness", nsamp)
rich_stats <- data.frame(SampleID, mean, sd, measure)
# Create a new dataframe to hold the means and standard deviations of evenness estimates
SampleID <- row.names(evenness)
mean <- apply(evenness, 1, mean)
sd <- apply(evenness, 1, sd)
measure <- rep("Inverse Simpson", nsamp)
even_stats <- data.frame(SampleID, mean, sd, measure)
alpha <- rbind(rich_stats, even_stats)
#Write csv of data to use for line plots
write.csv(alpha, "alpha_LS_ES.csv")
##############################################################################################################################
#With the csv files produced for Lower, Middle, and Upper, then name the headers accordingly and sort so you can plot the data:
#Read in the edited files, and change file names accordingly for each plot, the example here is for Middle:
alpha_LS_Richness= read.table("LS_Richness.csv",sep=',',header=TRUE)
ggplot(alpha_LS_Richness, aes(x = Order, y = Mean_2, color = Needle, group = Needle, shape = Needle)) +
  geom_point(size = 9) + 
  geom_line(size = 1.75) +
  facet_wrap(~measure, ncol = 1, scales = "free") +
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(values = c("black", "darkgreen", "navajowhite4", "darkgreen")) +
  geom_errorbar(aes(ymin=Min, ymax=Max), width=0.1, color="Black", size=0.75) +
  scale_x_continuous(
    breaks = c(1:1, 2:2, 3:3, 4:4, 5:5, 6:6, 7:7),
    labels = c("Aug2017", "Oct2017", "May2018", "Jul2018", "Oct2018", "May2019", "Sep2019")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
##############################################################################################################################
#Let's move on to 18S as well for alpha diversity metrics
#_______________Lower 18S_______________#
min_lib <- min(sample_sums(CB18_LM))
nsamp = nsamples(CB18_LM)
trials = 100
richness <- matrix(nrow = nsamp, ncol = trials)
row.names(richness) <- sample_names(CB18_LM)
evenness <- matrix(nrow = nsamp, ncol = trials)
row.names(evenness) <- sample_names(CB18_LM)
# It is always important to set a seed when you subsample so your result is replicable 
set.seed(3)
for (i in 1:100) {
  # Subsample
  r <- rarefy_even_depth(CB18_LM, sample.size = min_lib, verbose = FALSE, replace = TRUE)
  # Calculate richness
  rich <- as.numeric(as.matrix(estimate_richness(r, measures = "Observed")))
  richness[ ,i] <- rich
  # Calculate evenness
  even <- as.numeric(as.matrix(estimate_richness(r, measures = "InvSimpson")))
  evenness[ ,i] <- even
}
# Create a new dataframe to hold the means and standard deviations of richness estimates
SampleID <- row.names(richness)
mean <- apply(richness, 1, mean)
sd <- apply(richness, 1, sd)
measure <- rep("Richness", nsamp)
rich_stats <- data.frame(SampleID, mean, sd, measure)
# Create a new dataframe to hold the means and standard deviations of evenness estimates
SampleID <- row.names(evenness)
mean <- apply(evenness, 1, mean)
sd <- apply(evenness, 1, sd)
measure <- rep("Inverse Simpson", nsamp)
even_stats <- data.frame(SampleID, mean, sd, measure)
alpha <- rbind(rich_stats, even_stats)
write.csv(alpha, "alpha_LM_18S.csv")
#_______________Middle 18S_______________#
min_lib <- min(sample_sums(CB18_LS_regular))
nsamp = nsamples(CB18_LS_regular)
trials = 100
richness <- matrix(nrow = nsamp, ncol = trials)
row.names(richness) <- sample_names(CB18_LS_regular)
evenness <- matrix(nrow = nsamp, ncol = trials)
row.names(evenness) <- sample_names(CB18_LS_regular)
# It is always important to set a seed when you subsample so your result is replicable 
set.seed(3)
for (i in 1:100) {
  # Subsample
  r <- rarefy_even_depth(CB18_LS_regular, sample.size = min_lib, verbose = FALSE, replace = TRUE)
  # Calculate richness
  rich <- as.numeric(as.matrix(estimate_richness(r, measures = "Observed")))
  richness[ ,i] <- rich
  # Calculate evenness
  even <- as.numeric(as.matrix(estimate_richness(r, measures = "InvSimpson")))
  evenness[ ,i] <- even
}
# Create a new dataframe to hold the means and standard deviations of richness estimates
SampleID <- row.names(richness)
mean <- apply(richness, 1, mean)
sd <- apply(richness, 1, sd)
measure <- rep("Richness", nsamp)
rich_stats <- data.frame(SampleID, mean, sd, measure)
# Create a new dataframe to hold the means and standard deviations of evenness estimates
SampleID <- row.names(evenness)
mean <- apply(evenness, 1, mean)
sd <- apply(evenness, 1, sd)
measure <- rep("Inverse Simpson", nsamp)
even_stats <- data.frame(SampleID, mean, sd, measure)
alpha <- rbind(rich_stats, even_stats)
write.csv(alpha, "alpha_LS_18S.csv")
#_______________Middle-ES 18S_______________#
min_lib <- min(sample_sums(CB18_LS_induced))
nsamp = nsamples(CB18_LS_induced)
trials = 100
richness <- matrix(nrow = nsamp, ncol = trials)
row.names(richness) <- sample_names(CB18_LS_induced)
evenness <- matrix(nrow = nsamp, ncol = trials)
row.names(evenness) <- sample_names(CB18_LS_induced)
# It is always important to set a seed when you subsample so your result is replicable 
set.seed(3)
for (i in 1:100) {
  # Subsample
  r <- rarefy_even_depth(CB18_LS_induced, sample.size = min_lib, verbose = FALSE, replace = TRUE)
  # Calculate richness
  rich <- as.numeric(as.matrix(estimate_richness(r, measures = "Observed")))
  richness[ ,i] <- rich
  # Calculate evenness
  even <- as.numeric(as.matrix(estimate_richness(r, measures = "InvSimpson")))
  evenness[ ,i] <- even
}
# Create a new dataframe to hold the means and standard deviations of richness estimates
SampleID <- row.names(richness)
mean <- apply(richness, 1, mean)
sd <- apply(richness, 1, sd)
measure <- rep("Richness", nsamp)
rich_stats <- data.frame(SampleID, mean, sd, measure)
# Create a new dataframe to hold the means and standard deviations of evenness estimates
SampleID <- row.names(evenness)
mean <- apply(evenness, 1, mean)
sd <- apply(evenness, 1, sd)
measure <- rep("Inverse Simpson", nsamp)
even_stats <- data.frame(SampleID, mean, sd, measure)
alpha <- rbind(rich_stats, even_stats)
write.csv(alpha, "alpha_LS_ES_18S.csv")
#_______________Upper 18S_______________#
min_lib <- min(sample_sums(CB18_US))
nsamp = nsamples(CB18_US)
trials = 100
richness <- matrix(nrow = nsamp, ncol = trials)
row.names(richness) <- sample_names(CB18_US)
evenness <- matrix(nrow = nsamp, ncol = trials)
row.names(evenness) <- sample_names(CB18_US)
# It is always important to set a seed when you subsample so your result is replicable 
set.seed(3)
for (i in 1:100) {
  # Subsample
  r <- rarefy_even_depth(CB18_US, sample.size = min_lib, verbose = FALSE, replace = TRUE)
  # Calculate richness
  rich <- as.numeric(as.matrix(estimate_richness(r, measures = "Observed")))
  richness[ ,i] <- rich
  # Calculate evenness
  even <- as.numeric(as.matrix(estimate_richness(r, measures = "InvSimpson")))
  evenness[ ,i] <- even
}
# Create a new dataframe to hold the means and standard deviations of richness estimates
SampleID <- row.names(richness)
mean <- apply(richness, 1, mean)
sd <- apply(richness, 1, sd)
measure <- rep("Richness", nsamp)
rich_stats <- data.frame(SampleID, mean, sd, measure)
# Create a new dataframe to hold the means and standard deviations of evenness estimates
SampleID <- row.names(evenness)
mean <- apply(evenness, 1, mean)
sd <- apply(evenness, 1, sd)
measure <- rep("Inverse Simpson", nsamp)
even_stats <- data.frame(SampleID, mean, sd, measure)
alpha <- rbind(rich_stats, even_stats)
write.csv(alpha, "alpha_US_18S.csv")
##########################################################
##########################################################
#With the csv files produced for Lower, Middle, and Upper, then name the headers accordingly and sort so you can plot the data:
#Read in the edited files, and change file names accordingly for each plot, the example here is for Upper:
alpha_US18_Richness= read.table("alpha_US_18S_Richness.csv",sep=',',header=TRUE)
ggplot(alpha_US18_Richness, aes(x = Order, y = Mean_2, color = Needle, group = Needle, shape = Needle)) +
  geom_point(size = 9) + 
  geom_line(size = 1.75) +
  facet_wrap(~measure, ncol = 1, scales = "free") +
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(values = c("black", "darkgreen", "navajowhite4", "darkgreen")) +
  geom_errorbar(aes(ymin=Min, ymax=Max), width=0.1, color="Black", size=0.75) +
  scale_x_continuous(
    breaks = c(1:1, 2:2, 3:3, 4:4, 5:5, 6:6, 7:7),
    labels = c("Aug2017", "Oct2017", "May2018", "Jul2018", "Oct2018", "May2019", "Sep2019")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
