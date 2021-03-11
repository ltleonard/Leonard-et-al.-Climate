CB16_LM

min_lib <- min(sample_sums(CB16_US))

nsamp = nsamples(CB16_US)
trials = 100

richness <- matrix(nrow = nsamp, ncol = trials)
row.names(richness) <- sample_names(CB16_US)

evenness <- matrix(nrow = nsamp, ncol = trials)
row.names(evenness) <- sample_names(CB16_US)

# It is always important to set a seed when you subsample so your result is replicable 
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

write.csv(alpha, "alpha_US.csv")
##########################################################
##########################################################
alpha_LS_Simpson= read.table("alpha_LS_Simpson.csv",sep=',',header=TRUE)
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

##########################################################
##########################################################
alpha_LS_SM_Simpson= read.table("alpha_LS_SM_Simpson.csv",sep=',',header=TRUE)
alpha_LS_SM_Richness= read.table("Alpha_LS_SM_Richness.csv",sep=',',header=TRUE)


ggplot(alpha_LS_SM_Richness, aes(x = Order, y = Mean_2, color = Needle, group = Needle, shape = Needle)) +
  geom_point(size = 9) + 
  geom_line(size = 1.75) +
  facet_wrap(~measure, ncol = 1, scales = "free") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(values = c("black", "darkgreen", "navajowhite4", "darkgreen")) +
  geom_errorbar(aes(ymin=Min, ymax=Max), width=0.1, color="Black", size=0.75) +
  scale_y_continuous(limit=c(200,600), breaks=c(200,300,400,500,600))+
  scale_x_continuous(
    breaks = c(1:1, 2:2, 3:3, 4:4, 5:5, 6:6, 7:7),
    labels = c("Aug2017", "Oct2017", "May2018", "Jul2018", "Oct2018", "May2019", "Sep2019")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

##########################################################
##########################################################
alpha_LM_Simpson= read.table("alpha_LM_Simpson.csv",sep=',',header=TRUE)
alpha_LM_Richness= read.table("Alpha_LM_Richness.csv",sep=',',header=TRUE)


ggplot(alpha_LM_Richness, aes(x = Order, y = Mean_2, color = Needle, group = Needle, shape = Needle)) +
  geom_point(size = 9) + 
  geom_line(size = 1.75) +
  facet_wrap(~measure, ncol = 1, scales = "free") +
  scale_y_continuous(limit=c(200,600), breaks=c(200,300,400,500,600))+ 
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

##########################################################
##########################################################
alpha_US_Simpson= read.table("alpha_US_Simpson.csv",sep=',',header=TRUE)
alpha_US_Richness= read.table("alpha_US_Richness.csv",sep=',',header=TRUE)


ggplot(alpha_US_Richness, aes(x = Order, y = Mean_2, color = Needle, group = Needle, shape = Needle)) +
  geom_point(size = 9) + 
  geom_line(size = 1.75) +
  facet_wrap(~measure, ncol = 1, scales = "free") +
  scale_color_manual(values = c("black", "darkgreen", "navajowhite4", "darkgreen")) +
  scale_y_continuous(limit=c(200,600), breaks=c(200,300,400,500,600))+
  geom_errorbar(aes(ymin=Min, ymax=Max), width=0.1, color="Black", size=0.75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(
    breaks = c(1:1, 2:2, 3:3, 4:4, 5:5, 6:6, 7:7),
    labels = c("Aug2017", "Oct2017", "May2018", "Jul2018", "Oct2018", "May2019", "Sep2019")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

####################################################################################################################
####################################################################################################################
####################################################################################################################


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
##########################################################
##########################################################
alpha_US18_Simpson= read.table("alpha_US_18S_Simpson.csv",sep=',',header=TRUE)
alpha_US18_Richness= read.table("alpha_US_18S_Richness.csv",sep=',',header=TRUE)


ggplot(alpha_US18_Richness, aes(x = Order, y = Mean_2, color = Needle, group = Needle, shape = Needle)) +
  geom_point(size = 9) + 
  geom_line(size = 1.75) +
  facet_wrap(~measure, ncol = 1, scales = "free") +
  scale_color_manual(values = c("black", "darkgreen", "navajowhite4", "darkgreen")) +
  scale_y_continuous(limit=c(0,60), breaks=c(0,20,40,60))+
  geom_errorbar(aes(ymin=Min, ymax=Max), width=0.1, color="Black", size=0.75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(
    breaks = c(1:1, 2:2, 3:3, 4:4, 5:5, 6:6, 7:7),
    labels = c("Aug2017", "Oct2017", "May2018", "Jul2018", "Oct2018", "May2019", "Sep2019")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

##########################################################
##########################################################
alpha_LS18_Simpson= read.table("alpha_LS_18S_Simpson.csv",sep=',',header=TRUE)
alpha_LS18_Richness= read.table("alpha_LS_18S_Richness.csv",sep=',',header=TRUE)


ggplot(alpha_LS18_Richness, aes(x = Order, y = Mean_2, color = Needle, group = Needle, shape = Needle)) +
  geom_point(size = 9) + 
  geom_line(size = 1.75) +
  facet_wrap(~measure, ncol = 1, scales = "free") +
  scale_color_manual(values = c("black", "darkgreen", "navajowhite4", "darkgreen")) +
  geom_errorbar(aes(ymin=Min, ymax=Max), width=0.1, color="Black", size=0.75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(limit=c(0,60), breaks=c(0,20,40,60))+
  scale_x_continuous(
    breaks = c(1:1, 2:2, 3:3, 4:4, 5:5, 6:6, 7:7),
    labels = c("Aug2017", "Oct2017", "May2018", "Jul2018", "Oct2018", "May2019", "Sep2019")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

##########################################################
##########################################################
alpha_LS_SM18_Richness= read.table("alpha_LS_SM_18S_Richness.csv",sep=',',header=TRUE)
alpha_LS_SM18_Simpson= read.table("alpha_LS_SM_18S_Simpson.csv",sep=',',header=TRUE)


ggplot(alpha_LS_SM18_Richness, aes(x = Order, y = Mean_2, color = Needle, group = Needle, shape = Needle)) +
  geom_point(size = 9) + 
  geom_line(size = 1.75) +
  facet_wrap(~measure, ncol = 1, scales = "free") +
  scale_color_manual(values = c("black", "darkgreen", "navajowhite4", "darkgreen")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_errorbar(aes(ymin=Min, ymax=Max), width=0.1, color="Black", size=0.75) +
  scale_y_continuous(limit=c(0,60), breaks=c(0,20,40,60))+
  scale_x_continuous(
    breaks = c(1:1, 2:2, 3:3, 4:4, 5:5, 6:6, 7:7),
    labels = c("Aug2017", "Oct2017", "May2018", "Jul2018", "Oct2018", "May2019", "Sep2019")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

##########################################################
##########################################################
alpha_LM18_Simpson= read.table("alpha_LM_18S_Simpson.csv",sep=',',header=TRUE)
alpha_LM18_Richness= read.table("alpha_LM_18S_Richness.csv",sep=',',header=TRUE)


ggplot(alpha_LM18_Richness, aes(x = Order, y = Mean_2, color = Needle, group = Needle, shape = Needle)) +
  geom_point(size = 9) + 
  geom_line(size = 1.75) +
  facet_wrap(~measure, ncol = 1, scales = "free") +
  scale_color_manual(values = c("black", "darkgreen", "navajowhite4", "darkgreen")) +
  geom_errorbar(aes(ymin=Min, ymax=Max), width=0.1, color="Black", size=0.75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(limit=c(0,60), breaks=c(0,20,40,60))+
  scale_x_continuous(
    breaks = c(1:1, 2:2, 3:3, 4:4, 5:5, 6:6, 7:7),
    labels = c("Aug2017", "Oct2017", "May2018", "Jul2018", "Oct2018", "May2019", "Sep2019")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
