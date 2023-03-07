# Plot distribution of demographic variables in both discovery & replication samples


library(tidyverse)
library(ggplot2)
#library(ggthemes)
library(cowplot)
library(patchwork)


# Clear workspace
rm(list = ls()) 

# Load data from disovery/replication samples into dataframes
work_dir = '~/Code/multimodal_psychopathology_components'
demog_train = read_csv(paste(work_dir, '/data/demog_subj_discovery.csv', sep="", collapse=""))
demog_train$dataset = "discovery"
demog_test = read_csv(paste(work_dir, '/data/demog_subj_replication.csv', sep="", collapse=""))
demog_test$dataset = "replication"

# Merge discovery & replication samples into a single dataframe
demog_all <- rbind(demog_train,demog_test)

# Font size across plots
size_text = 14
size_axis_title = 16
size_legend = 16
size_axis_line = 0.5

# Colors & legend for discovery/replication samples
my_colors = c("#296879", "#98b7b9") # Previously: c("#1d9367", "#a8224a")
my_samples = c("Discovery sample\n(N = 3,504)", "Replication sample\n(N = 1,747)")


# Create separate plot for each variable 

# 1. Age
binwidth.age <- diff(range(demog_all$age/12))/20 #15

g_age <- ggplot(data = demog_all) +
  geom_histogram(mapping = aes(x = demog_all$age/12, 
                               fill = dataset), 
                 binwidth = binwidth.age) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(expand = c(0, 0), # make bar start at 0 without the extra spacing
                     limits = c(0, 450)) +
  theme(text = element_text(color = "black", size = size_text, family = "Avenir Next"), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        legend.position = "none",
        axis.line = element_line(color = "black", size = size_axis_line),
        axis.text.x = element_text(color = "black", vjust = 0),
        axis.ticks.y = element_line(color = "black", size = size_axis_line),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = size_axis_title)) +
  labs(x = "Age", 
       y = "# Subjects")
g_age


# 2. Sex
g_sex <- ggplot(data = demog_all) +
  geom_bar(mapping = aes(x = sex, fill = dataset)) +
  scale_x_discrete(limit = c("1", "2"), 
                   labels = c("Male", "Female")) + 
  scale_fill_manual(values = my_colors,
                    labels = my_samples) +
  scale_y_continuous(expand = c(0, 0), # make bar start at 0 without the extra spacing
                     limits = c(0, 2700)) +
  theme(text = element_text(color = "black", size = size_text, family = "Avenir Next"), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        legend.position = "none",
        axis.line = element_line(color = "black", size = size_axis_line),
        axis.text.x = element_text(color = "black", vjust = 0),
        axis.ticks.y = element_line(color = "black", size = size_axis_line),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = size_axis_title)) +
  labs(x = "Sex",
       y = "# Subjects")
g_sex


# 3. Ethnicity
g_ethn <- ggplot(data = demog_all) + 
  geom_bar(mapping = aes(x = ethnicity, fill = dataset)) +
  scale_x_discrete(limit = c("1", "2", "3", "4", "5", "6"), 
                   labels = c("Asian", "Black", "Hispanic", "Other", "White", "Multi")) + 
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(expand = c(0, 0), # make bar start at 0 without the extra spacing
                     limits = c(0, 3000),
                     breaks = c(0,500,1000,1500,2000,2500,3000)) +
  theme(text = element_text(color = "black", size = size_text, family = "Avenir Next"), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        legend.position = "none",
        axis.line = element_line(color = "black", size = size_axis_line),
        axis.text.x = element_text(color = "black", vjust = 0),
        axis.ticks.y = element_line(color = "black", size = size_axis_line),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = size_axis_title)) +
  labs(x = "Ethnicity",
       y = "# Subjects")
g_ethn
  
  
# 4. Site
g_site <- ggplot(data = demog_all) + 
  geom_bar(mapping = aes(x = factor(site), # factor site so that unused levels are hidden
                         fill = dataset)) + 
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(expand = c(0, 0), # make bar start at 0 without the extra spacing
                     limits = c(0, 650)) +
  theme(text = element_text(color = "black", size = size_text, family = "Avenir Next"), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        legend.position = "none",
        axis.line = element_line(color = "black", size = size_axis_line),
        axis.text.x = element_text(color = "black", vjust = 0),
        axis.ticks.y = element_line(color = "black", size = size_axis_line),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = size_axis_title)) +
  labs(x = "Site",
       y = "# Subjects")
g_site


# 5. Overall psychopathology
g_psycho <- ggplot(data = demog_all) +
  geom_histogram(mapping = aes(x = overall_psychopathology, fill = dataset)) +
  scale_fill_manual(values = my_colors,
                    labels = my_samples) +
  scale_x_continuous(breaks = c(-2,0,2,4,6,8,10)) + 
  scale_y_continuous(expand = c(0, 0), # make bar start at 0 without the extra spacing
                     limits = c(0, 1050)) +
  theme(text = element_text(color = "black", size = size_text, family = "Avenir Next"), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        legend.position = "none",
        axis.line = element_line(color = "black", size = size_axis_line),
        axis.text.x = element_text(color = "black", vjust = 0),
        axis.ticks.y = element_line(color = "black", size = size_axis_line),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = size_axis_title)) +
  labs(x = "Overall psychopathology", 
       y = "# Subjects")
g_psycho


# Sham figure for legend only
g <- ggplot(data = demog_all) +
  geom_histogram(mapping = aes(x = overall_psychopathology, fill = dataset)) +
  scale_fill_manual(values = my_colors,
                    labels = my_samples) +
  theme(text = element_text(color = "black", size = size_text, family = "Avenir Next"), 
        legend.text = element_text(size = size_legend)) +
  labs(fill = '')
g
  

# Combine all into a panel
legend <- get_legend(g)

panel1 <- plot_grid(g_age, #+ theme(legend.position = "none"),
                    g_sex, #+ theme(legend.position = "none"), 
                    g_ethn, #+ theme(legend.position = "none"),
                    rel_widths = c(2, 1, 2),
                    nrow = 1,
                    align = "v")
panel1

panel2 <- plot_grid(g_site + theme(legend.position="none"),
                    g_psycho + theme(legend.position="none"),
                    legend,  #+ theme(legend.spacing = 0.5),
                    rel_widths = c(2,2,1),
                    nrow = 1)
panel2


panel3 <- plot_grid(panel1,
                    panel2,
                    nrow = 2)
panel3


# Save as png figure
figname = paste(work_dir, "/output/fig_distrib_samples.png", sep="", collapse="")
ggsave(figname, plot = panel3, height=20, width=30, units="cm")

