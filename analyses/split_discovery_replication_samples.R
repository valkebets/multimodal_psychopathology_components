## Split ABCD sample into discovery and replication subsamples
# Written by Valeria Kebets, based on the code by Cedric Huchuan Xia
# https://github.com/cedricx/sCCA/blob/master/sCCA/code/final/go1sample_split.Rmd
# https://doi.org/10.1038/s41467-018-05317-y
  

library('caret')
library('ggplot2')
library('readr')
library('tidyr')
library('dplyr')
library('grid')
library('cowplot')


# Clear workspace
rm(list = ls()) 

# Load demographic data from all subjects
work_dir = '~/Code/multimodal_psychopathology_components'
df <- read.csv(paste(work_dir, '/data/demog_all_subj.csv', sep="", collapse=""), header=TRUE)

out_disc = paste(work_dir, '/data/demog_subj_discovery.csv', sep="", collapse="")
out_repl = paste(work_dir, '/data/demog_subj_replication.csv', sep="", collapse="")

# Split factor
split_factor = 0.667 # Split sample in 2/3 (discovery) and 1/3 (replication) subsamples

# Merge ethnicities into a single variable
# Add 'multiracial' category
df2 <- df %>%
  mutate(multi=rowSums(.[5:9]))

df2$ethnicity <- ifelse(df2$multi>1, '6',
                        ifelse(df2$asian=='1', '1',
                               ifelse(df2$black=='1', '2',
                                      ifelse(df2$hispanic=='1', '3',
                                             ifelse(df2$other=='1', '4',
                                                    ifelse(df2$white=='1', '5', 'x'))))))

# Keep only the combined ethnicity variable
demog <- df2[,c('subjectID','age','sex','site','ethnicity','overall_psychopathology')]

# Set seed so that the data are always split the same way
set.seed(3456)

# Split data
split_index <- createDataPartition(demog$overall_psychopathology, p = split_factor, list = F, times=1)
demog_disc <- demog[split_index,]
demog_repl <- demog[-split_index,]

# Save the sample split
write.csv(demog_disc, out_disc, row.names=FALSE, quote=FALSE)
write.csv(demog_repl, out_repl, row.names=FALSE, quote=FALSE)

# Print number of subjects in each subsample
cat('The final discovery sample has', dim(demog_disc)[1], 'subjects.')
cat('The final replication sample has', dim(demog_repl)[1], 'subjects.')

## Visualize the sample split
train.title <- "Discovery"
y.label <- "Percentage"
numbin <- 15
train.sex <- ggplot(demog_disc, aes(sex)) +
  geom_bar(aes(y=..count../sum(..count..)*100), fill='pink', alpha=0.8, width =0.5) +
  theme(axis.ticks = element_blank()) +
  scale_x_discrete(limit = c("1", "2"), labels = c("male","female"))+
  xlab("Sex") + ylab(y.label) + ggtitle(train.title)
train.ethnicity <- ggplot(demog_disc, aes(ethnicity)) +
  geom_bar(aes(y=..count../sum(..count..)*100), fill='red', alpha=0.8, width =0.5) +
  theme(axis.ticks = element_blank()) +
  scale_x_discrete(limit = c("1", "2","3","4","5","6"), 
                   labels = c("Asian","Black","Hispanic","Other","White","Multiracial"))+
  xlab("Sex") + ylab(y.label) + ggtitle(train.title)
binwidth.age <- diff(range(demog_disc$age/12))/numbin
train.age <- ggplot(demog_disc,aes(age/12)) +
  geom_histogram(aes(y=..count../sum(..count..)*100), binwidth=binwidth.age, fill='orange', alpha=0.8) +
  xlab("Age") + ylab(y.label) + ggtitle(train.title)
binwidth.psycho <- diff(range(demog_disc$overall_psychopathology))/numbin
train.psycho <- ggplot(demog_disc,aes(overall_psychopathology)) +
  geom_histogram(aes(y=..count../sum(..count..)*100), binwidth=binwidth.psycho, fill='purple', alpha=0.8) +
  xlab("Overall Psych") + ylab(y.label) + ggtitle(train.title)

test.title <- "Replication"
test.sex <- ggplot(demog_repl,aes(sex)) +
  geom_bar(aes(y=..count../sum(..count..)*100), fill='pink', alpha=0.5, width =0.5) +
  theme(axis.ticks = element_blank()) +
  scale_x_discrete(limit = c("1", "2"), labels = c("male","female"))+
  xlab("Sex") + ylab(y.label) + ggtitle(test.title)
test.ethnicity <- ggplot(demog_repl, aes(ethnicity)) +
  geom_bar(aes(y=..count../sum(..count..)*100), fill='red', alpha=0.5, width =0.5) +
  theme(axis.ticks = element_blank()) +
  scale_x_discrete(limit = c("1", "2","3","4","5","6"), 
                   labels = c("Asian","Black","Hispanic","Other","White","Multiracial"))+
  xlab("Sex") + ylab(y.label) + ggtitle(test.title)
binwidth.age <- diff(range(demog_repl$age/12))/numbin
test.age <- ggplot(demog_repl, aes(age/12)) +
  geom_histogram(aes(y=..count../sum(..count..)*100), binwidth=binwidth.age, fill='orange', alpha=0.5) +
  xlab("Age") + ylab(y.label) + ggtitle(test.title)

binwidth.psycho <- diff(range(demog_repl$overall_psychopathology))/numbin
test.psycho <- ggplot(demog_repl,aes(overall_psychopathology)) +
  geom_histogram(aes(y=..count../sum(..count..)*100), binwidth=binwidth.psycho, fill='purple', alpha=0.5) +
  xlab("Overall Psych") + ylab(y.label) + ggtitle(test.title)


# Combine all plots into a panel
data.split.plot <- plot_grid(train.sex, test.sex, train.ethnicity, test.ethnicity, train.age, test.age,
                             train.psycho, test.psycho,
                             align = "h", nrow =4)
data.split.plot

