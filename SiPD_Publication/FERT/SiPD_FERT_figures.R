library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)
library(haven)
library(psycho)
library(tidyverse)
library(viridis)
library(cowplot)

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Settings for plot ----------------------
raincloud_theme = theme(
  text = element_text(size = 20),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text = element_text(size = 18),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(size=0.5,linetype = "solid", colour = "grey"),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


col_groups <-c("#ABD822", '#14A4B1' ,'#002766')
col_base <- c("#ABD822", '#13657D')
col_multi <- magma(2, begin = 0.2, end = 0.65, direction = 1)


FERT_acc_HC_sort <- read.csv("~/Documents/GitHub/Serotonin_in_PD/SSRI_Analysis/FERT/FERT_acc_HC_sort.csv")
FERT_acc_sort <- read.csv("~/Documents/GitHub/Serotonin_in_PD/SSRI_Analysis/FERT/FERT_acc_sort.csv")
# Melt data into long format ---------------------

FERT_acc_HC_sort$Depression <- as.factor(FERT_acc_HC_sort$Depression)
levels(FERT_acc_HC_sort$Depression) <-list("No Depression"=0, "Mild Depression" = 1)

FERT_acc_HC_sort$Apathy <- as.factor(FERT_acc_HC_sort$Apathy)
levels(FERT_acc_HC_sort$Apathy) <-list("No Apathy"=0, "Mild Apathy" = 1)

FERT_acc_HC_sort$Part <- as.factor(FERT_acc_HC_sort$Part)
levels(FERT_acc_HC_sort$Part) <-list("Control"=0, "PD placebo" = 1, "PD citalopram" = 2)

FERT_long_HC=vector(mode="list", length=4)
FERT_long_HC[[1]] <- melt(FERT_acc_HC_sort, id.vars = c("Part","Apathy"),measure.vars = c("Happy","Sad","Disgusted","Afraid","Surprised","Angry"),
                    variable.name = "Emotion", value.name = "Accuracy")

FERT_long_HC[[2]] <- melt(FERT_acc_HC_sort, id.vars = c("Part","Apathy"),measure.vars = c("Q1_Happy","Q1_Sad","Q1_Disgust","Q1_Afraid","Q1_Surprised","Q1_Angry"),
                    variable.name = "Emotion", value.name = "Accuracy",na.rm = TRUE)
FERT_long_HC[[3]] <- melt(FERT_acc_HC_sort, id.vars = c("Part","Apathy"),measure.vars = c("Q2_Happy","Q2_Sad","Q2_Disgust","Q2_Afraid","Q2_Surprised","Q2_Angry"),
                          variable.name = "Emotion", value.name = "Accuracy")

FERT_long_HC[[4]] <- melt(FERT_acc_HC_sort, id.vars = c("Part","Apathy"),measure.vars = c("RT_Happy","RT_Sad","RT_Disgusted","RT_Afraid","RT_Surprised","RT_Angry"),
                          variable.name = "Emotion", value.name = "Accuracy")


# Get some summary statistics
lb <- function(x) mean(x,na.rm = TRUE) - sd(x,na.rm = TRUE)/sqrt(length(x))
ub <- function(x) mean(x,na.rm = TRUE) + sd(x,na.rm = TRUE)/sqrt(length(x))

sumld_Acc<- ddply(FERT_long_HC[[1]][complete.cases(FERT_long_HC[[1]]),], .(Emotion,Part,Apathy),summarise, mean = mean(Accuracy,na.omit = TRUE), 
                  median = median(Accuracy,na.omit = TRUE), lower = lb(Accuracy), upper = ub(Accuracy))


# effect of apathy ----------
FERT_ap <- subset(FERT_acc_HC_sort, Part!="Control")
FERT_ap_long <- melt(FERT_ap, id.vars = c("Apathy"),measure.vars = c("Q1_Happy","Q1_Sad","Q1_Disgust","Q1_Afraid","Q1_Surprised","Q1_Angry"),
                          variable.name = "Emotion", value.name = "Accuracy",na.rm = TRUE)
my_colours <- col_multi
j <- ggplot(data = FERT_ap_long, aes(x = as.factor(Emotion), y = Accuracy, color = as.factor(Apathy), group =  as.factor(Apathy))) +
  geom_line(position = position_dodge(0.5)) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Accuracy in %") + 
  scale_x_discrete(name ="Emotion") +
  theme(text = element_text(size=16), axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5),
        axis.line = element_line(color = "grey",size = 0.5,linetype="solid"), axis.ticks.x = element_line())
j



