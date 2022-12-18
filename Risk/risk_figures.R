library(viridis)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)
library(Rmisc)
library(tibble)
library(cowplot)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Settings ----------------------------------------------------------------
# Settings for plot
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


# Data prep ----------------------------------------------------------------
choices <- read.csv("~/Documents/GitHub/Serotonin_in_PD/SSRI_Analysis/Risk/Risk_HC_sort.csv")
choice_PD <- read.csv("~/Documents/GitHub/Serotonin_in_PD/SSRI_Analysis/Risk/Risk_sort.csv")

choice_PD <-na.omit(choice_PD)

choices$Group <- as.factor(choices$Group)
levels(choices$Group) <-list("Control"=0, "PD placebo" = 1, "PD citalopram" = 2)
choices$Apathy <- as.factor(choices$Apathy)
levels(choices$Apathy) <-list("No Apathy"=0, "Mild Apathy" = 1)
choice_PD$Apathy <- as.factor(choice_PD$Apathy)
levels(choice_PD$Apathy) <-list("No Apathy"=0, "Mild Apathy" = 1)

choices$Depression <- as.factor(choices$Depression)
levels(choices$Depression) <-list("No Depression"=0, "Mild Depression" = 1)
choice_PD$Depression <- as.factor(choice_PD$Depression)
levels(choice_PD$Depression) <-list("No Depression"=0, "Mild Depression" = 1)

choice_con <- filter(choices, Group == "Control")
choice_baseline <- filter(choices, Group == "Control" | Group=="PD placebo")
choice_PD_bs <-  filter(choices, Group == "PD placebo" | Group=="PD citalopram")
choice_PD_bs[1] <- c(1:18,1:18)

choices[1] <- c(1:60)
colnames(choices)[1]="Part"

# Gain apathy groups prep ------------------------------------------------------------------
apws_long_gain <- melt(choice_PD, id.vars = c("Part","Apathy"),
                      measure.vars = c("gain.plac", "gain.cit"),
                      variable.name = "Med")

apws_gain <- summarySE(apws_long_gain, measurevar="value", groupvars=c("Med","Apathy"),
                         na.rm=TRUE, conf.interval=.95)


apbs_gain <- summarySE(choice_con, measurevar="gain", groupvars=c("Group","Apathy"),
                                              na.rm=TRUE, conf.interval=.95)

colnames(apbs_gain)[1]="Med"
colnames(apbs_gain)[4]="value"

ap_gain <-rbind(apbs_gain,apws_gain)

# Gain depression groups prep ------------------------------------------------------------------
depws_long_gain <- melt(choice_PD, id.vars = c("Part","Depression"),
                       measure.vars = c("gain.plac", "gain.cit"),
                       variable.name = "Med")

depws_gain <- summarySE(depws_long_gain, measurevar="value", groupvars=c("Med", "Depression"),
                             na.rm=TRUE, conf.interval=.95)

depbs_gain <- summarySE(choice_con, measurevar="gain", groupvars=c("Group","Depression"),
                       na.rm=TRUE, conf.interval=.95)

colnames(depbs_gain)[1]="Med"
colnames(depbs_gain)[4]="value"

dep_gain <-rbind(depbs_gain,depws_gain)


# Loss apathy groups prep ------------------------------------------------------------------

apws_long_loss <- melt(choice_PD, id.vars = c("Part","Apathy"),
                       measure.vars = c("loss.plac", "loss.cit"),
                       variable.name = "Med")

apws_loss <- summarySE(apws_long_loss, measurevar="value", groupvars=c("Med","Apathy"),
                             na.rm=TRUE, conf.interval=.95)


apbs_loss <- summarySE(choice_con, measurevar="loss", groupvars=c("Group","Apathy"),
                       na.rm=TRUE, conf.interval=.95)

colnames(apbs_loss)[1]="Med"
colnames(apbs_loss)[4]="value"

ap_loss <-rbind(apbs_loss,apws_loss)


# Loss depression groups prep ------------------------------------------------------------------

depws_long_loss <- melt(choice_PD, id.vars = c("Part","Depression"),
                       measure.vars = c("loss.plac", "loss.cit"),
                       variable.name = "Med")

depws_loss <- summarySE(depws_long_loss, measurevar="value", groupvars=c("Med","Depression"),
                             na.rm=TRUE, conf.interval=.95)


depbs_loss <- summarySE(choice_con, measurevar="loss", groupvars=c("Group","Depression"),
                       na.rm=TRUE, conf.interval=.95)

colnames(depbs_loss)[1]="Med"
colnames(depbs_loss)[4]="value"

dep_loss <-rbind(depbs_loss,depws_loss)

# Mean risk seeking ----------------------------------------------------

a <- ggplot(data = subset(choices,!is.na(Group)), aes(x = Group, y = gain, fill = Group)) +
  labs(y = "Percent risky decisions") + 
  scale_x_discrete(name =" ") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = 1, color=NA) +
  geom_point(aes(y = gain, color = Group), position = position_jitter(width = .15), size = 2, alpha = 0.5) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  guides(fill = FALSE, col = FALSE) +
  scale_color_manual(values = col_groups) +
  scale_fill_manual(values = col_groups) +
  theme(text = element_text(size=20), axis.line = element_line(color = "grey",size = 0.5, linetype="solid"), axis.ticks.x = element_line()) 
a


pd <- position_dodge(0.2) # move them .01 to the left and right
b <-ggplot(data=subset(choices, Group!="Control"), aes(x = 1, y =gain, fill = Group, color=Group, linetype=Group)) +
  geom_point(aes(x=1, y = gain, color=Group), position = position_jitter(width = .01), size = 1, alpha = 0.7) + 
  theme_classic() +
  scale_color_manual(values = col_groups[2:3]) +
  scale_fill_manual(values = col_groups[2:3]) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(), legend.title=element_blank()) +
  labs(y="% risky decisions") +
  theme(text = element_text(size=20), axis.line = element_line(color = "grey",size = 0.5, linetype="solid"), axis.ticks.x = element_line())
b

# effects by apathy -----------------------

pd <- position_dodge(0.1) # move them .01 to the left and right
b <-ggplot(ap_gain, aes(x=Med, y=value, group=Apathy, color=Apathy, linetype=Apathy)) +
  geom_line(size=1,position=pd) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), size=0.7, width=0.1, position=pd) + 
  theme_classic() +
  scale_color_manual(values = col_multi) +
  scale_linetype_discrete(labels = c("No apathy", "Mild apathy")) +
  scale_x_discrete(labels=c("gain.plac" = "PD placebo", "gain.cit" = "PD citalopram","Control"="Control")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(), legend.title=element_blank()) +
  labs(y="% risky choices to gain") +
  theme(text = element_text(size=20), axis.line = element_line(color = "grey",size = 0.5, linetype="solid"), axis.ticks.x = element_line())
b


c <- ggplot(choices, aes(y = gain, x = Group, color=Apathy), position = position_jitter(width = .15), size = 2, alpha = 0.7) +
  geom_point(aes(y = gain, x = Group, color=Apathy), position = position_jitter(width = .15), size = 1, alpha = 0.7) + 
  scale_color_manual(values = col_multi) +
  labs(y="% risky choices to gain") +
  theme_classic() +
  theme(text = element_text(size=20), axis.line = element_line(color = "grey",size = 0.5, linetype="solid"), axis.ticks.x = element_line())
c


pd <- position_dodge(0.1) # move them .01 to the left and right
b <-ggplot(ap_loss, aes(x=Med, y=value, group=Apathy, color=Apathy, linetype=Apathy)) +
  geom_line(size=1,position=pd) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), size=0.7, width=0.1, position=pd) + 
  theme_classic() +
  scale_color_manual(values = col_multi) +
  scale_linetype_discrete(labels = c("No apathy", "Mild apathy")) +
  scale_x_discrete(labels=c("loss.plac" = "PD placebo", "loss.cit" = "PD citalopram","Control"="Control")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(), legend.title=element_blank()) +
  labs(y="% risky choices to avoid losing") +
  theme(text = element_text(size=20), axis.line = element_line(color = "grey",size = 0.5, linetype="solid"), axis.ticks.x = element_line())
b


c <- ggplot(choices, aes(y = loss, x = Group, color=Apathy), position = position_jitter(width = .15), size = 2, alpha = 0.7) +
  geom_point(aes(y = loss, x = Group, color=Apathy), position = position_jitter(width = .15), size = 1, alpha = 0.7) + 
  scale_color_manual(values = col_multi) +
  labs(y="% risky choices to avoid losing") +
  theme_classic() +
  theme(text = element_text(size=20), axis.line = element_line(color = "grey",size = 0.5, linetype="solid"), axis.ticks.x = element_line())
c

# effects by depression -----------------------

pd <- position_dodge(0.1) # move them .01 to the left and right
b <-ggplot(dep_gain, aes(x=Med, y=value, group=Depression, color=Depression, linetype=Depression)) +
  geom_line(size=1,position=pd) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), size=0.7, width=0.1, position=pd) + 
  theme_classic() +
  scale_color_manual(values = col_multi) +
  scale_linetype_discrete(labels = c("No depression", "Mild depression")) +
  scale_x_discrete(labels=c("gain.plac" = "PD placebo", "gain.cit" = "PD citalopram","Control"="Control")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(), legend.title=element_blank()) +
  labs(y="% risky choices to gain") +
  theme(text = element_text(size=20), axis.line = element_line(color = "grey",size = 0.5, linetype="solid"), axis.ticks.x = element_line())
b


c <- ggplot(choices, aes(y = gain, x = Group, color=Depression), position = position_jitter(width = .15), size = 2, alpha = 0.7) +
  geom_point(aes(y = gain, x = Group, color=Depression), position = position_jitter(width = .15), size = 1, alpha = 0.7) + 
  scale_color_manual(values = col_multi) +
  labs(y="% risky choices to gain") +
  theme_classic() +
  theme(text = element_text(size=20), axis.line = element_line(color = "grey",size = 0.5, linetype="solid"), axis.ticks.x = element_line())
c


pd <- position_dodge(0.1) # move them .01 to the left and right
b <-ggplot(dep_loss, aes(x=Med, y=value, group=Depression, color=Depression, linetype=Depression)) +
  geom_line(size=1,position=pd) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), size=0.7, width=0.1, position=pd) + 
  theme_classic() +
  scale_color_manual(values = col_multi) +
  scale_linetype_discrete(labels = c("No depression", "Mild depression")) +
  scale_x_discrete(labels=c("loss.plac" = "PD placebo", "loss.cit" = "PD citalopram","Control"="Control")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(), legend.title=element_blank()) +
  labs(y="% risky choices to loss") +
  theme(text = element_text(size=20), axis.line = element_line(color = "grey",size = 0.5, linetype="solid"), axis.ticks.x = element_line())
b

c <- ggplot(choices, aes(y = loss, x = Group, color=Depression), position = position_jitter(width = .15), size = 2, alpha = 0.7) +
  geom_point(aes(y = loss, x = Group, color=Depression), position = position_jitter(width = .15), size = 1, alpha = 0.7) + 
  scale_color_manual(values = col_multi) +
  labs(y="% risky choices to avoid losing") +
  theme_classic() +
  theme(text = element_text(size=20), axis.line = element_line(color = "grey",size = 0.5, linetype="solid"), axis.ticks.x = element_line())
c









