rm(list=ls())
library(sommer)
library(tidyverse)
library(ggplot2)
library(paletteer)
library(hrbrthemes)

## Figure 03

setwd("~/LSU Maria Montiel/Projects/project_mp/project_mp_01paper/project_mp_epistasis/Cross validation/results/")
my_palette <- c("#4682B4","#FFA500","#EE3A8C", "#2F4F4F")
res <- read.csv("corr_all.csv")

# barplot of every trial by A and AI
ggplot(res, aes(x = trial , y = Accuracy, fill = Model)) +
  geom_bar(stat = "summary", position = "dodge", color = "black", fun ="mean") +
  scale_fill_manual(values = my_palette) +
  facet_wrap(~trait, scales = "free_x") +
  theme_bw(base_size = 14) +
  ggtitle("Cross Validation Predictive Ability of the MP Materials comparing A versus AI effects") +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 1, by = 0.2))+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 16, lineheight = 1.2)) +
  labs(y = "Predictive ability", x = "Trials") +
  ggtitle("Cross Validation Predictive Ability of the MP Materials comparing A versus AI effects")

# boxplot for every trial

ggplot(res, aes(x=trial, y=Accuracy, fill=Model))+
  geom_jitter(color="black", size=0.6, alpha=0.9)+
  geom_boxplot(color="black",position=position_dodge(preserve = "single"))+
  scale_fill_manual(values=my_palette)+
  facet_wrap(~trait)+
  theme_bw(14)+
  ggtitle("Cross Validation Predictive Ability of the MP Materials") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size=16)) + 
  labs(y = "Predictive ability", x = "Materials") +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 1, by = 0.2))
