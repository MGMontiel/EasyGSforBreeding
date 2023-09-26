rm(list=ls())
library(sommer)
library(tidyverse)
library(ggplot2)
library(paletteer)
library(hrbrthemes)

## Figure for cross validation

setwd("")
my_palette <- c("#EE3A8C", "#2F4F4F","#4682B4","#FFA500")

res <- read.csv("CV.csv")

ggplot(res, aes(x=Material, y=Accuracy, fill=Model))+
  #geom_jitter(color="black", size=0.6, alpha=0.9)+
  geom_boxplot(color="black",position=position_dodge(preserve = "single"))+
  scale_fill_manual(values=my_palette)+
  facet_wrap(~trait)+
  theme_bw(14)+
  ggtitle("Cross Validation Predictive Ability of your pops") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size=16)) + 
  labs(y = "Predictive ability", x = "Materials") +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 1, by = 0.2))



# with the glitter
ggplot(res, aes(x=trait, y=Accuracy, fill=Material))+
  geom_jitter(color="black", size=0.6, alpha=0.9)+
  geom_boxplot(width=0.40, alpha= 0.7)+ 
  scale_fill_brewer(palette = "Dark2") +
  theme_ipsum() +
  theme (axis.title.x = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), 
         legend.position = "none")


