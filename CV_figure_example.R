rm(list=ls())
library(sommer)
library(tidyverse)
library(ggplot2)
library(paletteer)
library(hrbrthemes)

## Figure 03

setwd("~/LSU Maria Montiel/Projects/project_mp/project_mp_01paper/project_mp_epistasis/Cross validation/results/")
my_palette <- c("#EE3A8C", "#2F4F4F","#4682B4","#FFA500")

res1 <- read.csv("20_MP2_CV_A.csv", header=TRUE)
res1$Material <- factor (c("MP2"))
#res1$Model <- factor (c("Additive"))

res2 <- read.csv("21_MP2_1_CV_A.csv", header=TRUE)
res2$Material <- factor (c("MP2"))
#res2$trial <- factor (c("21 MP2 trial 1"))

res3 <- read.csv("21_MP2_2_CV_A.csv", header = TRUE)
res3$Material <- factor (c("MP2"))
#res3$trial <- factor (c("21 MP2 trial 2"))

res4 <- read.csv("21_MP6-8_1_CV_A.csv", header = TRUE)
res4$Material <- factor (c("MP6-8"))
#res2$trial <- factor (c("21 MP2 trial 1"))

res5 <- read.csv("21_MP6-8_2_CV_A.csv", header = TRUE)
res5$Material <- factor (c("MP6-8"))
#res2$trial <- factor (c("21 MP2 trial 1"))

res6 <- read.csv("22_MP6-8_1_CV_A.csv", header = TRUE)
res6$Material <- factor (c("MP6-8"))

res7 <- read.csv("22_MP6-8_2_CV_A.csv", header = TRUE)
res7$Material <- factor (c("MP6-8"))

res8 <- read.csv("20_MP2_CV_AI.csv", header = TRUE)
res8$Material <- factor (c("MP2"))

res9 <- read.csv("21_MP2_1_CV_AI.csv", header =TRUE)
res9$Material <- factor (c("MP2"))

res10 <- read.csv("21_MP2_2_CV_AI.csv", header =TRUE)
res10$Material <- factor (c("MP2"))

res11 <- read.csv("21_MP6-8_1_CV_AI.csv", header = TRUE)
res11$Material <- factor (c("MP6-8"))

res12 <- read.csv("21_MP6-8_2_CV_AI.csv", header = TRUE)
res12$Material <- factor (c("MP6-8"))

res13 <- read.csv("22_MP6-8_1_CV_AI.csv", header = TRUE)
res13$Material <- factor (c("MP6-8"))

res14 <- read.csv("22_MP6-8_2_CV_AI.csv", header = TRUE)
res14$Material <- factor (c("MP6-8"))

res <- rbind(res1, res2, res3, res4, res5, res6, res7, res8,
             res9, res10, res11, res12, res13, res14)

write.csv(res, "corr_all.csv")

ggplot(res, aes(x=Material, y=Accuracy, fill=Model))+
  #geom_jitter(color="black", size=0.6, alpha=0.9)+
  geom_boxplot(color="black",position=position_dodge(preserve = "single"))+
  scale_fill_manual(values=my_palette)+
  facet_wrap(~trait)+
  theme_bw(14)+
  ggtitle("Cross Validation Predictive Ability of the MP Materials") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size=16)) + 
  labs(y = "Predictive ability", x = "Materials") +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 1, by = 0.2))


## plot separating by trial

# Calculate the mean and standard deviation for each Model, Material, and trait combination
summary_data <- res %>%
  group_by(Model, Material, trait) %>%
  summarize(Mean_Accuracy = mean(Accuracy, na.rm = TRUE),
            SD_Accuracy = sd(Accuracy, na.rm = TRUE))

ggplot(summary_data, aes(x = Material, y = Mean_Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), color = "black") +
  geom_errorbar(aes(ymin = Mean_Accuracy - SD_Accuracy, ymax = Mean_Accuracy + SD_Accuracy),
                width = 0.5,  # Adjust the width as needed
                position = position_dodge(width = 0.9),  # Adjust the width as needed
                size = 0.4) +
  scale_fill_manual(values = my_palette) +
  facet_wrap(~trait) +
  theme_bw(base_size = 14) +
  ggtitle("Cross Validation Predictive Ability of the MP Materials comparing A versus AI effects") +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 1, by = 0.2))+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 16, lineheight = 1.2)) +
  labs(y = "Predictive ability", x = "Materials") +
  ggtitle("Cross Validation Predictive Ability of the MP Materials\ncomparing A versus AI effects")



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


