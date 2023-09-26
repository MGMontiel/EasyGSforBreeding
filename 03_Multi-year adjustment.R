rm(list=ls())
library(sommer)
library(SpATS)

##### set working directory###

setwd("~/LSU Maria Montiel/Projects/project_mp/project_mp_01paper/project_mp_epistasis/Metrics-blups/BLUES/blups-weighted/")

##### Loading the data#####


blup1 <- read.csv("21_MP2_1_blues_w.csv", header=T)
blup2 <- read.csv("21_MP2_2_blues_w.csv", header=T)
blup<- rbind(blup1, blup2)

#blup <- read.csv("20_MP2_blues_w.csv")

##### MET Model 

blup$trial <- as.factor(blup$trial)
blup$germplasmName <- as.factor(blup$germplasmName)

#blup$w_grain_length <- as.numeric(blup$w_grain_length)
#blup$milling <- as.numeric(blup$milling)
#blup$milling_w <- as.numeric(blup$milling_w)
str(blup)


# Calculate the BLUPS
met.<- mmer  (fixed = milling ~ 1 + trial,  
             random = ~ germplasmName,
             rcov = ~ vsr(units),
             data = blup)
                      
# with weights
met.<- mmer  (fixed = milling ~ 1 + trial,  
              random = ~ germplasmName,
              rcov = ~ vsr(units),
              data = blup,
              weights = milling_w )


# One trial, to have just blups (one date of planting)

met.<- mmer  (fixed = chalk ~ 1,  
              random = ~ germplasmName,
              rcov = ~ vsr(units),
              data = blup,
              weights = chalk_w )

# extract the blups form any of these models
blups.met <- predict.mmer(met., classify=c("germplasmName"), D = "germplasmName" )
blups <- blups.met$pvals

write.csv(blups, "blups.csv" )

##### Loading the data for making a loop for all the traits#####


blup1 <- read.csv("21_MP6-8_blups_w.csv", header=T)
blup2 <- read.csv("22_MP6-8_blups_w.csv", header=T)

selected_columns <- c("yield", "dth", "grain_length", "chalk", "milling", "trial", "germplasmName")  
data1_subset <- blup1[, selected_columns] # when both data sets don't have same columns
data2_subset <- blup2[, selected_columns]
blup <- rbind(data1_subset, data2_subset)

blup<- rbind(blup1, blup2)
str(blup)

##### MET Model 

blup$trial <- as.factor(blup$trial)
blup$germplasmName <- as.factor(blup$germplasmName)
#blup$milling <- as.numeric(blup$milling)

### loop for all the traits, with no weights

trait_columns <- c("yield", "dth", "grain_length", "chalk", "milling")


# Initialize a list to store adjusted results for each trait
adjusted_results <- list()

# Loop through each trait
for (trait in trait_columns) {
  
  met_model <- mmer(fixed = as.formula(paste(trait, "~ 1 + trial")),  
                    random = ~ germplasmName, # Random effect of germplasmName
                    rcov =  ~ vsr(units),    
                    data = blup)
  
  # Calculate multi-environmental BLUPs
  blups_met <- predict.mmer(met_model, classify = "germplasmName", D = "germplasmName")
  blups <- blups_met$pvals
  
  adjusted_results[[trait]] <- blups
}

# Combine results from the list into a single data frame
combined_results <- do.call(rbind, lapply(names(adjusted_results), function(trait) {
  data.frame(germplasmName = rownames(adjusted_results[[trait]]), trait = trait, adjusted_value = adjusted_results[[trait]])
}))

write.csv(combined_results, "blups.csv")


#----End----#


# weights for ID's - adjust residual for further analysis 
#vcov.mme < fit$vcov$C11_inv 
#w <- diag(vcov.mme)
