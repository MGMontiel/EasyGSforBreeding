rm(list=ls())
library(sommer)
library(SpATS)

##### set working directory###

setwd("")

##### Loading the data#####


blup1 <- read.csv("trial1_blues.csv", header=T)
blup2 <- read.csv("trial2_blues.csv", header=T)
blup<- rbind(blup1, blup2)

##### MET Model 

blup$trial <- as.factor(blup$trial)
blup$germplasmName <- as.factor(blup$germplasmName)

str(blup) #make sure your traits are numerics


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


blup1 <- read.csv("trial1.csv", header=T)
blup2 <- read.csv("trial2.csv", header=T)

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


#----FIN----#


