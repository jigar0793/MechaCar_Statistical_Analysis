# Loading the dplyr library
library(dplyr)

### DELIVERABLE #1
# Importing the mechacar data
mecha_data <- read.csv('/Users/jigar/Desktop/UCSD/CHALLENEGES/Module-15/MechaCar_Statistical_Analysis/Resources/MechaCar_mpg.csv',check.names = F, stringsAsFactors = F)

# Creating a Multiple linear regression mode using mpg as the dependent variable - passing in the dataset
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mecha_data)

# Generating summary to find out the p value and r-squared value
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mecha_data))


### DELIVERABLE #2
# Importing the mechacar data
suspension_data <- read.csv('/Users/jigar/Desktop/UCSD/CHALLENEGES/Module-15/MechaCar_Statistical_Analysis/Resources/Suspension_Coil.csv',check.names = F, stringsAsFactors = F)

# Creating a summary that contains the mean, median, variance, and standard deviation of the PSI column
total_summary <- summarize(suspension_data, Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

# Creating a summary table that contains the mean/median/variance/SD of cars grouped up by lot number
lot_summary = suspension_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')



### DELIVERABLE #3 - Determining if the sample represent the population (T-test)
# Using a t-test to see if the sample (all cars in the dataset)is statistically different from the population
t.test(suspension_data$PSI, mu = 1500)

# Using a t-test to see if each lot is statistically different from the population 
t.test(mu = 1500, subset(suspension_data,Manufacturing_Lot =="Lot1")$PSI)
t.test(mu = 1500, subset(suspension_data,Manufacturing_Lot =="Lot2")$PSI)
t.test(mu = 1500, subset(suspension_data,Manufacturing_Lot =="Lot3")$PSI)