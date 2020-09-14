# MPG Regression

# read csv files
MechaCar_mpg <- read_csv("MechaCar_mpg.csv")
suspension_coil <- read_csv("Suspension_Coil.csv")
head(MechaCar_mpg)

# normality test
shapiro.test(MechaCar_mpg$mpg)

# visualize distribution using density plot
ggplot(MechaCar_mpg,aes(mpg)) + geom_density()

# Convert to numeric matrix
used_matrix <- as.matrix(MechaCar_mpg[,c("vehicle length","vehicle weight", "mpg" ,"spoiler angle", "ground clearance")]) 

cor(used_matrix)

# Multiple linear model
lm(mpg ~ `vehicle length` + `vehicle weight` + `ground clearance` + `spoiler angle` ,data = MechaCar_mpg)

# Summarize model
summary(model)

# Get mean, median, variance and Std Dev of stat table
summarize_suspension <- suspension_coil %>%
  summarise(Mean_PSI = mean(PSI), Median_PSI = median(PSI),Variance_PSI = var(PSI),Stdev_PSI =sd(PSI))

# Get mean, median, variance and Std Dev of mfg lot
group_summarize_suspension <- suspension_coil  %>% group_by(Manufacturing_Lot) %>%
  summarise(Mean_PSI = mean(PSI), Median_PSI = median(PSI),Variance_PSI = var(PSI),Stdev_PSI =sd(PSI))

# Sample T-Test
t.test(suspension_coil$PSI, mu = 1500)