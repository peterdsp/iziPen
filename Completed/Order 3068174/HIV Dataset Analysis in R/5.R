# Load the necessary library if not already loaded
# install.packages("survival") # Uncomment and run if you haven't installed the package
library(survival)

# Read the dataset
dataHIV <- read.csv("dataHIV.csv")

# Fit the Cox model with interaction
cox_model <- coxph(Surv(time, death) ~ mode * CD4, data = dataHIV)

# Summarize the model
summary(cox_model)
