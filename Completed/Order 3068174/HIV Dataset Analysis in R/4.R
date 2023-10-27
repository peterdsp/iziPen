# Load the survival package
library(survival)

# Read the dataset
dataHIV <- read.csv("dataHIV.csv")

# Fit a Cox regression model
cox_model <- coxph(Surv(time, death) ~ mode + CD4, data = dataHIV)

# Summarize the model
summary(cox_model)