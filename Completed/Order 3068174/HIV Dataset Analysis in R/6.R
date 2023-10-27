# Load the necessary libraries
library(dplyr)
library(survival)

# Read the dataset
dataHIV <- read.csv("dataHIV.csv")

# Define the Cox proportional hazards model with CD4 on the original scale
model_original <- coxph(Surv(time, death) ~ CD4 + mode, data = dataHIV)

# Define the Cox proportional hazards model with CD4 on the log10 scale
dataHIV$log_CD4 <- log10(dataHIV$CD4 + 1)
model_log <- coxph(Surv(time, death) ~ log_CD4 + mode, data = dataHIV)

# Define the Cox proportional hazards model with CD4 on the square root scale
dataHIV$sqrt_CD4 <- sqrt(dataHIV$CD4)
model_sqrt <- coxph(Surv(time, death) ~ sqrt_CD4 + mode, data = dataHIV)

# Compare the models using AIC
AIC_original <- AIC(model_original)
AIC_log <- AIC(model_log)
AIC_sqrt <- AIC(model_sqrt)

# Print the AIC values for each model
cat("AIC for Original Scale CD4:", AIC_original, "\n")
cat("AIC for log10 Scale CD4:", AIC_log, "\n")
cat("AIC for Square Root Scale CD4:", AIC_sqrt, "\n")

# Determine the best functional form
if (AIC_original <= AIC_log && AIC_original <= AIC_sqrt) {
  cat("The original scale is the best functional form for CD4.")
} else if (AIC_log <= AIC_original && AIC_log <= AIC_sqrt) {
  cat("The log10 scale is the best functional form for CD4.")
} else {
  cat("The square root scale is the best functional form for CD4.")
}
