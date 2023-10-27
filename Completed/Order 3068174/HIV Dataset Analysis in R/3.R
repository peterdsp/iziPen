# Load the necessary packages
library(survival)
library(ggplot2)

# Read the dataset
data <- read.csv("dataHIV.csv")

# Fit a Cox proportional hazards model for each risk group
model_msm <- coxph(Surv(time, death) ~ CD4, data = subset(data, mode == "MSM"))
model_msw <- coxph(Surv(time, death) ~ CD4, data = subset(data, mode == "MSW"))
model_pwid <- coxph(Surv(time, death) ~ CD4, data = subset(data, mode == "PWID"))

# Get the estimated cumulative hazard functions for each model
cumhaz_msm <- survfit(model_msm)
cumhaz_msw <- survfit(model_msw)
cumhaz_pwid <- survfit(model_pwid)

# Extract the survival data
cumhaz_msm_df <- data.frame(time = cumhaz_msm$time, cumhazard = -log(cumhaz_msm$surv), group = "MSM")
cumhaz_msw_df <- data.frame(time = cumhaz_msw$time, cumhazard = -log(cumhaz_msw$surv), group = "MSW")
cumhaz_pwid_df <- data.frame(time = cumhaz_pwid$time, cumhazard = -log(cumhaz_pwid$surv), group = "PWID")

# Combine the data frames
cumhaz_data <- rbind(cumhaz_msm_df, cumhaz_msw_df, cumhaz_pwid_df)

# Plot the estimated cumulative hazard functions for each group
ggplot(data = cumhaz_data, aes(x = time, y = cumhazard, color = group)) +
  geom_step() +
  labs(title = "Cumulative Hazard Functions by Risk Group",
       x = "Time (years)",
       y = "Cumulative Hazard",
       color = "Risk Group") +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal()

# Save the plot as an image file if needed
ggsave("cumulative_hazard_plot.png", width = 8, height = 6)