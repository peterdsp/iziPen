# Load the required libraries
library(survival)
library(ggplot2)

# Read the dataset
data <- read.csv("dataHIV.csv")

# Create a survival object
surv.obj <- with(data, Surv(time, death))

# Create a Kaplan-Meier survival curve for each risk group
km_fit <- survfit(surv.obj ~ mode, data = data)

# Create a data frame for plotting
km_data <- data.frame(
  time = km_fit$time,
  survival = km_fit$surv,
  group = factor(km_fit$strata)  # Convert 'group' to a factor
)

# Create a Kaplan-Meier survival plot
p <- ggplot(km_data, aes(x = time, y = survival, color = group)) +
  geom_step() +
  labs(
    title = "Kaplan-Meier Survival Curves by Risk Group",
    x = "Time (Years)",
    y = "Survival Probability",
    color = "Risk Group"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("red", "green", "blue"))  # Customize colors

# Show the plot
print(p)
