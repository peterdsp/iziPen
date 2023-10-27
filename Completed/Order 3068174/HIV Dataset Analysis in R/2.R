# Load the required libraries
library(survival)

# Read the dataset
data <- read.csv("dataHIV.csv")

# Create a survival object
surv.obj <- with(data, Surv(time, death))

# Perform the Logrank test
logrank_test <- survdiff(surv.obj ~ data$mode)

# Perform the Wilcoxon test
wilcox_test <- survdiff(surv.obj ~ data$mode, rho = 0)

# Summarize the results
cat("Logrank Test (Mantel-Haenszel):\n")
print(logrank_test)

cat("\nWilcoxon Test:\n")
print(wilcox_test)
