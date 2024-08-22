library(vars)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

# Get the total length of the data
n <- nrow(varnew_ts)

# Get fitted values and residuals
fitted_values <- fitted(var_modelnew)
residuals_values <- residuals(var_modelnew)

# Adjust the length of fitted values and residuals to match the original data
fitted_incidences <- c(rep(NA, n - nrow(fitted_values)), fitted_values[, "incidences"])
residuals_incidences <- c(rep(NA, n - nrow(residuals_values)), residuals_values[, "incidences"])

fitted_mobility <- c(rep(NA, n - nrow(fitted_values)), fitted_values[, "mobility"])
residuals_mobility <- c(rep(NA, n - nrow(residuals_values)), residuals_values[, "mobility"])

fitted_policy <- c(rep(NA, n - nrow(fitted_values)), fitted_values[, "policy"])
residuals_policy <- c(rep(NA, n - nrow(residuals_values)), residuals_values[, "policy"])

# Create data frames for each variable
data_incidences <- data.frame(
  time = time(varnew_ts),
  actual = varnew_ts[, "incidences"],
  fitted = fitted_incidences,
  residuals = residuals_incidences
)

data_mobility <- data.frame(
  time = time(varnew_ts),
  actual = varnew_ts[, "mobility"],
  fitted = fitted_mobility,
  residuals = residuals_mobility
)

data_policy <- data.frame(
  time = time(varnew_ts),
  actual = varnew_ts[, "policy"],
  fitted = fitted_policy,
  residuals = residuals_policy
)

# Convert data to long format for ggplot
data_long_incidences <- data_incidences %>%
  gather(key = "Type", value = "Value", actual, fitted)

data_long_mobility <- data_mobility %>%
  gather(key = "Type", value = "Value", actual, fitted)

data_long_policy <- data_policy %>%
  gather(key = "Type", value = "Value", actual, fitted)

# Plot fitted and actual values for each variable
fit_plot_incidences <- ggplot(data_long_incidences, aes(x = time, y = Value, color = Type)) +
  geom_line(size = 1) +
  labs(title = "Fit and Actual Values for Incidences",
       x = "Time",
       y = "Values") +
  scale_color_manual(values = c("actual" = "black", "fitted" = "blue")) +
  theme_minimal()

fit_plot_mobility <- ggplot(data_long_mobility, aes(x = time, y = Value, color = Type)) +
  geom_line(size = 1) +
  labs(title = "Fit and Actual Values for Mobility",
       x = "Time",
       y = "Values") +
  scale_color_manual(values = c("actual" = "black", "fitted" = "blue")) +
  theme_minimal()

fit_plot_policy <- ggplot(data_long_policy, aes(x = time, y = Value, color = Type)) +
  geom_line(size = 1) +
  labs(title = "Fit and Actual Values for Policy",
       x = "Time",
       y = "Values") +
  scale_color_manual(values = c("actual" = "black", "fitted" = "blue")) +
  theme_minimal()

# Plot residuals for each variable
resid_plot_incidences <- ggplot(data_incidences, aes(x = time, y = residuals)) +
  geom_line(color = "black", size = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals for Incidences",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

resid_plot_mobility <- ggplot(data_mobility, aes(x = time, y = residuals)) +
  geom_line(color = "black", size = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals for Mobility",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

resid_plot_policy <- ggplot(data_policy, aes(x = time, y = residuals)) +
  geom_line(color = "black", size = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals for Policy",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

# Arrange plots for each variable on separate pages
grid.arrange(fit_plot_incidences, resid_plot_incidences, nrow = 2)
grid.arrange(fit_plot_mobility, resid_plot_mobility, nrow = 2)
grid.arrange(fit_plot_policy, resid_plot_policy, nrow = 2)

library(vars)

#Impulse response 
irf_result <- irf(var_modelnew, impulse = "incidences", response = "mobility", n.ahead = 5, boot = TRUE)
plot(irf_result)
irf_result_policy <- irf(var_modelnew, impulse = "policy", response = "incidences", n.ahead = 10, boot = TRUE)
plot(irf_result_policy)

########## circle root plot
var_roots <- roots(var_modelnew)
plot(Re(var_roots), Im(var_roots), type = "p", 
     xlim = c(-1, 1), ylim = c(-1, 1),
     xlab = "Real", ylab = "Imaginary", 
     main = "Roots of the Companion Matrix",
     asp = 1)
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "blue")
abline(h = 0, v = 0, col = "gray")
