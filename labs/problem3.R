# Clear all previous data
rm(list = ls())

# Set parameters
a <- 0
b <- 0.5
c <- 0.3989423
set.seed(1) # For reproducibility

# Function to estimate the area using Monte Carlo method
monte_carlo_estimate <- function(n, a, b, c) {
  x <- runif(n, min = 0, max = b)
  y <- runif(n, min = 0, max = c)
  y_curve <- (1 / sqrt(2 * 3.14)) * exp(-x^2 / 2)
  f_n <- sum(y < y_curve)
  area_I <- (b - a) * c * f_n / n
  return(area_I)
}

# Estimate the area for different values of n
n_values <- c(20, 50, 500, 10000)
estimated_areas <- numeric(length(n_values))

for (i in 1:length(n_values)) {
  estimated_areas[i] <- monte_carlo_estimate(n_values[i], a, b, c)
  print(paste("n =", n_values[i], "Estimated area =", estimated_areas[i]))
}

# Calculate the accurate area from the Normal table
accurate_area <- pnorm(0.5) - pnorm(0)

# Load necessary libraries
library(ggplot2)

# Generate random points for the final plot
n_final_plot <- 10000
x_final_plot <- runif(n_final_plot, min = a, max = b)
y_final_plot <- runif(n_final_plot, min = 0, max = c)
plot_data_final <- data.frame(x = x_final_plot, y = y_final_plot)

# Create a data frame for ggplot
plot_data_final$class <- ifelse(plot_data_final$y < (1 / sqrt(2 * 3.14)) * exp(-plot_data_final$x^2 / 2), "inside", "outside")

# Generate the base plot
p_final <- ggplot(plot_data_final, aes(x = x, y = y, color = class)) +
  geom_point(size = 1) +
  stat_function(fun = function(x) (1 / sqrt(2 * 3.14)) * exp(-x^2 / 2), color = "blue", linewidth = 1.2) +
  xlim(a - 0.1, b + 0.1) +
  ylim(-0.1, c + 0.1) + # Adjusted axis limits for zooming out
  geom_rect(aes(xmin = a, xmax = b, ymin = 0, ymax = c), fill = "transparent", color = "black") +
  scale_color_manual(values = c("inside" = "green", "outside" = "red")) +
  labs(
    title = "Monte Carlo Method - Estimating Area under the Curve",
    subtitle = paste(
      "Number of points:", n_final_plot,
      "\nActual area:", round(accurate_area, 4),
      "\nEstimated area:", round(estimated_areas[length(estimated_areas)], 4)
    ),
    x = "x",
    y = "y",
    color = "Point"
  ) +
  theme_minimal()

# Display the final plot
print(p_final)
