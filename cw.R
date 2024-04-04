setwd("E:/Second year/Simulation/Everything R")

### Question 01

library(simmer) 
library(simmer.plot)
library(gridExtra)


set.seed(2000)

# initializes a simulation environment
env <- simmer("Call Centre")

customer <- trajectory("customer") %>%
  seize("csr", 1) %>%
  timeout(function() runif(1,1,5)) %>%
  release("csr", 1)

for (num_csr in 1:6) {
  env %>%
    add_resource("csr", num_csr) %>%
    add_generator("customer", customer, function() rexp(1, 1/3))
}


env %>%
  run(until=1440)

# Plot resource usage and waiting time
p1 <- plot(get_mon_resources(env), metric = "usage", items = "server", step = TRUE)
p2 <- plot(get_mon_arrivals(env), metric = "waiting_time")

# display the graphs
p1
p2



### Question 02

# set seed for consistency and reproducibility
set.seed(150)

# Generate 7000 values for power dissipated
n <- 7000
voltage <- rnorm(n, mean = 12, sd = 2)
resistance <- rnorm(n, mean = 8, sd = 1)

# calculate the power dissipated
power <- voltage^2 / resistance

# scatter plot to visualize the relationship between power and voltage
ggplot(data.frame(voltage, power), aes(x = voltage, y = power)) +
  geom_point(color = "purple") +
  labs(x = "Voltage (V)", y = "Power (W)",
       title = "Scatter plot of Power vs Voltage")

# approximate mean and variance of power
mean_power <- mean(power)
var_power <- var(power)

# Estimate the probability that the power is greater than 20W
prob_power_gt_20 <- sum(power > 20) / length(power)

print("Results")
print(paste("Mean of Power: ", mean_power))
print(paste("Variance of Power: ", var_power))
print(paste("Probability that Power > 20W: ", prob_power_gt_20))


### Question 03

# Define the simulation function
simulate_sample_means <- function(n_samples, sample_size) {
  # Initialize a vector to store sample means
  sample_means <- numeric(n_samples)
  
  # Generate samples and calculate their means
  for (i in 1:n_samples) {
    sample <- runif(sample_size, min = -5, max = 5)
    sample_means[i] <- mean(sample)
  }
  
  # Return the sample means
  return(sample_means)
}

# Define a function to plot the histogram of sample means
plot_sample_means <- function(sample_means, sample_size) {
  # Create a data frame with the sample means
  data <- data.frame(sample_means = sample_means)
  
  # Plot a histogram of the sample means
  ggplot(data, aes(x = sample_means)) +
    geom_histogram(binwidth = 0.1, fill = "pink", color = "black") +
    theme_minimal() +
    labs(title = paste("Histogram of Sample Means (n =", sample_size, ")"),
         x = "Sample Mean",
         y = "Frequency")
}

# Run the simulation for different sample sizes
for (sample_size in c(10, 15, 30, 50)) {
  sample_means <- simulate_sample_means(500, sample_size)
  print(plot_sample_means(sample_means, sample_size))
}


### Question 04

# setting seed
set.seed(150)

# number of walks
walks <- 120

# realization numbers
real_num <- 5

# Initialize a matrix to store the realizations
realizations <- matrix(nrow=walks, ncol=real_num)

# Generate the realizations
for (i in 1:real_num) {
  # Generate the random steps
  steps <- sample(c(-1, 1), walks, replace=TRUE)
  
  # Calculate the cumulative sum of the steps to get the random walk
  realizations[,i] <- cumsum(steps)
}

# Print the realizations
print(realizations)

  
  
  