# Required libraries
library(ggplot2)

# Number of iterations
num_iterations <- 10


set.seed(42)

# Initialize the leader's action
leader_action <- sample(1:3, 1)

# Create an empty data frame to store actions and losses over time
data <- data.frame(
  Iteration = integer(),
  Action = integer(),
  Loss1 = numeric(),
  Loss2 = numeric(),
  Loss3 = numeric()
)

# Loss function for each action
loss <- function(action, true_action) {
  return(abs(action - true_action))
}

# Follow the Leader algorithm for online convex optimization
for (iteration in 1:num_iterations) {
  # Generate a random true action
  true_action <- sample(1:3, 1)
  
  # Calculate the loss for each action
  losses <- sapply(1:3, function(a) loss(a, true_action))
  
  # Select the action with the minimum cumulative loss up to this point
  leader_action <- which.min(cumsum(losses))
  
  # Add the chosen action and losses to the data frame
  data <- rbind(data, data.frame(
    Iteration = iteration,
    Action = leader_action,
    Loss_a1 = losses[1],
    Loss_a2 = losses[2],
    Loss_a3 = losses[3]
  ))
}

# Melt the data frame for plotting
melted_data <- reshape2::melt(data, id.vars = c("Iteration", "Action"))

# Line plot to visualize the losses of each action over iterations
ggplot(melted_data, aes(x = Iteration, y = value, color = variable)) +
  geom_line() +
  labs(title = "",
       x = "t",
       y = "Loss",
       color = "Action", size = 3) +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  scale_color_discrete(name = "Action") +
  theme_minimal() +
  theme(legend.text = element_text(size = 24),    # Increase legend text size
        axis.title = element_text(size = 24),     # Increase axis title size
        axis.text = element_text(size = 23))      # Increase axis label size
