library(ggplot2)

# Define some points in two dimensional scatter

p1 = c(1, 2)
p2 = c(3, 1)
p3 = c(1, 1)
p4 = c(4, 4)

df = as.data.frame(rbind(p1,p2,p3,p4))

# Plot points

plot1 = ggplot(df, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, stroke = 1) +
  geom_text(label = "Minority Instance", vjust = 3) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))

plot1

# Interpolate p2 with p1 for K=2
# p2 is colored red and its two nearest neighbors blue
# The convex line between the selected neighbor is colored in green


plot2 = ggplot(df, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, stroke = 1) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  geom_segment(aes(x = 1, y = 2, xend = 3, yend = 1), color = "green") +
  geom_point(data = df[2 ,], size = 5, shape = 3, color = "red", stroke = 1) +
  geom_text(data = df[2 ,], label = "Selected Point", vjust = 3, color = "red") +
  geom_point(data = df[1 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df[1 ,], label = "Nearest Neighbor", vjust = -3, color = "blue") +
  geom_point(data = df[3 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df[3 ,], label = "Nearest Neighbor", vjust = 3, color = "blue") +
  labs(x = "x1", y = "x2") +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))
  
plot2

# Compute new synthetic sample

lambda = 0.3
p5 = (1-lambda)*df[1 ,]+lambda*df[2 ,]

df1 = as.data.frame(rbind(df, p5))

# Add new point to plot

plot3 = ggplot(df1, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, stroke = 1) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  geom_segment(aes(x = 1, y = 2, xend = 3, yend = 1), color = "green") +
  geom_point(data = df1[2 ,], size = 5, shape = 3, color = "red", stroke = 1) +
  geom_text(data = df1[2 ,], label = "Selected Point", vjust = 3, color = "red", size = 4) +
  geom_point(data = df1[1 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df1[1 ,], label = "Nearest Neighbor", vjust = -3, color = "blue", size = 4) +
  geom_point(data = df1[3 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df1[3 ,], label = "Nearest Neighbor", vjust = 3, color = "blue", size = 4) +
  geom_point(data = df1[5 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_text(data = df1[5 ,], label = "Synthetic Point", vjust = 3, color = "green", size = 4) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))

plot3

# Plot new data with synthetic sample added

plot4 = ggplot(df1, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, stroke = 1) +
  geom_point(data = df1[5 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_text(data = df1[5 ,], label = "Synthetic Point", vjust = 3, color = "green", size = 4) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))

plot4

# Interpolate p1 with p3
# Procedure analogous to previous plotting

plot5 = ggplot(df1, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, stroke = 1) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  geom_segment(aes(x = 1, y = 2, xend = 1, yend = 1), color = "green") +
  geom_point(data = df1[1 ,], size = 5, shape = 3, color = "red", stroke = 1) +
  geom_text(data = df1[1 ,], label = "Selected Point", vjust = -3, color = "red", size = 4) +
  geom_point(data = df1[2 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df1[2 ,], label = "Nearest Neighbor", vjust = 3, color = "blue", size = 4) +
  geom_point(data = df1[3 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df1[3 ,], label = "Nearest Neighbor", vjust = 3, color = "blue", size = 4) +
  geom_point(data = df1[5 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))

plot5

lambda = 0.5
p6 = (1-lambda)*df[1 ,]+lambda*df[3 ,]

df2 = as.data.frame(rbind(df1, p6))

plot6 = ggplot(df2, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, stroke = 1) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  geom_segment(aes(x = 1, y = 2, xend = 1, yend = 1), color = "green") +
  geom_point(data = df2[1 ,], size = 5, shape = 3, color = "red", stroke = 1) +
  geom_text(data = df2[1 ,], label = "Selected Point", vjust = -3, color = "red", size = 4) +
  geom_point(data = df2[2 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df2[2 ,], label = "Nearest Neighbor", vjust = 3, color = "blue", size = 4) +
  geom_point(data = df2[3 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df2[3 ,], label = "Nearest Neighbor", vjust = 3, color = "blue", size = 4) +
  geom_point(data = df2[5 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_point(data = df2[6 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_text(data = df2[6 ,], label = "Synthetic Point", hjust = 1.2, color = "green", size = 4) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))

plot6

plot7 = ggplot(df2, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, stroke = 1) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  geom_point(data = df2[5 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_point(data = df2[6 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_text(data = df2[6 ,], label = "Synthetic Point", hjust = 1.2, color = "green", size = 4) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))

plot7

# Interpolate p4 with p2

plot8 = ggplot(df2, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, stroke = 1) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  geom_segment(aes(x = 4, y = 4, xend = 3, yend = 1), color = "green") +
  geom_point(data = df2[4 ,], size = 5, shape = 3, color = "red", stroke = 1) +
  geom_text(data = df2[4 ,], label = "Selected Point", vjust = -3, color = "red", size = 4) +
  geom_point(data = df2[1 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df2[1 ,], label = "Nearest Neighbor", vjust = -3, color = "blue", size = 4) +
  geom_point(data = df2[2 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df2[2 ,], label = "Nearest Neighbor", vjust = 3, color = "blue", size = 4) +
  geom_point(data = df2[5 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_point(data = df2[6 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

plot8

lambda = 0.4
p7 = (1-lambda)*df[2 ,]+lambda*df[4 ,]

df3 = as.data.frame(rbind(df2, p7))

plot9 = ggplot(df2, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, stroke = 1) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  geom_segment(aes(x = 4, y = 4, xend = 3, yend = 1), color = "green") +
  geom_point(data = df3[4 ,], size = 5, shape = 3, color = "red", stroke = 1) +
  geom_text(data = df3[4 ,], label = "Selected Point", vjust = -3, color = "red", size = 4) +
  geom_point(data = df3[1 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df3[1 ,], label = "Nearest Neighbor", vjust = -3, color = "blue", size = 4) +
  geom_point(data = df3[2 ,], size = 5, shape = 3, color = "blue", stroke = 1) +
  geom_text(data = df3[2 ,], label = "Nearest Neighbor", vjust = 3, color = "blue", size = 4) +
  geom_point(data = df3[5 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_point(data = df3[6 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_point(data = df3[7 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_text(data = df3[7 ,], label = "Synthetic Point", hjust = -0.2, color = "green", size = 4) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))

plot9

plot10 = ggplot(df2, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, stroke = 1) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  geom_point(data = df3[5 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_point(data = df3[6 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  geom_point(data = df3[7 ,], size = 5, shape = 3, color = "green", stroke = 1) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))

plot10


# Sample 100 additional points for K=2
# Sample lambda uniformly
# Randomly sample one of the for original points
# Restrict two K=2 nearest neighbors

df_k2 = df3 

for(i in 1:100) {
  
  lambda = runif(1)
  
  x1 = floor(runif(1, min=1, max=5))
  
  if(x1 == 1){
  x2 = floor(runif(1, min=2, max=4))
  }else if (x1 == 2) {
  x2 = floor(runif(1, min=1, max=4))
  }else if (x1 == 3){
    x2 = floor(runif(1, min=1, max=4))
  }else if (x1 == 4){
    x2 = floor(runif(1, min=1, max=3))
  }
  p = (1-lambda)*df[x1 ,]+lambda*df[x2 ,]
  df_k2 = as.data.frame(rbind(df_k2, p))
  
}

# Plot new data
# Color synthetic samples green and original samples black

plot11 = ggplot(df_k2, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, color = "green", stroke = 1) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  geom_point(data = df3[1 ,], size = 5, shape = 3, stroke = 1) +
  geom_point(data = df3[2 ,], size = 5, shape = 3, stroke = 1) +
  geom_point(data = df3[3 ,], size = 5, shape = 3, stroke = 1) +
  geom_point(data = df3[4 ,], size = 5, shape = 3, stroke = 1) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))

plot11

# Sample 100 additional points for K=3
# Does not require restriction of random sampling of original points

df_k3 = df3 

for(i in 1:100) {
  
  lambda = runif(1)
  x1 = floor(runif(1, min=1, max=5))
  x2 = floor(runif(1, min=1, max=5))
  
  p = (1-lambda)*df[x1 ,]+lambda*df[x2 ,]
  df_k3 = as.data.frame(rbind(df_k3, p))
  
}

plot12 = ggplot(df_k3, aes(V1, V2)) +
  geom_point(size = 5, shape = 3, color = "green", stroke = 1) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  labs(x = "x1", y = "x2") +
  geom_point(data = df3[1 ,], size = 5, shape = 3, stroke = 1) +
  geom_point(data = df3[2 ,], size = 5, shape = 3, stroke = 1) +
  geom_point(data = df3[3 ,], size = 5, shape = 3, stroke = 1) +
  geom_point(data = df3[4 ,], size = 5, shape = 3, stroke = 1) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16))


plot12

# plot_list = list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9,
#                 plot10, plot11, plot12)

