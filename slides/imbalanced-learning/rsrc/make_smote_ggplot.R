library(UBL)
library(ggplot2)
library(ggpubr)

## data
data(iris)
dat <- iris[, c(1, 2, 5)]
dat$Species <- factor(ifelse(dat$Species == "setosa", "rare", "common"))
## checking the class distribution of this artificial data set
table(dat$Species)


## now using SMOTE with k=5 to create a more "balanced problem"
newData <- SmoteClassif(Species ~ ., dat, C.perc = list(common = 1,rare = 6),k=5)
table(newData$Species)

## now using SMOTE with k=3 to create a more "balanced problem"
newData_2 <- SmoteClassif(Species ~ ., dat, C.perc = list(common = 1,rare = 6),k=1)
table(newData_2$Species)

plot1 = ggplot(dat, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point() +
  ggtitle("Original iris Data") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

plot2 = ggplot(newData, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point() +
  ggtitle("SMOTE'd iris Data (k=5)") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

plot3 = ggplot(newData_2, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point() +
  ggtitle("SMOTE'd iris Data (k=1)") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

p = ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)

ggsave(paste0("/Users/toby/Downloads/smoted_iris_data_ggplot.pdf"), p, width = 7.5, height = 3)


