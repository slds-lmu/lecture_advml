library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(mlr3pipelines)
library(ggplot2)
library(gridExtra)

set.seed(4829)

# get a cost sensitive task
task = tsk("german_credit")

# cost matrix as given on the UCI page of the german credit data set
# https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)
costs = matrix(c(0, 3, 1, 0), nrow = 2)
dimnames(costs) = list(truth = task$class_names, predicted = task$class_names)
print(costs)

# mlr3 needs truth in columns, predictions in rows
costs = t(costs)

# create a cost measure which calculates the absolute costs
m = msr("classif.costs", id = "german_credit_costs", costs = costs, normalize = FALSE)

num = seq(0, 1, by = 0.01)
df = as.data.frame(num)
df$cost = NA
learners = c("classif.log_reg", "classif.rpart", "classif.ranger")
out = list()

for (j in 1:length(learners)) {
  for (i in 1:length(num)) {
    th = list(thresholds = num[i])
    # fit models and evaluate with the cost measure
    learner = as_learner(lrn(learners[j], predict_type = "prob") %>>%
                           po("threshold", param_vals = th))
    
    rr = resample(task, learner, rsmp("cv", folds = 3))
    res = rr$aggregate(m)
    df$cost[i] = res
  }
  out[[j]] = df
}
# Find ideal thresholds with minimal cost

emp_min = lapply(out, function(x) x[x$cost == min(x$cost) ,])
sapply(emp_min, function(x) print(x))

theo_min = lapply(out, function(x) x[x$num == 0.75 ,])
sapply(theo_min, function(x) print(x))

# create plots

plot_log = ggplot(data=out[[1]], aes(x=num, y=cost, group=1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(x = "threshold") +
  ggtitle("Logistic Regression") +
  geom_point(data = emp_min[[1]], color = "blue") +
  geom_text(x=0.75, y=275, label="Empirical min (0.81, 145)", color = "blue", size = 4) +
  geom_point(data = theo_min[[1]], color = "orange") +
  geom_text(x=0.75, y=250, label="Theoretical min (0.75, 163.33)", color = "orange", size = 4) +
  theme(plot.title = element_text(hjust = 0.5))

plot_rpart = ggplot(data=out[[2]], aes(x=num, y=cost, group=1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(x = "threshold") +
  ggtitle("Classification Tree") +
  geom_point(data = emp_min[[2]], color = "blue") +
  geom_text(x=0.75, y=275, label="Empirical min (0.84, 166)", color = "blue", size = 4) +
  geom_point(data = theo_min[[2]], color = "orange") +
  geom_text(x=0.75, y=250, label="Theoretical min (0.75, 179)", color = "orange", size = 4) +
  theme(plot.title = element_text(hjust = 0.5))

plot_ranger = ggplot(data=out[[3]], aes(x=num, y=cost, group=1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(x = "threshold") +
  ggtitle("Random Forest") +
  geom_point(data = emp_min[[3]], color = "blue") +
  geom_text(x=0.75, y=275, label="Empirical min (0.7, 143.33)", color = "blue", size = 4) +
  geom_point(data = theo_min[[3]], color = "orange") +
  geom_text(x=0.75, y=250, label="Theoretical min (0.75, 148.33)", color = "orange", size = 4) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot_log, plot_rpart, plot_ranger, ncol = 3, nrow = 1)


