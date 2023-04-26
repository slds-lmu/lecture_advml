library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(ggplot2)
library(gridExtra)
library(OpenML)
library(dplyr)

# get a cost sensitive task
task = tsk("german_credit")

d = getOMLDataSet(31)
df = d[[2]]

p <- ggplot(df, aes(duration, credit_amount)) + 
  geom_point(size = 3) +
  geom_point(aes(colour = factor(class))) +
  xlab("Duration") +
  ylab("Credit Amount") +
  labs(color = "Credit Class") +
  scale_color_manual(breaks = c("good", "bad"),
                     values=c("blue", "red")) +
  ggtitle("Before Relabeling") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 20))   

# cost matrix as given on the UCI page of the german credit data set
# https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)
costs = matrix(c(0, 5, 1, 0), nrow = 2)
dimnames(costs) = list(truth = task$class_names, predicted = task$class_names)
print(costs)

# mlr3 needs truth in columns, predictions in rows
costs = t(costs)

# create a cost measure which calculates the absolute costs
m = msr("classif.costs", id = "german_credit_costs", costs = costs, normalize = FALSE)

learner = lrn("classif.ranger", predict_type = "prob")
resampling = rsmp("cv", folds = 3)

set.seed(4890)
resampling$instantiate(task)

rr = resample(task, learner, resampling)

pred = rr$prediction()

pred_df = as.data.frame(as.data.table(pred))

pred_df = pred_df[order(pred_df$row_ids, decreasing = FALSE),]

min_cost = sapply(1:1000, function (x) min(pred_df$prob.good[x]*costs[1, 1] + (1-pred_df$prob.good[x])*costs[1, 2], 
pred_df$prob.good[x]*costs[2, 1] + (1-pred_df$prob.good[x])*costs[2, 2]))

pred_df = cbind(pred_df, min_cost)

pred_df = pred_df %>% mutate(relabel = case_when(min_cost >= 0.5 ~ "good",
                                       min_cost < 0.5 ~"bad"))

df1 = cbind(df, pred_df$relabel)

p1 <- ggplot(df1, aes(duration, credit_amount)) + 
  geom_point(size = 3) +
  geom_point(aes(colour = factor(pred_df$relabel))) +
  xlab("Duration") +
  ylab("Credit Amount") +
  labs(color = "Credit Class") + 
  scale_color_manual(breaks = c("good", "bad"),
                     values=c("blue", "red")) +
  ggtitle("After Relabeling") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 20)) 

grid.arrange(p, p1, ncol = 2, nrow = 1)

