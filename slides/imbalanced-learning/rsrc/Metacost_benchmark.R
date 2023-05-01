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

names(df1)[names(df1) == "pred_df$relabel"] <- "relabled_class"

df1 = df1 %>% mutate(cost_min_class = case_when(class == "good" &
                                               relabled_class == "good" ~ "good",
                                                 class == "bad" &
                                               relabled_class == "bad" ~ "bad",
                                               class == "good" &
                                                 relabled_class == "bad" ~ "bad_relabel",
                                               class == "bad" &
                                                 relabled_class == "good" ~ "good_relabel"))

df1 = df1[1:50,]

p <- ggplot(df1, aes(duration, credit_amount, group = cost_min_class)) + 
  geom_point(aes(shape = cost_min_class, color = cost_min_class), size = 7) +
  scale_shape_manual(values=c(2, 2, 3, 3)) +
  scale_color_manual(values=c('black','red', 'black', 'blue')) +
  xlab("Duration") +
  ylab("Credit Amount") +
  labs(color = "Credit Class", shape = "Credit Class") +
  ggtitle("After Relabeling") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 20)) 
