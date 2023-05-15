library(ggplot2)
library(mvtnorm)
library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(dplyr)
library(gridExtra)

# Define function that samples positive and negative instances from two Gaussian
# distributions, creates classification task and sets up stratified cv.

set.seed(123456)

create_task = function(n_pos, n_neg, p) {
  mu_pos = rep(-1, p)
  mu_neg = rep(1, p)
  d_pos = as.data.frame(rmvnorm(n_pos, mean = mu_pos, sigma = diag(p)))
  d_neg = as.data.frame(rmvnorm(n_neg, mean = mu_neg, sigma = diag(p)))
  d_pos$y = as.factor(rep(1, n_pos))
  d_neg$y = as.factor(rep(-1, n_neg))
  d = rbind(d_pos, d_neg)
  tid = sprintf("t_%04i_%04i_%02i", n_pos, n_neg, p)
  tt = TaskClassif$new(id = tid, backend = d, target = "y", positive = "1")
  tt$col_roles$stratum = tt$target_names
  return(tt)
}

# Create tasks

ns_pos = c(10000, 1000, 100, 50)
tasks = lapply(ns_pos, function(n) create_task(n_neg = 10000, n_pos = n, p = 2))

learners = lrns(c("classif.log_reg", "classif.svm", "classif.rpart"))
resa = rsmp("cv", folds = 10L)

design = benchmark_grid(tasks, learners, resa)
bmr = benchmark(design)
 
mm = msrs(c("classif.acc", "classif.tpr", "classif.ppv", "classif.fbeta"),
               average = "micro")

aggr = bmr$aggregate(measures = mm)

# Rename columns and values for ggplots

table = aggr %>%
  rename("Task" = "task_id", "Learner" = "learner_id", "Accuracy" = "classif.acc",
         "TPR" = "classif.tpr", "PPV" = "classif.ppv", "F1_Score" = "classif.fbeta")

table$Learner[table$Learner == "classif.log_reg"] = "Logistic Regression"
table$Learner[table$Learner == "classif.rpart"] = "Classification Tree"
table$Learner[table$Learner == "classif.svm"] = "SVM"

table$Task[table$Task == "t_10000_10000_02"] = "10000/10000"
table$Task[table$Task == "t_1000_10000_02"] = "1000/10000"
table$Task[table$Task == "t_0100_10000_02"] = "100/10000"
table$Task[table$Task == "t_0050_10000_02"] = "50/10000"

x_labels = c("10000/10000", "1000/10000", "100/10000", "50/10000")

plot_acc = ggplot(table, aes(x=factor(Task, level = x_labels), y=Accuracy, group = Learner)) +
  geom_line(aes(color=Learner)) +
  xlab("Positive/Negative Ratio") +
  geom_point() +
  theme(text = element_text(size = 16))

plot_tpr = ggplot(table, aes(x=factor(Task, level = x_labels), y=TPR, group = Learner)) +
  geom_line(aes(color=Learner)) +
  geom_point() +
  xlab("Positive/Negative Ratio") +
  ylab("TPR") +
  theme(text = element_text(size = 16))

plot_ppv = ggplot(table, aes(x=factor(Task, level = x_labels), y=PPV, group = Learner)) +
  geom_line(aes(color=Learner)) +
  geom_point() +
  xlab("Positive/Negative Ratio") +
  ylab("PPV") +
  theme(text = element_text(size = 16))

plot_f1 = ggplot(table, aes(x=factor(Task, level = x_labels), y=F1_Score, group = Learner)) +
  geom_line(aes(color=Learner)) +
  geom_point() +
  xlab("Positive/Negative Ratio") +
  ylab("F1 Score") +
  theme(text = element_text(size = 16))


# Combine plots

p = grid.arrange(plot_acc, plot_tpr, plot_ppv, plot_f1, ncol = 2, nrow = 2)

ggsave(paste0(".../figure_man/benchmark_plots.pdf"), p, width = 15, height = 10)



