library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(mlr3pipelines)
library(OpenML)
library(dplyr)
library(knitr)

set.seed(7832)
lgr::get_logger("mlr3")$set_threshold("warn")

# get list of curated binary classification data sets (see https://arxiv.org/abs/1708.03731v2)
ds = listOMLDataSets(
  number.of.classes = 2,
  number.of.features = c(1, 100),
  number.of.instances = c(5000, 10000)
)
# select imbalanced data sets (without categorical features as SMOTE cannot handle them)
ds = subset(ds, minority.class.size / number.of.instances < 0.2 &
              number.of.symbolic.features == 1)

# create some vectors for preprocessing
vec = c(976, 980, 1021, 1056, 1069, 40900)
num = 1:6
pos = c("P", "P", "N", "TRUE", "TRUE", "Anomaly")

# create binary classification tasks for datasets 
data = lapply(vec, function(x) as.data.frame(getOMLDataSet(x)))
lapply(num, function(x) as.factor(data[[x]][[getOMLDataSet(vec[x])$target.features]]))
tasks = lapply(num, function(x) as_task_classif(data[[x]],
                                                id = getOMLDataSet(vec[x])$desc$name,
                                                target = getOMLDataSet(vec[x])$target.features,
                                                positive = pos[x]))

# check original class balances
for (i in tasks) {
  print(table(i$truth()))
}

un = c(1/6, 1/6, 1/6, 1/50, 1/80, 1/60)
# undersample majority class (relative to majority class)
po_under = lapply(un, function(x) po("classbalancing",
              id = "undersample", adjust = "major",
              reference = "major", shuffle = FALSE, ratio = x))

# reduce majority class by factor '1/ratio'
for (i in 1:length(tasks)){
print(table(po_under[[i]]$train(list(tasks[[i]]))$output$truth()))
}

ov = c(6, 6, 6, 100, 200, 60)
# oversample majority class (relative to majority class)
po_over = lapply(ov, function(x) po("classbalancing",
             id = "oversample", adjust = "minor",
             reference = "minor", shuffle = FALSE, ratio = x))

# enrich minority class by factor 'ratio'
for (i in 1:length(tasks)){
print(table(po_over[[i]]$train(list(tasks[[i]]))$output$truth()))
}

sm = c(4, 6, 6, 100, 200, 60)
# SMOTE enriches the minority class with synthetic data
gr_smote = lapply(sm, function(y)
  po("colapply", id = "int_to_num",
     applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
  po("smote", K = 5, dup_size = y) %>>%
  po("colapply", id = "num_to_int",
     applicator = function(x) as.integer(round(x, 0L)), affect_columns = selector_type("numeric")))

# enrich minority class by factor (dup_size + 1)
for (i in 1:length(tasks)){
print(table(gr_smote[[i]]$train(tasks[[i]])[[1L]]$truth()))
}

# create random forest learner
learner = lrn("classif.ranger", num.trees = 100, predict_type = "prob")
learner$id = "base_learner"

# combine learner with pipeline graph
learners_under = lapply(num, function(x) as_learner(po_under[[x]] %>>% learner))
for (i in learners_under){
i$id = "undersample.ranger"
}
learners_over = lapply(num, function(x) as_learner(po_over[[x]] %>>% learner))
for (i in learners_over){
i$id = "oversample.ranger"
}
learners_smote = lapply(num, function(x) as_learner(gr_smote[[x]] %>>% learner))
for (i in learners_smote){
i$id = "smote.ranger"
}

inner_cv = rsmp("cv", folds = 5)

mm = msrs(c("classif.acc", "classif.tpr", "classif.ppv", "classif.fbeta", "classif.auc"),
          average = "micro")

learners = list(learner, learners_under, learners_over, learners_smote)

# benchmark learners for different sampling configurations (be warned of runtime)
aggr = lapply(learners, function(x) benchmark(benchmark_grid(tasks = tasks,
                                                      x,
                                                      resamplings = inner_cv))$aggregate(measures = mm))

for (i in aggr){
  print(i)
}

# notice that simulation results are the nicest for the optdigits dataset
# so rerun experiment only for this task with specified learners and sampling settings
learners_opt = list(learner, learners_under[[2]], learners_over[[2]], learners_smote[[2]])

aggr_opt = benchmark(benchmark_grid(tasks = tasks[[2]],
                         learners_opt,
                         resamplings = inner_cv))$aggregate(measures = mm)
aggr_opt

out = as.data.frame(aggr_opt)

out = out[, c("learner_id", "classif.fbeta")]

colnames(out)[1] = "Learner"
colnames(out)[2] = "F1-Score"

out = out %>%
  mutate(Learner = recode(Learner, base_learner = "Base Ranger",
                          undersample.ranger = "Undersample Ranger",
                          oversample.ranger = "Oversample Ranger",
                          smote.ranger = "SMOTE Ranger"))

kable(out, "latex", booktabs = TRUE)
