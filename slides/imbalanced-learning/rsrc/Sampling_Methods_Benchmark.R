library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(mlr3pipelines)
library(mlr3tuning)
library(OpenML)
library(data.table)

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
ds

# pick one data set from list above
d = getOMLDataSet(980)
d

# make sure target is a factor and create mlr3 tasks
data = as.data.frame(d)
data[[d$target.features]] = as.factor(data[[d$target.features]])
task = as_task_classif(data, id = d$desc$name, target = d$target.features)
task

# prepocessed data
data("optdigits", package = "mlr3data")
task = as_task_classif(optdigits, target = "binaryclass", positive = "P")

skimr::skim(task$data())

# check original class balance
table(task$truth())

# undersample majority class (relative to majority class)
po_under = po("classbalancing",
              id = "undersample", adjust = "major",
              reference = "major", shuffle = FALSE, ratio = 1 / 6)
# reduce majority class by factor '1/ratio'
table(po_under$train(list(task))$output$truth())

# oversample majority class (relative to majority class)
po_over = po("classbalancing",
             id = "oversample", adjust = "minor",
             reference = "minor", shuffle = FALSE, ratio = 6)
# enrich minority class by factor 'ratio'
table(po_over$train(list(task))$output$truth())


# SMOTE enriches the minority class with synthetic data
gr_smote =
  po("colapply", id = "int_to_num",
     applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
  po("smote", dup_size = 6) %>>%
  po("colapply", id = "num_to_int",
     applicator = function(x) as.integer(round(x, 0L)), affect_columns = selector_type("numeric"))
# enrich minority class by factor (dup_size + 1)
table(gr_smote$train(task)[[1L]]$truth())

# create random forest learner
learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob")
learner$id = "base_learner"

# combine learner with pipeline graph
learner_under = as_learner(po_under %>>% learner)
learner_under$id = "undersample.ranger"
learner_over = as_learner(po_over %>>% learner)
learner_over$id = "oversample.ranger"
learner_smote = as_learner(gr_smote %>>% learner)
learner_smote$id = "smote.ranger"


inner_cv = rsmp("cv", folds = 5)
measure = msr("classif.fbeta")

mm = msrs(c("classif.acc", "classif.tpr", "classif.ppv", "classif.fbeta", "classif.auc"),
          average = "micro")

learners = list(learner, learner_under, learner_over, learner_smote)

design = benchmark_grid(tasks = task, learners, resamplings = inner_cv)
bmr = benchmark(design)

aggr = bmr$aggregate(measures = mm)
aggr

