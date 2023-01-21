library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(mlr3pipelines)

df = read.csv("/Users/toby/Downloads/adult.csv")

df = df[c("age", "fnlwgt", "educational.num", "capital.gain", "capital.loss", "hours.per.week", "income")]

df$income = as.factor(df$income)

# prepocessed data

task = TaskClassif$new("adult", df, target = "income", positive = ">50K")

table(task$truth())


# undersample majority class (relative to majority class)
po_under = po("classbalancing",
              id = "undersample", adjust = "major",
              reference = "major", shuffle = FALSE, ratio = 1 / 4)
# reduce majority class by factor '1/ratio'
table(po_under$train(list(task))$output$truth())

# oversample majority class (relative to majority class)
po_over = po("classbalancing",
             id = "oversample", adjust = "minor",
             reference = "minor", shuffle = FALSE, ratio = 3)
# enrich minority class by factor 'ratio'
table(po_over$train(list(task))$output$truth())


# SMOTE enriches the minority class with synthetic data
 gr_smote =
  po("colapply", id = "int_to_num",
     applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
  po("smote", K = 20, dup_size = 2)

table(gr_smote$train(task)[[1L]]$truth())

# create random forest learner
learner = lrn("classif.ranger", alpha = 0.5, num.trees = 50, predict_type = "prob")
learner$id = "base_learner"

# combine learner with pipeline graph
learner_under = as_learner(po_under %>>% learner)
learner_under$id = "undersample.ranger"
learner_over = as_learner(po_over %>>% learner)
learner_over$id = "oversample.ranger"
learner_smote = as_learner(gr_smote %>>% learner)
learner_smote$id = "smote.ranger"


inner_cv = rsmp("cv", folds = 10)
measure = msr("classif.fbeta")

mm = msrs(c("classif.acc", "classif.tpr", "classif.ppv", "classif.fbeta", "classif.auc"),
          average = "macro")

learners = list(learner, learner_under, learner_over, learner_smote)

design = benchmark_grid(tasks = task, learners, resamplings = inner_cv)
bmr = benchmark(design)

aggr = bmr$aggregate(measures = mm)
aggr