library(batchtools)
library(dplyr)
library(mlr)
library(knitr)

#save benchmark results
reg = loadRegistry("/Users/toby/Downloads/benchmark/multilabel_experiments", work.dir = "./")
res = reduceResultsList(ids = findDone(), 
  fun = function(job, res) res)
bench = mergeBenchmarkResults(res)
save(bench, file = "bench_results.RData")

BR_rf = sapply(1:10, function(x) bench[[1]][[x]][[1]][[6]][2])

STA_rf = sapply(1:10, function(x) bench[[1]][[x]][[7]][[6]][2])

Dataset = c("birds", "emotions", "enron", "genbase", "image", "langLog", "reuters",
          "scene", "slashdot", "yeast")

df = as.data.frame(rbind(BR_rf, STA_rf))

colnames(df) = Dataset

rownames(df) = c("BR(rf) F1-Score", "STA(rf) F1-Score")

round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

df = round_df(df, 3)

kable(df, "latex", booktabs = TRUE)
