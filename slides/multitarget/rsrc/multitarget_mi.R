library(infotheo)
library(knitr)
library(farff)

df = readARFF("slides/multitarget/rsrc/emotions.arff")

df = df[75:78]
num = 1:4
vec = c()
matrix = as.data.frame(1:4)

for (j in num) {
  for (i in num) {
    minf = mutinformation(df[j], df[i]) / (sqrt(entropy(df[j]) * entropy(df[i])))
    vec[i] = minf
  }
  matrix = cbind(matrix, vec)
}

matrix = matrix[2:5]

names(matrix)[1] = "Calm"
names(matrix)[2] = "Quiet"
names(matrix)[3] = "Sad"
names(matrix)[4] = "Angry"

row.names(matrix) = c("Calm", "Quiet", "Sad", "Angry")

kable(matrix, "latex", booktabs = TRUE)
