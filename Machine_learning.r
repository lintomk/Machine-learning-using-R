install.packages("class")
install.packages("sjPlot")
install.packages('TMB', type = 'source')
install.packages("Metrics")
install.packages("ggvis")
install.packages("ggplot")
library(tidyr)
library(ggplot2)
library(ggvis)
library(class)
library(sjPlot)
library(Metrics)
clevend_df <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", na.strings = c("?"), sep = ',', header = FALSE)
clevend_df$V14[clevend_df$V14<3] <- 0  
clevend_df$V14[clevend_df$V14>0] <- 1
names(clevend_df) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "rectecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num");
full_df <- clevend_df
complete_df <- full_df[complete.cases(full_df), ]
Normalised_df <- function(x) {((x-min(x))/(max(x)-min(x)))}
final_df <- as.data.frame(lapply(complete_df,Normalised_df))
sjc.elbow(final_df) 
smp_size <- floor (0.70 * nrow(final_df))
set.seed(123)
train_ind <- sample(seq_len(nrow(final_df)), size = smp_size)
training_df <- final_df[train_ind, ]
testing_df <- final_df[-train_ind, ]
predict_df <- knn(training_df, testing_df, final_df[train_ind,14], k=9)
accuracy(final_df[-train_ind,14], predict_df)
install.packages("ggvis")
library(ggvis)
complete_df %>% ggvis(~exang, ~age, fill = ~num) %>% layer_points()
install.packages("ggplot")
library(tidyr)
library(ggplot2)
complete_df %>% gather() %>% head()
ggplot(gather(complete_df), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
