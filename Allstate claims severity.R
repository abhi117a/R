# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.



library(h2o)
library(data.table)
localH2O = h2o.init(max_mem_size = '8g')


ID = 'id'
TARGET = 'loss'
SEED = 0

TRAIN_FILE = "../input/train.csv"
TEST_FILE = "../input/test.csv"
SUBMISSION_FILE = "../output/train_results.csv"


train = fread(TRAIN_FILE, showProgress = TRUE)
test = fread(TEST_FILE, showProgress = TRUE)

y_train = log(train[,TARGET, with = FALSE])[[TARGET]]

train[, c( TARGET) := NULL]
ntrain = nrow(train)
train_test = rbind(train, test)
head(train_test)
features = names(train)
for (f in features) {
  if (class(train_test[[f]])=="character") {
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
  }
}

head(train_test)
x_train = train_test[1:ntrain,]
x_test = train_test[(ntrain+1):nrow(train_test),]
x_train$loss <- y_train
head(x_train)

print("loading data")
train_xf <- as.h2o(x=x_train, destination_frame = "train.hex")
test_xf <- as.h2o(x=x_test, destination_frame = "test.hex")
print("loading data - DONE")
rownames(train_xf)
splits <- h2o.splitFrame(
  data = train_xf, 
  ratios = c(0.8),
  destination_frames = c("train_1.hex", "valid_1.hex")
)

train_xf_sp <- splits[[1]]
valid_xf_sp <- splits[[2]]

submission <- test_xf[, 1]
test_xf <- test_xf[, -1]

features <- colnames(train_xf_sp)[3:132]
tail(features)
head(features)
label <- "loss"

hyper_params = list( max_depth = seq(1,29,2) )
print("train gbm")
gbm_model_1 <- h2o.gbm(y = label,x=features, 
                       training_frame = train_xf_sp, 
                       ntrees=10,max_depth = 5,
                       distribution = "gaussian",
                       learn_rate = 0.1, 
                       sample_rate = 1,
                       nbins = 20, nbins_top_level = 1024, nbins_cats = 1024,
                       validation_frame = NULL, balance_classes = FALSE,
                       max_after_balance_size = 5, seed=1123, build_tree_one_node = FALSE,
                       nfolds = 5,stopping_rounds = 10, stopping_metric ="MSE")

summary(gbm_model_1@parameters)