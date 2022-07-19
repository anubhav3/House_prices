## 09.11.2021

#### Loading packages ####
library(caret)
library(ggplot2)

#### Loading the data ####
main_train_data <- read.csv("~/others/Courses/kaggle/House_prices/data/train.csv")
main_train_data$SalePrice <- log10(main_train_data$SalePrice)
main_test_data <- read.csv("~/others/Courses/kaggle/House_prices/data/test.csv")


#### Paritioning the main_train_data ####
set.seed(1)
train_index <- createDataPartition(main_train_data$SalePrice, p = 0.7, list = FALSE)
training <- main_train_data[train_index,]
testing <- main_train_data[-train_index,]

#### Let's remove variables which has NA

var_NA <- which(colSums(is.na(training)) >= 1)
var_NA_test <- which(colSums(is.na(main_test_data)) >= 1)
var_NA <- union(var_NA, var_NA_test)
training <- training[,-var_NA]
testing <- testing[,-var_NA]
main_test_data <- main_test_data[,-var_NA]

# Finding near zero var and then removing them
zerovar <- nearZeroVar(training)
training <- training[, -zerovar]
testing <- testing[, -zerovar]
main_test_data <- main_test_data[, -zerovar]


#### Doing PCA ####

## Separating numeric and character variables

var_type <- sapply(training, class)
num_var <- which(var_type == "integer" | var_type == "numeric")
char_var <- which(var_type == "character")

## Let us only use numeric variables
training <- training[, num_var]
testing <- testing[, num_var]
num_var_test <- num_var[-20]
main_test_data <- main_test_data[, num_var_test]

## PCA
N <- 20
pca <- prcomp(training[-N])
n <- 19
prop <- sum(pca$sdev[1:n])/sum(pca$sdev)

preProc <- preProcess(training[-N], method = "pca", pcaComp = n)
trainPC <- predict(preProc, training[-N])
trainPC <- data.frame(trainPC, training$SalePrice)

modelFit <- train(training.SalePrice ~ ., method = "glm", data = trainPC)

testPC <- predict(preProc, testing[-N])
pred_test <- predict(modelFit, testPC)

main_test_dataPC <- predict(preProc, main_test_data)
main_pred_test <- predict(modelFit, main_test_dataPC)

df <- data.frame(obs = testing$SalePrice, pred = pred_test)

ggplot(df) +
  geom_point(aes(x = obs, y = pred)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic()


##### Calculating RMSE ####
err_rmse <- sqrt(sum(df$obs - df$pred)^2)


##### Writing the predictions from the main_test_data #####
dd_main_test <- data.frame(Id = main_test_data$Id, SalePrice = 10^main_pred_test)

write.csv(x = dd_main_test, file = "result.csv", row.names = FALSE)

