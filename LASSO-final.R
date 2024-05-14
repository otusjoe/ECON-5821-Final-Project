#导入需要的包
install.packages("glmnet")
library(glmnet)

#读取数据
data <- read.csv("/Users/lianlian/Desktop/US_PCE_training.csv")
head(data)

#计算通胀率
pce_names <- data[c(1, 1), 1]
pce_values <- data[c(1), -1]
inflationrate_mat <- as.matrix(pce_values)
for (i in 1:732) { 
  inflationrate_mat[i] <- (log(pce_values[, i + 1]) - log(pce_values[, i]))*12
}
head(inflationrate_mat)  # 打印计算出的通胀率


# 提取自变量名称
predictors_names <- data[c(5:208), 1]
# 提取自变量数据
predictors_values <- data[c(5:208), -1]
# 将数据转换为矩阵
predictors_mat <- as.matrix(predictors_values)
# transform the raw x variables into log difference
for (i in 1:732) { 
  predictors_mat[i] <- (log(pce_values[, i + 1]) - log(pce_values[, i]))*12
}
head(predictors_mat)  # 打印计算出的log difference（for x）


#赋值
x<-predictors_mat
y<-inflationrate_mat


#检查是否有缺失值
#使用均值填充缺失值
is_missing <- is.na(x)
mean_val <- mean(x, na.rm = TRUE)
x[is.na(x)] <- mean_val
is_missing <- is.na(y)
mean_val <- mean(y, na.rm = TRUE)
y[is.na(y)] <- mean_val

#对x和y进行转置
X <- t(x)
Y <- t(y)

#构建模型进行LASSO
lasso_model <- glmnet(X, Y, alpha = 1)

#可视化f1
plot(lasso_model, xvar="lambda", label=TRUE)
print(lasso_model)

#进行交叉验证
cvfit=cv.glmnet(X,Y)
plot(cvfit)



