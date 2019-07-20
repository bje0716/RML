# Model Capacity 그래프 ####

DATA <- read.csv("mlData/testData.csv")

str(DATA)
plot(DATA, cex = 0.5)




# lm() 1차함수

Model_1 <- lm(outputs ~ inputs, DATA)
structure(Model_1)

plot(DATA, pch = 1, cex = 0.5)
abline(Model_1, lwd = 3, col = "red")



# lm() 2차함수

Model_2 <- lm(outputs ~ poly(inputs, 2), DATA)
structure(Model_2)

a <- seq(0, 1, length = 1000)
points(a, predict(Model_2, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "blue")


# lm() 5차함수

Model_5 <- lm(outputs ~ poly(inputs, 5), DATA)
structure(Model_5)

points(a, predict(Model_5, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "green")



# lm() 9차함수

Model_9 <- lm(outputs ~ poly(inputs, 9), DATA)
structure(Model_9)

points(a, predict(Model_9, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "yellow")



# 비교 그래프 작성

par(mfrow = c(2,2))

plot(DATA, pch = 1, cex = 0.5)
abline(Model_1, lwd = 3, col = "red")

plot(DATA, pch = 1, cex = 0.5)
a <- seq(0, 1, length = 1000)
points(a, predict(Model_2, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "blue")

plot(DATA, pch = 1, cex = 0.5)
points(a, predict(Model_5, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "green")

plot(DATA, pch = 1, cex = 0.5)
points(a, predict(Model_9, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "yellow")

par(mfrow = c(1,1))



















# Training Error ####

Elec <- read.csv("mlData/Electric.csv")
str(Elec)

plot(Elec$electricity ~ Elec$surface_area, cex = 1.5)




# 선형모델(1차함수)

Model_1 <- lm(electricity ~ surface_area, Elec)
structure(Model_1)



# 선형모델(1차함수) Capacity 부족

abline(Model_1, col = "red", lwd = 3)





# 실제 전기사용량(y)

y <- Elec$electricity
y[1:50]



# 선형모델(1차함수) 적용(예측값)

y_hat_1 <- predict(Model_1, Elec)
y_hat_1[1:50]




# Training Error - 선형모델(1차함수) 
# mean((실제값 - 예측값)^2)
# 모든 오차제곱의 평균구하기

Train_Err_1 <- mean((y - y_hat_1)^2)
Train_Err_1














# 비선형모델(2차함수)

Model_2 <- lm(electricity ~ poly(surface_area, 2), Elec)
structure(Model_2)






# 비선형모델(2차함수) 적용(예측값)

y_hat_2 <- predict(Model_2, Elec)
y_hat_2[1:50]






# Training Error - 비선형모델(2차함수) 

Train_Err_2 <- mean((y - y_hat_2)^2)



# 선형모델(1차함수) vs. 비선형모델(2차함수)

Train_Err_1 ; Train_Err_2











# 비선형모델(5차함수)

Model_5 <- lm(electricity ~ poly(surface_area, 5), Elec)
structure(Model_5)



# 비선형모델(5차함수) 적용(예측값)

y_hat_5 <- predict(Model_5, Elec)
y_hat_5[1:50]



# Training Error - 비선형모델(5차함수)

Train_Err_5 <- mean((y - y_hat_5)^2)



# 1차함수 vs. 2차함수 vs. 5차함수

Train_Err_1 ; Train_Err_2 ; Train_Err_5







# 비선형모델(9차함수)

Model_9 <- lm(electricity ~ poly(surface_area, 9), Elec)
structure(Model_9)



# 비선형모델(9차함수) 적용(예측값)

y_hat_9 <- predict(Model_9, Elec)
y_hat_9[1:50]



# Training Error - 비선형모델(9차함수)

Train_Err_9 <- mean((y - y_hat_9)^2)



# 1차함수 vs. 2차함수 vs. 5차함수 vs. 9차함수

Train_Err_1 ; Train_Err_2 ; Train_Err_5 ; Train_Err_9







# 그래프 출력

plot(Elec$surface_area, Elec$electricity, cex = 2)



# 선형모델(1차함수)

abline(Model_1, lwd = 5, col = "red")



# 비선형모델(2차함수)

a <- seq(500, 820, length = 1000)
points(a, predict(Model_2, data.frame(surface_area = a)),
       type = "l", lwd = 5, col = "blue")



# 비선형모델(5차함수)

points(a, predict(Model_5, data.frame(surface_area = a)), 
       type = "l", lwd = 5, col = "green")



# 비선형모델(9차함수)

points(a, predict(Model_9, data.frame(surface_area = a)), 
       type = "l", lwd = 5, col = "yellow")



















# Test Error ####
# Training Data와 Testing Data를 7:3으로 분리
# str(Elec) vs. str(Elec[trainIndex,]) vs. str(Elec[-trainIndex,])

Elec <- read.csv("mlData/Electric.csv")





# 매번 동일한 값을 추출하기 위해 설정

set.seed(2045)



# Training Data(70%) : Index

train_Index <- sample(1:nrow(Elec), nrow(Elec) * 0.7)



# Testing Data(30%) : 실제값(y)

y_test <- Elec$electricity[-train_Index]





# 선형모델(1차함수)
# Training Data로 Model_1 생성

Model_1 <- lm(electricity ~ surface_area, Elec[train_Index,])



# Testing Data에 선형모델(1차함수) 적용(예측값)

y_hat_1 <- predict(Model_1, Elec[-train_Index,])



# Testing Error - 선형모델(1차함수)

Test_Err_1 <- mean((y_test - y_hat_1)^2)
Test_Err_1



# 비선형모델(5차함수)
# Training Data로 Model_5 생성

Model_5 <- lm(electricity ~ poly(surface_area, 5), Elec[train_Index,])



# Testing Data에 비선형모델(5차함수) 적용(예측값)

y_hat_5 <- predict(Model_5, Elec[-train_Index,])



# Testing Error - 비선형모델(5차함수)

Test_Err_5 <- mean((y_test - y_hat_5)^2)
Test_Err_5



# 비선형모델(9차함수)
# Training Data로 Model_9 생성

Model_9 <- lm(electricity ~ poly(surface_area, 9), Elec[train_Index,])



# Testing Data에 비선형모델(9차함수) 적용(예측값)

y_hat_9 <- predict(Model_9, Elec[-train_Index,])



# Testing Error - 비선형모델(9차함수)

Test_Err_9 <- mean((y_test - y_hat_9)^2)
Test_Err_9



# Model_1 vs. Model_5 vs. Model_9

Test_Err_1 ; Test_Err_5 ; Test_Err_9

# Test Error 값이 가장 작은 모델을 선택
# 하지만 Testing Data도 평가과정에서 오염 발생
# 따라서 Testing Data를 Validation Data와 Testing Data로 분리하여 사용













# Validation Error ####
# Training Data : Validation Data : Testing Data

CARS <- read.csv("mlData/Cars.csv")
plot(CARS$mpg ~ CARS$weight, cex = 1.5)



# Training Data : Validation Data : Testing Data(6:2:2) 데이터 분할

# Training Data(60%) : Index

set.seed(2045)
train_IDX <- sample(1:nrow(CARS), nrow(CARS) * 0.6)



# Validation Data(20%) : Index

set.seed(2045)
valid_IDX <- sample(setdiff(1:nrow(CARS), train_IDX), nrow(CARS) * 0.2)



# Testing Data(20%) : Index

test_IDX <- setdiff(1:nrow(CARS), c(train_IDX, valid_IDX))



# 선형모델(1차함수)

Model_1 <- lm(mpg ~ weight, CARS[train_IDX,])
Pred_W_1 <- predict(Model_1, CARS[valid_IDX,])
Valid_Err_1 <- mean((CARS$mpg[valid_IDX] - Pred_W_1)^2)
Valid_Err_1




# 그래프

plot(CARS$mpg ~ CARS$weight, cex = 1.5)
abline(Model_1, lwd = 5, col = "red")






# 비선형모델(9차함수)

Model_2 <- lm(mpg ~ poly(weight, 9), CARS[train_IDX,])
Pred_W_2 <- predict(Model_2, CARS[valid_IDX,])
Valid_Err_2 <- mean((CARS$mpg[valid_IDX] - Pred_W_2)^2)
Valid_Err_2




# 그래프

a <- seq(1800, 5000, length = 10000)
points(a, predict(Model_2, data.frame(weight = a)), 
       type = "l", lwd = 5, col = "blue")





# 선형모델(다중회귀) : horsepower(마력) 추가

Model_3 <- lm(mpg ~ weight + horsepower, CARS[train_IDX,])
Pred_W_3 <- predict(Model_3, CARS[valid_IDX,])
Valid_Err_3 <- mean((CARS$mpg[valid_IDX] - Pred_W_3)^2)
Valid_Err_3




# 다중회귀분석 시각화
# install.packages("rgl")

library(rgl)

plot3d(mpg ~ weight + horsepower, CARS[valid_IDX,])




# 평가결과 비교 및 Testing Data 적용모델 선택
# 단일회귀 vs. 9차 함수 vs. 다중회귀

Valid_Err_1 ; Valid_Err_2 ; Valid_Err_3






# Testing Data 적용
# Model_2를 적용하여 최종 Generalization Error 추정

Test_Pred <- predict(Model_2, CARS[test_IDX,])
Test_Err <- mean((CARS$mpg[test_IDX] - Test_Pred)^2)


# Valid_Err vs. Test_Err

Valid_Err_2 ; Test_Err










# 개별실습자료 ####

Wage <- read.csv("mlData/Wage.csv")

str(Wage)

plot(Wage$wage ~ Wage$age)













# CV with Packages ####
# 01 - K-Fold Cross-Validaton
# 02 - LOOCV(Leave-One-Out Cross-Validation)

CARS <- read.csv("mlData/Cars.csv")
str(CARS)
plot(CARS$mpg ~ CARS$weight, cex = 1.5)



# Train : Test (7:3)

set.seed(2045)
TR <- sample(1:nrow(CARS), nrow(CARS) * 0.7)
TRD <- CARS[TR,]
TED <- CARS[-TR,]
nrow(TRD) ; nrow(TED)



# glm() : Generalized Linear Models
# 선형1차 모델

Model_CV <- glm(mpg ~ horsepower, data = TRD)
Model_CV



# cv.glm() 적용을 위하여 호출

library(boot)



# 01 - K-Fold Cross-Validaton

# glm()으로 생성한 모델에 
# cv.glm()로 Cross Validation을 적용
# K = 5(5-Fold) / K 대문자!!!

CV_ERR <- cv.glm(TRD, Model_CV, K = 5)



# Cross-Validaton 결과 확인
# 1st : prediction Error
# 2nd : adjusted Cross-Validation estimate(bias)

CV_ERR$delta



# 1차~5차 모델 생성 후 결과 비교

Degree <- 1:5



# 5-Fold Cross-Validation(K = 5)

CV_ERR_5 <- c()

for(d in Degree){
  Model_5CV <- glm(mpg ~ poly(horsepower, d), data = TRD)
  CV_ERR_5[d] <- cv.glm(TRD, Model_5CV, K = 5)$delta[1]
}

CV_ERR_5




# 결과 시각화

plot(Degree, CV_ERR_5, type = "b", col = "red")





# 10-Fold Cross-Validation(K = 10)

CV_ERR_10 <- c()



for(d in Degree){
  Model_10CV <- glm(mpg ~ poly(horsepower, d), data = TRD)
  CV_ERR_10[d] <- cv.glm(TRD, Model_10CV, K = 10)$delta[1]
}

CV_ERR_10



# 결과 시각화

plot(Degree, CV_ERR_10, type = "b", col = "blue")












# 02 - LOOCV(Leave-One-Out Cross-Validation)
# Data Point의 개수만큼 Model을 생성
# Model을 생성 시 한개의 Test Data를 제외
# 전체 결과의 평균을 적용

CV_ERR_L <- c()




# 1차부터 5차까지 모델적용

for(d in Degree){
  Model_LCV <- glm(mpg ~ poly(horsepower, d), data = TRD)
  CV_ERR_L[d] <- cv.glm(TRD, Model_LCV)$delta[1]
}

CV_ERR_L



# 결과 시각화

plot(Degree, CV_ERR_L, type = "b", col = "seagreen")






# 5-Fold CV vs. 10-Fold CV vs. LOOCV
# 결과 시각화 비교

plot(Degree, CV_ERR_5, 
     type = "b", col = "red", ylab = "CV Errors", lwd = 2)

lines(Degree, CV_ERR_10, type = "b", col = "blue", lwd = 2)

lines(Degree, CV_ERR_L, type = "b", col = "seagreen", lwd = 2)

legend("topright", pch = 1,
       c("5-fold CV", "10-fold CV", "LOOCV"),
       col = c("red", "blue", "seagreen"))



























# Bias-Variance Tradeoff ####

# Sample Data 생성

OutPut <- c()
InPut <- seq(-10, 10, by = 0.02)



set.seed(2045)



Noise <- rnorm(length(InPut), sd = 0.3)
OutPut <- sin(InPut) + Noise



# Sample Data 시각화

plot(InPut, OutPut, main = "Sample Data")
lines(InPut, sin(InPut), col = "tomato", lwd = 5)








# plots 창 분할(3 x 3)

par(mfrow = c(3, 3))



# 1차부터 9차까지 모델생성 및 시각화

for(i in 1:9){
  Model_T1 <- lm(OutPut ~ poly(InPut, i))
  New_Data <- data.frame(x = InPut)
  Pred <- predict.lm(Model_T1, newdata = New_Data)
  Plot_Name <- paste0(i, " Degree")
  plot(InPut, OutPut, main = Plot_Name, cex = 0.6)
  lines(InPut, Pred, col = "forestgreen", lwd = 3)
}




# 차수에 따른 예측값 list()

Pred_Degree <- list()  



# 차수 별 7개의 Test_Data 구성

for(i in 1:9){
  Pred_Model_Each <- list()
  for(j in 1:7){
    # Boostrap
    T_Data <- data.frame(f = OutPut, x = InPut)
    Test_INDEX <- sample(1:length(InPut), size = 7, replace = FALSE) 
    Train_Data <- T_Data[-Test_INDEX, ]
    Test_Data <- T_Data[Test_INDEX, ]
    Model_T2 <- lm(f ~ poly(x, i), data = Train_Data)
    Pred_Model_Each[[j]] <- data.frame(x = Test_Data$x,
                                       predict.lm(Model_T2,
                                                  newdata = Test_Data))
  }
  Pred_Degree[[i]] <- Pred_Model_Each
}



# Model Variance plot

for(j in 1:9){
  Plot_Name <- paste0(j, " Degree")
  plot(InPut, OutPut, 
       xlim = c(-10, 10), 
       ylim = c(-1.8, 1.8), 
       main = Plot_Name, cex = 0.6)
  for(i in 1:7){
    lines(Pred_Degree[[j]][[i]][ ,1], 
          Pred_Degree[[j]][[i]][ ,2], 
          col = rainbow(6)[i], 
          lwd = 3)
  }
}

par(mfrow = c(1, 1))

for(j in 1:9){
  Plot_Name <- paste0(j, " Degree")
  plot(InPut, OutPut, 
       xlim = c(-10, 10), 
       ylim = c(-1.8, 1.8), 
       main = Plot_Name, cex = 0.6)
  for(i in 1:7){
    lines(Pred_Degree[[j]][[i]][ ,1], 
          Pred_Degree[[j]][[i]][ ,2], 
          col = rainbow(10)[i], 
          lwd = 3)
  }
}













