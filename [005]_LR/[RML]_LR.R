# sigmoid( ) 함수 지정

sigmoid <- function(x){
  
  y <- 1 / (1 + exp(-x))
  
  return(y)
}



# sigmoid( ) 테스트

sigmoid(0)
sigmoid(100000000)
sigmoid(-100000000)



# sigmoid() 시각화

n <- seq(-10, 10, 0.01)
plot(sigmoid(n) ~ n, pch = 19, col = "tomato")







# Information Theory(정보 이론) ####
# A, B, C 사건의 발생확률에 대한 정보량

A <- 0.6 ; B <- 0.3 ; C <- 0.1
(I_A <- -log(A))
(I_B <- -log(B))
(I_C <- -log(C))



# Degree of Surprise(Information)
# AlphaGo Vs. Apes

AlphaGo <- 0.99 ; Apes <- 0.01
(I_AlphaGo <- -log(AlphaGo))
(I_Apes <- -log(Apes))




# Entropy(불확실성 척도) ####
# (승리)확률변수 P1과 P2를 사용하여 엔트로피 계산

# 1) 실력 차이가 많은 경우

P1 <- 0.99 ; P2 <- 0.01
(E1 <- -P1 * log(P1) - P2 * log(P2))



# 2) 실력 차이가 적은 경우

P1 <- 0.5 ; P2 <- 0.5
(E2 <- -P1 * log(P1) - P2 * log(P2))


E1 ; E2



# Cross-Entropy Error ####
# 교사데이터(y)가 Cross-Entropy의 가중치로 적용
# CE(-log(y_hat)) = y * -log(y_hat)
#                 = -y * log(y_hat)

# Loss = -y * log(y_hat) - (1 - y) * log(1 - y_hat)
# Cost = sum(Loss)













# 분류모델(Classification) ####

# sigmoid( ) 함수 지정

sigmoid <- function(x){
  
  y <- 1 / (1 + exp(-x))
  
  return(y)
}



# C_Machine( ) 분류머신 생성
# sigmoid( ) 함수를 필터로 적용

C_Machine <- function(x, w, b){
  
  y_hat <- sigmoid(w * x + b)
  
  return(y_hat)
}



# Cross-Entropy Error Cost Function()

Cost <- function(x, y, w, b){
  
  y_hat <- C_Machine(x, w, b)
  
  loss <- -y * log(y_hat) - (1 - y) * log(1 - y_hat)
  
  cost <- mean(loss)
  
  return(cost)
}



# 경사하강법 - 분류모델
# w변화(0.001)와 b변화(0.001)에 따른 
# Cost 변화량 계산

Trainer <- function(x, y, w, b, step){
  
  dw <- (Cost(x, y, w + 0.0001, b) - Cost(x, y, w, b)) / 0.0001
  db <- (Cost(x, y, w, b + 0.0001) - Cost(x, y, w, b)) / 0.0001
  
  w <- w - step * dw
  b <- b - step * db
  
  return(c(w, b))
}






# Default Classification ####

# Default 데이터 읽어오기

Default <- read.csv("mlData/Default.csv")

str(Default)



# default(카드연체), 학생여부, balance(미상환액), 수입

head(Default)

table(Default$default)

plot(Default$balance ~ Default$default)













# One-Hot Encoding

y <- ifelse(Default$default == "Yes", 1, 0)

table(y)





# balance 범위 확인

BLC <- Default$balance

range(BLC)

plot(BLC, cex = 0.5)

boxplot(BLC)



# 변수의 범위를 표준화(Standardization) / > range(scale(BLC))

range((BLC - mean(BLC)) / sd(BLC))

x <- (BLC - mean(BLC)) / sd(BLC)

range(x)



# 결과 비교

par(mfrow = c(2,2))



# Before Standardization

plot(BLC, cex = 0.5)

boxplot(BLC)



# After Standardization

plot(x, cex = 0.5)

boxplot(x)



par(mfrow = c(1,1))















# 경사하강법
# Trainer() Model 적용

w <- 2
b <- 1





# 반복횟수(5000)와 학습률(0.5) 값을 변경하면서 확인!!!
# 1분정도 소요

for(i in 1:5000){
  temp <- Trainer(x, y, w, b, 0.5)
  
  w <- temp[1]
  b <- temp[2]
}

w
b








# 업데이트된 파라미터 적용
# 0 ~ 1 사이 확률값 > options(scipen = 100)

y_prob <- C_Machine(x, w, b)


y_prob[2501:2600]



# 0.5(변경가능) 기준 분류
# 분류예측

y_label <- ifelse(y_prob > 0.5, "Yes", "No")


table(y_label)



# 실제값과 예측값을 비교
# 이진 혼돈 행렬(Binary Confusion Matrix)

table(Default$default, y_label,
      dnn = c("Actual", "Predicted"))





# Model Validation(상환기준)

(accuracy <- (9625 + 100)/(9625 + 42 + 233 + 100))

(precision <- 9625/(9625 + 233))

(recall <- 9625/(9625 + 42))



# F-Score

F_Score <- 2 * ((precision * recall) / (precision + recall))

F_Score





# Model Validation(연체기준)

(accuracy <- (9625 + 100)/(9625 + 42 + 233 + 100))

(precision <- 100/(42 + 100))

(recall <- 100/(233 + 100))



# F-Score

F_Score <- 2 * ((precision * recall) / (precision + recall))

F_Score















# glm() 함수 적용 ####

DFT <- read.csv("mlData/Default.csv")
str(DFT)

DFT$default <- ifelse(DFT$default == "Yes", 1, 0)
DFT$balance <- scale(DFT$balance)



# Train vs. Test(7:3)

set.seed(2045)
TR_IDX <- sample(1:nrow(DFT), nrow(DFT) * 0.7)

TR_DFT <- DFT[TR_IDX,]
TE_DFT <- DFT[-TR_IDX,]



# Modeling

Model_lr <-  glm(default ~ balance,
                 data = TR_DFT,
                 family = "binomial")

Model_lr



# Model_lr Model 적용
# 0 ~ 1 사이의 확률값

y_prob2 <- predict(Model_lr, 
                   newdata = TE_DFT, 
                   type = "response")


head(y_prob2)



# 0.5기준으로 분류

y_label2 <- ifelse(y_prob2 > 0.5, 1, 0)





# 이진 혼돈 행렬(Binary Confusion Matrix)

table(TE_DFT$default, y_label2,
      dnn = c("Actual", "Predicted"))






# Model Validation

(accurary2 <- (27 + 2891)/(2891 + 13 + 69 + 27))

(precision2 <- 27/(27 + 13))

(recall2 <- 27/(27 + 69))



# F-Score2

F_Score2 <- 2 * ((precision2 * recall2) / (precision2 + recall2))

F_Score2















# Smarket Data ####
# 야후파이낸스 S&P 500 Index
# Year      : 거래 년도
# Lag1~5    : 1~5일전 수익률
# Volume    : 주식거래량
# Today     : 오늘의 수익률
# Direction : 상승(Up), 하락(Down)

Smarket <- read.csv("mlData/Smarket.csv")

str(Smarket)

head(Smarket)







# Train vs. Test (7:3)

set.seed(2045)

TR_IX <- sample(1:nrow(Smarket), nrow(Smarket) * 0.7)
TR_S <- Smarket[TR_IX,]
TE_S <- Smarket[-TR_IX,]



# Modeling

Model_SM <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
            data = TR_S, 
            family = "binomial")

Model_SM



# 0 ~ 1 사이의 확률값으로 적용

Pred_PB <- predict(Model_SM,
                   newdata = TE_S,
                   type = "respons")

Pred_PB[101:200]



# 0.5를 기준으로 "Up" 또는 "Down" 분류

Pred_LR <- ifelse(Pred_PB > 0.5, "UP", "Down")


Pred_LR[101:200]




# 교차빈도표 생성

table(TE_S$Direction, Pred_LR, 
      dnn = c("Actual", "Predicted"))






# Model Validation

(accuracy_S <- (44 + 140)/(44 + 130 + 61 + 140))

(recall_S <- 507/(457 + 507))

(precision_S <- 507/(141 + 507))



# F-Score -> P-R Curve & PRAUC

F_Score_S <- 2 * ((precision_S * recall_S) / (precision_S + recall_S))

F_Score_S















# Multinomial Logistic Regression ####
# install.packages("nnet")

# Neural Network Package

library(nnet)


# iris Data

iris <- read.csv("mlData/IRIS.csv")

set.seed(2045)

iris_IDX <- sample(1:nrow(iris), nrow(iris) * 0.7)
TR_IS <- iris[iris_IDX,]
TE_IS <- iris[-iris_IDX,]



# Modeling

Model_MLR <- multinom(Species ~ ., TR_IS)


head(fitted(Model_MLR), 10)




# 분류에 포함될 확률값(type = "probs")

Pred_MLR_P <- predict(Model_MLR, newdata = TE_IS, type = "probs")

Pred_MLR_P[11:30]



# 분류값(type = "class")

Pred_MLR_C <- predict(Model_MLR, newdata = TE_IS, type = "class")

Pred_MLR_C[11:30]



# 교차빈도표

table(TE_IS$Species, Pred_MLR_C)











