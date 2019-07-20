# Machine 함수 정의 ####
# x, w, b를 받아 예측값(y_hat)을 리턴하는 머신(기계)
# y = wx + b

Machine <- function(x, w, b) {

	y_hat <- (w * x) + b

	return(y_hat)
}



# Machine 함수 사용 예시

x1 <- c(1, 3, 5, 7, 9)
w1 <- 2
b1 <- 1



# 실행(함수사용)

Machine(x1, w1, b1) 






# Gradient 함수정의 ####
# x, y, w, b를 받아 Parameter의 Gradient를 출력하는 머신(기계)

Gradient <- function(x, y, w, b) {

  # Machine 함수를 사용하여 현재 Parameter를 이용한 예측값(y_hat)을 구함
	y_hat <- Machine(x, w, b)

	# 공식에 따라 dw를 계산
	dw <- sum((y - y_hat) * (-2 * x))
	# 공식에 따라 db를 계산
	db <- sum((y - y_hat) * (-2))

	# 계산된 값으로 list 생성
	GDs <- list(dw = dw, db = db)
	
	# 생성된 list를 반환
	return(GDs)
}



# Gradient 함수 사용 예시

x1 <- c(1, 3, 5, 7, 9)
y1 <- c(2, 4, 6, 8, 10)
w1 <- 2
b1 <- 1

GDs <- Gradient(x1, y1, w1, b1)

GDs$dw
GDs$db







# Learning 함수정의 ####
# x, y, w, b를 받아 Update된 Parameter를 출력

Learning <- function(x, y, w, b, step) {

  # Gradient 함수를 사용하여 경사를 계산
	G <- Gradient(x, y, w, b)

	# w 업데이트
	uw <- w - step * G$dw
	# b 업데이트
	ub <- b - step * G$db 

	# Update된 w와 b로 리스트 생성
	Updated <- list(w = uw, b = ub)

	# Update된 리스트 반환
	return(Updated) 
}



# Learning 함수 사용 예시

x1 <- c(1, 3, 5, 7, 9)
y1 <- c(2, 4, 6, 8, 10)
w1 <- 2
b1 <- 1



# Learning Rate

step1 <- 0.0001

parameters <- Learning(x1, y1, w1, b1, step1)




# w1이 2에서 1.967로 Update

parameters$w

# b1이 1에서 0.995으로 Update

parameters$b













# Machine Learning Test ####
# Machine 함수를 정의하고 Gradient 함수를 정의하는데 사용
#	Gradient 함수를 정의하고 이를 Learning 함수를 정의하는데 사용
# 최종적으로 Learning 함수만 사용

# read.csv() 파일 읽어오기

DATA <- read.csv("mlData/testData.csv")


# 읽어온 데이터 확인

str(DATA)
head(DATA, 10)
plot(DATA)





# Learning Start!!!

# w와 b의 초기값 설정

w <- 2
b <- 3




# For(반복문)를 사용하여 반복 횟수를 1000번으로 지정

for(i in 1:1000){
	Parameters <- Learning(DATA$inputs, DATA$outputs, w, b, 0.0001)
	w <- Parameters$w
	b <- Parameters$b
} 



print(w)
print(b)



# 학습결과를 시각적으로 확인

plot(DATA$inputs, DATA$outputs,
     pch = 19, 
     cex = 0.3)

abline(b, w, lwd = 3, col = "red")








# lm() 함수 적용 ####
# lm()함수의 동작 원리는 위와 같음

Model <- lm(outputs ~ inputs, DATA)
structure(Model)

plot(DATA, pch = 1, cex = 0.5)
abline(Model, lwd = 3, col = "blue")












# Updated with loss
# loss : 오차제곱합 추가

Gradient <- function(x, y, w, b) {

  y_hat <- Machine(x, w, b)

  dw <- sum((y - y_hat) * (-2 * x))
  db <- sum((y - y_hat) * (-2))

  loss <- sum((y - y_hat) ^ 2)
  
  GDs <- list(dw = dw, db = db, loss = loss)
  
  return(GDs)
}




Learning <- function(x, y, w, b, step) {
  
  G <- Gradient(x, y, w, b)
  
  uw <- w - step * G$dw
  ub <- b - step * G$db 
  
  Updated <- list(w = uw, b = ub, loss = G$loss)

  return(Updated) 
}




loss <- c()
w <- 2 ; b <- 3



for(i in 1:1000){
  Parameters <- Learning(DATA$inputs, DATA$outputs, w, b, 0.000001)
  w <- Parameters$w
  b <- Parameters$b
  loss[i] <- Parameters$loss
}




head(loss, 100)

plot(loss[1:150])

plot(loss)

plot(loss[950:1000])



