# R 기본 함수 ####

print("Hello World")

"Hello World"





# 에러발생

print("Hello", "World")



# 여러 단어 출력

cat("Hello", "World")






# 에러발생 - 미지정 객체(Object)

print(koo)



# 객체값 할당

koo <- "Hello World"



# 객체값 출력

print(koo)


koo







# 문자, 내장객체, 사용자지정객체 출력

cat("Hello World", pi, koo)



# 여러 함수 실행

print("Hello World") ; print(pi) ; print(koo)



# 이탈문자(\n, \t) 사용

cat("Hello World", "\n", pi, "\t", koo)





# 객체명 지정 규칙 ####
# 1) 객체명은 숫자, 특수문자로 시작 불가
# 2) 사용 가능한 특수 문자 : ".", "_"
# 3) 객체명에 공백문자(" ") 포함 불가
# 4) 객체명은 대소문자 구별

# Qoo1에 9 할당

Qoo1 <- 9



# Qoo2에 3 할당

Qoo2 <- 3



# Qoo2의 값(3)을 Qoo1에 할당

Qoo1 <- Qoo2



# Qoo2에 7 할당

Qoo2 <- 7



# Qoo1 vs Qoo2

Qoo1

Qoo2





# Data Type ####

num <- 999
class(num)



char <- "R Program"
class(char)



logi <- TRUE
class(logi)






# 산술연산 ####

10 + 3

10 - 3

10 * 3



10 / 3

10 %/% 3

10 %% 3


10 ^ 2



# 연산 우선순위

7 + 8 * 9

(7 + 8) * 9




# 반올림연산

round(24.47)


round(24.56)

round(24.56, digits = 0)


round(24.99, -1)

round(25.51, -1)

round(24.35, 1)

round(24.246, 2)













# 논리연산 ####

88 > 99
88 < 99

"오이" == "호박"
"오이" != "호박"

88 >= 99
88 <= 99



# 주의

"팔" > 9




# AND/OR

x <- TRUE ; y <- FALSE

x & y
x | y

!x
isTRUE(x)

z <- TRUE

(x | y) & (z & y)







# 벡터 ####
# R의 최소 데이터 단위

a <- c(1, 3, 5, 7, 9)
is.vector(a)

b <- c("A", "B", "C")
is.vector(b)
  
c <- c(9)
is.vector(c)









# seq() 함수 사용하기

1:10

10:-10

2.5:9

9:0

seq(1, 10, 2)

seq(11, 20, 1.3)







# 벡터(Vector)의 길이 및 특정문자 포함 여부 찾기

qoo <- c(1, 3, 5, 7, 9)

koo <- seq(1, 10, 2)


length(koo)












# 데이터프레임 ####

v1 <- c("Renault", "Picasso", "Peugeot", "Focus", "Fiesta")

v2 <- c(24000, 17000, 23000, 15000, 12000)

v3 <- c(3, 5, 2, 2, 3)

DF <- data.frame(v1, v2, v3)

DF

DF <- data.frame(Model = v1,
                 Price = v2,
                 Count = v3)





# 데이터프레임 사용하기

DF$Model

DF[3, 1]

DF[1, ]

DF[, 3]

DF[c(3, 5),]

DF[, c(1, 3)]

DF[2:4,]

DF[, 1:3]



# subset() 함수 사용하기

subset(DF, Count > 2)

subset(DF, Model == "Focus")

subset(DF, Price >= 20000)

subset(DF, Count > 2 & Price > 20000)

subset(DF, Price < 20000 | Count > 2)

subset(DF, Price < 19000 & Count == 2)







