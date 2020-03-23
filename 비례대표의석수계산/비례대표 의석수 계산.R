### If the Korean language seems to be broken, Follow this.
### File -> Reopen with Encoding -> UTF-8 -> Click OK button
### (OR, File -> Reopen with Encoding -> CP949 or euc-kr -> Click OK button)

rm(list=ls(all.names=T))      # will clear all objects includes hidden objects.
dev.off()                     # closes the specified plot
gc()                          # free up memory and report the memory usage.
cat("\014")                   # clear console workspace

###############################################################################

########################## 비례대표 의석 수 계산 CODE #########################

## <참고 기준>
## 공직선거법 제189조(비례대표국회의원의석의 배분과 당선인의 결정ㆍ공고ㆍ통지)
## 공직선거법 부칙<법률 제16864호> 제4조(비례대표국회의원의석의 배분에 관한 특례)
## 중앙선관위, '비례대표 의석배분 방식 주요 개정 내용'(2020-01-30)

## <목차>
##### 0. 데이터 불러오기
##### 1. 의석할당정당 결정
##### 2. 의석할당정당의 의석수 산출 및 의석배분
##### 3. 전체 의석수 산출

###############################################################################

### [예제 데이터]

# (예시 1-1) : 선관위 예시 내용 => 맞는 것 확인함
# https://www.nec.go.kr/portal/bbs/view/B0000338/40164.do?menuNo=200185
partyname <- c("A당", "B당", "C당", "D당", "무소속 등")
data1 <- matrix(c(100, 80, 40, 30, 3), nrow=1)
colnames(data1) <- partyname
data2 <- matrix(c(0.4, 0.3, 0.1, 0.2, 0), nrow=1)
colnames(data2) <- partyname

# # (예시 1-2) : 선관위 예전 예시 내용 => step3만 54석으로 돌리면 됨 => 맞는것 확인
# # https://www.nec.go.kr/portal/bbs/view/B0000245/3363.do?menuNo=200033
# partyname <- c("가당", "나당", "다당", "라당", "마당" , "바당" ,"사당")
# data1 <- matrix(c(80,79,28,2,7,5,4), nrow=1)
# colnames(data1) <- partyname
# data2 <- matrix(c(0.36,0.38,0.125,0.07,0.03,0.01,0.025), nrow=1)
# colnames(data2) <- partyname
# 
# # (예시 2-1) : 개인이 만든 계산기 => 사이트와 일치하는 것 확인함
# # http://m.ppomppu.co.kr/new/bbs_view.php?id=issue&no=241552&page=1
# partyname <- c("더불어민주당", "미래통합당", "미래한국당", "정의당", "국민의당", "민생당", "우리공화당", "기타정당", "무소속")
# data1 <- matrix(c(129,113,0,2,0,3,3,0,3), nrow=1)
# colnames(data1) <- partyname
# data2 <- matrix(c(0.31,0,0.36,0.12,0.04,0.045,0.058,0.067,0), nrow=1)
# colnames(data2) <- partyname
# 
# # (예시 2-2) : 개인이 만든 계산기 엑셀 사례 => 다르게나옴
# partyname <- c("더불어민주당", "미래통합당", "미래한국당", "정의당", "국민의당", "민생당", "자유공화당", "기타정당", "무소속")
# data1 <- matrix(c(140,102,0,2,0,3,3,0,3), nrow=1)
# colnames(data1) <- partyname
# data2 <- matrix(c(0.37,0,0.35,0.1,0.04,0.035,0.045,0.06,0), nrow=1)
# colnames(data2) <- partyname
# 
# # (예시 3) => 사이트와 일치하는 것 확인함
# # http://www.kgreens.org/member/?mod=document&uid=896&pageid=1&fbclid=IwAR2c-sYwpP-7e2YNBp1thuUjLBneevla3qD2JvAw6xDtwxAtzJL04mFVrWA
# partyname <- c("민주당","자유한국당","정의당","미래한국당","바른미래당","그 외 정당", "무소속")
# data1 <- matrix(c(135,105,2,0,0,0,11), nrow=1)
# colnames(data1) <- partyname
# data2 <- matrix(c(0.4,0,0.13,0.38,0.03,0.06,0), nrow=1)
# colnames(data2) <- partyname
# 
# # (예시 4) => 사이트와 일치하는 것 확인함
# # https://www.mk.co.kr/news/politics/view/2019/12/1081199/
# partyname <- c("A당", "B당", "C당", "D당", "무소속")
# data1 <- matrix(c(100,3,110,10,30), nrow=1)
# colnames(data1) <- partyname
# data2 <- matrix(c(0.3,0.1,0.4,0.05,0.15), nrow=1)
# colnames(data2) <- partyname
# 
# # (예시 5) => 살짝 다르게 나옴 (참여연대 계산기와도 다름)
# # https://news.v.daum.net/v/20200311151012354
# partyname <- c("더불어민주당", "미래통합당", "미래한국당", "정의당", "국민의당", "민생당")
# data1 <- matrix(c(120,128,0,2,0,3), nrow=1)
# colnames(data1) <- partyname
# data2 <- matrix(c(0.40,0,0.40,0.1,0.05,0.05), nrow=1)
# colnames(data2) <- partyname

###############################################################################

##### 0. 데이터 불러오기

# 데이터 입력시 주의사항
# data1 : 지역구 당선인수 데이터 (정수)  => 합이 꼭 253이 되어야 함
# data2 : 비례득표비율 데이터 (%)

data1
data2

if(!sum(data1) == 253){print("warning : 지역구 합이 253석이 아님")}

###############################################################################


##### 1. 의석할당정당 결정

### (1) 지역구 5석 이상 정당
party1 <- partyname[data1 >= 5]
party1

### (2) 비례대표 전국유효득표 3% 이상 정당
party2 <- partyname[data2 >= 0.03]
party2

### (3) 의석할당정당
party <- union(party1, party2)
party <- party[!party %in% c("무소속", "무소속 등", "무소속등", "무소속 외", "무소속외")]
party <- party[!party %in% c("기타", "기타정당", "그 외 정당", "그 외")]
party

### 의석할당정당의 득표율로 바꾸는 코드 (공직선거법 제189조 3항)
denominator1 <- 1 - sum(data2[!colnames(data2) %in% party])
data2[colnames(data2) %in% party] <- data2[colnames(data2) %in% party] / denominator1
data2[!colnames(data2) %in% party] <- 0
data2

### 입력 데이터의 합이 100%가 안될 때(기타정당 입력을 안한 경우) 맞추는 코드
denominator2 <- sum(data2)
data2 <- data2 / denominator2
data2

###############################################################################

##### 2. 의석할당정당의 의석수 산출 및 의석배분

### (1단계) 30석에 대해 전국단위 준연동(연동비율 50%) 방식으로 각 정당별 연동배분의석수 산정

# x1 : 국회의원 정수 = 300
# x2 : 의석할당정당이 추천하지 않은 지역구국회의원 당선인수
# x3 : 해당 정당의 비례대표국회의원 선거 득표비율
# x4 : 해당 정당의 지역구 국회의원 당선인수

x1 = 300
x2 = sum(data1[!partyname %in% party])
x3 = data2[ ,partyname %in% party]
x4 = data1[ ,partyname %in% party]

# 연동배분의석수
step1 = round(( (x1-x2)*x3 - x4)/2)
step1[step1<1] <- 0

# 아래는 step2, step3와 행,열 형식을 맞추기 위한 코드
temp <- data1
temp[colnames(data1) %in% names(step1)] <- step1
temp[!colnames(data1) %in% names(step1)] <- 0
step1 <- temp


### (2단계)
# (2-1단계) 각 정당별 연동배분의석수의 합계 < 30석
#  => 잔여의석에 대해 기존 의석배분방식(병립형) 적용 배분
# (2-2단계) 각 정당별 연동배분의석수의 합계 > 30석
#  => 각 정당별 연동배분의석수비율대로 배분

{if (sum(step1) < 30) {

  step2 = (30 - sum(step1)) * data2
  step2_1 = floor(step2)
  step2_2 = step2 - floor(step2)
  
  while(30 - sum(step1) - sum(step2_1) > 0){
    index <- sample(which(number == max(number)), 1)
    step2_1[index] <- step2_1[index] + 1
    step2_2[index] <- 0
  }
  
  step2 <- step2_1
  step2_name <- "2-1단계 : 잔여배분의석수"
}
  else {
    step2 = 30 * step1 / sum(step1)
    step2_1 = floor(step2)
    step2_2 = step2 - floor(step2)
    
    while(30 - sum(step2_1) > 0){
      index <- sample(which(number == max(number)), 1)
      step2_1[index] <- step2_1[index] + 1
      step2_2[index] <- 0
    }
    step2 <- step2_1
    step2_name <- "2-2단계 : 조정의석수"
  }
}


### (3단계) 17석에 대해 기존 의석배분방식(병립형) 적용 배분
step3 = 17 * data2
step3_1 = floor(step3)
step3_2 = step3 - floor(step3)
while (17 - sum(step3_1) > 0){
  index <- sample(which(number == max(number)), 1)
  step3_1[index] <- step3_1[index] + 1
  step3_2[index] <- 0
}
step3 <- step3_1


###############################################################################

##### 3. 전체 의석수 산출

{if (sum(step1) > 30) { newstep1 <- 0 }
  else                { newstep1 <- step1 }}

# 각 단계의 의석수를 출력
print("1단계 : 연동배분의석수"); print(newstep1)
print(step2_name); print(step2)
print("3단계: 배분의석수"); print(step3)

# 준연동형 의석수, 병립형 의석수, 전체 비례대표 의석수 출력
result <- rbind(newstep1+step2, step3, newstep1+step2+step3)
result <- cbind(result, "합계"=apply(result, 1, sum))
rownames(result) <- c("준연동형비례", "병립형비례", "비례대표의석수")
result

# 위의 결과와 지역구 결과를 합쳐서 전체 의석수를 출력
final <- rbind(data1, result[3,-c(ncol(result))], data1 + result[3,-c(ncol(result))])
final <- cbind(final, "합계"=apply(final, 1, sum))
rownames(final) <- c("지역구", "비례대표", "전체")
final

