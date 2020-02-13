rm(list=ls())

### 패키지 로드 및 working directory 경로 설정
library(readxl)
setwd("C:/Users/USER/Desktop")

### 득표율 data 불러오기
percent_20 <- readxl::read_excel("data.xlsx", sheet="제20대_득표율", col_names=TRUE)
percent_19 <- readxl::read_excel("data.xlsx", sheet="제19대_득표율", col_names=TRUE)
percent_18 <- readxl::read_excel("data.xlsx", sheet="제18대_득표율", col_names=TRUE)
percent_17 <- readxl::read_excel("data.xlsx", sheet="제17대_득표율", col_names=TRUE)
percent_16 <- readxl::read_excel("data.xlsx", sheet="제16대_득표율", col_names=TRUE)
percent_15 <- readxl::read_excel("data.xlsx", sheet="제15대_득표율", col_names=TRUE)

### 20대 지역, 선거구명, 구시군명 정보를 따로 저장하고, 지역코드 추가
elec <- percent_20[,c(2:4)]
code <- (c(rep("1",49), rep("2",18), rep("3",12), rep("4",13), rep("5",8), rep("6",7), 
           rep("7",6), rep("8",1), rep("9",60), rep("10",8), rep("11",8), rep("12",11), 
           rep("13",10), rep("14",10), rep("15",13), rep("16",16), rep("17",3)))
elec <- cbind(code, elec)

### 득표율을 제외한 나머지 열을 제거
percent_20 <- percent_20[,-c(1:4)]
percent_19 <- percent_19[,-c(1:8)]
percent_18 <- percent_18[,-c(1:8)]
percent_17 <- percent_17[,-c(1:8)]
percent_16 <- percent_16[,-c(1:8)]
percent_15 <- percent_15[,-c(1:8)]

### 각 연도의 당명을 저장
partyname_20 <- colnames(percent_20)
partyname_19 <- colnames(percent_19)
partyname_18 <- colnames(percent_18)
partyname_17 <- colnames(percent_17)
partyname_16 <- colnames(percent_16)
partyname_15 <- colnames(percent_15)

### 최고 득표율 정당을 기입하기 위한, null matrix 생성
maxpercent <- matrix(c(rep(0, 253*6)), nrow=253, ncol=6)

### 최고 득표율을 얻은 정당을 찾아, 위 matrix에 기입
for(i in 1:253){maxpercent[i,1] <- partyname_20[which.max(percent_20[i,])]}
for(i in 1:253){maxpercent[i,2] <- partyname_19[which.max(percent_19[i,])]}
for(i in 1:253){maxpercent[i,3] <- partyname_18[which.max(percent_18[i,])]}
for(i in 1:253){maxpercent[i,4] <- partyname_17[which.max(percent_17[i,])]}
for(i in 1:253){maxpercent[i,5] <- partyname_16[which.max(percent_16[i,])]}
for(i in 1:253){maxpercent[i,6] <- partyname_15[which.max(percent_15[i,])]}

### 최고 득표율 matrix의 열 이름 기입
colnames(maxpercent) <- c("20대","19대","18대","17대","16대","15대")

### 당을 그룹화한 후에, 1~4 숫자로 변환
A <- c("새누리당_20", "새누리당_19", "한나라당_18", "한나라당_17", "한나라당_16", "신한국당_15",
       "자유선진당_19", "자유선진당_18", "자민련_17", "자유민주연합_16", "자유민주연합_15", "친박연대_18")
B <- c("더불어민주당_20", "민주통합당_19", "통합민주당_18", "새천년민주당_17",
       "열린우리당_17", "새천년민주당_16", "새정치국민회의_15", "통합민주당_15",
       "국민의당_20")
C <- c("정의당_20", "통합진보당_19", "민주노동당_18", "민주노동당_17")

for (i in 1:nrow(maxpercent)){
  for (j in 1:ncol(maxpercent)){
    ifelse(maxpercent[i,j] %in% A, maxpercent[i,j] <- 1, 
           ifelse(maxpercent[i,j] %in% B, maxpercent[i,j] <- 2,
                  ifelse(maxpercent[i,j] %in% C, maxpercent[i,j] <- 3, 
                         maxpercent[i,j] <- 4)))
  }
}

### 20,19,...,15의 순서를 15,16,...,20으로 바꾸고 
maxpercent <- maxpercent[,c(6,5,4,3,2,1)]

### 위에서 저장한 지역, 선거구명, 구시군명 정보와 합쳐 maxpercent 자료를 만듬
maxpercent <- cbind(elec, maxpercent)

### 15~20대 총선 당선 정당을 연속된 숫자로 나열 : paste1 = permu
paste1 <- paste0(maxpercent$`15대`,maxpercent$`16대`,maxpercent$`17대`,
                 maxpercent$`18대`,maxpercent$`19대`,maxpercent$`20대`)

### 15~20대 총선 당선 정당을 순서 상관없이 오름차순으로 나열 : paste2 = combi
temp <- t(maxpercent[, 5:10])
temp <- apply(temp, 2, sort)
temp <- t(temp)
paste2 <- paste0(temp[,1],temp[,2],temp[,3],temp[,4],temp[,5],temp[,6])

### 원래 maxpercent data와 위의 paste1, paste2를 합침
maxpercent <- cbind(maxpercent, as.numeric(paste1), as.numeric(paste2))
write.csv(maxpercent, file="maxpercent.csv")

################################################################################

### 추가 작업으로 무소속 데이터 처리 작업 시행

## 1. 20대 선거구명으로 기준을 통일하면서 문제가 발생한 선거구 3개 제거
# (1) 44 서울 강남구병 
# (2) 71 대구 서구
# (3) 226 경북 안동시
# 세부사항은 '무소속 문제있는애들.xlsx' 참고

## 2. 패턴을 간소화하기 위하여, '4'로 당선된 당선인들의 역대 정치 이력을 분석하여, 선거 전후 소속당이 명확한 경우에는 그 당으로 소속을 수기로 변경함.

## 3. 추가로, paste1열과 paste2열도 위 수정사항에 맞춰서 변경함. (코드는 없음)

## => 위 세 작업을 수행하여 변경된 파일은 'pattern.csv'파일로 저장함.
# 다음 분석부터 이 파일을 사용할 것.
