rm(list=ls())

### 패키지 로드 및 최고득표율정당 data 불러오기
library(tidyverse)
setwd("C:/Users/USER/Desktop")
pattern <- read.csv(file="pattern.csv", header=TRUE)

### 패턴을 보기위해 필요한 열만 저장 후 열 이름 수정
pattern <- pattern[,c(2:7,14:15)]
colnames(pattern) <- c("id", "num","code","지역", "선거구명", "구시군명", "permu", "combi")

### tibble 데이터로 바꾼 후, group 변수 생성
pattern <- as_tibble(pattern)
pattern <- pattern %>%
  mutate(group = ifelse(combi %in% c(111111,111112), "보수정당경향",
                        ifelse(combi %in% c(222222,122222), "민주당계경향",
                               ifelse(combi %in% c(111222,112222,111122), "중도경향", "그외"))))

### 전국단위, combi를 빈도수에 따라 내림차순 정렬
data00 <- pattern %>%
  group_by(combi, group) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
data00

### 지역별, permu를 빈도수에 따라 내림차순 정렬
data01 <- pattern %>% filter(code == 1) %>% # 서울
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data01
data02 <- pattern %>% filter(code == 2) %>% # 부산
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data02
data03 <- pattern %>% filter(code == 3) %>% # 대구
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data03
data04 <- pattern %>% filter(code == 4) %>% # 인천
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data04
data05 <- pattern %>% filter(code == 5) %>% # 광주
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data05
data06 <- pattern %>% filter(code == 6) %>% # 대전
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data06
data07 <- pattern %>% filter(code == 7) %>% # 울산
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data07
data08 <- pattern %>% filter(code == 8) %>% # 세종
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data08
data09 <- pattern %>% filter(code == 9) %>% # 경기
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data09
data10 <- pattern %>% filter(code == 10) %>% # 강원
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data10
data11 <- pattern %>% filter(code == 11) %>% # 충북
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data11
data12 <- pattern %>% filter(code == 12) %>% # 충남
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data12
data13 <- pattern %>% filter(code == 13) %>% # 전북
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data13
data14 <- pattern %>% filter(code == 14) %>% # 전남
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data14
data15 <- pattern %>% filter(code == 15) %>% # 경북
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data15
data16 <- pattern %>% filter(code == 16) %>% # 경남
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data16
data17 <- pattern %>% filter(code == 17) %>% # 제주
  group_by(permu, group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data17


### group별 빈도 분석
data00 <- pattern %>%
  group_by(group) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
data00

data01 <- pattern %>% filter(code == 1) %>% # 서울
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data01
data02 <- pattern %>% filter(code == 2) %>% # 부산
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data02
data03 <- pattern %>% filter(code == 3) %>% # 대구
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data03
data04 <- pattern %>% filter(code == 4) %>% # 인천
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data04
data05 <- pattern %>% filter(code == 5) %>% # 광주
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data05
data06 <- pattern %>% filter(code == 6) %>% # 대전
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data06
data07 <- pattern %>% filter(code == 7) %>% # 울산
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data07
data08 <- pattern %>% filter(code == 8) %>% # 세종
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data08
data09 <- pattern %>% filter(code == 9) %>% # 경기
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data09
data10 <- pattern %>% filter(code == 10) %>% # 강원
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data10
data11 <- pattern %>% filter(code == 11) %>% # 충북
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data11
data12 <- pattern %>% filter(code == 12) %>% # 충남
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data12
data13 <- pattern %>% filter(code == 13) %>% # 전북
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data13
data14 <- pattern %>% filter(code == 14) %>% # 전남
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data14
data15 <- pattern %>% filter(code == 15) %>% # 경북
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data15
data16 <- pattern %>% filter(code == 16) %>% # 경남
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data16
data17 <- pattern %>% filter(code == 17) %>% # 제주
  group_by(group) %>%  summarise(count=n()) %>%  arrange(desc(count)); data17