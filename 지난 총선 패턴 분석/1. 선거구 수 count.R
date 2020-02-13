rm(list=ls())

### 패키지 로드 및 working directory 경로 설정
library(readxl)
library(tidyverse)
setwd("C:/Users/USER/Desktop")

### 득표수 data 불러오기
data_20 <- readxl::read_excel("data.xlsx", sheet="제20대", col_names=TRUE)
data_19 <- readxl::read_excel("data.xlsx", sheet="제19대", col_names=TRUE)
data_18 <- readxl::read_excel("data.xlsx", sheet="제18대", col_names=TRUE)
data_17 <- readxl::read_excel("data.xlsx", sheet="제17대", col_names=TRUE)
data_16 <- readxl::read_excel("data.xlsx", sheet="제16대", col_names=TRUE)
data_15 <- readxl::read_excel("data.xlsx", sheet="제15대", col_names=TRUE)

### tibble data로 변환 후 빈 칸을 0으로 둠
data_20 <- as_tibble(data_20); data_20[is.na(data_20)] <- 0
data_19 <- as_tibble(data_19); data_19[is.na(data_19)] <- 0
data_18 <- as_tibble(data_18); data_18[is.na(data_18)] <- 0
data_17 <- as_tibble(data_17); data_17[is.na(data_17)] <- 0
data_16 <- as_tibble(data_16); data_16[is.na(data_16)] <- 0
data_15 <- as_tibble(data_15); data_15[is.na(data_15)] <- 0

### 선거구명이 겹치는 것을 방지하기 위해 '지역_선거구명'으로 선거구명 변환
data_20$"선거구명" <- paste(data_20$"지역", data_20$"선거구명", sep="_")
data_20 <- data_20[,-c(1:2)]
data_19$"선거구명" <- paste(data_19$"지역", data_19$"선거구명", sep="_")
data_19 <- data_19[,-c(1:2)]
data_18$"선거구명" <- paste(data_18$"지역", data_18$"선거구명", sep="_")
data_18 <- data_18[,-c(1:2)]
data_17$"선거구명" <- paste(data_17$"지역", data_17$"선거구명", sep="_")
data_17 <- data_17[,-c(1:2)]
data_16$"선거구명" <- paste(data_16$"지역", data_16$"선거구명", sep="_")
data_16 <- data_16[,-c(1:2)]
data_15$"선거구명" <- paste(data_15$"지역", data_15$"선거구명", sep="_")
data_15 <- data_15[,-c(1:2)]

### 6개년치에서 선거구명이 하나라도 존재하는 선거구들
fulljoindata <- data_20 %>%
  full_join(data_19, by="선거구명") %>%
  full_join(data_18, by="선거구명") %>%
  full_join(data_17, by="선거구명") %>%
  full_join(data_16, by="선거구명") %>%
  full_join(data_15, by="선거구명")

### 6개년치에서 선거구명이 모두 중복된 선거구들
innerjoindata <- data_20 %>%
  inner_join(data_19, by="선거구명") %>%
  inner_join(data_18, by="선거구명") %>%
  inner_join(data_17, by="선거구명") %>%
  inner_join(data_16, by="선거구명") %>%
  inner_join(data_15, by="선거구명")

nrow(fulljoindata)   # 488개 : 6번의 총선에서 모든 선거구명의 개수
nrow(innerjoindata)  # 83개 : 6번의 총선에서 모두 이름이 같았던 선거구의 개수

# 결론 : 20대 총선의 선거구는 253개인데, 생각한것보다 선거구의 이름이 제각기
# 선거구 이름을 20대 총선에 맞춰서 수정하는 작업이 필요함.