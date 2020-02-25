### If the Korean language seems to be broken, Follow this.
### File -> Reopen with Encoding -> UTF-8 -> OK button click

rm(list = ls(all.names = TRUE))   # will clear all objects includes hidden objects.
gc()                              # free up memory and report the memory usage.
cat("\014")                       # clear console workspace

##########################################################################################################
################################################## 목차 ##################################################
##########################################################################################################

## 1. function 정의 - 'prob', 'method', 're_sam', 'result_whole', 'result_region', 'result_capital'
## 2. 패키지 로드 및 데이터 불러오기 & 공통 부분(지역코드 변수 생성, 정당명 네이밍)
## 3. 열세자 무보정
## 4. 열세자 보정(당시 야권 우세)
## 5. 열세자 보정(당시 새누리당 우세)

##########################################################################################################
##################################### 1. 분석에 필요한 function 정의 #####################################
##########################################################################################################

### 1-(1) prob(sam_n, n_dist, n_cand, n_sim) : 시뮬레이션을 통한 당선 확률 계산 함수 
'prob' = function(sam_n, n_dist, n_cand, n_sim){
  result = matrix(0, n_dist, n_cand)                     # n_dist*n_cand zero matrix 생성
  
  for ( i in 1:n_dist ) {
    rand = rdirichlet(n_sim, as.matrix(sam_n[i, ]+1))    # 10000*n_cand matrix가 rand에 저장
    
    for ( j in 1:n_sim ){
      max_r = which(rand[j, ] == max(rand[j, ]))         # rand의 각 행에서 최대치인 열의 번호를 max_r에 저장
      result[i, max_r] = result[i, max_r] + 1            # result의 i행의 max_r열에 1씩 더해줌
    }
  }
  
  return(result/n_sim)                                   # n_sim번으로 나눠줌 => 당선 확률이 됨
}
# rdirichlet(n, alpha) : Dirichlet 분포에서 랜덤 편차를 생성하는 function.
# (n : 생성할 랜덤벡터의 수 / alpha : Vector(or matrix) containing shape parameters)


### 1-(2) method(est_p, n_dist, n_cand) : `prob`함수의 결과를 이용해, 의석 수의 신뢰구간 구하기
# 99% 신뢰수준, 신뢰계수 = 3

'method' = function(est_p, n_dist, n_cand){              # est_p : p(당선 확률 추정치) (n_dist*n_cand matrix)
  sum_p1 = apply(est_p, 2, sum)                          # p를 각 열에 대해 sum (1*n_cand vector)
  sum_var1 = apply(est_p*(1-est_p), 2, sum)              # p(1-p)를 각 열에 대해 sum (1*n_cand vector)
  
  sum_cumul = NULL
  for ( i in 1:n_cand ) {
    sum_cumul[i] = sum(sum_p1[1:i])                      # sum_p1의 순차누적합을 sum_cumul에 저장
  }
  sum_cum = round(sum_cumul)                             # sum_cumul을 반올림해서 정수로 만듬
  
  est = NULL
  est[1] = sum_cum[1]                                  
  for ( i in 2:n_cand ) {
    est[i] = sum_cum[i] - sum_cum[i-1]                   # sum_cum의 각 부분들을 계산(정수형)
  }                                                    
  
  l_b = NULL
  for( i in 1:n_cand) {                                  # floor : 소수점 내림
    l_b[i] = max(0, floor(sum_p1[i] - 3*sqrt(sum_var1[i])))
  }
  u_b = ceiling(sum_p1 + 3*sqrt(sum_var1))               # ceiling : 소수점 올림
  
  return(cbind(l_b, u_b, est))                           # lower, upper, est 구해줌
}


### 1-(3) re_sam(case, c_adj, n_dist, n_cand, sam_n, area)
# 열세자 효과를 고려하여 표본을 재생성
# case : 우세당 (case=1 : 당시 야권이 우세, case=2 : 당시 새누리당이 우세)
# c_adj : 보정크기 alpha

're_sam' = function(case, c_adj, n_dist, n_cand, sam_n, area4){
  n_dist = nrow(sam_n)                                       # 선거구 수 : 253
  n_cand = ncol(sam_n)                                       # 정당 수 : 24  
  sam_c = matrix(0, n_dist, n_cand)                          # n_dist*n_cand zero matrix 생성

    for ( i in 1:n_dist ) {
      
      if ( area4[i] == 1) {                                  # area4==1 : 수도권(서울,경인 지역) 
        n = sum(sam_n[i, ])                                  # 수도권의 각 선거구의 표본 수
        p = sam_n[i, ]/n                                     # 득표율 추정량
        
        if( case == 1 ) {sup = which.max(sam_n[i,2:4]) + 1}  # 야권 우세 경우 sup=가장 득표율 높은 당의 열 번호
        else if ( case == 2 ) {sup = 1}                      # 새누리당 우세 경우 sup=1
        
        sup_p = p[sup] - c_adj                               # 우세정당의 과다예측 방지 위해 보정크기를 뺌
        
        for ( j in 1:n_cand ) {
          if( sup_p > 0 ) {
            sam_c[i,j] = ifelse(j == sup, as.numeric(sup_p*n), as.numeric(sam_n[i,j]+p[j]/(1-p[sup])*c_adj*n))
            # 만약 j열이 우세정당이라면, 득표율 추정량에서 보정크기를 빼서 계산하고,
            # 열세정당이라면, 보정크기를 열세정당들의 지지율에 비례하여 배분해 더해줌.
          }
          else {sam_c[i,j] = sam_n[i,j]}                     # sup_p<0이면, 기존의 sam_n을 그대로씀 
        }
      }
      
      else { 
        for ( j in 1:n_cand ) {sam_c[i,j] = sam_n[i,j]}      # 수도권이 아닌 경우, 열세자 보정 안함
      }
    }
  
  return(sam_c)                                              # 보정된 표본 수
}

  
### 1-(4) result_whole(sam_n) : 전국단위 선거구 수 추정 결과

'result_whole'= function(sam_n){

  n_dist = nrow(sam_n)                                           # 전국 선거구 수 : 253
  n_cand = ncol(sam_n)                                           # 전국 정당 수 : 24
  
  # 당선확률을 구하고, 그것을 통해 의석 수 신뢰구간을 구함
  est_prob1 = prob(sam_n, n_dist, n_cand, n_sim)                 # 253*24
  result1_1 = method(est_prob1, n_dist, n_cand)                  # 24*3
  rownames(result1_1) = colnames(sam_n)
  
  # 무소속을 합치는 작업
  result_98 = apply(result1_1[c("98_1","98_2","98_3"),],2,sum)   # 무소속 합치기
  result1_2 = rbind(result1_1[1:21,], result_98)                 # 22개 = 정당 21개 + 무소속 1개 
  
  # 결과표에 들어갈 구분값 네이밍
  cl = rep("A",22)                                               # 전국 : "A"
  dist = rep("00",22)                                            # 전국 : 00
  dist.kor = rep("전국",22)
  
  # output 
  out_f1 = cbind(cl,dist,dist.kor, sbs.c,name_c, result1_2)      # 22*8 result output
  
  return(out_f1)
}


### 1-(5) result_region(j, sam_n6) : 6개 지역그룹별, 선거구 수 추정 결과
# sam_n6 = cbind(area6, sam_n) : 6개 지역 그룹화한 것과 표본을 합친 데이터

'result_region'= function(j, sam_n6){
  
  reg6 = sam_n6[area6==j, -1]                                    # 각 area6 code에 맞는 행만 추출
  n_dist6 = nrow(reg6)                                           # 서울이면 49개가 n_dist6로 됨
  n_cand = ncol(sam_n )                                          # 전국 정당 수 : 24
  
  # 당선확률을 구하고, 그것을 통해 의석 수 신뢰구간을 구함
  est_prob2 = prob(reg6, n_dist6, n_cand, n_sim)                 # 253*24
  result2_1 = method(est_prob2, n_dist6, n_cand)                 # 24*3
  rownames(result2_1) = colnames(reg6)
  
  # 무소속을 합치는 작업
  result_98 = apply(result2_1[c("98_1","98_2","98_3"),],2,sum)   # 무소속 합치기
  result2_2 = rbind(result2_1[1:21,], result_98)                 # 22개 = 정당 21개 + 무소속 1개 
  
  # 결과표에 들어갈 구분값 네이밍
  cl = rep("C", 22)                                              # 지역별 : "C"
  dist = rep(j, 22)                                              # 지역별 : 81,82,83,84,85,86
  area6.kor = c("서울","경인","충청","호남","영남","강원/제주")  # 6개 권역명
  dist.kor= rep(area6.kor[j-80], 22)
  
  # output 
  out_f2 = cbind(cl,dist,dist.kor, sbs.c,name_c, result2_2)      # 22*8 result output
  
  return(out_f2)
}


### 1-(6) result_capital(sam_n) : 수도권 선거구 수 추정 결과

'result_capital'= function(sam_n){
  
  n_dist = nrow(sam_n)                                           # 수도권 선거구 수 : 
  n_cand = ncol(sam_n)                                           # 전국 정당 수 : 24
  
  # 당선확률을 구하고, 그것을 통해 의석 수 신뢰구간을 구함
  est_prob3 = prob(sam_n, n_dist, n_cand, n_sim)                 # 253*24 추정 당선확률
  result3_1 = method(est_prob3, n_dist, n_cand)                  # 24*3 : 24개 정당 의석 수 신뢰구간과 추정치
  rownames(result3_1) = colnames(sam_n)
  
  # 무소속을 합치는 작업
  result_98 = apply(result3_1[c("98_1","98_2","98_3"),],2,sum)   # 무소속 합치기
  result3_2 = rbind(result3_1[1:21,], result_98)                 # 22개 = 정당 21개 + 무소속 1개 
  
  # 결과표에 들어갈 구분값 네이밍
  cl = rep("B",22)                                               # 수도권 : "B"
  dist = rep("71",22)                                            # 수도권 : 71
  dist.kor = rep("수도권",22)
  
  # output 
  out_f3 = cbind(cl,dist,dist.kor, sbs.c,name_c, result3_2)      # 22*8 result output
  
  return(out_f3)
}



##########################################################################################################
################# 2. 패키지 로드 및 데이터 불러오기 & 지역코드 변수 생성, 정당명 네이밍 ##################
##########################################################################################################

# dirichlet 난수생성을 위한 패키지
library(MCMCpack)                                       

# 출구조사 득표율 데이터 불러오기
setwd("C:/Users/USER/Desktop/총선(2016)")
data = read.csv("R_sbs.csv", header=T)                       # 253 obs, 28 variables.

# 표본 생성 (득표율에 비례하여 N개수의 20%만큼 표본 개수를 산출한것)
# (여기선 아래의 area4, area6, code, sbs.c, name_c 변수를 만들기 위한 용도)
sam_n = round(data[ ,5:ncol(data)]/100 * data[ ,4]*0.20)     # 253 obs, 24 variables.
colnames(sam_n) = substr(colnames(sam_n), 2, 5)              # 열 이름 앞의 X를 뺌
n_dist = nrow(sam_n)                                         # 선거구 수 : 253
n_cand = ncol(sam_n)                                         # 정당 수 : 24

# 지역코드 변수 생성 (data의 CODE의 앞의 2개 숫자)
area_code = NULL
for ( i in 1:n_dist ) { 
  area_code[i] = substr(data[i,2],1,2) 
}

# 4개의 지역으로 그룹화 (1:수도권, 2:영남, 3:호남, 4:나머지)
area4 = NULL
for ( i in 1:n_dist ) {
  if (area_code[i]=="11" || area_code[i]=="41" || area_code[i]=="28")
  { area4[i] = 1 }   # 11(서울), 41(경기), 28(인천)
  else if (area_code[i]=="26" || area_code[i]=="27" || area_code[i]=="31" || area_code[i]=="47" || area_code[i]=="48")
  { area4[i] = 2 }   # 26(부산), 27(대구), 31(울산), 47(경북), 48(경남)
  else if (area_code[i]=="29" || area_code[i]=="45" || area_code[i]=="46")
  { area4[i] = 3 }   # 29(광주), 45(전북), 46(전남)
  else
  { area4[i] = 4}    # 30(대전), 42(강원), 43(충북), 44(충남), 49(제주)
}

# 6개의 지역으로 그룹화 (81:서울, 82:경인, 83:충청, 84:호남, 85:영남, 86:강원/제주)
area6 = NULL
for ( i in 1:n_dist ) {
  if (area_code[i] == "11") { area6[i] = 81 }  # 서울
  else if (area_code[i]=="41" || area_code[i]=="28") { area6[i] = 82 }  # 경인
  else if (area_code[i]=="30" || area_code[i]=="43" || area_code[i]=="44" ) { area6[i] = 83  }  # 충청
  else if (area_code[i]=="29" || area_code[i]=="45" || area_code[i]=="46" ) { area6[i] = 84  }  # 호남
  else if (area_code[i]=="26" || area_code[i]=="27" || area_code[i]=="31" || area_code[i]=="47" || area_code[i]=="48") { area6[i] = 85 }  # 영남
  else if (area_code[i]=="42" || area_code[i]=="49") { area6[i] = 86 }  # 강원/제주
}

# 결과표에 들어갈 정당 기호(sbs.c) 및 정당명(name_c) 네이밍
code = c(colnames(sam_n[,1:21]),"98")                     # 21개 정당과 무소속 열 번호를 code변수에 넣음
sbs.c = NULL; name_c = NULL
for (i in 1:22){                                          # sbs.c와 name_c에 숫자와 정당명넣기.
  if(code[i]=="1"){sbs.c[i]="01"; name_c[i] = "새누리당"}
  else if(code[i]=="2"){sbs.c[i]="02"; name_c[i] = "더불어민주당"}
  else if(code[i]=="3"){sbs.c[i]="03"; name_c[i] = "국민의당"}
  else if(code[i]=="4"){sbs.c[i]="04"; name_c[i] = "정의당"}
  else if(code[i]=="5"){sbs.c[i]="05"; name_c[i] = "기독자유당"}
  else if(code[i]=="6"){sbs.c[i]="06"; name_c[i] = "민주당"}
  else if(code[i]=="7"){sbs.c[i]="07"; name_c[i] = "가자코리아"}
  else if(code[i]=="10"){sbs.c[i]="10"; name_c[i] = "고용복지연금선진화연대"}
  else if(code[i]=="11"){sbs.c[i]="11"; name_c[i] = "공화당"}
  else if(code[i]=="15"){sbs.c[i]="15"; name_c[i] = "노동당"}
  else if(code[i]=="16"){sbs.c[i]="16"; name_c[i] = "녹색당"}
  else if(code[i]=="18"){sbs.c[i]="18"; name_c[i] = "민중연합당"}
  else if(code[i]=="19"){sbs.c[i]="19"; name_c[i] = "복지국가당"}
  else if(code[i]=="22"){sbs.c[i]="22"; name_c[i] = "친반통일당"}
  else if(code[i]=="24"){sbs.c[i]="24"; name_c[i] = "통일한국당"}
  else if(code[i]=="25"){sbs.c[i]="25"; name_c[i] = "한국국민당"}
  else if(code[i]=="26"){sbs.c[i]="26"; name_c[i] = "한나라당"}
  else if(code[i]=="17"){sbs.c[i]="17"; name_c[i] = "대한민국당"}
  else if(code[i]=="20"){sbs.c[i]="20"; name_c[i] = "진리대한당"}
  else if(code[i]=="21"){sbs.c[i]="21"; name_c[i] = "친반통합"}
  else if(code[i]=="23"){sbs.c[i]="23"; name_c[i] = "친반평화통일당"}
  else if(code[i]=="98"){sbs.c[i]="98"; name_c[i] = "무소속"}
}

# 시뮬레이션 반복 횟수 
n_sim = 10000



##########################################################################################################
############################################ 3. 열세자 무보정 ############################################
##########################################################################################################


### 3-(1) 열세자 무보정 중 전국단위
sam_n = round(data[ ,5:ncol(data)]/100 * data[ ,4]*0.20)     # 253 obs, 24 variables.
colnames(sam_n) = substr(colnames(sam_n), 2, 5)              # 열 이름 앞의 X를 뺌
output3_1 = result_whole(sam_n)                              # 22*8 result output


### 3-(2) 열세자 무보정 중 6개 지역권 별
sam_6 = cbind(area6, sam_n)
output3_2 = result_region(81, sam_6)
for (k in 82:86) {
  output3_2 <- rbind(output3_2, result_region(k, sam_6))     # 132*8 : 22개 정당이 6개 있으니 132행
}                                                            


### 3-(3) 열세자 무보정 중 수도권(71)
sam_71 = rbind(sam_6[area6==81,], sam_6[area6==82,])[,-1]    # 81(서울), 82(경인)
output3_3 <- result_capital(sam_71)


### 결과 종합 (전국 "A", 6개 지역권 "C", 수도권 "B")
sbs.out1 = rbind(output3_1, output3_2, output3_3)
write.table(sbs.out1, file = "열세자무보정.csv", row.names=FALSE, col.names=TRUE, quote = FALSE, sep=",")



##########################################################################################################
##################################### 4. 열세자 보정(당시 야권 우세) #####################################
##########################################################################################################


case = 1         # 야권 우세
c_adj1 = 0.005   # 보정크기 alpha

### 4-(1) 열세자 무보정 중 전국단위
sam_n = round(data[ ,5:ncol(data)]/100 * data[ ,4]*0.20)              # 253 obs, 24 variables.
colnames(sam_n) = substr(colnames(sam_n), 2, 5)                       # 열 이름 앞의 X를 뺌
sam_c1 = round(re_sam(case, c_adj1, n_dist, n_cand, sam_n, area4))    # 253*24 보정된 표본 
colnames(sam_c1) = colnames(sam_n)                                    # 열 이름 네이밍
output4_1 = result_whole(sam_c1)                                      # 22*8 result output


### 4-(2) 열세자 무보정 중 6개 지역권 별
sam_6_c1 = cbind(area6, sam_c1)
output4_2 = result_region(81, sam_6_c1)
for (k in 82:86) {
  output4_2 <- rbind(output4_2, result_region(k, sam_6_c1))           # 132*8 : 22개 정당이 6개 있으니 132행
}                                                            


### 4-(3) 열세자 무보정 중 수도권(71)
sam_71_c1 = rbind(sam_6_c1[area6==81,], sam_6_c1[area6==82,])[,-1]    # 81(서울), 82(경인)
output4_3 <- result_capital(sam_71_c1)


### 결과 종합 (전국 "A", 6개 지역권 "C", 수도권 "B")
sbs.out2 = rbind(output4_1, output4_2, output4_3)
write.table(sbs.out2, file = "야권우세.csv", row.names=FALSE, col.names=TRUE, quote = FALSE, sep=",")



##########################################################################################################
################################### 5. 열세자 보정(당시 새누리당 우세) ###################################
##########################################################################################################


case = 2         # 새누리당 우세
c_adj2 = 0.005   # 보정크기 alpha

### 5-(1) 열세자 무보정 중 전국단위
sam_n = round(data[ ,5:ncol(data)]/100 * data[ ,4]*0.20)              # 253 obs, 24 variables.
colnames(sam_n) = substr(colnames(sam_n), 2, 5)                       # 열 이름 앞의 X를 뺌
sam_c2 = round(re_sam(case=2, c_adj2, n_dist, n_cand, sam_n, area4))  # 253x24짜리 보정된 표본 
colnames(sam_c2) = colnames(sam_n)                                    # 열 이름 네이밍
output5_1 = result_whole(sam_c2)                                      # 22*8 result output


### 5-(2) 열세자 무보정 중 6개 지역권 별
sam_6_c2 = cbind(area6, sam_c2)
output5_2 = result_region(81, sam_6_c2)
for (k in 82:86) {
  output5_2 <- rbind(output5_2, result_region(k, sam_6_c2))           # 132*8 : 22개 정당이 6개 있으니 132행
}                                                            


### 5-(3) 열세자 무보정 중 수도권(71)
sam_71_c2 = rbind(sam_6_c2[area6==81,], sam_6_c2[area6==82,])[,-1]    # 81(서울), 82(경인)
output5_3 <- result_capital(sam_71_c2)


### 결과 종합 (전국 "A", 6개 지역권 "C", 수도권 "B")
sbs.out3 = rbind(output5_1, output5_2, output5_3)
write.table(sbs.out3, file = "새누리당우세.csv", row.names=FALSE, col.names=TRUE, quote = FALSE, sep=",")
