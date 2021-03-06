## (공부)
# 허명회(2008). 선거여론조사결과의 베이즈적 해석 
library(lattice); library(coda); library(MCMCpack)
## 이명박 vs 박근혜 (2절)
n <- 700; nrepeat <- 1000000
p <- c(0.251, 0.221, 0.528)
alpha <- n*p + 1
theta <- rdirichlet(nrepeat,alpha)
round(mean((theta[,1]>theta[,2])),3)
## 이명박 vs 박근혜 (3절)
p <- c(0.251, 0.221, 0.207, 0.321)
alpha <- n*p + 1
theta <- rdirichlet(nrepeat,alpha)
round(mean((theta[,1]>theta[,2])&(theta[,1] > theta[,3])),3)
round(mean((theta[,2] > theta[,3])),3)


###############################################################################
rm(list=ls())
setwd("C:/Users/USER/Desktop/총선(2016)")
library(MCMCpack) # dirichlet 난수생성을 위한 패키지


##################################################################################################
# (function) 시뮬레이션을 통한 당선 확률 계산 함수 
# :prob(sam_n, n_dist, n_cand, n_sim)
###################################################################################################
'prob' = function(sam_n, n_dist, n_cand, n_sim){
  result = matrix(0, n_dist, n_cand)                  # 253x24 zero matrix
  for ( i in 1:n_dist ) {
    rand = rdirichlet(n_sim, as.matrix(sam_n[i,]+1) ) # 10000x24 matrix가 rand에 저장
    for ( j in 1:n_sim){
      max_r = which( rand[j,]==max(rand[j,]) )        # rand의 각 행에서 max인 열번호를 max_r에 저장
      result[i,max_r] = result[i,max_r]+1             # result의 i행의 max_r열을 1씩 더해주는걸 반복
    }
  }
  return(result/n_sim)                                # n_sim번 반복한것의 평균을 냄
}
# sam_n : 표본수 계산한 데이터
# n_dist : 253  (선거구 수)
# n_cand : 24 (후보자 수)
# n_sim : 10000
# rdrichlet(n,alpha) - n : Number of random vectors to generate.
#                  alpha : Vector or matrix containing shape parameters.


###################################################################################################
# (function) 의석수 추정: 
# 시뮬레이션으로 계산된 각 지역구에서의 후보자의 당선확률을 직접 이용, 99%신뢰수준, 신뢰계수=3
###################################################################################################
'method' = function(est_p, n_dist, n_cand){            # est_p : 추정 득표율 (253 x 24)
  sum_p1 = apply(est_p, 2, sum)                        # est_p를 각 열에 대해 sum
  sum_var1 = apply(est_p*(1-est_p), 2, sum)            # p(1-p)를 각 열에 대해 sum
  sum_cumul=NULL
  for ( i in 1:n_cand ) {
    sum_cumul[i] = sum(sum_p1[1:i])                    # sum_p1의 순차적 누적합을 sum_cumul에 저장
  }
  sum_cum = round(sum_cumul)                           # 반올림
  
  est = NULL
  est[1] = sum_cum[1]                                  
  for ( i in 2:n_cand ) {
    est[i]=sum_cum[i]-sum_cum[i-1]                     # 순차적 누적합의 각 부분들을 계산
  }                                                    # 결국, sum_p1의 정수형으로 됨
  l_b=NULL
  for( i in 1:n_cand) {                                # floor : 소수점 버림
    l_b[i] = max( 0, floor(sum_p1[i]-3*sqrt(sum_var1[i])) )
  }
  u_b = ceiling( sum_p1 + 3*sqrt(sum_var1) )           # ceiling : 올림
  return(cbind(l_b,u_b,est))                           # lower, upper, est 구해줌
}


###################################################################################################
# (function) 열세자효과 고려하여 표본자료를 재생성 : 
# re_sam(case#우세당(1:야권, 2:새누리당)#, c_adj#보정크기#, n_dist, n_cand, sam_n)
###################################################################################################
're_sam' = function(case, c_adj, n_dist, n_cand, sam_n, area){
  sam_c = matrix(0, n_dist, n_cand)                      # 253 x 24 zero matrix 생성
  for ( i in 1:n_dist ) {
    if ( area[i] == 1) {                                 # area는 선거구를 4개의 지역으로 나눈것 (여기선 서울,경인) 
      n = sum(sam_n[i,])                                 # 첫번째 선거구 표본수   ???
      p = sam_n[i,]/n                                    # 득표율 추정량
      
      if( case == 1) { sup = which.max(sam_n[i,2:4])+1 } # 야권인 경우 sup=가장 득표율 높은 당의 열 번호
      else if ( case == 2 ) { sup=1 }                    # 새누리당인 경우 sup=1
      
      sup_p = p[sup] - c_adj                             # 우세정당 과다예측 방지
      
      for ( j in 1:n_cand ) {
        if( sup_p > 0 ) {
          sam_c[i,j] = ifelse(j == sup, as.numeric(sup_p*n), as.numeric(sam_n[i,j] + p[j]/(1-p[sup])*c_adj*n))
          # 열세정당 원래 계산한거(sam_n) + 선거구 표본총합*보정계수*(1-우세정당)에서 비율만큼 더해줌  ????
        }
        else {sam_c[i,j]=sam_n[i,j] }
      }
    }
    else { 
      for ( j in 1:n_cand ) {
        sam_c[i,j]=sam_n[i,j]
      }
    }
    
  }
  return(sam_c)                        # 보정된 표본 수
}

###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################

# (아마 득표율) 데이터 불러오기
data = read.csv("R_sbs.csv",header=T) # 253 obs, 28 variable 
View(data)
###############################################################################################
# 데이터 생성

# 표본 생성 ( (아마 득표율)에 비례하여 N개수의 20%만큼 표본 개수를 산출한것 ) ?????
sam_n = round(data[,5:ncol(data)] /100 * data[,4] /5) # 253 obs, 24 variables.
# 열 이름 앞의 X를 뺌
colnames(sam_n) = substr(colnames(sam_n), 2, 5)

# 위에서 만든 functionr에 들어가는 변수명임
n_dist = nrow(sam_n) # 조사구 수 :253
n_cand = ncol(sam_n) # 후보자 수 :24

# 지역코드 변수 생성 (CODE의 앞의 2개 숫자)
area_code = NULL
for ( i in 1:n_dist ) { area_code[i] = substr(data[i,2],1,2) }
unique(area_code)

# 4개의 지역으로 그룹화 (1:서울,경기,인천  2:영남  3:호남  4:나머지)
area = NULL
for ( i in 1:n_dist ) {
  if( area_code[i] == "11" || area_code[i] == "41" || area_code[i] == "28" ) { area[i] = 1 } # 서울, 경인
  else if (area_code[i] == "26" || area_code[i] == "27" || area_code[i] == "31" || area_code[i] == "47" || area_code[i] == "48") { area[i] = 2 } #영남
  else if (area_code[i] == "29" || area_code[i] == "45" || area_code[i] == "46" ) { area[i] = 3 } # 호남
  else {area[i] = 4}
}

# 반복 횟수 
n_sim = 10000


##############################################################################################
#1. 열세자 무보정
##############################################################################################
est_prob1 = prob(sam_n, n_dist, n_cand, n_sim)  # [1:253, 1:24]짜리 추정 득표율
result1 = method(est_prob1, n_dist, n_cand)     # [1:24,1:3] : 24명 후보자에 대해 신뢰구간과 추정치
rownames(result1) = colnames(sam_n)

result_98 = apply(result1[c("98_1","98_2","98_3"),],2,sum)  #무소속 합치기
result2 = rbind(result1[1:21,],result_98)                   #22개= 후보자있는 정당 21개+ 무소속 1개 

code = c(colnames(sam_n[,1:21]),"98")                     # 21개 정당과 무소속 열 이름 네이밍
sbs.c = NULL; name_c = NULL
for (i in 1:22){                                          # sbs.c와 name_c에 이름 새겨넣는 작업
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

cl = rep("A",22)     # 전국/ 권역 구분값
dist = rep("00",22)
dist.kor = rep("전국",22)
out_f = cbind(cl,dist,dist.kor,sbs.c,name_c,result2)
# out_f에다가 구분코드들과 lower, upper, est를 넣음


##############################################################################################
#2. 열세자 보정(야권-더민주,국민,정의당 우세)
##############################################################################################
# 초기치
c_adj1=0.005                                    #보정 alpha = c_adj
########### 조정치 변경 시 #############
#c_adj1=0.01
#c_adj1=0.02
#c_adj1=0.025
#c_adj1=0.03
########################################

sam_c1 = round(re_sam(case=1, c_adj1, n_dist, n_cand, sam_n, area))  # 253x24짜리 보정된 표본 
est_prob2 = prob(sam_c1, n_dist, n_cand, n_sim)                      # [1:253, 1:24]짜리 추정 득표율
result1 = method(est_prob2, n_dist, n_cand)                          # [1:24,1:3] : 24명 후보자에 대해 신뢰구간과 추정치
rownames(result1) = colnames(sam_n)

result_98 = apply(result1[c("98_1","98_2","98_3"),],2,sum)           # 무소속 합치기
result2 = rbind(result1[1:21,],result_98)                            # 22개 = 후보자있는 정당 21개+ 무소속 1개
 
code = c(rownames(result1[1:21,]),"98")
sbs.c = NULL; name_c = NULL
for (i in 1:22){
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

cl= rep("A",22)
dist = rep("00",22)
dist.kor= rep("전국",22)
out_f2 = cbind(cl,dist,dist.kor ,sbs.c,name_c,result2)

out_f2_0.01 = out_f2

# c_adj1 조정할 경우 output (주의: 이름 잊지말고 바꿀 것) 
#out_f2_0.01 = out_f2
#out_f2_0.02 = out_f2
#out_f2_0.025 = out_f2
#out_f2_0.03 = out_f2

##############################################################################################
#3. 열세자 보정(새누리당 우세)
##############################################################################################
# 초기치
c_adj2=0.005
########### 조정치 변경 시 #############
#c_adj2=0.005
#c_adj2=0.005
#c_adj2=0.005
########################################

sam_c2 = round(re_sam(case=2, c_adj2, n_dist, n_cand, sam_n, area))
est_prob2 = prob(sam_c2, n_dist, n_cand, n_sim)
result1 = method(est_prob2, n_dist, n_cand)
rownames(result1) = colnames(sam_n)
result_98 = apply(result1[c("98_1","98_2","98_3"),],2,sum)
result2 = rbind(result1[1:21,],result_98)

code = c(rownames(result1[1:21,]),"98")
sbs.c = NULL; name_c = NULL
for (i in 1:22){
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

cl= rep("A",22)
dist = rep("00",22)
dist.kor= rep("전국",22)
out_f3 = cbind(cl,dist,dist.kor, sbs.c,name_c,result2)

out_f3_0.005 = out_f3

# c_adj2 조정할 경우 output (주의: 이름 잊지말고 바꿀 것) 
#out_f3_0.005 = out_f3
#out_f3_0.005 = out_f3






##############################################################################
# 6개의 지역으로 그룹화
area6 = NULL
for ( i in 1:n_dist ) {
  if( area_code[i] == "11") { area6[i] = 81 } # 서울
  else if (area_code[i] == "41" || area_code[i] == "28") { area6[i] = 82 } # 경인
  else if (area_code[i] == "30" || area_code[i] == "43" || area_code[i] == "44" ) { area6[i] =83  } # 충청
  else if (area_code[i] == "29" || area_code[i] == "45" || area_code[i] == "46" ) { area6[i] =84  } # 호남
  else if (area_code[i] == "26" || area_code[i] == "27" || area_code[i] == "31" || area_code[i] == "47" 
           || area_code[i] == "48") { area6[i] = 85 } # 영남
  else if (area_code[i] == "42" || area_code[i] == "49") { area6[i] = 86 } # 강원/제주
}
##################################################################################  
'result_reg'= function(j, sam_n6){
  
  reg6 = sam_n6[area6==j,-1]                           # 각 area6 code에 맞는 행만 추출
  n_dist6 = nrow(reg6)                                 # 서울이면 49개가 n_dist6로 됨
  
  est_prob1 = prob(reg6, n_dist6, n_cand, n_sim)       # 당선확률 계산
  result1 = method(est_prob1, n_dist6, n_cand)         # 의석 수 추정
  rownames(result1) = colnames(reg6)
  result_98 = apply(result1[c("98_1","98_2","98_3"),],2,sum)  # 무소속해당열 합
  result2 = rbind(result1[1:21,],result_98)
  
  code = c(rownames(result1[1:21,]),"98")
  sbs.c = NULL; name_c = NULL
  for (i in 1:22){
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
  
  area6.kor=c("서울","경인","충청","호남","영남","강원/제주")   # 6개 권역명
  
  
  cl= rep("C",22)     # 전국/ 권역 구분값
  
  dist = rep(j,22)
  dist.kor= rep(area6.kor[j-80],22)
  
  out_f = cbind(cl,dist,dist.kor,sbs.c,name_c,result2)  #[1:22,1:8]
  
  return(out_f)
}

##################################################################################
# 1. 열세자 무보정
# list로 나타내기
# result.ls <- lapply(81:86, function(i) result_reg(i,sam_n6))
##################################################################################

sam_n6 = cbind(area6,sam_n)

result.6 <- result_reg(81,sam_n6)
for (i in 82: 86){result.6 <- rbind(result.6, result_reg(i,sam_n6))}  #[1:132,1:8]  22개 후보가 6개가 있으니 132

# 수도권(71 = 서울(81)+경인(82)) 예측 결과
sam_71 = rbind(sam_n6[area6==81,],sam_n6[area6==82,])[,-1]  # [1:122,1:24]
n_dist71 = nrow(sam_71)
est_prob3 = prob(sam_71, n_dist71, n_cand, n_sim)  # [1:122, 1:24]
result1 = method(est_prob3, n_dist71, n_cand)     # [1:24,1:3]
rownames(result1) = colnames(sam_n)

result_98 = apply(result1[c("98_1","98_2","98_3"),],2,sum)  # 무소속해당열 합
result2 = rbind(result1[1:21,],result_98)       # [1:22,1:3]

code = c(rownames(result1[1:21,]),"98")
sbs.c = NULL; name_c = NULL
for (i in 1:22){
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

cl = rep("B",22)     # 전국/ 권역 구분값
dist = rep("71",22)
dist.kor= rep("수도권",22)
out_f71 = cbind(cl,dist,dist.kor,sbs.c,name_c,result2)      #[1:22,1:8]

###################################################################################
# 2. 열세자 보정(야권우세)
###################################################################################
sam_c1 = round(re_sam(case=1, c_adj=0.005, n_dist, n_cand, sam_n, area))
colnames(sam_c1) = colnames(sam_n)
sam_c61 = cbind(area6, sam_c1)

result.c61 <- result_reg(81, sam_c61)
for (i in 82: 86){result.c61<- rbind(result.c61,result_reg(i,sam_c61))} #[1:132,1:8]

# 수도권(71) 예측 결과
sam_c71 = rbind(sam_c61[area6==81,], sam_c61[area6==82,])[,-1]
n_distc71 = nrow(sam_c71)
est_prob3 = prob(sam_c71, n_distc71, n_cand, n_sim)  # [1:122, 1:24]
result1 = method(est_prob3, n_distc71, n_cand)     # [1:24,1:3]
rownames(result1) = colnames(sam_n)

result_98 = apply(result1[c("98_1","98_2","98_3"),],2,sum)  #무소속해당열 합
result2 = rbind(result1[1:21,],result_98)   #[1:22,1:3]

code = c(rownames(result1[1:21,]),"98")
sbs.c = NULL; name_c = NULL
for (i in 1:22){
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

cl= rep("B",22)     # 전국/ 권역 구분값
dist = rep("71",22)
dist.kor= rep("수도권",22)
out_fc71 = cbind(cl,dist,dist.kor,sbs.c,name_c,result2)

##############################################################################################
#3. 열세자 보정(새누리당 우세)
##############################################################################################
sam_c2 = round(re_sam(case=2, c_adj=0.005, n_dist, n_cand, sam_n, area))
colnames(sam_c2) = colnames(sam_n)
sam_c62 = cbind(area6,sam_c2)

result.c62 <- result_reg(81, sam_c62)
for (i in 82:86){result.c62 <- rbind(result.c62,result_reg(i,sam_c62))}

# 수도권(71) 예측 결과
sam_c72 = rbind(sam_c62[area6==81,],sam_c62[area6==82,])[,-1]
n_distc72 = nrow(sam_c72)
est_prob3 = prob(sam_c72, n_distc72, n_cand, n_sim)  # [1:122, 1:24]
result1 = method(est_prob3, n_distc72, n_cand)     # [1:24,1:3]
rownames(result1) = colnames(sam_n)

result_98 = apply(result1[c("98_1","98_2","98_3"),],2,sum)  #무소속해당열 합
result2 = rbind(result1[1:21,],result_98)

code = c(rownames(result1[1:21,]),"98")
sbs.c = NULL; name_c = NULL
for (i in 1:22){
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

cl = rep("B",22)     # 전국/ 권역 구분값
dist = rep("71",22)
dist.kor = rep("수도권",22)
out_fc72 = cbind(cl,dist,dist.kor,sbs.c,name_c,result2)

##############################################################################################
# 최종 결과 out
##############################################################################################
# 무보정 결과
sbs.out1 = rbind(out_f, result.6, out_f71)
write.table(sbs.out1, file = "무보정.csv", row.names=FALSE, col.names=TRUE, quote = FALSE, sep=",")

View(out_f)    # out_f :     22 x 8 - 전국(A)
View(result.6) # result.6 : 132 x 8 - 지역별(C) (6개지역 구분에서)
View(out_f71)  # out_f71 :   22 x 8 - 수도권(B)

# 보정 결과
# 1. 야권우세
sbs.outc1 = rbind(out_f2, result.c61, out_fc71)
write.table(sbs.outc1, file = "야권우세.csv", row.names=FALSE, col.names=TRUE, quote = FALSE, sep=",")

# 2. 새누리우세
sbs.outc2 = rbind(out_f3, result.c62, out_fc72)
write.table(sbs.outc2, file = "새누리우세.csv", row.names=FALSE, col.names=TRUE, quote = FALSE, sep=",")
