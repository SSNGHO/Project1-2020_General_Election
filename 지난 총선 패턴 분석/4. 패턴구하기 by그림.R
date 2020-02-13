##### 패키지 불러오기 #####
library(tidyverse)
library(ggplot2)
setwd("C:/Users/kangm/Desktop")

##### 데이터 불러오기 ######
data <- read.csv(file="C:/Users/kangm/Desktop/작업용/임요한/2020 총선/pattern.csv", header=TRUE)
data <- data[,c(1:3,7:14)]
colnames(data) <- c("id", "original_id", "region_code", "15","16","17","18","19","20", "pattern1", "pattern2")
data$id <- as.character(data$id); data$original_id <- as.character(data$original_id)
data$region_code <- as.factor(data$region_code)
data$pattern1 <- as.factor(data$pattern1); data$pattern2 <- as.factor(data$pattern2)

##### 그래프 함수 정의 #####
drawplot <- function(data, province="서울") {
  
  ##### province(한글)을 숫자로 변환 -> region #####
  region_table <- data.frame(num = 1:17, name = c("서울","부산","대구","인천","광주","대전",
                                                  "울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  region <- as.factor(match(province, region_table$name))
  
  ##### 입력된 province와 일치하는 값을 찾아 plot.data와 plot.gatherdata 생성 #####
  plot.data <- data[data$region_code %in% region, ]			## region 값과 일치하는 데이터 불러오기(plot.data) ##
  dist <- as.data.frame(table(plot.data$pattern1))				## 불러온 값에서 pattern의 분포 ##
  colnames(dist) <- c("pattern", "bin")
  dist$weight <- dist$bin / sum(dist$bin)				## pattern의 weight 구하기 (빈도 / 전체) ##
  plot.data$bin <- dist[match(plot.data$pattern1, dist$pattern), 2]		## plot.data에 각 pattern에 맞는 빈도와 weight 추가 ##
  plot.data$weight <- dist[match(plot.data$pattern1, dist$pattern), 3]
  plot.data <- plot.data[!duplicated(plot.data$pattern1),]			## 중복되는 pattern은 모두 제거 (선거구 기준이 아니라 pattern 기준으로 그래프를 그릴거니까!)  ##
  plot.data$label <- paste(plot.data$pattern1, "   (", plot.data$bin, ")")		## pattern 과 빈도수를 합쳐서 label로 쓸 변수 생성 ##
  plot.data$label <- factor(plot.data$label, levels=plot.data$label[ order(plot.data$bin, decreasing=T)])	## 생성된 label 변수에 순서 부여 (그래야 그래프 그릴 대 순서 정렬이 됨) ##
  plot.gatherdata <- as_tibble(plot.data) %>%
    gather("15","16","17","18","19","20", key="year", value="party") %>%
    arrange(region_code, desc(bin), id, year)
  
  ##### 그래프 제목 정하기, 각 패턴 별 weight(=패턴 빈도 / 전체 빈도)를 이용해서 alpha값(=scale10)과 size값(scale1) 만들기 #####
  title <- province[1]									## 모든 province 지역을 paste 하기
  if (length(province)>1) { for (i in 2:length(province)) title <- paste0(title, province[i])}
  title <- paste(title, "지역 최근 국회의원 선거 당선 정당 추세")				## 마지막으로  "지역 최근 국회의원 선거 당선 정당 추세" paste 해서 title 완성
  x <- plot.gatherdata$weight	
  if (min(x)==max(x)) {plot.gatherdata$scale10 <- 5.5; plot.gatherdata$scale1 <- 0.75} else {	## scale 10 : weight를 1과 10 사이의 숫자로 선형적으로 퍼뜨려 놓음
    plot.gatherdata$scale10 <- (x-min(x))/(max(x)-min(x))*10 + (max(x)-x)/(max(x)-min(x))	## scale 1 : weight를 0.5와 1 사이의 숫자로 선형적으로 퍼뜨려 놓음
    plot.gatherdata$scale1 <- (x-min(x))/(max(x)-min(x))*0.5 + (max(x)-x)/(max(x)-min(x))*1}	## min과 max가 같을 경우(weight 값이 하나 밖에 없음) 임이의 숫자 넣어주기
  
  ##### ggplot 그리기 #####
  ggplot(plot.gatherdata, aes(x=year, y=party, group=plot.gatherdata$label))+
    theme_classic()+ labs(title=title, y="당선 정당", x="") +
    scale_y_continuous(limits=c(1,4))+ 
    scale_x_discrete(labels=c("15대\n(1996)", "16대\n(2000)","17대\n(2004)","18대\n(2008)","19대\n(2012)","20대\n(2016)"))+
    theme(legend.position="right", legend.title.align=0.5, legend.title=element_text(face=2, size=20), legend.text=element_text(size=15),
          axis.text=element_text(size=12), axis.title=element_text(face=2, size=20),  plot.title=element_text(hjust=0.5, face=2, size=30)) + 
    guides(colour = guide_legend("패턴   (빈도)", ncol = 1, hjust=0.5))+
    geom_line(aes(color=plot.gatherdata$label), size=plot.gatherdata$scale10, alpha=plot.gatherdata$scale1)
}

##### 각 지역에 대한 그래프 png파일로 저장하기 #####
png(filename="서울.png", width=720); drawplot(data, province="서울"); dev.off()
png(filename="부산.png", width=720); drawplot(data, province="부산"); dev.off()
png(filename="대구.png", width=720); drawplot(data, province="대구"); dev.off()
png(filename="인천.png", width=720); drawplot(data, province="인천"); dev.off()
png(filename="광주.png", width=720); drawplot(data, province="광주"); dev.off()
png(filename="대전.png", width=720); drawplot(data, province="대전"); dev.off()
png(filename="울산.png", width=720); drawplot(data, province="울산"); dev.off()
png(filename="세종.png", width=720); drawplot(data, province="세종"); dev.off()
png(filename="경기.png", width=720); drawplot(data, province="경기"); dev.off()
png(filename="강원.png", width=720); drawplot(data, province="강원"); dev.off()
png(filename="충북.png", width=720); drawplot(data, province="충북"); dev.off()
png(filename="충남.png", width=720); drawplot(data, province="충남"); dev.off()
png(filename="전북.png", width=720); drawplot(data, province="전북"); dev.off()
png(filename="전남.png", width=720); drawplot(data, province="전남"); dev.off()
png(filename="경북.png", width=720); drawplot(data, province="경북"); dev.off()
png(filename="경남.png", width=720); drawplot(data, province="경남"); dev.off()
png(filename="제주.png", width=720); drawplot(data, province="제주"); dev.off()