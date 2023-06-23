library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)
library(RSelenium)

samsung_url <- "https://finance.naver.com/item/main.naver?code=005930"
sk_url <- "https://finance.naver.com/item/main.naver?code=000660"


#최근 분기실적 데이터프레임 추출하기(필요한 데이터만)
get_information_table <- function(url){
  html <- read_html(url, encoding="euc-kr")
  
  get_table <- html %>% 
    html_nodes("div.sub_section") %>% 
    html_table() %>% 
    .[[5]]
  
  get_table <- get_table[,c(1,6:10)]
  rm_row <- get_table[-c(2,16,17,18),]
  t_rm_row <- t(rm_row)
  rownames(t_rm_row) <- NULL
  df <- data.frame(t_rm_row)
  
  names(df) <- NULL
  colnames(df) <- df[1, ]
  df_1 <- df[-1,]
  colnames(df_1)[1] <- "분기"
  
  print(df_1)
  return(df_1)
  
}

#삼성전자와 sk하이닉스의 재무정보 데이터프레임
samsung_table <- get_information_table(samsung_url)
sk_table <- get_information_table(sk_url)


#필요한 종목(매출액,영업이익,당기순이익) numeric으로 바꾸기
sales_ss <- as.numeric(gsub(",","",samsung_table$매출액))
sales_sk <- as.numeric(gsub(",","",sk_table$매출액))

Operating_profit_ss <- as.numeric(gsub(",","",samsung_table$영업이익))
Operating_profit_sk <- as.numeric(gsub(",","",sk_table$영업이익))

Net_profit_ss <- as.numeric(gsub(",","",samsung_table$당기순이익))
Net_profit_sk <- as.numeric(gsub(",","",sk_table$당기순이익))


#필요한 종목(EPS, PER) numeric으로 바꾸기
EPS_ss <- as.numeric(gsub(",","",samsung_table$EPS))
EPS_sk <- as.numeric(gsub(",","",sk_table$EPS))

PER_ss <- as.numeric(samsung_table$PER)
PER_sk <- as.numeric(sk_table$PER)


#매출액, 영업이익, 당기순이익 plot그리기
par(mfrow=c(1,2))

options(scipen = 999)
plot(sales_ss, type='o',ylim=c(0,1000000), 
        xlab="Q",main="삼성전자 분기실적",col = 2)
lines(Operating_profit_ss,type='o',ylim=c(0,1000000),col=3 )
lines(Net_profit_ss,type='o',ylim=c(0,1000000),col=4 )
legend("topright",legend=c("매출액","영업이익","당기순이익"),fill=c(2,3,4),cex=0.6)


plot(sales_sk, type='o', ylim=c(-100000,200000), 
     xlab="Q",main="SK하이닉스 분기실적",col = 2)
lines(Operating_profit_sk,type='o',col=3 )
lines(Net_profit_sk,type='o',col=4 )
legend("topright",legend=c("매출액","영업이익","당기순이익"),fill=c(2,3,4),cex=0.6)


###################################################################
#상위 대기업 2곳을 제외한 나머지 반도체 업종 종목 분석

remDr <- remoteDriver(remoteServerAddr = 'localhost', 
                      port = 4680, 
                      browserName = "chrome") 
remDr$open()

remDr$navigate("https://finance.naver.com/sise/sise_group_detail.naver?type=upjong&no=278")

remDr$screenshot(display = TRUE)

webElem <- remDr$findElements(using = "css","input[checked]")

for(i in 1:length(webElem)) webElem[[i]]$clickElement() #찾아서 클릭

option <- paste("#option", 1:27, sep="")

for(i in c(1,4,6,11,12,25)) {
  webElem <- remDr$findElement(using="css", option[i])
  
  webElem$clickElement()
}
remDr$screenshot(display = TRUE)

element <- remDr$findElement(using="css", "div.item_btn > a")
element$clickElement()
html <- read_html(remDr$getPageSource()[[1]])

semicon_table <- html %>% 
  html_table() %>% 
  .[[3]]
semicon_table

#등락률>0, 거래량 top20 기업 추출
plus <- semicon_table[grep("^\\+", semicon_table$등락률), ]
plus_top20 <- plus[rank(plus$거래량, ties.method = "min") <= 20, ]

#등락률<0, 거래량 top20 기업 추출
minus <- semicon_table[grep("^\\-", semicon_table$등락률), ]
minus_top20 <- minus[rank(minus$거래량, ties.method = "min") <= 20, ]

#거래량 top40
top40 <- rbind(plus_top20,minus_top20)


#거래량 top40의 영업이익증가율과 매출액증가율 분포
top40$영업이익증가율 <- as.numeric(top40$영업이익증가율)
plot(top40$영업이익증가율, main = "영업이익증가율 분포", xlab = "영업이익증가율")
abline(h = 0, col = "red", lty = 2)

top40$매출액증가율 <- as.numeric(top40$매출액증가율)
plot(top40$매출액증가율, main = "매출액증가율 분포", xlab = "매출액증가율")
abline(h = 0, col = "red", lty = 2)

#거래량 top40의 PER과 ROE
top40$PER <- as.numeric(top40$PER)
top40$ROE <- as.numeric(top40$ROE)

plot(top40$ROE, top40$PER, xlab = "ROE", ylab = "PER", main="ROE와 PER의 분포")

#########################################
#추가적으로 네이버증권에서 얻을 수 있는 통계분석
#1)sk하이닉스와 삼성전자 외국인 소진율 비교
samsung_url_chart <- "https://api.finance.naver.com/siseJson.naver?symbol=005930&requestType=1&startTime=19930801&endTime=20230611&timeframe=month"
sk_url_chart <- "https://api.finance.naver.com/siseJson.naver?symbol=000660&requestType=1&startTime=19930801&endTime=20230611&timeframe=month"

#기업의 월별 데이터프레임 만드는 함수
get_corporation_df <- function(url){
  response <- GET(url)
  content <- content(response, "text", encoding = "UTF-8")
  content <- gsub("'", '"', content)
  data <- fromJSON(content)
  chart_df <- data.frame(data)
  
  colnames(chart_df) <- unlist(chart_df[1, ])
  chart_df <- chart_df[-1, ]
  rownames(chart_df) <- 1:nrow(chart_df)
  
  print(chart_df)
  return(chart_df)
  
}

samsung_df <- get_corporation_df(samsung_url_chart)
sk_df <- get_corporation_df(sk_url_chart)

#2016년부터 월별 데이터프레임
samsung_month <- samsung_df[270:nrow(samsung_df),]
sk_month <- sk_df[230:nrow(sk_df),]

#외국인소진율 박스플랏
samsung_month$외국인소진율 <- as.numeric(samsung_month$외국인소진율)
sk_month$외국인소진율 <- as.numeric(sk_month$외국인소진율)
boxplot(samsung_month$외국인소진율, sk_month$외국인소진율, names = c("삼성전자", "SK하이닉스"), 
        main = "Box Plot Comparison", xlab = "Data", ylab = "Value")

#2)삼성전자의 거래량과 종가의 상관관계
samsung_month$거래량 <- as.numeric(samsung_month$거래량)
samsung_month$종가 <- as.numeric(samsung_month$종가)
plot(samsung_month$거래량, samsung_month$종가, xlab="거래량", ylab="종가", main="삼성전자")
abline(lm(samsung_month$종가 ~ samsung_month$거래량),col="red")

#회귀선이 유의미한가
m <- lm(samsung_month$종가 ~ samsung_month$거래량)
summary(m) #p-value가 기각되므로 회귀선이 유의미
