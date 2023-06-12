---
title: "반도체 하반기 예측"
date: 2023-06-10
output: 
  github_document:
  toc: true
---

```{r setup, include = FALSE}
library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)

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


#매출액, 영업이익, 당기순이익 barplot그리기
par(mfrow=c(1,2))

options(scipen = 999)
barplot(cbind(sales_ss,Operating_profit_ss,Net_profit_ss)~samsung_table$분기, ylim=c(0,1000000), 
        xlab="Q",main="삼성전자 분기실적",beside=T,col = c("lightblue", "lightcyan", "lavender"))
legend("topright",legend=c("판매량","영업이익","당기순이익"),fill=c("lightblue", "lightcyan", "lavender"))


barplot(cbind(sales_sk,Operating_profit_sk,Net_profit_sk)~sk_table$분기, ylim=c(0,1000000), 
        xlab="Q",main="SK하이닉스 분기실적",beside=T,col = c("pink","mistyrose","cornsilk"))
legend("topright",legend=c("판매량","영업이익","당기순이익"),fill=c("pink","mistyrose","cornsilk"))


#삼성전자 EPS, PER 그래프 그리기
#EPS 막대그래프 그리기
par(mfrow=c(1,1))
barplot(EPS_ss~samsung_table$분기, ylim = c(0, 4000),
        xlab = "Q", ylab = "EPS", main = "삼성전자 EPS, PER")

# PER 꺾은선 그래프 추가
par(new = TRUE)
plot(PER_ss, type = "b", pch = 16, col = "red", axes = FALSE, ylim=c(0,15),xlab = "", ylab = "")
axis(4, col = "red", ylim = c(0, 15), las = 1)

#SK하이닉스 EPS, PER 그래프 그리기
#EPS 막대그래프 그리기
par(mfrow=c(1,1))
barplot(EPS_sk~sk_table$분기, ylim = c(-6000, 6000),
        xlab = "Q", ylab = "EPS", main = "SK하이닉스 EPS, PER")

# PER 꺾은선 그래프 추가
par(new = TRUE)
plot(PER_sk, type = "b", pch = 16, col = "red", axes = FALSE, ylim=c(-30,30),xlab = "", ylab = "")
axis(4, col = "red", ylim = c(0, 15), las = 1)


#차트그리기
samsung_url_chart <- "https://api.finance.naver.com/siseJson.naver?symbol=005930&requestType=1&startTime=19930801&endTime=20230611&timeframe=month"
sk_url_chart <- "https://api.finance.naver.com/siseJson.naver?symbol=000660&requestType=1&startTime=19930801&endTime=20230611&timeframe=month"
db_url_chart <- "https://api.finance.naver.com/siseJson.naver?symbol=000990&requestType=1&startTime=19930801&endTime=20230611&timeframe=month"
hm_url_chart <- "https://api.finance.naver.com/siseJson.naver?symbol=042700&requestType=1&startTime=19930801&endTime=20230611&timeframe=month"
  

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
db_df <- get_corporation_df(db_url_chart)
hm_df <- get_corporation_df(hm_url_chart)

#2016년부터 월별 데이터프레임
samsung_month <- samsung_df[270:nrow(samsung_df),]
sk_month <- sk_df[230:nrow(sk_df),]
db_month <- db_df[270:nrow(db_df),]
hm_month <- hm_df[127:nrow(hm_df),]


# 날짜문자열을 날짜 데이터로 변환
dates_1 <- as.Date(samsung_month$날짜, format = "%Y%m%d")
dates_2 <- as.Date(sk_month$날짜, format = "%Y%m%d")
dates_3 <- as.Date(db_month$날짜, format = "%Y%m%d")
dates_4 <- as.Date(hm_month$날짜, format = "%Y%m%d")

#시가차트 그리기
par(mfrow=c(2,2))
plot(dates_1, samsung_month$시가, type = "l", xlab = "Date", ylab = "Open Price", main = "삼성전자 시가차트")
plot(dates_2, sk_month$시가, type = "l", xlab = "Date", ylab = "Open Price", main = "SK하이닉스 시가차트")
plot(dates_3, db_month$시가, type = "l", xlab = "Date", ylab = "Open Price", main = "DB하이텍 시가차트")
plot(dates_4, hm_month$시가, type = "l", xlab = "Date", ylab = "Open Price", main = "한미반도체 시가차트")


```

We have data about `r nrow(diamonds)` diamonds. Only 
`r nrow(diamonds) - nrow(smaller)` are larger than
2.5 carats. The distribution of the remainder is shown
below:

```{r, echo = FALSE}
smaller %>% 
  ggplot(aes(carat)) + 
  geom_freqpoly(binwidth = 0.01)
```
