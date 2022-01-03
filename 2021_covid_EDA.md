covid-19 data analysis
================
Chansung Park
2021 5 22

현재 전 세계적으로 코로나가 유행하고 있다. 나는 최근 코로나 확진률이 급격하게 증가하고 신규 코로나 확진률이 높은 국가에 백신을
먼저 공급해줘야 한다고 생각한다. 백신이 급한 나라와 대륙을 추출하기 위해 전날을 기준으로 전 세계적으로 신규 코로나 확진률과
누적 코로나 확진률이 높은 국가와 대륙을 추출하고 최근 코로나 확진률의 증가 폭이 큰 나라들의 데이터를 시각화하여 코로나
데이터가 나타내고 있는 정보를 알아내고 앞으로의 상황은 어떻게 될 것인지 추측하여 백신을 어느 나라, 대륙에 먼저 공급해야
하는지 알아보고자 한다.

또한 2021년 한국의 코로나 신규 확진자 수, 신규 사망자 수를 시간의 흐름대로 분석하여 시각화하고 앞으로 한국의 코로나 신규
확진자 수가 어떻게 변화할 것인지에 대해 분석해보겠다.

필요한 패키지 불러오기

``` r
library(ggplot2)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(forecast)
```

데이터는 5월 22일까지의 전 세계 코로나 데이터이다.

``` r
getwd()
```

    ## [1] "C:/Users/chansung/Desktop/과제뭉치/찬성 3-1/탐색적데이터분석/exploratory_data_analysis찬성"

``` r
a <- read.csv("final project/covid_data(탐색적데이터분석).csv")
```

5월 22일을 기준으로 각 나라 별 100만명당 신규 확진자 수와 누적확진자 수를 구하였고 필요 데이터를 추출하였다.

``` r
df <- a %>%
  mutate(date=ymd(date)) %>% 
  filter(date == "2021-05-22") %>%
  mutate(cases_per_mil = new_cases/population*1000000) %>% #100만명당 새로운 확진자 수
  mutate(cum_cases_per_mil = total_cases/population*1000000) %>% #100만명당 누적 확진자 수
  mutate(location = factor(location)) %>%
  mutate(continent = factor(continent)) %>% 
  select(continent,location,cases_per_mil,cum_cases_per_mil) %>% 
  arrange(desc(cases_per_mil)) %>% 
  filter(continent != "") %>%  #대륙변수가 비어있는 것을 제외시켰다.(ex.유럽연합 평균값, 아시아 평균값 등) 
  drop_na()
```

박스플롯을 그려 신규 확진자 수의 분포를 알아본다.

``` r
ggplot(df) +
  geom_boxplot(aes(cases_per_mil))+
  coord_flip()
```

![1](https://user-images.githubusercontent.com/94727467/147906610-5dc5c082-95f1-44b2-8ae3-45d726fb880f.png)<!-- -->

``` r
ggplot(df) +
  geom_boxplot(aes(continent,cases_per_mil))+
  ylim(0,1000)
```

![2](https://user-images.githubusercontent.com/94727467/147906643-1d2c3cb4-b5b5-446a-8b03-850f4921a029.png)<!-- -->

``` r
df %>% 
  head(20)
```

    ##        continent            location cases_per_mil cum_cases_per_mil
    ## 1           Asia            Maldives     3625.9902         100574.98
    ## 2           Asia             Bahrain     1597.9238         126276.53
    ## 3  South America             Uruguay      998.6392          75425.90
    ## 4  South America           Argentina      711.8143          77765.74
    ## 5  South America            Paraguay      452.5743          46330.97
    ## 6  South America               Chile      392.3372          69229.89
    ## 7         Europe           Lithuania      372.1130          99319.29
    ## 8  South America            Colombia      368.2378          63101.51
    ## 9  North America Trinidad and Tobago      363.7037          13893.62
    ## 10 South America              Brazil      359.8523          75496.25
    ## 11 South America              Guyana      344.5387          20359.57
    ## 12 South America                Peru      318.1805          58257.31
    ## 13 South America            Suriname      317.0631          22037.59
    ## 14        Africa          Cape Verde      296.7690          52515.52
    ## 15          Asia               Nepal      294.8504          17354.10
    ## 16          Asia             Georgia      268.4766          84438.51
    ## 17 South America             Bolivia      257.4310          29646.98
    ## 18          Asia              Kuwait      238.1419          69832.24
    ## 19        Europe              Latvia      233.8032          69299.05
    ## 20        Europe             Belgium      217.0909          90350.53

100만명당 신규 확진자 수가 높은 상위 20나라를 추출해본다.

``` r
df2 <- df %>% 
  head(20)
```

100만명당 신규 확진자 수가 높은 상위 20나라는 이와 같다.

``` r
ggplot(df2,aes(reorder(location,cases_per_mil),cases_per_mil,fill=continent))+
  geom_col(stat = "identity")+
  xlab("lacation")+
  coord_flip()
```

![3](https://user-images.githubusercontent.com/94727467/147906654-68faabfd-2973-4626-b8ec-53ff4b6f112a.png)<!-- -->

``` r
# 시각화 한 결과 몰디브와 바레인의 확진률이 기형적으로 높으며 상위 20개 국가 중 남미 국가의 비율이 높은 것을 알 수 있다.(5월 22일 기준)
```

신규 확진자 수가 많은 나라를 구해봤으니 이제 코로나 확진률의 평균이 높은 대륙을 추출하겠다.

``` r
df3 <- df %>% 
  group_by(continent) %>%
  mutate(con_cases_per_mil = mean(cases_per_mil)) %>% 
  select(continent,con_cases_per_mil) %>% 
  arrange(con_cases_per_mil) 
  
df3 <- df3 %>% 
  count(continent,con_cases_per_mil) #대륙별 나라의 수를 알아본다.
 
df3 <- df3[,1:2]

df3[,2] = round(df3[,2],2)

df3 
```

    ## # A tibble: 6 x 2
    ## # Groups:   continent [6]
    ##   continent     con_cases_per_mil
    ##   <fct>                     <dbl>
    ## 1 Africa                    13.2 
    ## 2 Asia                     172.  
    ## 3 Europe                    70.3 
    ## 4 North America             49.7 
    ## 5 Oceania                    3.75
    ## 6 South America            385.

``` r
ggplot(df3,aes(reorder(continent,con_cases_per_mil),con_cases_per_mil,fill=continent))+
  geom_col(stat = "identity")+
  xlab("continent")
```

![4](https://user-images.githubusercontent.com/94727467/147906671-c55462e5-aa83-4576-b7f0-1d417773658c.png)<!-- -->

df3를 보면 South America \> Europe \> Asia \> North America \> Africa \>
Oceania 순으로 대륙별 100만명당 코로나 확진률이 높은 것을 볼 수 있다.

df2를 통해 5/22 기준 100만명당 신규 확진자 수가 가장 높은 20개 국가를 알 수 있으며 df3를 통해 5/22 기준
100만명당 확진률이 큰 대륙를 알 수 있다.

이제 각 나라별 100만명 당 누적 확진자 수의 분포를 시각화를 해보겠다.

처음은 박스플롯을 그려 데이터의 분포를 확인한다.

``` r
ggplot(df) +
  geom_boxplot(aes(cum_cases_per_mil))+
  coord_flip()
```

![5](https://user-images.githubusercontent.com/94727467/147906680-433664e0-628d-4be8-a871-2f161d5edd40.png)<!-- -->

``` r
ggplot(df) +
  geom_boxplot(aes(continent,cum_cases_per_mil))
```

![6](https://user-images.githubusercontent.com/94727467/147906695-9684e9d2-e471-430a-af00-826f037087bc.png)<!-- -->

상위 20개 국가를 추출하고 시각화한다.

``` r
df4 <- df %>% 
  arrange(desc(cum_cases_per_mil)) %>% 
  head(20)  

ggplot(df4,aes(reorder(location,cum_cases_per_mil),cum_cases_per_mil,fill=continent))+
  geom_col(stat = "identity")+
  coord_flip()+
  xlab("location")
```

![7](https://user-images.githubusercontent.com/94727467/147906714-1cb2b84d-8023-43da-9b93-3c9485d5eb56.png)<!-- -->

``` r
# 5/22 기준 누적확진률의 상위 20개 국가에는 유럽 국가의 비율이 대부분을 차지하고 있을만큼 높음을 알 수 있다. 
```

100만명당 누적확진수를 대륙 별로 시각화 해보았다.

``` r
ggplot(df,aes(reorder(location,cum_cases_per_mil),cum_cases_per_mil,fill=continent))+
  geom_col(stat = "identity")+
  xlab("location")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  theme(legend.position = "none")+
  coord_flip()+
  facet_wrap(~continent,scales = "free_y")
```

![8](https://user-images.githubusercontent.com/94727467/147906730-e171a600-587b-4e73-a5d1-95e0d0dad6ba.png)<!-- -->

``` r
#시각화 결과 유럽국가는 모든 국가들이 전반적으로 매우 높은 누적확진률을 보이고 있었고, 남아메리카, 아시아의 일부 국가 역시 높은 누적 확진률을 보이고 있다. 이러한 시각화 결과를 통해 상위 20개의 누적 확진률에서 유럽 국가가 대부분을 차지하고 있는지 알 수 있다. 
```

이제 각 대륙별 100만명 당 누적 확진자 수의 평균을 시각화를 해보겠다.

``` r
df5 <- df %>% 
  group_by(continent) %>%
  mutate(con_cumcases_per_mil = mean(cum_cases_per_mil)) %>% 
  select(continent,con_cumcases_per_mil) %>% 
  arrange(con_cumcases_per_mil) 

df5 <- df5 %>% 
  count(continent,con_cumcases_per_mil) #대륙별 나라의 수를 알아본다.
 
df5 <- df5[,1:2]

df5[,2] = round(df5[,2],2)

df5 
```

    ## # A tibble: 6 x 2
    ## # Groups:   continent [6]
    ##   continent     con_cumcases_per_mil
    ##   <fct>                        <dbl>
    ## 1 Africa                       7757.
    ## 2 Asia                        27341.
    ## 3 Europe                      75856.
    ## 4 North America               24166.
    ## 5 Oceania                       420.
    ## 6 South America               47429.

``` r
ggplot(df5,aes(reorder(continent,con_cumcases_per_mil),con_cumcases_per_mil,fill=continent))+
  geom_col(stat = "identity")+
  xlab("continent")
```

![9](https://user-images.githubusercontent.com/94727467/147906750-3d3cdf7a-0985-4923-9dfb-0fe51a159265.png)<!-- -->

df5을 통해 Europe \> South America \> Asia \> North America \> Africa \>
Oceania 순으로 100만명당 평균 누적 코로나 확진률이 높은 것을 볼 수 있다.

마지막으로 100만명 당 새로운 확진자 수가 큰 나라와 대륙, 100만명 당 누적 확진자 수가 큰 나라와 대륙의 시각화 자료를
한번에 보고 비교분석해보겠다.

``` r
c1 <- ggplot(df2,aes(reorder(location,cases_per_mil),cases_per_mil,fill=continent))+
  geom_col(stat = "identity")+
  coord_flip()+
  labs(x=NULL,y=NULL,title="top 20 cases_per_mil")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.7 ))

c2 <- ggplot(df4,aes(reorder(location,cum_cases_per_mil),cum_cases_per_mil,fill=continent))+
  geom_col(stat = "identity")+
  coord_flip()+
  labs(x=NULL,y=NULL,title="top 20 cum_cases_per_mil")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.7 ))

c3 <- ggplot(df3,aes(reorder(continent,con_cases_per_mil),con_cases_per_mil,fill=continent))+
  geom_col(stat = "identity")+
  labs(x=NULL,y=NULL,title="cases_per_mil by continent")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.7))

c4 <- ggplot(df5,aes(reorder(continent,con_cumcases_per_mil),con_cumcases_per_mil,fill=continent))+
  geom_col(stat = "identity")+
  labs(x=NULL,y=NULL,title="cum_cases_per_mil by continent")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.7 ))





grid.arrange(c1,c2,c3,c4, nrow=2, ncol=2)
```

![10](https://user-images.githubusercontent.com/94727467/147906760-bed78815-842b-4233-ab50-04a93f43ed39.png)<!-- -->

(5월22일 기준)

100만명당 신규 확진자의 상위 20개 국가를 살펴보면 기형적으로 몰디브, 바레인이 높고 남아메리카의 국가들이 많이 분포해 있다.

100만명당 누적 확진자의 상위 20개 국가를 살펴보면 유럽이 대부분을 차지하고 있다. 아무래도 유럽국가들은 국가 간의 통행이
자유롭고 마스크 착용을 잘 하지 않는 등의 이유로 확진률이 높다고 추측할 수 있다.

100만명 당 신규확진자, 100만명 당 누적확진자의 대륙별 평균을 살펴보면 평균 신규 확진자의 수는 남아메리카가 가장 높고 그
뒤를 유럽이 따르고 있다. 평균 누적 확진율을 살펴보면 유럽이 가장 높고 남아메리카가 그 뒤를 따르고 나머지 대륙들의 순위는
동일하다. 평균 누적 확진율은 유럽이 더 높지만 남아메리카의 평균 신규 확진율이 유럽의 약 2배에 넘기 때문에 이러한 추세가
이어지면 멀지 않은 미래에 남아메리카의 평균 누적 확진률이 유럽의 평균 누적 확진률을 뛰어넘을 것이라고 추측할 수 있다.

고로 우선적으로 백신이 공급되어야 할 대륙은 유럽, 남아메리카이지만 현재 확진자 수가 기형적으로 늘어나고 있는 몰디브와 바레인에게
가장 먼저 백신이 공급되어야 할 것이다. 그 후로 점차 백신을 공급한다고 할 때 우선적으로 공급해줘야 하는 나라들은 신규
확진자들이 많이 나오고 있는 df2에서 추출된 20개의 나라이다.

다음으로는 한국의 코로나 확진자 수, 코로나 사망자 수 데이터를 시간의 흐름대로 분석하여 앞으로의 한국 코로나 확진자 수와 사망자
수가 어떻게 변화할 것인지 시각화해보겠다.

``` r
#2021년 한국의 신규 확진자 데이터와 신규 사망자 데이터를 시계열데이터로 변환하여 각각 kor1, kor2에 저장하였다.

kor1 <-a %>% 
  mutate(date=ymd(date)) %>% 
  filter(location == "South Korea") %>%
  filter(date > ymd(20210101)) %>% 
  select(new_cases)

inds<- seq(as.Date("2021-01-01"), as.Date("2021-05-22"), by = "day")

kor1 <- ts(kor1,start = c(2021, 
          as.numeric(format(inds[1], "%j"))),
           frequency = 365)

kor2 <-a %>% 
  mutate(date=ymd(date)) %>% 
  filter(location == "South Korea") %>%
  filter(date > ymd(20210101)) %>% 
  select(new_deaths)

kor2 <- ts(kor2,start = c(2021, 
          as.numeric(format(inds[1], "%j"))),
           frequency = 365)
```

신규 확진자수와 신규 사망자수 시계열데이터 시각화

``` r
par(mfrow=c(2,1))
plot.ts(kor1)
plot.ts(kor2)
```

![11](https://user-images.githubusercontent.com/94727467/147906777-3ba8782d-bbe0-4b05-9fc7-c0d1602c583c.png)<!-- -->

3회 차분을 통해 데이터 정상화

``` r
kor1_diff <- diff(kor1, differences = 3)
kor2_diff <- diff(kor2, differences = 3)
```

차분한 데이터로 arima 모형 확인

``` r
acf(kor1_diff, lag.max = 20) 
```

![12](https://user-images.githubusercontent.com/94727467/147906791-cc93cfc4-ce48-418f-95d8-75839e250c5a.png)<!-- -->

``` r
pacf(kor1_diff, lag.max = 20)
```

![13](https://user-images.githubusercontent.com/94727467/147906813-29b067b4-1754-48e6-aa66-0edd1e6a1ac1.png)<!-- -->

``` r
acf(kor2_diff, lag.max = 20) 
```

![14](https://user-images.githubusercontent.com/94727467/147906829-0ef7884d-a4c3-43ae-94e3-74963d28250b.png)<!-- -->

``` r
pacf(kor2_diff, lag.max = 20)
```

![15](https://user-images.githubusercontent.com/94727467/147906850-fc41a8c5-ad77-474b-a4bc-31695c57eb27.png)<!-- -->

절단값이 분명하지 않기 때문에 arima 모형을 명확하게 확인하기 힘들다. 따라서 확진자, 사망자에 대한 ARIMA 모형 자동으로
확인한다.

``` r
kor1_arima <- auto.arima(kor1) #ARIMA(5,1,3)

kor2_arima <- auto.arima(kor2) #ARIMA(2,1,1)
```

확진자, 사망자 예측(앞으로 30일)

``` r
kor1_fc<- forecast(kor1_arima, h = 30) 

kor2_fc<-forecast(kor2_arima, h = 30)
```

확진자, 사망자 예측 시각화

``` r
par(mfrow=c(2,1))
plot(kor1_fc)
plot(kor2_fc)
```

![16](https://user-images.githubusercontent.com/94727467/147906868-a3e32726-1980-4731-bf3d-9d72e30372f1.png)<!-- -->

5월 22일 이후 30일간의 확진자, 사망자의 변화

확진자: 500-600명 초반대로 예측되며 상승과 하락이 번갈아서 이루어지는 것으로 보아 약간의 주기성이 있다고 볼 수 있다.
별다른 사건이 없으면 5월 22일 이후 30일동안의 확진자 수는 최근 확진자 수와 비슷할 것으로 예측된다.

사망자: 0-4명정도로 예측되며 4명대로 시작했다가 점차적으로 줄어들어 약 30일 뒤에는 0명대에 다다를것이라고 예측된다.

※출처

1.https://github.com/owid/covid-19-data/tree/master/public/data (데이터셋)

2.https://stackoverflow.com/questions/33128865/starting-a-daily-time-series-in-r
(일일 시계열데이터 생성)

3.https://woosa7.github.io/R-%EC%8B%9C%EA%B3%84%EC%97%B4%EB%B6%84%EC%84%9D-Time-Series-ARIMA/
(시계열데이터 예측모델링)

