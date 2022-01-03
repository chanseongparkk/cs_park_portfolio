naver movies sentimental analysis
================
chansung park
2021-12-14

``` r
# load in the libraries we'll need
#install.packages("webshot")
#webshot::install_phantomjs()
library(tidyverse)
library(tidytext)
library(stringr)
library(wordcloud2)
library(XML) 
library(N2H4)
library(KoNLP)
library(gridExtra)
```

## 네이버 영화 텍스트 마이닝 및 감성분석 (어바웃 타임, 아쿠아맨, 월드워Z, 인셉션)

### Data set

``` r
at_reviews <- NULL
aq_reviews <- NULL
ww_reviews <- NULL
ic_reviews <- NULL

about_time <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=92075&type=after&onlyActualPointYn=N&order=newest&page="

aqua_man <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=151153&type=after&onlyActualPointYn=N&order=newest&page="

world_war_z <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=51777&type=after&onlyActualPointYn=N&order=newest&page="

inception <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=52515&type=after&onlyActualPointYn=N&order=newest&page="
```

``` r
# about time review crawling
for (i in 1:240) { 
  newr <- NULL
  url <- paste(about_time, i, sep='')
  txt <- readLines(url, encoding="UTF-8")
  
  # 영화 리뷰 부분만 가져오기
  reviews <- txt[which(str_detect(txt, "id=\"_filtered_ment"))+4] 
  # 특수문자 제거
  reviews <- gsub("<.+?>|\t","", reviews) 
  
  newr <- cbind(reviews)
  at_reviews <- rbind(at_reviews, newr)
}

# aqua man  review crawling
for (i in 1:240) { 
  newr <- NULL
  url <- paste(aqua_man, i, sep='')
  txt <- readLines(url, encoding="UTF-8")
  
  # 영화 리뷰 부분만 가져오기
  reviews <- txt[which(str_detect(txt, "id=\"_filtered_ment"))+4] 
  # 특수문자 제거
  reviews <- gsub("<.+?>|\t","", reviews) 
  
  newr <- cbind(reviews)
  aq_reviews <- rbind(aq_reviews, newr)
}

# world war z review crawling
for (i in 1:240) { 
  newr <- NULL
  url <- paste(world_war_z, i, sep='')
  txt <- readLines(url, encoding="UTF-8")
  
  # 영화 리뷰 부분만 가져오기
  reviews <- txt[which(str_detect(txt, "id=\"_filtered_ment"))+4] 
  # 특수문자 제거
  reviews <- gsub("<.+?>|\t","", reviews) 
  
  newr <- cbind(reviews)
  ww_reviews <- rbind(ww_reviews, newr)
}

# inception review crawling
for (i in 1:240) { 
  newr <- NULL
  url <- paste(inception, i, sep='')
  txt <- readLines(url, encoding="UTF-8")
  
  # 영화 리뷰 부분만 가져오기
  reviews <- txt[which(str_detect(txt, "id=\"_filtered_ment"))+4] 
  # 특수문자 제거
  reviews <- gsub("<.+?>|\t","", reviews) 
  
  newr <- cbind(reviews)
  ic_reviews <- rbind(ic_reviews, newr)
}
```

# 텍스트 데이터 전처리

``` r
# 행번호로 id 열을 만들었고 리뷰에 빈칸을 제거했다.
at_review_dat <- tibble(reply = at_reviews,
                     n_char = nchar(at_reviews)) %>%
  filter(n_char>1) %>%
  mutate(id=row_number()) %>% 
  select(id,reply)

aq_review_dat <- tibble(reply = aq_reviews,
                     n_char = nchar(aq_reviews)) %>%
  filter(n_char>1) %>%
  mutate(id=row_number()) %>% 
  select(id,reply)

ww_review_dat <- tibble(reply = ww_reviews,
                     n_char = nchar(ww_reviews)) %>%
  filter(n_char>1) %>%
  mutate(id=row_number()) %>% 
  select(id,reply)

ic_review_dat <- tibble(reply = ic_reviews,
                     n_char = nchar(ic_reviews)) %>%
  filter(n_char>1) %>%
  mutate(id=row_number()) %>% 
  select(id,reply)
```

### 텍스트 데이터 Tokenization

``` r
# about time tokenization

at_comment <- at_review_dat %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos09,
                drop = FALSE)

at_n<- at_comment %>% 
filter(str_detect(word, "/n")) %>% 
  mutate(word_done  = str_remove(word, "/.*$")) %>% 
  filter(nchar(word_done) > 1)%>% 
  ungroup() 

at_p <- at_comment %>%
  filter(str_detect(word, "/p")) %>% 
  mutate(word_done =str_replace_all(word,"/.*$", "다")) %>%
  filter(nchar(word_done) > 1) %>% 
  ungroup()

at_word <- bind_rows(at_n, at_p) %>% 
  select(id, word_done) 
```

``` r
# aqua man tokenization

aq_comment <- aq_review_dat %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos09,
                drop = FALSE)

aq_n<- aq_comment %>% 
filter(str_detect(word, "/n")) %>%
  mutate(word_done  = str_remove(word, "/.*$")) %>% 
  filter(nchar(word_done) > 1)%>% 
  ungroup() 

aq_p <- aq_comment %>%
  filter(str_detect(word, "/p")) %>% 
  mutate(word_done =str_replace_all(word,"/.*$", "다")) %>%
  filter(nchar(word_done) > 1) %>% 
  ungroup()

aq_word <- bind_rows(aq_n, aq_p) %>% 
  select(id, word_done) 
```

``` r
# world war z tokenization

ww_comment <- ww_review_dat %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos09,
                drop = FALSE)

ww_n<- ww_comment %>% 
filter(str_detect(word, "/n")) %>% 
  mutate(word_done  = str_remove(word, "/.*$")) %>% 
  filter(nchar(word_done) > 1)%>% 
  ungroup() 

ww_p <- ww_comment %>%
  filter(str_detect(word, "/p")) %>% 
  mutate(word_done =str_replace_all(word,"/.*$", "다")) %>%
  filter(nchar(word_done) > 1) %>% 
  ungroup()

ww_word <- bind_rows(ww_n, ww_p) %>% 
  select(id, word_done) 
```

``` r
# inception tokenization

ic_comment <- ic_review_dat %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos09,
                drop = FALSE)

ic_n<- ic_comment %>% 
filter(str_detect(word, "/n")) %>% 
  mutate(word_done  = str_remove(word, "/.*$")) %>% 
  filter(nchar(word_done) > 1)%>% 
  ungroup() 

ic_p <- ic_comment %>%
  filter(str_detect(word, "/p")) %>% 
  mutate(word_done =str_replace_all(word,"/.*$", "다")) %>%
  filter(nchar(word_done) > 1) %>% 
  ungroup()

ic_word <- bind_rows(ic_n, ic_p) %>% 
  select(id, word_done) 
```

### Wordcloud를 통한 많이 쓰이는 단어 시각화

많이 쓰이는 단어를 시각화하는 것이므로 각각의 영화에서 많이쓰인 단어 100개를 추출하여 wordcloud로 시각화하겠다.

``` r
# about time wordcloud

at_count <- at_word %>% 
  count(word_done,sort=T)


wordcloud2(top_n(at_count,100),
           color = "random-light", 
           backgroundColor = "grey",
           fontFamily = '나눔바른고딕',
           minSize = 2)
```

![1](https://user-images.githubusercontent.com/94727467/147906258-fa2b48dc-4309-4cb2-aded-245ddb45d109.png)<!-- -->

``` r
# aqua man wordcloud

aq_count <- aq_word %>% 
  count(word_done,sort=T) 

wordcloud2(top_n(aq_count,100),
           color = "random-light", 
           backgroundColor = "grey",
           fontFamily = '나눔바른고딕',
           minSize = 2)
```

![2](https://user-images.githubusercontent.com/94727467/147906310-62de0c7c-4929-41fa-a2cb-f8225a8e2732.png)<!-- -->

``` r
# world war z wordcloud

ww_count <- ww_word %>% 
  count(word_done,sort=T)

wordcloud2(top_n(ww_count,100),
           color = "random-light", 
           backgroundColor = "grey",
           fontFamily = '나눔바른고딕',
           minSize = 2)
```

![3](https://user-images.githubusercontent.com/94727467/147906330-5157efac-d601-4aef-bcc1-6bd09d7fe34e.png)<!-- -->

``` r
# inception wordcloud

ic_count <- ic_word %>% 
  count(word_done,sort=T) 

wordcloud2(top_n(ic_count,100),
           color = "random-light", 
           backgroundColor = "grey",
           fontFamily = '나눔바른고딕',
           minSize = 2)
```

![4](https://user-images.githubusercontent.com/94727467/147906338-7e363922-9170-4c15-97f9-34d2b92b8b97.png)<!-- -->

### 정규표현식을 활용한 텍스트 데이터 수정 및 변형 (자주 등장하는 단어 위주로)

``` r
cs <- tibble(word=c("보다", "영화","하다","있다","했다","되다","싶다","알다","몇번","몇 번"))

at_word2 <- at_word %>% 
  anti_join(cs, by=c("word_done"="word")) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+으로"),
                          str_sub(at_word$word_done,1,nchar(at_word$word_done)-2),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+에서"),
                          str_sub(at_word$word_done,1,nchar(at_word$word_done)-2),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+것도"),
                          str_sub(at_word$word_done,1,nchar(at_word$word_done)-2),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+은"),
                          str_sub(at_word$word_done,1,nchar(at_word$word_done)-1),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+는"),
                          str_sub(at_word$word_done,1,nchar(at_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+도"),
                          str_sub(at_word$word_done,1,nchar(at_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+들"),
                          str_sub(at_word$word_done,1,nchar(at_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+을"),
                          str_sub(at_word$word_done,1,nchar(at_word$word_done)-1),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+가"),
                          str_sub(at_word$word_done,1,nchar(at_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=gsub("\\^ㅋ","", word_done)) %>% 
  mutate(word_done=gsub("\\^ㅎ","", word_done)) %>%
  filter(nchar(word_done)>1)
```

``` r
aq_word2 <- aq_word %>% 
  anti_join(cs, by=c("word_done"="word")) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+으로"),
                          str_sub(aq_word$word_done,1,nchar(aq_word$word_done)-2),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+에서"),
                          str_sub(aq_word$word_done,1,nchar(aq_word$word_done)-2),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+것도"),
                          str_sub(aq_word$word_done,1,nchar(aq_word$word_done)-2),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+은"),
                          str_sub(aq_word$word_done,1,nchar(aq_word$word_done)-1),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+는"),
                          str_sub(aq_word$word_done,1,nchar(aq_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+도"),
                          str_sub(aq_word$word_done,1,nchar(aq_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+들"),
                          str_sub(aq_word$word_done,1,nchar(aq_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+을"),
                          str_sub(aq_word$word_done,1,nchar(aq_word$word_done)-1),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+가"),
                          str_sub(aq_word$word_done,1,nchar(aq_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=gsub("\\^ㅋ","", word_done)) %>% 
  mutate(word_done=gsub("\\^ㅎ","", word_done)) %>%
  filter(nchar(word_done)>1)
```

``` r
ww_word2 <- ww_word %>% 
  anti_join(cs, by=c("word_done"="word")) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+으로"),
                          str_sub(ww_word$word_done,1,nchar(ww_word$word_done)-2),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+에서"),
                          str_sub(ww_word$word_done,1,nchar(ww_word$word_done)-2),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+것도"),
                          str_sub(ww_word$word_done,1,nchar(ww_word$word_done)-2),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+은"),
                          str_sub(ww_word$word_done,1,nchar(ww_word$word_done)-1),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+는"),
                          str_sub(ww_word$word_done,1,nchar(ww_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+도"),
                          str_sub(ww_word$word_done,1,nchar(ww_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+들"),
                          str_sub(ww_word$word_done,1,nchar(ww_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+을"),
                          str_sub(ww_word$word_done,1,nchar(ww_word$word_done)-1),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+가"),
                          str_sub(ww_word$word_done,1,nchar(ww_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=gsub("\\^ㅋ","", word_done)) %>% 
  mutate(word_done=gsub("\\^ㅎ","", word_done)) %>%
  filter(nchar(word_done)>1)
```

``` r
ic_word2 <- ic_word %>% 
  anti_join(cs, by=c("word_done"="word")) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+으로"),
                          str_sub(ic_word$word_done,1,nchar(ic_word$word_done)-2),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+에서"),
                          str_sub(ic_word$word_done,1,nchar(ic_word$word_done)-2),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+것도"),
                          str_sub(ic_word$word_done,1,nchar(ic_word$word_done)-2),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+은"),
                          str_sub(ic_word$word_done,1,nchar(ic_word$word_done)-1),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+는"),
                          str_sub(ic_word$word_done,1,nchar(ic_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+도"),
                          str_sub(ic_word$word_done,1,nchar(ic_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+들"),
                          str_sub(ic_word$word_done,1,nchar(ic_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+을"),
                          str_sub(ic_word$word_done,1,nchar(ic_word$word_done)-1),
                          word_done))  %>% 
  mutate(word_done=ifelse(str_detect(word_done,"[가-힣]+가"),
                          str_sub(ic_word$word_done,1,nchar(ic_word$word_done)-1),
                          word_done)) %>% 
  mutate(word_done=gsub("\\^ㅋ","", word_done)) %>% 
  mutate(word_done=gsub("\\^ㅎ","", word_done)) %>%
  filter(nchar(word_done)>1)
```

### Word Frequency 시각화 및 해석

영화별로 상위 20개 단어를 추출하여 시각화하겠다.

``` r
at_count2 <- at_word2 %>% 
  count(word_done,sort=T) %>%
  top_n(20) 

at_count_plot <- ggplot(at_count2,aes(n, fct_reorder(word_done, n))) +
  geom_col(fill = "#669933")+
  labs(y="word",x="frequancy",title="at_count_plot")+
  theme(plot.title = element_text(hjust = 0.5))
  
  

aq_count2 <- aq_word2 %>% 
  count(word_done,sort=T) %>%
  top_n(20) 

aq_count_plot <- ggplot(aq_count2,aes(n, fct_reorder(word_done, n))) +
  geom_col(fill = "#00CCFF")+
  labs(y="word",x="frequancy",title="aq_count_plot")+
  theme(plot.title = element_text(hjust = 0.5))

ww_count2 <- ww_word2 %>% 
  count(word_done,sort=T) %>%
  top_n(20) 

ww_count_plot <- ggplot(ww_count2,aes(n, fct_reorder(word_done, n))) +
  geom_col(fill = "#CC0000")+
  labs(y="word",x="frequancy",title="ww_count_plot")+
  theme(plot.title = element_text(hjust = 0.5))

ic_count2 <- ic_word2 %>% 
  count(word_done,sort=T) %>%
  top_n(20) 

ic_count_plot <- ggplot(ic_count2,aes(n, fct_reorder(word_done, n))) +
  geom_col(fill = "#CC99CC")+
  labs(y="word",x="frequancy",title = "ic_count_plot")+
  theme(plot.title = element_text(hjust = 0.5))
```

시각화 결과

``` r
grid.arrange(at_count_plot, aq_count_plot, ww_count_plot, ic_count_plot, ncol=2)
```

![5](https://user-images.githubusercontent.com/94727467/147906372-03a490a9-e32d-4b10-893b-f1c37c1d7269.png)<!-- -->

영화 별 상위 빈도수에 따른 해석

어바웃 타임: 인생, 감동, 로맨스, 사랑, 아버지 등의 단어가 많이 사용되었고 많은 감동을 주는 로맨스 영화인 것 같다.
인생영화라는 단어가 많이 나왔으며 나는 하루하루 마지막 날처럼 시간을 쓰는 내용의 영화인 것으로 해석할 수 있었다.
본인의 인생영화이기도 하기에 첫번째 영화로 뽑아보았다\!

아쿠아맨: 스토리, 재미있다, 액션, 마블, 뻔하다, 영상미, dc 등의 단어가 많이 사용되었고 상위 빈도수를 분석하여 나는
아쿠아맨이 스토리는 뻔하지만 영상미 좋고 재미있는 dc 혹은 마블의 영화인 것으로 해석할 수 있었다.

월드워 Z: 좀비, 좀비물, 재미있다, 스케일, 크다, 아쉽다 등의 단어가 많이 사용되었고 상위 빈도수를 분석하여 나는 월드워
Z가 브래드피트가 주인공인 스케일 크고 긴장감 넘치는 좀비영화인 것으로 해석할 수 있었고 아쉽다라는 단어도 많이 사용된 것으로
보아 재미있는 사람도 많았지만 아쉬웠다는 사람도 일부 있었음을 알 수 있었다.

인셉션: 최고, 놀라다, 명작, 스토리, 인생영화 등의 단어가 많이 사용되었고 상위 빈도수를 분석하여 나는 인셉션의 내용을 알 수
있는 단어가 많이 없어 정확한 내용은 알 수 없지만 인셉션이라는 영화가 많은 사람들을 놀라게하고 스토리가 매우 탄탄한 명작임을
유추할 수 있었다.

# tf-idf score 시각화

``` r
# 각 영화의 단어모음을 한 tibble으로 만들어준다.

at_word3 <- at_word %>% 
  mutate(title = "about time")

aq_word3 <- aq_word %>% 
  mutate(title = "aqua man")

ww_word3 <- ww_word %>% 
  mutate(title = "word war z")

ic_word3 <- ic_word %>% 
  mutate(title = "inception")

total_word <- bind_rows(at_word3, aq_word3, ww_word3, ic_word3)
```

``` r
# tf_idf 저장

tf_idf <- total_word %>% 
  group_by(title) %>% 
  count(word_done) %>% 
  bind_tf_idf(term = word_done, document = title, n = n) %>% 
  select(title,word_done,tf_idf)
```

``` r
# 영화 별 상위 15개 tf_idf 시각화

tf_idf %>% 
  group_by(title) %>% 
  top_n(15) %>% 
  ggplot(aes(tf_idf, fct_reorder(word_done,tf_idf),fill = title))+
  geom_col()+
  facet_wrap(~title,scale="free")+
  labs(y="word_done",title = "tf_idf score \n by title")+
  theme(plot.title = element_text(hjust = 0.5))
```

![6](https://user-images.githubusercontent.com/94727467/147906392-1f661ae9-949d-4050-91f8-a104d6995e3a.png)<!-- -->

tf\_idf score는 다른 문서에는 많지 않고 해당 문서에서 자주 등장하는 단어일수록 높은 점수를 받는다. 즉 문서 내에서
중요한 키워드라고 할 수 있는 것이다.

어바웃타임: 어바웃타임의 리뷰 내에서 중요한 키워드는 하루하루, 로맨스, 소중한, 시간여행, 따뜻해지다, 일상, 행복한 등이
있다. 중요한 키워드를 보고 다른 영화들과는 차별되게 하루하루 소중한 일상에 마음 따뜻한 영화이고 시간여행을 하는 로맨스
영화임을 유추할 수 있다.

아쿠아맨: 아쿠아맨의 리뷰 내에서 중요한 키워드는 dc, 제임스완, 바닷속, 아틀란티스, 아쿠아맨, 히어로물 등이 있다. 중요한
키워드를 보고 다른 영화와는 차별되게 이름에서 알 수 있듯이 아쿠아맨이라는 히어로가 바닷속(아틀란티스)에서 활약하는 모습을
보여주는 dc의 영화이고 감독은 제임스완임을 유추해볼 수 있다.

인셉션: 인셉션의 리뷰 내에서 중요한 키워드는 꿈속, 놀라다, 놀란감독, 무의식, 디카프리오, 상상력, 팽이, 천재 등이 있다.
중요한 키워드를 보고 다른 영화와는 차별되게 꿈속, 즉 무의식상태에서 영화를 전개해나가고 디카프리오 배우가 출연하고 놀란
감독의 영화임을 알 수 있고 상상력을 자극하는 장르의 영화임을 유추할 수 있다.

월드워 Z: 월드워 Z의 리뷰 내에서 중요한 키워드는 좀비,좀비물, 브래드피트, 이스라엘, 스릴 등이 있다. 중요한 키워드를 보고
다른 영화와는 차별되게 스릴있는 좀비물임을 알 수 있고 이스라엘이 영화 내용에 있을 것이고 브래드피트가 출연했음을 유추할 수
있다.
