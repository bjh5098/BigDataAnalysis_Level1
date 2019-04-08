# 정규식을 활용한 장비로그 분석하기 소스코드
setwd("c:\\temp")  
install.packages("wordcloud") 
library(wordcloud)

alert_1 <- readLines("oracle_alert_testdb.log") 

# 아래 부분이 중요합니다~!!
alert_2 <- grep("^ORA-",alert_1,value=T)
head(alert_2, 20)
 
errorcount <- table(alert_2)  
head(sort(errorcount, decreasing=T),20)

library(RColorBrewer) 
palete <- brewer.pal(9,"Set1") 
wordcloud(names(errorcount),freq=errorcount,scale=c(5,0.5),rot.per=0.5,min.freq=3,
random.order=F,random.color=T,colors=palete)


 
## ORA-12345: ~~  에서 숫자까지만 잘라내서 워드 클라우드 그리기
alert11 <- readLines("oracle_alert_testdb.log") 
alert12 <- grep("^ORA-+",alert11,value=T)  #ORA-12345 형식으로 된 줄만 걸러냄
alert13 <- substr(alert12,1,9) 
errorcount1 <- table(alert13)  
head(sort(errorcount1, decreasing=T),20)

library(RColorBrewer) 
palete <- brewer.pal(7,"Set1") 
wordcloud(names(errorcount1),freq=errorcount1,scale=c(5,0.5),rot.per=0.5,min.freq=3,
random.order=F,random.color=T,colors=palete)