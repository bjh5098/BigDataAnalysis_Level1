# ���Խ��� Ȱ���� ���α� �м��ϱ� �ҽ��ڵ�
setwd("c:\\temp")  
install.packages("wordcloud") 
library(wordcloud)

alert_1 <- readLines("oracle_alert_testdb.log") 

# �Ʒ� �κ��� �߿��մϴ�~!!
alert_2 <- grep("^ORA-",alert_1,value=T)
head(alert_2, 20)
 
errorcount <- table(alert_2)  
head(sort(errorcount, decreasing=T),20)

library(RColorBrewer) 
palete <- brewer.pal(9,"Set1") 
wordcloud(names(errorcount),freq=errorcount,scale=c(5,0.5),rot.per=0.5,min.freq=3,
random.order=F,random.color=T,colors=palete)


 
## ORA-12345: ~~  ���� ���ڱ����� �߶󳻼� ���� Ŭ���� �׸���
alert11 <- readLines("oracle_alert_testdb.log") 
alert12 <- grep("^ORA-+",alert11,value=T)  #ORA-12345 �������� �� �ٸ� �ɷ���
alert13 <- substr(alert12,1,9) 
errorcount1 <- table(alert13)  
head(sort(errorcount1, decreasing=T),20)

library(RColorBrewer) 
palete <- brewer.pal(7,"Set1") 
wordcloud(names(errorcount1),freq=errorcount1,scale=c(5,0.5),rot.per=0.5,min.freq=3,
random.order=F,random.color=T,colors=palete)