setwd("c:\\temp") 
#install.packages("KoNLP") 
#install.packages("wordcloud") 
#install.packages("stringr")

library(KoNLP)  
library(wordcloud)
library(stringr) 

useSejongDic() 

# Step 1: 분석할 파일을 불러와서 txt 변수에 저장합니다.
txt <- readLines("부산가족여행_블로그_2.txt",encoding="UTF-8")#######
head(txt,10)

# Step 2: 중복되는 행 제거하기
txt2 <- unique(txt)

# Step 3: 명사만 추출합니다
place <- extractNoun(txt2)
head(place,5)

place <- lapply(place, unique)  

# Step 4 : 불용어 제거 및 변경 작업
wordcount <- table(unlist(place)) 
head(sort(wordcount, decreasing=T),100)
   
cdata <- unlist(place) 
place2 <- str_replace_all(cdata,"[^[:alpha:][:blank:] ]","")
place2 <- gsub("부산","", place2)

place2 <- gsub("제목","", place2)
place2 <- gsub("날짜","", place2)
place2 <- gsub("질문","", place2)
place2 <- gsub("답변","", place2)
place2 <- gsub("시장","", place2)
place2 <- gsub("삼진","삼진어묵", place2)
place2 <- gsub("국제","국제시장", place2)
place2 <- gsub("깡통","깡통시장", place2)
place2 <- gsub("용두산","용두산공원", place2)
place2 <- gsub("더베이","더베이101", place2)
place2 <- gsub("기장","기장베이266", place2)
place2 <- gsub("등대","태종대등대", place2)
#####내가 직접 추가######
place2 <- gsub("해운","해운대", place2)
place2 <- gsub("광안","광안리", place2)
#######################

place2 <- gsub(paste(c("해동","용궁사","해동용궁사"),collapse='|'), "해동용궁사", place2)
place2 <- gsub(paste(c("자갈치","자갈치시장"),collapse='|'), "자갈치시장", place2)
place2 <- gsub(paste(c("돼지","국밥","돼지국밥"),collapse='|'), "돼지국밥",place2)
place2 <- gsub(paste(c("씨앗","호떡","씨앗호떡"),collapse='|'), "씨앗호떡", place2)
place2 <- gsub(paste(c("남포","영화","광장"),collapse='|'), "남포동PIFF광장", place2)
place2 <- gsub(paste(c("감천","문화","마을","감천문화마을"),collapse='|'), "감천문화마을", place2)
#######################
place2 <- gsub(paste(c("사람","사람들"),collapse='|'), "사람들", place2)
place2 <- gsub(paste(c("주차","주차장"),collapse='|'), "주차장", place2)
#######################

place3 <- Filter(function(x) {nchar(x) >= 2 & nchar(x) <= 10} ,place2)

wordcount <- table(place3) 
head(sort(wordcount, decreasing=T),40)



#Step 5 : 추가로 확인된 불용어를 제거합니다
txt <- readLines("부산gsub.txt")  # 부산gsub.txt 파일은 직접 만들어야 합니다
txt
cnt_txt <- length(txt)
cnt_txt
i <- 1
for( i in 1:cnt_txt) {
      place2 <-gsub((txt[i]),"",place2)     
      }

data6 <- Filter(function(x) {nchar(x) >= 2 & nchar(x) <= 10} ,place2)
wordcount <- table(data6)
head(sort(wordcount, decreasing=T),40)

# Step 6: 필터링 된 단어들의 빈도수를 집계한 후 많이 언급된 순서대로 40 개의 단어를 출력하여 확인합니다


#아래 과정으로 생성된 본인의수험번호.csv 파일을 실기 과제물_1 로 제출해야 합니다.
data11 <- head(sort(wordcount, decreasing=T),40)
write.csv(data11 , '201904-141.csv')

#Step 7. 결과를 워드클라우드로 시각화 합니다 
palete <- brewer.pal(7,"Set2")

wordcloud(names(data11),freq=data11,scale=c(5,1),rot.per=0.25,min.freq=10,
random.order=F,random.color=T,colors=palete,max.words=40) 

#아래 과정으로 생성된 본인의 수험번호.png 파일을 실기 과제물_2 로 제출해야 합니다. 
savePlot('201904-141.png' , type='png')



