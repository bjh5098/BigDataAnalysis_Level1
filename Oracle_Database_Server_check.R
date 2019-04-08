# 예제 1. 제주 여행지 추천 키워드 분석하기 
#[ 소스 코드 ]
#Step 1. 필요한 패키지 설치 및 실행

#만약 rJava 오류가 발생할 경우
#아래 코드에서 PC 에 설치된 자바 프로그램의 경로를 수정해서 사용하세요
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_144')
#Sys.getenv(“JAVA_HOME”)

setwd("c:\\temp") 
install.packages("KoNLP") 
install.packages("wordcloud") 
install.packages("stringr")	# <- 패키지를 설치합니다

library(KoNLP)  
library(wordcloud)
library(stringr)  # <- 패키지를 로딩합니다

#Step 2. 사전 관련 작업
useSejongDic() 

# 위 사전에 제주도 관광지명이 정확하게 안 들어 있기 때문에 아래와 같이 수동으로 추가합니다.
mergeUserDic(data.frame(readLines("제주도여행지.txt"), "ncn"))

#Step 3. 분석할 파일을 불러와서 txt 변수에 저장합니다.
txt <- readLines("jeju.txt")  # 인코딩 오류 날 경우 encoding=”UTF-8” 사용하세요
head(txt,5)  # 반드시 잘 불러왔는지 확인 하세요

#Step 4.중복되는 행 제거하기
txt2 <- unique(txt)

#Step 5. 명사만 추출합니다
place1 <- extractNoun(txt2)
head(place1,5)

place2 <- lapply(place1, unique) # 1 줄안에서 중복되는 단어 제거하기 

#Step 6. 불용어를 찾기 위해서 현재 상태의 단어 중 많이 언급된 순서로 100개 출력확인
wordcount <- table(unlist(place2)) 
head(sort(wordcount, decreasing=T),100)

#Step 7. 불용어 제거하기
cdata <- unlist(place2) 
place3 <- str_replace_all(cdata,"[^[:alpha:][:blank:] ]","")
place3 <- gsub("랜드","", place3)
place3 <- gsub("폭포","", place3)
place3 <- gsub("주도","", place3)
place3 <- gsub("이곳","", place3)
place3 <- gsub("구석","", place3)
place3 <- gsub("생각","", place3)
place3 <- gsub("애월","애월읍", place3)
place3 <- gsub("정상","수월봉", place3)
place3 <- gsub("에코","에코랜드", place3) 
place3 <- gsub("퍼시픽","퍼시픽랜드", place3) 
place3 <- gsub(paste(c("성산","일출"),collapse='|'), "성산일출봉", place3)
place3 <- gsub(paste(c("한라","한라산"),collapse='|'), "한라산", place3)
place3 <- gsub(paste(c("산방","산방산"),collapse='|'), "산방산", place3)
place3 <- gsub(paste(c("녹차","오설록"),collapse='|'), "오설록", place3)
place3 <- gsub(paste(c("천지","지연","천지연폭포"),collapse='|'), "천지연폭포", place3)
place3 <- gsub(paste(c("주상","절리","주상절리"),collapse='|'), "주상절리", place3)
place3 <- gsub(paste(c("만장","만장굴"),collapse='|'), "만장굴", place3)

wordcount2 <- table(place3 ) 
head(sort(wordcount2, decreasing=T),100)

#불용어가 많을 경우 아래와 같이 제거합니다
txt <- readLines("제주도여행코스gsub.txt")
txt
cnt_txt <- length(txt)
cnt_txt

for( i in 1:cnt_txt) {
      place3 <-gsub((txt[i]),"",place3)     
      }

place4 <- Filter(function(x) {nchar(x) >= 2 & nchar(x) <= 10} ,place3)

wordcount3 <- table(place4) 
head(sort(wordcount3, decreasing=T),30)

# Step 8. 추출된 단어들을 워드 클라우드로 시각화 하기 
palete <- brewer.pal(7,"Set1") 
wordcloud(names(wordcount3),freq=wordcount3,scale=c(5,1),rot.per=0.25,min.freq=4,
  random.order=F,random.color=T,colors=palete)

# 참고 : Step 9. wordcloud2 ( ) 패키지로 시각화 하기
install.packages("wordcloud2")
library(wordcloud2)
wordcount4 <- head(sort(wordcount3, decreasing=T),100)
wordcloud2(wordcount4,gridSize=1,size=0.5,shape="star")

# 빅데이터 분석실무 실기시험 응시하신 분들은 아래 과정을 추가로 진행하세요.

# 아래 과정으로 생성된 본인의수험번호.csv 파일을 실기 과제물_1 로 제출해야 합니다.
# 아래의 파일들은 모두 작업 디렉토리에 저장됩니다

getwd( )  #  작업 디렉토리 확인하기
data11 <- head(sort(wordcount3, decreasing=T),40)
write.csv(data11 , '본인의수험번호.csv')

#아래 과정으로 생성된 본인의 수험번호.png 파일을 실기 과제물_2 로 제출해야 합니다. 
savePlot('본인의수험번호.png' , type='png')

