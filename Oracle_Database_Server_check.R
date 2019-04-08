# ���� 1. ���� ������ ��õ Ű���� �м��ϱ� 
#[ �ҽ� �ڵ� ]
#Step 1. �ʿ��� ��Ű�� ��ġ �� ����

#���� rJava ������ �߻��� ���
#�Ʒ� �ڵ忡�� PC �� ��ġ�� �ڹ� ���α׷��� ��θ� �����ؼ� ����ϼ���
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_144')
#Sys.getenv(��JAVA_HOME��)

setwd("c:\\temp") 
install.packages("KoNLP") 
install.packages("wordcloud") 
install.packages("stringr")	# <- ��Ű���� ��ġ�մϴ�

library(KoNLP)  
library(wordcloud)
library(stringr)  # <- ��Ű���� �ε��մϴ�

#Step 2. ���� ���� �۾�
useSejongDic() 

# �� ������ ���ֵ� ���������� ��Ȯ�ϰ� �� ��� �ֱ� ������ �Ʒ��� ���� �������� �߰��մϴ�.
mergeUserDic(data.frame(readLines("���ֵ�������.txt"), "ncn"))

#Step 3. �м��� ������ �ҷ��ͼ� txt ������ �����մϴ�.
txt <- readLines("jeju.txt")  # ���ڵ� ���� �� ��� encoding=��UTF-8�� ����ϼ���
head(txt,5)  # �ݵ�� �� �ҷ��Դ��� Ȯ�� �ϼ���

#Step 4.�ߺ��Ǵ� �� �����ϱ�
txt2 <- unique(txt)

#Step 5. ���縸 �����մϴ�
place1 <- extractNoun(txt2)
head(place1,5)

place2 <- lapply(place1, unique) # 1 �پȿ��� �ߺ��Ǵ� �ܾ� �����ϱ� 

#Step 6. �ҿ� ã�� ���ؼ� ���� ������ �ܾ� �� ���� ��޵� ������ 100�� ���Ȯ��
wordcount <- table(unlist(place2)) 
head(sort(wordcount, decreasing=T),100)

#Step 7. �ҿ�� �����ϱ�
cdata <- unlist(place2) 
place3 <- str_replace_all(cdata,"[^[:alpha:][:blank:] ]","")
place3 <- gsub("����","", place3)
place3 <- gsub("����","", place3)
place3 <- gsub("�ֵ�","", place3)
place3 <- gsub("�̰�","", place3)
place3 <- gsub("����","", place3)
place3 <- gsub("����","", place3)
place3 <- gsub("�ֿ�","�ֿ���", place3)
place3 <- gsub("����","������", place3)
place3 <- gsub("����","���ڷ���", place3) 
place3 <- gsub("�۽���","�۽��ȷ���", place3) 
place3 <- gsub(paste(c("����","����"),collapse='|'), "���������", place3)
place3 <- gsub(paste(c("�Ѷ�","�Ѷ��"),collapse='|'), "�Ѷ��", place3)
place3 <- gsub(paste(c("���","����"),collapse='|'), "����", place3)
place3 <- gsub(paste(c("����","������"),collapse='|'), "������", place3)
place3 <- gsub(paste(c("õ��","����","õ��������"),collapse='|'), "õ��������", place3)
place3 <- gsub(paste(c("�ֻ�","����","�ֻ�����"),collapse='|'), "�ֻ�����", place3)
place3 <- gsub(paste(c("����","���屼"),collapse='|'), "���屼", place3)

wordcount2 <- table(place3 ) 
head(sort(wordcount2, decreasing=T),100)

#�ҿ� ���� ��� �Ʒ��� ���� �����մϴ�
txt <- readLines("���ֵ������ڽ�gsub.txt")
txt
cnt_txt <- length(txt)
cnt_txt

for( i in 1:cnt_txt) {
      place3 <-gsub((txt[i]),"",place3)     
      }

place4 <- Filter(function(x) {nchar(x) >= 2 & nchar(x) <= 10} ,place3)

wordcount3 <- table(place4) 
head(sort(wordcount3, decreasing=T),30)

# Step 8. ����� �ܾ���� ���� Ŭ����� �ð�ȭ �ϱ� 
palete <- brewer.pal(7,"Set1") 
wordcloud(names(wordcount3),freq=wordcount3,scale=c(5,1),rot.per=0.25,min.freq=4,
  random.order=F,random.color=T,colors=palete)

# ���� : Step 9. wordcloud2 ( ) ��Ű���� �ð�ȭ �ϱ�
install.packages("wordcloud2")
library(wordcloud2)
wordcount4 <- head(sort(wordcount3, decreasing=T),100)
wordcloud2(wordcount4,gridSize=1,size=0.5,shape="star")

# ������ �м��ǹ� �Ǳ���� �����Ͻ� �е��� �Ʒ� ������ �߰��� �����ϼ���.

# �Ʒ� �������� ������ �����Ǽ����ȣ.csv ������ �Ǳ� ������_1 �� �����ؾ� �մϴ�.
# �Ʒ��� ���ϵ��� ��� �۾� ���丮�� ����˴ϴ�

getwd( )  #  �۾� ���丮 Ȯ���ϱ�
data11 <- head(sort(wordcount3, decreasing=T),40)
write.csv(data11 , '�����Ǽ����ȣ.csv')

#�Ʒ� �������� ������ ������ �����ȣ.png ������ �Ǳ� ������_2 �� �����ؾ� �մϴ�. 
savePlot('�����Ǽ����ȣ.png' , type='png')
