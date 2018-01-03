
#Reading the NFL dataset .csv file

proj_dataset=read.csv("NFL_SocialMedia_sample_data1.csv")

#Loading tidyr package

library(tidyr)

#Separating out tstamp column to log and time

proj_dataset=separate(proj_dataset, tstamp, c("log","time"), sep="T")

#text mining the dataset

library(tm)
proj_dataset_content=proj_dataset$content
proj_dataset_content_corpus=VectorSource(proj_dataset_content)
proj_dataset_content_corpus=VCorpus(proj_dataset_content_corpus)
 
clean_corpus=function(corpus){
  corpus=tm_map(corpus, content_transformer(tolower))
  corpus=tm_map(corpus, removeWords, stopwords("en"))
  corpus=tm_map(corpus, removePunctuation)
  corpus=tm_map(corpus, removeNumbers)
  corpus=tm_map(corpus, stripWhitespace)
}


clean_nfl_content=clean_corpus(proj_dataset_content_corpus)

#Creating Document Term Matrix
nfl_dtm=DocumentTermMatrix(clean_nfl_content)
nfl_m=as.matrix(nfl_dtm)

#Identifying Term Frequency
term_frequency=colSums(nfl_m)
term_frequency=sort(term_frequency,decreasing = TRUE)
 
term_frequency_df=data.frame(term=names(term_frequency), num=term_frequency)

rownames(term_frequency_df)=1:nrow(term_frequency_df)

write.csv(term_frequency_df,"Term_Frequency.csv")

#creating the inverse document frequency

nfl_tfidf=weightTfIdf(nfl_dtm)

nfl_tfidf_m=as.matrix(nfl_tfidf)

rownames(nfl_tfidf_m)=1:nrow(nfl_tfidf_m)

#Normalize the vectors

func_norm=function(x){x/sqrt(sum(x^2))}

nfl_tfidf_m_norm=func_norm(nfl_tfidf_m)

#Kmeans Clustering

km_out=kmeans(nfl_tfidf_m_norm,10)

y_kmeans=km_out$cluster


cluster_out=data.frame(log=proj_dataset$content, cluster=y_kmeans)

write.csv(cluster_out,"cluster_out.csv")


cluster1=subset(cluster_out, subset = km_out$cluster==1)
cluster2=subset(cluster_out, subset = km_out$cluster==2)
cluster3=subset(cluster_out, subset = km_out$cluster==3)
cluster4=subset(cluster_out, subset = km_out$cluster==4)
cluster5=subset(cluster_out, subset = km_out$cluster==5)
cluster6=subset(cluster_out, subset = km_out$cluster==6)
cluster7=subset(cluster_out, subset = km_out$cluster==7)
cluster8=subset(cluster_out, subset = km_out$cluster==8)
cluster9=subset(cluster_out, subset = km_out$cluster==9)
cluster10=subset(cluster_out, subset = km_out$cluster==10)


#Cluster1 Top Words

cluster1_content=cluster1$log
cluster1_content_corpus=VectorSource(cluster1_content)
cluster1_content_corpus=VCorpus(cluster1_content_corpus)
cluster1_cleaned=clean_corpus(cluster1_content_corpus)
cluster1_dtm=DocumentTermMatrix(cluster1_cleaned)
cluster1_m=as.matrix(cluster1_dtm)
cluster1_term_frequency=colSums(cluster1_m)
cluster1_term_frequency=sort(cluster1_term_frequency, decreasing = TRUE)
cluster1_df= data.frame(term=names(cluster1_term_frequency),num=cluster1_term_frequency)
library(wordcloud)
wordcloud(cluster1_df$term,cluster1_df$num)


#Cluster2

cluster2_content=cluster2$log
cluster2_content_corpus=VectorSource(cluster2_content)
cluster2_content_corpus=VCorpus(cluster2_content_corpus)
cluster2_cleaned=clean_corpus(cluster2_content_corpus)
cluster2_dtm=DocumentTermMatrix(cluster2_cleaned)
cluster2_m=as.matrix(cluster2_dtm)
cluster2_term_frequency=colSums(cluster2_m)
cluster2_term_frequency=sort(cluster2_term_frequency, decreasing = TRUE)
cluster2_df= data.frame(term=names(cluster2_term_frequency),num=cluster2_term_frequency)
library(wordcloud)
wordcloud(cluster2_df$term,cluster2_df$num)

#Cluster3

cluster3_content=cluster3$log
cluster3_content_corpus=VectorSource(cluster3_content)
cluster3_content_corpus=VCorpus(cluster3_content_corpus)
cluster3_cleaned=clean_corpus(cluster3_content_corpus)
cluster3_dtm=DocumentTermMatrix(cluster3_cleaned)
cluster3_m=as.matrix(cluster3_dtm)
cluster3_term_frequency=colSums(cluster3_m)
cluster3_term_frequency=sort(cluster3_term_frequency, decreasing = TRUE)
cluster3_df= data.frame(term=names(cluster3_term_frequency),num=cluster3_term_frequency)
library(wordcloud)
wordcloud(cluster3_df$term,cluster3_df$num)

#Cluster4

cluster4_content=cluster4$log
cluster4_content_corpus=VectorSource(cluster4_content)
cluster4_content_corpus=VCorpus(cluster4_content_corpus)
cluster4_cleaned=clean_corpus(cluster4_content_corpus)
cluster4_dtm=DocumentTermMatrix(cluster4_cleaned)
cluster4_m=as.matrix(cluster4_dtm)
cluster4_term_frequency=colSums(cluster4_m)
cluster4_term_frequency=sort(cluster4_term_frequency, decreasing = TRUE)
cluster4_df= data.frame(term=names(cluster4_term_frequency),num=cluster4_term_frequency)
library(wordcloud)
wordcloud(cluster4_df$term, cluster4_df$num)

#Cluster5

cluster5_content=cluster5$log
cluster5_content_corpus=VectorSource(cluster5_content)
cluster5_content_corpus=VCorpus(cluster5_content_corpus)
cluster5_cleaned=clean_corpus(cluster5_content_corpus)
cluster5_dtm=DocumentTermMatrix(cluster5_cleaned)
cluster5_m=as.matrix(cluster5_dtm)
cluster5_term_frequency=colSums(cluster5_m)
cluster5_term_frequency=sort(cluster5_term_frequency, decreasing = TRUE)
cluster5_df= data.frame(term=names(cluster5_term_frequency),num=cluster5_term_frequency)
library(wordcloud)
wordcloud(cluster5_df$term, cluster5_df$num)

#Cluster6

cluster6_content=cluster6$log
cluster6_content_corpus=VectorSource(cluster6_content)
cluster6_content_corpus=VCorpus(cluster6_content_corpus)
cluster6_cleaned=clean_corpus(cluster6_content_corpus)
cluster6_dtm=DocumentTermMatrix(cluster6_cleaned)
cluster6_m=as.matrix(cluster6_dtm)
cluster6_term_frequency=colSums(cluster6_m)
cluster6_term_frequency=sort(cluster6_term_frequency, decreasing = TRUE)
cluster6_df= data.frame(term=names(cluster6_term_frequency),num=cluster6_term_frequency)
library(wordcloud)
wordcloud(cluster6_df$term, cluster6_df$num)


#Cluster7

cluster7_content=cluster7$log
cluster7_content_corpus=VectorSource(cluster7_content)
cluster7_content_corpus=VCorpus(cluster7_content_corpus)
cluster7_cleaned=clean_corpus(cluster7_content_corpus)
cluster7_dtm=DocumentTermMatrix(cluster7_cleaned)
cluster7_m=as.matrix(cluster7_dtm)
cluster7_term_frequency=colSums(cluster7_m)
cluster7_term_frequency=sort(cluster7_term_frequency, decreasing = TRUE)
cluster7_df= data.frame(term=names(cluster7_term_frequency),num=cluster7_term_frequency)
library(wordcloud)
wordcloud(cluster7_df$term, cluster7_df$num)

#Cluster8
cluster8_content=cluster8$log
cluster8_content_corpus=VectorSource(cluster8_content)
cluster8_content_corpus=VCorpus(cluster8_content_corpus)
cluster8_cleaned=clean_corpus(cluster8_content_corpus)
cluster8_dtm=DocumentTermMatrix(cluster8_cleaned)
cluster8_m=as.matrix(cluster8_dtm)
cluster8_term_frequency=colSums(cluster8_m)
cluster8_term_frequency=sort(cluster8_term_frequency, decreasing = TRUE)
cluster8_df= data.frame(term=names(cluster8_term_frequency),num=cluster8_term_frequency)
library(wordcloud)
wordcloud(cluster8_df$term, cluster8_df$num)

#Cluster9
cluster9_content=cluster9$log
cluster9_content_corpus=VectorSource(cluster9_content)
cluster9_content_corpus=VCorpus(cluster9_content_corpus)
cluster9_cleaned=clean_corpus(cluster9_content_corpus)
cluster9_dtm=DocumentTermMatrix(cluster9_cleaned)
cluster9_m=as.matrix(cluster9_dtm)
cluster9_term_frequency=colSums(cluster9_m)
cluster9_term_frequency=sort(cluster9_term_frequency, decreasing = TRUE)
cluster9_df= data.frame(term=names(cluster9_term_frequency),num=cluster9_term_frequency)
library(wordcloud)
wordcloud(cluster9_df$term, cluster9_df$num)

#Cluster10

cluster10_content=cluster10$log
cluster10_content_corpus=VectorSource(cluster10_content)
cluster10_content_corpus=VCorpus(cluster10_content_corpus)
cluster10_cleaned=clean_corpus(cluster10_content_corpus)
cluster10_dtm=DocumentTermMatrix(cluster10_cleaned)
cluster10_m=as.matrix(cluster10_dtm)
cluster10_term_frequency=colSums(cluster10_m)
cluster10_term_frequency=sort(cluster10_term_frequency, decreasing = TRUE)
cluster10_df= data.frame(term=names(cluster10_term_frequency),num=cluster10_term_frequency)
library(wordcloud)
wordcloud(cluster10_df$term, cluster10_df$num)

cluster1_top=cluster1_term_frequency[1:5]
cluster2_top=cluster2_term_frequency[1:5]
cluster3_top=cluster3_term_frequency[1:5]
cluster4_top=cluster4_term_frequency[1:5]
cluster5_top=cluster5_term_frequency[1:5]
cluster6_top=cluster6_term_frequency[1:5]
cluster7_top=cluster7_term_frequency[1:5]
cluster8_top=cluster8_term_frequency[1:5]
cluster9_top=cluster9_term_frequency[1:5]
cluster10_top=cluster10_term_frequency[1:5]



cluster1_top=data.frame(word=names(cluster1_top), count=cluster1_top)
cluster2_top=data.frame(word=names(cluster2_top), count=cluster2_top)
cluster3_top=data.frame(word=names(cluster3_top), count=cluster3_top)
cluster4_top=data.frame(word=names(cluster4_top), count=cluster4_top)
cluster5_top=data.frame(word=names(cluster5_top), count=cluster5_top)
cluster6_top=data.frame(word=names(cluster6_top), count=cluster6_top)
cluster7_top=data.frame(word=names(cluster7_top), count=cluster7_top)
cluster8_top=data.frame(word=names(cluster8_top), count=cluster8_top)
cluster9_top=data.frame(word=names(cluster9_top), count=cluster9_top)
cluster10_top=data.frame(word=names(cluster10_top), count=cluster10_top)



topwords=data.frame(cluster1_top,cluster2_top,cluster3_top,cluster4_top,cluster5_top,cluster6_top,cluster7_top,cluster8_top,cluster9_top,cluster10_top)

write.csv(topwords,"TopWords.csv")



















