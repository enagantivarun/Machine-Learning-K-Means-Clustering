
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

write.csv(topwords,"TopWords.csv")



















