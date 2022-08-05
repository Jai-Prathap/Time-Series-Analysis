#Assignment Task 1
setwd("E:/R/RStudio")
data=read.csv("PCE.csv")
colSums(is.na(data))
library(imputeTS)
library(dplyr)
library(VIM)
library(ggplot2)
aggr(data,numbers=TRUE, prop=FALSE)
datasetM<- ts(data$PCE, start=c(1959,1), end=c(2021,12), frequency=12)
ts<-na_kalman(datasetM, model="auto.arima")
library(forecast)
par(mfrow=c(2,2))
plot(ts)
autoplot(ts,xlab = "Year",ylab = "PCE")
train<-window(ts, start=c(1959, 1), end=c(2003, 6), frequency=12)
test <- window(ts, start = c(2003,7), end = c(2021,12),frequency=12)
plot(ts,ylab="PCE")


#AVG
fcavg<-meanf(train, h = 221)
accuracy(fcavg,ts)
plot(fcavg,ylab="PCE",xlab="Year")
#Naive
fcnaive <- naive(train, h = 221)
accuracy(fcnaive,ts)
plot(fcnaive,ylab="PCE",xlab="Year")
#SeasonalNaive
fcsnaive <- snaive(train, h = 221)
accuracy(fcsnaive,ts)
plot(fcsnaive,ylab="PCE",xlab="Year")
#Drift
fcdrift<-rwf(train, h = 221, drift = T)
accuracy(fcdrift,ts)
plot(fcdrift,ylab="PCE",xlab="Year")
#EXPO
fcexpo <- ets(train)
plot(fcexpo)
#arima
fcarima<-auto.arima(train)
forecast(fcarima,xreg = test)
plot(fcarima)
############
accuracy(fcdrift, test)
accuracy(forecast(fcexpo, h = 221)$mean, test)
accuracy(forecast(fcarima, h=221)$mean, test)
#####
autoplot(ts) +
  autolayer(rwf(train, drift=TRUE, h=221),
            series="Drift", PI=FALSE) + 
  scale_color_manual(values=c("red","cyan","green"))+
  autolayer(forecast(fcexpo, h = 221)$mean,
            series="Exponential moving average") +
  autolayer(forecast(fcarima, h=221)$mean,
            series="ARIMA")+ylab("PCE")+xlab("Year")
##############################################
###Simple forecasting methods
# drift method ------consider seasonality and trend
fcdr <- rwf(train, drift=TRUE, h=221)	
accuracy(fcdr, test)
accuracy(forecast(fcexpo, h = 221)$mean, test)
accuracy(forecast(fcarima, h=221)$mean, test)

# October 2022 estimate

prediction<- predict(fcarima,232)
prediction$pred[length(prediction$pred)]

#One-step Ahead
fcari1 <- Arima(test, model=fcarima)
fc1 <- window(fitted(fcari1), start=c(2003,7))
plot(ts,ylab="PCE",xlab="Year")
lines(fc1,col="green")

fcex <- ets(ts, model=fcexpo,use.initial.values = TRUE)
fc3 <- window(fitted(fcex), start=c(2003,7))
plot(ts,ylab="PCE",xlab="Year")
lines(fc3,col="red")

fcdrift <- rwf(ts, h=221, drift = TRUE)
fc4 <- window(fitted(fcdrift), start = c(2003,7))
plot(ts,ylab="PCE",xlab="Year")
lines(fc4,col="cyan")


#Task 2

library(VIM)
library(MASS)
library(stringr)
library(ggplot2) 
library(LDAvis) 
library(tm)
library(textclean)
library(topicmodels) 
library(tidyr)
library(tidytext)
library(tidyverse)
library(textmineR)
library(wordcloud)
library(dplyr)
library(jsonlite)
library(servr) 
#Gathering the data
data <- fromJSON("https://query.data.world/s/4ria2tfww73wmhfzke5z2w4zlez2re")
set.seed(386)
sample_data<-sample_n(data, 5000)
summary(sample_data)
colSums(is.na(sample_data))
aggr(sample_data,numbers=TRUE, prop=FALSE)

# Cleaning the data
cleaning <- function(x){
  x <- as.character(x)
  x <- x %>%
    str_to_lower() %>%  
    replace_contraction() %>% 
    replace_internet_slang() %>%
    replace_emoji() %>%
    replace_emoticon() %>% 
    replace_hash(replacement = "") %>% 
    replace_word_elongation() %>% 
    replace_number(remove = T) %>% 
    replace_date(replacement = "") %>% 
    replace_time(replacement = "") %>% 
    str_remove_all(pattern = "[[:punct:]]") %>% 
    str_remove_all(pattern = "[^\\s][0-9][^\\s]") %>% 
    str_squish() %>% 
    str_trim() 
  xdtm <- VCorpus(VectorSource(x)) %>%
    tm_map(removeWords, stopwords("en"))
  return(DocumentTermMatrix(xdtm,control = list(lemma=TRUE,stopwords=TRUE)))
  
}
#Splitting the verified data into good, bad, satisfactory 
sample_data_verified_purchase<- sample_data[sample_data$verified_purchase == "Verified Purchase",]
sample_data_not_verified_purchase<- sample_data[sample_data$verified_purchase == "na",]

sample_data_good <-sample_data_verified_purchase[sample_data_verified_purchase$
                                                   review_rating == "5.0 out of 5 stars"|
                                                   sample_data_verified_purchase$
                                                   review_rating == "4.0 out of 5 stars",]
sample_data_average <- sample_data_verified_purchase[sample_data_verified_purchase$
                                                       review_rating == "3.0 out of 5 stars",]
sample_data_bad <-sample_data_verified_purchase[sample_data_verified_purchase$
                                                  review_rating == "1.0 out of 5 stars"|
                                                  sample_data_verified_purchase$review_rating
                                                == "2.0 out of 5 stars",]

# Good
good_dtm <- cleaning(sample_data_good$review_text)
good_freqterm <- findFreqTerms(good_dtm,50)
good_dtm <- good_dtm[,dood_freqterm]
good_rownum <- apply(good_dtm,1,sum)
good_dtm <- good_dtm[good_rownum>0,]


dtm.new_good<-as.matrix(good_dtm)
iter <- 2000 
coherence_good<- c()
for (i in (3:20)){
  ldaOut_good <-LDA(good_dtm,i, method="Gibbs",
                    control=list(iter=iter,seed=386))
  phi_good <- topicmodels::posterior(ldaOut_good)$terms %>% as.matrix
  theta_good <- topicmodels::posterior(ldaOut_good)$topics %>% as.matrix 
  coherence_one_good <- mean(textmineR::CalcProbCoherence(phi = phi_good,dtm = dtm.new_good)) 
  coherence_good<-append(coherence_good,coherence_one_good)
}
k_good <- c(3:20)[which.max(coherence_good)] 
coherence_mat <- data.frame(k = c(3:20), coherence = coherence_good,
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence_good)) + geom_point() +
  geom_line(group = 1)+
  ggtitle(" Coherence Score for good review") + theme_minimal() + 
  scale_x_continuous(breaks = seq(1,31,1)) + ylab("Coherence")

lda_good <- LDA(good_dtm,k = k_good,control = list(seed = 386))
topic_good <- tidy(lda_good,matrix = "beta")

top_terms_good <- topic_good %>%
  group_by(topic) %>%
  top_n(5,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)

plot_topic_good <- top_terms_good %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

plot_topic_good

dtm_r_good<-Matrix::sparseMatrix(i = good_dtm$i, j = good_dtm$j,
                                 x = good_dtm$v, dims = c(good_dtm$nrow, good_dtm$ncol),
                                 dimnames = good_dtm$dimnames)

mod_lda_good <- FitLdaModel(dtm = dtm_r_good,
                            k = k_good, # number of topic
                            iterations = 500,
                            burnin = 180,
                            alpha = 0.1,beta = 0.05)

mod_lda_good$top_terms <- GetTopTerms(phi = mod_lda_good$phi,M = 5)

mod_lda_good$summary <- data.frame(topic = rownames(mod_lda_good$phi),
                                   coherence = round(mod_lda_good$coherence,3),
                                   top_terms = apply(mod_lda_good$top_terms,2,function(x){paste(x,collapse = ", ")}))

modsum_good <- mod_lda_good$summary %>%
  `rownames<-`(NULL)

modsum_good %>% pivot_longer(cols = coherence) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Best topics by coherence score",
       subtitle = "Text review with good rating",
       x = "Topics", y = "Value")


#Satisfactory Data
dtm_average <- cleaning(sample_data_average$review_text)
freqterm_average <- findFreqTerms(dtm_average,50)
dtm_average <- dtm_average[,freqterm_average]
rownum_average <- apply(dtm_average,1,sum)
dtm_average <- dtm_average[rownum_average>0,]

dtm.new_average<-as.matrix(dtm_average)
iter <- 2000 
coherence_average<- c()
for (i in (3:20)){
  ldaOut_average <-LDA(dtm_average,i, method="Gibbs",
                       control=list(iter=iter,seed=386))
  phi_average <- topicmodels::posterior(ldaOut_average)$terms %>% as.matrix
  theta_average <- topicmodels::posterior(ldaOut_average)$topics %>% as.matrix 
  coherence_one_average <- mean(textmineR::CalcProbCoherence(phi = phi_average, dtm = dtm.new_average)) 
  coherence_average<-append(coherence_average,coherence_one_average)
}
k_average <- c(3:20)[which.max(coherence_average)] 

coherence_mat_average <- data.frame(k = c(3:20), coherence = coherence_average,
                                    stringsAsFactors = FALSE)
ggplot(coherence_mat_average, aes(x = k, y = coherence_average)) + geom_point() +
  geom_line(group = 1)+
  ggtitle(" Coherence Score for Satisfactory  review") + theme_minimal() + 
  scale_x_continuous(breaks = seq(1,31,1)) + ylab("Coherence")


lda_average <- LDA(dtm_average,k = k_average,control = list(seed = 386))
topic_average <- tidy(lda_average,matrix = "beta")

top_terms_average <- topic_average %>%
  group_by(topic) %>%
  top_n(10,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_average <- top_terms_average %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

plot_topic_average

dtm_r_average<-Matrix::sparseMatrix(i = dtm_average$i, j = dtm_average$j,
                                    x = dtm_average$v, dims = c(dtm_average$nrow, dtm_average$ncol),
                                    dimnames = dtm_average$dimnames)

mod_lda_average <- FitLdaModel(dtm = dtm_r_average,
                               k = k_average, # number of topic
                               iterations = 500,
                               burnin = 180,
                               alpha = 0.1,beta = 0.05)

mod_lda_average$top_terms <- GetTopTerms(phi = mod_lda_average$phi,M = 10)

mod_lda_average$summary <- data.frame(topic = rownames(mod_lda_average$phi),
                                      coherence = round(mod_lda_average$coherence,3),
                                      top_terms = apply(mod_lda_average$top_terms,2,function(x){paste(x,collapse = ", ")}))

modsum_average <- mod_lda_average$summary %>%
  `rownames<-`(NULL)

modsum_average %>% pivot_longer(cols = coherence) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Best topics by coherence score",
       subtitle = "Text review with satisfactory rating",
       x = "Topics", y = "Value")
#Bad Data

dtm_bad <- cleaning(sample_data_bad$review_text)
freqterm_bad <- findFreqTerms(dtm_bad,5)
dtm_bad <- dtm_bad[,freqterm_bad]
rownum_bad <- apply(dtm_bad,1,sum)
dtm_bad <- dtm_bad[rownum_bad>0,]

dtm.new_bad<-as.matrix(dtm_bad)
iter <- 2000 
coherence_bad<- c()
for (i in (3:20)){
  ldaOut_bad <-LDA(dtm_bad,i, method="Gibbs",
                   control=list(iter=iter,seed=386))
  phi_bad <- topicmodels::posterior(ldaOut_bad)$terms %>% as.matrix
  theta_bad <- topicmodels::posterior(ldaOut_bad)$topics %>% as.matrix 
  coherence_one_bad <- mean(textmineR::CalcProbCoherence(phi = phi_bad, dtm = dtm.new_bad)) 
  coherence_bad<-append(coherence_bad,coherence_one_bad)
}
k_bad <- c(3:20)[which.max(coherence_bad)] 
coherence_mat_bad <- data.frame(k = c(3:20), coherence = coherence_bad,
                                stringsAsFactors = FALSE)
ggplot(coherence_mat_bad, aes(x = k, y = coherence_bad)) + geom_point() +
  geom_line(group = 1)+
  ggtitle(" Coherence Score for bad review") + theme_minimal() + 
  scale_x_continuous(breaks = seq(1,31,1)) + ylab("Coherence")

lda_bad <- LDA(dtm_bad,k = k_bad,control = list(seed = 386))
topic_bad <- tidy(lda_bad,matrix = "beta")

top_terms_bad <- topic_bad %>%
  group_by(topic) %>%
  top_n(10,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_bad <- top_terms_bad %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

plot_topic_bad

dtm_r_bad<-Matrix::sparseMatrix(i = dtm_bad$i, j = dtm_bad$j,
                                x = dtm_bad$v, dims = c(dtm_bad$nrow, dtm_bad$ncol),
                                dimnames = dtm_bad$dimnames)

mod_lda_bad <- FitLdaModel(dtm = dtm_r_bad,
                           k = k_bad, 
                           iterations = 500,
                           burnin = 180,
                           alpha = 0.1,beta = 0.05)

mod_lda_bad$top_terms <- GetTopTerms(phi = mod_lda_bad$phi,M = 10)

mod_lda_bad$summary <- data.frame(topic = rownames(mod_lda_bad$phi),
                                  coherence = round(mod_lda_bad$coherence,3),
                                  top_terms = apply(mod_lda_bad$top_terms,2,function(x){paste(x,collapse = ", ")}))

modsum_bad <- mod_lda_bad$summary %>%
  `rownames<-`(NULL)

modsum_bad %>% pivot_longer(cols = coherence) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Best topics by coherence score",
       subtitle = "Text review with bad rating",
       x = "Topics", y = "Value")

