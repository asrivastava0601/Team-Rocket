install.packages("tm", dependencies=TRUE)
install.packages("rebus", dependencies=TRUE)
install.packages("SnowballC", dependencies=TRUE)
install.packages("caret", dependencies=TRUE)
install.packages("rebus", dependencies=TRUE)
install.packages("caretEnsemble", dependencies=TRUE)
install.packages("superml", dependencies=TRUE)
install.packages("randomForest", dependencies=TRUE)
install.packages("doParallel")

library('tm')
library('rebus')
library('SnowballC')
library("doParallel")
library('superml')
library('caret')
library('caretEnsemble')
library('e1071')
library('randomForest')

mesa_df <- az_df[az_df[,'city'] == 'Mesa',]    #taking reviews of a particular city mesa

mesa_reviews <- az_review[az_review$business_id %in% mesa_df$business_id,]

unique(mesa_reviews$stars) #unique review stars in mesa city 


mesa_reviews<-mesa_reviews[!(mesa_reviews$stars==3),] #removing 3 star reviews as they are neutral in most cases

head(mesa_reviews,5) #head of the mesa review

#######Cleaning text data #########
mesa_reviews$text<-lapply(mesa_reviews$text, function(x) gsub("[^\u0001-\u007F]+","", x))   #remove all the reviews which are not in english
mesa_reviews$text <-gsub('http[[:alnum:]]*',' ',mesa_reviews$text) #remove website link
mesa_reviews$text <- tolower(mesa_reviews$text) # convert text to lower case
mesa_reviews$text <- removeNumbers(mesa_reviews$text) #remove numbers
mesa_reviews$text <-gsub('[[:punct:] ]+',' ',mesa_reviews$text) #remove punction marks
mesa_reviews$text <- stripWhitespace(mesa_reviews$text) # remove extra white space if any
mesa_reviews$text <- removeWords(mesa_reviews$text, stopwords("english")) #stop word removal
mesa_reviews$text <-SnowballC::wordStem(mesa_reviews$text, language = 'en') #stemming


non_alpha <- (unlist(str_match_all(mesa_reviews$text, "[^A-Z-^a-z]"))) #checking if review from other language is present
#un <- (non_alpha)
unique(non_alpha)


mesa_reviews$text <- mesa_reviews$text %>%  #rename few words manually
  str_replace_all("don", " do not ")
mesa_reviews$text <- mesa_reviews$text %>%
  str_replace_all("didn", " did not ")
mesa_reviews$text <- mesa_reviews$text %>%
  str_replace_all("wasn", " was not ")


mesa_reviews$text <- stripWhitespace(mesa_reviews$text) #remove whitestrip 


mesa_reviews$word_count<-sapply(strsplit(mesa_reviews$text, " "), length)  #get the length of the review and store in a new column


mesa_reviews$text_len = str_length(mesa_reviews$text)#get the number of words of the review and store in a new column

mesa_reviews$review_sentiment <- ifelse(mesa_reviews$stars>3, "positive", "negative") #label the dataset


#Custom stop word elimination
stopwords_freq<-c("food", "t", "s", "good", "place" ,"great", "not", "service", "like", "just", "time" ,"one", "back" ,"get", "go", "can" ,"do" ,"really", "will" ,"ordered", "us" ,"order" ,"restaurant", "chicken" ,"also", "got" ,"ve", "came", "even", "menu" ,"re" ,"m")
mesa_reviews$text <- removeWords(mesa_reviews$text, stopwords_freq)

#Generating tfidf
tfv <- TfIdfVectorizer$new(max_features = 25, remove_stopwords = FALSE) #tf
tf_mat <- tfv$fit_transform(mesa_reviews$text)#tfidf
tf_mat<-as.data.frame(tf_mat)#convert the tfidf to dataframe
tf_mat <- cbind(tf_mat,mesa_reviews$word_count,mesa_reviews$text_len) #binding the reviews length and word count in the dataframe 
training_set_tfidf <- cbind(tf_mat, mesa_reviews$review_sentiment) #binding the label

#head(training_set_tfidf,5) #the training set

#renaming few columns
colnames(training_set_tfidf)[26] = "word_count"
colnames(training_set_tfidf)[27] = "text_len"
colnames(training_set_tfidf)[28] = "y"

#dividing the data in traing and testing
train_model_tfidf<-head(training_set_tfidf,10000)  
train_app_tfidf <- tail(training_set_tfidf, -2897)
test_set_tfidf <- subset(train_app_tfidf, select = -c(y) )#remove y column from test set
#Naive Bayes tfidf model
bayes_tfidf<-naiveBayes(y~., data=train_model_tfidf) #naive bayes model

####testing the model####
bayes_tfidf_predict<-predict(bayes_tfidf, newdata=test_set_tfidf, type="class") #predict on test set
bayes_tfidf_predict_df<- cbind(bayes_tfidf_predict, train_app_tfidf$y)
colnames(bayes_tfidf_predict_df)[1] = "predicted"
colnames(bayes_tfidf_predict_df)[2]='original'
bayes_tfidf_predict_df<-as.data.frame(bayes_tfidf_predict_df)
bayes_tfidf_predict_df<-transform(bayes_tfidf_predict_df, original = as.factor(original))
bayes_tfidf_predict_df<-transform(bayes_tfidf_predict_df, predicted = as.factor(predicted)) #bind the result in a dataframe

#extract accuracy of the model
cm_bayes_tfidf<-confusionMatrix(bayes_tfidf_predict_df$predicted, bayes_tfidf_predict_df$original) #confusion matrix for random forest
str(cm_bayes_tfidf)
overall <- cm_bayes_tfidf$overall
bayes_tfidf_acc <- overall[['Accuracy']]
bayes_tfidf_kappa<- overall[['Kappa']]
confusionMatrix(bayes_tfidf_predict_df$predicted, bayes_tfidf_predict_df$original) #confusion matrix 

saveRDS(bayes_tfidf, "./bayes_tfidf.rds")

#train the random forest model using tfidf
rf_tfidf <- randomForest(
  y ~ .,
  data=train_model_tfidf
)

rf_tfidf_predict<-predict(rf_tfidf, newdata=test_set_tfidf, type="class") #predict on the model

#bind the result in a dataframe
rf_tfidf_predict_df<- cbind(rf_tfidf_predict, train_app_tfidf$y)
colnames(rf_tfidf_predict_df)[1] = "predicted"
colnames(rf_tfidf_predict_df)[2]='original'
rf_tfidf_predict_df<-as.data.frame(rf_tfidf_predict_df)
rf_tfidf_predict_df<-transform(rf_tfidf_predict_df, original = as.factor(original))
rf_tfidf_predict_df<-transform(rf_tfidf_predict_df, predicted = as.factor(predicted))

#extract accuracy of the model
cm_rf_tfidf<-confusionMatrix(rf_tfidf_predict_df$predicted, rf_tfidf_predict_df$original) #confusion matrix for random forest
str(cm_rf_tfidf)
overall <- cm_rf_tfidf$overall
rf_tfidf_acc <- overall[['Accuracy']]
rf_tfidf_kappa<- overall[['Kappa']]
confusionMatrix(rf_tfidf_predict_df$predicted, rf_tfidf_predict_df$original) #confusinon matrix

saveRDS(rf_tfidf, "./rf_tfidf.rds")


#get the DTM of reviews
review_corpus<-Corpus(VectorSource(mesa_reviews$text))
dtm_train <- DocumentTermMatrix(review_corpus)
dtm_corpus <- removeSparseTerms(dtm_train, 0.90)
dtm_train_matrix <- as.matrix(dtm_corpus)
dtm_train_matrix <- cbind(dtm_train_matrix,mesa_reviews$word_count,mesa_reviews$text_len) # #binding the reviews length and word count in the dataframe 
dtm_train_matrix <- cbind(dtm_train_matrix, mesa_reviews$review_sentiment) # binding the label
colnames(dtm_train_matrix)[ncol(dtm_train_matrix)] <- "y" #rename the column
training_set_dtm <- as.data.frame(dtm_train_matrix)
training_set_dtm$y <- as.factor(training_set_dtm$y)

colnames(training_set_dtm)[21] = "y"
colnames(training_set_dtm)[19] = "text_len"
colnames(training_set_dtm)[20] = "word_count"
training_set_dtm[,20] <- as.numeric(as.character(training_set_dtm[,20]))
training_set_dtm[,19] <- as.numeric(as.character(training_set_dtm[,19]))

##trinaing set of DTM###
training_set_dtm[sapply(training_set_dtm, is.factor)] <- lapply(training_set_dtm[sapply(training_set_dtm, is.factor)], as.integer)
training_set_dtm<-transform(training_set_dtm, y = as.factor(y))


#train test split for DTM data
train_model_dtm<-head(training_set_dtm,10000) 
train_app_dtm <- tail(training_set_dtm, -2897)

#Naive bayes model using DTM
bayes_dtm<-naiveBayes(y~., data=train_model_dtm) #train model
test_set_dtm <- subset(train_app_dtm, select = -c(y) ) #remove y label from the test set
bayes_dtm_predict<-predict(bayes_dtm, newdata=test_set_dtm, type="class") #predict the test 
bayes_dtm_predict_df<- cbind(bayes_dtm_predict, train_app_dtm$y)
colnames(bayes_dtm_predict_df)[1] = "predicted"
colnames(bayes_dtm_predict_df)[2]='original'
bayes_dtm_predict_df<-as.data.frame(bayes_dtm_predict_df)
bayes_dtm_predict_df<-transform(bayes_dtm_predict_df, original = as.factor(original))
bayes_dtm_predict_df<-transform(bayes_dtm_predict_df, predicted = as.factor(predicted))

#extract accuracy of the model
cm_bayes_dtm<-confusionMatrix(bayes_dtm_predict_df$predicted, bayes_dtm_predict_df$original) #confusion matrix for random forest
str(cm_bayes_dtm)
overall <- cm_bayes_dtm$overall
bayes_dtm_acc <- overall[['Accuracy']]
bayes_dtm_kappa<- overall[['Kappa']]

confusionMatrix(bayes_dtm_predict_df$predicted, bayes_dtm_predict_df$original) #confusion matrix

saveRDS(bayes_dtm, "./bayes_dtm.rds")

####Random forest model using tfidf####

rf_dtm <- randomForest(
  y ~ .,
  data=train_model_dtm
)


rf_dtm_predict<-predict(rf_dtm, newdata=test_set_dtm, type="class")

#get the result in a dataframe
rf_dtm_predict_df<- cbind(rf_dtm_predict, train_app_dtm$y)
colnames(rf_dtm_predict_df)[1] = "predicted"
colnames(rf_dtm_predict_df)[2]='original'
rf_dtm_predict_df<-as.data.frame(rf_dtm_predict_df)
rf_dtm_predict_df<-transform(rf_dtm_predict_df, original = as.factor(original))
rf_dtm_predict_df<-transform(rf_dtm_predict_df, predicted = as.factor(predicted))


#extract accuracy of the model
cm_rf_dtm<-confusionMatrix(rf_dtm_predict_df$predicted, rf_dtm_predict_df$original) #confusion matrix for random forest
str(cm_rf_dtm)
overall <- cm_rf_dtm$overall
rf_dtm_acc <- overall[['Accuracy']]
rf_dtm_kappa<- overall[['Kappa']]
confusionMatrix(rf_dtm_predict_df$predicted, rf_dtm_predict_df$original)

#save the model
saveRDS(rf_dtm, "./rf_dtm.rds")

####saving the data used for training and testing##
write.csv(train_app_dtm,"./train_app_dtm.csv", row.names = TRUE)
write.csv(train_model_dtm,"./train_model_dtm.csv", row.names = TRUE)
write.csv(train_model_tfidf,"./train_model_tfidf.csv", row.names = TRUE)
write.csv(train_app_tfidf,"./train_app_tfidf.csv", row.names = TRUE)

value_acc<- c(bayes_tfidf_acc,bayes_dtm_acc,rf_tfidf_acc,rf_dtm_acc)
names <- c("Bayes using tf-idf","Bayes using DTM","Random Forest using tf-idf", "Random Forest using DTM")
df_res_acc <- data.frame(names,value_acc)
value_kappa <- c(bayes_tfidf_kappa,bayes_dtm_kappa,rf_tfidf_kappa,rf_dtm_kappa)
df_res_kappa <- data.frame(names,value_kappa)



#Bar plot for kappa value
library(ggplot2)
# Basic barplot
p<-ggplot(data=df_res_kappa, aes(x=names, y=value_kappa,fill=names)) +
  geom_bar(stat="identity")+
  scale_fill_discrete(name = "Models") +
  labs(x ="Names of model", y= "Kappa", title = "Kappa value of various model")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 0.9))
p



#Bar plot for accuracy
library(ggplot2)
# Basic barplot
p1<-ggplot(data=df_res_acc, aes(x=names, y=value_acc,fill=names)) +
  geom_bar(stat="identity")+
  scale_fill_discrete(name = "Models") +
  labs(x ="Names of model", y= "Accuracy", title = "Accuracy of various model")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 0.9))
p1
