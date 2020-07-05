df <- read.csv("Final_training_set.csv",header = TRUE)


######install libraries#######
install.packages('tm')
install.packages('rebus')
install.packages('SnowballC')
install.packages('qdap')
install.packages('caret')
install.packages('caretEnsemble')
install.packages('e1071')
install.packages('randomForest')

library('tm')
library('rebus')
library('SnowballC')
library('qdap')
library('caret')
library('caretEnsemble')
library('e1071')
library('randomForest')

df<-transform(df, text = as.character(text)) #reviews column is of character type
#Detect non english text
non_alpha <- (unlist(str_match_all(df$text, "[^A-Z-^a-z]")))
unique(non_alpha)


#######Cleaning the text########
df$text<-lapply(df$text, function(x) gsub("[^\u0001-\u007F]+","", x)) #remove text which is not english
df$text <-gsub('http[[:alnum:]]*',' ',df$text) #remove website links
df$text <- tolower(df$text) #make the whole text to lower
df$text <- removeNumbers(df$text) #remove numbers from review
df$text <-gsub('[[:punct:] ]+',' ',df$text) #remove punctuation
df$text <- stripWhitespace(df$text) #remove extra whitestrip
df$text <- removeWords(df$text, stopwords("english")) #stop word removal
df$text <-SnowballC::wordStem(df$text, language = 'en') #stemming



#Detect non english text
non_alpha <- (unlist(str_match_all(df$text, "[^A-Z-^a-z]")))
unique(non_alpha)

####renaming few words#####
df$text <- df$text %>%    #rename few words
  str_replace_all("don", " do not ")
df$text <- df$text %>%
  str_replace_all("didn", " did not ")
df$text <- df$text %>%
  str_replace_all("wasn", " was not ")


df$text <- stripWhitespace(df$text) #remove extra space

df$word_count<-sapply(strsplit(df$text, " "), length) #get the length of reviews and store in a new column
df$text_len = str_length(df$text) #get the number of words in reviews and store in a new column

df$review_sentiment <- ifelse(df$stars>3, "positive", "negative") #label the dataset

#temp_freq <- freq_terms(df$text, 30) #get top 30 most frequent words
#custom stop word elimination
stopwords_freq<-c("food", "t", "s", "good", "place" ,"great", "not", "service", "like", "just", "time" ,"one", "back" ,"get", "go", "can" ,"do" ,"really", "will" ,"ordered", "us" ,"order" ,"restaurant", "chicken" ,"also", "got" ,"ve", "came", "even", "menu" ,"re" ,"m")
df$text <- removeWords(df$text, stopwords_freq)

review_corpus<-Corpus(VectorSource(df$text)) #corpus of reviews 
dtm_train <- DocumentTermMatrix(review_corpus) #document term matrix
dtm_corpus <- removeSparseTerms(dtm_train, 0.90)
dtm_train_matrix <- as.matrix(dtm_corpus)
dtm_train_matrix <- cbind(dtm_train_matrix,df$word_count,df$text_len)    #binding word_count and text_len
dtm_train_matrix <- cbind(dtm_train_matrix, df$review_sentiment) #bind the y label
colnames(dtm_train_matrix)[ncol(dtm_train_matrix)] <- "y" #rename the column

training_set <- as.data.frame(dtm_train_matrix) #convert to data frame
training_set$y <- as.factor(training_set$y)
training_set[,24] <- as.numeric(as.character(training_set[,24]))
training_set[,25] <- as.numeric(as.character(training_set[,25]))
training_set[sapply(training_set, is.factor)] <- lapply(training_set[sapply(training_set, is.factor)], as.integer)
#unique(training_set$y)
training_set<-transform(training_set, y = as.factor(y))
set.seed(123)

#remove the training data and the remaining data is used in shiny application
train_model<-head(training_set,100000)
train_app <- tail(training_set, -100000)
test_set <- subset(train_app, select = -c(y) ) #remove the y label from the data to be used in shiny application

########Naive Bayes model######
review_model_bayes<-naiveBayes(y~., data=train_model) #training on naive bayes model
####Testing the model#####
review_model_bayes_predict<-predict(review_model_bayes, newdata=test_set, type="class") #test on the data 
#actual and predict dataframe
review_model_bayes_predict_df<- cbind(review_model_bayes_predict, train_app$y) 
colnames(review_model_bayes_predict_df)[1] = "predicted"  #dataframe containg actual value and predicted value
colnames(review_model_bayes_predict_df)[2]='original'
head(review_model_bayes_predict_df,5)
review_model_bayes_predict_df<-as.data.frame(review_model_bayes_predict_df)
review_model_bayes_predict_df<-transform(review_model_bayes_predict_df, original = as.factor(original))
review_model_bayes_predict_df<-transform(review_model_bayes_predict_df, predicted = as.factor(predicted))

#extract accuracy and kappa of the model
cm_naive<-confusionMatrix(review_model_bayes_predict_df$predicted, review_model_bayes_predict_df$original) 
str(cm_naive)
overall <- cm_naive$overall
naive_acc <- overall[['Accuracy']]
naive_kappa<- overall[['Kappa']]
confusionMatrix(review_model_bayes_predict_df$predicted, review_model_bayes_predict_df$original) #confusion matrix to judge model columns



#random forest model 
rf_dtm_100 <- randomForest(
  y ~ .,
  data=train_model,ntree=500   #randomforest model
)


#####Random Forest model#######
review_model_rf_predict<-predict(rf_dtm_100, newdata=test_set, type="class")
#actual and predict dataframe
review_model_rf_predict_df<- cbind(review_model_rf_predict, train_app$y)
colnames(review_model_rf_predict_df)[1] = "predicted"
colnames(review_model_rf_predict_df)[2]='original'
review_model_rf_predict_df<-as.data.frame(review_model_rf_predict_df)
review_model_rf_predict_df<-transform(review_model_rf_predict_df, original = as.factor(original))
review_model_rf_predict_df<-transform(review_model_rf_predict_df, predicted = as.factor(predicted))

#extract accuracy of the model
cm<-confusionMatrix(review_model_rf_predict_df$predicted, review_model_rf_predict_df$original) #confusion matrix for random forest
str(cm)
overall <- cm$overall
rf_acc <- overall[['Accuracy']]
rf_kappa<- overall[['Kappa']]
confusionMatrix(review_model_rf_predict_df$predicted, review_model_rf_predict_df$original) #confusion matrix for random forest

#####Saving the model and files######
saveRDS(rf_dtm_100, "./rf_dtm_500.rds")
write.csv(train_model,"./train_model.csv", row.names = TRUE)
write.csv(train_app,"./train_app.csv", row.names = TRUE) #data to be used in shiny app
saveRDS(review_model_bayes, "./review_model_bayes.rds")

# model data to plot
value_acc <- c(rf_acc*100,naive_acc*100)
value_kappa<- c(rf_kappa*100,naive_kappa*100)
names <- c("Random Forest","Naive Bayes")
df_res_kappa <- data.frame(names,value_kappa)
df_res_acc<- data.frame(names,value_acc)
#ploting kappa values for both the model
p<-ggplot(data=df_res_kappa, aes(x=names, y=value_kappa,fill=names)) +
  geom_bar(stat="identity")+
  scale_fill_discrete(name= "Models") +
  labs(x ="Names of model", y= "Kappa", title = "Kappa value of models")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 0.9))
p
#ploting the accuracy of both the models
p1 <-ggplot(data=df_res_acc, aes(x=names, y=value_acc,fill=names)) +
  geom_bar(stat="identity")+
  scale_fill_discrete(name= "Models") +
  labs(x ="Names of model", y= "Accuracy", title = "Accuracy value of models")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 0.9))
p1

