Data Cleansing
=============== 
To remove null values
--------------------------------

df$Post.Relevance[is.na(df$Post.Relevance)] <- 1
df$PostAccuracy[is.na(df$PostAccuracy)] <- 1
df$PostComplexity[is.na(df$PostComplexity)] <- 2
df$QuestionRelevance[is.na(df$QuestionRelevance)] <- 1
df$QuestionCompleteness[is.na(df$QuestionCompleteness)] <- 1
df$QMedicallyComplex[is.na(df$QMedicallyComplex)] <- 2
df$QAnswerable[is.na(df$QAnswerable)] <- 1
df$QMedical.Context[is.na(df$QMedical.Context)] <- 0
df$QTreatment.Available[is.na(df$QTreatment.Available)] <- 1
df$UserGender[is.na(df$UserGender)] <- 1
df$UserAge[is.na(df$UserAge)] <- median(df$UserAge,na.rm=T)
df$Total.number.of.posts[is.na(df$Total.number.of.posts)] <- mean(df$Total.number.of.posts,na.rm=T)
df$TotalLikes[is.na(df$TotalLikes)] <- mean(df$TotalLikes,na.rm=T)
df$HY.Scale[is.na(df$HY.Scale)] <- 2
df$No.of.Secondary.Conditions[is.na(df$No.of.Secondary.Conditions)] <- 0
df$Number.of.followers[is.na(df$Number.of.followers)] <- 0
df$Degre <- as.numeric(paste(df$Degre))
df$Degre[is.na(df$Degre)] <- mean(df$Degre,na.rm=T)
df$Betwe[is.na(df$Betwe)] <- mean(df$Betwe,na.rm=T)
df$Cliquepart <- as.numeric(paste(df$Cliquepart))
df$Cliquepart[is.na(df$Cliquepart)] <- mean(df$Cliquepart,na.rm=T)
df$likeperpost <- as.numeric(paste(df$likeperpost))
df$likeperpost[is.na(df$likeperpost)] <- mean(df$likeperpost,na.rm=T)
df$postpermonth <- as.numeric(paste(df$postpermonth))
df$postpermonth[is.na(df$postpermonth)] <- mean(df$postpermonth,na.rm=T)
df$MonthsofSuffering[is.na(df$MonthsofSuffering)] <- mean(df$MonthsofSuffering,na.rm=T)
df$TenureMonths[is.na(df$TenureMonths)] <- mean(df$TenureMonths,na.rm=T)
df$HYDiffABS[is.na(df$HYDiffABS)] <- 2
df$CombinedDV[is.na(df$CombinedDV)] <- 2
df$Post.Sentiment[is.na(df$Post.Sentiment)] <- mean(df$Post.Sentiment,na.rm=T)
df$Qquality[is.na(df$Qquality)] <- mean(df$Qquality,na.rm=T)
df$FogScore[is.na(df$FogScore)] <- mean(df$FogScore,na.rm=T)
df$FleschScore[is.na(df$FleschScore)] <- mean(df$FleschScore,na.rm=T)


To change data type to Binary for variables with 0 and 1 values
-------------------------------------------------------------------------------------

final_df$PostAccuracy <- factor(final_df$PostAccuracy)
final_df$Post.Relevance <- factor(final_df$Post.Relevance)
final_df$PostComplexity <- factor(final_df$PostComplexity)
final_df$QuestionRelevance <- factor(final_df$QuestionRelevance)
final_df$QuestionCompleteness <- factor(final_df$QuestionCompleteness)
final_df$QMedicallyComplex <- factor(final_df$QMedicallyComplex)
final_df$QAnswerable <- factor(final_df$QAnswerable)
final_df$QMedical.Context <- factor(final_df$QMedical.Context)
final_df$QAnswerable<- factor(final_df$QAnswerable)
final_df$QMedical.Context<- factor(final_df$QMedical.Context)
final_df$QTreatment.Available<- factor(final_df$QTreatment.Available)

Model for Logistic Regression
----------------------------------------

model <- glm(cbind(Post.Relevance,PostAccuracy) ~ PostComplexity + QuestionRelevance + HY.Scale + UserAge + UserGender + Total.number.of.posts + TotalLikes + HY.Scale + No.of.Secondary.Conditions + Number.of.followers + Degre + likeperpost + postpermonth + MonthsofSuffering + TenureMonths + FleschScore,family=binomial(link='logit'),data=train, maxit = 100)

#Best Model
#model <- glm(CombinedDV ~ QMedicallyComplex + UserAge + UserGender + Total.number.of.posts + TotalLikes  + No.of.Secondary.Conditions + Number.of.followers + likeperpost + postpermonth + MonthsofSuffering + TenureMonths + Post.Sentiment,family=binomial(link='logit'),data=train, maxit = 100)
summary(model)

fitted.results <- predict(model,newdata=subset(test,select=c(3,4,5,6,7,8,9,10,11,12,14,17,18,19,20,23,26,27)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Post.Relevance)
print(paste('Accuracy',1-misClasificError))
anova(model, test="Chisq")
nrow(train$Thread.Number)