# setwd("./tests/testthat/")

qu<-read.csv("./kobo questions.csv", stringsAsFactors = F)
ch<-read.csv("./kobo choices.csv", stringsAsFactors = F)

expected<-read.csv("./testdata.csv", stringsAsFactors = F)


produced<-xlsformfill::xlsform_fill(qu,ch,500)

colnames(expected)<-colnames(expected) %>% tolower %>% gsub("/",".",.)
colnames(produced)<-colnames(produced) %>% tolower %>% gsub("/",".",.)




questionnaire<-koboquest::load_questionnaire(expected,qu,ch)



choice_numeric_to_logical<-function(df,questionnaire){

  sm_choice_columns<-sapply(names(df),q$question_is_sm_choice)


  for(i in which(sm_choice_columns)){
    df[,i]<-as.logical(df[,i])
  }

  df
}




expected <- choice_numeric_to_logical(expected,questionnaire)



`%missing in%` <- function(x,y){
  which.missing<-which(!(x %in% y))
  x[which.missing]
}



