skipped_values<-function(data,questionnaire){

  lapply(names(data),function(x){

    tryCatch({questionnaire$question_is_skipped(data,x)},error=function(e){
      warning(paste0("failed applying skiplogic for '",x,"' with error: ",e$message))
      rep(FALSE,nrow(data))})
  }) %>% as.data.frame(stringsAsFactors=F)
}



remove_skipped_values<-function(data,questionnaire){
  skipped<-skipped_values(data,questionnaire)
  for(i in ncol(data)){
    data[skipped[,i],i]<-NA
  }
  data
}
