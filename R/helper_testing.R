
compare_colnames<-function(x,y){


  missing <- colnames(x) %missing in% colnames(y)
  supoerfluous <- colnames(y) %missing in% colnames(x)
}


compare_ncol<-function(x,y){
  ncol(x)==ncol(y)
}


compare_types<-function(target, current){
  target[,which(sapply(target,function(x){all(is.na(x))}))]<-NULL
  all.equal(target = target[0,colnames(target)[which(colnames(target)%in% colnames(current))]],current = current[0,colnames(current)[which(colnames(current)%in% colnames(target))]])
}




