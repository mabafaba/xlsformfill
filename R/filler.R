

fill_decimal<-function(varname,n){
  filling<- tibble::tibble(abs((rnorm(n)*2)+20))
  colnames(filling)<-varname
  filling
}
fill_integer<-function(varname,n){
  filling<- tibble::tibble(round(abs((rnorm(n)*2)+20)))
  colnames(filling)<-varname
  filling
}

fill_text<-function(varname,n){
  filling<- tibble::tibble(stringi::stri_rand_strings(n,length = round(runif(n,max=100)),pattern = "[A-Za-z\\ ]"))
  colnames(filling)<-varname
  filling
}


fill_deviceid<-function(varname,n){
  n_devices<-round(n/50)
  if(n_devices<10){n_devices<-10}
  deviceids<-sprintf("deviceID%08d", 0:n_devices)
  filling<- tibble::tibble(sample(deviceids,n,T))
  colnames(filling)<-varname
  filling
}



fill_select_one<-function(varname,n,options){
  if(length(options)==0){
    options<-NA
    warning(paste0("no choices found for variable: '",varname,"' - filling this select_one with NA"))
    filling<- tibble::tibble(rep(NA,n))
    colnames(filling)<-varname
    return(filling)
  }

  filling<- tibble::tibble(sample(options,n,T))
  colnames(filling)<-varname
  filling
}


fill_select_multiple<-function(varname,n,options){
  if(length(options)==0){
    warning(paste0("no choices found for variable: '",varname,"' - ignoring this select_multiple"))
    return(tibble::tibble(1:n)[,0])
  }

  filling_logical<-matrix(sample(c(T,F),n*length(options),T),nrow = n)
  colnames(filling_logical)<-paste0(varname,"/",options)

  filling_concatenated_choices<- tibble::tibble(apply(filling_logical,1,function(x){paste0(options[which(x)],collapse=" ")}))
  colnames(filling_concatenated_choices)<-varname

  tibble::as_tibble(cbind(filling_concatenated_choices,filling_logical,stringsAsFactors=F))

}

fill_datetime<-function(varname,n){
  date_0<-Sys.time()
  filling<- tibble::tibble(date_0 - runif(n,min=0,max=10000))
  colnames(filling)<-varname
  filling
}



fill_uuid<-function(varname,n){
  filling<- tibble::tibble(sapply(1:n,generate_uuid))
  colnames(filling)<-varname
  filling
}



fill_calculate_placeholder<-function(varname,n){
  filling<- tibble::tibble(rep(NA,n))
  colnames(filling)<-varname
  filling
}



fill_calculate<-function(varname,kobo_calculation,other_data){

  calculation_as_rcode<-koboquest:::rify_condition(kobo_calculation)
  if(calculation_as_rcode==""){calculation_as_rcode<-"rep(NA,nrow(other_data))"}
  calc_result<-with(other_data,{
    tryCatch({
      calc_result<-eval(parse(text=calculation_as_rcode))
      if(length(calc_result)!=nrow(other_data)){
        stop(paste0("filling '",varname,"' with NA. Calculation did not produce one value per data row."))
      }
      calc_result
    },error=function(e){
      warning(paste0("filling '",varname,"' with NA. Calculation failed with error:",e$message))
      rep(NA,nrow(other_data))
    })
  })
  calc_result<-unlist(calc_result)
  filling<- tibble::tibble(calc_result)
  colnames(filling)<-varname
  return(filling)
}


