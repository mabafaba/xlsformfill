

#' generate data for an xlsform
#' @param questions the xlsform questions sheet as data frame
#' @param choices the xlsform choices sheet as a data frame
#' @param n the number of rows to generate
#' @details
#' @export
xlsform_fill <- function(questions,choices,n) {
  questions<-questions[!is.na(questions$name),]
  questions$name<-as.character(questions$name)
  questions$relevant<-as.character(questions$relevant)

  q<-get_questionnaire_functions(questions,choices)
  get_choices<-choices_of_question_funfact(questions,choices)
  get_raw_type<-function(varname){questions$type[match(varname,questions$name)][1]}
  filled<-lapply(questions$name,function(varname){

    filling<-tibble::tibble(1:n)[,0]

    # if(varname=="start"){browser()}
    if(get_raw_type(varname)=="start"){filling<-fill_datetime(varname,n)}
    if(get_raw_type(varname)=="end"){filling<-fill_datetime(varname,n)}
    if(get_raw_type(varname)=="deviceid"){filling<-fill_deviceid(varname,n)}

    if(get_raw_type(varname)=="date"){filling<-fill_datetime(varname,n)}
    if(get_raw_type(varname)=="time"){filling<-fill_datetime(varname,n)}
    if(tolower(get_raw_type(varname))=="datetime"){filling<-fill_datetime(varname,n)}

    if(get_raw_type(varname)=="text"){filling<-fill_text(varname,n)}
    if(get_raw_type(varname)=="decimal"){filling<-fill_decimal(varname,n)}
    if(get_raw_type(varname)=="integer"){filling<-fill_integer(varname,n)}

    if(get_raw_type(varname)=="calculate"){filling<-fill_calculate_placeholder(varname,n)}

    if(q$question_is_select_one(varname)){filling<-fill_select_one(varname,n,get_choices(varname))}
    if(q$question_is_select_multiple(varname)){filling<-fill_select_multiple(varname,n,get_choices(varname))}

    filling
  })  %>% do.call(cbind,.)


  filled[["uuid"]]<-fill_uuid("uuid",n)[["uuid"]]


  # add calculations



  filled<-remove_skipped_values(filled,q)
  filled<-add_form_calculations(filled,questions)
  filled
}


add_form_calculations<-function(data,questions){
  get_raw_type<-function(varname){questions$type[match(varname,questions$name)][1]}
  calculate_varnames<-questions$name[which(sapply(questions$name,get_raw_type)=="calculate")]
  get_calculation<-function(varname){questions$calculation[match(varname,questions$name)][1]}

  for(varname in calculate_varnames){
    data[[varname]]<-fill_calculate(varname,
                                      kobo_calculation = get_calculation(varname),
                                      other_data = data)[[varname]]

  }
  data
}




get_questionnaire_functions<-function(questions,choices){
  empty_data<-as.data.frame(lapply(questions$name,function(x){NA}))
  names(empty_data)<-questions$name
  empty_data<-empty_data[,!is.na(names(empty_data))]
  q<-koboquest::load_questionnaire(empty_data,questions,choices)
  q
}





choices_of_question_funfact<-function (questions, choices){


  names(questions) <- to_alphanumeric_lowercase(names(questions))
  names(choices) <- to_alphanumeric_lowercase(names(choices))

  data_colnames <- to_alphanumeric_lowercase(questions$name)

  # questions <- questions[match(data_colnames, questions$name),
  #                        ]
  # if (length(grep("^list[\\._]name$", "list_name", value = T)) <
  #     1) {
  #   stop("kobo 'choices' sheet must have a column named 'list.name' or 'list_name'")
  # }
  choices_per_data_column <- questions$type %>% as.character %>%
    strsplit(" ") %>% lapply(unlist) %>% lapply(function(x) {
      x %>% lapply(function(y) {
        grep(paste0(" ", y, " "), paste0(" ", choices[[grep("^list[\\._]name$",
                                                            names(choices), value = T)[1]]], " "), value = F,
             fixed = T)
      }) %>% unlist
    }) %>% lapply(hasdata) %>% lapply(function(x) {
      choices[x, ]
    })


  names(choices_per_data_column) <- data_colnames

  get_choices<-function(varname){
    varname<-to_alphanumeric_lowercase(varname)
    choices_per_data_column[[varname]]$name

  }
  return(get_choices)

}



