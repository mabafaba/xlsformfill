#' create sampling frame from kobo tool
#' @param choices the kobo questionnaire choices sheet as a data.frame
#' @param strata_list_names the choice list_name column values of the variables that together uniquely define each stratum
#' @return a data frame with two columns:
#' "strata": the names of the strata; all unique combinations of the choices in each provided strata list name, concatenated by two underscores ("__")
#' "population": randomly generated population for each stratum (lognormal distribution with meanlog 10 and sdlog 1)
#' @export
xlsform_generate_samplingframe<-function(choices, strata_list_names){
  assertthat::assert_that(is.data.frame(choices))
  assertthat::assert_that(nrow(choices)>0)
  assertthat::assert_that(is.character(strata_list_names))
  choices$list_name<-as.character(choices$list_name)
  choices$name<-as.character(choices$name)
  strata_choices<-lapply(strata_list_names,function(list_name){
    choices_strata <- choices[choices$list_name == list_name,"name"] %>% unlist %>%  unique
  })

  strata_names <- expand.grid(strata_choices) %>% apply(1,paste,collapse = "__") %>% unique

  strata_pops <- rlnorm(length(strata_names),meanlog = 10,sdlog = 1) %>% round
  strata_pops[strata_pops<=0]<-rlnorm(length(strata_pops<0),meanlog = 10,sdlog = 1) %>% round
  data.frame(strata = strata_names, population = strata_pops,stringsAsFactors = FALSE)

}






