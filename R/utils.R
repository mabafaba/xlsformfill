hasdata<-function (x, return.index = F) {
  index <- which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))
  value <- x[which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))]
  if (return.index) {
    return(index)
  }
  return(value)
}


to_alphanumeric_lowercase <- function(x){tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))}
to_alphanumeric_lowercase_colnames_df <- function(df){
  names(df) <- to_alphanumeric_lowercase(names(df))
  return(df)
}


generate_uuid <- function(...) {
  uppercase<-FALSE
  hex_digits <- c(as.character(0:9), letters[1:6])
  hex_digits <- if (uppercase) toupper(hex_digits) else hex_digits

  y_digits <- hex_digits[9:12]

  paste(
    paste0(sample(hex_digits, 8), collapse=''),
    paste0(sample(hex_digits, 4), collapse=''),
    paste0('4', sample(hex_digits, 3), collapse=''),
    paste0(sample(y_digits,1),
           sample(hex_digits, 3),
           collapse=''),
    paste0(sample(hex_digits, 12), collapse=''),
    sep='-')
}
