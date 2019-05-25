testthat::test_that("overal filled structure equal real data",{

  fake<-xlsformfill::xlsform_fill(questions = qu,
                                  choices = ch,
                                  n = 40)





})




testthat::test_that("is it perfect?",{
  missmatches<-compare_types(expected,produced)
  missmatches<-missmatches[grepl("Modes: ",missmatches)]
  missmatches_note<-paste0(missmatches,collapse="\n")
  testthat::expect_equal(length(missmatches),0,label = "it is not perfect.",info = missmatches_note)
  })

