context("Testing that it takes in the right data")

testobj<-shiny_api$new()

testthat::expect_equal(names(testobj$riks_api()[1,1]),"date")
testthat::expect_equal(class(testobj$riks_api())[1],"tbl_df")
testthat::expect_equal(names(testobj$riks_api()[1,2]),"Group")
testthat::expect_equal(names(testobj$riks_api()[1,3]),"Series")
testthat::expect_equal(names(testobj$riks_api()[1,4]),"Value")

testthat::expect_identical(testobj$riks_api()[1,]$Value,"4.92")
testthat::expect_identical(testobj$riks_api()[1,]$Series,"US 3M")
testthat::expect_identical(testobj$riks_api()[1,]$Group,"Euro market rates, maturity 3 months")
testthat::expect_identical(testobj$riks_api()[2,]$Value,"4.97")

testthat::expect_equal(tail( testobj$riks_api()$date,n=1),Sys.Date())


