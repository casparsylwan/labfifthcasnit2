context("Testing that it takes in the right data")

testobj<-shiny_api$new()

expect_equal(names(testobj$riks_api("a")[1,1]),"date")
expect_equal(class(testobj$riks_api("a"))[1],"tbl_df")
expect_equal(names(testobj$riks_api("a")[1,2]),"Group")
expect_equal(names(testobj$riks_api("a")[1,3]),"Series")
expect_equal(names(testobj$riks_api("a")[1,4]),"Value")

expect_identical(testobj$riks_api(us="a")[1,]$Value,"4.92")
expect_identical(testobj$riks_api(us="a")[1,]$Series,"US 3M")
expect_identical(testobj$riks_api(us="a")[1,]$Group,"Euro market rates, maturity 3 months")
expect_identical(testobj$riks_api()[2,]$Value,"4.97")

expect_equal(tail( testobj$riks_api(us="a")$date,n=1),Sys.Date())


