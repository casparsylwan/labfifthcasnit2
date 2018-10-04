context("Testing that it takes in the right data")

expect_equal(names(riks_api()[1,1]),"date")
expect_equal(class(riks_api())[1],"tbl_df")
expect_equal(names(riks_api()[1,2]),"Group")
expect_equal(names(riks_api()[1,3]),"Series")
expect_equal(names(riks_api()[1,4]),"Value")

expect_identical(riks_api(rate1="a")[1,]$Value,"4.92")
expect_identical(riks_api(rate1="a")[1,]$Series,"US 3M")
expect_identical(riks_api(rate1="a")[1,]$Group,"Euro market rates, maturity 3 months")
expect_identical(riks_api(rate2="a")[1,]$Value,"0.16")

expect_equal(tail(riks_api(rate1="a")$date, n=1),Sys.Date())


