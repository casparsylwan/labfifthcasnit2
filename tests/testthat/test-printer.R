context("Testing that it takes in the right data")

expect_equal(names(riks_api()[1,1]),"date")
expect_equal(class(riks_api())[1],"tbl_df")
expect_equal(names(riks_api()[1,2]),"Group")
expect_equal(names(riks_api()[1,3]),"Series")
expect_equal(names(riks_api()[1,4]),"Value")

