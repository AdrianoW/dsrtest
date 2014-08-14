library("testthat")

test_that("Fibonacci", {

   expect_error(F(mean))
   expect_is(F(1), "numeric")
   expect_equivalent(F(1), 1)
   expect_equivalent(F(1:6), c(1,2,3,5,8,13))
   expect_equivalent(F(c(1,NA,3)), c(1,NA,3))
   expect_equivalent(F(10), F(9)+F(8))
   expect_true(all(F(1:100)>0))
   expect_equivalent(F((1:10)+0.99), F(1:10))
   expect_error(F(-100))

})


