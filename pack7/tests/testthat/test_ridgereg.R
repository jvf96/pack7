context("ridgereg")

data("iris")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)

test_that("lenreg rejects errounous input", {
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda=0))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})


test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0)
  
  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})


test_that("print() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2.6)
  
  expect_output(ridgereg_mod$print(),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris, lambda = 2\\.6\\)")
})

test_that("predict() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 0.5)
  
  expect_equal(round(unname(ridgereg_mod$predict()[c(1,20,79)]),2), c(1.84, 1.44, 4.23))    
})

test_that("coef() method works", {
  a <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2.6)
  lm.r <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2.6)
  
  expect_equal(round(a$coef()[-1],1), round(lm.r$coef,1))
})
