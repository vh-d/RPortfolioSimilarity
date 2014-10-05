context("Similarity Functions")

test_that("two vectors similarity as expected", {
  t1 <- cbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))

  # basic properties of cosine similarity
  expect_true(is.na(vCosSimilarity(NA, NA)))
  expect_true(is.nan(vCosSimilarity(0, 0)))

  expect_equal(vCosSimilarity(c(1, 5, 2), c(3 , 1, 6)), wtVCosSimilarity(c(1, 5, 2), c(3 , 1, 6), c(1, 1, 1)))
  
  expect_equal(vCosSimilarity(c(1, 0), c(0 , 1)), 0)
  expect_equal(vCosSimilarity(c(-1, 0), c(1 , 0)), -1)

  # cosine similarity is scale-independent
  rnum1 <- runif(100)
  rnum2 <- runif(100)
  expect_equal(vCosSimilarity(rnum1, rnum2), vCosSimilarity((runif(1) + 1) * rnum1, (runif(1) + 1) * rnum2)) 
  
})