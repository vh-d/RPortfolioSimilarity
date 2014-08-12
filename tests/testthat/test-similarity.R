context("Similarity Functions")

test_that("two vectors similarity as expected", {
  t1 <- cbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))

  # basic properties of cosine similarity
  expect_true(is.na(cosSimilarity(NA, NA)))
  expect_true(is.nan(cosSimilarity(0, 0)))
  
  expect_equal(cosSimilarity(c(1, 0), c(0 , 1)), 0)
  expect_equal(cosSimilarity(c(-1, 0), c(1 , 0)), -1)

  # cosine similarity is scale-independent
  rnum1 <- runif(100)
  rnum2 <- runif(100)
  expect_equal(cosSimilarity(rnum1, rnum2), cosSimilarity((runif(1) + 1) * rnum1, (runif(1) + 1) * rnum2)) 
  
})