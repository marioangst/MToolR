test_that("A minimal mtoolr object can be created from scratch", {
  df <-
    data.frame(User_ID = c("User1"),
               To = c("Concept1"),
               From = c("Concept2"),
               Weight = c(2))
  min_test <- mentalmodel(df)
  expect_true(is_mtoolr(min_test))
})
