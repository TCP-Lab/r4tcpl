test_that("show_data returns actual dimensions", {
  expect_equal(
    show_data(data.frame(
    var1 = rep(1, 10),
    var2 = rep(1, 10),
    var3 = rep(1, 10)
    )),
    c(10, 3)
  )
})
