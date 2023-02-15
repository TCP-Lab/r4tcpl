
test_that("repmat() works as expected", {
  expect_equal(
    repmat(matrix(c(4,6,5,3,8,9,7,9), nrow = 2, ncol = 4, byrow = TRUE), 3, 2),
    matrix(rep(c(rep(c(4,6,5,3),2), rep(c(8,9,7,9),2)), 3),
           nrow = 6, ncol = 8, byrow = TRUE))
})



### Code to regenerate values for the test below:
# set.seed(...some seed...)
# val <- round(
#   c(rnorm(5, mean = 0, sd = 1),
#     rnorm(4, mean = -3, sd = 2),
#     rnorm(7, mean = 2, sd = 3)), 4)
# dig <- 4
# reference <- data.frame(
#   c(5,4,7),
#   c(round(mean(val[1:5]),dig),
#     round(mean(val[6:9]),dig),
#     round(mean(val[10:16]),dig)),
#   c(round(median(val[1:5]),dig),
#     round(median(val[6:9]),dig),
#     round(median(val[10:16]),dig)),
#   c(round(IQR(val[1:5]),dig),
#     round(IQR(val[6:9]),dig),
#     round(IQR(val[10:16]),dig)),
#   c(round(var(val[1:5]),dig),
#     round(var(val[6:9]),dig),
#     round(var(val[10:16]),dig)),
#   c(round(sd(val[1:5]),dig),
#     round(sd(val[6:9]),dig),
#     round(sd(val[10:16]),dig)),
#   c(round(sd(val[1:5])/sqrt(5),dig),
#     round(sd(val[6:9])/sqrt(4),dig),
#     round(sd(val[10:16])/sqrt(7),dig)))
test_that("descriptives returns correct statistics", {
  expect_equal(
    sum(descriptives(
      vals = c(1.2723, -0.7170, 0.1805, -0.3723, -0.2152, -8.0293, -1.2874,
               -3.6977, -2.1974, 3.0231, 4.7699, 4.8882, -0.7503, 3.9198,
               5.8581, 3.1373),
      design = c(rep("ctrl",5),
                 rep("drug_1",4),
                 rep("drug_2",7)),
      prec = 4) !=
          data.frame(c("ctrl", "drug_1", "drug_2"),
                     c(5, 4, 7),
                     c(0.0297, -3.8030, 3.5494),
                     c(-0.2152, -2.9476, 3.9198),
                     c(0.5528, 2.8107, 1.7489),
                     c(0.5865, 8.9263, 4.6139),
                     c(0.7658, 2.9877, 2.1480),
                     c(0.3425, 1.4938, 0.8119))),
    0)
  })
