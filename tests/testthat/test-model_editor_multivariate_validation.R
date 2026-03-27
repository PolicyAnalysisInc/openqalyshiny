test_that("multivariate distribution validator accepts supported distributions", {
  expect_true(openqalyshiny:::.validate_multivariate_distribution("dirichlet(alpha = c(a, b, c))")$valid)
  expect_true(openqalyshiny:::.validate_multivariate_distribution("mvnormal(mean = c(mu1, mu2), sd = c(sd1, sd2), cor = rho)")$valid)
  expect_true(openqalyshiny:::.validate_multivariate_distribution("mvnormal(mean = c(mu1, mu2), cov = sigma)")$valid)
  expect_true(openqalyshiny:::.validate_multivariate_distribution("multinomial(size = 1, prob = c(p1, p2))")$valid)
})

test_that("multivariate distribution validator rejects unsupported or incomplete calls", {
  expect_false(openqalyshiny:::.validate_multivariate_distribution("")$valid)
  expect_false(openqalyshiny:::.validate_multivariate_distribution("normal(mean = 0, sd = 1)")$valid)
  expect_false(openqalyshiny:::.validate_multivariate_distribution("dirichlet(alpha = c(a), scale = 2)")$valid)
  expect_false(openqalyshiny:::.validate_multivariate_distribution("mvnormal(mean = c(mu1, mu2))")$valid)
  expect_false(openqalyshiny:::.validate_multivariate_distribution("multinomial(size = 1)")$valid)
  expect_false(openqalyshiny:::.validate_multivariate_distribution("mvnormal(mean = c(mu1, mu2), sd = c(sd1, sd2))")$valid)
})
