# The posterior probability that a two-component robust mixture prior came from
# each of its components, under the two-arm binomial model the Stan program
# fits. The component the treatment effect is drawn from is truncated to the
# range the control rate leaves it, so the two integrals do not separate and
# there is no closed form; these tests pin the quadrature against nested
# adaptive integration and against the invariants the quantity has to satisfy.

binomial_weights_fixture <- function() {
  list(
    weights = c(0.5, 0.5),
    means = c(0.1315456, 0),
    sds = c(0.03505291, 0.6),
    n_control = 52L,
    n_successes_control = 42L,
    n_treatment = 55L,
    n_successes_treatment = 48L
  )
}

# Nested adaptive integration of the same quantity. Substituting the treatment
# rate for the treatment effect puts the inner integral on (0, 1) whatever the
# control rate is, so only the normaliser depends on it.
reference_marginal_likelihood <- function(mean, sd, n_control,
                                          n_successes_control, n_treatment,
                                          n_successes_treatment) {
  outer_integrand <- function(control_rate) {
    vapply(control_rate, function(rate) {
      normaliser <- stats::pnorm(1 - rate, mean, sd) -
        stats::pnorm(-rate, mean, sd)
      inner <- stats::integrate(
        function(treatment_rate) {
          stats::dbinom(n_successes_treatment, n_treatment, treatment_rate) *
            stats::dnorm(treatment_rate - rate, mean, sd)
        },
        lower = 0,
        upper = 1,
        rel.tol = 1e-10,
        subdivisions = 1000L
      )$value
      stats::dbinom(n_successes_control, n_control, rate) * inner / normaliser
    }, numeric(1))
  }

  stats::integrate(
    outer_integrand,
    lower = 0,
    upper = 1,
    rel.tol = 1e-10,
    subdivisions = 1000L
  )$value
}

reference_weights <- function(fixture) {
  marginals <- vapply(seq_along(fixture$weights), function(k) {
    reference_marginal_likelihood(
      mean = fixture$means[k],
      sd = fixture$sds[k],
      n_control = fixture$n_control,
      n_successes_control = fixture$n_successes_control,
      n_treatment = fixture$n_treatment,
      n_successes_treatment = fixture$n_successes_treatment
    )
  }, numeric(1))

  unnormalised <- fixture$weights * marginals
  unnormalised / sum(unnormalised)
}

call_binomial_weights <- function(fixture) {
  do.call(truncated_normal_mixture_binomial_weights, fixture)
}


test_that("the component weights agree with nested adaptive integration", {
  fixture <- binomial_weights_fixture()

  expect_equal(
    call_binomial_weights(fixture),
    reference_weights(fixture),
    tolerance = 1e-4
  )
})


test_that("the component weights sum to one", {
  expect_equal(sum(call_binomial_weights(binomial_weights_fixture())), 1)
})


test_that("indistinguishable components leave the prior weights untouched", {
  fixture <- binomial_weights_fixture()
  fixture$weights <- c(0.9, 0.1)
  fixture$means <- c(0.1, 0.1)
  fixture$sds <- c(0.2, 0.2)

  # No observation can tell the two components apart, so the data cancel out of
  # the ratio whatever they are.
  expect_equal(call_binomial_weights(fixture), c(0.9, 0.1), tolerance = 1e-8)
})


test_that("an observed effect the informative component predicts raises its weight", {
  fixture <- binomial_weights_fixture()
  # 44/55 against 33/52 is a difference of 0.165, within a standard error of
  # the informative mean of 0.132; the vague component spreads its mass over
  # the whole range instead.
  fixture$n_successes_treatment <- 44L
  fixture$n_successes_control <- 33L

  expect_gt(call_binomial_weights(fixture)[1], 0.5)
})


test_that("an observed effect the informative component rules out lowers its weight", {
  fixture <- binomial_weights_fixture()
  # 20/55 against 40/52 is a difference of -0.406, more than fifteen standard
  # errors below the informative mean.
  fixture$n_successes_treatment <- 20L
  fixture$n_successes_control <- 40L

  expect_lt(call_binomial_weights(fixture)[1], 0.01)
})


test_that("an empty treatment arm leaves the prior weights untouched", {
  fixture <- binomial_weights_fixture()
  fixture$weights <- c(0.3, 0.7)
  fixture$n_treatment <- 0L
  fixture$n_successes_treatment <- 0L

  # With no treatment arm the treatment effect is unobserved, and the control
  # arm is common to both components, so neither component is favoured.
  expect_equal(call_binomial_weights(fixture), c(0.3, 0.7), tolerance = 1e-6)
})
