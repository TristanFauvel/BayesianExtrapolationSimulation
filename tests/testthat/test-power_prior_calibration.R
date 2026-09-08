# The adaptive power prior of Nikolakopoulos et al. (2018) picks its calibration
# parameter by searching for the value whose type I error hits a target. The
# type I error is an expectation of an indicator over a single normal variate,
# so it has a closed form: the rejection region is a union of intervals and the
# probability is a sum of normal CDF differences. The original code estimated it
# by Monte Carlo instead. These tests pin the exact computation against that
# Monte Carlo estimator.

# The Monte Carlo estimator, written out independently of the implementation
# under test and kept in the original algebra (including the qnorm/pnorm round
# trips) so that it also validates the identities used to remove them.
monte_carlo_type_I_error <- function(calibration_parameter,
                                     source_sample_size_per_arm,
                                     target_sample_size_per_arm,
                                     source_treatment_effect_estimate,
                                     significance_level,
                                     target_data_sampling_variance,
                                     source_data_sampling_variance,
                                     theta_0,
                                     n_draws) {
  n <- target_sample_size_per_arm
  tau2 <- target_data_sampling_variance
  n0 <- source_sample_size_per_arm * tau2 / source_data_sampling_variance
  prior_variance <- tau2 / n0
  std_predictive_dist <- sqrt(tau2 / n0 + tau2 / n)

  x <- rnorm(n_draws, theta_0, sqrt(tau2 / n))
  b <- 2 * pnorm(-calibration_parameter)
  power_parameter <- ifelse(
    x > source_treatment_effect_estimate + std_predictive_dist * qnorm(1 - b / 2) |
      x < source_treatment_effect_estimate + std_predictive_dist * qnorm(b / 2),
    prior_variance /
      (((x - source_treatment_effect_estimate) / qnorm(1 - b / 2))^2 - tau2 / n),
    1
  )
  mean(((power_parameter * n0 * source_treatment_effect_estimate + x * n) /
          (power_parameter * n0 + n) +
          sqrt(tau2 / (power_parameter * n0 + n)) * qnorm(significance_level)) > 0)
}

# A spread of designs: the source/target variance ratio drives how much the
# borrowing weight can move, so it is the axis worth covering.
calibration_designs <- function() {
  list(
    list(label = "mepolizumab-like, small target variance",
         ess = 243.7677, n = 52, theta_s = 0.6931472, tau2 = 2.5, sigma0_2 = 4.02909),
    list(label = "mepolizumab-like, median target variance",
         ess = 243.7677, n = 52, theta_s = 0.6931472, tau2 = 4.2044, sigma0_2 = 4.02909),
    list(label = "mepolizumab-like, large target variance",
         ess = 243.7677, n = 52, theta_s = 0.6931472, tau2 = 7.0, sigma0_2 = 4.02909),
    list(label = "small source, null source effect",
         ess = 30, n = 100, theta_s = 0.0, tau2 = 1.0, sigma0_2 = 1.0),
    list(label = "negative source effect",
         ess = 120, n = 40, theta_s = -0.35, tau2 = 2.0, sigma0_2 = 1.4)
  )
}

test_that("the exact type I error matches a large Monte Carlo estimate", {
  n_draws <- 4e6
  set.seed(20240607)

  for (design in calibration_designs()) {
    for (calibration_parameter in c(0.05, 0.2, 0.5, 0.9, 1)) {
      exact <- adaptive_power_prior_type_I_error(
        calibration_parameter = calibration_parameter,
        source_sample_size_per_arm = design$ess,
        target_sample_size_per_arm = design$n,
        source_treatment_effect_estimate = design$theta_s,
        significance_level = 0.05,
        target_data_sampling_variance = design$tau2,
        source_data_sampling_variance = design$sigma0_2,
        theta_0 = 0
      )
      estimate <- monte_carlo_type_I_error(
        calibration_parameter = calibration_parameter,
        source_sample_size_per_arm = design$ess,
        target_sample_size_per_arm = design$n,
        source_treatment_effect_estimate = design$theta_s,
        significance_level = 0.05,
        target_data_sampling_variance = design$tau2,
        source_data_sampling_variance = design$sigma0_2,
        theta_0 = 0,
        n_draws = n_draws
      )
      # Agreement to within four Monte Carlo standard errors.
      standard_error <- sqrt(max(exact, 1e-6) * (1 - exact) / n_draws)
      expect_lt(abs(estimate - exact), 4 * standard_error,
                label = sprintf("|MC - exact| for %s at Z = %g (exact %.6f, MC %.6f)",
                                design$label, calibration_parameter, exact, estimate))
    }
  }
})

test_that("the exact type I error is a probability and increases with the calibration parameter", {
  design <- calibration_designs()[[2]]
  grid <- seq(0.01, 1, length.out = 25)
  values <- vapply(grid, function(z) {
    adaptive_power_prior_type_I_error(
      calibration_parameter = z,
      source_sample_size_per_arm = design$ess,
      target_sample_size_per_arm = design$n,
      source_treatment_effect_estimate = design$theta_s,
      significance_level = 0.05,
      target_data_sampling_variance = design$tau2,
      source_data_sampling_variance = design$sigma0_2,
      theta_0 = 0
    )
  }, numeric(1))

  expect_true(all(values >= 0 & values <= 1))
  expect_true(all(diff(values) > -1e-12))
})

test_that("findCalibrationParameter returns the value that attains the desired type I error", {
  design <- calibration_designs()[[2]]
  desired_tie <- 0.065

  calibration <- findCalibrationParameter(
    source_sample_size_per_arm = design$ess,
    target_sample_size_per_arm = design$n,
    source_treatment_effect_estimate = design$theta_s,
    desired_tie = desired_tie,
    significance_level = 0.05,
    target_data_sampling_variance = design$tau2,
    source_data_sampling_variance = design$sigma0_2,
    tolerance = 1e-4,
    theta_0 = 0
  )
  calibration_parameter <- as.numeric(calibration[1, 2])

  attained <- adaptive_power_prior_type_I_error(
    calibration_parameter = calibration_parameter,
    source_sample_size_per_arm = design$ess,
    target_sample_size_per_arm = design$n,
    source_treatment_effect_estimate = design$theta_s,
    significance_level = 0.05,
    target_data_sampling_variance = design$tau2,
    source_data_sampling_variance = design$sigma0_2,
    theta_0 = 0
  )
  expect_equal(attained, desired_tie, tolerance = 1e-3)
})

test_that("findCalibrationParameter falls back to a cut-off of 1 below the reference's precondition", {
  # Nikolakopoulos et al (2018) require the source estimate to exceed
  # min_prior_mean, equivalently maxZ_1_m_c2 > 0, and their script fails below
  # it. A source estimate this far below theta_0 breaks that precondition, so
  # there is no calibrated answer to give and we fall back to the cut-off
  # Gaussian_Gravestock_EBPP fixes. See test-pdccpp_admissible_cutoff.R.
  calibration <- findCalibrationParameter(
    source_sample_size_per_arm = 200,
    target_sample_size_per_arm = 50,
    source_treatment_effect_estimate = -3,
    desired_tie = 0.065,
    significance_level = 0.05,
    target_data_sampling_variance = 1,
    source_data_sampling_variance = 1,
    tolerance = 1e-4,
    theta_0 = 0
  )
  calibration_parameter <- as.numeric(calibration[1, 2])
  maximum <- as.numeric(calibration[3, 2])

  expect_equal(calibration_parameter, 1)

  # The value it used to return instead. Inside the reference's domain this is
  # the cut-off at which the type I error reaches that of full borrowing, and
  # returning it means "borrow fully". A source estimate this far below theta_0
  # puts it far below zero, where it means the opposite: discount everything.
  expect_lt(maximum, 0)
})

test_that("findCalibrationParameter is deterministic and does not consume the random stream", {
  arguments <- list(
    source_sample_size_per_arm = 243.7677,
    target_sample_size_per_arm = 52,
    source_treatment_effect_estimate = 0.6931472,
    desired_tie = 0.065,
    significance_level = 0.05,
    target_data_sampling_variance = 4.2044,
    source_data_sampling_variance = 4.02909,
    tolerance = 1e-4,
    theta_0 = 0
  )

  set.seed(1)
  first <- do.call(findCalibrationParameter, arguments)
  stream_after_first <- runif(1)

  set.seed(1)
  second <- do.call(findCalibrationParameter, arguments)
  stream_after_second <- runif(1)

  expect_identical(first, second)

  set.seed(1)
  expect_identical(runif(1), stream_after_first)
  expect_identical(stream_after_first, stream_after_second)
})

test_that("findCalibrationParameter keeps the labelled matrix its callers index into", {
  calibration <- findCalibrationParameter(
    source_sample_size_per_arm = 243.7677,
    target_sample_size_per_arm = 52,
    source_treatment_effect_estimate = 0.6931472,
    desired_tie = 0.065,
    significance_level = 0.05,
    target_data_sampling_variance = 4.2044,
    source_data_sampling_variance = 4.02909,
    tolerance = 1e-4,
    theta_0 = 0
  )

  expect_equal(dim(calibration), c(5L, 2L))
  expect_equal(calibration[, 1],
               c("Z_1-c/2", "sigma pred", "maximum Z_1-c/2",
                 "min_prior_mean", "type I er with A =1"))
  expect_false(anyNA(as.numeric(calibration[, 2])))
})

# A dense scan of the decision rule. Too coarse to pin down interval endpoints,
# but it cannot step over a whole component of the rejection region, which is
# the failure mode worth guarding against: the region has narrow slivers pressed
# up against each cut-off, where the borrowing weight jumps.
scanned_type_I_error <- function(calibration_parameter,
                                 source_sample_size_per_arm,
                                 target_sample_size_per_arm,
                                 source_treatment_effect_estimate,
                                 significance_level,
                                 target_data_sampling_variance,
                                 source_data_sampling_variance,
                                 theta_0,
                                 n_points = 400001L) {
  n <- target_sample_size_per_arm
  tau2 <- target_data_sampling_variance
  n0 <- source_sample_size_per_arm * tau2 / source_data_sampling_variance
  prior_variance <- tau2 / n0
  cutoff <- sqrt(tau2 / n0 + tau2 / n) * calibration_parameter

  probabilities <- seq(1e-12, 1 - 1e-12, length.out = n_points)
  x <- qnorm(probabilities, theta_0, sqrt(tau2 / n))
  deviation <- x - source_treatment_effect_estimate
  weight <- ifelse(abs(deviation) > cutoff,
                   prior_variance /
                     ((deviation / calibration_parameter)^2 - tau2 / n),
                   1)
  effective <- weight * n0
  rejects <- (effective * source_treatment_effect_estimate + x * n +
                qnorm(significance_level) * sqrt(tau2 * (effective + n))) > 0

  # Trapezoidal measure of the rejecting set on the probability scale.
  sum(diff(probabilities) * (rejects[-n_points] + rejects[-1]) / 2)
}

test_that("no component of the rejection region is missed", {
  # The first nine rows are the designs a 512-point bracketing scan got wrong,
  # by up to 1.2e-3 -- larger than the tolerance the calibration search uses.
  designs <- rbind(
    data.frame(ess = 60, n = 20, theta_s = 0.35, tau2 = 9, sigma0_2 = 1.4, z = 0.02),
    data.frame(ess = 60, n = 20, theta_s = 0.35, tau2 = 9, sigma0_2 = 1.4, z = 0.10),
    data.frame(ess = 243.7677, n = 20, theta_s = 0.35, tau2 = 25, sigma0_2 = 1.4, z = 0.35),
    data.frame(ess = 10, n = 20, theta_s = 0.35, tau2 = 9, sigma0_2 = 4.02909, z = 0.02),
    data.frame(ess = 10, n = 20, theta_s = 0.35, tau2 = 25, sigma0_2 = 4.02909, z = 0.02),
    data.frame(ess = 60, n = 52, theta_s = 0.35, tau2 = 25, sigma0_2 = 4.02909, z = 0.02),
    data.frame(ess = 243.7677, n = 52, theta_s = 0.35, tau2 = 25, sigma0_2 = 4.02909, z = 0.35),
    data.frame(ess = 10, n = 52, theta_s = 0.35, tau2 = 9, sigma0_2 = 12, z = 0.02),
    data.frame(ess = 10, n = 20, theta_s = 0.6931472, tau2 = 25, sigma0_2 = 12, z = 0.70),
    data.frame(ess = 243.7677, n = 52, theta_s = 0.6931472, tau2 = 4.2044,
               sigma0_2 = 4.02909, z = 0.5),
    data.frame(ess = 30, n = 100, theta_s = 0, tau2 = 1, sigma0_2 = 1, z = 0.25),
    data.frame(ess = 120, n = 40, theta_s = -0.35, tau2 = 2, sigma0_2 = 1.4, z = 0.8)
  )

  for (i in seq_len(nrow(designs))) {
    design <- designs[i, ]
    exact <- adaptive_power_prior_type_I_error(
      calibration_parameter = design$z,
      source_sample_size_per_arm = design$ess,
      target_sample_size_per_arm = design$n,
      source_treatment_effect_estimate = design$theta_s,
      significance_level = 0.05,
      target_data_sampling_variance = design$tau2,
      source_data_sampling_variance = design$sigma0_2,
      theta_0 = 0
    )
    scanned <- scanned_type_I_error(
      calibration_parameter = design$z,
      source_sample_size_per_arm = design$ess,
      target_sample_size_per_arm = design$n,
      source_treatment_effect_estimate = design$theta_s,
      significance_level = 0.05,
      target_data_sampling_variance = design$tau2,
      source_data_sampling_variance = design$sigma0_2,
      theta_0 = 0
    )
    expect_lt(abs(exact - scanned), 1e-5,
              label = sprintf("|exact - scanned| for design %d (exact %.8f, scanned %.8f)",
                              i, exact, scanned))
  }
})

test_that("no borrowing recovers the nominal significance level", {
  # As the calibration parameter goes to zero the cut-offs close up and the
  # borrowing weight collapses, so the rule becomes the ordinary one-sided test
  # and its type I error is the significance level exactly.
  for (significance_level in c(0.01, 0.025, 0.05, 0.1)) {
    attained <- adaptive_power_prior_type_I_error(
      calibration_parameter = 1e-8,
      source_sample_size_per_arm = 243.7677,
      target_sample_size_per_arm = 52,
      source_treatment_effect_estimate = 0.6931472,
      significance_level = significance_level,
      target_data_sampling_variance = 4.2044,
      source_data_sampling_variance = 4.02909,
      theta_0 = 0
    )
    expect_equal(attained, significance_level, tolerance = 1e-9)
  }
})

test_that("a non-positive calibration parameter is rejected rather than silently returning NaN", {
  # The original code reached qnorm of a negative number here and failed with
  # "t1 is NA"; this makes the cause explicit. It is still a failure: designs
  # whose cap is negative are not supported by the method.
  expect_error(
    adaptive_power_prior_type_I_error(
      calibration_parameter = -0.01,
      source_sample_size_per_arm = 100,
      target_sample_size_per_arm = 50,
      source_treatment_effect_estimate = 0.3,
      significance_level = 0.05,
      target_data_sampling_variance = 1,
      source_data_sampling_variance = 1,
      theta_0 = 0
    ),
    "must be positive"
  )
})

test_that("the search reproduces the reference implementation from the paper", {
  # Verbatim from the script accompanying Nikolakopoulos, van der Tweel and
  # Roes (2018), "Dynamic borrowing through empirical power priors that control
  # type I error", Biometrics 74(3):874-880. Kept as the fidelity check on the
  # port: it is the estimator the exact computation replaces.
  reference_find_S <- function(R = 1e6, n0 = 5, n1 = 50, m0 = .4, aD = 0.065,
                               sig = 0.05, sR = 1, tol = 0.0001, seed = 1010) {
    set.seed(seed)
    s0 <- sR / n0
    mR <- 0
    sP <- sqrt(sR / n0 + sR / n1)
    As <- -(((-(sqrt(sR) * sqrt(n0 + n1) * qnorm(sig) + n0 * m0)) / (n1 * sP)) - m0 / sP)
    X <- rnorm(R, mR, sqrt(sR / n1))
    uL <- min(As, 1); lL <- 0; S <- min(As, 1)
    t1 <- pnorm((sqrt(sR) * sqrt(n0 + n1) * qnorm(sig) + n0 * m0) / sqrt(sR * n1))
    t2 <- 0
    borrowing <- function(S) {
      B <- 2 * pnorm(-S)
      A1 <- ifelse((X > m0 + sP * qnorm(1 - B / 2)) | (X < m0 + sP * qnorm(B / 2)),
                   s0 / (((X - m0) / qnorm(1 - B / 2))^2 - sR / n1), 1)
      sum(((A1 * n0 * m0 + X * n1) / (A1 * n0 + n1) +
             sqrt(sR / (A1 * n0 + n1)) * qnorm(sig)) > 0) / R
    }
    if (t1 < aD) {
      S <- As
    } else {
      while ((abs(t1 - t2) > tol) & (abs(uL - lL) > tol)) {
        if (t2 != 0) t1 <- t2
        while ((t1 > aD) & (abs(uL - lL) > tol)) {
          uL <- S; S <- mean(c(lL, uL)); t1 <- borrowing(S)
        }
        if (t1 < pnorm((sqrt(n0 + n1) * qnorm(sig) + n0 * m0) / sqrt(sR * n1))) t2 <- t1
        while ((t2 < aD) & (abs(uL - lL) > tol)) {
          lL <- S; S <- mean(c(lL, uL)); t2 <- borrowing(S)
        }
      }
    }
    S
  }

  # The paper carries one variance shared by the historical and new data, so
  # the port reproduces it by giving the source and target the same sampling
  # variance. Its prior sample size is then the paper's n0 directly.
  ported <- function(n0, n1, m0, aD, sig, sR, tol) {
    as.numeric(findCalibrationParameter(
      source_sample_size_per_arm = n0, target_sample_size_per_arm = n1,
      source_treatment_effect_estimate = m0, desired_tie = aD,
      significance_level = sig, target_data_sampling_variance = sR,
      source_data_sampling_variance = sR,
      tolerance = tol, theta_0 = 0)[1, 2])
  }

  # The two configurations the paper's script runs, plus one more.
  for (design in list(list(n0 = 5, n1 = 50, m0 = 0.4, aD = 0.065),
                      list(n0 = 25, n1 = 50, m0 = 0.4, aD = 0.065),
                      list(n0 = 25, n1 = 50, m0 = 0.3, aD = 0.065))) {
    reference <- reference_find_S(
      R = 1e6, n0 = design$n0, n1 = design$n1, m0 = design$m0,
      aD = design$aD, sig = 0.05, sR = 1, tol = 0.0001, seed = 1010
    )
    exact <- ported(design$n0, design$n1, design$m0, design$aD, 0.05, 1, 0.0001)
    # The reference varies by ~0.003 across seeds at this many draws.
    expect_lt(abs(reference - exact), 0.015,
              label = sprintf("|reference - exact| for n0=%g m0=%g (%.6f vs %.6f)",
                              design$n0, design$m0, reference, exact))
  }
})

test_that("the reported full-borrowing type I error is equation (5)", {
  # Equation (5) of Nikolakopoulos et al (2018):
  #   Phi((sigma * sqrt(n0 + n1) * z_{1-eta} + n0 * mu_0) / (sqrt(n1) * sigma)).
  # The search compares against this value in two places; the reference script
  # wrote the second without its leading sigma, which is an identity only when
  # sigma^2 is 1. This pins the quantity so the two cannot drift apart again.
  for (design in calibration_designs()) {
    calibration <- findCalibrationParameter(
      source_sample_size_per_arm = design$ess,
      target_sample_size_per_arm = design$n,
      source_treatment_effect_estimate = design$theta_s,
      desired_tie = 0.065, significance_level = 0.05,
      target_data_sampling_variance = design$tau2,
      source_data_sampling_variance = design$sigma0_2,
      tolerance = 1e-4, theta_0 = 0
    )
    n0 <- design$ess * design$tau2 / design$sigma0_2
    expected <- pnorm(
      (sqrt(design$tau2) * sqrt(n0 + design$n) * qnorm(0.05) + n0 * design$theta_s) /
        sqrt(design$tau2 * design$n)
    )
    expect_equal(as.numeric(calibration[5, 2]), expected, tolerance = 1e-12)
  }
})
