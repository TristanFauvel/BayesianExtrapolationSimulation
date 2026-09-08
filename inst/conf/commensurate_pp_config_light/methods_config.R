# Full protocol configuration
methods_dict <- list(
  RMP = list(
    prior_weight = list(
      range = seq(0, 1, length.out = 11),
      parameter_name = "Prior weight",
      parameter_label = "w",
      parameter_notation = "$w$",
      type = "continuous",
      important_values = c(0.5),
      is_updated = TRUE
    ),
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    ),
    empirical_bayes = list(
      range = list(TRUE),
      parameter_name = "Empirical Bayes",
      parameter_label = "EB",
      parameter_notation = "EB",
      type = "categorical",
      is_updated = FALSE
    )
  ),
  NPP = list(
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    ),
    power_parameter_mean = list(
      range = list(0.5),
      parameter_name = "Power parameter prior mean",
      parameter_label = "xi_gamma",
      parameter_notation = "$\\xi_\\gamma$",
      type = "continuous",
      is_updated = FALSE,
      display = TRUE
    ),
    power_parameter_std = list(
      range = list(0.1, 0.2, 0.4),
      parameter_name = "Power parameter prior std",
      parameter_label = "sigma_gamma",
      parameter_notation = "$\\sigma_\\gamma$",
      type = "continuous",
      is_updated = FALSE,
      display = TRUE
    )
  ),
  separate = list(
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    )
  ),
  pooling = list(
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    )
  ),
  conditional_power_prior = list(
    power_parameter = list(
      range = seq(0, 1, length.out = 5),
      parameter_name = "Power prior parameter",
      parameter_label = "gamma",
      parameter_notation = "$\\gamma$",
      type = "continuous",
      important_values = c(0.5),
      is_updated = FALSE
    ),
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    )
  ),
  p_value_based_PP = list(
    shape_parameter = list(
      range = list(0.01, 0.1, 1, 10, 20),
      parameter_name = "Shape parameter",
      parameter_label = "k",
      parameter_notation = "$k$",
      type = "continuous",
      is_updated = FALSE
    ),
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    ),
    equivalence_margin = list(
      range = list(0.1, 0.5),
      parameter_name = "lambda",
      parameter_label = "lambda",
      parameter_notation = "$\\lambda$",
      type = "continuous",
      is_updated = FALSE
    )
  ),
  PDCCPP = list(
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    ),
    desired_tie = list(
      range = list(0.065),
      parameter_name = "Desired TIE",
      parameter_label = "desired_tie",
      parameter_notation = "Desired TIE",
      type = "continuous",
      is_updated = FALSE
    ),
    significance_level = list(
      range = list(0.05),
      parameter_name = "Significance level",
      parameter_label = "sig",
      parameter_notation = "Significance level",
      type = "continuous",
      is_updated = FALSE
    ),
    tolerance = list(
      range = list(0.0001),
      parameter_name = "Tolerance",
      parameter_label = "tol",
      parameter_notation = "Tolerance",
      type = "continuous",
      is_updated = FALSE
    ),
    n_iter = list(
      range = list(1e6),
      parameter_name = "Number of iterations",
      parameter_label = "n_iter",
      parameter_notation = "Number of iterations",
      type = "integer",
      is_updated = FALSE
    )
  ),
  EB_PP = list(
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    )
  ),
  test_then_pool_difference = list(
    significance_level = list(
      range = list(0.01, 0.1, 0.4, 0.8),
      parameter_name = "eta",
      parameter_label = "eta",
      parameter_notation = "$\\eta$",
      type = "continuous",
      is_updated = FALSE
    ),
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    )
  ),
  test_then_pool_equivalence = list(
    significance_level = list(
      range = list(0.1, 0.5),
      parameter_name = "eta",
      parameter_label = "eta",
      parameter_notation = "$\\eta$",
      type = "continuous",
      is_updated = FALSE
    ),
    equivalence_margin = list(
      range = list(0.1, 0.5, 0.8),
      parameter_name = "lambda",
      parameter_label = "lambda",
      parameter_notation = "$\\lambda$",
      type = "continuous",
      is_updated = FALSE
    ),
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    )
  ),
  commensurate_power_prior = list(
    heterogeneity_prior = list(
      range = list(
        list(family = "half_normal", std_dev = 1),
        list(family = "half_normal", std_dev = 5)
      ),
      parameter_name = "Heterogeneity prior",
      parameter_label = "tau",
      parameter_notation = "$\\tau$",
      type = "categorical",
      is_updated = FALSE
    ),
    initial_prior = list(
      range = list("noninformative"),
      parameter_name = "Initial prior",
      parameter_label = "pi_0",
      parameter_notation = "$\\pi_0$",
      type = "categorical",
      is_updated = FALSE
    )
  )
)
