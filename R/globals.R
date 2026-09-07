# Declare symbols that are either non-standard-evaluation column names used
# inside dplyr/rlang pipelines, or configuration variables that this package's
# functions intentionally resolve from the calling environment rather than
# receiving as arguments. Neither is a real "undefined variable" bug; this
# suppresses the corresponding R CMD check NOTE and lintr warning.
utils::globalVariables(c(
  ".", "analysis_config", "bayesian_metrics", "case_studies_config_dir",
  "case_study", "categories", "CI", "CI_low", "CI_power", "CI_tie", "CI_upper",
  "color", "combined_mean_CI", "conf_int_lower", "conf_int_success_proba_lower",
  "conf_int_success_proba_upper", "conf_int_tie_lower", "conf_int_tie_upper",
  "conf_int_upper", "control_drift", "design_prior_type", "dpi", "drift",
  "drift_value", "effect", "effect_label", "effect_name", "effect_type",
  "empirical_bayes_hyperparameters", "error_low", "error_low_power",
  "error_low_tie", "error_upper", "error_upper_power", "error_upper_tie",
  "figures_dir", "font", "frequentist_metrics",
  "frequentist_power_at_equivalent_tie_lower", "hyperparameter",
  "inference_metrics", "jitter_offset", "jittered_x", "label", "markersize",
  "mcse_success_proba", "means", "method", "Method", "methods_dict",
  "methods_labels", "nominal_frequentist_power_separate", "null_space", "OCs",
  "p_value", "parameters", "Parameters", "plots_to_latex",
  "relative_error_cap_width", "remake_figures", "rows", "small_text_size",
  "source_denominator_change_factor", "source_treatment_effect_estimate",
  "success_proba", "tables_dir", "target_sample_size_per_arm",
  "target_to_source_std_ratio", "target_treatment_effect", "text_size",
  "textwidth", "theta_0", "tie", "total_width", "treatment_drift", "x",
  "xvars", "y", "ymax", "ymin"
))
