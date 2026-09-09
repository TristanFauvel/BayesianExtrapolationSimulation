analyze_power_gains <- function(results_freq_df, output_path){
  # Check whether there are cases where the success proba is higher in the alternative space than the frequentist power at equivalent TIE
  subdf <- subset(results_freq_df,
                  (null_space == "left" & target_treatment_effect > theta_0) |
                    (null_space == "right" & target_treatment_effect < theta_0))

  # Determine whether the power of the method of interest is higher than the
  # power of a separate analysis at equivalent TIE. The decision rests on an
  # interval for the difference that recovers a variance from each of the two
  # intervals, rather than on standard errors reconstructed by assuming those
  # intervals were symmetric and normal.
  subdf <- flag_power_differences(subdf)

  power_gain_cases <- subset(subdf, power_gain)


  # Determine whether the power of the method of interest is higher than the power of the frequentist method at the nominal TIE (otherwise there is no point in borrowing)
  power_gain_cases <- subset(power_gain_cases, conf_int_success_proba_lower > nominal_frequentist_power_separate)


  # Detect anomalies where a separate analysis leads to power gains
  separate_analysis_gain <- subset(power_gain_cases, method == "separate")

  write.csv(file = paste0(output_path, "/", "power_gain_cases.csv"), power_gain_cases)#, power_gain_cases[,expected_colnames_scenario])

  write.csv(file = paste0(output_path, "/", "power_gain_cases_separate_analysis.csv"), separate_analysis_gain)

}

analyze_power_loss <- function(results_freq_df, output_path){
  # Check whether there are cases where the success proba is lower in the alternative space than the frequentist power at equivalent TIE while TIE is inflated compared to the nominal TIE.
  subdf <- subset(results_freq_df,
                  (null_space == "left" & target_treatment_effect > theta_0) |
                    (null_space == "right" & target_treatment_effect < theta_0))

  # The same difference interval decides losses as decides gains, so the two
  # analyses no longer apply different levels of stringency to the same
  # comparison.
  subdf <- flag_power_differences(subdf)
  power_loss_cases <- subset(subdf, power_loss)


  write.csv(file = paste0(output_path, "/","power_loss_cases.csv"), power_loss_cases[,expected_colnames_scenario])
}

analyze_power_loss_inflated_tie <- function(results_freq_df, output_path){
  # Check whether there are cases where the success proba is lower in the alternative space than the frequentist power at equivalent TIE while TIE is inflated compared to the nominal TIE.
  subdf <- subset(results_freq_df,
                  (null_space == "left" & target_treatment_effect > theta_0) |
                    (null_space == "right" & target_treatment_effect < theta_0))

  subdf <- subset(subdf, conf_int_tie_lower > 0.025)

  subdf <- flag_power_differences(subdf)
  power_loss_cases <- subset(subdf, power_loss)


  write.csv(file = paste0(output_path, "/", "power_loss_inflated_tie_cases.csv"), power_loss_cases[,expected_colnames_scenario])

}

analyze_noninflated_tie <- function(results_freq_df, output_path){
  # Check whether there are cases where the TIE is not inflated

  non_inflated_tie_cases <- subset(results_freq_df, conf_int_tie_upper < 0.025)
  non_inflated_tie_cases <- subset(non_inflated_tie_cases, drift == - source_treatment_effect_estimate)

  # Remove the cases where there was no borrowing
  # filter <- non_inflated_tie_cases$conf_int_ess_moment_upper >= 0 & non_inflated_tie_cases$conf_int_ess_moment_lower >= 0
  # non_inflated_tie_cases <- non_inflated_tie_cases[!filter,]


  write.csv(file = paste0(output_path, "/", "noninflated_tie_cases.csv"), non_inflated_tie_cases)

}
