# Simulation study logic - high level

## Introduction

The aim of this document is to illustrate how to run the simulation
study for user-defined configurations. It explains how to define a
specific scenario (source and target study characterstics), instantiate
a Bayesian model for partial extrapolation, define the configuration to
evaluate the frequentist OCs of this model.

The simulation study pipeline is designed in such a way that the
complexity behind model creation and data generation is abstracted away,
so that it is very easy for a new user to evaluate a specific method in
arbitrary scenarios. This “high-level” description of the simulation
study implementation is accompanied with a corresponding “low-level”
description in another vignette, which describes in more details the
components used at each step.

Source other scripts

``` r

env <- "full"
config_dir <- paste0(system.file(paste0("conf/", env), package = "RBExT"), "/")
scenarios_config <- yaml::read_yaml(paste0(config_dir, "scenarios_config.yml"))
```

## Load the case study configuration

Load the simulation configuration and the Belimumab case study
configuration from YAML files.

``` r

set.seed(42)
config_path <- system.file("conf/simulation_config.yml", package = "RBExT")
simulation_config <- yaml::yaml.load_file(config_path)

case_study <- "botox"
config_path <- system.file("conf/case_studies/botox.yml", package = "RBExT")
case_study_config <- yaml::yaml.load_file(config_path)
```

The case study configuration file contains all necessary information
about the case study, this includes: treatment effect distribution,
endpoint type, side of the null hypothesis, sample sizes, treatment
effect estimate in the source study, etc.

The simulation configuration specifies parameters such as the number of
simulation replicates, the number of drift values considered, the
critical value for the Bayesian decision criterion, the environment, the
case studies to iterate on and the methods considered.

Any modification to the simulation settings should be made through these
configuration files.

## Specific scenario

For a given case study, we will loop over different methods. Here, we
choose the RMP for illustration. For the considered method, we also loop
over target study sample size, drift, control drift, and method
parameters.

Here, we choose the following :

``` r

method <- "RMP"
method_parameters <- list(
  initial_prior = "noninformative", # This corresponds to the fact that the posterior for the adults data is derived from an uninformative prior (for consistency with other methods). May be removed in future releases.
  prior_weight = 0.5, # weight on the informative component of the mixture
  empirical_bayes = FALSE # Corresponds to whether some parameters of the prior are based on observed data.
)

target_sample_size_per_arm <- 91
drift <- 0.4
```

## Create data objects

Create a source_data instance, where information about the source data
is stored. We can also specify the denominator in the source treatment
effect summary measure (optional), in the in case we want to modify it.

``` r

source_denominator <- case_study_config$source$responses$control / case_study_config$source$total
source_data <- SourceData$new(case_study_config, source_denominator)
print(source_data)
```

    ## <SourceData>
    ##   Inherits from: <ObservedSourceData>
    ##   Public:
    ##     clone: function (deep = FALSE) 
    ##     control_rate: NULL
    ##     endpoint: continuous
    ##     equivalent_source_sample_size_per_arm: 233.995726495727
    ##     initialize: function (case_study_config, source_denominator = NA) 
    ##     sample_size_control: 235
    ##     sample_size_treatment: 233
    ##     standard_error: 0.1
    ##     summary_measure_likelihood: normal
    ##     to_dict: function () 
    ##     treatment_effect_estimate: 0.2
    ##     treatment_rate: NULL

Alternatively, if we want to use the control rate from the source study
data:

``` r

source_data <- SourceData$new(case_study_config)
print(source_data)
```

    ## <SourceData>
    ##   Inherits from: <ObservedSourceData>
    ##   Public:
    ##     clone: function (deep = FALSE) 
    ##     control_rate: NULL
    ##     endpoint: continuous
    ##     equivalent_source_sample_size_per_arm: 233.995726495727
    ##     initialize: function (case_study_config, source_denominator = NA) 
    ##     sample_size_control: 235
    ##     sample_size_treatment: 233
    ##     standard_error: 0.1
    ##     summary_measure_likelihood: normal
    ##     to_dict: function () 
    ##     treatment_effect_estimate: 0.2
    ##     treatment_rate: NULL

Create an instance of the BinaryTargetData class, which will allow us to
sample target study data. Note that this target_data depends on the
specific parameters chosen for the scenario (in particular, the drift
and target sample size).

``` r

summary_measure_likelihood <- case_study_config$summary_measure_likelihood
endpoint <- case_study_config$endpoint
target_data <- TargetDataFactory$new()
target_data <- target_data$create(
  source_data = source_data, case_study_config = case_study_config,
  target_sample_size_per_arm = target_sample_size_per_arm, treatment_drift = drift, summary_measure_likelihood = source_data$summary_measure_likelihood, target_to_source_std_ratio = 1
)
```

Here, we use a factory method design pattern. The target_data object is
of the BinaryTargetData class, which inherits from the TargetData class.

``` r

print(target_data)
```

    ## <ContinuousTargetData>
    ##   Inherits from: <TargetData>
    ##   Public:
    ##     clone: function (deep = FALSE) 
    ##     control_drift: 0
    ##     drift: 0.4
    ##     endpoint: continuous
    ##     generate: function (n_replicates) 
    ##     initialize: function (source_data, sampling_approximation, target_sample_size_per_arm, 
    ##     plot_sample: function (data) 
    ##     sample: NULL
    ##     sample_size_control: 91
    ##     sample_size_per_arm: 91
    ##     sample_size_treatment: 91
    ##     sampling_approximation: FALSE
    ##     standard_deviation: 1.52969188562837
    ##     summary_measure_likelihood: normal
    ##     to_dict: function () 
    ##     treatment_drift: 0.4
    ##     treatment_effect: 0.6

Now, we define the model we want to use for inferring the treatment
effect in the target study. Again, the model class definition relies on
the factory method design patter, which is very convenient in this
situation, as you can use the same function with different arguments to
instantiate any model:

``` r

model <- Model$new()

model <- model$create(
  case_study_config = case_study_config,
  method = method,
  method_parameters = method_parameters,
  source_data = source_data
)
```

Why does the model depend on the source data? Because the model depends
on the type of endpoint and summary measure, which are stored within the
source_data object.

## Launching the simulation

We first explicitly retrieve parameters that are important for computing
the OCs (the boundary of the null space and the critical value for the
Bayesian decision criterion), as well as the number of replicates for
the simulation study (which will be 10000)

``` r

theta_0 <- case_study_config$theta_0
critical_value <- simulation_config$critical_value
n_replicates <- scenarios_config$n_replicates
```

And now, it is finally time to compute the frequentist OCs of this model
in this scenario:

``` r

sim_outputs <- model$estimate_frequentist_operating_characteristics(
  theta_0 = theta_0,
  target_data = target_data,
  n_replicates = n_replicates,
  critical_value = critical_value,
  confidence_level = 0.95,
  null_space = case_study_config$null_space,
  verbose = 0,
  n_samples_quantiles_estimation = simulation_config$n_samples_quantiles_estimation,
  method = method,
  case_study = case_study
)

format_simulation_output_table(sim_outputs)
```

    ##               Metric    Value               95% CI
    ##  Success Probability  0.99540 [ 0.99387,  0.99663]
    ##             Coverage  0.83830 [ 0.83094,  0.84547]
    ##                  MSE  0.04734 [ 0.04649,  0.04819]
    ##                 Bias -0.10636 [-0.11008, -0.10264]
    ##       Posterior Mean  0.49364 [ 0.48992,  0.49736]
    ##     Posterior Median  0.48248 [ 0.47850,  0.48646]
    ##            Precision  0.32018 [ 0.31904,  0.32132]
    ##    Credible Interval       NA [ 0.20253,  0.84289]
    ##           ESS Moment  5.97814 [ 4.95184,  7.00443]
    ##        ESS Precision  6.63423 [ 5.73947,  7.52898]
    ##             ESS ELIR 87.52925 [87.26959, 87.78891]

At this point, the pipeline includes some logic to store the results as
a row in a results table. Once we have looped over all scenarios and
methods, the results table is then used to make figures

``` r

sessionInfo()
```

    ## R version 4.2.0 (2022-04-22)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 24.04.5 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices datasets  utils     methods   base     
    ## 
    ## other attached packages:
    ## [1] ggplot2_4.0.3 RBExT_0.0.2  
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] matrixStats_1.5.0    fs_2.1.0             assertions_0.3.0    
    ##   [4] progress_1.2.3       doParallel_1.0.17    RColorBrewer_1.1-3  
    ##   [7] rstan_2.32.7         latex2exp_0.9.8      tensorA_0.36.2.1    
    ##  [10] tools_4.2.0          backports_1.5.1      bslib_0.12.0        
    ##  [13] R6_2.6.1             rpart_4.1.16         otel_0.2.0          
    ##  [16] colorspace_2.1-3     Hmisc_5.3-0          nnet_7.3-17         
    ##  [19] withr_3.0.3          prettyunits_1.2.0    tidyselect_1.2.1    
    ##  [22] gridExtra_2.3.1      processx_3.9.0       compiler_4.2.0      
    ##  [25] extrafontdb_1.1      textshaping_1.0.5    cli_3.6.6           
    ##  [28] htmlTable_2.5.0      HDInterval_0.2.4     xml2_1.6.0          
    ##  [31] desc_1.4.3           posterior_1.7.1      sass_0.4.10         
    ##  [34] scales_1.4.0         checkmate_2.3.4      mvtnorm_1.4-2       
    ##  [37] S7_0.2.2             readr_2.2.0          pkgdown_2.2.1       
    ##  [40] QuickJSR_1.11.0      systemfonts_1.3.2    stringr_1.6.0       
    ##  [43] digest_0.6.39        StanHeaders_2.39.1   foreign_0.8-82      
    ##  [46] rmarkdown_2.32       svglite_2.2.2        base64enc_0.1-6     
    ##  [49] pkgconfig_2.0.3      htmltools_0.5.9      extrafont_0.20      
    ##  [52] fastmap_1.2.0        pwr_1.3-0            htmlwidgets_1.6.4   
    ##  [55] rlang_1.3.0          rstudioapi_0.19.0    jquerylib_0.1.4     
    ##  [58] farver_2.1.2         generics_0.1.4       jsonlite_2.0.0      
    ##  [61] dplyr_1.2.1          distributional_0.9.0 inline_0.3.21       
    ##  [64] magrittr_2.0.5       kableExtra_1.4.1     Formula_1.2-6       
    ##  [67] loo_2.10.1.9000      Rcpp_1.1.2           abind_1.4-8         
    ##  [70] viridis_0.6.5        lifecycle_1.0.5      stringi_1.8.9       
    ##  [73] yaml_2.3.12          pkgbuild_1.4.8       grid_4.2.0          
    ##  [76] parallel_4.2.0       crayon_1.5.3         hms_1.1.4           
    ##  [79] knitr_1.52           ps_1.9.3             pillar_1.11.1       
    ##  [82] codetools_0.2-18     stats4_4.2.0         rstantools_2.7.1    
    ##  [85] glue_1.8.1           evaluate_1.0.5       data.table_1.18.6.1 
    ##  [88] renv_1.0.11          RcppParallel_6.2.1   vctrs_0.7.3         
    ##  [91] tzdb_0.5.0           Rdpack_2.6.6         foreach_1.5.2       
    ##  [94] Rttf2pt1_1.3.14      gtable_0.3.6         purrr_1.2.2         
    ##  [97] tidyr_1.3.2          assertthat_0.2.1     cachem_1.1.0        
    ## [100] xfun_0.60            rbibutils_2.4.1      tidyverse_2.0.0     
    ## [103] roxygen2_8.1.0       ragg_1.5.2           viridisLite_0.4.3   
    ## [106] truncnorm_1.0-9      Bolstad2_1.0-29      RBesT_1.11-0        
    ## [109] tibble_3.3.1         iterators_1.0.14     cluster_2.1.3       
    ## [112] statmod_1.5.2        cmdstanr_0.9.0
