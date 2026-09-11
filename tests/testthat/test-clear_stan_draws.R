test_that("clear_stan_draws removes the directory of every process", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "aprepitant_RMP_111"))
  dir.create(file.path(root, "aprepitant_RMP_222"))
  # unlink() has to recurse: file.remove() cannot delete a non-empty directory,
  # and each process leaves its draws behind in one.
  file.create(file.path(root, "aprepitant_RMP_111", "draws.csv"))

  removed <- clear_stan_draws("Aprepitant", "RMP", root = root)

  expect_length(removed, 2)
  expect_false(dir.exists(file.path(root, "aprepitant_RMP_111")))
  expect_false(dir.exists(file.path(root, "aprepitant_RMP_222")))
})


test_that("clear_stan_draws leaves other case studies and methods alone", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "aprepitant_RMP_111"))
  dir.create(file.path(root, "aprepitant_conditional_power_prior_111"))
  dir.create(file.path(root, "belimumab_RMP_111"))

  clear_stan_draws("aprepitant", "RMP", root = root)

  expect_false(dir.exists(file.path(root, "aprepitant_RMP_111")))
  expect_true(dir.exists(file.path(root, "aprepitant_conditional_power_prior_111")))
  expect_true(dir.exists(file.path(root, "belimumab_RMP_111")))
})


test_that("clear_stan_draws only removes a process-id suffix", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "aprepitant_RMP_111"))
  # A method whose name extends another's would otherwise be swept up with it.
  dir.create(file.path(root, "aprepitant_RMP_variant_111"))

  clear_stan_draws("aprepitant", "RMP", root = root)

  expect_false(dir.exists(file.path(root, "aprepitant_RMP_111")))
  expect_true(dir.exists(file.path(root, "aprepitant_RMP_variant_111")))
})


test_that("clear_stan_draws copes with a draws root that does not exist", {
  absent <- file.path(withr::local_tempdir(), "never-created")

  expect_length(clear_stan_draws("aprepitant", "RMP", root = absent), 0)
})
