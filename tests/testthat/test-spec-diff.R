test_that("diff_spec_ir accurately detects added, removed, and modified paths", {
  s1 <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 20L, icc = 0.15),
    simulation = list(n_reps = 200L, seed = 2026L)
  )

  # Identical spec has zero changes
  d0 <- diff_spec_ir(s1, s1)
  expect_s3_class(d0, "spec_diff")
  expect_false(d0$has_changes)
  expect_equal(d0$n_changes, 0L)
  expect_equal(format_diff_summary(d0), "No changes.")

  # Modify cluster count, add random slope, add parameter grid
  s2 <- s1
  s2$hierarchy$n_clusters <- 75L
  s2$effects$random$slopes <- list(list(predictor = "x1", sd = 0.25))
  s2$effects$random$corr_matrix <- matrix(c(1, 0.2, 0.2, 1), 2, 2)
  s2$simulation$parameter_grid <- list(icc = c(0.1, 0.2, 0.3))

  d1 <- diff_spec_ir(s1, s2)
  expect_true(d1$has_changes)
  expect_true("hierarchy.n_clusters" %in% d1$changed_paths)
  expect_true("effects.random.slopes.x1.sd" %in% d1$changed_paths)
  expect_true("effects.random.corr_matrix" %in% d1$changed_paths)
  expect_true("simulation.parameter_grid.icc" %in% d1$changed_paths)
  expect_true(all(c("hierarchy", "effects", "simulation") %in% d1$changed_sections))

  row_cl <- d1$changes[d1$changes$path == "hierarchy.n_clusters", ]
  expect_equal(row_cl$change_type, "modified")
  expect_equal(row_cl$old_value, "40")
  expect_equal(row_cl$new_value, "75")

  row_grid <- d1$changes[d1$changes$path == "simulation.parameter_grid.icc", ]
  expect_equal(row_grid$change_type, "added")

  # Removing grid reports "removed"
  s3 <- s2
  s3$simulation$parameter_grid <- list()
  d2 <- diff_spec_ir(s2, s3)
  expect_equal(d2$changes$change_type[d2$changes$path == "simulation.parameter_grid.icc"], "removed")

  expect_no_error(capture.output(print(d1), type = "message"))
  expect_true(nzchar(format_diff_summary(d1)))
})
