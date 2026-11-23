test_that("all bundled presets compile into valid specs and demo app constructs cleanly", {
  presets <- spec_presets()
  expect_true(length(presets) >= 4L)

  for (nm in names(presets)) {
    res <- nl_to_spec(presets[[nm]]$prompt)
    expect_true(res$validation$valid, info = paste("Preset failed:", nm))
    expect_true(spec_to_controls(res$spec)$control_count >= 5L)
  }

  app <- specshiny_demo_app()
  expect_s3_class(app, "shiny.appobj")

  demo_file <- system.file("shiny", "demo", "app.R", package = "specshiny")
  expect_true(nzchar(demo_file) && file.exists(demo_file))
})
