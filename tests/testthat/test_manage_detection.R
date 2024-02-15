context("ManageDetection")

test_that("initializes with region, population, and surveillance", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template_vect <- template[region$get_indices()][,1]
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(template),
                                establish_pr = template_vect*0,
                                lambda = 1,
                                optimal = "none",
                                exist_sens = template_vect)
  expect_error(manage_detection <- ManageDetection(
    region, population_model, "surveillance", apply_stages = 2:3),
    paste("Surveillance object must be a 'SurveillanceDesign' or inherited",
          "class object."))
  template2 <- template*1
  template2[1] <- NA
  region2 <- bsspread::Region(template2)
  expect_error(manage_detection <- ManageDetection(
    region2, bsspread::StagedPopulation(region2, stage_matrix), surveillance,
    apply_stages = 2:3),
    "Surveillance object must be compatible with the region object.")
  expect_silent(manage_detection <- ManageDetection(
    region, population_model, surveillance, apply_stages = 2:3))
  class(manage_detection) # "ManageDetection" "ManageActions"
  expect_is(manage_detection, "ManageDetection")
  expect_s3_class(manage_detection, "ManageActions")
  expect_named(manage_detection, c(c("get_type", "apply")))
  expect_equal(manage_detection$get_type(), "detection")
})
