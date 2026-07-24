context("ManageContext")

test_that("initializes with parameters", {
  expect_silent(conteX <- ManageContext(c("Species 1", "Species 2")))
  expect_is(conteX, "ManageContext")
  expect_named(conteX, c("get_species_names", "get_species_types",
                         "get_resource_type", "get_management_purpose",
                         "get_threat_status"))
  expect_equal(conteX$get_species_names(), c("Species 1", "Species 2"))
  expect_equal(conteX$get_species_types(), c("pest", "weed", "disease"))

  expect_equal(conteX$get_resource_type(), "survey")
  expect_equal(conteX$get_management_purpose(), "delimitation")
  expect_equal(conteX$get_threat_status(), "detected")
  expect_silent(conteX <- ManageContext(c("Species 1", "Species 2"),
                          species_types = c("pest", "pest"),
                          resource_type = "treatment",
                          management_purpose = "containment",
                          threat_status = "contained"))
  expect_equal(conteX$get_species_types(), c("pest", "pest"))
  expect_equal(conteX$get_resource_type(), "treatment")
  expect_equal(conteX$get_management_purpose(), "containment")
  expect_equal(conteX$get_threat_status(), "contained")
})
