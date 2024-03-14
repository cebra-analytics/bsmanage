context("ManageContext")

test_that("initializes with parameters", {

  expect_silent(conteX <- ManageContext(c("Species 1", "Species 2")))
  expect_is(conteX, "ManageContext")
  expect_named(conteX, c("get_species_names", "get_species_types",
                         "get_impact_types", "get_action_types",
                         "get_manage_scope"))
  expect_equal(conteX$get_species_names(), c("Species 1", "Species 2"))
  expect_equal(conteX$get_species_types(), c("pest", "weed", "disease"))
  expect_equal(conteX$get_impact_types(), "none")
  expect_equal(conteX$get_action_types(), "none")
  expect_equal(conteX$get_manage_scope(), "optimal")
  expect_silent(conteX <- ManageContext(
    c("Species 1", "Species 2"),
    species_types = c("pest", "pest"),
    impact_types = c("ecological", "social"),
    action_types = c("surveillance", "removal"),
    manage_scope = "scenarios"))
  expect_equal(conteX$get_species_types(), c("pest", "pest"))
  expect_equal(conteX$get_impact_types(), c("ecological", "social"))
  expect_equal(conteX$get_action_types(), c("surveillance", "removal"))
  expect_equal(conteX$get_manage_scope(), "scenarios")
})
