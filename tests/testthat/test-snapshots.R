# Visual regression tests: each panel is compared with the SVG stored in _snaps/snapshots/.
# After an intended change, review and accept the new version with
#   testthat::snapshot_review("snapshots", path = "tests/testthat")
# Panel A is left out: it would store both microscopy images in the snapshot.

for (panel in paste0("panel_", LETTERS[2:6])) {
  test_that(paste(panel, "looks the same"), {
    vdiffr::expect_doppelganger(panel, fig[[panel]])
  })
}
