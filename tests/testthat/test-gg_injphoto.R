# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

# Then, use vdiffr::expect_doppleganger(<name of plot>, <ggplot object>) to make a test that fails if there are visual changes in <ggplot object>.
# test_that("output of ggplot() is stable", {
#   vdiffr::expect_doppelganger("A blank plot", ggplot())
# })
#
# test_that("plots have known output", {
#   disp_hist_base <- function() hist(mtcars$disp)
#   expect_doppelganger("disp-histogram-base", disp_hist_base)
#
#   disp_hist_ggplot <- ggplot(mtcars, aes(disp)) + geom_histogram()
#   expect_doppelganger("disp-histogram-ggplot", disp_hist_ggplot)
# })
