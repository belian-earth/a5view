.onLoad <- function(libname, pkgname) {
  # ggplot2 (Suggests) looks for a scale for every aesthetic. The `cell`
  # aesthetic of geom_a5() is an id, not a position or colour: "identity"
  # makes ggplot2 look for scale_cell_identity(), find none, and leave
  # it alone without a message.
  vctrs::s3_register("ggplot2::scale_type", "a5_cell", function(x) "identity")
  # cells_rgb() output is already colour: ggplot2 picks an identity
  # scale (no legend) and geom_a5() unpacks it.
  vctrs::s3_register("ggplot2::scale_type", "a5_rgb", function(x) "identity")
}
