#' Count number of taxa in an occurrence data frame
#'
#' Count the number of taxa (species, genera, families, orders,
#' classes, and phyla) in an occurrence data frame.
#'
#' @param data Data frame from a query to PaleobioDB as returned by
#'   the [pbdb_occurrences()] function using the argument `show =
#'   "class"` or `show = "classext"`.
#' @param do_plot Logical indicating whether to produce a plot (`TRUE`
#'   by default).
#' @param col Colour of the histogram.
#' @returns A data frame with the number of subtaxa in the data.
#' @export
#' @examples \dontrun{
#'   canidae_quat <- pbdb_occurrences(
#'     limit = "all", base_name = "Canidae",  interval = "Quaternary",
#'     show = c("coords", "classext", "ident"), vocab = "pbdb"
#'   )
#'   pbdb_subtaxa(canidae_quat)
#' }
pbdb_subtaxa <- function(data,
                         do_plot = TRUE,
                         col = "#0000FF") {
  species <- nrow(
    pbdb_temp_range(data = data, rank = "species", do_plot = FALSE)
  )
  genera <- nrow(
    pbdb_temp_range(data = data, rank = "genus", do_plot = FALSE)
  )
  families <- nrow(
    pbdb_temp_range(data = data, rank = "family", do_plot = FALSE)
  )
  orders <- nrow(
    pbdb_temp_range(data = data, rank = "order", do_plot = FALSE)
  )
  classes <- nrow(
    pbdb_temp_range(data = data, rank = "class", do_plot = FALSE)
  )
  phyla <- nrow(
    pbdb_temp_range(data = data, rank = "phylum", do_plot = FALSE)
  )
  subtaxa <- data.frame(species, genera, families, orders, classes, phyla)

  if (do_plot) {
    par(mar = c(8, 4, 2, 0))
    barplot(
      unlist(subtaxa),
      beside = TRUE,
      horiz = FALSE,
      col = col,
      border = FALSE,
      las = 2,
      ylab = "Number of taxa"
    )
  }

  subtaxa
}
