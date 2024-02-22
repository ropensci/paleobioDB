.extract_lat_lng <- function(data) {
  latlong <- data.frame(lng = data$lng, lat = data$lat)
  spl_df <- split(latlong, list(latlong$lng, latlong$lat), drop = TRUE)
  spl_count <- lapply(spl_df, function(x) cbind(x[1, ], Occur = nrow(x)))
  counts <- do.call(rbind, spl_count)
  counts
}

.add_col_ocean <- function(col_ocean, col_int, ...) {
  margins <- c(5.1, 0, 0, 0)
  maps::map(type = "n", mar = margins, ...)
  rect(
    par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
    col = col_ocean
  )
  maps::map(col = col_int, fill = TRUE, mar = margins, add = TRUE, ...)
}

#' @importFrom grDevices adjustcolor colorRampPalette
.add_points <- function(y, col_point, pch, ...) {
  pal <- colorRampPalette(col_point)
  y$n <- as.numeric(cut(y$Occur, breaks = 5))
  y$Col <- pal(5)[y$n]
  points(y[, 1:2], col = adjustcolor(y$Col, alpha.f = 0.8), pch = pch, ...)
  y
}

.add_legend <- function(y1, pch, ...) {
  # Overlay plot with transparent plot over the entire device
  opar <- par(
    fig = c(0, 1, 0, 1),
    oma = c(0, 0, 0, 0),
    mar = c(0, 0, 0, 0),
    new = TRUE
  )
  on.exit(par(opar))
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

  n <- length(unique(y1$Col))
  cols <- unique(y1$Col)[order(unique(y1$n))]
  lg_vals <- as.integer(seq(min(y1$Occur), max(y1$Occur), length.out = n))
  legend(
    "bottom",
    col = cols, legend = lg_vals, pch = pch, bg = "white",
    horiz = TRUE, inset = 0.01,
    title = "Occurrences", ...
  )
}

#' Map the fossil records
#'
#' The function opens a new window with a map showing the distribution
#' of the fossil records as points. These points are coloured
#' according to the number of occurrences per cell.
#'
#' @param data Input data frame. This data frame is the output of the
#'   [pbdb_occurrences()] function using the argument `show =
#'   "coords"`. See also Details and Examples.
#' @param col_int The colour of the mainland.
#' @param pch See [par()].
#' @param col_ocean The colour of the ocean.
#' @param main Title of the map. See [par()].
#' @param col_point Two or more colours that are used to generate the
#'   colour gradient showing the number of occurrences per coordinate
#'   in the map.
#' @param do_plot Logical. If `TRUE`, the function produces a plot in
#'   addition to returning a data frame with the occurrence counts.
#' @param ... Other parameters. See [par()] and [map()].
#' @details The argument `show = "coords"` in the [pbdb_occurrences()]
#'   function is required. We recommend the use of a cairo device
#'   ([X11()]) for better visualization of the maps. See Examples.
#' @returns A data frame with the number of occurrences per
#'   coordinate.
#' @seealso See [pbdb_occurrences()], [map()], [par()] and [colors()]
#'   help pages.
#' @export
#' @examples \dontrun{
#'   data <- pbdb_occurrences(
#'     limit = "all", vocab = "pbdb", base_name = "Canis", show = "coords"
#'   )
#'   X11(width = 12, height = 8)
#'   pbdb_map(data)
#'   pbdb_map(data, pch = 1)
#'   pbdb_map(
#'     data,
#'     pch = 19,
#'     col_point = c("pink", "red"),
#'     col_ocean = "light blue",
#'     main = "Canis"
#'   )
#' }
pbdb_map <- function(data, col_int = "white", pch = 19, col_ocean = "black",
                     main = NULL, col_point = c("light blue", "blue"),
                     do_plot = TRUE, ...) {
  if (!all(c("lat", "lng") %in% names(data))) {
    err_msg <- strwrap(
      paste(
        "Invalid data input. Use the argument 'show = \"coords\"' in the",
        "\"pbdb_occurrences\" function, e.g. pbdb_occurrences(...,",
        "show = \"coords\").\n\nSee \"pbdb_map\" help page."
      )
    )
    stop(paste(err_msg, collapse = "\n"))
  }

  y <- .extract_lat_lng(data)
  # Reorder the points so that the coordinates with the highest number
  # of occurrences come out on top in the plot, making them more
  # visible
  y <- y[order(y$Occur), ]
  if (do_plot) {
    .add_col_ocean(col_ocean, col_int, ...)
    y1 <- .add_points(y, col_point, pch, ...)
    title(main = main, line = 1, ...)
    .add_legend(y1, pch, ...)
  }
  invisible(y)
}

#-------------------------------------------------

.add_col_ocean_2 <- function(col_ocean, col_int, ...) {
  margins <- c(0, 0, 0, 0)
  maps::map(type = "n", mar = margins, ...)
  rect(
    # Subtract a little bit from the right side of the ocean rectangle
    # to avoid overlap with the raster legend
    par("usr")[1], par("usr")[3], par("usr")[2] - 3, par("usr")[4],
    col = col_ocean
  )
  maps::map(col = col_int, fill = TRUE, mar = margins, add = TRUE, ...)
}

.rasterize_coords <- function(y, res, ...) {
  e <- maps::map(plot = FALSE, ...)
  ext <- ext(e$range)
  r <- rast(ext)
  res(r) <- res
  values(r) <- NA
  r <- rasterize(y[, 1:2], r, y[, 3], fun = sum)
  r
}

#' @importFrom grDevices adjustcolor colorRampPalette
.add_pattern <- function(r, col_eff, ...) {
  pal <- colorRampPalette(col_eff)
  plot(r, col = adjustcolor(pal(50), alpha.f = 0.8), add = TRUE, ...)
}

.plot_raster_rich <- function(r, col_eff, col_ocean, col_int,
                              res, lg_title, ...) {
  opar <- par(oma = c(0, 0, 0, 5.1))
  on.exit(par(opar))
  .add_col_ocean_2(col_ocean, col_int, ...)
  .add_pattern(r, col_eff, ...)
  mtext(lg_title, 4, line = 3, cex = 1.5)
}

#' Plot a raster showing the number of fossil occurrences
#'
#' Creates a `SpatRaster` object and a plot of the sampling
#' effort (number of fossil records per cell).
#'
#' @param data Input data frame. This data frame is the output of the
#'   [pbdb_occurrences()] function using the argument `show =
#'   "coords"`. See also Details and Examples.
#' @param res The resolution of the `SpatRaster` object (in decimal
#'   degrees). See [terra::res()].
#' @param col_int The colour of the mainland.
#' @param col_ocean The colour of the ocean.
#' @param col_eff Two or more colours that are used to generate the
#'   colour gradient showing the number of occurrences per cell in the
#'   map.
#' @param do_plot Logical. If `TRUE`, the function produces a plot in
#'   addition to returning a `SpatRaster`.
#' @param ... Other parameters. See [par()] and [map()]
#' @details The argument `show = "coords"` in the [pbdb_occurrences()]
#'   function is required. We recommend the use of a cairo device
#'   ([X11()]) for better visualization of the maps. See Examples.
#' @returns A `SpatRaster` object with the sampling effort (number of
#'   fossil records per cell). This `SpatRaster` object has the
#'   resolution that was specified in the `res` argument. The default
#'   is `res = 5`. Users that wish to work with objects of this type
#'   should load package `terra`.
#' @seealso See [pbdb_occurrences()], [map()], [par()] and [colors()]
#'   help pages
#' @export
#' @examples \dontrun{
#'   data <- pbdb_occurrences(
#'     limit = "all", vocab = "pbdb", base_name = "Canis", show = "coords"
#'   )
#'   X11(width = 13, height = 7.8)
#'   pbdb_map_occur(data, res = 2)
#'   ## Get the raster object without plotting it
#'   pbdb_map_occur(data, res = 3, do_plot = FALSE)
#' }
pbdb_map_occur <- function(data,
                           res = 5,
                           col_int = "white",
                           col_ocean = "black",
                           col_eff = c("light blue", "blue"),
                           do_plot = TRUE,
                           ...) {
  if (!all(c("lat", "lng") %in% names(data))) {
    stop(
      "Invalid data input. ",
      "Please, add 'show = c(\"coords\")' to your pbdb_occurrences query."
    )
  }

  y <- as.matrix(.extract_lat_lng(data))
  r <- .rasterize_coords(y, res, ...)
  if (do_plot) {
    .plot_raster_rich(
      r, col_eff, col_ocean, col_int, res,
      lg_title = "Number of records",
      ...
    )
  }
  r
}

#-------------------------------------------------

.extract_rank_species <- function(data, res = res) {
  e <- maps::map(plot = FALSE)
  ext <- ext(e$range)
  r <- rast(ext)
  res(r) <- c(res, res)
  values(r) <- 0

  if (length(data$accepted_rank) != 0) {
    identified <- data[!is.na(data$accepted_rank), ]
    species <- identified[identified$accepted_rank == "species", ]
    s <- split(species, species$accepted_no)
  } else if (length(data$rnk) != 0) {
    identified <- data[!is.na(data$rnk), ]
    species <- identified[identified$rnk == 3, ]
    s <- split(species, species$tid)
  }

  rast_list <- lapply(s, function(y) {
    spl_crd <- split(y, paste(y$lng, y$lat))
    x <- as.matrix(
      do.call(rbind, lapply(spl_crd, function(x) c(x$lng[1], x$lat[1], 1)))
    )
    rasterize(x[, 1:2, drop = FALSE], r, x[, 3])
  })

  all <- sum(rast(rast_list), na.rm = TRUE)
  values(all)[values(all) == 0] <- NA
  all
}

.extract_rank_all <- function(data, res = res, rank = "genus") {
  e <- maps::map(plot = FALSE)
  ext <- ext(e$range)
  r <- rast(ext)
  res(r) <- c(res, res)
  values(r) <- 0
  ranks <- data.frame(
    rank = c("genus", "family", "order", "class", "phylum"),
    accepted_rank = c(
      "genus_no", "family_no", "order_no", "class_no", "phylum_no"
    ),
    rnk = c("gnn", "fmn", "odn", "cln", "phn")
  )

  if (length(data$accepted_rank) != 0) {
    identified <- data[!is.na(data$accepted_rank), ]
    col <- ranks$accepted_rank[ranks$rank == rank]
    ident <- identified[!is.na(identified[, col]), ]
    f <- ident[, col]
    s <- split(ident, f)
  } else if (length(data$rnk) != 0) {
    identified <- data[!is.na(data$rnk), ]
    col <- ranks$rnk[ranks$rank == rank]
    ident <- identified[!is.na(identified[, col]), ]
    f <- ident[, col]
    s <- split(ident, f)
  }

  rast_list <- lapply(s, function(y) {
    s <- split(y, paste(y$lng, y$lat))
    x <- as.matrix(
      do.call(rbind, lapply(s, function(x) c(x$lng[1], x$lat[1], 1)))
    )
    rasterize(x[, 1:2, drop = FALSE], r, x[, 3])
  })

  all <- sum(rast(rast_list), na.rm = TRUE)
  values(all)[values(all) == 0] <- NA
  all
}

#' Plot a raster showing the richness of taxa
#'
#' Creates a `SpatRaster` object and a plot with richness of
#' species, genera, families, etc. per cell.
#'
#' @param data Input data frame. This data frame is the output of the
#'   [pbdb_occurrences()] function using the argument `show =
#'   c("coords", "classext")`. See also Details and Examples.
#' @param rank Taxon rank for which richness is calculated. The
#'   options are: `"species"`, `"genus"`, `"family"`, `"order"`,
#'   `"class"` or `"phylum"`.  The default value is `"species"`.
#' @param do_plot Logical. If `TRUE`, the function produces a plot in
#'   addition to returning a `SpatRaster`.
#' @param res The resolution of the `SpatRaster` object (in decimal
#'   degrees). See [terra::res()].
#' @param col_int The colour of the mainland.
#' @param col_ocean The colour of the ocean.
#' @param col_rich Two or more colours that are used to generate the
#'   colour gradient showing the richness per cell in the map.
#' @param title A title for the plot, to be positioned to the right of
#'   the legend.
#' @param ... Other parameters. See [par()] and [map()].
#' @details The argument `show = c("coords", "classext")` in the
#'   [pbdb_occurrences()] function is required. We recommend the use
#'   of a cairo device ([X11()]) for better visualization of the
#'   graphs. See Examples.
#' @returns A `SpatRaster` object with the richness of the specified
#'   taxon rank per cell. This `SpatRaster` object has the resolution
#'   that was specified in the `res` argument. The default is `res =
#'   5`. Users that wish to work with objects of this type should load
#'   package `terra`.
#' @seealso See [pbdb_occurrences()], [map()], [par()] and [colors()]
#'   help pages.
#' @export
#' @examples \dontrun{
#'   data <- pbdb_occurrences(
#'     limit = 1000, vocab = "pbdb", base_name = "mammalia",
#'     show = c("classext", "coords")
#'   )
#'   X11(width = 13, height = 7.8)
#'   pbdb_map_richness(data, res = 8, rank = "genus")
#'   pbdb_map_richness(data, res = 8, rank = "family")
#'   ## Get the raster object without plotting the map
#'   pbdb_map_richness(data, res = 8, rank = "family", do_plot = FALSE)
#' }
pbdb_map_richness <- function(data,
                              rank = c("species", "genus", "family",
                                       "order", "class", "phylum"),
                              do_plot = TRUE,
                              res = 5,
                              col_int = "white",
                              col_ocean = "black",
                              col_rich = c("light blue", "blue"),
                              title = "Taxonomic richness",
                              ...) {
  rank <- match.arg(rank)

  pbdb_fields <- c(
    "accepted_no", "genus_no", "family_no", "order_no", "class_no", "phylum_no"
  )
  com_fields <- c("tid", "gnn", "fmn", "odn", "cln", "phn")

  pbdb_fields_in_data <- all(pbdb_fields %in% colnames(data))
  com_fields_in_data <- all(com_fields %in% colnames(data))
  coords_in_data <- all(c("lat", "lng") %in% colnames(data))

  if (!coords_in_data || !(pbdb_fields_in_data || com_fields_in_data)) {
    stop(
      "Invalid data input. In the \"pbdb_occurrences\" function, use the ",
      "following argument:\n",
      "    'pbdb_occurrences(..., show = c(\"classext\", \"coords\"))'\n",
      "See \"pbdb_map_richness\" help page."
    )
  }

  if (rank == "species") {
    r <- .extract_rank_species(data, res)
  } else {
    r <- .extract_rank_all(data, res, rank)
  }

  if (do_plot) {
    .plot_raster_rich(
      r, col_rich, col_ocean, col_int, res,
      lg_title = title,
      ...
    )
  }
  r
}
