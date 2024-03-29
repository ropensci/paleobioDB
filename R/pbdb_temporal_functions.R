#' Temporal resolution of fossil data
#'
#' Shows the temporal resolution of the fossil data.
#'
#' @param data Data frame from a query to PaleobioDB as returned by
#'   [pbdb_occurrences()].
#' @param do_plot Logical. If `TRUE`, the function creates a
#'   frequency plot of the data.
#' @returns A list with a summary of the temporal resolution of the
#'   data (min, max, 1st and 3rd quartiles, median and mean), and the
#'   temporal resolution of each fossil record (Ma).
#' @export
#' @examples \dontrun{
#'   data <- pbdb_occurrences(taxon_name = "Canidae", interval = "Quaternary")
#'   pbdb_temporal_resolution(data)
#' }
pbdb_temporal_resolution <- function(data, do_plot = TRUE) {
  if (!all(c("max_ma", "min_ma") %in% names(data)) &&
        !all(c("eag", "lag") %in% names(data))) {
    err_msg <- strwrap(
      paste(
        "No temporal information found in the provided data.frame.",
        "Make sure that it has a \"max_ma\" and a \"min_ma\" column",
        "(or \"eag\" and \"lag\" in compact form)."
      )
    )
    stop(paste(err_msg, collapse = "\n"))
  }

  early_age_col <- if ("eag" %in% names(data)) "eag" else "max_ma"
  late_age_col <- if ("lag" %in% names(data)) "lag" else "min_ma"

  diff <- as.numeric(data[[early_age_col]]) - as.numeric(data[[late_age_col]])
  tr <- list(summary = summary(diff), temporal_resolution = diff)

  if (do_plot) {
    hist(unlist(tr[[2]]), freq = TRUE, col = "#0000FF", border = FALSE,
         xlim = c(max(unlist(tr[[2]]), na.rm = TRUE), 0),
         breaks = 50, xlab = "Temporal resolution of the data (Ma)",
         main = "", col.lab = "grey30", col.axis = "grey30", cex.axis = 0.8)
  }

  tr
}

#' Temporal range of taxa
#'
#' Returns a data frame with the temporal range of the taxa within a
#' selected rank (species, genera, families, etc.), and optionally
#' generates a plot from it.
#'
#' @param data Data frame from a query to PaleobioDB as returned by
#'   [pbdb_occurrences()].  Important: it is required to have
#'   information about the taxonomic classification of the occurrences
#'   in the data frame, to do that set the `show` parameter to
#'   `"class"` or `"classext"` (see Examples).
#' @param rank The taxon rank to be analyzed.  The default value is
#'   `"species"`.
#' @param col Colour of the bars in the plot.
#' @param names Logical indicating whether to include the name of the
#'   taxa in the plot (`TRUE` by default).
#' @param do_plot Logical value indicating whether to produce a plot
#'   (`TRUE` by default).
#' @returns A data frame with the time span of the taxa selected
#'   (species, genera, etc.).
#' @export
#' @examples \dontrun{
#'   canis_quaternary <- pbdb_occurrences(
#'     limit = "all", base_name = "Canis", interval = "Quaternary",
#'     show = c("coords", "classext"), vocab = "pbdb"
#'   )
#'   pbdb_temp_range(canis_quaternary, rank = "species", names = FALSE)
#' }
pbdb_temp_range <- function(data,
                            rank = c("species", "genus", "family",
                                     "order", "class", "phylum"),
                            col = "#0000FF",
                            names = TRUE,
                            do_plot = TRUE) {
  rank <- match.arg(rank)
  temporal_range <- .extract_temporal_range(data, rank)

  if (do_plot) {
    pos <- seq_len(nrow(temporal_range)) - 0.9
    t_range <- cbind(temporal_range, pos)
    # Make right margin wide enough to fit the longest name
    right_margin <- max(nchar(row.names(t_range))) * 0.2
    opar <- par(mar = c(4, 1, 1, right_margin))
    on.exit(par(opar))
    plot(c(min(t_range$min), max(t_range$max)),
      c(0, nrow(t_range)),
      type = "n", axes = FALSE,
      xlab = "Time (Ma)", ylab = "",
      xlim = c(max(t_range$max), min(t_range$min))
    )
    segments(
      x0 = t_range$min,
      y0 = t_range$pos,
      x1 = t_range$max,
      y1 = t_range$pos,
      col = col,
      lwd = 6,
      lend = 2
    )
    axis(1, col = "gray30", cex.axis = 0.8)
    if (names) {
      text(
        x = t_range$min, y = t_range$pos,
        labels = paste("  ", row.names(t_range)),
        adj = c(0, 0.5), cex = 0.5, col = "gray30", xpd = NA
      )
    }
  }

  temporal_range
}

.extract_temporal_range <- function(data, rank) {
  col_abbr <- c(
    accepted_name = "tna", genus = "gnl", family = "fml",
    order = "odl", class = "cll", phylum = "phl"
  )

  if (!("phylum" %in% names(data) || "phl" %in% names(data))) {
    stop(
      paste(
        "Cannot extract temporal range for rank from data without rank",
        "information. Please add 'show = c(\"classext\")' or",
        "'show = c(\"class\")' to your pbdb_occurrences query."
      )
    )
  }

  long_names <- "phylum" %in% names(data)

  rank_col <- rank

  if (rank == "species") {
    # This is the corresponding column for the species name in the
    # data.frame
    rank_col <- "accepted_name"

    # Only select records with an accepted name of species rank
    if (long_names) {
      data <- data[data$accepted_rank == "species", ]
    } else {
      # See <https://paleobiodb.org/data1.2/config.txt?show=ranks>
      data <- data[data$rnk == 3, ]
    }
  }

  rank_col <- if (long_names) rank_col else col_abbr[[rank_col]]
  early_age_col <- if (long_names) "max_ma" else "eag"
  late_age_col <- if (long_names) "min_ma" else "lag"

  early_age <- as.numeric(data[[early_age_col]])
  late_age <- as.numeric(data[[late_age_col]])

  max_sp <- tapply(early_age, as.character(data[[rank_col]]), max)
  min_sp <- tapply(late_age, as.character(data[[rank_col]]), min)

  temporal_range <- data.frame(max_sp, min_sp)
  colnames(temporal_range) <- c("max", "min")
  temporal_range <- temporal_range[with(temporal_range, order(-max, -min)), ]

  temporal_range
}

#' Temporal variation in taxon richness
#'
#' Returns a data frame of temporal variation in taxon richness in the
#' indicated temporal extent and resolution from the provided
#' occurrence data and optionally produces a plot from it.
#'
#' @param data Data frame from a query to PaleobioDB as returned by
#'   [pbdb_occurrences()].  Important: it is required to have
#'   information about the taxonomic classification of the occurrences
#'   in the data frame, to do that set the `show` parameter to
#'   `"class"` or `"classext"` (see Examples).
#' @param rank The taxon rank to be analyzed. The default value is
#'   `"species"`.
#' @param colour Colour of the area of the polygon in the plot.
#' @param bord Colour of the border of the polygon.
#' @param temporal_extent Numeric vector to set the temporal extent
#'   (min, max).
#' @param res Numeric. Sets the duration of the intervals in the
#'   temporal extent.
#' @param ylab A label for the y axis.
#' @param do_plot Logical indicating whether to produce a plot (`TRUE`
#'   by default).
#' @export
#' @returns A data frame with the richness aggregated by the taxon
#'   rank in the specified temporal extent and resolution.
#'
#' @examples \dontrun{
#'   data <- pbdb_occurrences(
#'     limit = "all",
#'     vocab = "pbdb",
#'     base_name = "Canidae",
#'     show = "class"
#'   )
#'   pbdb_richness(data, rank = "species", res = 0.2, temporal_extent = c(0, 3))
#'}
pbdb_richness <- function(data,
                          rank = c("species", "genus", "family",
                                   "order", "class", "phylum"),
                          res = 1,
                          temporal_extent = c(0, 10),
                          colour = "#0000FF30",
                          bord = "#0000FF",
                          ylab = "Richness",
                          do_plot = TRUE) {
  rank <- match.arg(rank)
  temporal_range <- pbdb_temp_range(data = data, rank = rank, do_plot = FALSE)
  te <- temporal_extent
  time <- seq(from = min(te), to = (max(te)), by = res)

  means <- mapply(function(x, y) mean(c(x, y)), time[-length(time)], time[-1])

  intervals <- cbind(time[-length(time)], time[-1])
  # Presence/absence matrix per time step
  a <- apply(
    intervals,
    MARGIN = 1,
    function(interval) {
      temporal_range$max > interval[1] & temporal_range$min <= interval[2]
    },
    simplify = FALSE
  )
  a <- do.call(cbind, a)

  richness <- colSums(a, na.rm = TRUE)
  temporal_intervals <- paste(time[-length(time)], time[-1], sep = "-")
  richness <- data.frame(temporal_intervals, richness)
  if (do_plot) {
    plot.new()
    bottom_margin <- 5 + max(nchar(temporal_intervals)) / 3 + 0.4
    opar <- par(
      mar = c(bottom_margin, 5, 1, 5),
      font.lab = 1, col.lab = "grey20",
      col.axis = "grey50", cex.axis = 0.8
    )
    on.exit(par(opar))
    plot.window(
      xlim = c(max(te), min(te)), xaxs = "i",
      ylim = c(0, (max(richness[, 2])) + (max(richness[, 2]) / 10)), yaxs = "i"
    )

    abline(v = seq(min(te), max(te), by = res), col = "grey90", lwd = 1)
    abline(
      h = seq(
        0, max(richness[, 2]) + (max(richness[, 2]) / 10),
        by = (max(richness[, 2]) / 10)
      ),
      col = "grey90", lwd = 1
    )
    xx <- c(means[1], means, means[length(means)])
    yy <- c(0, richness[, 2], 0)
    polygon(xx, yy, col = colour, border = bord)
    axis(1,
      line = 1, las = 2, labels = temporal_intervals,
      at = means
    )
    axis(2, line = 1, las = 1)
    mtext(
      "Million years before present",
      line = floor(bottom_margin) - 2, adj = 1, side = 1
    )
    mtext(ylab, line = 3.5, adj = 0, side = 2)
  }

  richness
}

#' Appearance of new taxa and extinctions across time
#'
#' Returns a data frame with the appearance of new taxa and their last
#' appearances across time in the provided data and optionally
#' produces a plot from it, showing the new appearances or last
#' appearances.
#'
#' @param data Data frame from a query to PaleobioDB as returned by
#'   [pbdb_occurrences()].  Important: it is required to
#'   show the name of the families, orders, etc. in the data frame, to
#'   do that set: `show = c("classext", "ident")` (see Examples).
#' @param rank The taxon rank to be analyzed. Its default value is
#'   `"species"`.
#' @param temporal_extent Vector to set the temporal extent (min, max)
#' @param res Numeric. Sets the intervals of the temporal extent.
#' @param orig_ext Set to 1 to plot the number new appearances, or to
#'   2 to plot the number of extinctions.
#' @param colour Colour of the area of the polygon in the plot.
#' @param bord Colour of the border of the polygon.
#' @param ylab A label for the y axis.
#' @param do_plot Logical value indicating whether to produce a plot
#'   (`TRUE` by default).
#' @export
#' @returns A data frame with the number of first appearances and
#'   extinctions of the selected taxon rank across time.
#'
#' @examples \dontrun{
#'   canidae <- pbdb_occurrences(
#'     limit = "all", vocab = "pbdb",
#'     base_name = "Canidae", show = "classext"
#'   )
#'
#'   # Plot of the evolutionary rates
#'   pbdb_orig_ext(
#'     canidae,
#'     rank = "genus",
#'     orig_ext = 1,
#'     temporal_extent = c(0, 10), res = 1
#'   )
#'
#'   # Plot of the extinction rates
#'   pbdb_orig_ext(
#'     canidae,
#'     rank = "genus",
#'     orig_ext = 2,
#'     temporal_extent = c(0, 10), res = 1
#'   )
#' }
pbdb_orig_ext <- function(data,
                          rank = c("species", "genus", "family",
                                   "order", "class", "phylum"),
                          temporal_extent,
                          res,
                          orig_ext = 1,
                          colour = "#0000FF30",
                          bord = "#0000FF",
                          ylab = NULL,
                          do_plot = TRUE) {
  rank <- match.arg(rank)
  temporal_range <- pbdb_temp_range(data = data, rank = rank, do_plot = FALSE)
  te <- temporal_extent
  sequence <- seq(from = min(te), to = max(te), by = res)
  intervals <- data.frame(min = sequence[-length(sequence)], max = sequence[-1])
  labels1 <- paste(intervals[, 1], intervals[, 2], sep = "-")
  labels2 <- paste(labels1[-1], labels1[-length(labels1)], sep = " to ")

  res_sp <- apply(intervals, 1, function(int) {
    in_interval <- temporal_range$max > int[1] & temporal_range$min <= int[2]
    row.names(temporal_range)[in_interval]
  })

  change <- mapply(
    function(sp_a, sp_b) {
      new <- length(setdiff(sp_a, sp_b))
      ext <- length(setdiff(sp_b, sp_a))
      c(new, ext)
    },
    res_sp[-length(res_sp)],
    res_sp[-1]
  )

  change <- as.data.frame(t(change))
  names(change) <- c("new", "ext")
  row.names(change) <- labels2

  if (do_plot) {
    ymx <- max(change[, orig_ext])
    xmx <- sequence[length(sequence) - 1]
    xmn <- sequence[2]
    plot.new()

    opar <- par(
      mar = c(5, 5, 2, 5),
      font.lab = 1,
      col.lab = "grey20",
      col.axis = "grey50",
      cex.axis = 0.8
    )
    on.exit(par(opar))

    plot.window(
      xlim = c(xmx, xmn), xaxs = "i",
      ylim = c(0, ymx), yaxs = "i"
    )
    abline(v = seq(xmn, xmx, by = res), col = "grey90", lwd = 1)
    abline(h = seq(0, ymx, by = (ymx / 10)), col = "grey90", lwd = 1)
    xx <- c(xmn, sequence[2:(length(sequence) - 1)], xmx)
    yy <- c(0, change[, orig_ext], 0)
    polygon(xx, yy, col = colour, border = bord)

    axis(1, line = 1, labels = labels2, at = xx[-c(1, length(xx))])
    axis(2, line = 1, las = 1)
    mtext("Million years before present", line = 3, adj = 1, side = 1)
    if (is.null(ylab)) {
      rank_plurals <- c(
        species = "species", genus = "genera", family = "families",
        order = "orders", class = "classes", phylum = "phyla"
      )
      ylab <- paste("Number of", rank_plurals[rank])
    }
    mtext(ylab, line = 3, adj = 0, side = 2)
    title(ifelse(orig_ext == 1, "First appearences", "Last appearences"))
  }

  change
}
