image.scale <- function(
    z, zlim, col = heat.colors(12),
    breaks, horiz = TRUE, ylim = NULL, xlim = NULL, ...) {
  if (!missing(breaks)) {
    if (length(breaks) != (length(col) + 1)) {
      stop("must have one more break than colour")
    }
  }
  if (missing(breaks) & !missing(zlim)) {
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 1))
  }
  if (missing(breaks) & missing(zlim)) {
    zlim <- range(z, na.rm = TRUE)
    zlim[2] <- zlim[2] + c(zlim[2] - zlim[1]) * (1E-3) # adds a bit to the range in both directions
    zlim[1] <- zlim[1] - c(zlim[2] - zlim[1]) * (1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 1))
  }
  poly <- vector(mode = "list", length(col))
  for (i in seq(poly)) {
    poly[[i]] <- c(breaks[i], breaks[i + 1], breaks[i + 1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if (horiz) {
    YLIM <- c(0, 1)
    XLIM <- range(breaks)
  }
  if (!horiz) {
    YLIM <- range(breaks)
    XLIM <- c(0, 1)
  }
  if (missing(xlim)) xlim <- XLIM
  if (missing(ylim)) ylim <- YLIM
  plot(1, 1, t = "n", ylim = ylim, xlim = xlim, xaxt = xaxt, yaxt = yaxt, xaxs = "i", yaxs = "i", ...)
  for (i in seq(poly)) {
    if (horiz) {
      polygon(poly[[i]], c(0, 0, 1, 1), col = col[i], border = NA)
    }
    if (!horiz) {
      polygon(c(0, 0, 1, 1), poly[[i]], col = col[i], border = NA)
    }
  }
}

color.bar <- function(colores, min, max = -min, nticks = length(colores) + 1,
                      ro = 1, ticks = round(seq(min, max, len = nticks), ro), xm = NULL, ym = NULL, xw = NULL, yw = NULL, horiz = TRUE, xpos = 1) {
  xaxp <- par()$usr[1:2]
  yaxp <- par()$usr[3:4]
  nc <- length(colores)
  if (horiz & xpos == 1) pos <- 1
  if (horiz & xpos == 0) pos <- 3
  if (!horiz & xpos == 1) pos <- 2
  if (!horiz & xpos == 0) pos <- 4
  if (horiz) {
    if (is.null(xw)) {
      xw <- 0.4 * (xaxp[2] - xaxp[1])
    }
    if (is.null(yw)) {
      yw <- 0.025 * (yaxp[2] - yaxp[1])
    }
    if (is.null(xm)) {
      xm <- (xaxp[2] + xaxp[1]) / 2
    }
    if (is.null(ym)) {
      ym <- yaxp[1] + xpos * (yaxp[2] - yaxp[1]) - 2 * (xpos - .5) * yw
    }
    scale <- nc / (2 * xw)
    for (i in 1:nc) {
      x <- xm - xw + (i - 1) / scale
      rect(x, ym - yw, x + 1 / scale, ym + yw, col = colores[i], border = NA)
      text(x, ym - 2 * (xpos - .5) * yw, ticks[i], pos = pos, offset = 0.5)
    }
    text(xm + xw, ym - 2 * (xpos - .5) * yw, ticks[nc + 1], pos = pos, offset = 0.5)
  } else {
    if (is.null(xw)) {
      xw <- 0.025 * (xaxp[2] - xaxp[1])
    }
    if (is.null(yw)) {
      yw <- 0.4 * (yaxp[2] - yaxp[1])
    }
    if (is.null(xm)) {
      xm <- xaxp[1] + xpos * (xaxp[2] - xaxp[1]) - 2 * (xpos - .5) * xw
    }
    if (is.null(ym)) {
      ym <- (yaxp[2] + yaxp[1]) / 2
    }
    scale <- nc / (2 * yw)
    for (i in 1:nc) {
      y <- ym - yw + (i - 1) / scale
      rect(xm - xw, y, xm + xw, y + 1 / scale, col = colores[i], border = NA)
      text(xm - 2 * (xpos - .5) * xw, y, ticks[i], pos = pos, offset = 0.5)
    }
    text(xm - 2 * (xpos - .5) * xw, ym + yw, ticks[nc + 1], pos = pos, offset = 0.5)
  }
}
