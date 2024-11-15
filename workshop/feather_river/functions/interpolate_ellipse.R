# create a function that interpolates between two coordinate pairs with an ellipse arc
interpolate_ellipse <- function(x_start,
                                x_end,
                                y_start,
                                y_end,
                                n,
                                quadrant = "ur") {
  # set theta range and center based on the selected quadrant (upper-right,
  # upper-left, bottom-right, or bottom-left)
  if (quadrant == "ur") {
    theta <- seq(0, pi / 2, length.out = n)
    x_center <- min(x_start, x_end)
    y_center <- min(y_start, y_end)
  } else if (quadrant == "ul") {
    theta <- seq(pi / 2, pi, length.out = n)
    x_center <- max(x_start, x_end)
    y_center <- min(y_start, y_end)
  } else if (quadrant == "br") {
    theta <- seq(-pi / 2, 0, length.out = n)
    x_center <- min(x_start, x_end)
    y_center <- max(y_start, y_end)
  } else if (quadrant == "bl") {
    theta <- seq(pi, 3 * pi / 2, length.out = n)
    x_center <- max(x_start, x_end)
    y_center <- max(y_start, y_end)
  } else {
    stop("invalid quadrant, choose from 'ul', 'ur', 'br', or 'bl'")
  }

  # calculate the semi-major and semi-minor axes
  semi_major_axis <- abs(x_end - x_start)
  semi_minor_axis <- abs(y_end - y_start)

  # calculate x and y coordinates along the elliptical arc
  x_values <- x_center + semi_major_axis * cos(theta)
  y_values <- y_center + semi_minor_axis * sin(theta)

  # return a dataframe of x and y coordinates
  return(data.frame(x = x_values, y = y_values))
}
