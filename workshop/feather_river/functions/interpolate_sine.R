# create a function that interpolates between two coordinate pairs using a
# power-transformed sine function
interpolate_sine <- function(x_start, x_end, y_start, y_end, n, power = 2) {
  # generate n equally spaced x values between x_start and x_end
  x_values <- seq(x_start, x_end, length.out = n)

  # calculate the amplitude and vertical shift based on y_start and y_end
  amplitude <- (y_end - y_start) / 2
  y_center <- (y_start + y_end) / 2

  # scale x values to cover a half sine wave, ensuring it goes from -pi/2 to pi/2
  normalized_x <- (x_values - x_start) / (x_end - x_start) * pi - (pi / 2)

  # modify the sine curve to make it steeper in the middle while preserving symmetry
  y_values <- y_center + amplitude * sign(sin(normalized_x)) * abs(sin(normalized_x))^power

  # return a dataframe of x and y coordinates
  return(data.frame(x = x_values, y = y_values))
}
