# create a function that interpolates between two coordinate pairs with a sine curve
interpolate_sine <- function(x_start, x_end, y_start, y_end, n) {
  # generate n equally spaced x values between x_start and x_end
  x_values <- seq(x_start, x_end, length.out = n)

  # calculate the amplitude and vertical shift based on y_start and y_end
  amplitude <- (y_end - y_start) / 2
  y_center <- (y_start + y_end) / 2

  # scale x values to cover a half sine wave, ensuring it goes from -1 to 1
  y_values <- y_center + amplitude * sin((x_values - x_start) * (pi / (x_end - x_start)) - (pi / 2))

  # return a dataframe of x and y coordinates
  return(data.frame(x = x_values, y = y_values))
}
