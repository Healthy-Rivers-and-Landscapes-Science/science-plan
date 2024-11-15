get_clean_cdec_flow <- function(station,
                                sensor,
                                dur_code,
                                start_date,
                                end_date) {

  flow <- CDECRetrieve::cdec_query(
    station = station,
    sensor = sensor,
    dur_code = dur_code,
    start_date = start_date,
    end_date = end_date
  )

  flow <- flow[!is.na(flow$datetime), ]
  flow <- flow[!is.na(flow$parameter_value), ]

  flow$datetime <- lubridate::as_date(flow$datetime)

  year_part <- lubridate::year(flow$datetime) - (lubridate::month(flow$datetime) < 10)
  flow$water_year <- paste(year_part, year_part + 1, sep = "-")

  flow$station_water_year <- paste(flow$location_id, flow$water_year, sep = "_")

  flow$water_year_start <- as.Date(paste0(year_part, "-10-01"))

  flow$water_year_day <- as.integer(flow$datetime - flow$water_year_start + 1)

  return(flow)
}
