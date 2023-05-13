library("tidyverse")

data <- read_csv("movement_data.csv")
farms_keep <- c(data$departure_cph, data$dest_cph) |> unique() |> sample(250L)

data |>
	filter(departure_cph %in% farms_keep, dest_cph %in% farms_keep) |>
	slice_sample(prop = 0.5) |>
	write_csv("movement_data_halved.csv")
