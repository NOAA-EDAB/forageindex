## allsplit
cogoutfall <- readRDS(here::here(
  "SOEpyindex/1982-2022/allagg_fall_500_lennosst_ALLsplit_biascorrect/cogout.rds"
))
cogoutspring <- readRDS(here::here(
  "SOEpyindex/1982-2022/allagg_spring_500_lennosst_ALLsplit_biascorrect/cogout.rds"
))

cogdat <- as.data.frame(cogoutfall$COG_Table) |>
  dplyr::mutate(season = "Fall") |>
  dplyr::bind_rows(
    as.data.frame(cogoutspring$COG_Table) |>
      dplyr::mutate(season = "Spring")
  ) |>
  dplyr::rename(
    `Center of Gravity` = COG_hat,
    `Center of Gravity SE` = SE,
    Time = Year
  ) |>
  dplyr::mutate(direction = ifelse(m == 1, "Eastward", "Northward")) |>
  tidyr::pivot_longer(cols = c(`Center of Gravity`, `Center of Gravity SE`)) |>
  dplyr::mutate(
    Var = paste(season, direction, "Forage Fish", name),
    source = "ALLsplit"
  ) |>
  dplyr::rename(Value = value) |>
  dplyr::select(Time, Var, Value, source)

fallforagecog <- readRDS(here::here("SOEpyindex/fallforagecog.rds"))
springforagecog <- readRDS(here::here("SOEpyindex/springforagecog.rds"))

foragecog <- dplyr::bind_rows(fallforagecog, springforagecog) |>
  dplyr::mutate(source = "Foragefish") |>
  dplyr::select(Time, Var, Value, source)

dplyr::bind_rows(cogdat, foragecog) |>
  dplyr::mutate(
    season = ifelse(grepl("Fall", Var), "Fall", "Spring"),
    direction = ifelse(grepl("Eastward", Var), "Eastward", "Northward")
  ) |>
  dplyr::filter(!stringr::str_detect(Var, "SE")) |>
  ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, color = source)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw() +
  ggplot2::facet_grid(
    cols = ggplot2::vars(season),
    rows = ggplot2::vars(direction),
    scales = "free_y"
  )

ggplot2::ggsave(
  here::here("test_SOEpyindex/compare_cog.png"),
  width = 6,
  height = 4
)
