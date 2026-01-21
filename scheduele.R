suppressPackageStartupMessages(library(tidyverse))
library(gt)

# Session codes
actshort <- c(
  "Lecture" = "L",
  "Computer training" = "CS",
  "Compulsory, Seminar" = "S",
  "Compulsory, Computer training" = "CSM"
)

# Find lecture titles based on lecture slides
lectures <-
  tibble(files = dir(".", "E(L|CS|S)[0-9]*.qmd")) |>
  mutate(
    Session = gsub("\\.qmd", "", files),
    title = map_chr(files, \(x) rmarkdown::yaml_front_matter(x)$title),
    title = gsub("([A-Z0-9]*: )(\\w*)", "\\2", title),
    subtitle = map_chr(files, \(x) rmarkdown::yaml_front_matter(x)$subtitle),
    literature = map_chr(files, \(x) rmarkdown::yaml_front_matter(x)$reading),
    url = sprintf(
      # "[%s](https://sta220.github.io/documentation/%s.html)",
      "[%s](%s.qmd)",
      Session,
      Session
    )
  ) |>
  select(-files)
qs2::qs_save(lectures, "cache/lectures.qs2")


# Based on recent export from TimeEdit
readxl::read_excel("schema.xlsx") |>
  filter(
    grepl("STA220", Course),
    grepl("Erik", Teacher),
    !grepl("Digital Exam|Retake", Activity),
    # Hårdkodar bort session där jag hjälper Anna men inte har så mkt ansvar
    !(grepl("Anna", Teacher) & Activity == "Computer training")
  ) |>
  transmute(
    Week,
    Weekday = factor(Weekday, c("Monday", "Wednesday"), c("Mon", "Wed")),
    # Date = `Begin date`,
    Time = paste(`Begin time`, `End time`, sep = "-"),
    Shared = if_else(grepl("Anna", Teacher), "⌧", ""),
    Activity,
  ) |>
  group_by(Activity) |>
  mutate(Session = paste0("E", actshort[Activity], row_number())) |>
  ungroup() |>
  left_join(lectures, "Session") |>
  mutate(Session = coalesce(url, Session)) |>
  select(-url) |>
  group_by(Week) |>
  mutate(Weekday = if_else(duplicated(Weekday), "", Weekday)) |>
  gt() |>
  tab_footnote(
    footnote = "Shared sessin for the whole course (AGE and EB).",
    locations = cells_column_labels(columns = Shared)
  ) |>
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      rows = grepl("Lecture", Activity),
      columns = Activity
    )
  ) |>
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(
      rows = grepl("Seminar", Activity),
      columns = Activity
    )
  ) |>
  tab_style(
    style = cell_fill(color = "orange"),
    locations = cells_body(
      rows = grepl("Compulsory, Computer training", Activity),
      columns = Activity
    )
  ) |>
  tab_style(
    style = cell_fill(color = "yellow"),
    locations = cells_body(
      rows = grepl("^Computer training", Activity),
      columns = Activity
    )
  ) |>
  sub_missing(missing_text = "") |>
  fmt_markdown(columns = c(Session, literature)) |>
  tab_options(table.layout = "auto") |>
  tab_options(
    container.overflow.x = TRUE,
    table.font.size = px(11)
  ) |>
  opt_css("th, td { white-space: normal; }") |>
  qs2::qs_save("cache/gt_schedule.qs2")
