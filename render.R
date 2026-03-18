# Detta skript används för att rendera och uppdatera hela siten
library(tidyverse)


# Canvas-integration -----------------------------------------------------

library(vvcanvas)

canvas <- canvas_authenticate()
sta220 <- get_courses(canvas) |>
  as_tibble() |>
  filter(startsWith(course_code, "STA220"))


# Närvarolista studenter -------------------------------------------------

students <-
  get_course_students(canvas, sta220$id) |>
  as_tibble() |>
  arrange(sortable_name) |>
  select(sortable_name) |>
  filter(!grepl("student, Test", sortable_name))

# Save list
students |>
  mutate(i = row_number(), sign = "", X = "") |>
  relocate(i) |>
  flextable::flextable() |>
  flextable::width(width = c(1, 7, 7, 2), unit = "cm") |>
  flextable::vline(border = officer::fp_border(color = "black", width = 1)) |>
  flextable::hline(
    part = "body",
    border = officer::fp_border(color = "black", width = 1)
  ) |>
  flextable::save_as_docx(path = "cache/students.docx")


# PDFs to read -----------------------------------------------------------

# Befintliga filer
canvas_pdfs <-
  get_course_files(canvas, sta220$id) |>
  as_tibble() |>
  select(filename, url)

pdfs <-
  qs2::qs_read("cache/lectures.qs2") |>
  select(Session, literature) |>
  mutate(literature = strsplit(literature, "], [", fixed = TRUE)) |>
  unnest(literature) |>
  mutate(
    literature = gsub("(\\[?\\@)(\\w*)(.*)", "\\2", literature, perl = TRUE),
    filename = paste0(literature, ".pdf"),
    upload_from = paste0("articles/", literature, ".pdf")
  ) |>
  filter(
    upload_from %in% dir("articles", ".pdf", full.names = TRUE)
  ) |>
  left_join(canvas_pdfs)

# folder id för Eriks_material
Eriks_material <-
  vvcanvas::get_course_folders(canvas, sta220$id) |>
  filter(name == "Eriks_material") |>
  pluck("id")

# Upload PDF:s which are not already in Canvas
pdfs_to_upload <-
  pdfs |>
  filter(is.na(url)) |>
  pluck("upload_from")

# Om det finns nya filer så uppdateras Canvas med dessa
if (length(pdfs_to_upload) == 0) {
  message("All PDFs found in articles/ are already on canvas!")
} else {
  walk(pdfs_to_upload, \(x) {
    upload_folder_file(
      canvas,
      folder_id = Eriks_material,
      file_name = x
    )
  })

  # Läsinstruktioner
  article_read <-
    readr::read_csv("articles/article_read.csv") |>
    mutate(read = na_if(read, "all"))
  # tabell att presentera
  html_table <-
    pdfs |>
    mutate(
      url = sub("\\?.*$", "", url),
      Session,
      download = glue::glue("<a href='{url}'>{literature}</a>")
    ) |>
    left_join(article_read, c(literature = "article")) |>
    arrange(Session) |>
    mutate(Session = if_else(duplicated(Session), "", Session)) |>
    transmute(Session, download, comment = coalesce(read, "")) |>
    knitr::kable(format = "html", escape = FALSE)

  # HTML body att lägga in

  body <- '
Only some parts from the course book (Nguyen) are required:
<ul>
  <li><strong>Chapter 1:</strong> Whole</li>
  <li><strong>Chapter 2:</strong> Skip the last three sections concerning (hyper)graph databases and RDF</li>
  <li><strong>Chapter 3:</strong> Skip subsections "CPT" and "LOINC" as well as the full section "Using the Unified Medical Language System"</li>
</ul>
<p>
Addional required articles:
' |>
    paste(html_table, sep = "\n\n")

  # Uppdatera sidan
  update_page(
    canvas,
    sta220$id,
    "851450",
    page_params = list(
      title = "Reading for the DISA exam",
      body = body
    )
  )
}


# Rendera och publicera --------------------------------------------------

# publicera till GitHub Pages (gh-pages branch)
# Detta bygger allt även lokalt
system("quarto publish gh-pages --no-prompt")
