# Detta skript används för att rendera och uppdatera hela siten
library(tidyverse)


# Canvas-integration -----------------------------------------------------

library(vvcanvas)

canvas <- canvas_authenticate()
sta220 <- get_courses(canvas) |>
  as_tibble() |>
  filter(startsWith(course_code, "STA220"))


# Närvarolista studenter -------------------------------------------------

get_course_students(canvas, sta220$id) |>
  as_tibble() |>
  arrange(sortable_name) |>
  select(sortable_name) |>
  filter(!grepl("student, Test", sortable_name)) |>
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
    upload_from %in% dir("articles", full.names = TRUE)
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

  # tabell att presentera
  html_table <-
    pdfs |>
    mutate(
      url = sub("\\?.*$", "", url),
      Session,
      download = glue::glue("<a href='{url}'>{literature}</a>")
    ) |>
    arrange(Session) |>
    mutate(Session = if_else(duplicated(Session), "", Session)) |>
    select(Session, download) |>
    knitr::kable(format = "html", escape = FALSE)

  # HTML body att lägga in

  body <- '
Note that some required reading consints of web pages. 
Check the full reading list in the
<a href="https://sta220.github.io/documentation/">course plan</a>
<p>
You may also acces the referenced articles by links provided in the reference list.
If you are not able to do that, static PDF:s are found below.
' |>
    paste(html_table, sep = "\n\n")

  # Uppdatera sidan
  update_page(
    canvas,
    sta220$id,
    "851450",
    page_params = list(
      title = "PDF:s to read",
      body = body
    )
  )
}


# Rendera och publicera --------------------------------------------------

# Rendera siten lokalt
quarto::quarto_render(".")

# publicera till GitHub Pages (gh-pages branch)
system("quarto publish gh-pages --no-prompt")
