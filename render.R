# Detta skript används för att rendera och uppdatera hela sitden

library(tidyverse)
library(vvcanvas)

# Rendera siten lokalt
quarto::quarto_render(".")

# publicera till GitHub Pages (gh-pages branch)
system("quarto publish gh-pages --no-prompt")


# Publicera läslista -----------------------------------------------------

library(vvcanvas)

canvas <- canvas_authenticate()
sta220 <- get_courses(canvas) |>
  as_tibble() |>
  filter(startsWith(course_code, "STA220"))

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


# Upload PDF:s which are not already in Canvas
pdfs_to_upload <-
  pdfs |>
  filter(is.na(url)) |>
  pluck("upload_from")
if (length(pdfs_to_upload) > 0) {
  walk(pdfs_to_upload, \(x) {
    upload_folder_file(
      canvas,
      folder_id = Eriks_material,
      file_name = x
    )
  })
}

# tabell att presentera
html_table <-
  pdfs |>
  mutate(
    url = sub("\\?.*$", "", url),
    Session,
    download = glue::glue("<a href='{url}'>{literature}</a>")
  ) |>
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
