# an ev spara notes under extra-fälten i Zotero för mer detaljerade läsanvisningar. Avvaktar dock tills vidare.

bib <-
  RefManageR::ReadBib("references.bib") |>
  as_tibble(rownames = "ref") |>
  select(ref, note)
