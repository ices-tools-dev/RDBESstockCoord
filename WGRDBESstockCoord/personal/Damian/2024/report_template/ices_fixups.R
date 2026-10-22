## ices_fix_docx(): what is left to do to the Word report after Quarto, so that it follows the ICES template.
##
## Quarto puts every figure and table that has a label (fig-..., tbl-...) in a table of one cell, with a caption that has
## its own spacing and a doubled paragraph-properties element. Here the cell is taken away, the caption gets the caption
## style of the template (the table caption above its table, the figure caption below its figure) and the text of the
## cells of the tables gets the table styles of the template: Table - Heading for the first row, Table - Text for the
## others, and Table - Numbers (right) for a column Quarto aligned to the right.
##
## Used by report.R after the document is rendered:   source("report_template/ices_fixups.R"); ices_fix_docx("report.docx")
suppressMessages({library(xml2); library(zip)})

ices_fix_docx <- function(docx) {
  stopifnot(file.exists(docx))
  docx <- normalizePath(docx, winslash = "/")
  W  <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  ns <- c(w = W)
  dir <- tempfile("ices_fix_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  parts <- zip::zip_list(docx)$filename
  parts <- parts[!grepl("/$", parts)]
  utils::unzip(docx, exdir = dir)
  doc_file <- file.path(dir, "word/document.xml")
  doc  <- read_xml(doc_file)
  body <- xml_find_first(doc, "/w:document/w:body", ns)

  style_of <- function(p) xml_attr(xml_find_first(p, "w:pPr/w:pStyle", ns), "w:val", ns)
  set_style <- function(p, id) {                              # one w:pPr, its w:pStyle first, no direct alignment or spacing
    ppr <- xml_find_all(p, "w:pPr", ns)
    keep <- ppr[vapply(ppr, function(x) !inherits(xml_find_first(x, "w:pStyle", ns), "xml_missing"), NA)]
    if (!length(ppr)) { xml_add_child(p, "w:pPr", .where = 0L); ppr <- xml_find_all(p, "w:pPr", ns); keep <- list() }
    pp <- if (length(keep)) keep[[1]] else ppr[[1]]
    for (x in ppr) if (!identical(xml_path(x), xml_path(pp))) xml_remove(x)
    for (x in xml_find_all(pp, "w:jc | w:spacing | w:pStyle", ns)) xml_remove(x)
    xml_add_child(pp, "w:pStyle", .where = 0L)
    xml_set_attr(xml_find_first(pp, "w:pStyle", ns), "w:val", id, ns)
    invisible(p)
  }

  ## 1. the one-cell tables around the figures and the tables that have a caption
  n_unwrapped <- 0L
  for (tb in xml_find_all(body, "w:tbl", ns)) {
    rows <- xml_find_all(tb, "w:tr", ns); cells <- xml_find_all(rows, "w:tc", ns)
    if (length(rows) != 1L || length(cells) != 1L) next
    cap <- xml_find_all(cells[[1]], "w:p[w:pPr/w:pStyle[@w:val='ImageCaption']]", ns)
    if (!length(cap)) next
    for (kid in xml_children(cells[[1]])) if (xml_name(kid) != "tcPr") xml_add_sibling(tb, kid, .where = "before", .copy = TRUE)
    xml_remove(tb)
    n_unwrapped <- n_unwrapped + 1L
  }

  ## 2. the captions and the figures
  n_cap <- 0L
  for (p in xml_find_all(body, "w:p[w:pPr/w:pStyle[@w:val='ImageCaption']]", ns)) {
    set_style(p, if (grepl("^Table[\\s\\x{a0}]\\d", xml_text(p), perl = TRUE)) "TableCaption" else "ImageCaption")   # Quarto writes a no-break space after "Table"
    n_cap <- n_cap + 1L
  }
  for (p in xml_find_all(body, "w:p[w:r/w:drawing]", ns)) if (identical(style_of(p), "Compact")) set_style(p, "CaptionedFigure")

  ## 3. the text of the tables
  n_tab <- 0L
  for (tb in xml_find_all(body, "w:tbl", ns)) {
    n_tab <- n_tab + 1L
    for (r in seq_along(rows <- xml_find_all(tb, "w:tr", ns))) {
      header <- r == 1L && !inherits(xml_find_first(rows[[r]], "w:trPr/w:tblHeader", ns), "xml_missing")
      for (p in xml_find_all(rows[[r]], "w:tc/w:p", ns)) {
        right <- identical(xml_attr(xml_find_first(p, "w:pPr/w:jc", ns), "w:val", ns), "right")     # the first alignment is the one of the column
        set_style(p, if (header) { if (right) "Table-HeadingRight" else "Table-Heading" } else { if (right) "Table-Numbers" else "Table-Text" })
      }
    }
  }

  ## 4. written back, the parts in the order they had
  write_xml(doc, doc_file)
  unlink(docx)
  zip::zip(docx, files = parts, root = dir, mode = "mirror", include_directories = FALSE)
  message("ICES template: ", n_unwrapped, " layout tables taken away, ", n_cap, " captions, ", n_tab, " tables styled in ", basename(docx))
  invisible(c(unwrapped = n_unwrapped, captions = n_cap, tables = n_tab))
}
