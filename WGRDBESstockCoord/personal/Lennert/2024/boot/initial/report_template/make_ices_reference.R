## Builds ices_reference.docx, the Word reference document of report.qmd, from the ICES Scientific Reports template for chairs.
##
## The template (ICES_Scientific_Reports_chairs_template_2025.docx) is the one ICES gives to expert group chairs; it is
## linked from https://www.ices.dk/Science/expertgroups/Pages/WG_multiannual_businessreport.aspx. A report written to
## Word with this file as its reference document has the page, the running heads (ICES | <group> <year> | page), the
## fonts, the heading numbering, the captions and the tables of the template.
##
## What is added to the template: the styles Pandoc looks for and the template does not have (Compact, First Paragraph,
## Source Code, Table Caption, Image Caption, Figure, ...), each one resting on a style of the template; and a table style
## "Table" that is the template's own (ICES TableStyle). What is changed: the paragraph style called "Table" of the
## template (the small text of a table) gets another name, because Pandoc takes "Table" for the table style; and the name
## of the group in the running heads.
##
## From the project folder:   Rscript report_template/make_ices_reference.R [template.docx] [group]
## For a newer template: download it, put it here, and run this again.
suppressMessages({library(xml2); library(zip)})

args <- commandArgs(trailingOnly = TRUE)
template <- if (length(args) >= 1) args[[1]] else "report_template/ICES_Scientific_Reports_chairs_template_2025.docx"
group    <- if (length(args) >= 2) args[[2]] else "WGNSSK"                     # the acronym in the running heads
out      <- file.path(normalizePath(dirname(template), winslash = "/"), "ices_reference.docx")     # absolute: zip() works in another folder
stopifnot(file.exists(template))

W  <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
ns <- c(w = W)
dir <- tempfile("ices_ref_"); dir.create(dir)
parts <- zip::zip_list(template)$filename
parts <- parts[!grepl("/$", parts)]                                      # files only: a docx has no folder entries
utils::unzip(template, exdir = dir)

## ---- styles.xml ---------------------------------------------------------------------------------------------------
st_file <- file.path(dir, "word/styles.xml")
st <- read_xml(st_file)
root <- xml_root(st)
style_id <- function(id) xml_find_first(st, sprintf("//w:style[@w:styleId='%s']", id), ns)

## the paragraph style "Table" (the small text of a table) makes room for Pandoc's table style of the same name
old <- style_id("Table")
stopifnot(xml_attr(old, "w:type", ns) == "paragraph")
xml_attr(old, "w:styleId", ns) <- "TableParagraph"
old_name <- xml_find_first(old, "w:name", ns)
xml_attr(old_name, "w:val", ns) <- "Table paragraph"
for (r in xml_find_all(st, "//w:basedOn[@w:val='Table'] | //w:next[@w:val='Table'] | //w:link[@w:val='Table']", ns))
  xml_attr(r, "w:val", ns) <- "TableParagraph"

add_style <- function(xml) {
  if (!inherits(style_id(sub('.*w:styleId="([^"]+)".*', "\\1", xml)), "xml_missing")) return(invisible(NULL))   # the template has it
  xml_add_child(root, read_xml(sub("<w:style ", sprintf("<w:style xmlns:w=\"%s\" ", W), xml, fixed = TRUE)))
}
## the table style of Pandoc is the template's table style
add_style('<w:style w:type="table" w:styleId="Table"><w:name w:val="Table"/><w:basedOn w:val="ICESTableStyle"/><w:uiPriority w:val="59"/><w:qFormat/></w:style>')
## body text and lists
add_style('<w:style w:type="paragraph" w:styleId="FirstParagraph"><w:name w:val="First Paragraph"/><w:basedOn w:val="BodyText"/><w:next w:val="BodyText"/><w:qFormat/></w:style>')
add_style('<w:style w:type="paragraph" w:styleId="Compact"><w:name w:val="Compact"/><w:basedOn w:val="BodyText"/><w:qFormat/><w:pPr><w:spacing w:before="0" w:after="60"/></w:pPr></w:style>')
add_style('<w:style w:type="paragraph" w:styleId="Author"><w:name w:val="Author"/><w:basedOn w:val="Normal"/><w:next w:val="BodyText"/><w:qFormat/><w:pPr><w:keepNext/><w:keepLines/><w:spacing w:before="0" w:after="120"/><w:jc w:val="left"/></w:pPr><w:rPr><w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/><w:sz w:val="24"/></w:rPr></w:style>')
add_style('<w:style w:type="paragraph" w:styleId="Abstract"><w:name w:val="Abstract"/><w:basedOn w:val="Normal"/><w:next w:val="BodyText"/><w:qFormat/></w:style>')
add_style('<w:style w:type="paragraph" w:styleId="FootnoteBlockText"><w:name w:val="Footnote Block Text"/><w:basedOn w:val="FootnoteText"/><w:next w:val="FootnoteText"/></w:style>')
add_style('<w:style w:type="paragraph" w:styleId="DefinitionTerm"><w:name w:val="Definition Term"/><w:basedOn w:val="Normal"/><w:next w:val="Definition"/><w:pPr><w:keepNext/><w:keepLines/><w:spacing w:after="0"/></w:pPr><w:rPr><w:b/></w:rPr></w:style>')
add_style('<w:style w:type="paragraph" w:styleId="Definition"><w:name w:val="Definition"/><w:basedOn w:val="Normal"/></w:style>')
## code
add_style('<w:style w:type="paragraph" w:styleId="SourceCode"><w:name w:val="Source Code"/><w:basedOn w:val="Normal"/><w:link w:val="VerbatimChar"/><w:qFormat/><w:pPr><w:spacing w:before="60" w:after="120"/><w:jc w:val="left"/></w:pPr></w:style>')
add_style('<w:style w:type="character" w:styleId="VerbatimChar"><w:name w:val="Verbatim Char"/><w:basedOn w:val="DefaultParagraphFont"/><w:link w:val="SourceCode"/><w:rPr><w:rFonts w:ascii="Consolas" w:hAnsi="Consolas" w:cs="Consolas"/><w:sz w:val="17"/></w:rPr></w:style>')
add_style('<w:style w:type="character" w:styleId="SectionNumber"><w:name w:val="Section Number"/><w:basedOn w:val="DefaultParagraphFont"/><w:rPr><w:b/></w:rPr></w:style>')
## captions: the table caption above its table, the figure caption below its figure, both the template's caption
add_style('<w:style w:type="paragraph" w:styleId="TableCaption"><w:name w:val="Table Caption"/><w:basedOn w:val="Caption"/><w:next w:val="BodyText"/><w:qFormat/><w:pPr><w:keepNext/></w:pPr></w:style>')
add_style('<w:style w:type="paragraph" w:styleId="ImageCaption"><w:name w:val="Image Caption"/><w:basedOn w:val="Caption"/><w:next w:val="BodyText"/><w:qFormat/><w:pPr><w:keepNext w:val="0"/><w:spacing w:before="60" w:after="240"/></w:pPr></w:style>')
add_style('<w:style w:type="paragraph" w:styleId="Figure"><w:name w:val="Figure"/><w:basedOn w:val="Illustration"/><w:next w:val="BodyText"/><w:pPr><w:keepNext w:val="0"/></w:pPr></w:style>')
add_style('<w:style w:type="paragraph" w:styleId="CaptionedFigure"><w:name w:val="Captioned Figure"/><w:basedOn w:val="Illustration"/><w:next w:val="ImageCaption"/><w:pPr><w:keepNext/></w:pPr></w:style>')
write_xml(st, st_file)

## ---- the name of the group in the running heads (a content control bound to the Subject of the document, which the
## ---- report gives in its YAML; the text is for what does not read the binding) -----------------------------------
for (h in list.files(file.path(dir, "word"), "^header[0-9]+[.]xml$", full.names = TRUE)) {
  x <- read_xml(h)
  for (t in xml_find_all(x, "//w:t[text()='Acronym']", ns)) xml_text(t) <- group
  write_xml(x, h)
}

## ---- back into a .docx, the parts in the order of the template -----------------------------------------------------
unlink(out)
zip::zip(out, files = parts, root = dir, mode = "mirror", include_directories = FALSE)       # mirror: the paths inside the folder are kept
message("written: ", out, " (", file.size(out), " bytes) from ", basename(template))
