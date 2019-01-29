system("pdftotext -layout b5_palatino_portrait_book.pdf")
hyphens <- pdf_as_text <- readLines("b5_palatino_portrait_book.txt", warn = FALSE)
library(data.table)
library(dplyr)

paraz <- list()
paragraph <- 1
n_hyphens <- 0
for (l in seq_along(hyphens)){
  if (hyphens[[l]] == ""){
    paraz[[paragraph]] <- list(n_hyphens = n_hyphens, context = hyphens[[l - 1]], ell = l)
    paragraph <- paragraph + 1
    n_hyphens <- 0L
  } else {
    if (grepl("-$", hyphens[[l]])){
      n_hyphens <- n_hyphens + 1L
    }
  }
}

paraz_dt <- rbindlist(paraz)

#
Diana_log <- readLines("b5_palatino_portrait_book.log")
Diana_source <- readLines("b5_palatino_portrait_book.tex")
Super_source <- readLines("./Super-tax-targeting/b5-palatino-portrait-Super-tax-targeting-as-chapter.tex")
GST_source <- readLines("./GST-reform-package/b5-palatino-portrait-GST-reform-package-as-chapter.tex")
CGT_source <- readLines("./Hot-property-CG-and-NG/b5-portrait-palatino-CGT_and_neg_gearing_final-as-chapter.tex")
# Detect overfulls outside figures

figure_lines <- function(the_source){
  #Lines(not just figures) which may be ignored
  in_figure <- logical(length(the_source))
  for (line in seq_along(the_source)){
    if (line > 1){
      if (grepl("begin{figure}", the_source[line], fixed = TRUE)){
        in_figure[line] <- TRUE
      } else {
        if (grepl("end{figure}", the_source[line], fixed = TRUE)){
          in_figure[line] <- FALSE
        } else {
          in_figure[line] <- in_figure[line - 1L]
        }
      }
    }
  }
  which(in_figure)
}

all_figure_lines <- 
  rbindlist(list(
    data.table(src = "base", lines = figure_lines(Diana_source)),
    data.table(src = "Super", lines = figure_lines(Super_source)),
    data.table(src = "GST", lines = figure_lines(GST_source)),
    data.table(src = "CGT", lines = figure_lines(CGT_source))
  ))

section_lines <- function(the_source){
  grep("^\\\\section", the_source)
}


overfulls <- 
  data.table(points = as.numeric(gsub("^[^0-9]*([0-9][0-9]*\\.[0-9]+)pt.*$", 
                                      "\\1",
                                      Diana_log[grepl("overfull .hbox", Diana_log, ignore.case = TRUE)])), 
             lines = gsub("^.*lines\\s([0-9]+).*$", 
                          "\\1", 
                          Diana_log[grepl("overfull .hbox", Diana_log, ignore.case = TRUE)]))
overfulls %>%
  mutate(lines = as.integer(as.character(lines))) %>%
  mutate(srcno = cumsum(lead(lines) > lead(lines, n = 2L, default = 0))) %>% 
  setkey(srcno) %>%
  merge(data.table(srcno = 0:5, src = c("base", "Super", "GST", "CGT", "back", "back2"), key = "srcno")) %>%
  filter(points != 10.95, points != 5.47499, between(points, 1.5, 30)) %>% # chapters
  filter(between(points, 1, 10), 
         !(src == "base" & lines %in% figure_lines(Diana_source)),
         !(src == "Super" & lines %in% figure_lines(Super_source)), 
         !(src == "GST" & lines %in% figure_lines(GST_source)),
         !(src == "CGT" & lines %in% figure_lines(CGT_source))) ->
  true_overfulls

# consecutive hypens or words
last_word <- function(text){
  stringr::str_extract(string = text, pattern = "[A-Za-z0-9\\-]+$")
}

first_word <- function(text){
  stringr::str_extract(string = text, pattern = "^[A-Za-z0-9]+")
}

consecutives <- 
  setdiff(union(which(first_word(pdf_as_text) == first_word(lag(pdf_as_text)) & nchar(first_word(pdf_as_text)) > 0), 
                which(last_word(pdf_as_text) == last_word(lag(pdf_as_text)))), 
          # acceptable consecutives
          c(1351,4832,5767, 6536, 6652, 7280, 11319))


