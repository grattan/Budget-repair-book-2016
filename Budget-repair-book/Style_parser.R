library(magrittr)

BOOK <- readLines("Book.tex", encoding = "UTF-8")
BOOK_cleansed <- BOOK

BOOK_cleansed %>%
  gsub("(\\$*[0-9]+)\\s((b|m|(tr))illion)", "\\1~\\2", .) %>%
  # financial years bound
  # Needs negative look-behind
  gsub("(((1[0-9]{3})|(2[0-9]{3})).([0-9]{2}))", "\\\\mbox{\\1}", .) %>%
  gsub("([0-9])\\sper\\scent", "\\1~per~cent", .) %>%
  .
  # gsub("([0-9]+)\\s((to)|(and)|(or))\\s([0-9]+)", "\\1")