aux <- readLines("./Budget-repair-book/b5_portrait_book.tex")
super <- readLines("./Budget-repair-book/Super-tax-targeting/b5-portrait-Super-tax-targeting-as-chapter.tex")
gst <- readLines("./Budget-repair-book/GST-reform-package/b5-portrait-GST-reform-package-as-chapter.tex")
cgt <- readLines("./Budget-repair-book/Hot-property-CG-and-NG/b5-portrait-CGT_and_neg_gearing_final-as-chapter.tex")
entire <- c(aux, super, gst, cgt)
box_lines <- entire[grepl("begin{bigboxC*", entire, fixed = TRUE)]
boxe_labels <- gsub("^.*(box:[^}]+).*$", "\\1", box_lines)

writeClipboard(paste0(boxe_labels, sep = ",", collapse = ""))
