x <- readLines("b5_portrait_book.tex")

# Separate by spaces or command argument 
x.space <- 
  strsplit(x, split = "(\\s+)|\\{|\\[|[0-9]")

# exclude
x.command_words <- lapply(x.space, function(x) x[grepl("\\\\", x)])

x.commands <- lapply(x.command_words, function(x) gsub("^.*\\\\(([A-Za-z]+)|([^A-Za-z])).*$", "\\1", x))

unique(unlist(x.commands))

tex_environments <- 
  x[grepl("\\\\begin\\{([A-Za-z]+)\\}", x)]

unique(gsub("^.*\\\\begin\\{([A-Za-z]+)\\}.*$", "\\1", tex_environments))
