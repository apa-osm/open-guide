# render the book as HTML and open in a browser
xfun::in_dir("book", bookdown::render_book("index.Rmd", "bookdown::bs4_book"))
browseURL("docs/index.html")

# copy data directory to docs
R.utils::copyDirectory(
  from = "book/images",
  to = "docs/images",
  overwrite = TRUE,
  recursive = TRUE)
