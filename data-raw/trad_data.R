trad_language_base  <- read.csv("tests/translation_file.csv", sep = ";")
Encoding(trad_language_base$fr) <- "latin1"
trad_language_base$fr <- iconv(
trad_language_base$fr,
  "latin1",
  "UTF-8"
)

usethis::use_data(trad_language_base, overwrite = TRUE)
