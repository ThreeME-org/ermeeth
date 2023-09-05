trad_language_base  <- read.csv("tests/translation_file.csv", sep = ";")

Encoding(trad_language_base$fr) <- "UTF-8"

trad_language_base$fr <- iconv(
trad_language_base$fr,
  "UTF-8",
  "UTF-8"
)
Encoding(trad_language_base$es) <- "UTF-8"

trad_language_base$es <- iconv(
  trad_language_base$es,
  "UTF-8",
  "UTF-8"
)

usethis::use_data(trad_language_base, overwrite = TRUE)
