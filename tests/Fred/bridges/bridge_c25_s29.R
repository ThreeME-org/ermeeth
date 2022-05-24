bridge_sectors <-list( 
  "Agriculture" = c("sagr", "sfor"),
  "Energy intensive industry" = c("sveh", "spap",	"sche", "spla", "sgla"),
  "Other industry" = c("sfoo", "sigo", "scgo"),
  "Construction" = c("scon"),
  "Transport" = c("srai",	"sroa",	"swat",	"sair"),
  "Services" = c("spri", "sfin", "spub"),
  "Fossil based energy" = c("smin", "soil", "sgas", "seoi",	"sega", "seco"),
  "Other energy" = c("senu", "sewi", "seso",	"sehy",	"seot")
)


bridge_commodities <- list(
  "Agriculture" = c("cagr", "cfor"),
  "Energy intensive industry" = c("cveh", "cpap",	"cche", "cpla", "cmet","cgla"),
  "Other industry" = c("cfoo", "cigo", "ccgo"),
  "Construction" = c("ccon"),
  "Transport" = c("crai",	"croa",	"cwat",	"cair"),
  "Services" = c("cpri", "cfin", "cpub"),
  "Fossil based energy" = c("cmin", "ccoa", "ccoi", "cfut", "cgas"),
  "Other energy" = c("cele")
)

