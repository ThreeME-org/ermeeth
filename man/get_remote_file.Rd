% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_from_shared_3me.R
\name{get_remote_file}
\alias{get_remote_file}
\title{Get a file from the threeme share repo}
\usage{
get_remote_file(
  object = NULL,
  root = NULL,
  type = NULL,
  subtype = NULL,
  destination.folder = getwd(),
  repo = "https://raw.githubusercontent.com/ThreeME-org/shared_3me_data/main/resources"
)
}
\arguments{
\item{object}{name of the file with or without extension}

\item{root}{root folder name (lvl 1)}

\item{type}{type folder name (lvl 2)}

\item{subtype}{subtype folder name (lvl 3)}

\item{destination.folder}{path where to download}

\item{repo}{repo url}
}
\value{
download file if successful
}
\description{
Get a file from the threeme share repo
}
