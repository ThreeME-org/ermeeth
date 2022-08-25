### Install Dynamo

#' Install Dynamo
#' @description  This function installs the Dynamo executable in a specified location
#' @param dynamo_path directory path where to install Dynamo
#' @param overwrite TRUE to overwrite existing Dynamo install files
#'
#' @return Dynamo installation in the specified location
#' @export
#'
#' @importFrom zip unzip
#'
#' @examples
#' \dontrun{
#'  install_dynamo("../src/compiler")
#' }
install_dynamo <- function(dynamo_path = getwd() , overwrite = TRUE){


  #1. copy the dynamo files
  zip::unzip(system.file("dynamo.zip",package = "ermeeth"),exdir =file.path(dynamo_path,"compiler"),overwrite = overwrite)

  #2 Build other structural files
  if(!dir.exists(file.path(dynamo_path,"data"))){dir.create(file.path(dynamo_path,"data"))}

  if(!dir.exists(file.path(dynamo_path,"model"))){dir.create(file.path(dynamo_path,"model"))}

  #3 Move the shadowfile
  file.copy(from = file.path(dynamo_path,"compiler", "shadowfile_readme.mdl"),
            to = file.path(dynamo_path,"data", "shadowfile_readme.mdl"),
            overwrite = TRUE)
  file.remove(file.path(dynamo_path,"compiler", "shadowfile_readme.mdl"))

}

