#' Accessing example data
#'
#' @return the location of example data
#' @export
#'
#' @examples
#' access_example_data()
#'
#'
access_example_data <- function(){

     configDir <- tools::R_user_dir("ecositer", which="cache")
   if (!dir.exists(configDir)) {
     dir.create(configDir, recursive=TRUE)
   }
   configFile <- file.path(configDir, "CA792_veg_data.sqlite")
   if (!file.exists(configFile)) {
     destFile <- paste0(configDir, "/CA792_veg_data.zip")

     tryCatch(download.file(url = "https://github.com/natearoe/ecositer_data/blob/main/CA792_veg_data.zip?raw=TRUE",
                            destfile = destFile,
                            mode = "wb"),
              error=function(e) {
                message('An Error Occurred')
                print(e)
              },
              #if a warning occurs, tell me the warning
              warning=function(w) {
                message('A Warning Occurred')
                print(w)
                return(NA)
              })

     tryCatch(unzip(zipfile = destFile,
                    exdir = configDir),
              error=function(e) {
                message('An Error Occurred')
                print(e)
              },
              #if a warning occurs, tell me the warning
              warning=function(w) {
                message('A Warning Occurred')
                print(w)
                return(NA)
              }
    )
   }
  return(configFile)
}
