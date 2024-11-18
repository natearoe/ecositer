.onLoad <- function(libname, pkgname){
  if(packageVersion("soilDB") <= "2.8.5") {
    stop("ecositer requires at least version 2.8.6 of soilDB. ",
         "Please reinstall ecositer to access the development version of soilDB - \n",
         "remotes::install_github('natearoe/ecositer', dependencies = TRUE)"
         )
  }
}
