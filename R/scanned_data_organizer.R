#' Organize scanned datasheets into folders
#'
#' `scanned_data_organizer` is a tool to take scans of field data and organize it
#' into folders. The process starts with scanning your datasheets. All of
#' your datasheets can be scanned at once. The result of which will be a
#' single pdf file containing all of your datasheets. This function will
#' separate that pdf file into the individual sites that you sampled and
#' name those new pdfs using the site id. A folder will be created for each
#' site id, within which the datasheet will be stored. In addition, an
#' all_sites folder will be created where all of the pdfs will be stored
#' together.
#'
#' @param scanned_data_path the location of your scanned datasheets
#' @param directory the directory where you want the results stored
#' @param year_state_fips the year, state, and fips code for your site id
#' (e.g., "2023AK185")
#' @param site_number the site number for example in site id 2023AK185401, the
#' site_number is 401. site_number must be manually entered as a vector unless
#' you can extract the site_number from GPS point names or some other record
#' of the sites your sampled. If you cannot extract the site_number, you will
#' enter it following this format:
#' my_site_number <- c(401, 402, 405, 409, 410, 411)
#'
#' @return the result is a directory with folders for each site, within which
#' is a pdf of your datasheet named with the site name. A folder containing
#' all sites will also be created.
#' @export
#'
#' @examples
#' my_site_number <- c(100, 101, 102, 103, 123, 124, 125)
#' scanned_data_organizer(scanned_data_path = "S:/NRCS/XEROX SCANS/DOC057.pdf",
#' directory = "C:/Users/Nathan.Roe/Documents/Alaska2023/Willow_project",
#' year_state_fips = "2023AK185",
#' site_number = my_site_number)
#'
#' @importFrom pdftools pdf_length pdf_subset
#'
scanned_data_organizer <- function(scanned_data_path, directory, year_state_fips, site_number){
  # create the site name (e.g., 2023AK185401)
  site_names <- paste0(year_state_fips, site_number)

  # create the sequences that determine which pages to include in a pdf. you
  # will use this sequence to break your scan of all datasheets into separate
  # two page pdfs (front and back of datasheet), one pdf for each site.
  num <- pdftools::pdf_length(scanned_data_path)
  st <- seq(1, num, 2)
  en <- pmin(st + 1, num)

  # create the names for the directories
  path_names <- paste0(directory, "/", site_names)

  # create the names for the files
  file_names <- paste0(path_names, "/", site_names, ".pdf")

  # path to store all site datasheets
  all_sites <- paste0(directory, "/all_sites")
  if(dir.exists(all_sites) == FALSE) {
    dir.create(all_sites)
  } else {
    print("Directory already exists!")
  }

  # run a loop that will create a directory for each plot, if one
  # does not already exist
  for(i in seq_along(path_names)){
    if(dir.exists(path_names[i]) == FALSE) {
      dir.create(path_names[i])
    } else {
      print("Directory already exists!")
    }
  }

  for(i in seq_along(file_names)){
    pdftools::pdf_subset(scanned_data_path, pages = st[i]:en[i],
               output = file_names[i])
  }

  for(i in seq_along(site_names)){
    pdftools::pdf_subset(scanned_data_path, pages = st[i]:en[i],
               output = paste0(all_sites, "/", site_names[i], ".pdf"))
  }

}
