#' Species list
#'
#' @param SS logical - whether to use access data in NASIS selected set
#' @param dsn path to local NASIS database
#'
#' @return a species list from your NASIS vegplots with modern taxonomies, growth habit, native status, and higher level taxa from PLANTS database
#' @export
#'
#' @examples
#' species_list(SS = TRUE)
species_list <- function(SS = TRUE, static_location = NULL){

  if(SS == FALSE & is.null(static_location))
    stop('If SS = FALSE, static location must be provided.')

  if(SS == TRUE & !is.null(static_location))
    stop('Choose between SS and static_location. Arguments currently suggest accessing data from both sources.')

  if(SS == TRUE){
    # pull veg data from selected set
    my_ss <- soilDB::fetchVegdata(SS = TRUE)$vegplotspecies |> dplyr::select(plantsym, plantsciname) |> unique()
  } else {
    my_ss <- soilDB::fetchVegdata(SS = FALSE,
                          dsn = static_location)$vegplotspecies |> dplyr::select(plantsym, plantsciname) |> unique()

  }

  # access USDA PLANTS species list
  message("Downloading USDA PLANTS species list...")
  plants <- data.table::fread("https://raw.githubusercontent.com/natearoe/ecositer_data/main/PLANTS_2024_official.csv.gz")

 # id old taxa
 my_syns <-  plants |> dplyr::filter(Synonym.Symbol %in% my_ss$plantsym) |>
    dplyr::select(Accepted.Symbol, Synonym.Symbol)

 # complile modern taxa assoc. with old taxa
 repent <- plants |> dplyr::filter(Symbol %in% my_syns$Accepted.Symbol) |>
   dplyr::select(Accepted.Symbol, Scientific.Name)

 # join modern taxa with old
 forgiven <- my_syns |> dplyr::left_join(repent, dplyr::join_by(Accepted.Symbol == Accepted.Symbol))

 # join modern taxa in using old taxa
 pious_ss <- my_ss |> dplyr::left_join(forgiven, dplyr::join_by(plantsym == Synonym.Symbol))

 pious_ss$Accepted.Symbol <- ifelse(!is.na(pious_ss$Accepted.Symbol),
                                    pious_ss$Accepted.Symbol,
                                    pious_ss$plantsym)

 pious_ss$Accepted.Sci.Name <- ifelse(!is.na(pious_ss$Scientific.Name),
                                      pious_ss$Scientific.Name,
                                      pious_ss$plantsciname)

 # join in desired PLANTS database info using modern taxa symbol
  pious_ss |> dplyr::left_join(plants |> dplyr::select(Symbol, Common.Name, Family, Order, Growth.Habit, Native.Status),
                            dplyr::join_by(Accepted.Symbol == Symbol)) |>
   dplyr::rename(Current.Symbol = plantsym,
                 Current.Sci.Name = plantsciname) |>
   dplyr::select(Current.Symbol, Current.Sci.Name, Accepted.Symbol, Accepted.Sci.Name,
                 Common.Name, Growth.Habit, Native.Status, Family, Order) |>
   dplyr::arrange(dplyr::desc(Growth.Habit), Current.Sci.Name)


}
