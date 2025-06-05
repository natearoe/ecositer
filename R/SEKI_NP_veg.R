#' Example vegetation data set
#'
#' @description
#' Example data set from the Sequoia and King's Canyon National Parks Soil Survey conducted by the Natural Resources Conservation Service (Soil Survey Area 792).
#' All data is obtained from the National Soils Information System (NASIS) database.
#'
#'@format A data frame with 17444 rows and 44 variables:
#'\describe{
#'  \item{siteiid}{Primary key for Site table record}
#'  \item{usiteid}{Alpha-numeric code for Site}
#'  \item{siteobsiid}{Primary key for Site Observation table record}
#'  \item{vegplotid}{Alpha-numeric code for Vegetation Plot, often matching User Site ID}
#'  \item{vegplotiid}{Primary key for Vegetation Plot table record}
#'  \item{primarydatacollector}{Primary data collector for vegetation data}
#'  \item{vegdataorigin}{Origin of vegetation data (e.g., directly entered, imported, converted from other vegetation database)}
#'  \item{ecositeid}{Foreign key for Ecological Site in the Site Ecological Site History table}
#'  \item{ecositenm}{Name of Ecological Site}
#'  \item{ecostateid}{Foreign key for Ecological Site State in the Site Ecological Site History table}
#'  \item{ecostatename}{Name of Ecological Site State}
#'  \item{commphaseid}{Foreign key for Ecological Site Community Phase in the Site Ecological Site History table}
#'  \item{commphasename}{Name of Ecological Site Community Phase}
#'  \item{cancovtotalpct}{The estimated percent of the ground that is shaded by vegetation canopy at midday.}
#'  \item{cancovtotalclass}{The estimated percent of the ground that is shaded by vegetation canopy at midday, expressed as a class.}
#'  \item{overstorycancontotalpct}{The canopy cover percent of all species in the overstory stratum.}
#'  \item{overstorycancovtotalclass}{The canopy cover percent of all species in the overstory stratum, expressed as a class.}
#'  \item{plantsym}{Foreign key for Plant Symbol in the Plant table}
#'  \item{plantsciname}{The full genus and species name as listed in The PLANTS Database, USDA-NRCS, National Plant Data Center.}
#'  \item{plantnatvernm}{The most generally accepted common name of a plant.}
#'  \item{akstratumcoverclasspct}{The Alaska stratum cover class cover in percent.}
#'  \item{speciescancovpct}{The estimated canopy cover percent of a species.}
#'  \item{speciescomppct}{The percentage of a particular species in the plant, including tree, community within the plot.}
#'  \item{speciestraceamtflag}{A yes/no indicator that a 'trace' amount of a species exists. A 'trace' is defined as 'less than a measureable amount'.}
#'  \item{vegetationstratalevel}{Identifies which strata or level of the vegetation canopy the particular referenced plant is a part of.}
#'  \item{akstratumcoverclass}{General life form and horizontal layer (cover class) added to accommodate the STRATUMcv class attribute in AK SITE.}
#'  \item{plantheightcllowerlimit}{The lower height limit of the plant height class being described.}
#'  \item{plantheightclupperlimit}{The upper height limit of the plant height class being described.}
#'  \item{planttypegroup}{The designation of a plant type group being inventoried by lifeform.}
#'  \item{horizdatnm}{Datum Name.}
#'  \item{utmzone}{UTM Zone.}
#'  \item{utmeasting}{UTM Easting.}
#'  \item{utmnorthing}{UTM Northing.}
#'  \item{latdegrees}{Latitude degrees.}
#'  \item{latminutes}{Latitude minutes.}
#'  \item{latseconds}{Latitude seconds.}
#'  \item{latdir}{Latitude position north or south of the equator.}
#'  \item{longdegrees}{Longitude degrees.}
#'  \item{longminutes}{Longitude minutes.}
#'  \item{longseconds}{Longitude seconds.}
#'  \item{longdir}{Longitude east or west of Greenwich.}
#'  \item{latstDD}{Standardized latitude value in decimal degrees, in geographic coordinate system, WGS84 datum..}
#'  \item{longstDD}{Standardized longitude value in decimal degrees, in geographic coordinate system, WGS84 datum.}
#'  }
#'
#' @usage data(SEKI_NP_Veg)

"SEKI_NP_veg"
