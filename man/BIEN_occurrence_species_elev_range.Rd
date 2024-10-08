% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BIEN.R
\name{BIEN_occurrence_species_elev_range}
\alias{BIEN_occurrence_species_elev_range}
\title{Extract occurrence data for specified species within a country and elevation range from the BIEN database}
\usage{
BIEN_occurrence_species_elev_range(
  species,
  country = NULL,
  cultivated = FALSE,
  new.world = NULL,
  all.taxonomy = FALSE,
  native.status = FALSE,
  natives.only = TRUE,
  observation.type = FALSE,
  political.boundaries = FALSE,
  collection.info = FALSE,
  only.geovalid = TRUE,
  ...
)
}
\arguments{
\item{species}{A single species or a vector of species. Genus and species should be separated by a space. The genus should be capitalized (e.g., "Vaccinium floribundum").}

\item{country}{A single country or a vector of countries from which occurrence records should be retrieved. Country names must match those in the BIEN database.}

\item{cultivated}{Logical. Should known cultivated records be included? Default is \code{FALSE}.}

\item{new.world}{Logical or \code{NULL}. Controls whether the results are limited to the New World or Old World. \code{NULL} (default) returns global records, \code{TRUE} returns only New World records, and \code{FALSE} returns only Old World records.}

\item{all.taxonomy}{Logical. Should the returned data include all taxonomic information, both raw and scrubbed? Default is \code{FALSE}.}

\item{native.status}{Logical. Should the returned data include information on introduction status? Default is \code{FALSE}. When \code{TRUE}, additional information about introduction status is included.}

\item{natives.only}{Logical. Should introduced species be excluded from the results? Default is \code{TRUE}.}

\item{observation.type}{Logical. Should the returned data include information on the type of observation (e.g., specimen, plot)? Default is \code{FALSE}.}

\item{political.boundaries}{Logical. Should the returned data include information on political boundaries (e.g., country, state, municipality)? Default is \code{FALSE}.}

\item{collection.info}{Logical. Should additional information about collection and identification be included? Default is \code{FALSE}.}

\item{only.geovalid}{Logical. Should the records be limited to those with validated geographic coordinates? Default is \code{TRUE}.}

\item{...}{Additional arguments passed to internal functions.}
}
\value{
A dataframe containing occurrence records for the specified species, filtered by the specified parameters. If no valid records are found, an empty dataframe is returned.
}
\description{
\code{BIEN_occurrence_species_elev_range} retrieves occurrence records for specified species from the BIEN database, filtered by country and optional additional parameters such as cultivation status, geographic validation, and observation details.
}
\note{
U.S. Forest Inventory and Analysis (FIA) coordinates have been fuzzed and swapped for confidentiality. For more details, see: https://www.fia.fs.fed.us/tools-data/spatial/Policy/index.php.
}
\examples{
\dontrun{
# Retrieve occurrence data for Vaccinium floribundum within specified countries
BIEN_occurrence_species_elev_range(species = "Vaccinium floribundum",
                                   country = "Peru"))
}
}
\seealso{
Other occurrence functions: 
\code{\link{BIEN_occurrence_genus_country}()},
\code{\link{BIEN_occurrence_species_range}()},
\code{\link{BIEN_occurrence_state_range}()}
}
\concept{occurrence functions}
