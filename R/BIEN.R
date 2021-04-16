#'Extract occurrence data from BIEN for specified genera and country
#'
#' BIEN_occurrence_genus_country() downloads occurrence records for specific
#' genus/genera  and country from the BIEN database.
#' @param genus A single genus, or a vector of genera. Genera should be capitalized.
#' @param country A single country or a vector of country.
#' @param cultivated Return known cultivated records as well?  Default is FALSE.
#' @param new.world NULL (The default) returns global records, TRUE returns only New World, and FALSE only Old World.
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param natives.only Exclude detected introduced species?  Default is TRUE.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param collection.info Return additional information about collection and identification? The default value is FALSE.
#' @param ... Additional arguments passed to internal functions.
#' @note US FIA coordinates have been fuzzed and swapped, for more details see: https://www.fia.fs.fed.us/tools-data/spatial/Policy/index.php
#' @return Dataframe containing occurrence records for the specified genera.
#' @examples \dontrun{
#' BIEN_occurrence_genus_country("Vaccinium", "Peru")}
#' @family occurrence functions
#' @export
BIEN_occurrence_genus_country <-function(genus,
                                         country=NULL,
                                         cultivated=FALSE,
                                         new.world=NULL,
                                         all.taxonomy=FALSE,
                                         native.status=FALSE,
                                         natives.only=TRUE,
                                         observation.type=FALSE,
                                         political.boundaries=FALSE,
                                         collection.info=F, ...){
  .is_char(genus)
  .is_char(country)
  .is_log(cultivated)
  .is_log(all.taxonomy)
  .is_log_or_null(new.world)
  .is_log(native.status)
  .is_log(observation.type)
  .is_log(political.boundaries)
  .is_log(natives.only)
  .is_log(collection.info)
  if(is.null(country))
  {stop("Please supply either a country or 2-digit ISO code")}
  #set conditions for query

  cultivated_<-.cultivated_check(cultivated)
  newworld_<-.newworld_check(new.world)
  taxonomy_<-.taxonomy_check(all.taxonomy)
  native_<-.native_check(native.status)
  observation_<-.observation_check(observation.type)
  political_<-.political_check(political.boundaries)
  natives_<-.natives_check(natives.only)
  collection_<-.collection_check(collection.info)

  # set the query
  query <-
    paste("SELECT scrubbed_genus, scrubbed_species_binomial",
          taxonomy_$select,
          native_$select,
          political_$select,
          " ,latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
          collection_$select,
          cultivated_$select,
          newworld_$select,
          observation_$select,
          "
          FROM view_full_occurrence_individual
          WHERE scrubbed_genus in (",
          paste(shQuote(genus,
                        type = "sh"),
                collapse = ', '), ")",
          cultivated_$query,
          newworld_$query,natives_$query,"
          AND country in (", paste(shQuote(country, type = "sh"),
                                   collapse = ', '), ")",
          cultivated_$query,newworld_$query,natives_$query,"
            AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1
            AND (georef_protocol is NULL OR georef_protocol<>'county centroid') AND (is_centroid IS NULL OR is_centroid=0)
            AND observation_type IN ('plot','specimen','literature','checklist')
            AND scrubbed_species_binomial IS NOT NULL ;")

  return(.BIEN_sql(query, ...))

}

######################################################################
#####################

#'Extract occurrence data for specified species from BIEN
#'
#'BIEN_occurrence_species_range downloads occurrence records for specific species from the BIEN database.
#' @param species A single species, or a vector of species.  Genus and species should be separated by a space. Genus should be capitalized.
#' @param only.geovalid Should the returned records be limited to those with validated coordinates?  Default is TRUE
#' @param elevation_min Low elevation
#' @param elevation_max Max elevation
#' @param cultivated Return known cultivated records as well?  Default is FALSE.
#' @param new.world NULL (The default) returns global records, TRUE returns only New World, and FALSE only Old World.
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param natives.only Exclude detected introduced species?  Default is TRUE.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param collection.info Return additional information about collection and identification? The default value is FALSE.
#' @param ... Additional arguments passed to internal functions.
#' @note US FIA coordinates have been fuzzed and swapped, for more details see: https://www.fia.fs.fed.us/tools-data/spatial/Policy/index.php
#' @return Dataframe containing occurrence records for the specified species.
#' @examples \dontrun{
#' BIEN_occurrence_species_range("Vaccinium floribundum", 3000, 3300)}
#' @family occurrence functions
#' @export
BIEN_occurrence_species_range<-function(species,
                                  elevation_min,
                                  elevation_max,
                                  cultivated=FALSE,
                                  new.world=NULL,
                                  all.taxonomy=FALSE,
                                  native.status=FALSE,
                                  natives.only=TRUE,
                                  observation.type=FALSE,
                                  political.boundaries=FALSE,
                                  collection.info=F,
                                  only.geovalid=T, ...){

  #Test input
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(species)
  .is_log(native.status)
  .is_log(observation.type)
  .is_log(political.boundaries)
  .is_log(natives.only)
  .is_log(collection.info)
  .is_log(only.geovalid)

  #set conditions for query
  cultivated_<-.cultivated_check(cultivated)
  newworld_<-.newworld_check(new.world)
  taxonomy_<-.taxonomy_check(all.taxonomy)
  native_<-.native_check(native.status)
  observation_<-.observation_check(observation.type)
  political_<-.political_check(political.boundaries)
  natives_<-.natives_check(natives.only)
  collection_<-.collection_check(collection.info)
  geovalid_<-.geovalid_check(only.geovalid)


  # set the query
  query <- paste("SELECT scrubbed_species_binomial",
                 taxonomy_$select,
                 native_$select,
                 political_$select,
                 " ,latitude, longitude, elevation_m, date_collected,
                 datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                 collection_$select,
                 cultivated_$select,
                 newworld_$select,
                 observation_$select,
                 geovalid_$select,
                 "FROM view_full_occurrence_individual
                 WHERE scrubbed_species_binomial in (",
                 paste(shQuote(species, type = "sh"),collapse = ', '),
                 ")","
                 AND elevation_m BETWEEN ", elevation_min," AND ",
                 elevation_max,
                 cultivated_$query,
                 newworld_$query,
                 natives_$query,
                 observation_$query,
                 geovalid_$query,
                 "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi')
                 AND (georef_protocol is NULL OR georef_protocol<>'county centroid')
                 AND (is_centroid IS NULL OR is_centroid=0)
                 AND scrubbed_species_binomial IS NOT NULL
                 ORDER BY scrubbed_species_binomial ;")

  return(.BIEN_sql(query, ...))

}
