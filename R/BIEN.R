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
######################################################################\
###########################

#'Extract a list of all species in the BIEN database.
#'
#'BIEN_list_all produces a list of all species in the BIEN database.
#' @param country A single country or a vector of country.
#' @param elevation_min Low elevation
#' @param elevation_max Max elevation
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing a list of all species in the BIEN database.
#' @examples \dontrun{
#' species_list<-BIEN_list_species()}
#' @family list functions
#' @keywords internal
BIEN_list_species_range<-function(country, elevation_min, elevation_max, ...){
  .is_char(country)

  if(is.null(country))
  {stop("Please supply either a country or 2-digit ISO code")}

  query <- paste("SELECT scrubbed_species_binomial, latitude, longitude, elevation_m
                 FROM view_full_occurrence_individual",
                 "WHERE elevation_m BETWEEN ", elevation_min," AND ",
                 elevation_max,"
                 AND country in (", paste(shQuote(country, type = "sh"),
                                   collapse = ', '), ")",
                 ";")

  return(.BIEN_sql(query, ...))

}


#####################################################################
#######################
#'Extract species occurrence records by state and elevation range
#'BIEN_occurrence_state_range extracts occurrences records for the specified state(s).
#'Extract species occurrence records by state.
#'
#'BIEN_occurrence_state extracts occurrences records for the specified state(s).
#' @param state A state or vector of states (or other primary political divisions, e.g. provinces).
#' @param country A single country or vector of countries.
#' @param state.code A single state/province code, or a vector of states/province codes.
#' @param country.code A single country (or other primary administrative boundary) code or a vector of country codes equal in length to the vector of states/province codes.
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
#' @note Political division (or political division code) spelling needs to be exact and case-sensitive, see \code{\link{BIEN_metadata_list_political_names}} for a list of political divisions and associated codes.
#' @note This function requires you supply either 1) a single country with one or more states, or 2) vectors of equal length for each political level.
#' @return Dataframe containing occurrence records for the specified states/provinces.
#' @examples \dontrun{
#' BIEN_occurrence_state_range("Peru", "Cusco", 3000, 3300)}
#' @family occurrence functions
#' @export
BIEN_occurrence_state_range<-function(country=NULL,
                                state=NULL,
                                elevation_min,
                                elevation_max,
                                country.code=NULL,
                                state.code=NULL,
                                cultivated=FALSE,
                                new.world=NULL,
                                all.taxonomy=FALSE,
                                native.status=FALSE,
                                natives.only=TRUE,
                                observation.type=FALSE,
                                political.boundaries=FALSE,
                                collection.info=F, ...){
  .is_char(country)
  .is_char(state)
  .is_char(country.code)
  .is_char(state.code)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_log(native.status)
  .is_log(observation.type)
  .is_log(political.boundaries)
  .is_log(natives.only)
  .is_log(collection.info)

  #set conditions for query
  cultivated_<-.cultivated_check(cultivated)
  newworld_<-.newworld_check(new.world)
  taxonomy_<-.taxonomy_check(all.taxonomy)
  native_<-.native_check(native.status)
  observation_<-.observation_check(observation.type)
  political_<-.political_check(political.boundaries)
  natives_<-.natives_check(natives.only)
  collection_<-.collection_check(collection.info)

  if(is.null(country.code) & is.null(state.code)){

    ##state where
    if(length(country)==1){
      sql_where <- paste(" WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ")
                         AND state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ")
                         AND scrubbed_species_binomial IS NOT NULL")
    }else{

      if(length(country)==length(state)){

        sql_where<-"WHERE ("

        for(i in 1:length(country)){

          condition_i<- paste("(country = ", paste(shQuote(country[i], type = "sh"),collapse = ', '), " AND state_province = ", paste(shQuote(state[i], type = "sh"),collapse = ', '), ")")
          if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
          sql_where<-paste(sql_where,condition_i)

        }#for i

        sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")

      }else{
        stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")

      }

    }#if length(country>1)
  }else{

    ##state where
    if(length(country.code)==1){
      sql_where <- paste(" WHERE country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), "))
                         AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code, type = "sh"),collapse = ', '), "))
                         AND scrubbed_species_binomial IS NOT NULL")
    }else{

      if(length(country.code)==length(state.code)){

        sql_where<-"WHERE ("

        for(i in 1:length(country.code)){

          condition_i<- paste("country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code[i], type = "sh"),collapse = ', '), "))
                              AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code[i], type = "sh"),collapse = ', '), "))")
          if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
          sql_where<-paste(sql_where,condition_i)

        }#for i

        sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")

      }else{
        stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")

      }

    }#if length(country>1)

  }

  # set the query
  query <- paste("SELECT scrubbed_species_binomial" ,taxonomy_$select,political_$select, ", country, state_province, latitude, longitude, elevation_m, date_collected,datasource,
                  dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                 collection_$select,cultivated_$select,newworld_$select,native_$select,observation_$select,"
                 FROM view_full_occurrence_individual ",
                 sql_where,
                 "AND elevation_m BETWEEN ", elevation_min," AND ",
                 elevation_max,
                 cultivated_$query,newworld_$query,natives_$query,"
                  AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1
                  AND (georef_protocol is NULL OR georef_protocol<>'county centroid') AND (is_centroid IS NULL OR is_centroid=0)
                  AND observation_type IN ('plot','specimen','literature','checklist')
                  AND scrubbed_species_binomial IS NOT NULL ;")


  return(.BIEN_sql(query, ...))

}
######################################################################
#####################


#'List political divisions and associated geonames codes.
#'
#'BIEN_metadata_list_political_names downloads country, state, and county names and associated codes used by BIEN.
#' @param ... Additional arguments passed to internal functions.
#' @return A dataframe containing political division names and their associated codes.
#' @note Political names and codes follow http://www.geonames.org/
#' @examples \dontrun{
#' BIEN_metadata_list_political_names()}
#' @family metadata functions
#' @keywords internal
BIEN_metadata_list_political_names<-function(...){

  query<-'SELECT country,country_iso, state_province, state_province_ascii,state_province_code AS "state_code",
  county_parish,county_parish_ascii,county_parish_code AS "county_code" FROM county_parish ;'

  .BIEN_sql(query, ...)

}
######################################################################
# 04-22-2021  nueva fucnion
BIEN_list_county_paul <-function(country=NULL,state=NULL,county=NULL,country.code=NULL,state.code=NULL,county.code=NULL,cultivated=FALSE,new.world=NULL, ...){
  .is_char(country.code)
  .is_char(state.code)
  .is_char(county.code)
  .is_char(country)
  .is_char(state)
  .is_char(county)
  .is_log(cultivated)
  .is_log_or_null(new.world)

  # set base query components
  sql_select <-  paste("SELECT DISTINCT country, state_province, county, family, scrubbed_species_binomial ")
  sql_from <- paste(" FROM species_by_political_division ")

  if(is.null(country.code) & is.null(state.code) & is.null(county.code)){

    #sql where
    if(length(country)==1 & length(state)==1){
      sql_where <- paste(" WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ")
                         AND state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ")
                         AND county in (", paste(shQuote(county, type = "sh"),collapse = ', '), ")
                         AND scrubbed_species_binomial IS NOT NULL")
    }else{

      if(length(country)==length(state) & length(country)==length(county)){

        sql_where<-"WHERE ("

        for(i in 1:length(country)){

          condition_i<- paste("(country = ", paste(shQuote(country[i], type = "sh"),collapse = ', '), "
                              AND state_province = ", paste(shQuote(state[i], type = "sh"),collapse = ', '), "
                              AND county = ", paste(shQuote(county[i], type = "sh"),collapse = ', '), ")
                              ")

          if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
          sql_where<-paste(sql_where,condition_i)

        }#for i

        sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")

      }else{
        stop("If supplying more than one country and/or state the function requires matching vectors of countries, states and counties.")

      }



    }#if length(country>1)
  }else{

    #sql where
    if(length(country.code)==1 & length(state.code)==1){
      sql_where <- paste(" WHERE country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), "))
                         AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code, type = "sh"),collapse = ', '), "))
                         AND county in (SELECT county_parish_ascii FROM county_parish WHERE admin2code in (", paste(shQuote(county.code, type = "sh"),collapse = ', '), "))
                         AND scrubbed_species_binomial IS NOT NULL")
    }else{

      if(length(country)==length(state) & length(country)==length(county)){

        sql_where<-"WHERE ("

        for(i in 1:length(country)){

          condition_i<- paste("(country = (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), "))
                              AND state_province = (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code, type = "sh"),collapse = ', '), "))
                              AND county = (SELECT county_parish_ascii FROM county_parish WHERE admin2code in (", paste(shQuote(county.code, type = "sh"),collapse = ', '), "))" )

          if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
          sql_where<-paste(sql_where,condition_i)

        }#for i

        sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")

      }else{
        stop("If supplying more than one country and/or state the function requires matching vectors of countries, states and counties.")

      }



    }#if length(country>1)


  }


  sql_order_by <- paste(" ORDER BY scrubbed_species_binomial ")

  # adjust for optional parameters
  if(!cultivated){
    sql_where <- paste(sql_where, " AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) ")
  }else{
    sql_select  <- paste(sql_select, ",is_cultivated_observation,is_cultivated_in_region")
  }

  #if(!new.world){
  #  sql_select <- paste(sql_select,",is_new_world")
  #}else{
  #  sql_where <- paste(sql_where, "AND is_new_world = 1 ")
  #}

  newworld_<-.newworld_check(new.world)

  # form the final query
  query <- paste(sql_select,newworld_$select, sql_from, sql_where,newworld_$query, sql_order_by, " ;")


  ## form the final query
  #query <- paste(sql_select, sql_from, sql_where, sql_order_by, " ;")

  return(.BIEN_sql(query, ...))

}
