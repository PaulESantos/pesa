% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ih__api_institutions.R
\name{ih_api_institutions}
\alias{ih_api_institutions}
\title{Get institutions from The New York Botanical Index Herbariorum API}
\usage{
ih_api_institutions(country)
}
\arguments{
\item{country}{character string with the name of the country.}
}
\value{
A dataframe with the institutions for the given country.
}
\description{
This function allows users to get a dataframe of institutions from The New York Botanical Index Herbariorum API.
}
\examples{
ih_api_institutions("Brazil")

}
