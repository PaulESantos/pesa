% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_simmilarity.R
\name{group_similarity}
\alias{group_similarity}
\title{Group Similarity Function}
\usage{
group_similarity(vect, dist = 0.4)
}
\arguments{
\item{vect}{A character vector containing the elements to be grouped.}

\item{dist}{The threshold distance below which elements are considered similar. Default is 0.4.}
}
\value{
A tibble with columns "index" (the index of the original vector) and "group" (the group index).
}
\description{
This function groups elements in a vector based on their similarity using the Jaro-Winkler distance metric.
}
\examples{
group_similarity(c("apple", "apples", "banana", "orange", "applle"), dist = 0.3)

}
