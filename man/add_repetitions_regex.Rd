% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_rep.R
\name{add_repetitions_regex}
\alias{add_repetitions_regex}
\title{Agregar repeticiones basadas en expresiones regulares}
\usage{
add_repetitions_regex(
  data,
  column_name,
  regex_pattern,
  n_repetitions,
  new_column_name
)
}
\arguments{
\item{data}{El dataframe original.}

\item{column_name}{El nombre de la columna donde se realizará la búsqueda.}

\item{regex_pattern}{El patrón de expresión regular a buscar en la columna especificada.}

\item{n_repetitions}{El número de repeticiones que se agregarán después de cada coincidencia.}

\item{new_column_name}{El nombre de la nueva columna donde se agregarán las repeticiones.}
}
\value{
El dataframe resultante con la nueva columna agregada.
}
\description{
Esta función busca valores que coinciden con un patrón de expresión regular en una columna de un dataframe
y agrega repeticiones de esos valores en una nueva columna.
}
\examples{
df <-  tibble::tibble(
  var1 = c("a_1", "d", "d", "a_2", "d", "f", "a_3", "g", "u")
)
df_result <- add_repetitions_regex(df, "var1", "^(a|f)", 2, "new_var")
print(df_result)
}
