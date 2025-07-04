% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{create_MG_Parms_df}
\alias{create_MG_Parms_df}
\title{create_MG_Parms_df}
\usage{
create_MG_Parms_df(N_growth_patterns = 1, n_areas = 1, GP_per_area = 1)
}
\arguments{
\item{N_growth_patterns}{Number of growth patterns used in the model}

\item{n_areas}{Number of areas in the model}

\item{GP_per_area}{Number of growth patterns that occur in an area}
}
\value{
A data.frame formatted for inputs$ctl$MG_parms for `r4ss`
}
\description{
Function to create the inputs$ctl$MG_parms table.The number and order of
    parameters depends on the number of growth patterns and areas. This
    function creates the table in the correct order, according to the number
    of GP and areas specified. Currently, movement isn't included. This
    needs to be added to the function
}
\examples{
\dontrun{
data("example_ss_files")

example_ss_files$ctl$MG_parms <- create_MG_Parms_df( N_growth_patterns = 1,
                                                     n_areas = 1,
                                                     GP_per_area = 1 )
                                                     }
}
