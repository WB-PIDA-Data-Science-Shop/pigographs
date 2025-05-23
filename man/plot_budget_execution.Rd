% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pigoanalytics.R
\name{plot_budget_execution}
\alias{plot_budget_execution}
\title{Plot Budget Execution Ratios by Region}
\usage{
plot_budget_execution(data, exyear, govsector, budgetline)
}
\arguments{
\item{data}{A data.frame or tibble containing at least:
\code{year} (numeric), \code{sector}, \code{budget_line},
\code{region}, \code{execution_ratio}, \code{country_code}.}

\item{exyear}{Year to filter (numeric or a string representing an integer).}

\item{govsector}{One value from the \code{sector} column (e.g. \code{"Health"}).}

\item{budgetline}{One value from the \code{budget_line} column
(e.g. \code{"Spending: Capital expenditures in health"}).}
}
\value{
A ggplot2 object.
}
\description{
Filters a BOOST budget execution data frame by year, sector, and budget_line,
then returns a flipped execution‐ratio plot with reference bands.
}
