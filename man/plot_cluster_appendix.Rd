% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pigoanalytics.R
\name{plot_cluster_appendix}
\alias{plot_cluster_appendix}
\title{Plot Cluster Appendix Figures (optional)}
\usage{
plot_cluster_appendix(data, cluster_ctf, year_label)
}
\arguments{
\item{data}{A data frame or tibble containing at least these columns:
\itemize{
\item \code{family_name}
\item \code{var_name}
\item \code{ctf_distance}
\item \code{region}
\item \code{country_name}
}}

\item{cluster_ctf}{A string matching the \code{family_name} of the cluster to plot.}

\item{year_label}{A character label (e.g. \code{"2010 vs 2020"}) to include in the plot title.}
}
\value{
Invisibly returns \code{NULL}. Side effect: writes one PNG per indicator to disk.
}
\description{
Creates and saves a horizontal bar‐segment plot of \code{ctf_distance} for each indicator
(\code{var_name}) in the specified cluster (\code{family_name}), faceted by region. Outputs
PNG files into \verb{figures/appendix/<cluster>/}.
}
