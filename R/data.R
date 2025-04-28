#' Dynamic CTF Clusters Data
#'
#' A dataset containing information about dynamic CTF clusters.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{cluster}{Description of the cluster variable.}
#'   \item{country_code}{Country Code ISO3.}
#'   \item{country_name}{Country Name.}
#'   \item{income_group}{Income WB Classification.}
#'   \item{lending_category}{Lending WB Classification.}
#'   \item{region}{WB Region.}
#'   \item{value}{Indicator Coefficient.}
#'   \item{year}{Year.}
#' }
#' @source CLIAR World Bank Data.
"dynamic_ctf_clusters"

#' Static CTF Clusters Data
#'
#' A dataset containing information about static CTF clusters.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{country_code}{Country Code ISO3.}
#'   \item{country_group}{Country Group Classification.}
#'   \item{country_name}{Country Name.}
#'   \item{income_group}{Income WB Classification.}
#'   \item{lending_category}{Lending WB Classification.}
#'   \item{region}{Region WB Classification.}
#'   \item{value}{Indicator Coefficient.}
#'   \item{var_name}{Indicator Name.}
#'   \item{variable}{Indicator Code.}
#' }
#' @source CLIAR World Bank Data.
"static_ctf_clusters"

#' Budget Execution Data
#'
#' A dataset containing budget execution information, including approved and executed budget values, execution ratios, and classifications by region, sector, and spending type.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{region}{Region WB Classification.}
#'   \item{country_name}{Country Name.}
#'   \item{country_code}{Country Code ISO3.}
#'   \item{year}{Year.}
#'   \item{approved_value}{Approved budget value (numeric).}
#'   \item{budget_line}{Boost Data Classification.}
#'   \item{sector}{Boost Data Aggregation Classification.}
#'   \item{execution_ratio}{Ratio of Budget Execution (numeric, 0 to 1).}
#'   \item{executed_value}{Executed budget value (numeric).}
#'   \item{spending_type}{Spending Classification.}
#' }
#' @source BOOST World Bank Data <https://www.worldbank.org/en/programs/boost-portal/boost-data-lab#1>.
"budget_execution"
