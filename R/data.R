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

#’ Indicator Cluster Distance
#’
#’ A table of “distance to benchmark” for each institutional indicator, broken
#’ out by country and broad indicator cluster (“family”).
#’
#’ @format A data frame with 7,762 rows and 9 variables:
#’ \describe{
#’   \item{country_name}{Country name (character).}
#’   \item{country_code}{ISO3 country code (character).}
#’   \item{income_group}{World Bank income group classification (character).}
#’   \item{lending_category}{World Bank lending category (character).}
#’   \item{region}{World Bank region (character).}
#’   \item{variable}{Internal indicator code (character).}
#’   \item{var_name}{Human-readable indicator name (character).}
#’   \item{family_name}{Cluster or “family” that this indicator belongs to (character).}
#’   \item{ctf_distance}{Numeric distance of that country’s indicator value to the CTF benchmark (double).}
#’ }
#' @source CLIAR World Bank Data.
"indicator_cluster_distance"

#' Budget Execution Data
#'
#' A dataset containing budget execution information, including approved and executed budget values, execution ratios, and classifications by region, sector, and spending type.
#'
#' @format A data frame with the following columns:
#’ \describe{
#’   \item{region}{World Bank region code (character).}
#’   \item{country_name}{Country name (character).}
#’   \item{country_code}{ISO3 country code (character).}
#’   \item{lending_category}{World Bank lending category (character).}
#’   \item{year}{Fiscal year (numeric).}
#’   \item{approved_value}{Approved budget (numeric).}
#’   \item{budget_line}{Detailed budget line description (character).}
#’   \item{sector}{Sector name (character).}
#’   \item{executed_value}{Actual executed amount (numeric).}
#’   \item{execution_ratio}{Executed/approved × 100 (numeric).}
#’   \item{spending_type}{High-level spending type (character).}
#’ }
#' @source BOOST World Bank Data <https://www.worldbank.org/en/programs/boost-portal/boost-data-lab#1>.
"budget_execution"

#' @title GovTech Maturity Index
#' @description The 2020 GovTech dataset contained data/evidence collected from government websites using remotely measurable indicators (due to the COVID-19 pandemic) mostly reflecting de jure practices.
#' @format A data frame with 198 rows and 21 variables:
#' \describe{
#'   \item{\code{code}}{character Country code}
#'   \item{\code{economy}}{character Economy name}
#'   \item{\code{level}}{character Income group level}
#'   \item{\code{population}}{double Population}
#'   \item{\code{gni}}{double Gross National Income}
#'   \item{\code{gnipc}}{double Gross National Income per capita}
#'   \item{\code{region}}{character Region}
#'   \item{\code{tmis_sta}}{double Tax MIS Status}
#'   \item{\code{cust_sta}}{double Customs MIS Status}
#'   \item{\code{hrm_sta}}{double HRMIS Status}
#'   \item{\code{payr_sta}}{double Payroll MIS Status}
#'   \item{\code{e_prc_sta}}{double e-Procurement Status}
#'   \item{\code{dms_sta}}{double Debt Management System Status}
#'   \item{\code{pims_sta}}{double Public Investment MIS Status}
#'   \item{\code{tmis_yr}}{double Tax MIS year established}
#'   \item{\code{cust_yr}}{double Customs MIS year established}
#'   \item{\code{hrm_yr}}{double HRMIS year established}
#'   \item{\code{payr_yr}}{double Payroll MIS year established}
#'   \item{\code{e_prc_yr}}{double e-Procurement year established}
#'   \item{\code{dms_yr}}{double Debt Management System year established}
#'   \item{\code{pims_yr}}{double Public Investment MIS year established}
#'}
#' @source https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0065450/WBG_GovTech%20Dataset_Dec2020.xlsx
"dgss"
