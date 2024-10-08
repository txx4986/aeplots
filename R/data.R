#' @title Sample adverse event dataset with 2 treatment arms
#'
#' @format A data frame with 370 rows and 9 variables:
#' \describe{
#'    \item{id}{Participant's id}
#'    \item{arm}{Treatment arm: Placebo & Intervention}
#'    \item{ae_pt}{Adverse event preferred term}
#'    \item{aebodsys}{Body system class}
#'    \item{severity}{Severity}
#'    \item{date_rand}{Date of randomisation}
#'    \item{date_ae}{Date of adverse event}
#'    \item{last_visit}{Date of last visit}
#'    \item{variable1}{Variable 1 to be included in treatment effect estimation}
#'    \item{variable2}{Variable 2 to be included in treatment effect estimation}
#' }
"df2"

#' @title Sample adverse event dataset with 3 treatment arms
#'
#' @format A data frame with 370 rows and 7 variables:
#' \describe{
#'    \item{id}{Participant's id}
#'    \item{arm}{Treatment arm: Placebo, Intervention 1, Intervention 2}
#'    \item{ae_pt}{Adverse event preferred term}
#'    \item{aebodsys}{Body system class}
#'    \item{severity}{Severity}
#'    \item{date_rand}{Date of randomisation}
#'    \item{date_ae}{Date of adverse event}
#'    \item{last_visit}{Date of last visit}
#' }
"df3"

#' @title Sample adverse event dataset with 4 treatment arms
#'
#' @format A data frame with 370 rows and 7 variables:
#' \describe{
#'    \item{id}{Participant's id}
#'    \item{arm}{Treatment arm: Placebo, Intervention 1, Intervention 2, Intervention 3}
#'    \item{ae_pt}{Adverse event preferred term}
#'    \item{aebodsys}{Body system class}
#'    \item{severity}{Severity}
#'    \item{date_rand}{Date of randomisation}
#'    \item{date_ae}{Date of adverse event}
#'    \item{last_visit}{Date of last visit}
#' }
"df4"

#' @title Sample laboratory values with continuous outcomes dataset with 2 treatment arms
#'
#' @format A data frame with 2580 rows and 9 variables:
#' \describe{
#'    \item{id}{Participant's id}
#'    \item{arm}{Treatment arm: Placebo & Intervention}
#'    \item{visit}{Timepoint of measurement}
#'    \item{lab_test}{Laboratory test}
#'    \item{base}{Baseline value of measurement}
#'    \item{aval}{Measurement value}
#'    \item{region}{Participant's region}
#'    \item{strat}{Stratification variable}
#'    \item{time}{Time of measurement}
#' }
"lab2"

#' @title Sample laboratory values with continuous outcomes dataset with 3 treatment arms
#'
#' @format A data frame with 2580 rows and 9 variables:
#' \describe{
#'    \item{id}{Participant's id}
#'    \item{arm}{Treatment arm: Placebo, Intervention 1, Intervention 2}
#'    \item{visit}{Timepoint of measurement}
#'    \item{lab_test}{Laboratory test}
#'    \item{base}{Baseline value of measurement}
#'    \item{aval}{Measurement value}
#'    \item{region}{Participant's region}
#'    \item{strat}{Stratification variable}
#'    \item{time}{Time of measurement}
#' }
"lab3"

#' @title Sample laboratory values with continuous outcomes dataset with 3 treatment arms
#'
#' @format A data frame with 2580 rows and 9 variables:
#' \describe{
#'    \item{id}{Participant's id}
#'    \item{arm}{Treatment arm: Placebo, Intervention 1, Intervention 2, Intervention 3}
#'    \item{visit}{Timepoint of measurement}
#'    \item{lab_test}{Laboratory test}
#'    \item{base}{Baseline value of measurement}
#'    \item{aval}{Measurement value}
#'    \item{region}{Participant's region}
#'    \item{strat}{Stratification variable}
#'    \item{time}{Time of measurement}
#' }
"lab4"
