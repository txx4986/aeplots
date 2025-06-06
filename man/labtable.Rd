% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labtable.R
\name{labtable}
\alias{labtable}
\title{Table of summary statistics and model estimates for laboratory values with continuous outcomes}
\usage{
labtable(
  data,
  control,
  intervention_levels,
  lab_test = "lab_test",
  visit = "visit",
  id = "id",
  arm = "arm",
  aval = "aval",
  treatment_effect_estimate = TRUE,
  model_formula = "aval ~ arm + (1|id)",
  mean = TRUE,
  median = TRUE,
  n_missing = TRUE,
  control_name = NULL,
  intervention_names = NULL,
  IQR_format = c("iqr", "percentile"),
  mean_dp = 1,
  SD_dp = 1,
  median_dp = 1,
  IQR_dp = 1,
  estimate_sf = 3,
  CI_sf = 3,
  save_image_path = NULL,
  save_docx_path = NULL
)
}
\arguments{
\item{data}{dataframe with lab_test, visit, id, arm and aval columns (optional: variables to be included in the model)}

\item{control}{factor level of control arm}

\item{intervention_levels}{vector of factor levels for interventions}

\item{lab_test}{name of lab_test column}

\item{visit}{name of visit column}

\item{id}{name of id column}

\item{arm}{name of arm column}

\item{aval}{name of aval column}

\item{treatment_effect_estimate}{a logical value whether to include treatment effect estimate and 95\% CI column}

\item{model_formula}{formula for linear mixed model to estimate treatment effect and 95\% CI}

\item{mean}{a logical value whether to include mean and SD row in summary table}

\item{median}{a logical value whether to include median and IQR row in summary table}

\item{n_missing}{a logical value whether to include number of missing n row in summary table}

\item{control_name}{name of control arm}

\item{intervention_names}{vector of names for interventions}

\item{IQR_format}{format of IQR: to be presented as single "iqr" value or as 25th and 75th "percentile"}

\item{mean_dp}{number of decimal places for mean}

\item{SD_dp}{number of decimal places for standard deviation}

\item{median_dp}{number of decimal places for median}

\item{IQR_dp}{number of decimal places for IQR}

\item{estimate_sf}{number of significant figures for treatment effect estimate}

\item{CI_sf}{number of significant figures for 95\% CI}

\item{save_image_path}{file path to save table as image}

\item{save_docx_path}{file path to save table as docx}
}
\value{
flextable of laboratory values summary for baseline and each post-baseline timepoint by treatment arms
}
\description{
\code{labtable} is used to plot a table that summarises laboratory values with continuous outcomes for baseline and each post-baseline timepoint by treatment arms using the following summary statistics:
\itemize{
\item \strong{Mean (SD)}
\item \strong{Median (IQR)}
\item \strong{Number of missing}
\item \strong{Treatment effect estimate (Intervention vs Control, 95\% CI)}
}
}
\section{Treatment Effect Estimate}{

The treatment effect and its 95\% confidence interval is estimated via a linear mixed model with laboratory measurement as response.
The formula of the model can be specified through the \code{model_formula} argument.
The default model is \code{aval ~ arm + (1|id)}.
}

\examples{
labtable(lab2, control="Placebo", intervention_levels=c("Intervention"))
}
