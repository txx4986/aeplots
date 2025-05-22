#' Table of summary statistics and model estimates for laboratory values with continuous outcomes
#'
#' @description
#' `labtable` is used to plot a table that summarises laboratory values with continuous outcomes for baseline and each post-baseline timepoint by treatment arms using the following summary statistics:
#'
#' - **Mean (SD)**
#' - **Median (IQR)**
#' - **Number of missing**
#' - **Treatment effect estimate (95% CI)**
#'
#' @section Treatment Effect Estimate:
#' The treatment effect and its 95% confidence interval is estimated via a linear mixed model with laboratory measurement as response.
#' The formula of the model can be specified through the `model_formula` argument.
#' The default model is `aval ~ arm + (1|id)`.
#'
#' @param data dataframe with lab_test, visit, id, arm and aval columns (optional: variables to be included in the model)
#' @param control factor level of control arm
#' @param intervention_levels vector of factor levels for interventions
#' @param lab_test name of lab_test column
#' @param visit name of visit column
#' @param id name of id column
#' @param arm name of arm column
#' @param aval name of aval column
#' @param treatment_effect_estimate a logical value whether to include treatment effect estimate and 95% CI column
#' @param model_formula formula for linear mixed model to estimate treatment effect and 95% CI
#' @param mean a logical value whether to include mean and SD row in summary table
#' @param median a logical value whether to include median and IQR row in summary table
#' @param n_missing a logical value whether to include number of missing n row in summary table
#' @param control_name name of control arm
#' @param intervention_names vector of names for interventions
#' @param IQR_format format of IQR: to be presented as single "iqr" value or as 25th and 75th "percentile"
#' @param mean_dp number of decimal places for mean
#' @param SD_dp number of decimal places for standard deviation
#' @param median_dp number of decimal places for median
#' @param IQR_dp number of decimal places for IQR
#' @param estimate_sf number of significant figures for treatment effect estimate
#' @param CI_sf number of significant figures for 95% CI
#' @param save_image_path file path to save table as image
#' @param save_docx_path file path to save table as docx
#'
#' @return flextable of laboratory values summary for baseline and each post-baseline timepoint by treatment arms
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import flextable
#' @import common
#' @import stringr
#' @import lme4
#' @importFrom purrr possibly
#'
#' @export
#'
#' @examples
#' labtable(lab2, control="Placebo", intervention_levels=c("Intervention"))
labtable <- function(data, control, intervention_levels, lab_test="lab_test", visit="visit", id="id", arm="arm",
                     aval="aval", treatment_effect_estimate=TRUE, model_formula="aval ~ arm + (1|id)",
                     mean=TRUE, median=TRUE, n_missing=TRUE, control_name=NULL, intervention_names=NULL,
                     IQR_format=c("iqr", "percentile"), mean_dp=1, SD_dp=1, median_dp=1, IQR_dp=1, estimate_sf=3, CI_sf=3,
                     save_image_path=NULL, save_docx_path=NULL){
  # change the column names
  dataset <- data %>%
    rename("lab_test" = lab_test, "visit"=visit, "id"=id, "arm"=arm, "aval"=aval)

  # checks if the variable type for each column is correct
  stopifnot("visit variable type is not factor!" = is.factor(dataset[["visit"]]))
  stopifnot("visit variable is not ordered!" = is.ordered(dataset[["visit"]]))
  stopifnot("aval variable type is not numeric!" = is.numeric(dataset[["aval"]]))

  # checks that control is a single value
  stopifnot("control should be of length 1!" = length(control)==1)

  # checks if control and intervention_levels can be found in arm variable
  stopifnot("control level specified cannot be found in arm column!" = control %in% dataset$arm)
  stopifnot("intervention levels specified cannot be found in arm column!" = intervention_levels %in% dataset$arm)

  # sets control_name and intervention_names as control and intervention_levels if names not specified
  if (is.null(control_name)){
    control_name <- control
  }

  if (is.null(intervention_names)){
    intervention_names <- intervention_levels
  }

  # checks if control_name is a character of length 1
  stopifnot("control_name should be of length 1!" = length(control_name)==1)
  # checks if length of intervention_names equals to length of intervention_levels
  stopifnot("length of intervention_names needs to equal to length of intervention_levels!"= length(intervention_names)==length(intervention_levels))

  # number of arm factor levels
  arm_number <- length(unique(dataset$arm))
  # checks if arm_number equals to the number of arms specified
  stopifnot("total number of arms specified do not corresponed to the number of unique arms in arm column!" = (length(control) + length(intervention_levels)) == arm_number)

  # check IQR format is either "iqr" or "percentile", default to "iqr" if none specified
  IQR_format <- match.arg(IQR_format)

  # recode arm factor
  dataset$arm <- as.character(dataset$arm)
  dataset$arm[which(dataset$arm==control)] <- "C"
  dataset$arm[which(dataset$arm==intervention_levels[1])] <- "I1"
  dataset$arm[which(dataset$arm==intervention_levels[2])] <- "I2"
  dataset$arm[which(dataset$arm==intervention_levels[3])] <- "I3"
  dataset$arm <- as.factor(dataset$arm)

  # number of participants at risk per arm
  N0 <- length(unique((dataset %>% filter(arm=="C"))$id))
  N1 <- length(unique((dataset %>% filter(arm=="I1"))$id))
  N2 <- length(unique((dataset %>% filter(arm=="I2"))$id))
  N3 <- length(unique((dataset %>% filter(arm=="I3"))$id))

  options(dplyr.summarise.inform = FALSE)

  Table_lab <- dataset %>%
    group_by(lab_test, visit, arm) %>%
    summarise(
      mean = round(mean(aval), mean_dp),
      sd = round(sd(aval), SD_dp),
      median = round(median(aval), median_dp),
      IQR = ifelse(IQR_format=="iqr", round(IQR(aval, type=2), IQR_dp),
                   str_glue("{round(quantile(aval, probs=0.25), IQR_dp)}, {round(quantile(aval, probs=0.75), IQR_dp)}")),
      n_obs = sum(!is.na(aval))
    ) %>%
    ungroup() %>%
    complete(lab_test, visit, arm, fill=list(n_obs=0)) %>%
    mutate(
      `Mean (SD)` = str_glue("{mean} ({sd})"),
      `Median (IQR)` = str_glue("{median} ({IQR})"),
      `Number of missing` = as.character(case_when(arm=="C" ~ N0, arm=="I1" ~ N1, arm=="I2" ~ N2, arm=="I3" ~ N3) - n_obs)
    ) %>%
    select(-c(mean, sd, median, IQR, n_obs)) %>%
    pivot_longer(cols=!c(lab_test, visit, arm), names_to="summary_statistics", values_to="value") %>%
    pivot_wider(names_from=arm, values_from=value)

  # exclude treatment effect estimate column if there are more than 2 arms in the table
  if (arm_number > 2){
    treatment_effect_estimate <- FALSE
  }

  if (treatment_effect_estimate==TRUE){
    df_reg <- dataset %>%
      mutate(arm=relevel(arm, ref="C"))

    lmer_func <- function(x){
      fit <- lme4::lmer(as.formula(model_formula), data=df_reg %>% filter(lab_test==x), REML=FALSE)
      return(list(Estimate=fixef(fit)[["armI1"]],
                  lower=confint(profile(fit), level=0.95)["armI1", "2.5 %"],
                  upper=confint(profile(fit), level=0.95)["armI1", "97.5 %"]))
    }

    lmer_func_vect <- Vectorize(lmer_func)

    df_estimate <- data.frame(unique(dataset$lab_test)) %>%
      rename("lab_test"=unique.dataset.lab_test.) %>%
      rowwise() %>%
      mutate(
        Coef = list(possibly(lmer_func_vect, otherwise=NA)(lab_test)),
        Estimate = signif(Coef[[1]], estimate_sf),
        lowerCI = signif(ifelse(is.na(Estimate)==FALSE, Coef[[2]], NA), CI_sf),
        upperCI = signif(ifelse(is.na(Estimate)==FALSE, Coef[[3]], NA), CI_sf),
        Estimate = str_glue("{Estimate} ({lowerCI}, {upperCI})")
      ) %>%
      select(-c(Coef, lowerCI, upperCI))

    Table_lab <- Table_lab %>%
      merge(df_estimate, by="lab_test")
  }

  #to produce nice table
  name1 <- "N" %p% subsc("1")
  name2 <- "N" %p% subsc("2")
  name3 <- "N" %p% subsc("3")
  name4 <- "N" %p% subsc("4")
  border <- fp_border_default(width=1.5)

  if (mean==FALSE & median==FALSE & n_missing==FALSE){
    mean <- TRUE
  }

  if (mean==FALSE){
    Table_lab <- Table_lab %>% filter(summary_statistics!="Mean (SD)")
  }

  if (median==FALSE){
    Table_lab <- Table_lab %>% filter(summary_statistics!="Median (IQR)")
  }

  if (n_missing==FALSE){
    Table_lab <- Table_lab %>% filter(summary_statistics!="Number of missing")
  }

  if (arm_number==2){
    if (treatment_effect_estimate==TRUE){
      Table_lab_print <- Table_lab %>%
        select(lab_test, visit, summary_statistics, I1, C, Estimate) %>%
        flextable() %>%
        set_header_labels(lab_test="Lab Measure", visit="", summary_statistics="",
                          I1=str_glue("{intervention_names[1]} ({name1}={N1})"),
                          C=str_glue("{control_name} ({name2}={N0})"),
                          Estimate="Treatment effect estimate\n(Intervention vs Control, 95% CI)") %>%
        merge_v(j=6) %>%
        flextable::align(align="center", j=c(2:6), part="all") %>%
        vline(j=c(1, 3, 4, 5), part="all")
    } else{
      Table_lab_print <- Table_lab %>%
        select(lab_test, visit, summary_statistics, I1, C) %>%
        flextable() %>%
        set_header_labels(lab_test="Lab Measure", visit="", summary_statistics="",
                          I1=str_glue("{intervention_names[1]} ({name1}={N1})"),
                          C=str_glue("{control_name} ({name2}={N0})")) %>%
        flextable::align(align="center", j=c(2:5), part="all") %>%
        vline(j=c(1, 3, 4), part="all")
    }
  } else if (arm_number==3){
    Table_lab_print <- Table_lab %>%
      select(lab_test, visit, summary_statistics, I1, I2, C) %>%
      flextable() %>%
      set_header_labels(lab_test="Lab Measure", visit="", summary_statistics="",
                        I1=str_glue("{intervention_names[1]} ({name1}={N1})"),
                        I2=str_glue("{intervention_names[2]} ({name2}={N2})"),
                        C=str_glue("{control_name} ({name3}={N0})")) %>%
      flextable::align(align="center", j=c(2:6), part="all") %>%
      vline(j=c(1, 3, 4, 5), part="all")
  } else{
    Table_lab_print <- Table_lab %>%
      select(lab_test, visit, summary_statistics, I1, I2, I3, C) %>%
      flextable() %>%
      set_header_labels(lab_test="Lab Measure", visit="", summary_statistics="",
                        I1=str_glue("{intervention_names[1]} ({name1}={N1})"),
                        I2=str_glue("{intervention_names[2]} ({name2}={N2})"),
                        I3=str_glue("{intervention_names[3]} ({name3}={N3})"),
                        C=str_glue("{control_name} ({name4}={N0})")) %>%
      flextable::align(align="center", j=c(2:7), part="all") %>%
      vline(j=c(1, 3, 4, 5, 6), part="all")
  }

  Table_lab_print <- Table_lab_print %>%
    merge_v(j=c(1, 2)) %>%
    autofit() %>%
    vline(j=2, part="body") %>%
    hline(part="body") %>%
    bold(bold=TRUE, part="header") %>%
    bg(part="header", bg="gray80") %>%
    bg(part="body", bg="white")

  if (!is.null(save_image_path)){
    save_as_image(Table_lab_print, path=save_image_path)
  }

  if(!is.null(save_docx_path)){
    save_as_docx(Table_lab_print, path=save_docx_path)
  }

  return(Table_lab_print)

}
