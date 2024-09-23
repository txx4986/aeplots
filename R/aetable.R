#' Table of AE summary by body system class
#'
#' @description
#' `aetable` is used to plot a table of AE summary by body system class and arm using the following summary statistics:
#'
#' - **Total number of participants at risk per arm**
#' - **Frequency** (number of participants with at least one event for each body system class)
#' - **Proportions** (number of participants with at least one event for each body system class relative to number of participants at risk)
#' - **Number of adverse events per participant**: presented as **counts**, **mean (SD)**
#' - **Total number of events** and **incidence rates** (**number of events** relative to **total time in follow-up**)
#' - **Treatment effect estimate** and **95% confidence intervals**
#'
#' @section Treatment Effect Estimate:
#' The treatment effect and its 95% confidence interval are estimated via the following models below:
#'
#' - **Poisson (rate)**: fitting a generalised linear model with Poisson family and log link with length of follow up time as offset
#' - **Poisson (count)**: fitting a generalised linear model with Poisson family and log link with count as response
#' - **Negative Binomial (rate)**: fitting a Negative Binomial model with length of follow up time as offset
#' - **Negative Binomial (count)**: fitting a Negative Binomial model with count as response
#' - **Binomial (logit)**: fitting a generalised linear model with Binomial family and logit link
#' - **Binomial (log)**: fitting a generalised linear model with Binomial family and log link
#' - **Binomial (identity)**: fitting a generalised linear model with Binomial family and identity link
#'
#' The default model is the `Poisson (rate)` model.
#'
#' For Poisson and Negative Binomial models, the treatment effect estimate is the **Incidence Rate Ratio (IRR)**.
#' For Binomial (logit) model, the treatment effect estimate is the **Odds Ratio (OR)**.
#' For Binomial (log) model, the treatment effect estimate is the **Risk Ratio (RR)**.
#' For Binomial (identity) model, the treatment effect estimate is the **Risk Difference (RD)**.
#'
#' The reference group for the treatment arm in the regression model is the control arm.
#'
#' Additional covariates besides arm can be added into the model via the argument `variables`.
#' Note that interaction terms cannot be added to the model.
#'
#' @param data data frame with adverse_event, body_system_class, id, arm, date_rand and last_visit columns (optional: variables to be included in model)
#' @param control factor level of control arm
#' @param intervention_levels vector of factor levels for interventions
#' @param body_system_class name of body_system_class column
#' @param id name of id column
#' @param arm name of arm column
#' @param date_rand name of date_rand column
#' @param last_visit name of last_visit column
#' @param control_name name of control arm
#' @param intervention_names vector of names for interventions
#' @param treatment_effect_estimate a logical value whether to include treatment effect estimate and 95% CI column in summary table (only for 2 arms)
#' @param model model used for computation of treatment effect estimate and 95% CI
#' @param variables vector of variable names to be included in the model for computation of treatment effect estimate (excluding arm)
#' @param mean a logical value whether to include mean and SD column in summary table
#' @param drop_bodsys a logical value whether to drop body system class with no observations from summary table
#' @param IR_per_person incidence rate per number of person
#' @param proportions_dp number of decimal places for proportions
#' @param IR_dp number of decimal places for incidence rate
#' @param mean_dp number of decimal places for mean number of AEs per participant
#' @param SD_dp number of decimal places for standard deviation of number of AEs per participant
#' @param estimate_sf number of significant figures for treatment effect estimate
#' @param CI_sf number of significant figures for 95% CI
#' @param save_image_path file path to save table as image
#' @param save_docx_path file path to save table as docx
#'
#' @return flextable of AE summary by body system class and arm
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import flextable
#' @import common
#' @import lubridate
#' @import stringr
#' @importFrom purrr possibly
#' @importFrom MASS glm.nb
#'
#' @export
#'
#' @examples
#' df2$aebodsys <- as.factor(df2$aebodsys)
#' aetable(df2, body_system_class="aebodsys", control="Placebo", intervention_levels=c("Intervention"), treatment_effect_estimate=TRUE, variables = c("variable1", "variable2"))
aetable <- function(data, control, intervention_levels, body_system_class = "body_system_class", id = "id",
                    arm = "arm", date_rand = "date_rand", last_visit = "last_visit", control_name=NULL,
                    intervention_names=NULL, treatment_effect_estimate = TRUE, model="Poisson (rate)",
                    variables = c(), mean = TRUE, drop_bodsys=TRUE, IR_per_person=100, proportions_dp = 1, IR_dp = 1,
                    mean_dp = 1, SD_dp = 1, estimate_sf = 3, CI_sf = 3, save_image_path=NULL, save_docx_path=NULL){
  # change the column names
  dataset <- data %>%
    rename("body_system_class" = body_system_class, "id" = id, "arm" = arm, "date_rand" = date_rand,
           "last_visit" = last_visit)

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))
  stopifnot("date_rand variable type is not Date!" = is.Date(dataset[["date_rand"]]))
  stopifnot("last_visit variable type is not Date!" = is.Date(dataset[["last_visit"]]))

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
  # checks if arm number is 2, 3 or 4
  stopifnot("aetable can only take 2, 3 or 4 arms!" = (arm_number==2|arm_number==3|arm_number==4))

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

  dataset <- dataset %>%
    # follow up time is computed as difference between randomisation date and last visit date (units=weeks)
    mutate(follow_up_time = as.numeric(difftime(last_visit, date_rand, units="weeks")))

  Table1 <- dataset %>%
    group_by(body_system_class, id, arm, follow_up_time) %>%
    # count total number of adverse events of each participant for each body system class
    count() %>%
    group_by(arm) %>%
    # total length of follow up time for each arm
    mutate(Total_Time = sum(follow_up_time)) %>%
    group_by(body_system_class) %>%
    complete(arm, fill=list(n=0)) %>%
    group_by(body_system_class, arm, Total_Time, .drop=drop_bodsys) %>%
    summarise(
      # number of participants with at least one adverse event for each body system class and arm
      Frequency = length(unique(na.omit(id))),
      # total number of adverse events for each body system class and arm
      Events = sum(n),
      # mean number of adverse events for each participant for each body system class and arm
      Mean = round(mean(n), mean_dp),
      # standard deviation of number of adverse events for each participant for each body system class and arm
      SD = round(sd(n), SD_dp)) %>%
    mutate(
      # IR = Events / Total_Time
      IR = round(Events / Total_Time * IR_per_person, IR_dp),
      # Proportions = Frequency / N
      Proportions =
        case_when(arm=="C" ~ scales::percent(Frequency / N0, 10^(-proportions_dp)),
                  arm=="I1" ~ scales::percent(Frequency / N1, 10^(-proportions_dp)),
                  arm=="I2" ~ scales::percent(Frequency / N2, 10^(-proportions_dp)),
                  arm=="I3" ~ scales::percent(Frequency / N3, 10^(-proportions_dp)))) %>%
    mutate(
      # combine Frequency & Proportions, Events & IR, Mean & SD columns
      Frequency = str_glue("{Frequency} ({Proportions})"),
      Events = str_glue("{Events} ({IR})"),
      Mean = str_glue("{Mean} ({SD})")) %>%
    select(
      # drop Total_Time, SD, IR and Proportions from table
      -c(Total_Time, SD, IR, Proportions)) %>%
    pivot_wider(
      names_from = arm, values_from = c(Frequency, Events, Mean)) %>%
    filter(!is.na(body_system_class))

  # exclude IRR column if there are more than 2 arms in the table
  if (arm_number > 2){
    treatment_effect_estimate <- FALSE
  }

  if (treatment_effect_estimate==TRUE){
    # check if the model specified is either Poisson/Negative Binomial/Binomial
    stopifnot("Model specified is not one of Poisson (rate), Poisson (count), Negative Binomial (rate), Negative Binomial (count), Binomial (logit), Binomial (log) or Binomial (identity)" = (model=="Poisson (rate)")|(model=="Poisson (count)")|(model=="Negative Binomial (rate)")|(model=="Negative Binomial (count)")|(model=="Binomial (logit)")|model=="Binomial (log)"|(model=="Binomial (identity)"))

    # count number of AEs for each id grouped by body_system_class
    reg_df <- dataset %>%
      mutate(id=as.factor(id)) %>%
      group_by(body_system_class, id) %>%
      count() %>%
      group_by(body_system_class) %>%
      complete(id, fill=list(n=0))
    lookup <- dataset %>% select(all_of(c(variables, "id", "arm", "follow_up_time"))) %>% distinct()
    reg_df <- reg_df %>% merge(lookup, by="id")

    if (model=="Poisson (rate)"){
      estimator <- "IRR"

      # fit glm model with Poisson function and log link
      glm_func <- function(x){
        fit <- glm(n ~ . - follow_up_time, offset=log(follow_up_time), family=poisson(link="log"),
                   data=reg_df %>%
                     filter(body_system_class==x) %>%
                     mutate(arm=relevel(arm, ref="C")) %>%
                     ungroup() %>%
                     select(all_of(c(variables, "arm", "follow_up_time", "n"))))
        return(list(Estimate=coef(fit)[["armI1"]], lower=confint(profile(fit), level=0.95)["armI1","2.5 %"],
                    upper=confint(profile(fit), level=0.95)["armI1","97.5 %"]))
      }

    } else if (model=="Poisson (count)"){
      estimator <- "IRR"

      # fit glm model with Poisson function and log link
      glm_func <- function(x){
        fit <- glm(n ~ . , family=poisson(link="log"),
                   data=reg_df %>%
                     filter(body_system_class==x) %>%
                     mutate(arm=relevel(arm, ref="C")) %>%
                     ungroup() %>%
                     select(all_of(c(variables, "arm", "n"))))
        return(list(Estimate=coef(fit)[["armI1"]], lower=confint(profile(fit), level=0.95)["armI1","2.5 %"],
                    upper=confint(profile(fit), level=0.95)["armI1","97.5 %"]))
      }

    } else if (model=="Negative Binomial (rate)"){
      estimator <- "IRR"

      # fit negative binomial model
      glm_func <- function(x){
        fit <- MASS::glm.nb(n ~ . - follow_up_time + offset(log(follow_up_time)),
                            data=reg_df %>%
                              filter(body_system_class==x) %>%
                              mutate(arm=relevel(arm, ref="C")) %>%
                              ungroup() %>%
                              select(all_of(c(variables, "arm", "follow_up_time", "n"))))
        return(list(Estimate=coef(fit)[["armI1"]], lower=confint(profile(fit), level=0.95)["armI1","2.5 %"],
                    upper=confint(profile(fit), level=0.95)["armI1","97.5 %"]))
      }

    } else if (model=="Negative Binomial (count)"){
      estimator <- "IRR"

      # fit negative binomial model
      glm_func <- function(x){
        fit <- MASS::glm.nb(n ~ . ,
                            data=reg_df %>%
                              filter(body_system_class==x) %>%
                              mutate(arm=relevel(arm, ref="C")) %>%
                              ungroup() %>%
                              select(all_of(c(variables, "arm", "n"))))
        return(list(Estimate=coef(fit)[["armI1"]], lower=confint(profile(fit), level=0.95)["armI1","2.5 %"],
                    upper=confint(profile(fit), level=0.95)["armI1","97.5 %"]))
      }

    } else {
      # indicate whether each id had AE for each body_system_class
      reg_df <- reg_df %>%
        mutate(n=ifelse(n>=1, 1, 0)) %>%
        select(-c(follow_up_time))

      if(model=="Binomial (logit)"){
        estimator <- "OR"

        # fit glm model with binomial function and logit link
        glm_func <- function(x){
          fit <- glm(n ~ . , family=binomial(link="logit"),
                     data=reg_df %>%
                       filter(body_system_class==x) %>%
                       mutate(arm=relevel(arm, ref="C")) %>%
                       ungroup() %>%
                       select(all_of(c(variables, "arm", "n"))))
          return(list(Estimate=coef(fit)[["armI1"]], lower=confint(profile(fit), level=0.95)["armI1","2.5 %"],
                      upper=confint(profile(fit), level=0.95)["armI1","97.5 %"]))
        }
      } else if (model=="Binomial (log)"){
        estimator <- "RR"

        # fit glm model with binomial function and log link
        glm_func <- function(x){
          fit <- glm(n ~ . , family=binomial(link="log"),
                     data=reg_df %>%
                       filter(body_system_class==x) %>%
                       mutate(arm=relevel(arm, ref="C")) %>%
                       ungroup() %>%
                       select(all_of(c(variables, "arm", "n"))))
          return(list(Estimate=coef(fit)[["armI1"]], lower=confint(profile(fit), level=0.95)["armI1","2.5 %"],
                      upper=confint(profile(fit), level=0.95)["armI1","97.5 %"]))
        }
      } else {
        estimator <- "RD"

        # fit glm model with binomial function and identity link
        glm_func <- function(x){
          fit <- glm(n ~ . , family=binomial(link="identity"),
                     data=reg_df %>%
                       filter(body_system_class==x) %>%
                       mutate(arm=relevel(arm, ref="C")) %>%
                       ungroup() %>%
                       select(all_of(c(variables, "arm", "n"))))
          return(list(Estimate=coef(fit)[["armI1"]], lower=confint(profile(fit), level=0.95)["armI1","2.5 %"],
                      upper=confint(profile(fit), level=0.95)["armI1","97.5 %"]))
        }
      }
    }

    glm_func_vect <- Vectorize(glm_func)

    Table1_treatment <- Table1 %>%
      rowwise() %>%
      mutate(
        Coef = list(possibly(glm_func_vect, otherwise=NA)(body_system_class)))

    if(model=="Binomial (identity)"){
      Table1_treatment <- Table1_treatment %>%
        mutate(
          Estimate = signif(Coef[[1]], estimate_sf),
          lower = signif(ifelse(is.na(Estimate)==FALSE, Coef[[2]], NA), CI_sf),
          upper = signif(ifelse(is.na(Estimate)==FALSE, Coef[[3]], NA), CI_sf),
          CI = str_c("(", lower, ", ", upper, ")")
        ) %>%
        select(-c(Coef, lower, upper))

    } else{
      Table1_treatment <- Table1_treatment %>%
        mutate(
          Estimate = signif(exp(Coef[[1]]), estimate_sf),
          lower = signif(exp(ifelse(is.na(Estimate)==FALSE, Coef[[2]], NA)), CI_sf),
          upper = signif(exp(ifelse(is.na(Estimate)==FALSE, Coef[[3]], NA)), CI_sf),
          CI = str_c("(", lower, ", ", upper, ")")
        ) %>%
        select(-c(Coef, lower, upper))
    }
  }

  #to produce nice table
  name1 <- "N" %p% subsc("1")
  name2 <- "N" %p% subsc("2")
  name3 <- "N" %p% subsc("3")
  name4 <- "N" %p% subsc("4")
  border <- fp_border_default(width=1.5)

  if (mean==TRUE){
    if (arm_number==2){
      if (treatment_effect_estimate==TRUE){
        Table1_print <- Table1_treatment %>%
          select(body_system_class, Frequency_I1, Events_I1, Mean_I1, Frequency_C, Events_C, Mean_C, Estimate,
                 CI) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_I1="At least one event",
                       Events_I1="Number of events",
                       Mean_I1="Number of events",
                       Frequency_C="At least one event",
                       Events_C="Number of events",
                       Mean_C="Number of events",
                       Estimate = str_glue("Treatment Effect Estimate ({estimator})"),
                       CI = "95% CI")) %>%
          add_header_row(
            values=c("", str_glue("{intervention_names[1]} ({name1}={N1})"),
                     str_glue("{control_name} ({name2}={N0})"),
                     str_glue("Treatment Effect Estimate ({estimator})"), "95% CI"),
            colwidths = c(1, 3, 3, 1, 1)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)",
            Mean_I1="Mean number of events per participant (SD)", Frequency_C="N (%)",
            Events_C="n (IR)", Mean_C="Mean number of events per participant (SD)",
            Estimate = str_glue("Treatment Effect Estimate ({estimator})"), CI = "95% CI") %>%
          merge_h(part="header") %>%
          merge_v(part="header") %>%
          flextable::align(align="center", j = c(2:9), part="all") %>%
          autofit() %>%
          width(j=c(1, 9), width=1.1) %>%
          width(j=c(2:8), width=0.75) %>%
          vline(j=c(1:8), border=border, part="all") %>%
          fix_border_issues() %>%
          bold(i=1, bold=TRUE, part="header") %>%
          bg(part="header", bg="gray80") %>%
          bg(part="body", bg="white") %>%
          fontsize(size=6.5, part="all")
      } else {
        Table1_print <- Table1 %>%
          select(body_system_class, Frequency_I1, Events_I1, Mean_I1, Frequency_C, Events_C, Mean_C) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_I1="At least one event",
                       Events_I1="Number of events",
                       Mean_I1="Number of events",
                       Frequency_C="At least one event",
                       Events_C="Number of events",
                       Mean_C="Number of events")) %>%
          add_header_row(
            values=c("", str_glue("{intervention_names[1]} ({name1}={N1})"), str_glue("{control_name} ({name2}={N0})")),
            colwidths = c(1, 3, 3)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)",
            Mean_I1="Mean number of events per participant (SD)", Frequency_C="N (%)",Events_C="n (IR)",
            Mean_C="Mean number of events per participant (SD)") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:7), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:7), width=0.75) %>%
          vline(j=c(1:6), border=border, part="all") %>%
          fix_border_issues() %>%
          bold(i=1, bold=TRUE, part="header") %>%
          bg(part="header", bg="gray80") %>%
          bg(part="body", bg="white") %>%
          fontsize(size=6.5, part="all")
      }
    } else if (arm_number==3){
      Table1_print <- Table1 %>%
        select(body_system_class, Frequency_I1, Events_I1, Mean_I1, Frequency_I2, Events_I2, Mean_I2, Frequency_C,
               Events_C, Mean_C) %>%
        flextable() %>%
        add_header(
          values = c(Frequency_I1="At least one event",
                     Events_I1="Number of events",
                     Mean_I1="Number of events",
                     Frequency_I2="At least one event",
                     Events_I2="Number of events",
                     Mean_I2="Number of events",
                     Frequency_C="At least one event",
                     Events_C="Number of events",
                     Mean_C="Number of events")) %>%
        add_header_row(
          values=c("", str_glue("{intervention_names[1]} ({name1}={N1})"), str_glue("{intervention_names[2]} ({name2}={N2})"),
                   str_glue("{control_name} ({name3}={N0})")),
          colwidths = c(1, 3, 3, 3)) %>%
        set_header_labels(
          body_system_class="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)",
          Mean_I1="Mean number of events per participant (SD)",  Frequency_I2="N (%)", Events_I2="n (IR)",
          Mean_I2="Mean number of events per participant (SD)", Frequency_C="N (%)",Events_C="n (IR)",
          Mean_C="Mean number of events per participant (SD)") %>%
        merge_h(part="header") %>%
        flextable::align(align="center", j = c(2:10), part="all") %>%
        autofit() %>%
        width(j=1, width=1.1) %>%
        width(j=c(2:10), width=0.75) %>%
        vline(j=c(1:6), border=border, part="all") %>%
        fix_border_issues() %>%
        bold(i=1, bold=TRUE, part="header") %>%
        bg(part="header", bg="gray80") %>%
        bg(part="body", bg="white") %>%
        fontsize(size=6.5, part="all")
    } else {
      Table1_print <- Table1 %>%
        select(body_system_class, Frequency_I1, Events_I1, Mean_I1, Frequency_I2, Events_I2, Mean_I2, Frequency_I3,
               Events_I3, Mean_I3, Frequency_C, Events_C, Mean_C) %>%
        flextable() %>%
        add_header(
          values = c(Frequency_I1="At least one event",
                     Events_I1="Number of events",
                     Mean_I1="Number of events",
                     Frequency_I2="At least one event",
                     Events_I2="Number of events",
                     Mean_I2="Number of events",
                     Frequency_I3="At least one event",
                     Events_I3="Number of events",
                     Mean_I3="Number of events",
                     Frequency_C="At least one event",
                     Events_C="Number of events",
                     Mean_C="Number of events")) %>%
        add_header_row(
          values=c("", str_glue("{intervention_names[1]} ({name1}={N1})"), str_glue("{intervention_names[2]} ({name2}={N2})"),
                   str_glue("{intervention_names[3]} ({name3}={N3})"), str_glue("{control_name} ({name4}={N0})")),
          colwidths = c(1, 3, 3, 3, 3)) %>%
        set_header_labels(
          body_system_class="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)",
          Mean_I1="Mean number of events per participant (SD)",  Frequency_I2="N (%)", Events_I2="n (IR)",
          Mean_I2="Mean number of events per participant (SD)", Frequency_I3="N (%)", Events_I3="n (IR)",
          Mean_I3="Mean number of events per participant (SD)", Frequency_C="N (%)",Events_C="n (IR)",
          Mean_C="Mean number of events per participant (SD)") %>%
        merge_h(part="header") %>%
        flextable::align(align="center", j = c(2:13), part="all") %>%
        autofit() %>%
        width(j=1, width=1.1) %>%
        width(j=c(2:13), width=0.75) %>%
        vline(j=c(1:12), border=border, part="all") %>%
        fix_border_issues() %>%
        bold(i=1, bold=TRUE, part="header") %>%
        bg(part="header", bg="gray80") %>%
        bg(part="body", bg="white") %>%
        fontsize(size=6.5, part="all")
    }
  } else{
    if (arm_number==2){
      if (treatment_effect_estimate==TRUE){
        Table1_print <- Table1_treatment %>%
          select(body_system_class, Frequency_I1, Events_I1, Frequency_C, Events_C, Estimate, CI) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_I1="At least one event",
                       Events_I1="Number of events",
                       Frequency_C="At least one event",
                       Events_C="Number of events",
                       Estimate = str_glue("Treatment Effect Estimate ({estimator})"),
                       CI = "95% CI")) %>%
          add_header_row(
            values=c("", str_glue("{intervention_names[1]} ({name1}={N1})"), str_glue("{control_name} ({name2}={N0})"),
                     str_glue("Treatment Effect Estimate ({estimator})"), "95% CI"),
            colwidths = c(1, 2, 2, 1, 1)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)", Frequency_C="N (%)",
            Events_C="n (IR)", Estimate = str_glue("Treatment Effect Estimate ({estimator})"), CI = "95% CI") %>%
          merge_h(part="header") %>%
          merge_v(part="header") %>%
          flextable::align(align="center", j = c(2:7), part="all") %>%
          autofit() %>%
          width(j=c(1, 7), width=1.1) %>%
          width(j=c(2:6), width=0.75) %>%
          vline(j=c(1:6), border=border, part="all") %>%
          fix_border_issues() %>%
          bold(i=1, bold=TRUE, part="header") %>%
          bg(part="header", bg="gray80") %>%
          bg(part="body", bg="white") %>%
          fontsize(size=6.5, part="all")
      } else {
        Table1_print <- Table1 %>%
          select(body_system_class, Frequency_I1, Events_I1, Frequency_C, Events_C) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_I1="At least one event",
                       Events_I1="Number of events",
                       Frequency_C="At least one event",
                       Events_C="Number of events")) %>%
          add_header_row(
            values=c("", str_glue("{intervention_names[1]} ({name1}={N1})"), str_glue("{control_name} ({name2}={N0})")),
            colwidths = c(1, 2, 2)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)",Frequency_C="N (%)",
            Events_C="n (IR)") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:5), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:5), width=0.75) %>%
          vline(j=c(1:4), border=border, part="all") %>%
          fix_border_issues() %>%
          bold(i=1, bold=TRUE, part="header") %>%
          bg(part="header", bg="gray80") %>%
          bg(part="body", bg="white") %>%
          fontsize(size=6.5, part="all")
      }
    } else if (arm_number==3){
      Table1_print <- Table1 %>%
        select(body_system_class, Frequency_I1, Events_I1, Frequency_I2, Events_I2, Frequency_C, Events_C) %>%
        flextable() %>%
        add_header(
          values = c(Frequency_I1="At least one event",
                     Events_I1="Number of events",
                     Frequency_I2="At least one event",
                     Events_I2="Number of events",
                     Frequency_C="At least one event",
                     Events_C="Number of events")) %>%
        add_header_row(
          values=c("", str_glue("{intervention_names[1]} ({name1}={N1})"), str_glue("{intervention_names[2]} ({name2}={N2})"),
                   str_glue("{control_name} ({name3}={N0})")),
          colwidths = c(1, 2, 2, 2)) %>%
        set_header_labels(
          body_system_class="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)", Frequency_I2="N (%)",
          Events_I2="n (IR)", Frequency_C="N (%)",Events_C="n (IR)") %>%
        merge_h(part="header") %>%
        flextable::align(align="center", j = c(2:7), part="all") %>%
        autofit() %>%
        width(j=1, width=1.1) %>%
        width(j=c(2:7), width=0.75) %>%
        vline(j=c(1:6), border=border, part="all") %>%
        fix_border_issues() %>%
        bold(i=1, bold=TRUE, part="header") %>%
        bg(part="header", bg="gray80") %>%
        bg(part="body", bg="white") %>%
        fontsize(size=6.5, part="all")
    } else {
      Table1_print <- Table1 %>%
        select(body_system_class, Frequency_I1, Events_I1, Frequency_I2, Events_I2, Frequency_I3, Events_I3,
               Frequency_C, Events_C) %>%
        flextable() %>%
        add_header(
          values = c(Frequency_I1="At least one event",
                     Events_I1="Number of events",
                     Frequency_I2="At least one event",
                     Events_I2="Number of events",
                     Frequency_I3="At least one event",
                     Events_I3="Number of events",
                     Frequency_C="At least one event",
                     Events_C="Number of events")) %>%
        add_header_row(
          values=c("", str_glue("{intervention_names[1]} ({name1}={N1})"), str_glue("{intervention_names[2]} ({name2}={N2})"),
                   str_glue("{intervention_names[3]} ({name3}={N3})"), str_glue("{control_name} ({name4}={N0})")),
          colwidths = c(1, 2, 2, 2, 2)) %>%
        set_header_labels(
          body_system_class="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)", Frequency_I2="N (%)",
          Events_I2="n (IR)", Frequency_I3="N (%)", Events_I3="n (IR)", Frequency_C="N (%)",Events_C="n (IR)") %>%
        merge_h(part="header") %>%
        flextable::align(align="center", j = c(2:9), part="all") %>%
        autofit() %>%
        width(j=1, width=1.1) %>%
        width(j=c(2:9), width=0.75) %>%
        vline(j=c(1:8), border=border, part="all") %>%
        fix_border_issues() %>%
        bold(i=1, bold=TRUE, part="header") %>%
        bg(part="header", bg="gray80") %>%
        bg(part="body", bg="white") %>%
        fontsize(size=6.5, part="all")
    }
  }

  plot(Table1_print, scaling="full")

  if (!is.null(save_image_path)){
    save_as_image(Table1_print, path=save_image_path)
  }

  if(!is.null(save_docx_path)){
    save_as_docx(Table1_print, path=save_docx_path)
  }
}
