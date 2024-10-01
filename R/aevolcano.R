#' Volcano plot of AEs between two treatment arms to help identify potential signals of harm
#'
#' @description
#' `aevolcano` creates a volcano plot of treatment effect estimate with the following characteristics:
#'
#'  - **x-axis** represents the treatment effect estimate (risk difference/ relative risk / odds ratio/ incidence rate ratio)
#'  - **y-axis** represents the p-value estimated via two-tailed Fisher's exact test or model fitted on the -log10 scale
#'  - **centre of the bubble** indicates the coordinates for each adverse event
#'  - **size of bubble** is proportional to the total number of events for both treatment arms combined
#'  - **colour saturation** corresponds to the -log10(p-value), indicating the strength of the treatment effect with red indicating greater risk in the intervention arm and blue indicating greater risk in the control arm
#'  - **labels** are added to events where p-value is less than 0.1
#'
#' @section Treatment Effect Estimate:
#' The treatment effect and its p-value are estimated via the following models below:
#'
#' - **unadjusted (RD)**: unadjusted risk difference with p-value obtained via two-tailed Fisher's exact test
#' - **unadjusted (RR)**: unadjusted relative risk with p-value obtained via two-tailed Fisher's exact test
#' - **unadjusted (OR)**: unadjusted odds ratio with p-value obtained via two-tailed Fisher's exact test
#' - **unadjusted (IRR)**: unadjusted incidence rate ratio with p-value obtained via two-tailed Fisher's exact test
#' - **Poisson (rate)**: fitting a generalised linear model with Poisson family and log link with length of follow up time as offset
#' - **Poisson (count)**: fitting a generalised linear model with Poisson family and log link with count as response
#' - **Negative Binomial (rate)**: fitting a Negative Binomial model with length of follow up time as offset
#' - **Negative Binomial (count)**: fitting a Negative Binomial model with count as response
#' - **Binomial (logit)**: fitting a generalised linear model with Binomial family and logit link
#' - **Binomial (log)**: fitting a generalised linear model with Binomial family and log link
#' - **Binomial (identity)**: fitting a generalised linear model with Binomial family and identity link
#'
#' The default model is the `unadjusted (RD)` model.
#'
#' For Poisson and Negative Binomial models, the treatment effect estimate is the **Incidence Rate Ratio (IRR)**.
#' For Binomial (logit) model, the treatment effect estimate is the **Odds Ratio (OR)**.
#' For Binomial (log) model, the treatment effect estimate is the **Relative Risk (RR)**.
#' For Binomial (identity) model, the treatment effect estimate is the **Risk Difference (RD)**.
#'
#' The reference group for the treatment arm in the regression model is the control arm.
#'
#' Additional covariates besides arm can be added into the model via the argument `variables`.
#' Note that interaction terms cannot be added to the model.
#'
#' @param data data frame with adverse_event, body_system_class, id and arm columns (optional: date_rand, last_visit and variables to be included in model)
#' @param control factor level of control arm
#' @param intervention factor level of intervention arm
#' @param body_system_class name of body_system_class column
#' @param id name of id column
#' @param arm name of arm column
#' @param date_rand name of date_rand column
#' @param last_visit name of last_visit column
#' @param model unadjusted risk difference/relative risk or model used for computation of treatment effect estimate and p-value
#' @param variables vector of variable names to be included in the model for computation of treatment effect estimate (excluding arm)
#' @param bubble_colours vector of colours for bubbles indicating greater risk in the intervention arm or control arm
#' @param p_value_cutoff labels are added to events having p-values lower than p_value_cutoff
#' @param save_image_path file path to save volcano plot as image
#'
#' @return volcano plot of treatment effect estimate against -log10(p-value)
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import ggplot2
#' @import ggrepel
#' @import scales
#' @import stringr
#' @importFrom MASS glm.nb
#'
#' @export
#'
#' @examples
#' df2$aebodsys <- as.factor(df2$aebodsys)
#' aevolcano(df2, body_system_class="aebodsys", control="Placebo", intervention="Intervention")
aevolcano <- function(data, control, intervention, body_system_class="body_system_class", id="id", arm="arm",
                      date_rand = "date_rand", last_visit = "last_visit", model="unadjusted (RD)", variables = c(),
                      bubble_colours = c("#2b3bff", "#e14b31"), p_value_cutoff = 0.1, save_image_path=NULL){

  # check if the model specified is either unadjusted/Poisson/Negative Binomial/Binomial
  stopifnot("Model specified is not one of unadjusted (RD), unadjusted (RR), Poisson (rate), Poisson (count), Negative Binomial (rate), Negative Binomial (count), Binomial (logit), Binomial (log) or Binomial (identity)" = (model=="unadjusted (RR)")|(model=="unadjusted (RD)")|(model=="unadjusted (OR)")|(model=="unadjusted (IRR)")|(model=="Poisson (rate)")|(model=="Poisson (count)")|(model=="Negative Binomial (rate)")|(model=="Negative Binomial (count)")|(model=="Binomial (logit)")|model=="Binomial (log)"|(model=="Binomial (identity)"))

  # change the column names
  if ((model=="unadjusted (IRR)") | (model=="Poisson (rate)") | (model=="Negative Binomial (rate)")){
    dataset <- data %>%
      rename("body_system_class" = body_system_class, "id" = id, "arm" = arm, "date_rand"=date_rand,
             "last_visit"=last_visit)
    dataset <- dataset %>%
      # follow up time is computed as difference between randomisation date and last visit date (units=weeks)
      mutate(follow_up_time = as.numeric(difftime(last_visit, date_rand, units="weeks")))
  } else{
    dataset <- data %>%
      rename("body_system_class" = body_system_class, "id" = id, "arm" = arm)
  }

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))

  # checks if control and intervention are of length 1
  stopifnot("control should be of length 1!" = length(control)==1)
  stopifnot("intervention should be of length 1!" = length(intervention)==1)

  # checks if control and intervention can be found in arm variable
  stopifnot("control level specified cannot be found in arm column!" = control %in% dataset$arm)
  stopifnot("intervention level specified cannot be found in arm column!" = intervention %in% dataset$arm)

  # checks if bubble_colours have length 2 and are different
  stopifnot("bubble_colours should be of length 2!" = length(bubble_colours)==2)
  stopifnot("colours specified in bubble_colours should be different!" = bubble_colours[1]!=bubble_colours[2])

  # recode arm factor
  dataset$arm <- as.character(dataset$arm)
  dataset$arm[which(dataset$arm==control)] <- "C"
  dataset$arm[which(dataset$arm==intervention)] <- "I"
  dataset$arm <- as.factor(dataset$arm)

  # number of participants at risk per arm
  N1 <- length(unique((dataset %>% filter(arm=="I"))$id))
  N2 <- length(unique((dataset %>% filter(arm=="C"))$id))

  # create table of number of participants with at least 1 adverse event and total number od adverse events for each body system class
  Table6 <- dataset %>%
    group_by(body_system_class) %>%
    summarise(
      eventn1 = length(unique(id[arm=="I"])),
      N1 = N1,
      eventn2 = length(unique(id[arm=="C"])),
      N2 = N2,
      n_events = sum(!is.na(id)),
      n_events1 = sum(!is.na(id[arm=="I"])),
      n_events2 = sum(!is.na(id[arm=="C"]))
    ) %>%
    filter(!is.na(body_system_class))

  if (str_starts(model, "unadjusted")==TRUE){
    # for unadjusted estimates, compute p-value via Fisher's exact test
    fisher_test_func <- function(x){
      bodsys <- Table6 %>% filter(body_system_class==x)
      p_value <- fisher.test(rbind(c(bodsys$eventn1, bodsys$eventn2), c(bodsys$N1-bodsys$eventn1,
                                                                        bodsys$N2-bodsys$eventn2)))$p.value
      return(p_value)
    }
    fisher_test_func_vect <- Vectorize(fisher_test_func)

    Table6 <- Table6 %>%
      rowwise() %>%
      mutate(p_value = fisher_test_func_vect(body_system_class))

    if (model=="unadjusted (RD)"){
      estimator <- "Risk difference"

      Table6 <- Table6 %>%
        mutate(
          r1 = eventn1 / N1,
          r2 = eventn2 / N2,
          Estimate = r1 - r2)

    } else if (model=="unadjusted (RR)"){
      estimator <- "Relative risk"

      Table6 <- Table6 %>%
        mutate(
          r1 = eventn1 / N1,
          r2 = eventn2 / N2,
          Estimate = r1 / r2)

    } else if (model=="unadjusted (OR)"){
      estimator <- "Odds ratio"

      Table6 <- Table6 %>%
        mutate(
          odds1 = eventn1 / (N1 - eventn1),
          odds2 = eventn2 / (N2 - eventn2),
          Estimate = odds1 / odds2)

    } else if (model=="unadjusted (IRR)"){
      estimator <- "Incidence rate ratio"

      # total follow up time of all participants for each arm
      total_time_df <- dataset %>%
        group_by(arm, id) %>%
        summarise(max_follow_up_time=max(follow_up_time)) %>%
        group_by(arm) %>%
        summarise(total_time=sum(max_follow_up_time))

      total_time0 <- total_time_df[total_time_df$arm=="C",]$total_time
      total_time1 <- total_time_df[total_time_df$arm=="I",]$total_time

      Table6 <- Table6 %>%
        mutate(
          IR1 = n_events1 / total_time1,
          IR2 = n_events2 / total_time0,
          Estimate = IR1 / IR2)
    }
  } else{
    # count number of AEs for each id grouped by body_system_class
    reg_df <- dataset %>%
      mutate(id=as.factor(id)) %>%
      group_by(body_system_class, id) %>%
      count() %>%
      group_by(body_system_class) %>%
      complete(id, fill=list(n=0))
    if ((model=="Poisson (rate)") | (model=="Negative Binomial (rate)")){
      lookup <- dataset %>% select(all_of(c(variables, "id", "arm", "follow_up_time"))) %>% distinct()
    } else{
      lookup <- dataset %>% select(all_of(c(variables, "id", "arm"))) %>% distinct()
    }
    reg_df <- reg_df %>% merge(lookup, by="id")

    if (model=="Poisson (rate)"){
      estimator <- "Incidence rate ratio"

      # fit glm model with Poisson function and log link
      glm_func <- function(x){
        fit <- glm(n ~ . - follow_up_time, offset=log(follow_up_time), family=poisson(link="log"),
                   data=reg_df %>%
                     filter(body_system_class==x) %>%
                     mutate(arm=relevel(arm, ref="C")) %>%
                     ungroup() %>%
                     select(all_of(c(variables, "arm", "follow_up_time", "n"))))
        return(list(Estimate=coef(fit)[["armI"]], p_value=coef(summary(fit))[, 4][["armI"]]))
      }

    } else if (model=="Poisson (count)"){
      estimator <- "Incidence rate ratio"

      # fit glm model with Poisson function and log link
      glm_func <- function(x){
        fit <- glm(n ~ . , family=poisson(link="log"),
                   data=reg_df %>%
                     filter(body_system_class==x) %>%
                     mutate(arm=relevel(arm, ref="C")) %>%
                     ungroup() %>%
                     select(all_of(c(variables, "arm", "n"))))
        return(list(Estimate=coef(fit)[["armI"]], p_value=coef(summary(fit))[, 4][["armI"]]))
      }

    } else if (model=="Negative Binomial (rate)"){
      estimator <- "Incidence rate ratio"

      # fit negative binomial model
      glm_func <- function(x){
        fit <- MASS::glm.nb(n ~ . - follow_up_time + offset(log(follow_up_time)),
                            data=reg_df %>%
                              filter(body_system_class==x) %>%
                              mutate(arm=relevel(arm, ref="C")) %>%
                              ungroup() %>%
                              select(all_of(c(variables, "arm", "follow_up_time", "n"))))
        return(list(Estimate=coef(fit)[["armI"]], p_value=coef(summary(fit))[, 4][["armI"]]))
      }

    } else if (model=="Negative Binomial (count)"){
      estimator <- "Incidence rate ratio"

      # fit negative binomial model
      glm_func <- function(x){
        fit <- MASS::glm.nb(n ~ . ,
                            data=reg_df %>%
                              filter(body_system_class==x) %>%
                              mutate(arm=relevel(arm, ref="C")) %>%
                              ungroup() %>%
                              select(all_of(c(variables, "arm", "n"))))
        return(list(Estimate=coef(fit)[["armI"]], p_value=coef(summary(fit))[, 4][["armI"]]))
      }

    } else {
      # indicate whether each id had AE for each body_system_class
      reg_df <- reg_df %>%
        mutate(n=ifelse(n>=1, 1, 0))

      if(model=="Binomial (logit)"){
        estimator <- "Odds ratio"

        # fit glm model with binomial function and logit link
        glm_func <- function(x){
          fit <- glm(n ~ . , family=binomial(link="logit"),
                     data=reg_df %>%
                       filter(body_system_class==x) %>%
                       mutate(arm=relevel(arm, ref="C")) %>%
                       ungroup() %>%
                       select(all_of(c(variables, "arm", "n"))))
          return(list(Estimate=coef(fit)[["armI"]], p_value=coef(summary(fit))[, 4][["armI"]]))
        }
      } else if (model=="Binomial (log)"){
        estimator <- "Relative risk"

        # fit glm model with binomial function and log link
        glm_func <- function(x){
          fit <- glm(n ~ . , family=binomial(link="log"),
                     data=reg_df %>%
                       filter(body_system_class==x) %>%
                       mutate(arm=relevel(arm, ref="C")) %>%
                       ungroup() %>%
                       select(all_of(c(variables, "arm", "n"))))
          return(list(Estimate=coef(fit)[["armI"]], p_value=coef(summary(fit))[, 4][["armI"]]))
        }
      } else {
        estimator <- "Risk difference"

        # fit glm model with binomial function and identity link
        glm_func <- function(x){
          fit <- glm(n ~ . , family=binomial(link="identity"),
                     data=reg_df %>%
                       filter(body_system_class==x) %>%
                       mutate(arm=relevel(arm, ref="C")) %>%
                       ungroup() %>%
                       select(all_of(c(variables, "arm", "n"))))
          return(list(Estimate=coef(fit)[["armI"]], p_value=coef(summary(fit))[, 4][["armI"]]))
        }
      }
    }

    glm_func_vect <- Vectorize(glm_func)
    Table6 <- Table6 %>%
      rowwise() %>%
      mutate(
        Coef = list(possibly(glm_func_vect, otherwise=NA)(body_system_class)))

    if(model=="Binomial (identity)"){
      Table6 <- Table6 %>%
        mutate(
          Estimate = Coef[[1]],
          p_value = ifelse(is.na(Estimate)==FALSE, Coef[[2]], NA)
        ) %>%
        select(-c(Coef))

    } else{
      Table6 <- Table6 %>%
        mutate(
          Estimate = exp(Coef[[1]]),
          p_value = ifelse(is.na(Estimate)==FALSE, Coef[[2]], NA)
        ) %>%
        select(-c(Coef))
    }
  }

  Table6 <- Table6 %>%
    mutate(
      log_p_value = -log10(p_value),
      # label body system class with p_value less than p_value_cutoff (default=0.1)
      labels = ifelse(p_value < p_value_cutoff, as.character(body_system_class), ""))

  if(estimator=="Risk difference"){
    Table6 <- Table6 %>%
      mutate(harmful = ifelse(Estimate > 0, 1, 0))
  } else{
    Table6 <- Table6 %>%
      mutate(harmful = ifelse(Estimate > 1, 1, 0))
  }

  volcano_plot <- ggplot(Table6, aes(x=Estimate, y=log_p_value, size=n_events, color=as.factor(harmful), label=labels)) +
    geom_point(alpha=(Table6$log_p_value + 0.05)) +
    scale_y_continuous(name = "-log10(Fishers' Exact P-value)", expand=expansion(add = c(0.1,0.2))) +
    scale_size(range = c(1, 24)) +
    scale_color_manual(breaks = c("0", "1"), values=bubble_colours) +
    geom_text_repel(size=3, color="black", nudge_y=-Table6$n_events/(max(Table6$n_events)*6), min.segment.length=3.5) +
    theme(panel.background = element_blank(),
          legend.position = "none",
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"))

  if ((model=="unadjusted (RD)")|(model=="Binomial (identity)")){
    volcano_plot <- volcano_plot +
      scale_x_continuous(name=str_glue("{estimator}"),
                         expand=expansion(add = c(0.035, 0.035)),
                         breaks=seq(from=-1, to=1, by=0.1),
                         labels=as.character(seq(from=-1, to=1, by=0.1)))
  } else{
    volcano_plot <- volcano_plot +
      scale_x_continuous(name=str_glue("{estimator}"),
                         expand=expansion(add = c(0.5, 0.5)),
                         trans=log2_trans(),
                         breaks=c(0.10, 0.5, 1, 2, 5, 10, 50, 100, 220),
                         labels=as.character(c(0.10, 0.5, 1, 2, 5, 10, 50, 100, 220)))
  }

  plot(volcano_plot)

  if (!is.null(save_image_path)){
    ggsave(save_image_path, dpi=700, width=10, height=8, bg="white")
  }
}
