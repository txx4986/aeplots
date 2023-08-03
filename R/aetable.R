#' Table of AE summary by body system class
#'
#' @description
#' `aetable` is used to plot a table of AE summary by body system class and arm using the following summary statistics:
#'
#' - **Total number of participants at risk per arm**
#' - **Frequency** (number of participants with at least one event for each event)
#' - **Proportions** (number of participants with at least one event for each body system class relative to number of participants at risk)
#' - **Number of adverse events per participant**: presented as **counts**, **mean (SD)**
#' - **Total number of events** and **incidence rates** (**number of events** relative to **total time in follow-up**)
#' - **Treatment effect estimate (IRR)** and **95% confidence intervals**
#'
#' @param data data frame with adverse_event, body_system_class, id, arm, date_rand and last_visit columns
#' @param body_system_class name of body_system_class column
#' @param id name of id column
#' @param arm name of arm column
#' @param date_rand name of date_rand column
#' @param last_visit name of last_visit column
#' @param control factor level of control arm
#' @param intervention1 factor level of intervention 1 arm
#' @param intervention2 factor level of intervention 2 arm
#' @param intervention3 factor level of intervention 3 arm
#' @param control_name name of control arm
#' @param intervention1_name name of intervention 1 arm
#' @param intervention2_name name of intervention 2 arm
#' @param intervention3_name name of intervention 3 arm
#' @param IRR a logical value whether to include IRR and 95% CI column in summary table (only for 2 arms)
#' @param variables vector of variable names to be included in the glm Poisson model for computation of IRR (excluding arm)
#' @param mean a logical value whether to include mean and SD column in summary table
#' @param proportions_dp number of decimal places for proportions
#' @param IR_dp number of decimal places for incidence rate
#' @param mean_dp number of decimal places for mean number of AEs per participant
#' @param SD_dp number of decimal places for standard deviation of number of AEs per participant
#' @param IRR_sf number of significant figures for IRR
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
#' @import here
#'
#' @export
#'
#' @examples
#' ## ADAPT sample data
#' aetable(df, body_system_class="ae_02", control="Placebo", intervention1="Anti-IgE", IRR=TRUE, variables = c("agestrat", "IgEstrat"), mean=FALSE)
aetable <- function(data, body_system_class = "body_system_class", id = "id", arm = "arm", date_rand = "date_rand",
                    last_visit = "last_visit", control = "C", intervention1 = "I1", intervention2 = "I2",
                    intervention3="I3", control_name="Control", intervention1_name = "Intervention 1",
                    intervention2_name = "Intervention 2", intervention3_name = "Intervention 3", IRR = TRUE,
                    variables = c(), mean = TRUE, proportions_dp = 1, IR_dp = 1, mean_dp = 1, SD_dp = 1,
                    IRR_sf = 3, CI_sf = 3, save_image_path=NULL, save_docx_path=NULL){
  # change the column names
  dataset <- data %>%
    rename("body_system_class" = body_system_class, "id" = id, "arm" = arm, "date_rand" = date_rand,
           "last_visit" = last_visit)

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))
  stopifnot("date_rand variable type is not Date!" = is.Date(dataset[["date_rand"]]))
  stopifnot("last_visit variable type is not Date!" = is.Date(dataset[["last_visit"]]))

  # number of arm factor levels
  arm_number <- length(unique(dataset$arm))
  # recode arm factor
  # dataset <- dataset %>%
  #   mutate(
  #     arm =
  #       case_when(arm_number==2 ~ recode_factor(arm, arm1="A1", arm2="A2),
  #                 arm_number==3 ~ recode_factor(arm, arm1="A1", arm2="A2, arm3="A3"),
  #                 arm_number==4 ~ recode_factor(arm, arm1="A1", arm2="A2, arm3="A3", arm4="arm4")))
  dataset$arm <- as.character(dataset$arm)
  dataset$arm[which(dataset$arm==control)] <- "C"
  dataset$arm[which(dataset$arm==intervention1)] <- "I1"
  dataset$arm[which(dataset$arm==intervention2)] <- "I2"
  dataset$arm[which(dataset$arm==intervention3)] <- "I3"
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
    group_by(body_system_class, arm) %>%
    summarise(
      # number of participants with at least one adverse event for each body system class and arm
      Frequency = length(unique(id)),
      # total number of adverse events for each body system class and arm
      Events = sum(n),
      # total length of follow up time for each body system class and arm
      Total_Time = sum(follow_up_time),
      # mean number of adverse events for each participant for each body system class and arm
      Mean = round(mean(n), mean_dp),
      # standard deviation of number of adverse events for each participant for each body system class and arm
      SD = round(sd(n), SD_dp)) %>%
    mutate(
      # IR = Events / Total_Time
      IR = scales::percent(Events / Total_Time, 10^(-IR_dp)),
      # Proportions = Frequency / N
      Proportions =
        case_when(arm=="C" ~ scales::percent(Frequency / N0, 10^(-proportions_dp)),
                  arm=="I1" ~ scales::percent(Frequency / N1, 10^(-proportions_dp)),
                  arm=="I2" ~ scales::percent(Frequency / N2, 10^(-proportions_dp)),
                  arm=="I3" ~ scales::percent(Frequency / N3, 10^(-proportions_dp)))) %>%
    # mutate(
    #   # one decimal place for mean and SD
    #   across(c(Mean, SD), \(x) format(x, nsmall=1))) %>%
    mutate(
      # combine Frequency & Proportions, Events & IR, Mean & SD columns
      Frequency = str_glue("{Frequency} ({Proportions})"),
      Events = str_glue("{Events} ({IR})"),
      Mean = str_glue("{Mean} ({SD})")) %>%
    select(
      # drop Total_Time, SD, IR and Proportions from table
      -c(Total_Time, SD, IR, Proportions)) %>%
    pivot_wider(
      names_from = arm, values_from = c(Frequency, Events, Mean))

  # exclude IRR column if there are more than 2 arms in the table
  if (arm_number > 2){
    IRR <- FALSE
  }

  if (IRR==TRUE){
    crit_value <- qnorm(0.975, mean=0, sd=1)

    reg_df <- dataset %>%
      group_by(across(all_of(c(variables, "body_system_class", "id", "arm", "follow_up_time")))) %>%
      count()

    glm_func <- function(x){
      summary(glm(n ~ . - follow_up_time, offset=log(follow_up_time), family=poisson(link="log"),
                  data=reg_df %>%
                    filter(body_system_class==x) %>%
                    mutate(arm=relevel(arm, ref="C")) %>%
                    ungroup() %>%
                    select(all_of(c(variables, "arm", "follow_up_time", "n")))))$coefficients["armI1",c("Estimate",
                                                                                                        "Std. Error")]
    }

    glm_func_vect <- Vectorize(glm_func)

    Table1_IRR <- Table1 %>%
      rowwise() %>%
      mutate(
        Coef = list(possibly(glm_func_vect, otherwise=NA)(body_system_class)),
        Estimate = Coef[[1]][[1]],
        SE = ifelse(is.na(Estimate)==FALSE, Coef[[2]], NA),
        IRR = signif(exp(Estimate), IRR_sf),
        lower = signif(exp(Estimate - crit_value * SE), CI_sf),
        upper = signif(exp(Estimate + crit_value * SE), CI_sf),
        CI = str_c("(", lower, ", ", upper, ")")
      ) %>%
      select(-c(Coef, Estimate, SE, lower, upper))
  }

  #to produce nice table
  name1 <- "N" %p% subsc("1")
  name2 <- "N" %p% subsc("2")
  name3 <- "N" %p% subsc("3")
  name4 <- "N" %p% subsc("4")
  border <- fp_border_default(width=1.5)

  if (mean==TRUE){
    if (arm_number==2){
      if (IRR==TRUE){
        Table1_print <- Table1_IRR %>%
          select(body_system_class, Frequency_I1, Events_I1, Mean_I1, Frequency_C, Events_C, Mean_C, IRR, CI) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_I1="At least one event",
                       Events_I1="Number of events",
                       Mean_I1="Number of events",
                       Frequency_C="At least one event",
                       Events_C="Number of events",
                       Mean_C="Number of events",
                       IRR = "Treatment Effect Estimate (IRR)",
                       CI = "95% CI")) %>%
          add_header_row(
            values=c("", str_glue("{intervention1_name} ({name1}={N1})"), str_glue("{control_name} ({name2}={N0})"),
                     "Treatment Effect Estimate (IRR)", "95% CI"),
            colwidths = c(1, 3, 3, 1, 1)) %>%
          set_header_labels(
            ae_02="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)",
            Mean_I1="Mean number of events per participant (SD)", Frequency_C="N (%)",
            Events_C="n (IR)", Mean_C="Mean number of events per participant (SD)",
            IRR = "Treatment Effect Estimate (IRR)", CI = "95% CI") %>%
          merge_h(part="header") %>%
          merge_v(part="header") %>%
          flextable::align(align="center", j = c(2:9), part="all") %>%
          autofit() %>%
          width(j=c(1, 9), width=1.1) %>%
          width(j=c(2:8), width=0.75) %>%
          vline(j=c(1, 4, 7, 8), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(2, 5), border=border, part="header") %>%
          vline(i=3, j=c(3, 6), border=border, part="header") %>%
          vline(j=c(2, 3, 5, 6), border=border, part="body") %>%
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
            values=c("", str_glue("{intervention1_name} ({name1}={N1})"), str_glue("{control_name} ({name2}={N0})")),
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
          vline(j=c(1, 4), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(2, 5), border=border, part="header") %>%
          vline(i=3, j=c(3, 6), border=border, part="header") %>%
          vline(j=c(2, 3, 5, 6), border=border, part="body") %>%
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
          values=c("", str_glue("{intervention1_name} ({name1}={N1})"), str_glue("{intervention2_name} ({name2}={N2})"),
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
        vline(j=c(1, 4, 7), border=border, part="all") %>%
        vline(i=c(2, 3), j=c(2, 5, 8), border=border, part="header") %>%
        vline(i=3, j=c(3, 6, 9), border=border, part="header") %>%
        vline(j=c(2, 3, 5, 6, 8, 9), border=border, part="body") %>%
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
          values=c("", str_glue("{intervention1_name} ({name1}={N1})"), str_glue("{intervention2_name} ({name2}={N2})"),
                   str_glue("{intervention3_name} ({name3}={N3})"), str_glue("{control_name} ({name4}={N0})")),
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
        vline(j=c(1, 4, 7, 10), border=border, part="all") %>%
        vline(i=c(2, 3), j=c(2, 5, 8, 11), border=border, part="header") %>%
        vline(i=3, j=c(3, 6, 9, 12), border=border, part="header") %>%
        vline(j=c(2, 3, 5, 6, 8, 9, 11, 12), border=border, part="body") %>%
        bold(i=1, bold=TRUE, part="header") %>%
        bg(part="header", bg="gray80") %>%
        bg(part="body", bg="white") %>%
        fontsize(size=6.5, part="all")
    }
  } else{
    if (arm_number==2){
      if (IRR==TRUE){
        Table1_print <- Table1_IRR %>%
          select(body_system_class, Frequency_I1, Events_I1, Frequency_C, Events_C, IRR, CI) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_I1="At least one event",
                       Events_I1="Number of events",
                       Frequency_C="At least one event",
                       Events_C="Number of events",
                       IRR = "Treatment Effect Estimate (IRR)",
                       CI = "95% CI")) %>%
          add_header_row(
            values=c("", str_glue("{intervention1_name} ({name1}={N1})"), str_glue("{control_name} ({name2}={N0})"),
                     "Treatment Effect Estimate (IRR)", "95% CI"),
            colwidths = c(1, 2, 2, 1, 1)) %>%
          set_header_labels(
            ae_02="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)", Frequency_C="N (%)",
            Events_C="n (IR)", IRR = "Treatment Effect Estimate (IRR)", CI = "95% CI") %>%
          merge_h(part="header") %>%
          merge_v(part="header") %>%
          flextable::align(align="center", j = c(2:7), part="all") %>%
          autofit() %>%
          width(j=c(1, 7), width=1.1) %>%
          width(j=c(2:6), width=0.75) %>%
          vline(j=c(1, 3, 5, 6), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(2, 4), border=border, part="header") %>%
          vline(j=c(2, 4), border=border, part="body") %>%
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
            values=c("", str_glue("{intervention1_name} ({name1}={N1})"), str_glue("{control_name} ({name2}={N0})")),
            colwidths = c(1, 2, 2)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)",Frequency_C="N (%)",
            Events_C="n (IR)") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:5), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:5), width=0.75) %>%
          vline(j=c(1, 3), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(2, 4), border=border, part="header") %>%
          vline(j=c(2, 4), border=border, part="body") %>%
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
          values=c("", str_glue("{intervention1_name} ({name1}={N1})"), str_glue("{intervention2_name} ({name2}={N2})"),
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
        vline(j=c(1, 3, 5), border=border, part="all") %>%
        vline(i=c(2, 3), j=c(2, 4, 6), border=border, part="header") %>%
        vline(j=c(2, 4, 6), border=border, part="body") %>%
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
          values=c("", str_glue("{intervention1_name} ({name1}={N1})"), str_glue("{intervention2_name} ({name2}={N2})"),
                   str_glue("{intervention3_name} ({name3}={N3})"), str_glue("{control_name} ({name4}={N0})")),
          colwidths = c(1, 2, 2, 2, 2)) %>%
        set_header_labels(
          body_system_class="Body system class", Frequency_I1="N (%)", Events_I1="n (IR)", Frequency_I2="N (%)",
          Events_I2="n (IR)", Frequency_I3="N (%)", Events_I3="n (IR)", Frequency_C="N (%)",Events_C="n (IR)") %>%
        merge_h(part="header") %>%
        flextable::align(align="center", j = c(2:9), part="all") %>%
        autofit() %>%
        width(j=1, width=1.1) %>%
        width(j=c(2:9), width=0.75) %>%
        vline(j=c(1, 3, 5, 7), border=border, part="all") %>%
        vline(i=c(2, 3), j=c(2, 4, 6, 8), border=border, part="header") %>%
        vline(j=c(2, 4, 6, 8), border=border, part="body") %>%
        bold(i=1, bold=TRUE, part="header") %>%
        bg(part="header", bg="gray80") %>%
        bg(part="body", bg="white") %>%
        fontsize(size=6.5, part="all")
    }
  }

  plot(Table1_print)

  if (!is.null(save_image_path)){
    save_as_image(Table1_print, path=save_image_path)
  }

  if(!is.null(save_docx_path)){
    save_as_docx(Table1_print, path=save_docx_path)
  }
}
