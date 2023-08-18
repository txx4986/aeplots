#' Table of frequencies and proportions of events by severity categories
#'
#' @description
#' `aeseverity` is used to plot a table of AE summary by severity level and body system class for each arm using the following summary statistics:
#'
#' - **N**: Frequency (number of participants with at least one event for each severity level and body system class)
#' - **%**: Proportions (number of participants with at least one event for each severity level and body system class relative to number of participants at risk)
#' - **n**: Number of adverse events for each severity level and body system class
#'
#' Note that `aeseverity` can only take up to 5 severity levels for the severity variable.
#'
#' @param data data frame with adverse event, severity, id and arm columns
#' @param arm_levels vector of factor levels in arm variable
#' @param severity_levels vector of severity levels in ascending order if severity is not ordered factor
#' @param body_system_class name of body_system_class column
#' @param severity name of severity column
#' @param id name of id column
#' @param arm name of arm column
#' @param n_events a logical value whether to include number of events for each body system class and severity
#' @param arm_names vector of names for each arm in arm variable
#' @param severity_names vector of names for each severity in severity variable
#' @param proportions_dp number of decimal places for proportions
#' @param save_image_path file path to save table as image
#' @param save_docx_path file path to save table as docx
#'
#' @return flextable of frequencies and proportions of events by severity categories
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import flextable
#' @import common
#' @import stringr
#'
#' @export
#'
#' @examples
#' df2$aebodsys <- as.factor(df2$aebodsys)
#' df2$severity <- ordered(df2$severity, c("Mild", "Moderate", "Severe"))
#' aeseverity(df2, arm_levels=c("Intervention","Placebo"), body_system_class="aebodsys", proportions_dp=2)
aeseverity <- function(data, arm_levels, severity_levels=NULL, body_system_class="body_system_class",
                       severity="severity", id="id", arm="arm", n_events=TRUE, arm_names=NULL,
                       severity_names=NULL, proportions_dp=1, save_image_path=NULL, save_docx_path=NULL){
  # change the column names
  dataset <- data %>%
    rename("body_system_class"=body_system_class, "severity" = severity, "id" = id, "arm" = arm)

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))
  stopifnot("severity variable type is not factor!" = is.factor(dataset[["severity"]]))

  # checks if either severity variable is ordered or severity level is specified
  stopifnot("severity variable is not ordered or severity_levels is not specified!" = !(!is.ordered(dataset$severity) & is.null(severity_levels)))

  # checks if severity_levels can be found in severity variable
  stopifnot("severity levels specified cannot be found in arm column!" = severity_levels %in% dataset$severity)

  if (is.null(severity_levels)){
    severity_levels <- levels(dataset$severity)
  }

  if (is.null(severity_names)){
    severity_names <- severity_levels
  }

  # number of severity factor levels
  severity_number <- length(levels(dataset$severity))
  # checks if length of arm_levels equals to the number of arm factor levels
  stopifnot("length of severity_levels needs to be equal to the number of levels in severity!" = length(severity_levels)==severity_number)
  # checks if length of severity_names equals to the number of severity factor levels
  stopifnot("length of severity_names needs to be equal to the number of levels in severity!" = length(severity_names)==severity_number)

  # recode severity factor
  dataset$severity <- as.character(dataset$severity)
  dataset$severity[which(dataset$severity==severity_levels[1])] <- "S1"
  dataset$severity[which(dataset$severity==severity_levels[2])] <- "S2"
  dataset$severity[which(dataset$severity==severity_levels[3])] <- "S3"
  dataset$severity[which(dataset$severity==severity_levels[4])] <- "S4"
  dataset$severity[which(dataset$severity==severity_levels[5])] <- "S5"
  dataset$severity <- as.factor(dataset$severity)

  # checks if arm_levels can be found in arm variable
  stopifnot("arm levels specified cannot be found in arm column!" = arm_levels %in% dataset$arm)

  if (is.null(arm_names)){
    arm_names <- arm_levels
  }

  # number of arm factor levels
  arm_number <- length(unique(dataset$arm))
  # checks if length of arm_levels equals to the number of arm factor levels
  stopifnot("length of arm_levels needs to be equal to the number of levels in arm!" = length(arm_levels)==arm_number)
  # checks if length of arm_names equals to the number of arm factor levels
  stopifnot("length of arm_names needs to be equal to the number of levels in arm!" = length(arm_names)==arm_number)

  # recode arm factor
  dataset$arm <- as.character(dataset$arm)
  dataset$arm[which(dataset$arm==arm_levels[1])] <- "A1"
  dataset$arm[which(dataset$arm==arm_levels[2])] <- "A2"
  dataset$arm[which(dataset$arm==arm_levels[3])] <- "A3"
  dataset$arm[which(dataset$arm==arm_levels[4])] <- "A4"
  dataset$arm <- as.factor(dataset$arm)

  # number of participants at risk per arm
  N1 <- length(unique((dataset %>% filter(arm=="A1"))$id))
  N2 <- length(unique((dataset %>% filter(arm=="A2"))$id))
  N3 <- length(unique((dataset %>% filter(arm=="A3"))$id))
  N4 <- length(unique((dataset %>% filter(arm=="A4"))$id))

  options(dplyr.summarise.inform = FALSE)

  Table2 <- dataset %>%
    group_by(body_system_class, severity, arm) %>%
    summarise(
      Frequency=length(unique(id)), Events = sum(!is.na(id))) %>%
    mutate(
      Proportions =
        case_when(arm=="A1" ~ scales::percent(Frequency / N1, 10^(-proportions_dp)),
                  arm=="A2" ~ scales::percent(Frequency / N2, 10^(-proportions_dp)),
                  arm=="A3" ~ scales::percent(Frequency / N3, 10^(-proportions_dp)),
                  arm=="A4" ~ scales::percent(Frequency / N4, 10^(-proportions_dp))))

  #to produce nice table
  name1 <- "N" %p% subsc("1")
  name2 <- "N" %p% subsc("2")
  name3 <- "N" %p% subsc("3")
  name4 <- "N" %p% subsc("4")
  border <- fp_border_default(width=1.5)

  if (n_events==TRUE){
    Table2 <- Table2 %>%
      pivot_wider(
        names_from = arm, values_from = c(Frequency, Proportions, Events))
    if (arm_number==2){
      Table2_print <- Table2 %>%
        pivot_wider(
          names_from = severity, values_from = c(Frequency_A1, Proportions_A1, Events_A1, Frequency_A2,
                                                 Proportions_A2, Events_A2))
      if (severity_number==2){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A2_S1, Proportions_A2_S1, Events_A2_S1,
                 Frequency_A2_S2, Proportions_A2_S2, Events_A2_S2) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})")),
            colwidths = c(1, 6, 6)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A2_S1="N",
            Proportions_A2_S1="%", Events_A2_S1="n", Frequency_A2_S2="N", Proportions_A2_S2="%",
            Events_A2_S2="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:13), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:13), width=0.5) %>%
          vline(j=c(1, 7), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 10), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 8, 9, 10, 11, 12), border=border, part="body")
      } else if (severity_number==3){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A1_S3, Proportions_A1_S3, Events_A1_S3,
                 Frequency_A2_S1, Proportions_A2_S1, Events_A2_S1, Frequency_A2_S2, Proportions_A2_S2,
                 Events_A2_S2, Frequency_A2_S3, Proportions_A2_S3, Events_A2_S3) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Events_A1_S3=severity_names[3],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Events_A2_S3=severity_names[3])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})")),
            colwidths = c(1, 9, 9)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A1_S3="N",
            Proportions_A1_S3="%", Events_A1_S3="n", Frequency_A2_S1="N", Proportions_A2_S1="%", Events_A2_S1="n",
            Frequency_A2_S2="N", Proportions_A2_S2="%", Events_A2_S2="n", Frequency_A2_S3="N",
            Proportions_A2_S3="%", Events_A2_S3="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:19), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:19), width=0.5) %>%
          vline(j=c(1, 10), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 7, 13, 16), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18), border=border, part="body")
      } else if (severity_number==4){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A1_S3, Proportions_A1_S3, Events_A1_S3,
                 Frequency_A1_S4, Proportions_A1_S4, Events_A1_S4, Frequency_A2_S1, Proportions_A2_S1,
                 Events_A2_S1, Frequency_A2_S2, Proportions_A2_S2, Events_A2_S2, Frequency_A2_S3,
                 Proportions_A2_S3, Events_A2_S3, Frequency_A2_S4, Proportions_A2_S4, Events_A2_S4) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Events_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Events_A1_S4=severity_names[4],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Events_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Events_A2_S4=severity_names[4])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})")),
            colwidths = c(1, 12, 12)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A1_S3="N",
            Proportions_A1_S3="%", Events_A1_S3="n", Frequency_A1_S4="N", Proportions_A1_S4="%", Events_A1_S4="n",
            Frequency_A2_S1="N", Proportions_A2_S1="%", Events_A2_S1="n", Frequency_A2_S2="N",
            Proportions_A2_S2="%", Events_A2_S2="n", Frequency_A2_S3="N", Proportions_A2_S3="%", Events_A2_S3="n",
            Frequency_A2_S4="N", Proportions_A2_S4="%", Events_A2_S4="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:25), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:25), width=0.5) %>%
          vline(j=c(1, 13), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 7, 10, 16, 19, 22), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24), border=border,
                part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24),
                border=border, part="body")
      } else {
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A1_S3, Proportions_A1_S3, Events_A1_S3,
                 Frequency_A1_S4, Proportions_A1_S4, Events_A1_S4, Frequency_A1_S5, Proportions_A1_S5,
                 Events_A1_S5, Frequency_A2_S1, Proportions_A2_S1, Events_A2_S1, Frequency_A2_S2,
                 Proportions_A2_S2, Events_A2_S2, Frequency_A2_S3, Proportions_A2_S3, Events_A2_S3,
                 Frequency_A2_S4, Proportions_A2_S4, Events_A2_S4, Frequency_A2_S5, Proportions_A2_S5,
                 Events_A2_S5) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Events_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Events_A1_S4=severity_names[4],
                       Frequency_A1_S5=severity_names[5],
                       Proportions_A1_S5=severity_names[5],
                       Events_A1_S5=severity_names[5],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Events_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Events_A2_S4=severity_names[4],
                       Frequency_A2_S5=severity_names[5],
                       Proportions_A2_S5=severity_names[5],
                       Events_A2_S5=severity_names[5])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})")),
            colwidths = c(1, 15, 15)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A1_S3="N", Proportions_A1_S3="%",
            Frequency_A1_S4="N", Proportions_A1_S4="%", Frequency_A1_S5="N", Proportions_A1_S5="%",
            Frequency_A2_S1="N", Proportions_A2_S1="%", Frequency_A2_S2="N", Proportions_A2_S2="%",
            Frequency_A2_S3="N", Proportions_A2_S3="%", Frequency_A2_S4="N", Proportions_A2_S4="%",
            Frequency_A2_S5="N", Proportions_A2_S5="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:31), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:31), width=0.5) %>%
          vline(j=c(1, 16), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 7, 10, 13, 19, 22, 25, 28), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24, 26, 27, 29, 30), border=border,
                part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                    28, 29, 30), border=border, part="body")
      }

    } else if (arm_number==3){
      Table2_print <- Table2 %>%
        pivot_wider(
          names_from = severity, values_from = c(Frequency_A1, Proportions_A1, Events_A1, Frequency_A2,
                                                 Proportions_A2, Events_A2, Frequency_A3, Proportions_A3,
                                                 Events_A3))
      if (severity_number==2){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A2_S1, Proportions_A2_S1, Events_A2_S1,
                 Frequency_A2_S2, Proportions_A2_S2, Events_A2_S2, Frequency_A3_S1, Proportions_A3_S1,
                 Events_A3_S1, Frequency_A3_S2, Proportions_A3_S2, Events_A3_S2) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Events_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Events_A3_S2=severity_names[2])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})")), colwidths = c(1, 6, 6, 6)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A2_S1="N",
            Proportions_A2_S1="%", Events_A2_S1="n", Frequency_A2_S2="N", Proportions_A2_S2="%", Events_A2_S2="n",
            Frequency_A3_S1="N", Proportions_A3_S1="%", Events_A3_S1="n", Frequency_A3_S2="N",
            Proportions_A3_S2="%", Events_A3_S2="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:19), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:19), width=0.5) %>%
          vline(j=c(1, 7, 13), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 10, 16), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18), border=border, part="body")
      } else if (severity_number==3){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A1_S3, Proportions_A1_S3, Events_A1_S3,
                 Frequency_A2_S1, Proportions_A2_S1, Events_A2_S1, Frequency_A2_S2, Proportions_A2_S2,
                 Events_A2_S2, Frequency_A2_S3, Proportions_A2_S3, Events_A2_S3, Frequency_A3_S1,
                 Proportions_A3_S1, Events_A3_S1, Frequency_A3_S2, Proportions_A3_S2, Events_A3_S2,
                 Frequency_A3_S3, Proportions_A3_S3, Events_A3_S3) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Events_A1_S3=severity_names[3],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Events_A2_S3=severity_names[3],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Events_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Events_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Events_A3_S3=severity_names[3])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})")), colwidths = c(1, 9, 9, 9)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A1_S3="N",
            Proportions_A1_S3="%", Events_A1_S3="n", Frequency_A2_S1="N", Proportions_A2_S1="%", Events_A2_S1="n",
            Frequency_A2_S2="N", Proportions_A2_S2="%", Events_A2_S2="n", Frequency_A2_S3="N",
            Proportions_A2_S3="%", Events_A2_S3="n", Frequency_A3_S1="N", Proportions_A3_S1="%", Events_A3_S1="n",
            Frequency_A3_S2="N", Proportions_A3_S2="%", Events_A3_S2="n", Frequency_A3_S3="N",
            Proportions_A3_S3="%", Events_A3_S3="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:28), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:28), width=0.5) %>%
          vline(j=c(1, 10, 19), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 7, 13, 16, 22, 25), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24, 26, 27), border=border,
                part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27),
                border=border, part="body")
      } else if (severity_number==4){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A1_S3, Proportions_A1_S3, Events_A1_S3,
                 Frequency_A1_S4, Proportions_A1_S4, Events_A1_S4, Frequency_A2_S1, Proportions_A2_S1,
                 Events_A2_S1, Frequency_A2_S2, Proportions_A2_S2, Events_A2_S2, Frequency_A2_S3,
                 Proportions_A2_S3, Events_A2_S3, Frequency_A2_S4, Proportions_A2_S4, Events_A2_S4,
                 Frequency_A3_S1, Proportions_A3_S1, Events_A3_S1, Frequency_A3_S2, Proportions_A3_S2,
                 Events_A3_S2, Frequency_A3_S3, Proportions_A3_S3, Events_A3_S3, Frequency_A3_S4,
                 Proportions_A3_S4, Events_A3_S4) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Events_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Events_A1_S4=severity_names[4],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Events_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Events_A2_S4=severity_names[4],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Events_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Events_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Events_A3_S3=severity_names[3],
                       Frequency_A3_S4=severity_names[4],
                       Proportions_A3_S4=severity_names[4],
                       Events_A3_S4=severity_names[4])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})")), colwidths = c(1, 12, 12, 12)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A1_S3="N",
            Proportions_A1_S3="%", Events_A1_S3="n", Frequency_A1_S4="N", Proportions_A1_S4="%", Events_A1_S4="n",
            Frequency_A2_S1="N", Proportions_A2_S1="%", Events_A2_S1="n", Frequency_A2_S2="N",
            Proportions_A2_S2="%", Events_A2_S2="n", Frequency_A2_S3="N", Proportions_A2_S3="%", Events_A2_S3="n",
            Frequency_A2_S4="N", Proportions_A2_S4="%", Events_A2_S4="n", Frequency_A3_S1="N",
            Proportions_A3_S1="%", Events_A3_S1="n", Frequency_A3_S2="N", Proportions_A3_S2="%", Events_A3_S2="n",
            Frequency_A3_S3="N", Proportions_A3_S3="%", Events_A3_S3="n", Frequency_A3_S4="N",
            Proportions_A3_S4="%", Events_A3_S4="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:37), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:37), width=0.5) %>%
          vline(j=c(1, 13, 25), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 7, 10, 16, 19, 22, 28, 31, 34), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24, 26, 27, 29, 30, 32, 33, 35,
                         36), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27, 28,
                    29, 30, 31, 32, 33, 34, 35, 36), border=border, part="body")
      } else {
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A1_S3, Proportions_A1_S3, Events_A1_S3,
                 Frequency_A1_S4, Proportions_A1_S4, Events_A1_S4, Frequency_A1_S5, Proportions_A1_S5,
                 Events_A1_S5, Frequency_A2_S1, Proportions_A2_S1, Events_A2_S1, Frequency_A2_S2,
                 Proportions_A2_S2, Events_A2_S2, Frequency_A2_S3, Proportions_A2_S3, Events_A2_S3,
                 Frequency_A2_S4, Proportions_A2_S4, Events_A2_S4, Frequency_A2_S5, Proportions_A2_S5,
                 Events_A2_S5, Frequency_A3_S1, Proportions_A3_S1, Events_A3_S1, Frequency_A3_S2,
                 Proportions_A3_S2, Events_A3_S2, Frequency_A3_S3, Proportions_A3_S3, Events_A3_S3,
                 Frequency_A3_S4, Proportions_A3_S4, Events_A3_S4, Frequency_A3_S5, Proportions_A3_S5,
                 Events_A3_S5) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Events_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Events_A1_S4=severity_names[4],
                       Frequency_A1_S5=severity_names[5],
                       Proportions_A1_S5=severity_names[5],
                       Events_A1_S5=severity_names[5],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Events_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Events_A2_S4=severity_names[4],
                       Frequency_A2_S5=severity_names[5],
                       Proportions_A2_S5=severity_names[5],
                       Events_A2_S5=severity_names[5],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Events_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Events_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Events_A3_S3=severity_names[3],
                       Frequency_A3_S4=severity_names[4],
                       Proportions_A3_S4=severity_names[4],
                       Events_A3_S4=severity_names[4],
                       Frequency_A3_S5=severity_names[5],
                       Proportions_A3_S5=severity_names[5],
                       Events_A3_S5=severity_names[5])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})")), colwidths = c(1, 15, 15, 15)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A1_S3="N",
            Proportions_A1_S3="%", Events_A1_S3="n", Frequency_A1_S4="N", Proportions_A1_S4="%", Events_A1_S4="n",
            Frequency_A1_S5="N", Proportions_A1_S5="%", Events_A1_S5="n", Frequency_A2_S1="N",
            Proportions_A2_S1="%", Events_A2_S1="n", Frequency_A2_S2="N", Proportions_A2_S2="%", Events_A2_S2="n",
            Frequency_A2_S3="N", Proportions_A2_S3="%", Events_A2_S3="n", Frequency_A2_S4="N",
            Proportions_A2_S4="%", Events_A2_S4="n", Frequency_A2_S5="N", Proportions_A2_S5="%", Events_A2_S5="n",
            Frequency_A3_S1="N", Proportions_A3_S1="%", Events_A3_S1="n", Frequency_A3_S2="N",
            Proportions_A3_S2="%", Events_A3_S2="n", Frequency_A3_S3="N", Proportions_A3_S3="%", Events_A3_S3="n",
            Frequency_A3_S4="N", Proportions_A3_S4="%", Events_A3_S4="n", Frequency_A3_S5="N",
            Proportions_A3_S5="%", Events_A3_S5="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:46), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:46), width=0.5) %>%
          vline(j=c(1, 16, 31), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 7, 10, 13, 19, 22, 25, 28, 34, 37, 40, 43), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24, 26, 27, 29, 30, 32, 33, 35, 36,
                         38, 39, 41, 42, 44, 45), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                    28, 29, 30, 32, 33, 34, 35, 36, 37, 38, 39, 40), border=border, part="body")
      }

    } else {
      Table2_print <- Table2 %>%
        pivot_wider(
          names_from = severity, values_from = c(Frequency_A1, Proportions_A1, Events_A1, Frequency_A2,
                                                 Proportions_A2, Events_A2, Frequency_A3, Proportions_A3, Events_A3,
                                                 Frequency_A4, Proportions_A4, Events_A4))
      if (severity_number==2){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A2_S1, Proportions_A2_S1, Events_A2_S1,
                 Frequency_A2_S2, Proportions_A2_S2, Events_A2_S2, Frequency_A3_S1, Proportions_A3_S1,
                 Events_A3_S1, Frequency_A3_S2, Proportions_A3_S2, Events_A3_S2, Frequency_A4_S1,
                 Proportions_A4_S1, Events_A4_S1, Frequency_A4_S2, Proportions_A4_S2, Events_A4_S2) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Events_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Events_A3_S2=severity_names[2],
                       Frequency_A4_S1=severity_names[1],
                       Proportions_A4_S1=severity_names[1],
                       Events_A4_S1=severity_names[1],
                       Frequency_A4_S2=severity_names[2],
                       Proportions_A4_S2=severity_names[2],
                       Events_A4_S2=severity_names[2])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})"), str_glue("{arm_names[4]} ({name4}={N4})")),
            colwidths = c(1, 6, 6, 6, 6)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A2_S1="N",
            Proportions_A2_S1="%", Events_A2_S1="n", Frequency_A2_S2="N", Proportions_A2_S2="%", Events_A2_S2="n",
            Frequency_A3_S1="N", Proportions_A3_S1="%", Events_A3_S1="n", Frequency_A3_S2="N",
            Proportions_A3_S2="%", Events_A3_S2="n", Frequency_A4_S1="N", Proportions_A4_S1="%", Events_A4_S1="n",
            Frequency_A4_S2="N", Proportions_A4_S2="%", Events_A4_S2="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:25), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:25), width=0.5) %>%
          vline(j=c(1, 7, 13, 19), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 10, 16, 22), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24), border=border,
                part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 20, 21, 22, 23, 24), border=border,
                part="body")
      } else if (severity_number==3){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A1_S3, Proportions_A1_S3, Events_A1_S3,
                 Frequency_A2_S1, Proportions_A2_S1, Events_A2_S1, Frequency_A2_S2, Proportions_A2_S2,
                 Events_A2_S2, Frequency_A2_S3, Proportions_A2_S3, Events_A2_S3, Frequency_A3_S1,
                 Proportions_A3_S1, Events_A3_S1, Frequency_A3_S2, Proportions_A3_S2, Events_A3_S2,
                 Frequency_A3_S3, Proportions_A3_S3, Events_A3_S3, Frequency_A4_S1, Proportions_A4_S1,
                 Events_A4_S1, Frequency_A4_S2, Proportions_A4_S2, Events_A4_S2, Frequency_A4_S3,
                 Proportions_A4_S3, Events_A4_S3) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Events_A1_S3=severity_names[3],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Events_A2_S3=severity_names[3],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Events_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Events_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Events_A3_S3=severity_names[3],
                       Frequency_A4_S1=severity_names[1],
                       Proportions_A4_S1=severity_names[1],
                       Events_A4_S1=severity_names[1],
                       Frequency_A4_S2=severity_names[2],
                       Proportions_A4_S2=severity_names[2],
                       Events_A4_S2=severity_names[2],
                       Frequency_A4_S3=severity_names[3],
                       Proportions_A4_S3=severity_names[3],
                       Events_A4_S3=severity_names[3])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})"), str_glue("{arm_names[4]} ({name4}={N4})")),
            colwidths = c(1, 9, 9, 9, 9)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A1_S3="N",
            Proportions_A1_S3="%", Events_A1_S3="n", Frequency_A2_S1="N", Proportions_A2_S1="%", Events_A2_S1="n",
            Frequency_A2_S2="N", Proportions_A2_S2="%", Events_A2_S2="n", Frequency_A2_S3="N",
            Proportions_A2_S3="%", Events_A2_S3="n", Frequency_A3_S1="N", Proportions_A3_S1="%", Events_A3_S1="n",
            Frequency_A3_S2="N", Proportions_A3_S2="%", Events_A3_S2="n", Frequency_A3_S3="N",
            Proportions_A3_S3="%", Events_A3_S3="n", Frequency_A4_S1="N", Proportions_A4_S1="%", Events_A4_S1="n",
            Frequency_A4_S2="N", Proportions_A4_S2="%", Events_A4_S2="n", Frequency_A4_S3="N",
            Proportions_A4_S3="%", Events_A4_S3="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:37), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:37), width=0.5) %>%
          vline(j=c(1, 10, 19, 28), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 7, 13, 16, 22, 25, 31, 34), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24, 26, 27, 29, 30, 32, 33, 35,
                         36), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27, 29,
                    30, 31, 32, 33, 34, 35, 36), border=border,
                part="body")
      } else if (severity_number==4){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A1_S3, Proportions_A1_S3, Events_A1_S3,
                 Frequency_A1_S4, Proportions_A1_S4, Events_A1_S4, Frequency_A2_S1, Proportions_A2_S1,
                 Events_A2_S1, Frequency_A2_S2, Proportions_A2_S2, Events_A2_S2, Frequency_A2_S3,
                 Proportions_A2_S3, Events_A2_S3, Frequency_A2_S4, Proportions_A2_S4, Events_A2_S4,
                 Frequency_A3_S1, Proportions_A3_S1, Events_A3_S1, Frequency_A3_S2, Proportions_A3_S2,
                 Events_A3_S2, Frequency_A3_S3, Proportions_A3_S3, Events_A3_S3, Frequency_A3_S4,
                 Proportions_A3_S4, Events_A3_S4, Frequency_A4_S1, Proportions_A4_S1, Events_A4_S1,
                 Frequency_A4_S2, Proportions_A4_S2, Events_A4_S2, Frequency_A4_S3, Proportions_A4_S3,
                 Events_A4_S3, Frequency_A4_S4, Proportions_A4_S4, Events_A4_S4) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Events_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Events_A1_S4=severity_names[4],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Events_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Events_A2_S4=severity_names[4],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Events_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Events_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Events_A3_S3=severity_names[3],
                       Frequency_A3_S4=severity_names[4],
                       Proportions_A3_S4=severity_names[4],
                       Events_A3_S4=severity_names[4],
                       Frequency_A4_S1=severity_names[1],
                       Proportions_A4_S1=severity_names[1],
                       Events_A4_S1=severity_names[1],
                       Frequency_A4_S2=severity_names[2],
                       Proportions_A4_S2=severity_names[2],
                       Events_A4_S2=severity_names[2],
                       Frequency_A4_S3=severity_names[3],
                       Proportions_A4_S3=severity_names[3],
                       Events_A4_S3=severity_names[3],
                       Frequency_A4_S4=severity_names[4],
                       Proportions_A4_S4=severity_names[4],
                       Events_A4_S4=severity_names[4])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})"), str_glue("{arm_names[4]} ({name4}={N4})")),
            colwidths = c(1, 12, 12, 12, 12)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A1_S3="N",
            Proportions_A1_S3="%", Events_A1_S3="n", Frequency_A1_S4="N", Proportions_A1_S4="%", Events_A1_S4="n",
            Frequency_A2_S1="N", Proportions_A2_S1="%", Events_A2_S1="n", Frequency_A2_S2="N",
            Proportions_A2_S2="%", Events_A2_S2="n", Frequency_A2_S3="N", Proportions_A2_S3="%", Events_A2_S3="n",
            Frequency_A2_S4="N", Proportions_A2_S4="%", Events_A2_S4="n", Frequency_A3_S1="N",
            Proportions_A3_S1="%", Events_A3_S1="n", Frequency_A3_S2="N", Proportions_A3_S2="%", Events_A3_S2="n",
            Frequency_A3_S3="N", Proportions_A3_S3="%", Events_A3_S3="n", Frequency_A3_S4="N",
            Proportions_A3_S4="%", Events_A3_S4="n", Frequency_A4_S1="N", Proportions_A4_S1="%", Events_A4_S1="n",
            Frequency_A4_S2="N", Proportions_A4_S2="%", Events_A4_S2="n", Frequency_A4_S3="N",
            Proportions_A4_S3="%", Events_A4_S3="n", Frequency_A4_S4="N", Proportions_A4_S4="%",
            Events_A4_S4="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:49), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:49), width=0.5) %>%
          vline(j=c(1, 13, 25, 37), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 7, 10, 16, 19, 22, 28, 31, 34, 40, 43, 46), border=border, part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24, 26, 27, 29, 30, 32, 33, 35, 36,
                         38, 39, 41, 42, 44, 45, 47, 48), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27, 28,
                    29, 30, 31, 32, 33, 34, 35, 36, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48), border=border,
                part="body")
      } else {
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Events_A1_S1, Frequency_A1_S2,
                 Proportions_A1_S2, Events_A1_S2, Frequency_A1_S3, Proportions_A1_S3, Events_A1_S3,
                 Frequency_A1_S4, Proportions_A1_S4, Events_A1_S4, Frequency_A1_S5, Proportions_A1_S5,
                 Events_A1_S5, Frequency_A2_S1, Proportions_A2_S1, Events_A2_S1, Frequency_A2_S2,
                 Proportions_A2_S2, Events_A2_S2, Frequency_A2_S3, Proportions_A2_S3, Events_A2_S3,
                 Frequency_A2_S4, Proportions_A2_S4, Events_A2_S4, Frequency_A2_S5, Proportions_A2_S5,
                 Events_A2_S5, Frequency_A3_S1, Proportions_A3_S1, Events_A3_S1, Frequency_A3_S2,
                 Proportions_A3_S2, Events_A3_S2, Frequency_A3_S3, Proportions_A3_S3, Events_A3_S3,
                 Frequency_A3_S4, Proportions_A3_S4, Events_A3_S4, Frequency_A3_S5, Proportions_A3_S5,
                 Events_A3_S5, Frequency_A4_S1, Proportions_A4_S1, Events_A4_S1, Frequency_A4_S2,
                 Proportions_A4_S2, Events_A4_S2, Frequency_A4_S3, Proportions_A4_S3, Events_A4_S3,
                 Frequency_A4_S4, Proportions_A4_S4, Events_A4_S4, Frequency_A4_S5, Proportions_A4_S5,
                 Events_A4_S5) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Events_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Events_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Events_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Events_A1_S4=severity_names[4],
                       Frequency_A1_S5=severity_names[5],
                       Proportions_A1_S5=severity_names[5],
                       Events_A1_S5=severity_names[5],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Events_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Events_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Events_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Events_A2_S4=severity_names[4],
                       Frequency_A2_S5=severity_names[5],
                       Proportions_A2_S5=severity_names[5],
                       Events_A2_S5=severity_names[5],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Events_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Events_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Events_A3_S3=severity_names[3],
                       Frequency_A3_S4=severity_names[4],
                       Proportions_A3_S4=severity_names[4],
                       Events_A3_S4=severity_names[4],
                       Frequency_A3_S5=severity_names[5],
                       Proportions_A3_S5=severity_names[5],
                       Events_A3_S5=severity_names[5],
                       Frequency_A4_S1=severity_names[1],
                       Proportions_A4_S1=severity_names[1],
                       Events_A4_S1=severity_names[1],
                       Frequency_A4_S2=severity_names[2],
                       Proportions_A4_S2=severity_names[2],
                       Events_A4_S2=severity_names[2],
                       Frequency_A4_S3=severity_names[3],
                       Proportions_A4_S3=severity_names[3],
                       Events_A4_S3=severity_names[3],
                       Frequency_A4_S4=severity_names[4],
                       Proportions_A4_S4=severity_names[4],
                       Events_A4_S4=severity_names[4],
                       Frequency_A4_S5=severity_names[5],
                       Proportions_A4_S5=severity_names[5],
                       Events_A4_S5=severity_names[5])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})"), str_glue("{arm_names[4]} ({name4}={N4})")),
            colwidths = c(1, 15, 15, 15, 15)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%", Events_A1_S1="n",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Events_A1_S2="n", Frequency_A1_S3="N",
            Proportions_A1_S3="%", Events_A1_S3="n", Frequency_A1_S4="N", Proportions_A1_S4="%", Events_A1_S4="n",
            Frequency_A1_S5="N", Proportions_A1_S5="%", Events_A1_S5="n", Frequency_A2_S1="N",
            Proportions_A2_S1="%", Events_A2_S1="n", Frequency_A2_S2="N", Proportions_A2_S2="%", Events_A2_S2="n",
            Frequency_A2_S3="N", Proportions_A2_S3="%", Events_A2_S3="n", Frequency_A2_S4="N",
            Proportions_A2_S4="%", Events_A2_S4="n", Frequency_A2_S5="N", Proportions_A2_S5="%", Events_A2_S5="n",
            Frequency_A3_S1="N", Proportions_A3_S1="%", Events_A3_S1="n", Frequency_A3_S2="N",
            Proportions_A3_S2="%", Events_A3_S2="n", Frequency_A3_S3="N", Proportions_A3_S3="%", Events_A3_S3="n",
            Frequency_A3_S4="N", Proportions_A3_S4="%", Events_A3_S4="n", Frequency_A3_S5="N",
            Proportions_A3_S5="%", Events_A3_S5="n",Frequency_A4_S1="N", Proportions_A4_S1="%", Events_A4_S1="n",
            Frequency_A4_S2="N", Proportions_A4_S2="%", Events_A4_S2="n", Frequency_A4_S3="N",
            Proportions_A4_S3="%", Events_A4_S3="n", Frequency_A4_S4="N", Proportions_A4_S4="%", Events_A4_S4="n",
            Frequency_A4_S5="N", Proportions_A4_S5="%", Events_A4_S5="n") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:61), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:61), width=0.5) %>%
          vline(j=c(1, 16, 31, 46), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(4, 7, 10, 13, 19, 22, 25, 28, 34, 37, 40, 43, 49, 52, 55, 58), border=border,
                part="header") %>%
          vline(i=3, j=c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24, 26, 27, 29, 30, 32, 33, 35, 36,
                         38, 39, 41, 42, 44, 45, 47, 48, 50, 51, 53, 54, 56, 57, 59, 60), border=border,
                part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                    28, 29, 30, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 47, 48, 49, 50, 51, 52,
                    53, 54, 55, 56, 57, 58, 59, 60), border=border, part="body")
      }
    }
  }else{
    Table2 <- Table2 %>%
      select(-c(Events)) %>%
      pivot_wider(
        names_from = arm, values_from = c(Frequency, Proportions))
    if (arm_number==2){
      Table2_print <- Table2 %>%
        pivot_wider(
          names_from = severity, values_from = c(Frequency_A1, Proportions_A1, Frequency_A2, Proportions_A2))
      if (severity_number==2){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A2_S1, Proportions_A2_S1, Frequency_A2_S2, Proportions_A2_S2) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})")),
            colwidths = c(1, 4, 4)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A2_S1="N", Proportions_A2_S1="%",
            Frequency_A2_S2="N", Proportions_A2_S2="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:9), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:9), width=0.5) %>%
          vline(j=c(1, 5), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 7), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 6, 7, 8), border=border, part="body")
      } else if (severity_number==3){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A1_S3, Proportions_A1_S3, Frequency_A2_S1, Proportions_A2_S1, Frequency_A2_S2,
                 Proportions_A2_S2, Frequency_A2_S3, Proportions_A2_S3) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})")),
            colwidths = c(1, 6, 6)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A1_S3="N", Proportions_A1_S3="%",
            Frequency_A2_S1="N", Proportions_A2_S1="%", Frequency_A2_S2="N", Proportions_A2_S2="%",
            Frequency_A2_S3="N", Proportions_A2_S3="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:13), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:13), width=0.5) %>%
          vline(j=c(1, 7), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 5, 9, 11), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 8, 9, 10, 11, 12), border=border, part="body")
      } else if (severity_number==4){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A1_S3, Proportions_A1_S3, Frequency_A1_S4, Proportions_A1_S4, Frequency_A2_S1,
                 Proportions_A2_S1, Frequency_A2_S2, Proportions_A2_S2, Frequency_A2_S3, Proportions_A2_S3,
                 Frequency_A2_S4, Proportions_A2_S4) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})")),
            colwidths = c(1, 8, 8)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A1_S3="N", Proportions_A1_S3="%",
            Frequency_A1_S4="N", Proportions_A1_S4="%", Frequency_A2_S1="N", Proportions_A2_S1="%",
            Frequency_A2_S2="N", Proportions_A2_S2="%", Frequency_A2_S3="N", Proportions_A2_S3="%",
            Frequency_A2_S4="N", Proportions_A2_S4="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:17), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:17), width=0.5) %>%
          vline(j=c(1, 9), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 5, 7, 11, 13, 15), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12, 14, 16), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16), border=border, part="body")
      } else {
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A1_S3, Proportions_A1_S3, Frequency_A1_S4, Proportions_A1_S4, Frequency_A1_S5,
                 Proportions_A1_S5, Frequency_A2_S1, Proportions_A2_S1, Frequency_A2_S2, Proportions_A2_S2,
                 Frequency_A2_S3, Proportions_A2_S3, Frequency_A2_S4, Proportions_A2_S4, Frequency_A2_S5,
                 Proportions_A2_S5) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Frequency_A1_S5=severity_names[5],
                       Proportions_A1_S5=severity_names[5],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Frequency_A2_S5=severity_names[5],
                       Proportions_A2_S5=severity_names[5])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})")),
            colwidths = c(1, 10, 10)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A1_S3="N", Proportions_A1_S3="%",
            Frequency_A1_S4="N", Proportions_A1_S4="%", Frequency_A1_S5="N", Proportions_A1_S5="%",
            Frequency_A2_S1="N", Proportions_A2_S1="%", Frequency_A2_S2="N", Proportions_A2_S2="%",
            Frequency_A2_S3="N", Proportions_A2_S3="%", Frequency_A2_S4="N", Proportions_A2_S4="%",
            Frequency_A2_S5="N", Proportions_A2_S5="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:21), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:21), width=0.5) %>%
          vline(j=c(1, 11), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 5, 7, 9, 13, 15, 17, 19), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20), border=border, part="body")
      }

    } else if (arm_number==3){
      Table2_print <- Table2 %>%
        pivot_wider(
          names_from = severity, values_from = c(Frequency_A1, Proportions_A1, Frequency_A2, Proportions_A2,
                                                 Frequency_A3, Proportions_A3))
      if (severity_number==2){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A2_S1, Proportions_A2_S1, Frequency_A2_S2, Proportions_A2_S2, Frequency_A3_S1,
                 Proportions_A3_S1, Frequency_A3_S2, Proportions_A3_S2) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})")), colwidths = c(1, 4, 4, 4)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A2_S1="N", Proportions_A2_S1="%",
            Frequency_A2_S2="N", Proportions_A2_S2="%", Frequency_A3_S1="N", Proportions_A3_S1="%",
            Frequency_A3_S2="N", Proportions_A3_S2="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:13), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:13), width=0.5) %>%
          vline(j=c(1, 5, 9), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 7, 11), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 6, 7, 8, 10, 11, 12), border=border, part="body")
      } else if (severity_number==3){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A1_S3, Proportions_A1_S3, Frequency_A2_S1, Proportions_A2_S1, Frequency_A2_S2,
                 Proportions_A2_S2, Frequency_A2_S3, Proportions_A2_S3, Frequency_A3_S1, Proportions_A3_S1,
                 Frequency_A3_S2, Proportions_A3_S2, Frequency_A3_S3, Proportions_A3_S3) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})")), colwidths = c(1, 6, 6, 6)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A1_S3="N", Proportions_A1_S3="%",
            Frequency_A2_S1="N", Proportions_A2_S1="%", Frequency_A2_S2="N", Proportions_A2_S2="%",
            Frequency_A2_S3="N", Proportions_A2_S3="%", Frequency_A3_S1="N", Proportions_A3_S1="%",
            Frequency_A3_S2="N", Proportions_A3_S2="%", Frequency_A3_S3="N", Proportions_A3_S3="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:19), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:19), width=0.5) %>%
          vline(j=c(1, 7, 13), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 5, 9, 11, 15, 17), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12, 14, 16, 18), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18), border=border, part="body")
      } else if (severity_number==4){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A1_S3, Proportions_A1_S3, Frequency_A1_S4, Proportions_A1_S4, Frequency_A2_S1,
                 Proportions_A2_S1, Frequency_A2_S2, Proportions_A2_S2, Frequency_A2_S3, Proportions_A2_S3,
                 Frequency_A2_S4, Proportions_A2_S4, Frequency_A3_S1, Proportions_A3_S1, Frequency_A3_S2,
                 Proportions_A3_S2, Frequency_A3_S3, Proportions_A3_S3, Frequency_A3_S4, Proportions_A3_S4) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Frequency_A3_S4=severity_names[4],
                       Proportions_A3_S4=severity_names[4])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})")), colwidths = c(1, 8, 8, 8)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A1_S3="N", Proportions_A1_S3="%",
            Frequency_A1_S4="N", Proportions_A1_S4="%", Frequency_A2_S1="N", Proportions_A2_S1="%",
            Frequency_A2_S2="N", Proportions_A2_S2="%", Frequency_A2_S3="N", Proportions_A2_S3="%",
            Frequency_A2_S4="N", Proportions_A2_S4="%", Frequency_A3_S1="N", Proportions_A3_S1="%",
            Frequency_A3_S2="N", Proportions_A3_S2="%", Frequency_A3_S3="N", Proportions_A3_S3="%",
            Frequency_A3_S4="N", Proportions_A3_S4="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:25), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:25), width=0.5) %>%
          vline(j=c(1, 9, 17), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 5, 7, 11, 13, 15, 19, 21, 23), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 21, 22, 23, 24), border=border,
                part="body")
      } else {
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A1_S3, Proportions_A1_S3, Frequency_A1_S4, Proportions_A1_S4, Frequency_A1_S5,
                 Proportions_A1_S5, Frequency_A2_S1, Proportions_A2_S1, Frequency_A2_S2, Proportions_A2_S2,
                 Frequency_A2_S3, Proportions_A2_S3, Frequency_A2_S4, Proportions_A2_S4, Frequency_A2_S5,
                 Proportions_A2_S5, Frequency_A3_S1, Proportions_A3_S1, Frequency_A3_S2, Proportions_A3_S2,
                 Frequency_A3_S3, Proportions_A3_S3, Frequency_A3_S4, Proportions_A3_S4, Frequency_A3_S5,
                 Proportions_A3_S5) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Frequency_A1_S5=severity_names[5],
                       Proportions_A1_S5=severity_names[5],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Frequency_A2_S5=severity_names[5],
                       Proportions_A2_S5=severity_names[5],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Frequency_A3_S4=severity_names[4],
                       Proportions_A3_S4=severity_names[4],
                       Frequency_A3_S5=severity_names[5],
                       Proportions_A3_S5=severity_names[5])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})")), colwidths = c(1, 10, 10, 10)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A1_S3="N", Proportions_A1_S3="%",
            Frequency_A1_S4="N", Proportions_A1_S4="%", Frequency_A1_S5="N", Proportions_A1_S5="%",
            Frequency_A2_S1="N", Proportions_A2_S1="%", Frequency_A2_S2="N", Proportions_A2_S2="%",
            Frequency_A2_S3="N", Proportions_A2_S3="%", Frequency_A2_S4="N", Proportions_A2_S4="%",
            Frequency_A2_S5="N", Proportions_A2_S5="%", Frequency_A3_S1="N", Proportions_A3_S1="%",
            Frequency_A3_S2="N", Proportions_A3_S2="%", Frequency_A3_S3="N", Proportions_A3_S3="%",
            Frequency_A3_S4="N", Proportions_A3_S4="%", Frequency_A3_S5="N", Proportions_A3_S5="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:31), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:31), width=0.5) %>%
          vline(j=c(1, 11, 21), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 5, 7, 9, 13, 15, 17, 19, 23, 25, 27, 29), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30), border=border,
                part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 23, 24, 25, 26, 27, 28,
                    29, 30), border=border, part="body")
      }

    } else {
      Table2_print <- Table2 %>%
        pivot_wider(
          names_from = severity, values_from = c(Frequency_A1, Proportions_A1, Frequency_A2, Proportions_A2,
                                                 Frequency_A3, Proportions_A3, Frequency_A4, Proportions_A4))
      if (severity_number==2){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A2_S1, Proportions_A2_S1, Frequency_A2_S2, Proportions_A2_S2, Frequency_A3_S1,
                 Proportions_A3_S1, Frequency_A3_S2, Proportions_A3_S2, Frequency_A4_S1, Proportions_A4_S1,
                 Frequency_A4_S2, Proportions_A4_S2) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Frequency_A4_S1=severity_names[1],
                       Proportions_A4_S1=severity_names[1],
                       Frequency_A4_S2=severity_names[2],
                       Proportions_A4_S2=severity_names[2])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})"), str_glue("{arm_names[4]} ({name4}={N4})")),
            colwidths = c(1, 4, 4, 4, 4)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A2_S1="N", Proportions_A2_S1="%",
            Frequency_A2_S2="N", Proportions_A2_S2="%", Frequency_A3_S1="N", Proportions_A3_S1="%",
            Frequency_A3_S2="N", Proportions_A3_S2="%", Frequency_A4_S1="N", Proportions_A4_S1="%",
            Frequency_A4_S2="N", Proportions_A4_S2="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:17), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:17), width=0.5) %>%
          vline(j=c(1, 5, 9, 13), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 7, 11, 15), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12, 14, 16), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 6, 7, 8, 10, 11, 12, 14, 15, 16), border=border, part="body")
      } else if (severity_number==3){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A1_S3, Proportions_A1_S3, Frequency_A2_S1, Proportions_A2_S1, Frequency_A2_S2,
                 Proportions_A2_S2, Frequency_A2_S3, Proportions_A2_S3, Frequency_A3_S1, Proportions_A3_S1,
                 Frequency_A3_S2, Proportions_A3_S2, Frequency_A3_S3, Proportions_A3_S3, Frequency_A4_S1,
                 Proportions_A4_S1, Frequency_A4_S2, Proportions_A4_S2, Frequency_A4_S3, Proportions_A4_S3) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Frequency_A4_S1=severity_names[1],
                       Proportions_A4_S1=severity_names[1],
                       Frequency_A4_S2=severity_names[2],
                       Proportions_A4_S2=severity_names[2],
                       Frequency_A4_S3=severity_names[3],
                       Proportions_A4_S3=severity_names[3])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})"), str_glue("{arm_names[4]} ({name4}={N4})")),
            colwidths = c(1, 6, 6, 6, 6)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A1_S3="N", Proportions_A1_S3="%",
            Frequency_A2_S1="N", Proportions_A2_S1="%", Frequency_A2_S2="N", Proportions_A2_S2="%",
            Frequency_A2_S3="N", Proportions_A2_S3="%", Frequency_A3_S1="N", Proportions_A3_S1="%",
            Frequency_A3_S2="N", Proportions_A3_S2="%", Frequency_A3_S3="N", Proportions_A3_S3="%",
            Frequency_A4_S1="N", Proportions_A4_S1="%", Frequency_A4_S2="N", Proportions_A4_S2="%",
            Frequency_A4_S3="N", Proportions_A4_S3="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:25), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:25), width=0.5) %>%
          vline(j=c(1, 7, 13, 19), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 5, 9, 11, 15, 17, 21, 23), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24), border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 20, 21, 22, 23, 24), border=border,
                part="body")
      } else if (severity_number==4){
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A1_S3, Proportions_A1_S3, Frequency_A1_S4, Proportions_A1_S4, Frequency_A2_S1,
                 Proportions_A2_S1, Frequency_A2_S2, Proportions_A2_S2, Frequency_A2_S3, Proportions_A2_S3,
                 Frequency_A2_S4, Proportions_A2_S4, Frequency_A3_S1, Proportions_A3_S1, Frequency_A3_S2,
                 Proportions_A3_S2, Frequency_A3_S3, Proportions_A3_S3, Frequency_A3_S4, Proportions_A3_S4,
                 Frequency_A4_S1, Proportions_A4_S1, Frequency_A4_S2, Proportions_A4_S2, Frequency_A4_S3,
                 Proportions_A4_S3, Frequency_A4_S4, Proportions_A4_S4) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Frequency_A3_S4=severity_names[4],
                       Proportions_A3_S4=severity_names[4],
                       Frequency_A4_S1=severity_names[1],
                       Proportions_A4_S1=severity_names[1],
                       Frequency_A4_S2=severity_names[2],
                       Proportions_A4_S2=severity_names[2],
                       Frequency_A4_S3=severity_names[3],
                       Proportions_A4_S3=severity_names[3],
                       Frequency_A4_S4=severity_names[4],
                       Proportions_A4_S4=severity_names[4])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})"), str_glue("{arm_names[4]} ({name4}={N4})")),
            colwidths = c(1, 8, 8, 8, 8)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A1_S3="N", Proportions_A1_S3="%",
            Frequency_A1_S4="N", Proportions_A1_S4="%", Frequency_A2_S1="N", Proportions_A2_S1="%",
            Frequency_A2_S2="N", Proportions_A2_S2="%", Frequency_A2_S3="N", Proportions_A2_S3="%",
            Frequency_A2_S4="N", Proportions_A2_S4="%", Frequency_A3_S1="N", Proportions_A3_S1="%",
            Frequency_A3_S2="N", Proportions_A3_S2="%", Frequency_A3_S3="N", Proportions_A3_S3="%",
            Frequency_A3_S4="N", Proportions_A3_S4="%", Frequency_A4_S1="N", Proportions_A4_S1="%",
            Frequency_A4_S2="N", Proportions_A4_S2="%", Frequency_A4_S3="N", Proportions_A4_S3="%",
            Frequency_A4_S4="N", Proportions_A4_S4="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:33), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:33), width=0.5) %>%
          vline(j=c(1, 9, 17, 25), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 5, 7, 11, 13, 15, 19, 21, 23, 27, 29, 31), border=border, part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32), border=border,
                part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 21, 22, 23, 24, 26, 27, 28, 29,
                    30, 31, 32), border=border, part="body")
      } else {
        Table2_print <- Table2_print %>%
          select(body_system_class, Frequency_A1_S1, Proportions_A1_S1, Frequency_A1_S2, Proportions_A1_S2,
                 Frequency_A1_S3, Proportions_A1_S3, Frequency_A1_S4, Proportions_A1_S4, Frequency_A1_S5,
                 Proportions_A1_S5, Frequency_A2_S1, Proportions_A2_S1, Frequency_A2_S2, Proportions_A2_S2,
                 Frequency_A2_S3, Proportions_A2_S3, Frequency_A2_S4, Proportions_A2_S4, Frequency_A2_S5,
                 Proportions_A2_S5, Frequency_A3_S1, Proportions_A3_S1, Frequency_A3_S2, Proportions_A3_S2,
                 Frequency_A3_S3, Proportions_A3_S3, Frequency_A3_S4, Proportions_A3_S4, Frequency_A3_S5,
                 Proportions_A3_S5, Frequency_A4_S1, Proportions_A4_S1, Frequency_A4_S2, Proportions_A4_S2,
                 Frequency_A4_S3, Proportions_A4_S3, Frequency_A4_S4, Proportions_A4_S4, Frequency_A4_S5,
                 Proportions_A4_S5) %>%
          flextable() %>%
          add_header(
            values = c(Frequency_A1_S1=severity_names[1],
                       Proportions_A1_S1=severity_names[1],
                       Frequency_A1_S2=severity_names[2],
                       Proportions_A1_S2=severity_names[2],
                       Frequency_A1_S3=severity_names[3],
                       Proportions_A1_S3=severity_names[3],
                       Frequency_A1_S4=severity_names[4],
                       Proportions_A1_S4=severity_names[4],
                       Frequency_A1_S5=severity_names[5],
                       Proportions_A1_S5=severity_names[5],
                       Frequency_A2_S1=severity_names[1],
                       Proportions_A2_S1=severity_names[1],
                       Frequency_A2_S2=severity_names[2],
                       Proportions_A2_S2=severity_names[2],
                       Frequency_A2_S3=severity_names[3],
                       Proportions_A2_S3=severity_names[3],
                       Frequency_A2_S4=severity_names[4],
                       Proportions_A2_S4=severity_names[4],
                       Frequency_A2_S5=severity_names[5],
                       Proportions_A2_S5=severity_names[5],
                       Frequency_A3_S1=severity_names[1],
                       Proportions_A3_S1=severity_names[1],
                       Frequency_A3_S2=severity_names[2],
                       Proportions_A3_S2=severity_names[2],
                       Frequency_A3_S3=severity_names[3],
                       Proportions_A3_S3=severity_names[3],
                       Frequency_A3_S4=severity_names[4],
                       Proportions_A3_S4=severity_names[4],
                       Frequency_A3_S4=severity_names[5],
                       Proportions_A3_S4=severity_names[5],
                       Frequency_A4_S1=severity_names[1],
                       Proportions_A4_S1=severity_names[1],
                       Frequency_A4_S2=severity_names[2],
                       Proportions_A4_S2=severity_names[2],
                       Frequency_A4_S3=severity_names[3],
                       Proportions_A4_S3=severity_names[3],
                       Frequency_A4_S4=severity_names[4],
                       Proportions_A4_S4=severity_names[4],
                       Frequency_A4_S5=severity_names[5],
                       Proportions_A4_S5=severity_names[5])) %>%
          add_header_row(
            values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                     str_glue("{arm_names[3]} ({name3}={N3})"), str_glue("{arm_names[4]} ({name4}={N4})")),
            colwidths = c(1, 10, 10, 10, 10)) %>%
          set_header_labels(
            body_system_class="Body system class", Frequency_A1_S1="N", Proportions_A1_S1="%",
            Frequency_A1_S2="N", Proportions_A1_S2="%", Frequency_A1_S3="N", Proportions_A1_S3="%",
            Frequency_A1_S4="N", Proportions_A1_S4="%", Frequency_A1_S5="N", Proportions_A1_S5="%",
            Frequency_A2_S1="N", Proportions_A2_S1="%", Frequency_A2_S2="N", Proportions_A2_S2="%",
            Frequency_A2_S3="N", Proportions_A2_S3="%", Frequency_A2_S4="N", Proportions_A2_S4="%",
            Frequency_A2_S5="N", Proportions_A2_S5="%", Frequency_A3_S1="N", Proportions_A3_S1="%",
            Frequency_A3_S2="N", Proportions_A3_S2="%", Frequency_A3_S3="N", Proportions_A3_S3="%",
            Frequency_A3_S4="N", Proportions_A3_S4="%", Frequency_A3_S5="N", Proportions_A3_S5="%",
            Frequency_A4_S1="N", Proportions_A4_S1="%", Frequency_A4_S2="N", Proportions_A4_S2="%",
            Frequency_A4_S3="N", Proportions_A4_S3="%", Frequency_A4_S4="N", Proportions_A4_S4="%",
            Frequency_A4_S5="N", Proportions_A4_S5="%") %>%
          merge_h(part="header") %>%
          flextable::align(align="center", j = c(2:41), part="all") %>%
          autofit() %>%
          width(j=1, width=1.1) %>%
          width(j=c(2:41), width=0.5) %>%
          vline(j=c(1, 11, 21, 31), border=border, part="all") %>%
          vline(i=c(2, 3), j=c(3, 5, 7, 9, 13, 15, 17, 19, 23, 25, 27, 29, 33, 35, 37, 39), border=border,
                part="header") %>%
          vline(i=3, j=c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40),
                border=border, part="header") %>%
          vline(j=c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 23, 24, 25, 26, 27, 28,
                    29, 30, 32, 33, 34, 35, 36, 37, 38, 39, 40), border=border, part="body")
      }
    }
  }

  Table2_print <- Table2_print %>%
    bold(i=1, bold=TRUE, part="header") %>%
    bg(part="header", bg="gray80") %>%
    bg(part="body", bg="white") %>%
    fontsize(size=6.5, part="all")

  plot(Table2_print)

  if (!is.null(save_image_path)){
    save_as_image(Table2_print, path=save_image_path)
  }

  if(!is.null(save_docx_path)){
    save_as_docx(Table2_print, path=save_docx_path)
  }
}
