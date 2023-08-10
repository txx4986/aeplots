#' Table of frequencies and proportions of events by severity categories
#'
#' @param data data frame with adverse event, severity, id and arm columns
#' @param arm_levels vector of factor levels in arm variable
#' @param severity name of severity column
#' @param id name of id column
#' @param arm name of arm column
#' @param arm_names vector of names for each arm in arm variable
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
#' df2$severity <- ordered(df2$severity, c("Mild", "Moderate", "Severe"))
#' aeseverity(df2, arm_levels=c("Intervention","Placebo"), proportions_dp=2)
aeseverity <- function(data, arm_levels, severity="severity", id="id", arm="arm", arm_names=NULL,
                       proportions_dp=1, save_image_path=NULL, save_docx_path=NULL){
  # change the column names
  dataset <- data %>%
    rename("severity" = severity, "id" = id, "arm" = arm)

  # checks if the variable type for each column is correct
  stopifnot("severity variable type is not factor!" = is.factor(dataset[["severity"]]))

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
    group_by(severity, arm) %>%
    summarise(
      Frequency=length(unique(id))) %>%
    mutate(
      Proportions =
        case_when(arm=="A1" ~ scales::percent(Frequency / N1, 10^(-proportions_dp)),
                  arm=="A2" ~ scales::percent(Frequency / N2, 10^(-proportions_dp)),
                  arm=="A3" ~ scales::percent(Frequency / N3, 10^(-proportions_dp)),
                  arm=="A4" ~ scales::percent(Frequency / N4, 10^(-proportions_dp))))%>%
    pivot_wider(
      names_from = arm, values_from = c(Frequency, Proportions))

  #to produce nice table
  name1 <- "N" %p% subsc("1")
  name2 <- "N" %p% subsc("2")
  name3 <- "N" %p% subsc("3")
  name4 <- "N" %p% subsc("4")
  border <- fp_border_default(width=1.5)

  if (arm_number==2){
    Table2_print <- Table2 %>%
      mutate(
        Frequency_Total = sum(Frequency_A1, Frequency_A2, na.rm=TRUE),
        Proportions_Total = scales::percent(Frequency_Total/(N1+N2), 0.1)) %>%
      relocate(Proportions_A1, .after=Frequency_A1) %>%
      flextable() %>%
      add_header_row(
        values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"), "Total"),
        colwidths = c(1, 2, 2, 2)) %>%
      set_header_labels(
        severity="Severity", Frequency_A1="N", Proportions_A1="%", Frequency_A2="N", Proportions_A2="%",
        Frequency_Total="N", Proportions_Total="%") %>%
      flextable::align(align="center", j = c(2:7), part="all") %>%
      autofit() %>%
      width(j=c(2:7), width=0.8) %>%
      vline(j=c(1, 3, 5), border=border, part="all") %>%
      vline(i=2, j=c(2, 4, 6), border=border, part="header") %>%
      vline(j=c(2, 4, 6), border=border, part="body") %>%
      bold(i=1, bold=TRUE, part="header") %>%
      bg(part="header", bg="gray80") %>%
      bg(part="body", bg="white") %>%
      fontsize(size=8, part="all")

  } else if (arm_number==3){
    Table2_print <- Table2 %>%
      mutate(
        Frequency_Total = sum(Frequency_A1, Frequency_A2, Frequency_A3, na.rm=TRUE),
        Proportions_Total = scales::percent(Frequency_Total/(N1+N2+N3), 0.1)) %>%
      relocate(Proportions_A1, .after=Frequency_A1) %>%
      relocate(Proportions_A2, .after=Frequency_A2) %>%
      flextable() %>%
      add_header_row(
        values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                 str_glue("{arm_names[3]} ({name3}={N3})"), "Total"),
        colwidths = c(1, 2, 2, 2, 2)) %>%
      set_header_labels(
        severity="Severity", Frequency_A1="N", Proportions_A1="%", Frequency_A2="N", Proportions_A2="%",
        Frequency_A3="N", Proportions_A3="%", Frequency_Total="N", Proportions_Total="%") %>%
      flextable::align(align="center", j = c(2:9), part="all") %>%
      autofit() %>%
      width(j=c(2:9), width=0.8) %>%
      vline(j=c(1, 3, 5, 7), border=border, part="all") %>%
      vline(i=2, j=c(2, 4, 6, 8), border=border, part="header") %>%
      vline(j=c(2, 4, 6, 8), border=border, part="body") %>%
      bold(i=1, bold=TRUE, part="header") %>%
      bg(part="header", bg="gray80") %>%
      bg(part="body", bg="white") %>%
      fontsize(size=8, part="all")
  } else {
    Table2_print <- Table2 %>%
      mutate(
        Frequency_Total = sum(Frequency_A1, Frequency_A2, Frequency_A3, Frequency_A4, na.rm=TRUE),
        Proportions_Total = scales::percent(Frequency_Total/(N1+N2+N3+N4), 0.1)) %>%
      relocate(Proportions_A1, .after=Frequency_A1) %>%
      relocate(Proportions_A2, .after=Frequency_A2) %>%
      relocate(Proportions_A3, .after=Frequency_A3) %>%
      flextable() %>%
      add_header_row(
        values=c("", str_glue("{arm_names[1]} ({name1}={N1})"), str_glue("{arm_names[2]} ({name2}={N2})"),
                 str_glue("{arm_names[3]} ({name3}={N3})"), str_glue("{arm_names[4]} ({name4}={N4})"), "Total"),
        colwidths = c(1, 2, 2, 2, 2, 2)) %>%
      set_header_labels(
        severity="Severity", Frequency_A1="N", Proportions_A1="%", Frequency_A2="N", Proportions_A2="%",
        Frequency_A3="N", Proportions_A3="%", Frequency_A4="N", Proportions_A4="%", Frequency_Total="N",
        Proportions_Total="%") %>%
      flextable::align(align="center", j = c(2:11), part="all") %>%
      autofit() %>%
      width(j=c(2:11), width=0.8) %>%
      vline(j=c(1, 3, 5, 7, 9), border=border, part="all") %>%
      vline(i=2, j=c(2, 4, 6, 8, 10), border=border, part="header") %>%
      vline(j=c(2, 4, 6, 8, 10), border=border, part="body") %>%
      bold(i=1, bold=TRUE, part="header") %>%
      bg(part="header", bg="gray80") %>%
      bg(part="body", bg="white") %>%
      fontsize(size=8, part="all")
  }

  plot(Table2_print)

  if (!is.null(save_image_path)){
    save_as_image(Table2_print, path=save_image_path)
  }

  if(!is.null(save_docx_path)){
    save_as_docx(Table2_print, path=save_docx_path)
  }

}
