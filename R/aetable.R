#' Table of AE summary by body system class
#'
#' @param data Dataframe with adverse_event, body_system_class, id, arm, date_rand and last_visit columns
#' @param body_system_class Name of body_system_class column
#' @param id Name of id column
#' @param arm Name of arm column
#' @param date_rand Name of date_rand column
#' @param last_visit Name of last_visit column
#' @param arm1 Factor level of arm 1
#' @param arm2 Factor level of arm 2
#' @param arm3 Factor level of arm 3
#' @param arm4 Factor level of arm 4
#' @param arm1_name Name of arm 1
#' @param arm2_name Name of arm 2
#' @param arm3_name Name of arm 3
#' @param arm4_name Name of arm 4
#' @param save_image_path file path to save table as image
#' @param save_docx_path file path to save table as docx
#'
#' @return Flextable of AE summary by body system class
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import flextable
#' @import common
#'
#' @export
#'
#' @examples
#' aetable(df, body_system_class="ae_02", id="participant_id", arm="treatment_arm", date_rand="randomisation_date", last_visit="date_of_last_visit")
aetable <- function(data, body_system_class = "body_system_class", id = "id", arm = "arm", date_rand = "date_rand",
                    last_visit = "last_visit", arm1 = "A1", arm2 = "A2", arm3 = "A3", arm4="A4", arm1_name="Arm 1",
                    arm2_name = "Arm 2", arm3_name = "Arm 3", arm4_name = "Arm 4",
                    save_image_path=NULL, save_docx_path=NULL){
  # change the column names
  dataset <- data %>%
    rename("body_system_class" = body_system_class, "id" = id, "arm" = arm, "date_rand" = date_rand,
           "last_visit" = last_visit)

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))
  stopifnot("id variable type is not numeric or factor!" = is.numeric(dataset[["id"]]) | is.factor(dataset[["id"]]))
  stopifnot("date_rand variable type is not Date!" = is.Date(dataset[["date_rand"]]))
  stopifnot("last_visit variable type is not Date!" = is.Date(dataset[["last_visit"]]))

  arm_number <- length(unique(dataset$arm))
  # recode arm factor
  # dataset <- dataset %>%
  #   mutate(
  #     arm =
  #       case_when(arm_number==2 ~ recode_factor(arm, arm1="A1", arm2="A2),
  #                 arm_number==3 ~ recode_factor(arm, arm1="A1", arm2="A2, arm3="A3"),
  #                 arm_number==4 ~ recode_factor(arm, arm1="A1", arm2="A2, arm3="A3", arm4="arm4")))
  dataset$arm <- as.character(dataset$arm)
  dataset$arm[which(dataset$arm==arm1)] <- "A1"
  dataset$arm[which(dataset$arm==arm2)] <- "A2"
  dataset$arm[which(dataset$arm==arm3)] <- "A3"
  dataset$arm[which(dataset$arm==arm4)] <- "A4"
  dataset$arm <- as.factor(dataset$arm)

  # number of participants at risk per arm
  N1 <- length(unique((dataset %>% filter(arm=="A1"))$id))
  N2 <- length(unique((dataset %>% filter(arm=="A2"))$id))
  N3 <- length(unique((dataset %>% filter(arm=="A3"))$id))
  N4 <- length(unique((dataset %>% filter(arm=="A4"))$id))

  Table1 <- dataset %>%
    # follow up time is computed as difference between randomisation date and last visit date (units=weeks)
    mutate(follow_up_time = as.numeric(difftime(last_visit, date_rand, units="weeks"))) %>%
    group_by(body_system_class, id, arm) %>%
    # count total number of adverse events of each participant for each body system class
    summarise(
      sum_follow_up_time = sum(follow_up_time),
      n = n()) %>%
    group_by(body_system_class, arm) %>%
    summarise(
      # number of participants with at least one adverse event for each body system class and arm
      Frequency = length(unique(id)),
      # total number of adverse events for each body system class and arm
      Events = sum(n),
      # total length of follow up time for each body system class and arm
      Total_Time = sum(sum_follow_up_time),
      # mean number of adverse events for each participant for each body system class and arm
      Mean = round(mean(n), 1),
      # standard deviation of number of adverse events for each participant for each body system class and arm
      SD = round(sd(n), 1)) %>%
    mutate(
      # IR = Events / Total_Time
      IR = scales::percent(Events / Total_Time, 0.1),
      # Proportions = Frequency / N
      Proportions =
        case_when(arm=="A1" ~ scales::percent(Frequency / N1, 0.1),
                  arm=="A2" ~ scales::percent(Frequency / N2, 0.1),
                  arm=="A3" ~ scales::percent(Frequency / N3, 0.1),
                  arm=="A4" ~ scales::percent(Frequency / N4, 0.1))) %>%
    mutate(
      # one decimal place for mean and SD
      across(c(Mean, SD), \(x) format(x, nsmall=1))) %>%
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

  #to produce nice table
  name1 <- "N" %p% subsc("1")
  name2 <- "N" %p% subsc("2")
  name3 <- "N" %p% subsc("3")
  name4 <- "N" %p% subsc("4")
  border <- fp_border_default(width=1.5)

  if (arm_number==2){
    Table1_print <- Table1 %>%
      relocate(Events_A1, Mean_A1, .after = Frequency_A1) %>%
      flextable() %>%
      add_header(
        values = c(Frequency_A1="At least one event",
                   Events_A1="Number of events",
                   Mean_A1="Number of events",
                   Frequency_A2="At least one event",
                   Events_A2="Number of events",
                   Mean_A2="Number of events")) %>%
      add_header_row(
        values=c("", str_glue("{arm1_name} ({name1}={N1})"), str_glue("{arm2_name} ({name2}={N2})")),
        colwidths = c(1, 3, 3)) %>%
      set_header_labels(
        body_system_class="Body system class", Frequency_A1="N (%)", Events_A1="n (IR)",
        Mean_A1="Mean number of events per participant (SD)", Frequency_A2="N (%)",Events_A2="n (IR)",
        Mean_A2="Mean number of events per participant (SD)") %>%
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
  } else if (arm_number==3){
    Table1_print <- Table1 %>%
      select(body_system_class,Frequency_A1, Events_A1, Mean_A1, Frequency_A2, Events_A2, Mean_A2, Frequency_A3,
             Events_A3, Mean_A3) %>%
      flextable() %>%
      add_header(
        values = c(Frequency_A1="At least one event",
                   Events_A1="Number of events",
                   Mean_A1="Number of events",
                   Frequency_A2="At least one event",
                   Events_A2="Number of events",
                   Mean_A2="Number of events",
                   Frequency_A3="At least one event",
                   Events_A3="Number of events",
                   Mean_A3="Number of events")) %>%
      add_header_row(
        values=c("", str_glue("{arm1_name} ({name1}={N1})"), str_glue("{arm2_name} ({name2}={N2})"),
                 str_glue("{arm3_name} ({name3}={N3})")),
        colwidths = c(1, 3, 3, 3)) %>%
      set_header_labels(
        body_system_class="Body system class", Frequency_A1="N (%)", Events_A1="n (IR)",
        Mean_A1="Mean number of events per participant (SD)",  Frequency_A2="N (%)", Events_A2="n (IR)",
        Mean_A2="Mean number of events per participant (SD)", Frequency_A3="N (%)",Events_A3="n (IR)",
        Mean_A3="Mean number of events per participant (SD)") %>%
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
      select(body_system_class, Frequency_A1, Events_A1, Mean_A1, Frequency_A2, Events_A2, Mean_A2, Frequency_A3,
             Events_A3, Mean_A3, Frequency_A4, Events_A4, Mean_A4) %>%
      flextable() %>%
      add_header(
        values = c(Frequency_A1="At least one event",
                   Events_A1="Number of events",
                   Mean_A1="Number of events",
                   Frequency_A2="At least one event",
                   Events_A2="Number of events",
                   Mean_A2="Number of events",
                   Frequency_A3="At least one event",
                   Events_A3="Number of events",
                   Mean_A3="Number of events",
                   Frequency_A4="At least one event",
                   Events_A4="Number of events",
                   Mean_A4="Number of events")) %>%
      add_header_row(
        values=c("", str_glue("{arm1_name} ({name1}={N1})"), str_glue("{arm2_name} ({name2}={N2})"),
                 str_glue("{arm3_name} ({name3}={N3})"), str_glue("{arm4_name} ({name4}={N4})")),
        colwidths = c(1, 3, 3, 3, 3)) %>%
      set_header_labels(
        body_system_class="Body system class", Frequency_A1="N (%)", Events_A1="n (IR)",
        Mean_A1="Mean number of events per participant (SD)",  Frequency_A2="N (%)", Events_A2="n (IR)",
        Mean_A2="Mean number of events per participant (SD)", Frequency_A3="N (%)", Events_A3="n (IR)",
        Mean_A3="Mean number of events per participant (SD)", Frequency_A4="N (%)",Events_A4="n (IR)",
        Mean_A4="Mean number of events per participant (SD)") %>%
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

  plot(Table1_print)

  if (!is.null(save_image_path)){
    save_as_image(Table1_print, path=save_image_path)
  }

  if(!is.null(save_docx_path)){
    save_as_docx(Table1_print, path=save_docx_path)
  }
}
