#' Table of AE summary by body system class
#'
#' @param data Dataframe with adverse_event, body_system_class, id, arm, date_rand and last_visit columns
#' @param body_system_class Name of body_system_class column
#' @param id Name of id column
#' @param arm Name of arm column
#' @param date_rand Name of date_rand column
#' @param last_visit Name of last_visit column
#' @param control Factor level of control arm
#' @param intervention1 Factor level of intervention 1 arm
#' @param intervention2 Factor level of intervention 2 arm
#' @param intervention3 Factor level of intervention 3 arm
#' @param control_name Name of control
#' @param intervention1_name Name of intervention 1
#' @param intervention2_name Name of intervention 2
#' @param intervention3_name Name of intervention 3
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
                    last_visit = "last_visit", control = "C", intervention1 = "I1", intervention2 = "I2",
                    intervention3 = "I3", control_name="Control", intervention1_name="Intervention 1",
                    intervention2_name = "Intervention 2", intervention3_name = "Intervention 3"){
  # change the column names
  dataset <- data %>%
    rename("body_system_class" = body_system_class, "id" = id, "arm" = arm, "date_rand" = date_rand,
           "last_visit" = last_visit)

  arm_number <- length(levels(arm))
  # recode arm factor
  dataset <- dataset %>%
    mutate(
      arm =
        case_when(arm_number==2 ~ recode_factor(arm, control="C", intervention1="I1"),
                  arm_number==3 ~ recode_factor(arm, control="C", intervention1="I1", intervention2="I2"),
                  arm_number==4 ~ recode_factor(arm, control="C", intervention1="I1", intervention2="I2",
                                                intervention3="I3")))

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))
  stopifnot("id variable type is not numeric or factor!" = is.numeric(dataset[["id"]]) | is.factor(dataset[["id"]]))
  stopifnot("arm variable type is not factor!" = is.factor(dataset[["arm"]]))
  stopifnot("date_rand variable type is not Date!" = is.Date(dataset[["date_rand"]]))
  stopifnot("last_visit variable type is not Date!" = is.Date(dataset[["last_visit"]]))

  # number of participants at risk per arm
  N0 <- length(unique((dataset %>% filter(arm=="C"))$id))
  N1 <- length(unique((dataset %>% filter(arm=="I1"))$id))
  if (arm_number==3 | 4){
    N2 <- length(unique((dataset %>% filter(arm=="I2"))$id))
  }
  if (arm_number==4){
    N3 <- length(unique((dataset %>% filter(arm=="I3"))$id))
  }

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
        case_when(arm=="C" ~ scales::percent(Frequency / N0, 0.1),
                  arm=="I1" ~ scales::percent(Frequency / N1, 0.1),
                  arm=="I2" ~ scales::percent(Frequency / N2, 0.1),
                  arm=="I3" ~ scales::percent(Frequency / N3, 0.1))) %>%
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

  # to produce nice table
  name1 <- "N" %p% subsc("1")
  name2 <- "N" %p% subsc("2")
  border <- fp_border_default(width=1.5)

  Table1_print <- Table1 %>%
    relocate(Events_I1, Mean_I1, .after = Frequency_I1) %>%
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
    width(j=c(2, 3, 5, 6), width=2) %>%
    vline(j=c(1, 4), border=border, part="all") %>%
    vline(i=c(2, 3), j=c(2, 5), border=border, part="header") %>%
    vline(i=3, j=c(3, 6), border=border, part="header") %>%
    vline(j=c(2, 3, 5, 6), border=border, part="body") %>%
    bold(i=1, bold=TRUE, part="header") %>%
    bg(part="header", bg="gray80") %>%
    bg(part="body", bg="white")

  plot(Table1_print)
}
