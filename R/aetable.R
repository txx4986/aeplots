#' Table of AE summary by body system class
#'
#' @param data Dataframe with adverse_event, body_system_class, id, arm, date_rand and last_visit columns
#' @param body_system_class Name of body_system_class column
#' @param id Name of id column
#' @param arm Name of arm column
#' @param date_rand Name of date_rand column
#' @param last_visit Name of last_visit column
#' @param intervention_name Name of intervention
#' @param control_name Name of control
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
                    last_visit = "last_visit", intervention_name="Intervention", control_name="Control"){
  # change the column names
  dataset <- data %>%
    rename("body_system_class" = body_system_class, "id" = id, "arm" = arm, "date_rand" = date_rand, "last_visit" = last_visit)

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))
  stopifnot("id variable type is not numeric or factor!" = is.numeric(dataset[["id"]]) | is.factor(dataset[["id"]]))
  stopifnot("arm variable type is not factor!" = is.factor(dataset[["arm"]]))
  stopifnot("date_rand variable type is not Date!" = is.Date(dataset[["date_rand"]]))
  stopifnot("last_visit variable type is not Date!" = is.Date(dataset[["last_visit"]]))

  # number of participants at risk per arm
  N1 <- length(unique((dataset %>% filter(arm=="I"))$id))
  N2 <- length(unique((dataset %>% filter(arm=="C"))$id))

  Table1 <- dataset %>%
    # follow up time is computed as difference between randomisation date and last visit date (units=weeks)
    mutate(follow_up_time = as.numeric(difftime(last_visit, date_rand, units="weeks"))) %>%
    group_by(body_system_class, id, arm, follow_up_time) %>%
    # count total number of adverse events of each participant for each body system class
    count() %>%
    group_by(body_system_class) %>%
    summarise(
      # number of participants with at least one adverse event for each body system class (intervention arm)
      Frequency_Intervention = length(unique(id[arm=="I"])),
      # total number of adverse events for each body system class (intervention arm)
      Events_Intervention = sum(n[arm=="I"]),
      # total length of follow up time for each body system class (intervention arm)
      Total_Time_Intervention = sum(follow_up_time[arm=="I"]),
      # mean number of adverse events for each participant for each body system class (intervention arm)
      Mean_Intervention = round(mean(n[arm=="I"]), 1),
      # standard deviation of number of adverse events for each participant for each body system class (intervention arm)
      SD_Intervention = round(sd(n[arm=="I"]), 1),
      # number of participants with at least one adverse event for each body system class (control arm)
      Frequency_Control = length(unique(id[arm=="C"])),
      # total number of adverse events for each body system class (control arm)
      Events_Control = sum(n[arm=="C"]),
      # total length of follow up time for each body system class (control arm)
      Total_Time_Control= sum(follow_up_time[arm=="C"]),
      # mean number of adverse events for each participant for each body system class (control arm)
      Mean_Control = round(mean(n[arm=="C"]), 1),
      # standard deviation of number of adverse events for each participant for each body system class (control arm)
      SD_Control = round(sd(n[arm=="C"]), 1)) %>%
    mutate(
      # IR = Events_Intervention / Total_Time_Intervention (intervention arm)
      IR_Intervention = scales::percent(Events_Intervention / Total_Time_Intervention, 0.1),
      # Proportions = Frequency_Intervention / N1 (intervention arm)
      Proportions_Intervention = scales::percent(Frequency_Intervention / N1, 0.1),
      # IR = Events_Control / Total_Time_Control (control arm)
      IR_Control = scales::percent(Events_Control / Total_Time_Control, 0.1),
      # Proportions = Frequency_Control / N2 (control arm)
      Proportions_Control = scales::percent(Frequency_Control / N2, 0.1)) %>%
    mutate(
      # one decimal place for mean and SD
      across(c(Mean_Intervention, SD_Intervention, Mean_Control, SD_Control), \(x) format(x, nsmall=1))) %>%
    mutate(
      # combine Frequency & Proportions, Events & IR, Mean & SD columns
      Frequency_Intervention = str_glue("{Frequency_Intervention} ({Proportions_Intervention})"),
      Events_Intervention = str_glue("{Events_Intervention} ({IR_Intervention})"),
      Mean_Intervention = str_glue("{Mean_Intervention} ({SD_Intervention})"),
      Frequency_Control = str_glue("{Frequency_Control} ({Proportions_Control})"),
      Events_Control = str_glue("{Events_Control} ({IR_Control})"),
      Mean_Control = str_glue("{Mean_Control} ({SD_Control})")) %>%
    select(
      # drop Total_Time, SD, IR and Proportions from table
      -c(Total_Time_Intervention, SD_Intervention, Total_Time_Control, SD_Control, IR_Intervention,
         Proportions_Intervention, IR_Control, Proportions_Control)
    )

  # to produce nice table
  name1 <- "N" %p% subsc("1")
  name2 <- "N" %p% subsc("2")
  border <- fp_border_default(width=1.5)

  Table1_print <- Table1 %>%
    flextable() %>%
    add_header(
      values = c(Frequency_Intervention="At least one event",
                 Events_Intervention="Number of events",
                 Mean_Intervention="Number of events",
                 Frequency_Control="At least one event",
                 Events_Control="Number of events",
                 Mean_Control="Number of events")) %>%
    add_header_row(
      values=c("", str_glue("{intervention_name} ({name1}={N1})"), str_glue("{control_name} ({name2}={N2})")),
      colwidths = c(1, 3, 3)) %>%
    set_header_labels(
      body_system_class="Body system class", Frequency_Intervention="N (%)", Events_Intervention="n (IR)",
      Mean_Intervention="Mean number of events per participant (SD)", Frequency_Control="N (%)",
      Events_Control="n (IR)", Mean_Control="Mean number of events per participant (SD)") %>%
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

  return(Table1)
  plot(Table1_print)
}
