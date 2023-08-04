#' Stacked bar chart of proportions for each body system class by arm and maximum severity
#'
#' @param data data frame with adverse_event, body_system_class, severity, id and arm column
#' @param body_system_class name of body_system_class column
#' @param severity name of severity column
#' @param id name of id column
#' @param arm name of arm column
#' @param arm1 factor level of arm 1
#' @param arm2 factor level of arm 2
#' @param arm3 factor level of arm 3
#' @param arm4 factor level of arm 4
#' @param arm1_name name of arm 1
#' @param arm2_name name of arm 2
#' @param arm3_name name of arm 3
#' @param arm4_name name of arm 4
#' @param severity_levels vector of level of severity in ascending order if severity is not ordered factor
#' @param severity_colours vector of colours for level of severity in ascending order
#' @param save_image_path file path to save stacked bar chart plot as image
#'
#' @return stacked bar chart presenting the proportions of participants with each event by arm and by maximum severity
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' aestacked(df, body_system_class="ae_02", severity="ae_05", arm1="Anti-IgE", arm2="Placebo", severity_levels=c("Mild", "Moderate", "Severe"), arm1_name="Anti-IgE", arm2_name="Placebo")
aestacked <- function(data, body_system_class="body_system_class", severity="severity", id="id", arm="arm",
                      arm1="A1", arm2="A2", arm3="A3", arm4="A4", arm1_name="Arm 1", arm2_name="Arm 2",
                      arm3_name="Arm 3", arm4_name="Arm 4", severity_levels=c(), severity_colours=c(),
                      save_image_path=NULL){
  # change the column names
  dataset <- data %>%
    dplyr::rename("body_system_class" = body_system_class, "severity" = severity, "id" = id, "arm" = arm)

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))
  stopifnot("severity variable type is not factor!" = is.factor(dataset[["severity"]]))

  # checks if either severity variable is ordered or severity level is specified
  stopifnot("severity variable is not ordered or severity_levels is not specified!" = !(!is.ordered(dataset$severity) & length(severity_levels)==0))

  severity_number <- length(levels(dataset$severity))
  if (length(severity_colours)==0){
    severity_colours <- c("#a7d5ed", "#22a7f0", "#e14b31", "#c23728", "#000000")
  }

  #checks if length of severity_colours is greater than or equal to the number of levels in severity variable
  stopifnot("length of severity_levels needs to be greater than or equal to length of severity_colours!" = length(severity_colours)>=severity_number)

  # order severity factor
  if (!is.ordered(dataset$severity)){
    # checks if number of levels specified in severity_levels argument is greater than or equal the number of levels in severity variable
    stopifnot("number of levels in severity_levels argument needs to be greater than or equal to the number of levels in severity variable of data!" = (length(severity_levels)>=length(unique(dataset$severity))))
    dataset <- dataset %>%
      mutate(
        severity = ordered(severity, levels = severity_levels))
  }

  # number of arm factor levels
  arm_number <- length(unique(dataset$arm))
  # recode arm factor
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

  Table4 <- dataset %>%
    group_by(body_system_class, id, arm) %>%
    summarise(
      Maximum_Grade = max(severity)) %>%
    group_by(body_system_class, Maximum_Grade, arm) %>%
    summarise(
      Frequency = length(unique(id))) %>%
    complete(arm, fill=list(Frequency=0)) %>%
    mutate(
      Percentage = case_when(arm=="A1" ~ round(Frequency / N1 * 100, 1),
                             arm=="A2" ~ round(Frequency / N2 * 100, 1),
                             arm=="A3" ~ round(Frequency / N3 * 100, 1),
                             arm=="A4" ~ round(Frequency / N4 * 100, 1)))

  # order by length of bar for arm1
  ordering <- (Table4 %>%
                 filter(arm=="A1") %>%
                 group_by(body_system_class) %>%
                 summarise(length=sum(Percentage)) %>%
                 arrange(desc(length)))$body_system_class

  # plot stacked bar chart
  stacked <- ggplot(Table4, aes(x=arm, y=Percentage, fill=Maximum_Grade)) +
    geom_bar(position="stack", stat="identity", width=0.6, colour="black", linewidth=0.35) +
    scale_fill_manual(values=severity_colours, name="Maximum grade") +
    scale_y_continuous(limits=c(0, 101), breaks=seq(0, 100, 20), expand=c(0, 0)) +
    facet_wrap(~factor(body_system_class, levels=ordering), ncol = 1, strip.position='left') +
    coord_flip() +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color="grey", linewidth=0.1, linetype=1),
      axis.ticks.x=element_line(colour="black"),
      axis.ticks.y=element_blank(),
      axis.line=element_line(color="black"),
      panel.grid.major.y = element_blank(),
      axis.title.y = element_blank(),
      panel.spacing = unit(0, "lines"),
      strip.background=element_blank(),
      strip.placement = "outside",
      strip.text.y.left=element_text(angle=0),
      legend.background=element_blank(),
      legend.justification=c(1, 0),
      legend.position=c(1, 0))
  if (arm_number==2){
    stacked <- stacked +
      geom_text(data=subset(Table4, Frequency != 0), aes(label=Frequency), position=position_stack(vjust=0.8),
                colour="black", size=2) +
      scale_x_discrete(limits=rev, drop=FALSE, expand=c(0.9, 0), labels=c(arm2_name, arm1_name))
  } else if (arm_number==3){
    stacked <- stacked +
      geom_text(data=subset(Table4, Frequency != 0), aes(label=Frequency), position=position_stack(vjust=0.8),
                colour="black", size=1.3) +
      scale_x_discrete(limits=rev, drop=FALSE, expand=c(0.6, 0), labels=c(arm3_name, arm2_name, arm1_name)) +
      theme(axis.text.y = element_text(size=6))
  } else{
    stacked <- stacked +
      geom_text(data=subset(Table4, Frequency != 0), aes(label=Frequency), position=position_stack(vjust=0.8),
                colour="black", size=1) +
      scale_x_discrete(limits=rev, drop=FALSE, expand=c(0.4, 0), labels=c(arm4_name, arm3_name, arm2_name,
                                                                          arm1_name)) +
      theme(axis.text.y = element_text(size=5.5))
  }

  plot(stacked)

  if (!is.null(save_image_path)){
    ggsave(save_image_path, dpi=700, bg="white")
  }
}
