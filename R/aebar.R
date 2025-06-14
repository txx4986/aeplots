#' Bar chart for number of events reported per participant
#'
#' @param data data frame with adverse_event, id and arm columns
#' @param arm_levels vector of factor levels in arm variable
#' @param id name of id column
#' @param arm name of arm column
#' @param adverse_event name of adverse_event column
#' @param arm_names vector of names for each arm in arm variable
#' @param arm_colours vector of colours for each arm
#' @param facets a logical value whether to plot bar chart with facets
#' @param save_image_path file path to save bar chart as image
#'
#' @return Bar chart presenting the number of events reported per participant
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import ggplot2
#' @import stringr
#'
#' @export
#'
#' @examples
#' aebar(df2, arm_levels=c("Intervention","Placebo"), adverse_event="ae_pt", facets=FALSE)
aebar <- function(data, arm_levels, id="id", arm="arm", adverse_event="adverse_event", arm_names=NULL,
                  arm_colours=NULL, facets=TRUE, save_image_path=NULL){
  # change the column names
  dataset <- data %>%
    rename("id" = id, "arm" = arm, "adverse_event" = adverse_event)

  if (is.null(arm_names)){
    arm_names <- arm_levels
  }

  if (is.null(arm_colours)){
    arm_colours <- c("#e14b31", "#22a7f0", "#6AA84f", "#F1C232")
  }

  # checks if arm_levels can be found in arm variable
  stopifnot("arm levels specified cannot be found in arm column!" = arm_levels %in% dataset$arm)

  # number of arm factor levels
  arm_number <- length(unique(dataset$arm))
  # checks if length of arm_levels equals to the number of arm factor levels
  stopifnot("length of arm_levels needs to be equal to the number of levels in arm" = length(arm_levels)==arm_number)
  # checks if length of arm_names equals to the number of arm factor levels
  stopifnot("length of arm_names needs to be equal to the number of levels in arm" = length(arm_names)==arm_number)
  # checks if length of arm_colours is greater than or equal to the number of levels in arm
  stopifnot("length of arm_colours needs to be greater than or equal to number of levels in arm!" = length(arm_colours)>=arm_number)
  # checks if arm number is 2, 3 or 4
  stopifnot("aebar can only take 2, 3 or 4 arms!" = (arm_number==2|arm_number==3|arm_number==4))

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

  Table5 <- dataset %>%
    filter(!is.na(adverse_event)) %>%
    group_by(id, arm) %>%
    # count total number of adverse events of each participant
    count() %>%
    group_by(n, arm) %>%
    summarise(
      Frequency = sum(!is.na(id))
    ) %>%
    complete(arm, fill=list(Frequency=0)) %>%
    mutate(
      Percentage = case_when(arm=="A1" ~ round(Frequency / N1 * 100, 1),
                             arm=="A2" ~ round(Frequency / N2 * 100, 1),
                             arm=="A3" ~ round(Frequency / N3 * 100, 1),
                             arm=="A4" ~ round(Frequency / N4 * 100, 1)))

  # plot bar chart
  if (facets==TRUE){
    bar <- ggplot(Table5, aes(x=factor(n), y=Percentage, fill=arm)) +
      geom_bar(stat="identity", width=0.6) +
      scale_y_continuous(expand=expansion(add = c(0,5))) +
      labs(x="Number of adverse events", y="Percentage of participants") +
      scale_fill_manual(values=arm_colours) +
      theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="grey", linewidth=0.1, linetype=1),
        axis.ticks.y=element_line(colour="black", linewidth=0.3),
        axis.ticks.x=element_blank(),
        axis.line=element_line(color="black", linewidth=0.3),
        panel.grid.major.x = element_blank(),
        legend.position="none",
        panel.spacing = unit(0, "lines"),
        strip.background = element_rect(color="black", fill=NA))

    if (arm_number==2){
      bar <- bar +
        facet_wrap(~arm, ncol=1, strip.position='top',
                   labeller=as_labeller(c("A1"=str_glue("{arm_names[1]} (n={N1})"),
                                          "A2"=str_glue("{arm_names[2]} (n={N2})"))))
    } else if (arm_number==3){
      bar <- bar +
        facet_wrap(~arm, ncol=1, strip.position='top',
                   labeller=as_labeller(c("A1"=str_glue("{arm_names[1]} (n={N1})"),
                                          "A2"=str_glue("{arm_names[2]} (n={N2})"),
                                          "A3"=str_glue("{arm_names[3]} (n={N3})"))))
    } else{
      bar <- bar +
        facet_wrap(~arm, ncol=1, strip.position='top',
                   labeller=as_labeller(c("A1"=str_glue("{arm_names[1]} (n={N1})"),
                                          "A2"=str_glue("{arm_names[2]} (n={N2})"),
                                          "A3"=str_glue("{arm_names[3]} (n={N3})"),
                                          "A4"=str_glue("{arm_names[4]} (n={N4})"))))
    }

  } else{
    bar <- ggplot(Table5, aes(x=factor(n), y=Percentage, fill=arm)) +
      geom_col(position=position_dodge(0.7, preserve="single"), width=0.6) +
      scale_y_continuous(expand=expansion(add = c(0,5))) +
      labs(title="Adverse event count by treatment arm", x="Number of adverse events", y="Percentage of participants") +
      theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="grey", linewidth=0.1, linetype=1),
        axis.ticks.y=element_line(colour="black"),
        axis.ticks.x=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major.x = element_blank(),
        legend.background=element_blank(),
        legend.justification=c("right", "top"),
        legend.position=c(1, 1),
        plot.title=element_text(hjust=0.5))

    if (arm_number==2){
      bar <- bar +
        scale_fill_manual(values=arm_colours, name="",
                          labels=c(str_glue("{arm_names[1]} (n={N1})"), str_glue("{arm_names[2]} (n={N2})")))
    } else if (arm_number==3){
      bar <- bar +
        scale_fill_manual(values=arm_colours, name="",
                          labels=c(str_glue("{arm_names[1]} (n={N1})"), str_glue("{arm_names[2]} (n={N2})"),
                                   str_glue("{arm_names[3]} (n={N3})")))
    } else{
      bar <- bar +
        scale_fill_manual(values=arm_colours, name="",
                          labels=c(str_glue("{arm_names[1]} (n={N1})"), str_glue("{arm_names[2]} (n={N2})"),
                                   str_glue("{arm_names[3]} (n={N3})"), str_glue("{arm_names[4]} (n={N4})")))
    }
  }
  plot(bar)

  if (!is.null(save_image_path)){
    suppressMessages(ggsave(save_image_path, dpi=600, bg="white"))
  }
}
