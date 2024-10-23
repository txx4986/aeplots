#' Mean cumulative function plot for harm outcomes by treatment group
#'
#' @description
#' `aemcf` plots the mean cumulative function (MCF) of all adverse events, adverse events of specific organ systems or specific adverse events with embedded table of number of participants at risk.
#' The nonparametric MCF estimates (by Nelson-Aalen estimator) and the approximate 95% confidence intervals are obtained using the `mcf` function of the `reda` package.
#'
#' @param data data frame with adverse_event, body_system_class, id, arm, date_rand, date_ae and last_visit columns
#' @param arm_levels vector of factor levels in arm variable
#' @param subset an optional argument specifying a subset of observations to be used
#' @param adverse_event name of adverse_event column
#' @param body_system_class name of body_system_class column
#' @param id name of id column
#' @param arm name of arm column
#' @param date_rand name of date_rand column
#' @param date_ae name of date_ae column
#' @param last_visit name of last_visit column
#' @param arm_names vector of names for each arm in arm variable
#' @param arm_colours vector of colours for each arm
#' @param conf.int a logical value whether to plot the 95% confidence intervals
#' @param risk_table a logical value whether to plot the risk table
#' @param save_image_path file path to save MCF plot as image
#'
#' @return MCF plot with risk table
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import ggplot2
#' @import stringr
#' @import reda
#' @import ggsurvfit
#' @import lubridate
#' @importFrom patchwork wrap_plots
#'
#' @export
#'
#' @examples
#' aemcf(df2, c("Placebo", "Intervention"), subset= body_system_class=="Gastrointestinal", adverse_event="ae_pt", body_system_class="aebodsys")
aemcf <- function(data, arm_levels, subset, adverse_event="adverse_event", body_system_class="body_system_class",
                       id="id", arm="arm", date_rand="date_rand", date_ae="date_ae", last_visit="last_visit", time_units="days",
                       arm_names=NULL, arm_colours=NULL, conf.int=TRUE, risk_table=TRUE, save_image_path=NULL){
  # change the column names
  dataset <- data %>%
    rename("adverse_event" = adverse_event, "body_system_class" = body_system_class, "id" = id, "arm" = arm,
           "date_rand" = date_rand, "date_ae" = date_ae, "last_visit" = last_visit)

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))
  stopifnot("date_rand variable type is not Date!" = is.Date(dataset[["date_rand"]]))
  stopifnot("date_ae variable type is not Date!" = is.Date(dataset[["date_ae"]]))
  stopifnot("last_visit variable type is not Date!" = is.Date(dataset[["last_visit"]]))

  if (is.null(arm_names)){
    arm_names <- arm_levels
  }

  if (is.null(arm_colours)){
    arm_colours <- c("#F8766D", "#00BFC4", "#7CAE00", "#C77CFF")
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
  stopifnot("aemcf can only take 2, 3 or 4 arms!" = (arm_number==2|arm_number==3|arm_number==4))

  # check if time_units is either secs, mins, hours, days, weeks, months or years
  stopifnot("time_units specified must be either secs, mins, hours, days, weeks, months or years!" = (time_units=="secs")|(time_units=="mins")|(time_units=="hours")|(time_units=="days")|(time_units=="weeks")|(time_units=="months")|(time_units=="years"))

  # recode arm factor
  dataset$arm <- as.character(dataset$arm)
  dataset$arm[which(dataset$arm==arm_levels[1])] <- "A1"
  dataset$arm[which(dataset$arm==arm_levels[2])] <- "A2"
  dataset$arm[which(dataset$arm==arm_levels[3])] <- "A3"
  dataset$arm[which(dataset$arm==arm_levels[4])] <- "A4"
  dataset$arm <- as.factor(dataset$arm)

  # calculates time till AE occurred and follow-up time by time units specified
  if (time_units=="months"|time_units=="years"){
    dataset <- dataset %>%
      mutate(id=as.factor(id)) %>%
      mutate(ae_time=time_length(interval(date_rand, date_ae), time_units),
             follow_up_time=time_length(interval(date_rand, last_visit), time_units))
  } else{
    dataset <- dataset %>%
      mutate(id=as.factor(id)) %>%
      mutate(ae_time=as.numeric(difftime(date_ae, date_rand, units=time_units)),
             follow_up_time=as.numeric(difftime(last_visit, date_rand, units=time_units)))
  }

  lookup <- dataset %>% select(c("id", "arm", "follow_up_time")) %>% distinct()

  # get the possible subset
  if (!missing(subset)){
    subIdx <- eval(substitute(subset), dataset)
    stopifnot("subset argument must be logical!" = is.logical(subIdx))
    subIdx <- subIdx & ! is.na(subIdx)
    dataset <- dataset[subIdx, ]
  }

  # create time to event table grouped by arm
  df <- dataset %>%
    select(c(id, ae_time)) %>%
    merge(lookup, by="id", all.y=TRUE) %>%
    pivot_longer(cols=c(ae_time, follow_up_time), names_to="event", values_to="time") %>%
    distinct() %>%
    drop_na(time) %>%
    mutate(event=ifelse(event=="ae_time", 1, 0)) %>%
    group_by(id) %>%
    arrange(time, .by_group = TRUE)

  # calculate MCF estimates and 95% confidence intervals
  dfmcf <- mcf(Recur(time, id, event)~arm, data=df)

  # get next time point to plot confidence intervals using geom_rect
  mcf_table <- dfmcf@MCF %>%
    group_by(arm) %>%
    mutate(next_time=lead(time)) %>%
    mutate(next_time=ifelse(is.na(next_time), time, next_time))

  # plot MCF
  mcf_plot <-ggplot(mcf_table, aes(x=time, y=MCF, color=arm)) +
    geom_step(size=0.75) +
    labs(y="Mean cumulative number of events per participant", x=str_glue("Time ({str_to_title(time_units)})")) +
    scale_colour_manual(values=arm_colours, labels=arm_names) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color="grey90", linewidth=0.1, linetype=1),
      panel.grid.major.y = element_line(color="grey90", linewidth=0.1, linetype=1),
      axis.line=element_line(color="black", linewidth=0.3),
      axis.title=element_text(size=10),
      legend.direction='horizontal',
      legend.position = "top",
      legend.margin = margin(0, 0, 0, 0),
      legend.text=element_text(size=10),
      legend.title=element_text(size=10),
      axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
      axis.text=element_text(color='black')
    )

  # plot 95% confidence intervals if conf.int==TRUE
  if (conf.int==TRUE){

    mcf_plot <- mcf_plot +
      geom_rect(aes(xmin=time, xmax=next_time, ymin=lower, ymax=upper, fill=arm), alpha=0.2, linetype=0) +
      scale_fill_manual(values=arm_colours, labels=arm_names)
  }

  # plot risk table if risk_table==TRUE
  if (risk_table==TRUE){
    # obtain breaks for x-axis
    x_ticks <- ggplot_build(plot(dfmcf))$layout$panel_params[[1]]$x$breaks

    # fill in number at risk for x_ticks
    risk_table <- mcf_table %>%
      select(arm, time, numRisk) %>%
      group_by(arm) %>%
      complete(time=union(time, x_ticks)) %>%
      fill(numRisk) %>%
      fill(numRisk, .direction="up") %>%
      filter(time %in% x_ticks)

    # plot risk table
    gg_risktable <- risk_table |>
      ggplot(aes(x=time, y=arm, label=numRisk)) +
      scale_y_discrete(limits=rev, labels=rev(arm_names)) +
      labs(title="Number at risk") +
      geom_text(size=3) +
      labs(y=NULL, x=NULL) +
      theme_light() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title=element_text(hjust=0.5, size=10),
        panel.border=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_text(size=8, face="bold", hjust=0),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
      )

    pdf(NULL)

    # combine MCF plot and risk table
    gg_combined <- ggsurvfit_align_plots(list(mcf_plot, gg_risktable))

    MCFplot <- patchwork::wrap_plots(gg_combined[[1]], gg_combined[[2]], ncol=1, heights=c(1, 0.2))

    dev.off()

    plot(MCFplot)

  } else{

    mcf_plot <- mcf_plot +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

    plot(mcf_plot)

  }

  if (!is.null(save_image_path)){
    ggsave(save_image_path, dpi=700, bg="white")
  }
}
