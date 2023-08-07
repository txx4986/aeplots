#' Dot plot to visualise AE and harm profiles in two-arm randomised controlled trials
#'
#' @param data data frame with adverse_event, body_system_class, id and arm columns
#' @param body_system_class name of body_system_class column
#' @param id name of id column
#' @param arm name of arm column
#' @param control factor level of control arm
#' @param intervention factor level of intervention arm
#' @param save_image_path file path to save dot plot as image
#'
#' @return dot plot with proportions alongside treatment effect estimates (IRR) with accompanying 95% confidence interval
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @importFrom reshape2 melt
#' @import ggplot2
#' @import scales
#' @importFrom ggpubr annotate_figure, text_grob
#' @importFrom cowplot plot_grid
#'
#' @export
#'
#' @examples
#' aedot(df, body_system_class="ae_02", control="Placebo", intervention="Anti-IgE")
aedot <- function(data, body_system_class="body_system_class", id="id", arm="arm", control="C", intervention="I",
                  save_image_path=NULL){
  # change the column names
  dataset <- data %>%
    dplyr::rename("body_system_class" = body_system_class, "id" = id, "arm" = arm)

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))
  stopifnot("id variable type is not numeric or factor!" = is.numeric(dataset[["id"]]) | is.factor(dataset[["id"]]))

  # recode arm factor
  arm_number <- length(unique(dataset$arm))
  dataset$arm <- as.character(dataset$arm)
  dataset$arm[which(dataset$arm==control)] <- "C"
  dataset$arm[which(dataset$arm==intervention)] <- "I"
  dataset$arm <- as.factor(dataset$arm)

  # number of participants at risk per arm
  N1 <- length(unique((dataset %>% filter(arm=="I"))$id))
  N2 <- length(unique((dataset %>% filter(arm=="C"))$id))

  crit_value <- qnorm(0.975, mean=0, sd=1)

  # create table of relative risks and CIs
  Table3 <- dataset %>%
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
    mutate(
      r1 = eventn1 / N1,
      r2 = eventn2 / N2,
      Intervention = r1 * 100,
      Placebo = r2 *100,
      risk_diff = r1 - r2,
      seRD = sqrt(eventn1 * (N1 - eventn1) / N1^3 + eventn2 * (N2 - eventn2) / N2^3),
      lowerRD = risk_diff - crit_value * seRD,
      upperRD = risk_diff + crit_value * seRD,
      relrisk = r1 / r2,
      logRR = log(relrisk),
      stderrRR = sqrt(1/eventn1 - 1/N1 + 1/eventn2 - 1/N2),
      loglowerCIRR = logRR - crit_value * stderrRR,
      logupperCIRR = logRR + crit_value * stderrRR,
      lowerCIRR = exp(loglowerCIRR),
      upperCIRR = exp(logupperCIRR)
    )

  # decreasing order of relative risk
  RDSortedBS <- Table3[order(-Table3$relrisk),]
  # create subset of full data with the elements that we need for each half of the plot
  BSRisk <- subset(RDSortedBS, select=c(body_system_class, Intervention, Placebo, relrisk))
  BSRisk$body_system_class <- factor(BSRisk$body_system_class, levels=BSRisk$body_system_class[order(-BSRisk$relrisk)])
  BSRiskRatio <- subset(RDSortedBS, select=c(body_system_class, relrisk, lowerCIRR, upperCIRR))
  BSRiskRatio$body_system_class <- factor(BSRiskRatio$body_system_class, levels=BSRisk$body_system_class[order(-BSRisk$relrisk)])

  ByGroup <- reshape2::melt(BSRisk, id=c("body_system_class"))
  ByGroup <- ByGroup[ByGroup$variable != "relrisk",]

  # left is ggplot object of the group-specific risks (percentage of participants experiencing each type of event)
  left <- ggplot(ByGroup, aes(x=value, y=body_system_class, fill=variable)) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) +
    scale_fill_manual(values=c("red", "blue")) +
    ggtitle("") +
    ylab("Body system") +
    scale_x_continuous(name="Percentage of participants (%)") +
    scale_y_discrete(limits=rev(levels(ByGroup$body_system_class))) +
    theme(legend.position="bottom",
          legend.title=element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(color="grey", linewidth=0.1, linetype=1),
          axis.ticks.x=element_line(colour="black"),
          axis.line=element_line(color="black"),
          axis.text=element_text(size=6.5),
          axis.title=element_text(size=8))

  # right is a ggplot object of the estimate and corresponding confidence interval
  right <- ggplot(BSRiskRatio, aes(y=body_system_class, x=relrisk, xmin=lowerCIRR, xmax=upperCIRR, fill="Relative risk with 95% CI")) +
    ggstance::geom_pointrangeh(aes(xmin=lowerCIRR, xmax=upperCIRR)) +
    ggtitle("")+
    geom_vline(xintercept=1, linetype=2, colour="blue", linewidth=0.75) +
    scale_x_continuous(name="Relative risk with 95% CI",
                       trans=log2_trans(),
                       breaks=c(0.10, 0.5, 1, 2, 5, 10, 50, 100, 220),
                       labels=as.character(c(0.10, 0.5, 1, 2, 5, 10, 50, 100, 220))) +
    scale_y_discrete(limits=rev(levels(BSRiskRatio$body_system_class))) +
    theme(legend.position="bottom",
          legend.title=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(color="grey",linewidth=0.1, linetype=1),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_line(colour="black"),
          axis.text.x=element_text(angle=0, hjust=0.5, size=6.5),
          axis.line.x=element_line(linewidth=0.5, linetype="solid", colour="black"),
          axis.title=element_text(size=8))

  ### This code creates the table format for the number of patients experiencing an event per arm,
  ### or the total number of events of each type per arm
  tab_base <- ggplot(RDSortedBS, aes(y=body_system_class)) +
    ylab(NULL) + xlab(" ") + scale_y_discrete(limits = rev(levels(BSRiskRatio$body_system_class))) +
    theme(plot.title = element_text(hjust = 0.5, size=8), ## centering title on text
          axis.text.x=element_text(color="white"), ## need text to be printed so it stays aligned with figure but white so it's invisible
          axis.line=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.y=element_blank(),legend.position="bottom", legend.title = element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  ### Tables of number of participants experiencing each harm type for Intervention (I) and Placebo (P) arms
  t_I_n<-tab_base + geom_text(aes(x=1, label = eventn1, hjust = "middle"), size=3) + ggtitle(expression("I"["n"]))
  t_P_n<-tab_base + geom_text(aes(x=1, label = eventn2, hjust = "middle"), size=3) + ggtitle(expression("P"["n"]))
  ### Tables of total number of events per harm type for Intervention (I) and Placebo (P) arms
  t_I_event<-tab_base + geom_text(aes(x=1, label = n_events1, hjust = "middle"), size=3) + ggtitle(expression("I"["event"]))
  t_P_event<-tab_base + geom_text(aes(x=1, label = n_events2, hjust = "middle"), size=3) + ggtitle(expression("P"["event"]))

  ### Dot plot Version 2 - number of participants with each event and total number of events
  DotPlot <- plot_grid(left, right, t_I_n, t_I_event, t_P_n, t_P_event, nrow = 1, align = "h", rel_widths =
                         c(3,2,0.5,0.5,0.5,0.5), axis = "b")
  ### NB: may take a few seconds to generate
  DotPlot <- annotate_figure(DotPlot, bottom = text_grob(bquote("I:Intervention (N = "*.(N1)*"), P:Placebo (N ="*.(N2)*"); X"["n"]*"= number of participants in arm X with AE, X"["event"]*"= number of AEs in arm X"),
                                                         color = "black", face = "bold", size = 6.5),
                             top = text_grob("", color = "black", face = "bold", size = 6.5))

  plot(DotPlot)

  if (!is.null(save_image_path)){
    ggsave(save_image_path, dpi=700, bg="white")
  }
}
