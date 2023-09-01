#' Dot plot to visualise AE and harm profiles in two-arm randomised controlled trials
#'
#' @description
#' `aedot` creates a plot that summarises the AE and harm profiles via the following visualisations:
#'
#'  - **left panel**: percentage of participants who had an event in each treatment group
#'  - **central panel**: relative risk/ risk difference/ incidence rate ratio/ odds ratio of observing each event in the treatment group compared with the control group, along with 95% confidence interval
#'  - **right panel**: data table containing number of participants with at least one event and number of events by treatment group
#'
#' @section Treatment Effect Estimate:
#' The treatment effect and its 95% confidence interval are estimated via the following models below:
#'
#' - **unadjusted (RR)**: unadjusted relative risk
#' - **unadjusted (RD)**: unadjusted risk difference
#' - **Poisson (rate)**: fitting a generalised linear model with Poisson family and log link with length of follow up time as offset
#' - **Poisson (count)**: fitting a generalised linear model with Poisson family and log link with count as response
#' - **Negative Binomial (rate)**: fitting a Negative Binomial model with length of follow up time as offset
#' - **Negative Binomial (count)**: fitting a Negative Binomial model with count as response
#' - **Binomial (logit)**: fitting a generalised linear model with Binomial family and logit link
#' - **Binomial (log)**: fitting a generalised linear model with Binomial family and log link
#' - **Binomial (identity)**: fitting a generalised linear model with Binomial family and identity link
#'
#' The default model is the `unadjusted (RR)` model.
#'
#' For Poisson and Negative Binomial models, the treatment effect estimate is the **Incidence Rate Ratio (IRR)**.
#' For Binomial (logit) model, the treatment effect estimate is the **Odds Ratio (OR)**.
#' For Binomial (log) model, the treatment effect estimate is the **Relative Risk (RR)**.
#' For Binomial (identity) model, the treatment effect estimate is the **Risk Difference (RD)**.
#'
#' The reference group for the treatment arm in the regression model is the control arm.
#'
#' Additional covariates besides arm can be added into the model via the argument `variables`.
#' Note that interaction terms cannot be added to the model.
#'
#' @param data data frame with adverse_event, body_system_class, id and arm columns (optional: date_rand, last_visit and variables to be included in model)
#' @param control factor level of control arm
#' @param intervention factor level of intervention arm
#' @param body_system_class name of body_system_class column
#' @param id name of id column
#' @param arm name of arm column
#' @param date_rand name of date_rand column
#' @param last_visit name of last_visit column
#' @param model unadjusted relative risk/risk difference or model used for computation of treatment effect estimate and 95% CI
#' @param variables vector of variable names to be included in the model for computation of treatment effect estimate (excluding arm)
#' @param dot_colours vector of colours for dots on percentage of participants plot (left plot)
#' @param save_image_path file path to save dot plot as image
#'
#' @return dot plot with proportions alongside treatment effect estimates with accompanying 95% confidence interval
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @importFrom reshape2 melt
#' @import ggplot2
#' @import scales
#' @import stringr
#' @importFrom ggpubr annotate_figure text_grob
#' @importFrom cowplot plot_grid
#' @importFrom MASS glm.nb
#'
#' @export
#'
#' @examples
#' df2$aebodsys <- as.factor(df2$aebodsys)
#' aedot(df2, body_system_class="aebodsys", control="Placebo", intervention="Intervention")
aedot <- function(data, control, intervention, body_system_class="body_system_class", id="id", arm="arm",
                  date_rand = "date_rand", last_visit = "last_visit", model="unadjusted (RR)", variables = c(),
                  dot_colours = c("red", "blue"), save_image_path=NULL){

  # check if the model specified is either unadjusted/Poisson/Negative Binomial/Binomial
  stopifnot("Model specified is not one of unadjusted (RR), unadjusted (RD), Poisson (rate), Poisson (count), Negative Binomial (rate), Negative Binomial (count), Binomial (logit), Binomial (log) or Binomial (identity)" = (model=="unadjusted (RR)")|(model=="unadjusted (RD)")|(model=="Poisson (rate)")|(model=="Poisson (count)")|(model=="Negative Binomial (rate)")|(model=="Negative Binomial (count)")|(model=="Binomial (logit)")|model=="Binomial (log)"|(model=="Binomial (identity)"))

  # change the column names
  if ((model=="Poisson (rate)") | (model=="Negative Binomial (rate)")){
    dataset <- data %>%
      rename("body_system_class" = body_system_class, "id" = id, "arm" = arm, "date_rand"=date_rand,
                    "last_visit"=last_visit)
    dataset <- dataset %>%
      # follow up time is computed as difference between randomisation date and last visit date (units=weeks)
      mutate(follow_up_time = as.numeric(difftime(last_visit, date_rand, units="weeks")))
  } else{
    dataset <- data %>%
      rename("body_system_class" = body_system_class, "id" = id, "arm" = arm)
  }

  # checks if the variable type for each column is correct
  stopifnot("body_system_class variable type is not factor!" = is.factor(dataset[["body_system_class"]]))

  # checks if control and intervention are of length 1
  stopifnot("control should be of length 1!" = length(control)==1)
  stopifnot("intervention should be of length 1!" = length(intervention)==1)

  # checks if control and intervention can be found in arm variable
  stopifnot("control level specified cannot be found in arm column!" = control %in% dataset$arm)
  stopifnot("intervention level specified cannot be found in arm column!" = intervention %in% dataset$arm)

  # checks if dot_colours have length 2 and are different
  stopifnot("dot_colours should be of length 2!" = length(dot_colours)==2)
  stopifnot("colours specified in dot_colours should be different!" = dot_colours[1]!=dot_colours[2])

  # recode arm factor
  dataset$arm <- as.character(dataset$arm)
  dataset$arm[which(dataset$arm==control)] <- "C"
  dataset$arm[which(dataset$arm==intervention)] <- "I"
  dataset$arm <- as.factor(dataset$arm)

  # number of participants at risk per arm
  N1 <- length(unique((dataset %>% filter(arm=="I"))$id))
  N2 <- length(unique((dataset %>% filter(arm=="C"))$id))

  crit_value <- qnorm(0.975, mean=0, sd=1)

  # create table of relative risks and risk differences and CIs
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
    ) %>%
    filter(!is.na(body_system_class))

  if (model=="unadjusted (RR)"){
    estimator <- "Relative risk"

    Table3 <- Table3 %>%
      rename("Estimate"="relrisk", "lowerCI"="lowerCIRR", "upperCI"="upperCIRR")

  } else if (model=="unadjusted (RD)"){
    estimator <- "Risk difference"

    Table3 <- Table3 %>%
      rename("Estimate"="risk_diff", "lowerCI"="lowerRD", "upperCI"="upperRD")

  } else{
    # count number of AEs for each id grouped by body_system_class
    reg_df <- dataset %>%
      mutate(id=as.factor(id)) %>%
      group_by(body_system_class, id) %>%
      count() %>%
      group_by(body_system_class) %>%
      complete(id, fill=list(n=0))
    if ((model=="Poisson (rate)") | (model=="Negative Binomial (rate)")){
      lookup <- dataset %>% select(all_of(c(variables, "id", "arm", "follow_up_time"))) %>% distinct()
    } else{
      lookup <- dataset %>% select(all_of(c(variables, "id", "arm"))) %>% distinct()
    }
    reg_df <- reg_df %>% merge(lookup, by="id")

    if (model=="Poisson (rate)"){
      estimator <- "Incidence rate ratio"

      # fit glm model with Poisson function and log link
      glm_func <- function(x){
        fit <- glm(n ~ . - follow_up_time, offset=log(follow_up_time), family=poisson(link="log"),
                   data=reg_df %>%
                     filter(body_system_class==x) %>%
                     mutate(arm=relevel(arm, ref="C")) %>%
                     ungroup() %>%
                     select(all_of(c(variables, "arm", "follow_up_time", "n"))))
        return(list(Estimate=coef(fit)[["armI"]], lower=confint(profile(fit), level=0.95)["armI","2.5 %"],
                    upper=confint(profile(fit), level=0.95)["armI","97.5 %"]))
      }

    } else if (model=="Poisson (count)"){
      estimator <- "Incidence rate ratio"

      # fit glm model with Poisson function and log link
      glm_func <- function(x){
        fit <- glm(n ~ . , family=poisson(link="log"),
                   data=reg_df %>%
                     filter(body_system_class==x) %>%
                     mutate(arm=relevel(arm, ref="C")) %>%
                     ungroup() %>%
                     select(all_of(c(variables, "arm", "n"))))
        return(list(Estimate=coef(fit)[["armI"]], lower=confint(profile(fit), level=0.95)["armI","2.5 %"],
                    upper=confint(profile(fit), level=0.95)["armI","97.5 %"]))
      }

    } else if (model=="Negative Binomial (rate)"){
      estimator <- "Incidence rate ratio"

      # fit negative binomial model
      glm_func <- function(x){
        fit <- MASS::glm.nb(n ~ . - follow_up_time + offset(log(follow_up_time)),
                            data=reg_df %>%
                              filter(body_system_class==x) %>%
                              mutate(arm=relevel(arm, ref="C")) %>%
                              ungroup() %>%
                              select(all_of(c(variables, "arm", "follow_up_time", "n"))))
        return(list(Estimate=coef(fit)[["armI"]], lower=confint(profile(fit), level=0.95)["armI","2.5 %"],
                    upper=confint(profile(fit), level=0.95)["armI","97.5 %"]))
      }

    } else if (model=="Negative Binomial (count)"){
      estimator <- "Incidence rate ratio"

      # fit negative binomial model
      glm_func <- function(x){
        fit <- MASS::glm.nb(n ~ . ,
                            data=reg_df %>%
                              filter(body_system_class==x) %>%
                              mutate(arm=relevel(arm, ref="C")) %>%
                              ungroup() %>%
                              select(all_of(c(variables, "arm", "n"))))
        return(list(Estimate=coef(fit)[["armI"]], lower=confint(profile(fit), level=0.95)["armI","2.5 %"],
                    upper=confint(profile(fit), level=0.95)["armI","97.5 %"]))
      }

    } else {
      # indicate whether each id had AE for each body_system_class
      reg_df <- reg_df %>%
        mutate(n=ifelse(n>=1, 1, 0))

      if(model=="Binomial (logit)"){
        estimator <- "Odds ratio"

        # fit glm model with binomial function and logit link
        glm_func <- function(x){
          fit <- glm(n ~ . , family=binomial(link="logit"),
                     data=reg_df %>%
                       filter(body_system_class==x) %>%
                       mutate(arm=relevel(arm, ref="C")) %>%
                       ungroup() %>%
                       select(all_of(c(variables, "arm", "n"))))
          return(list(Estimate=coef(fit)[["armI"]], lower=confint(profile(fit), level=0.95)["armI","2.5 %"],
                      upper=confint(profile(fit), level=0.95)["armI","97.5 %"]))
        }
      } else if (model=="Binomial (log)"){
        estimator <- "Relative risk"

        # fit glm model with binomial function and log link
        glm_func <- function(x){
          fit <- glm(n ~ . , family=binomial(link="log"),
                     data=reg_df %>%
                       filter(body_system_class==x) %>%
                       mutate(arm=relevel(arm, ref="C")) %>%
                       ungroup() %>%
                       select(all_of(c(variables, "arm", "n"))))
          return(list(Estimate=coef(fit)[["armI"]], lower=confint(profile(fit), level=0.95)["armI","2.5 %"],
                      upper=confint(profile(fit), level=0.95)["armI","97.5 %"]))
        }
      } else {
        estimator <- "Risk difference"

        # fit glm model with binomial function and identity link
        glm_func <- function(x){
          fit <- glm(n ~ . , family=binomial(link="identity"),
                     data=reg_df %>%
                       filter(body_system_class==x) %>%
                       mutate(arm=relevel(arm, ref="C")) %>%
                       ungroup() %>%
                       select(all_of(c(variables, "arm", "n"))))
          return(list(Estimate=coef(fit)[["armI"]], lower=confint(profile(fit), level=0.95)["armI","2.5 %"],
                      upper=confint(profile(fit), level=0.95)["armI","97.5 %"]))
        }
      }
    }

    glm_func_vect <- Vectorize(glm_func)
    Table3 <- Table3 %>%
      rowwise() %>%
      mutate(
        Coef = list(possibly(glm_func_vect, otherwise=NA)(body_system_class)))

    if(model=="Binomial (identity)"){
      Table3 <- Table3 %>%
        mutate(
          Estimate = Coef[[1]],
          lowerCI = ifelse(is.na(Estimate)==FALSE, Coef[[2]], NA),
          upperCI = ifelse(is.na(Estimate)==FALSE, Coef[[3]], NA),
        ) %>%
        select(-c(Coef))

    } else{
      Table3 <- Table3 %>%
        mutate(
          Estimate = exp(Coef[[1]]),
          lowerCI = exp(ifelse(is.na(Estimate)==FALSE, Coef[[2]], NA)),
          upperCI = exp(ifelse(is.na(Estimate)==FALSE, Coef[[3]], NA)),
        ) %>%
        select(-c(Coef))
    }
  }

  # decreasing order of Estimate
  RDSortedBS <- Table3[order(-Table3$Estimate),]
  # create subset of full data with the elements that we need for each half of the plot
  BSRisk <- subset(RDSortedBS, select=c(body_system_class, Intervention, Placebo, Estimate))
  BSRisk$body_system_class <- factor(BSRisk$body_system_class,
                                     levels=BSRisk$body_system_class[order(-BSRisk$Estimate)])
  BSRiskRatio <- subset(RDSortedBS, select=c(body_system_class, Estimate, lowerCI, upperCI))
  BSRiskRatio$body_system_class <- factor(BSRiskRatio$body_system_class,
                                          levels=BSRisk$body_system_class[order(-BSRisk$Estimate)])

  ByGroup <- reshape2::melt(BSRisk, id=c("body_system_class"))
  ByGroup <- ByGroup[ByGroup$variable != "Estimate",]

  # left is ggplot object of the group-specific risks (percentage of participants experiencing each type of event)
  left <- ggplot(ByGroup, aes(x=value, y=body_system_class, fill=variable)) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) +
    scale_fill_manual(values=dot_colours) +
    ggtitle("") +
    ylab("Body system") +
    scale_x_continuous(name="Percentage of participants (%)") +
    scale_y_discrete(limits=rev(levels(ByGroup$body_system_class)), labels=function(x) str_wrap(x, width=20)) +
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
  right <- ggplot(BSRiskRatio, aes(y=body_system_class, x=Estimate, xmin=lowerCI, xmax=upperCI,
                                   fill=str_glue("{estimator} with 95% CI"))) +
    ggstance::geom_pointrangeh(aes(xmin=lowerCI, xmax=upperCI), fatten=1.5, size=1) +
    ggtitle("") +
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

  if ((model=="unadjusted (RD)")|(model=="Binomial (identity)")){
    right <- right +
      geom_vline(xintercept=0, linetype=2, colour="blue", linewidth=0.75) +
      scale_x_continuous(name=str_glue("{estimator} with 95% CI"),
                         breaks=c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1),
                         labels=as.character(c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)))
  } else{
    right <- right +
      geom_vline(xintercept=1, linetype=2, colour="blue", linewidth=0.75) +
      scale_x_continuous(name=str_glue("{estimator} with 95% CI"),
                         trans=log2_trans(),
                         breaks=c(0.10, 0.5, 1, 2, 5, 10, 50, 100, 220),
                         labels=as.character(c(0.10, 0.5, 1, 2, 5, 10, 50, 100, 220)))
  }

  # creates the table format for the number of patients experiencing an event per arm, or the total number of events of each type per arm
  tab_base <- ggplot(RDSortedBS, aes(y=body_system_class)) +
    ylab(NULL) + xlab(" ") + scale_y_discrete(limits = rev(levels(BSRiskRatio$body_system_class))) +
    theme(plot.title = element_text(hjust = 0.5, size=8), # centering title on text
          axis.text.x=element_text(color="white"), # need text to be printed so it stays aligned with figure but white so it's invisible
          axis.line=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.y=element_blank(),legend.position="bottom", legend.title = element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  # Tables of number of participants experiencing each harm type for Intervention (I) and Placebo (P) arms
  t_I_n<-tab_base + geom_text(aes(x=1, label = eventn1, hjust = "middle"), size=3) + ggtitle(expression("I"["n"]))
  t_P_n<-tab_base + geom_text(aes(x=1, label = eventn2, hjust = "middle"), size=3) + ggtitle(expression("P"["n"]))
  # Tables of total number of events per harm type for Intervention (I) and Placebo (P) arms
  t_I_event<-tab_base + geom_text(aes(x=1, label = n_events1, hjust = "middle"), size=3) + ggtitle(expression("I"["event"]))
  t_P_event<-tab_base + geom_text(aes(x=1, label = n_events2, hjust = "middle"), size=3) + ggtitle(expression("P"["event"]))

  # Dot plot - number of participants with each event and total number of events
  DotPlot <- cowplot::plot_grid(left, right, t_I_n, t_I_event, t_P_n, t_P_event, nrow = 1, align = "h",
                                rel_widths = c(3,2,0.5,0.5,0.5,0.5), axis = "b")
  DotPlot <- annotate_figure(DotPlot, bottom = text_grob(bquote("I:Intervention (N = "*.(N1)*"), P:Placebo (N ="*.(N2)*"); X"["n"]*"= number of participants in arm X with AE, X"["event"]*"= number of AEs in arm X"),
                                                         color = "black", face = "bold", size = 6.5),
                             top = text_grob("", color = "black", face = "bold", size = 6.5))

  plot(DotPlot)

  if (!is.null(save_image_path)){
    ggsave(save_image_path, dpi=700, width=10, height=8, bg="white")
  }
}
