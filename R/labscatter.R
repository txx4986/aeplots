#' Scatterplot matrix for baseline vs follow-up laboratory values by treatment group
#'
#' @description
#' `labscatter` generates a scatterplot matrix to visualise multiple continuous harm outcomes by treatment group.
#'  For each specified laboratory test, the function plots individual participants' baseline values against their maximum or minimum post-baseline values, or optionally their final visit value.
#'  Dashed lines represent the boundary between normal and abnormal thresholds, which can be defined using the `cutoff_list` argument or drawn from the the `lower` and `upper` columns in the dataset.
#'  Outliers beyond these thresholds are labelled with participant IDs.
#'
#' @param data dataframe with lab_test, visit, id, arm, base and aval columns (optional: lower and upper limit columns)
#' @param arm_levels vector of factor levels in arm variable
#' @param lab_test_list a character vector of laboratory test names to be included in the scatterplot matrix
#' @param limit_list a character vector specifying the direction of the cutoff for each laboratory test in `lab_test_list`; each element should be either `"upper"` or `"lower"`
#' @param cutoff_list a numeric vector of cutoff values (upper or lower) for each laboratory test in `lab_test_list`
#' @param lab_test name of lab_test column
#' @param visit name of visit column
#' @param id name of id column
#' @param arm name of arm column
#' @param base name of base column
#' @param aval name of aval column
#' @param lower name of lower column
#' @param upper name of upper column
#' @param last_visit a logical value whether to plot against final visit laboratory measurement value instead of maximum/minimum across all follow-up visits
#' @param arm_names a character vector of names for each arm in arm variable
#' @param arm_colours a vector of colours for each arm
#' @param save_image_path file path to save scatterplot matrix as image
#'
#' @return scatterplot matrix of
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import ggplot2
#' @import ggpubr
#' @import ggrepel
#' @import stringr
#'
#' @export
#'
#' @examples
#' labscatter(lab2, arm_levels=c("Placebo", "Intervention"), lab_test_list=c("Lymphocytes (GI/L)", "Monocytes (GI/L)", "Potassium (mmol/L)"), limit_list=c("upper", "upper", "lower"), cutoff_list=c(4.1, 1.1, 3.5))

labscatter <- function(data, arm_levels, lab_test_list, limit_list, cutoff_list, lab_test="lab_test", visit="visit", id="id",
                       arm="arm", base="base", aval="aval", lower, upper, last_visit=FALSE, arm_names=NULL, arm_colours=NULL,
                       save_image_path=NULL){
  # check column names specified can be found in data
  stopifnot("column name of lab_test cannot be found in data!" = lab_test %in% colnames(data))
  stopifnot("column name of visit cannot be found in data!" = visit %in% colnames(data))
  stopifnot("column name of id cannot be found in data!" = id %in% colnames(data))
  stopifnot("column name of arm cannot be found in data!" = arm %in% colnames(data))
  stopifnot("column name of base cannot be found in data!" = base %in% colnames(data))
  stopifnot("column name of aval cannot be found in data!" = aval %in% colnames(data))

  # change the column names
  dataset <- data %>%
    rename("lab_test"=lab_test, "visit"=visit, "id"=id, "arm"=arm, "base"=base, "aval"=aval)

  # checks if limit_list contains only either upper or lower
  stopifnot("limit_list should only contain either 'upper' or 'lower'!" = limit_list %in% c("upper", "lower"))

  # checks if lab tests in lab_test_list can be found in lab_test variable
  stopifnot("lab tests specified in lab_test_list cannot be found in lab_test column!" = lab_test_list %in% dataset$lab_test)

  # checks if length of lab_test_list equals length of limit_list
  stopifnot("length of limit_list needs to equal to length of lab_test_list!" = length(lab_test_list)==length(limit_list))

  # checks if the variable type for each column is correct
  stopifnot("visit variable type should be factor!" = is.factor(dataset[["visit"]]))
  stopifnot("visit variable should be ordered!" = is.ordered(dataset[["visit"]]))
  stopifnot("base variable type should be numeric!" = is.numeric(dataset[["base"]]))
  stopifnot("aval variable type should be numeric!" = is.numeric(dataset[["aval"]]))

  if (is.null(arm_names)){
    arm_names <- arm_levels
  }

  if (is.null(arm_colours)){
    arm_colours <- c("#2b3bff", "#e14b31", "#6AA84f", "#F1C232")
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
  stopifnot("labscatter can only take 2, 3 or 4 arms!" = (arm_number==2|arm_number==3|arm_number==4))

  # recode arm factor
  dataset$arm <- as.character(dataset$arm)
  dataset$arm[which(dataset$arm==arm_levels[1])] <- "A1"
  dataset$arm[which(dataset$arm==arm_levels[2])] <- "A2"
  dataset$arm[which(dataset$arm==arm_levels[3])] <- "A3"
  dataset$arm[which(dataset$arm==arm_levels[4])] <- "A4"
  dataset$arm <- as.factor(dataset$arm)

  options(dplyr.summarise.inform = FALSE)

  lab_test_len <- length(lab_test_list)

  # checks if either cutoff_list or upper/lower column names specified
  stopifnot("cutoff_list or upper/lower column names not specified!" = !missing(cutoff_list) | !missing(upper) | !missing(lower))

  if (!missing(cutoff_list)){
    # checks if length of lab_test_list equals length of cutoff_list
    stopifnot("length of cutoff_list needs to equal to length of lab_test_list!" = length(lab_test_list)==length(cutoff_list))

    # checks if upper/lower columns also specified
    stopifnot("cutoff_list already specified, don't have to specify upper/lower column names!" = (missing(upper) & missing(lower)))

    suppressMessages(lookup <- bind_cols(lab_test_list, limit_list, cutoff_list) %>%
                       rename("lab_test"=`...1`, "limit"=`...2`, "cutoff"=`...3`))

  } else{
    # checks if upper/lower is specified in limit_list and column name of upper/lower limit is specified
    # checks variable type for upper/lower column
    if ("upper" %in% limit_list){
      stopifnot("column name of upper limit not specified" = !missing(upper))
      stopifnot("column name of upper cannot be found in data!" = upper %in% colnames(data))
      dataset <- dataset %>%
        rename("upper"=upper)
      stopifnot("upper variable type should be numeric!" = is.numeric(dataset[["upper"]]))
    }

    if ("lower" %in% limit_list){
      stopifnot("column name of lower limit not specified" = !missing(lower))
      stopifnot("column name of lower cannot be found in data!" = lower %in% colnames(data))
      dataset <- dataset %>%
        rename("lower"=lower)
      stopifnot("lower variable type should be numeric!" = is.numeric(dataset[["lower"]]))
    }

    suppressMessages(lookup <- bind_cols(lab_test_list, limit_list) %>% rename("lab_test"=`...1`, "limit"=`...2`))

    # checks if there is a unique lower/upper limit
    for (i in (1:lab_test_len)){
      if (length(unique(dataset %>% filter(lab_test==lab_test_list[i]) %>% select(limit_list[i]))) != 1){
        stop(str_glue("The {limit_list[i]} limit of {lab_test_list[i]} lab test should be unique!"))
      }

      if (!is.numeric(unique(dataset %>% filter(lab_test==lab_test_list[i]) %>% select(limit_list[i]))[1, 1])){
        stop(str_glue("The {limit_list[i]} limit of {lab_test_list[i]} lab test should be numeric!"))
      }
    }

    lookup <- lookup %>%
      merge(dataset, by="lab_test", all.x=TRUE) %>%
      mutate(cutoff=ifelse(limit=="upper", upper, lower)) %>%
      select(c(lab_test, limit, cutoff)) %>%
      distinct()
  }

  plot_list <- vector("list", lab_test_len)

  for (i in (1:lab_test_len)){
    scatter_subset <- dataset %>%
      filter(lab_test==lab_test_list[i])

    if (last_visit==TRUE){
      y_axis_label <- "Final measurement"

      scatter_subset <- scatter_subset %>%
        filter(visit==levels(lab2$visit)[length(levels(lab2$visit))]) %>%
        rename("y_aval"=aval) %>%
        select(id, arm, base, y_aval)

      if (limit_list[i]=="upper"){
        scatter_subset <- scatter_subset %>%
          mutate(labels=ifelse((base>lookup[['cutoff']][i]|y_aval>lookup[['cutoff']][i]), as.character(id), ""))
      } else{
        scatter_subset <- scatter_subset %>%
          mutate(labels=ifelse((base<lookup[['cutoff']][i]|y_aval<lookup[['cutoff']][i]), as.character(id), ""))
      }
    } else{
      # checks if value for first visit is the same as baseline
      first_visit <- levels(scatter_subset$visit)[1]
      if (all((scatter_subset %>% filter(visit==first_visit))['base'] == (scatter_subset %>% filter(visit==first_visit))['aval'],
              na.rm=TRUE)){
        scatter_subset  <- scatter_subset %>%
          filter(visit!=first_visit)
      }

      if (limit_list[i]=="upper"){
        y_axis_label <- "Maximum"

        scatter_subset <- scatter_subset %>%
          group_by(id, arm, base) %>%
          summarise(y_aval=max(aval, na.rm=TRUE)) %>%
          mutate(labels=ifelse((base>lookup[['cutoff']][i]|y_aval>lookup[['cutoff']][i]), as.character(id), ""))
      } else{
        y_axis_label <- "Minimum"

        scatter_subset <- scatter_subset %>%
          group_by(id, arm, base) %>%
          summarise(y_aval=min(aval, na.rm=TRUE)) %>%
          mutate(labels=ifelse((base<lookup[['cutoff']][i]|y_aval<lookup[['cutoff']][i]), as.character(id), ""))
      }
    }

    scatter_plot <- ggplot(scatter_subset, aes(x=base, y=y_aval, shape=arm, color=arm, label=labels)) +
      geom_point() +
      scale_shape_manual(values=c(1:arm_number), labels=arm_names) +
      scale_color_manual(values=arm_colours, labels=arm_names) +
      geom_hline(yintercept=lookup[['cutoff']][i], linetype=2, col='black', linewidth=0.5) +
      geom_vline(xintercept=lookup[['cutoff']][i], linetype=2, col='black', linewidth=0.5) +
      geom_text_repel(size=2, show.legend=FALSE) +
      labs(title=str_glue("{lab_test_list[i]}"), x="Baseline", y=y_axis_label) +
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_line(color="grey", linewidth=0.1, linetype=1),
            axis.ticks.y=element_line(colour="black", linewidth=0.3),
            axis.ticks.x=element_line(colour="black", linewidth=0.3),
            axis.line=element_line(color="black", linewidth=0.3),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(hjust = 0.5))

    plot_list[[i]] <- scatter_plot
  }

  scatter_plot_matrix <- ggarrange(plotlist=plot_list, common.legend=TRUE, legend="bottom")
  plot(scatter_plot_matrix)

  if (!is.null(save_image_path)){
    suppressMessages(ggsave(save_image_path, dpi=600, width=10, height=8, bg="white"))
  }
}
