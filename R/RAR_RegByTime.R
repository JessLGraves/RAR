#' RAR Regression by Localized Measures
#'
#' This function performs linear regression on localized RAR measures for an entire sample (mean, standard deviation, and relative activity) at each time bin against an outcome of interest. This function will take a dataframe that contains RAR_Localized() measures with an outcome of interest already merged in. Or, it will take two separate dataframes, one with the RAR_Localized() measures and one with the outcome. If using two dataframes, you must specifiy the ID column, which will be used to merge the two together within the funciton.
#'
#' @author Jessica Graves
#' @param df_measures dataframe containing columns from RAR_Localized() for each subject. Mean, Standard Deviation, and Relative Activity columns must be named mean.act, sd.act, and rel.act, respectively. This dataframe must also have an id column. If dataframe is already merged with outcome of interest, leave df_outcome
#' @param df_outcome dataframe containing outcome of interest. This dataframe must also have an id column, which is named the same as df_measures id column.
#' @param id_column specifies the column name in df_measures and df_outcome that corresponds to the id
#' @param y_variable a string describing outcome of interest (e.g, "Depression score")
#' @param formula formula to run lm model, e.g. score ~ mean.act. 
#' @param model_name a string describing the model predictors, e.g. "Mean Activity". Optional.
#' @param time_type a string describing the time type, e.g. "Person Time" or "Clock Time". Optional.
#' @param plots a logical specifying if the user would like plot to be outputted. Default is FALSE
#'
#' @importFrom magrittr %>%


RAR_RegByTime <- function(df_measures, df_outcome=NULL, id_column=NULL, y_variable, formula, model_name=NULL, time_type = NULL, plots=c(TRUE, FALSE)){

    df_measures = as.data.frame(df_measures)

    if(!missing(model_name) & !missing(time_type)){
        model_summaries_label <- paste0("Model summaries for ",  y_variable, " against ", model_name, " at each time point (", time_type, ")")
        beta_plot_label <- paste0("Coefficient estimates of ",  model_name, " against ", y_variable, "\n at each time point (", time_type, ")")
    }

    if(missing(model_name) & missing(time_type)){
        model_summaries_label <- paste0("Model summaries by time bin")
        beta_plot_label <- paste0("Coefficient estimates by time bin")
    }

    if(!missing(df_outcome) & !missing(id_column)){
        df_outcome = as.data.frame(df_outcome)
        ids_outcome <- as.character(unique(eval(substitute(id_column), df_outcome, parent.frame())))
        ids_measures <- as.character(unique(eval(substitute(id_column), df_measures, parent.frame())))

        df_outcome_final = df_outcome[ids_outcome %in% ids_measures,]
        df = merge(df_measures, df_outcome_final, by=as.character(substitute(id_column)))

        if((as.character(substitute(id_column)) %in% colnames(df_measures) & as.character(substitute(id_column)) %in% colnames(df_outcome)) == FALSE){
            stop("Must have matching id column in each dataframe.")
        }

        # if (as.character(substitute(y_variable)) %in% colnames(df_outcome)[which(lapply(df_outcome, is.numeric)==TRUE)] !=TRUE) {
        #     stop("y_variable must be numeric")
        # }

        # y_variable_quo <- dplyr::enquo(y_variable)
    }

    #if(missing(df_outcome) & missing(id_column)){
     #   df = as.data.frame(df_measures)
    #}

    if(missing(df_outcome) ){
        df = as.data.frame(df_measures)
    }

    if(missing(plots)){
        plots = FALSE
    }

    # Prepare variables to meet dplry requirements

    formula_name <- dplyr::enquo(formula)
    id_codes <- unique(eval(substitute(id_column), df, parent.frame()))

    # Creating these variables as null, so as to avoid "binding" issue during devtools::check()
    . <- NULL
    fits <- NULL
    term <- NULL
    bins <- NULL

    # Regression by time bin
    reg_untidy <- df %>%
        dplyr::group_by(bins) %>%                             # group = bin_name
        dplyr::do(fits = stats::lm(!! formula_name, data =  . )) %>%         # run lm(formula)
        dplyr::rename(model=bins)                             # rename so that model = bin_name

    reg_tidy <- broom::tidy(reg_untidy, fits)   # put results into tidy list

    reg_coef_summary <- as.data.frame(reg_tidy) # summarize the coefficient results as dataframe

    # Create summary table for each model that includes coefficient summary and model statistics
    reg_fit_stats <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(reg_fit_stats) <- c("model", "Model.Pvalue","adj.r.squared", "AIC", "BIC")
    for (i in 1: length(levels(df$bins))){
        reg_fit_stats[i,1] <- as.character(reg_untidy$model[i])
        reg_fit_stats[i,2] <- round(broom::glance(reg_untidy$fits[[i]])$p.value, 3)
        reg_fit_stats[i,3]<- round(broom::glance(reg_untidy$fits[[i]])$adj.r.squared, 3)
        reg_fit_stats[i,4] <- round(AIC(reg_untidy$fits[[i]]), 3)
        reg_fit_stats[i,5] <- round(BIC(reg_untidy$fits[[i]]), 3)
    }

    reg_summary <- as.data.frame(merge(reg_coef_summary, reg_fit_stats, by="model", sort=F))
    reg_summary$model = as.character(reg_summary$model)
    reg_summary <- within(reg_summary, model[term != "(Intercept)"] <- "")              # Formatting for readability
    reg_summary <- within(reg_summary, Model.Pvalue[term != "(Intercept)"] <- "")       # Formatting for readability
    reg_summary <- within(reg_summary, adj.r.squared[term != "(Intercept)"] <- "")      # Formatting for readability
    reg_summary <- within(reg_summary, AIC[term != "(Intercept)"] <- "")                # Formatting for readability
    reg_summary <- within(reg_summary, BIC[term != "(Intercept)"] <- "")                # Formatting for readability

    # HTML version of reg_summary table for easy RMarkdown HTML outputs
    table.html =  knitr::kable(reg_summary, format="html", caption=model_summaries_label) %>% kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

    # Latex version of table for easy latex outputs
    table.tex = xtable::xtable(reg_summary, type="html", caption=model_summaries_label) # for outputting latex code

    if(plots==T){
    # Plot of beta coefficients and their standard error
    beta_plot = dotwhisker::dwplot(reg_tidy) + ggplot2::xlab("Coefficient estimate") + ggplot2::ylab("") +
        ggplot2::labs(title=beta_plot_label) +
        ggplot2::geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
        ggplot2::theme(plot.title=ggplot2::element_text(face="bold", hjust=0.5)) + ggplot2::scale_color_discrete(name="Models:") + ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))


    # Store all as objects in a list
    objects <- list("fits" = reg_untidy, "coef_summary" = reg_coef_summary,  "model_summaries" = reg_summary, "html_table" = table.html, "latex_table" = table.tex, "beta_plot" = beta_plot, "df" = df)

    }

    if(plots==F){
        objects <- list("fits" = reg_untidy, "coef_summary" = reg_coef_summary,  "model_summaries" = reg_summary, "html_table" = table.html, "latex_table" = table.tex, "df" = df)
    }

    return(objects)
}
