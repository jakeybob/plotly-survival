plotly_survival <- function(ggsurvplot, strata_colours="Set1", plot_title="",
                                      xaxis=list(title="time",
                                                 hoverformat=".2f"),
                                      yaxis=list(title="survival", 
                                                 hoverformat=".2f"),
                                      legend=list(),
                                      plot_CIs=TRUE,
                                      CI_opacity=0.25,
                                      CI_linewidth=0,
                                      censor_symbol="line-ns-open",
                                      censor_size=10,
                                      showlegend_surv=TRUE,
                                      showlegend_censor=FALSE,
                                      showlegend_CI=TRUE,
                                      showlegend_all=TRUE){
  
  # coerce to ggsurvplot where possible (will work for survfit objects)
  if(class(ggsurvplot)[1] != "ggsurvplot"){
    ggsurvplot <- ggsurvplot(ggsurvplot)
  }
  
  # defaults (can probably do something clever by combining lists so that users don't have to redefine e.g.
  # hoverinfo if they specifiy another xaxis attribute)
  # xaxis_default = list(title="time",
  #            hoverformat=".2f")
  # xaxis <- c(xaxis, xaxis_default)
  
  
  # extract survival data from ggsurvplot object
  df <- ggsurvplot$data.survplot
  df <- as.data.frame(df)[order(df$time), ]
  strata_name_if_no_strata <- "all"
  
  # number of strata in plot (e.g. 1 if no breakdown variable)
  num_of_strata <- ifelse(any(names(df)=="strata") == TRUE,
                          length(unique(df$strata)), 
                          1)
  
  # vector containing the names of the strata (for legend labels etc)
  if (num_of_strata > 1) {
    names_of_strata <- as.character(unique(df$strata))
  } else {
    names_of_strata <- strata_name_if_no_strata
    df$strata <- strata_name_if_no_strata
  }
  
  
  # convert dataframe into list of dataframes, where each list entry is one strata
  dfs <- list()  
  for(strata_name in names_of_strata){
    dfs[[strata_name]] <- dplyr::filter(df, strata==strata_name)
  }
  
  n <- sapply(dfs, nrow)  #  number of rows of data for each strata
  
  
  # Need to convert time series into stair-step. 
  ys <- sapply(n, function(x) rep(1:x, each = 2)[-2*x])  # doubled-up except for last time point
  xs <- sapply(n, function(x) c(1, rep(2:x, each = 2)))  # doubled-up except for first time-point
  
  if (num_of_strata == 1){  # if no strata will default to matrix, but has to be named list for later steps
    xs <- list(xs)
    names(xs) <- strata_name_if_no_strata
    ys <- list(ys)
    names(ys) <- strata_name_if_no_strata
  }
  
  
  # reconstruct data frames with the stair-stepped data
  for(strata_name in names_of_strata){
    dfs[[strata_name]] <- data.frame(x = dfs[[strata_name]]$time[xs[[strata_name]]], 
                                     y = dfs[[strata_name]]$surv[ys[[strata_name]]],
                                     up = dfs[[strata_name]]$upper[ys[[strata_name]]],
                                     low = dfs[[strata_name]]$lower[ys[[strata_name]]],
                                     dfs[[strata_name]][xs[[strata_name]], setdiff(names(dfs[[strata_name]]), c("x", "y"))])
  }
  
  create_ribbon_max <- function(df){
    # reorders "up" column (i.e. the upper confidence limit) so that the ribbons algorithm draws correctly.
    # Otherwise it draws over/underlapping triangles etc.
    a <- df$up[2:length(df$up)]   # swap order of every pair (after first entry) so that ribbon drawing works!
    b <- a
    b[c(T, F)] <- a[c(F,T)]
    b[c(F, T)] <- a[c(T,F)]
    df$up <- c(df$up[1], b)
    return(df)
  }
  
  dfs <- lapply(dfs, create_ribbon_max)
  
  
  # combine all the dataframes for each strata back into one, now that they have been stair-stepped
  dfALL <- dplyr::bind_rows(dfs)  %>% 
    dplyr::arrange(strata)
  
  censored <- dplyr::filter(dfALL, n.censor>0)  #  dataframe of all the censor events
  
  
  if(plot_CIs != TRUE){
    CI_opacity <- 0
    showlegend_CI <- FALSE
  }
  
  
  # create the plot_ly object
  p <- plotly::plot_ly(dfALL, 
               split=~strata, 
               colors=strata_colours, 
               hoverinfo="x+y+text", 
               legendgroup=~strata, 
               text=~strata)  %>%
    # draw the survival plots
    plotly::add_lines(x=~x, y=~y, color=~strata,
              name=~strata,
              legendgroup=~strata,
              showlegend=showlegend_surv) %>%
    # draw the confidence intervals
    plotly::add_ribbons(x=~x, ymin=~low, ymax=~up, color=~strata, 
                opacity=CI_opacity, 
                line=list(width=CI_linewidth),
                name=~strata,
                showlegend=showlegend_CI,
                legendgroup=~strata,
                hoverinfo="none")  %>%
    # draw the censor marks
    plotly::add_markers(x=~x, y=~y, color=~strata, data=censored, 
                marker=list(symbol=censor_symbol, size=censor_size),
                name=~strata,
                showlegend=showlegend_censor,
                legendgroup=~strata,
                hoverinfo="none") %>%
    plotly::layout(title=plot_title, 
           showlegend=showlegend_all,
           xaxis=xaxis, 
           yaxis=yaxis,
           legend=legend)
  
  return(p)
  
}