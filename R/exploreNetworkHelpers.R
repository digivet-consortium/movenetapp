#' @import ggplot2
snapshots_diffperiods_dotplot1 <- function(data_daily, data_7days, data_14days,
                                           data_28days,data_84days, title, ylab){

  colours <- c("84 days" = "grey", "3 months" = "grey",
               "28 days" = "lightblue", "1 month" = "lightblue",
               "14 days" = "red", "2 weeks" = "red",
               "7 days" = "blue", "1 week" = "blue",
               "1 day" = "darkgreen")

  names(data_daily) <- c("x","y")
  names(data_7days) <- c("x","y")
  names(data_14days) <- c("x","y")
  names(data_28days) <- c("x","y")
  names(data_84days) <- c("x","y")

  ggplot(data_daily, aes(x, y)) +
    labs(title = title) +
    xlab("Time") +
    ylab(ylab) +
    scale_y_continuous(limits = c(0, NA)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    geom_line(aes(col = "1 day")) +
    geom_point(data = data_7days, aes(col = "7 days")) +
    geom_point(data = data_14days, aes(col = "14 days")) +
    geom_point(data = data_28days, aes(col = "28 days")) +
    geom_point(data = data_84days, aes(col = "84 days")) +
    scale_color_manual(values = colours,
                       limits = c("84 days", "28 days", "14 days", "7 days", "1 day"))
 }

snapshots_diffperiods_dotplot2 <-
  function(data_daily, data_7days, data_14days, data_monthly, data_quarterly,
           title, ylab){

    colours <- c("quarterly" = "grey", "monthly" = "lightblue",
                 "fortnightly" = "red", "weekly" = "blue",
                 "daily" = "darkgreen")

    names(data_daily) <- c("x","y")
    names(data_7days) <- c("x","y")
    names(data_14days) <- c("x","y")
    names(data_monthly) <- c("x","y")
    names(data_quarterly) <- c("x","y")

    ggplot(data_daily, aes(x, y)) +
      labs(title = title) +
      xlab("Time") +
      ylab(ylab) +
      scale_y_continuous(limits = c(0, NA)) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14)) +
      geom_line(aes(col = "daily")) +
      geom_point(data = data_7days, aes(col = "weekly")) +
      geom_point(data = data_14days, aes(col = "fortnightly")) +
      geom_point(data = data_monthly, aes(col = "monthly")) +
      geom_point(data = data_quarterly, aes(col = "quarterly")) +
      scale_color_manual(values = colours,
                         limits = c("quarterly", "monthly", "fortnightly",
                                    "weekly", "daily"))
}

snapshot_summary_stats_linechart <- function(periodic_data, title, ylab){

  names(periodic_data) <- c("x", "min", "Q1", "median", "mean", "Q3", "max")

  ggplot(periodic_data) +
    aes(x) +
    labs(title = title) +
    xlab("Time") +
    ylab(ylab) +
    scale_y_continuous(limits = c(0,NA)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    geom_ribbon(aes(ymin = Q1, ymax = Q3), colour = "grey", alpha=0.5) +
    geom_line(aes(y = median), linetype = 1) +
    geom_line(aes(y = mean), linetype = 2)
}

periodic_data2summary_stats_df <-
  function(periodic_data, period_dates, period = c("month", "year")){
  df <-
    lapply(periodic_data, function(x){summary(x) |> round(digits = 3) |> unclass()}) |>
    bind_rows() |>
    `colnames<-`(c("min", "Q1", "median", "mean", "Q3", "max"))
  df <- cbind(period_dates, df)
  names(df)[1] <- period
  return(df)
  }

snapshot_barchart <- function(periodic_data, title, ylab){

  names(periodic_data) <- c("x","y")

  ggplot(data = periodic_data,
         aes(x, y)) +
    labs(title = title) +
    xlab("Time") +
    ylab(ylab) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    geom_bar(stat="identity")
}

monthxmonth_heatmap <- function(month_combinations, title){

  names(month_combinations)[3] <- "z"

  ggplot(data = month_combinations,
         aes(m1, m2, fill = z)) +
    labs(title = title) +
    xlab(NULL) +
    ylab(NULL) +
    scale_fill_gradient2(lim=c(0,1), low = "yellow", mid = "red", high = "black",
                         midpoint = 0.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    geom_tile()
}

snapshot_boxplot <- function(periodic_data, title, ylab) {

  names(periodic_data) <- c("x","y")

  ggplot(data = periodic_data,
         aes(x, y, group = x)) +
    labs(title = title) +
    xlab("Time") +
    ylab(ylab) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    geom_boxplot()
}

