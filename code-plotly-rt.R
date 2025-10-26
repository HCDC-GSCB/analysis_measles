library(plotly)
plot_rt <- function(data) {
  df_plot <- data
  df_plot <- head(do.call(rbind, by(df_plot, df_plot$id, rbind, NA)), -1)
  df_plot[, c("pl", "id")] <- lapply(df_plot[, c("pl", "id")], na.locf)
  df_plot[] <- lapply(df_plot, na.locf, fromLast = TRUE)
  
  col <- c("#f26522", "#ed1d24", "#7f3f98", "#ffde17", "#00a14b")
  df_plot$color <- col[as.factor(df_plot$pl)]
  
  
  if (any(is.na(df_plot$dates)) || any(is.na(df_plot$q1_rt)) || any(is.na(df_plot$q3_rt))) {
    stop("Dữ liệu không hợp lệ: Có giá trị NA trong các cột cần thiết.")
  }
  
  
  p <- plot_ly()
  
  
  df_plot <- df_plot %>%
    group_by(id) %>%
    mutate(q1_rt_adjusted = q1_rt + (row_number() - 1) * 0.0001,  
           q3_rt_adjusted = q3_rt + (row_number() - 1) * 0.0001)
  
  
  p <- p %>% add_ribbons(data = df_plot, ymin = ~q1_rt_adjusted, ymax = ~q3_rt_adjusted, 
                         x = ~dates, fillcolor = ~color,
                         text = ~paste0("<b>", format(dates, "%b %d"), "</b>",
                                        "<br><b>Rt:    </b> ", round(rt, 2),
                                        "<br>    (", round(q1_rt, 2),
                                        " - ", round(q3_rt, 2), ")"),
                         hoverinfo = "text", line = list(color = 'transparent'),
                         opacity = 0.5,
                         name = 'Q1 RT',
                         showlegend = FALSE) %>%
    add_lines(x = ~dates, y = rep(1, nrow(df_plot)),
              line = list(dash = 'dash', color = 'black'),
              hoverinfo = "none")
  
  
  unique_ids <- unique(df_plot$id)
  for (i in unique_ids) {
    group_data <- df_plot[df_plot$id == i, ]
    p <- p %>% add_trace(data = group_data, x = ~dates, y = ~rt, mode = 'lines',
                         line = list(color = group_data$color[1]), 
                         text = ~paste0("<b>", format(dates, "%b %d"), "</b>",
                                        "<br><b>Rt:    </b> ", round(rt, 2),
                                        "<br>    (", round(q1_rt, 2),
                                        " - ", round(q3_rt, 2), ")"),
                         hoverinfo = "text",
                         showlegend = FALSE)
  }
  
  
  p <- p %>% add_segments(x = as.Date("2024-09-16"), xend = as.Date("2024-09-16"), 
                          y = 4, yend = 0, line = list(color = "black", width = 2), showlegend = FALSE, hoverinfo ="none") %>%
    add_segments(x = as.Date("2024-08-31"), xend = as.Date("2024-08-31"), 
                 y = 4, yend = 0, line = list(color = "black", width = 2), showlegend = FALSE, hoverinfo ="none") 
  
  
  p <- p %>% add_annotations(text = "VAC\nfor < 10y",
                             x = as.Date("2024-09-23") , 
                             y = 3.5, 
                             showarrow = FALSE, 
                             font = list(size = 12, color = "black")) %>%
    add_annotations(text = "VAC\nfor < 5y",
                    x = as.Date("2024-09-07") , 
                    y = 3.5, 
                    showarrow = FALSE, 
                    font = list(size = 12, color = "black"))
  
  p <- p %>% layout(
    title = "",
    xaxis = list(title = "Date", showgrid = TRUE, zeroline = FALSE),
    yaxis = list(title = "Estimate Rt", showgrid = TRUE, zeroline = FALSE),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hoverlabel = list(bgcolor = 'rgba(255,255,255,0.75)'),
    shapes = list(
      list(
        type = "rect",
        x0 = as.Date("2024-09-16"),
        x1 = as.Date("2024-09-16") + 15,
        y0 = 3,
        y1 = 4,
        fillcolor = "#42CAFD",
        line = list(color = "transparent")
      ),
      list(
        type = "rect",
        x0 = as.Date("2024-08-31"),
        x1 = as.Date("2024-08-31") + 14,
        y0 = 3,
        y1 = 4,
        fillcolor = "#42CAFD",
        line = list(color = "transparent")))
  )
  return(p)
}


df_filter_plot <- filter(df_filter, dates >= "2024-07-12")
p_hist <- plot_ly(df_filter_plot, x = ~dates) %>%
  add_bars(y = ~I, 
           name = "Cases",
           text = ~paste0("<b>", format(dates, "%b %d"), "</b>",
                          "<br><b>Cases: </b> ", I,
                          "<br><b>Moving Average: </b> ", round(mva_14d,2)
           ),
           hoverinfo = "text",
           textposition = 'none',
           marker = list(color = "#87A2FF", line = list(color = "black", width = 1))
  ) %>%
  add_trace(y = ~mva_14d, name = "Moving Average", 
            mode = 'lines', 
            line = list(color = "#E78F81", width = 2),
            text = ~paste0("<b>", format(dates, "%b %d"), "</b>",
                           "<br><b>Cases: </b> ", I,
                           "<br><b>Moving Average: </b> ", round(mva_14d,2)
            ),
            hoverinfo = "text") %>%
  layout(
    xaxis = list(title = "", tickangle = -60, tickformat = "%b %d", dtick = "M1"
    ),
    yaxis = list(title = "Incidence"),
    legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center"),
    plot_bgcolor = 'white',
    hoverlabel = list(bgcolor = 'rgba(255,255,255,0.75)')
  )


df_rt_plot <- filter(df_rt, dates >= "2024-07-12")

p_rt <- plot_rt(df_rt_plot)

subplot(p_hist, p_rt, nrows = 2, shareX = T)