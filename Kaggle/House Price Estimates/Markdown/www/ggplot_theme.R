# Custom ggplot theme
theme_custom <- function(base_size = 8,
                        base_family = ""){
  theme_minimal(base_size = base_size, 
                base_family = base_family) %+replace%
    theme(
      axis.line = element_line("Black"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.y = element_text(family = "URWGothic", colour = "Black", 
                                  angle = 90, vjust = 1, hjust = 0.5, size = 10),
      axis.title.x = element_text(family = "URWGothic", colour = "Black", 
                                  vjust = 0, hjust = 0.5, size = 10),
      axis.text = element_text(family = "URWGothic", colour = "Black", size = 8),
      axis.ticks.x.bottom = element_line(colour = "Black"),
      axis.ticks.y.left = element_line(colour = "Black"),
      legend.position = "bottom",
      legend.title = element_text(family = "URWGothic", colour = "Black", 
                                  size = 8),
      #legend.direction = "vertical",
      legend.text = element_text(family = "URWGothic", colour = "Black", 
                                 size = 8),
      plot.title = element_text(family = "URWGothic", 
                                colour = "Black", size = 15, hjust = 0.5, 
                                vjust = 1, margin = margin(b = 10, unit = "pt")),
      plot.background = element_rect(fill = "#ffffff"),
      panel.background = element_rect(fill = "#ffffff")
    )
}