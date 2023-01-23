library(tidyverse)
library(scales)

eikonomics_colors <- c("#073b4c","#ef476f", "#118ab2", "#ffd166", 
                              "#06d6a0","#35B779FF", "#21908CFF")

show_col(eikonomics_colors)

scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = eikonomics_colors)
}

scale_colour_continuous <- function(...) {
  scale_colour_manual(..., values = eikonomics_colors)
}

  theme_eikonomics <- function(axis_text, legend_text, axis_title, legend_position) { 
    theme(
      panel.background = element_rect(fill = "white", colour = "White"),
      axis.text = element_text(size = axis_text), 
      legend.text = element_text(size = legend_text),
      axis.title = element_text(size = axis_title, face = "bold"), 
      legend.position = legend_position, 
      legend.title = element_blank())
  }
  



