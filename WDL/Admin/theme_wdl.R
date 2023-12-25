
### WDL THEME ==================================================================

## package dependencies
pacman::p_load (tidyverse, showtext, hrbrthemes)

## font function ---------------------------------------------------------------

font_wdl <- function () {
  
  library (showtext)
  showtext::font_add_google("Work Sans", "worksans")
  showtext::showtext_auto()
  
}

## colour functions ------------------------------------------------------------

# for fill argument
scale_fill_wdl <- function() {
  structure (list (
    scale_fill_manual (values = c("#0dcdc0", "#264653", "#e9c46a", "#f4a261", "#e76f51"))
  ))
}

# for col argument (when scale is discrete e.g. scatter plot)
scale_colour_discrete_wdl <- function () {
  structure (list (
    scale_color_manual (values = c("#0dcdc0", "#264653", "#e9c46a", "#f4a261", "#e76f51"))
  ))
}

# for col argument (when scale is continuous e.g. gradient plot)
scale_colour_cont_wdl <- function() {
  structure (list (
    scale_fill_gradientn (colours = c("#191919","#120D25","#202020","#362870","#483595","#5B44BB","#7B68C7","#9C8DD5","#BDB3E3","#DED9F1"))
  ))
}

## theme function --------------------------------------------------------------

theme_wdl <- function (base_size = 8, base_font = NA){
  theme (
    text = element_text (colour = "#000245", face = "plain", family = "worksans"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,8,0)),
    axis.title = element_text(size = 10, face = "plain", colour = "#000245", hjust = 0.5),
    axis.text.x = element_text (angle = 90, vjust = 0.5),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
    panel.background = element_blank(),
    legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
    legend.title = element_text (colour = "#000245", face = "bold"), 
    plot.margin = margin(1,1,1.5,1.2, "cm")
    )
}



