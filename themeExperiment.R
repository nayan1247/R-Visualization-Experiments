# Color Palettes from Paul Tol's "Colour Schemes"
# REference ggthemes package code https://github.com/jrnold/ggthemes/blob/master/R/ptol.R
ptol_pal <- function() {
  function(n) {
    if (n > 12) {
      stop(sprintf("%s is greater than the max number of colors", n))
    } else if (n < 1) {
      stop("the number of colors cannot be zero")
    }
    ggthemes_data$ptol$qualitative[[n]]
  }
}

scale_colour_ptol <- function(...) {
  discrete_scale("colour", "ptol", ptol_pal(), ...)
}

scale_color_ptol <- scale_colour_ptol

scale_fill_ptol <- function(...) {
  discrete_scale("fill", "ptol", ptol_pal(), ...)
}


ggthemes_data <- {
  ## x to hold value of list as I create it
  x <- list()
  
  x$ptol <-
    list(qualitative =
           list(
             c("#4477AA"),
             c("#4477AA", "#CC6677"),
             c("#4477AA", "#DDCC77", "#CC6677"),
             c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
             c("#4477AA", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
             c("#4477AA", "#88CCEE", "#117733", "#DDCC77", "#CC6677",
               "#AA4499"),
             c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677",
               "#AA4499"),
             c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77",
               "#CC6677", "#AA4499"),
             c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77",
               "#CC6677", "#882255", "#AA4499"),
             c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77",
               "#661100", "#CC6677", "#882255", "#AA4499"),
             c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933",
               "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
             c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933",
               "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")
           ))
  

  x$hc <- list()
  x$hc$palettes <- list()
  x$hc$palettes$default <- c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c",
                             "#8085e9", "#f15c80", "#e4d354", "#8085e8", "#8d4653", "#91e8e1")
  x$hc$palettes$darkunica <- c("#2b908f", "#90ee7e", "#f45b5b", "#7798BF", "#aaeeee", "#ff0066", "#eeaaee",
                               "#55BF3B", "#DF5353", "#7798BF", "#aaeeee")
  x$hc$bg <- c(default = "#FFFFFF",
               darkunica = "#2a2a2b")
  ## Return
  x
  
}

theme_minimalPlus <- function(base_size = 20, base_family = "") {
  # Starts with theme_bw and remove most parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      title = element_text(vjust=2),
      plot.title = element_text(size = rel(1.2), face = "bold"),
      panel.grid.major = element_line(colour = "gray100"),
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text =        element_text(size = rel(1)),
      legend.title =       element_text(hjust = 0,size = rel(1.1)),
      axis.text =        element_text(size = rel(1)),
      axis.title =       element_text(size = rel(1.2)),
      panel.background  = element_rect(fill="grey92"),
      panel.border      = element_blank(),
      strip.background  =  element_rect(fill = "blue", colour = "black", size = 4),
      plot.background   = element_blank(),
      text =               element_text(
        family = base_family, face = "plain",
        colour = "brown", size = base_size,
        lineheight = 0.9, hjust = 0.5, vjust = 0, angle = 0,
        margin = margin(), debug = FALSE
      ),
      
      complete = TRUE
    )
}


catBox(mtcars,"cyl","mpg") + 
  theme_minimalPlus(base_size = 10)+
  scale_color_ptol("cyl")+
  ggtitle("Categorical Box Plot")
