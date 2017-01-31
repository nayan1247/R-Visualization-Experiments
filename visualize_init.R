## A code base which has easy callable functions to do common plotting
## required for business


################################################################################
################################################################################
############## Define a Theme Template
fte_theme <- function() {
  library(RColorBrewer)
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=20,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=30, vjust=1.25)) +
    theme(axis.text.x=element_text(size=20,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=20,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=20,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=20,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}



#################################################################################
#################################################################################
############### 1. Heat Map with 3 numeric attributes and 2 factor attributes


heatmap_3vars<-function(df_dataFrame,
                        xAxis_factor,
                        yAxis_factor,
                        cellColour_numeric,
                        bubbleColour_numeric,
                        textInBubble_numeric)
{
  library(ggplot2)
  ggplot(df_dataFrame, aes_string(x=xAxis_factor ,y=yAxis_factor)) +   
    geom_tile(aes_string(fill = cellColour_numeric))+
    scale_fill_gradient(low="yellow", high="red")+
    geom_point(aes_string(colour = bubbleColour_numeric),size =20)  +    ## geom_point for circle illusion
    geom_text(aes_string(label = textInBubble_numeric),size=6,colour = "black",fontface = "bold")+
    scale_color_gradient(low = "white",high = "green")
}

## Example Usage
library(sqldf)
mtcars_plot<-sqldf("select cyl,gear,avg(wt) as wt, avg(qsec) as qsec, avg(mpg) as mpg from mtcars group by 1,2")
heatmap_3vars(df_dataFrame=mtcars_plot,
              xAxis_factor="cyl",
              yAxis_factor="gear",
              cellColour_numeric="wt",
              bubbleColour_numeric="qsec",
              textInBubble_numeric="mpg")+
  theme_minimalPlus()




#################################################################################
#################################################################################
############### 2. Heat Map with 1 numeric attributes and 2 factor attributes


heatmap_1vars<-function(df_dataFrame,
                        xAxis_factor,
                        yAxis_factor,
                        cellColour_numeric
                        )
{
  library(ggplot2)
  ggplot(df_dataFrame, aes_string(x=xAxis_factor ,y=yAxis_factor)) +   
    geom_tile(aes_string(fill = cellColour_numeric))+
    scale_fill_gradient(low="yellow", high="red")
}


## Example Usage
library(sqldf)
mtcars_plot<-sqldf("select cyl,gear,avg(wt) as wt, avg(qsec) as qsec, avg(mpg) as mpg from mtcars group by 1,2")

heatmap_1vars(df_dataFrame=mtcars_plot,
              xAxis_factor="cyl",
              yAxis_factor="gear",
              cellColour_numeric="wt") +
  theme_minimalPlus()+
  ggtitle("Categorical Box Plot")

#################################################################################
#################################################################################
############### 3. Categorical Box Plot

catBox<-function(df_dataFrame,
                 xAxis_factor,
                 yAxis_numeric){
  library(ggplot2)
  ggplot(df_dataFrame, aes_string(y=yAxis_numeric, x=xAxis_factor,colour = xAxis_factor)
         ) +
    geom_boxplot(lwd=0.75,fill="#f1f8fe",outlier.size = 3)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  fte_theme()
}

## Example Usage
mtcars$cyl<-as.factor(mtcars$cyl)
catBox(mtcars,"cyl","mpg") + 
  theme_minimalPlus()+
  scale_color_ptol("cyl")+
  scale_fill_ptol("cyl")+ggtitle("Categorical Box Plot")
