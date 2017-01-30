## A code base which has easy callable functions to do common plotting
## required for business

#################################################################################
#################################################################################
############### 1. Heat Map with 3 attributes
library(ggplot2)
View(mtcars)

heatmap_3vars<-function(df_dataFrame,
                        xAxis_factor,
                        yAxis_factor,
                        cellColour_numeric,
                        bubbleColour_numeric,
                        textInBubble_numeric)
{
  ggplot(df_dataFrame, aes_string(x=xAxis_factor ,y=yAxis_factor)) +   
    geom_tile(aes_string(fill = cellColour_numeric))+
    theme_bw() +  
    scale_fill_gradient(low="yellow", high="red",guide = guide_legend(title = "Average Confidence"))+
    geom_point(aes_string(colour = bubbleColour_numeric),size =20)  +    ## geom_point for circle illusion
    geom_text(aes_string(label = textInBubble_numeric),size=6,colour = "black",fontface = "bold")+
    scale_color_gradient(low = "white",high = "blue")+       ## color of the corresponding aes
    scale_size(range = c(8, 8))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=20),
          legend.text=element_text(size=17),
          legend.title=element_text(size=20))# no title
}

## Example Usage
library(sqldf)
mtcars_plot<-sqldf("select cyl,gear,avg(wt) as wt, avg(qsec) as qsec, avg(mpg) as mpg from mtcars group by 1,2")
heatmap_3vars(df_dataFrame=mtcars_plot,
              xAxis_factor="cyl",
              yAxis_factor="gear",
              cellColour_numeric="wt",
              bubbleColour_numeric="qsec",
              textInBubble_numeric="mpg")


