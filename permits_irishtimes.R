library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library("grid")

permits <- read.csv(".data/permits.csv")

it_theme <- function() {
        
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
                theme(legend.background = element_rect(fill=color.background)) +
                theme(legend.text = element_text(size=7,color=color.axis.title)) +
                
                # Set title and axis labels, and format these and tick marks
                theme(plot.title=element_text(color=color.title, size=10)) +
                theme(axis.text.x=element_text(size=10, angle = -20, color=color.axis.text)) +
                theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
                theme(axis.title.x=element_text(size=8,color=color.axis.title)) +
                theme(axis.title.y=element_text(size=8,color=color.axis.title)) +
                
                # Plot margins
                theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

permits_new <- melt(permits, id.vars = c("Row.Labels"))
permits_new$Countries <- permits_new$variable
permits_new$value <- as.numeric(permits_new$value)

ggplot(permits_new, aes(Row.Labels, value, color = Countries,
                        size = log(value))) +
        geom_point(alpha = 0.75) +
        fte_theme() +
        scale_size_continuous(guide = FALSE) +
        ylab(' Permits') +
        xlab(' Industry ')
        
        