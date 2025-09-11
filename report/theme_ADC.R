theme_ADC <- 
  theme_bw(base_size=12,base_family="Helvetica") +
  theme(
    plot.title=element_text(size=11, face="bold",margin=margin(10,0,10,0),color="#1D244F"),
    plot.subtitle = element_text(size=10,margin=margin(0,0,10,0),color="#1D244F"),
    axis.text.x = element_text(angle=50, size=8, vjust=0.5, color="#1D244F"),
    axis.text.y = element_text(size=8, color="#1D244F"),
    axis.title.x = element_text(color="#1D244F",vjust=-.5,size=10),
    axis.title.y = element_text(color="#1D244F",angle=90,vjust=.5,size=10),
    panel.background=element_rect(fill="white"),
    axis.line = element_line(color="#1D244F"),
    panel.grid.major = element_line(colour = "gray", size = 0.01), 
    panel.grid.minor = element_line(colour = "gray", size = 0.04),
  )