library(ggplot2)

#import data
data = read.csv("~/sloping_slab/data/water_level_data.csv")

#define color palettes
cw_color_pal = colorRampPalette(c("#FF4500", "#FFA500", "#FFD700"))
cw_color = cw_color_pal(6)
nfec_color_pal = colorRampPalette(c("#87CEFA", "#4169E1", "#000080"))
nfec_color = nfec_color_pal(7)

#define time stamp format
data$datetime_CW = as.POSIXct(data$datetime_CW, format = "%m/%d/%Y %H:%M")
data$datetime_NFEC = as.POSIXct(data$datetime_NFEC, format = "%m/%d/%Y %H:%M")

#define plot params
linesize = 0.3

#plot the base
plot_cw = ggplot(data = data)+
  geom_line(aes(x = datetime_CW, y = CW1), color = cw_color[1], size = linesize)+
  theme_bw(base_size = 16)+
  xlab(NULL)+
  ylab("Water Level (m)")+
  ylim(0,2)+
  geom_text(label = '(a)', aes(x = data$datetime_CW[1], y = 1.9))

#add lines
for(i in 1:5){
  data_i = data.frame(x = data$datetime_CW, y = data[,i+2])
  plot_cw = plot_cw +
    geom_line(data = data_i, aes(x = x, y = y), color = cw_color[i+1], size = linesize)
}
  
#plot the base
plot_nfec = ggplot(data = data)+
  geom_line(aes(x = datetime_NFEC, y = NFEC_1), color = nfec_color[1], size = linesize)+
  theme_bw(base_size = 16)+
  xlab("Time")+
  ylab("Water Level (m)")+
  ylim(0,2)+
  geom_text(label = '(b)', aes(x = data$datetime_NFEC[1], y = 1.9))

#add lines
for(i in 1:6){
  data_i = data.frame(x = data$datetime_NFEC, y = data[,i+9], size = linesize)
  plot_nfec = plot_nfec +
    geom_line(data = data_i, aes(x = x, y = y), color = nfec_color[i+1])
}

#organize plots and export
cowplot::plot_grid(plot_cw, plot_nfec, nrow = 2)
ggsave("~/sloping_slab/figures/water_levels.png",width = 6, height = 6, units = 'in', dpi = 600)
