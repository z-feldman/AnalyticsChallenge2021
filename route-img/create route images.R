library(ggplot2)

##### create images of the route tree
tree_df <- read.csv('route-img/route tree.csv', stringsAsFactors = F)

ggplot(tree_df, aes(x = x, y = y, group = Route)) +
  geom_path(size = 4, arrow = arrow(type = 'closed', angle = 30, length = unit(0.4, 'inches'))) +
  theme_void() 
ggsave('route-img/full route tree.png', width = 5, height = 5 * (16/9), bg = 'transparent', dpi = 1000)


for (i in unique(tree_df$Route)) {
  ggplot(tree_df, aes(x = x, y = y, group = Route)) +
    geom_path(size = 4, color = 'grey10', arrow = arrow(type = 'closed', angle = 30, length = unit(0.4, 'inches'))) +
    geom_path(data = tree_df[which(tree_df$Route==i),], size = 4, color = 'red', arrow = arrow(type = 'closed', angle = 30, length = unit(0.4, 'inches'))) +
    theme_void() 
  
  ggsave(paste0('route-img/',i,'.png'), width = 5, height = 5 * (16/9), bg = 'transparent', dpi = 1000)
}
