.tmin <- min(df$t)

plot1 <- 
  ggplot (data = NULL)+
  geom_point(data = df, 
             aes(x = x, y = y, color = z), 
             size =0.5, alpha = 0.5)+
  scale_color_gradientn (colors = rainbow (12))+
  geom_path(data = contorno, 
            aes(x = x,y = y), 
            color = 'black', size =0.2)+
  coord_fixed()+
  scale_x_continuous(limits = c(314000,321000), breaks = c(316000,318000,320000), expand = c(0,0))+
  scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
  facet_wrap(.~cenario, nrow=1)+
  transition_time(24*(df$t-.tmin)) +
  ease_aes('linear')+
  labs (
    x = "Coordenada x",
    y = "Coordenada y",
    color = "Profundidade\nda partícula (m)",
    title = 'Tempo: {as.integer(frame_time)}h')

animate(
  plot1, 
  duration = 20, 
  fps = 10, 
  width = 25,
  height = 15, 
  units = 'cm', 
  res = 150, 
  renderer = gifski_renderer())

anim_save(
  filename = paste0(gsub('/', ' - ', nome),".gif"),
  path = nRTools::createDir("gif/[Planta] Profundidade em cores"))
