.tmin <- min(df$t)

plot1 <- 
  ggplot (data = NULL)+   
  geom_point(data = group_by(df, particula), 
            aes(x = x, y = y,  color = Destino), 
            size =0.5)+
  geom_path(data = contorno, 
            aes(x = x,y = y), 
            color = 'black', size =0.2)+
  coord_fixed()+
  scale_x_continuous(limits = c(314000,321000), breaks = c(316000,318000,320000), expand = c(0,0))+
  scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
  facet_wrap(.~cenario, nrow=2)+
  scale_color_manual(values= c("CFME"="#49D4C6","CFMD"="#FEDB5D","VER"="#A096C5","VT"="#ED6E85","Reservatório"="#78BC2F"), drop = FALSE)+
  guides(color = guide_legend(override.aes = list(size=3)))+
  transition_time(24*(df$t-.tmin)) +
  ease_aes('linear')+
  labs (
    x = "Coordenada x",
    y = "Coordenada y",
    color = "Destino\nda partícula (m)",
    title = 'Tempo: {as.integer(frame_time)}h')

animate(
  plot1, 
  duration = 20, 
  fps = 10, 
  width = 25, 
  height = 15, 
  units = 'cm', 
  res = 300, 
  renderer = gifski_renderer())

anim_save(
  filename = paste0(gsub('/', ' - ', nome),".gif"),
  path = nRTools::createDir("gif/[Planta] Destino em cores"))
