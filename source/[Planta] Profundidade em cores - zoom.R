
# -------------------------------------------------------------------------
# Transformação da base de dados
# -------------------------------------------------------------------------

dic <- data.frame(cenario = unique(df$cenario), cenario_n = seq(1:length(unique(df$cenario))))
df2 <- left_join(df, dic)



for(j in unique(df2$cenario_n)){
  df_filter <- df2 %>% 
    filter(cenario_n == j)
  plot1 <- 
    ggplot (data = NULL)+
    geom_point(data = df_filter, 
               aes(x = x, y = y, color = z), 
               size =0.05, alpha = 0.5)+
    scale_color_gradientn (colors = rainbow (12))+
    geom_path(data = contorno, 
              aes(x = x,y = y), 
              color = 'black', size =0.2)+
    labs (
      x = "Coordenada x",
      y = "Coordenada y",
      color = "Profundidade\nda partícula (m)")+
    coord_fixed()+
    scale_x_continuous(limits = c(314000,321000), breaks = c(316000,318000,320000), expand = c(0,0))+
    scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
    ggtitle(df_filter$cenario[1])
  
  ggsave(
    filename = paste0(gsub('/', ' - ', nome)," - ",df_filter$cenario[1],".png"),
    path = nRTools::createDir("img/[Planta] Profundidade em cores - zoom"),
    plot = plot1,
    device = "png",
    width = 25,
    height = 25,
    units = "cm"
  )
  j <- j+1
}

