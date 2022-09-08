
# -------------------------------------------------------------------------
# Transformação da base de dados
# -------------------------------------------------------------------------

dic <- data.frame(cenario = unique(df$cenario), cenario_n = seq(1:length(unique(df$cenario))))
df2 <- left_join(df, dic)


# Loop dos gráficos

for(j in unique(df2$cenario_n)){
  df_filter <- df2 %>% 
    filter(cenario_n == 1)
  df_split <-df_filter %>% 
      group_by(particula, cenario, Destino) %>% 
      group_modify(~add_blank(., 1))
  plot1 <- 
    ggplot (data = NULL)+
    geom_path(data = df_split, 
                aes(x = x, y = y), 
                size =0.05, alpha = 0.5,  color = 'grey')+  
    geom_point(data = df_filter, 
               aes(x = x, y = y, color = z), 
               size =0.02, alpha = 0.5)+
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
    ggtitle(paste("Cenário",df_filter$cenario[1]))+ 
    theme(legend.position="right")+
    guides(colour=guide_colourbar(barheight=35,label.position="right",nbin = 30))
  
  plot1
  ggsave(
    filename = paste0(gsub('/', ' - ', nome)," - ",df_filter$cenario[1],".png"),
    path = nRTools::createDir("img/[Planta] Profundidade em cores - zoom"),
    plot = plot1,
    device = "png",
    width = 25,
    height = 25,
    units = "cm",
    dpi = 500
  )
  j <- j+1
}

