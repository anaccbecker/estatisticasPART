
# -------------------------------------------------------------------------
# Transformação da base de dados
# -------------------------------------------------------------------------

dic <- data.frame(cenario = unique(df$cenario), cenario_n = seq(1:length(unique(df$cenario))))
df2 <- left_join(df, dic)


# Loop dos gráficos

for(j in unique(df2$cenario_n)){
  df_filter <- df2 %>% 
    filter(cenario_n == j)
  df_split <-df_filter %>% 
      group_by(particula, cenario, Destino) %>% 
      group_modify(~add_blank(., 1))
  plot1 <- 
    ggplot (data = NULL)+
    geom_path(data = df_split, 
                aes(x = x, y = y,  color = Destino), 
                size =0.05, alpha = 0.5)+ 
    geom_point(data = group_by(df_filter, particula), 
               aes(x = x, y = y,  color = Destino), 
               size =0.3)+
    geom_path(data = contorno, 
              aes(x = x,y = y), 
              color = 'black', size =0.2)+
    labs (
      x = "Coordenada x",
      y = "Coordenada y")+
    coord_fixed()+
    scale_x_continuous(limits = c(314000,321000), breaks = c(316000,318000,320000), expand = c(0,0))+
    scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
    scale_color_manual(values= c("CFME"="#49D4C6","CFMD"="#FEDB5D","VER"="#A096C5","VT"="#ED6E85","Reservatório"="#78BC2F"), drop = FALSE)+
    guides(color = guide_legend(override.aes = list(size=3)))+
    ggtitle(paste("Cenário",df_filter$cenario[1]))
  
  ggsave(
    filename = paste0(gsub('/', ' - ', nome)," - ",df_filter$cenario[1],".png"),
    path = nRTools::createDir("img/[Planta] Destino em cores - zoom"),
    plot = plot1,
    device = "png",
    width = 25,
    height = 25,
    units = "cm"
  )
  j <- j+1
}

