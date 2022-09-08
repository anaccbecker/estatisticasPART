
# Inserindo NAs na tabela

df_split <-df %>% 
  group_by(particula, cenario, Destino) %>% 
  group_modify(~add_blank(., 1))

# Gráfico

plot1 <- 
    ggplot (data = NULL)+
    geom_path(data = df_split, 
              aes(x = x, y = y), 
              size =0.05, alpha = 0.5,  color = 'grey')+  
    geom_point(data = df, 
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
    facet_wrap(.~cenario, nrow=1)


ggsave(
    filename = paste0(gsub('/', ' - ', nome),".png"),
    path = nRTools::createDir("img/[Planta] Profundidade em cores"),
    plot = plot1,
    device = "png",
    width = 35,
    height = 10,
    units = "cm"
)


