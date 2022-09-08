
 # Inserindo NAs na tabela
 
 df_split <-df %>% 
   group_by(particula, cenario, Destino) %>% 
   group_modify(~add_blank(., 1))
 
 # Gráfico
 
plot1 <- 
    ggplot (data = NULL)+
    geom_path(data = df_split, 
               aes(x = x, y = y,  color = Destino), 
               size =0.05, alpha = 0.5)+    
    geom_point(data = group_by(df, particula), 
              aes(x = x, y = y,  color = Destino), 
              size =0.05)+
    geom_path(data = contorno, 
              aes(x = x,y = y), 
              color = 'black', size =0.2)+
    labs (
        x = "Coordenada x",
        y = "Coordenada y")+
    coord_fixed()+
    scale_x_continuous(limits = c(314000,321000), breaks = c(316000,318000,320000), expand = c(0,0))+
    scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
    facet_wrap(.~cenario, nrow=1)+
  scale_color_manual(values= c("CFME"="#49D4C6","CFMD"="#FEDB5D","VER"="#A096C5","VT"="#ED6E85","Reservatório"="#78BC2F"), drop = FALSE)+
  guides(color = guide_legend(override.aes = list(size=3, shape = 19)))


ggsave(
    filename = paste0(gsub('/', ' - ', nome),".png"),
    path = nRTools::createDir("img/[Planta] Destino em cores"),
    plot = plot1,
    device = "png",
    width = 35,
    height = 10,
    units = "cm"
)
 
## Apenas as que ficaram no reservatório
#plot1 <- 
#  ggplot (data = NULL)+
#  geom_path(data = filter(df_split, Destino == "Reservatório"), 
#            aes(x = x, y = y,  color = Destino), 
#            size =0.05, alpha = 0.5)+   
#  geom_point(data = group_by(filter(df, Destino == "Reservatório"), particula), 
#             aes(x = x, y = y,  color = Destino), 
#             size =0.05)+
#  geom_path(data = contorno, 
#            aes(x = x,y = y), 
#            color = 'black', size =0.2)+
#  labs (
#    x = "Coordenada x",
#    y = "Coordenada y")+
#  coord_fixed()+
#  scale_x_continuous(limits = c(314000,321000), breaks = c(316000,318000,320000), expand = c(0,0))+
#  scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
#  facet_wrap(.~cenario, nrow=2)+
#  scale_color_manual(values= c("CFME"="#49D4C6","CFMD"="#FEDB5D","VER"="#A096C5","VT"="#ED6E85","Reservatório"="#78BC2F"), drop = FALSE)+
#  guides(color = guide_legend(override.aes = list(size=3, shape = 19)))
#
#ggsave(
#  filename = paste0(gsub('/', ' - ', nome)," - reservatório.png"),
#  path = nRTools::createDir("img/[Planta] Destino em cores"),
#  plot = plot1,
#  device = "png",
#  width = 25,
#  height = 15,
#  units = "cm"
#)
#