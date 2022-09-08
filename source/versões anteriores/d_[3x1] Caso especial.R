
dfc <- df %>% filter(cenario == "1") %>% select(1:6)

a <- .FiltraSecao(dfc, 319000, 8973000,317000, 8975000)
.FiltraSecao(dfc, 317280.0, 8973754.0, 318046.0, 8975018.0)

df_secao1 <- .FiltraSecao(dfc, 317280, 8973754, 318046, 8975018, 100)
df_secao2 <- .FiltraSecao(dfc, 318046, 8975018, 318496, 8975959, 100)
df_secao3 <- .FiltraSecao(dfc, 318496, 8975959, 318843, 8976066, 100)


df_secao <- union(df_secao1,a)


# Seção 11: Longitudinal - log boom
df_secao <- .FiltraSecao(dfc, 316384.94, 8973217.94, 319000, 8976000, 10000)
fundo <- .readFundo("csv/Seções/316384.94, 8973217.94, 319483.75, 8975146.48.csv")

df_secao <-  data.frame(x = NA, y = NA, z= NA, t = NA, particula = NA)
  x1 <- 316970
  y1 <- 8974134
  x3 <- 318898.9
  y3 <- 8976117
  delta <- 40
  x2 <- x1 + delta                     
  y2 <- y1 + delta
  x4 <- x3 + delta
  y4 <- y3 + delta
  m <- (y1-y3)/(x1-x3)
  m2<- (y1-y2)/(x1-x2)
  df_secao <- filter(df, y > y3+m*(x-x3) & 
                       y < y4+m*(x-x4) &
                       y > min(y1,y3) &
                       y < max(y1,y3)) %>% 
    mutate(Secao = paste(round(x1,0),round(y1,0),round(x3,0),round(y3,0),delta),
           x1=x1,
           y1=y1,
           x3=x3,
           y3=y3
    )
  df_secao
  
  
plot1 <- 
  ggplot (data = NULL)+
  geom_point(data = dfc, 
             aes(x = x, y = y, color = z), 
             size =0.1, alpha = 0.5)+
  scale_color_gradientn(colors = c("#004d00","#ccffcc"))+
  #geom_point(data = df_secao, 
  #           aes(x = x, y = y), 
  #           size =0.5, alpha = 0.5, color = '#ac39ac')+
  geom_line(aes(x = c(df_secao$x1[1],df_secao$x3[1]),
                y = c(df_secao$y1[1],df_secao$y3[1])),
            size =1.3,  color = '#ac39ac')+
  geom_path(data = lbl1, 
            aes(x = x,y = y), 
            color = 'black', size =0.8)+
  geom_path(data = lbl2, 
            aes(x = x,y = y), 
            color = 'black', size =0.8)+
  geom_path(data = contorno, 
            aes(x = x,y = y), 
            color = 'black', size =0.2)+
  #geom_path(data = usina, 
  #          aes(x = x,y = y), 
  #          color = 'black', size =0.8)+
  labs (title = "(b) Reservatório em planta\n(seção transversal* em roxo)",
        x = "Coordenada x",
        y = "Coordenada y",
        color = "Profundidade\nda partícula (m)")+
  coord_fixed()+
  #theme(legend.position = "none")+
  scale_x_continuous(limits = c(314000,321000), expand = c(0,0))+
  scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))

plot1

# Seção Transversal -------------------------------------------------------


.tmin <- min(df$t)

plot2 <- df_secao %>% 
  mutate(d1 = sqrt(x1^2+y1^2),
         d3 = sqrt(x3^2+y3^2),
         dp = sqrt(x^2+y^2),
         dm = dp-min(d1,d3),
         dm_max = max(dm),
         dm_min = min(dm)) %>% 
  ggplot ()+    
  geom_line(data = fundo,
            aes(x = .OrientacaoX(dm, dm_max, dm_min, F), y = z), 
            size =0.5, alpha = 0.5, color='black')+
  #geom_line(data = fundo2,
  #          aes(x = .OrientacaoX(dm, dm_max, dm_min, F), y = z), 
  #          size =0.5, alpha = 0.5, color='red')+
  #geom_line(data = fundo3,
  #          aes(x = .OrientacaoX(dm, dm_max, dm_min, F), y = z), 
  #          size =0.5, alpha = 0.5, color='red')+
  #geom_line(aes(x = .OrientacaoX(dm, fundo$dm_max, fundo$dm_min, F), y = z, color = (t-.tmin)*24, group =particula), 
  #           size =0.3)+
  geom_point(aes(x = .OrientacaoX(dm, fundo$dm_max, fundo$dm_min, F), y = z, color = Destino, group =particula), 
             size =1, alpha = 0.8, stroke = 1)+
  scale_y_continuous(breaks = c(0,-20,-40), limits = c(-50,0))+
  scale_color_manual(values= c("CFME"="#49D4C6","CFMD"="#FEDB5D","VER"="#A096C5","VT"="#ED6E85","Reservatório"="#78BC2F"), drop = FALSE)+
  #coord_fixed()+
  #scale_color_gradientn(colors = c("#eb92f0", "black"))+
  #scale_shape_manual( values = c(5,16,18,8,20))+
  labs (title = "(c) Seção transversal*",
        x = " ",
        y = "Profundidade (m)",
        color = "Destino")

#plot2

# Histograma ----------------------------------------------------------------------
##por contagem
#plot3 <- df_secao %>% 
#    ggplot (aes( z, group = particula))+
#    geom_histogram(bins = 50, fill = "#ac39ac")+
#    coord_flip()+
#    labs(x = "Profundidade (m)", y = "Número de partículas")+
#    #scale_y_continuous(limits = c(0,20000))+
#    scale_x_continuous(limits = c(-50,2))+
#    labs(title = "(a) Distribuição das partículas\nna seção transversal*")+
#    theme()
#
#plot3
#

#por percentual


#length(unique(df_secao$t))

tabela <- df_secao %>% 
  mutate(z_group = ifelse(z>=-5, -2.5, ifelse(z>=-10,-7.5, ifelse(z>=-15,-12.5,ifelse(z>=-20, -17.5, ifelse(z>=-25,-22.5, ifelse(z>=-30,-27.5, 
                                                                                                                                 ifelse(z>=-35, -32.5, ifelse(z>=-40, -37.5, iflse(z>=-45,-42.5, ifelse(z>=-50, -47.5))))))))))) %>% 
  group_by(particula, z_group) %>%
  mutate(contagem = n())

tabela2 <- tabela %>% 
  mutate(total_contagem = sum(tabela$contagem),
         percentual = contagem/ total_contagem*100,
         y_pos = max(percentual),
         x_pos = z_group)


plot3 <-  tabela2 %>% 
  ggplot ()+
  geom_bar(aes(x= z_group, y = percentual/100), 
           width = 4.8,
           fill = "#ac39ac",
           stat = "identity")+
  coord_flip()+
  labs(x = "Profundidade (m)", y = "Percentual de partículas (%)")+
  #scale_y_continuous(expand = c(0,1))+
  scale_x_continuous(limits = c(-50,2))+
  labs(title = "(a) Distribuição das partículas\nna seção transversal*")+
  scale_y_continuous(labels = scales::percent_format(accuracy =1.)) 

#plot3


# Gráfico conjunto --------------------------------------------------------

plot <- (plot3+plot1)/plot2+
  plot_annotation(
    title = paste("Cenário ", dfc$cenario[1]),
    #subtitle = "subtitle",
    caption = paste("*Seção transversal entre as coordenadas UTM: (",
                    round(df_secao$x1,0),", ",
                    round(df_secao$y1,0),") e (",
                    round(df_secao$x3,0),", ",
                    round(df_secao$y3,0),")"),
    theme = theme(plot.caption = element_text(hjust = 0.2),
                  plot.title = element_text(hjust = 0.5,face="bold", size = 18),
    ))

ggsave(
  filename = sprintf("Cenario %s [Caso especial].png",dfc$cenario[1]),
  path = nRTools::createDir("img/[3x1] Seção Transversal"),
  plot = plot,
  device = "png",
  width = 20,
  height = 18,
  units = "cm"
)


