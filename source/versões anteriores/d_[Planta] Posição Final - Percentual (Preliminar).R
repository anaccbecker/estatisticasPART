
# CFME --------------------------------------------------------------------


df_secao <- .FiltraSecao(df, 318975, 8977000, 318711, 8977800, delta = 300)


plot1 <- 
    ggplot (data = NULL)+
    geom_point(data = df, 
               aes(x = x, y = y, color = z), 
               size =0.1, alpha = 0.5)+
    scale_color_gradientn(colors = c("#004d00","#ccffcc"))+
    geom_point(data = df_secao, 
               aes(x = x, y = y), 
               size =0.5, alpha = 0.5, color = '#ac39ac')+
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
    scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
    theme()

plot1

# CFMD -------------------------------------------------------------------------


df_secao <- .FiltraSecao(df, 320000, 8974450, 319152, 8975060, delta = 40)


plot1 <- 
    ggplot (data = NULL)+
    geom_line(data = df, 
               aes(x = x, y = y, group = particula), 
               size =0.1, alpha = 0.5)+
    geom_point(data = df_secao, 
               aes(x = x, y = y), 
               size =0.5, alpha = 0.5, color = '#ac39ac')+
    geom_path(data = contorno, 
              aes(x = x,y = y), 
              color = 'black', size =0.2)+
    #geom_path(data = usina, 
    #          aes(x = x,y = y), 
    #          color = 'black', size =0.8)+
    labs (title = "(b) Reservatório em planta\n(seção transversal* em roxo)",
          x = "Coordenada x",
          y = "Coordenada y")+
    coord_fixed()+
    #theme(legend.position = "none")+
    scale_x_continuous(limits = c(314000,321000), expand = c(0,0))+
    scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
    facet_wrap(.~cenario, nrow=2)

plot1

# Vertedor -------------------------------------------------------------------------


df_secao <- .FiltraSecao(df, 319152, 8975060,318625, 8975378 , delta = 40)


plot1 <- 
    ggplot (data = NULL)+
    geom_point(data = df, 
               aes(x = x, y = y, color = z), 
               size =0.1, alpha = 0.5)+
    scale_color_gradientn(colors = c("#004d00","#ccffcc"))+
    geom_point(data = df_secao, 
               aes(x = x, y = y), 
               size =0.5, alpha = 0.5, color = '#ac39ac')+
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
    scale_x_continuous(limits = c(318000,319000), expand = c(0,0))+
    scale_y_continuous(limits = c(8975000,8976000), expand = c(0,0))+
    theme()

plot1

# Vertedor de troncos -------------------------------------------------------------------------


df_secao <- .FiltraSecao(df, 318750, 8975750,318750, 8976100 , delta = 125)

plot1 <- 
    ggplot (data = NULL)+
    geom_point(data = df, 
               aes(x = x, y = y, color = z), 
               size =0.1, alpha = 0.5)+
    scale_color_gradientn(colors = c("#004d00","#ccffcc"))+
    geom_point(data = df_secao, 
               aes(x = x, y = y), 
               size =0.5, alpha = 0.5, color = '#ac39ac')+
    geom_path(data = contorno, 
              aes(x = x,y = y), 
              color = 'black', size =0.2)+
    labs (x = "Coordenada x",
          y = "Coordenada y")+
    coord_fixed()+
    scale_x_continuous(limits = c(318000,319000), expand = c(0,0))+
    scale_y_continuous(limits = c(8974500,8976250), expand = c(0,0))


plot1

# mapa

plot1 <- 
    ggplot (data = NULL)+
    geom_path(data = contorno, 
              aes(x = x,y = y), 
              color = 'black', size =0.2)+
    labs (x = "Coordenada x",
          y = "Coordenada y")+
    coord_fixed()+
    scale_x_continuous(limits = c(318250,319000), expand = c(0,0))+
    scale_y_continuous(limits = c(8975700,8976250), expand = c(0,0))+
    theme()

plot1

# Seção problemática -------------------------------------------------------------------------


df_secao <- .FiltraSecao(df, 318625, 8975375,318625, 8975500 , delta = 125)


plot1 <- 
    ggplot (data = NULL)+
    geom_point(data = df, 
               aes(x = x, y = y, color = z), 
               size =0.1, alpha = 0.5)+
    scale_color_gradientn(colors = c("#004d00","#ccffcc"))+
    geom_point(data = df_secao, 
               aes(x = x, y = y), 
               size =0.5, alpha = 0.5, color = '#ac39ac')+
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
    scale_x_continuous(limits = c(318000,319000), expand = c(0,0))+
    scale_y_continuous(limits = c(8975000,8976000), expand = c(0,0))+
    theme()

plot1
