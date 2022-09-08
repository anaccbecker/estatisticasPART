# -------------------------------------------------------------------------
# Indicando em quais paineis aparecem as estruturas
# -------------------------------------------------------------------------

# Inserir quais paineis aparece o log boom

.lbl1 <- bind_rows(
    lbl1 %>% mutate(cenario = 'Cenário 1\ncom log boom'),
    lbl1 %>% mutate(cenario = 'Cenário E\ncom log boom'),
    lbl1 %>% mutate(cenario = 'Cenário 1\ncom log boom e desviador/retentor'),
    lbl1 %>% mutate(cenario = 'Cenário E\ncom log boom e desviador/retentor'),
    lbl1 %>% mutate(cenario = 'Cenário B\ncom log boom e desviador/retentor'),
    lbl1 %>% mutate(cenario = 'Cenário F\ncom log boom e desviador/retentor')
)
.lbl1$cenario <- factor(.lbl1$cenario, levels = new_titles)

.lbl2 <- bind_rows(
    lbl2 %>% mutate(cenario = 'Cenário 1\ncom log boom'),
    lbl2 %>% mutate(cenario = 'Cenário E\ncom log boom'),
    lbl2 %>% mutate(cenario = 'Cenário 1\ncom log boom e desviador/retentor'),
    lbl2 %>% mutate(cenario = 'Cenário E\ncom log boom e desviador/retentor'),
    lbl2 %>% mutate(cenario = 'Cenário B\ncom log boom e desviador/retentor'),
    lbl2 %>% mutate(cenario = 'Cenário F\ncom log boom e desviador/retentor')
)
.lbl2$cenario <- factor(.lbl2$cenario, levels = new_titles)

# Inserir quais paineis aparece o retentor/desviador

.rdmd <- bind_rows(
    rdmd %>% mutate(cenario = 'Cenário 1\ncom log boom e desviador/retentor'),
    rdmd %>% mutate(cenario = 'Cenário E\ncom log boom e desviador/retentor'),
    rdmd %>% mutate(cenario = 'Cenário B\ncom log boom e desviador/retentor'),
    rdmd %>% mutate(cenario = 'Cenário F\ncom log boom e desviador/retentor')
)
.rdmd$cenario <- factor(.rdmd$cenario, levels = new_titles)

.rdme <- bind_rows(
    rdme %>% mutate(cenario = 'Cenário 1\ncom log boom e desviador/retentor'),
    rdme %>% mutate(cenario = 'Cenário E\ncom log boom e desviador/retentor'),
    rdme %>% mutate(cenario = 'Cenário B\ncom log boom e desviador/retentor'),
    rdme %>% mutate(cenario = 'Cenário F\ncom log boom e desviador/retentor')
)
.rdme$cenario <- factor(.rdme$cenario, levels = new_titles)



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
    geom_path(data = .lbl1, 
              aes(x = x,y = y), 
              color = '#11612d', size =0.4)+
    geom_path(data = .lbl2, 
              aes(x = x,y = y), 
              color = '#11612d', size =0.4)+
    geom_path(data = .rdmd, 
              aes(x = x,y = y), 
              color = 'black', size =0.4, linetype = 'dashed')+
    geom_path(data = .rdme, 
              aes(x = x,y = y), 
              color = 'black', size =0.4, linetype = 'dashed')+
    labs (
        x = "Coordenada x",
        y = "Coordenada y",
        color = "Profundidade\nda partícula (m)")+
    coord_fixed()+
    scale_x_continuous(limits = c(314000,321000), breaks = c(316000,318000,320000), expand = c(0,0))+
    scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
    facet_wrap(.~cenario, nrow=4)+ 
    theme(legend.position="right")+
    guides(colour=guide_colourbar(barheight=20,label.position="right",nbin = 30)) ## tamanho da legenda


ggsave(
    filename = paste0(gsub('/', ' - ', nome),".png"),
    path = nRTools::createDir("img/[Planta] Profundidade em cores"),
    plot = plot1,
    device = "png",
    width = 20,
    height = 28,
    units = "cm"
)



