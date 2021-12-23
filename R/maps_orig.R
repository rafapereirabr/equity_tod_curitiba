source('./R/setup.R')




#### read context data -----------------------

# Admin boundaries
code_cur <- geobr::lookup_muni(name_muni='Curitiba')$code_muni
muni <- geobr::read_municipality(code_muni = code_cur)

# roads
  # download from OSM 
  bb <- st_bbox(muni)
  not_so_much_data <- opq(bbox = 'city of curitiba') %>%
    add_osm_feature(key = 'highway') %>%
    osmdata_sf()
  roads <- not_so_much_data$osm_lines

  # crop
  roads <- st_transform(roads, 3857)
  muni <- st_transform(muni, 3857)
  roads <- roads[muni,]  

  # ggplot() + 
  #   geom_sf(data=muni, color='red') +
  #   geom_sf(data=roads, color='gray')


# transport network
df_brt <- read_rds('./data/transport/brt_sf.rds')
df_brt_new <- read_rds('./data/transport/brt_new_sf.rds')
df_bus <- read_rds('./data/transport/buses_sf.rds')


# maptile
map_tile1 <-  read_rds('./data/maptile/maptile_crop_gmaps_cur_2019.rds')
map_tile2 <-  read_rds('./data/maptile/maptile_crop_mapbox_cur_2019.rds')


#### read land use and accessibility data -----------------------

# Population and land use data
  df_land <- aopdata::read_landuse(city='cur', geometry = T)
  df_access <- aopdata::read_access(city='Curitiba',
                                    mode='public_transport',
                                    peak = T,
                                    year=2019,
                                    showProgress = F, 
                                    geometry = T)
# real state
  imobs <- read_rds('./data/real_state/planta_valores.rds')

# reproject to use map tiles
  df_brt <- st_transform(df_brt, 3857)
  df_brt_new <- st_transform(df_brt_new, 3857)
  df_bus <- st_transform(df_bus, 3857)
  df_land <- st_transform(df_land, 3857)
  df_access <- st_transform(df_access, 3857)
  muni <- st_transform(muni, 3857)
  
  # real state
  imobs <- st_transform(imobs, 3857)
  imobs_df <- sfheaders::sf_to_df(imobs, fill = T)
  

# only main bus lines within Curitiba
  table(df_bus2$CATEGORIA)
  df_bus2 <- st_intersection(df_bus, muni)
  df_bus2 <- subset(df_bus2, CATEGORIA != 'METROPOLITANO')
  df_bus2 <- subset(df_bus2, CATEGORIA != 'TURISMO')
  df_bus2 <- subset(df_bus2, CATEGORIA != 'MADRUGUEIRO')

# rbind transport network
  df_brt$type <- 'BRT'
  df_brt_new$type <- 'New BRT'
  df_bus2$type <- 'Buses'
  df_brt <- select(df_brt, type, geometry)
  df_brt_new <- select(df_brt_new, type, geometry)
  df_bus2 <- select(df_bus2, type, geometry)
  
  pt <- rbind(df_brt, df_brt_new, df_bus2) 
  pt$type <- factor(pt$type, levels = c('Buses', 'BRT', 'New BRT'))
  
#### recode data -----------------------

# replace NA with 0


  
  
  
#### Fig.1 -----------------------
  
#### ***Fig.1 A - transport network -----------------------

map_transp <- ggplot() + 
              geom_raster(data = map_tile2, aes(x, y, fill = hex), alpha = 1) +
              scale_fill_identity() +
              coord_equal() +
              geom_sf(data=muni, fill=NA, color='black') +
              geom_sf(data=pt, aes(color=type, size=type, group=type), alpha=.5, show.legend = "line") +
              scale_size_manual(values=c(.1, .5, .5), labels=c("Bus", "BRT", 'New BRT'), guide = FALSE) +
              scale_color_manual(values=c("gray60", "#2171b5", "#238b45"), labels=c("Bus", "BRT", 'New\nBRT')) +
              theme_void() +
              labs(color='') +
              theme(
                # Legends
                # legend.title=element_blank(),
                legend.position=c(0.85, 0.2), # horz vert
                legend.direction='vertical',
                legend.box='vertical',
                legend.text=element_text(size=8),
                legend.key.size = unit(.6, "cm"),
                legend.key.width = unit(.8, "cm"))
  

# ggsave(map_transp, filename = './figures/t.png',
#        dpi = 200, width = 16, height = 20, units = 'cm')


#### ***fig.1 B - Pop density -----------------------
map_pop <- ggplot() +
              geom_sf(data=muni, fill=NA, color='black') +
              geom_sf(data=subset(df_land, P001>0), aes(fill=P001), color=NA, alpha=.9) +
              scale_fill_viridis_c(option = 'viridis') +
              labs(fill='Population') +
              theme_void() +
              ggsn::scalebar(data = muni,
                             dist = 5, 
                             dist_unit = "km", 
                             st.dist = 0.03,
                             transform = FALSE, 
                             model = "WGS84", 
                             st.size = 2.5, height=0.01,
                             border.size = 0.3)
                             # anchor  = c(x=-49.2, y=-26.65))
                            #y.min=-26.65, y.max=-26.65, x.max=-49.2, x.min= -49.2)



#### ***fig.1 c - income  -----------------------
  
map_renda <- ggplot() +
              geom_sf(data=muni, fill=NA, color='black') +
              geom_sf(data=subset(df_land, P001>0), aes(fill=as.factor(R003)), color=NA, alpha=.9) +
              # scale_fill_brewer(palette = 'RdBu') +
              scale_fill_viridis_d(option = 'cividis') +
              labs(fill='Income\ndecile') +
              theme_void()



#### ***fig.1 D - chart -----------------------


  # find index of closest BRT line
  nearest = st_nearest_feature(df_land, df_brt)
  # calculate distance to closest BRT line
  df_land$dist_to_brt <- st_distance(df_land, df_brt[nearest,], by_element=TRUE)

  # to Km
  df_land$dist_to_brt <- as.numeric(df_land$dist_to_brt/1000)

# chart
chart_1d <- ggplot(data=subset(df_land, P001>0), aes(x=dist_to_brt, y=R001, color=dist_to_brt)) +
            geom_point(alpha=.4, show.legend = FALSE) +
            scale_color_viridis_c(option = 'inferno', direction = -1) +
            # geom_smooth(method="glm", se=FALSE, linetype = 2) +
            labs(y='Average household\nincome per capita',
                 x='Distance to nearest\nBRT corridor (Km)',
                 color='Distance\nto BRT') +
            # scale_y_continuous(trans='log10') +
            theme_minimal()


ggplot(data=subset(df_land, P001>0), aes(x=dist_to_brt, y=P001, color=dist_to_brt)) +
  geom_point(alpha=.4, show.legend = FALSE) +
  scale_color_viridis_c(option = 'inferno', direction = -1) +
  # geom_smooth(method="glm", se=FALSE, linetype = 2) +
  labs(y='Average household\nincome per capita',
       x='Distance to nearest\nBRT corridor (Km)',
       color='Distance\nto BRT') +
  # scale_y_continuous(trans='log10') +
  theme_minimal()

# alternative hex
    ggplot(data=subset(df_land, P001>0), aes(x=dist_to_brt, y=P001)) +
      geom_hex(bins = 100) +
      scale_fill_viridis_c(option = 'inferno', direction = 1) +
    # geom_smooth(method="glm", se=FALSE, linetype = 2) +
    labs(y='Average household\nincome per capita',
         x='Distance to nearest\nBRT corridor (Km)',
         color='Distance\nto BRT') +
    # scale_y_continuous(trans='log10') +
    theme_minimal()

    
#### ***fig 1 save -----------------------

fig1 <- plot_grid(map_transp, map_pop, map_renda, chart_1d,
                   labels = c('A', 'B', 'C', 'D'), label_size = 12,
                    ncol=2)


ggsave(fig1, filename = './figures/fig_1.png',
       dpi = 300, width = 16, height = 16, units = 'cm')




#### Fig 2 -----------------------

#### *** Fig 2. A Real state values  -----------------------

temp_raster <-
   ggplot() + 
  geom_sf(data=muni, fill=NA, color='black') +
   geom_raster(data=imobs_df, aes(x=x, y=y, z = V2017),
               binwidth = 70, stat = "summary_2d", fun = mean, na.rm = TRUE, alpha=.9) +
  scale_fill_viridis_c(option = 'inferno') +
  geom_sf(data=subset(pt, type=='BRT'), aes(color=type, size=type, group=type), 
          alpha=.3, show.legend = "line") +
  scale_size_manual(values=c(.5), labels=c("BRT"), guide = FALSE) +
  scale_color_manual(values=c("#2171b5"), labels=c("BRT")) + # #2171b5
  labs(fill="Average\nvalue in R$", color="") +
  theme_void() +
  theme(
    # Legends
    # legend.title=element_blank(),
    legend.title=element_text(size=9),
    legend.text=element_text(size=7),
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(.6, "cm"))


# ggsave(temp_raster, filename = './figures/temp_raster70_inferno.png',
#        dpi = 300, width = 16, height = 16, units = 'cm')



# # akima interpolate values
# df3 <- sfheaders::sf_to_df(imobs, fill = T)
# head(df3)
# values_interp <- with(subset(df3, !is.na(V2017)), interp(x , y, V2017, duplicate = 'strip')) %>%
#                   with(cbind(V2017=as.vector(z),  # Column-major order
#                              x=rep(x, times=length(y)),
#                              y=rep(y, each=length(x)))) %>%
#                   as.data.frame() %>% 
#                   na.omit()
# 
# head(values_interp)
# 
# temp_akima <- ggplot(values_interp) +
#  # geom_sf(data=muni, fill=NA, color='black') +
#   geom_contour_filled(aes(x=x, y=y, z=V2017), alpha=.8) 
# 
#  ggsave(temp_akima, filename = './figures/temp_akima.png',
#         dpi = 200, width = 16, height = 16, units = 'cm')


#### ***fig.2 B - chart dist to BRT -----------------------


# find index of closest BRT line
nearest_imb = st_nearest_feature(imobs, df_brt)
# calculate distance to closest BRT line
imobs$dist_to_brt <- st_distance(imobs, df_brt[nearest_imb,], by_element=TRUE)

# to Km
imobs$dist_to_brt <- as.numeric(imobs$dist_to_brt/1000)

# chart
chart_2B <- ggplot(data=imobs, aes(x=dist_to_brt, y=V2017, color=dist_to_brt)) +
  geom_point(alpha=.2, show.legend = FALSE) +
  scale_color_viridis_c(option = 'inferno', direction = -1) +
  # geom_smooth(method="glm", se=FALSE, linetype = 2) +
  labs(y='Average value in R$',
       x='Distance to nearest\nBRT corridor (Km)',
       color='Distance\nto BRT') +
  # scale_y_continuous(trans='log10') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())



# alternative hexbin
ggplot(data=imobs, aes(x=dist_to_brt, y=V2017)) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c(option = 'inferno', direction = 1) +
  labs(y='Average value in R$',
       x='Distance to nearest\nBRT corridor (Km)',
       fill='Number of\nproperties') +
  # scale_y_continuous(trans='log10') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())  

# ggsave(chart_2B, filename = './figures/chart_2B.png',
#        dpi = 300, width = 16, height = 16, units = 'cm')
# 



#### ***fig 2 save -----------------------

fig2 <- plot_grid(temp_raster, chart_2B,
                  labels = c('A', 'B'), label_size = 12,
                  ncol=2)

ggsave(fig2, filename = './figures/fig_2.png',
       dpi = 300, width = 16, height = 8, units = 'cm')







#### Fig 4 -----------------------

#### *** Fig 4. ineq access to jobs  -----------------------

# title='Distribution of the proportion of jobs accessible', color="Income/ndecile",
# subtitle='by public transport in less than 30 min. by income decile'

df_access$R003 <- factor(x = df_access$R003,
                         levels = 1:10, 
                         labels = c("D1\nPoorest", paste0('D', 2:9), "D10\nWealthiest"))

fig_4A <- ggplot() +
          geom_boxplot(data=subset(df_access, !is.na(R003)),
                       aes(x = factor(R003), y=CMAET60, color=factor(R003))) +
          scale_color_brewer(palette = 'RdBu') +
          labs(x='Income decile', y="Proportion of\njobs accessible", color='Income\ndecile') +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          theme(panel.grid.major.x = element_blank())
        


fig4 <- plot_grid(temp_raster, fig_4A,
                  labels = c('A', 'B'), label_size = 12,
                  ncol=2)


ggsave(fig4, filename = './figures/fig_4A.png',
       dpi = 300, width = 16, height = 8, units = 'cm')



#### *** Fig 4. ineq access to hospitals  -----------------------
ggplot() +
  geom_boxplot(data=subset(df_access, !is.na(R003)),
               aes(x = factor(R003), y=CMASM60, color=factor(R003))) +
  scale_color_brewer(palette = 'RdBu') +
  labs(title='Distribution of the proportion of jobs accessible', color="Income/ndecile",
       subtitle='by public transport in less than 30 min. by income decile',
       x='Income decile', y="Accessibility") +
  scale_x_discrete(labels=c("D1 Poorest", paste0('D', 2:9), "D10 Wealthiest")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
