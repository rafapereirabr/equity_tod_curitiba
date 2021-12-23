source('./R/setup.R')


#' de todos os 20% dos imoveis mais caros, XX % estao até 1km do BRT
#' de todos os 20% da pop mais rica, XX % estao até 1km do BRT

#### read context data -----------------------

# Admin boundaries
code_cur <- geobr::lookup_muni(name_muni='Curitiba')$code_muni
muni <- geobr::read_municipality(code_muni = code_cur)


# roads
  # # download from OSM 
  # bb <- st_bbox(muni)
  # not_so_much_data <- opq(bbox = 'city of curitiba') %>%
  #   add_osm_feature(key = 'highway') %>%
  #   osmdata_sf()
  # roads <- not_so_much_data$osm_lines
  # 
  # # crop
  # roads <- st_transform(roads, 3857)
  # muni <- st_transform(muni, 3857)
  # roads <- roads[muni,]  
  # # save
  # write_rds(roads, './data/transport/osm_roads.rds')
roads <- read_rds('./data/transport/osm_roads.rds')

table(roads$highway)

roads2 <- subset(roads, highway %in%  c('footway', 
                                        'motorway', 
                                        'primary', 
                                        'primary_link', 
                                        'residential',
                                        'secondary', 
                                        'secondary_link', 
                                        'trunk', 
                                        'trunk_link'))



ggplot() +
  geom_sf(data=muni, color='red') +
  geom_sf(data=roads2, color='gray')


# BRT network
df_brt_new <- read_rds('./data/transport/brt_new_sf.rds')
df_bus <- read_rds('./data/transport/buses_sf.rds')
df_brt <- read_rds('./data/transport/brt_sf.rds')

# retirar ligeirinho q nao corre em corredor exclusivo
df_brt <- subset(df_brt, CD_LINHA != 'X12')
# mapview(b)



# maptile
map_tile1 <- read_rds('./data/maptile/maptile_crop_gmaps_cur_2019.rds')
map_tile2 <- read_rds('./data/maptile/maptile_crop_mapbox_cur_2019.rds')




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
  head(imobs)

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
  df_bus2 <- st_intersection(df_bus, muni)
  table(df_bus2$CATEGORIA)
  df_bus2 <- subset(df_bus2, CATEGORIA != 'METROPOLITANO')
  df_bus2 <- subset(df_bus2, CATEGORIA != 'TURISMO')
  df_bus2 <- subset(df_bus2, CATEGORIA != 'MADRUGUEIRO')

# rbind transport network
  df_brt$type <- 'BRT'
  df_brt_new$type <- 'Future BRT'
  df_bus2$type <- 'Buses'
  df_brt <- select(df_brt, type, geometry)
  df_brt_new <- select(df_brt_new, type, geometry)
  df_bus2 <- select(df_bus2, type, geometry)
  
  pt <- rbind(df_brt, df_brt_new, df_bus2) 
  pt$type <- factor(pt$type, levels = c('Buses', 'BRT', 'Future BRT'))
  table(pt$type)
  
 

#### Access inequalities  -----------------------
  
# health
 access_health_poor <- subset(df_access, R003 %in% 1:4) %>%  weighted.mean(x=.$CMAST60, w=.$P001, na.rm=T)
 access_health_rich <- subset(df_access, R003 %in% 10) %>%  weighted.mean(x=.$CMAST60, w=.$P001, na.rm=T)

 access_health_rich /  access_health_poor
 
# jobs
  access_jobs_poor <- subset(df_access, R003 %in% 1:4) %>%  weighted.mean(x=.$CMATT60, w=.$P001, na.rm=T)
 access_jobs_rich <- subset(df_access, R003 %in% 10) %>%  weighted.mean(x=.$CMATT60, w=.$P001, na.rm=T)
 
 access_jobs_rich / access_jobs_poor

##
 
 
#### recode data -----------------------

# income decile as factors
  df_access$R003 <- factor(x = df_access$R003,
                           levels = 1:10, 
                           labels = c("D1\nPoorest", paste0('D', 2:9), "D10\nWealthiest"))
  

# legend size
theme_legend_size <- theme(
    # Legends
    # legend.title=element_blank(),
    legend.title=element_text(size=9),
    legend.text=element_text(size=7),
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(.6, "cm"))
  

theme_legend_size2 <- theme(
  # Legends
  # legend.title=element_blank(),
  legend.title=element_text(size=8),
  legend.text=element_text(size=7),
  legend.key.size = unit(.3, "cm"),
  legend.key.width = unit(.35, "cm"),
  legend.position="bottom")

theme_legend_size3 <- theme(
  # Legends
  # legend.title=element_blank(),
  legend.title=element_text(size=8),
  legend.text=element_text(size=7),
  legend.key.size = unit(.3, "cm"),
  legend.key.width = unit(.35, "cm"))

  
#### Fig.1 -----------------------
  
#### ***Fig.1 A - transport network -----------------------

map_transp <- ggplot() + 
              geom_raster(data = map_tile2, aes(x, y, fill = hex), alpha = 1) +
              scale_fill_identity() +
              coord_equal() +
              geom_sf(data=muni, fill=NA, color='black') +
              geom_sf(data=pt, aes(color=type, size=type, group=type), alpha=.5, show.legend = "line") +
              scale_size_manual(values=c(.1, .5, .5), labels=c("Bus", "BRT", 'Future BRT'), guide = FALSE) +
              scale_color_manual(values=c("gray60", "#2171b5", "#238b45"), labels=c("Bus", "BRT", 'Future\nBRT')) +
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
              theme_legend_size3



#### ***fig.1 c - income  -----------------------
  
map_renda <- ggplot() +
              geom_sf(data=muni, fill=NA, color='black') +
              geom_sf(data=subset(df_land, P001>0), aes(fill=as.factor(R003)), color=NA, alpha=.9) +
              # scale_fill_brewer(palette = 'RdBu') +
              scale_fill_viridis_d(option = 'cividis') +
              labs(fill='Income\ndecile') +
              theme_void() +
              theme_legend_size3

#### *** Fig 2. A Real state values  -----------------------
  
map_imobs <-
  ggplot() + 
  geom_sf(data=muni, fill=NA, color='black') +
  stat_summary_hex(data=imobs_df, aes(x=x, y=y, z = V2017),
                   binwidth = 200, fun = mean, na.rm = TRUE, alpha=.9) +
  scale_fill_viridis_c(option = 'inferno') +
    # geom_sf(data=subset(pt, type=='BRT'), aes(color=type, size=type, group=type), 
    #         alpha=.3, show.legend = "line") +
    # scale_size_manual(values=c(.5), labels=c("BRT"), guide = FALSE) +
    # scale_color_manual(values=c("#2171b5"), labels=c("BRT")) + # #2171b5
    labs(fill="Average\nvalue in\nR$ per m2", color="") +
    theme_void() +
    theme_legend_size3 +
    ggsn::scalebar(data = muni,
                   dist = 5, 
                   dist_unit = "km", 
                   st.dist = 0.03,
                   transform = FALSE, 
                   model = "WGS84", 
                   st.size = 2.5, height=0.01,
                   border.size = 0.3)


#### ***fig 1 save -----------------------

fig1 <- plot_grid(map_transp, map_pop, map_renda, map_imobs,
                   labels = c('A', 'B', 'C', 'D'), label_size = 12,
                    ncol=2)


ggsave(fig1, filename = './figures/fig_1_maps.png',
       dpi = 300, width = 16, height = 16, units = 'cm')




#### Fig 2 -----------------------

  # # trend
  # my_formula <- y ~ poly(x, 2, raw = TRUE) # poly
  # my_formula <- y ~ x # expo
  # # https://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/
  #
  # ggplot(data=subset(df_land, P001>0 & pop_density < Inf), aes(x=dist_to_brt, y=pop_density, color=dist_to_brt)) +
  #   geom_point(alpha=.2, show.legend = FALSE) +
  #   scale_color_viridis_c(option = 'viridis', direction = -1) +
  #   geom_smooth(method = "loess", se = TRUE, fill=NA,
  #               formula = my_formula, 
  #               colour = "#2c7fb8", alpha=.1) +
  #   labs(y='Population density per Km2',
  #        x='Distance to nearest\nBRT corridor (Km)',
  #        color='Distance\nto BRT') +
  #   # scale_y_continuous(trans='log10') +
  #   theme_minimal() +
  #   theme(panel.grid.minor = element_blank(),
  #         text = element_text(size=8))
  
  
  
  
  # find index of closest BRT line
  nearest = st_nearest_feature(df_land, df_brt)
  
# calculate distance to closest BRT line
  df_land$dist_to_brt <- st_distance(df_land, df_brt[nearest,], by_element=TRUE)

  # to Km
  df_land$dist_to_brt <- as.numeric(df_land$dist_to_brt/1000)
  summary(df_land$dist_to_brt)
  
  # density
  df_land$areakm2 <- st_area(df_land) %>% as.numeric() / 1000000
  df_land$pop_density <- df_land$P001 / df_land$areakm2 
  

#### ***fig.2 A - Pop density -----------------------
  summary(subset(df_land, P001>0)$pop_density)
  
  # chart_2a <- ggplot(data=subset(df_land, P001>0), aes(x=dist_to_brt, y=pop_density, color=dist_to_brt)) +
  #             geom_point(alpha=.2, show.legend = FALSE) +
  #             scale_color_viridis_c(option = 'viridis', direction = -1) +
  #             geom_smooth(method = "loess", se = F, fill=NA,
  #                         formula = my_formula, 
  #                         colour = "#2c7fb8", alpha=.1) +
  #             labs(y='Population density per Km2',
  #                          x='Distance to nearest\nBRT corridor (Km)',
  #                          color='Distance\nto BRT') +
  #             # scale_y_continuous(trans='log10') +
  #             theme_minimal() +
  #             theme(panel.grid.minor = element_blank(),
  #                   text = element_text(size=8))
  

# alternative hex
chart_2a_hex <- ggplot(data=subset(df_land, P001>0), aes(x=dist_to_brt, y=pop_density, weight=P001)) +
                geom_hex(bins = 80) +
                scale_fill_viridis_c(option = 'viridis', direction = 1) +
                # geom_smooth(method="glm", se=FALSE, linetype = 2) +
                labs(y='Population density per Km2',
                     x='Distance to nearest\nBRT corridor (Km)',
                     fill='Total population') +
                theme_minimal() +
                theme_legend_size2 +
                theme(panel.grid.minor = element_blank(),
                      text = element_text(size=8))



#### ***fig.2 B - income -----------------------
  
    # chart_2b <- ggplot(data=subset(df_land, P001>0), aes(x=dist_to_brt, y=R001, color=dist_to_brt)) +
    #               geom_point(alpha=.2, show.legend = FALSE) +
    #               scale_color_viridis_c(option = 'cividis', direction = -1) +
    #               geom_smooth(method = "loess", se = F, fill=NA,
    #                           formula = my_formula, 
    #                           colour = "#2c7fb8", alpha=.1) +
    #               labs(y='Average household\nincome per capita',
    #                    x='Distance to nearest\nBRT corridor (Km)',
    #                    color='Distance\nto BRT') +
    #               # scale_y_continuous(trans='log10') +
    #               theme_minimal() +
    #               theme(panel.grid.minor = element_blank(),
    #                     text = element_text(size=8))
  
  
# alternative hex
chart_2b_hex <- 
    ggplot(data=subset(df_land, P001>0), aes(x=dist_to_brt, y=P001, weight=P001)) +
    #geom_point(size=0.5, alpha=.1) +
    geom_hex(bins = 80) +
      scale_fill_viridis_c(option = 'cividis', direction = 1) +
    # geom_smooth(method="glm", se=FALSE, linetype = 2) +
    labs(y='Average household\nincome per capita',
         x='Distance to nearest\nBRT corridor (Km)',
         fill='Total population') +
  theme_minimal() +
  theme_legend_size2 +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=8))



#### ***fig.2 C property values  -----------------------


# find index of closest BRT line
nearest_imb = st_nearest_feature(imobs, df_brt)
# calculate distance to closest BRT line
imobs$dist_to_brt <- st_distance(imobs, df_brt[nearest_imb,], by_element=TRUE)

# to Km
imobs$dist_to_brt <- as.numeric(imobs$dist_to_brt/1000)
summary(imobs$dist_to_brt)

# chart
    # chart_2c <- ggplot(data=imobs, aes(x=dist_to_brt, y=V2017, color=dist_to_brt)) +
    #               geom_point(alpha=.2, show.legend = FALSE) +
    #               scale_color_viridis_c(option = 'inferno', direction = -1) +
    #               geom_smooth(method = "loess", se = F, fill=NA,
    #                           formula = my_formula, 
    #                           colour = "#2c7fb8", alpha=.1) +
    #               labs(y='Average property value in R$ per m2',
    #                    x='Distance to nearest\nBRT corridor (Km)',
    #                    color='Distance\nto BRT') +
    #               # scale_y_continuous(trans='log10') +
    #               theme_minimal() +
    #               theme_legend_size2 +
    #               theme(panel.grid.minor = element_blank(),
    #                     text = element_text(size=8))


# alternative hexbin
chart_2c_hex <- 
  ggplot(data=imobs, aes(x=dist_to_brt, y=V2017)) +
  geom_hex(bins = 110) +
  scale_fill_viridis_c(option = 'inferno', direction = 1,
                       labels = function(x) x/1000) +
  labs(y='Average value in R$ per m2',
       x='Distance to nearest\nBRT corridor (Km)',
       fill='Number of\nproperties\nin thousands') +
  # scale_y_continuous(trans='log10') +
  theme_minimal() +
  theme_legend_size +
  theme_legend_size2 +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=8)
        )

# ggsave(chart_2B, filename = './figures/chart_2B.png',
#        dpi = 300, width = 16, height = 16, units = 'cm')
# 



#### ***fig 2 save -----------------------

        # fig2 <- plot_grid(chart_2a, chart_2b, chart_2c,
        #                   labels = c('A', 'B', 'C'), label_size = 12, ncol=3)
        # 
        # ggsave(fig2, filename = './figures/fig_2_dist_brt_loess.png',
        #        dpi = 300, width = 20, height = 8, units = 'cm')


fig2_hex <- plot_grid(chart_2a_hex, chart_2b_hex, chart_2c_hex,
                  labels = c('A', 'B', 'C'), label_size = 12, ncol=3)

ggsave(fig2_hex, filename = './figures/fig_2_dist_brt_hex.png',
       dpi = 300, width = 20, height = 8, units = 'cm')





#### Fig 3 jobs-----------------------

#### *** Fig 3. map  -----------------------

f3a_map <- ggplot() +
            geom_sf(data=muni, fill=NA, color='black') +
            geom_sf(data=subset(df_access, P001>0), aes(fill=CMATT60), color=NA, alpha=.8) +
            scale_fill_viridis_c(option = 'inferno', labels = scales::percent) +
            labs(fill='Access\nto jobs') +
            theme_void() +
            theme_legend_size3 +
            ggsn::scalebar(data = muni,
                           dist = 5, 
                           dist_unit = "km", 
                           st.dist = 0.03,
                           transform = FALSE, 
                           model = "WGS84", 
                           st.size = 1.5, height=0.01,
                           border.size = 0.3)

        


#### *** Fig 3. boxplot  -----------------------

# title='Distribution of the proportion of jobs accessible', color="Income/ndecile",
# subtitle='by public transport in less than 30 min. by income decile'

f3b_box <- ggplot() +
            geom_boxplot(data=subset(df_access, !is.na(R003)),
                         aes(x = factor(R003), y=CMATT60, color=factor(R003)),
                         outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
            scale_color_viridis_d(option = 'cividis') +
            labs(x='Income decile', y="Proportion of jobs accessible", color='Income\ndecile') +
            scale_y_continuous(labels = scales::percent) +
            scale_x_discrete(breaks=c('D1\nPoorest', 'D4', 'D6', 'D10\nWealthiest')) +
            theme_minimal() +        
            theme_legend_size3 +
            theme(panel.grid = element_blank(),
                  text = element_text(size=8))


fig3 <- plot_grid(f3a_map, f3b_box,
                  labels = c('A', 'B'), label_size = 12,
                  ncol=2)


ggsave(fig3, filename = './figures/fig_3.png',
       dpi = 300, width = 16, height = 8, units = 'cm')











#### Fig 4 health-----------------------

#### *** Fig 4. map  -----------------------

f4a_map <- ggplot() +
            geom_sf(data=muni, fill=NA, color='black') +
            geom_sf(data=subset(df_access, P001>0), aes(fill=CMAST60), color=NA, alpha=.8) +
            scale_fill_viridis_c(option = 'inferno', labels = scales::percent) +
            labs(fill='Access to\nhealthcare\nfacilities') +
            theme_void() +
            theme_legend_size3 +
            ggsn::scalebar(data = muni,
                           dist = 5, 
                           dist_unit = "km", 
                           st.dist = 0.03,
                           transform = FALSE, 
                           model = "WGS84", 
                           st.size = 1.5, height=0.01,
                           border.size = 0.3)




#### *** Fig 3. boxplot  -----------------------

f4b_box <- ggplot() +
            geom_boxplot(data=subset(df_access, !is.na(R003)),
                         aes(x = factor(R003), y=CMAST60, color=factor(R003)),
                         outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
            # scale_color_brewer(palette = 'RdBu') +
            scale_color_viridis_d(option = 'cividis') +
            labs(x='Income decile', y="Proportion of health facilities accessible", color='Income\ndecile') +
            scale_y_continuous(labels = scales::percent) +
            scale_x_discrete(breaks=c('D1\nPoorest', 'D4', 'D6', 'D10\nWealthiest')) +
            theme_minimal() +        
            theme_legend_size3 +
            theme(panel.grid = element_blank(),
                  text = element_text(size=8))




fig4 <- plot_grid(f4a_map, f4b_box,
                  labels = c('A', 'B'), label_size = 12,
                  ncol=2)


ggsave(fig4, filename = './figures/fig_4.png',
       dpi = 300, width = 16, height = 8, units = 'cm')
