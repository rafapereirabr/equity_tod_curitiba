source('./R/setup.R')

# devtools::install_github("BlakeRMills/MetBrewer") 
library(MetBrewer)

#' de todos os 20% dos imoveis mais caros, XX % estao até 1km do BRT
#' de todos os 20% da pop mais rica, XX % estao até 1km do BRT


#### 1. load data -----------------------

#### *1.1 context data -----------------------

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




#### *1.2 land use and accessibility data -----------------------

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
  
 

 
#### *1.3 recode data -----------------------

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
  legend.key.width = unit(.35, "cm"),
  plot.background = element_rect(fill = "white", colour = NA)
)

  
  
# #### ***Fig.2 - transport network
# 
# map_transp <- ggplot() + 
#               geom_raster(data = map_tile2, aes(x, y, fill = hex), alpha = 1) +
#               scale_fill_identity() +
#               coord_equal() +
#               geom_sf(data=muni, fill=NA, color='black') +
#               geom_sf(data=pt, aes(color=type, size=type, group=type),  show.legend = "line") +
#               scale_size_manual(values=c(.1, .5, .5), labels=c("Bus", "BRT", 'Future BRT'), guide = 'none') +
#               scale_color_manual(values=c("gray60", "#2171b5", "#238b45"), labels=c("Bus", "BRT", 'Future\nBRT')) +
#               theme_void() +
#               labs(color='') +
#               theme(
#                 # Legends
#                 # legend.title=element_blank(),
#                 legend.position=c(0.85, 0.2), # horz vert
#                 legend.direction='vertical',
#                 legend.box='vertical',
#                 legend.text=element_text(size=8),
#                 legend.key.size = unit(.6, "cm"),
#                 legend.key.width = unit(.8, "cm"))
#   
# 
#  ggsave(map_transp, filename = './figures/transport_network.png',
#         dpi = 200, width = 16, height = 20, units = 'cm')





#### 2. Prepare data for box-plots -----------------------

###  *2.1 calculate distances from hex cells
gc(reset = T)


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


###  *2.2 calculate distances from real state properties
gc(reset = T)

# find index of closest BRT line
nearest_imb = st_nearest_feature(imobs, df_brt)

# calculate distance to closest BRT line
gc(reset = T)
imobs$dist_to_brt <- st_distance(imobs, df_brt[nearest_imb,], by_element=TRUE)

# to Km
imobs$dist_to_brt <- as.numeric(imobs$dist_to_brt/1000)
summary(imobs$dist_to_brt)



###  *2.3 discretize distances
gc(reset = T)

# real state properties
setDT(imobs)
summary(imobs$dist_to_brt)
imobs[, dist_to_brt_d := cut(x=dist_to_brt, 
                             breaks=c(-1, .3, .6, 1, 2, 3, 5,10, 20),
                             labels=c('0-0.3', '0.3-0.6', '0.6-1.0', '1-2', '1-3', '3-5', '5-10', '10+'))]

table(imobs$dist_to_brt_d, useNA = 'always')

# h3 grid
setDT(df_land)
summary(df_land$dist_to_brt)
df_land[, dist_to_brt_d := cut(x=dist_to_brt, 
                               breaks=c(-1, .3, .6, 1, 2, 3, 5,10, 20),
                               labels=c('0-.3', '.3-.6', '.6-1', '1-2', '1-3', '3-5', '5-10', '10+'))]
table(df_land$dist_to_brt_d, useNA = 'always')

subset(df_land, is.na(dist_to_brt_d))

# back to sf
df_land <- st_sf(df_land)



#### 3. calculate Access inequalities  -----------------------

# health
access_health_poor <- subset(df_access, R003 %in% 1:4) %>%  weighted.mean(x=.$CMAST60, w=.$P001, na.rm=T)
access_health_rich <- subset(df_access, R003 %in% 10) %>%  weighted.mean(x=.$CMAST60, w=.$P001, na.rm=T)

access_health_rich /  access_health_poor

# jobs
access_jobs_poor <- subset(df_access, R003 %in% 1:4) %>%  weighted.mean(x=.$CMATT60, w=.$P001, na.rm=T)
access_jobs_rich <- subset(df_access, R003 %in% 10) %>%  weighted.mean(x=.$CMATT60, w=.$P001, na.rm=T)

access_jobs_rich / access_jobs_poor





#### fig.3 - Pop density -----------------------
map_pop <- ggplot() +
              geom_sf(data=muni, fill=NA, color='black') +
              geom_sf(data=subset(df_land, P001>0), aes(fill=pop_density), color=NA, alpha=.9) +
              geom_sf(data=subset(pt, type == 'BRT'), color='red', show.legend = FALSE,
                      size=.5, alpha=.9, linetype = "dotted") +
              scale_fill_viridis_c(option = 'viridis') +
              labs(fill='Pop. density\nper Km2') +
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


box_pop <- ggplot(data= subset(df_land, P001>0) ) +
            geom_boxplot(aes(x=dist_to_brt_d, y=pop_density, weight=P001, color=dist_to_brt_d, fill=dist_to_brt_d),
                         show.legend = FALSE, outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
            scale_color_viridis_d(option = 'cividis') +
            scale_fill_viridis_d(option = 'cividis', alpha=.2) +
            # scale_fill_manual(values=met.brewer("Greek" ,8), alpha=.2)+
            # scale_color_manual(values=met.brewer("Greek",8)) +
            labs(x='Distance to nearest BRT corridor (Km)', y="Population density per Km2") +
            theme_minimal() +        
            theme_legend_size3 +
            theme(panel.grid = element_blank(),
                  text = element_text(size=8))



fig3 <- plot_grid(map_pop, box_pop,
                      labels = c('A', 'B'), label_size = 12, ncol=2
                  # , rel_widths = c(2,1)
                  # , rel_heights = c(2,1)
)


ggsave(fig3, filename = './figures/fig3_pop_density.png',
       dpi = 300, width = 20, height = 10, units = 'cm')



#### fig.4 - income  -----------------------
  

map_income <- ggplot() +
              geom_sf(data=muni, fill=NA, color='black') +
               geom_sf(data=subset(df_land, P001>0), aes(fill=as.factor(R003)), color=NA, alpha=.9) +
              # scale_fill_brewer(palette = 'RdBu') +
              geom_sf(data=subset(pt, type == 'BRT'), color='red', show.legend = FALSE,
                      size=.5, alpha=.9, linetype = "dotted") +
              scale_fill_viridis_d(option = 'viridis') +
              labs(fill='Income\ndecile') +
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



box_income <- ggplot(data=subset(df_land, P001>0)) +
                  geom_boxplot(aes(x=dist_to_brt_d, y=R001, weight=P001, color=dist_to_brt_d, fill=dist_to_brt_d),
                               show.legend = FALSE, outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
                  scale_color_viridis_d(option = 'cividis') +
                  scale_fill_viridis_d(option = 'cividis', alpha=.2) +
                  labs(x='Distance to nearest BRT corridor (Km)', y="Average household nincome per capita") +
                  theme_minimal() +        
                  theme_legend_size3 +
                  theme(panel.grid = element_blank(),
                        text = element_text(size=8))



fig4 <- plot_grid(map_income, box_income,
                  labels = c('A', 'B'), label_size = 12, ncol=2
                  # , rel_widths = c(2,1)
                  # , rel_heights = c(2,1)
)


ggsave(fig4, filename = './figures/fig4_income.png',
       dpi = 300, width = 20, height = 10, units = 'cm')







#### Fig 5. A Real state values  -----------------------
  
map_imobs <-
  ggplot() + 
  geom_sf(data=muni, fill=NA, color='black') +
  stat_summary_hex(data=imobs_df, aes(x=x, y=y, z = V2017),
                   binwidth = 200, fun = mean, na.rm = TRUE, alpha=.9) +
  scale_fill_viridis_c(option = 'inferno') +
  # geom_sf(data=subset(pt, type == 'BRT'), color='red', show.legend = FALSE,
  #         size=.3, alpha=.9, linetype = "dotted") +
  
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



box_imobs <- ggplot(data=imobs) +
              geom_boxplot(aes(x=dist_to_brt_d, y=V2017, color=dist_to_brt_d, fill=dist_to_brt_d),
                           show.legend = FALSE, outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
              scale_color_viridis_d(option = 'cividis') +
              scale_fill_viridis_d(option = 'cividis', alpha=.2) +
              labs(x='Distance to nearest BRT corridor (Km)', y="Average value in R$ per m2") +
              #scale_x_discrete(breaks=c('D1\nPoorest', 'D4', 'D6', 'D10\nWealthiest')) +
              theme_minimal() +        
              theme_legend_size3 +
              theme(panel.grid = element_blank(),
                    text = element_text(size=8))




fig5 <- plot_grid(map_imobs, box_imobs,
                  labels = c('A', 'B'), label_size = 12, ncol=2
                  # , rel_widths = c(2,1)
                  # , rel_heights = c(2,1)
)


ggsave(fig5, filename = './figures/fig5_property_values.png',
       dpi = 300, width = 20, height = 10, units = 'cm')










#### Fig 6 Access to jobs-----------------------

f6a_map <- ggplot() +
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


# title='Distribution of the proportion of jobs accessible', color="Income/ndecile",
# subtitle='by public transport in less than 30 min. by income decile'

f6b_box <- ggplot() +
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


fig6 <- plot_grid(f6a_map, f6b_box,
                  labels = c('A', 'B'), label_size = 12,
                  ncol=2)


ggsave(fig6, filename = './figures/fig6_access_jobs.png',
       dpi = 300, width = 16, height = 8, units = 'cm')











#### Fig 7 Access to health-----------------------

f7a_map <- ggplot() +
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


f7b_box <- ggplot() +
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




fig7 <- plot_grid(f7a_map, f7b_box,
                  labels = c('A', 'B'), label_size = 12,
                  ncol=2)


ggsave(fig7, filename = './figures/fig7_access_health.png',
       dpi = 300, width = 16, height = 8, units = 'cm')
