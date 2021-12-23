library(aopdata)
library(ggplot2)
library(sf)
library(akima)
library(dplyr)
library(sfheaders)
library(akima)


df <- st_read('C:/Users/user/Downloads/PGV/Valores_PGV.shp')
head(df)

accss <- read_access(city = 'cur', geometry = T)

df2 <- st_transform(df, crs = st_crs(accss))

# interpolate values
df3 <- sfheaders::sf_to_df(df2, fill = T)
head(df3)
values_interp <- with(subset(df3, !is.na(V2017)), interp(x , y, V2017, duplicate = 'strip')) %>%
              with(cbind(V2017=as.vector(z),  # Column-major order
             x=rep(x, times=length(y)),
             y=rep(y, each=length(x)))) %>%
  as.data.frame() %>% na.omit()

head(values_interp)

ggplot(values_interp) +
  geom_sf(data= accss, color='gray80') +
  geom_contour_filled(aes(x=x, y=y, z=travel_time), alpha=.8) 




ggplot() + 
  geom_sf(data= accss, color='gray80') +
  geom_sf(data= df2[c(1, 100, 1000, 3000, 4000, 10000),], color='red') +
  theme_void()



inter <- st_join(df2, accss)

inter <- subset(inter, P001 >0)

# valor e renda
stats::cor(inter$V2017, inter$R001, method = "pearson",  use = "complete.obs")

# valor e acesso empregos
  stats::cor(inter$V2017, inter$CMATT15, method = "pearson", use = "complete.obs")

  # valorizacao  e renda
  
  inter$valorizacao <- inter$V2017 - inter$V2002
  stats::cor(inter$valorizacao, inter$CMATT60, method = "pearson", use = "complete.obs")
  stats::cor(inter$valorizacao, inter$R001, method = "pearson", use = "complete.obs")
  