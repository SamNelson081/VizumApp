
devtools::install_github(repo = "lydialucchesi/Vizumap", force = TRUE)
library(Vizumap)
library(dplyr)
library(ggplot2)

load("/Users/luc093/Desktop/phd/papers/VizumapJoss/GBRexample/UB_Joss.rda")
data(UB)

pixUB <- pixelate(UB_shp, id = "region")

df <-
  data.frame(region = sapply(slot(UB_shp, "polygons"), function(x)
    slot(x, "ID")),
    name = unique(UB_shp@data$scID))

amc95$region <- df[match(amc95$scID, df$name), 1]
amc95$region <- as.character(amc95$region)

amc95$region %in% pixUB$region

amc95_q <- amc95[, 3:15]

dat <-
  read.uv(data = amc95,
          estimate = "TSS",
          error = "TSS_error")

map <-
  build_pmap(
    data = dat,
    distribution = "discrete",
    pixelGeo = pixUB,
    id = "region",
    palette = "Oranges",
    q = amc95_q,
    border = UB_shp
  )

map <- view(map) + coord_fixed()

