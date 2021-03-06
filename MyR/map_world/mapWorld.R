install.packages("rworldmap")
install.packages("googleVis")
library(rworldmap)
library(googleVis)
library(maptools)
#
library(sf)
library(leaflet)
library(mapview)
library(tmap)
#

theCountries <- c("DEU", "COD", "BFA")
# ��� �������� ����� � ������� ISO3, ������� �� ������ �������� �������.

malDF <- data.frame(country = c("DEU", "COD", "BFA"),
                    malaria = c(1, 1, 1))
# malDF - ��� data.frame � ���������� ����� ISO3 � ���������� ���
# ������� � ������� �����

malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
# ��� ����������� ��� malDF data.frame � ������ ����� ������.

mapCountryData(malMap, nameColumnToPlot="malaria", catMethod = "categorical",
               missingCountryCol = gray(.8))

# � ��� �������� ���, � �������, ��� �������� ������� �������
# ���� �������
## �������� ���������� �������� �����, � �������-���� � ��������� ������
malDF <- data.frame(country = c("DEU", "COD", "BFA"),
                    malaria = c(1, 1, 2))


## ��������� �������
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")

## ������� �������� colourPalette
mapCountryData(malMap, nameColumnToPlot="���", catMethod = "categorical",
               missingCountryCol = gray(.8), colourPalette = c("red", "blue"))

G1 <- gvisGeoMap(Exports,locationvar='Country',numvar='Profit',options=list(dataMode='regions'))

plot(G1)

data(wrld_simpl)
myCountries = wrld_simpl@data$NAME %in% c("Germany", "United Kingdom", "Sweden", "Netherlands", "New Zealand")
plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])

#########################################################
# Gett the World sf data
data("World")

# Turn on the view mode in tmap
tmap_mode("view")

# Plot World using tmap
tm_basemap("Esri.WorldStreetMap") +
  tm_shape(World) +
  tm_polygons(col = "continent")

# Plot world using mapview
mapview(World, map.types = "Esri.WorldStreetMap")

# Gett the World sf data
data("World")

# Turn on the view mode in tmap
tmap_mode("plot")

# Plot World using tmap
tm_basemap("Esri.WorldStreetMap") +
  tm_shape(World) +
  tm_polygons() +
  tm_text(text = "iso_a3")
 