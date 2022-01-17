#------------------------------------------------------------------------------
# Type: script
# Name: get_sentinel.R
# Author: Chris Reudenbach, creuden@gmail.com
# Description:  retrieves sentinel data
#               and exemplary defines AOI and calculates albedo
# Dependencies:
# Output: original sentinel tile
#         AOI window of this tile (research_area)
#         top of atmosphere albedo AOI
#         surface albedo AOI
# Copyright: GPL (>= 3)
#------------------------------------------------------------------------------

# laden der notwendigen Bibliotheken
## Achtung sie müssen evtl. installiert werden
library(envimaR)
library(rprojroot)
## setzen des aktuellen Projektverzeichnisses (erstellt mit envimaR) als rootDIR
rootDIR = "~/edu/courses_src/geoinfo/"

# einlesen des zuvor erstellten setup Srkiptes
source(file.path(rootDIR, "src/functions/000_setup.R"))
library(sen2r)

old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = paste(old_path, "/home/creu/apps/Sen2Cor-02.09.00-Linux64/bin/", sep = ":"))
check_sen2r_deps()

# Zoomen sie auf den Harz und digitalisieren Sie ein Viereck
# Beenden Sie den Vorgang mit Done
#harz = mapedit::editMap()
#st_crs(harz) = st_crs(harz)
# ausschreiben der Datei
#sf::st_write(harz ,paste0(envrmt$path_data_lev0,"/harz.geojson"))

safe_is_online("/home/creu/.sen2r/lta_orders/lta_20211213_163011.json")
json_path <- paste0(envrmt$path_data_lev0,"/harz2.json")


out_dir_3 <- tempfile(pattern = "sen2r_out_3_")  # new output folder

myextent_3 <- system.file("extdata/vector/scalve.kml", package = "sen2r")

out_paths_3 <- sen2r(
  param_list = json_path_2, 
  extent = myextent_3, 
  extent_name = "newxtent",
  timewindow = c(as.Date("2020-10-01"), as.Date("2020-10-30")),
  path_out = out_dir_3
)



out_paths_2 <- sen2r(param_list = json_path)
# einlesen des Pfades zur Datei in die Varible myextent
myextent = paste0(envrmt$path_data_lev0,"/harz.geojson")




# wir erstellen Dateinamen (einfach so hinnehmen ;-)
fn_noext=xfun::sans_ext(basename(list.files(paste0(envrmt$path_research_area,"/BOA/"),pattern = "S2B2A")))
fn = basename(list.files(paste0(envrmt$path_research_area,"/BOA/"),pattern = "S2B2A"))

# jetzt lesen wir die Daten ein
stack=raster::stack(paste0(envrmt$path_research_area,"/BOA/",fn))

plot(stack)

# berechnen der Pberflächen Albedo einer angepassten Regression aus dem Paket 'agriwater'
# Einlesen der notwendigen Knäle aus dem Sentinel Stack  in einzelne raster ebenen
# /10000 ist notwendig um die Originalwerte korrekt zu skalieren
b2 <- stack[[2]]/10000
b3 <- stack[[3]]/10000
b4 <- stack[[4]]/10000
b8 <- stack[[8]]/10000

# lineare regression albedo top of Atmosphere
alb_top = b2 * 0.32 + b3 * 0.26 + b4 * 0.25 + b8 * 0.17
# lineare regression oberflächenalbedo
alb_surface = 0.6054 * alb_top + 0.0797

# Visualierung
plot(alb_top)
plot(alb_surface)
