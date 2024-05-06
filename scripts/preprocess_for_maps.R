# preprocess_for_maps.R
#### simplify maps ------

# global distribution ------
library(rnaturalearth)
library(sf)
library(countrycode)

map <- ne_countries(
  scale="medium",
  returnclass='sf'
) %>% filter(admin !="Antarctica")

# Eliminate small islands etc.

map<-dplyr::filter(map,admin!='Wallis and Futuna')
map<-dplyr::filter(map,admin!='Vatican')
map<-dplyr::filter(map,admin!='British Indian Ocean Territory') 
map<-dplyr::filter(map,admin!='Aruba')
map<-dplyr::filter(map,admin!='American Samoa')
map<-dplyr::filter(map,admin!='Antigua and Barbuda')
map<-dplyr::filter(map,admin!='Ashmore and Cartier Islands')
map<-dplyr::filter(map,admin!='British Virgin Islands') 
map<-dplyr::filter(map,admin!='Bermuda')
map<-dplyr::filter(map,admin!='Benin')
map<-dplyr::filter(map,admin!='Pitcairn Islands')
map<-dplyr::filter(map,admin!='Norfolk Island')                                                
map<-dplyr::filter(map,admin!='Montserrat')
map<-dplyr::filter(map,admin!='Fiji')                                                                             
map<-dplyr::filter(map,admin!='Cook Islands') 
map<-dplyr::filter(map,admin!='Comoros')
map<-dplyr::filter(map,admin!='Cayman Islands')
map<-dplyr::filter(map,admin!='Faroe Islands')
map<-dplyr::filter(map,admin!='Equatorial Guinea')
map<-dplyr::filter(map,admin!='Falkland Islands') 
map<-dplyr::filter(map,admin!='Federated States of Micronesia') 
map<-dplyr::filter(map,admin!='French Polynesia') 
map<-dplyr::filter(map,admin!='French Southern and Antarctic Lands') 
map<-dplyr::filter(map,admin!='Indian Ocean Territories') 
map<-dplyr::filter(map,admin!='Jersey')
map<-dplyr::filter(map,admin!='Greenland')
map<-dplyr::filter(map,admin!='Grenada')
map<-dplyr::filter(map,admin!='Heard Island and McDonald Islands')
map<-dplyr::filter(map,admin!='Isle of Man') 
map<-dplyr::filter(map,admin!='Kiribati')
map<-dplyr::filter(map,admin!='Maldives')
map<-dplyr::filter(map,admin!='Marshall Islands') 
map<-dplyr::filter(map,admin!='Mauritius')
map<-dplyr::filter(map,admin!='New Caledonia') 
map<-dplyr::filter(map,admin!='Northern Mariana Islands') 
map<-dplyr::filter(map,admin!='Saint Barthelemy')
map<-dplyr::filter(map,admin!='Saint Helena')
map<-dplyr::filter(map,admin!='Saint Kitts and Nevis')
map<-dplyr::filter(map,admin!='Saint Lucia')
map<-dplyr::filter(map,admin!='Saint Martin')
map<-dplyr::filter(map,admin!='Saint Pierre and Miquelon')
map<-dplyr::filter(map,admin!='Saint Vincent and the Grenadines')
map<-dplyr::filter(map,admin!='Vanuatu')
map<-dplyr::filter(map,admin!='Tonga')
map<-dplyr::filter(map,admin!='Tonga')
map<-dplyr::filter(map,admin!='Samoa')
map<-dplyr::filter(map,admin!='Siachen Glacier')
map<-dplyr::filter(map,admin!='Seychelles') 
map<-dplyr::filter(map,admin!='Sint Maarten') 
map<-dplyr::filter(map,admin!='Solomon Islands') 
map<-dplyr::filter(map,admin!='South Georgia and South Sandwich Islands') 
map<-dplyr::filter(map,admin!='Turks and Caicos Islands') 
map<-dplyr::filter(map,admin!='United States Virgin Islands')
map<-dplyr::filter(map,admin!='Nauru')
map<-dplyr::filter(map,admin!='Niue')

# Convert into country codes
map$COWcode <- countrycode(map$adm0_a3, "iso3c", "ecb",warn = FALSE) 

# Some locations not labelled....
ind <- is.na(map$COWcode)
#map$formal_en[ind]

