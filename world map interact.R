library(magrittr)
library(rvest)
url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCode"]') %>%
  html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
head(iso_codes)
library(readxl)
url <- "https://www.un.org/en/development/desa/population/publications/dataset/fertility/wfr2012/Data/Data_Sources/TABLE%20A.8.%20%20Percentage%20of%20childless%20women%20and%20women%20with%20parity%20three%20or%20higher.xlsx"
destfile <- "dataset_childlessness.xlsx"
download.file(url, destfile)
childlessness_data <- read_excel(destfile)
cols <- which(grepl("childless", childlessness_data[2, ]))
childlessness_data <- childlessness_data[-c(1:3), c(1, 3, cols:(cols + 2))]
names(childlessness_data) <- c("Country", "Period", "35-39", "40-44", "45-49")
head(childlessness_data)
gender_index_data <- read.csv("https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/743/data.csv")
head(gender_index_data)
library(dplyr)
gender_index_data["RecentYear"] <- apply(gender_index_data, 1, function(x){as.numeric(x[max(which(!is.na(x)))])})
gender_index_data <- gender_index_data[gender_index_data$Subindicator.Type == "Rank", ] %>% 
  select(-Subindicator.Type, -Indicator.Id)
names(gender_index_data) <- c("ISO3", "Country", "Indicator", as.character(c(2006:2016, 2018)), "RecentYear")
head(gender_index_data)
library(maps)
library(ggplot2)
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)
childlessness_data['ISO3'] <- iso_codes$ISO3[match(childlessness_data$Country, iso_codes$Country)]
world_data["ISO3"] <- iso_codes$ISO3[match(world_data$region, iso_codes$Country)]
library(reshape2)
childlessness_melt <- melt(childlessness_data, id = c("Country", "ISO3", "Period"), 
                           variable.name = "Indicator", value.name = "Value")
childlessness_melt$Value <- as.numeric(childlessness_melt$Value)
gender_index_melt <- melt(gender_index_data, id = c("ISO3", "Country", "Indicator"), 
                          variable.name = "Period", value.name = "Value")
childlessness_melt["DataType"] <- rep("Childlessness", nrow(childlessness_melt))
gender_index_melt["DataType"] <- rep("Gender Gap Index", nrow(gender_index_melt))
df <- rbind(childlessness_melt, gender_index_melt)
worldMaps <- function(df, world_data, data_type, period, indicator){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 14),
                       axis.title = element_text(size = 14),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotdf <- df[df$Indicator == indicator & df$DataType == data_type & df$Period == period,]
  plotdf <- plotdf[!is.na(plotdf$ISO3), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['DataType'] <- rep(data_type, nrow(world_data))
  world_data['Period'] <- rep(period, nrow(world_data))
  world_data['Indicator'] <- rep(indicator, nrow(world_data))
  world_data['Value'] <- plotdf$Value[match(world_data$ISO3, plotdf$ISO3)]
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("Source: ", ifelse(data_type == "Childlessness", "United Nations" , "World Bank"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    labs(fill = data_type, color = data_type, title = NULL, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
}
library(leaflet)
