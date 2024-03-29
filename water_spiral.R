if (!require("pacman"))  install.packages("pacman")

pacman::p_load("here",       #file path
               "tidyverse",  #data cleaning etc
               "janitor",    #clean column names 
               "gganimate"   #animation
               )

#Here will point to the top-level directory where the .Rproj file is saved. 
#Changing the working directory to that folder
path <- here::here()
setwd(path)

aqua_raw <- readr::read_csv(here("data","AQUASTAT.csv"))

aqua_raw <- aqua_raw %>% janitor::clean_names()

aqua_raw %>%  filter(year <= 2020)

aqua_grouped <- aqua_raw %>% 
                 mutate(across(c(country, year),
                                 ~as.factor(.)
                               )
                        ) %>%
                group_by(country) %>% 
                arrange(year)
                  


#Initial plan was to select the 10 most populous countries. However, the data on Russia was sparse
#Hence, it was replaced by the 11th most populous country - Ethiopia
countries <- c("India",  "China", "United States of America" , "Indonesia", "Pakistan", 
               "Nigeria" ,"Brazil", "Bangladesh",  "Mexico", "Ethiopia" )

aqua_filtered_var <- aqua_grouped %>% 
                            filter(variable == "Total renewable water resources per capita", 
                            country  %in% countries )

aqua_filtered_var$step_number <- seq(1:nrow(aqua_filtered_var))                     

aqua_filtered_var  %>% #basic graph
                   ggplot(aes(year, value, group = country, colour = country)) +
                   geom_line() +
               
                   labs( title   = stringr::str_wrap("Total renewable water resources per capita over the years for select countries.") ,
                        caption = str_wrap("Total renewable water is defined as Total annual renewable water resources per inhabitant.
                                    Source:  FAO. [2024]. AQUASTAT Core Database. Food and Agriculture Organization of the United Nations."
                                           )
                        ) +
                   coord_polar() +  #turns it into a spiral
    #all theme elements
    theme(
    panel.background    = element_rect(fill = "black", colour = NA, linewidth =  NA) ,
    plot.background     = element_rect(fill = "black"),
    panel.grid          = element_blank(),
    axis.text.x         = element_text(colour ="yellow", angle = 48, hjust = 1),
    axis.text.y         = element_blank(),
    axis.ticks          = element_blank(),
    axis.ticks.length   = unit(-5, "pt") ,
    axis.title          = element_text(colour ="#001f3f"),
    plot.title          = element_text(colour ="white", hjust = 0.5, size = 12),
    plot.title.position = "plot",
    plot.caption        = element_text(hjust = 0.5, face = "italic", colour = "white"),
    legend.title        = element_blank(),
    legend.background   = element_rect(fill = NA, colour  = NULL),
    legend.text         = element_text(colour ="ivory"),
    legend.key.height   = unit(15, "pt"),
    legend.key = element_rect(colour = NA, fill = NA)
  ) +
  transition_reveal(along = step_number)


anim_save("water_spiral.gif", path = "graphs", height = 15, width = 15, units = "in", res = 300)


