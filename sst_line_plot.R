if (!require("pacman")) install.packages("pacman")

pacman::p_load("here",       #file path
               "tidyverse",  #data cleaning etc
               "ggrepel",    #annotation
               "glue"        #dynamic plot title 
              )

path <- here::here()
setwd(path)

df_raw <- readr::read_csv(here::here("data", "timeseries_era5_daily_sst_60S-60N_data_clean.csv"))

df_clean <- df_raw %>% mutate(month     = month(date, label = TRUE, abbr = TRUE),
                              month     = factor(month),
                              year      = year(date),
                              day       = day(date),
                              sst       = as.numeric(sst),
                              this_year = year == max(year) #for creating a line of separate size 
                              ) %>% 
                       rename(sea_surface_temp = sst) %>% 
                       arrange(year)

last_data <- df_clean %>% filter(row_number() == n()) #for annotating in the plot

#Main plot
df_clean %>% 
  #main plot
  ggplot(aes(x = month, y = sea_surface_temp, group = year)) +
  geom_hline(yintercept = mean(df_clean$sea_surface_temp) , colour = "firebrick", linetype="dashed") +
  geom_line(aes(colour = year, size = this_year)) +
  geom_text_repel(data = last_data, color = "white", 
                  aes(label = year )) +
  scale_y_continuous( breaks = seq(18, 28, 0.2)) +
  scale_x_discrete( expand = c(0,0)) +
  #labels and style
  labs(
    x       = "Months",
    y       = "Daily mean sea surface temperature (°C)",
    title   = str_wrap(glue::glue("Daily sea surface temperature data from ERA5 averaged over the 60°S–60°N 
                                from {min(df_clean$year)} to {max(df_clean$year)}")) , #dynamically add start and end year to title
    caption = str_wrap("Source: ERA5 data from Climate Data Store (CDS) at
                             https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels"
                      ) 
      ) +
  scale_size_manual(breaks= c(FALSE, TRUE),
                    values = c(0.25, 1.25), 
                    guide = "none") +
  scale_colour_viridis_c(breaks = seq(min(df_clean$year), max(df_clean$year), 5),
                        guide = guide_colourbar(frame.colour = "white", frame.linewidth = 0.5), #to make legend border stand out 
  ) + 
#All theme elements
  theme(
    panel.background    = element_rect(fill = "black", colour = "white", linewidth =  1) ,
    plot.background     = element_rect(fill = "#33ddee"),
    panel.grid          = element_blank(),
    axis.text           = element_text(colour ="#001f3f"),
    axis.ticks          = element_line(colour ="white"),
    axis.ticks.length   = unit(-5, "pt") ,
    axis.title          = element_text(colour ="#001f3f"),
    plot.title          = element_text(colour ="#001f3f", hjust = 0.5, size = 12),
    legend.title        = element_blank(),
    legend.background   = element_rect(fill = NA),
    legend.text         = element_text(colour ="#001f3f"),
    legend.key.height   = unit(55, "pt")
  )

ggsave(here::here("graphs","sst_line_plot.png"),  dpi = "retina")
ggsave(here::here("graphs","sst_line_plot_tall.png"), width = 8, height = 8, dpi = "retina")

