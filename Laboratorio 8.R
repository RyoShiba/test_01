
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
#library(gapminder)
library(ggplot2)
library(hrbrthemes)
library(viridisLite)
library(gganimate)
library(RColorBrewer)
#display.brewer.all()

################################################################################
#coast_vs_waste %>% drop_na() %>% print()
coast_vs_waste1 <- 
  coast_vs_waste %>%
  as_tibble() %>% 
  filter(Year == 2010) %>% 
  drop_na() %>%
  arrange(desc(`Total population (Gapminder)`)) %>% 
  print(n = 10, width = Inf)

mismanaged_vs_gdp1 <- 
  mismanaged_vs_gdp %>%
  as_tibble() %>% 
  filter(Year == 2010) %>% 
  drop_na() %>%
  arrange(desc(`Total population (Gapminder)`)) %>% 
  print(n = 10, width = Inf)

waste_vs_gdp1 <- 
  waste_vs_gdp %>%
  as_tibble() %>% 
  filter(Year == 2010) %>% 
  drop_na() %>%
  arrange(desc(`Total population (Gapminder)`)) %>% 
  print(n = 10, width = Inf)
################################################################################  

coast_vs_waste1 <-
  coast_vs_waste1 %>% 
  mutate(Poblac = (`Coastal population`*100)/`Total population (Gapminder)`) %>% 
  mutate(Costa1 = `Coastal population`/1000000) %>% 
  print()

#No saleeeeeeeee
coast_vs_waste1 <-
  coast_vs_waste1 %>% 
  mutate(Group = case_when(Poblac %% Poblaci < 21 ~ "Q1"
                           Poblac %% Poblaci > 20 & Poblac < 4 ~ "Q2",
                           Poblac %% Poblaci > 40 & Poblac < 61 ~ "Q3",
                           Poblac %% Poblaci > 60 & Poblac < 81 ~ "Q4",
                           Poblac %% Poblaci > 80 ~ "Q5",
                           TRUE ~ Poblac)) %>%
  print(n = 10)

  ################################################################################
ggplot(coast_vs_waste1, aes(x=`Poblac`, y= Costa1, size =`Total population (Gapminder)`, color = Poblac)) +
  geom_point(alpha=0.6) +
  theme_ipsum() +
  theme(legend.position="right") +
  ylab("Mismanaged plastic waste (mill tonnes)") +
  xlab("% Coastal Pop") +
  title("Relación entre mal manejo de basura y % poblacion que vive en costa") +
  scale_y_continuous(breaks = seq(25,250, 50)) 
  
################################################################################
library(ggExtra)
x <- ggplot(coast_vs_waste1, aes(x=Poblac, y=Costa1, color='Total population (Gapminder)', size = Costa1)) +
  geom_point() +
  theme(legend.position="botom")
ggMarginal(x, type="histogram", fill = "SpringGreen", yparams = list(bins=10), xparams = list(bins=10), groupColour = TRUE, size = 2) 
 











#purrr::set_names(coast_vs_waste)

h1{
  color: #104E8B;
    font-family: 'Avenir';
}

hr{
  color: #B22222;
    font-family: 'Avenir';
}

body{
  font-family: 'Avenir';
  font-size: 14pt;
  color: #36648B;
    line-heigh: 2em;
}

################################################################################
#Operaciones

coast_vs_waste1 <-
  coast_vs_waste1 %>% 
  as.data.frame(coast_vs_waste1) %>% 
  mutate(Poblac = (`Coastal population`*100)/`Total population (Gapminder)`) %>% 
  print(n = 10)



  
#################################################################################
waste_vs_gdp1 <-
  waste_vs_gdp1 %>% 
  drop_na() %>% 
  print()

ggplot(data = pm10_s9) + 
  geom_point(mapping = aes(x = fecha, y = pm10))    

  ggplot(data = waste_vs_gdp1) + 
    geom_point(mapping =  aes(Year, `Total population (Gapminder)`)) 

    
        facet_grid(rows = vars(Year)) +  
    theme_bw() +
    theme(legend.position = "bottom") 
    print()
  

#################################################################################


ggplot(mismanaged_vs_gdp1, aes(x=`GDP per capita, PPP (constant 2011 international $) (Rate)`, y=`Per capita mismanaged plastic waste (kilograms per person per day)`, size = `Total population (Gapminder)`)) +
  geom_point(alpha=0.7) +
  theme_ipsum(grid_col=#B8860B) +
  theme(legend.position="bottom") +
  ylab("Per capita mismanaged plastic waste (kg/day)") +
  xlab("GDP per capita") +
  theme(legend.position = "none")

##############################################################################

waste_vs_gdp1 <- 
  waste_vs_gdp [,-c(4)] %>%
  as.Date(waste_vs_gdp,Year) %>% 
  filter(Entity == "Mexico") %>%
    arrange(Year) %>% 
    print()

library(plotly)
ggplot(waste_vs_gdp1, aes(Year, 'Total population (Gapminder)')) +
  geom_point() +
#  scale_x_log10() +
  theme_bw() +
  labs(title = 'Year: {frame_time}', x = 'Year', y = 'Total population (Gapminder)') +
  transition_time(Year) +
  ease_aes('linear')



#  scale_size(range = c(.1, 24), name="Population (M)")

#datos_wide <-
#  coast_vs_waste [ ,-c(4, 5)] %>%
#    pivot_wider(names_from = Year,
#              names_prefix = "Y_",
#              values_from = `Total population (Gapminder)`) %>%
#  filter(Code == "JPN" | Code =="MEX" | Code == "ARG" | Code == "BRA" | 
#         Code == "USA" | Code == "COL" | Code == "ECU" | Code == "VEN") %>% 
#  as_tibble() %>% 
#  print()

datos1 <-
  coast_vs_waste [ ,-c(4, 5)] %>%
  filter(Code == "JPN" | Code =="MEX" | Code == "ARG" | Code == "BRA" | 
       Code == "USA" | Code == "COL" | Code == "ECU" | Code == "VEN") %>%
  
  print()

#No sale
datos1 %>% 
ggplot(aes(Year, 'Total_population_(Gapminder)', color = Code)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  labs(title = 'Year: {frame_time}', x = 'Año', y = 'Población Total') +
  transition_time(Year) +
  ease_aes('linear')

#No sale
datos1 %>%
  ggplot( aes(x=Year, y='Total_population_(Gapminder)', group=Code, color=Code)) +
  geom_line() +
  geom_point() +
  #scale_color_viridis(discrete = TRUE) +
  ggtitle("Tamaño de población a través del tiempo") +
  theme_ipsum() +
  ylab("Población Total") +
  transition_reveal(Year)

#Area
  ggplot(coast_vs_waste, aes(x=Year, y='Total_population_(Gapminder)', fill=Code)) + 
  geom_area()

# Plot
datos_wide %>%
  ggplot( aes(x=Year, y='Total_population_(Gapminder)', group=Code, color=Code)) +
  scale_x_log10() +
  geom_line()






################################################################################
  
















ggplot(malla1, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')
