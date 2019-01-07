#Building Data Visualization Tools
#week 1
#Welcome
#Basic plotting with ggplot2
#introduction
library(ggplot2)
library(faraway)
library(titanic)
#Use the following conventions to initialize a ggplot object:
iris <- iris  
nepali <- nepali

# install.packages("titanic") # If you don't have the package installed
data("titanic_train", package = "titanic")
titanic <- titanic_train


#initializing a ggplot object
  ## Generic code
  object <- ggplot(nepali, aes(x = nepali$ht, y = nepali$wt ))
  ## or, if you don't need to save the object
  ggplot(nepali, aes(x = nepali$ht, y = nepali$wt ))
  
  #plot aesthetics
  #creating a basic ggplot plot
  
  #histograma
  ggplot(data = titanic, aes(x = Fare)) + 
    geom_histogram()
  
  #forma 1
  ggplot(data = titanic) + 
    geom_histogram(aes(x = Fare))
  
  #forma 2
  ggplot() + 
    geom_histogram(data = titanic, aes(x = Fare))

  #forma 3
  titanic %>%
    ggplot() + 
    geom_histogram(aes(x = Fare))
  # or
  titanic %>%
    ggplot(aes(x = Fare)) + 
    geom_histogram()
    
  
  #Geoms
  ggplot(titanic, aes(x = Fare)) +
    geom_histogram(bins = 15)
  
  
  #
  ggplot(worldcup, aes(x = Time, y = Passes)) + 
    geom_point()
  
  #error
  ggplot(worldcup, aes(x = Time)) + 
    geom_point()

  #
  ggplot(worldcup, aes(x = Time, y = Passes,
                       color = Position, size = Shots)) + 
    geom_point()

  #use multiple geoms
  library(dplyr)
  
  noteworthy_players <- worldcup %>% filter(Shots == max(Shots) |
                                              Passes == max(Passes)) %>%
    mutate(point_label = paste(Team, Position, sep = "-"))
  
  ggplot(worldcup, aes(x = Passes, y = Shots)) +
    geom_point() +
    geom_text(data = noteworthy_players, aes(label = point_label),
              vjust = "inward", hjust = "inward", color = "red") +
    geom_point(data = noteworthy_players, color = "red")
  
    #
  ggplot(worldcup, aes(x = Time)) + 
    geom_histogram(binwidth = 10) + 
    geom_vline(xintercept = 90 * 0:6,
               color = "blue", alpha = 0.5)
  
  #Constan aesthetics
  ggplot(worldcup, aes(x = Time, y = Passes)) +
    geom_point(color = "darkgreen",shape=21,fill="black")
  
  #para shapes desde el 21 al 25, se puede especificar fill q es el relleno
  
  #example plots
  library(faraway)
  data(nepali)
  
  #
  nepali <- nepali %>%
    select(id, sex, wt, ht, age) %>%
    mutate(id = factor(id),
           sex = factor(sex, levels = c(1, 2),
                        labels = c("Male", "Female"))) %>%
    distinct(id, .keep_all = TRUE)
  
  #After this cleaning, the data looks like this:
    
    head(nepali)
  
    #histogramas
    ggplot(nepali, aes(x = ht)) + 
      geom_histogram()
    
    #
    ggplot(nepali, aes(x = ht)) + 
      geom_histogram(fill = "lightblue", color = "black") + 
      ggtitle("Height of children") + 
      xlab("Height (cm)") + xlim(c(0, 120))
    
    #scatterplots
    ggplot(nepali, aes(x = ht, y = wt)) + 
      geom_point()
    
    #
    ggplot(nepali, aes(x = ht, y = wt)) + 
      geom_point(color = "blue", size = 0.5) + 
      ggtitle("Weight versus Height") + 
      xlab("Height (cm)") + ylab("Weight (kg)")
    
  #
    ggplot(nepali, aes(x = ht, y = wt, color = sex)) + 
    geom_point(size = 0.5) + 
      ggtitle("Weight versus Height") + 
      xlab("Height (cm)") + ylab("Weight (kg)")
    
    #boxplots
    ggplot(nepali, aes(x = 1, y = ht)) + 
      geom_boxplot() + 
      xlab("")+ ylab("Height (cm)")
    
    #
    ggplot(nepali, aes(x = sex, y = ht)) + 
      geom_boxplot() + 
      xlab("Sex")+ ylab("Height (cm)") 
    
    #extensiones of ggplot 2
    library(GGally)
    ggpairs(nepali %>% select(sex, wt, ht, age))
    
    
    #customizing ggplot2 plots
    #introduction
    
    
    #guidelines for good plots
    library(dplyr)
    library(ggplot2)
    library(gridExtra)
    library(ggthemes)
    
    #
    library(faraway)
    data(nepali)
    data(worldcup)
    
    #
    library(dlnm)
    data(chicagoNMMAPS)
    chic <- chicagoNMMAPS
    chic_july <- chic %>%
      filter(month == 7 & year == 1995)
    
    #six guidelines for good graphics
    # Aim for high data density.
    ggplot(worldcup, aes(x = Time, y = Shots)) + 
      geom_point() + 
      theme_classic()
    
    ggplot(worldcup, aes(x = Time, y = Shots)) + 
      geom_point() + 
      theme_dark()
    
    #
    library(ggthemes)
    ggplot(worldcup, aes(x = Time, y = Shots)) + 
      geom_point() + 
      theme_tufte()
    
    #
    chicago_plot <- ggplot(chic_july, aes(x = date, y = death)) + 
      xlab("Day in July 1995") + 
      ylab("All-cause deaths") + 
      ylim(0, 450) 
    
    chicago_plot + 
      geom_area(fill = "black") + 
      theme_excel() 
    
    chicago_plot + 
      geom_line() + 
      theme_tufte() 
    
     # Use clear, meaningful labels.
    
    library(forcats)
    # Create a messier example version of the data
    wc_example_data <- worldcup %>%
      dplyr::rename(Pos = Position) %>%
      mutate(Pos = fct_recode(Pos,
                              "DC" = "Defender",
                              "FW" = "Forward", 
                              "GK" = "Goalkeeper",
                              "MF" = "Midfielder"))
    
    wc_example_data %>%
      ggplot(aes(x = Pos)) + 
      geom_bar() 
    
    wc_example_data %>%
      mutate(Pos = fct_recode(Pos,
                              "Defender" = "DC",
                              "Forward" = "FW", 
                              "Goalkeeper" = "GK",
                              "Midfielder" = "MF")) %>%
      ggplot(aes(x = Pos)) +
      geom_bar(fill = "lightgray") + 
      xlab("") + 
      ylab("Number of players") + 
      coord_flip() + 
      theme_tufte()
    
    
    # Provide useful references.
    ggplot(filter(worldcup, Position == "Goalkeeper"), aes(x = Passes, y = Shots)) + 
      geom_point(size = 1.5) + 
      theme_few()  + 
      geom_smooth()
    
    ggplot(worldcup, aes(x = Passes, y = Shots,color=Position)) + 
      geom_point(size = 1.5) + 
      theme_few()  + 
      geom_smooth()
    
    
    # Highlight interesting aspects of the data.
    noteworthy_players <- worldcup %>%
      filter(Shots == max(Shots) | Passes == max(Passes)) %>%
      mutate(point_label = paste0(Team, Position, sep = ", "))
    noteworthy_players
    
    #
    ggplot(worldcup, aes(x = Passes, y = Shots)) + 
      geom_point(alpha = 0.5) + 
      geom_text(data = noteworthy_players, aes(label = point_label),
                vjust = "inward", hjust = "inward", color = "blue") +
      theme_few()
    
    
    # Consider using small multiples.
    data(worldcup)
    worldcup %>%
      ggplot(aes(x = Time, y = Shots, color = Position)) + 
      geom_point() 
    
    #
    worldcup %>%
      ggplot(aes(x = Time, y = Shots)) + 
      geom_point() +
      facet_grid(. ~ Position) 
    
    #
    worldcup %>%
      filter(Team %in% c("Spain", "Netherlands")) %>%
      ggplot(aes(x = Time, y = Shots)) + 
      geom_point() +
      facet_grid(Team ~ Position) 
    
    #
    worldcup %>%
      ggplot(aes(x = Time, y = Shots)) + 
      geom_point(alpha = 0.25) +
      facet_wrap(~ Team, ncol = 8) 
    
    #
    nepali <- nepali %>%
    mutate(sex = factor(sex, levels = c(1, 2), 
                        labels = c("Male", "Female")))
    
    #
    ggplot(nepali, aes(ht, wt)) + 
      geom_point() + 
      facet_grid(. ~ sex)
    
    #
    nepali <- nepali %>%
      mutate(sex = factor(sex, levels = c("Female", "Male")))
    
    #
    ggplot(nepali, aes(ht, wt)) + 
      geom_point() + 
      facet_grid(. ~ sex)
    
    # Make order meaningful.
    
    ## Left plot
    worldcup %>%
      group_by(Team) %>%
      summarize(mean_time = mean(Time)) %>%
      ggplot(aes(x = mean_time, y = Team)) + 
      geom_point() + 
      theme_few() + 
      xlab("Mean time per player (minutes)") + ylab("") 
    
    ## Right plot
    worldcup %>%
      group_by(Team) %>%
      summarize(mean_time = mean(Time)) %>%
      arrange(mean_time) %>%                         # re-order and re-set
      mutate(Team = factor(Team, levels = Team)) %>% # factor levels before plotting
      ggplot(aes(x = mean_time, y = Team)) + 
      geom_point() + 
      theme_few() + 
      xlab("Mean time per player (minutes)") + ylab("") 
    
    #
    worldcup %>%
      select(Position, Time, Shots) %>%
      group_by(Position) %>%
      mutate(ave_shots = mean(Shots),
             most_shots = Shots == max(Shots)) %>%
      ungroup() %>%
      arrange(ave_shots) %>%
      mutate(Position = factor(Position, levels = unique(Position))) %>%
      ggplot(aes(x = Time, y = Shots, color = most_shots)) + 
      geom_point(alpha = 0.5) + 
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                         guide = FALSE) + 
      facet_grid(. ~ Position) + 
      theme_few()
    
    #
    worldcup %>%
      dplyr::select(Team, Time) %>%
      dplyr::group_by(Team) %>%
      dplyr::mutate(ave_time = mean(Time),
                    min_time = min(Time),
                    max_time = max(Time)) %>%
      dplyr::arrange(ave_time) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Team = factor(Team, levels = unique(Team))) %>%
      ggplot(aes(x = Time, y = Team)) + 
      geom_segment(aes(x = min_time, xend = max_time, yend = Team),
                   alpha = 0.5, color = "gray") + 
      geom_point(alpha = 0.5) + 
      geom_point(aes(x = ave_time), size = 2, color = "red", alpha = 0.5) + 
      theme_minimal() + 
      ylab("")
    
    #scales and color
    ## Generic code
    #scale_[aesthetic]_[vector type]
    
    ggplot(worldcup, aes(x = Time, y = Passes, color = Position, size = Shots)) + 
      geom_point(alpha = 0.5)
    
    #
    ggplot(worldcup, aes(x = Time, y = Passes, color = Position, size = Shots)) + 
      geom_point(alpha = 0.5) + 
      scale_x_continuous(name = "Time played (minutes)", 
                         breaks = 90 * c(2, 4, 6),
                         minor_breaks = 90 * c(1, 3, 5))
    
    #
    ggplot(worldcup, aes(x = Time, y = Passes, color = Position, size = Shots)) + 
      geom_point(alpha = 0.5) + 
      scale_x_continuous(name = "Time played (minutes)", 
                         breaks = 90 * c(2, 4, 6),
                         minor_breaks = 90 * c(1, 3, 5)) + 
      scale_size_continuous(name = "Shots on goal",
                            breaks = c(0, 10, 20))
    
    #
    ggplot(chic_july, aes(x = date, y = death)) + 
      geom_line() 
    
    
    #
    ggplot(chic_july, aes(x = date, y = death)) + 
      geom_line() + 
      scale_x_date(name = "Date in July 1995",
                   date_labels = "%m-%d")
    
    #
    ggplot(chic_july, aes(x = date, y = death)) + 
      geom_line() +
      scale_y_log10(breaks = c(1:4 * 100))
    
    #
    library(RColorBrewer)
    display.brewer.pal(name = "Set1", n = 8)
    display.brewer.pal(name = "PRGn", n = 8)
    display.brewer.pal(name = "PuBuGn", n = 8)
    
    #
    wc_example <- ggplot(worldcup, aes(x = Time, y = Passes,
                                       color = Position, size = Shots)) + 
      geom_point(alpha = 0.25) 
    
    a <- wc_example + 
      scale_color_brewer(palette = "Set1") + 
      ggtitle("Set1")
    b <- wc_example + 
      scale_color_brewer(palette = "Dark2") + 
      ggtitle("Dark2")
    c <- wc_example + 
      scale_color_brewer(palette = "Pastel2") + 
      ggtitle("Pastel2") + 
      theme_dark()
    d <- wc_example + 
      scale_color_brewer(palette = "Accent") + 
      ggtitle("Accent")
    grid.arrange(a, b, c, d, ncol = 2)
    
    #
    ggplot(worldcup, aes(x = Time, y = Passes,
                         color = Position, size = Shots)) + 
      geom_point(alpha = 0.5) + 
      scale_color_manual(values = c("blue", "red", 
                                    "darkgreen", "darkgray"))
    
   #
    library(viridis)
    
    worldcup %>%
      ggplot(aes(x = Time, y = Shots, color = Passes)) + 
      geom_point(size = 0.9) + 
      facet_wrap(~ Position) + 
      scale_color_viridis()
    
    #
    worldcup %>%
      ggplot(aes(x = Time, y = Shots, color = Position)) + 
      geom_point(alpha = 0.7) + 
      scale_color_viridis(discrete = TRUE)
    
    #
    library(gridExtra)
    
    worldcup_ex <- worldcup %>%
      ggplot(aes(x = Time, y = Shots, color = Passes)) + 
      geom_point(size = 0.9) 
    
    magma_plot <- worldcup_ex + 
      scale_color_viridis(option = "A") + 
      ggtitle("magma")
    inferno_plot <- worldcup_ex + 
      scale_color_viridis(option = "B") + 
      ggtitle("inferno")
    plasma_plot <- worldcup_ex + 
      scale_color_viridis(option = "C") + 
      ggtitle("plasma")
    viridis_plot <- worldcup_ex + 
      scale_color_viridis(option = "D") + 
      ggtitle("viridis")
    
    grid.arrange(magma_plot, inferno_plot, plasma_plot, viridis_plot, ncol = 2)
    
     #to find out more
    
    #ggplot2 basics
    #cuestionario
    
  #Week 2
    #Mapping
    # Introduction
    
    # Basics of Mapping
    library(ggplot2)
    library(magrittr)
    us_map <- map_data("state")
    head(us_map, 3)
    
    us_map %>% 
      dplyr::filter(region %in% c("north carolina", "south carolina")) %>%
      ggplot(aes(x = long, y = lat)) +
      geom_point()
    
    #If you try to join these points by just using a path geom rather than a points geom, however, you’ll have a problem:
      
      us_map %>% 
      dplyr::filter(region %in% c("north carolina", "south carolina")) %>%
      ggplot(aes(x = long, y = lat)) +
      geom_path()
    
      #
      us_map %>% 
        dplyr::filter(region %in% c("north carolina", "south carolina")) %>%
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_path()
      
      #
      us_map %>% 
        dplyr::filter(region %in% c("north carolina", "south carolina")) %>%
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "lightblue", color = "black")
      
      
      #
      us_map %>% 
        dplyr::filter(region %in% c("north carolina", "south carolina")) %>%
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "lightblue", color = "black") + 
        theme_void()
      
      #
      us_map %>% 
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "lightblue", color = "black") + 
        theme_void()
      
      #
      data(votes.repub)
      head(votes.repub)
      
      #
      library(dplyr)
      library(viridis)
      
      votes.repub %>%
        tbl_df() %>%
        mutate(state = rownames(votes.repub),
               state = tolower(state)) %>%
        right_join(us_map, by = c("state" = "region")) %>%
        ggplot(aes(x = long, y = lat, group = group, fill = `1976`)) +
        geom_polygon(color = "black") + 
        theme_void() + 
        scale_fill_viridis(name = "Republican\nvotes (%)")
      
      #
      library(readr)
      serial <- read_csv(paste0("https://raw.githubusercontent.com/",
                                "dgrtwo/serial-ggvis/master/input_data/",
                                "serial_podcast_data/serial_map_data.csv"))
      head(serial, 3)
      
      #
      serial <- serial %>%
        mutate(long = -76.8854 + 0.00017022 * x,
               lat  = 39.23822 + 1.371014e-04 * y,
               tower = Type == "cell-site")
      serial %>%
        slice(c(1:3, (n() - 3):(n())))
      
      #
      maryland <- map_data('county', region = 'maryland')
      head(maryland)
      
      #
      baltimore <- maryland %>%
        filter(subregion %in% c("baltimore city", "baltimore"))
      head(baltimore, 3)
      
      #
      ggplot(baltimore, aes(x = long, y = lat, group = group)) + 
        geom_polygon(fill = "lightblue", color = "black") + 
        theme_void()
    
      #
      ggplot(baltimore, aes(x = long, y = lat, group = group)) + 
        geom_polygon(fill = "lightblue", color = "black") + 
        geom_point(data = serial, aes(group = NULL, color = tower)) + 
        theme_void() + 
        scale_color_manual(name = "Cell tower", values = c("black", "red"))
      
        
    # ggmap, Google Maps API
      ## install.packages("ggmap")
      library(ggmap)
      beijing <- get_map("Beijing", zoom = 12)
      Caracas <- get_map("Caracas")
      
      ggmap(beijing)
      ggmap(Caracas)
      
      #
      ggmap(beijing) + 
        theme_void() + 
        ggtitle("Beijing, China")
      
      #
      map_1 <- get_map("Estes Park", zoom = 12,
                       source = "google", maptype = "terrain") %>%
        ggmap(extent = "device")
      
      map_2 <- get_map("Estes Park", zoom = 12,
                       source = "stamen", maptype = "watercolor") %>%
        ggmap(extent = "device")
      
      map_3 <- get_map("Estes Park", zoom = 12,
                       source = "google", maptype = "hybrid") %>%
        ggmap(extent = "device")
      
      library(gridExtra)
      grid.arrange(map_1, map_2, map_3, nrow = 1) 
      
      #
      get_map(c(2.35, 48.86), zoom = 10) %>%
        ggmap(extent = "device")
      
      #
      get_map("Baltimore County", zoom = 10, 
              source = "stamen", maptype = "toner") %>%
        ggmap() + 
        geom_polygon(data = baltimore, aes(x = long, y = lat, group = group),
                     color = "navy", fill = "lightblue", alpha = 0.2) + 
        geom_point(data = serial, aes(x = long, y = lat, color = tower)) + 
        theme_void() + 
        scale_color_manual(name = "Cell tower", values = c("black", "red"))
      
      #
      geocode("Supreme Court of the United States")
      geocode("Los Proceres")
      -66.85351 
      get_map(geocode("Centro Mohedano"), zoom = 15) %>%
        ggmap(extent = "device")
      
      #
      geocode("1 First St NE, Washington, DC")
      
      #
      mapdist("Baltimore, MD",
              "1 First St NE, Washington, DC") %>%
        select(from, to, miles)
      
    # Mapping US counties and states
      library(choroplethr)
      library(choroplethrMaps)
      
      data(df_pop_county)
      df_pop_county %>% slice(1:3)
      
      #
      county_choropleth(df_pop_county)
      
      #
      county_choropleth(df_pop_county, state_zoom = c("colorado", "wyoming"))
      
      #
      county_choropleth(df_pop_county, state_zoom = c("north carolina"),
                        reference_map = TRUE)
      
      #
      # library(readr)
      # floyd_events <- read_csv("data/floyd_events.csv") 
      # floyd_events %>% slice(1:3)
      
    # More advanced mapping– Spatial objects
      library(tigris)
      library(sp)
      denver_tracts <- tracts(state = "CO", county = 31, cb = TRUE)
    
      #  
      class(denver_tracts)
      
      #
      plot(denver_tracts)
      
      #
      bbox(denver_tracts)
      
      #
      is.projected(denver_tracts)
      
      #
      proj4string(denver_tracts)
      
      #
      head(denver_tracts@data)
      
      #
      roads <- primary_roads()
      
      plot(denver_tracts, col = "lightblue")
      plot(roads, add = TRUE, col = "darkred")
      
      #
      denver_tracts_df <- fortify(denver_tracts)
      denver_tracts_df %>%
        dplyr::select(1:4) %>% dplyr::slice(1:5)
      
      #
      denver_tracts_df %>%
        ggplot(aes(x = long, y = lat, group = group)) + 
        geom_polygon(fill = "lightblue", color = "black") + 
        theme_void()
      
      #
      proj4string(denver_tracts)
      
      
      ## Generic code
      proj4string(my_spatial_object) <- "+proj=longlat +datum=NAD83"
      
      #
      library(sp)
      CRS("+proj=longlat +datum=NAD27")
      
      CRS("+init=epsg:28992")
      
      ## Generic code
      my_spatial_object <- spTransform(my_spatial_object,
                                       CRS = CRS("+init=epsg:4267"))
      
      #
      usamap <- map_data("state") %>%
        ggplot(aes(long, lat, group = group)) +
        geom_polygon(fill = "white", colour = "black")
      
      map_1 <- usamap + coord_map() + ggtitle("default") 
      map_2 <- usamap + coord_map("gilbert") + ggtitle("+ coord_map('gilbert')")
      map_3 <- usamap + coord_map("conic", lat0 = 30) + 
        ggtitle("+ coord_map('conic', lat0 = 30)")
      
      grid.arrange(map_1, map_2, map_3, ncol = 1)
      
      #shapefiles
      
      #R como SIG (sist info geo)
      load("data/fars_colorado.RData")
      driver_data %>% 
        dplyr::select(1:5) %>% dplyr::slice(1:5)
      
      # Where to find more on mapping with R
    
    #htmlWidgets
    # Overview of htmlWidgets
      library(faraway) 
      data(worldcup)
      library(plotly)
      
      plot_ly(worldcup, type = "scatter",
              x = ~ Time, y = ~ Shots, color = ~ Position)
      
      #
      plot_ly(worldcup, type = "scatter",
              x = ~ Time, y = ~ Shots, color = I("blue"))
      
      #
      worldcup %>%
        mutate(Name = rownames(worldcup)) %>%
        plot_ly(x = ~ Time, y = ~ Shots, color = ~ Position) %>%
        add_markers(text = ~ Name, hoverinfo = "text")
      
      #
      worldcup %>%
        mutate(Name = rownames(worldcup)) %>%
        plot_ly(x = ~ Time, y = ~ Shots, color = ~ Position) %>%
        add_markers(text = ~ paste("<b>Name:</b> ", Name, "<br />", 
                                   "<b>Team:</b> ", Team),
                    hoverinfo = "text")
      
      #
      worldcup %>%
        plot_ly(x = ~ Time, y = ~ Shots, color = ~ Position) %>%
        add_markers()
      
      #
      worldcup %>%
        mutate(Name = rownames(worldcup)) %>%
        plot_ly(x = ~ Time, y = ~ Shots, z = ~ Passes,
                color = ~ Position, size = I(3)) %>%
        add_markers(text = ~ paste("<b>Name:</b> ", Name, "<br />", 
                                   "<b>Team:</b> ", Team),
                    hoverinfo = "text")
      
      #
      class(volcano)
      volcano[1:4, 1:4]
      View(volcano)
    
      plot_ly(z = ~ volcano, type = "surface")
      
      #OJO
      worldcup_scatter <- worldcup %>%
        ggplot(aes(x = Time, y = Shots, color = Position)) + 
        geom_point() 
      ggplotly(worldcup_scatter)
      
      
      # plotly package
   
    # Creating your own widget
      library(sigma)
      data <- system.file("examples/ediaspora.gexf.xml", package = "sigma")
      sigma(data)
      
      devtools::install_github('jjallaire/sigma')
      
      library(sigma)
      sigma(system.file("examples/ediaspora.gexf.xml", package = "sigma"))
    
      #
      #install.packages("htmlwidgets")
      devtools::create("mywidget")               # create package using devtools
      setwd("mywidget")                          # navigate to package dir
      htmlwidgets::scaffoldWidget("mywidget")    # create widget scaffolding
      devtools::install()                        # install the package so we can try it
      
      library(mywidget)
      mywidget("hello, world")
      #Mapping Quiz
    
      #1)
      library(ggplot2)
      us_map <- map_data("usa")
      head(us_map, 3)
      
      ggplot(us_map, aes(x = long , y = lat, group= group  ))+
        geom_path()
        
      #4) ruta casa blanca al capitolio
      library(ggmap)
      inauguration_route <- route(from = "US Capitol Building",
                                  to = "White House", 
                                  structure = "route",
                                  mode = "walking")
      inaug_route_map <- get_map("Metro Center, Washington DC",
                                 zoom = 14) %>% 
        ggmap(extent = "device") + 
        geom_path(data = inauguration_route,
                  color = "darkred", size = 1.1)
      
      #hoteles
      library(dplyr)
      dc_hotels <- data_frame(address = 
                                c("1401 Pennsylvania Ave NW, Washington DC", 
                                  "1331 Pennsylvania Ave NW, Washington DC")) %>%
        dplyr::bind_cols(geocode(dc_hotels$address))
      dc_hotels
      
      #
      inaug_route_map
      
      #
      inaug_route_map + 
        geom_point(data = dc_hotels, aes(x = lon, y = lat))
      
      #pregunta 10
      library(ggplot2)
      library(dplyr)
      USArrests %>%
        mutate(region = tolower(rownames(USArrests))) %>%
        left_join(map_data("state"), by = "region") %>%
        ggplot(aes(x = long, y = lat, group = group, color = Murder)) + 
        geom_polygon() + 
        theme_void()
      
    #Week 3
    
    #Introduction
    #Overview of grid graphics
      
    #Grobs
      library(grid)
      my_circle <- circleGrob(x = 0.5, y = 0.5, r = 0.5,
                              gp = gpar(col = "gray", lty = 3))
      
      grid.draw(my_circle)
      
      #
      my_circle <- circleGrob(name = "my_circle",
                              x = 0.5, y = 0.5, r = 0.5,
                              gp = gpar(col = "gray", lty = 3))
      grid.draw(my_circle)
      
      my_rect <- rectGrob(x = 0.5, y = 0.5, width = 0.8, height = 0.3)
      grid.draw(my_rect)
      
      #
      grid.edit("my_circle", gp = gpar(col = "red", lty = 1))
      
      #
      wc_plot <- ggplot(worldcup, aes(x = Time, y = Passes)) + 
        geom_point()
      grid.draw(wc_plot)
      
    #
      grid.draw(wc_plot)
      grid.draw(my_circle)
      
      #
      wc_plot
      grid.force()
      grid.ls()
      
      #
      grid.edit("geom_point.points.100", gp = gpar(col = "red"))
      grid.edit("GRID.text.130", gp = gpar(fontface = "bold"))
      
      #
      candy <- circleGrob(r = 0.1, x = 0.5, y = 0.6)
      stick <- segmentsGrob(x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 0.5)
      lollipop <- gTree(children = gList(candy, stick))
      grid.draw(lollipop)
      
      #
      grid.ls(lollipop)
      
      #Viewports
      grid.draw(rectGrob())
      sample_vp <- viewport(x = 0.5, y = 0.5, 
                            width = 0.5, height = 0.5,
                            just = c("left", "bottom"))
      pushViewport(sample_vp)
      grid.draw(roundrectGrob())
      grid.draw(lollipop)
      popViewport()
      
      #
      grid.draw(rectGrob())
      sample_vp <- viewport(x = 0.5, y = 0.5, 
                            width = 0.5, height = 0.5,
                            just = c("center", "center"))
      pushViewport(sample_vp)
      grid.draw(roundrectGrob())
      grid.draw(lollipop)
      popViewport()
      
      #
      grid.draw(rectGrob())
      sample_vp <- viewport(x = 0.75, y = 0.75, 
                            width = 0.25, height = 0.25,
                            just = c("left", "bottom"))
      pushViewport(sample_vp)
      grid.draw(roundrectGrob())
      grid.draw(lollipop)
      popViewport()
      
      #
      grid.draw(rectGrob())
      
      sample_vp_1 <- viewport(x = 0.75, y = 0.75, 
                              width = 0.25, height = 0.25,
                              just = c("left", "bottom"))
      pushViewport(sample_vp_1)
      grid.draw(roundrectGrob())
      grid.draw(lollipop)
      popViewport()
      
      sample_vp_2 <- viewport(x = 0, y = 0, 
                              width = 0.5, height = 0.5,
                              just = c("left", "bottom"))
      pushViewport(sample_vp_2)
      grid.draw(roundrectGrob())
      grid.draw(lollipop)
      popViewport()
      
      #
      grid.draw(rectGrob())
      
      sample_vp_1 <- viewport(x = 0.5, y = 0.5, 
                              width = 0.5, height = 0.5,
                              just = c("left", "bottom"))
      sample_vp_2 <- viewport(x = 0.1, y = 0.1, 
                              width = 0.4, height = 0.4,
                              just = c("left", "bottom"))
      
      pushViewport(sample_vp_1)
      grid.draw(roundrectGrob(gp = gpar(col = "red")))
      pushViewport(sample_vp_2)
      grid.draw(roundrectGrob())
      grid.draw(lollipop)
      popViewport(2)
      
      #
      grid.draw(rectGrob())
      
      sample_vp_1 <- viewport(x = 0.5, y = 0.5, 
                              width = 0.5, height = 0.5,
                              just = c("left", "bottom"))
      pushViewport(sample_vp_1)
      grid.draw(roundrectGrob())
      grid.draw(lollipop)
      popViewport()
    
      #
      grid.ls()
      
      #
      worldcup %>%
        ggplot(aes(x = Time, y = Passes)) + 
        geom_point()
      grid.force()
      
      #
      grid.ls()
      
      #
      balt_counties <- map_data("county", region = "maryland") %>%
        mutate(our_counties = subregion %in% c("baltimore", "baltimore city"))
      balt_map <- get_map("Baltimore County", zoom = 10) %>%
        ggmap(extent = "device") + 
        geom_polygon(data = filter(balt_counties, our_counties == TRUE),
                     aes(x = long, y = lat, group = group),
                     fill = "red", color = "darkred", alpha = 0.2)
      
      #
      maryland_map <- balt_counties %>%
        ggplot(aes(x = long, y = lat, group = group, fill = our_counties)) + 
        geom_polygon(color = "black") + 
        scale_fill_manual(values = c("white", "darkred"), guide = FALSE) + 
        theme_void() + 
        coord_map()
      
      grid.draw(ggplotGrob(balt_map))
      md_inset <- viewport(x = 0, y = 0, 
                           just = c("left", "bottom"),
                           width = 0.35, height = 0.35)
      pushViewport(md_inset)
      grid.draw(rectGrob(gp = gpar(alpha = 0.5, col = "white")))
      grid.draw(rectGrob(gp = gpar(fill = NA, col = "black")))
      grid.draw(ggplotGrob(maryland_map))
      popViewport()
      
      #Grid graphics coordinate systems
      ex_vp <- viewport(x = 0.5, y = 0.5, 
                        just = c("center", "center"),
                        height = 0.8, width = 0.8,
                        xscale = c(0, 100), yscale = c(0, 10))
      pushViewport(ex_vp)
      grid.draw(rectGrob())
      grid.draw(circleGrob())
      grid.draw(circleGrob(x = unit(20, "native"), y = unit(5, "native"),
                           r = 0.1, gp = gpar(fill = "lightblue")))
      grid.draw(circleGrob(x = unit(85, "native"), y = unit(8, "native"),
                           r = 0.1, gp = gpar(fill = "darkred")))
      popViewport()
      
      
    #the gridExtra package
      library(gridExtra)
      grid.arrange(lollipop, circleGrob(),
                   rectGrob(), lollipop, 
                   ncol = 2)
      
      #
      time_vs_shots <- ggplot(worldcup, aes(x = Time, y = Shots)) + 
        geom_point()
      player_positions <- ggplot(worldcup, aes(x = Position)) + 
        geom_bar()
      
      grid.arrange(time_vs_shots, player_positions, ncol = 2)
      
      #
      grid.arrange(time_vs_shots, player_positions,
                   layout_matrix = matrix(c(1, 2, 2), ncol = 3))
      
      #
      grid.arrange(time_vs_shots, player_positions,
                   layout_matrix = matrix(c(1, NA, NA, NA, 2, 2), 
                                          byrow = TRUE, ncol = 3))
      #
      worldcup_table <- worldcup %>%
        filter(Team %in% c("Germany", "Spain", "Netherlands", "Uruguay")) %>%
        group_by(Team) %>%
        dplyr::summarize(`Average time` = round(mean(Time), 1),
                         `Average shots` = round(mean(Shots), 1)) %>%
        tableGrob()
      
      grid.draw(ggplotGrob(time_vs_shots))
      wc_table_vp <- viewport(x = 0.22, y = 0.85, 
                              just = c("left", "top"),
                              height = 0.1, width = 0.2)
      pushViewport(wc_table_vp)
      grid.draw(worldcup_table)
      popViewport()
      
    #where to find more about grid graphics
    
    
    #Week 4
      
      #Building a new theme
      #Introduction
      library(ggplot2)
      ggplot(data = mtcars, aes(x = disp, y = mpg)) + 
        geom_point() + 
        theme_classic()
      
      #Why build a new theme
      #default theme
      x <- theme_get()
      class(x)
      
      #
      new_theme <- theme_minimal()
      theme_set(new_theme)
      
      #
      ggplot(data = mtcars, aes(disp, mpg)) + 
        geom_point() + 
        facet_grid( . ~ gear)
      
      #para establecer un tema por defecto al cargar el paquete ggplot2
      setHook(packageEvent("ggplot2", "onLoad"), 
              function(...) ggplot2::theme_set(ggplot2::theme_minimal()))
      
      #building a new theme
      
      newtheme <- theme_bw() + theme(plot.title = element_text(color = "darkred"))
      
      newtheme$panel.border
      
      newtheme <- newtheme + 
        theme(panel.border = element_rect(color = "steelblue", size = 2))
      
      #
      library(faraway)
      ggplot(data = worldcup, aes(Time, Shots)) + 
        geom_point() + 
        ggtitle("World Cup Data") + 
        newtheme
      
      #
      ggplot(data = worldcup, aes(Time, Shots)) + 
        geom_point() + 
        facet_wrap(facets = ~ Position, ncol = 2) + 
        ggtitle("World Cup Data") + 
        newtheme
      
      #summary
      
      #Build new graphical elements
      #Introduction
      
      #Building a Geom
      
      #estructura basica
      # GeomNEW <- ggproto("GeomNEW", Geom,
      #                    required_aes = <a character vector of required aesthetics>,
      #                    default_aes = aes(<default values for certain aesthetics>),
      #                    draw_key = <a function used to draw the key in the legend>,
      #                    draw_panel = function(data, panel_scales, coord) {
      #                      ## Function that returns a grid grob that will 
      #                      ## be plotted (this is where the real work occurs)
      #                    }
      # )
      
      library(grid)
      GeomMyPoint <- ggproto("GeomMyPoint", Geom,
                             required_aes = c("x", "y"),
                             default_aes = aes(shape = 1),
                             draw_key = draw_key_point,
                             draw_panel = function(data, panel_scales, coord) {
                               ## Transform the data first
                               coords <- coord$transform(data, panel_scales)
                               
                               ## Let's print out the structure of the 'coords' object
                               str(coords)
                               
                               ## Construct a grid grob
                               pointsGrob(
                                 x = coords$x,
                                 y = coords$y,
                                 pch = coords$shape
                               )
                             })
      
      #
      geom_mypoint <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", na.rm = FALSE, 
                               show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
          geom = GeomMyPoint, mapping = mapping,  
          data = data, stat = stat, position = position, 
          show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
        )
      }
      
      #
      ggplot(data = worldcup, aes(Time, Shots)) + geom_mypoint()
      
      #Example: An automatic transparency geom
      GeomAutoTransparent <- ggproto("GeomAutoTransparent", Geom,
                                     required_aes = c("x", "y"),
                                     default_aes = aes(shape = 19),
                                     draw_key = draw_key_point,
                                     draw_panel = function(data, panel_scales, coord) {
                                       ## Transform the data first
                                       coords <- coord$transform(data, panel_scales)
                                       
                                       ## Compute the alpha transparency factor based on the
                                       ## number of data points being plotted
                                       n <- nrow(data)
                                       if(n > 100 && n <= 200)
                                         coords$alpha <- 0.3
                                       else if(n > 200)
                                         coords$alpha <- 0.15
                                       else
                                         coords$alpha <- 1
                                       ## Construct a grid grob
                                       grid::pointsGrob(
                                         x = coords$x,
                                         y = coords$y,
                                         pch = coords$shape,
                                         gp = grid::gpar(alpha = coords$alpha)
                                       )
                                     })
      
      #
      geom_transparent <- function(mapping = NULL, data = NULL, stat = "identity",
                                   position = "identity", na.rm = FALSE, 
                                   show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
          geom = GeomAutoTransparent, mapping = mapping,  
          data = data, stat = stat, position = position, 
          show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
        )
      }
      
      #
      ggplot(data = worldcup, aes(Time, Shots)) + geom_transparent()
      
      #
      library(dplyr)
      ggplot(data = sample_n(worldcup, 150), aes(Time, Shots)) +
        geom_transparent()
      
      #
      ggplot(data = sample_n(worldcup, 50), aes(Time, Shots)) + 
        geom_transparent()
      
      #
      ggplot(data = worldcup, aes(Time, Shots)) + 
        geom_transparent() + 
        facet_wrap(~ Position, ncol = 2) + 
        newtheme
      
      #building a stat
      #estructura basica
      StatNEW <- ggproto("StatNEW", Stat,
                         compute_group = <a function that does computations>,
                         default_aes = aes(<default values for certain aesthetics>),
                         required_aes = <a character vector of required aesthetics>)
      
    
      #example: normal confidence intervals
      library(datasets)
      library(dplyr)
      data("airquality")
      monthly <- dplyr::group_by(airquality, Month) %>%
        dplyr::summarize(ozone = mean(Ozone, na.rm = TRUE),
                         stderr = sd(Ozone, na.rm = TRUE) / sqrt(sum(!is.na(Ozone))))
      monthly
      
      #
      ggplot(monthly, aes(x = Month, y = ozone)) + 
        geom_point() + 
        ylab("Ozone (ppb)")
      
      #
      StatConfint <- ggproto("StatConfint", Stat,
                             compute_group = function(data, scales) {
                               ## Compute the line segment endpoints
                               x <- data$x
                               xend <- data$x
                               y <- data$y - 1.96 * data$stderr
                               yend <- data$y + 1.96 * data$stderr
                               
                               ## Return a new data frame
                               data.frame(x = x, xend = xend,
                                          y = y, yend = yend)
                             },
                             required_aes = c("x", "y", "stderr")
      )
      
      #
      stat_confint <- function(mapping = NULL, data = NULL, geom = "segment",
                               position = "identity", na.rm = FALSE, 
                               show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
          stat = StatConfInt, 
          data = data, 
          mapping = mapping, 
          geom = geom, 
          position = position, 
          show.legend = show.legend, 
          inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
        )
      }
      
      #
      ggplot(data = monthly, aes(x = Month, y = ozone, stderr = stderr)) + 
        geom_point() + 
        ylab("Ozone (ppb)") + 
        geom_segment(stat = "confint")
      
      #combining geoms and stats
      StatSkinnybox <- ggproto("StatSkinnybox", Stat, 
                               compute_group = function(data, scales) {
                                 probs <- c(0, 0.25, 0.5, 0.75, 1)
                                 qq <- quantile(data$y, probs, na.rm = TRUE) 
                                 out <- qq %>% as.list %>% data.frame
                                 names(out) <- c("ymin", "lower", "middle", 
                                                 "upper", "ymax")
                                 out$x <- data$x[1]
                                 out
                               },
                               required_aes = c("x", "y")
      )
      
      #
      stat_skinnybox <- function(mapping = NULL, data = NULL, geom = "skinnybox",
                                 position = "identity", show.legend = NA, 
                                 outliers = TRUE, inherit.aes = TRUE, ...) {
        ggplot2::layer(
          stat = StatSkinnybox, 
          data = data, 
          mapping = mapping, 
          geom = geom, 
          position = position, 
          show.legend = show.legend, 
          inherit.aes = inherit.aes,
          params = list(outliers = outliers, ...)
        )        
      }
      
      #
      library(scales)
      draw_panel_function <- function(data, panel_scales, coord) {
        coords <- coord$transform(data, panel_scales) %>%
          mutate(lower = rescale(lower, from = panel_scales$y.range),
                 upper = rescale(upper, from = panel_scales$y.range),
                 middle = rescale(middle, from = panel_scales$y.range))
        med <- pointsGrob(x = coords$x,
                          y = coords$middle,
                          pch = coords$shape)
        lower <- segmentsGrob(x0 = coords$x,
                              x1 = coords$x,
                              y0 = coords$ymin,
                              y1 = coords$lower,
                              gp = gpar(lwd = coords$size))
        upper <- segmentsGrob(x0 = coords$x,
                              x1 = coords$x,
                              y0 = coords$upper,
                              y1 = coords$ymax,
                              gp = gpar(lwd = coords$size))
        gTree(children = gList(med, lower, upper))
      }
      
      GeomSkinnybox <- ggproto("GeomSkinnybox", Geom,
                               required_aes = c("x", "ymin", "lower", "middle", 
                                                "upper", "ymax"),
                               default_aes = aes(shape = 19, lwd = 2),
                               draw_key = draw_key_point,
                               draw_panel = draw_panel_function
      )
      
      #
      geom_skinnybox <- function(mapping = NULL, data = NULL, stat = "skinnybox", 
                                 position = "identity", show.legend = NA, 
                                 na.rm = FALSE, inherit.aes = TRUE, ...) {
        layer(
          data = data, 
          mapping = mapping,
          stat = stat,
          geom = GeomSkinnybox,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
        )
      }
      
      #
      mutate(airquality, Month = factor(Month)) %>%
        ggplot(aes(Month, Ozone)) + 
        geom_skinnybox()
      
      #summary
      
      #Build a new Geom
    
    
    
    
    