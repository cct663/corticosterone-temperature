## Script for Taff, Wingfield, Vitousek 2025
## Written by Conor Taff (cct663@gmail.com)
## Last updated 1/28/2025

## Load packages & settings ----
    pacman::p_load(nasapower, tidyverse, lubridate, lutz, rnaturalearth, climwin, mgcv, sjPlot, RColorBrewer, ggpubr)

## Load data ----
    ## Load the cort data in wide format
        d_cort <- read.delim(here::here("raw_data", "raw_data.txt"))
        
## Wrangle data and add columns ----                
    ## Add timezone to the dataframe
        d_cort$timezone <- tz_lookup_coords(d_cort$latitude, d_cort$longitude, method = "accurate")
        
    ## Add offset to UTC
        d_cort$utc_offset <- sapply(1:nrow(d_cort), function(i) {
          tz_offset(d_cort$timezone[i], dt = as.POSIXct(d_cort$date[i], format = "%m/%d/%y"))$utc_offset_h
        })
        
    ## Make a date time column
        # note which rows are missing time data
          d_cort$time_missing <- "no"
          d_cort$time_missing[is.na(d_cort$capture_hour)] <- "yes"
        
        # assigning a default time for missing values to be able to work with the dates
            d_cort$capture_hour[is.na(d_cort$capture_hour)] <- 10    
            d_cort$capture_minute[is.na(d_cort$capture_minute)] <- 0
            
            d_cort$date <- mdy(d_cort$date, format = "%mm/%dd/%yy")[1:1727]
            
            d_cort$date_time_local <- d_cort$date + hours(d_cort$capture_hour) + minutes(d_cort$capture_minute)
            
            d_cort$date_time_utc <- d_cort$date_time - hours(d_cort$utc_offset)
            
## Get temperature data for each location ----
    # first collapse into unique sampling locations and years to make downloads easier
      locs <- d_cort %>%
        group_by(latitude, longitude, year, altitude_m) %>%
              summarise(#start_yr = min(year), end_yr = max(year),
                        start_dt = min(date), end_dt = max(date),
                        n_samples = n()) %>%
              as.data.frame()
            
      locs$site <- paste0(letters[((1:nrow(locs) - 1) %% 26) + 1], 
                          ceiling((1:nrow(locs)) / 26))
            
    # for each unique location/year, go through and download the temperature data
        
        # loop through each location            
            for (i in 1:nrow(locs)){
              # function to get data from nasapower
                nd <- get_power(
                  community = "AG",
                  pars = c("T2M_MAX", "T2M_MIN", "T2M", "PRECTOTCORR"),
                  lonlat = c(locs$longitude[i], locs$latitude[i]),
                  dates = c(locs$start_dt[i] - 28, locs$end_dt[i]),
                  temporal_api = "DAILY",
                  time_standard = "LST"  # use local solar time, this should match up to local date
                )
                
        
                
              # elevation is stored in the header, get it out here
                header <- capture.output(print(nd))
                nd$NASA_ELEV <- as.numeric(sub(".*= ([0-9.]+) meters.*", "\\1", 
                                        grep("elevation from MERRA-2", header, value = TRUE, ignore.case = TRUE)))
                
              # add the site location indicator and site altitude
                nd$site <- locs$site[i]
                nd$site_elevation <- locs$altitude_m[i]
              
              # save the downloaded temperature for this loop    
                if(i == 1){
                  dl_temperature <- as.data.frame(nd)
                }
                if(i > 1){
                  dl_temperature <- rbind(dl_temperature, as.data.frame(nd))
                }
                
              # print the status
                print(paste(i, "of", nrow(locs), sep = " "))
            }  
      
    # Now downloaded expected/average temperature for each location/date to create anomaly
        locs2 <- d_cort %>%
          group_by(latitude, longitude, altitude_m, date, doy, year, day, month) %>%
          summarise(
            n_samples = n()) %>%
          as.data.frame()
        
        locs2$site2 <- paste0(letters[((1:nrow(locs2) - 1) %% 26) + 1], 
                            ceiling((1:nrow(locs2)) / 26))

    # loop through each location for expected temperature        
                for (i in 1:nrow(locs2)){
                  # make a list of dates to download
                      start_yr <- 1981
                      end_yr <- 2010
                      dates_list <- data.frame(date = as.Date(paste(seq(start_yr, end_yr, 1), rep(substr(locs2$date[i], 6, 10), 30), sep = "-")))
                    
                    # download the 30 years daily data by location
                        nd <- get_power(
                          community = "AG",
                          pars = c("T2M_MAX", "T2M_MIN", "T2M"),
                          lonlat = c(locs2$longitude[i], locs2$latitude[i]),
                          dates = c(dates_list$date[1], dates_list$date[30]),
                          temporal_api = "DAILY",
                          time_standard = "LST"
                        )
                        
                    # subset to only the day needed
                        nd2 <- nd %>%
                          filter(nd$DOY > locs2$doy[i] - 7, nd$DOY < locs2$doy[i] + 7)
                    
                    # calculate averages
                        locs2$avg_T2M_MAX[i] <- mean(na.omit(nd2$T2M_MAX))
                        locs2$sd_T2M_MAX[i] <- sd(na.omit(nd2$T2M_MAX))
                        locs2$avg_T2M_MIN[i] <- mean(na.omit(nd2$T2M_MIN))
                        locs2$sd_T2M_MIN[i] <- sd(na.omit(nd2$T2M_MIN))
                        locs2$avg_T2M[i] <- mean(na.omit(nd2$T2M))
                        locs2$sd_T2M[i] <- sd(na.omit(nd2$T2M))
                      
                  # elevation is stored in the header, get it out here
                    header <- capture.output(print(nd2))
                    locs2$NASA_ELEV[i] <- as.numeric(sub(".*= ([0-9.]+) meters.*", "\\1", 
                                            grep("elevation from MERRA-2", header, value = TRUE, ignore.case = TRUE)))
                    
                  print(paste(i, " of ", nrow(locs2)))
                    
                }          
    
## Save the temperature data ----
    # The prior two blocks shouldn't need to be run unless something has been changed
            write.table(dl_temperature, here::here("saved_objects", "dl_temperature.txt"), sep = "\t", row.names = FALSE)
            dl_temperature <- read.delim(here::here("saved_objects", "dl_temperature.txt"))
            
            write.table(locs2, here::here("saved_objects", "expected_temps.txt"), sep = "\t", row.names = FALSE)
            locs2 <- read.delim(here::here("saved_objects", "expected_temps.txt"), sep = "\t")

## Correct for elevation ----
    # The NASA temperature is for an average elevation across the grid. 
      # I want to correct for elevation where the sampling actually occurred using a standard lapse rate
      
      # lapse rate
        # in practice this shouldn't matter much since sampling at each site/species is mostly at one elevation
            lapse_rate <- 6.5 / 1000 #6.5 C per 1000 meters elevation
      
      # Correct each temperature in observed dataset
            dl_temperature$T2M_MAX_correct <- dl_temperature$T2M_MAX + 
                (dl_temperature$NASA_ELEV - dl_temperature$site_elevation) * lapse_rate
            
            dl_temperature$T2M_MIN_correct <- dl_temperature$T2M_MIN + 
              (dl_temperature$NASA_ELEV - dl_temperature$site_elevation) * lapse_rate
            
            dl_temperature$T2M_correct <- dl_temperature$T2M + 
              (dl_temperature$NASA_ELEV - dl_temperature$site_elevation) * lapse_rate
            
      # correct each temperature in expected dataset
            locs2$avg_T2M_MAX_correct <- locs2$avg_T2M_MAX +
              (locs2$NASA_ELEV - locs2$altitude_m) * lapse_rate
            
            locs2$avg_T2M_MIN_correct <- locs2$avg_T2M_MIN +
              (locs2$NASA_ELEV - locs2$altitude_m) * lapse_rate
            
            locs2$avg_T2M_correct <- locs2$avg_T2M +
              (locs2$NASA_ELEV - locs2$altitude_m) * lapse_rate

## More data wrangling ----
        # change date format
            d_cort$date2 <- format(as.Date(d_cort$date), format = "%d/%m/%Y")
            dl_temperature$date2 <- format(as.Date(dl_temperature$YYYYMMDD), format = "%d/%m/%Y")
            
        # get site id into cort data by joining to locations
            d_cort$joiner <- paste(d_cort$latitude, d_cort$longitude, d_cort$year, d_cort$altitude_m, sep = "_")
            locs$joiner <- paste(locs$latitude, locs$longitude, locs$year, locs$altitude_m, sep = "_")
            
            d_cort2 <- plyr::join(d_cort, locs[, c("latitude", "longitude",
                                                   "start_dt", "end_dt",
                                                   "n_samples", "site", "joiner")], "joiner", "left", "first")
           
        # clean up date columns and join to temperature
            dl_temperature$DATE <- as.Date(dl_temperature$YYYYMMDD)
            dl_temperature$doy <- yday(dl_temperature$DATE)
            dl_temperature$joiner2 <- paste(dl_temperature$LAT, dl_temperature$LON,
                                            dl_temperature$YEAR, dl_temperature$site_elevation,
                                            dl_temperature$doy, sep = "_")
            d_cort2$joiner2 <- paste(d_cort2$joiner, d_cort2$doy, sep = "_")
            d_cort3 <- plyr::join(d_cort2, dl_temperature, "joiner2", "left", "first")
            
            idx = which(duplicated(names(d_cort3)))
            d_cort3 <- d_cort3[, -idx]   # remove any duplicated columns from join
        
        # add count of samples per species
            for(i in 1:nrow(d_cort3)){
              d_cort3$spcs_count[i] <- nrow(subset(d_cort3, d_cort3$alpha4 == d_cort3$alpha4[i]))
            }
            
        # a few records seem to have major mistakes that I can't reconcile. removing them here
            d_cort3[c(1463, 1388), ] <- NA
            
        # determine speed measure for each individual
            for(i in 1:nrow(d_cort3)){
              spd <- c(
                ifelse(!is.na(d_cort3$b1_cort[i]) & d_cort3$b1_cort[i] != "" &
                         !is.na(d_cort3$b2_cort[i]) & d_cort3$b2_cort[i] != "" &
                         !is.na(d_cort3$time_b1_min[i]) & d_cort3$time_b1_min[i] != "" &
                         !is.na(d_cort3$time_b2_min[i]) & d_cort3$time_b2_min[i] != "",
                       (d_cort3$b2_cort[i] - d_cort3$b1_cort[i]) / (d_cort3$time_b2_min[i] - d_cort3$time_b1_min[i]), NA),
                ifelse(!is.na(d_cort3$b2_cort[i]) & d_cort3$b2_cort[i] != "" &
                         !is.na(d_cort3$b3_cort[i]) & d_cort3$b3_cort[i] != "" &
                         !is.na(d_cort3$time_b2_min[i]) & d_cort3$time_b2_min[i] != "" &
                         !is.na(d_cort3$time_b3_min[i]) & d_cort3$time_b3_min[i] != "",
                       (d_cort3$b3_cort[i] - d_cort3$b2_cort[i]) / (d_cort3$time_b3_min[i] - d_cort3$time_b2_min[i]), NA),
                ifelse(!is.na(d_cort3$b3_cort[i]) & d_cort3$b3_cort[i] != "" &
                         !is.na(d_cort3$b4_cort[i]) & d_cort3$b4_cort[i] != "" &
                         !is.na(d_cort3$time_b3_min[i]) & d_cort3$time_b3_min[i] != "" &
                         !is.na(d_cort3$time_b4_min[i]) & d_cort3$time_b4_min[i] != "",
                       (d_cort3$b4_cort[i] - d_cort3$b3_cort[i]) / (d_cort3$time_b4_min[i] - d_cort3$time_b3_min[i]), NA),
                ifelse(!is.na(d_cort3$b4_cort[i]) & d_cort3$b4_cort[i] != "" &
                         !is.na(d_cort3$b5_cort[i]) & d_cort3$b5_cort[i] != "" &
                         !is.na(d_cort3$time_b4_min[i]) & d_cort3$time_b4_min[i] != "" &
                         !is.na(d_cort3$time_b5_min[i]) & d_cort3$time_b5_min[i] != "",
                       (as.numeric(d_cort3$b5_cort[i]) - d_cort3$b4_cort[i]) / (d_cort3$time_b5_min[i] - d_cort3$time_b4_min[i]), NA),
                ifelse(!is.na(d_cort3$b5_cort[i]) & d_cort3$b5_cort[i] != "" &
                         !is.na(d_cort3$b6_cort[i]) & d_cort3$b6_cort[i] != "" &
                         !is.na(d_cort3$time_b5_min[i]) & d_cort3$time_b5_min[i] != "" &
                         !is.na(d_cort3$time_b6_min[i]) & d_cort3$time_b6_min[i] != "",
                       (d_cort3$b6_cort[i] - as.numeric(d_cort3$b5_cort[i])) / (d_cort3$time_b6_min[i] - d_cort3$time_b5_min[i]), NA),
                ifelse(!is.na(d_cort3$b6_cort[i]) & d_cort3$b6_cort[i] != "" &
                         !is.na(d_cort3$b7_cort[i]) & d_cort3$b7_cort[i] != "" &
                         !is.na(d_cort3$time_b6_min[i]) & d_cort3$time_b6_min[i] != "" &
                         !is.na(d_cort3$time_b7_min[i]) & d_cort3$time_b7_min[i] != "",
                       (d_cort3$b7_cort[i] - d_cort3$b6_cort[i]) / (d_cort3$time_b7_min[i] - d_cort3$time_b6_min[i]), NA)
              )
              
              d_cort3$speed[i] <- max(na.omit(spd))
            }
            
        # Join expected temperature to capture data
            expect <- locs2[, c("latitude", "longitude", "avg_T2M_MAX_correct", "avg_T2M_MIN_correct", "avg_T2M_correct")]
            d_cort3 <- plyr::join(d_cort3, expect, c("latitude", "longitude"), "left", "first")
            
            d_cort3$max_anomaly <- d_cort3$T2M_MAX - d_cort3$avg_T2M_MAX_correct
            d_cort3$min_anomaly <- d_cort3$T2M_MIN - d_cort3$avg_T2M_MIN_correct
            d_cort3$avg_anomaly <- d_cort3$T2M - d_cort3$avg_T2M_correct
      
            
## Descriptive/methods plots ----
    # Plot the elevation of the grid average vs. the elevation of the actual sites
          plot_elevation <- dl_temperature %>%
              filter(!duplicated(site)) %>%
              ggplot(dl_temperature, mapping = aes(x = site_elevation, y = NASA_ELEV)) +
              geom_point(color = "slateblue", alpha = 0.7) +
              geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
              theme_classic() +
              theme(panel.grid = element_blank(),
                    axis.text = element_text(size = 12),
                    axis.title = element_text(size = 14)) +
              xlab("Site elevation (m)") +
              ylab("Average grid elevation (m)") +
              coord_cartesian(expand = FALSE, xlim = c(0, 5140), ylim = c(0, 5140)) 
            
    # Plot the location of sampling around the world with point size scaled by number of birds
            world_map <- ne_countries(scale = "medium", returnclass = "sf")
            
            plot_map <- ggplot(data = world_map) +
              geom_sf() +
              geom_point(data = locs, aes(x = longitude, y = latitude, 
                                          size = n_samples), fill = "coral3", 
                         shape = 21, alpha = 0.8) +
              scale_size_continuous(range = c(0.5, 4)) +
              theme_bw() +
              theme(panel.grid = element_blank(), axis.text = element_text(size = 12),
                    axis.title = element_text(size = 14)) +
              xlab("Longitude") + ylab("Latitude") +
              labs(size = "Samples")
            
            ggsave(here::here("figs_and_tabs", "map_fig.png"), plot_map, device = "png", 
                   dpi = 300, width = 6.5, height = 3, units = "in")
            
    # plot range of temperature measures by species and latitude
          temp_plot <- d_cort3 %>%
            filter(is.na(alpha4) == FALSE, T2M_correct > -18) %>%
            group_by(alpha4) %>%
            summarise(n = n(), lat = abs(mean(latitude, na.rm = TRUE)),
                      av_t = mean(T2M_correct, na.rm = TRUE),
                      sd_t = sd(T2M_correct, na.rm = TRUE),
                      min_t = min(T2M_correct, na.rm = TRUE),
                      max_t = max(T2M_correct, na.rm = TRUE)) %>%
            filter(n > 9) %>%
            ggplot(mapping = aes(x = av_t, y = lat)) +
            geom_segment(mapping = aes(y = lat, yend = lat, x = min_t, xend = max_t), size = 0.5, color = "gray50") +
            geom_segment(mapping = aes(y = lat, yend = lat, x = av_t - sd_t, xend = av_t + sd_t), size = 1) +
            geom_point(fill = "lightblue", shape = 21, size = 2) +
            theme_bw() +
            theme(panel.grid = element_blank(), axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12)) +
            xlab("Temperature (°C)") +
            ylab("Absolute Latitude") +
            annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5, size = 6)
          
          ggsave(here::here("figs_and_tabs", "temp_plot.png"), temp_plot, 
                 device = "png", dpi = 300, width = 4.3, height = 5.2, units = "in")
          
      # plot range of temperature anomaly measures by species and latitude
          temp_plot2 <- d_cort3 %>%
            filter(is.na(alpha4) == FALSE, avg_anomaly > -18) %>%
            group_by(alpha4) %>%
            summarise(n = n(), lat = abs(mean(latitude, na.rm = TRUE)),
                      av_t = mean(avg_anomaly, na.rm = TRUE),
                      sd_t = sd(avg_anomaly, na.rm = TRUE),
                      min_t = min(avg_anomaly, na.rm = TRUE),
                      max_t = max(avg_anomaly, na.rm = TRUE)) %>%
            filter(n > 9) %>%
            ggplot(mapping = aes(x = av_t, y = lat)) +
            geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
            geom_segment(mapping = aes(y = lat, yend = lat, x = min_t, xend = max_t), size = 0.5, color = "gray50") +
            geom_segment(mapping = aes(y = lat, yend = lat, x = av_t - sd_t, xend = av_t + sd_t), size = 1) +
            geom_point(fill = "lightblue", shape = 21, size = 2) +
            theme_bw() +
            theme(panel.grid = element_blank(), axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12)) +
            xlab("Temperature Anomaly (°C)") +
            ylab("Absolute Latitude") +
            annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5, size = 6)
          
          ggsave(here::here("figs_and_tabs", "temp_plot2.png"), temp_plot2, 
                 device = "png", dpi = 300, width = 4.3, height = 5.2, units = "in")
            
## Fit GAMMS for all species combined using avg/max/min temperature ----
    # make sure that species is a factor  
        d_cort3$alpha4 <- as.factor(d_cort3$alpha4)  
          
    # make some scaled versions of measures...not currently using these
          d_cort3$avg_anomaly_s <- scale(d_cort3$avg_anomaly)
          d_cort3$max_anomaly_s <- scale(d_cort3$max_anomaly)
          d_cort3$min_anomaly_s <- scale(d_cort3$min_anomaly)
          d_cort3$T2M_MIN_correct_s <- scale(d_cort3$T2M_MIN_correct)
          d_cort3$T2M_MAX_correct_s <- scale(d_cort3$T2M_MAX_correct)
          d_cort3$T2M_correct_s <- scale(d_cort3$T2M)
          
    # models using all species with combo of base/max/speed cort and avg/min/max temperature
      # note, there are a small number of very low temperatures that could be errors in location or 
      # are so far off that better to exclude
         
        # base cort   
            # average temperature
              moddat_mba <- d_cort3[d_cort3$T2M_correct > -5 & is.na(d_cort3$base_cort) == FALSE, ]
              moddat_mba$response <- log(moddat_mba$base_cort)
              m_base_avg <- mgcv::gam(log(base_cort) ~ s(T2M_correct) + s(alpha4, bs = "re") + 
                                s(T2M_correct, alpha4, bs = "re"),
                              data = moddat_mba)  
             
            # max temperature 
              moddat_mbh <- d_cort3[d_cort3$T2M_MAX_correct > -5 & is.na(d_cort3$base_cort) == FALSE, ]
              moddat_mbh$response <- log(moddat_mbh$base_cort)
              m_base_max <- mgcv::gam(log(base_cort) ~ s(T2M_MAX_correct) + s(alpha4, bs = "re") + 
                                        s(T2M_MAX_correct, alpha4, bs = "re"), 
                                      data = moddat_mbh) 
            
            # min temperature  
              moddat_mbl <- d_cort3[d_cort3$T2M_MIN_correct > -5 & is.na(d_cort3$base_cort) == FALSE, ]
              moddat_mbl$response <- log(moddat_mbl$base_cort)
              m_base_min <- mgcv::gam(log(base_cort) ~ s(T2M_MIN_correct) + s(alpha4, bs = "re") + 
                                        s(T2M_MIN_correct, alpha4, bs = "re", sp = .3), 
                                      data = moddat_mbl) 
          
        # max cort   
            # average temperature
              moddat_mma <- d_cort3[d_cort3$T2M_correct > -5 & is.na(d_cort3$max_cort) == FALSE, ]
              moddat_mma$response <- log(moddat_mma$max_cort)
              m_max_avg <- mgcv::gam(log(max_cort) ~ s(T2M_correct) + s(alpha4, bs = "re") + 
                                        s(T2M_correct, alpha4, bs = "re"), 
                                      data = moddat_mma)  
            
            # max temperature 
              moddat_mmh <- d_cort3[d_cort3$T2M_correct > -5 & is.na(d_cort3$max_cort) == FALSE, ]
              moddat_mmh$response <- log(moddat_mmh$max_cort)
              m_max_max <- mgcv::gam(log(max_cort) ~ s(T2M_MAX_correct) + s(alpha4, bs = "re") + 
                                        s(T2M_MAX_correct, alpha4, bs = "re"), 
                                      data = moddat_mmh) 
            
            # min temperature  
              moddat_mml <- d_cort3[d_cort3$T2M_correct > -5 & is.na(d_cort3$max_cort) == FALSE, ]
              moddat_mml$response <- log(moddat_mml$max_cort)
              m_max_min <- mgcv::gam(log(max_cort) ~ s(T2M_MIN_correct) + s(alpha4, bs = "re") + 
                                        s(T2M_MIN_correct, alpha4, bs = "re"), 
                                      data = moddat_mml) 

        # speed cort   
            # average temperature
              moddat_msa <- d_cort3[d_cort3$T2M_correct > -5 & d_cort3$speed > 0, ]
              moddat_msa$response <- log(moddat_msa$speed)
              m_spd_avg <- mgcv::gam(log(speed) ~ s(T2M_correct) + s(alpha4, bs = "re") + 
                                       s(T2M_correct, alpha4, bs = "re"), 
                                     data = moddat_msa)  
            
            # max temperature  
              moddat_msh <- d_cort3[d_cort3$T2M_correct > -5 & d_cort3$speed > 0, ]
              moddat_msh$response <- log(moddat_msh$speed)
              m_spd_max <- mgcv::gam(log(speed) ~ s(T2M_MAX_correct) + s(alpha4, bs = "re") + 
                                       s(T2M_MAX_correct, alpha4, bs = "re"), 
                                     data = moddat_msh) 
            
            # min temperature  
              moddat_msl <- d_cort3[d_cort3$T2M_correct > -5 & d_cort3$speed > 0, ]
              moddat_msl$response <- log(moddat_msl$speed)
              m_spd_min <- mgcv::gam(log(speed) ~ s(T2M_MIN_correct) + s(alpha4, bs = "re") + 
                                       s(T2M_MIN_correct, alpha4, bs = "re"), 
                                     data = moddat_msl) 
 
## Fit the same GAMMS but using temperature anomaly ----                       
    # repeat the same models but using temperature anomaly
        # base cort   
            # average temperature anomaly
              moddat_mbaa <- d_cort3[d_cort3$avg_anomaly > -20 & is.na(d_cort3$base_cort) == FALSE, ]
              moddat_mbaa$response <- log(moddat_mbaa$base_cort)
              m_base_avg_a <- mgcv::gam(log(base_cort) ~ s(avg_anomaly) + s(alpha4, bs = "re") + 
                                s(avg_anomaly, alpha4, bs = "re"),
                              data = moddat_mbaa)  
             
            # max temperature 
              moddat_mbha <- d_cort3[d_cort3$max_anomaly > -20 & is.na(d_cort3$base_cort) == FALSE, ]
              moddat_mbha$response <- log(moddat_mbha$base_cort)
              m_base_max_a <- mgcv::gam(log(base_cort) ~ s(max_anomaly) + s(alpha4, bs = "re") + 
                                        s(max_anomaly, alpha4, bs = "re"), 
                                      data = moddat_mbha) 
            
            # min temperature  
              moddat_mbla <- d_cort3[d_cort3$min_anomaly > -20 & is.na(d_cort3$base_cort) == FALSE, ]
              moddat_mbla$response <- log(moddat_mbla$base_cort)
              m_base_min_a <- mgcv::gam(log(base_cort) ~ s(min_anomaly, sp = 0.2) + s(alpha4, bs = "re") + 
                                        s(min_anomaly, alpha4, bs = "re", sp = .3), 
                                      data = moddat_mbla) 
                  
        # max cort   
            # average temperature
              moddat_mmaa <- d_cort3[d_cort3$avg_anomaly > -20 & is.na(d_cort3$max_cort) == FALSE, ]
              moddat_mmaa$response <- log(moddat_mmaa$max_cort)
              m_max_avg_a <- mgcv::gam(log(max_cort) ~ s(avg_anomaly, sp = 0.1) + s(alpha4, bs = "re") + 
                                        s(avg_anomaly, alpha4, bs = "re"), 
                                      data = moddat_mmaa)  
            
            # max temperature 
              moddat_mmha <- d_cort3[d_cort3$max_anomaly > -20 & is.na(d_cort3$max_cort) == FALSE, ]
              moddat_mmha$response <- log(moddat_mmha$max_cort)
              m_max_max_a <- mgcv::gam(log(max_cort) ~ s(max_anomaly, sp = 0.1) + s(alpha4, bs = "re") + 
                                        s(max_anomaly, alpha4, bs = "re"), 
                                      data = moddat_mmha) 
            
            # min temperature  
              moddat_mmla <- d_cort3[d_cort3$min_anomaly > -20 & is.na(d_cort3$max_cort) == FALSE, ]
              moddat_mmla$response <- log(moddat_mmla$max_cort)
              m_max_min_a <- mgcv::gam(log(max_cort) ~ s(min_anomaly, sp = 0.1) + s(alpha4, bs = "re") + 
                                        s(min_anomaly, alpha4, bs = "re"), 
                                      data = moddat_mmla) 
        
        # speed cort   
            # average temperature
              moddat_msaa <- d_cort3[d_cort3$avg_anomaly > -20 & d_cort3$speed > 0, ]
              moddat_msaa$response <- log(moddat_msaa$speed)
              m_spd_avg_a <- mgcv::gam(log(speed) ~ s(avg_anomaly, sp = 0.1) + s(alpha4, bs = "re") + 
                                       s(avg_anomaly, alpha4, bs = "re"), 
                                     data = moddat_msaa)  
            
            # max temperature  
              moddat_msha <- d_cort3[d_cort3$max_anomaly > -20 & d_cort3$speed > 0, ]
              moddat_msha$response <- log(moddat_msha$speed)
              m_spd_max_a <- mgcv::gam(log(speed) ~ s(max_anomaly, sp = 0.1) + s(alpha4, bs = "re") + 
                                       s(max_anomaly, alpha4, bs = "re"), 
                                     data = moddat_msha) 
            
            # min temperature  
              moddat_msla <- d_cort3[d_cort3$min_anomaly > -20 & d_cort3$speed > 0, ]
              moddat_msla$response <- log(moddat_msla$speed)
              m_spd_min_a <- mgcv::gam(log(speed) ~ s(min_anomaly, sp = 0.1) + s(alpha4, bs = "re") + 
                                       s(min_anomaly, alpha4, bs = "re"), 
                                     data = moddat_msla)               
            
## Custom plotting function to use with the GAMMs ----
              # Create a function for plots
        gam_cort_plot <- function(model_name = NA, lab_lett = "A", xlabber = "Average Temperature (°C)",
                                  moddat = NA, temp_var = NA,
                                  ylabber = "Baseline Corticosterone (log ng/ml)",
                                  col_col = "#D95F02", fill_col = "#D95F02", p_obj = p_obj_in){

        # get intercept
            intercept <- coef(model_name)[1]

            # Create a new data frame for predictions (using a sequence of temperature values)
            new_data <- data.frame(temp_var = seq(min(na.omit(moddat[[temp_var]])),
                                                     max(na.omit(moddat[[temp_var]])), length.out = 100))

        # get details from fit model
            p_obj <- plot(model_name, residuals = TRUE, pages = 1)
            p_obj2 <- p_obj[[1]]
            sm_df <- as.data.frame(p_obj2[c("x", "se", "fit")])
            data.df <- as.data.frame(p_obj2[c("raw", "p.resid")])

        # add intercept to fit values
            sm_df$fit <- sm_df$fit + intercept

        # Add the intercept to the residuals
            data.df$p.resid <- data.df$p.resid + intercept

        # make the plot
            ggplot(sm_df, aes(x = x, y = fit)) +
              geom_point(data = data.df, mapping = aes(x = raw, y = p.resid), size = 0.6, color = "gray50", alpha = 0.3) +
              geom_ribbon(aes(ymin = fit-se*1.96, ymax = fit+se*1.96, y = NULL), alpha = 0.35, fill = fill_col) +
              geom_line(color = col_col, size = 1.2) +
              xlab(xlabber) +
              ylab(ylabber) +
              theme_bw() +
              theme(panel.grid = element_blank(), axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
              annotate("text", x = -Inf, y = Inf, label = lab_lett, hjust = -0.5, vjust = 1.5, size = 6)
        }
## Make plots and tables for all species combined ----                    
    
      # now make plots with the function  
        # for average temperature
            # base cort average temperature
              p_base_avg <- gam_cort_plot(m_base_avg, lab_lett = "A", xlabber = "Average Temperature (°C)",
                                          ylabber = "Baseline Corticosterone \n (log ng/ml)", moddat = moddat_mba, temp_var = "T2M_correct")
              
            # max cort average temperature
              p_max_avg <- gam_cort_plot(m_max_avg, lab_lett = "B", xlabber = "Average Temperature (°C)",
                                          ylabber = "Maximum Corticosterone \n (log ng/ml)", moddat = moddat_mma, temp_var = "T2M_correct")
              
            # cort speed average temperature
              p_spd_avg <- gam_cort_plot(m_spd_avg, lab_lett = "C", xlabber = "Average Temperature (°C)",
                                         ylabber = "Corticosterone Speed \n (log ng/ml/min)", moddat = moddat_msa, temp_var = "T2M_correct")
    
            # combine plots     
              avg_combo <- ggarrange(p_base_avg + ylim(c(0, 4.5)) + xlim(c(-3,38)), p_max_avg + ylim(c(2, 5.5)) + xlim(c(-3,38)), p_spd_avg + ylim(c(-1, 2.5)) + xlim(c(-3,38)), nrow = 1)
              avg_combo
              ggsave(here::here("figs_and_tabs", "p_avg_combo.png"), avg_combo,
                     device = "png", width = 9.5, height = 2.9, dpi = 300, units = "in")
              
        # for minimum temperature
            # base cort min temperature
              p_base_min <- gam_cort_plot(m_base_min, lab_lett = "A", xlabber = "Minimum Temperature (°C)",
                                          ylabber = "Baseline Corticosterone \n (log ng/ml)", moddat = moddat_mbl, temp_var = "T2M_MIN_correct")
              
            # max cort min temperature
              p_max_min <- gam_cort_plot(m_max_min, lab_lett = "B", xlabber = "Minimum Temperature (°C)",
                                         ylabber = "Maximum Corticosterone \n (log ng/ml)", moddat = moddat_mml, temp_var = "T2M_MIN_correct")
              
            # cort speed min temperature
              p_spd_min <- gam_cort_plot(m_spd_min, lab_lett = "C", xlabber = "Minimum Temperature (°C)",
                                         ylabber = "Corticosterone Speed \n (log ng/ml/min)", moddat = moddat_msl, temp_var = "T2M_MIN_correct")
              
            # combine plots     
              min_combo <- ggarrange(p_base_min + ylim(c(0, 5)) + xlim(c(-6, 30)), p_max_min + ylim(c(2, 6)) + xlim(c(-6, 30)), 
                                     p_spd_min + ylim(c(-1, 3.5)) + xlim(c(-6, 30)), nrow = 1)
              ggsave(here::here("figs_and_tabs", "p_min_combo.png"), min_combo,
                     device = "png", width = 9.5, height = 2.9, dpi = 300, units = "in")

        # for max temperature
            # base cort max temperature
              p_base_max <- gam_cort_plot(m_base_max, lab_lett = "A", xlabber = "Maximum Temperature (°C)",
                                          ylabber = "Baseline Corticosterone \n (log ng/ml)", moddat = moddat_mbh, temp_var = "T2M_MAX_correct")
              
            # max cort max temperature
              p_max_max <- gam_cort_plot(m_max_max, lab_lett = "B", xlabber = "Maximum Temperature (°C)",
                                         ylabber = "Maximum Corticosterone \n (log ng/ml)", moddat = moddat_mmh, temp_var = "T2M_MAX_correct")
              
            # cort speed min temperature
              p_spd_max <- gam_cort_plot(m_spd_max, lab_lett = "C", xlabber = "Maximum Temperature (°C)",
                                         ylabber = "Corticosterone Speed \n (log ng/ml/min)", moddat = moddat_msh, temp_var = "T2M_MAX_correct")
              
            # combine plots     
              max_combo <- ggarrange(p_base_max + ylim(c(-1, 5)), p_max_max + ylim(c(1.5, 5.5)), p_spd_max + ylim(c(-2, 2.5)), nrow = 1)
              ggsave(here::here("figs_and_tabs", "p_max_combo.png"), max_combo,
                     device = "png", width = 9.5, height = 2.9, dpi = 300, units = "in")              
                      
      
      # make some tables
           avg_table <- tab_model(m_base_avg, m_max_avg, m_spd_avg,
                                  pred.labels = c("Intercept", "Average Temperature Smooth",
                                                  "Species Random Effect", "Avg. Temp * Species Smooth"),
                                  dv.labels = c("log(Base Cort)", "log(Max Cort)", "log(Cort Speed)"),
                                  title = "Average Temperature",
                                  show.stat = TRUE, show.fstat = TRUE)   
           
           min_table <- tab_model(m_base_min, m_max_min, m_spd_min,
                                  pred.labels = c("Intercept", "Minimum Temperature Smooth",
                                                  "Species Random Effect", "Min. Temp * Species Smooth"),
                                  dv.labels = c("log(Base Cort)", "log(Max Cort)", "log(Cort Speed)"),
                                  title = "Minimum Temperature",
                                  show.stat = TRUE, show.fstat = TRUE)  
           
           max_table <- tab_model(m_base_max, m_max_max, m_spd_max,
                                  pred.labels = c("Intercept", "Maximum Temperature Smooth",
                                                  "Species Random Effect", "Max. Temp * Species Smooth"),
                                  dv.labels = c("log(Base Cort)", "log(Max Cort)", "log(Cort Speed)"),
                                  title = "Maximum Temperature",
                                  show.stat = TRUE, show.fstat = TRUE) 
           
## Repeat plots and tables using temperature anomaly ----
        # for average temperature anomaly
            # base cort average temperature
              p_base_avg_a <- gam_cort_plot(m_base_avg_a, lab_lett = "A", xlabber = "Average Anomaly (°C)",
                                          ylabber = "Baseline Corticosterone \n (log ng/ml)", moddat = moddat_mbaa, temp_var = "avg_anomaly")
              
            # max cort average temperature
              p_max_avg_a <- gam_cort_plot(m_max_avg_a, lab_lett = "B", xlabber = "Average Anomaly (°C)",
                                          ylabber = "Maximum Corticosterone \n (log ng/ml)", moddat = moddat_mmaa, temp_var = "avg_anomaly")
              
            # cort speed average temperature
              p_spd_avg_a <- gam_cort_plot(m_spd_avg_a, lab_lett = "C", xlabber = "Average Anomaly (°C)",
                                         ylabber = "Corticosterone Speed \n (log ng/ml/min)", moddat = moddat_msaa, temp_var = "avg_anomaly")
           
            # combine plots     
              avg_combo_a <- ggarrange(p_base_avg_a + ylim(c(-1, 5.1)), p_max_avg_a + ylim(c(1.5, 6)), p_spd_avg_a + ylim(c(-2.5, 4)), nrow = 1)
              ggsave(here::here("figs_and_tabs", "p_avg_combo_a.png"), avg_combo_a,
                     device = "png", width = 9.5, height = 2.9, dpi = 300, units = "in")
              
        # for minimum temperature anomaly
            # base cort min temperature
              p_base_min_a <- gam_cort_plot(m_base_min_a, lab_lett = "A", xlabber = "Minimum Anomaly (°C)",
                                          ylabber = "Baseline Corticosterone \n (log ng/ml)", moddat = moddat_mbla, temp_var = "min_anomaly")
              
            # max cort min temperature
              p_max_min_a <- gam_cort_plot(m_max_min_a, lab_lett = "B", xlabber = "Minimum Anomaly (°C)",
                                         ylabber = "Maximum Corticosterone \n (log ng/ml)", moddat = moddat_mmla, temp_var = "min_anomaly")
              
            # cort speed min temperature
              p_spd_min_a <- gam_cort_plot(m_spd_min_a, lab_lett = "C", xlabber = "Minimum Anomaly (°C)",
                                         ylabber = "Corticosterone Speed \n (log ng/ml/min)", moddat = moddat_msla, temp_var = "min_anomaly")
              
            # combine plots     
              min_combo_a <- ggarrange(p_base_min_a + ylim(c(0, 5)) + xlim(c(-13, 30)), p_max_min_a + ylim(c(1.5, 5.5)) + xlim(c(-13, 30)), 
                                       p_spd_min_a + ylim(c(-2.5, 4)) + xlim(c(-13, 30)), nrow = 1)
              ggsave(here::here("figs_and_tabs", "p_min_combo_a.png"), min_combo_a,
                     device = "png", width = 9.5, height = 2.9, dpi = 300, units = "in")

        # for max temperature anomaly
            # base cort max temperature
              p_base_max_a <- gam_cort_plot(m_base_max_a, lab_lett = "A", xlabber = "Maximum Anomaly (°C)",
                                          ylabber = "Baseline Corticosterone \n (log ng/ml)", moddat = moddat_mbha, temp_var = "max_anomaly")
              
            # max cort max temperature
              p_max_max_a <- gam_cort_plot(m_max_max_a, lab_lett = "B", xlabber = "Maximum Anomaly (°C)",
                                         ylabber = "Maximum Corticosterone \n (log ng/ml)", moddat = moddat_mmha, temp_var = "max_anomaly")
              
            # cort speed min temperature
              p_spd_max_a <- gam_cort_plot(m_spd_max_a, lab_lett = "C", xlabber = "Maximum Anomaly (°C)",
                                         ylabber = "Corticosterone Speed \n (log ng/ml/min)", moddat = moddat_msha, temp_var = "max_anomaly")
              
            # combine plots     
              max_combo_a <- ggarrange(p_base_max_a + ylim(c(-0.1, 5)), p_max_max_a + ylim(c(2, 5.5)), p_spd_max_a + ylim(c(-2, 3)), nrow = 1)
              ggsave(here::here("figs_and_tabs", "p_max_combo_a.png"), max_combo_a,
                     device = "png", width = 9.5, height = 2.9, dpi = 300, units = "in")              
                      
      
      # make some tables
           avg_table_a <- tab_model(m_base_avg_a, m_max_avg_a, m_spd_avg_a,
                                  pred.labels = c("Intercept", "Average Temp. Anomaly Smooth",
                                                  "Species Random Effect", "Avg. Anomaly * Species Smooth"),
                                  dv.labels = c("log(Base Cort)", "log(Max Cort)", "log(Cort Speed)"),
                                  title = "Average Temperature Anomaly",
                                  show.stat = TRUE, show.fstat = TRUE)   
           
           min_table_a <- tab_model(m_base_min_a, m_max_min_a, m_spd_min_a,
                                  pred.labels = c("Intercept", "Minimum Temp. Anomaly Smooth",
                                                  "Species Random Effect", "Min. Anomaly * Species Smooth"),
                                  dv.labels = c("log(Base Cort)", "log(Max Cort)", "log(Cort Speed)"),
                                  title = "Minimum Temperature Anomaly",
                                  show.stat = TRUE, show.fstat = TRUE)  
           
           max_table_a <- tab_model(m_base_max_a, m_max_max_a, m_spd_max_a,
                                  pred.labels = c("Intercept", "Maximum Temp. Anomaly Smooth",
                                                  "Species Random Effect", "Max. Anomaly * Species Smooth"),
                                  dv.labels = c("log(Base Cort)", "log(Max Cort)", "log(Cort Speed)"),
                                  title = "Maximum Temperature Anomaly",
                                  show.stat = TRUE, show.fstat = TRUE)            

      
## Individual species GAMs (NO LONGER INCLUDED) ----
           
  # Removed from manuscript...not enough data to be useful
    five_spps <- subset(d_cort3, d_cort3$alpha4 == "CORE" | d_cort3$alpha4 == "DEJU" |
                         d_cort3$alpha4 == "GWCS" |
                         d_cort3$alpha4 == "SAVS" | d_cort3$alpha4 == "SMLO")
    
    five_spps <- subset(five_spps, five_spps$T2M_MIN_correct > -10)              
    
    five_spps <- five_spps[, c("latitude", "longitude", "alpha4", "base_cort", "max_cort", "doy",
                               "T2M_correct", "T2M_MIN_correct", "T2M_MAX_correct", "speed", "year")]
    
    mCORE_b <- lm(log(base_cort) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "CORE", ])
    mCORE_m <- lm(log(max_cort) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "CORE", ])
    mCORE_s <- lm(log(speed) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "CORE" & five_spps$speed > 0, ])
    
    mDEJU_b <- lm(log(base_cort) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "DEJU", ])
    mDEJU_m <- lm(log(max_cort) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "DEJU", ])
    mDEJU_s <- lm(log(speed) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "DEJU" & five_spps$speed > 0, ])
    
    mGWCS_b <- lm(log(base_cort) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "GWCS", ])
    mGWCS_m <- lm(log(max_cort) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "GWCS", ])
    mGWCS_s <- lm(log(speed) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "GWCS" & five_spps$speed > 0, ])
    
    mSAVS_b <- lm(log(base_cort) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "SAVS", ])
    mSAVS_m <- lm(log(max_cort) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "SAVS", ])
    mSAVS_s <- lm(log(speed) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "SAVS" & five_spps$speed > 0, ])
    
    mSMLO_b <- lm(log(base_cort) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "SMLO", ])
    mSMLO_m <- lm(log(max_cort) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "SMLO", ])
    mSMLO_s <- lm(log(speed) ~ T2M_MIN_correct, data = five_spps[five_spps$alpha4 == "SMLO" & five_spps$speed > 0, ])
    

    
    sp_base <- ggplot(five_spps, mapping = aes(x = T2M_MIN_correct, y = log(base_cort), fill = alpha4, color = alpha4)) +
      geom_point(alpha = 0.3, mapping = aes(color = alpha4)) +
      geom_smooth(se = FALSE, mapping = aes(color = alpha4, linetype = alpha4), method = "lm") +
      scale_linetype_manual(values = c("11", "51", "solid", "11", "11")) +
      #scale_linetype_discrete(values = c("solid", "solid", "longdash", "3313", "dotted")) +
      theme_bw() +
      theme(panel.grid = element_blank(), axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      xlab("Minimum Temperature (°C)") +
      ylab("Baseline Corticosterone \n (log ng/ml)") +
      guides(linetype = "none", fill = "none", color = "none") +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5, size = 6)
    
    sp_max <- ggplot(five_spps, mapping = aes(x = T2M_MIN_correct, y = log(max_cort), fill = alpha4)) +
      geom_point(alpha = 0.4, mapping = aes(color = alpha4)) +
      geom_smooth(se = FALSE, mapping = aes(color = alpha4, linetype = alpha4), method = "lm") +
      scale_linetype_manual(values = c("solid", "51", "11", "11", "11")) +
      theme_bw() +
      theme(panel.grid = element_blank(), axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      xlab("Minimum Temperature (°C)") +
      ylab("Maximum Corticosterone \n (log ng/ml)") +
      guides(linetype = "none", fill = "none", color = "none") +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5, size = 6)
    
    sp_spd <- ggplot(five_spps, mapping = aes(x = T2M_MIN_correct, y = log(speed), fill = alpha4)) +
      geom_point(alpha = 0.4, shape = 21, mapping = aes(color = alpha4)) +
      geom_smooth(se = FALSE, mapping = aes(color = alpha4, linetype = alpha4), method = "lm") +
      scale_linetype_manual(values = c("solid", "51", "11", "11", "51")) +
      theme_bw() +
      theme(panel.grid = element_blank(), axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      xlab("Minimum Temperature (°C)") +
      ylab("Corticosterone Speed \n (log ng/ml/min)") +
      labs(fill = "Species", color = "Species", linetype = "Species") +
      ylim(c(-1, 3)) +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5, size = 6) +
      guides(linetype = guide_legend(override.aes = list(linetype = "solid")))
      
    
    p_5sps <- ggpubr::ggarrange(sp_base, sp_max, sp_spd, nrow = 1, widths = c(1, 1, 1.4))
    ggsave(here::here("figs_and_tabs", "p_5sps.png"), p_5sps,
           device = "png", width = 9.5, height = 2.9, dpi = 300, units = "in")
    
    
    
