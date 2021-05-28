# Shiny App Development, Carbon Containment Lab
library(shiny)
library(usmap)
library(readxl)

# Read in libraries for map production
library(usmap)
library(stringr)
library(grid)
library(gridExtra)

# Read in libraries for data wrangling
library(tidyverse)
library(sf)
library(plotly)
library(scales)
library(kableExtra)
library(dplyr)

ui <- fluidPage(
  titlePanel("Effects of Treatments on Total Potential, Separated by Region"),
  
  sidebarLayout(position = "right", 
                mainPanel(h1("Plot Display"), plotOutput("USMap"), 
                          tableOutput("SummaryTable"),
                          plotOutput("PoissonFire")),
                sidebarPanel(h1("Selections for Scenario"),
                             fluidRow(
                               radioButtons("radio_region", h3("Region for Consideration"),
                                            choices = list("East WA" = 1,
                                                           "East OR" = 2,
                                                           "High Mortality" = 3,
                                                           "High Potential" = 4,
                                                           "Warm Springs" = 5,
                                                           "Yakima Nation" = 6,
                                                           "Total PNW" = 7),
                                            selected = 1),
                               radioButtons("radio_prior", h3("Prior Treatment Assumed"),
                                            choices = list("Slash Piles" = 1,
                                                           "Wood Chipping" = 2,
                                                           "No Prior Treatment" = 3),
                                            selected = 1),
                               radioButtons("radio_burns", h3("Prescribed Burns?"),
                                            choices = list("Yes" = 1, "No" = 2),
                                            selected = 1),
                               numericInput("num_acres", h3("Number of Acres treated yearly"),
                                            value = 1000
                               ),
                               
                               sliderInput("slider_hold", h3("Percentage of Wood Permanently Stored"),
                                           min = 0, max = 100, value = 10),
                               sliderInput("slider_years", h3("Years of Storage Considered"),
                                           min = 0, max = 100, value = 10),
                               sliderInput("num", h3("Reduction in K-Value (percent)"),
                                           min = 0, max = 15, value = 0)
                             )
                )
  )
  
)


server <- function(input, output) {
  # Output showing county by county Total Loss
  output$USMap <- renderPlot({
    x <- NA
    dwm <- read_excel("WAOR_DWMTotal.xlsx")
    dwm <- dwm[3:nrow(dwm), 1:2]
    names(dwm) <- c("county", "dwm_total")
    
    sdw <- read_excel("WAOR_StandingDeadWoodTotal.xlsx")
    sdw <- sdw[3:nrow(sdw), 1:2]
    names(sdw) <- c("county", "sdw_total")
    
    live <- read_excel("WAOR_TreeCarbon_Total.xlsx")
    live <- live[3:nrow(live), 1:2]
    names(live) <- c("county", "live_total")
    
    acres <- read_excel("WAOR_Acreage.xlsx")
    acres <- acres[3:nrow(acres), 1:2]
    names(acres) <- c("county", "acres")
    
    dead <- read_excel("WAOR_DeadTransfer_Rates.xlsx")
    dead <- dead[3:nrow(dead), 1:2]
    names(dead) <- c("county", "dead_rate")
    dead$dead_rate[dead$dead_rate == "-"] <- 0
    
    grossgrowth <- read_excel("WAOR_GrossGrowth_Rates.xlsx")
    grossgrowth <- grossgrowth[3:nrow(grossgrowth), 1:2]
    names(grossgrowth) <- c("county", "grossgrowth")
    grossgrowth$grossgrowth <- as.numeric(grossgrowth$grossgrowth)
    
    mort <- read_excel("WAOR_Mortality_Rates.xlsx")
    mort <- mort[3:nrow(mort), 1:2]
    names(mort) <- c("county", "mortality")
    mort$mortality[mort$mortality == "-"] <- 0
    mort$mortality <- as.numeric(mort$mortality)
    
    x <- merge(dwm, sdw, by = "county")
    x <- merge(x, live, by = "county")
    x <- merge(x, acres, by = "county")
    x <- merge(x, grossgrowth, by = "county")
    x <- merge(x, mort, by = "county")
    x <- merge(x, dead, by = "county")
    
    x$fips <- substr(x$county, 1, 5)
    x$fips = str_remove(x$fips, "^0+")
    
    x[,2:8] <- sapply(x[,2:8], as.numeric)
    
    x <- rbind(x, data.frame("county" = c("Asotin, WA ", "Douglas, WA ", "Grant, WA ", "Adams, WA ", "Franklin, WA ", "Benton, WA ", "Sherman OR ", "Gilliam OR "),
                             "dwm_total" = c(0,0,0,0,0,0,0,0), "sdw_total" = c(0,0,0,0,0,0,0,0), 
                             "live_total" = c(0,0,0,0,0,0,0,0), 
                             "acres" = c(0,0,0,0,0,0,0,0), "grossgrowth" = c(0,0,0,0,0,0,0,0),
                             "mortality" = c(0,0,0,0,0,0,0,0), "dead_rate" = c(0,0,0,0,0,0,0,0),
                             "fips" = c("53003", "53017", "53025", "53001", "53021", "53005", "41055", "41021")))
    
    if(input$radio_region == 1) {
      x <- x[grep("Okanogan|Chelan|Kittitas|Yakima|Klickitat|Douglas|Grant|Benton|Franklin|Adams|Lincoln|Ferry|Stevens|Pend Oreille|Spokane|Whitman|Garfield|Walla Walla|Columbia|Asotin", x$county),]
      x <- x[grep(" WA ", x$county),]
    } else if (input$radio_region == 2) {
      x <- x[-grep("Clatsop|Columbia|Multnomah|Hood River|Clackamas|Washington|Tillamook|Yamhill|Marion|Polk|Lincoln|Benton|Linn|Lane|Douglas|Coos|Curry|Josephine|Jackson", x$county),]
      x <- x[grep(" OR ", x$county),]
    } else if (input$radio_region == 3) {
      x <- x[x$fips %in% c("41031", "41051", "53007", "53013", "53023", "53037", "53047", "53077"),]
    } else if (input$radio_region == 4) {
      x <- x[x$fips %in% c("41001", "41013", "41017", "41035", "41037", "41045", "41049", "41065"),]
    } else if (input$radio_region == 5) {
      x <- x[grep("OR Jefferson", x$county),]
    } else if (input$radio_region == 6) {
      x <- x[grep("Yakima", x$county),]
    }
    
    years_btwn <- ceiling(sum(as.numeric(x$acres)) / input$num_acres)
    
    # Comparison of baseline (initial) to treated (x)
    initial_x <- x
    
    # Set the baseline value of k
    initial_k <- 0.10
    
    # Deal with determining what the k-rate should be
    if(input$radio_prior == 1) {
      # Slash and control burns
      k_rate <- 0.10
      fire_risk <- 0.005
    } else if (input$radio_prior == 2) {
      # Wood chipping and control burns 
      k_rate <- 0.15
      fire_risk <- 0.005
    } else if (input$radio_prior == 3) {
      # Nothing
      k_rate <- 0.10
      fire_risk <- 0.02
    }
    
    # If you are using prescribed burns
    if (input$radio_burns == 1) fire_risk <- fire_risk * 0.63
    
    # Deal with the slider concerning how much is held out
    holdout <- x
    holdout$dwm_total <- holdout$dwm_total * input$slider_hold / 100
    holdout$sdw_total <- holdout$sdw_total * input$slider_hold / 100
    holdout$live_total <- holdout$live_total * input$slider_hold / 100
    
    # Remove the holdout from the treated frame (it's added back later after decay)
    x$dwm_total <- x$dwm_total * (100 - input$slider_hold) / 100
    x$sdw_total <- x$sdw_total * (100 - input$slider_hold) / 100
    x$live_total <- x$live_total * (100 - input$slider_hold) / 100
    
    # Now look at the reduction of k-rate 
    k_rate <- k_rate - (input$num / 100)
    
    # Store baselines for decay
    k_rate_standard <- k_rate
    initial_k_standard <- initial_k
    
    # Store baseline for fire risk
    fire_risk_standard <- fire_risk
    
    # Initialize effect counters as moot values
    growth_boost <- -1
    fire_counter <- -1
    dodge_extreme <- -1
    
    # How many years are we running this?
    years_left <- input$slider_years
    
    # Initial growth rates
    regular_growth <- x$grossgrowth
    
    # Take a step forward in time with each iteration of this loop
    while (years_left > 0) {
      # Growth boost accounts for years after a fire elevating the 
      # rate of growth to a x3 level
      if (growth_boost > 0) {
        x$grossgrowth <- growth_pattern[growth_boost]
        initial_x$grossgrowth <- growth_pattern[growth_boost]
        growth_boost <- growth_boost - 1
      }
      if (growth_boost == 0) {
        x$grossgrowth <- regular_growth
        initial_x$grossgrowth <- regular_growth
        growth_boost <- -1
      }
      # If treatment occurred, k-rate slowly climbs back up
      if (k_rate < k_rate_standard) k_rate <- k_rate + 0.01
      
      # If treatment occurred, fire risk climbs after 5 years by .01 
      # until it returns to regular level
      if (fire_counter > 0) fire_counter <- fire_counter - 1
      if (fire_counter == 0 & fire_risk < fire_risk_standard) {
        fire_risk <- fire_risk + .01
      }
      if (fire_counter == 0 & fire_risk == fire_risk_standard) {
        fire_counter <- -1
      }
      
      if (dodge_extreme > 0) dodge_extreme <- dodge_extreme - 1
      if (dodge_extreme == 0) dodge_extreme <- -1
      
      # Prescribed burn occurs every six years, takes 10% of volume of 
      # dead wood stocks
      if (input$radio_burns == 1 & years_left %% 6 == 0) {
        x$dwm_total <- x$dwm_total * 0.9
        x$sdw_total <- x$sdw_total * 0.9
        
        initial_x$dwm_total <- initial_x$dwm_total * 0.9
        initial_x$sdw_total <- initial_x$sdw_total * 0.9
      }
      
      # Show annual decay of material
      x$dwm_total <- x$dwm_total * exp(-k_rate)
      initial_x$dwm_total <- initial_x$dwm_total * exp(-initial_k)
      x$sdw_total <- x$sdw_total * exp(-k_rate)
      initial_x$sdw_total <- initial_x$sdw_total * exp(-initial_k)
      
      # Move living to standing dead pool at mortality rate
      x$sdw_total <- x$sdw_total + (x$live_total * x$mortality)
      initial_x$sdw_total <- initial_x$sdw_total + (initial_x$live_total * initial_x$mortality)
      x$live_total <- x$live_total - (x$live_total * x$mortality)
      initial_x$live_total <- initial_x$live_total - (initial_x$live_total * initial_x$mortality)
      
      # Move standing to downed at dead wood transfer rate
      x$dwm_total <- x$dwm_total + (x$sdw_total * x$dead_rate)
      initial_x$dwm_total <- initial_x$dwm_total + (initial_x$sdw_total * initial_x$dead_rate)
      x$sdw_total <- x$sdw_total - (x$sdw_total * x$dead_rate)
      initial_x$sdw_total <- initial_x$sdw_total - (initial_x$sdw_total * initial_x$dead_rate)
      
      # Increase living by growth rate
      x$live_total <- x$live_total * (1 + x$grossgrowth)
      initial_x$live_total <- initial_x$live_total * (1 + initial_x$grossgrowth)
      
      # If a fire occurs in a particular year
      if(runif(1) < fire_risk) {
        fire_type <- runif(1)
        if (dodge_extreme > 0 & fire_type > 0.65) fire_type <- 0.64
        if (fire_type < 0.4) {
          # Low-type fire
          # Loss of total volume is only 30%
          x$dwm_total <- x$dwm_total * 0.7
          x$sdw_total <- x$sdw_total * 0.7
          x$live_total <- x$live_total * 0.7
          
          initial_x$dwm_total <- initial_x$dwm_total * 0.7
          initial_x$sdw_total <- initial_x$sdw_total * 0.7
          initial_x$live_total <- initial_x$live_total * 0.7
          
          # New growth rates - return to normal over six years
          growth_pattern <- c(0.01, 0.01, 0.03, 0.06, 0.11, 0.15)
          growth_boost <- 6
          
        } else if (0.40 < fire_type & fire_type < 0.65) {
          # Moderate-type fire
          # Loss of total volume is 45%
          x$dwm_total <- x$dwm_total * 0.55
          x$sdw_total <- x$sdw_total * 0.55
          x$live_total <- x$live_total * 0.55
          
          initial_x$dwm_total <- initial_x$dwm_total * 0.55
          initial_x$sdw_total <- initial_x$sdw_total * 0.55
          initial_x$live_total <- initial_x$live_total * 0.55
          
          # New growth rates - return to normal over eight years
          growth_pattern <- c(0.02, 0.02, 0.04, 0.05, 0.08, 0.10, 0.14, 0.18)
          growth_boost <- 8
          
        } else if (0.65 < fire_type & fire_type < 0.90) {
          # High-type fire
          # Loss of total volume is 70%
          x$dwm_total <- x$dwm_total * 0.3
          x$sdw_total <- x$sdw_total * 0.3
          x$live_total <- x$live_total * 0.3
          
          initial_x$dwm_total <- initial_x$dwm_total * 0.3
          initial_x$sdw_total <- initial_x$sdw_total * 0.3
          initial_x$live_total <- initial_x$live_total * 0.3
          
          # New growth rates - return to normal over twelve years
          growth_pattern <- c(0.04, 0.04, 0.06, 0.06, 0.08, 0.09, 
                              0.11, 0.12, 0.14, 0.16, 0.18, 0.20)
          growth_boost <- 12
          
          # 15 years until another extreme fire can occur
          dodge_extreme <- 15
          
        } else {
          # Extremely high-type fire
          # Loss of total volume is 90%
          x$dwm_total <- x$dwm_total * 0.1
          x$sdw_total <- x$sdw_total * 0.1
          x$live_total <- x$live_total * 0.1
          
          initial_x$dwm_total <- initial_x$dwm_total * 0.1
          initial_x$sdw_total <- initial_x$sdw_total * 0.1
          initial_x$live_total <- initial_x$live_total * 0.1
          
          # New growth rates - return to 90% over twenty years
          growth_pattern <- c(0.02, 0.02, 0.03, 0.03, 0.05, 
                              0.05, 0.06, 0.07, 0.08, 0.09,
                              0.11, 0.13, 0.15, 0.17, 0.19, 
                              0.20, 0.21, 0.23, 0.24, 0.25)
          growth_boost <- 20
          
          # 25 years until another extreme fire can occur
          dodge_extreme <- 25
        }
      }
      
      # If land is treated this year, make the adjustment
      if (years_left %% years_btwn == 0) {
        # If treated, k_rate goes to zero and fire risk goes down for 5 years
        # then creeps up to initial value
        k_rate <- 0
        fire_risk <- 0
        fire_counter <- 5
      }
      # Move forward one year
      years_left <- years_left - 1
    }
    
    # Add the holdout values back to treatment scenario
    x$dwm_total <- x$dwm_total + holdout$dwm_total
    x$sdw_total <- x$sdw_total + holdout$sdw_total
    x$live_total <- x$live_total + holdout$live_total
    
    # Calculate the overalls 
    x$overall <- x$dwm_total + x$sdw_total + x$live_total
    initial_x$overall <- initial_x$dwm_total + initial_x$sdw_total + initial_x$live_total
    
    # Calculate improvement
    x$improvement <- x$overall - initial_x$overall
    
    # Put in terms of millions of bdt
    x$improvement <- x$improvement / 1000000
    x$overall <- x$overall / 1000000
    
    # # Finally, produce the plot
    # p1 <- plot_usmap(include = c("WA", "OR"), regions = "county",
    #                  data = y[, c("fips", "overall")],
    #                  values = "overall", color = "black") +
    #   labs(title = "Overall Carbon Storage per County",
    #        subtitle = "Measured in Millions Short Dry Tons of Carbon",
    #        caption = "Source: FIA") +
    #   scale_fill_gradient2("Million Dry Tons", low = "#800d00",
    #                        mid = "white", high = "#065735", midpoint = 0, space = "Lab",
    #                        na.value = "grey75", guide = "colourbar",
    #                        aesthetics = "fill", label = scales::comma) +
    #   theme(legend.position = "right") +
    #   theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    
    p2 <- plot_usmap(include = c("WA", "OR"), regions = "county",
                     data = x[, c("fips", "improvement")],
                     values = "improvement", color = "black") +
      labs(title = "Difference in Carbon Storage over Baseline Model",
           subtitle = "Measured in Millions Short Dry Tons of Carbon",
           caption = "Source: FIA") +
      scale_fill_gradient2("Million Dry Tons", low = "#800d00",
                           mid = "white", high = "#065735", midpoint = 0, space = "Lab",
                           na.value = "grey75", guide = "colourbar",
                           aesthetics = "fill", label = scales::comma) +
      theme(legend.position = "right") +
      theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    
    # Matrix for plot layout
    m <- matrix(c(1),
                byrow = TRUE, ncol = 4)
    # Produce output
    grid.arrange(p2, layout_matrix = m)
  })
  
  # Table showing results of calculation
  output$SummaryTable <- function() {
    x <- NA
    dwm <- read_excel("WAOR_DWMTotal.xlsx")
    dwm <- dwm[3:nrow(dwm), 1:2]
    names(dwm) <- c("county", "dwm_total")
    
    sdw <- read_excel("WAOR_StandingDeadWoodTotal.xlsx")
    sdw <- sdw[3:nrow(sdw), 1:2]
    names(sdw) <- c("county", "sdw_total")
    
    live <- read_excel("WAOR_TreeCarbon_Total.xlsx")
    live <- live[3:nrow(live), 1:2]
    names(live) <- c("county", "live_total")
    
    acres <- read_excel("WAOR_Acreage.xlsx")
    acres <- acres[3:nrow(acres), 1:2]
    names(acres) <- c("county", "acres")
    
    dead <- read_excel("WAOR_DeadTransfer_Rates.xlsx")
    dead <- dead[3:nrow(dead), 1:2]
    names(dead) <- c("county", "dead_rate")
    dead$dead_rate[dead$dead_rate == "-"] <- 0
    
    grossgrowth <- read_excel("WAOR_GrossGrowth_Rates.xlsx")
    grossgrowth <- grossgrowth[3:nrow(grossgrowth), 1:2]
    names(grossgrowth) <- c("county", "grossgrowth")
    grossgrowth$grossgrowth <- as.numeric(grossgrowth$grossgrowth)
    
    mort <- read_excel("WAOR_Mortality_Rates.xlsx")
    mort <- mort[3:nrow(mort), 1:2]
    names(mort) <- c("county", "mortality")
    mort$mortality[mort$mortality == "-"] <- 0
    mort$mortality <- as.numeric(mort$mortality)
    
    x <- merge(dwm, sdw, by = "county")
    x <- merge(x, live, by = "county")
    x <- merge(x, acres, by = "county")
    x <- merge(x, grossgrowth, by = "county")
    x <- merge(x, mort, by = "county")
    x <- merge(x, dead, by = "county")
    
    x$fips <- substr(x$county, 1, 5)
    x$fips = str_remove(x$fips, "^0+")
    
    x[,2:8] <- sapply(x[,2:8], as.numeric)
    
    x <- rbind(x, data.frame("county" = c("Asotin, WA ", "Douglas, WA ", "Grant, WA ", "Adams, WA ", "Franklin, WA ", "Benton, WA ", "Sherman OR ", "Gilliam OR "),
                             "dwm_total" = c(0,0,0,0,0,0,0,0), "sdw_total" = c(0,0,0,0,0,0,0,0), 
                             "live_total" = c(0,0,0,0,0,0,0,0), 
                             "acres" = c(0,0,0,0,0,0,0,0), "grossgrowth" = c(0,0,0,0,0,0,0,0),
                             "mortality" = c(0,0,0,0,0,0,0,0), "dead_rate" = c(0,0,0,0,0,0,0,0),
                             "fips" = c("53003", "53017", "53025", "53001", "53021", "53005", "41055", "41021")))
    
    if(input$radio_region == 1) {
      x <- x[grep("Okanogan|Chelan|Kittitas|Yakima|Klickitat|Douglas|Grant|Benton|Franklin|Adams|Lincoln|Ferry|Stevens|Pend Oreille|Spokane|Whitman|Garfield|Walla Walla|Columbia|Asotin", x$county),]
      x <- x[grep(" WA ", x$county),]
    } else if (input$radio_region == 2) {
      x <- x[-grep("Clatsop|Columbia|Multnomah|Hood River|Clackamas|Washington|Tillamook|Yamhill|Marion|Polk|Lincoln|Benton|Linn|Lane|Douglas|Coos|Curry|Josephine|Jackson", x$county),]
      x <- x[grep(" OR ", x$county),]
    } else if (input$radio_region == 3) {
      x <- x[x$fips %in% c("41031", "41051", "53007", "53013", "53023", "53037", "53047", "53077"),]
    } else if (input$radio_region == 4) {
      x <- x[x$fips %in% c("41001", "41013", "41017", "41035", "41037", "41045", "41049", "41065"),]
    } else if (input$radio_region == 5) {
      x <- x[grep("OR Jefferson", x$county),]
    } else if (input$radio_region == 6) {
      x <- x[grep("Yakima", x$county),]
    }
    
    years_btwn <- ceiling(sum(as.numeric(x$acres)) / input$num_acres)
    
    # Comparison of baseline (initial) to treated (x)
    initial_x <- x
    
    # Set the baseline value of k
    initial_k <- 0.10
    
    # Deal with determining what the k-rate should be
    if(input$radio_prior == 1) {
      # Slash and control burns
      k_rate <- 0.10
      fire_risk <- 0.005
    } else if (input$radio_prior == 2) {
      # Wood chipping and control burns 
      k_rate <- 0.15
      fire_risk <- 0.005
    } else if (input$radio_prior == 3) {
      # Nothing
      k_rate <- 0.10
      fire_risk <- 0.02
    }
    
    # If you are using prescribed burns
    if (input$radio_burns == 1) fire_risk <- fire_risk * 0.63
    
    # Deal with the slider concerning how much is held out
    holdout <- x
    holdout$dwm_total <- holdout$dwm_total * input$slider_hold / 100
    holdout$sdw_total <- holdout$sdw_total * input$slider_hold / 100
    holdout$live_total <- holdout$live_total * input$slider_hold / 100
    
    # Remove the holdout from the treated frame (it's added back later after decay)
    x$dwm_total <- x$dwm_total * (100 - input$slider_hold) / 100
    x$sdw_total <- x$sdw_total * (100 - input$slider_hold) / 100
    x$live_total <- x$live_total * (100 - input$slider_hold) / 100
    
    # Now look at the reduction of k-rate 
    k_rate <- k_rate - (input$num / 100)
    
    # Store baselines for decay
    k_rate_standard <- k_rate
    initial_k_standard <- initial_k
    
    # Store baseline for fire risk
    fire_risk_standard <- fire_risk
    
    # Initialize effect counters as moot values
    growth_boost <- -1
    fire_counter <- -1
    dodge_extreme <- -1
    
    # How many years are we running this?
    years_left <- input$slider_years
    
    # Initial growth rates
    regular_growth <- x$grossgrowth
    
    # Take a step forward in time with each iteration of this loop
    while (years_left > 0) {
      # Growth boost accounts for years after a fire elevating the 
      # rate of growth to a x3 level
      if (growth_boost > 0) {
        x$grossgrowth <- growth_pattern[growth_boost]
        initial_x$grossgrowth <- growth_pattern[growth_boost]
        growth_boost <- growth_boost - 1
      }
      if (growth_boost == 0) {
        x$grossgrowth <- regular_growth
        initial_x$grossgrowth <- regular_growth
        growth_boost <- -1
      }
      # If treatment occurred, k-rate slowly climbs back up
      if (k_rate < k_rate_standard) k_rate <- k_rate + 0.01
      
      # If treatment occurred, fire risk climbs after 5 years by .01 
      # until it returns to regular level
      if (fire_counter > 0) fire_counter <- fire_counter - 1
      if (fire_counter == 0 & fire_risk < fire_risk_standard) {
        fire_risk <- fire_risk + .01
      }
      if (fire_counter == 0 & fire_risk == fire_risk_standard) {
        fire_counter <- -1
      }
      
      if (dodge_extreme > 0) dodge_extreme <- dodge_extreme - 1
      if (dodge_extreme == 0) dodge_extreme <- -1
      
      # Prescribed burn occurs every six years, takes 10% of volume of 
      # dead wood stocks
      if (input$radio_burns == 1 & years_left %% 6 == 0) {
        x$dwm_total <- x$dwm_total * 0.9
        x$sdw_total <- x$sdw_total * 0.9
        
        initial_x$dwm_total <- initial_x$dwm_total * 0.9
        initial_x$sdw_total <- initial_x$sdw_total * 0.9
      }
      
      # Show annual decay of material
      x$dwm_total <- x$dwm_total * exp(-k_rate)
      initial_x$dwm_total <- initial_x$dwm_total * exp(-initial_k)
      x$sdw_total <- x$sdw_total * exp(-k_rate)
      initial_x$sdw_total <- initial_x$sdw_total * exp(-initial_k)
      
      # Move living to standing dead pool at mortality rate
      x$sdw_total <- x$sdw_total + (x$live_total * x$mortality)
      initial_x$sdw_total <- initial_x$sdw_total + (initial_x$live_total * initial_x$mortality)
      x$live_total <- x$live_total - (x$live_total * x$mortality)
      initial_x$live_total <- initial_x$live_total - (initial_x$live_total * initial_x$mortality)
      
      # Move standing to downed at dead wood transfer rate
      x$dwm_total <- x$dwm_total + (x$sdw_total * x$dead_rate)
      initial_x$dwm_total <- initial_x$dwm_total + (initial_x$sdw_total * initial_x$dead_rate)
      x$sdw_total <- x$sdw_total - (x$sdw_total * x$dead_rate)
      initial_x$sdw_total <- initial_x$sdw_total - (initial_x$sdw_total * initial_x$dead_rate)
      
      # Increase living by growth rate
      x$live_total <- x$live_total * (1 + x$grossgrowth)
      initial_x$live_total <- initial_x$live_total * (1 + initial_x$grossgrowth)
      
      # If a fire occurs in a particular year
      if(runif(1) < fire_risk) {
        fire_type <- runif(1)
        if (dodge_extreme > 0 & fire_type > 0.65) fire_type <- 0.64
        if (fire_type < 0.4) {
          # Low-type fire
          # Loss of total volume is only 30%
          x$dwm_total <- x$dwm_total * 0.7
          x$sdw_total <- x$sdw_total * 0.7
          x$live_total <- x$live_total * 0.7
          
          initial_x$dwm_total <- initial_x$dwm_total * 0.7
          initial_x$sdw_total <- initial_x$sdw_total * 0.7
          initial_x$live_total <- initial_x$live_total * 0.7
          
          # New growth rates - return to normal over six years
          growth_pattern <- c(0.01, 0.01, 0.03, 0.06, 0.11, 0.15)
          growth_boost <- 6
          
        } else if (0.40 < fire_type & fire_type < 0.65) {
          # Moderate-type fire
          # Loss of total volume is 45%
          x$dwm_total <- x$dwm_total * 0.55
          x$sdw_total <- x$sdw_total * 0.55
          x$live_total <- x$live_total * 0.55
          
          initial_x$dwm_total <- initial_x$dwm_total * 0.55
          initial_x$sdw_total <- initial_x$sdw_total * 0.55
          initial_x$live_total <- initial_x$live_total * 0.55
          
          # New growth rates - return to normal over eight years
          growth_pattern <- c(0.02, 0.02, 0.04, 0.05, 0.08, 0.10, 0.14, 0.18)
          growth_boost <- 8
          
        } else if (0.65 < fire_type & fire_type < 0.90) {
          # High-type fire
          # Loss of total volume is 70%
          x$dwm_total <- x$dwm_total * 0.7
          x$sdw_total <- x$sdw_total * 0.3
          x$live_total <- x$live_total * 0.3
          
          initial_x$dwm_total <- initial_x$dwm_total * 0.3
          initial_x$sdw_total <- initial_x$sdw_total * 0.3
          initial_x$live_total <- initial_x$live_total * 0.3
          
          # New growth rates - return to normal over twelve years
          growth_pattern <- c(0.04, 0.04, 0.06, 0.06, 0.08, 0.09, 
                              0.11, 0.12, 0.14, 0.16, 0.18, 0.20)
          growth_boost <- 12
          
          # 15 years until another extreme fire can occur
          dodge_extreme <- 15
          
        } else {
          # Extremely high-type fire
          # Loss of total volume is 90%
          x$dwm_total <- x$dwm_total * 0.1
          x$sdw_total <- x$sdw_total * 0.1
          x$live_total <- x$live_total * 0.1
          
          initial_x$dwm_total <- initial_x$dwm_total * 0.1
          initial_x$sdw_total <- initial_x$sdw_total * 0.1
          initial_x$live_total <- initial_x$live_total * 0.1
          
          # New growth rates - return to 90% over twenty years
          growth_pattern <- c(0.02, 0.02, 0.03, 0.03, 0.05, 
                              0.05, 0.06, 0.07, 0.08, 0.09,
                              0.11, 0.13, 0.15, 0.17, 0.19, 
                              0.20, 0.21, 0.23, 0.24, 0.25)
          growth_boost <- 20
          
          # 25 years until another extreme fire can occur
          dodge_extreme <- 25
        }
      }
      
      # If land is treated this year, make the adjustment
      if (years_left %% years_btwn == 0) {
        # If treated, k_rate goes to zero and fire risk goes down for 5 years
        # then creeps up to initial value
        k_rate <- 0
        fire_risk <- 0
        fire_counter <- 5
      }
      # Move forward one year
      years_left <- years_left - 1
    }
    
    # Add the holdout values back to treatment scenario
    x$dwm_total <- x$dwm_total + holdout$dwm_total
    x$sdw_total <- x$sdw_total + holdout$sdw_total
    x$live_total <- x$live_total + holdout$live_total
    
    # Calculate the overalls 
    x$overall <- x$dwm_total + x$sdw_total + x$live_total
    initial_x$overall <- initial_x$dwm_total + initial_x$sdw_total + initial_x$live_total
    
    # Calculate improvement
    x$improvement <- x$overall - initial_x$overall
    
    # Put in terms of millions of bdt
    x$improvement <- x$improvement / 1000000
    x$overall <- x$overall / 1000000
    x$dwm_total <- x$dwm_total / 1000000
    x$sdw_total <- x$sdw_total / 1000000
    x$live_total <- x$live_total / 1000000
    
    x <- x[, -c(5:8)]
    
    for (i in c(2,3,4,6,7)) {
      x[, i] <- round(x[, i], 2)
    }
    if (input$radio_region != 7) x <- x[, -5]
    
    if (input$radio_region != 7) {
      x <- rbind(x, data.frame("county" = "TOTAL", "dwm_total" = sum(x$dwm_total),
                               "sdw_total" = sum(x$sdw_total), 
                               "live_total" = sum(x$live_total),
                               "overall" = sum(x$dwm_total) + sum(x$sdw_total) + sum(x$live_total),
                               "improvement" = sum(x$improvement)))
      names(x) <- c("County", "DWM Total", "SDW Total", "Live Total", 
                    "Overall Carbon", "Improvement")
    } else {
      x <- rbind(x, data.frame("county" = "TOTAL", "dwm_total" = sum(x$dwm_total),
                               "sdw_total" = sum(x$sdw_total), 
                               "live_total" = sum(x$live_total), "fips" = "0",
                               "overall" = sum(x$dwm_total) + sum(x$sdw_total) + sum(x$live_total),
                               "improvement" = sum(x$improvement)))
      names(x) <- c("County", "DWM Total", "SDW Total", "Live Total", "Fips", 
                    "Overall Carbon", "Improvement")
    }
    
    x %>% knitr::kable("html") %>% 
      kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right") %>% 
      add_header_above(c(" " = 2, "Carbon Components (millions BDT)" = 3, "Summary (millions BDT)" = 2))
  }
  
  # Table Showing simulated loss due to fire
  output$PoissonFire <- renderPlot({
    if(input$radio_prior == 1) {
      fire_risk <- 0.005
    } else if (input$radio_prior == 2) {
      fire_risk <- 0.005
    } else if (input$radio_prior == 3) {
      fire_risk <- 0.02
    }
    
    # If you are using prescribed burns
    if (input$radio_burns == 1) fire_risk <- fire_risk * 0.63
    
    # In millions of short dry tons
    initial_value <- 126315550286 / 1000000
    poisson_data <- data.frame('data' = rpois(3000, fire_risk * 1000),
                               'baseline' = rpois(3000, .025 * 1000))
    poisson_data$data <- poisson_data$data * (input$slider_years / 10)
    
    # Figure out number of counties
    if(input$radio_region == 1) {
      ncounties <- 21
    } else if (input$radio_region == 2) {
      ncounties <- 17
    } else if (input$radio_region == 3) {
      ncounties <- 8
    } else if (input$radio_region == 4) {
      ncounties <- 8
    } else if (input$radio_region == 5) {
      ncounties <- 1
    } else if (input$radio_region == 6) {
      ncounties <- 1
    } else {
      ncounties <- 76
    }
    
    poisson_data$data <- poisson_data$data * (ncounties / 76)
    poisson_data$baseline <- poisson_data$baseline * (ncounties / 76)
    
    poisson_data %>% ggplot() + 
      geom_density(aes(x = data, y = stat(count / sum(count))), 
                   color = 'black', fill = "light blue", alpha = .25,
                   bins = 30) +
      geom_density(aes(x = baseline, y = stat(count / sum(count))), 
                   color = 'black', fill = "pink", alpha = .25,
                   bins = 30) +
      geom_vline(xintercept = mean(poisson_data$data), 
                 size = 1, 
                 linetype = 'dashed',
                 color = 'blue') +
      geom_vline(xintercept = mean(poisson_data$baseline), 
                 size = 1, 
                 linetype = 'dashed',
                 color = 'pink') +
      theme_bw() +
      labs(x = paste0('Number of fires per ', input$slider_years, ' years'),
           y = 'Proportion',
           title = 'Poisson Estimated Number of Significant Burns',
           subtitle = 'Red = baseline, blue = improved scenario')
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
