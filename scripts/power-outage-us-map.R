library(tidyverse)
library(jsonlite)
library(DatawRappr)

api_key <- Sys.getenv("AP_KEY")
us_map <- Sys.getenv("CHART_CODE")
url <- Sys.getenv("URL")

datawrapper_auth(api_key =  api_key, overwrite=TRUE)

## US MAP BY COUNTY

counties <- fromJSON(url)

states <- read_csv("states.csv")

counties <- counties %>% inner_join(states, by="US_State_FIPS")

counties <- counties %>% mutate(percent_out = CustomersOut/CustomersTracked*100)


## current time

et_time <- as.POSIXlt(Sys.time(), tz = "America/New_York")

formatted_time <- format(et_time, "%B %d at %I:%M %p EST")

formatted_time <- sub(" 0", " ", formatted_time)   # day
formatted_time <- sub(" at 0", " at ", formatted_time) # hour

formatted_time <- gsub("AM", "a.m.", formatted_time)
formatted_time <- gsub("PM", "p.m.", formatted_time)


dw_data_to_chart(counties, us_map)

dw_edit_chart(us_map,
              title = paste("Share of homes and businesses without power by county"),
              intro = paste("Click counties to see the percentage of utility customers that don't have power."),
              annotate = paste("Chart updated ", formatted_time,"<br>PowerOutage.com only has gas data for Connecticut's old county boundaries."),
              describe = list(
                "source-name" = "PowerOutage.com",
                "source-url" = "https://poweroutage.us/?_gl=1*1ntcbh5*_gcl_au*MjA2MTA4MTU1Ny4xNzY5MTA5MDM4*_ga*MTMwNjE2MjYyLjE3NjkxMDkwMzg.*_ga_SQFNJ1T8YB*czE3NjkxMDkwMzgkbzEkZzAkdDE3NjkxMDkwMzgkajYwJGwwJGgw",
                "aria-description" = "A choropleth map showing the percentage of people without power."
              ),
              byline = "Annie Jennemann/Get the Facts Data Team",
              data = list(
                "column-format" = list(
                  "US_FULL_FIPS" = list(
                    "type" = "text"
                  )
                )
              ),
              axes = list(
                keys = "US_FULL_FIPS",
                values = "percent_out"),
              visualize = list(
                
                "map-key-attr" = "GEOID",
                colorscale = list(
                  enabled = TRUE,
                  mode = "continuous",
                  stops = "equidistant",
                  interpolation = "equidistant",
                  colors = list(
                    list(color = "#FFDDB6", position = 0),
                    list(color = "#FFC580", position = .125),
                    list(color = "#FFA84E", position = .25),
                    list(color = "#F48E26", position = .375),
                    list(color = "#D6842F", position = .5),
                    list(color = "#AF6116", position = .625),
                    list(color = "#713F01", position = .750),
                    list(color = "#583003", position = .875),
                    list(color = "#3F2104", position = 1)
                  )
                ),
                
                tooltip = list(
                  enabled = TRUE,
                  sticky = TRUE,
                  body = "{{ customerstracked == 0 ? 'No homes or businesses tracked' : CONCAT('<b>',FORMAT(percent_out, '0.[00]%'),'</b> of homes and businesses tracked are without power. <br><br><b>Without power</b>: ',FORMAT(customersout, '0,0.[00]'),'<br><b>Tracked</b>: ',FORMAT(customerstracked, '0,0.[00]')) }}",
                  title = "{{ countyname }}, {{ statename }}"
                  
                ),
                legends = list(
                  color = list(
                    labels = "ruler",
                    labelFormat = "0%",
                    size = "220"
                  )
                )
              )
              
              
)


dw_publish_chart(us_map)

## State by state maps 


counties_states <- counties %>% filter(StateName == "Kansas" | StateName == "Indiana" | StateName == "Iowa" | StateName ==  "California" | StateName == "Nebraska" | StateName == "Arkansas" | StateName == "Missouri" | StateName == "New Mexico" | StateName == "Oklahoma" | StateName == "Mississippi" | StateName == "Maryland" | StateName == "Florida" | StateName == "Massachusetts" | StateName == "Louisiana" | StateName == "Pennsylvania" | StateName == "Wisconsin" | StateName == "Georgia" | StateName == "Kentucky" | StateName == "Ohio" | StateName == "Maine" | StateName == "New Hampshire" | StateName == "Vermont" | StateName == "New York" | StateName == "Alabama" | StateName == "North Carolina" | StateName == "South Carolina")

dw_codes <- read_csv("datawrapper_codes.csv")

counties_states <- counties_states %>% inner_join(dw_codes, by=c("StateName"="state_name"))

my_function <- function(state) {
  
  state_data <- counties_states %>% filter(StateName == state) %>% mutate(max_value = max(CustomersOut))
  
  state_data$max_value[state_data$max_value < 100] <- 100
  
  maxvalue <- state_data$max_value[1]
  
  code <- state_data$code[1]
  
  total_outage <- state_data %>% group_by(StateName) %>% summarise(total = sum(CustomersOut))
  
  sum_out <- total_outage$total[1]
  
  sum_out <- prettyNum(sum_out, big.mark = ",", preserve.width = "none")
  
  dw_data_to_chart(state_data, code)
  
  dw_edit_chart(code,
                title = paste("Number of homes and businesses in ",state," without power"),
                intro = paste0("There are ",sum_out," homes and businesses in ",state," without power. Click counties to see how many utility customers don't have power."),
                annotate = paste("Chart updated ", formatted_time),
                describe = list(
                  "source-name" = "PowerOutage.com",
                  "source-url" = "https://poweroutage.us/?_gl=1*1ntcbh5*_gcl_au*MjA2MTA4MTU1Ny4xNzY5MTA5MDM4*_ga*MTMwNjE2MjYyLjE3NjkxMDkwMzg.*_ga_SQFNJ1T8YB*czE3NjkxMDkwMzgkbzEkZzAkdDE3NjkxMDkwMzgkajYwJGwwJGgw",
                  "aria-description" = "A choropleth map showing the percentage of people without power."
                ),
                byline = "Annie Jennemann/Get the Facts Data Team",
                data = list(
                  "column-format" = list(
                    "US_Full_FIPS" = list(
                      "type" = "text"
                    )
                  )
                ),
                axes = list(
                  keys = "US_Full_FIPS",
                  values = "CustomersOut"),
                visualize = list(
                  basemap = paste0(
                    gsub(" ", "-", tolower(state)),  # replaces spaces with dashes
                    "-counties"
                  ),
                  "map-key-attr" = "GEOID",
                  colorscale = list(
                    enabled = TRUE,
                    mode = "continuous",
                    stops = "equidistant",
                    interpolation = "equidistant",
                    rangeMax = maxvalue,
                    colors = list(
                      list(color = "#FFDDB6", position = 0),
                      list(color = "#FFC580", position = .125),
                      list(color = "#FFA84E", position = .25),
                      list(color = "#F48E26", position = .375),
                      list(color = "#D6842F", position = .5),
                      list(color = "#AF6116", position = .625),
                      list(color = "#713F01", position = .750),
                      list(color = "#583003", position = .875),
                      list(color = "#3F2104", position = 1)
                    )
                  ),
                  
                  tooltip = list(
                    enabled = TRUE,
                    sticky = TRUE,
                    body = "{{ customerstracked == 0 ? 'No homes or businesses tracked' : CONCAT('<b>',FORMAT(percent_out, '0.[00]%'),'</b> of homes and businesses tracked are without power. <br><br><b>Without power</b>: ',FORMAT(customersout, '0,0.[00]'),'<br><b>Tracked</b>: ',FORMAT(customerstracked, '0,0.[00]')) }}",
                    title = "{{ countyname }}, {{ statename }}"
                    
                  ),
                  legends = list(
                    color = list(
                      labels = "ruler",
                      labelFormat = "0,0.[00]",
                      size = "220"
                    )
                  )
                )
                
                
                
                
  )
  
  
  
  dw_publish_chart(code)
  
  
}


for(state in unique(counties_states$StateName)) {
  my_function(state)
}
