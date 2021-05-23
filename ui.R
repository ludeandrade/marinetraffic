#### --- UI

ui <- semanticPage(
  theme = "paper",
  grid(mainGrid,
       area_styles = list(mapa = "border: 3px solid transparent",
                          kpi = "border: 3px solid transparent",
                          barra = "border: 3px solid transparent",
                          sidebar = "border: 3px solid transparent"),

       mapa = div(leafletOutput("map")),
       
       kpi = grid(subGridKpi,
                  area_styles = list(
                    left = "border: 5px solid transparent",
                    right = "border: 5px solid transparent"),
                  left = uiOutput("observations"),
                  right = uiOutput("total")),
       
       barra = div(div(class = "inlay", style = "height:30px;width:100%;background-color:transparent"),
                   plotlyOutput("speed")),
       
       sidebar = grid(subGridSidebar,
                      area_styles = list(top = "border: 5px solid transparent; align-items: center;display: flex;flex-direction: row;flex-wrap: wrap;justify-content: center;",
                                         middle = "border: 5px solid transparent",
                                         bottom = "border: 5px solid transparent"),
                      top = div(div(class = "inlay", style = "height:15px;width:100%;background-color:transparent"),
                                header("MARINE TRAFFIC DASHBOARD", description = "Information from AIS Signal"),
                                div(class = "inlay", style = "height:20px;width:100%;background-color:transparent"),
                                div(class = "inlay", style = paste("height:3px;width:100%;background-color:", lightGrey)),
                                div(class = "inlay", style = "height:30px;width:100%;background-color:transparent"),
                                dropDownUI("vessels", c("Select vessel type","Select vessel name")),
                                div(class = "inlay", style = "height:20px;width:100%;background-color:transparent")),
                      
                      middle = div(uiOutput("description"), style = "width:100%"),
                      
                      bottom = div(div(class = "inlay", style = paste("height:3px;width:100%;background-color:", lightGrey)),
                                   div(class = "inlay", style = "height:30px;width:100%;background-color:transparent"),
                                   div(action_button(input_id = "infoButton", label = "INFO", width = "100%"), style = "align-items: center; justify-content: center"),
                                   div(class = "inlay", style = "height:30px;width:100%;background-color:transparent"),
                                   div(h4("Developed by Luciana Linhares for Appsilon.", style = paste0("color:", darkGrey)))
                                   
                      )
       ) # subgrid
  ) # grid
) # semantic page
