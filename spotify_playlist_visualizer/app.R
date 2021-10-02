readRenviron(".Renviron")
library(shiny)
library(spotifyr)
library(lubridate)
library(tidyverse)
library(plotly)

# list of vars that can be selected
vars_list <- list(
    "Acousticness", "Added At", "Added By", "Danceability",
    "Energy", "Instrumentalness", "Liveness", "Popularity", "Speechiness",
    "Tempo", "Valence", "Duration (Sec)"
)

# Define UI
ui <- fluidPage(
    
    tabsetPanel(
    tabPanel(
        "Visualizer",
    # Application title
    titlePanel("Spotify Playlist Visualizer"),

    textInput("uri", "Input Playlist ID (At the end of playlist URL): "),
    actionButton("submit", "Submit"),

    
    sidebarLayout(
        sidebarPanel(
            selectInput("x", "X-axis:", vars_list),
            selectInput("y", "Y-axis:", vars_list),
            selectInput("color", "Color:", c("None", vars_list)),
            checkboxInput("smooth", "Trendline", value = FALSE)
        ),
        
        mainPanel(plotlyOutput("plot"))
    )),
    tabPanel("About",
             tags$h1("About"),
             "Created by Ryan Moerer using the",
             tags$a(href="https://github.com/charlie86/spotifyr", "spotifyr"),
             HTML("r package and the Spotify API.<br>"),
             "Code:",
             tags$a(href="https://github.com/rmoerer/spotify-playlist-visualizer", "https://github.com/rmoerer/spotify-playlist-visualizer")
             
    )
    )
    )


# Define server logic
server <- function(input, output) {
    
    # Get playlist data, reactive to user input
    playlist_data <- eventReactive(input$submit, {
        audio_features <- get_playlist_audio_features(playlist_uris = input$uri) %>%
            filter(!is.null(track.artists)) %>%
            group_by(added_by.id) %>%
            mutate(
                Artist = map_chr(track.artists, function(x) x$name[1]), 
                added_at = lubridate::as_datetime(added_at),
                num_obs = n(),
                track.explicit = ifelse(track.explicit == TRUE, 1, 0),
                track.duration = track.duration_ms/1000
            ) %>%
            rename(
                Acousticness = acousticness,
                `Added At` = added_at,
                `Added By` = added_by.id,
                `Danceability` = danceability,
                `Energy` = energy,
                Instrumentalness = instrumentalness,
                Liveness = liveness,
                Popularity = track.popularity,
                Speechiness = speechiness,
                Tempo = tempo,
                Valence = valence,
                `Duration (Sec)` = track.duration
            )
    })
    
    # Render plotly plot
    output$plot <- renderPlotly({
        # playlist data
        data <- playlist_data()
        
        # Set points color if color is selected
        if (input$color == "None") {
            base_plot <- data %>%
                ggplot()   
        } else {
            base_plot <- data %>%
                ggplot(aes(color = get(input$color))) # aes_string() doesn't seem to work with
            base_plot$labels$colour <- input$color    # col names with spaces so use get() instead
        }
        
        # if Added By is one of the selected axes, jitter points
        if (input$x == "Added By" | input$y == "Added By") {
            plot <- base_plot +
                geom_jitter(aes(x = get(input$x), y = get(input$y),
                               text = paste("Track: ", track.name, "\n",
                                            "Artist: ", Artist, "\n",
                                            "Album: ", track.album.name, "\n",
                                            input$x, ": ", get(input$x), "\n",
                                            input$y, ": ", get(input$y), "\n",
                                            sep="")),
                            size=2,
                            alpha=0.8) +
                labs(x = input$x, y = input$y,
                     title = data$playlist_name[1]) +
                theme_minimal() 
        } else { # else, render regular scatter
            plot <- base_plot +
                geom_point(aes(x = get(input$x), y = get(input$y),
                               text = paste("Track: ", track.name, "\n",
                                            "Artist", Artist, "\n",
                                            "Album: ", track.album.name, "\n",
                                            "Added By: ", `Added By`, "\n",
                                            input$x, ": ", get(input$x), "\n",
                                            input$y, ": ", get(input$y), "\n",
                                            sep="")),
                           size = 2,
                           alpha = 0.8) +
                labs(x = input$x, y = input$y,
                     title = data$playlist_name[1]) +
                theme_minimal()
            # if trendline is checked add smooth
            if (input$smooth == TRUE) {
                plot <- plot +
                    geom_smooth(aes(x = get(input$x), y = get(input$y), color=NULL), se = FALSE)
            }
        }
        
        ggplotly(plot, tooltip = "text")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
