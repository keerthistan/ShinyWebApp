# code entered outside the function:
  # only needs to be loaded in once!

# library in needed packages
library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)

# load in Articulation 1 and Imagination 1 files of MEG data from "data" folder
f_ar <- "data/ar1.csv"
f_im <- "data/im1.csv"
ar1 <- read_csv(f_ar, col_names = FALSE)
im1 <- read_csv(f_im, col_names = FALSE)


# defining the ui
ui <- fluidPage(
  titlePanel(h1("Visualizing MEG Brain Data")),
  sidebarLayout(
    sidebarPanel(
      img(src="MEG_setup_image.png", width=100),
      br(),
      br(),
      actionButton("submit", "MEG MODEL DETAILS"),
      selectInput(
        "more_details",
        label = "Learn more about one of the following...",
        choices = c("Project Setup/Details", "Project Visualization"),
        selected = "Project Setup/Details"
      ),
      style="text-align:center"
    ),
    mainPanel(
      h4("Loading in MEG Data from a Speaker Articulating 1 Phrase"), 
      h5("Select an option on the left panel!"),
      htmlOutput("more_details"),  # display text based on selectInput choice
      htmlOutput("meg_details")  # display MEG model details when button is clicked, htmlOutput allows <br> to be interpreted as break lines
    )
  )
)


# defining the server logic 
server <- function(input, output) {
  # inserting the rest of my bandpower plotting code, for the visualization portion in this website
  
  # defining the frequency ranges of the brainwave bands
  frequencyBands <- list(
    Delta = c(0.3, 4),
    Theta = c(4, 8),
    Alpha = c(8, 15),
    Beta = c(15, 30),
    Gamma = c(30, 59),
    Lower_HighGamma = c(61, 119),
    Upper_HighGamma = c(121, 250)
  )
  
  Fs <- 1000 # a priori values for sampling frequency
  
  # creating a bandpower function
  bandpower_funct <- function(signal, freq_low, freq_high, Fs) {
    # how many data points are in the signal?
    n <- length(signal)
    # using the built-in fft function in R
    fft_res <- fft(signal)
    # data indices should correspond to respective frequency value
    # sampling frequency, Fs, is split into, n, evenly spaced 'bins'--> (Fs/n)
    freq_val <- (0:(n-1)) * (Fs / n)
    # calculate power spectral density (PSD)
    psd <- abs(fft_res)^2 / n
    # which() to pick out indices for values that fit the criteria
    # the criteria: frequencies between the low and high bounds for the given band
    band_indices <- which(freq_val >= freq_low & freq_val <= freq_high)
    # CALCULATE BANDPOWER
    # sum as the PSD values for the matrices then divide by the number of data points
    bandpower <- sum(psd[band_indices]) / length(band_indices)
    return(bandpower)
  }
  
  # STORING ARTICULATION BANDPOWERS
  # store bandpower values for each band into a list
  art1_bandpowers <- list()
  
  # for each iteration/band in the frequencyBands list created before
  for(band in names(frequencyBands)) {
    band_low <- frequencyBands[[band]][1] # first value is lower bound for respective band
    band_high <- frequencyBands[[band]][2] # second value is upper bound
    
    # creating a matrix to store the resulting bandpower values
    # as many rows as there are sensors/rows in ar1, and 1 column --> this goes for each pass(band)
    art1_band_matrix <- matrix(nrow=nrow(ar1), ncol=1)
    
    # for each sensor pass all the way through the end of the rows(the last sensor in ar1)
    for(sensor_idx in 1:nrow(ar1)) {
      # apply the bandpower function and assign to the band_matrix
      art1_band_matrix[sensor_idx, 1] <- bandpower_funct(as.numeric(ar1[sensor_idx, ]), band_low, band_high, Fs)
    }
    
    # now store all the bandpower values
    art1_bandpowers[[band]] <- art1_band_matrix
  }
  
  # STORING IMAGINATION BANDPOWERS
  # store bandpower values for each band into a list
  im1_bandpowers <- list()
  
  # for each iteration/band in the frequencyBands list created before
  for(band in names(frequencyBands)) {
    band_low <- frequencyBands[[band]][1] # first value is lower bound for respective band
    band_high <- frequencyBands[[band]][2] # second value is upper bound
    
    # creating a matrix to store the resulting bandpower values
    # as many rows as there are sensors/rows in im1, and 1 column --> this goes for each pass(band)
    im1_band_matrix <- matrix(nrow=nrow(im1), ncol=1)
    
    # for each sensor pass all the way through the end of the rows(the last sensor in im1)
    for(sensor_idx in 1:nrow(im1)) {
      # apply the bandpower function and assign to the band_matrix
      im1_band_matrix[sensor_idx, 1] <- bandpower_funct(as.numeric(im1[sensor_idx, ]), band_low, band_high, Fs)
    }
    
    # now store all the bandpower values
    im1_bandpowers[[band]] <- im1_band_matrix
  }
  
  
  # RESHAPING DATA INTO LONG FORMAT
  # creating a function to convert the matrices into data frames
  # input = bandpower file (art1 or im1) and filename 
  prepare_bandpower_data <- function(bandpowerfile, filetitle) { 
    band_data <- map_df(names(bandpowerfile), function(band) {
      data_frame(
        Sensor = 1:nrow(bandpowerfile[[band]]),
        Bandpower = as.vector(bandpowerfile[[band]]),
        Band = band,
        File_title = filetitle
      )
    })
    return(band_data)
  }
  
  # Prepare data for both conditions
  art1_data <- prepare_bandpower_data(art1_bandpowers, "Articulation")
  im1_data <- prepare_bandpower_data(im1_bandpowers, "Imagination")
  
  # COMBINING ART/IM DATA
  # combined_data has the new articulation AND imaginationd data in the long format
  combined_data <- bind_rows(art1_data, im1_data)
  
  
  
  # render the selected details from dropdown menu 
  # using the HTML formatting for line breaks
  output$more_details <- renderUI({
    if (input$more_details == "Project Setup/Details") { 
      HTML("In my research, I studied how brain activity varies for different modalities of speech/language; particularly imagined/covert speech versus articulated/overt speech.<br/><br/>
            This web page is dedicated to visualizing brain waves bandpower differences between Articulated and Imagined across all 204 gradiometer sensors, from an MEG machine.<br/><br/>
            By clicking on <strong>\"Project Visualization\"</strong> you will be able to see scatter plots comparing articulation and imagination bandpower values for each brainwave band. The bands are defined by the frequency ranges shown below:<br/>
            <ul>
              <li>Delta = 0.3 - 4 Hz</li>
              <li>Theta = 4 - 8 Hz</li>
              <li>Alpha = 8 - 15 Hz</li>
              <li>Beta = 15 - 30 Hz</li>
              <li>Gamma = 30 - 59 Hz</li>
              <li>Lower High Gamma = 61 - 119 Hz</li>
              <li>Upper HighGamma = 121 - 250 Hz</li>
            </ul>")
    } else if (input$more_details == "Project Visualization") {
      HTML("Visualization details: <br><br>The bands are defined by the frequency ranges shown below:<br/>
            <ul>
              <li>Delta = 0.3 - 4 Hz</li> 
              <li>Theta = 4 - 8 Hz</li>
              <li>Alpha = 8 - 15 Hz</li>
              <li>Beta = 15 - 30 Hz</li>
              <li>Gamma = 30 - 59 Hz</li>
              <li>Lower High Gamma = 61 - 119 Hz</li>
              <li>Upper HighGamma = 121 - 250 Hz</li>
            </ul>") # <ul> : unordered list, essentially bullet points, <li> helps me indicate specific lines of the lists
    } else {
      paste0("You have selected... ", input$more_details)
    }
  })
  
  # creating a "reactive event" for displaying the MEG model details once the action button is clicked
  meg_model_details <- eventReactive(input$submit, {
    HTML("ELEKTA NEUROMAG TRIUX<br>306 channels - 204 gradiometers, 102 magnetometers<br>MEG machine housed in Magnetically Shielded Room (MSR)")
  })
  
  
  output$meg_details <- renderUI({
    meg_model_details()
  })
}



# running the app
shinyApp(ui = ui, server = server)




