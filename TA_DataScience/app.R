# Library 
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(Metrics)
library(rsconnect)
rsconnect::setAccountInfo(name='winlye',
                          token='2E9C7DA12EBEE56ACE16A6A904C6D889',
                          secret='DYlZxs7LsvOcloUriaOzCBIaR3YyT1aDVbCbcHuO')

pdrb_dataset <- read.csv("data/PDRB.csv")
upah_dataset <- read.csv("data/Upah.csv")

drb_clean <- pdrb_dataset %>%
  rename(PDRB = Produk.Domestik.Regional.Bruto.per.Kapita.HB..Rp.) %>%
  filter(!is.na(Provinsi) & !is.na(PDRB)) %>%
  filter(!Provinsi %in% c("Papua Pegunungan", "Papua Barat Daya", "Papua Tengah", "Papua Selatan"))

upah_clean <- upah_dataset %>%
  select(-2, -11) %>%
  filter(complete.cases(.)) %>%
  filter(!Provinsi == "Rata-rata") %>%
  rename(X1 = X0.1, X7 = X7.8.2009, X8 = X.00)

merged_data <- merge(pdrb_clean, upah_clean, by = "Provinsi", all = FALSE)
merged_data$rata_rata_upah <- round(
  rowMeans(merged_data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")], na.rm = TRUE) / 1e6, 1
)

# model regresi linear
model <- lm(rata_rata_upah ~ PDRB, data = merged_data)
merged_data$predicted_upah <- predict(model, merged_data)

# hilangkan outlier
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  data <- data %>%
    filter(data[[column]] >= (Q1 - 1.5 * IQR) & data[[column]] <= (Q3 + 1.5 * IQR))
  return(data)
}

# UI Shiny
ui <- fluidPage(
  titlePanel("Analisis Hubungan antara PDRB Per Kapita dan Rata-Rata Upah Bulanan"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("visual_type", "Pilih Tipe Visualisasi:", 
                  choices = c("Distribusi PDRB", "Distribusi Upah", "Hubungan PDRB & Upah", "Prediksi vs Kenyataan")),
      
      checkboxInput("remove_outliers", "Hilangkan Outlier", FALSE),
      
      conditionalPanel(
        condition = "input.visual_type == 'Distribusi PDRB'",
        sliderInput("pdrb_dist_range", "Rentang PDRB:", 
                    min = floor(min(merged_data$PDRB) / 100000) * 100000, 
                    max = ceiling(max(merged_data$PDRB) / 100000) * 100000, 
                    value = c(floor(min(merged_data$PDRB) / 100000) * 100000, 
                              ceiling(max(merged_data$PDRB) / 100000) * 100000),
                    step = 100000)
      ),
      
      conditionalPanel(
        condition = "input.visual_type == 'Hubungan PDRB & Upah'",
        sliderInput("pdrb_rel_range", "Rentang PDRB:", 
                    min = floor(min(merged_data$PDRB) / 20000) * 20000, 
                    max = ceiling(max(merged_data$PDRB) / 20000) * 20000, 
                    value = c(floor(min(merged_data$PDRB) / 20000) * 20000, 
                              ceiling(max(merged_data$PDRB) / 20000) * 20000),
                    step = 20000)
      ),
      
      hr(),
      h4("Cari Data Provinsi"),
      selectInput("search_provinsi", "Pilih Provinsi:", 
                  choices = c("Semua", unique(merged_data$Provinsi)), selected = "Semua")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Visualisasi", plotOutput("main_plot")),
        tabPanel("Tabel Data", tableOutput("data_summary")),
        tabPanel("Detail Provinsi", tableOutput("provinsi_details"))
      )
    )
  )
)

# Server Shiny
server <- function(input, output) {
  
  # Filter data 
  filtered_data <- reactive({
    data <- merged_data
    if (input$remove_outliers) {
      data <- remove_outliers(data, "rata_rata_upah")
      data <- remove_outliers(data, "PDRB")
    }
    if (input$visual_type == "Distribusi PDRB") {
      data <- data %>%
        filter(PDRB >= input$pdrb_dist_range[1] & PDRB <= input$pdrb_dist_range[2])
    }
    if (input$visual_type == "Hubungan PDRB & Upah") {
      data <- data %>%
        filter(PDRB >= input$pdrb_rel_range[1] & PDRB <= input$pdrb_rel_range[2])
    }
    data
  })
  
  # Data highlight provinsi
  highlight_data <- reactive({
    if (input$search_provinsi == "Semua") {
      return(NULL)
    } else {
      merged_data %>% filter(Provinsi == input$search_provinsi)
    }
  })
  
  output$main_plot <- renderPlot({
    data <- filtered_data()
    highlight <- highlight_data()
    
    if (input$visual_type == "Distribusi PDRB") {
      ggplot(data, aes(x = PDRB, y = Provinsi)) +
        geom_col(fill = "steelblue") +
        geom_col(data = highlight, aes(x = PDRB, y = Provinsi), fill = "red") +
        scale_x_continuous(labels = comma) +
        labs(title = "Distribusi PDRB", x = "PDRB", y = "Provinsi") +
        theme_minimal()
    } else if (input$visual_type == "Distribusi Upah") {
      ggplot(data, aes(x = rata_rata_upah, y = Provinsi)) +
        geom_col(fill = "darkorange") +
        geom_col(data = highlight, aes(x = rata_rata_upah, y = Provinsi), fill = "red") +
        labs(title = "Distribusi Upah", x = "Rata-rata Upah (Juta)", y = "Provinsi") +
        theme_minimal()
    } else if (input$visual_type == "Hubungan PDRB & Upah") {
      ggplot(data, aes(x = PDRB, y = rata_rata_upah)) +
        geom_point(color = "purple") +
        geom_point(data = highlight, aes(x = PDRB, y = rata_rata_upah), color = "red", size = 5) +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        scale_x_continuous(labels = comma) +
        labs(title = "Hubungan antara PDRB dan Rata-rata Upah",
             x = "PDRB", y = "Rata-rata Upah (Juta)") +
        theme_minimal()
    } else if (input$visual_type == "Prediksi vs Kenyataan") {
      ggplot(data, aes(x = predicted_upah, y = rata_rata_upah)) +
        geom_point(color = "darkblue") +
        geom_point(data = highlight, aes(x = predicted_upah, y = rata_rata_upah), color = "red", size = 5) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
        labs(title = "Prediksi vs Kenyataan", x = "Prediksi Upah", y = "Rata-rata Upah") +
        theme_minimal()
    }
  })
  
  # Tabel data
  output$data_summary <- renderTable({
    filtered_data()
  })
  
  # Tabel detail provinsi
  output$provinsi_details <- renderTable({
    highlight_data()
  })
}

# Running
shinyApp(ui, server)

