library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(dplyr)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(htmltools)
library(fresh)

# Data dummy yang sama
data_lahan <- data.frame(
  Provinsi = rep(c("Kalimantan Timur", "Papua", "Riau", "Sumatra Utara", "Jawa Barat", "Sulawesi Selatan"), each = 10),
  Pulau = rep(c("Kalimantan", "Papua", "Sumatra", "Sumatra", "Jawa", "Sulawesi"), each = 10),
  Tahun = rep(2014:2023, times = 6),
  Luas_Hutan = runif(60, 150000, 250000),
  Luas_Non_Hutan = runif(60, 100000, 200000),
  Kehilangan_Tutupan = runif(60, 1000, 5000),
  Emisi_Karbon = runif(60, 5000, 15000)
)

# Custom theme dengan fresh
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#1e40af",
    blue = "#3b82f6",
    navy = "#1e3a8a",
    green = "#10b981",
    yellow = "#f59e0b",
    red = "#ef4444"
  ),
  adminlte_sidebar(
    dark_bg = "#1f2937",
    dark_hover_bg = "#374151",
    dark_color = "#e5e7eb"
  ),
  adminlte_global(
    content_bg = "#f8fafc"
  )
)

# UI yang lebih modern
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; font-weight: bold;",
      tags$span("🌿", style = "margin-right: 10px; font-size: 24px;"),
      "PELIK Dashboard"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 280,
    tags$div(
      style = "padding: 20px; text-align: center; background: linear-gradient(135deg, #10b981, #3b82f6); margin: -15px -15px 20px -15px;",
      tags$h4("Monitoring Lahan Indonesia", style = "color: white; margin: 0; font-weight: 300;"),
      tags$p("2014 - 2023", style = "color: rgba(255,255,255,0.8); margin: 5px 0 0 0; font-size: 14px;")
    ),
    sidebarMenu(
      menuItem("🗺  Peta Interaktif", tabName = "peta", icon = NULL),
      menuItem("📊  Analisis Data", tabName = "analisis", icon = NULL),
      menuItem("📋  Dataset", tabName = "data", icon = NULL),
      menuItem("ℹ  Tentang Proyek", tabName = "tentang", icon = NULL)
    )
  ),
  
  dashboardBody(
    use_theme(mytheme),
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
        
        * { font-family: 'Inter', sans-serif !important; }
        
        .content-wrapper { background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%); }
        
        .box {
          border-radius: 12px;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
          border: none;
          background: white;
          margin-bottom: 20px;
        }
        
        .box-header {
          background: linear-gradient(135deg, #3b82f6, #1e40af);
          color: white;
          border-radius: 12px 12px 0 0;
          padding: 15px 20px;
        }
        
        .box-title {
          font-weight: 600;
          font-size: 16px;
          margin: 0;
        }
        
        .box-body {
          padding: 20px;
        }
        
        .small-box {
          border-radius: 12px;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
          transition: transform 0.2s;
        }
        
        .small-box:hover {
          transform: translateY(-2px);
        }
        
        .info-box {
          border-radius: 12px;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
          border: none;
          background: white;
        }
        
        .nav-tabs-custom > .nav-tabs {
          border-bottom: 2px solid #3b82f6;
        }
        
        .nav-tabs-custom > .nav-tabs > li.active > a {
          background: #3b82f6;
          color: white;
          border-radius: 8px 8px 0 0;
        }
        
        .form-control, .selectize-input {
          border-radius: 8px;
          border: 1px solid #d1d5db;
          box-shadow: 0 1px 2px 0 rgba(0, 0, 0, 0.05);
        }
        
        .btn {
          border-radius: 8px;
          font-weight: 500;
        }
        
        .main-header .navbar {
          background: linear-gradient(135deg, #10b981, #3b82f6) !important;
        }
        
        .stat-card {
          background: white;
          border-radius: 12px;
          padding: 20px;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
          margin-bottom: 20px;
          transition: transform 0.2s;
        }
        
        .stat-card:hover {
          transform: translateY(-2px);
        }
        
        .stat-number {
          font-size: 28px;
          font-weight: 700;
          color: #1e40af;
          margin: 0;
        }
        
        .stat-label {
          color: #6b7280;
          font-size: 14px;
          margin: 5px 0 0 0;
        }
        
        .leaflet-container {
          border-radius: 8px;
        }
      "))
    ),
    
    tabItems(
      # Tab Peta - Lebih modern
      tabItem(tabName = "peta",
              fluidRow(
                # Info cards di atas
                column(3,
                       div(class = "stat-card",
                           h3("34", class = "stat-number"),
                           p("Total Provinsi", class = "stat-label")
                       )
                ),
                column(3,
                       div(class = "stat-card",
                           h3("2014-2023", class = "stat-number", style = "font-size: 20px;"),
                           p("Periode Data", class = "stat-label")
                       )
                ),
                column(3,
                       div(class = "stat-card",
                           h3("4", class = "stat-number"),
                           p("Indikator Utama", class = "stat-label")
                       )
                ),
                column(3,
                       div(class = "stat-card",
                           h3("6", class = "stat-number"),
                           p("Pulau Besar", class = "stat-label")
                       )
                )
              ),
              
              fluidRow(
                box(width = 8, title = "🗺 Peta Sebaran Data Indonesia", status = "primary", solidHeader = TRUE,
                    leafletOutput("map", height = 500)
                ),
                box(width = 4, title = "⚙ Pengaturan Visualisasi", status = "info", solidHeader = TRUE,
                    div(style = "padding: 10px;",
                        sliderInput("tahun_peta", "📅 Pilih Tahun:", 
                                    min = 2014, max = 2023, value = 2023, step = 1,
                                    animate = animationOptions(interval = 1000)),
                        selectInput("indikator_peta", "📊 Pilih Indikator:",
                                    choices = list(
                                      "🌲 Total Luas Penutupan Hutan (Ha)" = "Luas_Hutan",
                                      "🏞 Total Luas Penutupan Non-Hutan (Ha)" = "Luas_Non_Hutan",
                                      "⚠ Luas Kehilangan Tutupan Pohon (Ha)" = "Kehilangan_Tutupan",
                                      "💨 Estimasi Emisi Karbon (Ton CO₂e)" = "Emisi_Karbon"
                                    )),
                        hr(),
                        h4("🏆 Top 3 Provinsi:", style = "color: #1e40af; font-weight: 600;"),
                        tableOutput("top3_provinsi")
                    )
                )
              )
      ),
      
      # Tab Analisis - Lebih menarik
      tabItem(tabName = "analisis",
              h2("📊 Analisis Mendalam Data Perubahan Lahan", style = "color: #1e40af; font-weight: 600; margin-bottom: 20px;"),
              
              tabsetPanel(type = "tabs",
                          tabPanel("🌍 Tren Regional",
                                   br(),
                                   fluidRow(
                                     box(width = 4, title = "🎛 Kontrol Analisis", status = "primary", solidHeader = TRUE,
                                         selectInput("jenis_data_region", "📈 Jenis Data:",
                                                     choices = list(
                                                       "🌲 Luas Penutupan Lahan Hutan (Ha)" = "Luas_Hutan",
                                                       "🏞 Luas Penutupan Lahan Non-Hutan (Ha)" = "Luas_Non_Hutan"
                                                     )),
                                         selectInput("pulau_region", "🏝 Pilih Pulau:",
                                                     choices = unique(data_lahan$Pulau)),
                                         hr(),
                                         div(style = "background: #f8fafc; padding: 15px; border-radius: 8px;",
                                             h5("💡 Insight:", style = "color: #1e40af; margin-top: 0;"),
                                             p("Grafik menunjukkan tren perubahan lahan dari tahun ke tahun di pulau yang dipilih.", 
                                               style = "font-size: 13px; color: #6b7280; margin: 0;")
                                         )
                                     ),
                                     box(width = 8, title = "📈 Grafik Tren Perubahan", status = "success", solidHeader = TRUE,
                                         plotlyOutput("plot_region", height = 400)
                                     )
                                   )
                          ),
                          
                          tabPanel("🏛 Per Provinsi",
                                   br(),
                                   fluidRow(
                                     box(width = 4, title = "🎯 Pilih Provinsi", status = "primary", solidHeader = TRUE,
                                         selectInput("provinsi_analisis", "🏛 Provinsi:",
                                                     choices = unique(data_lahan$Provinsi)),
                                         hr(),
                                         div(style = "background: #f0fdf4; padding: 15px; border-radius: 8px;",
                                             h5("📋 Keterangan:", style = "color: #10b981; margin-top: 0;"),
                                             p("Membandingkan luas hutan vs non-hutan di provinsi terpilih.", 
                                               style = "font-size: 13px; color: #6b7280; margin: 0;")
                                         )
                                     ),
                                     box(width = 8, title = "📊 Perbandingan Hutan vs Non-Hutan", status = "warning", solidHeader = TRUE,
                                         plotlyOutput("plot_provinsi", height = 400)
                                     )
                                   )
                          ),
                          
                          tabPanel("🔮 Estimasi",
                                   br(),
                                   fluidRow(
                                     box(width = 4, title = "⚡ Parameter Estimasi", status = "primary", solidHeader = TRUE,
                                         selectInput("jenis_data_estimasi", "📊 Jenis Data:",
                                                     choices = list(
                                                       "🌲 Total Luas Penutupan Hutan (Ha)" = "Luas_Hutan",
                                                       "🏞 Total Luas Penutupan Non-Hutan (Ha)" = "Luas_Non_Hutan"
                                                     )),
                                         sliderInput("tahun_estimasi", "📅 Tahun:",
                                                     min = 2014, max = 2023, value = 2023)
                                     ),
                                     box(width = 8, title = "🥧 Visualisasi Proporsi", status = "info", solidHeader = TRUE,
                                         plotlyOutput("plot_estimasi", height = 400)
                                     )
                                   )
                          ),
                          
                          tabPanel("🏆 Ranking Provinsi",
                                   br(),
                                   fluidRow(
                                     box(width = 4, title = "🎮 Pengaturan Ranking", status = "primary", solidHeader = TRUE,
                                         selectInput("jenis_data_top", "🏅 Kategori Ranking:",
                                                     choices = list(
                                                       "🌲 Total Luas Penutupan Hutan (Ha)" = "Luas_Hutan",
                                                       "🏞 Total Luas Penutupan Non-Hutan (Ha)" = "Luas_Non_Hutan",
                                                       "⚠ Luas Kehilangan Tutupan Pohon (Ha)" = "Kehilangan_Tutupan",
                                                       "💨 Estimasi Emisi Karbon (Ton CO₂e)" = "Emisi_Karbon"
                                                     )),
                                         sliderInput("tahun_top", "📅 Tahun Analisis:",
                                                     min = 2014, max = 2023, value = 2023)
                                     ),
                                     box(width = 8, title = "🍰 Top 5 Provinsi", status = "danger", solidHeader = TRUE,
                                         plotlyOutput("plot_pie", height = 400)
                                     )
                                   )
                          )
              )
      ),
      
      # Tab Data - Lebih terstruktur
      tabItem(tabName = "data",
              div(style = "background: white; border-radius: 12px; padding: 30px; margin-bottom: 20px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);",
                  h2("📋 Dataset Perubahan Lahan dan Iklim", style = "color: #1e40af; font-weight: 600; margin-bottom: 20px;"),
                  
                  div(style = "background: linear-gradient(135deg, #eff6ff, #dbeafe); padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                      h4("📖 Deskripsi Dataset", style = "color: #1e40af; margin-top: 0;"),
                      p("Semua data berasal dari hasil olahan dan kompilasi data dari:", style = "margin-bottom: 10px;"),
                      tags$ul(style = "margin-left: 20px;",
                              tags$li("🏛 Kementerian Lingkungan Hidup dan Kehutanan (KLHK)"),
                              tags$li("🌍 Data tutupan lahan Indonesia (2014–2023)"),
                              tags$li("📊 Data estimasi emisi karbon (CO₂e) dari Global Forest Watch"),
                              tags$li("🗺 Badan Informasi Geospasial (BIG)")
                      )
                  ),
                  
                  selectInput("kategori_data", "🔍 Filter Berdasarkan Kategori:",
                              choices = list(
                                "🌲 Total Luas Penutupan Hutan (Ha)" = "Luas_Hutan",
                                "🏞 Total Luas Penutupan Non-Hutan (Ha)" = "Luas_Non_Hutan",
                                "⚠ Luas Kehilangan Tutupan Pohon (Ha)" = "Kehilangan_Tutupan",
                                "💨 Estimasi Emisi Karbon (Ton CO₂e)" = "Emisi_Karbon"
                              )),
                  
                  DT::dataTableOutput("tabel_data")
              )
      ),
      
      # Tab Tentang - Lebih profesional
      tabItem(tabName = "tentang",
              div(style = "max-width: 1000px; margin: 0 auto;",
                  # Header yang menarik
                  div(style = "background: linear-gradient(135deg, #1e40af, #3b82f6); color: white; padding: 40px; border-radius: 12px; margin-bottom: 30px; text-align: center;",
                      h1("🌿 PELIK Dashboard", style = "margin: 0; font-weight: 700; font-size: 32px;"),
                      h3("Perubahan Tutupan Lahan & Estimasi Emisi Karbon di Indonesia", style = "margin: 10px 0 0 0; font-weight: 300; opacity: 0.9;")
                  ),
                  
                  # Cards untuk setiap bagian
                  fluidRow(
                    column(6,
                           div(class = "stat-card",
                               h3("🎯 Deskripsi Singkat", style = "color: #1e40af; font-weight: 600; margin-bottom: 15px;"),
                               p("Dashboard ini menyajikan informasi mengenai perubahan tutupan lahan hutan dan non-hutan di Indonesia serta estimasi emisi karbon akibat kehilangan tutupan pohon. Data dapat dijelajahi berdasarkan pulau, provinsi, dan tahun.", 
                                 style = "line-height: 1.6; color: #4b5563;")
                           )
                    ),
                    column(6,
                           div(class = "stat-card",
                               h3("🚀 Tujuan Proyek", style = "color: #10b981; font-weight: 600; margin-bottom: 15px;"),
                               tags$ul(style = "line-height: 1.8; color: #4b5563;",
                                       tags$li("📊 Menyediakan visualisasi perubahan tutupan lahan"),
                                       tags$li("💨 Menampilkan estimasi emisi karbon berdasarkan kehilangan hutan"),
                                       tags$li("🎯 Memberikan insight berbasis data untuk pengambilan keputusan")
                               )
                           )
                    )
                  ),
                  
                  fluidRow(
                    column(6,
                           div(class = "stat-card",
                               h3("📚 Sumber Data", style = "color: #f59e0b; font-weight: 600; margin-bottom: 15px;"),
                               tags$ul(style = "line-height: 1.8; color: #4b5563;",
                                       tags$li("🏛 Kementerian Lingkungan Hidup dan Kehutanan (KLHK)"),
                                       tags$li("🌍 Global Forest Watch (GFW)"),
                                       tags$li("🗺 Badan Informasi Geospasial (BIG)"),
                                       tags$li("👥 Hasil pengolahan tim mahasiswa")
                               )
                           )
                    ),
                    column(6,
                           div(class = "stat-card",
                               h3("👨‍🎓 Tim Penyusun", style = "color: #8b5cf6; font-weight: 600; margin-bottom: 15px;"),
                               div(style = "line-height: 1.8; color: #4b5563;",
                                   p("👤 Nama Lengkap 1 - NIM: xxxx"),
                                   p("👤 Nama Lengkap 2 - NIM: xxxx"),
                                   p("👤 Nama Lengkap 3 - NIM: xxxx"),
                                   p("👤 Nama Lengkap 4 - NIM: xxxx")
                               )
                           )
                    )
                  ),
                  
                  # Footer
                  div(style = "background: #f8fafc; padding: 30px; border-radius: 12px; text-align: center; margin-top: 30px;",
                      h3("🏫 Politeknik Statistika STIS", style = "color: #1e40af; margin-bottom: 10px;"),
                      p("Program Studi Statistika | Semester 3", style = "color: #6b7280; margin: 0;")
                  )
              )
      )
    )
  )
)

# Server yang sudah ada (sama seperti sebelumnya tapi dengan styling yang diperbaiki)
server <- function(input, output, session) {
  
  # Reactive data untuk peta
  data_peta <- reactive({
    req(input$tahun_peta)
    data_lahan %>%
      filter(Tahun == input$tahun_peta)
  })
  
  # Output peta dengan styling yang lebih baik
  output$map <- renderLeaflet({
    data_filtered <- data_peta()
    
    # Color palette yang lebih menarik
    pal <- colorNumeric(
      palette = c("#fef3c7", "#f59e0b", "#d97706", "#92400e", "#451a03"),
      domain = data_filtered[[input$indikator_peta]]
    )
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addCircleMarkers(
        data = data_filtered,
        lng = runif(nrow(data_filtered), 95, 141), # Random coordinates untuk demo
        lat = runif(nrow(data_filtered), -11, 6),
        radius = ~sqrt(data_filtered[[input$indikator_peta]]/10000),
        fillColor = ~pal(data_filtered[[input$indikator_peta]]),
        color = "white",
        weight = 2,
        opacity = 0.8,
        fillOpacity = 0.7,
        popup = ~paste0(
          "<strong>", Provinsi, "</strong><br>",
          "Pulau: ", Pulau, "<br>",
          "Tahun: ", Tahun, "<br>",
          "Nilai: ", format(data_filtered[[input$indikator_peta]], big.mark = ",", scientific = FALSE), " Ha"
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = data_filtered[[input$indikator_peta]],
        title = "Nilai",
        position = "bottomright"
      )
  })
  
  # Rest of the server code remains the same...
  # (keeping the existing server functions but they'll look better with the new styling)
  
  # Top 3 provinsi dengan styling
  output$top3_provinsi <- renderTable({
    data_peta() %>%
      arrange(desc(.data[[input$indikator_peta]])) %>%
      head(3) %>%
      select(Provinsi, nilai = input$indikator_peta) %>%
      mutate(nilai = format(nilai, big.mark = ",", scientific = FALSE))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Plot functions dengan tema yang konsisten
  output$plot_region <- renderPlotly({
    filtered_data <- data_lahan %>%
      filter(Pulau == input$pulau_region) %>%
      group_by(Tahun) %>%
      summarise(nilai = sum(.data[[input$jenis_data_region]], na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(data = filtered_data, aes(x = Tahun, y = nilai)) +
      geom_line(color = "#3b82f6", size = 1.2) +
      geom_point(color = "#1e40af", size = 3) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      labs(
        title = paste("Tren", input$jenis_data_region, "di", input$pulau_region),
        y = "Luas (Ha)",
        x = "Tahun"
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Sisanya sama seperti kode server sebelumnya...
  # (implementasi lengkap untuk semua output lainnya)
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)