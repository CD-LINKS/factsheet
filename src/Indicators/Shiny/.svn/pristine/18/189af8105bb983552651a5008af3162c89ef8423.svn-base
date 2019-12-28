#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. http://shiny.rstudio.com/
#
# Libraries ---------------------------------------------------------------
library(ggplot2)
library(tools)
library(reshape)
library(reshape2)
library(plyr)
library(dplyr)
library(data.table)
library(knitr)
library(tidyr)
library(colourpicker)
library(shiny)

source('./plot_build.R')
source('./WJlib.R')

# Initialzing global vars ---------------------------------------------
if(dir.exists("../data_files")){datapath="../data_files"}else{datapath="./"}
data_filenames <- list.files(path=datapath, pattern = "\\.csv$|\\.out$|\\.rda$|\\.scn$", ignore.case = TRUE)

ini_filenames <- list.files(path="./settings/", pattern = "\\.ini$")
color_ssp <- c("#00FF00","#0000FF","#FF0000","#FFB900","#FF00FF")
color_decomp <- c("#FFCC00", "#C0504D", "#9BBB59", "#8064A2", "#4BC0C6", "#F79646") #, "#797A7A", "#376092")
color_sspland1 <- c('#b2df8a','#33a02c','#6a3d9a','#fdbf6f','#ff7f00','#1f78b4','#e31a1c')
color_sspland2 <- c('#bebada','#fb8072','#8dd3c7','#80b1d3','#ffffb3')
color_s4n <- c('#C32117','#ff7f00','#4daf4a','#a65628','#377eb8','#984ea3')
#pblallcols <- colorlist_pbl
#pblallcols <- c(hemelblauw3010,hemelblauw3011,hemelblauw3012,hemelblauw3013,hemelblauw3014,hemelblauw3015,hemelblauw3016,hemelblauw3017,hemelblauw3018,hemelblauw3019,mosgroen3020,mosgroen3021,mosgroen3022,mosgroen3023,mosgroen3024,mosgroen3025,mosgroen3026,mosgroen3027,mosgroen3028,mosgroen3029,violet3030,violet3031,violet3032,violet3033,violet3034,violet3035,violet3036,violet3037,violet3038,violet3039,donkergeel3040,donkergeel3041,donkergeel3042,donkergeel3043,donkergeel3044,donkergeel3045,donkergeel3046,donkergeel3047,donkergeel3048,donkergeel3049,paars3050,paars3051,paars3052,paars3053,paars3054,paars3055,paars3056,paars3057,paars3058,paars3059,lichtblauw3060,lichtblauw3061,lichtblauw3062,lichtblauw3063,lichtblauw3064,lichtblauw3065,lichtblauw3066,lichtblauw3067,lichtblauw3068,lichtblauw3069,roze3070,roze3071,roze3072,roze3073,roze3074,roze3075,roze3076,roze3077,roze3078,roze3079,groen3080,groen3081,groen3082,groen3083,groen3084,groen3085,groen3086,groen3087,groen3088,groen3089,rood3090,rood3091,rood3092,rood3093,rood3094,rood3095,rood3096,rood3097,rood3098,rood3099,donkergroen3100,donkergroen3101,donkergroen3102,donkergroen3103,donkergroen3104,donkergroen3105,donkergroen3106,donkergroen3107,donkergroen3108,donkergroen3109,oranje3110,oranje3111,oranje3112,oranje3113,oranje3114,oranje3115,oranje3116,oranje3117,oranje3118,oranje3119,donkerbruin3120,donkerbruin3121,donkerbruin3122,donkerbruin3123,donkerbruin3124,donkerbruin3125,donkerbruin3126,donkerbruin3127,donkerbruin3128,donkerbruin3129,robijnrood3130,robijnrood3131,robijnrood3132,robijnrood3133,robijnrood3134,robijnrood3135,robijnrood3136,robijnrood3137,robijnrood3138,robijnrood3139,bruin3140,bruin3141,bruin3142,bruin3143,bruin3144,bruin3145,bruin3146,bruin3147,bruin3148,bruin3149,mintgroen3150,mintgroen3151,mintgroen3152,mintgroen3153,mintgroen3154,mintgroen3155,mintgroen3156,mintgroen3157,mintgroen3158,mintgroen3159,geel3160,geel3161,geel3162,geel3163,geel3164,geel3165,geel3166,geel3167,geel3168,geel3169,donkerblauw3170,donkerblauw3171,donkerblauw3172,donkerblauw3173,donkerblauw3174,donkerblauw3175,donkerblauw3176,donkerblauw3177,donkerblauw3178,donkerblauw3179,roodgeelgroen3200,roodgeelgroen3201,roodgeelgroen3202,roodgeelgroen3203,roodgeelgroen3204,roodgeelgroen3205,roodgeelgroen3206,roodgeelgroen3207,roodgeelgroen3208,roodgeelgroen3209,roodhemelblauw3300,roodhemelblauw3301,roodhemelblauw3302,roodhemelblauw3303,roodhemelblauw3304,roodhemelblauw3305,roodhemelblauw3306,roodhemelblauw3307,roodhemelblauw3308,roodhemelblauw3309,zwart3400,zwart3401,zwart3402,zwart3403,zwart3404,zwart3405,zwart3406,zwart3407,zwart3408,zwart3409,wit,hemelblauwdonker3610,hemelblauwdonker3611,mosgroendonker3620,mosgroendonker3621,violetdonker3630,violetdonker3631,geeldonker3640,geeldonker3641,groendonker3680,groendonker3681,rooddonker3690,rooddonker3691)
#pblmaincols <- c(hemelblauw3010, mosgroen3020, violet3030, donkergeel3040, paars3050, lichtblauw3060, roze3070, groen3080, rood3090, donkergroen3100, oranje3110, donkerbruin3120, donkerbruin3120, robijnrood3130, bruin3140, mintgroen3150, geel3160, donkerblauw3170, zwart3400)

pblallcols <- unlist(colorlist_pbl, use.names=F)
pblmaincols <- c(colorlist_pbl$hemelblauw3010, colorlist_pbl$mosgroen3020, colorlist_pbl$violet3030, colorlist_pbl$donkergeel3040, colorlist_pbl$paars3050, 
                 colorlist_pbl$lichtblauw3060, colorlist_pbl$roze3070, colorlist_pbl$groen3080, colorlist_pbl$rood3090, colorlist_pbl$donkergroen3100, 
                 colorlist_pbl$oranje3110, colorlist_pbl$donkerbruin3120, colorlist_pbl$donkerbruin3120, colorlist_pbl$robijnrood3130, colorlist_pbl$bruin3140, 
                 colorlist_pbl$mintgroen3150, colorlist_pbl$geel3160, colorlist_pbl$donkerblauw3170, colorlist_pbl$zwart3400)
# Plot presets
plotpresets <- list(None="",
                    tornado_decomp="G1 = G1 + geom_point()"
                    )

plotpresetsdefault <- names(plotpresets)[1]

### UI code  ============
# This main bit 'ui' contains all the frontend bits, defining menu items etc.
# ===

ui <- function(request) {
  
  fluidPage(
  sidebarLayout(
    sidebarPanel(width=3,
      tabsetPanel(id="tabs",
         tabPanel("Main", id="Main",
                  uiOutput("flex_options_chart"),
                  selectInput("themeopt", "select theme:", choices=c("theme_bw", "theme_classic", "theme_dark","theme_gray","theme_light","theme_minimal","theme_void", "Other"), selected="theme_classic"),
                  sliderInput('TextSize', 'Text size adjustment', min=-16, max=20, value=0, step=1, round=0)
          ),
         tabPanel("Titles & Legend",
                  textInput("title", "Title:", value = ""),
                  textInput("xlab", "Title x-axis", value = ""),
                  checkboxInput('xlabrotate', 'Rotate x-axis 90 deg', value = FALSE),
                  textInput("ylab", "Label y-axis", value = ""),
                  checkboxInput("ylabpercent", "y-label as percentage?", value = FALSE),
                  selectInput("LegendPosition", label = "Select position of the legend", choices=c("left", "top", "right", "bottom"), selected = "right")
         ),
        tabPanel("Size and scales",
                 #checkboxInput('summary', 'Summary in line chart?', value=FALSE),
                 numericInput('ChartHeight', 'Chart height (pixels)', min=1, max=10000, value=400),
                 numericInput('ChartWidth', 'Chart widht (pixels)', min=1, max=10000, value=500),
                 checkboxInput('yman', 'Manual y range?', value=FALSE),
                 numericInput("ymin", label = "Minimum y", value = 0),
                 numericInput("ymax", label = "Maximum y", value = 0),
                 checkboxInput('xman', 'Manual x range?', value=FALSE),
                 numericInput("xmin", label = "Minimum x", value = 2010),
                 numericInput("xmax", label = "Maximum x", value = 2050),
                 selectInput("scaling", label = "Scale by", choices=c("None", "Relative", "Absolute")),
                 numericInput("scalingyr", label = "Scale relative to year", value = 2010, step = 1)
        ),
        tabPanel("Colors",
                 selectInput("colour_preset", label = "Select color preset", choices=c("None","PBL","SSP", "Decomp", "SSP land 1", "SSP land 2", "SIM4NEXUS"), selected = "PBL"),
                 selectInput("colour_allow", label = "Allow colors", choices=c("PBL","All"), selected = "PBL"),
                 lapply(1:12, function(i) {
                   colourpicker::colourInput(paste("c",i,sep=""),paste("Pick colour",i), value=pblmaincols[i], palette = 'limited', allowedCols = pblallcols)
                 })
        ),
        tabPanel("Data & settings",
                 fileInput('file1', 'Choose file(s)', multiple = TRUE),
                 selectInput("dataset",label = "Choose Dataset", choices = data_filenames, selected = "Combined_data.csv")
        )
    )),
    mainPanel(
         downloadButton("download_png", "Download higher quality PNG"),
         imageOutput("plot1"),
         tabsetPanel(id="settingstabs",
           tabPanel('Chart',
             column(4, 
                uiOutput("flex_options")
             ),
             column(4,
                uiOutput("flex_options2"),
                checkboxInput("facet_grid", "Facet grid (2d)", value = FALSE),
                numericInput("ncol", label = "Nr. of colums", value = 2, min=1, step=1),
                selectInput("scales", label = "Facet scales", choices=c("free_y", "free_x","free", "fixed"), selected = "free")
             ),
              column(4, 
                 uiOutput("flex_options3")
            )
           ),
          tabPanel("Table", 
                   downloadButton('downloadData', 'Download selected data'),
                   dataTableOutput("mytable")
          ),
          tabPanel("ggplot code",
                   selectInput("preset_code",label = "Preset plot", choices = names(plotpresets), selected = plotpresetsdefault),
                   textAreaInput("plot_text_add", "Add code lines to plot build function:", value="#G1 = G1 + code to add to the plot\n", width='600px', height ='200px'),
                   verbatimTextOutput("coderecycle")
          ),
          selected="Chart"
      )
    )
  )) 
  
}

### Server code =======================
# Here are al the backend things
# ===
server <- function(input, output, session) {
  
  # Reading csv ====
  options(shiny.maxRequestSize=1000*1024^2)
  session$onSessionEnded(stopApp)
  
  uploadstatus <- reactiveValues(
    usefile1 = TRUE
  )
  
  observeEvent(input$dataset, {uploadstatus$usefile1 <- FALSE})
  observeEvent(input$file1, {uploadstatus$usefile1 <- TRUE})
  
  agmip_csv <- reactive({
    req(input)
    
    if (is.null(input$file1) | uploadstatus$usefile1 == FALSE) {
      fname = paste(datapath,input$dataset,sep="/")
      DATA <- my_dataread(fname)
    } else {
      DATA <- my_dataread(input$file1[,4], input$file1[,1])
    }
    
    updateTabsetPanel(session, "settingstabs", selected="Chart")
    
    return(DATA)
  })
  
  # Flexible UI options -----
  output$flex_options <- renderUI({
    
    dyn_taglist <- tagList()
    
    for(i in 1:dim(agmip_csv())[2]) { # looping over column names
      in_name <- colnames(agmip_csv())[i]

      if (is.integer(agmip_csv()[,i])) { #
        in_choices <- c("All",unique(agmip_csv()[,i]))
      } else {
        in_choices <- c("All",levels(agmip_csv()[,i]))
      }
      

      if(length(in_choices)<=23){
        in_selected <- "All" # Default is all unless there are a lot of things
      } else {
        in_selected <- in_choices[1]
      }

      if (in_name %like% "Region"){
        if ("WLD" %in% in_choices) {
          in_selected <- "WLD"
        } else {
          in_selected <- "World"
        }
      }
      if (in_name %like% "Scenario"){in_selected <- levels(agmip_csv()[1,i])[1]}
      if (in_name %like% "Variable" & !("Variable_AgMIP" %in% colnames(agmip_csv()))){in_selected <- levels(agmip_csv()[1,i])[1]} 
      if (in_name %like% "Item"& !("Variable_AgMIP" %in% colnames(agmip_csv()))){in_selected <- levels(agmip_csv()[1,i])[1]}
      
      #mym files read as 'dim#' all variables, usally the last one is total or world so a usefull default. Same for Variables ending with T
      if (in_name %like% "dim"){in_selected <- in_choices[length(in_choices)]}
      if (in_name == "NRT"){in_selected <- in_choices[length(in_choices)]}
      if(is.na(in_selected)){
        in_selected <- in_choices[1]
      }
      dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(in_name, label=in_name, choices = in_choices, selected = in_selected, multiple = TRUE, selectize = TRUE))
    }
    
    dyn_taglist
  })
  
  output$flex_options2 <- renderUI({
    
    dyn_taglist <- tagList()
    dyn_taglist <- tagAppendChildren(dyn_taglist,
                       selectInput("x_var", label = "x_axis:", choices = colnames(agmip_csv()),selected = "Year"),
                       selectInput("y_var", label = "y_axis:", choices = colnames(agmip_csv()), selected = if(!("value" %in% colnames(agmip_csv()))){tail(colnames(agmip_csv()),1)}else{c("value")}),
                       selectInput("facet", label = "Facet by:", choices = c(colnames(agmip_csv()), "None"),selected = "None"),
                       selectInput("facet2", label = "Facets nr2 by:", choices = c(colnames(agmip_csv()), "None"),selected = "None")
    )
    dyn_taglist
  })
  
  output$flex_options3 <- renderUI({
    
    dyn_taglist <- tagList()
    col_options = c("None", colnames(agmip_csv()))
    def_cols = col_options[col_options != "Year" & col_options != "value"]
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('color', 'Color', choices = col_options, selected=def_cols[2]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('linetype', 'Linetype', choices = col_options, selected=def_cols[3]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('shape', 'Shape (for points)', choices = col_options, selected=def_cols[4]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('fill', 'Fill (for bar, etc)', choices = col_options, selected=def_cols[5]))
    
    dyn_taglist
  })
  
  output$flex_options_chart <- renderUI({
    dyn_taglist <- tagList()
    
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput("chart", label = "Choose chart type", choices = c("Line","Point","Bar","Ribbon","Area","Boxplot","linesummary","geom_smooth", "None"), selected = "Line"))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput('size', label = 'Size/thickness', value=1.5, step=0.25))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput("dodgewidth", label = "Unstack width bars", value = 0, step=0.1))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput('alpha', 'Alpha', min=0, max=1, value=1, step=0.05))
    
    dyn_taglist
    })
  
  heightSize <- reactive({
    input$ChartHeight
  })
  
  widthSize = reactive({
    input$ChartWidth
  })
  
  # Various observers 
  # Observer for changes in color input
  observeEvent(input$colour_preset, {
    # Color presets -----
    preset <- input$colour_preset
    
    if(preset=="PBL") {
      col_scale <- pblmaincols[1:12]
    } else if(preset=="Decomp") {
      col_scale <- color_decomp
    } else if(preset=="SSP") {
      col_scale <- color_ssp
    } else if(preset=="SSP land 1") {
      col_scale <- color_sspland1
    } else if(preset=="SSP land 2") {
      col_scale <- color_sspland2
    } else if(preset=="SIM4NEXUS") {
      col_scale <- color_s4n
    }
    
    for (i in 1:length(col_scale)) {
      if(preset=="PBL") {
        updateColourInput(session, paste("c",i,sep=""), value = col_scale[i], palette = 'limited', allowedCols = pblallcols)
      } else {
        updateColourInput(session, paste("c",i,sep=""), value = col_scale[i], palette = 'square')
      }
    } 
  })
  
  observeEvent(input$colour_allow, {
    for (i in 1:12) {
      if(input$colour_allow=="PBL"){
        updateColourInput(session, paste("c",i,sep=""), palette = 'limited', allowedCols = pblallcols)
      } else {
        updateColourInput(session, paste("c",i,sep=""), palette = 'square')
      }
    }
  })
  
  
  # Plot data generation ====
  input_selection <- reactive({
    input_sel <- list()
    for(i in 1:dim(agmip_csv())[2]) { # looping over column names
      in_name <- colnames(agmip_csv())[i]
      input_sel[[in_name]] <- input[[in_name]]
    }
    return(input_sel)
  })
  
  plot_data <- reactive({
    req(input)
    df <- agmip_csv()
    df <- plot_data_wj(df,input_selection(),input$scaling, input$scalingyr)
    return(df)
  })
  
  #Outputs and rendering --------
  output$plot1 <- renderPlot({
    
    req(input)
    if (nrow(plot_data())==0){return(NULL)}

    df <- plot_data()
    G1_text <- plot_build_wj(df, input)
    eval(parse(text=G1_text))
    eval(parse(text=plotpresets[[input$preset_code]]))
    eval(parse(text=input$plot_text_add))
    
    plot(G1)

  }, height=heightSize, width=widthSize)
  
  output$mytable <- renderDataTable({
    plot_data()
  })
  
  output$coderecycle <- renderText({
    codetext <- ""
    codetext <- paste(codetext,"source('./WJlib.R')\n",sep="")
    codetext <- paste(codetext,"source('./plot_build_simple.R')\n",sep="")
    codetext <- paste(codetext,"df <- my_dataread('",datapath,input$dataset,"')\n",sep="")
    codetext <- paste(codetext,"input_sel <- ", paste(deparse(input_selection()), sep="", collapse=""), "\n", sep="")
    codetext <- paste(codetext,"df <- plot_data_wj(df,input_sel,'", input$scaling,"',", input$scalingyr,")\n",sep="")
    codetext <- paste(codetext, plot_build_wj(plot_data(), input), sep="\n")
    codetext <- paste(codetext, plotpresets[[input$preset_code]],
                      gsub("#G1 \\= G1 \\+ code to add to the plot","",input$plot_text_add), sep="\n")
    
    if(input$title==''){
      plotname <- "plot.png"
    } else {
      plotname <- paste(stringconvert(input$title, ss, input), ".png", sep="")
    }
    
    codetext <- paste(codetext, "ggsave('",plotname,"', G1, width=",input$ChartWidth/72,", height=",input$ChartHeight/72,", device='png')\n", sep="")
    
    return(codetext)
  })
  
  output$downloadData <- downloadHandler(
    filename =  'Selected_values.csv',
    content = function(file) {
      write.csv(plot_data(), file, row.names=FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_png <- downloadHandler(
    
    filename = function(){
      plotname <- "plot.png"
    },

    content = function(file) {
      df = plot_data()
      G1_text <- plot_build_wj(df, reactiveValuesToList(input))
      eval(parse(text=G1_text))
      eval(parse(text=plotpresets[[input$preset_code]]))
      eval(parse(text=input$plot_text_add))
      ggsave(file, G1, width=input$ChartWidth/72, height=input$ChartHeight/72, device="png")
    },
    contentType = "image/png" 
  )
  
}
# Run the application  ----
shinyApp(ui = ui, server = server, enableBookmarking = "url")

