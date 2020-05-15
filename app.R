#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# define helper functions
getSheetID<-function(URL){
  sub(pattern="^.*/spreadsheets/d/(.*)/.*$",replacement="\\1",x=URL)}
####
keyURLs<-read.csv("www/keyURLs.csv") #ignore error
keyURLs$gid<-sapply(keyURLs$URL,getSheetID)
keyURLs$dlURL<-sapply(keyURLs$gid,function(x){paste0("https://docs.google.com/spreadsheets/d/",x,"/export?format=csv")})

library(shiny);require(shinydashboard);library(rmarkdown);require(kableExtra)
source("gradingScript.R")

ui <- navbarPage(title = "GoogleFormGrader",
  tabPanel("Setup",
    # Boxes need to be put in a row (or column)
      div(img(src="GPlogo.png",width=300),style="padding:10px"),
      div(style="background-color:#f0f4ff;width:300px;padding:10px 10px 0px 10px; margin:5px; border: 3px solid #3e0055",
        p("This app is designed to help teachers grade and summarize student performance on Galactic Polymath-styled webquests", a("like this one",href="https://docs.google.com/document/d/1CJN5xuUUsLMO481qcUBDshBzkmde0JGhQeEBGsysb8Y/edit?usp=sharing",target="_blank"),".",style="font-weight:500;color:#3e0055;")
        ),
      h3("Step 1:"),
      p("Upload student form responses, downloaded as \"Comma-Separated Values\" format from Google Sheets. Here's an",a("example video tutorial-- but choose CSV, not Excel format.",href="https://www.youtube.com/watch?v=iySTMUYFY9k")),
      fluidRow(style="padding-left:5em",fileInput("responses", "Choose .CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))),
    h3("Step 2:"),
    p("Choose which GP webquest you want to grade or upload your own custom key"),
    fluidRow(style="padding-left:5em",
      selectInput("whichKey","Choose Default Key",choices = list(`Compare Your Air`="airq",`Future option`="future")),selected="airq",p(strong("   OR   ")),
      fileInput("custKey", "Advanced: Choose Custom Key CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
            ),
    h3("Step 3:"),
    verbatimTextOutput("console"),
     h3("Step 4:"),
    actionButton('make',"Make Report")
  ),#End Setup Panel
  
  tabPanel("Preview",
    htmlOutput("preview")
    
    
  ),#End Preview Panel
  
  tabPanel("Export",
  h2("Choose format to export your report"),
  br(),
  textInput("custfn","Enter custom filename", value="Webquest_Grades",placeholder = "Webquest_Grades"),
  radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
   downloadButton('downloadReport')
  )#End Export Panel
  
  
)

server <- function(input, output) {
################## 
### Setup Tab
  vals<-reactiveValues()
  
#Grade once files are in order
 observe({
    req(input$responses)
     vals$studentResponses<-read.csv(input$responses$datapath)
    })
  
observe(
  if(length(input$custKey)==0){
    req(vals$studentResponses)
    keyfile<-file.path(tempdir(),"keyfile.csv")
    download.file(keyURLs$dlURL[match(input$whichKey,keyURLs$keyID)],keyfile)
    vals$key<-read.csv(keyfile,stringsAsFactors =F)
    }else{read.csv(vals$key<-input$custKey$datapath,stringsAsFactors =F)}
 )
  
output$console<-renderText( head(as.matrix(vals$studentResponses)))

#Make Preview Report when button pushed
observeEvent(input$make,{
  vals$gradesheet<-grade(vals$key,vals$studentResponses) 
  
  f<-"preview.html"
  # Render the document
  params=list(gradesheet=vals$gradesheet)
  rmarkdown::render(
    'template.Rmd',
    output_file = f,
    envir = new.env(parent = globalenv()),
    params=params
  )
  
})

  
  
##################  
### Preview tab
prevFileTest<-reactiveFileReader(1000,NULL,"preview.html",file.exists) 

output$preview<- renderUI({
   if(!prevFileTest()){
     h1("Finish all steps in the Setup Tab to generate a preview report.")
   }else{
    includeHTML("preview.html") 
   }
 })
 
################## 
### Export tab
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(input$custfn, sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
     
      # # temporarily switch to the temp dir, in case you do not have write
      # # permission to the current working directory
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      # file.copy(src, 'report.Rmd', overwrite = TRUE)

      out <- render("template.Rmd", output_format=switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ),output_file=file,params=list(gradesheet=vals$gradesheet))
      
    }
  )


  
}



# Run the application 
shinyApp(ui = ui, server = server)
