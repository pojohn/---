library(shiny)
library(car)

shinyUI(fluidPage(
  
  titlePanel("迴歸分析"),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput('data_file', '上傳csv檔',
                accept=c('text/csv', 'text/comma-separated-values',
                         'text/plain', '.csv')),
      
      
      #表單摺疊按鈕
      HTML("<button type='button' class='btn btn-danger' data-toggle='collapse' data-target='#option1'>文件設定</button>"),
      
      #表單可摺疊的部分
      div(id = "option1", class = "collapse in", #默認顯示：class="collapse in"
          #第一行是不是變數
          checkboxInput('header', 'Header', TRUE),
          h6("資料第一行是不是變數名稱",style = "color:darkblue"),
          radioButtons("enc","文字編碼",
                       c(big5 = "BIG5",utf8 = "UTF-8")),
          h6("★Windows中文編碼請選BIG5",br(),"★WLinux/Mac中文編碼請選UTF-8",style = "color:darkblue")
      ),
      
      tags$hr(),
      #表單摺疊按鈕
      HTML("<button type='button' class='btn btn-hg btn-primary' data-toggle='collapse' data-target='#option2'>變數設定</button>"),
      
      #表單可摺疊的部分
      div(id = "option2", class = "collapse", #默認顯示：class="collapse in"
          checkboxGroupInput('r_axis',  
                             label = h3("類別變數"),
                             choices = names(data),
                             selected = ),
          
          selectInput("n1_axis", 
                      label = h4("n1"),
                      choices = "Please Select One",
                      selected = "Please Select One"),
          
          selectInput("n2_axis", 
                      label = h4("n2"),
                      choices = "Please Select One",
                      selected = "Please Select One"),
          # 新變數命名
          textInput("variable", label = h3("新變數命名"), value = "new"),
          #全選按鈕
          actionButton("selectnew","add")
          
      ),
      tags$hr(),
      #表單摺疊按鈕
      HTML("<button type='button' class='btn btn-warning' data-toggle='collapse' data-target='#option3'>反應變數與解釋變數</button>"),
      
      #表單可摺疊的部分
      div(id = "option3", class = "collapse", #默認顯示：class="collapse in"
          #選擇反應變數
          
          selectInput("y_axis", 
                      label = h3("反應變數 response variable(y)"),
                      choices = "Please Select One",
                      selected = "Please Select One"),
          #全選按鈕
          actionButton("selectall","全選"),
          #選擇解釋變數(未完成)
          
          checkboxGroupInput('x_axis',  
                             label = h3("解釋變數 explanatory(x)"),
                             choices = names(data),
                             selected = )
      ),
      tags$hr(),
      
      
      
      
      #回歸方法
      radioButtons("direction","回歸方法 direction",
                   c("both(backward)" = "both.backward" , 
                     "both(forward)" = "both.forward" , "backward" = "backward" , "forward" = "forward")),
      radioButtons("knrow","選擇模型之準則",
                   c(AIC = "AIC",BIC = "BIC"))
    ),
    
    mainPanel(
      #用四個標籤顯示結果
      tabsetPanel( 
        tabPanel("Raw Data And Summary",h4("Summary"),verbatimTextOutput("data_summary"),h4("Raw data"), dataTableOutput('data_table')), 
        tabPanel("Scatter Plot",checkboxInput('reg_line', label = "輔助線 Regression Line", F),
                 plotOutput("scatter_plot", width = "100%", height = "1000px")  ),
        tabPanel("Model Summary",h4("Summary of Coefficients"), tableOutput("lm"),
                 h4("Analysis Of Variance"), tableOutput("anova"),
                 h4("F Test"),verbatimTextOutput("ftest") ),
        tabPanel("Model step Summary", verbatimTextOutput("model_step_summary"),verbatimTextOutput("model_step_call_1")),
        tabPanel("Residual plots",verbatimTextOutput("model_step_call_2"), plotOutput("residual_plots", width = "100%", height = "1000px")),
        type = "pills"
      )
    )
  )
))
