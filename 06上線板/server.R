library(shiny)
library(car)
library(xtable)
shinyServer(function(input, output, session) {
  #讀取數據
  readData <- reactive({
    inFile <- input$data_file
    if (is.null(inFile))
      return(iris)
    read.csv(inFile$datapath, header=input$header,fileEncoding =input$enc)
  })
  
  #獲取變量列表
  getVarList <- reactive({
    data = readData()
    names(data)
  })
  
  #動態更新下拉菜單
  observe({
    #選擇類別變數
    updateCheckboxGroupInput(session,"r_axis",
                             choices = getVarList())
    #變數平方項或交乘項
    updateSelectInput(session, "n1_axis",
                      choices = c("Please Select One",getVarList()),
                      selected = "Please Select One")
    updateSelectInput(session, "n2_axis",
                      choices = c("Please Select One",getVarList()),
                      selected = "Please Select One")
    
    #選擇解釋變數
    updateSelectInput(session, "y_axis",
                      choices = c("Please Select One",getVarList()),
                      selected = "Please Select One")
    
  })
  
  #選擇反應變數
  getVarList2 <- reactive({
    data = readData()
    names(data)[-which(names(data)==input$y_axis)]
  })
  observe({
    updateCheckboxGroupInput(session,"x_axis",
                             choices = getVarList2())
  })
  #反應變數全選按鈕
  observe({
    if(is.null(input$selectall))  return()
    
    if(input$selectall%%2 == 0){
      updateCheckboxGroupInput(session ,"x_axis",choices = getVarList2())
    }else{
      updateCheckboxGroupInput(session ,"x_axis",choices = getVarList2(),selected = getVarList2())
    }
  })
  
  
  #對原始資料進行類別的變換(正在修改)
  
  data2<- reactive({
    data = readData()
    if(is.null(input$r_axis)) return(data)
      data.frame(data[-which.names(input$r_axis,names(data))],
               apply(data[input$r_axis], 2 , as.factor)) 
      
  })
  
  
  #反應變數與解釋變數加入
  data3<-reactive({
    data.frame(data2()[input$y_axis],data2()[input$x_axis]  )
  })
  #對原始資料進行類別的變換與加入交乘項(正在修改)
  values <- reactiveValues(variable = NA)
  observe({
    if(input$selectnew > 0) {
      values$variable <- isolate(input$variable)
    }
  })
  ns<-reactive({
    if(input$selectnew > 0)
    if(input$n1_axis !="Please Select One" && input$n2_axis != "Please Select One"){
      data<-data2()
      n1=data[input$n1_axis]
      n2=data[input$n2_axis]
      n3=n1*n2
      names(n3)=values$variable
      data.frame(apply(n3, 2, as.vector) )
    }
  })
  #輸出原始數據
  output$data_table <- renderDataTable({
       data.frame(  data2()  )   
  },options = list(pageLength = 10) )
  #原樣輸出summary信息
  output$data_summary <- renderPrint({
    if(is.null(data)) return(NULL)
    summary(data2())
    
  })
  #繪製散佈圖矩陣
  output$scatter_plot <- renderPlot({
    if(is.null(input$x_axis)) return(NULL)
    if(input$y_axis!="Please Select One" ){
      plot(data3())
      if(input$reg_line){
        scatterplotMatrix(data3(),spread=T)
      }
    }
  })
  #計算model_0跟model_1
  model0 =  reactive({
    if (is.null(input$y_axis))
    {return(NULL)
    } else {
      y = input$y_axis
      eval(substitute(lm(y ~ 1, data=data3()), list(y=as.name(y))))
    }})
  
  model =  reactive({
    if (is.null(input$y_axis) && is.null(input$x_axis))
    {return(NULL)
    } else {
      y = input$y_axis
      eval(substitute(lm(y ~ ., data=data3()), list(y=as.name(y))))
    }})
  #輸出model summary信息
  output$lm <- renderTable({
    if (is.null(input$y_axis) || is.null(input$x_axis))
    {return(NULL)
    } else {model()
    }})
  
  output$anova <- renderTable({
    if (is.null(input$y_axis) || is.null(input$x_axis))
    {return(NULL)
    } else {
      Anova(model() ,type = 3)
    }})
  
  output$ftest <- renderPrint({
    if (is.null(input$y_axis) || is.null(input$x_axis))
    {return(NULL)
    } else {
      Anova(model0(),model(),type = 3)
    }})
  
  #原樣輸出step模型與summary信息
  model_step<-reactive({
    data <- readData()
    if(is.null(input$x_axis)) return(NULL)
    if(input$y_axis!="Please Select One") {
      n=nrow(data3())
      y=input$y_axis
      if(input$knrow == "AIC"){
        r = 2
      } else {
        r = log(n)
      }
      if(input$direction =="both.backward")
        lm.st=step(model(),scope=list(upper = model(), lower = model0()),direction ="both",k = r)
      if(input$direction =="both.forward")
        lm.st=step(model0(),scope=list(upper = model(), lower = model0()),direction ="both",k = r)
      if(input$direction =="backward")
        lm.st=step(model(),scope=list(upper = model(), lower = model0()),direction ="backward",k = r)
      if(input$direction =="forward")
        lm.st=step(model0(),scope=list(upper = model(), lower = model0()),direction ="forward",k = r)
      s=summary(lm.st)
      s
    }
  })
  output$model_step_summary <- renderPrint({
    model_step()
  })
  output$model_step_call_1<- renderPrint({
    msc=model_step()
    cat("最後的模型是")
    msc$call 
  })
  output$model_step_call_2<- renderPrint({
    msc=model_step()
    cat("最後的模型是")
    msc$call 
  })
  #繪製殘差圖
  output$residual_plots <- renderPlot ({
    data <- readData()
    par(mfrow=c(2,2))
    if(is.null(input$x_axis)) return(NULL)
    if(input$y_axis!="Please Select One") {
      n=nrow(data3())
      y=input$y_axis
      if(input$knrow == "AIC"){
        r = 2
      } else {
        r = log(n)
      }
      if(input$direction =="both.backward")
        lm.st=step(model(),scope=list(upper = model(), lower = model0()),direction ="both",k = r)
      if(input$direction =="both.forward")
        lm.st=step(model0(),scope=list(upper = model(), lower = model0()),direction ="both",k = r)
      if(input$direction =="backward")
        lm.st=step(model(),scope=list(upper = model(), lower = model0()),direction ="backward",k = r)
      if(input$direction =="forward")
        lm.st=step(model0(),scope=list(upper = model(), lower = model0()),direction ="forward",k = r)
      plot(lm.st)
    }
  })
})