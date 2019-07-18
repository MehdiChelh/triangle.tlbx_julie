

source("Fonctions_vMJ.R",encoding='UTF-8')
source("BootChain_new_vMJ.R",encoding='UTF-8')
attach(loadNamespace("ChainLadder"), name = "ChainLadder_all")


server <- function(input, output, session) {
  
  
  
  ##########Things that apply generally go here
  Percent_values<-c(0.5,0.75,0.9,0.95,0.995,0.999)
  rownames(MW2008) <-c(2001:2009)
  rownames(MW2014) <-c(2001:2017)
  rownames(GenIns) <-c(2001:2010)
  #note that CL Bootstrap percentile results table column headings will need changing if the above values change
  #Change GenIns cohort and development periods of GenIns so that they are as per reserving book
  
  # GenIns2<-GenIns
  # row.names(GenIns2)<-c(0,1,2,3,4,5,6,7,8,9)
  # colnames(GenIns2)<-c(0,1,2,3,4,5,6,7,8,9,10)
  ##############################################
  colnames(MW2014)<-c(1:17)
  toggleModal(session, "Licensemodal", toggle = "open")
  output$Book1 <-renderImage({
    return(list(
      src = "Book3.png",
      contentType = "image/png",
      alt = "Book"
      #width=179,
      #height=254
    ))
  },deleteFile = FALSE
  )
  
  
  
  
  ##################################################################
  ################Chain Ladder######################################
  ##################################################################
  
  
  
  dataset_CLInput <- reactive({
    switch(input$datasetCL,
           "UKMotor" = UKMotor,
           "MW2008"=MW2008,
           "RAA" = RAA,
           "MW2014"=MW2014,
           "GenIns"=GenIns
           #"Reserving book" = GenIns2,
    )
  })
  
  
  output$ui <- renderUI({
    if (input$color_type==FALSE)
      DT::dataTableOutput('Datatri3')
    
    switch(input$color_type,
           "bar" = DT::dataTableOutput('Datatri2'),
           "gradient" = formattableOutput('Datatri1')
    )
  })
  
  output$Datatri1 <- renderFormattable({ 
    datasetshow           <- as.data.frame(round(as.matrix.data.frame(dataset_CLInput()/as.numeric(input$unitselect))))
    datasetshow[is.na(datasetshow)] <- ''
    rownames(datasetshow) <- rownames(dataset_CLInput())
    formattable(datasetshow,list(area(col = colnames(datasetshow)) ~ color_tile('green','red'))) #%>% #'#f0c300', #'#00bfa5', '#ffd600'  # bleu jaune moutarde <'#40c4ff', '#ffd600'
    
  })
  
  output$Datatri2 <- DT::renderDataTable({ 
    datasetshow           <- as.data.frame(round(as.matrix.data.frame(dataset_CLInput()/as.numeric(input$unitselect))))
    rownames(datasetshow) <- rownames(dataset_CLInput())
    Color<- ifelse(input$Vision_globale_montants,'#f0c300','white')
    datatable(datasetshow,   extensions=c('Buttons'),class = 'stripe compact',selection = 'none',
              options=list(dom='t',
                           scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='_all'))))%>%
      formatStyle(colnames(datasetshow),
                  background = styleColorBar(range(na.omit(datasetshow)), Color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')%>%
      formatStyle(colnames(datasetshow),
                  backgroundColor = styleEqual(NA, c('lightgray')))
  })
  
  output$Datatri3 <- DT::renderDataTable({  
    datasetshow           <- as.data.frame(round(as.matrix.data.frame(dataset_CLInput()/as.numeric(input$unitselect))))
    datasetshow[is.na(datasetshow)] <- ''
    rownames(datasetshow) <- rownames(dataset_CLInput())
    
    datatable(datasetshow,   extensions=c('Buttons'),class = 'stripe compact',selection = 'none',
              options=list(dom='t',buttons=list('copy','print',list(extend='collection',buttons=c('csv','excel','pdf'),text='Download'))
                           ,scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='_all'))))%>%
      formatStyle(colnames(datasetshow),
                  backgroundColor = styleEqual(NA, c('lightgray')))
  })
  
  
  
  
  
  
  
  output$Linkratios1 <- DT::renderDataTable({
    datasetshow <- dataset_CLInput()
    chain1_link<-ata(datasetshow)
    chain1_sum<-summary(chain1_link,digits=3)
    d<-dim(datasetshow)[1]
    link_out<-chain1_sum[1:(d-1),]
    
    datatable(link_out, selection = list(target="cell"),extensions=c('Buttons'),class = 'stripe compact',
              options=list(dom='Bfrtip',
                           scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='cell'),list(targets = 'cell', visible = FALSE)))) %>%
      formatStyle(colnames(link_out),
                  backgroundColor = styleEqual(NA, c('lightgray')))
  })
  
  
  
  
  
  # output$devCoef = renderHighchart({
  #   validate(
  #     need(input$Linkratios1_cell_clicked  != "", "Veuillez double - cliquer sur un coefficient")
  #   )
  #   datasetshow <- dataset_CLInput()
  #   
  #   m <- input$Linkratios1_cell_clicked 
  #   m <- as.data.frame(cbind(m$row,m$col))
  #   names(m)<-c('row','col')
  #   Plot_highchart(datasetshow,m)
  #   
  # })
  
  
  
  output$devCoefChosen = renderPlotly({
    datasetshow<- dataset_CLInput()
    validate(
      need(input$Linkratios1_cell_clicked  != "", "Veuillez double-cliquer sur un coefficient")
    )
    m <- input$Linkratios1_cell_clicked 
    m <- as.data.frame(cbind(m$row,m$col))
    names(m)<-c('row','col')
    map <- input$Linkratios1_cells_selected 
    map <- as.data.frame(map)
    Plot_ggplot(datasetshow,map,m) #dataset_CLInput()
  })
  
  
  
  
  
  
  data <- reactive({
    if(is.null(input$User_Entry)){
      datasetshow           <- dataset_CLInput()
      User_Entry            <- rep('1',dim(datasetshow)[1]-1)
      User_Entry            <- rbind(ata(datasetshow),User_Entry)
      User_Entry            <-  as.data.frame(User_Entry[dim(User_Entry),],stringsAsFactors = FALSE)
      TF <- ifelse(input$TailFactor,FALSE,TRUE)
      TFValue <- tailFactor 
      User_Entry$TailFactor <- ifelse(input$TailFactor,rep(TFValue,2),rep('Indisponible',2))
      
      
      User_Entry            <-  User_Entry[1,]
      
    }
    else {
      User_Entry = input$User_Entry
    }
    
    User_Entry
  })
  
  
  
  observe({
    output$User_Entry <- renderDT({data()},server=TRUE,selection = 'none',editable='cell',
                                  options=list(dom='t',
                                               scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                                               columnDefs=list(list(className='dt-left',targets='_all'))))
    
  })
  
  
  
  link_out       <- reactive({
    m            <- input$Linkratios1_cells_selected
    datasetshow  <- dataset_CLInput()
    d            <- dim(datasetshow)[1]-1
    
    chain1_link  <-ata(datasetshow)
    chain1_sum   <-summary(chain1_link,digits=3)
    
    delta_chosen <-1
    chain1       <-chainladder(datasetshow,weights=1, delta=delta_chosen)
    Poids        <-chain1$weights
    Poids[m]     <- 0 
    
    
    CustomCoef  <- rep(0,d)
    for(i in 1:length(CustomCoef)){
      CustomCoef[i] <-sum(na.omit(datasetshow[1:(d-i+1),i])*na.omit(Poids[1:(d-i+1),i])*na.omit(chain1_link[1:(d-i+1),i]))/sum(na.omit(datasetshow[1:(d-i+1),i])*na.omit(Poids[1:(d-i+1),i]))}
    
    
    
    CustomCoef <- round(CustomCoef,digits=3)
    cat('Customcoef - OK','\n')
    
    for(j in 1:pmin(d,5)){
      # print(j)
      eval(parse(text=paste0('CustomCoef',j,' <- rep(0,d)')))
      eval(parse(text=paste0('for(i in 1:length(CustomCoef',j,')){
                             if(d-i+1-',j,' >= 0){
                             CustomCoef',j,'[i] <-round(sum(na.omit(datasetshow[(d-i+1-',j,'+1):(d-i+1),i])*na.omit(Poids[(d-i+1-',j,'+1):(d-i+1),i])*na.omit(chain1_link[(d-i+1-',j,'+1):(d-i+1),i]))/sum(na.omit(datasetshow[(d-i+1-',j,'+1):(d-i+1),i])*na.omit(Poids[(d-i+1-',j,'+1):(d-i+1),i])),digits=3)
                             }
                             else CustomCoef',j,'[i] <- 1
    }')))
                                      #eval(parse(text=paste0('print(CustomCoef',j,')')))
}
    
    link_out <-rbind(chain1_sum,CustomCoef)
    row.names(link_out)[d+1]  <-"Moyenne simple"
    row.names(link_out)[d+3]  <-"Moyenne retraitÃ©e"
    
    link_out <- link_out[c(d+1, d+3),]
    
    for(j in 1:pmin(d,5)){
      eval(parse(text=paste0('link_out <- rbind(link_out,CustomCoef',j,')')))
    }
    
    LastRow  <- data()
    TempLastRow <- LastRow[,-length(LastRow)]
    # print('DonnÃ©es sur TempLastRow <- LastRow[,-length(LastRow)] ')
    # print(class(TempLastRow))
    # print(dim(TempLastRow))
    # print(length(TempLastRow))
    # print(TempLastRow)
    # 
    link_out <- rbind(link_out,TempLastRow)
    # print('link_out = ')
    # print(link_out)
    
    # Remplacement les valeur Ã©ditÃ©es
    mapUser_Entry <- input$User_Entry_cell_edit
    i             <- dim(link_out)[1]
    j             <- mapUser_Entry$col
    
    # print('mapUser_Entry = ')
    # print(mapUser_Entry)
    
    if(is.null(mapUser_Entry)==FALSE){
      if(dim(link_out)[2]>=j){
        link_out[i,j] <- mapUser_Entry$value
      }
    }
    # print('link_out = ')
    # print(link_out)
    link_out
    
    
})
  
  
  
  
  
  
  
  previousSelection <<- NULL
  # d <<- NULL
  
  # dimension <- reactive({
  #   datasetshow <- dataset_CLInput()
  #   d <- dim(datasetshow)[2]-1
  #   d
  # })
  
  # dimension <<- NULL
  # print('dimension = ')
  # print(dimension)
  
  
  
  
  
  
  observeEvent(input$buttons,{ #i
    isolate({
      previousSelection <<- input$LinkratiosCustom_cells_selected
      
    })
  })
  
  
  observe({ #i
    Mehdi <- input$LinkratiosCustom_cells_selected
  })
  # cat("\n","previousSelection = ")
  # print(previousSelection)
  # 
  # input$LinkratiosCustom_input <- input$LinkratiosCustom_cells_selected
  
  observe({
    datasetshow <<- dataset_CLInput()
    d <- dim(datasetshow)[2]-1
    previousSelection <<- cbind(rep(2,d), c(1:d)) #Axel
    cat("\n","previousSelection dans observe = ", "\n")
    print(previousSelection)
  }, priority =1000)
  
  
  
  
  output$LinkratiosCustom <- DT::renderDataTable({
    link_out   <- link_out()
    # print("previousSelection dans linkratiosCustom = ")
    # print(previousSelection)
    
    
    # cat("\n","dataset_CLInput() = ", "\n")
    # print(datasetshow)
    
    
    # cat("\n","previousSelection = ", "\n")
    # print(previousSelection)
    
    
    
    ######################### A creuser plus tard ############################
    # Actual <- input$LinkratiosCustom_cells_selected
    # 
    # m      <- input$LinkratiosCustom_cell_clicked
    # Reve   <- Mehdi
    # Reve[which(Reve[,2]==m$col),] <- c(m$row,m$col)
    
    # if(!is.null(Mehdi) && xor(is.null(Actual), sum(na.omit(Mehdi == Actual)))){
    #   previous <<- Mehdi
    # }
    # else previous <<- previousSelection
    # 
    # if(is.null(input$LinkratiosCustom_cell_clicked)){
    #   previous <<- previousSelection
    # }
    
    
    DT::datatable(link_out ,
                  #selection=list( target="cell",  selected = cbind(c(1, 3, 4, 5), c(3, 2, 1, 2))), #Axel
                  selection=list( target="cell",  selected = previousSelection ), #Axel
                  extensions=c('Buttons'),class = 'stripe compact',
                  options=list(dom='Bfrtip',
                               scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                               columnDefs=list(list(className='dt-left',targets='cell'),list(targets = 'cell', visible = FALSE))))
  })
  
  
  
  
  
  tailFactor <<- 1
  
  
  observe({ 
    User_Entry    <- data()
    mapUser_Entry <<- input$User_Entry_cell_edit
    i             <- 1
    j             <- mapUser_Entry$col
    
    
    # print('mapUser_Entry$value = ')
    # print(class(User_Entry))
    # print(dim(User_Entry))
    # print(mapUser_Entry)
    # print(dim(User_Entry)[2])
    
    if(input$TailFactor){
      if(is.null(mapUser_Entry)==FALSE){
        if(j==dim(User_Entry)[2]){
          
          tailFactor <<- as.numeric(as.character(mapUser_Entry$value))#as.numeric(as.character()
        }
      }
    }
    
    print("tailFactor = ")
    print(tailFactor)
    
  })
  
  
  # observe({
  #     tailFactor        <- reactive({ 
  #       tailFactorStock <- tailFactorStock()
  #       indice_tailFactorStock <- ifelse(input$TailFactor,1,0)
  #       TF <- indice_tailFactorStock*tailFactorStock + 1*indice_tailFactorStock
  #       TF
  #     })
  #     
  # })
  
  # observe({
  #   if(input$TailFactor==FALSE){
  #     tailFactor   <- 1
  #   }
  # })
  #CoefFinaux <<- NULL
  # observe({
  CoefFinaux                   <- reactive({
    datasetshow              <- dataset_CLInput()
    link_out                 <- link_out()
    
    activateur               <- input$User_Entry_cell_edit
    
    
    
    map                      <- input$LinkratiosCustom_cells_selected
    map                      <- map[order(map[,2]),]
    
    CoefFinaux               <- as.data.frame(matrix(as.numeric(as.character(link_out[map])),ncol=dim(datasetshow)[2], byrow=TRUE))
    colnames(CoefFinaux)     <- c(dimnames(ata(datasetshow))$dev,'TailFactor')
    
    CoefFinaux$TailFactor    <- tailFactor
    
    if(input$TailFactor){
      if(is.null(activateur)==F)
        if(activateur$col==length(CoefFinaux)){
          CoefFinaux$TailFactor    <- as.numeric(as.character(activateur$value))  #link_out[dim(link_out)[1],dim(link_out)[2]]
        }
    }
    
    # if(length(m)==0){
    #   if(m$col==length(CoefFinaux)){
    #     CoefFinaux$TailFactor    <- m$value
    #   }
    # }
    print('activateur =')
    print(activateur)
    print('CoefFinaux =')
    print(CoefFinaux)
    print('taiFactor =')
    print(tailFactor)
    rownames(CoefFinaux)     <- c('Link ratios retenus')
    CoefFinaux 
    
    
    
    
    
    #})
    
  })
  
  output$Retenus <- DT::renderDataTable({
    if(input$TailFactor==FALSE){
      Retenus <- CoefFinaux()[,1:(dim(CoefFinaux())[2]-1)] #CoefFinaux()
    }
    if(input$TailFactor){
      Retenus <- CoefFinaux() #CoefFinaux)()
    }
    Retenus
  },
  selection=list( target="none"),extensions=c('Buttons'),class = 'stripe compact',
  options=list(dom='t',
               scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
               columnDefs=list(list(className='dt-left',targets='cell'),list(targets = 'cell', visible = FALSE))))
  
  
  
  output$CL_triproj <- DT::renderDataTable({
    validate(
      need(input$LinkratiosCustom_cells_selected != "", "Veuillez ne sÃ©lectionner qu'une mÃ©thode par annÃ©e de developpement")
    )
    triangle_charge   <- dataset_CLInput()
    vecteur_coef      <- CoefFinaux() #CoefFinaux()
    
    TF                <- as.numeric(as.character(vecteur_coef[1,dim(vecteur_coef)[2]]))
    
    print('TF=')
    print(TF)
    vecteur_coef      <- c(as.numeric(as.character(vecteur_coef[1,-dim(vecteur_coef)[2]])))
    if(input$TailFactor){
      Resultats       <- Project_triangle(triangle_charge,vecteur_coef,TF)}
    else Resultats       <- Project_triangle(triangle_charge,vecteur_coef,1)
    Demo              <- as.data.frame(round(as.matrix.data.frame(Resultats[[1]]/as.numeric(input$unitselect))))
    rownames(Demo)    <- rownames(triangle_charge)
    Demo 
    datatable(Demo,selection=list( target="none"),extensions=c('Buttons'),class = 'stripe compact',
              options=list(dom='Bfrtip',
                           scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='cell'),list(targets = 'cell', visible = FALSE))))
  })
  
  
  Final   <- reactive({
    triangle_charge    <- dataset_CLInput()
    vecteur_coef       <- CoefFinaux()  #CoefFinaux()
    TF                 <- as.numeric(as.character(vecteur_coef[1,dim(vecteur_coef)[2]]))
    print(TF)
    vecteur_coef       <- c(as.numeric(as.character(vecteur_coef[1,-dim(vecteur_coef)[2]])))
    if(input$TailFactor){
      Resultats        <- Project_triangle(triangle_charge,vecteur_coef,TF)}
    else Resultats     <- Project_triangle(triangle_charge,vecteur_coef,1)
    Ultime_sans_TF     <- as.data.frame(round(as.matrix.data.frame(Project_triangle(triangle_charge,vecteur_coef,1)[[1]]/as.numeric(input$unitselect))))[,dim(Resultats[[1]])[2]]
    Final              <- as.data.frame(round(Resultats[[2]]/as.numeric(input$unitselect)))
    rownames(Final)    <- rownames(triangle_charge)
    colnames(Final)    <- c('Ultime')
    Diagonale          <- c(round(getLatestCumulative(triangle_charge)/as.numeric(input$unitselect)))
    Final              <- cbind(Diagonale, Ultime_sans_TF, Final)
    Final$IBNR_sans_TF <- Final$Ultime_sans_TF - Final$Diagonale
    Final$IBNR         <- Final$Ultime - Final$Diagonale
    colonnes           <- c('Diagonale')
    if(input$d_d){
      Dossier_Dossier<-rep(round(1000/as.numeric(input$unitselect)),dim(Final)[1])
      Final              <- cbind(Dossier_Dossier, Final)
      Final$IBNR_sans_TF <- Final$Ultime_sans_TF - Final$Diagonale -Final$Dossier_Dossier
      Final$IBNR         <- Final$Ultime - Final$Diagonale -Final$Dossier_Dossier
      colonnes           <- c('Diagonale','Dossier_Dossier')}
    Total                <- apply(Final, 2,sum)
    Final                <- rbind(Final,Total) 
    rownames(Final)      <- c(rownames(triangle_charge), "Total")   
    Final
  })
  
  
  output$CL_results1   <- DT::renderDataTable({
    validate(
      need(input$LinkratiosCustom_cells_selected != "", "Veuillez choisir tous vos facteurs de dÃ©veloppement")
    )
    Final <- Final()
    colonnes           <- c('Diagonale')
    if(input$d_d){
      colonnes           <- c('Diagonale','Dossier_Dossier')}
    
    
    datatable(Final,selection=list( target="none"),extensions=c('Buttons'),class = 'stripe compact',
              options=list(dom='Bfrtip',
                           scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='cell'),list(targets = 'cell', visible = FALSE))))%>%
      formatStyle(columns=colonnes,
                  backgroundColor = 'lightblue')
  })
  
  
  
  
  output$Tot_IBNR <- renderValueBox({
    validate(
      need(input$LinkratiosCustom_cells_selected != "", "Choix de coefficients requis")
    )
    
    Final <- Final()
    p<-Final[dim(Final)[1],dim(Final)[2]]
    valueBox(      formatC(p, format="d", big.mark=',')       ,
                   'Total Expected Revenue'      ,
                   #icon = icon("stats",lib='glyphicon'),
                   color = "purple")            
  })
  # output$Tot_IBNR <- renderValueBox({     valueBox(      formatC(1000, format="d", big.mark='.')      ,
  #                                                      'Total Expected Revenue'      ,
  #                                                      icon = icon("gbp",lib='glyphicon')      ,
  #                                                      color = "green")    
  # })
  
  ############################################################
  #############################CL Bootstrap###################
  ############################################################ 
  
  
  
  
  CLBootdataset_chosen <- reactive({
    switch(input$dataset_boot,
           "RAA" = RAA,
           "UKMotor" = UKMotor,
           "MW2008"=MW2008,
           "MW2014"=MW2014,
           "GenIns"=GenIns)
  })
  
  observeEvent(input$InfoCLBoot1, {
    showModal(modalDialog(
      title = "",
      includeMarkdown("CLBoot_documentation.md"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  #Reset inputs button
  observeEvent(input$ResetCLBootinputs, {
    shinyjs::reset("CLBoot_inputpanel")
  })
  
  output$Datatri_CLBoot <- DT::renderDataTable({
    
    if(input$Switch_ChainLadder==FALSE){
      datasetshow <-dataset_CLInput()
    }
    if(input$Switch_ChainLadder){
      datasetshow <- CLBootdataset_chosen() 
    }
    #show the dataset in raw form as in simple R script.
    print(input$Switch_ChainLadder)
    datasetshow <-as.data.frame(round(as.matrix.data.frame(datasetshow/as.numeric(input$unitselect_CLBoot))))
    if(input$Switch_ChainLadder==FALSE){
      rownames(datasetshow) <- rownames(dataset_CLInput())
    }
    if(input$Switch_ChainLadder){
      rownames(datasetshow) <- rownames(CLBootdataset_chosen())
    }
    datatable(datasetshow, selection = 'none',class = 'stripe compact',
              options=list(dom='t',
                           scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='_all'))))%>%
      formatStyle(colnames(datasetshow),
                  backgroundColor = styleEqual(NA, c('lightgray')))
    
    
  })
  ##function to change process dist
  processfunc<-function(value){
    if (value==1) {proc="od.pois"}
    else if (value==2) {proc="gamma"}
  }
  
  ##function to reformat bootstrap results for use in DT:Datatable
  boottable<-function (bootsumry,useunits) {   
    #input is BootChainLadder summary output, units for numbers 
    #produces table with headings latest, Mean ult, Mean Reserve, Total S.D, CV
    #with the final row as Total across all cohorts
    #start with by origins
    
    sboot_origin<-cbind(
      bootsumry$ByOrigin["Latest"]/useunits,
      bootsumry$ByOrigin["Mean Ultimate"]/useunits,
      bootsumry$ByOrigin["Mean IBNR"]/useunits,
      bootsumry$ByOrigin["SD IBNR"]/useunits,
      (bootsumry$ByOrigin["SD IBNR"]/bootsumry$ByOrigin["Mean IBNR"])*100
    )
    colnames(sboot_origin)<-list("Diagonale","Ult' Moyen","IBNR Moyen","Ecart-type IBNR", "CV(%)")
    #now do totals
    sboot_total<-cbind(
      bootsumry$Totals["Latest:",]/useunits,
      bootsumry$Totals["Mean Ultimate:",]/useunits,
      bootsumry$Totals["Mean IBNR",]/useunits,
      bootsumry$Totals["SD IBNR",]/useunits,
      (bootsumry$Totals["SD IBNR",]/bootsumry$Totals["Mean IBNR",])*100
    )
    colnames(sboot_total)<-list("Diagonale","Ult' Moyen","IBNR Moyen","Ecart-type IBNR", "CV(%)")
    row.names(sboot_total)<-list("Total")
    sboot_all<-rbind(sboot_origin,sboot_total) 
    return(sboot_all)
  } 
  
  ##function to reformat bootstrap percentile results for use in DT:Datatable
  bootpercenttable<-function (bootquantile,useunits) {   
    #input is BootChainLadder qauntile output, units for numbers 
    #produces table with headings for reserve at default percentile values plus user-defined value 
    #with the final row as Total across all cohorts
    #start with by origins
    ncolquant<-ncol(bootquantile$ByOrigin)
    finalcolname<-colnames(bootquantile$ByOrigin[ncolquant])
    user_percentile<-substr(finalcolname,6,nchar(finalcolname))
    sbootquant_origin<-cbind(
      bootquantile$ByOrigin["IBNR 50%"]/useunits,
      bootquantile$ByOrigin["IBNR 75%"]/useunits,
      bootquantile$ByOrigin["IBNR 90%"]/useunits,
      bootquantile$ByOrigin["IBNR 95%"]/useunits,
      bootquantile$ByOrigin["IBNR 99.5%"]/useunits,
      bootquantile$ByOrigin["IBNR 99.9%"]/useunits
      ,bootquantile$ByOrigin[ncolquant]/useunits
    )
    
    colnames(sbootquant_origin)<-list("Reserve 50%","Reserve 75%","Reserve 90%","Reserve 95%","Reserve 99.5%","Reserve 99.9%",paste("Reserve @ choix-utilisateur: ",user_percentile))
    #now do totals
    sbootquant_total<-cbind(
      bootquantile$Totals["IBNR 50%",]/useunits,
      bootquantile$Totals["IBNR 75%",]/useunits,
      bootquantile$Totals["IBNR 90%",]/useunits,
      bootquantile$Totals["IBNR 95%",]/useunits,
      bootquantile$Totals["IBNR 99.5%",]/useunits,
      bootquantile$Totals["IBNR 99.9%",]/useunits
      ,bootquantile$Totals[ncolquant,]/useunits
    )
    colnames(sbootquant_total)<-list("Reserve 50%","Reserve 75%","Reserve 90%","Reserve 95%","Reserve 99.5%","Reserve 99.9%",paste("Reserve @ choix-utilisateur: ",user_percentile))
    row.names(sbootquant_total)<-list("Total")
    sbootquant_all<-rbind(sbootquant_origin,sbootquant_total) 
    return(sbootquant_all)
  } 
  
  ###################################################### Calcul des sigmas de Mack #####################################################
  
  MackDT <- reactive({
    if(input$Switch_ChainLadder==FALSE){
      datasetshow<-dataset_CLInput()
    }
    if(input$Switch_ChainLadder){
      datasetshow<-CLBootdataset_chosen()
    }
    Charges  <- datasetshow
    LR       <- ata(datasetshow)
    Residus_2 <- LR[1:dim(LR)[2],]
    vwtd <- summary(LR)[dim(summary(LR))[1],]
    for(j in 1:dim(Residus_2)[2]){
      for(i in 1:dim(Residus_2)[1]){
        Residus_2[i,j] <- (Residus_2[i,j] - vwtd[j])^2
      }
    }
    Charges_residuelles <- Residus_2
    for(j in 1:dim(Charges_residuelles)[2]){
      for(i in 1:dim(Charges_residuelles)[1]){
        Charges_residuelles[i,j] <-  Residus_2[i,j]*Charges[i,j]
      }
    }
    Charges_residuelles <- as.data.frame.matrix(Charges_residuelles)
    Charges_residuelles[is.na(Charges_residuelles) ] <- 0
    SigmaMack <- apply(Charges_residuelles,2,mean)
    SigmaMack <- as.data.frame(t(as.data.frame(round(sqrt(SigmaMack),digits=4))))
    rownames(SigmaMack) <- c('Sigma')
    SigmaMack
  })
  
  output$Mack <-DT::renderDataTable({
    
    datatable(MackDT(), selection = 'none',class = 'stripe compact',
              options=list(dom='Bfrtip',
                           scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='_all'))))
  })
  
  
  output$MackPlot<-renderHighchart({
    MackDTCat <- colnames(MackDT())
    MackDT    <- as.numeric(MackDT())
    
    h <- highchart() %>%
      # hc_title(text = paste("Fonction de rÃ©partition"),
      #          style = list(fontSize = "20px")) %>% 
      
      hc_xAxis(categories = MackDTCat,title=list(text="DÃ©veloppement") ) %>% #categories=boot_run_fdr ,
      hc_yAxis(title = list(text = paste("Distribution des sigmas de Mack"))) %>% 
      hc_add_series(data = MackDT,type = "spline",  name=paste(""), color="#f0c300")%>%  ##8b0000
      hc_tooltip( crosshairs = TRUE,backgroundColor = "white", borderWidth = 2,shared = TRUE)
    h
    
  })
  
  
  ###run bootstrap with 1 simulation just to get residuals
  output$ui_boot <- renderUI({
    if (input$color_type_boot==FALSE)
      DT::dataTableOutput('Bootstrap_residuals3')
    
    switch(input$color_type_boot,
           "bar_boot" = DT::dataTableOutput('Bootstrap_residuals2'),
           "gradient_boot" = formattableOutput('Bootstrap_residuals1')
    )
  })
  
  
  output$RappelCoefCL <- renderDataTable({
    CoefFinaux <- CoefFinaux()      
    if(input$Diffusion_TF==FALSE){
      CoefFinaux[,dim(CoefFinaux)[2]] <- ' Non pris en compte'
    }
    CoefFinaux
    
  },
  selection=list( target="none"),extensions=c('Buttons'),class = 'stripe compact',
  options=list(dom='t',
               scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
               columnDefs=list(list(className='dt-left',targets='cell'),list(targets = 'cell', visible = FALSE))))
  
  
  output$Bootstrap_residuals1<-renderFormattable({
    
    if(input$Switch_ChainLadder==FALSE){
      CoefFinaux <- CoefFinaux()
      
      if(input$Diffusion_TF==FALSE){
        CoefFinaux[,dim(CoefFinaux)[2]] <- 1
      }
      if(input$RecupCoef==TRUE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, TRUE, CoefFinaux)
      }
      if(input$RecupCoef==FALSE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
      }
    }
    if(input$Switch_ChainLadder){
      Initial_boot<-BootChainLadder2_MJ(CLBootdataset_chosen(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE ,FALSE, FALSE)
    }
    resid_out<-as.data.frame(Initial_boot$ChainLadder.Residuals[,,1])
    
    if(input$Switch_ChainLadder==FALSE){
      colnames(resid_out)<-colnames(dataset_CLInput())
      row.names(resid_out)<-row.names(dataset_CLInput()) 
    }
    if(input$Switch_ChainLadder){
      colnames(resid_out)<-colnames(CLBootdataset_chosen())
      row.names(resid_out)<-row.names(CLBootdataset_chosen())
    }
    
    resid_out<-round(resid_out,digits=3)
    resid_out[is.na(resid_out)] <- ''
    formattable(resid_out,list(area(col = colnames(resid_out)) ~ color_tile("green", 'red'))) #%>% #'#f0c300'
    
  })
  
  
  output$Bootstrap_residuals2<-DT::renderDataTable({
    if(input$Switch_ChainLadder==FALSE){
      CoefFinaux <- CoefFinaux()
      if(input$Diffusion_TF==FALSE){
        CoefFinaux[,dim(CoefFinaux)[2]] <- 1
      }
      print('CoefFinaux dans bootstrap')
      print(CoefFinaux)
      
      if(input$RecupCoef==TRUE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, TRUE, CoefFinaux)
      }
      if(input$RecupCoef==FALSE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
      }
    }
    if(input$Switch_ChainLadder){
      Initial_boot<-BootChainLadder2_MJ(CLBootdataset_chosen(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
    }
    
    resid_out<-as.data.frame(Initial_boot$ChainLadder.Residuals[,,1])
    
    if(input$Switch_ChainLadder==FALSE){
      colnames(resid_out)<-colnames(dataset_CLInput())
      row.names(resid_out)<-row.names(dataset_CLInput()) 
    }
    if(input$Switch_ChainLadder){
      colnames(resid_out)<-colnames(CLBootdataset_chosen())
      row.names(resid_out)<-row.names(CLBootdataset_chosen())
    }
    resid_out<-round(resid_out,digits=3)
    
    Color<- ifelse(input$Vision_globale_residus,'#f0c300','white')
    datatable(resid_out,   extensions=c('Buttons'),class = 'stripe compact',selection = 'none',
              options=list(dom='t',
                           scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='_all'))))%>%
      formatStyle(colnames(resid_out),
                  background = styleColorBar(range(na.omit(resid_out)), Color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')%>%
      formatStyle(colnames(resid_out),
                  backgroundColor = styleEqual(NA, c('lightgray')))
  })
  
  output$Bootstrap_residuals3<-DT::renderDataTable({
    if(input$Switch_ChainLadder==FALSE){
      CoefFinaux <- CoefFinaux()      
      if(input$Diffusion_TF==FALSE){
        CoefFinaux[,dim(CoefFinaux)[2]] <- 1
      }
      print('CoefFinaux dans bootstrap')
      print(CoefFinaux)
      
      if(input$RecupCoef==TRUE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, TRUE, CoefFinaux)
      }
      if(input$RecupCoef==FALSE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
      }
    }
    if(input$Switch_ChainLadder){
      Initial_boot<-BootChainLadder2_MJ(CLBootdataset_chosen(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
    }
    resid_out<-as.data.frame(Initial_boot$ChainLadder.Residuals[,,1])
    if(input$Switch_ChainLadder==FALSE){
      colnames(resid_out)<-colnames(dataset_CLInput())
      row.names(resid_out)<-row.names(dataset_CLInput()) 
    }
    if(input$Switch_ChainLadder){
      colnames(resid_out)<-colnames(CLBootdataset_chosen())
      row.names(resid_out)<-row.names(CLBootdataset_chosen())
    }
    
    resid_out<-round(resid_out,digits=3)
    datatable(resid_out,   extensions=c('Buttons'),class = 'stripe compact',selection = 'none',
              options=list(dom='t',buttons=list('Copier','Imprimer',list(extend='collection',buttons=c('csv','excel','pdf'),text='TÃ©lÃ©charger'))
                           ,scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='_all'))))%>%
      formatStyle(colnames(resid_out),
                  backgroundColor = styleEqual(NA, c('lightgray')))
  })
  
  
  output$ui_bootSimple <- renderUI({
    if (input$color_type_boot==FALSE)
      DT::dataTableOutput('Bootstrap_residualsSimple3')
    
    switch(input$color_type_boot,
           "bar_boot" = DT::dataTableOutput('Bootstrap_residualsSimple2'),
           "gradient_boot" = formattableOutput('Bootstrap_residualsSimple1')
    )
  })
  
  
  output$Bootstrap_residualsSimple1<-renderFormattable({
    if(input$Switch_ChainLadder==FALSE){
      CoefFinaux <- CoefFinaux()      
      if(input$Diffusion_TF==FALSE){
        CoefFinaux[,dim(CoefFinaux)[2]] <- 1
      }
      print('CoefFinaux dans bootstrap')
      print(CoefFinaux)
      
      if(input$RecupCoef==TRUE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, TRUE, CoefFinaux)
      }
      if(input$RecupCoef==FALSE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
      }
    }
    if(input$Switch_ChainLadder){
      Initial_boot<-BootChainLadder2_MJ(CLBootdataset_chosen(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
    }
    resid_SimpleOut<-as.data.frame(Initial_boot$Unscaled.Residuals[,,1])
    
    
    
    if(input$Switch_ChainLadder==FALSE){
      colnames(resid_SimpleOut)<-colnames(dataset_CLInput())
      row.names(resid_SimpleOut)<-row.names(dataset_CLInput())
    }
    if(input$Switch_ChainLadder){
      colnames(resid_SimpleOut)<-colnames(CLBootdataset_chosen())
      row.names(resid_SimpleOut)<-row.names(CLBootdataset_chosen())
    }
    
    resid_SimpleOut<-round(resid_SimpleOut,digits=3)
    resid_SimpleOut[is.na(resid_SimpleOut)] <- ''
    formattable(resid_SimpleOut,list(area(col = colnames(resid_SimpleOut)) ~ color_tile("green", 'red'))) #%>% #'#f0c300'
    
  })
  
  
  output$Bootstrap_residualsSimple2<-DT::renderDataTable({
    if(input$Switch_ChainLadder==FALSE){
      CoefFinaux <- CoefFinaux()      
      if(input$Diffusion_TF==FALSE){
        CoefFinaux[,dim(CoefFinaux)[2]] <- 1
      }
      print('CoefFinaux dans bootstrap')
      print(CoefFinaux)
      
      if(input$RecupCoef==TRUE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, TRUE, CoefFinaux)
      }
      if(input$RecupCoef==FALSE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
      }
    }
    if(input$Switch_ChainLadder){
      Initial_boot<-BootChainLadder2_MJ(CLBootdataset_chosen(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
    }
    
    resid_SimpleOut<-as.data.frame(Initial_boot$Unscaled.Residuals[,,1])
    
    
    if(input$Switch_ChainLadder==FALSE){
      colnames(resid_SimpleOut)<-colnames(dataset_CLInput())
      row.names(resid_SimpleOut)<-row.names(dataset_CLInput())
    }
    if(input$Switch_ChainLadder){
      colnames(resid_SimpleOut)<-colnames(CLBootdataset_chosen())
      row.names(resid_SimpleOut)<-row.names(CLBootdataset_chosen())
    }
    
    resid_SimpleOut<-round(resid_SimpleOut,digits=3)
    
    Color<- ifelse(input$Vision_globale_residus,'#f0c300','white')
    datatable(resid_SimpleOut,   extensions=c('Buttons'),class = 'stripe compact',selection = 'none',
              options=list(dom='t',
                           scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='_all'))))%>%
      formatStyle(colnames(resid_SimpleOut),
                  background = styleColorBar(range(na.omit(resid_SimpleOut)), Color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')%>%
      formatStyle(colnames(resid_SimpleOut),
                  backgroundColor = styleEqual(NA, c('lightgray')))
  })
  
  output$Bootstrap_residualsSimple3<-DT::renderDataTable({
    if(input$Switch_ChainLadder==FALSE){
      CoefFinaux <- CoefFinaux()      
      if(input$Diffusion_TF==FALSE){
        CoefFinaux[,dim(CoefFinaux)[2]] <- 1
      }
      print('CoefFinaux dans bootstrap')
      print(CoefFinaux)
      
      if(input$RecupCoef==TRUE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, TRUE, CoefFinaux)
      }
      if(input$RecupCoef==FALSE){
        Initial_boot<-BootChainLadder2_MJ(dataset_CLInput(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
      }
    }
    if(input$Switch_ChainLadder){
      Initial_boot<-BootChainLadder2_MJ(CLBootdataset_chosen(), 1,process.dist=c(processfunc(input$bootprocessdist)),TRUE,TRUE, FALSE, FALSE)
    }
    
    resid_SimpleOut<-as.data.frame(Initial_boot$Unscaled.Residuals[,,1])
    
    
    if(input$Switch_ChainLadder==FALSE){
      colnames(resid_SimpleOut)<-colnames(dataset_CLInput())
      row.names(resid_SimpleOut)<-row.names(dataset_CLInput())
    }
    if(input$Switch_ChainLadder){
      colnames(resid_SimpleOut)<-colnames(CLBootdataset_chosen())
      row.names(resid_SimpleOut)<-row.names(CLBootdataset_chosen())
    }
    resid_SimpleOut<-round(resid_SimpleOut,digits=3)
    datatable(resid_SimpleOut,   extensions=c('Buttons'),class = 'stripe compact',selection = 'none',
              options=list(dom='t',buttons=list('Copier','Imprimer',list(extend='collection',buttons=c('csv','excel','pdf'),text='TÃ©lÃ©charger'))
                           ,scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                           columnDefs=list(list(className='dt-left',targets='_all'))))%>%
      formatStyle(colnames(resid_SimpleOut),
                  backgroundColor = styleEqual(NA, c('lightgray')))
  })
  
  
  #######action button to run bootstrap
  observeEvent(input$Runboot,{
    #now all the code below only runs when input$Runboot changes
    #add user-defined percentile value to list of default percentile values
    
    
    Percent_values<-c(Percent_values,input$Boot_percentile/100)
    
    if (input$seedoption==1) {set.seed(input$CLBootstrap_seed)}
    else if (input$seedoption==0) {set.seed(NULL)}
    
    
    if(input$Switch_ChainLadder==FALSE){
      CoefFinaux <- CoefFinaux()      
      if(input$Diffusion_TF==FALSE){
        CoefFinaux[,dim(CoefFinaux)[2]] <- 1
      }
      print('CoefFinaux dans bootstrap')
      print(CoefFinaux)
      
      if(input$RecupCoef==TRUE){
        First_Bootresults<-BootChainLadder2_MJ(dataset_CLInput(), input$Boot_sims,process.dist=c(processfunc(input$bootprocessdist)),input$Param,TRUE, TRUE, CoefFinaux)
      }
      if(input$RecupCoef==FALSE){
        First_Bootresults<-BootChainLadder2_MJ(dataset_CLInput(), input$Boot_sims,process.dist=c(processfunc(input$bootprocessdist)),input$Param,TRUE, FALSE, FALSE)
      }
    }
    
    
    if(input$Switch_ChainLadder){
      First_Bootresults <- BootChainLadder2_MJ(CLBootdataset_chosen(), input$Boot_sims,process.dist=c(processfunc(input$bootprocessdist)),input$Param,TRUE, FALSE, FALSE)
    }
    #First_Bootresults is a list with items such as Triangle, CL factors, all simulations in an array etc.                                
    if(input$Recentrage){
      Final <- Final()
      First_Bootresults_custom <- First_Bootresults
      
      
      print('ok1')
      
      if(input$sans_TF==1){
        print('ok2')
        if(input$d_d){
          print('ok3')
          deter <- as.vector(Final[1:(dim(Final)[1]-1),5]*as.numeric(as.character(input$unitselect)))
          deter <- array(deter,dim=c(length(deter),1,input$Boot_sims)) 
          print('ok3')
        }      
        if(input$d_d==FALSE){
          print('ok4')
          deter <- as.vector(Final[1:(dim(Final)[1]-1),4]*as.numeric(as.character(input$unitselect)))
          deter <- array(deter,dim=c(length(deter),1,input$Boot_sims)) 
          print('ok4')
        }
        print('ok2')
      }
      if(input$sans_TF==0){
        print('ok2')
        if(input$d_d){
          print('ok3')
          deter <- as.vector(Final[1:(dim(Final)[1]-1),6]*as.numeric(as.character(input$unitselect)))
          deter <- array(deter,dim=c(length(deter),1,input$Boot_sims)) 
          print('ok3')
        }      
        if(input$d_d==FALSE){
          print('ok4')
          deter <- as.vector(Final[1:(dim(Final)[1]-1),5]*as.numeric(as.character(input$unitselect)))
          deter <- array(deter,dim=c(length(deter),1,input$Boot_sims)) 
          print('ok4')
        }
      }
      
    } 
    sto   <- apply(First_Bootresults$IBNR.ByOrigin,1,'mean')
    sto   <- array(sto,dim=c(length(sto),1,input$Boot_sims))
    print('sto Ok')
    
    
    Bootresults               <- First_Bootresults
    
    
    
    if(input$Recentrage){
      print('ok1')
      if(input$methode=='additive'){
        print('ok2')
        Bootresults$IBNR.ByOrigin <- First_Bootresults$IBNR.ByOrigin - sto + deter
        print(dim(First_Bootresults$IBNR.ByOrigin))
        print(dim(deter))
        print(dim(sto))
        print('ok2')
      }
      
      if(input$methode=='multiplicative'){
        Final <- Final()
        TF<- ifelse(input$sans_TF==1,0,1)
        Position <- dim(Final)[2]*TF+(dim(Final)[2]-1)*(1-TF)
        Bootresults$IBNR.ByOrigin   <-  ifelse(sto==0,rep(Final[1,Position],input$Boot_sims),First_Bootresults$IBNR.ByOrigin*deter/sto)
        print('Position sto ==0 ')
        
        print(which(sto==0))
        print('sto dans Recentrage= ')
        # Bootresults$IBNR.ByOrigin  <- First_Bootresults$IBNR.ByOrigin*deter/ifelse(sto!=0,sto,1)
        # Bootresults$IBNR.ByOrigin  <- First_Bootresults$IBNR.ByOrigin*deter/sto
      }
      Bootresults$IBNR.Totals   <- apply(Bootresults$IBNR.ByOrigin,3,sum)   
      
    }
    if(input$Recentrage==FALSE){
      Bootresults$IBNR.ByOrigin <- First_Bootresults$IBNR.ByOrigin 
    } 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$Bootstrap_results<-DT::renderDataTable({
      Bootsumry     <- summary(Bootresults)
      Summarytoshow <- boottable(Bootsumry,as.numeric(input$unitselect_CLBoot))
      Summarytoshow <- round(Summarytoshow,digits=0)
      Summarytoshow
    },
    extensions=c('Buttons'),selection='none',class = 'stripe compact',
    options=list(dom='Bfrtip',
                 scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                 columnDefs=list(list(className='dt-right',targets='_all'))))
    
    
    
    
    
    
    
    output$Bootstrap_graphs1<-renderPlotly({
      boot_run<-Bootresults
      # print('boot_run = ')
      # print(boot_run$IBNR.Totals==apply(boot_run$IBNR.ByOrigin,3,sum))
      # print(table(boot_run$IBNR.Totals==apply(boot_run$IBNR.ByOrigin,3,sum)))
      
      
      #
      
      boot_run_10000  <- as.data.frame(floor(boot_run$IBNR.Totals/input$amp)*input$amp)
      Final <- Final()
      
      
      sto   <- mean(boot_run$IBNR.Totals)
      print('sto =')
      print(sto)
      print('sto printed')
      
      
      if(input$Recentrage){
        deter <- Final[dim(Final)[1],dim(Final)[2]]*as.numeric(as.character(input$unitselect))
        if(input$sans_TF==1){
          if(input$d_d){
            deter <- Final[dim(Final)[1],5]*as.numeric(as.character(input$unitselect))
          }      
          if(input$d_d==FALSE){
            deter <- Final[dim(Final)[1],4]*as.numeric(as.character(input$unitselect))
          }
        }
        if(input$sans_TF==0){
          if(input$d_d){
            deter <- Final[dim(Final)[1],6]*as.numeric(as.character(input$unitselect))
          }      
          if(input$d_d==FALSE){
            deter <- Final[dim(Final)[1],5]*as.numeric(as.character(input$unitselect))
          }
        }
        
      }
      if(input$Recentrage==FALSE){
        deter <- sto 
      }
      
      names(boot_run_10000)        <- c("Montants_IBNR")
      
      h <- ggplotly(
        ggplot(boot_run_10000) +
          geom_bar(aes(x=Montants_IBNR),color="#f2f3f4",fill="#f0c300")+
          #geom_line(aes(x=Montants_IBNR),color="#f0c300")+
          geom_vline(aes(xintercept = sto,colour='stochastique'))+
          geom_vline(aes(xintercept = deter,colour=ifelse(input$Recentrage,'deterministe','stochastique')))+
          theme_minimal()+
          #ggtitle('Histogramme de simulations de charges')+
          xlab("Montants d'IBNR")+
          ylab('Effectif dans les simulations')+
          scale_colour_manual("Moyennes", 
                              values = c(stochastique="red",deterministe="lightblue")) 
      )
      h
      
      
    })
    
    
    output$Bootstrap_graphs2<-renderHighchart({
      boot_run     <- Bootresults
      boot_run_fdr <- table(floor(boot_run$IBNR.Totals/input$amp)*input$amp)
      boot_run_fdr <- as.data.frame(cumsum(boot_run_fdr)/sum(table(floor(boot_run$IBNR.Totals/input$amp)*input$amp)))
      boot_run_fdr <- cbind(rownames(boot_run_fdr),boot_run_fdr)
      names(boot_run_fdr) <- c('Quantiles','Fdr')
      
      h <- highchart() %>%
        # hc_title(text = paste("Fonction de rÃ©partition"),
        #          style = list(fontSize = "20px")) %>% 
        
        hc_xAxis(categories = boot_run_fdr$Quantiles,title=list(text="Quantiles") ) %>% #categories=boot_run_fdr ,
        hc_yAxis(title = list(text = paste("Fdr()"))) %>% 
        hc_add_series(data = boot_run_fdr$Fdr,type = "spline",  name=paste("RÃ©partition des IBNR"), color="#f0c300")%>%  ##8b0000
        hc_tooltip( crosshairs = TRUE,backgroundColor = "white", borderWidth = 2,shared = TRUE)
      h
      
    })
    
    
    output$Bootstrap_graphs3<-renderPlot({
      boot_run<-Bootresults
      plot(boot_run,which=3)
      print(boot_run)
    })
    
    
    output$Bootstrap_graphs4<-renderPlot({
      boot_run<-Bootresults
      plot(boot_run,which=4)
      
    })
    
    
    output$Bootstrap_percentiles<-DT::renderDataTable({
      quantiles_run<-quantile(Bootresults,probs=Percent_values)
      percentiles_toshow<-bootpercenttable(quantiles_run,as.numeric(input$unitselect_CLBoot))
      percentiles_toshow<-round(percentiles_toshow,digits=0)
      percentiles_toshow
    },
    extensions=c('Buttons'),selection='none',class = 'stripe compact',
    options=list(dom='Bfrtip',buttons=list('copy','print',list(extend='collection',buttons=c('csv','excel','pdf'),text='Download'))
                 ,scrollY=TRUE,scrollX=TRUE,ordering=FALSE,paging=FALSE,searching=FALSE,info=FALSE,
                 columnDefs=list(list(className='dt-right',targets='_all'))))
    
    
  }) # Reactive ends
  
  
  
  
  
  
  
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  
}      #end the server stuff

