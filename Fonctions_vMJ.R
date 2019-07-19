row <- 6

m <- as.data.frame(row)
m$col <- 1
#m$value = 1.5
map <-matrix(c(6,1:3),ncol=2,nrow=2,byrow=TRUE)



Extract_dataframe <- function(triangle,m){
  
  triangle <- as.data.frame(triangle)
  Extract  <- triangle[triangle$dev==m$col,]
  return(na.omit(Extract))
}

Extract_dataframe(UKMotor,m)

Plot_settings <-function(triangle,m){
  Final    <- Extract_dataframe(triangle,m)
  Final_LR <- Extract_dataframe(ata(triangle),m)
  Final <- Final[-dim(Final)[1],]
  Final$LR <- Final_LR$value
  Final$origin <- as.numeric(Final$origin)
  Final$LR     <- as.numeric(Final$LR)
  return(Final)
}

Plot_settings(UKMotor,m)

Plot_ggplot  <- function(Triangle,map,m){
  # m   : derniere cellule cliquee
  # map : liste des cellules selectionne
  # Triangle doit etre deja bien parametre
  Charges <- Triangle
  if(length(m)==0){
    m<-as.data.frame(cbind(row=0,col=0))
    #Triangle <- Plot_settings(Triangle,m)
  }
  if(length(m)!=0){
    Triangle <- Plot_settings(Triangle,m)
  }
  if(length(map)==0){
    map<-as.data.frame(cbind(V1=1,V2=0))
  }
  
  maptemporary                          <- map[map[,2]==m$col,1]
  # print('maptemporary =')
  # print(maptemporary)
  # print(is.null(maptemporary))
  # print(as.numeric(maptemporary)==0)
  Triangle$PointsSize                <- 3
  Triangle$PointsSize[maptemporary]  <- 4
  Triangle$PointsColor               <- 'conserves'
  Triangle$PointsColor[maptemporary] <- 'elimines'
  Triangle$PointsShape               <- 'disque'
  Triangle$PointsShape[maptemporary] <- 'raye'
  
  previous_mcol<-NULL
   if(length(m)==0){
    previous_mcol<-m$col}
  
  Lignes                                <- dim(Triangle)[1]
  if(length(maptemporary)==0){
    Mean_unselected                       <- mean(Triangle$LR)
  }
  Mean_unselected                       <- sum(Triangle$value*Triangle$LR)/sum(Triangle$value) #mean(Triangle$LR[-maptemporary])
  print('Mean_unselected = ')
  print(Mean_unselected)
  Mean_Claims                           <- Charges[1:Lignes,m$col]
  #Mean_Claims                           <- ifelse(is.null(m$col)||length(m)==0,Charges[1:Lignes,previous_mcol],Charges[1:Lignes,m$col])
  
  print('Mean_Claims1 = ')
  print(Mean_Claims)
   # if(is.na(maptemporary)||length(maptemporary)==0){
   #  Mean_Claims                           <- sum(Mean_Claims*Triangle$LR)/sum(Mean_Claims)
   # }
  Mean_Claims                           <- sum(Mean_Claims[-maptemporary]*Triangle$LR[-maptemporary])/sum(Mean_Claims[-maptemporary])
  
  print('Mean_Claims2 = ')
  print(Mean_Claims)
  
    
  if(length(maptemporary)==0){
    Mean_unselected  <- sum(Triangle$value*Triangle$LR)/sum(Triangle$value) 
  }
  print(maptemporary)
  if(length(maptemporary)==0){
    Triangle$PointsSize                <- 3
    Triangle$PointsColor               <- 'conserves'
    Triangle$PointsShape               <- 'disque'
  }
  if(is.null(maptemporary)){
    Triangle$PointsSize                <- 3
    Triangle$PointsColor               <- 'conserves'
    Triangle$PointsShape               <- 'disque'
  }
  if(length(m)==0){
    Triangle$PointsSize                <- 3
    Triangle$PointsColor               <- 'conserves'
    Triangle$PointsShape               <- 'disque'
  }
  
  if(is.null(m)){
    Triangle$PointsSize                <- 3
    Triangle$PointsColor               <- 'conserves'
    Triangle$PointsShape               <- 'disque'
  }
  
  #Adouci <- as.data.frame(spline(Triangle$origin,Triangle$LR))
  Triangle$Jugement_Expert <- ifelse(Triangle$PointsSize==3,4,2)
  # print(names(Triangle))
  # print(summary(Triangle))
  print(Triangle)
  
  #Triangle$value <- Triangle$value-min(Triangle$value)+1000
  names(Triangle) <- c("Annee", "developpement"     ,    "Montant"    , "Link_ratio"      ,   "PointsSize"  ,"PointsColor" ,"PointsShape" ,"Jugement_Expert")
  
  # print(names(Triangle))
  # print(summary(Triangle))
  # print(Triangle)
  
  ## Rescaling du second axe
  a <-lm(c(min(Triangle$Montant),max(Triangle$Montant))~c(min(Triangle$Link_ratio),max(Triangle$Link_ratio)))$coefficients[2]
  b <-lm(c(min(Triangle$Montant),max(Triangle$Montant))~c(min(Triangle$Link_ratio),max(Triangle$Link_ratio)))$coefficients[1]
   #min(Triangle$Montant)
  #a <- max(Triangle$Montant)
  # b <- max(Triangle$Link_ratio)
  # print(a*Triangle$Link_ratio+b)
  # print(a)
  # print(b)
  
  ay <- list(
    tickfont = list(size=11.7),
    titlefont=list(size=14.6),
    overlaying = "y",
    showline = FALSE,
    side = "right",
    title = "Montants",
    tickcolor='#000',
    position=0.97
  )
  g <- ggplotly(ggplot(Triangle,aes(x=Annee,y=Link_ratio)) + 
                geom_segment(Triangle,mapping=aes(x=Annee,y=min(Triangle$Link_ratio), xend = Annee, yend = (Montant-b)/a,color=PointsColor, alpha= Jugement_Expert),size=6) +
                # geom_point(mapping=aes(x=Annee,y=(Montant-b)*0.5/(a),color=PointsColor,size=Montant,alpha=Jugement_Expert))+
       
                geom_line() +
                geom_point(aes(size=Montant,color=PointsColor,shape=PointsShape,alpha=PointsSize)) + #,guide=FALSE
                geom_abline(slope=0,intercept=Mean_unselected,color='gray',linetype='dashed') +
                geom_abline(slope=0,intercept=Mean_Claims ,color='#f0c300',linetype='dotted') +
                  
                  
                  
    
                  scale_y_continuous('Link ratios', 
                                     sec.axis = sec_axis(~ .*a+b, name = 'Montants'),#)+
                                     limits=c(min(Triangle$Link_ratio),max(Triangle$Link_ratio)))+
                                     
                  scale_x_continuous('Annees de survenance', breaks = Triangle$Annee)+
                  
                  theme_minimal() +#theme(legend.position = "none") +
                  
                  scale_fill_manual("",
                                    labels =c('elimines','conserves'),
                                    values =c('#f0c300','darkred'))+ 
                                     scale_color_manual("Jugement d'expert",
                    labels =c('elimines','conserves'),
                    values =c('#f0c300','darkred')) +

                  
                  
                  scale_shape_manual("",
                                   labels=c('raye','disque'),
                                   values=c(19,4),guide=FALSE)+
                scale_size(guide=FALSE)+
                  theme(legend.position="bottom"),
              tooltip=c('Annee','Link_ratio','intercept','Montant')
                 #ggplotly(,
                  
                  
                  # xlab('Annees de survenance') +
                  # ylab('Link ratios'),
                #tooltip=c('origin','LR','intercept')
     )%>%
    add_lines(x=~Annee, y=~Montant, yaxis="y2",
              data=Triangle, showlegend=FALSE, inherit=FALSE,opacity=0.005) %>% #,labels=Triangle$Link_ratio #, color = I('white')
    layout(yaxis2 = ay,
            legend=list(
            orientation="h", x = 0.4, y = -0.2)
    )
  return(g)
}
Plot_ggplot(UKMotor,map,m)


   ggplot(mtcars, aes(cyl, mpg)) +
    geom_point()+
    geom_segment(aes(x=cyl,y=10, xend=cyl, yend=qsec),color='red')

# Create a simple secondary axis
p + scale_y_continuous(sec.axis = sec_axis(~ . + 4))

Plot_ggplot(UKMotor,map,m)

Plot_highchart <- function(triangle,m){
  triangle <- Plot_settings(triangle,m) 
  h <- highchart() %>%
        hc_title(text = paste("Coefficients de developpement n°",triangle$dev[1]),
                 style = list(fontSize = "20px")) %>% 
        
        hc_xAxis(categories = as.numeric(triangle$origin), title=list(text="Annees") ) %>%
        hc_yAxis_multiples(
          #opposite = TRUE,
          list(title = list(text = paste("Coefficients de developpement"))),
          list(title = list(text = paste("Montants de charge")), opposite = TRUE)) %>% 
        hc_add_series(data = triangle$value, type = "column", yAxis = 1,  name=paste("Montants de charge"),color="#f0c300")%>% 
        hc_add_series(data = triangle$LR, type = "spline", yAxis = 0 ,name=paste("coefficients de developpement"),color="#8B0000")%>%
        #hc_add_series(data = triangle$LR, type = "point", yAxis = 0, size=4)%>%
        hc_tooltip( crosshairs = TRUE,backgroundColor = "#FCFFC5", borderWidth = 2,shared = TRUE) 
  return(h)
  
}

Project_triangle <- function(triangle_charge,vecteur_coef,TailFactor){
  triangle <- triangle_charge
  if(dim(triangle_charge)[1]!=length(vecteur_coef)+1){
    print("Attention - dimensions incompatibles")
  }
  Prod_coef <- rev(vecteur_coef)
  for(i in 2:length(Prod_coef)){
    Prod_coef[i] <- Prod_coef[i-1]*rev(vecteur_coef)[i]
  }
  Prod_coef <- c(1,Prod_coef)
  Charge_Ultime <- rep(0,dim(triangle)[1])
  for(i in 1:dim(triangle)[1]){
    Charge_Ultime[i]<-triangle[i,dim(triangle)[1]-i+1]*Prod_coef[i]
      }
  Charge_Ultime <- Charge_Ultime*TailFactor
  
  vecteur_coef <- c(1,vecteur_coef)
  for(i in 1:dim(triangle)[1]){
    for(j in 1:dim(triangle)[2]){
      if(i+j-1>dim(triangle)[1]){
        triangle[i,j]<-triangle[i,j-1]*vecteur_coef[j]
      }
    }
  }
  
  if(sum(triangle[,dim(triangle)[2]])==sum(Charge_Ultime)){cat('Verification - OK')}
  return(list(triangle, Charge_Ultime))
} 
 



Echantillonnage_residus <-function(residus, positions, R){
  residus <- as.vector(residus)
  residus <- residus[!is.na(residus)]
  Echantillon <- sample(residus, prod(dim(positions)[-3]) * R, 
                    replace = T)
  Echantillon <- array(Echantillon, dim = c(dim(positions)[-3], R))
  Echantillon[is.na(positions)] <- NA
  return(Echantillon)
}

DuplicMatrice<-function (x, d, new.size) {
  n <- length(dim(x))
  perm <- 1:n
  perm[d] <- n
  perm[n] <- d
  x <- aperm(x, perm)
  x <- array(x, dim = c(dim(x)[-n], new.size))
  x <- aperm(x, perm)
  return(x)
}



Nouveau_Triangle_Sup <- function (exp.clms, residus, R) {
  residus <- as.vector(residus)
  residus <- residus[!is.na(residus)]
  EchantillonResidus <- sample(residus, prod(dim(exp.clms)[-3]) * R, 
                        replace = T)
  EchantillonResidus <- array(EchantillonResidus, dim = c(dim(exp.clms)[-3], R))
  EchantillonResidus[is.na(exp.clms)] <- NA
  
  
  n <- length(dim(exp.clms))
  cat('n = ',n, '\n')
  perm <- 1:n
  cat('perm = ',perm,'\n')
  perm[3] <- n
  perm[n] <- 3
  exp.clms <- aperm(exp.clms, perm)
  exp.clms <- array(exp.clms, dim = c(dim(exp.clms)[-n], R))
  exp.clms <- aperm(exp.clms, perm)
  
  
  out <- EchantillonResidus * sqrt(abs(exp.clms)) + exp.clms
  # EchantillonResidus <- Echantillonnage_residus(residus, exp.clms, R)
  # exp.clms <- DuplicMatrice(exp.clms, 3, R)
  # out <- EchantillonResidus * sqrt(abs(exp.clms)) + exp.clms
  return(out)
}




