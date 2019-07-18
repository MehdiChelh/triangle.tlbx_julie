 #scatterplot3d - not always needed

install.packages("shiny")
install.packages("shinydashboard")
install.packages("ChainLadder")
install.packages("markdown")
install.packages("copula")  #load the copula package
install.packages("distr")  #distribution install.packages
install.packages("scatterplot3d") 
install.packages("matrixcalc")
# install.packages("actuar")  # removed this as seemed to cause problem with ChainLadder - not sure why, as worked before!
install.packages("shinythemes")
install.packages("shinyLP")
install.packages("shinyBS")
install.packages("shinyjs")
install.packages("scales") # used for adding commas as separators for numbers
install.packages("DT") # for fancy datatables
install.packages("ggplot2") 
install.packages("plyr")
install.packages('rhandsontable')
install.packages('highcharter')
install.packages('plotly')
install.packages('shinyWidgets')
install.packages('formattable')


###############################NOTICE and LICENSE###################################
#This file is part of GIRA - a claims reserving application designed for educational purposes only
##Please see https://github.com/djhindley/shiny-server/blob/master/claimsreserving/NoticeLicense.md for notice and license relating to this 
#Copyright 2017 David Hindley.
#This program is free software; you can redistribute it and/or modify it under the terms of the 
#GNU General Public License as published by the Free Software Foundation, either version 2 of the License, 
#or (at your option) any later version.
#This program is distributed in the hope that it will be useful, 
#but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
#See the GNU General Public License for more details. 
#If you do not already have a copy of this license, write to 
#the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA or see http://www.gnu.org/licenses/
#####################################################################################
##libraries and overall variables set here
#Date: 1 October 2017
library(shiny)
library(shinydashboard)
library(ChainLadder)
library(copula)         # load the copula package
library(distr)          # distribution library
library(scatterplot3d)  # scatterplot3d - not always needed
library(matrixcalc)
#library(actuar)        # removed this as seemed to cause problem with ChainLadder - not sure why, as worked before!
library(shinythemes)
library(shinyLP)
library(shinyBS)
library(shinyjs)
library(scales)         # used for adding commas as separators for numbers
library(DT)             # for fancy datatables
library(ggplot2)        # for good graphs
library(markdown)       # for good graphs
library(plyr)
library(rhandsontable)
library(highcharter)
library(plotly)
library(shinyWidgets)
library(formattable)


Triangle_options<-c("RAA", 
                    "UKMotor",
                    "MW2008",
                    "MW2014",
                    "GenIns")
options(shiny.sanitize.errors = TRUE)
#Special CSS for loading page.
appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"


# setwd('G:/3-R&D/1 Assurance/11 - Projet P&C/2019 - Shiny-CL/shiny-server-master/claimsreserving/Data')
# GenIns         <- read.csv('GenIns.csv')
# MW2008         <- read.csv('MW2008.csv')
# MW2014         <- read.csv('MW2014.csv')
# RAA            <- read.csv('RAA.csv')
# Reservingbook  <- read.csv('Reserving book.csv')
# UKMotor        <- read.csv('UKMotor.csv')


# GenIns         <- as.matrix(GenIns)
# MW2008         <- as.matrix(MW2008)
# MW2014         <- as.matrix(MW2014)
# RAA            <- as.matrix(RAA)
# Reservingbook  <- as.matrix(Reservingbook)
# UKMotor        <- as.matrix(UKMotor)



  