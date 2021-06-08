# =====================================================================================================
#  Sylvie Foucher le 21 janvier 2019
# =====================================================================================================
source("\\\\asap1\\fame\\eco\\fe\\R\\Eco chart generator\\EcoChartGenerator_libraries.R")
source("\\\\asap1\\fame\\eco\\fe\\R\\Eco chart generator\\EcoChartGenerator_MultiChart.R")
source("\\\\asap1\\fame\\eco\\fe\\R\\Eco chart generator\\ECO_Statlinks.R")
source ("\\\\asap1\\fame\\eco\\fe\\Foucher_S\\R_graphs\\EcoFunctions.R")
source ("\\\\asap1\\fame\\eco\\fe\\Foucher_S\\R_graphs\\EcoGraphFunctions.R")
 setwd("K:\\users\\Foucher_s\\_R\\Templates\\outputs\\")
 
# Data Extract 
#---------------------------
  
X              <- c("House", "Labour", "Education", "ALMP", "Social")
series_labels  <- c("Housing policies to enhance labour mobility", "Labour tax reforms to improve job opportunities",
                    "Expanding vocational education and training", "Strengthening active labour market policies", 
                     "Improving targeting and coverage of social benefits")

Fully      <- c(15.78947368,	 16,	23.52941176,	27.5,	50)
NoAction   <- c( 84.21052632,  84,	76.47058824,	72.5,	50)
alldata    <-   data.frame(X,Fully, NoAction)
   
# -- mes calculs
#------------------------------------------------------
# Graph Parameters
# --------------------------------------------------
  
 param_language    <- "EN"
 Nb_chapter       <- 0     # pour 
 NB_fig           <- 12                               #toString(NB_fig),
 
 Chart_size    <- c("FW" , "1_3P")
 Page_division <- c(1 , 1)

#-----------------------------------------------------
if (param_language == "EN"){
  title        <-    paste0("Figure ",NB_fig  , ". The take up of structural reforms to help people and displaced workers has been modest") 

  subtitle <- "Per cent share of Going for Growth recommendations"
  
  footnote <- "Note: Refers to reform priorities identified in Going for Growth in 2017 for the 46 economies covered."
  source   <- "OECD Going for growth 2018."
  series_labelsX  <-  c("Improving targeting and coverage of social benefits", "Strenghening active labour market policies", "Expanding vocational education and training", 
                                   "Labour tax reforms to improve job opportunities", "Housing policies to enhance labour mobility" )
  series_axis_labels         <-  c(" ", " ") 
}
#--------------------------------------------- CHART 1
panel_ontop   <- FALSE
 
series_codes   <-  c("NoAction", "Fully")
series_labels   <-  c(" No Action taken in 2007  ", " Fully implemented or in process of implementation ")
series_axis       <-  c("L", "L")
 
left_scale         <-c (0, 100, 20)
legendpos       <- c ("0.1", "0.90", "horizontal") 
# ------------------------------------------------- 
alldata1         <- select(alldata,append("X", series_codes) )    
alldata_Stk      <- df.StackedData(alldata,series_codes) 

EcoChart_init(series_axis, series_labels)   

if (interactive()==TRUE) {graphics.off()}

ch1  <-   ggplot(data = alldata1)    #expand_limits(y=left_scale[1:2])  +
ch1 <- ch1 + geom_bar (data = alldata_Stk,mapping  = aes( x = reorder(X,value*1), y = value,fill  = cGroupOrder,group = cGroupOrder), stat = "identity", position = "stack", color  = "black", size = 0.3)  
 
ch1 <- ch1 +scale_fill_manual  (labels   = series_labels  ,   values  =  series_colors)     
ch1 <- ch1 +scale_y_continuous (position = Yposition,breaks = seq( left_scale[1] , to = left_scale[2] , by = left_scale[3] ) , expand   = c(0, 0)) 
ch1 <- ch1 +labs               (x= ""  , y  = manage_axis_title( series_axis_labels[1], Yposition),     title    =  " ",   subtitle = " ")  
ch1 <- ch1 +coord_cartesian    (ylim= c(left_scale[1]-(abs(left_scale[2]-left_scale[1])/200000)*abs(left_scale[1]), left_scale[2]))  
ch1 <- ch1 +scale_x_discrete    (labels = rev(series_labelsX) )  
ch1 <- ch1 +coord_flip()             +  guides(fill = guide_legend(reverse = TRUE)) 
ch1 <- ch1 +theme_ECO(legendposition  = "bottom",legenddirection =  legendpos[3],panelontop=  panel_ontop,rotateXLabel  = "NO", empty_axis_on =Emptyaxis,ticksOnX= "NO")  
ch1 <- ch1 +theme(axis.title.y      =element_text(size=7,vjust = 1.085, margin = margin(r = -0.2, unit = "in")))
  
list1 <- list (LEFT_Y_LABEL  =  series_axis_labels[1],    RIGHT_Y_LABEL =  series_axis_labels[2],CHART = ch1, SERIES_CODES=series_codes , SERIES_LABELS =  series_labels) # pour statlinks

if (interactive()==TRUE) {graphics.off()}
print(ch1)

#----------------------------------------------------------------------   MultiCharts
charts       <- list(ch1)  
EcoChart_output  ( charts, "YES"  )              # pour creer tous les output requis eps, txt, statlinks
Eco_CreateStatlink( title,  subtitle,  paste0(output_folder,output_filename,".xls"),  footnote, source,  list1) 
