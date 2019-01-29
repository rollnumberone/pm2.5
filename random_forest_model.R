## loading the library to read the data from excel file
library(openxlsx)

##Reading the excel file
data2<- read.xlsx("G:/Notes/Data Science/case study pm2.5/PMlevel.xlsx", sheet = 1)

#viewing the data
View(data2)

#checking the structure of the data
str(data2)

#checking the dimension of the data
dim(data2)

#loading the hmisc library
library(Hmisc)

#checking the description of the data
describe(data2)


#removing the not required columns
data2<-data2[,-c(1,2)]

#PM2.5 has 2067 values missing
sum(is.na(data2$pm2.5))

#removing the missing values
data2<-na.omit(data2)

#correcting the data types of certain columns
for (i in 1:5) {
  data2[,i]<-as.integer(data2[,i])
  
}

data2$cbwd<-as.factor(data2$cbwd)
data2$Is<-as.integer(data2$Is)
data2$Ir<-as.integer(data2$Ir)

# change "cv" in cbwd to "SW"
levels(data2$cbwd)[1] <- "SW"


#checking distribution of PM2.5
hist(data2$pm2.5)

#checking the distribution of DEWP
summary(data2$DEWP)  #data is normally distributed
hist(data2$DEWP)

#checking the outliers 
boxplot(data2$DEWP) #no outliers present


#checking the distribution of TEMP
summary(data2$TEMP)  #data is normally distributed
hist(data2$TEMP)

#checking the outliers
boxplot(data2$TEMP) #no outliers present

#checking the distribution of PRES
summary(data2$PRES)  #data is normally distributed
hist(data2$PRES)

#checking the outliers
boxplot(data2$PRES)  #no outliers present


#checking the distribution of cbwd
summary(data$cbwd)
#loading the ggplot library
library(ggplot2)
print(ggplot(data2, aes(data[, 8])) +
        geom_histogram(stat = "count") +
        xlab(colnames(data)[8]))


#checking the distribution of lws
summary(data2$Iws)
hist(data2$Iws)  #data distribution is right skewed

#min value is 0.45 and maximum value is 565.49

#checking for outliers
boxplot(data2$Iws)
21.91 + 1.5* IQR(data2$Iws)  #outliers are present

##checking  upto how much value can explain 99% of data
x<-quantile(data2$Iws, prob = c(0.99))   #value of x comes out to be 268.4776

#outliers can be removed by capping the values above x by x
data2$Iws[data2$Iws>x]<-x


#checking the distribution of ls
summary(data2$Is)
hist(data2$Is)  #right skewed distribution

#checking the distribution of lr
summary(data2$Ir)
hist(data2$Ir)   #right skewed distribution


#correlation of PM2.5 with other variables

for(i in 5:11) {
  if(!is.factor(data2[,i])) {
    print(paste("pm2.5 & ", colnames(data2)[i], sep = ""))
    print(cor(data2$pm2.5, data2[, i], use = "complete.obs"))
    
  }
}


#correlation between other variables variables
cor(data2[,-8])
# DEWP & TEMP, DEWP & PRES, TEMP & PRES are highly correlated




############      FEATURE ENGINEERING  ###############

 ##divding the day into four quarters
 first_quater_day<-ifelse(data2$hour<7,1,0)  #first quarter
 second_quater_day<-ifelse(data2$hour>6 & data2$hour<13,1,0)  #second quarter
 third_quarter_day<-ifelse(data2$hour>12 & data2$hour<19,1,0) #third quarter
 fourth_quater_day<-ifelse(data2$hour>18,1,0)  #fourth quarter

 #adding these variables in the data set
 data2<-cbind(data2,first_quater_day,second_quater_day,third_quarter_day,fourth_quater_day)

 ## Dividind the year in buckets of the month
 first_three_months<-ifelse(data2$month<4,1,0) #first three month
 four_to_six_month<-ifelse(data2$month>3 & data2$month<7,1,0)  #four to six month
 seven_to_nine_month<-ifelse(data2$month>6 & data2$month<10,1,0) #seven to nine months
 ten_to_twelve_month<-ifelse(data2$month>9,1,0)  #ten to twelve month

 #adding these variables in the data set
data2<-cbind(data2, first_three_months,four_to_six_month,seven_to_nine_month,ten_to_twelve_month)


 ## Dividing each month in buckets of week
 week1<-ifelse(data2$day<8,1,0) #first week
 week2<-ifelse(data2$day>7 & data2$day<15,1,0) #second week
 week3<-ifelse(data2$day>14 & data2$day<22,1,0) # third week
 week4<-ifelse(data2$day>21 & data2$day<29,1,0) # fourth week
 week5<-ifelse(data2$day>28,1,0) # fifth week
 
 #adding these variables in the data set
data2<-cbind(data2,week1,week2,week3,week4,week5)

# creating the variables for wind speed between 0 and 50
wind_speed_0to50<-ifelse(data2$Iws<=50,1,0)

#adding the variable in the data set
data2<-cbind(data2,wind_speed_0to50)
 


 #creating buckets on dew
 dew_neg20_to_zero<-ifelse(data2$DEWP>=(-20) & data2$DEWP<0,1,0) # dew between -20 and 0
 dew_zero_and_above<-ifelse(data2$DEWP>0,1,0)  #dew between 0 and above

 
 # adding these variables in the data set
 data2<-cbind(data2,dew_neg20_to_zero,dew_zero_and_above)
 


#checking the structure and dimension of the data
str(data2)
dim(data2)



#changing the level of cbwd 
levels(data2$cbwd)[1] <- "1"
levels(data2$cbwd)[2] <- "2"
levels(data2$cbwd)[3] <- "3"
levels(data2$cbwd)[4] <- "4"

#convertig cbwd into integer
data2$cbwd<-as.integer(data2$cbwd)


#############     MOdelling    #########

##  Loading the catools library
library(caTools)

# using set seed to keep our  testing and training data same 
set.seed(130)

#Splitting the data 
sample2<- sample.split(data2$pm2.5,SplitRatio = 0.80)

#creating test and train
train2 <- subset(data2,sample2==T)
test2 <- subset(data2,sample2==F)

#checking the dimension of train
dim(train2)


## loading randomForest to create a random forest model#####
library(randomForest)


##Creating the model
rf2<-randomForest(pm2.5~.,data=train2)

##printing the model
print(rf2)

##predicting the PM2.5 in test data
p2<-predict(rf2,test2)

##Creating a data frame  "t2" of the predicted values and the orginal values
t2<-(data.frame(p2,test2$pm2.5))

##checking the head and tail of t2
head(t2)
tail(t2)

##checking mean absolute percentage error
mape <- mean(abs((pred-test$pm2.5))/test$pm2.5)

##loading metrics library for rmse()
library(Metrics) 

## Checking the root mean square error 
rmse(p2,test2$pm2.5)

## checking the accuracy of the model
100 - rmse(p2,test2$pm2.5)


##checking the importance of variables 
varImpPlot(rf2)

## plotting the model to checking after how many ntree the out of box errors gets contant
plot(rf2)


##tuning the random forest model
t2<-tuneRF(train2[,-4],train2[,4],
           stepFactor = 0.5,
           plot=T,
           ntreeTry = 250,
           trace = T,
           improve = 0.05)

## creating model using 225 ntree and 16 mtry
rf2<-randomForest(pm2.5~.,data=train2,
                  ntree = 250,
                  mtry = 16)


#printing the model
print(rf2)

##checking the importance of variables
varImpPlot(rf2)

## predicting the PM2.5 in the test data
p2<-predict(rf2,test2)

##Creating a data frame  "t2" of the predicted values and the orginal values
t2<-(data.frame(p2,test2$pm2.5))

##checking the head and tail of t2
head(t2)
tail(t2)

##checking mean absolute percentage error
mape <- mean(abs((p2-test2$pm2.5))/test2$pm2.5)

## checking the accuracy of the model
100 - rmse(p2,test2$pm2.5)





### First run all the code above then run Rshiny code ###########

###############             R shiny          #########


## Loading the libraries
library(shiny)
library(shinydashboard)
library(plotly)

#creating the UI 
ui<- shinyUI(
  
  ##Dashboard page
  dashboardPage(
                    dashboardHeader(title = "PM2.5 Prediction",
                    dropdownMenu(
                    type = "message",
                    messageItem(from = "Kamran",
                                  message = "Hello sir, have a nice day",
                                  time = Sys.time()))),
    
    ###sidebar
    dashboardSidebar(
      
      ##two tabs
      sidebarMenu(
        menuItem("Prediction",tabName = "Predict"),
        menuItem("Insights", tabName = "Insights")
      )),
    
    
    #creating the body of the dashboard
    dashboardBody(
      tabItems(
        
        ###creating tab for taking input for prediction
        tabItem(tabName = "Predict",
                fluidRow(
                  box(title="Inputs", status = "primary",solidHeader = T,
                      
                      ##Taking different variables as input
                      numericInput("m","Enter the month","1",min = 1,max = 12), 
                      numericInput("d","Enter the day","1",min = 1,max = 31),
                      numericInput("h","Enter the hour","1",min = 1, max = 23),
                      numericInput("dd","Enter the dew point","1",min = -40, max = 40),
                      numericInput("t","Enter the temperature in fahrenheit","1",min = -50, max = 50),
                      numericInput("pp","Enter the pressure in hectopascal","1",min = 1),
                      selectInput("wd", "Select cumilative wind direction",
                      c("SOUTH WEST" = 1,
                       "NORTH EAST" = 2,
                       "NORTH WEST" = 3,
                      "SOUTH EAST" = 4)),
                      numericInput("wind_speed","Enter cumilative wind ","1",min = 1),
                      numericInput("rr","Enter cumilative hours of rain","0",min = 0),
                      numericInput("ss","Enter cumilative hours of snow","0",min = 0),
                      actionButton("Run_model","Submit")
                  ),
                  box(status = "warning",textOutput("text"), title = "Predicted PM2.5",
                      solidHeader = T )
               
                  
                   )),
        
        ##Tab to plot some graphs
        tabItem(tabName = "Insights",
                
                fluidRow(
                  ## Graphs between month and PM2.5 value
                  box(title = "Distribution of PM2.5 over different months", status = "primary", solidHeader = T,
                      plotlyOutput("plot1")),
                  
                  ## Graphs between dew and PM2.5 value
                  box(title = "Variation of PM2.5 with DEW ", status = "primary", solidHeader = T,
                      plotlyOutput("plot3")),
                  
                  ## Graphs between rain and PM2.5 value
                  box(title = " Variation of PM2.5 with rain", status = "primary", solidHeader = T,
                      plotlyOutput("plot2")),
                  
                  ## Graphs showing the distribution of PM2.5 in different wind direction
                  box(title = "PM2.5 distribution by different wind direction", status = "primary", solidHeader = T,
                      plotlyOutput("plot4"))
                  ))
          ))))



#creating the server function 
server<-function(input,output)
{
  # Bar chart between month and Pm2.5
  output$plot1<- renderPlotly({
    plot_ly(x = as.factor(data2$month), y = data2$pm2.5 , 
            type="bar",marker = list(color = 'rgb(158,202,225)')) %>% 
      layout(
             xaxis = list(title = "Months"),
             yaxis = list (title = "PM2.5"))
  })
  
  #Plot between rain and PM2.5
  output$plot2<- renderPlotly({
    plot_ly(x = as.factor(data2$Ir), y = data2$pm2.5 , 
            type="bar") %>% 
      layout(
        xaxis = list(title = "Cumilative hours of rain"),
        yaxis = list (title = "PM2.5"))
  })
  
  #plot between Dew and Pm2.5
  output$plot3<- renderPlotly({
    plot_ly(x = as.factor(data2$DEWP), y = data2$pm2.5 , 
            type="scatter") %>% 
      layout(
        xaxis = list(title = "DEW"),
        yaxis = list (title = "PM2.5"))
  })
  
  #Plot showing the distribution of PM2.5 at different wind directions
  output$plot4<- renderPlotly({
    plot_ly(data2, labels = c("South West","North East", "North West", "South East"), 
            values = c("140.79","95.96","70.53","115.89"), type = 'pie')
    
  })
  
  ##Text output where output will be displayed
  output$text<-renderText(
    { 
      paste("Predicted PM2.5 value is")
    })
  
  ## reading the iput
  user_input <- reactive({
    # this is how you fetch the input variables from ui component
    pm2.5<-as.integer(0)
    month<-as.integer(input$m)
    day<-as.integer(input$d)
    hour<-as.integer(input$h)
    DEWP<-as.integer(input$dd)
    TEMP<-as.numeric(input$t)
    PRES<-as.numeric(input$pp)
    cbwd<-as.integer(input$wd)
    Iws<-as.numeric(input$wind_speed)
    Is<-as.integer(input$ss)
    Ir<-as.integer(input$rr)
    
    
    ##Creating all the feature engineering variables for input data
    first_quater_day<-ifelse(hour<7,1,0)
    second_quater_day<-ifelse(hour>6 & hour<13,1,0)
    third_quarter_day<-ifelse(hour>12 & hour<19,1,0)
    fourth_quater_day<-ifelse(hour>18,1,0)
  
    first_three_months<-ifelse(month<4,1,0)
    four_to_six_month<-ifelse(month>3 & month<7,1,0)
    seven_to_nine_month<-ifelse(month>6 & month<10,1,0)
    ten_to_twelve_month<-ifelse(month>9,1,0)
    
    week1<-ifelse(day<8,1,0)
    week2<-ifelse(day>7 & day<15,1,0)
    week3<-ifelse(day>14 & day<22,1,0)
    week4<-ifelse(day>21 & day<29,1,0)
    week5<-ifelse(day>28,1,0)
    
    wind_speed_0to50<-ifelse(Iws<=50,1,0)
   
    dew_neg20_to_zero<-ifelse(DEWP>=(-20) & DEWP<0,1,0)
    dew_zero_and_above<-ifelse(DEWP>0,1,0)
   
    #binding all the variables together
    user_input<-cbind(month,day,hour,pm2.5,DEWP,TEMP,PRES,cbwd,Iws,Is,Ir,
                      first_quater_day,second_quater_day,third_quarter_day,fourth_quater_day,
                      first_three_months,four_to_six_month,seven_to_nine_month,ten_to_twelve_month,
                      week1,week2,week3,week4,week5,
                      wind_speed_0to50,dew_neg20_to_zero,dew_zero_and_above)
    
    ##Creating a data frame               
    user_input<-as.data.frame(user_input)
    
    #converting the type of variable as required
    
    user_input$month<-as.integer(user_input$month)
    user_input$day<-as.integer(user_input$day)
    user_input$hour<-as.integer(user_input$hour)
    user_input$DEWP<-as.integer(user_input$DEWP)
    user_input$TEMP<-as.numeric(user_input$TEMP)
    user_input$PRES<-as.numeric(user_input$PRES)
    user_input$cbwd<-as.integer(user_input$cbwd)
    user_input$Iws<-as.numeric(user_input$Iws)
    user_input$Is<-as.integer(user_input$Is)
    user_input$Ir<-as.integer(user_input$Ir)
    user_input$pm2.5<-as.integer(user_input$pm2.5)
    
    for (i in 12:27) {
      user_input[,i]<-as.numeric(user_input[,i])
    }

    user_input<-as.data.frame(user_input)
    
  })
  
  #observing when user enter submit
  observeEvent(input$Run_model, 
               {
                                ##precting the PM2.5 using the data that user has entered
                 output_p <-   predict(rf2, newdata = user_input())
                 
                 #displaying the predicted value
                 output$text<-renderText(
                   {
                     paste("Predicted PM2.5 value is ",output_p,"ug/m^3")
                   })
               })
  }

##invoking the shiny app
shinyApp(ui=ui,server=server)



























