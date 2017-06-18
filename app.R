# A brief timeline of the evolution of peer review
# Companion to 'A multi-disciplinary perspective on emergent and future innovations in peer review'
# Should be deployed to https://dgraziotin.shinyapps.io/peerreviewtimeline/

# Copyright 2017 Daniel Graziotin, <daniel@ineed.coffee>
#
# LICENSE
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction, including without limitation the rights to 
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
# and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#  
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
# TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE 
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

if(!require(dplyr)) {
  install.packages("dplyr", dependencies = T)
}

if (!require("devtools")) {
  install.packages("devtools", dependencies = T)
}

require(devtools)

if (!require(timevis)) {
  devtools::install_github("daattali/timevis")
}

if (!require(shiny)) {
  install.packages("shiny", dependencies = T)
}

require(dplyr)
require(timevis)
library(shiny)

rerank <- function(pr.timeline.df, orderGroups = FALSE) {
  pr.timeline.df$Id <-
    rank(pr.timeline.df$Id, ties.method = "first")
  
  if (orderGroups) {
    pr.timeline.df$GroupId <- dense_rank(pr.timeline.df$GroupId)
  }
  
  return(pr.timeline.df)
}


timeline <- function(pr.timeline.df, orderGroups = FALSE) {
  pr.timeline.df <- rerank(pr.timeline.df, orderGroups)
  pr.timeline <- data.frame(
    id      = 1:max(pr.timeline.df$Id),
    content = pr.timeline.df$Event,
    start   = pr.timeline.df$Date,
    end     = rep(NA, max(pr.timeline.df$Id)),
    type    = rep("box", max(pr.timeline.df$Id)),
    group   = pr.timeline.df$GroupId,
    groups  = pr.timeline.df$Group,
    style = rep(
      "color: white;background-color:#f2673c; border:0;",
      NROW(pr.timeline.df)
    )
  )
  
  groups <- data.frame(id = pr.timeline.df$GroupId,
                       content = pr.timeline.df$Group)
  
  groups <- distinct(groups[order(groups$id), ])
  
  return(timevis(
    pr.timeline,
    groups = data.frame(
      id = groups$id,
      content = groups$content,
      title = groups$content
    ),
    fit = TRUE,
    showZoom = TRUE,
    options = list(
      showCurrentTime = FALSE,
      orientation = "top",
      editable = FALSE,
      min = "1550",
      max = "2150"
      
    )
  ))
  
}

pr.timeline.data <- read.csv2("./timeline-peerreview.csv")
colnames(pr.timeline.data) <- c("Id",	"Event", "Date", "Group",	"GroupId")

pr.timeline.viz <- timeline(pr.timeline.data, TRUE)

pr.timeline.viz.1665.1967 <- timeline(subset(pr.timeline.data, pr.timeline.data$Date <= 1967), TRUE)
pr.timeline.viz.1968.2009 <- timeline(
  subset(
    pr.timeline.data,
    pr.timeline.data$Date > 1967 &
      pr.timeline.data$Date <= 2009
  ),
  TRUE
)
pr.timeline.viz.2010.2017 <- timeline(
  subset(
    pr.timeline.data,
    pr.timeline.data$Date > 2009 &
      pr.timeline.data$Date <= 2017
  ),
  TRUE
)

ui <- fluidPage(
  title = "A timeline of peer review",
  tags$head(tags$style(
    HTML(
      "
      .vis-panel.vis-bottom, .vis-panel.vis-center, .vis-panel.vis-left, .vis-panel.vis-right, .vis-panel.vis-top {
      border: 1px solid #ccc;
      }
      div.vis-item.vis-dot {
      background-color:#333;
      border-color:#f2673c;
      
      }
      div.vis-item.vis-line {
      border-color:#666;
      }
      div.vis-inner {
      color:#333;
      }
      .vis-text {
      color:#333 !important;
      font-weight: bold !important;
      }
      
      "
    )
    )),
  div(
    id = "header",
    div(
      "A brief timeline of the evolution of peer review: complete dataset. Scroll down for zoomed timelines."
    )
  ),
  mainPanel(
    id = "prtimelineviz",
    timevisOutput(outputId = 'prtimelineviz'),
    width = 12
  ),
  div(
    "A brief timeline of the evolution of peer review: The primordial times 1665-1967."
  ),
  mainPanel(
    id = "prtimelineviz16651967",
    timevisOutput(outputId = 'prtimelineviz16651967'),
    width = 12
  ),
  div(
    "A brief timeline of the evolution of peer review: The revolution 1968-2009."
  ),
  mainPanel(
    id = "prtimelineviz19682009",
    timevisOutput(outputId = 'prtimelineviz19682009'),
    width = 12
  ),
  div(
    "A brief timeline of the evolution of peer review: The revolution 2010-2017"
  ),
  mainPanel(
    id = "prtimelineviz20102017",
    timevisOutput(outputId = 'prtimelineviz20102017'),
    width = 12
  )
)


server <- function(input, output, session) {
  output$prtimelineviz <- renderTimevis({
    pr.timeline.viz
  })
  output$prtimelineviz16651967 <- renderTimevis({
    pr.timeline.viz.1665.1967
  })
  output$prtimelineviz19682009 <- renderTimevis({
    pr.timeline.viz.1968.2009
  })
  output$prtimelineviz20102017 <- renderTimevis({
    pr.timeline.viz.2010.2017
  })
}

shinyApp(ui = ui, server = server)
