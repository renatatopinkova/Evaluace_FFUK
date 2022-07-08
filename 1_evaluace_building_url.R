# Load libraries ----------------------------------------------------------

library(RSelenium)
library(wdman)
library(dplyr)
library(tidyr)
library(purrr)

# Set up driver -----------------------------------------------------------

# set up port - must be an integer, otherwise throws an error in wdman::selenium() command
PORT <- as.integer(4445)

# set up server
server <- wdman::selenium(port = PORT) # note: could be started with selenium() w/o port, but better to specify it

# set up the browser
# firefox - recommended, crashes the least
browser <- remoteDriver(
  browserName = "firefox",
  port = PORT
)

class(browser)


# open browser
browser$open()

# Building the url db -----------------------------------------------------


browser$navigate("https://hodnoceni.ff.cuni.cz/results/evaluation/summer-term-20212022/department")

url_default <- browser$findElements("css", ".ev--click a") %>% 
  map(~.x$getElementAttribute("href")) %>%
  flatten()




# Get courses URLs --------------------------------------------------------

# replace url with iterations of url_default links
browser$navigate("https://hodnoceni.ff.cuni.cz/results/evaluation/summer-term-20212022/department/21-FU")

url_courses <- browser$findElements("css", ".div-table-course-gl a") %>% 
  map(~.x$getElementAttribute("href"))
