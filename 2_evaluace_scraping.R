# Load libraries ----------------------------------------------------------

library(RSelenium)
library(wdman)
library(plyr)
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


# Open browser ------------------------------------------------------------

browser$open()

url <- "https://hodnoceni.ff.cuni.cz/results/evaluation/summer-term-20212022/course/21bASGV10005p1"

# navigate to evaluace
browser$navigate(url)

# get course name
course_name <- browser$findElement("class", "ev--page-title")$getElementText()

# get semester name
semester <- browser$findElement("class", "ev--page-subtitle")$getElementText() %>%
  str_remove_all("Výsledky . ")

# get the teacher box
teacher_box <- browser$findElements("class", "teacher-boxs") %>% 
  map(~.x$getElementText()) %>% 
  # clean it
  map(~str_remove_all(.x, pattern = "\nodrazující\nvynikající\n")) %>%
  map(~str_split(.x, "\\n")) %>% 
  flatten()

# get average course rating
avg_course <- browser$findElement("class", "course-average-rating")$getElementText() %>%
  # extract number w/ decimals
  str_extract("\\d+\\.?\\d*")

# get each question, n of answers, and average answer
rating <- browser$findElements("class", "results-question-item") %>% 
  map(~.x$getElementText()) %>%
  map(~str_split(.x, pattern = "\\n")) %>% 
  flatten() 



# get N of ratings and N of students
n_rating <- browser$findElements("class", "ta-r") %>%
  map(~.x$getElementText())



# Get the comments --------------------------------------------------------


# show comments for positives
browser$findElement("class", "blue-hotfix-button")$clickElement()

# scrape content
positive <- browser$findElements("css", ".ev--dark-text p") %>%
  map(~.x$getElementText())

# toggle comments for positive - hide them
browser$findElement("class", "blue-hotfix-button")$clickElement()


# show comments to negative
browser$findElement("css selector", ".results-question-item+ .results-question-item .clickable")$clickElement()

# scrape content
negative <- browser$findElements("css", ".ev--dark-text p") %>%
  map(~.x$getElementText())


# Putting it together ??  -------------------------------------------------

comments <- list(negative = flatten(negative), 
                 positive = flatten(positive))



tibble(course_name = course_name, avg = avg_course, rated = n_rating[[1]], n_students = n_rating[[2]], url = url) %>%
  unnest(everything())


tibble(course_name = course_name, url = url, teacher = teacher_box) %>%
  unnest(everything()) %>% 
  mutate(teacher_rating = as.numeric(str_extract(teacher, "\\d+\\.?\\d*")),
         teacher = str_trim(str_remove(teacher, "\\d+\\.?\\d*%")))
  
