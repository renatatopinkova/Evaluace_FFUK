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


# Start scraping ----------------------------------------------------------



## Get basic course info ---------------------------------------------------


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
avg_course <- browser$findElement("class", "course-average-rating")$getElementText()


# get N of ratings and N of students
n_rating <- browser$findElements("class", "ta-r") %>%
  map(~.x$getElementText()) 



## Get particular ratings --------------------------------------------------



questions_wording <- c("S uvážením všeho, co jste do této chvíle vyplnil/a, zhodnote, prosíme, pøedmìt celkovì.",
                       "Byl pro Vás pøedmìt pøínosný?",
                       "Pøekrýval se podle Vás obsah výuky v tomto pøedmìtu zbyteènì s obsahem výuky v jiném pøedmìtu?",
                       "Jaká byla podle Vás nároènost pøedmìtu v porovnání s ostatními pøedmìty?",
                       "Zhodnote, prosíme, pedagogické pùsobení vyuèujícího.")


# get each question, n of answers, and average answer
rating <- browser$findElements("class", "results-question-item") %>% 
  map(~.x$getElementText()) %>%
  map(~str_split(.x, pattern = "\\n")) %>% 
  flatten() %>% 
  map(~tibble(var = .[1],
                      resp = .[2],
                      val  = .[3])) %>%
  bind_rows() %>%
  filter(!str_detect(resp, 'Komentáøù')) %>% 
  mutate_at(c("resp", "val"), str_extract, "\\d+\\.?\\d*") %>%
  mutate(var = case_when(var == questions_wording[1] ~ "total",
                         var == questions_wording[2] ~ "useful",
                         var == questions_wording[3] ~ "overlap",
                         var == questions_wording[4] ~ "difficulty",
                         var == questions_wording[5] ~ "teacher"))





## Get the comments --------------------------------------------------------


# show comments for positives
browser$findElement("class", "blue-hotfix-button")$clickElement()


# scrape content
positive <- browser$findElements("css", ".ev--dark-text p") %>%
  map(~.x$getElementText()) 

# toggle comments for positive - hide them
browser$findElement("class", "blue-hotfix-button")$clickElement()


# show comments to negative
browser$findElement("css", ".results-question-item+ .results-question-item .clickable")$clickElement()

# scrape content
negative <- browser$findElements("css", ".ev--dark-text p") %>%
  map(~.x$getElementText()) 


# Putting it together ??  -------------------------------------------------


# get course rating rundown
tibble(course_name = course_name, 
       semester = semester,
       avg = avg_course, 
       rated = n_rating[[1]], 
       n_students = n_rating[[2]], 
       url = url, 
       var = rating$var,
       resp = rating$resp,
       val = rating$val) %>%
  unnest(course_name) %>% 
  mutate_at(., c("avg", "rated", "n_students", "resp", "val"), ~as.numeric(str_extract(., "\\d+\\.?\\d*")))
  

# get teacher stats 
tibble(course_name = course_name, url = url, teacher = teacher_box) %>%
  unnest(everything()) %>% 
  mutate(teacher_rating = as.numeric(str_extract(teacher, "\\d+\\.?\\d*")),
         teacher = str_trim(str_remove(teacher, "\\d+\\.?\\d*%"))) %>% 
  bind_cols(., rating[rating$var == "teacher", ])


## TODO: would be nice to have comments as nested column in teacher stats
comments <- list(negative = flatten(negative), 
                 positive = flatten(positive))