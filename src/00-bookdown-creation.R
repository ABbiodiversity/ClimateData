library(bookdown)
library(dplyr)
library(kableExtra)

# Bookdown resources https://bookdown.org/yihui/bookdown/github.html

# Confirms the nojekyll is created
file.create(paste0(getwd(), "/docs/.nojekyll"))

# Render bookdown
bookdown::render_book(input = "bookdown/", 
                      output_format = "bookdown::gitbook",
                      output_dir = "D:/abmi/ClimateData/docs/")




