
library(rvest)
library(dplyr)
library(stringr)

url <- 'https://www.guinnessworldrecords.com/search?term=Fastest%20marathon%20dressed%20as&page=1&type=record&max=500&partial=_Results&#search-results'

html <- read_html('download_html/Search Results _ Guinness World Records v2.html')

links <- as.character(html_nodes(html,"h3 a"))
http_link <- substr(str_extract(links,'http.*\\">'),1,nchar(str_extract(links,'http.*\\">'))-2)
http_description <- substr(str_extract(links,'>.*</a>'),2,nchar(str_extract(links,'>.*</a>'))-4)
desc_and_time <- as.character(html_nodes(html,"article div div"))

time_extract <- '\\d{1,2}\\shr{0,1}s{0,1}(,){0,1}(\\s\\d{1,2}\\smin){0,1}( and){0,1}(\\s\\d{1,2}(.\\d){0,2}\\ssec){0,1}'
time_chr <- str_extract(desc_and_time,time_extract)

df <- data.frame(url=http_link,desc=http_description,time_string=time_chr,stringsAsFactors = F)

marathons <- filter(df,grepl('marathon',tolower(df$desc)))
dressed_as <- filter(marathons,!grepl('time',tolower(marathons$desc)))
dressed_as <- filter(dressed_as,!grepl('tallest',tolower(dressed_as$desc)))
dressed_as <- filter(dressed_as,!grepl('most',tolower(dressed_as$desc)))
dressed_as <- filter(dressed_as,!grepl('longest',tolower(dressed_as$desc)))
dressed_as <- filter(dressed_as,!grepl('fixed seat row',tolower(dressed_as$desc)))
dressed_as <- filter(dressed_as,!grepl('inline skating',tolower(dressed_as$desc)))
dressed_as <- filter(dressed_as,!grepl('dribbling',tolower(dressed_as$desc)))
dressed_as <- filter(dressed_as,!grepl('robotic walking',tolower(dressed_as$desc)))
dressed_as <- filter(dressed_as,!grepl('skipping',tolower(dressed_as$desc)))
dressed_as <- filter(dressed_as,!grepl('relay team',tolower(dressed_as$desc)))
dressed_as <- filter(dressed_as,!is.na(time_string))

test <- dressed_as
dressed_as$hour <- as.integer(str_extract(str_extract(tolower(dressed_as$time_string),'\\d{1,2}\\sh'),'\\d+'))
dressed_as$min <- as.integer(str_extract(str_extract(tolower(dressed_as$time_string),'\\d{1,2}\\smin'),'\\d+'))
dressed_as$sec <- as.numeric(str_extract(str_extract(tolower(dressed_as$time_string),'\\d{1,2}(.\\d){0,2}\\ssec'),'\\d+(.\\d){0,2}'))

dressed_as$min <- ifelse(is.na(dressed_as$min),0,dressed_as$min)
dressed_as$sec <- ifelse(is.na(dressed_as$sec),0,dressed_as$sec)

dressed_as$time <- paste0(
  paste0(dressed_as$hour),':',
  if_else(nchar(dressed_as$min)==1,paste0(0,dressed_as$min),paste0(dressed_as$min)),':',
  if_else(nchar(dressed_as$sec)==1,paste0(0,dressed_as$sec),paste0(dressed_as$sec))
)

dressed_as$type <- if_else(grepl('half',dressed_as$desc),'half','full')
dressed_as$total_time_in_secs <- dressed_as$hour*(60*60) + dressed_as$min*(60) + dressed_as$sec

# Finding outfits
dressed_as$desc <- gsub('  ',' ',dressed_as$desc)

extra_text <- '(Fastest (half ){0,1}marathon (dressed ){0,1}((as a )|(as an )|(as )|(in a )|(in an )|(in )|(by a )|(wearing a )|(wearing )))|(Fastest (half ){0,1}marathon )'
dressed_as$outfit <- gsub(extra_text,'',dressed_as$desc)

dressed_as$outfit[dressed_as$outfit=='distance (male)'] <- 'any outfit (male)'
dressed_as$outfit[dressed_as$outfit=='(female, masters 35)'] <- 'any outfit (female, masters 35)'

dressed_as$last_update <- Sys.Date()

write.csv(dressed_as,"data/fastest_marathon_dressed_as.csv", row.names = F)
