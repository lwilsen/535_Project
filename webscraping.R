### webscraping portion: to get demographics on the different boroughs

library(tidyverse)
library(jsonlite)
wiki <- readLines("/Users/eschmucker/Documents/STAT535/Final Project/boroughs_wiki copy.html")
str(wiki)

head(wiki)

wiki <- unlist(wiki)


(contains_census <- which(str_detect(wiki,"Density of population")))

smaller <- wiki[390:620]

#cut out some common lines: <p class=\"p21\"><span class=\"s2\"><i>square</i></span></p>" 

## cut </td>
smaller <- smaller[- which(str_detect(smaller, "\\<\\/td\\>"))]

## cut <tr>
smaller <- smaller[- which(str_detect(smaller, "\\<\\/*tr\\>"))]

# cut <td colspan=\"2\" valign=\"middle\" class=\"td2\">"
smaller <- smaller[ -which(str_detect(smaller, "\\<td colspan=.+"))]

# cut <td valign=\"middle\" class=\"td19\">" 
smaller <- smaller[ - which(str_detect(smaller, "\\<td valign.+"))]


### wait. don't cut these whole lines. 
# cut <p class="p21"><span class="s2"><i>
smaller <- str_replace_all(smaller, "\\<p class=\"p\\d\\d\"\\>\\<span class=\"s2\"\\>\\<i\\>", "")

# cut <p class="p21"><span class="s2">
smaller <- str_replace_all(smaller, "\\<p class=\"p\\d\\d\"\\>\\<span class=\"s2\"\\>", "")

# cut </span></p>" and <b> and </b>
smaller <- str_replace_all(smaller, "\\/*b\\>","")

#cut <span...  or </span 
smaller <- str_replace_all(smaller, "\\<\\/*span.+", "")

#cut </i>
smaller <- str_replace_all(smaller, "\\<\\/i\\>","")
smaller <- smaller[-2]

# get rid of <ul>
smaller <- smaller[- which(str_detect(smaller, "\\<\\/*ul.+"))]

#get rid of <p...
smaller <- smaller[- which(str_detect(smaller, "\\<p"))]

#get rid of extra spaces
smaller <- str_replace_all(smaller, "  +", "")

#get rid of > or <
smaller <- str_replace_all(smaller, "\\<|\\>|\\/", "")
cat(smaller[21])


### putting together the dataframe
boroughs <- data.frame(
  Borough = c("The Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
  Population = Population,
  Land_area_mi = Land_area_mi,
  Pop_density = Pop_density,
  gdp = gdp
  )

Population = numeric(5)
for (i in 1:5){
  Population[i] = smaller[20+i+6*(i-1)]
}

Land_area_mi = numeric(5)
for (i in 1:5){
  Land_area_mi[i] = smaller[21+i+6*(i-1)]
}

Pop_density = numeric(5)
for (i in 1:5){
  Pop_density[i] = smaller[23+i+6*(i-1)]
}

gdp = numeric(5)
for (i in 1:5){
  gdp[i] = smaller[25+i+6*(i-1)]
}

##exporting to csv
write.csv(boroughs, "wiki.csv")





