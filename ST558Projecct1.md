ST558Project
================
Mandy Liesch
10/2/2021

-   [Required Packages](#required-packages)
-   [Function Creation](#function-creation)
    -   [Type Fuction](#type-fuction)
    -   [Type Cleaning Function](#type-cleaning-function)
-   [Data Preparation](#data-preparation)
    -   [Filling in and Cleaning the typeCleaning
        Function](#filling-in-and-cleaning-the-typecleaning-function)

\#`{r, include=FALSE} #knitr::opts_chunk$set(echo = TRUE) #`

# Required Packages

``` r
#load the required packages
require("httr")
```

    ## Loading required package: httr

    ## Warning: package 'httr' was built under R version 4.1.1

``` r
require("jsonlite")
```

    ## Loading required package: jsonlite

``` r
require("tidyverse")
```

    ## Loading required package: tidyverse

    ## Warning: package 'tidyverse' was built under R version 4.1.1

    ## -- Attaching packages ------------------------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.1.1

    ## Warning: package 'tibble' was built under R version 4.1.1

    ## Warning: package 'readr' was built under R version 4.1.1

    ## -- Conflicts ---------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter()  masks stats::filter()
    ## x purrr::flatten() masks jsonlite::flatten()
    ## x dplyr::lag()     masks stats::lag()

``` r
require("RCurl")
```

    ## Loading required package: RCurl

    ## Warning: package 'RCurl' was built under R version 4.1.1

    ## 
    ## Attaching package: 'RCurl'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     complete

``` r
require("ggplot2")
library(usethis)
```

    ## Warning: package 'usethis' was built under R version 4.1.1

``` r
library(knitr)
library(rmarkdown)
```

    ## Warning: package 'rmarkdown' was built under R version 4.1.1

``` r
use_git_config(user.name="Mandy Liesch", user.email="amliesch@ncsu.edu")
```

``` r
#code to create the rendering
rmarkdown::render("ST558Projecct1.Rmd", 
                  output_format = "github_document", 
                  output_options = list(toc=TRUE, toc_depth = 2, html_preview=FALSE))
```

# Function Creation

## Type Fuction

### Determining Primary and Secondary Pokemon Type

There are 18 different types of pokemon. These 18 types apply to both
Pokémon and their moves. It is possible for pokemon to have two
different types, but they usually have a primary type. This initital
function was created to help the user figure out which type they would
like to specify.

``` r
#Return all the pokemon names and their informative urls into a dataframe 


allPoke<-function(){
  allPokeURL<-"https://pokeapi.co/api/v2/pokemon?limit=1500"
  pokeData <- RCurl::getURL(allPokeURL)
  pokeDataDF<-jsonlite::fromJSON(pokeData)
  output <- pokeDataDF$results
  pokeData1<-purrr::map(output$url, jsonlite::fromJSON)
  return(pokeData1)
}
```

allPokeURL&lt;-“<https://pokeapi.co/api/v2/pokemon?limit=1500>” pokeData
&lt;- RCurl::getURL(allPokeURL)
pokeDataDF&lt;-jsonlite::fromJSON(pokeData) output &lt;-
pokeDataDF*r**e**s**u**l**t**s**p**o**k**e**D**a**t**a* &lt;  − *p**u**r**r**r* :  : *m**a**p*(*o**u**t**p**u**t*url,
jsonlite::fromJSON)

pokeName&lt;-frame\[\[index\]\]*n**a**m**e**p**o**k**e**H**e**i**g**h**t* &lt;  − *f**r**a**m**e*\[\[*i**n**d**e**x*\]\]height
pokeWeight&lt;-frame\[\[index\]\]*w**e**i**g**h**t**p**o**k**e**T**y**p**e*1 &lt;  − *f**r**a**m**e*\[\[*i**n**d**e**x*\]\]types*t**y**p**e*name\[1\]
pokeType2&lt;-frame\[\[index\]\]*t**y**p**e**s*type$name\[2\]
stats&lt;-t(frame\[\[index\]\]\[\[“stats”\]\]\[1\])

``` r
types <- function(type){
  ###
  # This functions returns a data.frame with the numeric key and the name of the types        #associated with pokemon. It can also return the data for a single type if a type ID        #number or name is passed.
  ###
  
  # Get the overall type from the type endpoint, allowing a user specified type.
  baseURL<-"https://pokeapi.co/api/v2/type/"
  typeData <- RCurl::getURL(baseURL)
  typeDataDF<-jsonlite::fromJSON(typeData)
  
  # Select and create the type data.frame from the results.
  output <-  typeDataDF$results
  outputType <- data.frame(type=1:20, output)
  
  # If team does not equal "all", check if it is a valid pokemon type or numeric value.
  if (type != "all"){
    
    # If team is in the id column, subset output for just that row.
    if (type %in% outputType$type){
      pokeType <- type
      pokeTypeSpec <-GET(paste0(baseURL,pokeType))
      pokeTypeparsed <- pokeTypeSpec$content %>% rawToChar() %>% fromJSON()
      typeParseRes<-pokeTypeparsed$pokemon
      pokePrimary<-purrr::map(typeParseRes$pokemon$url, jsonlite::fromJSON)
      return(pokePrimary)
    }
    # If team is in the fullName column, subset output for just that row.
    if (type %in% outputType$name){
      pokeType <- type
      pokeTypeSpec <-GET(paste0(baseURL,pokeType))
      pokeTypeparsed <- pokeTypeSpec$content %>% rawToChar() %>% fromJSON()
      typeParseRes<-pokeTypeparsed$pokemon
      pokePrimary<-purrr::map(typeParseRes$pokemon$url, jsonlite::fromJSON)
      return(pokePrimary)
    }
    # Otherwise, throw an informative error.
    else {
    message <- paste("ERROR: The numeric Index of Pokemon Type, or Pokemon Type is not                          clear.",
                    "Select from the menu above, or type('all') to find information of all                      of the Pokemon Type you're looking for.")
    print(outputType[1:2])
    stop(message)
    
    }
   }
  else {
  pokePrimary<-allPoke() 
  return(pokePrimary)
  }
}
```

``` r
genOut <- function(generation){
  ###
  # This functions returns a data.frame with the numeric key and the name of the types        #associated with pokemon. It can also return the data for a single type if a type ID        #number or name is passed.
  ###
  
  # Get the overall type from the type endpoint, allowing a user specified type.
  baseURL<-"https://pokeapi.co/api/v2/generation/"
  genData <- RCurl::getURL(baseURL)
  genDataDF<-jsonlite::fromJSON(genData)
  
  # Select and create the type data.frame from the results.
  output <-  genDataDF$results
  outputGen <- data.frame(gen=1:8, output)
  
  # If team does not equal "all", check if it is a valid pokemon type or numeric value.
  if (generation != "all"){
    
    # If team is in the id column, subset output for just that row.
    if (generation %in% outputGen$name){
      pokeGen <- generation
      pokeGenSpec <-GET(paste0(baseURL,pokeGen))
      pokeGenparsed <- pokeGenSpec$content %>% rawToChar() %>% fromJSON()
      genParseRes<-pokeGenparsed$pokemon_species
      genPrimary<-purrr::map(genParseRes$url, jsonlite::fromJSON)
      return(genPrimary)
    }
    # If team is in the fullName column, subset output for just that row.
    if (generation %in% outputGen$gen){
      pokeGen <- generation
      pokeGenSpec <-GET(paste0(baseURL,pokeGen))
      pokeGenparsed <- pokeGenSpec$content %>% rawToChar() %>% fromJSON()
      genParseRes<-pokeGenparsed$pokemon_species
      genPrimary<-purrr::map(genParseRes$url, jsonlite::fromJSON)
    }
    # Otherwise, throw an informative error.
    else {
    message <- paste("ERROR: The number or index of generation chosen is not a valid selection.",
                    "Select from the menu above, of try generation('all') to find the Pokemon Type you're looking for.")
    print(outputGen[1:2])
    stop(message)
    
    }
   }
  #else {
    
  }
```

## Type Cleaning Function

### Using the API to read the urls to Populate the Data Frame

This function was originally designed to take the input from the type
function, and process and clean it to the final output. However, I had
to change this due to processing speed of the API querying. It took 15
minutes or more to run this process. To expedite the process, running
the user specified type through the function to get the list, which is
then fed into the type processing frame as “Frame”. The code for the
original function driven task is commented out, and replaces the first
two lines.

``` r
#typeFrame<-function(type, index){
#  frame<-types(type)
#typeAllFrames<-types('all')
#typeFireFrames<-types('fire')

typeFrame<-function(frame, index){
  agg<-frame[[index]]
  pokeid<-agg$id
  pokeName<-agg$name
  pokeHeight<-agg$height
  pokeWeight<-agg$weight
  pokeType1<-agg$types$type$name[1]
  pokeType2<-agg$types$type$name[2]
  stats<-t(agg[["stats"]][1])
  pokeFinal<-as.data.frame(cbind(pokeid, pokeName, pokeHeight, pokeWeight, pokeType1,        pokeType2, stats))
  return(pokeFinal)
}
```

``` r
#genChoice<-genOut(2)

genFrame<-function(frame, index){
  pokeid<-frame[[index]]$id
  pokeName<-frame[[index]]$name
  pokeGen<-frame[[index]]$generation$name
  pokeGrowth<-frame[[index]]$growth_rate$name
  pokeHappy<-frame[[index]]$base_happiness
  pokeCapture<-frame[[index]]$capture_rate
  pokeGenFinal<-as.data.frame(cbind(pokeid, pokeName, pokeGen, pokeGrowth, pokeHappy,     pokeCapture))
  return(pokeGenFinal)
}
```

# Data Preparation

## Filling in and Cleaning the typeCleaning Function

``` r
listLen<-length(typeAllFrames)
numIndex<-1:listLen

finalTypeFrame<-lapply(X = numIndex, FUN = typeFrame, frame= typeAllFrames)
finalFrame<-do.call(rbind.data.frame, finalTypeFrame)
```

``` r
cleanFrame<-function(lapplyFrame){
  lapplyFrame$hp<-lapplyFrame$`1`
  lapplyFrame$hp<-as.numeric(lapplyFrame$hp)
  lapplyFrame$`1`<-NULL
  lapplyFrame$attack<-lapplyFrame$`2`
  lapplyFrame$attack<-as.numeric(lapplyFrame$attack)
  lapplyFrame$`2`<-NULL
  lapplyFrame$defense<-lapplyFrame$`3`
  lapplyFrame$defense<-as.numeric(lapplyFrame$defense)
  lapplyFrame$`3`<-NULL
  lapplyFrame$specialattack<-lapplyFrame$`4`
  lapplyFrame$specialattack<-as.numeric(lapplyFrame$specialattack)
  lapplyFrame$`4`<-NULL
  lapplyFrame$specialdefense<-lapplyFrame$`5`
  lapplyFrame$specialdefense<-as.numeric(lapplyFrame$specialdefense)
  lapplyFrame$`5`<-NULL
  lapplyFrame$speed<-lapplyFrame$`6`
  lapplyFrame$speed<-as.numeric(lapplyFrame$speed)
  lapplyFrame$`6`<-NULL
  lapplyFrame$pokeHeight<-as.numeric(lapplyFrame$pokeHeight)
  lapplyFrame$pokeWeight<-as.numeric(lapplyFrame$pokeWeight)
  lapplyFrame$pokeid<-as.numeric(lapplyFrame$pokeid)
  return(lapplyFrame)
}  
```

``` r
listGenLen<-length(genChoice)
numGenIndex<-1:listGenLen

finalGenFrame<-lapply(X = numGenIndex, FUN = genFrame, frame = genChoice)

finalGenFrame<-do.call(rbind.data.frame, finalFrame)

finalGenFrame$pokeid<-as.numeric(finalGenFrame$pokeid)
finalGenFrame$pokeHappy<-as.numeric(finalGenFrame$pokeHappy)
finalGenFrame$pokeCapture<-as.numeric(finalGenFrame$pokeCapture)
```

``` r
gen3Fire<-merge(finalFrame, finalGenFrame, by=c('pokeid','pokeName'))
tables<-cleanFrame(finalFrame)
tables<-as_tibble(tables)

bargraph<-tables %>%
    mutate(SUM = rowSums(.[7:12])) %>%
    group_by(pokeType1) %>%
    summarise_at(vars(SUM), list(Average = mean))

plot1 <- ggplot(bargraph)
  

tables %>%
  count(pokeType1) %>%
  mutate(Percentage=n/sum(n)*100)

tables %>%
  count(pokeType2) %>%
  mutate(Percentage=n/sum(n)*100)
  

hist2 <- tables %>%
  group_by(pokeType2) %>%
  summarise(Freq=n())
```
