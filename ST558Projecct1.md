ST558Project
================
Mandy Liesch
10/2/2021

-   [Required Packages](#required-packages)
-   [Function Creation](#function-creation)
    -   [Type Fuctions](#type-fuctions)
        -   [allPoke(): Returns ALL
            Pokemon](#allpoke-returns-all-pokemon)
        -   [types(): User Defined Type](#types-user-defined-type)
        -   [typeFrame(): Returning Several
            Columns](#typeframe-returning-several-columns)
        -   [allGen(): Returns All Generations of
            Pokemon](#allgen-returns-all-generations-of-pokemon)
    -   [Type Cleaning Function](#type-cleaning-function)
        -   [Using the API to read the urls to Populate the Data
            Frame](#using-the-api-to-read-the-urls-to-populate-the-data-frame)
-   [Data Preparation](#data-preparation)
    -   [Filling in and Cleaning the typeCleaning
        Function](#filling-in-and-cleaning-the-typecleaning-function)

\#`{r, include=FALSE} #knitr::opts_chunk$set(echo = TRUE) #`

# Required Packages

``` r
#load the required packages
require("httr")
require("jsonlite")
require("tidyverse")
require("RCurl")
require("ggplot2")
library(usethis)
library(knitr)
library(rmarkdown)
use_git_config(user.name="Mandy Liesch", user.email="amliesch@ncsu.edu")
```

``` r
#code to create the rendering
rmarkdown::render("ST558Projecct1.Rmd", 
                  output_format = "github_document", 
                  output_options = list(toc=TRUE, toc_depth = 3, html_preview=FALSE))
```

# Function Creation

## Type Fuctions

### allPoke(): Returns ALL Pokemon

There are 18 different types of Pokemon. These 18 types apply to both
Pokémon and their moves. It is possible for Pokemon to have two
different types, but they usually have a primary type. This initial
function was created to help the user figure out which type they would
like to specify. There are several functions necessary to create the
output by type.

``` r
#Return all the pokemon names and their informative urls into a dataframe. 
allPoke<-function(){
  #Connect to the Pokemon endpoint, using a limit to make sure all are returned.
  allPokeURL<-"https://pokeapi.co/api/v2/pokemon?limit=1500"
  #Utilize the RCurl to get all of the json data from the endpoint
  pokeData <- RCurl::getURL(allPokeURL)
  #Parse the JSon data into a list with the content variables
  pokeDataDF<-jsonlite::fromJSON(pokeData)
  #pull all of the important results into the dataframe (the pokemon name and URL)
  output <- pokeDataDF$results
  #Use the purr package to pull all of the JSON Data for each pokemon URL into a list.
  pokeData1<-purrr::map(output$url, jsonlite::fromJSON)
  #Return this list when the function is called. 
  return(pokeData1)
}
```

### types(): User Defined Type

This function utilizes the Type endpoint to return the information of a
type that the user specifies. It has many different layers, and allows
the user to input a number from 1-20, that corresponds to a pokemon
type, OR a type character string, like ‘fire’. If ‘all’ is specified, a
list is returned with all pokemon, from the allPoke() function, listed
previously. If an incorrect string or number is put in, a list is
returned of appropriate options in Pokemon type.

``` r
types <- function(type){
  ###
  # This functions returns a data.frame with the numeric key and the name of the types       #associated with pokemon. It can also return the data for a single type if a type ID       #number or name is passed.
  ###
  
  # Get the overall type from the type endpoint, allowing a user specified type.
  baseURL<-"https://pokeapi.co/api/v2/type/"
  # pull the information for all types from the endpoint
  typeData <- RCurl::getURL(baseURL)
  # convert this into a dataframe
  typeDataDF<-jsonlite::fromJSON(typeData)
  # Select and create the type data.frame from the results.
  output <-  typeDataDF$results
  #Create a table to return the type and number in case bad input is specified.
  outputType <- data.frame(type=1:20, output)
  
  #First If Function
  # If type does not equal "all", check if it is a valid pokemon type or numeric value.
  if (type != "all"){
    
    # If type is in the numeric type column, subset output for just that row.
    if (type %in% outputType$type){
      #set the function to query
      pokeType <- type
      #Create the new link to read the user specified API value
      pokeTypeSpec <-GET(paste0(baseURL,pokeType))
      #Run the API through the data, return the JSON
      pokeTypeparsed <- pokeTypeSpec$content %>% rawToChar() %>% fromJSON()
      #Pull the desired data from the pokemon subset of list.
      typeParseRes<-pokeTypeparsed$pokemon
      #Use the purr package to return the json data
      pokePrimary<-purrr::map(typeParseRes$pokemon$url, jsonlite::fromJSON)
      #Return the dataframe.
      return(pokePrimary)
    }
    # If type is in the name column, subset output for just that table, see above for          # documentation.
    if (type %in% outputType$name){
      pokeType <- type
      pokeTypeSpec <-GET(paste0(baseURL,pokeType))
      pokeTypeparsed <- pokeTypeSpec$content %>% rawToChar() %>% fromJSON()
      typeParseRes<-pokeTypeparsed$pokemon
      pokePrimary<-purrr::map(typeParseRes$pokemon$url, jsonlite::fromJSON)
      return(pokePrimary)
    }
    # Otherwise, if type does not equal all, throw an informative error, return the options     # table.
    else {
    message <- paste("ERROR: The numeric Index of Pokemon Type, or Pokemon Type is not                          clear.",
                    "Select from the menu above, or type('all') to find information of all                      of the Pokemon Type you're looking for.")
    print(outputType[1:2])
    stop(message)
    }
  }
  #If the user specified the all function, return the output of the allPoke() function.
  else {
  pokePrimary<-allPoke() 
  return(pokePrimary)
  }
}
```

### typeFrame(): Returning Several Columns

The list function dataset is very complex. This nested function turns
the user specified type list into the dataframe that we desire. The
index value is a numerical input that is used to return the function.
Due to speed concerns, I removed this nesting function capability, for
processing, requiring data frames to be run. The top lines of code
change to:

``` r
typeFrame<-function(type, index){
  frame<-types(type)
```

``` r
typeFrame<-function(frame, index){
  ###
  # This functions takes the specified type defined by a dataframe in the types function,   # and creates one row of a dataframe by pokemon index number. 
  ###
  
  #for simplification, the indexing process is changed to a shorter command
  agg<-frame[[index]]
  #Pull the data for the pokemon ID. 
  pokeid<-agg$id
  #Pull the data for the pokemon Name.
  pokeName<-agg$name
  #Pull Pokemon Height Data
  pokeHeight<-agg$height
  #Pull Pokemon Weight Data
  pokeWeight<-agg$weight
  #Pull data from Type 1. 
  pokeType1<-agg$types$type$name[1]
  #Pull data from Type 2.
  pokeType2<-agg$types$type$name[2]
  #The stats column for pokemon stats is vertical, and needs to be transposed
  #so it fits onto one line. It is the first column of the $stats data.
  stats<-t(agg[["stats"]][1])
  #Create a dataframe
  pokeFinal<-as.data.frame(cbind(pokeid, pokeName, pokeHeight, pokeWeight, pokeType1,        pokeType2, stats))
  #return the dataframe.
  return(pokeFinal)
}
```

### allGen(): Returns All Generations of Pokemon

Like the allPoke() function above, this generation function queries the
Generation API to get all of the information available. This is not a
function that is user queryable, and is part of the generation function.

``` r
allGen<-function(){
  #Connect to the Pokemon Species endpoint, using a limit to make sure all are returned.
  allGenURL<-"https://pokeapi.co/api/v2/pokemon-species?limit=1000/"
  #Utilize the RCurl to get all of the json data from the endpoint
  genData <- RCurl::getURL(allGenURL)
  #Parse the JSon data into a list with the content variables
  genDataDF<-jsonlite::fromJSON(genData)
  #pull all of the important results into the dataframe (the pokemon name and URL)
  output <- genDataDF$results
  #Use the purr package to pull all of the JSON Data for each pokemon URL into a list.
  genData1<-purrr::map(output$url, jsonlite::fromJSON)
  #Return this list when the function is called.
  return(genData1)
}
```

``` r
genOut <- function(generation){
  ###
  # This functions returns a data.frame with the numeric key and the name of the types       #associated with pokemon. It can also return the data for a single type if a type ID       #number or name is passed.
  ###
  
  # Get the overall generation from the type endpoint, allowing a user specified             # generation.
  baseURL<-"https://pokeapi.co/api/v2/generation/"
  #pull all of the generation URLs, and put them into a json.
  genData <- RCurl::getURL(baseURL)
  genDataDF<-jsonlite::fromJSON(genData)
  
  # Select and create the type data.frame from the results.
  output <-  genDataDF$results
  #create the data frame needed for specifying errors.
  outputGen <- data.frame(gen=1:8, output)
  
  # If generation does not equal "all", check if it is a valid pokemon type or numeric       #value.
  if (generation != "all"){
    
    # If generation is in the number column, subset output for just that row.
    if (generation %in% outputGen$name){
      #use the user specified value to generate the new API section.
      pokeGen <- generation
      #put the url into the API
      pokeGenSpec <-GET(paste0(baseURL,pokeGen))
      #parse through the data to get the URLs from the API query.
      pokeGenparsed <- pokeGenSpec$content %>% rawToChar() %>% fromJSON()
      #Pull data from the pokemon species subset of the generations.
      genParseRes<-pokeGenparsed$pokemon_species
      #Get the individual pokemon URLs
      genPrimary<-purrr::map(genParseRes$url, jsonlite::fromJSON)
      #Return the list.
      return(genPrimary)
    }
    # If generation is in the generation list column, subset output for just that row.
    #Repeats from above
    if (generation %in% outputGen$gen){
      pokeGen <- generation
      pokeGenSpec <-GET(paste0(baseURL,pokeGen))
      pokeGenparsed <- pokeGenSpec$content %>% rawToChar() %>% fromJSON()
      genParseRes<-pokeGenparsed$pokemon_species
      genPrimary<-purrr::map(genParseRes$url, jsonlite::fromJSON)
      return(genPrimary)
    }
    # Otherwise, throw an informative error to get the appropriate values that occur. .
    else {
    message <- paste("ERROR: The number or index of generation chosen is not a valid                           selection.",
                     "Select from the menu above, of try generation('all') to find the                         Pokemon and Generation you're looking for.")
               print(outputGen[1:2])
               stop(message)
    }
  }
  #If all is specified, then run the allGen() function that we defined earlier.
  else {
  genPrimary<-allGen() 
  return(genPrimary)
 } 
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
genFrame<-function(generation, index){
  frame<-genOut(generation)
  pokeid<-frame[[index]]$id
  pokeName<-frame[[index]]$name
  pokeGen<-frame[[index]]$generation$name
  pokeGrowth<-frame[[index]]$growth_rate$name
  pokeHappy<-frame[[index]]$base_happiness
  pokeCapture<-frame[[index]]$capture_rate
  pokeGenFinal<-as.data.frame(cbind(pokeid, pokeName, pokeGen, pokeGrowth, pokeHappy,        pokeCapture))
  return(pokeGenFinal)
}
```

``` r
cleanFrame<-function(lapplyFrame){
  ### The purpose of this function is to take the list run through lapply, and clean and
  #   correct the column formatting so the data can be manipulated with dplyr, and   graphed.
  ###
  
  #rename the columns to their correct name
  lapplyFrame$hp<-lapplyFrame$`1`
  #change the column type to numeric
  lapplyFrame$hp<-as.numeric(lapplyFrame$hp)
  #remove the old column value.
  lapplyFrame$`1`<-NULL
  #repeat this for all columns that it needs to happen to. 
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
numGenIndex<-1:100

finalGenFrame<-lapply(X = numGenIndex, FUN = genFrame, generation = 3)

finalGenFrame<-do.call(rbind.data.frame, finalFrame)

finalGenFrame$pokeid<-as.numeric(finalGenFrame$pokeid)
finalGenFrame$pokeHappy<-as.numeric(finalGenFrame$pokeHappy)
finalGenFrame$pokeCapture<-as.numeric(finalGenFrame$pokeCapture)
```

# Data Preparation

## Filling in and Cleaning the typeCleaning Function

``` r
#Running the types function for the user determined function (want 'all')
typeAllFrames<-types('all')

#Calculate the total length of the frame
listLen<-length(typeAllFrames)

#Calculate the index values necessary for the lapply column.
numIndex<-1:listLen

#Use the typeAllFrames, and the numIndex values to use the lapply value to get the total frame.

#I have not figured out how to put this into a function with length needed? but I can call this dataframe and proceed.
finalTypeFrame<-lapply(X = numIndex, FUN = typeFrame, frame=typeAllFrames)

#Convert the data using the do call function into the character dataframe.
finalFrame<-do.call(rbind.data.frame, finalTypeFrame)

#drop this output frame into the cleanFrame function
finalClean<-cleanFrame(finalFrame)
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
