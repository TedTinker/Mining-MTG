# The first 560 lines of R construct a data-frame from the JSON file.
# The next 30 lines make some neat plots. 

# install.packages("tidyverse")       # Installation only necessary once.
# install.packages("rjson")           # I think all these packages are needed, 
# install.packages("gtools")          # I'm scared to remove them.
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("stringi")
# install.packages("plyr")
# install.packages("scales")
# install.packages("viridis")

library(tidyverse)
library(rjson)      # To read JSON
library(gtools)   
library(stringr)    # To interact with strings
library(ggplot2)    # Nice plots
library(stringi)    # Regex parsing
library(plyr)
library(scales)
library(viridis)

setwd("/Users/Theodore/Desktop/R_Studio_Stuff/data")  # Change this to your own directory
MTG.json <- fromJSON(file="AllCards.json",method='C') 
    # Download JSON file from https://mtgjson.com

rows=17761    # When testing, decrease this so you don't use the whole data-set every time

MTG.df <- data.frame(matrix(ncol=15,nrow=rows))   
     # We pull 15 variables from the data and generate more

# The function "isItThere" is vital to parsing JSON. 
# Nested lists and inconsistant variable types require it 
# for producing consistant dataframe row-lengths.
# The argument "json" is a JSON object.
# "x" is the position in the JSON list to open; 
# each will be a Magic: The Gathering card.
# "string" is the name of the variable to check for in the JSON element x.
# "otherwise" is what the program should return 
# if the variable doesn't exist; sometimes "", sometimes NA.
# "after" is the nested list index in case we need to check 
# elements of a nested list which may not exist.
isItThere <- function(json,x,string,otherwise,after="") {
  if(after!="") {if(is.null(json[[x]][[string]][after])) {return(otherwise)} else
  {return(json[[x]][[string]][after])}} else
    if(is.null(json[[x]][[string]])) {return(otherwise)} else {return(json[[x]][[string]])}}

# Color-identity is expressed as up to five of the characters WUBR and G. 
sapply(1:rows, function(x) {colorIdentity <- c(isItThere(MTG.json,x,"colorIdentity",NA,1),
                                               isItThere(MTG.json,x,"colorIdentity",NA,2),
                                               isItThere(MTG.json,x,"colorIdentity",NA,3),
                                               isItThere(MTG.json,x,"colorIdentity",NA,4),
                                               isItThere(MTG.json,x,"colorIdentity",NA,5))
colorIdentity <- colorIdentity[!is.na(colorIdentity)]

# Supertype, Type, and Subtype are collections of zero to three character strings        
supertype <- c(isItThere(MTG.json,x,"supertypes",NA,1),
               isItThere(MTG.json,x,"supertypes",NA,2),
               isItThere(MTG.json,x,"supertypes",NA,3))
supertype <-supertype[!is.na(supertype)]

type <- c(isItThere(MTG.json,x,"types",NA,1),
          isItThere(MTG.json,x,"types",NA,2),
          isItThere(MTG.json,x,"types",NA,3))
type <-type[!is.na(type)]

subtype <- c(isItThere(MTG.json,x,"subtypes",NA,1),
             isItThere(MTG.json,x,"subtypes",NA,2),
             isItThere(MTG.json,x,"subtypes",NA,3))
subtype <-subtype[!is.na(subtype)]
# Once these character variables are set, we will decompose them into factors

# The list of printings is a collection of character-string abbreviations of set-names.  
# We will decompose it into printing dates later.
printList <- c(unlist(lapply(1:length(MTG.json[[x]][["printings"]]),function (y)
{MTG.json[[x]][["printings"]][[y]]})))

MTG.df[x,] <<- c(MTG.json[[x]][["name"]],     # We describe all variables as we name them below
                 paste(colorIdentity,collapse=""),
                 MTG.json[[x]][["cmc"]],
                 paste(supertype,collapse=" "),
                 paste(type,collapse=" "),
                 paste(subtype,collapse=" "),
                 isItThere(MTG.json,x,"text",NA),
                 nchar(isItThere(MTG.json,x,"text","")),
                 isItThere(MTG.json,x,"power",NA),
                 isItThere(MTG.json,x,"toughness",NA),
                 isItThere(MTG.json,x,"loyalty",NA),
                 length(MTG.json[[x]][["rulings"]]),
                 length(MTG.json[[x]][["legalities"]]),
                 length(MTG.json[[x]][["printings"]]),
                 paste(printList, collapse=" "))})

MTG.df <- setNames(MTG.df,c("Name",                 # Character; MTG card name
                            "ColorIdentity",        # Factor; Color Identity
                            "ConvertedManaCost",    # Integer; number of mana required to cast
                            "Supertype",            # Character; broad categories
                            "Type",                 # Character; main card types
                            "Subtype",              # Character; supplimentary types
                            "Text",                 # Character; Rules text, not flavor text
                            "TextLength",           # Integer; number of characters in Text
                            "Power",                # Often an integer, sometimes a character
                            "Toughness",            # Often an integer, sometimes a character
                            "Loyalty",              # Integer; for Planeswalkers
                            "Rulings",              # Number of rulings
                            "Legalities",           # Number of legalities
                            "Printings",            # Number of printings
                            "PrintingsList"))       # Character; all set abbrvs

MTG.df <- transmute(MTG.df, Name=as.character(Name),
                    ColorIdentity=factor(ColorIdentity),
                    ConvertedManaCost=as.integer(ConvertedManaCost),
                    Supertype=as.character(Supertype),
                    Type=as.character(Type),
                    Subtype=as.character(Subtype),
                    Text=as.character(Text),
                    TextLength=as.integer(TextLength),
                    Power=as.character(Power),         # Mostly integers, some odd characters
                    Toughness=as.character(Toughness), # Mostly integers, some odd characters
                    Loyalty=as.integer(Loyalty),
                    Rulings=as.integer(Rulings),
                    Legalities=as.integer(Legalities),
                    Printings=as.integer(Printings),
                    PrintingsList=as.character(PrintingsList))

# The color identities should be in this order
MTG.df <- mutate(MTG.df,ColorIdentity=factor(ColorIdentity,levels=c("C","W","U","B","R","G",
                                                                    "WU","WB","WR","WG","UB","UR","UG","BR","BG","RG","WUB","WUR","WUG","WBR",
                                                                    "WBG","WRG","UBR","UBG", "URG","BRG","WUBR", "WUBG","WURG","WBRG",
                                                                    "UBRG","WUBRG"))) 
MTG.df$ColorIdentity[is.na(MTG.df$ColorIdentity)] <- "C" 

# Power and Toughness have some irregularities which should be sorted
MTG.df <- mutate(MTG.df,Power=factor(Power,levels=c("", "*","-1",".5","0","1","1.5","1+*","2",
                                                    "2.5","2+*","3","3.5","4","5","6","7","7-*","8","9","10","11","12","13","14","15","99")))
MTG.df$Power[is.na(MTG.df$Power)] <- ""

MTG.df <- mutate(MTG.df,Toughness=factor(Toughness,levels=c("", "*","-1",".5","0","1","1.5",
                                                            "1+*","2","2.5","2+*","3","3.5","4","5","6","7","7-*","8","9","10","11","12","13",
                                                            "14","15","99")))
MTG.df$Toughness[is.na(MTG.df$Toughness)] <- ""

MTG.df <- mutate(MTG.df, Legend = str_count(MTG.df$Supertype,"Legendary"),
                 Basic = str_count(MTG.df$Supertype,"Basic"),
                 
                 Artifact = str_count(MTG.df$Type,"Artifact"),
                 Creature =str_count(MTG.df$Type,"Creature")+ 
                   str_count(MTG.df$Type,"EastureCray"), # A card in a joke set is in pig-latin
                 Enchantment = str_count(MTG.df$Type,"Enchantment"),
                 Land = str_count(MTG.df$Type,"Land"),
                 Instant = str_count(MTG.df$Type,"Instant"),
                 Sorcery = str_count(MTG.df$Type,"Sorcery"),
                 Tribal = str_count(MTG.df$Type,"Tribal"),
                 Planeswalker = str_count(MTG.df$Type,"Planeswalker"))

# Subtypes were included only if about 100 examples were available
MTG.df <- mutate(MTG.df,
                 Artificer  = str_count(MTG.df$Subtype,"Artificer"),
                 Angel = str_count(MTG.df$Subtype,"Angel"),
                 Aura = str_count(MTG.df$Subtype,"Aura"),
                 Beast = str_count(MTG.df$Subtype,"Beast"),
                 Bird  = str_count(MTG.df$Subtype,"Bird"),
                 Cat = str_count(MTG.df$Subtype,"Cat"),
                 Cleric = str_count(MTG.df$Subtype,"Cleric"),
                 Construct = str_count(MTG.df$Subtype,"Construct"),
                 Demon = str_count(MTG.df$Subtype,"Demon"),
                 Dragon = str_count(MTG.df$Subtype,"Dragon"),
                 Giant = str_count(MTG.df$Subtype,"Giant"),
                 Knight = str_count(MTG.df$Subtype,"Knight"),
                 Shaman = str_count(MTG.df$Subtype,"Shaman"),
                 Druid = str_count(MTG.df$Subtype,"Druid"),
                 Eldrazi = str_count(MTG.df$Subtype,"Eldrazi"),
                 Elemental = str_count(MTG.df$Subtype,"Elemental"),
                 Elf = str_count(MTG.df$Subtype,"Elf"),
                 Equipment = str_count(MTG.df$Subtype,"Equipment"),
                 Golem = str_count(MTG.df$Subtype,"Golem"),
                 Human = str_count(MTG.df$Subtype,"Human"),
                 Horror = str_count(MTG.df$Subtype,"Horror"),
                 Illusion = str_count(MTG.df$Subtype,"Illusion"),
                 Insect = str_count(MTG.df$Subtype,"Insect"),
                 Merfolk = str_count(MTG.df$Subtype,"Merfolk"),
                 Monk = str_count(MTG.df$Subtype,"Monk"),
                 Rogue = str_count(MTG.df$Subtype,"Rogue"),
                 Scout  = str_count(MTG.df$Subtype,"Scout"),
                 Soldier = str_count(MTG.df$Subtype,"Soldier"),
                 Shapeshifter = str_count(MTG.df$Subtype,"Shapeshifter"),
                 Spirit = str_count(MTG.df$Subtype,"Spirit"),
                 Sliver = str_count(MTG.df$Subtype,"Sliver"),
                 Snake = str_count(MTG.df$Subtype,"Snake"),
                 Vampire = str_count(MTG.df$Subtype,"Vampire"),
                 Warrior = str_count(MTG.df$Subtype,"Warrior"),
                 Wall = str_count(MTG.df$Subtype,"Wall"),
                 Werewolf = str_count(MTG.df$Subtype,"Werewolf"),
                 Wizard = str_count(MTG.df$Subtype,"Wizard"),
                 Wurm = str_count(MTG.df$Subtype,"Wurm"),
                 Zombie = str_count(MTG.df$Subtype,"Zombie"))

MTG.df <- mutate(MTG.df, y1993 = as.numeric(str_count(MTG.df$PrintingsList,"LEA") >= 1|
                                              str_count(MTG.df$PrintingsList,"LEB") >= 1|
                                              str_count(MTG.df$PrintingsList,"2ED") >= 1|
                                              str_count(MTG.df$PrintingsList,"ARN") >= 1))
# The sets Alpha, Beta, Second Edition, and Arabian Nights were printed in 1993.
# We performed a similar mutation for every year up to 2018.

MTG.df <- mutate(MTG.df, y1994 = as.numeric(str_count(MTG.df$PrintingsList,"ATQ") >= 1|
                                              str_count(MTG.df$PrintingsList,"3ED") >= 1|
                                              str_count(MTG.df$PrintingsList,"LEG") >= 1|
                                              str_count(MTG.df$PrintingsList,"DRK") >= 1|
                                              str_count(MTG.df$PrintingsList,"FEM") >= 1),
                 y1995 = as.numeric(str_count(MTG.df$PrintingsList,"4ED") >= 1|
                                      str_count(MTG.df$PrintingsList,"ICE") >= 1|
                                      str_count(MTG.df$PrintingsList,"CHR") >= 1|
                                      str_count(MTG.df$PrintingsList,"HML") >= 1),
                 y1996 = as.numeric(str_count(MTG.df$PrintingsList,"ALL") >= 1|
                                      str_count(MTG.df$PrintingsList,"MIR") >= 1),
                 y1997 = as.numeric(str_count(MTG.df$PrintingsList,"5ED") >= 1|
                                      str_count(MTG.df$PrintingsList,"POR") >= 1|
                                      str_count(MTG.df$PrintingsList,"WTH") >= 1|
                                      str_count(MTG.df$PrintingsList,"TMP") >= 1),
                 y1998 = as.numeric(str_count(MTG.df$PrintingsList,"STH") >= 1|
                                      str_count(MTG.df$PrintingsList,"EXO") >= 1|
                                      str_count(MTG.df$PrintingsList,"PO2") >= 1|
                                      str_count(MTG.df$PrintingsList,"UGL") >= 1|
                                      str_count(MTG.df$PrintingsList,"USG") >= 1|
                                      str_count(MTG.df$PrintingsList,"ATH") >= 1),
                 y1999 = as.numeric(str_count(MTG.df$PrintingsList,"ULG") >= 1|
                                      str_count(MTG.df$PrintingsList,"6ED") >= 1|
                                      str_count(MTG.df$PrintingsList,"UDS") >= 1|
                                      str_count(MTG.df$PrintingsList,"PTK") >= 1|
                                      str_count(MTG.df$PrintingsList,"S99") >= 1|
                                      str_count(MTG.df$PrintingsList,"MMQ") >= 1|
                                      str_count(MTG.df$PrintingsList,"BRB") >= 1),
                 y2000 = as.numeric(str_count(MTG.df$PrintingsList,"NEM") >= 1|
                                      str_count(MTG.df$PrintingsList,"S00") >= 1|
                                      str_count(MTG.df$PrintingsList,"PCY") >= 1|
                                      str_count(MTG.df$PrintingsList,"INV") >= 1|
                                      str_count(MTG.df$PrintingsList,"BTD") >= 1),
                 y2001 = as.numeric(str_count(MTG.df$PrintingsList,"PLS") >= 1|
                                      str_count(MTG.df$PrintingsList,"7ED") >= 1|
                                      str_count(MTG.df$PrintingsList,"APC") >= 1|
                                      str_count(MTG.df$PrintingsList,"ODY") >= 1|
                                      str_count(MTG.df$PrintingsList,"DKM") >= 1),
                 y2002 = as.numeric(str_count(MTG.df$PrintingsList,"TOR") >= 1|
                                      str_count(MTG.df$PrintingsList,"JUD") >= 1|
                                      str_count(MTG.df$PrintingsList,"ONS") >= 1),
                 y2003 = as.numeric(str_count(MTG.df$PrintingsList,"LGN") >= 1|
                                      str_count(MTG.df$PrintingsList,"SCG") >= 1|
                                      str_count(MTG.df$PrintingsList,"8ED") >= 1|
                                      str_count(MTG.df$PrintingsList,"MRD") >= 1),
                 y2004 = as.numeric(str_count(MTG.df$PrintingsList,"DST") >= 1|
                                      str_count(MTG.df$PrintingsList,"5DN") >= 1|
                                      str_count(MTG.df$PrintingsList,"CHK") >= 1|
                                      str_count(MTG.df$PrintingsList,"UNH") >= 1),
                 y2005 = as.numeric(str_count(MTG.df$PrintingsList,"BOK") >= 1|
                                      str_count(MTG.df$PrintingsList,"SOK") >= 1|
                                      str_count(MTG.df$PrintingsList,"9ED") >= 1|
                                      str_count(MTG.df$PrintingsList,"RAV") >= 1),
                 y2006 = as.numeric(str_count(MTG.df$PrintingsList,"GPT") >= 1|
                                      str_count(MTG.df$PrintingsList,"DIS") >= 1|
                                      str_count(MTG.df$PrintingsList,"CSP") >= 1|
                                      str_count(MTG.df$PrintingsList,"TSP") >= 1),
                 y2007 = as.numeric(str_count(MTG.df$PrintingsList,"PLC") >= 1|
                                      str_count(MTG.df$PrintingsList,"FUT") >= 1|
                                      str_count(MTG.df$PrintingsList,"10E") >= 1|
                                      str_count(MTG.df$PrintingsList,"MED") >= 1|
                                      str_count(MTG.df$PrintingsList,"LRW") >= 1|
                                      str_count(MTG.df$PrintingsList,"EVG") >= 1),
                 y2008 = as.numeric(str_count(MTG.df$PrintingsList,"MOR") >= 1|
                                      str_count(MTG.df$PrintingsList,"SHM") >= 1|
                                      str_count(MTG.df$PrintingsList,"EVE") >= 1|
                                      str_count(MTG.df$PrintingsList,"DRB") >= 1|
                                      str_count(MTG.df$PrintingsList,"ME2") >= 1|
                                      str_count(MTG.df$PrintingsList,"ALA") >= 1|
                                      str_count(MTG.df$PrintingsList,"DD2") >= 1),
                 y2009 = as.numeric(str_count(MTG.df$PrintingsList,"CON") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDC") >= 1|
                                      str_count(MTG.df$PrintingsList,"ARB") >= 1|
                                      str_count(MTG.df$PrintingsList,"M10") >= 1|
                                      str_count(MTG.df$PrintingsList,"TD0") >= 1|
                                      str_count(MTG.df$PrintingsList,"V09") >= 1|
                                      str_count(MTG.df$PrintingsList,"HOP") >= 1|
                                      str_count(MTG.df$PrintingsList,"ME3") >= 1|
                                      str_count(MTG.df$PrintingsList,"ZEN") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDD") >= 1|
                                      str_count(MTG.df$PrintingsList,"H09") >= 1),
                 y2010 = as.numeric(str_count(MTG.df$PrintingsList,"WWK") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDE") >= 1|
                                      str_count(MTG.df$PrintingsList,"ROE") >= 1|
                                      str_count(MTG.df$PrintingsList,"DPA") >= 1|
                                      str_count(MTG.df$PrintingsList,"ARC") >= 1|
                                      str_count(MTG.df$PrintingsList,"M11") >= 1|
                                      str_count(MTG.df$PrintingsList,"V10") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDF") >= 1|
                                      str_count(MTG.df$PrintingsList,"SOM") >= 1|
                                      str_count(MTG.df$PrintingsList,"TD0") >= 1|
                                      str_count(MTG.df$PrintingsList,"PD2") >= 1),
                 y2011 = as.numeric(str_count(MTG.df$PrintingsList,"ME4") >= 1|
                                      str_count(MTG.df$PrintingsList,"MBS") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDG") >= 1|
                                      str_count(MTG.df$PrintingsList,"NPH") >= 1|
                                      str_count(MTG.df$PrintingsList,"CMD") >= 1|
                                      str_count(MTG.df$PrintingsList,"M12") >= 1|
                                      str_count(MTG.df$PrintingsList,"V11") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDH") >= 1|
                                      str_count(MTG.df$PrintingsList,"ISD") >= 1|
                                      str_count(MTG.df$PrintingsList,"PD3") >= 1),
                 y2012 = as.numeric(str_count(MTG.df$PrintingsList,"DKA") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDI") >= 1|
                                      str_count(MTG.df$PrintingsList,"AVR") >= 1|
                                      str_count(MTG.df$PrintingsList,"PC2") >= 1|
                                      str_count(MTG.df$PrintingsList,"M13") >= 1|
                                      str_count(MTG.df$PrintingsList,"V12") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDJ") >= 1|
                                      str_count(MTG.df$PrintingsList,"RTR") >= 1|
                                      str_count(MTG.df$PrintingsList,"CM1") >= 1),
                 y2013 = as.numeric(str_count(MTG.df$PrintingsList,"TD2") >= 1|
                                      str_count(MTG.df$PrintingsList,"GTC") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDK") >= 1|
                                      str_count(MTG.df$PrintingsList,"DGM") >= 1|
                                      str_count(MTG.df$PrintingsList,"MMA") >= 1|
                                      str_count(MTG.df$PrintingsList,"M14") >= 1|
                                      str_count(MTG.df$PrintingsList,"V13") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDL") >= 1|
                                      str_count(MTG.df$PrintingsList,"THS") >= 1|
                                      str_count(MTG.df$PrintingsList,"C13") >= 1),
                 y2014 = as.numeric(str_count(MTG.df$PrintingsList,"BNG") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDM") >= 1|
                                      str_count(MTG.df$PrintingsList,"JOU") >= 1|
                                      str_count(MTG.df$PrintingsList,"MD1") >= 1|
                                      str_count(MTG.df$PrintingsList,"CNS") >= 1|
                                      str_count(MTG.df$PrintingsList,"VMA") >= 1|
                                      str_count(MTG.df$PrintingsList,"M15") >= 1|
                                      str_count(MTG.df$PrintingsList,"V14") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDN") >= 1|
                                      str_count(MTG.df$PrintingsList,"KTK") >= 1|
                                      str_count(MTG.df$PrintingsList,"C14") >= 1|
                                      str_count(MTG.df$PrintingsList,"DD3") >= 1),
                 y2015 = as.numeric(str_count(MTG.df$PrintingsList,"FRF") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDO") >= 1|
                                      str_count(MTG.df$PrintingsList,"DTK") >= 1|
                                      str_count(MTG.df$PrintingsList,"TPR") >= 1|
                                      str_count(MTG.df$PrintingsList,"MM2") >= 1|
                                      str_count(MTG.df$PrintingsList,"ORI") >= 1|
                                      str_count(MTG.df$PrintingsList,"V15") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDP") >= 1|
                                      str_count(MTG.df$PrintingsList,"BFZ") >= 1|
                                      str_count(MTG.df$PrintingsList,"EXP") >= 1|
                                      str_count(MTG.df$PrintingsList,"C15") >= 1|
                                      str_count(MTG.df$PrintingsList,"PZ1") >= 1),
                 y2016 = as.numeric(str_count(MTG.df$PrintingsList,"EXP") >= 1|
                                      str_count(MTG.df$PrintingsList,"OGW") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDQ") >= 1|
                                      str_count(MTG.df$PrintingsList,"W16") >= 1|
                                      str_count(MTG.df$PrintingsList,"SOI") >= 1|
                                      str_count(MTG.df$PrintingsList,"EMA") >= 1|
                                      str_count(MTG.df$PrintingsList,"EMN") >= 1|
                                      str_count(MTG.df$PrintingsList,"V16") >= 1|
                                      str_count(MTG.df$PrintingsList,"CN2") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDR") >= 1|
                                      str_count(MTG.df$PrintingsList,"KLD") >= 1|
                                      str_count(MTG.df$PrintingsList,"MPS") >= 1|
                                      str_count(MTG.df$PrintingsList,"PZ2") >= 1|
                                      str_count(MTG.df$PrintingsList,"C16") >= 1|
                                      str_count(MTG.df$PrintingsList,"PCA") >= 1),
                 y2017 = as.numeric(str_count(MTG.df$PrintingsList,"MPS") >= 1|
                                      str_count(MTG.df$PrintingsList,"AER") >= 1|
                                      str_count(MTG.df$PrintingsList,"MM3") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDS") >= 1|
                                      str_count(MTG.df$PrintingsList,"W17") >= 1|
                                      str_count(MTG.df$PrintingsList,"AKH") >= 1|
                                      str_count(MTG.df$PrintingsList,"MPS") >= 1|
                                      str_count(MTG.df$PrintingsList,"CMA") >= 1|
                                      str_count(MTG.df$PrintingsList,"E01") >= 1|
                                      str_count(MTG.df$PrintingsList,"HOU") >= 1|
                                      str_count(MTG.df$PrintingsList,"C17") >= 1|
                                      str_count(MTG.df$PrintingsList,"XLN") >= 1|
                                      str_count(MTG.df$PrintingsList,"DDT") >= 1|
                                      str_count(MTG.df$PrintingsList,"IMA") >= 1|
                                      str_count(MTG.df$PrintingsList,"E02") >= 1|
                                      str_count(MTG.df$PrintingsList,"V17") >= 1|
                                      str_count(MTG.df$PrintingsList,"UST") >= 1),
                 y2018 = as.numeric(str_count(MTG.df$PrintingsList,"RIX") >= 1|
                                      str_count(MTG.df$PrintingsList,"A25") >= 1|
                                      str_count(MTG.df$PrintingsList,"DOM") >= 1|
                                      str_count(MTG.df$PrintingsList,"M19") >= 1))
# No observations in 2018, but we included it anyway

# These five variables were helpful for our data-mining project, sorting cards by color identity.
# They count words int he text-box related to each color.
MTG.df <- mutate(MTG.df, WhiteWords= 0+stri_count(MTG.df$Text,regex="(exile(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(prevent(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(defender(?![^\\(]*\\\\))") + 
stri_count(MTG.df$Text,regex="(attacking or blocking(?![^\\(]*\\\\))") + 
stri_count(MTG.df$Text,regex="(blocking or attacking(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(double strike(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(first strike(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(aura(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(enchantment(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(flying(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(indestructible(?![^\\(]*\\\\))") + 
stri_count(MTG.df$Text,regex="(lifelink(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(protection(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(token(?![^\\(]*\\\\))") +
stri_count(MTG.df$Text,regex="(vigilance(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(destroy all creatures(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(tap target(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(\\-\\d\\/\\+\\d(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(\\+0\\/\\+\\d(?![^\\(]*\\\\))")+
# Mark Rosewater says the above attributes are primary in White
.5*(stri_count(MTG.df$Text,regex="(destroy target artifact(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(return(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(graveyard(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(scry(?![^\\(]*\\\\))")+   
stri_count(MTG.df$Text,regex="(\\+\\d\\/\\+\\d(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(\\+\\d\\/\\+0(?![^\\(]*\\\\))"))+
# Secondary in white; assign them half a word
.15*(stri_count(MTG.df$Text,regex="(each basic land(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(flash(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(hexproof(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(counter(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(reach(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(trample(?![^\\(]*\\\\))")+
stri_count(MTG.df$Text,regex="(\\-\\d\\/\\-\\d(?![^\\(]*\\\\))")))
# Tertiary in White; assign .15 of a word
MTG.df$WhiteWords[is.na(MTG.df$WhiteWords)] <- 0
# The regex (blah(?![^(]*\\)) matches all blah outside parentheses.
# This lets the code ignore 'reminder text,' text in parentheses.
# The regex (\\-\\d\\/\\+\\d(?![^\\(]*\\\\))) matches all -a/+b where a, b are digits
# We performed a similar process with every other color

MTG.df <- mutate(MTG.df, BlueWords= 0+stri_count(MTG.df$Text,regex="(return(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(draw(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(can't be blocked(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(copy(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(counter(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(flash(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(flying(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(under its owner's control(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(doesn't untap during(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(hexproof(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(scry(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(top \\S cards from his or her library into his or her graveyard(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(tap or untap(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(untap(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(\\-\\d\\/\\+\\d(?![^\\(]*\\\\))")+
                   stri_count(MTG.df$Text,regex="(\\+\\d\\/\\-\\d(?![^\\(]*\\\\))")+
                   stri_count(MTG.df$Text,regex="(\\-\\d\\/\\-0(?![^\\(]*\\\\))")+
                   .5*(stri_count(MTG.df$Text,regex="(each basic land(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(token(?![^\\(]*\\\\))") +
                         stri_count(MTG.df$Text,regex="(exile(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(can't be countered(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(defender(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(\\+\\d\\/\\+\\d(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(\\+0\\/\\+\\d(?![^\\(]*\\\\))"))+
                   .15*(stri_count(MTG.df$Text,regex="(indestructible(?![^\\(]*\\\\))")+
                          stri_count(MTG.df$Text,regex="(protection(?![^\\(]*\\\\))")+
                          stri_count(MTG.df$Text,regex="(trample(?![^\\(]*\\\\))")+
                          stri_count(MTG.df$Text,regex="(\\+\\d\\/\\+0(?![^\\(]*\\\\))")+
                          stri_count(MTG.df$Text,regex="(\\-\\d\\/\\-\\d(?![^\\(]*\\\\))"))) 
MTG.df$BlueWords[is.na(MTG.df$BlueWords)] <- 0

MTG.df <- mutate(MTG.df, BlackWords = 0 +
                   stri_count(MTG.df$Text,regex="(sacrifice(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(deathtouch(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(destroy(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(discard(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(gain control(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(whenever another creature dies(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(menace(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(search(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(graveyard(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(\\+\\d\\/\\+\\d(?![^\\(]*\\\\))")+
                   stri_count(MTG.df$Text,regex="(\\-\\d\\/\\-\\d(?![^\\(]*\\\\))")+
                   stri_count(MTG.df$Text,regex="(\\+\\d\\/\\-\\d(?![^\\(]*\\\\))")+
                   .5*(stri_count(MTG.df$Text,regex="(draw(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(token(?![^\\(]*\\\\))") +
                         stri_count(MTG.df$Text,regex="(destroy all creatures(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(defender(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(deal \\d damage(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(flying(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(haste(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(indestructible(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(lifelink(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(scry(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(\\+\\d\\/\\+0(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(\\-\\d\\/\\-0(?![^\\(]*\\\\))"))+
                   .15*(stri_count(MTG.df$Text,regex="(first strike(?![^\\(]*\\\\))")+
                          stri_count(MTG.df$Text,regex="(flash(?![^\\(]*\\\\))")+
                          stri_count(MTG.df$Text,regex="(protection(?![^\\(]*\\\\))")+
                          stri_count(MTG.df$Text,regex="(trample(?![^\\(]*\\\\))")))
MTG.df$BlackWords[is.na(MTG.df$BlackWords)] <- 0

MTG.df <- mutate(MTG.df, RedWords = 0 +
                   stri_count(MTG.df$Text,regex="(can't be countered(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(copy(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(deal \\d damage(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(deal damage(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(double strike(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(first strike(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(combat phase(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(haste(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(menace(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(if able(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(must(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(destroy target artifact(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(destroy target land(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(tap target land(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(random(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(attack(?![^\\(]*\\\\))")+
                   stri_count(MTG.df$Text,regex="(\\+\\d\\/\\+0(?![^\\(]*\\\\))")+
                   .5*(stri_count(MTG.df$Text,regex="(token(?![^\\(]*\\\\))") +
                         stri_count(MTG.df$Text,regex="(return(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(can't block(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(draw(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(defender(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(flying(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(prowess(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(reach(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(scry(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(\\+\\d\\/\\+\\d(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(\\+\\d\\/\\-\\d(?![^\\(]*\\\\))"))+
                   .15*(
                     stri_count(MTG.df$Text,regex="(indestructible(?![^\\(]*\\\\))")+
                       stri_count(MTG.df$Text,regex="(flash(?![^\\(]*\\\\))")+
                       stri_count(MTG.df$Text,regex="(protection(?![^\\(]*\\\\))")+
                       stri_count(MTG.df$Text,regex="(\\-\\d\\/\\-\\d(?![^\\(]*\\\\))")))
MTG.df$RedWords[is.na(MTG.df$RedWords)] <- 0

MTG.df <- mutate(MTG.df, GreenWords = 0+
                   stri_count(MTG.df$Text,regex="(can't be countered(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(destroy target artifact(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(destroy target enchantment(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(destroy target creature with flying(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(fight(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(deals damage equal to its power(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(reach(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(trample(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(each creature(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(forest(?![^\\(]*\\\\))") +
                   stri_count(MTG.df$Text,regex="(each land(?![^\\(]*\\\\))") + 
                   stri_count(MTG.df$Text,regex="(token(?![^\\(]*\\\\))")+
                   stri_count(MTG.df$Text,regex="(\\+\\d\\/\\+\\d(?![^\\(]*\\\\))") + 
                   .5*(stri_count(MTG.df$Text,regex="(exile(?![^\\(]*\\\\))") +
                         stri_count(MTG.df$Text,regex="(draw(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(deathtouch(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(defender(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(flash(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(must block(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(hexproof(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(indestructible(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(vigilance(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(scry(?![^\\(]*\\\\))")+   
                         stri_count(MTG.df$Text,regex="(\\+0\\/\\+\\d(?![^\\(]*\\\\))")+
                         stri_count(MTG.df$Text,regex="(\\+\\d\\/\\+0(?![^\\(]*\\\\))"))+
                   .15*(
                     stri_count(MTG.df$Text,regex="(flying(?![^\\(]*\\\\))")+
                       stri_count(MTG.df$Text,regex="(haste(?![^\\(]*\\\\))")+
                       stri_count(MTG.df$Text,regex="(protection(?![^\\(]*\\\\))")+
                       stri_count(MTG.df$Text,regex="(\\-\\d\\/\\-\\d(?![^\\(]*\\\\))")))
MTG.df$GreenWords[is.na(MTG.df$GreenWords)] <- 0


# The dataframe is complete, with the name MTG.df.
# Here are some quick plots to show what is possible using the data:

powerPlot <- qplot(Power,Toughness,data=MTG.df) + geom_count() + scale_fill_discrete(drop=FALSE) +
     scale_x_discrete(drop=FALSE) + scale_size_continuous(range = c(0, 7),trans="log10") +
     geom_abline(slope=1,color="Red")
powerPlot
# Plot all cards power/toughness

MTG.simple <- filter(MTG.df,Power %in% c("","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15") 
                          & Toughness %in% c("","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15") 
                          & ConvertedManaCost %in% c(0:25))
MTG.simple <- mutate(MTG.simple,Power=factor(Power,levels=c("","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")))
MTG.simple <- mutate(MTG.simple,Toughness=factor(Toughness,levels=c("","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")))

powerPlot2 <- qplot(Power,Toughness,data=MTG.simple) + geom_count() +
     scale_colour_gradientn(colours=heat.colors(2)) + scale_fill_discrete(drop=FALSE)+ 
     scale_x_discrete(drop=FALSE) + scale_size_continuous(range = c(1, 9),trans="log10")+ 
     geom_abline(slope=1,color="Red",alpha=.25)
powerPlot2
# Same as above but no finicky powers and toughnesses

log10sqr_trans <- trans_new(name="log10sqr_trans",
          transform = function(x){return(log10(x)**2)},
          inverse = function(x){return(10**(x**(1/2)))},
          domain=c(0.001,Inf))

powerPlot3 <- qplot(Power,Toughness,data=MTG.simple) + geom_count(aes(color=ConvertedManaCost)) + 
     scale_colour_viridis() + scale_fill_discrete(drop=FALSE) +
     scale_x_discrete(drop=FALSE) + scale_size(breaks=c(1,3,10,33,100,333,1000,3333),range=c(1.3,12),trans=log10sqr_trans) +
     geom_abline(slope=1,color="Red",alpha=.25) +
     ggtitle("Magic: The Gathering Cards with simple P/T by Mana Cost")
powerPlot3
# Add color for CMC