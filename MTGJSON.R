# install.packages("tidyverse")       # Installation only necessary on the first run
# install.packages("rjson")
# install.packages("gtools")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("magick")


library(tidyverse)
library(rjson)      # Parse JSON
library(gtools)   
library(stringr)    # Interact with strings
library(ggplot2)    # Nice plots
library(magick)     # Image manipulation

setwd("/Users/Theodore/Desktop/R_Studio_Stuff/data")      # Change this to your own working directory
MTG.json <- fromJSON(file="AllCards.json",method='C')     # JSON file from https://mtgjson.com (not great for R)


rows=17761    # When testing, decrease this so you don't use the whole data-set every time

MTG.df <- data.frame(matrix(ncol=22,nrow=rows))   # Currently tracking 22 variables

# The following function is vital to parsing JSON. 
# Nested lists and inconsistant variable types require it for producing consistant row-lengths.
# The argument "json" is a JSON object.
# "x" is the position in the JSON list to open; each will be a Matgic: The Gathering card.
# "string" is the name of the variable to look for in the JSON element x.
# "otherwise" is what the program should return if the variable doesn't exist; sometimes "", sometimes NA.
# "after" is the nested list index in case we need to check elements of a nested list which may not exist.

isItThere <- function(json,x,string,otherwise,after="") {
  if(after!="") {if(is.null(json[[x]][[string]][after])) {return(otherwise)} else
  {return(json[[x]][[string]][after])}} else
    if(is.null(json[[x]][[string]])) {return(otherwise)} else {return(json[[x]][[string]])}}

sapply(1:rows, function(x) {MTG.df[x,] <<- c(MTG.json[[x]][["name"]],
                                             isItThere(MTG.json,x,"manaCost",0),
                                             MTG.json[[x]][["cmc"]],
                                             str_count(isItThere(MTG.json,x,"manaCost",""),"W"),
                                             str_count(isItThere(MTG.json,x,"manaCost",""),"U"),
                                             str_count(isItThere(MTG.json,x,"manaCost",""),"B"),
                                             str_count(isItThere(MTG.json,x,"manaCost",""),"R"),
                                             str_count(isItThere(MTG.json,x,"manaCost",""),"G"),
                                             isItThere(MTG.json,x,"supertypes",NA,1),
                                             isItThere(MTG.json,x,"supertypes",NA,2),
                                             isItThere(MTG.json,x,"types",NA,1),
                                             isItThere(MTG.json,x,"types",NA,2),
                                             isItThere(MTG.json,x,"subtypes",NA,1),
                                             isItThere(MTG.json,x,"subtypes",NA,2),
                                             isItThere(MTG.json,x,"text",NA),
                                             nchar(isItThere(MTG.json,x,"text","")),
                                             isItThere(MTG.json,x,"power",NA),
                                             isItThere(MTG.json,x,"toughness",NA),
                                             isItThere(MTG.json,x,"loyalty",NA),
                                             length(MTG.json[[x]][["rulings"]]),
                                             length(MTG.json[[x]][["printings"]]),
                                             length(MTG.json[[x]][["legalities"]]))})


MTG.df <- setNames(MTG.df,c("Name",                 # Character; MTG card name
                            "ManaCost",             # Character; Mana Cost in typical online format
                            "ConvertedManaCost",    # Integer; number of mana required to use
                            "White",                # Integer; number of white mana required
                            "Blue",                 # Integer; number of blue mana required
                            "Black",                # Etc
                            "Red", 
                            "Green", 
                            "Supertype",            # Factor; only sometimes exists
                            "Supertype2",           # Factor; only sometimes exists
                            "Type",                 # Factor; usually exists
                            "Type2",                # Factor; only sometimes exists
                            "Subtype",              # Factor; only sometimes exists
                            "Subtype2",             # Factor; only sometimes exists
                            "Text",                 # Character; Rules text, not flavor text
                            "TextLength",           # Integer; number of characters in Text
                            "Power",                # Often an integer, sometimes a character
                            "Toughness",            # Often an integer, sometimes a character
                            "Loyalty",              # Integer; for Planeswalkers
                            "Rulings",              # Number of rulings
                            "Printings",            # Number of printings
                            "Legalities"))          # Number of legalities
MTG.df <- transmute(MTG.df, Name=as.character(Name),
                    ManaCost=as.character(ManaCost),
                    ConvertedManaCost=as.integer(ConvertedManaCost),
                    White=as.integer(White),
                    Blue=as.integer(Blue),
                    Black=as.integer(Black),
                    Red=as.character(Red),
                    Green=as.integer(Green),
                    Supertype=as.factor(Supertype),
                    Supertype2=as.factor(Supertype),
                    Type=as.factor(Type),
                    Type2=as.factor(Type2),
                    Subtype=as.factor(Subtype),
                    Subtype2=as.factor(Subtype2),
                    Text=as.character(Text),
                    TextLength=as.integer(TextLength),
                    Power=as.character(Power),         # These should be integers, but *s are weird
                    Toughness=as.character(Toughness),
                    Loyalty=as.integer(Loyalty),
                    Rulings=as.integer(Rulings),
                    Printings=as.integer(Printings),
                    Legalities=as.integer(Legalities))

plotAll <- ggplot(MTG.df[-125,],aes(x=ConvertedManaCost,y=TextLength),na.rm=TRUE) + 
  geom_count(shape=1,alpha=.3) +       # Count-plot of all cards, CMC by rules-text-length
  geom_smooth(method="glm",col="Red",se=TRUE) +    # Add a general linear regression line; not terribly helpful
  xlim(0,16) + ylim(0,770) +           # Consistant axes
  ggtitle("All Cards")                 # Title

MTG.White <- filter(MTG.df,White >= 1) # Subsets of MTG.df by color
MTG.Blue <- filter(MTG.df,Blue >= 1)
MTG.Black <- filter(MTG.df,Black >= 1) # I realize some cards' color identities may not be totally accurate
MTG.Red <- filter(MTG.df,Red >= 1)     
MTG.Green <- filter(MTG.df,Green >= 1) # I'll fix it later, maybe

plotWhite <- ggplot(MTG.White,aes(x=ConvertedManaCost,y=TextLength)) + 
  geom_count(shape=1,color="darkgray",alpha=.3) + xlim(0,16) + ylim(0,770) +
  ggtitle("White Cards")

plotBlue <- ggplot(MTG.Blue,aes(x=ConvertedManaCost,y=TextLength)) + 
  geom_count(shape=1,color="blue",alpha=.3) + xlim(0,16) + ylim(0,770)+
  ggtitle("Blue Cards")

plotBlack <- ggplot(MTG.Black,aes(x=ConvertedManaCost,y=TextLength)) + 
  geom_count(shape=1,color="black",alpha=.3) + xlim(0,16) + ylim(0,770)+
  ggtitle("Black Cards")

plotRed <- ggplot(MTG.Red,aes(x=ConvertedManaCost,y=TextLength)) + 
  geom_count(shape=1,color="red",alpha=.3) + xlim(0,16) + ylim(0,770)+
  ggtitle("Red Cards")

plotGreen <- ggplot(MTG.Green,aes(x=ConvertedManaCost,y=TextLength)) + 
  geom_count(shape=1,color="darkgreen",alpha=.3) + xlim(0,16) + ylim(0,770)+
  ggtitle("Green Cards")

MTG.creature <- filter(MTG.df, Type == "Creature")             # Subsets based on typing
MTG.spells <- filter(MTG.df, Type %in% c("Instant","Sorcery")) # Including enchantments is difficult

plotCreature <- ggplot(MTG.creature,aes(x=ConvertedManaCost,y=TextLength)) + 
  geom_count(shape=1,color="brown",alpha=.3) + xlim(0,16) + ylim(0,770) +
  ggtitle("Creatures")

plotNonCreature <- ggplot(MTG.spells,aes(x=ConvertedManaCost,y=TextLength,color=Type)) + 
  geom_count(shape=1,alpha=.7) + xlim(0,16) + ylim(0,770) +
  ggtitle("Non-Creatures")

setwd("/Users/Theodore/Desktop/R_Studio_Stuff/RStudio_Images") # Change this to where you'd like to save images
png("AllCards.png")    # Makes a png object to be saved
plotAll                # Plot to add to png
dev.off()              # Clear png to do another

png("WhiteCards.png")
plotWhite
dev.off()

png("BlueCards.png")
plotBlue
dev.off()

png("BlackCards.png")
plotBlack
dev.off()

png("RedCards.png")
plotRed
dev.off()

png("GreenCards.png")
plotGreen
dev.off()

png("CreatureCards.png")
plotCreature
dev.off()

png("NonCreatureCards.png")
plotNonCreature
dev.off()

magicAll <- image_read("AllCards.png")        # Read in each image for more manipulation
magicWhite <- image_read("WhiteCards.png")
magicBlue <- image_read("BlueCards.png")
magicBlack <- image_read("BlackCards.png")
magicRed <- image_read("RedCards.png")
magicGreen <- image_read("GreenCards.png")
magicCreature <- image_read("CreatureCards.png")
magicNonCreature <- image_read("NonCreatureCards.png")

magicColorComp <- c(magicAll,magicWhite,magicBlue,magicBlack,magicRed,magicGreen)  # Vector of images
magicColorComp <- image_animate(magicColorComp,fps=.5,dispose="previous")          # Animate

image_write(magicColorComp,"Magic-Color-Comp.gif")                                 # Save animated gif