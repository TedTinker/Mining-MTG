# Mining-MTG
A JSON file of all Magic: The Gathering Cards is available here: https://mtgjson.com. Parsing it is a challenge, but
I've managed to pull loads of info from it. In my DataMining repository, the final.RMD and final.PDF files 
were made with a partner for a class and explore the data with predictive models. 
In this repository I've collected some extra graphs and visualizations. 

Run the R script to play with the data yourself:

Download the JSON file from https://mtgjson.com (I used All Cards + Extras), then change MTG4reddit.R so the lines

setwd("/Users/Theodore/Desktop/R_Studio_Stuff/data")  
MTG.json <- fromJSON(file="AllCards.json",method='C') 

point to that JSON file on your computer. Then you can run the script in RStudio and play with the dataframe yourself!

Copywrite:
I don't own Hasbro or Wizards of the Coast or Magic: The Gathering, but I'd sure like to.
The R script is freeware. 
