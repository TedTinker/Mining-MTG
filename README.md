# Mining-MTG
A JSON file of all Magic: The Gathering Cards is available here: https://mtgjson.com. Parsing it is a challenge, but
I've managed to pull some interesting information from it. I've started by making an animation comparing the relationship
between a card's color, its Converted Mana Cost, and its rules-text-length. In the future I'd like to measure more variables 
(like detecting words of interest in the rules-text) and make a tool which predicts CMC from other variables,
or color from other variables.

I think it's fun observing the data for elements of MTG's game-design philosophy. For example, compare the CMC distribution
of 0-text-length cards for each color; how often does each color have vanilla creatures of each CMC? How does that relate
to the MTG color-wheel? 
