
# ceg
chain-event graph for R. This package implements the theory of CEG.
You will be able to generate CEG objects from manual input or from formated data

For the theory about CEG, please see https://www.researchgate.net/project/Chain-Event-Graphs
This site presents some papers related to CEG.

Additionally,  see https://www.crcpress.com/Chain-Event-Graphs/Collazo-Goergen-Smith/p/book/9781498729604

About the code itself, I have some observations:
Rodrigo Collazo provided me with his original code, developed for testing the theory and not for 
general public. I designed an object-oriented software architecture and wrapped his original 
code in S4 classes. 

The final result is operational, but have a lot of space for improvements. I devise the need of
-- substituting part of the code for existing R functions, 
-- use a better data structure to represent the graphs
-- move from Rgraphviz to ggplot2 extensions
-- change part of the code that manipulates data to tidyverse functions, to improve code readability 

We also should provide a vignette to help possible users. 

Please, feel free to contact us for any critical modification needed to help your research or work.

Cheers
Pier Taranti
