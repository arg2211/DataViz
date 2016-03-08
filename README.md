# DataViz
Contains G4063 assignments and data visualizations.

## *Assignment 1*

#### Big Graph
D3 visualization of roughly 5 hours of tweets collected about presidential candidates. Main goal is to demonstrate differences in frequency of tweets about each candidate in twenty-minute intervals.

#### Compare Bernie to Hillary
D3-created histogram of number of tweets about Bernie Sanders that transitions to number of tweets about Hillary Clinton.  Main goal is to compare frequency of tweets about both candidates in twenty-minute intervals.

#### Compare Cruz to Rubio
Histogram with same function as above but tweets about Cruz then transition to tweets about Rubio.

#### Shiny App
This R Shiny app uses 30 minutes of tweets that I collected about ten presidential candidates to create a tweet frequency plot and a word cloud; you can select one candidate to view at a time.  For the frequency plot, move the slider bar to change the number of bins in the plot. The word cloud shows a maximum of 250 words that were most commonly repeated in the tweets about the chosen candidate. 

## *Assignment 2*

#### Force Layout - All Candidates
Created a social network visualization using a  D3 force layout. Data are tweets collected over ~5 hours about four presidential candidates: Hillary Clinton, Ted Cruz, Bernie Sanders, and Donald Trump. Nodes represent Twitter users and directed edges represent mentions.

#### Force Layout Curvy - Specific Candidate
Same as above except tweets were collected for ~30 minutes about datasets are split into separate groups for each candidate.

#### Gephi
Contains same four tweet mention networks as above, where each network represents Twitter users talking about a certain presidential candidate. The size of the node grows and the color of the node darkens as in-degree calculation increases.  Thus, users with the largest and darkest nodes are mentioned the most.  Additionally, the text size of the Twitter user labels gets bigger as the out-degree calculation increases; the largest Twitter handles tweet the most about the given candidate.  Screenshots of each network are also included.

#### Bonus
Same as force layout curvy visualizations but edges are straight lines.
