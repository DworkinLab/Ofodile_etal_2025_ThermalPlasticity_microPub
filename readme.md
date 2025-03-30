# README 

This repository contains the data and scripts to replicate the analysis of the following study:

Ofodile, Lotachukwu., Appenteng, Judith., Jaffri, Mubeen., Dworkin, Ian. & Stewart, Andrew D. 2025. No degradation of temperature-mediated phenotypic plasticity in *Drosophila melanogaster* after more than 275 generations of artificial
selection on body-size. microPublication BIOLOGY. Accepted.

## summary of experimental variables
Image_ID - image file identifier, removed from csv.
Treat -  what body-size treatment the flies come from,
Temp - what temperature the flies were reared at
Rep -  which replicate block (Rep) of the experiment was done (the project was done in two replicate blocks).
Vial - replicate vial within block

 Note - only 1 of the 2 evolutionary replicate lineage within each treatment was measured in this experiment.


## email from Andrew to Ian, Nov 5th 2020

In brief, the students (Lota and Judith, hence the name: “Lotith”) reared the body-size treatments at three different temperatures and measured the thorax length of males and females at each temperature. Below is a VERY rough beginning of a manuscript that I started three years ago, but should give you an idea of the experimental design. (note from ID, manuscript draft in manuscript folder)


Here is the Excel data sheet that I assembled from the student-collected data. There are two tabs. The first is the complete data set, where I included the results of the “experimental” lines (opposite size-selection in males and females). The second tab has that data removed. At the time, I hadn’t published anything about the experimental lines, and I didn’t want to scoop myself, so I took those out, thinking that I might publish this paper first. Obviously that didn’t happen, so, if the data is interesting, we can keep it in, but I haven’t spent much time playing with it. As for the spreadsheet, the ID column is the (theoretical) name of the image file; it’s only entered for the second replicate, as the student that was in charge of data entry for the first replicate did not provide it, but the file structure is the same. I think the rest of column ID’s are self-explainitory- what body-size treatment (Treat)  the flies come from, what temperature the flies were reared at (Temp) and which replicate (Rep) of the experiment was done (the project was done in two replicate blocks). Within each block X treatment, three replicate vials (Vial 1, 2, or 3) were created. From each vial, the goal was to measure the thorax of 10 males and 10 females, but you will see that it’s not perfect, with some uneven samples across the experiment. In addition to the Body-Size lines (including the control), I had the students also test an external control by using LHm flies from the main lab population (from which the body-size were derived many, many moons ago). The idea was that this might allow to account for any changes in plasticity due to Ne/drift/sieving effects. To that end, I created what I called “Delta”, which is simply the thorax length of an individual, minus the average thorax length of LHm (separated by sex) from that block. Then to convert that from a linear scale (mm, I believe) everything, I also then divided that deviation by the LHm average, to get an idea of what percent change in size was seen (%Delta). These may or may not be informative ways of looking at things, but they make intuitive sense to me. Finally, I have to point out, for experiment tractability (i.e. sample size) that undergrads could handle, we had to limit this experiment to 1 of the two replicate lines from the main body-size experiment (Line 1, in all cases), so we can’t show universality for the BodySize treatments.



So, way back “then” my colleague (Katie Costanzo) who lives in the world of phenotypic plasticity in mosquitos, did me a huge solid by running some stats on them. I am attaching the SAS outputs she gave me, in case they inform what you might do. She was very clear to point out that she struggles to stay up-to-date as to exactly what the current accepted stats methods are when she publishes, so these might not have been in vogue three years ago, let along today. There are a lot of files here, so may be more confusing that helpful. One of the things I thought would be interesting to look at is not just if there is plasticity, but rather, are the slopes of the plasticity lines the same or different, so some of these analyses attempt to explore that.
