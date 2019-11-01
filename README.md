# Applied Analytics "Winter Pressures" Project
> Modelling an entire hospital using r-simmer and routinely-collected data



Based on techniques used in [http://futurehospital.rcpjournal.org/content/6/1/17.abstract]

[Winter Pressures Modelling Project](https://www.health.org.uk/improvement-projects/use-of-novel-modelling-techniques-and-routinely-collected-data-to-explore-responses-to)

Article describing this specific model in process.

## Contributors

* **Tom Lawton** – [@LawtonTri](https://twitter.com/lawtontri)
* **Michael McCooe**
* **Dan Mason**
* **Abigail Dutton**

## Licence

Distributed under the [GNU GPL 3.0](https://www.gnu.org/licenses/gpl-3.0.en.html)

Dual-licensed under the [Community Research and Academic Programming License](http://matt.might.net/articles/crapl/)

In the event of any conflicts between the two, the GPL takes precedence

## Installation/Use

This is proof-of-concept code and is based on Bradford Royal Infirmary. The intention is to show that modelling of this scale, using routinely-available data sources, is possible, and to provide code showing how it might be done. As-is, it is not general-purpose enough to work elsewhere.

However, it should in theory be possible to run it in your own hospital, but should only be undertaken by someone who understands the code as it will need modifying in places. The Bradford-centric parts have mostly been pulled out into separate files.

You will need:

* **SUS Data** - specifically `Admitted Patient Care (CDS V6-2 Type 130)`
* **Bed management info** - a way of mapping SUS patient episodes into a list of desired locations for the patient in priority order (in the model this is mostly done on `Treatment Function Code`)
* **Ward data** - ward capacities, surge capacities, and details of how decisions are made to open/close beds

The only important directories are the ones numbered *1*, *2*, and *5*. Others are for development purposes only and have been left here for learning purposes.

SUS data feeds directly into the code in directory 1 - you only need `1 - SUS v2 data transformation TL4.R`

Trajectory identification needs to be put into `3 SUS v2-byEpisode data to generate trajectory codes.R` in directory 2

Trajectory ward priorities go in `Trajectories.R` in directory 5

Wards go in `Wards.csv` in the same directory.

Opening/closure of wards/surge capacity is in the model itself - see `WardController.R`

Critical care is also dealt with in the model itself as BRI has a rather complex setup with a "real" critical care (known as "ICU" in the model), and part of another ward (21) which appears as Critical Care in the SUS data but isn't really. Your hospital (hopefully) avoids this and can have the relevant code removed.

## Disclaimer

If the licensing above didn't already make it clear, this code is proof-of-concept level. Specifically, the author was an imperative programmer with no R experience at the start of the project and you'll see his slow development into a more functional one over the course of the code. Things have not always been done in the best (or most "R") manner.

If you wish to learn about this technique, there is a [repository](https://github.com/thigger/ICU-Model) designed for the purpose. The code there is much cleaner and easier to understand. It aims to simulate a single ward, with tick-rate in days rather than seconds, but the technique is otherwise identical. It is being run as a workshop at the [NHS-R community's](https://nhsrcommunity.com/) conference and we hope there will be more teaching available in the future.

## Acknowledgements

![Health Foundation Logo](https://www.health.org.uk/themes/custom/health_foundation/assets/images/logo.png)

The [Winter Pressures Modelling Project](https://www.health.org.uk/improvement-projects/use-of-novel-modelling-techniques-and-routinely-collected-data-to-explore-responses-to) is part of the Health Foundation’s Applied Analytics programme. The Health Foundation is an independent charity committed to bringing about better health and health care for people in the UK.

Massive thanks to Iñaki Ucar, creator of r-simmer, for his support of the work - both in terms of advice and modifications to r-simmer to make it more suitable for this type of work.

Thanks also to John Birkinshaw, Kuldeep Sohal, Naeem Sheikh, Claire Chadwick, Michael Rooney, Sharon Thorley, James Taylor, Sarah Cooper, Brad Wilson, Rob Collin, and the Yorkshire & Humber PSTRC's Digital Innovation theme.

![Y&H PSTRC Logo](https://yhpstrc.org/wp-content/uploads/2019/03/nihr-new.jpg) ![Improvement Academy Logo](https://www.improvementacademy.org/images/logos/IA_logo_July18.png)