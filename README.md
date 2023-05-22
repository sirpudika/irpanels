# Hi there! 🌳
`irpanels` is a little package to help you wrangle with and visualize the data from the [Swiss Environmental Panel](https://istp.ethz.ch/research/sep.html) and the [Swiss Mobility Panel](https://istp.ethz.ch/research/swiss-mobility-panel.html). This repository is a fork from the [`sep` package](https://github.com/bonschorno/sep) and extends its functionality to data from the Swiss Mobility Panel.

The **latest update** was on *May 23, 2023*. You can download and update the package using `devtools::install_github("sirpudika/irpanels")`. For the package to work, you must also install the package pollster (using `install.packages("pollster")`).

## How does `irpanels` help me?

`irpanels` helps collaborators to automate the data analysis, the visualizations for the survey reports, and the codebook compilation. For the exploratory data analysis, the `eda_*` functions contain the most common operations. The `plot_*` functions, on the other hand, simplify visualizations once the data has been cleaned. Finally, the `cb_*` functions help to partially automate the creation of our codebooks.

To help you get started, we created a vignette for the [visualization](https://sirpudika.github.io/irpanels/doc/Walk-through.html) and the [codebook](https://sirpudika.github.io/irpanels/doc/codebook_walkthrough.pdf) functions.

## What is the Swiss Environmental Panel? 

The Swiss Environmental Panel is based on a random sample of the Swiss population over 15 years of age from the harmonised population register of the Federal Statistical Office (FSO/SRPH). Included are also foreign nationals residing in Switzerland. Respondents are invited to participate by letter in the main language of their municipality. The survey can be answered on paper or electronically in German, French or Italian. In order to be able to record developments over time, some questions are taken over from similar surveys in 1994 and 2007 ("Swiss Environmental Survey" by Andreas Diekmann and co-workers). The project is designed as a panel with short and long surveys, i.e. the same people will be surveyed several times over the next few years. Two surveys are conducted per year. For example, you can find our report on 5G in Switzerland [here](https://ethz.ch/content/dam/ethz/special-interest/dual/istp-dam/documents/ISTP/Research/SEP/de/Welle%205_Umweltpanel_Ergebnisbericht_DE.pdf).

## What is the Swiss Mobility Panel?

The Swiss Mobility Panel explores public opinion dynamics in Switzerland in relation to mobility preferences and behavior, changes therein, and linkages to preferences and behavior in other policy realms.

Around 9,500 randomly selected individuals participate in the Swiss Mobility Panel and answer these and related questions. Having started in October 2020, the Swiss Mobility Panel is an ongoing research project, a collaboration between the research group International Political Economy and Environmental Politics and the Institute for Transport Planning and Systems at the Institute of Science, Technology and Policy of ETH Zurich. Results and data can be found [here](https://istp.ethz.ch/research/swiss-mobility-panel.html#Results).
