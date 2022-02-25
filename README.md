# Hi there! 🌳
`sep` is a little package to help you wrangle with and visualize the data from the Swiss Environmental Panel. 

## What is the Swiss Environmental Panel? 

The Swiss Environmental Panel is based on a random sample of the Swiss population over 15 years of age from the harmonised population register of the Federal Statistical Office (FSO/SRPH). Included are also foreign nationals residing in Switzerland. Respondents are invited to participate by letter in the main language of their municipality. The survey can be answered on paper or electronically in German, French or Italian. In order to be able to record developments over time, some questions are taken over from similar surveys in 1994 and 2007 ("Swiss Environmental Survey" by Andreas Diekmann and co-workers). The project is designed as a panel with short and long surveys, i.e. the same people will be surveyed several times over the next few years. Two surveys are conducted per year. You can find our latest report on 5G in Switzerland [here](https://ethz.ch/content/dam/ethz/special-interest/dual/istp-dam/documents/ISTP/Research/SEP/de/Welle%205_Umweltpanel_Ergebnisbericht_DE.pdf).

## How does `sep` help me?

`sep` helps collaborators to automate the data analysis, the visualisations for the survey reports, and the codebook compliation. For the exploratory data analysis, the `eda_*` functions contain the most common operations. The `plot_*` functions, on the other hand, simplify visualisations once the data has been cleaned. Finally, the `cb_*` functions help to partially automate the creation of our codebooks.

To help you get started, we created a vignette for the [visualization](https://bonschorno.github.io/sep/doc/Walk-through.html) and the [codebook](https://bonschorno.github.io/sep/doc/codebook_walkthrough.pdf) functions.
