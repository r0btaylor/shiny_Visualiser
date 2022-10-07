# shiny_Visulaiser
A shiny web application that enables non-technical users to access and visualise data using R commands on the application backend

## 
This tool is designed to provide users from the humanitarian sector with an accessible platform for processing and interrogating data from the 2022 Teknaf socioeconomic survey.     

The Shiny package for R is used to create a graphical user interface which enables users to run pre-defined R script on the application backend.    

Key features include:    
* Completely automated data processing and cleansing.     
* Generation of summary statistics.    
* Automated plotting using 3 chart types.    
* Interactive geospatial data visualisation.

## Feature Walkthrough
A video walthrough of important application features can be accessed at:    
https://youtu.be/5PkidWLumo8

## Demo App
A live demo version of the application can be accessed at the following link:    
https://hwisevis.shinyapps.io/hwiseVis/    
**Note: The demo app requires no file download or installation. Demonstration data is automatically imported upon application loading.**    

## Installation    
**The application runs in an R environment and requires R and RStudio to be installed locally.**
1. Clone/download the repo to your local drive. **Keep the directory structure intact** Unzip if required.    
2. Open the appFiles directory.
3. Run the 'runMe.R' script to launch the application script in RStudio.
4. Click 'RunApp' from the top right of the main pane.
5. All required packages and dependencies are installed. **This may take some time on first run**

## Usage
* Data is supplied to the application in the form of a locally stored .xlsx file    
* A demo dataset has been provided within the application files:    
    **Path:**  appFiles\hwiseVis\data\demo_data.xlsx    
    **Password:** password
    
## Notable packages used

**Application & UI:** Chang, W. et al. (2022) shiny: Web Application Framework for R [online].  Available from: https://CRAN.R-project.org/package=shiny.    

**UI widgets:** Perrier, V., Meyer, F. and Granjon, D. (2022) shinyWidgets: Custom Inputs Widgets for Shiny [online].  Available from: https://CRAN.R-project.org/package=shinyWidgets.    

**Data handling:** Wickham, H., François, R., Henry, L. and Müller, K. (2022) dplyr: A Grammar of Data Manipulation [online].  Available from: https://CRAN.R-project.org/package=dplyr.    

**Summary statistics:** Waring, E., Quinn, M., McNamara, A., Rubia, E.A. de la, Zhu, H. and Ellis, S. (2022) skimr: Compact and Flexible Summaries of Data [online].  Available from: https://CRAN.R-project.org/package=skimr.    

**Graphing:** Wickham, H. (2016) ggplot2: Elegant Graphics for Data Analysis [online].  Springer-Verlag New York. Available from: https://ggplot2.tidyverse.org. 

**Interactive plots:** Sievert, C. (2020) Interactive Web-Based Data Visualization with R, plotly, and shiny [online].  Chapman and Hall/CRC. Available from: https://plotly-r.com.    

**Geospatial visualisation:** Cheng, J., Karambelkar, B. and Xie, Y. (2022) leaflet: Create Interactive Web Maps with the JavaScript ‘Leaflet’ Library [online].  Available from: https://CRAN.R-project.org/package=leaflet. 

## License
MIT License    

Copyright (c) [2022] [Robert Taylor]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
