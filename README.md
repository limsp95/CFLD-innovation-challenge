# CFLD-innovation-challenge

### THEME
Innovative data driven decision making in land evaluation.

### BUSINESS CHALLENGE
The student teams are challenged to combine innovative data sourcing and creative machine learning application to evaluate raw land prices and its potential growth in at least one of the following cities: Kuala Lumpur, New Delhi, Mumbai, Manila and Johor Bahru.

### SOLUTION OUTLINE
Chosen city of analysis: Kuala Lumpur (KL)

1. The greater KL area is divided into 17 areas, Ampang Jaya, Balakong, Batu Arang, Beranang, Bukit Tinggi, Kajang, Klang, Pengkalan, Petaling Jaya, Rawang, Selayang Baru, Semenyih, Serendah, Shah Alam, Subang Jaya	
2. Data collection using scrapping. Property and land price data were scrapped from https://www.brickz.my/
3. Address from scrapped data is converted into geo-coordinates which are fed into a Google maps API to return 
  a) the number of bus stops, trian stations, convenient stores, industry within a fixed km radius and 
  b) the nearest schools, convenient stores, industry etc., in distance
  c) distance to KL city center and airport
4. Area level data on population, consumer price index, etc. are gathered.
5. Data from different sources are merged and cleaned.
6. Build models and select models based on accuracy and interpretability. A random forest model was selected as the final model. The variable importnce plots tells us the relative importance of different factors and a partial depenance plot tells us how it affects the property price.
7. Profits = Predcted property price (future) - land price (current)  &  ROI = (investment gain - investment cost)/(investment cost)
8. Predict population in 2023 based on time series.
9. Put predicted property back into property price model
10. Make recommendations based on profit and ROI. Supplement recommendation with research and white papers regardng invest KL


This project was done in collaboration with Cho Zin Tun, Tan Yan Zhou, Vinod Vijayakumaran, Wang Jia (Claire), Wang Shenghao.
The team placed 4th out of 18 teams in the final judging.

