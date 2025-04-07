# FootyClustR ⚽📊  
_A Data-Driven Approach to Football Player Scouting Using R_

## 📌 Overview  
At the end of the 2023/24 football season, AS Roma's manager De Rossi discussed the upcoming summer transfer market and mentioned he would follow the Atalanta recruitment model, and would seek players with pace such as Jeremie Frimpong. Inspired by this real-life scenario, this project applies **clustering and data-driven analysis** to identify football players similar to a reference player (**Jeremie Frimpong**), who meet the selected age and market value attributes. The model is built using **R**, incorporating **K-means clustering, data visualization, and statistical analysis**.

## 🚀 Key Features  
✔ **Player Clustering**: Identifies similar players using selected KPIs (possession KPIs only in this case) 
✔ **Machine Learning Methods**: K-means clustering applied to football scouting  
✔ **Advanced Visualizations**: Cluster plots, radar charts and bar rankings for insights  
✔ **Market Value Insights**: Filters players based on transfer value  

## 📊 Data & Methodology  
- Identification of the **business problem**: the Atalanta model and the need for players similar to Frimpong
- Conversion into an **analytical problem**: age and market value constraints, identification of relevant KPIs based on academic definitions (Hughes et al., 2012)
- KPIs are **scaled and normalized** before clustering using **K-means**.  
- A **ranking system** was created using an **Aggregate Score** combining selected attributes.  
- Visualizations include **cluster plots, radar charts, and ranking bar charts**. 

## 🔎 Visual Examples  
### 📌 Clustered Players Example  
![2nd k_means clustering](https://github.com/user-attachments/assets/91a1f215-2e95-4cc0-bac8-2d93ecfdc86d)

### 📌 Bar Chart Example  
![Top10](https://github.com/user-attachments/assets/5b4d0a0f-a802-4412-8c9c-eb57d0e97b7a)

### 📌 Radar Chart Comparison  
![Frimpong_Steuckers](https://github.com/user-attachments/assets/c3a5182e-be35-4bd9-88ef-b669328a7f36)

## 🏆 Results and insights
- 4 potential candidates shortlisted from an initial pool of 6710 athletes.
- The model identified 3 players closely matching Jeremie Frimpong’s KPIs, plus one selected because of a high aggregate score.
- The radar chart visualization highlights key strengths and weaknesses.
- Transfer market values were integrated to meet the initial business requirements.

## 📌 Future Improvements
🔹 Automate data import from online APIs
🔹 Expand scouting methodology with hierarchical clustering
🔹 To be followed by further scouting, assessing tactical/technical, physical, psychological, and social attributes.
🔹 Develop an interactive Shiny dashboard

🏆 Credits
- Data sourced from fbref, transfermarkt, and fotmob
- Analysis performed in R
