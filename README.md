# Analyzing the Link Between Nutrition & Cancer Risk 
## About this Project
This was my final project for the **Data Analysis** course at the **Athens University of Economics and Business (AUEB)**. My goal was to dig into real-world health data and see how much our lifestyle and diet actually impact cancer-preventive biomarkers like **Beta-Carotene** and **Retinol**.

Working with health data is always a challenge because the numbers are rarely "clean" or "normal," so this project was a great opportunity for me to apply some advanced statistical techniques to find meaningful patterns.

## The Data
I analyzed a dataset of **315 patients**, looking at 12 different factors including:
* **Lifestyle:** Age, Smoking habits, and BMI.
* **Diet:** Daily intake of calories, fiber, fat, and alcohol.
* **The Goal:** Predicting the concentration of `BETAPLAS` and `RETPLAS` in the blood.

##  My Approach

* **Handling "Messy" Data:** Real-world health data isn't perfect. I used **non-parametric tests** (like Wilcoxon and Kruskal-Wallis) because the vitamin levels didn't follow a normal distribution.
* **Finding Connections:** I used **Spearman’s Correlation** to see how things like fiber or alcohol intake directly relate to blood chemistry.
* **Building the Model:** I built multiple linear regression models. To make them work properly, I applied **logarithmic transformations**—this helped stabilize the variance and made the predictions much more reliable.
* **Quality Check:**. I used **Cook’s Distance** to hunt for influential outliers that might be skewing the model.

## What I Found
* **Fiber & Age matter:** Generally, older patients and those with a fiber-rich diet showed better vitamin levels.
* **The "Downsides":** Smoking and a high BMI were the biggest "red flags," consistently showing a negative impact on Beta-Carotene levels.
* **The Bottom Line:** A simple linear model isn't enough for health data; the log-transformation was the "key" to getting a valid and useful result.

##  Tech
* **Language:** R 
* **Tools:** RStudio, ggplot2 for visualizations, and the `car` package for model diagnostics.

---
**Konstantinos Pasapnois** *Statistics Student at AUEB*
