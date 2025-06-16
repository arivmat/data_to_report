
# 📊 From Data to Report

> Developed by *Andrea Rivera Mateos*

![Badge](https://img.shields.io/badge/R_code-%23276DC3?logo=R&labelColor=white&logoColor=%23276DC3)
![Badge](https://img.shields.io/badge/HTML-grey?logo=html5&logoColor=gray&labelColor=white)
![Badge](https://img.shields.io/badge/CSS-blue?logo=css3&logoColor=white&labelColor=white)

Welcome to **Report Generator**, an interactive R Shiny dashboard that helps you **upload**, **clean**, **analyze**, and **export** data — all without writing a single line of code.

---

## 🚀 At a Glance

This project empowers users to explore and present data with zero coding skills.  
It’s especially useful for data consultants, researchers, and analysts who need fast and flexible reporting tools:

- Provides a user-friendly dashboard to manage raw data
- Enables non-technical users to perform statistical analysis
- Allows building custom dashboards from saved analyses
- Exports results to PowerPoint (.pptx)

---

## 🧠 Project Overview

The Report Generator is composed of:

1. 🏠 **Home Interface** – Upload or load existing projects
2. 🧽 **Data Cleaning Panel** – Edit, transform, and clean data
3. 📊 **Data Analysis Panel** – Perform statistical analysis
4. 🧱 **Dashboard Builder** – Organize results into layout boxes
5. 📤 **Export Module** – Generate a PowerPoint-ready report

---

## 📦 R Packages Used

| Category            | Packages                                                             |
|---------------------|----------------------------------------------------------------------|
| Web Interface        | `shiny`, `shinydashboard`, `shinyjs`, `DT`, `rhandsontable`         |
| Data Handling        | `dplyr`, `tidyr`, `readxl`, `haven`, `lubridate`, `rlang`           |
| Plotting             | `ggplot2`                                                            |
| PowerPoint Export    | `officer`, `flextable`                                               |

> All packages are installed automatically when you launch the app.

---

## 🛠️ Installation & Setup

### Requirements

- R ≥ 4.0
- RStudio

### Setup Instructions

```bash
# Clone the repository
git clone https://github.com/arivmat/report_generator.git

# Open the project in RStudio
# Run the app
shiny::runApp()

```


## ✨ Features

<details>
<summary><strong> 🔍 Data Upload & Cleaning </strong></summary>

- Import `.csv`, `.xlsx`, `.xls`, or `.dta` files.
- Clean your dataset with tools to:
  - Rename variables
  - Drop rows with missing values
  - Fill NAs with *Mean* or *"Unknown"*
  - Parse dates, recode values, and more!
- Undo recent edits before committing changes.

</details>

---

<details>
<summary><strong> 📈 Data Analysis </strong></summary>

- **Univariate**: Frequency, Mean, Median, Std Dev, Histograms, Boxplots
- **Bivariate**: Contingency tables, Scatterplots with regression lines
- **Multivariate**: Missingness summaries

</details>

---

<details>
<summary><strong> 🧩 Customizable Dashboard </strong></summary>

- Drag-and-drop saved analyses into layout slots
- Choose between table or chart views
- Automatically rebuild dashboard UI

</details>

---

<details>
<summary><strong> 📤 Export to PowerPoint </strong></summary>

- Create a presentation-ready `.pptx` report
- Includes tables, charts, and project info
- Powered by [`officer`](https://davidgohel.github.io/officer/) and [`flextable`](https://davidgohel.github.io/flextable/)

</details>



## 📁 File Structure

```r

report-generator/
│
├── app.R                 # main Shiny application
├── saved_projects/       # folder for storing .rds project files
├── docs                  # project documentation

```

## 👩‍💻 Author

Andrea Rivera Mateos

## 🪪 Licence

GPL-3.0 license
Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>
Everyone is permitted to copy and distribute verbatim copies of this license document, but changing it is not allowed.

