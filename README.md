# DENVAedes-Alert-BE App: Introduction and Instruction Manual

Welcome to the DENVAedes-Alert-BE App, an interactive dashboard designed to provide real-time and historical analysis of Dengue and Aedes mosquito data in Belgium. This guide will help you understand the functionalities and features of the app and provide step-by-step instructions on how to use each component effectively.

## Table of Contents
1. [Introduction](#introduction)
2. [App Structure](#app-structure)
3. [User Guide](#user-guide)
   - [About Tab](#about-tab)
   - [Realtime Risk Tab](#realtime-risk-tab)
   - [Historical Risk Tab](#historical-risk-tab)
   - [Cumulative Dengue Cases Tab](#cumulative-dengue-cases-tab)
   - [Cumulative Aedes Positive Years Tab](#cumulative-aedes-positive-years-tab)
   - [Data Management Tab](#data-management-tab)
4. [Data Requirements and Format](#data-requirements-and-format)
5. [Post Code / Shape File Data Notes](#post-code--shape-file-data-notes)
6. [Frequently Asked Questions](#frequently-asked-questions)
7. [Technical Notes and Troubleshooting](#technical-notes-and-troubleshooting)

---

## Introduction
The DENVAedes-Alert-BE App is a data visualization tool that combines various datasets related to Dengue cases and Aedes mosquito surveillance. It provides insights into real-time risks, historical trends, and cumulative data over the past five years, helping stakeholders make informed decisions and monitor trends effectively.

---

## App Structure
The application consists of several tabs, each dedicated to a specific type of analysis or functionality:

1. **About:** Overview of the app, including its purpose, data sources, and project background.
2. **Realtime Risk:** Displays real-time risk analysis based on recent data.
3. **Historical Risk:** Visualizes historical risk data over selected time periods.
4. **Cumulative Dengue Cases:** Shows cumulative Dengue cases over the last five years.
5. **Cumulative Aedes Positive Years:** Visualizes cumulative Aedes mosquito surveillance data.
6. **Data Management:** Tools for uploading, verifying, and deleting data.

---

## User Guide

### About Tab
The "About" tab provides a detailed introduction to the app, including its purpose, data sources, and any relevant background information. Here you can find the README file of the project for more details.

### Realtime Risk Tab
This tab allows users to explore real-time risk data related to Dengue cases and Aedes mosquito presence.

- **Date Display:** Displays the current date used for analysis.
- **DF Case Range Slider:** Adjusts the range of weeks to consider for real-time analysis.
- **Map Visualization:** Displays a map showing Aedes mosquito presence and Dengue case overlaps.
- **Aedes-Dengue Overlap Table:** Displays a table of areas with overlaps between Aedes mosquito presence and Dengue cases.

### Historical Risk Tab
This tab visualizes historical risk data for selected years and months.

- **Year Selection Dropdown:** Choose the year for which to view historical data.
- **Month Range Slider:** Select the range of months to analyze within the chosen year.
- **Historical Risk Map:** Displays a map of historical data based on user selections.
- **Historical Overlap Table:** Shows detailed information on historical Aedes and Dengue overlaps.

### Cumulative Dengue Cases Tab
This tab shows the cumulative Dengue cases over the past five years.

- **Cumulative Map:** Displays a color-coded map representing Dengue cases across Belgium.
- **Summary Table:** A table summarizing Dengue cases for different postcodes.

### Cumulative Aedes Positive Years Tab
This tab visualizes cumulative data on Aedes mosquito presence over the past five years.

- **Cumulative Aedes Map:** A map showing areas with positive Aedes mosquito data over the last five years.
- **Summary Table:** A table summarizing the data for different postcodes.

### Data Management Tab
This tab provides tools for data upload and management.

- **Upload Data:** Allows users to upload new data files in CSV format for either Aedes or Dengue datasets.
- **Verify and Confirm Upload:** Users can verify uploaded data before confirming the update to the global dataset.
- **Delete All Data:** A button to delete all existing data from the global dataset.

---

## Data Requirements and Format

### Aedes Surveillance Data
- **Required Columns:** `Observation_ID`, `Site_codes`, `Municipality`, `Postcode`, `Latitude`, `Longitude`, `Site_type`, `Detection_date`
- **Formats:**
  - `Observation_ID`: Numeric, unique identifier.
  - `Postcode`: Four-digit numerical code.
  - `Latitude` and `Longitude`: Decimal format, valid ranges for coordinates.
  - `Detection_date`: `YYYY-MM-DD` format.

### Dengue Surveillance Data
- **Required Columns:** `Sample_ID`, `Source_country`, `Postcode`, `Report_date`
- **Formats:**
  - `Sample_ID`: Numeric, unique identifier.
  - `Postcode`: Four-digit numerical code.
  - `Report_date`: `YYYY-MM-DD` format.

### General Data Guidelines
- Ensure no spaces in column names.
- Data should be in long format, one observation per row.
- Use consistent naming conventions for countries and postcodes.

---

## Post Code / Shape File Data Notes

### Recommended Naming Conventions
- **List of Post Codes:** [Belgium Post Codes](https://www.geo.be/catalog/details/9738c7c0-5255-11ea-8895-34e12d0f0423?l=en)
- **Country Names:** Consider using the [UN M49 Standard](https://unstats.un.org/unsd/methodology/m49/) for tracking regional information.

### Nomenclature of Belgium Administrative Units
There is a discrepancy between using postcodes (BE.BPOST dataset) and municipalities. It is recommended to maintain a consistent nomenclature when dealing with geographical data to avoid confusion.

---

## Frequently Asked Questions

1. **How do I upload new data?**
   Navigate to the "Data Management" tab, select the dataset type, and upload your CSV file. Verify the data before confirming the upload.

2. **What should I do if I see validation errors?**
   Check the uploaded data format and ensure all required columns and formats match the specifications mentioned in the "Data Requirements and Format" section.

3. **Can I delete specific data entries?**
   Currently, you can only delete all data. Use the "Delete All Data" button cautiously.

4. **How do I reset the map view?**
   Click the reset view button (globe icon) at the bottom of each map.

---

## Technical Notes and Troubleshooting

1. **Data Not Displaying:**
   Ensure the uploaded data meets the format requirements and all necessary columns are present.

2. **Error Messages:**
   Check for detailed error messages in the "Data Management" tab. Common issues include incorrect column names or data types.

3. **Deployment Issues:**
   Make sure you have all required libraries installed and the `renv` environment initialized.

For further assistance, consult the project documentation or contact the project team.

---

This manual should serve as a comprehensive guide to using the DENVAedes-Alert-BE App. If you have any additional questions, feel free to reach out to the development team for support. Happy analyzing!
