
Welcome to the Post Mortem Interval (PMI) Prediction App. This application helps you predict the PMI (Hour) based on tissue type, circRNA (or other RNAs), expression levels, and temperature.

Application Overview
This app predicts Post Mortem Interval (PMI) using circRNA expression data with Support Vector Machines (SVR) and Transfer Learning technology.

Section Descriptions:
1. Home
Provides an introduction to the application and information about the lab. Includes citation information for the research paper this tool is based on.

2. Mouse Data
Predict PMI using mouse data. Features include:

Select tissue type and circRNA from dropdown menus
Adjust temperature and expression values
View 3D visualization of predictions
See statistical plots comparing actual vs predicted values
Access detailed model information
3. Human Data
Predict PMI using human data with optional transfer learning from mouse models. Features include:

Same input options as Mouse Data section
Toggle transfer learning option (enabled by default)
View transfer learning performance plots
Access QQ plots for model diagnostics
4. User Data
Upload and analyze your own data. Features include:

Upload Excel files with your experimental data
Select relevant columns from your data
Generate predictions using your custom dataset
View 3D visualizations specific to your data
5. Website Statistics
Track application usage metrics including:

Total views and daily views
Number of unique users
Detailed access logs
Interactive charts of usage patterns
6. Publications
For reading and helping
Fu J, Song B, Qian J, Cheng J, Chiampanichayakul S, Anuchapreeda S, Fu J. Exploring the Post Mortem Interval (PMI) Estimation Model by circRNA circRnf169 in Mouse Liver Tissue. Int J Mol Sci. 2025 Jan 26;26(3):1046. doi: 10.3390/ijms26031046. PMID: 39940814.
Song B, Fu J, Qian J, He T, Cheng J, Chiampanichayakul S, Anuchapreeda S, Fu J. Development of Mathematical Models Using circRNA Combinations (circTulp4, circSlc8a1, and circStrn3) in Mouse Brain Tissue for Postmortem Interval Estimation. Int J Mol Sci. 2025 May 8;26(10):4495. doi: 10.3390/ijms26104495. PMID: 40429639.
Song B, Fu J, Cheng J, Chiampanichayakul S, Anuchapreeda S, Fu J. Circular RNA circFat3 as a biomarker for construction of postmortem interval Estimation models in mouse brain tissues at multiple temperatures. Sci Rep. 2025 Jul 1;15(1):21577. doi: 10.1038/s41598-025-07998-0. PMID: 40593252.
Data Requirements:
For user uploads, your Excel file must contain these columns:

Tissue - Tissue type
circRNA - circRNA identifier
Expression - Expression level
Temperature - Temperature in Celsius
PMI_Hour - Actual PMI in hours (for model training)
Technical Notes:
The application uses:

Support Vector Regression with RBF kernel (SVR),Random Forest (RF), and Neural Network (NN) for modeling
Transfer learning for mouse-to-human predictions
3D visualization with convex hulls for data boundaries
Interactive plots with Plotly
Note: All predictions should be interpreted by qualified professionals in context with other forensic evidence.



About the Lab
Key Laboratory of Epigenetics and Oncology, the Research Center for Preclinical Medicine

Director: Prof. Junjiang Fu

Address: Southwest Medical University, Luzhou 646000, Sichuan, China


Support Information
For support, please contact us at:
Name: Mazaher Maghsoudloo

Email: mazaher@swmu.edu.cn

                babak1146@gmail.com
