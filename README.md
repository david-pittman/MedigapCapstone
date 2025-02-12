R code: Idaho_eventstudy.R -- In this code, we take our average premiums, enrollment, and offerings by year and control group, run a glm regression, after turning year into the event time (ie -3, -2, 0, 1, etc)
We take the coefficients and 95% CIs for both the control states and Idaho and plot them both on one plot (EventStudyOverlay.png) and side-by-side (EventStudySideBySide.png). 
We last take the difference between those two sets of coefficients and plot them to give a single line (EventStudyDifference.png) showing our event study. 

Data as CSV files:
DiD_overall -- average premiums, enrollment, and offerings by year and control group on which we use to run our DiD regression 
event_study_results -- The output (coefficients and 95% CIs) of the initial DiD regression of average premiums by year/event_time and control group 
event_study_diff -- the difference between the coefficients for the control group and Idaho (upon writing this, I don't realize how the resulting CIs are constructed.) 
