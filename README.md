# marinetraffic
Marine traffic dashboard developed for Appsilon

Shiny Developer @Appsilon

Using the Marine data, available here please build a Shiny app using shiny.semantic 0.4.0

We’re primarily interested in how you build the app structure, how you implement the calculation logic, and the quality of code. If you have any questions, please be sure to send us an email!
Goals
•	User can select a vessel type (Ship_type) from the dropdown field
•	User can select a vessel from a dropdown field (available vessels should correspond to the selected type). Dropdown fields should be created as a Shiny module
•	For the vessel selected, find the observation when it sailed the longest distance between two consecutive observations. If there is a situation when a vessel moves exactly the same amount of meters, please select the most recent.  
•	Display that on the map - show two points, the beginning and the end of the movement. The map should be created using the leaflet library. Changing type and vessel name should re-render the map and the note.
•	Provide a short note saying how much the ship sailed - distance should be provided in meters.
•	Also please use the best practices you know, to ensure project quality. The application should be reasonably efficient and tested with testthat.
•	Please think about the app interface inspired by the following mockups. You can add additional statistics and visualizations to make your dashboard look attractive. It depends on you how you want to structure it.
o	Example 1
o	Example 2
o	Working Shiny app examples: https://demo.appsilon.ai/.

As a solution please provide a link to the deployed application on shinyapps.io and a link to Github repository.

Data description
See additional explanation of what is transmitted on the AIS signal.

LAT - ship’s latitude
LON - ship’s longitude
SPEED - ship’s speed in knots
COURSE - ship’s course as angle
HEADING - ship’s compass direction
DESTINATION - ship’s destination (reported by the crew)
FLAG - ship’s flag
LENGTH - ship’s length in meters
SHIPNAME - ship’s name
SHIPTYPE - ship’s type
SHIP_ID - ship’s unique identifier
WIDTH - ship’s width in meters
DWT - ship’s deadweight in tones
DATETIME - date and time of the observation
PORT - current port reported by the vessel
Date - date extracted from DATETIME
Week_nb - week number extracted from date
Ship_type - ship’s type from SHIPTYPE
Port - current port assigned based on the ship’s location
Is_parked - indicator whether the ship is moving or not 

Good luck!


