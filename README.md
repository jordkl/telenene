# Hawaiʻi Bird Tracking Visualization

An interactive R Shiny application for visualizing and analyzing GPS tracking data from birds in Hawaiʻi, deployed via ShinyProxy. This tool provides dynamic mapping capabilities with various visualization options and data filtering features.

## Features

### Data Handling
- Upload and process CTT GPS tracking data in CSV format
- HDOP filtering for data quality control
- Automatic downsampling for large datasets
- Date range filtering
- Behavioral season filtering (Nesting, Molting, Flocking)

### Visualization Options
- Interactive map with multiple base layers (Satellite and OpenStreetMap)
- Multiple visualization modes:
  - Individual point plotting
  - Daily/Weekly/Monthly point averaging
  - Heatmap visualization
  - Movement tracking lines
- Color-coded markers by individual bird (serial number)
- Popup information displaying timestamp, season, and point count

### Data Export
- Download filtered and processed data

## Accessing the Application

The application is deployed via ShinyProxy and can be accessed at:
telenene.org

## Data Format Requirements

The application accepts CSV files with the following required columns:
- `datetime`: Timestamp of the GPS reading
- `lat`: Latitude coordinate
- `lon`: Longitude coordinate
- `serial`: Unique identifier for each bird
- `hdop`: Horizontal dilution of precision (quality indicator)

## Geographic Coverage

The application is optimized for the Hawaiian Islands region:
- Latitude: 18.5°N to 22.5°N
- Longitude: 160.5°W to 154.5°W

## Usage Tips

1. **Data Upload**:
   - Use the file upload button to select your CSV file
   - Files should be under [specify size limit] MB
   - Data will be automatically validated upon upload

2. **Filtering Options**:
   - HDOP Filter: Values below 2.5 indicate good quality data
   - Use date range selector to focus on specific time periods
   - Select behavioral seasons of interest

3. **Visualization**:
   - Toggle between different map views using the layer control
   - Enable heatmap for density visualization
   - Use averaging options for clearer patterns in large datasets
   - Enable movement tracks to see bird trajectories

## Support

contact@nene.org

## Privacy Notice

This application processes bird tracking data locally within the session. No data is permanently stored on the server.

## Authors

Jordan Lerma
Nene.org

## Version
1.2.4
