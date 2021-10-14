# Introductions and Transfers Tool

This tool is designed to facilitate the process of reviewing applications for introductions and transfers of live aquaculture stock. 

Currently, the app is operational for DFO Maritimes and Gulf region and will generate suggested responses based on the species being transferred (intended species or possible hitchhikers) as well as the origin and destination sites.

# Installation

- Users must first install R (on DFO computers, you can do that from the Software Center)
- [Download](https://github.com/remi-daigle/ITapp/archive/master.zip) and unzip this repository or otherwise clone the code onto your computer.
- RStudio based usage: Open `ITapp.Rproj` in RStudio, open the `app.R` file, then click on the `Run App` button
- Shortcut based usage:  Create a shortcut to `ITapp.bat` in a convenient location (e.g. your desktop), double click on your shorcut. This method assumes you have R v.4.1.1 installed in a specific location (e.g. `C:\Program Files\R\R-4.1.1\bin\R.exe`)

The first time you run it, it will take a **very** long time to install, but this is a one time thing. This is necessary to avoid including sensitive data in this repository. Feel free to create a shortcut to the `ITapp.bat`file on your desktop.

# Operations

- Ensure that preferences are set correctly in the 'Settings' tab. The user can set the threshold year for which to ignore older biofouling monitoring records or incidental observation records. The user can also set the number of consecutive non-detections from the biofouling monitoring required to consider an AIS as "failed to establish". Currently, a failure to establish **does not** affect the tool's recommended response, but the invasion history information is provided in Table 3 in the "Supportive Tables" section of the Summary tab.
- On the Origin tab, select the origin site's province and lease identifier. By default, after lease selection, the app will display the 3 nearest biofouling monitoring sites, and the 5 nearest incidental observations, but it will only select the nearest biofouling monitoring site for consideration in the Summary tab. The user may want to increase the number of visible sites or select additional sites. Only sites which are selected will be considered in the Summary tab.
- Repeat the above for the destination site on the Destination tab.
- On the Summary tab, select the "Applicable Product Description" (i.e. the product being moved intentionally), and select relevant "Other Considerations"
- Review the Supportive Tables and Suggested Response on the Summary Tab. The user can return to the other tabs to modify any of the inputs if desired.

# Troubleshoouting
- Any issues can be posted on the [GitHub](https://github.com/remi-daigle/ITapp/issues) repository

