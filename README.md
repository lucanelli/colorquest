# Color Quest


## Introduction
Color Quest is an interactive Shiny app designed to explore and select color palettes for data visualizations with a focus on accessibility. It helps users understand how their color choices in data visualizations are perceived by individuals with color vision deficiencies, promoting inclusivity in scientific communication.

## Features

* Color Palette Selection: Choose up to four colors for visualization.
* Simulation of Color Vision Deficiencies: Visualize how colors appear to people with deuteranopia, protanopia, and desaturation.
* Multiple Plot Types: Experiment with scatter plots, line plots, box plots, histograms, and heatmaps.
* Custom Image Upload: Upload and test how your images appear with different color palettes.

## Installation
To set up the Color Quest app locally, follow these steps:

### Clone the repository:
git clone https://github.com/lucanelli/colorquest.git
Install required R packages:
install.packages(c("shiny", "colourpicker", "colorBlindness", "ggplot2", "terra", "KernSmooth", "tidyterra", "grid", "jpeg", "shinycssloaders"))
Usage

### Launch the app in RStudio by opening the app.R file and clicking 'Run App'.
Use the color pickers in the sidebar to select your desired colors.
Navigate through different tabs to view your selected colors in various plot types.
Upload your own images to see how they appear with the selected colors.

## Contributing
Contributions to the project are welcome! Please refer to the CONTRIBUTING.md file for detailed instructions on how to contribute.

## Support and Contact
For support, questions, or more information, please email luca.nelli@glasgow.ac.uk.

## Acknowledgements
Color Quest was developed by Luca Nelli at University of Glasgoe. We thank all contributors and users for their support and feedback.

## License
This project is licensed under the MIT License (LICENSE).

## Citation
**Suggested Citation:**
Color-blindness friendly plots were generated using ColorQuest software (Nelli, 2023)

*Nelli (2023). "Color Quest: An Interactive Tool for Exploring Color Palettes and Enhancing Visualization Accessibility." Journal of XXX , XX(X), XXX-XXX.*
