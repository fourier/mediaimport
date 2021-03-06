[![Build Status](https://travis-ci.org/fourier/mediaimport.svg?branch=master)](https://travis-ci.org/fourier/mediaimport)
# Media Import
Media Import is a simple Win32/OSX utility for copying images(and other files) according to their timestamps. It is implemented with LispWorks 7.1 32bit for OSX and Windows.
![example](https://github.com/fourier/mediaimport/raw/screenshots/screenshot1.png "Example")
![example-win32](https://github.com/fourier/mediaimport/raw/screenshots/screenshot-win.png "Window screenshot")

## Features
- It verifies if there are already files with the suggested names, and if there are, skip if they are identical or suggest another name;
- Allows to search by file mask like "\*.png, \*.jpg";
- Allows to look in subdirectories;
- Rename according to the pattern, where one can specify subdirectories and files using template declarations like {yyyy} for years;
- Allows to get timestamp from EXIF data for JPEG files

## Installation
Go to [this location](https://github.com/fourier/mediaimport/releases) to download the latest version of the software.

## Copyright
Copyright (c) [Alexey Veretennikov](mailto:alexey.veretennikov@gmail.com). 
Icon made by Pixel perfect from www.flaticon.com
