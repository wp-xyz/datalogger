# datalogger

DataLogger is a small application which reads measured values from a digital multimeter and plots them as a function of time.

![Screenshot](screenshots/datalogger-main.png)

# Features
* Interfaces to some digital multimeters with digital output via serial port. Use a serial/usb converter if your computer does not have a serial input any more. Supported models: 
* Supported models: Conrad VC630, VC820, VC830, VC840, VC850, or "user-defined" (based on the DMM chips _FS9721_LP3_ (seven-segment output) and _FS9922-DMM4_ (digit output) by Fortune Semiconductor)
* Linear or logarithmic display of the measured quantity.
* Measurement interval: a few measurements per second, or slower. Interval can be varied during the measurement.
* Transformations: User-provided equation to convert the measured quantity to some other quantity. The application initially was written to convert the analog voltage output of a pressure meter to pressure in mBar.
* Save measured curve as xml, csv or spreadsheet files. Re-load and overlay previously measured curves.

# Compilation
* The application can be compiled as 32-bit or 64-bit binary from the source files in this repository using _Lazarus_ v2.0+ / _FPC_ v3.0+ for Windows, Linux gtk2 and qt4/qt5. macOS might work as well, but is not tested.
* Compilation requires the packages _fpspreadsheet_ and _synapse_, both available via the _Online-Package-Manager_ of _Lazarus_.
