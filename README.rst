pas-libusb -- Object Oriented wrapper for LibUSB
================================================

`libusb <http://www.libusb.org/>`_ provides access to USB devices in user space.

This project provides Pascal header translations plus an object-oriented
wrapper for convenience.

Note: Currently only the legacy version 0.1 of libusb is supported. The new
version 1.0 introduced major changes in the API and will be supported later.

License
-------

    Copyright (C) 2012 Johann Glaser <Johann.Glaser@gmx.at>

    This program is free software; you can redistribute it and/or modify  
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or  
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


Directory Structure
-------------------

  ``src/``
    Header translations and OOP wrapper.

  ``src/examples/``
    Example for the direct usage of the OOP wrapper. This directory also has a
    ``Makefile``.

Build
-----

::

  $ cd src/examples/
  $ make

For further information see the comment at the top of `src/examples/testfirmware.pas
<pas-libusb/blob/master/src/examples/testfirmware.pas>`_.

Usage
-----

Simply add the units ``LibUSB`` and ``USB`` to the uses-clause of your
program. Derive from the class ``TUSBDevice`` to implement your custom driver.

The unit ``EZUSB`` provdes the class ``TUSBDeviceEZUSB`` to interface to the
Cypress EZ-USB AN2131 microcontrollers. It provides functions to access the
on-chip SRAM and to download its firmware.

Platform
--------

This project was compiled with `FreePascal <http://www.freepascal.org/>`_
2.6.0 on Linux.

Other Projects
--------------

**k7103-usb**
  The USB Interface of the Velleman k7103 PC Storage Oscilloscope
  http://k7103.sourceforge.net/ uses these units to communicate with the
  hardware.

**EZ-Tools**
  EZ-Tools is a command line tool for generic access to devices with a built
  in Cypress EZ-USB AN2131 microcontroller.
