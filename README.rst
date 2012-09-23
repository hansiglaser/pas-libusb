pas-libusb -- Object Oriented wrapper for libusb and libusbx
============================================================

`libusb <http://www.libusb.org/>`_ and its fork `libusbx
<http://libusbx.sourceforge.net/>`_ provide access to USB devices in user
space.

This project provides Pascal header translations plus an object-oriented
wrapper for convenience.

This branch provides support for the new version 1.0 of libusb and its fork
libusbx.

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
    Examples for the direct usage of the OOP wrapper. This directory also has a
    ``Makefile``.

Build
-----

::

  $ cd src/examples/
  $ make

For further information see the comment at the top of
`src/examples/testfirmware.pas <src/examples/testfirmware.pas>`_.

Usage
-----

Simply add the units ``LibUsb``, ``LibUsbOop`` and ``LibUsbUtil`` to the
uses-clause of your program. Derive from the class ``TLibUsbDevice`` to
implement your custom driver. If your device uses several interfaces, you
should derive from TLibUsbInterface and implement the driver. But be careful,
because EP0 doesn't belong to an interface but to the device. If your device
uses control messages and bulk endpoints, think of a sane solution.

The unit ``EZUSB`` provides the class ``TLibUsbDeviceEZUSB`` to interface to
the Cypress EZ-USB AN2131 microcontrollers. It provides functions to access the
on-chip SRAM and to download its firmware.

Changes from libusb-0.1
-----------------------
 - all class names now start with ``TLibUsb*``
 - you have to create a ``TLibUsbContext`` before any other action
 - new ``TLibUsb*MatchClass``
 - ``TLibUsbDevice``: USB configuration is now separate from the constructor,
   you have to call ``SetConfiguration`` manually
 - ``USBFindDevices``   is now replaced by ``TLibUsbContext.FindDevices``
 - ``USBFindInterface`` is now replaced by ``TLibUsbDevice.FindInterface``
 - ``FindInterface``    is now replaced by ``TLibUsbInterface.FindEndpoint``
 - no procedural ``USB_Send``, ``USB_Recv``, ... any more
 - ``TUSBPseudoHIDInterface`` not yet translated
 - ``TLibUsbDeviceEZUSB``:
    - you have to call ``SetConfiguration`` manually
    - ``LoadMem`` issues ``ELibUsb`` instead of ``EInOutError``

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

TODO
----

 - pseudo-hid interface
