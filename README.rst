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

Each file contains a header showing the according license.

 - libusb(x) and its header translation are licensed under LGPL 2.1 (or later).
 - Some C preprocessor macros were translated to Pascal functions. These are
   licensed under a modified LGPL.
 - All other Pascal units (especially the OOP wrapper) are licensed under a
   modified LGPL which allows static linking (see the file
   COPYING.modifiedLGPL.txt).
 - The example programs are released as public domain so you can base
   commercial work on them.

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
`src/examples/testfirmware.pas <libusb-1.0/src/examples/testfirmware.pas>`_.

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

The main work was performed on a Debian GNU/Linux AMD64 machine with
libusb-1.0 version 1.0.12.

A user successfully used pas-libusb on a Raspberry Pi (ARM processor) with
the Raspbian Debian GNU/Linux based distribution. Although the libusb-1.0
package version 1.0.9 originally installed didn't work (due to lacking the
two functions libusb_get_port_number() and libusb_get_port_path()), he
manually upgraded from libusbx sources to version 1.0.14 which now works.
The same user also reports libusb-1.0 1.0.12 on Linux Mint i386 to work.

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
