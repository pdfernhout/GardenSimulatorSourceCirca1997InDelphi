The Garden with Insight(TM) garden simulator README.TXT
Last updated: 10/1/97

== License

This software program, including all documentation, Delphi source code, and data files, is
Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All Rights Reserved.
Garden with Insight™ is a trademark of Paul D. Fernhout and Cynthia F. Kurtz. 

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

== For more information, contact:

Kurtz-Fernhout Software
http://www.gardenwithinsight.com
support@gardenwithinsight.com

== Required system

The minimum recommended computer to run this software is a 486DX 66Mhz with 256 color VGA and 20MB free hard disk space running Windows 3.1 (8MB RAM) or Windows 95 (16MB). We have successfully run the program on a 386DX 25Mhz machine with a math coprocessor and 16 color VGA and Windows 3.1 (8MB RAM), but the program runs very slowly on such a low end machine. It should also run (slowly) in 4MB under Windows 3.1 but we have not recently tested that configuration. A sound card is optional for tool sound effects and music.

There are two versions: a 16-bit version and a 32-bit version. There are no differences between them except that they were compiled in different versions of Borland Delphi.

== What works where

Windows 3.1: The 16-bit version runs here; the 32-bit version does not. The 32-bit version does NOT run under Win32s.

Windows 95: Both the 16-bit and 32-bit versions run under Windows 95. We have not fully tested long file name support under Windows 95.

Windows NT 3.51 and 4.0: The 16-bit and 32-bit versions run under Windows NT 3.51 and 4.0.

Macintosh Softwindows 2.0: The 16-bit version runs, slowly, with this setup on the Macintosh, but only with the 487 option disabled. See the "Macintosh SoftWindows 2.0 problems" section below.

== Last-minute additions

In the Options menu on the garden window you will find a new item not mentioned in the help system called "Show tool how-to hints". When this option is turned on, you will see hints that tell you what you can do with each tool action (like "water" or "plant") as you move the mouse around in the garden window. The hint for each tool action depends on whether the cursor is inside a soil patch, over the base of a plant, or outside any soil patches. The hint also tells you when you can or can't use a particular tool action (for example, you can't plant a seed if there are no soil patches). These how-to hints might answer some of your questions about using the tools.

There are two reasons you might not want to use tool how-to hints after you get used to the tools. 1) The tool how-to hints replace hints that give you information about soil patches, plants and the weather. You might find that later you will prefer to see this information instead. 2) If your computer is not very fast, you might find that the tool how-to hints make the mouse move clumsily in the garden window, because the program is taking extra time to decide which hint to show. To stop using the tool how-to hints, turn off the "Show tool how-to hints" option in the Options menu.

== Help problems

Problem: Under Windows NT 3.51 (and possibly other platforms), the help file shows no pictures; only little asterisks show where pictures should be. 
Workaround: This is because of not enough free resources, probably. Close other programs and the pictures should reappear.

Problem: If you use a 640x480 monitor it may be difficult to do the tutorial because you can't see the program's window and the help window at the same time. 
Workaround: We have included in the basic packages a plain text version of the tutorial you can print out (tutorial.txt). It has no pictures but is identical to the on-screen tutorial otherwise. 

== General problems

Problem: If you are using large fonts, some of the bitmaps in the help system will be garbled slightly. 
Workaround: None.

Problem: You may see numerical exceptions when the simulation is running.
Workaround: If you encounter any numerical exceptions while the simulation is running (that is, if the numerical exceptions window appears with some messages in it), please click the Save as button to save the messages to a file, then email us a message with the numerical exceptions in it. We will try to replicate the problem and fix it. Sometimes you can make the problem go away by reseeding the plant or removing the soil patch and making another. Sometimes just running the simulation for a while works too.

Problem: You experience the following while moving the mouse over the running simulation: "Application Error: Not Present Fault in module WIN87EM.DLL" and you have a Logitech mouse. 
Workaround: A conflict exists between Garden with Insight(TM) and the DOS program CLOAKING.EXE, which loads the Logitech DOS mouse driver LMOUSE.EXE program into extended memory. To disable the cloaking program, type "REM" in front of the line in your AUTOEXEC.BAT file that contains "CLOAKING.EXE", then reboot your machine. You don't need this driver to use your mouse in Windows; you may need this driver or another one to use a Logitech mouse under DOS. Actually this problem is not caused by Garden with Insight(TM) but by an incompatibility between mathematical calculations in Delphi (in which Garden with Insight(TM) is written) and the CLOAKING program. Hopefully this conflict will be fixed in the future.

Problem: The tools appear to be in very faint bluish squares rather than floating transparently over the garden. 
Workaround: We encountered this problem testing on a Gateway 2000 P5-100XL using Windows 95 and the ATI Mach 64 Tech-enhanced driver version 4.02 (macxw4.drv). Try getting a later version of the ATI Mach 64 drivers, although we are not sure this will fix the problem. We have not seen this problem with either the ATI Mach 32 or any other vendor's graphics card.

== Macintosh SoftWindows 2.0 problems

Problem: When trying to run the installation program under Macintosh SoftWindows 2.0, you may see the errors "Cannot find DDEML.DLL" and "Cannot find VER.DLL".  
Workaround: Copy the installation file to the c:\windows\system directory and run it from there.
 
Problem: A General Protection Fault appears during installation under Macintosh SoftWindows 2.0. 
Workaround: Try running the install program again. If that doesn't work, try exiting SoftWindows and the running the install program again. If that doesn't work, download the self-extracting WinZip archive from our web site, which will put the files on your machine. Then you can create the program icons manually in the file manager if you would like to.
 
Problem: Windows 3.1 crashes and goes to DOS under Macintosh SoftWindows 2.0 when running the simulation.
Workaround: Disable the 487 option. This should fix the problem.
 
Problem: Sounds and music don't play under SoftWindows 2.0.
Workaround: No workaround at this time. Please let us know if you find one.
 
== Installation problems 
 
Problem: Install program gives a "Could not find file" error and quits.
Workaround: It is possible you did not have enough space on your hard disk to download the file. Check the file size to see that the entire file was downloaded (the web site tells you what size the files should be). Also make sure you have several megabytes of free hard disk space. If you have sufficient file space, you can either download the file again in case it was corrupted, or try downloading the WinZip self-extracting archive version of the Windows 3.1 product and create the icons manually from the file manager if you would like to.

Problem: The installation program creates an extra temporary directory under your Windows directory and places one file in it, and leaves the file there. The file is used to display the license.txt during installation.
Workaround: After the program is installed you can delete this temporary directory.
