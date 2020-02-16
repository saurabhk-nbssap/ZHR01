*----------------------------------------------------------------------*
* PDF Form CONVERSION FOR: HINCSAN0

* Author: G.Ashok kumar  User:C5061983
*
* Date	 : 27-12-2004
*
* Program description: Modified print program for HR_IN_SANLST_99M
* form to get the display in PDF and SAP Script form.

*----------------------------------------------------------------------*


REPORT Z6HR031R_INCSAN0 .
*********************************************************************
* This report gives a monthly listing of Superannuation Contributions
*********************************************************************

INCLUDE Z6HR031I_PCSANIN1.
*INCLUDE PCSANIN1.           "Declare Tables & Infotypes.

INCLUDE Z6HR031I_PCREMIN6.
*INCLUDE PCREMIN6.           "International & Country Specific Cluster.
INCLUDE Z6HR031I_PCSANIN5.
*INCLUDE PCSANIN5.           "Selection Screen.
INCLUDE Z6HR031I_PCSANIN6.
*INCLUDE PCSANIN6.           "Include for Layout.

INCLUDE Z6HR031I_PCSANIN2.
*INCLUDE PCSANIN2.           "Main Processing.

INCLUDE Z6HR031I_PCSANIN3.
*INCLUDE PCSANIN3.           "Common Routines.
INCLUDE Z6HR031I_PCSANIN4.
*INCLUDE PCSANIN4.           "Display Routines.
INCLUDE Z6HR031I_PCSAPIN0.
*INCLUDE PCSAPIN0.           "SAP Script Routines.
INCLUDE Z6HR031I_PCSANIN7.
*INCLUDE PCSANIN7.           "Commonly Used Routines.
