*----------------------------------------------------------------------*
* PDF Form CONVERSION FOR: HINCEPF0

* Author: G. Ashok kumar  User:C5061983
*
* Date   : 30-12-2004
*
* Program description: Modified print program for HR_IN_EPF010_99M,
* HR_IN_EPF005_99M AND HR_IN_EPF12A_99M
* form to get the display in PDF and SAP Script form.

*----------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 8/10/2015
*   REASON FOR CHANGE: Authorization Check
*   REQUEST #: IRDK920944
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Naren Karra
*   CHANGE ON: 20/12/2016
*   REASON FOR CHANGE: Added UAN field
*   REQUEST #: IRDK920944
*   ( since the above TR was not moved to PRD - commenting it's changes and moving forward )
* --------------------------------------------------------------------------------------------*

*  Report for the Provident Fund Calculations of India Payroll

REPORT Z6HR032R_HINCEPF0.

INCLUDE Z6HR032I_PCEPFIN1.
*INCLUDE PCEPFIN1.             " Declaring tables and infotypes used
INCLUDE Z6HR032I_PCEPFIN2.
*INCLUDE PCEPFIN2.             " Defines Internal Tables
INCLUDE Z6HR032I_PCREMIN6.
*INCLUDE PCREMIN6.             " Include for Indian Cluster
INCLUDE Z6HR032I_PCEPFIN6.
*INCLUDE PCEPFIN6.             " Include for Selection Screen
INCLUDE Z6HR032I_PCEPFIN5.
*INCLUDE PCEPFIN5.             " Include for Layouts
INCLUDE Z6HR032I_PCEPFIN3.
*INCLUDE PCEPFIN3.             " Main processing
INCLUDE Z6HR032I_PCSAPIN0.
*INCLUDE PCSAPIN0.             " SAP Script routines
INCLUDE Z6HR032I_PCEPFIN4.
*INCLUDE PCEPFIN4.             " Standard routines to display results
INCLUDE Z6HR032I_PCEPFIN7.
*INCLUDE PCEPFIN7.             " Subroutines to read DB tables
INCLUDE Z6HR032I_PCEPFIN8.
*INCLUDE PCEPFIN8.             " Common routines required for EPF report
INCLUDE Z6HR032I_PCEPFIN9.
*INCLUDE PCEPFIN9.             " Subroutines to read infotype data
