report hincf160.
*-------------------------------------------------------------------*
*
* PDF Form CONVERSION FOR: HR_IN_TAXF16000Y,HR_IN_TAXF1600AA        *
*                          HR_IN_TAXF16NX_Y & HR_IN_TAXF16NX_P Forms*
*
* Author : Pratheek Bhonsley & Tarun Kumar Mishra
*          (C5146253)           (I056108)                        *
*
* Date : 19/10/2010                                                 *
*
* Program description: Print program for FORM16,16AA,ANNEXURE-16 &  *
*                                        FORM12BA                   *
*
* supports Both PDF Output & SAPScript output.                      *
*-------------------------------------------------------------------*

* -------------------------------------------------------------------- *
*        Program to generate the form 16 for India
* -------------------------------------------------------------------- *
include pcf16in11.               " Declares tables and infotypes needed
include pumapin7.
include pcf16in12.               " Declarations
include pcremin6.                " Include for Global & Indian Cluster
include pcf16in15.               " Include for Selection Screen
include pcf16in14.               " Selection screen layout
include pcf16in13.               " Main Processing
include zpcf16in17.               " Subroutines
include pcf16in16_pdf.           " Print rountine for PDF
*INCLUDE PCSAPIN0.               " SAP Script routines
*INCLUDE PCF16INUT01.             " Include for unit tests
*INCLUDE PCF16INUT02.             " Include for unit tests  2
include pcf16in8.                " Subroutines for reading tables
