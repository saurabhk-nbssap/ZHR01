*----------------------------------------------------------------------*
*   INCLUDE PCSANIN1                                                   *
*----------------------------------------------------------------------*

TABLES : PERNR,
         SSCRFIELDS,
         SADR,
         T596F.


INFOTYPES : 0000,              "Actions
            0001,              "Organizational Assignment
            0002,              "Personal Data
            0008.              "Basic Details.

* Internal Tables.

DATA: BEGIN OF HR_ERROR OCCURS 10.
        INCLUDE STRUCTURE HRERROR.
DATA: END OF HR_ERROR.

* Table to hold the data.
DATA: BEGIN OF DISP_BODY OCCURS 10,
        PERNR LIKE PERNR-PERNR, "Personnel No.
        ENAME LIKE PERNR-ENAME, "Employee Name

        GRSAL LIKE PC207-BETRG, "Gross Salary
        EMPCT LIKE PC207-BETRG, "Employer's Contribution
        ZZICNUM LIKE PC2_IN12-ICNUM,
        ZZdtoj   like pa0002-gbdat,  " Date of Birth
        ZZdtob   like pa0002-gbdat,  " Date of Joining
        ZZdtol   like pa0002-gbdat,  " Date of Leaving
        kostl TYPE kostl, " Cost Center         "Anees
        ktext TYPE ktext. "Cost Center text     "Anees
DATA: END OF DISP_BODY.

* Table to hold temporary data.
DATA : BEGIN OF TEMP_TAB OCCURS 10,
        TRUID LIKE PC2_IN12-SANID,
        GRSAL LIKE PC207-BETRG,
        EMPCT LIKE PC207-BETRG.
DATA : END OF TEMP_TAB.


DATA: BEGIN OF TRGDIR OCCURS 10.
        INCLUDE STRUCTURE PC261.
DATA: END OF TRGDIR.

DATA: BEGIN OF TSAN OCCURS 10.
        INCLUDE STRUCTURE PC2_IN12.
DATA: END OF TSAN.

DATA: BEGIN OF TRT OCCURS 10.
        INCLUDE STRUCTURE PC207.
DATA: END OF TRT.

DATA: BEGIN OF TCRT OCCURS 10.
        INCLUDE STRUCTURE PC208.
DATA: END OF TCRT.


DATA: TEMPCT LIKE PC207-BETRG.

DATA: TSANID LIKE T7INS1-SANID.
DATA: COUNT TYPE I.

TYPE-POOLS: slis.
DATA: fieldcat TYPE slis_t_fieldcat_alv with header line,
      begin of g_itab_fcode occurs 10,
       fcode like rsmpe-func,
      end   of g_itab_fcode.


***** START OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****

***** DATA DECLARATION PART *****

DATA:   gv_fp_outputparams    TYPE  sfpoutputparams,
        gv_fpname             TYPE  fpname,
        gv_fm_name            TYPE  funcname,
        gv_e_interface_type   TYPE  fpinterfacetype,
        gv_w_cx_root TYPE REF TO cx_root,
        gv_mesg TYPE string.

DATA: GV_MONTH_PDF type PA0001-ENAME, "To store month and year
      GV_GRAND_TOT_PDF type PC207-BETRG, "To store Grand total of Gross
      GV_PER_TOT_PDF TYPE PC207-BETRG. "To store Grand total of percent

data:  gt_output_tab type  TY_HCM_HINCSAN0_PDF. " Internal table

data:  gs_output_tab like line of  gt_output_tab. " Structure

***** END OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****
