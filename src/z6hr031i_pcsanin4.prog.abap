*----------------------------------------------------------------------*
***INCLUDE PCSANIN4 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY.
  DATA : NO TYPE I,
         RET_CD LIKE SY-SUBRC.


DATA: repid like sy-repid.

DELETE disp_body WHERE GRSAL is INITIAL.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name         = 'Z6HR031R_INCSAN0'
            i_internal_tabname     = 'DISP_BODY'
            i_inclname             = 'Z6HR031I_PCSANIN1'
       CHANGING
            ct_fieldcat            = fieldcat[]
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.


  read table fieldcat with key fieldname = 'PERNR'.
  fieldcat-seltext_m = 'Personnel No.'(032).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 14.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'ENAME'.
  fieldcat-seltext_m = 'Employee Name'(033).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 14.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'GRSAL'.
  fieldcat-seltext_m = 'Basic Salart'(034).
  fieldcat-ddictxt = 'M'.
  fieldcat-outputlen = 13.
  modify fieldcat index sy-tabix.

  read table fieldcat with key fieldname = 'EMPCT'.
  fieldcat-seltext_l = 'Employer Contribution'(035).
  fieldcat-ddictxt = 'L'.
  fieldcat-outputlen = 22.
  modify fieldcat index sy-tabix.
  clear fieldcat.

  read table fieldcat with key fieldname = 'ZZICNUM'.
  fieldcat-seltext_l = 'Super Annua.Number'.
  fieldcat-seltext_m = 'Super Annua.Number'.
  fieldcat-seltext_s = 'Super Annua.Number'.
  fieldcat-reptext_ddic = 'L'.
  fieldcat-outputlen = 20.
  modify fieldcat index sy-tabix.
  clear fieldcat.

  read table fieldcat with key fieldname = 'ZZDTOB'.
  fieldcat-seltext_l = 'Date of Birth'.
  fieldcat-seltext_m = 'Date of Birth'.
  fieldcat-seltext_s = 'Date of Birth'.
  fieldcat-reptext_ddic = 'L'.
  fieldcat-outputlen = 10.
  modify fieldcat index sy-tabix.
  clear fieldcat.

    read table fieldcat with key fieldname = 'ZZDTOJ'.
  fieldcat-seltext_l = 'Date of Joining'.
  fieldcat-seltext_m = 'Date of Joining'.
  fieldcat-seltext_s = 'Date of Joining'.
  fieldcat-reptext_ddic = 'L'.
  fieldcat-outputlen = 10.
  modify fieldcat index sy-tabix.
  clear fieldcat.

    read table fieldcat with key fieldname = 'ZZDTOL'.
  fieldcat-seltext_l = 'Date of Leaving'.
  fieldcat-seltext_m = 'Date of Leaving'.
  fieldcat-seltext_s = 'Date of Leaving'.
  fieldcat-reptext_ddic = 'L'.
  fieldcat-outputlen = 10.
  modify fieldcat index sy-tabix.
  clear fieldcat.

  "Anees
  read table fieldcat with key fieldname = 'KOSTL'.
  fieldcat-seltext_l = 'Cost.Center'.
  fieldcat-seltext_m = 'Cost.Center'.
  fieldcat-seltext_s = 'Cost.Center'.
  fieldcat-reptext_ddic = 'L'.
  fieldcat-outputlen = 10.
  modify fieldcat index sy-tabix.
  clear fieldcat.

  read table fieldcat with key fieldname = 'KTEXT'.
  fieldcat-seltext_l = 'Cost.Center'.
  fieldcat-seltext_m = 'Cost.Center'.
  fieldcat-seltext_s = 'Cost.Center'.
  fieldcat-reptext_ddic = 'L'.
  fieldcat-outputlen = 20.
  modify fieldcat index sy-tabix.
  clear fieldcat.

  "End

  repid = sy-repid.
  refresh g_itab_fcode.
  move 'AMBC' TO g_itab_fcode-fcode.
  append g_itab_fcode.
  move 'CORC' TO g_itab_fcode-fcode.
  append g_itab_fcode.



  DESCRIBE TABLE DISP_BODY LINES NO.

  IF NO > 0.

*    CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
*         EXPORTING
*              BASIC_LIST_TITLE     = 'Superannuation List'(036)
*              FILE_NAME            = 'HINCSAN0'
*              LAY_OUT              = 0
*              DYN_PUSHBUTTON_TEXT1 = 'Print Errors'(037)
*              DYN_PUSHBUTTON_TEXT2 = 'Print Form'(038)
*              ADDITIONAL_OPTIONS   = ' '
*         IMPORTING
*              RETURN_CODE          = RET_CD
*         TABLES
*              DATA_TAB             = DISP_BODY
*              FIELDNAME_TAB        = FIELD_NAMES
*         EXCEPTIONS
*              DOWNLOAD_PROBLEM     = 1
*              NO_DATA_TAB_ENTRIES  = 2
*              TABLE_MISMATCH       = 3
*              PRINT_PROBLEMS       = 4
*              OTHERS               = 5.


    perform display_alv_grid in program HINCALV0
                          TABLES DISP_BODY fieldcat g_itab_fcode
                          USING repid text-036 ''.

    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE E171(PN).
*       problem in downloading
      WHEN 3.
        MESSAGE E112(HRPADIN01).
*       There is mismatch between the field table and data table
      WHEN 4.
        MESSAGE E113(HRPADIN01).
*       Error due to print problems
      WHEN 5.
      WHEN OTHERS.
    ENDCASE.

    IF SY-BATCH = 'X'.
*   For output in SAP Script
      PERFORM ERROR_CASES.
      PERFORM PRINT_MODULE.
    ENDIF.
*
*    CHECK SY-SUBRC EQ 0.
*
*    CASE RET_CD.
*      WHEN 1.
*         PERFORM DISPLAY_ERROR TABLES HR_ERROR.
*      WHEN 2.
*        PERFORM PRNT.
*    ENDCASE.
  ENDIF.




ENDFORM.                               " DISPLAY

*&---------------------------------------------------------------------*
*&      Form  PRNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_module.

  DATA: LANG LIKE SY-LANGU,
        CNT TYPE I,
        FORMNAME(30) TYPE C,
        SNAME LIKE T596F-SNAME,
        MONTH(15),
        YEAR(4),
        PGRSAL LIKE PC207-BETRG,       "Page Total for Gross Salary
        PEMPCT LIKE PC207-BETRG,       "Page Total for Grat. Contr.
        GGRSAL LIKE PC207-BETRG,       "Grand Total for Gross Salary
        GEMPCT LIKE PC207-BETRG,       "Grand Total for Grat. Contr.
        N1 TYPE I VALUE 0,
        N2 TYPE I VALUE 0,
        P TYPE I VALUE 39,
        TCOUNTER TYPE I,
        LAST_PG(1),
        PAGENO TYPE I,
        FLG(1),
        ENDDATE LIKE SY-DATUM,
        R1 TYPE I,
        R2 TYPE F.

  MOVE 'EN' TO LANG.
  ENDDATE = PN-ENDDA.

  IF DISP_FLG_LOT LT 1.
    FORMNAME = LAYOUT.
    LANG     = 'EN'.
  ELSE.
* Read table T596F.
    SNAME = '40INSAN'.
    PERFORM RE596F USING SNAME ENDDATE.
    PERFORM (T596F-MODNA) IN PROGRAM (T596F-PGMNA) CHANGING FORMNAME.
    PERFORM GET_OUTPUT_TYPE USING    ENDDATE
                            CHANGING FORMNAME
                                     P_SCRIPT.
    IF P_SCRIPT IS INITIAL.
       P_PDF = 'X'.
    ELSE.
       CLEAR P_PDF.
    ENDIF.
  ENDIF.

***** START OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****

  IF P_SCRIPT = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****


  PERFORM OPEN_FORM USING LANG.

***** START OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****

  ENDIF.

***** END OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****



* Intializing Counters.
  N1 = 1.
  N2 = N1 + P.

  DESCRIBE TABLE DISP_BODY LINES R1.
  WHILE ( 0 < 1 ).

***** START OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****

  IF P_SCRIPT = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****



    PERFORM START_FORM USING FORMNAME LANG 'MPAGE'.

***** START OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****

  ENDIF.

***** END OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****



* Get Month/Year.
    PERFORM MONTH_NAME CHANGING MONTH YEAR.
    CONCATENATE MONTH(3) YEAR INTO MONTH SEPARATED BY SPACE.

***** START OF CHANGES FOR THE PDF FORM ON '27-12-2004' *****

    GV_MONTH_PDF = MONTH.

***** END OF CHANGES FOR THE PDF FORM ON '27-12-2004' *****


    PERFORM CONVERT_TO_SCRIPTVAR USING 'monyr' MONTH.

* Page No.
    PAGENO = PAGENO + 1.
    PERFORM CONVERT_TO_SCRIPTVAR USING 'pageno' PAGENO.


    CLEAR : PGRSAL, PEMPCT, TCOUNTER.

    LOOP AT DISP_BODY FROM N1 TO N2.

      TCOUNTER = TCOUNTER + 1.
      CNT = CNT + 1.

***** START OF CHANGES FOR THE PDF FORM ON '27-12-2004' *****

    IF P_SCRIPT = 'X'.

***** END OF CHANGES FOR THE PDF FORM ON '27-12-2004' *****


* Get Employee Details.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'serno' CNT.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'pernr' DISP_BODY-PERNR.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'ename' DISP_BODY-ENAME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'grsal' DISP_BODY-GRSAL.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'empct' DISP_BODY-EMPCT.

      PERFORM WRITE_FORM USING 'INFO' 'APPEND' 'BODY' 'MAIN'.

***** START OF CHANGES FOR THE PDF FORM ON '27-12-2004' *****

    ENDIF.

***** END OF CHANGES FOR THE PDF FORM ON '27-12-2004' *****


* Page Totals.
      PGRSAL = PGRSAL + DISP_BODY-GRSAL.
      PEMPCT = PEMPCT + DISP_BODY-EMPCT.

    ENDLOOP.

    R2 = R1 / N2.

*    IF TCOUNTER < 40.
    IF R2 LE 1.
       FLG = 1.
    ELSE.
       N1 = N2 + 1.
       N2 = N1 + P.
    ENDIF.

* Grand Totals.
    GGRSAL = GGRSAL + PGRSAL.
    GEMPCT = GEMPCT + PEMPCT.

***** START OF CHANGES FOR THE PDF FORM ON '27-12-2004' *****

    IF P_SCRIPT = 'X'.

***** END OF CHANGES FOR THE PDF FORM ON '27-12-2004' *****





    LAST_PG = ' '.
    PERFORM CONVERT_TO_SCRIPTVAR USING 'last_pg' LAST_PG.

    PERFORM CONVERT_TO_SCRIPTVAR USING 'pgrsal' PGRSAL.
    PERFORM CONVERT_TO_SCRIPTVAR USING 'pempct' PEMPCT.

    PERFORM WRITE_FORM USING 'PGTOT' 'APPEND' 'BODY' 'MAIN'.

    IF FLG IS INITIAL.
      PERFORM END_FORM.
    ELSE.
      EXIT.
    ENDIF.


***** START OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****

    ENDIF.

   IF FLG IS INITIAL.

    ELSE.
      EXIT.
    ENDIF .


***** END OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****

  ENDWHILE.

  LAST_PG = 'Y'.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'last_pg' LAST_PG.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'ggrsal' GGRSAL.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'gempct' GEMPCT.


***** START OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****

 GV_GRAND_TOT_PDF = GGRSAL.
 GV_PER_TOT_PDF = GEMPCT.

 IF P_SCRIPT = 'X'.

***** END OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****


  PERFORM WRITE_FORM USING 'GRTOT' 'APPEND' 'BODY' 'MAIN'.
  PERFORM END_FORM.

  PERFORM CLOSE_FORM.


***** START OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****

ENDIF.

IF P_PDF = 'X'.

GV_FPNAME = FORMNAME.

***** TO EXECUTE THE FUNCTION MODULES REQUIRED FOR *****
***** PDF OUTPUT *****
PERFORM PDF_DISPLAY.

ENDIF.


***** END OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****



ENDFORM.                               " PRNT

FORM error_cases.
  PERFORM DISPLAY_ERROR TABLES HR_ERROR.
ENDFORM.


***** START OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****

*&---------------------------------------------------------------------*
*&      Form  PDF_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PDF_DISPLAY .

TRY.

 CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
   EXPORTING
     i_name                    = GV_FPNAME
  IMPORTING
    E_FUNCNAME                 = GV_FM_NAME
    E_INTERFACE_TYPE           = GV_E_INTERFACE_TYPE.
           .

  CATCH CX_ROOT INTO GV_W_CX_ROOT.
      GV_MESG = GV_W_CX_ROOT->GET_TEXT( ).
      MESSAGE GV_MESG TYPE 'E'.

ENDTRY.


  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams      = gv_fp_outputparams
    EXCEPTIONS
      CANCEL                = 1
      USAGE_ERROR           = 2
      SYSTEM_ERROR          = 3
      INTERNAL_ERROR        = 4
      OTHERS                = 5
            .
  IF sy-subrc <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


***** APPENDS DATA OF DISP_BODY TABLE *****
***** TO GT_OUTPUT_TAB TABLE *****

loop at disp_body into gs_output_tab.

   gs_output_tab-SERNO = SY-TABIX.
   append gs_output_tab to gt_output_tab.

endloop.

***** CALLS THE CREATED FUNCTION MODULE *****

  CALL FUNCTION GV_FM_NAME
    EXPORTING
        GS_PER_TOT_PDF = GV_PER_TOT_PDF
        GS_GRAND_TOT_PDF = GV_GRAND_TOT_PDF
        MONTH = GV_MONTH_PDF
        DISP_BODY =  gt_output_tab
    EXCEPTIONS
        USAGE_ERROR = 1
        SYSTEM_ERROR = 2
        INTERNAL_ERROR = 3
        OTHERS = 4.


CALL FUNCTION 'FP_JOB_CLOSE'
* IMPORTING
*   E_RESULT             =
 EXCEPTIONS
    USAGE_ERROR          = 1
    SYSTEM_ERROR         = 2
    INTERNAL_ERROR       = 3
    OTHERS               = 4
          .
IF sy-subrc <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.


ENDFORM.                    " PDF_DISPLAY

***** ENDE OF CHANGES FOR PDF FORM BY C5061983 '27-12-2004' *****
