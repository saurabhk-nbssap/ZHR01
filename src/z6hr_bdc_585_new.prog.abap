*&---------------------------------------------------------------------*
*& Report  Z6HR_BDC_585_NEW
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6HR_BDC_585_NEW.

*& Report  Z_HR_UPLOAD_INFOTYPE_0585


*report  z_hr_upload_infotype_0585.

TABLES:PA0585.

DATA:BEGIN OF IT_585 OCCURS 0 ,

     PERNR TYPE RP50G-PERNR,   "Personnel no
     BEGDA TYPE PA0585-BEGDA,  "Begin Date
     ENDDA TYPE PA0585-ENDDA,  "End data
     SBS01 TYPE PA0585-SBS01,  "Sub section code
     SBD01 TYPE PA0585-SBD01,  "Sub section division number
     PCN01(12) TYPE C,         "Proposed contribution
     SBS02 TYPE PA0585-SBS02,  "Sub section code
     SBD02 TYPE PA0585-SBD02,  "Sub section division number
     PCN02(12) TYPE C,         "Proposed contribution
     SBS03 TYPE PA0585-SBS03,  "Sub section code
     SBD03 TYPE PA0585-SBD03,  "Sub section division number
     PCN03(12) TYPE C,         "Proposed contribution
     SBS04 TYPE PA0585-SBS04,  "Sub section code
     SBD04 TYPE PA0585-SBD04,  "Sub section division number
     PCN04(12) TYPE C,         "Proposed contribution
     SBS05 TYPE PA0585-SBS05,  "Sub section code
     SBD05 TYPE PA0585-SBD05,  "Sub section division number
     PCN05(12) TYPE C,         "Proposed contribution
     SBS06 TYPE PA0585-SBS06,  "Sub section code
     SBD06 TYPE PA0585-SBD06,  "Sub section division number
     PCN06(12) TYPE C,         "Proposed contribution
     SBS07 TYPE PA0585-SBS07,  "Sub section code
     SBD07 TYPE PA0585-SBD07,  "Sub section division number
     PCN07(12) TYPE C,         "Proposed contribution
     SBS08 TYPE PA0585-SBS08,  "Sub section code
     SBD08 TYPE PA0585-SBD08,  "Sub section division number
     PCN08(12) TYPE C,         "Proposed contribution
     SBS09 TYPE PA0585-SBS09,  "Sub section code
     SBD09 TYPE PA0585-SBD09,  "Sub section division number
     PCN09(12) TYPE C,         "Proposed contribution
     SBS10 TYPE PA0585-SBS10,  "Sub section code
     SBD10 TYPE PA0585-SBD10,  "Sub section division number
     PCN10(12) TYPE C,         "Proposed contribution
     SBS11 TYPE PA0585-SBS11,  "Sub section code
     SBD11 TYPE PA0585-SBD12,  "Sub section division number
     PCN11(12) TYPE C,         "Proposed contribution
     SBS12 TYPE PA0585-SBS12,  "Sub section code
     SBD12 TYPE PA0585-SBD12,  "Sub section division number
     PCN12(12) TYPE C,         "Proposed contribution
     SBS13 TYPE PA0585-SBS13,  "Sub section code
     SBD13 TYPE PA0585-SBD13,  "Sub section division number
     PCN13(12) TYPE C,         "Proposed contribution
     SBS14 TYPE PA0585-SBS14,  "Sub section code
     SBD14 TYPE PA0585-SBD14,  "Sub section division number
     PCN14(12) TYPE C,         "Proposed contribution
     SBS15 TYPE PA0585-SBS15,  "Sub section code
     SBD15 TYPE PA0585-SBD15,  "Sub section division number
     PCN15(12) TYPE C,         "Proposed contribution
     END OF IT_585.

DATA:WA_585A LIKE  IT_585.

SELECTION-SCREEN: BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-000.
PARAMETERS: P_FILE TYPE  RLGRAP-FILENAME  OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK BLK1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

* *-to locate file in a directory (on value request)

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      STATIC        = 'X'
    CHANGING
      FILE_NAME     = P_FILE
    EXCEPTIONS
      MASK_TOO_LONG = 1
      OTHERS        = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM EMP_INVT.


*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA.
*            READ FILE INTO INTERNAL TABLE.
  DATA: LV_FNAME TYPE STRING.
  LV_FNAME = P_FILE.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = LV_FNAME
      FILETYPE                = 'ASC'
      HAS_FIELD_SEPARATOR     = '#'
    TABLES
      DATA_TAB                = IT_585[]
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8
      HEADER_NOT_ALLOWED      = 9
      SEPARATOR_NOT_ALLOWED   = 10
      HEADER_TOO_LONG         = 11
      UNKNOWN_DP_ERROR        = 12
      ACCESS_DENIED           = 13
      DP_OUT_OF_MEMORY        = 14
      DISK_FULL               = 15
      DP_TIMEOUT              = 16
      OTHERS                  = 17.

  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE E000(ZHRMSG) WITH LV_FNAME.
  ENDIF.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  emp_invt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EMP_INVT.
  DATA : WA_0585  TYPE P0585,
         IT_RETURN TYPE BAPIRETURN1 OCCURS 0 WITH HEADER LINE,
         IT_05851  TYPE P0585 OCCURS 0,
         IT_PA0585 TYPE STANDARD TABLE OF PA0585 WITH HEADER LINE .

  LOOP AT IT_585 INTO WA_585A.
    MOVE-CORRESPONDING WA_585A TO WA_0585.
    WA_0585-INFTY = '0585'.
    APPEND WA_0585 TO IT_05851.
    CLEAR:WA_0585 ,
          WA_585A.
  ENDLOOP.
  SORT IT_05851 BY PERNR.
  SELECT * FROM PA0585 INTO TABLE IT_PA0585
  FOR ALL ENTRIES IN IT_05851
  WHERE PERNR = IT_05851-PERNR.

  LOOP AT IT_05851 INTO WA_0585.
**            Convert START Date into internal format (ex: 10/06/04 -> 20040610
*    call function 'CONVERSION_EXIT_DATEX_INPUT'
*      exporting
*        input  = wa_0585-begda
*      importing
*        output = wa_0585-begda.
**                Convert END Date into internal format (ex: 10/06/04 -> 20040610 )
*
*    call function 'CONVERSION_EXIT_DATEX_INPUT'
*      exporting
*        input  = wa_0585-endda
*      importing
*        output = wa_0585-endda.
*******Function module to lock personnel no

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        NUMBER = WA_0585-PERNR.
*     IMPORTING
*        return = WA_RETURN.

    READ TABLE IT_PA0585 WITH KEY PERNR = WA_0585-PERNR
                                BEGDA = WA_0585-BEGDA
                                ENDDA = WA_0585-ENDDA.
    IF SY-SUBRC IS INITIAL.
      MOVE WA_0585-SBS14 TO IT_PA0585-SBS14.
      MOVE WA_0585-SBD14 TO IT_PA0585-SBD14.
      MOVE WA_0585-PCN14 TO IT_PA0585-PCN14.
      MOVE WA_0585-PCN14 TO IT_PA0585-ACN14.

      MOVE-CORRESPONDING IT_PA0585 TO WA_0585.


      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
       EXPORTING
         INFTY                  =  '0585'
         NUMBER                 =  WA_0585-PERNR
*      SUBTYPE                =
*      OBJECTID               =
*      LOCKINDICATOR          =
        VALIDITYEND            =  WA_0585-ENDDA
        VALIDITYBEGIN          =  WA_0585-BEGDA
*      RECORDNUMBER           =
         RECORD                 = WA_0585
         OPERATION              = 'MOD'
         TCLAS                  =  'A'
        DIALOG_MODE            = '1'
        NOCOMMIT               =  ''.
*      VIEW_IDENTIFIER        =
*      SECONDARY_RECORD       =
*    IMPORTING
*      RETURN                 =   IT_RETURN[].
*      KEY                    =
      IF SY-SUBRC = 0.
        WRITE:/ 'IT 585 Modified successfully for Emp:' , WA_0585-PERNR.
      ENDIF.
    ELSE.

      MOVE WA_0585-PCN14 TO WA_0585-ACN14.

      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          INFTY                  =  '0585'
          NUMBER                 =  WA_0585-PERNR
*      SUBTYPE                =
*      OBJECTID               =
*      LOCKINDICATOR          =
         VALIDITYEND            =  WA_0585-ENDDA
         VALIDITYBEGIN          =  WA_0585-BEGDA
*      RECORDNUMBER           =
          RECORD                 = WA_0585
          OPERATION              = 'INS'
          TCLAS                  =  'A'
         DIALOG_MODE            = '1'
         NOCOMMIT               =  ''.
*      VIEW_IDENTIFIER        =
*      SECONDARY_RECORD       =
*    IMPORTING
*      RETURN                 =   IT_RETURN[].
*      KEY                    =
      IF SY-SUBRC = 0.
        WRITE:/ 'IT 585 Uploaded successfully for Emp:' , WA_0585-PERNR.
      ENDIF.

    ENDIF.

*    Function module to unlock the personnel no

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        NUMBER = WA_0585-PERNR.
    CLEAR WA_0585.
  ENDLOOP.

ENDFORM.                    " EMP_INVT
