*----------------------------------------------------------------------*
*   INCLUDE PCSANIN7                                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->$LANGU  text
*      <--$ENAME  text
*----------------------------------------------------------------------*
FORM READ_NAME USING    $LANGU
               CHANGING $ENAME.

  CALL FUNCTION 'RP_EDIT_NAME'
              EXPORTING
                   FORMAT    ='01'
                   LANGU     = $LANGU
                   MOLGA     ='40'
                  PP0002     = P0002
             IMPORTING
                  EDIT_NAME = $ENAME
*                    RETCODE   =
             EXCEPTIONS
                  OTHERS    = 1.


ENDFORM.                               " READ_NAME

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TEXT_BEG  text
*----------------------------------------------------------------------*
FORM PROGRESS_INDICATOR USING TEXT_BEG.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = TEXT_BEG
       EXCEPTIONS
            OTHERS = 1.

ENDFORM.                               " PROGRESS_INDICATOR

*&---------------------------------------------------------------------*
*&      Form  BUILD_ERROR
*&---------------------------------------------------------------------*
FORM BUILD_ERROR TABLES   HR_ERROR STRUCTURE HR_ERROR
                 USING    $PERNR
                          $MSGID
                          $MSGNO
                          $MSGV1
                          $MSGV2
                          $MSGV3
                          $MSGV4.

  HR_ERROR-PERNR = $PERNR.
  HR_ERROR-ARBGB = $MSGID.
  HR_ERROR-MSGTY = 'E'.
  HR_ERROR-MSGNO = $MSGNO.
  HR_ERROR-MSGV1 = $MSGV1.
  HR_ERROR-MSGV2 = $MSGV2.
  HR_ERROR-MSGV3 = $MSGV3.
  HR_ERROR-MSGV4 = $MSGV4.
  APPEND HR_ERROR.

ENDFORM.                               " BUILD_ERROR

*&---------------------------------------------------------------------*
*&      Form  MONTH_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_MONTH  text
*      <--P_YEAR  text
*----------------------------------------------------------------------*
FORM MONTH_NAME CHANGING P_MONTH
                         P_YEAR.

DATA:  MN  TYPE  I .

  P_YEAR = PN-BEGDA(4).
  MN = PN-BEGDA+4(2).

  CASE MN.
    WHEN  1.
       P_MONTH = 'JANUARY'.
    WHEN  2.
       P_MONTH = 'FEBRUARY'.
    WHEN  3.
       P_MONTH = 'MARCH'.
    WHEN  4.
       P_MONTH = 'APRIL'.
    WHEN  5.
       P_MONTH = 'MAY'.
    WHEN  6.
       P_MONTH = 'JUNE'.
    WHEN  7.
       P_MONTH = 'JULY'.
    WHEN  8.
       P_MONTH = 'AUGUST'.
    WHEN  9.
       P_MONTH = 'SEPTEMBER'.
    WHEN  10.
       P_MONTH = 'OCTOBER'.
    WHEN  11.
       P_MONTH = 'NOVEMBER'.
    WHEN  12.
       P_MONTH = 'DECEMBER'.
  ENDCASE.


ENDFORM.                    " MONTH_NAME

*&---------------------------------------------------------------------*
*&      Form  LEAD_SPC_OR_SPC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0413   text                                                *
*      <--P_DISP_BODY-SEQNO  text                                      *
*----------------------------------------------------------------------*
FORM LEAD_SPC_OR_SPC USING $CONST CHANGING $FIELD.
  DATA: LEN TYPE I,
        CNT TYPE I,
        TEMP(50).

  CONDENSE $FIELD NO-GAPS.
  LEN = STRLEN( $FIELD ).
  IF LEN < $CONST AND LEN NE 0.        "#EC PORTABLE
    CNT = $CONST - LEN.
    MOVE $FIELD+0(LEN) TO TEMP+0(LEN).
    MOVE TEMP+0(LEN) TO $FIELD+CNT(LEN).
    WHILE CNT NE 0.
      CNT = CNT - 1.
      MOVE ' ' TO $FIELD+CNT(1).
    ENDWHILE.
  ENDIF.
  IF LEN = 0.
    CNT = $CONST.
    WHILE CNT GT 0.
      CNT = CNT - 1.
      MOVE ' ' TO $FIELD+CNT(1).
    ENDWHILE.
  ENDIF.

ENDFORM.                               " LEAD_SPC_OR_SPC

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->HR_ERROR  text
*----------------------------------------------------------------------*
FORM DISPLAY_ERROR TABLES HR_ERROR STRUCTURE HRERROR.
  DATA: NUM TYPE I.

  DESCRIBE TABLE HR_ERROR LINES NUM.
  IF NUM > 0.
    CALL FUNCTION 'HR_DISPLAY_ERROR_LIST'
    EXPORTING
         NO_POPUP         = ' '
         NO_PRINT         = 'X'
*         NO_IMG           = ' '
*         LINESIZE         = SY-LINSZ
     TABLES
          ERROR            = HR_ERROR
         EXCEPTIONS
              INVALID_LINESIZE = 1
              OTHERS           = 2.
  ELSE.
    MESSAGE S165(HRPADIN01).
*   There are no errors
    if sy-batch = 'X'.
      write: TEXT-NER.
    ENDIF.
  ENDIF.


ENDFORM.                    " DISPLAY_ERROR

*&---------------------------------------------------------------------*
*&      Form  RE596F
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->$SNAME  text
*      -->$DATUM  text
*----------------------------------------------------------------------*
FORM RE596F USING  $SNAME
                   $DATUM .

  CHECK T596F-SNAME NE $SNAME OR
        T596F-BEGDA GT $DATUM OR T596F-ENDDA LT $DATUM.
  SELECT * FROM T596F WHERE SNAME EQ $SNAME
                        AND BEGDA LE $DATUM
                        AND ENDDA GE $DATUM.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    MESSAGE E145(HRPADIN01).
*   Layout Form set either not defined or not valid
    PERFORM BUILD_ERROR TABLES HR_ERROR
                       USING SPACE SY-MSGID SY-MSGNO
                       SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
  ENDIF.

ENDFORM.                    " RE596F
