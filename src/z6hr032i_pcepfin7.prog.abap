*----------------------------------------------------------------------*
*   INCLUDE PCEPFIN7                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_ADDRNO
*&---------------------------------------------------------------------*
*      -->P_TRUST_ID  text
*----------------------------------------------------------------------*
FORM GET_ADDRNO USING    P_TRUST_ID.
  SELECT SINGLE TSTAD INTO T7INF1-TSTAD FROM T7INF1
                      WHERE TSTID = TRUST_ID.
ENDFORM.                               " GET_ADDRNO

*&---------------------------------------------------------------------*
*&      Form  GET_ACC_FACTOR
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      -->P_KONST   text
*      -->P_DATE  text
*      <--P_VALUE  text
*----------------------------------------------------------------------*
FORM GET_ACC_FACTOR USING    VALUE(P_KONST)
                             P_DATE
                    CHANGING P_VALUE LIKE T511K-KWERT.

  SELECT SINGLE KWERT INTO T511K-KWERT FROM T511K
                      WHERE KONST = P_KONST AND
                            BEGDA LE P_DATE AND ENDDA GE P_DATE.
  IF SY-SUBRC = 0.
    P_VALUE = T511K-KWERT.
  ELSE.
    MESSAGE E059(HRPADIN01) WITH 'T511K' P_KONST.
  ENDIF.

ENDFORM.                               " GET_ACC_FACTOR

*---------------------------------------------------------------------*
*       FORM re596f                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  $sname                                                        *
*  -->  $datum                                                        *
*---------------------------------------------------------------------*
FORM RE596F USING $SNAME $DATUM.
  DATA: KEY(8) TYPE C.

  CHECK T596F-SNAME NE $SNAME OR
        T596F-BEGDA GT $DATUM OR T596F-ENDDA LT $DATUM.
  SELECT * FROM T596F WHERE SNAME EQ $SNAME
                        AND BEGDA LE $DATUM
                        AND ENDDA GE $DATUM.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    MESSAGE E059(HRPADIN01) WITH 'T596F' $SNAME.
*   No entry in table & for key &
    PERFORM BUILD_ERROR TABLES HR_ERROR
                       USING SPACE SY-MSGID SY-MSGNO
                       SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
  ENDIF.
ENDFORM.                               "RE596F

