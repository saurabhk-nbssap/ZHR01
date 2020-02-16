*----------------------------------------------------------------------*
*   INCLUDE PCEPFIN9                                                   *
*XXXNTnote number  <date>     Note<note number>:<short description>
*RBSNT794651       10032005   Note794651: Reason of leaving trust not
*                                         printed correctly
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&       Form  CHECK_NEW_SUBS
*&---------------------------------------------------------------------*
FORM CHECK_NEW_SUBS.

  LOOP AT PEPF WHERE TSTID = TRUST_ID.
    EXIT.
  ENDLOOP.
  IF PEPF-BEGDA GT PN-BEGDA.
    MAIN_TAB-NEW_THIS_MONTH = 1.
    PERFORM FILL_FORM5_TAB.
  ELSE.
    READ TABLE LEPF WITH KEY ENDDA = LAST_MONTH_ENDDA.
    IF SY-SUBRC = 0.
    IF LEPF-TSTID <> TRUST_ID.
      MAIN_TAB-NEW_THIS_MONTH = 1.
      PERFORM FILL_FORM5_TAB.
    ELSEIF REPONAME = 'FORM5'.
      REJECT.
     ENDIF.
* NO RECORD FOUND FOR LAST MONTH MEANS EMPLOYEE HAS JOINED TRUST THIS
* MONTH ONLY
    ELSE.
     MAIN_TAB-NEW_THIS_MONTH = 1.
     PERFORM FILL_FORM5_TAB.
    ENDIF.
  ENDIF.

ENDFORM.                               " CHECK_NEW_SUBS

*&---------------------------------------------------------------------*
*&      Form  CHECK_LEAVERS
*&---------------------------------------------------------------------*
FORM CHECK_LEAVERS.

**  NO. OF SUBSCRIBERS LEFT SERVICE.
*  LOOP AT PEPF WHERE TSTID = TRUST_ID.
*    CONTINUE.
*  ENDLOOP.
*  IF PEPF-ENDDA LT PN-ENDDA.
*    MAIN_TAB-LEFT_SERVICE = 1.
*    FORM10_TAB-DOL = PEPF-ENDDA.
*    FORM10_TAB-EEPFN = PEPF-EEPFN.
*    FORM10_TAB-PFREF = PEPF-PFREF.
*  ELSE.
*    LOOP AT P0587 WHERE BEGDA LE NEXT_MONTH_BEGDA AND
*                        ENDDA GE NEXT_MONTH_BEGDA.
*      IF P0587-TSTID <> TRUST_ID.
*        MAIN_TAB-LEFT_SERVICE = 1.
*        FORM10_TAB-DOL = PEPF-ENDDA.
*        FORM10_TAB-EEPFN = PEPF-EEPFN.
*        FORM10_TAB-PFREF = PEPF-PFREF.
*      ELSEIF REPONAME = 'FORM10'.
*        REJECT.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

*  NO. OF SUBSCRIBERS LEFT SERVICE.
DATA: BEGIN OF TEMP_PEPF OCCURS 10.
        INCLUDE STRUCTURE PC2_IN07.
DATA: END OF TEMP_PEPF.
DATA: WA_PEPF LIKE PC2_IN07.
DATA: FLAG_LEAVE_SERVICE,
      TEMP_ENDDA LIKE PN-ENDDA.

 CLEAR FLAG_LEAVE_SERVICE.
 TEMP_PEPF[] = PEPF[].
 SORT PEPF BY BEGDA DESCENDING.
 READ TABLE PEPF WITH KEY TSTID = TRUST_ID .
 IF SY-SUBRC = 0.
  MOVE-CORRESPONDING PEPF TO WA_PEPF.
 ENDIF.
 CLEAR PEPF.
 SORT PEPF BY BEGDA ASCENDING.

 LOOP AT PEPF WHERE TSTID NE TRUST_ID AND
                    BEGDA GT WA_PEPF-ENDDA.
  CLEAR TEMP_PEPF.
   SORT TEMP_PEPF DESCENDING.
   READ TABLE TEMP_PEPF INDEX 1.
   IF SY-SUBRC = 0.
    IF TEMP_PEPF-TSTID = TRUST_ID.
    LOOP AT P0587 WHERE BEGDA LE NEXT_MONTH_BEGDA AND
                        ENDDA GE NEXT_MONTH_BEGDA.
      IF P0587-TSTID <> TRUST_ID.
      IF FLAG_LEAVE_SERVICE NE 1.
        MAIN_TAB-LEFT_SERVICE = 1.
       FORM10_TAB-DOL = TEMP_PEPF-ENDDA.
       FORM10_TAB-EEPFN = TEMP_PEPF-EEPFN.
       IF TEMP_PEPF-PFRFN IS NOT INITIAL.       "PKT1152442
         FORM10_TAB-PFRFN = TEMP_PEPF-PFRFN.
       ELSE.
         FORM10_TAB-PFREF = TEMP_PEPF-PFREF.
       ENDIF.
       FLAG_LEAVE_SERVICE = 1.
       FORM10_TAB_ENTRY = 1.
      ENDIF.
      ELSEIF REPONAME = 'FORM10'.
        REJECT.
      ENDIF.
    ENDLOOP.
    ELSE.
     IF FLAG_LEAVE_SERVICE NE 1.
      IF WA_PEPF IS INITIAL.
       TEMP_ENDDA = PEPF-BEGDA - 1.
      ELSE.
       TEMP_ENDDA = WA_PEPF-ENDDA.
      ENDIF.
      READ TABLE PEPF WITH KEY ENDDA = TEMP_ENDDA.
       IF SY-SUBRC = 0.
        MAIN_TAB-LEFT_SERVICE = 1.
        FORM10_TAB-DOL = PEPF-ENDDA.
        FORM10_TAB-EEPFN = PEPF-EEPFN.
        IF PEPF-PFRFN IS NOT INITIAL.       "PKT1152442
          FORM10_TAB-PFRFN = PEPF-PFRFN.
        ELSE.
          FORM10_TAB-PFREF = PEPF-PFREF.
        ENDIF.
        FLAG_LEAVE_SERVICE = 1.
        FORM10_TAB_ENTRY = 1.
       ENDIF.
     ENDIF.
    ENDIF.
   ENDIF.
 ENDLOOP.

* If the trust is getting changed on the 1st day of the next month or
* if the employee is getting terminated middle of the month
 IF SY-SUBRC <> 0.
     LOOP AT P0587 WHERE BEGDA LE NEXT_MONTH_BEGDA AND
                         ENDDA GE NEXT_MONTH_BEGDA.
     IF P0587-TSTID <> TRUST_ID .
       MAIN_TAB-LEFT_SERVICE = 1.
       FORM10_TAB-DOL = WA_PEPF-ENDDA.
       FORM10_TAB-EEPFN = WA_PEPF-EEPFN.
       IF WA_PEPF-PFRFN IS NOT INITIAL.         "PKT1152442
         FORM10_TAB-PFRFN = WA_PEPF-PFRFN.
       ELSE.
         FORM10_TAB-PFREF = WA_PEPF-PFREF.
       ENDIF.
       FORM10_TAB_ENTRY = 1.
     ELSEIF REPONAME = 'FORM10'.
        REJECT.
     ENDIF.
     ENDLOOP.

* if the employee is getting terminated middle of the month
     IF SY-SUBRC <> 0.
      LOOP AT PEPF WHERE TSTID = TRUST_ID.
        CONTINUE.
      ENDLOOP.
      IF PEPF-ENDDA LE PN-ENDDA.
       MAIN_TAB-LEFT_SERVICE = 1.
       FORM10_TAB-DOL = PEPF-ENDDA.
       FORM10_TAB-EEPFN = PEPF-EEPFN.
       IF PEPF-PFRFN IS NOT INITIAL.          "PKT1152442
         FORM10_TAB-PFRFN = PEPF-PFRFN.
       ELSE.
         FORM10_TAB-PFREF = PEPF-PFREF.
       ENDIF.
       FORM10_TAB_ENTRY = 1.
      ELSEIF REPONAME = 'FORM10'.
        REJECT.
      ENDIF.
     ENDIF.
 ENDIF.


ENDFORM.                               " CHECK_LEAVERS

*&---------------------------------------------------------------------*
*&      Form CHECK_THIS_MONTH
*&---------------------------------------------------------------------*
FORM CHECK_THIS_MONTH.
  READ TABLE PEPF WITH KEY TSTID = TRUST_ID.
  IF SY-SUBRC <> 0.
    REJECT.
  ENDIF.
ENDFORM.                               " CHECK_THIS_MONTH

*&---------------------------------------------------------------------*
*&      Form  GET_FAMILY_INFO
*&---------------------------------------------------------------------*
FORM GET_FAMILY_INFO.
  RP-PROVIDE-FROM-LAST P0002 SPACE BEGDA1800 PN-ENDDA.
  IF PNP-SW-FOUND EQ '1'.
    IF P0002-GESCH = '1'.
      FORM5_TAB-GENDER = 'M'.
    ELSE.
      FORM5_TAB-GENDER = 'F'.
    ENDIF.
    FORM5_TAB-DOB = P0002-GBDAT.
  ELSE.
    MESSAGE S089(HRPADIN01) WITH '0002' PERNR-PERNR BEGDA1800 PN-ENDDA.
*   There is no infotype & for personnel no & from period & to &
    PERFORM BUILD_ERROR TABLES HR_ERROR
                       USING SPACE SY-MSGID SY-MSGNO
                       SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
  ENDIF.

  IF FORM5_TAB-GENDER = 'F' AND P0002-FAMST NE '0'.
    SUBTY = '1'.
  ELSE.
    SUBTY = '11'.
  ENDIF.

  RP-PROVIDE-FROM-LAST P0021 SUBTY BEGDA1800 PN-ENDDA.
  IF PNP-SW-FOUND EQ '1'.
    CONCATENATE P0021-FANAM P0021-FAVOR INTO FORM5_TAB-FATH_NAME
                                               SEPARATED BY SPACE.
    FORM10_TAB-FATH_NAME = FORM5_TAB-FATH_NAME.
  ELSE.
    MESSAGE S089(HRPADIN01) WITH '0021' PERNR-PERNR BEGDA1800 PN-ENDDA.
*   There is no infotype & for personnel no & from period & to &
    PERFORM BUILD_ERROR TABLES HR_ERROR
                       USING SPACE SY-MSGID SY-MSGNO
                       SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
  ENDIF.


ENDFORM.                               " GET_FAMILY_INFO

*&---------------------------------------------------------------------*
*&      Form  REASON_TO_LEAVE
*&---------------------------------------------------------------------*
*FORM REASON_TO_LEAVE.
*
*  IF FORM10_TAB-DOL EQ PN-ENDDA.
*
*    SELECT SINGLE MNTXT INTO REASON FROM T529T
*    WHERE SPRSL = SY-LANGU
*    AND   MASSN = P0000-MASSN.
*
*    MOVE REASON TO FORM10_TAB-REASON_LEAVE.
*
*  ELSE.
*
*  RP-PROVIDE-FROM-LAST P0000 SPACE PN-BEGDA PN-ENDDA.
*  IF PNP-SW-FOUND EQ '1'.
*    SELECT SINGLE MNTXT INTO REASON FROM T529T
*    WHERE SPRSL = SY-LANGU
*    AND   MASSN = P0000-MASSN.
*
*    MOVE REASON TO FORM10_TAB-REASON_LEAVE.
*    ENDIF.
*
*  ENDIF.
*
*ENDFORM.                    " REASON_TO_LEAVE

*&---------------------------------------------------------------------*
*&      Form  REASON_TO_LEAVE
*&---------------------------------------------------------------------*
FORM REASON_TO_LEAVE.                                       "RBSNT794651

  DATA TEMP TYPE D.

*Reason for leaving is printed only for employment status 0,1&2.
  IF FORM10_TAB-DOL EQ PN-ENDDA.
    TEMP = PN-ENDDA + 1.
    RP-PROVIDE-FROM-LAST P0000 SPACE PN-BEGDA TEMP.
    IF PNP-SW-FOUND EQ '1'.
      IF P0000-STAT2 NE '3'.

        SELECT SINGLE MGTXT INTO REASON FROM T530T
        WHERE SPRSL = SY-LANGU
        AND   MASSN = P0000-MASSN
        AND   MASSG = P0000-MASSG.
        MOVE REASON TO FORM10_TAB-REASON_LEAVE.

      ENDIF.

    ENDIF.

  ELSE.

    RP-PROVIDE-FROM-LAST P0000 SPACE PN-BEGDA PN-ENDDA.
    IF PNP-SW-FOUND EQ '1'.
      IF P0000-STAT2 NE '3'.

        SELECT SINGLE MGTXT INTO REASON FROM T530T
        WHERE SPRSL = SY-LANGU
        AND   MASSN = P0000-MASSN
        AND   MASSG = P0000-MASSG.
        MOVE REASON TO FORM10_TAB-REASON_LEAVE.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " REASON_TO_LEAVE
