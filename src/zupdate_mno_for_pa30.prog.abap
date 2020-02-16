*&---------------------------------------------------------------------*
*& Report  ZUPDATE_MNO_FOR_PA30
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zupdate_mno_for_pa30
       NO STANDARD PAGE HEADING LINE-SIZE 255.

**INCLUDE bdcrecx1.


TYPES: BEGIN OF ty_pa30,
* data element: PERNR_D
         pernr(038),
* data element: TIMRE
**         timr6(001),
* data element: PERNR_D
**         pernr_1(038),
* data element: TIMRE
**         timr6_1(001),
* data element: CHOIC
*         choic(035),
* data element: SUBTY
*         subty(004),
* data element: BEGDA
**         begda(010),
* data element: ENDDA
*         endda_008(010),
* data element: SYSID
         usrid(030),
       END OF ty_pa30.
DATA: it_pa30        TYPE TABLE OF ty_pa30,
      wa_pa30        TYPE          ty_pa30,
      wa_pa          TYPE          ty_pa30,
      i_tab_raw_data TYPE  truxs_t_text_data.
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   e_group_opened.
*       message texts
TABLES: t100.
*** End generated data section ***

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_file TYPE ibipparms-path.
SELECTION-SCREEN END OF BLOCK b1.
selection-screen begin of block b2 with frame title text-003.
  PARAMETERS ctumode1 LIKE ctu_params-dismode DEFAULT 'E'.
  PARAMETERS cupdate1 LIKE ctu_params-updmode DEFAULT 'S'.
selection-screen end of block b2.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name = syst-cprog
    IMPORTING
      file_name    = p_file.

START-OF-SELECTION.
  PERFORM xls_to_bdc.
  PERFORM it_to_bdc.


FORM bdc_transaction USING tcode.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.
* batch input session
***  IF SESSION = 'X'.
***    CALL FUNCTION 'BDC_INSERT'
***         EXPORTING TCODE     = TCODE
***         TABLES    DYNPROTAB = BDCDATA.
***    IF SMALLLOG <> 'X'.
***      WRITE: / 'BDC_INSERT'(I03),
***               TCODE,
***               'returncode:'(I05),
***               SY-SUBRC,
***               'RECORD:',
***               SY-INDEX.
***    ENDIF.
**** call transaction using
***  ELSE.
  REFRESH messtab.
  CALL TRANSACTION 'PA30' USING bdcdata
                   MODE   ctumode1
                   UPDATE cupdate1
                   MESSAGES INTO messtab.
***    L_SUBRC = SY-SUBRC.
***    IF SMALLLOG <> 'X'.
  WRITE: / 'CALL_TRANSACTION',
           tcode,
           'returncode:'(I05),
           l_subrc,
           'RECORD:',
           sy-index.
  LOOP AT messtab.
    MESSAGE ID     messtab-msgid
            TYPE   messtab-msgtyp
            NUMBER messtab-msgnr
            INTO l_mstring
            WITH messtab-msgv1
                 messtab-msgv2
                 messtab-msgv3
                 messtab-msgv4.
    WRITE: / messtab-msgtyp, l_mstring(250).
  ENDLOOP.
***      SKIP.
***    ENDIF.
***** Erzeugen fehlermappe ************************************************
***    IF L_SUBRC <> 0 AND E_GROUP <> SPACE.
***      IF E_GROUP_OPENED = ' '.
***        CALL FUNCTION 'BDC_OPEN_GROUP'
***             EXPORTING  CLIENT   = SY-MANDT
***                        GROUP    = E_GROUP
***                        USER     = E_USER
***                        KEEP     = E_KEEP
***                        HOLDDATE = E_HDATE.
***         E_GROUP_OPENED = 'X'.
***      ENDIF.
***      CALL FUNCTION 'BDC_INSERT'
***           EXPORTING TCODE     = TCODE
***           TABLES    DYNPROTAB = BDCDATA.
***    ENDIF.
***  ENDIF.
  REFRESH bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  IF fval <> space.
    CLEAR bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    APPEND bdcdata.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  XLS_TO_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM xls_to_bdc .
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = i_tab_raw_data
      i_filename           = p_file
    TABLES
      i_tab_converted_data = it_pa30
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IT_TO_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM it_to_bdc .
  LOOP AT it_pa30 INTO wa_pa30.

    SELECT SINGLE pernr usrid FROM pa0105 INTO wa_pa
                              WHERE pernr = wa_pa30-pernr
                                AND subty = 'CELL'
                                AND endda >=  sy-datum.

    IF sy-subrc = 0.

      PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RP50G-PERNR'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_field       USING 'RP50G-PERNR'
                                    wa_pa30-pernr.
**      PERFORM bdc_field       USING 'RP50G-TIMR6'
**                                    wa_pa30-timr6.
      PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=MOD'.
**      PERFORM bdc_field       USING 'RP50G-PERNR'
**                                    wa_pa30-pernr_1.
**      PERFORM bdc_field       USING 'RP50G-TIMR6'
**                                    wa_pa30-timr6_1.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RP50G-SUBTY'.
      PERFORM bdc_field       USING 'RP50G-CHOIC'
                                     '105'    .        "wa_pa30-choic .
      PERFORM bdc_field       USING 'RP50G-SUBTY'
                                      'CELL' .               "wa_pa30-subty.
      PERFORM bdc_dynpro      USING 'MP010500' '2000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'P0105-USRID'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=UPD'.
**      PERFORM bdc_field       USING 'P0105-BEGDA'
**                                    wa_pa30-begda.
*    PERFORM bdc_field       USING 'P0105-ENDDA'
*                                  wa_pa30-endda_008.
*    if  bdcdata-fnam = 'P0105-USRID'
*    and bdcdata-fval = ' '
*    and bdcdata-fval = 'XX'.

      PERFORM bdc_field       USING 'P0105-USRID'

                                    wa_pa30-usrid.
*     endif.
      PERFORM bdc_transaction USING 'PA30'.
*     else.
*      Need to create new bdc
    ENDIF.
  ENDLOOP.
ENDFORM.
