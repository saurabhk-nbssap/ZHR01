*&---------------------------------------------------------------------*
*& Report  ZHR_DELIMIT_ACTIVE_EMPLOYEE_SW
*&
*&---------------------------------------------------------------------*
*& Update endda for employee's family data
*&
*&---------------------------------------------------------------------*

REPORT  zhr_delimit_active_employee_sw.

DATA : g_pernr TYPE pa0000-pernr.
DATA : lt_pa0000  TYPE STANDARD TABLE OF pa0000,
       ls_pa0000 LIKE LINE OF lt_pa0000.
DATA : lt_pa0021  TYPE STANDARD TABLE OF p0021,
       ls_pa0021 LIKE LINE OF lt_pa0021.
DATA : ls_return LIKE bapireturn1.
DATA : ls_return1 LIKE bapireturn1.
DATA : ls_key LIKE bapipakey.
FIELD-SYMBOLS : <f_p0021> TYPE p0021.
FIELD-SYMBOLS : <f_p0021_record> TYPE p0021.
DATA pa0021 LIKE pa0021.
DATA ls_record LIKE p0021.


SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_pernr FOR g_pernr.
PARAMETERS: p_endda TYPE pa0000-endda OBLIGATORY. " End date for delimit the record
*PARAMETERS: p_begda TYPE pa0000-endda OBLIGATORY. " Start date for inserting new record
SELECTION-SCREEN END OF BLOCK a1.


* Select active employees from PA0000
SELECT * FROM pa0000 INTO TABLE lt_pa0000
  WHERE pernr IN s_pernr
  AND stat2 = '3'.

* Read Family Data
IF lt_pa0000[] IS NOT INITIAL.
  SELECT * FROM pa0021 INTO CORRESPONDING FIELDS OF TABLE lt_pa0021
    FOR ALL ENTRIES IN lt_pa0000
    WHERE pernr = lt_pa0000-pernr
    AND endda = '99991231'.
ENDIF.
*** Delimiting logic starts here

LOOP AT lt_pa0021 ASSIGNING <f_p0021>.
  <f_p0021>-infty  = '0021'.
 ls_record = <f_p0021>.
  ls_record-endda = p_endda.

  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
  EXPORTING
    number        = <f_p0021>-pernr
 IMPORTING
   return         = ls_return.

* Update record
CALL FUNCTION 'HR_INFOTYPE_OPERATION'
  EXPORTING
    infty          = '0021'
    number         = <f_p0021>-pernr
    subtype        = <f_p0021>-subty
    objectid       = <f_p0021>-objps
    lockindicator  = <f_p0021>-sprps
    validityend    = <f_p0021>-endda
    validitybegin  = <f_p0021>-begda
    recordnumber   = <f_p0021>-seqnr
    record         = ls_record
    operation      = 'MOD'
   nocommit = ''
 IMPORTING
   return                 = ls_return1
   key                    = ls_key  .
 IF NOT  ls_return1 IS INITIAL.
    WRITE : /  ls_return1-message.
  ELSE.
    WRITE : / 'Record upadated successfully For Employee:' , <f_p0021>-pernr , 'Subtype: ', <f_p0021>-subty .
  ENDIF.

* Unlock Pernr
  CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
       EXPORTING
            number = <f_p0021>-pernr.
ENDLOOP.
