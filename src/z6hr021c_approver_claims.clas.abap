class Z6HR021C_APPROVER_CLAIMS definition
  public
  final
  create public .

*"* public components of class Z6HR021C_APPROVER_CLAIMS
*"* do not include other source files here!!!
public section.

  interfaces IF_AC_NEXT_APPROVER .
  interfaces IF_BADI_INTERFACE .
protected section.
*"* protected components of class Z6HR021C_APPROVER_CLAIMS
*"* do not include other source files here!!!
private section.
*"* private components of class Z6HR021C_APPROVER_CLAIMS
*"* do not include other source files here!!!
ENDCLASS.



CLASS Z6HR021C_APPROVER_CLAIMS IMPLEMENTATION.


method IF_AC_NEXT_APPROVER~GET_NEXT_APPROVER.
*  IF IFD_APLVL EQ 1.
*    efd_apgrp = 'DEFAULT_APPROVE'.
*  ENDIF.

  DATA : LV_SACHA TYPE P0001-SACHA.
  DATA : LV_USRID TYPE SY-UNAME.

if IFD_APLVL eq 1.
   SELECT SINGLE SACHA FROM PA0001 INTO LV_SACHA
                                   WHERE PERNR = ist_p0001-pernr
                                    AND  BEGDA LE SY-DATUM
                                    AND  ENDDA GE SY-DATUM.

    IF SY-SUBRC EQ 0.
      SELECT SINGLE USRID FROM T526 INTO LV_USRID WHERE WERKS = IST_P0001-WERKS
                                                  AND SACHX = LV_SACHA.
      IF NOT LV_USRID IS INITIAL.
        SELECT SINGLE PERNR FROM PA0105 INTO EFD_APERN  WHERE USRTY = '0001'
                                                          AND USRID = LV_USRID
                                                          AND BEGDA LE SY-DATUM
                                                          AND ENDDA GE SY-DATUM.
        efd_apgrp = 'DEFAULT_APPROVE'.
      ENDIF.
    ENDIF.
endif.

endmethod.
ENDCLASS.
