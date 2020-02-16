class Z6HR016C_DET_APPR_LENCASH definition
  public
  final
  create public .

*"* public components of class Z6HR016C_DET_APPR_LENCASH
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_HRPBSIN_GET_LE_APPRV .
protected section.
*"* protected components of class Z6HR016C_DET_APPR_LENCASH
*"* do not include other source files here!!!
private section.
*"* private components of class Z6HR016C_DET_APPR_LENCASH
*"* do not include other source files here!!!
ENDCLASS.



CLASS Z6HR016C_DET_APPR_LENCASH IMPLEMENTATION.


method IF_HRPBSIN_GET_LE_APPRV~GET_LE_APPROVER.
  DATA : LV_SACHZ TYPE P0001-SACHZ.

if aprvl_lvl eq 1.
   SELECT SINGLE SACHZ FROM PA0001 INTO LV_SACHZ
                                   WHERE PERNR = PERNR
                                    AND  BEGDA LE SY-DATUM
                                    AND  ENDDA GE SY-DATUM.

    IF SY-SUBRC EQ 0.
      SELECT SINGLE USRID FROM T526 INTO APPRVR WHERE WERKS = PERS_AREA
                                                  AND SACHX = LV_SACHZ.

    ENDIF.
endif.
endmethod.
ENDCLASS.
