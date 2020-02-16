class ZGET_CIT_ADDRESS definition
  public
  final
  create public .

*"* public components of class ZGET_CIT_ADDRESS
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_HR_IN_ER_ADDRESS .
protected section.
*"* protected components of class ZGET_CIT_ADDRESS
*"* do not include other source files here!!!
private section.
*"* private components of class ZGET_CIT_ADDRESS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZGET_CIT_ADDRESS IMPLEMENTATION.


method IF_EX_HR_IN_ER_ADDRESS~GET_CIT_ADDRESS.
DATA: WA_T7INT5 TYPE T7INT5,
      WA_P0001 TYPE P0001,
      WA_ADRC TYPE ADRC.

READ TABLE p0001 INTO wa_p0001 INDEX 1.

IF WA_P0001-bukrs = '1000'.
  SELECT SINGLE * FROM T7INT5 INTO WA_T7INT5
                  WHERE TXGRP = '01'.
ELSEIF WA_P0001-bukrs = '2000'.
  SELECT SINGLE * FROM T7INT5 INTO WA_T7INT5
                  WHERE TXGRP = '02'.
ENDIF.

SELECT SINGLE * FROM ADRC INTO WA_ADRC
                WHERE ADDRNUMBER = WA_T7INT5-ADDRS.

SADR-STRAS = WA_ADRC-NAME_CO.
SADR-PFACH = WA_ADRC-STR_SUPPL1.
SADR-ORT01 = WA_ADRC-CITY1.
SADR-PSTLZ = WA_ADRC-POST_CODE1.
endmethod.


method IF_EX_HR_IN_ER_ADDRESS~GET_ER_ADDRESS.

endmethod.
ENDCLASS.
