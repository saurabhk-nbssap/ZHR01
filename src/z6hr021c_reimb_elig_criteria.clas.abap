class Z6HR021C_REIMB_ELIG_CRITERIA definition
  public
  final
  create public .

*"* public components of class Z6HR021C_REIMB_ELIG_CRITERIA
*"* do not include other source files here!!!
public section.

  interfaces IF_AC_CHANGE_ELIGIBILITY .
  interfaces IF_BADI_INTERFACE .
protected section.
*"* protected components of class Z6HR021C_REIMB_ELIG_CRITERIA
*"* do not include other source files here!!!
private section.
*"* private components of class Z6HR021C_REIMB_ELIG_CRITERIA
*"* do not include other source files here!!!
ENDCLASS.



CLASS Z6HR021C_REIMB_ELIG_CRITERIA IMPLEMENTATION.


method IF_AC_CHANGE_ELIGIBILITY~CHANGE_ELIGIBILITY.
*DATA : WA_CTB_ELIGB TYPE PIN_REIMB_DATA2.
*
*WA_CTB_ELIGB-PERNR = IFD_PERNR.
*WA_CTB_ELIGB-RETYP = 'SMED'.
*WA_CTB_ELIGB-VALID_FROM = SY-DATUM.
*WA_CTB_ELIGB-VALID_TO = '99991231'.
*WA_CTB_ELIGB-TOTAMT = 1000.
*APPEND WA_CTB_ELIGB TO CTB_ELIGB.
*CLEAR  WA_CTB_ELIGB.

endmethod.
ENDCLASS.
