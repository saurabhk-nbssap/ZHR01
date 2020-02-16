class ZCL_IM_6HR026_BEGDA_CHANGE definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6HR026_BEGDA_CHANGE
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_HRXSS_PER_BEGDA .
protected section.
*"* protected components of class ZCL_IM_6HR026_BEGDA_CHANGE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_6HR026_BEGDA_CHANGE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_6HR026_BEGDA_CHANGE IMPLEMENTATION.


method IF_EX_HRXSS_PER_BEGDA~DEFAULT_DATE.

 if infty = '0585' or infty = '0586' or infty = '0185'.
    begda = '20060401'.
  endif.



endmethod.
ENDCLASS.
