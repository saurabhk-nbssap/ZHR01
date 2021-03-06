class ZCL_IM_HR_PA20_PA30_AUTH definition
  public
  final
  create public .

public section.

  interfaces IF_EX_HRPAD00AUTH_TIME .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_HR_PA20_PA30_AUTH IMPLEMENTATION.


  method IF_EX_HRPAD00AUTH_TIME~BEGDA_ENDDA_COMPARE_EXIT.
  endmethod.


  method IF_EX_HRPAD00AUTH_TIME~CONSIDER_SY_DATUM_EXIT.
*    BREAK 10106.
  endmethod.


  method IF_EX_HRPAD00AUTH_TIME~CONSIDER_TIME_BY_MAX_AUTH.
** date: 30.01.2019
** Developer : 10106
** Requested by: Venugopal Menon
** below validation is to restrisct all infotype data for all users , show only IT 001 data if parameter ZHR_IT001 maintained in Su01
** only for PA20 or PA30
DATA: lv_usr_parva TYPE usr05-parva.

if sy-tcode = 'PA20' or sy-tcode = 'PA30'.

*     SELECT SINGLE parva
*     FROM usr05
*     INTO lv_usr_parva
*     WHERE bname = sy-uname
*     AND parid = 'ZHR_IT001'
*     AND parva = 'X'.
*     IF lv_usr_parva = 'X'.
*
*
*       IF INFTY = '0008'.
*         BREAK 10106.
*         IS_AUTHORIZED = ''.
**         MESSAGE 'You Are Not Authorised to Display/Change Other Info Types.' TYPE 'E'.
*       ENDIF.
*     endif.
endif.



  endmethod.


  method IF_EX_HRPAD00AUTH_TIME~RESTRICT_PAYROLL_ACCESS.
*** date: 30.01.2019
*** Developer : 10106
*** Requested by: Venugopal Menon
*** below validation is to restrisct all infotype data for all users , show only IT 001 data if parameter ZHR_IT001 maintained in Su01
*** only for PA20 or PA30
*DATA: lv_usr_parva TYPE usr05-parva.
*BREAK 10106.
*if sy-tcode = 'PA20' or sy-tcode = 'PA30'.
*
*     SELECT SINGLE parva
*     FROM usr05
*     INTO lv_usr_parva
*     WHERE bname = sy-uname
*     AND parid = 'ZHR_IT001'
*     AND parva = 'X'.
*     IF lv_usr_parva = 'X'.
**       IF INNNN-INFTY <> '0001'.
**         MESSAGE 'You Are Not Authorised to Display/Change Other Info Types.' TYPE 'E'.
**       ENDIF.
*     endif.
*endif.



  endmethod.


  method IF_EX_HRPAD00AUTH_TIME~RESTRICT_TIME_ACCESS.
  endmethod.
ENDCLASS.
