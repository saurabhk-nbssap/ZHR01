class ZCL_IM_HR_PA20_PA30 definition
  public
  final
  create public .

public section.

  interfaces IF_EX_HRPAD00INFTY .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_HR_PA20_PA30 IMPLEMENTATION.


  method IF_EX_HRPAD00INFTY~AFTER_INPUT.
*value( TCLAS ) TYPE TCLAS  Transaction Class
*value( NEW_INNNN ) TYPE PRELP  Created Infotype
*value( OLD_INNNN ) TYPE PRELP  Infotype Before Change
*value( IPSYST )  TYPE PSYST  Work Fields for Dialog Control
*value( I001P ) TYPE T001P  Personnel Area/Subarea
*value( I503 )  TYPE T503 Employee Group/Subgroup
*value( IPREF )	TYPE PREF	Assignment Values for HR Objects
*ERROR_WITH_MESSAGE   Error


    BREAK 10106.

  endmethod.


  method IF_EX_HRPAD00INFTY~BEFORE_OUTPUT.
*value( TCLAS ) TYPE TCLAS  Transaction Class
*value( IPSYST )  TYPE PSYST  Work Fields for Dialog Control
*value( I001P ) TYPE T001P  Personnel Area/Subarea
*value( I503 )  TYPE T503 Employee Group/Subgroup
*value( INNNN ) TYPE PRELP  Infotype
*value( IPREF )	TYPE PREF	Assignment Values for HR Objects

** date: 29.01.2019
** Developer : 10106
** Requested by: Venugopal Menon
** below validation is to restrisct all infotype data for all users , show only IT 001 data if parameter ZHR_IT001 maintained in Su01
** only for PA20 or PA30
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
*       IF INNNN-INFTY <> '0001'.
*         MESSAGE 'You Are Not Authorised to Display/Change Other Info Types.' TYPE 'E'.
*       ENDIF.
*     endif.
*endif.

  endmethod.


  method IF_EX_HRPAD00INFTY~IN_UPDATE.
  endmethod.
ENDCLASS.
