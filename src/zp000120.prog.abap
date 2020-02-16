*----------------------------------------------------------------------*
*   INCLUDE ZP000120                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  MOD_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODULE_PBO_0001 OUTPUT.
    tables t500p.
*  break ibmabap01.
data : wa_BEZEI type BEZEI,
       wa_loc type Z6HRA_LOCATION-ZLOCATION_TEXT.

*---
  loop at screen.
  if screen-group3 = '003'.
   screen-input = 0.
  modify screen.
 endif.

   if screen-group3 = '004'.
   screen-input = 0.
  modify screen.
 endif.
 endloop.


*---
if not P0001-ZZSTATE is initial.
  clear wa_BEZEI .
select single BEZEI from t005u into wa_BEZEI
  where SPRAS = 'EN'
        and LAND1 = t500p-land1
        and BLAND = P0001-ZZSTATE .
  if sy-subrc  = 0.
    P0001-ZZAOP = wa_BEZEI .
  endif.


if not P0001-ZZLOCATION_KEY is initial.
  clear wa_loc.
  select single ZLOCATION_TEXT from Z6HRA_LOCATION
    into wa_loc where SPRAS  = 'EN'
                      and STATE = P0001-ZZSTATE
                      and ZLOCATION_KEY = P0001-ZZLOCATION_KEY.
    if sy-subrc = 0.
     p0001-ZZLOC_TEXT = wa_loc.
    endif.
    endif.
  endif.



ENDMODULE.                 " MOD_PBO  OUTPUT
