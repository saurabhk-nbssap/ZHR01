*----------------------------------------------------------------------*
*   INCLUDE ZP000920                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  MOD_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODULE_PBO_0009 OUTPUT.

  if P0009-ZZTRAN_TYPE eq 'N'.
    if P0009-ZZRTGS_NEFT_IFSC is initial.
      loop at screen.
        if screen-NAME = 'P0009-ZZRTGS_NEFT_IFSC'.
          screen-required = '1'.
          modify screen.
        endif.
      endloop.
    endif.
  else.
    if P0009-ZZRTGS_NEFT_IFSC is initial.
      loop at screen.
        if screen-NAME = 'P0009-ZZRTGS_NEFT_IFSC'.
          screen-required = ''.
          modify screen.
        endif.
      endloop.
    endif.
  endif.

ENDMODULE.                 " MOD_PBO  OUTPUT
