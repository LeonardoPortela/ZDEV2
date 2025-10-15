*----------------------------------------------------------------------*
***INCLUDE MZLES003COCKPIT2001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_2001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_2001 output.

  loop at screen.
    if ( screen-name = 'BTNE' ).
      if vg_peso_digitar is initial.
        screen-input = 1.
      else.
        screen-input = 0.
      endif.
      modify screen.
    endif.
    if ( screen-name = 'BTNC' ).
      if vg_peso_digitar is initial.
        screen-input = 0.
      else.
        screen-input = 1.
      endif.
      modify screen.
    endif.
    if ( screen-name = 'VG_PESO_DIGITADO' ).
      if vg_peso_digitar is initial.
        screen-input = 0.
      else.
        screen-input = 1.
      endif.
      modify screen.
    endif.
  endloop.

endmodule.                 " STATUS_2001  OUTPUT
