*----------------------------------------------------------------------*
***INCLUDE LZLES0003F02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  COLOCA_VIRGULA
*&---------------------------------------------------------------------*
*       Acrescenta virgula nas casas decimais
*----------------------------------------------------------------------*
form coloca_virgula  using valor type char50.

  data: vg_decimal type char2.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = valor
    importing
      output = valor.

  vg_decimal = valor+48(2).

  valor      = valor+1(47).

  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = valor
    importing
      output = valor.

  concatenate valor ',' vg_decimal into valor.

endform.                    " COLOCA_VIRGULA
