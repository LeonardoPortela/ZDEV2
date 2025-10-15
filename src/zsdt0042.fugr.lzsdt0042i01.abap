*----------------------------------------------------------------------*
***INCLUDE LZSDT0042I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  VERIFICA_ERRO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module VERIFICA_ERRO input.

IF zsdt0042-VLR_ALIQ IS NOT INITIAL
AND zsdt0042-WAERK IS INITIAL.
  MESSAGE E836(SD) WITH 'Para os campos que Vlr.Aliq estiver preenchido'
                        'é obrigatorio o preenchimento do campo Moeda'.
ENDIF.
endmodule.                 " VERIFICA_ERRO  INPUT
