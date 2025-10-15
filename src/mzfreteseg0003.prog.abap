*----------------------------------------------------------------------*
***INCLUDE MZFRETESEG0003 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ALTERAR_CADASTRO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterar_cadastro INPUT.

  check: not it_valor_seg-icone is initial.

  READ TABLE it_valor_seg INTO wa_valor_seg WITH KEY mark = 'X'.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_valor_seg TO zvalor_seg_terc.
    CLEAR: vg_insert.
  ELSE.
    MESSAGE 'Favor selecionar um registro!' TYPE 'E' DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

ENDMODULE.                 " ALTERAR_CADASTRO  INPUT
