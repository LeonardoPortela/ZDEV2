*----------------------------------------------------------------------*
***INCLUDE ZFIR0067_PBO.
*----------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.
ENDMODULE.

MODULE criar_objetos OUTPUT.

  PERFORM: create_container_alv_tree,
           iniciar_tree.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

  qd_versao_1 = wa_saida_mov_flx-txt_vrs1.
  qd_versao_2 = wa_saida_mov_flx-txt_vrs2.

ENDMODULE.

MODULE STATUS_0102 OUTPUT.
  SET PF-STATUS 'PF0102'.
  SET TITLEBAR 'T0102'.
ENDMODULE.
