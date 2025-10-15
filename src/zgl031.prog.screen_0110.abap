* Descrição: Tela de parâmetros do cadastro de apropriações
* e chamada as telas 0120, 0130.

PROCESS BEFORE OUTPUT.
  MODULE: iniciar_tela_0110,
          pbo_0110,
          tratar_campos_0110.

  CALL SUBSCREEN sub_0110 INCLUDING sy-repid screen_item.

PROCESS AFTER INPUT.
  CALL SUBSCREEN sub_0110.

  MODULE pai_0110.

PROCESS ON VALUE-REQUEST.
  FIELD wl_cabecalho_0110-seq_lcto        MODULE help_seq_lcto.
  FIELD wl_cabecalho_0110-tp_opr          MODULE define_tp_opr.
  FIELD wl_cabecalho_0110-nro_apolice     MODULE help_nro_apolice.
  FIELD wl_cabecalho_0110-waers      MODULE create_dropdown_box_0110.
