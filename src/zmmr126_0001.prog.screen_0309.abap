PROCESS BEFORE OUTPUT.

  MODULE status_0309.
*
PROCESS AFTER INPUT.

  FIELD zde_zsdt0001cg_alv-nm_peso_bruto MODULE altera_bruto ON REQUEST.

  FIELD zde_zsdt0001cg_alv-nm_peso_tara MODULE altera_tara ON REQUEST.

  FIELD zde_zsdt0001cg_alv-nm_peso_subtotal
   MODULE valida_subtotal ON REQUEST.

  CHAIN.
    FIELD zde_zsdt0001cg_alv-nm_peso_bruto.
    FIELD zde_zsdt0001cg_alv-nm_peso_tara.
    MODULE atribui_info_carga ON CHAIN-REQUEST.
  ENDCHAIN.

  FIELD zde_zsdt0001cg_alv-nr_perc_umi MODULE perc_01 ON REQUEST.
  FIELD zde_zsdt0001cg_alv-nr_perc_imp MODULE perc_02 ON REQUEST.
  FIELD zde_zsdt0001cg_alv-nr_perc_ava MODULE perc_03 ON REQUEST.
  FIELD zde_zsdt0001cg_alv-nr_perc_ard MODULE perc_04 ON REQUEST.
  FIELD zde_zsdt0001cg_alv-nr_perc_que MODULE perc_05 ON REQUEST.
  FIELD zde_zsdt0001cg_alv-nr_perc_esv MODULE perc_06 ON REQUEST.
  FIELD zde_zsdt0001cg_alv-nr_perc_car MODULE perc_07 ON REQUEST.

  FIELD zde_zsdt0001cg_alv-in_gmo
  MODULE prc_in_gmo ON REQUEST.

  FIELD zde_zsdt0001cg_alv-nr_resultado_01
  MODULE prc_in_gmo ON REQUEST.

  FIELD zde_zsdt0001cg_alv-nr_resultado_02
  MODULE prc_in_gmo ON REQUEST.

  FIELD zde_zsdt0001cg_alv-nr_res_rr1_rr2
  MODULE prc_in_gmo ON REQUEST.

  FIELD zde_zsdt0001cg_alv-in_srr_origem_partic
  MODULE prc_in_gmo ON REQUEST.

  FIELD zde_zsdt0001cg_alv-id_outro_partic
  MODULE prc_in_gmo ON REQUEST.

  FIELD zde_zsdt0001cg_alv-in_srr_declarado
  MODULE prc_in_gmo ON REQUEST.

  FIELD zde_zsdt0001cg_alv-in_srr_declarado_2
  MODULE prc_in_gmo ON REQUEST.

  FIELD zde_zsdt0001cg_alv-in_teste_srr_2
  MODULE prc_in_gmo ON REQUEST.

  FIELD zde_zsdt0001cg_alv-id_classificadora
  MODULE prc_in_gmo ON REQUEST.

  FIELD zde_zsdt0001cg_alv-ck_class_dest
  MODULE prc_in_gmo ON REQUEST.
*  *  rmb
  CHAIN.
    FIELD zde_zsdt0001cg_alv-ck_class_dest.
  ENDCHAIN.


  CHAIN.
    FIELD zde_zsdt0001cg_alv-nr_perc_umi .
    FIELD zde_zsdt0001cg_alv-nr_perc_imp.
    FIELD zde_zsdt0001cg_alv-nr_perc_ava.
    FIELD zde_zsdt0001cg_alv-nr_perc_ard.
    FIELD zde_zsdt0001cg_alv-nr_perc_que.
    FIELD zde_zsdt0001cg_alv-nr_perc_esv.
    FIELD zde_zsdt0001cg_alv-nr_perc_car.
    MODULE atribui_info_carga ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_zsdt0001cg_alv-nm_peso_subtotal.
    FIELD zde_zsdt0001cg_alv-nr_ticket.
    FIELD zde_zsdt0001cg_alv-in_gmo.
    FIELD zde_zsdt0001cg_alv-nr_resultado_01.
    FIELD zde_zsdt0001cg_alv-nr_resultado_02.
    FIELD zde_zsdt0001cg_alv-in_srr_origem_partic.
    FIELD zde_zsdt0001cg_alv-in_srr_declarado.
    FIELD zde_zsdt0001cg_alv-in_srr_declarado_2.
    FIELD zde_zsdt0001cg_alv-in_teste_srr_2.
    FIELD zde_zsdt0001cg_alv-id_outro_partic.
    FIELD zde_zsdt0001cg_alv-id_classificadora.

    MODULE atribui_info_carga ON CHAIN-REQUEST.
  ENDCHAIN.

PROCESS ON HELP-REQUEST.

  FIELD zde_zsdt0001cg_alv-in_gmo MODULE value_help_classifica.
  FIELD zde_zsdt0001cg_alv-nr_resultado_01 MODULE value_help_classifica.
  FIELD zde_zsdt0001cg_alv-nr_resultado_02 MODULE value_help_classifica.
  FIELD zde_zsdt0001cg_alv-in_srr_origem_partic MODULE
value_help_classifica.
  FIELD zde_zsdt0001cg_alv-in_srr_declarado MODULE
value_help_classifica.
  FIELD zde_zsdt0001cg_alv-in_srr_declarado_2 MODULE
value_help_classifica.
  FIELD zde_zsdt0001cg_alv-in_teste_srr_2 MODULE value_help_classifica.
  FIELD zde_zsdt0001cg_alv-id_outro_partic MODULE value_help_classifica.
  FIELD zde_zsdt0001cg_alv-ds_outro_partic MODULE value_help_classifica.
  FIELD zde_zsdt0001cg_alv-id_classificadora MODULE
value_help_classifica.
  FIELD zde_zsdt0001cg_alv-nr_res_rr1_rr2 MODULE value_help_classifica.
  FIELD zde_zsdt0001cg_alv-tp_transgenia MODULE value_help_classifica.

* MODULE USER_COMMAND_0309.
