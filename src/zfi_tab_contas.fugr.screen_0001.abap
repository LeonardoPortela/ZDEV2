
PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zimp_contas_cons CURSOR nextline.
    MODULE liste_show_liste.
    MODULE desc_lifnr.
    MODULE data_vencto.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
*    FIELD ZIMP_CONTAS_CONS-BUKRS .
      FIELD zimp_contas_cons-belnr .
      FIELD zimp_contas_cons-xblnr .
*    FIELD ZIMP_CONTAS_CONS-BLART .
*    FIELD ZIMP_CONTAS_CONS-BUDAT .
*    FIELD ZIMP_CONTAS_CONS-ZLSCH .
*    FIELD ZIMP_CONTAS_CONS-ZFBDT .
*    FIELD ZIMP_CONTAS_CONS-ZBD1T .
      FIELD zimp_contas_cons-lifnr .
      FIELD zimp_contas_cons-dmbtr .
      FIELD zimp_contas_cons-cod_barras .
      FIELD v_desc_lifnr.
      FIELD v_dt_vencto.
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
*    FIELD ZIMP_CONTAS_CONS-BUKRS .
      FIELD zimp_contas_cons-belnr .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.
