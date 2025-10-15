
PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zdco_produtor CURSOR nextline.
    MODULE liste_show_liste.
    MODULE zdesc_codigos.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zdco_produtor-nu_dco .
      FIELD zdco_produtor-nr_dco .
      FIELD zdco_produtor-qt_material .
      FIELD zdco_produtor-id_fornecedor .
      FIELD zdco_produtor-cd_material .
      FIELD zdco_produtor-nu_aviso .
      FIELD zdco_produtor-obs_dco .
      FIELD zdco_produtor-cd_centro .
      FIELD zdco_produtor-cd_safra .
      FIELD zdco_produtor-nu_cda .
      FIELD zdco_produtor-cd_tipo_leilao .
      FIELD zdco_produtor-vbeln .
      FIELD zdco_produtor-qt_entregue .
      FIELD zdco_produtor-qt_remessa .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zdco_produtor-nu_dco .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.
