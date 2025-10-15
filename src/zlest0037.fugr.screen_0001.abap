PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zlest0037 CURSOR nextline.
    MODULE liste_show_liste.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zlest0037-matnr .
      FIELD zlest0037-bukrs .
      FIELD zlest0037-cd_modal .
      FIELD zlest0037-lifnr .
      FIELD zlest0037-matkl .
      FIELD zlest0037-operacao .
      FIELD zlest0037-ck_servico .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zlest0037-matnr .
      FIELD zlest0037-bukrs .
      FIELD zlest0037-cd_modal .
      FIELD zlest0037-lifnr .
      FIELD zlest0037-matkl .
      FIELD zlest0037-operacao .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.
