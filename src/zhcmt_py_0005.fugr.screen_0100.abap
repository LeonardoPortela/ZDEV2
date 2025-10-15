PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zhcmt_py_0005 CURSOR nextline.
    MODULE liste_show_liste.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zhcmt_py_0005-bukrs .
      FIELD zhcmt_py_0005-bukrs_ate .
      FIELD zhcmt_py_0005-abkrs .
      FIELD zhcmt_py_0005-nivel .
      FIELD zhcmt_py_0005-aprovador .
      FIELD zhcmt_py_0005-dt_val_de .
      FIELD zhcmt_py_0005-dt_val_ate .
      FIELD zhcmt_py_0005-aprovador_tmp .
      FIELD zhcmt_py_0005-dt_val_de_tmp .
      FIELD zhcmt_py_0005-dt_val_ate_tmp .
      FIELD zhcmt_py_0005-data_atual .
      FIELD zhcmt_py_0005-hora_atual .
      FIELD zhcmt_py_0005-usuario .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zhcmt_py_0005-bukrs .
      FIELD zhcmt_py_0005-bukrs_ate .
      FIELD zhcmt_py_0005-abkrs .
      FIELD zhcmt_py_0005-nivel .
      FIELD zhcmt_py_0005-aprovador .
      FIELD zhcmt_py_0005-dt_val_de .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.
