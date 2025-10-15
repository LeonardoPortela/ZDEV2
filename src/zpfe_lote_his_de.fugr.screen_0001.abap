
process before output.
  module liste_initialisieren.
  loop at extract with control
   tctrl_zpfe_lote_his_de cursor nextline.
    module liste_show_liste.
  endloop.
*
process after input.
  module liste_exit_command at exit-command.
  module liste_before_loop.
  loop at extract.
    module liste_init_workarea.
    chain.
      field zpfe_lote_his_de-chvid_adm .
      field zpfe_lote_his_de-chvid_emp .
      module set_update_flag on chain-request.
    endchain.
    field vim_marked module liste_mark_checkbox.
    chain.
      field zpfe_lote_his_de-chvid_adm .
      module liste_update_liste.
    endchain.
  endloop.
  module liste_after_loop.
