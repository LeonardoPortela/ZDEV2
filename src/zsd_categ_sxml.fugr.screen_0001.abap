
process before output.
  module liste_initialisieren.
  loop at extract with control
   tctrl_zsd_categ_sxml cursor nextline.
    module liste_show_liste.
  endloop.
*
process after input.
  module liste_exit_command at exit-command.
  module liste_before_loop.
  loop at extract.
    module liste_init_workarea.
    chain.
      field zsd_categ_sxml-nftype .
      module set_update_flag on chain-request.
    endchain.
    field vim_marked module liste_mark_checkbox.
    chain.
      field zsd_categ_sxml-nftype .
      module liste_update_liste.
    endchain.
  endloop.
  module liste_after_loop.
