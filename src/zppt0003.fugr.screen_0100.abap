process before output.
 module liste_initialisieren.
 loop at extract with control
  tctrl_zppt0003 cursor nextline.
   module liste_show_liste.
 endloop.
*
process after input.
 module liste_exit_command at exit-command.
 module liste_before_loop.
 loop at extract.
   module liste_init_workarea.
   chain.
    field zppt0003-werks .
    field zppt0003-verid .
    field zppt0003-matnr .
    field zppt0003-peso_desc .
    module set_update_flag on chain-request.
   endchain.
   field vim_marked module liste_mark_checkbox.
   chain.
    field zppt0003-werks .
    field zppt0003-verid .
    field zppt0003-matnr .
    module liste_update_liste.
   endchain.
 endloop.
 module liste_after_loop.
