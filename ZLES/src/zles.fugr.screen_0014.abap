process before output.
 module liste_initialisieren.
 loop at extract with control
  tctrl_zlest0010 cursor nextline.
   module liste_show_liste.
 endloop.
*
process after input.
 module liste_exit_command at exit-command.
 module liste_before_loop.
 loop at extract.
   module liste_init_workarea.
   chain.
    field zlest0010-bukrs .
    field zlest0010-branch .
    field zlest0010-nfenum .
    field zlest0010-cnpj .
    field zlest0010-zdata_chega .
    field zlest0010-peso_nota .
    field zlest0010-peso_chega .
    field zlest0010-tp_movi .
    field zlest0010-cnpj2 .
    module set_update_flag on chain-request.
   endchain.
   field vim_marked module liste_mark_checkbox.
   chain.
    field zlest0010-bukrs .
    field zlest0010-branch .
    field zlest0010-nfenum .
    module liste_update_liste.
   endchain.
 endloop.
 module liste_after_loop.
