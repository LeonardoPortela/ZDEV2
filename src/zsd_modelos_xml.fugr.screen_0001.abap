
process before output.
  module liste_initialisieren.
  loop at extract with control
   tctrl_zsd_modelos_xml cursor nextline.
    module liste_show_liste.
  endloop.
*
process after input.
  module liste_exit_command at exit-command.
  module liste_before_loop.
  loop at extract.
    module liste_init_workarea.
    chain.
      field zsd_modelos_xml-model_eletronico .
      field zsd_modelos_xml-model_normal .
      field zsd_modelos_xml-texto_tela1 .
      field zsd_modelos_xml-texto_tela2 .
      field zsd_modelos_xml-texto_tela3 .
      field zsd_modelos_xml-texto_tela4 .
      module set_update_flag on chain-request.
    endchain.
    field vim_marked module liste_mark_checkbox.
    chain.
      field zsd_modelos_xml-model_eletronico .
      field zsd_modelos_xml-model_normal .
      module liste_update_liste.
    endchain.
  endloop.
  module liste_after_loop.
