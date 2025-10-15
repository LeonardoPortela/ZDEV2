
process before output.
  module detail_init.
*
process after input.
  module detail_exit_command at exit-command.
  module detail_set_pfstatus.
  chain.
    field zsd_modelos_xml-model_eletronico .
    field zsd_modelos_xml-model_normal .
    field zsd_modelos_xml-texto_tela1 .
    field zsd_modelos_xml-texto_tela2 .
    field zsd_modelos_xml-texto_tela3 .
    field zsd_modelos_xml-texto_tela4 .
    module set_update_flag on chain-request.
  endchain.
  chain.
    field zsd_modelos_xml-model_eletronico .
    field zsd_modelos_xml-model_normal .
    module detail_pai.
  endchain.
