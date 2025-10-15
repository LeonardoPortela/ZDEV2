
process before output.
  module status_2002.
  module buid_options.
  module visibilidade_2002.
*
process after input.

  module user_command_2002_exit at exit-command.

  chain.
    field znota_import-docnum .
    field znota_import-itmnum .
    field znota_import-itdidoc .
    field znota_import-ndi .
    field znota_import-ddi .
    field znota_import-xlocdesemb .
    field znota_import-ufdesemb .
    field znota_import-ddesemb .
    field znota_import-cexportador .
  endchain.

  module user_command_2002.
