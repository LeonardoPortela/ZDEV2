process before output.
  module trata_fields.
  module status_0300.

process after input.

  chain.
    field ws_header_ivoice-status.
    field ws_header_ivoice-desc_status.
    field ws_header_ivoice-data_receb.
    field ws_header_ivoice-valor_receb.
    module set_desc_status on chain-request.
  endchain.

  module user_command_0300.
