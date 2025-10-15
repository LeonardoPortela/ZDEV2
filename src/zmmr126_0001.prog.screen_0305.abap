process before output.

  module status_0305.
*
process after input.

  field zde_zsdt0001nt_alv-id_entrada       module get_entra on request.
  field zde_zsdt0001nt_alv-id_mod_fiscal    module set_model on request.
  field zde_zsdt0001nt_alv-nr_fornecedor_ie module get_forne on request.

  chain.
    field zde_zsdt0001nt_alv-id_entrada.
    field zde_zsdt0001nt_alv-cfop.
    field zde_zsdt0001nt_alv-id_mod_fiscal.
    field zde_zsdt0001nt_alv-id_fornecedor.
    field zde_zsdt0001nt_alv-nr_fornecedor_ie.
    field zde_zsdt0001nt_alv-nr_nota.
    field zde_zsdt0001nt_alv-nm_serie.
    field zde_zsdt0001nt_alv-dt_emissao.
    field zde_zsdt0001nt_alv-nr_quantidade.
    field zde_zsdt0001nt_alv-nr_valor.
    field zde_zsdt0001nt_alv-dt_vencimento_form.
    field zde_zsdt0001nt_alv-id_entregue_por.
    field zde_zsdt0001nt_alv-nr_romaneio_ent.
    field zde_zsdt0001nt_alv-ds_observacao.
    field zde_zsdt0001nt_alv-po_number.
    module atribui_info_nota on chain-request.
  endchain.

  module user_command_0305.

process on value-request.

  field zde_zsdt0001nt_alv-id_entrada module value_request_id_entrada.
