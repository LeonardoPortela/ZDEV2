*&---------------------------------------------------------------------*
*& Report ZMMR197
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zmmr197.

data: lt_retorno      type table of zmme_retorno_supplier_item,
      lv_string       type string,
      lv_data_conv    type string,
      lv_datum        type timestamp,
      lv_info_request type string,
      lv_offset       type string,
      lv_matnr        type matnr,
      lv_matnr18      type matnr18,
      it_zmmt0177     type table of zmmt0177,
      c_numeric       type string value ' -.0123456789'.


do.
  lv_info_request = '0' && ';' && lv_offset.
  lv_offset = lv_offset + 50.
  zcl_int_ob_supplier_item_coupa=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = lv_info_request importing e_integracao = data(r_response) ).
  if r_response is not initial.
    lv_string = r_response-ds_data_retorno.
    replace all occurrences of '-' in lv_string with '_'.
    /ui2/cl_json=>deserialize( exporting json = lv_string changing data = lt_retorno ).

    if lt_retorno is initial.

      clear lv_offset.

      exit.

    endif.


    convert date sy-datum time sy-uzeit into time stamp lv_datum time zone sy-zonlo.

    call method cl_xlf_date_time=>create
      exporting
        timestamp = lv_datum
      receiving
        iso8601   = lv_data_conv.

    replace all occurrences of '-' in lv_data_conv with '_'.

    delete lt_retorno where supplier-number = 'none' or
                            supplier-number = ' ' or
                            contract-status <> 'published' or
                            item-active = '' or
                            contract-start_date > lv_data_conv.


    loop at lt_retorno assigning field-symbol(<fs_retorno>).
      if <fs_retorno>-price co c_numeric.
        append initial line to it_zmmt0177 assigning field-symbol(<fs_contratos1>).
        <fs_contratos1>-contract_id   = <fs_retorno>-contract-id.
        lv_matnr = |{ <fs_retorno>-item-item_number alpha = in }|.
        lv_matnr18 = |{ <fs_retorno>-item-item_number alpha = in }|.

        <fs_contratos1>-item_number       = lv_matnr18.
        <fs_contratos1>-supplier_number   = <fs_retorno>-supplier-number.
        <fs_contratos1>-price         = <fs_retorno>-price.
*      <fs_contratos1>-updated_at    = <fs_retorno>-updated_at.
*      <fs_contratos1>-created_a<fs_retorno>t    = <fs_retorno>-created_at.
        concatenate <fs_retorno>-updated_at+0(4) <fs_retorno>-updated_at+5(2) <fs_retorno>-updated_at+8(2) into <fs_contratos1>-updated_at.
        concatenate <fs_retorno>-created_at+0(4) <fs_retorno>-created_at+5(2) <fs_retorno>-created_at+8(2) into <fs_contratos1>-created_at.

        <fs_contratos1>-purchasable   = <fs_retorno>-item-active.

        <fs_contratos1>-supplier_number = |{ <fs_contratos1>-supplier_number alpha = in }|.
      endif.

    endloop.
  endif.
enddo.
if it_zmmt0177[] is not initial.
  modify zmmt0177 from table it_zmmt0177.
  commit work.
endif.
