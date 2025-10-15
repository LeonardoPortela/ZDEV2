function zfu_zpm0065.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_FATURA) TYPE  RE_BELNR
*"     REFERENCE(I_CNPJ) TYPE  STCD1
*"  EXPORTING
*"     VALUE(E_NU_REQ) TYPE  BANFN
*"  TABLES
*"      IT_ZPMT0025 STRUCTURE  ZPMT0025
*"----------------------------------------------------------------------

  data: it_zpmt0032 type table of zpmt0032,
        it_zpmt0024 type table of zpmt0024,
        it_zpmt0026 type table of zpmt0026.

  data: wa_requisition_items  type bapiebanc,
        it_requisition_items  type standard table of bapiebanc,
        it_return             type standard table of bapireturn,
        wa_return             type bapireturn,
        it_ACCOUNT_ASSIGNMENT type standard table of bapiebkn,
        wa_ACCOUNT_ASSIGNMENT type bapiebkn,
        w_number              like bapiebanc-preq_no,
        item                  type p decimals 2,
        cont_preco            type p decimals 2,
        cont_vlr              type p decimals 2,
        c_e                   type c value 'I',
        c_x                   type c value 'X'.


  select * from zpmt0032 into table it_zpmt0032
    where fatura eq i_fatura
     and  cnpj   eq i_cnpj.


  select * from zpmt0024 into table it_zpmt0024
  where fatura eq i_fatura
   and  cnpj   eq i_cnpj.


  select * from zpmt0026 into table it_zpmt0026
  where fatura eq i_fatura
   and  cnpj   eq i_cnpj.

  sort it_zpmt0026 by cod_material ascending.
  data(gt_zpmt0026) = it_zpmt0026.

****  Separando a quantidade de materiais.
  delete adjacent duplicates from gt_zpmt0026 comparing fatura cod_material.


  loop at gt_zpmt0026 assigning field-symbol(<fw_zpmt0026>).
    clear: <fw_zpmt0026>-qtde, <fw_zpmt0026>-vlr_unt, <fw_zpmt0026>-vlr_total, cont_vlr, cont_preco.

    loop at it_zpmt0026 assigning field-symbol(<fs_zpmt0026>) where cod_material eq <fw_zpmt0026>-cod_material.
      add <fs_zpmt0026>-qtde to <fw_zpmt0026>-qtde.
      add <fs_zpmt0026>-vlr_unt to cont_preco.
      add 1 to cont_vlr.

    endloop.

    add 10 to item.
    <fw_zpmt0026>-vlr_unt = cont_preco / cont_vlr.
    <fw_zpmt0026>-vlr_total = <fw_zpmt0026>-qtde * <fw_zpmt0026>-vlr_unt.

    read table it_zpmt0024 into data(_zpmt0024) with key fatura = <fw_zpmt0026>-fatura
                                                        cnpj   = <fw_zpmt0026>-cnpj.

    read table it_zpmt0032 into data(_zpmt0032) with  key fatura = _zpmt0024-fatura cnpj = _zpmt0024-cnpj.
*    Verificando ordem para centro no SET 'MAGGI_ZPM0065_ORDEM'
    zcl_webservic_protheus=>get_ordem( exporting werks = _zpmt0032-centro importing aufnr = data(w_ordem) ).

*   Selecionando centro de custo da ordem
    zcl_webservic_protheus=>get_c_c_equip( exporting aufnr = w_ordem importing kostl = data(w_kostl) ).

    append value #(
     item          = item
     cnpj          = _zpmt0032-cnpj
     lifnr         = _zpmt0032-lifnr
     empresa       = _zpmt0032-empresa
     dt_fatura     = _zpmt0032-dt_fatura
     hr_fatura     = _zpmt0032-hr_fatura
     cliente       = _zpmt0032-cliente
     cnpj_cliente  = _zpmt0032-cnpj_cliente
     centro        = _zpmt0032-centro
     ordem         =  w_ordem
     activity      = '0010'
     werks         = _zpmt0032-centro
     mandt         = sy-mandt
     fatura        = <fw_zpmt0026>-fatura
     cod_material  = <fw_zpmt0026>-cod_material
     desc_material = <fw_zpmt0026>-desc_material
     qtde          = <fw_zpmt0026>-qtde
     und           = <fw_zpmt0026>-und
     vlr_unt       = <fw_zpmt0026>-vlr_unt
     vlr_total     = <fw_zpmt0026>-vlr_total
     bsart         = 'PCSF'
     ekgrp         = 'F99'
     acctasscat    = 'F'
     gl_account    = ' '
     costcenter    = w_kostl
     co_area       = 'MAGI'
*     CREAT_DATE    = SY-DATUM
     cod_status    = '1'
     status_proc   = 'Aguardando pedido' ) to it_zpmt0025.


    data aux_codmat    type matnr18.
    data aux_matnr    type matnr18.
    data aux_MATKL    type maRA-matkl.
    data aux_SAKNR    type zmmt0039-saknr.
    pack <fw_zpmt0026>-cod_material to aux_codmat.
    unpack aux_codmat to aux_codmat.


    select single matnr from zpmt0034 where cod_material = @aux_codmat into @aux_matnr.
    select single maktx from makt where matnr = @aux_matnr into @wa_requisition_items-short_text.
    select single matkl from maRA where matnr = @aux_matnr into @aux_MATKL.
    select single saknr from zmmt0039 where matkl = @aux_MATKL into @aux_SAKNR.


    wa_requisition_items-doc_type   = 'RCS'.                   "Tipo de requisição de compra (P/ Agro sempre NB)
    wa_requisition_items-preq_item  = item.      "N Item
    wa_requisition_items-material   = aux_matnr.       "N Material
    "wa_requisition_items-short_text = <fw_zpmt0026>-desc_material.        "Texto Breve Material
    "wa_requisition_items-store_loc  = gv_deposito.       "Depósito
    wa_requisition_items-quantity   = <fw_zpmt0026>-qtde.
    wa_requisition_items-pur_group  = 'F99'.       "Grupo de Comprador
    wa_requisition_items-plant      = _zpmt0032-centro.             "Centro
    "wa_requisition_items-trackingno =  gv_acompanha .
    wa_requisition_items-preq_name  = sy-uname.
    wa_requisition_items-deliv_date = sy-datum.               "Data da remessa
    wa_requisition_items-del_datcat = 1.                      "Tipo de data da remessa
*      wa_requisition_items-prio_urg   = gv_un.
    "wa_requisition_items-mrp_contr = gv_planejador.
    wa_requisition_items-c_amt_bapi = <fw_zpmt0026>-vlr_total.
    wa_requisition_items-price_unit = <fw_zpmt0026>-vlr_unt.
    wa_requisition_items-acctasscat = 'F'.

    append wa_requisition_items to it_requisition_items.
    clear: aux_codmat,aux_matnr.

    wa_ACCOUNT_ASSIGNMENT-preq_item = item.
    wa_ACCOUNT_ASSIGNMENT-created_on = sy-datum.
    wa_ACCOUNT_ASSIGNMENT-created_by = sy-uname.
    wa_ACCOUNT_ASSIGNMENT-g_l_acct = aux_SAKNR.
    wa_ACCOUNT_ASSIGNMENT-bus_area = _zpmt0032-centro.
    wa_ACCOUNT_ASSIGNMENT-cost_ctr = w_kostl.
    wa_ACCOUNT_ASSIGNMENT-order_no = w_ordem.


    append wa_ACCOUNT_ASSIGNMENT to it_ACCOUNT_ASSIGNMENT.


    clear: wa_requisition_items,_zpmt0032,w_kostl, w_ordem,wa_ACCOUNT_ASSIGNMENT,aux_SAKNR.

  endloop.

  call function 'BAPI_REQUISITION_CREATE'
    importing
      number                         = w_number
    tables
      requisition_items              = it_requisition_items
      requisition_account_assignment = it_ACCOUNT_ASSIGNMENT
      return                         = it_return
      "extensionin        = lt_extensionin
    .

  read table it_return into wa_return with key type = c_e.
  if sy-subrc eq 0.

    "Commit
*    call function 'BAPI_TRANSACTION_COMMIT'
*      exporting
*        wait = c_x.

    "gravar no campo novo w_number

*    loop at it_zpmt0032 assigning field-symbol(<_add_BANFN_32>).
*      <_add_BANFN_32>-banfn = w_number.
*    endloop.
*
*    loop at it_zpmt0025 assigning field-symbol(<_add_BANFN_25>).
*      <_add_BANFN_25>-banfn = w_number.
*    endloop.

    e_nu_req = w_number.

    modify zpmt0024 from table it_zpmt0024.
    modify zpmt0025 from table it_zpmt0025.
    modify zpmt0026 from table it_zpmt0026.
    modify zpmt0032 from table it_zpmt0032.


    clear: w_number.
    clear: it_requisition_items[],it_ACCOUNT_ASSIGNMENT[], it_return[].
    message s000(zwrm001) display like 'S' with 'Fatura aprovada com sucesso!'.

*    sy-ucomm = 'REFRESH'.
*    LEAVE TO SCREEN 0100.
  else.
    message e000(zwrm001) display like 'S' with wa_return-message wa_return-message_v1 wa_return-message_v2 wa_return-message_v3.
    clear: w_number.
    clear: it_requisition_items[],it_ACCOUNT_ASSIGNMENT[], it_return[].
  endif.

  clear: w_number.
  clear: it_requisition_items[],it_ACCOUNT_ASSIGNMENT[], it_return[].

endfunction.
