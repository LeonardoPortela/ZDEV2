*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZFIR0008                                                                             *
*& Chamado        : USER STORY 140931                                                                    *
*& Data           : 07/01/2025                                                                           *
*& Especificado   : Antonio Rodrigues                                                                    *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 07/01/2025  |DEVK9A2C12  |NSEGATIN       |Desenvilvimento inicial. Chamado: 140931.                   *
*--------------------------------------------------------------------------------------------------------*
report zfir0008.

*--------------------------------------------------------------------*
* I N T E R N A L  T A B L E S                                       *
*--------------------------------------------------------------------*
types: begin of ty_0007.
         include structure zfit0007.
types:   lifnrw type lfa1-lifnr,
       end of ty_0007.

data: tg_0007      type table of ty_0007,
      it_lfa1_orig type table of lfa1, ""Melhorias automação Lanç Fatura de Energia #166647 - BG
      it_lfa1_dest type table of lfa1. ""Melhorias automação Lanç Fatura de Energia #166647 - BG
constants:
  read(4) type c value 'READ',
  edit(4) type c value 'EDIT',
  save(4) type c value 'SAVE'.

data: wa_j_1btxic3    type j_1btxic3.

data: lt_returns  type table of bapiret2.

data: chdat(8)   type c,
      houtput(8) type n.

data  function    like sy-ucomm.
data: variant_for_selection like tvimv-variant,
      view_name             like  dd02v-tabname.

data: begin of status_j_1btxic3v. "state vector
        include structure vimstatus.
data: end of status_j_1btxic3v.

data: begin of header occurs 1.
        include structure vimdesc.
data: end of header.

data: begin of namtab occurs 50.
        include structure vimnamtab.
data: end of namtab.

data: rangetab    type table of vimsellist initial size 50
       with header line,
      oc_rangetab type table of vimsellist initial size 50.

data: dpl_sellist    type table of vimsellist initial size 50 with header line.

data: begin of e071k_tab occurs 100.    "keys of changed entries
        include structure e071k.       "(used as parameter for VIEWPROC)
data: end of e071k_tab.

data: org_crit_inst type vimty_oc_type, lockuser type sy-uname.
data: excl_cua_funct like vimexclfun occurs 0 with header line.
data: begin of j_1btxic3v_extract occurs 0010.
        include structure j_1btxic3v.
        include structure vimflagtab.
data: end of j_1btxic3v_extract.
* Table for all entries loaded from database
data: begin of j_1btxic3v_total occurs 0010.
        include structure j_1btxic3v.
        include structure vimflagtab.
data: end of j_1btxic3v_total.
*--------------------------------------------------------------------*
* S E L E C T I O N - S C R E E N                                    *
*--------------------------------------------------------------------*
* Tela de Seleção
selection-screen begin of block b1 with frame title text-001.
* Tipo de Lançamento de Documento
  selection-screen begin of block b2 with frame title text-002.
    parameters: rb_pedid radiobutton group grp default 'X',
                rb_migo  radiobutton group grp,
                rb_miro  radiobutton group grp.

  selection-screen end of block b2.
selection-screen end of block b1.

*--------------------------------------------------------------------*
* S T A R T - O F - S E L E C T I O N                                *
*--------------------------------------------------------------------*
start-of-selection.
  perform: zf_select_data,  "Selecionar dados para o processamento
           zf_process_data. "Processa os dados selecionados

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       Selecionar dados para o processamento
*----------------------------------------------------------------------*
form zf_select_data.

  case abap_on.
    when rb_pedid. "Lançamento Doc. Pedido
* Selecionar dados para o processamento - Lançamento Doc. Pedido
      perform zf_select_data_pedid.

    when rb_migo.  "Lançamento Doc. Material
* Selecionar dados para o processamento - Lançamento Doc. Material
      perform zf_select_data_migo.

    when rb_miro.  "Lançamento Doc. Faturamento
* Selecionar dados para o processamento - Lançamento Doc. Faturamento
      perform zf_select_data_miro.

    when others.
*   Do nothing
  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form ZF_SELECT_DATA_PEDID
*&---------------------------------------------------------------------*
*&      Selecionar dados para o processamento - Lançamento Doc. Pedido
*&---------------------------------------------------------------------*
form zf_select_data_pedid.

  select * from zfit0007
    into corresponding fields of table  tg_0007
  where matnr ne space
    and mwskz ne space
    and ebeln eq space.
*    and base  gt 0.

  if not sy-subrc is initial.
* Não encontrados dados para esta seleção
    message s114(pt) display like sy-abcde+4(1). "E
    stop.
  else.
    loop at tg_0007 assigning field-symbol(<fs_0007>).

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = <fs_0007>-werks
        importing
          output = <fs_0007>-lifnrw.

    endloop.
    "Melhorias automação Lanç Fatura de Energia #166647 - BG - INICIO SELECT
    select *
      from lfa1
      into table it_lfa1_orig
      for all entries in tg_0007
      where lifnr = tg_0007-lifnr.

    select *
    from lfa1
    into table it_lfa1_dest
    for all entries in tg_0007
    where lifnr = tg_0007-lifnrw.
    "Melhorias automação Lanç Fatura de Energia #166647 - BG - FIM SELECT
  endif.

  sort it_lfa1_orig by lifnr.
  sort it_lfa1_dest by lifnr.

endform.
*&---------------------------------------------------------------------*
*&      Form ZF_SELECT_DATA_MIGO
*&---------------------------------------------------------------------*
*&      Selecionar dados para o processamento - Lançamento Doc. Material
*&---------------------------------------------------------------------*
form zf_select_data_migo.

  select * from zfit0007
    into corresponding fields of table tg_0007
  where ebeln ne space
    and mblnr eq space.

  if not sy-subrc is initial.
* Não encontrados dados para esta seleção
    message s114(pt) display like sy-abcde+4(1). "E
    stop.

  endif.

  loop at tg_0007 assigning field-symbol(<fs_0007>).

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_0007>-werks
      importing
        output = <fs_0007>-lifnrw.

  endloop.

  "Melhorias automação Lanç Fatura de Energia #166647 - BG - INICIO SELECT
  select *
    from lfa1
    into table it_lfa1_orig
    for all entries in tg_0007
    where lifnr = tg_0007-lifnr.

  select *
      from lfa1
      into table it_lfa1_dest
      for all entries in tg_0007
      where lifnr = tg_0007-lifnrw.
  "Melhorias automação Lanç Fatura de Energia #166647 - BG - FIM SELECT

  sort it_lfa1_orig by lifnr.
  sort it_lfa1_dest by lifnr.


endform.
*&---------------------------------------------------------------------*
*&      Form ZF_SELECT_DATA_MIRO
*&---------------------------------------------------------------------*
*&      Selecionar dados para o processamento - Lançamento Doc. Faturamento
*&---------------------------------------------------------------------*
form zf_select_data_miro.

  select * from zfit0007
    into table tg_0007
  where mblnr ne space
    and belnr eq space.
*    and base  gt 0.

  if not sy-subrc is initial.
* Não encontrados dados para esta seleção
    message s114(pt) display like sy-abcde+4(1). "E
    stop.

  endif.

  loop at tg_0007 assigning field-symbol(<fs_0007>).

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_0007>-werks
      importing
        output = <fs_0007>-lifnrw.

  endloop.

  "Melhorias automação Lanç Fatura de Energia #166647 - BG - INICIO SELECT
  select *
    from lfa1
    into table it_lfa1_orig
    for all entries in tg_0007
    where lifnr = tg_0007-lifnr.

  select *
      from lfa1
      into table it_lfa1_dest
      for all entries in tg_0007
      where lifnr = tg_0007-lifnrw.
  "Melhorias automação Lanç Fatura de Energia #166647 - BG - FIM SELECT

  sort it_lfa1_orig by lifnr.
  sort it_lfa1_dest by lifnr.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       Processa os dados selecionados
*----------------------------------------------------------------------*
form zf_process_data.

  case abap_on.
    when rb_pedid. "Lançamento Doc. Pedido
* Processa os dados selecionados - Lançamento Doc. Pedido
      perform zf_process_data_pedid.

    when rb_migo.  "Lançamento Doc. Material
* Processa os dados selecionados - Lançamento Doc. Material
      perform zf_process_data_migo.

    when rb_miro.  "Lançamento Doc. Faturamento
* Processa os dados selecionados - Lançamento Doc. Faturamento
      perform zf_process_data_miro.

    when others.
*   Do nothing
  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form ZF_PROCESS_DATA_PEDID
*&---------------------------------------------------------------------*
*&      Processa os dados selecionados - Lançamento Doc. Pedido
*&---------------------------------------------------------------------*
form zf_process_data_pedid.
  "Melhorias automação Lanç Fatura de Energia #166647 - BG -- variaveis e constantes - inicio
  data: tl_poaccount   type standard table of bapimepoaccount,
        tl_poaccountx  type standard table of bapimepoaccountx,
        tl_poitem_ped  type standard table of bapimepoitem,
        tl_poitemx_ped type standard table of bapimepoitemx,
        tl_return_ped  type standard table of bapiret2,
        lt_poitem_a    type standard table of bapimepoitem.


  data: el_poheader_ped  type                   bapimepoheader,
        el_poheaderx_ped type                   bapimepoheaderx,
        el_poitem_ped    type                   bapimepoitem,
        el_poitemx_ped   type                   bapimepoitemx,
        el_poaccount     type                   bapimepoaccount,
        el_poaccountx    type                   bapimepoaccountx.

  data: vl_purchaseorder type                   ebeln.

  "Melhorias automação Lanç Fatura de Energia #166647 - BG -- variaveis e constantes - FIM

  select matnr, meins from mara
    into table @data(tl_mara)
    for all entries in @tg_0007
  where matnr eq @tg_0007-matnr.

  if sy-subrc is initial.
    sort tl_mara by matnr.

  endif.

  loop at tg_0007 into data(el_0007).
    perform  f_atualiza_j1btax changing el_0007.

* CABEÇALHO
    el_poheader_ped-comp_code   = el_0007-bukrs.        "Empresa
    el_poheader_ped-vendor      = el_0007-lifnr.        "Fornecedor
    el_poheader_ped-doc_type    = 'PCEE'.               "Tipo de Pedido
    el_poheader_ped-purch_org   = 'OC01'.               "Organização de Compras
    el_poheader_ped-doc_date    = sy-datlo.             "Data do Pedido
    el_poheader_ped-langu       = sy-langu.             "Idioma
    el_poheader_ped-pur_group   = 'C03'.                "Grupo de Compradores
    el_poheader_ped-currency    = 'BRL'.                "Moeda pela ZMM0045
    el_poheader_ped-exch_rate   = 1.                    "Taxa de Câmbio pela ZMM0045
    el_poheader_ped-our_ref     = el_0007-invoiceid.
    el_poheader_ped-quot_date   = el_0007-duedate.      "Data vencimento
    el_poheader_ped-pmnttrms    = 'Z001'.
    el_poheader_ped-incoterms1  = 'CIF'.
    el_poheader_ped-incoterms2  = 'CIF'.

    el_poheaderx_ped-comp_code  = abap_on.              "Empresa
    el_poheaderx_ped-vendor     = abap_on.              "Fornecedor
    el_poheaderx_ped-doc_type   = abap_on.              "Tipo de Pedido
    el_poheaderx_ped-purch_org  = abap_on.              "Organização de Compras
    el_poheaderx_ped-doc_date   = abap_on.              "Data do Pedido
    el_poheaderx_ped-langu      = abap_on.              "Idioma
    el_poheaderx_ped-pur_group  = abap_on.              "Grupo de Compradores
    el_poheaderx_ped-currency   = abap_on.              "Moeda pela ZMM0045
    el_poheaderx_ped-exch_rate  = abap_on.              "Taxa de Câmbio pela ZMM0045
    el_poheaderx_ped-our_ref    = abap_on.
    el_poheaderx_ped-quot_date  = abap_on.              "Data vencimento
    el_poheaderx_ped-pmnttrms   = abap_on.
    el_poheaderx_ped-incoterms1 = abap_on.
    el_poheaderx_ped-incoterms2 = abap_on.

* ITEM
    el_poitem_ped-po_item      = 10.                   "Item
    el_poitem_ped-acctasscat   = 'K'.
    el_poitem_ped-material     = el_0007-matnr.        "Material
    el_poitem_ped-quantity     = 1.                    "Quantidade
    el_poitem_ped-po_price     = 1.                    "Transferência do preço: 1 = bruto, 2 = líquido
    el_poitem_ped-price_unit   = 1.                    "Unidade de preço

    read table tl_mara into data(el_mara) with key matnr = el_0007-matnr binary search.

    if sy-subrc is initial.
      el_poitem_ped-orderpr_un = el_mara-meins.        "Unidade do preço do pedido

    endif.

    clear: el_mara.
    el_poitem_ped-net_price    = el_0007-totalinvoice. "Preço
    el_poitem_ped-tax_code     = el_0007-mwskz.        "Código do Imposto
    el_poitem_ped-plant        = el_0007-werks.        "Centro
    el_poitem_ped-stge_loc     = space.                "Depósito
    el_poitem_ped-batch        = space.
    append el_poitem_ped to tl_poitem_ped.

    el_poitemx_ped-po_item      = 10.                  "Item
    el_poitemx_ped-acctasscat   = abap_on.
    el_poitemx_ped-material     = abap_on.             "Material
    el_poitemx_ped-quantity     = abap_on.             "Quantidade
    el_poitemx_ped-po_price     = abap_on.             "Transferência do preço: 1 = bruto, 2 = líquido
    el_poitemx_ped-price_unit   = abap_on.             "Unidade de preço
    el_poitemx_ped-orderpr_un   = abap_on.             "Unidade do preço do pedido
    el_poitemx_ped-net_price    = abap_on.             "Preço
    el_poitemx_ped-tax_code     = abap_on.             "Código do Imposto
    el_poitemx_ped-plant        = abap_on.             "Centro
    el_poitemx_ped-stge_loc     = abap_on.             "Depósito
    el_poitemx_ped-batch        = abap_on.
    append el_poitemx_ped to tl_poitemx_ped.
* CLASSIFICACAO CONTABIL
    el_poaccount-po_item      = 10.                    "Item
    el_poaccount-gl_account   = '0000412023'.          "Energia eletrica
    el_poaccount-costcenter   = el_0007-kostl.
    append el_poaccount to tl_poaccount.

    el_poaccountx-po_item      = 10.
    el_poaccountx-gl_account   = abap_on.
    el_poaccountx-costcenter   = abap_on.
    append el_poaccountx to tl_poaccountx.
*** Cria o Pedido.
    call function 'BAPI_PO_CREATE1'
      exporting
        poheader         = el_poheader_ped
        poheaderx        = el_poheaderx_ped
      importing
        exppurchaseorder = vl_purchaseorder
      tables
        return           = tl_return_ped
        poitem           = tl_poitem_ped
        poitemx          = tl_poitemx_ped
        poaccount        = tl_poaccount
        poaccountx       = tl_poaccountx.
* Verifica se existe erros na criação do documento.
    read table tl_return_ped transporting no fields with key type = 'E'.

    if sy-subrc is initial.
* Salva mensagens retornadas da criação do documento com erro.
      perform zf_log_erro_cria_doc tables tl_return_ped
                                    using el_0007.

    else.
* Verifica a mensagem de sucesso da criação do documento.
      read table tl_return_ped into data(el_return_ped) with key type    = 'S'
                                                                  id     = '06'
                                                                  number = '017'.

      if     sy-subrc         is initial and
         not vl_purchaseorder is initial.
* Commit o Pedido
        call function 'BAPI_TRANSACTION_COMMIT'.

        update zfit0007
           set ebeln = vl_purchaseorder
        where bukrs     eq el_0007-bukrs
          and werks     eq el_0007-werks
          and invoiceid eq el_0007-invoiceid.

        if sy-subrc is initial.
          commit work.
* Verifica mensagens de LOG da criação do documento com erro para eliminar.
          perform zf_log_check_cria_doc using el_0007.

          "Faz o recalculo do imposto
          do 5 times.
            refresh lt_poitem_a.
            loop at tl_poitem_ped into data(lo_wa_poitem_a).
              data(_item) = lo_wa_poitem_a-po_item.
              clear lo_wa_poitem_a.
              lo_wa_poitem_a-po_item = _item.
              lo_wa_poitem_a-calctype = 'G'. "Aceitar comp.preço sem modif., calcular novamente imposto
              append lo_wa_poitem_a to lt_poitem_a.
            endloop.
            data(lt_return) = zcl_integracao_coupa_ped_comp=>bapi_po_change(
              exporting
                iv_test     = ''
                iw_poheader = value bapimepoheader( po_number = vl_purchaseorder )
              changing
                ct_poitem   = lt_poitem_a ).
          enddo.
        else.
          rollback work.

        endif.

      endif.

    endif.

    clear: el_poheader_ped, el_poheaderx_ped, tl_return_ped, tl_poitem_ped,
           tl_poitemx_ped, tl_poaccount, tl_poaccountx, vl_purchaseorder.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form ZF_PROCESS_DATA_MIGO
*&---------------------------------------------------------------------*
*&      Processa os dados selecionados - Lançamento Doc. Material
*&---------------------------------------------------------------------*
form zf_process_data_migo.

  data: cll_migo type ref to zcl_migo.

  data: tl_itens type zde_migo_itens_t.

  data: el_cabecalho type zde_migo_cabecalho,
        el_itens     type zde_migo_itens.


  select matnr, meins from mara
    into table @data(tl_mara)
    for all entries in @tg_0007
  where matnr eq @tg_0007-matnr.

  if sy-subrc is initial.
    sort tl_mara by matnr.

  endif.

  loop at tg_0007 into data(el_0007).

    " Melhorias automação Lanç Fatura de Energia #166647 - BG  -- INICIO
    select single frgke
      from ekko
      into @data(v_frgke)
      where ebeln = @el_0007-ebeln.

*    check v_frgke eq '2'.


    " Melhorias automação Lanç Fatura de Energia #166647 - BG  -- FIM

* CABEÇALHO MOVIMENTO DE MERCADORIA
    el_cabecalho-data_documento  = el_0007-referencedate.
    el_cabecalho-data_lancamento = sy-datum.
    el_cabecalho-descricao       = |{ 'NexInvoice ID: ' } { el_0007-invoiceid alpha = out }|.
    el_cabecalho-valor_total     = el_0007-totalinvoice.
    el_cabecalho-ver_gr_gi_slip  = '1'.
    el_cabecalho-ver_gr_gi_slip  = '1'.
    el_cabecalho-zchave_nfe      = el_0007-invoicekey .
    el_cabecalho-exch_rate       = 1.
* ITEM MOVIMENTO DE MERCADORIA
    el_itens-tipo_movimento     = '101'.
    el_itens-material           = el_0007-matnr.
    el_itens-fornecedor         = el_0007-lifnr.
    el_itens-po_number          = el_0007-ebeln.
    el_itens-po_item            = '00010'.
    el_itens-peso               = 1.
    read table tl_mara into data(el_mara) with key matnr = el_0007-matnr binary search.
    el_itens-unidade            = el_mara-meins.
    clear el_mara.

    append el_itens to tl_itens.
    create object cll_migo.
* Objeto de Manibulação de MIGO - Criar Movimento
    try.
        call method cll_migo->criar
          exporting
            i_cabecalho = el_cabecalho
            i_itens     = tl_itens
            i_bapi_wait = abap_off
          importing
            e_retorno   = data(tl_retorno)
            mat_doc     = data(vl_mat_doc)
            doc_year    = data(vl_doc_year)
          receiving
            r_gerou     = data(vl_gerou).
      catch zcx_migo_exception into data(zcxl_migo_exception).
        append initial line to tl_retorno assigning field-symbol(<fs_retorno>).
        <fs_retorno>-type       = zcxl_migo_exception->msgty.
        <fs_retorno>-number     = zcxl_migo_exception->msgno.
        <fs_retorno>-message_v1 = zcxl_migo_exception->msgv1.
        <fs_retorno>-message_v2 = zcxl_migo_exception->msgv2.
        <fs_retorno>-message_v3 = zcxl_migo_exception->msgv3.
        <fs_retorno>-message_v4 = zcxl_migo_exception->msgv4.
        <fs_retorno>-id         = zcxl_migo_exception->msgid.
        <fs_retorno>-message    = zcxl_migo_exception->get_text( ).
        <fs_retorno>-parameter  = 'ZCL_MIGO'.
        <fs_retorno>-row        = 1.
        <fs_retorno>-system     = sy-sysid && 'CLNT' && sy-mandt.
* Verifica se existe erros na criação do documento.
        read table tl_retorno transporting no fields with key type = 'E'.

        if sy-subrc is initial.
* Salva mensagens retornadas da criação do documento com erro.
          perform zf_log_erro_cria_doc tables tl_retorno
                                        using el_0007.

        endif.

        message <fs_retorno>-message type 'S' display like 'E'.

      catch zcx_pedido_compra_exception into data(zcxl_pedido_compra_exception).
        append initial line to tl_retorno assigning <fs_retorno>.
        <fs_retorno>-type       = zcxl_pedido_compra_exception->msgty.
        <fs_retorno>-number     = zcxl_pedido_compra_exception->msgno.
        <fs_retorno>-message_v1 = zcxl_pedido_compra_exception->msgv1.
        <fs_retorno>-message_v2 = zcxl_pedido_compra_exception->msgv2.
        <fs_retorno>-message_v3 = zcxl_pedido_compra_exception->msgv3.
        <fs_retorno>-message_v4 = zcxl_pedido_compra_exception->msgv4.
        <fs_retorno>-id         = zcxl_pedido_compra_exception->msgid.
        <fs_retorno>-message    = zcxl_pedido_compra_exception->get_text( ).
        <fs_retorno>-parameter  = 'ZCL_MIGO'.
        <fs_retorno>-row        = 1.
        <fs_retorno>-system     = sy-sysid && 'CLNT' && sy-mandt.
* Verifica se existe erros na criação do documento.
        read table tl_retorno transporting no fields with key type = 'E'.

        if sy-subrc is initial.
* Salva mensagens retornadas da criação do documento com erro.
          perform zf_log_erro_cria_doc tables tl_retorno
                                        using el_0007.

        endif.

        message <fs_retorno>-message type 'S' display like 'E'.

    endtry.

    if not vl_gerou   is initial and
       not vl_mat_doc is initial.
      update zfit0007
         set mblnr = vl_mat_doc
             mjahr = vl_doc_year
      where bukrs     eq el_0007-bukrs
        and werks     eq el_0007-werks
        and invoiceid eq el_0007-invoiceid.

      if sy-subrc is initial.
        commit work.
* Verifica mensagens de LOG da criação do documento com erro para eliminar.
        perform zf_log_check_cria_doc using el_0007.

      else.
        rollback work.

      endif.

    else.
* Verifica se existe erros na criação do documento.
      read table tl_retorno transporting no fields with key type = 'E'.

      if sy-subrc is initial.
* Salva mensagens retornadas da criação do documento com erro.
        perform zf_log_erro_cria_doc tables tl_retorno
                                      using el_0007.

      endif.

    endif.

    clear: el_0007, el_cabecalho, el_itens, tl_itens, tl_retorno, vl_mat_doc, vl_doc_year, vl_gerou.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form ZF_PROCESS_DATA_MIRO
*&---------------------------------------------------------------------*
*&      Processa os dados selecionados - Lançamento Doc. Faturamento
*&---------------------------------------------------------------------*
form zf_process_data_miro.

  data: cll_miro type ref to zcl_miro.

  data: tl_itens  type zde_miro_itens_t,
        tl_contas type zbapi_incinv_gl_account_t,
        tl_ln_txt type tline_t.

  data: el_cabecalho           type zde_miro_cabecalho,
        el_itens               type zde_miro_itens,
        el_ln_txt              type tline,
        el_fatura_energia_upst type zfie_fatura_energia_upst,
        el_return_erro         type zstruct_return_api_eudr.

  select matnr, meins from mara
    into table @data(tl_mara)
    for all entries in @tg_0007
  where matnr eq @tg_0007-matnr.

  if sy-subrc is initial.
    sort tl_mara by matnr.

  endif.

  select ebeln, netpr from ekpo
    into table @data(tl_ekpo)
    for all entries in @tg_0007
  where ebeln eq @tg_0007-ebeln.

  if sy-subrc is initial.
    sort tl_ekpo by ebeln.

  endif.

  create object cll_miro.

  loop at tg_0007 into data(el_0007).
    "Atualiza J1BTAX novamente para cada fatura
    perform  f_atualiza_j1btax changing el_0007.

* Objeto de Manibulação de MIRO - Retorna Forma de Pagamento e Banco Empresa.
    cll_miro->get_formapag_banco_empresa( exporting i_bukrs = el_0007-bukrs
                                                    i_lifnr = el_0007-lifnr
                                                    i_bvtyp = '0001'
                                          importing e_banco_empresa   = el_cabecalho-housebankid
                                                    e_forma_pagamento = el_cabecalho-pymt_meth  ).

    if el_cabecalho-pymt_meth is initial.
      el_cabecalho-pymt_meth  = 'E'. "Boleto

    endif.
* Cabeçalho.
    el_cabecalho-invoice_ind    = abap_true.
    el_cabecalho-calc_tax_ind   = abap_true.
    el_cabecalho-goods_affected = abap_true.
    el_cabecalho-doc_type       = 'RE'.
    el_cabecalho-doc_date       = el_0007-referencedate.
    el_cabecalho-doc_date_mov   = sy-datlo.
    el_cabecalho-comp_code      = el_0007-bukrs.
    el_cabecalho-bus_area       = el_0007-werks.
    el_cabecalho-lifnr          = el_0007-lifnr.
    el_cabecalho-currency       = 'BRL'.
    el_cabecalho-header_txt     = |{ 'NexInvoice ID: ' } { el_0007-invoiceid alpha = out }|.
    el_cabecalho-pmnttrms       = 'Z001'.
    el_cabecalho-j_1bnftype     = 'S1'.
    el_cabecalho-del_costs_taxc = el_0007-mwskz.
    el_cabecalho-alloc_nmbr     = el_0007-ebeln.
    el_cabecalho-series         = '2'.
    el_cabecalho-nf_number9     = el_0007-invoicenumber+4(6).
    el_cabecalho-gross_amount   = el_0007-totalinvoice.
    el_cabecalho-exch_rate      = 1.
    el_cabecalho-chave_nfe      = el_0007-invoicekey.
    el_cabecalho-item_text      = |{ 'NexInvoice ID: ' } { el_0007-invoiceid alpha = out }|.
    el_cabecalho-partner_bk     = '0001'.
    el_cabecalho-doc_date_ven   = el_0007-duedate.
    el_cabecalho-doc_date_cal   = el_0007-duedate.
    el_cabecalho-boleto         = el_0007-barcode.
* Item.
    el_itens-invoice_doc_item   = 1.
    el_itens-po_number          = el_0007-ebeln.
    el_itens-po_item            = '00010'.
    el_itens-tax_code           = el_0007-mwskz.
    el_itens-quantity           = 1.
    read table tl_mara into data(el_mara) with key matnr = el_0007-matnr binary search.
    el_itens-po_unit            = el_mara-meins.
    clear el_mara.
    read table tl_ekpo into data(el_ekpo) with key ebeln = el_0007-ebeln.
    el_itens-item_amount        = el_ekpo-netpr.
    el_itens-ref_doc            = el_0007-mblnr.
    el_itens-ref_doc_year       = el_0007-mjahr.
    el_itens-ref_doc_it         = 1.

    append el_itens to tl_itens.
    clear el_itens.
* Objeto de Manibulação de MIRO - Criar MIRO.
    try.
        call method cll_miro->criar
          exporting
            i_cabecalho        = el_cabecalho
            i_itens            = tl_itens
            i_contas           = tl_contas
            i_lines_miro_texto = tl_ln_txt
            i_bapi_wait        = abap_off
          importing
            e_invoicedocnumber = data(vl_invoicedocnumber)
            e_fiscalyear       = data(vl_fiscalyear)
            e_retorno          = data(tl_retorno)
          receiving
            r_gerou            = data(vl_gerou).

      catch zcx_miro_exception into data(zcxl_miro_exception).
        append initial line to tl_retorno assigning field-symbol(<fs_retorno>).
        <fs_retorno>-type       = zcxl_miro_exception->msgty.
        <fs_retorno>-number     = zcxl_miro_exception->msgno.
        <fs_retorno>-message_v1 = zcxl_miro_exception->msgv1.
        <fs_retorno>-message_v2 = zcxl_miro_exception->msgv2.
        <fs_retorno>-message_v3 = zcxl_miro_exception->msgv3.
        <fs_retorno>-message_v4 = zcxl_miro_exception->msgv4.
        <fs_retorno>-id         = zcxl_miro_exception->msgid.
        <fs_retorno>-message    = zcxl_miro_exception->get_text( ).
        <fs_retorno>-parameter  = 'ZCL_MIRO'.
        <fs_retorno>-row        = 1.
        <fs_retorno>-system     = sy-sysid && 'CLNT' && sy-mandt.
* Verifica se existe erros na criação do documento.
        read table tl_retorno transporting no fields with key type = 'E'.

        if sy-subrc is initial.
* Salva mensagens retornadas da criação do documento com erro.
          perform zf_log_erro_cria_doc tables tl_retorno
                                        using el_0007.

        endif.

        message <fs_retorno>-message type 'S' display like 'E'.

    endtry.

    if not vl_gerou            is initial and
       not vl_invoicedocnumber is initial.
      update zfit0007
         set belnr = vl_invoicedocnumber
             gjahr = vl_fiscalyear
      where bukrs     eq el_0007-bukrs
        and werks     eq el_0007-werks
        and invoiceid eq el_0007-invoiceid.

      if sy-subrc is initial.
        commit work.
* Verifica mensagens de LOG da criação do documento com erro para eliminar.
        perform zf_log_check_cria_doc using el_0007.
*** Atualiza a base da NextInvoice chamando a PI Autom. Fatura Energia - Atual. Status.
        el_fatura_energia_upst-invoiceid       = el_0007-invoiceid.
        el_fatura_energia_upst-paymentprotocol = space.
        el_fatura_energia_upst-launchprotocol  = vl_invoicedocnumber && '/' && vl_fiscalyear.
        el_fatura_energia_upst-invoicestatus   = '5'. "Integrada
        el_fatura_energia_upst-releasedate     = el_cabecalho-doc_date_mov(4) && sy-uline(1) && el_cabecalho-doc_date_mov+4(2) && sy-uline(1) && el_cabecalho-doc_date_mov+6(2).
        el_fatura_energia_upst-paymentdate     = 'null'.
* Chama API Autom. Fatura Energia - Atual. Status.
        try.
            zcl_int_ob_fi_fat_ener_upst=>zif_integracao_outbound~get_instance( )->execute_request(
                 exporting
                   i_info_request           = el_fatura_energia_upst
                 importing
                   e_id_integracao          = data(resul_id)
                   e_integracao             = data(result_json)
                 ).
          catch zcx_integracao into data(zcx_integracao).
            message id zcx_integracao->msgid type 'E' number zcx_integracao->msgno with zcx_integracao->msgv1 zcx_integracao->msgv2 zcx_integracao->msgv3 zcx_integracao->msgv4.
            return.

          catch zcx_error into data(zcx_error).
            if resul_id is not initial.
              select single ds_data_retorno
                from zintegracao_log into @data(vl_data_return_erro)
                where id_integracao eq @resul_id.

              if     sy-subrc            is initial  and
                 not vl_data_return_erro is initial.
                /ui2/cl_json=>deserialize( exporting json = vl_data_return_erro changing data = el_return_erro ).

                if not el_return_erro-data is initial.
                  message |(WSC Atualiza status Automação Energia) { el_return_erro-data } | type 'S' display like 'E'.

                endif.

              endif.

            endif.

            message id zcx_error->msgid type 'S' number zcx_error->msgno with zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4 display like 'E'.

        endtry.

      else.
        rollback work.

      endif.

    else.
* Verifica se existe erros na criação do documento.
      read table tl_retorno transporting no fields with key type = 'E'.

      if sy-subrc is initial.
* Salva mensagens retornadas da criação do documento com erro.
        perform zf_log_erro_cria_doc tables tl_retorno
                                      using el_0007.

      endif.

    endif.

    clear: el_cabecalho, el_itens, tl_itens, tl_retorno, tl_contas, tl_ln_txt, vl_invoicedocnumber, vl_fiscalyear,
           el_fatura_energia_upst, vl_data_return_erro, el_return_erro.

  endloop.

endform.
*&---------------------------------------------------------------------*
*& Form zf_log_erro_cria_doc
*&---------------------------------------------------------------------*
*& Salva mensagens retornadas da criação do documento com erro
*&---------------------------------------------------------------------*
*&      --> PL_RETURN_PED TI de Parâmetro de retorno de mensagens
*&      --> UE_0007       WA da tabela Faturas NexInvoice
*&---------------------------------------------------------------------*
form zf_log_erro_cria_doc tables pt_return_ped structure bapiret2
                           using ue_0007       type      ty_0007.

  tables: zfit0013.

  data: tl_0013 type table of zfit0013.

  loop at pt_return_ped.
    at first.
      select single * from zfit0013 where bukrs     eq ue_0007-bukrs
                                      and werks     eq ue_0007-werks
                                      and invoiceid eq ue_0007-invoiceid.

      if sy-subrc is initial.
        delete from zfit0013 where bukrs     eq ue_0007-bukrs
                               and werks     eq ue_0007-werks
                               and invoiceid eq ue_0007-invoiceid.

        if sy-subrc is initial.
          commit work.

        else.
          rollback work.

        endif.

      endif.

    endat.

    move-corresponding: pt_return_ped to zfit0013,
                        ue_0007       to zfit0013.
    move: pt_return_ped-number    to zfit0013-znumber,
          pt_return_ped-parameter to zfit0013-zparameter,
          pt_return_ped-row       to zfit0013-zrow,
          pt_return_ped-system    to zfit0013-zsystem,
          sy-tabix                to zfit0013-seq_registro.

    append zfit0013 to tl_0013.
    clear: zfit0013.

  endloop.

  if not tl_0013[] is initial.
    insert zfit0013 from table tl_0013.

    if sy-subrc is initial.
      commit work.

    else.
      rollback work.

    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*& Form zf_log_check_cria_doc
*&---------------------------------------------------------------------*
*& Verifica mensagens de LOG da criação do documento com erro para eliminar
*&---------------------------------------------------------------------*
*&      -->UE_0007 - WA da tabela Faturas NexInvoice
*&---------------------------------------------------------------------*
form zf_log_check_cria_doc using ue_0007 type ty_0007.

  delete from zfit0013 where bukrs     eq ue_0007-bukrs
                         and werks     eq ue_0007-werks
                         and invoiceid eq ue_0007-invoiceid.

  if sy-subrc is initial.
    commit work.

  else.
    rollback work.

  endif.

endform.

"Melhorias automação Lanç Fatura de Energia #166647 - BG  -- INICIO
form atualiza_icms.
  constants:
    read(4) type c value 'READ',
    edit(4) type c value 'EDIT',
    save(4) type c value 'SAVE'.

  data: wa_j_1btxic3    type j_1btxic3.

  data: lt_returns  type table of bapiret2.

  data: chdat(8)   type c,
        houtput(8) type n.

  data  function    like sy-ucomm.
  data: variant_for_selection like tvimv-variant,
        view_name             like  dd02v-tabname.

  data: begin of status_j_1btxic3v. "state vector
          include structure vimstatus.
  data: end of status_j_1btxic3v.

  data: begin of header occurs 1.
          include structure vimdesc.
  data: end of header.

  data: begin of namtab occurs 50.
          include structure vimnamtab.
  data: end of namtab.

  data: rangetab    type table of vimsellist initial size 50
         with header line,
        oc_rangetab type table of vimsellist initial size 50.

  data: dpl_sellist    type table of vimsellist initial size 50 with header line.

  data: begin of e071k_tab occurs 100.    "keys of changed entries
          include structure e071k.       "(used as parameter for VIEWPROC)
  data: end of e071k_tab.

  data: org_crit_inst type vimty_oc_type, lockuser type sy-uname.
  data: excl_cua_funct like vimexclfun occurs 0 with header line.
  data: begin of j_1btxic3v_extract occurs 0010.
          include structure j_1btxic3v.
          include structure vimflagtab.
  data: end of j_1btxic3v_extract.
* Table for all entries loaded from database
  data: begin of j_1btxic3v_total occurs 0010.
          include structure j_1btxic3v.
          include structure vimflagtab.
  data: end of j_1btxic3v_total.


  refresh namtab.
  clear   namtab.
  refresh header.
  clear   header.
  refresh rangetab.
  clear   rangetab.
  call function 'VIEW_GET_DDIC_INFO'
    exporting
      viewname              = 'J_1BTXIC3V' "view_name
      variant_for_selection = variant_for_selection
    tables
      x_header              = header
      x_namtab              = namtab
      sellist               = rangetab
    exceptions
      no_tvdir_entry        = 3
      table_not_found       = 5.

  "gravar GRUOP 31  rangetab
  read table rangetab assigning field-symbol(<rangetab>) with key viewfield = 'GRUOP'.
  if sy-subrc = 0.
    <rangetab>-value = '31'.
  endif.
  read table rangetab assigning <rangetab> with key viewfield = 'LAND1'.
  if sy-subrc = 0.
    <rangetab>-value = 'BR'.
  endif.

  read table header index 1.
  call function 'VIEWPROC_J_1BTXIC3V'
    exporting
      fcode                     = read
      view_action               = 'U' "view_action
      view_name                 = 'J_1BTXIC3V' "view_name
    tables
      excl_cua_funct            = excl_cua_funct
      extract                   = j_1btxic3v_extract
      total                     = j_1btxic3v_total
      x_header                  = header
      x_namtab                  = namtab
      dba_sellist               = rangetab "dba_sellist
      dpl_sellist               = dpl_sellist
      corr_keytab               = e071k_tab
    exceptions
      missing_corr_number       = 1
      no_value_for_subset_ident = 2.
  case sy-subrc.
    when 1.
      raise missing_corr_number.
    when 2.
      raise no_value_for_subset_ident.
  endcase.
  "
  status_j_1btxic3v-upd_flag = 'X'.
*
  refresh:  j_1btxic3v_extract, j_1btxic3v_total.

  clear: j_1btxic3v_extract, j_1btxic3v_total.
  j_1btxic3v_extract-land1      = 'BR'.
  j_1btxic3v_extract-gruop      = '31'.
  j_1btxic3v_extract-shipfrom   = ''.
  j_1btxic3v_extract-shipto     = ''.
  j_1btxic3v_extract-value      = ''.
  j_1btxic3v_extract-value2     = ''.
  j_1btxic3v_extract-value3     = ''.
  j_1btxic3v_extract-rate       = 'base'.
  j_1btxic3v_extract-base       = ''.
*    if wa_icms_j1btxic3-icms_base lt 100.
*      j_1btxic3v_extract-taxlaw     = 'IA2'.
*    endif.
*    select single *
*       into wa_j_1btxic3
*       from j_1btxic3
*       where land1     = 'BR'
*       and   shipfrom  = wa_regio_parid
*    and   shipto    = wa_regio_branch
*    and   gruop     = '31'
*    and   value     = p_branch
*    and   value2    = wl_itens-matnr
*    and   value3    = p_parid.
*    if sy-subrc = 0.
*      j_1btxic3v_extract-validto    = wa_j_1btxic3-validto.
*      j_1btxic3v_extract-validfrom  = wa_j_1btxic3-validfrom.
*      j_1btxic3v_extract-action     = 'U'.
*    else.
**      MOVE sy-datum TO chdat.
*      move '20010101' to chdat.
*      houtput = '99999999' - chdat.
*      j_1btxic3v_extract-validfrom  = houtput.
*      move '99991231' to chdat.
*      houtput = '99999999' - chdat.
*      j_1btxic3v_extract-validto    = houtput.
*      j_1btxic3v_extract-action     = 'N'.
*    endif.
  append j_1btxic3v_extract.
  move-corresponding j_1btxic3v_extract to j_1btxic3v_total.
  append j_1btxic3v_total.
*  endloop.

  if j_1btxic3v_extract[] is not initial.
    call function 'VIEWPROC_J_1BTXIC3V'
      exporting
        fcode                     = save
        view_action               = 'U' "maint_mode
        view_name                 = 'J_1BTXIC3V' "view_name
        corr_number               = ' ' "corr_number
      importing
        update_required           = status_j_1btxic3v-upd_flag
      tables
        excl_cua_funct            = excl_cua_funct
        extract                   = j_1btxic3v_extract
        total                     = j_1btxic3v_total
        x_header                  = header
        x_namtab                  = namtab
        dba_sellist               = rangetab "dba_sellist
        dpl_sellist               = dpl_sellist
        corr_keytab               = e071k_tab
      exceptions
        missing_corr_number       = 1
        no_value_for_subset_ident = 2
        saving_correction_failed  = 3.
    case sy-subrc.
      when 1.
        raise missing_corr_number.
      when 2.
        raise no_value_for_subset_ident.
      when 3.
    endcase.
    "
    clear  status_j_1btxic3v-upd_flag.
    function = save.
    excl_cua_funct-function = 'ANZG'.
    append excl_cua_funct.
    excl_cua_funct-function = 'NEWL'.
    append excl_cua_funct.
    excl_cua_funct-function = 'KOPE'.
    append excl_cua_funct.
    excl_cua_funct-function = 'DELE'.
    append excl_cua_funct.
    excl_cua_funct-function = 'ORGI'.
    append excl_cua_funct.
    excl_cua_funct-function = 'MKAL'.
    append excl_cua_funct.
    excl_cua_funct-function = 'MKBL'.
    append excl_cua_funct.
    excl_cua_funct-function = 'MKLO'.
    append excl_cua_funct.
    excl_cua_funct-function = 'HELP'.
    append excl_cua_funct.

*      call function 'VIEWPROC_J_1BTXIC3V'
*        exporting
*          fcode                     = edit
*          view_action               = 'U' "maint_mode
*          view_name                 = 'J_1BTXIC3V' "view_name
*          corr_number               = '' "corr_number
*        importing
*          ucomm                     = function
*          update_required           = status_j_1btxic3v-upd_flag
*        tables
*          excl_cua_funct            = excl_cua_funct
*          extract                   = j_1btxic3v_extract
*          total                     = j_1btxic3v_total
*          x_header                  = header
*          x_namtab                  = namtab
*          dba_sellist               = rangetab "dba_sellist
*          dpl_sellist               = dpl_sellist
*          corr_keytab               = e071k_tab
*        exceptions
*          missing_corr_number       = 1
*          no_value_for_subset_ident = 2.
*      case sy-subrc.
*        when 1.
*
*        when 2.
*
*        when others.
*          exit.
*      endcase.

  endif.
endform.

form f_atualiza_j1btax changing el_0007 type ty_0007.
  IF el_0007-base = 0.
     exit.
  ENDIF.
  "Melhorias automação Lanç Fatura de Energia #166647 - BG -- INICIO
  refresh namtab.
  clear   namtab.
  refresh header.
  clear   header.
  refresh rangetab.
  clear   rangetab.
  call function 'VIEW_GET_DDIC_INFO'
    exporting
      viewname              = 'J_1BTXIC3V' "view_name
      variant_for_selection = variant_for_selection
    tables
      x_header              = header
      x_namtab              = namtab
      sellist               = rangetab
    exceptions
      no_tvdir_entry        = 3
      table_not_found       = 5.

  "gravar GRUOP 31  rangetab
  read table rangetab assigning field-symbol(<rangetab>) with key viewfield = 'GRUOP'.
  if sy-subrc = 0.
    <rangetab>-value = '31'.
  endif.
  read table rangetab assigning <rangetab> with key viewfield = 'LAND1'.
  if sy-subrc = 0.
    <rangetab>-value = 'BR'.
  endif.

  read table header index 1.
  call function 'VIEWPROC_J_1BTXIC3V'
    exporting
      fcode                     = read
      view_action               = 'U' "view_action
      view_name                 = 'J_1BTXIC3V' "view_name
    tables
      excl_cua_funct            = excl_cua_funct
      extract                   = j_1btxic3v_extract
      total                     = j_1btxic3v_total
      x_header                  = header
      x_namtab                  = namtab
      dba_sellist               = rangetab "dba_sellist
      dpl_sellist               = dpl_sellist
      corr_keytab               = e071k_tab
    exceptions
      missing_corr_number       = 1
      no_value_for_subset_ident = 2.
  case sy-subrc.
    when 1.
      raise missing_corr_number.
    when 2.
      raise no_value_for_subset_ident.
  endcase.
  "
  status_j_1btxic3v-upd_flag = 'X'.
*
  refresh:  j_1btxic3v_extract, j_1btxic3v_total.

  clear: j_1btxic3v_extract, j_1btxic3v_total.
  j_1btxic3v_extract-land1      = 'BR'.
  j_1btxic3v_extract-gruop      = '31'.
  read table it_lfa1_orig into data(wa_lfa1_orig) with key lifnr = el_0007-lifnr BINARY SEARCH.
  if sy-subrc is initial.
    j_1btxic3v_extract-shipfrom   = wa_lfa1_orig-regio.
    j_1btxic3v_extract-value3     = wa_lfa1_orig-lifnr.
  endif.
  read table it_lfa1_dest into data(wa_lfa1_dest) with key lifnr = el_0007-lifnrw BINARY SEARCH.
  if sy-subrc is initial.
    j_1btxic3v_extract-shipto     = wa_lfa1_dest-regio.
  endif.
  j_1btxic3v_extract-value      = el_0007-werks.
  j_1btxic3v_extract-value2     = el_0007-matnr.
  j_1btxic3v_extract-rate       = el_0007-aliquota.
*    j_1btxic3v_extract-base       = el_0007-BASE.
  j_1btxic3v_extract-base       =  ( el_0007-base / el_0007-totalinvoice ) * 100.
  if j_1btxic3v_extract-base lt 100.
    j_1btxic3v_extract-taxlaw     = 'IA2'.
  endif.
  select single *
     into wa_j_1btxic3
     from j_1btxic3
     where land1     = 'BR'
     and   shipfrom  = wa_lfa1_orig-regio
  and   shipto    = wa_lfa1_dest-regio
  and   gruop     = '31'
  and   value     = el_0007-werks
  and   value2    = el_0007-matnr
  and   value3    = wa_lfa1_orig-lifnr.
  if sy-subrc = 0.
    j_1btxic3v_extract-validto    = wa_j_1btxic3-validto.
    j_1btxic3v_extract-validfrom  = wa_j_1btxic3-validfrom.
    j_1btxic3v_extract-action     = 'U'.
  else.
    move '20010101' to chdat.
    houtput = '99999999' - chdat.
    j_1btxic3v_extract-validfrom  = houtput.
    move '99991231' to chdat.
    houtput = '99999999' - chdat.
    j_1btxic3v_extract-validto    = houtput.
    j_1btxic3v_extract-action     = 'N'.
  endif.
  append j_1btxic3v_extract.
  move-corresponding j_1btxic3v_extract to j_1btxic3v_total.
  append j_1btxic3v_total.


  if j_1btxic3v_extract[] is not initial.
    call function 'VIEWPROC_J_1BTXIC3V'
      exporting
        fcode                     = save
        view_action               = 'U' "maint_mode
        view_name                 = 'J_1BTXIC3V' "view_name
        corr_number               = ' ' "corr_number
      importing
        update_required           = status_j_1btxic3v-upd_flag
      tables
        excl_cua_funct            = excl_cua_funct
        extract                   = j_1btxic3v_extract
        total                     = j_1btxic3v_total
        x_header                  = header
        x_namtab                  = namtab
        dba_sellist               = rangetab "dba_sellist
        dpl_sellist               = dpl_sellist
        corr_keytab               = e071k_tab
      exceptions
        missing_corr_number       = 1
        no_value_for_subset_ident = 2
        saving_correction_failed  = 3.
    case sy-subrc.
      when 1.
        raise missing_corr_number.
      when 2.
        raise no_value_for_subset_ident.
      when 3.
    endcase.
    "
  endif.

  "Atualiza J1BTAX novamente para cada fatura

endform.
"Melhorias automação Lanç Fatura de Energia #166647 - BG  -- FIM
