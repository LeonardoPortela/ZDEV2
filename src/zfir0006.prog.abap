*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZFIR0006                                                                             *
*& Chamado        : USER STORY 140931                                                                    *
*& Data           : 24/12/2024                                                                           *
*& Especificado   : Antonio Rodrigues                                                                    *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 24/12/2024  |DEVK9A2C12  |NSEGATIN       |Desenvilvimento inicial. Chamado: 140931.                   *
*--------------------------------------------------------------------------------------------------------*
report zfir0006.

tables: zfit0007.

*--------------------------------------------------------------------*
* I N T E R N A L  T A B L E S                                       *
*--------------------------------------------------------------------*
data: tg_fatura_energia_invoifile type table of zfie_fatura_energia_nexinvoice.

*--------------------------------------------------------------------*
* S E L E C T I O N - S C R E E N                                    *
*--------------------------------------------------------------------*
* Tela de Seleção
selection-screen begin of block b1 with frame title text-001.
  parameters: p_dtini type ie_start_date obligatory,
              p_dtend type ie_end_date.

selection-screen end of block b1.

*--------------------------------------------------------------------*
* S T A R T - O F - S E L E C T I O N                                *
*--------------------------------------------------------------------*
start-of-selection.
  perform: zf_get_data,    "Busca dados para o processamento (API)
           zf_proces_data. "Processa dados selecionados pela API

*&---------------------------------------------------------------------*
*&      Form  ZF_GET_DATA
*&---------------------------------------------------------------------*
*       Busca dados para o processamento (API)
*----------------------------------------------------------------------*
form zf_get_data.

*--------------------------------------------------------------------*
* W O R K   A R E A S                                                *
*--------------------------------------------------------------------*
  data: el_fatura_energia_busca type zfie_fatura_energia_busca,
        el_return_erro          type zstruct_return_api_eudr.

  el_fatura_energia_busca-starterduedate = |{ p_dtini(4) && sy-uline(1) && p_dtini+4(2) && sy-uline(1) && p_dtini+6(2) }|.

  if p_dtend is initial.
    el_fatura_energia_busca-finishedduedate = el_fatura_energia_busca-starterduedate.

  else.
    el_fatura_energia_busca-finishedduedate = |{ p_dtend(4) && sy-uline(1) && p_dtend+4(2) && sy-uline(1) && p_dtend+6(2) }|.

  endif.
* Autom. Fatura Energia - Busca.
  try.
      zcl_int_ob_fi_fat_ener_busca=>zif_integracao_outbound~get_instance( )->execute_request(
           exporting
             i_info_request           = el_fatura_energia_busca
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
            message |(WSC Automação Energia) { el_return_erro-data } | type 'E'.

          endif.

        endif.

      endif.

      message id zcx_error->msgid type 'E' number zcx_error->msgno with zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
      return.

  endtry.
* Recupera os dados do JSON
  /ui2/cl_json=>deserialize( exporting json = result_json-ds_data_retorno changing data = tg_fatura_energia_invoifile ).

  if tg_fatura_energia_invoifile is initial.
    message 'Retorno da Consulta ao Serviço Automação Energia está inválida ou vazia!' type 'S' display like 'W'.
    stop.

  else.
    delete tg_fatura_energia_invoifile where invoicestatus ne '1'.

    if tg_fatura_energia_invoifile is initial.
      message 'Não há dados de fatura para processar' type 'S'.
      stop.

    endif.

  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  ZF_SHOW_DATA
*&---------------------------------------------------------------------*
*       Processa dados selecionados pela API
*----------------------------------------------------------------------*
form zf_proces_data.

*--------------------------------------------------------------------*
* I N T E R N A L  T A B L E S                                       *
*--------------------------------------------------------------------*
  data: tl_0007 type table of zfit0007.

*--------------------------------------------------------------------*
* W O R K   A R E A S                                                *
*--------------------------------------------------------------------*
  data: el_fatura_energia_upst type zfie_fatura_energia_upst,
        el_return_erro         type zstruct_return_api_eudr.

*--------------------------------------------------------------------*
* V A R I A B L E S                                                  *
*--------------------------------------------------------------------*
  data: vl_loop type i.

*--------------------------------------------------------------------*
* C O N S T A N T S                                                  *
*--------------------------------------------------------------------*
  constants: cl_codfil type char20 value 'Codigo da Filial',
             cl_codemp type char20 value 'Codigo da Empresa',
             cl_classi type char20 value 'Classificacao'.

  loop at tg_fatura_energia_invoifile into data(el_fatura_energia_invoifile).
* Retorno Custom Fields.
    loop at el_fatura_energia_invoifile-customfields into data(el_customfields).
      case el_customfields-fieldname.
        when cl_codfil. "Codigo da Filial
          move el_customfields-fieldvalue to zfit0007-werks.
          add 1 to vl_loop.

        when cl_codemp. "Codigo da Empresa
          move el_customfields-fieldvalue to zfit0007-bukrs.
          add 1 to vl_loop.

        when others.
*       Do nothing
      endcase.

      if vl_loop eq 2.
        clear vl_loop.
        exit.

      endif.

    endloop.

    "Melhorias automação Lanç Fatura de Energia #166647 - BG -- INICIO
    move el_fatura_energia_invoifile-energycomplement-energyclassification to zfit0007-energyclassification.
    move el_fatura_energia_invoifile-energycomplement-energysubgroup to zfit0007-energysubgroup.

    read table el_fatura_energia_invoifile-taxes into data(wa_taxes) with key taxname = 'ICMS'.
    if sy-subrc is initial.
      move wa_taxes-taxname to zfit0007-taxname.
      move wa_taxes-aliquot to zfit0007-aliquota.
      move wa_taxes-tax to zfit0007-taxa.
      move wa_taxes-basecalculation to zfit0007-base.
    endif.

    "Melhorias automação Lanç Fatura de Energia #166647 - BG -- FIM

    if not vl_loop is initial.
      clear vl_loop.

    endif.

    move: el_fatura_energia_invoifile-invoiceid          to zfit0007-invoiceid,
          el_fatura_energia_invoifile-invoiceaccesskey   to zfit0007-invoicekey,
          el_fatura_energia_invoifile-invoicenumber      to zfit0007-invoicenumber,
          el_fatura_energia_invoifile-suppliercnpj       to zfit0007-stcd1,
          el_fatura_energia_invoifile-contractnumber     to zfit0007-contractnumber,
          el_fatura_energia_invoifile-totalinvoiceamount to zfit0007-totalinvoice,
          el_fatura_energia_invoifile-barcode            to zfit0007-barcode.

    zfit0007-duedate       = el_fatura_energia_invoifile-duedate(4) && el_fatura_energia_invoifile-duedate+5(2) && el_fatura_energia_invoifile-duedate+8(2).
    zfit0007-referencedate = el_fatura_energia_invoifile-referencedate+6(4) && el_fatura_energia_invoifile-referencedate+3(2) && el_fatura_energia_invoifile-referencedate(2).

    select single lifnr from lfa1 into zfit0007-lifnr where stcd1 eq zfit0007-stcd1.
* Retorno Cost Allocation.
    loop at el_fatura_energia_invoifile-costallocations into data(el_costallocations).
      move: el_costallocations-custcenterdescription to zfit0007-kostl.
      exit.

    endloop.
* Retorno Invoice Files.
    read table el_fatura_energia_invoifile-invoicefiles into zfit0007-invoicefiles index 1.
* Usuário, data e hora e criação.
    move: sy-uname to zfit0007-us_criacao,
          sy-datlo to zfit0007-dt_criacao,
          sy-timlo to zfit0007-hr_criacao.

    append zfit0007 to tl_0007.
    clear: zfit0007.

  endloop.

  if not tl_0007 is initial.
    modify zfit0007 from table tl_0007.

    if sy-subrc is initial.
* Dados foram salvos com sucesso.
      message s104(zehs).
      commit work.

      loop at tl_0007 into zfit0007.
        el_fatura_energia_upst-invoiceid       = zfit0007-invoiceid.
        el_fatura_energia_upst-paymentprotocol = space.
        el_fatura_energia_upst-launchprotocol  = |Salvo na Base SAP|.
        el_fatura_energia_upst-invoicestatus   = '3'. "Integrada
        el_fatura_energia_upst-releasedate     = sy-datlo(4) && sy-uline(1) && sy-datlo+4(2) && sy-uline(1) && sy-datlo+6(2).
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

      endloop.

    else.
* Ocorreu erro ao gravar dados
* sy-abcde+4(1) = E - Erro
      message s073(gd) display like sy-abcde+4(1).
      rollback work.

    endif.

  endif.

endform.
