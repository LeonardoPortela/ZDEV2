*----------------------------------------------------------------------*
***INCLUDE LZMMG006F01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_XML_TESTE
*&---------------------------------------------------------------------*
form f_xml_teste changing p_xml type string.

  p_xml = '<?xml version="1.0" encoding="UTF-8"?><requisition-header>' &&
  '<custom-fields><tipo-de-requisicao><external-ref-num>RP</external-ref-num>' &&
  '<external-ref-code>CP|RP</external-ref-code></tipo-de-requisicao>' &&
  '<req-automatica type="boolean">true</req-automatica><justificativa-para-compra-emergencial>' &&
  'teste</justificativa-para-compra-emergencial></custom-fields><currency><code>BRL</code></currency><requested-by>' &&
  '<login>filippo</login></requested-by><ship-to-address><location-code>0155</location-code>' &&
  '</ship-to-address><requisition-lines type="array"><requisition-line><line-num type="integer">' &&
  '1</line-num><unit-price type="decimal">120.00</unit-price><quantity type="decimal">1.0' &&
  '</quantity><need-by-date type="dateTime">2022-08-31T00:00:00-05:00</need-by-date><item>' &&
  '<name>368300 PREST SERV AFERICAO BALANCA - 3002701 SERV AFERICAO BALANCA</name></item>' &&
  '<account><name>0001 - AMAGGI EXPORTACAO E IMPORTACAO LTDA-0155 - AMAGGI LUCAS DO RIO VERDE INDUSTRIA-Ordem de Manutenção-0010550198 - Manut Mec Fab-7105159 -' &&
   'MANUTENÇÃO TELA MOINHO-0010 - REALIZAR MANUTENÇÃO NA TELA MOINHO-412107 - ' &&
   'Peças e acessórios - manut. mecânica</name><code>0001-0155-FM-0010550198-7105159-0010-412107</code>' &&
   '<active type="boolean">true</active><account-type><name>Plano de Contas AMAGGI</name><currency><code>BRL' &&
   '</code></currency></account-type></account><currency><code>BRL</code></currency><custom-fields><risco-de-terceiros/>' &&
   '<preco-por>1</preco-por><requisicao_sap>123</requisicao_sap></custom-fields></requisition-line></requisition-lines></requisition-header>'.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_CAB
*&---------------------------------------------------------------------*
form f_preenche_cab using p_eban type zmms_dados_int_coupa_eban
                   changing p_inter type zmms_dados_int_coupa_intermedi.


*  Seleciona anexo vinculado a requisição.
  select * from zmmt0181 into table @data(it_zmmt0181)
    where id_documento eq @p_eban-banfn.
  if sy-subrc eq 0.
    loop at it_zmmt0181 into data(ws_zmmt0181).
      append value #( id = ws_zmmt0181-id_retorno_anexo ) to p_inter-attachments.
    endloop.
  endif.

  if p_eban-prio_urg = 1.
    p_inter-external_ref_num = 'RE'.
  else.
    p_inter-external_ref_num = 'RP'.
  endif.

  p_inter-afnam = p_eban-afnam.

  p_inter-external_ref_code = 'CP' && '|' && p_inter-external_ref_num.

  if p_eban-cab_txt is not initial.
    p_inter-compra_emergencial = p_eban-cab_txt.
  else.
    p_inter-compra_emergencial = 'Requisição automatica SAP'.
  endif.

  p_inter-currency = p_eban-waers.

  "p_inter-requested_by = p_eban-ernam.
  p_inter-requested_by = 'integra.requisicao'.

  p_inter-location_code = p_eban-werks.
  p_inter-afnam = p_eban-afnam.

*>>>Begin-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo
  if p_eban-steus_afvc = 'PM04'.
    p_inter-external_ref_num  = 'SCAE'.
    p_inter-external_ref_code = 'SCAE'.
  endif.
*<<<End-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo


endform.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_ITEM
*&---------------------------------------------------------------------*
form f_preenche_item using  p_item_count type i
                   changing p_inter type zmms_dados_int_coupa_intermedi
                            p_eban type zmms_dados_int_coupa_eban.
  data: vlifnr_coupa type eban-lifnr_coupa.
  data: vseq   type i,
        vcont  type i,
        vtxz01 type ekpo-txz01.
  vcont = 0.

  append initial line to p_inter-requisition_lines assigning field-symbol(<fs_item>).

  "-----#01#
  write p_item_count to <fs_item>-line_num no-zero left-justified.

  "-----#02#
  if p_eban-steus_afvc = 'PM04'.          "BUG SOLTO 166532
    <fs_item>-unit_price = p_eban-preis.  "BUG SOLTO 166532
  else.
    <fs_item>-unit_price = '0.00'.  "p_eban-preis. "enviar ZERO para o coupa assumir o valor do contrato
  endif.
  condense <fs_item>-unit_price no-gaps.

  <fs_item>-afnam = p_eban-afnam. "Incluido / AOENNING.

  "-----#03#
  <fs_item>-quantity = p_eban-menge.
  condense <fs_item>-quantity no-gaps.

  "-----#04#
  <fs_item>-need_by_date = p_eban-lfdat(4) && gc_hifen && p_eban-lfdat+4(2) && gc_hifen && p_eban-lfdat+6(2) && 'T00:00:00-05:00'.

  "-----#05#
  if p_eban-pstyp ne '9'.
    vtxz01 = p_eban-txz01.
*    DATA(_tam) = strlen( vtxz01 ).
*    DO _tam TIMES.
*      IF vtxz01+vseq(1) NE space .
*        vcont = 0.
*      ENDIF.
*      IF vtxz01+vseq(1) EQ space.
*        ADD 1 TO vcont.
*      ENDIF.
*      IF vcont GT 1.
*        vtxz01+vseq(1) = '#'.
*      ENDIF.
*      ADD 1 TO vseq.
*    ENDDO.
*    REPLACE ALL OCCURRENCES OF '#' IN vtxz01 WITH ''.

    <fs_item>-item = p_eban-matnr &&  ` - ` && vtxz01.

  else.

    if p_eban-esll_srvpos is initial.

      <fs_item>-item = p_eban-matnr &&  ` - ` && p_eban-txz01.

    else.

      <fs_item>-item = p_eban-matnr &&  ` ` && p_eban-txz01 &&  ` - ` && p_eban-esll_srvpos && ` ` && p_eban-esll_ktext1.

    endif.

    "PERFORM f_remove_char_special CHANGING <fs_item>-item.

  endif.

  "-----#06#
*  <fs_item>-account_name = p_eban-t001_bukrs &&  gc_hifen &&  p_eban-t001_butxt && gc_hifen &&
*                           p_eban-werks &&  gc_hifen &&  p_eban-t001w_name1 &&  gc_hifen && 'FM' &&  gc_hifen &&
*                           p_eban-ebkn_kostl &&  gc_hifen &&  p_eban-cskt_ltext &&  gc_hifen &&
*                           p_eban-ebkn_aufnr &&  gc_hifen &&  p_eban-aufk_ktext &&  gc_hifen &&
*                           p_eban-v_npact_vornr &&  gc_hifen &&  p_eban-v_npact_ltxa1 &&  gc_hifen &&
*                           p_eban-ebkn_sakto &&  gc_hifen &&  p_eban-skat_txt50.

  "PERFORM f_remove_char_special CHANGING <fs_item>-account_name.

  "-----#07#
  if p_eban-knttp is not initial.

    <fs_item>-account_code = p_eban-t001_bukrs &&  gc_hifen && p_eban-werks &&  gc_hifen && 'FM' &&  gc_hifen &&
                             p_eban-ebkn_kostl &&  gc_hifen && p_eban-ebkn_aufnr &&  gc_hifen &&
                             p_eban-v_npact_vornr &&  gc_hifen && p_eban-ebkn_sakto.

    clear <fs_item>-deposito.
  else.

*    <fs_item>-account_code = p_eban-t001_bukrs &&  gc_hifen && p_eban-werks &&  gc_hifen && 'FM' &&
*                             gc_hifen && 'E' && gc_hifen && 'NA' && gc_hifen && p_eban-lgort.

    <fs_item>-account_code = p_eban-t001_bukrs &&  gc_hifen && p_eban-werks && gc_hifen && 'E' && gc_hifen && 'NA' && gc_hifen && p_eban-lgort.

    "-----#13#
    "IF p_eban-lgort IS NOT INITIAL.
    <fs_item>-deposito = p_eban-werks && '|' && p_eban-lgort.
    "ELSE.
    "<fs_item>-deposito = p_eban-werks.
    "ENDIF.


  endif.

  "PERFORM f_remove_char_special CHANGING <fs_item>-account_code.

  "-----#08#
  <fs_item>-account_type_name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

  "-----#09#
  <fs_item>-account_currency = p_eban-waers.

  "-----#10#
  <fs_item>-currency = p_eban-waers.

  "-----#11#
  "<fs_item>-risco_de_terceiros = 'TBD'.

  "-----#12#
  <fs_item>-preco_por = '1'.

  "-----#14#
  if p_eban-esll_extrow is not initial.
    <fs_item>-requisicao_sap = p_eban-banfn && '/' && p_eban-bnfpo && '/' && p_eban-esll_extrow.
  else.
    <fs_item>-requisicao_sap = p_eban-banfn && '/' && p_eban-bnfpo.
  endif.
  " preencher comprador
  data w024 type t024.
  select single *
    into w024
    from t024
    where ekgrp = p_eban-ekgrp.

  <fs_item>-comprador_responsvel-name = ''.
  <fs_item>-comprador_responsvel-external_ref_num  = p_eban-ekgrp.
  <fs_item>-comprador_responsvel-external_ref_code = w024-telfx && '|' && p_eban-ekgrp.

*>>>Begin-Stefanini-MM-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo
  if p_eban-steus_afvc = 'PM04'.

    <fs_item>-comprador_responsvel-name = ''.
    <fs_item>-comprador_responsvel-external_ref_num  = ''.
    <fs_item>-comprador_responsvel-external_ref_code = ''.

  endif.
*<<<End-Stefanini-MM-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo

  if p_eban-lifnr_coupa is not initial.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = p_eban-lifnr_coupa
      importing
        output = vlifnr_coupa.

    <fs_item>-supplier-number = vlifnr_coupa.
  else.
*    <fs_item>-supplier-number = '1'. "A definir
    clear <fs_item>-supplier-number.
  endif.

*>>>Begin-Stefanini-MM-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo
  if p_eban-steus_afvc = 'PM04'.

    <fs_item>-check_pm04 = abap_true.

    select single lifnr, pstyp
      from eban
      into @data(ls_eban)
      where banfn = @<fs_item>-requisicao_sap(10)
        and bnfpo = @<fs_item>-requisicao_sap+11(5).

    if ls_eban-lifnr is not initial.

      "<supplier_number>
      <fs_item>-supplier-number = |{ ls_eban-lifnr alpha = out }|.
    else.
      select single lifnr, pstyp
     from eban
     into @data(ws_eban)
     where banfn = @<fs_item>-requisicao_sap(10)
       and lifnr ne @space.
      if sy-subrc = 0.
        <fs_item>-supplier-number = |{ ws_eban-lifnr alpha = out }|.
      endif.
    endif.

    "<IVA>
    if ls_eban-pstyp = '0'.

      <fs_item>-iva-external_ref_code = 'Y1'.

    elseif ls_eban-pstyp = '9'.

      <fs_item>-iva-external_ref_code = 'S1'.

    endif.

    "<shipping_term>
    <fs_item>-shipping_term-code = 'CIF'.

    "<payment_term>
    <fs_item>-payment_term-code = '10 DDL'.

  elseif p_eban-steus_afvc = 'PM03' OR p_eban-steus_afvc = 'PM05'.
    <fs_item>-check_pm03 = abap_true.

    if ls_eban-lifnr is not initial.

      "<supplier_number>
      <fs_item>-supplier-number = |{ ls_eban-lifnr alpha = out }|.
    else.
      select single lifnr, pstyp
     from eban
     into @ws_eban
     where banfn = @<fs_item>-requisicao_sap(10)
       and lifnr ne @space.
      if sy-subrc = 0.
        <fs_item>-supplier-number = |{ ws_eban-lifnr alpha = out }|.
      endif.
    endif.


  elseif p_eban-steus_afvc = 'ESTQ' .


    <fs_item>-check_pm04 = 'E'. "ESTOQUE REVER ESTA LOGICA POIS NAOL ATENDE ESTOQUE


  endif.
*<<<End-Stefanini-MM-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo

  if p_eban-konnr_coupa is not initial.
    <fs_item>-contract-id = p_eban-konnr_coupa.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_INICIALIZA_XML_VARS
*&---------------------------------------------------------------------*
form f_inicializa_xml_vars changing p_cab_xml type string
                                    p_itens_xml type string
                                    p_full_xml type string
                                    p_cond_item_xml_pm04 type string.


  p_full_xml = '<?xml version="1.0" encoding="utf-8"?><requisition-header>' &&'<attachments type="array">#03#</attachments>'
               &&'#01#<requisition-lines type="array">#02#</requisition-lines></requisition-header>'.

  p_cab_xml = '<custom-fields><tipo-de-requisicao><external-ref-num>#01#</external-ref-num>' &&
               '<external-ref-code>#02#</external-ref-code></tipo-de-requisicao>' &&
               '<req-automatica type="boolean">true</req-automatica>' &&
               '<justificativa-para-compra-emergencial>#03#</justificativa-para-compra-emergencial>' &&
               '<solicitado-por>#15#</solicitado-por>' && '<fornecedor-determinado>#99#</fornecedor-determinado>' &&
               '</custom-fields><currency><code>#04#</code></currency><requested-by><login>#05#</login>' &&
               '</requested-by><ship-to-address><location-code>#06#</location-code></ship-to-address>'.

*  p_itens_xml = '<requisition-line><line-num type="integer">#01#</line-num><unit-price type="decimal">#02#</unit-price>' &&
*                '<quantity type="decimal">#03#</quantity><need-by-date type="dateTime">#04#</need-by-date><item>' &&
*                '<name>#05#</name></item><account><name>#06#</name><code>#07#</code><active type="boolean">true</active>' &&
*                '<account-type><name>#08#</name><currency><code>#09#</code></currency></account-type></account>' &&
*                '<currency><code>#10#</code></currency><custom-fields><risco-de-terceiros>#11#</risco-de-terceiros>' &&
*                '<preco-por>#12#</preco-por><deposito>#13#</deposito><requisicao_sap>#14#</requisicao_sap>' &&
*                '</custom-fields></requisition-line>'.

  p_itens_xml = '<requisition-line><line-num type="integer">#01#</line-num><unit-price type="decimal">#02#</unit-price>' &&
                '<quantity type="decimal">#03#</quantity><need-by-date type="dateTime">#04#</need-by-date><item>' &&
                '<name>#05#</name></item><account><code>#07#</code><active type="boolean">true</active>' &&
                '<account-type><name>#08#</name><currency><code>#09#</code></currency></account-type></account>' &&
                '<currency><code>#10#</code></currency><custom-fields><risco-de-terceiros>#11#</risco-de-terceiros>' &&
                '<preco-por>#12#</preco-por><deposito>#13#</deposito><requisicao_sap>#14#</requisicao_sap>' &&
                '<comprador_responsvel><name>#15#</name><external-ref-num>#16#</external-ref-num><external-ref-code>#17#</external-ref-code></comprador_responsvel>' &&
                '<solicitado-por>#18#</solicitado-por>' && '#24#' && '</custom-fields>' && '#25#' && '</requisition-line>'.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_REPLACE_CAB
*&---------------------------------------------------------------------*
form f_replace_cab using p_req type zmms_dados_int_coupa_intermedi
                changing p_cab_xml type string.

  replace '#01#' in p_cab_xml with p_req-external_ref_num.
  replace '#02#' in p_cab_xml with p_req-external_ref_code.
  replace '#03#' in p_cab_xml with p_req-compra_emergencial.
  replace '#04#' in p_cab_xml  with p_req-currency.
  replace '#05#' in p_cab_xml  with p_req-requested_by.
  replace '#06#' in p_cab_xml  with p_req-location_code.
  replace '#15#' in p_cab_xml  with p_req-afnam.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_REPLACE_ITEM
*&---------------------------------------------------------------------*
form f_replace_item using p_itens_tab type zmmc_int_coupa_intermedi_itens
                          p_template_xml type string
                          p_full_xml_pm04 type string
                          p_lv_full_xml_pm04_iva  type string
                          p_full_xml_estq type string
                          p_lv_full_xml_estq_iva  type string
                          p_lv_full_xml_pm03 type string
                 changing p_itens_xml  type string
                          p_cab_xml type string.

  data lv_item_temp type string.
  data lv_det(1).

  loop at p_itens_tab assigning field-symbol(<fs_item>).

    lv_item_temp = p_template_xml.

    replace '#01#' in lv_item_temp with <fs_item>-line_num.
    replace '#02#' in lv_item_temp with <fs_item>-unit_price.
    replace '#03#' in lv_item_temp with <fs_item>-quantity.
    replace '#04#' in lv_item_temp with <fs_item>-need_by_date.
    replace '#05#' in lv_item_temp with <fs_item>-item.

    replace '#06#' in lv_item_temp with <fs_item>-account_name.
    replace '#07#' in lv_item_temp with <fs_item>-account_code.
    replace '#08#' in lv_item_temp with <fs_item>-account_type_name.
    replace '#09#' in lv_item_temp with <fs_item>-account_currency.

    replace '#10#' in lv_item_temp with <fs_item>-currency.
    replace '#11#' in lv_item_temp with <fs_item>-risco_de_terceiros.
    replace '#12#' in lv_item_temp with <fs_item>-preco_por.
    replace '#13#' in lv_item_temp with <fs_item>-deposito.
    replace '#14#' in lv_item_temp with <fs_item>-requisicao_sap.

    replace '#15#' in lv_item_temp with <fs_item>-comprador_responsvel-name.
    replace '#16#' in lv_item_temp with <fs_item>-comprador_responsvel-external_ref_num.
    replace '#17#' in lv_item_temp with <fs_item>-comprador_responsvel-external_ref_code.

    replace '#18#' in lv_item_temp with <fs_item>-afnam.

    if <fs_item>-check_pm04 eq abap_true.
      replace '#24#' in lv_item_temp with p_lv_full_xml_pm04_iva.
      replace '#25#' in lv_item_temp with p_full_xml_pm04.
    elseif <fs_item>-check_pm04 eq 'E'.
      replace '#24#' in lv_item_temp with p_lv_full_xml_estq_iva.
      replace '#25#' in lv_item_temp with p_full_xml_estq.
    elseif <fs_item>-check_pm03 eq abap_true.
      replace '#24#' in lv_item_temp with space.
      replace '#25#' in lv_item_temp with p_lv_full_xml_pm03.
    else.
      replace '#24#' in lv_item_temp with space.
      replace '#25#' in lv_item_temp with space.
    endif.

    replace '#19#' in lv_item_temp with <fs_item>-iva.     "-MM-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo
    replace '#20#' in lv_item_temp with <fs_item>-contract-id.
    replace '#21#' in lv_item_temp with <fs_item>-supplier-number.
*    condense lv_item_temp no-gaps.

*>>>Begin-Stefanini-MM-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo
    replace '#22#' in lv_item_temp with <fs_item>-shipping_term.
    replace '#23#' in lv_item_temp with <fs_item>-payment_term.
*<<<End-Stefanini-MM-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo

    if <fs_item>-contract-id      is initial and
        <fs_item>-supplier-number is not initial.
      lv_det = 'X'.
    endif.


    p_itens_xml = p_itens_xml && lv_item_temp.

  endloop.

  if lv_det = 'X'.
    replace '#99#' in p_cab_xml  with 'true'.
  else.
    replace '#99#' in p_cab_xml  with 'false'.
  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_REPLACE_FULL
*&---------------------------------------------------------------------*
form f_replace_full using p_xml_full type string
                          p_cab_xml type string
                          p_itens_xml type string
                          p_req       type zmms_dados_int_coupa_intermedi
                 changing p_xml_ret type string.

  data: p_attachments type string.
  clear: p_attachments.
  loop at p_req-attachments into data(ws_attachments).
    if p_attachments is initial.
      p_attachments = |<attachments><id>{ ws_attachments-id }</id></attachments>|.
    else.
      p_attachments = |{ p_attachments }<attachments><id>{ ws_attachments-id }</id></attachments>|.
    endif.
  endloop.

  p_xml_ret = p_xml_full.

  replace '#01#' in p_xml_ret with p_cab_xml.
  replace '#02#' in p_xml_ret with p_itens_xml.
  replace '#03#' in p_xml_ret with p_attachments.

endform.
*&---------------------------------------------------------------------*
*& FORM F_REMOVE_CHAR_SPECIAL
*&---------------------------------------------------------------------*
form f_remove_char_special changing p_text type c.

  data lv_text2 type char100.

  lv_text2 = p_text.

  call function 'ES_REMOVE_SPECIAL_CHARACTER'
    exporting
      text1       = lv_text2
    importing
      corr_string = lv_text2.

  p_text = lv_text2.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_COMPACTA_XML
*&---------------------------------------------------------------------*
form f_compacta_xml changing p_xml type string
                         p_xstring type xstring.

  data lo_xml_ret type ref to cl_xml_document.

  data lv_ret_xml type string.

  lv_ret_xml = p_xml.

  clear p_xml.

  create object lo_xml_ret.

  if lo_xml_ret->parse_string( lv_ret_xml ) = 0.

    "lo_xml_ret->display( ).

    call method lo_xml_ret->render_2_string
      exporting
        pretty_print = 'X'
      importing
        retcode      = data(lv_ret)
        stream       = p_xml.

    call method lo_xml_ret->render_2_xstring
      exporting
        pretty_print = 'X'
      importing
        retcode      = lv_ret
        stream       = p_xstring.

  endif.

endform.
*&---------------------------------------------------------------------*
*& Form f_check_req_pm04
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IW_REQ
*&      <-- LV_FULL_XML_PM04
*&---------------------------------------------------------------------*
form f_check_req_pm04  using    p_iw_req
                       changing p_lv_full_xml_pm04
                                p_lv_full_xml_pm04_iva
                                p_lv_full_xml_estq
                                p_lv_full_xml_estq_iva.


  p_lv_full_xml_pm04_iva = '<iva><external-ref-code>#19#</external-ref-code></iva>'.
  p_lv_full_xml_pm04     = '<contract><id>#20#</id></contract><supplier><number>#21#</number></supplier>' && '<shipping_term><code>#22#</code></shipping_term>' &&
                       '<payment_term><code>#23#</code></payment_term>'.



  clear p_lv_full_xml_estq_iva.
  p_lv_full_xml_estq     = '<contract><id>#20#</id></contract><supplier><number>#21#</number></supplier>'.

endform.
*&---------------------------------------------------------------------*
*& Form f_check_req_pm03
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_EBAN
*&      <-- LV_FULL_XML_PM03
*&---------------------------------------------------------------------*
form f_check_req_pm03  using    p_it_eban
                       changing p_lv_full_xml_pm03.

  p_lv_full_xml_pm03  = '<supplier><number>#21#</number></supplier>'.

endform.
