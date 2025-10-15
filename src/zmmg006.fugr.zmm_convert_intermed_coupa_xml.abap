function zmm_convert_intermed_coupa_xml.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IW_REQ) TYPE  ZMMS_DADOS_INT_COUPA_INTERMEDI
*"     REFERENCE(IT_EBAN) TYPE  ZMMC_DADOS_INT_COUPA_EBAN OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_XML) TYPE  STRING
*"     REFERENCE(E_XML_X) TYPE  XSTRING
*"----------------------------------------------------------------------
  data: lv_full_xml          type string,
        lv_full_xml_pm04     type string,
        lv_full_xml_pm03     type string,
        lv_full_xml_pm04_iva type string,
        lv_full_xml_estq     type string,
        lv_full_xml_estq_iva type string.

  data lv_cab_xml type string.
  data lv_item_template_xml type string. " TEMPLATE DE ITEM
  data lv_itens_xml type string. " TODOS OS ITENS.

  perform f_check_req_pm04 using it_eban changing lv_full_xml_pm04 lv_full_xml_pm04_iva lv_full_xml_estq lv_full_xml_estq_iva.
  perform f_check_req_pm03 using it_eban changing lv_full_xml_pm03 .

  perform f_inicializa_xml_vars
    changing lv_cab_xml
             lv_item_template_xml
             lv_full_xml
             lv_full_xml_pm04.




  perform f_replace_cab
    using iw_req
 changing lv_cab_xml.

  perform f_replace_item
    using iw_req-requisition_lines
          lv_item_template_xml
          lv_full_xml_pm04
          lv_full_xml_pm04_iva
          lv_full_xml_estq
          lv_full_xml_estq_iva
          lv_full_xml_pm03
 changing lv_itens_xml
          lv_cab_xml.

  perform f_replace_full
    using lv_full_xml
          lv_cab_xml
          lv_itens_xml
          iw_req
 changing e_xml.

  call function 'SCMS_STRING_TO_XSTRING'
    exporting
      text   = e_xml
    importing
      buffer = e_xml_x
    exceptions
      failed = 1
      others = 2.

  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

  if sy-sysid = 'DEV'.
    clear e_xml.
    perform f_xml_teste changing e_xml.

  endif.

  "PERFORM f_compacta_xml CHANGING e_xml e_xml_x.

endfunction.
