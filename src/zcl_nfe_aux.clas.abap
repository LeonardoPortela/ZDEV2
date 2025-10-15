class ZCL_NFE_AUX definition
  public
  final
  create public .

public section.

  class-methods VERIFICAR_CNPJ_TERCEIRO
    importing
      !IV_CNPJ type J_1BCGC
    returning
      value(RV_RETURN) type SAP_BOOL .
  class-methods VALIDA_NFE_TERCEIRO
    importing
      !IO_PROC_PARAMS type ref to CL_EDOC_BR_PROC_PARAMS
    returning
      value(RV_RESULT) type EDOC_PROC_STEP_RESULT .
  class-methods GET_BUKRS_TERCEIRO
    returning
      value(RV_BUKRS) type BUKRS .
  class-methods GET_WERKS_TERCEIRO
    returning
      value(RV_WERKS) type WERKS_D .
  class-methods GET_ORGDATA_TERCEIRO
    importing
      !IV_CNPJ type J_1BCGC
    returning
      value(RT_ORGDATA) type J_1BNFE_ORGDATA_TAB .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NFE_AUX IMPLEMENTATION.


  method GET_BUKRS_TERCEIRO.

    rv_bukrs = '0001'.

  endmethod.


  METHOD get_orgdata_terceiro.

    DATA(lv_bukrs) = zcl_nfe_aux=>get_bukrs_terceiro( ).
    "DATA lv_cgc TYPE j_1bcgccom.
    DATA lv_cnpj TYPE j_1bwfield-cgc_number.

    CALL FUNCTION 'J_1BREAD_CGC_COMPANY'
      EXPORTING
        bukrs      = lv_bukrs
      IMPORTING
        cgc_number = lv_cnpj.

    CHECK lv_cnpj IS NOT INITIAL.

    CALL FUNCTION 'J_1BNFE_GET_ORGDATA_FROM_CNPJ'
      EXPORTING
        i_cnpj     = lv_cnpj
      IMPORTING
        et_orgdata = rt_orgdata
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.


  ENDMETHOD.


  METHOD get_werks_terceiro.

    rv_werks = '0101'.

  ENDMETHOD.


  METHOD valida_nfe_terceiro.

    DATA lv_cnpj TYPE j_1bcgc.

    DATA lo_xml_doc TYPE REF TO cl_xml_document.

    CREATE OBJECT lo_xml_doc.

    DATA lv_xml TYPE string.

    CHECK io_proc_params IS BOUND.

    DATA(lv_xmlx) = io_proc_params->get_xml( ).

    lv_xml = zcl_abap_convert=>xstring_to_string_utf8( lv_xmlx ).

    CHECK lv_xml IS NOT INITIAL.

    CHECK lo_xml_doc->parse_string( stream = lv_xml ) = 0.

    DATA(lo_dest) = lo_xml_doc->find_node( 'dest' ).

    DATA(lo_cnpj) = lo_dest->get_first_child( ).

    lv_cnpj = lo_cnpj->get_value( ).

    IF zcl_nfe_aux=>verificar_cnpj_terceiro( lv_cnpj ) = abap_true.
      rv_result = cl_edocument_br_in=>sc_edoc_result-status_authorized.
    ENDIF.

  ENDMETHOD.


  METHOD verificar_cnpj_terceiro.

    DATA lv_raiz TYPE c LENGTH 8.

    "Tratativa para quando o Destinatario for PF
    if iv_cnpj = '00000000000000' or
       iv_cnpj = space.
      rv_return = abap_true.
      RETURN.
    endif.

    IF strlen( iv_cnpj ) < 8.
      rv_return = abap_false.
      RETURN.
    ENDIF.

    lv_raiz = iv_cnpj(8).

    SELECT COUNT(*) FROM t001z
      WHERE party = 'J_1BCG'
        AND paval = lv_raiz.

    IF sy-dbcnt > 0.
      rv_return = abap_false.
    ELSE.
      rv_return = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
