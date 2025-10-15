*&---------------------------------------------------------------------*
*& Report ZBSILVA1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbsilva1.

SELECT *
  FROM zi_t056p_valorbem_sum
  INTO TABLE @DATA(lt_valorbem)
  WHERE idcontrato = '1000000004616'.

BREAK-POINT.

IF  sy-subrc = 0.

  DATA(lv_valorsum) = 0.

  READ TABLE lt_valorbem  ASSIGNING FIELD-SYMBOL(<lfs_calculated_data_t>) INDEX 1.
  IF sy-subrc = 0.
    DATA(lv_idcontrato) = <lfs_calculated_data_t>-idcontrato.
  ENDIF.

  LOOP AT lt_valorbem ASSIGNING FIELD-SYMBOL(<lfs_calculated_data>).

    IF <lfs_calculated_data>-idcontrato <> lv_idcontrato.
      lv_idcontrato = <lfs_calculated_data>-idcontrato.
      lv_valorsum = 0.
    ENDIF.

    " Calcular ValorSum
    IF lv_valorsum = 0.
      lv_valorsum = <lfs_calculated_data>-Montante
                   + ( <lfs_calculated_data>-Montante * <lfs_calculated_data>-ValorZsoll ).
    ELSE.
      lv_valorsum = lv_valorsum
                   + ( lv_valorsum * <lfs_calculated_data>-ValorZsoll ).
    ENDIF.

    <lfs_calculated_data>-ValorSum = lv_valorsum.

  ENDLOOP.


ENDIF.

**DATA: lo_objeto TYPE REF TO zcl_int_ob_safra_crt_order.
**CREATE OBJECT lo_objeto.
**
**CALL METHOD lo_objeto->set_metodo_http
**  EXPORTING
**    i_metodo = zif_integracao_inject=>co_request_method_post. " Ou PUT, GET, DELETE
**
**TRY.
**lo_objeto->zif_integracao_outbound~get_instance( )->execute_request(
**   EXPORTING
**    i_info_request  = ls_ordem "Com estruturas que podem ser ZDE_SAFRA_CONTROL_ORDEM ou ZDE_SAFRA_OD_EXTERNALID
**  IMPORTING
**    e_id_integracao = DATA(resul_id)
**    e_integracao    = DATA(result_json) ).
**CATCH zcx_integracao.
**CATCH zcx_error.
*
*
**DATA: lo_objeto TYPE REF TO zcl_int_ob_safra_crt_contact.
**CREATE OBJECT lo_objeto.
**
**TRY.
**    CALL METHOD lo_objeto->set_metodo_http
**      EXPORTING
**        i_metodo = zif_integracao_inject=>co_request_method_post. " Ou PUT, GET, DELETE
**
**    lo_objeto->zif_integracao_outbound~get_instance( )->execute_request(
**      EXPORTING
**        i_info_request           = ls_contacts      "Com estruturas que podem ser ZDE_SAFRACONTROL_CONTATCS ou ZDE_SAFRA_EXTID_CONTROL
**      IMPORTING
**        e_id_integracao          = DATA(resul_id)                 " Id. de Integração
**        e_integracao             = DATA(result_json)                 " Tabela de Integração
**    ).
**  CATCH zcx_integracao. " Classe de Erro de Integração
**  CATCH zcx_error.      " Classe de Erro Genérica
*
*DATA: lo_objeto TYPE REF TO zcl_int_ob_safra_crt_product.
*CREATE OBJECT lo_objeto.
*
*TRY.
*    CALL METHOD lo_objeto->set_metodo_http
*      EXPORTING
*        i_metodo = zif_integracao_inject=>co_request_method_post. " Ou PUT, GET, DELETE
*
*    lo_objeto->zif_integracao_outbound~get_instance( )->execute_request(
*      EXPORTING
*        i_info_request           = ls_product      "Com estruturas que podem ser ZDE_SAFRA_CONTROL_PRODUCTS ou ZDE_SAFRA_EXTID_PRD
*      IMPORTING
*        e_id_integracao          = DATA(resul_id)                   " Id. de Integração
*        e_integracao             = DATA(result_json)                " Tabela de Integração
*    ).
*  CATCH zcx_integracao. " Classe de Erro de Integração
*  CATCH zcx_error.      " Classe de Erro Genérica


*DATA(LV_TESTE1) = 'OI'.
*DATA(LV_TESTE2) = 'OLÁ'.
*BREAK-POINT.
*DATA(v_url) = |{ LV_TESTE1 }/{ LV_TESTE2 }|.


*DATA: lv_fullpath TYPE string,
*      lv_xstring  TYPE xstring,
*      lv_b64      TYPE string.
*
*DATA: lv_rc         TYPE sy-subrc,
*      lv_msg        TYPE string,
*      lt_file_table TYPE filetable,
*      lt_bin        TYPE STANDARD TABLE OF x255.
**
**PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.
*
*PARAMETERS: p_file TYPE string.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  CALL METHOD cl_gui_frontend_services=>file_open_dialog
*    EXPORTING
*      file_filter = 'Excel Files (*.xlsx)|*.xlsx|All Files (*.*)|*.*'
*    CHANGING
*      file_table  = lt_file_table
*      rc          = lv_rc
*      user_action = lv_rc
*    EXCEPTIONS
*      OTHERS      = 1.
*  IF sy-subrc IS INITIAL.
*    p_file = lt_file_table[ 1 ].
*  ENDIF.
*
*START-OF-SELECTION.
*
*  " Carregar arquivo .xlsx do computador e converter para XSTRING
*  CALL METHOD cl_gui_frontend_services=>gui_upload
*    EXPORTING
*      filename        = p_file
*      filetype        = 'BIN'
*    IMPORTING
*      filelength      = DATA(lv_length)
*    CHANGING
*      data_tab        = lt_bin " TYPE STANDARD TABLE OF x255
*    EXCEPTIONS
*      file_open_error = 1
*      file_read_error = 2
*      OTHERS          = 3.
*
*  IF sy-subrc <> 0.
*    WRITE: 'Erro ao carregar o arquivo.'.
*    EXIT.
*  ENDIF.
*
*  " Converter binário para XSTRING
*  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*    EXPORTING
*      input_length = lv_length
*    IMPORTING
*      buffer       = lv_xstring
*    TABLES
*      binary_tab   = lt_bin.
*
*  " Codificar XSTRING em Base64
*  CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
*    EXPORTING
*      input  = lv_xstring
*    IMPORTING
*      output = lv_b64.
*
*  " Chamada da RFC
*  CALL FUNCTION 'ZMM_IB_BORDERO_RECEB_CD_LUFT'
*    EXPORTING
*      iv_xml         = lv_b64
*    EXCEPTIONS
*      write_error    = 1
*      log_save_error = 2
*      OTHERS         = 3.
*
*    IF sy-subrc = 0.
*
*      WRITE: / 'Chamada executada com sucesso!', v_url.
*    ELSE.
*      WRITE: / 'Erro na execução da RFC. SY-SUBRC:', sy-subrc.
*    ENDIF.
