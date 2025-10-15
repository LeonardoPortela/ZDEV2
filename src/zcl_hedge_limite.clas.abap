class ZCL_HEDGE_LIMITE definition
  public
  final
  create public .

public section.

  methods CONSULTA_LIMITE
    importing
      !IV_POPUP type FLAG default 'X'
      !IS_0094 type ZSDT0094
    returning
      value(RV_LIMITE_OK) type FLAG .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_HEDGE_LIMITE .
  methods CONSTRUCTOR .
protected section.
PRIVATE SECTION.

  CLASS-DATA go_instance TYPE REF TO zcl_hedge_limite .
  DATA gt_limite TYPE zsdc0372 .
  DATA gv_padrao_html TYPE string.

  METHODS mostra_popup
    IMPORTING
      !iv_mensagem         TYPE string
    RETURNING
      VALUE(rv_confirmado) TYPE flag .
  METHODS enviar_email
    IMPORTING
      !is_0094 TYPE zsdt0094 .
ENDCLASS.



CLASS ZCL_HEDGE_LIMITE IMPLEMENTATION.


  METHOD constructor.

    SELECT * FROM zsdt0372
      INTO TABLE gt_limite.

    gv_padrao_html = '<p>&Aacute;rea: #04#<br />Solicita&ccedil;&atilde;o: #01#<br />Valor BRL: #02#<br />' &&
                     'Valor USD: #03#</p>'.

  ENDMETHOD.


  METHOD consulta_limite.

    DATA lv_limite TYPE zvalorlimite.
    DATA lv_mess TYPE string.
    DATA lv_vlr_str TYPE c LENGTH 30.

    data lv_valor TYPE DMBTR.

    CLEAR rv_limite_ok.

    lv_valor = is_0094-total_proporc.

    IF lv_valor < 0.
      lv_valor = lv_valor * -1.
    ENDIF.

    CHECK lv_valor IS NOT INITIAL.

    LOOP AT gt_limite ASSIGNING FIELD-SYMBOL(<fs_limite>).

      " 100 < 105
      IF <fs_limite>-valorlimite < lv_valor.
        rv_limite_ok = abap_false.
      ELSE.
        rv_limite_ok = abap_true.
      ENDIF.

    ENDLOOP.

    CHECK rv_limite_ok IS INITIAL AND iv_popup IS NOT INITIAL.

    WRITE is_0094-total_proporc TO lv_vlr_str LEFT-JUSTIFIED.

    lv_mess = `Alerta Hedge! valor de R$ ` && lv_vlr_str.

    rv_limite_ok = me->mostra_popup( lv_mess ).

    CHECK rv_limite_ok = abap_true.

    me->enviar_email( is_0094 ).

  ENDMETHOD.


  METHOD enviar_email.

    DATA lt_email TYPE TABLE OF msgtx.
    DATA lt_stvarv TYPE	tvarvc_t.

    DATA at_receivers TYPE somlreci1_t.
    DATA as_doc_dat	TYPE sodocchgi1.
    DATA at_list  TYPE sopcklsti1_t.
    DATA at_contents_txt TYPE	html_table.
    DATA lv_field TYPE c LENGTH 20.
    DATA lv_body TYPE w3_html.
    DATA lv_dolar TYPE dmbtr.
    DATA lv_value TYPE c LENGTH 30.
    DATA lv_origem TYPE c LENGTH 40.

    IF is_0094-taxa_cambio IS NOT INITIAL.
      lv_dolar = is_0094-total_proporc / is_0094-taxa_cambio.
    ENDIF.

    lv_body = gv_padrao_html.

    CLEAR: at_receivers, as_doc_dat, at_list, at_contents_txt.

    LOOP AT gt_limite ASSIGNING FIELD-SYMBOL(<fs_stvar>).


      "LOOP AT lt_stvarv ASSIGNING FIELD-SYMBOL(<fs_stvar>).

      DO 4 TIMES.

        lv_field = '<fs_stvar>-email_' && '0' && sy-index.

        ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_value>).

        CHECK sy-subrc EQ 0.

        IF <fs_value> IS NOT INITIAL.

          APPEND INITIAL LINE TO at_receivers ASSIGNING FIELD-SYMBOL(<fs_rec>).
          <fs_rec>-receiver = <fs_value>.
          <fs_rec>-rec_type = 'U'.

        ENDIF.

      ENDDO.

    ENDLOOP.

    APPEND INITIAL LINE TO at_list ASSIGNING FIELD-SYMBOL(<fs_list>).

    <fs_list>-head_start = 1.
    <fs_list>-head_num = 0.
    <fs_list>-body_start = 1.
    <fs_list>-body_num = 99999.
    <fs_list>-doc_type =  'HTM'.

    as_doc_dat-obj_name = 'Alerta - Hedge SAP'.
    as_doc_dat-obj_descr = 'Alerta - Hedge SAP'.
    as_doc_dat-no_change = 'X'.

    REPLACE '#01#' IN lv_body WITH |{ is_0094-nro_sol_ov ALPHA = OUT }|.

    WRITE is_0094-total_proporc TO lv_value LEFT-JUSTIFIED.

    REPLACE '#02#' IN lv_body WITH lv_value.

    WRITE lv_dolar TO lv_value LEFT-JUSTIFIED.

    REPLACE '#03#' IN lv_body WITH lv_value.

    IF is_0094-tipo = 'VDI' OR is_0094-tipo = 'FRI'.
      lv_origem = 'Insumos'.
    ELSEIF is_0094-tipo = 'VDA' OR is_0094-tipo = 'FRE' OR is_0094-tipo = ''.
      lv_origem = 'Mercado Interno'.
    ELSEIF is_0094-tipo = 'AQV' OR is_0094-tipo = 'TBP' OR is_0094-tipo = 'TBO'.
      lv_origem = 'Serv Aquavi&aacute;rio'.
    ELSEIF is_0094-tipo = 'PDI'.
      lv_origem = 'Pedido'.
    ENDIF.

    REPLACE '#04#' IN lv_body WITH lv_origem.

    APPEND lv_body TO at_contents_txt.

    "Enviar
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = as_doc_dat
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = at_list
        contents_txt               = at_contents_txt
        receivers                  = at_receivers
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        operation_no_authorization = 4
        OTHERS                     = 99.

    IF sy-subrc NE 0.

    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    IF go_instance IS NOT BOUND.
      go_instance = NEW zcl_hedge_limite( ).
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.


  METHOD mostra_popup.

    DATA lv_answer.

    CLEAR rv_confirmado.

    MESSAGE i016(ds) WITH iv_mensagem.

*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
*        titlebar              = sy-title
*        text_question         = iv_mensagem
*        display_cancel_button = space
*      IMPORTING
*        answer                = lv_answer
*      EXCEPTIONS
*        text_not_found        = 1
*        OTHERS                = 2.
*
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.


*    IF lv_answer = '1'.
      rv_confirmado = abap_true.
*    ELSE.
*
*      MESSAGE s016(ds) WITH 'Hedge não disparado' 'para este lançamento!'.
*      EXIT.
*
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
