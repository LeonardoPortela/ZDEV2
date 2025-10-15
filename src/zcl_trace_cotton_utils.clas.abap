class ZCL_TRACE_COTTON_UTILS definition
  public
  final
  create public .

public section.

  class-methods GET_FARDOS_BLOCO_TRACE_COTTON
    importing
      !I_SAFRA type CHAR4
      !I_FILIAL_ALGODOEIRA type WERKS_D
      !I_BLOCO type LGORT_D optional
      !I_BLOCOS type ZPPS0008_001_T optional
      !I_MATNR type MATNR optional
      !I_CHECK_EMBARQUE_SAP type CHAR01 optional
      !I_RETURN_DADOS_ACTS type CHAR01 optional
    exporting
      !E_MSG_ERROR type STRING
      !E_FARDOS_BLOCO_TRACE_COTTON type ZPPS0007_T .
  class-methods DISPARAR_EMAIL_ALERTA
    importing
      !I_ZPPT0002_T type ZPPT0002_T optional
      !I_ZPPT0002 type ZPPT0002 optional
      !I_TITULO_EMAIL type STRING
      !I_TEXTO_CORPO_EMAIL type STRING
      !I_MSG_ERROR type STRING optional .
  class-methods SET_DADOS_ACTS
    importing
      !I_SAFRA type CHARG_D
    changing
      !C_FARDOS_TRACE type ZPPS0007_T .
  class-methods DISPARAR_EMAIL_ALERTA_GENERICO
    importing
      !I_TITULO_EMAIL type STRING
      !I_TEXTO_CORPO_EMAIL type STRING
      !I_MSG_ERROR type STRING optional
      !I_ULTIMO_EMAIL type CHAR14 optional
    returning
      value(R_EMAIL_ENVIADO) type CHAR01 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TRACE_COTTON_UTILS IMPLEMENTATION.


  METHOD disparar_email_alerta.

    DATA: lva_msg_error               TYPE string,
          lva_data_limite             TYPE char14,
          lva_data_ultimo_email       TYPE char14,
          lva_qtde_tentativas_retorno TYPE i,
          lva_ret_date                TYPE tvpod-rudat,
          lva_ret_time                TYPE tvpod-rutim,
          lva_protocolo               TYPE zppt0002-id_referencia,
          lva_duration_integer        TYPE i.

    DATA: lva_texto_corpo   TYPE string.
    DATA: lit_zsdt0105      TYPE TABLE OF zsdt0105.
    DATA: lit_zsdt0296      TYPE TABLE OF zsdt0296.
    DATA: lit_zmail         TYPE TABLE OF zmail.
    DATA: it_html           TYPE TABLE OF w3html INITIAL SIZE 0.
    DATA: objpack     TYPE TABLE OF sopcklsti1,
          lwa_objpack TYPE sopcklsti1.

    DATA: objhead     TYPE TABLE OF solisti1.
    DATA: objbin_ord  TYPE TABLE OF solisti1.
    DATA: objbin_log  TYPE TABLE OF solisti1.
    DATA: objbin_ann  TYPE solisti1.
    DATA: objbin      TYPE TABLE OF solisti1.
    DATA: wa_objbin   TYPE solisti1.
    DATA: content_hex TYPE STANDARD TABLE OF solix.
    DATA: objtxt      TYPE TABLE OF solisti1.
    DATA: reclist     TYPE TABLE OF somlreci1.
    DATA: lwa_reclist TYPE somlreci1.
    DATA: doc_chng    TYPE sodocchgi1.
    DATA: tab_lines   TYPE sy-tabix.
    DATA: lit_zppt0002_proc TYPE zppt0002_t.

    DATA: lv_valor TYPE string.

    DATA: lv_title_email TYPE string.

    DATA: vl_nmdfe    TYPE string,
          vl_docnum   TYPE string,
          vl_filial   TYPE string,
          vl_data_aut TYPE string,
          vl_msg_ret  TYPE string.

    DEFINE conc_html.
      lv_valor = &1.

      CALL FUNCTION 'ZHTML_ADD'
        EXPORTING
          i_texto = lv_valor
        TABLES
          it_html = it_html.
    END-OF-DEFINITION.

    IF i_zppt0002_t IS NOT INITIAL.
      lit_zppt0002_proc = i_zppt0002_t.
    ELSEIF i_zppt0002 IS NOT INITIAL.
      APPEND i_zppt0002 TO lit_zppt0002_proc.
    ELSE.
      EXIT.
    ENDIF.

    DATA(lva_envia_email_alerta) = abap_false.

    SORT lit_zppt0002_proc BY dt_email_alerta hr_email_alerta.

    LOOP AT lit_zppt0002_proc ASSIGNING FIELD-SYMBOL(<fs_zppt0002>).
      lva_data_ultimo_email       = |{ <fs_zppt0002>-dt_email_alerta }{ <fs_zppt0002>-hr_email_alerta }|.
    ENDLOOP.

    "Verificar regras para disparo email
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarvc)
     WHERE name = 'TRACE_COTTON_TIME_EMAIL_ALERTA'.

    IF sy-subrc EQ 0 AND lwa_tvarvc-low IS NOT INITIAL.
      lva_duration_integer = lwa_tvarvc-low.
    ELSE.
      lva_duration_integer = 900. "Padrao 15 minutos
    ENDIF.

    CALL FUNCTION 'TSTR_CALC_TIME'
      EXPORTING
        iv_begin_datelocal_req   = sy-datum
        iv_begin_timelocal_req   = sy-uzeit
        iv_duration_integer      = lva_duration_integer
        iv_direction             = '-'
      IMPORTING
        ev_end_datelocal         = lva_ret_date
        ev_end_timelocal         = lva_ret_time
      EXCEPTIONS
        fatal_error              = 1
        time_invalid             = 2
        time_missing             = 3
        tstream_not_loadable     = 4
        tstream_generation_error = 5
        parameter_error          = 6
        unspecified_error        = 7
        OTHERS                   = 8.

    lva_data_limite = |{ lva_ret_date }{ lva_ret_time }|.

    IF  lva_data_ultimo_email < lva_data_limite OR lva_data_ultimo_email IS INITIAL.
      lva_envia_email_alerta = abap_true.
    ELSE.
      lva_envia_email_alerta = abap_false.
    ENDIF.

    CHECK lva_envia_email_alerta EQ abap_true.


*---------------------------------------------------------------------------------------------------------------------*
*   Envio Email Alerta
*---------------------------------------------------------------------------------------------------------------------*

    "Verificar emails para disparo alerta
    SELECT *
      FROM tvarvc INTO TABLE @DATA(lit_tvarvc_emails_alerta)
     WHERE name = 'TRACE_COTTON_EMAILS_ALERTA'.

    CLEAR: reclist[].

    "Determinação Destinatarios
    IF lit_tvarvc_emails_alerta[] IS NOT INITIAL.

      LOOP AT lit_tvarvc_emails_alerta INTO DATA(lwa_email).
        lwa_reclist-receiver = lwa_email-low.
        lwa_reclist-rec_type = 'U'.

        TRANSLATE lwa_reclist-receiver TO LOWER CASE.
        APPEND lwa_reclist TO reclist.
      ENDLOOP.

    ELSE.

      CASE sy-sysid.
        WHEN 'PRD'.

          lwa_reclist-receiver = 'suporte.sap@amaggi.com.br'.
          lwa_reclist-rec_type = 'U'.
          APPEND lwa_reclist TO reclist.

        WHEN OTHERS.

          lwa_reclist-receiver = 'wellington.pereira@amaggi.com.br'.
          lwa_reclist-rec_type = 'U'.
          APPEND lwa_reclist TO reclist.

          lwa_reclist-receiver = 'fabio.sa@amaggi.com.br'.
          lwa_reclist-rec_type = 'U'.
          APPEND lwa_reclist TO reclist.
      ENDCASE.

    ENDIF.

    CHECK reclist[] IS NOT INITIAL.

    "lv_title_email = |SAP { sy-sysid } - RETORNO PROCESSAMENTO BENEFICIAMENTO - TRACE COTTON|.
    lv_title_email = i_titulo_email.

    "lva_texto_corpo = |Houve falha de retorno no processamento do Beneficiamento para o sistema Trace Cotton! Verificar Log Transação ZWS0004 - Id Interface: 034 e Id. Referencia: { lva_protocolo } |.
    lva_texto_corpo = i_texto_corpo_email.

    CONCATENATE lva_texto_corpo '- Data:' sy-datum '- Hora: ' sy-uzeit INTO lva_texto_corpo SEPARATED BY space.

    "Monta Corpo Email
    conc_html '<html>'.
    conc_html '<head><title>'.
    conc_html    lv_title_email.
    conc_html '</title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
    conc_html '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
    conc_html '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
    conc_html    lva_texto_corpo.
    conc_html '</STRONG></FONT></DIV><BR>'.
    conc_html '<FONT face=Verdana color=#0000ff size=2>'.
    conc_html '<BR>'.

    conc_html '<DIV align=center><FONT face=Verdana color=#ff0000 size=3><STRONG>'.
    conc_html    i_msg_error.
    conc_html '</STRONG></FONT></DIV>'.

    conc_html '<BR>'.
    conc_html '<BR>'.
    conc_html '<DIV align=left>'.

    conc_html '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>'.

    conc_html '</DIV>'.
    conc_html '<BR>'.
    conc_html '</body>'.
    conc_html '</html>'.

    "Corpo
    doc_chng-obj_name  = lv_title_email.
    doc_chng-obj_descr = lv_title_email.
    doc_chng-no_change = 'X'.

    CLEAR lwa_objpack-transf_bin.
    lwa_objpack-head_start = 1.
    lwa_objpack-head_num   = 0.
    lwa_objpack-body_start = 1.
    lwa_objpack-body_num   = 99999.
    lwa_objpack-doc_type   = 'HTM'.
    APPEND lwa_objpack TO objpack.

    "Enviar
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = objpack
        contents_txt               = it_html
        receivers                  = reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        operation_no_authorization = 4
        OTHERS                     = 99.

    LOOP AT lit_zppt0002_proc ASSIGNING <fs_zppt0002>.
      UPDATE zppt0002 SET dt_email_alerta = sy-datum
                          hr_email_alerta = sy-uzeit
        WHERE acharg     EQ <fs_zppt0002>-acharg
          AND werks      EQ <fs_zppt0002>-werks
          AND id_sessao  EQ <fs_zppt0002>-id_sessao
          AND lgort      EQ <fs_zppt0002>-lgort
          AND cd_safra   EQ <fs_zppt0002>-cd_safra.

      COMMIT WORK.
    ENDLOOP.

  ENDMETHOD.


  METHOD disparar_email_alerta_generico.

    DATA: lva_msg_error               TYPE string,
          lva_data_limite             TYPE char14,
          lva_data_ultimo_email       TYPE char14,
          lva_qtde_tentativas_retorno TYPE i,
          lva_ret_date                TYPE tvpod-rudat,
          lva_ret_time                TYPE tvpod-rutim,
          lva_protocolo               TYPE zppt0002-id_referencia,
          lva_duration_integer        TYPE i.

    DATA: lva_texto_corpo   TYPE string.
    DATA: lit_zsdt0105      TYPE TABLE OF zsdt0105.
    DATA: lit_zsdt0296      TYPE TABLE OF zsdt0296.
    DATA: lit_zmail         TYPE TABLE OF zmail.
    DATA: it_html           TYPE TABLE OF w3html INITIAL SIZE 0.
    DATA: objpack     TYPE TABLE OF sopcklsti1,
          lwa_objpack TYPE sopcklsti1.

    DATA: objhead     TYPE TABLE OF solisti1.
    DATA: objbin_ord  TYPE TABLE OF solisti1.
    DATA: objbin_log  TYPE TABLE OF solisti1.
    DATA: objbin_ann  TYPE solisti1.
    DATA: objbin      TYPE TABLE OF solisti1.
    DATA: wa_objbin   TYPE solisti1.
    DATA: content_hex TYPE STANDARD TABLE OF solix.
    DATA: objtxt      TYPE TABLE OF solisti1.
    DATA: reclist     TYPE TABLE OF somlreci1.
    DATA: lwa_reclist TYPE somlreci1.
    DATA: doc_chng    TYPE sodocchgi1.
    DATA: tab_lines   TYPE sy-tabix.
    DATA: lit_zppt0002_proc TYPE zppt0002_t.

    DATA: lv_valor TYPE string.

    DATA: lv_title_email TYPE string.

    DATA: vl_nmdfe    TYPE string,
          vl_docnum   TYPE string,
          vl_filial   TYPE string,
          vl_data_aut TYPE string,
          vl_msg_ret  TYPE string.

    DEFINE conc_html.
      lv_valor = &1.

      CALL FUNCTION 'ZHTML_ADD'
        EXPORTING
          i_texto = lv_valor
        TABLES
          it_html = it_html.
    END-OF-DEFINITION.


    DATA(lva_envia_email_alerta) = abap_false.

    CLEAR: r_email_enviado.

    lva_data_ultimo_email = i_ultimo_email.


    "Verificar regras para disparo email
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarvc)
     WHERE name = 'TRACE_COTTON_TIME_EMAIL_ALERTA'.

    IF sy-subrc EQ 0 AND lwa_tvarvc-low IS NOT INITIAL.
      lva_duration_integer = lwa_tvarvc-low.
    ELSE.
      lva_duration_integer = 900. "Padrao 15 minutos
    ENDIF.

    CALL FUNCTION 'TSTR_CALC_TIME'
      EXPORTING
        iv_begin_datelocal_req   = sy-datum
        iv_begin_timelocal_req   = sy-uzeit
        iv_duration_integer      = lva_duration_integer
        iv_direction             = '-'
      IMPORTING
        ev_end_datelocal         = lva_ret_date
        ev_end_timelocal         = lva_ret_time
      EXCEPTIONS
        fatal_error              = 1
        time_invalid             = 2
        time_missing             = 3
        tstream_not_loadable     = 4
        tstream_generation_error = 5
        parameter_error          = 6
        unspecified_error        = 7
        OTHERS                   = 8.

    lva_data_limite = |{ lva_ret_date }{ lva_ret_time }|.

    IF  lva_data_ultimo_email < lva_data_limite OR lva_data_ultimo_email IS INITIAL.
      lva_envia_email_alerta = abap_true.
    ELSE.
      lva_envia_email_alerta = abap_false.
    ENDIF.

    CHECK lva_envia_email_alerta EQ abap_true.


*---------------------------------------------------------------------------------------------------------------------*
*   Envio Email Alerta
*---------------------------------------------------------------------------------------------------------------------*

    "Verificar emails para disparo alerta
    SELECT *
      FROM tvarvc INTO TABLE @DATA(lit_tvarvc_emails_alerta)
     WHERE name = 'TRACE_COTTON_EMAILS_ALERTA'.

    CLEAR: reclist[].

    "Determinação Destinatarios
    IF lit_tvarvc_emails_alerta[] IS NOT INITIAL.

      LOOP AT lit_tvarvc_emails_alerta INTO DATA(lwa_email).
        lwa_reclist-receiver = lwa_email-low.
        lwa_reclist-rec_type = 'U'.

        TRANSLATE lwa_reclist-receiver TO LOWER CASE.
        APPEND lwa_reclist TO reclist.
      ENDLOOP.

    ELSE.

      CASE sy-sysid.
        WHEN 'PRD'.

          lwa_reclist-receiver = 'suporte.sap@amaggi.com.br'.
          lwa_reclist-rec_type = 'U'.
          APPEND lwa_reclist TO reclist.

        WHEN OTHERS.

          lwa_reclist-receiver = 'wellington.pereira@amaggi.com.br'.
          lwa_reclist-rec_type = 'U'.
          APPEND lwa_reclist TO reclist.

          lwa_reclist-receiver = 'fabio.sa@amaggi.com.br'.
          lwa_reclist-rec_type = 'U'.
          APPEND lwa_reclist TO reclist.
      ENDCASE.

    ENDIF.

    CHECK reclist[] IS NOT INITIAL.

    lv_title_email  = i_titulo_email.
    lva_texto_corpo = i_texto_corpo_email.

    CONCATENATE lva_texto_corpo '- Data:' sy-datum '- Hora: ' sy-uzeit INTO lva_texto_corpo SEPARATED BY space.

    "Monta Corpo Email
    conc_html '<html>'.
    conc_html '<head><title>'.
    conc_html    lv_title_email.
    conc_html '</title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
    conc_html '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
    conc_html '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
    conc_html    lva_texto_corpo.
    conc_html '</STRONG></FONT></DIV><BR>'.
    conc_html '<FONT face=Verdana color=#0000ff size=2>'.
    conc_html '<BR>'.

    conc_html '<DIV align=center><FONT face=Verdana color=#ff0000 size=3><STRONG>'.
    conc_html    i_msg_error.
    conc_html '</STRONG></FONT></DIV>'.

    conc_html '<BR>'.
    conc_html '<BR>'.
    conc_html '<DIV align=left>'.

    conc_html '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>'.

    conc_html '</DIV>'.
    conc_html '<BR>'.
    conc_html '</body>'.
    conc_html '</html>'.

    "Corpo
    doc_chng-obj_name  = lv_title_email.
    doc_chng-obj_descr = lv_title_email.
    doc_chng-no_change = 'X'.

    CLEAR lwa_objpack-transf_bin.
    lwa_objpack-head_start = 1.
    lwa_objpack-head_num   = 0.
    lwa_objpack-body_start = 1.
    lwa_objpack-body_num   = 99999.
    lwa_objpack-doc_type   = 'HTM'.
    APPEND lwa_objpack TO objpack.

    "Enviar
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = objpack
        contents_txt               = it_html
        receivers                  = reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        operation_no_authorization = 4
        OTHERS                     = 99.

    r_email_enviado = abap_true.

  ENDMETHOD.


  METHOD get_fardos_bloco_trace_cotton.

   DATA: lwa_consulta_fardos   TYPE zpps0008,
         lit_zmmt0008          TYPE TABLE OF zmmt0008.

    CLEAR: e_msg_error, e_fardos_bloco_trace_cotton, lwa_consulta_fardos.

    IF i_safra IS INITIAL.
      e_msg_error = |Safra não foi informada! |.
      RETURN.
    ENDIF.

    IF i_filial_algodoeira IS INITIAL.
      e_msg_error = |Filial Algodoeira não foi informada! |.
      RETURN.
    ENDIF.

    IF i_bloco IS INITIAL AND i_blocos IS INITIAL.
      e_msg_error = |Bloco não foi informado! |.
      RETURN.
    ENDIF.

    IF i_matnr IS NOT INITIAL.
      SELECT SINGLE *
        FROM mara INTO @DATA(lwa_mara_check)
       WHERE matnr EQ @i_matnr.

      IF sy-subrc NE 0.
        e_msg_error = |Cadastro Material: { i_matnr } não encontrado! |.
        RETURN.
      ENDIF.

      IF lwa_mara_check-mtart NE 'ZFER'.
        e_msg_error = |Material { i_matnr } não é do tipo ZFER! |.
        RETURN.
      ENDIF.

      IF lwa_mara_check-normt IS INITIAL.
        e_msg_error = |Denominação/Classificação Material { i_matnr } não informada no cadastro! |.
        RETURN.
      ENDIF.
    ENDIF.

    TRY.
      lwa_consulta_fardos-filial_algodoeira  = i_filial_algodoeira.
      lwa_consulta_fardos-safra              = i_safra.
      lwa_consulta_fardos-check_embarque_sap = i_check_embarque_sap.
      lwa_consulta_fardos-bloco              = i_bloco.
      lwa_consulta_fardos-blocos             = i_blocos.

      zcl_int_ob_cons_fardo_trace_ct=>zif_integracao_outbound~get_instance(
      )->execute_request( EXPORTING i_info_request = lwa_consulta_fardos
                          IMPORTING e_integracao = DATA(lwa_integracao) ).

      IF lwa_integracao-ds_data_retorno IS NOT INITIAL.
        /ui2/cl_json=>deserialize( EXPORTING json = lwa_integracao-ds_data_retorno CHANGING data = e_fardos_bloco_trace_cotton ).
      ENDIF.

      SORT e_fardos_bloco_trace_cotton by nr_fardo_completo.

      "Verificar se Fardo já foi embarcado no SAP
      IF i_check_embarque_sap EQ abap_true AND e_fardos_bloco_trace_cotton[] IS NOT INITIAL.
        CLEAR: lit_zmmt0008[].

        SELECT *
          FROM zmmt0008 INTO TABLE lit_zmmt0008
           FOR ALL ENTRIES IN e_fardos_bloco_trace_cotton
         WHERE werks  = e_fardos_bloco_trace_cotton-id_filial_algodoeira(4)
           AND lgort  = e_fardos_bloco_trace_cotton-bloco(4)
           AND charg  = e_fardos_bloco_trace_cotton-nr_fardo_completo(10)
           AND safra  = e_fardos_bloco_trace_cotton-safra(10).

        LOOP AT e_fardos_bloco_trace_cotton ASSIGNING FIELD-SYMBOL(<fs_fardo_trace>).
          READ TABLE lit_zmmt0008 INTO DATA(lwa_zmmt0008) WITH KEY werks = <fs_fardo_trace>-id_filial_algodoeira
                                                                   lgort = <fs_fardo_trace>-bloco
                                                                   charg = <fs_fardo_trace>-nr_fardo_completo
                                                                   safra = <fs_fardo_trace>-safra.

          IF sy-subrc EQ 0.
            <fs_fardo_trace>-embarcado_sap = abap_true.
          ENDIF.
        ENDLOOP.
      ENDIF.

    CATCH zcx_integracao INTO DATA(zcx_integracao).

      MESSAGE ID zcx_integracao->msgid TYPE 'I'
       NUMBER zcx_integracao->msgno
         WITH zcx_integracao->msgv1
              zcx_integracao->msgv2
              zcx_integracao->msgv3
              zcx_integracao->msgv4 INTO e_msg_error.
      e_msg_error = |Comunicação Trace Cotton: { e_msg_error } |.
      RETURN.

    CATCH zcx_error INTO DATA(zcx_error).

      MESSAGE ID zcx_error->msgid TYPE 'I'
       NUMBER zcx_error->msgno
         WITH zcx_error->msgv1
              zcx_error->msgv2
              zcx_error->msgv3
              zcx_error->msgv4 INTO e_msg_error .
      e_msg_error = |Comunicação Trace Cotton: { e_msg_error } |.
      RETURN.

    ENDTRY.

    CHECK e_msg_error IS INITIAL.

    IF i_matnr IS NOT INITIAL.
      LOOP AT e_fardos_bloco_trace_cotton INTO DATA(lwa_fardo_trace) WHERE cd_classificacao NE lwa_mara_check-normt.
        e_msg_error = |Bloco { i_bloco } contem fardos do tipo { lwa_fardo_trace-cd_classificacao } no Trace Cotton! Tipo Material Informado no SAP: { lwa_mara_check-normt }  !|.
        RETURN.
      ENDLOOP.
    ENDIF.

    if i_return_dados_acts eq abap_true.
      "Preenchimento Dados e Validações ACTS
      zcl_trace_cotton_utils=>set_dados_acts( EXPORTING i_safra        = CONV #( i_safra )
                                              CHANGING  c_fardos_trace = e_fardos_bloco_trace_cotton ).
    endif.


  ENDMETHOD.


  method SET_DADOS_ACTS.

    DATA: lva_valida_acts TYPE char1.

    SELECT SINGLE *
      FROM setleaf INTO @DATA(lwa_set_valida_acts)
     WHERE setname EQ 'MAGGI_ACTS_ZMM0023'.

    IF i_safra GE lwa_set_valida_acts-valfrom.
      lva_valida_acts = abap_true.
    ENDIF.

    LOOP AT c_fardos_trace ASSIGNING FIELD-SYMBOL(<fs_fardo_trace>).

      CASE lva_valida_acts.
        WHEN abap_true.

          CASE <fs_fardo_trace>-status_takeup_lote_recente.
            WHEN 'Aprovado'.

              CASE <fs_fardo_trace>-possui_acts.
                WHEN abap_false.

                  IF <fs_fardo_trace>-takeup_marcado_acts = abap_true.

                    <fs_fardo_trace>-validacao_acts-icon        = icon_led_red.
                    <fs_fardo_trace>-validacao_acts-status      = <fs_fardo_trace>-status_takeup_lote_recente.
                    <fs_fardo_trace>-validacao_acts-possuiacts  = <fs_fardo_trace>-possui_acts.

                  ELSE.

                    <fs_fardo_trace>-validacao_acts-icon        = icon_led_green.
                    <fs_fardo_trace>-validacao_acts-status      = <fs_fardo_trace>-status_takeup_lote_recente.
                    <fs_fardo_trace>-validacao_acts-possuiacts  = <fs_fardo_trace>-possui_acts.

                  ENDIF.

                WHEN abap_true.

                  <fs_fardo_trace>-validacao_acts-icon       = icon_led_green.
                  <fs_fardo_trace>-validacao_acts-status     = <fs_fardo_trace>-status_takeup_lote_recente.
                  <fs_fardo_trace>-validacao_acts-possuiacts = <fs_fardo_trace>-possui_acts.

              ENDCASE.

            WHEN OTHERS.

              <fs_fardo_trace>-validacao_acts-icon       = icon_led_red.
              <fs_fardo_trace>-validacao_acts-status     = <fs_fardo_trace>-status_takeup_lote_recente.
              <fs_fardo_trace>-validacao_acts-possuiacts = <fs_fardo_trace>-possui_acts.

          ENDCASE. "CASE <fs_fardo_trace>-status_takeup_lote_recente


        WHEN abap_false.

          <fs_fardo_trace>-validacao_acts-icon        = icon_led_green.
          <fs_fardo_trace>-validacao_acts-status      = <fs_fardo_trace>-status_takeup_lote_recente.
          <fs_fardo_trace>-validacao_acts-possuiacts  = abap_false.

      ENDCASE. " CASE lva_valida_acts.

    ENDLOOP.

  endmethod.
ENDCLASS.
