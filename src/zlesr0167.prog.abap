*&---------------------------------------------------------------------*
*& Report ZLESR0167
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlesr0167.

TABLES: zmmt0201, zsdt0133.

*----------------------------------------------------------------------*
* Tela de seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_par_01 TYPE c AS CHECKBOX. "Carga Entrada Insumos - Transação ZSDT0112
  SELECT-OPTIONS: s_viag01 FOR zmmt0201-viagem_id.
  PARAMETERS: p_dtpc01 TYPE erdat,
              p_dias01 TYPE numc3 DEFAULT 10.  "*-US190812-16.09.2025-#190812-JT-inicio
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Tela de seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_par_02 TYPE c AS CHECKBOX. "Carga Saida Insumos - Transação ZSDT0112
  SELECT-OPTIONS: s_viag02 FOR zmmt0201-viagem_id.
  PARAMETERS: p_dtpc02 TYPE erdat,
              p_dias02 TYPE numc3 DEFAULT 10.  "*-US190812-16.09.2025-#190812-JT-inicio
*SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Tela de seleção                                                      *
*----------------------------------------------------------------------*
  PARAMETERS: p_par_03 TYPE c AS CHECKBOX. "Carga Saida Insumos - Transação ZSDT0112
  SELECT-OPTIONS: s_nrocg  FOR zsdt0133-nro_cg.
  PARAMETERS: p_dtpc03 TYPE erdat,             "*-US190812-16.09.2025-#190812-JT-inicio
              p_dias03 TYPE numc3 DEFAULT 10.  "*-US190812-16.09.2025-#190812-JT-inicio
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF sy-batch EQ abap_true.
    TRY.
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.

    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.

    ENDIF.

  ENDIF.

  IF p_par_01 EQ abap_true. "Carga Entrada Insumos - Transação ZSDT0112
    PERFORM: f_consultar_processo_01.
  ENDIF.

  IF p_par_02 EQ abap_true. "Carga Saida Insumos - Transação ZSDT0112
    PERFORM: f_consultar_processo_02.
  ENDIF.

  IF p_par_03 EQ abap_true. "Carga Saida Insumos - Transação ZSDT0112
    PERFORM: f_consultar_processo_03.  "*-US190812-16.09.2025-#190812-JT-inicio
  ENDIF.

FORM f_consultar_processo_01 .

  DATA: lwa_zlest185          TYPE zlest0185,
        lwa_response_consulta TYPE zlese0274.

  RANGES: lra_date_proc FOR zmmt0201-date_create.
  DATA: lva_data_base TYPE erdat.

  IF s_viag01[] IS INITIAL. "Se não informou ID viagem, consultar por data
    APPEND INITIAL LINE TO lra_date_proc ASSIGNING FIELD-SYMBOL(<fs_date_proc>).

    IF p_dtpc01 IS NOT INITIAL.
      <fs_date_proc>-sign   = 'I'.
      <fs_date_proc>-option = 'EQ'.
      <fs_date_proc>-low    = p_dtpc01.
    ELSE.
*     lva_data_base = sy-datum - 10.        "*-US190812-16.09.2025-#190812-JT-inicio
      lva_data_base = sy-datum - p_dias01.  "*-US190812-16.09.2025-#190812-JT-inicio
      <fs_date_proc>-sign   = 'I'.
      <fs_date_proc>-option = 'GE'.
      <fs_date_proc>-low    = lva_data_base.
    ENDIF.
  ENDIF.

  SELECT *
    FROM zmmt0201 INTO TABLE @DATA(lit_zmmt0201)
   WHERE dt_autorizacao_embarque IN @lra_date_proc
     AND viagem_id               IN @s_viag01.

  DELETE lit_zmmt0201 WHERE viagem_id IS INITIAL OR cancel EQ abap_true OR dt_autorizacao_embarque IS INITIAL.

  DELETE lit_zmmt0201 WHERE viagem_aprovada_carguero EQ abap_true.

  CHECK lit_zmmt0201[] IS NOT INITIAL.

  LOOP AT lit_zmmt0201 INTO DATA(lwa_zmmt0201).
    CLEAR: lwa_zlest185.

    lwa_zlest185-viagem_id = lwa_zmmt0201-viagem_id.

    TRY.
        zcl_int_ob_get_viagem_carguero=>zif_integracao_outbound~get_instance(
                           )->execute_request( EXPORTING i_info_request = lwa_zlest185
                                               IMPORTING e_integracao   = DATA(lwa_zintegracao) ).

        CHECK lwa_zintegracao-ds_data_retorno IS NOT INITIAL.


        /ui2/cl_json=>deserialize( EXPORTING json = lwa_zintegracao-ds_data_retorno CHANGING data = lwa_response_consulta ).

        PERFORM f_processa_retorno_01 USING lwa_zmmt0201 lwa_response_consulta.


      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao). " Classe de Erro de Integração
        CONTINUE.
      CATCH zcx_error INTO DATA(lwa_zcx_error). " Classe de Erro de Integração
        CONTINUE.
    ENDTRY.

  ENDLOOP.


ENDFORM.

FORM f_consultar_processo_02 .


  DATA: lwa_zlest185          TYPE zlest0185,
        lwa_response_consulta TYPE zlese0274.

  RANGES: lra_date_proc FOR zmmt0201-date_create.
  DATA: lva_data_base TYPE erdat.

  IF s_viag02[] IS INITIAL. "Se não informou ID viagem, consultar por data
    APPEND INITIAL LINE TO lra_date_proc ASSIGNING FIELD-SYMBOL(<fs_date_proc>).

    IF p_dtpc02 IS NOT INITIAL.
      <fs_date_proc>-sign   = 'I'.
      <fs_date_proc>-option = 'EQ'.
      <fs_date_proc>-low    = p_dtpc02.
    ELSE.
*     lva_data_base = sy-datum - 10.        "*-US190812-16.09.2025-#190812-JT-inicio
      lva_data_base = sy-datum - p_dias02.  "*-US190812-16.09.2025-#190812-JT-inicio
      <fs_date_proc>-sign   = 'I'.
      <fs_date_proc>-option = 'GE'.
      <fs_date_proc>-low    = lva_data_base.
    ENDIF.
  ENDIF.

  SELECT *
    FROM zsdt0133 INTO TABLE @DATA(lit_zsdt0133)
   WHERE dt_autorizacao_embarque IN @lra_date_proc
     AND viagem_id               IN @s_viag02.

  DELETE lit_zsdt0133 WHERE viagem_id IS INITIAL OR dt_canc IS NOT INITIAL OR dt_autorizacao_embarque IS INITIAL.

  DELETE lit_zsdt0133 WHERE viagem_aprovada_carguero EQ abap_true.

  CHECK lit_zsdt0133[] IS NOT INITIAL.

  LOOP AT lit_zsdt0133 INTO DATA(lwa_zsdt0133).
    CLEAR: lwa_zlest185.

    lwa_zlest185-viagem_id = lwa_zsdt0133-viagem_id.

    TRY.
        zcl_int_ob_get_viagem_carguero=>zif_integracao_outbound~get_instance(
                           )->execute_request( EXPORTING i_info_request = lwa_zlest185
                                               IMPORTING e_integracao   = DATA(lwa_zintegracao) ).

        CHECK lwa_zintegracao-ds_data_retorno IS NOT INITIAL.


        /ui2/cl_json=>deserialize( EXPORTING json = lwa_zintegracao-ds_data_retorno CHANGING data = lwa_response_consulta ).

        PERFORM f_processa_retorno_02 USING lwa_zsdt0133 lwa_response_consulta.


      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao). " Classe de Erro de Integração
        CONTINUE.
      CATCH zcx_error INTO DATA(lwa_zcx_error). " Classe de Erro de Integração
        CONTINUE.
    ENDTRY.

  ENDLOOP.


ENDFORM.

*-US190812-16.09.2025-#190812-JT-inicio
FORM f_consultar_processo_03 .

  DATA: lwa_zlest181          TYPE zlest0181,
        lwa_response_consulta TYPE zlese0275.

  RANGES: lra_date_proc FOR zmmt0201-date_create.
  DATA: lva_data_base TYPE erdat.

  IF s_nrocg[] IS INITIAL. "Se não informou ID viagem, consultar por data
    APPEND INITIAL LINE TO lra_date_proc ASSIGNING FIELD-SYMBOL(<fs_date_proc>).

    IF p_dtpc03 IS NOT INITIAL.
      <fs_date_proc>-sign   = 'I'.
      <fs_date_proc>-option = 'EQ'.
      <fs_date_proc>-low    = p_dtpc03.
    ELSE.
*     lva_data_base = sy-datum - 10.        "*-US190812-16.09.2025-#190812-JT-inicio
      lva_data_base = sy-datum - p_dias03.  "*-US190812-16.09.2025-#190812-JT-inicio
      <fs_date_proc>-sign   = 'I'.
      <fs_date_proc>-option = 'GE'.
      <fs_date_proc>-low    = lva_data_base.
    ENDIF.
  ENDIF.

  SELECT zsdt0133~*
    FROM zsdt0133
    INNER JOIN zi_sd_st_cg_sai_insumos ON zi_sd_st_cg_sai_insumos~nro_cg  = zsdt0133~nro_cg
                                      AND zi_sd_st_cg_sai_insumos~status IN ('1','2','3')
    INNER JOIN zlest0181               ON zlest0181~nro_cg_sai_in         = zsdt0133~nro_cg
                                      AND id_carguero                    IS NOT INITIAL
   WHERE zsdt0133~data_atual   IN @lra_date_proc
     AND zsdt0133~nro_cg       IN @s_nrocg
    INTO TABLE @DATA(lit_zsdt0133).

  CHECK sy-subrc = 0.

  DELETE lit_zsdt0133 WHERE integrar_carguero        = abap_off
                         OR dt_frete_contratado     IS NOT INITIAL
                         OR dt_autorizacao_embarque IS NOT INITIAL.

  CHECK lit_zsdt0133[] IS NOT INITIAL.

  LOOP AT lit_zsdt0133 INTO DATA(lwa_zsdt0133).
    CLEAR: lwa_zlest181.

    lwa_zlest181-nro_cg_sai_in = lwa_zsdt0133-nro_cg.

    TRY.
        zcl_int_ob_get_frete_carguero=>zif_integracao_outbound~get_instance(
                                    )->execute_request( EXPORTING i_info_request = lwa_zlest181
                                                        IMPORTING e_integracao   = DATA(lwa_zintegracao) ).

        CHECK lwa_zintegracao-ds_data_retorno IS NOT INITIAL.

        /ui2/cl_json=>deserialize( EXPORTING json = lwa_zintegracao-ds_data_retorno CHANGING data = lwa_response_consulta ).

        PERFORM f_processa_retorno_03 USING lwa_zsdt0133 lwa_response_consulta.

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao). " Classe de Erro de Integração
        CONTINUE.
      CATCH zcx_error INTO DATA(lwa_zcx_error). " Classe de Erro de Integração
        CONTINUE.
    ENDTRY.

  ENDLOOP.


ENDFORM.
*-US190812-16.09.2025-#190812-JT-fim

FORM f_get_xstring_of_url USING p_url TYPE string CHANGING c_xstring TYPE zde_data_xstring.


  DATA : l_url       TYPE string,
         l_content   TYPE xstring,
         http_client TYPE REF TO if_http_client.

  FREE: c_xstring.

  l_url =  p_url.

  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = l_url
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.

  CHECK sy-subrc = 0.

  http_client->send( ).
  http_client->receive( ).
  l_content  = http_client->response->get_data( ).
  http_client->close( ).
  c_xstring   = l_content.

ENDFORM.

FORM f_processa_retorno_01 USING p_zmmt0201          TYPE zmmt0201
                                 p_response_consulta TYPE zlese0274.

  DATA: lt_arquivos    TYPE zmmt_arquivos,
        e_file_xstring TYPE xstring.

  DATA: lit_zmmt0203 TYPE TABLE OF zmmt0203.

  CHECK p_zmmt0201-nro_cg IS NOT INITIAL AND p_response_consulta-trip_id IS NOT INITIAL.

  DATA(_lib_aprova_viagem_carguero) = abap_false.
  DATA(_spart) = zcl_carga_entrada_insumos=>get_spart_carga( i_nro_carga = p_zmmt0201-nro_cg ).


*-------------------------------------------------------------------------------------------------------------------------*
* Validar Exigencia Documento Fase
*-------------------------------------------------------------------------------------------------------------------------*
  DATA(_exige_doc_fase) = abap_false.

  CASE _spart.
    WHEN '02'. "Fertilizantes
      _exige_doc_fase = abap_false.
    WHEN '03'. "Defensivos
      _exige_doc_fase = abap_false.
    WHEN '04'. "Sementes

      SELECT SINGLE *
        FROM lfa1 INTO @DATA(lwa_lfa1_pc)
       WHERE lifnr EQ @p_zmmt0201-ponto_coleta.

      CHECK sy-subrc EQ 0.

      SELECT SINGLE *
       FROM lfa1 INTO @DATA(lwa_lfa1_lr)
      WHERE lifnr EQ @p_zmmt0201-local_entrega.

      CHECK sy-subrc EQ 0.

      IF ( lwa_lfa1_pc-regio EQ 'MT' OR
           lwa_lfa1_lr-regio EQ 'MT' ). "Exige fase se Origem ou Destino for MT
        _exige_doc_fase = abap_true.
      ENDIF.

  ENDCASE.


*-------------------------------------------------------------------------------------------------------------------------*
* Baixar documentos que não foram baixados
*-------------------------------------------------------------------------------------------------------------------------*
  IF p_zmmt0201-doc_outros_anexado = abap_true.
    DELETE p_response_consulta-documents-additional WHERE type = '10'.
  ENDIF.

  IF p_zmmt0201-doc_bordero_anexado = abap_true.
    DELETE p_response_consulta-documents-additional WHERE type = '20'.
  ENDIF.

  IF p_zmmt0201-doc_fase_anexado = abap_true OR _exige_doc_fase EQ abap_false.
    DELETE p_response_consulta-documents-additional WHERE type = '21'.
  ENDIF.

  IF p_zmmt0201-doc_certificado_anexado = abap_true.
    DELETE p_response_consulta-documents-additional WHERE type = '22'.
  ENDIF.

  LOOP AT p_response_consulta-documents-additional INTO DATA(lwa_document_viagem).
    PERFORM f_anexa_doc_carga_entrada_in  USING lwa_document_viagem-url
                                                lwa_document_viagem-type
                                       CHANGING p_zmmt0201.
  ENDLOOP.


  "Download NF-e
  IF p_zmmt0201-doc_nfe_anexado IS INITIAL AND p_response_consulta-documents-fiscal[] IS NOT INITIAL. .

    DATA(_baixar_nfe) = abap_false.

    CASE _spart.
      WHEN '02'. "Fertilizantes
        _baixar_nfe = abap_true.
      WHEN '03'. "Defensivos
        _baixar_nfe = abap_true.
      WHEN '04'. "Sementes

        DATA(_doc_fase_ok) = abap_false.

        IF _exige_doc_fase EQ abap_true.
          IF p_zmmt0201-doc_fase_anexado IS NOT INITIAL.
            _doc_fase_ok = abap_true.
          ENDIF.
        ELSE.
          _doc_fase_ok = abap_true.
        ENDIF.

        IF _doc_fase_ok                       EQ abap_true AND
           p_zmmt0201-doc_certificado_anexado IS NOT INITIAL.
          _baixar_nfe = abap_true.
        ENDIF.
    ENDCASE.


    IF _baixar_nfe EQ abap_true.

      CLEAR: lit_zmmt0203[].

      LOOP AT p_response_consulta-documents-fiscal INTO DATA(lwa_document_fiscal).

        IF lwa_document_fiscal-issue_date IS INITIAL.
          RETURN.
        ENDIF.

        APPEND INITIAL LINE TO lit_zmmt0203 ASSIGNING FIELD-SYMBOL(<fs_zmmt0203>).

        <fs_zmmt0203>-nro_cg      = p_zmmt0201-nro_cg.
        <fs_zmmt0203>-chave_nfe   = lwa_document_fiscal-access_key.
        <fs_zmmt0203>-data_nfe    = lwa_document_fiscal-issue_date+00(04) &&
                                    lwa_document_fiscal-issue_date+05(02) &&
                                    lwa_document_fiscal-issue_date+08(02).
        <fs_zmmt0203>-processo    = 2.
        <fs_zmmt0203>-user_create = sy-uname.
        <fs_zmmt0203>-date_create = sy-datum.
        <fs_zmmt0203>-time_create = sy-uzeit.
      ENDLOOP.

      MODIFY zmmt0203 FROM TABLE lit_zmmt0203.
      p_zmmt0201-doc_nfe_anexado = abap_true.
      UPDATE zmmt0201 SET doc_nfe_anexado = abap_true WHERE nro_cg = p_zmmt0201-nro_cg.
      COMMIT WORK.

    ENDIF.

  ENDIF.

*-------------------------------------------------------------------------------------------------------------------------*
*  Aprovar Liberação Carregamento Carguero
*-------------------------------------------------------------------------------------------------------------------------*

  CASE _spart.
    WHEN '02'. "Fertilizantes
      _lib_aprova_viagem_carguero = abap_true.
    WHEN '03'. "Defensivos
      _lib_aprova_viagem_carguero = abap_true.
    WHEN '04'. "Sementes
      IF p_zmmt0201-doc_nfe_anexado IS NOT INITIAL.
        _lib_aprova_viagem_carguero = abap_true.
      ENDIF.
  ENDCASE.

  IF _lib_aprova_viagem_carguero EQ abap_true AND p_zmmt0201-viagem_aprovada_carguero = abap_false.
    PERFORM f_aprova_viagem_carguero_cg_e USING p_zmmt0201 p_response_consulta.
  ENDIF.

ENDFORM.


FORM f_processa_retorno_02 USING p_zsdt0133          TYPE zsdt0133
                                 p_response_consulta TYPE zlese0274.

  DATA: lt_arquivos    TYPE zmmt_arquivos,
        e_file_xstring TYPE xstring.

  DATA: lit_zsdt0410 TYPE TABLE OF zsdt0410.

  CHECK p_zsdt0133-nro_cg IS NOT INITIAL AND p_response_consulta-trip_id IS NOT INITIAL.

  DATA(_lib_aprova_viagem_carguero) = abap_false.
  DATA(_spart) = zcl_carga_saida_insumos=>get_spart_carga( i_nro_carga = p_zsdt0133-nro_cg ).

  DATA(lra_nro_carga) = VALUE zsdt_range_nro_cg( ( sign = 'I' option = 'EQ' low = p_zsdt0133-nro_cg ) ).

  zcl_carga_saida_insumos=>busca_dados_carga(
    EXPORTING
      i_nr_carga     =  lra_nro_carga
    IMPORTING
      e_cargas       = DATA(lit_carga)
      e_solicitacoes = DATA(lit_solicitacoes)
      e_notas_venda  = DATA(lit_notas_venda)
      e_romaneios    = DATA(lit_romaneios)
      e_notas_transf = DATA(lit_notas_transf) ).

  READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.

  CHECK sy-subrc EQ 0 AND lit_carga[] IS NOT INITIAL.


*-------------------------------------------------------------------------------------------------------------------------*
* Validar Exigencia Documento Fase
*-------------------------------------------------------------------------------------------------------------------------*
  DATA(_exige_doc_fase) = abap_false.

  CASE _spart.
    WHEN '02'. "Fertilizantes
      _exige_doc_fase = abap_false.
    WHEN '03'. "Defensivos
      _exige_doc_fase = abap_false.
    WHEN '04'. "Sementes
      _exige_doc_fase = abap_false.

      DATA(_embarque_armazem) = zcl_carga_saida_insumos=>check_embarque_armazem( i_nro_cg =  p_zsdt0133-nro_cg ).

      IF _embarque_armazem NE abap_true. "Se não estiver embarcando no Armazem, deve verificar Origem e Destino para verificar se exige Fase
        SELECT SINGLE *
          FROM lfa1 INTO @DATA(lwa_lfa1_pc)
         WHERE lifnr EQ @lwa_carga-codigo_pc.

        CHECK sy-subrc EQ 0.

        DATA(_region_lr_mt) = abap_false.
        LOOP AT lit_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao>).
          SELECT SINGLE *
            FROM kna1 INTO @DATA(lwa_kna1_lr)
           WHERE kunnr EQ @<fs_solicitacao>-kunnr.

          IF sy-subrc EQ 0 AND lwa_kna1_lr-regio EQ 'MT'.
            _region_lr_mt = abap_true.
          ENDIF.
        ENDLOOP.

        CHECK sy-subrc EQ 0.

        IF ( lwa_lfa1_pc-regio EQ 'MT' OR
             _region_lr_mt EQ abap_true ). "Exige fase se Origem ou Destino for MT
          _exige_doc_fase = abap_true.
        ENDIF.
      ENDIF.

  ENDCASE.


*-------------------------------------------------------------------------------------------------------------------------*
* Baixar documentos que não foram baixados
*-------------------------------------------------------------------------------------------------------------------------*
  IF p_zsdt0133-doc_outros_anexado = abap_true.
    DELETE p_response_consulta-documents-additional WHERE type = '10'.
  ENDIF.

  IF p_zsdt0133-doc_bordero_anexado = abap_true.
    DELETE p_response_consulta-documents-additional WHERE type = '20'.
  ENDIF.

  IF p_zsdt0133-doc_fase_anexado = abap_true OR _exige_doc_fase EQ abap_false.
    DELETE p_response_consulta-documents-additional WHERE type = '21'.
  ENDIF.

  IF p_zsdt0133-doc_certificado_anexado = abap_true.
    DELETE p_response_consulta-documents-additional WHERE type = '22'.
  ENDIF.

  LOOP AT p_response_consulta-documents-additional INTO DATA(lwa_document_viagem).
    PERFORM f_anexa_doc_carga_saida_in  USING lwa_document_viagem-url
                                              lwa_document_viagem-type
                                     CHANGING p_zsdt0133.
  ENDLOOP.


  "Download NF-e
  IF p_zsdt0133-doc_nfe_anexado IS INITIAL AND p_response_consulta-documents-fiscal[] IS NOT INITIAL. .

    DATA(_baixar_nfe) = abap_false.

    CASE _spart.
      WHEN '02'. "Fertilizantes
        _baixar_nfe = abap_true.
      WHEN '03'. "Defensivos
        _baixar_nfe = abap_true.
      WHEN '04'. "Sementes

        DATA(_doc_fase_ok) = abap_false.

        IF _exige_doc_fase EQ abap_true.
          IF p_zsdt0133-doc_fase_anexado IS NOT INITIAL.
            _doc_fase_ok = abap_true.
          ENDIF.
        ELSE.
          _doc_fase_ok = abap_true.
        ENDIF.

        _baixar_nfe = abap_true.
*        IF _exige_doc_fase EQ abap_false.
*          _baixar_nfe = abap_true.
*        ELSE.
*          IF _doc_fase_ok                       EQ abap_true AND
*             p_zsdt0133-doc_certificado_anexado IS NOT INITIAL.
*            _baixar_nfe = abap_true.
*          ENDIF.
*        ENDIF.

    ENDCASE.


    IF _baixar_nfe EQ abap_true AND lit_romaneios[] IS INITIAL.

      CLEAR: lit_zsdt0410[].

      LOOP AT p_response_consulta-documents-fiscal INTO DATA(lwa_document_fiscal).
        APPEND INITIAL LINE TO lit_zsdt0410 ASSIGNING FIELD-SYMBOL(<fs_zsdt0410>).

        IF lwa_document_fiscal-issue_date IS INITIAL.
          RETURN.
        ENDIF.

        <fs_zsdt0410>-nro_cg      = p_zsdt0133-nro_cg.
        <fs_zsdt0410>-chave_nfe   = lwa_document_fiscal-access_key.
        <fs_zsdt0410>-data_nfe    = lwa_document_fiscal-issue_date+00(04) &&
                                    lwa_document_fiscal-issue_date+05(02) &&
                                    lwa_document_fiscal-issue_date+08(02).
        <fs_zsdt0410>-processo    = 2.
        <fs_zsdt0410>-user_create = sy-uname.
        <fs_zsdt0410>-date_create = sy-datum.
        <fs_zsdt0410>-time_create = sy-uzeit.
      ENDLOOP.

      MODIFY zsdt0410 FROM TABLE lit_zsdt0410.
      p_zsdt0133-doc_nfe_anexado = abap_true.
      UPDATE zsdt0133 SET doc_nfe_anexado = abap_true WHERE nro_cg = p_zsdt0133-nro_cg.
      COMMIT WORK.

    ENDIF.

  ENDIF.

*-------------------------------------------------------------------------------------------------------------------------*
*  Aprovar Liberação Carregamento Carguero
*-------------------------------------------------------------------------------------------------------------------------*

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_aprovar_viagem)
   WHERE name = 'ZLESR0167_NAO_APROVA_PROC_02'.

  IF sy-subrc NE 0.

    CASE _spart.
      WHEN '02'. "Fertilizantes
        _lib_aprova_viagem_carguero = abap_true.
      WHEN '03'. "Defensivos
        _lib_aprova_viagem_carguero = abap_true.
      WHEN '04'. "Sementes
        IF p_zsdt0133-doc_nfe_anexado IS NOT INITIAL.
          _lib_aprova_viagem_carguero = abap_true.
        ENDIF.
    ENDCASE.

    IF _lib_aprova_viagem_carguero EQ abap_true AND p_zsdt0133-viagem_aprovada_carguero = abap_false.
      PERFORM f_aprova_viagem_carguero_cg_s USING p_zsdt0133 p_response_consulta.
    ENDIF.

  ENDIF.

ENDFORM.

*-US190812-16.09.2025-#190812-JT-inicio
FORM f_processa_retorno_03 USING p_zsdt0133          TYPE zsdt0133
                                 p_response_consulta TYPE zlese0275.

  DATA: lv_def_transp  TYPE char01.

  lv_def_transp = abap_false.

  CHECK p_zsdt0133-nro_cg IS NOT INITIAL AND p_response_consulta-id IS NOT INITIAL.

  LOOP AT p_response_consulta-transportadores INTO DATA(_transportadores).
    IF _transportadores-status = '0'.
      lv_def_transp = abap_false.
      EXIT.
    ELSE.
      lv_def_transp = abap_true.
    ENDIF.
  ENDLOOP.

  CHECK lv_def_transp = abap_true.

  UPDATE zsdt0133 SET dt_frete_contratado     = sy-datum
                      int_dt_frete_contratado = abap_true
                WHERE nro_cg                  = p_zsdt0133-nro_cg.

  COMMIT WORK AND WAIT.

ENDFORM.
*-US190812-16.09.2025-#190812-JT-fim


FORM f_anexa_doc_carga_entrada_in USING p_url      TYPE string
                                        p_tipo
                               CHANGING c_zmmt0201 TYPE zmmt0201.

  DATA: lva_name_file      TYPE string,
        lva_extension_file TYPE string.

  DATA: lt_arquivos    TYPE zmmt_arquivos.

  DATA: lva_string TYPE zde_data_xstring.

  CHECK c_zmmt0201-nro_cg IS NOT INITIAL AND p_url IS NOT INITIAL.

  PERFORM f_get_xstring_of_url USING p_url CHANGING lva_string.

  PERFORM f_get_name_extension_file USING p_url
                                 CHANGING lva_name_file
                                          lva_extension_file.

  CHECK lva_string IS NOT INITIAL.

  APPEND INITIAL LINE TO lt_arquivos ASSIGNING FIELD-SYMBOL(<fs_arquivos>).
  <fs_arquivos>-descricao = lva_name_file && '.' && lva_extension_file.
  <fs_arquivos>-xstring   = lva_string.
  <fs_arquivos>-tipo      = lva_extension_file.

  CALL METHOD zcl_carga_entrada_insumos=>anexar_documentos_carga(
    EXPORTING
      i_nro_carga = c_zmmt0201-nro_cg
      i_arquivos  = lt_arquivos
    IMPORTING
      e_msg_erro  = DATA(lv_msg) ).

  CHECK lv_msg IS INITIAL.

  CASE p_tipo.
    WHEN '10'. "Outros
      UPDATE zmmt0201 SET doc_outros_anexado = abap_true WHERE nro_cg = c_zmmt0201-nro_cg.
      c_zmmt0201-doc_outros_anexado = abap_true.
    WHEN '20'. "Bordero
      UPDATE zmmt0201 SET doc_bordero_anexado = abap_true WHERE nro_cg = c_zmmt0201-nro_cg.
      c_zmmt0201-doc_bordero_anexado = abap_true.
    WHEN '21'. "Fase
      UPDATE zmmt0201 SET doc_fase_anexado = abap_true WHERE nro_cg = c_zmmt0201-nro_cg.
      c_zmmt0201-doc_fase_anexado = abap_true.
    WHEN '22'. "Certificado
      UPDATE zmmt0201 SET doc_certificado_anexado = abap_true WHERE nro_cg = c_zmmt0201-nro_cg.
      c_zmmt0201-doc_certificado_anexado = abap_true.
  ENDCASE.

  COMMIT WORK.


ENDFORM.


FORM f_anexa_doc_carga_saida_in USING p_url    TYPE string
                                      p_tipo
                             CHANGING c_zsdt0133 TYPE zsdt0133.

  DATA: lva_name_file      TYPE string,
        lva_extension_file TYPE string.

  DATA: lt_arquivos    TYPE zmmt_arquivos.

  DATA: lva_string TYPE zde_data_xstring.

  CHECK c_zsdt0133-nro_cg IS NOT INITIAL AND p_url IS NOT INITIAL.

  PERFORM f_get_xstring_of_url USING p_url CHANGING lva_string.

  PERFORM f_get_name_extension_file USING p_url
                                 CHANGING lva_name_file
                                          lva_extension_file.

  CHECK lva_string IS NOT INITIAL.

  APPEND INITIAL LINE TO lt_arquivos ASSIGNING FIELD-SYMBOL(<fs_arquivos>).
  <fs_arquivos>-descricao = lva_name_file && '.' && lva_extension_file.
  <fs_arquivos>-xstring   = lva_string.
  <fs_arquivos>-tipo      = lva_extension_file.

  CALL METHOD zcl_carga_saida_insumos=>anexar_documentos_carga(
    EXPORTING
      i_nro_carga = c_zsdt0133-nro_cg
      i_arquivos  = lt_arquivos
    IMPORTING
      e_msg_erro  = DATA(lv_msg) ).

  CHECK lv_msg IS INITIAL.

  CASE p_tipo.
    WHEN '10'. "Outros
      UPDATE zsdt0133 SET doc_outros_anexado = abap_true WHERE nro_cg = c_zsdt0133-nro_cg.
      c_zsdt0133-doc_outros_anexado = abap_true.
    WHEN '20'. "Bordero
      UPDATE zsdt0133 SET doc_bordero_anexado = abap_true WHERE nro_cg = c_zsdt0133-nro_cg.
      c_zsdt0133-doc_bordero_anexado = abap_true.
    WHEN '21'. "Fase
      UPDATE zsdt0133 SET doc_fase_anexado = abap_true WHERE nro_cg = c_zsdt0133-nro_cg.
      c_zsdt0133-doc_fase_anexado = abap_true.
    WHEN '22'. "Certificado
      UPDATE zsdt0133 SET doc_certificado_anexado = abap_true WHERE nro_cg = c_zsdt0133-nro_cg.
      c_zsdt0133-doc_certificado_anexado = abap_true.
  ENDCASE.

  COMMIT WORK.


ENDFORM.

FORM f_get_name_extension_file USING p_url TYPE string
                            CHANGING c_name_file TYPE string
                                     c_extension_file TYPE string.

  DATA: lv_input     TYPE string,
        lv_name_file TYPE string,
        lv_start_pos TYPE i,
        lv_end_pos   TYPE i.

  CLEAR: c_name_file, c_extension_file.

  lv_input = p_url.

  FIND '/viagens/' IN lv_input MATCH OFFSET lv_start_pos.
  lv_start_pos =  lv_start_pos + 9.
  lv_input = lv_input+lv_start_pos.

  FIND '/' IN lv_input MATCH OFFSET lv_start_pos.
  lv_start_pos = lv_start_pos + 1.
  lv_input     = lv_input+lv_start_pos.

  FIND '?' IN lv_input MATCH OFFSET lv_end_pos.

  lv_name_file = lv_input(lv_end_pos).

  SPLIT lv_name_file AT '.' INTO c_name_file c_extension_file.

ENDFORM.

FORM f_aprova_viagem_carguero_cg_e  USING p_zmmt0201 TYPE zmmt0201
                                          p_response_consulta TYPE zlese0274.

  DATA: lva_pdf_xstring  TYPE xstring,
        lva_chave        TYPE string,
        lwa_nfe_terceiro TYPE zmme0252.

  CHECK p_zmmt0201-viagem_id IS NOT INITIAL.

  SELECT SINGLE *
    FROM zlest0185 INTO @DATA(lwa_zlest0185)
   WHERE viagem_id EQ @p_zmmt0201-viagem_id.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM zmmt0203 INTO @DATA(lwa_zmmt0203)
   WHERE nro_cg EQ @p_zmmt0201-nro_cg.

  CHECK sy-subrc EQ 0 AND lwa_zmmt0203-chave_nfe IS NOT INITIAL.

*  SELECT SINGLE *
*    FROM zib_nfe_dist_ter INTO @DATA(lwa_zib_nfe_dist_ter)
*   WHERE chave_nfe EQ @lwa_zmmt0203-chave_nfe.
*
*  CHECK sy-subrc EQ 0.

*-----------------------------------------------------------------------------------------------------*
*  Obter Autorização no Storage do Carguero para Upload Doc. Aprovação
*-----------------------------------------------------------------------------------------------------*

*  TRY .
*
*      DATA(lva_url_doc_aprovacao) =  zcl_int_ob_carguero_tn_apr_ter=>zif_int_ob_carguero_tn_apr_ter~get_url_documento( EXPORTING i_viagem_id  = lwa_zlest0185-viagem_id ).
*
*      "Carega(autentica) a URL do Documento de Viagem.
*      zcl_integracao_upload_auth=>zif_integracao_upload_auth~get_instance(
*        )->set_empresa(       EXPORTING i_bukrs         = lwa_zlest0185-bukrs
*        )->set_id_referencia( EXPORTING i_id_referencia = lwa_zlest0185-viagem_id
*        )->set_blob_path(     EXPORTING i_blob_path     = CONV #( lva_url_doc_aprovacao )
*        )->get_json(          IMPORTING e_json          = DATA(vl_json_tn)
*        )->set_ds_data(       EXPORTING i_json          = vl_json_tn
*        )->set_ds_url(
*        )->set_send_msg(      IMPORTING e_id_integracao = DATA(vl_id_integracao)
*                                        e_url_upload    = DATA(vl_url_upload)
*        ).
*
*    CATCH zcx_integracao INTO DATA(ex_integracao_tn).
*       RETURN.
*
*
*    CATCH zcx_error INTO DATA(ex_error_tn).
*       RETURN.
*
*  ENDTRY.

*-----------------------------------------------------------------------------------------------------*
*  Gerar PDF Doc. Aprovação
*-----------------------------------------------------------------------------------------------------*
  "lva_chave =  lwa_zmmt0203-chave_nfe.

*  CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
*    EXPORTING
*      i_chave = lva_chave
*      i_tipo  = 'PDF'
*    IMPORTING
*      out     = lva_pdf_xstring.

  "zcl_carga_entrada_insumos=>gerar_autorizacao_embarque_sm( EXPORTING i_carga  = p_zmmt0201-nro_cg
  "                                                                    i_binary = abap_true
  "                                                                    i_preview = abap_false
  "                                                          IMPORTING e_pdf_xstring = lva_pdf_xstring ).

  " CHECK lva_pdf_xstring IS NOT INITIAL.

*-----------------------------------------------------------------------------------------------------*
*  Aprova viagem no Carguero
*-----------------------------------------------------------------------------------------------------*

  CLEAR: lwa_nfe_terceiro.

  READ TABLE p_response_consulta-documents-fiscal INTO DATA(lwa_document_fiscal) INDEX 1.
  CHECK sy-subrc EQ 0 AND lwa_document_fiscal-access_key IS NOT INITIAL.


  lwa_nfe_terceiro-chave_nfe        = lwa_document_fiscal-access_key.
  lwa_nfe_terceiro-dt_emissao       = lwa_document_fiscal-issue_date+00(04) &&
                                      lwa_document_fiscal-issue_date+05(02) &&
                                      lwa_document_fiscal-issue_date+08(02).

  lwa_nfe_terceiro-numero           = lwa_document_fiscal-number.
  lwa_nfe_terceiro-serie            = lwa_document_fiscal-series.
  lwa_nfe_terceiro-qtde_faturada    = lwa_document_fiscal-invoiced_quantity.


  TRY .
      zcl_int_ob_carguero_tn_apr_ter=>zif_int_ob_carguero_tn_apr_ter~get_instance(
        )->aprovar_viagem(
        EXPORTING
          i_viagem_id       = p_zmmt0201-viagem_id
          i_dt_carregamento = sy-datum
          i_peso_bruto      = CONV #( p_response_consulta-weight_ticket-gross_weight )
          i_peso_liquido    = CONV #( p_response_consulta-weight_ticket-net_weight )
          i_peso_tara       = CONV #( p_response_consulta-weight_ticket-tara_weight )
          i_nfe_terceiro    = lwa_nfe_terceiro
        ).

    CATCH zcx_integracao INTO DATA(ex_integracao).
*      IF NOT ( ex_integracao->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
*               ex_integracao->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
*        MESSAGE ID ex_integracao->msgid TYPE 'S' NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4.
*      ENDIF.

      RETURN.

    CATCH zcx_error INTO DATA(ex_error).
      "MESSAGE ID ex_integracao->msgid TYPE 'S' NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4.

      RETURN.
  ENDTRY.

*-----------------------------------------------------------------------------------------------------*
*  Realizar Upload do Arquivo no Storage
*-----------------------------------------------------------------------------------------------------*

*  TRY.
*      zcl_integracao_upload_exec=>zif_integracao_upload_exec~get_instance(
*        )->set_empresa(       EXPORTING i_bukrs         = lwa_zlest0185-bukrs
*        )->set_id_referencia( EXPORTING i_id_referencia = lwa_zlest0185-viagem_id
*        )->get_json(          EXPORTING i_file_bin      = lva_pdf_xstring
*                              IMPORTING e_json_xstring  = DATA(vl_json_tn_xstring)
*        )->set_ds_url(        EXPORTING i_url_upload    = vl_url_upload
*        )->set_ds_data(       EXPORTING i_json_xstring  = vl_json_tn_xstring
*        )->set_send_msg(      IMPORTING e_id_integracao = vl_id_integracao
*        ).
*
*    CATCH zcx_integracao INTO DATA(ex_integracao_tn_ret).
*       RETURN.
*    CATCH zcx_error INTO DATA(ex_error_tn_ret).
*       RETURN.
*
*  ENDTRY.

*-----------------------------------------------------------------------------------------------------*
*  Confirmar Envio Doc. Aprovação Storage
*-----------------------------------------------------------------------------------------------------*

  IF p_zmmt0201-inco1 = 'CIF'.

    TRY .
        zcl_integracao_upload_conf=>zif_integracao_upload_conf~get_instance(
          )->set_empresa(       EXPORTING i_bukrs         = lwa_zlest0185-bukrs
          )->set_id_referencia( EXPORTING i_id_referencia = lwa_zlest0185-viagem_id
          )->set_ds_url(        EXPORTING i_viagem_id     = lwa_zlest0185-viagem_id
          )->set_send_msg(      IMPORTING e_id_integracao = DATA(l_id_integracao)
          ).

      CATCH zcx_integracao INTO DATA(ex_integracao_tn_con).
        RETURN.

      CATCH zcx_error INTO DATA(ex_error_tn_con).
        RETURN.
    ENDTRY.

  ENDIF.

*-----------------------------------------------------------------------------------------------------*
*  Encerrar Processo marcando a carga de entrada
*-----------------------------------------------------------------------------------------------------*

  UPDATE zmmt0201 SET viagem_aprovada_carguero = abap_true WHERE nro_cg = p_zmmt0201-nro_cg.

ENDFORM.


FORM f_aprova_viagem_carguero_cg_s  USING p_zsdt0133 TYPE zsdt0133
                                          p_response_consulta TYPE zlese0274.

  DATA: lva_pdf_xstring  TYPE xstring,
        lva_chave        TYPE string,
        lwa_nfe_terceiro TYPE zmme0252.

  CHECK p_zsdt0133-viagem_id IS NOT INITIAL.

  SELECT SINGLE *
    FROM zlest0185 INTO @DATA(lwa_zlest0185)
   WHERE viagem_id EQ @p_zsdt0133-viagem_id.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM zsdt0129 INTO @DATA(lwa_zsdt0129)
   WHERE nro_cg EQ @p_zsdt0133-nro_cg.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM zsdt0410 INTO @DATA(lwa_zsdt0410)
   WHERE nro_cg EQ @p_zsdt0133-nro_cg.

  CHECK sy-subrc EQ 0 AND lwa_zsdt0410-chave_nfe IS NOT INITIAL.

*-----------------------------------------------------------------------------------------------------*
*  Aprova viagem no Carguero
*-----------------------------------------------------------------------------------------------------*

  CLEAR: lwa_nfe_terceiro.

  READ TABLE p_response_consulta-documents-fiscal INTO DATA(lwa_document_fiscal) INDEX 1.
  CHECK sy-subrc EQ 0 AND lwa_document_fiscal-access_key IS NOT INITIAL.


  lwa_nfe_terceiro-chave_nfe        = lwa_document_fiscal-access_key.
  lwa_nfe_terceiro-dt_emissao       = lwa_document_fiscal-issue_date+00(04) &&
                                      lwa_document_fiscal-issue_date+05(02) &&
                                      lwa_document_fiscal-issue_date+08(02).

  lwa_nfe_terceiro-numero           = lwa_document_fiscal-number.
  lwa_nfe_terceiro-serie            = lwa_document_fiscal-series.
  lwa_nfe_terceiro-qtde_faturada    = lwa_document_fiscal-invoiced_quantity.


  TRY .
      zcl_int_ob_carguero_tn_apr_ter=>zif_int_ob_carguero_tn_apr_ter~get_instance(
        )->aprovar_viagem(
        EXPORTING
          i_viagem_id       = p_zsdt0133-viagem_id
          i_dt_carregamento = sy-datum
          i_peso_bruto      = CONV #( p_response_consulta-weight_ticket-gross_weight )
          i_peso_liquido    = CONV #( p_response_consulta-weight_ticket-net_weight )
          i_peso_tara       = CONV #( p_response_consulta-weight_ticket-tara_weight )
          i_nfe_terceiro    = lwa_nfe_terceiro
        ).

    CATCH zcx_integracao INTO DATA(ex_integracao).
*      IF NOT ( ex_integracao->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
*               ex_integracao->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
*        MESSAGE ID ex_integracao->msgid TYPE 'S' NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4.
*      ENDIF.

      RETURN.

    CATCH zcx_error INTO DATA(ex_error).
      "MESSAGE ID ex_integracao->msgid TYPE 'S' NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4.

      RETURN.
  ENDTRY.

*-----------------------------------------------------------------------------------------------------*
*  Confirmar Envio Doc. Aprovação Storage
*-----------------------------------------------------------------------------------------------------*

  IF lwa_zsdt0129-inco1 = 'CIF'.

    TRY .
        zcl_integracao_upload_conf=>zif_integracao_upload_conf~get_instance(
          )->set_empresa(       EXPORTING i_bukrs         = lwa_zlest0185-bukrs
          )->set_id_referencia( EXPORTING i_id_referencia = lwa_zlest0185-viagem_id
          )->set_ds_url(        EXPORTING i_viagem_id     = lwa_zlest0185-viagem_id
          )->set_send_msg(      IMPORTING e_id_integracao = DATA(l_id_integracao)
          ).

      CATCH zcx_integracao INTO DATA(ex_integracao_tn_con).
        RETURN.

      CATCH zcx_error INTO DATA(ex_error_tn_con).
        RETURN.
    ENDTRY.

  ENDIF.

*-----------------------------------------------------------------------------------------------------*
*  Encerrar Processo marcando a carga de entrada
*-----------------------------------------------------------------------------------------------------*

  UPDATE zsdt0133 SET viagem_aprovada_carguero = abap_true WHERE nro_cg = p_zsdt0133-nro_cg.

ENDFORM.
