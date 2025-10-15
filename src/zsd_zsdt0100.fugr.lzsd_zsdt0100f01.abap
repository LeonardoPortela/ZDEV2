*----------------------------------------------------------------------*
***INCLUDE LZSD_ZSDT0100F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_refresh_all
*&---------------------------------------------------------------------*
FORM f_refresh_all .

  CLEAR: zsds_tela_calculo_aprovacao, gv_ucomm, gt_saldos, gv_error.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_existe_checklist
*&---------------------------------------------------------------------*
FORM f_existe_checklist USING uv_simu  TYPE  zsded003
                              uv_vbeln TYPE  vbeln_va
                     CHANGING cv_erro TYPE c.

  " 1 - sem checklist ativo
  " 2 - com checklist, mas com inconformidade
  " space - com checklist e ok

  CLEAR cv_erro.

  SELECT SINGLE status FROM zsdt0381
    INTO @DATA(lv_st_wf)
      WHERE doc_simulacao = @uv_simu.

*  READ TABLE gt_checklist TRANSPORTING NO FIELDS
*      WITH KEY doc_simulacao = uv_simu.
*
*  IF sy-subrc NE 0.

  SELECT * FROM zi_in_chklist_sim_item
    "APPENDING TABLE @gt_checklist
    INTO TABLE @DATA(lt_checklist)
      WHERE doc_simulacao = @uv_simu.

*  ENDIF.

  READ TABLE lt_checklist TRANSPORTING NO FIELDS
   WITH KEY doc_simulacao = uv_simu
            ativo = abap_true.

  IF sy-subrc NE 0.
    cv_erro = '1'.
    EXIT.
  ENDIF.

  READ TABLE lt_checklist TRANSPORTING NO FIELDS
   WITH KEY doc_simulacao = uv_simu
            tpinconf = 'S'
            flag_sim = abap_true.

  IF sy-subrc EQ 0.

    IF lv_st_wf <> '04'.
      cv_erro = '2'.
      EXIT.
    ELSE.
      CLEAR cv_erro.
      EXIT.
    ENDIF.


  ENDIF.

  READ TABLE lt_checklist TRANSPORTING NO FIELDS
   WITH KEY doc_simulacao = uv_simu
            tpinconf = 'N'
            flag_nao = abap_true.

  IF sy-subrc EQ 0.
    IF lv_st_wf <> '04'.
      cv_erro = '2'.
      EXIT.
    ELSE.
      CLEAR cv_erro.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preenche_saldos
*&---------------------------------------------------------------------*
FORM f_preenche_saldos USING uv_vbeln TYPE vbeln_va
                             uv_simu TYPE zsded003.

  SELECT SINGLE doc_simulacao,vbeln,cnpj_cpf,safra,cultura,
      valor_limite_disponivel_sap FROM zsd_in_est_limite_ov_01
    INTO @DATA(ls_sap)
      WHERE doc_simulacao = @uv_simu
        AND vbeln = @uv_vbeln.

  IF ls_sap-cnpj_cpf IS NOT INITIAL AND ls_sap-valor_limite_disponivel_sap > 0.

    APPEND INITIAL LINE TO gt_saldos ASSIGNING FIELD-SYMBOL(<fs_linha>).

    <fs_linha>-origem = 'S'.
    <fs_linha>-origem_str = 'SAP'.
    <fs_linha>-id_sap = '0'.
    <fs_linha>-id_opus = '0'.

    IF ls_sap-valor_limite_disponivel_sap > 0.
      <fs_linha>-vlr_ttl_disp = ls_sap-valor_limite_disponivel_sap.
    ELSE.
      <fs_linha>-vlr_ttl_disp = 0.
    ENDIF.

    <fs_linha>-saldo_dispo = ls_sap-valor_limite_disponivel_sap.
    <fs_linha>-saldo_usado = '0.00'.
    <fs_linha>-waerk = 'USD'.

    SELECT * FROM zi_sd_in_limite_opus
      INTO TABLE @DATA(lt_opus)
        WHERE emitente = @ls_sap-cnpj_cpf
             AND safra = @ls_sap-safra
            AND cultura = @ls_sap-cultura
            AND vlrlimite > 0.

    LOOP AT lt_opus ASSIGNING FIELD-SYMBOL(<fs_opus>).

      APPEND INITIAL LINE TO gt_saldos ASSIGNING <fs_linha>.

      <fs_linha>-origem = 'O'.
      <fs_linha>-origem_str = 'OPUS'.
      <fs_linha>-id_sap = <fs_opus>-id_limite_sap.
      <fs_linha>-id_opus = <fs_opus>-id_limite.
      <fs_linha>-vlr_ttl_disp = <fs_opus>-vlrlimite.
      <fs_linha>-saldo_dispo = <fs_opus>-vlrlimite.
      <fs_linha>-saldo_usado = '0.00'.
      <fs_linha>-waerk = 'USD'.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_cria_nova_linha
*&---------------------------------------------------------------------*
FORM f_cria_nova_linha USING uv_vbeln TYPE vbeln_va
                             uv_teste TYPE flag
                             uv_status TYPE zsde_status_wf
                             uv_origem TYPE zsde_saldo_origem
                             uv_waerk TYPE waerk
                             uv_kursf TYPE kursf
                             uv_valor TYPE netwr_ap
                             uv_id_limite TYPE zsde_id_limite_sap
                             uv_just_text TYPE zsde_just_wf
                        CHANGING ct_116 TYPE zsds_zsdt0116_tab
                                 ct_mess TYPE bapiret2_tab.

  DATA lv_valor TYPE p DECIMALS 6.

  APPEND INITIAL LINE TO ct_116 ASSIGNING FIELD-SYMBOL(<fs_116>).

  IF uv_teste IS INITIAL.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZSEQ_0116_'
      IMPORTING
        number      = <fs_116>-seq.

  ENDIF.

  <fs_116>-vbeln = uv_vbeln.

  <fs_116>-user_apv = <fs_116>-user_solicitante = sy-uname.
  <fs_116>-dt_apv = <fs_116>-data_solicitante = sy-datum.
  <fs_116>-hr_apv = <fs_116>-hora_solicitante = sy-uzeit.
  <fs_116>-status = space.
  <fs_116>-status_workflow = uv_status.
  <fs_116>-saldo_origem = uv_origem.
  <fs_116>-vlr_liberado = uv_valor.

  IF uv_just_text IS NOT INITIAL.
    <fs_116>-just_workflow = uv_just_text.
  ENDIF.

  IF uv_id_limite IS NOT INITIAL.
    <fs_116>-id_limite = uv_id_limite.
  ENDIF.

  <fs_116>-waerk = uv_waerk.
  <fs_116>-kursf = uv_kursf.

  IF <fs_116>-kursf IS NOT INITIAL.

    IF <fs_116>-waerk <> 'USD'.
      <fs_116>-vlr_liberado_moeda = uv_valor *  uv_kursf.
    ELSE.
      <fs_116>-vlr_liberado_moeda = uv_valor.
    ENDIF.

  ENDIF.

  " 08.09.2025 - RAMON -->
  <fs_116>-vlr_original  = <fs_116>-vlr_liberado.
  <fs_116>-vlr_original_moeda = <fs_116>-vlr_liberado_moeda.
  " 08.09.2025 - RAMON --<

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_atualiza_campos
*&---------------------------------------------------------------------*
FORM f_atualiza_campos .

  DATA lv_ttl_apro TYPE netwr_ap.
  DATA lv_erro.

  LOOP AT gt_saldos ASSIGNING FIELD-SYMBOL(<fs_saldos>).

    <fs_saldos>-saldo_dispo = <fs_saldos>-vlr_ttl_disp.

    IF <fs_saldos>-saldo_usado IS NOT INITIAL.

      SUBTRACT <fs_saldos>-saldo_usado FROM <fs_saldos>-saldo_dispo.

      ADD <fs_saldos>-saldo_usado TO lv_ttl_apro.

    ENDIF.

    IF <fs_saldos>-saldo_dispo < 0.
      MESSAGE e016(ds) WITH 'Valor usado maior que o disponivel'.
      lv_erro = abap_true.
    ENDIF.

  ENDLOOP.

  IF lv_erro = abap_true.
    EXIT.
  ENDIF.

  zsds_tela_calculo_aprovacao-vlr_ttl_wf = 0.

  zsds_tela_calculo_aprovacao-vlr_ttl_saldo = lv_ttl_apro.

  zsds_tela_calculo_aprovacao-vlr_ttl_wf = zsds_tela_calculo_aprovacao-vlr_ttl_lib - lv_ttl_apro.

  IF zsds_tela_calculo_aprovacao-vlr_ttl_saldo > zsds_tela_calculo_aprovacao-vlr_ttl_lib.
    MESSAGE e016(ds) WITH 'Valor aprovado é maior do que o valor à liberar'.
    EXIT.
  ENDIF.

  IF zsds_tela_calculo_aprovacao-vlr_ttl_wf < 0.
    MESSAGE e016(ds) WITH  'Valor à enviar para aprovação não pode ser negativo'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processar_apos_ok
*&---------------------------------------------------------------------*
FORM f_processar_apos_ok.

  DATA lv_msgx TYPE c LENGTH 200.
  DATA cv_ret.

  gv_error = abap_false.

  IF zsds_tela_calculo_aprovacao-vlr_ttl_wf > 0.

    lv_msgx = |Deseja enviar O.V { zsds_tela_calculo_aprovacao-vbeln } para aprovação por Workflow?|.

  ELSE.

    lv_msgx = |Confirmar aprovação da O.V { zsds_tela_calculo_aprovacao-vbeln } ?|.

  ENDIF.

  PERFORM f_popup_to_confirm USING lv_msgx CHANGING cv_ret.

  IF cv_ret <> '1'.
    gv_error = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_to_confirm USING p_question TYPE c
                     CHANGING p_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = sy-title
      text_question  = p_question
      default_button = '1'
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& Form f_busca_vlr_fin
*&---------------------------------------------------------------------*
FORM f_busca_vlr_fin  USING uv_simu TYPE zsded003
                   CHANGING cv_vlr_fin TYPE netwr_ap.

  SELECT SUM( vlr_limite_fin ) FROM zsd_in_est_saldo_02
    INTO @cv_vlr_fin
      WHERE doc_simulacao = @uv_simu.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_text
*&---------------------------------------------------------------------*
FORM f_get_text USING uv_title CHANGING cv_texto TYPE zsde_just_wf.

  DATA lv_just TYPE string.


  CALL FUNCTION 'Z_CAIXA_TEXTO'
    IMPORTING
      ev_texto = lv_just.

  cv_texto = lv_just.
*
*
*  DATA lv_code TYPE c.
*
*  DATA lt_fields  TYPE TABLE OF sval.
*
*  lt_fields =
*    VALUE #( ( tabname = 'ZSDT0116' fieldname = 'JUST_WORKFLOW' field_obl = '' fieldtext = uv_title value =  cv_texto )  ).
*
*  CALL FUNCTION 'POPUP_GET_VALUES'
*    EXPORTING
*      popup_title     = sy-title
*    IMPORTING
*      returncode      = lv_code
*    TABLES
*      fields          = lt_fields
*    EXCEPTIONS
*      error_in_fields = 1
*      OTHERS          = 2.
*
*  CHECK lv_code IS INITIAL AND lt_fields[] IS NOT INITIAL.
*
*  LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_field>) WHERE value IS NOT INITIAL.
*
*    CASE <fs_field>-fieldname.
*      WHEN 'JUST_WORKFLOW'.
*        cv_texto = <fs_field>-value.
*    ENDCASE.
*
*  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_com_popup
*&---------------------------------------------------------------------*
FORM f_com_popup USING uv_simu  TYPE zsded003
                       uv_vbeln TYPE vbeln_va
                       uv_tpsim TYPE  char2
                       uv_kwert TYPE netwr_ak
                       uv_kwert_m TYPE netwr_ak
                       uv_waerk TYPE waerk
                       iv_kursf TYPE kursf
                       uv_test TYPE c
              CHANGING cv_erro TYPE flag
                       ct_116   TYPE zsds_zsdt0116_tab
                       ct_mess  TYPE bapiret2_tab.

  CALL FUNCTION 'Z_OV_POPUP_LIBERACAO_SALDO'
    EXPORTING
      iv_simu     = uv_simu
      iv_vbeln    = uv_vbeln
      iv_tpsim    = uv_tpsim
      iv_waerk    = uv_waerk
      iv_kwert    = uv_kwert
      iv_kwert_m  = uv_kwert_m
      iv_kursf    = iv_kursf
      iv_teste    = uv_test
    IMPORTING
      et_zsdt0116 = ct_116
      ev_erro     = cv_erro
      et_message  = ct_mess.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_sem_popup
*&---------------------------------------------------------------------*
FORM f_sem_popup USING uv_simu  TYPE zsded003
                       uv_vbeln TYPE vbeln_va
                       uv_waerk TYPE waerk
                       uv_kwert TYPE netwr_ak
                       uv_kwert_m TYPE netwr_ak
                       uv_kursf TYPE kursf
                       uv_test TYPE c
              CHANGING cv_erro TYPE flag
                       ct_116   TYPE zsds_zsdt0116_tab
                       ct_mess  TYPE bapiret2_tab.

  DATA lv_limite TYPE netwr_ap.
  DATA lv_vlr_aux TYPE netwr_ap.
  DATA lv_vlr_solicitado TYPE netwr_ap.
  DATA lv_vlr_fin TYPE netwr_ap.

  CLEAR cv_erro.

  SELECT SINGLE * FROM zsd_in_est_limite_ov_01
    INTO @DATA(ls_limite)
      WHERE doc_simulacao = @uv_simu
        AND vbeln = @uv_vbeln.

  IF sy-subrc NE 0.

    cv_erro = abap_true.
    ct_mess = VALUE bapiret2_tab(  ( type = 'E' id = 'DS' number = '016' message = 'Sem saldo cadastrado' ) ).
    EXIT.

  ENDIF.

  " buscar saldo financeiro lv_vlr_fin = ( recebido das demais ovs simuladores ) -  ( quanto eu ja aprovei dessas ov na 116 )
  PERFORM f_busca_vlr_fin USING uv_simu CHANGING lv_vlr_fin.

  " muda de variavel para ir descontando quanto foi sendo usado....
  lv_vlr_solicitado = uv_kwert.

  " enquanto tiver valor solicitado
  WHILE lv_vlr_solicitado > 0.

    CLEAR lv_vlr_aux.

    " se tem saldo financeiro
    IF lv_vlr_fin > 0.

      " x = saldo financeiro menos valor solicitado
      lv_vlr_aux = lv_vlr_fin - lv_vlr_solicitado.

      " se x ficar maior que zero, então posso descontar integral o valor solicitado
      IF lv_vlr_aux > 0.

        PERFORM f_cria_nova_linha
         USING uv_vbeln
               uv_test
               'A' "<- Aprovado direto sem WF
               'F' "<- usando o limite do FINANCEIRO
               uv_waerk
               uv_kursf
               lv_vlr_solicitado
               '0'
               ''
      CHANGING ct_116[] ct_mess[].

        lv_vlr_solicitado = 0.

        " se ficar negativo, significa que vou usar o saldo integral do financeiro
      ELSE.

        lv_limite = lv_vlr_fin.

        PERFORM f_cria_nova_linha
            USING uv_vbeln
                  uv_test
                  'A' "<- Aprovado direto sem WF
                  'F' "<-- usando o limite do FINANCEIRO
                  uv_waerk
                  uv_kursf
                  lv_limite
                  '0'
                  ''
         CHANGING ct_116[] ct_mess[].

        " retiro o que foi usado do limite FINANCEIRO
        SUBTRACT lv_vlr_fin FROM lv_vlr_solicitado.

        " atualiza o saldo virtual do FINANCEIRO para 0
        lv_vlr_fin = 0.

      ENDIF.

      " se tiver saldo do sap
    ELSEIF ls_limite-valor_limite_disponivel_sap > 0.

      " x = saldo financeiro menos valor solicitado
      lv_vlr_aux = ls_limite-valor_limite_disponivel_sap - lv_vlr_solicitado.

      " se x ficar maior que zero, então posso descontar integral o valor solicitado
      IF lv_vlr_aux > 0.

        PERFORM f_cria_nova_linha
         USING uv_vbeln
               uv_test
               'A' "<- Aprovado direto sem WF
               'S' "<- usando o limite do SAP
               uv_waerk
               uv_kursf
               lv_vlr_solicitado
               '0'
               ''
      CHANGING ct_116[] ct_mess[].

        lv_vlr_solicitado = 0.

        " se ficar negativo, significa que vou usar o saldo integral do sap
      ELSE.

        " se ficar negativo, então uso o saldo total do sap
        lv_limite = ls_limite-valor_limite_disponivel_sap.

        PERFORM f_cria_nova_linha
            USING uv_vbeln
                  uv_test
                  'A' "<- Aprovado direto sem WF
                  'S' "<-- usando o limite do sap]
                  uv_waerk
                  uv_kursf
                  lv_limite
                  '0'
                  ''
         CHANGING ct_116[] ct_mess[].

        " retiro o que foi usado do limite SAP
        SUBTRACT ls_limite-valor_limite_disponivel_sap FROM lv_vlr_solicitado.

        " atualiza o saldo virtual do sap para 0
        ls_limite-valor_limite_disponivel_sap = 0.

      ENDIF.

      " se tiver limite do opus
    ELSEIF ls_limite-valor_limite_disponivel_opus > 0.

      lv_vlr_aux = ls_limite-valor_limite_disponivel_opus - lv_vlr_solicitado.

      " se x ficar maior que zero, então posso descontar integral o valor solicitado
      IF lv_vlr_aux > 0.

        PERFORM f_cria_nova_linha
         USING uv_vbeln
               uv_test
               'A' "<- Aprovado direto sem WF
               'O' "<- usando o limite do OPUS
               uv_waerk
               uv_kursf
               lv_vlr_solicitado
               '0'
               ''
      CHANGING ct_116[] ct_mess[].

        lv_vlr_solicitado = 0.

        " se ficar negativo, significa que vou usar o saldo integral do opus
      ELSE.

        lv_limite = ls_limite-valor_limite_disponivel_opus.

        PERFORM f_cria_nova_linha
            USING uv_vbeln
                  uv_test
                  'A' "<- Aprovado direto sem WF
                  'O' "<-- usando o limite do OPUS
                  uv_waerk
                  uv_kursf
                  lv_limite
                  '0'
                  ''
         CHANGING ct_116[] ct_mess[].

        " retiro o que foi usado do limite OPUS
        SUBTRACT ls_limite-valor_limite_disponivel_opus FROM lv_vlr_solicitado.

        " atualiza o saldo virtual do OPUS para 0
        ls_limite-valor_limite_disponivel_opus = 0.

      ENDIF.

      " não tem saldo nos limites, entao enviamos para aprovação
    ELSE.

      PERFORM f_cria_nova_linha
       USING uv_vbeln
             uv_test
             'L' "<- Liberado WF
             'Z' "<- usando aprovação via ZSDT0117
             uv_waerk
             uv_kursf
             lv_vlr_solicitado
             '0'
             ''
    CHANGING ct_116[] ct_mess[].

      lv_vlr_solicitado = 0.

    ENDIF.

  ENDWHILE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fix_round
*&---------------------------------------------------------------------*
FORM f_fix_round USING iv_vlr_moeda TYPE netwr_ap
              CHANGING ct_116 TYPE zsds_zsdt0116_tab.

  DATA lv_sum TYPE netwr_ap.
  DATA lv_dif TYPE netwr_ap.

  lv_dif = iv_vlr_moeda." valor inicial para comeaçr

  WHILE lv_dif <> 0.

    CLEAR lv_sum.

    LOOP AT ct_116 ASSIGNING FIELD-SYMBOL(<fs_116>).
      ADD <fs_116>-vlr_liberado_moeda TO lv_sum.
    ENDLOOP.

    lv_dif = iv_vlr_moeda - lv_sum.

    CHECK lv_dif <> 0.

    READ TABLE ct_116 ASSIGNING <fs_116> WITH KEY saldo_origem = 'Z'.

    IF sy-subrc NE 0.
      READ TABLE ct_116 ASSIGNING <fs_116> WITH KEY saldo_origem = 'O'.

      IF sy-subrc NE 0.
        READ TABLE ct_116 ASSIGNING <fs_116> WITH KEY saldo_origem = 'S'.
      ENDIF.

      IF sy-subrc NE 0.
        READ TABLE ct_116 ASSIGNING <fs_116> WITH KEY saldo_origem = 'F'.
      ENDIF.

      IF sy-subrc NE 0.
        READ TABLE ct_116 ASSIGNING <fs_116> WITH KEY saldo_origem = space.
      ENDIF.

    ENDIF.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    ADD lv_dif TO <fs_116>-vlr_liberado_moeda.

    <fs_116>-vlr_original_moeda = <fs_116>-vlr_liberado_moeda.

  ENDWHILE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_calcular_linha
*&---------------------------------------------------------------------*
FORM f_calcular_linha USING uv_vbeln TYPE vbeln_va
                            uv_origem TYPE zsde_saldo_origem
                            ut_0116 TYPE  zsds_zsdt0116_tab
                   CHANGING cv_vlr_a_rec TYPE netwr_ap
                            cv_vlr_rec TYPE netwr_ap
                            ct_rec TYPE zsds_zsdt0116_rec_tab.

  DATA lv_a_rec_aux TYPE netwr_ap.

  LOOP AT ut_0116 ASSIGNING FIELD-SYMBOL(<fs_0116>)
      WHERE vbeln = uv_vbeln
        AND saldo_origem = uv_origem.

    lv_a_rec_aux = cv_vlr_rec.

    IF <fs_0116>-waerk <> 'USD'.
      SUBTRACT <fs_0116>-vlr_liberado_moeda FROM lv_a_rec_aux.
    ELSE.
      SUBTRACT <fs_0116>-vlr_liberado FROM lv_a_rec_aux.
    ENDIF.

    APPEND INITIAL LINE TO ct_rec ASSIGNING FIELD-SYMBOL(<fs_calc>).

    <fs_calc>-vbeln = uv_vbeln.
    <fs_calc>-waerk = <fs_0116>-waerk.
    <fs_calc>-saldo_origem = <fs_0116>-saldo_origem.

    IF <fs_0116>-waerk <> 'USD'.
      <fs_calc>-vlr_liberado = <fs_0116>-vlr_liberado_moeda.
    ELSE.
      <fs_calc>-vlr_liberado = <fs_0116>-vlr_liberado.
    ENDIF.

    " se ficar positivo, recebi tudo
    IF lv_a_rec_aux > 0.

      IF <fs_0116>-waerk <> 'USD'.
        <fs_calc>-vlr_recebido = <fs_0116>-vlr_liberado_moeda.
      ELSE.
        <fs_calc>-vlr_recebido = <fs_0116>-vlr_liberado.
      ENDIF.

      cv_vlr_rec = lv_a_rec_aux.

      " se negativo, recebi menos do que esperava.
    ELSE.
      <fs_calc>-vlr_recebido = cv_vlr_rec.

      " zera o que foi recebido
      cv_vlr_rec = 0.

    ENDIF.

    IF <fs_0116>-waerk <> 'USD'.
      <fs_calc>-vlr_restante = <fs_0116>-vlr_liberado_moeda - <fs_calc>-vlr_recebido.
    ELSE.
      <fs_calc>-vlr_restante = <fs_0116>-vlr_liberado - <fs_calc>-vlr_recebido.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_valor_a_rec
*&---------------------------------------------------------------------*
FORM f_valor_a_rec USING uv_vbeln TYPE vbeln_va
                         ct_0116 TYPE  zsds_zsdt0116_tab
                CHANGING cv_vlr_a_rec TYPE netwr_ap.

  CLEAR cv_vlr_a_rec.

  LOOP AT ct_0116 ASSIGNING FIELD-SYMBOL(<fs_116>) WHERE vbeln = uv_vbeln.
    ADD <fs_116>-vlr_liberado TO cv_vlr_a_rec.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_atualiza_vlr_fin
*&---------------------------------------------------------------------*
FORM f_atualiza_vlr_fin .

  DATA lv_vlr_fin TYPE netwr_ap.

  CHECK zsds_tela_calculo_aprovacao-vlr_ttl_fn > 0.

  lv_vlr_fin = zsds_tela_calculo_aprovacao-vlr_ttl_fn.

  SUBTRACT zsds_tela_calculo_aprovacao-vlr_ttl_wf FROM lv_vlr_fin.

  " se ficar negativo, consegue colocar todo o valor do wf no valor financeiro
  IF lv_vlr_fin < 0.

    " então o será usado é o valor cheio
    zsds_tela_calculo_aprovacao-vlr_ttl_lib_fn = zsds_tela_calculo_aprovacao-vlr_ttl_fn.

    "zsds_tela_calculo_aprovacao-vlr_ttl_fn = 0.

    SUBTRACT zsds_tela_calculo_aprovacao-vlr_ttl_lib_fn FROM zsds_tela_calculo_aprovacao-vlr_ttl_wf.

  ELSE.

    "SUBTRACT zsds_tela_calculo_aprovacao-vlr_ttl_wf FROM zsds_tela_calculo_aprovacao-vlr_ttl_fn.
    zsds_tela_calculo_aprovacao-vlr_ttl_lib_fn = zsds_tela_calculo_aprovacao-vlr_ttl_wf.
    zsds_tela_calculo_aprovacao-vlr_ttl_wf = 0.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_INSERE_TXT
*&---------------------------------------------------------------------*
FORM f_mensagem_insere_txt TABLES p_ret_tab STRUCTURE bapiret2
                            USING i_type TYPE bapi_mtype
                                  p_string TYPE string.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).

  DATA lv_msg1 TYPE sy-msgv1.
  DATA lv_msg2 TYPE sy-msgv1.
  DATA lv_msg3 TYPE sy-msgv1.
  DATA lv_msg4 TYPE sy-msgv1.

  lv_texto = p_string.

  CALL FUNCTION 'TR_SPLIT_TEXT'
    EXPORTING
      iv_text  = lv_texto
      iv_len   = 50
    IMPORTING
      et_lines = lt_trtexts.

  LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE sy-tabix.
      WHEN 1.
        lv_msg1 = <fs_line>.
      WHEN 2.
        lv_msg2 = <fs_line>.
      WHEN 3.
        lv_msg3 = <fs_line>.
      WHEN 4.
        lv_msg4 = <fs_line>.
    ENDCASE.

  ENDLOOP.

  PERFORM f_mensagem_insere
    TABLES p_ret_tab
     USING i_type
           'DS'
           '016'
           lv_msg1
           lv_msg2
           lv_msg3
           lv_msg4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_insere TABLES p_ret_tab STRUCTURE bapiret2
                        USING i_type TYPE bapi_mtype
                              i_id  TYPE  symsgid
                              i_number  TYPE  symsgno
                              i_mess_v1 TYPE any
                              i_mess_v2 TYPE any
                              i_mess_v3 TYPE any
                              i_mess_v4 TYPE any.

  APPEND INITIAL LINE TO p_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  MESSAGE ID <fs_ret>-id TYPE <fs_ret>-type NUMBER <fs_ret>-number
    WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
      <fs_ret>-message_v4 INTO <fs_ret>-message.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_job_open
*&---------------------------------------------------------------------*
FORM f_job_open USING uv_name TYPE  btcjob
                 CHANGING cv_count TYPE btcjobcnt.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = uv_name
    IMPORTING
      jobcount         = cv_count
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_job_close
*&---------------------------------------------------------------------*
FORM f_job_close USING uv_name TYPE btcjob
              CHANGING cv_count TYPE btcjobcnt.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount  = cv_count
      jobname   = uv_name
      strtimmed = abap_true.

ENDFORM.
