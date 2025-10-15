*----------------------------------------------------------------------*
***INCLUDE LZSD_ESTRATEGIA_LIBF01.
*----------------------------------------------------------------------*
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
    INTO TABLE @gt_checklist
      WHERE doc_simulacao = @uv_simu.

*  ENDIF.

  READ TABLE gt_checklist TRANSPORTING NO FIELDS
   WITH KEY doc_simulacao = uv_simu
            ativo = abap_true.

  IF sy-subrc NE 0.
    cv_erro = '1'.
    EXIT.
  ENDIF.

  READ TABLE gt_checklist TRANSPORTING NO FIELDS
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

  READ TABLE gt_checklist TRANSPORTING NO FIELDS
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
*& Form f_check_valor_estrat
*&---------------------------------------------------------------------*
FORM f_check_valor_estrat TABLES t_ordens STRUCTURE zsd_ord_vendas_est
                                 t_estra STRUCTURE zsd_estrategia_ov.

  DATA lv_valor TYPE c LENGTH 50.

  DATA(lv_existe) = abap_false.

  LOOP AT t_ordens ASSIGNING FIELD-SYMBOL(<fs_ordens>).

    READ TABLE t_estra TRANSPORTING NO FIELDS
      WITH KEY vbeln = <fs_ordens>-vbeln
               seq = <fs_ordens>-seq.

    CHECK sy-subrc EQ 0.

    lv_existe = abap_false.

    LOOP AT t_estra ASSIGNING FIELD-SYMBOL(<fs_estrat>) WHERE vbeln = <fs_ordens>-vbeln
        AND seq = <fs_ordens>-seq.

      IF <fs_estrat>-valor_ate >= <fs_ordens>-vlr_acumulado.
        lv_existe = abap_true.
      ENDIF.

    ENDLOOP.

    IF lv_existe = abap_false.

      WRITE <fs_ordens>-vlr_acumulado TO lv_valor CURRENCY 'USD' LEFT-JUSTIFIED.

      <fs_ordens>-msgty = 'E'.
      <fs_ordens>-msgx = `Não há estratégia para o valor acumulado: ` && lv_valor && `. Procure o departamento responsável`.

    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_cria_nova_linha
*&---------------------------------------------------------------------*
FORM f_cria_nova_linha USING uv_vbeln TYPE vbeln_va
                             uv_teste TYPE flag
                             uv_status TYPE zsde_status_wf
                             uv_origem TYPE zsde_saldo_origem
                             uv_valor TYPE netwr_ap
                        CHANGING ct_116 TYPE zsds_zsdt0116_tab
                                 ct_mess TYPE bapiret2_tab.

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

ENDFORM.
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
FORM f_get_text USING uv_title CHANGING cv_texto TYPE c.

  DATA lv_code TYPE c.

  DATA lt_fields  TYPE TABLE OF sval.

  lt_fields =
    VALUE #( ( tabname = 'ZSDT0116' fieldname = 'JUST_WORKFLOW' field_obl = '' fieldtext = uv_title value =  cv_texto )  ).

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = sy-title
    IMPORTING
      returncode      = lv_code
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  CHECK lv_code IS INITIAL AND lt_fields[] IS NOT INITIAL.

  LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_field>) WHERE value IS NOT INITIAL.

    CASE <fs_field>-fieldname.
      WHEN 'JUST_WORKFLOW'.
        cv_texto = <fs_field>-value.
    ENDCASE.

  ENDLOOP.


ENDFORM.
