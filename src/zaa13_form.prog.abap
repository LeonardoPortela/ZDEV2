*&---------------------------------------------------------------------*
*&  Include           ZAA13_FORM
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
FORM busca_dados.
  DATA: vl_deprec_brl TYPE knafa,
        vl_deprec_usd TYPE knafa.

  DATA: wl_saida LIKE LINE OF tg_saida.
  CLEAR: tg_saida[].

  DATA(vl_ano) = sy-datum+0(4).

  PERFORM busca_zaa007.

  SELECT  DISTINCT
          anla~bukrs,
          anla~anln1,
          anla~anln2,
          anla~txt50,
          anla~txa50,
          anla~zugdt,
          anla~deakt,
          anlz~werks,
          anlz~kostl
    FROM anla
    INNER JOIN anlz ON anla~bukrs EQ anlz~bukrs
                   AND anla~anln1 EQ anlz~anln1
                   AND anla~anln2 EQ anlz~anln2
    INTO TABLE @DATA(tg_imobilizado)
    WHERE anla~bukrs IN @p_bukrs[]
      AND anla~anln1 IN @p_anln1[]
    AND anla~anln2 IN @p_anln2[] "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA
      AND anlz~werks IN @p_werks.

  IF tg_imobilizado IS NOT INITIAL.
    DELETE tg_imobilizado[] WHERE deakt IS NOT INITIAL.


    IF ( tg_imobilizado[] IS NOT INITIAL ).
      DATA: r_anln1 TYPE RANGE OF anlc-anln1,
            r_anln2 TYPE RANGE OF anlc-anln2.

      LOOP AT tg_imobilizado[] INTO DATA(w_imobilizado).
        DATA(_anln1) = VALUE range_anln1_in_s( sign = 'I'  option = 'EQ' low = w_imobilizado-anln1 ).
        DATA(_anln2) = VALUE range_anln2_in_s( sign = 'I'  option = 'EQ' low = w_imobilizado-anln2 ).
        APPEND _anln1 TO r_anln1[].
        APPEND _anln2 TO r_anln2[].
      ENDLOOP.

      SORT r_anln1[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM r_anln1[] COMPARING low.
      SORT r_anln2[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM r_anln2[] COMPARING low.

      SELECT DISTINCT bukrs, anln1, anln2, afabe, gjahr, kansw, knafa, nafap, nafag
        FROM anlc INTO TABLE @DATA(tg_anlc_brl)
        WHERE bukrs   IN @p_bukrs
          AND anln1   IN @r_anln1
          AND anln2   IN @r_anln2
          AND gjahr   EQ @vl_ano
          AND afabe   EQ '01'.

      SELECT DISTINCT bukrs, anln1, anln2, afabe, gjahr, kansw, knafa, nafap, nafag
       FROM anlc INTO TABLE @DATA(tg_anlc_usd)
       WHERE bukrs   IN @p_bukrs
         AND anln1   IN @r_anln1
         AND anln2   IN @r_anln2
         AND gjahr   EQ @vl_ano
         AND afabe   EQ '41'.

      CHECK tg_anlc_brl[] IS NOT INITIAL.

      LOOP AT tg_imobilizado[] INTO DATA(wl_imobilizado).

        READ TABLE tg_anlc_brl INTO DATA(wl_anlc_brl) WITH KEY bukrs = wl_imobilizado-bukrs
                                                               anln1 = wl_imobilizado-anln1
                                                               anln2 = wl_imobilizado-anln2.

        READ TABLE tg_anlc_usd INTO DATA(wl_anlc_usd) WITH KEY bukrs = wl_imobilizado-bukrs
                                                               anln1 = wl_imobilizado-anln1
                                                               anln2 = wl_imobilizado-anln2.


        MOVE wl_imobilizado-bukrs  TO wl_saida-bukrs.
        MOVE wl_imobilizado-werks  TO wl_saida-werks.
        MOVE wl_imobilizado-anln1  TO wl_saida-anln1.
        MOVE wl_imobilizado-anln2  TO wl_saida-anln2.
        MOVE wl_imobilizado-anln2  TO wl_saida-anln2.
        MOVE wl_imobilizado-txt50  TO wl_saida-txt50.
        MOVE wl_imobilizado-txa50  TO wl_saida-txa50.
        MOVE wl_imobilizado-kostl  TO wl_saida-kostl.
        MOVE wl_anlc_brl-kansw     TO wl_saida-vlr_aq_brl.
        MOVE wl_anlc_usd-kansw     TO wl_saida-vlr_aq_usd.

        wl_saida-deprec_brl   = ( wl_anlc_brl-knafa + wl_anlc_brl-nafag ).
        wl_saida-deprec_usd   = ( wl_anlc_usd-knafa + wl_anlc_usd-nafag ).

        IF ( wl_saida-deprec_brl LT 0 ).
          vl_deprec_brl = ( wl_saida-deprec_brl * -1 ).
          wl_saida-vlr_contabil_brl = ( wl_saida-vlr_aq_brl - vl_deprec_brl ).
        ELSE.
          wl_saida-vlr_contabil_brl = ( wl_saida-vlr_aq_brl - wl_saida-deprec_brl ).
        ENDIF.

        IF ( wl_saida-deprec_usd LT 0 ).
          vl_deprec_usd = ( wl_saida-deprec_usd * -1 ).
          wl_saida-vlr_contabil_usd = ( wl_saida-vlr_aq_usd - vl_deprec_usd ).
        ELSE.
          wl_saida-vlr_contabil_usd = ( wl_saida-vlr_aq_usd - wl_saida-deprec_usd ).
        ENDIF.

        APPEND wl_saida TO tg_saida[].

        CLEAR: wl_imobilizado, wl_anlc_brl, wl_anlc_usd, wl_saida, vl_deprec_brl, vl_deprec_usd.
      ENDLOOP.

      IF ( tg_saida[] IS NOT INITIAL ) AND ( tg_saida_aux[] IS NOT INITIAL ).

        LOOP AT tg_saida_aux[] INTO DATA(wl_saida_aux).

          LOOP AT tg_saida[] ASSIGNING <wg_saida> WHERE bukrs EQ wl_saida_aux-bukrs
                                                    AND werks EQ wl_saida_aux-werks
                                                    AND anln1 EQ wl_saida_aux-anln1.
            "Atualiza na tabela de saída os imobilizados que já estão na ZAA007.
            IF ( sy-subrc = 0 ).

              MOVE-CORRESPONDING wl_saida_aux TO <wg_saida>.

            ENDIF.

          ENDLOOP.

        ENDLOOP.

      ELSEIF ( tg_saida[] IS INITIAL ) AND ( tg_saida_aux[] IS NOT INITIAL )  .

        LOOP AT tg_saida_aux[] INTO wl_saida_aux.
          APPEND wl_saida_aux TO tg_saida[].
        ENDLOOP.

      ENDIF.

      IF ( tg_saida[] IS INITIAL ).
        MESSAGE 'Sem resultado para os parâmetros informados!' TYPE 'I'.
        EXIT.
      ENDIF.

    ELSE.
      MESSAGE 'Sem resultado para os parâmetros informados!' TYPE 'I'.
      STOP. "EXIT.
    ENDIF.

    CLEAR: tg_imobilizado[].
  ELSE.
    MESSAGE 'Sem resultado para os parâmetros informados!' TYPE 'I'.
    STOP. "EXIT.
  ENDIF.

ENDFORM. "Form  BUSCA_DADOS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_ZAA007
*&---------------------------------------------------------------------*
*  Busca por fluxos já iniciados gravados na ZAA007.
*----------------------------------------------------------------------*
FORM busca_zaa007.

  IF ( tg_saida[] IS NOT INITIAL ).

    SELECT DISTINCT * FROM zaa007 INTO TABLE @DATA(t_za007)
      FOR ALL ENTRIES IN @tg_saida[]
      WHERE bukrs EQ @tg_saida-bukrs
        AND werks EQ @tg_saida-werks
        AND anln1 EQ @tg_saida-anln1.

    IF ( sy-subrc = 0 ).
      tg_saida_aux[] = CORRESPONDING #( t_za007[] ).
      PERFORM prepara_saida.
    ENDIF.

  ELSE.

    SELECT DISTINCT * FROM zaa007 INTO TABLE t_za007
      WHERE bukrs IN p_bukrs
        AND werks IN p_werks
        AND anln1 IN p_anln1
      AND anln2 IN p_anln2. "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA

    IF ( sy-subrc = 0 ).
      tg_saida_aux[] = CORRESPONDING #( t_za007[] ).
    ENDIF.

  ENDIF.

ENDFORM. "Form  BUSCA_ZAA007


*&---------------------------------------------------------------------*
*&      Form  PREPARA_SAIDA
*&---------------------------------------------------------------------*
FORM prepara_saida.

  DATA: lt_celltab TYPE lvc_t_styl,
        vl_obj_key TYPE sibflporb-instid,
        tl_anexos  TYPE TABLE OF bdn_con.

  DATA(vl_ano) = sy-datum+0(4).

  IF ( tg_saida_aux[] IS NOT INITIAL ).
    LOOP AT tg_saida_aux INTO DATA(wl_saida_aux).

      DELETE tg_saida[] WHERE bukrs EQ wl_saida_aux-bukrs
                          AND werks EQ wl_saida_aux-werks
                          AND anln1 EQ wl_saida_aux-anln1.

      APPEND wl_saida_aux TO tg_saida[].

    ENDLOOP.

  ENDIF.

  CHECK ( tg_saida[] IS NOT INITIAL ).
  CLEAR: lt_celltab[].

  LOOP AT tg_saida[] ASSIGNING <wg_saida>.

    vl_obj_key = |ZAA18{ <wg_saida>-anln1 }{ <wg_saida>-werks }{ vl_ano }|.

    "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA
*    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
*      EXPORTING
*        classname          = 'ZAA13'
*        objkey             = vl_obj_key
*        client             = sy-mandt
*      TABLES
*        gos_connections    = tl_anexos
*      EXCEPTIONS
*        no_objects_found   = 1
*        internal_error     = 2
*        internal_gos_error = 3
*        OTHERS             = 4.

    "IF ( tl_anexos[] IS NOT INITIAL ).
    SELECT SINGLE * FROM srgbtbrel WHERE instid_a = @vl_obj_key AND typeid_a = 'ZAA13' AND reltype = 'ATTA' INTO @DATA(WA_srgbtbrel).

    IF sy-subrc = 0.
      <wg_saida>-anexo = '@1E@'.
      <wg_saida>-check_anexo = 'X'.
    ELSE.
      <wg_saida>-anexo = '@1F@'.
      <wg_saida>-check_anexo = ''.
    ENDIF.

    PERFORM fill_celltab USING: 'B' 'ESTADO_BEM'  CHANGING lt_celltab,
                                'B' 'RESPONSAVEL' CHANGING lt_celltab,
                                'B' 'ANEXO'       CHANGING lt_celltab.


    IF ( <wg_saida>-cellstyles[] IS INITIAL ).
      INSERT LINES OF lt_celltab INTO TABLE <wg_saida>-cellstyles.
    ELSE.
      LOOP AT lt_celltab[] ASSIGNING FIELD-SYMBOL(<lfs_celltab>).
        DELETE <wg_saida>-cellstyles[] WHERE fieldname = <lfs_celltab>-fieldname.
        APPEND <lfs_celltab> TO <wg_saida>-cellstyles[].
      ENDLOOP.
    ENDIF.


  ENDLOOP.

ENDFORM. "Form  PREPARA_SAIDA

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
FORM fill_celltab  USING    VALUE(p_mode)      TYPE char1
                            VALUE(p_fieldname) TYPE lvc_fname
                   CHANGING p_lt_celltab       TYPE lvc_t_styl.


  DATA: ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  IF p_mode EQ 'D'. "Desbloquear Edição
    l_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSEIF p_mode EQ 'B'. "Bloquear Edição
    l_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  IF p_fieldname EQ 'ANEXO'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_button.
  ELSE.
    ls_celltab-style = l_mode.
  ENDIF.

  ls_celltab-fieldname = p_fieldname.

  INSERT ls_celltab INTO TABLE p_lt_celltab.


ENDFORM. "Form  FILL_CELLTAB

*&---------------------------------------------------------------------*
*&      Form  ENVIA_SOLICITACAO
*&---------------------------------------------------------------------*

FORM envia_solicitacao .
  DATA: tl_envia TYPE TABLE OF zaa007.

  LOOP AT tg_envia[] ASSIGNING <wg_saida>.

    <wg_saida>-dt_solicitacao = sy-datum.
    <wg_saida>-solicitante    = sy-uname.
    <wg_saida>-data_atual     = sy-datum.
    <wg_saida>-hora_atual     = sy-uzeit.

    IF ( <wg_saida>-anln2 IS INITIAL ).
      CLEAR <wg_saida>-anln2.
    ENDIF.

    UPDATE zaa008
    SET dt_estorno    = sy-datum
        hr_estorno    = sy-uzeit
        user_estorno  = sy-uname
    WHERE bukrs EQ <wg_saida>-bukrs
      AND werks EQ <wg_saida>-werks
      AND anln1 EQ <wg_saida>-anln1
      AND anln2 EQ <wg_saida>-anln2.

  ENDLOOP.

  MOVE-CORRESPONDING tg_envia[] TO tl_envia[].

  MODIFY zaa007 FROM TABLE tl_envia[].
  COMMIT WORK.

  IF ( sy-subrc EQ 0 ).
    MESSAGE 'Solicitação de baixa encaminhada com sucesso!' TYPE 'S'.
  ENDIF.

ENDFORM. "Form  ENVIA_SOLICITACAO


*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_REPORT
*&---------------------------------------------------------------------*
FORM z_hotspot_report  USING    p_e_row_id
                                p_e_column_id
                                p_es_row_no.

  READ TABLE tg_saida[] INTO DATA(wl_saida) INDEX p_e_row_id.

  SET PARAMETER ID 'AN1' FIELD wl_saida-anln1.
  IF ( wl_saida-anln2 IS NOT INITIAL ).
    SET PARAMETER ID 'AN2' FIELD wl_saida-anln2.
  ENDIF.
  SET PARAMETER ID 'BUK' FIELD wl_saida-bukrs.
  CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.

ENDFORM.
