*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_dados.

  SELECT zsdt0330~*,
         t001w~name1
    FROM zsdt0330
    INNER JOIN t001w ON t001w~werks = zsdt0330~werks
    INTO TABLE @t_zsdt0330
   WHERE zsdt0330~werks      IN @s_werks
     AND zsdt0330~data_carga IN @s_data
     AND cancelado            = @abap_false.

ENDFORM.

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_fardos.

  SELECT zsdt0330~*,
         t001w~name1
    FROM zsdt0330
    INNER JOIN t001w ON t001w~werks = zsdt0330~werks
    INTO TABLE @t_zsdt0330
   WHERE zsdt0330~werks          IN @s_werks
     AND zsdt0330~data_carga     IN @s_data
     AND zsdt0330~id_carga       IN @s_id
     AND zsdt0330~safra          IN @s_safra
     AND zsdt0330~status_estorno IN (@abap_off,'I')
     AND cancelado                = @abap_false.

  CHECK t_zsdt0330[] IS NOT INITIAL.

  t_zsdt0330_aux[] = t_zsdt0330[].
  SORT t_zsdt0330_aux BY id_carga.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0330_aux
                        COMPARING id_carga.

  SELECT *
    FROM zsdt0327
    INTO TABLE t_zsdt0327
     FOR ALL ENTRIES IN t_zsdt0330_aux
   WHERE id_carga     = t_zsdt0330_aux-id_carga
     AND tipo_integra = 'FB'.

  SELECT *
    FROM zsdt0331
    INTO TABLE t_zsdt0331
     FOR ALL ENTRIES IN t_zsdt0330
   WHERE id_carga     = t_zsdt0330-id_carga
     AND cancelado    = abap_false.

  IF p_suce = abap_false.
    DELETE t_zsdt0330 WHERE status_fardo = '3'.
  ENDIF.

  IF p_erro = abap_false.
    DELETE t_zsdt0330 WHERE status_fardo = '2'.
  ENDIF.

  IF p_agua = abap_false.
    DELETE t_zsdt0330 WHERE status_fardo = '0' OR
                            status_fardo = '1'.
  ENDIF.

  SORT t_zsdt0327 BY id_carga matnr werks lgort acharg safra seq DESCENDING.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0327
                        COMPARING id_carga matnr werks lgort acharg safra.

ENDFORM.

**********************************************************************
* selecao lotes gerados
**********************************************************************
FORM f_selecao_lotes.

  SELECT zsdt0330~*,
         t001w~name1
    FROM zsdt0330
    INNER JOIN t001w ON t001w~werks = zsdt0330~werks
    INTO TABLE @t_zsdt0330
   WHERE zsdt0330~werks          IN @s_werks
     AND zsdt0330~data_carga     IN @s_data
     AND zsdt0330~id_carga       IN @s_id
     AND zsdt0330~safra          IN @s_safra
     AND zsdt0330~status_estorno IN (@abap_off,'I')
     AND cancelado                = @abap_false.

  CHECK t_zsdt0330[] IS NOT INITIAL.

  t_zsdt0330_aux[] = t_zsdt0330[].
  SORT t_zsdt0330_aux BY id_carga.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0330_aux
                        COMPARING id_carga.

  SELECT *
    FROM zsdt0327
    INTO TABLE t_zsdt0327
     FOR ALL ENTRIES IN t_zsdt0330_aux
   WHERE id_carga     = t_zsdt0330_aux-id_carga
     AND tipo_integra = 'GL'.

  SELECT *
    FROM zmmt0008
    INTO TABLE t_zmmt0008
     FOR ALL ENTRIES IN t_zsdt0330
   WHERE werks        = t_zsdt0330-werks
     AND lgort        = t_zsdt0330-lgort
     AND charg        = t_zsdt0330-acharg
     AND vbeln        = t_zsdt0330-vbeln
     AND nr_romaneio  = t_zsdt0330-nr_romaneio.

  IF p_suce = abap_false.
    DELETE t_zsdt0330 WHERE status_gera_lote = '3'.
  ENDIF.

  IF p_erro = abap_false.
    DELETE t_zsdt0330 WHERE status_gera_lote = '2'.
  ENDIF.

  IF p_agua = abap_false.
    DELETE t_zsdt0330 WHERE status_gera_lote = '0' OR
                            status_gera_lote = '1'.
  ENDIF.

  SORT t_zsdt0327 BY id_carga matnr werks lgort acharg safra seq DESCENDING.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0327
                        COMPARING id_carga matnr werks lgort acharg safra.

ENDFORM.

**********************************************************************
* selecao lotes gerados
**********************************************************************
FORM f_selecao_estorno.

*-----comentado #129705-20.12.2023-JT-inicio
  SELECT zsdt0330~*,
         t001w~name1
    FROM zsdt0330
    INNER JOIN t001w ON t001w~werks = zsdt0330~werks
    INTO TABLE @t_zsdt0330
   WHERE zsdt0330~werks          IN @s_werks
     AND zsdt0330~data_carga     IN @s_data
     AND zsdt0330~id_carga       IN @s_id
     AND zsdt0330~safra          IN @s_safra
     AND zsdt0330~status_estorno IN ('I','D')
     AND cancelado                = @abap_false.

  CHECK t_zsdt0330[] IS NOT INITIAL.

  t_zsdt0330_aux[] = t_zsdt0330[].
  SORT t_zsdt0330_aux BY id_carga.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0330_aux
                        COMPARING id_carga.

  SELECT *
    FROM zsdt0327
    INTO TABLE t_zsdt0327
     FOR ALL ENTRIES  IN t_zsdt0330_aux
   WHERE id_carga      = t_zsdt0330_aux-id_carga
     AND tipo_integra IN ('FB','ES').

  IF p_suce = abap_false.
    DELETE t_zsdt0330 WHERE status_gera_lote = '3'.
  ENDIF.

  IF p_erro = abap_false.
    DELETE t_zsdt0330 WHERE status_gera_lote = '2'.
  ENDIF.

  IF p_agua = abap_false.
    DELETE t_zsdt0330 WHERE status_gera_lote = '0' OR
                            status_gera_lote = '1'.
  ENDIF.

  SORT t_zsdt0327 BY id_carga matnr werks lgort acharg safra seq DESCENDING.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0327
                        COMPARING id_carga matnr werks lgort acharg safra.
*-----comentado #129705-20.12.2023-JT-fim

ENDFORM.

**********************************************************************
* selecao notas fiscais
**********************************************************************
FORM f_selecao_notas_fiscais.

  SELECT zsdt0330~*,
         t001w~name1
    FROM zsdt0330
    INNER JOIN t001w ON t001w~werks = zsdt0330~werks
    INTO TABLE @t_zsdt0330
   WHERE zsdt0330~werks          IN @s_werks
     AND zsdt0330~data_carga     IN @s_data
     AND zsdt0330~docnum         IN @s_docnum
     AND zsdt0330~status_estorno IN (@abap_off,'I')
     AND cancelado                = @abap_false.

  DELETE t_zsdt0330 WHERE docnum           IS INITIAL
                      AND docnum_cancelado IS INITIAL.

  CHECK t_zsdt0330[] IS NOT INITIAL.

  SORT t_zsdt0330 BY docnum.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0330
                        COMPARING docnum.

  t_zsdt0330_aux[] = t_zsdt0330[].
  SORT t_zsdt0330_aux BY id_carga.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0330_aux
                        COMPARING id_carga.

  SELECT *
    FROM zsdt0327
    INTO TABLE t_zsdt0327
     FOR ALL ENTRIES IN t_zsdt0330_aux
   WHERE id_carga     = t_zsdt0330_aux-id_carga
     AND tipo_integra = 'NF'.

  SORT t_zsdt0327 BY id_carga matnr werks lgort acharg safra seq DESCENDING.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0327
                        COMPARING id_carga matnr werks lgort acharg safra.

ENDFORM.

FORM f_selecao_ganho_perda_peso.


  SELECT *
    FROM zsdt0344 AS a
    INTO TABLE t_zsdt0344
   WHERE dt_registro  IN s_data
     AND nr_romaneio  IN s_nr_rom
     AND EXISTS (  SELECT id_carga
                     FROM zsdt0330 AS b
                      WHERE b~id_carga = a~id_carga
                        AND b~werks    IN s_werks
                        AND b~safra    IN s_safra ).


ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_dados.

  DATA: l_tot_qtd_penden TYPE i,
        l_tot_qtd_emproc TYPE i,
        l_tot_qtd_proces TYPE i.

  FREE: t_alv.

  LOOP AT t_zsdt0330 INTO w_zsdt0330.

    CLEAR w_alv.

    IF w_zsdt0330-status_estorno = abap_off.
      w_alv-status = 'Transferência'.

      IF     w_zsdt0330-status_fardo = '0'.
        w_alv-qtd_penden     = 1.
        w_alv-total          = 1.
        l_tot_qtd_penden     = l_tot_qtd_penden + 1.
      ELSEIF w_zsdt0330-status_fardo = '1'.
        w_alv-qtd_emproc     = 1.
        w_alv-total          = 1.
        l_tot_qtd_emproc     = l_tot_qtd_emproc + 1.
      ELSEIF w_zsdt0330-status_fardo = '2'.
        w_alv-qtd_proces_err = 1.
        w_alv-total          = 1.
        l_tot_qtd_proces     = l_tot_qtd_proces + 1.
      ELSEIF w_zsdt0330-status_fardo = '3'.
        w_alv-qtd_proces_suc = 1.
        w_alv-total          = 1.
        l_tot_qtd_proces     = l_tot_qtd_proces + 1.
      ENDIF.

      w_alv-werks            = w_zsdt0330-werks.
      w_alv-name1            = w_zsdt0330-name1.

      COLLECT w_alv  INTO t_alv.
    ENDIF.

*------------------------------------
    CLEAR w_alv.

    IF w_zsdt0330-status_estorno = 'I'.
      w_alv-status = 'Transferência (Adicionados)'.

      IF     w_zsdt0330-status_fardo = '0'.
        w_alv-qtd_penden     = 1.
        w_alv-total          = 1.
        l_tot_qtd_penden     = l_tot_qtd_penden + 1.
      ELSEIF w_zsdt0330-status_fardo = '1'.
        w_alv-qtd_emproc     = 1.
        w_alv-total          = 1.
        l_tot_qtd_emproc     = l_tot_qtd_emproc + 1.
      ELSEIF w_zsdt0330-status_fardo = '2'.
        w_alv-qtd_proces_err = 1.
        w_alv-total          = 1.
        l_tot_qtd_proces     = l_tot_qtd_proces + 1.
      ELSEIF w_zsdt0330-status_fardo = '3'.
        w_alv-qtd_proces_suc = 1.
        w_alv-total          = 1.
        l_tot_qtd_proces     = l_tot_qtd_proces + 1.
      ENDIF.

      w_alv-werks            = w_zsdt0330-werks.
      w_alv-name1            = w_zsdt0330-name1.

      COLLECT w_alv  INTO t_alv.
    ENDIF.

*------------------------------------
    CLEAR w_alv.

    IF w_zsdt0330-status_estorno = abap_off.
      w_alv-status = 'Geração de Lotes'.

      IF     w_zsdt0330-status_gera_lote = '0'.
        w_alv-qtd_penden     = 1.
        w_alv-total          = 1.
        l_tot_qtd_penden     = l_tot_qtd_penden + 1.
      ELSEIF w_zsdt0330-status_gera_lote = '1'.
        w_alv-qtd_emproc     = 1.
        w_alv-total          = 1.
        l_tot_qtd_emproc     = l_tot_qtd_emproc + 1.
      ELSEIF w_zsdt0330-status_gera_lote = '2'.
        w_alv-qtd_proces_err = 1.
        w_alv-total          = 1.
        l_tot_qtd_proces     = l_tot_qtd_proces + 1.
      ELSEIF w_zsdt0330-status_gera_lote = '3'.
        w_alv-qtd_proces_suc = 1.
        w_alv-total          = 1.
        l_tot_qtd_proces     = l_tot_qtd_proces + 1.
      ENDIF.

      w_alv-werks            = w_zsdt0330-werks.
      w_alv-name1            = w_zsdt0330-name1.

      COLLECT w_alv  INTO t_alv.
    ENDIF.

*------------------------------------
    CLEAR w_alv.

    IF w_zsdt0330-status_estorno = 'I'.
      w_alv-status = 'Geração de Lotes (Adicionados)'.

      IF     w_zsdt0330-status_gera_lote = '0'.
        w_alv-qtd_penden     = 1.
        w_alv-total          = 1.
        l_tot_qtd_penden     = l_tot_qtd_penden + 1.
      ELSEIF w_zsdt0330-status_gera_lote = '1'.
        w_alv-qtd_emproc     = 1.
        w_alv-total          = 1.
        l_tot_qtd_emproc     = l_tot_qtd_emproc + 1.
      ELSEIF w_zsdt0330-status_gera_lote = '2'.
        w_alv-qtd_proces_err = 1.
        w_alv-total          = 1.
        l_tot_qtd_proces     = l_tot_qtd_proces + 1.
      ELSEIF w_zsdt0330-status_gera_lote = '3'.
        w_alv-qtd_proces_suc = 1.
        w_alv-total          = 1.
        l_tot_qtd_proces     = l_tot_qtd_proces + 1.
      ENDIF.

      w_alv-werks            = w_zsdt0330-werks.
      w_alv-name1            = w_zsdt0330-name1.

      COLLECT w_alv  INTO t_alv.
    ENDIF.

*------------------------------------
    CLEAR w_alv.

    IF w_zsdt0330-status_estorno = 'D'.
      w_alv-status = 'Estorno Fardinhos'.

      IF     w_zsdt0330-status_fardo = '0'.
        w_alv-qtd_penden     = 1.
        w_alv-total          = 1.
        l_tot_qtd_penden     = l_tot_qtd_penden + 1.
      ELSEIF w_zsdt0330-status_fardo = '1'.
        w_alv-qtd_emproc     = 1.
        w_alv-total          = 1.
        l_tot_qtd_emproc     = l_tot_qtd_emproc + 1.
      ELSEIF w_zsdt0330-status_fardo = '2'.
        w_alv-qtd_proces_err = 1.
        w_alv-total          = 1.
        l_tot_qtd_proces     = l_tot_qtd_proces + 1.
      ELSEIF w_zsdt0330-status_fardo = '3'.
        w_alv-qtd_proces_suc = 1.
        w_alv-total          = 1.
        l_tot_qtd_proces     = l_tot_qtd_proces + 1.
      ENDIF.

      w_alv-werks            = w_zsdt0330-werks.
      w_alv-name1            = w_zsdt0330-name1.

      COLLECT w_alv  INTO t_alv.
    ENDIF.

  ENDLOOP.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_fardos.

  FREE: t_alv_stat.

  LOOP AT t_zsdt0330 INTO w_zsdt0330.

    CLEAR: w_zsdt0327, w_alv_stat.

    MOVE-CORRESPONDING w_zsdt0330  TO w_alv_stat.

    READ TABLE t_zsdt0327 INTO w_zsdt0327  WITH KEY id_carga = w_zsdt0330-id_carga
                                                    matnr    = w_zsdt0330-matnr
                                                    werks    = w_zsdt0330-werks
                                                    lgort    = w_zsdt0330-lgort
                                                    acharg   = w_zsdt0330-acharg
                                                    safra    = w_zsdt0330-safra.
    IF sy-subrc <> 0.
      READ TABLE t_zsdt0331 INTO w_zsdt0331  WITH KEY id_carga = w_zsdt0330-id_carga
                                                      matnr    = w_zsdt0330-matnr
                                                      werks    = w_zsdt0330-werks
                                                      lgort    = w_zsdt0330-lgort
                                                      safra    = w_zsdt0330-safra.
      IF sy-subrc = 0.
        w_alv_stat-status_trace = icon_led_yellow.
      ELSE.
        w_alv_stat-status_trace = icon_dummy.
      ENDIF.
    ELSEIF w_zsdt0327-tipo_msg = 'E'.
      w_alv_stat-status_trace = icon_led_red.
    ELSEIF w_zsdt0327-tipo_msg = 'S'.
      w_alv_stat-status_trace = icon_led_green.
    ELSE.
      w_alv_stat-status_trace = icon_dummy.
    ENDIF.

    IF w_alv_stat-status_fardo = '1'.
      w_alv_stat-status_trace = icon_activity.
    ENDIF.

    APPEND w_alv_stat             TO t_alv_stat.
  ENDLOOP.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_estorno.

*-----comentado #129705-20.12.2023-JT-inicio
  FREE: t_alv_esto.

  LOOP AT t_zsdt0330 INTO w_zsdt0330.

    CLEAR: w_zsdt0327, w_alv_esto.

    MOVE-CORRESPONDING w_zsdt0330  TO w_alv_esto.

    READ TABLE t_zsdt0327 INTO w_zsdt0327  WITH KEY id_carga = w_zsdt0330-id_carga
                                                    matnr    = w_zsdt0330-matnr
                                                    werks    = w_zsdt0330-werks
                                                    lgort    = w_zsdt0330-lgort
                                                    acharg   = w_zsdt0330-acharg
                                                    safra    = w_zsdt0330-safra.
    IF sy-subrc <> 0.
      READ TABLE t_zsdt0331 INTO w_zsdt0331  WITH KEY id_carga = w_zsdt0330-id_carga
                                                      matnr    = w_zsdt0330-matnr
                                                      werks    = w_zsdt0330-werks
                                                      lgort    = w_zsdt0330-lgort
                                                      safra    = w_zsdt0330-safra.
      IF sy-subrc = 0.
        w_alv_esto-status_trace = icon_led_yellow.
      ELSE.
        w_alv_esto-status_trace = icon_dummy.
      ENDIF.
    ELSEIF w_zsdt0327-tipo_msg = 'E'.
      w_alv_esto-status_trace = icon_led_red.
    ELSEIF w_zsdt0327-tipo_msg = 'S'.
      w_alv_esto-status_trace = icon_led_green.
    ELSE.
      w_alv_esto-status_trace = icon_dummy.
    ENDIF.

    IF w_alv_esto-status_fardo = '1'.
      w_alv_esto-status_trace = icon_activity.
    ENDIF.

    w_alv_esto-desc_estorno   = COND #( WHEN w_zsdt0330-status_estorno = 'I' THEN 'Fardo Adicionado'
                                        WHEN w_zsdt0330-status_estorno = 'D' THEN 'Fardo Eliminado' ).

    APPEND w_alv_esto             TO t_alv_esto.
  ENDLOOP.
*-----comentado #129705-20.12.2023-JT-fim

ENDFORM.

**********************************************************************
* reprocessar fardos
**********************************************************************
FORM f_reprocessa_fardos.

  FREE: t_rows, l_erro.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione ao menos uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT t_rows INTO w_rows.

    READ TABLE t_alv_stat INTO w_alv_stat INDEX w_rows-index.

    CHECK sy-subrc = 0.

    IF w_alv_stat-status_fardo <> '0' AND
       w_alv_stat-status_fardo <> '2'.
      l_erro = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF l_erro = abap_true.
    MESSAGE s024(sd) WITH 'Há Fardos escolhidos qua ja estão Processados.' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE t_rows LINES DATA(l_lines).

  DATA(l_proce) = 0.

*----------------------------------------------------
* reenvio
*----------------------------------------------------
  LOOP AT t_rows INTO w_rows.

    READ TABLE t_alv_stat INTO w_alv_stat INDEX w_rows-index.
    l_tabix = sy-tabix.

    CHECK sy-subrc = 0.

    l_proce = l_proce + 1.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = |Reprocessando { l_lines } / { l_proce }|.

    DATA(l_task) = 'TRACE_TRANSFER' && w_alv_stat-id_carga && w_alv_stat-matnr && w_alv_stat-werks && w_alv_stat-lgort
                                    && w_alv_stat-acharg   && w_alv_stat-safra.

    CALL FUNCTION 'ZSD_REPROCESSA_TRANSFERENCIA' STARTING NEW TASK l_task
      EXPORTING
        i_id_carga = w_alv_stat-id_carga
        i_matnr    = w_alv_stat-matnr
        i_werks    = w_alv_stat-werks
        i_lgort    = w_alv_stat-lgort
        i_acharg   = w_alv_stat-acharg
        i_safra    = w_alv_stat-safra
      EXCEPTIONS
        OTHERS     = 1.

    w_alv_stat-status_trace = icon_activity.
    MODIFY t_alv_stat    FROM w_alv_stat INDEX w_rows-index TRANSPORTING status_trace.
  ENDLOOP.

ENDFORM.

**********************************************************************
* reprocessar fardos
**********************************************************************
FORM f_reprocessa_lotes USING p_ok_code.

  FREE: t_rows, l_erro1, l_erro2, l_erro3.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione ao menos uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT t_rows INTO w_rows.

    READ TABLE t_alv_lote INTO w_alv_lote INDEX w_rows-index.

    CHECK sy-subrc = 0.

    IF w_alv_lote-status_fardo <> '3'.
      l_erro1 = abap_true.
      EXIT.
    ENDIF.

    IF p_ok_code = '&GERALOTE'.
      IF w_alv_lote-status_gera_lote <> '0' AND
         w_alv_lote-status_gera_lote <> '2'.
        l_erro2 = abap_true.
        EXIT.
      ENDIF.
    ENDIF.

    IF p_ok_code = '&EMAIL' OR p_ok_code = '&INTEGRAR'.
      IF w_alv_lote-status_gera_lote <> '3'.
        l_erro3 = abap_true.
        EXIT.
      ENDIF.

    ENDIF.

  ENDLOOP.

  IF l_erro1 = abap_true.
    MESSAGE s024(sd) WITH 'Fardo escolhido não está pronto para Processamento!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF l_erro2 = abap_true.
    MESSAGE s024(sd) WITH 'Fardo escolhido não está Pendente de Processamento!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF l_erro3 = abap_true.
    MESSAGE s024(sd) WITH 'Fardo escolhido ainda não foi Processado!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE t_rows LINES DATA(l_lines).

  DATA(l_proce) = 0.

*----------------------------------------------------
* reenvio
*----------------------------------------------------
  LOOP AT t_rows INTO w_rows.

    READ TABLE t_alv_lote INTO w_alv_lote INDEX w_rows-index.
    l_tabix = sy-tabix.

    CHECK sy-subrc = 0.

    l_proce = l_proce + 1.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = |Reprocessando { l_lines } / { l_proce }|.

    DATA(l_task) = 'TRACE_TRANSFER' && w_alv_lote-id_carga && w_alv_lote-matnr && w_alv_lote-werks && w_alv_lote-lgort
                                    && w_alv_lote-acharg   && w_alv_lote-safra.

    CALL FUNCTION 'ZSD_REPROCESSA_GERAR_LOTES' STARTING NEW TASK l_task
      EXPORTING
        i_id_carga = w_alv_lote-id_carga
        i_matnr    = w_alv_lote-matnr
        i_werks    = w_alv_lote-werks
        i_lgort    = w_alv_lote-lgort
        i_acharg   = w_alv_lote-acharg
        i_safra    = w_alv_lote-safra
        i_ucomm    = p_ok_code
      EXCEPTIONS
        OTHERS     = 1.

    w_alv_lote-status_trace   = icon_activity.
    w_alv_lote-status_email   = icon_activity.
    w_alv_lote-status_integra = icon_activity.

    MODIFY t_alv_lote      FROM w_alv_lote INDEX w_rows-index TRANSPORTING status_trace status_email status_integra.
  ENDLOOP.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_lotes.

  FREE: t_alv_lote.

  LOOP AT t_zsdt0330 INTO w_zsdt0330.

    CLEAR: w_zsdt0327, w_zmmt0008, w_alv_lote.

    READ TABLE t_zmmt0008 INTO w_zmmt0008  WITH KEY werks    = w_zsdt0330-werks
                                                    lgort    = w_zsdt0330-lgort
                                                    charg    = w_zsdt0330-acharg
                                                    vbeln    = w_zsdt0330-vbeln.

    READ TABLE t_zsdt0327 INTO w_zsdt0327  WITH KEY id_carga = w_zsdt0330-id_carga
                                                    matnr    = w_zsdt0330-matnr
                                                    werks    = w_zsdt0330-werks
                                                    lgort    = w_zsdt0330-lgort
                                                    acharg   = w_zsdt0330-acharg
                                                    safra    = w_zsdt0330-safra.

    MOVE-CORRESPONDING: w_zmmt0008           TO w_alv_lote,
                        w_zsdt0330           TO w_alv_lote.
    MOVE              : w_zmmt0008-placa_cav TO w_alv_lote-placa_cav,
                        w_zmmt0008-motorista TO w_alv_lote-motorista.

    IF w_zsdt0330-nr_romaneio IS INITIAL.
      MOVE w_zmmt0008-nr_romaneio            TO w_alv_lote-nr_romaneio.
    ENDIF.

    IF     w_zsdt0327-tipo_msg = 'E'.
      w_alv_lote-status_trace = icon_led_red.
    ELSEIF w_zsdt0327-tipo_msg = 'S'.
      w_alv_lote-status_trace = icon_led_green.
    ELSE.
      w_alv_lote-status_trace = icon_dummy.
    ENDIF.

    IF w_zsdt0330-status_gera_lote = '1'.
      w_alv_lote-status_trace = icon_activity.
    ENDIF.

    IF w_zmmt0008-vbeln IS NOT INITIAL.
      IF w_zsdt0330-status_email_gl = abap_true.
        w_alv_lote-status_email = icon_led_green.
      ELSE.
        w_alv_lote-status_email = icon_alert.
      ENDIF.
    ELSE.
      w_alv_lote-status_email = icon_dummy.
    ENDIF.

    IF w_zmmt0008-vbeln IS NOT INITIAL.
      IF w_zsdt0330-status_api_gl = abap_true.
        w_alv_lote-status_integra = icon_led_green.
      ELSE.
        w_alv_lote-status_integra = icon_alert.
      ENDIF.
    ELSE.
      w_alv_lote-status_integra = icon_dummy.
    ENDIF.

    APPEND w_alv_lote              TO t_alv_lote.
  ENDLOOP.

ENDFORM.

**********************************************************************
* processa dados - notas fiscais
**********************************************************************
FORM f_processa_notas_fiscais.

  FREE: t_alv_nota.

  LOOP AT t_zsdt0330 INTO w_zsdt0330.

    CLEAR: w_zsdt0327, w_alv_nota.

    MOVE-CORRESPONDING w_zsdt0330  TO w_alv_nota.

    IF w_zsdt0330-status_nf_enviada = abap_off.
      w_alv_nota-status_nf  = icon_alert.
    ELSE.
      w_alv_nota-status_nf  = icon_led_green.
    ENDIF.

    APPEND w_alv_nota               TO t_alv_nota.
  ENDLOOP.

ENDFORM.

**********************************************************************
* reenvia NF
**********************************************************************
FORM f_reenvia_nota_fiscal.

  FREE: t_rows, l_erro.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione ao menos uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE t_rows LINES DATA(l_lines).

  DATA(l_proce) = 0.

*----------------------------------------------------
* reenvio
*----------------------------------------------------
  LOOP AT t_rows INTO w_rows.

    READ TABLE t_alv_nota INTO w_alv_nota INDEX w_rows-index.
    l_tabix = sy-tabix.

    CHECK sy-subrc = 0.

    l_proce = l_proce + 1.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = |Reenviando { l_lines } / { l_proce }|.

    DATA(l_task) = 'ENVIA_NF_TRACE_COTTON' && w_alv_nota-docnum.

    w_acttab-docnum      = COND #( WHEN w_alv_nota-docnum_cancelado IS NOT INITIAL THEN w_alv_nota-docnum_cancelado
                                                                                   ELSE w_alv_nota-docnum ).
    w_acttab-action_requ = 'C'.
    w_acttab-docsta      = '1'.
    w_acttab-cancel      = COND #( WHEN w_alv_nota-docnum_cancelado IS NOT INITIAL THEN abap_true
                                                                                   ELSE abap_false ).

    CALL FUNCTION 'ZSD_ENVIA_NF_TRACE_COTTON' STARTING NEW TASK l_task
      EXPORTING
        i_acttab = w_acttab.

    w_alv_nota-status_nf    = icon_activity.
    MODIFY t_alv_nota    FROM w_alv_nota INDEX w_rows-index TRANSPORTING status_nf.
  ENDLOOP.

ENDFORM.

**********************************************************************
* reenvio ordem venda
**********************************************************************
FORM f_selecao_reenvio_ov.

  SELECT *
    FROM zsdt0213_integra
    INTO TABLE t_zsdt0213_int
   WHERE vbeln       IN s_vbeln
     AND data_reg    IN s_dataov
     AND nro_sol_ov  IN s_solic
     AND integrado    = abap_false.

ENDFORM.

**********************************************************************
* processa dados - notas fiscais
**********************************************************************
FORM f_processa_reenvio_ov.

  FREE: t_alv_reenv.

  LOOP AT t_zsdt0213_int            INTO w_zsdt0213_int.
    MOVE-CORRESPONDING w_zsdt0213_int TO w_alv_reenv.
    MOVE icon_led_red                 TO w_alv_reenv-status.

    CASE w_zsdt0213_int-metodo.
      WHEN 'DELETE'.
        w_alv_reenv-descr  = 'Exclusão OV'.
      WHEN 'PUT'.
        w_alv_reenv-descr  = 'Modificação OV'.
      WHEN 'POST'.
        w_alv_reenv-descr  = 'Cadastro OV'.
    ENDCASE.

    APPEND w_alv_reenv                TO t_alv_reenv.
  ENDLOOP.

  SORT t_alv_reenv BY nro_sol_ov
                      vbeln
                      data_reg DESCENDING
                      hora_reg DESCENDING.

ENDFORM.

FORM f_processa_ganho_perda.

  FREE: t_alv_ganho_perda.

  CHECK t_zsdt0344[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0330 INTO TABLE @DATA(lit_zsdt0330)
     FOR ALL ENTRIES IN @t_zsdt0344
    WHERE id_carga EQ @t_zsdt0344-id_carga
      AND seq      EQ @t_zsdt0344-seq_carga.

  DATA(t_zsdt0344_aux) = t_zsdt0344[].
  DELETE t_zsdt0344_aux WHERE mblnr IS INITIAL.
  IF t_zsdt0344_aux[] IS NOT INITIAL.
    SELECT mblnr, mjahr , smbln
      FROM mseg INTO TABLE @DATA(lit_mseg)
       FOR ALL ENTRIES IN @t_zsdt0344_aux
      WHERE smbln EQ @t_zsdt0344_aux-mblnr.
  ENDIF.

  SORT lit_mseg BY smbln.
  SORT lit_zsdt0330 BY id_carga seq.


  LOOP AT t_zsdt0344            INTO DATA(lwa_zsdt0344).
    APPEND INITIAL LINE TO t_alv_ganho_perda ASSIGNING FIELD-SYMBOL(<fs_alv_ganho_perda>).

    READ TABLE lit_zsdt0330 INTO DATA(lwa_zdt0330) WITH KEY id_carga = lwa_zsdt0344-id_carga
                                                            seq      = lwa_zsdt0344-seq_carga BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    MOVE-CORRESPONDING lwa_zsdt0344 TO <fs_alv_ganho_perda>.

    <fs_alv_ganho_perda>-werks = lwa_zdt0330-werks.
    <fs_alv_ganho_perda>-safra = lwa_zdt0330-safra.

    IF lwa_zsdt0344-mblnr IS NOT INITIAL.
      READ TABLE lit_mseg WITH KEY smbln = lwa_zsdt0344-mblnr BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        <fs_alv_ganho_perda>-estornado = abap_true.
      ENDIF.
    ENDIF.

    "MOVE icon_led_red      TO <fs_alv_ganho_perda>-status.

  ENDLOOP.

  SORT t_alv_ganho_perda BY werks
                            nr_romaneio
                            dt_registro DESCENDING
                            hr_registro DESCENDING.

ENDFORM.

**********************************************************************
* reenvia OV
**********************************************************************
FORM f_reenvia_ordem_venda.

  FREE: t_rows, l_erro.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione ao menos uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE t_rows LINES DATA(l_lines).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = |Reenviando...|.

*----------------------------------------------------
* reenvio
*----------------------------------------------------
  LOOP AT t_rows INTO w_rows.

    READ TABLE t_alv_reenv INTO w_alv_reenv INDEX w_rows-index.
    l_tabix = sy-tabix.

    CHECK sy-subrc = 0.

    CASE w_alv_reenv-metodo.
      WHEN 'PUT' OR 'POST'.
        CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE'
          EXPORTING
            i_nro_sol_ov = w_alv_reenv-nro_sol_ov
            i_posnr      = w_alv_reenv-posnr
            i_acao       = 'C'
          EXCEPTIONS
            OTHERS       = 1.

      WHEN 'DELETE'.
        CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE'
          EXPORTING
            i_nro_sol_ov = w_alv_reenv-nro_sol_ov
            i_posnr      = w_alv_reenv-posnr
            i_vbeln      = w_alv_reenv-vbeln
            i_acao       = 'E'
          EXCEPTIONS
            OTHERS       = 1.
    ENDCASE.
  ENDLOOP.

ENDFORM.

FORM f_processar_mov_ganho_perda.

  FREE: t_rows, l_erro.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione ao menos uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE t_rows LINES DATA(l_lines).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = |Processando...|.

*----------------------------------------------------
* reenvio
*----------------------------------------------------
  LOOP AT t_rows INTO w_rows.

    READ TABLE t_alv_ganho_perda INTO DATA(lwa_alv_ganho_perda) INDEX w_rows-index.
    l_tabix = sy-tabix.

    CHECK sy-subrc = 0.

    zcl_comercializacao_algodao=>ck_ajuste_sobra_perda_romaneio(
      EXPORTING
        i_id_carga      = CONV #( lwa_alv_ganho_perda-id_carga )
        i_ch_referencia = CONV #( lwa_alv_ganho_perda-ch_referencia_rom )
      IMPORTING
        e_msg_sucesso   = DATA(_msg_sucesso)
        e_mblnr         = DATA(_mblnr)
        e_mjahr         = DATA(mjahr)
        e_msg_error     = DATA(_msg_error)  ).

    IF _msg_sucesso IS NOT INITIAL.
      MESSAGE _msg_sucesso TYPE 'S'.
    ELSEIF _msg_error IS NOT INITIAL.
      MESSAGE _msg_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDLOOP.

ENDFORM.

**********************************************************************
* alv saida
**********************************************************************
FORM f_alv_saida.

  CASE abap_true.
    WHEN p_painel.
      CALL SCREEN 100.
    WHEN p_status.
      CALL SCREEN 200.
  ENDCASE.

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

* PERFORM f_funcoes.
  PERFORM f_fieldcatalog.
  PERFORM f_sort.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
  w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-zebra        = abap_false.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
  w_layout-stylefname   = 'CELLSTYLES'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF g_grid IS INITIAL. " AND  g_custom_container IS NOT INITIAL.
    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'HEADER'.

    PERFORM f_alv_header .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = g_custom_container
      EXCEPTIONS
        html_display_error = 1.

    "Grafico 1
    CALL METHOD cl_gui_cfw=>flush.

    CREATE OBJECT: g_custom_container
       EXPORTING
         container_name = 'CC_IMG',
         picture
       EXPORTING
         parent = g_custom_container.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = cl_container_95.
*    ENDIF.

    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.

    CASE abap_true.
      WHEN p_painel.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_alv[]
            it_sort                       = t_sort[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

      WHEN p_formbl.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_alv_stat[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

*-----comentado #129705-20.12.2023-JT-inicio
      WHEN p_estorn.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_alv_esto[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
*-----comentado #129705-20.12.2023-JT-fim

      WHEN p_geralo.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_alv_lote[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

      WHEN p_notafi.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_alv_nota[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

      WHEN p_reenv.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_alv_reenv[]
            it_sort                       = t_sort[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

        "SD - Ganho Peso Automatico Algodao US #145369 - WPP
      WHEN p_ganper.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_alv_ganho_perda[]
            it_sort                       = t_sort[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
        "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

    ENDCASE.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF lines( t_rows ) > 0.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

ENDFORM.

**********************************************************************
*  cabecalho
**********************************************************************
FORM f_alv_header .

  DATA: wl_data1(10),
        wl_data2(10),
        wl_hora(8),
        wl_linha(60),
        wl_text TYPE sdydo_text_element.

  DATA: wa_t001       TYPE t001,
        wa_j_1bbranch TYPE j_1bbranch.

  IF     p_painel = abap_true.
    wl_linha = 'Painel de Processamento Transferência'.
  ELSEIF p_formbl = abap_true.
    wl_linha = 'Status de Fardos - Formação de Blocos'.
  ELSEIF p_geralo = abap_true.
    wl_linha = 'Status de Fardos - Geração de Lotes'.
  ELSEIF p_notafi = abap_true.
    wl_linha = 'Status de Integração Notas Fiscais'.
  ENDIF.

  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

  CALL METHOD obj_dyndoc_id->new_line.

  IF s_werks[] IS NOT INITIAL.
    READ TABLE s_werks INDEX 1.

    IF s_werks-high IS NOT INITIAL.
      CONCATENATE  'Centro....................:' s_werks-low 'a' s_werks-high
             INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE  'Centro....................:' s_werks-low
              INTO wl_linha SEPARATED BY space.
    ENDIF.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

* CALL METHOD obj_dyndoc_id->new_line.

  IF s_data[] IS NOT INITIAL.
    READ TABLE s_data INDEX 1.

    wl_data1 = s_data-low+6(2)  && '.' && s_data-low+4(2) && '.'  && s_data-low(4).
    wl_data2 = s_data-high+6(2) && '.' && s_data-high+4(2) && '.' && s_data-high(4).

    IF s_data-high IS NOT INITIAL.
      CONCATENATE  'Data Processamento:' wl_data1 'a' wl_data2
             INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE  'Data Processamento:' wl_data1
             INTO wl_linha SEPARATED BY space.
    ENDIF.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

ENDFORM.                    " ZF_ALV_HEADER

**********************************************************************
*  imagen
**********************************************************************
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM

**********************************************************************
*  barra tarefas
**********************************************************************
FORM f_funcoes.

  FREE: t_function.

  w_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_check.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND w_function TO t_function.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_sort.

  FREE: t_sort.

  CASE abap_true.
    WHEN p_painel.
      w_sort-fieldname = 'WERKS'.
      w_sort-subtot    = 'X'.
*     w_SORT-SPOS      = 1.
      w_sort-up        = 'X'.
      APPEND w_sort   TO t_sort.

    WHEN p_reenv.
      w_sort-fieldname = 'NRO_SOL_OV'.
*     w_sort-subtot    = 'X'.
      w_sort-spos      = 1.
      w_sort-up        = 'X'.
      APPEND w_sort   TO t_sort.

      w_sort-fieldname = 'DATA_REG'.
*     w_sort-subtot    = 'X'.
      w_sort-spos      = 2.
      w_sort-up        = 'X'.
      APPEND w_sort   TO t_sort.

      w_sort-fieldname = 'HORA_REG'.
*     w_sort-subtot    = 'X'.
      w_sort-spos      = 3.
      w_sort-up        = 'X'.
      APPEND w_sort   TO t_sort.
  ENDCASE.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  CASE abap_true.
    WHEN p_painel.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_ALV' 'WERKS'          'Centro'                   '07'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        02  ''      ''       'T_ALV' 'NAME1'          'Nome'                     '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        03  ''      ''       'T_ALV' 'STATUS'         'Fase Processamento'       '25'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        04  ''      ''       'T_ALV' 'QTD_PENDEN'     'Aguardando Process.'      '22'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        05  ''      ''       'T_ALV' 'QTD_EMPROC'     'Em Processamento'         '22'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  ''      ''       'T_ALV' 'QTD_PROCES_ERR' 'Processados c/ Erro'      '22'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  ''      ''       'T_ALV' 'QTD_PROCES_sUC' 'Processados c/ Sucesso'   '22'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  ''      ''       'T_ALV' 'TOTAL'          'Total'                    '22'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

    WHEN p_formbl.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_ALV_STAT'  'STATUS_TRACE'   'Status'             '07'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' 'X',
        02  ''      ''       'T_ALV_STAT'  'ID_CARGA'       'ID Carregamento'    '40'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        03  'MARA'  'MATNR'  'T_ALV_STAT'  'MATNR'          'Material'           '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        04  ''      ''       'T_ALV_STAT'  'WERKS'          'Centro'             '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        05  ''      ''       'T_ALV_STAT'  'LGORT'          'Depósito Orig.'     '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  ''      ''       'T_ALV_STAT'  'ACHARG'         'Fardinho'           '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  ''      ''       'T_ALV_STAT'  'SAFRA'          'Safra'              '06'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        08  ''      ''       'T_ALV_STAT'  'LGORT_REC'      'Depósito Receb'     '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        09  ''      ''       'T_ALV_STAT'  'CD_SAI'         'Cód.Sai'            '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        10  'MSEG'  'MBLNR'  'T_ALV_STAT'  'MBLNR'          'Doc.Material'       '13'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
        11  'MSEG'  'MJAHR'  'T_ALV_STAT'  'MJAHR'          'Ano Docto'          '10'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
        12  'VBAK'  'VBELN'  'T_ALV_STAT'  'VBELN'          'Ordem de Venda'     '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        13  ''      ''       'T_ALV_STAT'  'CH_REFERENCIA'  'Referência'         '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        14  ''      ''       'T_ALV_STAT'  'USER_CARGA'     'Usuário Carga'      '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        15  ''      ''       'T_ALV_STAT'  'DATA_CARGA'     'Data Carga'         '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        16  ''      ''       'T_ALV_STAT'  'HORA_CARGA'     'Hora Carga'         '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

*-----comentado #129705-20.12.2023-JT-inicio
    WHEN p_estorn.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_ALV_STAT'  'STATUS_TRACE'   'Status'             '07'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' 'X',
        02  ''      ''       'T_ALV_STAT'  'ID_CARGA'       'ID Carregamento'    '40'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        03  ''      ''       'T_ALV_STAT'  'DESC_ESTORNO'   'Ação'               '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        04  'MARA'  'MATNR'  'T_ALV_STAT'  'MATNR'          'Material'           '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        05  ''      ''       'T_ALV_STAT'  'WERKS'          'Centro'             '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        06  ''      ''       'T_ALV_STAT'  'LGORT'          'Depósito Orig.'     '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  ''      ''       'T_ALV_STAT'  'ACHARG'         'Fardinho'           '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        08  ''      ''       'T_ALV_STAT'  'SAFRA'          'Safra'              '06'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        09  ''      ''       'T_ALV_STAT'  'LGORT_REC'      'Depósito Receb'     '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        10  ''      ''       'T_ALV_STAT'  'CD_SAI'         'Cód.Sai'            '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        11  'MSEG'  'MBLNR'  'T_ALV_STAT'  'MBLNR'          'Doc.Material'       '13'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
        12  'MSEG'  'MJAHR'  'T_ALV_STAT'  'MJAHR'          'Ano Docto'          '10'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
        13  'MSEG'  'MBLNR'  'T_ALV_STAT'  'MBLNR_ESTORNO'  'Doc.Mat.Estorno'    '15'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
        14  'MSEG'  'MJAHR'  'T_ALV_STAT'  'MJAHR_ESTORNO'  'Ano Docto Estorno'  '15'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
        15  'VBAK'  'VBELN'  'T_ALV_STAT'  'VBELN'          'Ordem de Venda'     '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        16  ''      ''       'T_ALV_STAT'  'CH_REFERENCIA'  'Referência'         '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        17  ''      ''       'T_ALV_STAT'  'USER_ESTORNO'   'Usuário Carga'      '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        18  ''      ''       'T_ALV_STAT'  'DATA_ESTORNO'   'Data Carga'         '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        19  ''      ''       'T_ALV_STAT'  'HORA_ESTORNO'   'Hora Carga'         '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.
*-----comentado #129705-20.12.2023-JT-fim

    WHEN p_geralo.
      PERFORM f_estrutura_alv USING:
        01  ''         ''             'T_ALV_LOTE'  'STATUS_TRACE'   'Status'             '07'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' 'X',
        02  ''         ''             'T_ALV_LOTE'  'STATUS_EMAIL'   'Status Email'       '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' 'X' 'X',
        03  ''         ''             'T_ALV_LOTE'  'STATUS_INTEGRA' 'Status Integração'  '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' 'X' 'X',
        04  ''         ''             'T_ALV_LOTE'  'ID_CARGA'       'ID Carregamento'    '40'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        05  ''         ''             'T_ALV_LOTE'  'WERKS'          'Centro'             '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        06  ''         ''             'T_ALV_LOTE'  'LGORT'          'Depósito'           '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  ''         ''             'T_ALV_LOTE'  'ACHARG'         'Fardinho'           '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        08  ''         ''             'T_ALV_LOTE'  'SAFRA'          'Safra'              '06'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        09  'ZMMT0008' 'NR_ROMANEIO'  'T_ALV_LOTE'  'NR_ROMANEIO'    'Nr.Romaneio'        '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        10  ''         ''             'T_ALV_LOTE'  'NFNUM'          'Nota Fiscal'        '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        11  'VBAK'     'VBELN'        'T_ALV_LOTE'  'VBELN'          'Ordem Venda'        '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        12  ''         ''             'T_ALV_LOTE'  'PLACA_CAV'      'Placa Cavalo'       '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        13  ''         ''             'T_ALV_LOTE'  'MOTORISTA'      'Motorista'          '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        14  ''         ''             'T_ALV_LOTE'  'MENGE'          'Quantidade'         '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

    WHEN p_notafi.
      PERFORM f_estrutura_alv USING:
        01  ''         ''             'T_ALV_NOTA'  'STATUS_NF'      'Status'             '07'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' 'X',
        02  ''         ''             'T_ALV_NOTA'  'DOCNUM'         'Nr.Documento'       '13'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' 'X',
        03  ''         ''             'T_ALV_NOTA'  'DOCNUM_CANCELADO' 'Nr.Doc.Cancelado' '17'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' 'X',
        04  ''         ''             'T_ALV_NOTA'  'ID_CARGA'       'ID Carregamento'    '40'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        05  'MARA'     'MATNR'        'T_ALV_NOTA'  'MATNR'          'Material'           '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  ''         ''             'T_ALV_NOTA'  'WERKS'          'Centro'             '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  ''         ''             'T_ALV_NOTA'  'LGORT'          'Depósito'           '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        08  ''         ''             'T_ALV_NOTA'  'SAFRA'          'Safra'              '06'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

    WHEN p_reenv.
      PERFORM f_estrutura_alv USING:
        01  ''         ''             'T_ALV_REENV' 'STATUS'         'Status'             '07'  ' '    ' ' ' ' 'X '  ' ' ' ' ' ' ' ' 'X' 'X',
        02  'VBAK'     'VBELN'        'T_ALV_REENV' 'VBELN'          'Ordem Venda'        '13'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' 'X',
        03  'ZSDT0066' 'NRO_SOL_OV'   'T_ALV_REENV' 'NRO_SOL_OV'     'Solicitação'        '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        04  ''         ''             'T_ALV_REENV' 'POSNR'          'Posição'            '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        05  ''         ''             'T_ALV_REENV' 'DESCR'          'Método'             '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  ''         ''             'T_ALV_REENV' 'DATA_REG'       'Data Envio'         '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  ''         ''             'T_ALV_REENV' 'HORA_REG'       'Hora Envio'         '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

      "SD - Ganho Peso Automatico Algodao US #145369 - WPP
    WHEN p_ganper.
      PERFORM f_estrutura_alv USING:
        02  ''         ''             'T_ALV_GANHO_PERDA' 'WERKS'          'Centro'                '06'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        03  ''         ''             'T_ALV_GANHO_PERDA' 'ID_CARGA'       'Id.Carregamento'       '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        03  ''         ''             'T_ALV_GANHO_PERDA' 'SAFRA'          'Safra'                 '05'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        04  ''         ''             'T_ALV_GANHO_PERDA' 'VBELN'          'Ordem Venda'           '11'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' '',
        05  ''         ''             'T_ALV_GANHO_PERDA' 'NR_ROMANEIO'    'Romaneio'              '09'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' '',
        06  ''         ''             'T_ALV_GANHO_PERDA' 'MBLNR'          'Doc.Material'          '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' '',
        07  ''         ''             'T_ALV_GANHO_PERDA' 'MJAHR'          'Ano.Material'          '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' '',
        08  ''         ''             'T_ALV_GANHO_PERDA' 'QUANTIDADE'     'Quantidade'            '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' '',
        08  ''         ''             'T_ALV_GANHO_PERDA' 'ESTORNADO'      'Estornado'             '09'  ' '    ' ' ' ' ' '  ' ' 'X' ' ' ' ' ' ' '',
        09  ''         ''             'T_ALV_GANHO_PERDA' 'DT_REGISTRO'    'Dt.Registro'           '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' '',
        10  ''         ''             'T_ALV_GANHO_PERDA' 'HR_REGISTRO'    'Hr.Registro'           '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ''.
      "SD - Ganho Peso Automatico Algodao US #145369 - WPP


  ENDCASE.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).                                    "16

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
**********************************************************************
