*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_dados.

  SELECT zppt0030~id_referencia,
         zppt0030~acharg,
         zppt0030~werks,
         zppt0030~status_registro,
         zppt0030~processamento,
         zppt0030~emproc_normal,
         zppt0030~proces_normal,
         zppt0030~emproc_estorn,
         zppt0030~proces_estorn,
         t001w~name1
    FROM zppt0030
    INNER JOIN t001w ON t001w~werks = zppt0030~werks
    INTO TABLE @t_zppt0030
   WHERE zppt0030~werks     IN @s_werks
     AND zppt0030~data_proc IN @s_data.

ENDFORM.

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_reenvio.

  SELECT *
    FROM zppt0006
    INTO TABLE t_zppt0006
   WHERE werks     IN s_werks
     AND data      IN s_data
     AND id_cotton IN s_idcot.

  CHECK sy-subrc = 0.

*PP - Melhorias ZPPT0003 - 03112022 - BUG #92422 - BG

  SELECT *
    FROM zppt0002
    INTO TABLE t_zppt0002
     FOR ALL ENTRIES IN t_zppt0006
   WHERE id_cotton    = t_zppt0006-id_cotton
     AND werks     = t_zppt0006-werks.

*    SELECT *
*    FROM zppt0002
*    INTO TABLE t_zppt0002
*     FOR ALL ENTRIES IN t_zppt0006
*   WHERE acharg    = t_zppt0006-acharg
*     AND werks     = t_zppt0006-werks.

*  SELECT *
*    FROM zppt0002
*    APPENDING TABLE t_zppt0002
*     FOR ALL ENTRIES IN t_zppt0006
*   WHERE charg     = t_zppt0006-charg
*     AND werks     = t_zppt0006-werks.

  SORT t_zppt0002 BY acharg werks.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_dados.

  DATA: l_tot_qtd_penden TYPE i,
        l_tot_qtd_emproc TYPE i,
        l_tot_qtd_proces TYPE i.

  FREE: t_alv.

  LOOP AT t_zppt0030 INTO w_zppt0030.

    CLEAR w_alv.

    CASE w_zppt0030-processamento.

      WHEN '01'. "Processamento fardos
        CASE w_zppt0030-status_registro.
          WHEN '01'.
            w_alv-status = 'Produção'.
          WHEN '03'.
            w_alv-status = 'Classificacao'.
          WHEN '07' OR '09'.
            w_alv-status = 'Transferência'.
          WHEN OTHERS.
            w_alv-status = w_zppt0030-status_registro.
        ENDCASE.

        IF     w_zppt0030-emproc_normal = abap_false AND
               w_zppt0030-proces_normal = abap_false.
          w_alv-qtd_penden = 1.
          w_alv-total      = 1.
          l_tot_qtd_penden = l_tot_qtd_penden + 1.
        ELSEIF w_zppt0030-emproc_normal = abap_true AND
               w_zppt0030-proces_normal = abap_false.
          w_alv-qtd_emproc = 1.
          w_alv-total      = 1.
          l_tot_qtd_emproc = l_tot_qtd_emproc + 1.
        ELSEIF w_zppt0030-proces_normal = abap_true.
          w_alv-qtd_proces = 1.
          w_alv-total      = 1.
          l_tot_qtd_proces = l_tot_qtd_proces + 1.
        ENDIF.

      WHEN '02'.  "Estorno fardos
        w_alv-status = 'Estorno'.

        IF     w_zppt0030-emproc_estorn = abap_false AND
               w_zppt0030-proces_estorn = abap_false.
          w_alv-qtd_penden = 1.
          w_alv-total      = 1.
          l_tot_qtd_penden = l_tot_qtd_penden + 1.
        ELSEIF w_zppt0030-emproc_estorn = abap_true AND
               w_zppt0030-proces_estorn = abap_false.
          w_alv-qtd_emproc = 1.
          w_alv-total      = 1.
          l_tot_qtd_emproc = l_tot_qtd_emproc + 1.
        ELSEIF w_zppt0030-proces_estorn = abap_true.
          w_alv-qtd_proces = 1.
          w_alv-total      = 1.
          l_tot_qtd_proces = l_tot_qtd_proces + 1.
        ENDIF.
    ENDCASE.

    w_alv-werks       = w_zppt0030-werks.
    w_alv-name1       = w_zppt0030-name1.

    COLLECT w_alv  INTO t_alv.
  ENDLOOP.

*----------------------------------------
* total
*----------------------------------------
* CLEAR w_alv.
* w_alv-status     = 'T O T A L'.
* w_alv-qtd_penden = l_tot_qtd_penden.
* w_alv-qtd_emproc = l_tot_qtd_emproc.
* w_alv-qtd_proces = l_tot_qtd_proces.
* w_alv-total      = l_tot_qtd_penden + l_tot_qtd_emproc + l_tot_qtd_proces.
* APPEND w_alv    TO t_alv.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_reenvio.

  FREE: t_alv_reen, r_name,
        t_zppt0006_pri,
        t_zppt0006_atu.

*----------------------------------------------
*-Montar Range icons
*----------------------------------------------
  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_GREEN_LIGHT'.
  APPEND r_name.

  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_YELLOW_LIGHT'.
  APPEND r_name.

  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_RED_LIGHT'.
  APPEND r_name.

  SELECT id
         name
    FROM icon
    INTO TABLE t_icon
   WHERE name IN r_name.

  SORT t_icon  BY name.

  t_zppt0006_grp[] = t_zppt0006[].

*  SORT t_zppt0006_grp BY id_cotton.
*  DELETE ADJACENT DUPLICATES FROM t_zppt0006_grp
*                        COMPARING id_cotton.
*
*  DATA(l_prim)  = 0.
*  DATA(l_sequ)  = 0.
*  DATA(l_tem_r) = abap_false.
*
*  SORT t_zppt0006 BY id_cotton.
*
*  LOOP AT t_zppt0006_grp INTO w_zppt0006_grp.
*
*    DATA(l_tabix) = sy-tabix.
*
*    FREE: l_prim, l_sequ, l_tem_r.
*
*    LOOP AT t_zppt0006 INTO w_zppt0006 WHERE id_cotton = w_zppt0006_grp-id_cotton.
*      IF w_zppt0006-flag_envio <> 'P'.
*        l_tem_r = abap_true.
*      ENDIF.
*      IF w_zppt0006-charg = w_zppt0006-acharg.
*        l_prim = l_prim + 1.
*      ELSE.
*        l_sequ = l_sequ + 1.
*        APPEND w_zppt0006  TO t_zppt0006_atu.
*      ENDIF.
*    ENDLOOP.
*
*    IF l_prim > 0 AND l_sequ = 0.
*      APPEND w_zppt0006    TO t_zppt0006_pri.
*    ENDIF.
*
*    IF l_tem_r = abap_true.
*      w_zppt0006_grp-flag_envio = 'R'.
*    ELSE.
*      w_zppt0006_grp-flag_envio = 'P'.
*    ENDIF.
*    MODIFY t_zppt0006_grp FROM w_zppt0006_grp INDEX l_tabix.
*  ENDLOOP.
*
** FREE: t_zppt0006_grp.
**
** APPEND LINES OF t_zppt0006_pri  TO t_zppt0006_grp[].
** APPEND LINES OF t_zppt0006_atu  TO t_zppt0006_grp[].
*
*  SORT t_zppt0006_grp BY id_cotton.
*  DELETE ADJACENT DUPLICATES FROM t_zppt0006_grp
*                        COMPARING id_cotton.
*
*  SORT t_zppt0006_atu BY werks charg acharg id DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM t_zppt0006_atu
*                        COMPARING werks charg acharg.
*
*  LOOP AT t_zppt0006_grp INTO w_zppt0006_grp.
*
*    CLEAR w_zppt0002.
*    READ TABLE t_zppt0002 INTO w_zppt0002 WITH KEY acharg = w_zppt0006_grp-acharg
*                                                            werks = w_zppt0006_grp-werks.
*    IF sy-subrc <> 0.
*      READ TABLE t_zppt0002 INTO w_zppt0002 WITH KEY charg = w_zppt0006_grp-charg
*                                                  werks = w_zppt0006_grp-werks.
*    ENDIF.
*
*    IF p_pend = abap_true AND  w_zppt0006_grp-status_msg = 'S'.
*      CONTINUE.
*    ENDIF.
*
*    IF w_zppt0006_grp-flag_envio = 'P'.
*      l_icon_name = 'ICON_GREEN_LIGHT'.
*    ELSE.
*      l_icon_name = 'ICON_YELLOW_LIGHT'.
*    ENDIF.
*
*    READ TABLE t_icon  INTO w_icon  WITH KEY name = l_icon_name
*                                    BINARY SEARCH.
*
*    w_alv_reen-status      = w_icon-id.
*    w_alv_reen-werks       = w_zppt0006_grp-werks.
*    w_alv_reen-id_cotton   = w_zppt0006_grp-id_cotton.
*    w_alv_reen-lgort       = w_zppt0006_grp-lgort.
*    w_alv_reen-data        = w_zppt0006_grp-data.
*    w_alv_reen-cd_safra    = w_zppt0002-cd_safra.
*    w_alv_reen-cd_sai      = w_zppt0002-cd_sai.
*    w_alv_reen-flag_envio  = w_zppt0006_grp-flag_envio.
*    w_alv_reen-mensagem    = w_zppt0006_grp-cd_mensagem.
*
*    APPEND w_alv_reen      TO t_alv_reen.
*  ENDLOOP.

  SORT t_zppt0006_grp BY id_cotton data hora DESCENDING .
  DELETE ADJACENT DUPLICATES FROM t_zppt0006_grp
                        COMPARING id_cotton.

  DATA(l_prim)  = 0.
  DATA(l_sequ)  = 0.
  DATA(l_tem_r) = abap_false.

  SORT t_zppt0006 BY id_cotton.

  LOOP AT t_zppt0006_grp INTO w_zppt0006_grp.

    DATA(l_tabix) = sy-tabix.

    FREE: l_prim, l_sequ, l_tem_r.

    LOOP AT t_zppt0006 INTO w_zppt0006 WHERE id_cotton = w_zppt0006_grp-id_cotton.
      IF w_zppt0006-flag_envio <> 'P'.
        l_tem_r = abap_true.
      ENDIF.
      IF w_zppt0006-charg = w_zppt0006-acharg.
        l_prim = l_prim + 1.
      ELSE.
        l_sequ = l_sequ + 1.
        APPEND w_zppt0006  TO t_zppt0006_atu.
      ENDIF.
    ENDLOOP.

    IF l_prim > 0 AND l_sequ = 0.
      APPEND w_zppt0006    TO t_zppt0006_pri.
    ENDIF.

    IF l_tem_r = abap_true.
      w_zppt0006_grp-flag_envio = 'R'.
    ELSE.
      w_zppt0006_grp-flag_envio = 'P'.

    ENDIF.
    MODIFY t_zppt0006_grp FROM w_zppt0006_grp INDEX l_tabix.
  ENDLOOP.

* FREE: t_zppt0006_grp.
*
* APPEND LINES OF t_zppt0006_pri  TO t_zppt0006_grp[].
* APPEND LINES OF t_zppt0006_atu  TO t_zppt0006_grp[].

  SORT t_zppt0006_grp BY id_cotton acharg.
  DELETE ADJACENT DUPLICATES FROM t_zppt0006_grp
                        COMPARING id_cotton acharg.

  SORT t_zppt0006_atu BY werks charg acharg id DESCENDING.
  DELETE ADJACENT DUPLICATES FROM t_zppt0006_atu
                        COMPARING werks charg acharg.


  SORT t_zppt0002 BY id_cotton acharg charg.
  DELETE ADJACENT DUPLICATES FROM t_zppt0002
                          COMPARING id_cotton acharg charg.
  LOOP AT t_zppt0006_grp INTO w_zppt0006_grp.
*PP - Melhorias ZPPT0003 - 03112022 - BUG #92422 - BG
    "pegar o acharg da tabela zppt0002
    CLEAR w_zppt0002.

    IF p_pend = abap_true AND  w_zppt0006_grp-status_msg = 'S'.
      CONTINUE.
    ENDIF.

    "READ TABLE t_zppt0002 INTO w_zppt0002 WITH KEY acharg = w_zppt0006_grp-acharg
    " werks = w_zppt0006_grp-werks.
    "IF sy-subrc EQ 0.
    LOOP AT t_zppt0002 INTO w_zppt0002 WHERE werks = w_zppt0006_grp-werks
                                             AND id_cotton = w_zppt0006_grp-id_cotton.

*      IF p_pend = abap_true AND  w_zppt0006_grp-status_msg = 'S'.
*          CONTINUE.
*        ENDIF.

      IF  w_zppt0002-id_cotton = w_zppt0006_grp-id_cotton AND w_zppt0002-werks = w_zppt0006_grp-werks.
*          IF p_pend = abap_true AND  w_zppt0002-status_registro(1) <> 'E' .
*            CONTINUE.
*          ENDIF.
        IF p_pend = abap_true.

          IF ( w_zppt0002-status_registro(1) = 'E' OR ( w_zppt0002-charg IS INITIAL AND w_zppt0002-status = 'R' AND w_zppt0002-status_registro = '01' ) ).

            l_icon_name = 'ICON_YELLOW_LIGHT'.

            READ TABLE t_icon  INTO w_icon  WITH KEY name = l_icon_name
                                   BINARY SEARCH.

            w_alv_reen-status      = w_icon-id.
            w_alv_reen-werks       = w_zppt0006_grp-werks.
            w_alv_reen-acharg      = w_zppt0002-acharg.
            w_alv_reen-id_cotton   = w_zppt0006_grp-id_cotton.
            w_alv_reen-lgort       = w_zppt0006_grp-lgort.
            w_alv_reen-data        = w_zppt0006_grp-data.
            w_alv_reen-cd_safra    = w_zppt0002-cd_safra.
            w_alv_reen-cd_sai      = w_zppt0002-cd_sai.
            w_alv_reen-flag_envio  = w_zppt0006_grp-flag_envio.
            w_alv_reen-mensagem    = w_zppt0006_grp-cd_mensagem.

            APPEND w_alv_reen      TO t_alv_reen.
          ENDIF.
        ELSE.
          IF ( w_zppt0002-status_registro(1) = 'E' OR ( w_zppt0002-charg IS INITIAL AND w_zppt0002-status = 'R' AND w_zppt0002-status_registro = '01' ) ).
            l_icon_name = 'ICON_YELLOW_LIGHT'.
          ELSE.
            l_icon_name = 'ICON_GREEN_LIGHT'.
          ENDIF.
          READ TABLE t_icon  INTO w_icon  WITH KEY name = l_icon_name
                                          BINARY SEARCH.

          w_alv_reen-status      = w_icon-id.
          w_alv_reen-werks       = w_zppt0006_grp-werks.
          w_alv_reen-acharg      = w_zppt0002-acharg.
          w_alv_reen-id_cotton   = w_zppt0006_grp-id_cotton.
          w_alv_reen-lgort       = w_zppt0006_grp-lgort.
          w_alv_reen-data        = w_zppt0006_grp-data.
          w_alv_reen-cd_safra    = w_zppt0002-cd_safra.
          w_alv_reen-cd_sai      = w_zppt0002-cd_sai.
          w_alv_reen-flag_envio  = w_zppt0006_grp-flag_envio.
          w_alv_reen-mensagem    = w_zppt0006_grp-cd_mensagem.

          APPEND w_alv_reen      TO t_alv_reen.
        ENDIF.


      ENDIF.


    ENDLOOP.
    "ELSE.
*      READ TABLE t_zppt0002 INTO w_zppt0002 WITH KEY charg = w_zppt0006_grp-charg
*                                                  werks = w_zppt0006_grp-werks.
*      LOOP AT t_zppt0002 INTO w_zppt0002 WHERE charg = w_zppt0006_grp-charg AND
*                                                  werks = w_zppt0006_grp-werks.
*
*      ENDLOOP.
    " ENDIF.
*  ENDLOOP.
*
*  IF sy-subrc <> 0.
*
*  ENDIF.


  ENDLOOP.



ENDFORM.

**********************************************************************
* REENVIO TRACE
**********************************************************************
FORM f_reenvio_trace.

  DATA: e_fardo     TYPE zpme0059.
  DATA: t_fardo  TYPE zpmt0059,
        vl_charg TYPE zppt0002-charg.

  DATA: obj_trace TYPE REF TO zcl_webservice_trace.
  FREE: obj_trace.

  CREATE OBJECT: obj_trace.

  FREE: t_rows.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione ao menos uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE t_rows LINES DATA(l_lines).

  DATA(l_reenv) = 0.

*----------------------------------------------------
* reenvio
*----------------------------------------------------
  LOOP AT t_rows INTO w_rows.

    FREE: t_fardo.

    READ TABLE t_alv_reen INTO w_alv_reen INDEX w_rows-index.

    CHECK sy-subrc = 0.

    "CHECK w_alv_reen-flag_envio <> 'P'.

    l_reenv = l_reenv + 1.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = |Reenviando { l_lines } / { l_reenv }|.

*----------------------------
*-- trata 1o envio
*----------------------------
    READ TABLE t_zppt0006_pri INTO w_zppt0006_atu WITH KEY id_cotton = w_alv_reen-id_cotton.

    IF sy-subrc = 0.
      LOOP AT t_zppt0002 INTO w_zppt0002 WHERE id_cotton = w_zppt0006_atu-id_cotton
                                           AND werks     = w_zppt0006_atu-werks.
*                                          AND charg     = w_zppt0006_atu-charg.

        CLEAR: e_fardo.

*PP - Melhorias ZPPT0003 - 03112022 - BUG #92422 - BG  - INICIO

*Seleciona linha e processa Reenvio
*Pega o ID_COTTON
*Vai na tabela ZPPS_XIMFBF_LOG verifica se tem o CHARG,
*se sim, pega o CHARG e preenche na TABELA ZPPT0002,
*Se nao encontrou CHARG
*Vai na tabela ZMMT0006 verifica se tem o CHARG
*se sim, pega o CHARG e preenche na TABELA ZPPT0002,

        IF w_zppt0002-charg IS INITIAL AND w_zppt0002-status = 'R' AND w_zppt0002-status_registro = '01'.

          SELECT SINGLE charg INTO vl_charg FROM zpps_ximfbf_log  WHERE id_cotton = w_alv_reen-id_cotton.
          IF sy-subrc IS NOT INITIAL.
            SELECT SINGLE batch_d INTO vl_charg FROM zmmt0006  WHERE id_cotton = w_alv_reen-id_cotton.
          ENDIF.
          "w_zppt0002-charg = VL_CHARG.

          UPDATE zppt0002 SET charg = vl_charg WHERE id_cotton = w_alv_reen-id_cotton.

        ENDIF.

        READ TABLE t_zppt0006_pri INTO w_zppt0006_atu WITH KEY id_cotton = w_zppt0002-id_cotton
*                                                              charg     = w_zppt0002-charg
                                                               werks     = w_zppt0002-werks.
        CHECK sy-subrc = 0.

        e_fardo-nr_fardo              = w_zppt0002-acharg.
        e_fardo-id_filial_sap         = w_zppt0002-werks.
        e_fardo-nr_maquina            = w_zppt0002-verid.
*---> 14/06/2023 - Migração S4 - JS
*        e_fardo-id_material_sap       = w_zppt0002-matnr.
        e_fardo-id_material_sap = CONV #( w_zppt0002-matnr ).
*<--- 14/06/2023 - Migração S4 - JS
        e_fardo-quantidade            = w_zppt0002-menge.

        IF w_zppt0002-id_cotton IS INITIAL.
          e_fardo-nr_fardo_origem     = w_zppt0002-charg.
        ELSE.
          e_fardo-nr_fardo_origem     = w_zppt0002-id_cotton.
        ENDIF.

        e_fardo-dt_lancamento         = w_zppt0002-budat.

        CONCATENATE w_zppt0002-budat+0(4) '-'
                    w_zppt0002-budat+4(2) '-'
                    w_zppt0002-budat+6(2)
               INTO e_fardo-dt_lancamento.

        e_fardo-dt_documento          = w_zppt0002-bldat.

        CONCATENATE w_zppt0002-bldat+0(4) '-'
                    w_zppt0002-bldat+4(2) '-'
                    w_zppt0002-bldat+6(2)
               INTO e_fardo-dt_documento.

        e_fardo-dt_fabricacao         = w_zppt0002-dt_fabricacao.

        CONCATENATE w_zppt0002-dt_fabricacao+0(4) '-'
                    w_zppt0002-dt_fabricacao+4(2) '-'
                    w_zppt0002-dt_fabricacao+6(2)
               INTO e_fardo-dt_fabricacao.

        e_fardo-hr_fabricacao         = w_zppt0002-hr_fabricacao.
        e_fardo-status                = w_zppt0002-status.
        e_fardo-usuario               = w_zppt0002-usnam.
        e_fardo-safra                 = w_zppt0002-cd_safra.
        e_fardo-id_fardinho           = w_zppt0002-id_fardinho.
        e_fardo-cd_sai                = w_zppt0002-cd_sai.
        e_fardo-peso_bruto            = w_zppt0002-peso_bruto.
        e_fardo-peso_liquido          = w_zppt0002-peso_liquido.
        e_fardo-id_fardao             = w_zppt0002-id_fardao.
        e_fardo-cd_classificacao      = w_zppt0002-cd_classificacao.
        e_fardo-deposito              = w_zppt0002-lgort.
        e_fardo-doc_material_fardinho = w_zppt0002-mblnr.
        e_fardo-doc_material_fardao   = w_zppt0002-mblnr02.
        e_fardo-nome_responsavel      = ' '.
        e_fardo-dt_atualizacao        = sy-datum.

        CONCATENATE sy-datum+0(4) '-'
                    sy-datum+4(2) '-'
                    sy-datum+6(2)
               INTO e_fardo-dt_atualizacao.

        IF     w_zppt0006_atu-status_registro = '05'.
          w_zppt0006-status_registro      = '06'.
        ELSEIF w_zppt0006_atu-status_registro = 'E5'.
          w_zppt0006-status_registro      = 'E6'.
        ENDIF.

        e_fardo-status_registro       = w_zppt0006_atu-status_registro.
        e_fardo-cd_mensagem           = w_zppt0006_atu-cd_mensagem.
        e_fardo-rg_atualizado         = 1.
        e_fardo-qtd_fardinhos         = w_zppt0002-qtd_fardinhos.
        e_fardo-peso_caroco           = w_zppt0002-peso_caroco.
        e_fardo-peso_fibrilha         = w_zppt0002-peso_fibrilha.
        e_fardo-rowversion            = 0.
        e_fardo-seq_num               = 0.

        APPEND e_fardo               TO t_fardo.
      ENDLOOP.

      IF t_fardo[] IS NOT INITIAL.
        CALL METHOD obj_trace->atualiza_trace
          EXPORTING
            t_fardo  = t_fardo               " Estrutura para retorno
          RECEIVING
            ret_code = DATA(_ret_code).      " Status Code

        IF _ret_code = 200.
          LOOP AT t_zppt0006_pri INTO w_zppt0006_pri WHERE id_cotton = w_alv_reen-id_cotton.
            w_zppt0006_pri-flag_envio = 'P'.
            MODIFY zppt0006      FROM w_zppt0006_pri.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ELSE.

      LOOP AT t_zppt0006_atu INTO w_zppt0006_atu WHERE id_cotton = w_alv_reen-id_cotton.

        CLEAR: e_fardo, w_zppt0002.

        READ TABLE t_zppt0002 INTO w_zppt0002 WITH KEY acharg = w_zppt0006_atu-acharg
                                                       werks  = w_zppt0006_atu-werks.
        CHECK sy-subrc = 0.
*PP - Melhorias ZPPT0003 - 03112022 - BUG #92422 - BG  - INICIO

*Seleciona linha e processa Reenvio
*Pega o ID_COTTON
*Vai na tabela ZPPS_XIMFBF_LOG verifica se tem o CHARG,
*se sim, pega o CHARG e preenche na TABELA ZPPT0002,
*Se nao encontrou CHARG
*Vai na tabela ZMMT0006 verifica se tem o CHARG
*se sim, pega o CHARG e preenche na TABELA ZPPT0002,

        IF w_zppt0002-charg IS INITIAL AND w_zppt0002-status = 'R' AND w_zppt0002-status_registro = '01'.

          SELECT SINGLE charg INTO vl_charg FROM zpps_ximfbf_log  WHERE id_cotton = w_alv_reen-id_cotton.
          IF sy-subrc IS NOT INITIAL.
            SELECT SINGLE batch_d INTO vl_charg FROM zmmt0006  WHERE id_cotton = w_alv_reen-id_cotton.
          ENDIF.
          "w_zppt0002-charg = VL_CHARG.

          UPDATE zppt0002 SET charg = vl_charg WHERE id_cotton = w_alv_reen-id_cotton.

        ENDIF.




        e_fardo-nr_fardo              = w_zppt0002-acharg.
        e_fardo-id_filial_sap         = w_zppt0002-werks.
        e_fardo-nr_maquina            = w_zppt0002-verid.
*---> 14/06/2023 - Migração S4 - JS
*        e_fardo-id_material_sap       = w_zppt0002-matnr.
        e_fardo-id_material_sap = CONV #( w_zppt0002-matnr ).
*<--- 14/06/2023 - Migração S4 - JS
        e_fardo-quantidade            = w_zppt0002-menge.

        IF w_zppt0002-id_cotton IS INITIAL.
          e_fardo-nr_fardo_origem     = w_zppt0002-charg.
        ELSE.
          e_fardo-nr_fardo_origem     = w_zppt0002-id_cotton.
        ENDIF.

        e_fardo-dt_lancamento         = w_zppt0002-budat.

        CONCATENATE w_zppt0002-budat+0(4) '-'
                    w_zppt0002-budat+4(2) '-'
                    w_zppt0002-budat+6(2)
               INTO e_fardo-dt_lancamento.

        e_fardo-dt_documento          = w_zppt0002-bldat.

        CONCATENATE w_zppt0002-bldat+0(4) '-'
                    w_zppt0002-bldat+4(2) '-'
                    w_zppt0002-bldat+6(2)
               INTO e_fardo-dt_documento.

        e_fardo-dt_fabricacao         = w_zppt0002-dt_fabricacao.

        CONCATENATE w_zppt0002-dt_fabricacao+0(4) '-'
                    w_zppt0002-dt_fabricacao+4(2) '-'
                    w_zppt0002-dt_fabricacao+6(2)
               INTO e_fardo-dt_fabricacao.

        e_fardo-hr_fabricacao         = w_zppt0002-hr_fabricacao.
        e_fardo-status                = w_zppt0002-status.
        e_fardo-usuario               = w_zppt0002-usnam.
        e_fardo-safra                 = w_zppt0002-cd_safra.
        e_fardo-id_fardinho           = w_zppt0002-id_fardinho.
        e_fardo-cd_sai                = w_zppt0002-cd_sai.
        e_fardo-peso_bruto            = w_zppt0002-peso_bruto.
        e_fardo-peso_liquido          = w_zppt0002-peso_liquido.
        e_fardo-id_fardao             = w_zppt0002-id_fardao.
        e_fardo-cd_classificacao      = w_zppt0002-cd_classificacao.
        e_fardo-deposito              = w_zppt0002-lgort.
        e_fardo-doc_material_fardinho = w_zppt0002-mblnr.
        e_fardo-doc_material_fardao   = w_zppt0002-mblnr02.
        e_fardo-nome_responsavel      = ' '.
        e_fardo-dt_atualizacao        = sy-datum.

        CONCATENATE sy-datum+0(4) '-'
                    sy-datum+4(2) '-'
                    sy-datum+6(2)
               INTO e_fardo-dt_atualizacao.

        IF     w_zppt0006_atu-status_registro = '05'.
          w_zppt0006-status_registro      = '06'.
        ELSEIF w_zppt0006_atu-status_registro = 'E5'.
          w_zppt0006-status_registro      = 'E6'.
        ENDIF.

        e_fardo-status_registro       = w_zppt0006_atu-status_registro.
        e_fardo-cd_mensagem           = w_zppt0006_atu-cd_mensagem.
        e_fardo-rg_atualizado         = 1.
        e_fardo-qtd_fardinhos         = w_zppt0002-qtd_fardinhos.
        e_fardo-peso_caroco           = w_zppt0002-peso_caroco.
        e_fardo-peso_fibrilha         = w_zppt0002-peso_fibrilha.
        e_fardo-rowversion            = 0.
        e_fardo-seq_num               = 0.

        APPEND e_fardo               TO t_fardo.
      ENDLOOP.

      IF t_fardo[] IS NOT INITIAL.
        CALL METHOD obj_trace->atualiza_trace
          EXPORTING
            t_fardo  = t_fardo               " Estrutura para retorno
          RECEIVING
            ret_code = _ret_code.      " Status Code

        IF _ret_code = 200.
          LOOP AT t_zppt0006_atu INTO w_zppt0006_atu WHERE id_cotton = w_alv_reen-id_cotton.
            w_zppt0006_atu-flag_envio = 'P'.
            MODIFY zppt0006      FROM w_zppt0006_atu.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    COMMIT WORK.
  ENDLOOP.

  IF l_reenv > 0.
    MESSAGE s024(sd) WITH 'Reenvio finalizado.'.
  ENDIF.

ENDFORM.

**********************************************************************
* alv saida
**********************************************************************
FORM f_alv_saida.

  CASE abap_true.
    WHEN p_painel.
      CALL SCREEN 100.
    WHEN p_reenvi.
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

      WHEN p_reenvi.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_alv_reen[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
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

  IF p_painel = abap_true.
    wl_linha = 'Painel de Processamento JOBS Algodão'.
  ELSE.
    wl_linha = 'Reenvio de Fardos ao Trace Cotton'.
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
    CONCATENATE  'Centro....................:' s_werks-low
            INTO wl_linha SEPARATED BY space.

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
*
*  IF s_hora[] IS NOT INITIAL.
*    READ TABLE s_hora INDEX 1.
*
*    wl_data1 = s_hora-low(2)  && ':' && s_hora-low+2(2)  && ':' && s_hora-low+4(2).
*    wl_data2 = s_hora-high(2) && ':' && s_hora-high+2(2) && ':' && s_hora-high+4(2).
*
*    IF s_hora-high IS NOT INITIAL.
*      CONCATENATE  'Hora Processamento:' wl_data1 'a' wl_data2
*             INTO wl_linha SEPARATED BY space.
*    ELSE.
*      CONCATENATE  'Hora Processamento:' wl_data1
*             INTO wl_linha SEPARATED BY space.
*    ENDIF.
*
*    wl_text = wl_linha.
*    CALL METHOD obj_dyndoc_id->new_line.
*
*    CALL METHOD obj_dyndoc_id->add_text
*      EXPORTING
*        text         = wl_text
*        sap_fontsize = cl_dd_area=>list_normal.
*  ENDIF.

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

  CHECK p_painel = abap_true.

  w_sort-fieldname = 'WERKS'.
  w_sort-subtot    = 'X'.
* w_SORT-SPOS      = 1.
  w_sort-up        = 'X'.
  APPEND w_sort   TO t_sort.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  CASE abap_true.
    WHEN p_painel.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_ALV' 'WERKS'         'Centro'                   '07'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        02  ''      ''       'T_ALV' 'NAME1'         'Nome'                     '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        03  ''      ''       'T_ALV' 'STATUS'        'Fase Processamento'       '25'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        04  ''      ''       'T_ALV' 'QTD_PENDEN'    'Aguardando Process.'      '20'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        05  ''      ''       'T_ALV' 'QTD_EMPROC'    'Em Processamento'         '20'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  ''      ''       'T_ALV' 'QTD_PROCES'    'Processados'              '20'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  ''      ''       'T_ALV' 'TOTAL'         'Total'                    '20'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

    WHEN p_reenvi.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_ALV_REEN' 'STATUS'        'Status'              '07'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        02  ''      ''       'T_ALV_REEN' 'WERKS'         'Centro'              '07'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        03  ''      ''       'T_ALV_REEN' 'ID_COTTON'     'ID Cotton'           '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        04  ''      ''       'T_ALV_REEN' 'ACHARG'        'ACHARG'              '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        05  ''      ''       'T_ALV_REEN' 'LGORT'         'Depósito'            '09'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  ''      ''       'T_ALV_REEN' 'DATA'          'Dt.Proc.'            '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  ''      ''       'T_ALV_REEN' 'CD_SAFRA'      'Safra'               '06'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        08  ''      ''       'T_ALV_REEN' 'CD_SAI'        'Nro.Fardinho'        '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        09  ''      ''       'T_ALV_REEN' 'MENSAGEM'      'Mensagem'            '120' ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

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
