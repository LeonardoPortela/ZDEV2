*&---------------------------------------------------------------------*
*& Report ZPP_REENVIA_TRACE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpp_reenvia_trace.

TYPES: BEGIN OF ty_file,
         id_cotton TYPE zppt0002-id_cotton,
         acharg    TYPE zppt0002-acharg,
       END OF ty_file.

TYPES: BEGIN OF ty_alv_reen,
         status     TYPE icon-id,
         werks      TYPE zppt0006-werks,
         id_cotton  TYPE zppt0006-id_cotton,
         acharg     TYPE zppt0006-acharg,
         lgort      TYPE zppt0006-lgort,
         data       TYPE zppt0006-data,
         cd_safra   TYPE zppt0002-cd_safra,
         cd_sai     TYPE zppt0002-cd_sai,
         flag_envio TYPE zppt0006-flag_envio,
         mensagem   TYPE zppt0006-cd_mensagem.
TYPES: END   OF ty_alv_reen.

DATA: t_excel        LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      t_excel2       LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      vl_file        TYPE rlgrap-filename,
      vmsg(50),
      it_file        TYPE TABLE OF ty_file,
      wa_file        TYPE ty_file,
      t_zppt0002     TYPE TABLE OF zppt0002,
      w_zppt0002     TYPE zppt0002,
      t_zppt0006     TYPE TABLE OF zppt0006,
      w_zppt0006     TYPE zppt0006,
      t_zppt0030     TYPE TABLE OF zppt0030_trace,
      w_zppt0030     TYPE zppt0030_trace,
      t_zppt0006_grp TYPE TABLE OF zppt0006,
      w_zppt0006_grp TYPE zppt0006,
      t_zppt0006_atu TYPE TABLE OF zppt0006,
      w_zppt0006_atu TYPE zppt0006,
      t_zppt0006_pri TYPE TABLE OF zppt0006,
      w_zppt0006_pri TYPE zppt0006,
      t_alv_reen     TYPE TABLE OF ty_alv_reen,
      w_alv_reen     TYPE ty_alv_reen,
      l_jobname      TYPE tbtcjob-jobname,
      l_level        TYPE numc5,
      l_name         TYPE tbtcjob-jobname,
      l_number       TYPE tbtcjob-jobcount.

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
  PARAMETERS  p_file TYPE file_name.
SELECTION-SCREEN END   OF BLOCK a2.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM z_busca_file.

START-OF-SELECTION.

  PERFORM f_upload_file.
  PERFORM f_selecao_trace.
  PERFORM f_reenvia_trace.

FORM f_reenvia_trace.

  DATA: e_fardo  TYPE zpme0059.
  DATA: t_fardo  TYPE zpmt0059,
        lv_json  TYPE string,
        vl_charg TYPE zppt0002-charg.

  DATA: obj_trace TYPE REF TO zcl_webservice_trace.
  FREE: obj_trace.

  CREATE OBJECT: obj_trace.

  DESCRIBE TABLE t_alv_reen LINES DATA(l_lines).

  DATA(l_reenv) = 0.

  SORT t_alv_reen BY id_cotton acharg.

  SORT t_zppt0030 BY id_cotton acharg status_registro DESCENDING.
  DELETE ADJACENT DUPLICATES FROM t_zppt0030 COMPARING id_cotton acharg.

*----------------------------------------------------
* reenvio
*----------------------------------------------------
  LOOP AT t_alv_reen INTO w_alv_reen.

    FREE: t_fardo.

    l_reenv = l_reenv + 1.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = |Reenviando { l_lines } / { l_reenv }|.

    READ TABLE t_zppt0030 INTO w_zppt0030 WITH KEY id_cotton = w_alv_reen-id_cotton
                                                   acharg    = w_alv_reen-acharg.
    IF sy-subrc = 0.

      PERFORM f_cria_job USING w_zppt0030-json_retorno.

*      CALL METHOD obj_trace->atualiza_trace
*        EXPORTING
*          t_fardo       = t_fardo               " Estrutura para retorno
*          i_json        = w_zppt0030-json_retorno
*          id_referencia = 'REENVIO'   "in_ref     " Id Referencia do LOG da Integração
*        RECEIVING
*          ret_code      = DATA(_ret_code).      " Status Code

      CONTINUE.
    ENDIF.

*----------------------------
*-- trata 1o envio
*----------------------------
    READ TABLE t_zppt0006_pri INTO w_zppt0006_atu WITH KEY id_cotton = w_alv_reen-id_cotton.

    IF sy-subrc = 0.
      LOOP AT t_zppt0002 INTO w_zppt0002 WHERE id_cotton = w_zppt0006_atu-id_cotton
                                           AND werks     = w_zppt0006_atu-werks.
*                                          AND charg     = w_zppt0006_atu-charg.

        CLEAR: e_fardo.

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
        CALL METHOD /ui2/cl_json=>serialize
          EXPORTING
            data   = t_fardo
          RECEIVING
            r_json = lv_json.

        PERFORM f_cria_job USING lv_json.

*        CALL METHOD obj_trace->atualiza_trace
*          EXPORTING
*            t_fardo       = t_fardo               " Estrutura para retorno
*            id_referencia = 'REENVIO'   "in_ref     " Id Referencia do LOG da Integração
*          RECEIVING
*            ret_code      = DATA(_ret_code).      " Status Code

*        IF _ret_code = 200.
*          LOOP AT t_zppt0006_pri INTO w_zppt0006_pri WHERE id_cotton = w_alv_reen-id_cotton.
*            w_zppt0006_pri-flag_envio = 'P'.
*            MODIFY zppt0006      FROM w_zppt0006_pri.
*          ENDLOOP.
*        ENDIF.
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
        CALL METHOD /ui2/cl_json=>serialize
          EXPORTING
            data   = t_fardo
          RECEIVING
            r_json = lv_json.

        PERFORM f_cria_job USING lv_json.

*        CALL METHOD obj_trace->atualiza_trace
*          EXPORTING
*            t_fardo       = t_fardo               " Estrutura para retorno
*            id_referencia = 'REENVIO'   "in_ref     " Id Referencia do LOG da Integração
*          RECEIVING
*            ret_code      = _ret_code.      " Status Code

*        IF _ret_code = 200.
*          LOOP AT t_zppt0006_atu INTO w_zppt0006_atu WHERE id_cotton = w_alv_reen-id_cotton.
*            w_zppt0006_atu-flag_envio = 'P'.
*            MODIFY zppt0006      FROM w_zppt0006_atu.
*          ENDLOOP.
*        ENDIF.
      ENDIF.
    ENDIF.

    COMMIT WORK.
  ENDLOOP.

  IF l_reenv > 0.
    MESSAGE s024(sd) WITH 'Reenvio finalizado.'.
  ENDIF.

ENDFORM.

*-----------------------------------------------------------------------------
* selecao
*-----------------------------------------------------------------------------
FORM f_selecao_trace.

  SELECT *
  FROM zppt0030_trace
  INTO TABLE t_zppt0030
   FOR ALL ENTRIES IN it_file
 WHERE id_cotton = it_file-id_cotton.

  SELECT *
  FROM zppt0006
  INTO TABLE t_zppt0006
   FOR ALL ENTRIES IN it_file
 WHERE id_cotton = it_file-id_cotton.

  CHECK sy-subrc = 0.

  SELECT *
    FROM zppt0002
    INTO TABLE t_zppt0002
     FOR ALL ENTRIES IN t_zppt0006
   WHERE id_cotton = t_zppt0006-id_cotton
     AND werks     = t_zppt0006-werks.

  t_zppt0006_grp[] = t_zppt0006[].

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
    CLEAR w_zppt0002.

    LOOP AT t_zppt0002 INTO w_zppt0002 WHERE werks = w_zppt0006_grp-werks
                                         AND id_cotton = w_zppt0006_grp-id_cotton.

      IF  w_zppt0002-id_cotton = w_zppt0006_grp-id_cotton AND w_zppt0002-werks = w_zppt0006_grp-werks.

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
    ENDLOOP.
  ENDLOOP.

ENDFORM.


FORM z_busca_file .

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = 'C:\'
      mask             = '*.XLS'
      mode             = 'O'
      title            = 'Busca de Arquivo'
    IMPORTING
      filename         = vl_file
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  MOVE vl_file TO p_file.

ENDFORM.

FORM f_upload_file.

  CHECK NOT p_file IS INITIAL.

  DATA: vl_file  TYPE rlgrap-filename,
        vmsg(50).

  REFRESH: t_excel, it_file.

  MOVE p_file TO vl_file.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = vl_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 5
      i_end_row               = 60000
    TABLES
      intern                  = t_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Carregando Planilha...'.

  DELETE t_excel WHERE col <> 3 AND col <> 4.

  IF t_excel[] IS NOT INITIAL.
    t_excel2[] = t_excel[].

    SORT t_excel2 BY row col.

    DELETE ADJACENT DUPLICATES FROM t_excel COMPARING row.

    LOOP AT t_excel.
      LOOP AT t_excel2 WHERE row = t_excel-row.
        CASE t_excel2-col.
          WHEN 3. MOVE t_excel2-value TO wa_file-id_cotton.
          WHEN 4. MOVE t_excel2-value TO wa_file-acharg.
        ENDCASE.
      ENDLOOP.
      APPEND wa_file TO it_file.
    ENDLOOP.
  ENDIF.

  SORT it_file BY id_cotton acharg.

ENDFORM.

FORM f_cria_job USING p_json.

  l_jobname = |REENVIA_TRACE|.
  l_level   = l_level + 1.
  l_name    = l_jobname && '_' && l_level.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = l_name
    IMPORTING
      jobcount         = l_number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc IS INITIAL.
    SUBMIT ztestejt0005 WITH p_json = p_json
                     VIA JOB l_name
                      NUMBER l_number
                         AND RETURN.
  ENDIF.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = l_number
        jobname              = l_name
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.
  ENDIF.


ENDFORM.
