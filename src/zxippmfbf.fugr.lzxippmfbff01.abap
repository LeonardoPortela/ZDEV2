*----------------------------------------------------------------------*
***INCLUDE LZXIPPMFBFF01 .
*----------------------------------------------------------------------*
*
*
TABLES: zpps_ximfbf_log.

*&---------------------------------------------------------------------*
*&      Form  YF_INICIALIZACOES
*&---------------------------------------------------------------------*
FORM yf_inicializacoes.

  REFRESH: yt_log_mfpf,
           yt_geo_oubound_ok,
           yt_geo_oubound_erro,
           yt_goodsmovements.

  CLEAR:   yt_log_mfpf,
           yt_geo_oubound_ok,
           yt_geo_oubound_erro,
           yt_goodsmovements,
           w_bapiret2.

  CLEAR:   vc_ano_doc_contabil,
           vc_dt_lacto_gerado,
           vc_ult_doc_gerado,
           vc_empr_doc_gerado.

  PERFORM yf_limpa_ctrl_coletor_ordprod.

ENDFORM.                    " YF_INICIALIZACOES

*&---------------------------------------------------------------------*
*&      Form  YF_ITENS_ENTRADA_CONSUMO
*&---------------------------------------------------------------------*
FORM yf_itens_entrada_consumo CHANGING pf_erro.

***********
* Preenche Header da BAPI MFBF
***********
  DATA: vl_nrobol        TYPE c LENGTH 20.
  CLEAR: pf_erro.

  IF vc_aufnr_pai IS INITIAL.

    CLEAR: w_bflushflags,
           w_bflushdatagen.

*   Analisa Entrada e/ou Consumo
    IF w_xi_mfbf-zst_atlz = cc_i.
*     Entrada
      PERFORM yf_obtem_dados_ordemprod USING cc_x.
      IF vc_aufnr_pai IS INITIAL.
        pf_erro = cc_x.
        EXIT.
      ENDIF.
      w_bflushflags-bckfltype     = cc_tipo_conf_01.
      w_bflushdatagen-storageloc  = w_xi_mfbf-cd_safra.
    ELSE.
*     Consumo
      PERFORM yf_obtem_dados_ordemprod USING space.
      IF vc_aufnr_pai IS INITIAL.
        pf_erro = cc_x.
        EXIT.
      ENDIF.
      w_bflushflags-bckfltype     = cc_tipo_conf_11.
      w_bflushdatagen-storageloc  = w_xi_mfbf-cd_safra.
    ENDIF.

    w_bflushdatagen-materialnr   = vc_matnr_pai.
    w_bflushdatagen-prodplant    = vc_werks_pai.
    w_bflushdatagen-prodversion  = vc_verid_pai.
    w_bflushdatagen-backflquant  = w_xi_mfbf-qteprod.
    w_bflushdatagen-pdc_number   = cc_pdc_number.
    w_bflushdatagen-postdate     = w_xi_mfbf-dtmvto.
    w_bflushdatagen-docdate      = sy-datum.
    w_bflushdatagen-batch        = w_xi_mfbf-charg.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = w_xi_mfbf-nrobol
      IMPORTING
        output = vl_nrobol.
    CONCATENATE vl_nrobol
                '-'
                w_xi_mfbf-fgorigem
                INTO w_bflushdatagen-docheadertxt
                SEPARATED BY space.

    vc_ano_doc_contabil         = sy-datum(4).
    vc_lgort_pai                = w_bflushdatagen-storageloc.

  ENDIF.

***********
* Preenche Itens da BAPI MFBF
***********
  CHECK: w_bflushflags-bckfltype = cc_tipo_conf_11.

  PERFORM yf_check_item_consumo USING w_xi_mfbf-matnr
                                      w_xi_mfbf-werks
                                      w_xi_mfbf-cd_safra
                                      w_xi_mfbf-qteprod
                             CHANGING yt_goodsmovements-material_long "YT_GOODSMOVEMENTS-MATERIAL " >> ---> S4 Migration - 07/07/2023 - RZ
                                      yt_goodsmovements-entry_uom.

*  IF YT_GOODSMOVEMENTS-MATERIAL IS INITIAL.     " >> ---> S4 Migration - 07/07/2023 - RZ
  IF yt_goodsmovements-material_long IS INITIAL. " >> ---> S4 Migration - 07/07/2023 - RZ
    pf_erro = cc_x.
    EXIT.
  ENDIF.

  yt_goodsmovements-plant     = w_xi_mfbf-werks.
  yt_goodsmovements-stge_loc  = w_xi_mfbf-cd_safra.
  yt_goodsmovements-move_type = cc_bwart_261.
  yt_goodsmovements-entry_qnt = w_xi_mfbf-qteprod.
  yt_goodsmovements-item_text = w_xi_mfbf-id_matgeo.

*  SELECT SINGLE meins
*    INTO yt_goodsmovements-entry_uom
*    FROM mara
*   WHERE matnr = yt_goodsmovements-material.

* Anexa itens
  APPEND yt_goodsmovements.

ENDFORM.                    " YF_ITENS_ENTRADA_CONSUMO

*&---------------------------------------------------------------------*
*&      Form  YF_OBTEM_DADOS_ORDEMPROD
*&---------------------------------------------------------------------*
FORM yf_obtem_dados_ordemprod USING VALUE(p_check_matr_werks).

  DATA: lc_matnr   TYPE matnr.

* Limpa controle de coletor
  PERFORM yf_limpa_ctrl_coletor_ordprod.

* Obtem dados de produção da ordem (Coletor)
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = w_xi_mfbf-aufnr
    IMPORTING
      output = vc_aufnr_pai.

  SELECT SINGLE verid prwrk pmatn
    INTO (vc_verid_pai, vc_werks_pai, vc_matnr_pai)
    FROM ckmlmv013
   WHERE aufnr = vc_aufnr_pai.

  IF NOT sy-subrc IS INITIAL.
    PERFORM yf_limpa_ctrl_coletor_ordprod.
    REFRESH: yt_execretrn.
    CLEAR:   yt_execretrn.
    yt_execretrn-type    = cc_e.
    yt_execretrn-id      = cc_classe_mensagem.
    yt_execretrn-number  = '000'.
    yt_execretrn-message = TEXT-m05.
    yt_execretrn-message_v1 = w_xi_mfbf-aufnr.
    REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
             WITH w_xi_mfbf-aufnr.
    APPEND yt_execretrn.
    IF w_xi_mfbf-zst_atlz = cc_i OR w_xi_mfbf-zst_atlz = cc_e.
*     Log de Entrada ou Estorno
      PERFORM yf_distribui_log_xi USING cc_tpexec_01.
    ELSEIF w_xi_mfbf-zst_atlz = cc_c.
*     Log de Consumo
      PERFORM yf_distribui_log_xi USING cc_tpexec_02.
    ENDIF.
    EXIT.
  ENDIF.

  CHECK:p_check_matr_werks = cc_x.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = w_xi_mfbf-matnr
    IMPORTING
      output = lc_matnr.

  IF vc_matnr_pai <> lc_matnr OR  vc_werks_pai <> w_xi_mfbf-werks.
    PERFORM yf_limpa_ctrl_coletor_ordprod.
    REFRESH: yt_execretrn.
    CLEAR:   yt_execretrn.
    yt_execretrn-type    = cc_e.
    yt_execretrn-id      = cc_classe_mensagem.
    yt_execretrn-number  = '000'.
    yt_execretrn-message = TEXT-m06.
    yt_execretrn-message_v1 = w_xi_mfbf-matnr.
    yt_execretrn-message_v2 = w_xi_mfbf-werks.
    yt_execretrn-message_v3 = w_xi_mfbf-aufnr.
    REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
             WITH w_xi_mfbf-matnr.
    REPLACE FIRST OCCURRENCE OF '&2' IN yt_execretrn-message
             WITH w_xi_mfbf-werks.
    REPLACE FIRST OCCURRENCE OF '&3' IN yt_execretrn-message
             WITH w_xi_mfbf-aufnr.
    APPEND yt_execretrn.
    IF w_xi_mfbf-zst_atlz = cc_i OR w_xi_mfbf-zst_atlz = cc_e.
*     Log de Entrada ou Estorno
      PERFORM yf_distribui_log_xi USING cc_tpexec_01.
    ELSEIF w_xi_mfbf-zst_atlz = cc_c.
*     Log de Consumo
      PERFORM yf_distribui_log_xi USING cc_tpexec_02.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_OBTEM_DADOS_ORDEMPROD

*&---------------------------------------------------------------------*
*&      Form  YF_CHECK_ITEM_CONSUMO
*&---------------------------------------------------------------------*
FORM yf_check_item_consumo USING p_check_matnr
                                 p_check_werks
                                 p_check_lgort
                                 p_check_qtde
                        CHANGING p_matnr_out
                                 p_entry_uom.

  DATA: lq_labst TYPE labst.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = p_check_matnr
    IMPORTING
      output = p_matnr_out.

  SELECT COUNT( * )
    FROM mara
   WHERE matnr = p_matnr_out.

  IF NOT sy-subrc IS INITIAL.
    yt_execretrn-type    = cc_e.
    yt_execretrn-id      = cc_classe_mensagem.
    yt_execretrn-number  = '000'.
    yt_execretrn-message = TEXT-m09.
    yt_execretrn-message_v1 = p_check_matnr.
    REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
             WITH p_check_matnr.
    APPEND yt_execretrn.
    PERFORM yf_distribui_log_xi USING cc_tpexec_02.
    CLEAR p_matnr_out.
    CLEAR p_entry_uom.
    EXIT.
  ENDIF.

  IF p_matnr_out = vc_matnr_pai.
    yt_execretrn-type    = cc_e.
    yt_execretrn-id      = cc_classe_mensagem.
    yt_execretrn-number  = '000'.
    yt_execretrn-message = TEXT-m10.
    yt_execretrn-message_v1 = p_check_matnr.
    yt_execretrn-message_v2 = vc_matnr_pai.
    yt_execretrn-message_v3 = vc_aufnr_pai.
    REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
             WITH p_check_matnr.
    REPLACE FIRST OCCURRENCE OF '&2' IN yt_execretrn-message
             WITH vc_matnr_pai.
    REPLACE FIRST OCCURRENCE OF '&3' IN yt_execretrn-message
             WITH vc_aufnr_pai.
    APPEND yt_execretrn.
    PERFORM yf_distribui_log_xi USING cc_tpexec_02.
    CLEAR p_matnr_out.
    CLEAR p_entry_uom.
    EXIT.
  ENDIF.

  SELECT SINGLE labst
    INTO lq_labst
    FROM mard
   WHERE matnr = p_matnr_out
     AND werks = p_check_werks
     AND lgort = p_check_lgort.

  IF lq_labst < p_check_qtde.
    yt_execretrn-type    = cc_e.
    yt_execretrn-id      = cc_classe_mensagem.
    yt_execretrn-number  = '000'.
    yt_execretrn-message = TEXT-m11.
    yt_execretrn-message_v1 = p_check_lgort.
    yt_execretrn-message_v2 = p_check_matnr.
    REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
             WITH p_check_lgort.
    REPLACE FIRST OCCURRENCE OF '&2' IN yt_execretrn-message
             WITH p_check_matnr.
    APPEND yt_execretrn.
    PERFORM yf_distribui_log_xi USING cc_tpexec_02.
    CLEAR p_matnr_out.
    CLEAR p_entry_uom.
    EXIT.
  ENDIF.

  SELECT SINGLE meins
    INTO p_entry_uom
    FROM mara
   WHERE matnr = p_matnr_out.

  PERFORM yf_check_decimal_point USING p_check_qtde
                                       p_entry_uom
                                       p_check_matnr
                                       p_check_lgort
                                 CHANGING p_entry_uom.
  IF p_entry_uom IS INITIAL.
    CLEAR p_matnr_out.
    EXIT.
  ENDIF.

ENDFORM.                    " YF_CHECK_ITEM_CONSUMO



*&---------------------------------------------------------------------*
*&      Form  CHECK_DECIMAL_POINT
*&---------------------------------------------------------------------*
*       Überprüft die Anzahl der Dezimalstelle
*----------------------------------------------------------------------*
*      -->P_ERFMG  Menge                                               *
*      -->P_ERFME  Einheit                                             *
*----------------------------------------------------------------------*
FORM yf_check_decimal_point USING p_erfmg TYPE cowb_comp-erfmg
                                  p_erfme TYPE cowb_comp-erfme
                                  p_check_matnr TYPE mara-matnr
                                  p_check_lgort TYPE mseg-lgort
                            CHANGING p_entry_uom_out.

* Check whether too many decimals are entered for used unit of measure
  CALL FUNCTION 'CO_R0_CHECK_DECIMAL_POINT'
    EXPORTING
      i_quantity = p_erfmg
      i_unit     = p_erfme
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc <> 0.
    CLEAR p_entry_uom_out.
*    Error found: Too many decimals entered
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    yt_execretrn-type    = cc_e.
    yt_execretrn-id      = cc_classe_mensagem.
    yt_execretrn-number  = '000'.
    yt_execretrn-message = TEXT-m13.
    yt_execretrn-message_v1 = p_check_lgort.
    yt_execretrn-message_v2 = p_check_matnr.
    REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
             WITH p_check_matnr.
    APPEND yt_execretrn.
    PERFORM yf_distribui_log_xi USING cc_tpexec_02.
  ENDIF.

ENDFORM.                               " CHECK_DECIMAL_POINT
*&---------------------------------------------------------------------*
*&      Form  YF_LIMPA_CTRL_COLETOR_ORDPROD
*&---------------------------------------------------------------------*
FORM yf_limpa_ctrl_coletor_ordprod .

* Limpa controle de coletor para ordem de produção
  CLEAR: vc_aufnr_pai,
         vc_matnr_pai,
         vc_werks_pai,
         vc_lgort_pai,
         vc_verid_pai.

ENDFORM.                    " YF_LIMPA_CTRL_COLETOR_ORDPROD

*&---------------------------------------------------------------------*
*&      Form  YF_EXEC_BAPI_ENTRADA_CONSUMO
*&---------------------------------------------------------------------*
FORM yf_exec_bapi_entrada_consumo .

  DATA: w_goodsmovements    TYPE bapi2017_gm_item_create,
        w_return            TYPE bapiret2,
        wa_zpps_goodsmv_log TYPE zpps_goodsmv_log,
        wa_zpps_retmvt_log  TYPE zpps_retmvt_log.

  CLEAR: vc_confirmation,
         vc_dt_lacto_gerado,
         vc_ult_doc_gerado,
         vc_empr_doc_gerado.

  CHECK: NOT vc_aufnr_pai IS INITIAL.

  IF w_bflushflags-bckfltype = cc_tipo_conf_11.
    CHECK: NOT yt_goodsmovements[] IS INITIAL.
  ENDIF.

  CLEAR: sy-msgid, sy-msgty, sy-msgno, sy-msgv1,
         sy-msgv2, sy-msgv3, sy-msgv4.

  " setando parametro para não retornar pela interface de ordem de servico pelo metodo:IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE
  SET PARAMETER ID 'ZINBOUNDGEO' FIELD 'X'.

  CLEAR: wa_zpps_goodsmv_log,
         w_goodsmovements.
  LOOP AT yt_goodsmovements INTO w_goodsmovements.
    MOVE-CORRESPONDING w_goodsmovements TO wa_zpps_goodsmv_log.
    wa_zpps_goodsmv_log-mandt     = sy-mandt.
    wa_zpps_goodsmv_log-data      = sy-datum.
    wa_zpps_goodsmv_log-hora      = sy-uzeit.
    wa_zpps_goodsmv_log-obj_key   = w_xi_mfbf-obj_key.
    wa_zpps_goodsmv_log-matnr     = w_goodsmovements-material.
    wa_zpps_goodsmv_log-docheadertxt = w_bflushdatagen-docheadertxt.
    wa_zpps_goodsmv_log-backflquant  = w_bflushdatagen-backflquant.
    MODIFY zpps_goodsmv_log FROM wa_zpps_goodsmv_log.
  ENDLOOP.


  IF w_xi_mfbf-zst_atlz = cc_c.
    PERFORM yf_processa_shdb.
  ELSE.
    "Campos de material tratados. Pseudo comentário adicionado            " >> ---> S4 Migration - 07/07/2023 - RZ
    CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS' "#EC CI_USAGE_OK[2438131] " >> ---> S4 Migration - 07/07/2023 - RZ
      EXPORTING
        bflushflags    = w_bflushflags
        bflushdatagen  = w_bflushdatagen
      IMPORTING
        confirmation   = vc_confirmation
        return         = yt_return
      TABLES
*       SERIALNR       =
        goodsmovements = yt_goodsmovements.
  ENDIF.
*
*  IF yt_return[] IS NOT INITIAL.
*    CLEAR: wa_zpps_retmvt_log,
*           w_return.
*    LOOP AT yt_return INTO w_return.
*      MOVE-CORRESPONDING w_return TO wa_zpps_retmvt_log.
*      wa_zpps_goodsmv_log-mandt     = sy-mandt.
*      wa_zpps_goodsmv_log-data      = sy-datum.
*      wa_zpps_goodsmv_log-hora      = sy-uzeit.
*      wa_zpps_goodsmv_log-obj_key   = w_xi_mfbf-obj_key.
*      wa_zpps_goodsmv_log-docheadertxt = w_bflushdatagen-docheadertxt.
*      MODIFY zpps_retmvt_log FROM wa_zpps_retmvt_log.
*    ENDLOOP.
*  ENDIF.

* Efetiva alterações se sucesso
  IF NOT vc_confirmation IS INITIAL.
    COMMIT WORK AND WAIT.
  ENDIF.

  SET PARAMETER ID 'ZINBOUNDGEO' FIELD space.

* Gera LOG de Processamento
  IF w_xi_mfbf-zst_atlz = cc_i.
    PERFORM yf_gera_log_exec_bapi USING cc_tpexec_01.
  ENDIF.
  IF w_xi_mfbf-zst_atlz = cc_i.
    PERFORM yf_distribui_log_xi USING cc_tpexec_01.
  ELSEIF w_xi_mfbf-zst_atlz = cc_c.
    PERFORM yf_distribui_log_xi USING cc_tpexec_02.
  ENDIF.

* Limpa área de registros processados
  REFRESH: yt_goodsmovements,
          yt_return.

  CLEAR:  yt_goodsmovements,
          w_bflushflags,
          w_bflushdatagen,
          vc_aufnr_pai.

ENDFORM.                    " YF_EXEC_BAPI_ENTRADA_CONSUMO

*&---------------------------------------------------------------------*
*&      Form  YF_DISTRIBUI_LOG_XI
*&---------------------------------------------------------------------*
FORM yf_distribui_log_xi  USING VALUE(p_tipoexec) .

  DATA: ln_mjahr TYPE mjahr,
        ld_budat TYPE budat,
        lc_bukrs TYPE bukrs,
        lc_awkey TYPE awkey,
        lc_belnr TYPE belnr_d.

  DATA: lc_shkzg TYPE shkzg,
        lp_dmbtr TYPE dmbtr,
        lp_dmbe2 TYPE dmbe2,
        lp_dmbe3 TYPE dmbe2,
        lp_matnr TYPE matnr.
  DATA: matgeo        TYPE c LENGTH 38.

  IF p_tipoexec = cc_tpexec_01.

    MOVE: w_xi_mfbf-obj_key           TO yt_log_mfpf-obj_key,
          sy-datum                    TO yt_log_mfpf-dt_atualizacao,
          sy-uzeit                    TO yt_log_mfpf-hr_atualizacao.

    IF w_xi_mfbf-zst_atlz = cc_i.
      MOVE  '09'                      TO yt_log_mfpf-interface.
    ELSE.
      MOVE  '27'                      TO yt_log_mfpf-interface.
    ENDIF.

    LOOP AT yt_execretrn.
      MOVE-CORRESPONDING yt_execretrn TO yt_log_mfpf.
      MOVE: yt_execretrn-number       TO yt_log_mfpf-num.
      APPEND yt_log_mfpf.
    ENDLOOP.

  ELSEIF p_tipoexec = cc_tpexec_02.

    IF vc_dt_lacto_gerado IS INITIAL.
      IF w_xi_mfbf-zst_atlz = cc_e.
*       Busca dados lançamento para estorno
        PERFORM yf_obtem_dados_doctomaterial USING w_xi_mfbf-mblnr
                                                   vc_matnr_pai
                                                   vc_ano_doc_contabil
                                                   cc_bwart_262
                                                   vc_werks_pai
                                                   vc_lgort_pai
                                          CHANGING ln_mjahr
                                                   lc_bukrs
                                                   ld_budat.
      ENDIF.
    ELSE.
      ld_budat = vc_dt_lacto_gerado.
      lc_bukrs = vc_empr_doc_gerado.
    ENDIF.

    IF ( NOT ld_budat IS INITIAL ) AND ( NOT lc_bukrs IS INITIAL ).
      IF NOT vc_ult_doc_gerado IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vc_ult_doc_gerado
          IMPORTING
            output = vc_ult_doc_gerado.
        IF ln_mjahr IS INITIAL.
          ln_mjahr = ld_budat(4).
        ENDIF.
        CONCATENATE vc_ult_doc_gerado ln_mjahr INTO lc_awkey.
        SELECT belnr
          INTO lc_belnr
          FROM bkpf
            UP TO 1 ROWS
         WHERE bukrs = lc_bukrs
           AND gjahr = ln_mjahr
           AND budat = ld_budat
           AND tcode = 'MFBF'
           AND awtyp = 'MKPF'
           AND awkey = lc_awkey.
        ENDSELECT.
      ENDIF.
    ENDIF.

    IF NOT lc_belnr IS INITIAL.
      IF w_xi_mfbf-zst_atlz = cc_c.
        lc_shkzg = 'H'. "Movto 261 - Crédito
      ELSE.
        lc_shkzg = 'S'. "Movto 261 - Débito
      ENDIF.

* ---> S4 Migration - 07/07/2023 - JP
*      SELECT DMBTR DMBE2 DMBE3 MATNR
*        INTO (LP_DMBTR, LP_DMBE2, LP_DMBE3, LP_MATNR)
*        FROM BSEG
*       WHERE BUKRS = LC_BUKRS
*         AND BELNR = LC_BELNR
*         AND GJAHR = LN_MJAHR
*         AND BUZID = 'M'
*         AND SHKZG = LC_SHKZG.
*
      DATA: lv_rldnr TYPE  rldnr,
            lv_bukrs TYPE  bukrs,
            lv_belnr TYPE  belnr_d,
            lv_gjahr TYPE  gjahr.

      lv_bukrs = lc_bukrs.
      lv_belnr = lc_belnr.
      lv_gjahr = ln_mjahr.

      DATA: lt_bseg_aux TYPE fagl_t_bseg.

      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr       = lv_rldnr
        EXCEPTIONS
          not_found     = 1
          more_than_one = 2.

      IF sy-subrc = 0.

        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            i_rldnr   = lv_rldnr
            i_bukrs   = lv_bukrs
            i_belnr   = lv_belnr
            i_gjahr   = lv_gjahr
          IMPORTING
            et_bseg   = lt_bseg_aux
          EXCEPTIONS
            not_found = 1.
      ENDIF.

      IF sy-subrc <> 0 OR lines( lt_bseg_aux ) = 0.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ELSE.

        sy-dbcnt = lines( lt_bseg_aux ).

      ENDIF.


      LOOP AT lt_bseg_aux INTO DATA(w_bseg).

        IF w_bseg-buzid = 'M' AND
           w_bseg-shkzg = lc_shkzg.

          lp_dmbtr = w_bseg-dmbtr.
          lp_dmbe2 = w_bseg-dmbe2.
          lp_dmbe3 = w_bseg-dmbe3.
          lp_matnr = w_bseg-matnr.

* <--- S4 Migration - 07/07/2023 - JP

          "yt_geo_oubound_ok-dmbtr = yt_geo_oubound_ok-dmbtr + lp_dmbtr.
          "yt_geo_oubound_ok-dmbe2 = yt_geo_oubound_ok-dmbe2 + lp_dmbe2.
          "yt_geo_oubound_ok-dmbe3 = yt_geo_oubound_ok-dmbe3 + lp_dmbe3.
          yt_geo_oubound_ok-dmbtr = lp_dmbtr.
          yt_geo_oubound_ok-dmbe2 = lp_dmbe2.
          yt_geo_oubound_ok-dmbe3 = lp_dmbe3.
          yt_geo_oubound_ok-matnr = lp_matnr.
          " Recuperando o ID_MATGEO para o retorno do sucesso..
          LOOP AT yt_goodsmovements.
*          IF YT_GOODSMOVEMENTS-MATERIAL = LP_MATNR.     " >> ---> S4 Migration - 07/07/2023 - RZ
            IF yt_goodsmovements-material_long = lp_matnr. " >> ---> S4 Migration - 07/07/2023 - RZ
              yt_geo_oubound_ok-id_matgeo = yt_goodsmovements-item_text.
            ENDIF.
          ENDLOOP.


          " Alteração para permitir retorno de varios itens qd sucesso baixa de consumo por ordem de producao para o GEO
          IF yt_execretrn-type = cc_s AND w_xi_mfbf-zst_atlz = cc_c.
            MOVE: w_xi_mfbf-obj_key         TO yt_geo_oubound_ok-idbol,
                  w_xi_mfbf-nrobol          TO yt_geo_oubound_ok-nrobol,
                  w_xi_mfbf-fgorigem        TO yt_geo_oubound_ok-fgorigem,
                  w_xi_mfbf-dtmvto          TO yt_geo_oubound_ok-erdat,
                  "w_xi_mfbf-id_matgeo       TO yt_geo_oubound_ok-id_matgeo,
                  "w_xi_mfbf-matnr           TO yt_geo_oubound_ok-matnr,
                  sy-datum                  TO yt_geo_oubound_ok-dtproc,
                  sy-uzeit                  TO yt_geo_oubound_ok-hrproc,
                  cc_bwart_261              TO yt_geo_oubound_ok-bwart,

                  lc_awkey                  TO yt_geo_oubound_ok-docmat.

*          CONCATENATE yt_execretrn-message_v2 yt_execretrn-message_v3
*                 INTO yt_geo_oubound_ok-docmat.
            APPEND yt_geo_oubound_ok.
          ENDIF.

          CLEAR: lp_dmbtr, lp_dmbe2, lp_dmbe3.

* ---> S4 Migration - 07/07/2023 - JP
*      ENDSELECT.
        ENDIF.
      ENDLOOP.

* <--- S4 Migration - 07/07/2023 - JP
    ENDIF.

    LOOP AT yt_execretrn.
*      IF yt_execretrn-type = cc_s and w_xi_mfbf-zst_atlz ne cc_c.
*        MOVE: w_xi_mfbf-obj_key         TO yt_geo_oubound_ok-idbol,
*              w_xi_mfbf-nrobol          TO yt_geo_oubound_ok-nrobol,
*              w_xi_mfbf-fgorigem        TO yt_geo_oubound_ok-fgorigem,
*              w_xi_mfbf-dtmvto          TO yt_geo_oubound_ok-erdat,
*              w_xi_mfbf-id_matgeo       TO yt_geo_oubound_ok-id_matgeo,
*              vc_matnr_pai              TO yt_geo_oubound_ok-matnr,
*              sy-datum                  TO yt_geo_oubound_ok-dtproc,
*              sy-uzeit                  TO yt_geo_oubound_ok-hrproc.
*        IF w_xi_mfbf-zst_atlz = cc_i.
*          MOVE cc_bwart_131             TO yt_geo_oubound_ok-bwart.
*        ELSEIF  w_xi_mfbf-zst_atlz = cc_c.
*          MOVE cc_bwart_261             TO yt_geo_oubound_ok-bwart.
*        ELSEIF  w_xi_mfbf-zst_atlz = cc_e.
*          MOVE cc_bwart_262             TO yt_geo_oubound_ok-bwart.
*        ENDIF.
*        CONCATENATE yt_execretrn-message_v2 yt_execretrn-message_v3
*               INTO yt_geo_oubound_ok-docmat.
*        APPEND yt_geo_oubound_ok.
*      ELSEIF
      IF yt_execretrn-type = cc_e.
        MOVE-CORRESPONDING yt_execretrn TO yt_geo_oubound_erro.
        MOVE: yt_execretrn-number       TO yt_geo_oubound_erro-nromsg,
              w_xi_mfbf-obj_key         TO yt_geo_oubound_erro-idbol,
              w_xi_mfbf-nrobol          TO yt_geo_oubound_erro-nrobol,
              w_xi_mfbf-fgorigem        TO yt_geo_oubound_erro-fgorigem,
              w_xi_mfbf-dtmvto          TO yt_geo_oubound_erro-erdat,
              sy-uzeit                  TO yt_geo_oubound_erro-erzet,
              sy-datum                  TO yt_geo_oubound_erro-dtproc,
              sy-uzeit                  TO yt_geo_oubound_erro-hrproc.
        IF w_xi_mfbf-zst_atlz = cc_i.
          MOVE cc_bwart_131             TO yt_geo_oubound_erro-bwart.
        ELSEIF  w_xi_mfbf-zst_atlz = cc_c.
          MOVE cc_bwart_261            TO yt_geo_oubound_erro-bwart.
        ELSEIF  w_xi_mfbf-zst_atlz = cc_e.
          MOVE cc_bwart_262            TO yt_geo_oubound_erro-bwart.
        ENDIF.
        APPEND yt_geo_oubound_erro.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " YF_DISTRIBUI_LOG_XI

*&---------------------------------------------------------------------*
*&      Form  YF_OBTEM_NRCONFIRMACAO
*&---------------------------------------------------------------------*
FORM yf_obtem_nrconfirmacao CHANGING pf_erro.

  DATA: lc_mblnr TYPE mblnr,
        ld_budat TYPE budat,
        ln_mjahr TYPE mjahr,
        lc_bukrs TYPE bukrs.

  CLEAR: pf_erro,
         vc_confirmation.

* Obtem dados da ordem de produção
  PERFORM yf_obtem_dados_ordemprod  USING space.
  IF vc_aufnr_pai IS INITIAL.
    pf_erro = cc_x.
    EXIT.
  ENDIF.

* Obtem data do documento contábil para o documento de material
  PERFORM yf_obtem_dados_doctomaterial USING w_xi_mfbf-mblnr
                                             space
                                             space
                                             space
                                             space
                                             space
                                    CHANGING ln_mjahr
                                             lc_bukrs
                                             ld_budat.
  IF ld_budat IS INITIAL.
    pf_erro = cc_x.
    PERFORM yf_limpa_ctrl_coletor_ordprod.
    REFRESH: yt_execretrn.
    CLEAR:   yt_execretrn.
    yt_execretrn-type    = cc_e.
    yt_execretrn-id      = cc_classe_mensagem.
    yt_execretrn-number  = '000'.
    yt_execretrn-message_v1 = w_xi_mfbf-mblnr.
    yt_execretrn-message = TEXT-m12.
    REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
             WITH w_xi_mfbf-mblnr.
    APPEND yt_execretrn.
*   Log para Estorno
    PERFORM yf_distribui_log_xi USING cc_tpexec_01.
    EXIT.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = w_xi_mfbf-mblnr
    IMPORTING
      output = lc_mblnr.

* Obtem a confirmação para o documento de material
  SELECT t1~prtnr
    INTO vc_confirmation
    FROM blpk AS t1
   INNER JOIN blpp AS t2
      ON t2~prtnr = t1~prtnr
     AND t2~belnr = lc_mblnr
      UP TO 1 ROWS
  WHERE t1~matnr = vc_matnr_pai
    AND t1~werks = vc_werks_pai
    AND t1~verid = vc_verid_pai
    AND t1~budat = ld_budat.
  ENDSELECT.

* Limpa o Coletor
  PERFORM yf_limpa_ctrl_coletor_ordprod.

  IF vc_confirmation IS INITIAL.
    pf_erro = cc_x.
    REFRESH: yt_execretrn.
    CLEAR:   yt_execretrn.
    yt_execretrn-type    = cc_e.
    yt_execretrn-id      = cc_classe_mensagem.
    yt_execretrn-number  = '000'.
    yt_execretrn-message_v1 = w_xi_mfbf-mblnr.
    IF ld_budat IS INITIAL.
      yt_execretrn-message = TEXT-m07.
    ELSE.
      yt_execretrn-message = TEXT-m08.
    ENDIF.
    REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
             WITH w_xi_mfbf-mblnr.
    APPEND yt_execretrn.
*   Log para Estorno
    PERFORM yf_distribui_log_xi USING cc_tpexec_01.
  ENDIF.

ENDFORM.                    " YF_OBTEM_NRCONFIRMACAO

*&---------------------------------------------------------------------*
*&      Form  YF_EXEC_BAPI_ESTORNO
*&---------------------------------------------------------------------*
FORM yf_exec_bapi_estorno .

  CHECK: NOT vc_confirmation IS INITIAL.

  CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
    EXPORTING
      confirmation     = vc_confirmation
*     POSTDATE         =
*     CANC_PDCOLLNR    =
    IMPORTING
      cancconfirmation = vc_cancconfirm
      return           = w_bapiret2.

* Efetiva alterações se sucesso
  IF NOT vc_cancconfirm IS INITIAL.
    COMMIT WORK AND WAIT.
  ENDIF.

* Gera LOG de Processamento
  PERFORM yf_gera_log_exec_bapi USING cc_tpexec_02.
  PERFORM yf_distribui_log_xi   USING cc_tpexec_01.

* Limpa área de registros processados
  CLEAR:  w_bapiret2, vc_aufnr_pai.

ENDFORM.                    " YF_EXEC_BAPI_ESTORNO

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_LOG_EXEC_BAPI
*&---------------------------------------------------------------------*
FORM yf_gera_log_exec_bapi USING VALUE(p_tipoexec).

  DATA: lf_msgerro VALUE space,
        lc_belnr   TYPE belnr_d.

  REFRESH: yt_execretrn.

  CLEAR:   yt_execretrn,
           vc_dt_lacto_gerado,
           vc_ult_doc_gerado.

* Analisa retorno da BAPI
  IF p_tipoexec = cc_tpexec_01.

    IF vc_confirmation IS  INITIAL.
*     Erro na execução da BAPI entrada/consumo
      IF yt_return[] IS INITIAL.
        IF NOT sy-msgid IS INITIAL AND NOT sy-msgno IS INITIAL.
          yt_execretrn-type    = cc_e.
          yt_execretrn-id      = sy-msgid.
          yt_execretrn-number  = sy-msgno.
          yt_execretrn-message_v1 = sy-msgv1.
          yt_execretrn-message_v2 = sy-msgv2.
          yt_execretrn-message_v3 = sy-msgv3.
          yt_execretrn-message_v4 = sy-msgv4.
          MESSAGE ID sy-msgid TYPE cc_e NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  INTO yt_execretrn-message.
          APPEND yt_execretrn.
        ENDIF.
        yt_execretrn-type    = cc_e.
        yt_execretrn-id      = cc_classe_mensagem.
        yt_execretrn-number  = '000'.
        yt_execretrn-message = TEXT-m01.
        APPEND yt_execretrn.
      ELSE.
        READ TABLE yt_return WITH KEY type = cc_e
                             TRANSPORTING NO FIELDS.
        IF sy-subrc IS INITIAL.
          lf_msgerro = cc_x.
        ENDIF.
        LOOP AT yt_return.
          MOVE-CORRESPONDING yt_return TO yt_execretrn.
          IF lf_msgerro = cc_x.
*           Grava LOG somente das mensagens de erro
            IF yt_return-type = cc_e.
              APPEND yt_execretrn.
            ENDIF.
          ELSE.
*           Grava LOG de todas as mensagens
            yt_execretrn-type = cc_e.
            APPEND yt_execretrn.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
*     Sucesso na execução da BAPI entrada/consumo
      yt_execretrn-type    = cc_s.
      yt_execretrn-id      = cc_classe_mensagem.
      yt_execretrn-number  = '000'.
      yt_execretrn-message = TEXT-m02.
      yt_execretrn-message_v1 = vc_confirmation.
      PERFORM yf_recupera_docto_material
              CHANGING vc_ult_doc_gerado
                       yt_execretrn-message_v3
                       vc_dt_lacto_gerado
                       vc_empr_doc_gerado.
      IF vc_ult_doc_gerado IS INITIAL.
        yt_execretrn-type    = cc_e.
        yt_execretrn-message = TEXT-m03.
      ELSE.
        yt_execretrn-message_v2 = vc_ult_doc_gerado.
        REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
                WITH vc_confirmation.
        REPLACE FIRST OCCURRENCE OF '&2' IN yt_execretrn-message
                WITH yt_execretrn-message_v2.
        REPLACE FIRST OCCURRENCE OF '&3' IN yt_execretrn-message
                WITH yt_execretrn-message_v3.
      ENDIF.
      APPEND yt_execretrn.
    ENDIF.

  ELSEIF p_tipoexec = cc_tpexec_02.
*   Analisa retorno da BAPI de estorno
    IF vc_cancconfirm IS  INITIAL.
*     Erro na execução da BAPI estorno
      MOVE-CORRESPONDING w_bapiret2 TO yt_execretrn.
      yt_execretrn-type   = cc_e.
      yt_execretrn-number = w_bapiret2-number.
      APPEND yt_execretrn.
    ELSE.
*     Sucesso na execução da BAPI estorno
      SELECT MAX( belnr )
        INTO lc_belnr
        FROM blpp
       WHERE prtnr = vc_cancconfirm.
      yt_execretrn-type       = cc_s.
      yt_execretrn-id         = cc_classe_mensagem.
      yt_execretrn-number     = '000'.
      yt_execretrn-message    = TEXT-m04.
      yt_execretrn-message_v1 = vc_confirmation.
      yt_execretrn-message_v2 = vc_cancconfirm.
      yt_execretrn-message_v3 = lc_belnr.
      REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
               WITH vc_confirmation.
      REPLACE FIRST OCCURRENCE OF '&2' IN yt_execretrn-message
               WITH vc_cancconfirm.
      REPLACE FIRST OCCURRENCE OF '&3' IN yt_execretrn-message
               WITH lc_belnr.
      APPEND yt_execretrn.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_GERA_LOG_EXEC_BAPI

*&---------------------------------------------------------------------*
*&      Form  YF_RECUPERA_DOCTO_MATERIAL
*&---------------------------------------------------------------------*
FORM yf_recupera_docto_material  CHANGING s_docto_matnr
                                          s_docto_mjahr
                                          s_docto_budat
                                          s_docto_bukrs.

  DATA: lc_mblnr TYPE mblnr  VALUE space,
        ln_mjahr TYPE mjahr  VALUE '0000',
        lc_bwart TYPE bwart,
        ld_budat TYPE budat,
        lc_matnr TYPE matnr,
        lc_werks TYPE werks_d,
        lc_lgort TYPE lgort_d,
        lc_bukrs TYPE bukrs.

  CLEAR: s_docto_matnr,
         s_docto_mjahr,
         s_docto_budat,
         s_docto_bukrs.

* Obtem Item para extração do documento de material
  READ TABLE yt_goodsmovements INDEX 1.
  IF sy-subrc IS INITIAL.
    lc_bwart = cc_bwart_261.
    lc_matnr = yt_goodsmovements-material.
    lc_werks = yt_goodsmovements-plant.
    lc_lgort = yt_goodsmovements-stge_loc.
  ELSE.
    lc_bwart = cc_bwart_131.
    lc_matnr = vc_matnr_pai.
    lc_werks = vc_werks_pai.
    lc_lgort = vc_lgort_pai.
  ENDIF.

* Busca o documento de material em parâmetro de memória
*  GET PARAMETER ID 'MBN' FIELD lc_mblnr.

* Busca o documento de material
  IF vc_ano_doc_contabil IS INITIAL.
    vc_ano_doc_contabil = sy-datum(4).
  ENDIF.

* Obtem data do documento de material baseado nos parametros do boletim atual processado..
  SELECT MAX( t1~mblnr )
    INTO lc_mblnr
    FROM mseg AS t1
   INNER JOIN mkpf AS t2
      ON t2~mblnr = t1~mblnr
     AND t2~mjahr = t1~mjahr
     AND t2~bktxt = w_bflushdatagen-docheadertxt
   WHERE t1~mjahr = vc_ano_doc_contabil
     AND t1~lgort =  lc_lgort
     AND t1~bwart = lc_bwart
     AND t1~matnr = lc_matnr
     AND t1~werks = lc_werks
     AND t1~aufnr = vc_aufnr_pai.

  CHECK: sy-subrc IS INITIAL.

  IF NOT lc_mblnr IS INITIAL.
*   Valida se o ID é para o mesmo documento e obtem o ano
    PERFORM yf_obtem_dados_doctomaterial USING lc_mblnr
                                               lc_matnr
                                               vc_ano_doc_contabil
                                               lc_bwart
                                               lc_werks
                                               lc_lgort
                                      CHANGING ln_mjahr
                                               lc_bukrs
                                               ld_budat.
    IF NOT ln_mjahr IS INITIAL.
      s_docto_matnr = lc_mblnr.
      s_docto_mjahr = ln_mjahr.
      s_docto_budat = ld_budat.
      s_docto_bukrs = lc_bukrs.
*     Ok documento encontrado, encerra processamento
      EXIT.
    ENDIF.
  ENDIF.


  SELECT bukrs
         MAX( mblnr )
         MAX( mjahr )
    INTO (lc_bukrs, lc_mblnr, ln_mjahr)
    FROM mseg
   WHERE mjahr >= vc_ano_doc_contabil
     AND bwart  = lc_bwart
     AND matnr  = lc_matnr
     AND werks  = lc_werks
     AND lgort  = lc_lgort
     AND aufnr  = vc_aufnr_pai
    GROUP BY bukrs mblnr mjahr.
  ENDSELECT.

  CHECK: sy-subrc IS INITIAL.

  SELECT SINGLE budat
    INTO ld_budat
    FROM mkpf
   WHERE mblnr = lc_mblnr
     AND mjahr = ln_mjahr.

  s_docto_matnr = lc_mblnr.
  s_docto_mjahr = ln_mjahr.
  s_docto_budat = ld_budat.
  s_docto_bukrs = lc_bukrs.

ENDFORM.                    " YF_RECUPERA_DOCTO_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  YF_OBTEM_DADOS_DOCTOMATERIAL
*&---------------------------------------------------------------------*
FORM yf_obtem_dados_doctomaterial USING pc_mblnr
                                        pc_matnr
                                        pn_anodoc
                                        pc_bwart
                                        pc_werks
                                        pc_lgort
                               CHANGING pn_mjahr
                                        pc_bukrs
                                        pd_budat.

  RANGES: rg_matnr FOR mseg-matnr OCCURS 1,
          rg_mjahr FOR mseg-mjahr OCCURS 1,
          rg_bwart FOR mseg-bwart OCCURS 3,
          rg_werks FOR mseg-werks OCCURS 1,
          rg_lgort FOR mseg-lgort OCCURS 1.

  DATA: ld_budat TYPE budat,
        ln_mjahr TYPE mjahr,
        lc_mblnr TYPE mblnr,
        lc_bukrs TYPE bukrs.

  CLEAR: pn_mjahr,
         pc_bukrs,
         pd_budat.

  CHECK: NOT vc_aufnr_pai IS INITIAL
    AND  NOT pc_mblnr     IS INITIAL.

* Material
  IF NOT pc_matnr IS INITIAL.
    rg_matnr-sign   = cc_i.
    rg_matnr-option = cc_eq.
    rg_matnr-low    = pc_matnr.
    APPEND rg_matnr.
  ENDIF.

* Ano fiscal
  IF NOT pn_anodoc IS INITIAL.
    rg_mjahr-sign   = cc_i.
    rg_mjahr-option = cc_ge.
    rg_mjahr-low    = pn_anodoc.
    APPEND rg_mjahr.
  ENDIF.

* Tipo de Movimento
  IF pc_bwart IS INITIAL.
    rg_bwart-sign   = cc_i.
    rg_bwart-option = cc_eq.
    rg_bwart-low    = cc_bwart_131.
    APPEND rg_bwart.
    rg_bwart-low    = cc_bwart_261.
    APPEND rg_bwart.
    rg_bwart-low    = cc_bwart_262.
    APPEND rg_bwart.
  ELSE.
    rg_bwart-sign   = cc_i.
    rg_bwart-option = cc_eq.
    rg_bwart-low    = pc_bwart.
    APPEND rg_bwart.
  ENDIF.

* Centro
  IF NOT pc_werks IS INITIAL.
    rg_werks-sign   = cc_i.
    rg_werks-option = cc_eq.
    rg_werks-low    = pc_werks.
    APPEND rg_werks.
  ENDIF.

* Depósito
  IF NOT pc_lgort IS INITIAL.
    rg_lgort-sign   = cc_i.
    rg_lgort-option = cc_eq.
    rg_lgort-low    = pc_lgort.
    APPEND rg_lgort.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pc_mblnr
    IMPORTING
      output = lc_mblnr.

* Obtem data do documento contábil para o documento de material
  SELECT MAX( t1~mjahr ) t1~bukrs t2~budat
    INTO (ln_mjahr, lc_bukrs, ld_budat)
    FROM mseg AS t1
   INNER JOIN mkpf AS t2
      ON t2~mblnr = t1~mblnr
     AND t2~mjahr = t1~mjahr
      UP TO 1 ROWS
   WHERE t1~mblnr  = lc_mblnr
     AND t1~mjahr IN rg_mjahr
     AND t1~lgort IN rg_lgort
     AND t1~bwart IN rg_bwart
     AND t1~matnr IN rg_matnr
     AND t1~werks IN rg_werks
     AND t1~aufnr  = vc_aufnr_pai
   GROUP BY t1~bukrs t2~budat.
  ENDSELECT.

  CHECK: sy-subrc IS INITIAL.

  pn_mjahr = ln_mjahr.
  pc_bukrs = lc_bukrs.
  pd_budat = ld_budat.

ENDFORM.                    " YF_OBTEM_DADOS_DOCTOMATERIAL

*&---------------------------------------------------------------------*
*&      Form  YF_RETORNA_LOG_INTERFACE_XI
*&---------------------------------------------------------------------*
FORM yf_retorna_log_interface_xi .

  IF NOT yt_geo_oubound_ok[] IS INITIAL.
    CALL FUNCTION 'Z_MM_GRAVA_BOL_SUCESS'
      TABLES
        return_sucess = yt_geo_oubound_ok.

*    CALL FUNCTION 'Z_MM_OUTBOUND_BOL_SUCESS' IN BACKGROUND TASK
*      DESTINATION 'XI_GEO_RETURN_SUCESS'
*      TABLES
*        RETURN_SUCESS = YT_GEO_OUBOUND_OK.
  ENDIF.

  IF NOT yt_geo_oubound_erro[] IS INITIAL.

    CALL FUNCTION 'Z_MM_GRAVA_BOL_ERROR'
      TABLES
        return_error = yt_geo_oubound_erro.

*    CALL FUNCTION 'Z_MM_OUTBOUND_BOL_ERROR' IN BACKGROUND TASK
*      DESTINATION 'XI_GEO_RETURN_ERROR'
*      TABLES
*        RETURN_ERROR = YT_GEO_OUBOUND_ERRO.
  ENDIF.

  IF NOT yt_log_mfpf[] IS INITIAL.
    CALL FUNCTION 'Z_FI_GRAVA_RETURN'
      TABLES
        outreturn = yt_log_mfpf.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        OUTRETURN = YT_LOG_MFPF.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = yt_log_mfpf.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = yt_log_mfpf.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
  ENDIF.

ENDFORM.                    " YF_RETORNA_LOG_INTERFACE_XI

*&---------------------------------------------------------------------*
*&      Form  YF_PROCESSA_SHDB
*&---------------------------------------------------------------------*
*       Processa SHDB para baixa de consumo para ordem de produção
*----------------------------------------------------------------------*
FORM yf_processa_shdb .
  DATA: vl_tabix        TYPE sy-tabix,
        vl_cnt(2)       TYPE n,
        vl_quebra(2)    TYPE n,
        vl_qtde         TYPE c LENGTH 16,
        vl_budat_char   TYPE char10,
        vl_bldat_char   TYPE char10,
        vl_field_mat    TYPE c LENGTH 30,
        vl_field_quant  TYPE c LENGTH 30,
        vl_field_centro TYPE c LENGTH 30,
        vl_field_dep    TYPE c LENGTH 30,
        vl_message      TYPE string,
        vl_mode         TYPE c LENGTH 1.
  DATA: wa_goodsmovements TYPE bapi2017_gm_item_create.

  vl_tabix = sy-tabix.

  REFRESH:
           ti_msg   ,
           ti_bdc   .
  CLEAR wa_goodsmovements.
  CONCATENATE w_bflushdatagen-postdate+6(2) '. ' w_bflushdatagen-postdate+4(2) '.' w_bflushdatagen-postdate(4)
       INTO vl_budat_char.
  CONCATENATE sy-datum+6(2) '. ' sy-datum+4(2) '.' sy-datum(4)
       INTO vl_bldat_char.
*  break-point.
  PERFORM zf_bdc USING:


  'X'   'SAPLBARM'            '0800',
  ' '   'BDC_CURSOR'          'RM61B-RB_KOMPO',
  ' '   'BDC_OKCODE'          '=RBTYP',
  ' '   'RM61B-RB_KOMPO'      'X',
  ' '   'RM61B-BUDAT'         vl_budat_char, "w_xi_mfbf-dtmvto
  ' '   'RM61B-BLDAT'         vl_bldat_char, "w_xi_mfbf-dtmvto
  ' '   'RM61B-WERKS'         w_xi_mfbf-werks,

  'X'   'SAPLBARM'            '0800',
  ' '   'BDC_OKCODE'          '=ISTDA',
  ' '   'RM61B-RB_KOMPO'      'X',
  ' '   'RM61B-BUDAT'         vl_budat_char, "w_xi_mfbf-dtmvto
  ' '   'RM61B-BLDAT'         vl_bldat_char, "w_xi_mfbf-dtmvto
  ' '   'RM61B-BKTXT'         w_bflushdatagen-docheadertxt,
  ' '   'BDC_CURSOR'          'RM61B-VERID',
  ' '   'RM61B-MATNR'         w_bflushdatagen-materialnr,
  ' '   'RM61B-WERKS'         w_xi_mfbf-werks,
  ' '   'RM61B-VERID'         vc_verid_pai,
  ' '   'RM61B-BOM_OFF'       'X',

* loop dos materiais para baixa
  'X'   'SAPLCOWB'            '0130',
  ' '   'BDC_OKCODE'          '/00'.
*  ' '   'BDC_CURSOR'          'COWB_COMP-LGORT(16)'.

  CLEAR: vl_cnt.
  vl_cnt = 1.
  LOOP AT yt_goodsmovements INTO wa_goodsmovements.
    CONCATENATE 'COWB_COMP-MATNR('
                 vl_cnt
                           ')' INTO vl_field_mat.
    CONCATENATE 'COWB_COMP-ERFMG_R('
                 vl_cnt
                           ')' INTO vl_field_quant.
    CONCATENATE 'COWB_COMP-WERKS('
                 vl_cnt
                           ')' INTO vl_field_centro.
    CONCATENATE 'COWB_COMP-LGORT('
                 vl_cnt
                           ')' INTO vl_field_dep.
    "vl_qtde = wa_goodsmovements-ENTRY_QNT.
    WRITE wa_goodsmovements-entry_qnt TO vl_qtde.
*     replace all occurrences of regex ',' in vl_qtde with '.'.
    CONDENSE vl_qtde.
    PERFORM zf_bdc USING:
    ' '   vl_field_mat      wa_goodsmovements-material,
    ' '   vl_field_quant    vl_qtde,
    ' '   vl_field_dep      wa_goodsmovements-stge_loc,
    ' '   vl_field_centro   wa_goodsmovements-plant.
    vl_cnt = vl_cnt + 1.
  ENDLOOP.

  PERFORM zf_bdc USING:
  'X'   'SAPLCOWB'            '0130',
  ' '   'BDC_OKCODE'          '=WEIT',
  ' '   'BDC_SUBSCR'          'SAPLCOWB',

  'X'   'SAPLBARM'            '0800',
  ' '   'BDC_OKCODE'          '/EEND'.

  vl_mode = 'N'.

  CALL TRANSACTION cc_mfbf
     USING ti_bdc
     MODE   vl_mode
     UPDATE cc_s

     MESSAGES INTO ti_msg.
  IF ti_msg[] IS NOT INITIAL.
    READ TABLE ti_msg
       INTO wa_msg
    WITH KEY msgtyp = cc_s.
    IF sy-subrc EQ 0.
      vc_confirmation    = wa_msg-msgv1.
      vc_ult_doc_gerado  = wa_msg-msgv1.
      vc_dt_lacto_gerado = sy-datum.
      SELECT SINGLE bukrs
         INTO vc_empr_doc_gerado
        FROM mseg
      WHERE mblnr = vc_ult_doc_gerado
        AND mjahr = sy-datum(4).
* Sucesso na execução da BAPI entrada/consumo
      yt_execretrn-type    = cc_s.
      yt_execretrn-id      = cc_classe_mensagem.
      yt_execretrn-number  = '000'.
      yt_execretrn-message_v1 = vc_confirmation.
      yt_execretrn-message_v2 = vc_ult_doc_gerado.
      APPEND yt_execretrn.
    ENDIF.
*Gravando log de erro de execução
    LOOP AT ti_msg INTO wa_msg WHERE msgtyp = cc_e.
      CLEAR:   yt_execretrn.
      MOVE-CORRESPONDING wa_msg TO yt_execretrn.
      MESSAGE ID     wa_msg-msgid
              TYPE   wa_msg-msgtyp
              NUMBER wa_msg-msgnr
              WITH wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4
        INTO vl_message.
      yt_execretrn-type    = cc_e.
      yt_execretrn-id      = cc_classe_mensagem.
      yt_execretrn-number  = '000'.
      yt_execretrn-message        = vl_message.
      yt_execretrn-message_v1     = wa_msg-msgv1.
      yt_execretrn-message_v2     = wa_msg-msgv2.
      yt_execretrn-message_v3     = wa_msg-msgv3.
      yt_execretrn-message_v4     = wa_msg-msgv4.
*      yt_execretrn-message = text-m06.
*      yt_execretrn-message_v1 = w_xi_mfbf-matnr.
*      yt_execretrn-message_v2 = w_xi_mfbf-werks.
*      yt_execretrn-message_v3 = w_xi_mfbf-aufnr.
*      replace first occurrence of '&1' in yt_execretrn-message
*               with w_xi_mfbf-matnr.
*      replace first occurrence of '&2' in yt_execretrn-message
*               with w_xi_mfbf-werks.
*      replace first occurrence of '&3' in yt_execretrn-message
*               with w_xi_mfbf-aufnr.

      APPEND yt_execretrn.
    ENDLOOP.
  ENDIF.

ENDFORM.                "YF_PROCESSA_SHDB

*&---------------------------------------------------------------------*
*&      Form  zf_bdc
*&---------------------------------------------------------------------*
FORM zf_bdc USING p_dynbegin TYPE any
                  p_name     TYPE any
                  p_value    TYPE any.

  IF p_dynbegin EQ cc_x.
    wa_bdc-program  = p_name.
    wa_bdc-dynpro   = p_value.
    wa_bdc-dynbegin = p_dynbegin.

    APPEND wa_bdc
      TO ti_bdc.
  ELSE.
    wa_bdc-fnam = p_name.
    wa_bdc-fval = p_value.

    APPEND wa_bdc
      TO ti_bdc.
  ENDIF.

  CLEAR wa_bdc.
ENDFORM.                    " ZF_BDC
