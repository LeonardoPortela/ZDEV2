*----------------------------------------------------------------------*
***INCLUDE Z_PP_MFBF_YF_INICIALIZACOESF01 .
*----------------------------------------------------------------------*

TABLES: zpps_ximfbf_log.
*&---------------------------------------------------------------------*
*&      Form  YF_INICIALIZACOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yf_inicializacoes .

  REFRESH: yt_log_mfpf,
           yt_geo_oubound_ok,
           yt_geo_oubound_erro,
           yt_goodsmovements,
           t_xi_mfbf_aux.

  CLEAR:   yt_log_mfpf,
           yt_geo_oubound_ok,
           yt_geo_oubound_erro,
           yt_goodsmovements,
           w_bapiret2,
           t_xi_mfbf_aux.

  CLEAR:   vc_ano_doc_contabil,
           vc_dt_lacto_gerado,
           vc_ult_doc_gerado,
           vc_empr_doc_gerado.

  PERFORM yf_limpa_ctrl_coletor_ordprod.

  vl_mode = 'N'.

ENDFORM.                    " YF_INICIALIZACOES
*&---------------------------------------------------------------------*
*&      Form  YF_LIMPA_CTRL_COLETOR_ORDPROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yf_limpa_ctrl_coletor_ordprod .
* Limpa controle de coletor para ordem de produção
  CLEAR: vc_aufnr_pai,
         vc_matnr_pai,
         vc_werks_pai,
         vc_lgort_pai,
         vc_verid_pai.

ENDFORM.                    " YF_LIMPA_CTRL_COLETOR_ORDPROD
*&---------------------------------------------------------------------*
*&      Form  YF_ITENS_ENTRADA_CONSUMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VF_ERRO  text
*----------------------------------------------------------------------*
FORM yf_itens_entrada_consumo  CHANGING pf_erro.

***********
* Preenche Header da BAPI MFBF
***********
  DATA: vl_nrobol TYPE c LENGTH 20,
        vl_xchpf  TYPE mara-xchpf,
        vnum(8)   TYPE c,  " "#128647-30.11.2023-JT-inicio
        vseq(8)   TYPE p.  " "#128647-30.11.2023-JT-inicio
  "
  CLEAR: pf_erro.

*------------------
  MESSAGE s024(sd) WITH '**** inicio - Entrada Consumo 1'.
*------------------

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
      w_bflushflags-bckfltype       = cc_tipo_conf_11.
      w_bflushflags-components_type = '1'.
      w_bflushdatagen-storageloc    = w_xi_mfbf-cd_safra.
      "W_BFLUSHDATAGEN-PLANORDER   = W_XI_MFBF-AUFNR.

    ENDIF.

*------------------
    MESSAGE s024(sd) WITH '**** fim - Entrada Consumo 1'.
*------------------

*-CS2022000332-#RIMINI-01.06.2022-inicio
*    IF w_xi_mfbf-charg NE ''.
*      SELECT SINGLE * FROM mcha
*        WHERE matnr EQ w_xi_mfbf-matnr
*        AND werks   EQ w_xi_mfbf-werks
*        AND charg   EQ w_xi_mfbf-charg.
*
*      IF sy-subrc NE 0.
*        pf_erro = cc_x.
*        EXIT.
*      ENDIF.
*    ENDIF.
*-CS2022000332-#RIMINI-01.06.2022-fim

*------------------
    MESSAGE s024(sd) WITH '**** inicio - Entrada Consumo 2'.
*------------------

    w_bflushdatagen-materialnr   = vc_matnr_pai.
    w_bflushdatagen-prodplant    = vc_werks_pai.
    w_bflushdatagen-prodversion  = vc_verid_pai.
    w_bflushdatagen-backflquant  = w_xi_mfbf-qteprod.
    w_bflushdatagen-pdc_number   = cc_pdc_number.
    w_bflushdatagen-postdate     = w_xi_mfbf-dtmvto.
    w_bflushdatagen-docdate      = sy-datum.
    "Algodão RFID
    IF w_xi_mfbf-zst_atlz = cc_i AND   w_xi_mfbf-id_cotton IS NOT INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZID_RFID2'  "'ZID_RFID'   "#128647-30.11.2023-JT-inicio
          toyear      = w_xi_mfbf-dtmvto+0(4)
        IMPORTING
          number      = vseq.
      vnum = vseq .
      "
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vnum
        IMPORTING
          output = vnum.

*-#128647-30.11.2023-JT-inicio
*     CONCATENATE w_xi_mfbf-dtmvto+0(4) vnum INTO  w_bflushdatagen-batch.
      CONCATENATE w_xi_mfbf-dtmvto+2(2) vnum INTO  w_bflushdatagen-batch.
*-#128647-30.11.2023-JT-fim

      CONCATENATE 'RFID' w_xi_mfbf-id_cotton INTO  w_bflushdatagen-docheadertxt.
      DATA(_charg)    = w_xi_mfbf-charg.              "*-CS2022000332-#84404-02.08.2022-JT-inicio
      vf_new_charg    = w_bflushdatagen-batch.
      w_xi_mfbf-charg = w_bflushdatagen-batch.

      UPDATE  zpps_ximfbf_log SET charg = w_xi_mfbf-charg
                        WHERE obj_key   = w_xi_mfbf-obj_key
                          AND charg     = _charg.     "*-CS2022000332-#84404-02.08.2022-JT-inicio
      COMMIT WORK.
    ELSE.
      vf_new_charg                 = w_xi_mfbf-charg.
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
    ENDIF.

    vc_ano_doc_contabil         = sy-datum(4).
    vc_lgort_pai                = w_bflushdatagen-storageloc.

*    select single xchpf
*      into vl_xchpf
*      from mara
*     where matnr eq W_BFLUSHDATAGEN-MATERIALNR.
*
*      if vl_xchpf = 'X' .
*        W_BFLUSHDATAGEN-BATCH        = W_BFLUSHDATAGEN-STORAGELOC .
*      endif.
  ENDIF.

*------------------
  MESSAGE s024(sd) WITH '**** fim - Entrada Consumo 2'.
*------------------

***********
* Preenche Itens da BAPI MFBF
***********
  CHECK: w_bflushflags-bckfltype = cc_tipo_conf_11.


*------------------
  MESSAGE s024(sd) WITH '**** inicio - Entrada Consumo 3'.
*------------------

  LOOP AT t_xi_mfbf_log INTO w_xi_mfbf_log WHERE obj_key = w_xi_mfbf-obj_key.

    PERFORM yf_check_item_consumo USING w_xi_mfbf_log-matnr
                                        w_xi_mfbf_log-werks
                                        w_xi_mfbf_log-cd_safra
                                        w_xi_mfbf_log-qteprod
                               CHANGING yt_goodsmovements-material
                                        yt_goodsmovements-entry_uom.

    IF yt_goodsmovements-material IS INITIAL.
      pf_erro = cc_x.
      EXIT.
    ENDIF.

    yt_goodsmovements-plant     = w_xi_mfbf_log-werks.
    yt_goodsmovements-stge_loc  = w_xi_mfbf_log-cd_safra.
    yt_goodsmovements-move_type = cc_bwart_261.
    yt_goodsmovements-entry_qnt = w_xi_mfbf_log-qteprod.
    yt_goodsmovements-item_text = w_xi_mfbf_log-id_matgeo.

    SELECT SINGLE xchpf
      INTO vl_xchpf
      FROM mara
     WHERE matnr EQ yt_goodsmovements-material.

    IF vl_xchpf = 'X' .
      yt_goodsmovements-batch        = w_xi_mfbf_log-charg.
    ENDIF.
*   Anexa itens
    APPEND yt_goodsmovements.
    CLEAR yt_goodsmovements.
  ENDLOOP.

*------------------
  MESSAGE s024(sd) WITH '**** fim - Entrada Consumo 3'.
*------------------

ENDFORM.                    " YF_ITENS_ENTRADA_CONSUMO
*&---------------------------------------------------------------------*
*&      Form  YF_CHECK_ITEM_CONSUMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_XI_MFBF_MATNR  text
*      -->P_W_XI_MFBF_WERKS  text
*      -->P_W_XI_MFBF_CD_SAFRA  text
*      -->P_W_XI_MFBF_QTEPROD  text
*      <--P_YT_GOODSMOVEMENTS_MATERIAL  text
*      <--P_YT_GOODSMOVEMENTS_ENTRY_UOM  text
*----------------------------------------------------------------------*
FORM yf_check_item_consumo USING p_check_matnr
                                 p_check_werks
                                 p_check_lgort
                                 p_check_qtde
                        CHANGING p_matnr_out
                                 p_entry_uom.

  DATA: lc_mard TYPE mard.

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

  SELECT SINGLE * INTO lc_mard
    FROM mard
   WHERE matnr = p_matnr_out
     AND werks = p_check_werks
     AND lgort = p_check_lgort.

  IF lc_mard-labst < p_check_qtde.
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

  IF lc_mard-sperr IS NOT INITIAL.
    yt_execretrn-type    = cc_e.
    yt_execretrn-id      = cc_classe_mensagem.
    yt_execretrn-number  = '000'.
    CASE lc_mard-sperr.
      WHEN 'X'.
        yt_execretrn-message = TEXT-m14.
      WHEN 'A'.
        yt_execretrn-message = TEXT-m15.
    ENDCASE.
    yt_execretrn-message_v1 = p_check_matnr.
    yt_execretrn-message_v2 = p_check_werks.
    yt_execretrn-message_v3 = p_check_lgort.
    REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
             WITH p_check_matnr.
    REPLACE FIRST OCCURRENCE OF '&2' IN yt_execretrn-message
             WITH p_check_werks.
    REPLACE FIRST OCCURRENCE OF '&3' IN yt_execretrn-message
             WITH p_check_lgort.
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
*&      Form  YF_DISTRIBUI_LOG_XI
*&---------------------------------------------------------------------*
FORM yf_distribui_log_xi_lote  USING    p_tipoexec.

  DATA: l_idbol  TYPE ze_idbol,
        l_nrobol TYPE ze_nrobol.

  LOOP AT t_xi_mfbf_log           INTO w_xi_mfbf_log WHERE obj_key  EQ w_xi_mfbf_aux-obj_key
                                                       AND nrobol   EQ w_xi_mfbf_aux-nrobol
                                                       AND fgorigem EQ w_xi_mfbf_aux-fgorigem
                                                       AND werks    EQ w_xi_mfbf_aux-werks.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = w_xi_mfbf_log-matnr
      IMPORTING
        output = w_xi_mfbf_log-matnr.

    SELECT SINGLE matnr
             INTO @DATA(_matnr)
             FROM nsdm_v_mchb
            WHERE matnr = @w_xi_mfbf_log-matnr
              AND werks = @w_xi_mfbf_log-werks
              AND lgort = @w_xi_mfbf_log-cd_safra
              AND charg = @w_xi_mfbf_log-charg.

    CHECK sy-subrc <> 0.

    l_idbol  = w_xi_mfbf_log-obj_key.
    l_nrobol = w_xi_mfbf_log-nrobol.

    MOVE: '800'                     TO yt_geo_oubound_erro-nromsg,
*         w_xi_mfbf_log-obj_key     TO yt_geo_oubound_erro-idbol,
*         w_xi_mfbf_log-nrobol      TO yt_geo_oubound_erro-nrobol,
          l_idbol                   TO yt_geo_oubound_erro-idbol,
          l_nrobol                  TO yt_geo_oubound_erro-nrobol,
          w_xi_mfbf_log-fgorigem    TO yt_geo_oubound_erro-fgorigem,
          w_xi_mfbf_log-dtmvto      TO yt_geo_oubound_erro-erdat,
          sy-uzeit                  TO yt_geo_oubound_erro-erzet,
          sy-datum                  TO yt_geo_oubound_erro-dtproc,
          sy-uzeit                  TO yt_geo_oubound_erro-hrproc,
          w_xi_mfbf-fg_tpmovto      TO yt_geo_oubound_erro-bwart,
         " 'Lote/Material/Centro/Deposito não existe'  TO yt_geo_oubound_erro-message,

          w_xi_mfbf_log-matnr       TO yt_geo_oubound_erro-message_v1,
          w_xi_mfbf_log-werks       TO yt_geo_oubound_erro-message_v2,
          w_xi_mfbf_log-charg       TO yt_geo_oubound_erro-message_v3,
          w_xi_mfbf_log-cd_safra    TO yt_geo_oubound_erro-message_v4.

    yt_geo_oubound_erro-message =  | Lote: { w_xi_mfbf_log-charg }  Material: { w_xi_mfbf_log-matnr } Centro:{ w_xi_mfbf_log-werks } Deposito: { w_xi_mfbf_log-cd_safra } não existe | .

*    SHIFT yt_geo_oubound_erro-idbol  LEFT DELETING LEADING '0'.
*    SHIFT yt_geo_oubound_erro-nrobol LEFT DELETING LEADING '0'.

    APPEND yt_geo_oubound_erro.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  YF_DISTRIBUI_LOG_XI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CC_TPEXEC_02  text
*----------------------------------------------------------------------*
FORM yf_distribui_log_xi  USING    p_tipoexec.
  DATA: ln_mjahr TYPE mjahr,
        ld_budat TYPE budat,
        lc_bukrs TYPE bukrs,
        lc_awkey TYPE awkey,
        lc_belnr TYPE belnr_d.

  DATA: lc_shkzg TYPE shkzg,
        lc_buzid TYPE buzid,
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
      yt_log_mfpf-info_adicional_1 = 'Z_PP_MFBF-RETORNO LOG'.
      yt_log_mfpf-info_adicional_2 = w_xi_mfbf-id_cotton.
      APPEND yt_log_mfpf.
      MODIFY zret_sigam_algd FROM TABLE yt_log_mfpf.
      COMMIT WORK.

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
        IF w_xi_mfbf-zst_atlz = cc_c.
          SELECT belnr
            INTO lc_belnr
            FROM bkpf
              UP TO 1 ROWS
           WHERE bukrs = lc_bukrs
             AND gjahr = ln_mjahr
             AND bldat = ld_budat
             AND tcode = 'MFBF'
             AND awtyp = 'MKPF'
             AND awkey = lc_awkey.
          ENDSELECT.
        ELSE.
          SELECT belnr
            INTO lc_belnr
            FROM bkpf
              UP TO 1 ROWS
           WHERE bukrs = lc_bukrs
             AND gjahr = ln_mjahr
             AND bldat = ld_budat
             AND tcode = 'MB1A'
             AND awtyp = 'MKPF'
             AND awkey = lc_awkey.
          ENDSELECT.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT lc_belnr IS INITIAL.
      IF w_xi_mfbf-zst_atlz = cc_m .
        "if w_xi_mfbf-FG_TPMOVTO eq '261'.
        lc_shkzg = 'S'. "Movto 261 - Crédito
        lc_buzid = 'S'.
      ELSEIF w_xi_mfbf-fg_tpmovto EQ '261'.
        lc_shkzg = 'H'. "Movto 261 - Débito
        lc_buzid = 'M'.
      ELSE.
        lc_shkzg = 'S'. "Movto 261 - Débito
        lc_buzid = 'M'.
      ENDIF.



* ---> S4 Migration - 19/07/2023 - DG
      ""AJUSTE PARA VOLTAR A BUSCAR DA BSEG  -- COMENTADO DESENVOLVIMENTO MIGRAÇÃO --- LP - 26-10-2023

      SELECT dmbtr dmbe2 dmbe3 matnr
        INTO (lp_dmbtr, lp_dmbe2, lp_dmbe3, lp_matnr)
        FROM bseg
       WHERE bukrs = lc_bukrs
         AND belnr = lc_belnr
         AND gjahr = ln_mjahr
         AND buzid = lc_buzid
         AND shkzg = lc_shkzg.


*      DATA lv_rldnr TYPE rldnr.
*
*      DATA: lt_bseg_aux TYPE fagl_t_bseg.

*      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
*        IMPORTING
*          e_rldnr       = lv_RLDNR
*        EXCEPTIONS
*          not_found     = 1
*          more_than_one = 2.
*
*      IF sy-subrc = 0.
*
*        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
*          EXPORTING
*            i_rldnr   = lv_RLDNR
*            i_bukrs   = lc_bukrs
*            i_belnr   = lc_belnr
*            i_gjahr   = ln_mjahr
*          IMPORTING
*            et_bseg   = lt_bseg_aux
*          EXCEPTIONS
*            not_found = 1.
*      ENDIF.

*      CALL FUNCTION 'FAGL_GET_BSEG'
*        EXPORTING
*          i_bukrs = lc_bukrs
*          i_belnr = lc_belnr
*          i_gjahr = ln_mjahr
*        IMPORTING
*          et_bseg = lt_bseg_aux
*        EXCEPTIONS
*          OTHERS  = 2.


*      DELETE lt_bseg_aux WHERE  buzid NE lc_buzid AND shkzg NE lc_shkzg.
*
*      LOOP AT lt_bseg_aux INTO DATA(wa_bseg).
*
*        lp_dmbtr = wa_bseg-dmbtr.
*        lp_dmbe2 = wa_bseg-dmbe2.
*        lp_dmbe3 = wa_bseg-dmbe3.
        " lp_matnr = wa_bseg-matnr.



        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = lp_matnr
          IMPORTING
            output = lp_matnr.



* <--- S4 Migration - 19/07/2023 - DG

        yt_geo_oubound_ok-dmbtr = lp_dmbtr.
        yt_geo_oubound_ok-dmbe2 = lp_dmbe2.
        yt_geo_oubound_ok-dmbe3 = lp_dmbe3.
        yt_geo_oubound_ok-matnr = lp_matnr.
*        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*          EXPORTING
*            input  = lp_matnr
*          IMPORTING
*            output = yt_geo_oubound_ok-matnr.



        " Recuperando o ID_MATGEO para o retorno do sucesso..
        LOOP AT yt_goodsmovements.
          IF yt_goodsmovements-material = yt_geo_oubound_ok-matnr.

            yt_geo_oubound_ok-id_matgeo = yt_goodsmovements-item_text.
          ENDIF.
        ENDLOOP.


        " Alteração para permitir retorno de varios itens qd sucesso baixa de consumo por ordem de producao para o GEO
        IF yt_execretrn-type = cc_s AND ( w_xi_mfbf-zst_atlz = cc_c OR w_xi_mfbf-zst_atlz = cc_m ).
          MOVE: w_xi_mfbf-obj_key         TO yt_geo_oubound_ok-idbol,
                w_xi_mfbf-nrobol          TO yt_geo_oubound_ok-nrobol,
                w_xi_mfbf-fgorigem        TO yt_geo_oubound_ok-fgorigem,
                w_xi_mfbf-dtmvto          TO yt_geo_oubound_ok-erdat,
                sy-datum                  TO yt_geo_oubound_ok-dtproc,
                sy-uzeit                  TO yt_geo_oubound_ok-hrproc,
                w_xi_mfbf-fg_tpmovto      TO yt_geo_oubound_ok-bwart,
                lc_awkey                  TO yt_geo_oubound_ok-docmat.

          APPEND yt_geo_oubound_ok.
        ENDIF.

        CLEAR: lp_dmbtr, lp_dmbe2, lp_dmbe3.
        " ENDLOOP. "ENDSELECT. ---> S4 Migration - 19/07/2023 - DG
      ENDSELECT . "<< FIM "AJUSTE PARA VOLTAR A BUSCAR DA BSEG  -- COMENTADO DESENVOLVIMENTO MIGRAÇÃO --- LP - 26-10-2023
    ENDIF.

    LOOP AT yt_execretrn.
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
        ELSEIF  w_xi_mfbf-zst_atlz = cc_c OR w_xi_mfbf-zst_atlz = cc_m.
          MOVE w_xi_mfbf-fg_tpmovto     TO yt_geo_oubound_erro-bwart.
        ELSEIF  w_xi_mfbf-zst_atlz = cc_e.
          MOVE cc_bwart_262             TO yt_geo_oubound_erro-bwart.
        ENDIF.
        APPEND yt_geo_oubound_erro.
      ENDIF.
    ENDLOOP.

  ENDIF.
ENDFORM.                    " YF_DISTRIBUI_LOG_XI
*&---------------------------------------------------------------------*
*&      Form  YF_OBTEM_NRCONFIRMACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VF_ERRO  text
*----------------------------------------------------------------------*
FORM yf_obtem_nrconfirmacao  CHANGING pf_erro.
  DATA: lc_mblnr TYPE mblnr,
        ld_budat TYPE budat,
        ln_mjahr TYPE mjahr,
        lc_bukrs TYPE bukrs.

  CLEAR: pf_erro,
         vc_confirmation.

*------------------
  MESSAGE s024(sd) WITH '**** inicio - obtem confirmacao 1'.
*------------------

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

*------------------
  MESSAGE s024(sd) WITH '**** fim - obtem confirmacao 1'.
*------------------

ENDFORM.                    " YF_OBTEM_NRCONFIRMACAO
*&---------------------------------------------------------------------*
*&      Form  YF_OBTEM_DADOS_DOCTOMATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_XI_MFBF_MBLNR  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      <--P_LN_MJAHR  text
*      <--P_LC_BUKRS  text
*      <--P_LD_BUDAT  text
*----------------------------------------------------------------------*
FORM yf_obtem_dados_doctomaterial  USING pc_mblnr
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
*&      Form  YF_EXEC_BAPI_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*       text
*----------------------------------------------------------------------*
*      -->P_CC_TPEXEC_02  text
*----------------------------------------------------------------------*
FORM yf_gera_log_exec_bapi  USING    p_tipoexec.

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
*       text
*----------------------------------------------------------------------*
*      <--P_VC_ULT_DOC_GERADO  text
*      <--P_YT_EXECRETRN_MESSAGE_V3  text
*      <--P_VC_DT_LACTO_GERADO  text
*      <--P_VC_EMPR_DOC_GERADO  text
*----------------------------------------------------------------------*
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

FORM yf_validar_lote CHANGING p_erro.

  FREE p_erro.

  CHECK w_xi_mfbf_aux-nrobol IS NOT INITIAL.

  LOOP AT t_xi_mfbf_log INTO w_xi_mfbf_log WHERE obj_key  EQ w_xi_mfbf_aux-obj_key
                                             AND nrobol   EQ w_xi_mfbf_aux-nrobol
                                             AND fgorigem EQ w_xi_mfbf_aux-fgorigem
                                             AND werks    EQ w_xi_mfbf_aux-werks.


    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = w_xi_mfbf_log-matnr
      IMPORTING
        output = w_xi_mfbf_log-matnr.

    SELECT SINGLE xchpf
     INTO @DATA(lv_lote)
     FROM mara
    WHERE matnr EQ @w_xi_mfbf_log-matnr.

    IF lv_lote IS NOT INITIAL.

      SELECT SINGLE matnr
               INTO @DATA(_matnr)
               FROM nsdm_v_mchb
              WHERE matnr = @w_xi_mfbf_log-matnr
                AND werks = @w_xi_mfbf_log-werks
                AND lgort = @w_xi_mfbf_log-cd_safra
                AND charg = @w_xi_mfbf_log-charg.

      IF sy-subrc <> 0.
        p_erro = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  YF_EXEC_BAPI_ENTRADA_CONSUMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yf_exec_bapi_entrada_consumo .

  DATA: vl_teste_shdb TYPE c LENGTH 1.

  CLEAR: vl_teste_shdb.

  CHECK: NOT vc_aufnr_pai IS INITIAL.

  IF w_bflushflags-bckfltype = cc_tipo_conf_11.
    CHECK: NOT yt_goodsmovements[] IS INITIAL.
  ENDIF.

  CLEAR: sy-msgid, sy-msgty, sy-msgno, sy-msgv1,
         sy-msgv2, sy-msgv3, sy-msgv4.


  CLEAR: wa_zpps_goodsmv_log.

*------------------
  MESSAGE s024(sd) WITH '**** inicio - valida boletim'.
*------------------

  IF w_xi_mfbf_aux-zst_atlz = cc_c.

    PERFORM valida_boletim.

  ENDIF.

*------------------
  MESSAGE s024(sd) WITH '**** fim - valida boletim'.
*------------------

  IF vl_erro IS INITIAL.

*------------------
    MESSAGE s024(sd) WITH '**** inicio - valida lote'.
*------------------

    IF vl_teste_shdb IS NOT INITIAL."Apenas para teste
      PERFORM yf_processa_shdb.
    ENDIF.

*-#126585 - 31.10.2023 - JT - inicio
    PERFORM yf_validar_lote CHANGING l_erro_lote.

    IF l_erro_lote = abap_true.
      PERFORM yf_distribui_log_xi_lote USING cc_tpexec_01.
    ELSE.
      PERFORM yf_processa_bapi_consumo_geo.
*-#126585 - 31.10.2023 - JT - fim
    ENDIF.

*------------------
    MESSAGE s024(sd) WITH '**** fim - valida lote'.
*------------------

  ENDIF.

*------------------
  MESSAGE s024(sd) WITH '**** inicio - processar consumo 2'.
*------------------

  IF w_xi_mfbf_aux-zst_atlz = cc_i.

    PERFORM yf_distribui_log_xi USING cc_tpexec_01.

  ELSEIF w_xi_mfbf_aux-zst_atlz = cc_c.

    PERFORM yf_distribui_log_xi USING cc_tpexec_02.

  ENDIF.

*------------------
  MESSAGE s024(sd) WITH '**** fim - processar consumo 2'.
*------------------

* Limpa área de registros processados
  REFRESH: yt_goodsmovements,
           yt_return,
           yt_execretrn.

  CLEAR: yt_goodsmovements,
         w_bflushflags,
         w_bflushdatagen,
         vc_aufnr_pai.

ENDFORM.                    " YF_EXEC_BAPI_ENTRADA_CONSUMO
*&---------------------------------------------------------------------*
*&      Form  YF_RETORNA_LOG_INTERFACE_XI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yf_retorna_log_interface_xi .

*--> 25.08.2023 15:53:00 - Migração S4 – ML - Início
  DATA: lv_rfc TYPE rfcdest,
        lv_fm  TYPE rs38l_fnam.
*<-- 25.08.2023 15:53:00 - Migração S4 – ML – Fim

  IF NOT yt_geo_oubound_ok[] IS INITIAL.

*    CALL FUNCTION 'Z_MM_GRAVA_BOL_SUCESS'
*      TABLES
*        RETURN_SUCESS = YT_GEO_OUBOUND_OK.

*--> 25.08.2023 15:54:16 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_MM_OUTBOUND_BOL_SUCESS' IN BACKGROUND TASK
*      DESTINATION 'XI_GEO_RETURN_SUCESS'
*      TABLES
*        return_sucess = yt_geo_oubound_ok.

    " lv_fm = 'Z_MM_OUTBOUND_BOL_SUCESS'.

*** Stefanini - IR231105 - 27/08/2025 - LAZAROSR - Início de Alteração
    PERFORM enviar_bol_sucesso.

*    CONSTANTS: cc_fm_s TYPE rs38l_fnam VALUE 'Z_MM_OUTBOUND_BOL_SUCESS'.
*
*    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
*      EXPORTING
*        i_fm          = cc_fm_s
*      IMPORTING
*        e_rfc         = lv_rfc
*      EXCEPTIONS
*        no_rfc        = 1
*        no_rfc_config = 2
*        OTHERS        = 3.


*    IF sy-subrc EQ 0.
*      CALL FUNCTION cc_fm_s IN BACKGROUND TASK
*        DESTINATION lv_rfc
*        TABLES
*          return_sucess = yt_geo_oubound_ok.
*    ELSE.
*      CALL FUNCTION cc_fm_s IN BACKGROUND TASK
*        TABLES
*          return_sucess = yt_geo_oubound_ok.
*    ENDIF.

*** Stefanini - IR231105 - 27/08/2025 - LAZAROSR - Fim de Alteração

*<-- 25.08.2023 15:54:16 - Migração S4 – ML – Fim

    LOOP AT yt_geo_oubound_ok INTO DATA(wa_ok).
      READ TABLE yt_geo_oubound_erro WITH KEY idbol = wa_ok-idbol TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        DELETE yt_geo_oubound_erro WHERE idbol = wa_ok-idbol.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF NOT yt_geo_oubound_erro[] IS INITIAL.

*    CALL FUNCTION 'Z_MM_GRAVA_BOL_ERROR'
*      TABLES
*        RETURN_ERROR = YT_GEO_OUBOUND_ERRO.

*--> 25.08.2023 15:18:49 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_MM_OUTBOUND_BOL_ERROR' IN BACKGROUND TASK
*      DESTINATION 'XI_GEO_RETURN_ERROR'
*      TABLES
*        return_error = yt_geo_oubound_erro.

    " lv_fm = 'Z_MM_OUTBOUND_BOL_ERROR'.
    CONSTANTS: cc_fm_e TYPE rs38l_fnam VALUE 'Z_MM_OUTBOUND_BOL_ERROR'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = cc_fm_e
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION cc_fm_e IN BACKGROUND TASK
        DESTINATION lv_rfc
        TABLES
          return_error = yt_geo_oubound_erro.
    ELSE.
      CALL FUNCTION cc_fm_e IN BACKGROUND TASK
        TABLES
          return_error = yt_geo_oubound_erro.
    ENDIF.
*<-- 25.08.2023 15:18:49 - Migração S4 – ML – Fim
  ENDIF.

  IF NOT yt_log_mfpf[] IS INITIAL.

*    CALL FUNCTION 'Z_FI_GRAVA_RETURN'
*      TABLES
*        OUTRETURN = YT_LOG_MFPF.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = yt_log_mfpf.

    CONSTANTS: cc_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = cc_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION cc_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = yt_log_mfpf.
    ELSE.
      CALL FUNCTION cc_fm IN BACKGROUND TASK
        TABLES
          outreturn = yt_log_mfpf.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
  ENDIF.

  COMMIT WORK.

ENDFORM.                    " YF_RETORNA_LOG_INTERFACE_XI
*&---------------------------------------------------------------------*
*&      Form  YF_CHECK_DECIMAL_POINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_CHECK_QTDE  text
*      -->P_P_ENTRY_UOM  text
*      -->P_P_CHECK_MATNR  text
*      -->P_P_CHECK_LGORT  text
*      <--P_P_ENTRY_UOM  text
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

ENDFORM.                    " YF_CHECK_DECIMAL_POINT
*&---------------------------------------------------------------------*
*&      Form  YF_OBTEM_DADOS_ORDEMPROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CC_X  text
*----------------------------------------------------------------------*
FORM yf_obtem_dados_ordemprod  USING   VALUE(p_check_matr_werks).

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
*&      Form  YF_PROCESSA_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
        vl_count        TYPE i.
  DATA: wa_goodsmovements TYPE bapi2017_gm_item_create.
  "W_GOODSMOVEMENTS TYPE BAPI2017_GM_ITEM_CREATE,
  vl_tabix = sy-tabix.

  LOOP AT yt_goodsmovements INTO wa_goodsmovements.
    MOVE-CORRESPONDING wa_goodsmovements TO wa_zpps_goodsmv_log.
    wa_zpps_goodsmv_log-mandt        = sy-mandt.
    wa_zpps_goodsmv_log-data         = sy-datum.
    wa_zpps_goodsmv_log-hora         = sy-uzeit.
    wa_zpps_goodsmv_log-obj_key      = w_xi_mfbf-obj_key.
    wa_zpps_goodsmv_log-matnr        = wa_goodsmovements-material.
    wa_zpps_goodsmv_log-docheadertxt = w_bflushdatagen-docheadertxt.
    wa_zpps_goodsmv_log-backflquant  = w_bflushdatagen-backflquant.
    MODIFY zpps_goodsmv_log FROM wa_zpps_goodsmv_log.
  ENDLOOP.

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
  'X'   'SAPLCOWB'            '0130',
  ' '   'BDC_OKCODE'          '/00'.

  CLEAR: vl_cnt.

  vl_cnt = 1.

* loop dos materiais para baixa
  LOOP AT yt_goodsmovements INTO wa_goodsmovements.
    CONCATENATE 'COWB_COMP-MATNR(' vl_cnt ')'   INTO vl_field_mat.
    CONCATENATE 'COWB_COMP-ERFMG_R(' vl_cnt ')' INTO vl_field_quant.
    CONCATENATE 'COWB_COMP-WERKS(' vl_cnt ')'   INTO vl_field_centro.
    CONCATENATE 'COWB_COMP-LGORT(' vl_cnt ')'   INTO vl_field_dep.

    WRITE wa_goodsmovements-entry_qnt TO vl_qtde.

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

  CALL TRANSACTION cc_mfbf
     USING ti_bdc
     MODE   vl_mode
     UPDATE cc_s
     MESSAGES INTO ti_msg.

  WAIT UP TO 3 SECONDS.

  IF ti_msg[] IS NOT INITIAL.

    READ TABLE ti_msg INTO wa_msg  WITH KEY msgtyp = cc_s.

    IF sy-subrc EQ 0.

      SELECT COUNT(*)
        INTO vl_count
        FROM zpps_goodsmv_log
        WHERE obj_key = w_xi_mfbf-obj_key.

      IF vl_count > 0 .

        IF wa_msg-msgnr = '193' .

          vc_confirmation    = wa_msg-msgv1.
          vc_ult_doc_gerado  = wa_msg-msgv1.
          vc_dt_lacto_gerado = sy-datum.

          SELECT SINGLE bukrs
            INTO vc_empr_doc_gerado
            FROM mseg
           WHERE mblnr = vc_ult_doc_gerado
             AND mjahr = sy-datum(4).
*      Sucesso na execução da BAPI entrada/consumo

          yt_execretrn-type    = cc_s.
          yt_execretrn-id      = cc_classe_mensagem.
          yt_execretrn-number  = '000'.
          yt_execretrn-message_v1 = vc_confirmation.
          yt_execretrn-message_v2 = vc_ult_doc_gerado.
          APPEND yt_execretrn.

          UPDATE zpps_ximfbf_log SET mblnr = vc_ult_doc_gerado WHERE obj_key = w_xi_mfbf-obj_key.
        ELSE .
          CLEAR:   yt_execretrn.

          yt_execretrn-type       = cc_e.
          yt_execretrn-id         = cc_classe_mensagem.
          yt_execretrn-number     = '000'.
          yt_execretrn-message    = 'Erro.Verifique se o material esta bloqueado no SAP'.
          yt_execretrn-message_v1 = 'Erro.Verifique se o material esta bloqueado no SAP'.
          yt_execretrn-message_v2 = '60'.
          yt_execretrn-message_v3 = 'L'.

          APPEND yt_execretrn.
        ENDIF.
      ENDIF.
    ENDIF.
*Gravando log de erro de execução
    LOOP AT ti_msg INTO wa_msg WHERE msgtyp = cc_e.

      CLEAR:   yt_execretrn.
      MOVE-CORRESPONDING wa_msg TO yt_execretrn.
      MESSAGE ID   wa_msg-msgid
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

      APPEND yt_execretrn.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " YF_PROCESSA_SHDB
*&---------------------------------------------------------------------*
*&      Form  ZF_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2735   text
*      -->P_2736   text
*      -->P_2737   text
*----------------------------------------------------------------------*
FORM zf_bdc  USING  p_dynbegin TYPE any
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
*&---------------------------------------------------------------------*
*&      Form  YF_PROCESSA_BAPI_CONSUMO_GEO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yf_processa_bapi_consumo_geo .

  DATA : w_goodsmovements TYPE bapi2017_gm_item_create,
         gs_blpp          LIKE blpp,
         wdoc_material    LIKE zpps_ximfbf_log-mblnr,
         it_mseg          TYPE TABLE OF mseg WITH HEADER LINE,
         cancconfirmation LIKE bapi_rm_datkey-cancconfirmation.

  DATA: it_mkpf  TYPE TABLE OF  mkpf,
        it_mseg2 TYPE TABLE OF  mseg.

  DATA: qtd_mseg     TYPE i,
        qtd_produtos TYPE i.

  CLEAR   : w_bapiret2, wdoc_material, vl_erro, vc_confirmation, cancconfirmation, it_mseg, it_mseg[], qtd_mseg, qtd_produtos.
  REFRESH : yt_return.

*------------------
  MESSAGE s024(sd) WITH '**** inicio - bapi bloqueia material'.
*------------------

  CLEAR: w_goodsmovements .
  LOOP AT yt_goodsmovements INTO w_goodsmovements.
    MOVE-CORRESPONDING w_goodsmovements TO wa_zpps_goodsmv_log.
    wa_zpps_goodsmv_log-mandt        = sy-mandt.
    wa_zpps_goodsmv_log-data         = sy-datum.
    wa_zpps_goodsmv_log-hora         = sy-uzeit.
    wa_zpps_goodsmv_log-obj_key      = w_xi_mfbf-obj_key.
    wa_zpps_goodsmv_log-matnr        = w_goodsmovements-material.
    wa_zpps_goodsmv_log-docheadertxt = w_bflushdatagen-docheadertxt.
    wa_zpps_goodsmv_log-backflquant  = w_bflushdatagen-backflquant.
    MODIFY zpps_goodsmv_log FROM wa_zpps_goodsmv_log.

    "Bloquear os materiais, se algum material estiver bloqueado a bapi cria o documento de material sem o mesmo
    PERFORM yf_bloqueia_material USING w_goodsmovements-material w_goodsmovements-plant.

  ENDLOOP.

*------------------
  MESSAGE s024(sd) WITH '**** fim - bapi bloqueia material'.
*------------------

  IF vl_erro <> 'X' .

*------------------
    MESSAGE s024(sd) WITH '**** inicio - bapi consume geo 1'.
*------------------

    " setando parametro para não retornar pela interface de ordem de servico pelo metodo:IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE
    SET PARAMETER ID 'ZINBOUNDGEO' FIELD 'X'.
* ---> S4 Migration - 22/06/2023 - MA
    CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        bflushflags    = w_bflushflags
        bflushdatagen  = w_bflushdatagen
      IMPORTING
        confirmation   = vc_confirmation
        return         = yt_return
      TABLES
        goodsmovements = yt_goodsmovements.
* <--- S4 Migration - 22/06/2023 - MA
    SET PARAMETER ID 'ZINBOUNDGEO' FIELD space.

*------------------
    MESSAGE s024(sd) WITH '**** fim - bapi consume geo 1'.
*------------------

    IF yt_return-type = 'E'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      CLEAR: yt_execretrn.
      yt_execretrn-type       = cc_e.
      yt_execretrn-id         = yt_return-id."CC_CLASSE_MENSAGEM.
      yt_execretrn-number     = yt_return-number.
      yt_execretrn-message    = yt_return-message.
      yt_execretrn-message_v1 = yt_return-message_v1.
      yt_execretrn-message_v2 = yt_return-message_v2.
      yt_execretrn-message_v3 = yt_return-message_v3.
      APPEND yt_execretrn.
      CLEAR yt_return.

    ELSE.

      "Sucesso na execução da BAPI entrada/consumo
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

*------------------
      MESSAGE s024(sd) WITH '**** inicio - bapi consume geo 2'.
*------------------

*-CS2022000332-#84404-02.08.2022-JT-inicio
      DO 10 TIMES.
        SELECT SINGLE * INTO gs_blpp
          FROM blpp
         WHERE prtnr = vc_confirmation
           AND prtps = '0001'.
        IF sy-subrc <> 0.
          WAIT UP TO 2 SECONDS.
          CONTINUE.
        ENDIF.
        EXIT.
      ENDDO.
*-CS2022000332-#84404-02.08.2022-JT-fim
*------------------
      MESSAGE s024(sd) WITH '**** fim - bapi consume geo 2'.
*------------------

      vc_ult_doc_gerado  = gs_blpp-belnr.
      vf_new_mblnr       = gs_blpp-belnr.  "*-CS2022000332-#84404-02.08.2022-JT
      vc_dt_lacto_gerado = sy-datum.

*------------------
      MESSAGE s024(sd) WITH '**** inicio - bapi consume geo 3'.
*------------------

*-CS2022000332-#84404-02.08.2022-JT-inicio
      DO 10 TIMES.
        SELECT * INTO TABLE it_mseg
          FROM mseg
         WHERE mblnr = vc_ult_doc_gerado
           AND mjahr = sy-datum(4).
        IF sy-subrc <> 0.
          WAIT UP TO 2 SECONDS.
          CONTINUE.
        ENDIF.
        EXIT.
      ENDDO.
*-CS2022000332-#84404-02.08.2022-JT-fim

*------------------
      MESSAGE s024(sd) WITH '**** fim - bapi consume geo 3'.
*------------------

      REFRESH: it_mkpf, it_mseg2.

*------------------
      MESSAGE s024(sd) WITH '**** inicio - bapi consume geo 4'.
*------------------

*-CS2022000332-#84404-02.08.2022-JT-inicio
      DO 10 TIMES.
        SELECT *
          FROM mkpf
          INTO TABLE it_mkpf
         WHERE mblnr = vc_ult_doc_gerado
           AND mjahr = sy-datum(4).
        IF sy-subrc <> 0.
          WAIT UP TO 2 SECONDS.
          CONTINUE.
        ENDIF.
        EXIT.
      ENDDO.
*-CS2022000332-#84404-02.08.2022-JT-fim

*------------------
      MESSAGE s024(sd) WITH '**** fim - bapi consume geo 4'.
*------------------

      READ TABLE it_mkpf INTO DATA(wa_mkpf) INDEX 1.
      IF wa_mkpf-bktxt+0(4) = 'RFID'.
        wa_mkpf-xblnr = wa_mkpf-bktxt+4(16).
        MODIFY it_mkpf FROM wa_mkpf INDEX 1 TRANSPORTING xblnr.

        CALL FUNCTION 'MB_CHANGE_DOCUMENT' "
          TABLES
            zmkpf = it_mkpf
            zmseg = it_mseg2.
      ENDIF.
      READ TABLE it_mseg INDEX 1.
      vc_empr_doc_gerado = it_mseg-bukrs.

      wdoc_material = vc_ult_doc_gerado.

      "Verificar se Materiais informados foram gerados
      DESCRIBE TABLE it_mseg           LINES qtd_mseg.
      DESCRIBE TABLE yt_goodsmovements LINES qtd_produtos.

      IF qtd_mseg NE qtd_produtos AND w_xi_mfbf_aux-zst_atlz NE cc_i.
        CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
          EXPORTING
            confirmation     = vc_confirmation
          IMPORTING
            cancconfirmation = cancconfirmation.

        IF cancconfirmation IS NOT INITIAL.
          CLEAR: wdoc_material.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.
      ENDIF.

      IF cancconfirmation IS NOT INITIAL.
        CLEAR: yt_execretrn.
        yt_execretrn-type       = cc_e.
        yt_execretrn-id         = 'Z00'."CC_CLASSE_MENSAGEM.
        yt_execretrn-number     = '999'.
        yt_execretrn-message    = 'Documento foi gerado e estornado por erro de quantidade de item(s)!'.
        yt_execretrn-message_v1 = it_mseg-mblnr.
        yt_execretrn-message_v2 = it_mseg-mjahr.
        yt_execretrn-message_v3 = vc_confirmation.
        yt_execretrn-message_v4 = cancconfirmation.
        APPEND yt_execretrn.
        CLEAR yt_return.
      ELSE.
        "Gera LOG de Processamento
        IF w_xi_mfbf-zst_atlz = cc_i.
          PERFORM yf_gera_log_exec_bapi USING cc_tpexec_01.
        ELSEIF w_xi_mfbf-zst_atlz = cc_c.
          yt_execretrn-type    = cc_s.
          yt_execretrn-id      = cc_classe_mensagem.
          yt_execretrn-number  = '000'.
          yt_execretrn-message_v1 = vc_ult_doc_gerado.
          yt_execretrn-message_v2 = vc_ult_doc_gerado.
          APPEND yt_execretrn.
        ENDIF.
      ENDIF.
    ENDIF.

    IF w_xi_mfbf-fgorigem EQ abap_false.
      UPDATE zpps_ximfbf_log
       SET mblnr            = wdoc_material
           confirmation     = vc_confirmation
           cancconfirmation = cancconfirmation
     WHERE obj_key = w_xi_mfbf-obj_key
       AND charg   = vf_new_charg.  "*-CS2022000332-#84404-02.08.2022-JT-inicio
    ELSE.
      UPDATE zpps_ximfbf_log
         SET mblnr            = wdoc_material
             confirmation     = vc_confirmation
             cancconfirmation = cancconfirmation
       WHERE obj_key = w_xi_mfbf-obj_key.
    ENDIF.

  ENDIF.

*------------------
  MESSAGE s024(sd) WITH '**** inicio - bapi desbloq material'.
*------------------

  "Desbloquear os materiais
  LOOP AT yt_goodsmovements INTO w_goodsmovements.
    PERFORM yf_desbloqueia_material USING w_goodsmovements-material w_goodsmovements-plant.
  ENDLOOP.

*------------------
  MESSAGE s024(sd) WITH '**** fim - bapi desbloq material'.
*------------------

ENDFORM.                    " YF_PROCESSA_BAPI_CONSUMO_GEO
*&---------------------------------------------------------------------*
*&      Form  VALIDA_BOLETIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_boletim .
  DATA: it_mkpf TYPE TABLE OF mkpf,
        it_mseg TYPE TABLE OF mseg.

  DATA: wa_mkpf     TYPE mkpf,
        wa_mseg     TYPE mseg,
        wa_zfit0033 TYPE zfit0033.

  DATA: vl_message  TYPE bapi_msg,
        vl_data_val TYPE datum,
        i_data      TYPE datum,
        i_bukrs     TYPE bukrs,
        vl_monat    TYPE monat,
        vl_gjahr    TYPE gjahr.

  CLEAR vl_ajustado.

  "--Verificação Lote
  "  LOOP AT T_XI_MFBF_LOG INTO W_XI_MFBF_LOG WHERE OBJ_KEY = W_XI_MFBF-OBJ_KEY.

  "    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  "      EXPORTING
  "        INPUT  = W_XI_MFBF_LOG-MATNR
  "      IMPORTING
  "        OUTPUT = VL_MATNR.

  "    SELECT SINGLE XCHAR
  "      INTO VL_XCHAR
  "      FROM MARC
  "     WHERE MATNR = VL_MATNR
  "       AND WERKS = W_XI_MFBF-WERKS.

  "    IF VL_XCHAR = 'X'.
  "      VL_MESSAGE = 'Entrar Lote'.
  "      VL_ERRO = 'X'.

  "    ENDIF.

  "  ENDLOOP.
  "--Fim Verificação

  "--Verificação se o boletim ja foi baixado
  IF vl_erro <> 'X'.

    vl_quant = 0.

    vl_nrbol_aux  = w_xi_mfbf-nrobol.

    SHIFT vl_nrbol_aux LEFT DELETING LEADING '0'.

    CONCATENATE vl_nrbol_aux ' - ' w_xi_mfbf-fgorigem INTO vl_nrbol RESPECTING BLANKS.

    vl_mjahr = w_xi_mfbf-dtmvto(4).

    SELECT *
      INTO TABLE it_mkpf
      FROM mkpf
     WHERE bktxt = vl_nrbol
       AND mjahr = vl_mjahr.

    IF sy-subrc IS INITIAL.

      SELECT *
        INTO TABLE it_mseg
        FROM mseg
         FOR ALL ENTRIES IN it_mkpf
       WHERE mblnr = it_mkpf-mblnr
         AND werks = w_xi_mfbf-werks.

*---> 06/07/2023 - Migração S4 - WS
      SORT it_mseg.
*<--- 06/07/2023 - Migração S4 - WS
      DELETE ADJACENT DUPLICATES FROM it_mseg COMPARING ALL FIELDS.

      LOOP AT it_mseg INTO wa_mseg.
        IF wa_mseg-shkzg = 'S'.
          vl_quant = vl_quant - wa_mseg-menge.
        ELSE.
          vl_quant = vl_quant + wa_mseg-menge.
        ENDIF.
      ENDLOOP.

    ENDIF.

*    DATA(VL_TOT) =
*    REDUCE ERFMG(
*                   INIT I TYPE ERFMG
*                     FOR LS IN T_XI_MFBF_LOG
*                     WHERE ( OBJ_KEY EQ W_XI_MFBF-OBJ_KEY )
*                     NEXT I = I + LS-QTEPROD
*                ).
*
*    IF VL_TOT NE VL_QUANT.
*      VL_MESSAGE = 'Quantidade divergente do boletim apontado. Verificar!'.
*      VL_ERRO = 'X'.
*    ELSEIF VL_QUANT > 0 .
*      VL_MESSAGE = 'Boletim ja foi baixado'.
*      VL_ERRO = 'X'.
*    ENDIF.

    IF vl_quant > 0 .
      vl_message = 'Boletim ja foi baixado'.
      READ TABLE it_mkpf INTO DATA(wa_mkpf2) INDEX 1.
      PERFORM ajustar_log USING wa_mkpf2 w_xi_mfbf CHANGING vl_ajustado.
      vl_erro = 'X'.
    ENDIF.

  ENDIF.
  "--Fim Verificação se o boletim ja foi baixado

  "--Verica se o Mês esta fechado
  IF vl_erro <> 'X' .
    "I_BUKRS
    "W_XI_MFBF-WERKS.
    SELECT SINGLE bukrs
      FROM t001k
      INTO i_bukrs
     WHERE bwkey = w_xi_mfbf-werks.



    vl_data_val = w_xi_mfbf-dtmvto.

    "I_DATA      = '00000000'.
    i_data      = w_xi_mfbf-dtmvto.


    PERFORM valida_mes_mm USING i_data
                                i_bukrs
                       CHANGING vl_data_val
                                vl_message .
*    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
*      EXPORTING
*        P_DATA_ENT     = I_DATA
*        P_BUKRS        = I_BUKRS
*        P_VAL_FI       = ''
*        P_VAL_MM       = 'X'
*      IMPORTING
*        P_DATA_VAL     = VL_DATA_VAL
*      EXCEPTIONS
*        DATA_FI_MM_NAO = 1
*        OTHERS         = 2.
    IF vl_message IS NOT INITIAL .
      vl_erro = 'X'.
    ELSE.
      IF i_data+0(6)   LE vl_data_val+0(6) AND  "periodo está aberto
        sy-datum+0(6) NE vl_data_val+0(6).     "mas não é o do mês corrente
        vl_monat = i_data+4(2).
        vl_gjahr = i_data+0(4).

        SELECT SINGLE *
            FROM zfit0033
            INTO wa_zfit0033
            WHERE monat      = vl_monat
            AND   gjahr      = vl_gjahr
            AND   usnam      = sy-uname.
*        ENDIF.

        IF sy-subrc  NE 0.
          vl_erro = 'X'.
          vl_message  = 'Usuário sem permissão para lançar em mês retroativo'.
        ELSEIF wa_zfit0033-lib_contab IS INITIAL.
          vl_erro = 'X'.
          vl_message  = 'Usuário bloqueado para lançamento'.
        ELSEIF    sy-datum GT wa_zfit0033-data_lim.
          vl_erro = 'X'.
          vl_message  = 'Data limite ultrapassada para lançamento'.
        ELSEIF ( sy-datum EQ wa_zfit0033-data_lim AND sy-uzeit GT wa_zfit0033-hora_lim ).
          vl_erro = 'X'.
          vl_message  = 'Hora limite ultrapassada para lançamento'.
        ENDIF.


      ELSEIF i_data+0(6)  NE vl_data_val+0(6).

        vl_erro = 'X'.
        vl_message  = 'Periodo fechado para movimentação'.

      ENDIF.
    ENDIF.
  ENDIF.
  "--Fim Verica se o Mês esta fechado


  "--Envia Log Interface
  IF vl_erro = 'X' AND vl_ajustado = abap_false.
    CLEAR:   yt_execretrn.
    yt_execretrn-type       = cc_e.
    yt_execretrn-id         = cc_classe_mensagem.
    yt_execretrn-number     = '000'.
    yt_execretrn-message    = vl_message.
    yt_execretrn-message_v1 = vl_message.
    yt_execretrn-message_v2 = '60'.
    yt_execretrn-message_v3 = 'L'.
    APPEND yt_execretrn.
  ENDIF.

ENDFORM.                    " VALIDA_BOLETIM
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_CONSUMO_COMBUSTIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_consumo_combustivel .

  DATA: BEGIN OF wa_hkont,
          matnr LIKE mara-matnr,
          matkl LIKE mara-matkl,
          saknr LIKE zmmt0002-saknr,
        END   OF wa_hkont.

  DATA: vg_goodheader LIKE bapi2017_gm_head_01,
        vg_code       LIKE bapi2017_gm_code,
        vg_headret    LIKE bapi2017_gm_head_ret,
        wa_gooditem   LIKE bapi2017_gm_item_create,
        it_gooditem   LIKE STANDARD TABLE OF bapi2017_gm_item_create.

  DATA: it_hkont           LIKE HASHED   TABLE OF wa_hkont
                           WITH UNIQUE KEY matnr.

  DATA: vg_tabix      LIKE sy-tabix,
        vl_nrobol     TYPE c LENGTH 20,
        wdoc_material LIKE zpps_ximfbf_log-mblnr.


  CLEAR    : vg_goodheader, vg_code, vg_headret, vl_erro.
  REFRESH  : it_gooditem.

  PERFORM valida_boletim.

  IF vl_erro NE 'X'.

*------------------
    MESSAGE s024(sd) WITH '**** inicio - processar combustivel 1'.
*------------------

*    LOOP AT t_xi_mfbf_log INTO w_xi_mfbf_log WHERE obj_key = w_xi_mfbf_aux-obj_key.
*
*      vg_tabix = sy-tabix.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = w_xi_mfbf_log-matnr
*        IMPORTING
*          output = w_xi_mfbf_log-matnr.
*
*      IF sy-subrc = 0.
*        MODIFY t_xi_mfbf_log FROM w_xi_mfbf_log INDEX vg_tabix.
*      ENDIF.
*
*    ENDLOOP.

    SELECT DISTINCT t1~matnr t2~matkl t2~saknr
      FROM mara AS t1
     INNER JOIN zmmt0002 AS t2
        ON t1~matkl EQ t2~matkl
      INTO TABLE it_hkont
       FOR ALL ENTRIES IN t_xi_mfbf_log
     WHERE ( t1~matnr EQ t_xi_mfbf_log-matnr ).

    IF ( w_xi_mfbf_aux-dt_lancamento IS INITIAL ).
      vg_goodheader-pstng_date  = sy-datum.
    ELSE.
      vg_goodheader-pstng_date  = w_xi_mfbf_aux-dt_lancamento.
    ENDIF.

    IF ( w_xi_mfbf_aux-dtmvto IS INITIAL ).
      vg_goodheader-doc_date = sy-datum.
    ELSE.
      vg_goodheader-doc_date = w_xi_mfbf_aux-dtmvto.
    ENDIF.

    vg_goodheader-pr_uname        = sy-uname.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = w_xi_mfbf_aux-nrobol
      IMPORTING
        output = vl_nrobol.

    CONCATENATE vl_nrobol '-' w_xi_mfbf_aux-fgorigem INTO vg_goodheader-header_txt SEPARATED BY space.

    vg_goodheader-ver_gr_gi_slipx = 'X'.
    vg_code-gm_code               = '03'.

    "perform valida_boletim.
    LOOP AT t_xi_mfbf_log INTO w_xi_mfbf_log WHERE obj_key = w_xi_mfbf_aux-obj_key.

*---> 19/06/2023 - Migração S4 - DG
*      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*        EXPORTING
*          input        = w_xi_mfbf_log-matnr
*        IMPORTING
*          output       = wa_gooditem-material
*        EXCEPTIONS
*          length_error = 1
*          OTHERS       = 2.

      DATA(v_len) = strlen( w_xi_mfbf_log-matnr ).

*      IF v_len > 18.
*        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*          EXPORTING
*            input        = w_xi_mfbf_log-matnr
*          IMPORTING
*            output       = wa_gooditem-material_long
*          EXCEPTIONS
*            length_error = 1
*            OTHERS       = 2.
*      ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = w_xi_mfbf_log-matnr
        IMPORTING
          output       = wa_gooditem-material
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
*      ENDIF.


*<--- 19/06/2023 - Migração S4 - DG

      "--> Pesquisa cta do razão
      READ TABLE it_hkont INTO wa_hkont WITH KEY matnr = wa_gooditem-material.

      IF ( sy-subrc EQ 0 ).
        wa_gooditem-gl_account = wa_hkont-saknr.
      ENDIF.

      wa_gooditem-plant      = w_xi_mfbf_aux-werks.
      wa_gooditem-stge_loc   = w_xi_mfbf_aux-cd_safra.
      wa_gooditem-tr_part_ba = w_xi_mfbf_aux-werks.
      wa_gooditem-move_type  = w_xi_mfbf_aux-fg_tpmovto.
      wa_gooditem-entry_qnt  = w_xi_mfbf_log-qteprod.
      wa_gooditem-batch      = w_xi_mfbf_log-charg.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_xi_mfbf_log-cd_ccusto
        IMPORTING
          output = wa_gooditem-costcenter.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_xi_mfbf_log-aufnr
        IMPORTING
          output = wa_gooditem-orderid.

**---------------- PP-IR071827-INTERFACE_GEO_Alterar_BATCH-BG -- INICIO--------
** Verificar se o material é controlado por lote, caso for positivo setar o lote.
*      SELECT SINGLE XCHPF
*        INTO @DATA(VL_XCHPF)
*        FROM MARA
*       WHERE MATNR EQ @W_XI_MFBF_LOG-MATNR.
*
*      IF SY-SUBRC IS INITIAL AND VL_XCHPF = 'X' AND WA_GOODITEM-BATCH IS INITIAL.
*        WA_GOODITEM-BATCH = W_XI_MFBF_LOG-CD_SAFRA.
*      ENDIF.
**---------------- PP-IR071827-INTERFACE_GEO_Alterar_BATCH-BG -- FIM --------
      APPEND wa_gooditem TO it_gooditem.

    ENDLOOP.

*------------------
    MESSAGE s024(sd) WITH '**** fim - processar combustivel 1'.
*------------------

    SET PARAMETER ID 'ZINBOUNDGEO' FIELD 'X'.

*------------------
    MESSAGE s024(sd) WITH '**** inicio - processar combustivel 2'.
*------------------

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = vg_goodheader
        goodsmvt_code    = vg_code
*       testrun          = 'X'
      IMPORTING
        goodsmvt_headret = vg_headret
      TABLES
        goodsmvt_item    = it_gooditem
        return           = yt_return.

    IF yt_return-type = 'E'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      CLEAR:   yt_execretrn.

      yt_execretrn-type       = cc_e.
      yt_execretrn-id         = yt_return-id."CC_CLASSE_MENSAGEM.
      yt_execretrn-number     = yt_return-number.
      yt_execretrn-message    = yt_return-message.
      yt_execretrn-message_v1 = yt_return-message_v1.
      yt_execretrn-message_v2 = yt_return-message_v2.
      yt_execretrn-message_v3 = yt_return-message_v3.

      APPEND yt_execretrn.

      CLEAR yt_return.

    ELSE.
      "Sucesso na execução da BAPI entrada/consumo
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      wdoc_material = vg_headret.
      vc_ult_doc_gerado  = wdoc_material.
      vc_dt_lacto_gerado = w_xi_mfbf_aux-dtmvto.

      SELECT SINGLE bukrs
        INTO vc_empr_doc_gerado
        FROM mseg
       WHERE mblnr = vc_ult_doc_gerado
         AND mjahr = sy-datum(4).

      SET PARAMETER ID 'ZINBOUNDGEO' FIELD space.

      "Gera LOG de Processamento
      yt_execretrn-type       = cc_s.
      yt_execretrn-id         = cc_classe_mensagem.
      yt_execretrn-number     = '000'.
      yt_execretrn-message_v1 = vg_headret.
      yt_execretrn-message_v2 = vg_headret.
      APPEND yt_execretrn.

      UPDATE zpps_ximfbf_log SET mblnr = wdoc_material WHERE obj_key = w_xi_mfbf_aux-obj_key.

    ENDIF.

*------------------
    MESSAGE s024(sd) WITH '**** fim - processar combustivel 2'.
*------------------

  ENDIF.

*------------------
  MESSAGE s024(sd) WITH '**** inicio - processar combustivel 3'.
*------------------

  PERFORM yf_distribui_log_xi USING cc_tpexec_02.

*------------------
  MESSAGE s024(sd) WITH '**** fim - processar combustivel 3'.
*------------------

ENDFORM.                    " PROCESSA_CONSUMO_COMBUSTIVEL
*&---------------------------------------------------------------------*
*&      Form  YF_ITENS_ENTRADA_COMBUSTIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VF_ERRO  text
*----------------------------------------------------------------------*
FORM yf_itens_entrada_combustivel  CHANGING pf_erro.

  DATA: vl_xchpf  TYPE mara-xchpf.

  CLEAR: w_bflushflags,
         w_bflushdatagen.

*------------------
  MESSAGE s024(sd) WITH '**** inicio - Entrada combustivel 1'.
*------------------

  LOOP AT t_xi_mfbf_log INTO w_xi_mfbf_log WHERE obj_key = w_xi_mfbf-obj_key.

    CLEAR: yt_goodsmovements.

    PERFORM yf_check_item_consumo USING w_xi_mfbf_log-matnr
                                        w_xi_mfbf_log-werks
                                        w_xi_mfbf_log-cd_safra
                                        w_xi_mfbf_log-qteprod
                               CHANGING yt_goodsmovements-material
                                        yt_goodsmovements-entry_uom.

    IF yt_goodsmovements-material IS INITIAL.
      pf_erro = cc_x.
      EXIT.
    ENDIF.

    yt_goodsmovements-plant     = w_xi_mfbf_log-werks.
    yt_goodsmovements-stge_loc  = w_xi_mfbf_log-cd_safra.
    yt_goodsmovements-move_type = w_xi_mfbf_log-fg_tpmovto.
    yt_goodsmovements-entry_qnt = w_xi_mfbf_log-qteprod.
    yt_goodsmovements-item_text = w_xi_mfbf_log-id_matgeo.

** Verificar se o material é controlado por lote, caso for positivo setar o lote.
    SELECT SINGLE xchpf
      INTO vl_xchpf
      FROM mara
     WHERE matnr EQ yt_goodsmovements-material.

    IF sy-subrc IS INITIAL AND vl_xchpf = 'X' AND yt_goodsmovements-batch IS INITIAL.
      yt_goodsmovements-batch = w_xi_mfbf_log-cd_safra.
    ENDIF.

*   Anexa itens
    APPEND yt_goodsmovements.
  ENDLOOP.

*------------------
  MESSAGE s024(sd) WITH '**** fim - Entrada combustivel 1'.
*------------------

ENDFORM.                    " YF_ITENS_ENTRADA_COMBUSTIVEL
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_erros CHANGING p_erro.
  "Caso tenha dado algum dump ou erro no processamento anterior e não tenha atualizado o campo processado.
  DATA : t_log    TYPE TABLE OF zpps_ximfbf_log,
         t_prod   TYPE TABLE OF zpps_ximfbf_log,
         l_data1  TYPE sy-datum,
         l_data2  TYPE sy-datum,
         l_quant  TYPE i,
         t_status TYPE zde_btcstatus_t.

*-CS2022000332-#84404-02.08.2022-JT-inicio
  FREE: p_erro.
*------------------------------------------
* bloqueia registros para processamento
*------------------------------------------
  p_erro = zcl_integracao_cotton_sap=>lock_objeto( 'BOLETIM_CONSUMO_GEO' ).

  CHECK p_erro = abap_false.
*-CS2022000332-#84404-02.08.2022-JT-fim

*-CS2022000332-#84404-02.08.2022-JT-inicio
  l_data1 = sy-datum - 5.
  l_data2 = sy-datum - 1.

*---------------------------------
* verifica se houve JOB cancelado
*-trata reprocessamento entrada producao
*---------------------------------
  FREE: l_quant, t_status, t_prod.

  APPEND 'A' TO t_status.

  TRY.
      zcl_job=>get_job_programa_execucao(
        EXPORTING
          i_progname     = sy-cprog
          i_sdldate_init = l_data1
          i_status       = t_status
        IMPORTING
          e_quantidade   = l_quant ).
    CATCH zcx_job.
  ENDTRY.

  IF l_quant >= 1.
    SELECT *
      FROM zpps_ximfbf_log
      INTO TABLE t_prod
     WHERE processado    = cc_p
       AND zrg_atulizado = cc_s
       AND zst_atlz      = cc_i  "*-CS2022000332-#84404-02.08.2022-JT-fim
       AND id_interface NE 'S' "Projeto Reestruturação Algodao 2024
       AND data         >= l_data1
       AND mblnr         = abap_off. "space.
  ENDIF.

*---------------------------------
* verifica algum registro processado sem
* documento gerado. Se houver volta ao
* status de processamento
*---------------------------------
  SELECT *
    FROM zpps_ximfbf_log
    APPENDING TABLE t_prod
   WHERE processado    = cc_s
     AND zrg_atulizado = cc_s
     AND zst_atlz      = cc_i  "*-CS2022000332-#84404-02.08.2022-JT-fim
     AND id_interface NE 'S' "Projeto Reestruturação Algodao 2024
     AND ( data       >= l_data1
     AND   data       <= sy-datum )
     AND mblnr         = abap_off. "space.

  LOOP AT t_prod           INTO DATA(w_prod).
    w_prod-processado         = cc_n.
    w_prod-zrg_atulizado      = cc_n.
    MODIFY zpps_ximfbf_log FROM w_prod.
  ENDLOOP.

  COMMIT WORK.
*-CS2022000332-#84404-02.08.2022-JT-fim

  SELECT *
    FROM zpps_ximfbf_log
    INTO TABLE t_log
   WHERE processado    EQ 'P'
     AND zrg_atulizado EQ 'S'
     AND id_interface  NE 'S' "Projeto Reestruturação Algodao 2024
     AND zst_atlz      <> cc_i  "*-CS2022000332-#84404-02.08.2022-JT-fim
     AND mblnr         EQ abap_off. "space.

*---> 06/07/2023 - Migração S4 - WS
  SORT t_log BY obj_key.
*<--- 06/07/2023 - Migração S4 - WS
  DELETE ADJACENT DUPLICATES FROM t_log COMPARING obj_key.

  LOOP AT t_log INTO w_xi_mfbf.

    REFRESH yt_execretrn.

    yt_execretrn-type       = cc_e.
    yt_execretrn-id         = cc_classe_mensagem.
    yt_execretrn-number     = '000'.
    yt_execretrn-message_v2 = '60'.
    yt_execretrn-message_v3 = 'L'.

    IF w_xi_mfbf-zst_atlz = cc_i.
      yt_execretrn-message    = 'Ocorreu um erro inesperado no processamento desta entrada de produção, favor reenvia-lo'.
      yt_execretrn-message_v1 = 'Ocorreu um erro inesperado no processamento desta entrada de produção, favor reenvia-lo'..
    ELSE.
      yt_execretrn-message    = 'Ocorreu um erro inesperado na integração deste boletim, favor reenvia-lo'.
      yt_execretrn-message_v1 = 'Ocorreu um erro inesperado na integração deste boletim, favor reenvia-lo'.
    ENDIF.

    APPEND yt_execretrn.

    IF w_xi_mfbf-zst_atlz = cc_i.
      PERFORM yf_distribui_log_xi USING cc_tpexec_01.
    ELSE.
      PERFORM yf_distribui_log_xi USING cc_tpexec_02.
    ENDIF.

    CLEAR w_xi_mfbf.
  ENDLOOP.

  UPDATE zpps_ximfbf_log
     SET processado     = 'S'
   WHERE processado    EQ 'P'
     AND zrg_atulizado EQ 'S'
     AND zst_atlz      <> cc_i    "*-CS2022000332-#84404-02.08.2022-JT-fim
     AND id_interface  NE 'S' "Projeto Reestruturação Algodao 2024
     AND mblnr         EQ abap_off. "space.

  COMMIT WORK.

  PERFORM yf_retorna_log_interface_xi.


ENDFORM.                    " PROCESSA_ERROS

*form valida_saldo_produto using p_sdate
*                                p_matnr
*                                p_werks
*                                p_lgort .
*
*  DATA: "tlx   LIKE t_l OCCURS 0 WITH HEADER LINE,
*        t_lst LIKE abaplist OCCURS 0 WITH HEADER LINE,
*
*  v_stock      TYPE menge_d,
*  v_stockx     TYPE c LENGTH 22.
*
*  "RETURN STRUCTURE  BAPIRET2
*
*  SUBMIT rm07mlbd WITH datum     EQ p_sdate
*                  WITH matnr    EQ p_matnr
*                  WITH werks    EQ p_werks
*                  WITH lgort    EQ p_lgort
*                  WITH xsum     EQ ' '
*                  WITH lgbst    EQ 'X'
*                  WITH bwbst    EQ ' '
*                  WITH pa_wdzer EQ 'X'
*                  WITH pa_wdzew EQ 'X'
*                  WITH pa_wdwiz EQ 'X'
*                  WITH pa_wdwuw EQ 'X'
*                  WITH pa_wdwew EQ 'X'
*                  WITH pa_ndzer EQ 'X'
*                  WITH pa_ndsto EQ 'X'
*  EXPORTING LIST TO MEMORY AND RETURN.
*
*  CALL FUNCTION 'LIST_FROM_MEMORY'
*    TABLES
*      listobject = t_lst
*    EXCEPTIONS
*      not_found  = 1
*      OTHERS     = 2.
*
*  IF sy-subrc EQ 0.
*
*    CALL FUNCTION 'LIST_TO_ASCI'
*      TABLES
*        listasci           = tlx
*        listobject         = t_lst
*      EXCEPTIONS
*        empty_list         = 1
*        list_index_invalid = 2
*        OTHERS             = 3.
*
**  Pega linha 8 (saldo e UMB)
*    LOOP AT tlx FROM 8 TO 8.
*      SHIFT tlx-like+23(25) LEFT DELETING LEADING ' '.
*      SHIFT tlx-like+48(5) LEFT DELETING LEADING ' '.
*      MOVE  tlx-like+23(25) TO v_stockx.
*      TRANSLATE v_stockx USING '. '.
*      CONDENSE  v_stockx NO-GAPS.
*      TRANSLATE v_stockx USING ',.'.
**    Unidade de medida
*      MOVE tlx-like+48(5) TO v_meins.
**     Converte unidade de medida de M3 para L
*      IF v_meins = 'M3'.
*        v_stockx = v_stockx * 1000.
*      ENDIF.
**    Saldo na data de lançamento
*      MOVE v_stockx TO v_stock.
*    ENDLOOP.
*
**    Eliminar dados da memória
*    CALL FUNCTION 'LIST_FREE_MEMORY'
*      TABLES
*        listobject = t_lst.
**    Compara quantidades a baixar ou a vender com saldo na data de lançamento
*    IF ( item_migo-quantidade > v_stock ).
*
*      CONCATENATE 'Material' item_migo-material 'Centro' cabec_migo-centro_orig 'Depósito'
*                  p_lgort 'Em' cabec_migo-dt_base 'Baixar' item_migo-quantidade
*                  'Saldo' v_stockx 'UMB' v_meins INTO msg_erro SEPARATED BY space.
*      MOVE: 'E'       TO return-type,
*            msg_erro  TO return-message.
*      APPEND return. CLEAR: return, msg_erro.
*      v_erro = c_x.
*      EXIT.
*    ENDIF.
*
*  ELSE.
*
*      CONCATENATE 'Verificar disponibilidade do material' item_migo-material 'na data'
*                  cabec_migo-dt_base 'transação MB5B' INTO msg_erro SEPARATED BY space.
*
*      MOVE: 'E'      TO return-type,
*            msg_erro TO return-message.
*      APPEND return. CLEAR return.
*      v_erro = c_x.
*
*      EXIT.
*
*    ENDIF.
*
*    "ENDLOOP.
*
*    CLEAR: p_sdate, p_werks, p_lgort, p_matnr.
*
*  "ENDIF.
*
*
*endform.
*&---------------------------------------------------------------------*
*&      Form  VALIDA_MES_MM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DATA  text
*      -->P_I_BUKRS  text
*      <--P_VL_DATA_VAL  text
*      <--P_VL_MESSAGE  text
*----------------------------------------------------------------------*
FORM valida_mes_mm  USING    p_data_ent
                             p_bukrs
                    CHANGING p_data_val
                             p_message.

  DATA: wa_marv   TYPE marv,
        p_data_mp TYPE datum,
        p_ano     TYPE c LENGTH 4,
        p_mes     TYPE c LENGTH 2.


  CLEAR: p_data_val, p_message.

  SELECT SINGLE * INTO wa_marv
    FROM marv
   WHERE bukrs EQ p_bukrs.

  IF sy-subrc IS INITIAL.

    IF NOT wa_marv-xruem IS INITIAL.
      p_ano = wa_marv-vmgja.
      p_mes = wa_marv-vmmon.
      CONCATENATE p_ano p_mes '01' INTO p_data_mp.

      IF ( p_data_ent(4) = p_ano ) AND ( p_data_ent+4(2) = p_mes ).
        p_data_val = p_data_ent.
      ELSEIF  p_data_mp GE p_data_ent  . " >=
        p_data_val = p_data_mp.
      ENDIF.

    ENDIF.

    CHECK p_data_val IS INITIAL.

    p_ano = wa_marv-lfgja.
    p_mes = wa_marv-lfmon.
    CONCATENATE p_ano p_mes '01' INTO p_data_mp.

    IF ( p_data_ent(4) = p_ano ) AND ( p_data_ent+4(2) = p_mes ).
      p_data_val = p_data_ent.
    ELSEIF p_data_mp GE p_data_ent.
      p_data_val = p_data_mp.
    ENDIF.

    IF p_data_val IS INITIAL.
      CONCATENATE 'Data' p_data_ent 'sem período MM válido!' INTO p_message SEPARATED BY space. "MESSAGE e001 RAISING sem_periodo WITH p_data_ent.
    ENDIF.

  ELSE.
    CONCATENATE 'Período de MM não encontrado p/ empresa ' p_bukrs '!' INTO p_message SEPARATED BY space. " MESSAGE e002 RAISING periodo_nao_encontrado WITH p_bukrs.
  ENDIF.

ENDFORM.                    " VALIDA_MES_MM
*&---------------------------------------------------------------------*
*&      Form  YF_BLOQUEIA_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yf_bloqueia_material USING p_material
                                p_werks.


  DATA: vl_user    TYPE sy-msgv1,
        vl_message TYPE bapi_msg.

* ---> S4 Migration - 21/06/2023 - MA
  DATA: lv_material TYPE marc-matnr.
  lv_material = p_material.
*  CALL FUNCTION 'ENQUEUE_EMMARCE'
*    EXPORTING
*      matnr          = p_material "MARC_KEYTAB-MATNR
*      werks          = p_werks "MARC_KEYTAB-WERKS
*    EXCEPTIONS
*      foreign_lock   = 01
*      system_failure = 02.

  CALL FUNCTION 'ENQUEUE_EMMARCE'
    EXPORTING
      matnr          = lv_material "MARC_KEYTAB-MATNR
      werks          = p_werks "MARC_KEYTAB-WERKS
    EXCEPTIONS
      foreign_lock   = 01
      system_failure = 02.

* <--- S4 Migration - 21/06/2023 - MA

  vl_user  = sy-msgv1.            "ch zu 30e

  IF sy-subrc = 1 .
    CONCATENATE 'Os dados de centro ' p_werks  ', material' p_material 'estão bloqueados pelo usuário ' vl_user INTO vl_message SEPARATED BY space.
    vl_erro = 'X'.
  ELSEIF sy-subrc = 2 .
    CONCATENATE 'Erro ao bloquear material ' p_material  ', para o centro '  p_werks ', para a baixa do boletim' vl_user INTO vl_message SEPARATED BY space.
    vl_erro = 'X'.
  ENDIF.

  IF vl_erro = 'X'.
    "--Envia Log Interface
    CLEAR:   yt_execretrn.

    yt_execretrn-type       = cc_e.
    yt_execretrn-id         = cc_classe_mensagem.
    yt_execretrn-number     = '000'.
    yt_execretrn-message    = vl_message.
    yt_execretrn-message_v1 = vl_message.
    yt_execretrn-message_v2 = '60'.
    yt_execretrn-message_v3 = 'L'.

    APPEND yt_execretrn.

  ENDIF.

ENDFORM.                    " YF_BLOQUEIA_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  YF_DESBLOQUEIA_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yf_desbloqueia_material USING p_material
                                   p_werks.
* ---> S4 Migration - 21/06/2023 - MA
  DATA: lv_material TYPE marc-matnr.
  lv_material = p_material.

*  CALL FUNCTION 'DEQUEUE_EMMARCE'
*    EXPORTING
*      matnr = p_material
*      werks = p_werks.

  CALL FUNCTION 'DEQUEUE_EMMARCE'
    EXPORTING
      matnr = lv_material
      werks = p_werks.
* <--- S4 Migration - 21/06/2023 - MA

ENDFORM.                    " YF_BLOQUEIA_MATERIAL

*FORM Z_CHECK_MOV_CERTIFICADO USING P_ZPPS_XIMFBF_LOG TYPE ZPPS_XIMFBF_LOG.
*
*   CALL FUNCTION 'ZSD_CHECK_MOV_CERTIFICADO'
*     EXPORTING
*       I_TP_MOV           = 'E'
*       I_ZPPS_XIMFBF_LOG  = P_ZPPS_XIMFBF_LOG.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  AJUSTAR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MKPF  text
*      -->P_W_XI_MFBF  text
*----------------------------------------------------------------------*
FORM ajustar_log  USING  p_mkpf    TYPE mkpf
                         p_boletin TYPE zpps_ximfbf_log
                 CHANGING p_vl_ajustado TYPE char01.

  CHECK p_mkpf IS NOT INITIAL.
  CHECK p_mkpf-mblnr IS NOT INITIAL.
  CHECK p_boletin-obj_key IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(wa_blpp)
    FROM blpp
   WHERE belnr EQ @p_mkpf-mblnr.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE * INTO @DATA(wa_mseg)
    FROM mseg
   WHERE mblnr EQ @p_mkpf-mblnr
     AND mjahr EQ @p_mkpf-mjahr.

  CHECK sy-subrc IS INITIAL.

  vc_ult_doc_gerado  = p_mkpf-mblnr.
  vc_dt_lacto_gerado = p_mkpf-budat.
  vc_empr_doc_gerado = wa_mseg-bukrs.

  UPDATE zpps_ximfbf_log
     SET mblnr = p_mkpf-mblnr
         confirmation = wa_blpp-prtnr
   WHERE obj_key = p_boletin-obj_key.

  CHECK sy-subrc IS INITIAL.

  p_vl_ajustado = abap_true.

ENDFORM.

*** Stefanini - IR231105 - 27/08/2025 - LAZAROSR - Início de Alteração
FORM enviar_bol_sucesso.

  DATA:
    lt_sucesso_cabecalho TYPE TABLE OF zmme_return_sucess,
    lt_sucesso_item      TYPE TABLE OF zmme_return_sucess,
    lt_sucesso_envio     TYPE TABLE OF zmme_return_sucess,
    lt_zppt0041          TYPE TABLE OF zppt0041,
    ls_zppt0041          TYPE zppt0041.


  lt_sucesso_cabecalho = yt_geo_oubound_ok[].
  lt_sucesso_item      = yt_geo_oubound_ok[].

  SORT lt_sucesso_item BY idbol.

  SORT lt_sucesso_cabecalho BY idbol.
  DELETE ADJACENT DUPLICATES FROM lt_sucesso_cabecalho COMPARING idbol.

  LOOP AT lt_sucesso_cabecalho INTO DATA(ls_cabecalho).

    CLEAR lt_sucesso_envio.

    READ TABLE lt_sucesso_item TRANSPORTING NO FIELDS
                            WITH KEY idbol  = ls_cabecalho-idbol
                                                    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT lt_sucesso_item INTO DATA(ls_item) FROM sy-tabix.

        IF ls_cabecalho-idbol NE ls_item-idbol.
          EXIT.
        ENDIF.

        APPEND ls_item TO lt_sucesso_envio.

      ENDLOOP.

    ENDIF.

    IF lt_sucesso_envio IS NOT INITIAL.

      CALL FUNCTION 'Z_MM_OUTBOUND_BOL_SUCESS'
        TABLES
          return_sucess = lt_sucesso_envio.

      COMMIT WORK.

      PERFORM preencher_zppt0041 USING ls_cabecalho-idbol
                                       ls_cabecalho-nrobol
                                       'S'
                                 CHANGING ls_zppt0041.

      APPEND ls_zppt0041 TO lt_zppt0041.

    ENDIF.

  ENDLOOP.

  MODIFY zppt0041 FROM TABLE lt_zppt0041.
  IF sy-subrc IS INITIAL.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.

FORM preencher_zppt0041 USING i_v_idbol    TYPE zppt0041-idbol
                              i_v_nrobol   TYPE zppt0041-nrobol
                              i_v_tp_envio TYPE zppt0041-tp_envio
                        CHANGING c_s_zppt0041 TYPE zppt0041.

  TRY.

      CLEAR c_s_zppt0041.

      c_s_zppt0041-id_log   = cl_system_uuid=>create_uuid_x16_static( ).
      c_s_zppt0041-idbol    = i_v_idbol.
      c_s_zppt0041-nrobol   = i_v_nrobol.
      c_s_zppt0041-uname    = sy-uname.
      c_s_zppt0041-datum    = sy-datum.
      c_s_zppt0041-uzeit    = sy-uzeit.
      c_s_zppt0041-tp_envio = i_v_tp_envio.

    CATCH cx_root.
      " Ocorreu um erro...
  ENDTRY.

ENDFORM.
*** Stefanini - IR231105 - 27/08/2025 - LAZAROSR - Fim de Alteração
