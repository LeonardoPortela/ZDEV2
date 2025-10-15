*&---------------------------------------------------------------------*
*& Report  ZSDR0058
*&
*&---------------------------------------------------------------------*
*& Observacao
*& Este programa estava com sua ultima versao na ordem abaixo. Tirei
*& o objeto desta ordem e a coloquei na nova request
*& Request antiga: DEVK9A16RD       ABAP         16.08.2022 teste
*& Request atual : DEVK9A17K1       ABAP         01.09.2022 SD-010922-CS2021000218-ZSDT0109-Teste API AgriQ #89886 JT
*&---------------------------------------------------------------------*
REPORT zsdr0058.

TABLES: j_1bnfdoc, zcte_ciot, j_1bnfe_active, zcte_identifica, vbak, zib_nfe_forn, zlest0073, vttp, vbfa,zsdt0001, zib_contabil, zglt050,
        j_1bbranch,zsdt0133.

DATA: tg_j_1bnfdoc TYPE TABLE OF j_1bnfdoc WITH HEADER LINE,
      tg_zcte_ciot TYPE TABLE OF zcte_ciot WITH HEADER LINE.

*----------------------------------------------------------------------*
* tela de Seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_docnum  FOR j_1bnfdoc-docnum NO-EXTENSION NO INTERVALS.
  PARAMETER p_fgl_mn AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: p_c_ciot  FOR zcte_ciot-cd_ciot NO-EXTENSION NO INTERVALS,
                  p_canc    FOR zcte_ciot-cancelado NO-EXTENSION NO INTERVALS,
                  p_nrciot  FOR zcte_ciot-nr_ciot NO-EXTENSION NO INTERVALS,
                  p_status  FOR zcte_ciot-st_ciot NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: p_dc_002  FOR zcte_identifica-docnum NO-EXTENSION NO INTERVALS,
                  p_reas    FOR zcte_identifica-reason  NO-EXTENSION NO INTERVALS,
                  p_reas_1  FOR zcte_identifica-reason1 NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  SELECT-OPTIONS: p_vbeln  FOR vbak-vbeln NO-EXTENSION NO INTERVALS,
                  p_tknum  FOR vbak-tknum NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS: p_chave   FOR zib_nfe_forn-nu_chave NO-EXTENSION NO INTERVALS,
                  p_brc_05  FOR zib_nfe_forn-branch NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b5.

SELECTION-SCREEN: BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-006.
  PARAMETER p_chec AS CHECKBOX DEFAULT ''.
  PARAMETER p_file TYPE rlgrap-filename DEFAULT 'C:\maggi\planilha.xlsx'.
SELECTION-SCREEN: END OF BLOCK b6.

SELECTION-SCREEN: BEGIN OF BLOCK b7 WITH FRAME TITLE TEXT-007.
  PARAMETER p_impgr AS CHECKBOX DEFAULT ''.
  PARAMETER p_filegr TYPE rlgrap-filename DEFAULT 'C:\maggi\planilha_gr.xlsx'.
SELECTION-SCREEN: END OF BLOCK b7.

SELECTION-SCREEN: BEGIN OF BLOCK b8 WITH FRAME TITLE TEXT-008.
  SELECT-OPTIONS: p_chv_aq  FOR zlest0073-chave_nfe NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b8.

SELECTION-SCREEN: BEGIN OF BLOCK b9 WITH FRAME TITLE TEXT-009.
  SELECT-OPTIONS: tknum_09 FOR vttp-tknum NO-EXTENSION NO INTERVALS,
                  tpnum_09 FOR vttp-tpnum NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b9.


SELECTION-SCREEN: BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-010.
  PARAMETER p_chk_10 AS CHECKBOX DEFAULT ''.
  SELECT-OPTIONS: vbelv_10 FOR vbfa-vbelv   NO-EXTENSION NO INTERVALS,
                  posnv_10 FOR vbfa-posnv   NO-EXTENSION NO INTERVALS,
                  vbeln_10 FOR vbfa-vbeln   NO-EXTENSION NO INTERVALS,
                  posnn_10 FOR vbfa-posnn   NO-EXTENSION NO INTERVALS,
                  vbtpn_10 FOR vbfa-vbtyp_n NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b10.

SELECTION-SCREEN: BEGIN OF BLOCK b11 WITH FRAME TITLE TEXT-011.
  PARAMETER: p_chk_11 AS CHECKBOX DEFAULT '',
             sq_dc TYPE c LENGTH 2.
  SELECT-OPTIONS: ch_ref   FOR zsdt0001-ch_referencia  NO-EXTENSION NO INTERVALS,
                  dc_rom   FOR zsdt0001-doc_rem        NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b11.


SELECTION-SCREEN: BEGIN OF BLOCK b12 WITH FRAME TITLE TEXT-012.
  PARAMETER: p_chk_12 AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN: END OF BLOCK b12.

SELECTION-SCREEN: BEGIN OF BLOCK b13 WITH FRAME TITLE TEXT-013.
  PARAMETER: p_chk_13 AS CHECKBOX DEFAULT ''.
  SELECT-OPTIONS: p_objkey  FOR zib_contabil-obj_key NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b13.

SELECTION-SCREEN: BEGIN OF BLOCK b14 WITH FRAME TITLE TEXT-014.
  PARAMETER: p_chk_14 AS CHECKBOX DEFAULT ''.
  SELECT-OPTIONS: p_seqlct  FOR zglt050-seq_lcto   NO-EXTENSION NO INTERVALS,
                  p_dtcria  FOR zglt050-dt_criacao NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b14.

SELECTION-SCREEN: BEGIN OF BLOCK b15 WITH FRAME TITLE TEXT-015.
  PARAMETER: p_chk_15 AS CHECKBOX DEFAULT ''.
  SELECT-OPTIONS: p_branch  FOR j_1bbranch-branch  NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b15.

SELECTION-SCREEN: BEGIN OF BLOCK b16 WITH FRAME TITLE TEXT-016.

  PARAMETER: p_chk_16 AS CHECKBOX DEFAULT ''.
  SELECT-OPTIONS: p_dc01  FOR j_1bnfdoc-docnum NO-EXTENSION NO INTERVALS.
  PARAMETER: p_docsta TYPE j_1bnfe_active-docsta.
  PARAMETER: p_scssta TYPE j_1bnfe_active-scssta.
  PARAMETER: p_cancel TYPE j_1bnfe_active-cancel.
  PARAMETER: p_actreq TYPE j_1bnfe_active-action_requ.
  PARAMETER: p_msstat TYPE j_1bnfe_active-msstat.
SELECTION-SCREEN: END OF BLOCK b16.

*-CS2021000218-01.09.2022-#89886-JT-inicio
SELECTION-SCREEN: BEGIN OF BLOCK b17 WITH FRAME TITLE TEXT-017.
  PARAMETER: p_chk_17 AS CHECKBOX DEFAULT ''.

  PARAMETER: p_url        TYPE ui_src_url,
             p_user       TYPE zde_ui_src_username60,
             p_pass       TYPE zde_ui_src_password60,
             p_add1       TYPE zde_add01,
             p_add2       TYPE zde_add02.
SELECTION-SCREEN: END   OF BLOCK b17.
*-CS2021000218-01.09.2022-#89886-JT-fim

SELECTION-SCREEN: BEGIN OF BLOCK b18 WITH FRAME TITLE TEXT-018.
  PARAMETER: p_chk_18 AS CHECKBOX DEFAULT ''.
  SELECT-OPTIONS: s_nro_cg  FOR zsdt0133-nro_cg.
SELECTION-SCREEN: END   OF BLOCK b18.


START-OF-SELECTION.


  DATA: wl_branch_detail TYPE bapibranch.

  DATA: vl_filter TYPE string.

  IF p_docnum-low IS NOT INITIAL.

    vl_filter = p_docnum-low.

    CHECK vl_filter NE '*'.

    IF p_fgl_mn IS NOT INITIAL.
      DATA: wl_j_1bnfdoc TYPE j_1bnfdoc.

      CLEAR: wl_j_1bnfdoc.
      SELECT SINGLE * INTO wl_j_1bnfdoc
        FROM j_1bnfdoc
       WHERE docnum EQ p_docnum-low.

      CHECK sy-subrc = 0.

      IF wl_j_1bnfdoc-manual IS INITIAL.
        wl_j_1bnfdoc-manual = 'X'.
      ELSE.
        wl_j_1bnfdoc-manual = ''.
      ENDIF.

      MODIFY j_1bnfdoc FROM wl_j_1bnfdoc.

    ELSE.
      SELECT *
        FROM j_1bnfdoc AS a INTO TABLE tg_j_1bnfdoc
       WHERE a~docnum EQ p_docnum-low
         AND a~crenam EQ 'R3JOB'
         AND EXISTS ( SELECT docnum
                        FROM j_1bnfe_active AS b
                       WHERE b~docnum EQ a~docnum
                         AND b~docsta EQ '1'
                         AND b~scssta EQ '' ).

      LOOP AT tg_j_1bnfdoc.
        PERFORM alterar_status_doc USING '2' tg_j_1bnfdoc-docnum.
      ENDLOOP.
    ENDIF.

  ELSEIF p_c_ciot-low IS NOT INITIAL.

    vl_filter = p_c_ciot-low.

    CHECK vl_filter NE '*'.

    SELECT *
      FROM zcte_ciot INTO TABLE tg_zcte_ciot
     WHERE cd_ciot EQ p_c_ciot-low.

    LOOP AT tg_zcte_ciot.
      tg_zcte_ciot-cancelado = p_canc-low.
      tg_zcte_ciot-st_ciot   = p_status-low.

      IF p_nrciot-low IS NOT INITIAL.
        tg_zcte_ciot-nr_ciot = p_nrciot-low.
      ENDIF.

      MODIFY zcte_ciot FROM tg_zcte_ciot.
    ENDLOOP.

  ELSEIF p_dc_002-low IS NOT INITIAL.

    vl_filter = p_dc_002-low.

    CHECK vl_filter NE '*'.

    UPDATE zcte_identifica SET reason  =  p_reas-low
                               reason1 =  p_reas_1-low
                         WHERE docnum EQ p_dc_002-low.

  ELSEIF p_vbeln-low IS NOT INITIAL.

    vl_filter = p_vbeln-low.

    CHECK vl_filter NE '*'.

    UPDATE vbak SET tknum = p_tknum-low
              WHERE vbeln EQ p_vbeln-low.

  ELSEIF p_chave-low IS NOT INITIAL.

    vl_filter = p_chave-low.
    CHECK vl_filter NE '*'.

    UPDATE zib_nfe_forn SET branch   = p_brc_05-low
                      WHERE nu_chave EQ p_chave-low.

  ELSEIF p_chec IS NOT INITIAL.
    PERFORM importa_excel.
  ELSEIF p_impgr IS NOT INITIAL.
    PERFORM imp_excel_ee_gr.
  ELSEIF p_chv_aq-low IS NOT INITIAL.
    DELETE FROM zlest0060 WHERE chave_nfe    = p_chv_aq-low
                            AND peso_fiscal  = 0
                            AND peso_liq_ret = 0.

    DELETE FROM zlest0073 WHERE chave_nfe        = p_chv_aq-low
                            AND peso_vinculado   = 0
                            AND peso_liqret_vinc = 0.

  ELSEIF tknum_09-low IS NOT INITIAL AND
         tpnum_09-low IS NOT INITIAL.

    SELECT SINGLE *
      FROM vttp INTO @DATA(wl_vttp)
     WHERE tknum = @tknum_09-low
       AND tpnum = @tpnum_09-low.

    IF sy-subrc = 0.
      DELETE FROM vttp WHERE tknum = wl_vttp-tknum
                         AND tpnum = wl_vttp-tpnum.
    ENDIF.

  ELSEIF p_chk_10 IS NOT INITIAL.

    SELECT SINGLE *
      FROM vbfa INTO @DATA(wl_vbfa)
     WHERE vbelv    = @vbelv_10-low
       AND posnv    = @posnv_10-low
       AND vbeln    = @vbeln_10-low
       AND posnn    = @posnn_10-low
       AND vbtyp_n  = @vbtpn_10-low.

    IF sy-subrc = 0.
      DELETE FROM vbfa WHERE vbelv    = wl_vbfa-vbelv
                         AND posnv    = wl_vbfa-posnv
                         AND vbeln    = wl_vbfa-vbeln
                         AND posnn    = wl_vbfa-posnn
                         AND vbtyp_n  = wl_vbfa-vbtyp_n.
    ENDIF.

  ELSEIF ( p_chk_11   IS NOT INITIAL ) AND
         ( sq_dc      IS NOT INITIAL ) AND
         ( ch_ref-low IS NOT INITIAL ).

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(wl_0001)
     WHERE ch_referencia EQ @ch_ref-low.

    CHECK sy-subrc = 0.

    CASE sq_dc.
      WHEN '01'.
        wl_0001-seq_lcto     = dc_rom-low.
      WHEN '02'.
        wl_0001-nro_nf_rem   = dc_rom-low.
      WHEN '03'.
        wl_0001-doc_aviso    = dc_rom-low.
      WHEN '04'.
        wl_0001-doc_rem      = dc_rom-low.
      WHEN '05'.
        wl_0001-fatura_prod  = dc_rom-low.
      WHEN '06'.
        wl_0001-nro_nf_prod  = dc_rom-low.
      WHEN '07'.
        wl_0001-doc_transp   = dc_rom-low.
      WHEN '08'.
        wl_0001-fknum        = dc_rom-low.
      WHEN '09'.
        wl_0001-ov_frete     = dc_rom-low.
      WHEN '10'.
        wl_0001-fatura_frete = dc_rom-low.
      WHEN '11'.
        wl_0001-nro_nf_frete = dc_rom-low.
    ENDCASE.

    MODIFY zsdt0001 FROM wl_0001.

  ELSEIF ( p_chk_12   IS NOT INITIAL ).

    SELECT SINGLE *
      FROM setleaf INTO @DATA(_wl_setleaf)
     WHERE setname = 'ZLES0077_IMP_AJUS_NFE'
       AND valfrom = @sy-uname.

    IF sy-subrc = 0.
      PERFORM f_imp_arq_ajuste_zles0077.
    ENDIF.

  ELSEIF ( p_chk_13   IS NOT INITIAL ).

    SELECT SINGLE *
      FROM zib_contabil INTO @DATA(wl_zib_contabil)
     WHERE obj_key EQ @p_objkey-low.

    CHECK ( sy-subrc = 0 ) AND ( p_objkey-low IS NOT INITIAL ).

    UPDATE zib_contabil SET rg_atualizado = 'N'
     WHERE obj_key EQ @p_objkey-low.

  ELSEIF ( p_chk_14 IS NOT INITIAL ).

    CHECK p_dtcria-low IS NOT INITIAL AND ( p_seqlct-low IS NOT INITIAL ).

    UPDATE zglt050 SET dt_criacao = p_dtcria-low
     WHERE seq_lcto = p_seqlct-low.

  ELSEIF ( p_chk_15 IS NOT INITIAL ).

    LOOP AT p_branch INTO DATA(wl_branch_par).

      CHECK wl_branch_par-low IS NOT INITIAL.

      SELECT SINGLE *
        FROM j_1bbranch  INTO @DATA(wl_branch)
       WHERE branch EQ @wl_branch_par-low.

      CHECK sy-subrc EQ 0.

      CLEAR: wl_branch_detail.

      CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
        EXPORTING
          company       = wl_branch-bukrs
          branch        = wl_branch-branch
        IMPORTING
          branch_detail = wl_branch_detail.

      UPDATE j_1bbranch SET stcd1 = wl_branch_detail-cgc_number
       WHERE branch EQ wl_branch-branch.

    ENDLOOP.

  ELSEIF ( p_chk_16 IS NOT INITIAL ).

    SELECT SINGLE *
      FROM j_1bnfe_active INTO @DATA(wl_active_01)
     WHERE docnum EQ @p_dc01-low.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(wl_doc_01)
     WHERE docnum EQ @p_dc01-low.

    CHECK sy-subrc EQ 0.

    wl_active_01-docsta       = p_docsta.
    wl_active_01-scssta       = p_scssta.
    wl_active_01-cancel       = p_cancel.
    wl_active_01-action_requ  = p_actreq.
    wl_active_01-msstat       = p_msstat.

    MODIFY j_1bnfe_active FROM wl_active_01.

    wl_doc_01-docstat      = p_docsta.
    wl_doc_01-cancel       = p_cancel.

    MODIFY j_1bnfdoc FROM wl_doc_01.

  ELSEIF ( p_chk_17   IS NOT INITIAL ).

    PERFORM f_testa_agriq.

  ELSEIF ( p_chk_18   IS NOT INITIAL ).

    SELECT *
      FROM zsdt0133 INTO TABLE @DATA(lit_zsdt01133)
     WHERE nro_cg IN @s_nro_cg.

    DELETE lit_zsdt01133 WHERE versao_processo IS INITIAL.


    LOOP AT lit_zsdt01133 INTO DATA(lwa_zsdt0133).

      zcl_carga_saida_insumos=>preencher_lotes_carga(
        EXPORTING
          i_nro_cg    = lwa_zsdt0133-nro_cg
        RECEIVING
          r_msg_error = DATA(lva_msg_error)
      ).

      MESSAGE  lva_msg_error TYPE 'S'.

    ENDLOOP.

  ENDIF.

FORM alterar_status_doc USING p_tp_authcod TYPE zib_nota_fiscal_sap-tp_authcod
                              p_docnum     TYPE zib_nota_fiscal_sap-nu_documento_sap.


  DATA: ls_acttab_new TYPE j_1bnfe_active,
        ls_doc_new    TYPE j_1bnfdoc.

  CHECK p_docnum IS NOT INITIAL.

* Get current status of NF-e
  CALL FUNCTION 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
    EXPORTING
      i_docnum = p_docnum
    IMPORTING
      e_acttab = ls_acttab_new
    EXCEPTIONS
      no_entry = 1
      OTHERS   = 2.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE * INTO ls_doc_new
    FROM j_1bnfdoc
   WHERE docnum EQ p_docnum.

  CHECK sy-subrc EQ 0.

  CASE p_tp_authcod.
    WHEN '2'.

      "Nota Fiscal
      MOVE '1'                   TO ls_doc_new-docstat.
      "MOVE IT_NOTAS-AUTHCOD      TO LS_DOC_NEW-AUTHCOD.
      "MOVE IT_NOTAS-CODE         TO LS_DOC_NEW-CODE.

      "Active Nota Fiscal
      MOVE 'C'                   TO ls_acttab_new-action_requ.
      MOVE '2'                   TO ls_acttab_new-scssta.
      "MOVE IT_NOTAS-AUTHCOD     TO LS_ACTTAB_NEW-AUTHCOD.
      MOVE 'X'                   TO ls_acttab_new-cancel.
      MOVE 'B'                   TO ls_acttab_new-msstat.

      MODIFY j_1bnfdoc      FROM ls_doc_new.
      MODIFY j_1bnfe_active FROM ls_acttab_new.

*     DATA: IT_NOTAS    TYPE TABLE OF ZIB_NOTA_FISCAL_SAP,
*           WA_NOTA     TYPE ZIB_NOTA_FISCAL_SAP.

*     CLEAR: IT_NOTAS[], WA_NOTA.
*
*     CHECK P_DOCNUM IS NOT INITIAL.
*
*     WA_NOTA-TP_AUTHCOD       = P_TP_AUTHCOD.
*     WA_NOTA-NU_DOCUMENTO_SAP = P_DOCNUM.
*     WA_NOTA-DT_AUTHCOD       = SY-DATUM.
*     IF SY-TIMLO IS INITIAL.
*       WA_NOTA-HR_AUTHCOD     = SY-UZEIT.
*     ELSE.
*       WA_NOTA-HR_AUTHCOD     = SY-TIMLO.
*     ENDIF.
*     WA_NOTA-DOCSTAT          = '1'.
*
*     "CONCATENATE P_MSGV1 P_MSGV2 P_MSGV3 P_MSGV4 INTO WA_NOTA-MS_ERRO SEPARATED BY SPACE.
*
*     APPEND WA_NOTA TO IT_NOTAS.
*
*     CALL FUNCTION 'Z_SD_INBOUND_NFE_XML'
*       TABLES
*        IT_NOTAS  = IT_NOTAS.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPORTA_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM importa_excel .


  TYPES: BEGIN OF ty_0041.
           INCLUDE TYPE zglt041.
  TYPES: END OF ty_0041.

  TYPES: BEGIN OF ty_erro,
           msg(100),
         END OF ty_erro.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

  DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata,   "Guarda o mapeamento
        it_erro    TYPE TABLE OF ty_erro.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
  DATA: wa_bdcdata LIKE LINE OF ti_bdcdata,
        wa_0041    TYPE ty_0041,
        wa_erro    TYPE ty_erro.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
  DATA: BEGIN OF it_msg OCCURS 0.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF it_msg.
  DATA: wl_mode(1),
        wg_documento(10),
        vmsg(50),
        wl_erro(1),
        wl_seq           TYPE i,
        wl_seq1          TYPE c LENGTH 11,
        wl_seq2(5),
        wa_skb1          TYPE skb1.

  DATA: t_excel  LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
        t_excel2 LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.


  CLEAR t_excel.
  REFRESH t_excel.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 3
      i_end_col               = 11
      i_end_row               = 10000
    TABLES
      intern                  = t_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  CASE sy-subrc.
    WHEN 1.
      MESSAGE 'Parâmetros inválidos.' TYPE 'I'.
    WHEN 2.
      MESSAGE 'Não foi possível ler o arquivo, verifique o caminho e tente novamente.' TYPE 'I'.
    WHEN 3.
      MESSAGE 'Erro ao importar o arquivo.' TYPE 'I'.
  ENDCASE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Atualizando Dados'.

  t_excel2[] = t_excel[].

  SORT t_excel2 BY row col.

  CLEAR: t_excel2, wl_seq.

  LOOP AT t_excel.
    IF t_excel-row = t_excel2-row.
      CONTINUE.
    ENDIF.
    LOOP AT t_excel2 WHERE row = t_excel-row.
      CASE t_excel2-col.
        WHEN 1.
          "Empresa
          wa_0041-bukrs            = t_excel2-value.
        WHEN 2.
          "Conta
          wa_0041-saknr            = t_excel2-value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_0041-saknr
            IMPORTING
              output = wa_0041-saknr.
        WHEN 4.
          "cod.Balanço
          wa_0041-cod_clas_bal     = t_excel2-value.
        WHEN 5.
          "cod.Nota
          wa_0041-cod_clas_not2    = t_excel2-value.
        WHEN 6.
          "Cta.Monetaria (S/N)
          wa_0041-cta_monet        = t_excel2-value.
        WHEN 7.
          "Cta Intercompany (S/N)
          wa_0041-cta_intercompany = t_excel2-value.
        WHEN 8.
          "Cod.Departamento
          wa_0041-dep_resp2        = t_excel2-value.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_0041-dep_resp2
            IMPORTING
              output = wa_0041-dep_resp2.
        WHEN 9.
          "Usuario responsavel
          wa_0041-bname2           = t_excel2-value.
        WHEN 10.
          "Prazo Entrega
          wa_0041-prazo_entr       = t_excel2-value.
        WHEN 11.
          "Criterio Vcto Partidas em aberto
          wa_0041-crit_vecto       = t_excel2-value.
      ENDCASE.
    ENDLOOP.

    ADD 1 TO wl_seq.

    wl_seq1 = wl_seq.

    CONCATENATE 'Gravando linha:' wl_seq1 INTO vmsg.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = vmsg.

    SELECT SINGLE * INTO wa_skb1       "#EC CI_DB_OPERATION_OK[2431747]
      FROM skb1
     WHERE bukrs EQ wa_0041-bukrs
       AND saknr EQ wa_0041-saknr.

    IF sy-subrc IS INITIAL.
      INSERT INTO zglt041 VALUES wa_0041.
    ENDIF.

  ENDLOOP.

  MESSAGE 'Fim atualização' TYPE 'I'.

ENDFORM.

FORM imp_excel_ee_gr .

  DATA: gt_planilha LIKE STANDARD TABLE OF alsmex_tabline,
        wl_planilha LIKE alsmex_tabline,
        wl_ee_docs  TYPE zmmt_ee_zgr_docs.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = TEXT-i01.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_filegr
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 55
      i_end_row               = 10000
    TABLES
      intern                  = gt_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF gt_planilha[] IS INITIAL.
    MESSAGE 'Nenhum registro encontrado!' TYPE 'S'.
    RETURN.
  ENDIF.

  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR: wl_ee_docs.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        wl_ee_docs-obj_key   = wl_planilha-value.
      WHEN 2.
        wl_ee_docs-bukrs     = wl_planilha-value.
      WHEN 3.
        wl_ee_docs-branch    = wl_planilha-value.
      WHEN 4.
        wl_ee_docs-po_number = wl_planilha-value.
      WHEN 5.
        wl_ee_docs-av_vbeln  = wl_planilha-value.
      WHEN 6.
        wl_ee_docs-mm_mblnr  = wl_planilha-value.
      WHEN 7.
        wl_ee_docs-mm_mjahr  = wl_planilha-value.
      WHEN 8.
        wl_ee_docs-ft_belnr  = wl_planilha-value.
      WHEN 9.
        wl_ee_docs-ft_gjahr  = wl_planilha-value.
      WHEN 10.
        wl_ee_docs-docnum    = wl_planilha-value.
    ENDCASE.

    AT END OF row.
      IF ( wl_ee_docs-obj_key  IS NOT INITIAL ).
        MODIFY zmmt_ee_zgr_docs FROM wl_ee_docs.
      ENDIF.
    ENDAT.

  ENDLOOP.

ENDFORM.

FORM f_imp_arq_ajuste_zles0077.

  TYPES: BEGIN OF ty_saida_vinc_romaneio,
           check        TYPE c,
           chave_nfe    TYPE c LENGTH 44,
           peso_fiscal  TYPE zlest0060-peso_fiscal,
           netwr        TYPE zlest0060-netwr,
           peso_liq_ret	TYPE zlest0060-peso_liq_ret,
           vlr_liq_ret  TYPE zlest0060-vlr_liq_ret,
           docnum       TYPE zlest0060-docnum,
         END OF ty_saida_vinc_romaneio.

  DATA: gt_planilha LIKE STANDARD TABLE OF alsmex_tabline,
        wl_planilha LIKE alsmex_tabline,
        vl_dt_temp  TYPE sydatum,
        var_answer  TYPE c,
        tg_arq_proc TYPE TABLE OF ty_saida_vinc_romaneio WITH HEADER LINE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = TEXT-i14.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = 'C:\Amaggi\ZLES0077\ajuste_nfe.xlsx'
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 55
      i_end_row               = 10000
    TABLES
      intern                  = gt_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF gt_planilha[] IS INITIAL.
    MESSAGE s836(sd) WITH TEXT-e60 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.


  DATA(_index) = 0.
  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR: tg_arq_proc.
      ADD 1 TO _index.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        tg_arq_proc-chave_nfe    = wl_planilha-value.
      WHEN 2.
        tg_arq_proc-peso_fiscal  = wl_planilha-value.
      WHEN 3.
        tg_arq_proc-netwr        = wl_planilha-value.
      WHEN 4.
        tg_arq_proc-peso_liq_ret = wl_planilha-value.
      WHEN 5.
        tg_arq_proc-vlr_liq_ret  = wl_planilha-value.
      WHEN 6.
        tg_arq_proc-docnum       = wl_planilha-value.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = tg_arq_proc-docnum
          IMPORTING
            output = tg_arq_proc-docnum.
    ENDCASE.

    AT END OF row.
      CHECK tg_arq_proc-chave_nfe IS NOT INITIAL.
      APPEND tg_arq_proc.
    ENDAT.

  ENDLOOP.

  DATA(_lines) = lines( tg_arq_proc[] ).

  CHECK _lines > 0.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = | { _lines } registro(s) encontrado(s). Confirma processamento?|
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  SELECT *
    FROM zlest0060 INTO TABLE @DATA(_tg_0060_ajuste)
     FOR ALL ENTRIES IN @tg_arq_proc
   WHERE chave_nfe = @tg_arq_proc-chave_nfe.

  LOOP AT _tg_0060_ajuste INTO DATA(_wl_0060_ajuste) .

    CHECK _wl_0060_ajuste-peso_liq_ret IS NOT INITIAL.

    READ TABLE tg_arq_proc WITH KEY chave_nfe = _wl_0060_ajuste-chave_nfe
                                    docnum    = _wl_0060_ajuste-docnum.

    IF ( sy-subrc = 0 ) AND ( _wl_0060_ajuste-docnum IS NOT INITIAL ) AND
       ( tg_arq_proc-peso_fiscal  IS NOT INITIAL ) AND ( tg_arq_proc-netwr       IS NOT INITIAL ) AND
       ( tg_arq_proc-peso_liq_ret IS NOT INITIAL ) AND ( tg_arq_proc-vlr_liq_ret IS NOT INITIAL ).
      _wl_0060_ajuste-peso_fiscal   = tg_arq_proc-peso_fiscal.
      _wl_0060_ajuste-netwr         = tg_arq_proc-netwr.
      _wl_0060_ajuste-peso_liq_ret  = tg_arq_proc-peso_liq_ret.
      _wl_0060_ajuste-vlr_liq_ret   = tg_arq_proc-vlr_liq_ret.
    ELSE.
      _wl_0060_ajuste-peso_fiscal = _wl_0060_ajuste-peso_liq_ret.
      _wl_0060_ajuste-netwr       = _wl_0060_ajuste-vlr_liq_ret.
    ENDIF.
    MODIFY zlest0060 FROM _wl_0060_ajuste.

    LOOP AT tg_arq_proc WHERE chave_nfe = _wl_0060_ajuste-chave_nfe.
      tg_arq_proc-check = 'X'.
      MODIFY tg_arq_proc.
    ENDLOOP.

  ENDLOOP.

  LOOP AT tg_arq_proc WHERE check IS NOT INITIAL.
    PERFORM f_atualiza_saldo_nf USING tg_arq_proc-chave_nfe.
  ENDLOOP.

  MESSAGE 'Processamento concluído!' TYPE 'S'.

ENDFORM.

FORM f_atualiza_saldo_nf  USING p_chave_nfe TYPE zlest0060-chave_nfe.

  DATA: tg_0060      TYPE TABLE OF zlest0060 WITH HEADER LINE,
        wl_zlest0073 TYPE zlest0073,
        wl_zlest0056 TYPE zlest0056,
        wl_zlest0104 TYPE zlest0104,
        wl_zsdt0001  TYPE zsdt0001,
        vl_chave     TYPE zlest0073,
        wl_j_1bnflin TYPE j_1bnflin.


  vl_chave = p_chave_nfe.

  DELETE FROM zlest0073 WHERE chave_nfe EQ vl_chave.
  COMMIT WORK.

  SELECT *
    FROM zlest0060 INTO TABLE tg_0060
   WHERE chave_nfe EQ vl_chave.

  CHECK tg_0060[] IS NOT INITIAL.

  CLEAR: wl_zlest0073, wl_zsdt0001, wl_zlest0056.

  wl_zlest0073-chave_nfe = vl_chave.

  LOOP AT tg_0060.

    IF ( wl_zlest0073-peso_origem  IS INITIAL ) OR
       ( wl_zlest0073-valor_origem IS INITIAL ).

      CLEAR: wl_zlest0104.
      SELECT SINGLE * FROM zlest0104 INTO wl_zlest0104 WHERE emissor = tg_0060-werks.

      SELECT SINGLE *
        FROM zsdt0001 INTO wl_zsdt0001
       WHERE tp_movimento  = 'E'
         AND bukrs         = wl_zlest0104-bukrs
         AND branch        = wl_zlest0104-branch
         AND nr_romaneio   = tg_0060-nr_romaneio
         AND nr_safra      = tg_0060-safra
         AND parid         = tg_0060-rm_codigo.

      CHECK sy-subrc = 0.

      wl_zlest0073-valor_origem  = wl_zsdt0001-netwr.
      wl_zlest0073-peso_origem   = wl_zsdt0001-peso_fiscal.
      wl_zlest0073-peso_subtotal = wl_zsdt0001-peso_subtotal.

    ENDIF.

    ADD tg_0060-peso_fiscal   TO  wl_zlest0073-peso_vinculado.
    ADD tg_0060-netwr         TO  wl_zlest0073-valor_vinculado.
    ADD tg_0060-peso_subtotal TO  wl_zlest0073-peso_subtot_vinc.
    ADD tg_0060-peso_liq_ret  TO  wl_zlest0073-peso_liqret_vinc.
    ADD tg_0060-peso_retido   TO  wl_zlest0073-peso_retido_vinc.
  ENDLOOP.

  INSERT zlest0073 FROM wl_zlest0073.

  IF wl_zsdt0001 IS NOT INITIAL.
    CLEAR: wl_zsdt0001-vinc_tot_aquav, wl_zsdt0001-cte_emit_aquav.
    wl_zsdt0001-peso_retido_real = wl_zlest0073-peso_retido_vinc.
    wl_zsdt0001-peso_liqret_real = wl_zlest0073-peso_liqret_vinc.

    IF ( wl_zlest0073-peso_origem - wl_zlest0073-peso_vinculado ) <= 0.
      wl_zsdt0001-vinc_tot_aquav = 'X'.
    ENDIF.

    MODIFY zsdt0001 FROM wl_zsdt0001.
  ENDIF.

  COMMIT WORK.

ENDFORM.

FORM f_gera_doc.

  DATA: yt_log_pozgr   TYPE TABLE OF zfie_ret_document
                       WITH HEADER LINE INITIAL SIZE 0,
        yt_poitem      TYPE TABLE OF bapimepoitem
                       WITH HEADER LINE INITIAL SIZE 0,
        yt_partner     TYPE TABLE OF  bapiekkop
                       WITH HEADER LINE INITIAL SIZE 0,
        yt_poship      TYPE TABLE OF  bapiitemship
                       WITH HEADER LINE INITIAL SIZE 0,

        yt_poshipx     TYPE TABLE OF  bapiitemshipx
                      WITH HEADER LINE INITIAL SIZE 0,

        yt_poitemx     TYPE TABLE OF bapimepoitemx
                       WITH HEADER LINE INITIAL SIZE 0,
        yt_poschedule  TYPE TABLE OF bapimeposchedule
                       WITH HEADER LINE INITIAL SIZE 0,
        yt_poschedulex TYPE TABLE OF bapimeposchedulx
                       WITH HEADER LINE INITIAL SIZE 0,
        yt_poaccount   TYPE TABLE OF bapimepoaccount
                       WITH HEADER LINE INITIAL SIZE 0,
        yt_poaccountx  TYPE TABLE OF bapimepoaccountx
                       WITH HEADER LINE INITIAL SIZE 0,
        yt_t001l       TYPE TABLE OF t001l
                       WITH HEADER LINE INITIAL SIZE 0,
        yt_return      TYPE TABLE OF bapiret2
                       WITH HEADER LINE INITIAL SIZE 0.


  DATA: w_zmmt_po_zgr TYPE zmmt_po_zgr,
        w_poheader    TYPE bapimepoheader,
        w_partner     TYPE bapiekkop,
        w_poheaderx   TYPE bapimepoheaderx,
        w_expheader   TYPE bapimepoheader,
        w_t001l       TYPE t001l.


  DATA: vc_purchaseorder    LIKE bapimepoheader-po_number,
        vc_obj_key          TYPE awkey,
        vn_ebelp            TYPE ebelp,
        vg_erro             TYPE c LENGTH 1,
        vg_no_price_from_po TYPE bapiflag-bapiflag.

  DATA: s_expheader TYPE bapimepoheader.

  CLEAR: s_expheader.

  w_poheader-comp_code   = '0001'.
  w_poheader-doc_type    = 'ZUB'.
  w_poheader-suppl_plnt  = '0124'. "Centro Fornecedor Saida
  w_poheader-purch_org   = 'OC01'.
  w_poheader-pur_group   = 'G01'.
  w_poheader-currency    = 'BRL'.
  w_poheader-creat_date  = sy-datum.


  w_poheaderx-comp_code  = abap_true.
  w_poheaderx-doc_type   = abap_true.
  w_poheaderx-suppl_plnt = abap_true.
  w_poheaderx-purch_org  = abap_true.
  w_poheaderx-pur_group  = abap_true.
  w_poheaderx-currency   = abap_true.
  w_poheaderx-creat_date = abap_true.

  yt_poitem-po_item     = '00010'.
  yt_poitem-material    = '000000000000119892'.
  yt_poitem-plant       = '0155'.
  yt_poitem-stge_loc    = 'ARMZ'.
  yt_poitem-quantity    = '300000'.
  yt_poitem-po_unit     = 'KG'.
  yt_poitem-batch       = '2021'.
  yt_poitem-incoterms1  = 'CIF'.
  yt_poitem-incoterms2  = 'CIF'.
  yt_poitem-conf_ctrl   = space.
  APPEND yt_poitem.

  yt_poitemx-po_item    = yt_poitem-po_item.
  yt_poitemx-po_itemx   =  abap_true.
  yt_poitemx-material   =  abap_true.
  yt_poitemx-plant      =  abap_true.
  yt_poitemx-stge_loc   =  abap_true.
  yt_poitemx-quantity   =  abap_true.
  yt_poitemx-po_unit    =  abap_true.
  yt_poitemx-batch      =  abap_true.
  yt_poitemx-incoterms1 =  abap_true.
  yt_poitemx-incoterms2 =  abap_true.
  yt_poitemx-conf_ctrl  =  abap_true.
  APPEND yt_poitemx.

  yt_partner-partnerdesc = 'PR'.
  yt_partner-langu       = 'P'.
  yt_partner-buspartno   = '0000000124'.
  APPEND yt_partner.

  yt_poship-po_item = yt_poitem-po_item.
  yt_poship-route   = 'SIN2'.
  APPEND yt_poship.

  yt_poshipx-po_item = yt_poitem-po_item.
  yt_poshipx-route   = abap_true.
  APPEND yt_poshipx.


  CALL FUNCTION 'BAPI_PO_CREATE1' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      poheader         = w_poheader
      poheaderx        = w_poheaderx
*     POADDRVENDOR     =
*     TESTRUN          =
*     MEMORY_UNCOMPLETE            =
*     MEMORY_COMPLETE  =
*     POEXPIMPHEADER   =
*     POEXPIMPHEADERX  =
*     VERSIONS         =
*     NO_MESSAGING     =
*     NO_MESSAGE_REQ   =
*     NO_AUTHORITY     =
      no_price_from_po = vg_no_price_from_po
    IMPORTING
*     EXPPURCHASEORDER =
      expheader        = s_expheader
*     EXPPOEXPIMPHEADER            =
    TABLES
      return           = yt_return
      poitem           = yt_poitem
      poitemx          = yt_poitemx
*     POADDRDELIVERY   =
      poschedule       = yt_poschedule
      poschedulex      = yt_poschedulex
      poaccount        = yt_poaccount
*     POACCOUNTPROFITSEGMENT       =
      poaccountx       = yt_poaccountx
*     POCONDHEADER     =
*     POCONDHEADERX    =
*     POCOND           =
*     POCONDX          =
*     POLIMITS         =
*     POCONTRACTLIMITS =
*     POSERVICES       =
*     POSRVACCESSVALUES            =
*     POSERVICESTEXT   =
*     EXTENSIONIN      =
*     EXTENSIONOUT     =
*     POEXPIMPITEM     =
*     POEXPIMPITEMX    =
*     POTEXTHEADER     =
*     POTEXTITEM       =
*     ALLVERSIONS      =
      popartner        = yt_partner
*     POCOMPONENTS     =
*     POCOMPONENTSX    =
      poshipping       = yt_poship
      poshippingx      = yt_poshipx
*     POSHIPPINGEXP    =
    .


  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*   EXPORTING
*     WAIT          =
*   IMPORTING
*     RETURN        =


ENDFORM.

*-CS2021000218-01.09.2022-#89886-JT-inicio
*&---------------------------------------------------------------------*
* testar API AGRIQ
*&---------------------------------------------------------------------*
FORM f_testa_agriq.

  DATA: lob_web_service TYPE REF TO zcl_webservice,
        l_url           TYPE zurlwebadm,
        l_user          TYPE string,
        l_pass          TYPE string,
        l_add1          TYPE string,
        l_add2          TYPE string,
        l_saida         TYPE string.

  CREATE OBJECT lob_web_service.

  l_url  = p_url.
  l_add1 = p_add1.
  l_add2 = p_add2.

  lob_web_service->zif_webservice~ck_usa_auth_webservice = abap_true.
  lob_web_service->zif_webservice~nm_auth_webservice     = 'AGRIQ_TOKEN'.

  TRY.
      DATA(lob_http) = lob_web_service->url( i_url = l_url ).      "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
      DATA(lob_uri)  = lob_web_service->get_uri(  ).
    CATCH zcx_webservice INTO DATA(lc_exception).
      RETURN.
  ENDTRY.

  lob_web_service->zif_webservice~abrir_conexao( i_http = lob_http i_autenticar = abap_true ).

  IF p_user IS INITIAL.
    l_user = lob_web_service->get_usuario( ).
  ELSE.
    l_user = p_user.
  ENDIF.

  IF p_pass IS INITIAL.
    l_pass = lob_web_service->get_senha( ).
  ELSE.
    l_pass = p_pass.
  ENDIF.

  CALL METHOD lob_http->request->set_header_field
    EXPORTING
      name  = 'User'
      value = l_user.

  CALL METHOD lob_http->request->set_header_field
    EXPORTING
      name  = 'Password'
      value = l_pass.

  CALL METHOD lob_http->request->set_header_field
    EXPORTING
      name  = 'Subscription-Key'
      value = l_add1.

  CALL METHOD lob_http->request->set_header_field
    EXPORTING
      name  = 'X-Tenant'
      value = l_add2.

  lob_web_service->zif_webservice~consultar(
    EXPORTING
      i_http                     = lob_http
    IMPORTING
      e_code                     = DATA(lva_code)
      e_reason                   = DATA(lva_reason)
    RECEIVING
      e_resultado                = DATA(lva_json_retorno)
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5
  ).

  DATA(l_str)  = strlen( lva_json_retorno ).
  DATA(l_fim)  = abap_off.
  DATA(l_lin)  = -200.
  DATA(l_lin2) = 0.
  DATA(l_fixo) = 200.

  WRITE:/80 'Retorno TOKEN AGRIQ'.
  SKIP.

  DO.
    l_lin  = l_lin + 200.
    l_lin2 = l_lin + 200.
    IF l_lin2 > l_str.
      l_fixo = l_lin - l_str.
      l_fixo = abs( l_fixo ).
      l_fim  = abap_true.
    ENDIF.

    l_saida = lva_json_retorno+l_lin(l_fixo).
    WRITE:/ l_saida.

    IF l_fim = abap_true.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.
*-CS2021000218-01.09.2022-#89886-JT-fim
