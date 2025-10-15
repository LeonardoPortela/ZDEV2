*&---------------------------------------------------------------------*
*& Report ZFATURAMENTO_CONTIGENCIA_ECC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfaturamento_contigencia_ecc.

TABLES: zsdt0001, zmmt_ee_zgr.

CONSTANTS: c_0(1)  TYPE c                            VALUE '0',
           c_1(1)  TYPE c                            VALUE '1',
           c_01    TYPE bapi2017_gm_code             VALUE '01',
           c_02(2) TYPE c                            VALUE '02',
           c_03(2) TYPE c                            VALUE '03',
           c_04(2) TYPE c                            VALUE '04',
           c_08(2) TYPE c                            VALUE '08',
           c_06(2) TYPE c                            VALUE '06',
           c_07(2) TYPE c                            VALUE '07',
           c_09(2) TYPE c                            VALUE '09',
           c_10    TYPE zfie_ret_document-interface  VALUE '10',
           c_11    TYPE zfie_ret_document-interface  VALUE '11',
           c_12    TYPE zfie_ret_document-interface  VALUE '12',
           c_05    TYPE zfie_ret_document-interface  VALUE '05'.

SELECTION-SCREEN: BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
  PARAMETERS    : p_impro  RADIOBUTTON GROUP g1 USER-COMMAND usr1 DEFAULT 'X',

                  "p_dcarg   RADIOBUTTON GROUP g1,
                  p_eqnum  RADIOBUTTON GROUP g1,
                  p_impee  RADIOBUTTON GROUP g1,
                  p_impft  RADIOBUTTON GROUP g1,
                  p_impod  RADIOBUTTON GROUP g1,
                  p_pcomp  RADIOBUTTON GROUP g1,
                  p_ptran  RADIOBUTTON GROUP g1,
                  p_ckfee  RADIOBUTTON GROUP g1,
                  p_ckcomp RADIOBUTTON GROUP g1,
                  p_cktran RADIOBUTTON GROUP g1,
                  p_ckrom  RADIOBUTTON GROUP g1,
                  p_ckfti  RADIOBUTTON GROUP g1,
                  p_fixrem RADIOBUTTON GROUP g1,
                  p_updata RADIOBUTTON GROUP g1, "Update dados da nota.
                  p_fixmir RADIOBUTTON GROUP g1.

SELECTION-SCREEN: END   OF BLOCK b0.
*
*SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
*PARAMETERS    : p_fdomi  RADIOBUTTON  GROUP g2 USER-COMMAND usr2 DEFAULT 'X',
*                p_fftra  RADIOBUTTON  GROUP g2,
*                p_eqxdxk RADIOBUTTON  GROUP g2,
*                p_vcxdxk RADIOBUTTON  GROUP g2,
*                p_movgc  RADIOBUTTON  GROUP g2,
*                p_eqid4  RADIOBUTTON  GROUP g2.
*
*PARAMETERS  p_restgc AS CHECKBOX.
*SELECTION-SCREEN: END   OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: p_chref    FOR zsdt0001-ch_referencia   MODIF ID t2 NO INTERVALS, " OBLIGATORY,
                  p_branch   FOR zsdt0001-branch          MODIF ID t2 NO INTERVALS, " OBLIGATORY,
                  p_safra    FOR zsdt0001-nr_safra        MODIF ID t2 NO INTERVALS NO-EXTENSION, " OBLIGATORY,
                  p_nrrom    FOR zsdt0001-nr_romaneio     MODIF ID t2, " OBLIGATORY,
                  p_dtmov    FOR zsdt0001-dt_movimento    MODIF ID t2, " OBLIGATORY,
                  p_tpmov    FOR zsdt0001-tp_movimento    MODIF ID t2 NO INTERVALS, " OBLIGATORY,
                  p_stproc   FOR zsdt0001-st_proc         MODIF ID t2 NO INTERVALS  NO-EXTENSION, " OBLIGATORY,
                  p_aviso    FOR zsdt0001-doc_aviso       MODIF ID t2 NO INTERVALS, " OBLIGATORY,
                  p_objee    FOR zmmt_ee_zgr-obj_key      MODIF ID t2 NO INTERVALS. " OBLIGATORY,
SELECTION-SCREEN: END OF BLOCK b3.


SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: p_cknfe  TYPE c AS CHECKBOX,
              p_ckznfw TYPE c AS CHECKBOX,
              p_ckznfa TYPE c AS CHECKBOX,
              p_ckfre  TYPE c AS CHECKBOX,
              p_alv    TYPE c AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b4.


PARAMETERS pobjkeye TYPE zmmt_ee_zgr-obj_key NO-DISPLAY.


START-OF-SELECTION.

  IF pobjkeye IS NOT INITIAL.
    PERFORM z_estorna_miro USING pobjkeye.
    EXIT.
  ELSE.
    PERFORM f_processar.
  ENDIF.


FORM f_processar .

  CASE abap_true.
    WHEN p_impro.
      PERFORM f_importar_romaneios_ecc.
    WHEN p_ckrom. "Check se faturamento entre S4 x ECC esta igual
      PERFORM f_check_faturamento_romaneio.
    WHEN p_ckfti.
      PERFORM f_check_faturamento_inter.
    WHEN p_impft.
      PERFORM f_importar_frete_inter.
      "WHEN p_dcarg. "Desvincular Carguero
      "  PERFORM f_desvinc_rom_carguero.
    WHEN p_eqnum.
      PERFORM f_equalizar_snum.
    WHEN p_impee.
      PERFORM f_importar_ent_estoque.
    WHEN p_pcomp.
      PERFORM f_processa_entrada_est_compras.
    WHEN p_ptran.
      PERFORM f_processa_entrada_est_transf.
    WHEN p_ckcomp.
      PERFORM f_check_entrada_est_compras.
    WHEN p_cktran.
      PERFORM f_check_entrada_est_transf.
    WHEN p_fixrem.
      PERFORM f_fix_remess_rom_ent_estoque.
    WHEN p_impod.
      PERFORM f_importar_ordem_car.
    WHEN p_ckfee.
      PERFORM f_check_forn_ent_estoque.
    WHEN p_updata. "Altarar dados da nota.
      PERFORM f_update_data_nfe.
    WHEN p_fixmir.
      PERFORM f_fix_miro_ee.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM f_importar_romaneios_ecc.

  MESSAGE 'Operação não permitida!' TYPE 'I'.
  RETURN.

  CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0001'.


ENDFORM.

FORM f_desvinc_rom_carguero .

  CHECK p_chref IS NOT INITIAL.

  SELECT SINGLE * FROM zsdt0001 INTO @DATA(lwa_zsdt0001) WHERE ch_referencia IN @p_chref.

  CHECK ( sy-subrc EQ 0 ) AND ( lwa_zsdt0001-id_ordem IS NOT INITIAL ).


  SELECT SINGLE * FROM zlest0185 INTO @DATA(lwa_zlest0185) WHERE id_ordem = @lwa_zsdt0001-id_ordem.

  CHECK sy-subrc EQ 0.

  CLEAR: lwa_zlest0185-id_ordem.

  MODIFY zlest0185 FROM lwa_zlest0185.

ENDFORM.

FORM f_equalizar_snum .

  MESSAGE 'Operação não permitida!' TYPE 'I'.
  RETURN.


  CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0005'.

ENDFORM.

FORM f_importar_ent_estoque .

  MESSAGE 'Operação não permitida!' TYPE 'I'.
  RETURN.

  CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0006'.

ENDFORM.

FORM f_processa_entrada_est_compras.


  UPDATE zmmt_ee_zgr SET zrg_atlz = c_0
   WHERE ( tp_operacao IN (c_01, c_02, c_03, c_04, c_08, c_09, c_10 ) OR tp_operacao EQ space )
     AND fat_contingencia_ecc = abap_true
     AND obj_key IN p_objee.

  MESSAGE 'Registro liberados para processamento' TYPE 'I'.

ENDFORM.

FORM f_processa_entrada_est_transf.

  UPDATE zmmt_ee_zgr SET zrg_atlz = c_0
   WHERE tp_operacao IN (c_05, c_06, c_07)
     AND fat_contingencia_ecc = abap_true
     AND obj_key IN p_objee.

  MESSAGE 'Registro liberados para processamento' TYPE 'I'.

ENDFORM.

FORM f_check_entrada_est_compras.
  DATA: lit_zmmt_ee_zgr  TYPE TABLE OF zmmt_ee_zgr.

  IF p_objee[]  IS NOT INITIAL.
    SELECT * FROM zmmt_ee_zgr INTO TABLE lit_zmmt_ee_zgr
     WHERE ( tp_operacao IN (c_01, c_02, c_03, c_04, c_08, c_09, c_10 ) OR tp_operacao EQ space )
       AND obj_key IN p_objee.
  ELSE.
    SELECT * FROM zmmt_ee_zgr INTO TABLE lit_zmmt_ee_zgr
     WHERE ( tp_operacao IN (c_01, c_02, c_03, c_04, c_08, c_09, c_10 ) OR tp_operacao EQ space )
       AND obj_key IN p_objee
       AND fat_contingencia_ecc = abap_true.
  ENDIF.

  IF lit_zmmt_ee_zgr[] IS INITIAL.
    WRITE: / 'Nenhum registro de estoque encontrado para analise....'.
    RETURN.
  ELSE.
    WRITE: / 'Analisando Entradas Compras com erro na geração do estoque....'.
  ENDIF.

  SELECT *
    FROM zmmt_ee_zgr_docs INTO TABLE @DATA(lit_docs)
    FOR ALL ENTRIES IN @lit_zmmt_ee_zgr
    WHERE obj_key = @lit_zmmt_ee_zgr-obj_key.

  SORT lit_docs BY obj_key.

  DATA(lva_erro) = abap_false.
  LOOP AT lit_zmmt_ee_zgr INTO DATA(lwa_zmmt_zgr).
    READ TABLE lit_docs INTO DATA(lwa_docs) WITH KEY obj_key = lwa_zmmt_zgr-obj_key BINARY SEARCH.
    CHECK sy-subrc NE 0.

    lva_erro = abap_true.

    WRITE: / 'Obj.key com Erro:' && lwa_zmmt_zgr-obj_key.
  ENDLOOP.

  IF lva_erro EQ abap_false.
    WRITE: / 'Nenhum registro com erro de geração de estoque'.
  ENDIF.


ENDFORM.


FORM f_check_entrada_est_transf.

  DATA: lit_zmmt_ee_zgr  TYPE TABLE OF zmmt_ee_zgr.

  IF p_objee[] IS NOT INITIAL.
    SELECT * FROM zmmt_ee_zgr INTO TABLE lit_zmmt_ee_zgr
     WHERE tp_operacao IN (c_05, c_06, c_07)
         AND obj_key IN p_objee.
  ELSE.
    SELECT * FROM zmmt_ee_zgr INTO TABLE lit_zmmt_ee_zgr
     WHERE tp_operacao IN (c_05, c_06, c_07)
         AND obj_key IN p_objee
         AND fat_contingencia_ecc = abap_true.
  ENDIF.

  IF lit_zmmt_ee_zgr[] IS INITIAL.
    WRITE: / 'Nenhum registro de estoque encontrado para analise....'.
    RETURN.
  ELSE.
    WRITE: / 'Analisando Entradas Transferencia com erro na geração do estoque....'.
  ENDIF.

  SELECT *
    FROM zmmt_ee_zgr_docs INTO TABLE @DATA(lit_docs)
    FOR ALL ENTRIES IN @lit_zmmt_ee_zgr
    WHERE obj_key = @lit_zmmt_ee_zgr-obj_key.

  SORT lit_docs BY obj_key.

  DATA(lva_erro) = abap_false.
  LOOP AT lit_zmmt_ee_zgr INTO DATA(lwa_zmmt_zgr).
    READ TABLE lit_docs INTO DATA(lwa_docs) WITH KEY obj_key = lwa_zmmt_zgr-obj_key BINARY SEARCH.
    CHECK sy-subrc NE 0.

    lva_erro = abap_true.

    WRITE: / 'Obj.key com Erro:' && lwa_zmmt_zgr-obj_key.
  ENDLOOP.

  IF lva_erro EQ abap_false.
    WRITE: / 'Nenhum registro com erro de geração de estoque'.
  ENDIF.




ENDFORM.


FORM f_check_faturamento_romaneio.

  DATA: lit_zsdt0001       TYPE TABLE OF zsdt0001.
  DATA: lit_zsdt0001_erro  TYPE TABLE OF zsdt0001.

  DATA: lva_msg_retorno TYPE string,
        lva_ok          TYPE char01.

  CLEAR: lit_zsdt0001_erro[].

  SELECT * FROM zsdt0001 INTO TABLE lit_zsdt0001
   WHERE fat_contingencia_ecc = abap_true
     AND ch_referencia IN p_chref
     AND branch IN p_branch
     AND nr_safra IN p_safra
     AND nr_romaneio IN p_nrrom
     AND dt_movimento IN p_dtmov
     AND tp_movimento IN p_tpmov
     AND st_proc IN p_stproc.


  IF lit_zsdt0001[] IS INITIAL.
    WRITE: / 'Nenhum registro de romaneio encontrado para analise....'.
    RETURN.
  ELSE.
    WRITE: / 'Analisando romaneios com inconsistencia no faturamento....'.
  ENDIF.

  DATA(lva_erro) = abap_false.

  DATA(_check_valor) = abap_true.
  LOOP AT lit_zsdt0001 INTO DATA(lwa_zsdt0001).

    DATA(_not_show_alv) = abap_true.
    IF p_alv EQ abap_true.
      _not_show_alv = abap_false.
    ENDIF.

    DATA(_ckfre)    = p_ckfre.
    DATA(_cknfe)    = p_cknfe.
    DATA(_ckznfw)   = p_ckznfw.
    DATA(_ckznfa)   = p_ckznfa.

    IF p_ckfre IS NOT INITIAL OR
       p_cknfe IS NOT INITIAL OR
       p_ckznfw IS NOT INITIAL OR
       p_ckznfa IS NOT INITIAL.

      IF lwa_zsdt0001-doc_rem IS INITIAL.
        CLEAR: _cknfe.
      ENDIF.

      IF lwa_zsdt0001-seq_lcto IS INITIAL.
        CLEAR: _ckznfw.
      ENDIF.

      IF lwa_zsdt0001-doc_transp IS INITIAL.
        CLEAR: _ckfre.
      ENDIF.

      IF NOT ( lwa_zsdt0001-doc_material IS NOT INITIAL AND lwa_zsdt0001-doc_rem IS INITIAL ).
        CLEAR: _ckznfa.
      ENDIF.

      CHECK _ckznfw IS NOT INITIAL OR
            _ckfre IS NOT INITIAL OR
            _cknfe IS NOT INITIAL OR
            _ckznfa IS NOT INITIAL.

    ENDIF.

    CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
      EXPORTING
        i_ch_referencia        = lwa_zsdt0001-ch_referencia
        i_not_show_divergencia = _not_show_alv
        i_check_frete_ok       = _ckfre
        i_check_danfe_ok       = _cknfe
        i_check_danfe_znfw_ok  = _ckznfw
        i_check_danfe_arm_ok   = _ckznfa
        i_check_valor          = _check_valor
      IMPORTING
        e_ok                   = lva_ok
        e_msg_retorno          = lva_msg_retorno.

    IF lva_ok EQ abap_false.

      APPEND lwa_zsdt0001 TO lit_zsdt0001_erro.

      IF _ckznfw IS NOT INITIAL OR
         _ckfre  IS NOT INITIAL OR
         _cknfe  IS NOT INITIAL OR
         _ckznfa IS NOT INITIAL.
        WRITE: / 'Ch.Referencia com Erro:' && lwa_zsdt0001-ch_referencia.
      ELSE.
        WRITE: / 'Ch.Referencia com Erro:' && lwa_zsdt0001-ch_referencia
                                          && ' - Filial: '  && lwa_zsdt0001-branch
                                          && ' - Romaneio: '   && lwa_zsdt0001-nr_romaneio
                                          && ' - Tp. Movimento: ' && lwa_zsdt0001-tp_movimento .
        ""  && ' CFOP: ' && lva_MSG_RETORNO .

        WRITE : / 'CFOP : ' &&   lva_msg_retorno.
      ENDIF.

      lva_erro = abap_true.
    ENDIF.

  ENDLOOP.

  IF lva_erro EQ abap_false.
    WRITE: / 'Nenhum registro de romaneio com erro no faturamento...'.
  ELSE.
    zcl_utils_abap=>show_data_alv(
      i_data_table     = lit_zsdt0001_erro
      i_structure_name = 'ZSDT0001'
      i_grid_title     = 'Romaneios com diferença no faturamento'
    ).
  ENDIF.

ENDFORM.


FORM f_check_faturamento_inter.

  DATA: lit_zlest0108  TYPE TABLE OF zlest0108.

  DATA: lva_msg_retorno TYPE string,
        lva_ok          TYPE char01.

  SELECT * FROM zlest0108 INTO TABLE lit_zlest0108
   WHERE fat_contingencia_ecc = abap_true
     AND vbeln IN p_aviso.


  IF lit_zlest0108[] IS INITIAL.
    WRITE: / 'Nenhum registro encontrado para analise....'.
    RETURN.
  ELSE.
    WRITE: / 'Analisando registros com inconsistencia no faturamento....'.
  ENDIF.

  DATA(lva_erro) = abap_false.

  DATA(_check_valor) = abap_true.
  LOOP AT lit_zlest0108 INTO DATA(lwa_zlest0108).

    DATA(_not_show_alv) = abap_true.
    IF p_alv EQ abap_true.
      _not_show_alv = abap_false.
    ENDIF.

    CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
      EXPORTING
        i_vbeln                = lwa_zlest0108-vbeln
        i_not_show_divergencia = _not_show_alv
      IMPORTING
        e_ok                   = lva_ok
        e_msg_retorno          = lva_msg_retorno.

    IF lva_ok EQ abap_false.

      WRITE: / 'Aviso com Erro:' && lwa_zlest0108-vbeln.

      lva_erro = abap_true.
    ENDIF.
  ENDLOOP.

  IF lva_erro EQ abap_false.
    WRITE: / 'Nenhum registro com erro no faturamento...'.
  ENDIF.




ENDFORM.

FORM f_fix_remess_rom_ent_estoque.

  TYPES: BEGIN OF ty_zsdt0001,
           ref_doc_no TYPE zmmt_ee_zgr-ref_doc_no.
           INCLUDE STRUCTURE zsdt0001.
  TYPES: END OF ty_zsdt0001.

  DATA: lit_zsdt0001_s TYPE TABLE OF ty_zsdt0001.

  DATA: var_answer TYPE c.

  MESSAGE 'Operação não permitida!' TYPE 'I'.
  RETURN.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente corrigir as remessas?'
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




  "Selecionar Romaneios de Saida para capturar Remessa
  SELECT *
    FROM zsdt0001 INTO CORRESPONDING FIELDS OF TABLE lit_zsdt0001_s
   WHERE fat_contingencia_ecc EQ abap_true.

  DELETE lit_zsdt0001_s WHERE tp_movimento NE 'S'.
  DELETE lit_zsdt0001_s WHERE doc_rem_ecc IS INITIAL.

  LOOP AT lit_zsdt0001_s ASSIGNING FIELD-SYMBOL(<fs_zsdt0001_s>).
    <fs_zsdt0001_s>-ref_doc_no = <fs_zsdt0001_s>-doc_rem_ecc.
  ENDLOOP.

  CHECK lit_zsdt0001_s[] IS NOT INITIAL.

  "Selecionar Entradas de Estoque Transferencia
  SELECT *
    FROM zmmt_ee_zgr INTO TABLE @DATA(lit_zmmt_ee_zgr)
     FOR ALL ENTRIES IN @lit_zsdt0001_s
    WHERE ref_doc_no = @lit_zsdt0001_s-ref_doc_no.

  DELETE lit_zmmt_ee_zgr WHERE tp_operacao NE c_05.

  "Selecionar Romaneios de Entrada vinculado a Remessa de Saida
  SELECT *
    FROM zsdt0001 INTO TABLE @DATA(lit_zsdt0001_e)
     FOR ALL ENTRIES IN @lit_zsdt0001_s
    WHERE doc_rem = @lit_zsdt0001_s-doc_rem_ecc.

  DELETE lit_zsdt0001_e WHERE tp_movimento NE 'E'.

  "Selecionar Notas vinculadas ao Comboio Aquaviario vinculado a Remessa de Saida
  SELECT *
    FROM zlest0060 INTO TABLE @DATA(lit_zlest0060)
     FOR ALL ENTRIES IN @lit_zsdt0001_s
    WHERE doc_rem = @lit_zsdt0001_s-doc_rem_ecc.

  DATA(_registros_atualizados)   = abap_false.

  LOOP AT lit_zsdt0001_s INTO DATA(lwa_zsdt0001_s).

    "Entrada Transf
    READ TABLE lit_zmmt_ee_zgr INTO DATA(lwa_zmmt_ee_zgr) WITH KEY ref_doc_no = lwa_zsdt0001_s-doc_rem_ecc.
    IF sy-subrc EQ 0.
      SELECT SINGLE *
        FROM likp INTO @DATA(lwa_likp)
       WHERE vbeln EQ @lwa_zsdt0001_s-doc_rem_ecc.

      IF ( sy-subrc NE 0 ) AND ( lwa_zsdt0001_s-doc_rem IS NOT INITIAL ) .
        lwa_zmmt_ee_zgr-ref_doc_no = lwa_zsdt0001_s-doc_rem.
        MODIFY zmmt_ee_zgr FROM lwa_zmmt_ee_zgr.
        COMMIT WORK.
        _registros_atualizados = abap_true.
      ENDIF.
    ENDIF.

    "Romaneios de entrada
    READ TABLE lit_zsdt0001_e INTO DATA(lwa_zsdt0001_e) WITH KEY doc_rem = lwa_zsdt0001_s-doc_rem_ecc.
    IF sy-subrc EQ 0.
      SELECT SINGLE *
        FROM likp INTO lwa_likp
       WHERE vbeln EQ lwa_zsdt0001_s-doc_rem_ecc.

      IF ( sy-subrc NE 0 ) AND ( lwa_zsdt0001_s-doc_rem IS NOT INITIAL ) .
        lwa_zsdt0001_e-doc_rem = lwa_zsdt0001_s-doc_rem.
        MODIFY zsdt0001 FROM lwa_zsdt0001_e.
        COMMIT WORK.
        _registros_atualizados = abap_true.
      ENDIF.
    ENDIF.

    "Notas aquaviario
    READ TABLE lit_zlest0060 INTO DATA(lwa_zlest0060) WITH KEY doc_rem = lwa_zsdt0001_s-doc_rem_ecc.
    IF sy-subrc EQ 0.
      SELECT SINGLE *
        FROM likp INTO lwa_likp
       WHERE vbeln EQ lwa_zsdt0001_s-doc_rem_ecc.

      IF ( sy-subrc NE 0 ) AND ( lwa_zsdt0001_s-doc_rem IS NOT INITIAL ) .
        lwa_zlest0060-doc_rem = lwa_zsdt0001_s-doc_rem.
        MODIFY zlest0060 FROM lwa_zlest0060.
        COMMIT WORK.
        _registros_atualizados = abap_true.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF _registros_atualizados = abap_true.
    MESSAGE 'Registros foram atualizados!' TYPE 'I'.
  ELSE.
    MESSAGE 'Nenhum registro foi atualizado!' TYPE 'I'.
  ENDIF.



ENDFORM.

FORM f_importar_frete_inter .

  MESSAGE 'Operação não permitida!' TYPE 'I'.
  RETURN.

  CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0008'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_importar_ordem_car
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_importar_ordem_car .

  DATA: lva_destination TYPE char40.


  MESSAGE 'Operação não permitida!' TYPE 'I'.
  RETURN.

  DATA: lit_zsdt0001od_ecc TYPE TABLE OF zsdt0001od_tmp,
        lit_zlest0185_ecc  TYPE TABLE OF zlest0185,

        lit_zsdt0001od_s4  TYPE TABLE OF zsdt0001od,
        lit_zlest0185_s4   TYPE TABLE OF zlest0185.

  CASE sy-sysid.
    WHEN 'DEV'.
      lva_destination =  'DEV_ECC'.
    WHEN 'QAS'.
      lva_destination =  'QAS_ECC'.
    WHEN 'PRD'.
      lva_destination =  'PRD_ECC'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  CHECK lva_destination IS NOT INITIAL.

  "Função que Le o XML no GRC
  CALL FUNCTION 'ZLES_FATURAMENTO_CONTINGENCIA' DESTINATION lva_destination
    EXPORTING
      i_operacao       = '08'
    TABLES
      t_out_zsdt0001od = lit_zsdt0001od_ecc
      t_out_zlest0185  = lit_zlest0185_ecc.

  IF lit_zsdt0001od_ecc[] IS INITIAL OR lit_zlest0185_ecc[] IS INITIAL.
    MESSAGE 'Registro nao encontrado complemetamento para importação!' TYPE 'I'.
    RETURN.
  ENDIF.

  SELECT *
     FROM zsdt0001od INTO TABLE lit_zsdt0001od_s4
    FOR ALL ENTRIES IN lit_zsdt0001od_ecc
    WHERE id_ordem = lit_zsdt0001od_ecc-id_ordem.

  SELECT *
     FROM zlest0185 INTO TABLE lit_zlest0185_s4
    FOR ALL ENTRIES IN lit_zlest0185_ecc
    WHERE viagem_id = lit_zlest0185_ecc-viagem_id.


  SORT lit_zsdt0001od_s4  BY id_ordem.
  SORT lit_zsdt0001od_ecc BY id_ordem.

  SORT lit_zlest0185_s4   BY viagem_id.
  SORT lit_zlest0185_ecc  BY viagem_id.

  DATA(_registros_importados) = abap_false.
  LOOP AT lit_zsdt0001od_ecc INTO DATA(lwa_zsdt0001od_ecc).

    READ TABLE lit_zsdt0001od_s4 INTO DATA(lwa_zsdt0001od_s4) WITH KEY  id_ordem = lwa_zsdt0001od_ecc-id_ordem BINARY SEARCH.

    CHECK sy-subrc NE 0.

    CLEAR: lwa_zsdt0001od_s4.

    MOVE-CORRESPONDING lwa_zsdt0001od_ecc TO lwa_zsdt0001od_s4.

    lwa_zsdt0001od_s4-fat_contingencia_ecc = abap_true.

    MODIFY zsdt0001od FROM lwa_zsdt0001od_s4.
    COMMIT WORK AND WAIT.

    _registros_importados = abap_true.
  ENDLOOP.

  IF _registros_importados EQ abap_true.
    MESSAGE 'Ordem Carregamento importadas com sucesso!' TYPE 'I'.
  ELSE.
    MESSAGE 'Todas Ordem Carregamento ja foram importadas!' TYPE 'I'.
  ENDIF.

  _registros_importados = abap_false.
  LOOP AT lit_zlest0185_ecc INTO DATA(lwa_zlest0185_ecc).

    READ TABLE lit_zlest0185_s4 INTO DATA(lwa_zlest0185_s4) WITH KEY  viagem_id = lwa_zlest0185_ecc-viagem_id BINARY SEARCH.

    CHECK sy-subrc NE 0.

    CLEAR: lwa_zlest0185_s4.

    MOVE-CORRESPONDING lwa_zlest0185_ecc TO lwa_zlest0185_s4.

    lwa_zlest0185_s4-fat_contingencia_ecc = abap_true.

    MODIFY zlest0185 FROM lwa_zlest0185_s4.
    COMMIT WORK AND WAIT.

    _registros_importados = abap_true.
  ENDLOOP.

  IF _registros_importados EQ abap_true.
    MESSAGE 'Viagens importadas com sucesso!' TYPE 'I'.
  ELSE.
    MESSAGE 'Todas Viagens ja foram importadas!' TYPE 'I'.
  ENDIF.

ENDFORM.

FORM f_check_forn_ent_estoque.

  DATA: lva_destination TYPE char40.

  "RANGES: lra_object FOR nriv-object.

  DATA: lit_zmmt_ee_zgr_ecc TYPE TABLE OF zmmt_ee_zgr_tmp,
        lit_zmmt_ee_zgr_s4  TYPE TABLE OF zmmt_ee_zgr.

  CASE sy-sysid.
    WHEN 'DEV'.
      lva_destination =  'DEV_ECC'.
    WHEN 'QAS'.
      lva_destination =  'QAS_ECC'.
    WHEN 'PRD'.
      lva_destination =  'PRD_ECC'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  CHECK lva_destination IS NOT INITIAL.

  "Função que Le o XML no GRC
  CALL FUNCTION 'ZLES_FATURAMENTO_CONTINGENCIA' DESTINATION lva_destination
    EXPORTING
      i_operacao        = '09'
    TABLES
      t_out_zmmt_ee_zgr = lit_zmmt_ee_zgr_ecc.


  DELETE lit_zmmt_ee_zgr_ecc WHERE po_number IS INITIAL.

  DELETE lit_zmmt_ee_zgr_ecc WHERE obj_key NOT IN p_objee.

  IF lit_zmmt_ee_zgr_ecc[] IS INITIAL.
    MESSAGE 'Nenhuma entrada estoque encontrada' TYPE 'I'.
    RETURN.
  ENDIF.


  WRITE: / 'Analisando entradas estoque com inconsistencia no fornecedor pedido....'.

  SELECT *
     FROM zmmt_ee_zgr INTO TABLE lit_zmmt_ee_zgr_s4
    FOR ALL ENTRIES IN lit_zmmt_ee_zgr_ecc
    WHERE obj_key = lit_zmmt_ee_zgr_ecc-obj_key.



  DATA(lit_zmmt_ee_zgr_aux) = lit_zmmt_ee_zgr_s4[].
  SORT lit_zmmt_ee_zgr_aux BY po_number.
  DELETE ADJACENT DUPLICATES FROM lit_zmmt_ee_zgr_aux COMPARING po_number.

  SELECT *
    FROM ekko INTO TABLE @DATA(lit_ekko)
     FOR ALL ENTRIES IN @lit_zmmt_ee_zgr_aux
  WHERE ebeln = @lit_zmmt_ee_zgr_aux-po_number.

  LOOP AT lit_zmmt_ee_zgr_s4 ASSIGNING FIELD-SYMBOL(<fs_zmmt>).
    READ TABLE lit_ekko INTO DATA(lwa_ekko) WITH KEY ebeln = <fs_zmmt>-po_number.
    IF sy-subrc EQ 0.
      <fs_zmmt>-lifnr = lwa_ekko-lifnr.
    ELSE.
      CLEAR: <fs_zmmt>-lifnr.
    ENDIF.
  ENDLOOP.

  SORT lit_zmmt_ee_zgr_s4  BY obj_key.
  SORT lit_zmmt_ee_zgr_ecc BY obj_key.

  DATA(_registros_importados) = abap_false.
  LOOP AT lit_zmmt_ee_zgr_ecc INTO DATA(lwa_zmmt_ee_zgr_ecc).

    READ TABLE lit_zmmt_ee_zgr_s4 INTO DATA(lwa_zmmt_ee_zgr_s4) WITH KEY  obj_key = lwa_zmmt_ee_zgr_ecc-obj_key BINARY SEARCH.

    CHECK sy-subrc EQ 0 AND lwa_zmmt_ee_zgr_s4-lifnr <> lwa_zmmt_ee_zgr_ecc-lifnr.

    WRITE: / 'Ch.Referencia com Erro:' && lwa_zmmt_ee_zgr_ecc-obj_key
                                       && ' - Fornecedor/Pedido ECC: '  && lwa_zmmt_ee_zgr_ecc-lifnr && ' / ' && lwa_zmmt_ee_zgr_ecc-po_number
                                       && ' - Fornecedor/Pedido S4: '   && lwa_zmmt_ee_zgr_s4-lifnr  && ' / ' && lwa_zmmt_ee_zgr_s4-po_number.


  ENDLOOP.

  WRITE: / 'Processamento concluido...' .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_data_nfe
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_update_data_nfe .

*&---------------------------------------------------------------  .
*& Inicio equalização valores da NFes ambiente SAP ECC / SAP S4/HANA/ AOENNING.
*&---------------------------------------------------------------
  TYPES:
    BEGIN OF ty_log,
      status        TYPE char05,
      docnum        TYPE j_1bnflin-docnum,
      tip_impos     TYPE char10,
      zcheck_excbas TYPE char01,
      zcheck_base   TYPE char01,
      zcheck_rate   TYPE char01,
      zcheck_taxval TYPE char01,
    END OF ty_log.

  DATA: var_answer TYPE c.

  DATA: vg_ok                    TYPE  char01,
        vg_msg_retorno           TYPE  string,
        tg_j_1bnflin             TYPE TABLE OF j_1bnflin,
        tg_j_1bnfstx             TYPE TABLE OF j_1bnfstx,
        tg_j_1bnfdoc             TYPE TABLE OF j_1bnfdoc,
        tg_log                   TYPE TABLE OF ty_log,
        fs_log                   TYPE  ty_log,
        fs_j_1bnflin             TYPE j_1bnflin,
        fs_j_1bnfstx             TYPE j_1bnfstx,
        fs_j_1bnfdoc             TYPE j_1bnfdoc,
        lva_impostos_nfe         TYPE string,
        t_j_1bnfstx_ecc          TYPE TABLE OF j_1bnfstx,
        fs_dados_faturamento_ecc TYPE  zde_compare_faturamento.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente corrigir os documentos?'
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

  FREE: tg_j_1bnflin, tg_j_1bnfstx, tg_j_1bnfdoc.

*&---------------------------------------------------------------  .
*& 1º - Verefica dados na tabela de romaneio
*&---------------------------------------------------------------
  SELECT ch_referencia, nro_nf_prod FROM zsdt0001
  INTO TABLE @DATA(tg_zsdt0001)
    WHERE ch_referencia IN @p_chref.
  IF sy-subrc EQ 0.

*&---------------------------------------------------------------  .
*& 2º - Seleciona dados do SAP ECC
*&---------------------------------------------------------------

    LOOP AT tg_zsdt0001 ASSIGNING FIELD-SYMBOL(<fs_zsdt0001>).
      CLEAR: vg_ok, vg_msg_retorno, fs_dados_faturamento_ecc, fs_j_1bnflin, fs_j_1bnfstx, t_j_1bnfstx_ecc, lva_impostos_nfe.

      CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
        EXPORTING
          i_ch_referencia         = <fs_zsdt0001>-ch_referencia
          i_get_dados_fat_ecc     = abap_true
        IMPORTING
          e_ok                    = vg_ok
          e_msg_retorno           = vg_msg_retorno
          e_impostos_nfe          = lva_impostos_nfe
          e_dados_faturamento_ecc = fs_dados_faturamento_ecc.

      IF fs_dados_faturamento_ecc IS INITIAL.
        CONTINUE.
      ENDIF.

      "seleciona dados cabeçalho da nota fiscal j_1bnfdoc.
      SELECT SINGLE * FROM j_1bnfdoc INTO fs_j_1bnfdoc
      WHERE docnum EQ <fs_zsdt0001>-nro_nf_prod.
      IF sy-subrc EQ 0.
        IF fs_j_1bnfdoc-nftot NE fs_dados_faturamento_ecc-netwr_nf.
          fs_j_1bnfdoc-nftot = fs_dados_faturamento_ecc-netwr_nf.

          APPEND fs_j_1bnfdoc TO tg_j_1bnfdoc.
        ENDIF.
      ENDIF.

      "seleciona dados da nota fiscal j_1bnflin.
      SELECT SINGLE * FROM j_1bnflin INTO fs_j_1bnflin
      WHERE docnum EQ <fs_zsdt0001>-nro_nf_prod.
      IF fs_j_1bnflin IS NOT INITIAL.
*&---------------------------------------------------------------  .
*& 3º - Check valor da nfe, se estiver diferente do valor ambiente ( SAP ECC ), fazer o ajuste no valor.
*&---------------------------------------------------------------

        IF fs_j_1bnflin-netwr NE fs_dados_faturamento_ecc-netwr_nf.
          "Ajustar o valor.
          fs_j_1bnflin-netwr  = fs_dados_faturamento_ecc-netwr_nf.
          fs_j_1bnflin-nfnet  = fs_dados_faturamento_ecc-netwr_nf.
          fs_j_1bnflin-netwrt = fs_dados_faturamento_ecc-netwr_nf.
          fs_j_1bnflin-nfnett = fs_dados_faturamento_ecc-netwr_nf.

          "Calcular o valor unt com base na quantidade.
          IF fs_j_1bnflin-menge IS NOT INITIAL.
            fs_j_1bnflin-netpr = ( fs_dados_faturamento_ecc-netwr_nf / fs_j_1bnflin-menge ).
            fs_j_1bnflin-nfpri = fs_j_1bnflin-netpr.
          ENDIF.

          APPEND fs_j_1bnflin TO tg_j_1bnflin.

          fs_log-status = 'P'. "Processado
          fs_log-docnum = <fs_zsdt0001>-nro_nf_prod.
          APPEND fs_log TO tg_log.
        ENDIF.
      ELSE.
        CONTINUE.
      ENDIF.

      /ui2/cl_json=>deserialize( EXPORTING json = lva_impostos_nfe CHANGING data = t_j_1bnfstx_ecc ).

      "seleciona dados imposto da nota nota fiscal.
      SELECT * FROM j_1bnfstx INTO TABLE  @DATA(it_j_1bnfstx)
      WHERE docnum EQ @<fs_zsdt0001>-nro_nf_prod.

      IF t_j_1bnfstx_ecc[] NE it_j_1bnfstx[].

        DELETE from j_1bnfstx WHERE docnum EQ <fs_zsdt0001>-nro_nf_prod.

*  &---------------------------------------------------------------  .
*  & 4º - Atualizar valor do imposto tabela j_1bnfstx.
*  &---------------------------------------------------------------

        LOOP AT t_j_1bnfstx_ecc ASSIGNING FIELD-SYMBOL(<fs_j1bnfstx_s4>).
          <fs_j1bnfstx_s4>-docnum = <fs_zsdt0001>-nro_nf_prod.
          APPEND <fs_j1bnfstx_s4> TO tg_j_1bnfstx.
        ENDLOOP.

        FREE: it_j_1bnfstx.
      ENDIF.

    ENDLOOP.

    IF tg_j_1bnflin IS NOT INITIAL.
      MODIFY j_1bnflin FROM TABLE tg_j_1bnflin.
    ENDIF.

    IF tg_j_1bnfstx IS NOT INITIAL.
      MODIFY j_1bnfstx FROM TABLE tg_j_1bnfstx.
    ENDIF.

    IF tg_j_1bnfdoc IS NOT INITIAL.
      MODIFY j_1bnfdoc FROM TABLE tg_j_1bnfdoc.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.


    SORT tg_log BY docnum.

    LOOP AT tg_log INTO DATA(ws_log).
      IF ws_log-status EQ 'P'.
        WRITE: / 'Valor do docnum ":' && ws_log-docnum && ' - Alterado com sucesso'.
      ENDIF.

      IF ws_log-tip_impos IS NOT INITIAL.
        WRITE: / 'Docnum com imposto preenchdio diferente "Outro Imposto":' && ws_log-docnum && ' - Tipo imposto: '  && ws_log-tip_impos.
      ENDIF.
    ENDLOOP.


    WRITE: / 'Processamento concluido...' .
    IF tg_log IS INITIAL.
      WRITE: / 'Dados não encontrado para alteração...' .
    ENDIF.
  ELSE.
    MESSAGE 'Dados não encontrado!' TYPE 'I'.
    RETURN.
  ENDIF.

  FREE: tg_zsdt0001.



*&---------------------------------------------------------------  .
*& Fim equalização valores da NFes ambiente SAP ECC / SAP S4/HANA/ AOENNING.
*&---------------------------------------------------------------
ENDFORM.

FORM f_fix_miro_ee .

  DATA: number           TYPE tbtcjob-jobcount,
        name             TYPE tbtcjob-jobname,
        print_parameters TYPE pri_params.


  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_contingencia)
   WHERE name = 'ZFAT_CONT_US_FIX_MIRO'
    AND low = @sy-uname.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_contingencia_tp)
   WHERE name = 'ZFAT_CONT_TIPO_FIX_MIRO'.

  IF p_objee[] IS NOT INITIAL.
    SELECT *
      FROM zmmt_ee_zgr INTO TABLE @DATA(lit_zmmt_ee)
     WHERE obj_key IN @p_objee.
  ENDIF.

  DELETE lit_zmmt_ee WHERE comp_code EQ '0035'.

  CHECK lit_zmmt_ee[] IS NOT INITIAL.


  CASE lwa_contingencia_tp-low.
    WHEN '1'. "Estornar Miro

      LOOP AT lit_zmmt_ee INTO DATA(lwa_zmmt_ee).

        SELECT SINGLE *
          FROM zmmt_ee_zgr_docs  INTO @DATA(lwa_docs)
         WHERE obj_key = @lwa_zmmt_ee-obj_key.

        CHECK sy-subrc EQ 0 AND lwa_docs-ft_belnr IS NOT INITIAL.

        DATA(lc_user_job) = zcl_job=>get_user_job( ).

        CLEAR: number.

        CONCATENATE 'ZFAT_CONT_MIRO_E' lwa_zmmt_ee-obj_key INTO name SEPARATED BY '_'.

        CALL FUNCTION 'JOB_OPEN'
          EXPORTING
            jobname          = name
          IMPORTING
            jobcount         = number
          EXCEPTIONS
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            OTHERS           = 4.

        IF sy-subrc IS INITIAL.

          IF 1 = 2.
            SUBMIT zfaturamento_contigencia_ecc
              WITH pobjkeye EQ lwa_zmmt_ee-obj_key
               AND RETURN.
          ENDIF.
          SUBMIT zfaturamento_contigencia_ecc TO SAP-SPOOL SPOOL PARAMETERS print_parameters WITHOUT SPOOL DYNPRO VIA JOB name NUMBER number
            WITH pobjkeye EQ lwa_zmmt_ee-obj_key
            USER lc_user_job
             AND RETURN.

          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'JOB_CLOSE'
              EXPORTING
                jobcount             = number
                jobname              = name
                strtimmed            = 'X'
              EXCEPTIONS
                cant_start_immediate = 1
                invalid_startdate    = 2
                jobname_missing      = 3
                job_close_failed     = 4
                job_nosteps          = 5
                job_notex            = 6
                lock_failed          = 7
                OTHERS               = 8.

            IF sy-subrc IS NOT INITIAL.
              DATA(ck_erro) = abap_true.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno INTO DATA(mtext) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              CALL FUNCTION 'BP_JOB_DELETE'
                EXPORTING
                  jobcount                 = number
                  jobname                  = name
                EXCEPTIONS
                  cant_delete_event_entry  = 1
                  cant_delete_job          = 2
                  cant_delete_joblog       = 3
                  cant_delete_steps        = 4
                  cant_delete_time_entry   = 5
                  cant_derelease_successor = 6
                  cant_enq_predecessor     = 7
                  cant_enq_successor       = 8
                  cant_enq_tbtco_entry     = 9
                  cant_update_predecessor  = 10
                  cant_update_successor    = 11
                  commit_failed            = 12
                  jobcount_missing         = 13
                  jobname_missing          = 14
                  job_does_not_exist       = 15
                  job_is_already_running   = 16
                  no_delete_authority      = 17
                  OTHERS                   = 18.
              IF sy-subrc IS NOT INITIAL.
                ck_erro = abap_false.
              ENDIF.
            ENDIF.
          ELSE.
            ck_erro = abap_true.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            CALL FUNCTION 'BP_JOB_DELETE'
              EXPORTING
                jobcount                 = number
                jobname                  = name
              EXCEPTIONS
                cant_delete_event_entry  = 1
                cant_delete_job          = 2
                cant_delete_joblog       = 3
                cant_delete_steps        = 4
                cant_delete_time_entry   = 5
                cant_derelease_successor = 6
                cant_enq_predecessor     = 7
                cant_enq_successor       = 8
                cant_enq_tbtco_entry     = 9
                cant_update_predecessor  = 10
                cant_update_successor    = 11
                commit_failed            = 12
                jobcount_missing         = 13
                jobname_missing          = 14
                job_does_not_exist       = 15
                job_is_already_running   = 16
                no_delete_authority      = 17
                OTHERS                   = 18.
            IF sy-subrc IS NOT INITIAL.
              ck_erro = abap_false.
            ENDIF.
          ENDIF.
        ENDIF.

        "Aguardar execução do job
        zcl_job=>get_instance(
         )->set_key_job( i_jobname = name i_jobcount = number
         )->get_wait_job_exec(
         ).

      ENDLOOP.

    WHEN '2'. "Gerar Miro

      LOOP AT lit_zmmt_ee INTO lwa_zmmt_ee.

        SELECT SINGLE *
          FROM zmmt_ee_zgr_docs  INTO lwa_docs
         WHERE obj_key = lwa_zmmt_ee-obj_key.

        CHECK sy-subrc EQ 0 AND lwa_docs-ft_belnr IS INITIAL.

        lc_user_job = zcl_job=>get_user_job( ).

        CLEAR: number.

        CONCATENATE 'ZFAT_CONT_MIRO_G' lwa_zmmt_ee-obj_key INTO name SEPARATED BY '_'.

        CALL FUNCTION 'JOB_OPEN'
          EXPORTING
            jobname          = name
          IMPORTING
            jobcount         = number
          EXCEPTIONS
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            OTHERS           = 4.

        IF sy-subrc IS INITIAL.
          SUBMIT zmmr019_force_miro TO SAP-SPOOL SPOOL PARAMETERS print_parameters WITHOUT SPOOL DYNPRO VIA JOB name NUMBER number
            WITH pobjkey EQ lwa_zmmt_ee-obj_key
            USER lc_user_job
             AND RETURN.

          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'JOB_CLOSE'
              EXPORTING
                jobcount             = number
                jobname              = name
                strtimmed            = 'X'
              EXCEPTIONS
                cant_start_immediate = 1
                invalid_startdate    = 2
                jobname_missing      = 3
                job_close_failed     = 4
                job_nosteps          = 5
                job_notex            = 6
                lock_failed          = 7
                OTHERS               = 8.

            IF sy-subrc IS NOT INITIAL.
              ck_erro = abap_true.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              CALL FUNCTION 'BP_JOB_DELETE'
                EXPORTING
                  jobcount                 = number
                  jobname                  = name
                EXCEPTIONS
                  cant_delete_event_entry  = 1
                  cant_delete_job          = 2
                  cant_delete_joblog       = 3
                  cant_delete_steps        = 4
                  cant_delete_time_entry   = 5
                  cant_derelease_successor = 6
                  cant_enq_predecessor     = 7
                  cant_enq_successor       = 8
                  cant_enq_tbtco_entry     = 9
                  cant_update_predecessor  = 10
                  cant_update_successor    = 11
                  commit_failed            = 12
                  jobcount_missing         = 13
                  jobname_missing          = 14
                  job_does_not_exist       = 15
                  job_is_already_running   = 16
                  no_delete_authority      = 17
                  OTHERS                   = 18.
              IF sy-subrc IS NOT INITIAL.
                ck_erro = abap_false.
              ENDIF.
            ENDIF.
          ELSE.
            ck_erro = abap_true.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            CALL FUNCTION 'BP_JOB_DELETE'
              EXPORTING
                jobcount                 = number
                jobname                  = name
              EXCEPTIONS
                cant_delete_event_entry  = 1
                cant_delete_job          = 2
                cant_delete_joblog       = 3
                cant_delete_steps        = 4
                cant_delete_time_entry   = 5
                cant_derelease_successor = 6
                cant_enq_predecessor     = 7
                cant_enq_successor       = 8
                cant_enq_tbtco_entry     = 9
                cant_update_predecessor  = 10
                cant_update_successor    = 11
                commit_failed            = 12
                jobcount_missing         = 13
                jobname_missing          = 14
                job_does_not_exist       = 15
                job_is_already_running   = 16
                no_delete_authority      = 17
                OTHERS                   = 18.
            IF sy-subrc IS NOT INITIAL.
              ck_erro = abap_false.
            ENDIF.
          ENDIF.
        ENDIF.

        "Aguardar execução do job
        zcl_job=>get_instance(
         )->set_key_job( i_jobname = name i_jobcount = number
         )->get_wait_job_exec(
         ).

      ENDLOOP.

  ENDCASE.




ENDFORM.

FORM z_estorna_miro USING p_objkey     TYPE zmmt_ee_zgr-obj_key.


  DATA: it_return           TYPE TABLE OF bapiret2.

  DATA: vg_message TYPE string.
  DATA: vl_erro TYPE c.

  DATA: vg_invoicedocnumber_miro TYPE bapi_incinv_fld-inv_doc_no.
  DATA: wa_invoicedocnumber TYPE bapi_incinv_fld.
  DATA: wa_pstng_date       TYPE bapi2017_gm_head_02-pstng_date.

  DATA: vl_message_v1 TYPE symsgv,
        vl_message_v2 TYPE symsgv,
        vl_message_v3 TYPE symsgv,
        vl_werks      TYPE t001w-j_1bbranch,
        vg_refkey	    TYPE j_1brefkey,
        vg_awkey      TYPE awkey,
        wa_j_1bnflin  TYPE j_1bnflin,
        wa_j_1bnfdoc  TYPE j_1bnfdoc,
        p_data_ent    TYPE datum,
        p_data_val    TYPE datum,
        wa_rbkp       TYPE rbkp,
        wa_bkpf       TYPE bkpf,
        it_bsak       TYPE TABLE OF bsak WITH HEADER LINE,
        it_notas      TYPE TABLE OF zib_nota_fiscal_sap WITH HEADER LINE,
        val_fi        TYPE char1,
        val_mm        TYPE char1.

  CLEAR: wa_invoicedocnumber, wa_pstng_date,vg_invoicedocnumber_miro.

  SELECT SINGLE *
    FROM zmmt_ee_zgr_docs INTO @DATA(lwa_docs)
   WHERE obj_key EQ @p_objkey.

  CHECK sy-subrc EQ 0 AND lwa_docs-ft_belnr IS NOT INITIAL AND lwa_docs-ft_gjahr IS NOT INITIAL.

  CONCATENATE lwa_docs-ft_belnr lwa_docs-ft_gjahr INTO vg_refkey.
  CONCATENATE lwa_docs-ft_belnr lwa_docs-ft_gjahr INTO vg_awkey.

  SELECT SINGLE * INTO wa_rbkp
    FROM rbkp
   WHERE belnr EQ lwa_docs-ft_belnr
     AND gjahr EQ lwa_docs-ft_gjahr.

  CHECK sy-subrc IS INITIAL.

  IF wa_rbkp-stblg IS NOT INITIAL.
    CLEAR: lwa_docs-ft_belnr, lwa_docs-ft_gjahr.
    MODIFY zmmt_ee_zgr_docs FROM lwa_docs.
    EXIT.
  ENDIF.

  SELECT SINGLE * INTO wa_bkpf
    FROM bkpf
   WHERE bukrs = wa_rbkp-bukrs
     AND gjahr = wa_rbkp-gjahr
     AND awtyp = 'RMRP'
     AND awkey = vg_awkey.

  IF sy-subrc IS INITIAL.

    SELECT * INTO TABLE it_bsak
      FROM bsak
     WHERE gjahr EQ wa_bkpf-gjahr
       AND bukrs EQ wa_bkpf-bukrs
       AND belnr EQ wa_bkpf-belnr.

    IF sy-subrc IS INITIAL.
      vl_erro   = 'X'.
      WRITE: / 'Documento' && lwa_docs-ft_belnr && lwa_docs-ft_gjahr && 'compensando!'.
    ENDIF.
  ENDIF.

  wa_invoicedocnumber-inv_doc_no = lwa_docs-ft_belnr.
  wa_invoicedocnumber-fisc_year  = lwa_docs-ft_gjahr.
  wa_invoicedocnumber-reason_rev = 'Z1'.

  wa_pstng_date = wa_rbkp-budat.

*    SELECT SINGLE j_1bbranch
*      INTO vl_werks
*      FROM t001w
*     WHERE werks EQ doc_item-werks.
*
*    SET PARAMETER ID 'ZWERKS' FIELD vl_werks.

  DATA(ck_continuar) = abap_true.

  WHILE ck_continuar EQ abap_true.

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
      EXPORTING
        invoicedocnumber          = wa_invoicedocnumber-inv_doc_no
        fiscalyear                = wa_invoicedocnumber-fisc_year
        reasonreversal            = wa_invoicedocnumber-reason_rev
        postingdate               = wa_pstng_date
      IMPORTING
        invoicedocnumber_reversal = vg_invoicedocnumber_miro
      TABLES
        return                    = it_return.

    READ TABLE it_return INTO DATA(wa_return) WITH KEY type = 'E'.
    IF sy-subrc EQ 0.

      DATA(_bloq) = abap_false.
      PERFORM f_check_msg_bloq TABLES it_return CHANGING _bloq wa_return.
      IF _bloq EQ abap_false.
        ck_continuar = abap_false.
      ELSE.
        MESSAGE ID wa_return-id TYPE 'S' NUMBER wa_return-number WITH wa_return-message_v1 wa_return-message_v2 wa_return-message_v3 wa_return-message_v4
        DISPLAY LIKE 'E'.

        WAIT UP TO 1 SECONDS.
        CLEAR: it_return[].
      ENDIF.

    ELSE.
      ck_continuar = abap_false.
    ENDIF.

  ENDWHILE.

  " SET PARAMETER ID 'ZWERKS' FIELD ''.


  IF it_return[] IS NOT INITIAL.
    READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      vl_erro = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      PERFORM limpa_ref_miro_fiscal IN PROGRAM zmmr020_01 IF FOUND
        USING wa_invoicedocnumber-inv_doc_no wa_invoicedocnumber-fisc_year.

    ENDIF.
  ELSE.
    CLEAR : vl_erro.
    CLEAR wa_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    PERFORM limpa_ref_miro_fiscal IN PROGRAM zmmr020_01 IF FOUND
        USING wa_invoicedocnumber-inv_doc_no wa_invoicedocnumber-fisc_year.
  ENDIF.

  WAIT UP TO 2 SECONDS.

  SELECT SINGLE * INTO @DATA(wa_rbkp_check)
    FROM rbkp
   WHERE belnr EQ @lwa_docs-ft_belnr
     AND gjahr EQ @lwa_docs-ft_gjahr.

  IF sy-subrc EQ 0 AND wa_rbkp_check-stblg IS NOT INITIAL.
    CLEAR: lwa_docs-ft_belnr, lwa_docs-ft_gjahr.
    MODIFY zmmt_ee_zgr_docs FROM lwa_docs.
    EXIT.
  ENDIF.




ENDFORM.


FORM limpa_ref_miro_fiscal
    USING p_inv_doc_no TYPE re_belnr
        p_fisc_year  TYPE gjahr.

  CALL FUNCTION 'ZGRC_LIMPA_REF_MIRO_FISCAL'
    EXPORTING
      invoicedocnumber = p_inv_doc_no
      fiscalyear       = p_fisc_year.

ENDFORM.


FORM f_check_msg_bloq TABLES p_return STRUCTURE bapiret2
                    CHANGING p_bloq   TYPE c
                             _wl_return TYPE bapiret2.

  DATA: lc_wait_int_m3_897 TYPE i.

  p_bloq = abap_false.

  READ TABLE p_return INTO _wl_return WITH KEY type = 'E' id = 'ME' number = '006'.
  IF sy-subrc EQ 0.
    p_bloq = abap_true.
    EXIT.
  ENDIF.

  READ TABLE p_return INTO _wl_return WITH KEY type = 'E' id = 'M3' number = '024'.
  IF sy-subrc EQ 0.
    p_bloq = abap_true.
    EXIT.
  ENDIF.

  READ TABLE p_return INTO _wl_return WITH KEY type = 'E' id = 'M3' number = '682'.
  IF sy-subrc EQ 0.
    p_bloq = abap_true.
    EXIT.
  ENDIF.

  READ TABLE p_return INTO _wl_return WITH KEY type = 'E' id = 'M3' number = '897'.
  IF sy-subrc EQ 0.

    SELECT SINGLE *
      FROM setleaf INTO @DATA(_wl_set_interval_m3_897)
     WHERE setname = 'ENT_EST_INTERVAL_M3_897'.

    IF ( sy-subrc EQ 0 ) AND ( _wl_set_interval_m3_897-valfrom > 0 ).
      lc_wait_int_m3_897 = _wl_set_interval_m3_897-valfrom.
      WAIT UP TO lc_wait_int_m3_897 SECONDS.
    ENDIF.

    p_bloq = abap_true.
    EXIT.
  ENDIF.

  LOOP AT p_return INTO DATA(wa_return_interno).
    MESSAGE ID wa_return_interno-id TYPE 'S'
     NUMBER wa_return_interno-number
       WITH wa_return_interno-message_v1 wa_return_interno-message_v2 wa_return_interno-message_v3 wa_return_interno-message_v4
    DISPLAY LIKE 'E'.
  ENDLOOP.

ENDFORM.
