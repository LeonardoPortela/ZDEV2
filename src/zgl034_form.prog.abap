*&---------------------------------------------------------------------*
*&  Include           ZGL034_FORM
*&---------------------------------------------------------------------*

FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  FREE: wa_fcat, it_fcat.

  CASE p_screen.
    WHEN '0100'.

      PERFORM f_estrutura_alv USING:

         01  ''         ''             'IT_SAIDA_0100' 'STATUS_CTB'    'St.Ctb.'             '07'   ' '    '' ' ' 'C' ' ' ' ' ' ',
         02  'ZGLT080'  'SEQ_LCTO'     'IT_SAIDA_0100' 'SEQ_LCTO'      'Seq.Lcto'            '08'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         03  'ZGLT080'  'BUKRS'        'IT_SAIDA_0100' 'BUKRS'         'Empresa'             '07'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         04  'T001'     'BUTXT'        'IT_SAIDA_0100' 'BUTXT'         'Nome Empresa'        ''     ' '    '' ' ' ' ' ' ' ' ' ' ',
         05  'ZGLT080'  ''             'IT_SAIDA_0100' 'LIFNR'         'Fornecedor'          '10'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         06  'LFA1'     'NAME1'        'IT_SAIDA_0100' 'NAME1'         'Nome do Fornecedor'  ''     ' '    '' ' ' ' ' ' ' ' ' ' ',
         07  'ZGLT080'  'ZFBDT'        'IT_SAIDA_0100' 'ZFBDT'         'Dt.Venc.'            ''     ' '    '' ' ' ' ' ' ' ' ' ' ',
         08  'ZGLT080'  'NETWR'        'IT_SAIDA_0100' 'NETWR'         'Vlr.Pagto R$'        '13'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         08  'ZGLT080'  'VLR_LIQ_RET'  'IT_SAIDA_0100' 'VLR_LIQ_RET'   'Vlr.Pgto.Liq.R$'     '15'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         08  'ZGLT080'  'VLR_DESCONTO' 'IT_SAIDA_0100' 'VLR_DESCONTO'  'Vlr.Crédito.R$'      '14'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         09  ''         ''             'IT_SAIDA_0100' 'LOTE'          'Lote'                ''     ' '    '' ' ' ' ' 'X' ' ' ' ',
         10  'ZGLT080'  ''             'IT_SAIDA_0100' 'ANEXOS'        'Anexos'              '06'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         11  'ZGLT080'  ''             'IT_SAIDA_0100' 'USNAM'         'Usuário'             ''     ' '    '' ' ' ' ' ' ' ' ' ' ',
         12  'ZGLT080'  ''             'IT_SAIDA_0100' 'DT_LCTO'       'Dt.Registro'         ''     ' '    '' ' ' ' ' ' ' ' ' ' ',
         13  'ZGLT080'  ''             'IT_SAIDA_0100' 'HR_LCTO'       'Hr.Registro'         ''     ' '    '' ' ' ' ' ' ' ' ' ' '.

    WHEN '0110'.

      PERFORM f_estrutura_alv USING:

          0  ''         ''           'IT_SAIDA_0110' 'STATUS_CTB'    'Sts.Ctb'           '06'   ' '    ''  ' ' 'C' 'X' ' ' ' ',
          1  ''         ''           'IT_SAIDA_0110' 'STATUS_NF'     'Sts.NF'            '06'   ' '    ''  ' ' 'C' 'X' ' ' ' ',
          2  ''         ''           'IT_SAIDA_0110' 'DOC_LCTO'      'Doc.Lcto'          ''     ' '    ''  ' ' ' ' 'X' ' ' ' ',
          3  ''         ''           'IT_SAIDA_0110' 'BELNR'         'Doc.Ctb.'          ''     ' '    ''  ' ' ' ' 'X' ' ' ' ',
          4  'ZGLT081'  'DOCNUM'     'IT_SAIDA_0110' 'DOCNUM'        'Doc.Num.'          '10'   ' '    ''  ' ' ' ' 'X' ' ' ' ',
          5  ''         ''           'IT_SAIDA_0110' 'SEQITEM'       'Item'              '04'   ' '    ''  ' ' ' ' ' ' ' ' ' ',
          6  'SKA1'     'SAKNR'      'IT_SAIDA_0110' 'HKONT'         'Conta'             '10'   'X'    ''  ' ' ' ' ' ' ' ' ' ',
          7  ''         ''           'IT_SAIDA_0110' 'GSBER'         'Filial'            '06'   'X'    ''  ' ' ' ' ' ' 'X' ' ',
          8  ''         ''           'IT_SAIDA_0110' 'KOSTL'         'C.Custo'           ''     'X'    ''  ' ' ' ' ' ' 'X' ' ',
          9  'ZGLT081'  'VBELN'      'IT_SAIDA_0110' 'VBELN'         'Ordem'             ''     'X'    ''  ' ' ' ' ' ' ' ' ' ',
         10  'ZGLT081'  'BLDAT'      'IT_SAIDA_0110' 'BLDAT'         'Dt.Emissão'        ''     'X'    ''  ' ' ' ' ' ' ' ' ' ',
         11  'ZGLT081'  'BUDAT'      'IT_SAIDA_0110' 'BUDAT'         'Dt.Lcto'           ''     'X'    ''  ' ' ' ' ' ' ' ' ' ',
         12  'ZGLT081'  'MENGE'      'IT_SAIDA_0110' 'MENGE'         'Quantidade'        '10'   'X'    ''  ' ' ' ' ' ' ' ' ' ',
         13  'ZGLT081'  'NETWR'      'IT_SAIDA_0110' 'NETWR'         'Valor R$'          '10'   'X'    'X' ' ' ' ' ' ' ' ' ' ',
         14  'ZGLT081'  'NFENUM'     'IT_SAIDA_0110' 'NFENUM'        'Nro.Nota'          ''     'X'    ''  ' ' ' ' ' ' ' ' ' ',
         15  'ZGLT081'  'SERIES'     'IT_SAIDA_0110' 'SERIES'        'Ser.'              ''     'X'    ''  ' ' ' ' ' ' ' ' ' ',
         16  'ZGLT081'  'MATNR'      'IT_SAIDA_0110' 'MATNR'         'Material'          '10'   'X'    ''  ' ' ' ' ' ' ' ' ' ',
         17  'ZGLT081'  'ASNUM'      'IT_SAIDA_0110' 'ASNUM'         'Cod.Serviço'       '10'   'X'    ''  ' ' ' ' ' ' ' ' ' ',
         18  'ZGLT081'  'SGTXT'      'IT_SAIDA_0110' 'SGTXT'         'Texto'             '25'   'X'    ''  ' ' ' ' ' ' ' ' ' ',
         19  'ZGLT081'  'COD_BARRAS' 'IT_SAIDA_0110' 'COD_BARRAS'    'Cod.Barras'        '49'   ' '    ''  ' ' ' ' ' ' ' ' ' '.
*         20  'ZMMT0104' 'ID_LMS'     'IT_SAIDA_0110' 'ID_LMS'        'Sol.Treinamento'   '10'   'X'    ''  ' ' ' ' ' ' 'X' ' '.

    WHEN '0112'.

      PERFORM f_estrutura_alv USING:

        01  ''         ''             'IT_SAIDA_0112' 'GSBER'         'Filial'            '06'   ' '    ''  ' ' ' ' ' ' 'X' ' ',
        02  'ZGLT081'  'NFENUM'       'IT_SAIDA_0112' 'NFENUM'        'Nro.Nota'          ''     ' '    ''  ' ' ' ' ' ' ' ' ' ',
        03  'ZGLT081'  'SERIES'       'IT_SAIDA_0112' 'SERIES'        'Ser.'              ''     ' '    ''  ' ' ' ' ' ' ' ' ' ',
        04  'ZGLT081'  'NETWR'        'IT_SAIDA_0112' 'NETWR'         'Valor R$'          '13'   ' '    'X' ' ' ' ' ' ' ' ' ' ',
        05  'ZGLT080'  'VLR_LIQ_RET'  'IT_SAIDA_0112' 'VLR_LIQ_RET'   'Vlr.Liq.R$'        '13'   ' '    ''  ' ' ' ' ' ' ' ' ' '.


      IF vg_modify_cbar IS NOT INITIAL.

        PERFORM f_estrutura_alv USING:

        05  'ZGLT081'  'COD_BARRAS' 'IT_SAIDA_0112' 'COD_BARRAS'    'Cod.Barras'        '49'   'X'    ''  ' ' ' ' ' ' ' ' ' '.
      ENDIF.

    WHEN '0113'.

      IF vg_modify_irf IS NOT INITIAL.

        PERFORM f_estrutura_alv USING:

        01  ''           ''            'IT_SAIDA_0113' 'WT_BASMAN'     'Base Manual'          '11'   'X'  ''  ' ' ' ' ' ' ' ' 'X',
        01  ''           ''            'IT_SAIDA_0113' 'WT_TAXMAN'     'IRF Manual'           '11'   'X'  ''  ' ' ' ' ' ' ' ' 'X'.

      ELSE.

        PERFORM f_estrutura_alv USING:

        01  ''           ''            'IT_SAIDA_0113' 'WT_BASMAN'     'Base Manual'          '11'   ' '  ''  ' ' ' ' ' ' ' ' 'X',
        01  ''           ''            'IT_SAIDA_0113' 'WT_TAXMAN'     'IRF Manual'           '11'   ' '  ''  ' ' ' ' ' ' ' ' 'X'.


      ENDIF.

      PERFORM f_estrutura_alv USING:

        02  'ZGLT088'   'WITHT'        'IT_SAIDA_0113' 'WITHT'         'Categ.IRF'            '09'   ' '    ''  ' ' ' ' ' ' ' ' ' ',
        03  'ZGLT088'   'WT_WITHCD'    'IT_SAIDA_0113' 'WT_WITHCD'     'Cód.IRF'              '07'   ' '    ''  ' ' ' ' ' ' ' ' ' ',
        04  'T059U'     'TEXT40'       'IT_SAIDA_0113' 'TEXT40'        'Descrição'            '30'   ' '    ''  ' ' ' ' ' ' ' ' ' '.

      IF vg_modify_irf IS NOT INITIAL.

        PERFORM f_estrutura_alv USING:

        05  'ZGLT088'   'WI_TAX_BASE'  'IT_SAIDA_0113' 'WI_TAX_BASE'   'Base IRF'             '13'   'X'    ''  ' ' ' ' ' ' ' ' ' ',
        06  'ZGLT088'   'WI_TAX_AMT'   'IT_SAIDA_0113' 'WI_TAX_AMT'    'Valor IRF'            '13'   'X'    'X' ' ' ' ' ' ' ' ' ' '.

      ELSE.

        PERFORM f_estrutura_alv USING:

        05  'ZGLT088'   'WI_TAX_BASE'  'IT_SAIDA_0113' 'WI_TAX_BASE'   'Base IRF'             '13'   ' '    ''  ' ' ' ' ' ' ' ' ' ',
        06  'ZGLT088'   'WI_TAX_AMT'   'IT_SAIDA_0113' 'WI_TAX_AMT'    'Valor IRF'            '13'   ' '    'X' ' ' ' ' ' ' ' ' ' '.


      ENDIF.

  ENDCASE.

ENDFORM.

FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                           VALUE(p_tabname)       LIKE dd02d-tabname
                           VALUE(p_field)         LIKE dd03d-fieldname
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                           VALUE(p_outputlen)
                           VALUE(p_edit)
                           VALUE(p_sum)
                           VALUE(p_emphasize)
                           VALUE(p_just)
                           VALUE(p_hotspot)
                           VALUE(p_f4)
                           VALUE(p_checkbox).


  IF p_tabname = 'IT_SAIDA_0110'.
    CASE p_field.
      WHEN 'DOCNUM' OR 'STATUS_CTB' OR 'STATUS_NF' OR 'DOC_LCTO' OR 'BELNR'.
        CHECK vg_opr_lcto EQ c_display.
    ENDCASE.
  ENDIF.

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.

  IF ( vg_opr_lcto = c_edit ) OR
     ( vg_opr_lcto = c_new ).
    wa_fcat-do_sum      = p_sum.
  ENDIF.

  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
  wa_fcat-checkbox    = p_checkbox.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_exclude_fcode USING p_screen.

  APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_exclude_fcode.

  CASE p_screen.
    WHEN '0112' OR '0113'.
      APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_exclude_fcode.
  ENDCASE.

  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_check             TO it_exclude_fcode.

ENDFORM.

FORM f_selecionar_dados.

  CLEAR: vg_not_found.

  PERFORM: f_limpa_variaveis,
           f_config_ranges.

  IF vg_ger_mov IS NOT INITIAL. "Filtrar Somente Lctos com NF sem gerar.

    CLEAR: tg_081_aux[], tg_080[].
    SELECT *
      FROM zglt081 INTO CORRESPONDING FIELDS OF TABLE tg_081_aux
     WHERE objkey     NE ''
       AND docnum     EQ '0000000000'
       AND part_forn  EQ ''
       AND loekz      EQ ''
       AND desconto   EQ ''
       AND sem_nf     EQ ''.

    IF tg_081_aux[] IS NOT INITIAL.
      SELECT *
        FROM zglt080 INTO CORRESPONDING FIELDS OF TABLE tg_080
         FOR ALL ENTRIES IN tg_081_aux
       WHERE seq_lcto EQ tg_081_aux-seq_lcto
         AND loekz    EQ ''.
    ENDIF.

  ELSE.

    IF p_seq_lcto-low IS INITIAL.

      IF p_bukrs-low IS INITIAL.
        vg_not_found = 'X'.
        MESSAGE s836(sd) WITH TEXT-w02 DISPLAY LIKE 'S'.
        RETURN.
      ENDIF.

      IF p_budat-low IS INITIAL.
        vg_not_found = 'X'.
        MESSAGE s836(sd) WITH TEXT-w03 DISPLAY LIKE 'S'.
        RETURN.
      ENDIF.

    ENDIF.

    SELECT *
      FROM zglt080 AS a INTO CORRESPONDING FIELDS OF TABLE tg_080
     WHERE a~bukrs    IN r_bukrs
       AND a~lifnr    IN r_lifnr
       AND a~zfbdt    IN r_zfbdt
       AND a~seq_lcto IN r_seq_lcto
       AND a~loekz    EQ ''
       AND a~usnam    IN r_usnam
       AND EXISTS ( SELECT *
                      FROM zglt081 AS b
                     WHERE b~seq_lcto = a~seq_lcto
                       AND b~budat IN r_budat
                       AND b~bldat IN r_bldat ).
  ENDIF.

  IF tg_080[] IS INITIAL.
    vg_not_found = 'X'.
    MESSAGE s836(sd) WITH TEXT-w01 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  LOOP AT tg_080.
    "Atualização de Doc.Contabeis antes da selecão dos itens do Lcto.
    CLEAR: wa_saida_0100.
    MOVE-CORRESPONDING tg_080 TO wa_saida_0100.
    PERFORM f_seleciona_0110 USING wa_saida_0100.

    tg_080-instid_a = tg_080-seq_lcto.
    MODIFY tg_080.
  ENDLOOP.

  SELECT *
    FROM t001 INTO CORRESPONDING FIELDS OF TABLE tg_t001
     FOR ALL ENTRIES IN tg_080
   WHERE bukrs = tg_080-bukrs.

  SELECT *
    FROM lfa1 INTO CORRESPONDING FIELDS OF TABLE tg_lfa1
     FOR ALL ENTRIES IN tg_080
   WHERE lifnr = tg_080-lifnr.

  SELECT *
    FROM zglt081 INTO TABLE tg_081
     FOR ALL ENTRIES IN tg_080
   WHERE seq_lcto = tg_080-seq_lcto.

  SELECT *
   FROM srgbtbrel INTO TABLE tg_anexos
    FOR ALL ENTRIES IN tg_080
  WHERE reltype  EQ 'ATTA'
    AND instid_a = tg_080-instid_a
    AND typeid_a EQ 'ZGL059'.

ENDFORM.

FORM f_limpa_variaveis .

  CLEAR: wa_saida_0100,
         it_saida_0100[],
         wa_saida_0110,
         it_saida_0110[],
         tg_080[],
         tg_081[],
         tg_081_aux[],
         tg_t001.

ENDFORM.

FORM f_config_ranges .

  CLEAR: r_bukrs,     r_bukrs[],
         r_lifnr,     r_lifnr[],
         r_budat,     r_budat[],
         r_bldat,     r_bldat[],
         r_zfbdt,     r_zfbdt[],
         r_seq_lcto,  r_seq_lcto[],
         r_usnam,     r_usnam[].

  "----------------------------------------------------
  " Empresa
  "----------------------------------------------------
  IF p_bukrs-low IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_bukrs-low
      IMPORTING
        output = p_bukrs-low.

    r_bukrs-sign   = 'I'.
    r_bukrs-option = 'EQ'.
    r_bukrs-low  = p_bukrs-low.
    r_bukrs-high = p_bukrs-low.

    IF ( p_bukrs-high IS NOT INITIAL ).
      r_bukrs-option = 'BT'.
      r_bukrs-high = p_bukrs-high .
    ENDIF.

    APPEND r_bukrs.
  ENDIF.

  "----------------------------------------------------
  " Fornecedor
  "----------------------------------------------------
  IF p_lifnr-low IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_lifnr-low
      IMPORTING
        output = p_lifnr-low.

    r_lifnr-sign   = 'I'.
    r_lifnr-option = 'EQ'.
    r_lifnr-low  = p_lifnr-low.
    r_lifnr-high = p_lifnr-low.

    IF ( p_lifnr-high IS NOT INITIAL ).
      r_lifnr-option = 'BT'.
      r_lifnr-high = p_lifnr-high .
    ENDIF.
    APPEND r_lifnr.
  ENDIF.

  "----------------------------------------------------
  " Data Lcto.
  "----------------------------------------------------
  IF p_budat-low IS NOT INITIAL.
    r_budat-sign   = 'I'.
    r_budat-option = 'EQ'.
    r_budat-low  = p_budat-low.
    r_budat-high = p_budat-low.

    IF ( p_budat-high IS NOT INITIAL ).
      r_budat-option = 'BT'.
      r_budat-high = p_budat-high .
    ENDIF.
    APPEND r_budat.
  ENDIF.

  "----------------------------------------------------
  " Data do Documento
  "----------------------------------------------------
  IF p_bldat-low IS NOT INITIAL.
    r_bldat-sign   = 'I'.
    r_bldat-option = 'EQ'.
    r_bldat-low  = p_bldat-low.
    r_bldat-high = p_bldat-low.

    IF ( p_bldat-high IS NOT INITIAL ).
      r_bldat-option = 'BT'.
      r_bldat-high = p_bldat-high .
    ENDIF.
    APPEND r_bldat.
  ENDIF.


  "----------------------------------------------------
  " Data Vcto.
  "----------------------------------------------------
  IF p_zfbdt-low IS NOT INITIAL.
    r_zfbdt-sign   = 'I'.
    r_zfbdt-option = 'EQ'.
    r_zfbdt-low  = p_zfbdt-low.
    r_zfbdt-high = p_zfbdt-low.

    IF ( p_zfbdt-high IS NOT INITIAL ).
      r_zfbdt-option = 'BT'.
      r_zfbdt-high = p_zfbdt-high .
    ENDIF.
    APPEND r_zfbdt.
  ENDIF.

  "----------------------------------------------------
  " Seq. Lcto
  "----------------------------------------------------
  IF p_seq_lcto-low IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_seq_lcto-low
      IMPORTING
        output = p_seq_lcto-low.

    r_seq_lcto-sign   = 'I'.
    r_seq_lcto-option = 'EQ'.
    r_seq_lcto-low  = p_seq_lcto-low.
    r_seq_lcto-high = p_seq_lcto-low.

    IF ( p_seq_lcto-high IS NOT INITIAL ).
      r_seq_lcto-option = 'BT'.
      r_seq_lcto-high = p_seq_lcto-high .
    ENDIF.
    APPEND r_seq_lcto.
  ENDIF.


  "----------------------------------------------------
  " Usuario.
  "----------------------------------------------------
  IF p_usnam-low IS NOT INITIAL.
    r_usnam-sign   = 'I'.
    r_usnam-option = 'EQ'.
    r_usnam-low  = p_usnam-low.
    r_usnam-high = p_usnam-low.

    IF ( p_usnam-high IS NOT INITIAL ).
      r_usnam-option = 'BT'.
      r_usnam-high = p_usnam-high .
    ENDIF.
    APPEND r_usnam.
  ENDIF.

ENDFORM.

FORM f_refresh_alv USING p_alv.
  CASE p_alv.
    WHEN '0100'.
      CALL METHOD obj_alv_0100->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    WHEN '0110'.
      CALL METHOD obj_alv_0110->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
  ENDCASE.
ENDFORM.

FORM f_processa_dados .

  DATA: vl_sem_ctb  TYPE c,
        vl_doc_lcto TYPE c.

  CHECK vg_not_found IS INITIAL.

  LOOP AT tg_080.
    CLEAR: wa_saida_0100, tg_t001, vl_sem_ctb, vl_doc_lcto.
    MOVE-CORRESPONDING tg_080 TO wa_saida_0100.

    READ TABLE tg_t001 WITH KEY bukrs = wa_saida_0100-bukrs.
    IF sy-subrc = 0.
      wa_saida_0100-butxt = tg_t001-butxt.
    ENDIF.

    READ TABLE tg_lfa1 WITH KEY lifnr = wa_saida_0100-lifnr.
    IF sy-subrc = 0.
      wa_saida_0100-name1 = tg_lfa1-name1.
    ENDIF.

    wa_saida_0100-status_ctb  = icon_light_out. "Status Inicial
    LOOP AT tg_081 WHERE seq_lcto  = tg_080-seq_lcto
                     AND part_forn = space.
      IF tg_081-doc_lcto IS NOT INITIAL.
        wa_saida_0100-status_ctb = icon_yellow_light.  "Quando algum item já gerou Ctb.
        vl_doc_lcto = 'X'.
      ENDIF.

      IF tg_081-belnr IS INITIAL.
        vl_sem_ctb = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF ( vl_doc_lcto IS NOT INITIAL ) AND ( vl_sem_ctb IS INITIAL ).
      wa_saida_0100-status_ctb = icon_green_light.
*      LOOP AT TG_081 WHERE SEQ_LCTO   = TG_080-SEQ_LCTO
*                       AND DOCNUM     = SPACE
*                       AND PART_FORN  = ''
*                       AND ESTORNO_NF = ''.
*        CLEAR: WA_SAIDA_0110.
*        MOVE-CORRESPONDING TG_081 TO WA_SAIDA_0110_AUX.
*        PERFORM F_GERAR_NF USING WA_SAIDA_0110_AUX
*                        CHANGING WA_SAIDA_0110_AUX-DOCNUM.
*      ENDLOOP.
    ENDIF.

    LOOP AT tg_anexos WHERE instid_a = tg_080-instid_a.
      ADD 1 TO wa_saida_0100-anexos.
    ENDLOOP.

    APPEND wa_saida_0100 TO it_saida_0100.
  ENDLOOP.

  SORT it_saida_0100 BY seq_lcto DESCENDING.

ENDFORM.

FORM f_create_lote .

  DATA: vl_lote_tmp TYPE zglt034-lote,
        wl_zglt034  TYPE zglt034.

  CLEAR: vl_lote_tmp, wl_zglt034.
  CALL TRANSACTION 'ZGL015' AND SKIP FIRST SCREEN.
  GET PARAMETER ID 'LOT' FIELD  vl_lote_tmp.

  CHECK vl_lote_tmp IS NOT INITIAL.

  zglt080-lote = vl_lote_tmp.

  SELECT SINGLE *
    FROM zglt034 INTO wl_zglt034
   WHERE lote = zglt080-lote.

  IF ( sy-subrc NE 0 ) OR ( wl_zglt034-bukrs IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e03 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  UPDATE zglt034 SET tcode = sy-tcode
  WHERE lote = zglt080-lote.

  zglt080-bukrs = wl_zglt034-bukrs.

ENDFORM.

FORM f_register_f4_for_fields USING p_screen.
  DATA wl_f4 TYPE lvc_t_f4 WITH HEADER LINE.

  CASE p_screen.
    WHEN '0100'.


    WHEN '0110'.

      wl_f4-fieldname = 'ASNUM'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'GSBER'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'ID_LMS'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'KOSTL'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.



      CALL METHOD obj_alv_0110->register_f4_for_fields
        EXPORTING
          it_f4 = wl_f4[].

  ENDCASE.
ENDFORM.                    "REGISTER_F4_FOR_FIELDS

FORM f_gravar_lcto USING p_validar_lcto.

   DATA: VG_MATNR TYPE CHAR18.

*------------------------------------------------------------------*
* Field Simbols
*------------------------------------------------------------------*

  FIELD-SYMBOLS: <saida_0110> TYPE ty_saida_0100.

*------------------------------------------------------------------*
* Types
*------------------------------------------------------------------*
  TYPES: ty_lms_conta TYPE RANGE OF zmmt0103-saknr,
         ty_rg_bukrs  TYPE RANGE OF zglt081-bukrs.

*------------------------------------------------------------------*
* Internal Tables
*------------------------------------------------------------------*

  DATA: gt_081       TYPE TABLE OF zglt081,
        gt_081_aux   TYPE TABLE OF zglt081,
        gt_083       TYPE TABLE OF zglt083,
        gt_085       TYPE TABLE OF zglt085,
        gt_088       TYPE TABLE OF zglt088,
        gt_089       TYPE TABLE OF zglt089,
        tg_0110_agr  TYPE TABLE OF ty_saida_0110 WITH HEADER LINE,
        tg_irf_agr   TYPE TABLE OF ty_irf WITH HEADER LINE,
        "TG_0110_AUX    TYPE TABLE OF TY_SAIDA_0110 WITH HEADER LINE,
        gt_impostos  LIKE TABLE OF tg_impo WITH HEADER LINE,
        tg_0110_cbar TYPE TABLE OF ty_saida_0110 WITH HEADER LINE.

*------------------------------------------------------------------*
* Work Areas
*------------------------------------------------------------------*

  DATA: wl_j1baa    TYPE j_1baa,
        wl_shipfrom TYPE lfa1-regio,
        wl_shipto   TYPE lfa1-regio,
        wl_lfa1     TYPE lfa1,
        wl_t001w    TYPE t001w,
        wl_080      TYPE zglt080,
        wl_080_aux  TYPE zglt080,
        wl_081      TYPE zglt081,
        wl_081_aux  TYPE zglt081,
        wl_082      TYPE zglt082,
        wl_083      TYPE zglt083,
        wl_085      TYPE zglt085,
        wl_088      TYPE zglt088,
        wl_089      TYPE zglt089,
        wl_asmd     TYPE asmd,
        wl_mara     TYPE mara,
        vl_xblnr    TYPE zglt035-xblnr,
        vl_vlr_nf   TYPE wrbtr.

*------------------------------------------------------------------*
* Variaveis
*------------------------------------------------------------------*
  DATA: vl_idx_item    TYPE i,
        vl_item_forn   TYPE i,
        vl_idx_str     TYPE string,
        vl_valid       TYPE c,
        vl_error       TYPE c,
        vl_lcto_irf    TYPE c,
        vl_ger_ctb     TYPE c,
        vl_gsber_forn  TYPE zglt081-gsber,
        vl_seqlcto_ger TYPE zglt087-message,
        vl_cbar_str    TYPE c LENGTH 20,
        vl_vlr_cbar_01 TYPE c LENGTH 10,
        vl_vlr_cbar    TYPE c LENGTH 10.

  DATA: answ_treina TYPE c.

  IF ( p_validar_lcto IS INITIAL ) AND
     ( vg_call_ext IS INITIAL ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente gravar o Lançamento?'
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

  ENDIF.

  CLEAR: vl_gsber_forn , wl_080, gt_081[], gt_085[], gt_088[], gt_089[].

  IF vg_call_ext IS NOT INITIAL. "Caso chamada Externa, atribui dados automaticamente
    vg_id_log = 1.
    vg_opr_lcto = c_new.
    PERFORM f_monta_dados_ext.
  ENDIF.

  IF vg_call_ext IS INITIAL.
    CALL METHOD obj_alv_0110->check_changed_data.
  ENDIF.

*----------------------------------------------------------------------*
* Grava Cabeçalho
*----------------------------------------------------------------------*
  PERFORM f_gerar_seq_lcto USING p_validar_lcto
                        CHANGING zglt080-seq_lcto
                                 vl_valid.
  IF vl_valid IS INITIAL.
    RETURN.
  ENDIF.

  IF vg_call_ext IS INITIAL.
    PERFORM f_atrib_forma_pagto.
  ENDIF.

  wl_080-seq_lcto     = zglt080-seq_lcto.
  wl_080-lote         = zglt080-lote.
  wl_080-bukrs        = zglt080-bukrs.
  wl_080-lifnr        = zglt080-lifnr.
  wl_080-hbkid        = zglt080-hbkid.
  wl_080-ref_doc_no   = zglt080-ref_doc_no.
  wl_080-netwr        = zglt080-netwr.
  wl_080-bktxt        = zglt080-bktxt.
  wl_080-zlsch        = zglt080-zlsch.
  wl_080-zlspr        = zglt080-zlspr. " RJF - CS2024000206 - Lançamentos Bloqueados - Contratos Corporativos (ZGL059)
  wl_080-bvtyp        = zglt080-bvtyp.
  wl_080-budat        = zglt080-budat.
  wl_080-zfbdt        = zglt080-zfbdt.
  wl_080-sem_nf       = zglt080-sem_nf.
  wl_080-usnam        = sy-uname.
  wl_080-dt_lcto      = sy-datum.
  wl_080-hr_lcto      = sy-uzeit.

  PERFORM f_valid_cabec USING wl_080
                     CHANGING vl_valid.

  IF vl_valid IS INITIAL.
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* Grava Itens.
*----------------------------------------------------------------------*
  IF it_saida_0110[] IS INITIAL.
    MESSAGE s836(sd) WITH TEXT-e18 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' TEXT-e18 '' ''.
    RETURN.
  ENDIF.

  DATA(r_contas) = VALUE ty_lms_conta( FOR w_110 IN it_saida_0110[] (
                                      sign = 'I'
                                      option = 'EQ'
                                      low = w_110-hkont ) ).
  SORT r_contas BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM r_contas COMPARING low.

  "CS2020001085
*  SELECT z103~saknr, z103~fg_obrig, z103~fg_atencao
*    FROM zmmt0103 AS z103
*    INTO TABLE @DATA(it_zmmt0103)
*    WHERE z103~saknr IN @r_contas.
*  IF sy-subrc = 0.
*    "Criar range das empresas que não entram na regra do treinamento:
*    DATA(rg_bukrs_exc) = VALUE ty_rg_bukrs(
*        ( sign = 'I' option = 'EQ' low = '0035' )
*        ( sign = 'I' option = 'EQ' low = '0038' )
*        ( sign = 'I' option = 'EQ' low = '0039' )
*        ( sign = 'I' option = 'EQ' low = '0043' )   ).
*
*    CLEAR: v_resp_treina.
*    LOOP AT it_zmmt0103[] INTO DATA(w_103).
*      READ TABLE it_saida_0110[] INTO DATA(w_0110) WITH KEY hkont = w_103-saknr.
*
*      IF ( line_exists( rg_bukrs_exc[ low = w_0110-bukrs  ] ) ).
*        CONTINUE.
*      ENDIF.
*
*      IF ( sy-subrc = 0 ) AND ( w_103-fg_atencao IS NOT INITIAL )
*          AND ( w_0110-id_lms IS INITIAL )." AND ( w_0110-bukrs NOT IN rg_bukrs_exc[] ).
*        .
*        "MESSAGE 'Preencher o campo “Sol.Treinamento"' TYPE 'I'.
*        CALL FUNCTION 'POPUP_TO_CONFIRM'
*          EXPORTING
*            titlebar              = 'Confirmação'
*            text_question         = 'Lançamento referente à treinamento?'
*            text_button_1         = 'Sim'
*            icon_button_1         = 'ICON_CHECKED'
*            text_button_2         = 'Nao'
*            icon_button_2         = 'ICON_CANCEL'
*            display_cancel_button = 'X'
*            popup_type            = 'ICON_MESSAGE_ERROR'
*          IMPORTING
*            answer                = answ_treina.
*
*        v_resp_treina = COND #( WHEN answ_treina = 1 THEN 'S'
*                                WHEN answ_treina = 2 THEN 'N'
*                                ELSE ' ' ).
*
*        IF ( answ_treina EQ 1 ).
*          MESSAGE 'Obrigatório preencher o campo “Sol.Treinamento' TYPE 'E'.
*          EXIT.
*        ENDIF.
*      ELSEIF ( sy-subrc = 0 ) AND ( w_103-fg_obrig IS NOT INITIAL )
*         AND ( w_0110-id_lms IS INITIAL )." AND ( w_0110-bukrs NOT IN rg_bukrs_exc[] ).
*        MESSAGE 'Obrigatório preencher o campo “Sol.Treinamento' TYPE 'E'.
*        EXIT.
*      ELSE.
*
*        TRY.
*            DATA(w_0105) = VALUE zmmt0105(
*              ebeln    = '0000000000'
*              ebelp    = 00000
*              cpf      = it_0110_treina[ id_lms = w_0110-id_lms
*                                         seq_lcto = w_0110-seq_lcto
*                                         seqitem  = w_0110-seqitem  ]-cpf
*              id_lms   = w_0110-id_lms
*              tipo     = 'Z'
*              lifnr    = zglt080-lifnr
*              atencao  = v_resp_treina "COND #( WHEN W_103-FG_ATENCAO IS NOT INITIAL AND W_0110-ID_LMS IS NOT INITIAL THEN 'S' ELSE 'N' )
*              lote     = zglt080-lote " W_0110-LOTE
*              seq_lcto = zglt080-seq_lcto " W_0110-SEQ_LCTO
*              seqitem  = w_0110-seqitem
*              data     = sy-datum
*              hora     = sy-uzeit
*              usuario  = sy-uname ).
*
*            MODIFY zmmt0105 FROM w_0105.
*
*          CATCH cx_sy_itab_line_not_found.
*        ENDTRY.
*
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  IF ( ( wl_080-zlsch NE 'D' ) AND ( wl_080-zlsch NE 'E' ) ). " Não é Lançamento por Boleto
    "Caso os NFs sejam todas iguais,
    "marcar para gerar uma partida de fornecedor agrupada.
    PERFORM f_nf_unica TABLES it_saida_0110.
    IF vg_nf_unica IS NOT INITIAL.
      wl_080-nf_unica   = 'X'.
      wl_080-agrp_forn  = 'X'.
    ENDIF.
  ELSE. " É Lançamento por Boleto
    "Caso os codigos de barras sejam todos iguais,
    "marcar para gerar uma partida de fornecedor agrupada.
    PERFORM f_cbar_unico TABLES it_saida_0110.
    IF vg_cbar_unico IS NOT INITIAL.
      wl_080-agrp_forn  = 'X'.
      wl_080-cod_barras = vg_cbar_unico.
    ENDIF.

    PERFORM f_nf_unica TABLES it_saida_0110.
    IF vg_nf_unica IS NOT INITIAL.
      wl_080-nf_unica   = 'X'.
    ENDIF.

  ENDIF.

  IF ( vg_opr_lcto = c_edit ) AND  ( p_validar_lcto IS INITIAL ).
    DELETE FROM zglt081 WHERE seq_lcto = wl_080-seq_lcto.
    DELETE FROM zglt083 WHERE seq_lcto = wl_080-seq_lcto.
    DELETE FROM zglt085 WHERE seq_lcto = wl_080-seq_lcto.
    DELETE FROM zglt086 WHERE seq_lcto = wl_080-seq_lcto.
    DELETE FROM zglt088 WHERE seq_lcto = wl_080-seq_lcto.
  ENDIF.

  "Agrupa Itens da Mesma NF.
  PERFORM f_monta_agrp_nf TABLES it_saida_0110
                                 tg_0110_agr
                           USING zglt080-netwr
                                 zglt080-vlr_desconto.

  PERFORM f_gerar_dados_irf TABLES tg_0110_agr
                             USING vl_error
                                   vl_lcto_irf.
  IF vl_error IS NOT INITIAL.
    ROLLBACK WORK.
    RETURN.
  ENDIF.

  "Gerar Total Liquido
  CLEAR: zglt080-vlr_liq_ret.
  LOOP AT tg_0110_agr.
    ADD tg_0110_agr-vlr_liq_ret TO zglt080-vlr_liq_ret.
  ENDLOOP.

  IF ( zglt080-vlr_liq_ret IS INITIAL ).
    MESSAGE s836(sd) WITH 'Vlr. Vlr. Pagto Líquido R$' TEXT-e17 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' 'Vlr. Pagto Líquido R$' TEXT-e17 ''.
    RETURN.
  ENDIF.

  wl_080-vlr_liq_ret  = zglt080-vlr_liq_ret.

  IF ( zglt080-netwr IS INITIAL ).
    MESSAGE s836(sd) WITH 'Vlr. Pagamento R$' TEXT-e17 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' 'Vlr. Pagamento R$' TEXT-e17 ''.
    RETURN.
  ENDIF.

  wl_080-netwr        = zglt080-netwr.
  wl_080-vlr_desconto = zglt080-vlr_desconto.

  vl_idx_item = 1.
  LOOP AT it_saida_0110 INTO wa_saida_0110.
    CLEAR: wl_081, wl_082, wl_j1baa, wl_shipfrom, wl_shipto, wl_lfa1,
           wl_t001w.

    vl_idx_str = vl_idx_item.

    READ TABLE tg_0110_agr WITH KEY nfenum = wa_saida_0110-nfenum
                                    series = wa_saida_0110-series.
    IF sy-subrc = 0.
      wl_081-agrp_nf    = tg_0110_agr-agrp_nf.
      wl_081-gsber_nf   = tg_0110_agr-gsber. "Filial de Lançamento da NF.
    ENDIF.

    wl_081-seq_lcto     = wl_080-seq_lcto.
    wl_081-seqitem      = vl_idx_item.
    wl_081-lote         = wl_080-lote.
    wl_081-bukrs        = wl_080-bukrs.
    wl_081-gsber        = wa_saida_0110-gsber.
    wl_081-bschl        = '40'.
    wl_081-hkont        = wa_saida_0110-hkont.
    wl_081-kostl        = wa_saida_0110-kostl.
    wl_081-vbeln        = wa_saida_0110-vbeln.
    wl_081-budat        = wa_saida_0110-budat.
    wl_081-bldat        = wa_saida_0110-bldat.
    wl_081-menge        = wa_saida_0110-menge.
    wl_081-netwr        = wa_saida_0110-netwr.

    IF wl_081-netwr < 0.
      wl_081-desconto   = 'X'.
      wl_081-netwr      = abs( wl_081-netwr ).
      wl_081-bschl      = '50'.
    ENDIF.

    wl_081-nfenum       = wa_saida_0110-nfenum.
    wl_081-series       = wa_saida_0110-series.
    wl_081-ref_doc_no   = wa_saida_0110-ref_doc_no.

    VG_MATNR = |{ wa_saida_0110-matnr ALPHA = OUT }|.
    VG_MATNR = |{ VG_MATNR ALPHA = IN }|.
    wa_saida_0110-matnr = VG_MATNR.

    wl_081-matnr        = wa_saida_0110-matnr.
    wl_081-asnum        = wa_saida_0110-asnum.
    wl_081-sgtxt        = wa_saida_0110-sgtxt.
    wl_081-id_lms       = wa_saida_0110-id_lms.

    IF ( zglt080-zlsch = 'E'  ) OR ( zglt080-zlsch = 'D' ).  "Boleto
      wl_081-cod_barras   = wa_saida_0110-cod_barras.
    ENDIF.

    wl_081-objkey       = wa_saida_0110-objkey.
    wl_081-bnfpo        = wa_saida_0110-bnfpo.
    wl_081-sem_nf       = wl_080-sem_nf.
    IF ( wl_081-menge > 0 ) AND ( wl_081-netwr > 0 ).
      wl_081-netpr      =  wl_081-netwr / wl_081-menge.
    ENDIF.

    IF wl_081-matnr IS NOT INITIAL.
      wl_081-cprod = wl_081-matnr.

      CLEAR: wl_mara.
      SELECT SINGLE *
        FROM mara INTO wl_mara
       WHERE matnr = wl_081-matnr.

      wl_081-meins = wl_mara-meins.

    ELSEIF wl_081-asnum IS NOT INITIAL.
      wl_081-cprod = wl_081-asnum.

      CLEAR: wl_asmd.
      SELECT SINGLE *
        FROM asmd INTO wl_asmd
       WHERE asnum = wl_081-asnum.

      wl_081-meins = wl_asmd-meins.

    ENDIF.

    "Inicio Processamento caso Lançamento Possuir NF
    IF ( zglt080-sem_nf  IS INITIAL ) AND
       ( wl_081-desconto IS INITIAL ). " Não for partida de Desconto de Fornecedor

      "Busca Parâmetro Geração NF.
      PERFORM f_get_parametro USING vl_idx_str
                                    wl_080
                                    wl_081
                           CHANGING wl_082
                                    vl_valid.
      IF vl_valid IS INITIAL.
        RETURN.
      ENDIF.

      "Leitura de Categoria NF.
      SELECT SINGLE * INTO wl_j1baa
        FROM j_1baa
       WHERE nftype EQ wl_082-nftype.

      "Busca Origem Destino.
      SELECT SINGLE *
        FROM t001w INTO wl_t001w
       WHERE werks EQ wl_081-gsber_nf.

      "Busca Fornecedor
      SELECT SINGLE *
        FROM lfa1 INTO wl_lfa1
       WHERE lifnr EQ wl_080-lifnr.

      "Busca Filial
      MOVE: wl_lfa1-regio  TO wl_shipfrom.
      MOVE: wl_t001w-regio TO wl_shipto.

      wl_081-cfop     = wl_082-cfop.
      wl_081-itmtyp   = wl_082-itmtyp.
      wl_081-operacao = wl_082-operacao.

      LOOP AT tg_0110_agr WHERE nfenum = wa_saida_0110-nfenum
                            AND series = wa_saida_0110-series.

        tg_0110_agr-nftype = wl_082-nftype.
        MODIFY tg_0110_agr.

      ENDLOOP.

      "Busca de Impostos para o Item/NF
      PERFORM f_monta_impostos TABLES gt_impostos
                                USING wl_j1baa
                                      wl_080      "Cabeçalho
                                      wl_081      "Itens
                                      wl_082      "Parâmetros
                                      wl_shipfrom
                                      wl_shipto.

      IF gt_impostos[] IS INITIAL.
        MESSAGE s836(sd) WITH TEXT-e59 'Seq.Item' TEXT-e16 vl_idx_str DISPLAY LIKE 'S'.
        CONCATENATE TEXT-e59 'Seq.Item' TEXT-e16 vl_idx_str INTO vg_message SEPARATED BY space.
        PERFORM f_log_gravacao USING 'E' vg_message '' ''.
        RETURN.
      ENDIF.

      LOOP AT gt_impostos.
        CLEAR: wl_085.
        MOVE: wl_080-seq_lcto        TO wl_085-seq_lcto,
              wl_081-seqitem         TO wl_085-seqitem,
              gt_impostos-taxtyp     TO wl_085-taxtyp,
              gt_impostos-base       TO wl_085-base,
              gt_impostos-rate       TO wl_085-rate,
              gt_impostos-taxval     TO wl_085-taxval,
              gt_impostos-excbas     TO wl_085-excbas,
              gt_impostos-othbas     TO wl_085-othbas.

        APPEND wl_085 TO gt_085.
      ENDLOOP.

      "---------------------------------------------------------*
      " Gravar Parceiros
      "---------------------------------------------------------*

      "Fornecedor.
      CLEAR: wl_083.
      MOVE: wl_080-seq_lcto        TO wl_083-seq_lcto,
            wl_081-seqitem         TO wl_083-seqitem,
            'LF'                   TO wl_083-parvw,
            'V'                    TO wl_083-partyp,
            wl_080-lifnr           TO wl_083-parid.
      APPEND wl_083 TO gt_083.

      "Filial
      CLEAR: wl_083.
      MOVE: wl_080-seq_lcto        TO wl_083-seq_lcto,
            wl_081-seqitem         TO wl_083-seqitem,
            'BR'                   TO wl_083-parvw,
            'B'                    TO wl_083-partyp,
            wl_081-gsber_nf        TO wl_083-parid.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_083-parid
        IMPORTING
          output = wl_083-parid.

      APPEND wl_083 TO gt_083.

    ENDIF.

    "Aplica Validações no item do Lançamento
    PERFORM f_valida_item USING wl_081
                                wl_j1baa
                                wl_shipto
                                wl_shipfrom
                                vl_idx_str
                                p_validar_lcto
                       CHANGING vl_valid.

    IF vl_valid IS INITIAL.
      RETURN.
    ENDIF.

    APPEND wl_081 TO gt_081.

    ADD 1 TO vl_idx_item.
  ENDLOOP.

*-----------------------------------------------------------------------*
*  Validação Notas Fiscais
*-----------------------------------------------------------------------*
  IF ( vg_opr_lcto NE c_display ) AND ( zglt080-sem_nf IS INITIAL ).
    LOOP AT tg_0110_agr.

      CLEAR: vl_xblnr, vl_vlr_nf.
      CONCATENATE tg_0110_agr-nfenum '-' tg_0110_agr-series INTO vl_xblnr.

      "Validação Doc. NF Lançado
      CLEAR: gt_081_aux[].
      SELECT a~seq_lcto a~nfenum a~series a~docnum
        FROM zglt081 AS a INNER JOIN zglt080 AS b ON a~seq_lcto = b~seq_lcto
        INTO CORRESPONDING FIELDS OF TABLE  gt_081_aux[]
      WHERE b~lifnr    EQ zglt080-lifnr
        AND a~nfenum   EQ tg_0110_agr-nfenum
        AND a~series   EQ tg_0110_agr-series
        AND a~seq_lcto NE zglt080-seq_lcto
        AND b~loekz    EQ '' "Não excluído
        AND b~sem_nf   EQ ''."Não é somente Lcto Contabil.

      IF NOT ( gt_081_aux[] IS INITIAL ).
        CLEAR: tg_j_1bnfdoc[].
        SELECT docnum nftype doctyp direct docdat cancel nfenum series parid
          FROM j_1bnfdoc INTO CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
          FOR ALL ENTRIES IN gt_081_aux
        WHERE docnum EQ gt_081_aux-docnum
          AND cancel NE 'X'.

        IF NOT ( tg_j_1bnfdoc[] IS INITIAL ) .
          READ TABLE tg_j_1bnfdoc INDEX 1.
          IF tg_j_1bnfdoc-docnum NE 0.
            ROLLBACK WORK.
            CONCATENATE 'Para NF. informada já tem o registro fiscal nro'
                         tg_j_1bnfdoc-docnum
                         ',verificar com a Área Fiscal'
                        '( NF/Série:' vl_xblnr ')' INTO vg_message SEPARATED BY space.

            MESSAGE vg_message TYPE 'S'.
            PERFORM f_log_gravacao USING 'E' vg_message '' ''.
            RETURN.
          ENDIF.

        ELSE.
          READ TABLE gt_081_aux INTO wl_081_aux INDEX 1.
          SELECT SINGLE *
            FROM zglt080 INTO wl_080_aux
           WHERE seq_lcto = wl_081_aux-seq_lcto.

          IF sy-subrc = 0.
            ROLLBACK WORK.
            CONCATENATE 'Para NF. informada já tem o Seq. Lançamento de Nro:'
                         wl_080_aux-seq_lcto '( NF/Série:' vl_xblnr ')' INTO vg_message SEPARATED BY space.
            MESSAGE vg_message TYPE 'S'.
            PERFORM f_log_gravacao USING 'E' vg_message '' ''.
            RETURN.
          ENDIF.
        ENDIF.

      ENDIF.
      "Fim Validação Lançamento

      vl_vlr_nf = tg_0110_agr-netwr.

      CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
        EXPORTING
          p_lifnr          = zglt080-lifnr
          p_parvw          = 'LF'
          p_nftype         = tg_0110_agr-nftype
          p_xblnr          = vl_xblnr
          p_data           = tg_0110_agr-bldat
          p_werks          = tg_0110_agr-gsber_nf
          p_valor_nf       = vl_vlr_nf
          p_ck_somente_dup = abap_true
        EXCEPTIONS
          error            = 1
          OTHERS           = 2.
      IF NOT sy-subrc IS INITIAL.
        ROLLBACK WORK.
        CONCATENATE sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 '( NF/Série:' vl_xblnr ')' INTO vg_message SEPARATED BY space.
        CONCATENATE sy-msgv4 '( NF/Série:' vl_xblnr ')' INTO sy-msgv4.
        MESSAGE s836(sd) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
        PERFORM f_log_gravacao USING 'E' vg_message '' ''.
        RETURN.
      ENDIF.

    ENDLOOP.
  ENDIF.

*-----------------------------------------------------------------------*
* Lança Partida Fornecedor
*-----------------------------------------------------------------------*
  IF wl_080-agrp_forn IS INITIAL.

    "Lançar Partida Fornecedor por NF
    LOOP AT tg_0110_agr.
      CLEAR: wl_081, vl_xblnr, vl_item_forn, vl_vlr_cbar, vl_vlr_cbar_01.
      CONCATENATE tg_0110_agr-nfenum '-' tg_0110_agr-series INTO vl_xblnr.
      wl_081-agrp_nf      = tg_0110_agr-agrp_nf.
      wl_081-part_forn    = 'X'.
      wl_081-seq_lcto     = wl_080-seq_lcto.
      wl_081-seqitem      = vl_idx_item.
      wl_081-lote         = wl_080-lote.
      wl_081-bukrs        = wl_080-bukrs.
      wl_081-bschl        = '31'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_080-lifnr
        IMPORTING
          output = wl_081-hkont.

      wl_081-gsber  = tg_0110_agr-gsber. "Filial NF

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_081-gsber
        IMPORTING
          output = wl_081-gsber.

      wl_081-netwr       = tg_0110_agr-netwr.

      IF vl_lcto_irf IS NOT INITIAL. "É lançamento com IRF
        wl_081-vlr_liq_ret = tg_0110_agr-vlr_liq_ret.
      ENDIF.

      "Verificar se possui descontos para a NF.
      CLEAR: gt_081_aux[].
      gt_081_aux[] = gt_081[].
      LOOP AT gt_081_aux INTO wl_081_aux WHERE nfenum = tg_0110_agr-nfenum
                                           AND series = tg_0110_agr-series
                                           AND desconto IS NOT INITIAL.
        SUBTRACT wl_081_aux-netwr FROM wl_081-netwr.

        IF vl_lcto_irf IS NOT INITIAL. "É lançamento com IRF
          SUBTRACT wl_081_aux-netwr FROM wl_081-vlr_liq_ret.
        ENDIF.
      ENDLOOP.

      IF ( zglt080-zlsch = 'E'  ) OR ( zglt080-zlsch = 'D' ).  "Boleto
        wl_081-cod_barras   = tg_0110_agr-cod_barras.
        IF strlen( tg_0110_agr-cod_barras ) = 47.
          vl_vlr_cbar_01 = tg_0110_agr-cod_barras+37(10).
        ELSEIF strlen( tg_0110_agr-cod_barras ) = 48.
          CONCATENATE tg_0110_agr-cod_barras+5(6) tg_0110_agr-cod_barras+12(4) INTO vl_vlr_cbar_01.
        ENDIF.

        IF strlen( tg_0110_agr-cod_barras ) = 47.
          wl_081-esrnr = tg_0110_agr-cod_barras(10).
          wl_081-esrre = tg_0110_agr-cod_barras+10(27).
        ENDIF.

        IF vl_lcto_irf IS INITIAL. "Não é lançamento com IRF
          vl_cbar_str = wl_081-netwr.
        ELSE.
          vl_cbar_str = wl_081-vlr_liq_ret.
        ENDIF.

        PERFORM f_tratar_campo CHANGING vl_cbar_str.
        vl_vlr_cbar = vl_cbar_str(10).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vl_vlr_cbar
          IMPORTING
            output = vl_vlr_cbar.

        IF ( vl_vlr_cbar_01 NE  vl_vlr_cbar ).
          ROLLBACK WORK.
          CONCATENATE 'Valor do código de Barras é diferente do valor para pagamento do fornecedor'
                      '( NF/Série:' vl_xblnr ')' INTO vg_message SEPARATED BY space.
          MESSAGE vg_message TYPE 'S'.
          PERFORM f_log_gravacao USING 'E' vg_message '' ''.
          RETURN.
        ENDIF.

      ENDIF.

      wl_081-ref_doc_no = vl_xblnr.
      wl_081-sgtxt      = wl_080-bktxt.
      wl_081-sem_nf     = wl_080-sem_nf.

      APPEND wl_081 TO gt_081.

      vl_item_forn = vl_idx_item. "Armazena Item Fornecedor.

      ADD 1 TO vl_idx_item.

      "Lançamento partidas contas IRF
      IF vl_lcto_irf IS NOT INITIAL.
        LOOP AT tg_irf WHERE nfenum    = tg_0110_agr-nfenum
                         AND series    = tg_0110_agr-series.

          CLEAR: wl_081, wl_088, wl_089, vl_xblnr.

          "Gravar Linha IRF referente a Partida Fornecedor
          wl_088-seq_lcto      = wl_080-seq_lcto.
          wl_088-seqitem       = vl_item_forn.
          wl_088-witht         = tg_irf-witht.
          wl_088-wt_withcd     = tg_irf-wt_withcd.
          wl_088-wi_tax_base   = tg_irf-wi_tax_base.
          wl_088-wi_tax_amt    = tg_irf-wi_tax_amt.
          wl_088-hkont         = tg_irf-hkont.
          wl_088-wt_basman     = tg_irf-wt_basman.
          wl_088-wt_taxman     = tg_irf-wt_taxman.

          IF vg_call_ext IS NOT INITIAL. "Zerar Bases
            wl_088-wi_tax_base   = 0.
            wl_088-wi_tax_amt    = 0.
            wl_088-wt_basman     = 'X'.
            wl_088-wt_taxman     = 'X'.
          ENDIF.

          APPEND wl_088 TO gt_088.

          "Gravar Linha IRF referente Resumo NF
          wl_089-seq_lcto      = wl_080-seq_lcto.
          wl_089-gsber         = tg_irf-gsber.
          wl_089-nfenum        = tg_irf-nfenum.
          wl_089-series        = tg_irf-series.
          wl_089-witht         = tg_irf-witht.
          wl_089-wt_withcd     = tg_irf-wt_withcd.
          wl_089-wi_tax_base   = tg_irf-wi_tax_base.
          wl_089-wi_tax_amt    = tg_irf-wi_tax_amt.
          wl_089-hkont         = tg_irf-hkont.
          wl_089-wt_basman     = tg_irf-wt_basman.
          wl_089-wt_taxman     = tg_irf-wt_taxman.
          wl_089-bas_neg       = tg_irf-bas_neg.

          IF vg_call_ext IS NOT INITIAL. "Zerar Bases
            wl_089-wi_tax_base   = 0.
            wl_089-wi_tax_amt    = 0.
            wl_089-wt_basman     = 'X'.
            wl_089-wt_taxman     = 'X'.
          ENDIF.

          APPEND wl_089 TO gt_089.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ELSE. "Grava Somente uma Partida Fornecedor

    "Busca Filial para Lcto de Partida de Forncedor Agrupada
    PERFORM f_get_filial_forn TABLES it_saida_0110
                            CHANGING vl_gsber_forn.

    CLEAR: wl_081, vl_item_forn, vl_xblnr, gt_081_aux[], vl_vlr_cbar, vl_vlr_cbar_01.

    gt_081_aux[] = gt_081[].

    IF wl_080-nf_unica IS NOT INITIAL.
      LOOP AT gt_081_aux INTO wl_081_aux WHERE nfenum IS NOT INITIAL
                                           AND series IS NOT INITIAL.
        CONCATENATE wl_081_aux-nfenum '-' wl_081_aux-series INTO vl_xblnr.
        EXIT.
      ENDLOOP.
    ELSE.
      vl_xblnr = wl_080-ref_doc_no.
    ENDIF.

    wl_081-part_forn    = 'X'.
    wl_081-seq_lcto     = wl_080-seq_lcto.
    wl_081-seqitem      = vl_idx_item.
    wl_081-lote         = wl_080-lote.
    wl_081-bukrs        = wl_080-bukrs.
    wl_081-bschl        = '31'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_080-lifnr
      IMPORTING
        output = wl_081-hkont.

    wl_081-gsber      = vl_gsber_forn.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_081-gsber
      IMPORTING
        output = wl_081-gsber.

    wl_081-netwr       = zglt080-netwr - wl_080-vlr_desconto.

    IF vl_lcto_irf IS NOT INITIAL. "É lançamento com IRF
      wl_081-vlr_liq_ret = zglt080-vlr_liq_ret - wl_080-vlr_desconto..
    ENDIF.

    IF ( zglt080-zlsch = 'E'  ) OR ( zglt080-zlsch = 'D' ).  "Boleto
      wl_081-cod_barras   = wl_080-cod_barras.

      IF strlen( wl_080-cod_barras ) = 47.
        vl_vlr_cbar_01 = wl_080-cod_barras+37(10).
      ELSEIF strlen( wl_080-cod_barras ) = 48.
        CONCATENATE wl_080-cod_barras+5(6) wl_080-cod_barras+12(4) INTO vl_vlr_cbar_01.
      ENDIF.

      IF strlen( tg_0110_agr-cod_barras ) = 47.
        wl_081-esrnr = wl_080-cod_barras(10).
        wl_081-esrre = wl_080-cod_barras+10(27).
      ENDIF.

      IF vl_lcto_irf IS INITIAL. "Não é lançamento com IRF
        vl_cbar_str = wl_081-netwr.
      ELSE.
        vl_cbar_str = wl_081-vlr_liq_ret.
      ENDIF.

      PERFORM f_tratar_campo CHANGING vl_cbar_str.
      vl_vlr_cbar = vl_cbar_str(10).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_vlr_cbar
        IMPORTING
          output = vl_vlr_cbar.

      IF ( vl_vlr_cbar_01 NE vl_vlr_cbar ).
        ROLLBACK WORK.
        CONCATENATE 'Valor do código de Barras é diferente do valor para pagamento do fornecedor!'
                    '( Partida Agrupada )' INTO vg_message SEPARATED BY space.
        MESSAGE vg_message TYPE 'S'.
        PERFORM f_log_gravacao USING 'E' vg_message '' ''.
        RETURN.
      ENDIF.

    ENDIF.

    wl_081-ref_doc_no = vl_xblnr.
    wl_081-sgtxt      = wl_080-bktxt.
    wl_081-sem_nf     = wl_080-sem_nf.

    APPEND wl_081 TO gt_081.

    vl_item_forn = vl_idx_item. "Armazena Item Fornecedor.

    ADD 1 TO vl_idx_item.

    "Lançamento partidas contas IRF
    IF vl_lcto_irf IS NOT INITIAL.

      PERFORM f_monta_agrp_irf TABLES tg_irf
                                      tg_irf_agr.

      LOOP AT tg_irf_agr.
        CLEAR: wl_081, wl_088, vl_xblnr.

        "Gravar Linha IRF referente a Partida Fornecedor
        wl_088-seq_lcto      = wl_080-seq_lcto.
        wl_088-seqitem       = vl_item_forn.
        wl_088-witht         = tg_irf_agr-witht.
        wl_088-wt_withcd     = tg_irf_agr-wt_withcd.
        wl_088-wi_tax_base   = tg_irf_agr-wi_tax_base.
        wl_088-wi_tax_amt    = tg_irf_agr-wi_tax_amt.
        wl_088-hkont         = tg_irf_agr-hkont.
        wl_088-wt_basman     = 'X'.
        wl_088-wt_taxman     = 'X'.

        IF vg_call_ext IS NOT INITIAL. "Zerar Bases
          wl_088-wi_tax_base   = 0.
          wl_088-wi_tax_amt    = 0.
          wl_088-wt_basman     = 'X'.
          wl_088-wt_taxman     = 'X'.
        ENDIF.

        APPEND wl_088 TO gt_088.

      ENDLOOP.

      LOOP AT tg_irf.
        CLEAR: wl_089.
        "Gravar Linha IRF referente Resumo NF
        wl_089-seq_lcto      = wl_080-seq_lcto.
        wl_089-gsber         = tg_irf-gsber.
        wl_089-nfenum        = tg_irf-nfenum.
        wl_089-series        = tg_irf-series.
        wl_089-witht         = tg_irf-witht.
        wl_089-wt_withcd     = tg_irf-wt_withcd.
        wl_089-wi_tax_base   = tg_irf-wi_tax_base.
        wl_089-wi_tax_amt    = tg_irf-wi_tax_amt.
        wl_089-hkont         = tg_irf-hkont.
        wl_089-wt_basman     = tg_irf-wt_basman.
        wl_089-wt_taxman     = tg_irf-wt_taxman.
        wl_089-bas_neg       = tg_irf-bas_neg.

        IF vg_call_ext IS NOT INITIAL. "Zerar Bases
          wl_089-wi_tax_base   = 0.
          wl_089-wi_tax_amt    = 0.
          wl_089-wt_basman     = 'X'.
          wl_089-wt_taxman     = 'X'.
        ENDIF.

        APPEND wl_089 TO gt_089.
      ENDLOOP.

    ENDIF.

  ENDIF.

  IF p_validar_lcto IS INITIAL.
    MODIFY zglt080 FROM wl_080.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e08 TEXT-e09 DISPLAY LIKE 'E'.
      PERFORM f_log_gravacao USING 'E' TEXT-e08 TEXT-e09 ''.
      RETURN.
    ENDIF.

    MODIFY zglt081 FROM TABLE gt_081.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e08 TEXT-e10 DISPLAY LIKE 'E'.
      PERFORM f_log_gravacao USING 'E' TEXT-e08 TEXT-e10 ''.
      RETURN.
    ENDIF.

    MODIFY zglt083 FROM TABLE gt_083.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e08 TEXT-e44 DISPLAY LIKE 'E'.
      PERFORM f_log_gravacao USING 'E' TEXT-e08 TEXT-e44 ''.
      RETURN.
    ENDIF.

    MODIFY zglt085 FROM TABLE gt_085.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e08 TEXT-e42 DISPLAY LIKE 'E'.
      PERFORM f_log_gravacao USING 'E' TEXT-e08 TEXT-e42 ''.
      RETURN.
    ENDIF.

    MODIFY zglt088 FROM TABLE gt_088.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e08 TEXT-e51 DISPLAY LIKE 'E'.
      PERFORM f_log_gravacao USING 'E' TEXT-e08 TEXT-e51 ''.
      RETURN.
    ENDIF.

    MODIFY zglt089 FROM TABLE gt_089.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e08 TEXT-e51 DISPLAY LIKE 'E'.
      PERFORM f_log_gravacao USING 'E' TEXT-e08 TEXT-e51 ''.
      RETURN.
    ENDIF.

  ENDIF.

  IF p_validar_lcto IS NOT INITIAL. "Modo somente validação de Erros
    MESSAGE s836(sd) WITH TEXT-s01 DISPLAY LIKE 'S'.
  ELSE.
    COMMIT WORK.
    MESSAGE s836(sd) WITH TEXT-s02 DISPLAY LIKE 'S'.
    CONCATENATE TEXT-s02 'Nro Seq. Lcto:' wl_080-seq_lcto INTO vg_message SEPARATED BY space.
    vl_seqlcto_ger = wl_080-seq_lcto.
    PERFORM f_log_gravacao USING 'S' vg_message '' vl_seqlcto_ger.

    IF vg_call_ext IS NOT INITIAL. "Chamada Externa
      CLEAR: wa_saida_0100.
      MOVE-CORRESPONDING wl_080 TO wa_saida_0100.
      PERFORM f_gerar_ctb_lcto USING wa_saida_0100.
    ELSE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Deseja gerar documento contábil para o Lançamento?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF var_answer EQ '1'.
        CLEAR: wa_saida_0100.
        MOVE-CORRESPONDING wl_080 TO wa_saida_0100.
        PERFORM f_gerar_ctb_lcto USING wa_saida_0100.

        PERFORM f_renew_consulta USING '0100'.
      ENDIF.

      LEAVE TO SCREEN 0.

    ENDIF.
  ENDIF.


ENDFORM.

FORM f_exit_lcto .

  IF vg_opr_lcto NE c_display.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente sair do Lançamento?'
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
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.

FORM f_valida_item  USING    p_zglt081   TYPE zglt081
                             p_j1baa     TYPE j_1baa
                             p_shipto    TYPE lfa1-regio
                             p_shipfrom  TYPE lfa1-regio
                             p_idx       TYPE string
                             p_validacao
                    CHANGING p_valid.

  DATA: wl_tka02    TYPE tka02,
        wl_cskb     TYPE cskb,
        wl_skb1     TYPE skb1,
        wl_tbsl     TYPE tbsl,
        wl_asmd     TYPE asmd,
        wl_mara     TYPE mara,
        wl_csks     TYPE csks,
        wl_bbranch  TYPE j_1bbranch,
        vl_valid_dt TYPE c,
        v_aufnr     TYPE aufk-aufnr,
        vl_idx      TYPE string.

  CLEAR: p_valid, wl_tka02, wl_cskb, wl_skb1, wl_tbsl.

  vl_idx = p_idx.

  IF ( p_zglt081-seq_lcto IS INITIAL ) AND ( p_validacao IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Seq.Lcto' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Seq.Lcto' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-seqitem IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Seq.Item' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Seq.Item' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-lote IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Lote' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Lote' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-bukrs IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Empresa' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Empresa' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-bschl IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Chv.Lcto' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Chv.Lcto' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-hkont IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Conta' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Conta' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-hkont EQ zglt080-lifnr ).
    MESSAGE s836(sd) WITH TEXT-e40 TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e40 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-gsber IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Filial' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Filial' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-gsber_nf IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e58 TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e58 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-kostl IS INITIAL ) AND
         ( p_zglt081-hkont NE zglt080-lifnr ).
    MESSAGE s836(sd) WITH TEXT-e15 'C.Custo' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'C.Custo' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-budat IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Data Lcto' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Data Lcto' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-bldat IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Data Emissão' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Data Emissão' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-bldat > p_zglt081-budat ).
    MESSAGE s836(sd) WITH TEXT-e71 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e71 p_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-nfenum IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Nro. NF' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Nro. NF' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-series IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Série' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Série' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-menge IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Quantidade' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Quantidade' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-netwr IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Valor R$' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Valor R$' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-netpr IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Preço' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Preço' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( ( p_zglt081-matnr IS INITIAL ) AND ( p_zglt081-asnum IS INITIAL  ) ) AND
         ( p_zglt081-hkont NE zglt080-lifnr ).
    MESSAGE s836(sd) WITH TEXT-e15 'Material/Serviço' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Material/Serviço' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( ( p_zglt081-matnr IS NOT INITIAL ) AND ( p_zglt081-asnum IS NOT INITIAL  ) ).
    MESSAGE s836(sd) WITH TEXT-e43 'Material/Serviço' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e43 'Material/Serviço' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-cprod IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Produto' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Produto' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ENDIF.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = p_zglt081-bldat
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    MESSAGE s836(sd) WITH TEXT-e15 'Data Emissão' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Data Emissão' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ENDIF.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = p_zglt081-budat
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    MESSAGE s836(sd) WITH TEXT-e15 'Data Lcto' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Data Lcto' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ENDIF.

  IF vg_call_ext IS INITIAL. "Não for chamada externa
    IF p_zglt081-vbeln IS NOT INITIAL.
      CLEAR: v_aufnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = p_zglt081-vbeln
        IMPORTING
          output = v_aufnr.

      SELECT SINGLE *
        FROM aufk INTO @DATA(_wl_aufk)
       WHERE bukrs = @p_zglt081-bukrs
         AND aufnr = @v_aufnr.

      IF sy-subrc NE 0.
        MESSAGE s836(sd) WITH TEXT-e69 v_aufnr TEXT-e16 p_idx DISPLAY LIKE 'S'.
        CONCATENATE TEXT-e69 v_aufnr TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
        PERFORM f_log_gravacao USING 'E' vg_message '' ''.
        RETURN.
      ENDIF.

      IF ( p_zglt081-kostl IS NOT INITIAL ) AND ( p_zglt081-kostl NE _wl_aufk-kostv ).
        MESSAGE s836(sd) WITH TEXT-e70 TEXT-e16 p_idx DISPLAY LIKE 'S'.
        CONCATENATE TEXT-e70 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
        PERFORM f_log_gravacao USING 'E' vg_message '' ''.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ( zglt080-zlsch = 'E'  ) OR ( zglt080-zlsch = 'D' ). " Se for Boleto

    "Validar Códigos de Barras.
    IF p_zglt081-cod_barras IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e62 TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e62 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.
    ENDIF.

    DATA(_tam) = strlen( p_zglt081-cod_barras ).

    CONDENSE p_zglt081-cod_barras NO-GAPS.

    DATA(_tam_01) = strlen( p_zglt081-cod_barras ).

    IF ( _tam NE _tam_01 ).
      MESSAGE s836(sd) WITH TEXT-e66 TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e66 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.
    ENDIF.

    "Validar Tamanho Cod. de Barras.
    IF ( strlen( p_zglt081-cod_barras ) NE 48 ) AND ( strlen( p_zglt081-cod_barras ) NE 47 ).
      MESSAGE s836(sd) WITH TEXT-e63 TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e63 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.
    ENDIF.

    LOOP AT it_saida_0110 INTO DATA(wl_0110_aux) WHERE nfenum     = p_zglt081-nfenum
                                                   AND series     = p_zglt081-series
                                                   AND cod_barras NE p_zglt081-cod_barras.

      IF wl_0110_aux-cod_barras IS NOT INITIAL .
        CONCATENATE p_zglt081-nfenum '-' p_zglt081-series INTO DATA(_nf_serie_aux).
        MESSAGE s836(sd) WITH TEXT-e64 TEXT-e65 _nf_serie_aux DISPLAY LIKE 'S'.
        CONCATENATE TEXT-e64 TEXT-e65 _nf_serie_aux INTO vg_message SEPARATED BY space.
        PERFORM f_log_gravacao USING 'E' vg_message '' ''.
        RETURN.
      ENDIF.
    ENDLOOP.


  ENDIF.

  "Valida Código Serviço
  IF p_zglt081-asnum IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_zglt081-asnum
      IMPORTING
        output = p_zglt081-asnum.

    SELECT SINGLE *
      FROM asmd INTO wl_asmd
     WHERE asnum = p_zglt081-asnum.

    IF sy-subrc NE 0.
      MESSAGE s836(sd) WITH TEXT-e27 TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e27 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.
    ENDIF.
  ENDIF.

  "Valida Código Material
  IF p_zglt081-matnr IS NOT INITIAL.

*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = p_zglt081-matnr
*      IMPORTING
*        output = p_zglt081-matnr.

    SELECT SINGLE *
      FROM mara INTO wl_mara
     WHERE matnr = p_zglt081-matnr.

    IF sy-subrc NE 0.
      MESSAGE s836(sd) WITH TEXT-e28 TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e28 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.
    ENDIF.
  ENDIF.

  IF ( p_zglt081-meins IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e15 'Un.Medida' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e15 'Un.Medida' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_shipto IS INITIAL ).

    IF ( zglt080-sem_nf IS INITIAL ) AND ( p_zglt081-desconto IS INITIAL ).
      MESSAGE s836(sd) WITH TEXT-e41 'Dest. Merc.' TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e41 'Dest. Merc.' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.
    ENDIF.

  ELSEIF ( p_shipfrom IS INITIAL ).

    IF ( zglt080-sem_nf IS INITIAL ) AND ( p_zglt081-desconto IS INITIAL ).
      MESSAGE s836(sd) WITH TEXT-e41 'Origem Merc.' TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e41 'Origem Merc.' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.
    ENDIF.

  ELSEIF ( p_zglt081-agrp_nf IS INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e41 'Agrp.NF' TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e41 'Agrp.NF' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ELSEIF ( p_zglt081-cfop IS INITIAL ).

    IF ( zglt080-sem_nf IS INITIAL ) AND ( p_zglt081-desconto IS INITIAL ).

      MESSAGE s836(sd) WITH TEXT-e41 'CFOP' TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e41 'CFOP' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.

    ENDIF.

  ELSEIF ( p_zglt081-itmtyp IS INITIAL ).

    IF ( zglt080-sem_nf IS INITIAL ) AND ( p_zglt081-desconto IS INITIAL ).

      MESSAGE s836(sd) WITH TEXT-e41 'Tp.Item NF' TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e41 'Tp.Item NF' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.

    ENDIF.

  ELSEIF ( p_zglt081-operacao IS INITIAL ).

    IF ( zglt080-sem_nf IS INITIAL ) AND ( p_zglt081-desconto IS INITIAL ).

      MESSAGE s836(sd) WITH TEXT-e41 'Cod.Operação' TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e41 'Cod.Operação' TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.

    ENDIF.

  ENDIF.

  "Controle data de lançamento Fiscal
  PERFORM f_valida_data USING p_zglt081-budat
                              '1'
                              p_idx
                     CHANGING vl_valid_dt.

  CHECK vl_valid_dt IS NOT INITIAL.

  PERFORM f_valida_periodo USING p_zglt081-bukrs
                                 p_zglt081-budat
                        CHANGING vl_valid_dt.

  CHECK vl_valid_dt IS NOT INITIAL.

  "Valida Filial
  SELECT SINGLE *
    FROM j_1bbranch INTO wl_bbranch
   WHERE bukrs  EQ p_zglt081-bukrs
     AND branch EQ p_zglt081-gsber.

  IF sy-subrc NE 0.
    MESSAGE s836(sd) WITH TEXT-e30 TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e30 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ENDIF.

  "Valida Filial NF
  SELECT SINGLE *
    FROM j_1bbranch INTO wl_bbranch
   WHERE bukrs  EQ p_zglt081-bukrs
     AND branch EQ p_zglt081-gsber_nf.

  IF sy-subrc NE 0.
    MESSAGE s836(sd) WITH TEXT-e58 TEXT-e16 p_idx DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e58 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ENDIF.


  "Valida C.Custo
  IF p_zglt081-kostl IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_zglt081-kostl
      IMPORTING
        output = p_zglt081-kostl.

    SELECT SINGLE *
      FROM csks INTO wl_csks
     WHERE bukrs  = p_zglt081-bukrs
       AND gsber  = p_zglt081-gsber
       AND kostl  = p_zglt081-kostl
       AND datab  LE sy-datum
       AND datbi  GE sy-datum.

    IF sy-subrc NE 0.
      MESSAGE s836(sd) WITH TEXT-e29 TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e29 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.
    ENDIF.

    IF wl_csks-bkzkp = 'X'. "Centro de custo bloqueado para lançamentos primários
      MESSAGE s836(sd) WITH TEXT-e57 TEXT-e16 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e57 TEXT-e16 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.
    ENDIF.

  ENDIF.

  IF p_zglt081-kostl IS NOT INITIAL.

    "Se  a conta for  classe de custo  preencher ZGLT036-SEQSUB = 000001
    SELECT SINGLE *
      FROM tbsl INTO wl_tbsl
     WHERE bschl EQ p_zglt081-bschl.

    SELECT SINGLE *
      FROM tka02 INTO wl_tka02
     WHERE bukrs  = p_zglt081-bukrs.

    SELECT SINGLE *
      FROM cskb INTO wl_cskb
     WHERE kokrs  = wl_tka02-kokrs
       AND kstar  = p_zglt081-hkont+0(10)
       AND datab  LE sy-datum
       AND datbi  GE sy-datum.

    IF '0200_0201_0202' CS p_zglt081-bukrs. "EUROPA
      SELECT SINGLE *
        FROM skb1 INTO wl_skb1
       WHERE bukrs = p_zglt081-bukrs
         AND saknr = p_zglt081-hkont+0(10)
         AND fstag   = 'YB09'.
    ENDIF.

    IF ( wl_cskb-kstar IS NOT INITIAL OR wl_skb1-fstag   = 'YB09' ) AND wl_tbsl-koart  = 'S'.
      p_zglt081-seqsub  = 1.
    ELSE.
      CLEAR: p_zglt081-kostl, p_zglt081-seqsub.
      MESSAGE s836(sd) WITH TEXT-e13 TEXT-e14 p_idx DISPLAY LIKE 'S'.
      CONCATENATE TEXT-e13 TEXT-e14 vl_idx INTO vg_message SEPARATED BY space.
      PERFORM f_log_gravacao USING 'E' vg_message '' ''.
      RETURN.
    ENDIF.

  ENDIF.

  p_valid = 'X'.

ENDFORM.

FORM f_valid_cabec  USING    p_zglt080 TYPE zglt080
                    CHANGING p_valid.


  DATA: wl_034      TYPE zglt034,
        wl_035      TYPE zglt035,
        wl_lfbk     TYPE lfbk,
        vl_valid_dt TYPE c.


  CLEAR: p_valid, wl_034, wl_035.

  IF ( p_zglt080-lote IS INITIAL ).
    MESSAGE s836(sd) WITH 'Lote' TEXT-e17 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' 'Lote' TEXT-e17 '' .
    RETURN.
  ELSE.

    SELECT SINGLE *
      FROM zglt034 INTO wl_034
     WHERE lote = p_zglt080-lote.

    IF ( sy-subrc NE 0 ).
      MESSAGE s836(sd) WITH TEXT-e20 DISPLAY LIKE 'S'.
      PERFORM f_log_gravacao USING 'E' TEXT-e20 '' ''.
      RETURN.
    ENDIF.

    IF vg_opr_lcto EQ c_new.
      SELECT SINGLE *
        FROM zglt035 INTO wl_035
       WHERE lote = p_zglt080-lote.

      IF ( sy-subrc EQ 0 ).
        MESSAGE s836(sd) WITH TEXT-e21 p_zglt080-lote DISPLAY LIKE 'S'.
        vg_message = p_zglt080-lote.
        PERFORM f_log_gravacao USING 'E' TEXT-e21 vg_message ''.
        RETURN.
      ENDIF.
    ENDIF.

  ENDIF.

  IF ( p_zglt080-bukrs IS INITIAL ).
    MESSAGE s836(sd) WITH 'Empresa' TEXT-e17 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' 'Empresa' TEXT-e17 ''.
    RETURN.
  ELSEIF ( p_zglt080-lifnr IS INITIAL ).
    MESSAGE s836(sd) WITH 'Fornecedor' TEXT-e17 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' 'Fornecedor' TEXT-e17 ''.
    RETURN.
  ELSEIF ( p_zglt080-hbkid IS INITIAL ).
    MESSAGE s836(sd) WITH 'Banco Empresa' TEXT-e17 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' 'Banco Empresa' TEXT-e17 ''.
    RETURN.
  ELSEIF ( p_zglt080-zlsch IS INITIAL ).
    MESSAGE s836(sd) WITH 'Forma Pgto.' TEXT-e17 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' 'Forma Pgto.' TEXT-e17 ''.
    RETURN.
  ELSEIF ( p_zglt080-zfbdt IS INITIAL ).
    MESSAGE s836(sd) WITH 'Dt.Vencimento' TEXT-e17 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' 'Dt.Vencimento' TEXT-e17 ''.
    RETURN.
  ELSEIF ( p_zglt080-zfbdt < sy-datum ).
    MESSAGE s836(sd) WITH TEXT-e33 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' TEXT-e33 '' ''.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM t012 INTO @DATA(wl_t012)
   WHERE bukrs = @p_zglt080-bukrs
     AND hbkid = @p_zglt080-hbkid.

  IF sy-subrc NE 0.
    MESSAGE s836(sd) WITH TEXT-e67 p_zglt080-hbkid TEXT-e68 p_zglt080-bukrs DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' TEXT-e33 '' ''.
    CONCATENATE TEXT-e67 p_zglt080-hbkid TEXT-e68 p_zglt080-bukrs INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ENDIF.

  "Controle data de lançamento Fiscal
  IF p_zglt080-budat IS NOT INITIAL.
    PERFORM f_valida_data USING p_zglt080-budat
                                '1'
                                ''
                       CHANGING vl_valid_dt.

    CHECK vl_valid_dt IS NOT INITIAL.
  ENDIF.

  "Valida Data Vencimento - Prazo 72 Hrs/ Dia Util.
  PERFORM f_valida_data USING p_zglt080-zfbdt
                              '2'
                              ''
                     CHANGING vl_valid_dt.

  CHECK vl_valid_dt IS NOT INITIAL.

*  IF ( P_ZGLT080-ZLSCH EQ 'D' ) AND ( P_ZGLT080-HBKID NE 'BBD' ).
*    MESSAGE S836(SD) WITH TEXT-E31 DISPLAY LIKE 'S'.
*    PERFORM F_LOG_GRAVACAO USING 'E' TEXT-E31 '' ''.
*    RETURN.
*  ENDIF.

  IF zglt080-bvtyp IS NOT INITIAL.
    SELECT SINGLE *
      FROM lfbk INTO wl_lfbk
     WHERE lifnr = zglt080-lifnr
       AND bvtyp = zglt080-bvtyp.

    IF sy-subrc NE 0.
      MESSAGE s836(sd) WITH TEXT-e26 DISPLAY LIKE 'S'.
      PERFORM f_log_gravacao USING 'E' TEXT-e26 '' ''.
      RETURN.
    ENDIF.
  ENDIF.

  p_valid = 'X'.

ENDFORM.

FORM f_retorna_status_zib USING p_doc_lcto
                                p_ano_lcto
                       CHANGING p_zibchv TYPE zib_contabil_chv
                                p_ziberr TYPE zib_contabil_err.

  DATA v_objkey TYPE char20.

  CLEAR: p_zibchv, p_ziberr.
  CONCATENATE 'ZGL17' p_doc_lcto p_ano_lcto INTO v_objkey.

  SELECT SINGLE *
    FROM zib_contabil_chv
    INTO p_zibchv
   WHERE obj_key = v_objkey.

  IF ( sy-subrc IS NOT INITIAL ).
    SELECT SINGLE *
      FROM zib_contabil_err
      INTO p_ziberr
     WHERE obj_key = v_objkey.
  ENDIF.

ENDFORM.


FORM f_gerar_ctb_lcto USING p_saida_0100 TYPE ty_saida_0100.

  DATA: vl_ger_ctb TYPE c,
        tg_0110    TYPE TABLE OF ty_saida_0110 WITH HEADER LINE.

  CHECK p_saida_0100 IS NOT INITIAL.

  SELECT *
    FROM zglt081 INTO CORRESPONDING FIELDS OF TABLE tg_0110
   WHERE seq_lcto = p_saida_0100-seq_lcto.

  PERFORM f_lock_lcto USING 'B' p_saida_0100-seq_lcto. "Bloqueia Lcto
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO vg_message.
    MESSAGE vg_message TYPE 'S'.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    EXIT.
  ENDIF.

  IF ( p_saida_0100-agrp_forn IS INITIAL ). " Gerar Ctb Por NF

    SORT tg_0110 BY nfenum series.
    DELETE ADJACENT DUPLICATES FROM tg_0110 COMPARING nfenum series.

    LOOP AT tg_0110 WHERE doc_lcto IS INITIAL.
      PERFORM f_gerar_ctb_nf USING tg_0110
                                   ''   "Liberar Lote
                                   'X'  "Gerar Ctb. por NF
                          CHANGING vl_ger_ctb.
    ENDLOOP.

    CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
      EXPORTING
        p_num_lote = p_saida_0100-lote.

  ELSE.

    READ TABLE tg_0110 INDEX 1.
    CHECK ( sy-subrc = 0 ) AND ( tg_0110-doc_lcto IS INITIAL ).

    PERFORM f_gerar_ctb_nf USING tg_0110
                                 'X'     "Liberar Lote
                                 ''      "Gerar Ctb. por NF
                        CHANGING vl_ger_ctb.
  ENDIF.

  PERFORM f_lock_lcto USING 'D' p_saida_0100-seq_lcto. "Desbloqueia Lcto


ENDFORM.

FORM f_gerar_ctb_sel.

  DATA: tg_lote_lib TYPE TABLE OF zglt034-lote WITH HEADER LINE,
        vl_valid_dt TYPE c,
        vl_ger_ctb  TYPE c.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente gerar o(s) documento(s) contábil(eis) para o(s) registro(s) selecionado(s)?'
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

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.
    CHECK sy-subrc = 0.


    "115706 CS2022000452 Bloquear Geração de Doc. Fiscal em Período Fechado - ZGL059 - PSA
    "Ajustada a regra para no click validar o usuario e periodo!

** RJF - ini - #77270 - CS2022000452 Bloquear Geração de Doc. Fiscal em Período Fechado - ZGL059
*    IF wa_saida_0100-budat IS NOT INITIAL.
*      PERFORM f_valida_data USING wa_saida_0100-budat
*                                  '1'
*                                  ''
*                         CHANGING vl_valid_dt.
*
*      CHECK vl_valid_dt IS NOT INITIAL.
*    ENDIF.
*
*    "Valida Data Vencimento - Prazo 72 Hrs/ Dia Util.
*    PERFORM f_valida_data USING wa_saida_0100-zfbdt
*                                '2'
*                                ''
*                       CHANGING vl_valid_dt.
*
*    CHECK vl_valid_dt IS NOT INITIAL.
** RJF - Fim - #77270 - CS2022000452 Bloquear Geração de Doc. Fiscal em Período Fechado - ZGL059

    PERFORM f_gerar_ctb_lcto USING wa_saida_0100.

  ENDLOOP.


  PERFORM f_renew_consulta USING '0100'.

ENDFORM.

FORM f_gerar_ctb_nf USING p_saida    TYPE ty_saida_0110
                          p_lib_lote TYPE c
                          p_ctb_nf   TYPE c
                 CHANGING p_gerado_ctb.

  DATA: wl_080         TYPE zglt080,
        tg_081         TYPE TABLE OF zglt081 WITH HEADER LINE,
        wl_034         TYPE zglt034,
        gt_zglt036     TYPE TABLE OF zglt036,
        wl_zglt036     TYPE zglt036,
        wl_zglt035     TYPE zglt035,
        vl_zuonr       TYPE string,
        vl_gsber       TYPE zglt036-gsber,
        vl_doc_lcto    TYPE zglt081-doc_lcto,
        vl_dt_lcto_ctb TYPE zglt081-dt_lcto_ctb,
        vl_seqitem     TYPE zglt036-seqitem,
        vl_valid_dt    TYPE c,
        v_aufnr        TYPE aufk-aufnr,
        vl_xblnr       TYPE zglt035-xblnr.

  CLEAR: wl_080, wl_034, tg_081[], wl_zglt035, gt_zglt036[], p_gerado_ctb.

  CHECK p_saida IS NOT INITIAL.

  CHECK ( p_saida-doc_lcto IS INITIAL ) AND ( p_saida-part_forn IS INITIAL ).

  SELECT SINGLE *
    FROM zglt080 INTO wl_080
   WHERE seq_lcto = p_saida-seq_lcto.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM zglt034 INTO wl_034
   WHERE lote = p_saida-lote.

  CHECK sy-subrc EQ 0.

  IF wl_034-status_lote EQ 'A'.
    MESSAGE s836(sd) WITH TEXT-e61 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' TEXT-e61 '' ''.
    RETURN.
  ENDIF.

  PERFORM f_valida_periodo USING p_saida-bukrs
                                 p_saida-budat
                        CHANGING vl_valid_dt.

  CHECK vl_valid_dt IS NOT INITIAL.


  IF p_ctb_nf IS NOT INITIAL.
    SELECT *
      FROM zglt081 INTO TABLE tg_081
     WHERE seq_lcto = p_saida-seq_lcto
       AND agrp_nf  = p_saida-agrp_nf.
  ELSE.
    SELECT *
      FROM zglt081 INTO TABLE tg_081
     WHERE seq_lcto = p_saida-seq_lcto.
  ENDIF.

  "Caso já tenha gerado, somente atualizar.
  CLEAR: vl_doc_lcto, vl_dt_lcto_ctb.
  LOOP AT tg_081 WHERE doc_lcto IS NOT INITIAL.
    vl_doc_lcto    = tg_081-doc_lcto.
    vl_dt_lcto_ctb = tg_081-dt_lcto_ctb.
    EXIT.
  ENDLOOP.

  IF vl_doc_lcto IS NOT INITIAL.
    IF p_ctb_nf IS NOT INITIAL.
      UPDATE zglt081 SET doc_lcto     = vl_doc_lcto
                         dt_lcto_ctb  = vl_dt_lcto_ctb
                         estorno_nf   = ''
       WHERE seq_lcto = p_saida-seq_lcto
         AND agrp_nf  = p_saida-agrp_nf.
      "AND PART_FORN = ''.
    ELSE.
      UPDATE zglt081 SET doc_lcto     = vl_doc_lcto
                         dt_lcto_ctb  = vl_dt_lcto_ctb
                         estorno_nf   = ''
       WHERE seq_lcto = p_saida-seq_lcto.
      "AND PART_FORN = ''.
    ENDIF.
    RETURN.
  ENDIF.

  CHECK tg_081[] IS NOT INITIAL.

  READ TABLE tg_081 WITH KEY part_forn = ''. "Leitura somente para pegar Dados Cabeçalho

  CLEAR: vl_xblnr.
  IF ( p_ctb_nf IS NOT INITIAL ) OR ( wl_080-nf_unica IS NOT INITIAL ).
    CONCATENATE tg_081-nfenum '-' tg_081-series INTO vl_xblnr.
  ELSE.
    vl_xblnr = tg_081-ref_doc_no.
  ENDIF.

  MOVE: p_saida-lote              TO wl_zglt035-lote,
        p_saida-bukrs             TO wl_zglt035-bukrs,
        wl_034-dep_resp           TO wl_zglt035-dpto_resp,
        'BRL'                     TO wl_zglt035-moeda_doc,
        'LM'                      TO wl_zglt035-blart,
        wl_080-bktxt              TO wl_zglt035-bktxt,
        vl_xblnr                  TO wl_zglt035-xblnr,
        tg_081-budat              TO wl_zglt035-budat,
        tg_081-bldat              TO wl_zglt035-bldat,
        tg_081-budat              TO wl_zglt035-dt_lcto,
        tg_081-budat+4(2)         TO wl_zglt035-monat,
        tg_081-budat(4)           TO wl_zglt035-gjahr,
        sy-uname                  TO wl_zglt035-usnam,
        sy-datum                  TO wl_zglt035-dt_entrada,
        sy-uzeit                  TO wl_zglt035-hr_entrada.


  vl_seqitem = 1.
  LOOP AT tg_081.
    CLEAR: wl_zglt036.

    wl_zglt036-hbkid    = wl_080-hbkid.
    wl_zglt036-zlsch    = wl_080-zlsch.
    wl_zglt036-zlspr    = wl_080-zlspr. "RJF - CS2024000206 - Lançamentos Bloqueados - Contratos Corporativos (ZGL059)
    wl_zglt036-dt_vct   = wl_080-zfbdt.
    wl_zglt036-bvtyp    = wl_080-bvtyp.
    wl_zglt036-hkont    = tg_081-hkont.
    wl_zglt036-bschl    = tg_081-bschl.
    wl_zglt036-seqitem  = vl_seqitem.
    wl_zglt036-gsber    = tg_081-gsber.
    wl_zglt036-kostl    = tg_081-kostl.

    IF tg_081-vbeln IS NOT INITIAL.
      CLEAR: v_aufnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = tg_081-vbeln
        IMPORTING
          output = v_aufnr.

      SELECT SINGLE *
        FROM aufk INTO @DATA(_wl_aufk)
       WHERE bukrs = @wl_080-bukrs
         AND aufnr = @v_aufnr.

      IF sy-subrc EQ 0.
        IF ( wl_zglt036-kostl IS NOT INITIAL ) AND ( wl_zglt036-kostl EQ _wl_aufk-kostv ).
          wl_zglt036-aufnr    = tg_081-vbeln.
        ENDIF.
      ENDIF.
    ENDIF.

    wl_zglt036-seqsub   = tg_081-seqsub.
    wl_zglt036-zuonr    = tg_081-ref_doc_no.
    wl_zglt036-sgtxt    = tg_081-sgtxt.
    wl_zglt036-esrnr    = tg_081-esrnr.
    wl_zglt036-esrre    = tg_081-esrre.

    IF tg_081-matnr IS NOT INITIAL.
      wl_zglt036-matnr_fi = tg_081-matnr.
    ENDIF.

    MOVE: abs( tg_081-netwr ) TO wl_zglt036-vlr_moeda_doc.
    "ABS( TG_081-NETWR ) TO WL_ZGLT036-VLR_MOEDA_INT,
    "ABS( TG_081-NETWR ) TO WL_ZGLT036-VLR_MOEDA_FORTE.

    IF wl_zglt036-bschl <> '31'.
      CLEAR: wl_zglt036-hbkid, wl_zglt036-bvtyp, wl_zglt036-dt_vct, wl_zglt036-zlsch, wl_zglt036-zlspr.
    ENDIF.

    APPEND wl_zglt036 TO gt_zglt036.

    CLEAR: wl_zglt036.
    ADD 1 TO vl_seqitem.
  ENDLOOP.

  "Verificar diferenças Outras Moedas antes de criar documento p/ Contabilização.
  PERFORM f_ctb_outras_moedas TABLES gt_zglt036
                               USING wl_zglt035.

  CALL METHOD zcl_gerar_lote=>contabilizar_lote(
    CHANGING
      i_zglt036 = gt_zglt036
      i_zglt035 = wl_zglt035 ).

  IF p_ctb_nf IS NOT INITIAL.
    UPDATE zglt081 SET doc_lcto     = wl_zglt035-doc_lcto
                       dt_lcto_ctb  = sy-datum
                       estorno_nf   = ''
     WHERE seq_lcto  = p_saida-seq_lcto
       AND agrp_nf   = p_saida-agrp_nf.
    "AND PART_FORN = ''.
  ELSE.
    UPDATE zglt081 SET doc_lcto    = wl_zglt035-doc_lcto
                       dt_lcto_ctb = sy-datum
                       estorno_nf  = ''
     WHERE seq_lcto  = p_saida-seq_lcto.
    "AND PART_FORN = ''.
  ENDIF.

  COMMIT WORK.

  IF p_lib_lote IS NOT INITIAL.
    CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
      EXPORTING
        p_num_lote = p_saida-lote.
  ENDIF.

  p_gerado_ctb = 'X'.

ENDFORM.

FORM f_handle_hotspot_click  USING  i_row_id     TYPE lvc_s_row
                                    i_column_id  TYPE lvc_s_col
                                    is_row_no    TYPE lvc_s_roid.

  DATA: it_rsparams TYPE TABLE OF rsparams,
        wa_rsparams TYPE rsparams.

  DATA: opt     TYPE ctu_params,
        vl_lote TYPE zglt034-lote.

  CLEAR: wa_saida_0100.
  CASE i_column_id.
    WHEN 'LOTE'.
      READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX i_row_id.

      CHECK wa_saida_0100-lote IS NOT INITIAL.

      REFRESH: it_rsparams.

      wa_rsparams-selname = 'P_BUKRS'.
      wa_rsparams-kind = 'P'.  "SELECT OPTIONS TO BE PASSED
      wa_rsparams-sign = 'I'.
      wa_rsparams-option = 'EQ'.
      wa_rsparams-low    = wa_saida_0100-bukrs.
      wa_rsparams-high   = wa_saida_0100-bukrs.
      APPEND wa_rsparams TO it_rsparams.

      wa_rsparams-selname = 'P_LOTE'.
      wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
      wa_rsparams-sign = 'I'.
      wa_rsparams-option = 'EQ'.
      wa_rsparams-low    = wa_saida_0100-lote.
      wa_rsparams-high   = wa_saida_0100-lote.
      APPEND wa_rsparams TO it_rsparams.

      SUBMIT zgl018 WITH SELECTION-TABLE it_rsparams
              AND RETURN.


    WHEN 'DOC_LCTO'.
      CLEAR: wa_saida_0100.
      READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX i_row_id.

      CHECK wa_saida_0100-doc_lcto IS NOT INITIAL.

      CLEAR: vl_lote.
      SET PARAMETER ID 'BLN' FIELD wa_saida_0100-doc_lcto.
      SET PARAMETER ID 'LOT' FIELD vl_lote.
      CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
    WHEN 'BELNR'.
      CLEAR: wa_saida_0100.
      READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX i_row_id.

      CHECK wa_saida_0100-belnr IS NOT INITIAL.

      SET PARAMETER ID 'BLN' FIELD wa_saida_0100-belnr.
      SET PARAMETER ID 'BUK' FIELD wa_saida_0100-bukrs.
      SET PARAMETER ID 'GJR' FIELD wa_saida_0100-dt_lcto_ctb(4).
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_NOVO_LCTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_novo_lcto .

  PERFORM f_clear_dados_lcto USING ''.

  zglt080-hbkid = 'BBRA'.

  vg_opr_lcto = c_new.

  PERFORM f_free_0110.
  CALL SCREEN 0110 STARTING AT 01 01 ENDING AT 170 22 .

  PERFORM f_renew_consulta USING '0100'.

  LEAVE TO SCREEN 0100.

ENDFORM.

FORM f_display_lcto .

  CLEAR: it_sel_rows[].

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  IF lines( it_sel_rows ) NE 1.
    MESSAGE s836(sd) WITH TEXT-e19 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.

  CHECK sy-subrc = 0.

  PERFORM f_seleciona_0110 USING wa_saida_0100.

  vg_opr_lcto = c_display.

  PERFORM f_free_0110.
  CALL SCREEN 0110 STARTING AT 01 01 ENDING AT 170 22 .

ENDFORM.

FORM f_clear_dados_lcto USING p_msg_confirm.

  IF p_msg_confirm IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Desejar realmente limpar os dados informados?'
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

  ENDIF.

  CLEAR: vg_sel_lote,
         zglt080,
         it_saida_0110[],
         tg_irf_basman[],
         tg_irf[].

ENDFORM.

FORM f_refresh_ctb .

  DATA: wl_zglt035 TYPE zglt035.

  FIELD-SYMBOLS <saida_0100> TYPE ty_saida_0100.

  LOOP AT it_saida_0100 ASSIGNING <saida_0100>.

    "Verifica se Doc. Lcto foi estornado.
    IF ( <saida_0100>-bukrs    IS NOT INITIAL ) AND
       ( <saida_0100>-doc_lcto IS NOT INITIAL ) AND
       ( <saida_0100>-belnr    IS INITIAL ).

      CLEAR: wl_zglt035.
      SELECT SINGLE *
        FROM zglt035
        INTO wl_zglt035
       WHERE bukrs    = <saida_0100>-bukrs
         AND doc_lcto = <saida_0100>-doc_lcto.

      IF ( ( sy-subrc EQ 0 ) AND ( wl_zglt035-loekz EQ 'X' ) ) OR (  sy-subrc NE 0 ).
        UPDATE zglt080 SET doc_lcto    = space
                           dt_lcto_ctb = space
                     WHERE seq_lcto = <saida_0100>-seq_lcto
                       AND doc_lcto = <saida_0100>-doc_lcto.

        IF sy-subrc = 0.
          <saida_0100>-status_ctb = icon_red_light.
          <saida_0100>-doc_lcto   = space.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    CASE <saida_0100>-status_ctb.
      WHEN icon_yellow_light.

        PERFORM f_retorna_status_zib USING <saida_0100>-doc_lcto
                                           <saida_0100>-dt_lcto_ctb(4)
                                  CHANGING wa_zib_chave
                                           wa_zib_erro.

        IF ( wa_zib_chave IS NOT INITIAL ).
          <saida_0100>-status_ctb  = icon_green_light .
          <saida_0100>-belnr       = wa_zib_chave-belnr.

          UPDATE zglt080 SET belnr    = wa_zib_chave-belnr
                       WHERE seq_lcto = <saida_0100>-seq_lcto
                         AND doc_lcto = <saida_0100>-doc_lcto.

        ELSEIF ( wa_zib_erro IS NOT INITIAL ).
          <saida_0100>-status_ctb  = icon_red_light.
        ENDIF.
    ENDCASE.

  ENDLOOP.

  PERFORM f_refresh_alv USING '0100'.


ENDFORM.

FORM f_eliminar_lcto.

  DATA: wl_081 TYPE zglt081.

  FIELD-SYMBOLS: <saida_0100> TYPE ty_saida_0100.

  DATA: tg_lote_lib TYPE TABLE OF zglt034-lote WITH HEADER LINE,
        vl_ger_ctb  TYPE c.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente eliminar o(s) registro(s) selecionado(s)?'
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

  LOOP AT it_sel_rows INTO wa_sel_rows.

    CLEAR: wl_081.

    READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.
    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM zglt081 INTO wl_081
     WHERE seq_lcto = wa_saida_0100-seq_lcto
       AND doc_lcto NE '0000000000'.

    IF ( sy-subrc = 0 ) AND ( wl_081-doc_lcto IS NOT INITIAL ).
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e22 wl_081-seq_lcto DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    UPDATE zglt080 SET loekz = 'X'
     WHERE seq_lcto = wa_saida_0100-seq_lcto.

    UPDATE zglt081 SET loekz = 'X'
     WHERE seq_lcto = wa_saida_0100-seq_lcto.

    IF sy-subrc = 0.
      MESSAGE s836(sd) WITH TEXT-s03 DISPLAY LIKE 'S'.
    ENDIF.

  ENDLOOP.

  PERFORM f_renew_consulta USING '0100'.

  LEAVE TO SCREEN 0100.

ENDFORM.

FORM f_estorno_ctb.

  DATA: wl_080 TYPE zglt080,
        wl_081 TYPE zglt081,
        gt_081 TYPE TABLE OF zglt081.

  DATA: it_dta   TYPE STANDARD TABLE OF bdcdata,
        wa_dta   TYPE bdcdata,
        wg_bdc   TYPE bdcdata,
        tg_bdc   TYPE TABLE OF bdcdata,
        tg_msg   TYPE TABLE OF bdcmsgcoll,
        wg_msg   TYPE bdcmsgcoll,
        opt      TYPE ctu_params,
        vl_stblg TYPE bkpf-stblg.

  FIELD-SYMBOLS: <saida_0110> TYPE ty_saida_0110.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0110->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  IF lines( it_sel_rows ) NE 1.
    MESSAGE s836(sd) WITH TEXT-e19 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente gerar o estorno para o registro selecionado?'
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

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  READ TABLE it_saida_0110 ASSIGNING <saida_0110> INDEX wa_sel_rows-index.

  IF <saida_0110>-belnr IS INITIAL.
    ROLLBACK WORK.
    MESSAGE s836(sd) WITH TEXT-e23 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CLEAR: wl_081.
  SELECT SINGLE *
    FROM zglt081 INTO wl_081
   WHERE seq_lcto = <saida_0110>-seq_lcto
     AND docnum   NE '0000000000'.

  IF sy-subrc = 0.
    ROLLBACK WORK.
    MESSAGE s836(sd) WITH TEXT-e47 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SELECT *
    FROM zglt081 INTO TABLE gt_081
   WHERE seq_lcto = <saida_0110>-seq_lcto
     AND belnr    = <saida_0110>-belnr.

  FREE: it_dta.
  DEFINE shdb.
    CLEAR wa_dta.
    wa_dta-program   = &1.
    wa_dta-dynpro    = &2.
    wa_dta-dynbegin  = &3.
    wa_dta-fnam      = &4.
    wa_dta-fval      = &5.
    APPEND wa_dta TO it_dta.
  END-OF-DEFINITION.

  shdb:
  'SAPMF05A' '0105' 'X'  ' '           ' ',
  ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
  ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
  ' '        ' '    ' '  'RF05A-BELNS' <saida_0110>-belnr,
  ' '        ' '    ' '  'BKPF-BUKRS'  <saida_0110>-bukrs,
  ' '        ' '    ' '  'RF05A-GJAHS' <saida_0110>-dt_lcto_ctb(4),
  ' '        ' '    ' '  'UF05A-STGRD' '01'.

  opt-dismode = 'E'.
  CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE stblg
    FROM bkpf INTO vl_stblg
   WHERE bukrs = <saida_0110>-bukrs
     AND belnr = <saida_0110>-belnr
     AND gjahr = <saida_0110>-dt_lcto_ctb(4).

  CHECK ( sy-subrc = 0 ) AND ( vl_stblg IS NOT INITIAL ).

  LOOP AT gt_081 INTO wl_081.
    wl_081-doc_lcto    = space.
    wl_081-belnr       = space.
    wl_081-dt_lcto_ctb = space.
    MODIFY zglt081 FROM wl_081.
  ENDLOOP.

  COMMIT WORK.
  MESSAGE s836(sd) WITH TEXT-s04 DISPLAY LIKE 'S'.

  PERFORM f_seleciona_0110 USING wa_saida_0100.
  PERFORM f_refresh_alv USING '0110'.

ENDFORM.

FORM f_importar_lcto.

  CALL SCREEN 0111 STARTING AT 02 02 ENDING AT 83 04.

ENDFORM.

FORM f_import_itens .

  DATA: gt_planilha LIKE STANDARD TABLE OF alsmex_tabline,
        wl_planilha LIKE alsmex_tabline,
        vl_dt_temp  TYPE sydatum.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = TEXT-i14.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
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

  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR: wa_saida_0110.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        wa_saida_0110-hkont = wl_planilha-value.
      WHEN 2.
        wa_saida_0110-gsber = wl_planilha-value.
      WHEN 3.
        wa_saida_0110-kostl = wl_planilha-value.
      WHEN 4.
        wa_saida_0110-vbeln = wl_planilha-value.
      WHEN 5.
        CLEAR: vl_dt_temp.
        CONCATENATE wl_planilha-value+6(4)
                    wl_planilha-value+3(2)
                    wl_planilha-value(2) INTO vl_dt_temp.
        wa_saida_0110-budat = vl_dt_temp.
      WHEN 6.
        CLEAR: vl_dt_temp.
        CONCATENATE wl_planilha-value+6(4)
                    wl_planilha-value+3(2)
                    wl_planilha-value(2) INTO vl_dt_temp.
        wa_saida_0110-bldat = vl_dt_temp.
      WHEN 7.
        PERFORM f_tratar_campo CHANGING wl_planilha-value.
        wa_saida_0110-menge = wl_planilha-value.
      WHEN 8.
        PERFORM f_tratar_campo CHANGING wl_planilha-value.
        wa_saida_0110-netwr = wl_planilha-value.
      WHEN 9.
        wa_saida_0110-nfenum = wl_planilha-value.
      WHEN 10.
        wa_saida_0110-series = wl_planilha-value.
      WHEN 11.
        wa_saida_0110-matnr = wl_planilha-value.
      WHEN 12.
        wa_saida_0110-asnum = wl_planilha-value.
      WHEN 13.
        wa_saida_0110-sgtxt = wl_planilha-value.
      WHEN 14.
        wa_saida_0110-cod_barras = wl_planilha-value.
      WHEN 15.
        wa_saida_0110-id_lms = wl_planilha-value.
    ENDCASE.

    AT END OF row.
      PERFORM f_conversion_input_0110 USING wa_saida_0110.
      APPEND wa_saida_0110 TO it_saida_0110.
    ENDAT.

  ENDLOOP.

  PERFORM f_refresh_alv USING '0110'.

  CALL METHOD obj_alv_0110->check_changed_data.

  MESSAGE s836(sd) WITH TEXT-s05 DISPLAY LIKE 'S'.
  LEAVE TO SCREEN 0.

ENDFORM.

FORM f_tratar_campo CHANGING v_value.
  REPLACE '.' WITH ' ' INTO v_value.
  REPLACE ',' WITH '.' INTO v_value.

  CONDENSE v_value NO-GAPS.
ENDFORM.

FORM f_edit_lcto.

  DATA: wl_081 TYPE zglt081,
        gt_089 TYPE TABLE OF zglt089 WITH HEADER LINE.

  FIELD-SYMBOLS <saida_0110> TYPE ty_saida_0110.

  CLEAR: it_sel_rows[].

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  IF lines( it_sel_rows ) NE 1.
    MESSAGE s836(sd) WITH TEXT-e19 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.


  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.

  CHECK sy-subrc = 0.

  SELECT SINGLE *
    FROM zglt081 INTO wl_081
   WHERE seq_lcto = wa_saida_0100-seq_lcto
     AND doc_lcto NE '0000000000'.

  IF ( sy-subrc = 0 ) AND ( wl_081-doc_lcto IS NOT INITIAL ).
    MESSAGE s836(sd) WITH TEXT-e39 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM f_lock_lcto USING 'B' wa_saida_0100-seq_lcto. "Bloqueia Lcto
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  CLEAR: zglt080,
         it_saida_0110[],
         tg_irf[],
         tg_irf_basman[].

  MOVE-CORRESPONDING wa_saida_0100 TO zglt080.
  SELECT *
    FROM zglt081 INTO CORRESPONDING FIELDS OF TABLE it_saida_0110
   WHERE seq_lcto = zglt080-seq_lcto.

  "Carrega IRF
  SELECT *
    FROM zglt089 INTO TABLE gt_089
   WHERE seq_lcto = zglt080-seq_lcto.

  "Carrega IRF Modificados
  LOOP AT gt_089.
    READ TABLE it_saida_0110 INTO wa_saida_0110 WITH KEY nfenum  = gt_089-nfenum
                                                         series  = gt_089-series.
    CHECK sy-subrc = 0.

    IF gt_089-wt_basman IS NOT INITIAL. "Base Manual
      CLEAR: tg_irf_basman.
      MOVE-CORRESPONDING gt_089 TO tg_irf_basman.
      "TG_IRF_BASMAN-WI_TAX_AMT = 0.
      APPEND tg_irf_basman.
    ENDIF.
  ENDLOOP.


  LOOP AT it_saida_0110 INTO wa_saida_0110.

    IF wa_saida_0110-part_forn IS NOT INITIAL.
      DELETE it_saida_0110.
    ELSE.
      IF wa_saida_0110-desconto IS NOT INITIAL.
        wa_saida_0110-netwr = abs( wa_saida_0110-netwr ) * -1.
      ENDIF.

      MODIFY it_saida_0110 FROM wa_saida_0110.
    ENDIF.


*    REFRESH: GT_ESTILO.
*    IF <SAIDA_0110>-HKONT = ZGLT080-LIFNR.
*
*      PERFORM F_STYLE_DISABLE_EDIT USING 'ASNUM'
*                                         CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*
*      PERFORM F_STYLE_DISABLE_EDIT USING 'NETWR'
*                                         CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*
*      PERFORM F_STYLE_DISABLE_EDIT USING 'GSBER'
*                                         CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*
*      PERFORM F_STYLE_DISABLE_EDIT USING 'HKONT'
*                                         CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*
*      PERFORM F_STYLE_DISABLE_EDIT USING 'KOSTL'
*                                          CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*
*      PERFORM F_STYLE_DISABLE_EDIT USING 'MATNR'
*                                         CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*
*      PERFORM F_STYLE_DISABLE_EDIT USING 'REF_DOC_NO'
*                                         CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*
*      PERFORM F_STYLE_DISABLE_EDIT USING 'SGTXT'
*                                         CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*
*      PERFORM F_STYLE_DISABLE_EDIT USING 'VBELN'
*                                         CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*
*
*      INSERT LINES OF GT_ESTILO INTO TABLE <SAIDA_0110>-ESTILO.
*    ENDIF.
  ENDLOOP.

  vg_opr_lcto = c_edit.

  PERFORM f_free_0110.
  CALL SCREEN 0110 STARTING AT 01 01 ENDING AT 170 22 .

  PERFORM f_lock_lcto USING 'D' wa_saida_0100-seq_lcto. "Desbloqueia Lcto

ENDFORM.

FORM f_style_disable_edit USING p_fieldname TYPE any
                                p_style     TYPE any.

  wl_estilo-fieldname = p_fieldname.
  wl_estilo-style     = p_style.

  APPEND wl_estilo TO gt_estilo.
ENDFORM.

FORM f_gerar_seq_lcto  USING    p_validar_lcto  TYPE c
                       CHANGING p_seq_lcto      TYPE zglt080-seq_lcto
                                p_valid.

  p_valid = 'X'.

  CHECK vg_opr_lcto EQ c_new.
  CHECK p_validar_lcto IS INITIAL.
  CHECK p_seq_lcto IS INITIAL.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZNUM_PCC'
      quantity    = '1'
    IMPORTING
      number      = p_seq_lcto.

  IF zglt080-seq_lcto IS INITIAL.
    CLEAR: p_valid.
    MESSAGE s836(sd) WITH TEXT-e06 TEXT-e07 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' TEXT-e06 TEXT-e07 '' .
    RETURN.
  ENDIF.

ENDFORM.

FORM f_gerar_nf_sel.

  CHECK vg_opr_lcto = c_display.

  FIELD-SYMBOLS: <saida_0110> TYPE ty_saida_0110.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0110->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente gerar a(s) Nota(s) Fiscal(is) para o(s) registro(s) selecionado(s)?'
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

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0110 ASSIGNING <saida_0110> INDEX wa_sel_rows-index.
    CHECK sy-subrc = 0.
    CHECK <saida_0110>-docnum IS INITIAL.

    "Lançamento só vai gerar pgto(Sem NF).
    CHECK <saida_0110>-sem_nf IS INITIAL.

    PERFORM f_gerar_nf USING <saida_0110>
                             ''
                    CHANGING <saida_0110>-docnum.

    PERFORM f_lock_lcto_item USING 'D' <saida_0110>-seq_lcto <saida_0110>-seqitem. "Desbloqueia Lcto

  ENDLOOP.

  PERFORM f_seleciona_0110 USING wa_saida_0100.
  PERFORM f_refresh_alv USING '0110'.


ENDFORM.

FORM f_gerar_nf USING p_saida_0110 TYPE ty_saida_0110
                      p_not_msg    TYPE c
             CHANGING p_ret_docnum TYPE zglt081-docnum.

  "Internal Tables
  DATA: gt_081      TYPE TABLE OF zglt081,
        gt_082      TYPE TABLE OF zglt082,
        gt_083      TYPE TABLE OF zglt083,
        gt_084      TYPE TABLE OF zglt084,
        gt_085      TYPE TABLE OF zglt085,
        gt_asmdt    TYPE TABLE OF asmdt,
        gt_asmd     TYPE TABLE OF asmd,
        tg_batl1    TYPE TABLE OF j_1batl1t WITH HEADER LINE,
        tg_batl2    TYPE TABLE OF j_1batl2t WITH HEADER LINE,
        tg_batl4    TYPE TABLE OF j_1batl4t WITH HEADER LINE,
        tg_batl5    TYPE TABLE OF j_1batl5t WITH HEADER LINE,
        tl_partner  TYPE TABLE OF bapi_j_1bnfnad,
        tl_item     TYPE TABLE OF bapi_j_1bnflin,
        tl_item_add TYPE TABLE OF bapi_j_1bnflin_add,
        tl_item_tax TYPE TABLE OF bapi_j_1bnfstx,
        tl_return   TYPE TABLE OF bapiret2,
        tl_msg      TYPE TABLE OF bapi_j_1bnfftx.

  "Work Áreas
  DATA: wl_080                  TYPE zglt080,
        wl_081                  TYPE zglt081,
        wl_082                  TYPE zglt082,
        wl_083                  TYPE zglt083,
        wl_084                  TYPE zglt084,
        wl_085                  TYPE zglt085,
        wl_t007a                TYPE t007a,
        wl_header               TYPE bapi_j_1bnfdoc,
        wl_header_add           TYPE bapi_j_1bnfdoc_add,
        wl_partner              TYPE bapi_j_1bnfnad,
        wl_item                 TYPE bapi_j_1bnflin,
        wl_item_add             TYPE bapi_j_1bnflin_add,
        wl_item_tax             TYPE bapi_j_1bnfstx,
        wl_msg                  TYPE bapi_j_1bnfftx,
        wl_return               TYPE bapiret2,
        wl_nfcheck              TYPE bapi_j_1bnfcheck,
        wl_material_text_record TYPE makt,

        wl_j1baa                TYPE j_1baa,
        wl_j1bb2                TYPE j_1bb2,
        wl_lfa1_key             TYPE lfa1,
        wl_zib_nfe              TYPE zib_nfe_forn,
        wl_kna1                 TYPE kna1,
        wl_j1bad                TYPE j_1bad,
        wl_asmdt                TYPE asmdt,
        wl_asmd                 TYPE asmd,
        wl_doc_est              TYPE j_1bnfdoc.

  "Variáveis.
  DATA: vl_cont          TYPE sy-tabix,
        vl_werks         TYPE werks_d,
        vl_itmnum        TYPE j_1bnflin-itmnum,
        vl_refkey        TYPE j_1bnflin-refkey,
        vl_docnum        TYPE j_1bnfdoc-docnum,
        vl_field_aux(20).

  CHECK p_saida_0110-seq_lcto IS NOT INITIAL.
  CHECK p_saida_0110-seqitem  IS NOT INITIAL.

  REFRESH: tl_partner, tl_item, tl_item_add[], tl_item_tax[], tl_return[], tl_msg[],
           gt_081[], gt_082[], gt_083[], gt_084[], gt_085[],tg_batl1[],
           tg_batl2[], tg_batl4[], tg_batl5[].

  CLEAR: wl_header, wl_header_add, wl_partner, wl_item, wl_item_add, wl_item_tax,
         wl_msg, wl_return, wl_nfcheck, wl_material_text_record, vl_cont, vl_werks,
         wl_j1baa, wl_j1bb2, wl_lfa1_key, wl_zib_nfe, wl_kna1, vl_itmnum, vl_refkey,
         vl_docnum, vl_field_aux, vl_refkey, vl_docnum.

  CLEAR: wl_080, wl_081, wl_082,wl_083, wl_084,wl_085.

  CHECK p_saida_0110-part_forn IS INITIAL.
  CHECK p_saida_0110-desconto  IS INITIAL.

  "Lançamento só vai gerar pgto(Sem NF).
  CHECK p_saida_0110-sem_nf IS INITIAL.

*-----------------------------------------------------------------*
*  Verifica se já tem nota com a mesma referência
*-----------------------------------------------------------------*
  IF p_saida_0110-agrp_nf IS INITIAL.
    IF p_not_msg IS INITIAL.
      MESSAGE s836(sd) WITH 'Agrp.NF' TEXT-e46 DISPLAY LIKE 'S'.
    ENDIF.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM zglt081 INTO wl_081
   WHERE seq_lcto = p_saida_0110-seq_lcto
     AND seqitem  = p_saida_0110-seqitem.

  IF ( sy-subrc = 0 ) AND ( wl_081-docnum IS NOT INITIAL ).
    RETURN.
  ENDIF.

  PERFORM f_lock_lcto_item USING 'B' p_saida_0110-seq_lcto p_saida_0110-seqitem. "Bloqueia Lcto
  IF sy-subrc <> 0.
    IF p_not_msg IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    EXIT.
  ENDIF.

  CLEAR: wl_081.

  CONCATENATE 'ZG59' p_saida_0110-seq_lcto p_saida_0110-agrp_nf INTO vl_refkey.

  CLEAR: vl_docnum.
  SELECT SINGLE a~docnum
    FROM j_1bnflin AS a INTO vl_docnum
   WHERE a~refkey = vl_refkey
     AND NOT EXISTS ( SELECT *
                        FROM j_1bnfdoc AS b
                       WHERE b~docref = a~docnum
                         AND b~doctyp = '5' ).

  IF ( sy-subrc = 0 ) AND ( vl_docnum IS NOT INITIAL ).
    UPDATE zglt081 SET docnum = vl_docnum
     WHERE seq_lcto  = p_saida_0110-seq_lcto
       AND agrp_nf   = p_saida_0110-agrp_nf.
    "AND PART_FORN = ''.

    p_ret_docnum = vl_docnum.

    IF p_not_msg IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-s06 vl_docnum DISPLAY LIKE 'S'.
    ENDIF.

    RETURN.
  ENDIF.

*-----------------------------------------------------------------*
*  Leitura de Cabeçalho.
*-----------------------------------------------------------------*
  SELECT SINGLE *
    FROM zglt080 INTO wl_080
   WHERE seq_lcto = p_saida_0110-seq_lcto.

  IF sy-subrc NE 0.
    IF p_not_msg IS INITIAL.
      MESSAGE s836(sd) WITH 'Cabeçalho.Doc.' TEXT-e46 DISPLAY LIKE 'S'.
    ENDIF.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM zglt081 INTO wl_081
   WHERE seq_lcto  = p_saida_0110-seq_lcto
     AND belnr     = ''
     AND part_forn = ''
     AND desconto  = ''.

  IF sy-subrc = 0.
    IF p_not_msg IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e23 DISPLAY LIKE 'S'.
    ENDIF.
    RETURN.
  ENDIF.

*-----------------------------------------------------------------*
*  Leitura de Parâmetros.
*-----------------------------------------------------------------*
  SELECT SINGLE *
    FROM zglt082 INTO wl_082
   WHERE operacao = p_saida_0110-operacao.

  IF sy-subrc NE 0.
    MESSAGE s836(sd) WITH 'Parâmetro NF' TEXT-e46 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

*-----------------------------------------------------------------*
*  Leitura Textos Direitos Fiscais
*-----------------------------------------------------------------*
  CLEAR: wl_t007a.
  SELECT SINGLE *
    FROM t007a INTO wl_t007a
   WHERE kalsm EQ 'TAXBRA'
     AND mwskz EQ wl_082-taxcode.

  "ICMS
  SELECT *
    FROM j_1batl1t INTO TABLE tg_batl1
   WHERE taxlaw EQ wl_t007a-j_1btaxlw1
     AND langu  EQ sy-langu.

  "IPI
  SELECT *
    FROM j_1batl2t INTO TABLE tg_batl2
   WHERE taxlaw EQ wl_t007a-j_1btaxlw2
     AND langu  EQ sy-langu.

  "COFINS
  SELECT *
    FROM j_1batl4t INTO TABLE tg_batl4
   WHERE taxlaw EQ wl_t007a-j_1btaxlw4
     AND langu  EQ sy-langu.

  "PIS
  SELECT *
    FROM j_1batl5t INTO TABLE tg_batl5
   WHERE taxlaw EQ wl_t007a-j_1btaxlw5
     AND langu  EQ sy-langu.

*-----------------------------------------------------------------*
*  Leitura de Itens.
*-----------------------------------------------------------------*
  SELECT *
    FROM zglt081 INTO TABLE gt_081
   WHERE seq_lcto  = p_saida_0110-seq_lcto
     AND agrp_nf   = p_saida_0110-agrp_nf
     AND desconto  = ''
     AND part_forn = ''.

  IF gt_081[] IS INITIAL..
    MESSAGE s836(sd) WITH 'Itens NF' TEXT-e46 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

*-----------------------------------------------------------------*
*  Leitura dados Serviços.
*-----------------------------------------------------------------*
  SELECT *
    FROM asmd INTO TABLE gt_asmd
     FOR ALL ENTRIES IN gt_081
   WHERE asnum = gt_081-asnum.

  IF gt_asmd[] IS NOT INITIAL.
    SELECT *
      FROM asmdt INTO TABLE gt_asmdt
       FOR ALL ENTRIES IN gt_asmd
     WHERE asnum = gt_asmd-asnum
       AND spras = sy-langu.
  ENDIF.

*-----------------------------------------------------------------*
*  Leitura de Impostos.
*-----------------------------------------------------------------*
  SELECT *
    FROM zglt085 INTO TABLE gt_085
    FOR ALL ENTRIES IN gt_081
   WHERE seq_lcto = gt_081-seq_lcto
     AND seqitem  = gt_081-seqitem.

*-----------------------------------------------------------------*
*  Leitura Parceiros NF.
*-----------------------------------------------------------------*
  SELECT *
    FROM zglt083 INTO TABLE gt_083
   WHERE seq_lcto = p_saida_0110-seq_lcto
     AND seqitem  = p_saida_0110-seqitem.

  IF gt_083[] IS INITIAL..
    MESSAGE s836(sd) WITH 'Parceiros NF' TEXT-e46 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

*-----------------------------------------------------------------*
*  Leitura de Categoria NF.
*-----------------------------------------------------------------*
  SELECT SINGLE * INTO wl_j1baa
    FROM j_1baa
   WHERE nftype EQ wl_082-nftype.

  IF sy-subrc NE 0.
    MESSAGE s836(sd) WITH 'Categoria NF' TEXT-e46 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

*-----------------------------------------------------------------*
*  Geração NF.
*-----------------------------------------------------------------*

*  Preenchimento Header.
  wl_header-nftype  = wl_j1baa-nftype.
  wl_header-doctyp  = wl_j1baa-doctyp.
  wl_header-direct  = wl_j1baa-direct.
  IF wl_j1baa-entrad IS NOT INITIAL.
    wl_header-entrad = wl_j1baa-entrad.
  ENDIF.
  wl_header-docdat  = p_saida_0110-bldat.
  wl_header-pstdat  = p_saida_0110-budat.
  IF wl_080-fis_dt_atual IS NOT INITIAL.
    wl_header-pstdat = sy-datum.
  ENDIF.
  wl_header-credat  = sy-datum.
  wl_header-model   = wl_j1baa-model.

  IF gt_asmd[] IS NOT INITIAL.
    wl_header-nfesrv  = 'X'.
  ENDIF.

  "WL_HEADER-DOCREF  = P_SAIDA_0110-REFERENCIA.

  IF ( p_saida_0110-nfenum IS NOT INITIAL ) AND
     ( wl_j1baa-nfe    EQ 'X' ) AND
     ( wl_j1baa-direct EQ '1' ).

    IF ( wl_j1baa-partyp EQ 'C' ).
      SELECT SINGLE * INTO wl_kna1
        FROM kna1
       WHERE kunnr EQ wl_080-lifnr.
    ELSE.
      SELECT SINGLE * INTO wl_lfa1_key
        FROM lfa1
       WHERE lifnr EQ wl_080-lifnr.
    ENDIF.

    IF ( sy-subrc EQ 0 ).

      IF ( NOT wl_kna1 IS INITIAL ).
        SELECT SINGLE * INTO wl_zib_nfe
          FROM zib_nfe_forn
         WHERE nu_chave_cnpj EQ wl_kna1-stcd1
           AND nu_chave_numero EQ p_saida_0110-nfenum
           AND nu_chave_serie  EQ p_saida_0110-series.
      ELSE.
        SELECT SINGLE * INTO wl_zib_nfe
          FROM zib_nfe_forn
         WHERE nu_chave_cnpj EQ wl_lfa1_key-stcd1
           AND nu_chave_numero EQ p_saida_0110-nfenum
           AND nu_chave_serie  EQ p_saida_0110-series.
      ENDIF.

      IF ( sy-subrc NE 0 ).
        wl_msg-message = 'Arquivo XML da NF-e não cadastrado.'.
        APPEND wl_msg TO tl_msg.
        CLEAR: wl_msg.
      ELSE.
        wl_header-access_key = wl_zib_nfe-nu_chave.
        wl_header-docstat    = wl_zib_nfe-st_nota.
        wl_header-tpemis     = wl_zib_nfe-nu_chave+34(1).
      ENDIF.

    ENDIF.

  ENDIF.

  IF sy-subrc IS INITIAL.
    IF wl_j1baa-form IS NOT INITIAL.
      SELECT SINGLE * INTO wl_j1bb2
        FROM j_1bb2
       WHERE bukrs  EQ p_saida_0110-bukrs
         AND branch EQ p_saida_0110-gsber_nf
         AND form   EQ wl_j1baa-form.

      IF sy-subrc IS INITIAL.
        MOVE wl_j1bb2-series TO wl_header-series.
      ENDIF.
    ELSE.
      wl_header-series = p_saida_0110-series.
    ENDIF.
  ENDIF.

  wl_header-inco1   = p_saida_0110-inco1.
  wl_header-inco2   = p_saida_0110-inco2.
  wl_header-manual  = 'X'.
  wl_header-waerk   = 'BRL'.
  wl_header-bukrs   = p_saida_0110-bukrs.
  wl_header-branch  = p_saida_0110-gsber_nf.
  wl_header-parvw   = 'LF'. "Validar
  wl_header-parid   = wl_080-lifnr.
  wl_header-nfe     = wl_j1baa-nfe.

  IF wl_j1baa-form IS INITIAL.
    wl_header-nfnum   = p_saida_0110-nfenum.
    wl_header-series  = p_saida_0110-series.
  ENDIF.

* Preenche Header Tot
  LOOP AT gt_081 INTO wl_081.
    ADD wl_081-netwr TO wl_header_add-nftot.
  ENDLOOP.

* Preenche NFCHECK
  wl_nfcheck-chekcon = 'X'.

* Preenche Partner
  LOOP AT gt_083 INTO wl_083.
    CLEAR: wl_partner.

    wl_partner-parvw  = wl_083-parvw.
    wl_partner-partyp = wl_083-partyp.

    IF wl_083-partyp EQ 'B'. "Filial
      CLEAR vl_werks.
      SHIFT wl_083-parid LEFT DELETING LEADING '0'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_083-parid
        IMPORTING
          output = vl_werks.

      CONCATENATE  p_saida_0110-bukrs vl_werks INTO wl_partner-parid.
    ELSE.
      wl_partner-parid  = wl_083-parid.
    ENDIF.

    APPEND wl_partner TO tl_partner.
  ENDLOOP.

* Preenche Item
  LOOP AT gt_081 INTO wl_081.
    CLEAR: wl_item.

    ADD 10 TO vl_itmnum.

    IF wl_081-matnr IS NOT INITIAL.
      CALL FUNCTION 'J_1B_MATERIAL_READ'
        EXPORTING
          matnr                = wl_081-matnr
          val_area             = wl_081-gsber_nf
          val_type             = space
          language             = sy-langu
          i_werks              = wl_081-gsber_nf
        IMPORTING
          nbm                  = wl_item-nbm
          matuse               = wl_item-matuse
          matorg               = wl_item-matorg
          material_text_record = wl_material_text_record
          e_matkl              = wl_item-matkl
        EXCEPTIONS
          material_not_found   = 1
          valuation_not_found  = 2
          OTHERS               = 3.

      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.

      wl_item-matnr   = wl_081-matnr.

    ELSEIF wl_081-asnum IS NOT INITIAL.

      READ TABLE gt_asmd  INTO wl_asmd  WITH KEY asnum = wl_081-asnum.
      READ TABLE gt_asmdt INTO wl_asmdt WITH KEY asnum = wl_081-asnum.

      wl_material_text_record-maktx = wl_asmdt-asktx.
      "WL_ITEM-SRVNR = WL_ASMD-ASNUM.
      wl_item-matkl = wl_asmd-matkl.

    ENDIF.

    wl_item-maktx   = wl_material_text_record-maktx.
    wl_item-itmnum  = vl_itmnum.
    wl_item-bwkey   = wl_081-gsber_nf.
    wl_item-refkey  = vl_refkey.
    "WL_ITEM-REFTYP  = 'ZW'.
    wl_item-menge   = wl_081-menge.
    wl_item-meins   = wl_081-meins.
    wl_item-itmtyp  = wl_081-itmtyp.
    wl_item-werks   = wl_081-gsber_nf.
    wl_item-cfop_10 = wl_081-cfop.
    wl_item-netpr   = wl_081-netpr.
    wl_item-netwr   = wl_081-netwr.
    wl_item-taxlw1  = wl_t007a-j_1btaxlw1.
    wl_item-taxlw2  = wl_t007a-j_1btaxlw2.
    wl_item-taxlw4  = wl_t007a-j_1btaxlw4.
    wl_item-taxlw5  = wl_t007a-j_1btaxlw5.
    wl_item-incltx  = 'X'.
    APPEND wl_item TO tl_item.
    CLEAR: wl_item.

* Preenche Item ADD
*    WL_ITEM_ADD-ITMNUM = VL_ITMNUM.
*    WL_ITEM_ADD-DIRECT = WL_HEADER-DIRECT.
*    APPEND WL_ITEM_ADD TO TL_ITEM_ADD.
*    CLEAR: WL_ITEM_ADD.

** Preenche Item TAX
    LOOP AT gt_085 INTO wl_085 WHERE seq_lcto EQ wl_081-seq_lcto
                                 AND seqitem  EQ wl_081-seqitem.
      CLEAR: wl_item_tax.

      wl_item_tax-itmnum = vl_itmnum.
      wl_item_tax-taxtyp = wl_085-taxtyp.
      wl_item_tax-base   = wl_085-base.
      wl_item_tax-rate   = wl_085-rate.
      wl_item_tax-taxval = wl_085-taxval.
      wl_item_tax-excbas = wl_085-excbas.
      wl_item_tax-othbas = wl_085-othbas.

      APPEND wl_item_tax TO tl_item_tax.
    ENDLOOP.
  ENDLOOP.

* Texto de Taxlaw1
  READ TABLE tg_batl1 INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM f_add_text_lei TABLES tl_msg
                            USING tg_batl1-line1
                                  tg_batl1-line2
                                  tg_batl1-line3
                                  tg_batl1-line4
                                  vl_cont.

  ENDIF.

* Texto de Taxlaw2
  READ TABLE tg_batl2 INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM f_add_text_lei TABLES tl_msg
                            USING tg_batl2-line1
                                  tg_batl2-line2
                                  tg_batl2-line3
                                  tg_batl2-line4
                                  vl_cont.
  ENDIF.

* Texto de Taxlaw4
  READ TABLE tg_batl4 INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM f_add_text_lei TABLES tl_msg
                            USING tg_batl4-line1
                                  tg_batl4-line2
                                  tg_batl4-line3
                                  tg_batl4-line4
                                  vl_cont.
  ENDIF.

* Texto de Taxlaw5
  READ TABLE tg_batl5 INDEX 1.
  IF sy-subrc IS INITIAL.
    PERFORM f_add_text_lei TABLES tl_msg
                            USING tg_batl5-line1
                                  tg_batl5-line2
                                  tg_batl5-line3
                                  tg_batl5-line4
                                  vl_cont.
  ENDIF.

* Criar NF
  CLEAR: vl_docnum.
  CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA'
    EXPORTING
      obj_header     = wl_header
      obj_header_add = wl_header_add
      nfcheck        = wl_nfcheck
    IMPORTING
      e_docnum       = vl_docnum
    TABLES
      obj_partner    = tl_partner
      obj_item       = tl_item
      obj_item_add   = tl_item_add
      obj_item_tax   = tl_item_tax
      obj_header_msg = tl_msg
      return         = tl_return.

  IF vl_docnum IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    p_ret_docnum = vl_docnum.

    UPDATE zglt081 SET docnum = vl_docnum
     WHERE seq_lcto  = p_saida_0110-seq_lcto
       AND agrp_nf   = p_saida_0110-agrp_nf.
    "AND PART_FORN = ''.

    MESSAGE s836(sd) WITH TEXT-s06 vl_docnum DISPLAY LIKE 'S'.

  ELSE.
    PERFORM f_gravar_log TABLES tl_return
                          USING 'DOCNUM'
                                p_saida_0110.

    MESSAGE s836(sd) WITH TEXT-e45 DISPLAY LIKE 'S'.
  ENDIF.

ENDFORM.


FORM f_add_text_lei TABLES tl_msg
                     USING p_line1  TYPE j_1btaxlwt
                           p_line2  TYPE j_1btaxlwt
                           p_line3  TYPE j_1btaxlwt
                           p_line4  TYPE j_1btaxlwt
                           p_cont   TYPE sy-tabix.

  DATA: wl_msg TYPE bapi_j_1bnfftx.

  CHECK ( p_line1 IS NOT INITIAL ) OR
        ( p_line2 IS NOT INITIAL ) OR
        ( p_line3 IS NOT INITIAL ) OR
        ( p_line4 IS NOT INITIAL ).

  CLEAR: wl_msg.
  ADD 1 TO p_cont.

  IF p_line1 IS NOT INITIAL.
    wl_msg-seqnum  = p_cont.
    wl_msg-message = p_line1.
    ADD 1 TO wl_msg-linnum.
    APPEND wl_msg TO tl_msg.
  ENDIF.

  IF p_line2 IS NOT INITIAL.
    wl_msg-seqnum  = p_cont.
    wl_msg-message = p_line2.
    ADD 1 TO wl_msg-linnum.
    APPEND wl_msg TO tl_msg.
  ENDIF.

  IF p_line3 IS NOT INITIAL.
    wl_msg-seqnum  = p_cont.
    wl_msg-message = p_line3.
    ADD 1 TO wl_msg-linnum.
    APPEND wl_msg TO tl_msg.
  ENDIF.

  IF p_line4 IS NOT INITIAL.
    wl_msg-seqnum  = p_cont.
    wl_msg-message = p_line4.
    ADD 1 TO wl_msg-linnum.
    APPEND wl_msg TO tl_msg.
  ENDIF.

ENDFORM.

FORM f_monta_impostos TABLES tl_impo      STRUCTURE tg_impo
                       USING p_j1baa      TYPE j_1baa
                             p_080        TYPE zglt080  "Cabeçalho
                             p_081        TYPE zglt081  "Itens
                             p_082        TYPE zglt082
                             p_shipfrom   TYPE lfa1-regio
                             p_shipto     TYPE lfa1-regio.

  DATA: wl_081  TYPE zglt081,
        wl_1baa TYPE j_1baa.

  DATA: tg_084 TYPE TABLE OF zglt084 WITH HEADER LINE. "Parâmetros Impostos

  DATA: wl_1baj  TYPE j_1baj,
        wl_1bajt TYPE j_1bajt.

  DATA: BEGIN OF wl_1btxic,
          rate TYPE j_1btxic3-rate,
          base TYPE j_1btxic3-base,
        END OF wl_1btxic.

  DATA: wl_base_aux    TYPE j_1btxic3-base,
        wl_a924        TYPE a924,
        wl_konp        TYPE konp,
        wl_1btxsdc     TYPE j_1btxsdc,
        wl_1btxpis     TYPE j_1btxpis,
        wl_1btxcof     TYPE j_1btxcof,
        vl_calc_pis    TYPE c,
        vl_calc_cofins TYPE c.

  "WL_IMPO_COMP LIKE LINE OF TG_IMPO_COMP.

  CLEAR: vl_calc_pis, vl_calc_cofins, wl_a924, wl_konp, tg_084[], wl_081, tl_impo[].

  MOVE-CORRESPONDING p_081 TO wl_081.

  SELECT *
    FROM zglt084 INTO TABLE tg_084
   WHERE operacao = wl_081-operacao.

  "Monta Impostos.
  REFRESH: tg_impo.
  LOOP AT tg_084.
    CLEAR: tg_impo, wl_1baj, wl_1bajt.

    SELECT SINGLE *
      FROM j_1baj INTO wl_1baj
     WHERE taxtyp = tg_084-taxtyp.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bajt INTO wl_1bajt
     WHERE taxtyp = tg_084-taxtyp
       AND spras  = sy-langu.

    MOVE: tg_084-taxtyp     TO tg_impo-taxtyp,
          wl_1bajt-ttypetxt TO tg_impo-ttypetxt,
          wl_1baj-taxgrp    TO tg_impo-taxgrp.

    APPEND tg_impo.
  ENDLOOP.

  CLEAR: wl_1btxsdc.
  " SELECT SINGLE *
  "   FROM J_1BTXSDC INTO WL_1BTXSDC.
  "  WHERE TAXCODE EQ P_082-TAXCODE.

  SELECT SINGLE *
    FROM j_1btxpis INTO wl_1btxpis
   WHERE country EQ c_br
     AND gruop   EQ c_60
     AND value   EQ p_080-bukrs
     AND value2  EQ p_082-taxcode.

  IF sy-subrc = 0.
    vl_calc_pis = c_x.
  ENDIF.

  SELECT SINGLE *
    FROM j_1btxcof INTO wl_1btxcof
   WHERE country EQ c_br
     AND gruop   EQ c_60
     AND value   EQ p_080-bukrs
     AND value2  EQ p_082-taxcode.

  IF sy-subrc = 0.
    vl_calc_cofins = c_x.
  ENDIF.

  LOOP AT tg_impo.

    IF tg_impo-taxtyp EQ c_icm3.

      CASE p_082-opertyp.
        WHEN c_t.

          "Indica se, em uma nota fiscal, se trata de uma Entrada, ou seja,
          "uma nota fiscal de entrada que foi emitida pelo próprio cliente
          " em vez de (como usual) ser pelo fornecedor.
          IF p_j1baa-entrad EQ c_x.

            SELECT SINGLE rate base
              FROM j_1btxic3 INTO wl_1btxic
             WHERE land1    = c_br
               AND shipfrom = p_shipfrom
               AND shipto   = p_shipto
               AND gruop    = c_30
               AND value    = p_080-lifnr
               AND value2   = wl_081-cprod.

            IF sy-subrc NE 0.

              SELECT SINGLE rate base
                FROM j_1btxic3 INTO wl_1btxic
               WHERE land1    = c_br
                 AND shipfrom = p_shipfrom
                 AND shipto   = p_shipto
                 AND gruop    = c_40
                 AND value    = p_080-lifnr.

              IF sy-subrc NE 0.
                "IF P_PARVW NE C_BR AND P_PARVW NE C_AG.
                SELECT SINGLE rate base
                  FROM j_1btxic2 INTO wl_1btxic
                 WHERE land1     = c_br
                   AND shipfrom  = p_shipfrom
                   AND shipto    = p_shipto
                   AND matnr     = wl_081-cprod.
                "ENDIF.
                IF sy-subrc NE 0.
                  SELECT SINGLE rate
                    FROM j_1btxic1 INTO wl_1btxic
                   WHERE land1     = c_br
                     AND shipfrom  = p_shipfrom
                     AND shipto    = p_shipto.
                ENDIF.
              ENDIF.

            ENDIF.

          ELSE.

            SELECT SINGLE rate base
              FROM j_1btxic3 INTO wl_1btxic
             WHERE land1    = c_br
               AND shipfrom = p_shipfrom
               AND shipto   = p_shipto
               AND gruop    = c_76
               AND value    = p_080-lifnr
               AND value2   = wl_081-cprod.

            IF sy-subrc NE 0.
              SELECT SINGLE rate base
                FROM j_1btxic2 INTO wl_1btxic
               WHERE land1    = c_br
                 AND shipfrom = p_shipfrom
                 AND shipto   = p_shipto
                 AND matnr    = wl_081-cprod.

              IF sy-subrc NE 0.
                SELECT SINGLE rate
                  FROM j_1btxic1 INTO wl_1btxic
                 WHERE land1    = c_br
                   AND shipfrom = p_shipfrom
                   AND shipto   = p_shipto.
              ENDIF.
            ENDIF.

          ENDIF.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.

          IF wl_1btxic-base IS INITIAL.
            IF wl_konp-kbetr GT wl_081-netpr.
              wl_081-netwr = wl_081-menge * wl_konp-kbetr.
            ENDIF.
            tl_impo-base   = wl_081-netwr.
            tl_impo-taxval = ( tl_impo-base * ( wl_1btxic-rate / 100 ) ).
            tl_impo-othbas = 0.
          ELSE.
            IF wl_konp-kbetr GT wl_081-netpr.
              wl_081-netwr = wl_081-menge * wl_konp-kbetr.
            ENDIF.
            tl_impo-base   = wl_081-netwr * ( wl_1btxic-base / 100 ).
            tl_impo-taxval = tl_impo-base * ( wl_1btxic-rate / 100 ).
            tl_impo-othbas = wl_081-netwr - tl_impo-base.

          ENDIF.
          tl_impo-rate = wl_1btxic-rate.
*          IF P_082-COMPLEMENTO EQ 'S'.
*            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
*                   TL_IMPO-EXCBAS.
*          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo.
        WHEN c_i.
          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          MOVE: wl_081-netwr TO tl_impo-excbas.
*          IF P_082-COMPLEMENTO EQ 'S'.
*            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
*                   TL_IMPO-EXCBAS.
*          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo.
        WHEN c_n.
          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          MOVE: wl_081-netwr TO tl_impo-othbas.
*          IF P_082-COMPLEMENTO EQ 'S'.
*            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
*                   TL_IMPO-EXCBAS.
*          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo.
      ENDCASE.

    ELSEIF vl_calc_pis    EQ c_x
       AND tg_impo-taxtyp EQ c_ipis.

      SELECT SINGLE *
        FROM j_1btxpis INTO wl_1btxpis
       WHERE country EQ c_br
         AND gruop   EQ c_72
         AND value   EQ wl_081-gsber_nf.

      MOVE-CORRESPONDING: tg_impo TO tl_impo.
      IF sy-subrc IS INITIAL.
        tl_impo-base   = wl_081-netwr.
        tl_impo-rate   = wl_1btxpis-rate.
        tl_impo-taxval = tl_impo-base * ( wl_1btxpis-rate / 100 ).
        tl_impo-othbas = 0.
      ELSE.
        MOVE: wl_081-netwr TO tl_impo-othbas.
      ENDIF.
*      IF P_082-COMPLEMENTO EQ 'S'.
*        CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
*               TL_IMPO-EXCBAS.
*      ENDIF.
      APPEND tl_impo.
      CLEAR: tl_impo, wl_1btxpis.

    ELSEIF vl_calc_cofins EQ c_x
       AND tg_impo-taxtyp EQ c_icof.

      SELECT SINGLE *
        FROM j_1btxcof INTO wl_1btxcof
       WHERE country EQ c_br
         AND gruop   EQ c_71
         AND value   EQ wl_081-gsber_nf.

      MOVE-CORRESPONDING: tg_impo TO tl_impo.
      IF sy-subrc IS INITIAL.
        tl_impo-base   = wl_081-netwr.
        tl_impo-rate   = wl_1btxcof-rate.
        IF  tl_impo-base > 0 AND wl_1btxcof-rate  > 0.
          tl_impo-taxval = tl_impo-base * ( wl_1btxcof-rate / 100 ).
        ENDIF.
        tl_impo-othbas = 0.
      ELSE.
        MOVE: wl_081-netwr TO tl_impo-othbas.
      ENDIF.

*      IF P_082-COMPLEMENTO EQ 'S'.
*        CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
*               TL_IMPO-EXCBAS.
*      ENDIF.
      APPEND tl_impo.
      CLEAR: tl_impo, wl_1btxcof.

    ELSEIF tg_impo-taxtyp EQ c_ics1.

      CLEAR: wl_1baa.
      SELECT SINGLE *
        FROM j_1baa INTO wl_1baa
       WHERE itmtyp EQ wl_081-itmtyp.

      IF wl_1baa-entrad EQ c_x.

        SELECT SINGLE rate base
          FROM j_1btxic3 INTO wl_1btxic
         WHERE land1     = c_br
           AND shipfrom  = p_shipfrom
           AND shipto    = p_shipto
           AND gruop     = c_30
           AND value     = p_080-lifnr
           AND value2    = wl_081-cprod.

        IF sy-subrc NE 0.
          SELECT SINGLE rate base
            FROM j_1btxic3 INTO wl_1btxic
           WHERE land1     = c_br
             AND shipfrom  = p_shipfrom
             AND shipto    = p_shipto
             AND gruop     = c_40
             AND value     = p_080-lifnr.

          IF sy-subrc NE 0.
            SELECT SINGLE rate
              FROM j_1btxic1 INTO wl_1btxic
             WHERE land1    = c_br
               AND shipfrom = p_shipfrom
               AND shipto   = p_shipto.
          ENDIF.

        ENDIF.

      ELSE.
        SELECT SINGLE rate base
          FROM j_1btxic3
          INTO wl_1btxic
           WHERE land1     = c_br
             AND shipfrom  = p_shipfrom
             AND shipto    = p_shipto
             AND gruop     = c_76
             AND value     = p_080-lifnr
             AND value2    = wl_081-cprod.

        IF sy-subrc NE 0.
          SELECT SINGLE rate
            FROM j_1btxic1
            INTO wl_1btxic
             WHERE land1    = c_br
               AND shipfrom = p_shipfrom
               AND shipto   = p_shipto.
        ENDIF.

      ENDIF.
      MOVE-CORRESPONDING: tg_impo TO tl_impo.


      tl_impo-rate =  wl_1btxic-rate .
      IF wl_1btxic-base > 0 AND  wl_1btxic-rate > 0.
        tl_impo-base = wl_081-netwr / ( ( wl_1btxic-base - wl_1btxic-rate ) / 100 ).
      ENDIF.
      IF tl_impo-base > 0 AND  tl_impo-rate > 0.
        tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
      ENDIF.

*      IF P_082-COMPLEMENTO EQ 'S'.
*        CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
*               TL_IMPO-EXCBAS.
*      ENDIF.

      APPEND tl_impo.
      CLEAR: tl_impo, wl_1btxic.
    ELSE.
      "Aqui outros impostos
      MOVE-CORRESPONDING: tg_impo TO tl_impo.
      MOVE: wl_081-netwr TO tl_impo-othbas.

*      IF P_082-COMPLEMENTO EQ 'S'.
*        CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
*               TL_IMPO-EXCBAS.
*      ENDIF.

      APPEND tl_impo.
      CLEAR: tl_impo.
    ENDIF.

  ENDLOOP.


ENDFORM.

FORM f_monta_agrp_nf  TABLES p_saida_0110    STRUCTURE tg_sai_0110
                             p_0110_agr      STRUCTURE tg_sai_0110
                       USING p_tot_nf        TYPE zglt080-netwr
                             p_tot_desconto  TYPE zglt080-vlr_desconto.

  DATA: vl_agrp_nf         TYPE zglt081-agrp_nf,
        vl_filial_nf       TYPE zglt081-gsber,
        vl_filial_definida TYPE c.

  CLEAR: p_tot_nf, p_tot_desconto.

  LOOP AT p_saida_0110 INTO wa_saida_0110.
    PERFORM f_conversion_input_0110 USING wa_saida_0110.
    MODIFY p_saida_0110 FROM wa_saida_0110.
  ENDLOOP.

  vl_agrp_nf = 1.

  p_0110_agr[] = p_saida_0110[].

*  DELETE P_0110_AGR WHERE ( ( GSBER  IS INITIAL ) OR
*                            ( NFENUM IS INITIAL ) OR
*                            ( SERIES IS INITIAL ) OR
*                            ( BUDAT  IS INITIAL ) OR
*                            ( NETWR  IS INITIAL ) ).

  SORT p_0110_agr BY nfenum series.
  DELETE ADJACENT DUPLICATES FROM p_0110_agr COMPARING nfenum series.
  LOOP AT p_0110_agr.
    CLEAR: p_0110_agr-gsber, p_0110_agr-netwr,p_0110_agr-vlr_desconto,
           vl_filial_nf, vl_filial_definida.

    p_0110_agr-agrp_nf = vl_agrp_nf.
    LOOP AT p_saida_0110 INTO wa_saida_0110 WHERE nfenum = p_0110_agr-nfenum
                                              AND series = p_0110_agr-series.

      IF wa_saida_0110-netwr < 0. "Não acumular partidas de Desconto na NF.
        p_tot_desconto = p_tot_desconto + abs( wa_saida_0110-netwr ).
        CONTINUE.
      ENDIF.

      ADD wa_saida_0110-netwr       TO p_0110_agr-netwr.

      "Atribuição Filial de Lcto da NF.
      IF vl_filial_definida IS INITIAL.

        IF vl_filial_nf IS INITIAL.
          vl_filial_nf = wa_saida_0110-gsber.
          p_0110_agr-gsber = vl_filial_nf.
        ENDIF.

        "Caso a NF possua lançamentos em varias filiais, define Matriz como Filial de Lançamento da NF
        IF vl_filial_nf NE wa_saida_0110-gsber.
          CONCATENATE zglt080-bukrs+2(2) '01' INTO p_0110_agr-gsber.
          vl_filial_definida = 'X'.
        ENDIF.

      ENDIF.

    ENDLOOP.

    ADD p_0110_agr-netwr       TO p_tot_nf.

    p_0110_agr-vlr_liq_ret = p_0110_agr-netwr.

    MODIFY p_0110_agr.

    ADD 1 TO vl_agrp_nf.
  ENDLOOP.

ENDFORM.

FORM f_monta_agrp_irf  TABLES p_irf         STRUCTURE tg_irf
                              p_irf_agr     STRUCTURE tg_irf.

  CLEAR: p_irf_agr[].

  p_irf_agr[] = p_irf[].

  SORT p_irf_agr BY witht.
  DELETE ADJACENT DUPLICATES FROM p_irf_agr COMPARING witht.
  LOOP AT p_irf_agr.
    CLEAR: p_irf_agr-wi_tax_base, p_irf_agr-wi_tax_amt, p_irf_agr-hkont.

    LOOP AT p_irf WHERE witht EQ p_irf_agr-witht
                    AND wi_tax_base > 0.

      ADD p_irf-wi_tax_base  TO p_irf_agr-wi_tax_base.
      ADD p_irf-wi_tax_amt   TO p_irf_agr-wi_tax_amt.

      IF ( p_irf_agr-hkont IS INITIAL ) AND ( p_irf-hkont IS NOT INITIAL ).
        p_irf_agr-hkont = p_irf-hkont.
      ENDIF.

    ENDLOOP.

    MODIFY p_irf_agr.
  ENDLOOP.

ENDFORM.

FORM f_reply_dados_nf_item .

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente replicar os dados da NF para os itens?'
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

  LOOP AT it_saida_0110 INTO wa_saida_0110.
    wa_saida_0110-nfenum = zglt080-nfenum.
    wa_saida_0110-series = zglt080-series.
    wa_saida_0110-budat  = zglt080-budat.
    wa_saida_0110-bldat  = zglt080-bldat.
    MODIFY it_saida_0110 FROM wa_saida_0110 INDEX sy-tabix.
  ENDLOOP.
  PERFORM f_refresh_alv USING '0110'.
ENDFORM.

FORM f_free_0110 .

  IF obj_alv_0110 IS NOT INITIAL.
    CALL METHOD obj_alv_0110->free.
    CALL METHOD cl_gui_cfw=>flush.

    FREE: obj_alv_0110.
  ENDIF.

ENDFORM.

FORM f_valida_data USING p_data  TYPE sy-datum
                         p_tipo  TYPE c
                         p_linha
                CHANGING p_valid.

  DATA: vg_last_day    TYPE sy-datum,
        vg_first_day   TYPE sy-datum,
        wa_zmmt0066    TYPE zmmt0066,
        vl_data_val    TYPE sy-datum,
        vl_n_uteis     TYPE sy-index,
        gt_sab_dom_fer TYPE TABLE OF iscal_day WITH HEADER LINE,
        e_status(1),
        e_messa(64).

  CLEAR: p_valid.

  CASE p_tipo.
    WHEN '1'. " Lançamento

      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          i_bukrs  = zglt080-bukrs
          i_data   = p_data
        IMPORTING
          e_status = e_status
          e_messa  = e_messa
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF e_status = 'E'.
        IF p_linha IS NOT INITIAL.
          vg_message = p_linha.
          CONCATENATE e_messa TEXT-e16 vg_message INTO vg_message SEPARATED BY space.
          MESSAGE vg_message TYPE 'S'.
          PERFORM f_log_gravacao USING 'E' vg_message '' ''.
        ELSE.
          vg_message = e_messa.
          MESSAGE vg_message TYPE 'S'.
          PERFORM f_log_gravacao USING 'E' vg_message '' ''.
        ENDIF.
        RETURN.
      ENDIF.

      "Controle data de lançamento Fiscal
      CONCATENATE p_data(6) '01' INTO vg_first_day.
      CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
        EXPORTING
          i_date = vg_first_day
        IMPORTING
          e_date = vg_last_day.

      REFRESH gt_sab_dom_fer.
      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          factory_calendar           = 'ZF'
          date_from                  = p_data
          date_to                    = vg_last_day
        TABLES
          holidays                   = gt_sab_dom_fer
        EXCEPTIONS
          factory_calendar_not_found = 1
          holiday_calendar_not_found = 2
          date_has_invalid_format    = 3
          date_inconsistency         = 4
          OTHERS                     = 5.

      READ TABLE gt_sab_dom_fer WITH KEY date = p_data.
      IF sy-subrc = 0.
        IF p_linha IS NOT INITIAL.
          MESSAGE s836(sd) WITH TEXT-e34 TEXT-e16 p_linha DISPLAY LIKE 'S'.
          vg_message = p_linha.
          CONCATENATE TEXT-e34 TEXT-e16 vg_message INTO vg_message SEPARATED BY space.
          PERFORM f_log_gravacao USING 'E' vg_message '' ''.
        ELSE.
          MESSAGE s836(sd) WITH TEXT-e34 DISPLAY LIKE 'S'.
          PERFORM f_log_gravacao USING 'E' TEXT-e34 '' ''.
        ENDIF.
        RETURN.
      ELSE.

        "Verifica ultimo dia útil
*        DO.
*          READ TABLE GT_SAB_DOM_FER WITH KEY DATE = VG_LAST_DAY.
*          IF SY-SUBRC NE 0.
*            EXIT.
*          ENDIF.
*          SUBTRACT 1 FROM VG_LAST_DAY.
*        ENDDO.
*        IF P_DATA EQ VG_LAST_DAY.
*          SELECT SINGLE *
*            FROM ZMMT0066
*            INTO WA_ZMMT0066
*           WHERE USNAM = SY-UNAME.
*          IF SY-SUBRC NE 0.
*            IF P_LINHA IS NOT INITIAL.
*              MESSAGE S836(SD) WITH TEXT-E35 TEXT-E36 TEXT-E16 P_LINHA DISPLAY LIKE 'S'.
*              VG_MESSAGE = P_LINHA.
*              CONCATENATE TEXT-E35 TEXT-E36 TEXT-E16 VG_MESSAGE INTO VG_MESSAGE SEPARATED BY SPACE.
*              PERFORM F_LOG_GRAVACAO USING 'E' VG_MESSAGE '' ''.
*            ELSE.
*              MESSAGE S836(SD) WITH TEXT-E35 TEXT-E36 DISPLAY LIKE 'S'.
*              PERFORM F_LOG_GRAVACAO USING 'E' TEXT-E35 TEXT-E36 ''.
*            ENDIF.
*            RETURN.
*          ENDIF.
*        ENDIF.
      ENDIF.

    WHEN '2'. "Vencimento

      vl_data_val = sy-datum.
      ADD 3 TO vl_data_val.

      CLEAR: gt_sab_dom_fer[], vl_n_uteis.
      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          factory_calendar           = 'ZF'
          date_from                  = sy-datum
          date_to                    = p_data
        TABLES
          holidays                   = gt_sab_dom_fer
        EXCEPTIONS
          factory_calendar_not_found = 1
          holiday_calendar_not_found = 2
          date_has_invalid_format    = 3
          date_inconsistency         = 4
          OTHERS                     = 5.

      DESCRIBE TABLE gt_sab_dom_fer LINES vl_n_uteis.
      ADD vl_n_uteis TO vl_data_val.

      IF ( vl_data_val GT p_data ).
        IF p_linha IS NOT INITIAL.
          MESSAGE s836(sd) WITH TEXT-e38 TEXT-e16 p_linha DISPLAY LIKE 'S'.
          vg_message = p_linha.
          CONCATENATE TEXT-e38 TEXT-e16 vg_message INTO vg_message SEPARATED BY space.
          PERFORM f_log_gravacao USING 'E' vg_message '' ''.
        ELSE.
          MESSAGE s836(sd) WITH TEXT-e38 DISPLAY LIKE 'S'.
          PERFORM f_log_gravacao USING 'E' TEXT-e38 '' ''.
        ENDIF.
        RETURN.
      ELSE.
        READ TABLE gt_sab_dom_fer WITH KEY date = p_data.
        IF sy-subrc = 0.
          IF p_linha IS NOT INITIAL.
            MESSAGE s836(sd) WITH TEXT-e37 TEXT-e16 p_linha DISPLAY LIKE 'S'.
            vg_message = p_linha.
            CONCATENATE TEXT-e37 TEXT-e16 vg_message INTO vg_message SEPARATED BY space.
            PERFORM f_log_gravacao USING 'E' vg_message '' ''.
          ELSE.
            MESSAGE s836(sd) WITH TEXT-e37 DISPLAY LIKE 'S'.
            PERFORM f_log_gravacao USING 'E' TEXT-e37 '' ''.
          ENDIF.
          RETURN.
        ENDIF.
      ENDIF.

  ENDCASE.

  p_valid = 'X'.

ENDFORM.

FORM f_gravar_log TABLES tl_return STRUCTURE bapiret2
                   USING VALUE(p_field)
                         p_saida_0110 TYPE ty_saida_0110.

  DATA: wl_cont TYPE sy-tabix,
        wl_086  TYPE zglt086.

  CLEAR: wl_cont.

  DELETE FROM zglt086 WHERE seq_lcto = p_saida_0110-seq_lcto
                        AND agrp_nf  = p_saida_0110-agrp_nf.

  LOOP AT tl_return.
    CLEAR: wl_086.

    ADD 1 TO wl_cont.
    MOVE: p_saida_0110-seq_lcto TO wl_086-seq_lcto,
          p_saida_0110-agrp_nf  TO wl_086-agrp_nf,
          p_field               TO wl_086-field,
          wl_cont               TO wl_086-buzei,
          tl_return-type        TO wl_086-type,
          tl_return-message     TO wl_086-mensagem,
          sy-datum              TO wl_086-erdat,
          sy-uzeit              TO wl_086-ertim.
    MODIFY zglt086 FROM wl_086.

  ENDLOOP.

ENDFORM.

FORM f_montar_layout .

  REFRESH estrutura.
  PERFORM f_montar_estrutura USING:
     1  ''   ''            'GT_086' 'FIELD'    'Field'      '10',
     1  ''   ''            'GT_086' 'BUZEI'    'Cont.'      '05',
     1  ''   ''            'GT_086' 'TYPE'     'Tipo'       '04',
     1  ''   ''            'GT_086' 'MENSAGEM' 'Msg'        '50',
     1  ''   ''            'GT_086' 'ERDAT'    'Data'       '10',
     1  ''   ''            'GT_086' 'ERTIM'    'Hora'       '10'.

ENDFORM.                    " MONTAR_LAYOUT


FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen     = x_contador.
  ELSE.
    wa_estrutura-outputlen     =  p_outputlen.
  ENDIF.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura

FORM f_seleciona_0110  USING  p_saida_0100.

  DATA: wl_086              TYPE zglt086,
        wl_zglt035          TYPE zglt035,
        tg_zimp_contas_cons TYPE TABLE OF zimp_contas_cons WITH HEADER LINE,
        tg_bsik             TYPE TABLE OF bsik WITH HEADER LINE,
        vl_tabix            TYPE sy-tabix,
        tg_zglt081_tmp      TYPE TABLE OF zglt081 WITH HEADER LINE.

  CLEAR: zglt080,
         it_saida_0110[],
         tg_irf[],
         tg_irf_basman[],
         tg_zimp_contas_cons[],
         tg_bsik[].

  MOVE-CORRESPONDING p_saida_0100 TO zglt080.
  SELECT *
    FROM zglt081 INTO CORRESPONDING FIELDS OF TABLE it_saida_0110
   WHERE seq_lcto = zglt080-seq_lcto.

  LOOP AT it_saida_0110 INTO wa_saida_0110.
    vl_tabix = sy-tabix.

    CLEAR: wl_086.

    IF wa_saida_0110-desconto IS NOT INITIAL.
      wa_saida_0110-netwr = abs( wa_saida_0110-netwr ) * -1.
    ENDIF.

    SELECT SINGLE *
      FROM zglt086 INTO wl_086
     WHERE seq_lcto = wa_saida_0110-seq_lcto
       AND agrp_nf  = wa_saida_0110-agrp_nf.

    IF ( wa_saida_0110-docnum IS NOT INITIAL ).
      wa_saida_0110-status_nf = icon_green_light.
    ELSEIF ( wl_086 IS NOT INITIAL ).
      wa_saida_0110-status_nf = icon_red_light.
    ELSE.
      wa_saida_0110-status_nf = icon_light_out.
    ENDIF.

*    Atualiza Contabil -----------------------------------------*
    "Verifica se Doc. Lcto foi estornado.
    IF ( wa_saida_0110-doc_lcto IS NOT INITIAL ) AND
       ( wa_saida_0110-belnr    IS INITIAL ).
      CLEAR: wl_zglt035.
      SELECT SINGLE *
        FROM zglt035 INTO wl_zglt035
       WHERE bukrs    = wa_saida_0110-bukrs
         AND doc_lcto = wa_saida_0110-doc_lcto.

      IF ( ( sy-subrc EQ 0 ) AND ( wl_zglt035-loekz EQ 'X' ) ) OR ( sy-subrc NE 0 ).

        CLEAR: tg_zglt081_tmp[].

        SELECT *
          FROM zglt081 INTO TABLE tg_zglt081_tmp
         WHERE seq_lcto = wa_saida_0110-seq_lcto
           AND doc_lcto = wa_saida_0110-doc_lcto.

        LOOP AT tg_zglt081_tmp.
          PERFORM f_lock_lcto_item USING 'B' tg_zglt081_tmp-seq_lcto tg_zglt081_tmp-seqitem. "Bloqueia Lcto
          IF sy-subrc EQ 0.
            tg_zglt081_tmp-doc_lcto    = space.
            tg_zglt081_tmp-dt_lcto_ctb = space.

            MODIFY zglt081 FROM tg_zglt081_tmp.

            CLEAR: wa_saida_0110-doc_lcto,wa_saida_0110-dt_lcto_ctb.

            PERFORM f_lock_lcto_item USING 'D' tg_zglt081_tmp-seq_lcto tg_zglt081_tmp-seqitem. "Bloqueia Lcto
          ENDIF.
        ENDLOOP.

      ENDIF.
    ENDIF.

    IF ( wa_saida_0110-doc_lcto IS INITIAL ).
      wa_saida_0110-status_ctb  = icon_light_out.
    ELSE.
      IF wa_saida_0110-belnr IS NOT INITIAL.
        wa_saida_0110-status_ctb = icon_green_light.
      ELSE.
        PERFORM f_retorna_status_zib USING wa_saida_0110-doc_lcto
                                           wa_saida_0110-dt_lcto_ctb(4)
                                  CHANGING wa_zib_chave
                                           wa_zib_erro.

        IF ( wa_zib_chave IS NOT INITIAL ).

          CLEAR: tg_zglt081_tmp[].

          SELECT *
            FROM zglt081 INTO TABLE tg_zglt081_tmp
           WHERE seq_lcto = wa_saida_0110-seq_lcto
             AND doc_lcto = wa_saida_0110-doc_lcto.

          LOOP AT tg_zglt081_tmp.
            PERFORM f_lock_lcto_item USING 'B' tg_zglt081_tmp-seq_lcto tg_zglt081_tmp-seqitem. "Bloqueia Lcto
            IF sy-subrc EQ 0.

              tg_zglt081_tmp-belnr  = wa_zib_chave-belnr.
              MODIFY zglt081 FROM tg_zglt081_tmp.

              wa_saida_0110-status_ctb = icon_green_light.
              wa_saida_0110-belnr      = wa_zib_chave-belnr.

              PERFORM f_lock_lcto_item USING 'D' tg_zglt081_tmp-seq_lcto tg_zglt081_tmp-seqitem. "Bloqueia Lcto
            ENDIF.
          ENDLOOP.

        ELSEIF ( wa_zib_erro IS NOT INITIAL ).
          wa_saida_0110-status_ctb = icon_red_light.
        ELSE.
          wa_saida_0110-status_ctb = icon_yellow_light.
        ENDIF.
      ENDIF.

      IF ( wa_saida_0110-belnr      IS NOT INITIAL ) AND
         ( wa_saida_0110-docnum     IS INITIAL ) AND
         ( wa_saida_0110-estorno_nf IS INITIAL ) AND
         ( wa_saida_0110-sem_nf     IS INITIAL ) AND
         ( wa_saida_0110-part_forn  IS INITIAL ) AND
         ( wa_saida_0110-desconto   IS INITIAL ).

        PERFORM f_gerar_nf USING wa_saida_0110
                                 'X'
                        CHANGING wa_saida_0110-docnum.

        PERFORM f_lock_lcto_item USING 'D' wa_saida_0110-seq_lcto wa_saida_0110-seqitem. "Desbloqueia Lcto

      ENDIF.

    ENDIF.

    IF ( wa_saida_0110-belnr IS NOT INITIAL ) AND
       ( wa_saida_0110-cod_barras IS NOT INITIAL ) AND
       ( zglt080-zlsch = 'D' ).
      CLEAR: tg_zimp_contas_cons, tg_bsik.

      SELECT SINGLE *
        FROM zimp_contas_cons INTO tg_zimp_contas_cons
       WHERE bukrs = wa_saida_0110-bukrs
         AND belnr = wa_saida_0110-belnr
         AND gjahr = wa_saida_0110-dt_lcto_ctb(4).

      IF ( sy-subrc NE 0 ) OR
         ( ( sy-subrc = 0  ) AND ( tg_zimp_contas_cons-cod_barras IS INITIAL ) ).

        SELECT SINGLE *
          FROM bsik INTO tg_bsik
         WHERE bukrs = wa_saida_0110-bukrs
           AND gjahr = wa_saida_0110-dt_lcto_ctb(4)
           AND belnr = wa_saida_0110-belnr.

        IF sy-subrc = 0.
          MOVE-CORRESPONDING tg_bsik TO tg_zimp_contas_cons.
          tg_zimp_contas_cons-cod_barras = wa_saida_0110-cod_barras.
          MODIFY zimp_contas_cons FROM tg_zimp_contas_cons.
        ENDIF.
      ENDIF.

    ENDIF.

    MODIFY it_saida_0110 FROM wa_saida_0110 INDEX vl_tabix.
  ENDLOOP.


ENDFORM.

FORM f_estorno_nf .

  DATA: vl_doc_est  TYPE j_1bnfdoc-docnum,
        vl_valid_dt TYPE c.

  CHECK vg_opr_lcto = c_display.

  FIELD-SYMBOLS: <saida_0110> TYPE ty_saida_0110.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0110->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  IF lines( it_sel_rows[] ) >  1.
    MESSAGE s836(sd) WITH TEXT-e19 DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente estornar a Nota Fiscal selecionada?'
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

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0110 ASSIGNING <saida_0110> INDEX wa_sel_rows-index.
    CHECK sy-subrc = 0.
    CHECK <saida_0110>-docnum IS NOT INITIAL.

    PERFORM f_valida_periodo USING <saida_0110>-bukrs
                                   <saida_0110>-budat
                          CHANGING vl_valid_dt.

    CHECK vl_valid_dt IS NOT INITIAL.

    CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
      EXPORTING
        doc_number               = <saida_0110>-docnum
        ref_type                 = space
        ref_key                  = space
        can_dat                  = sy-datum
      IMPORTING
        doc_number               = vl_doc_est
      EXCEPTIONS
        document_not_found       = 1
        cancel_not_possible      = 2
        nf_cancel_type_not_found = 3
        database_problem         = 4
        docum_lock               = 5
        nfe_cancel_simulation    = 6
        OTHERS                   = 7.
    IF ( sy-subrc EQ 0 ) AND ( vl_doc_est IS NOT INITIAL ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      UPDATE zglt081 SET docnum     = space
                         estorno_nf = 'X'
        WHERE seq_lcto = <saida_0110>-seq_lcto
          AND agrp_nf  = <saida_0110>-agrp_nf.
    ENDIF.
  ENDLOOP.

  PERFORM f_seleciona_0110 USING wa_saida_0100.
  PERFORM f_refresh_alv USING '0110'.

ENDFORM.

FORM f_get_parametro  USING    p_idx   TYPE string
                               p_080   TYPE zglt080
                               p_081   TYPE zglt081
                      CHANGING p_082   TYPE zglt082
                               p_valid TYPE c.

  CLEAR: p_valid.

  SELECT SINGLE *
    FROM zglt082 INTO p_082
   WHERE bukrs = p_081-bukrs
     AND wekrs = p_081-gsber_nf
     AND lifnr = p_080-lifnr.

  IF sy-subrc NE 0.
    SELECT SINGLE *
      FROM zglt082 INTO p_082
     WHERE bukrs = p_081-bukrs
       AND wekrs = p_081-gsber_nf
       AND lifnr = space.

    IF sy-subrc NE 0 .
      SELECT SINGLE *
        FROM zglt082 INTO p_082
       WHERE lifnr = p_080-lifnr
         AND bukrs = space
         AND wekrs = space.

      IF sy-subrc NE 0.
        SELECT SINGLE *
          FROM zglt082 INTO p_082
         WHERE bukrs = space
           AND wekrs = space
           AND lifnr = space.

        IF sy-subrc NE 0.
          MESSAGE s836(sd) WITH TEXT-e48 TEXT-e16 p_idx DISPLAY LIKE 'S'.
          vg_message = p_idx.
          CONCATENATE TEXT-e48 TEXT-e16 vg_message INTO vg_message SEPARATED BY space.
          PERFORM f_log_gravacao USING 'E' vg_message '' ''.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_082-cfop IS INITIAL.
    vg_message = | Parâmetro encontrado( Opr. { p_082-operacao } ), não possui CFOP informado. Verificar com a Área Fiscal - Linha: { p_idx }|.
    MESSAGE vg_message TYPE 'S'.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ENDIF.

  IF p_082-itmtyp IS INITIAL.
    vg_message = | Parâmetro encontrado( Opr. { p_082-operacao } ), não possui o Tipo de item da nota fiscal informado. Verificar com a Área Fiscal - Linha: { p_idx }|.
    MESSAGE vg_message TYPE 'S'.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ENDIF.

  IF p_082-nftype IS INITIAL.
    vg_message = | Parâmetro encontrado( Opr. { p_082-operacao } ), não possui Ctg.de nota fiscal informada. Verificar com a Área Fiscal - Linha: { p_idx }|.
    MESSAGE vg_message TYPE 'S'.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ENDIF.

  p_valid = 'X'.

ENDFORM.

FORM f_renew_consulta  USING  p_alv.

  CASE p_alv.
    WHEN '0100'.
      PERFORM: f_selecionar_dados,
               f_processa_dados,
               f_refresh_alv USING '0100'.
    WHEN '0110'.
  ENDCASE.


ENDFORM.

FORM f_dados_nf_agrp .

  DATA: vl_tot_nf   TYPE zglt080-netwr,
        vl_tot_desc TYPE zglt080-netwr,
        vl_error    TYPE c,
        vl_lcto_irf TYPE c.

  CHECK it_saida_0110[] IS NOT INITIAL.

  CLEAR: it_saida_0112[].

  "Configuração Edição Campos
  CLEAR: vg_modify_cbar, vg_modify_irf.

  CASE vg_opr_lcto.
    WHEN c_display.
    WHEN c_edit.
      vg_modify_irf = 'X'.

      IF ( ( zglt080-zlsch = 'E'  ) OR ( zglt080-zlsch = 'D' ) ).
        vg_modify_cbar = 'X'.
      ENDIF.
    WHEN c_new.
      vg_modify_irf = 'X'.

      IF ( ( zglt080-zlsch = 'E'  ) OR ( zglt080-zlsch = 'D' ) ).
        vg_modify_cbar = 'X'.
      ENDIF.
  ENDCASE.

  IF obj_alv_0112 IS NOT INITIAL.
    CALL METHOD obj_alv_0112->free.
    CALL METHOD cl_gui_cfw=>flush.
    FREE: obj_alv_0112.
  ENDIF.

  IF obj_alv_0113 IS NOT INITIAL.
    CALL METHOD obj_alv_0113->free.
    CALL METHOD cl_gui_cfw=>flush.
    FREE: obj_alv_0113.
  ENDIF.

  PERFORM f_monta_agrp_nf TABLES it_saida_0110
                                 it_saida_0112
                           USING vl_tot_nf
                                 vl_tot_desc.

  IF ( vg_opr_lcto EQ c_new ) OR ( vg_opr_lcto EQ c_edit ).
    "Gerar Dados IRF para as Notas Fiscais.
    PERFORM f_gerar_dados_irf TABLES it_saida_0112
                               USING vl_error
                                     vl_lcto_irf.

  ELSEIF ( vg_opr_lcto EQ c_display ).
    CLEAR: tg_irf[].
  ENDIF.

  DELETE it_saida_0112 WHERE ( ( gsber  IS INITIAL ) OR
                               ( nfenum IS INITIAL ) OR
                               ( series IS INITIAL ) OR
                               ( budat  IS INITIAL ) OR
                               ( netwr  IS INITIAL ) ).

  IF ( vg_opr_lcto EQ c_display ).
    SELECT *
      FROM zglt089 INTO TABLE @DATA(gt_089)
     WHERE seq_lcto = @zglt080-seq_lcto.

    IF gt_089[] IS NOT INITIAL.
      LOOP AT it_saida_0112 INTO wa_saida_0112.
        DATA(_tabix) = sy-tabix.

        LOOP AT gt_089 INTO DATA(wl_089) WHERE nfenum  = wa_saida_0112-nfenum
                                           AND series  = wa_saida_0112-series.

          SUBTRACT wl_089-wi_tax_amt FROM wa_saida_0112-vlr_liq_ret.
        ENDLOOP.

        MODIFY it_saida_0112 FROM wa_saida_0112 INDEX _tabix.
      ENDLOOP.
    ENDIF.
  ENDIF.

  CALL SCREEN 0112 STARTING AT 04 04 ENDING AT 110 19.


ENDFORM.

FORM f_atrib_forma_pagto.

  DATA: wl_lfbk TYPE lfbk.

  CHECK ( zglt080-lifnr IS NOT INITIAL ) AND
        ( zglt080-bukrs IS NOT INITIAL ).

  CHECK vg_opr_lcto NE c_display.

  CLEAR: zglt080-hbkid.

  TRY .
      zcl_miro=>get_formapag_banco_empresa( EXPORTING i_bukrs           = zglt080-bukrs
                                                      i_lifnr           = zglt080-lifnr
                                                      i_bvtyp           = zglt080-bvtyp
                                                      i_zlsch           = zglt080-zlsch
                                            IMPORTING e_banco_empresa   = zglt080-hbkid
                                                      e_forma_pagamento = zglt080-zlsch ).
      IF zglt080-hbkid IS INITIAL.
        MESSAGE s836(sd) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'S'.
        EXIT.
      ENDIF.
    CATCH zcx_miro_exception INTO DATA(ex_miro).  "
      ex_miro->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
      RETURN.
  ENDTRY.

  IF 1 = 2.
    IF zglt080-hbkid = 'BBD'.
      zglt080-zlsch = 'D'.
    ELSEIF zglt080-hbkid = 'BBRA'.
      IF zglt080-bvtyp IS INITIAL.
        zglt080-zlsch = 'E'.
      ELSE.
        CLEAR: wl_lfbk.
        SELECT SINGLE *
          FROM lfbk INTO wl_lfbk
         WHERE lifnr = zglt080-lifnr
           AND bvtyp = zglt080-bvtyp.

        IF sy-subrc = 0.
          IF wl_lfbk-bankl(3) = '001'.
            zglt080-zlsch = 'U'.
          ELSE.
            zglt080-zlsch = 'S'.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

  IF ( zglt080-bukrs IS NOT INITIAL ) AND ( zglt080-hbkid IS NOT INITIAL ) AND ( zglt080-bvtyp IS INITIAL  ).
    CONCATENATE zglt080-bukrs '-' zglt080-hbkid '%' INTO DATA(_bukrs_hbkid).

    SELECT SINGLE *
      FROM setleaf INTO @DATA(wl_setleaf)
     WHERE setname = 'MAGGI_ZGL059_ZLSCH'
       AND valfrom LIKE @_bukrs_hbkid.

    IF ( sy-subrc = 0 ).
      DATA(_tam) = strlen( wl_setleaf-valfrom ).
      SUBTRACT 1 FROM _tam.
      zglt080-zlsch =  wl_setleaf-valfrom+_tam(1).
    ENDIF.
  ENDIF.

ENDFORM.


FORM f_valida_periodo USING p_bukrs  TYPE zglt080-bukrs
                            p_budat  TYPE zglt080-budat
                   CHANGING p_valida.

  DATA: vl_data_val TYPE sy-datum,
        vl_budat2   TYPE zglt080-budat,
        sano(4),
        vano        TYPE i,
        smes(2),
        vmes        TYPE i.

  CLEAR: p_valida.

  CLEAR: vl_data_val, vl_budat2, sano, vano, smes, vmes.

  CALL FUNCTION 'Z_RET_DATA_MES_ABERTO'
    EXPORTING
      p_data_ent  = p_budat
      p_bukrs     = p_bukrs
    IMPORTING
      p_data_val  = vl_data_val
    EXCEPTIONS
      sem_periodo = 1
      OTHERS      = 2.

  IF vl_data_val+0(6) NE p_budat+0(6) AND p_budat+4(2) LE 12.
    MESSAGE s836(sd) WITH TEXT-e49 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' TEXT-e49 '' ''.
    RETURN.
  ENDIF.

  p_valida  = 'X'.

ENDFORM.

FORM f_monta_dados_ext .

  DATA: r_gerar_lote TYPE REF TO zcl_gerar_lote,
        tg_zmmt0024  TYPE TABLE OF zmmt0024 WITH HEADER LINE.

  CREATE OBJECT r_gerar_lote.

  IF vg_objkey_proc IS NOT INITIAL. "Telefonia

    "Deleta Logs.
    DELETE FROM zglt087 WHERE objkey = vg_objkey_proc.
    COMMIT WORK.

    CLEAR: zglt080, it_saida_0110[], wa_saida_0110, tg_zmmt0024[].

    SELECT *
      FROM zmmt0024 INTO TABLE tg_zmmt0024
     WHERE objkey = vg_objkey_proc.

    CHECK tg_zmmt0024[] IS NOT INITIAL.

    "Preeche cabeçalho documento.

    READ TABLE tg_zmmt0024 INDEX 1.

    r_gerar_lote->create_lote( EXPORTING i_bukrs      = tg_zmmt0024-bukrs
                                         i_descr_lote = 'Telefonia'
                                         i_dep_resp   = '46'
                                         i_user_resp  = sy-uname
                               IMPORTING e_num_lote   = zglt080-lote ).


    zglt080-bukrs       = tg_zmmt0024-bukrs.
    zglt080-lifnr       = tg_zmmt0024-flief.
    zglt080-hbkid       = tg_zmmt0024-hbkid.
    zglt080-netwr       = tg_zmmt0024-brtwr.
    zglt080-bktxt       = ''.
    zglt080-zlsch       = tg_zmmt0024-zlsch.
    zglt080-bvtyp       = tg_zmmt0024-bvtyp.
    zglt080-zfbdt       = tg_zmmt0024-zfbdt.

    "Preenche Itens
    LOOP AT tg_zmmt0024.
      CLEAR: wa_saida_0110.
      wa_saida_0110-hkont       = tg_zmmt0024-saknr.
      wa_saida_0110-gsber       = tg_zmmt0024-branch.
      wa_saida_0110-kostl       = tg_zmmt0024-kostl.
      wa_saida_0110-vbeln       = tg_zmmt0024-bednr.
      wa_saida_0110-budat       = sy-datum.
      wa_saida_0110-bldat       = tg_zmmt0024-bldat.
      wa_saida_0110-menge       = tg_zmmt0024-menge.
      wa_saida_0110-netwr       = tg_zmmt0024-brtwr.
      wa_saida_0110-nfenum      = tg_zmmt0024-nr_documento.
      wa_saida_0110-series      = tg_zmmt0024-series.
      wa_saida_0110-matnr       = tg_zmmt0024-matnr.
      wa_saida_0110-sgtxt       = tg_zmmt0024-txz01.
      wa_saida_0110-cod_barras  = tg_zmmt0024-cod_barras.
      wa_saida_0110-objkey      = tg_zmmt0024-objkey.
      wa_saida_0110-bnfpo       = tg_zmmt0024-bnfpo.

      APPEND wa_saida_0110 TO it_saida_0110.
    ENDLOOP.

    RETURN.
  ENDIF.

ENDFORM.

FORM f_log_gravacao  USING  p_type     TYPE zglt087-type
                            p_msg      TYPE zglt087-message
                            p_msg_2    TYPE zglt087-message
                            p_seq_lcto TYPE zglt087-message.

  DATA: wl_zglt087  TYPE zglt087,
        vl_seq_lcto TYPE zglt080-seq_lcto.

  CHECK vg_call_ext IS NOT INITIAL.
  CHECK vg_objkey_proc IS NOT INITIAL.
  CHECK p_msg IS NOT INITIAL.

  wl_zglt087-objkey    = vg_objkey_proc.
  wl_zglt087-id        = vg_id_log.
  wl_zglt087-type      = p_type.
  wl_zglt087-data      = sy-datum.
  wl_zglt087-hora      = sy-uzeit.
  wl_zglt087-seq_lcto  = p_seq_lcto.

  IF p_msg_2 IS INITIAL.
    wl_zglt087-message = p_msg.
  ELSE.
    CONCATENATE p_msg p_msg_2 INTO wl_zglt087-message SEPARATED BY space.
  ENDIF.

  MODIFY zglt087 FROM wl_zglt087.

  ADD 1 TO vg_id_log.


ENDFORM.

FORM f_importar_anexos.

  DATA: folder_id TYPE sofdk,
        owner_dat TYPE soud3,
        vl_error  TYPE c.

  DATA: it_file_table	  TYPE filetable,
        wa_file_table   TYPE file_table,
        lc_rc	          TYPE i,
        vl_file_table_c TYPE string.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  CHECK sy-subrc = 0.
  READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.
  CHECK sy-subrc = 0.

  CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
    EXPORTING
      region    = 'B'
    IMPORTING
      folder_id = folder_id
    EXCEPTIONS
      OTHERS    = 0.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Importar Arquivo '
      file_filter             = 'Files PDF (*.PDF)|*.PDF|'
      multiselection          = abap_true
      initial_directory       = 'C:\Amaggi\'
    CHANGING
      file_table              = it_file_table
      rc                      = lc_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  CLEAR: vl_error.
  LOOP AT it_file_table INTO wa_file_table.
    vl_file_table_c = wa_file_table.
    PERFORM f_create_anexos USING wa_saida_0100 folder_id vl_file_table_c vl_error.
    IF vl_error IS NOT INITIAL.
      RETURN.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM f_create_anexos  USING  p_saida_0100    TYPE ty_saida_0100
                             p_folder_id     TYPE sofdk
                             p_path_and_file TYPE string
                    CHANGING p_error.

  MESSAGE s003(vpd).


  DATA: back LIKE sy-ucomm VALUE 'BAC',  " Back
        canc LIKE sy-ucomm VALUE 'ESC',  " Cancel
        stop LIKE sy-ucomm VALUE 'RET'.  " Terminate SAPoffice

  DATA is_object  TYPE borident.
  DATA attachment TYPE borident.
  DATA documents  TYPE STANDARD TABLE OF sood4.

  DATA document   TYPE sood4.
  DATA ep_attachment TYPE char40.

  DATA l_cancelled   LIKE sonv-flag.
  DATA bin_filesize  LIKE soxwd-doc_length.
  DATA file_format   LIKE rlgrap-filetype.
  DATA path_and_file TYPE string.
  DATA object_type   LIKE soodk-objtp.
  DATA put_to_kpro   LIKE sonv-flag.
  DATA p_objcont     TYPE TABLE OF soli.
  DATA p_objhead     TYPE TABLE OF soli.
  DATA p_objpara     TYPE TABLE OF selc.
  DATA p_objparb     TYPE TABLE OF soop1.

  DATA doc_id      LIKE soodk.
  DATA att_id      LIKE soodk.
  DATA hd_dat      LIKE sood1.
  DATA fm_dat      LIKE sofm1.
  DATA new_doc_id  LIKE soodk.
  DATA new_att_id  LIKE soodk.
  DATA new_hd_dat  LIKE sood2.
  DATA new_fm_dat  LIKE sofm2.
  DATA old_doc_id  LIKE soodk.
  DATA fol_dat     LIKE sofdd.
  DATA old_enccnt  LIKE sood-enccnt.
  DATA attach_list LIKE sood5 OCCURS 0 WITH HEADER LINE.
  DATA link_list   LIKE soodk OCCURS 0 WITH HEADER LINE.
  DATA l_reappear  LIKE sonv-flag.
  DATA l_answer.
  DATA l_filename           TYPE string.
  DATA reference_type_kpro VALUE 'K'.      "KPro reference
  DATA obj_type             LIKE soodk-objtp.
  DATA owner_dat            LIKE soud3.



  DATA  p_header_data LIKE sood2.
  DATA  p_folmem_data LIKE sofm2.
  DATA  l_name TYPE string.                                 "1041757 >>
  DATA  g_document     LIKE sood4.
  DATA  lo_objhead TYPE REF TO cl_bcs_objhead.

  IF owner IS INITIAL.
    CLEAR owner_dat.
    MOVE sy-uname TO owner_dat-sapnam.
    CALL FUNCTION 'SO_NAME_CONVERT'
      EXPORTING
        name_in               = owner_dat
        no_address_name       = 'X'
      IMPORTING
        name_out              = owner_dat
      EXCEPTIONS
        communication_failure = 71
        office_name_not_exist = 19
        parameter_error       = 23
        sap_name_not_exist    = 29
        system_failure        = 72
        user_not_exist        = 34.
    IF sy-subrc NE 0.
      p_error = 'X'.
      RETURN.
    ENDIF.

    MOVE owner_dat-usrnam TO owner.

    CALL FUNCTION 'SO_FOLDER_HEADER_READ'
      EXPORTING
        folder_id                  = p_folder_id
        owner                      = owner
      IMPORTING
        folder_data                = sofd_dat
      EXCEPTIONS
        communication_failure      = 71
        folder_not_exist           = 6
        operation_no_authorization = 21
        system_failure             = 72.

    IF sy-subrc NE 0.
      p_error = 'X'.
      RETURN.
    ENDIF.

  ENDIF.

  g_document-foltp = p_folder_id-foltp.
  g_document-folyr = p_folder_id-folyr.
  g_document-folno = p_folder_id-folno.
  g_document-folrg = sofd_dat-folrg.

  CALL FUNCTION 'SO_OBJECT_UPLOAD'
    EXPORTING
      filetype                = 'BIN'
      path_and_file           = p_path_and_file
      no_dialog               = 'X'
    IMPORTING
      f_cancelled             = l_cancelled
      filelength              = bin_filesize
      act_filetype            = file_format
      act_filename            = path_and_file
      act_objtype             = object_type
      file_put_to_kpro        = put_to_kpro
    TABLES
      objcont                 = p_objcont
    EXCEPTIONS
      invalid_type            = 1
      object_type_not_allowed = 2
      kpro_insert_error       = 3
      file_to_large           = 4
      OTHERS                  = 5.

  CASE sy-subrc.
    WHEN 0.
      IF NOT l_cancelled IS INITIAL.
        MESSAGE s118(so). g_document-okcode = canc. EXIT.
      ENDIF.
      hd_dat-objlen = bin_filesize.

      IF NOT put_to_kpro IS INITIAL.
        hd_dat-extct = reference_type_kpro.
      ENDIF.

      CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
        EXPORTING
          full_name     = path_and_file
        IMPORTING
          stripped_name = l_filename.

      CLEAR: p_objhead, p_objhead[].
      lo_objhead = cl_bcs_objhead=>create( p_objhead[] ).
      lo_objhead->set_filename( l_filename ).
      lo_objhead->set_format( file_format ).
      lo_objhead->set_vsi_profile( cl_bcs_vsi_profile=>get_profile( ) ).
      p_objhead[] = lo_objhead->mt_objhead.
      hd_dat-file_ext = object_type.

      "if hd_dat-objdes is initial.
**       split l_filename at '.' into hd_dat-objdes l_filename.
      PERFORM so_split_file_and_extension(saplso30)
                                          USING l_filename
                                                hd_dat-objdes
                                                object_type.
      g_document-objdes = hd_dat-objdes.
      "endif.

    WHEN 1.
      p_error = 'X'.
      MESSAGE i422(so) WITH file_format. g_document-okcode = canc.
      EXIT.
    WHEN 2.
      p_error = 'X'.
      MESSAGE i322(so). g_document-okcode = canc.
      EXIT.
    WHEN 3.
      p_error = 'X'.
      MESSAGE i444(so). g_document-okcode = canc.
      EXIT.
    WHEN 4.
      p_error = 'X'.
      MESSAGE i425(so). g_document-okcode = canc.
      EXIT.
    WHEN 5.
      p_error = 'X'.
      DATA lv_done TYPE c.
      lv_done = cl_bcs_vsi_profile=>output_vsi_error( ).
      IF lv_done IS INITIAL.
        MESSAGE i424(so).
      ENDIF.
      g_document-okcode = 'ESC'.
      EXIT.
  ENDCASE.

  hd_dat-file_ext = object_type.
  obj_type = 'EXT'.

  CALL FUNCTION 'SO_OBJECT_INSERT'
    EXPORTING
      folder_id                  = p_folder_id
      object_type                = obj_type
      object_hd_change           = hd_dat
      object_fl_change           = fm_dat
      owner                      = owner
    IMPORTING
      object_id                  = new_doc_id
      object_hd_display          = new_hd_dat
      object_fl_display          = new_fm_dat
    TABLES
      objcont                    = p_objcont
      objhead                    = p_objhead
      objpara                    = p_objpara
      objparb                    = p_objparb
    EXCEPTIONS
      active_user_not_exist      = 35
      communication_failure      = 71
      component_not_available    = 1
      dl_name_exist              = 3
      folder_no_authorization    = 5
      folder_not_exist           = 6
      object_type_not_exist      = 17
      operation_no_authorization = 21
      owner_not_exist            = 22
      parameter_error            = 23
      substitute_not_active      = 31
      substitute_not_defined     = 32
      system_failure             = 72
      x_error                    = 1000.

  IF sy-subrc > 0.
    p_error = 'X'.
    RETURN.
  ELSE.
    MOVE: new_doc_id-objtp TO g_document-objtp,
          new_doc_id-objyr TO g_document-objyr,
          new_doc_id-objno TO g_document-objno,
          new_hd_dat       TO p_header_data,
          new_fm_dat       TO p_folmem_data.
    g_document-objnam = new_hd_dat-objnam.
    g_document-objdes = new_hd_dat-objdes.
    g_document-okcode = 'CREA'.
    IF sy-batch IS INITIAL AND sy-binpt IS INITIAL.
      MESSAGE s109(so).
    ENDIF.
  ENDIF.

  is_object-objtype = 'ZGL059'.
  is_object-objkey  = p_saida_0100-seq_lcto.

  IF g_document-okcode = 'CREA' OR g_document-okcode = 'CHNG'.
    attachment-objtype = 'MESSAGE'.
    attachment-objkey  =  g_document(34).
    CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
      EXPORTING
        obj_rolea    = is_object
        obj_roleb    = attachment
        relationtype = 'ATTA'
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc = 0.
      MESSAGE 'Arquivo(s) Anexado(s)' TYPE 'S'.
    ENDIF.
  ENDIF.




ENDFORM.


FORM f_gerar_dados_irf TABLES p_0110_agr STRUCTURE tg_sai_0110
                        USING p_error
                              p_lcto_irf.

  DATA: xbkpf       TYPE TABLE OF bkpf WITH HEADER LINE,
        xbseg       TYPE TABLE OF bseg WITH HEADER LINE,
        xbset       TYPE TABLE OF bset WITH HEADER LINE,
        it_lfbw     TYPE TABLE OF lfbw WITH HEADER LINE,
        x_with_item TYPE TABLE OF with_itemx WITH HEADER LINE,
        wl_x001     TYPE x001,
        tg_t059u    TYPE TABLE OF t059u WITH HEADER LINE.

  CLEAR: tg_irf[], p_lcto_irf, p_error.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = zglt080-bukrs
    IMPORTING
      output = zglt080-bukrs.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = zglt080-lifnr
    IMPORTING
      output = zglt080-lifnr.


  IF zglt080-bukrs IS INITIAL.
    p_error = 'X'.
    MESSAGE s836(sd) WITH TEXT-e52 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' TEXT-e52 '' ''.
    RETURN.
  ENDIF.

  IF zglt080-lifnr IS INITIAL.
    p_error = 'X'.
    MESSAGE s836(sd) WITH TEXT-e53 DISPLAY LIKE 'S'.
    PERFORM f_log_gravacao USING 'E' TEXT-e53 '' ''.
    RETURN.
  ENDIF.

  CALL FUNCTION 'FI_CHECK_EXTENDED_WT'
    EXPORTING
      i_bukrs              = zglt080-bukrs
    EXCEPTIONS
      component_not_active = 1
      not_found            = 2
      OTHERS               = 3.

  CHECK sy-subrc = 0.   "extended functionality active

  SELECT * INTO TABLE it_lfbw
    FROM lfbw
   WHERE lifnr = zglt080-lifnr
     AND bukrs = zglt080-bukrs.

  CHECK it_lfbw[] IS NOT INITIAL.

*  IF IT_LFBW[] IS INITIAL.
*    LOOP AT P_0110_AGR.
*      P_0110_AGR-VLR_LIQ_RET = P_0110_AGR-NETWR.
*      MODIFY P_0110_AGR.
*    ENDLOOP.
*
*    RETURN.
*  ENDIF.

  p_lcto_irf = 'X'.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs = zglt080-bukrs
    IMPORTING
      e_x001  = wl_x001.

  IF sy-subrc NE 0.
    p_error = 'X'.
    MESSAGE s836(sd) WITH TEXT-e54 TEXT-e55 DISPLAY LIKE 'S'.
    CONCATENATE TEXT-e54 TEXT-e55 vg_message INTO vg_message SEPARATED BY space.
    PERFORM f_log_gravacao USING 'E' vg_message '' ''.
    RETURN.
  ENDIF.

  "Quando pagamento for por boleto, caso os codigos de barras sejam todos iguais,
  "marcar para gerar uma partida de fornecedor agrupada,
  PERFORM f_cbar_unico TABLES it_saida_0110.

  "Lançar Partida Fornecedor por NF
  LOOP AT p_0110_agr.

    CLEAR: xbkpf, xbseg, xbkpf[], xbseg[], x_with_item[].

    CHECK ( p_0110_agr-netwr IS NOT INITIAL ) AND
          ( p_0110_agr-bldat IS NOT INITIAL ) AND
          ( p_0110_agr-budat IS NOT INITIAL ).

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = p_0110_agr-bldat
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    CHECK ( sy-subrc EQ 0 ).

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = p_0110_agr-budat
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    CHECK ( sy-subrc EQ 0 ).

    "Refresh Data Global Memory
    CALL FUNCTION 'FI_WT_DELETE_KONTAB'
      EXPORTING
        i_clearing_date = p_0110_agr-budat
      EXCEPTIONS
        OTHERS          = 1.

    CALL FUNCTION 'FI_WT_DELETE_TAX_AMOUNTS'
      EXCEPTIONS
        OTHERS = 1.

    xbkpf-bukrs = zglt080-bukrs.
    xbkpf-gjahr = p_0110_agr-budat(4).
    xbkpf-blart = 'LM'.
    xbkpf-bldat = p_0110_agr-bldat.
    xbkpf-budat = p_0110_agr-budat.
    xbkpf-monat = p_0110_agr-budat+4(2).
    xbkpf-usnam = sy-uname.
    xbkpf-waers = 'BRL'.

    xbkpf-basw2 = wl_x001-basw2.
    xbkpf-basw3 = wl_x001-basw3.
    xbkpf-kuty2 = wl_x001-kuty2.
    xbkpf-kuty3 = wl_x001-kuty3.
    xbkpf-hwaer = 'BRL'.
    xbkpf-hwae2 = wl_x001-hwae2.
    xbkpf-hwae3 = wl_x001-hwae3.
    xbkpf-umrd2 = wl_x001-umrd2.
    xbkpf-umrd3 = wl_x001-umrd3.
    xbkpf-curt2 = wl_x001-curt2.
    xbkpf-curt3 = wl_x001-curt3.
    xbkpf-wwert = sy-datum.

    APPEND xbkpf.

    xbseg-bukrs = xbkpf-bukrs.
    xbseg-gjahr = xbkpf-gjahr.
    xbseg-buzei = '001'.
    xbseg-bschl = '31'.
    xbseg-koart = 'K'.
    xbseg-shkzg = 'H'.
    xbseg-gsber = p_0110_agr-gsber.
    xbseg-qsskz = 'XX'.
    xbseg-dmbtr = p_0110_agr-netwr.
    xbseg-wrbtr = p_0110_agr-netwr.
    xbseg-lifnr = zglt080-lifnr.
    xbseg-zfbdt = zglt080-zfbdt.
    "XBSEG-ZTERM = 'Z001'.

    PERFORM f_converte_moeda USING p_0110_agr-netwr
                                   'BRL'
                                   wl_x001-hwae2
                          CHANGING xbseg-dmbe2.


    PERFORM f_converte_moeda USING p_0110_agr-netwr
                                   'BRL'
                                   wl_x001-hwae3
                          CHANGING xbseg-dmbe3.

    APPEND xbseg.

    READ TABLE xbseg INDEX 1.

    LOOP AT it_lfbw.

      CLEAR: x_with_item.

      x_with_item-bukrs      = xbseg-bukrs .
      x_with_item-belnr      = xbseg-belnr .
      x_with_item-gjahr      = xbseg-gjahr .
      x_with_item-buzei      = xbseg-buzei .
      x_with_item-koart      = xbseg-koart .
      x_with_item-wt_acco    = xbseg-lifnr .

      x_with_item-witht      = it_lfbw-witht .
      x_with_item-wt_withcd  = it_lfbw-wt_withcd .
      x_with_item-wt_oldbuz  = xbseg-buzei.

      "Verifica se Usuario Informou base IRF
      READ TABLE tg_irf_basman WITH KEY nfenum    = p_0110_agr-nfenum
                                        series    = p_0110_agr-series
                                        witht     = x_with_item-witht.
      IF sy-subrc = 0.
        x_with_item-wt_basman = 'X'.
        x_with_item-wt_qsshb  = tg_irf_basman-wi_tax_base.

        IF tg_irf_basman-bas_neg IS NOT INITIAL.
          x_with_item-wt_qsshb = abs( x_with_item-wt_qsshb ) * -1.
        ENDIF.

        IF tg_irf_basman-wt_taxman IS NOT INITIAL. "Montante IRF entrado manualmente
          x_with_item-wt_amnman = 'X'.
          x_with_item-wt_qbshb  = tg_irf_basman-wi_tax_amt.
          x_with_item-wt_qbuihb = tg_irf_basman-wi_tax_amt.

          IF tg_irf_basman-bas_neg IS NOT INITIAL.
            x_with_item-wt_qbshb  = abs( x_with_item-wt_qbshb  ) * -1.
            x_with_item-wt_qbuihb = abs( x_with_item-wt_qbuihb ) * -1.
          ENDIF.
        ENDIF.

      ENDIF.

      APPEND x_with_item .

    ENDLOOP.

*   Put with item data in global memory
    CALL FUNCTION 'FI_WT_PUT_X_WITH_ITEM'
      TABLES
        t_with_item = x_with_item.

    CALL FUNCTION 'FI_WT_FB01_CALCULATE_WT'
      EXPORTING
        i_aktyp = 'H'
        i_dyncl = 'B'
      TABLES
        i_bkpf  = xbkpf
        i_bseg  = xbseg
        i_bset  = xbset
      EXCEPTIONS
        OTHERS  = 1.

    IF sy-subrc NE 0.
      p_error = 'X'.
      MESSAGE s836(sd) WITH TEXT-e56 DISPLAY LIKE 'S'.
      PERFORM f_log_gravacao USING 'E' TEXT-e56 '' ''.
      RETURN.
    ENDIF.

    CALL FUNCTION 'FI_WT_GET_X_WITH_ITEM'
      TABLES
        t_with_item = x_with_item.

    READ TABLE xbseg INDEX 1.
    IF ( sy-subrc = 0 ) AND ( abs( xbseg-dmbtr ) > 0 ).
      p_0110_agr-vlr_liq_ret = abs( xbseg-dmbtr ).
      MODIFY p_0110_agr.
    ELSE.
      p_error = 'X'.
      MESSAGE s836(sd) WITH TEXT-e56 DISPLAY LIKE 'S'.
      PERFORM f_log_gravacao USING 'E' TEXT-e56 '' ''.
      RETURN.
    ENDIF.

    LOOP AT x_with_item.
      CLEAR: tg_irf.

      tg_irf-gsber        = p_0110_agr-gsber.
      tg_irf-nfenum       = p_0110_agr-nfenum.
      tg_irf-series       = p_0110_agr-series.
      tg_irf-witht        = x_with_item-witht.
      tg_irf-wt_withcd    = x_with_item-wt_withcd.
      tg_irf-wi_tax_base  = abs( x_with_item-wt_qsshb ).
      tg_irf-wi_tax_amt   = abs( x_with_item-wt_qbshb ).
      tg_irf-hkont        = x_with_item-hkont.

      IF x_with_item-wt_qsshb < 0.
        tg_irf-bas_neg = 'X'.
      ENDIF.

      "Verifica se definiu base manual
      READ TABLE tg_irf_basman WITH KEY nfenum    = tg_irf-nfenum
                                        series    = tg_irf-series
                                        witht     = tg_irf-witht.
      IF sy-subrc = 0.
        tg_irf-wt_basman  = 'X'.

        IF tg_irf_basman-wt_taxman IS NOT INITIAL.
          tg_irf-wt_taxman = 'X'.
        ENDIF.

      ENDIF.

      APPEND tg_irf.
    ENDLOOP.

  ENDLOOP.

  IF tg_irf[] IS NOT INITIAL.

    REFRESH: tg_t059u.

    SELECT *
      FROM t059u INTO TABLE tg_t059u
      FOR ALL ENTRIES IN tg_irf
     WHERE spras EQ sy-langu
       AND land1 EQ 'BR'
       AND witht EQ tg_irf-witht.

    LOOP AT tg_irf.

      READ TABLE tg_t059u WITH KEY witht = tg_irf-witht.
      IF sy-subrc IS INITIAL.
        tg_irf-text40 = tg_t059u-text40.
      ENDIF.

      MODIFY tg_irf.

    ENDLOOP.

  ENDIF.



ENDFORM.

FORM f_cbar_unico TABLES p_0110 STRUCTURE tg_sai_0110.

  DATA: tg_0110_cbar  TYPE TABLE OF ty_saida_0110 WITH HEADER LINE.

  CLEAR: vg_cbar_unico.

  CHECK ( zglt080-zlsch = 'E'  ) OR ( zglt080-zlsch = 'D' ).

  tg_0110_cbar[] = p_0110[].
  SORT tg_0110_cbar BY cod_barras.
  DELETE ADJACENT DUPLICATES FROM tg_0110_cbar COMPARING cod_barras.

  IF ( lines( tg_0110_cbar[] ) = 1 ).
    READ TABLE tg_0110_cbar INDEX 1.
    IF tg_0110_cbar-cod_barras IS NOT INITIAL.
      vg_cbar_unico = tg_0110_cbar-cod_barras.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_nf_unica TABLES p_0110 STRUCTURE tg_sai_0110.

  DATA: tg_0110_nf  TYPE TABLE OF ty_saida_0110 WITH HEADER LINE.

  CLEAR: vg_nf_unica.

  tg_0110_nf[] = p_0110[].

  DELETE tg_0110_nf WHERE ( ( nfenum IS INITIAL ) OR
                            ( series IS INITIAL ) ).

  SORT tg_0110_nf BY nfenum series.
  DELETE ADJACENT DUPLICATES FROM tg_0110_nf COMPARING nfenum series.

  IF ( lines( tg_0110_nf[] ) = 1 ).
    READ TABLE tg_0110_nf INDEX 1.
    IF ( tg_0110_nf-nfenum IS NOT INITIAL ) AND ( tg_0110_nf-series IS NOT INITIAL ).
      vg_nf_unica = 'X'.
    ENDIF.
  ENDIF.


ENDFORM.

FORM f_converte_moeda USING p_vlr_converter  "TYPE J_1BNETPRI
                            p_currency_orig  TYPE t001-waers
                            p_currency_dest  TYPE t001-waers
                   CHANGING p_vlr_convertido. "TYPE J_1BNETPRI.

  CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
    EXPORTING
      client           = sy-mandt
      date             = sy-datum
      foreign_currency = p_currency_dest "PARA
      local_amount     = p_vlr_converter
      local_currency   = p_currency_orig "DE
      rate             = 0
      type_of_rate     = 'M'
      read_tcurr       = 'X'
    IMPORTING
      foreign_amount   = p_vlr_convertido.

ENDFORM.


FORM f_conversion_input_0110 USING p_sai_0110 TYPE ty_saida_0110.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_sai_0110-bukrs
    IMPORTING
      output = p_sai_0110-bukrs.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_sai_0110-nfenum
    IMPORTING
      output = p_sai_0110-nfenum.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_sai_0110-gsber
    IMPORTING
      output = p_sai_0110-gsber.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_sai_0110-hkont
    IMPORTING
      output = p_sai_0110-hkont.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_sai_0110-kostl
    IMPORTING
      output = p_sai_0110-kostl.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_sai_0110-matnr
    IMPORTING
      output = p_sai_0110-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_sai_0110-asnum
    IMPORTING
      output = p_sai_0110-asnum.


ENDFORM.

FORM f_import_base_irf .

  DATA: gt_planilha LIKE STANDARD TABLE OF alsmex_tabline,
        wl_planilha LIKE alsmex_tabline,
        vl_dt_temp  TYPE sydatum,
        tg_irf_imp  TYPE TABLE OF ty_irf WITH HEADER LINE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = TEXT-i14.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
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

  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR: tg_irf_imp.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        tg_irf_imp-gsber  = wl_planilha-value.
      WHEN 2.
        tg_irf_imp-nfenum = wl_planilha-value.
      WHEN 3.
        tg_irf_imp-series = wl_planilha-value.
      WHEN 4.
        tg_irf_imp-witht  = wl_planilha-value.
      WHEN 5.
        PERFORM f_tratar_campo CHANGING wl_planilha-value.
        tg_irf_imp-wi_tax_base = wl_planilha-value.
    ENDCASE.

    AT END OF row.

      IF ( tg_irf_imp-gsber    IS NOT INITIAL ) AND
         ( tg_irf_imp-nfenum   IS NOT INITIAL ) AND
         ( tg_irf_imp-series   IS NOT INITIAL ) AND
         ( tg_irf_imp-witht    IS NOT INITIAL ).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = tg_irf_imp-nfenum
          IMPORTING
            output = tg_irf_imp-nfenum.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = tg_irf_imp-gsber
          IMPORTING
            output = tg_irf_imp-gsber.

        DELETE tg_irf_basman WHERE nfenum    = tg_irf_imp-nfenum
                               AND series    = tg_irf_imp-series
                               AND witht     = tg_irf_imp-witht.

        READ TABLE tg_irf WITH KEY nfenum    = tg_irf_imp-nfenum
                                   series    = tg_irf_imp-series
                                   witht     = tg_irf_imp-witht.
        IF sy-subrc = 0.
          CLEAR: tg_irf_basman.
          MOVE-CORRESPONDING tg_irf TO tg_irf_basman.
          tg_irf_basman-wt_basman   = 'X'.
          tg_irf_basman-wi_tax_base = tg_irf_imp-wi_tax_base.
          APPEND tg_irf_basman.
        ENDIF.



      ENDIF.

    ENDAT.

  ENDLOOP.

  MESSAGE s836(sd) WITH TEXT-s05 DISPLAY LIKE 'S'.
  LEAVE TO SCREEN 0.

ENDFORM.

FORM f_get_filial_forn TABLES p_saida_0110 STRUCTURE tg_sai_0110
                     CHANGING p_gsber  TYPE zglt081-gsber.

  DATA: tg_0110_aux  TYPE TABLE OF ty_saida_0110 WITH HEADER LINE.

  IF lines( p_saida_0110 ) = 1.
    READ TABLE p_saida_0110 INTO wa_saida_0110 INDEX 1.
    p_gsber = wa_saida_0110-gsber.
  ELSE.
    tg_0110_aux[] = p_saida_0110[].
    SORT tg_0110_aux BY gsber.
    DELETE ADJACENT DUPLICATES FROM tg_0110_aux COMPARING gsber.
    IF lines( tg_0110_aux ) = 1.
      READ TABLE tg_0110_aux INDEX 1.
      p_gsber = tg_0110_aux-gsber.
    ELSE.
      CONCATENATE zglt080-bukrs+2(2) '01' INTO p_gsber.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_ctb_outras_moedas TABLES i_zglt036 STRUCTURE zglt036
                          USING i_zglt035 TYPE zglt035.


  DATA: gt_zglt036         TYPE TABLE OF zglt036,
        wl_zglt036         TYPE zglt036,
        wa_t001            TYPE t001,
        wa_moedas          TYPE x001,
        wl_tbsl            TYPE tbsl,
        i_data             TYPE gdatu_inv,
        e_ukurs  	         TYPE ukurs_curr,
        vl_index           TYPE i,
        vl_dif             TYPE zglt036-vlr_moeda_int,
        vl_moeda_int       TYPE zglt036-vlr_moeda_int,
        vl_moeda_forte     TYPE zglt036-vlr_moeda_forte,
        vl_moeda_grupo     TYPE zglt036-vlr_moeda_grupo,
        vl_tot_moeda_int   TYPE zglt036-vlr_moeda_int,
        vl_tot_moeda_forte TYPE zglt036-vlr_moeda_forte,
        vl_tot_moeda_grupo TYPE zglt036-vlr_moeda_grupo.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  CLEAR: vl_tot_moeda_int, vl_tot_moeda_forte, vl_tot_moeda_grupo,
         vl_moeda_int    , vl_moeda_forte    , vl_moeda_grupo.

  SELECT SINGLE bukrs waers INTO (wa_t001-bukrs,wa_t001-waers)
    FROM t001
   WHERE bukrs EQ i_zglt035-bukrs.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs = i_zglt035-bukrs
    IMPORTING
      e_x001  = wa_moedas.

  IF i_zglt035-moeda_interna IS INITIAL.
    i_zglt035-moeda_interna = wa_t001-waers.
  ENDIF.

  IF i_zglt035-moeda_forte IS INITIAL.
    i_zglt035-moeda_forte = wa_moedas-hwae2.
  ENDIF.

  IF i_zglt035-moeda_grupo IS INITIAL.
    i_zglt035-moeda_grupo = wa_moedas-hwae3.
  ENDIF.

  CREATE OBJECT obj_zcl_util_sd.

  i_data = i_zglt035-dt_lcto.
  obj_zcl_util_sd->set_data(  EXPORTING i_data = i_data ).
  obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
  obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = i_zglt035-moeda_doc ).

  LOOP AT i_zglt036 INTO wl_zglt036.

    IF wl_zglt036-vlr_moeda_int EQ 0.
      IF i_zglt035-moeda_interna EQ i_zglt035-moeda_doc.
        wl_zglt036-vlr_moeda_int = wl_zglt036-vlr_moeda_doc.
      ELSE.
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = i_zglt035-moeda_interna ).
        obj_zcl_util_sd->taxa_cambio(  RECEIVING e_ukurs = e_ukurs ).
        IF e_ukurs LT 0.
          wl_zglt036-vlr_moeda_int = wl_zglt036-vlr_moeda_doc / abs( e_ukurs ).
        ELSE.
          wl_zglt036-vlr_moeda_int = wl_zglt036-vlr_moeda_doc * abs( e_ukurs ).
        ENDIF.
      ENDIF.

      IF ( wl_zglt036-vlr_moeda_int = 0 ) AND
         ( wl_zglt036-vlr_moeda_doc > 0 ) AND
         ( wl_zglt036-vlr_moeda_doc < 1 ).
        wl_zglt036-vlr_moeda_int = '0.01'.
      ENDIF.
    ENDIF.

    IF wl_zglt036-vlr_moeda_forte EQ 0.
      IF i_zglt035-moeda_forte EQ i_zglt035-moeda_doc.
        wl_zglt036-vlr_moeda_forte = wl_zglt036-vlr_moeda_doc.
      ELSE.
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = i_zglt035-moeda_forte ).
        obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).
        IF e_ukurs LT 0.
          wl_zglt036-vlr_moeda_forte = wl_zglt036-vlr_moeda_doc / abs( e_ukurs ).
        ELSE.
          wl_zglt036-vlr_moeda_forte = wl_zglt036-vlr_moeda_doc * abs( e_ukurs ).
        ENDIF.
      ENDIF.

      IF ( wl_zglt036-vlr_moeda_forte = 0 ) AND
         ( wl_zglt036-vlr_moeda_doc   > 0 ) AND
         ( wl_zglt036-vlr_moeda_doc   < 1 ).
        wl_zglt036-vlr_moeda_forte = '0.01'.
      ENDIF.
    ENDIF.

    IF wl_zglt036-vlr_moeda_grupo EQ 0.
      IF i_zglt035-moeda_grupo EQ i_zglt035-moeda_doc.
        wl_zglt036-vlr_moeda_grupo = wl_zglt036-vlr_moeda_doc.
      ELSE.
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = i_zglt035-moeda_grupo ).
        obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).
        IF e_ukurs LT 0.
          wl_zglt036-vlr_moeda_grupo = wl_zglt036-vlr_moeda_doc / abs( e_ukurs ).
        ELSE.
          wl_zglt036-vlr_moeda_grupo = wl_zglt036-vlr_moeda_doc * abs( e_ukurs ).
        ENDIF.
      ENDIF.

      IF ( wl_zglt036-vlr_moeda_grupo = 0 ) AND
         ( wl_zglt036-vlr_moeda_doc   > 0 ) AND
         ( wl_zglt036-vlr_moeda_doc   < 1 ).
        wl_zglt036-vlr_moeda_grupo = '0.01'.
      ENDIF.
    ENDIF.

    CLEAR: wl_tbsl.
    SELECT SINGLE *
      FROM tbsl INTO wl_tbsl
     WHERE bschl = wl_zglt036-bschl.

    "Caso não encontre a chave para algum lançamento, sai do procedimento
    IF ( sy-subrc NE 0 ) OR ( wl_tbsl-shkzg IS INITIAL ).
      RETURN.
    ENDIF.

    IF wl_zglt036-bschl NE '31'.
      IF wl_tbsl-shkzg = 'S'. "Débito
        ADD wl_zglt036-vlr_moeda_int   TO vl_tot_moeda_int.
        ADD wl_zglt036-vlr_moeda_forte TO vl_tot_moeda_forte.
        ADD wl_zglt036-vlr_moeda_grupo TO vl_tot_moeda_grupo.
      ELSE.
        SUBTRACT wl_zglt036-vlr_moeda_int   FROM vl_tot_moeda_int.
        SUBTRACT wl_zglt036-vlr_moeda_forte FROM vl_tot_moeda_forte.
        SUBTRACT wl_zglt036-vlr_moeda_grupo FROM vl_tot_moeda_grupo.
      ENDIF.
    ENDIF.

    MODIFY i_zglt036 FROM wl_zglt036.
  ENDLOOP.

  "Verifica Diferenças moedas, caso encontre, joga na primeira partida de razão.
  CLEAR: wl_zglt036.
  READ TABLE i_zglt036 INTO wl_zglt036 WITH KEY bschl = '31'.
  CHECK sy-subrc = 0.

  vl_moeda_int    = wl_zglt036-vlr_moeda_int.
  vl_moeda_forte  = wl_zglt036-vlr_moeda_forte.
  vl_moeda_grupo  = wl_zglt036-vlr_moeda_grupo.

  SORT i_zglt036 BY vlr_moeda_doc DESCENDING. "Ordernar para começar da partida de maior valor

  LOOP AT i_zglt036 INTO wl_zglt036 WHERE bschl EQ '40'.

    "Moeda Interna
    IF ( vl_tot_moeda_int IS NOT INITIAL ) AND ( vl_moeda_int IS NOT INITIAL ).
      CLEAR: vl_dif.
      vl_dif = vl_moeda_int - vl_tot_moeda_int.
      IF ( vl_dif NE 0 ) AND ( abs( vl_dif ) <= 5 ).
        ADD vl_dif TO wl_zglt036-vlr_moeda_int.
      ENDIF.
    ENDIF.

    "Moeda Forte
    IF ( vl_tot_moeda_forte IS NOT INITIAL ) AND ( vl_moeda_forte IS NOT INITIAL ).
      CLEAR: vl_dif.
      vl_dif = vl_moeda_forte - vl_tot_moeda_forte.
      IF ( vl_dif NE 0 ) AND ( abs( vl_dif ) <= 5 ).
        ADD vl_dif TO wl_zglt036-vlr_moeda_forte.
      ENDIF.
    ENDIF.

    "Moeda Grupo
    IF ( vl_tot_moeda_grupo IS NOT INITIAL ) AND ( vl_moeda_grupo IS NOT INITIAL ).
      CLEAR: vl_dif.
      vl_dif = vl_moeda_grupo - vl_tot_moeda_grupo.
      IF ( vl_dif NE 0 ) AND ( abs( vl_dif ) <= 5 ).
        ADD vl_dif TO wl_zglt036-vlr_moeda_grupo.
      ENDIF.
    ENDIF.

    MODIFY i_zglt036 FROM wl_zglt036.

    RETURN.

  ENDLOOP.

ENDFORM.

FORM z_prepara_mensagem USING pobj_key
                              ptype
                              interface
                              pmessage
                              pmessage_v1 .

  CLEAR wa_outreturn.

  wa_outreturn-obj_key        = pobj_key.
  wa_outreturn-interface      = interface.
  wa_outreturn-dt_atualizacao = sy-datum.
  wa_outreturn-hr_atualizacao = sy-uzeit.
  wa_outreturn-type           = ptype.
  wa_outreturn-id             = 'MM'.
  wa_outreturn-num            = '899'.
  wa_outreturn-message        = pmessage.
  wa_outreturn-message_v1     = pmessage_v1.

  APPEND wa_outreturn TO it_outreturn.


ENDFORM.                    "z_prepara_mensagem

FORM f_envia_log_legado .

* Chamar função assíncrona de retorno, confirmando a gravação
* de dados
  IF NOT it_outreturn[] IS INITIAL.
    SORT it_outreturn BY obj_key interface.

    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
      DESTINATION 'XI_SIGAM_RETURN'
      TABLES
        outreturn = it_outreturn.

    COMMIT WORK.

  ENDIF.

ENDFORM.                    " Z_ENVIA_LOG_LEGADO
*&---------------------------------------------------------------------*
*&      Form  PROC_RETORNO_LEGADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM proc_retorno_legado .

  DATA: vl_message_v1 TYPE zfie_ret_document-message_v1,
        tg_0081       TYPE TABLE OF zglt081 WITH HEADER LINE,
        tg_zglt087    TYPE TABLE OF zglt087 WITH HEADER LINE.

  CLEAR: it_sel_rows[], it_outreturn[].

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK lines( it_sel_rows ) EQ 1.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.

  CHECK sy-subrc = 0.

  SELECT *
    FROM zglt081 INTO TABLE tg_0081
   WHERE seq_lcto EQ wa_saida_0100-seq_lcto
     AND objkey   NE ''.

  SORT tg_0081 BY objkey.
  DELETE ADJACENT DUPLICATES FROM tg_0081 COMPARING objkey.

  LOOP AT tg_0081.
    CLEAR: tg_zglt087[].

    SELECT *
      FROM zglt087 INTO TABLE tg_zglt087
     WHERE objkey = tg_0081-objkey.

    "Monta Log Retorno
    LOOP AT tg_zglt087.
      vl_message_v1 = tg_zglt087-seq_lcto.
      PERFORM z_prepara_mensagem USING tg_zglt087-objkey
                                       tg_zglt087-type
                                       '29'
                                       tg_zglt087-message
                                       vl_message_v1 .
    ENDLOOP.

  ENDLOOP.

  PERFORM f_envia_log_legado.

ENDFORM.

FORM f_lanca_fiscal_dt_atual.

  CLEAR: it_sel_rows[], it_outreturn[].

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK lines( it_sel_rows ) EQ 1.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.

  CHECK sy-subrc = 0.

  SELECT SINGLE *
    FROM zglt080 INTO @DATA(_wl_080)
   WHERE seq_lcto EQ @wa_saida_0100-seq_lcto.

  CHECK sy-subrc = 0.

  IF _wl_080-fis_dt_atual EQ abap_false.
    _wl_080-fis_dt_atual = abap_true.
    MODIFY zglt080 FROM _wl_080.
    IF sy-subrc = 0.
      MESSAGE 'Documento Fiscal será lançado com data atual!' TYPE 'S'.
    ENDIF.
  ELSE.
    _wl_080-fis_dt_atual = abap_false.
    MODIFY zglt080 FROM _wl_080.
    IF sy-subrc = 0.
      MESSAGE 'Documento Fiscal será lançado com data informada no lançamento!' TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_atualiza_irf_manual.

  "Check IRF que foram zerados.
  LOOP AT it_saida_0113 INTO wa_saida_0113.

    DELETE tg_irf_basman WHERE nfenum    = wa_saida_0113-nfenum
                           AND series    = wa_saida_0113-series
                           AND witht     = wa_saida_0113-witht.


    IF wa_saida_0113-wt_basman IS NOT INITIAL.

      READ TABLE tg_irf WITH KEY nfenum    = wa_saida_0113-nfenum
                                 series    = wa_saida_0113-series
                                 witht     = wa_saida_0113-witht.
      IF sy-subrc = 0.
        CLEAR: tg_irf_basman.
        MOVE-CORRESPONDING tg_irf TO tg_irf_basman.
        tg_irf_basman-wt_basman   = 'X'.
        tg_irf_basman-wi_tax_base = wa_saida_0113-wi_tax_base.

        IF wa_saida_0113-wt_taxman IS NOT INITIAL.
          tg_irf_basman-wt_taxman   = 'X'.
          tg_irf_basman-wi_tax_amt = wa_saida_0113-wi_tax_amt.
        ENDIF.

        "TG_IRF_BASMAN-WI_TAX_AMT  = 0.
        APPEND tg_irf_basman.
      ENDIF.

    ENDIF.

  ENDLOOP.

  SORT tg_irf_basman BY nfenum series witht.
  DELETE ADJACENT DUPLICATES FROM tg_irf_basman COMPARING nfenum series witht.

ENDFORM.

FORM f_saida_0113 USING  p_saida_0112 TYPE ty_saida_0110.

  DATA: gt_089   TYPE TABLE OF zglt089 WITH HEADER LINE,
        tg_t059u TYPE TABLE OF t059u WITH HEADER LINE.

  CLEAR: it_saida_0113[].

  IF ( vg_opr_lcto EQ c_display ).

    "Carrega IRF
    CLEAR: gt_089[], tg_t059u[].

    SELECT *
      FROM zglt089 INTO TABLE gt_089
     WHERE seq_lcto = zglt080-seq_lcto.

    CHECK gt_089[] IS NOT INITIAL.

    SELECT *
      FROM t059u INTO TABLE tg_t059u
       FOR ALL ENTRIES IN gt_089
     WHERE spras EQ sy-langu
       AND land1 EQ 'BR'
       AND witht EQ gt_089-witht.

    LOOP AT gt_089 WHERE nfenum  = p_saida_0112-nfenum
                     AND series  = p_saida_0112-series.

      CLEAR: wa_saida_0113.
      MOVE-CORRESPONDING gt_089 TO wa_saida_0113.

      READ TABLE tg_t059u WITH KEY witht = gt_089-witht.
      IF sy-subrc IS INITIAL.
        wa_saida_0113-text40 = tg_t059u-text40.
      ENDIF.

      APPEND wa_saida_0113 TO it_saida_0113.
    ENDLOOP.

  ELSE.

    LOOP AT tg_irf WHERE nfenum  = p_saida_0112-nfenum
                     AND series  = p_saida_0112-series.

      CLEAR: wa_saida_0113.

      MOVE-CORRESPONDING tg_irf TO wa_saida_0113.

      READ TABLE tg_irf_basman WITH KEY nfenum    = wa_saida_0113-nfenum
                                        series    = wa_saida_0113-series
                                        witht     = wa_saida_0113-witht.
      IF sy-subrc = 0.
        wa_saida_0113-wi_tax_base  = tg_irf_basman-wi_tax_base.
        "WA_SAIDA_0113-WI_TAX_AMT   = 0.
        wa_saida_0113-wt_basman    = 'X'.

        IF tg_irf_basman-wt_taxman IS NOT INITIAL.
          wa_saida_0113-wi_tax_amt  = tg_irf_basman-wi_tax_amt.
          wa_saida_0113-wt_taxman   = 'X'.
        ENDIF.

      ENDIF.

      APPEND wa_saida_0113 TO it_saida_0113.

    ENDLOOP.

  ENDIF.

ENDFORM.

FORM f_aplic_txt_ctb.

  DATA: v_competencia TYPE string,
        v_nfenum      TYPE c LENGTH 10.

  CHECK vg_opr_lcto NE c_display.

  FIELD-SYMBOLS: <saida_0110> TYPE ty_saida_0110.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente aplicar um texto contabil padrão para o(s) registro(s) do Lançamento?'
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

  SELECT SINGLE *
    FROM lfa1 INTO @DATA(_wl_lfa1)
   WHERE lifnr = @zglt080-lifnr.

  IF ( sy-subrc NE 0 ) OR ( zglt080-lifnr IS INITIAL ).
    MESSAGE 'Fornecedor não encontrado/informado!' TYPE 'S'.
    EXIT.
  ENDIF.

  LOOP AT it_saida_0110 ASSIGNING <saida_0110>.

    CASE <saida_0110>-bldat+4(2).
      WHEN '01'.
        v_competencia = 'JAN'.
      WHEN '02'.
        v_competencia = 'FEV'.
      WHEN '03'.
        v_competencia = 'MAR'.
      WHEN '04'.
        v_competencia = 'ABR'.
      WHEN '05'.
        v_competencia = 'MAI'.
      WHEN '06'.
        v_competencia = 'JUN'.
      WHEN '07'.
        v_competencia = 'JUL'.
      WHEN '08'.
        v_competencia = 'AGO'.
      WHEN '09'.
        v_competencia = 'SET'.
      WHEN '10'.
        v_competencia = 'OUT'.
      WHEN '11'.
        v_competencia = 'NOV'.
      WHEN '12'.
        v_competencia = 'DEZ'.
    ENDCASE.

    v_competencia = v_competencia && '/' && <saida_0110>-bldat(4).

    CLEAR: v_nfenum.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <saida_0110>-nfenum
      IMPORTING
        output = v_nfenum.

    CONCATENATE 'NF' v_nfenum 'REF.' v_competencia 'FORN.' _wl_lfa1-name1 INTO <saida_0110>-sgtxt SEPARATED BY space.

  ENDLOOP.

  LEAVE TO SCREEN 0110.

ENDFORM.

FORM f_lock_lcto USING p_status
                       p_seq_lcto TYPE zglt080-seq_lcto.

  sy-subrc = 0.

  CHECK ( p_seq_lcto IS NOT INITIAL ).

  CASE p_status.
    WHEN 'B'. "Bloqueio

      CALL FUNCTION 'ENQUEUE_EZGLT080'
        EXPORTING
          seq_lcto       = p_seq_lcto
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

    WHEN 'D'. "Desbloqueio

      CALL FUNCTION 'DEQUEUE_EZGLT080'
        EXPORTING
          seq_lcto = p_seq_lcto.
  ENDCASE.

ENDFORM.


FORM f_lock_lcto_item USING p_status
                            p_seq_lcto TYPE zglt081-seq_lcto
                            p_seqitem  TYPE zglt081-seqitem.

  sy-subrc = 0.

  CHECK ( p_seq_lcto IS NOT INITIAL ) AND ( p_seqitem  IS NOT INITIAL ).

  CASE p_status.
    WHEN 'B'. "Bloqueio

      CALL FUNCTION 'ENQUEUE_EZGLT081'
        EXPORTING
          seq_lcto       = p_seq_lcto
          seqitem        = p_seqitem
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

    WHEN 'D'. "Desbloqueio

      CALL FUNCTION 'DEQUEUE_EZGLT081'
        EXPORTING
          seq_lcto = p_seq_lcto
          seqitem  = p_seqitem.

  ENDCASE.

ENDFORM.
