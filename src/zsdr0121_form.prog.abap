*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_FORM
*&---------------------------------------------------------------------*

FORM f_refresh_alv USING p_alv.

  CASE p_alv.
    WHEN '0100'.
      CHECK obj_alv_0100 IS NOT INITIAL.

      CALL METHOD obj_alv_0100->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

  ENDCASE.

ENDFORM.

FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  gs_layout-sel_mode   = 'A'.
  gs_layout-ctab_fname = 'COLOR'.
  gs_variant-report  = sy-repid.
  wa_stable-row         = 'X'.
  wa_stable-col         = 'X'.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  FREE: wa_fcat, it_fcat.

  CASE p_screen.
    WHEN '0100'.

      PERFORM f_estrutura_alv USING:

        01  ''                      ''             'IT_SAIDA_0100'  'STATUS'            'Status'                    '06'   ' '    ''  ' ' ' ' 'X' ' ' 'X' ,
        02  'J_1BNFDOC'             'DOCNUM'       'IT_SAIDA_0100'  'DOCNUM'            'Docnum NF-e'               '11'   ' '    ''  ' ' ' ' 'X' ' ' 'X' ,
        03  'J_1BNFDOC'             'NFENUM'       'IT_SAIDA_0100'  'NFENUM'            'Num. NF-e'                 '09'   ' '    ''  ' ' ' ' ' ' ' ' 'X' ,
        04  'J_1BNFDOC'             'SERIES'       'IT_SAIDA_0100'  'SERIE'             'Série'                     '09'   ' '    ''  ' ' 'C' ' ' ' ' 'X' ,
        05  'J_1BNFDOC'             'DOCDAT'       'IT_SAIDA_0100'  'DOCDAT'            'Dt.Emissão'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        06  ' '                     ''             'IT_SAIDA_0100'  'CHAVE_NFE'         'Chave NF-e'                '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        07  'J_1BNFDOC'             'BRANCH'       'IT_SAIDA_0100'  'BRANCH'            'Filial'                    '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
        08  'J_1BBRANCH'            'NAME'         'IT_SAIDA_0100'  'DS_FILIAL'         'Ds.Filial'                 '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        09  'J_1BNFLIN'             'MATNR'        'IT_SAIDA_0100'  'MATNR'             'Material'                  '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        10  'J_1BNFLIN'             'MENGE'        'IT_SAIDA_0100'  'MENGE'             'Vol.NF-e(KG)'              '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        11  'J_1BNFLIN'             'NETPR'        'IT_SAIDA_0100'  'NETPR'             'Unit.'                     '08'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
        12  'J_1BNFLIN'             'NETWR'        'IT_SAIDA_0100'  'NETWR'             'Valor Total'               '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        13  'J_1BNFLIN'             'CHARG'        'IT_SAIDA_0100'  'CHARG'             'Lote'                      '04'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        14  'J_1BNFLIN'             'MENGE'        'IT_SAIDA_0100'  'MENGE_BOL'         'Vol.Aplic.Prod(KG)'        '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        15  'J_1BNFLIN'             'MENGE'        'IT_SAIDA_0100'  'SALDO_NF'          'Saldo NF-e(KG)'            '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        16  'J_1BNFLIN'             'MENGE'        'IT_SAIDA_0100'  'SALDO_FINAL'       'Saldo NF-e Tot.(KG)'       '19'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        17  'ZSDT0251'            'QTDE_UTILIZADA' 'IT_SAIDA_0100'  'QTDE_UTILIZADA'    'Quant.Utilizada'           '19'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        18  'ZSDT0246'              'ID_BOLETIM'   'IT_SAIDA_0100'  'ID_BOLETIM'        'Id.Boletim'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*-CS2021000386 - 28.04.2021 - JT - inicio
        19  'ZSDT0246'              'DESCR_SOJA'   'IT_SAIDA_0100'  'DESCR_SOJA'        'Categ.Soja'                '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        20  'ZSDT0251'              'LGORT_V'      'IT_SAIDA_0100'  'LGORT_V'           'Depósito'                  '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        21  'ZSDT0251'              'MBLNR_MB1B'   'IT_SAIDA_0100'  'MBLNR_MB1B'        'Doc.Troc.Dep'              '12'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
        22  'ZSDT0251'              'MJAHR_MB1B'   'IT_SAIDA_0100'  'MJAHR_MB1B'        'Ano'                       '04'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        23  'MKPF'                  'BKTXT'        'IT_SAIDA_0100'  'BKTXT'             'Nr.Fornecimento'           '25'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*-CS2021000386 - 28.04.2021 - JT - fim
        24  'J_1BNFDOC'             'DOCDAT'       'IT_SAIDA_0100'  'DOCDAT_NFE_RET'    'Dt.Emi.NF-e Ret.'          '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        25  'J_1BNFDOC'             'NFENUM'       'IT_SAIDA_0100'  'NFENUM_NFE_RET'    'Nro NF-e Ret.'             '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        26  'J_1BNFDOC'             'DOCNUM'       'IT_SAIDA_0100'  'DOCNUM_NFE_RET'    'Docnum NF-e Ret.'          '16'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
        27  ''                       ''            'IT_SAIDA_0100'  'CHAVE_NFE_RET'     'Chave NF-e Ret.'           '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        28  ''                       ''            'IT_SAIDA_0100'  'DIAS_EMISSAO'      'Dias Pendentes'            '14'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
        29  ''                       ''            'IT_SAIDA_0100'  'DISP_PORTO'        'Disponível Porto'          '16'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
        30  ''                       ''            'IT_SAIDA_0100'  'MESSAGE_MB1B'      'Mensagem de Erro'          '100'  ' '    ''  ' ' 'C' ' ' ' ' '' .

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
                           VALUE(p_key).

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = p_key.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
* wa_fcat-checkbox    = p_check.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_exclude_fcode USING p_screen.

  APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_check             TO it_exclude_fcode.

ENDFORM.

FORM f_limpa_variaveis .

  CLEAR: wa_saida_0100,
         it_saida_0100[],
         tg_vbrk[],
         tg_vbrp[],
         tg_likp[],
         tg_lips[],
         tg_j_1bnflin[],
         tg_j_1bnfe_active[],
         tg_vbfa_estorno[],
         tg_j_1bnfdoc[],
         tg_zsdt0246[],
         tg_zfiwrt0008[],
         tg_zsdt0249[],
         tg_j_1bbranch[].

ENDFORM.

FORM f_selecionar_dados .

  PERFORM f_limpa_variaveis.

  SELECT *
    FROM icon
    INTO TABLE tg_icon
   WHERE name = 'ICON_RED_LIGHT'
      OR name = 'ICON_GREEN_LIGHT'
      OR name = 'ICON_LIGHT_OUT'.

  "Cabeçalho Faturas
  SELECT *
    FROM vbrk APPENDING CORRESPONDING FIELDS OF TABLE tg_vbrk
   WHERE erdat   IN p_docdat
     AND fkart   EQ 'ZIND' AND DRAFT = SPACE .

  DELETE tg_vbrk WHERE bupla NOT IN p_branch.

  CHECK tg_vbrk[] IS NOT INITIAL.

  LOOP AT tg_vbrk.
    tg_vbrk-refkey = tg_vbrk-vbeln.
    MODIFY tg_vbrk.
  ENDLOOP.

  "Itens Doc. Fiscal
  SELECT *
    FROM j_1bnflin INTO TABLE tg_j_1bnflin
     FOR ALL ENTRIES IN tg_vbrk
   WHERE refkey EQ tg_vbrk-refkey.

  CHECK tg_j_1bnflin[] IS NOT INITIAL.

  SELECT *
    FROM j_1bnfe_active INTO TABLE tg_j_1bnfe_active
     FOR ALL ENTRIES IN tg_j_1bnflin
   WHERE docnum     EQ tg_j_1bnflin-docnum.

  CHECK tg_j_1bnfe_active[] IS NOT INITIAL.

  "Escrituração Entrada Filial Destino
  SELECT *
    FROM zfiwrt0008 APPENDING TABLE tg_zfiwrt0008
     FOR ALL ENTRIES IN tg_j_1bnflin
   WHERE docnum_saida     EQ tg_j_1bnflin-docnum
     AND docs_estornados  EQ abap_false
     AND loekz            EQ abap_false.

  "Itens Faturas
  SELECT *
    FROM vbrp INTO TABLE tg_vbrp
     FOR ALL ENTRIES IN tg_vbrk
   WHERE vbeln EQ tg_vbrk-vbeln.

  CHECK tg_vbrp[] IS NOT INITIAL.

  "Cabeçalho Remessas
  SELECT *
    FROM likp INTO TABLE tg_likp
     FOR ALL ENTRIES IN tg_vbrp
   WHERE vbeln EQ tg_vbrp-vgbel.

  CHECK tg_likp[] IS NOT INITIAL.

*-CS2021000386 - 28.04.2021 - JT - inicio
  SELECT *
    FROM lips INTO TABLE tg_lips
     FOR ALL ENTRIES IN tg_vbrp
   WHERE vbeln EQ tg_vbrp-vgbel
     AND posnr EQ tg_vbrp-vgpos.
*-CS2021000386 - 28.04.2021 - JT - fim

  SELECT *
    FROM zsdt0023 INTO TABLE tg_zsdt0023
     FOR ALL ENTRIES IN tg_likp
   WHERE vbeln EQ tg_likp-vbeln.

  CHECK tg_zsdt0023[] IS NOT INITIAL.

  "Fluxo de Documentos Estornados
  SELECT *
    FROM vbfa INTO TABLE tg_vbfa_estorno
     FOR ALL ENTRIES IN tg_vbrk
   WHERE vbelv   EQ tg_vbrk-vbeln
     AND vbtyp_n EQ 'N'. "Estorno Fatura

  SELECT *
    FROM zsdt0249 INTO TABLE tg_zsdt0249
     FOR ALL ENTRIES IN tg_j_1bnflin
   WHERE docnum EQ tg_j_1bnflin-docnum.

*-CS2021000386 - 28.04.2021 - JT - inicio
  SELECT *
    FROM zsdt0251 INTO TABLE tg_zsdt0251
     FOR ALL ENTRIES IN tg_j_1bnflin
   WHERE docnum EQ tg_j_1bnflin-docnum.
*-CS2021000386 - 28.04.2021 - JT - fim

  IF tg_zsdt0249[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0246 INTO TABLE tg_zsdt0246
       FOR ALL ENTRIES IN tg_zsdt0249
     WHERE id_boletim EQ tg_zsdt0249-id_boletim.

    SELECT *
      FROM zsdt0252 INTO TABLE tg_zsdt0252
       FOR ALL ENTRIES IN tg_zsdt0249
     WHERE id_boletim EQ tg_zsdt0249-id_boletim
       AND branch     EQ tg_zsdt0249-branch
       AND charg      EQ tg_zsdt0249-charg
       AND id_agrp    EQ tg_zsdt0249-id_agrp.

    DELETE tg_zsdt0252 WHERE seqlcto_devol IS INITIAL.

    IF tg_zsdt0252[] IS NOT INITIAL.
      SELECT *
        FROM zfiwrt0008 APPENDING TABLE tg_zfiwrt0008
         FOR ALL ENTRIES IN tg_zsdt0252
       WHERE seq_lcto EQ tg_zsdt0252-seqlcto_devol.

*-CS2021000386 - 28.04.2021 - JT - inicio
      SELECT *
        FROM mkpf INTO TABLE tg_mkpf
         FOR ALL ENTRIES IN tg_zsdt0252
       WHERE mblnr EQ tg_zsdt0252-doc_prod_01
         AND mjahr EQ tg_zsdt0252-ano_doc_prod_01.
*-CS2021000386 - 28.04.2021 - JT - fim
    ENDIF.
  ENDIF.

  DELETE tg_zfiwrt0008 WHERE ( docnum IS INITIAL ) OR ( docs_estornados EQ abap_true ) OR ( loekz EQ abap_true ).

  IF tg_zfiwrt0008[] IS NOT INITIAL.
    SELECT *
      FROM j_1bnfe_active APPENDING TABLE tg_j_1bnfe_active
       FOR ALL ENTRIES IN tg_zfiwrt0008
     WHERE docnum     EQ tg_zfiwrt0008-docnum.
  ENDIF.

  IF tg_j_1bnfe_active[] IS NOT INITIAL.
    SELECT *
      FROM j_1bnfdoc INTO TABLE tg_j_1bnfdoc
       FOR ALL ENTRIES IN tg_j_1bnfe_active
     WHERE docnum EQ tg_j_1bnfe_active-docnum.

    IF tg_j_1bnfdoc[] IS NOT INITIAL.
      SELECT *
        FROM j_1bbranch INTO TABLE tg_j_1bbranch
         FOR ALL ENTRIES IN tg_j_1bnfdoc
       WHERE branch EQ tg_j_1bnfdoc-branch.
    ENDIF.

  ENDIF.

  PERFORM f_check_auth_tables_nfe TABLES tg_j_1bnfe_active tg_j_1bnfdoc.


ENDFORM.

FORM f_processa_dados.

  SORT tg_zsdt0249 BY id_boletim.
  SORT tg_zsdt0246 BY id_boletim.
  SORT tg_zsdt0251 BY docnum.

  LOOP AT tg_vbrk. " Fatura Cabeçalho

    FREE:  wa_saida_0100.
    CLEAR: tg_icon.

    READ TABLE tg_vbfa_estorno WITH KEY vbelv = tg_vbrk-vbeln. "Check se fatura foi estornada
    CHECK sy-subrc NE 0.

    READ TABLE tg_vbrp WITH KEY vbeln = tg_vbrk-vbeln. "Item Fatura
    CHECK sy-subrc EQ 0.

    READ TABLE tg_likp WITH KEY vbeln = tg_vbrp-vgbel. "Remessa
    CHECK sy-subrc EQ 0.

    CLEAR tg_lips.
    READ TABLE tg_lips WITH KEY vbeln = tg_vbrp-vgbel "Remessa
                                posnr = tg_vbrp-vgpos. "Remessa

    READ TABLE tg_zsdt0023 WITH KEY vbeln = tg_likp-vbeln.
    CHECK sy-subrc EQ 0.

    READ TABLE tg_j_1bnflin WITH KEY refkey = tg_vbrk-refkey. "Item Doc. Fiscal
    CHECK sy-subrc EQ 0.

    READ TABLE tg_j_1bnfe_active WITH KEY docnum = tg_j_1bnflin-docnum. "Dados NF-e
    CHECK sy-subrc EQ 0.

    READ TABLE tg_j_1bnfdoc WITH KEY docnum = tg_j_1bnflin-docnum. "Dados NF-e
    CHECK sy-subrc EQ 0.

    wa_saida_0100-docnum        = tg_j_1bnfe_active-docnum.
    wa_saida_0100-nfenum        = tg_j_1bnfe_active-nfnum9.
    wa_saida_0100-serie         = tg_j_1bnfe_active-serie.
    wa_saida_0100-docdat        = tg_j_1bnfdoc-docdat.
    wa_saida_0100-matnr         = tg_j_1bnflin-matnr.

*   CONCATENATE tg_vbrp-vgbel '-' tg_vbrp-vgpos
*          INTO wa_saida_0100-bktxt.
    wa_saida_0100-bktxt = tg_vbrp-vgbel.

    PERFORM f_monta_chave_docnum USING tg_j_1bnfe_active
                              CHANGING wa_saida_0100-chave_nfe.

    wa_saida_0100-branch        = tg_j_1bnfdoc-branch.
    READ TABLE tg_j_1bbranch WITH KEY branch = tg_j_1bnfdoc-branch.
    IF sy-subrc EQ 0.
      wa_saida_0100-ds_filial   = tg_j_1bbranch-name.
    ENDIF.

    CASE tg_j_1bnflin-meins.
      WHEN 'KG'.
        wa_saida_0100-menge  = tg_j_1bnflin-menge.
      WHEN 'TO'.
        wa_saida_0100-menge  = tg_j_1bnflin-menge * 1000.
      WHEN OTHERS.
        APPEND wa_saida_0100 TO it_saida_0100.
        CONTINUE.
    ENDCASE.

    wa_saida_0100-netwr         = tg_j_1bnflin-netwr.
    wa_saida_0100-netpr         = tg_j_1bnflin-netpr.
    wa_saida_0100-charg         = tg_j_1bnflin-charg.
    wa_saida_0100-dias_emissao  = sy-datum - tg_j_1bnfdoc-docdat.

    DATA(_disp_porto) = abap_false.

    READ TABLE tg_zfiwrt0008 WITH KEY docnum_saida = tg_j_1bnflin-docnum.
    IF ( sy-subrc EQ 0 ) AND ( tg_j_1bnflin-docnum IS NOT INITIAL ) AND ( tg_zfiwrt0008-docnum IS NOT INITIAL ).
      READ TABLE tg_j_1bnfe_active WITH KEY docnum = tg_zfiwrt0008-docnum.
      IF sy-subrc EQ 0.
        _disp_porto = abap_true.
      ENDIF.
    ENDIF.

    IF _disp_porto EQ abap_true.
      wa_saida_0100-disp_porto = 'Sim'.
      CHECK ( r_prt_s EQ abap_true ) OR ( r_prt_t EQ abap_true ).
    ELSE.
      wa_saida_0100-disp_porto = 'Não'.
      CHECK ( r_prt_n EQ abap_true ) OR ( r_prt_t EQ abap_true ).
    ENDIF.

    wa_saida_0100-saldo_nf = wa_saida_0100-menge.

*-CS2021000386 - 28.04.2021 - JT - inicio
    READ TABLE tg_zsdt0251 WITH KEY docnum = tg_j_1bnflin-docnum. "Dados NF-e
    IF sy-subrc = 0.
      wa_saida_0100-lgort_v        = tg_zsdt0251-lgort_v.
      wa_saida_0100-qtde_utilizada = tg_zsdt0251-qtde_utilizada.
      wa_saida_0100-mblnr_mb1b     = tg_zsdt0251-mblnr_mb1b.
      wa_saida_0100-mjahr_mb1b     = tg_zsdt0251-mjahr_mb1b.
      wa_saida_0100-message_mb1b   = tg_zsdt0251-message_mb1b.

      IF     tg_zsdt0251-status_mb1b IS INITIAL.
        READ TABLE tg_icon WITH KEY name = 'ICON_LIGHT_OUT'.
        wa_saida_0100-status = tg_icon-id.
      ELSEIF tg_zsdt0251-status_mb1b = 'S'.
        READ TABLE tg_icon WITH KEY name = 'ICON_GREEN_LIGHT'.
        wa_saida_0100-status = tg_icon-id.
      ELSEIF tg_zsdt0251-status_mb1b = 'E'.
        READ TABLE tg_icon WITH KEY name = 'ICON_RED_LIGHT'.
        wa_saida_0100-status = tg_icon-id.
      ENDIF.
    ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim

    READ TABLE tg_zsdt0249 WITH KEY docnum = tg_j_1bnflin-docnum.
    IF sy-subrc EQ 0. "NF Com Boletim

      DATA(_nf_com_saldo) = abap_false.

      LOOP AT tg_zsdt0249 WHERE docnum = tg_j_1bnflin-docnum.
        DATA(_last_tabix_doc) = sy-tabix.
      ENDLOOP.

      LOOP AT tg_zsdt0249 WHERE docnum EQ tg_j_1bnflin-docnum..

        DATA(_tabix) = sy-tabix.

        FREE: wa_saida_0100-categ_soja,
              wa_saida_0100-descr_soja,
              wa_saida_0100-mblnr,
              wa_saida_0100-mjahr,
              wa_saida_0100-mblnr_mb1b,
              wa_saida_0100-mjahr_mb1b,
              wa_saida_0100-message_mb1b.
*             wa_saida_0100-bktxt.

*-CS2021000386 - 28.04.2021 - JT - inicio
        READ TABLE tg_zsdt0246 WITH KEY id_boletim = tg_zsdt0249-id_boletim.
        IF sy-subrc = 0.
          wa_saida_0100-categ_soja = tg_zsdt0246-categ_soja.

          IF     tg_zsdt0246-categ_soja = 'RR'.
            wa_saida_0100-descr_soja = 'RR-Soja Transgênica'.
          ELSEIF tg_zsdt0246-categ_soja = 'CO'.
            wa_saida_0100-descr_soja = 'CO-Soja Convencional'.
          ENDIF.
        ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim

        READ TABLE tg_zsdt0252 WITH KEY id_boletim = tg_zsdt0249-id_boletim
                                        branch     = tg_zsdt0249-branch
                                        charg      = tg_zsdt0249-charg
                                        id_agrp    = tg_zsdt0249-id_agrp.
        CHECK sy-subrc EQ 0.

*-CS2021000386 - 28.04.2021 - JT - inicio
        READ TABLE tg_mkpf WITH KEY mblnr = tg_zsdt0252-doc_prod_01
                                    mjahr = tg_zsdt0252-ano_doc_prod_01.
        IF sy-subrc = 0.
          wa_saida_0100-mblnr = tg_mkpf-mblnr.
          wa_saida_0100-mjahr = tg_mkpf-mjahr.
*         wa_saida_0100-bktxt = tg_mkpf-bktxt.
        ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim

        wa_saida_0100-menge_bol         = tg_zsdt0249-qtde_vinc.
        wa_saida_0100-saldo_nf          = wa_saida_0100-saldo_nf - wa_saida_0100-menge_bol.
        wa_saida_0100-id_boletim        = tg_zsdt0252-id_boletim.

        READ TABLE tg_zfiwrt0008 WITH KEY seq_lcto = tg_zsdt0252-seqlcto_devol.
        IF ( sy-subrc EQ 0 ) AND ( tg_zsdt0252-seqlcto_devol IS NOT INITIAL ).

          READ TABLE tg_j_1bnfe_active INTO DATA(wl_active_dev) WITH KEY docnum = tg_zfiwrt0008-docnum.
          IF ( sy-subrc EQ 0 ) AND ( tg_zfiwrt0008-docnum IS NOT INITIAL ).
            READ TABLE tg_j_1bnfdoc INTO DATA(wl_doc_dev) WITH KEY docnum = tg_zfiwrt0008-docnum.
            IF sy-subrc EQ 0.
              wa_saida_0100-docdat_nfe_ret  = wl_doc_dev-docdat.
              wa_saida_0100-nfenum_nfe_ret  = wl_doc_dev-nfenum.
              wa_saida_0100-docnum_nfe_ret  = wl_doc_dev-docnum.
              wa_saida_0100-dias_emissao    = wl_doc_dev-docdat - tg_j_1bnfdoc-docdat.

              PERFORM f_monta_chave_docnum USING wl_active_dev
                                        CHANGING wa_saida_0100-chave_nfe_ret.
            ENDIF.
          ENDIF.
        ENDIF.

        IF ( _tabix EQ _last_tabix_doc ).
          wa_saida_0100-saldo_final = wa_saida_0100-saldo_nf.

          IF wa_saida_0100-saldo_final > 0.
            _nf_com_saldo = abap_true.
          ENDIF.
        ENDIF.

*-CS2021000386 - 28.04.2021 - JT - inicio
        IF wa_saida_0100-categ_soja IN p_categ[].
          APPEND wa_saida_0100 TO it_saida_0100.
        ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim
      ENDLOOP.

*-CS2021000386 - 28.04.2021 - JT - inicio
*     DELETE it_saida_0100 WHERE categ_soja NOT IN p_categ[].
*-CS2021000386 - 28.04.2021 - JT - fim

      CASE abap_true.
        WHEN r_sld_n.
          IF _nf_com_saldo EQ abap_true.
            DELETE it_saida_0100 WHERE docnum EQ tg_j_1bnflin-docnum.
          ENDIF.
        WHEN r_sld_s.
          IF _nf_com_saldo EQ abap_false.
            DELETE it_saida_0100 WHERE docnum EQ tg_j_1bnflin-docnum.
          ENDIF.
      ENDCASE.

    ELSE. "NF Sem Boletim

      CHECK ( r_sld_s EQ abap_true ) OR ( r_sld_t EQ abap_true ).

      wa_saida_0100-saldo_final = wa_saida_0100-saldo_nf.

      IF wa_saida_0100-lgort_v EQ 'PO17'.

        wa_saida_0100-categ_soja = 'RR'.
        wa_saida_0100-descr_soja = 'RR-Soja Transgênica'.

      ELSEIF wa_saida_0100-lgort_v EQ 'PO58'.

        wa_saida_0100-categ_soja = 'CO'.
        wa_saida_0100-descr_soja = 'CO-Soja Convencional'.

"FF #191283 - inicio
      ELSEIF wa_saida_0100-lgort_v EQ 'POD2'.

        wa_saida_0100-categ_soja = 'RE'.
        wa_saida_0100-descr_soja = 'RE-Soja Transgênica EUDR'.


      ELSEIF wa_saida_0100-lgort_v EQ 'POD3'.

        wa_saida_0100-categ_soja = 'CE'.
        wa_saida_0100-descr_soja = '-Soja Convencional EUDR'.

"FF #191283 - fim



      ENDIF.

*-CS2021000386 - 28.04.2021 - JT - inicio
      IF wa_saida_0100-categ_soja IN p_categ[].
        APPEND wa_saida_0100 TO it_saida_0100.
      ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_movimentar_deposito CHANGING p_erro.

  DATA: t_index_rows TYPE lvc_t_row,
        t_row_no     TYPE lvc_t_roid,
        w_et_row_no  LIKE LINE OF t_row_no.

  FREE: t_index_rows,
        t_row_no,
        p_erro.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF t_row_no[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não foi selecionada nenhuma linha.'.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  LOOP AT t_row_no INTO w_et_row_no.
    CLEAR wa_saida_0100.
    READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX w_et_row_no-row_id.
    CHECK sy-subrc = 0.
    IF wa_saida_0100-qtde_utilizada > 0.
      p_erro = abap_true.
      PERFORM f_fill_color_saida CHANGING wa_saida_0100.
      MODIFY it_saida_0100   FROM wa_saida_0100 INDEX w_et_row_no-row_id.
    ENDIF.
  ENDLOOP.

  IF p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Linhas marcadas já possuem quantidade consumida!'.
    PERFORM f_refresh_alv USING '0100'.
    EXIT.
  ENDIF.

  LOOP AT t_row_no INTO w_et_row_no.
    CLEAR wa_saida_0100.
    READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX w_et_row_no-row_id.
    CHECK sy-subrc = 0.
    PERFORM f_executa_mb1b.
  ENDLOOP.
ENDFORM.

FORM f_executa_mb1b.

  FREE: wl_header,
        wl_item,
        tl_item,
        wl_mblnr,
        wl_mjahr,
        tl_return,
        l_message,
        l_lgort.

  CLEAR: tg_mkpf,
         tg_zsdt0251,
         tg_j_1bnflin,
         tg_vbrp.

  READ TABLE tg_mkpf      WITH KEY mblnr  = wa_saida_0100-mblnr
                                   mjahr  = wa_saida_0100-mjahr.
  READ TABLE tg_j_1bnflin WITH KEY docnum = wa_saida_0100-docnum.
  READ TABLE tg_vbrp      WITH KEY vbeln  = tg_j_1bnflin-refkey.
  READ TABLE tg_zsdt0251  WITH KEY docnum = wa_saida_0100-docnum. "Dados NF-e

*---------------------------------------------
* define deposito destino
*---------------------------------------------
  IF     tg_zsdt0251-lgort_v = 'PO58'.
    l_lgort = 'PO17'.
  ELSEIF tg_zsdt0251-lgort_v = 'PO17'.
    l_lgort = 'PO58'.

"FF #191283 - inicio
  ELSEIF tg_zsdt0251-lgort_v = 'POD2'.
    l_lgort = 'POD3'.

  ELSEIF tg_zsdt0251-lgort_v = 'POD3'.
    l_lgort = 'POD2'.
"FF #191283 - fim


  ENDIF.



*-CODE
  wl_code-gm_code      = '06'.

*-header
  wl_header-pstng_date = sy-datum.
  wl_header-doc_date   = sy-datum.
  wl_header-header_txt = tg_vbrp-vgbel.  "tg_mkpf-bktxt.

*-itens
*--> 19.06.2023 - Migration S4 – MIGNOW - Start
"  wl_item-material     = tg_j_1bnflin-matnr.
DATA(v_len1) = strlen( tg_j_1bnflin-matnr ).
IF v_len1 > 18.
wl_item-MATERIAL_LONG = tg_j_1bnflin-matnr .
ELSE.
wl_item-material = tg_j_1bnflin-matnr .
ENDIF.
*<-- 19.06.2023 - Migration S4 – MIGNOW – End
  wl_item-plant        = tg_zsdt0251-werks_v. "A Fixar
  wl_item-stge_loc     = tg_zsdt0251-lgort_v.
  wl_item-batch        = tg_zsdt0251-charg. " Lote_FILIAL
  wl_item-move_type    = '311'.
* wl_item-move_plant   = tg_zsdt0251-werks_v.
  wl_item-move_stloc   = l_lgort.
  wl_item-move_batch   = tg_zsdt0251-charg.
  wl_item-entry_qnt    = tg_j_1bnflin-menge.
  APPEND wl_item      TO tl_item.

*---------------------------------------------
* movimenta deposito
*---------------------------------------------
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      goodsmvt_header  = wl_header
      goodsmvt_code    = wl_code
    IMPORTING
      materialdocument = wl_mblnr
      matdocumentyear  = wl_mjahr
    TABLES
      goodsmvt_item    = tl_item
      return           = tl_return.

  IF wl_mblnr IS NOT INITIAL.
    UPDATE zsdt0251 SET mblnr_mb1b   = wl_mblnr
                        mjahr_mb1b   = wl_mjahr
                        lgort_v      = l_lgort
                        status_mb1b  = 'S'
                        message_mb1b = ''
                  WHERE docnum = wa_saida_0100-docnum.
  ELSE.
    LOOP AT tl_return WHERE type = 'E'.
      IF sy-tabix = 1.
        l_message = tl_return-message.
      ELSE.
        CONCATENATE l_message tl_return-message
               INTO l_message
            SEPARATED BY '|'.
      ENDIF.
    ENDLOOP.

    UPDATE zsdt0251 SET mblnr_mb1b   = ''
                        mjahr_mb1b   = ''
                        status_mb1b  = 'E'
                        message_mb1b = l_message
                  WHERE docnum = wa_saida_0100-docnum.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.

FORM f_fill_color_saida  CHANGING p_saida_0100 TYPE ty_saida_0100.

  DATA: wl_color  TYPE kkblo_specialcol.

  CLEAR: wl_color.
  wl_color-fieldname = 'QTDE_UTILIZADA'.
  wl_color-color-col = 6.
  wl_color-color-int = 1.
  wl_color-color-inv = 1.
  APPEND wl_color TO p_saida_0100-color.

* wl_color-fieldname = 'SALDO_EXPORTAR'.
* APPEND wl_color TO p_saida_0100-color.

ENDFORM.

FORM f_show_alv.

  CALL SCREEN 0100.

ENDFORM.

FORM f_monta_chave_docnum USING p_active TYPE j_1bnfe_active
                       CHANGING p_chave.

  CLEAR: p_chave.

  CONCATENATE p_active-regio
              p_active-nfyear
              p_active-nfmonth
              p_active-stcd1
              p_active-model
              p_active-serie
              p_active-nfnum9
              p_active-docnum9
              p_active-cdv INTO p_chave.

  IF strlen( p_chave ) NE 44.
    CLEAR: p_chave.
  ENDIF.

ENDFORM.

FORM f_check_auth_tables_nfe  TABLES t_active STRUCTURE j_1bnfe_active
                                     t_doc    STRUCTURE j_1bnfdoc.

  DATA: v_candat_null TYPE j_1bnfdoc-candat.

  CLEAR: v_candat_null.

  DELETE t_active WHERE NOT ( docsta  EQ '1' AND cancel EQ abap_false AND scssta NE '2' ).
  DELETE t_doc    WHERE NOT ( docstat EQ '1' AND cancel EQ abap_false AND candat EQ v_candat_null ).

ENDFORM.
