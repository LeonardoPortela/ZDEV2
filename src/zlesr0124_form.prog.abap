*&---------------------------------------------------------------------*
*&  Include           ZLESR0111_FORM
*&---------------------------------------------------------------------*

FORM f_refresh_alv USING p_alv.

  CASE p_alv.
    WHEN '0100'.

      CHECK obj_alv_0100 IS NOT INITIAL.

      CALL METHOD obj_alv_0100->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    WHEN '0110'.

  ENDCASE.

ENDFORM.

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

        01  'ZLEST0146'      'ID_RECEPCAO'              'IT_SAIDA_0100_01'   'ID_RECEPCAO'             'Id.Recepção'          '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        04  'ZLEST0146'      'CNPJ_RESPONSAVEL'         'IT_SAIDA_0100_01'   'CNPJ_RESPONSAVEL'        'CNPJ Responsável'     '17'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        05  'ZLEST0146'      'LOCAL_CODIGO_URF'         'IT_SAIDA_0100_01'   'LOCAL_CODIGO_URF'        'Cod.URF'              '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        06  'ZLEST0146'      'LOCAL_CODIGO_RA'          'IT_SAIDA_0100_01'   'LOCAL_CODIGO_RA'         'Cod.RA '              '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        07  'ZLEST0146'      'TRANSPORTADOR_CNPJ'       'IT_SAIDA_0100_01'   'TRANSPORTADOR_CNPJ'      'Transportador CNPJ'   '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        08  'ZLEST0146'      'TRANSPORTADOR_CPF'        'IT_SAIDA_0100_01'   'TRANSPORTADOR_CPF'       'Transportador CPF'    '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        09  'ZLEST0146'      'PESO_AFERIDO_RECEPCAO'    'IT_SAIDA_0100_01'   'PESO_AFERIDO_RECEPCAO'   'Peso Aferido.Rec.'    '17'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        10  'ZLEST0147'      'CHAVE_NFE'                'IT_SAIDA_0100_01'   'CHAVE_NFE'               'Chave NF-e'           '45'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        10  'ZLEST0147'      'CHAVE_NFF'                'IT_SAIDA_0100_01'   'CHAVE_NFF'               'Chave NF-f'           '35'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        11  'ZLEST0147'      'EMISSOR_CNPJ'             'IT_SAIDA_0100_01'   'EMISSOR_CNPJ'            'Fornecedor CNPJ'      '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        12  'ZLEST0147'      'EMISSOR_CPF'              'IT_SAIDA_0100_01'   'EMISSOR_CPF'             'Fornecedor CPF'       '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        13  'ZLEST0147'      'EMISSOR_IE'               'IT_SAIDA_0100_01'   'EMISSOR_IE'              'Fornecedor IE'        '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        14  'ZLEST0147'      'MODEL'                    'IT_SAIDA_0100_01'   'MODEL'                   'Modelo'               '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        15  'ZLEST0147'      'SERIE'                    'IT_SAIDA_0100_01'   'SERIE'                   'Série'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        16  'ZLEST0147'      'NFNUM9'                   'IT_SAIDA_0100_01'   'NFNUM9'                  'Núm.NF'               '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        16  'ZLEST0147'      'DOCNUM'                   'IT_SAIDA_0100_01'   'DOCNUM'                  'Docnum'               '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        17  'ZLEST0147'      'DT_EMISSAO'               'IT_SAIDA_0100_01'   'DT_EMISSAO'              'Dt.Emissão'           '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        18  'ZLEST0146'      'DT_RECEPCAO'              'IT_SAIDA_0100_01'   'DT_RECEPCAO'             'Dt.Recepção'          '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        19  'MARA'           'MATNR'                    'IT_SAIDA_0100_01'   'MATNR'                   'Material'             '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        20  'MAKT'           'MAKTX'                    'IT_SAIDA_0100_01'   'MAKTX'                   'Ds.Material'          '40'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        21  'LFA1'           'LIFNR'                    'IT_SAIDA_0100_01'   'CD_PORTO'                'Cd.Porto'             '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        21  ''               ''                         'IT_SAIDA_0100_01'   'DS_PORTO'                'Ds.Porto'             '40'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        22  ''               ''                         'IT_SAIDA_0100_01'   'CITY_PORTO'              'Cidade Porto'         '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        23  ''               ''                         'IT_SAIDA_0100_01'   'NF_TERCEIRO'             'NF.Terc.'             '08'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
        24  'ZLEST0146'      'DT_IMPORTACAO'            'IT_SAIDA_0100_01'   'DT_IMPORTACAO'           'Dt.Importação'        '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        25  'ZLEST0146'      'HR_IMPORTACAO'            'IT_SAIDA_0100_01'   'HR_IMPORTACAO'           'Hr.Importação'        '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        26  'ZLEST0146'      'US_IMPORTACAO'            'IT_SAIDA_0100_01'   'US_IMPORTACAO'           'Us.Importação'        '13'   ' '    ''  ' ' ' ' ' ' ' ' '' .


    WHEN '0110'.


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
                           VALUE(p_check).

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
  wa_fcat-checkbox    = p_check.

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

  CLEAR: wa_saida_0100_01,
         it_saida_0100_01[],
         tg_0142[],
         tg_0146[],
         tg_0147[],
         tg_0170[],
         tg_lfa1[],
         tg_makt[],
         tg_lin[],
         tg_adrc[],
         tg_zsdt0168[],
         vg_not_found.

ENDFORM.

FORM f_selecionar_dados .

  PERFORM f_limpa_variaveis.

  SELECT *
    FROM zlest0146 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0146
   WHERE a~dt_recepcao   IN p_dtrcc
     AND a~dt_importacao IN p_dtimp
     AND a~us_importacao IN p_usimp
     AND a~cancel        EQ abap_false
     AND a~importado     EQ abap_true
     AND EXISTS ( SELECT *
                    FROM zlest0147 AS b
                   WHERE b~id_recepcao   EQ a~id_recepcao
                     AND b~emissor_cnpj  IN p_stcd1
                     AND b~emissor_cpf   IN p_stcd2
                     AND b~dt_emissao    IN p_dtemi
                     AND b~nfnum9        IN p_numnf
                  ).

  CHECK tg_0146[] IS NOT INITIAL.

  SELECT *
    FROM zlest0147 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0147
     FOR ALL ENTRIES IN tg_0146
   WHERE id_recepcao EQ tg_0146-id_recepcao.

  LOOP AT tg_0147.
    tg_0147-stcd1 = tg_0147-emissor_cnpj.
    MODIFY tg_0147.
  ENDLOOP.


  SELECT *
    FROM zsdt0168 INTO TABLE tg_zsdt0168
     FOR ALL ENTRIES IN tg_0146
   WHERE codigo_ra = tg_0146-local_codigo_ra.

  IF tg_zsdt0168[] IS NOT INITIAL.
    SELECT *
      FROM lfa1 INTO CORRESPONDING FIELDS OF TABLE tg_lfa1
       FOR ALL ENTRIES IN tg_zsdt0168
     WHERE lifnr = tg_zsdt0168-lifnr.
  ENDIF.

  IF tg_0147[] IS NOT INITIAL.

    SELECT *
      FROM j_1bnflin INTO CORRESPONDING FIELDS OF TABLE tg_lin
       FOR ALL ENTRIES IN tg_0147
     WHERE docnum EQ tg_0147-docnum.

    IF tg_lin[] IS NOT INITIAL.
      SELECT *
        FROM makt INTO CORRESPONDING FIELDS OF TABLE tg_makt
         FOR ALL ENTRIES IN tg_lin
       WHERE matnr EQ tg_lin-matnr
         AND spras EQ sy-langu.
    ENDIF.

    SELECT *
      FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE tg_lfa1
       FOR ALL ENTRIES IN tg_0147
     WHERE stcd1 EQ tg_0147-stcd1.

  ENDIF.

  IF tg_lfa1[] IS NOT INITIAL.
    SELECT *
      FROM adrc INTO CORRESPONDING FIELDS OF TABLE tg_adrc
       FOR ALL ENTRIES IN tg_lfa1
    WHERE addrnumber = tg_lfa1-adrnr.
  ENDIF.


ENDFORM.

FORM f_processa_dados .

  CHECK vg_not_found IS INITIAL.

  LOOP AT tg_0146.

    CLEAR: wa_saida_0100_01, tg_0147.

    READ TABLE tg_0147 WITH KEY id_recepcao = tg_0146-id_recepcao.

    wa_saida_0100_01-id_recepcao            =  tg_0146-id_recepcao.
    wa_saida_0100_01-cnpj_responsavel       =  tg_0146-cnpj_responsavel.
    wa_saida_0100_01-local_codigo_urf       =  tg_0146-local_codigo_urf.
    wa_saida_0100_01-local_codigo_ra        =  tg_0146-local_codigo_ra.
    wa_saida_0100_01-transportador_cnpj     =  tg_0146-transportador_cnpj.
    wa_saida_0100_01-transportador_cpf      =  tg_0146-transportador_cpf.
    wa_saida_0100_01-peso_aferido_recepcao  =  tg_0146-peso_aferido_recepcao.
    wa_saida_0100_01-chave_nfe              =  tg_0147-chave_nfe.
    wa_saida_0100_01-chave_nff              =  tg_0147-chave_nff.
    wa_saida_0100_01-emissor_cnpj           =  tg_0147-emissor_cnpj.
    wa_saida_0100_01-emissor_cpf            =  tg_0147-emissor_cpf.
    wa_saida_0100_01-emissor_ie             =  tg_0147-emissor_ie.
    wa_saida_0100_01-nfnum9                 =  tg_0147-nfnum9.
    wa_saida_0100_01-model                  =  tg_0147-model.
    wa_saida_0100_01-serie                  =  tg_0147-serie.
    wa_saida_0100_01-docnum                 =  tg_0147-docnum.
    wa_saida_0100_01-dt_emissao             =  tg_0147-dt_emissao.
    wa_saida_0100_01-dt_recepcao            =  tg_0146-dt_recepcao.
    wa_saida_0100_01-dt_importacao          =  tg_0146-dt_importacao.
    wa_saida_0100_01-hr_importacao          =  tg_0146-hr_importacao.
    wa_saida_0100_01-us_importacao          =  tg_0146-us_importacao.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_saida_0100_01-nfnum9
      IMPORTING
        output = wa_saida_0100_01-nfnum9.


    READ TABLE tg_lin WITH KEY docnum = wa_saida_0100_01-docnum.

    IF ( sy-subrc EQ 0 ) AND ( tg_lin-docnum IS NOT INITIAL ).
      wa_saida_0100_01-matnr = tg_lin-matnr.

      READ TABLE tg_makt WITH KEY matnr = tg_lin-matnr.
      IF sy-subrc EQ 0.
        wa_saida_0100_01-maktx = tg_makt-maktx.
      ENDIF.
    ENDIF.

    READ TABLE tg_zsdt0168 WITH KEY codigo_ra = wa_saida_0100_01-local_codigo_ra.
    IF ( sy-subrc EQ 0 ) AND ( tg_zsdt0168-lifnr IS NOT INITIAL ).
      READ TABLE tg_lfa1 WITH KEY lifnr = tg_zsdt0168-lifnr.
      IF sy-subrc EQ 0.
        wa_saida_0100_01-cd_porto = tg_lfa1-lifnr.
        wa_saida_0100_01-ds_porto = tg_lfa1-name1.
        READ TABLE tg_adrc WITH KEY addrnumber = tg_lfa1-adrnr.
        IF sy-subrc EQ 0.
          wa_saida_0100_01-city_porto = tg_adrc-city1.
        ENDIF.
      ENDIF.
    ENDIF.

    IF tg_0147-stcd1 IS NOT INITIAL.
      READ TABLE tg_lfa1 WITH KEY stcd1 = tg_0147-stcd1
                                  ktokk = 'ZFIC'. "Intercompany
      IF sy-subrc NE 0.
        wa_saida_0100_01-nf_terceiro = icon_okay.
      ENDIF.
    ENDIF.

    "Material
    IF ( p_matnr[] IS NOT INITIAL ) AND ( wa_saida_0100_01-matnr NOT IN p_matnr[] ).
      CONTINUE.
    ENDIF.

    IF ( p_cdprt[] IS NOT INITIAL ) AND ( wa_saida_0100_01-cd_porto NOT IN p_cdprt[] ).
      CONTINUE.
    ENDIF.

    APPEND wa_saida_0100_01 TO it_saida_0100_01.

  ENDLOOP.

  SORT it_saida_0100_01 BY id_recepcao DESCENDING.


"" AHSS - Chamado 145585 - 17/07/2024 - Ajuste para filtrar pela chave ##Inicio
  IF p_knfe is not initial.

    DELETE  it_saida_0100_01 WHERE chave_nfe NOT IN p_knfe.

  ENDIF.
"" AHSS - Chamado 145585 - 17/07/2024 - Ajuste para filtrar pela chave ##Fim




ENDFORM.

FORM f_cancel_recep_carga.

  DATA: zcl_cct_recepcao_carga TYPE REF TO zcl_cct_recepcao_carga.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente cancelar a(s) Recepção(ões) de Carga(s) selecionada(s)?'
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

    READ TABLE it_saida_0100_01 INTO wa_saida_0100_01 INDEX wa_sel_rows-index.

    CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100_01-id_recepcao IS NOT INITIAL ).

    DATA(_cancelada) = abap_false.

    PERFORM f_cancel_recepcao USING wa_saida_0100_01-id_recepcao
                           CHANGING _cancelada.
  ENDLOOP.

  MESSAGE 'Recepção de Carga(s) canceladas(s) com sucesso!' TYPE 'S'.


ENDFORM.

FORM f_log_cancel_recepcao.

  CLEAR: tg_0146_cancel[].

  SELECT *
    FROM zlest0146 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0146_cancel
   WHERE a~dt_recepcao  IN p_dtrcc
     AND a~importado    EQ abap_true
     AND a~cancel       EQ 'X'.

  CHECK tg_0146_cancel[] IS NOT INITIAL.

  SELECT *
    FROM zlest0147 INTO TABLE @DATA(tg_0147_cancel)
     FOR ALL ENTRIES IN @tg_0146_cancel
   WHERE id_recepcao EQ @tg_0146_cancel-id_recepcao.

  CHECK tg_0147_cancel[] IS NOT INITIAL.

  LOOP AT tg_0146_cancel.
    READ TABLE tg_0147_cancel INTO DATA(wl_0147_cancel) WITH KEY id_recepcao = tg_0146_cancel-id_recepcao.
    IF sy-subrc = 0.
      tg_0146_cancel-chave_nfe      = wl_0147_cancel-chave_nfe.
      tg_0146_cancel-chave_nff      = wl_0147_cancel-chave_nff.
      tg_0146_cancel-emissor_cnpj   = wl_0147_cancel-emissor_cnpj.
      tg_0146_cancel-emissor_cpf    = wl_0147_cancel-emissor_cpf.
      tg_0146_cancel-emissor_ie     = wl_0147_cancel-emissor_ie.
      tg_0146_cancel-nfnum9         = wl_0147_cancel-nfnum9.
      tg_0146_cancel-dt_emissao     = wl_0147_cancel-dt_emissao.
    ENDIF.

    MODIFY tg_0146_cancel.
  ENDLOOP.

 PERFORM f_montar_layout_log_cancel.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 170
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tg_0146_cancel.


ENDFORM.

FORM f_executar .

  PERFORM: f_selecionar_dados,
           f_processa_dados.

  CALL SCREEN 0100.

ENDFORM.

FORM f_ler_diretorio USING p_tp_import.

  DATA: v_index         TYPE sy-tabix,
        v_mask_unix     TYPE epsfilnam,
        v_mask_locl(60) TYPE c,
        v_importado.

  PERFORM f_preenche_caminho.

  CLEAR: tg_msg[].

  CHECK: wa_path-p_input IS NOT INITIAL.

  v_prefix_ent = c_all.

  "-------------------------------------------------------------------*
  " Importar Arquivos Excel
  "-------------------------------------------------------------------*
  CLEAR: t_dir_loc_f[], t_dir_local[].

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = text-i01.

  CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
    EXPORTING
      directory  = wa_path-p_input
      filter     = '*.xlsx'
    TABLES
      file_table = t_dir_loc_f
      dir_table  = t_dir_local
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.

* Consiste arquivo apto para processamento
  LOOP AT t_dir_loc_f INTO DATA(wa_files_doc).
    CHECK wa_files_doc-pathname(1) NE '~'.

    PERFORM f_carrega_arq USING wa_files_doc-pathname
                                c_l
                                abap_true. "Excel
  ENDLOOP.

  "-------------------------------------------------------------------*
  " Importar Arquivos txt
  "-------------------------------------------------------------------*
  CLEAR: t_dir_loc_f[], t_dir_local[].

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = text-i02.

  CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
    EXPORTING
      directory  = wa_path-p_input
      filter     = c_mask_loc
    TABLES
      file_table = t_dir_loc_f
      dir_table  = t_dir_local
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.

  IF sy-subrc <> 0 OR t_dir_loc_f[] IS INITIAL.
*    MESSAGE W899(FI)
*       WITH 'Diretório Local: ' WA_PATH-P_INPUT
*            ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
*            C_MASK_LOC
*       INTO V_MENSAGEM.
*
*    MESSAGE V_MENSAGEM TYPE C_S.
  ELSE.
*   Consiste arquivo apto para processamento
    LOOP AT t_dir_loc_f INTO wa_files_doc.
      PERFORM f_carrega_arq USING wa_files_doc-pathname
                                  c_l
                                  abap_false. "Excel
    ENDLOOP.
  ENDIF.

  PERFORM f_log_processamento.
  EXIT.

  WRITE: '======| Resumo Processamento |=========================================='.

  "Mensagem com Erro
  tg_msg_aux[] = tg_msg[].
  DELETE tg_msg_aux WHERE type NE 'E'.
  IF lines( tg_msg_aux[] ) NE 0.
    WRITE: 'Total de Registros com Erro:   ', lines( tg_msg_aux[] ).
    WRITE: /.
  ENDIF.

  "Mensagem com Atenção
  tg_msg_aux[] = tg_msg[].
  DELETE tg_msg_aux WHERE type NE 'W'.
  IF lines( tg_msg_aux[] ) NE 0.
    WRITE: 'Total de Registros com Atenção:   ', lines( tg_msg_aux[] ).
    WRITE: /.
  ENDIF.

  "Mensagem com Sucesso
  tg_msg_aux[] = tg_msg[].
  DELETE tg_msg_aux WHERE type NE 'S'.
  IF lines( tg_msg_aux[] ) NE 0.
    WRITE: 'Total de Registros com Sucesso:   ', lines( tg_msg_aux[] ).
    WRITE: /.
  ENDIF.

  WRITE: '======| Detalhamento |================================================'.
  WRITE: /.

  "Mensagem com Erro
  PERFORM f_log_detalhamento TABLES tg_msg
                              USING 'E'
                                    'Erro'.

  "Mensagem com Atenção
  PERFORM f_log_detalhamento TABLES tg_msg
                              USING 'W'
                                    'Atenção'.

  "Mensagem com Sucesso
  PERFORM f_log_detalhamento TABLES tg_msg
                              USING 'S'
                                    'Sucesso'.

ENDFORM.                    " LE_DIRETORIO

FORM f_carrega_arq USING p_filename
                         p_tipo
                         p_excel.

  DATA: v_file  TYPE rlgrap-filename.

  CLEAR: tg_file[], tg_planilha[].

  CONCATENATE wa_path-p_input p_filename INTO DATA(_arquivo).

  IF p_excel EQ abap_true.

    v_file = _arquivo.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = v_file
        i_begin_col             = 1
        i_begin_row             = 2
        i_end_col               = 55
        i_end_row               = 10000
      TABLES
        intern                  = tg_planilha
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.

  ELSE.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = _arquivo
        filetype                = c_asc
      TABLES
        data_tab                = tg_file
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.

  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF p_excel EQ abap_true.
    PERFORM f_processa_arquivo_excel USING p_filename.
  ELSE.
    PERFORM f_processa_arquivo       USING p_filename.
  ENDIF.

ENDFORM.                    " CARREGA_ARQ

FORM f_processa_arquivo USING p_filename.

  DATA: wl_retorno          TYPE zde_retorno_proc.

  DATA: wl_zlest0146 TYPE zlest0146,
        wl_zlest0147 TYPE zlest0147,
        lt_zlest0168 TYPE zlest0168_t.

  DATA: v_valor_aux TYPE c LENGTH 255,
        v_valor_c13 TYPE c LENGTH 13.

  DELETE tg_file WHERE linha IS INITIAL.

  CLEAR: it_nfe_cons_portal[], wl_nfe_cons_portal.

  LOOP AT tg_file.
    CLEAR: wl_nfe_cons_portal.

    wl_nfe_cons_portal = tg_file+72(44).

    IF strlen( wl_nfe_cons_portal ) = 44.
      APPEND wl_nfe_cons_portal TO it_nfe_cons_portal.
    ENDIF.
  ENDLOOP.

  IF it_nfe_cons_portal[] IS NOT INITIAL.
    CALL FUNCTION 'ZCCT_CONFIRMA_REC_NF_PORTAL'
      EXPORTING
        i_chaves = it_nfe_cons_portal[].
  ENDIF.

  LOOP AT tg_file.

    DATA(_tabix) = sy-tabix.

    CLEAR: wl_retorno, wl_zlest0146, wl_zlest0147, lt_zlest0168.

    wl_zlest0146-cnpj_responsavel          = tg_file+00(14).
    wl_zlest0146-local_codigo_urf          = tg_file+14(10).
    wl_zlest0146-local_codigo_ra           = tg_file+24(10).
    wl_zlest0146-local_latitude            = tg_file+34(12).
    wl_zlest0146-local_longitude           = tg_file+46(12).

*--------------------------------------------------------------------------*
*   Transportador CNPJ/CPF
*--------------------------------------------------------------------------*
    v_valor_aux = tg_file+58(14).
    CONDENSE v_valor_aux NO-GAPS.

    IF strlen( v_valor_aux ) = 14.
      wl_zlest0146-transportador_cnpj      = v_valor_aux.
    ELSEIF strlen( v_valor_aux ) = 11.
      wl_zlest0146-transportador_cpf       = v_valor_aux.
    ENDIF.

    wl_zlest0147-chave_nfe                 = tg_file+72(44).
    wl_zlest0147-nfnum9                    = tg_file+116(9).
    wl_zlest0147-serie                     = tg_file+125(3).
    wl_zlest0147-model                     = tg_file+128(3).
    CONDENSE wl_zlest0147-model NO-GAPS.

    IF wl_zlest0147-model NE '55'.
      wl_zlest0147-nfnum                   = wl_zlest0147-nfnum9.
    ENDIF.

    wl_zlest0147-nfyear                    = tg_file+131(2).
    wl_zlest0147-nfmonth                   = tg_file+133(2).
    wl_zlest0147-sigla_uf_emissor          = tg_file+135(5).

    CONDENSE wl_zlest0147-sigla_uf_emissor NO-GAPS.
    IF strlen( wl_zlest0147-sigla_uf_emissor ) EQ 2.
      wl_zlest0147-sigla_uf_emissor    = 'BR-' && wl_zlest0147-sigla_uf_emissor.
    ENDIF.

    wl_zlest0147-dt_emissao                = tg_file+140(8).

*--------------------------------------------------------------------------*
*   Emissor CNPJ/CPF
*--------------------------------------------------------------------------*

    v_valor_aux = tg_file+148(14).
    CONDENSE v_valor_aux NO-GAPS.

    IF strlen( v_valor_aux ) = 14.
      wl_zlest0147-emissor_cnpj            = v_valor_aux.
    ELSEIF strlen( v_valor_aux ) = 11.
      wl_zlest0147-emissor_cpf             = v_valor_aux.
    ENDIF.

    wl_zlest0147-emissor_ie                = tg_file+162(18).
    CONDENSE wl_zlest0147-emissor_ie NO-GAPS.

    v_valor_c13 = tg_file+180(13). "Number(13,3)
    REPLACE ALL OCCURRENCES OF ',' IN v_valor_c13 WITH ''.
    REPLACE ALL OCCURRENCES OF '.' IN v_valor_c13 WITH ''.
    CONDENSE v_valor_c13 NO-GAPS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_valor_c13
      IMPORTING
        output = v_valor_c13.

    v_valor_aux = v_valor_c13(10) && '.' && v_valor_c13+10(3).

    wl_zlest0146-peso_aferido_recepcao     = v_valor_aux.

    wl_zlest0146-dt_recepcao               = tg_file+193(8).

    PERFORM f_processa_recepcao USING 'X' "Gravar Registro
                             CHANGING wl_zlest0146
                                      wl_zlest0147
                                      lt_zlest0168
                                      wl_retorno.

    IF wl_retorno-type IS NOT INITIAL.
      PERFORM f_controle_msg  USING p_filename
                                    wl_retorno-type
                                    wl_retorno-msgno
                                    wl_retorno-texto
                                    _tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM f_processa_arquivo_excel USING p_filename.

  DATA: wl_retorno          TYPE zde_retorno_proc.

  DATA: wl_zlest0146 TYPE zlest0146,
        wl_zlest0147 TYPE zlest0147,
        lt_zlest0168 TYPE zlest0168_t.

  DATA: v_valor_aux TYPE c LENGTH 255,
        v_valor_c13 TYPE c LENGTH 13.


  CLEAR: it_nfe_cons_portal[], wl_nfe_cons_portal.

  LOOP AT tg_planilha.

    AT NEW row.
      CLEAR: wl_nfe_cons_portal.
    ENDAT.

    IF tg_planilha-value(1) = space.
      SHIFT tg_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE tg_planilha-col.
      WHEN 7.
        wl_nfe_cons_portal  = tg_planilha-value.
    ENDCASE.

    AT END OF row.
      IF strlen( wl_nfe_cons_portal ) = 44.
        APPEND wl_nfe_cons_portal TO it_nfe_cons_portal.
      ENDIF.
    ENDAT.
  ENDLOOP.

  IF it_nfe_cons_portal[] IS NOT INITIAL.
    CALL FUNCTION 'ZCCT_CONFIRMA_REC_NF_PORTAL'
      EXPORTING
        i_chaves = it_nfe_cons_portal[].
  ENDIF.

  LOOP AT tg_planilha.

    AT NEW row.
      DATA(_tabix) = sy-tabix.
      CLEAR: wl_retorno, wl_zlest0146, wl_zlest0147, lt_zlest0168.
    ENDAT.

    IF tg_planilha-value(1) = space.
      SHIFT tg_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE tg_planilha-col.
      WHEN 1.
        wl_zlest0146-cnpj_responsavel  = tg_planilha-value.
      WHEN 2.
        wl_zlest0146-local_codigo_urf  = tg_planilha-value.
      WHEN 3.
        wl_zlest0146-local_codigo_ra   = tg_planilha-value.
      WHEN 4.
        wl_zlest0146-local_latitude   = tg_planilha-value.
      WHEN 5.
        wl_zlest0146-local_longitude   = tg_planilha-value.

      WHEN 6. "Transportador CNPJ/CPF

        v_valor_aux                    = tg_planilha-value.
        CONDENSE v_valor_aux NO-GAPS.

        IF strlen( v_valor_aux ) = 14.
          wl_zlest0146-transportador_cnpj      = v_valor_aux.
        ELSEIF strlen( v_valor_aux ) = 11.
          wl_zlest0146-transportador_cpf       = v_valor_aux.
        ENDIF.

      WHEN 7.
        wl_zlest0147-chave_nfe          = tg_planilha-value.
      WHEN 8.
        wl_zlest0147-nfnum9             = tg_planilha-value.
      WHEN 9.
        wl_zlest0147-serie              = tg_planilha-value.
      WHEN 10.

        wl_zlest0147-model              = tg_planilha-value.
        CONDENSE wl_zlest0147-model NO-GAPS.

        IF wl_zlest0147-model NE '55'.
          wl_zlest0147-nfnum            = wl_zlest0147-nfnum9.
        ENDIF.

      WHEN 11.
        v_valor_aux                     = tg_planilha-value.
        CONDENSE v_valor_aux NO-GAPS.

        wl_zlest0147-nfyear             = v_valor_aux(2).
        wl_zlest0147-nfmonth            = v_valor_aux+2(2).
      WHEN 12.
        wl_zlest0147-sigla_uf_emissor   = tg_planilha-value.

        CONDENSE wl_zlest0147-sigla_uf_emissor NO-GAPS.
        IF strlen( wl_zlest0147-sigla_uf_emissor ) EQ 2.
          wl_zlest0147-sigla_uf_emissor    = 'BR-' && wl_zlest0147-sigla_uf_emissor.
        ENDIF.
      WHEN 13.
        wl_zlest0147-dt_emissao          = tg_planilha-value.
      WHEN 14. "Emissor CNPJ/CPF

        v_valor_aux = tg_planilha-value.
        CONDENSE v_valor_aux NO-GAPS.

        IF strlen( v_valor_aux ) = 14.
          wl_zlest0147-emissor_cnpj            = v_valor_aux.
        ELSEIF strlen( v_valor_aux ) = 11.
          wl_zlest0147-emissor_cpf             = v_valor_aux.
        ENDIF.
      WHEN 15.
        wl_zlest0147-emissor_ie                = tg_planilha-value.
        CONDENSE wl_zlest0147-emissor_ie NO-GAPS.
      WHEN 16.
        v_valor_c13 = tg_planilha-value.
        REPLACE ALL OCCURRENCES OF ',' IN v_valor_c13 WITH '.'.
        CONDENSE v_valor_c13 NO-GAPS.

        wl_zlest0146-peso_aferido_recepcao     = v_valor_c13.
      WHEN 17.
        wl_zlest0146-dt_recepcao               = tg_planilha-value.
    ENDCASE.

    AT END OF row.

      PERFORM f_processa_recepcao USING 'X' "Gravar Registro
                             CHANGING wl_zlest0146
                                      wl_zlest0147
                                      lt_zlest0168
                                      wl_retorno.

      IF wl_retorno-type IS NOT INITIAL.
        PERFORM f_controle_msg  USING p_filename
                                      wl_retorno-type
                                      wl_retorno-msgno
                                      wl_retorno-texto
                                      _tabix.
      ENDIF.

    ENDAT.
  ENDLOOP.

ENDFORM.

FORM f_preenche_caminho.

  wa_path-p_input = 'C:\Amaggi\ImpRecepcaoCargaCCT\'.

ENDFORM.                    " PREENCHE_CAMINHO

FORM f_controle_msg  USING  p_filename
                            p_type
                            p_msgno
                            p_texto
                            p_tabix.

  CLEAR: tg_msg.

  tg_msg-filename   = p_filename.
  tg_msg-repid      = sy-repid.
  tg_msg-type       = p_type.
  tg_msg-msgno      = p_msgno.
  tg_msg-texto      = p_texto.
  tg_msg-tabix      = p_tabix.

  CASE tg_msg-type.
    WHEN 'E'.
      tg_msg-type_ic = icon_led_red.
    WHEN 'W'.
      tg_msg-type_ic = icon_led_yellow.
    WHEN 'S'.
      tg_msg-type_ic = icon_led_green.
  ENDCASE.

  APPEND tg_msg.

ENDFORM.


FORM f_processa_recepcao USING p_gravar_registro       TYPE c
                      CHANGING p_zlest0146             TYPE zlest0146
                               p_zlest0147             TYPE zlest0147
                               p_zlest0168             TYPE zlest0168_t
                               p_retorno_proc          TYPE zde_retorno_proc.

  DATA: v_msg  TYPE string.

  CLEAR: p_retorno_proc.

*  DATA: WL_SET_CONS_NFE_VAL_NF   TYPE SETLEAF,
*        WL_SET_CONS_NFE_VAL_URF  TYPE SETLEAF,
*        WL_SET_CONS_NFE_VAL_RA   TYPE SETLEAF.
*
*  CLEAR: WL_SET_CONS_NFE_VAL_NF, WL_SET_CONS_NFE_VAL_URF, WL_SET_CONS_NFE_VAL_RA.
*
*  IF ( P_ZLEST0147-MODEL = '55' ).
*
*    SELECT SINGLE *
*      FROM SETLEAF INTO WL_SET_CONS_NFE_VAL_NF
*     WHERE SETNAME EQ 'ZLES0164_CONS_NFE'
*       AND VALFROM EQ 'VAL_NFE'.
*
*    SELECT SINGLE *
*      FROM SETLEAF INTO WL_SET_CONS_NFE_VAL_URF
*     WHERE SETNAME EQ 'ZLES0164_CONS_NFE'
*       AND VALFROM EQ 'VAL_URF'.
*
*    SELECT SINGLE *
*      FROM SETLEAF INTO WL_SET_CONS_NFE_VAL_RA
*     WHERE SETNAME EQ 'ZLES0164_CONS_NFE'
*       AND VALFROM EQ 'VAL_RA'.
*
*    SELECT SINGLE *
*      FROM ZLEST0186 INTO @DATA(_WL_ZLEST0168)
*     WHERE CHAVE EQ @P_ZLEST0147-CHAVE_NFE.
*
*    IF ( SY-SUBRC NE 0 ) AND ( WL_SET_CONS_NFE_VAL_NF-VALFROM IS NOT INITIAL ).
*      MESSAGE S136 WITH P_ZLEST0147-CHAVE_NFE INTO V_MSG.
*      P_RETORNO_PROC-TYPE     = 'E'.
*      P_RETORNO_PROC-MSGNO    = SY-MSGNO.
*      P_RETORNO_PROC-TEXTO    = V_MSG.
*      RETURN.
*    ENDIF.
*
*    IF ( P_ZLEST0146-LOCAL_CODIGO_URF <> _WL_ZLEST0168-CODIGO_URF ) AND ( WL_SET_CONS_NFE_VAL_URF-VALFROM IS NOT INITIAL ).
*      MESSAGE S137 WITH P_ZLEST0147-CHAVE_NFE  _WL_ZLEST0168-CODIGO_URF P_ZLEST0146-LOCAL_CODIGO_URF INTO V_MSG.
*      P_RETORNO_PROC-TYPE     = 'E'.
*      P_RETORNO_PROC-MSGNO    = SY-MSGNO.
*      P_RETORNO_PROC-TEXTO    = V_MSG.
*      RETURN.
*    ENDIF.
*
*    IF ( P_ZLEST0146-LOCAL_CODIGO_RA <> _WL_ZLEST0168-CODIGO_RA ) AND ( WL_SET_CONS_NFE_VAL_RA-VALFROM IS NOT INITIAL ).
*      MESSAGE S138 WITH P_ZLEST0147-CHAVE_NFE  _WL_ZLEST0168-CODIGO_RA P_ZLEST0146-LOCAL_CODIGO_RA INTO V_MSG.
*      P_RETORNO_PROC-TYPE     = 'E'.
*      P_RETORNO_PROC-MSGNO    = SY-MSGNO.
*      P_RETORNO_PROC-TEXTO    = V_MSG.
*      RETURN.
*    ENDIF.
*
**    IF ( _WL_ZLEST0168-DT_RECEPCAO IS NOT INITIAL ) AND ( _WL_ZLEST0168-DT_RECEPCAO NE P_ZLEST0146-DT_RECEPCAO ).
**      P_ZLEST0146-DT_RECEPCAO = _WL_ZLEST0168-DT_RECEPCAO.
**    ENDIF.
*
*  ENDIF.


  CALL FUNCTION 'ZCCT_PROC_RECEPCAO_CARGA'
    EXPORTING
      i_gravar_registro  = p_gravar_registro
      i_recinto_terceiro = abap_true
    CHANGING
      c_zlest0146        = p_zlest0146
      c_zlest0147        = p_zlest0147
      c_zlest0168        = p_zlest0168
      c_retorno          = p_retorno_proc.

ENDFORM.

FORM f_log_detalhamento  TABLES p_msg STRUCTURE tg_msg
                         USING  p_type
                                p_desc.

  tg_msg_aux[] = p_msg[].
  DELETE tg_msg_aux WHERE type NE p_type.

  CHECK lines( tg_msg_aux[] ) NE 0.

  WRITE: '| Registros com ', p_desc , ' |'.

  SORT tg_msg_aux BY filename.
  DATA(_cab) = abap_false.
  LOOP AT tg_msg_aux.
    IF _cab EQ abap_false.
      WRITE: 'Arquivo: ' , tg_msg_aux-filename.
      _cab = abap_true.
      WRITE: /.
    ENDIF.

    WRITE: ' Nr.Msg: ' , tg_msg_aux-msgno , 'Texto: ' , tg_msg_aux-texto , ' Linha: ',  tg_msg_aux-tabix.
    WRITE: /.
  ENDLOOP.

ENDFORM.

FORM f_cancel_recepcao  USING p_id_recepcao TYPE zlest0146-id_recepcao
                     CHANGING c_cancelada.

  c_cancelada = abap_false.

  SELECT SINGLE *
    FROM zlest0146 INTO @DATA(wl_0146)
   WHERE id_recepcao EQ @p_id_recepcao
     AND importado   EQ @abap_true.

  IF ( sy-subrc NE 0 ) OR (  p_id_recepcao IS INITIAL ).
    MESSAGE s015 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*  SELECT SINGLE *
*    FROM ZLEST0147 INTO @DATA(WL_0147)
*   WHERE ID_RECEPCAO EQ @WL_0146-ID_RECEPCAO.
*
*  IF SY-SUBRC EQ 0.
*    SELECT SINGLE *
*      FROM ZSDT_RETLOTE INTO @DATA(_WL_RETLOTE)
*     WHERE DOCNUM = @WL_0147-DOCNUM.
*
*    IF SY-SUBRC EQ 0.
*      MESSAGE S150 WITH _WL_RETLOTE-DOCNUM_RET DISPLAY LIKE 'S'.
*      EXIT.
*    ENDIF.
*  ENDIF.

  wl_0146-cancel    = 'X'.
  wl_0146-dt_cancel = sy-datum.
  wl_0146-hr_cancel = sy-uzeit.
  wl_0146-us_cancel = sy-uname.

  MODIFY zlest0146 FROM wl_0146.

  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE s017 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zlest0147 INTO @DATA(wl_0147)
   WHERE id_recepcao EQ @wl_0146-id_recepcao.

  IF sy-subrc IS INITIAL AND
     wl_0147-chave_nfe IS NOT INITIAL.
    DELETE FROM zlest0186 WHERE chave EQ wl_0147-chave_nfe.
  ENDIF.

  DELETE FROM zsdt0264 WHERE id_recepcao EQ p_id_recepcao.

  c_cancelada = abap_true.

  MESSAGE s016 DISPLAY LIKE 'S'.

ENDFORM.

FORM f_log_processamento .

  CHECK tg_msg[] IS NOT INITIAL.

  tg_msg_out[] = tg_msg[].

  SORT tg_msg BY filename type tabix.

  PERFORM f_montar_layout_log_proc.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 140
      i_screen_end_line     = 25
    TABLES
      t_outtab              = tg_msg_out.


ENDFORM.


FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit).

  CLEAR: wa_estrutura.

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

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  IF wa_estrutura-fieldname EQ 'TYPE_IC'.
    wa_estrutura-just = 'C'.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM f_montar_layout_log_cancel.
  REFRESH estrutura.
  PERFORM f_montar_estrutura USING:
     01  ''   ''            'TG_0146_CANCEL' 'ID_RECEPCAO'     'Id.Rec.'          '10' '',
     02  ''   ''            'TG_0146_CANCEL' 'DT_RECEPCAO'     'Dt.Recepção'      '11' '',
     05  ''   ''            'TG_0146_CANCEL' 'DT_CANCEL'       'Dt.Cancel'        '10' '',
     06  ''   ''            'TG_0146_CANCEL' 'HR_CANCEL'       'Hr.Cancel'        '10' '',
     07  ''   ''            'TG_0146_CANCEL' 'US_CANCEL'       'Us.Cancel'        '13' '',
     08  ''   ''            'TG_0146_CANCEL' 'CHAVE_NFE'       'Chave NF-e'       '45' '',
     08  ''   ''            'TG_0146_CANCEL' 'CHAVE_NFF'       'Chave NF-f'       '45' '',
     09  ''   ''            'TG_0146_CANCEL' 'EMISSOR_CNPJ'    'CNPJ Fornecedor'  '16' '',
     09  ''   ''            'TG_0146_CANCEL' 'EMISSOR_CPF'     'CPF Fornecedor'   '16' '',
     09  ''   ''            'TG_0146_CANCEL' 'EMISSOR_IE'      'IE Fornecedor'    '16' '',
     10  ''   ''            'TG_0146_CANCEL' 'NFNUM9'          'Núm. NF'          '10' '',
     11  ''   ''            'TG_0146_CANCEL' 'DT_EMISSAO'      'Dt.Emissão'       '11' ''.

ENDFORM.                    " MONTAR_LAYOUT

FORM f_montar_layout_log_proc.
  REFRESH estrutura.
  PERFORM f_montar_estrutura USING:
     01  ''   ''            'TG_MSG_OUT' 'FILENAME'    'Arquivo'       '10' '',
     02  ''   ''            'TG_MSG_OUT' 'TYPE_IC'     'Tp.Msg.'       '07' '',
     05  ''   ''            'TG_MSG_OUT' 'MSGNO'       'Núm.Msg.'      '08' '',
     06  ''   ''            'TG_MSG_OUT' 'TEXTO'       'Texto'         '100' '',
     07  ''   ''            'TG_MSG_OUT' 'TABIX'       'Linha'         '05' ''.

ENDFORM.
