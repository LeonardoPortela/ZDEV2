REPORT zgos_localiza_anexos.
TYPE-POOLS: icon, slis, sofolenti1.

*&---------------------------------------------------------------------*
*& SELECTION SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_user TYPE soffphio-crea_user.
SELECT-OPTIONS: s_date FOR sy-datum.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_limit TYPE i DEFAULT 1000.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_delmod TYPE c RADIOBUTTON GROUP del.
PARAMETERS: p_delsc1 TYPE c RADIOBUTTON GROUP del.
PARAMETERS: p_delall TYPE c RADIOBUTTON GROUP del.
SELECTION-SCREEN END OF BLOCK b3.

*&---------------------------------------------------------------------*
*& TYPE DEFINITIONS
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_output,
         status_link         TYPE char20,
         missing_tables      TYPE string,
         delete_status       TYPE string,
         origem_tabela       TYPE char20,
         soffcont1_phio_id   TYPE soffcont1-phio_id,
         soffcont1_clustr    TYPE soffcont1-clustr,
         soffcont1_doc_size  TYPE i,
         soffphio_phio_id    TYPE soffphio-phio_id,
         soffphio_loio_id    TYPE soffphio-loio_id,
         soffphio_crea_time  TYPE soffphio-crea_time,
         soffphf_mimetype    TYPE soffphf-mimetype,
         soffphf_file_name   TYPE soffphf-file_name,
         soffphf_file_size   TYPE p LENGTH 15 DECIMALS 2,
         soc3n_objid         TYPE soc3n-objid,
         soc3n_filename      TYPE soc3n-filename,
         soc3n_objtp         TYPE soc3n-objtp,
         soc3n_objyr         TYPE soc3n-objyr,
         soc3n_objno         TYPE soc3n-objno,
         sood_objdes         TYPE sood-objdes,
         sood_ownnam         TYPE sood-ownnam,
         sood_extct          TYPE sood-extct,
         sood_objtp          TYPE sood-objtp,
         sood_objyr          TYPE sood-objyr,
         sood_objno          TYPE sood-objno,
         srgbtbrel_instid_a  TYPE srgbtbrel-instid_a,
         srgbtbrel_instid_b  TYPE srgbtbrel-instid_b,
         srgbtbrel_reltype   TYPE srgbtbrel-reltype,
         srgbtbrel_typeid_a  TYPE srgbtbrel-typeid_a,
         srgbtbrel_typeid_b  TYPE srgbtbrel-typeid_b,
       END OF ty_output.

*&---------------------------------------------------------------------*
*& DATA DEFINITIONS
*&---------------------------------------------------------------------*
DATA: gt_output   TYPE STANDARD TABLE OF ty_output,
      gs_output   TYPE ty_output,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_user IS INITIAL AND s_date[] IS INITIAL.
    MESSAGE 'Informe pelo menos um filtro: Usuário ou Data.' TYPE 'E'.
  ENDIF.
  IF p_limit <= 0.
    MESSAGE 'Limite deve ser maior que zero.' TYPE 'E'.
  ENDIF.

  IF p_limit > 10000.
    MESSAGE 'Limite máximo permitido: 10.000 registros.' TYPE 'E'.
  ENDIF.

  PERFORM soffphio_search.

  IF gt_output IS INITIAL.
    MESSAGE 'Nenhum registro encontrado para os filtros informados.' TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM process_mass_deletion.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  PERFORM build_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'HANDLE_USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
    TABLES
      t_outtab                 = gt_output
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM build_fieldcat.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  DATA: l_col TYPE i VALUE 1.

  DEFINE add_field.
    CLEAR ls_fieldcat.
    ls_fieldcat-col_pos = l_col.
    ls_fieldcat-fieldname = &1.
    ls_fieldcat-seltext_l = &2.
    ls_fieldcat-seltext_m = &3.
    ls_fieldcat-seltext_s = &4.
    APPEND ls_fieldcat TO gt_fieldcat.
    ADD 1 TO l_col.
  END-OF-DEFINITION.

  add_field 'STATUS_LINK'         'Status da Ligação' 'Status Ligação' 'Status'.
  add_field 'MISSING_TABLES'      'Tabelas Faltantes' 'Tabelas Fal.' 'Faltantes'.
  add_field 'DELETE_STATUS'       'Status da Exclusão' 'Status Exclusão' 'Status Excl.'.
  add_field 'SOFFCONT1_PHIO_ID'   'SOFFCONT1-PHIO_ID' 'SOFFCONT1-PHIO_ID' 'SOFFCONT1-PHIO_ID'.
  add_field 'SOFFCONT1_CLUSTR'    'SOFFCONT1-CLUSTR' 'SOFFCONT1-CLUSTR' 'SOFFCONT1-CLUSTR'.
  add_field 'SOFFCONT1_DOC_SIZE'  'SOFFCONT1-DOC_SIZE' 'SOFFCONT1-SIZE' 'SOFFCONT1-SIZE'.
  add_field 'SOFFPHIO_PHIO_ID'    'SOFFPHIO-PHIO_ID' 'SOFFPHIO-PHIO_ID' 'SOFFPHIO-PHIO_ID'.
  add_field 'SOFFPHIO_LOIO_ID'    'SOFFPHIO-LOIO_ID' 'SOFFPHIO-LOIO_ID' 'SOFFPHIO-LOIO_ID'.
  add_field 'SOFFPHIO_CREA_TIME'  'SOFFPHIO-CREA_TIME' 'SOFFPHIO-CREA_TIME' 'SOFFPHIO-CREA_TIME'.
  add_field 'SOFFPHF_FILE_NAME'   'SOFFPHF-FILE_NAME' 'SOFFPHF-FILENAME' 'SOFFPHF-FILENAME'.
  add_field 'SOFFPHF_MIMETYPE'    'SOFFPHF-MIMETYPE' 'SOFFPHF-MIMETYPE' 'SOFFPHF-MIMETYPE'.
  add_field 'SOFFPHF_FILE_SIZE'   'SOFFPHF-FILE_SIZE (MB)' 'SOFFPHF-SIZE(MB)' 'SOFFPHF-SIZE(MB)'.
  add_field 'SOC3N_OBJID'         'SOC3N-OBJID' 'SOC3N-OBJID' 'SOC3N-OBJID'.
  add_field 'SOC3N_OBJTP'         'SOC3N-OBJTP' 'SOC3N-OBJTP' 'SOC3N-OBJTP'.
  add_field 'SOC3N_OBJYR'         'SOC3N-OBJYR' 'SOC3N-OBJYR' 'SOC3N-OBJYR'.
  add_field 'SOC3N_OBJNO'         'SOC3N-OBJNO' 'SOC3N-OBJNO' 'SOC3N-OBJNO'.
  add_field 'SOOD_OBJDES'         'SOOD-OBJDES' 'SOOD-OBJDES' 'SOOD-OBJDES'.
  add_field 'SOOD_EXTCT'          'SOOD-EXTCT' 'SOOD-EXTCT' 'SOOD-EXTCT'.
  add_field 'SOOD_OBJTP'          'SOOD-OBJTP' 'SOOD-OBJTP' 'SOOD-OBJTP'.
  add_field 'SOOD_OBJYR'          'SOOD-OBJYR' 'SOOD-OBJYR' 'SOOD-OBJYR'.
  add_field 'SOOD_OBJNO'          'SOOD-OBJNO' 'SOOD-OBJNO' 'SOOD-OBJNO'.
  add_field 'SRGBTBREL_INSTID_A'  'SRGBTBREL-INSTID_A (Obj. Origem)' 'SRGBTBREL-INSTID_A' 'SRGBTBREL-INSTID_A'.
  add_field 'SRGBTBREL_TYPEID_A'  'SRGBTBREL-TYPEID_A (Tipo Obj.)' 'SRGBTBREL-TYPEID_A' 'SRGBTBREL-TYPEID_A'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'Z_ALV_STATUS'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM handle_user_command USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.
  READ TABLE gt_output INTO gs_output INDEX rs_selfield-tabindex.
  IF sy-subrc <> 0. RETURN. ENDIF.

  CASE r_ucomm.
    WHEN 'DOWNLOAD'.
      PERFORM download_attachment USING gs_output.
    WHEN 'DELETE'.
      PERFORM handle_delete_button USING gs_output.
      IF sy-subrc = 0.
        DELETE gt_output INDEX rs_selfield-tabindex.
        rs_selfield-refresh = 'X'.
      ENDIF.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DELETE_BUTTON
*&---------------------------------------------------------------------*
FORM handle_delete_button USING us_selection TYPE ty_output.
  DATA lv_answer TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar      = 'Confirmar Exclusão'
      text_question = 'Qual o escopo da exclusão?'
      text_button_1 = 'Apenas esta linha (SOFFCONT1)'
      text_button_2 = 'Anexo Completo (Todas as Tabelas)'
    IMPORTING
      answer        = lv_answer
    EXCEPTIONS
      OTHERS        = 1.
  IF sy-subrc <> 0 OR lv_answer = 'A'.
    MESSAGE 'Exclusão cancelada.' TYPE 'I'.
    sy-subrc = 4.
    RETURN.
  ENDIF.

  DATA lv_success TYPE abap_bool.
  PERFORM perform_deletion USING us_selection lv_answer abap_false CHANGING lv_success.

  IF lv_success = abap_true.
    COMMIT WORK AND WAIT.
    MESSAGE 'Exclusão realizada com sucesso.' TYPE 'S'.
    sy-subrc = 0.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Falha na exclusão.' TYPE 'E'.
    sy-subrc = 4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DOWNLOAD_ATTACHMENT
*&---------------------------------------------------------------------*
FORM download_attachment USING us_selection TYPE ty_output.
  DATA: lt_binary_tab TYPE solix_tab,
        lv_doc_size   TYPE i,
        lv_tempdir    TYPE string,
        lv_fullpath   TYPE string,
        ls_sood       TYPE sood.

  ls_sood-objtp = us_selection-sood_objtp.
  ls_sood-objyr = us_selection-sood_objyr.
  ls_sood-objno = us_selection-sood_objno.

  CALL FUNCTION 'SO_DOCUMENT_READ_API1'
    EXPORTING
      document_data  = ls_sood
    IMPORTING
      document_size  = lv_doc_size
    TABLES
      contents_hex   = lt_binary_tab
    EXCEPTIONS
      document_id_not_exist    = 1
      operation_no_authorization = 2
      OTHERS                   = 4.

  IF sy-subrc <> 0 OR lt_binary_tab IS INITIAL.
    MESSAGE 'Erro ao ler o anexo via chave SOOD.' TYPE 'E'.
    RETURN.
  ENDIF.

  cl_gui_frontend_services=>get_temp_directory(
    CHANGING temp_dir = lv_tempdir
    EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0. lv_tempdir = 'C:\TEMP\'. ENDIF.

  CONCATENATE lv_tempdir us_selection-soffphf_file_name INTO lv_fullpath.

  cl_gui_frontend_services=>gui_download(
    EXPORTING
      bin_filesize = lv_doc_size
      filename     = lv_fullpath
      filetype     = 'BIN'
    CHANGING
      data_tab     = lt_binary_tab
    EXCEPTIONS OTHERS = 1 ).

  IF sy-subrc = 0.
    cl_gui_frontend_services=>execute(
      EXPORTING document = lv_fullpath
      EXCEPTIONS OTHERS = 1 ).
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PERFORM_DELETION
*&---------------------------------------------------------------------*
FORM perform_deletion USING is_selection    TYPE ty_output
                            iv_delete_mode  TYPE c
                            iv_test_run     TYPE abap_bool
                      CHANGING cv_success   TYPE abap_bool.
  cv_success = abap_false.
  IF iv_test_run = abap_true.
    cv_success = abap_true.
    RETURN.
  ENDIF.

  CASE iv_delete_mode.
    WHEN '1'.
      DELETE FROM soffcont1 WHERE phio_id = is_selection-soffcont1_phio_id AND clustr = is_selection-soffcont1_clustr.
    WHEN '2'.
      DELETE FROM srgbtbrel WHERE instid_b = is_selection-srgbtbrel_instid_b.
      DELETE FROM sood WHERE objtp = is_selection-sood_objtp AND objyr = is_selection-sood_objyr AND objno = is_selection-sood_objno.
      DELETE FROM soc3n WHERE objtp = is_selection-soc3n_objtp AND objyr = is_selection-soc3n_objyr AND objno = is_selection-soc3n_objno.
      DELETE FROM soffphf WHERE phio_id = is_selection-soffphio_phio_id.
      DELETE FROM soffphio WHERE phio_id = is_selection-soffphio_phio_id.
      DELETE FROM soffcont1 WHERE phio_id = is_selection-soffcont1_phio_id.
  ENDCASE.

  IF sy-subrc = 0.
    cv_success = abap_true.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_MASS_DELETION
*&---------------------------------------------------------------------*
FORM process_mass_deletion.
  IF p_delsc1 = abap_false AND p_delall = abap_false.
    RETURN.
  ENDIF.

  DATA: lv_error_flag   TYPE abap_bool,
        lv_delete_count TYPE i,
        lv_success      TYPE abap_bool.

  IF p_delall = abap_true.
    DATA lt_unique_phio TYPE TABLE OF ty_output.
    lt_unique_phio = gt_output.
    SORT lt_unique_phio BY soffphio_phio_id.
    DELETE ADJACENT DUPLICATES FROM lt_unique_phio COMPARING soffphio_phio_id.

    LOOP AT lt_unique_phio ASSIGNING FIELD-SYMBOL(<fs_unique_phio>).
      IF lv_delete_count >= p_limit.
        MESSAGE |Limite de processamento ({ p_limit }) atingido.| TYPE 'W'.
        EXIT.
      ENDIF.
      lv_delete_count = lv_delete_count + 1.
      PERFORM perform_deletion USING <fs_unique_phio> '2' p_test CHANGING lv_success.
      IF lv_success = abap_false. lv_error_flag = abap_true. ENDIF.
      LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs_line>) WHERE soffphio_phio_id = <fs_unique_phio>-soffphio_phio_id.
        <fs_line>-delete_status = COND #( WHEN lv_success = abap_false THEN 'ERRO na exclusão'
                                          WHEN p_test = abap_true THEN 'TESTE - Exclusão completa simulada'
                                          ELSE 'EXCLUÍDO - Vínculos removidos' ).
      ENDLOOP.
    ENDLOOP.
  ELSEIF p_delsc1 = abap_true.
    LOOP AT gt_output ASSIGNING <fs_line>.
      IF lv_delete_count >= p_limit.
        MESSAGE |Limite de processamento ({ p_limit }) atingido.| TYPE 'W'.
        EXIT.
      ENDIF.
      lv_delete_count = lv_delete_count + 1.
      PERFORM perform_deletion USING <fs_line> '1' p_test CHANGING lv_success.
      IF lv_success = abap_false. lv_error_flag = abap_true. ENDIF.
      <fs_line>-delete_status = COND #( WHEN lv_success = abap_false THEN 'ERRO na exclusão'
                                        WHEN p_test = abap_true THEN 'TESTE - Exclusão SOFFCONT1 simulada'
                                        ELSE 'EXCLUÍDO - SOFFCONT1' ).
    ENDLOOP.
  ENDIF.

  IF p_test = abap_false.
    IF lv_error_flag = abap_true.
      ROLLBACK WORK.
      MESSAGE 'Ocorreram erros. Nenhuma exclusão foi efetivada.' TYPE 'E'.
    ELSE.
      COMMIT WORK AND WAIT.
      DELETE gt_output WHERE delete_status IS NOT INITIAL AND delete_status <> 'ERRO na exclusão'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SOFFPHIO_SEARCH
*&---------------------------------------------------------------------*
FORM soffphio_search.
  PERFORM search_in_soffphio.
  PERFORM search_in_srgbtbrel.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SEARCH_IN_SOFFPHIO
*&---------------------------------------------------------------------*
FORM search_in_soffphio.
  DATA: lt_soffphio TYPE TABLE OF soffphio,
        ls_soffphio TYPE soffphio,
        ls_date_range LIKE LINE OF s_date,
        lv_timestamp_from TYPE string,
        lv_timestamp_to TYPE string.

  IF s_date[] IS NOT INITIAL.
    LOOP AT s_date INTO ls_date_range.
      IF ls_date_range-high IS INITIAL.
        ls_date_range-high = ls_date_range-low.
      ENDIF.

      CONCATENATE ls_date_range-low '000000' INTO lv_timestamp_from.
      CONCATENATE ls_date_range-high '235959' INTO lv_timestamp_to.

      IF p_user IS NOT INITIAL.
        SELECT * FROM soffphio APPENDING TABLE lt_soffphio
          UP TO p_limit ROWS
          WHERE crea_user = p_user
            AND crea_time BETWEEN lv_timestamp_from AND lv_timestamp_to.
      ELSE.
        SELECT * FROM soffphio APPENDING TABLE lt_soffphio
          UP TO p_limit ROWS
          WHERE crea_time BETWEEN lv_timestamp_from AND lv_timestamp_to.
      ENDIF.
    ENDLOOP.
  ELSEIF p_user IS NOT INITIAL.
    SELECT * FROM soffphio INTO TABLE lt_soffphio
      UP TO p_limit ROWS
      WHERE crea_user = p_user.
  ENDIF.

  LOOP AT lt_soffphio INTO ls_soffphio.
    PERFORM trace_from_soffphio_multi USING ls_soffphio 'SOFFPHIO'.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SEARCH_IN_SRGBTBREL
*&---------------------------------------------------------------------*
FORM search_in_srgbtbrel.
  DATA: lt_srgbtbrel TYPE TABLE OF srgbtbrel,
        ls_srgbtbrel TYPE srgbtbrel.

  IF s_date[] IS NOT INITIAL.
    SELECT * FROM srgbtbrel INTO TABLE lt_srgbtbrel
      UP TO p_limit ROWS
      WHERE reltype = 'ATTA'
        AND utctime IN s_date.
    LOOP AT lt_srgbtbrel INTO ls_srgbtbrel.
      PERFORM trace_from_srgbtbrel_origin USING ls_srgbtbrel.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form TRACE_FROM_SOFFPHIO_MULTI
*&---------------------------------------------------------------------*
FORM trace_from_soffphio_multi USING p_soffphio TYPE soffphio
                                     p_origem TYPE char20.
  DATA: lt_soffcont1_all TYPE TABLE OF soffcont1,
        ls_soffcont1_item TYPE soffcont1,
        ls_soffphf TYPE soffphf,
        ls_soc3n TYPE soc3n,
        ls_sood TYPE sood,
        ls_srgbtbrel TYPE srgbtbrel.

  SELECT * FROM soffcont1 INTO TABLE lt_soffcont1_all
    WHERE phio_id = p_soffphio-phio_id
    ORDER BY clustr.

  SELECT SINGLE * FROM soffphf INTO ls_soffphf
    WHERE phio_id = p_soffphio-phio_id.

  SELECT SINGLE * FROM soc3n INTO ls_soc3n
    WHERE objid = p_soffphio-loio_id.

  IF sy-subrc = 0.
    SELECT SINGLE * FROM sood INTO ls_sood
      WHERE objtp = ls_soc3n-objtp
        AND objyr = ls_soc3n-objyr
        AND objno = ls_soc3n-objno.

    IF sy-subrc = 0.
      DATA(lv_instid_b) = |FOL47000000000004{ ls_sood-objtp }{ ls_sood-objyr }{ ls_sood-objno }|.
      SELECT SINGLE * FROM srgbtbrel INTO ls_srgbtbrel
        WHERE instid_b = lv_instid_b
          AND reltype = 'ATTA'.
    ENDIF.
  ENDIF.

  IF lt_soffcont1_all IS NOT INITIAL.
    LOOP AT lt_soffcont1_all INTO ls_soffcont1_item.
      CLEAR gs_output.
      PERFORM fill_output_line USING p_soffphio ls_soffcont1_item ls_soffphf
                                     ls_soc3n ls_sood ls_srgbtbrel p_origem.
      APPEND gs_output TO gt_output.
    ENDLOOP.
  ELSE.
    CLEAR gs_output.
    PERFORM fill_output_line_no_soffcont1 USING p_soffphio ls_soffphf
                                                ls_soc3n ls_sood ls_srgbtbrel p_origem.
    APPEND gs_output TO gt_output.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FILL_OUTPUT_LINE
*&---------------------------------------------------------------------*
FORM fill_output_line USING p_soffphio TYPE soffphio
                            p_soffcont1 TYPE soffcont1
                            p_soffphf TYPE soffphf
                            p_soc3n TYPE soc3n
                            p_sood TYPE sood
                            p_srgbtbrel TYPE srgbtbrel
                            p_origem TYPE char20.
  DATA lv_missing TYPE string.
  gs_output-status_link = 'COMPLETA'.
  gs_output-origem_tabela = p_origem.

  gs_output-soffphio_phio_id = p_soffphio-phio_id.
  gs_output-soffphio_loio_id = p_soffphio-loio_id.
  gs_output-soffphio_crea_time = p_soffphio-crea_time.
  gs_output-soffcont1_phio_id = p_soffcont1-phio_id.
  gs_output-soffcont1_clustr = p_soffcont1-clustr.
  IF p_soffcont1-clustd IS NOT INITIAL.
    gs_output-soffcont1_doc_size = xstrlen( p_soffcont1-clustd ).
  ELSE.
    gs_output-soffcont1_doc_size = 0.
    gs_output-status_link = 'SEM_DADOS'.
    CONCATENATE lv_missing 'CLUSTD_VAZIO;' INTO lv_missing.
  ENDIF.
  IF p_soffphf-phio_id IS NOT INITIAL.
    gs_output-soffphf_mimetype = p_soffphf-mimetype.
    gs_output-soffphf_file_name = p_soffphf-file_name.
    gs_output-soffphf_file_size = p_soffphf-file_size / 1024 / 1024.
  ENDIF.
  IF p_soc3n-objid IS NOT INITIAL.
    gs_output-soc3n_objid = p_soc3n-objid.
    gs_output-soc3n_filename = p_soc3n-filename.
    gs_output-soc3n_objtp = p_soc3n-objtp.
    gs_output-soc3n_objyr = p_soc3n-objyr.
    gs_output-soc3n_objno = p_soc3n-objno.
  ELSE.
    CONCATENATE lv_missing 'SOC3N;' INTO lv_missing.
    gs_output-status_link = 'INCONSISTENTE'.
  ENDIF.
  IF p_sood-objtp IS NOT INITIAL.
    gs_output-sood_objdes = p_sood-objdes.
    gs_output-sood_ownnam = p_sood-ownnam.
    gs_output-sood_extct = p_sood-extct.
    gs_output-sood_objtp = p_sood-objtp.
    gs_output-sood_objyr = p_sood-objyr.
    gs_output-sood_objno = p_sood-objno.
  ELSE.
    CONCATENATE lv_missing 'SOOD;' INTO lv_missing.
    IF gs_output-status_link = 'COMPLETA'.
      gs_output-status_link = 'INCONSISTENTE'.
    ENDIF.
  ENDIF.
  IF p_srgbtbrel-instid_a IS NOT INITIAL.
    gs_output-srgbtbrel_instid_a = p_srgbtbrel-instid_a.
    gs_output-srgbtbrel_instid_b = p_srgbtbrel-instid_b.
    gs_output-srgbtbrel_reltype = p_srgbtbrel-reltype.
    gs_output-srgbtbrel_typeid_a = p_srgbtbrel-typeid_a.
    gs_output-srgbtbrel_typeid_b = p_srgbtbrel-typeid_b.
  ELSE.
    CONCATENATE lv_missing 'SRGBTBREL;' INTO lv_missing.
    IF gs_output-status_link = 'COMPLETA'.
      gs_output-status_link = 'ÓRFÃO'.
    ENDIF.
  ENDIF.
  gs_output-missing_tables = lv_missing.
ENDFORM.

*&---------------------------------------------------------------------*
*& Outras sub-rotinas (fill_output_line_no_soffcont1, trace_from_srgbtbrel_origin, etc.)
*&---------------------------------------------------------------------*
FORM fill_output_line_no_soffcont1 USING p_soffphio TYPE soffphio p_soffphf TYPE soffphf p_soc3n TYPE soc3n p_sood TYPE sood p_srgbtbrel TYPE srgbtbrel p_origem TYPE char20.
  DATA lv_missing TYPE string.
  gs_output-status_link = 'QUEBRA_CRÍTICA'.
  gs_output-origem_tabela = p_origem.
  lv_missing = 'SOFFCONT1_INEXISTENTE;'.
  gs_output-soffphio_phio_id = p_soffphio-phio_id.
  gs_output-soffphio_loio_id = p_soffphio-loio_id.
  gs_output-soffphio_crea_time = p_soffphio-crea_time.
  gs_output-soffcont1_doc_size = 0.
  IF p_soffphf-phio_id IS NOT INITIAL.
    gs_output-soffphf_mimetype = p_soffphf-mimetype.
    gs_output-soffphf_file_name = p_soffphf-file_name.
    gs_output-soffphf_file_size = p_soffphf-file_size / 1024 / 1024.
  ENDIF.
  IF p_soc3n-objid IS NOT INITIAL.
    gs_output-soc3n_objid = p_soc3n-objid.
    gs_output-soc3n_filename = p_soc3n-filename.
    gs_output-soc3n_objtp = p_soc3n-objtp.
    gs_output-soc3n_objyr = p_soc3n-objyr.
    gs_output-soc3n_objno = p_soc3n-objno.
  ENDIF.
  IF p_sood-objtp IS NOT INITIAL.
    gs_output-sood_objdes = p_sood-objdes.
    gs_output-sood_ownnam = p_sood-ownnam.
    gs_output-sood_extct = p_sood-extct.
    gs_output-sood_objtp = p_sood-objtp.
    gs_output-sood_objyr = p_sood-objyr.
    gs_output-sood_objno = p_sood-objno.
  ENDIF.
  IF p_srgbtbrel-instid_a IS NOT INITIAL.
    gs_output-srgbtbrel_instid_a = p_srgbtbrel-instid_a.
    gs_output-srgbtbrel_instid_b = p_srgbtbrel-instid_b.
    gs_output-srgbtbrel_reltype = p_srgbtbrel-reltype.
    gs_output-srgbtbrel_typeid_a = p_srgbtbrel-typeid_a.
    gs_output-srgbtbrel_typeid_b = p_srgbtbrel-typeid_b.
  ENDIF.
  gs_output-missing_tables = lv_missing.
ENDFORM.

FORM trace_from_srgbtbrel_origin USING p_srgbtbrel TYPE srgbtbrel.
  CLEAR gs_output.
  gs_output-origem_tabela = 'SRGBTBREL'.
  gs_output-status_link = 'COMPLETA'.
  gs_output-srgbtbrel_instid_a = p_srgbtbrel-instid_a.
  gs_output-srgbtbrel_instid_b = p_srgbtbrel-instid_b.
  gs_output-srgbtbrel_reltype = p_srgbtbrel-reltype.
  gs_output-srgbtbrel_typeid_a = p_srgbtbrel-typeid_a.
  gs_output-srgbtbrel_typeid_b = p_srgbtbrel-typeid_b.
  PERFORM trace_from_srgbtbrel USING p_srgbtbrel.
  APPEND gs_output TO gt_output.
ENDFORM.

FORM trace_from_srgbtbrel USING p_srgbtbrel TYPE srgbtbrel.
  DATA: ls_sood TYPE sood, ls_soc3n TYPE soc3n, ls_soffphio TYPE soffphio, ls_soffcont1 TYPE soffcont1, ls_soffphf TYPE soffphf,
        lv_objtp TYPE sood-objtp, lv_objyr TYPE sood-objyr, lv_objno TYPE sood-objno, lv_missing TYPE string.
  gs_output-status_link = 'COMPLETA'. CLEAR lv_missing.
  lv_objtp = p_srgbtbrel-instid_b+17(3). lv_objyr = p_srgbtbrel-instid_b+20(2). lv_objno = p_srgbtbrel-instid_b+22(12).
  SELECT SINGLE * FROM sood INTO ls_sood WHERE objtp = lv_objtp AND objyr = lv_objyr AND objno = lv_objno.
  IF sy-subrc = 0.
    gs_output-sood_objdes = ls_sood-objdes. gs_output-sood_ownnam = ls_sood-ownnam. gs_output-sood_extct = ls_sood-extct.
    gs_output-sood_objtp = ls_sood-objtp. gs_output-sood_objyr = ls_sood-objyr. gs_output-sood_objno = ls_sood-objno.
  ELSE. CONCATENATE lv_missing 'SOOD;' INTO lv_missing. gs_output-status_link = 'PARCIAL'. ENDIF.
  SELECT SINGLE * FROM soc3n INTO ls_soc3n WHERE objtp = lv_objtp AND objyr = lv_objyr AND objno = lv_objno.
  IF sy-subrc = 0.
    gs_output-soc3n_objid = ls_soc3n-objid. gs_output-soc3n_filename = ls_soc3n-filename. gs_output-soc3n_objtp = ls_soc3n-objtp.
    gs_output-soc3n_objyr = ls_soc3n-objyr. gs_output-soc3n_objno = ls_soc3n-objno.
    SELECT SINGLE * FROM soffphio INTO ls_soffphio WHERE loio_id = ls_soc3n-objid.
    IF sy-subrc = 0.
      gs_output-soffphio_phio_id = ls_soffphio-phio_id. gs_output-soffphio_loio_id = ls_soffphio-loio_id. gs_output-soffphio_crea_time = ls_soffphio-crea_time.
      SELECT SINGLE * FROM soffcont1 INTO ls_soffcont1 WHERE phio_id = ls_soffphio-phio_id.
      IF sy-subrc = 0. gs_output-soffcont1_phio_id = ls_soffcont1-phio_id. gs_output-soffcont1_doc_size = xstrlen( ls_soffcont1-clustd ).
      ELSE. CONCATENATE lv_missing 'SOFFCONT1;' INTO lv_missing. gs_output-status_link = 'PARCIAL'. ENDIF.
      gs_output-soffcont1_clustr = ls_soffcont1-clustr.
      SELECT SINGLE * FROM soffphf INTO ls_soffphf WHERE phio_id = ls_soffphio-phio_id.
      IF sy-subrc = 0. gs_output-soffphf_mimetype = ls_soffphf-mimetype. gs_output-soffphf_file_name = ls_soffphf-file_name. gs_output-soffphf_file_size = ls_soffphf-file_size / 1024 / 1024.
      ELSE. CONCATENATE lv_missing 'SOFFPHF;' INTO lv_missing. ENDIF.
    ELSE. CONCATENATE lv_missing 'SOFFPHIO;SOFFCONT1;SOFFPHF;' INTO lv_missing. gs_output-status_link = 'PARCIAL'. ENDIF.
  ELSE. CONCATENATE lv_missing 'SOC3N;SOFFPHIO;SOFFCONT1;SOFFPHF;' INTO lv_missing. gs_output-status_link = 'PARCIAL'. ENDIF.
  gs_output-missing_tables = lv_missing.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CONFIGURE_ALV_COLUMNS
*&---------------------------------------------------------------------*
FORM configure_alv_columns.
  DATA: lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column  TYPE REF TO cl_salv_column.

  lo_columns = go_alv->get_columns( ).
  lo_columns->set_optimize( abap_true ).

  TRY.
      " Colunas de Status
      lo_column = lo_columns->get_column( 'STATUS_LINK' ).
      lo_column->set_long_text( 'Status da Ligação' ).
      lo_column->set_medium_text( 'Status Ligação' ).

      lo_column = lo_columns->get_column( 'MISSING_TABLES' ).
      lo_column->set_long_text( 'Tabelas Faltantes' ).
      lo_column->set_medium_text( 'Tabelas Fal.' ).

      lo_column = lo_columns->get_column( 'DELETE_STATUS' ).
      lo_column->set_long_text( 'Status da Exclusão' ).
      lo_column->set_medium_text( 'Status Exclusão' ).

      " Colunas SOFFCONT1
      lo_column = lo_columns->get_column( 'SOFFCONT1_PHIO_ID' ).
      lo_column->set_long_text( 'SOFFCONT1-PHIO_ID' ).
      lo_column = lo_columns->get_column( 'SOFFCONT1_CLUSTR' ).
      lo_column->set_long_text( 'SOFFCONT1-CLUSTR' ).
      lo_column = lo_columns->get_column( 'SOFFCONT1_DOC_SIZE' ).
      lo_column->set_long_text( 'SOFFCONT1-DOC_SIZE' ).

      " Colunas SOFFPHIO
      lo_column = lo_columns->get_column( 'SOFFPHIO_PHIO_ID' ).
      lo_column->set_long_text( 'SOFFPHIO-PHIO_ID' ).
      lo_column = lo_columns->get_column( 'SOFFPHIO_LOIO_ID' ).
      lo_column->set_long_text( 'SOFFPHIO-LOIO_ID' ).
      lo_column = lo_columns->get_column( 'SOFFPHIO_CREA_TIME' ).
      lo_column->set_long_text( 'SOFFPHIO-CREA_TIME' ).

      " Colunas SOFFPHF
      lo_column = lo_columns->get_column( 'SOFFPHF_FILE_NAME' ).
      lo_column->set_long_text( 'SOFFPHF-FILE_NAME' ).
      lo_column = lo_columns->get_column( 'SOFFPHF_MIMETYPE' ).
      lo_column->set_long_text( 'SOFFPHF-MIMETYPE' ).
      lo_column = lo_columns->get_column( 'SOFFPHF_FILE_SIZE' ).
      lo_column->set_long_text( 'SOFFPHF-FILE_SIZE (MB)' ).

      " Colunas SOC3N
      lo_column = lo_columns->get_column( 'SOC3N_OBJID' ).
      lo_column->set_long_text( 'SOC3N-OBJID' ).
      lo_column = lo_columns->get_column( 'SOC3N_OBJTP' ).
      lo_column->set_long_text( 'SOC3N-OBJTP' ).
      lo_column = lo_columns->get_column( 'SOC3N_OBJYR' ).
      lo_column->set_long_text( 'SOC3N-OBJYR' ).
      lo_column = lo_columns->get_column( 'SOC3N_OBJNO' ).
      lo_column->set_long_text( 'SOC3N-OBJNO' ).

      " Colunas SOOD
      lo_column = lo_columns->get_column( 'SOOD_OBJDES' ).
      lo_column->set_long_text( 'SOOD-OBJDES' ).
      lo_column = lo_columns->get_column( 'SOOD_EXTCT' ).
      lo_column->set_long_text( 'SOOD-EXTCT' ).
      lo_column = lo_columns->get_column( 'SOOD_OBJTP' ).
      lo_column->set_long_text( 'SOOD-OBJTP' ).
      lo_column = lo_columns->get_column( 'SOOD_OBJYR' ).
      lo_column->set_long_text( 'SOOD-OBJYR' ).
      lo_column = lo_columns->get_column( 'SOOD_OBJNO' ).
      lo_column->set_long_text( 'SOOD-OBJNO' ).

      " Colunas SRGBTBREL
      lo_column = lo_columns->get_column( 'SRGBTBREL_INSTID_A' ).
      lo_column->set_long_text( 'SRGBTBREL-INSTID_A (Obj. Origem)' ).
      lo_column->set_medium_text( 'SRGBTBREL-INSTID_A' ).
      lo_column = lo_columns->get_column( 'SRGBTBREL_TYPEID_A' ).
      lo_column->set_long_text( 'SRGBTBREL-TYPEID_A (Tipo Obj.)' ).
      lo_column->set_medium_text( 'SRGBTBREL-TYPEID_A' ).

    CATCH cx_salv_not_found.
      " Coluna não encontrada, tratar erro se necessário
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_MASS_DELETION
*&---------------------------------------------------------------------*
*& Processa a exclusão em massa com base nas opções da tela.
*&---------------------------------------------------------------------*
FORM process_mass_deletion.
  " Só executa se um modo de exclusão foi selecionado
  IF p_delsc1 = abap_false AND p_delall = abap_false.
    RETURN.
  ENDIF.

  DATA: lv_error_flag   TYPE abap_bool,
        lv_delete_count TYPE i.
  DATA(lo_handler_mass) = NEW lcl_event_handler( ).

  IF p_delall = abap_true.
    " Cenário 1: Excluir anexos completos (usando PHIOs únicos)
    DATA: lt_unique_phio TYPE TABLE OF ty_output.
    lt_unique_phio = gt_output.
    SORT lt_unique_phio BY soffphio_phio_id.
    DELETE ADJACENT DUPLICATES FROM lt_unique_phio COMPARING soffphio_phio_id.

    LOOP AT lt_unique_phio ASSIGNING FIELD-SYMBOL(<fs_unique_phio>).
      IF lv_delete_count >= p_limit.
        MESSAGE |Limite de processamento ({ p_limit }) atingido. Processo interrompido.| TYPE 'W'.
        EXIT.
      ENDIF.
      lv_delete_count = lv_delete_count + 1.

      DATA(lv_success) = lo_handler_mass->perform_deletion(
                           is_selection = <fs_unique_phio>
                           iv_delete_mode = '2'
                           iv_test_run    = p_test
                         ).
      IF lv_success = abap_false.
        lv_error_flag = abap_true.
      ENDIF.
      " Atualiza o status para todas as linhas com o mesmo PHIO_ID
      LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs_line>) WHERE soffphio_phio_id = <fs_unique_phio>-soffphio_phio_id.
        IF lv_success = abap_true.
          <fs_line>-delete_status = COND #( WHEN p_test = abap_true THEN 'TESTE - Exclusão completa simulada'
                                            ELSE 'EXCLUÍDO - Vínculos removidos' ).
        ELSE.
          <fs_line>-delete_status = 'ERRO na exclusão'.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ELSEIF p_delsc1 = abap_true.
    " Cenário 2: Excluir apenas fragmentos SOFFCONT1 (loop em todas as linhas)
    LOOP AT gt_output ASSIGNING <fs_line>.
      IF lv_delete_count >= p_limit.
        MESSAGE |Limite de processamento ({ p_limit }) atingido. Processo interrompido.| TYPE 'W'.
        EXIT.
      ENDIF.
      lv_delete_count = lv_delete_count + 1.

      DATA(lv_success_line) = lo_handler_mass->perform_deletion(
                                 is_selection = <fs_line>
                                 iv_delete_mode = '1'
                                 iv_test_run    = p_test
                               ).
      IF lv_success_line = abap_false.
        lv_error_flag = abap_true.
      ENDIF.
      " Atualiza o status para a linha individual
      IF lv_success_line = abap_true.
        <fs_line>-delete_status = COND #( WHEN p_test = abap_true THEN 'TESTE - Exclusão SOFFCONT1 simulada'
                                          ELSE 'EXCLUÍDO - SOFFCONT1' ).
      ELSE.
        <fs_line>-delete_status = 'ERRO na exclusão'.
      ENDIF.
    ENDLOOP.
  ENDIF.

  " Controle de transação centralizado
  IF p_test = abap_false.
    IF lv_error_flag = abap_true.
      ROLLBACK WORK.
      MESSAGE 'Ocorreram erros. Nenhuma exclusão foi efetivada. Verifique a coluna de status.' TYPE 'E'.
    ELSE.
      COMMIT WORK AND WAIT.
      " Remover registros da exibição ALV somente em execução real e bem-sucedida
      DELETE gt_output WHERE delete_status IS NOT INITIAL AND delete_status <> 'ERRO na exclusão'.
    ENDIF.
  ENDIF.
ENDFORM.
