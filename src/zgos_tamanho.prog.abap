REPORT zgos_tamanho.

*&---------------------------------------------------------------------*
*& SELECTION SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_data FOR sy-datum OBLIGATORY.
PARAMETERS: p_user TYPE xubname.
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& TYPE-POOLS
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

*&---------------------------------------------------------------------*
*& TYPE DEFINITIONS
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_anexo_data,
         phio_id   TYPE soffphio-phio_id,
         crea_time TYPE soffphio-crea_time,
         clustr    TYPE soffcont1-clustr,
       END OF ty_anexo_data.

TYPES: BEGIN OF ty_output,
         ano           TYPE c LENGTH 4,
         mes           TYPE c LENGTH 2,
         qtd_anexos    TYPE i,
         tamanho_total TYPE p LENGTH 15 DECIMALS 2,
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
  PERFORM fetch_and_aggregate_data.

  IF gt_output IS INITIAL.
    MESSAGE 'Nenhum anexo encontrado para os filtros informados.' TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM calculate_yearly_totals.
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
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = gt_output
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM build_fieldcat.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv,
        l_col       TYPE i VALUE 1.

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

  add_field 'ANO'           'Ano'           'Ano'        'Ano'.
  add_field 'MES'           'Mês'           'Mês'        'Mês'.
  add_field 'QTD_ANEXOS'    'Qtd. Anexos'   'Qtd. Anexos' 'Qtd.'.
  add_field 'TAMANHO_TOTAL' 'Tamanho (MB)' 'Tam. (MB)'  'Tam.'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALCULATE_YEARLY_TOTALS
*&---------------------------------------------------------------------*
FORM calculate_yearly_totals.
  DATA: lt_output_with_totals TYPE TABLE OF ty_output,
        ls_yearly_total       TYPE ty_output,
        lv_current_year       TYPE c LENGTH 4.

  LOOP AT gt_output INTO gs_output.
    IF lv_current_year IS INITIAL.
      lv_current_year = gs_output-ano.
    ENDIF.

    IF gs_output-ano <> lv_current_year.
      ls_yearly_total-mes = 'TOTAL'.
      APPEND ls_yearly_total TO lt_output_with_totals.
      CLEAR ls_yearly_total.
      lv_current_year = gs_output-ano.
    ENDIF.

    APPEND gs_output TO lt_output_with_totals.
    ls_yearly_total-ano = gs_output-ano.
    ls_yearly_total-qtd_anexos = ls_yearly_total-qtd_anexos + gs_output-qtd_anexos.
    ls_yearly_total-tamanho_total = ls_yearly_total-tamanho_total + gs_output-tamanho_total.
  ENDLOOP.

  IF ls_yearly_total-ano IS NOT INITIAL.
    ls_yearly_total-mes = 'TOTAL'.
    APPEND ls_yearly_total TO lt_output_with_totals.
  ENDIF.

  gt_output = lt_output_with_totals.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FETCH_AND_AGGREGATE_DATA
*&---------------------------------------------------------------------*
FORM fetch_and_aggregate_data.
  DATA: lt_anexos         TYPE STANDARD TABLE OF ty_anexo_data,
        lv_timestamp_from TYPE string,
        lv_timestamp_to   TYPE string.

  DATA(ls_date_range) = s_data[ 1 ].
  IF ls_date_range-high IS INITIAL.
    ls_date_range-high = ls_date_range-low.
  ENDIF.

  CONCATENATE ls_date_range-low '000000' INTO lv_timestamp_from.
  CONCATENATE ls_date_range-high '235959' INTO lv_timestamp_to.

  SELECT a~phio_id, a~crea_time, b~clustr
    FROM soffphio AS a
    INNER JOIN soffcont1 AS b ON a~phio_id = b~phio_id
    WHERE a~crea_time BETWEEN @lv_timestamp_from AND @lv_timestamp_to
      AND ( @p_user IS INITIAL OR a~crea_user = @p_user )
    INTO TABLE @lt_anexos.

  IF lt_anexos IS INITIAL.
    RETURN.
  ENDIF.

  SORT lt_anexos BY phio_id.

  DATA: lv_ano TYPE c LENGTH 4,
        lv_mes TYPE c LENGTH 2.

  LOOP AT lt_anexos ASSIGNING FIELD-SYMBOL(<fs_anexo_group>)
      GROUP BY ( phio_id   = <fs_anexo_group>-phio_id
                 crea_time = <fs_anexo_group>-crea_time ).

    CLEAR gs_output.
    lv_ano = <fs_anexo_group>-crea_time(4).
    lv_mes = <fs_anexo_group>-crea_time+4(2).

    DATA(lv_doc_size) = 0.
    LOOP AT GROUP <fs_anexo_group> ASSIGNING FIELD-SYMBOL(<fs_anexo_part>).
      lv_doc_size = lv_doc_size + <fs_anexo_part>-clustr.
    ENDLOOP.

    READ TABLE gt_output INTO gs_output WITH KEY ano = lv_ano mes = lv_mes.
    IF sy-subrc = 0.
      gs_output-qtd_anexos    = gs_output-qtd_anexos + 1.
      gs_output-tamanho_total = gs_output-tamanho_total + ( lv_doc_size / 1024 / 1024 ).
      MODIFY gt_output FROM gs_output TRANSPORTING qtd_anexos tamanho_total WHERE ano = lv_ano AND mes = lv_mes.
    ELSE.
      gs_output-ano           = lv_ano.
      gs_output-mes           = lv_mes.
      gs_output-qtd_anexos    = 1.
      gs_output-tamanho_total = lv_doc_size / 1024 / 1024.
      APPEND gs_output TO gt_output.
    ENDIF.
  ENDLOOP.

  SORT gt_output BY ano mes.
ENDFORM.
