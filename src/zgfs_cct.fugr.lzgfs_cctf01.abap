*----------------------------------------------------------------------*
***INCLUDE LZGFS_CCTF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SELECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_selecao.

  FREE: t_zsdt0264,
        r_docdat.

  r_docdat-sign   = 'I'.
  r_docdat-option = 'BT'.
  r_docdat-low    = g_dt_lista_de.
  r_docdat-high   = g_dt_lista_ate.
  APPEND r_docdat.

  SELECT *
    FROM zsdt0264 as a
    INTO TABLE t_zsdt0264
   WHERE docdat IN r_docdat
    AND id_recepcao EQ '0000000000'
    and not exists (

      select b~id_recepcao
        from zlest0147 as b INNER JOIN zlest0146 as c ON b~id_recepcao = c~id_recepcao
       WHERE b~chave_nfe = a~chave
         and c~cancel    = space

    ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_saida .

  FREE: t_alv.

  LOOP AT t_zsdt0264 INTO w_zsdt0264.
    CLEAR w_alv.
    MOVE-CORRESPONDING w_zsdt0264 TO w_alv.
    APPEND w_alv TO t_alv.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv .

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.

    PERFORM build_fieldcatalog.
*   PERFORM sort_field.

    w_layout-zebra = 'X'.
*   W_layout-edit  = 'X'. " Makes all Grid editable

    " SET_TABLE_FOR_FIRST_DISPLAY
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout       = w_layout
      CHANGING
        it_fieldcatalog = t_fieldcatalog
        it_sort         = lt_sort
        it_outtab       = t_alv. " Data

  ELSE.
    CALL METHOD g_grid->refresh_table_display.
  ENDIF.

ENDFORM.

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM build_fieldcatalog.

* get fieldcatalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSDS0051'
    CHANGING
      ct_fieldcat      = t_fieldcatalog.

  SORT t_fieldcatalog BY scrtext_l.

* change fieldcatalog
  DATA: ls_fieldcatalog TYPE lvc_s_fcat.

  LOOP AT t_fieldcatalog INTO ls_fieldcatalog.
    CASE ls_fieldcatalog-fieldname.
*      WHEN 'INTEGRADO'.
*        ls_fieldcatalog-col_pos   = 1.
*        ls_fieldcatalog-outputlen = 10.
*        ls_fieldcatalog-checkbox  = abap_true.
*        ls_fieldcatalog-coltext   = 'Integrado'.
      WHEN 'DOCNUM'.
        ls_fieldcatalog-col_pos   = 2.
        ls_fieldcatalog-outputlen = 15.
        ls_fieldcatalog-coltext   = 'Documento'.
      WHEN 'CHAVE'.
        ls_fieldcatalog-col_pos   = 3.
        ls_fieldcatalog-outputlen = 45.
        ls_fieldcatalog-coltext   = 'Chave'.
      WHEN 'DOCDAT'.
        ls_fieldcatalog-col_pos   = 4.
        ls_fieldcatalog-outputlen = 15.
        ls_fieldcatalog-coltext   = 'Data Documento'.
      WHEN 'BRANCH'.
        ls_fieldcatalog-col_pos   = 5.
        ls_fieldcatalog-outputlen = 20.
        ls_fieldcatalog-coltext   = 'Local Negócio'.
      WHEN 'TERMINAL'.
        ls_fieldcatalog-col_pos   = 6.
        ls_fieldcatalog-outputlen = 13.
        ls_fieldcatalog-coltext   = 'Terminal'.
      WHEN 'DT_ENT'.
        ls_fieldcatalog-col_pos   = 7.
        ls_fieldcatalog-outputlen = 12.
        ls_fieldcatalog-coltext   = 'Data Criação'.
      WHEN 'HR_ENT'.
        ls_fieldcatalog-col_pos   = 8.
        ls_fieldcatalog-outputlen = 12.
        ls_fieldcatalog-coltext   = 'Hora Criação'.
      WHEN 'MOTIVO'.
        ls_fieldcatalog-col_pos   = 9.
        ls_fieldcatalog-outputlen = 50.
        ls_fieldcatalog-coltext   = 'Motivo'.
      WHEN OTHERS.
        ls_fieldcatalog-no_out    = abap_true.
    ENDCASE.

    MODIFY t_fieldcatalog FROM ls_fieldcatalog INDEX sy-tabix.
  ENDLOOP.
ENDFORM.                               " build_fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  F_REPROCESSAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_reprocessar.

  DATA: tg_selectedcell TYPE lvc_t_cell,
        envdat          TYPE sy-datum.

  CALL METHOD g_grid->get_selected_cells
    IMPORTING
      et_cell = tg_selectedcell.

  envdat = sy-datum + 1.

  LOOP AT tg_selectedcell INTO DATA(wa_selectedcell).

    CLEAR w_alv.
    READ TABLE t_alv INTO w_alv INDEX wa_selectedcell-row_id-index.

    UPDATE zsdt0264 SET naoimportar = abap_false
                        envdat      = envdat
                        motivo      = 'Aguardando Reprocessamento!'
                       WHERE docnum = w_alv-docnum.

  ENDLOOP.

  COMMIT WORK.

  PERFORM f_selecao.
  PERFORM f_monta_saida.

  CALL METHOD g_grid->refresh_table_display.

ENDFORM.
