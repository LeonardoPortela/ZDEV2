*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_FORM
*&---------------------------------------------------------------------*


FORM f_refresh_alv USING p_alv.

  CALL METHOD obj_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.


ENDFORM.

FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  DATA: v_col_pos       TYPE i,
        v_ref_tabname   LIKE dd02d-tabname,
        v_ref_fieldname LIKE dd03d-fieldname,
        v_tabname       LIKE dd02d-tabname,
        v_field         LIKE dd03d-fieldname,
        v_scrtext_l     LIKE dd03p-scrtext_l,
        v_outputlen     LIKE lvc_s_fcat-outputlen,
        v_edit          LIKE lvc_s_fcat-edit,
        v_sum           LIKE lvc_s_fcat-do_sum,
        v_emphasize     LIKE lvc_s_fcat-emphasize,
        v_just          LIKE lvc_s_fcat-just,
        v_hotspot       LIKE lvc_s_fcat-hotspot,
        v_f4            LIKE lvc_s_fcat-f4availabl,
        v_check         LIKE lvc_s_fcat-checkbox,
        v_no_out        LIKE lvc_s_fcat-no_out.

  DATA: wa_fcat_out TYPE lvc_s_fcat.

  FREE: wa_fcat, it_fcat.

  v_col_pos = 0.

  LOOP AT tg_dd03l_out INTO DATA(_wl_dd03l_out) WHERE ( fieldname NE 'MANDT' ) AND ( rollname NE 'MANDT' ).
    READ TABLE tg_dd04t_out INTO DATA(_wl_dd04t_out) WITH KEY rollname   = _wl_dd03l_out-rollname
                                                              as4local   = _wl_dd03l_out-as4local
                                                              as4vers    = _wl_dd03l_out-as4vers.

    CHECK sy-subrc EQ 0.

    CLEAR: wa_fcat_out.

    v_col_pos               = v_col_pos + 1.
    wa_fcat_out-col_pos     = v_col_pos.
    wa_fcat_out-ref_table   = _wl_dd03l_out-tabname.
    wa_fcat_out-ref_field   = _wl_dd03l_out-fieldname.
    wa_fcat_out-tabname     = 'IT_SAIDA'.
    wa_fcat_out-fieldname   = _wl_dd03l_out-fieldname.
    wa_fcat_out-scrtext_l   = _wl_dd04t_out-scrtext_l.
    wa_fcat_out-outputlen   = strlen( _wl_dd04t_out-scrtext_l ).

    IF wa_fcat_out-fieldname = 'ID'    OR
       wa_fcat_out-fieldname = 'VKORG'.
      wa_fcat_out-no_out    = abap_true.
    ENDIF.

*-----------------------------------------------------------------------------------------*
*   Não utilizar a f_exit_0008, pois ela esta obsoleta. Utilizar a f_exit_0008_v2
*-----------------------------------------------------------------------------------------*
    PERFORM f_exit_0008 CHANGING  wa_fcat_out-col_pos
                                  wa_fcat_out-ref_table
                                  wa_fcat_out-ref_field
                                  wa_fcat_out-tabname
                                  wa_fcat_out-fieldname
                                  wa_fcat_out-scrtext_l
                                  wa_fcat_out-outputlen
                                  wa_fcat_out-edit
                                  wa_fcat_out-do_sum
                                  wa_fcat_out-emphasize
                                  wa_fcat_out-just
                                  wa_fcat_out-hotspot
                                  wa_fcat_out-f4availabl
                                  wa_fcat_out-checkbox.

    PERFORM f_exit_0008_v2 CHANGING wa_fcat_out.
    PERFORM f_estrutura_alv USING: wa_fcat_out.


  ENDLOOP.

ENDFORM.

FORM f_estrutura_alv USING p_fcat_out TYPE lvc_s_fcat.

  IF ( p_fcat_out-scrtext_l IS NOT INITIAL ).
    IF ( p_fcat_out-reptext IS INITIAL ).
      p_fcat_out-reptext = p_fcat_out-scrtext_l.
    ENDIF.

    IF ( p_fcat_out-scrtext_m IS INITIAL ).
      p_fcat_out-scrtext_m = p_fcat_out-scrtext_l.
    ENDIF.

    IF ( p_fcat_out-scrtext_s IS INITIAL ).
      p_fcat_out-scrtext_s = p_fcat_out-scrtext_l.
    ENDIF.
  ENDIF.


  APPEND p_fcat_out TO it_fcat.
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

  CLEAR: <fs_wa_saida>,
         <fs_it_saida>[],
         tg_field_screen_key[],
         tg_dd03l[],
         tg_dd04t[],
         tg_dd04t_out[],
         tg_dd03l_out[].

ENDFORM.

FORM f_selecionar_dados.

  PERFORM f_limpa_variaveis.

  SELECT *
    FROM dd03l INTO TABLE tg_dd03l
   WHERE tabname = p_db_tab.

  IF tg_dd03l[] IS INITIAL.
    MESSAGE | Dados da tabela { p_db_tab } não encontrados! Tabela DD03L!| TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SORT tg_dd03l BY position.

  LOOP AT tg_dd03l INTO DATA(_wl_dd03l) WHERE keyflag EQ abap_true..
    CLEAR: tg_field_screen_key.
    tg_field_screen_key-field_screen = '<FS_WA_REGISTRO_MANTER>' && '-' && _wl_dd03l-fieldname.
    APPEND tg_field_screen_key.
  ENDLOOP.

  IF tg_dd03l[] IS NOT INITIAL.
    SELECT *
      FROM dd04t INTO TABLE tg_dd04t[]
       FOR ALL ENTRIES IN tg_dd03l
    WHERE rollname   EQ tg_dd03l-rollname
      AND ddlanguage EQ sy-langu
      AND as4local   EQ tg_dd03l-as4local
      AND as4vers    EQ tg_dd03l-as4vers.
  ENDIF.

*-------------------------------------------------------------------------------*
*  Get Dados Monta Field Cat
*-------------------------------------------------------------------------------*

  SELECT *
    FROM dd03l INTO TABLE tg_dd03l_out
   WHERE tabname = p_stcnam.

  IF tg_dd03l[] IS INITIAL.
    MESSAGE | Dados da tabela { p_stcnam } não encontrados! Tabela DD03L!| TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF tg_dd03l_out[] IS NOT INITIAL.
    SELECT *
      FROM dd04t INTO TABLE tg_dd04t_out[]
       FOR ALL ENTRIES IN tg_dd03l_out
    WHERE rollname   EQ tg_dd03l_out-rollname
      AND ddlanguage EQ sy-langu
      AND as4local   EQ tg_dd03l_out-as4local
      AND as4vers    EQ tg_dd03l_out-as4vers.
  ENDIF.

  TRY.
      SELECT *
        FROM (p_db_tab) INTO TABLE <fs_table>
       WHERE vkorg     = p_vkorg
         AND cancelado = abap_false.

    CATCH cx_sy_dynamic_osql_error.
      MESSAGE 'Erro ao executar o SQL Dinamico!' TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE PROGRAM.
  ENDTRY.

ENDFORM.

FORM f_processa_dados .

  LOOP AT <fs_table> ASSIGNING <fs_wa_table>.

    CLEAR: <fs_wa_saida>.

    MOVE-CORRESPONDING <fs_wa_table> TO <fs_wa_saida>.

    PERFORM f_exit_0004 CHANGING <fs_wa_saida>.

    IF <fs_wa_saida> IS NOT INITIAL.
      APPEND <fs_wa_saida> TO <fs_it_saida>.
    ENDIF.
  ENDLOOP.

  IF p_stcsai IS NOT INITIAL.
    PERFORM f_exit_0007 TABLES <fs_it_saida>.
  ENDIF.

  PERFORM f_exit_0010 TABLES <fs_it_saida>.


ENDFORM.

FORM f_get_cond_chave USING p_saida TYPE string
                   CHANGING p_cond  TYPE rsds_where.

  DATA: v_cond_line     TYPE rsdswhere,
        vfield          TYPE string,
        v_value_field_c TYPE string.

  FIELD-SYMBOLS: <value_field> TYPE any.

  CLEAR: p_cond.

  p_cond-tablename = p_db_tab.

  LOOP AT tg_dd03l INTO DATA(_wl_dd03l) WHERE keyflag EQ abap_true AND fieldname NE 'MANDT' AND rollname NE 'MANDT' .

    CLEAR: v_cond_line-line.

    vfield = p_saida && '-' && _wl_dd03l-fieldname.

    ASSIGN (vfield) TO <value_field>.

    IF <value_field> IS ASSIGNED.
      v_value_field_c = '''' && <value_field> && ''''.
    ENDIF.

    IF p_cond-where_tab[] IS INITIAL.
      CONCATENATE '(' _wl_dd03l-fieldname 'EQ' v_value_field_c  ')'
             INTO v_cond_line-line SEPARATED BY space.

    ELSE.
      CONCATENATE 'AND (' _wl_dd03l-fieldname 'EQ' v_value_field_c ')'
             INTO v_cond_line-line SEPARATED BY space.
    ENDIF.

    APPEND v_cond_line TO p_cond-where_tab.
  ENDLOOP.

  IF p_cond-where_tab[] IS INITIAL.
    MESSAGE 'Não foi possível montar a condição para acessar o registro' TYPE 'S'.
    EXIT.
  ENDIF.


ENDFORM.
