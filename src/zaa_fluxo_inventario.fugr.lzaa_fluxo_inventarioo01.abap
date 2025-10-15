*----------------------------------------------------------------------*
***INCLUDE LZAA_FLUXO_INVENTARIOO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZAA001'.
  SET TITLEBAR 'ZAA001'.

  PERFORM init_alv.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

*******************************************************************************************
* selecao dados
*******************************************************************************************
FORM f_selecao_dados.

  SELECT *
    FROM anla
    INTO TABLE t_anla
     FOR ALL ENTRIES IN t_dados
   WHERE bukrs = t_dados-bukrs
     AND anln1 = t_dados-anln1
     AND anln2 = t_dados-anln2.

  SELECT *
    FROM anlz
    INTO TABLE t_anlz
     FOR ALL ENTRIES IN t_dados
   WHERE bukrs  = t_dados-bukrs
     AND anln1  = t_dados-anln1
     AND anln2  = t_dados-anln2
     AND bdatu >= sy-datum.

  SELECT *
    FROM zaa005
    INTO TABLE t_zaa005
     FOR ALL ENTRIES IN t_dados
   WHERE bukrs = t_dados-bukrs
     AND anln1 = t_dados-anln1
     AND anln2 = t_dados-anln2.

  SORT t_anla BY bukrs anln1 anln2.

  SORT t_anlz BY bukrs anln1 anln2.
  DELETE ADJACENT DUPLICATES FROM t_anlz
                        COMPARING bukrs anln1 anln2.

  SORT t_zaa005 BY bukrs anln1 anln2.
  DELETE ADJACENT DUPLICATES FROM t_zaa005
                        COMPARING bukrs anln1 anln2.

ENDFORM.

*******************************************************************************************
* monta dados
*******************************************************************************************
FORM f_monta_dados USING p_tabix.

  LOOP AT t_dados INTO w_dados.

    CLEAR w_anla.
    READ TABLE t_anla INTO w_anla WITH KEY bukrs = w_dados-bukrs
                                           anln1 = w_dados-anln1
                                           anln2 = w_dados-anln2
                                  BINARY SEARCH.
    l_tem_anla = sy-subrc.

    CLEAR w_anlz.
    READ TABLE t_anlz INTO w_anlz WITH KEY bukrs = w_dados-bukrs
                                           anln1 = w_dados-anln1
                                           anln2 = w_dados-anln2
                                  BINARY SEARCH.

    CLEAR w_zaa005.
    READ TABLE t_zaa005 INTO w_zaa005 WITH KEY bukrs = w_dados-bukrs
                                               anln1 = w_dados-anln1
                                               anln2 = w_dados-anln2
                                      BINARY SEARCH.
    l_tem_zaa005 = sy-subrc.

    CLEAR w_sobra.
    IF l_tem_anla <> 0.
      MOVE-CORRESPONDING w_dados    TO w_sobra.
      IF l_tem_zaa005 = 0.
        MOVE-CORRESPONDING w_zaa005 TO w_sobra.
        MOVE w_zaa005-gsber         TO w_sobra-werks.
      ENDIF.
      MOVE abap_true                TO w_sobra-manual.
    ELSE.
      MOVE-CORRESPONDING w_anlz     TO w_sobra.
      MOVE-CORRESPONDING w_zaa005   TO w_sobra.
      MOVE w_zaa005-gsber           TO w_sobra-werks.
      MOVE-CORRESPONDING w_anla     TO w_sobra.
    ENDIF.

    IF w_sobra-gjahr IS INITIAL.
      w_sobra-gjahr = g_gjahr.
    ENDIF.

    IF w_sobra-aktiv IS INITIAL.
      w_sobra-aktiv = sy-datum.
    ENDIF.

    PERFORM f_set_colunas        USING l_tem_anla
                              CHANGING w_sobra.

    IF p_tabix = 0.
      APPEND w_sobra                  TO t_sobra.
    ELSE.
      MODIFY t_sobra FROM w_sobra  INDEX p_tabix.
    ENDIF.

  ENDLOOP.

ENDFORM.

*******************************************************************************************
* AJUSTA CELLTAB
*******************************************************************************************
FORM f_ajusta_celltab.

  CHECK t_sobra[] IS NOT INITIAL.

  SELECT *
    FROM anla
    INTO TABLE t_anla
     FOR ALL ENTRIES IN t_sobra
   WHERE bukrs = l_bukrs
     AND anln1 = t_sobra-anln1
     AND anln2 = t_sobra-anln2.

  SORT t_anla BY bukrs anln1 anln2.

  LOOP AT t_sobra INTO w_sobra.
    l_tabix = sy-tabix.

    CLEAR w_anla.
    READ TABLE t_anla INTO w_anla WITH KEY bukrs = l_bukrs
                                           anln1 = w_sobra-anln1
                                           anln2 = w_sobra-anln2
                                  BINARY SEARCH.
    l_tem_anla = sy-subrc.

    PERFORM f_set_colunas        USING l_tem_anla
                              CHANGING w_sobra.

    MODIFY t_sobra  FROM w_sobra INDEX l_tabix.
  ENDLOOP.

ENDFORM.

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.
  ENDIF.

  PERFORM build_fieldcatalog.
  PERFORM toolbar_alv.

  l_stable-row          = abap_true.
  l_stable-col          = abap_true.

  w_layout-zebra        = abap_false.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
* w_layout-cwidth_opt   = 'X'.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.
  w_layout-stylefname   = 'CELLTAB'.
  w_layout-ctab_fname   = 'COLORCELL'.

  IF m_event_handler IS INITIAL.
    CREATE OBJECT m_event_handler.
    SET HANDLER : m_event_handler->toolbar FOR g_grid.
    SET HANDLER : m_event_handler->user_command FOR g_grid.
  ENDIF.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
*     it_sort              = lt_sort
      it_outtab            = t_sobra.

  SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid.
*              lcl_event_handler=>on_double_click  FOR g_grid.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD g_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = l_stable.

ENDFORM.                    " init_tree

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM build_fieldcatalog.

  FREE: t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'ANLN1'.
  ls_fieldcatalog-ref_table = '    '.
  ls_fieldcatalog-ref_field = '     '.
  ls_fieldcatalog-col_pos   = 1.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-coltext   = 'Imobilizado'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'ANLN2'.
  ls_fieldcatalog-ref_table = '    '.
  ls_fieldcatalog-ref_field = '     '.
  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 4.
  ls_fieldcatalog-no_out    = abap_true.
  ls_fieldcatalog-coltext   = 'Sub-Num'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'GJAHR'.
  ls_fieldcatalog-ref_table = 'ZAA005'.
  ls_fieldcatalog-ref_field = 'GJAHR'.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 9.
  ls_fieldcatalog-dd_outlen = 4.
  ls_fieldcatalog-coltext   = 'Ano Inv.'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'TXT50'.
  ls_fieldcatalog-ref_table = 'ANLA'.
  ls_fieldcatalog-ref_field = 'TXT50'.
  ls_fieldcatalog-col_pos   = 4.
  ls_fieldcatalog-outputlen = 34.
  ls_fieldcatalog-dd_outlen = 50.
  ls_fieldcatalog-coltext   = 'Denominação Imobilizado'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'TXA50'.
  ls_fieldcatalog-ref_table = 'ANLA'.
  ls_fieldcatalog-ref_field = 'TXA50'.
  ls_fieldcatalog-col_pos   = 5.
  ls_fieldcatalog-outputlen = 30.
  ls_fieldcatalog-dd_outlen = 50.
  ls_fieldcatalog-coltext   = 'Marca e Modelo'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'WERKS'.
  ls_fieldcatalog-ref_table = 'ANLZ'.
  ls_fieldcatalog-ref_field = 'WERKS'.
  ls_fieldcatalog-col_pos   = 6.
  ls_fieldcatalog-outputlen = 5.
  ls_fieldcatalog-dd_outlen = 4.
  ls_fieldcatalog-coltext   = 'Filial'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'KOSTL'.
  ls_fieldcatalog-ref_table = 'ANLZ'.
  ls_fieldcatalog-ref_field = 'KOSTL'.
  ls_fieldcatalog-col_pos   = 7.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-coltext   = 'Centro Custo'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'CC_SOBRA'.
  ls_fieldcatalog-ref_table = 'ANLZ'.
  ls_fieldcatalog-ref_field = 'KOSTL'.
  ls_fieldcatalog-col_pos   = 8.
  ls_fieldcatalog-outputlen = 13.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-coltext   = 'C.Custo Sobra'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'INVNR'.
  ls_fieldcatalog-ref_table = 'ANLA'.
  ls_fieldcatalog-ref_field = 'INVNR'.
  ls_fieldcatalog-col_pos   = 9.
  ls_fieldcatalog-outputlen = 9.
  ls_fieldcatalog-dd_outlen = 25.
  ls_fieldcatalog-coltext   = 'No.Chassi'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'SERNR'.
  ls_fieldcatalog-ref_table = 'ANLA'.
  ls_fieldcatalog-ref_field = 'SERNR'.
  ls_fieldcatalog-col_pos   = 10.
  ls_fieldcatalog-outputlen = 11.
  ls_fieldcatalog-dd_outlen = 18.
  ls_fieldcatalog-coltext   = 'No.Série'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'STORT'.
  ls_fieldcatalog-ref_table = '    '.
  ls_fieldcatalog-ref_field = '     '.
  ls_fieldcatalog-col_pos   = 11.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-coltext   = 'Localização'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'RAUMN'.
  ls_fieldcatalog-ref_table = 'ANLZ'.
  ls_fieldcatalog-ref_field = 'RAUMN'.
  ls_fieldcatalog-col_pos   = 12.
  ls_fieldcatalog-outputlen = 6.
  ls_fieldcatalog-dd_outlen = 8.
  ls_fieldcatalog-coltext   = 'Sala'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'KFZKZ'.
  ls_fieldcatalog-ref_table = 'ANLZ'.
  ls_fieldcatalog-ref_field = 'KFZKZ'.
  ls_fieldcatalog-col_pos   = 13.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Placa'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SOBRA'.
  ls_fieldcatalog-fieldname = 'AKTIV'.
  ls_fieldcatalog-ref_table = 'ANLA'.
  ls_fieldcatalog-ref_field = 'AKTIV'.
  ls_fieldcatalog-col_pos   = 14.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-no_out    = abap_true.
  ls_fieldcatalog-coltext   = 'Dt.Incorp.'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv.

  FREE: pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude.
*
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: l_erro TYPE c.

  CASE ok_code.
    WHEN '&SALVAR'.
      CLEAR l_erro.

      CALL METHOD g_grid->check_changed_data.

      PERFORM f_ajusta_celltab.
      PERFORM f_valida_dados CHANGING l_erro.

      IF l_erro = abap_false.
        PERFORM f_grava.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  l_stable-row = 'X'.
  l_stable-col = 'X'.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = l_stable.

ENDMODULE.

*&---------------------------------------------------------------------*
*& F_setar colunas
*&---------------------------------------------------------------------*
FORM f_set_colunas USING p_edit
                CHANGING p_sobra TYPE ty_sobra.

  DATA: l_modo TYPE lvc_style.

  FREE: t_celltab.

* p_edit  = 0 --> so exibir
* p_edit <> 0 --> editar

  IF p_edit = 0.
    l_modo = cl_gui_alv_grid=>mc_style_disabled.
  ELSE.
    l_modo = cl_gui_alv_grid=>mc_style_enabled.
  ENDIF.

  w_celltab-fieldname = 'ANLN1'.
  w_celltab-style     = l_modo. "cl_gui_alv_grid=>mc_style_enabled.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'ANLN2'.
  w_celltab-style     = l_modo. "cl_gui_alv_grid=>mc_style_enabled.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'GJAHR'.
  w_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'TXT50'.
  w_celltab-style     = l_modo.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'TXA50'.
  w_celltab-style     = l_modo.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'WERKS'.
  w_celltab-style     = l_modo.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'KOSTL'.
  w_celltab-style     = l_modo.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'CC_SOBRA'.
  w_celltab-style     = cl_gui_alv_grid=>mc_style_enabled.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'INVNR'.
  w_celltab-style     = l_modo.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'SERNR'.
  w_celltab-style     = l_modo.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'STORT'.
  w_celltab-style     = l_modo.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'RAUMN'.
  w_celltab-style     = l_modo.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'KFZKZ'.
  w_celltab-style     = l_modo.
  APPEND w_celltab    TO t_celltab.

  w_celltab-fieldname = 'AKTIV'.
  w_celltab-style     = l_modo.
  APPEND w_celltab    TO t_celltab.

  p_sobra-celltab[] = t_celltab[].

ENDFORM.

*&---------------------------------------------------------------------*
*& F_validar
*&---------------------------------------------------------------------*
FORM f_valida_dados CHANGING p_erro.

  CLEAR p_erro.

  DELETE t_sobra WHERE anln1 IS INITIAL.

  LOOP AT t_sobra INTO w_sobra.
    FREE w_sobra-colorcell[].
    w_sobra-duplic = abap_false.
    MODIFY t_sobra FROM w_sobra INDEX sy-tabix.
  ENDLOOP.

  LOOP AT t_sobra INTO w_sobra.
    FREE: t_colorcell.

    l_tabix = sy-tabix.

    IF w_sobra-manual = abap_true.
      IF w_sobra-gjahr IS INITIAL.
        p_erro = abap_true.
        PERFORM f_color_cell USING 'GJAHR'.
      ENDIF.

      IF w_sobra-txt50 IS INITIAL.
        p_erro = abap_true.
        PERFORM f_color_cell USING 'TXT50'.
      ENDIF.

      IF w_sobra-txa50 IS INITIAL.
        p_erro = abap_true.
        PERFORM f_color_cell USING 'TXA50'.
      ENDIF.

      IF w_sobra-werks IS INITIAL.
        p_erro = abap_true.
        PERFORM f_color_cell USING 'WERKS'.
      ENDIF.

      IF w_sobra-kostl IS INITIAL.
        p_erro = abap_true.
        PERFORM f_color_cell USING 'KOSTL'.
      ENDIF.

*     IF w_sobra-cc_sobra IS INITIAL.
*       p_erro = abap_true.
*       PERFORM f_color_cell USING 'CC_SOBRA'.
*     ENDIF.

*     IF w_sobra-invnr IS INITIAL.
*       p_erro = abap_true.
*       PERFORM f_color_cell USING 'INVNR'.
*     ENDIF.

*     IF w_sobra-sernr IS INITIAL.
*       p_erro = abap_true.
*       PERFORM f_color_cell USING 'SERNR'.
*     ENDIF.

*     IF w_sobra-stort IS INITIAL.
*       p_erro = abap_true.
*       PERFORM f_color_cell USING 'STORT'.
*     ENDIF.

      IF w_sobra-raumn IS INITIAL.
        p_erro = abap_true.
        PERFORM f_color_cell USING 'RAUMN'.
      ENDIF.

*     IF w_sobra-kfzkz IS INITIAL.
*       p_erro = abap_true.
*       PERFORM f_color_cell USING 'KFZKZ'.
*     ENDIF.

*     IF w_sobra-AKTIV IS INITIAL.
*       p_erro = abap_true.
*       PERFORM f_color_cell USING 'AKTIV'.
*     ENDIF.
    ELSE.
*     IF w_sobra-cc_sobra IS INITIAL.
*       p_erro = abap_true.
*       PERFORM f_color_cell USING 'CC_SOBRA'.
*     ENDIF.
    ENDIF.

*---------------------------------------
*---Centro de Custo
*---------------------------------------
    SELECT kostl
      FROM csks
        UP TO 1 ROWS
      INTO @DATA(l_kostl)
     WHERE kostl  = @w_sobra-kostl
       AND datbi >= @sy-datum.
    ENDSELECT.

    IF sy-subrc <> 0.
      p_erro = abap_true.
      PERFORM f_color_cell USING 'KOSTL'.
      MESSAGE s024(sd) WITH text-100 DISPLAY LIKE 'E'.
    ENDIF.

*---------------------------------------
*---Centro de Custo Sobra
*---------------------------------------
    SELECT kostl
      FROM csks
        UP TO 1 ROWS
      INTO @l_kostl
     WHERE kostl  = @w_sobra-cc_sobra
       AND datbi >= @sy-datum.
    ENDSELECT.

*   IF sy-subrc <> 0.
*     p_erro = abap_true.
*     PERFORM f_color_cell USING 'CC_SOBRA'.
*     MESSAGE s024(sd) WITH text-100 DISPLAY LIKE 'E'.
*   ENDIF.

  ENDLOOP.

  IF p_erro = abap_true.
    MESSAGE i024(sd) WITH text-030.
    EXIT.
  ENDIF.

*---------------------------------------
* valida duplicidade
*---------------------------------------
  t_sobra_aux[] = t_sobra[].

  LOOP AT t_sobra INTO w_sobra.
    FREE: t_colorcell,
          l_cont.

    l_tabix = sy-tabix.

    LOOP AT t_sobra_aux INTO w_sobra_aux WHERE anln1 = w_sobra-anln1
                                           AND anln2 = w_sobra-anln2.
      l_cont = l_cont + 1.
    ENDLOOP.

    IF l_cont > 1.
      p_erro = abap_true.
      w_sobra-duplic = abap_true.
      PERFORM f_color_cell USING 'ANLN1'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF p_erro = abap_true.
    MESSAGE i024(sd) WITH text-040.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& F_GRAVA
*&---------------------------------------------------------------------*
FORM f_color_cell USING p_campo.

  w_colorcell-fname = p_campo.
  w_colorcell-color-col = '6'.
  w_colorcell-color-int = '1'.
  w_colorcell-color-inv = '1'.
  APPEND w_colorcell  TO t_colorcell.

  w_sobra-colorcell[] = t_colorcell[].

  MODIFY t_sobra FROM w_sobra INDEX l_tabix.
ENDFORM.

*&---------------------------------------------------------------------*
*& F_GRAVA
*&---------------------------------------------------------------------*
FORM f_grava.

*-------------------------------------
* eliminar linhas
*-------------------------------------
  LOOP AT t_sobra_elim  INTO w_sobra_elim.
    DELETE FROM zaa005 WHERE anln1 = w_sobra_elim-anln1
                         AND anln2 = w_sobra_elim-anln2
                         AND bukrs = l_bukrs.
  ENDLOOP.

*-------------------------------------
* grava zaa005
*-------------------------------------
  LOOP AT t_sobra INTO w_sobra.

    CLEAR w_zaa005.

    SELECT anln1
      FROM zaa005
      INTO @DATA(l_anln1)
        UP TO 1 ROWS
     WHERE anln1  = @w_sobra-anln1
       AND anln2  = @w_sobra-anln2
       AND bukrs  = @l_bukrs
       AND gsber  = @w_sobra-werks
       AND kostl  = @w_sobra-kostl
       AND gjahr  = @w_sobra-gjahr
       AND manual = @abap_false.
    ENDSELECT.

    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    w_zaa005-mandt    = sy-mandt.
    w_zaa005-gjahr    = w_sobra-gjahr.
    w_zaa005-anln1    = w_sobra-anln1.
    w_zaa005-anln2    = w_sobra-anln2.
    w_zaa005-bukrs    = l_bukrs.
    w_zaa005-gsber    = w_sobra-werks.
    w_zaa005-kostl    = w_sobra-kostl.
    w_zaa005-aktiv    = w_sobra-aktiv.
    w_zaa005-txt50    = w_sobra-txt50.
    w_zaa005-txa50    = w_sobra-txa50.
    w_zaa005-invnr    = w_sobra-invnr.
    w_zaa005-sernr    = w_sobra-sernr.
    w_zaa005-stort    = w_sobra-stort.
    w_zaa005-raumn    = w_sobra-raumn.
    w_zaa005-kfzkz    = w_sobra-kfzkz.
    w_zaa005-cc_sobra = w_sobra-cc_sobra.
    w_zaa005-zimob_v  = '4'.
    w_zaa005-manual   = abap_true. "w_sobra-manual.
    w_zaa005-dt_proc  = sy-datum.
    w_zaa005-hr_proc  = sy-uzeit.
    w_zaa005-us_proc  = sy-uname.

    MODIFY zaa005  FROM w_zaa005.

    COMMIT WORK AND WAIT.
  ENDLOOP.

ENDFORM.
