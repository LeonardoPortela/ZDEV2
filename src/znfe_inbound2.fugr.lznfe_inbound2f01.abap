*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUND2F01.
*----------------------------------------------------------------------*

************************************************************************
* Form f_selecao_fornecedor
************************************************************************
FORM f_selecao_fornecedor  USING p_bukrs
                                 p_lifnr.

  CREATE OBJECT lc_zcl_nfe.

  FREE: t_saida.

  t_lfbw = lc_zcl_nfe->get_fornecedor_irf( i_bukrs = p_bukrs
                                           i_lifnr = p_lifnr ).

  CHECK t_lfbw[] IS NOT INITIAL.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE t_saida
    FROM lfbw
    INNER JOIN t001  ON t001~bukrs  = p_bukrs
    INNER JOIN t059u ON t059u~spras = sy-langu
                    AND t059u~land1 = t001~land1
                    AND t059u~witht = lfbw~witht
     FOR ALL ENTRIES IN t_lfbw
   WHERE lfbw~bukrs = t_lfbw-bukrs
     AND lfbw~lifnr = t_lfbw-lifnr.

ENDFORM.

************************************************************************
* exibir dados
************************************************************************
FORM f_exibir_dados.

  CALL SCREEN 0100 STARTING AT 40  06
                     ENDING AT 140 13.

ENDFORM.

***********************************************************************************
* INICIA ALV
***********************************************************************************
FORM f_init_alv.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
* w_layout-no_rowmark   = abap_true.
  w_layout-zebra        = abap_true.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_true.
* w_layout-stylefname   = 'CELLSTYLES'.
* w_layout-ctab_fname   = 'CELLCOLOR'.
  w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-no_rowmark   = abap_true.

  IF g_grid_det IS INITIAL.
    PERFORM f_fieldcatalog USING 'DETALHE'.

    CREATE OBJECT g_custom_container_det
      EXPORTING
        container_name = 'CC_DETALHAMENTO'.

    CREATE OBJECT g_grid_det
      EXPORTING
        i_parent          = g_custom_container_det
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid_det,
                 lcl_event_handler=>on_data_changed  FOR g_grid_det,
                 lcl_event_handler=>user_command     FOR g_grid_det,
                 lcl_event_handler=>toolbar          FOR g_grid_det.

    CALL METHOD g_grid_det->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_saida
*       it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD g_grid_det->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog USING p_tipo.

  FREE t_fieldcat.

  PERFORM f_estrutura_alv USING:
    01  ''           ''         'T_SAIDA'    'WITHT'                  'Ctg.IRF'              '07'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    02  ''           ''         'T_SAIDA'    'TEXT40'                 'Descrição'            '40'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    03  ''           ''         'T_SAIDA'    'WT_WITHCD'              'Cód.IRF'              '07'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    04  ''           ''         'T_SAIDA'    'WT_SUBJCT'              'Sujeito?'             '08'  ' ' ' ' ' ' ' '  ' ' 'X' ' ' ' ' ' ' ' ',
    05  ''           ''         'T_SAIDA'    'QSREC'                  'Escalão IRF'          '11'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    06  ''           ''         'T_SAIDA'    'WT_WTSTCD'              'Ident.Fiscal IRF'     '07'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    07  ''           ''         'T_SAIDA'    'WT_EXNR'                'No.Certf.Isencao'     '25'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    08  ''           ''         'T_SAIDA'    'WT_EXRT'                'Tx.Isenção'           '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    09  ''           ''         'T_SAIDA'    'WT_EXDF'                'Dt.Inicio Isenção'    '18'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    10  ''           ''         'T_SAIDA'    'WT_EXDT'                'Dt.Final.Isenção'     '18'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    11  ''           ''         'T_SAIDA'    'WT_WTEXRS'              'Motivo Isenção'       '17'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
* w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-coltext     = p_scrtext_l.
* w_fieldcat-scrtext_s   = p_scrtext_l.
* w_fieldcat-scrtext_m   = p_scrtext_l.
* w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
  w_fieldcat-no_out      = p_no_out.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

************************************************************************
*  Module STATUS_0100 OUTPUT
************************************************************************
MODULE status_0100 OUTPUT.

  FREE: ok_code.

  SET PF-STATUS 'PF_0100'.
  SET TITLEBAR 'PF_0100'.

  PERFORM f_init_alv.

ENDMODULE.

************************************************************************
*     Module  USER_COMMAND_0100  INPUT
************************************************************************
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'VOLTAR'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

  FREE: ok_code.

ENDMODULE.

************************************************************************
***********************************************************************
