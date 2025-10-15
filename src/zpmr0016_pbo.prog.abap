*----------------------------------------------------------------------*
* FORM SET_TABNAME
* AUTOR: ENIO JESUS
* 25.11.2015
*----------------------------------------------------------------------*
MODULE ts_set_active_tab OUTPUT.

  CONDENSE: g_tabstrip-qtd1,
            g_tabstrip-qtd2,
            g_tabstrip-qtd3 NO-GAPS.

  itabstrip-activetab = g_ts_0100-pressed_tab.



  CASE g_ts_0100-pressed_tab.
    WHEN 'TAB_DISPONIVEIS'.
      g_ts_0100-subscreen = '0110'.
    WHEN 'TAB_EMPRESTADOS'.
      g_ts_0100-subscreen = '0120'.
    WHEN 'TAB_RESPONSAVEL'.
      g_ts_0100-subscreen = '0130'.
    WHEN OTHERS.
  ENDCASE.

  IF tbx_centro IS NOT INITIAL.
    PERFORM modif_screen_0100_.
  ENDIF.

  CONCATENATE '@3R@ Equipamentos Disponíveis' '(' g_tabstrip-qtd1 ')'
  INTO g_tabstrip-tab1 SEPARATED BY space.

  CONCATENATE '@K4@ Equipamentos Emprestados' '(' g_tabstrip-qtd2 ')'
  INTO g_tabstrip-tab2 SEPARATED BY space.

  CONCATENATE '@PQ@ Em sua Responsabilidade' '(' g_tabstrip-qtd3 ')'
  INTO g_tabstrip-tab3 SEPARATED BY space.
ENDMODULE.                 " SET_TABNAME  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TS_GET_ACTIVE_TAB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ts_get_active_tab INPUT.
  CASE sy-ucomm.
    WHEN 'TAB_DISPONIVEIS'. g_ts_0100-pressed_tab = 'TAB_DISPONIVEIS'.
    WHEN 'TAB_EMPRESTADOS'. g_ts_0100-pressed_tab = 'TAB_EMPRESTADOS'.
    WHEN 'TAB_RESPONSAVEL'. g_ts_0100-pressed_tab = 'TAB_RESPONSAVEL'.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " TS_GET_ACTIVE_TAB  INPUT

*&---------------------------------------------------------------------*
*& FORM MONTA_ALV_TELA_0110                                            *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
  PERFORM modif_screen_0100.

*  IF SY-UCOMM EQ 'ENTER'.
  CASE w_cursor_field.
    WHEN 'TBX_CENTRO'.
      SET CURSOR FIELD 'TBX_EQUIPAMENTO'.
  ENDCASE.
*
*    IF TBX_CENTRO IS NOT INITIAL.
*      SET CURSOR FIELD 'TBX_EQUIPAMENTO'.
*    ENDIF.
*  ENDIF.

ENDMODULE.                    "STATUS_0100 OUTPUT

*----------------------------------------------------------------------*
*  MODULE PBO_0110 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0110 OUTPUT.
  REFRESH it_fcat.
  IF p_eq_sup IS INITIAL.
    PERFORM alv_preenche_cat USING:
     'CBX_EMPRESTAR ' 'Selecione        ' '9'  ''  ''  ''  'X' 'X'  '' '' '' 'X',
     'EQUNR         ' 'Nº Equipamento   ' '30' ''  'X' ''  ' ' ' '  '' '' '' ' ',
*     'ICON_EQ_INF   ' 'Eqpt Inferior?   ' '10' ''  'X' ''  ' ' ' '  '' '' '' ' ',
     'IWERK         ' 'Centro           ' '6'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
     'EQKTX         ' 'Desc. Equipamento' '80' ''  ''  ''  ' ' ' '  '' '' '' ' '.
  ELSE.
    PERFORM alv_preenche_cat USING:
    'CBX_EMPRESTAR ' 'Selecione        ' '9'  ''  ''  ''  'X' 'X'  '' '' '' 'X',
    'EQUNR         ' 'Nº Equipamento   ' '30' ''  'X' ''  ' ' ' '  '' '' '' ' ',
*    'EQ_SUP        ' 'Eqpt Superior?   ' '13' ''  'X' ''  ' ' ' '  '' '' '' ' ',
    'IWERK         ' 'Centro           ' '6'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
    'EQKTX         ' 'Desc. Equipamento' '80' ''  ''  ''  ' ' ' '  '' '' '' ' '.

  ENDIF.

  IF ( obj_custom_0110 IS INITIAL ).
    CREATE OBJECT obj_custom_0110
      EXPORTING
        container_name              = 'CUSTOM_EQUI_DISPONIVEIS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0110
      EXPORTING
        i_parent          = obj_custom_0110
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  wa_layout-grid_title = 'Equipamentos disponiveis para emprestimo!!'.
  wa_layout-cwidth_opt = 'X'.

  "Registra os eventos a serem utilizados
  SET HANDLER:
      lcl_event_handler=>on_data_changed  FOR obj_alv_0110,
      lcl_event_handler=>on_double_click  FOR obj_alv_0110,
      lcl_event_handler=>set_toolbar      FOR obj_alv_0110,
      lcl_event_handler=>get_ucomm        FOR obj_alv_0110.

*  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL  TO GT_EXC_BUTTON.

*  PERFORM EXCLUDE_TB_FUNCTIONS CHANGING IT_EXCLUDE_FCODE.


  CALL METHOD obj_alv_0110->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      it_toolbar_excluding          = gt_exc_button
      i_save                        = 'A'
    CHANGING
      it_fieldcatalog               = it_fcat
      it_outtab                     = it_saida_equi_disponiveis
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0110->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0110->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDMODULE.                    "PBO_0110 OUTPUT

*&---------------------------------------------------------------------*
*& FORM MONTA_ALV_TELA_0120                                            *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

MODULE status_0200 OUTPUT.

  PERFORM modif_screen.
  SET PF-STATUS '0200' EXCLUDING it_ucomm.
  SET TITLEBAR '0200'.

  CASE w_cursor_field.
    WHEN 'TBX_CENTRO_DESTINO'.
*  IF TBX_CENTRO_DESTINO IS NOT INITIAL.
      SET CURSOR FIELD 'TBX_QT_DIAS'.
*  ENDIF.
  ENDCASE.
ENDMODULE.                 " STATUS_0200  OUTPUT

*----------------------------------------------------------------------*
*  MODULE PBO_0120 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0120 OUTPUT.
  REFRESH it_fcat.
  PERFORM alv_preenche_cat USING:
         'EQUNR      ' 'Nº Equip.'         '10'  '' 'X' '' ''  ''  '' '' '' '',
         'EQKTX      ' 'Desc. Equipamento' '40'  '' ''  '' ''  ''  '' '' '' '',
         'CENT_ORIGEM' 'Origem'            '6 '  '' ''  '' ''  ''  '' '' '' '',
         'IWERK      ' 'Destino'           '6 '  '' ''  '' ''  ''  '' '' '' '',
         'ERDAT      ' 'Data'              '10'  '' ''  '' ''  ''  '' '' '' '',
         'QT_DIAS    ' 'Dias'              '4 '  '' ''  '' ''  ''  '' '' '' '',
         'UNAME      ' 'Responsável'       '10'  '' ''  '' ''  ''  '' '' '' ''.
*  'DT_DEVOLUCAO' 'Data de devolução' '17' '' ''  '' 'X' ''  'ZEQUI_EMPRESTIMO' 'ERDAT'         'IT_SAIDA_EQUI_EMPRESTADOS',
*  'HR_DEVOLUCAO' 'Hora de devolução' '17' '' ''  '' 'X' ''  'ZLEST0056'        'HORA_REGISTRO' 'IT_SAIDA_EQUI_EMPRESTADOS'.

  IF ( obj_custom_0120 IS INITIAL ).
    CREATE OBJECT obj_custom_0120
      EXPORTING
        container_name              = 'CUSTOM_EQUI_EMPRESTADOS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0120
      EXPORTING
        i_parent          = obj_custom_0120
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  "Registra os eventos a serem utilizados
  SET HANDLER:
      lcl_event_handler=>on_double_click  FOR obj_alv_0120,
      lcl_event_handler=>set_toolbar      FOR obj_alv_0120.

  wa_layout-grid_title = space.

  APPEND cl_gui_alv_grid=>mc_fc_excl_all  TO gt_exc_button.

  "Registra os eventos a serem utilizados
*  SET HANDLER:
*      LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR OBJ_ALV_0120,
*      LCL_EVENT_HANDLER=>SET_TOOLBAR     FOR OBJ_ALV_0120.

  CALL METHOD obj_alv_0120->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      it_toolbar_excluding          = gt_exc_button
    CHANGING
      it_outtab                     = it_saida_equi_emprestados
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0120->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDMODULE.                    "PBO_0120 OUTPUT

*&---------------------------------------------------------------------*
*& FORM MONTA_ALV_TELA_0130                                            *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*
MODULE pbo_0130 OUTPUT.
  REFRESH it_fcat.

  PERFORM alv_preenche_cat USING:
 'CBX_DEVOLVER      ' 'Selecione'         '9'  '' ''  '' 'X' 'X' '' '' '' 'X',
         'EQUNR      ' 'Nº Equipamento'    '30' '' 'X' '' ''  ''  '' '' '' '',
         'EQKTX      ' 'Desc. Equipamento' '40' '' ''  '' ''  ''  '' '' '' '',
         'CENT_ORIGEM' 'Origem'            '8'  '' ''  '' ''  ''  '' '' '' '',
         'IWERK      ' 'Destino'           '8'  '' ''  '' ''  ''  '' '' '' '',
         'ERDAT      ' 'Data'              '10' '' ''  '' ''  ''  '' '' '' '',
       'QT_DIAS      ' 'Dias'              '6'  '' ''  '' ''  ''  '' '' '' ''.
*    'DT_DEVOLUCAO      ' 'Dt devolução'      '11' '' ''  '' 'X' ''  'ZEQUI_EMPRESTIMO' 'ERDAT'         'IT_SAIDA_EQUI_RESPONSAVEL' '',
*    'HR_DEVOLUCAO      ' 'Hr devolução'      '11' '' ''  '' 'X' ''  'ZLEST0056'        'HORA_REGISTRO' 'IT_SAIDA_EQUI_RESPONSAVEL' ''.

  IF ( obj_custom_0130 IS INITIAL ).
    CREATE OBJECT obj_custom_0130
      EXPORTING
        container_name              = 'CUSTOM_EQUI_RESPONSAVEL'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0130
      EXPORTING
        i_parent          = obj_custom_0130
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.
  CLEAR wa_layout.
  wa_layout-grid_title = 'Equipamentos disponiveis para devolução!!'.
  wa_layout-cwidth_opt = 'X'.
  wa_layout-ctab_fname = 'CELLCOLOR'. " Rubenilson - 24.12.24 - US138088
  wa_stable-col = 'X'.
  wa_stable-row = 'X'.


  "Registra os eventos a serem utilizados
  SET HANDLER:
  lcl_event_handler=>on_double_click FOR obj_alv_0130,
  lcl_event_handler=>on_data_changed FOR obj_alv_0130,
  lcl_event_handler=>set_toolbar     FOR obj_alv_0130,
  lcl_event_handler=>get_ucomm       FOR obj_alv_0130.

*  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL  TO GT_EXC_BUTTON.

  CALL METHOD obj_alv_0130->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      it_toolbar_excluding          = gt_exc_button
    CHANGING
      it_outtab                     = it_saida_equi_responsavel
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0130->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
ENDMODULE.                    "PBO_0130 OUTPUT

*&---------------------------------------------------------------------*
*& FORM MONTA_ALV_TELA_0200                                            *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.
  REFRESH: it_fcat.
  PERFORM orderna_equi_para_transf.
  PERFORM alv_preenche_cat USING:
        'EQUNR'         'Nº Equipamento'             '10' '' 'X' '' ''  ''  '' '' '' '',
        'IWERK'         'Centro'                     '06' '' ''  '' ''  ''  '' '' '' '',
        'EQKTX'         'Desc. Equipamento'          '40' '' ''  '' ''  ''  '' '' '' '',
        'CBX_ORD_ABAST' 'Gerar Ordem Abastecimento?' '23' '' ''  '' ''  'X' '' '' '' '',
        'CBX_ORD_REMON' 'Gerar Ordem Remonta?'       '20' '' ''  '' ''  'X' '' '' '' ''.
*** Inicio - Rubenilson - 23.12.24 - US138088
  IF p_temp IS NOT INITIAL.
    PERFORM alv_preenche_cat USING 'DEVOLUCAO_AUTOMATICA' 'Devolução Automática' '20' '' ''  '' ''  'X' '' '' '' ''.
  ENDIF.
*** Fim - Rubenilson - 23.12.24 - US138088

  IF ( obj_custom_0200 IS INITIAL ).
    CREATE OBJECT obj_custom_0200
      EXPORTING
        container_name              = 'CUSTOM_EMPRESTIMO_EQUI'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

*  WA_LAYOUT-SEL_MODE = 'A'.
    wa_layout-excp_conds    = 'X'.
    wa_layout-grid_title    = space.
    wa_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
    wa_layout-stylefname = 'CELLTAB'."Rubenilson - 23.12.24 - US138088
    wa_layout-edit = abap_true."Rubenilson - 23.12.24 - US138088

    CREATE OBJECT obj_alv_0200
      EXPORTING
        i_parent          = obj_custom_0200
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.


    SET HANDLER: lcl_event_handler=>on_data_changed FOR obj_alv_0200.
*               LCL_EVENT_HANDLER=>SET_TOOLBAR     FOR OBJ_ALV_0200.

    APPEND cl_gui_alv_grid=>mc_fc_excl_all  TO gt_exc_button.

    CALL METHOD obj_alv_0200->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        it_toolbar_excluding          = gt_exc_button
      CHANGING
        it_outtab                     = it_saida_emprestimo_equi
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    wa_stable-col = 'X'.
    wa_stable-row = 'X'.

    CALL METHOD obj_alv_0200->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  ENDIF.

  CALL METHOD obj_alv_0200->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDMODULE.                    "PBO_0200 OUTPUT





*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS '0300'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0300  OUTPUT

*----------------------------------------------------------------------*
*  MODULE PBO_0300 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.
  REFRESH: it_fcat.
  IF p_eq_sup IS INITIAL.
    PERFORM alv_preenche_cat USING:
    'CHECK' 'No-out'    ' ' '' '' '' 'X' '' '' '' '' 'X',
    'STATUS' 'Status' '4' '' 'X' '' '' '' '' '' '' '',  "RJF - US 158036-26-11-2024-#158036-RJF
    'EQUNR' 'Inferior' '10' '' 'X' '' '' '' '' '' '' '',
    'EQKTX' 'Des eqpto inferior' '33' '' '' '' '' '' '' '' '' '',
    'HEQUI' 'Superior' '10' '' 'X' '' '' '' '' '' '' '',
    'EQKTX_' 'Des eqpto superior' '30' '' '' '' '' '' '' '' '' '',
    'DET' 'Detalhes Status' '30' '' '' '' '' '' '' '' '' ''. "RJF - US 158036-26-11-2024-#158036-RJF
  ELSE.
    PERFORM alv_preenche_cat USING:
    'CHECK' 'No-out'    ' ' '' '' '' 'X' '' '' '' '' 'X',
    'STATUS' 'Status' '4' '' 'X' '' '' '' '' '' '' '',  "RJF - US 158036-26-11-2024-#158036-RJF
    'HEQUI' 'Superior' '10' '' 'X' '' '' '' '' '' '' '',
    'EQKTX_' 'Des eqpto superior' '33' '' '' '' '' '' '' '' '' '',
    'EQUNR' 'Inferior' '10' '' 'X' '' '' '' '' '' '' '',
    'EQKTX' 'Des eqpto inferior' '30' '' '' '' '' '' '' '' '' '',
    'DET' 'Detalhes Status' '30' '' '' '' '' '' '' '' '' ''. "RJF - US 158036-26-11-2024-#158036-RJF

  ENDIF.

  IF ( obj_custom_0300 IS INITIAL ).
    CREATE OBJECT obj_custom_0300
      EXPORTING
        container_name              = 'CUSTOM_STATUS_EQUNR'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_status
      EXPORTING
        i_parent          = obj_custom_0300
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  wa_layout-grid_title = 'Hierarquia dos Equipamentos'.

  "Registra os eventos a serem utilizados
  SET HANDLER:
      lcl_event_handler=>set_toolbar     FOR obj_alv_status,
      lcl_event_handler=>get_ucomm       FOR obj_alv_status,
      lcl_event_handler=>on_double_click FOR obj_alv_status.

  CALL METHOD obj_alv_status->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      it_toolbar_excluding          = gt_exc_button
    CHANGING
      it_outtab                     = it_status_equnr
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
ENDMODULE.                 " PBO_0300  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0300 INPUT.
  CASE sy-ucomm.
    WHEN 'OK'.
*      LEAVE TO SCREEN 0200.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " PAI_0300  INPUT
