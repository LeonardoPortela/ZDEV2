DATA: ctn_0001 TYPE REF TO cl_gui_custom_container,
      alv_0001 TYPE REF TO cl_gui_alv_grid,
      ctl_0001 TYPE lvc_t_fcat,
      lay_0001 TYPE lvc_s_layo.

DATA: it_logs TYPE TABLE OF zglt059 WITH HEADER LINE.

*----------------------------------------------------------------------*
***INCLUDE ZGL019_0001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  IF alv_0001 IS NOT INITIAL.
    CALL METHOD alv_0001->free.
  ENDIF.

  IF ctn_0001 IS NOT INITIAL.
    CALL METHOD ctn_0001->free.
  ENDIF.

  CLEAR: alv_0001, ctn_0001.

  CLEAR: it_logs[].

  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0001  INPUT


*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TL0001' WITH tg_saida-saknr.

  IF it_logs[] IS INITIAL.
    SELECT * INTO TABLE it_logs
      FROM zglt059
     WHERE bukrs EQ tg_saida-bukrs
       AND saknr EQ tg_saida-saknr
       AND monat IN p_monat
       AND gjahr EQ p_gjahr
      ORDER BY dt_liberacao hr_liberacao.
  ENDIF.

  IF ctn_0001 IS INITIAL.

    CREATE OBJECT ctn_0001
      EXPORTING
        container_name = 'ALV_LOGS'.

    CREATE OBJECT alv_0001
      EXPORTING
        i_parent = ctn_0001.

    CLEAR: ctl_0001.

    PERFORM z_estrutura_fieldcat TABLES ctl_0001 USING:
        'IT_LOGS' 'BUKRS'         text-001       ' ' 01 08 space space space space space space space space, "'Empresa'
        'IT_LOGS' 'SAKNR'         text-018       ' ' 02 12 space space space space space space space space, "'Conta'
        'IT_LOGS' 'MONAT'         text-019       ' ' 03 05 space space space space space space space space, "'Mês'
        'IT_LOGS' 'GJAHR'         text-048       ' ' 04 05 space space space space space space space space, "'Ano'
        'IT_LOGS' 'DEP_RESP'      text-049       ' ' 05 05 space space space space space space space space, "'Depart.'
        'IT_LOGS' 'DT_LIBERACAO'  text-050       ' ' 06 12 space space space space space space space space, "'Dt. Liberação'
        'IT_LOGS' 'HR_LIBERACAO'  text-051       ' ' 07 12 space space space space space space space space, "'Hr. Liberação'
        'IT_LOGS' 'BN_LIBERACAO'  text-052       ' ' 08 15 space space space space space space space space, "'Usuário'
        'IT_LOGS' 'NIVEL'         text-053       ' ' 09 05 space space space space space space space space, "'Nível'
        'IT_LOGS' 'CK_RECUSA'     text-054       ' ' 10 05 space space space space space space space space, "'Recusa'
        'IT_LOGS' 'STATUS_LIB'    text-017       ' ' 11 05 space space space space space space space space, "'Status'
        'IT_LOGS' 'CK_ULTIMO_LOG' text-055       ' ' 12 05 space space space space space space space space. "'Último Log'

    CLEAR: lay_0001.
    lay_0001-zebra    = abap_true.
    lay_0001-sel_mode = 'A'.
    lay_0001-zebra    = abap_true.

    CALL METHOD alv_0001->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = lay_0001
      CHANGING
        it_fieldcatalog = ctl_0001
        it_outtab       = it_logs[].
  ENDIF.

  CALL METHOD alv_0001->refresh_table_display.

ENDMODULE.                 " STATUS_0001  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  Z_ESTRUTURA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_CATALOGO     text
*      -->P_TAB_NAME      text
*      -->P_FIELDNAME     text
*      -->P_TEXTO_GRANDE  text
*      -->P_HOT           text
*      -->P_POSICAO       text
*      -->P_OUTPUTLEN     text
*      -->P_FIX_COLUMN    text
*      -->P_CONVEXIT      text
*      -->P_DO_SUM        text
*      -->P_ICON          text
*      -->P_JUST          text
*      -->P_EMPHASIZE     text
*      -->P_EDIT          text
*      -->P_CHECKBOX      text
*----------------------------------------------------------------------*
FORM z_estrutura_fieldcat TABLES it_catalogo TYPE lvc_t_fcat
                           USING p_tab_name
                                 p_fieldname
                                 p_texto_grande
                                 p_hot
                                 p_posicao
                                 p_outputlen
                                 p_fix_column
                                 p_convexit
                                 p_do_sum
                                 p_icon
                                 p_just
                                 p_emphasize
                                 p_edit
                                 p_checkbox.

  DATA: wa_catalog TYPE lvc_s_fcat.
  wa_catalog-tabname     = p_tab_name.
  wa_catalog-fieldname   = p_fieldname.
  wa_catalog-scrtext_l   = p_texto_grande.
  wa_catalog-scrtext_m   = p_texto_grande.
  wa_catalog-scrtext_s   = p_texto_grande.
  wa_catalog-hotspot     = p_hot.
  wa_catalog-col_pos     = p_posicao.
  wa_catalog-outputlen   = p_outputlen.
  wa_catalog-fix_column  = p_fix_column.
  wa_catalog-convexit    = p_convexit.
  wa_catalog-do_sum      = p_do_sum.
  wa_catalog-icon        = p_icon.
  wa_catalog-just        = p_just.
  wa_catalog-emphasize   = p_emphasize.
  wa_catalog-edit        = p_edit.
  wa_catalog-checkbox    = p_checkbox.
  APPEND wa_catalog TO it_catalogo.

ENDFORM.                    " Z_ESTRUTURA_FIELDCAT
