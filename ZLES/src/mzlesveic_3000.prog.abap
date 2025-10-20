*----------------------------------------------------------------------*
***INCLUDE MZLESVEIC_3000 .
*----------------------------------------------------------------------*
DATA: it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row.

DATA: it_exclude_3000 TYPE ui_functions,
      wa_exclude_3000 LIKE LINE OF it_exclude_3000.

DATA: ctl_alv_3000       TYPE REF TO cl_gui_alv_grid,
      ctl_con_3000       TYPE REF TO cl_gui_custom_container,
      gs_lay_3000        TYPE lvc_s_layo,
      gs_var_3000        TYPE disvariant,
      gs_scroll_col_3000 TYPE lvc_s_col,
      gs_scroll_row_3000 TYPE lvc_s_roid,
      it_catalog_3000    TYPE lvc_t_fcat,
      it_card_select     TYPE TABLE OF zlest0002_card WITH HEADER LINE.

* types
TYPES:
  t_fieldcat TYPE slis_fieldcat_alv,
  t_events   TYPE slis_alv_event,
  t_layout   TYPE slis_layout_alv.

DATA:
  w_fieldcat TYPE t_fieldcat,
  w_events   TYPE t_events,
  w_layout   TYPE t_layout.

DATA:
  i_fieldcat   TYPE STANDARD TABLE OF t_fieldcat,
  i_exc_button TYPE slis_t_extab,
  l_program    TYPE sy-repid.


*&---------------------------------------------------------------------*
*&      Module  STATUS_3000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_3000 OUTPUT.

  FIELD-SYMBOLS: <fs_card> TYPE lvc_s_fcat.

  IF ctl_con_3000 IS INITIAL.

    CREATE OBJECT ctl_con_3000
      EXPORTING
        container_name = 'ALV_CARD'.

    CREATE OBJECT ctl_alv_3000
      EXPORTING
        i_parent = ctl_con_3000.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZLEST0002_CARD'
      CHANGING
        ct_fieldcat      = it_catalog_3000.

    gs_var_3000-report   = sy-repid.
    gs_lay_3000-sel_mode = 'A'.
    gs_lay_3000-zebra    = abap_true.

    LOOP AT it_catalog_3000 ASSIGNING <fs_card>.
      CASE <fs_card>-fieldname.
        WHEN 'PC_VEICULO'.
          <fs_card>-outputlen = 10.
        WHEN 'TP_CARD_PED'.
          <fs_card>-outputlen = 05.
        WHEN 'NR_CARD_PED'.
          <fs_card>-outputlen = 30.
        WHEN 'CK_CARD_PADRAO'.
          <fs_card>-outputlen = 10.
      ENDCASE.
    ENDLOOP.

    CALL METHOD ctl_alv_3000->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_3000
        is_variant           = gs_var_3000
        i_default            = space
        it_toolbar_excluding = it_exclude_3000
      CHANGING
        it_fieldcatalog      = it_catalog_3000
        it_outtab            = it_cards[].

    CALL METHOD ctl_alv_3000->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_3000->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_3000->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_3000
      es_row_no   = gs_scroll_row_3000.

  SET PF-STATUS 'PFCARDPED'.
  SET TITLEBAR 'TLCARDPED'.

ENDMODULE.                 " STATUS_3000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_3000_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_3000_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_3000 INPUT.
  CASE ok_code.
    WHEN 'NOVO'.
      CLEAR: wa_cards.
      wa_cards-pc_veiculo = wa_veiculo_tela-pc_veiculo.
      CALL SCREEN 3001 STARTING AT 15 20.
    WHEN 'DELETE'.
      IF it_card_select[] IS INITIAL.
        MESSAGE s080.
        RETURN.
      ENDIF.
      LOOP AT it_card_select.
        DELETE FROM zlest0002_card WHERE pc_veiculo  EQ it_card_select-pc_veiculo
                                     AND tp_card_ped EQ it_card_select-tp_card_ped.
      ENDLOOP.
      COMMIT WORK.
      PERFORM pesquisar_cards.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_3000  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info INPUT.

  CALL METHOD ctl_alv_3000->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_3000
      es_row_no   = gs_scroll_row_3000.

ENDMODULE.                 " GET_SCROLL_INFO  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows INPUT.

  CLEAR it_selected_rows.

  CALL METHOD ctl_alv_3000->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CLEAR it_card_select[].

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_cards INTO wa_cards INDEX wa_selected_rows-index.
    MOVE-CORRESPONDING wa_cards TO it_card_select.
    APPEND it_card_select.
  ENDLOOP.

ENDMODULE.                 " GET_SELECTED_ROWS  INPUT

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_CARDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pesquisar_cards .
  SELECT * INTO TABLE it_cards FROM zlest0002_card WHERE pc_veiculo EQ wa_veiculo_tela-pc_veiculo.
ENDFORM.                    " PESQUISAR_CARDS


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_3001_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_3001_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_3001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_3001 OUTPUT.

  SET PF-STATUS 'PFCARDINC'.
  SET TITLEBAR 'TLCARDINC'.

ENDMODULE.                 " STATUS_3001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_3001 INPUT.

  CASE ok_code.
    WHEN 'SALVAR'.
      PERFORM salvar_card.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_3001  INPUT

*&---------------------------------------------------------------------*
*&      Form  SALVAR_CARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_card .

  IF wa_cards-ck_card_padrao EQ abap_true.
    UPDATE zlest0002_card SET ck_card_padrao = space
     WHERE pc_veiculo EQ wa_cards-pc_veiculo.
  ENDIF.

  MODIFY zlest0002_card FROM wa_cards.

  PERFORM pesquisar_cards.

  LEAVE TO SCREEN 0.

ENDFORM.                    " SALVAR_CARD

*&---------------------------------------------------------------------*
*&      Form  FM_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_log.
  DATA: t_zlest0216 TYPE TABLE OF zlest0216.
  FREE: t_zlest0216.
  SELECT  *
  FROM zlest0216
  INTO TABLE t_zlest0216
  WHERE pc_veiculo EQ b_placa-low.

  PERFORM build_fieldcatlog_log.
  PERFORM build_layout_log.

  l_program = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = l_program
      it_excluding          = i_exc_button
      is_layout             = w_layout
      it_fieldcat           = i_fieldcat
      i_save                = 'A'
      i_grid_title          = 'Log Modificações.'
      i_screen_start_column = 10
      i_screen_start_line   = 20
      i_screen_end_column   = 120
      i_screen_end_line     = 40
    TABLES
      t_outtab              = t_zlest0216
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATLOG_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatlog_log .
  CLEAR: i_fieldcat.
  PERFORM build_fcatalog_log USING:
           'PC_VEICULO '  'ZLEST0216' 'Placa veículo             .',
           'DATA_MODIF '  'ZLEST0216' 'Data modificação',
           'HORA_MODIF '  'ZLEST0216' 'Hora modificação',
           'CAMPO      '  'ZLEST0216' 'Campo Técnico Modificado',
           'VALOR_ANT  '  'ZLEST0216' 'Valor original modificado',
           'VALOR_ATUAL'  'ZLEST0216' 'Valor atual',
           'USUARIO    '  'ZLEST0216' 'Nome do usuário'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCATALOG_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0438   text
*      -->P_0439   text
*      -->P_0440   text
*----------------------------------------------------------------------*
FORM build_fcatalog_log USING l_field l_tab l_text.

  w_fieldcat-fieldname      = l_field.
  w_fieldcat-tabname        = l_tab.
  w_fieldcat-seltext_m      = l_text.

  APPEND w_fieldcat TO i_fieldcat.
  CLEAR w_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout_log .
  CLEAR: w_layout.
  w_layout-colwidth_optimize = 'X'.
  w_layout-zebra             = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SALVE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZLEST0002  text
*----------------------------------------------------------------------*
FORM fm_salve_log  USING p_zlest0002 TYPE zlest0002.
  FREE: it_zlest0216.
  IF p_zlest0002-pc_veiculo NE wa_zlest0002_mem-pc_veiculo.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'PC_VEICULO'
    valor_ant =  wa_zlest0002_mem-pc_veiculo
    valor_atual = p_zlest0002-pc_veiculo
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-country  NE wa_zlest0002_mem-country.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'COUNTRY'
    valor_ant =  wa_zlest0002_mem-country
    valor_atual = p_zlest0002-country
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-cd_uf NE wa_zlest0002_mem-cd_uf.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'CD_UF'
    valor_ant =  wa_zlest0002_mem-cd_uf
    valor_atual = p_zlest0002-cd_uf
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-taxjurcode NE wa_zlest0002_mem-taxjurcode.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'TAXJURCODE'
    valor_ant =  wa_zlest0002_mem-taxjurcode
    valor_atual = p_zlest0002-taxjurcode
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-pstlz NE wa_zlest0002_mem-pstlz.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'PSTLZ'
    valor_ant =  wa_zlest0002_mem-pstlz
    valor_atual = p_zlest0002-pstlz
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-chassi NE wa_zlest0002_mem-chassi.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'CHASSI'
    valor_ant =  wa_zlest0002_mem-chassi
    valor_atual = p_zlest0002-chassi
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-marca NE wa_zlest0002_mem-marca.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'MARCA'
    valor_ant =  wa_zlest0002_mem-marca
    valor_atual = p_zlest0002-marca
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-cor NE wa_zlest0002_mem-cor.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'COR'
    valor_ant =  wa_zlest0002_mem-cor
    valor_atual = p_zlest0002-cor
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-cd_renavam NE wa_zlest0002_mem-cd_renavam.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'CD_RENAVAM'
    valor_ant =  wa_zlest0002_mem-cd_renavam
    valor_atual = p_zlest0002-cd_renavam
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-modelo NE wa_zlest0002_mem-modelo.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'MODELO'
    valor_ant =  wa_zlest0002_mem-modelo
    valor_atual = p_zlest0002-modelo
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-ano NE wa_zlest0002_mem-ano.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'ANO'
    valor_ant =  wa_zlest0002_mem-ano
    valor_atual = p_zlest0002-ano
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-ct_veiculo NE wa_zlest0002_mem-ct_veiculo.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'CT_VEICULO'
    valor_ant =  wa_zlest0002_mem-ct_veiculo
    valor_atual = p_zlest0002-ct_veiculo
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-tara  NE wa_zlest0002_mem-tara.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'TARA '
    valor_ant =  wa_zlest0002_mem-tara
    valor_atual = p_zlest0002-tara
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-tp_carroceria NE wa_zlest0002_mem-tp_carroceria.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'TP_CARROCERIA'
    valor_ant =  wa_zlest0002_mem-tp_carroceria
    valor_atual = p_zlest0002-tp_carroceria
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-cap_kg NE wa_zlest0002_mem-cap_kg.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'CAP_KG'
    valor_ant =  wa_zlest0002_mem-cap_kg
    valor_atual = p_zlest0002-cap_kg
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-tp_veiculo NE wa_zlest0002_mem-tp_veiculo.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'TP_VEICULO'
    valor_ant =  wa_zlest0002_mem-tp_veiculo
    valor_atual = p_zlest0002-tp_veiculo
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-cap_m3 NE wa_zlest0002_mem-cap_m3.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'CAP_M3'
    valor_ant =  wa_zlest0002_mem-cap_m3
    valor_atual = p_zlest0002-cap_m3
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-tp_rodado NE wa_zlest0002_mem-tp_rodado.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'TP_RODADO'
    valor_ant =  wa_zlest0002_mem-tp_rodado
    valor_atual = p_zlest0002-tp_rodado
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-qt_eixo NE wa_zlest0002_mem-qt_eixo.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'QT_EIXO'
    valor_ant =  wa_zlest0002_mem-qt_eixo
    valor_atual = p_zlest0002-qt_eixo
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-tp_carroceria2 NE wa_zlest0002_mem-tp_carroceria2.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'TP_CARROCERIA2'
    valor_ant =  wa_zlest0002_mem-tp_carroceria2
    valor_atual = p_zlest0002-tp_carroceria2
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-st_bloqueio  NE wa_zlest0002_mem-st_bloqueio .
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'ST_BLOQUEIO '
    valor_ant =  wa_zlest0002_mem-st_bloqueio
    valor_atual = p_zlest0002-st_bloqueio
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-agregado NE wa_zlest0002_mem-agregado.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'AGREGADO'
    valor_ant =  wa_zlest0002_mem-agregado
    valor_atual = p_zlest0002-agregado
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-cto_comodato NE wa_zlest0002_mem-cto_comodato.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'CTO_COMODATO'
    valor_ant =  wa_zlest0002_mem-cto_comodato
    valor_atual = p_zlest0002-cto_comodato
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-irregularidade NE wa_zlest0002_mem-irregularidade.
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'IRREGULARIDADE'
    valor_ant =  wa_zlest0002_mem-irregularidade
    valor_atual = p_zlest0002-irregularidade
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

  IF p_zlest0002-status  NE wa_zlest0002_mem-status .
    APPEND VALUE #(
    pc_veiculo = p_zlest0002-pc_veiculo
    data_modif = sy-datum
    hora_modif = sy-uzeit
    campo = 'STATUS '
    valor_ant =  wa_zlest0002_mem-status
    valor_atual = p_zlest0002-status
    usuario = sy-uname ) TO it_zlest0216.
  ENDIF.

*  IF p_zlest0002-OBSERVACOES NE wa_zlest0002_mem-OBSERVACOES.
*    APPEND VALUE #(
*    pc_veiculo = p_zlest0002-pc_veiculo
*    data_modif = sy-datum
*    hora_modif = sy-uzeit
*    campo = 'OBSERVACOES'
*    valor_ant =  wa_zlest0002_mem-pc_veiculo
*    valor_atual = p_zlest0002-pc_veiculo
*    usuario = sy-uname ) TO it_zlest0216.
*  ENDIF.


  IF it_zlest0216 IS NOT INITIAL.
    MODIFY zlest0216 FROM TABLE it_zlest0216.
    COMMIT WORK.
  ENDIF.

ENDFORM.
