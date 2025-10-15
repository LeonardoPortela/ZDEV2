*&---------------------------------------------------------------------*
*& FORM CHECAR_ORDENS_MANUTENC                                         *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

FORM valida_ordens_manut.

  DATA: it_fleet  TYPE TABLE OF fleet,
        ls_imptt  TYPE imptt,
        wa_fleet  TYPE fleet,
        text      TYPE char200,
        lv_answer TYPE c."Rubenilson - 24.12.24 - US138088

  CLEAR: wa_saida_emprestimo_equi, it_fleet,
         wa_fleet, return_status.

*** Inicio - Rubenilson - 24.12.24 - US138088
  DATA(lt_equi) = it_saida_emprestimo_equi.
  DELETE lt_equi WHERE devolucao_automatica IS INITIAL.
  IF lt_equi IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = 'Existem registros com devolução automática, deseja prosseguir?'
        text_button_1  = 'Sim'
        text_button_2  = 'Não'
      IMPORTING
        answer         = lv_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc = 0.
      IF lv_answer <> '1'.
        return_status = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
*** Fim - Rubenilson - 24.12.24 - US138088

  LOOP AT it_saida_emprestimo_equi INTO wa_saida_emprestimo_equi.

    wa_saida_emprestimo_equi-equnr = |{ wa_saida_emprestimo_equi-equnr ALPHA = IN }|.
* Captura os dados do veículo;
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_fleet
      FROM fleet AS a
     INNER JOIN equi AS b ON b~equnr = wa_saida_emprestimo_equi-equnr
                         AND a~objnr = b~objnr
      WHERE key_num NE ' ' OR tq_combustivel_1 NE '0'.

*   Verifica se o tanque de combustível não possuí valor, e se ordem de abastecimento foi marcado
*   então é necessário entrar com um valor para tq combustível, pois só assim é possível gerar uma
*   ordem de abastecimento;
    READ TABLE it_fleet INTO wa_fleet WITH KEY objnr+2 = wa_saida_emprestimo_equi-equnr.

    IF wa_fleet IS INITIAL.
      CHECK wa_saida_emprestimo_equi-cbx_ord_abast = 'X'.
      MESSAGE i836(sd) WITH TEXT-022 wa_saida_emprestimo_equi-equnr TEXT-023 TEXT-024.
      return_status = 'X'.
      EXIT.
*   Verifica se o tanque de combustível possuí um valor, e se ordem de abastecimento não foi marcado
*   então é necessário criar uma ordem de abastecimento;
*    ELSE.
*      CHECK WA_SAIDA_EMPRESTIMO_EQUI-CBX_ORD_ABAST = ''.
*      MESSAGE I836(SD) WITH TEXT-022 WA_SAIDA_EMPRESTIMO_EQUI-EQUNR TEXT-025 TEXT-026.
*      RETURN_STATUS = 'X'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "VALIDA_ORDENS_MANUT

*&---------------------------------------------------------------------*
*& FORM CHECAR_INFO_DESTINO                                            *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

FORM valida_info_destino.

  CLEAR: loc_instalacao, wa_saida_emprestimo_equi.
  DATA: r_zuteis TYPE REF TO zuteis.
  CREATE OBJECT r_zuteis.

  DATA: it_values TYPE TABLE OF rgsb4,
        wa_values TYPE rgsb4.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      level           = 0
      setnr           = 'MAGGI_CENTROS_MODULO_PM'
      table           = 'T001W'
      class           = '0000'
      no_descriptions = 'X'
      no_rw_info      = 'X'
      fieldname       = 'WERKS'
    TABLES
      set_values      = it_values
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  READ TABLE it_saida_emprestimo_equi INTO wa_saida_emprestimo_equi WITH KEY
             iwerk = tbx_centro_destino.

  IF sy-subrc IS INITIAL.
    MESSAGE w836(sd) WITH TEXT-046 TEXT-047.

    return_status = 'X'.
    EXIT.
  ENDIF.

* Checa se o centro de planejamento informado é válido
  READ TABLE it_values INTO wa_values WITH KEY
             from = tbx_centro_destino.

  IF sy-subrc IS INITIAL.

    "Valida local de instalação para transferencia de recurso.
    CLEAR: loc_instalacao.
    r_zuteis->z_seleciona_local_tranf(
      EXPORTING
        werks = CONV #( tbx_centro_destino )
      IMPORTING
        local = loc_instalacao
    ).

    IF loc_instalacao IS INITIAL.
*  Concatena local de instalação do centro ( Ex.: "1801.FRO" )
*      CONCATENATE tbx_centro_destino '.FRO' INTO loc_instalacao.

      MESSAGE e836(sd) WITH TEXT-052  tbx_centro_destino.
      return_status = 'X'.
      EXIT.

    ENDIF.


  ELSE.
    MESSAGE w836(sd) WITH TEXT-006.
    return_status = 'X'.
  ENDIF.

  IF p_defi IS INITIAL.
    IF tbx_qt_dias IS INITIAL.
      MESSAGE e836(sd) WITH TEXT-051 .
      return_status = 'X'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& FORM ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat    USING: p_campo         TYPE c
                                p_desc          TYPE c
                                p_tam           TYPE c
                                p_hot           TYPE c
                                p_zero          TYPE c
                                p_sum           TYPE c
                                p_edit          TYPE c
                                p_check         TYPE c
                                p_ref_tabname   LIKE dd02d-tabname
                                p_ref_fieldname LIKE dd03d-fieldname
                                p_tabname       LIKE dd02d-tabname
                                p_no_out        TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.
  CLEAR: wa_layout, wl_fcat.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-edit      = p_edit.
  wl_fcat-checkbox  = p_check.
  wl_fcat-ref_table = p_ref_tabname.
  wl_fcat-ref_field = p_ref_fieldname.
  wl_fcat-tabname   = p_ref_tabname.
  wl_fcat-no_out    = p_no_out.
  APPEND wl_fcat TO it_fcat.
ENDFORM.                    "ALV_PREENCHE_CAT

FORM alv_preenche_cat_dev    USING: p_campo         TYPE c
                                p_desc          TYPE c
                                p_tam           TYPE c
                                p_hot           TYPE c
                                p_zero          TYPE c
                                p_sum           TYPE c
                                p_edit          TYPE c
                                p_check         TYPE c
                                p_ref_tabname   LIKE dd02d-tabname
                                p_ref_fieldname LIKE dd03d-fieldname
                                p_tabname       LIKE dd02d-tabname
                                p_no_out        TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.
  CLEAR: wl_fcat, ls_layout.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-edit      = p_edit.
  wl_fcat-checkbox  = p_check.
  wl_fcat-ref_table = p_ref_tabname.
  wl_fcat-ref_field = p_ref_fieldname.
  wl_fcat-tabname   = p_ref_tabname.
  wl_fcat-no_out    = p_no_out.

  APPEND wl_fcat TO it_fcat.
ENDFORM.                    "ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  EXIBIR_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exibir_equip.
  GET CURSOR FIELD w_cursor_field.
  CASE w_cursor_field.
    WHEN 'TBX_EQUIPAMENTO'."Ordem Manuteção
      IF tbx_equipamento IS NOT INITIAL.
        SET PARAMETER ID 'EQN' FIELD tbx_equipamento.
        CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXIBIR_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM shdb_exibir_plan .

  IF tbx_equipamento IS NOT INITIAL.
*    SET PARAMETER ID 'EQN' FIELD TBX_EQUIPAMENTO.
*    CALL TRANSACTION 'IP18' AND SKIP FIRST SCREEN .

    FREE ti_bdcdata[].
    FREE it_msg[].

    PERFORM f_bdc_data USING:

   '        ' '     ' 'T'   'IP18       ' 'BS AA X   F   ',
   'RIMPOS00' ' 1000' 'X'   '           ' '              ',
   '        ' '     ' ' '   'BDC_CURSOR ' 'EQUNR-LOW     ',
   '        ' '     ' ' '   'BDC_OKCODE ' '=ONLI         ',
   '        ' '     ' ' '   'EQUNR-LOW  ' tbx_equipamento ,
   '        ' '     ' ' '   'SPERRE     ' 'X             ',
   '        ' '     ' ' '   'ABREA      ' 'X             ',
   '        ' '     ' ' '   'STATA      ' 'X             ',
   '        ' '     ' ' '   'VARIANT    ' '/PLS          '.
*   'SAPMSSY0' ' 0120' 'X'   '           ' '             ',
*   '        ' '     ' ' '   ' BDC_CURSOR' '04/03        ',
*   '        ' '     ' ' '   ' BDC_OKCODE' '=BACK        ',
*   'RIMPOS00' ' 1000' 'X'   '           ' '             ',
*   '        ' '     ' ' '   ' BDC_OKCODE' '/EECAN       ',
*   '        ' '     ' ' '   ' BDC_CURSOR' 'MITYP-LOW    '.

    CLEAR p_erro.
    PERFORM zf_call_transaction USING 'IP18' CHANGING p_erro.

    IF p_erro IS NOT INITIAL.
      MESSAGE TEXT-049 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    MESSAGE TEXT-048 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1397   text
*      -->P_1398   text
*      -->P_1399   text
*      -->P_1400   text
*      -->P_1401   text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.

  APPEND VALUE #(
                program   = p_program
                dynpro    = p_dynpro
                dynbegin  = p_start
                fnam      = p_fnam
                fval      = p_fval
  ) TO ti_bdcdata.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0495   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM zf_call_transaction USING p_trans CHANGING p_erro.

  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  DATA: wl_cont    TYPE sy-tabix,
        wl_mode(1).

*  FREE IT_MSG .
  CLEAR wl_mode.
  CLEAR wl_cont.
  CLEAR it_msg.
  wl_mode = 'E'.

  CALL TRANSACTION p_trans USING ti_bdcdata
                           MODE wl_mode
                           MESSAGES INTO it_msg.
  COMMIT WORK.
  WAIT UP TO 2 SECONDS.

  CLEAR: wl_cont.

  IF line_exists( it_msg[ msgtyp = 'A' ] ).
    p_erro = abap_true.
  ELSE.
    IF line_exists( it_msg[ msgtyp = 'E' ] ).
      p_erro = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0130  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0130 INPUT.
  IF sy-ucomm EQ 'BTN_CHECK_EQ_INF'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'TL_400'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'BT_CONF'.

      DATA: r_iniciar_proc_zbapis TYPE REF TO zbapis,
            r_finish_proc_zbapis  TYPE REF TO z_seleciona_dados,
            obj_dev               TYPE REF TO z_seleciona_dados.

      DATA(obj_main) = NEW zbapis( ).
      CREATE OBJECT: obj_dev.

      obj_main->z_iniciar_processo_devolucao( ).

      IF vg_erro IS INITIAL.

        obj_dev->z_atualiza_tela_respons( ).
      ENDIF.


    WHEN 'BT_CANC'.
      FREE it_saida_dev_equi.
      FREE it_status_equnr.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0400 OUTPUT.
  CLEAR: p_dev_por, p_data_dev, p_hora_dev, wa_layout.
  p_dev_por = sy-uname.
  p_data_dev = sy-datum.
  p_hora_dev = sy-uzeit.

  REFRESH it_fcat.
  CLEAR ls_layout.
  PERFORM alv_preenche_cat_dev USING:
        'CBX_DEVOLVER      ' 'Selecione'         '9'  '' ''  '' 'X' 'X' '' '' '' 'X',
           'EQUNR      ' 'Nº Equipamento'    '14' '' 'X' '' ''  ''  '' '' '' '',
           'EQKTX      ' 'Desc. Equipamento' '20' '' ''  '' ''  ''  '' '' '' '',
           'CENT_ORIGEM' 'Origem'            '6'  '' ''  '' ''  ''  '' '' '' '',
           'IWERK      ' 'Destino'           '7'  '' ''  '' ''  ''  '' '' '' '',
           'ERDAT      ' 'Data'              '10' '' ''  '' ''  ''  '' '' '' '',
         'QT_DIAS      ' 'Dias'              '6'  '' ''  '' ''  ''  '' '' '' ''.
*    'DT_DEVOLUCAO      ' 'Dt devolução'      '11' '' ''  '' 'X' ''  'ZEQUI_EMPRESTIMO' 'ERDAT'         'IT_SAIDA_EQUI_RESPONSAVEL' '',
*    'HR_DEVOLUCAO      ' 'Hr devolução'      '11' '' ''  '' 'X' ''  'ZLEST0056'        'HORA_REGISTRO' 'IT_SAIDA_EQUI_RESPONSAVEL' ''.

  IF ( obj_custom_0400 IS INITIAL ).
    CREATE OBJECT obj_custom_0400
      EXPORTING
        container_name              = 'CUSTOM_DEV_EQUI'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.


    CREATE OBJECT obj_alv_0400
      EXPORTING
        i_parent          = obj_custom_0400
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.


*    LS_LAYOUT-SEL_MODE = ''.
  ls_layout-no_rowmark = 'X'.
  ls_layout-grid_title = space.
  ls_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela

*  WA_STABLE-COL = ' '.
*  WA_STABLE-ROW = ' '.

  SET HANDLER: lcl_event_handler=>on_data_changed FOR obj_alv_0400.
*               LCL_EVENT_HANDLER=>SET_TOOLBAR     FOR OBJ_ALV_0400.

  APPEND cl_gui_alv_grid=>mc_fc_excl_all  TO gt_exc_button.

  CALL METHOD obj_alv_0400->set_table_for_first_display
    EXPORTING
      is_layout                     = ls_layout
      it_toolbar_excluding          = gt_exc_button
    CHANGING
      it_outtab                     = it_saida_dev_equi
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0400->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0400->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_LUPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2495   text
*      -->P_WA_SAIDA_DEV_EQUI_EQUNR  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM f_lupa USING p_msg1 p_msg2 p_msg3.
  DATA: vl_message(150) TYPE c.
  CLEAR vl_message.

  p_msg2 = |{ p_msg2 ALPHA = OUT }|.
  CONCATENATE p_msg1 p_msg2 p_msg3 INTO vl_message SEPARATED BY space.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 99
      text       = vl_message.

ENDFORM. "f_lupa

*&---------------------------------------------------------------------*
*&      Form  F_LUPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2495   text
*      -->P_WA_SAIDA_DEV_EQUI_EQUNR  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM f_lupa_ USING p_msg1 p_msg2 p_msg3.
  DATA: vl_message(150) TYPE c.
  CLEAR vl_message.

  p_msg2 = |{ p_msg2 ALPHA = OUT }|.
  CONCATENATE p_msg1 p_msg2 p_msg3 INTO vl_message SEPARATED BY space.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 99
      text       = vl_message.
ENDFORM. "f_lupa

*&---------------------------------------------------------------------*
*&      Form  SHDB_MOD_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_ORDERID  text
*      -->P_AT_NUMERO_ORDEM  text
*----------------------------------------------------------------------*
FORM shdb_mod_ordem  USING    p_funct_loc
                              p_at_numero_ordem.

*  DATA: P_RESP, CHECK, P_ERRO(1).
*  DATA: TI_BDCDATA TYPE STANDARD TABLE OF BDCDATA,
*        WA_BDCDATA LIKE LINE OF TI_BDCDATA.


  CLEAR: wa_bdcdata, ti_bdcdata, it_msg.
  FREE: ti_bdcdata, ti_bdcdata[], it_msg[].



  PERFORM f_bdc_data USING:

'          '  '    '    'T '     '             '   'BS AA X   F                                           ',
'SAPLCOIH '   '0101'    'X '     '             '   '                                                       ',
'         '   '    '    '  '     '             '   '                                                       ' ,
'         '   '    '    '  '     'BDC_CURSOR  '   'CAUFVD-AUFNR',
'         '   '    '    '  '     'BDC_OKCODE  '   ' /00                                                   ',
'         '   '    '    '  '     'CAUFVD-AUFNR'    p_at_numero_ordem,
'SAPLCOIH '   '3000'    'X '     '             '   '                                                       ',
'         '   '    '    '  '     '             '   '                                                       ',
'         '   '    '    '  '     'BDC_OKCODE  '   '=BU                                                   ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                3001SUB_ALL   ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                1100SUB_LEVEL ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                1102SUB_KOPF  ' ,
*'         '   '    '    '  '     'CAUFVD-KTEXT'   ' Ordem de abastecimento                                ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                1105SUB_BTN   ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                1104SUB_TEXT  ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                1120SUB_AUFTRAG ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLIPAR                                0415SUB_ADRESSE ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLIPAR                                0415SUB_ADDR_PM ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                0154HEADER      ' ,
*'         '   '    '    '  '     ' CAUFVD-INGPR'   ' ABS                                                   ' ,
*'         '   '    '    '  '     ' CAUFVD-VAPLZ'   ' OFICINA                                               ' ,
*'         '   '    '    '  '     ' CAUFVD-VAWRK'   ' 1507                                                  ' ,
*'         '   '    '    '  '     ' CAUFVD-ILART'   ' Z11                                                   ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                0153MAINORDER   ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLIPAR                                0421PARTNER     ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7400SUB_PM_ADDR ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7402SUB_PM_ADDR_BTN ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7300TERM        ' ,
*'         '   '    '    '  '     'CAUFVD-GSTRP'   ' 19.12.2018                                              ' ,
*'         '   '    '    '  '     'CAUFVD-PRIOK'   ' 4                                                       ' ,
*'         '   '    '    '  '     ' CAUFVD-GLTRP'   ' 19.12.2018                                              ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7301SUB_BTN     ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7310SUB_ADD     ' ,
'         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7100OBJECT      ' ,
*'         '   '    '    '  '     'BDC_CURSOR  '   p_funct_loc,  "FF - 10.04.2024 - #137726
'         '   '    '    '  '     'BDC_CURSOR  '   'CAUFVD-TPLNR', "FF - 10.04.2024 - #137726
  '         '   '    '    '  '   'CAUFVD-TPLNR'   p_funct_loc ,
*'         '   '    '    '  '     'CAUFVD-EQUNR'   ' 1157085                                                 ' ,
  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                0815NOTIFICATION_DATA ' ,
  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOI0                                0310AVO         ' ,
*'         '   '    '    '  '     ' AFVGD-LTXA1 '   ' Ordem de abastecimento                                  ' ,
*'         '   '    '    '  '     ' AFVGD-INDET '   ' 1                                                       ' ,
*'         '   '    '    '  '     ' AFVGD-ARBPL '   ' OFICINA                                                 ' ,
*'         '   '    '    '  '     ' AFVGD-WERKS '   ' 1507                                                    ' ,
*'         '   '    '    '  '     ' AFVGD-STEUS '   ' PM01                                                    ' ,
*'         '   '    '    '  '     ' AFVGD-ARBEH '   ' H                                                       ' ,
*  '         '   '    '    '  '     'BDC_OKCODE  '   ' =ENTR                                                            ',
*'         '   '    '    '  '     ' AFVGD-DAUNE '   ' H                                                       ' ,
  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                0153SUB_SERVICE '.
*  'SAPLCOIH '   '3000'    'X '     '             '   '                                                         ',
*  '         '   '    '    '  '     '             '   '                                                         ',
*  '         '   '    '    '  '     'BDC_OKCODE  '   '=BU                                                     ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                3001SUB_ALL     ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                1100SUB_LEVEL   ' ,
*  '         '   '    '    '  '     'BDC_OKCODE  '   ' =ENTR                                                            ',
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                1102SUB_KOPF    ' ,
**  '         '   '    '    '  '     'BDC_CURSOR  '   'CAUFVD-KTEXT                                            ' ,
**'         '   '    '    '  '     ' CAUFVD-KTEXT'   ' Ordem de abastecimento                                  ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                1105SUB_BTN     ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                1104SUB_TEXT    ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                1120SUB_AUFTRAG ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLIPAR                                0415SUB_ADRESSE ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLIPAR                                0415SUB_ADDR_PM ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                0154HEADER      ' ,
**'         '   '    '    '  '     ' CAUFVD-INGPR'   ' ABS                                                     ' ,
**'         '   '    '    '  '     ' CAUFVD-VAPLZ'   ' OFICINA                                                 ' ,
**'         '   '    '    '  '     ' CAUFVD-VAWRK'   ' 1507                                                    ' ,
**'         '   '    '    '  '     ' CAUFVD-ILART'   ' Z11                                                     ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                0153MAINORDER   ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLIPAR                                0421PARTNER     ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7400SUB_PM_ADDR ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7402SUB_PM_ADDR_BTN ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7300TERM        ' ,
**'         '   '    '    '  '     ' CAUFVD-GSTRP'   ' 19.12.2018                                              ' ,
**'         '   '    '    '  '     ' CAUFVD-PRIOK'   ' 4                                                       ' ,
**'         '   '    '    '  '     ' CAUFVD-GLTRP'   ' 19.12.2018                                              ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7301SUB_BTN     ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7310SUB_ADD     ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                7100OBJECT      ' ,
*  '         '   '    '    '  '     'CAUFVD-TPLNR'   P_FUNCT_LOC,
**'         '   '    '    '  '     ' CAUFVD-EQUNR'   ' 1157085                                                 ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                0815NOTIFICATION_DATA ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOI0                                0310AVO         ' ,
**'         '   '    '    '  '     ' AFVGD-LTXA1 '   ' Ordem de abastecimento                                  ' ,
**'         '   '    '    '  '     ' AFVGD-INDET '   ' 1                                                       ' ,
**'         '   '    '    '  '     ' AFVGD-ARBPL '   ' OFICINA                                                 ' ,
**'         '   '    '    '  '     ' AFVGD-WERKS '   ' 1507                                                    ' ,
**'         '   '    '    '  '     ' AFVGD-STEUS '   ' PM01                                                    ' ,
**'         '   '    '    '  '     ' AFVGD-ARBEH '   ' H                                                       ' ,
**'         '   '    '    '  '     ' AFVGD-DAUNE '   ' H                                                       ' ,
*  '         '   '    '    '  '     'BDC_SUBSCR  '   'SAPLCOIH                                0153SUB_SERVICE ' .

  CLEAR p_erro.
  DATA(lv_tcode) = sy-tcode.

  EXPORT lv_tcode FROM lv_tcode TO MEMORY ID 'ZPM0026'. "Export para ser importado na exit ZXWocu07

  PERFORM zf_call_transaction USING 'IW32' CHANGING p_erro.

ENDFORM.

FORM shdb_modif_notif USING p_notif.


  CLEAR: wa_bdcdata, ti_bdcdata, it_msg.
  FREE: ti_bdcdata, ti_bdcdata[], it_msg[].

  PERFORM f_bdc_data USING:

'SAPLIQS0'  '100'   'X' ''  '',
''  ''  ' ' 'BDC_CURSOR'    'RIWO00-QMNUM',
''  ''  ' ' 'BDC_OKCODE'    '/00',
''  ''  ' ' 'RIWO00-QMNUM'  p_notif,
'SAPLIQS0'  '7200'  'X' ''  '',
''  ''  ' ' 'BDC_OKCODE'    '/00',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                1050SCREEN_1',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                1051NOTIF_TYPE',
*''  ''  ' ' 'VIQMEL-QMTXT'  'Ordem de remonta',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLQM07                                3000ACTION-BOX',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7235SUB_GROUP_10',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7212CUSTOM_SCREEN',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7322SUBSCREEN_1',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIWO1                                0100OBJEKT',
''  ''  ' ' 'BDC_CURSOR'    'RIWO1-TPLNR',
''  ''  ' ' 'RIWO1-TPLNR'   '',
*''  ''  ' ' 'RIWO1-EQUNR'   '901812',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7710SUBSCREEN_2',
*''  ''  ' ' 'VIQMEL-QMGRP'  'F0000010',
*''  ''  ' ' 'VIQMEL-QMCOD'  '20',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7326SUBSCREEN_3',
*''  ''  ' ' 'VIQMEL-INGRP'  'ABS',
*''  ''  ' ' 'VIQMEL-IWERK'  '189',
*''  ''  ' ' 'RIWO00-GEWRK'  'OFICINA',
*''  ''  ' ' 'RIWO00-SWERK'  '189',
*''  ''  ' ' 'VIQMEL-QMNAM'  'FFONSECA',
*''  ''  ' ' 'VIQMEL-QMDAT'  '11.04.2024',
*''  ''  ' ' 'VIQMEL-MZEIT'  '0,534108796296296',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIPAR                                0400INTPAR',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIPAR                                0450VERA',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7328SUBSCREEN_4',
*''  ''  ' ' 'VIQMEL-AUSVN'  '11.04.2024',
*''  ''  ' ' 'VIQMEL-AUZTV'  '0,534166666666667',
*''  ''  ' ' 'VIQMEL-AUZTB'  '0',
*''  ''  ' ' 'VIQMEL-MAUEH'  'H',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7324SUBSCREEN_5',
'SAPLIQS0'  '7200'  'X' ''  '',
''  ''  ' ' 'BDC_OKCODE'    '=BUCH',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                1050SCREEN_1',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                1051NOTIF_TYPE',
''  ''  ' ' 'BDC_CURSOR'    'VIQMEL-QMTXT',
*''  ''  ' ' 'VIQMEL-QMTXT'  'Ordem de remonta',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLQM07                                3000ACTION-BOX',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7235SUB_GROUP_10',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7212CUSTOM_SCREEN',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7322SUBSCREEN_1',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIWO1                                0100OBJEKT'.
*''  ''  ' ' 'RIWO1-TPLNR'   'MAGI.001.0189.FROT.FR005',
*''  ''  ' ' 'RIWO1-EQUNR'   '901812',
*''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7710SUBSCREEN_2',
*''  ''  ' ' 'VIQMEL-QMGRP'  'F0000010',
*''  ''  ' ' 'VIQMEL-QMCOD'  '20',
*''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7326SUBSCREEN_3',
*''  ''  ' ' 'VIQMEL-INGRP'  'ABS',
*''  ''  ' ' 'VIQMEL-IWERK'  '189',
*''  ''  ' ' 'RIWO00-GEWRK'  'OFICINA',
*''  ''  ' ' 'RIWO00-SWERK'  '189',
*''  ''  ' ' 'VIQMEL-QMNAM'  'FFONSECA',
*''  ''  ' ' 'VIQMEL-QMDAT'  '11.04.2024',
*''  ''  ' ' 'VIQMEL-MZEIT'  '0,534108796296296',
*''  ''  ' ' 'BDC_SUBSCR'    'SAPLIPAR                                0400INTPAR',
*''  ''  ' ' 'BDC_SUBSCR'    'SAPLIPAR                                0450VERA',
*''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7328SUBSCREEN_4',
*''  ''  ' ' 'VIQMEL-AUSVN'  '11.04.2024',
*''  ''  ' ' 'VIQMEL-AUZTV'  '0,534166666666667',
*''  ''  ' ' 'VIQMEL-AUZTB'  '0',
*''  ''  ' ' 'VIQMEL-MAUEH'  'H',
*''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7324SUBSCREEN_5'.


  CLEAR p_erro.
  PERFORM zf_call_transaction USING 'IW22' CHANGING p_erro.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_DADOS_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM sel_dados_equip  USING p_row p_column.

  FREE clicks.
  ADD 1 TO clicks.

  TRY .
      DATA(wa_saida) = it_saida_equi_disponiveis[ p_row ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  FREE it_exib_equip.
  it_exib_equip = it_saida_equi_disponiveis.

  CASE  p_column.
    WHEN 'EQUNR'.
      READ TABLE it_exib_equip ASSIGNING FIELD-SYMBOL(<w_disponivel>) WITH KEY equnr = wa_saida-equnr.

      IF sy-subrc = 0.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING        "TITLEBAR = 'Confirmar'
            text_question         = 'Deseja acessar qual visão?'
            text_button_1         = 'Exibir equipamentos'
            text_button_2         = 'Exibir Planos Manut'
            display_cancel_button = 'X' "VALUE(DISPLAY_CANCEL_BUTTON) DEFAULT 'X'
          IMPORTING
            answer                = p_resp.


        CASE p_resp.
          WHEN '1'.
            SET PARAMETER ID 'EQN' FIELD <w_disponivel>-equnr.
            CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .

          WHEN '2'.
            FREE: ti_bdcdata[], ti_bdcdata, it_msg, it_msg[].

            PERFORM f_bdc_data USING:

           '        ' '     ' 'T'   'IP18       ' 'BS AA X   F   ',
           'RIMPOS00' ' 1000' 'X'   '           ' '              ',
           '        ' '     ' ' '   'BDC_CURSOR ' 'EQUNR-LOW     ',
           '        ' '     ' ' '   'BDC_OKCODE ' '=ONLI         ',
           '        ' '     ' ' '   'EQUNR-LOW  ' <w_disponivel>-equnr ,
           '        ' '     ' ' '   'SPERRE     ' 'X             ',
           '        ' '     ' ' '   'ABREA      ' 'X             ',
           '        ' '     ' ' '   'STATA      ' 'X             ',
           '        ' '     ' ' '   'VARIANT    ' '/PLS          '.


            CLEAR p_erro.
            PERFORM zf_call_transaction USING 'IP18' CHANGING p_erro.

            IF p_erro IS NOT INITIAL.
              MESSAGE TEXT-049 TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.

          WHEN OTHERS.
            LEAVE TO SCREEN 0100.
        ENDCASE.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_DADOS_EQUIP_EMPREST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM sel_dados_equip_emprest  USING    p_e_row
                                       p_e_column_fieldname.

  FREE clicks.
  ADD 1 TO clicks.

  TRY .
      DATA(wa_saida) = it_saida_equi_emprestados[ p_e_row ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  FREE gt_saida_equi_emprestados.
  gt_saida_equi_emprestados = it_saida_equi_emprestados.

  CASE  p_e_column_fieldname.
    WHEN 'EQUNR'.
      READ TABLE gt_saida_equi_emprestados ASSIGNING FIELD-SYMBOL(<w_disponivel>) WITH KEY equnr = wa_saida-equnr.

      IF sy-subrc = 0.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING        "TITLEBAR = 'Confirmar'
            text_question         = 'Deseja acessar qual visão?'
            text_button_1         = 'Exibir equipamentos'
            text_button_2         = 'Exibir Planos Manut'
            display_cancel_button = 'X' "VALUE(DISPLAY_CANCEL_BUTTON) DEFAULT 'X'
          IMPORTING
            answer                = p_resp.


        CASE p_resp.
          WHEN '1'.
            SET PARAMETER ID 'EQN' FIELD <w_disponivel>-equnr.
            CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .

          WHEN '2'.
            FREE: ti_bdcdata[], ti_bdcdata, it_msg, it_msg[].

            PERFORM f_bdc_data USING:

           '        ' '     ' 'T'   'IP18       ' 'BS AA X   F   ',
           'RIMPOS00' ' 1000' 'X'   '           ' '              ',
           '        ' '     ' ' '   'BDC_CURSOR ' 'EQUNR-LOW     ',
           '        ' '     ' ' '   'BDC_OKCODE ' '=ONLI         ',
           '        ' '     ' ' '   'EQUNR-LOW  ' <w_disponivel>-equnr ,
           '        ' '     ' ' '   'SPERRE     ' 'X             ',
           '        ' '     ' ' '   'ABREA      ' 'X             ',
           '        ' '     ' ' '   'STATA      ' 'X             ',
           '        ' '     ' ' '   'VARIANT    ' '/PLS          '.


            CLEAR p_erro.
            PERFORM zf_call_transaction USING 'IP18' CHANGING p_erro.

            IF p_erro IS NOT INITIAL.
              MESSAGE TEXT-049 TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.

          WHEN OTHERS.
            LEAVE TO SCREEN 0100.
        ENDCASE.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_DADOS_EQUIP_RESPOSAV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM sel_dados_equip_resposav  USING    p_row
                                        p_column.


  FREE clicks.
  ADD 1 TO clicks.

  TRY .
      DATA(wa_saida) = it_saida_equi_responsavel[ p_row ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  FREE gt_saida_equi_responsavel.
  gt_saida_equi_responsavel = it_saida_equi_responsavel.

  CASE  p_column.
    WHEN 'EQUNR'.
      READ TABLE gt_saida_equi_responsavel ASSIGNING FIELD-SYMBOL(<w_disponivel>) WITH KEY equnr = wa_saida-equnr.

      IF sy-subrc = 0.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING        "TITLEBAR = 'Confirmar'
            text_question         = 'Deseja acessar qual visão?'
            text_button_1         = 'Exibir equipamentos'
            text_button_2         = 'Exibir Planos Manut'
            display_cancel_button = 'X' "VALUE(DISPLAY_CANCEL_BUTTON) DEFAULT 'X'
          IMPORTING
            answer                = p_resp.


        CASE p_resp.
          WHEN '1'.
            SET PARAMETER ID 'EQN' FIELD <w_disponivel>-equnr.
            CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .

          WHEN '2'.
            FREE: ti_bdcdata[], ti_bdcdata, it_msg, it_msg[].

            PERFORM f_bdc_data USING:

           '        ' '     ' 'T'   'IP18       ' 'BS AA X   F   ',
           'RIMPOS00' ' 1000' 'X'   '           ' '              ',
           '        ' '     ' ' '   'BDC_CURSOR ' 'EQUNR-LOW     ',
           '        ' '     ' ' '   'BDC_OKCODE ' '=ONLI         ',
           '        ' '     ' ' '   'EQUNR-LOW  ' <w_disponivel>-equnr ,
           '        ' '     ' ' '   'SPERRE     ' 'X             ',
           '        ' '     ' ' '   'ABREA      ' 'X             ',
           '        ' '     ' ' '   'STATA      ' 'X             ',
           '        ' '     ' ' '   'VARIANT    ' '/PLS          '.


            CLEAR p_erro.
            PERFORM zf_call_transaction USING 'IP18' CHANGING p_erro.

            IF p_erro IS NOT INITIAL.
              MESSAGE TEXT-049 TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.

          WHEN OTHERS.
            LEAVE TO SCREEN 0100.
        ENDCASE.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_TOOLBAR_EXCLUDING  text
*----------------------------------------------------------------------*
FORM exclude_tb_functions CHANGING pt_exclude TYPE ui_functions .
  DATA ls_exclude TYPE ui_func.
  CLEAR ls_exclude.

*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_MAXIMUM .
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_MINIMUM .
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUBTOT .
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUM .
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_AVERAGE .
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_MB_SUM .
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_MB_SUBTOT .
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.

  ls_exclude = cl_gui_alv_grid=>mc_fc_filter .
  APPEND ls_exclude TO pt_exclude.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIF_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modif_screen .

  LOOP AT SCREEN.
    IF p_defi EQ abap_true.
      IF screen-name EQ 'TBX_QT_DIAS' OR
      screen-name EQ 'LB_QTD_DIAS'.

        screen-invisible = 1.
        screen-active    = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM modif_screen_0100 .

  LOOP AT SCREEN.
    IF tbx_centro IS INITIAL .
      IF screen-name EQ 'QUADRO_002' OR
        screen-name EQ 'P_DEFI' OR
        screen-name EQ 'P_TEMP'.
        screen-invisible = 1.
        screen-active    = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIF_SCREEN_0100_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modif_screen_0100_ .

  LOOP AT SCREEN.
    IF g_ts_0100-subscreen NE '0110'.
      IF screen-name EQ 'QUADRO_002' OR
        screen-name EQ 'P_DEFI' OR
        screen-name EQ 'P_TEMP'.
        screen-invisible = 1.
        screen-active    = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REL_VIS_EPTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TX_CENTRO  text
*----------------------------------------------------------------------*
FORM rel_vis_epto  USING    p_tx_centro.

  DATA: r_equip TYPE RANGE OF equi-equnr.
  DATA: t_equip TYPE TABLE OF equz.

  FREE: lt_equi.

  SELECT *
  FROM equz
  INTO TABLE @DATA(t_eq_dip)
  WHERE iwerk EQ @p_tx_centro
  AND datbi EQ '99991231'.

  CHECK t_equz IS NOT INITIAL.

  SELECT *
  FROM zequi_emprestimo
  INTO TABLE @DATA(t_cen_orig)
  WHERE cent_origem EQ @tbx_centro.

  SELECT *
  FROM zequi_emprestimo
  INTO TABLE @DATA(t_cen_dest)
  WHERE iwerk EQ @tbx_centro.


  LOOP AT t_cen_orig ASSIGNING FIELD-SYMBOL(<w_origem>).
    APPEND VALUE #( sign    = 'I' option  = 'EQ' low   = <w_origem>-equnr  ) TO r_equip.
  ENDLOOP.

  SORT t_eq_dip ASCENDING BY equnr.
  DELETE t_eq_dip WHERE equnr IN r_equip.
  CLEAR: r_equip.

  SELECT *
  FROM equz AS a
  INNER JOIN v_equi AS b ON b~equnr EQ a~equnr
  INTO CORRESPONDING FIELDS OF TABLE t_equip
    WHERE a~datbi EQ '99991231'
      AND b~bukrs EQ '0015'
      AND b~eqtyp IN ( 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 - ins


  CHECK t_equip IS NOT INITIAL.
  SORT t_equip ASCENDING BY equnr.
  DELETE ADJACENT DUPLICATES FROM t_equip COMPARING equnr.

  SELECT *
  FROM eqkt
  INTO TABLE @DATA(t_eqkt)
    FOR ALL ENTRIES IN @t_equip
    WHERE equnr EQ @t_equip-equnr.

  LOOP AT t_equip ASSIGNING FIELD-SYMBOL(<w_equ>).

    READ TABLE t_eq_dip ASSIGNING FIELD-SYMBOL(<w_eq_disp>) WITH KEY equnr = <w_equ>-equnr.
    IF sy-subrc EQ 0.
      fs_equip-equnr   = <w_eq_disp>-equnr.
      fs_equip-origem  = <w_eq_disp>-iwerk.
      fs_equip-destino = ' '.
      fs_equip-status  = 'Disponivél'.
    ENDIF.

    READ TABLE t_cen_orig ASSIGNING FIELD-SYMBOL(<w_eq_orig>) WITH KEY equnr = <w_equ>-equnr.
    IF sy-subrc EQ 0.
      fs_equip-equnr   = <w_eq_orig>-equnr.
      fs_equip-origem  = <w_eq_orig>-cent_origem.
      fs_equip-destino = <w_eq_orig>-iwerk.
      fs_equip-status  = 'Emprestado'.
    ENDIF.

    READ TABLE t_cen_dest ASSIGNING FIELD-SYMBOL(<w_eq_dest>) WITH KEY equnr = <w_equ>-equnr.
    IF sy-subrc EQ 0.
      fs_equip-equnr   = <w_eq_dest>-equnr.
      fs_equip-origem  = <w_eq_dest>-cent_origem.
      fs_equip-destino = <w_eq_dest>-iwerk.
      fs_equip-status  = 'Em sua responsabilidade'.
    ENDIF.

    CHECK fs_equip IS NOT INITIAL.

    READ TABLE t_eqkt INTO DATA(w_eqkt) WITH KEY equnr = <w_equ>-equnr.
    IF sy-subrc EQ 0.
      fs_equip-eqktx = w_eqkt-eqktx.
    ENDIF.

    APPEND fs_equip TO lt_equi.
    CLEAR: fs_equip.
  ENDLOOP.

  CHECK lt_equi IS NOT INITIAL.
  SORT lt_equi ASCENDING BY status equnr.
  FREE: t_cen_dest, t_cen_orig, t_eq_dip.
  CLEAR: r_equip.

  CALL SCREEN 0500.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  SET PF-STATUS 'TL0500'.
  SET TITLEBAR 'TIT0500'.

  PERFORM alv_preenche_cat_500 USING:
          'EQUNR'     'Nº eqpto           '  '10' ' ' 'X'  '' ''  ''  '' '' '' '',
          'EQKTX'     'Desc eqpto         '  '40' ' ' ''  '' ''  ''  '' '' '' '',
          'ORIGEM'    'Centro origem      '  '04' ' ' ''  '' ''  ''  '' '' '' '',
          'DESTINO'   'Centro Destrino    '  '04' ' ' ''  '' ''  '' '' '' '' '',
          'STATUS'    'Status equipamento '  '40' ' ' ''  '' ''  '' '' '' '' ''.


  IF ( obj_custom_0500 IS INITIAL ).
    CREATE OBJECT obj_custom_0500
      EXPORTING
        container_name              = 'CUSTOM_REL_EQUI'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    wa_layout_500-excp_conds    = 'X'.
    wa_layout_500-grid_title    = space.
    wa_layout_500-cwidth_opt    = 'X'.     "  Otimizar colunas na tela


    CREATE OBJECT obj_alv_0500
      EXPORTING
        i_parent          = obj_custom_0500
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  CALL METHOD obj_alv_0500->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout_500
      it_toolbar_excluding          = gt_exc_button
    CHANGING
      it_outtab                     = lt_equi
      it_fieldcatalog               = lt_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0500->refresh_table_display
    EXPORTING
      is_stable = wa_stable_500.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT_500
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2372   text
*      -->P_2373   text
*      -->P_2374   text
*      -->P_2375   text
*      -->P_2376   text
*      -->P_2377   text
*      -->P_2378   text
*      -->P_2379   text
*      -->P_2380   text
*      -->P_2381   text
*      -->P_2382   text
*      -->P_2383   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat_500  USING: p_campo         TYPE c
                                p_desc          TYPE c
                                p_tam           TYPE c
                                p_hot           TYPE c
                                p_zero          TYPE c
                                p_sum           TYPE c
                                p_edit          TYPE c
                                p_check         TYPE c
                                p_ref_tabname   LIKE dd02d-tabname
                                p_ref_fieldname LIKE dd03d-fieldname
                                p_tabname       LIKE dd02d-tabname
                                p_no_out        TYPE c.

  DATA: wl_fcat_500 TYPE lvc_s_fcat.
  CLEAR: wl_fcat_500.
  wl_fcat_500-fieldname = p_campo.
  wl_fcat_500-scrtext_l = p_desc.
  wl_fcat_500-scrtext_m = p_desc.
  wl_fcat_500-scrtext_s = p_desc.
  wl_fcat_500-hotspot   = p_hot.
  wl_fcat_500-no_zero   = p_zero.
  wl_fcat_500-outputlen = p_tam.
  wl_fcat_500-edit      = p_edit.
  wl_fcat_500-checkbox  = p_check.
  wl_fcat_500-ref_table = p_ref_tabname.
  wl_fcat_500-ref_field = p_ref_fieldname.
  wl_fcat_500-tabname   = p_ref_tabname.
  wl_fcat_500-no_out    = p_no_out.
  APPEND wl_fcat_500 TO lt_fcat.

ENDFORM.
