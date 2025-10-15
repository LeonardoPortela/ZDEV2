*&---------------------------------------------------------------------*
*&  Include           ZAA01I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  SET PARAMETER ID 'AN1' FIELD anla-anln1.
  SET PARAMETER ID 'AN2' FIELD anla-anln2.
  SET PARAMETER ID 'BUK' FIELD anla-bukrs.

  w_ucomm = sy-ucomm(4).
  CLEAR w_up.
  IF w_ucomm = 'CRIA'.
    w_up = 'X'.
  ENDIF.

  CLEAR w_save.
  w_bukrs = anla-bukrs.
  w_anln1 = anla-anln1.
  w_anln2 = anla-anln2.

  SELECT SINGLE butxt FROM t001
    INTO wt_butxt
    WHERE bukrs = w_bukrs.

  CASE sy-ucomm(4).
    WHEN 'BACK'.
      LEAVE PROGRAM .
    WHEN 'EXIB' OR 'CRIA' OR 'ENTE'.
      PERFORM f_busca_dados_tela.
      g_tabstrip_110-pressed_tab = c_tabstrip_110-tab1.
      CALL SCREEN 110.
    WHEN 'EXCL'.
      PERFORM f_apaga_registros.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.

  PERFORM f_busca_descricoes USING zaa_controle_doc-pais
                                   zaa_controle_doc-estado
                                   zaa_controle_doc-estado_com
                                   zaa_controle_doc-feins.

  CASE sy-ucomm.
    WHEN 'BACK'.
*      LEAVE TO SCREEN 100.
      PERFORM f_voltar.
    WHEN 'SAVE'.
      PERFORM f_salva_dados.
    WHEN 'MODI' OR 'MOD'.
      IF w_up IS INITIAL.
        w_up = 'X'.
      ELSE.
        CLEAR w_up.
      ENDIF.
    WHEN 'IMOB' OR 'IMAB'.
      SET PARAMETER ID 'AN1' FIELD w_anln1.
      SET PARAMETER ID 'AN2' FIELD w_anln1.
      SET PARAMETER ID 'BUK' FIELD w_bukrs.
      IF w_up IS INITIAL.
        CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
      ELSE.
        break abap.
        CALL TRANSACTION 'AS02' AND SKIP FIRST SCREEN.
        WAIT UP TO 1 SECONDS.
        SELECT SINGLE txt50 txa50" ANLHTXT
          FROM anla INTO (w_txt50, w_txa50)
          WHERE bukrs = w_bukrs AND
                anln1 = w_anln1 AND
                anln2 = w_anln2.
        IF sy-subrc <> 0.
          MESSAGE i001(aa) WITH w_anln1 w_anln2 w_bukrs.
          LEAVE SCREEN.
        ELSE.

          SELECT SINGLE anlhtxt FROM anlh
            INTO w_anlhtxt
            WHERE bukrs = w_bukrs AND
                  anln1 = w_anln1.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0110  INPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TABSTRIP_110'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tabstrip_110_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tabstrip_110-tab1.
      g_tabstrip_110-pressed_tab = c_tabstrip_110-tab1.
    WHEN c_tabstrip_110-tab2.
      g_tabstrip_110-pressed_tab = c_tabstrip_110-tab2.
    WHEN c_tabstrip_110-tab3.
      g_tabstrip_110-pressed_tab = c_tabstrip_110-tab3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TABSTRIP_110_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0112  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0116 INPUT.

  CASE sy-ucomm.
    WHEN 'MAIS'.
      ADD 1 TO w_buzei.
      zaa_controle_hip-bukrs = w_bukrs.
      zaa_controle_hip-anln1 = w_anln1.
      zaa_controle_hip-anln2 = w_anln2.
      zaa_controle_hip-buzei = w_buzei.
      APPEND zaa_controle_hip TO t_hip.
      CLEAR zaa_controle_hip.
    WHEN 'CLEAR'.
      CLEAR zaa_controle_hip.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0112  INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_HIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_hip_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_HIP'
                              'T_HIP'
                              ' '
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TC_HIP_USER_COMMAND INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0113  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0113 INPUT.

  DATA: ls_line       TYPE LINE OF tywf_tline,
        l_docu       TYPE LINE OF rsfbtltab,
        lt_docu       TYPE rsfbtltab.

  REFRESH lt_docu.

  IF w_up EQ 'X'.
    CALL METHOD g_docu_editor->get_text_as_r3table
      IMPORTING
        table  = gt_lines
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      MESSAGE i062(ctwf) WITH sy-repid 'GET_TEXT_AS_R3TABLE' sy-subrc.
      EXIT.
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.
    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    LOOP AT gt_lines INTO ls_line.
      l_docu-tdformat = '/'.
      l_docu-tdline   = ls_line.
      APPEND l_docu TO lt_docu.
    ENDLOOP.

    IF lt_docu[] <> ct_docu[].
      ct_docu[] = lt_docu[].
      w_save = 'X'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0113  INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_HIP_EDIT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc_hip_edit_modify INPUT.
  MODIFY t_hip
    INDEX tc_hip_edit-current_line.

  READ TABLE t_hip INDEX tc_hip_edit-current_line.
  t_hip-bukrs = w_bukrs.
  t_hip-anln1 = w_anln1.
  MODIFY t_hip
    INDEX tc_hip_edit-current_line.
ENDMODULE.                    "TC_HIP_EDIT_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC_HIP_EDIT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc_hip_edit_mark INPUT.
  DATA: g_tc_hip_edit_wa2 LIKE LINE OF t_hip.
  IF tc_hip_edit-line_sel_mode = 1
  AND t_hip-flag = 'X'.
    LOOP AT t_hip INTO g_tc_hip_edit_wa2
      WHERE flag = 'X'.
      g_tc_hip_edit_wa2-flag = ''.
      MODIFY t_hip
        FROM g_tc_hip_edit_wa2
        TRANSPORTING flag.
    ENDLOOP.
  ENDIF.
  MODIFY t_hip
    INDEX tc_hip_edit-current_line
    TRANSPORTING flag.
ENDMODULE.                    "TC_HIP_EDIT_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_HIP_EDIT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_hip_edit_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_HIP_EDIT'
                              'T_HIP'
                              'FLAG'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TC_HIP_EDIT_USER_COMMAND INPUT
*&---------------------------------------------------------------------*
*&      Module  CONTROLE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE controle INPUT.
  w_save = 'X'.
ENDMODULE.                 " CONTROLE  INPUT
