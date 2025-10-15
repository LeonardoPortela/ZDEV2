*&---------------------------------------------------------------------*
*&  Include           ZAA01O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       PAI
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  get PARAMETER ID 'AN1' field anla-anln1.
  get PARAMETER ID 'AN2' field anla-anln2.
  get PARAMETER ID 'BUK' field anla-bukrs.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.
  IF w_up = 'X'.
    SET PF-STATUS '0110'.
  ELSE.
    SET PF-STATUS '0111'.
  ENDIF.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0110  OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TS 'TABSTRIP_110'. DO NOT CHANGE THIS LINE
*&SPWIZARD: SETS ACTIVE TAB
MODULE tabstrip_110_active_tab_set OUTPUT.
  tabstrip_110-activetab = g_tabstrip_110-pressed_tab.
  IF w_up NE 'X'.
    CASE g_tabstrip_110-pressed_tab.
      WHEN c_tabstrip_110-tab1.
        g_tabstrip_110-subscreen = '0111'.
      WHEN c_tabstrip_110-tab2.
        g_tabstrip_110-subscreen = '0112'.
      WHEN c_tabstrip_110-tab3.
        g_tabstrip_110-subscreen = '0113'.
      WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
    ENDCASE.
  ELSE.
    CASE g_tabstrip_110-pressed_tab.
      WHEN c_tabstrip_110-tab1.
        g_tabstrip_110-subscreen = '0115'.
      WHEN c_tabstrip_110-tab2.
        IF w_ucomm EQ 'CRIA'.
          g_tabstrip_110-subscreen = '0116'.
        ELSE.
          g_tabstrip_110-subscreen = '0117'.
        ENDIF.
      WHEN c_tabstrip_110-tab3.
        g_tabstrip_110-subscreen = '0113'.
      WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
    ENDCASE.
  ENDIF.
ENDMODULE.                    "TABSTRIP_110_ACTIVE_TAB_SET OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_HIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_hip_change_tc_attr OUTPUT.
  DESCRIBE TABLE t_hip LINES tc_hip-lines.
ENDMODULE.                    "TC_HIP_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_HIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_hip_get_lines OUTPUT.
  g_tc_hip_lines = sy-loopc.
ENDMODULE.                    "TC_HIP_GET_LINES OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_DOC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE inicializa_doc OUTPUT.

*data: CT_DOCU type RSFBTLTAB OCCURS 0.

  DATA: lc_line_length   TYPE i   VALUE '72',
        ls_docu          TYPE LINE OF rsfbtltab.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'DOCUEDIT'
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.
  ENDIF.

  IF g_docu_editor IS INITIAL.
    CREATE OBJECT g_docu_editor
      EXPORTING
        parent                     = g_custom_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = lc_line_length
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        OTHERS                     = 1.
    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.
  ENDIF.

* insert contents of table gt_lines into control...
  CLEAR gt_lines[].
  LOOP AT ct_docu INTO ls_docu.
    APPEND ls_docu-tdline TO gt_lines.
  ENDLOOP.

*--set to display resp. change mode
  IF NOT w_up IS INITIAL.
*  IF cs_state-action = swbm_c_op_edit.
*    IF  gt_lines[] IS INITIAL
*    and g_get_template = 'X'.
*      PERFORM get_docu_template   CHANGING gt_lines.
*    ENDIF.
    g_get_template = ' '.

    CALL METHOD g_docu_editor->set_readonly_mode
      EXPORTING
        readonly_mode = cl_gui_textedit=>false
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.
  ELSE.
    CALL METHOD g_docu_editor->set_readonly_mode
      EXPORTING
        readonly_mode = cl_gui_textedit=>true
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.
  ENDIF.

  CALL METHOD g_docu_editor->set_text_as_r3table
    EXPORTING
      table  = gt_lines
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE i062(ctwf) WITH sy-repid 'SET_TEXT_AS_R3TABLE' sy-subrc.
    LEAVE TO SCREEN 0.
  ENDIF.


  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2.
  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

ENDMODULE.                 " INICIALIZA_DOC  OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_HIP_EDIT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_hip_edit_change_tc_attr OUTPUT.
  DESCRIBE TABLE t_hip LINES tc_hip_edit-lines.
ENDMODULE.                    "TC_HIP_EDIT_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_HIP_EDIT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_hip_edit_get_lines OUTPUT.
  g_tc_hip_edit_lines = sy-loopc.
ENDMODULE.                    "TC_HIP_EDIT_GET_LINES OUTPUT
