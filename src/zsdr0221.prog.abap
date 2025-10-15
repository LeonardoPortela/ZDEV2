* ==================================================================== *
*                         © AMAGGI                                     *
* ==================================================================== *
* Program.....: ZSDR0221                                               *
* Title.......: Cadastro de Perguntas do checklist                     *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 25/04/2025                                             *
* -------------------------------------------------------------------- *
REPORT zsdr0221.

TYPE-POOLS: slis, abap, icon.

TABLES: sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZSDS378'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

DATA gt_dd07t TYPE TABLE OF dd07t.
DATA go_cc_cus_01 TYPE REF TO cl_gui_custom_container.
DATA go_cc_alv_01 TYPE REF TO cl_gui_alv_grid.
"dados alv
DATA gt_dados_alv TYPE STANDARD TABLE OF zsds378.
DATA gt_deleted_alv TYPE STANDARD TABLE OF zsds378.

DATA gt_relacao TYPE TABLE OF zsdt0378_r.
DATA gt_fieldcat TYPE lvc_t_fcat.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.
DATA gv_changed TYPE c.
DATA gv_error TYPE c.
"SELECTION-SCREEN: FUNCTION KEY 1

*" DADOS PRINCIPAIS
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*  "SELECT-OPTIONS so_aufnr FOR ZSDS378-aufnr.
*
*SELECTION-SCREEN END OF BLOCK b1.
*
*" DADOS SECUNDARIOS
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
*
*
*SELECTION-SCREEN END OF BLOCK b2.
*
*" DADOS DO ALV
*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002.
*  PARAMETERS p_vari TYPE slis_vari.
*SELECTION-SCREEN END OF BLOCK b3.

*INITIALIZATION.
*  "PERFORM f_preenche_data.
*  "PERFORM F_BOTAO_FUNCTION.
*  PERFORM default_variant CHANGING p_vari.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
*  PERFORM f4_for_variant CHANGING p_vari.

START-OF-SELECTION.
  PERFORM f_seleciona.
  PERFORM f_exibe_alv.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  IF gt_dd07t IS INITIAL.

    SELECT * FROM dd07t
      INTO TABLE gt_dd07t
        WHERE domname IN ('ZSDTPCHECK','ZSDTPCOND','ZSDTPINCONF')
          AND ddlanguage = sy-langu.

  ENDIF.

  SELECT * FROM zsdt0378
    INTO CORRESPONDING FIELDS OF TABLE gt_dados_alv
      WHERE deleted = abap_false.

  SORT gt_dados_alv BY inativar ASCENDING checkid ASCENDING .

  IF gt_dados_alv IS NOT INITIAL.

    SELECT * FROM zsdt0378_r
      INTO TABLE gt_relacao
        FOR ALL ENTRIES IN gt_dados_alv
          WHERE checkid = gt_dados_alv-checkid
            AND deleted = abap_false.

  ENDIF.

  PERFORM f_refresh_dados_alv.

ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  DATA lt_eventos TYPE slis_t_event.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.
  DATA lw_settings TYPE lvc_s_glay.

  "IF gt_dados_alv IS NOT INITIAL.

  CALL SCREEN 9000.

*  ELSE.
*
*    MESSAGE s213(v4) DISPLAY LIKE 'E'.
*    EXIT.
*
*  ENDIF.

ENDFORM.                    " F_EXIBE_ALV
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:

      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,

      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_double_click.
    PERFORM f_handle_double_click USING e_row e_column es_row_no.
  ENDMETHOD.


  METHOD handle_hotspot_click.
    PERFORM f_hotspot_click USING e_row_id e_column_id es_row_no.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_alv USING e_ucomm.
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_handle_toolbar USING e_object e_interactive.
  ENDMETHOD.

  METHOD handle_data_changed.

    DATA lv_do_refresh.
    gv_error = space.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<fs_mod>).

      READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
        INDEX <fs_mod>-row_id.

      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO gt_dados_alv ASSIGNING <fs_alv>.
      ENDIF.

      PERFORM f_modifica_linha
        USING <fs_mod>-fieldname <fs_mod>-value
      CHANGING <fs_alv>.

      PERFORM f_alv_status_change CHANGING <fs_alv>.

      lv_do_refresh = abap_true.

      <fs_alv>-flag_changed = abap_true.

    ENDLOOP.

*    LOOP AT er_data_changed->mt_deleted_rows ASSIGNING FIELD-SYMBOL(<fs_deleted>).
*
*      READ TABLE gt_dados_alv ASSIGNING <fs_alv>
*        INDEX <fs_deleted>-row_id.
*
*      CHECK sy-subrc EQ 0.
*
*      lv_do_refresh = abap_true.
*
*      <fs_alv>-flag_changed = abap_true.
*      <fs_alv>-inativar = abap_true.
*
*      APPEND <fs_alv> TO gt_deleted_alv.
*
*    ENDLOOP.

    CLEAR er_data_changed->mt_deleted_rows[].

    PERFORM f_refresh_alv USING abap_true abap_true abap_false.

  ENDMETHOD.

ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_handle_events_popup DEFINITION.

  PUBLIC SECTION.
    METHODS : on_user_command
      FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events_popup IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM f_user_command_popup USING e_salv_function.
  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "lcl_handle_events DEFINITION
*&---------------------------------------------------------------------*
*& Form f_user_command
*&---------------------------------------------------------------------*
FORM f_user_command_popup USING p_ucomm TYPE salv_de_function.

*  CASE p_ucomm.
*
*    WHEN 'PROC'.
*
*      gv_popup_processa = 'X'.
*
*      LEAVE TO SCREEN 0.
*    WHEN OTHERS.
*
*      gv_popup_processa = space.
*
*      LEAVE TO SCREEN 0.
*
*  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form user_dt_changed
*&---------------------------------------------------------------------*
FORM user_dt_changed USING rr_data_changed TYPE REF TO
                           cl_alv_changed_data_protocol.

  DATA lv_field TYPE c LENGTH 40.

  LOOP AT rr_data_changed->mt_mod_cells ASSIGNING FIELD-SYMBOL(<fs_mod_cells>).

    READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_saida>)
     INDEX <fs_mod_cells>-row_id.

    CHECK sy-subrc EQ 0.

    lv_field = '<FS_SAIDA>-' && <fs_mod_cells>-fieldname.

    ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_field>).

    CHECK <fs_field> IS ASSIGNED.

    <fs_field> = <fs_mod_cells>-value.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PFSTATUS
*&---------------------------------------------------------------------*
FORM f_status_set USING p_extab TYPE slis_t_extab.          "#EC CALLED

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "F_PFSTATUS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat.

  CLEAR gt_fieldcat.

  SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = gc_struc_name
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
      i_internal_tabname     = gc_internal_tab
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE gt_fieldcat WHERE fieldname = 'SELEC'.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = gc_icon_field.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext = 'Status'.
    <fs_fcat>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fieldcat ASSIGNING <fs_fcat>
    WITH KEY fieldname = 'VINCULO'.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext = 'Vínculo'.
    <fs_fcat>-dd_outlen = 000010.
    <fs_fcat>-hotspot = abap_true.
  ENDIF.

  READ TABLE gt_fieldcat ASSIGNING <fs_fcat>
    WITH KEY fieldname = 'MSGTX'.

  IF sy-subrc EQ 0.
    <fs_fcat>-reptext = 'Status'.
    <fs_fcat>-dd_outlen = 000001.
  ENDIF.

  READ TABLE gt_fieldcat ASSIGNING <fs_fcat>
    WITH KEY fieldname = 'PERGUNTA'.

  IF sy-subrc EQ 0.
    <fs_fcat>-dd_outlen = 000010.
  ENDIF.

  DELETE gt_fieldcat WHERE fieldname = gc_select_field.
  DELETE gt_fieldcat WHERE fieldname = 'DELETED'.
  DELETE gt_fieldcat WHERE fieldname = 'COLOR'.

  "DELETE gt_fieldcat WHERE fieldname = 'ICON'.
  " DELETE gt_fieldcat WHERE fieldname = 'MSGTX'.

  DELETE gt_fieldcat WHERE fieldname = 'TPCHECK_TXT'.
  DELETE gt_fieldcat WHERE fieldname = 'TPCOND_TXT'.
  DELETE gt_fieldcat WHERE fieldname = 'TPINCONF_TXT'.


  PERFORM f_fieldcat_modi USING 'CHECKID' 'JUST' 'C' CHANGING gt_fieldcat.
*  PERFORM f_fieldcat_modi USING 'TPCHECK_TXT' 'COLPOS' '7' CHANGING gt_fieldcat.
*  PERFORM f_fieldcat_modi USING 'TPCOND_TXT' 'COLPOS' '8' CHANGING gt_fieldcat.
*  PERFORM f_fieldcat_modi USING 'TPINCONF_TXT' 'COLPOS' '9' CHANGING gt_fieldcat.

*  PERFORM f_fieldcat_modi USING 'TPCHECK_TXT' 'REPTEXT' 'Avaliação' CHANGING gt_fieldcat.
*  PERFORM f_fieldcat_modi USING 'TPCOND_TXT' 'REPTEXT' 'Condicional' CHANGING gt_fieldcat.
*  PERFORM f_fieldcat_modi USING 'TPINCONF_TXT' 'REPTEXT' 'Inconformidade' CHANGING gt_fieldcat.

  PERFORM f_fieldcat_modi USING 'VINCULO' 'REPTEXT' 'Vínculo' CHANGING gt_fieldcat.


  DELETE gt_fieldcat WHERE fieldname = 'SIM_NAO_INCONFOR'.
  DELETE gt_fieldcat WHERE fieldname = 'INATIVAR'.


  DELETE gt_fieldcat WHERE fieldname = 'USER_CREATE'.
  DELETE gt_fieldcat WHERE fieldname = 'DATE_CREATE'.
  DELETE gt_fieldcat WHERE fieldname = 'TIME_CREATE'.
  DELETE gt_fieldcat WHERE fieldname = 'USER_CHANGE'.
  DELETE gt_fieldcat WHERE fieldname = 'DATE_CHANGE'.
  DELETE gt_fieldcat WHERE fieldname = 'TIME_CHANGE'.

  DELETE gt_fieldcat WHERE fieldname = 'FLAG_VINCULO'.
  DELETE gt_fieldcat WHERE fieldname = 'FLAG_CHANGED'.

  LOOP AT gt_fieldcat ASSIGNING <fs_fcat>
    WHERE fieldname = 'PERGUNTA'
       OR fieldname = 'TPCHECK'
       OR fieldname = 'TPCOND'
       OR fieldname = 'TPINCONF'.

    <fs_fcat>-edit = 'X'.

  ENDLOOP.

ENDFORM.                    " F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema.

  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_s.

  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_SISTEMA_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_insere.

  PERFORM f_mensagem_insere
    TABLES gt_bapiret2
     USING sy-msgty
           sy-msgid
           sy-msgno
           sy-msgv1
           sy-msgv2
           sy-msgv3
           sy-msgv4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
FORM f_sap_indicator USING p_text TYPE c
                           p_percent TYPE i.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percent
      text       = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
FORM f4_for_variant CHANGING f_vari TYPE slis_vari.

  DATA: lw_variant TYPE disvariant.

  lw_variant-variant = f_vari.
  lw_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = lw_variant
      i_save     = 'A'
    IMPORTING
      es_variant = lw_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    f_vari = lw_variant-variant.
  ENDIF.

ENDFORM.                    "f4_for_variant
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VARIANT
*&---------------------------------------------------------------------*
FORM default_variant CHANGING f_vari TYPE slis_vari.
  DATA: lw_variant TYPE disvariant.

  lw_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = lw_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc = 0.
    f_vari = lw_variant-variant.
  ENDIF.

ENDFORM.                    " DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_DATA
*&---------------------------------------------------------------------*
FORM f_preenche_data .

  "DATA(lv_ini) = sy-datum.
  "DATA(lv_fim) = sy-datum.

  "CHECK so_data[] IS INITIAL.

  "SUBTRACT 15 FROM lv_ini.
  "ADD 15 TO lv_fim.

  "APPEND 'IBT' && lv_ini && lv_fim TO so_data.

ENDFORM.

"PERFORM f_mensagem_insere TABLES p_ret2
"USING 'E' 'ZMM' '000' 'SYSID' text-t02
"gw_034-logsys space space.

FORM f_mensagem_bapiret USING p_mess TYPE bapiret2.

  MESSAGE ID p_mess-id TYPE 'S' NUMBER p_mess-number
    WITH p_mess-message_v1 p_mess-message_v2
         p_mess-message_v3 p_mess-message_v4.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_insere TABLES p_ret_tab STRUCTURE bapiret2
                        USING i_type TYPE bapi_mtype
                              i_id  TYPE  symsgid
                              i_number  TYPE  symsgno
                              i_mess_v1 TYPE any
                              i_mess_v2 TYPE any
                              i_mess_v3 TYPE any
                              i_mess_v4 TYPE any.

  APPEND INITIAL LINE TO p_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  MESSAGE ID <fs_ret>-id TYPE <fs_ret>-type NUMBER <fs_ret>-number
    WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
      <fs_ret>-message_v4 INTO <fs_ret>-message.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_EXIBE_POPUP
*&---------------------------------------------------------------------*
FORM f_mensagem_exibe_popup USING p_bapiret2_tab TYPE bapiret2_t.

  DATA: l_lines TYPE i.

  DESCRIBE TABLE p_bapiret2_tab LINES l_lines.

  IF l_lines <= 1 OR sy-batch = 'X'.

    LOOP AT p_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_ret2>).

      MESSAGE ID <fs_ret2>-id
            TYPE 'S'
          NUMBER <fs_ret2>-number
            WITH <fs_ret2>-message_v1
                 <fs_ret2>-message_v2
                 <fs_ret2>-message_v3
                 <fs_ret2>-message_v4 DISPLAY LIKE <fs_ret2>-type.

    ENDLOOP.

  ELSE.

    CALL FUNCTION 'MESSAGES_INITIALIZE'.

    LOOP AT p_bapiret2_tab ASSIGNING <fs_ret2>.

      IF <fs_ret2>-id IS INITIAL.

        <fs_ret2>-id = 'DS'. "<-classe padrao abap
        <fs_ret2>-number = '016'.
        <fs_ret2>-message_v1 = <fs_ret2>-message.

      ENDIF.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = <fs_ret2>-id
          "EXCEPTION_IF_NOT_ACTIVE  = 'X'
          msgty                  = <fs_ret2>-type
          msgv1                  = <fs_ret2>-message_v1
          msgv2                  = <fs_ret2>-message_v2
          msgv3                  = <fs_ret2>-message_v3
          msgv4                  = <fs_ret2>-message_v4
          txtnr                  = <fs_ret2>-number
          "ZEILE                    = ' '
          "IMPORTING
          "ACT_SEVERITY             =
          "MAX_SEVERITY             =
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2
          OTHERS                 = 3.     "#EC CI_SUBRC

    ENDLOOP.

    CALL FUNCTION 'MESSAGES_STOP'
      EXCEPTIONS
        a_message = 1
        e_message = 2
        i_message = 3
        w_message = 4
        OTHERS    = 5.     "#EC CI_SUBRC

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        "CORRECTIONS_OPTION          = ' '
        "CORRECTIONS_FUNC_TEXT       = ' '
        "LINE_FROM                   = ' '
        "LINE_TO                     = ' '
        "OBJECT                      = ' '
        "SEND_IF_ONE                 = ' '
        batch_list_type     = 'B'
        show_linno          = ' '
        show_linno_text     = 'X'
        show_linno_text_len = '3'
        i_use_grid          = ' '
        i_amodal_window     = ' '
        "MSG_SELECT_FUNC             = ' '
        "MSG_SELECT_FUNC_TEXT        = ' '
        "IMPORTING
        "CORRECTIONS_WANTED          =
        "E_EXIT_COMMAND              =
        "MSG_SELECTED                =
      EXCEPTIONS
        inconsistent_range  = 1
        no_messages         = 2
        OTHERS              = 3.     "#EC CI_SUBRC

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_coluna_descr
*&---------------------------------------------------------------------*
FORM f_set_edit  USING p_fieldname TYPE slis_fieldname.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-edit = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VERIFICA_LINHA_SELEC
*&---------------------------------------------------------------------*
FORM f_verifica_linha_selec CHANGING p_error TYPE c.

*  READ TABLE gt_dados_alv WITH KEY selec = 'X' TRANSPORTING NO FIELDS.
*
*  IF sy-subrc NE 0.
*    MESSAGE s851(v4) DISPLAY LIKE 'E'.
*    p_error = 'X'.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa
*&---------------------------------------------------------------------*
FORM f_processa.

  DATA lr_selec TYPE RANGE OF flag.

  DATA lv_ret.

  IF sy-batch IS INITIAL.

    PERFORM f_verifica_linha_selec CHANGING lv_ret.

    CHECK lv_ret IS INITIAL.

    PERFORM f_popup_to_confirm USING TEXT-t01 CHANGING lv_ret.

    CHECK lv_ret = '1'.

    APPEND 'IEQX' TO lr_selec.

  ELSE.
    CLEAR lr_selec.

  ENDIF.

*  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.
*
*
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_to_confirm USING p_question TYPE c
                     CHANGING p_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = sy-title
      text_question  = p_question
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_INSERE_TXT
*&---------------------------------------------------------------------*
FORM f_mensagem_insere_txt USING i_type TYPE bapi_mtype
                                 p_string TYPE string.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).

  DATA lv_msg1 TYPE sy-msgv1.
  DATA lv_msg2 TYPE sy-msgv1.
  DATA lv_msg3 TYPE sy-msgv1.
  DATA lv_msg4 TYPE sy-msgv1.

  lv_texto = p_string.

  CALL FUNCTION 'TR_SPLIT_TEXT'
    EXPORTING
      iv_text  = lv_texto
      iv_len   = 30
    IMPORTING
      et_lines = lt_trtexts.

  LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE sy-tabix.
      WHEN 1.
        lv_msg1 = <fs_line>.
      WHEN 2.
        lv_msg2 = <fs_line>.
      WHEN 3.
        lv_msg3 = <fs_line>.
      WHEN 4.
        lv_msg4 = <fs_line>.
    ENDCASE.

  ENDLOOP.

  PERFORM f_mensagem_insere
    TABLES gt_bapiret2
     USING i_type
           'DS'
           '016'
           lv_msg1
           lv_msg2
           lv_msg3
           lv_msg4.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR '9000'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM f_save.
    WHEN 'BACK'.
      PERFORM f_leave_to_screen.
    WHEN 'EXIT'.
      PERFORM f_leave_to_screen.
    WHEN 'CANC'.
      PERFORM f_leave_to_screen.
    WHEN OTHERS.
      "go_cc_alv_01->check_changed_data( ).
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALV_01_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_01_init OUTPUT.

  PERFORM f_create_alv_01.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  f_coluna_descr
*&---------------------------------------------------------------------*
FORM f_coluna_descr USING p_fieldname TYPE slis_fieldname
                          p_text TYPE scrtext_l
                 CHANGING p_tab TYPE lvc_t_fcat..

  READ TABLE p_tab ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-scrtext_s = p_text.
  <fs_cat>-scrtext_m = p_text.
  <fs_cat>-scrtext_l = p_text.
  <fs_cat>-reptext = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ALV_01
*&---------------------------------------------------------------------*
FORM f_create_alv_01 .

  DATA lt_exclude TYPE ui_functions.
  DATA lw_settings TYPE lvc_s_glay.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.

  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_event_handler.

  "lw_layout-grid_title = sy-title.

  lw_layout-sel_mode = 'A'.
  "lw_layout-no_headers = 'X'.
  "lw_layout-no_toolbar = 'X'. XXXXX
  lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.

  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.
  lw_layout-stylefname = 'CELLTAB'.
  lw_layout-info_fname = 'COLOR'.

  "lw_settings-edt_cll_cb = 'X'.

  IF go_cc_alv_01 IS INITIAL.

    CREATE OBJECT go_cc_alv_01
      EXPORTING
        i_parent = cl_gui_container=>default_screen.

    PERFORM f_monta_fieldcat.

    PERFORM f_exclude_tb_functions CHANGING lt_exclude.

*    CALL METHOD go_cc_alv_01->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD go_cc_alv_01->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT lo_handle.

    SET HANDLER lo_handle->handle_toolbar FOR go_cc_alv_01.
    SET HANDLER lo_handle->handle_hotspot_click FOR go_cc_alv_01.
    SET HANDLER lo_handle->handle_user_command  FOR go_cc_alv_01.
    SET HANDLER lo_handle->handle_data_changed  FOR go_cc_alv_01.
    SET HANDLER lo_handle->handle_double_click FOR go_cc_alv_01.

    lw_variant-report       = sy-repid.
    lw_variant-username     = sy-uname.

    " Configuration for first display.
    CALL METHOD go_cc_alv_01->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = lt_exclude
        is_layout            = lw_layout
        is_variant           = lw_variant
        i_save               = 'A'
      CHANGING
        it_outtab            = gt_dados_alv
        it_fieldcatalog      = gt_fieldcat.


    " Set editable cells to ready for input initially
    CALL METHOD go_cc_alv_01->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    PERFORM f_refresh_dados_alv.

    CALL METHOD go_cc_alv_01->set_frontend_layout
      EXPORTING
        is_layout = lw_layout.

    " Set editable cells to ready for input initially
    CALL METHOD go_cc_alv_01->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD go_cc_alv_01->refresh_table_display( ).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita2  USING p_fieldname TYPE slis_fieldname
                            p_text_s TYPE scrtext_s
                            p_text_l TYPE scrtext_l.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-scrtext_s = p_text_s.
  <fs_cat>-scrtext_m = p_text_s.
  <fs_cat>-scrtext_l = p_text_l.
  <fs_cat>-reptext = p_text_l.

ENDFORM.
"PERFORM f_fieldcat_modi USING 'PESO' 'EDIT' 'X' CHANGING lt_fieldcat.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_MODI
*&---------------------------------------------------------------------*
FORM f_fieldcat_modi USING p_fieldname TYPE slis_fieldname
                           p_column TYPE c
                           p_value TYPE any
                  CHANGING p_field_cat TYPE lvc_t_fcat.

  READ TABLE p_field_cat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  DATA(lv_name) = '<FS_FCAT>-' && p_column.

  ASSIGN (lv_name) TO FIELD-SYMBOL(<fs_colum>).

  CHECK sy-subrc EQ 0.

  <fs_colum> = p_value.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREENCHE_X
*&---------------------------------------------------------------------*
FORM f_preenche_x USING iv_struct TYPE any
               CHANGING cv_structx TYPE any.

  DATA lo_linetype TYPE REF TO cl_abap_structdescr.

  ASSIGN ('IV_STRUCT') TO FIELD-SYMBOL(<lf_data>).

  CHECK sy-subrc EQ 0.

  lo_linetype ?= cl_abap_typedescr=>describe_by_data( <lf_data> ).

  LOOP AT lo_linetype->components ASSIGNING FIELD-SYMBOL(<lf_component>).

    DATA(lv_campo) = 'IV_STRUCT-' && <lf_component>-name.
    DATA(lv_campox) = 'CV_STRUCTX-' && <lf_component>-name.

    ASSIGN (lv_campo) TO FIELD-SYMBOL(<lf_campo>).

    CHECK sy-subrc EQ 0.

    ASSIGN (lv_campox) TO FIELD-SYMBOL(<lf_campox>).

    IF <lf_campo> IS NOT INITIAL.
      <lf_campox> = 'X'.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_status_change
*&---------------------------------------------------------------------*
FORM f_alv_status_change CHANGING cs_alv TYPE zsds378.

  cs_alv-icon = icon_green_light.
  cs_alv-msgtx = 'Ativo'.

  IF cs_alv-inativar = abap_true.

    cs_alv-icon = icon_red_light.
    cs_alv-msgtx = 'Inativo'.

  ENDIF.

  IF cs_alv-deleted = abap_true.
    cs_alv-icon = icon_red_light.
    cs_alv-msgtx = 'Excluído'.
  ENDIF.

  PERFORM f_check_vinculo CHANGING cs_alv.

  IF cs_alv-tpcheck = 'P'.

    IF cs_alv-flag_vinculo = abap_true.
      cs_alv-vinculo = icon_display_more.
    ELSE.
      cs_alv-vinculo = icon_enter_more.
    ENDIF.

  ELSE.

    cs_alv-vinculo = icon_enter_more.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_domain_value
*&---------------------------------------------------------------------*
FORM f_domain_value  USING uv_field TYPE c
                           uv_value TYPE c
                     CHANGING cv_field_txt TYPE c.

  READ TABLE gt_dd07t ASSIGNING FIELD-SYMBOL(<fs_domain>)
    WITH KEY domname = uv_field
             domvalue_l = uv_value.

  CHECK sy-subrc EQ 0.

  cv_field_txt = <fs_domain>-ddtext.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_handle_toolbar
*&---------------------------------------------------------------------*
FORM f_handle_toolbar USING us_toolbar TYPE REF TO  cl_alv_event_toolbar_set
                            uv_interactive TYPE char01.

  PERFORM f_add_button USING 'DESATIVAR' 'Ativar/Desativar' '@3J@' CHANGING us_toolbar.
  "PERFORM f_add_button USING 'EXC' 'Excluir' '@18@' CHANGING us_toolbar.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_add_button
*&---------------------------------------------------------------------*
FORM f_add_button USING uv_syucomm TYPE syucomm
                        uv_text TYPE c
                        uv_icon TYPE iconname
               CHANGING us_toolbar TYPE REF TO  cl_alv_event_toolbar_set.

  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE 3 TO ls_toolbar-butn_type.
  APPEND ls_toolbar TO us_toolbar->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE uv_syucomm TO ls_toolbar-function.
  MOVE uv_icon TO ls_toolbar-icon.
  MOVE uv_text TO ls_toolbar-quickinfo.
  MOVE uv_text TO ls_toolbar-text.
  MOVE ' ' TO ls_toolbar-disabled.
  APPEND ls_toolbar TO us_toolbar->mt_toolbar.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_selected_rows
*&---------------------------------------------------------------------*
FORM f_get_selected_rows USING uo_alv TYPE REF TO cl_gui_alv_grid
                      CHANGING ct_row TYPE lvc_t_roid
                               cv_erro.

  CHECK uo_alv IS BOUND.

  CALL METHOD uo_alv->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_index)
      et_row_no     = ct_row.

  IF ct_row IS INITIAL.
    cv_erro = abap_true.
    MESSAGE s016(ds) WITH 'Selecionar ao menos uma linha' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh_alv
*&---------------------------------------------------------------------*
FORM f_refresh_alv USING uv_changed TYPE c
                         uv_stable TYPE c
                         uv_soft TYPE c.

  DATA ls_stable TYPE lvc_s_stbl.

  ls_stable-col = abap_true.
  ls_stable-row = abap_true.

  IF go_cc_alv_01 IS BOUND.

    IF uv_stable = abap_false.
      go_cc_alv_01->refresh_table_display( i_soft_refresh = uv_soft ).
    ELSE.
      go_cc_alv_01->refresh_table_display( EXPORTING is_stable = ls_stable i_soft_refresh = uv_soft ).
    ENDIF.

  ENDIF.

  gv_changed = uv_changed.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LEAVE_TO_SCREEN
*&---------------------------------------------------------------------*
FORM f_leave_to_screen .

  DATA lv_answer TYPE c VALUE '1'.

  IF gv_changed = abap_true.
    PERFORM f_popup_to_confirm USING 'Dados não foram gravados, sair mesmo assim?' CHANGING lv_answer.
  ENDIF.

  CHECK lv_answer EQ '1'.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_save
*&---------------------------------------------------------------------*
FORM f_save.

  DATA lv_seq TYPE i.

  DATA lt_0378 TYPE TABLE OF zsdt0378.
  DATA lt_0378_r TYPE TABLE OF zsdt0378_r.

  go_cc_alv_01->check_changed_data( ).

  APPEND LINES OF gt_deleted_alv TO gt_dados_alv.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE flag_changed = abap_true.

    IF <fs_alv>-checkid IS INITIAL.

      PERFORM f_get_number CHANGING <fs_alv>-checkid.

      <fs_alv>-user_create = sy-uname.
      <fs_alv>-date_create = sy-datum.
      <fs_alv>-time_create = sy-uzeit.

    ELSE.

      <fs_alv>-user_change = sy-uname.
      <fs_alv>-date_change = sy-datum.
      <fs_alv>-time_change = sy-uzeit.

    ENDIF.

    APPEND INITIAL LINE TO lt_0378 ASSIGNING FIELD-SYMBOL(<fs_0378>).

    MOVE-CORRESPONDING <fs_alv> TO <fs_0378>.

    PERFORM f_alv_status_change CHANGING <fs_alv>.

  ENDLOOP.

  SORT gt_relacao BY checkid checkid_r ASCENDING.

  LOOP AT gt_relacao INTO DATA(ls_relacao).

    READ TABLE gt_relacao ASSIGNING FIELD-SYMBOL(<fs_relacao>) INDEX sy-tabix.

    AT NEW checkid.

      IF <fs_relacao>-sequencial IS INITIAL.
        PERFORM f_get_seq_rel CHANGING <fs_relacao>.
      ENDIF.

    ENDAT.

    APPEND INITIAL LINE TO lt_0378_r ASSIGNING FIELD-SYMBOL(<fs_0378_r>).

    MOVE-CORRESPONDING <fs_relacao> TO <fs_0378_r>.

    IF <fs_0378_r>-deleted = abap_false.

      <fs_0378_r>-user_create = sy-uname.
      <fs_0378_r>-date_create = sy-datum.
      <fs_0378_r>-time_create = sy-uzeit.

    ELSE.

      <fs_0378_r>-user_change = sy-uname.
      <fs_0378_r>-date_change = sy-datum.
      <fs_0378_r>-time_change = sy-uzeit.

    ENDIF.

  ENDLOOP.

  IF lt_0378[] IS NOT INITIAL.

    MODIFY zsdt0378 FROM TABLE lt_0378.

    LOOP AT gt_deleted_alv ASSIGNING <fs_alv>.

      DELETE gt_dados_alv WHERE checkid = <fs_alv>-checkid.

    ENDLOOP.

  ENDIF.

  IF lt_0378_r[] IS NOT INITIAL.

    MODIFY zsdt0378_r FROM TABLE lt_0378_r.

  ENDIF.

  COMMIT WORK AND WAIT.

  PERFORM f_refresh_alv USING abap_false abap_false abap_false.

  MESSAGE s016(ds) WITH 'Dados gravados'.



  "PERFORM f_leave_to_screen.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fill_celltab
*&---------------------------------------------------------------------*
FORM fill_celltab USING VALUE(p_mode)
                  CHANGING pt_celltab TYPE lvc_t_styl.
  DATA: ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  IF p_mode EQ 'RW'.
    l_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.                                "p_mode eq 'RO'
    l_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  pt_celltab = VALUE #(
    ( fieldname = 'PERGUNTA' style = l_mode )
    ( fieldname = 'TPCHECK' style = l_mode )
    ( fieldname = 'TPCOND' style = l_mode )
    ( fieldname = 'TPINCONF' style = l_mode ) ).

ENDFORM.                               " FILL_CELLTAB
*&---------------------------------------------------------------------*
*& Form F_exclude_tb_functions
*&---------------------------------------------------------------------*
FORM f_exclude_tb_functions CHANGING pt_exclude TYPE ui_functions.
*
  DATA ls_exclude TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO pt_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.

ENDFORM.                               " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*& Form f_modifica_linha
*&---------------------------------------------------------------------*
FORM f_modifica_linha USING uv_fieldname TYPE c
                            uv_value TYPE any
                   CHANGING cs_alv TYPE zsds378.

  DATA lv_fieldname TYPE c LENGTH 30.

  lv_fieldname = 'CS_ALV-' && uv_fieldname.

  ASSIGN (lv_fieldname) TO FIELD-SYMBOL(<fs_field>).

  <fs_field> = uv_value.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_number
*&---------------------------------------------------------------------*
FORM f_get_number CHANGING cv_checkid TYPE zsdcheckid.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZSDCHECKID'
    IMPORTING
      number                  = cv_checkid
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.
    MESSAGE e016(ds) WITH 'Cadastrar intervalo(SNRO)' 'ZSDCHECKID'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_hotspot_click
*&---------------------------------------------------------------------*
FORM f_hotspot_click USING e_row_id TYPE  lvc_s_row
                           e_column_id  TYPE  lvc_s_col
                           es_row_no  TYPE  lvc_s_roid.
  DATA lv_canc.
  DATA lt_relacao TYPE TABLE OF zsdt0378_r.

  CASE e_column_id.
    WHEN 'VINCULO'.

      READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
       INDEX e_row_id.

      CHECK sy-subrc EQ 0.

      CHECK <fs_alv>-tpcheck = 'P'.

      IF <fs_alv>-checkid IS INITIAL.
        MESSAGE s016(ds) WITH 'Gravar os dados antes de vincular' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF <fs_alv>-tpcond IS INITIAL.
        MESSAGE s016(ds) WITH 'Preencher Sim ou Não para a condição' 'de vinculo' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      <fs_alv>-color = 'C500'.

      PERFORM f_refresh_alv USING abap_false abap_true abap_true.

      LOOP AT gt_relacao ASSIGNING FIELD-SYMBOL(<fs_relacao>)
          WHERE checkid = <fs_alv>-checkid.

        APPEND <fs_relacao> TO lt_relacao.

      ENDLOOP.

      CALL FUNCTION 'ZSDMF_SEL_PERGUNTAS_POPUP'
        EXPORTING
          iv_checkid = <fs_alv>-checkid
        IMPORTING
          ev_canc    = lv_canc
        CHANGING
          ct_relacao = lt_relacao.

      CLEAR <fs_alv>-color.

      PERFORM f_refresh_alv USING abap_false abap_true abap_true.

      IF lv_canc IS INITIAL.

        LOOP AT gt_relacao ASSIGNING FIELD-SYMBOL(<fs_global>) WHERE checkid = <fs_alv>-checkid.

          READ TABLE lt_relacao ASSIGNING <fs_relacao>
            WITH KEY checkid = <fs_global>-checkid
                     checkid_r = <fs_global>-checkid_r.

          IF sy-subrc NE 0.
            APPEND <fs_relacao> TO gt_relacao.
          ENDIF.

        ENDLOOP.

        LOOP AT lt_relacao ASSIGNING <fs_relacao> WHERE checkid = <fs_alv>-checkid.

          READ TABLE gt_relacao ASSIGNING <fs_global>
            WITH KEY checkid = <fs_relacao>-checkid
                     checkid_r = <fs_relacao>-checkid_r.

          IF sy-subrc EQ 0.
            <fs_global> = <fs_relacao>.
          ELSE.
            APPEND <fs_relacao> TO gt_relacao.
          ENDIF.

        ENDLOOP.

        PERFORM f_alv_status_change CHANGING <fs_alv>.

        PERFORM f_refresh_alv USING abap_true abap_true abap_true.

      ENDIF.

*  	WHEN .
*  	WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_check_vinculo
*&---------------------------------------------------------------------*
FORM f_check_vinculo CHANGING cs_alv TYPE zsds378..

  READ TABLE gt_relacao TRANSPORTING NO FIELDS
    WITH KEY checkid = cs_alv-checkid
             deleted = abap_false.

  IF sy-subrc EQ 0.
    cs_alv-flag_vinculo = abap_true.
  ELSE.
    cs_alv-flag_vinculo = abap_false.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_seq_rel
*&---------------------------------------------------------------------*
FORM f_get_seq_rel CHANGING cs_relacao TYPE zsdt0378_r.

  SELECT MAX( sequencial ) FROM zsdt0378_r
    INTO @DATA(lv_seq)
      WHERE checkid = @cs_relacao-checkid
        AND checkid_r = @cs_relacao-checkid_r.

  IF sy-subrc EQ 0.
    cs_relacao-sequencial = lv_seq + 1.
  ELSE.
    cs_relacao-sequencial = 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_handle_double_click
*&---------------------------------------------------------------------*
FORM f_handle_double_click USING uv_row TYPE lvc_s_row
                                 uv_column TYPE  lvc_s_col
                                 uv_row_no TYPE	lvc_s_roid.


  DATA lv_canc.
  DATA lt_relacao TYPE TABLE OF zsdt0378_r.

  DATA lt_text_aux TYPE TABLE OF txw_note.
  DATA lv_edit TYPE c.
  DATA tl_texto TYPE catsxt_longtext_itab.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).


  CASE uv_column.

    WHEN 'PERGUNTA'.

      READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) INDEX uv_row.

      CHECK sy-subrc EQ 0.

      <fs_alv>-color = 'C500'.

      PERFORM f_refresh_alv USING abap_false abap_true abap_true .

      lv_texto = <fs_alv>-pergunta.

      CALL FUNCTION 'TR_SPLIT_TEXT'
        EXPORTING
          iv_text  = lv_texto
          iv_len   = 60
        IMPORTING
          et_lines = lt_trtexts.

      LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

        APPEND INITIAL LINE TO lt_text_aux ASSIGNING FIELD-SYMBOL(<fs_aux>).

        <fs_aux>-line = <fs_line>.

      ENDLOOP.

      "IF <fs_alv>-checkid IS INITIAL.
      lv_edit = abap_true.
      "<fs_alv>-flag_changed = abap_true.
      "ELSE.
      "lv_edit = abap_false.
      " ENDIF.

      CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
        EXPORTING
          edit_mode = lv_edit
        TABLES
          t_txwnote = lt_text_aux[].

      CLEAR <fs_alv>-pergunta.

      LOOP AT lt_text_aux ASSIGNING FIELD-SYMBOL(<fs_text>).

        CHECK <fs_text>-line IS NOT INITIAL.

        CONCATENATE <fs_alv>-pergunta <fs_text>-line
          INTO <fs_alv>-pergunta SEPARATED BY space.

        <fs_alv>-flag_changed = 'X'.

      ENDLOOP.

      CONDENSE <fs_alv>-pergunta.

      CLEAR <fs_alv>-color.

      PERFORM f_refresh_alv USING lv_edit abap_true abap_true.

  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_user_command_alv
*&---------------------------------------------------------------------*
FORM f_user_command_alv USING uv_ucomm.

  CASE uv_ucomm.
    WHEN 'DESATIVAR'.
      PERFORM f_acti_desat.
    WHEN OTHERS.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_acti_desat
*&---------------------------------------------------------------------*
FORM f_acti_desat .

  DATA lt_row TYPE lvc_t_roid.
  DATA lv_erro.

  PERFORM f_get_selected_rows
    USING go_cc_alv_01
  CHANGING lt_row lv_erro.

  CHECK lv_erro IS INITIAL.

  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<fs_row>).

    READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
      INDEX <fs_row>-row_id.

    CHECK sy-subrc EQ 0.

    IF <fs_alv>-inativar = abap_false.
      <fs_alv>-inativar = abap_true.
    ELSE.
      <fs_alv>-inativar = abap_false.
    ENDIF.

    <fs_alv>-flag_changed = abap_true.

    PERFORM f_alv_status_change CHANGING <fs_alv>.

  ENDLOOP.

  PERFORM f_refresh_alv USING abap_true abap_true abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh_dados_alv
*&---------------------------------------------------------------------*
FORM f_refresh_dados_alv .

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    PERFORM f_alv_status_change CHANGING <fs_alv>.

    "PERFORM fill_celltab USING 'RO' CHANGING <fs_alv>-celltab.
    PERFORM fill_celltab USING 'RW' CHANGING <fs_alv>-celltab.

  ENDLOOP.


ENDFORM.
