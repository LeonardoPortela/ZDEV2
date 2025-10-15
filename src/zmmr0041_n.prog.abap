* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZMMR0041_N                                             *
* Title.......: Cockpit Notas Fiscais de Serviço (MIGO/MIRO)           *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 01/08/2022                                             *
* -------------------------------------------------------------------- *
REPORT zmmr0041_n.

INCLUDE zmmr_0041_n_top.
INCLUDE zmmr_0041_n_pbo.
INCLUDE zmmr_0041_n_pai.

INITIALIZATION.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.
  DATA _acesso(1) VALUE 'S'.

  LOOP AT s_branch INTO DATA(w_branch).
    IF w_branch-high IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT 'ZNFSE_INB'  ID 'ZANFETER'   FIELD '06' "Aceite Fiscal
                                          ID 'ZNFETERMEP' FIELD s_bukrs-low
                                          ID 'ZNFETERFIL' FIELD w_branch-high.
      IF sy-subrc IS NOT INITIAL.
        _acesso = 'N'.
        EXIT.
      ENDIF.
    ENDIF.
    AUTHORITY-CHECK OBJECT 'ZNFSE_INB'  ID 'ZANFETER'   FIELD '06' "Aceite Fiscal
                                        ID 'ZNFETERMEP' FIELD s_bukrs-low
                                        ID 'ZNFETERFIL' FIELD w_branch-low.
    IF sy-subrc IS NOT INITIAL.
      _acesso = 'N'.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF  _acesso = 'S'.
    PERFORM f_seleciona.
    PERFORM f_processa_dados.
    PERFORM f_exibe_alv.
  ELSE.
    MESSAGE i000(z_mm) WITH 'Sem acesso a esta filial  (OBJ ZNFSE_INB )' DISPLAY LIKE 'E'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  CLEAR gt_dados_alv.

  SELECT /tcsr/t_act~bukrs            /tcsr/t_act~branch      /tcsr/t_act~cancel
         /tcsr/t_act~last_stepstatus  /tcsr/t_hd~nfse_numero  /tcsr/t_hd~nfse_serie
         /tcsr/t_hd~nfse_value        /tcsr/t_hd~dtemissao    /tcsr/t_act~lifnr
         /tcsr/t_hd~nfse_year         /tcsr/t_hd~nfse_month   /tcsr/t_act~guid_header
         /tcsr/t_hd~p_cpf             /tcsr/t_hd~p_cnpj       /tcsr/t_hd~t_cnpj
         /tcsr/t_act~erdat
    INTO CORRESPONDING FIELDS OF TABLE gt_dados_alv
    FROM /tcsr/t_act
    INNER JOIN /tcsr/t_hd
    ON /tcsr/t_hd~guid_header EQ /tcsr/t_act~guid_header
    WHERE /tcsr/t_act~bukrs      IN s_bukrs
    AND   /tcsr/t_act~branch     IN s_branch
    AND   /tcsr/t_hd~dtemissao   IN s_dtemis
    AND   /tcsr/t_hd~nfse_numero IN s_nfsenu
    AND   /tcsr/t_act~lifnr      IN s_lifnr
    AND   /tcsr/t_hd~p_cpf       IN s_cpf
    AND   /tcsr/t_hd~p_cnpj      IN s_cnpj.

  CHECK sy-subrc EQ 0.

  SORT gt_dados_alv BY guid_header.

  " 25.11.2022
  DELETE gt_dados_alv WHERE last_stepstatus = '102'.

  SELECT * FROM zibt_nfse_001
    INTO TABLE gt_nfse_001
      FOR ALL ENTRIES IN gt_dados_alv
        WHERE guid_header = gt_dados_alv-guid_header
          AND mblnr IN s_mblnr
          AND lblni IN s_lblni
          AND belnr IN s_belnr
          AND ebeln IN s_ebeln.

  SELECT * FROM zibt_nfse_002
    INTO TABLE gt_nfse_002
      FOR ALL ENTRIES IN gt_dados_alv
        WHERE guid_header = gt_dados_alv-guid_header
         AND ebeln IN s_ebeln.

  IF sy-subrc IS INITIAL.
    SORT gt_nfse_001 BY guid_header.
  ENDIF.

  SELECT * FROM lfa1
    INTO TABLE gt_lfa1
      FOR ALL ENTRIES IN gt_dados_alv
        WHERE lifnr = gt_dados_alv-lifnr.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = '/TCSR/D_STEPSTATUS'
      text           = 'X'
      langu          = 'E' "sy-langu
      bypass_buffer  = 'X'
    TABLES
      dd07v_tab      = gt_status
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
                           rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&IC1'.
      PERFORM f_hyperlink USING rs_selfield.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

  rs_selfield-refresh = 'X'.

  r_ucomm = '&REFRESH'.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_FUNCTION
*&---------------------------------------------------------------------*
FORM f_botao_function.

  sscrfields-functxt_01 = 'BOTAO 1'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_COMMAND
*&---------------------------------------------------------------------*
FORM f_botao_command.

  IF sy-ucomm = 'FC01'.
    "EXECUTA FUNÇÃO DO BOTAO 1
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  "DATA lw_layout TYPE slis_layout_alv.
  "DATA lw_variant TYPE disvariant.

  IF gt_dados_alv IS NOT INITIAL.

    CALL SCREEN 9000.

  ELSE.
    MESSAGE s213(v4) DISPLAY LIKE 'E'.
    EXIT.

  ENDIF.

ENDFORM.                    " F_EXIBE_ALV
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


  "LVC_FIELDCATALOG_MERGE
  " SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-cprog
      i_internal_tabname     = gc_internal_tab
      i_structure_name       = gc_struc_name
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = gc_icon_field.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext_ddic = 'Status'.
    <fs_fcat>-ddic_outputlen = 000010.
  ENDIF.

  DELETE gt_fieldcat WHERE fieldname = gc_select_field.
  DELETE gt_fieldcat WHERE fieldname = 'COLOR'.

  PERFORM f_coluna_edita USING 'MSGTX' 'Status'.

  "PERFORM f_coluna_edita USING 'COUNT_SAP' 'Notas SAP'.


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
*&---------------------------------------------------------------------*
*&      Form  F_HYPERLINK
*&---------------------------------------------------------------------*
FORM f_hyperlink USING rs_selfield TYPE slis_selfield.

  DATA lw_saida_alv LIKE LINE OF gt_dados_alv.

  CHECK rs_selfield-value IS NOT INITIAL.

  READ TABLE gt_dados_alv INTO lw_saida_alv INDEX rs_selfield-tabindex.

  CASE rs_selfield-fieldname.

    WHEN 'EBELN'.

      CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
        EXPORTING
          i_ebeln = lw_saida_alv-ebeln.

    WHEN 'LBLNI' OR 'PACKNO'.

      SET PARAMETER ID 'BES' FIELD ' '.
      SET PARAMETER ID 'LBL' FIELD lw_saida_alv-lblni.
      SET PARAMETER ID 'LBD' FIELD 'X'.

      CALL TRANSACTION 'ML81N' AND SKIP FIRST SCREEN.

    WHEN 'MBLNR' OR 'MJAHR'.

      SET PARAMETER ID 'MBN' FIELD lw_saida_alv-mblnr.
      SET PARAMETER ID 'MJA' FIELD lw_saida_alv-mjahr.
      SET PARAMETER ID 'BUK' FIELD lw_saida_alv-bukrs.

      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          i_action            = 'A04'
          i_refdoc            = 'R02'
          i_notree            = 'X'
          i_no_auth_check     = ' '
          i_deadend           = 'X'
          i_skip_first_screen = 'X'
          i_okcode            = 'OK_GO'
          i_mblnr             = lw_saida_alv-mblnr
          i_mjahr             = lw_saida_alv-mjahr
        EXCEPTIONS
          illegal_combination = 0
          OTHERS              = 0.

    WHEN 'BELNR' OR 'GJAHR'.

      SET PARAMETER ID 'RBN' FIELD lw_saida_alv-belnr.

      SET PARAMETER ID 'GJR' FIELD lw_saida_alv-gjahr.

      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " F_HYPERLINK

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
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita  USING p_fieldname TYPE slis_fieldname
                           p_text TYPE scrtext_l.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-seltext_s = p_text.
  <fs_cat>-seltext_m = p_text.
  <fs_cat>-seltext_l = p_text.
  <fs_cat>-reptext_ddic = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VERIFICA_LINHA_SELEC
*&---------------------------------------------------------------------*
FORM f_verifica_linha_selec CHANGING p_error TYPE c.

  READ TABLE gt_dados_alv WITH KEY selec = 'X' TRANSPORTING NO FIELDS.

  IF sy-subrc NE 0.
    MESSAGE s851(v4) DISPLAY LIKE 'E'.
    p_error = 'X'.
  ENDIF.

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

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.



  ENDLOOP.

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
*&      Form  F_FILL_FIELDCAT1
*&---------------------------------------------------------------------*
FORM f_fill_fieldcat1 .

* Build the fieldcat according to DDIC structure
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZIBS_NFSE_001'
    CHANGING
      ct_fieldcat      = gt_fieldcatalog1.

  DELETE gt_fieldcatalog1 WHERE fieldname = 'SELEC'.

  PERFORM f_fieldcat_prop
    USING 'LBLNI_FLAG' 'ICON' 'X'
 CHANGING gt_fieldcatalog1.

  PERFORM f_fieldcat_prop
  USING 'LBLNI_FLAG' 'OUTPUTLEN' '2'
CHANGING gt_fieldcatalog1.

  PERFORM f_fieldcat_prop
    USING 'STATUS_ICON' 'ICON' 'X'
 CHANGING gt_fieldcatalog1.

  PERFORM f_fieldcat_prop
    USING 'STATUS_ICON' 'OUTPUTLEN' '2'
 CHANGING gt_fieldcatalog1.

  PERFORM f_fieldcat_prop
    USING 'LOG' 'ICON' 'X'
 CHANGING gt_fieldcatalog1.

  PERFORM f_fieldcat_prop
    USING 'LAST_STEPSTATUS' 'NO_OUT' 'X'
 CHANGING gt_fieldcatalog1.

  PERFORM f_fieldcat_prop
    USING 'CANCEL' 'CHECKBOX' 'X'
 CHANGING gt_fieldcatalog1.


  READ TABLE gt_fieldcatalog1 ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = 'LOG'.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext = 'Log'.
    <fs_fcat>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fieldcatalog1 ASSIGNING <fs_fcat>
    WITH KEY fieldname = 'STATUS_ICON'.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext = 'Status'.
    <fs_fcat>-dd_outlen = 000010.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM f_handle_hotspot_click USING
                             VALUE(row_id)    LIKE lvc_s_roid-row_id
                             VALUE(fieldname) LIKE lvc_s_col-fieldname.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
FORM f_processa_dados .

  DATA lv_manual TYPE c.
  DATA lv_cont   TYPE i.
  "data lt_nfse_001 TYPE TABLE OF zibt_nfse_001.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados_alv>).

    DATA(vl_tabix) = sy-tabix.

    READ TABLE gt_nfse_001 ASSIGNING FIELD-SYMBOL(<fs_nfse_001>)
                           WITH KEY guid_header = <fs_dados_alv>-guid_header.

*Inicio Alteração - Leandro Valentim Ferreira - #114554
    IF sy-subrc EQ 0.
      IF s_ebeln[] IS NOT INITIAL.
        READ TABLE s_ebeln TRANSPORTING NO FIELDS WITH KEY low = <fs_nfse_001>-ebeln.
        IF sy-subrc NE 0.
          DELETE gt_dados_alv INDEX vl_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF s_mblnr[] IS NOT INITIAL.
        READ TABLE s_mblnr TRANSPORTING NO FIELDS WITH KEY low = <fs_nfse_001>-mblnr.
        IF sy-subrc NE 0.
          DELETE gt_dados_alv INDEX vl_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF s_lblni[] IS NOT INITIAL.
        READ TABLE s_lblni TRANSPORTING NO FIELDS WITH KEY low = <fs_nfse_001>-lblni.
        IF sy-subrc NE 0.
          DELETE gt_dados_alv INDEX vl_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF s_belnr[] IS NOT INITIAL.
        READ TABLE s_belnr TRANSPORTING NO FIELDS WITH KEY low = <fs_nfse_001>-belnr.
        IF sy-subrc NE 0.
          DELETE gt_dados_alv INDEX vl_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF s_nsmiro[] IS NOT INITIAL.
        READ TABLE s_nsmiro  TRANSPORTING NO FIELDS WITH KEY low = <fs_nfse_001>-se_recordid.
        IF sy-subrc NE 0.
          DELETE gt_dados_alv INDEX vl_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.

    ELSE.
      IF s_ebeln[] IS NOT INITIAL.
        DELETE gt_dados_alv INDEX vl_tabix.
        CONTINUE.
      ENDIF.
      IF s_mblnr[] IS NOT INITIAL.
        DELETE gt_dados_alv INDEX vl_tabix.
        CONTINUE.
      ENDIF.
      IF s_lblni[] IS NOT INITIAL.
        DELETE gt_dados_alv INDEX vl_tabix.
        CONTINUE.
      ENDIF.
      IF s_belnr[] IS NOT INITIAL.
        DELETE gt_dados_alv INDEX vl_tabix.
        CONTINUE.
      ENDIF.
      IF s_docnum[] IS NOT INITIAL.
        DELETE gt_dados_alv INDEX vl_tabix.
        CONTINUE.
      ENDIF.
      IF s_nsmiro[] IS NOT INITIAL.
        DELETE gt_dados_alv INDEX vl_tabix.
        CONTINUE.
      ENDIF.
    ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - #114554

    <fs_dados_alv>-log         = '@96@'. "Ícone histórico
    lv_cont = 0.
    LOOP AT gt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002>) WHERE guid_header = <fs_dados_alv>-guid_header.
      IF <fs_nfse_002>-lblni IS NOT INITIAL OR <fs_nfse_002>-mblnr IS NOT INITIAL.
        ADD 1 TO lv_cont.
      ENDIF.
    ENDLOOP.

    IF lv_cont EQ 0.
      <fs_dados_alv>-lblni_flag = icon_incomplete.
    ELSEIF lv_cont GT 1.
      <fs_dados_alv>-lblni_flag = icon_positive.
    ELSE.
      <fs_dados_alv>-lblni_flag = icon_checked.
    ENDIF.

    CASE <fs_dados_alv>-last_stepstatus.
      WHEN '13' OR '86' OR '87' OR '88'.
        "<fs_dados_alv>-status_icon = '@AH@'.

        PERFORM f_build_icon
          USING '@AH@'
                <fs_dados_alv>-last_stepstatus
       CHANGING <fs_dados_alv>-status_icon.

      WHEN '03' OR '89'.
        "<fs_dados_alv>-status_icon = '@AG@'.

        PERFORM f_build_icon
          USING '@AG@'
                <fs_dados_alv>-last_stepstatus
        CHANGING <fs_dados_alv>-status_icon.

      WHEN '05'.
        "<fs_dados_alv>-status_icon = '@5E@'.

        PERFORM f_build_icon
          USING '@5E@'
                <fs_dados_alv>-last_stepstatus
        CHANGING <fs_dados_alv>-status_icon.

      WHEN '100' OR '101'.
        "<fs_dados_alv>-status_icon = '@DF@'.

        PERFORM f_build_icon
          USING '@DF@'
                <fs_dados_alv>-last_stepstatus
        CHANGING <fs_dados_alv>-status_icon.

      WHEN '102'.
        "<fs_dados_alv>-status_icon = '@11@'.

        PERFORM f_build_icon
          USING '@11@'
                <fs_dados_alv>-last_stepstatus
        CHANGING <fs_dados_alv>-status_icon.

      WHEN '103'.
        "<fs_dados_alv>-status_icon = '@BA@'.

        PERFORM f_build_icon
          USING '@BA@'
                <fs_dados_alv>-last_stepstatus
        CHANGING <fs_dados_alv>-status_icon.

      WHEN OTHERS.
        "<fs_dados_alv>-status_icon = '@AH@'.

        PERFORM f_build_icon
          USING '@AH@'
                <fs_dados_alv>-last_stepstatus
        CHANGING <fs_dados_alv>-status_icon.

    ENDCASE.

    PERFORM f_dados_linha CHANGING <fs_dados_alv> lv_manual.

    IF lv_manual = 'X'.
    ENDIF.

  ENDLOOP.

  CASE 'X'.
    WHEN p_all.

    WHEN p_pend.
      DELETE gt_dados_alv WHERE cancel = 'X'.
      DELETE gt_dados_alv WHERE belnr_fret IS NOT INITIAL OR  belnr IS NOT INITIAL.
    WHEN p_final.
      DELETE gt_dados_alv WHERE cancel = 'X'.
*      DELETE gt_dados_alv WHERE last_stepstatus = '101'.
      DELETE gt_dados_alv WHERE belnr_fret IS INITIAL AND  belnr IS INITIAL.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_PROP
*&---------------------------------------------------------------------*
FORM f_fieldcat_prop USING p_fieldname TYPE c
                           p_column TYPE c
                           p_value TYPE c
                  CHANGING p_cat_tab TYPE lvc_t_fcat.

  DATA lv_field TYPE c LENGTH 100.

  READ TABLE p_cat_tab ASSIGNING FIELD-SYMBOL(<fs_fieldcat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  lv_field = '<FS_FIELDCAT>-' && p_column.

  ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_column>).

  CHECK sy-subrc EQ 0.

  <fs_column> = p_value.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SELECTED_LINE
*&---------------------------------------------------------------------*
FORM f_get_selected_line.

  DATA: lo_selections TYPE REF TO cl_salv_selections.

  CLEAR gv_erro.

  CHECK sy-ucomm(1) NE '&'.

  CHECK go_alv_grid1 IS BOUND.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.
    CLEAR <fs_alv>-selec.
  ENDLOOP.

  CALL METHOD go_alv_grid1->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_rows)
      et_row_no     = DATA(lt_row_no).

  IF lt_rows IS INITIAL.

    gv_erro = 'X'.

    PERFORM f_put_mensagem USING 'E' 'Selecionar uma linha para prosseguir'.

  ENDIF.

  LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_row>).

    READ TABLE gt_dados_alv ASSIGNING <fs_alv>
      INDEX <fs_row>-index.

    CHECK sy-subrc EQ 0.

    <fs_alv>-selec = 'X'.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_ALV
*&---------------------------------------------------------------------*
FORM f_refresh_alv .

  DATA lw_stable TYPE lvc_s_stbl.

  "PERFORM f_seleciona.

*Inicio Alteração - Leandro Valentim Ferreira - 21.06.23 - #114554
  IF gt_nfse_001[] IS NOT INITIAL.
    SELECT *
           INTO TABLE @DATA(tl_zibt_nfse_003)
           FROM zibt_nfse_003
           FOR ALL ENTRIES IN @gt_nfse_001
           WHERE guid_header EQ @gt_nfse_001-guid_header.

    IF sy-subrc EQ 0.
      SORT tl_zibt_nfse_003 BY guid_header.
      SELECT *
             INTO TABLE @DATA(tl_zibt_nfse_002)
             FROM zibt_nfse_002
             FOR ALL ENTRIES IN @tl_zibt_nfse_003
             WHERE guid_header EQ @tl_zibt_nfse_003-guid_header.

      SORT tl_zibt_nfse_002 BY guid_header.
      LOOP AT tl_zibt_nfse_003 INTO DATA(wl_zibt_nfse_003).
        READ TABLE tl_zibt_nfse_002 TRANSPORTING NO FIELDS
                                    WITH KEY guid_header = wl_zibt_nfse_003-guid_header
                                    BINARY SEARCH.
        IF sy-subrc NE 0.
          DELETE FROM zibt_nfse_003 WHERE guid_header EQ wl_zibt_nfse_003-guid_header.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 21.06.23 - #114554

  lw_stable-row = 'X'.
  lw_stable-col = 'X'.

  CHECK go_alv_grid1 IS BOUND.

  CALL METHOD go_alv_grid1->refresh_table_display
    EXPORTING
      is_stable      = lw_stable
      i_soft_refresh = 'X'
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VIEW_PDF
*&---------------------------------------------------------------------*
FORM f_view_pdf.

  DATA: lt_image_tab TYPE w3mimetabtype,
        lv_size      TYPE i.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.

    CALL FUNCTION '/TCSR/F_PDF_DISPLAY'
      EXPORTING
        iv_guid_header    = <fs_alv>-guid_header
        iv_op_type        = '2'
      IMPORTING
        ev_image_size     = lv_size
        et_image_tab      = lt_image_tab
      EXCEPTIONS
        pdf_not_found     = 1
        error_display_pdf = 2
        OTHERS            = 3.

    EXIT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VIEW_XML
*&---------------------------------------------------------------------*
FORM f_view_xml.

  DATA: lv_xml_content TYPE xstring,
        lo_dom         TYPE REF TO if_ixml_document.


  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.

    SELECT SINGLE xmlstring
      FROM /tcsr/t_xml
      INTO @DATA(rv_xml)
      WHERE guid_header = @<fs_alv>-guid_header .

    MOVE rv_xml TO lv_xml_content.

    CHECK lv_xml_content IS NOT INITIAL.

    CALL FUNCTION 'SDIXML_XML_TO_DOM'
      EXPORTING
        xml           = lv_xml_content
      IMPORTING
        document      = lo_dom
      EXCEPTIONS
        invalid_input = 1
        OTHERS        = 2.
    IF sy-subrc NE 0.
      CLEAR: lo_dom.
    ENDIF.

    IF lo_dom IS NOT INITIAL.
      CALL FUNCTION 'SDIXML_DOM_TO_SCREEN'
        EXPORTING
          document = lo_dom
        EXCEPTIONS
          OTHERS   = 1.
      IF sy-subrc NE 0.
        CLEAR lo_dom.
      ENDIF.
    ENDIF.

    EXIT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_PDF
*&---------------------------------------------------------------------*
FORM f_download_pdf .

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.

    PERFORM f_download_pdf_line USING  <fs_alv>-guid_header.
    EXIT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_DOWNLOAD_PDF_LINE
*&---------------------------------------------------------------------*
FORM f_download_pdf_line USING p_guid_header TYPE /tcsr/e_guid_header.

  CONSTANTS: lc_file_type TYPE char10 VALUE 'BIN'.

  DATA: lt_image_tab       TYPE w3mimetabtype,
        lv_selected_folder TYPE string,
        lv_file_name       TYPE string,
        lv_size            TYPE i.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = CONV #( TEXT-m02 ) "Diretório
      initial_folder       = 'C:\'
    CHANGING
      selected_folder      = lv_selected_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  CALL FUNCTION '/TCSR/F_PDF_DISPLAY'
    EXPORTING
      iv_guid_header    = p_guid_header
      iv_op_type        = '1'
    IMPORTING
      ev_image_size     = lv_size
      et_image_tab      = lt_image_tab
    EXCEPTIONS
      pdf_not_found     = 1
      error_display_pdf = 2
      OTHERS            = 3.
  IF sy-subrc EQ 0.

    CONCATENATE lv_selected_folder '\' 'NFSe.pdf'
           INTO lv_file_name.

    " Download xml to the selected destination
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename          = lv_file_name
        bin_filesize      = lv_size
        filetype          = lc_file_type
      CHANGING
        data_tab          = lt_image_tab
      EXCEPTIONS
        file_write_error  = 1
        no_authority      = 5
        unknown_error     = 6
        access_denied     = 15
        OTHERS            = 24 ).

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PUT_MENSAGEM
*&---------------------------------------------------------------------*
FORM f_put_mensagem USING p_type TYPE c
                          p_string TYPE string.

  DATA lw_bapiret TYPE bapiret2.

  DATA lv_msgty TYPE sy-msgty.

  CHECK p_string IS NOT INITIAL.

  IF p_type IS INITIAL.
    lv_msgty = 'E'.
  ELSE.
    lv_msgty = p_type.
  ENDIF.

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
        lw_bapiret-message_v1 = <fs_line>.
      WHEN 2.
        lw_bapiret-message_v2 = <fs_line>.
      WHEN 3.
        lw_bapiret-message_v3 = <fs_line>.
      WHEN 4.
        lw_bapiret-message_v4 = <fs_line>.
    ENDCASE.

  ENDLOOP.

  lw_bapiret-id = 'DS'.
  lw_bapiret-type = lv_msgty.
  lw_bapiret-number = '016'.

  MESSAGE ID lw_bapiret-id TYPE 'S' NUMBER lw_bapiret-number
    WITH lw_bapiret-message_v1 lw_bapiret-message_v2
         lw_bapiret-message_v3 lw_bapiret-message_v4 DISPLAY LIKE lw_bapiret-type.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_XML
*&---------------------------------------------------------------------*
FORM f_download_xml.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.

    PERFORM f_download_xml_line USING <fs_alv>-guid_header.
    EXIT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_XML_LINE
*&---------------------------------------------------------------------*
FORM f_download_xml_line USING p_guid_header TYPE /tcsr/e_guid_header. .

  CONSTANTS: lc_file_type TYPE char10 VALUE 'BIN'.

  DATA: lv_xml_content     TYPE xstring,
        lv_string          TYPE xstring,
        lv_size            TYPE i,
        lv_selected_folder TYPE string,
        lv_file_name       TYPE string,
        lt_xml_tab         TYPE dcxmllines,
        lo_dom             TYPE REF TO if_ixml_document.

  SELECT SINGLE xmlstring
    FROM /tcsr/t_xml
      INTO @DATA(xml_content)
        WHERE guid_header = @p_guid_header.

  CHECK sy-subrc EQ 0.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = CONV #( TEXT-m02 ) "Diretório
      initial_folder       = 'C:\'
    CHANGING
      selected_folder      = lv_selected_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  MOVE xml_content TO lv_xml_content.

  CHECK lv_xml_content IS NOT INITIAL.

  CALL FUNCTION 'SDIXML_XML_TO_DOM'
    EXPORTING
      xml           = lv_xml_content
    IMPORTING
      document      = lo_dom
    EXCEPTIONS
      invalid_input = 1
      OTHERS        = 2.
  IF sy-subrc NE 0.
    CLEAR: lo_dom.
  ENDIF.

  REFRESH: lt_xml_tab[].

  " Convert DOM to XML doc (table)
  CALL FUNCTION 'SDIXML_DOM_TO_XML'
    EXPORTING
      document      = lo_dom
      pretty_print  = ' '
    IMPORTING
      xml_as_string = lv_string
      size          = lv_size
    TABLES
      xml_as_table  = lt_xml_tab
    EXCEPTIONS
      no_document   = 1
      OTHERS        = 2.

  CONCATENATE lv_selected_folder '\' 'NFSe.xml'
         INTO lv_file_name.

  cl_gui_frontend_services=>gui_download(
    EXPORTING
      filename          = lv_file_name
      bin_filesize      = lv_size
      filetype          = lc_file_type
    CHANGING
      data_tab          = lt_xml_tab
    EXCEPTIONS
      file_write_error  = 1
      no_authority      = 5
      unknown_error     = 6
      access_denied     = 15
      OTHERS            = 24 ).



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EDIT_DOC
*&---------------------------------------------------------------------*
FORM f_edit_doc.

  DATA lo_util TYPE REF TO /tcsr/c_util_monitor.
  DATA lv_toler TYPE /tcsr/e_tolerance_tax.
  DATA lw_header TYPE  zsheader_data_nfse_inbound.
  DATA lw_payment TYPE  zspayment_data_nfse_inbound.
  DATA lt_cond_items TYPE  /tcsr/y_cond_list.
  DATA lt_scr_popup_list TYPE /tcsr/y_po_list.

  DATA lv_edit TYPE flag.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.

    MOVE-CORRESPONDING <fs_alv> TO lw_header.
    MOVE-CORRESPONDING <fs_alv> TO lw_payment.

    CREATE OBJECT lo_util
      EXPORTING
        iv_guid_header = <fs_alv>-guid_header.

    CHECK lo_util IS BOUND.

    TRY .
        lo_util->get_xml( ).
      CATCH /tcsr/cx_exception INTO DATA(lo_exception).
        "Display Error message
        MESSAGE ID lo_exception->if_t100_message~t100key-msgid
           TYPE 'E'
         NUMBER lo_exception->if_t100_message~t100key-msgno
           WITH lo_exception->if_t100_message~t100key-attr1
                lo_exception->if_t100_message~t100key-attr2
                lo_exception->if_t100_message~t100key-attr3
                lo_exception->if_t100_message~t100key-attr4.
    ENDTRY.

    SELECT SINGLE value
      INTO @DATA(lv_toler_s)
      FROM /tcsr/t_param
      WHERE programm = 'PROCESS_STEPS'
        AND name     = 'TOLERANCE_TAX'.

    IF lv_toler IS INITIAL.
      lv_toler = '0.50'.
    ELSE.
      REPLACE ',' IN lv_toler_s WITH '.'.
      lv_toler = lv_toler_s.
    ENDIF.

    lo_util->get_scr_cond_list(
      EXPORTING
        iv_tolerance_tax  = lv_toler
      IMPORTING
        et_scr_cond_list  = lt_cond_items[]
      CHANGING
        it_po_list        = lt_scr_popup_list[]
        ).

    IF <fs_alv>-lifnr IS NOT INITIAL.
      SELECT bvtyp bankl bankn FROM lfbk UP TO 1 ROWS
          INTO CORRESPONDING FIELDS OF lw_payment
            WHERE lifnr = <fs_alv>-lifnr ORDER BY bvtyp ASCENDING. ENDSELECT.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE banka
          FROM bnka
          INTO CORRESPONDING FIELDS OF lw_payment
          WHERE bankl = lw_payment-bankl.
      ENDIF.
    ENDIF.

    READ TABLE gt_nfse_001 ASSIGNING FIELD-SYMBOL(<fs_nfse_001>)
      WITH KEY guid_header = <fs_alv>-guid_header.

    IF sy-subrc EQ 0.

      MOVE-CORRESPONDING <fs_nfse_001> TO lw_header.
      MOVE-CORRESPONDING <fs_nfse_001> TO lw_payment.

    ENDIF.

    CLEAR lv_edit.
    IF sy-ucomm = 'STATUS_DOC'.
      IF <fs_alv>-lblni IS INITIAL AND <fs_alv>-mblnr IS INITIAL AND <fs_alv>-belnr_fret IS INITIAL.
        lv_edit = 'X'.
      ELSE.
        lv_edit = space.
      ENDIF.
    ELSEIF sy-ucomm = 'EDIT_DOC'.
      IF <fs_alv>-lblni IS INITIAL AND <fs_alv>-mblnr IS INITIAL AND <fs_alv>-belnr_fret IS INITIAL.
        lv_edit = 'P'.
      ELSE.
        lv_edit = space.
      ENDIF.
    ENDIF.

    PERFORM f_form_pagamento
      USING lw_header
   CHANGING lw_payment.

    " 02.02.2023 - RAMON 102826 -->>
    IF <fs_nfse_001> IS ASSIGNED.

      IF <fs_nfse_001>-zbvtyp IS NOT INITIAL.

        lw_payment-pymt_meth = <fs_nfse_001>-pymt_meth.
        lw_payment-boleto = <fs_nfse_001>-boleto.
        lw_payment-obs_financeira = <fs_nfse_001>-obs_financeira.
        lw_payment-hbkid = <fs_nfse_001>-housebankid.
        lw_payment-bvtyp = <fs_nfse_001>-zbvtyp.
*lw_payment-BANKN = '99999-6'. """""""""""""""""
*lw_payment-BANKL  = '34170499' ."""""""""""""""""
*lw_payment-BANKA = Banco Itáu S.A.

      ENDIF.

    ENDIF.

    " 02.02.2023 - RAMON 102826 --<<

    CALL FUNCTION 'ZMM_NFSE_EXIBE_NF'
      EXPORTING
        i_edit               = lv_edit
        i_toler              = lv_toler
        it_cond_items        = lt_cond_items
        i_bukrs              = <fs_alv>-bukrs "USER STORY 158527 - MMSILVA - 17.01.2025
      CHANGING
        c_header             = lw_header
        c_payment            = lw_payment
        c_nfse               = <fs_alv>
      EXCEPTIONS
        nota_duplicada       = 1
        nota_duplic_recusada = 2
        OTHERS               = 2.
*** Inicio - Rubenilson  - 29.08.24 - US147735
    IF sy-subrc EQ '1'.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = ''
          txt1  = 'Nota fiscal duplicada, solicite autorização junto ao CSC Fiscal'
          txt2  = 'Após aprovação "Gerar Folha de Serviço" novamente'.

      RETURN.

    ELSEIF sy-subrc EQ  '2'.

      MESSAGE 'Documento não autorizado para lançamento' TYPE 'I'.

      RETURN.
    ENDIF.
*** Fim - Rubenilson  - 29.08.24 - US147735

  ENDLOOP.

  PERFORM f_seleciona.
  PERFORM f_processa_dados.
  PERFORM f_refresh_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILL_FIELDCAT1
*&---------------------------------------------------------------------*
FORM f_fill_fieldcat2 .

* Build the fieldcat according to DDIC structure
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZIBS_NFSE_005'
    CHANGING
      ct_fieldcat      = gt_fieldcatalog2.

  DELETE gt_fieldcatalog2 WHERE fieldname = 'SELEC'.

  PERFORM f_fieldcat_prop
    USING 'MSGTY_ICON' 'ICON' 'X'
 CHANGING gt_fieldcatalog2.

  PERFORM f_fieldcat_prop USING 'GUID_HEADER' 'NO_OUT' 'X' CHANGING gt_fieldcatalog2.
  PERFORM f_fieldcat_prop USING 'MSGTY' 'NO_OUT' 'X' CHANGING gt_fieldcatalog2.
  PERFORM f_fieldcat_prop USING 'MSGV1' 'NO_OUT' 'X' CHANGING gt_fieldcatalog2.
  PERFORM f_fieldcat_prop USING 'MSGV2' 'NO_OUT' 'X' CHANGING gt_fieldcatalog2.
  PERFORM f_fieldcat_prop USING 'MSGV3' 'NO_OUT' 'X' CHANGING gt_fieldcatalog2.
  PERFORM f_fieldcat_prop USING 'MSGV4' 'NO_OUT' 'X' CHANGING gt_fieldcatalog2.

*  PERFORM f_fieldcat_prop
*    USING 'CANCEL' 'CHECKBOX' 'X'
* CHANGING gt_fieldcatalog1.

  READ TABLE gt_fieldcatalog2 ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = 'MSGTY_ICON'.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext = 'Log'.
    <fs_fcat>-dd_outlen = 000010.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ON_CLICK
*&---------------------------------------------------------------------*
FORM f_on_click USING e_row TYPE lvc_s_row
                      e_column TYPE lvc_s_col
                      es_row_no TYPE lvc_s_roid.

  IF e_column = 'EBELN'
    OR e_column = 'BELNR'
    OR e_column = 'BELNR_FRET'
    OR e_column = 'LBLNI'
    OR e_column = 'LBLNI_FLAG'
    OR e_column = 'PACKNO'
    OR e_column = 'MBLNR'
    OR e_column = 'GJAHR'
    OR e_column = 'GJAHR_FRET'
    OR e_column = 'DOCNUM'
    OR e_column = 'LOG'.

    PERFORM f_click_hotspot USING e_column es_row_no-row_id.

    EXIT.

  ENDIF.

  PERFORM f_demark_row.
  PERFORM f_mark_row USING es_row_no-row_id.
  PERFORM f_edit_doc.
  PERFORM f_refresh_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_NFSE
*&---------------------------------------------------------------------*
FORM f_update_nfse .

  DATA lt_nfse_001 TYPE TABLE OF zibt_nfse_001.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.

    APPEND INITIAL LINE TO lt_nfse_001 ASSIGNING FIELD-SYMBOL(<fs_001>).

    MOVE-CORRESPONDING <fs_alv> TO <fs_001>.

  ENDLOOP.

  CHECK lt_nfse_001 IS NOT INITIAL.

  MODIFY zibt_nfse_001 FROM TABLE lt_nfse_001.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MARK_ROW
*&---------------------------------------------------------------------*
FORM f_mark_row  USING p_row TYPE i.

  READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
    INDEX p_row.

  CHECK sy-subrc EQ 0.

  <fs_alv>-selec = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CLEAR_SELECT_0005
*&---------------------------------------------------------------------*
FORM f_demark_row .

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.
    CLEAR <fs_alv>-selec.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_LOG
*&---------------------------------------------------------------------*
FORM f_exibe_log .

*  CHECK go_alv_grid2 IS NOT INITIAL.

  gw_layout2-sel_mode    = 'A'.
  gw_layout2-col_opt = 'X'.
  gw_layout2-cwidth_opt = 'X'.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.

    CHECK <fs_alv>-guid_header IS NOT INITIAL.

    PERFORM f_get_log USING <fs_alv>-guid_header.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '9000'
        i_show        = 'X'
        i_repid       = sy-repid
        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.

*    PERFORM f_fill_fieldcat2.
*
*    CALL METHOD go_alv_grid2->set_table_for_first_display
*      EXPORTING
*        is_layout       = gw_layout2
*        i_save          = 'A'
*        "it_toolbar_excluding = gt_exclude_fcode
*      CHANGING
*        it_fieldcatalog = gt_fieldcatalog2
*        it_outtab       = gt_nfse_005.

    EXIT.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_LOG
*&---------------------------------------------------------------------*
FORM f_exibe_log2 USING p_index TYPE i.

  CHECK go_alv_grid2 IS NOT INITIAL.

  gw_layout2-sel_mode    = 'A'.
  gw_layout2-col_opt = 'X'.
  gw_layout2-cwidth_opt = 'X'.

  READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
    INDEX p_index.

  CHECK sy-subrc EQ 0.

  CHECK <fs_alv>-guid_header IS NOT INITIAL.

  PERFORM f_get_log USING <fs_alv>-guid_header.

  PERFORM f_fill_fieldcat2.

  CALL METHOD go_alv_grid2->set_table_for_first_display
    EXPORTING
      is_layout       = gw_layout2
      i_save          = 'A'
      "it_toolbar_excluding = gt_exclude_fcode
    CHANGING
      it_fieldcatalog = gt_fieldcatalog2
      it_outtab       = gt_nfse_005.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita2  USING p_fieldname TYPE slis_fieldname
                            p_text_s TYPE scrtext_s
                            p_text_l TYPE scrtext_l
                   CHANGING p_field_cat TYPE lvc_t_fcat.

  READ TABLE p_field_cat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-scrtext_s  = p_text_s.
  <fs_cat>-scrtext_m = p_text_s.
  <fs_cat>-scrtext_l = p_text_l.
  <fs_cat>-reptext = p_text_s.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_LOG
*&---------------------------------------------------------------------*
FORM f_get_log USING p_guid TYPE /tcsr/e_guid_header.

  CHECK p_guid IS NOT INITIAL.

  SELECT * FROM zibt_nfse_005
    INTO CORRESPONDING FIELDS OF TABLE gt_nfse_005
      WHERE guid_header = p_guid.

  REFRESH tg_msg_ret.
  LOOP AT gt_nfse_005 ASSIGNING FIELD-SYMBOL(<fs_005>).

    CASE <fs_005>-msgty.
      WHEN 'S'.
        <fs_005>-msgty_icon = '@08@'.
      WHEN 'E'.
        <fs_005>-msgty_icon = '@0A@'.
      WHEN 'W' OR 'I'.
        <fs_005>-msgty_icon = '@09@'.
    ENDCASE.

    MOVE: <fs_005>-message                   TO tg_msg_ret-msg,
          ''                                 TO tg_msg_ret-aba,
          <fs_005>-msgty_icon                TO tg_msg_ret-icon.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR
*&---------------------------------------------------------------------*
FORM f_estornar .

  DATA lv_answer TYPE c.
  DATA lo_znfse TYPE REF TO zcl_nfse_inbound.

  CLEAR gt_bapiret2.

  PERFORM f_popup_to_confirm
    USING 'Confirmar Estorno?'
 CHANGING lv_answer.

  CHECK lv_answer = '1'.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.

    CREATE OBJECT lo_znfse
      EXPORTING
        i_guid = <fs_alv>-guid_header.

    lo_znfse->estorno( ).

    APPEND LINES OF lo_znfse->mt_mess TO gt_bapiret2.

  ENDLOOP.

  PERFORM f_mensagem_exibe_popup USING gt_bapiret2.

  PERFORM f_seleciona.
  PERFORM f_processa_dados.
  PERFORM f_refresh_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CANCELAR
*&---------------------------------------------------------------------*
FORM f_cancelar .

  DATA lv_answer TYPE c.
  DATA lo_znfse TYPE REF TO zcl_nfse_inbound.

  CLEAR gt_bapiret2.

  PERFORM f_popup_to_confirm
    USING 'Confirmar cancelamento?'
 CHANGING lv_answer.

  CHECK lv_answer = '1'.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.

    CREATE OBJECT lo_znfse
      EXPORTING
        i_guid = <fs_alv>-guid_header.

    lo_znfse->cancelar( ).

    APPEND LINES OF lo_znfse->mt_mess TO gt_bapiret2.

  ENDLOOP.

  PERFORM f_mensagem_exibe_popup USING gt_bapiret2.

  PERFORM f_seleciona.
  PERFORM f_processa_dados.
  PERFORM f_refresh_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_ICON
*&---------------------------------------------------------------------*
FORM f_build_icon USING p_icon TYPE c
                        p_status TYPE /tcsr/e_stepstatus
               CHANGING p_icon_out TYPE c.

  DATA lv_info TYPE c LENGTH 35.

  CHECK p_status IS NOT INITIAL.

  READ TABLE gt_status ASSIGNING FIELD-SYMBOL(<fs_status>)
    WITH KEY domvalue_l = p_status.

  CHECK sy-subrc EQ 0.

  lv_info = <fs_status>-ddtext.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = p_icon
      info   = lv_info
    IMPORTING
      result = p_icon_out.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DADOS_LINHA
*&---------------------------------------------------------------------*
FORM f_dados_linha CHANGING p_linha TYPE zibs_nfse_001
                            p_manual TYPE c.

  " dados de fornecedor ----------------------
  READ TABLE gt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
    WITH KEY lifnr = p_linha-lifnr.

  IF sy-subrc EQ 0.

    p_linha-name1 = <fs_lfa1>-name1.
    p_linha-stcd1 = <fs_lfa1>-stcd1.
    p_linha-stcd2 = <fs_lfa1>-stcd2.
    p_linha-stcd3 = <fs_lfa1>-stcd3.

  ENDIF.

  " dados de documentos gerados

  READ TABLE gt_nfse_001 ASSIGNING FIELD-SYMBOL(<fs_nfse_001>)
    WITH KEY guid_header = p_linha-guid_header.

  IF sy-subrc EQ 0.

    MOVE-CORRESPONDING <fs_nfse_001> TO p_linha.

    " 04.04.2023 - RAMON 107971 -->>
    IF <fs_nfse_001>-nao_retem_iss = 'X'.
      p_linha-nao_reter_iss = 'X'.
    ENDIF.
    " 04.04.2023 - RAMON 107971 --<<

  ENDIF.

  LOOP AT gt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002>) WHERE guid_header = p_linha-guid_header.
    IF sy-subrc EQ 0.
      p_linha-ebeln = <fs_nfse_002>-ebeln.
      IF <fs_nfse_002>-lblni IS NOT INITIAL.
        p_linha-lblni  = <fs_nfse_002>-lblni.
        p_linha-packno = <fs_nfse_002>-packno.
      ENDIF.
      IF <fs_nfse_002>-mblnr IS NOT INITIAL.
        p_linha-mblnr = <fs_nfse_002>-mblnr.
        p_linha-mjahr = <fs_nfse_002>-mjahr.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM f_estorno_manual CHANGING p_linha p_manual.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CLICK_HOTSPOT
*&---------------------------------------------------------------------*
FORM f_click_hotspot USING p_column TYPE lvc_s_col
                           p_index TYPE i.

  READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>)
    INDEX p_index.

  CHECK sy-subrc EQ 0.

  CASE p_column.
    WHEN 'LOG'.
      PERFORM f_exibe_log2 USING p_index.
    WHEN 'EBELN'.

      CHECK <fs_dados>-ebeln IS NOT INITIAL.

      CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
        EXPORTING
          i_ebeln = <fs_dados>-ebeln.

    WHEN 'LBLNI_FLAG'.
      CHECK <fs_dados>-lblni_flag = icon_positive.
      REFRESH it_docs_alv.
      LOOP AT gt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002>) WHERE guid_header =  <fs_dados>-guid_header.
        MOVE-CORRESPONDING <fs_nfse_002> TO wa_docs.
        APPEND wa_docs TO it_docs_alv.
      ENDLOOP.
      CALL SCREEN 9100 STARTING AT 050 3
                         ENDING AT 80 10.

    WHEN 'LBLNI' OR 'PACKNO'.

      CHECK <fs_dados>-lblni IS NOT INITIAL.

      SET PARAMETER ID 'BES' FIELD ' '.
      SET PARAMETER ID 'LBL' FIELD <fs_dados>-lblni.
      SET PARAMETER ID 'LBD' FIELD 'X'.

      CALL TRANSACTION 'ML81N' AND SKIP FIRST SCREEN.

    WHEN 'MBLNR' OR 'MJAHR'.

      CHECK <fs_dados>-mblnr IS NOT INITIAL.

      SET PARAMETER ID 'MBN' FIELD <fs_dados>-mblnr.
      SET PARAMETER ID 'MJA' FIELD <fs_dados>-mjahr.
      SET PARAMETER ID 'BUK' FIELD <fs_dados>-bukrs.

      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          i_action            = 'A04'
          i_refdoc            = 'R02'
          i_notree            = 'X'
          i_no_auth_check     = ' '
          i_deadend           = 'X'
          i_skip_first_screen = 'X'
          i_okcode            = 'OK_GO'
          i_mblnr             = <fs_dados>-mblnr
          i_mjahr             = <fs_dados>-mjahr
        EXCEPTIONS
          illegal_combination = 0
          OTHERS              = 0.

    WHEN 'BELNR' OR 'GJAHR'.

      CHECK <fs_dados>-belnr IS NOT INITIAL.

      SET PARAMETER ID 'RBN' FIELD <fs_dados>-belnr.

      SET PARAMETER ID 'GJR' FIELD <fs_dados>-gjahr.

      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.


    WHEN 'BELNR_FRET' OR 'GJAHR_FRET'.

      CHECK <fs_dados>-belnr_fret IS NOT INITIAL.

      SET PARAMETER ID 'RBN' FIELD <fs_dados>-belnr_fret.

      SET PARAMETER ID 'GJR' FIELD <fs_dados>-gjahr_fret.



    WHEN 'DOCNUM'.

      CHECK <fs_dados>-docnum IS NOT INITIAL.

      SET PARAMETER ID 'JEF' FIELD <fs_dados>-docnum.

      CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FORM_PAGAMENTO
*&---------------------------------------------------------------------*
FORM f_form_pagamento USING ps_header TYPE zsheader_data_nfse_inbound
                   CHANGING ls_payment TYPE zspayment_data_nfse_inbound.

  DATA lv_value TYPE netwr_fp.
  DATA vg_pymt_meth TYPE zspayment_data_nfse_inbound-pymt_meth.

  vg_pymt_meth = ls_payment-pymt_meth.
  lv_value = ps_header-nfse_value.

  CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
    EXPORTING
      p_bukrs           = ps_header-bukrs
      p_lifnr           = ps_header-lifnr
      p_valor           = lv_value
      p_bvtyp           = ls_payment-bvtyp
    IMPORTING
      p_forma_pagamento = ls_payment-pymt_meth
      p_princ_bnc_emp   = ls_payment-hbkid
    EXCEPTIONS
      nao_fornecedor    = 1
      fornecedor_conta  = 2
      fornecedor_banco  = 3
      faixa_valor       = 4
      banco_empresa     = 5
      OTHERS            = 6.

  IF vg_pymt_meth IS NOT INITIAL.
    ls_payment-pymt_meth = vg_pymt_meth.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_DOCUMENTOS
*&---------------------------------------------------------------------*
FORM f_estorno_manual CHANGING p_linha TYPE zibs_nfse_001
                               p_atualizou TYPE c.

  CLEAR p_atualizou.
  DATA vxblnr TYPE rbkp-xblnr.
  DATA w_rbkp TYPE rbkp.
  DATA lv_num TYPE c LENGTH 40.
  DATA lv_num2 TYPE c LENGTH 40.

  CLEAR w_rbkp.
  " MIRO FOI ESTORNADA MANUALMENTE ?
  IF p_linha-belnr IS NOT INITIAL.

    SELECT SINGLE stblg FROM rbkp
      INTO @DATA(lv_stblg)
      WHERE belnr = @p_linha-belnr
        AND gjahr = @p_linha-gjahr.

    IF sy-subrc EQ 0 AND lv_stblg IS NOT INITIAL.
      p_linha-belnr = space.
      p_linha-gjahr = space.

      p_linha-se_recordid = space.
      p_linha-se_detail = space.

      p_atualizou = 'F'.

    ENDIF.
  ELSEIF p_linha-mblnr IS INITIAL.
    IF p_linha-nfse_numero CA '-'.
      SPLIT p_linha-nfse_numero AT '-' INTO  p_linha-nfse_numero  p_linha-nfse_serie.
    ENDIF.

    IF  p_linha-nfse_serie IS INITIAL.
      p_linha-nfse_serie = '1'.
    ENDIF.

    lv_num = p_linha-nfse_numero.

    SHIFT lv_num LEFT DELETING LEADING '0'.

    CONDENSE lv_num NO-GAPS.

    CHECK lv_num IS NOT INITIAL.

    DATA(_tam) = strlen( lv_num ).
    IF _tam GT 9.
      _tam = _tam - 9.
      lv_num = lv_num+_tam(9) .
      SHIFT lv_num LEFT DELETING LEADING '0'.
    ELSE.
      lv_num = lv_num .
    ENDIF.

    lv_num2 = lv_num.
    CONCATENATE lv_num '-'  p_linha-nfse_serie INTO lv_num.

    vxblnr = lv_num.
    SELECT SINGLE * FROM rbkp
      INTO w_rbkp
      WHERE bukrs = p_linha-bukrs
        AND bldat = p_linha-dtemissao
        AND lifnr = p_linha-lifnr
        AND xblnr = vxblnr
        AND rmwwr = p_linha-nfse_value
        AND stblg = ' '.
    IF sy-subrc NE 0.
      p_linha-nfse_serie =  p_linha-dtemissao+2(2).
      CLEAR lv_num.
      CONCATENATE lv_num2 '-'  p_linha-nfse_serie INTO lv_num.
      vxblnr = lv_num.
      SELECT SINGLE * FROM rbkp
        INTO w_rbkp
        WHERE bukrs = p_linha-bukrs
          AND bldat = p_linha-dtemissao
          AND lifnr = p_linha-lifnr
          AND xblnr = vxblnr
          AND rmwwr = p_linha-nfse_value
          AND stblg = ' '.
    ENDIF.
    IF sy-subrc = 0.
      p_linha-belnr = w_rbkp-belnr.
      p_linha-gjahr = w_rbkp-gjahr.

      p_linha-se_recordid = space.
      p_linha-se_detail = space.
      SELECT SINGLE *
        INTO @DATA(w_rseg)
        FROM rseg
       WHERE belnr = @w_rbkp-belnr
       AND   gjahr = @w_rbkp-gjahr.
      IF sy-subrc = 0.
        p_linha-ebeln = w_rseg-ebeln.
        SELECT SINGLE *
          FROM ekbe
          INTO @DATA(w_ekbe)
          WHERE belnr = @w_rseg-lfbnr
*          AND   gjahr = @w_rseg-lfgja
          AND   ebeln = @w_rseg-ebeln
          AND   ebelp = @w_rseg-ebelp.
        IF sy-subrc = 0 AND w_ekbe-vgabe = '9'.
          p_linha-lblni = w_rseg-lfbnr.
          SELECT SINGLE *
          FROM ekbe
          INTO @DATA(w_ekbe2)
          WHERE belnr = @w_ekbe-lfbnr
          AND   gjahr = @w_ekbe-lfgja
          AND   ebeln = @w_ekbe-ebeln
          AND   ebelp = @w_ekbe-ebelp
          AND   vgabe = '1'.
          IF sy-subrc = 0.
            p_linha-mblnr = w_ekbe2-belnr.
            p_linha-mjahr = w_ekbe2-gjahr.
          ENDIF.
        ELSEIF  w_ekbe-vgabe = '1'.
          p_linha-mblnr = w_ekbe-belnr.
          p_linha-mjahr = w_ekbe-gjahr.
        ENDIF.
      ENDIF.
      UPDATE zibt_nfse_001
      SET belnr = p_linha-belnr
          gjahr = p_linha-gjahr
          lblni = p_linha-lblni
          mblnr = p_linha-mblnr
          mjahr = p_linha-mjahr
         WHERE guid_header = p_linha-guid_header.

      UPDATE /tcsr/t_act  SET last_stepstatus = '100'
         WHERE guid_header = p_linha-guid_header.

      COMMIT WORK.
    ENDIF.

  ENDIF.


  IF p_atualizou = 'F'.

    UPDATE zibt_nfse_001
      SET belnr = p_linha-belnr
          gjahr = p_linha-gjahr
    WHERE guid_header = p_linha-guid_header.
    COMMIT WORK.

  ENDIF.

  IF p_linha-mblnr IS NOT INITIAL.

    SELECT SINGLE smbln FROM mseg
      INTO @DATA(lv_smbln)
        WHERE smbln = @p_linha-mblnr
          AND sjahr = @p_linha-mjahr.

    IF sy-subrc EQ 0 AND lv_smbln IS NOT INITIAL.

      p_linha-mblnr = space.
      p_linha-mjahr = space.

      p_atualizou = 'X'.

    ENDIF.

  ENDIF.

  IF p_linha-lblni IS NOT INITIAL.

    SELECT SINGLE loekz FROM essr
      INTO @DATA(lv_loekz)
        WHERE lblni = @p_linha-lblni.

    IF sy-subrc EQ 0 AND lv_loekz IS NOT INITIAL.
      p_linha-lblni = space.
      p_linha-packno = space.
      p_linha-mblnr = space.
      p_linha-mjahr = space.
      p_atualizou = 'X'.

    ENDIF.

  ENDIF.

  IF p_atualizou = 'X'.

    UPDATE zibt_nfse_001
      SET belnr = p_linha-belnr
          gjahr = p_linha-gjahr
          se_recordid = p_linha-se_recordid
          se_detail = p_linha-se_detail
          lblni = p_linha-lblni
          packno = p_linha-packno
          mblnr = p_linha-mblnr
          mjahr = p_linha-mjahr
    WHERE guid_header = p_linha-guid_header.

    UPDATE zibt_nfse_002
          SET lblni = p_linha-lblni
              packno = p_linha-packno
              mblnr = p_linha-mblnr
              mjahr = p_linha-mjahr
        WHERE guid_header = p_linha-guid_header.

    COMMIT WORK.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_LAYOUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_layout2 .
  REFRESH lt_fcat_lvc2.
  PERFORM monta_fieldcat2 USING:
         'LBLNI'      'ESSR'      'Folha aceite'   '10'       'LBLNI',
         'MBLNR'      'MSEG'      'Doc.Material'   '10'       'MBLNR',
         'MJAHR'      'MSEG'      'Tipo'           '06'       'MJAHR'.

ENDFORM.

FORM monta_fieldcat2 USING p_field
                          p_tabref
                          p_text
                          p_out
                          p_ref_field.
  CLEAR:  wa_fcat_lvc.
  wa_fcat_lvc-fieldname   = p_field.
  wa_fcat_lvc-tabname     = '<FS_DATA>'.
  wa_fcat_lvc-ref_table   = p_tabref.
  wa_fcat_lvc-seltext     = p_text.

  wa_fcat_lvc-scrtext_m  = p_text.
  wa_fcat_lvc-scrtext_l   = p_text.
  wa_fcat_lvc-scrtext_s   = p_text.

  wa_fcat_lvc-outputlen   = p_out.
  wa_fcat_lvc-ref_field   = p_ref_field.

  IF p_field = 'MBLNR' OR p_field = 'LBLNI'.
    wa_fcat_lvc-hotspot = c_x.
  ENDIF.


*inclui dados da work-área p/ tabela sem cab.
  APPEND wa_fcat_lvc TO lt_fcat_lvc2.

ENDFORM.                    " monta_fieldcat
