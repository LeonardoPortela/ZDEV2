* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZHCMR_COUPA_0001                                       *
* Title.......: USER Integ. Status Coupa / 72647 JA                    *
* Author......: Jeferson Silva                                         *
* Date........: 02/02/2022                                             *
* Correções...: 06/04/2022 - RAMON LIMA                                *
* -------------------------------------------------------------------- *
REPORT zhcmr_coupa_0001 NO STANDARD PAGE HEADING.

TYPE-POOLS: slis, abap, icon.

TABLES: sscrfields.

DATA gt_set          TYPE TABLE OF rgsbv.
DATA gv_erro         TYPE c.
DATA gt_dados_alv    TYPE STANDARD TABLE OF zhcms_log_int_coupa_alv.
DATA gt_fieldcat     TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2     TYPE TABLE OF bapiret2.
DATA gt_zhcmt0007    TYPE TABLE OF zhcmt0007.
DATA gt_zhcmt0007_ad TYPE TABLE OF zhcmt0007_ad.
DATA lv_id        TYPE string.

DATA:
  lv_ativo TYPE char1,
  lv_bloq  TYPE char1.

CONSTANTS gc_internal_tab TYPE slis_tabname   VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name   TYPE dd02l-tabname  VALUE 'ZHCMS_LOG_INT_COUPA_ALV'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field   TYPE slis_fieldname VALUE 'COUPA_ICON'.

CONSTANTS gc_aguardando   TYPE zehcm_status   VALUE space.
CONSTANTS gc_enviado      TYPE zehcm_status   VALUE 'S'.
CONSTANTS gc_nao_env      TYPE zehcm_status   VALUE 'N'.
CONSTANTS gc_liberado     TYPE zehcm_status   VALUE 'L'.


DATA go_int TYPE REF TO zcl_integracao_user_coupa.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS so_data FOR sy-datum OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS p_flag TYPE flag USER-COMMAND comm.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  PERFORM f_preenche_data.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF p_flag IS NOT INITIAL.
      IF screen-group4 = '001'.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ELSE.
      screen-active = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


START-OF-SELECTION.
  PERFORM f_limpa_variavel.
  PERFORM f_seleciona.
  PERFORM f_processa_sele.

  IF p_flag IS INITIAL.
    PERFORM f_exibe_alv.
  ELSE.
    PERFORM f_enviar_job .
  ENDIF.


END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona.
  DATA l_data  TYPE sy-datum.
  DATA l_data_e TYPE sy-datum.

** - Caso  p_flag marcado execute via job será paremetro
*  IF  p_flag IS NOT INITIAL AND p_data_a >= sy-datum .
*    l_data = sy-datum - 2.
*    l_data_e = sy-datum - 2.
*  ELSE.
*    l_data = p_data_a.
*    l_data_e = l_data - 2.
*  ENDIF.

  SELECT * FROM zhcmt0007_ad
    INTO TABLE gt_zhcmt0007_ad
      WHERE dt_registro IN so_data.

  "DELETE gt_zhcmt0007_ad WHERE dt_registro NOT BETWEEN l_data_e AND l_data.

ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command USING r_ucomm     TYPE sy-ucomm
                          rs_selfield TYPE slis_selfield.   "#EC CALLED

  CASE r_ucomm.
    WHEN '&IC1'.
      PERFORM f_hyperlink USING rs_selfield.
    WHEN 'ENVIAR'.
      PERFORM f_enviar USING rs_selfield.
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
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  DATA lw_layout  TYPE slis_layout_alv.
  DATA lw_variant TYPE disvariant.

  IF gt_dados_alv IS NOT INITIAL.

    PERFORM f_monta_fieldcat.

    lw_layout-zebra             = abap_true.
    lw_layout-colwidth_optimize = abap_true.
    lw_layout-box_fieldname     = gc_select_field.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'F_STATUS_SET'
        i_callback_user_command  = 'F_USER_COMMAND'
        is_layout                = lw_layout
        it_fieldcat              = gt_fieldcat
        i_save                   = 'A'
        is_variant               = lw_variant
      TABLES
        t_outtab                 = gt_dados_alv
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

    IF sy-subrc <> 0.
      PERFORM f_mensagem_sistema.
    ENDIF.

  ELSE.

    IF NOT line_exists( gt_bapiret2[ type = 'E' ] ).

      MESSAGE s213(v4) DISPLAY LIKE 'E'.
      EXIT.
    ELSE.

      PERFORM f_formatar_msgs_global.
      PERFORM f_mensagem_exibe_popup USING gt_bapiret2.

    ENDIF.

  ENDIF.

ENDFORM.                    " F_EXIBE_ALV
*&---------------------------------------------------------------------*
*&      Form  F_PFSTATUS
*&---------------------------------------------------------------------*
FORM f_status_set USING p_extab TYPE slis_t_extab.          "#EC CALLED

  SET PF-STATUS 'ZSTANDARD'.

ENDFORM.                    "F_PFSTATUS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat.
  REFRESH gt_fieldcat.


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

  LOOP AT gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN gc_icon_field .
        <fs_fcat>-just = 'C'.
        <fs_fcat>-reptext_ddic = 'Status'.
        <fs_fcat>-ddic_outputlen = 000010.
    ENDCASE.

  ENDLOOP.

  DELETE gt_fieldcat WHERE fieldname = gc_select_field.
  DELETE gt_fieldcat WHERE fieldname = 'STATUS_COUPA'.
  DELETE gt_fieldcat WHERE fieldname = 'TOTAL_ITENS'.

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
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_exibe USING p_type TYPE sy-msgty
                            p_msgv1 TYPE sy-msgv1
                            p_msgv2 TYPE sy-msgv2
                            p_msgv3 TYPE sy-msgv3
                            p_msgv4 TYPE sy-msgv4.

  MESSAGE ID 'DS' TYPE 'S' NUMBER '016'
    WITH p_msgv1 p_msgv2 p_msgv3 p_msgv4 DISPLAY LIKE p_type.

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
*&      Form  F_HYPERLINK
*&---------------------------------------------------------------------*
FORM f_hyperlink USING rs_selfield TYPE slis_selfield.

  DATA lw_saida_alv LIKE LINE OF gt_dados_alv.

  CHECK rs_selfield-value IS NOT INITIAL.

  READ TABLE gt_dados_alv INTO lw_saida_alv INDEX rs_selfield-tabindex.

  CASE rs_selfield-fieldname.
    WHEN 'COLUNA'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " F_HYPERLINK

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

  IF l_lines <= 1.

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

      IF <fs_ret2>-id IS INITIAL OR <fs_ret2>-system <> sy-sysid.

        <fs_ret2>-id = 'DS'. "<-classe padrao abap
        <fs_ret2>-number = '016'.
        <fs_ret2>-message_v1 = <fs_ret2>-message.

      ENDIF.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = <fs_ret2>-id
          msgty                  = <fs_ret2>-type
          msgv1                  = <fs_ret2>-message_v1
          msgv2                  = <fs_ret2>-message_v2
          msgv3                  = <fs_ret2>-message_v3
          msgv4                  = <fs_ret2>-message_v4
          txtnr                  = <fs_ret2>-number
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
        batch_list_type     = 'B'
        show_linno          = ' '
        show_linno_text     = 'X'
        show_linno_text_len = '3'
        i_use_grid          = ' '
        i_amodal_window     = ' '
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
*&      Form  F_PROCESSA_SELE
*&---------------------------------------------------------------------*
FORM f_processa_sele .

  DATA:
    lw_saida_alv LIKE LINE OF gt_dados_alv,
    l_email      TYPE zhcmt0007-email,
    l_cname      TYPE zhcmt0007-cname,
    l_line       TYPE i.


  LOOP AT gt_zhcmt0007_ad ASSIGNING FIELD-SYMBOL(<fs_zhcmt>) .

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados_alv>).

    MOVE-CORRESPONDING <fs_zhcmt> TO <fs_dados_alv>.
    SELECT SINGLE cname email INTO ( l_cname, l_email )
             FROM zhcmt0007 WHERE pernr = <fs_zhcmt>-pernr.

    TRANSLATE l_cname TO UPPER CASE.
    <fs_dados_alv>-cname = l_cname.

    IF l_email IS NOT INITIAL.
      FIND '@' IN l_email MATCH OFFSET l_line.
      IF l_line IS NOT INITIAL.
        <fs_dados_alv>-login = l_email+0(l_line).
        <fs_dados_alv>-email = l_email.
      ENDIF.
    ENDIF.

    IF <fs_zhcmt>-id_integra IS NOT INITIAL.
      <fs_dados_alv>-coupa_icon = icon_complete.
    ELSE.
      <fs_dados_alv>-coupa_icon = icon_generate.
    ENDIF.

    ADD 1 TO <fs_dados_alv>-total_itens.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
FORM f_limpa_variavel .

  CLEAR: gt_set , gv_erro, gt_dados_alv, gt_bapiret2.
  REFRESH: gt_dados_alv,gt_bapiret2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FORMATAR_MSGS_GLOBAL
*&---------------------------------------------------------------------*
FORM f_formatar_msgs_global.

  SORT gt_bapiret2 BY message_v1 message_v2 message_v3 message_v4.

  DELETE ADJACENT DUPLICATES FROM gt_bapiret2
        COMPARING message_v1 message_v2 message_v3 message_v4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ENVIAR
*&---------------------------------------------------------------------*
FORM f_enviar USING rs_selfield TYPE slis_selfield.

  DATA: lv_count    TYPE i,
        lv_erro     TYPE c,
        lv_xml      TYPE string,
        lv_metodo   TYPE string,
        lv_http_url TYPE string,
        lv_token    TYPE string,
        lv_retorno  TYPE string,
        lt_text_tab TYPE TABLE OF text_tab,
        lv_num      TYPE num10,
        lv_id       TYPE string.


  LOOP AT  gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>)
                       WHERE selec = 'X' .
*    AND status_coupa = gc_aguardando.

    READ TABLE gt_zhcmt0007_ad ASSIGNING FIELD-SYMBOL(<fs_zhcm>)
                          INDEX sy-tabix.

    READ TABLE gt_dados_alv TRANSPORTING NO FIELDS
      WITH KEY selec = 'X'
               coupa_icon = '@DF@'.

*    IF <fs_zhcm>-id_integra IS NOT INITIAL.
*      PERFORM f_mensagem_exibe
*        USING 'E'
*              'Há linhas selecionadas que'
*              'já foram enviadas para o COUPA'
*              space space.
*      EXIT.
*    ENDIF.

    IF <fs_zhcm>-ck_ativar = 'X'.
      lv_ativo = 'X'.
    ENDIF.

    IF <fs_zhcm>-ck_desligado = 'X' OR <fs_zhcm>-ck_bloquear = 'X'.
      lv_bloq = 'X'.
    ENDIF.

    CHECK lv_erro IS INITIAL.

    TRY.

        zcl_integracao_user_coupa=>zif_integracao_user_coupa~get_instance(
        )->execute_process( EXPORTING i_user = <fs_dados>-login i_ativo = lv_ativo i_bloqueado = lv_bloq ).

      CATCH zcx_integracao INTO DATA(ex_int).

        DATA(lv_text) = ex_int->get_longtext( ).

      CATCH zcx_error INTO DATA(ex_erro).

        lv_text = ex_erro->get_longtext( ).

    ENDTRY.

    IF lv_text  IS INITIAL.

      <fs_dados>-coupa_icon = icon_complete.
      <fs_dados>-status_coupa = 'Concluido'.
      <fs_dados>-data_exe = sy-datum.

      UPDATE zhcmt0007_ad
         SET id_integra = lv_id
       WHERE pernr = <fs_dados>-pernr.

    ELSE.
      <fs_dados>-status_coupa =  lv_text.
      <fs_dados>-coupa_icon = icon_incomplete.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_XML_TESTE
*&---------------------------------------------------------------------*
FORM f_xml CHANGING p_xml TYPE string
                          p_ativo TYPE char1
                          p_bloq  TYPE char1.
*
*  IF p_ativo = 'X'.
*    p_xml = '<user><active>TRUE</active></user>'.
*  ELSEIF p_bloq = 'X'.
*    p_xml = '<user><active>FALSE</active></user>'.
*
*  ENDIF.
*


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ENVIAR_JOB
*&---------------------------------------------------------------------*
FORM f_enviar_job .
  DATA: lv_count     TYPE i,
        lv_erro      TYPE c,
        lv_xml       TYPE string,
        lv_metodo    TYPE string,
        lv_http_url  TYPE string,
        lv_token     TYPE string,
        lv_retorno   TYPE string,
        lt_text_tab  TYPE TABLE OF text_tab,
        lv_num       TYPE num10,
        lw_dados_alv LIKE LINE OF gt_dados_alv.

  DELETE gt_dados_alv WHERE coupa_icon = '@DF@'.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>).

    READ TABLE gt_zhcmt0007_ad ASSIGNING FIELD-SYMBOL(<fs_zhcm>)
                                                INDEX sy-tabix.
    TRY.

        IF <fs_zhcm>-ck_ativar = 'X'.
          lv_ativo = 'X'.
        ENDIF.

        IF <fs_zhcm>-ck_desligado = 'X' OR <fs_zhcm>-ck_bloquear = 'X'.
          lv_bloq = 'X'.
        ENDIF.

        zcl_integracao_user_coupa=>zif_integracao_user_coupa~get_instance(
        )->execute_process( EXPORTING i_user = <fs_dados>-login i_ativo = lv_ativo i_bloqueado = lv_bloq ).

      CATCH zcx_integracao INTO DATA(ex_int).

        DATA(lv_text) = ex_int->get_longtext( ).

      CATCH zcx_error INTO DATA(ex_erro).

        lv_text = ex_erro->get_longtext( ).
    ENDTRY.

    IF lv_text  IS INITIAL.
      <fs_dados>-coupa_icon = icon_complete.
      <fs_dados>-status_coupa = 'Concluido'.
      <fs_dados>-data_exe = sy-datum.
      UPDATE zhcmt0007_ad
     SET id_integra = lv_id
   WHERE pernr = <fs_dados>-pernr.

    ELSE.
      <fs_dados>-status_coupa =  lv_text.
      <fs_dados>-coupa_icon = icon_incomplete.

    ENDIF.
    WRITE:/ <fs_dados>-cname, <fs_dados>-login, <fs_dados>-status_coupa.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_DATA
*&---------------------------------------------------------------------*
FORM f_preenche_data .

*  DATA(lv_ini) = sy-datum.
*  DATA(lv_fim) = sy-datum.
*
*  CHECK so_data[] IS INITIAL.
*
*  SUBTRACT 2 FROM lv_ini.
*
*  "ADD 15 TO lv_fim.
*
*  APPEND 'IBT' && lv_ini && lv_fim TO so_data.

ENDFORM.
