* ==================================================================== *
*                         © RECLIKE                                   *
* ==================================================================== *
* Program.....: ZMMR174                                                *
* Title.......: Integração Material X COUPA                            *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 17/03/2017                                             *
* -------------------------------------------------------------------- *
REPORT zmmr174.

TYPE-POOLS: slis, abap, icon.

"TABLES: zmms_int_mat_coupa_alv, sscrfields.
TABLES: mara,marc.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZMMS_INT_MAT_COUPA_ALV'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

"dados alv
DATA gt_dados_alv TYPE STANDARD TABLE OF zmms_int_mat_coupa_alv.
DATA gt_t023t TYPE TABLE OF t023t.
DATA gt_grpmerc TYPE TABLE OF zintegrcoupa01.
DATA gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.
DATA gt_ausp TYPE TABLE OF ausp.
DATA gt_asmdt TYPE TABLE OF asmdt.
DATA gt_cdhdr TYPE TABLE OF cdhdr.
DATA gt_int TYPE TABLE OF zintegrcoupa01.
DATA gv_erro TYPE c.
DATA: xv_jobnm TYPE btcjob.
DATA: xv_stepc TYPE btcstepcnt.
DATA: var_werks TYPE marc-werks.
DATA: var_matnr TYPE zppet016-id_material_sap. "marc-matnr.
DATA: vg_job      TYPE i.
DATA lv_asnum2 TYPE asnum.
DATA lv_matnr2 TYPE matnr.

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS so_werks FOR marc-werks.
  SELECT-OPTIONS so_matnr FOR marc-matnr.
  SELECT-OPTIONS so_mtart FOR mara-mtart.
  SELECT-OPTIONS so_ersda FOR mara-ersda.
  PARAMETERS p_qtde TYPE int4.
SELECTION-SCREEN END OF BLOCK b1.

" DADOS DO ALV
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002.
  PARAMETERS p_envi AS CHECKBOX.
  PARAMETERS p_vari TYPE slis_vari NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.

  "PERFORM F_BOTAO_FUNCTION.
  PERFORM default_variant CHANGING p_vari.

  "AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  "PERFORM f4_for_variant CHANGING p_vari.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.

  IF sy-batch IS NOT  INITIAL.
    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        jobname         = xv_jobnm
        stepcount       = xv_stepc
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.

    IF xv_jobnm+0(8) NE 'COUPAMAT'.
*      PERFORM f_verifica_job CHANGING gv_erro.
*
*      CHECK gv_erro IS INITIAL.
      IF xv_jobnm = 'INTEGRACAO_COUPA_MATERIAL'.
        SELECT SINGLE COUNT(*) INTO vg_job
            FROM tbtco
           WHERE jobname EQ xv_jobnm
             AND status EQ 'R'.
        IF vg_job NE 1.
          MESSAGE s016(ds) WITH 'Job esta sendo executado!' sy-cprog DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE s016(ds) WITH 'Nome deve ser->INTEGRACAO_COUPA_MATERIAL' sy-cprog DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ELSE.
*-------Inicio Bug Solto 146336 RICARDO SPEREIRA-------------------------*
      IF xv_jobnm NE 'COUPAMAT_AGRUPADO'.
*-------Fim Bug Solto 146336 RICARDO SPEREIRA-------------------------*
        SPLIT xv_jobnm AT '|' INTO DATA(lv_desc)
                                   DATA(lv_matnr)
                                   DATA(lv_werks).

        REFRESH: so_werks,so_matnr.
        IF lv_werks IS INITIAL AND lv_matnr IS INITIAL.
          EXIT.
        ENDIF.
        IF lv_werks IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_werks
            IMPORTING
              output = var_werks.
          so_werks-sign   = 'I'.
          so_werks-option = 'EQ'.
          so_werks-low    = var_werks.
          APPEND  so_werks.
        ENDIF.
        "
        IF lv_matnr IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_matnr
            IMPORTING
              output = var_matnr.

          so_matnr-sign   = 'I'.
          so_matnr-option = 'EQ'.
          so_matnr-low    = var_matnr.
          APPEND  so_matnr.
        ENDIF.
      ENDIF.


      p_qtde = 1000.
    ENDIF.
  ENDIF.

  PERFORM f_preenche_data.

  PERFORM f_seleciona.
  PERFORM f_processa.

  IF sy-batch = 'X'.
    PERFORM f_enviar_coupa.
  ELSE.
    PERFORM f_exibe_alv.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  DATA lt_dados_aux TYPE TABLE OF zmms_int_mat_coupa_alv.

  DATA lt_id_merc TYPE TABLE OF zintegrcoupa01.

  CLEAR gt_dados_alv.

  SELECT marc~werks name1 mara~matnr maktx mtart mmsta mstae matkl
         meins steuc laeda ersda mara~matnr AS objek FROM mara
    INNER JOIN makt ON mara~matnr = makt~matnr
                   AND makt~spras = sy-langu
    INNER JOIN marc ON mara~matnr = marc~matnr
    INNER JOIN t001w ON marc~werks = t001w~werks
    INTO CORRESPONDING FIELDS OF TABLE gt_dados_alv
      WHERE marc~werks IN so_werks
        AND mara~matnr IN so_matnr
        AND mara~mtart IN so_mtart
        AND ( laeda IN so_ersda OR ersda IN so_ersda ).

  CHECK sy-subrc EQ 0.

  SORT gt_dados_alv BY werks matnr ASCENDING.

  SELECT * FROM t023t
    INTO TABLE gt_t023t
    FOR ALL ENTRIES IN gt_dados_alv
      WHERE matkl = gt_dados_alv-matkl
        AND spras = sy-langu.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    IF <fs_alv>-mtart NE 'ZDIE'.
      <fs_alv>-id_integr = <fs_alv>-matnr && <fs_alv>-werks.
    ENDIF.

    <fs_alv>-objectid = <fs_alv>-matnr.

    APPEND INITIAL LINE TO lt_id_merc ASSIGNING FIELD-SYMBOL(<fs_id_merc>).

    <fs_id_merc>-id_integr  = <fs_alv>-matkl.
    <fs_id_merc>-ident_proc = 'GM'.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'SERVICO'
      IMPORTING
        output = <fs_alv>-atinn.

    " 29.06.2022 - RAMON - AJUSTE TEXTO LONGO 82234 -->
    PERFORM f_texto_longo
      USING <fs_alv>-matnr
            <fs_alv>-maktx
   CHANGING <fs_alv>-maktx_l.
    " 29.06.2022 - RAMON - AJUSTE TEXTO LONGO 82234 --<

  ENDLOOP.

  CHECK gt_dados_alv IS NOT INITIAL.

  SELECT * FROM cdhdr
    INTO TABLE gt_cdhdr
    FOR ALL ENTRIES IN gt_dados_alv
      WHERE objectclas = 'MATERIAL'
        AND objectid = gt_dados_alv-objectid
        AND udate IN so_ersda.

  SORT gt_cdhdr BY udate DESCENDING utime DESCENDING.

*  LOOP AT gt_dados_alv ASSIGNING <fs_alv>.
*
*    READ TABLE gt_int ASSIGNING FIELD-SYMBOL(<fs_int>)
*      WITH KEY id_integr = <fs_alv>-id_integr.
*
*    " encontrou um envio
*    IF sy-subrc EQ 0.
*
*      " verifica se a data de atualização é maior que a data de envio
*
*      READ TABLE gt_cdhdr ASSIGNING FIELD-SYMBOL(<fs_cdhdr>)
*        WITH KEY objectid = <fs_alv>-objectid.
*
*      IF sy-subrc EQ 0.
*
*        IF <fs_cdhdr>-udate >= <fs_int>-dt_atual.
*          <fs_alv>-permit_envio = 'X'.
*        ELSEIF <fs_cdhdr>-udate >= <fs_int>-dt_atual AND <fs_cdhdr>-utime >= <fs_int>-hr_atual.
*          <fs_alv>-permit_envio = 'X'.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDLOOP.

  CHECK gt_dados_alv IS NOT INITIAL.

  SELECT * FROM ausp
    INTO TABLE gt_ausp
      FOR ALL ENTRIES IN gt_dados_alv
        WHERE objek = gt_dados_alv-objek
          AND atinn = gt_dados_alv-atinn.

  IF gt_ausp IS NOT INITIAL.

    LOOP AT gt_dados_alv ASSIGNING <fs_alv> WHERE mtart = 'ZDIE'.

      LOOP AT gt_ausp ASSIGNING FIELD-SYMBOL(<fs_ausp>) WHERE objek = <fs_alv>-objek.

        <fs_alv>-asnum = |{ <fs_ausp>-atwrt ALPHA = IN  }|.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_alv>-asnum
          IMPORTING
            output = lv_asnum2.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_alv>-matnr
          IMPORTING
            output = lv_matnr2.

*        <fs_alv>-id_integr = <fs_alv>-asnum && <fs_alv>-werks.
        <fs_alv>-id_integr = lv_asnum2 && lv_matnr2 && <fs_alv>-werks. "BUG 99160

        APPEND <fs_alv> TO lt_dados_aux.

      ENDLOOP.

    ENDLOOP.

    SELECT * FROM asmdt
      INTO TABLE gt_asmdt
        FOR ALL ENTRIES IN lt_dados_aux
          WHERE asnum = lt_dados_aux-asnum.

  ENDIF.

  APPEND LINES OF lt_dados_aux TO gt_dados_alv.

  SORT gt_dados_alv BY werks matnr asnum DESCENDING.

  DELETE ADJACENT DUPLICATES FROM gt_dados_alv COMPARING werks matnr asnum.

  SELECT * FROM zintegrcoupa01
    INTO TABLE gt_int
    FOR ALL ENTRIES IN gt_dados_alv
      WHERE id_integr = gt_dados_alv-id_integr
        AND ( ident_proc = 'SV' OR ident_proc = 'MT' ).

  SORT lt_id_merc.

  DELETE ADJACENT DUPLICATES FROM lt_id_merc.

  CHECK lt_id_merc IS NOT INITIAL.

  SELECT * FROM zintegrcoupa01
    INTO TABLE gt_grpmerc
      FOR ALL ENTRIES IN lt_id_merc
       WHERE id_integr = lt_id_merc-id_integr
        AND ident_proc = lt_id_merc-ident_proc.


  LOOP AT gt_dados_alv ASSIGNING <fs_alv>.

    READ TABLE gt_int ASSIGNING FIELD-SYMBOL(<fs_int>)
      WITH KEY id_integr = <fs_alv>-id_integr.

    " encontrou um envio
    IF sy-subrc EQ 0.

      " verifica se a data de atualização é maior que a data de envio

      " 23.11.2022 - RAMON - CORREÇÃO QUANDO EXECUTADO PELO JOB 'COUPAMAT' -->
      IF sy-batch = 'X'.

        IF xv_jobnm+0(8) EQ 'COUPAMAT'.
          <fs_alv>-permit_envio = 'X'.
          CONTINUE.
        ENDIF.

      ENDIF.

      " 23.11.2022 - RAMON - CORREÇÃO QUANDO EXECUTADO PELO JOB 'COUPAMAT' --<

      READ TABLE gt_cdhdr ASSIGNING FIELD-SYMBOL(<fs_cdhdr>)
        WITH KEY objectid = <fs_alv>-objectid.

      IF sy-subrc EQ 0.

        IF <fs_cdhdr>-udate >= <fs_int>-dt_atual.
          <fs_alv>-permit_envio = 'X'.
        ELSEIF <fs_cdhdr>-udate >= <fs_int>-dt_atual AND <fs_cdhdr>-utime >= <fs_int>-hr_atual.
          <fs_alv>-permit_envio = 'X'.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

  CASE r_ucomm.
    WHEN 'ENVIAR'.
      PERFORM f_enviar_coupa.
    WHEN '&IC1'.
      PERFORM f_hyperlink   USING rs_selfield.
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

  "sscrfields-functxt_01 = 'BOTAO 1'.

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

  DATA lw_layout TYPE slis_layout_alv.
  DATA lw_variant TYPE disvariant.

  IF gt_dados_alv IS NOT INITIAL.

    IF p_vari IS NOT INITIAL.
      lw_variant-report = sy-repid.
      lw_variant-variant = p_vari.
    ENDIF.

    PERFORM f_monta_fieldcat.

    lw_layout-zebra             = abap_true.
    lw_layout-colwidth_optimize = abap_true.
    lw_layout-box_fieldname = gc_select_field.
    lw_layout-info_fieldname = 'COLOR'.

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
  DELETE gt_fieldcat WHERE fieldname = 'TIPO_PROC'.
  DELETE gt_fieldcat WHERE fieldname = 'COLOR'.

  DELETE gt_fieldcat WHERE fieldname = 'OBJEK'.
  DELETE gt_fieldcat WHERE fieldname = 'ATINN'.
  DELETE gt_fieldcat WHERE fieldname = 'ID_INTEGR'.
  DELETE gt_fieldcat WHERE fieldname = 'OBJECTID'.
  DELETE gt_fieldcat WHERE fieldname = 'PERMIT_ENVIO'.

  PERFORM f_coluna_edita USING 'MSGTX' 'Status'.

  PERFORM f_coluna_edita USING 'MAKTX_L' 'Texto Longo'.

  PERFORM f_coluna_edita USING 'ID_COUPA' 'ID COUPA'.


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

  DATA(lv_ini) = sy-datum.
  DATA(lv_fim) = sy-datum.

  IF so_ersda[] IS INITIAL.

    IF p_qtde IS NOT INITIAL.
      SUBTRACT p_qtde FROM lv_ini.
    ELSE.
      SUBTRACT 1 FROM lv_ini.
    ENDIF.

    APPEND 'IBT' && lv_ini && lv_fim TO so_ersda.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HYPERLINK
*&---------------------------------------------------------------------*
FORM f_hyperlink USING rs_selfield TYPE slis_selfield.

  DATA lw_saida_alv LIKE LINE OF gt_dados_alv.

  CHECK rs_selfield-value IS NOT INITIAL.

  READ TABLE gt_dados_alv INTO lw_saida_alv INDEX rs_selfield-tabindex.

  CASE rs_selfield-fieldname.

    WHEN 'MATKL'.

      DATA lr_matkl TYPE RANGE OF matkl.

      APPEND 'IEQ' && lw_saida_alv-matkl TO lr_matkl.

      SUBMIT zmmr171
        WITH p_envi EQ space
        WITH so_matkl IN lr_matkl AND RETURN.

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
* CONTROLE DE ATUALIZAÇÃO DE TELA DINAMICAMENTE

*    DATA lt_return TYPE TABLE OF ddshretval.
*    DATA lt_fields TYPE TABLE OF dynpread.
*
*    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*      EXPORTING
*        tabname           = 'ZTPP_009'
*        fieldname         = 'MATNR_DUMMY'
*        "searchhelp        = 'ZHPP_DUMMY'
*        "shlpparam         = 'MATNR_DUMMY'
*        "IMPORTING
*        "user_reset        =
*      TABLES
*        return_tab        = lt_return
*      EXCEPTIONS
*        field_not_found   = 1
*        no_help_for_field = 2
*        inconsistent_help = 3
*        no_values_found   = 4
*        OTHERS            = 5.
*
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_ret>).
*
*      APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_dyn>).
*
**      stepl
**
**      fieldinp
*
*      CASE <fs_ret>-fieldname.
*        WHEN 'WERKS'.
*          <fs_dyn>-fieldname = 'P_WERKS'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*        WHEN 'ARBPL'.
*          <fs_dyn>-fieldname = 'P_ARBPL'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*        WHEN 'VORNR'.
*
*          <fs_dyn>-fieldname = 'P_VORNR'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*        WHEN 'FLAG_DUMPS'.
*
*          APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_dyn2>).
*
*          IF <fs_ret>-fieldval = 'X'.
*
*            <fs_dyn>-fieldname = 'P_DUM_S'.
*            <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*            <fs_dyn2>-fieldname = 'P_DUM_N'.
*            <fs_dyn2>-fieldvalue = space.
*
*          ELSE.
*            <fs_dyn>-fieldname = 'P_DUM_N'.
*            <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*            <fs_dyn2>-fieldname = 'P_DUM_S'.
*            <fs_dyn2>-fieldvalue = space.
*          ENDIF.
*
*        WHEN 'MATNR_DUMMY'.
*
*          <fs_dyn>-fieldname = 'P_MATNR'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*
*      ENDCASE.
*
*    ENDLOOP.
*
*    CALL FUNCTION 'DYNP_VALUES_UPDATE'
*      EXPORTING
*        dyname               = '1000'
*        dynumb               = '1000'
*      TABLES
*        dynpfields           = lt_fields
*      EXCEPTIONS
*        invalid_abapworkarea = 1
*        invalid_dynprofield  = 2
*        invalid_dynproname   = 3
*        invalid_dynpronummer = 4
*        invalid_request      = 5
*        no_fielddescription  = 6
*        undefind_error       = 7
*        OTHERS               = 8.
*
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.


***AT SELECTION-SCREEN OUTPUT.
**
**    LOOP AT SCREEN.
**
**      IF screen-name CP '*P_WERKS*'
**        OR screen-name CP '*P_DUM_N*'
**        OR screen-name CP '*P_ARBPL*'
**        OR screen-name CP '*P_VORNR*'.
**
**        screen-input = 0.
**        MODIFY SCREEN.
**      ENDIF.
**
**    ENDLOOP.
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

  DATA lv_ident_proc TYPE zcoupa_ident_proc.
  DATA lv_msgx TYPE msgtx.
  DATA lv_id_int TYPE zcoupa_id_integr.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>).

    IF <fs_dados>-mtart = 'ZDIE'.

      lv_ident_proc = 'SV'.
      lv_msgx = 'Serv.Atualizado, aguardando envio'.

    ELSE.

      lv_msgx = 'Mat.Atualizado, aguardando envio'.
      lv_ident_proc = 'MT'.

    ENDIF.

    READ TABLE gt_int ASSIGNING FIELD-SYMBOL(<fs_coupa>)
      WITH KEY id_integr = <fs_dados>-id_integr
               ident_proc = lv_ident_proc.

    IF sy-subrc EQ 0.

      IF <fs_dados>-permit_envio = 'X'.
        <fs_dados>-icon  = icon_generate.
        <fs_dados>-msgtx = lv_msgx.
        <fs_dados>-id_coupa = <fs_coupa>-fields.
      ELSE.
        <fs_dados>-icon = icon_complete.
        <fs_dados>-msgtx = 'Envio realizado'.
        <fs_dados>-id_coupa = <fs_coupa>-fields.
      ENDIF.

    ELSE.

      <fs_dados>-icon  = icon_generate.
      <fs_dados>-msgtx = 'Aguardando envio'.

    ENDIF.

    READ TABLE gt_grpmerc ASSIGNING FIELD-SYMBOL(<fs_merc>)
      WITH KEY id_integr = <fs_dados>-matkl.

    IF sy-subrc NE 0.

      <fs_dados>-icon  = icon_yellow_light.
      <fs_dados>-msgtx = 'Grupo de Mercadorias não existe no COUPA'.
    ENDIF.

    READ TABLE gt_asmdt ASSIGNING FIELD-SYMBOL(<fs_asmdt>)
      WITH KEY asnum = <fs_dados>-asnum.

    IF sy-subrc EQ 0.

      <fs_dados>-asktx = <fs_asmdt>-asktx.

    ENDIF.

    READ TABLE gt_t023t ASSIGNING FIELD-SYMBOL(<fs_t023t>)
      WITH KEY matkl = <fs_dados>-matkl.

    IF sy-subrc EQ 0.
      <fs_dados>-wgbez60 = <fs_t023t>-wgbez60.
    ENDIF.

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
*&      Form  F_ENVIAR_COUPA
*&---------------------------------------------------------------------*
FORM f_enviar_coupa .

  DATA lw_envio TYPE zmms_int_mat_coupa_item.

  DATA lr_selec TYPE RANGE OF flag.

  DATA lv_ret.

  DATA lo_object TYPE REF TO zcl_integracao_coupa_material.

  IF sy-batch IS INITIAL.

    PERFORM f_verifica_linha_selec CHANGING lv_ret.

    CHECK lv_ret IS INITIAL.

    PERFORM f_popup_to_confirm USING 'Confirmar envio dos dados?' CHANGING lv_ret.

    CHECK lv_ret = '1'.

    APPEND 'IEQX' TO lr_selec.

  ELSE.
    CLEAR lr_selec.
  ENDIF.

  CLEAR gt_bapiret2.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.

    TRY.

        CHECK <fs_dados>-icon NE icon_complete.

        MOVE-CORRESPONDING <fs_dados> TO lw_envio.

        lo_object ?= zcl_integracao_coupa_material=>zif_integracao_coupa_material~get_instance( ).

        lo_object->enviar_mat_coupa( lw_envio ).

        " lv_mess = `Envio realizado ID COUPA: ` && lw_integ-fields.

        DATA(lt_mess) = lo_object->zif_integracao_coupa_material~get_messages( ).

        READ TABLE lt_mess ASSIGNING FIELD-SYMBOL(<fs_mess>)
          WITH KEY type = 'S'.

        IF sy-subrc EQ 0.

          <fs_dados>-icon = icon_complete.
          <fs_dados>-msgtx = 'Envio realizado'.
          <fs_dados>-id_coupa = <fs_mess>-message+25.

          CONDENSE <fs_dados>-id_coupa NO-GAPS.

          IF xv_jobnm+0(8) EQ 'COUPAMAT' AND xv_jobnm NE 'COUPAMAT_AGRUPADO'.
            EXIT.
          ENDIF.
          "
        ELSE.

          READ TABLE lt_mess ASSIGNING <fs_mess>
            WITH KEY type = 'E'.

          IF sy-subrc EQ 0.
            <fs_dados>-icon = icon_red_light.
            <fs_dados>-msgtx = <fs_mess>-message.
            APPEND LINES OF lt_mess TO gt_bapiret2.

"**********************************************************************Start "175292 Ajustar ZMMR201 para reprocessar jobs com erro PSA
            DATA: _job      TYPE string,
                  _material TYPE matnr,
                  _centro   TYPE werks.

            CLEAR:  _job,_material,_centro.
            SPLIT xv_jobnm AT '|' INTO _job _material _centro.
            IF _job EQ 'COUPAMAT'.
              UPDATE zmmt0184 SET fg_processado = 'N' WHERE  matnr = _material AND werks = _centro AND data_processo = sy-datum.
              COMMIT WORK.
            ENDIF.
"**********************************************************************Finish "175292 Ajustar ZMMR201 para reprocessar jobs com erro PSA

          ENDIF.

        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int).
        "me->zif_integracao_coupa_material~set_message( ex_int->get_longtext( ) ).
      CATCH zcx_error INTO DATA(ex_erro).
        "me->zif_integracao_coupa_material~set_message( ex_int->get_longtext( ) ).
    ENDTRY.

  ENDLOOP.

  PERFORM f_mensagem_exibe_popup USING gt_bapiret2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_JOB
*&---------------------------------------------------------------------*
FORM f_verifica_job CHANGING p_erro.

  CALL FUNCTION 'ZMM_JOB_CHECK'
    EXPORTING
      i_cprog   = sy-cprog
    IMPORTING
      ev_active = p_erro.

  IF p_erro = 'X'.
    MESSAGE s016(ds) WITH 'Já existe um job para o programa' sy-cprog DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TEXTO_LONGO
*&---------------------------------------------------------------------*
FORM f_texto_longo USING p_matnr TYPE matnr
                         p_maktx TYPE maktx
                CHANGING p_maktx_l TYPE string.

  DATA lv_name TYPE tdobname.
  DATA lt_lines TYPE TABLE OF tline.

  CHECK p_matnr IS NOT INITIAL.

  CLEAR p_maktx_l.

  lv_name = p_matnr.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'BEST'
      language                = sy-langu
      name                    = lv_name
      object                  = 'MATERIAL'
    TABLES
      lines                   = lt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'GRUN'
        language                = sy-langu
        name                    = lv_name
        object                  = 'MATERIAL'
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      p_maktx_l = p_maktx.
      EXIT.
    ENDIF.

  ENDIF.

  LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>).

    DATA(lv_line) = <fs_line>-tdline.

    CONDENSE lv_line.

    p_maktx_l = p_maktx_l && lv_line && ` `.

  ENDLOOP.

ENDFORM.
