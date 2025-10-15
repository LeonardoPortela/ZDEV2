* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZSDR0231                                               *
* Title.......: Gestão Estoque(Necessidades/Disponibilidade)           *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 08/07/2025                                             *
* -------------------------------------------------------------------- *
REPORT zsdr0231.

"&F03 - BACK
"&F15 - LEAVE
"&F12 - CANCEL

" FM06LF01_PICKUP - PARA LINKS
" COPIAR STATUS GUI DE BCALV_TEST_FULLSCREEN_USER_COM E RENOMEAR PARA STANDARD
" EM X01 TROCAR PARA MENSAGENS DE DADOS NAO ENCONTRADOS
" IMPLEMENTAR F_SELECIONA

TYPE-POOLS: slis, abap, icon.

TABLES: zsds_gestao_estoque_filtro, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZSDS_GESTAO_ESTOQUE_ALV'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.

"dados alv
DATA gt_dados_alv TYPE STANDARD TABLE OF zsds_gestao_estoque_alv.
DATA gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.

"SELECTION-SCREEN: FUNCTION KEY 1

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

*  SELECT-OPTIONS so_bukrs FOR zsds_gestao_estoque_filtro-bukrs NO-EXTENSION NO INTERVALS OBLIGATORY.
  SELECT-OPTIONS so_matkl FOR zsds_gestao_estoque_filtro-matkl NO INTERVALS.
  SELECT-OPTIONS so_culti FOR zsds_gestao_estoque_filtro-cultivar NO-EXTENSION NO INTERVALS.
  SELECT-OPTIONS so_matnr FOR zsds_gestao_estoque_filtro-matnr.
  SELECT-OPTIONS so_werks FOR zsds_gestao_estoque_filtro-werks.
  SELECT-OPTIONS so_safra FOR zsds_gestao_estoque_filtro-safra.

SELECTION-SCREEN END OF BLOCK b1.

*" DADOS SECUNDARIOS
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
*  PARAMETERS rb_cul RADIOBUTTON GROUP rg01. " Cultivar
*  PARAMETERS rb_cen RADIOBUTTON GROUP rg01. " Centro
*  PARAMETERS rb_mat RADIOBUTTON GROUP rg01. " Material
*  PARAMETERS rb_sem RADIOBUTTON GROUP rg01. " Sem agrupamento
*SELECTION-SCREEN END OF BLOCK b2.

" DADOS DO ALV
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  "PERFORM f_preenche_data.
  "PERFORM F_BOTAO_FUNCTION.
  PERFORM default_variant CHANGING p_vari.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_culti-low.
  PERFORM f4_for_cultivar CHANGING so_culti-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant CHANGING p_vari.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.
  PERFORM f_seleciona.
  PERFORM f_exibe_alv.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  SELECT * FROM zi_sd_in_ge_mara
      INTO TABLE @DATA(it_mara)
      WHERE matnr IN @so_matnr
        AND matkl IN @so_matkl
        AND werks IN @so_werks
        AND cultivar IN @so_culti.

  CHECK sy-subrc EQ 0.

  SELECT * FROM zi_sd_in_ge_uni
    INTO TABLE @DATA(lt_estoque)
      FOR ALL ENTRIES IN @it_mara
        WHERE matnr = @it_mara-matnr
          AND werks = @it_mara-werks.

  SELECT * FROM zi_sd_in_ge_vendas
    INTO TABLE @DATA(lt_vendas)
      FOR ALL ENTRIES IN @it_mara
        WHERE matnr = @it_mara-matnr
          AND werks = @it_mara-werks.

  SELECT * FROM zi_sd_in_ge_compras
    INTO TABLE @DATA(lt_compras)
      FOR ALL ENTRIES IN @it_mara
        WHERE matnr = @it_mara-matnr
          AND werks = @it_mara-werks.

  LOOP AT it_mara ASSIGNING FIELD-SYMBOL(<fs_mara>).

    READ TABLE lt_estoque ASSIGNING FIELD-SYMBOL(<fs_estoque>)
      WITH KEY matnr = <fs_mara>-matnr
               werks = <fs_mara>-werks.

    CHECK sy-subrc EQ 0.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-cultivar = <fs_mara>-cultivar.

    <fs_alv>-werks = <fs_mara>-werks.

    IF <fs_estoque>-lifnr <> '0000000000'.
      <fs_alv>-lifnr = <fs_estoque>-lifnr.
    ENDIF.

    <fs_alv>-matnr = <fs_mara>-matnr.
    <fs_alv>-name1 = <fs_estoque>-name1.
*    <fs_alv>-safra
    <fs_alv>-matkl = <fs_mara>-matkl.
    <fs_alv>-ort01 = <fs_estoque>-ort01.
    <fs_alv>-meins = ''.
    <fs_alv>-estoque =  <fs_estoque>-clabs.

    READ TABLE lt_compras ASSIGNING FIELD-SYMBOL(<fs_compras>)
      WITH KEY matnr = <fs_mara>-matnr
           werks = <fs_mara>-werks.

    IF sy-subrc EQ 0.
      <fs_alv>-compras = <fs_compras>-menge.
      <fs_alv>-bukrs = <fs_compras>-bukrs.
      <fs_alv>-bsart = <fs_compras>-bsart.
    ENDIF.

    READ TABLE lt_vendas ASSIGNING FIELD-SYMBOL(<fs_vendas>)
      WITH KEY matnr = <fs_mara>-matnr
               werks = <fs_mara>-werks.

    IF sy-subrc EQ 0.
      <fs_alv>-vendas = <fs_vendas>-kwmeng.
      <fs_alv>-bukrs = <fs_vendas>-vkorg.
      <fs_alv>-auart = <fs_vendas>-auart.
    ELSE.
      <fs_alv>-vendas = 0.
    ENDIF.

    <fs_alv>-total = <fs_alv>-compras + <fs_alv>-estoque.
    <fs_alv>-dispon = <fs_alv>-total +  <fs_alv>-vendas.

  ENDLOOP.

ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

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

  PERFORM f_delete_fieldcat
    USING: gc_select_field,
          'COLOR', 'MEINS',
          'BUKRS', 'AUART',
          'BSART', 'ERDAT',
          'AEDAT','SAFRA'.

  PERFORM f_coluna_edita USING 'CULTIVAR' 'Cultivar'.
  PERFORM f_coluna_edita USING 'ESTOQUE' 'Estoque'.
  PERFORM f_coluna_edita USING 'COMPRAS' 'Compras'.
  PERFORM f_coluna_edita USING 'TOTAL' 'Total'.
  PERFORM f_coluna_edita USING 'VENDAS' 'Vendas'.
  PERFORM f_coluna_edita USING 'DISPON' 'Disponível'.
  PERFORM f_coluna_edita USING 'NAME1' 'Centro de Distribuição'.

ENDFORM.                    " F_MONTA_FIELDCAT
"PERFORM f_fieldcat_modi USING 'PESO' 'EDIT' 'X' CHANGING lt_fieldcat.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_MODI
*&---------------------------------------------------------------------*
FORM f_fieldcat_modi USING p_fieldname TYPE slis_fieldname
                           p_column TYPE c
                           p_value TYPE any
                  CHANGING p_field_cat TYPE slis_t_fieldcat_alv.

  READ TABLE p_field_cat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  DATA(lv_name) = '<FS_FCAT>-' && p_column.

  ASSIGN (lv_name) TO FIELD-SYMBOL(<fs_colum>).

  CHECK sy-subrc EQ 0.

  <fs_colum> = p_value.

ENDFORM.
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

  IF sy-batch = 'X'.

    IF strlen( p_text ) > 40.
      MESSAGE s000(d2) WITH p_text(40) p_text+40.
    ELSE.
      MESSAGE s000(d2) WITH p_text.
    ENDIF.

  ELSE.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = p_percent
        text       = p_text.

  ENDIF.

ENDFORM.
*& --------------------------------------------------------------------*
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

  DATA lr_werks TYPE RANGE OF werks_d.
  DATA lr_matnr TYPE RANGE OF matnr.
  DATA lr_auart TYPE RANGE OF auart.
  DATA lr_vkorg TYPE RANGE OF vkorg.
  DATA lr_vtweg TYPE RANGE OF vtweg.
  DATA lr_spart TYPE RANGE OF spart.
  DATA lr_date  TYPE RANGE OF sy-datum.
  DATA lr_matkl TYPE RANGE OF matkl.
  DATA lv_ini TYPE sydatum.

  DATA lw_saida_alv LIKE LINE OF gt_dados_alv.

  CHECK rs_selfield-value IS NOT INITIAL.

  READ TABLE gt_dados_alv INTO lw_saida_alv INDEX rs_selfield-tabindex.

  CHECK lw_saida_alv-werks IS NOT INITIAL AND lw_saida_alv-matnr IS NOT INITIAL.

  lr_werks = VALUE #( ( sign = 'I' option = 'EQ' low = lw_saida_alv-werks ) ).
  lr_matnr = VALUE #( ( sign = 'I' option = 'EQ' low = lw_saida_alv-matnr ) ).

  lv_ini = sy-datum - 700.

  lr_date = VALUE #( ( sign = 'I' option = 'BT' low = lv_ini high = sy-datum ) ).

  CASE rs_selfield-fieldname.
    WHEN 'ESTOQUE'.

      SUBMIT zmmr129
               WITH s_cent IN lr_werks
               WITH s_matn IN lr_matnr AND RETURN.


    WHEN 'VENDAS'.

      lr_auart = VALUE #( ( sign = 'I' option = 'EQ' low = lw_saida_alv-auart ) ).
      lr_vkorg = VALUE #( ( sign = 'I' option = 'EQ' low = lw_saida_alv-bukrs ) ).
      lr_vtweg = VALUE #( ( sign = 'I' option = 'BT' low = '01' high = '99' ) ).
      lr_spart = VALUE #( ( sign = 'I' option = 'BT' low = '01' high = '99' ) ).


      SUBMIT zsdr0018
        WITH sd = abap_true
        WITH mm = abap_false
        WITH ativo_sd = abap_true
        WITH ativo_mm = abap_false
        WITH p_cent IN lr_werks
        WITH p_mater IN lr_matnr
        WITH p_tpcont IN lr_auart
        WITH p_orgven IN lr_vkorg
        WITH p_cdist IN lr_vtweg
        WITH p_sativ IN lr_spart
        WITH p_datent IN lr_date
        WITH p_fatuv  IN lr_date AND RETURN.

    WHEN 'COMPRAS'.

      lr_auart = VALUE #( ( sign = 'I' option = 'EQ' low = lw_saida_alv-bsart ) ).
      lr_vkorg = VALUE #( ( sign = 'I' option = 'EQ' low = lw_saida_alv-bukrs ) ).
      lr_spart = VALUE #( ( sign = 'I' option = 'BT' low = '01' high = '99' ) ).
      lr_matkl = VALUE #( ( sign = 'I' option = 'EQ' low = lw_saida_alv-matkl  ) ).

      SUBMIT zsdr0018
          WITH sd = abap_false
          WITH mm = abap_true
          WITH bloq_mm = abap_false
          WITH elim_mm = abap_false
          WITH ativo_mm = abap_true
          WITH todos_mm = abap_false
          WITH p_centro IN lr_werks
          WITH p_mat IN lr_matnr
          WITH p_pedido IN lr_auart
          WITH p_emp IN lr_vkorg
          WITH p_matkl IN lr_matkl
          WITH p_sativ IN lr_spart
          WITH p_data IN lr_date AND RETURN.

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


  "IF REDUCE i( INIT x = 0 FOR ls IN it_saida WHERE ( check IS NOT INITIAL ) NEXT x = x + 1 ) NE 1.
  "MESSAGE 'Selecione uma linha por vez' TYPE 'I'.
  "EXIT.
  "ENDIF.

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
*& Form f_delete_fieldcat
*&---------------------------------------------------------------------*
FORM f_delete_fieldcat  USING p_fieldname TYPE slis_fieldname.

  DELETE gt_fieldcat WHERE fieldname = p_fieldname.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f4_for_cultivar
*&---------------------------------------------------------------------*
FORM f4_for_cultivar CHANGING cv_culti TYPE zsde_cultivar.

  DATA lt_values TYPE STANDARD TABLE OF zmmt0217.
  DATA lt_return TYPE STANDARD TABLE OF ddshretval.


  SELECT * FROM zmmt0217 INTO TABLE lt_values.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DESC_CULTIVAR'    " Name of field in VALUE_TAB
      value_org       = 'S'        " Value return: C: cell by cell, S: structured
    TABLES
      value_tab       = lt_values  " Table of values: entries cell by cell
      return_tab      = lt_return  " Return the selected value
    EXCEPTIONS
      parameter_error = 1          " Incorrect parameter
      no_values_found = 2          " No values found
      OTHERS          = 3.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  cv_culti = ls_return-fieldval.

ENDFORM.
