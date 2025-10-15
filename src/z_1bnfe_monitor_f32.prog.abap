*----------------------------------------------------------------------*
***INCLUDE Z_1BNFE_MONITOR_F32 .
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.

TYPES : BEGIN OF ty_carta_correc,
          docnum        TYPE zcarta_correcao-docnum,
          id_cc         TYPE zcarta_correcao-id_cc,
          authcode      TYPE zcarta_correcao-authcode,
          dt_authcod    TYPE zcarta_correcao-dt_authcod,
          hr_authcod    TYPE zcarta_correcao-hr_authcod,
          code          TYPE zcarta_correcao-code,
          dt_atualizado TYPE zcarta_correcao-dt_atualizado,
          hr_atualizado TYPE zcarta_correcao-hr_atualizado,
          msg           TYPE zcarta_correcao-msg,
          msg_correc    TYPE c LENGTH 1000,
          usuario       TYPE zcarta_correcao-usuario,
          status_trans  TYPE zcarta_correcao-status_trans,
          status_icon   TYPE c LENGTH 4,
          novo_terminal TYPE zcarta_correcao-novo_terminal,
          doc_material  TYPE zcarta_correcao-doc_material,
          ano_material  TYPE zcarta_correcao-ano_material,
        END OF ty_carta_correc.

DATA : tl_carta_correc           TYPE TABLE OF ty_carta_correc,
       tl_carta_correc_selection TYPE TABLE OF ty_carta_correc,
       sl_carta_correc_selection TYPE ty_carta_correc,
       sl_carta_correc           TYPE ty_carta_correc,
       sl_zsdt_depara_depo       TYPE zsdt_depara_depo,
       wa_zcarta_correcao        TYPE zcarta_correcao.


DATA : wa_cont   TYPE REF TO cl_gui_custom_container , " Objeto Container
       wa_alv_cc TYPE REF TO cl_gui_alv_grid, " Objeto ALV.
       wa_event  TYPE REF TO lcl_event_receiver,
       it_fcat   TYPE TABLE OF lvc_s_fcat,
       s_variant TYPE disvariant,              " Tabela Estrutura colunas relatorio
       wa_layout TYPE lvc_s_layo            . " Layout da Lista / Fim do DATA

*&---------------------------------------------------------------------*
*&      Form  CARTA_CORRECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carta_correcao .
  REFRESH : it_zcarta_correcao, tl_carta_correc.
  IF wa_alv_selection IS NOT INITIAL.

    IF ( wa_alv_selection-docsta      EQ '1' AND
         wa_alv_selection-cancel      NE abap_true AND
         wa_alv_selection-scssta      NE '2' AND
         wa_alv_selection-action_requ IS NOT INITIAL ).

      PERFORM : zsel_carta_correc,
                f_alv_cc.
      CALL SCREEN 0104  STARTING AT 02 02 ENDING AT 100 25.
    ELSE.
      MESSAGE 'A NF-e deve estar autorizada para lançar carta de correção !' TYPE 'W'.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE 'Nenhuma NF-e Selecionada !' TYPE 'W'.
    EXIT.
  ENDIF.

ENDFORM.                    " CARTA_CORRECAO
*&---------------------------------------------------------------------*
*&      Form  LANC_CARTA_CORRECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lanc_carta_correcao .

  TYPES: BEGIN OF ty_doc_werks_lgort,
           docnum TYPE j_1bnflin-docnum,
           lgort  TYPE lgort_d,
           werks  TYPE werks_d,
         END OF ty_doc_werks_lgort.

  DATA: tg_doc_werks_lgort TYPE TABLE OF ty_doc_werks_lgort WITH HEADER LINE.

  DATA: var_answer TYPE c.
  DATA: zcl_cce TYPE REF TO zcl_cce.

  DATA : vl_length          TYPE i,
         v_text_correc      TYPE char1000sf,
         vl_id              TYPE zcarta_correcao-id_cc,
         ls_zcarta_correcao TYPE zcarta_correcao.

  CLEAR: vl_id.
  IF obg_descbox IS NOT INITIAL.
    CALL METHOD obg_descbox->get_text_as_r3table
      IMPORTING
        table = tg_editor.

    LOOP AT tg_editor INTO wg_editor.
      IF sy-tabix EQ 1.
        txt_correc = wg_editor-line.

      ELSEIF sy-tabix GE 2.
        CONCATENATE txt_correc wg_editor-line INTO txt_correc SEPARATED BY space.

      ENDIF.
    ENDLOOP.

  ENDIF.

  vl_length = strlen( txt_correc ).

  IF vl_length < 15 OR vl_length > 1000.
    MESSAGE 'A carta de correção deve ter entre 15 a 1000 caracteres!' TYPE 'E'.
  ENDIF.

  v_text_correc = txt_correc.

  FREE: zcl_cce.
  CREATE OBJECT zcl_cce.

  DATA(_transf_auto) = ''.
  IF lines( it_alv_selection[] ) > 1.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Atenção'
        text_question         = 'Deseja realmente criar uma carta de correção para cada documento selecionado?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK var_answer = '1'.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Atenção'
        text_question         = 'Deseja realizar movimentação de estoque após a autorização das Cartas de Correção?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF var_answer = '1'.

      CLEAR: tg_doc_werks_lgort[].
      LOOP AT it_alv_selection INTO wa_alv_selection.

        DATA(_disponivel) = ''.
        PERFORM: verificar_disp_transf CHANGING _disponivel.
        IF _disponivel IS INITIAL.
          RETURN.
        ENDIF.

        tg_doc_werks_lgort-docnum = wa_alv_selection-docnum.

        SELECT SINGLE *
          FROM j_1bnflin INTO @DATA(_lin)
         WHERE docnum = @wa_alv_selection-docnum.

        IF ( sy-subrc = 0 ) AND ( _lin-refkey(10) IS NOT INITIAL ).
          SELECT SINGLE c~*
            INTO @DATA(_zsdt0023)
            FROM vbrk AS a INNER JOIN vbfa     AS b ON a~vbeln = b~vbeln
                           INNER JOIN zsdt0023 AS c ON b~vbelv = c~vbeln
           WHERE a~vbeln EQ @_lin-refkey(10)
             AND b~vbtyp_n EQ 'M'
             AND b~vbtyp_v EQ 'J'.

          IF sy-subrc = 0.
            tg_doc_werks_lgort-werks = _zsdt0023-werks_v.
            tg_doc_werks_lgort-lgort = _zsdt0023-lgort_v.
          ENDIF.
        ENDIF.

        APPEND tg_doc_werks_lgort.
      ENDLOOP.

      SORT tg_doc_werks_lgort BY werks lgort.
      DELETE ADJACENT DUPLICATES FROM tg_doc_werks_lgort COMPARING werks lgort.

      IF lines( tg_doc_werks_lgort[] ) > 1.
        MESSAGE 'Documentos devem ter o mesmo Centro/Deposito Virtual!' TYPE 'S'.
        RETURN.
      ELSEIF lines( tg_doc_werks_lgort[] ) = 1.
        READ TABLE tg_doc_werks_lgort INDEX 1.
        IF sy-subrc = 0.
          gf_werks         = tg_doc_werks_lgort-werks.
          gf_lgort_origem  = tg_doc_werks_lgort-lgort.
        ENDIF.
      ENDIF.

      _transf_auto            = 'X'.
      gf_inf_dados_transf_cce = 'X'.
      gf_opt_dep = 'X'.
      gf_opt_cen = ' '.
      CALL SCREEN 0105 STARTING AT 2 1 ENDING AT 45 10.
      gf_inf_dados_transf_cce = ''.

      CASE 'X'.
        WHEN gf_opt_dep.
          DATA(_tp_transf) = '1'.  "Transferência por Depósito
        WHEN gf_opt_cen.
          _tp_transf       = '2'.  "Transferência por Centro
        WHEN OTHERS.
          _tp_transf       = ''.
      ENDCASE.
    ENDIF.

  ENDIF.

  LOOP AT it_alv_selection INTO wa_alv_selection.

    zcl_cce->novo_registro( ).
    zcl_cce->set_docnum( wa_alv_selection-docnum ).
    zcl_cce->set_texto_correcao( v_text_correc ).

    LOOP AT it_corr_parc.
      CASE it_corr_parc-parvw.
        WHEN 'PC'.
          zcl_cce->set_novo_loc_coleta( it_corr_parc-new_parid ).
        WHEN 'LR'.
          zcl_cce->set_novo_loc_entrega( it_corr_parc-new_parid ).
        WHEN 'Z1'.
          zcl_cce->set_novo_terminal( it_corr_parc-new_parid ).
      ENDCASE.
    ENDLOOP.

    IF _transf_auto IS NOT INITIAL.
      CALL METHOD zcl_cce->set_dados_transf_auto
        EXPORTING
          i_tp_transf        = _tp_transf
          i_centro_origem    = gf_werks
          i_deposito_origem  = gf_lgort_origem
          i_centro_destino   = gf_werks_d
          i_deposito_destino = gf_lgort_destino.
    ENDIF.

    zcl_cce->gravar_registro( RECEIVING i_gravou = DATA(_gravou) ).

    IF _gravou IS INITIAL.
      RETURN.
    ENDIF.

  ENDLOOP.


*    SELECT SINGLE MAX( ID_CC )
*      INTO VL_ID
*      FROM ZCARTA_CORRECAO
*     WHERE DOCNUM = WA_ALV_SELECTION-DOCNUM.
*
*    IF VL_ID IS INITIAL .
*      VL_ID  = 0.
*    ENDIF.
*
*    VL_ID = VL_ID + 1.
*
*    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN TXT_CORREC WITH 'a' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN TXT_CORREC WITH 'e' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF        'í'     IN TXT_CORREC WITH 'i' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN TXT_CORREC WITH 'o' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN TXT_CORREC WITH 'u' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN TXT_CORREC WITH 'c' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF        '&'     IN TXT_CORREC WITH '&#38;'.
*    REPLACE ALL OCCURRENCES OF        ''''    IN TXT_CORREC WITH '&#39;'.
*    REPLACE ALL OCCURRENCES OF        'º'     IN TXT_CORREC WITH 'o' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF        'ª'     IN TXT_CORREC WITH 'a' IGNORING CASE.
*
*    LS_ZCARTA_CORRECAO-DOCNUM      = WA_ALV_SELECTION-DOCNUM.
*    LS_ZCARTA_CORRECAO-ID_CC       = VL_ID.
*    LS_ZCARTA_CORRECAO-MSG_CORREC1 = TXT_CORREC(250).
*    LS_ZCARTA_CORRECAO-MSG_CORREC2 = TXT_CORREC+250(250).
*    LS_ZCARTA_CORRECAO-MSG_CORREC3 = TXT_CORREC+500(250).
*    LS_ZCARTA_CORRECAO-MSG_CORREC4 = TXT_CORREC+750(250).
*    LS_ZCARTA_CORRECAO-USUARIO     = SY-UNAME.
*
*    LOOP AT IT_CORR_PARC.
*
*      CASE IT_CORR_PARC-PARVW.
*        WHEN 'PC'.
*          LS_ZCARTA_CORRECAO-NOVO_LOC_COLETA = IT_CORR_PARC-NEW_PARID.
*        WHEN 'LR'.
*          LS_ZCARTA_CORRECAO-NOVO_LOC_ENTREGA = IT_CORR_PARC-NEW_PARID.
*        WHEN 'Z1'.
*          LS_ZCARTA_CORRECAO-NOVO_TERMINAL = IT_CORR_PARC-NEW_PARID.
*      ENDCASE.
*    ENDLOOP.
*
*    MODIFY ZCARTA_CORRECAO FROM LS_ZCARTA_CORRECAO.
*
*    CALL FUNCTION 'Z_MONTA_XML_CTA_CORRECAO'
*      EXPORTING
*        P_DOCNUM     = WA_ALV_SELECTION-DOCNUM
*        P_TXT_CORREC = TXT_CORREC.
*
*  CLEAR TXT_CORREC.

  PERFORM zsel_carta_correc.

  CALL METHOD obg_descbox->delete_text.

  "clear obg_descbox.
ENDFORM.                    " LANC_CARTA_CORRECAO

*&---------------------------------------------------------------------*
*&      Form  zexibe_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM zrefresh_alv_cc.
  CALL METHOD wa_alv_cc->refresh_table_display.

ENDFORM.                    "zrefresh_alv_cc
*&---------------------------------------------------------------------*
*&      Form  zsel_carta_correc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zsel_carta_correc.

  REFRESH: it_zcarta_correcao, tl_carta_correc.

  SELECT *
    FROM zcarta_correcao
    INTO TABLE it_zcarta_correcao
   WHERE docnum EQ wa_alv_selection-docnum.

  SORT it_zcarta_correcao BY id_cc.

  LOOP AT it_zcarta_correcao INTO wa_zcarta_correcao.



    MOVE-CORRESPONDING wa_zcarta_correcao TO sl_carta_correc.

    CONCATENATE wa_zcarta_correcao-msg_correc1
                wa_zcarta_correcao-msg_correc2
                wa_zcarta_correcao-msg_correc3
                wa_zcarta_correcao-msg_correc4 INTO  sl_carta_correc-msg_correc.

    CASE wa_zcarta_correcao-status_trans.
      WHEN: 'X'.
        sl_carta_correc-status_icon = icon_led_green.
      WHEN OTHERS.
        sl_carta_correc-status_icon =  icon_led_yellow.
    ENDCASE.


    APPEND sl_carta_correc TO tl_carta_correc .

  ENDLOOP.

ENDFORM.                    "zsel_carta_correc

*&---------------------------------------------------------------------*
*&      Form  zsel_carta_correc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zimp_carta_correc.

  DATA: vl_ds_url TYPE agr_url.

  DATA: zcl_cce TYPE REF TO zcl_cce.

  LOOP AT tl_carta_correc_selection INTO sl_carta_correc_selection.

    FREE zcl_cce.
    CREATE OBJECT zcl_cce
      EXPORTING
        i_docnum = sl_carta_correc_selection-docnum
        i_id_cc  = sl_carta_correc_selection-id_cc.

    zcl_cce->imprimir( ).

*    SELECT SINGLE DS_URL
*      INTO VL_DS_URL
*      FROM ZCARTA_CORRECAO
*     WHERE DOCNUM EQ SL_CARTA_CORREC_SELECTION-DOCNUM
*       AND ID_CC  EQ SL_CARTA_CORREC_SELECTION-ID_CC.
*
*    IF SY-SUBRC IS INITIAL.
*
*      CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
*        EXPORTING
*          NODE_DATA = VL_DS_URL.
*    ENDIF.

  ENDLOOP.

ENDFORM.                    "zimp_carta_correc

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no                      ,

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive                   ,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.


ENDCLASS.                    "lcl_event_receiver DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
    "perform z_handle_hotspot using    e_row_id
    "                                  e_column_id
    "                                  es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    "perform z_handle_toolbar using e_object
    "                               e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    " perform z_handle_command using e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command
  "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION



*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV_FAT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_exibe_alv_cc OUTPUT.

  s_variant-report = sy-repid.
  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CC_ALV_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
  IF wa_alv_cc IS INITIAL AND NOT
    wa_cont IS INITIAL.

    CREATE OBJECT wa_alv_cc
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event IS INITIAL.

    CREATE OBJECT wa_event.

    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv_cc.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv_cc.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv_cc.

  ENDIF.

  wa_layout-sel_mode = 'A'.

  CALL METHOD wa_alv_cc->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      i_default                     = 'X'
      is_variant                    = s_variant      "is_layout = s_layout
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = tl_carta_correc
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK NOT wa_alv_cc IS INITIAL.


ENDMODULE.                 " Z_EXIBE_ALV_FAT  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  alv_preenche_cat2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TABNAME  text
*      -->P_CAMPO    text
*      -->P_DESC     text
*      -->P_TAM      text
*      -->P_HOT      text
*      -->P_ZERO     text
*----------------------------------------------------------------------*
FORM alv_preenche_cat_cc USING   p_tabname TYPE dd02d-tabname
                               p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c           .
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = p_tabname .
  wl_fcat-fieldname = p_campo   .
  wl_fcat-scrtext_l = p_desc    .
  wl_fcat-scrtext_m = p_desc    .
  wl_fcat-scrtext_s = p_desc    .
  wl_fcat-hotspot   = p_hot     .
  wl_fcat-outputlen = p_tam     .
  wl_fcat-no_zero   = p_zero    .

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    "alv_preenche_cat2

*&---------------------------------------------------------------------*
*&      Form  F_ALV_FAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_cc .
  REFRESH  it_fcat.
  PERFORM alv_preenche_cat_cc USING:

        'TL_CARTA_CORREC' 'STATUS_ICON'   TEXT-016   '6'  ' '  ' ',"
        "'TL_CARTA_CORREC' 'MSG_CORREC'    TEXT-014   '40'  ' '  ' ',"
        'TL_CARTA_CORREC' 'AUTHCODE'      TEXT-007   '9'  ' '  ' ',"
        'TL_CARTA_CORREC' 'DT_AUTHCOD'    TEXT-008   '12'  ' '  ' ',"
        'TL_CARTA_CORREC' 'HR_AUTHCOD'    TEXT-009   '14'  ' '  ' ',"
        'TL_CARTA_CORREC' 'CODE'          TEXT-010   '8'  ' '  ' ',"
        'TL_CARTA_CORREC' 'DT_ATUALIZADO' TEXT-011   '14'  ' '  ' ',"
        'TL_CARTA_CORREC' 'HR_ATUALIZADO' TEXT-012   '12'  ' '  ' ',"
        'TL_CARTA_CORREC' 'MSG'           TEXT-013   '40'  ' '  ' ',
        'TL_CARTA_CORREC' 'USUARIO'       TEXT-015   '10'  ' '  ' '."


ENDFORM.                    " F_ALV_FAT
*&---------------------------------------------------------------------*
*&      Form  TRANS_ESTOQUE_CARTA_CORREC
*&---------------------------------------------------------------------*
FORM trans_estoque_carta_correc .

  DATA: wa_carta_correcao TYPE zcarta_correcao,
        wa_zsdt_retlote   TYPE zsdt_retlote.

  DATA: tl_rows       TYPE lvc_t_row,
        sl_rows       TYPE lvc_s_row,
        var_answer    TYPE c,
        vl_doc_msg    TYPE string,
        vl_msg_exibir TYPE string.


  SELECT SINGLE *
    INTO wa_zsdt_retlote
    FROM zsdt_retlote
   WHERE docnum = wa_alv_selection-docnum.

*  IF SY-SUBRC = 0.
*    VL_DOC_MSG = WA_ZSDT_RETLOTE-DOCNUM_RET.
*    CONCATENATE 'Nota já está vinculada a um Retorno de formação de lote:' VL_DOC_MSG '!' INTO VL_MSG_EXIBIR.
*    MESSAGE VL_MSG_EXIBIR TYPE 'W'.
*    RETURN.
*  ENDIF.

  CALL METHOD wa_alv_cc->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  READ TABLE tl_rows INTO sl_rows INDEX 1.

  READ TABLE tl_carta_correc INTO sl_carta_correc_selection INDEX sl_rows-index.

  SELECT SINGLE * FROM zcarta_correcao INTO wa_carta_correcao WHERE docnum EQ sl_carta_correc_selection-docnum
                                                                AND id_cc  EQ sl_carta_correc_selection-id_cc.

  IF ( sy-subrc EQ 0 ).

    IF ( wa_carta_correcao-authcode IS INITIAL ).
      MESSAGE 'Carta de correção não esta autorizada' TYPE 'W'.
      EXIT.
    ELSE.

      IF ( wa_carta_correcao-status_trans EQ 'X' ).

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Transferência de Estoque'
            text_question         = 'Já existem movimentações de estoque para esta carta de correção, estornar via MBST. Fazer nova movimentação?'
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        CASE var_answer.
          WHEN: '1'.
            gf_inf_dados_transf_cce = ''.
            gf_opt_dep = 'X'.
            gf_opt_cen = ' '.
            CALL SCREEN 0105 STARTING AT 2 1 ENDING AT 45 10.
        ENDCASE.

        "MESSAGE 'Transferência já efetuada.' TYPE 'W'.
        "EXIT.

      ELSEIF NOT ( wa_carta_correcao-authcode IS INITIAL ) AND ( wa_carta_correcao-status_trans IS INITIAL ).
        gf_inf_dados_transf_cce = ''.
        gf_opt_dep = 'X'.
        gf_opt_cen = ' '.
        CALL SCREEN 0105 STARTING AT 2 1 ENDING AT 45 10.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " TRANS_ESTOQUE_CARTA_CORREC
*&---------------------------------------------------------------------*
*&      Form  TRANSF_ESTOQUE
*&---------------------------------------------------------------------*
FORM transf_estoque .

  DATA: zcl_cce TYPE REF TO zcl_cce.

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  CLEAR: tl_rows, sl_rows.

  CALL METHOD wa_alv_cc->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  READ TABLE tl_rows INTO sl_rows INDEX 1.
  CHECK sy-subrc = 0.

  READ TABLE tl_carta_correc INTO sl_carta_correc_selection INDEX sl_rows-index.
  CHECK sy-subrc = 0.

  FREE zcl_cce.
  CREATE OBJECT zcl_cce
    EXPORTING
      i_docnum = sl_carta_correc_selection-docnum
      i_id_cc  = sl_carta_correc_selection-id_cc.

  CASE 'X'.
    WHEN gf_opt_dep.
      DATA(_tp_transf) = '1'.  "Transferência por Depósito
    WHEN gf_opt_cen.
      _tp_transf       = '2'.  "Transferência por Centro
    WHEN OTHERS.
      _tp_transf       = ''.
  ENDCASE.

  CALL METHOD zcl_cce->transf_estoque
    EXPORTING
      i_tp_transf        = _tp_transf
      i_centro_origem    = gf_werks
      i_deposito_origem  = gf_lgort_origem
      i_centro_destino   = gf_werks_d
      i_deposito_destino = gf_lgort_destino
    RECEIVING
      e_gravou           = DATA(_gravou).

  IF _gravou IS NOT INITIAL.
    PERFORM zsel_carta_correc.
    LEAVE TO SCREEN 0.
  ENDIF.

*  DATA: WA_LIN TYPE J_1BNFLIN,
*        WA_DOC TYPE J_1BNFDOC.
*
*  DATA: FLAG_ERROR TYPE C.
*
*  DATA: DIA(2)   TYPE C,
*        MES(2)   TYPE C,
*        ANO(4)   TYPE C,
*        DATA(10) TYPE C.
*
*  DATA: BEGIN OF GMHEAD.
*          INCLUDE STRUCTURE BAPI2017_GM_HEAD_01.
*  DATA: END OF GMHEAD.
*
*  DATA: BEGIN OF GMCODE.
*          INCLUDE STRUCTURE BAPI2017_GM_CODE.
*  DATA: END OF GMCODE.
*
*  DATA: BEGIN OF IT_GMCREATE OCCURS 100.
*          INCLUDE STRUCTURE BAPI2017_GM_ITEM_CREATE.
*  DATA: END OF IT_GMCREATE.
*
*  DATA: BEGIN OF ERRMSG OCCURS 10.
*          INCLUDE STRUCTURE BAPIRET2.
*  DATA: END OF ERRMSG.
*
*  DATA: TL_ROWS            TYPE LVC_T_ROW,
*        SL_ROWS            TYPE LVC_S_ROW,
*        WA_ZSDT_DEPARA_CEN1 TYPE ZSDT_DEPARA_CEN,
*        WA_ZSDT_DEPARA_CEN2 TYPE ZSDT_DEPARA_CEN.
*
*  IF GF_OPT_CEN = 'X'.
*    SELECT SINGLE *
*      FROM ZSDT_DEPARA_CEN
*      INTO WA_ZSDT_DEPARA_CEN1
*      WHERE CENTROV_1 = GF_WERKS.
*
*    IF SY-SUBRC NE 0.
*      MESSAGE 'Centro Origem deve ser Virtual' TYPE 'I'.
*      EXIT.
*    ENDIF.
*    "
*    SELECT SINGLE *
*      FROM ZSDT_DEPARA_CEN
*      INTO WA_ZSDT_DEPARA_CEN2
*      WHERE CENTROV_1 = GF_WERKS_D.
*
*    IF SY-SUBRC NE 0.
*      MESSAGE 'Centro Destino deve ser Virtual' TYPE 'I'.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  IF WA_ZSDT_DEPARA_CEN1-CENTRO_REAL NE WA_ZSDT_DEPARA_CEN2-CENTRO_REAL.
*    MESSAGE 'Cen. ori/dest. devem pertencer ao mesmo Cen.Real ' TYPE 'I'.
*    EXIT.
*  ENDIF.
*  CLEAR: TL_ROWS, SL_ROWS.
*
*  CALL METHOD WA_ALV_CC->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = TL_ROWS.
*
*  READ TABLE TL_ROWS INTO SL_ROWS INDEX 1.
*
*  READ TABLE TL_CARTA_CORREC INTO SL_CARTA_CORREC_SELECTION INDEX SL_ROWS-INDEX.
*
*  IF ( SY-SUBRC EQ 0 ).
*
*    SELECT SINGLE * FROM J_1BNFLIN INTO WA_LIN WHERE DOCNUM EQ SL_CARTA_CORREC_SELECTION-DOCNUM.
*    IF ( SY-SUBRC EQ 0 ).
*      SELECT SINGLE * FROM J_1BNFDOC INTO WA_DOC WHERE DOCNUM EQ SL_CARTA_CORREC_SELECTION-DOCNUM.
*
*      IF ( SY-SUBRC EQ 0 ).
*
*        GMHEAD-PSTNG_DATE = SY-DATUM.
*        GMHEAD-DOC_DATE   = SY-DATUM.
*
*        DIA  = WA_DOC-DOCDAT+6(2).
*        MES  = WA_DOC-DOCDAT+4(2).
*        ANO  = WA_DOC-DOCDAT(4).
*        CONCATENATE DIA '.' MES '.' ANO INTO DATA.
*        CONCATENATE WA_DOC-NFENUM DATA INTO GMHEAD-HEADER_TXT SEPARATED BY SPACE.
*
*        GMCODE-GM_CODE = '06'.
*
*        IT_GMCREATE-MATERIAL   = WA_LIN-MATNR.
*        IT_GMCREATE-PLANT      = GF_WERKS.
*        IT_GMCREATE-STGE_LOC   = GF_LGORT_ORIGEM.
*        IT_GMCREATE-BATCH      = WA_LIN-CHARG.
*        IF GF_OPT_CEN = 'X'.
*          IT_GMCREATE-MOVE_TYPE  = '301'.
*          IT_GMCREATE-MOVE_PLANT = GF_WERKS_D.
*        ELSE.
*          IT_GMCREATE-MOVE_TYPE  = 'ZA5'.
*          CLEAR IT_GMCREATE-MOVE_PLANT.
*        ENDIF.
*        IT_GMCREATE-ENTRY_QNT  = WA_LIN-MENGE.
*        IT_GMCREATE-ENTRY_UOM  = WA_LIN-MEINS.
*        IT_GMCREATE-MOVE_STLOC = GF_LGORT_DESTINO.
*
*        APPEND IT_GMCREATE.
*
*        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*          EXPORTING
*            GOODSMVT_HEADER = GMHEAD
*            GOODSMVT_CODE   = GMCODE
*          TABLES
*            GOODSMVT_ITEM   = IT_GMCREATE
*            RETURN          = ERRMSG.
*
*        CLEAR: FLAG_ERROR.
*        LOOP AT ERRMSG.
*          IF ERRMSG-TYPE EQ 'E'.
*            FLAG_ERROR = 'X'.
*          ENDIF.
*        ENDLOOP.
**** Modificação - Eduardo Ruttkowski Tavares - 12.09.2013 >>> INI
** CH  109593 - Ajuste Z_1BNFE_MONITOR_F32
*        IF FLAG_ERROR NE 'X'.
*          SELECT SINGLE *
*            FROM ZSDT_DEPARA_DEPO
*            INTO SL_ZSDT_DEPARA_DEPO
*              WHERE LGORT EQ GF_LGORT_DESTINO.
*          WA_ZCARTA_CORRECAO-NOVO_TERMINAL = SL_ZSDT_DEPARA_DEPO-LIFNR.
*        ENDIF.
**** Modificação - Eduardo Ruttkowski Tavares - 12.09.2013 >>> END
*
*        IF ( FLAG_ERROR NE 'X' ).
*
*
*          COMMIT WORK AND WAIT.
*          IF ( SY-SUBRC EQ 0 ).
*
*            UPDATE ZCARTA_CORRECAO SET STATUS_TRANS  = 'X'
*                                       NOVO_TERMINAL = SL_ZSDT_DEPARA_DEPO-LIFNR
*                                                          WHERE DOCNUM EQ SL_CARTA_CORREC_SELECTION-DOCNUM
*                                                            AND ID_CC  EQ SL_CARTA_CORREC_SELECTION-ID_CC.
*            IF ( SY-SUBRC EQ 0 ).
*              SL_CARTA_CORREC_SELECTION-STATUS_TRANS = 'X'.
*              SL_CARTA_CORREC_SELECTION-STATUS_ICON = ICON_LED_GREEN.
*
*              MODIFY TL_CARTA_CORREC FROM SL_CARTA_CORREC_SELECTION INDEX SL_ROWS-INDEX.
*              CALL METHOD WA_ALV_CC->REFRESH_TABLE_DISPLAY.
*              MESSAGE 'Transferência de Estoque com Sucesso.' TYPE 'S'.
*
*              LEAVE TO SCREEN 0.
*            ENDIF.
*
*          ENDIF.
*
*        ELSE.
*          MESSAGE ERRMSG-MESSAGE TYPE 'W'.
*        ENDIF.
*
*        CLEAR: GF_WERKS, GF_LGORT_ORIGEM, GF_LGORT_DESTINO.
*      ELSE.
*
*      ENDIF.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " TRANSF_ESTOQUE
*&---------------------------------------------------------------------*
*&      Form  CARTA_CORRECAO_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carta_correcao_cte .
  IF ( it_alv_selection[] IS NOT INITIAL ) AND ( wa_alv_selection IS NOT INITIAL ).

    IF ( wa_alv_selection-docsta      EQ '1' AND
         wa_alv_selection-cancel      NE abap_true AND
         wa_alv_selection-scssta      NE '2' AND
         wa_alv_selection-action_requ IS NOT INITIAL ).

      PERFORM : zsel_carta_correc,
                f_alv_cc.
      REFRESH it_cte_corr.
      wl_desactive = 'X'.
      "04  02
      CALL SCREEN 0106  STARTING AT  02 02 ENDING AT 139 24.
    ELSE.
      MESSAGE 'A CT-e deve estar autorizada para lançar carta de correção !' TYPE 'W'.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE 'Nenhuma CT-e Selecionada !' TYPE 'W'.
    EXIT.
  ENDIF.
ENDFORM.                    " CARTA_CORRECAO_CTE
*&---------------------------------------------------------------------*
*&      Form  GRAVA_CTE_CORRECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_cte_correcao .
  DATA : vl_length          TYPE i,
         vl_id              TYPE zcarta_correcao-id_cc,
         ls_zcarta_correcao TYPE zcarta_correcao,
         tl_input_81        TYPE zsdt0081_t,
         wl_input_81        TYPE zsdt0081,
         wl_zsdt0080        TYPE zsdt0080.

  DATA: zcl_cce TYPE REF TO zcl_cce.

  CLEAR: vl_id.
*  IF OBG_DESCCTE IS NOT INITIAL.
*    CALL METHOD OBG_DESCCTE->GET_TEXT_AS_R3TABLE
*      IMPORTING
*        TABLE = TG_EDITOR.
*
*    LOOP AT TG_EDITOR INTO WG_EDITOR.
*      IF SY-TABIX EQ 1.
*        TXT_CORREC = WG_EDITOR-LINE.
*
*      ELSEIF SY-TABIX GE 2.
*        CONCATENATE TXT_CORREC WG_EDITOR-LINE INTO TXT_CORREC SEPARATED BY SPACE.
*
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.

  vl_length = strlen( txt_correc ).

  "IF VL_LENGTH < 15 OR VL_LENGTH > 1000.
  "  MESSAGE 'A observação deve ser de 15 a 1000 caracteres!' TYPE 'E'.
  "ENDIF.

  x_linha = 0.
  DELETE it_cte_corr WHERE grupo IS INITIAL AND campo IS INITIAL.
  LOOP AT it_cte_corr INTO wa_cte_corr  .
    ADD 1 TO x_linha.
    IF wa_cte_corr-grupo IS INITIAL OR
       wa_cte_corr-campo IS INITIAL OR
       wa_cte_corr-valor IS INITIAL.
      MESSAGE 'TAG incompleta!' TYPE 'E'.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0080
      INTO wl_zsdt0080
      WHERE grupo = wa_cte_corr-grupo
      AND   campo = wa_cte_corr-campo.
    IF sy-subrc NE 0.
      MESSAGE 'TAG inválida!' TYPE 'E'.
    ENDIF.

    IF ( ( wa_cte_corr-grupo = 'veic' ) OR
         ( wa_cte_corr-grupo = 'moto' ) OR
         ( wa_cte_corr-grupo = 'infQ' ) ) AND
         ( wa_cte_corr-pc_veiculo(2) IS INITIAL ).
      MESSAGE 'Placa não informada!' TYPE 'E'.
    ENDIF.

    IF ( ( wa_cte_corr-grupo = 'infNFe'  OR wa_cte_corr-grupo = 'infUnidTransp'  ) AND
         ( wa_cte_corr-chave(2) IS INITIAL ) ).
      MESSAGE 'Chave não informada!' TYPE 'E'.
    ENDIF.

  ENDLOOP.

  IF x_linha = 0.
    MESSAGE 'Informe ao menos uma TAG para correção!' TYPE 'E'.
  ENDIF.

*  SELECT SINGLE MAX( ID_CC )
*    INTO VL_ID
*    FROM ZCARTA_CORRECAO
*   WHERE DOCNUM = WA_ALV_SELECTION-DOCNUM.
*
*  IF VL_ID IS INITIAL .
*    VL_ID  = 0.
*  ENDIF.
*
*  VL_ID = VL_ID + 1.

*  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN TXT_CORREC WITH 'a' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN TXT_CORREC WITH 'e' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        'í'     IN TXT_CORREC WITH 'i' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN TXT_CORREC WITH 'o' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN TXT_CORREC WITH 'u' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN TXT_CORREC WITH 'c' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        '&'     IN TXT_CORREC WITH '&#38;'.
*  REPLACE ALL OCCURRENCES OF        ''''    IN TXT_CORREC WITH '&#39;'.
*  REPLACE ALL OCCURRENCES OF        'º'     IN TXT_CORREC WITH 'o' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        'ª'     IN TXT_CORREC WITH 'a' IGNORING CASE.
*
*  LS_ZCARTA_CORRECAO-DOCNUM      = WA_ALV_SELECTION-DOCNUM.
*  LS_ZCARTA_CORRECAO-ID_CC       = VL_ID.
*  LS_ZCARTA_CORRECAO-MSG_CORREC1 = TXT_CORREC(250).
*  LS_ZCARTA_CORRECAO-MSG_CORREC2 = TXT_CORREC+250(250).
*  LS_ZCARTA_CORRECAO-MSG_CORREC3 = TXT_CORREC+500(250).
*  LS_ZCARTA_CORRECAO-MSG_CORREC4 = TXT_CORREC+750(250).
*  LS_ZCARTA_CORRECAO-USUARIO     = SY-UNAME.
*  MODIFY ZCARTA_CORRECAO FROM LS_ZCARTA_CORRECAO.

  " Grava Tags
  REFRESH tl_input_81.
  LOOP AT it_cte_corr INTO wa_cte_corr  .

    CLEAR wl_input_81-item.

    MOVE: sy-mandt                 TO wl_input_81-mandt,
          wa_alv_selection-docnum  TO wl_input_81-docnum,
          wa_cte_corr-grupo        TO wl_input_81-grupo,
          wa_cte_corr-campo        TO wl_input_81-campo,
          wa_cte_corr-valor        TO wl_input_81-valor,
          wa_cte_corr-valor1       TO wl_input_81-valor1,
          wa_cte_corr-valor2       TO wl_input_81-valor2,
          wa_cte_corr-valor3       TO wl_input_81-valor3.

    CASE wa_cte_corr-grupo.
      WHEN: 'veic' OR 'moto' OR 'infQ'.
        wl_input_81-item = wa_cte_corr-pc_veiculo(2).
      WHEN: 'infNFe' OR 'infUnidTransp'.
        wl_input_81-item = wa_cte_corr-chave(2).
*      WHEN 'infUnidTransp'.
*        DATA(_LEN) = STRLEN( WA_CTE_CORR-CHAVE ).
*        IF _LEN > 44.
*          DATA(_SEP) = _LEN - 44.
*          TL_INPUT_81-CHAVE_NFE = WA_CTE_CORR-CHAVE+_SEP(44).
*        ENDIF.
    ENDCASE.

    APPEND wl_input_81 TO tl_input_81.
  ENDLOOP.

  "MODIFY ZSDT0081 FROM TABLE TL_INPUT_81.

  "REFRESH TL_INPUT_81.

  FREE: zcl_cce.
  CREATE OBJECT zcl_cce.

  zcl_cce->novo_registro( ).
  zcl_cce->set_docnum( wa_alv_selection-docnum ).
  zcl_cce->set_campos_correcao_cte( i_zsdt0081_t = tl_input_81 ).
  zcl_cce->gravar_registro( RECEIVING i_gravou = DATA(_gravou) ).

  IF _gravou IS INITIAL.
    RETURN.
  ENDIF.

*
*  CALL FUNCTION 'Z_MONTA_XML_CTE_CORRECAO'
*    EXPORTING
*      P_DOCNUM = WA_ALV_SELECTION-DOCNUM
*      P_ID_CC  = VL_ID.

  CLEAR txt_correc.

  REFRESH: it_cte_corr, tg_editor.

  PERFORM zsel_carta_correc.

  "CALL METHOD OBG_DESCCTE->DELETE_TEXT.

  wl_desactive = 'X'.

  CALL METHOD ctl_alv_cte_corr->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.

  "REFRESH TG_EDITOR.
  "CALL METHOD OBG_DESCCTE->SET_READONLY_MODE
  "  EXPORTING
  "   READONLY_MODE = 1.

ENDFORM.                    " GRAVA_CTE_CORRECAO
*&---------------------------------------------------------------------*
*&      Form  BUSCA_TAGS_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_tags_cte .
  DATA: tl_input_81        TYPE TABLE OF zsdt0081 WITH HEADER LINE.
  CLEAR it_selected_rows_cc.
  CALL METHOD ctl_alv_cte_resu->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows_cc.
  x_linha = 0.
  LOOP AT it_selected_rows_cc INTO wa_selected_rows_cc.
    ADD 1 TO x_linha .
  ENDLOOP.
  IF x_linha NE 1.
    MESSAGE 'Selecione apenas uma linha !' TYPE 'W'.
    EXIT.
  ENDIF.
  LOOP AT it_selected_rows_cc INTO wa_selected_rows_cc.
    READ TABLE tl_carta_correc INTO sl_carta_correc INDEX wa_selected_rows_cc-index.
    EXIT.
  ENDLOOP.
  REFRESH it_cte_corr.
  SELECT *
    FROM zsdt0081
    INTO TABLE tl_input_81
    WHERE docnum  = sl_carta_correc-docnum
    AND   id_cc   = sl_carta_correc-id_cc.

  LOOP AT tl_input_81.
    wa_cte_corr-grupo  = tl_input_81-grupo.
    wa_cte_corr-campo  = tl_input_81-campo.
    wa_cte_corr-valor  = tl_input_81-valor.
    wa_cte_corr-valor1 = tl_input_81-valor1.
    wa_cte_corr-valor2 = tl_input_81-valor2.
    wa_cte_corr-valor3 = tl_input_81-valor3.
    APPEND wa_cte_corr TO it_cte_corr.
  ENDLOOP.

  wl_desactive = 'X'.

  CALL METHOD ctl_alv_cte_corr->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.

  REFRESH tg_editor.
  "CALL METHOD OBG_DESCCTE->SET_READONLY_MODE
  "  EXPORTING
  "    READONLY_MODE = 1.

ENDFORM.                    " BUSCA_TAGS_CTE
*&---------------------------------------------------------------------*
*&      Form  MANIFESTO_ELETRONICO
*&---------------------------------------------------------------------*
FORM manifesto_eletronico RAISING zcx_error. "*-#133089-12.02.2024-JT.

  DATA: var_len     TYPE i,
        var_answer  TYPE c,
        wa_zsdt0105 TYPE zsdt0105,
        vl_msg      TYPE string.

  CLEAR: vg_init_mdfe, vg_new_mdfe.

*-#133089-21.02.2024-JT-inicio
  IF vg_faturamento_autom = abap_true.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  var_len = lines( it_alv_selection ).

  IF ( var_len EQ 0 ).
    vl_msg = 'Nenhum documento selecionado!'.
    MESSAGE vl_msg TYPE 'W'.
    RETURN.
  ENDIF.

  CASE sy-tcode.
    WHEN 'ZMDFE'.

      READ TABLE it_alv_selection INDEX 1 INTO wa_alv_selection.

      SELECT SINGLE * INTO @DATA(wa_mdfe)
        FROM zsdt0237
       WHERE docnum EQ @wa_alv_selection-docnum.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'Z_GRC_MDFE_AVULSA'
          EXPORTING
            i_docnum = wa_alv_selection-docnum.
      ELSE.
        CALL SCREEN 0107 STARTING AT 02 02 ENDING AT 156 27.
      ENDIF.

    WHEN OTHERS.

      TRY .
          LOOP AT it_alv_selection INTO wa_alv_selection.

            SELECT SINGLE * FROM zlest0061 INTO @DATA(_wl_zlest0061) WHERE docnum EQ @wa_alv_selection-docnum.
            CHECK sy-subrc NE 0.

            zcl_faturamento=>zif_faturamento~get_instance(
              )->get_processo_emissao_docs( EXPORTING i_docnum = wa_alv_selection-docnum
              )->get_ck_emissao_mdfe( i_docnum = wa_alv_selection-docnum
              ).

          ENDLOOP.

        CATCH zcx_faturamento INTO DATA(ex_faturamento).
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              ex_faturamento->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
              EXIT.
            WHEN abap_true.
              MESSAGE ID ex_faturamento->msgid TYPE 'S' NUMBER ex_faturamento->msgno WITH ex_faturamento->msgv1 ex_faturamento->msgv2 ex_faturamento->msgv3 ex_faturamento->msgv4 INTO DATA(l_mesg).
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
              EXIT.
          ENDCASE.
*-#133089-21.02.2024-JT-fim

        CATCH zcx_error INTO DATA(ex_error).
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
              EXIT.
            WHEN abap_true.
              MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
              EXIT.
          ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDTRY.

*-#133089-21.02.2024-JT-inicio
      IF vg_faturamento_autom = abap_true.
        PERFORM f_fatura_autom_mdfe.
      ELSE.
        CALL SCREEN 0107 STARTING AT 02 02 ENDING AT 156 27.
      ENDIF.
*-#133089-21.02.2024-JT-fim

  ENDCASE.


ENDFORM.                    " MANIFESTO_ELETRONICO
*&---------------------------------------------------------------------*
*&      Form  CHANGE_ROWS
*&---------------------------------------------------------------------*

FORM change_rows .

  LOOP AT it_cte_corr INTO wa_cte_corr.

    IF wa_cte_corr-grupo IS INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR  : ls_edit, wa_cte_corr-field_style.
    REFRESH: lt_edit.

    ls_edit-fieldname = 'PC_VEICULO'.
    IF ( ( wa_cte_corr-grupo EQ 'veic' ) OR
         ( wa_cte_corr-grupo EQ 'moto' ) OR
         ( wa_cte_corr-grupo EQ 'infQ' ) ).
      ls_edit-style  = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_edit-style  = cl_gui_alv_grid=>mc_style_disabled.
      CLEAR wa_cte_corr-pc_veiculo.
    ENDIF.
    ls_edit-style2 = space.
    ls_edit-style3 = space.
    ls_edit-style4 = space.
    ls_edit-maxlen = 12.
    INSERT ls_edit INTO TABLE lt_edit.

    ls_edit-fieldname = 'CHAVE'.
    IF ( wa_cte_corr-grupo EQ 'infNFe' OR
         wa_cte_corr-grupo EQ 'infUnidTransp' ) .
      ls_edit-style  = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_edit-style  = cl_gui_alv_grid=>mc_style_disabled.
      CLEAR wa_cte_corr-chave.
    ENDIF.
    ls_edit-style2 = space.
    ls_edit-style3 = space.
    ls_edit-style4 = space.
    ls_edit-maxlen = 48.
    INSERT ls_edit INTO TABLE lt_edit.

    INSERT LINES OF lt_edit INTO TABLE  wa_cte_corr-field_style.

    MODIFY it_cte_corr FROM  wa_cte_corr TRANSPORTING field_style chave pc_veiculo.

  ENDLOOP.

  CALL METHOD ctl_alv_cte_corr->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.                    " CHANGE_ROWS
*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_DISP_TRANSF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verificar_disp_transf CHANGING c_disponivel.

  DATA: wa_zsdt_retlote   TYPE zsdt_retlote.

  DATA: vl_doc_msg    TYPE string,
        vl_msg_exibir TYPE string.

  CLEAR: c_disponivel.

*  SELECT SINGLE *
*    INTO WA_ZSDT_RETLOTE
*    FROM ZSDT_RETLOTE
*   WHERE DOCNUM = WA_ALV_SELECTION-DOCNUM.

*  IF SY-SUBRC = 0.
*    VL_DOC_MSG = WA_ZSDT_RETLOTE-DOCNUM_RET.
*    CONCATENATE 'Nota já está vinculada a um Retorno de formação de lote:' VL_DOC_MSG '!' INTO VL_MSG_EXIBIR.
*    MESSAGE VL_MSG_EXIBIR TYPE 'W'.
*  ELSE.
  c_disponivel  = 'X'.
*    VL_MSG_EXIBIR = 'Nota disponível para transferência'.
*    MESSAGE VL_MSG_EXIBIR TYPE 'S'.
*  ENDIF.

ENDFORM.                    " VERIFICAR_DISP_TRANSF
*&---------------------------------------------------------------------*
*&      Form  LOAD_PARC_CCE_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_parc_cce_nf .

  DATA: values  TYPE vrm_values WITH HEADER LINE.

  REFRESH: it_corr_parc.
  CLEAR: it_corr_parc, wg_corr_parc.

  REFRESH: values.
  CLEAR: values.

  values-key  = 'PC - Ponto de Coleta'.
  APPEND values.

  values-key  = 'LR - Local de Entrega'.
  APPEND values.

  values-key  = 'Z1 - Terminal- Porto'.
  APPEND values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'WG_CORR_PARC-PARVW'
      values          = values[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    "MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    "        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DADOS_PARC_NF_CCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dados_parc_nf_cce .

  DATA: BEGIN OF tg_nf_lin OCCURS 0,
          refkey TYPE j_1bnflin-refkey,
          refitm TYPE j_1bnflin-refitm,
          reftyp TYPE j_1bnflin-reftyp,
          vbeln  TYPE c LENGTH 10,
        END OF tg_nf_lin.

  DATA: wl_parid_z1 TYPE j_1bparid,
        wl_parid_pc TYPE j_1bparid,
        wl_parid_lr TYPE j_1bparid.

  DATA: wa_info_c       TYPE kna1,
        wa_info_k       TYPE lfa1,
        vl_old_parid_nf TYPE j_1bnfnad-parid,
        wl_j1bnfnad     TYPE j_1bnfnad.

  DATA: BEGIN OF wl_remessa_itm,
          ch_referencia LIKE zsdt0001-ch_referencia,
          doc_rem       LIKE zsdt0001-doc_rem,
        END OF wl_remessa_itm.

  CLEAR: vl_old_parid_nf.

  SELECT SINGLE *
    INTO wl_j1bnfnad
    FROM j_1bnfnad
   WHERE docnum = wa_alv_selection-docnum
     AND parvw  = wg_corr_parc-parvw(2).

  IF sy-subrc = 0.
    vl_old_parid_nf = wl_j1bnfnad-parid.
  ENDIF.

  IF vl_old_parid_nf IS INITIAL.

    REFRESH: tg_nf_lin.

    SELECT refkey refitm reftyp
      INTO CORRESPONDING FIELDS OF TABLE tg_nf_lin
      FROM j_1bnflin
     WHERE docnum = wa_alv_selection-docnum.

    LOOP AT tg_nf_lin.

      CLEAR wl_remessa_itm.

      tg_nf_lin-vbeln = tg_nf_lin-refkey(10).

      CASE tg_nf_lin-reftyp.
        WHEN 'BI'.
          SELECT SINGLE re~ch_referencia l~vbeln
            INTO wl_remessa_itm
            FROM vbfa AS v
           INNER JOIN lips AS l ON l~vbeln = v~vbelv
           INNER JOIN zsdt0001 AS re ON re~doc_rem = l~vbeln
           WHERE v~vbeln         EQ tg_nf_lin-vbeln
             AND re~tp_movimento EQ 'S'
             AND v~vbtyp_n       EQ 'M'
             AND v~vbtyp_v       EQ 'J'. "Entrega via Faturamento
        WHEN 'MD'.
          SELECT SINGLE re~ch_referencia l~vbeln
            INTO wl_remessa_itm
            FROM vbfa AS v
           INNER JOIN lips AS l ON l~vbeln = v~vbelv
           INNER JOIN zsdt0001 AS re ON re~doc_rem = l~vbeln
           WHERE v~vbeln         EQ tg_nf_lin-vbeln
             AND re~tp_movimento EQ 'S'
             AND v~vbtyp_n       EQ 'R'
             AND v~vbtyp_v       EQ 'J'.  "Entrega via documento de material
      ENDCASE.

      IF ( sy-subrc = 0 ) AND ( wl_remessa_itm-doc_rem IS NOT INITIAL ).

        CLEAR: wl_parid_pc, wl_parid_lr, wl_parid_z1.

        CALL FUNCTION 'Z_LES_TIPO_REMESSA'
          EXPORTING
            p_vbeln    = wl_remessa_itm-doc_rem
          CHANGING
            p_parid_pc = wl_parid_pc  "Ponto de Coleta
            p_parid_lr = wl_parid_lr  "Local de Entrega
            p_parid_z1 = wl_parid_z1. "Terminal Porto

        CASE wg_corr_parc-parvw(2).
          WHEN 'LR'.
            IF wl_parid_lr IS NOT INITIAL.
              vl_old_parid_nf  = wl_parid_lr.
              EXIT.
            ENDIF.
          WHEN 'PC'.
            IF wl_parid_pc IS NOT INITIAL.
              vl_old_parid_nf  = wl_parid_pc.
              EXIT.
            ENDIF.
          WHEN 'Z1'.
            IF wl_parid_z1 IS NOT INITIAL.
              vl_old_parid_nf  = wl_parid_z1.
              EXIT.
            ENDIF.
        ENDCASE.

      ENDIF.

    ENDLOOP.

  ENDIF.

  CLEAR: wg_corr_parc-new_parid.
  IF wg_corr_parc-new_parid_c IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_corr_parc-new_parid_c
      IMPORTING
        output = wg_corr_parc-new_parid_c.

    wg_corr_parc-new_parid = wg_corr_parc-new_parid_c.

  ELSEIF wg_corr_parc-new_parid_v IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_corr_parc-new_parid_v
      IMPORTING
        output = wg_corr_parc-new_parid_v.

    wg_corr_parc-new_parid = wg_corr_parc-new_parid_v.
  ENDIF.

  CASE wg_corr_parc-parvw(2).
    WHEN 'LR'.

      wg_corr_parc-partxt = 'Local de Entrega'.

      IF vl_old_parid_nf IS NOT INITIAL.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            p_parceiro   = vl_old_parid_nf
            p_partype    = 'C'
          CHANGING
            wa_info_part = wa_info_k
            wa_info_c    = wa_info_c.

        wg_corr_parc-old_parid =  vl_old_parid_nf.

        IF wa_info_c-stcd1 IS NOT INITIAL.
          wg_corr_parc-old_tipo      =  'J'.
          wg_corr_parc-old_cnpj_cpf  =  wa_info_c-stcd1.
        ELSEIF wa_info_c-stcd2 IS NOT INITIAL.
          wg_corr_parc-old_tipo      =  'F'.
          wg_corr_parc-old_cnpj_cpf  =  wa_info_c-stcd2.
        ENDIF.

        wg_corr_parc-old_ie    =  wa_info_c-stcd3.
        wg_corr_parc-old_rz    =  wa_info_c-name1.

      ENDIF.

      IF wg_corr_parc-new_parid IS NOT INITIAL.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            p_parceiro   = wg_corr_parc-new_parid
            p_partype    = 'C'
          CHANGING
            wa_info_part = wa_info_k
            wa_info_c    = wa_info_c.

        IF wa_info_c-stcd1 IS NOT INITIAL.
          wg_corr_parc-new_tipo      =  'J'.
          wg_corr_parc-new_cnpj_cpf  =  wa_info_c-stcd1.
        ELSEIF wa_info_c-stcd2 IS NOT INITIAL.
          wg_corr_parc-new_tipo      =  'F'.
          wg_corr_parc-new_cnpj_cpf  =  wa_info_c-stcd2.
        ENDIF.

        wg_corr_parc-new_ie    =  wa_info_c-stcd3.
        wg_corr_parc-new_rz    =  wa_info_c-name1.

      ENDIF.

    WHEN 'PC' OR 'Z1'.

      IF wg_corr_parc-parvw(2) = 'PC'.
        wg_corr_parc-partxt = 'Ponto de Coleta'.
      ELSEIF wg_corr_parc-parvw(2) = 'Z1'.
        wg_corr_parc-partxt = 'Terminal - Porto'.
      ENDIF.

      IF vl_old_parid_nf IS NOT INITIAL.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            p_parceiro   = vl_old_parid_nf
            p_partype    = 'V'
          CHANGING
            wa_info_part = wa_info_k
            wa_info_c    = wa_info_c.

        wg_corr_parc-old_parid =  vl_old_parid_nf.

        IF wa_info_k-stcd1 IS NOT INITIAL.
          wg_corr_parc-old_tipo      =  'J'.
          wg_corr_parc-old_cnpj_cpf  =  wa_info_k-stcd1.
        ELSEIF wa_info_k-stcd2 IS NOT INITIAL.
          wg_corr_parc-old_tipo      =  'F'.
          wg_corr_parc-old_cnpj_cpf  =  wa_info_k-stcd2.
        ENDIF.

        wg_corr_parc-old_ie    =  wa_info_k-stcd3.
        wg_corr_parc-old_rz    =  wa_info_k-name1.

      ENDIF.

      IF wg_corr_parc-new_parid IS NOT INITIAL.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            p_parceiro   = wg_corr_parc-new_parid
            p_partype    = 'V'
          CHANGING
            wa_info_part = wa_info_k
            wa_info_c    = wa_info_c.

        IF wa_info_k-stcd1 IS NOT INITIAL.
          wg_corr_parc-new_tipo      =  'J'.
          wg_corr_parc-new_cnpj_cpf  =  wa_info_k-stcd1.
        ELSEIF wa_info_k-stcd2 IS NOT INITIAL.
          wg_corr_parc-new_tipo      =  'F'.
          wg_corr_parc-new_cnpj_cpf  =  wa_info_k-stcd2.
        ENDIF.

        wg_corr_parc-new_ie    =  wa_info_k-stcd3.
        wg_corr_parc-new_rz    =  wa_info_k-name1.

      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_CORR_PARC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_corr_parc .

  PERFORM get_dados_parc_nf_cce.

  CASE wg_corr_parc-parvw(2).
    WHEN 'PC' OR 'LR' OR 'Z1'.

      READ TABLE it_corr_parc WITH KEY parvw = wg_corr_parc-parvw(2).
      IF sy-subrc = 0.
        MESSAGE 'Função Parceiro NF já incluída! Deve ser removida para nova inclusão!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF wg_corr_parc-old_parid IS INITIAL.
        MESSAGE 'Parceiro atual não Encontrado!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF wg_corr_parc-old_cnpj_cpf IS INITIAL.
        MESSAGE 'CNPJ/CPF Parceiro atual não Encontrado!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF wg_corr_parc-old_rz IS INITIAL.
        MESSAGE 'Razão Social Parceiro atual não Encontrada!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF wg_corr_parc-new_parid IS INITIAL.
        MESSAGE 'Novo Parceiro não informado!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF wg_corr_parc-new_rz IS INITIAL.
        MESSAGE 'Razão Social do novo Parceiro não encontrado!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF wg_corr_parc-new_cnpj_cpf IS INITIAL.
        MESSAGE 'CNPJ/CPF do novo Parceiro não encontrado!' TYPE 'I'.
        EXIT.
      ENDIF.

    WHEN OTHERS.
      MESSAGE 'Função Parceiro NF é um campo obrigatório!' TYPE 'I'.
      EXIT.
  ENDCASE.

  APPEND wg_corr_parc TO it_corr_parc.

  MESSAGE 'Correção adicionada com sucesso!' TYPE 'S'.
  CLEAR wg_corr_parc.
  EXIT.

ENDFORM.

FORM del_corr_parc .

  CASE wg_corr_parc-parvw(2).
    WHEN 'PC' OR 'LR' OR 'Z1'.

      LOOP AT it_corr_parc WHERE parvw = wg_corr_parc-parvw(2).
        DELETE it_corr_parc.
        MESSAGE 'Correção Parceiro Eliminada!' TYPE 'I'.
      ENDLOOP.

    WHEN OTHERS.
      MESSAGE 'Função Parceiro NF é um campo obrigatório!' TYPE 'W'.
      EXIT.
  ENDCASE.

ENDFORM.

FORM atualiza_txt_corr_parc.

  TYPES: BEGIN OF ty_editor_parc,
           line(200),
         END   OF ty_editor_parc.

  DATA: tg_edt_corr_parc TYPE TABLE OF ty_editor_parc,
        wg_edt_corr_parc TYPE ty_editor_parc.

  DATA: vl_txt_cc_par     TYPE string,
        vl_txt_cc_par_aux TYPE string,
        vl_txt_cnpj_cpf   TYPE string.

  CALL METHOD obg_descbox->set_readonly_mode
    EXPORTING
      readonly_mode = 0.

  IF it_corr_parc[] IS NOT INITIAL.

    CALL METHOD obg_descbox->set_readonly_mode
      EXPORTING
        readonly_mode = 1.

    LOOP AT it_corr_parc.

      CLEAR: vl_txt_cc_par_aux.

      IF it_corr_parc-new_tipo = 'F'.
        vl_txt_cnpj_cpf = 'CPF:'.
      ELSE.
        vl_txt_cnpj_cpf = 'CNPJ:'.
      ENDIF.

      IF it_corr_parc-new_ie IS NOT INITIAL.

        CONCATENATE 'Considerar dados corretos do' it_corr_parc-partxt ':'
                    vl_txt_cnpj_cpf it_corr_parc-new_cnpj_cpf
                    'Razão Social:' it_corr_parc-new_rz
                    'IE:' it_corr_parc-new_ie
        INTO vl_txt_cc_par_aux SEPARATED BY space.

      ELSE.
        CONCATENATE 'Considerar dados corretos do' it_corr_parc-partxt ':'
                    vl_txt_cnpj_cpf it_corr_parc-new_cnpj_cpf
                    'Razão Social:' it_corr_parc-new_rz
        INTO vl_txt_cc_par_aux SEPARATED BY space.

      ENDIF.

      it_corr_parc-txt_corr = vl_txt_cc_par_aux.

      MODIFY it_corr_parc.

      CLEAR wg_edt_corr_parc.
      wg_edt_corr_parc-line = it_corr_parc-txt_corr.
      APPEND wg_edt_corr_parc TO tg_edt_corr_parc.

      wg_edt_corr_parc-line = ' '.
      APPEND wg_edt_corr_parc TO tg_edt_corr_parc.

    ENDLOOP.

  ENDIF.

  CALL METHOD obg_descbox->set_text_as_r3table
    EXPORTING
      table = tg_edt_corr_parc.

ENDFORM.

*-#133089-21.02.2024-JT-inicio
**********************************************************************
* faturamento automatico - gerar MDFE
**********************************************************************
FORM f_fatura_autom_mdfe.

  PERFORM: novo_mdfe,
           gravar_mdfe,
           emitir_mdfe.

ENDFORM.
*-#133089-21.02.2024-JT-fim

**********************************************************************
**********************************************************************
