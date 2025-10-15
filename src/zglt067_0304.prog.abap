*----------------------------------------------------------------------*
***INCLUDE ZGLT067_0304 .
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_zglt032.
         INCLUDE TYPE zglt032.
TYPES:   shkzg TYPE shkzg,
         koart TYPE koart,
         xbilk TYPE xbilk.
TYPES: END OF ty_zglt032.

TYPES: BEGIN OF  ty_0306.
TYPES: lc_name   TYPE name1,
       lc_akont  TYPE akont,
       lc_txt50a TYPE txt50_skat,
       lc_skont  TYPE skont,
       lc_txt50s TYPE txt50_skat,
       lc_ktopl  TYPE ktopl.
TYPES: END OF ty_0306.

DATA: wa_stable_0304       TYPE lvc_s_stbl,
      lc_celula_0304       TYPE lvc_s_modi,
*{   DELETE         DEVK9A1UKD                                        6
*\      it_mov_lct_banco TYPE TABLE OF zde_mov_lct_banco WITH HEADER LINE,
*}   DELETE
*{   INSERT         DEVK9A1UKD                                        5
*
      it_mov_lct_banco     TYPE TABLE OF ty_zde_mov_lct_banco WITH HEADER LINE,
      it_mov_lct_banco_aux TYPE TABLE OF zde_mov_lct_banco WITH HEADER LINE,
*}   INSERT
      it_zglt036_comp      TYPE TABLE OF zglt036.

DATA: wa_aux  TYPE zsaldo_cta_moeda,
      wa_0306 TYPE ty_0306.

DATA: ctl_alv_0304 TYPE REF TO cl_gui_alv_grid,
      it_zglt031   TYPE TABLE OF zglt031 WITH HEADER LINE,
      it_zglt032   TYPE TABLE OF ty_zglt032 WITH HEADER LINE,
      it_cta_banc  TYPE TABLE OF zsaldo_cta_moeda WITH HEADER LINE,
      it_ska1      TYPE TABLE OF ska1 WITH HEADER LINE,
      it_tbsl      TYPE TABLE OF tbsl WITH HEADER LINE.

DATA: ck_deb_cred    TYPE c LENGTH 1,
      ck_conta_banco TYPE c LENGTH 1,
      gb_tipo_valor  TYPE c LENGTH 1.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver_0304 DEFINITION.
  PUBLIC SECTION.
    DATA: validar_data  TYPE c,
          error_in_data TYPE c,
          ls_good       TYPE lvc_s_modi,
          lv_value      TYPE lvc_value,
          wa_tbsl       TYPE tbsl.

*    METHODS: ON_F4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID IMPORTING E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS E_DISPLAY.
*{   INSERT         DEVK9A1UKD                                        1
*
    TYPES: ddshretval_tabl TYPE TABLE OF ddshretval.

    METHODS: on_f4 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.
*}   INSERT

    METHODS: data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

    METHODS: data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.

    METHODS: subtotal_text FOR EVENT subtotal_text OF cl_gui_alv_grid IMPORTING es_subtottxt_info ep_subtot_line e_event_data.
*{   INSERT         DEVK9A1UKD                                        4

    METHODS: my_f4
      IMPORTING et_bad_cells  TYPE lvc_t_modi
                es_row_no     TYPE lvc_s_roid
                er_event_data TYPE REF TO cl_alv_event_data
                e_display     TYPE c
                e_fieldname   TYPE lvc_fname
      EXPORTING lt_f4         TYPE ddshretval_tabl.
*
*}   INSERT

  PRIVATE SECTION.

    TYPES: ddshretval_table TYPE TABLE OF ddshretval.

    METHODS: informar_parceiro
      IMPORTING
        i_zglt032       TYPE ty_zglt032
        i_zglt032_b     TYPE ty_zglt032
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS: perform_semantic_checks
      IMPORTING
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS: atualiza_transferencia
      IMPORTING
        i_ck_chave_deb_cred TYPE char01
        pr_data_changed     TYPE REF TO cl_alv_changed_data_protocol.

*    METHODS: MY_F4
*          IMPORTING ET_BAD_CELLS   TYPE LVC_T_MODI
*                    ES_ROW_NO      TYPE LVC_S_ROID
*                    ER_EVENT_DATA  TYPE REF TO CL_ALV_EVENT_DATA
*                    E_DISPLAY      TYPE C
*                    E_FIELDNAME    TYPE LVC_FNAME
*          EXPORTING LT_F4          TYPE DDSHRETVAL_TABLE.

ENDCLASS.                    "lcl_event_receiver DEFINITION

DATA: event_handler_0304 TYPE REF TO lcl_event_receiver_0304.
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver_0304 IMPLEMENTATION.

*{   REPLACE        DEVK9A1UKD                                        2
*\*  METHOD ON_F4.
*\*
*\*    DATA: LS_F4 TYPE DDSHRETVAL,
*\*          LT_F4 TYPE TABLE OF DDSHRETVAL.
*\*
*\*    CASE E_FIELDNAME.
*\*      WHEN 'TP_LCTO'.
*\*
*\*        CALL METHOD MY_F4
*\*          EXPORTING
*\*            ES_ROW_NO     = ES_ROW_NO
*\*            ER_EVENT_DATA = ER_EVENT_DATA
*\*            ET_BAD_CELLS  = ET_BAD_CELLS
*\*            E_DISPLAY     = E_DISPLAY
*\*            E_FIELDNAME   = E_FIELDNAME
*\*          IMPORTING
*\*            LT_F4         = LT_F4.
*\*
*\*    ENDCASE.
*\*
*\*  ENDMETHOD.                    "ON_F4
*  METHOD ON_F4.

  METHOD on_f4.

    DATA: ls_f4 TYPE ddshretval,
          lt_f4 TYPE TABLE OF ddshretval.

    CASE e_fieldname.
      WHEN 'TP_LCTO'.

        CALL METHOD my_f4
          EXPORTING
            es_row_no     = es_row_no
            er_event_data = er_event_data
            et_bad_cells  = et_bad_cells
            e_display     = e_display
            e_fieldname   = e_fieldname
          IMPORTING
            lt_f4         = lt_f4.

    ENDCASE.

  ENDMETHOD.                    "ON_F4



*
*    DATA: LS_F4 TYPE DDSHRETVAL,
*          LT_F4 TYPE TABLE OF DDSHRETVAL.
*
*    CASE E_FIELDNAME.
*      WHEN 'TP_LCTO'.
*
*        CALL METHOD MY_F4
*          EXPORTING
*            ES_ROW_NO     = ES_ROW_NO
*            ER_EVENT_DATA = ER_EVENT_DATA
*            ET_BAD_CELLS  = ET_BAD_CELLS
*            E_DISPLAY     = E_DISPLAY
*            E_FIELDNAME   = E_FIELDNAME
*          IMPORTING
*            LT_F4         = LT_F4.
*
*    ENDCASE.
*
*  ENDMETHOD.                    "ON_F4
*}   REPLACE
*{   INSERT         DEVK9A1UKD                                        3

  METHOD my_f4.

    DATA: wa_tab      LIKE LINE OF it_mov_lct_banco,
          lt_fcat     TYPE lvc_t_fcat,
          ls_fieldcat TYPE lvc_s_fcat,
          l_tabname   TYPE dd03v-tabname,
          l_fieldname TYPE dd03v-fieldname,
          l_help_valu TYPE help_info-fldvalue,
          lt_bad_cell TYPE lvc_t_modi,
          lp_wa       TYPE REF TO data.

    IF e_fieldname EQ 'TP_LCTO'.

      SET PARAMETER ID: 'ZBANK_SAKNR' FIELD zde_saldo_cta_banco-saknr,
                        'BUK'         FIELD zde_saldo_cta_banco-bukrs.

      CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
        EXPORTING
          tabname           = 'ZGLT031'
          fieldname         = 'TP_LCTO'
        TABLES
          return_tab        = lt_f4
        EXCEPTIONS
          field_not_found   = 1
          no_help_for_field = 2
          inconsistent_help = 3
          no_values_found   = 4
          OTHERS            = 5.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "my_f4
*
*}   INSERT

*  METHOD MY_F4.
*
*    DATA: WA_TAB      LIKE LINE OF IT_MOV_LCT_BANCO,
*          LT_FCAT     TYPE LVC_T_FCAT,
*          LS_FIELDCAT TYPE LVC_S_FCAT,
*          L_TABNAME   TYPE DD03V-TABNAME,
*          L_FIELDNAME TYPE DD03V-FIELDNAME,
*          L_HELP_VALU TYPE HELP_INFO-FLDVALUE,
*          LT_BAD_CELL TYPE LVC_T_MODI,
*          LP_WA       TYPE REF TO DATA.
*
*    IF E_FIELDNAME EQ 'TP_LCTO'.
*
*      SET PARAMETER ID: 'ZBANK_SAKNR' FIELD ZDE_SALDO_CTA_BANCO-SAKNR,
*                        'BUK'         FIELD ZDE_SALDO_CTA_BANCO-BUKRS.
*
*      CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*        EXPORTING
*          TABNAME           = 'ZGLT031'
*          FIELDNAME         = 'TP_LCTO'
*        TABLES
*          RETURN_TAB        = LT_F4
*        EXCEPTIONS
*          FIELD_NOT_FOUND   = 1
*          NO_HELP_FOR_FIELD = 2
*          INCONSISTENT_HELP = 3
*          NO_VALUES_FOUND   = 4
*          OTHERS            = 5.
*
*      IF SY-SUBRC IS NOT INITIAL.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*    ENDIF.
*  ENDMETHOD.                    "my_f4

  METHOD informar_parceiro.

    DATA: wa_zglt031 TYPE zglt031.

    CLEAR: lv_value.
    CLEAR: zde_mov_lct_banco.

    READ TABLE it_zglt031 INTO wa_zglt031 WITH KEY tp_lcto = i_zglt032-tp_lcto.
    IF sy-subrc IS INITIAL.
      zde_mov_lct_banco-descricao = wa_zglt031-descricao.
    ENDIF.
    zde_mov_lct_banco-umskz       = i_zglt032-umskz.
    zde_mov_lct_banco-koart       = i_zglt032-koart.
    zde_mov_lct_banco-bukrs       = zde_saldo_cta_banco-bukrs.
    zde_mov_lct_banco-bschl       = i_zglt032-bschl.
    zde_mov_lct_banco-bschl_banco = i_zglt032_b-bschl.
    IF ( kna1-kunnr IS NOT INITIAL ) OR ( lfa1-lifnr IS NOT INITIAL ).
      IF kna1-kunnr IS NOT INITIAL.
        zde_mov_fornecedor-parid = kna1-kunnr.
      ELSE.
        zde_mov_fornecedor-parid = lfa1-lifnr.
      ENDIF.
      zde_mov_lct_banco-saknr = zde_mov_fornecedor-parid.
      zde_mov_lct_banco-parid = zde_mov_fornecedor-parid.
    ENDIF.
    CALL SCREEN 0306 STARTING AT 15 01.

    IF zde_mov_lct_banco IS NOT INITIAL.

      MOVE zde_mov_lct_banco-saknr TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'SAKNR'
          i_value     = lv_value.

      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'PARID'
          i_value     = lv_value.

      MOVE zde_mov_lct_banco-skont TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'SKONT'
          i_value     = lv_value.

      MOVE zde_mov_lct_banco-akont TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'AKONT'
          i_value     = lv_value.

      MOVE zde_mov_lct_banco-koart TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'KOART'
          i_value     = lv_value.

      MOVE zde_mov_lct_banco-umskz TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'UMSKZ'
          i_value     = lv_value.

      MOVE zde_mov_lct_banco-skont TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'SKONT'
          i_value     = lv_value.

      MOVE zde_mov_lct_banco-bschl TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'BSCHL'
          i_value     = lv_value.

      MOVE zde_mov_lct_banco-bschl_banco TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'BSCHL_BANCO'
          i_value     = lv_value.
    ENDIF.

  ENDMETHOD.                    "INFORMAR_PARCEIRO

  METHOD atualiza_transferencia.

    DATA: wa_tbsl     TYPE tbsl,
          wa_zglt032  TYPE zglt032,
          tp_deb_cred TYPE shkzg.

    MOVE zde_mov_lct_banco-saknr TO lv_value.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'SAKNR'
        i_value     = lv_value.

    MOVE zde_mov_lct_banco-tcurr TO lv_value.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'TCURR'
        i_value     = lv_value.

    MOVE zde_mov_lct_banco-ukurs TO lv_value.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'UKURS'
        i_value     = lv_value.

    IF zde_mov_lct_banco-tcurr EQ zde_mov_lct_banco-fcurr.
      zde_mov_lct_banco-dmbe2 = zde_mov_lct_banco-dmbtr.
    ENDIF.

    MOVE zde_mov_lct_banco-dmbe2 TO lv_value.
    CONDENSE lv_value NO-GAPS.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'DMBE2'
        i_value     = lv_value.

    LOOP AT it_tbsl INTO wa_tbsl WHERE shkzg EQ i_ck_chave_deb_cred.
      READ TABLE it_zglt032 INTO wa_zglt032
      WITH KEY tp_lcto = zde_mov_lct_banco-tp_lcto
               hkont   = zde_mov_lct_banco-saknr
               bschl   = wa_tbsl-bschl.
      IF sy-subrc IS INITIAL.
        MOVE wa_zglt032-bschl TO lv_value.
        CALL METHOD pr_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'BSCHL'
            i_value     = lv_value.
      ENDIF.
    ENDLOOP.

    CASE i_ck_chave_deb_cred.
      WHEN 'H'.
        tp_deb_cred = 'S'.
      WHEN 'S'.
        tp_deb_cred = 'H'.
    ENDCASE.

    LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto EQ zde_mov_lct_banco-tp_lcto
                                         AND hkont   EQ zde_saldo_cta_banco-saknr
                                         AND shkzg   NE i_ck_chave_deb_cred.
      "(2) - Localizar chave de lançamento do Tipo de Lançamento para a conta bancária
      MOVE wa_zglt032-bschl TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'BSCHL_BANCO'
          i_value     = lv_value.
    ENDLOOP.

    "Variação Cambial """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    MOVE zde_mov_lct_banco-saknr_variacao TO lv_value.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'SAKNR_VARIACAO'
        i_value     = lv_value.

    MOVE zde_mov_lct_banco-ukurs_variacao TO lv_value.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'UKURS_VARIACAO'
        i_value     = lv_value.

    MOVE zde_mov_lct_banco-dmbe2_variacao TO lv_value.
    CONDENSE lv_value NO-GAPS.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'DMBE2_VARIACAO'
        i_value     = lv_value.

    MOVE zde_mov_lct_banco-dmbe2_variacaoc TO lv_value.
    CONDENSE lv_value NO-GAPS.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'DMBE2_VARIACAOC'
        i_value     = lv_value.

    MOVE zde_mov_lct_banco-operador_cotacao TO lv_value.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'OPERADOR_COTACAO'
        i_value     = lv_value.

    "Chave do Banco
    IF zde_mov_lct_banco-dmbe2_variacao IS NOT INITIAL.
      IF zde_mov_lct_banco-dmbe2_variacao GT 0.
        LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto EQ zde_mov_lct_banco-tp_lcto
                                             AND hkont   EQ zde_mov_lct_banco-saknr_variacao
                                             AND shkzg   EQ tp_deb_cred.
          zde_mov_lct_banco-bschl_variacaob = wa_zglt032-bschl.
        ENDLOOP.
      ELSE.
        LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto EQ zde_mov_lct_banco-tp_lcto
                                             AND hkont   EQ zde_mov_lct_banco-saknr_variacao
                                             AND shkzg   NE tp_deb_cred.
          zde_mov_lct_banco-bschl_variacaob = wa_zglt032-bschl.
        ENDLOOP.
      ENDIF.
    ELSE.
      CLEAR: zde_mov_lct_banco-bschl_variacaob.
    ENDIF.

    MOVE zde_mov_lct_banco-bschl_variacaob TO lv_value.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'BSCHL_VARIACAOB'
        i_value     = lv_value.

    MOVE zde_mov_lct_banco-bschl_variacao TO lv_value.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_good-row_id
        i_fieldname = 'BSCHL_VARIACAO'
        i_value     = lv_value.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  ENDMETHOD.                    "ATUALIZA_TRANSFERENCIA

  METHOD subtotal_text.
*{   REPLACE        DEVK9A1UKD                                        7
*\    DATA: wa_mov_lct_banco2 TYPE zde_mov_lct_banco,
*\          wa_mov_lct_banco  TYPE zde_mov_lct_banco.
    DATA: wa_mov_lct_banco2 TYPE ty_zde_mov_lct_banco,
          wa_mov_lct_banco  TYPE ty_zde_mov_lct_banco.
*}   REPLACE

    FIELD-SYMBOLS: <fs>  TYPE any.
    FIELD-SYMBOLS: <fs2> TYPE any.

    ASSIGN e_event_data->m_data->* TO <fs>.
    IF sy-subrc EQ 0.

      IF es_subtottxt_info(07) EQ 'TP_LCTO'.

        ASSIGN ep_subtot_line->* TO <fs2>.
        wa_mov_lct_banco2 = <fs2>.

        READ TABLE it_mov_lct_banco INTO wa_mov_lct_banco WITH KEY tp_lcto = wa_mov_lct_banco2-tp_lcto.
        <fs> = wa_mov_lct_banco-descricao.

        wa_mov_lct_banco2-dmbtr = 0.
        wa_mov_lct_banco2-dmbe2 = 0.
        LOOP AT it_mov_lct_banco INTO wa_mov_lct_banco WHERE tp_lcto = wa_mov_lct_banco2-tp_lcto.
          wa_mov_lct_banco2-dmbtr = wa_mov_lct_banco2-dmbtr + wa_mov_lct_banco-dmbtr.
          wa_mov_lct_banco2-dmbe2 = wa_mov_lct_banco2-dmbe2 + wa_mov_lct_banco-dmbe2.
        ENDLOOP.

        wa_mov_lct_banco = <fs2>.
        wa_mov_lct_banco-dmbtr = wa_mov_lct_banco2-dmbtr.
        wa_mov_lct_banco-dmbe2 = wa_mov_lct_banco2-dmbe2.
        <fs2> = wa_mov_lct_banco.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "subtotal_text

  "BCALV_EDIT_03
  METHOD perform_semantic_checks.

    FIELD-SYMBOLS: <fs_cell> TYPE lvc_s_modi.

    DATA: vl_value              TYPE lvc_value,
          wl_valor              TYPE zde_payments,
          vg_tabix              TYPE sy-tabix,
*{   REPLACE        DEVK9A1UKD                                        8
*\          wa_mov_lct_banco      TYPE zde_mov_lct_banco,
          wa_mov_lct_banco      TYPE ty_zde_mov_lct_banco,
*}   REPLACE
          wa_zglt031            TYPE zglt031,
          wa_cta_banc           TYPE zsaldo_cta_moeda,
          wa_zglt032            TYPE ty_zglt032,
          wa_zglt032_b          TYPE ty_zglt032,
          ck_possui_conta_banco TYPE c LENGTH 1,
          ck_somente_cta_banco  TYPE c LENGTH 1,
          ck_possui_cta_cli_for TYPE c LENGTH 1,
          ck_chave_deb_cred     TYPE c LENGTH 1, "Chave de Débito e Crédito do Lançamento
          ck_chave_deb_cred_b   TYPE c LENGTH 1, "Chave de Débito e Crédito da Conta Bancária
          lc_val_text           TYPE val_text,
          lc_bschl_banco        TYPE bschl,
          lc_conta_variacao     TYPE saknr.

    LOOP AT pr_data_changed->mt_good_cells INTO ls_good WHERE fieldname EQ 'TP_LCTO'.
      vg_tabix = sy-tabix.
      lv_value = ls_good-value.

      IF ls_good-value IS INITIAL.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_value
        IMPORTING
          output = wa_zglt031-tp_lcto.

      IF wa_zglt031-tp_lcto NE '0000000000'.
        READ TABLE it_zglt031 INTO wa_zglt031 WITH KEY tp_lcto = wa_zglt031-tp_lcto.
        IF sy-subrc IS NOT INITIAL.
          error_in_data = abap_true.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = lv_value
            IMPORTING
              output = lv_value.

          CALL METHOD pr_data_changed->add_protocol_entry
            EXPORTING
              i_msgid     = 'ZFI'
              i_msgno     = '041'
              i_msgty     = 'E'
              i_msgv1     = lv_value
              i_fieldname = ls_good-fieldname
              i_row_id    = ls_good-row_id.
        ELSE.
          "Somente valida conta de banco se o lançamento já estiver com valor (debito/crédito).
          READ TABLE it_mov_lct_banco INTO wa_mov_lct_banco INDEX ls_good-row_id.
          IF wa_mov_lct_banco-dmbtr NE 0.

            "Verifica Chave de Lançamento no Banco
            "H  Crédito
            "S  Débito
            IF wa_mov_lct_banco-dmbtr GT 0. "Crédito na Conta Banco (Contra Partida)
              ck_chave_deb_cred   = 'H'.
              ck_chave_deb_cred_b = 'S'.
            ELSEIF wa_mov_lct_banco-dmbtr LT 0. "Débito na Conta Banco (Contra Partida)
              ck_chave_deb_cred   = 'S'.
              ck_chave_deb_cred_b = 'H'.
            ENDIF.

            "As Contas de Contrapartida possui conta de banco
            ck_possui_conta_banco = abap_false.
            LOOP AT it_cta_banc INTO wa_cta_banc.
              LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto = wa_zglt031-tp_lcto AND hkont = wa_cta_banc-saknr AND shkzg EQ ck_chave_deb_cred.
                READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_zglt032-bschl BINARY SEARCH.
                IF wa_tbsl-shkzg EQ ck_chave_deb_cred.
                  ck_possui_conta_banco = abap_true.
                ENDIF.
              ENDLOOP.
            ENDLOOP.

            "As Contas de Contrapartida são somente de banco
            "As Contas de Contrapartida possui conta de cliente/fornecedor
            ck_somente_cta_banco  = abap_true.
            ck_possui_cta_cli_for = abap_false.
            LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto = wa_zglt031-tp_lcto AND hkont NE zde_saldo_cta_banco-saknr.
              READ TABLE it_cta_banc INTO wa_cta_banc WITH KEY saknr = wa_zglt032-hkont BINARY SEARCH.
              "YB08 -- Grupo de Conta de Variação Cambial
              IF ( sy-subrc IS NOT INITIAL ).
                IF wa_zglt032-xbilk IS NOT INITIAL.
                  ck_somente_cta_banco = abap_false.
                ELSE.
                  lc_conta_variacao = wa_zglt032-hkont.
                ENDIF.
              ENDIF.
              IF ( wa_zglt032-koart EQ 'D' ) OR ( wa_zglt032-koart EQ 'K' ).
                ck_possui_cta_cli_for = abap_true.
              ENDIF.
            ENDLOOP.

            IF ( ck_possui_conta_banco EQ abap_true ) AND ( ck_somente_cta_banco EQ abap_true ) AND ( wa_zglt031-st_trans_banc EQ abap_true ).
              CLEAR: zde_mov_lct_banco.
              zde_mov_lct_banco-tp_lcto        = wa_zglt031-tp_lcto.
              zde_mov_lct_banco-bukrs          = zde_saldo_cta_banco-bukrs.
              zde_mov_lct_banco-fcurr          = zde_saldo_cta_banco-waers.
              zde_mov_lct_banco-dmbtr          = wa_mov_lct_banco-dmbtr.
              zde_mov_lct_banco-saknr_variacao = lc_conta_variacao.
              CALL SCREEN 0305 STARTING AT 15 01.

              IF zde_mov_lct_banco IS NOT INITIAL.
                CALL METHOD atualiza_transferencia
                  EXPORTING
                    i_ck_chave_deb_cred = ck_chave_deb_cred
                    pr_data_changed     = pr_data_changed.
              ELSE.
                error_in_data = abap_true.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = lv_value
                  IMPORTING
                    output = lv_value.

                CALL METHOD pr_data_changed->add_protocol_entry
                  EXPORTING
                    i_msgid     = 'ZFI'
                    i_msgno     = '042'
                    i_msgty     = 'E'
                    i_msgv1     = lv_value
                    i_fieldname = ls_good-fieldname
                    i_row_id    = ls_good-row_id.
              ENDIF.
*              ENDIF.
            ELSEIF ck_possui_cta_cli_for EQ abap_true.
              "***************************************************************************************************>>>>>>>>
              "***************************************************************************************************>>>>>>>> inicio cliente fornecedor
              "Verifica Chave de Lançamento no Banco
              "H  Crédito
              "S  Débito
              IF wa_mov_lct_banco-dmbtr GT 0. "Crédito na Conta Banco (Contra Partida)
                ck_chave_deb_cred   = 'H'.
                ck_chave_deb_cred_b = 'S'.
              ELSEIF wa_mov_lct_banco-dmbtr LT 0. "Débito na Conta Banco (Contra Partida)
                ck_chave_deb_cred   = 'S'.
                ck_chave_deb_cred_b = 'H'.
              ENDIF.

              READ TABLE it_zglt032 INTO wa_zglt032 WITH KEY tp_lcto = wa_zglt031-tp_lcto
                                                             hkont   = zde_saldo_cta_banco-saknr
                                                             shkzg   = ck_chave_deb_cred_b.
              IF sy-subrc IS INITIAL.
                MOVE wa_zglt032 TO wa_zglt032_b.
                "(2) - Localizar chave de lançamento do Tipo de Lançamento para a conta bancária
                ck_chave_deb_cred_b = wa_zglt032-shkzg.
                lc_bschl_banco      = wa_zglt032-bschl.
                IF ck_chave_deb_cred EQ ck_chave_deb_cred_b.
                  error_in_data = abap_true.
                  CASE ck_chave_deb_cred.
                    WHEN 'H'.
                      CALL METHOD pr_data_changed->add_protocol_entry
                        EXPORTING
                          i_msgid     = 'ZFI'
                          i_msgno     = '045'
                          i_msgty     = 'E'
                          i_msgv1     = wa_mov_lct_banco-tp_lcto
                          i_fieldname = ls_good-fieldname
                          i_row_id    = ls_good-row_id.
                    WHEN 'S'.
                      CALL METHOD pr_data_changed->add_protocol_entry
                        EXPORTING
                          i_msgid     = 'ZFI'
                          i_msgno     = '046'
                          i_msgty     = 'E'
                          i_msgv1     = wa_mov_lct_banco-tp_lcto
                          i_fieldname = ls_good-fieldname
                          i_row_id    = ls_good-row_id.
                  ENDCASE.
                ELSE.
                  CLEAR: lv_value.
                  LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto EQ wa_zglt031-tp_lcto AND shkzg EQ ck_chave_deb_cred AND hkont NE zde_saldo_cta_banco-saknr.

                    READ TABLE it_cta_banc INTO wa_cta_banc WITH KEY saknr = wa_zglt032-hkont BINARY SEARCH.
                    IF sy-subrc IS INITIAL.
                      CONTINUE.
                    ENDIF.

                    CALL METHOD informar_parceiro
                      EXPORTING
                        i_zglt032       = wa_zglt032
                        i_zglt032_b     = wa_zglt032_b
                        pr_data_changed = pr_data_changed.

                    IF lv_value IS INITIAL.
                      CALL METHOD pr_data_changed->add_protocol_entry
                        EXPORTING
                          i_msgid     = 'ZFI'
                          i_msgno     = '050'
                          i_msgty     = 'E'
                          i_msgv1     = wa_zglt032-tp_lcto
                          i_fieldname = ls_good-fieldname
                          i_row_id    = ls_good-row_id.
                    ELSE.
                      wa_mov_lct_banco-dmbe2 = wa_mov_lct_banco-dmbtr.
                      MOVE wa_mov_lct_banco-dmbe2 TO lv_value.
                      CONDENSE lv_value NO-GAPS.
                      CALL METHOD pr_data_changed->modify_cell
                        EXPORTING
                          i_row_id    = ls_good-row_id
                          i_fieldname = 'DMBE2'
                          i_value     = lv_value.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ELSE.
                SELECT SINGLE ddtext INTO lc_val_text
                   FROM dd07t
                  WHERE domname EQ 'SHKZG'
                    AND ddlanguage EQ sy-langu
                    AND domvalue_l EQ ck_chave_deb_cred_b.

                CALL METHOD pr_data_changed->add_protocol_entry
                  EXPORTING
                    i_msgid     = 'ZFI'
                    i_msgno     = '049'
                    i_msgty     = 'E'
                    i_msgv1     = wa_mov_lct_banco-tp_lcto
                    i_msgv2     = zde_saldo_cta_banco-saknr
                    i_msgv3     = lc_val_text
                    i_fieldname = ls_good-fieldname
                    i_row_id    = ls_good-row_id.
              ENDIF.
              "***************************************************************************************************<<<<<<<<
              "***************************************************************************************************<<<<<<<< fim cliente fornecedor
            ELSE.
              CLEAR: lv_value.

              "H  Crédito
              "S  Débito
              IF wa_mov_lct_banco-dmbtr GT 0. "Crédito na Conta Banco (Contra Partida)
                ck_chave_deb_cred   = 'H'.
                ck_chave_deb_cred_b = 'S'.
              ELSEIF wa_mov_lct_banco-dmbtr LT 0. "Débito na Conta Banco (Contra Partida)
                ck_chave_deb_cred   = 'S'.
                ck_chave_deb_cred_b = 'H'.
              ENDIF.

              "Localizar chave da conta banco para somente permitir chave contrária
              "CK_CHAVE_DEB_CRED   - Lançamento
              "CK_CHAVE_DEB_CRED_C - Tipo de Lançamento
              "(1) - Localizar Paramtro do Tipo de Lançamento para a conta bancária
              READ TABLE it_zglt032 INTO wa_zglt032 WITH KEY tp_lcto = wa_zglt031-tp_lcto
                                                             hkont   = zde_saldo_cta_banco-saknr
                                                             shkzg   = ck_chave_deb_cred_b.
              IF sy-subrc IS INITIAL.
                "(2) - Localizar chave de lançamento do Tipo de Lançamento para a conta bancária
                READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_zglt032-bschl BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  ck_chave_deb_cred_b = wa_tbsl-shkzg.
                  lc_bschl_banco      = wa_tbsl-bschl.

                  IF ck_chave_deb_cred EQ ck_chave_deb_cred_b.
                    error_in_data = abap_true.
                    CASE ck_chave_deb_cred.
                      WHEN 'H'.
                        CALL METHOD pr_data_changed->add_protocol_entry
                          EXPORTING
                            i_msgid     = 'ZFI'
                            i_msgno     = '045'
                            i_msgty     = 'E'
                            i_msgv1     = wa_zglt031-tp_lcto
                            i_fieldname = ls_good-fieldname
                            i_row_id    = ls_good-row_id.
                      WHEN 'S'.
                        CALL METHOD pr_data_changed->add_protocol_entry
                          EXPORTING
                            i_msgid     = 'ZFI'
                            i_msgno     = '046'
                            i_msgty     = 'E'
                            i_msgv1     = wa_zglt031-tp_lcto
                            i_fieldname = ls_good-fieldname
                            i_row_id    = ls_good-row_id.
                    ENDCASE.
                  ELSE.
                    LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto EQ wa_zglt031-tp_lcto AND shkzg EQ ck_chave_deb_cred AND hkont NE zde_saldo_cta_banco-saknr.

                      READ TABLE it_cta_banc INTO wa_cta_banc WITH KEY saknr = wa_zglt032-hkont BINARY SEARCH.
                      IF sy-subrc IS INITIAL.
                        CONTINUE.
                      ENDIF.

                      MOVE wa_zglt032-hkont TO lv_value.
                      CALL METHOD pr_data_changed->modify_cell
                        EXPORTING
                          i_row_id    = ls_good-row_id
                          i_fieldname = 'SAKNR'
                          i_value     = lv_value.

                      MOVE wa_zglt032-bschl TO lv_value.
                      CALL METHOD pr_data_changed->modify_cell
                        EXPORTING
                          i_row_id    = ls_good-row_id
                          i_fieldname = 'BSCHL'
                          i_value     = lv_value.

                      MOVE lc_bschl_banco TO lv_value.
                      CALL METHOD pr_data_changed->modify_cell
                        EXPORTING
                          i_row_id    = ls_good-row_id
                          i_fieldname = 'BSCHL_BANCO'
                          i_value     = lv_value.

                      IF wa_mov_lct_banco-tcurr EQ wa_mov_lct_banco-fcurr.
                        wa_mov_lct_banco-dmbe2 = wa_mov_lct_banco-dmbtr.
                        MOVE wa_mov_lct_banco-dmbe2 TO lv_value.
                        CONDENSE lv_value NO-GAPS.
                        CALL METHOD pr_data_changed->modify_cell
                          EXPORTING
                            i_row_id    = ls_good-row_id
                            i_fieldname = 'DMBE2'
                            i_value     = lv_value.
                      ENDIF.
                    ENDLOOP.
                  ENDIF.
                ENDIF.
              ELSE.
                SELECT SINGLE ddtext INTO lc_val_text
                   FROM dd07t
                  WHERE domname EQ 'SHKZG'
                    AND ddlanguage EQ sy-langu
                    AND domvalue_l EQ ck_chave_deb_cred_b.

                CALL METHOD pr_data_changed->add_protocol_entry
                  EXPORTING
                    i_msgid     = 'ZFI'
                    i_msgno     = '049'
                    i_msgty     = 'E'
                    i_msgv1     = wa_zglt031-tp_lcto
                    i_msgv2     = zde_saldo_cta_banco-saknr
                    i_msgv3     = lc_val_text
                    i_fieldname = ls_good-fieldname
                    i_row_id    = ls_good-row_id.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "Validar Tipo de Lançamento
    LOOP AT pr_data_changed->mt_good_cells INTO ls_good WHERE fieldname EQ 'DMBTR'.

      MOVE zde_saldo_cta_banco-waers TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'FCURR'
          i_value     = lv_value.

      MOVE zde_saldo_cta_banco-waers TO lv_value.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'TCURR'
          i_value     = lv_value.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE it_mov_lct_banco INTO wa_mov_lct_banco INDEX ls_good-row_id.

      IF wa_mov_lct_banco-tp_lcto IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE it_zglt031 INTO wa_zglt031 WITH KEY tp_lcto = wa_mov_lct_banco-tp_lcto.

      wa_mov_lct_banco-dmbtr = lv_value.

      "Verifica Chave de Lançamento no Banco
      "H  Crédito
      "S  Débito
      IF wa_mov_lct_banco-dmbtr GT 0. "Crédito na Conta Banco
        ck_chave_deb_cred   = 'H'.
        ck_chave_deb_cred_b = 'S'.
      ELSEIF wa_mov_lct_banco-dmbtr LT 0. "Débito na Conta Banco
        ck_chave_deb_cred   = 'S'.
        ck_chave_deb_cred_b = 'H'.
      ENDIF.

      "Somente valida conta de banco se o lançamento já estiver com valor (debito/crédito).
      IF wa_mov_lct_banco-dmbtr NE 0.

        "As Contas de Contrapartida possui conta de banco
        ck_possui_conta_banco = abap_false.
        LOOP AT it_cta_banc INTO wa_cta_banc.
          LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto EQ wa_mov_lct_banco-tp_lcto AND hkont EQ wa_cta_banc-saknr AND shkzg EQ ck_chave_deb_cred.
            READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_zglt032-bschl BINARY SEARCH.
            IF wa_tbsl-shkzg EQ ck_chave_deb_cred.
              ck_possui_conta_banco = abap_true.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        "As Contas de Contrapartida possui somente conta de banco
        "As Contas de Contrapartida possui conta de cliente/fornecedor
        ck_somente_cta_banco  = abap_true.
        ck_possui_cta_cli_for = abap_false.
        LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto = wa_mov_lct_banco-tp_lcto  AND hkont NE zde_saldo_cta_banco-saknr.
          READ TABLE it_cta_banc INTO wa_cta_banc WITH KEY saknr = wa_zglt032-hkont BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            IF wa_zglt032-xbilk IS NOT INITIAL.
              ck_somente_cta_banco = abap_false.
            ELSE.
              lc_conta_variacao = wa_zglt032-hkont.
            ENDIF.
          ENDIF.
          IF ( wa_zglt032-koart EQ 'D' ) OR ( wa_zglt032-koart EQ 'K' ).
            ck_possui_cta_cli_for = abap_true.
          ENDIF.
        ENDLOOP.

        IF ( ck_possui_conta_banco EQ abap_true ) AND ( ck_somente_cta_banco EQ abap_true ) AND ( wa_zglt031-st_trans_banc EQ abap_true ).
          CLEAR: zde_mov_lct_banco.
          zde_mov_lct_banco-tp_lcto        = wa_mov_lct_banco-tp_lcto.
          zde_mov_lct_banco-bukrs          = zde_saldo_cta_banco-bukrs.
          zde_mov_lct_banco-fcurr          = zde_saldo_cta_banco-waers.
          zde_mov_lct_banco-dmbtr          = wa_mov_lct_banco-dmbtr.
          zde_mov_lct_banco-saknr_variacao = lc_conta_variacao.
          CALL SCREEN 0305 STARTING AT 15 01.

          IF zde_mov_lct_banco IS NOT INITIAL.
            CALL METHOD atualiza_transferencia
              EXPORTING
                i_ck_chave_deb_cred = ck_chave_deb_cred
                pr_data_changed     = pr_data_changed.
          ELSE.
            error_in_data = abap_true.
            CALL METHOD pr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = 'ZFI'
                i_msgno     = '042'
                i_msgty     = 'E'
                i_msgv1     = wa_mov_lct_banco-tp_lcto
                i_fieldname = ls_good-fieldname
                i_row_id    = ls_good-row_id.
          ENDIF.
*          ENDIF.

        ELSEIF ck_possui_cta_cli_for EQ abap_true.
          "***************************************************************************************************>>>>>>>>
          "***************************************************************************************************>>>>>>>> inicio cliente fornecedor
          "Verifica Chave de Lançamento no Banco
          "H  Crédito
          "S  Débito
          IF wa_mov_lct_banco-dmbtr GT 0. "Crédito na Conta Banco (Contra Partida)
            ck_chave_deb_cred   = 'H'.
            ck_chave_deb_cred_b = 'S'.
          ELSEIF wa_mov_lct_banco-dmbtr LT 0. "Débito na Conta Banco (Contra Partida)
            ck_chave_deb_cred   = 'S'.
            ck_chave_deb_cred_b = 'H'.
          ENDIF.

          READ TABLE it_zglt032 INTO wa_zglt032 WITH KEY tp_lcto = wa_mov_lct_banco-tp_lcto
                                                         hkont   = zde_saldo_cta_banco-saknr
                                                         shkzg   = ck_chave_deb_cred_b.
          IF sy-subrc IS INITIAL.
            MOVE wa_zglt032 TO wa_zglt032_b.
            "(2) - Localizar chave de lançamento do Tipo de Lançamento para a conta bancária
            ck_chave_deb_cred_b = wa_zglt032-shkzg.
            lc_bschl_banco      = wa_zglt032-bschl.
            IF ck_chave_deb_cred EQ ck_chave_deb_cred_b.
              error_in_data = abap_true.
              CASE ck_chave_deb_cred.
                WHEN 'H'.
                  CALL METHOD pr_data_changed->add_protocol_entry
                    EXPORTING
                      i_msgid     = 'ZFI'
                      i_msgno     = '045'
                      i_msgty     = 'E'
                      i_msgv1     = wa_mov_lct_banco-tp_lcto
                      i_fieldname = ls_good-fieldname
                      i_row_id    = ls_good-row_id.
                WHEN 'S'.
                  CALL METHOD pr_data_changed->add_protocol_entry
                    EXPORTING
                      i_msgid     = 'ZFI'
                      i_msgno     = '046'
                      i_msgty     = 'E'
                      i_msgv1     = wa_mov_lct_banco-tp_lcto
                      i_fieldname = ls_good-fieldname
                      i_row_id    = ls_good-row_id.
              ENDCASE.
            ELSE.
              LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto EQ wa_mov_lct_banco-tp_lcto AND shkzg EQ ck_chave_deb_cred  AND hkont NE zde_saldo_cta_banco-saknr.

                READ TABLE it_cta_banc INTO wa_cta_banc WITH KEY saknr = wa_zglt032-hkont BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  CONTINUE.
                ENDIF.

                CALL METHOD informar_parceiro
                  EXPORTING
                    i_zglt032       = wa_zglt032
                    i_zglt032_b     = wa_zglt032_b
                    pr_data_changed = pr_data_changed.

                IF lv_value IS INITIAL.
                  CALL METHOD pr_data_changed->add_protocol_entry
                    EXPORTING
                      i_msgid     = 'ZFI'
                      i_msgno     = '050'
                      i_msgty     = 'E'
                      i_msgv1     = wa_zglt032-tp_lcto
                      i_fieldname = ls_good-fieldname
                      i_row_id    = ls_good-row_id.
                ELSE.
                  wa_mov_lct_banco-dmbe2 = wa_mov_lct_banco-dmbtr.
                  MOVE wa_mov_lct_banco-dmbe2 TO lv_value.
                  CONDENSE lv_value NO-GAPS.
                  CALL METHOD pr_data_changed->modify_cell
                    EXPORTING
                      i_row_id    = ls_good-row_id
                      i_fieldname = 'DMBE2'
                      i_value     = lv_value.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ELSE.
            SELECT SINGLE ddtext INTO lc_val_text
               FROM dd07t
              WHERE domname EQ 'SHKZG'
                AND ddlanguage EQ sy-langu
                AND domvalue_l EQ ck_chave_deb_cred_b.

            CALL METHOD pr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = 'ZFI'
                i_msgno     = '049'
                i_msgty     = 'E'
                i_msgv1     = wa_mov_lct_banco-tp_lcto
                i_msgv2     = zde_saldo_cta_banco-saknr
                i_msgv3     = lc_val_text
                i_fieldname = ls_good-fieldname
                i_row_id    = ls_good-row_id.
          ENDIF.
          "***************************************************************************************************<<<<<<<<
          "***************************************************************************************************<<<<<<<< fim cliente fornecedor
        ELSE.
          CLEAR: lv_value.

          "Verifica Chave de Lançamento no Banco
          "H  Crédito
          "S  Débito
          IF wa_mov_lct_banco-dmbtr GT 0. "Crédito na Conta Banco (Contra Partida)
            ck_chave_deb_cred   = 'H'.
            ck_chave_deb_cred_b = 'S'.
          ELSEIF wa_mov_lct_banco-dmbtr LT 0. "Débito na Conta Banco (Contra Partida)
            ck_chave_deb_cred   = 'S'.
            ck_chave_deb_cred_b = 'H'.
          ENDIF.

          "Localizar chave da conta banco para somente permitir chave contrária
          "CK_CHAVE_DEB_CRED   - Lançamento
          "CK_CHAVE_DEB_CRED_C - Tipo de Lançamento
          "(1) - Localizar Paramtro do Tipo de Lançamento para a conta bancária
          READ TABLE it_zglt032 INTO wa_zglt032 WITH KEY tp_lcto = wa_mov_lct_banco-tp_lcto
                                                         hkont   = zde_saldo_cta_banco-saknr
                                                         shkzg   = ck_chave_deb_cred_b.
          IF sy-subrc IS INITIAL.
            "(2) - Localizar chave de lançamento do Tipo de Lançamento para a conta bancária
            ck_chave_deb_cred_b = wa_zglt032-shkzg.
            lc_bschl_banco      = wa_zglt032-bschl.
            IF ck_chave_deb_cred EQ ck_chave_deb_cred_b.
              error_in_data = abap_true.
              CASE ck_chave_deb_cred.
                WHEN 'H'.
                  CALL METHOD pr_data_changed->add_protocol_entry
                    EXPORTING
                      i_msgid     = 'ZFI'
                      i_msgno     = '045'
                      i_msgty     = 'E'
                      i_msgv1     = wa_mov_lct_banco-tp_lcto
                      i_fieldname = ls_good-fieldname
                      i_row_id    = ls_good-row_id.
                WHEN 'S'.
                  CALL METHOD pr_data_changed->add_protocol_entry
                    EXPORTING
                      i_msgid     = 'ZFI'
                      i_msgno     = '046'
                      i_msgty     = 'E'
                      i_msgv1     = wa_mov_lct_banco-tp_lcto
                      i_fieldname = ls_good-fieldname
                      i_row_id    = ls_good-row_id.
              ENDCASE.
            ELSE.
              LOOP AT it_zglt032 INTO wa_zglt032 WHERE tp_lcto EQ wa_mov_lct_banco-tp_lcto AND shkzg EQ ck_chave_deb_cred AND hkont NE zde_saldo_cta_banco-saknr.

                READ TABLE it_cta_banc INTO wa_cta_banc WITH KEY saknr = wa_zglt032-hkont BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  CONTINUE.
                ENDIF.

                IF lv_value IS INITIAL.
                  MOVE wa_zglt032-hkont TO lv_value.
                  CALL METHOD pr_data_changed->modify_cell
                    EXPORTING
                      i_row_id    = ls_good-row_id
                      i_fieldname = 'SAKNR'
                      i_value     = lv_value.

                  MOVE wa_zglt032-bschl TO lv_value.
                  CALL METHOD pr_data_changed->modify_cell
                    EXPORTING
                      i_row_id    = ls_good-row_id
                      i_fieldname = 'BSCHL'
                      i_value     = lv_value.

                  MOVE lc_bschl_banco TO lv_value.
                  CALL METHOD pr_data_changed->modify_cell
                    EXPORTING
                      i_row_id    = ls_good-row_id
                      i_fieldname = 'BSCHL_BANCO'
                      i_value     = lv_value.

                  IF wa_mov_lct_banco-tcurr EQ wa_mov_lct_banco-fcurr.
                    wa_mov_lct_banco-dmbe2 = wa_mov_lct_banco-dmbtr.
                    MOVE wa_mov_lct_banco-dmbe2 TO lv_value.
                    CONDENSE lv_value NO-GAPS.
                    CALL METHOD pr_data_changed->modify_cell
                      EXPORTING
                        i_row_id    = ls_good-row_id
                        i_fieldname = 'DMBE2'
                        i_value     = lv_value.
                  ENDIF.

                ENDIF.
              ENDLOOP.
            ENDIF.
          ELSE.
            SELECT SINGLE ddtext INTO lc_val_text
               FROM dd07t
              WHERE domname EQ 'SHKZG'
                AND ddlanguage EQ sy-langu
                AND domvalue_l EQ ck_chave_deb_cred_b.

            CALL METHOD pr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = 'ZFI'
                i_msgno     = '049'
                i_msgty     = 'E'
                i_msgv1     = wa_mov_lct_banco-tp_lcto
                i_msgv2     = zde_saldo_cta_banco-saknr
                i_msgv3     = lc_val_text
                i_fieldname = ls_good-fieldname
                i_row_id    = ls_good-row_id.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.                    "perform_semantic_checks

  METHOD data_changed.
    error_in_data = space.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.
  ENDMETHOD.                    "on_data_chaged

  METHOD data_changed_finished.

    DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd,
          i_data          TYPE gdatu_inv,
          e_ukurs  	      TYPE ukurs_curr.

    DATA: wa_zglt031 TYPE zglt031.
*{   REPLACE        DEVK9A1UKD                                        9
*\    FIELD-SYMBOLS: <fsmov> TYPE zde_mov_lct_banco.
    FIELD-SYMBOLS: <fsmov> TYPE ty_zde_mov_lct_banco.
*}   REPLACE

    IF e_modified IS NOT INITIAL.

      LOOP AT it_mov_lct_banco ASSIGNING <fsmov>.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fsmov>-tp_lcto
          IMPORTING
            output = wa_zglt031-tp_lcto.

        READ TABLE it_zglt031 INTO wa_zglt031 WITH KEY tp_lcto = <fsmov>-tp_lcto.
        IF sy-subrc IS INITIAL.
          <fsmov>-descricao = wa_zglt031-descricao.
        ENDIF.
        IF <fsmov>-tcurr EQ <fsmov>-fcurr.
          <fsmov>-dmbe2 = <fsmov>-dmbtr.
        ELSE.
          IF <fsmov>-ukurs IS NOT INITIAL.
            i_data = sy-datum.
            CREATE OBJECT obj_zcl_util_sd.
            IF zde_saldo_cta_banco-waers NE 'GBP'.
              obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data ).
              obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
              obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = <fsmov>-fcurr ).
              obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = <fsmov>-tcurr ).
              obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).
            ELSE.
              obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data   ).
              obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
              obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'GBP' ).
              obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'USD' ).
              obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).
            ENDIF.
            IF e_ukurs LT 0.
              IF '0201_0202' CS zde_saldo_cta_banco-bukrs AND
                ( <fsmov>-tcurr EQ 'USD' AND <fsmov>-fcurr EQ 'EUR' ).
                <fsmov>-dmbe2 = <fsmov>-dmbtr * <fsmov>-ukurs.
              ELSE.
                IF zde_saldo_cta_banco-waers NE 'GBP'.
                  <fsmov>-dmbe2 = <fsmov>-dmbtr / <fsmov>-ukurs.
                ELSE.
                  <fsmov>-dmbe2 = <fsmov>-dmbtr * <fsmov>-ukurs.
                ENDIF.
              ENDIF.
            ELSE.
              IF '0201_0202' CS zde_saldo_cta_banco-bukrs  AND
                ( <fsmov>-tcurr EQ 'EUR' AND <fsmov>-fcurr EQ 'USD' ).
                <fsmov>-dmbe2 = <fsmov>-dmbtr / <fsmov>-ukurs.
              ELSE.
                IF zde_saldo_cta_banco-waers NE 'GBP'.
                  <fsmov>-dmbe2 = <fsmov>-dmbtr * <fsmov>-ukurs.
                ELSE.
                  <fsmov>-dmbe2 = <fsmov>-dmbtr / <fsmov>-ukurs.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            <fsmov>-dmbe2 = 0.
          ENDIF.
        ENDIF.
      ENDLOOP.

      DATA: tl_dynpfields TYPE TABLE OF dynpread,
            wl_dynpfields TYPE dynpread.

      sd_compen_02 = sd_compen_cop.
      LOOP AT it_mov_lct_banco INTO DATA(fsmov).
        sd_compen_02 = sd_compen_02  + fsmov-dmbtr.
      ENDLOOP.

      MOVE: 'SD_COMPEN_02'        TO wl_dynpfields-fieldname,
            sd_compen_02        TO wl_dynpfields-fieldvalue.
      CONDENSE wl_dynpfields-fieldvalue NO-GAPS.

      APPEND wl_dynpfields TO tl_dynpfields.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname     = sy-repid
          dynumb     = sy-dynnr
        TABLES
          dynpfields = tl_dynpfields.


      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '/00'
        EXCEPTIONS
          function_not_supported = 1.

      wa_stable_0304-row = abap_true.
      wa_stable_0304-col = abap_true.
      CALL METHOD ctl_alv_0304->refresh_table_display
        EXPORTING
          is_stable = wa_stable_0304.
    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA: ctl_con_0304       TYPE REF TO cl_gui_custom_container,
      gs_lay_0304        TYPE lvc_s_layo,
      gs_var_0304        TYPE disvariant,
      gs_scroll_col_0304 TYPE lvc_s_col,
      gs_scroll_row_0304 TYPE lvc_s_roid,
      it_catalog_0304    TYPE lvc_t_fcat.

DATA: it_selected_0304 TYPE lvc_t_row,
      wa_selected_0304 TYPE lvc_s_row.

DATA: it_exclude_0304 TYPE ui_functions,
      wa_exclude_0304 LIKE LINE OF it_exclude_0304.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0304  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0304 OUTPUT.

  DATA: fs_sort_0304 TYPE lvc_s_sort,
        gt_sort_0304 TYPE lvc_t_sort.


  SET PF-STATUS 'PF0303'.
  SET TITLEBAR 'TLDER'.

  IF ctl_con_0304 IS INITIAL.

    CREATE OBJECT ctl_con_0304
      EXPORTING
        container_name = 'ALV_LANC'.

    CREATE OBJECT ctl_alv_0304
      EXPORTING
        i_parent = ctl_con_0304.

    PERFORM fill_it_fieldcatalog_0304.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0304.
*   Set layout parameters for ALV grid

    gs_lay_0304-zebra      = 'X'.
    gs_lay_0304-edit_mode  = 'X'.
    gs_lay_0304-col_opt    = 'X'.
*{   REPLACE        DEVK9A1UKD                                        4
*\    gs_lay_0304-stylefname = 'STYLE'.
    gs_lay_0304-stylefname = 'STYLE'.
*    gs_lay_0304-NO_F4 = abap_false.
*}   REPLACE

    APPEND '&LOCAL&CUT'           TO it_exclude_0304.
    APPEND '&LOCAL&MOVE_ROW'      TO it_exclude_0304.
    APPEND '&LOCAL&PASTE'         TO it_exclude_0304.
    APPEND '&LOCAL&PASTE_NEW_ROW' TO it_exclude_0304.
    APPEND '&VARI_ADMIN'          TO it_exclude_0304.
    APPEND '&LOCAL&COPY'          TO it_exclude_0304.
    APPEND '&LOCAL&COPY_ROW'      TO it_exclude_0304.
    APPEND '&VLOTUS'              TO it_exclude_0304.
    APPEND '&AQW'                 TO it_exclude_0304.
    APPEND '&PRINT'               TO it_exclude_0304.
*    APPEND '&MB_SUM'              TO it_exclude_0304.
*    APPEND '&AVERAGE'             TO it_exclude_0304.
    APPEND '&MB_VIEW'             TO it_exclude_0304.
    APPEND '&MB_EXPORT'           TO it_exclude_0304.
*    APPEND '&MB_FILTER'           TO it_exclude_0304.
    APPEND '&GRAPH'               TO it_exclude_0304.
    APPEND '&INFO'                TO it_exclude_0304.
    APPEND '&CHECK'               TO it_exclude_0304.

*{   DELETE         DEVK9A1UKD                                        2
*\    CALL METHOD ctl_alv_0304->set_table_for_first_display
*\      EXPORTING
*\        is_layout            = gs_lay_0304
*\        is_variant           = gs_var_0304
*\        i_save               = 'A'
*\        it_toolbar_excluding = it_exclude_0304        "I_SAVE = 'A'
*\      CHANGING
*\        it_fieldcatalog      = it_catalog_0304 "IT_EXCEPT_QINFO = IT_HINTS
*\        it_outtab            = it_mov_lct_banco[]
*\        it_sort              = gt_sort_0304[].
*}   DELETE

    CALL METHOD ctl_alv_0304->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv_0304->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT event_handler_0304.
    SET HANDLER event_handler_0304->data_changed_finished FOR ctl_alv_0304.
    SET HANDLER event_handler_0304->data_changed          FOR ctl_alv_0304.
    SET HANDLER event_handler_0304->subtotal_text         FOR ctl_alv_0304.
*    SET HANDLER EVENT_HANDLER_0304->ON_F4                 FOR CTL_ALV_0304.
*{   INSERT         DEVK9A1UKD                                        1
*
* RJF - Ini - ZFIS36 - Ficha de Conciliação de Contas Bancarias

    SET HANDLER event_handler_0304->on_f4                 FOR ctl_alv_0304.

    CLEAR: fs_sort_0304.
    fs_sort_0304-spos       = 1.     "first sorting key
    fs_sort_0304-fieldname  = 'TP_LCTO'. "fieldname for sort
    fs_sort_0304-up         = 'X'. "sort ascending
    INSERT fs_sort_0304 INTO TABLE gt_sort_0304. "insert to sort table

    DATA: gs_f4 TYPE lvc_s_f4,
          gt_f4 TYPE lvc_t_f4.

    gs_f4-fieldname  = 'TP_LCTO'.
    gs_f4-register   = 'X'.
    gs_f4-getbefore = 'X' .
    APPEND gs_f4 TO gt_f4.
    CLEAR gs_f4.

    CALL METHOD ctl_alv_0304->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4.

    CALL METHOD ctl_alv_0304->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0304
        is_variant           = gs_var_0304
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_0304        "I_SAVE = 'A'
      CHANGING
        it_fieldcatalog      = it_catalog_0304 "IT_EXCEPT_QINFO = IT_HINTS
        it_outtab            = it_mov_lct_banco[]
        it_sort              = gt_sort_0304[].
* RJF - Fim - ZFIS36 - Ficha de Conciliação de Contas Bancarias
*}   INSERT
    CALL METHOD ctl_alv_0304->refresh_table_display.

*{   DELETE         DEVK9A1UKD                                        3
*\    DATA: gs_f4 TYPE lvc_s_f4,
*\          gt_f4 TYPE lvc_t_f4.
*\
*\    gs_f4-fieldname  = 'TP_LCTO'.
*\    gs_f4-register   = 'X'.
*\    APPEND gs_f4 TO gt_f4.
*\
*\    CALL METHOD ctl_alv_0304->register_f4_for_fields
*\      EXPORTING
*\        it_f4 = gt_f4.
*}   DELETE

  ELSE.
    wa_stable_0304-row = abap_true.
    wa_stable_0304-col = abap_true.
    CALL METHOD ctl_alv_0304->refresh_table_display
      EXPORTING
        is_stable = wa_stable_0304.
  ENDIF.

  CALL METHOD ctl_alv_0304->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0304
      es_row_no   = gs_scroll_row_0304.

ENDMODULE.                 " STATUS_0304  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0304_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0304_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0304_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0304
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0304 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0304> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_MOV_LCT_BANCO'
    CHANGING
      ct_fieldcat      = it_catalog_0304.

  lc_col_pos = 1.

  LOOP AT it_catalog_0304 ASSIGNING <fs_cat_0304>.
    <fs_cat_0304>-col_pos = lc_col_pos.
    <fs_cat_0304>-tabname   = 'ZDE_MOV_LCT_BANCO'.
    <fs_cat_0304>-fieldname = <fs_cat_0304>-fieldname.
    ADD 1 TO lc_col_pos.
    CASE <fs_cat_0304>-fieldname.
      WHEN 'TP_LCTO'.
        <fs_cat_0304>-edit       = abap_true.
        "<FS_CAT_0304>-F4AVAILABL = ABAP_TRUE.
        "<FS_CAT_0304>-CHECKTABLE = 'ZGLT031'.
*{   INSERT         DEVK9A1UKD                                        1
*
        <fs_cat_0304>-f4availabl = 'X'.
        <fs_cat_0304>-checktable = 'ZGLT031'.
*        <FS_CAT_0304>-DFIELDNAME = 'TP_LCTO'.

        <fs_cat_0304>-fieldname = 'TP_LCTO'.
        <fs_cat_0304>-ref_table = 'ZGLT031'.
        <fs_cat_0304>-ref_field = 'TP_LCTO'.

*}   INSERT
        "<FS_CAT_0304>-DFIELDNAME = 'TP_LCTO'.
        "Z_PSQ_TP_LANC"
      WHEN 'SGTXT'.
        <fs_cat_0304>-edit      = abap_true.
        <fs_cat_0304>-outputlen = 30.
      WHEN 'DESCRICAO'.
        <fs_cat_0304>-outputlen = 30.
      WHEN 'FCURR' OR 'TCURR' OR 'SAKNR' OR 'UKURS' OR 'UKURS_VARIACAO'.
        <fs_cat_0304>-edit      = abap_false.
      WHEN 'BUKRS' OR 'KOART' OR 'PARID' OR 'AKONT' OR 'UMSKZ' OR 'SKONT' OR 'TXT50' OR 'TXT50_VARIACAO' OR 'DMBE2_VARIACAOC'.
        <fs_cat_0304>-edit      = abap_false.
        <fs_cat_0304>-no_out    = abap_true.
      WHEN 'BSCHL' .
        <fs_cat_0304>-edit      = abap_false.
        <fs_cat_0304>-no_out    = abap_true.
        <fs_cat_0304>-scrtext_l = 'Chave Outras'.
      WHEN 'BSCHL_BANCO'.
        <fs_cat_0304>-edit      = abap_false.
        <fs_cat_0304>-no_out    = abap_true.
        <fs_cat_0304>-scrtext_l = 'Chave Banco'.
      WHEN 'BSCHL_VARIACAO'.
        <fs_cat_0304>-edit      = abap_false.
        <fs_cat_0304>-no_out    = abap_true.
        <fs_cat_0304>-scrtext_l = 'Chave Variação Outras'.
      WHEN 'BSCHL_VARIACAOB'.
        <fs_cat_0304>-edit      = abap_false.
        <fs_cat_0304>-no_out    = abap_true.
        <fs_cat_0304>-scrtext_l = 'Chave Variação Banco'.
      WHEN 'VALOR_PAYMENTS' OR 'VALOR_RESIDUAL'.
        <fs_cat_0304>-edit      = abap_true.
        <fs_cat_0304>-outputlen = 20.
      WHEN 'DMBTR'.
        <fs_cat_0304>-do_sum    = abap_true.
        <fs_cat_0304>-outputlen = 15.
        IF lc_arquivo_extrato EQ abap_false.
          <fs_cat_0304>-edit    = abap_true.
        ELSE.
          <fs_cat_0304>-edit    = abap_false.
        ENDIF.
      WHEN 'DMBE2' OR 'DMBE2_VARIACAO'.
        <fs_cat_0304>-do_sum    = abap_true.
        <fs_cat_0304>-outputlen = 15.
        <fs_cat_0304>-edit      = abap_false.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0304

*&---------------------------------------------------------------------*
*&      Form  GERAR_LOTE_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gerar_lote_documentos .

  CLEAR: it_mov_lct_banco[].

  PERFORM popula_info_disp_lote.

  LOOP AT it_zglt031.
    it_mov_lct_banco-tp_lcto   = it_zglt031-tp_lcto.
    it_mov_lct_banco-descricao = it_zglt031-descricao.
    it_mov_lct_banco-fcurr = zde_saldo_cta_banco-waers.
    it_mov_lct_banco-bukrs = zde_saldo_cta_banco-bukrs.
    it_mov_lct_banco-tcurr = zde_saldo_cta_banco-waers.
    it_mov_lct_banco-dmbtr = 0.
    it_mov_lct_banco-dmbe2 = 0.
*{   INSERT         DEVK9A1UKD                                        1
*
    DATA: wa_style TYPE lvc_s_styl,
          style    TYPE lvc_t_styl WITH HEADER LINE.

*      IF it_bsik_alv-waers = zde_saldo_cta_banco-waers.
    wa_style-fieldname = 'TP_LCTO'.
    wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT  wa_style INTO TABLE style.
    it_mov_lct_banco-style[] = style[].
*      ENDIF.

*}   INSERT
    APPEND it_mov_lct_banco.
  ENDLOOP.

  sd_compen_cop = sd_compen.
  sd_compen_02  = sd_compen.
  vg_text_010 = TEXT-018.
  CALL SCREEN 0304 STARTING AT 10 3.
  CLEAR: sd_compen_cop.


ENDFORM.                    " GERAR_LOTE_DOCUMENTOS

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0304
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0304 .

  gs_var_0304-report      = sy-repid.
  gs_var_0304-handle      = '0304'.
  gs_var_0304-log_group   = abap_false.
  gs_var_0304-username    = abap_false.
  gs_var_0304-variant     = abap_false.
  gs_var_0304-text        = abap_false.
  gs_var_0304-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0304

*&---------------------------------------------------------------------*
*&      Module  STATUS_0305  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0305 OUTPUT.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd,
        i_data          TYPE gdatu_inv,
        calc_date	      LIKE p0001-begda,
        e_ukurs  	      TYPE ukurs_curr,
        lc_int_varia    TYPE i.

  DATA: wa_t001_0304 TYPE t001.

  SET PF-STATUS 'PF0303'.
  SET TITLEBAR 'TL0305'.

  CLEAR: zde_mov_lct_banco-tcurr.

  SELECT SINGLE ktopl INTO wa_t001_0304-ktopl
    FROM t001
   WHERE bukrs EQ zde_mov_lct_banco-bukrs.

  IF zde_mov_lct_banco-saknr IS NOT INITIAL.

    SELECT SINGLE * INTO wa_aux
      FROM zsaldo_cta_moeda
     WHERE bukrs EQ zde_mov_lct_banco-bukrs
       AND saknr EQ zde_mov_lct_banco-saknr.

    IF sy-subrc IS INITIAL.
      zde_mov_lct_banco-tcurr = wa_aux-waers.
    ENDIF.

    SELECT SINGLE txt50 INTO zde_mov_lct_banco-txt50
      FROM skat
     WHERE spras EQ sy-langu
       AND ktopl EQ wa_t001_0304-ktopl
       AND saknr EQ zde_mov_lct_banco-saknr.

  ENDIF.

*  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*    EXPORTING
*      DATE      = ZDE_SALDO_CTA_BANCO-BUDAT
*      DAYS      = 1
*      MONTHS    = 1
*      YEARS     = 0
*      SIGNUM    = '-'
*    IMPORTING
*      CALC_DATE = CALC_DATE.

  i_data = zde_saldo_cta_banco-budat.

  IF ( zde_mov_lct_banco-tcurr IS NOT INITIAL ) AND ( zde_mov_lct_banco-ukurs IS INITIAL ).
    IF zde_mov_lct_banco-tcurr EQ zde_mov_lct_banco-fcurr.
      zde_mov_lct_banco-dmbe2 = zde_mov_lct_banco-dmbtr.
      zde_mov_lct_banco-ukurs = 1.
    ELSE.
      """" buscar taxa de moeda de origem para destino para saber que tipo de fator usar ( divisão ou multiplicação )
      CREATE OBJECT obj_zcl_util_sd.
      obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data ).
      obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
      obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = zde_mov_lct_banco-fcurr ).
      obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = zde_mov_lct_banco-tcurr ).
      obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).

      zde_mov_lct_banco-ukurs = abs( e_ukurs ).

      PERFORM calcular
         USING e_ukurs zde_mov_lct_banco-ukurs zde_mov_lct_banco-bukrs zde_mov_lct_banco-tcurr zde_mov_lct_banco-fcurr zde_mov_lct_banco-dmbtr
      CHANGING zde_mov_lct_banco-dmbe2 zde_mov_lct_banco-operador_cotacao.

      IF zde_mov_lct_banco-ukurs_variacao IS NOT INITIAL.

        PERFORM calcular
          USING e_ukurs
                zde_mov_lct_banco-ukurs_variacao
                zde_mov_lct_banco-bukrs
                zde_mov_lct_banco-tcurr
                zde_mov_lct_banco-fcurr
                zde_mov_lct_banco-dmbtr
       CHANGING zde_mov_lct_banco-dmbe2_variacaoc
                zde_mov_lct_banco-operador_cotacao.
        zde_mov_lct_banco-dmbe2_variacao = zde_mov_lct_banco-dmbe2 - zde_mov_lct_banco-dmbe2_variacaoc.
      ENDIF.

    ENDIF.
  ELSEIF zde_mov_lct_banco-tcurr IS NOT INITIAL AND
         zde_mov_lct_banco-fcurr IS NOT INITIAL AND
         zde_mov_lct_banco-tcurr NE zde_mov_lct_banco-fcurr AND
         zde_mov_lct_banco-ukurs IS NOT INITIAL AND
         zde_mov_lct_banco-dmbtr NE 0 AND
         zde_mov_lct_banco-ukurs NE 0.

    CREATE OBJECT obj_zcl_util_sd.
    obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data ).
    obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
    obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = zde_mov_lct_banco-fcurr ).
    obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = zde_mov_lct_banco-tcurr ).
    obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).

    PERFORM calcular
      USING e_ukurs
            zde_mov_lct_banco-ukurs
            zde_mov_lct_banco-bukrs
            zde_mov_lct_banco-tcurr
            zde_mov_lct_banco-fcurr
            zde_mov_lct_banco-dmbtr
   CHANGING zde_mov_lct_banco-dmbe2
            zde_mov_lct_banco-operador_cotacao.

    IF gb_tipo_valor EQ 'T' AND "ZDE_MOV_LCT_BANCO-UKURS_VARIACAO IS NOT INITIAL
          zde_mov_lct_banco-ukurs_variacao_new IS NOT INITIAL.
      "Calculo de Variação Cambial pela taxa """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      PERFORM calcular2
         USING e_ukurs
               zde_mov_lct_banco-ukurs_variacao_new
               zde_mov_lct_banco-bukrs
               zde_mov_lct_banco-tcurr
               zde_mov_lct_banco-fcurr
               zde_mov_lct_banco-dmbtr
      CHANGING zde_mov_lct_banco-dmbe2_variacaoc
               zde_mov_lct_banco-operador_cotacao.

      zde_mov_lct_banco-dmbe2_variacao = zde_mov_lct_banco-dmbe2 - zde_mov_lct_banco-dmbe2_variacaoc.

    ELSEIF gb_tipo_valor EQ 'V' AND zde_mov_lct_banco-dmbe2_variacaoc NE 0.
      "Calculo de Variação Cambial pelao valor """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      IF e_ukurs LT 0.
        IF '0201_0202' CS zde_saldo_cta_banco-bukrs AND ( zde_mov_lct_banco-tcurr EQ 'USD' AND zde_mov_lct_banco-fcurr EQ 'EUR' ).
          zde_mov_lct_banco-ukurs_variacao  = zde_mov_lct_banco-dmbe2_variacaoc / zde_mov_lct_banco-dmbtr.
          lc_int_varia = zde_mov_lct_banco-ukurs_variacao * 10000.
          zde_mov_lct_banco-ukurs_variacao = lc_int_varia / 10000.
        ELSE.
          zde_mov_lct_banco-ukurs_variacao = zde_mov_lct_banco-dmbtr / zde_mov_lct_banco-dmbe2_variacaoc.
          lc_int_varia = zde_mov_lct_banco-ukurs_variacao * 10000.
          zde_mov_lct_banco-ukurs_variacao = lc_int_varia / 10000.
        ENDIF.
      ELSE.
        IF '0201_0202' CS zde_saldo_cta_banco-bukrs AND ( zde_mov_lct_banco-tcurr EQ 'EUR' AND zde_mov_lct_banco-fcurr EQ 'USD' ).
          zde_mov_lct_banco-ukurs_variacao  = zde_mov_lct_banco-dmbtr / zde_mov_lct_banco-dmbe2_variacaoc.
          lc_int_varia = zde_mov_lct_banco-ukurs_variacao * 10000.
          zde_mov_lct_banco-ukurs_variacao = lc_int_varia / 10000.
        ELSE.
          zde_mov_lct_banco-ukurs_variacao  = zde_mov_lct_banco-dmbe2_variacaoc / zde_mov_lct_banco-dmbtr.
          lc_int_varia = zde_mov_lct_banco-ukurs_variacao * 10000.
          zde_mov_lct_banco-ukurs_variacao = lc_int_varia / 10000.
        ENDIF.
      ENDIF.
      zde_mov_lct_banco-dmbe2_variacao = zde_mov_lct_banco-dmbe2 - zde_mov_lct_banco-dmbe2_variacaoc.
    ELSE.
      CLEAR: zde_mov_lct_banco-dmbe2_variacao, zde_mov_lct_banco-dmbe2_variacaoc.
    ENDIF.
  ENDIF.

  IF zde_mov_lct_banco-saknr_variacao IS NOT INITIAL.
    LOOP AT it_zglt032 WHERE tp_lcto = zde_mov_lct_banco-tp_lcto.
      READ TABLE it_cta_banc WITH KEY saknr = it_zglt032-hkont BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        IF it_zglt032-xbilk IS INITIAL.
          READ TABLE it_tbsl WITH KEY bschl = it_zglt032-bschl BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            IF ( ( it_tbsl-shkzg EQ 'H' ) AND ( zde_mov_lct_banco-dmbe2_variacao GT 0 ) ) OR
               ( ( it_tbsl-shkzg EQ 'S' ) AND ( zde_mov_lct_banco-dmbe2_variacao LT 0 ) ). "Crédito
              zde_mov_lct_banco-saknr_variacao = it_zglt032-hkont.
              zde_mov_lct_banco-bschl_variacao = it_zglt032-bschl.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SELECT SINGLE txt50 INTO zde_mov_lct_banco-txt50_variacao
      FROM skat
     WHERE spras EQ sy-langu
       AND ktopl EQ wa_t001_0304-ktopl
       AND saknr EQ zde_mov_lct_banco-saknr_variacao.
  ENDIF.

  "Verifica Chave de Lançamento no Banco
  IF zde_mov_lct_banco-dmbtr GT 0.  "Crédito na Conta de Contra Partida
    ck_deb_cred = 'H'.
  ELSEIF zde_mov_lct_banco-dmbtr LT 0."Débito na Conta de Contra Partida
    ck_deb_cred = 'S'.
  ENDIF.

  SET PARAMETER ID: 'ZBANK_SAKNR' FIELD zde_saldo_cta_banco-saknr,
                    'ZDEB_CRED'   FIELD ck_deb_cred,
                    'ZTP_LCTO'    FIELD zde_mov_lct_banco-tp_lcto,
                    'BUK'         FIELD zde_saldo_cta_banco-bukrs.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'G1' OR screen-group1 EQ 'G2'.
      screen-required = 0.
      screen-input    = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF zde_mov_lct_banco-tcurr IS NOT INITIAL AND zde_mov_lct_banco-tcurr NE zde_mov_lct_banco-fcurr.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'G1'.
        screen-required = 1.
        screen-input    = 1.
        MODIFY SCREEN.
      ENDIF.
      IF zde_mov_lct_banco-saknr_variacao IS NOT INITIAL.
        IF screen-group1 EQ 'G2'.
          screen-input    = 1.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 EQ 'G2'.
          screen-input    = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0305  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0305_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0305_exit INPUT.
  CLEAR: zde_mov_lct_banco.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0305_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0305  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0305 INPUT.
  CASE ok_code.
    WHEN 'CONFIRMAR'.

      CHECK zde_mov_lct_banco-tcurr IS NOT INITIAL.

      CHECK zde_mov_lct_banco-txt50 IS NOT INITIAL.

      SELECT SINGLE * INTO wa_aux
        FROM zsaldo_cta_moeda
       WHERE bukrs EQ zde_mov_lct_banco-bukrs
         AND saknr EQ zde_mov_lct_banco-saknr.

      IF wa_aux-waers EQ zde_mov_lct_banco-fcurr.
        CLEAR: zde_mov_lct_banco-ukurs.
      ENDIF.

      IF ( wa_aux-waers NE zde_mov_lct_banco-fcurr ) AND ( zde_mov_lct_banco-ukurs IS INITIAL ).
        MESSAGE s043 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF zde_mov_lct_banco-tcurr NE zde_mov_lct_banco-fcurr AND zde_mov_lct_banco-saknr_variacao IS NOT INITIAL AND
         ( zde_mov_lct_banco-ukurs_variacao EQ 0 OR zde_mov_lct_banco-dmbe2_variacaoc EQ 0 ).
        MESSAGE s055 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CHECK zde_mov_lct_banco-dmbe2 IS NOT INITIAL.

      CLEAR: ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0305  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_update_flag INPUT.
  CLEAR: zde_mov_lct_banco-tcurr, zde_mov_lct_banco-txt50, zde_mov_lct_banco-ukurs_variacao.
ENDMODULE.                 " SET_UPDATE_FLAG  INPUT

*&---------------------------------------------------------------------*
*&      Module  VERIFICA_CONTA_BANCO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verifica_conta_banco INPUT.

  "Verifica Chave de Lançamento no Banco
  IF zde_mov_lct_banco-dmbtr GT 0. "Crédito na Conta Banco Contra Partida
    ck_deb_cred = 'H'.
  ELSEIF zde_mov_lct_banco-dmbtr LT 0. "Débito na Conta Banco Contra Partida
    ck_deb_cred = 'S'.
  ENDIF.

  "Verifica se conta do banco está parametrizada
  ck_conta_banco = abap_false.
  LOOP AT it_cta_banc WHERE bukrs EQ zde_mov_lct_banco-bukrs
                        AND saknr EQ zde_mov_lct_banco-saknr.

    LOOP AT it_zglt032 WHERE tp_lcto = zde_mov_lct_banco-tp_lcto AND hkont = it_cta_banc-saknr AND shkzg EQ ck_deb_cred.
      ck_conta_banco = abap_true.
    ENDLOOP.
  ENDLOOP.

  IF ck_conta_banco EQ abap_false.
    MESSAGE e044 WITH zde_mov_lct_banco-saknr zde_mov_lct_banco-tp_lcto.
  ENDIF.

ENDMODULE.                 " VERIFICA_CONTA_BANCO  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0304 INPUT.

  DATA: e_valid	TYPE char01.

  CASE ok_code.
    WHEN 'CONFIRMAR'.

      CALL METHOD ctl_alv_0304->check_changed_data
        IMPORTING
          e_valid = e_valid.

      CHECK e_valid EQ abap_true.
      REFRESH it_zglt036_comp.
      IF  tp_comp = 'D'.

        MOVE-CORRESPONDING it_mov_lct_banco[] to it_mov_lct_banco_aux[].
        PERFORM gerar_lote_contabil TABLES it_mov_lct_banco_aux[]  it_zglt036_comp
                                    USING zde_saldo_cta_banco.
        PERFORM gerar_compensacao_fornecedor TABLES it_zglt036_comp.
        CLEAR tp_comp .
      ELSE.
        MOVE-CORRESPONDING it_mov_lct_banco[] to it_mov_lct_banco_aux[].
        PERFORM gerar_lote_contabil TABLES it_mov_lct_banco_aux[] it_zglt036_comp
                                    USING zde_saldo_cta_banco.

      ENDIF.
      CLEAR: ok_code.
      CLEAR sd_compen.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0304  INPUT

*&---------------------------------------------------------------------*
*&      Form  GERAR_LOTE_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gerar_lote_contabil  TABLES   p_mov_lct_banco STRUCTURE zde_mov_lct_banco
                                   p_it_zglt036_comp STRUCTURE zglt036
                          USING    p_cta_banco     TYPE zde_saldo_cta_banco.

  DATA: it_zglt031     TYPE TABLE OF zglt031 WITH HEADER LINE,
        it_zglt031_dep TYPE TABLE OF zglt031 WITH HEADER LINE.
  DATA: i_descr_lote TYPE zdescr_lote,
        e_num_lote   TYPE zlote_num,
        i_dep_resp   TYPE char2,
        it_mov_banco TYPE TABLE OF zde_mov_lct_banco WITH HEADER LINE,
        wa_mov_banco TYPE zde_mov_lct_banco,
        it_zglt036   TYPE TABLE OF zglt036,
        wa_zglt036   TYPE zglt036,
        wa_zglt035   TYPE zglt035,
        lc_seqitem   TYPE n LENGTH 6,
        it_zglt032   TYPE TABLE OF zglt032 WITH HEADER LINE,
        it_tbsl      TYPE TABLE OF tbsl WITH HEADER LINE,
        wa_tbsl_var  TYPE tbsl,
        wa_t001      TYPE t001,
        wa_moedas    TYPE x001,
        e_num_doc	   TYPE num10,
        lc_variacao	 TYPE zde_payments_1.

  DATA: lc_vl_doc_1 TYPE zde_payments_1,
        lc_vl_int_1 TYPE zde_payments_1,
        lc_vl_for_1 TYPE zde_payments_1,
        lc_vl_grp_1 TYPE zde_payments_1,
        lc_vl_doc_2 TYPE zde_payments_1,
        lc_vl_int_2 TYPE zde_payments_1,
        lc_vl_for_2 TYPE zde_payments_1,
        lc_vl_grp_2 TYPE zde_payments_1,
        lc_vl_doc_3 TYPE zde_payments_1,
        lc_vl_int_3 TYPE zde_payments_1,
        lc_vl_for_3 TYPE zde_payments_1,
        lc_vl_grp_3 TYPE zde_payments_1.

  DATA: it_zglt036_flg TYPE zde_zglt036_flg_t,
        wa_zglt036_flg TYPE zde_zglt036_flg.

  DATA: obj_cotacao TYPE REF TO zcl_util_sd,
        lc_ukurs    TYPE ukurs_curr,
        lc_data     TYPE gdatu_inv,
        lc_shkzg    TYPE tbsl-shkzg.

  CLEAR: it_zglt036[], it_zglt036.

  DELETE p_mov_lct_banco WHERE dmbtr EQ 0.
  DELETE p_mov_lct_banco WHERE tp_lcto EQ space.

  CHECK p_mov_lct_banco[] IS NOT INITIAL.


  SELECT * INTO TABLE it_zglt031
    FROM zglt031
     FOR ALL ENTRIES IN p_mov_lct_banco
   WHERE tp_lcto EQ p_mov_lct_banco-tp_lcto.

  CHECK it_zglt031[] IS NOT INITIAL.

  MOVE it_zglt031[] TO it_zglt031_dep[].
  SORT it_zglt031_dep BY dpto_resp blart.
  DELETE ADJACENT DUPLICATES FROM it_zglt031_dep COMPARING dpto_resp blart.

  "Separa moedas transferência P_MOV_LCT_BANCO

  LOOP AT p_mov_lct_banco.
    MOVE-CORRESPONDING p_mov_lct_banco TO it_mov_banco.
    APPEND it_mov_banco.
  ENDLOOP.

  SORT it_mov_banco BY bukrs tp_lcto fcurr tcurr.
  DELETE ADJACENT DUPLICATES FROM it_mov_banco COMPARING tp_lcto tcurr.

  SELECT * INTO TABLE it_zglt032
    FROM zglt032
     FOR ALL ENTRIES IN p_mov_lct_banco
   WHERE tp_lcto EQ p_mov_lct_banco-tp_lcto
     AND bschl   EQ p_mov_lct_banco-bschl.

  SELECT * APPENDING TABLE it_zglt032
    FROM zglt032
     FOR ALL ENTRIES IN p_mov_lct_banco
   WHERE tp_lcto EQ p_mov_lct_banco-tp_lcto
     AND bschl   EQ p_mov_lct_banco-bschl_banco.

  "SORT IT_ZGLT032 BY TP_LCTO BSCHL HKONT.

  IF it_zglt032[] IS NOT INITIAL.
    SELECT * INTO TABLE it_tbsl
      FROM tbsl
       FOR ALL ENTRIES IN it_zglt032
     WHERE bschl EQ it_zglt032-bschl.
    SORT it_tbsl BY bschl.
  ENDIF.

  SELECT SINGLE * INTO wa_aux
    FROM zsaldo_cta_moeda
   WHERE bukrs EQ p_cta_banco-bukrs
     AND saknr EQ p_cta_banco-saknr.

  SELECT SINGLE bukrs waers INTO (wa_t001-bukrs,wa_t001-waers)
    FROM t001
   WHERE bukrs EQ p_cta_banco-bukrs.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs = p_cta_banco-bukrs
    IMPORTING
      e_x001  = wa_moedas.

  CREATE OBJECT obj_cotacao.

  "Departamentos e Tipo de Documento Contábil
  LOOP AT it_zglt031_dep WHERE st_aprova EQ abap_false.

    CONCATENATE p_cta_banco-budat+6(2) '-' p_cta_banco-budat+4(2) '-' p_cta_banco-budat(4) INTO i_descr_lote.
    CONCATENATE TEXT-007 i_descr_lote INTO i_descr_lote SEPARATED BY space.

    MOVE it_zglt031_dep-dpto_resp TO i_dep_resp.

    IF tp_comp NE 'D'.
      CALL METHOD zcl_gerar_lote=>create_lote
        EXPORTING
          i_bukrs       = p_cta_banco-bukrs
          i_descr_lote  = i_descr_lote
          i_dep_resp    = i_dep_resp
          i_user_resp   = sy-uname
          i_status_lote = 'L'
        IMPORTING
          e_num_lote    = e_num_lote.
    ENDIF.

    "Tipo de Lançamentos de Lote de Documento Contabil
    LOOP AT it_zglt031 WHERE dpto_resp EQ it_zglt031_dep-dpto_resp
                         AND blart     EQ it_zglt031_dep-blart.

      "Moedas - Tipo de Lançamentos de Lote de Documento Contabil (Cabeçalho)
      "Quebra por Empresa/Tipo Lançamento/Moeda Origem/Moeda Destino
      LOOP AT it_mov_banco WHERE tp_lcto EQ it_zglt031-tp_lcto.

        CLEAR: wa_zglt035.
        wa_zglt035-tp_lcto   = it_mov_banco-tp_lcto.
        wa_zglt035-lote      = e_num_lote.
        wa_zglt035-bukrs     = it_mov_banco-bukrs.

        IF NOT ( it_mov_banco-tcurr EQ wa_t001-waers   OR
                 it_mov_banco-tcurr EQ wa_moedas-hwae2 OR
                 it_mov_banco-tcurr EQ wa_moedas-hwae3 )
           OR
           NOT ( it_mov_banco-fcurr EQ wa_t001-waers   OR
                 it_mov_banco-fcurr EQ wa_moedas-hwae2 OR
                 it_mov_banco-fcurr EQ wa_moedas-hwae3 ).

          IF NOT ( it_mov_banco-tcurr EQ wa_t001-waers   OR
                 it_mov_banco-tcurr EQ wa_moedas-hwae2 OR
                 it_mov_banco-tcurr EQ wa_moedas-hwae3 ).
            wa_zglt035-moeda_doc = it_mov_banco-tcurr.
          ELSE.
            wa_zglt035-moeda_doc = it_mov_banco-fcurr.
          ENDIF.
        ELSE.
          wa_zglt035-moeda_doc = it_mov_banco-fcurr.
        ENDIF.

*        "Alterei pra sempre pegar a moeda  do banco 17.03.2021
*        wa_zglt035-moeda_doc = zde_saldo_cta_banco-waers.

        wa_zglt035-dpto_resp = it_zglt031-dpto_resp.
        wa_zglt035-blart     = it_zglt031-blart.

        wa_zglt035-bldat     = p_cta_banco-budat.
        wa_zglt035-budat     = p_cta_banco-budat.
        wa_zglt035-dt_lcto   = sy-datum.

        IF it_mov_banco-tcurr IS NOT INITIAL AND it_mov_banco-tcurr NE p_cta_banco-waers.
          wa_zglt035-taxa = wa_mov_banco-ukurs.
        ENDIF.

        wa_zglt035-moeda_interna = wa_t001-waers.   "Código da moeda
        wa_zglt035-moeda_forte   = wa_moedas-hwae2. "Código da moeda
        wa_zglt035-moeda_grupo   = wa_moedas-hwae3. "Código da moeda

        "XBLNR               -- Nº documento de referência
        CONCATENATE p_cta_banco-budat+6(2) '-' p_cta_banco-budat+4(2) '-' p_cta_banco-budat(4) INTO i_descr_lote.
        CONCATENATE TEXT-007 i_descr_lote INTO i_descr_lote SEPARATED BY space.
        MOVE i_descr_lote TO wa_zglt035-bktxt. "              -- Texto de cabeçalho de documento

        "Movimentos Lançados com Tipo de Movimento do Mesmo departamento e tipo de documento
        " (Itens do Documento)
        CLEAR: it_zglt036.
        lc_seqitem = 1.

        LOOP AT p_mov_lct_banco INTO wa_mov_banco
           WHERE bukrs   EQ it_mov_banco-bukrs
             AND tp_lcto EQ it_mov_banco-tp_lcto
             AND fcurr   EQ it_mov_banco-fcurr
             AND tcurr   EQ it_mov_banco-tcurr.

          IF NOT ( ( wa_mov_banco-dmbe2_variacao NE 0 ) AND ( it_zglt031-st_trans_banc EQ abap_true ) ).
            CLEAR: it_zglt031-st_trans_banc.
          ENDIF.
          IF tp_comp EQ 'D'.
            it_zglt031-st_trans_banc = 'X'.
          ENDIF.

          CASE it_zglt031-st_trans_banc .
            WHEN abap_true.
              " Ini Tranferência Bancária """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              lc_variacao = wa_mov_banco-dmbe2_variacao.

              " Chave da Variação Cambial """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
              IF wa_mov_banco-bschl_variacao IS NOT INITIAL.
                READ TABLE it_tbsl INTO wa_tbsl_var WITH KEY bschl = wa_mov_banco-bschl_variacao BINARY SEARCH.
              ENDIF.

              "Item 1 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra o Banco """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl_banco hkont = p_cta_banco-saknr.
              READ TABLE it_tbsl WITH KEY bschl = wa_mov_banco-bschl_banco BINARY SEARCH.

              CLEAR: wa_zglt036.
              wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
              wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
              wa_zglt036-bschl      = wa_mov_banco-bschl_banco.  "Chave de lançamento
              wa_zglt036-hkont      = p_cta_banco-saknr.   "Conta do Razão da contabilidade geral
              IF ( wa_mov_banco-umskz IS INITIAL ).
                wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
              ELSE.
                IF ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ).
                  wa_zglt036-umskz  = wa_mov_banco-umskz.  "Código de Razão Especial
                ELSE.
                  wa_zglt036-umskz  = it_zglt032-umskz.    "Código de Razão Especial
                ENDIF.
              ENDIF.
              wa_zglt036-vbund      = it_zglt032-vbund.    "Nº sociedade parceira
              wa_zglt036-kostl      = it_zglt032-kostl.    "Centro de custo
              wa_zglt036-prctr      = it_zglt032-prctr.    "Centro de lucro
              wa_zglt036-aufnr      = it_zglt032-aufnr.    "Nº ordem
              wa_zglt036-matnr      = it_zglt032-matnr.    "Nº do material
              wa_zglt036-matnr_fi   = it_zglt032-matnr_fi. "Nº do material
              wa_zglt036-zuonr      = it_zglt032-zuonr.    "Nº atribuição

              IF wa_mov_banco-sgtxt IS INITIAL.
                wa_zglt036-sgtxt    = it_zglt032-sgtxt."Texto do item
              ELSE.
                wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
              ENDIF.
              wa_zglt036-gsber         = wa_aux-gsber. "Divisão

              IF wa_zglt035-moeda_doc EQ it_mov_banco-fcurr.
                wa_zglt036-vlr_moeda_doc = abs( wa_mov_banco-dmbtr )."Valor Moeda Documento
              ELSE.
                wa_zglt036-vlr_moeda_doc = abs( wa_mov_banco-dmbe2 )."Valor Moeda Documento
              ENDIF.

              "LC_DATA = WA_ZGLT035-DT_LCTO.
              "Data da Ficha
              lc_data = p_cta_banco-budat.

              CASE wa_mov_banco-tcurr.
                WHEN wa_t001-waers.
                  wa_zglt036-vlr_moeda_int   = wa_mov_banco-dmbe2.
                  IF wa_zglt035-moeda_doc = 'GBP'.
                    lc_data = p_cta_banco-budat.
                    obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                    wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
                  ELSEIF wa_zglt035-moeda_doc EQ wa_moedas-hwae2.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae2 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_forte = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae2 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                  IF wa_zglt035-moeda_doc EQ wa_moedas-hwae3.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae3 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_grupo = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae3 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                WHEN wa_moedas-hwae2.
                  wa_zglt036-vlr_moeda_forte = wa_mov_banco-dmbe2.

                  IF wa_zglt035-moeda_doc EQ wa_t001-waers.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_t001-waers EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_int = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_moedas-hwae2 ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_t001-waers ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_forte / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_forte * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                  IF wa_zglt035-moeda_doc EQ wa_moedas-hwae3.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae3 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_grupo = wa_mov_banco-dmbtr.
                    ELSE.
                      "LC_DATA = WA_ZGLT035-DT_LCTO.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae3 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                WHEN wa_moedas-hwae3.
                  wa_zglt036-vlr_moeda_grupo = wa_mov_banco-dmbe2.

                  IF wa_zglt035-moeda_doc EQ wa_t001-waers.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_t001-waers EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_int = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_moedas-hwae3 ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_t001-waers ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_grupo / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_grupo * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                  IF wa_zglt035-moeda_doc EQ wa_moedas-hwae2.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae2 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_forte = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae2 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                WHEN OTHERS.

                  IF wa_zglt035-moeda_doc EQ wa_t001-waers.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_t001-waers EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_int = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_zglt035-moeda_doc ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_t001-waers ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                  IF wa_zglt035-moeda_doc = 'GBP'.
                    lc_data = p_cta_banco-budat.
                    obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                    wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
                  ELSEIF wa_zglt035-moeda_doc EQ wa_moedas-hwae2.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae2 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_forte = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae2 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                  IF wa_zglt035-moeda_doc EQ wa_moedas-hwae3.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae3 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_grupo = wa_mov_banco-dmbtr.
                    ELSE.
                      "LC_DATA = WA_ZGLT035-DT_LCTO.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae3 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.
              ENDCASE.

              wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA
              wa_zglt036-valut      = p_cta_banco-budat.

              IF wa_zglt035-moeda_doc EQ it_mov_banco-fcurr.
                lc_vl_doc_1 = wa_mov_banco-dmbtr."Valor Moeda Documento
              ELSE.
                lc_vl_doc_1 = wa_mov_banco-dmbe2."Valor Moeda Documento
              ENDIF.

              "LC_VL_DOC_1 = WA_ZGLT036-VLR_MOEDA_DOC.
              lc_vl_int_1 = wa_zglt036-vlr_moeda_int.
              lc_vl_for_1 = wa_zglt036-vlr_moeda_forte.
              lc_vl_grp_1 = wa_zglt036-vlr_moeda_grupo.

              wa_zglt036-vlr_moeda_doc    = abs( wa_zglt036-vlr_moeda_doc   ).
              wa_zglt036-vlr_moeda_int    = abs( wa_zglt036-vlr_moeda_int   ).
              wa_zglt036-vlr_moeda_forte  = abs( wa_zglt036-vlr_moeda_forte ).
              wa_zglt036-vlr_moeda_grupo  = abs( wa_zglt036-vlr_moeda_grupo ).
              APPEND wa_zglt036 TO it_zglt036.

              " Fim Item 1 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              "Item 2 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra de Contrapartida """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              ADD 1 TO lc_seqitem.
              READ TABLE it_tbsl WITH KEY bschl = wa_mov_banco-bschl BINARY SEARCH.

              READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl hkont = wa_mov_banco-saknr.
              IF ( sy-subrc IS NOT INITIAL ) AND ( ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ) ).
                READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl.
              ENDIF.

              CLEAR: wa_zglt036.
              wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
              wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
              wa_zglt036-bschl      = wa_mov_banco-bschl. "Chave de lançamento
              wa_zglt036-hkont      = wa_mov_banco-saknr."Conta do Razão da contabilidade geral
              IF ( wa_mov_banco-umskz IS INITIAL ).
                wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
              ELSE.
                IF ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ).
                  wa_zglt036-umskz    = wa_mov_banco-umskz.  "Código de Razão Especial
                ELSE.
                  wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
                ENDIF.
              ENDIF.
              wa_zglt036-vbund      = it_zglt032-vbund.         "Nº sociedade parceira
              wa_zglt036-kostl      = it_zglt032-kostl.         "Centro de custo
              wa_zglt036-prctr      = it_zglt032-prctr.         "Centro de lucro
              wa_zglt036-aufnr      = it_zglt032-aufnr.         "Nº ordem
              wa_zglt036-matnr      = it_zglt032-matnr.         "Nº do material
              wa_zglt036-matnr_fi   = it_zglt032-matnr_fi.      "Nº do material
              wa_zglt036-zuonr      = it_zglt032-zuonr.         "Nº atribuição

              IF wa_mov_banco-sgtxt IS INITIAL.
                wa_zglt036-sgtxt    = it_zglt032-sgtxt.  "Texto do item
              ELSE.
                wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
              ENDIF.
              wa_zglt036-gsber         = wa_aux-gsber. "Divisão

              CASE wa_mov_banco-tcurr.
                WHEN wa_t001-waers.    "EUR

                  wa_zglt036-vlr_moeda_int = lc_vl_int_1 - lc_variacao.

                  obj_cotacao->set_data(  EXPORTING i_data    = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae2 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

                  obj_cotacao->set_data(  EXPORTING i_data    = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae3 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

                  IF wa_t001-waers EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int.
                  ELSEIF wa_moedas-hwae2 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_forte.
                  ELSEIF wa_moedas-hwae3 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_grupo.
                  ELSE.
                    obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_zglt035-moeda_doc ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                    IF lc_ukurs LT 0.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int / lc_ukurs.
                    ELSE.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int * lc_ukurs.
                    ENDIF.
                  ENDIF.

                WHEN wa_moedas-hwae2.  "USD -- CONVERTER

                  wa_zglt036-vlr_moeda_forte = lc_vl_for_1 - lc_variacao.

                  obj_cotacao->set_data(  EXPORTING i_data    = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_moedas-hwae2 ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_t001-waers ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_forte / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_forte * lc_ukurs.
                  ENDIF.

                  obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae3 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

                  IF wa_t001-waers EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int.
                  ELSEIF wa_moedas-hwae2 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_forte.
                  ELSEIF wa_moedas-hwae3 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_grupo.
                  ELSE.
                    obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_zglt035-moeda_doc ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

                    IF lc_ukurs LT 0.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int / lc_ukurs.
                    ELSE.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int * lc_ukurs.
                    ENDIF.
                  ENDIF.

                WHEN wa_moedas-hwae3.  "USD -- CONVERTER

                  wa_zglt036-vlr_moeda_grupo = lc_vl_grp_1 - lc_variacao.

                  obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_moedas-hwae3 ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_t001-waers ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_grupo / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_grupo * lc_ukurs.
                  ENDIF.

                  obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae2 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

                  IF wa_t001-waers EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int.
                  ELSEIF wa_moedas-hwae2 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_forte.
                  ELSEIF wa_moedas-hwae3 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_grupo.
                  ELSE.
                    obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_zglt035-moeda_doc ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

                    IF lc_ukurs LT 0.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int / lc_ukurs.
                    ELSE.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int * lc_ukurs.
                    ENDIF.
                  ENDIF.

                WHEN OTHERS.

                  wa_zglt036-vlr_moeda_doc = lc_vl_doc_1 - lc_variacao.

                  IF wa_t001-waers EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_zglt035-moeda_doc ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_t001-waers ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

                    IF lc_ukurs LT 0.
                      wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc / lc_ukurs.
                    ELSE.
                      wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc * lc_ukurs.
                    ENDIF.
                  ENDIF.

                  obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae2 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

                  IF wa_zglt035-moeda_doc = 'GBP'.
                    lc_data = p_cta_banco-budat.
                    obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                    wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
                  ENDIF.

                  obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae3 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

              ENDCASE.

              wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA

              READ TABLE it_cta_banc WITH KEY saknr = wa_zglt036-hkont BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wa_zglt036-valut = p_cta_banco-budat.
              ENDIF.

              lc_vl_doc_2 = wa_zglt036-vlr_moeda_doc.
              lc_vl_int_2 = wa_zglt036-vlr_moeda_int.
              lc_vl_for_2 = wa_zglt036-vlr_moeda_forte.
              lc_vl_grp_2 = wa_zglt036-vlr_moeda_grupo.

              wa_zglt036-vlr_moeda_doc    = abs( wa_zglt036-vlr_moeda_doc   ).
              wa_zglt036-vlr_moeda_int    = abs( wa_zglt036-vlr_moeda_int   ).
              wa_zglt036-vlr_moeda_forte  = abs( wa_zglt036-vlr_moeda_forte ).
              wa_zglt036-vlr_moeda_grupo  = abs( wa_zglt036-vlr_moeda_grupo ).

              APPEND wa_zglt036 TO it_zglt036.
              " Fim Item 2 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              ADD 1 TO lc_seqitem.

              "Item 3 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra de Variação Cambial """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "IF LC_VARIACAO NE 0.
              READ TABLE it_tbsl WITH KEY bschl = wa_mov_banco-bschl_variacao BINARY SEARCH.

              READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl_variacao hkont = wa_mov_banco-saknr_variacao.
              IF ( sy-subrc IS NOT INITIAL ) AND ( ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ) ).
                READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl.
              ENDIF.

              CLEAR: wa_zglt036.
              wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
              wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
              wa_zglt036-bschl      = wa_mov_banco-bschl_variacao. "Chave de lançamento
              wa_zglt036-hkont      = wa_mov_banco-saknr_variacao. "Conta do Razão da contabilidade geral
              wa_zglt036-umskz      = it_zglt032-umskz.            "Código de Razão Especial
              wa_zglt036-vbund      = it_zglt032-vbund.         "Nº sociedade parceira
              wa_zglt036-kostl      = it_zglt032-kostl.         "Centro de custo
              wa_zglt036-prctr      = it_zglt032-prctr.         "Centro de lucro
              wa_zglt036-aufnr      = it_zglt032-aufnr.         "Nº ordem
              wa_zglt036-matnr      = it_zglt032-matnr.         "Nº do material
              wa_zglt036-matnr_fi   = it_zglt032-matnr_fi.      "Nº do material
              wa_zglt036-zuonr      = it_zglt032-zuonr.         "Nº atribuição

              IF wa_mov_banco-sgtxt IS INITIAL.
                wa_zglt036-sgtxt    = it_zglt032-sgtxt.  "Texto do item
              ELSE.
                wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
              ENDIF.

              wa_zglt036-gsber         = wa_aux-gsber. "Divisão

              lc_vl_doc_3 = abs( lc_vl_doc_1 ) - abs( lc_vl_doc_2 ).
              lc_vl_int_3 = abs( lc_vl_int_1 ) - abs( lc_vl_int_2 ).
              lc_vl_for_3 = abs( lc_vl_for_1 ) - abs( lc_vl_for_2 ).
              lc_vl_grp_3 = abs( lc_vl_grp_1 ) - abs( lc_vl_grp_2 ).

              "H  Crédito
              "S  Débito
              lc_shkzg = it_tbsl-shkzg.

              IF wa_mov_banco-dmbe2 GT 0.
                IF it_tbsl-shkzg EQ 'S'.
                  IF lc_vl_doc_3 LE 0.
                    wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                    CLEAR: lc_vl_doc_3.
                  ENDIF.
                  IF lc_vl_int_3 LE 0.
                    wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                    CLEAR: lc_vl_int_3.
                  ENDIF.
                  IF lc_vl_for_3 LE 0.
                    wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                    CLEAR: lc_vl_for_3.
                  ENDIF.
                  IF lc_vl_grp_3 LE 0.
                    wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                    CLEAR: lc_vl_grp_3.
                  ENDIF.
                ELSE.
                  IF lc_vl_doc_3 GT 0.
                    wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                    CLEAR: lc_vl_doc_3.
                  ENDIF.
                  IF lc_vl_int_3 GT 0.
                    wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                    CLEAR: lc_vl_int_3.
                  ENDIF.
                  IF lc_vl_for_3 GT 0.
                    wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                    CLEAR: lc_vl_for_3.
                  ENDIF.
                  IF lc_vl_grp_3 GT 0.
                    wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                    CLEAR: lc_vl_grp_3.
                  ENDIF.
                ENDIF.
              ELSE.
                IF it_tbsl-shkzg EQ 'H'.
                  IF lc_vl_doc_3 LE 0.
                    wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                    CLEAR: lc_vl_doc_3.
                  ENDIF.
                  IF lc_vl_int_3 LE 0.
                    wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                    CLEAR: lc_vl_int_3.
                  ENDIF.
                  IF lc_vl_for_3 LE 0.
                    wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                    CLEAR: lc_vl_for_3.
                  ENDIF.
                  IF lc_vl_grp_3 LE 0.
                    wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                    CLEAR: lc_vl_grp_3.
                  ENDIF.
                ELSE.
                  IF lc_vl_doc_3 GT 0.
                    wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                    CLEAR: lc_vl_doc_3.
                  ENDIF.
                  IF lc_vl_int_3 GT 0.
                    wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                    CLEAR: lc_vl_int_3.
                  ENDIF.
                  IF lc_vl_for_3 GT 0.
                    wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                    CLEAR: lc_vl_for_3.
                  ENDIF.
                  IF lc_vl_grp_3 GT 0.
                    wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                    CLEAR: lc_vl_grp_3.
                  ENDIF.
                ENDIF.
              ENDIF.

*              IF wa_zglt035-moeda_doc = 'GBP'.
*                lc_data = p_cta_banco-budat.
*                obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
*                obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
*                obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
*                obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
*                obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
*                wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
*              ENDIF.

              wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA
              APPEND wa_zglt036 TO it_zglt036.

              wa_zglt036_flg-doc_lcto        = wa_zglt036-doc_lcto.
              wa_zglt036_flg-seqitem         = wa_zglt036-seqitem.
              wa_zglt036_flg-seqsub          = wa_zglt036-seqsub.
              wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
              wa_zglt036_flg-fl_cv_moeda_int = abap_true.
              wa_zglt036_flg-fl_cv_moeda_for = abap_true.
              wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
              APPEND  wa_zglt036_flg TO it_zglt036_flg.

              ADD 1 TO lc_seqitem.

              "Item 4 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra de Variação Cambial """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
              IF lc_vl_doc_3 IS NOT INITIAL OR
                 lc_vl_int_3 IS NOT INITIAL OR
                 lc_vl_for_3 IS NOT INITIAL OR
                 lc_vl_grp_3 IS NOT INITIAL.

                CLEAR: wa_zglt036.

                LOOP AT it_zglt032 WHERE tp_lcto = wa_mov_banco-tp_lcto.
                  READ TABLE it_cta_banc WITH KEY saknr = it_zglt032-hkont BINARY SEARCH.
                  IF sy-subrc IS NOT INITIAL.
                    READ TABLE it_ska1 WITH KEY saknr = it_zglt032-hkont BINARY SEARCH.
                    IF it_ska1-xbilk IS INITIAL.
                      READ TABLE it_tbsl WITH KEY bschl = it_zglt032-bschl BINARY SEARCH.
                      IF sy-subrc IS INITIAL.
                        IF it_tbsl-shkzg <> lc_shkzg.
                          wa_zglt036-hkont = it_zglt032-hkont.
                          wa_zglt036-bschl = it_zglt032-bschl.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDLOOP.

                READ TABLE it_tbsl WITH KEY bschl = wa_zglt036-bschl BINARY SEARCH.

                wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
                wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
                wa_zglt036-umskz      = it_zglt032-umskz.            "Código de Razão Especial
                wa_zglt036-vbund      = it_zglt032-vbund.         "Nº sociedade parceira
                wa_zglt036-kostl      = it_zglt032-kostl.         "Centro de custo
                wa_zglt036-prctr      = it_zglt032-prctr.         "Centro de lucro
                wa_zglt036-aufnr      = it_zglt032-aufnr.         "Nº ordem
                wa_zglt036-matnr      = it_zglt032-matnr.         "Nº do material
                wa_zglt036-matnr_fi   = it_zglt032-matnr_fi.      "Nº do material
                wa_zglt036-zuonr      = it_zglt032-zuonr.         "Nº atribuição

                IF wa_mov_banco-sgtxt IS INITIAL.
                  wa_zglt036-sgtxt    = it_zglt032-sgtxt.  "Texto do item
                ELSE.
                  wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
                ENDIF.

                wa_zglt036-gsber      = wa_aux-gsber. "Divisão

                "H  Crédito
                "S  Débito
                IF wa_mov_banco-dmbe2 GT 0.
                  IF it_tbsl-shkzg EQ 'S'.
                    IF lc_vl_doc_3 LE 0.
                      wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                      CLEAR: lc_vl_doc_3.
                    ENDIF.
                    IF lc_vl_int_3 LE 0.
                      wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                      CLEAR: lc_vl_int_3.
                    ENDIF.
                    IF lc_vl_for_3 LE 0.
                      wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                      CLEAR: lc_vl_for_3.
                    ENDIF.
                    IF lc_vl_grp_3 LE 0.
                      wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                      CLEAR: lc_vl_grp_3.
                    ENDIF.
                  ELSE.
                    IF lc_vl_doc_3 GT 0.
                      wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                      CLEAR: lc_vl_doc_3.
                    ENDIF.
                    IF lc_vl_int_3 GT 0.
                      wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                      CLEAR: lc_vl_int_3.
                    ENDIF.
                    IF lc_vl_for_3 GT 0.
                      wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                      CLEAR: lc_vl_for_3.
                    ENDIF.
                    IF lc_vl_grp_3 GT 0.
                      wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                      CLEAR: lc_vl_grp_3.
                    ENDIF.
                  ENDIF.
                ELSE.
                  IF it_tbsl-shkzg EQ 'H'.
                    IF lc_vl_doc_3 LE 0.
                      wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                      CLEAR: lc_vl_doc_3.
                    ENDIF.
                    IF lc_vl_int_3 LE 0.
                      wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                      CLEAR: lc_vl_int_3.
                    ENDIF.
                    IF lc_vl_for_3 LE 0.
                      wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                      CLEAR: lc_vl_for_3.
                    ENDIF.
                    IF lc_vl_grp_3 LE 0.
                      wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                      CLEAR: lc_vl_grp_3.
                    ENDIF.
                  ELSE.
                    IF lc_vl_doc_3 GT 0.
                      wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                      CLEAR: lc_vl_doc_3.
                    ENDIF.
                    IF lc_vl_int_3 GT 0.
                      wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                      CLEAR: lc_vl_int_3.
                    ENDIF.
                    IF lc_vl_for_3 GT 0.
                      wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                      CLEAR: lc_vl_for_3.
                    ENDIF.
                    IF lc_vl_grp_3 GT 0.
                      wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                      CLEAR: lc_vl_grp_3.
                    ENDIF.
                  ENDIF.
                ENDIF.

                IF wa_zglt035-moeda_doc = 'GBP'.
                  lc_data = p_cta_banco-budat.
                  obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
                ENDIF.

                wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA
                APPEND wa_zglt036 TO it_zglt036.

                wa_zglt036_flg-doc_lcto        = wa_zglt036-doc_lcto.
                wa_zglt036_flg-seqitem         = wa_zglt036-seqitem.
                wa_zglt036_flg-seqsub          = wa_zglt036-seqsub.
                wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
                wa_zglt036_flg-fl_cv_moeda_int = abap_true.
                wa_zglt036_flg-fl_cv_moeda_for = abap_true.
                wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
                APPEND  wa_zglt036_flg TO it_zglt036_flg.
              ENDIF.

              "ENDIF.
              " Fim Tranferência Bancária """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            WHEN abap_false.
              " Ini Lançamento Normal """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              "Item 1 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra o Banco """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl_banco hkont   = p_cta_banco-saknr.
              READ TABLE it_tbsl WITH KEY bschl = wa_mov_banco-bschl_banco BINARY SEARCH.

              CLEAR: wa_zglt036.
              wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
              wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
              wa_zglt036-bschl      = wa_mov_banco-bschl_banco.  "Chave de lançamento
              wa_zglt036-hkont      = p_cta_banco-saknr.   "Conta do Razão da contabilidade geral
              IF ( wa_mov_banco-umskz IS INITIAL ).
                wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
              ELSE.
                IF ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ).
                  wa_zglt036-umskz  = wa_mov_banco-umskz.  "Código de Razão Especial
                ELSE.
                  wa_zglt036-umskz  = it_zglt032-umskz.    "Código de Razão Especial
                ENDIF.
              ENDIF.
              wa_zglt036-vbund      = it_zglt032-vbund.    "Nº sociedade parceira
              wa_zglt036-kostl      = it_zglt032-kostl.    "Centro de custo
              wa_zglt036-prctr      = it_zglt032-prctr.    "Centro de lucro
              wa_zglt036-aufnr      = it_zglt032-aufnr.    "Nº ordem
              wa_zglt036-matnr      = it_zglt032-matnr.    "Nº do material
              wa_zglt036-matnr_fi   = it_zglt032-matnr_fi. "Nº do material
              wa_zglt036-zuonr      = it_zglt032-zuonr.    "Nº atribuição

              IF wa_mov_banco-sgtxt IS INITIAL.
                wa_zglt036-sgtxt    = it_zglt032-sgtxt."Texto do item
              ELSE.
                wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
              ENDIF.
              wa_zglt036-gsber         = wa_aux-gsber. "Divisão
              wa_zglt036-vlr_moeda_doc = abs( wa_mov_banco-dmbtr )."Valor Moeda Documento

              IF wa_zglt035-moeda_doc = 'GBP'.
                lc_data = p_cta_banco-budat.
                obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
              ELSEIF wa_t001-waers EQ wa_mov_banco-tcurr.
                wa_zglt036-vlr_moeda_int = abs( wa_mov_banco-dmbe2 )."Valor Moeda Interna
              ELSE.
                CASE wa_mov_banco-tcurr.
                  WHEN wa_moedas-hwae2. "Moeda Forte
                    wa_zglt036-vlr_moeda_forte  = abs( wa_mov_banco-dmbe2 )."Valor Moeda forte
                  WHEN wa_moedas-hwae3. "Moeda Grupo
                    wa_zglt036-vlr_moeda_grupo  = abs( wa_mov_banco-dmbe2 )."Valor Moeda Grupo
                ENDCASE.
              ENDIF.
              wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA
              wa_zglt036-valut      = p_cta_banco-budat.
              IF wa_zglt035-moeda_doc NE 'GBP'.
                wa_zglt036-vlr_moeda_forte  = 0. "alrs
              ENDIF.
              APPEND wa_zglt036 TO it_zglt036.
              " Fim Item 1 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              "Item 2 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra de Contrapartida """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              ADD 1 TO lc_seqitem.

              READ TABLE it_tbsl WITH KEY bschl = wa_mov_banco-bschl BINARY SEARCH.

              READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl hkont = wa_mov_banco-saknr.
              IF ( sy-subrc IS NOT INITIAL ) AND ( ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ) ).
                READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl.
              ENDIF.

              CLEAR: wa_zglt036.
              wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
              wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
              wa_zglt036-bschl      = wa_mov_banco-bschl. "Chave de lançamento
              wa_zglt036-hkont      = wa_mov_banco-saknr."Conta do Razão da contabilidade geral
              IF ( wa_mov_banco-umskz IS INITIAL ).
                wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
              ELSE.
                IF ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ).
                  wa_zglt036-umskz    = wa_mov_banco-umskz.  "Código de Razão Especial
                ELSE.
                  wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
                ENDIF.
              ENDIF.
              wa_zglt036-vbund      = it_zglt032-vbund.         "Nº sociedade parceira
              wa_zglt036-kostl      = it_zglt032-kostl.         "Centro de custo
              wa_zglt036-prctr      = it_zglt032-prctr.         "Centro de lucro
              wa_zglt036-aufnr      = it_zglt032-aufnr.         "Nº ordem
              wa_zglt036-matnr      = it_zglt032-matnr.         "Nº do material
              wa_zglt036-matnr_fi   = it_zglt032-matnr_fi.      "Nº do material
              wa_zglt036-zuonr      = it_zglt032-zuonr.         "Nº atribuição

              IF wa_mov_banco-sgtxt IS INITIAL.
                wa_zglt036-sgtxt    = it_zglt032-sgtxt.  "Texto do item
              ELSE.
                wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
              ENDIF.
              wa_zglt036-gsber         = wa_aux-gsber. "Divisão
              wa_zglt036-vlr_moeda_doc = abs( wa_mov_banco-dmbtr )."Valor Moeda Documento

              IF wa_t001-waers EQ wa_mov_banco-tcurr.
                wa_zglt036-vlr_moeda_int = abs( wa_mov_banco-dmbe2 )."Valor Moeda Interna
              ELSE.
                CASE wa_mov_banco-tcurr.
                  WHEN wa_moedas-hwae2. "Moeda Forte
                    wa_zglt036-vlr_moeda_forte  = abs( wa_mov_banco-dmbe2 )."Valor Moeda forte
                  WHEN wa_moedas-hwae3. "Moeda Grupo
                    wa_zglt036-vlr_moeda_grupo  = abs( wa_mov_banco-dmbe2 )."Valor Moeda Grupo
                ENDCASE.
              ENDIF.
              IF wa_zglt035-moeda_doc = 'GBP'.
                lc_data = p_cta_banco-budat.
                obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
              ENDIF.
              wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA

              READ TABLE it_cta_banc WITH KEY saknr = wa_zglt036-hkont BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wa_zglt036-valut = p_cta_banco-budat.
              ENDIF.
              IF wa_zglt035-moeda_doc NE 'GBP'.
                wa_zglt036-vlr_moeda_forte  = 0. "alrs
              ENDIF.
              APPEND wa_zglt036 TO it_zglt036.
              " Fim Item 2 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              ADD 1 TO lc_seqitem.
              " Fim Lançamento Normal """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          ENDCASE.

        ENDLOOP.

        IF tp_comp = 'D'.
          LOOP AT it_zglt036 INTO wa_zglt036.
            IF wa_zglt036-bschl IS NOT INITIAL.
              APPEND wa_zglt036 TO p_it_zglt036_comp.
            ENDIF.
          ENDLOOP.
        ELSE.
          CALL METHOD zcl_gerar_lote=>contabilizar_lote
            EXPORTING
              i_zglt036_flg = it_zglt036_flg
            IMPORTING
              e_num_doc     = e_num_doc
            CHANGING
              i_zglt036     = it_zglt036
              i_zglt035     = wa_zglt035.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    IF tp_comp NE 'D'.
      SUBMIT z_grava_zib_zgl WITH p_lote = e_num_lote AND RETURN.
      MESSAGE s048 WITH e_num_lote.
    ENDIF.

  ENDLOOP.

  "Departamentos e Tipo de Documento Contábil
  LOOP AT it_zglt031_dep WHERE st_aprova EQ abap_true.

    CONCATENATE p_cta_banco-budat+6(2) '-' p_cta_banco-budat+4(2) '-' p_cta_banco-budat(4) INTO i_descr_lote.
    CONCATENATE TEXT-007 i_descr_lote INTO i_descr_lote SEPARATED BY space.

    MOVE it_zglt031_dep-dpto_resp TO i_dep_resp.
    IF tp_comp NE 'D'.
      CALL METHOD zcl_gerar_lote=>create_lote
        EXPORTING
          i_bukrs       = p_cta_banco-bukrs
          i_descr_lote  = i_descr_lote
          i_dep_resp    = i_dep_resp
          i_user_resp   = sy-uname
          i_status_lote = 'A'
        IMPORTING
          e_num_lote    = e_num_lote.
    ENDIF.

    "Tipo de Lançamentos de Lote de Documento Contabil
    LOOP AT it_zglt031 WHERE dpto_resp EQ it_zglt031_dep-dpto_resp
                         AND blart     EQ it_zglt031_dep-blart.

      "Moedas - Tipo de Lançamentos de Lote de Documento Contabil (Cabeçalho)
      "Quebra por Empresa/Tipo Lançamento/Moeda Origem/Moeda Destino
      LOOP AT it_mov_banco WHERE tp_lcto EQ it_zglt031-tp_lcto.

        CLEAR: wa_zglt035.
        wa_zglt035-tp_lcto   = it_mov_banco-tp_lcto.
        wa_zglt035-lote      = e_num_lote.
        wa_zglt035-bukrs     = it_mov_banco-bukrs.

        IF NOT ( it_mov_banco-tcurr EQ wa_t001-waers   OR
                 it_mov_banco-tcurr EQ wa_moedas-hwae2 OR
                 it_mov_banco-tcurr EQ wa_moedas-hwae3 )
           OR
           NOT ( it_mov_banco-fcurr EQ wa_t001-waers   OR
                 it_mov_banco-fcurr EQ wa_moedas-hwae2 OR
                 it_mov_banco-fcurr EQ wa_moedas-hwae3 ).

          IF NOT ( it_mov_banco-tcurr EQ wa_t001-waers   OR
                 it_mov_banco-tcurr EQ wa_moedas-hwae2 OR
                 it_mov_banco-tcurr EQ wa_moedas-hwae3 ).
            wa_zglt035-moeda_doc = it_mov_banco-tcurr.
          ELSE.
            wa_zglt035-moeda_doc = it_mov_banco-fcurr.
          ENDIF.
        ELSE.
          wa_zglt035-moeda_doc = it_mov_banco-fcurr.
        ENDIF.

*        "Alterei pra sempre pegar a moeda  do banco 17.03.2021
*        wa_zglt035-moeda_doc = zde_saldo_cta_banco-waers.

        wa_zglt035-dpto_resp = it_zglt031-dpto_resp.
        wa_zglt035-blart     = it_zglt031-blart.

        wa_zglt035-bldat     = p_cta_banco-budat.
        wa_zglt035-budat     = p_cta_banco-budat.
        wa_zglt035-dt_lcto   = sy-datum.

        IF it_mov_banco-tcurr IS NOT INITIAL AND it_mov_banco-tcurr NE p_cta_banco-waers.
          wa_zglt035-taxa = wa_mov_banco-ukurs.
        ENDIF.

        wa_zglt035-moeda_interna = wa_t001-waers.   "Código da moeda
        wa_zglt035-moeda_forte   = wa_moedas-hwae2. "Código da moeda
        wa_zglt035-moeda_grupo   = wa_moedas-hwae3. "Código da moeda

        "XBLNR               -- Nº documento de referência
        CONCATENATE p_cta_banco-budat+6(2) '-' p_cta_banco-budat+4(2) '-' p_cta_banco-budat(4) INTO i_descr_lote.
        CONCATENATE TEXT-007 i_descr_lote INTO i_descr_lote SEPARATED BY space.
        MOVE i_descr_lote TO wa_zglt035-bktxt. "              -- Texto de cabeçalho de documento

        "Movimentos Lançados com Tipo de Movimento do Mesmo departamento e tipo de documento
        " (Itens do Documento)
        CLEAR: it_zglt036.
        lc_seqitem = 1.

        LOOP AT p_mov_lct_banco INTO wa_mov_banco
           WHERE bukrs   EQ it_mov_banco-bukrs
             AND tp_lcto EQ it_mov_banco-tp_lcto
             AND fcurr   EQ it_mov_banco-fcurr
             AND tcurr   EQ it_mov_banco-tcurr.

          IF NOT ( ( wa_mov_banco-dmbe2_variacao NE 0 ) AND ( it_zglt031-st_trans_banc EQ abap_true ) ).
            CLEAR: it_zglt031-st_trans_banc.
          ENDIF.

          IF tp_comp EQ 'D'.
            it_zglt031-st_trans_banc = 'X'.
          ENDIF.

          CASE it_zglt031-st_trans_banc.
            WHEN abap_true.
              " Ini Tranferência Bancária """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              lc_variacao = wa_mov_banco-dmbe2_variacao.

              " Chave da Variação Cambial """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
              IF wa_mov_banco-bschl_variacao IS NOT INITIAL.
                READ TABLE it_tbsl INTO wa_tbsl_var WITH KEY bschl = wa_mov_banco-bschl_variacao BINARY SEARCH.
              ENDIF.

              "Item 1 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra o Banco """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl_banco hkont = p_cta_banco-saknr.
              READ TABLE it_tbsl WITH KEY bschl = wa_mov_banco-bschl_banco BINARY SEARCH.

              CLEAR: wa_zglt036.
              wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
              wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
              wa_zglt036-bschl      = wa_mov_banco-bschl_banco.  "Chave de lançamento
              wa_zglt036-hkont      = p_cta_banco-saknr.   "Conta do Razão da contabilidade geral
              IF ( wa_mov_banco-umskz IS INITIAL ).
                wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
              ELSE.
                IF ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ).
                  wa_zglt036-umskz  = wa_mov_banco-umskz.  "Código de Razão Especial
                ELSE.
                  wa_zglt036-umskz  = it_zglt032-umskz.    "Código de Razão Especial
                ENDIF.
              ENDIF.
              wa_zglt036-vbund      = it_zglt032-vbund.    "Nº sociedade parceira
              wa_zglt036-kostl      = it_zglt032-kostl.    "Centro de custo
              wa_zglt036-prctr      = it_zglt032-prctr.    "Centro de lucro
              wa_zglt036-aufnr      = it_zglt032-aufnr.    "Nº ordem
              wa_zglt036-matnr      = it_zglt032-matnr.    "Nº do material
              wa_zglt036-matnr_fi   = it_zglt032-matnr_fi. "Nº do material
              wa_zglt036-zuonr      = it_zglt032-zuonr.    "Nº atribuição

              IF wa_mov_banco-sgtxt IS INITIAL.
                wa_zglt036-sgtxt    = it_zglt032-sgtxt."Texto do item
              ELSE.
                wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
              ENDIF.
              wa_zglt036-gsber         = wa_aux-gsber. "Divisão

              IF wa_zglt035-moeda_doc EQ it_mov_banco-fcurr.
                wa_zglt036-vlr_moeda_doc = wa_mov_banco-dmbtr."Valor Moeda Documento
              ELSE.
                wa_zglt036-vlr_moeda_doc = wa_mov_banco-dmbe2."Valor Moeda Documento
              ENDIF.

              "Data da Ficha
              lc_data = p_cta_banco-budat.

              CASE wa_mov_banco-tcurr.
                WHEN wa_t001-waers.
                  wa_zglt036-vlr_moeda_int   = wa_mov_banco-dmbe2.
                  IF wa_zglt035-moeda_doc = 'GBP'.
                    lc_data = p_cta_banco-budat.
                    obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                    wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
                  ELSEIF wa_zglt035-moeda_doc EQ wa_moedas-hwae2.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae2 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_forte = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae2 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                  IF wa_zglt035-moeda_doc EQ wa_moedas-hwae3.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae3 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_grupo = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae3 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                WHEN wa_moedas-hwae2.
                  wa_zglt036-vlr_moeda_forte = wa_mov_banco-dmbe2.

                  IF wa_zglt035-moeda_doc EQ wa_t001-waers.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_t001-waers EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_int = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_moedas-hwae2 ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_t001-waers ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_forte / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_forte * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                  IF wa_zglt035-moeda_doc EQ wa_moedas-hwae3.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae3 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_grupo = wa_mov_banco-dmbtr.
                    ELSE.
                      "LC_DATA = WA_ZGLT035-DT_LCTO.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae3 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                WHEN wa_moedas-hwae3.
                  wa_zglt036-vlr_moeda_grupo = wa_mov_banco-dmbe2.

                  IF wa_zglt035-moeda_doc EQ wa_t001-waers.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_t001-waers EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_int = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_moedas-hwae3 ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_t001-waers ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_grupo / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_grupo * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                  IF wa_zglt035-moeda_doc EQ wa_moedas-hwae2.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae2 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_forte = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae2 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                WHEN OTHERS.

                  IF wa_zglt035-moeda_doc EQ wa_t001-waers.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc. "sempre na moeda do banco
                    "03.12.2021 ALRS problema conversao moeda interna
                    IF wa_zglt035-moeda_doc = 'GBP'.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_zglt035-moeda_doc ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_t001-waers ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
                    ENDIF.
                  ENDIF.

                  IF wa_zglt035-moeda_doc = 'GBP'.
                    lc_data = p_cta_banco-budat.
                    obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

*                    wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
                    wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc / abs( lc_ukurs ).

                  ELSEIF wa_zglt035-moeda_doc EQ wa_moedas-hwae2.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae2 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_forte = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae2 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                  IF wa_zglt035-moeda_doc EQ wa_moedas-hwae3.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    IF wa_moedas-hwae3 EQ wa_mov_banco-fcurr.
                      wa_zglt036-vlr_moeda_grupo = wa_mov_banco-dmbtr.
                    ELSE.
                      obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                      obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                      obj_cotacao->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
                      obj_cotacao->set_tcurr( EXPORTING i_tcurr = wa_moedas-hwae3 ).
                      obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                      IF lc_ukurs LT 0.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                      ELSE.
                        wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                      ENDIF.
                    ENDIF.
                  ENDIF.

              ENDCASE.

              wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA
              wa_zglt036-valut      = p_cta_banco-budat.

              lc_vl_doc_1 = wa_zglt036-vlr_moeda_doc.
              lc_vl_int_1 = wa_zglt036-vlr_moeda_int.
              lc_vl_for_1 = wa_zglt036-vlr_moeda_forte.
              lc_vl_grp_1 = wa_zglt036-vlr_moeda_grupo.

              wa_zglt036-vlr_moeda_doc    = abs( wa_zglt036-vlr_moeda_doc   ).
              wa_zglt036-vlr_moeda_int    = abs( wa_zglt036-vlr_moeda_int   ).
              wa_zglt036-vlr_moeda_forte  = abs( wa_zglt036-vlr_moeda_forte ).
              wa_zglt036-vlr_moeda_grupo  = abs( wa_zglt036-vlr_moeda_grupo ).
              APPEND wa_zglt036 TO it_zglt036.

              " Fim Item 1 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              "Item 2 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra de Contrapartida """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              ADD 1 TO lc_seqitem.
              READ TABLE it_tbsl WITH KEY bschl = wa_mov_banco-bschl BINARY SEARCH.

              READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl hkont = wa_mov_banco-saknr.
              IF ( sy-subrc IS NOT INITIAL ) AND ( ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ) ).
                READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl.
              ENDIF.

              CLEAR: wa_zglt036.
              wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
              wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
              wa_zglt036-bschl      = wa_mov_banco-bschl. "Chave de lançamento
              wa_zglt036-hkont      = wa_mov_banco-saknr."Conta do Razão da contabilidade geral
              IF ( wa_mov_banco-umskz IS INITIAL ).
                wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
              ELSE.
                IF ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ).
                  wa_zglt036-umskz    = wa_mov_banco-umskz.  "Código de Razão Especial
                ELSE.
                  wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
                ENDIF.
              ENDIF.
              wa_zglt036-vbund      = it_zglt032-vbund.         "Nº sociedade parceira
              wa_zglt036-kostl      = it_zglt032-kostl.         "Centro de custo
              wa_zglt036-prctr      = it_zglt032-prctr.         "Centro de lucro
              wa_zglt036-aufnr      = it_zglt032-aufnr.         "Nº ordem
              wa_zglt036-matnr      = it_zglt032-matnr.         "Nº do material
              wa_zglt036-matnr_fi   = it_zglt032-matnr_fi.      "Nº do material
              wa_zglt036-zuonr      = it_zglt032-zuonr.         "Nº atribuição

              IF wa_mov_banco-sgtxt IS INITIAL.
                wa_zglt036-sgtxt    = it_zglt032-sgtxt.  "Texto do item
              ELSE.
                wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
              ENDIF.
              wa_zglt036-gsber         = wa_aux-gsber. "Divisão

              CASE wa_mov_banco-tcurr.
                WHEN wa_t001-waers.    "EUR

                  wa_zglt036-vlr_moeda_int = lc_vl_int_1 - lc_variacao.

                  obj_cotacao->set_data(  EXPORTING i_data    = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae2 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

                  obj_cotacao->set_data(  EXPORTING i_data    = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae3 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

                  IF wa_t001-waers EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int.
                  ELSEIF wa_moedas-hwae2 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_forte.
                  ELSEIF wa_moedas-hwae3 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_grupo.
                  ELSE.
                    obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_zglt035-moeda_doc ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                    IF lc_ukurs LT 0.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int / lc_ukurs.
                    ELSE.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int * lc_ukurs.
                    ENDIF.
                  ENDIF.

                WHEN wa_moedas-hwae2.  "USD -- CONVERTER

                  wa_zglt036-vlr_moeda_forte = lc_vl_for_1 - lc_variacao.

                  obj_cotacao->set_data(  EXPORTING i_data    = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_moedas-hwae2 ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_t001-waers ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_forte / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_forte * lc_ukurs.
                  ENDIF.

                  obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae3 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

                  IF wa_t001-waers EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int.
                  ELSEIF wa_moedas-hwae2 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_forte.
                  ELSEIF wa_moedas-hwae3 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_grupo.
                  ELSE.
                    obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_zglt035-moeda_doc ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

                    IF lc_ukurs LT 0.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int / lc_ukurs.
                    ELSE.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int * lc_ukurs.
                    ENDIF.
                  ENDIF.

                WHEN wa_moedas-hwae3.  "USD -- CONVERTER

                  wa_zglt036-vlr_moeda_grupo = lc_vl_grp_1 - lc_variacao.

                  obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_moedas-hwae3 ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_t001-waers ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_grupo / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_grupo * lc_ukurs.
                  ENDIF.

                  obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae2 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

                  IF wa_t001-waers EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int.
                  ELSEIF wa_moedas-hwae2 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_forte.
                  ELSEIF wa_moedas-hwae3 EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_grupo.
                  ELSE.
                    obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_zglt035-moeda_doc ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

                    IF lc_ukurs LT 0.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int / lc_ukurs.
                    ELSE.
                      wa_zglt036-vlr_moeda_doc = wa_zglt036-vlr_moeda_int * lc_ukurs.
                    ENDIF.
                  ENDIF.

                WHEN OTHERS.

                  wa_zglt036-vlr_moeda_doc = lc_vl_doc_1 - lc_variacao.

                  IF wa_t001-waers EQ wa_zglt035-moeda_doc.
                    wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc.
                  ELSE.
                    obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_zglt035-moeda_doc ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_t001-waers ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

                    IF lc_ukurs LT 0.
                      wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc / lc_ukurs.
                    ELSE.
                      wa_zglt036-vlr_moeda_int = wa_zglt036-vlr_moeda_doc * lc_ukurs.
                    ENDIF.
                  ENDIF.

                  obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae2 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

                  IF wa_zglt035-moeda_doc = 'GBP'.
                    lc_data = p_cta_banco-budat.
                    obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                    obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                    obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                    obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                    obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
*                    wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
                    wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc / abs( lc_ukurs ).
                  ENDIF.

                  obj_cotacao->set_data(  EXPORTING i_data = lc_data ).
                  obj_cotacao->set_kurst( EXPORTING i_kurst   = 'B' ).
                  obj_cotacao->set_waerk( EXPORTING i_waerk   = wa_t001-waers ).
                  obj_cotacao->set_tcurr( EXPORTING i_tcurr   = wa_moedas-hwae3 ).
                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).

                  IF lc_ukurs LT 0.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int / lc_ukurs.
                  ELSE.
                    wa_zglt036-vlr_moeda_grupo = wa_zglt036-vlr_moeda_int * lc_ukurs.
                  ENDIF.

              ENDCASE.

              wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA

              READ TABLE it_cta_banc WITH KEY saknr = wa_zglt036-hkont BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wa_zglt036-valut = p_cta_banco-budat.
              ENDIF.

              lc_vl_doc_2 = wa_zglt036-vlr_moeda_doc.
              lc_vl_int_2 = wa_zglt036-vlr_moeda_int.
              lc_vl_for_2 = wa_zglt036-vlr_moeda_forte.
              lc_vl_grp_2 = wa_zglt036-vlr_moeda_grupo.

              wa_zglt036-vlr_moeda_doc    = abs( wa_zglt036-vlr_moeda_doc   ).
              wa_zglt036-vlr_moeda_int    = abs( wa_zglt036-vlr_moeda_int   ).
              wa_zglt036-vlr_moeda_forte  = abs( wa_zglt036-vlr_moeda_forte ).
              wa_zglt036-vlr_moeda_grupo  = abs( wa_zglt036-vlr_moeda_grupo ).

              APPEND wa_zglt036 TO it_zglt036.
              " Fim Item 2 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              ADD 1 TO lc_seqitem.

              "Item 3 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra de Variação Cambial """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "IF LC_VARIACAO NE 0.
              READ TABLE it_tbsl WITH KEY bschl = wa_mov_banco-bschl_variacao BINARY SEARCH.

              READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl_variacao hkont = wa_mov_banco-saknr_variacao.
              IF ( sy-subrc IS NOT INITIAL ) AND ( ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ) ).
                READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl.
              ENDIF.

              CLEAR: wa_zglt036.
              wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
              wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
              wa_zglt036-bschl      = wa_mov_banco-bschl_variacao. "Chave de lançamento
              wa_zglt036-hkont      = wa_mov_banco-saknr_variacao. "Conta do Razão da contabilidade geral
              wa_zglt036-umskz      = it_zglt032-umskz.            "Código de Razão Especial
              wa_zglt036-vbund      = it_zglt032-vbund.         "Nº sociedade parceira
              wa_zglt036-kostl      = it_zglt032-kostl.         "Centro de custo
              wa_zglt036-prctr      = it_zglt032-prctr.         "Centro de lucro
              wa_zglt036-aufnr      = it_zglt032-aufnr.         "Nº ordem
              wa_zglt036-matnr      = it_zglt032-matnr.         "Nº do material
              wa_zglt036-matnr_fi   = it_zglt032-matnr_fi.      "Nº do material
              wa_zglt036-zuonr      = it_zglt032-zuonr.         "Nº atribuição

              IF wa_mov_banco-sgtxt IS INITIAL.
                wa_zglt036-sgtxt    = it_zglt032-sgtxt.  "Texto do item
              ELSE.
                wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
              ENDIF.

              wa_zglt036-gsber         = wa_aux-gsber. "Divisão

              lc_vl_doc_3 = abs( lc_vl_doc_1 ) - abs( lc_vl_doc_2 ).
              lc_vl_int_3 = abs( lc_vl_int_1 ) - abs( lc_vl_int_2 ).
              lc_vl_for_3 = abs( lc_vl_for_1 ) - abs( lc_vl_for_2 ).
              lc_vl_grp_3 = abs( lc_vl_grp_1 ) - abs( lc_vl_grp_2 ).

              "H  Crédito
              "S  Débito
              lc_shkzg = it_tbsl-shkzg.

              IF wa_mov_banco-dmbe2 GT 0.
                IF it_tbsl-shkzg EQ 'S'.
                  IF lc_vl_doc_3 LE 0.
                    wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                    CLEAR: lc_vl_doc_3.
                  ENDIF.
                  IF lc_vl_int_3 LE 0.
                    wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                    CLEAR: lc_vl_int_3.
                  ENDIF.
                  IF lc_vl_for_3 LE 0.
                    wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                    CLEAR: lc_vl_for_3.
                  ENDIF.
                  IF lc_vl_grp_3 LE 0.
                    wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                    CLEAR: lc_vl_grp_3.
                  ENDIF.
                ELSE.
                  IF lc_vl_doc_3 GT 0.
                    wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                    CLEAR: lc_vl_doc_3.
                  ENDIF.
                  IF lc_vl_int_3 GT 0.
                    wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                    CLEAR: lc_vl_int_3.
                  ENDIF.
                  IF lc_vl_for_3 GT 0.
                    wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                    CLEAR: lc_vl_for_3.
                  ENDIF.
                  IF lc_vl_grp_3 GT 0.
                    wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                    CLEAR: lc_vl_grp_3.
                  ENDIF.
                ENDIF.
              ELSE.
                IF it_tbsl-shkzg EQ 'H'.
                  IF lc_vl_doc_3 LE 0.
                    wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                    CLEAR: lc_vl_doc_3.
                  ENDIF.
                  IF lc_vl_int_3 LE 0.
                    wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                    CLEAR: lc_vl_int_3.
                  ENDIF.
                  IF lc_vl_for_3 LE 0.
                    wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                    CLEAR: lc_vl_for_3.
                  ENDIF.
                  IF lc_vl_grp_3 LE 0.
                    wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                    CLEAR: lc_vl_grp_3.
                  ENDIF.
                ELSE.
                  IF lc_vl_doc_3 GT 0.
                    wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                    CLEAR: lc_vl_doc_3.
                  ENDIF.
                  IF lc_vl_int_3 GT 0.
                    wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                    CLEAR: lc_vl_int_3.
                  ENDIF.
                  IF lc_vl_for_3 GT 0.
                    wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                    CLEAR: lc_vl_for_3.
                  ENDIF.
                  IF lc_vl_grp_3 GT 0.
                    wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                    CLEAR: lc_vl_grp_3.
                  ENDIF.
                ENDIF.
              ENDIF.
*              IF wa_zglt035-moeda_doc = 'GBP'.
*                lc_data = p_cta_banco-budat.
*                obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
*                obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
*                obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
*                obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
*                obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
*                wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
*              ENDIF.

              wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA
              APPEND wa_zglt036 TO it_zglt036.

              wa_zglt036_flg-doc_lcto        = wa_zglt036-doc_lcto.
              wa_zglt036_flg-seqitem         = wa_zglt036-seqitem.
              wa_zglt036_flg-seqsub          = wa_zglt036-seqsub.
              wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
              wa_zglt036_flg-fl_cv_moeda_int = abap_true.
              wa_zglt036_flg-fl_cv_moeda_for = abap_true.
              wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
              APPEND  wa_zglt036_flg TO it_zglt036_flg.

              ADD 1 TO lc_seqitem.

              "Item 4 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra de Variação Cambial """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
              IF lc_vl_doc_3 IS NOT INITIAL OR
                 lc_vl_int_3 IS NOT INITIAL OR
                 lc_vl_for_3 IS NOT INITIAL OR
                 lc_vl_grp_3 IS NOT INITIAL.

                CLEAR: wa_zglt036.

                LOOP AT it_zglt032 WHERE tp_lcto = wa_mov_banco-tp_lcto.
                  READ TABLE it_cta_banc WITH KEY saknr = it_zglt032-hkont BINARY SEARCH.
                  IF sy-subrc IS NOT INITIAL.
                    READ TABLE it_ska1 WITH KEY saknr = it_zglt032-hkont BINARY SEARCH.
                    IF it_ska1-xbilk IS INITIAL.
                      READ TABLE it_tbsl WITH KEY bschl = it_zglt032-bschl BINARY SEARCH.
                      IF sy-subrc IS INITIAL.
                        IF it_tbsl-shkzg <> lc_shkzg.
                          wa_zglt036-hkont = it_zglt032-hkont.
                          wa_zglt036-bschl = it_zglt032-bschl.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDLOOP.

                READ TABLE it_tbsl WITH KEY bschl = wa_zglt036-bschl BINARY SEARCH.

                wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
                wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
                wa_zglt036-umskz      = it_zglt032-umskz.            "Código de Razão Especial
                wa_zglt036-vbund      = it_zglt032-vbund.         "Nº sociedade parceira
                wa_zglt036-kostl      = it_zglt032-kostl.         "Centro de custo
                wa_zglt036-prctr      = it_zglt032-prctr.         "Centro de lucro
                wa_zglt036-aufnr      = it_zglt032-aufnr.         "Nº ordem
                wa_zglt036-matnr      = it_zglt032-matnr.         "Nº do material
                wa_zglt036-matnr_fi   = it_zglt032-matnr_fi.      "Nº do material
                wa_zglt036-zuonr      = it_zglt032-zuonr.         "Nº atribuição

                IF wa_mov_banco-sgtxt IS INITIAL.
                  wa_zglt036-sgtxt    = it_zglt032-sgtxt.  "Texto do item
                ELSE.
                  wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
                ENDIF.

                wa_zglt036-gsber      = wa_aux-gsber. "Divisão

                "H  Crédito
                "S  Débito
                IF wa_mov_banco-dmbe2 GT 0.
                  IF it_tbsl-shkzg EQ 'S'.
                    IF lc_vl_doc_3 LE 0.
                      wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                      CLEAR: lc_vl_doc_3.
                    ENDIF.
                    IF lc_vl_int_3 LE 0.
                      wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                      CLEAR: lc_vl_int_3.
                    ENDIF.
                    IF lc_vl_for_3 LE 0.
                      wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                      CLEAR: lc_vl_for_3.
                    ENDIF.
                    IF lc_vl_grp_3 LE 0.
                      wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                      CLEAR: lc_vl_grp_3.
                    ENDIF.
                  ELSE.
                    IF lc_vl_doc_3 GT 0.
                      wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                      CLEAR: lc_vl_doc_3.
                    ENDIF.
                    IF lc_vl_int_3 GT 0.
                      wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                      CLEAR: lc_vl_int_3.
                    ENDIF.
                    IF lc_vl_for_3 GT 0.
                      wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                      CLEAR: lc_vl_for_3.
                    ENDIF.
                    IF lc_vl_grp_3 GT 0.
                      wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                      CLEAR: lc_vl_grp_3.
                    ENDIF.
                  ENDIF.
                ELSE.
                  IF it_tbsl-shkzg EQ 'H'.
                    IF lc_vl_doc_3 LE 0.
                      wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                      CLEAR: lc_vl_doc_3.
                    ENDIF.
                    IF lc_vl_int_3 LE 0.
                      wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                      CLEAR: lc_vl_int_3.
                    ENDIF.
                    IF lc_vl_for_3 LE 0.
                      wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                      CLEAR: lc_vl_for_3.
                    ENDIF.
                    IF lc_vl_grp_3 LE 0.
                      wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                      CLEAR: lc_vl_grp_3.
                    ENDIF.
                  ELSE.
                    IF lc_vl_doc_3 GT 0.
                      wa_zglt036-vlr_moeda_doc   = abs( lc_vl_doc_3 ).
                      CLEAR: lc_vl_doc_3.
                    ENDIF.
                    IF lc_vl_int_3 GT 0.
                      wa_zglt036-vlr_moeda_int   = abs( lc_vl_int_3 ).
                      CLEAR: lc_vl_int_3.
                    ENDIF.
                    IF lc_vl_for_3 GT 0.
                      wa_zglt036-vlr_moeda_forte = abs( lc_vl_for_3 ).
                      CLEAR: lc_vl_for_3.
                    ENDIF.
                    IF lc_vl_grp_3 GT 0.
                      wa_zglt036-vlr_moeda_grupo = abs( lc_vl_grp_3 ).
                      CLEAR: lc_vl_grp_3.
                    ENDIF.
                  ENDIF.
                ENDIF.

*                IF wa_zglt035-moeda_doc = 'GBP'.
*                  lc_data = p_cta_banco-budat.
*                  obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
*                  obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
*                  obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
*                  obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
*                  obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
*                  wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
*                ENDIF.

                wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA
                APPEND wa_zglt036 TO it_zglt036.

                wa_zglt036_flg-doc_lcto        = wa_zglt036-doc_lcto.
                wa_zglt036_flg-seqitem         = wa_zglt036-seqitem.
                wa_zglt036_flg-seqsub          = wa_zglt036-seqsub.
                wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
                wa_zglt036_flg-fl_cv_moeda_int = abap_true.
                wa_zglt036_flg-fl_cv_moeda_for = abap_true.
                wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
                APPEND  wa_zglt036_flg TO it_zglt036_flg.
              ENDIF.

              "ENDIF.
              " Fim Tranferência Bancária """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            WHEN abap_false.
              " Ini Lançamento Normal """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              "Item 1 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra o Banco """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl_banco hkont   = p_cta_banco-saknr.
              READ TABLE it_tbsl WITH KEY bschl = wa_mov_banco-bschl_banco BINARY SEARCH.

              CLEAR: wa_zglt036.
              wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
              wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
              wa_zglt036-bschl      = wa_mov_banco-bschl_banco.  "Chave de lançamento
              wa_zglt036-hkont      = p_cta_banco-saknr.   "Conta do Razão da contabilidade geral
              IF ( wa_mov_banco-umskz IS INITIAL ).
                wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
              ELSE.
                IF ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ).
                  wa_zglt036-umskz  = wa_mov_banco-umskz.  "Código de Razão Especial
                ELSE.
                  wa_zglt036-umskz  = it_zglt032-umskz.    "Código de Razão Especial
                ENDIF.
              ENDIF.
              wa_zglt036-vbund      = it_zglt032-vbund.    "Nº sociedade parceira
              wa_zglt036-kostl      = it_zglt032-kostl.    "Centro de custo
              wa_zglt036-prctr      = it_zglt032-prctr.    "Centro de lucro
              wa_zglt036-aufnr      = it_zglt032-aufnr.    "Nº ordem
              wa_zglt036-matnr      = it_zglt032-matnr.    "Nº do material
              wa_zglt036-matnr_fi   = it_zglt032-matnr_fi. "Nº do material
              wa_zglt036-zuonr      = it_zglt032-zuonr.    "Nº atribuição

              IF wa_mov_banco-sgtxt IS INITIAL.
                wa_zglt036-sgtxt    = it_zglt032-sgtxt."Texto do item
              ELSE.
                wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
              ENDIF.
              wa_zglt036-gsber         = wa_aux-gsber. "Divisão
              wa_zglt036-vlr_moeda_doc = abs( wa_mov_banco-dmbtr )."Valor Moeda Documento
              wa_zglt036-vlr_moeda_forte = 0.
              IF wa_zglt035-moeda_doc = 'GBP'.
                lc_data = p_cta_banco-budat.
                obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
              ELSEIF wa_t001-waers EQ wa_mov_banco-tcurr.
                wa_zglt036-vlr_moeda_int = abs( wa_mov_banco-dmbe2 )."Valor Moeda Interna
              ELSE.
                CASE wa_mov_banco-tcurr.
                  WHEN wa_moedas-hwae2. "Moeda Forte
                    wa_zglt036-vlr_moeda_forte  = abs( wa_mov_banco-dmbe2 )."Valor Moeda forte
                  WHEN wa_moedas-hwae3. "Moeda Grupo
                    wa_zglt036-vlr_moeda_grupo  = abs( wa_mov_banco-dmbe2 )."Valor Moeda Grupo
                ENDCASE.
              ENDIF.
              wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA
              wa_zglt036-valut      = p_cta_banco-budat.

              APPEND wa_zglt036 TO it_zglt036.
              " Fim Item 1 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              "Item 2 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              "Contra de Contrapartida """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              ADD 1 TO lc_seqitem.

              READ TABLE it_tbsl WITH KEY bschl = wa_mov_banco-bschl BINARY SEARCH.

              READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl hkont = wa_mov_banco-saknr.
              IF ( sy-subrc IS NOT INITIAL ) AND ( ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ) ).
                READ TABLE it_zglt032 WITH KEY tp_lcto = wa_mov_banco-tp_lcto bschl = wa_mov_banco-bschl.
              ENDIF.

              CLEAR: wa_zglt036.
              wa_zglt036-seqitem    = lc_seqitem. "Sequencial de Item
              wa_zglt036-tp_lcto    = wa_mov_banco-tp_lcto.
              wa_zglt036-bschl      = wa_mov_banco-bschl. "Chave de lançamento
              wa_zglt036-hkont      = wa_mov_banco-saknr."Conta do Razão da contabilidade geral
              IF ( wa_mov_banco-umskz IS INITIAL ).
                wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
              ELSE.
                IF ( it_tbsl-koart EQ 'D' ) OR ( it_tbsl-koart EQ 'K' ).
                  wa_zglt036-umskz    = wa_mov_banco-umskz.  "Código de Razão Especial
                ELSE.
                  wa_zglt036-umskz    = it_zglt032-umskz.    "Código de Razão Especial
                ENDIF.
              ENDIF.
              wa_zglt036-vbund      = it_zglt032-vbund.         "Nº sociedade parceira
              wa_zglt036-kostl      = it_zglt032-kostl.         "Centro de custo
              wa_zglt036-prctr      = it_zglt032-prctr.         "Centro de lucro
              wa_zglt036-aufnr      = it_zglt032-aufnr.         "Nº ordem
              wa_zglt036-matnr      = it_zglt032-matnr.         "Nº do material
              wa_zglt036-matnr_fi   = it_zglt032-matnr_fi.      "Nº do material
              wa_zglt036-zuonr      = it_zglt032-zuonr.         "Nº atribuição

              IF wa_mov_banco-sgtxt IS INITIAL.
                wa_zglt036-sgtxt    = it_zglt032-sgtxt.  "Texto do item
              ELSE.
                wa_zglt036-sgtxt    = wa_mov_banco-sgtxt."Texto do item
              ENDIF.
              wa_zglt036-gsber         = wa_aux-gsber. "Divisão
              wa_zglt036-vlr_moeda_doc = abs( wa_mov_banco-dmbtr )."Valor Moeda Documento

              IF wa_t001-waers EQ wa_mov_banco-tcurr.
                wa_zglt036-vlr_moeda_int = abs( wa_mov_banco-dmbe2 )."Valor Moeda Interna
              ELSE.
                CASE wa_mov_banco-tcurr.
                  WHEN wa_moedas-hwae2. "Moeda Forte
                    wa_zglt036-vlr_moeda_forte  = abs( wa_mov_banco-dmbe2 )."Valor Moeda forte
                  WHEN wa_moedas-hwae3. "Moeda Grupo
                    wa_zglt036-vlr_moeda_grupo  = abs( wa_mov_banco-dmbe2 )."Valor Moeda Grupo
                ENDCASE.
              ENDIF.
              wa_zglt036-tax_code   = it_zglt032-tax_code. "Código do IVA

              READ TABLE it_cta_banc WITH KEY saknr = wa_zglt036-hkont BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wa_zglt036-valut = p_cta_banco-budat.
              ENDIF.
              IF wa_zglt035-moeda_doc = 'GBP'.
                lc_data = p_cta_banco-budat.
                obj_cotacao->set_data(  EXPORTING i_data  = lc_data   ).
                obj_cotacao->set_kurst( EXPORTING i_kurst = 'B' ).
                obj_cotacao->set_waerk( EXPORTING i_waerk = 'GBP' ).
                obj_cotacao->set_tcurr( EXPORTING i_tcurr = 'USD' ).
                obj_cotacao->taxa_cambio( RECEIVING e_ukurs = lc_ukurs ).
                wa_zglt036-vlr_moeda_forte =  wa_zglt036-vlr_moeda_doc * abs( lc_ukurs ).
              ENDIF.
              APPEND wa_zglt036 TO it_zglt036.
              " Fim Item 2 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              ADD 1 TO lc_seqitem.
              " Fim Lançamento Normal """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          ENDCASE.

        ENDLOOP.
        IF tp_comp = 'D'.
          LOOP AT it_zglt036 INTO wa_zglt036.
            IF wa_zglt036-bschl IS NOT INITIAL.
              APPEND wa_zglt036 TO p_it_zglt036_comp.
            ENDIF.
          ENDLOOP.
        ELSE.
          CALL METHOD zcl_gerar_lote=>contabilizar_lote
            EXPORTING
              i_zglt036_flg = it_zglt036_flg
            IMPORTING
              e_num_doc     = e_num_doc
            CHANGING
              i_zglt036     = it_zglt036
              i_zglt035     = wa_zglt035.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    IF tp_comp NE 'D'.
      SUBMIT z_grava_zib_zgl WITH p_lote = e_num_lote AND RETURN.
      MESSAGE s048 WITH e_num_lote.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " GERAR_LOTE_CONTABIL

*&---------------------------------------------------------------------*
*&      Module  STATUS_0306  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0306 OUTPUT.

  DATA: lc_val_text TYPE dd07t-ddtext.

  SELECT SINGLE ddtext INTO lc_val_text
    FROM dd07t
   WHERE domname    EQ 'KOART'
     AND ddlanguage EQ sy-langu
     AND domvalue_l EQ zde_mov_lct_banco-koart.

  SET PF-STATUS 'PF0303'.
  SET TITLEBAR 'TL0306' WITH zde_mov_lct_banco-descricao lc_val_text.
  CLEAR: wa_0306, zde_mov_lct_banco-skont.

  SELECT SINGLE ktopl INTO wa_0306-lc_ktopl
    FROM t001
   WHERE bukrs EQ zde_mov_lct_banco-bukrs.

  IF zde_mov_lct_banco-saknr IS NOT INITIAL.
    "Procurar Nome parceiro / Conta de Reconciliação
    CASE zde_mov_lct_banco-koart.
      WHEN 'D'.	"Cliente
        SELECT SINGLE name1 INTO wa_0306-lc_name  FROM kna1 WHERE kunnr EQ zde_mov_lct_banco-saknr.
        SELECT SINGLE akont INTO wa_0306-lc_akont FROM knb1 WHERE kunnr EQ zde_mov_lct_banco-saknr
           AND bukrs EQ zde_mov_lct_banco-bukrs.
      WHEN 'K'.	"Fornecedores
        SELECT SINGLE name1 INTO wa_0306-lc_name  FROM lfa1 WHERE lifnr EQ zde_mov_lct_banco-saknr.
        SELECT SINGLE akont INTO wa_0306-lc_akont FROM lfb1 WHERE lifnr EQ zde_mov_lct_banco-saknr
           AND bukrs EQ zde_mov_lct_banco-bukrs.
    ENDCASE.

    zde_mov_lct_banco-akont = wa_0306-lc_akont.

    IF ( zde_mov_lct_banco-umskz IS NOT INITIAL ) AND ( wa_0306-lc_akont IS NOT INITIAL ).
      SELECT SINGLE skont INTO wa_0306-lc_skont
        FROM t074
       WHERE ktopl EQ wa_0306-lc_ktopl
         AND koart EQ zde_mov_lct_banco-koart
         AND umskz EQ zde_mov_lct_banco-umskz
         AND hkont EQ wa_0306-lc_akont.

      zde_mov_lct_banco-skont = wa_0306-lc_skont.
    ENDIF.

    IF wa_0306-lc_akont IS NOT INITIAL.
      SELECT SINGLE txt50 INTO wa_0306-lc_txt50a
        FROM skat
       WHERE spras EQ sy-langu
         AND ktopl EQ wa_0306-lc_ktopl
         AND saknr EQ wa_0306-lc_akont.
    ENDIF.

    IF wa_0306-lc_skont IS NOT INITIAL.
      SELECT SINGLE txt50 INTO wa_0306-lc_txt50s
        FROM skat
       WHERE spras EQ sy-langu
         AND ktopl EQ wa_0306-lc_ktopl
         AND saknr EQ wa_0306-lc_skont.
    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_0306  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0306_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0306_exit INPUT.
  CLEAR: zde_mov_lct_banco, wa_0306.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0306_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0306  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0306 INPUT.

  CASE ok_code.
    WHEN 'CONFIRMAR'.
      CHECK wa_0306 IS NOT INITIAL.

      CASE zde_mov_lct_banco-koart.
        WHEN 'D'.	"Cliente
          SELECT SINGLE name1 INTO wa_0306-lc_name  FROM kna1 WHERE kunnr EQ zde_mov_lct_banco-saknr.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE s756(fi) WITH zde_mov_lct_banco-umskz DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        WHEN 'K'.	"Fornecedores
          SELECT SINGLE name1 INTO wa_0306-lc_name  FROM lfa1 WHERE lifnr EQ zde_mov_lct_banco-saknr.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE s756(fi) WITH zde_mov_lct_banco-umskz DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
      ENDCASE.

      IF wa_0306-lc_skont IS INITIAL.
        MESSAGE s051 WITH zde_mov_lct_banco-umskz DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CLEAR: ok_code, wa_0306.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0306  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG_0306  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_update_flag_0306 INPUT.
  CLEAR: wa_0306.
  IF zde_mov_lct_banco-parid IS NOT INITIAL.
    zde_mov_lct_banco-saknr = zde_mov_lct_banco-parid.
  ENDIF.
ENDMODULE.                 " SET_UPDATE_FLAG_0306  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG_TAXA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_update_flag_taxa INPUT.

  CLEAR: zde_mov_lct_banco-tcurr, zde_mov_lct_banco-txt50.

  gb_tipo_valor = 'T'.
  MOVE zde_mov_lct_banco-ukurs_variacao_new TO zde_mov_lct_banco-ukurs_variacao.

ENDMODULE.                 " SET_UPDATE_FLAG_TAXA  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG_VALOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_update_flag_valor INPUT.

  CLEAR: zde_mov_lct_banco-tcurr, zde_mov_lct_banco-txt50.
  gb_tipo_valor = 'V'.

  IF zde_mov_lct_banco-dmbe2_variacaoc NE 0 AND zde_mov_lct_banco-dmbtr NE 0.
    zde_mov_lct_banco-dmbe2_variacaoc = abs( zde_mov_lct_banco-dmbe2_variacaoc ) * ( zde_mov_lct_banco-dmbtr / abs( zde_mov_lct_banco-dmbtr ) ).
  ENDIF.

ENDMODULE.                 " SET_UPDATE_FLAG_VALOR  INPUT

*&---------------------------------------------------------------------*
*&      Form  CALCULAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UKURS  text
*      -->P_ZDE_MOV_LCT_BANCO_BUKRS  text
*      -->P_ZDE_MOV_LCT_BANCO_TCURR  text
*      -->P_ZDE_MOV_LCT_BANCO_FCURR  text
*      <--P_ZDE_MOV_LCT_BANCO_DMBE2_VARIAC  text
*      <--P_ZDE_MOV_LCT_BANCO_OPERADOR_COT  text
*----------------------------------------------------------------------*
FORM calcular  USING    p_e_ukurs      TYPE ukurs_curr
                        p_e_ukurs_cal  TYPE ukurs_curr
                        p_bukrs        TYPE bukrs
                        p_tcurr        TYPE tcurr_curr
                        p_fcurr        TYPE waers_skb1
                        p_valor_base   TYPE zde_payments_1
               CHANGING p_vlr_variacao TYPE zde_payments_1
                        p_operador     TYPE char01.

  "BRL PARA EUR -- USD  P_E_UKURS +

  IF p_e_ukurs LT 0 AND p_fcurr NE 'GBP'.                   "IR034258
    p_vlr_variacao = p_valor_base / abs( p_e_ukurs_cal ).
    p_operador     = '*'.
  ELSE.
    p_vlr_variacao  = p_valor_base * abs( p_e_ukurs_cal ).
    p_operador      = '/'.
  ENDIF.

ENDFORM.                    " CALCULAR

*&---------------------------------------------------------------------*
*&      Form  CALCULAR2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UKURS  text
*      -->P_ZDE_MOV_LCT_BANCO_BUKRS  text
*      -->P_ZDE_MOV_LCT_BANCO_TCURR  text
*      -->P_ZDE_MOV_LCT_BANCO_FCURR  text
*      <--P_ZDE_MOV_LCT_BANCO_DMBE2_VARIAC  text
*      <--P_ZDE_MOV_LCT_BANCO_OPERADOR_COT  text
*----------------------------------------------------------------------*
FORM calcular2 USING    p_e_ukurs      TYPE ukurs_curr
                        p_e_ukurs_cal  TYPE zukurs_curr_dec10
                        p_bukrs        TYPE bukrs
                        p_tcurr        TYPE tcurr_curr
                        p_fcurr        TYPE waers_skb1
                        p_valor_base   TYPE zde_payments_1
               CHANGING p_vlr_variacao TYPE zde_payments_1
                        p_operador     TYPE char01.

  IF p_e_ukurs LT 0 AND p_fcurr NE 'GBP'.
    p_vlr_variacao = p_valor_base / abs( p_e_ukurs_cal ).
    p_operador     = '*'.
  ELSE.
    p_vlr_variacao  = p_valor_base * abs( p_e_ukurs_cal ).
    p_operador      = '/'.
  ENDIF.

ENDFORM.                    " CALCULAR2

*&---------------------------------------------------------------------*
*&      Form  POPULA_INFO_DISP_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popula_info_disp_lote .

  DATA: wa_t001g TYPE t001.

  FIELD-SYMBOLS <fs032> TYPE ty_zglt032.

  SET PARAMETER ID: 'ZBANK_SAKNR' FIELD zde_saldo_cta_banco-saknr,
                    'BUK'         FIELD zde_saldo_cta_banco-bukrs.

  SELECT SINGLE * INTO wa_t001g FROM t001 WHERE bukrs EQ zde_saldo_cta_banco-bukrs.

  SELECT * INTO TABLE it_zglt031
    FROM zglt031 AS c
   WHERE c~st_conc_banc EQ 'X'
     AND c~loekz        EQ abap_false
     AND EXISTS ( SELECT *
                    FROM zglt032 AS i
                   WHERE i~tp_lcto EQ c~tp_lcto
                     AND i~hkont   EQ zde_saldo_cta_banco-saknr
                     AND EXISTS ( SELECT * FROM skb1 AS b "#EC CI_DB_OPERATION_OK[2431747]
                                   WHERE b~bukrs EQ zde_saldo_cta_banco-bukrs
                                     AND b~saknr EQ i~hkont ) ).

  DELETE it_zglt031 WHERE bukrs IS NOT INITIAL AND bukrs NE zde_saldo_cta_banco-bukrs.

  IF it_zglt031[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zglt032
      FROM zglt032
       FOR ALL ENTRIES IN it_zglt031
     WHERE tp_lcto EQ it_zglt031-tp_lcto.
  ENDIF.

  IF it_zglt032[] IS NOT INITIAL.
    SELECT * INTO TABLE it_tbsl
      FROM tbsl
       FOR ALL ENTRIES IN it_zglt032
     WHERE bschl EQ it_zglt032-bschl.

    SORT it_tbsl BY bschl.

    SELECT * INTO TABLE it_ska1        "#EC CI_DB_OPERATION_OK[2389136]
      FROM ska1                        "#EC CI_DB_OPERATION_OK[2431747]
       FOR ALL ENTRIES IN it_zglt032
     WHERE ktopl EQ wa_t001g-ktopl
       AND saknr EQ it_zglt032-hkont.

    SORT it_ska1 BY saknr.

  ENDIF.

  LOOP AT it_zglt032 ASSIGNING <fs032>.
    "Busca tipo da chave
    "Busca natureza da conta Razão/Cliente/Fornecedor
    READ TABLE it_tbsl WITH KEY bschl = <fs032>-bschl BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs032>-shkzg = it_tbsl-shkzg.
      <fs032>-koart = it_tbsl-koart.
    ENDIF.

    READ TABLE it_ska1 WITH KEY saknr = <fs032>-hkont BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs032>-xbilk = it_ska1-xbilk.
    ENDIF.
  ENDLOOP.

  SELECT * INTO TABLE it_cta_banc
    FROM zsaldo_cta_moeda
   WHERE bukrs EQ zde_saldo_cta_banco-bukrs
     AND saknr NE zde_saldo_cta_banco-saknr.

  SORT it_cta_banc BY saknr.

ENDFORM.                    " POPULA_INFO_DISP_LOTE
