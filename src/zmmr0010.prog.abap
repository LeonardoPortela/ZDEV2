*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZMMR0010                                                *
* Descrição  : Relatório Algodoeira - Formação de Blocos               *
* Módulo     : MM                                Transação: ZMMT0001   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 16/06/2011 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zmmr0010 NO STANDARD PAGE HEADING MESSAGE-ID sd.

*----------------------------------------------------------------------*
*                                Type Pools                            *
*----------------------------------------------------------------------*
TYPE-POOLS icon.

TABLES: mchb, zppt0002.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_mchb,
         matnr      TYPE mchb-matnr,
         werks      TYPE mchb-werks,
         lgort      TYPE mchb-lgort,
         charg      TYPE mchb-charg,
         clabs      TYPE mchb-clabs,
         cspem      TYPE mchb-cspem,
         maktx      TYPE makt-maktx,
         lgortr     TYPE mchb-lgort,
         chargr     TYPE mchb-charg,
         matnc      TYPE mchb-matnr,
         cd_sai     TYPE zppt0002-cd_sai,
         icon       TYPE char05,
         log        TYPE char40,
         possuiacts TYPE string,
         status     TYPE string,
       END   OF type_mchb,

       BEGIN OF type_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END   OF type_makt,

       BEGIN OF type_msn,
         tp_msn   TYPE bapi_mtype,
         doc_mat  TYPE bapi2017_gm_head_ret-mat_doc,
         ano      TYPE bapi2017_gm_head_ret-doc_year,
         lote     TYPE charg_d,
         messagem TYPE bapi_msg,
       END   OF type_msn.



*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_mb1b  TYPE char4  VALUE 'MB1B',
           c_table TYPE char10 VALUE 'T_MCHB',
           c_x     TYPE char1  VALUE 'X',
           c_p     TYPE char1  VALUE 'P'.

**----------------------------------------------------------------------*
**                               Classes                                *
**----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA s_event TYPE REF TO lcl_event_receiver.
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION                            *
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor
        IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_event_receiver DEFINITION INHERITING FROM cl_gui_alv_grid..
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING
                    i_parent TYPE REF TO cl_gui_custom_container,

      set_delay_time,
      on_rows_selection FOR EVENT delayed_changed_sel_callback
        OF cl_gui_alv_grid.


ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION                        *
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.

  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        i_parent = i_parent.
    SET HANDLER me->on_rows_selection FOR ALL INSTANCES.
*    SET HANDLER ME->ZM_HANDLE_TOOLBAR FOR ALL INSTANCES.
  ENDMETHOD. "constructor

  METHOD set_delay_time.
    CALL METHOD me->set_delay_change_selection( 1 ).
  ENDMETHOD. "set_delay_time

  METHOD on_rows_selection.
    PERFORM do_sum.
  ENDMETHOD.                    "on_rows_selection

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_mchb      TYPE TABLE OF type_mchb,
      t_zppt0002  TYPE TABLE OF zppt0002,
      t_makt      TYPE TABLE OF type_makt,
      t_trans     TYPE TABLE OF type_mchb,
      t_fcat      TYPE TABLE OF lvc_s_fcat,
      t_tool      TYPE ui_functions,
      t_msn       TYPE TABLE OF type_msn,
      s_cont      TYPE REF TO cl_gui_custom_container,
      s_alv       TYPE REF TO lcl_event_receiver, "CL_GUI_ALV_GRID,
      obg_toolbar TYPE REF TO lcl_alv_toolbar,
      s_layout    TYPE lvc_s_layo,
      l_safra     TYPE numc4.  "*-CS2023000189-02.05.2023-#108747-JT

DATA: p_erro TYPE char01.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE TEXT-018.
  PARAMETERS: p_blocos TYPE c RADIOBUTTON GROUP rb1
                              DEFAULT 'X'
                              USER-COMMAND radio,
              p_sd     TYPE c RADIOBUTTON GROUP rb1,
              p_reser  TYPE c RADIOBUTTON GROUP rb1,
              p_estoq  TYPE c RADIOBUTTON GROUP rb1,
              p_recep  TYPE c RADIOBUTTON GROUP rb1,
              p_kunnr  TYPE kunnr,
              p_sgtxt  TYPE bktxt,
              p_bldat  TYPE mkpf-bldat, " OBLIGATORY,
              p_matnc  LIKE mchb-matnr.
SELECTION-SCREEN END   OF BLOCK a2.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
    s_matnr  FOR mchb-matnr NO INTERVALS NO-EXTENSION,
    s_werks  FOR mchb-werks NO INTERVALS NO-EXTENSION,
    s_lgorta FOR mchb-lgort NO INTERVALS NO-EXTENSION,
    s_lgortr FOR mchb-lgort NO INTERVALS NO-EXTENSION MODIF ID bbb,
    s_charg  FOR mchb-charg NO INTERVALS NO-EXTENSION MODIF ID aaa,
    p_cdsai  FOR zppt0002-cd_sai NO INTERVALS NO-EXTENSION MODIF ID aaa.
SELECTION-SCREEN END   OF BLOCK a1.



*----------------------------------------------------------------------*
*                           AT SELECTION-SCREEN                        *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF ( ( screen-name EQ '%_S_LGORTR_%_APP_%-TEXT' ) OR ( screen-name EQ '%_S_LGORTR_%_APP_%-OPTI_PUSH' ) OR ( screen-name EQ 'S_LGORTR-LOW' ) ) .
      IF NOT p_recep IS INITIAL.
        screen-output    = 1.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF ( ( screen-name EQ 'P_MATNC' ) OR ( screen-name EQ '%_P_MATNC_%_APP_%-TEXT' ) ) .
      IF  p_recep IS INITIAL.
        screen-output    = 1.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF ( ( screen-name EQ 'P_KUNNR' ) OR ( screen-name EQ '%_P_KUNNR_%_APP_%-TEXT' ) ) .
      IF  p_reser IS INITIAL AND
          p_estoq IS INITIAL.
        screen-output    = 1.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF ( ( screen-name EQ 'P_SGTXT' ) OR ( screen-name EQ '%_P_SGTXT_%_APP_%-TEXT' ) ) .
      IF  p_reser IS INITIAL AND
          p_estoq IS INITIAL.
        screen-output    = 1.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF ( ( screen-name EQ 'P_BLDAT' ) OR ( screen-name EQ '%_P_BLDAT_%_APP_%-TEXT' ) ) .
      IF  p_reser IS INITIAL AND
          p_estoq IS INITIAL.
        screen-output    = 1.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 EQ 'AAA'.
      IF p_sd EQ 'X'.
        screen-active    = 1.
        screen-required  = 0.
      ELSE.
        screen-active    = 0.
        screen-required  = 0.
        CLEAR: s_charg.
        REFRESH: s_charg.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 EQ 'BBB'.

      IF ( ( p_reser EQ 'X' ) OR ( p_estoq EQ 'X' ) ).
        screen-active    = 0.
        screen-required  = 0.
        CLEAR:  s_lgortr.
        REFRESH:  s_lgortr.

      ELSE.
        screen-active    = 1.
        screen-required  = 0.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
*                           Start of Selection                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CLEAR: p_erro.
  IF s_werks IS INITIAL.
    MESSAGE i836 WITH TEXT-026.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF s_lgorta IS INITIAL.
    MESSAGE i836 WITH TEXT-027.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_matnr IS INITIAL.
    MESSAGE i836 WITH TEXT-028.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF NOT p_blocos IS INITIAL.
    IF s_lgortr IS INITIAL.
      MESSAGE i836 WITH TEXT-024.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  IF NOT p_sd IS INITIAL .
    IF s_lgortr IS INITIAL.
      MESSAGE i836 WITH TEXT-024.
      LEAVE LIST-PROCESSING.
    ENDIF.
    IF s_charg IS INITIAL.
      MESSAGE i836 WITH TEXT-025.
      LEAVE LIST-PROCESSING.
    ENDIF.
*-CS2023000189-28.08.2023-#108747-JT-inicio
    l_safra = s_charg-low.
    IF l_safra <= 2010 OR l_safra > 2050.
      MESSAGE i836 WITH TEXT-033.
      LEAVE LIST-PROCESSING.
    ENDIF.
*-CS2023000189-28.08.2023-#108747-JT-fim

  ELSEIF NOT p_sd IS INITIAL AND p_recep IS NOT INITIAL.
    CLEAR p_matnc.
  ENDIF.

  IF NOT p_reser IS INITIAL OR
     NOT p_estoq IS INITIAL.

    IF p_kunnr IS INITIAL.
      MESSAGE i836 WITH TEXT-019.
      LEAVE LIST-PROCESSING.
    ENDIF.
    IF p_sgtxt IS INITIAL.
      MESSAGE i836 WITH TEXT-022.
      LEAVE LIST-PROCESSING.
    ENDIF.
    IF p_bldat IS INITIAL.
      MESSAGE i836 WITH TEXT-023.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    CLEAR p_kunnr.
    CLEAR p_sgtxt.
  ENDIF.

  IF NOT p_recep IS INITIAL.
    IF p_matnc IS INITIAL.
      MESSAGE i836 WITH TEXT-021.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    CLEAR p_matnc.
  ENDIF.

* Seleciona Dados
  PERFORM: z_seleciona_dados.
  IF p_erro IS INITIAL.
* Processa Dados
    PERFORM: z_processa_dados ,

* Monta FieldCat
             z_monta_fieldcat .

    CHECK NOT t_mchb[] IS INITIAL.

    CALL SCREEN 0100.
  ELSE.
    MESSAGE 'Erro comunicação API, procurar TI Tracecotton.' TYPE 'I' DISPLAY LIKE 'S'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR                                         *
*&---------------------------------------------------------------------*
*                           Incluindo Botão ALV                        *
*----------------------------------------------------------------------*
FORM z_handle_toolbar USING p_object      TYPE REF TO cl_alv_event_toolbar_set
                            p_interactive TYPE char1.

* Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

* Botão Transfêrencia
  CLEAR sl_toolbar.
  MOVE:  c_mb1b            TO sl_toolbar-function ,
         icon_ws_truck     TO sl_toolbar-icon     ,
         TEXT-017          TO sl_toolbar-quickinfo,
         TEXT-017          TO sl_toolbar-text     ,
         space             TO sl_toolbar-disabled .
  APPEND sl_toolbar TO p_object->mt_toolbar.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND                                         *
*&---------------------------------------------------------------------*
*                      User Command Botões Incluidos                   *
*----------------------------------------------------------------------*
FORM z_handle_command USING p_ucomm TYPE syucomm.

  CASE p_ucomm.
    WHEN c_mb1b.
*     Gerar Transferência
      PERFORM z_gerar_transf.
      CALL METHOD s_alv->refresh_table_display.
  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_TRANSF                                           *
*&---------------------------------------------------------------------*
*                           Gerar Transferência                        *
*----------------------------------------------------------------------*
FORM z_gerar_transf.

  DATA: tl_rows        TYPE lvc_t_row,
        sl_rows        TYPE lvc_s_row,
        sl_trans       TYPE type_mchb,
        wl_cont        TYPE sy-tabix,
        wl_cont_txt(6) TYPE c,
        l_erro         TYPE c,
        resposta,
        msg(150)       TYPE c.

  REFRESH t_trans.

* Verifica Seleção de Linhas
  CALL METHOD s_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF tl_rows[] IS INITIAL.
    MESSAGE i836 WITH TEXT-012.
    EXIT.
  ENDIF.

  FREE l_erro.

  LOOP AT tl_rows INTO sl_rows.
    READ TABLE t_mchb INTO sl_trans
      INDEX sl_rows-index.

*-CS2023000189-02.05.2023-#108747-JT-inicio
    SELECT SINGLE *
             FROM zsdt0330
             INTO @DATA(w_0330)
            WHERE matnr     = @sl_trans-matnr
              AND werks     = @sl_trans-werks
              AND lgort     = @sl_trans-lgort
              AND acharg    = @sl_trans-charg
              AND safra     = @sl_trans-chargr
              AND cancelado = @abap_false.

    IF sy-subrc = 0.
      l_erro = abap_true.
      EXIT.
    ENDIF.
*-CS2023000189-02.05.2023-#108747-JT-fim

    APPEND sl_trans TO t_trans.
    CLEAR: sl_rows ,
           sl_trans.
  ENDLOOP.

*-CS2023000189-02.05.2023-#108747-JT-inicio
  IF l_erro = abap_true.
    MESSAGE s024(sd) WITH 'Há fardos escolhidos que estão em Processamento Automático!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*-CS2023000189-02.05.2023-#108747-JT-fim

  DESCRIBE TABLE t_trans LINES wl_cont.


  IF ( wl_cont GT 0 ).

    wl_cont_txt = wl_cont.

    CONCATENATE 'Serão transferidos' wl_cont_txt 'fardos.' INTO msg SEPARATED BY space.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Geração de Fardos'
        text_question         = msg
        text_button_1         = 'Ok'
        icon_button_1         = '@0V@'
        text_button_2         = 'Cancelar'
        icon_button_2         = '@0W@'
        default_button        = '1'
        display_cancel_button = ' '
      IMPORTING
        answer                = resposta.


    IF ( resposta EQ 1 ).

      "Consulta dados API.



      " Cria Transferência
      PERFORM z_cria_trans.
    ELSE.
      REFRESH:  t_trans.
      MESSAGE s836(sd) WITH 'Ação cancelada'.
    ENDIF.
  ENDIF.
ENDFORM.                    " Z_GERAR_TRANSF

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                             Seleciona Dados                          *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona MCHB
  PERFORM: z_seleciona_mchb,

* Seleciona MAKT
           z_seleciona_makt.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MCHB                                         *
*&---------------------------------------------------------------------*
*                              Seleciona MCHB                          *
*----------------------------------------------------------------------*
FORM z_seleciona_mchb.

  DATA: sl_mchb   TYPE type_mchb,
        vl_index  TYPE p,
        vg_qtde   TYPE p,
        vg_werk   TYPE werks_d,
        vg_cd_sai TYPE char20,
        vg_lote   TYPE char04,
        vg_lgort  TYPE lgort_d,
        r_data    TYPE zde_acts_tracecotton_t,
        v_trace   TYPE char1 VALUE 0,
        v_letrac  TYPE char1 VALUE 0.

  DATA: r_charg  TYPE RANGE OF zppt0002-charg .

  REFRESH t_mchb.
  CLEAR: p_erro.

*-CS2023000189-28.08.2023-#108747-JT-inicio
  l_safra = COND #( WHEN p_sd = abap_true THEN s_charg-low
                                          ELSE 0 ).
*-CS2023000189-28.08.2023-#108747-JT-fim

*FIELD-SYMBOLS: <t_mchb> TYPE REF TO mchb.

  IF p_cdsai IS INITIAL.

    SELECT matnr werks lgort
           charg clabs cspem
      FROM mchb
      INTO TABLE t_mchb
    WHERE  matnr IN s_matnr
      AND  werks IN s_werks
      AND  lgort IN s_lgorta.

    "Selecionar cd sai.
    IF t_mchb IS NOT INITIAL.
*---  CS1042952/IR119361 --->
      "Consulta SET Safra.
      SELECT SINGLE *
    FROM setleaf
    INTO @DATA(i_data)
      WHERE setname EQ 'MAGGI_ACTS_ZMM0023'.

*-CS2023000189-28.08.2023-#108747-JT-inicio   / Comentado --Ajuste 30/08/2023 / aoenning.
*      i_data-valfrom = COND #( WHEN p_sd = abap_true THEN l_safra  / Comentado --Ajuste 30/08/2023 / aoenning.
*                                                     ELSE i_data-valfrom ). / Comentado --Ajuste 30/08/2023 / aoenning.
*-CS2023000189-28.08.2023-#108747-JT-fim  / Comentado --Ajuste 30/08/2023 / aoenning.


      SELECT * FROM zppt0002 INTO TABLE t_zppt0002
        FOR ALL ENTRIES IN t_mchb
        WHERE acharg EQ t_mchb-charg AND cd_safra EQ l_safra. "EQ i_data-valfrom. --Ajuste 30/08/2023 / aoenning.
      IF sy-subrc EQ 0.
        r_charg = VALUE #( FOR l IN t_zppt0002 ( sign = 'I' option = 'EQ' low = l-acharg  ) ).
        IF l_safra >= i_data-valfrom.
          v_trace = 0.
        ELSE.
          v_trace = 1.
        ENDIF.
      ELSE.
        v_trace = 1.
      ENDIF.

    ENDIF.
*<---  CS1042952/IR119361 ---

  ELSE.

    SELECT matnr werks lgort
           charg clabs cspem
      FROM mchb AS a
      INTO TABLE t_mchb
    WHERE  matnr IN s_matnr
      AND  werks IN s_werks
      AND  lgort IN s_lgorta
      AND  EXISTS ( SELECT * FROM zppt0002 AS b WHERE acharg EQ a~charg AND cd_sai IN p_cdsai ).

    "Selecionar cd sai.
    IF t_mchb IS NOT INITIAL.
      SELECT * FROM zppt0002 INTO TABLE t_zppt0002
        FOR ALL ENTRIES IN t_mchb
        WHERE acharg EQ t_mchb-charg.
    ENDIF.

  ENDIF.
*---  CS1042952/IR119361 --->
*  IF v_trace EQ 0.
  IF r_charg IS NOT INITIAL.

    DELETE t_mchb WHERE charg NOT IN r_charg.
    CHECK t_mchb IS NOT INITIAL.
  ENDIF.
*  ENDIF.
*<---  CS1042952/IR119361 --

  CLEAR: vg_qtde.
  DESCRIBE TABLE t_mchb LINES vg_qtde.

  "Consulta da API.
*  "============================Inicio USER STORY 90364 - Anderson Oenning.
  TRY .
      CLEAR: vg_werk, vg_cd_sai, vg_lote, vg_lgort.
      vg_werk   = s_werks-low.
      vg_cd_sai = p_cdsai-low.
      vg_lote = s_lgorta-low.
      vg_lgort = s_charg-low.

*---  CS1042952/IR119361 --->
*      IF v_trace EQ 0.
*        zcl_int_acts_tracecotton=>zif_int_acts_tracecotton~get_instance( i_servico =  '23'
*        )->set_fazenda( EXPORTING i_fazenda = vg_werk
*        )->set_lote( EXPORTING i_lote = vg_lgort
*        )->set_lgort(  EXPORTING i_lgort =  vg_lote
*        )->set_cd_sai(  EXPORTING i_cd_sai = vg_cd_sai
*        )->set_dados_acts( IMPORTING e_return = r_data ).
*      ENDIF.
*<---  CS1042952 ---

*      CHECK r_data IS NOT INITIAL.

*MOVE-CORRESPONDING t_mchb TO <t_mchb>.

      LOOP AT t_mchb ASSIGNING FIELD-SYMBOL(<sl_mchb>).

*---  CS1042952/IR119361 --->
        IF v_letrac EQ 0 AND v_trace EQ 0.
          zcl_int_acts_tracecotton=>zif_int_acts_tracecotton~get_instance( i_servico =  '23'
         )->set_fazenda( EXPORTING i_fazenda = vg_werk
         )->set_lote( EXPORTING i_lote = vg_lgort
         )->set_lgort(  EXPORTING i_lgort =  vg_lote
         )->set_cd_sai(  EXPORTING i_cd_sai = vg_cd_sai
         )->set_dados_acts( IMPORTING e_return = r_data ).
          v_letrac = 1.
        ENDIF.
*<---  CS1042952/IR119361 ---

        vl_index = sy-tabix.

        READ TABLE t_zppt0002 INTO DATA(ws_zppt0002) WITH KEY acharg = <sl_mchb>-charg.
        IF sy-subrc EQ 0 AND v_trace EQ 0.
          <sl_mchb>-cd_sai = ws_zppt0002-cd_sai.

          READ TABLE r_data INTO DATA(w_data) WITH KEY codigosai = ws_zppt0002-cd_sai.
          IF sy-subrc EQ 0.

            PERFORM f_lupa USING vl_index vg_qtde.

            IF w_data-takeupmarcadoacts EQ abap_true.
              IF w_data-possuiacts EQ abap_true AND w_data-statustakeuploterecente EQ 'Aprovado'.
                <sl_mchb>-icon    = icon_led_green.
                <sl_mchb>-status  = w_data-statustakeuploterecente.
                <sl_mchb>-possuiacts  = w_data-possuiacts.

              ELSEIF w_data-possuiacts EQ abap_false AND w_data-statustakeuploterecente NE 'Aprovado'.
                <sl_mchb>-icon   = icon_led_red.
                <sl_mchb>-status = w_data-statustakeuploterecente.
                <sl_mchb>-possuiacts  = w_data-possuiacts.

              ELSEIF w_data-possuiacts EQ abap_true AND w_data-statustakeuploterecente NE 'Aprovado'.
                <sl_mchb>-icon    = icon_led_red.
                <sl_mchb>-status  = w_data-statustakeuploterecente.
                <sl_mchb>-possuiacts  = w_data-possuiacts.

              ELSEIF w_data-possuiacts EQ abap_false AND w_data-statustakeuploterecente EQ 'Aprovado'.
                <sl_mchb>-icon    = icon_led_red.
                <sl_mchb>-status  = w_data-statustakeuploterecente.
                <sl_mchb>-possuiacts  = w_data-possuiacts.
              ENDIF.

            ELSE.

              IF w_data-statustakeuploterecente EQ 'Aprovado'.
                <sl_mchb>-icon   = icon_led_green.
                <sl_mchb>-status = w_data-statustakeuploterecente.
                <sl_mchb>-possuiacts  = abap_false.

              ELSEIF w_data-statustakeuploterecente NE 'Aprovado'.
                <sl_mchb>-icon   = icon_led_red.
                <sl_mchb>-status = w_data-statustakeuploterecente.
                <sl_mchb>-possuiacts  = abap_false.

              ELSE.
                <sl_mchb>-icon   = icon_led_red.
                <sl_mchb>-status = abap_false.
                <sl_mchb>-possuiacts  = abap_false.
              ENDIF.
            ENDIF.

          ELSE.
            <sl_mchb>-icon   = icon_led_red.
            <sl_mchb>-status = abap_false.
            <sl_mchb>-possuiacts  = abap_false.
          ENDIF.

        ELSE.
*---  CS1042952/IR119361 --->
          IF v_trace EQ 1.
            <sl_mchb>-icon   = icon_led_green.
            <sl_mchb>-status = w_data-statustakeuploterecente.
            <sl_mchb>-possuiacts  = abap_false.

            IF <sl_mchb>-clabs IS INITIAL AND <sl_mchb>-cspem IS NOT INITIAL.
              <sl_mchb>-clabs = <sl_mchb>-cspem.
              MODIFY t_mchb FROM <sl_mchb> INDEX vl_index.
*              TRANSPORTING clabs.
            ENDIF.
*<---  CS1042952/IR119361 ---
          ELSE.
            <sl_mchb>-icon   = icon_led_red.
            <sl_mchb>-status = abap_false.
            <sl_mchb>-possuiacts  = abap_false.
          ENDIF.
        ENDIF.

        IF <sl_mchb>-clabs IS INITIAL AND v_trace EQ 0.
          <sl_mchb>-clabs = sl_mchb-cspem.
*      MODIFY t_mchb FROM sl_mchb INDEX vl_index
*        TRANSPORTING clabs.
        ENDIF.


        CLEAR: w_data, ws_zppt0002.
*        FREE: r_data.
      ENDLOOP.

    CATCH zcx_integracao INTO DATA(ex_integra).    "
      <sl_mchb>-icon   = icon_led_red.
      <sl_mchb>-status = abap_false.
      <sl_mchb>-possuiacts  = abap_false.
      p_erro = abap_true.
*      ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

    CATCH zcx_error INTO DATA(ex_error).    "  "
      <sl_mchb>-icon   = icon_led_red.
      <sl_mchb>-status = abap_false.
      <sl_mchb>-possuiacts  = abap_false.
      p_erro = abap_true.
*      ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.
  "============================Inicio USER STORY 90364 - Anderson Oenning.
  DELETE t_mchb WHERE clabs EQ 0.
  SORT t_mchb BY matnr ASCENDING
                 werks ASCENDING
                 lgort ASCENDING
                 charg ASCENDING.

  IF t_mchb[] IS INITIAL.
    MESSAGE i836 WITH text-016.
    EXIT.
  ENDIF.

  IF p_recep IS INITIAL.
    sl_mchb-lgortr = s_lgortr-low.
  ENDIF.
  sl_mchb-chargr = s_charg-low.
  sl_mchb-matnc  = p_matnc.
  MODIFY t_mchb FROM sl_mchb
    TRANSPORTING lgortr chargr matnc
    WHERE lgortr EQ space.

ENDFORM.                    " Z_SELECIONA_MCHB

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_FIELDCAT                                         *
*&---------------------------------------------------------------------*
*                           Monta FieldCat                             *
*----------------------------------------------------------------------*
FORM z_monta_fieldcat.

  REFRESH: t_fcat,
           t_tool.

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:
    c_table 'ICON'       TEXT-030 10 space,
    c_table 'WERKS'      TEXT-002 06 space,
    c_table 'MATNR'      TEXT-003 10 c_x  ,
    c_table 'MAKTX'      TEXT-004 20 space,
    c_table 'CHARG'      TEXT-005 12 space,
    c_table 'CD_SAI'     TEXT-029 20 space,
    c_table 'CLABS'      TEXT-006 14 space,
    c_table 'LGORT'      TEXT-007 09 space,
    c_table 'LGORTR'     TEXT-008 12 space,
    c_table 'CHARGR'     TEXT-009 13 space,
    c_table 'POSSUIACTS' TEXT-031 10 space,
    c_table 'STATUS'     TEXT-032 10 space,
    c_table 'MATNC'      TEXT-020 10 c_x.




* Monta Layout
  PERFORM z_layout.

* Deleta Botões
  PERFORM z_deleta_bot USING: '&LOCAL&APPEND'       ,
                              '&LOCAL&COPY'         ,
                              '&LOCAL&COPY_ROW'     ,
                              '&LOCAL&CUT'          ,
                              '&LOCAL&DELETE_ROW'   ,
                              '&LOCAL&INSERT_ROW'   ,
                              '&LOCAL&MOVE_ROW'     ,
                              '&LOCAL&PASTE'        ,
                              '&LOCAL&PASTE_NEW_ROW',
                              '&LOCAL&UNDO'         ,
                              '&CHECK'              .

ENDFORM.                    " Z_MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.

  CLEAR s_layout.

  s_layout-zebra = 'X'.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  Z_DELETA_BOT                                             *
*&---------------------------------------------------------------------*
*                             Deleta Botões                            *
*----------------------------------------------------------------------*
FORM z_deleta_bot USING p_bot TYPE c.

  DATA sl_tool TYPE ui_func.

  sl_tool = p_bot.
  APPEND sl_tool TO t_tool.

ENDFORM.                    " Z_DELETA_BOT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM z_preenche_fieldcat USING p_table TYPE c
                               p_field TYPE c
                               p_desc  TYPE c
                               p_len   TYPE n
                               p_zero  TYPE c.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = p_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.
  sl_fcat-outputlen = p_len.
  sl_fcat-no_zero   = p_zero.

  APPEND sl_fcat TO t_fcat.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR 'TB0100'.
  ENDCASE.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_OBJ_ALV  OUTPUT                                     *
*&---------------------------------------------------------------------*
*                                 Obj Alv                              *
*----------------------------------------------------------------------*
MODULE zm_obj_alv OUTPUT.

* Instancia Container
  PERFORM: z_inst_cont ,
* Instancia Alv
           z_inst_alv  ,
* Instancia Eventos
           z_inst_event,
* Exibe Alv
           z_exibe_alv .

ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_CONT                                              *
*&---------------------------------------------------------------------*
*                       Instancia Container                            *
*----------------------------------------------------------------------*
FORM z_inst_cont.

  CHECK s_cont IS INITIAL.

  CREATE OBJECT s_cont
    EXPORTING
      container_name              = 'CC_ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH TEXT-010.
  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM z_inst_alv.

  CHECK s_alv IS INITIAL.

*  CREATE OBJECT S_ALV
*    EXPORTING
*      I_PARENT          = S_CONT
*    EXCEPTIONS
*      ERROR_CNTL_CREATE = 1
*      ERROR_CNTL_INIT   = 2
*      ERROR_CNTL_LINK   = 3
*      ERROR_DP_CREATE   = 4
*      OTHERS            = 5.
*
*  IF NOT SY-SUBRC IS INITIAL.
*    MESSAGE I836 WITH TEXT-011.
*  ENDIF.

  CREATE OBJECT s_alv
    EXPORTING
      i_parent = s_cont.

  CREATE OBJECT obg_toolbar
    EXPORTING
      io_alv_grid = s_alv.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_INST_EVENT                                             *
*&---------------------------------------------------------------------*
*                           Instancia Eventos                          *
*----------------------------------------------------------------------*
FORM z_inst_event.

  SET HANDLER obg_toolbar->zm_handle_user_command FOR s_alv.
  SET HANDLER obg_toolbar->zm_handle_toolbar      FOR s_alv.

*  CHECK S_EVENT IS INITIAL.
*
*  CREATE OBJECT S_EVENT.
*  SET HANDLER: S_EVENT->ZM_HANDLE_USER_COMMAND FOR S_ALV,
*               S_EVENT->ZM_HANDLE_TOOLBAR      FOR S_ALV.


  CALL METHOD s_alv->register_delayed_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select.
  CALL METHOD s_alv->set_delay_time( ).

ENDFORM.                    " Z_INST_EVENT

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  DATA vl_int TYPE int4.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
      i_default                     = c_x
      is_layout                     = s_layout
      it_toolbar_excluding          = t_tool
    CHANGING
      it_outtab                     = t_mchb
      it_fieldcatalog               = t_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD s_alv->set_ready_for_input.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_TRANS                                             *
*&---------------------------------------------------------------------*
*                           Cria Transferência                         *
*----------------------------------------------------------------------*
FORM z_cria_trans.

  DATA: sl_trans    TYPE type_mchb,
        sl_header   TYPE bapi2017_gm_head_01,
        vl_code     TYPE bapi2017_gm_code,
        vl_material TYPE bapi2017_gm_head_ret-mat_doc,
        vl_year     TYPE bapi2017_gm_head_ret-doc_year,
        tl_item     TYPE TABLE OF bapi2017_gm_item_create,
        tl_return   TYPE TABLE OF bapiret2,
        sl_return   TYPE bapiret2,
        sl_item     TYPE bapi2017_gm_item_create,
        r_data      TYPE zde_acts_tracecotton_t,
        i_date      TYPE bapi2017_gm_head_ret-doc_year,
        i_doc_mat   TYPE bapi2017_gm_head_ret-mat_doc,
        sl_zmmt0008 TYPE zmmt0008.

  REFRESH t_msn.
  CLEAR sl_header.

  SORT t_trans BY matnr ASCENDING
                  werks ASCENDING
                  lgort ASCENDING
                  charg ASCENDING.

  FREE: t_msn.

  READ TABLE t_trans INTO DATA(ws_trans) WITH KEY icon = icon_led_red.
  IF sy-subrc EQ 0.
    tl_return = VALUE #(  ( type = 'E' message =  'Existem lotes com status diferente de aprovado ou não possui ACTS, verificar.') ).
    PERFORM z_monta_erro TABLES tl_return
                            USING i_doc_mat
                                  i_date
                                  ws_trans-charg.
  ENDIF.

  IF t_msn IS INITIAL.

    FREE: t_msn.
    LOOP AT t_trans INTO sl_trans.

      CLEAR: vl_material,
             vl_year    ,
             sl_zmmt0008.


      REFRESH: tl_item  ,
               tl_return.

      IF sy-tabix EQ 1.
        vl_code = '06'.
        sl_header-pstng_date = sy-datum.
        IF p_reser EQ 'X'.
          sl_header-doc_date   = p_bldat.
        ELSE.
          sl_header-doc_date   = sy-datum.
        ENDIF.
      ENDIF.
*
*      READ TABLE t_zppt0002 INTO DATA(ls_zppt0002) WITH KEY acharg = sl_trans-charg.
*      IF sy-subrc EQ 0.
*        DATA(i_safra) = ls_zppt0002-cd_safra.
*      ENDIF.


      IF NOT p_blocos IS INITIAL.
        sl_item-move_type  = '311'.
      ENDIF.

      IF NOT p_sd IS INITIAL.
        sl_item-move_type  = 'ZA1'.
      ENDIF.

      IF NOT p_reser IS INITIAL.
        sl_item-move_type  = '344'.
      ENDIF.

      IF NOT p_estoq IS INITIAL.
        sl_item-move_type  = '343'.
      ENDIF.

      IF NOT p_recep IS INITIAL.
        sl_item-move_type  = '309'.
      ENDIF.

* ---> S4 Migration - 06/07/2023 - FC
      "sl_item-material   = sl_trans-matnr.

      DATA(v_len) = strlen( sl_trans-matnr ).

      IF v_len > 18.
        sl_item-material_long = sl_trans-matnr.
      ELSE.
        sl_item-material = sl_trans-matnr.
      ENDIF.
* <--- S4 Migration - 06/07/2023 - FC

      sl_item-plant      = sl_trans-werks.
      sl_item-stge_loc   = sl_trans-lgort.
      sl_item-batch      = sl_trans-charg.
      sl_item-entry_qnt  = sl_trans-clabs.
      sl_item-move_plant = sl_trans-werks.
      sl_item-move_stloc = sl_trans-lgortr.
      sl_item-move_mat   = sl_trans-matnc.
      sl_item-customer   = p_kunnr.
      sl_header-header_txt  = p_sgtxt.

      IF NOT sl_trans-chargr IS INITIAL.
        sl_item-move_batch = sl_trans-chargr.
      ELSE.
        sl_item-move_batch = sl_trans-charg.
      ENDIF.

      APPEND sl_item TO tl_item.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = sl_header
          goodsmvt_code    = vl_code
        IMPORTING
          materialdocument = vl_material
          matdocumentyear  = vl_year
        TABLES
          goodsmvt_item    = tl_item
          return           = tl_return.

      IF NOT vl_material IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = c_x.
        REFRESH tl_return.
        sl_return-type     = 'S'.
        CONCATENATE TEXT-013
                    vl_material
                    TEXT-014
                    vl_year
                    TEXT-015
               INTO sl_return-message SEPARATED BY space.
        APPEND sl_return TO tl_return.
        PERFORM z_monta_erro TABLES tl_return
                              USING vl_material
                                    vl_year
                                    sl_trans-charg.
        IF sl_item-move_type EQ 'ZA1'.
          sl_zmmt0008-werks  = sl_trans-werks.
          sl_zmmt0008-lgort  = sl_trans-lgort.
          sl_zmmt0008-charg  = sl_trans-charg.
          sl_zmmt0008-menge  = sl_trans-clabs.
          sl_zmmt0008-matnr  = s_matnr-low.    "*-S4H-US 123905-25.09.2023-JT
          sl_zmmt0008-lgortr = s_lgortr-low.   "*-S4H-US 123905-25.09.2023-JT
          sl_zmmt0008-safra  = s_charg-low.    "*-S4H-US 123905-25.09.2023-JT
          sl_zmmt0008-mblnr  = vl_material.    "*-S4H-US 123905-25.09.2023-JT
          sl_zmmt0008-mjahr  = vl_year.        "*-S4H-US 123905-25.09.2023-JT
          MODIFY zmmt0008 FROM sl_zmmt0008.
        ENDIF.

        DELETE t_trans WHERE matnr EQ sl_trans-matnr
                         AND werks EQ sl_trans-werks
                         AND lgort EQ sl_trans-lgort
                         AND charg EQ sl_trans-charg.
        DELETE t_mchb  WHERE matnr EQ sl_trans-matnr
                         AND werks EQ sl_trans-werks
                         AND lgort EQ sl_trans-lgort
                         AND charg EQ sl_trans-charg.

      ELSE.

*     Retorna Erro
        PERFORM z_monta_erro TABLES tl_return
                              USING vl_material
                                    vl_year
                                    sl_trans-charg.
      ENDIF.

      CLEAR: sl_trans,
             sl_item.
*             i_safra.

    ENDLOOP.

    IF NOT t_msn[] IS INITIAL.
      CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
        TABLES
          table    = t_msn
        EXCEPTIONS
          fb_error = 1
          OTHERS   = 2.
    ENDIF.

  ELSE.
    IF NOT t_msn[] IS INITIAL.
      CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
        TABLES
          table    = t_msn
        EXCEPTIONS
          fb_error = 1
          OTHERS   = 2.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_CRIA_TRANS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MAKT                                         *
*&---------------------------------------------------------------------*
*                               Seleciona MAKT                         *
*----------------------------------------------------------------------*
FORM z_seleciona_makt.

  DATA tl_mchb TYPE TABLE OF type_mchb.

  REFRESH t_makt.

  CHECK NOT t_mchb[] IS INITIAL.
  tl_mchb[] = t_mchb[].

  SORT tl_mchb BY matnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_mchb COMPARING matnr.

  SELECT matnr maktx
    FROM makt
    INTO TABLE t_makt
    FOR ALL ENTRIES IN tl_mchb
  WHERE  matnr EQ tl_mchb-matnr
    AND  spras EQ c_p.

    SORT t_makt BY matnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_MAKT

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS                                         *
*&---------------------------------------------------------------------*
*                              Processa Dados                          *
*----------------------------------------------------------------------*
FORM z_processa_dados.

  DATA: sl_makt  TYPE type_makt,
        sl_mchb  TYPE type_mchb,
        vl_index TYPE i.

  LOOP AT t_mchb INTO sl_mchb.

    vl_index = sy-tabix.


    READ TABLE t_makt INTO sl_makt
      WITH KEY matnr = sl_mchb-matnr
      BINARY SEARCH.

    sl_mchb-maktx = sl_makt-maktx.

    MODIFY t_mchb FROM sl_mchb
      INDEX vl_index
      TRANSPORTING maktx.

    CLEAR: sl_makt,
           sl_mchb.

  ENDLOOP.

ENDFORM.                    " Z_PROCESSA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_ERRO                                             *
*&---------------------------------------------------------------------*
*                             Retorna Erro                             *
*----------------------------------------------------------------------*
FORM z_monta_erro TABLES p_return  STRUCTURE bapiret2
                   USING p_mat_doc TYPE bapi2017_gm_head_ret-mat_doc
                         p_year    TYPE bapi2017_gm_head_ret-doc_year
                         p_charg   TYPE charg_d.

  DATA: sl_return TYPE bapiret2,
        sl_msn    TYPE type_msn.

  LOOP AT p_return INTO sl_return.

    sl_msn-tp_msn    = sl_return-type.
    sl_msn-messagem  = sl_return-message.
    sl_msn-doc_mat   = p_mat_doc.
    sl_msn-ano       = p_year.
    sl_msn-lote      = p_charg.

    APPEND sl_msn TO t_msn.
    CLEAR: sl_return,
           sl_msn   .
  ENDLOOP.

ENDFORM.                    " Z_MONTA_ERRO

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              Exit Command                            *
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  DO_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_sum .
  DATA: tl_rows        TYPE lvc_t_row.

  CALL METHOD s_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  DATA(_sel) = lines( tl_rows ).

  IF _sel > 0.
    MESSAGE |Selecionadas { _sel } linhas | TYPE 'S'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LUPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_INDEX  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM f_lupa USING p_msg1 TYPE p p_msg2 TYPE p.
  DATA: vl_message(150) TYPE c.
  CLEAR vl_message.

*  CONCATENATE | Constultando o registro p_msg1 'de ' p_msg2 INTO vl_message SEPARATED BY space.
  vl_message = |Constultando o registro { p_msg1 } de { p_msg2 }|.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 99
      text       = vl_message.
ENDFORM. "f_lupa
