*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 27/04/2022                                              &*
*& Descrição:Prestação de Serviço Transbordo - Cockipt Pgto Transbordo&*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :  CS2022000141                                           &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP           DEVK9A12JY   27.04.2022                             &*
*&--------------------------------------------------------------------&*
REPORT zlesr0155.

**********************************************************************
* tabelas
**********************************************************************
TABLES: zlest0221, sscrfields.

**********************************************************************
* types
**********************************************************************
TYPES: BEGIN OF ty_rbkp_sel,
         lifnr TYPE rbkp-lifnr,
         xblnr TYPE rbkp-xblnr,
         stblg TYPE rbkp-stblg.
TYPES: END   OF ty_rbkp_sel.

TYPES: BEGIN OF ty_bkpf_sel,
         bukrs TYPE bkpf-bukrs,
         gjahr TYPE bkpf-gjahr,
         awkey TYPE bkpf-awkey.
TYPES: END   OF ty_bkpf_sel.

TYPES: BEGIN OF ty_alv,
         bukrs           TYPE zlest0221-bukrs,
         cnpj_filial     TYPE zlest0221-cnpj_filial,
         cnpj_transbordo TYPE zlest0221-cnpj_transbordo,
         nfps            TYPE zlest0221-nfps,
         datasaida       TYPE zlest0221-datasaida,
         valor_servico   TYPE zlest0221-valor_servico,
         ebeln           TYPE ekbe-ebeln,
         belnr           TYPE rbkp-belnr,
         augbl           TYPE bsak-augbl,
         augdt           TYPE bsak-augdt,
         matnr           TYPE ekbe-matnr,
         maktx           TYPE makt-maktx.
TYPES: END   OF ty_alv.

**********************************************************************
* variaveis
**********************************************************************
DATA: gva_bukrs             TYPE zlest0221-bukrs,
      gva_cnpj_filial       TYPE zlest0221-cnpj_filial,
      gva_cnpj_prest        TYPE zlest0221-cnpj_transbordo,
      gva_data(10)          TYPE c,
      gva_mes               TYPE zlest0221-monat,
      gva_valor             TYPE zlest0221-valor_servico,
      gva_desc_branch(30)   TYPE c,
      gva_desc_branch_t(30) TYPE c,

      t_zlest0221           TYPE TABLE OF zlest0221,
      w_zlest0221           TYPE zlest0221,
      t_rbkp_sel            TYPE TABLE OF ty_rbkp_sel,
      w_rbkp_sel            TYPE ty_rbkp_sel,
      t_bkpf_sel            TYPE TABLE OF ty_bkpf_sel,
      w_bkpf_sel            TYPE ty_bkpf_sel,
      t_alv                 TYPE TABLE OF ty_alv,
      w_alv                 TYPE ty_alv,
      l_leave               TYPE syst_ucomm,

*------------------------------------
*---- ALV
*------------------------------------
      g_grid                TYPE REF TO cl_gui_alv_grid,
      g_custom_container    TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager  TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1           TYPE REF TO cl_gui_container,
      cl_container_95       TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id         TYPE REF TO cl_dd_document,
      picture               TYPE REF TO cl_gui_picture,
      l_graphic_conv        TYPE i,
      l_graphic_offs        TYPE i,
      graphic_size          TYPE i,
      l_graphic_xstr        TYPE xstring,
      url(255)              TYPE c,
      graphic_url(255),
      t_function            TYPE ui_functions,
      w_function            TYPE ui_func,
*
      t_fieldcat            TYPE lvc_t_fcat,
      w_fieldcat            TYPE lvc_s_fcat,
      t_colorcell           TYPE TABLE OF lvc_s_scol,
      w_colorcell           TYPE lvc_s_scol,
      t_exctab              TYPE slis_t_extab,
      w_exctab              TYPE slis_extab,
      w_layout              TYPE lvc_s_layo,
      w_stable              TYPE lvc_s_stbl,
      t_style               TYPE lvc_t_styl,
      w_style               TYPE lvc_s_styl,
      t_rows                TYPE lvc_t_row,
      w_rows                TYPE lvc_s_row,
      ok_code               TYPE sy-ucomm.
*
*------------------------------------
*---- figuras
*------------------------------------
DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: l_sel_button TYPE smp_dyntxt,
      l_opcao      TYPE char1.

data: v_passou type c.

**********************************************************************
* classes / implementacoes
**********************************************************************
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.
  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

ENDCLASS.

**********************************************************************
* tela selecao
**********************************************************************
PARAMETERS:     p_submit TYPE char20 NO-DISPLAY.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-003.
PARAMETERS:     p_lancto RADIOBUTTON GROUP g1 USER-COMMAND rad1 DEFAULT 'X'.
PARAMETERS:     p_consul RADIOBUTTON GROUP g1.
SELECTION-SCREEN END   OF BLOCK b0.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs  FOR zlest0221-bukrs           MODIF ID t1a NO-EXTENSION NO INTERVALS, " OBLIGATORY,
                s_cnfil  FOR zlest0221-cnpj_filial     MODIF ID t1  NO-EXTENSION NO INTERVALS, "MATCHCODE OBJECT  kred,
                s_cntran FOR zlest0221-cnpj_transbordo MODIF ID t1  NO-EXTENSION NO INTERVALS ,
                s_nfps   FOR zlest0221-nfps            MODIF ID t1  NO-EXTENSION NO INTERVALS,
                s_data   FOR zlest0221-datasaida       MODIF ID t1  NO-EXTENSION NO INTERVALS,
                s_monat  FOR zlest0221-monat           MODIF ID t1  NO-EXTENSION NO INTERVALS,
                s_valor  FOR zlest0221-valor_servico   MODIF ID t1  NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_bukrs2   FOR zlest0221-bukrs           MODIF ID t2a  NO INTERVALS , "Empresa
                s_cnfil2   FOR zlest0221-cnpj_filial     MODIF ID t2   NO INTERVALS , "CNPJ Filial
                s_cntra2   FOR zlest0221-cnpj_transbordo MODIF ID t2   NO INTERVALS , "CNPJ Prestador de Serviço
                s_data2    FOR zlest0221-datasaida       MODIF ID t2a  NO-EXTENSION . " Data Emissão NF Serviço
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'
SELECTION-SCREEN FUNCTION KEY 2.  "Will have a function code of 'FC02'
SELECTION-SCREEN FUNCTION KEY 3.  "Will have a function code of 'FC02'

SELECTION-SCREEN SKIP.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: PUSHBUTTON 25(45) marct USER-COMMAND marct
VISIBLE LENGTH 16.
SELECTION-SCREEN: END OF LINE.
*
*AT SELECTION-SCREEN ON s_cnfil.
*  PERFORM fm_at_selection_screen_s_cnfil.
*
*AT SELECTION-SCREEN ON s_cntran.
*  PERFORM fm_at_selection_screen_s_cntr.


**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.
  marct = 'Salvar'.

  FREE MEMORY ID 'ZLESR0154'.
  l_leave = 'LEAVE'.
  EXPORT l_leave FROM l_leave TO MEMORY ID 'ZLESR0154'.

  l_opcao                = '2'.

  l_sel_button-icon_id   = icon_dangerous_goods.
  l_sel_button-icon_text = 'Transbordo'.
  sscrfields-functxt_01  = l_sel_button.

  l_sel_button-icon_id   = icon_warehouse.
  l_sel_button-icon_text = 'Armazenagem'.
  sscrfields-functxt_02  = l_sel_button.

  l_sel_button-icon_id   = icon_work_center.
  l_sel_button-icon_text = 'Prestação Serviços'.
  sscrfields-functxt_03  = l_sel_button.

**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN.

  FREE MEMORY ID 'ZLESR0154'.
  FREE: l_leave.

  CASE sy-ucomm.
    WHEN 'FC01'.
      l_opcao = '3'.
      IF p_submit = 'ZLESR0154'.
        LEAVE PROGRAM.
      ELSE.
        SUBMIT zlesr0154 VIA SELECTION-SCREEN WITH p_submit = sy-cprog
                         AND RETURN.
        IMPORT l_leave TO l_leave FROM MEMORY ID 'ZLESR0154'.
        IF l_leave = 'LEAVE'.
          LEAVE PROGRAM.
        ENDIF.
      ENDIF.

    WHEN 'FC02'.
      l_opcao = '3'.
      IF p_submit = 'ZLESR0156'.
        LEAVE PROGRAM.
      ELSE.
        SUBMIT zlesr0156 VIA SELECTION-SCREEN WITH p_submit = sy-cprog
                         AND RETURN.

        IMPORT l_leave TO l_leave FROM MEMORY ID 'ZLESR0154'.
        IF l_leave = 'LEAVE'.
          LEAVE PROGRAM.
        ENDIF.
      ENDIF.

    WHEN 'FC03'.
      l_opcao = '3'.

    WHEN 'MARCT'.
      l_opcao = '3'.
PERFORM fm_start_of_selection.

  ENDCASE.


**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF p_lancto = abap_true.
      IF screen-group1(2) = 'T2'.
        screen-input     = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1(3) = 'T1A'.
        screen-required = 2.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF p_consul = abap_true.

      if s_bukrs2-low is not INITIAL and v_passou is INITIAL.
        v_passou = 'X'.
        SUBMIT zlesr0156_2                  WITH p_db_tab  = 'zlest0221'
                                            WITH p_STCNAM = 'zlest0221_OUT'
                                            WITH P_SCMANT = '0052'
                                            WITH p_TITLE = 'COCKPIT TRANSBORDO - Prestação de Serviços'
                                            WITH p_empres = s_bukrs2-low
                                            with p_CNPJ_F = s_cnfil2-low
                                            with p_CNPJ_P = s_cntra2-low
                                            WITH p_D_DE = s_data2-low
                                            WITH P_d_ate = s_data2-high
      AND  RETURN.

        endif.
      IF screen-group1(2) = 'T1'  OR
         screen-group1(3) = 'T1A'.
        screen-input     = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1(3) = 'T2A'.
        screen-required = 2.
        MODIFY SCREEN.
      ENDIF.
      if screen-name =  'MARCT'.
        screen-invisible = 1.
        MODIFY SCREEN.
        endif.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


AT SELECTION-SCREEN ON s_monat.
  PERFORM fm_at_selection_screen_s_monat.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cnfil-low.
  PERFORM fm_at_selection_screen_s_cnfil USING 'S_CNFIL-LOW' CHANGING s_cnfil-low .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cntran-low.
  PERFORM fm_at_selection_screen_s_cntr USING 'S_CNTRAN-LOW' CHANGING s_cntran-low.

*AT SELECTION-SCREEN.

  "PERFORM fm_at_selection_screen.


**********************************************************************
*start
**********************************************************************
START-OF-SELECTION.


  IF p_lancto = abap_true.
    IF s_bukrs IS INITIAL.
      MESSAGE s024(sd) WITH 'Informar campos obrigatórios.' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

  IF p_consul = abap_true.
    IF s_bukrs2 IS INITIAL OR
       s_data2  IS INITIAL.
      MESSAGE s024(sd) WITH 'Informar campos obrigatórios.' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

  CASE abap_true.
    WHEN p_lancto.
      "PERFORM fm_start_of_selection.

    WHEN p_consul.

      SUBMIT zlesr0156_2                  WITH p_db_tab  = 'zlest0221'
                                            WITH p_STCNAM = 'zlest0221_OUT'
                                            WITH P_SCMANT = '0052'
                                            WITH p_TITLE = 'COCKPIT TRANSBORDO - Prestação de Serviços'
                                            WITH p_empres = s_bukrs2-low
                                            with p_CNPJ_F = s_cnfil2-low
                                            with p_CNPJ_P = s_cntra2-low
                                            WITH p_D_DE = s_data2-low
                                            WITH P_d_ate = s_data2-high
      AND  RETURN.




*      PERFORM fm_consulta.
*
*      IF t_alv[] IS INITIAL.
*        MESSAGE s024(sd) WITH text-100 DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*
*      PERFORM fm_exibe_alv.
  ENDCASE.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.
*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen .


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  DATA: lva_ans  TYPE c.

  IF s_cnfil-low IS NOT INITIAL.
    SELECT SINGLE *
      INTO @DATA(lwa_1bbranch)
      FROM j_1bbranch
       WHERE bukrs  EQ @s_bukrs-low
         AND stcd1  EQ @s_cnfil-low .

    IF lwa_1bbranch-stcd1 IS INITIAL.
      MESSAGE i000(z01) WITH 'CNPJ não pertence a empresa informada'.
      STOP.
    ENDIF.
  ENDIF.

  IF s_monat-low > 12 OR s_monat-low < 1.
    MESSAGE i000(z01) WITH 'Mês informado não existe'.
    STOP.
  ENDIF.

  SELECT SINGLE *
    FROM lfa1 INTO @DATA(lwa_lfa1)
   WHERE stcd1 EQ @s_cntran-low.

  IF lwa_lfa1-lifnr IS INITIAL.
    MESSAGE i000(z01) WITH 'CNPJ Prestador de Serviço não encontrado'.
    STOP.
  ENDIF.


  SELECT SINGLE *
    INTO @DATA(lwa_zlest0221_aux)
    FROM zlest0221
     WHERE bukrs  EQ @s_bukrs-low
       AND cnpj_transbordo EQ  @s_cntran-low
       AND nfps EQ @s_nfps-low.

  IF lwa_zlest0221_aux IS INITIAL.

    DATA: lwa_zlest0221 TYPE zlest0221.

    lwa_zlest0221-bukrs = s_bukrs-low.
    lwa_zlest0221-cnpj_filial =  s_cnfil-low.
    lwa_zlest0221-nfps = s_nfps-low.
    lwa_zlest0221-datasaida = s_data-low.
    lwa_zlest0221-cnpj_transbordo = s_cntran-low.
    lwa_zlest0221-monat = s_monat-low.
    lwa_zlest0221-valor_servico = s_valor-low.

    lwa_zlest0221-zdt_atual = sy-datum.
    lwa_zlest0221-zhr_atual = sy-uzeit.
    lwa_zlest0221-z_usuario = sy-uname.

    MODIFY zlest0221 FROM lwa_zlest0221.

    COMMIT WORK.
    CLEAR: lwa_zlest0221.

    MESSAGE i000(z01) WITH 'NF Lançada com Sucesso!'.
    RETURN.

  ELSE.
    MESSAGE i000(z01) WITH 'Dados já estão cadastrados'.
    RETURN.
  ENDIF.


*  SELECT SINGLE *
*    INTO @DATA(lwa_zlest0221_aux)
*    FROM zlest0221
*     WHERE bukrs  EQ @s_bukrs-low
*       AND cnpj_transbordo EQ  @s_cntran-low
*       AND nfps EQ @s_nfps-low.
*
*  IF lwa_zlest0221_aux IS INITIAL.
*    SELECT SINGLE *
*      FROM lfa1 INTO @DATA(lwa_lfa1)
*    WHERE stcd1 EQ @s_cntran-low.
*
*    IF lwa_lfa1-lifnr IS NOT INITIAL.
*
*      SELECT SINGLE *
*        FROM rbkp INTO @DATA(lwa_rbkp)
*        WHERE stcd1 EQ @s_cntran-low
*            AND lifnr EQ @lwa_lfa1-lifnr
*            AND xblnr EQ @s_nfps-low
*            AND stblg EQ ''.
*
*      IF lwa_rbkp IS NOT INITIAL.

*        CONCATENATE lwa_rbkp-belnr lwa_rbkp-gjahr INTO DATA(lva_awkey).
*
*        SELECT SINGLE *
*          FROM bkpf INTO @DATA(lwa_bkpf)
*            WHERE bukrs EQ @lwa_rbkp-bukrs
*                AND gjahr EQ @lwa_rbkp-gjahr
*                AND awkey EQ @lva_awkey.
*
*        SELECT SINGLE *
*          FROM bsik INTO @DATA(lwa_bsik)
*            WHERE bukrs EQ @lwa_bkpf-bukrs
*                AND gjahr EQ @lwa_bkpf-gjahr
*                AND belnr EQ @lwa_bkpf-belnr.

*        IF lwa_bsik IS NOT INITIAL.

*          CONCATENATE 'NF' s_nfps-low 'não pago!' 'Deseja Lançar?' INTO DATA(lva_text) SEPARATED BY space.
*
*          CALL FUNCTION 'POPUP_TO_CONFIRM'
*            EXPORTING
*              titlebar              = 'Confirma ação'
*              text_question         = lva_text
*              text_button_1         = 'Sim'
*              icon_button_1         = 'ICON_CHECKED'
*              text_button_2         = 'Não'
*              icon_button_2         = 'ICON_CANCEL'
*              popup_type            = 'ICON_MESSAGE_ERROR'
*              display_cancel_button = ''
*            IMPORTING
*              answer                = lva_ans.
*
*          IF lva_ans = 1.

*            DATA: lwa_zlest0221 TYPE zlest0221.
*
*            lwa_zlest0221-bukrs = s_bukrs-low.
*            lwa_zlest0221-cnpj_filial =  s_cnfil-low.
*            lwa_zlest0221-nfps = s_nfps-low.
*            lwa_zlest0221-datasaida = s_data-low.
*            lwa_zlest0221-cnpj_transbordo = s_cntran-low.
*            lwa_zlest0221-monat = s_monat-low.
*            lwa_zlest0221-valor_servico = s_valor-low.
*
*            lwa_zlest0221-zdt_atual = sy-datum.
*            lwa_zlest0221-zhr_atual = sy-uzeit.
*            lwa_zlest0221-z_usuario = sy-uname.
*
*            MODIFY zlest0221 FROM lwa_zlest0221.
*
*            COMMIT WORK.
*            CLEAR: lwa_zlest0221.
*
*            MESSAGE i000(z01) WITH 'NF Lançada com Sucesso!'.
*            RETURN.

*        gva_bukrs = s_bukrs-low.
*        gva_cnpj_prest = s_cntran-low.
*        CONCATENATE s_data-low+6(2) '/' s_data-low+4(2) '/' s_data-low+0(4) INTO gva_data.
*        gva_mes = s_monat-low.
*        gva_valor = s_valor-low.
*
*        CALL SCREEN 1001 STARTING AT 10 08 ENDING AT 70 15.
*          ELSE.
*            EXIT.
*          ENDIF.
*        ELSE.
*          SELECT SINGLE *
*            FROM bsak INTO @DATA(lwa_bsak)
*            WHERE bukrs EQ @lwa_bkpf-bukrs
*               AND gjahr EQ @lwa_bkpf-gjahr
*          AND belnr EQ @lwa_bkpf-belnr.
*          IF lwa_bsik IS NOT INITIAL.
*            MESSAGE i000(z01) WITH 'NF Paga'.
*            RETURN.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        MESSAGE i000(z01) WITH 'Dados não encontrado para essa seleção'.
*        RETURN.
*      ENDIF.
*    ELSE.
*      MESSAGE i000(z01) WITH 'CNPJ Prestador de Serviço não encontrado'.
*      RETURN.
*    ENDIF.
*ELSE.
*  MESSAGE i000(z01) WITH 'Dados já estão cadastrados'.
*  RETURN.
*ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_consulta.

  DATA: l_awkey    TYPE bkpf-awkey.

  FREE: t_rbkp_sel, t_bkpf_sel, t_alv.

*------------------------------------------------
* selecao
*------------------------------------------------
  SELECT *
    FROM zlest0221
    INTO TABLE t_zlest0221
   WHERE bukrs           IN s_bukrs2
     AND cnpj_filial     IN s_cnfil2
     AND cnpj_transbordo IN s_cntra2
     AND datasaida       IN s_data2.

  CHECK t_zlest0221[] IS NOT INITIAL.

  SELECT lifnr, stcd1
    FROM lfa1
    INTO TABLE @DATA(t_lfa1)
     FOR ALL ENTRIES IN @t_zlest0221
   WHERE stcd1 = @t_zlest0221-cnpj_transbordo.

  SORT t_lfa1 BY stcd1.
  DELETE ADJACENT DUPLICATES FROM t_lfa1
                        COMPARING stcd1.

  LOOP AT t_zlest0221 INTO w_zlest0221.
    READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY stcd1 = w_zlest0221-cnpj_transbordo
                      BINARY SEARCH.
    CHECK sy-subrc = 0.
    w_rbkp_sel-lifnr     = w_lfa1-lifnr.
    w_rbkp_sel-xblnr     = w_zlest0221-nfps.
    w_rbkp_sel-stblg     = abap_off.
    APPEND w_rbkp_sel   TO t_rbkp_sel.
  ENDLOOP.

  IF  t_rbkp_sel[] IS NOT INITIAL.
    SELECT bukrs, belnr, gjahr, lifnr, xblnr, stblg
      FROM rbkp
      INTO TABLE @DATA(t_rbkp)
       FOR ALL ENTRIES IN @t_rbkp_sel
     WHERE lifnr = @t_rbkp_sel-lifnr
       AND xblnr = @t_rbkp_sel-xblnr
       AND stblg = @t_rbkp_sel-stblg.
  ENDIF.

  IF t_rbkp[] IS NOT INITIAL.
    SELECT belnr, gjahr, ebeln, matnr
      FROM ekbe
      INTO TABLE @DATA(t_ekbe)
       FOR ALL ENTRIES IN @t_rbkp
     WHERE belnr = @t_rbkp-belnr
       AND gjahr = @t_rbkp-gjahr.
  ENDIF.

  IF t_rbkp[] IS NOT INITIAL.
    SELECT matnr, maktx
      FROM makt
      INTO TABLE @DATA(t_makt)
       FOR ALL ENTRIES IN @t_ekbe
     WHERE matnr = @t_ekbe-matnr
       AND spras = @sy-langu.
  ENDIF.

  LOOP AT t_rbkp     INTO DATA(w_rbkp).
    w_bkpf_sel-bukrs    = w_rbkp-bukrs.
    w_bkpf_sel-gjahr    = w_rbkp-gjahr.
    w_bkpf_sel-awkey    = w_rbkp-belnr && w_rbkp-gjahr.
    APPEND w_bkpf_sel  TO t_bkpf_sel.
  ENDLOOP.

  IF t_bkpf_sel[] IS NOT INITIAL.
    SELECT bukrs, belnr, gjahr, awkey
      FROM bkpf
      INTO TABLE @DATA(t_bkpf)
       FOR ALL ENTRIES IN @t_bkpf_sel
     WHERE bukrs = @t_bkpf_sel-bukrs
       AND gjahr = @t_bkpf_sel-gjahr
       AND awkey = @t_bkpf_sel-awkey.
  ENDIF.

  IF t_bkpf[] IS NOT INITIAL.
    SELECT bukrs, belnr, gjahr
      FROM bsik
      INTO TABLE @DATA(t_bsik)
       FOR ALL ENTRIES IN @t_bkpf
     WHERE bukrs = @t_bkpf-bukrs
       AND belnr = @t_bkpf-belnr
       AND gjahr = @t_bkpf-gjahr.

    SELECT bukrs, belnr, gjahr, augbl, augdt
      FROM bsak
      INTO TABLE @DATA(t_bsak)
       FOR ALL ENTRIES IN @t_bkpf
     WHERE bukrs = @t_bkpf-bukrs
       AND belnr = @t_bkpf-belnr
       AND gjahr = @t_bkpf-gjahr.
  ENDIF.

*--------------------------------------------------------
* montar alv
*--------------------------------------------------------
  LOOP AT t_zlest0221 INTO w_zlest0221.
    FREE: w_alv,
          w_lfa1,
          w_rbkp.

    READ TABLE t_lfa1 INTO w_lfa1 WITH KEY stcd1 = w_zlest0221-cnpj_transbordo
                                  BINARY SEARCH.

    READ TABLE t_rbkp INTO w_rbkp WITH KEY lifnr = w_lfa1-lifnr
                                           xblnr = w_zlest0221-nfps
                                           stblg = abap_off.

    READ TABLE t_ekbe INTO DATA(w_ekbe) WITH KEY belnr = w_rbkp-belnr
                                                 gjahr = w_rbkp-gjahr.

    READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_ekbe-matnr.

    l_awkey = w_rbkp-belnr && w_rbkp-gjahr.

    READ TABLE t_bkpf INTO DATA(w_bkpf) WITH KEY bukrs = w_rbkp-bukrs
                                                 gjahr = w_rbkp-gjahr
                                                 awkey = l_awkey.

    LOOP AT t_bsik INTO DATA(w_bsik) WHERE bukrs = w_bkpf-bukrs
                                       AND belnr = w_bkpf-belnr
                                       AND gjahr = w_bkpf-gjahr.
    ENDLOOP.
    IF sy-subrc <> 0.
      LOOP AT t_bsak INTO DATA(w_bsak) WHERE bukrs = w_bkpf-bukrs
                                         AND belnr = w_bkpf-belnr
                                         AND gjahr = w_bkpf-gjahr.
        w_alv-augbl        = w_bsak-augbl.
        w_alv-augdt        = w_bsak-augdt.
        EXIT.
      ENDLOOP.
    ENDIF.

    w_alv-bukrs            = w_zlest0221-bukrs.
    w_alv-cnpj_filial      = w_zlest0221-cnpj_filial.
    w_alv-cnpj_transbordo  = w_zlest0221-cnpj_transbordo.
    w_alv-nfps             = w_zlest0221-nfps.
    w_alv-datasaida        = w_zlest0221-datasaida.
    w_alv-valor_servico    = w_zlest0221-valor_servico.
    w_alv-ebeln            = w_ekbe-ebeln.
    w_alv-belnr            = w_rbkp-belnr.
    w_alv-matnr            = w_ekbe-matnr.
    w_alv-maktx            = w_makt-maktx.

    APPEND w_alv          TO t_alv.

    FREE: w_ekbe, w_bkpf, w_makt.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_exibe_alv.

  CALL SCREEN 200.

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

  PERFORM f_fieldcatalog.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
  w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-zebra        = abap_false.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.
  w_layout-stylefname   = 'CELLSTYLES'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF g_grid IS INITIAL.
    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'HEADER'.

    PERFORM f_alv_header .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = g_custom_container
      EXCEPTIONS
        html_display_error = 1.

    "Grafico 1
    CALL METHOD cl_gui_cfw=>flush.

    CREATE OBJECT: g_custom_container
       EXPORTING
         container_name = 'CC_IMG',
         picture
       EXPORTING
         parent = g_custom_container.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = cl_container_95.
*    ENDIF.

    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_alv[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF lines( t_rows ) > 0.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

ENDFORM.

**********************************************************************
*  imagen
**********************************************************************
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
     01  ''      ''       'T_ALV'   'BUKRS'               'Empresa'                     '08'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     02  'LFA1'  'STCD1'  'T_ALV'   'CNPJ_FILIAL'         'CNPJ Filial'                 '16'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     03  'LFA1'  'STCD1'  'T_ALV'   'CNPJ_TRANSBORDO'     'CNPJ Prest.Serv.'            '16'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     04  ''      ''       'T_ALV'   'NFPS'                'NFPS'                        '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     05  ''      ''       'T_ALV'   'DATASAIDA'           'Data NFPS'                   '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     06  ''      ''       'T_ALV'   'VALOR_SERVICO'       'Valor NFPS'                  '16'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     07  ''      ''       'T_ALV'   'EBELN'               'Pedido'                      '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     08  ''      ''       'T_ALV'   'BELNR'               'Miro'                        '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     09  ''      ''       'T_ALV'   'AUGBL'               'Doc.Compensação'             '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     10  ''      ''       'T_ALV'   'AUGDT'               'Data Compensação'            '17'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     11  'MARA'  'MATNR'  'T_ALV'   'MATNR'               'Cód.Mat.Pedido'              '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     12  ''      ''       'T_ALV'   'MAKTX'               'Descr.Mat.Pedido'            '50'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).                                    "16

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
*  cabecalho
**********************************************************************
FORM f_alv_header .

  DATA: wl_data1(10),
        wl_data2(10),
        wl_hora(8),
        wl_linha(60),
        wl_text TYPE sdydo_text_element.

  DATA: wa_t001       TYPE t001,
        wa_j_1bbranch TYPE j_1bbranch.

  wl_linha = text-123.

  wl_text  = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

*  CALL METHOD obj_dyndoc_id->new_line.
*
*  IF s_bukrs[] IS NOT INITIAL.
*    CONCATENATE  'Empresa....:' s_bukrs2-low
*           INTO wl_linha SEPARATED BY space.
*
*    wl_text = wl_linha.
*    CALL METHOD obj_dyndoc_id->new_line.
*
*    CALL METHOD obj_dyndoc_id->add_text
*      EXPORTING
*        text         = wl_text
*        sap_fontsize = cl_dd_area=>list_normal.
*  ENDIF.
*
*  IF s_dnfsv2[] IS NOT INITIAL.
*    READ TABLE s_dnfsv2 INDEX 1.
*
*    wl_data1 = s_dnfsv2-low+6(2)  && '.' && s_dnfsv2-low+4(2)  && '.' && s_dnfsv2-low(4).
*    wl_data2 = s_dnfsv2-high+6(2) && '.' && s_dnfsv2-high+4(2) && '.' && s_dnfsv2-high(4).
*
*    IF s_dnfsv2-high IS NOT INITIAL.
*      CONCATENATE  'Dt.Emiss.NF Serviço..:' wl_data1 'a' wl_data2
*             INTO wl_linha SEPARATED BY space.
*    ELSE.
*      CONCATENATE  'Dt.Emiss.NF Serviço,,:' wl_data1
*             INTO wl_linha SEPARATED BY space.
*    ENDIF.
*
*    wl_text = wl_linha.
*    CALL METHOD obj_dyndoc_id->new_line.
*
*    CALL METHOD obj_dyndoc_id->add_text
*      EXPORTING
*        text         = wl_text
*        sap_fontsize = cl_dd_area=>list_normal.
*  ENDIF.

ENDFORM.                    " ZF_ALV_HEADER

*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
  SET PF-STATUS 'PF1001'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.
  CASE sy-ucomm.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN  'ENTER'.
      PERFORM fm_salva_dados.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_SALVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_salva_dados .

*  DATA: lwa_zlest0221 TYPE zlest0221.
*
*  lwa_zlest0221-bukrs = s_bukrs-low.
*  lwa_zlest0221-cnpj_filial = gva_cnpj_filial.
*  lwa_zlest0221-nfps = s_nfps-low.
*  lwa_zlest0221-datasaida = s_data-low.
*  lwa_zlest0221-cnpj_transbordo = s_cntran-low.
*  lwa_zlest0221-monat = s_monat-low.
*  lwa_zlest0221-valor_servico = s_valor-low.
*
*  MODIFY zlest0221 FROM lwa_zlest0221.
*
*  COMMIT WORK.
*
*  CLEAR: lwa_zlest0221.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN_S_CNFIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen_s_cnfil USING fieldname    TYPE dynpread-fieldname
                                    CHANGING s_cnfil TYPE zlest0221-cnpj_filial.


  DATA: it_dynfields TYPE TABLE OF dynpread,
        st_dynfields LIKE LINE OF it_dynfields.

  CHECK p_lancto = abap_true.

  st_dynfields-fieldname = 'S_BUKRS-LOW'.
  APPEND st_dynfields TO it_dynfields.

  st_dynfields-fieldname = 'S_CNFIL-LOW'.
  APPEND st_dynfields TO it_dynfields.


  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
      translate_to_upper   = abap_true
    TABLES
      dynpfields           = it_dynfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.


  DATA:  lwa_1bbranch TYPE j_1bbranch.
  FIELD-SYMBOLS: <st_dynfields> LIKE LINE OF it_dynfields .

  LOOP AT it_dynfields ASSIGNING <st_dynfields> .
    CASE <st_dynfields>-fieldname .
      WHEN 'S_BUKRS-LOW' .
        lwa_1bbranch-bukrs = <st_dynfields>-fieldvalue .
    ENDCASE.
  ENDLOOP.


  DATA: BEGIN OF it_1bbranch OCCURS 0,
          bukrs  TYPE j_1bbranch-bukrs,
          branch TYPE j_1bbranch-branch,
          name   TYPE j_1bbranch-name,
          stcd1  TYPE j_1bbranch-stcd1,
        END OF it_1bbranch.

  SELECT DISTINCT bukrs branch name stcd1 INTO TABLE it_1bbranch
  FROM j_1bbranch
   WHERE bukrs  EQ lwa_1bbranch-bukrs.


  DATA: it_return TYPE TABLE OF ddshretval WITH HEADER LINE .

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'STCD1'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'J_1BBRANCH-STCD1'
      value_org       = 'S'
    TABLES
      value_tab       = it_1bbranch
      return_tab      = it_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.


  READ TABLE it_return INDEX 1 .
  CHECK sy-subrc EQ 0 .

  LOOP AT it_dynfields ASSIGNING <st_dynfields> .
    CASE <st_dynfields>-fieldname .
      WHEN fieldname .
        <st_dynfields>-fieldvalue = it_return-fieldval .
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP .

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = it_dynfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  MOVE: lwa_1bbranch-name TO gva_desc_branch.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN_S_CNTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen_s_cntr USING fieldname    TYPE dynpread-fieldname
                                    CHANGING s_cntran TYPE zlest0221-cnpj_filial.

  DATA: it_dynfields TYPE TABLE OF dynpread,
        st_dynfields LIKE LINE OF it_dynfields.

  CHECK p_lancto = abap_true.

  st_dynfields-fieldname = 'S_CNTRAN-LOW'.
  APPEND st_dynfields TO it_dynfields.


  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
      translate_to_upper   = abap_true
    TABLES
      dynpfields           = it_dynfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.


  DATA:  lwa_stcd1 TYPE lfa1.
  FIELD-SYMBOLS: <st_dynfields> LIKE LINE OF it_dynfields .

  LOOP AT it_dynfields ASSIGNING <st_dynfields> .
    CASE <st_dynfields>-fieldname .
      WHEN 'S_CNTRAN-LOW' .
        lwa_stcd1-stcd1 = <st_dynfields>-fieldvalue .
    ENDCASE.
  ENDLOOP.

  DATA: BEGIN OF it_lfa1 OCCURS 0,
          lifnr TYPE lfa1-lifnr,
          name1 TYPE lfa1-name1,
          stcd1 TYPE lfa1-stcd1,
        END OF it_lfa1.

  SELECT DISTINCT lifnr name1 stcd1 INTO TABLE it_lfa1
  FROM lfa1
   WHERE stcd1 <> ''.

  DATA: it_return TYPE TABLE OF ddshretval WITH HEADER LINE .

  SORT it_lfa1 BY lifnr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'STCD1'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'LFA1-STCD1'
      value_org       = 'S'
    TABLES
      value_tab       = it_lfa1
      return_tab      = it_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.


  READ TABLE it_return INDEX 1 .
  CHECK sy-subrc EQ 0 .

  LOOP AT it_dynfields ASSIGNING <st_dynfields> .
    CASE <st_dynfields>-fieldname .
      WHEN fieldname .
        <st_dynfields>-fieldvalue = it_return-fieldval .
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP .

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = it_dynfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN_S_MONAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen_s_monat .

  CHECK p_lancto = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.


  SET PF-STATUS 'ZLESR0154'.
  SET TITLEBAR  'ZLESR0154'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  FREE: t_rows[].

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.
