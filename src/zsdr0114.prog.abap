*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Contratos Prestação Serviços de Fretes                  *
* Transação..: ZSDT0166                                                *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
REPORT zsdr0114.

**----------------------------------------------------------------------*
** Tables --------------------------------------------------------------*
**----------------------------------------------------------------------*
TABLES: t001, kna1, zsdt0169_seq.

**----------------------------------------------------------------------*
** Variaveis -----------------------------------------------------------*
**----------------------------------------------------------------------*
DATA: v_ucomm1 LIKE sy-ucomm,
      v_ucomm2 LIKE sy-ucomm,
      v_ucomm3 LIKE sy-ucomm,
      v_ucomm4 LIKE sy-ucomm,
      v_ucomm  TYPE sy-ucomm,
      v_valida TYPE c,
      v_salvou TYPE c.

**----------------------------------------------------------------------*
** Tipos ---------------------------------------------------------------*
**----------------------------------------------------------------------*
TYPES: BEGIN OF ty_tela.
         INCLUDE TYPE zsdt0169_seq.
         TYPES: wgbez TYPE t023t-wgbez60.
TYPES: desc_cliente TYPE kna1-name1.
TYPES: desc_coleta  TYPE lfa1-name1.
TYPES: desc_entrega TYPE kna1-name1.
TYPES: cliente_tela TYPE kunnr.
TYPES: desc_zterm   TYPE text1_052.
TYPES: END OF ty_tela.

TYPES: BEGIN OF ty_tela_cabec.
         INCLUDE TYPE zsdt0244.
         TYPES: desc_emp TYPE butxt.
TYPES: cnpj     TYPE stcd1.
TYPES: desc_cliente TYPE kna1-name1.
TYPES: END OF ty_tela_cabec.


**----------------------------------------------------------------------*
** Tabelas internas ----------------------------------------------------*
**----------------------------------------------------------------------*
DATA: t_consultar TYPE TABLE OF zsds021,
      t_editar    TYPE TABLE OF zsdt0169_seq,
      t_saida     TYPE TABLE OF zsds022,
      t_fieldcat  TYPE   slis_t_fieldcat_alv,
      t_fieldcat2 TYPE lvc_t_fcat,
      t_zsdt0244  TYPE TABLE OF zsdt0244,
      t_kna1      TYPE TABLE OF kna1.

*----------------------------------------------------------------------*
* Estruturas ----------------------------------------------------------*
*----------------------------------------------------------------------*
DATA: wa_layout  TYPE slis_layout_alv,
      wa_layout2 TYPE lvc_s_layo,
      wa_tela    TYPE ty_tela,
      wa_cabec   TYPE ty_tela_cabec,
      wa_toolbar TYPE stb_button.

*---------------------------------------------------------------------
*
* Classes locais (Definição)
*
*---------------------------------------------------------------------
*
CLASS lcl_grid_event DEFINITION.
* seção publica
  PUBLIC SECTION.
*...Barra de Ferramentas
    METHODS handle_toolbar
                FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.
*...User Command
    METHODS handle_command_grid
                FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
ENDCLASS. "LCL_GRID_EVENT DEFINITION
*---------------------------------------------------------------------
*
* Classes locais (Implementação)
*
*---------------------------------------------------------------------
*
CLASS lcl_grid_event IMPLEMENTATION.
  METHOD handle_toolbar.
*...Barra de Ferramentas
    PERFORM f_toolbar_grid CHANGING e_object.
  ENDMETHOD. "handle_toolba
  METHOD handle_command_grid.
*...Rotinas do botão Z da barra de ferramentas do ALV
    v_ucomm = e_ucomm.
    PERFORM f_command USING e_ucomm.
  ENDMETHOD. "handle_command_grid
ENDCLASS. "LCL_GRID_EVENT IMPLEMENTATION
*---------------------------------------------------------------------
*
* Classes ------------------------------------------------------------
*
*---------------------------------------------------------------------
*
DATA: lcl_alv           TYPE REF TO cl_gui_alv_grid,
      lcl_container_alv TYPE REF TO cl_gui_custom_container,
      lcl_event         TYPE REF TO lcl_grid_event.

**----------------------------------------------------------------------*
** Tela de seleção -----------------------------------------------------*
**----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 0200 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bu_1 FOR t001-bukrs NO INTERVALS NO-EXTENSION,
                s_ku_1 FOR kna1-kunnr,
                s_dt_1 FOR sy-datum.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 0200.

SELECTION-SCREEN BEGIN OF SCREEN 0300 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_bu_2 FOR t001-bukrs NO INTERVALS NO-EXTENSION,
                s_ku_2 FOR kna1-kunnr NO INTERVALS NO-EXTENSION,
                s_dt_2 FOR sy-datum DEFAULT sy-datum NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF SCREEN 0300.

SELECTION-SCREEN BEGIN OF SCREEN 0400 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_bu_3 FOR t001-bukrs NO INTERVALS NO-EXTENSION,
                s_ku_3 FOR kna1-kunnr NO INTERVALS NO-EXTENSION,
                s_dt_3 FOR sy-datum DEFAULT sy-datum NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF SCREEN 0400.

SELECTION-SCREEN BEGIN OF SCREEN 0600 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-005.
SELECT-OPTIONS: s_bu_4 FOR t001-bukrs,
                s_ku_4 FOR kna1-kunnr,
                s_ano_4 FOR zsdt0169_seq-ano NO INTERVALS NO-EXTENSION .
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN END OF SCREEN 0600.

SELECTION-SCREEN BEGIN OF TABBED BLOCK tabs FOR 6 LINES.
SELECTION-SCREEN TAB (18) tab1 USER-COMMAND v_ucomm1 DEFAULT SCREEN 0200.
SELECTION-SCREEN TAB (30) tab2 USER-COMMAND v_ucomm2 DEFAULT SCREEN 0300.
SELECTION-SCREEN TAB (18) tab3 USER-COMMAND v_ucomm3 DEFAULT SCREEN 0400.
SELECTION-SCREEN TAB (18) tab4 USER-COMMAND v_ucomm4 DEFAULT SCREEN 0600.
SELECTION-SCREEN END OF BLOCK tabs.

AT SELECTION-SCREEN.

  CHECK sy-ucomm <> 'V_UCOMM1' AND
 sy-ucomm <> 'V_UCOMM2' AND
 sy-ucomm <> 'V_UCOMM3' AND
 sy-ucomm <> 'V_UCOMM4' .

  IF sy-dynnr = '0300'.
    PERFORM f_seleciona_0800.
  ENDIF.

  IF sy-dynnr = '0400'.
    PERFORM f_nova_programacao.
  ENDIF.

  IF sy-dynnr = '0600'.
    PERFORM f_alv_editar.
  ENDIF.


INITIALIZATION.
  tab1 = 'Consultar Contrato'(t01).
  tab2 = 'Novo Contrato'(t02).
  tab3 = 'Nova Programação'(t03).
  tab4 = 'Editar Programação'(t04).

  IF s_bu_1[] IS INITIAL.
    s_bu_1 = VALUE #(
     sign   = 'I'
     option = 'EQ'
     low    = |{ '0001' }| ).
    APPEND s_bu_1.
  ENDIF.

  IF s_bu_2[] IS INITIAL.
    s_bu_2 = VALUE #(
     sign   = 'I'
     option = 'EQ'
     low    = |{ '0001' }| ).
    APPEND s_bu_2.
  ENDIF.

  IF s_bu_3[] IS INITIAL.
    s_bu_3 = VALUE #(
     sign   = 'I'
     option = 'EQ'
     low    = |{ '0001' }| ).
    APPEND s_bu_3.
  ENDIF.

  IF s_bu_4[] IS INITIAL.
    s_bu_4 = VALUE #(
     sign   = 'I'
     option = 'EQ'
     low    = |{ '0001' }| ).
    APPEND s_bu_4.
  ENDIF.
*----------------------------------------------------------------------*
* Start-of-Selection --------------------------------------------------*
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CASE tabs-dynnr.
    WHEN '0200'.      "Consulta
      PERFORM f_seleciona_0200.
      PERFORM f_alv_consulta.
    WHEN '0300'.      "Novo
*      PERFORM F_SELECIONA_0800.
    WHEN '0400'.      "Nova Programação
*      PERFORM F_NOVA_PROGRAMACAO.
    WHEN '0600'.      "Editar Programação
*      PERFORM F_ALV_EDITAR.

  ENDCASE.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_0200
*&---------------------------------------------------------------------*
FORM f_seleciona_0200 .

  SELECT *
    FROM zsdt0244
    INTO TABLE t_zsdt0244
   WHERE bukrs      IN s_bu_1
     AND id_cliente IN s_ku_1
     AND data_atual IN s_dt_1.

  IF sy-subrc NE 0.
    MESSAGE s000(z_les) WITH text-004 DISPLAY LIKE 'S'.
    STOP.
  ENDIF.


  SELECT *
    FROM kna1
    INTO TABLE t_kna1
     FOR ALL ENTRIES IN t_zsdt0244
   WHERE kunnr = t_zsdt0244-id_cliente.


  LOOP AT t_zsdt0244 ASSIGNING FIELD-SYMBOL(<fs_zsdt0244>).

    APPEND INITIAL LINE TO t_consultar ASSIGNING FIELD-SYMBOL(<fs_consultar>).

    <fs_consultar>-ano    = <fs_zsdt0244>-ano.
    <fs_consultar>-data   = <fs_zsdt0244>-data_atual.
    <fs_consultar>-bukrs  = <fs_zsdt0244>-bukrs.
    <fs_consultar>-id_ctr = <fs_zsdt0244>-id_ctr.
    <fs_consultar>-kunnr  = <fs_zsdt0244>-id_cliente.

    READ TABLE t_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY kunnr = <fs_zsdt0244>-id_cliente.

    IF <fs_kna1> IS ASSIGNED.
      <fs_consultar>-name1 = <fs_kna1>-name1.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_CONSULTA
*&---------------------------------------------------------------------*
FORM f_alv_consulta .

  DATA: lv_program TYPE sy-repid.

  lv_program  = sy-repid.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = lv_program
      i_structure_name       = 'ZSDS021'
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  wa_layout-expand_all = abap_true.
  wa_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = lv_program
      is_layout          = wa_layout
      it_fieldcat        = t_fieldcat
    TABLES
      t_outtab           = t_consultar
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.

  SET PF-STATUS 'PF_0500'.
  SET TITLEBAR 'TITULO_0500'.

  IF v_salvou = abap_true.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'WA_TELA-ID_CLIENTE'    OR
             'WA_TELA-PC_CODIGO'     OR
             'WA_TELA-LR_CODIGO'     OR
             'WA_TELA-GR_MERCADORIA' OR
             'WA_TELA-QUANTIDADE'    OR
             'WA_TELA-UNID_QUANT'    OR
             'WA_TELA-DMBTR'         OR
             'WA_TELA-UNID_TARIFA'   OR
             'WA_TELA-ZTERM'.
          screen-input = 0.
          MODIFY SCREEN.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK'.
      CLEAR: v_salvou.
      SET SCREEN 0.

    WHEN 'CANC' OR 'EXIT'.
      CLEAR: v_salvou.
      SET SCREEN 0.

    WHEN 'ATUALIZAR'.

      PERFORM f_atualiza_tela_0500.

    WHEN 'SALVAR'.

      PERFORM f_salvar_zsdt0169_seq.

    WHEN OTHERS.

      PERFORM f_atualiza_tela_0500.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_SALVAR_ZSDT0169_SEQ
*&---------------------------------------------------------------------*
FORM f_salvar_zsdt0169_seq .

  DATA: v_id_ctr_seq TYPE zde_id_ctr_seq.

*  CLEAR: V_SALVOU.

  IF v_salvou IS INITIAL.

    IF wa_tela-id_cliente    IS INITIAL OR
       wa_tela-gr_mercadoria IS INITIAL OR
       wa_tela-quantidade    IS INITIAL OR
       wa_tela-unid_quant    IS INITIAL OR
       wa_tela-dmbtr         IS INITIAL OR
       wa_tela-unid_tarifa   IS INITIAL OR
       wa_tela-zterm         IS INITIAL OR
       wa_tela-pc_codigo     IS INITIAL OR
       wa_tela-lr_codigo     IS INITIAL.
      MESSAGE s000(z_les) WITH text-010 DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.

* Valida se cadastro já existe.
*  SELECT SINGLE *
*    FROM ZSDT0169_SEQ
*   WHERE ID_CLIENTE  = @WA_TELA-ID_CLIENTE
*     AND BUKRS       = @WA_TELA-BUKRS
*    INTO @DATA(WA_ZSDT0169_SEQ).
*
*  IF SY-SUBRC IS INITIAL.
*    MESSAGE S000(Z_LES) WITH TEXT-021 TEXT-022 DISPLAY LIKE 'S'.
*    RETURN.
*  ENDIF.

    SELECT MAX( id_ctr_seq )
      FROM zsdt0169_seq
      INTO v_id_ctr_seq
     WHERE  ano = sy-datum(4).

    wa_tela-id_ctr_seq = v_id_ctr_seq + 1.
    wa_tela-data = sy-datum.
    wa_tela-hora = sy-uzeit.
    CONDENSE wa_tela-id_ctr_seq.

* Valida  cliente x cliente
    SELECT SINGLE *
      FROM kna1
     WHERE kunnr = @wa_tela-cliente_tela
      INTO @DATA(wa_kunnr_1).

    SELECT SINGLE *
      FROM kna1
     WHERE kunnr = @wa_tela-id_cliente
      INTO @DATA(wa_kunnr_2).

    IF wa_kunnr_1-stcd1 IS NOT INITIAL AND
       wa_kunnr_2-stcd1 IS NOT INITIAL.
      IF wa_kunnr_1-stcd1(8) NE wa_kunnr_2-stcd1(8).
        MESSAGE s000(z_les) WITH text-015 DISPLAY LIKE 'S'.
        RETURN.
      ENDIF.
    ENDIF.

    IF wa_kunnr_1-stcd2 IS NOT INITIAL AND
       wa_kunnr_2-stcd2 IS NOT INITIAL.
      IF wa_kunnr_1-stcd2 NE wa_kunnr_2-stcd2.
        MESSAGE s000(z_les) WITH text-015 DISPLAY LIKE 'S'.
        RETURN.
      ENDIF.
    ENDIF.

    IF wa_kunnr_1-stcd1 IS NOT INITIAL AND
       wa_kunnr_2-stcd2 IS NOT INITIAL.
      MESSAGE s000(z_les) WITH text-017 DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.

    IF wa_kunnr_1-stcd2 IS NOT INITIAL AND
       wa_kunnr_2-stcd1 IS NOT INITIAL.
      MESSAGE s000(z_les) WITH text-017 DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.

    MODIFY zsdt0169_seq FROM wa_tela.
    COMMIT WORK.
    MESSAGE s000(z_les) WITH text-009 DISPLAY LIKE 'S'.
    v_salvou = abap_true.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0700  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0700 OUTPUT.

  SET PF-STATUS 'PF_0700'.
  SET TITLEBAR 'TITULO_700'.

  IF v_ucomm = 'VISUALIZAR'.
    LOOP AT SCREEN.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0700  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0700 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      SET SCREEN 0.
    WHEN 'EDITAR'.
      PERFORM f_editar_zsdt0169_seq.
      PERFORM f_atualiza_alv.
      SET SCREEN 900.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_EDITAR
*&---------------------------------------------------------------------*
FORM f_alv_editar .

  DATA: r_id_ctr_seq TYPE RANGE OF zsdt0169_seq-id_ctr_seq.

  SELECT *
    FROM zsdt0169_seq
    INTO TABLE t_editar
   WHERE bukrs      IN s_bu_4
     AND id_cliente IN s_ku_4
     AND id_ctr_seq IN r_id_ctr_seq
     AND ano        IN s_ano_4.

  IF sy-subrc NE 0.
    MESSAGE e000(z_les) WITH text-004. " DISPLAY LIKE 'S'.
    STOP.
  ENDIF.

  MOVE-CORRESPONDING t_editar[] TO t_saida[].

  CALL SCREEN 0900.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_NOVA_PROGRAMACAO
*&---------------------------------------------------------------------*
FORM f_nova_programacao .

* Novo
  IF s_bu_3[] IS INITIAL OR
     s_ku_3[] IS INITIAL OR
     s_dt_3[] IS INITIAL.

    IF s_bu_3[] IS INITIAL.
      MESSAGE e000(z_les) WITH 'Informe código Empresa Prest. Serviço'(V01).
    ENDIF.

    IF s_ku_3[] IS INITIAL.
      MESSAGE e000(z_les) WITH 'Informe Código do Cliente'(V02).
    ENDIF.

    IF s_dt_3[] IS INITIAL.
      MESSAGE e000(z_les) WITH 'Campo data obrigatório'(V03).
    ENDIF.

  ELSE.

    SELECT SINGLE bukrs
      FROM t001
     WHERE bukrs IN @s_bu_3
      INTO @DATA(vl_bukrs).

    IF sy-subrc NE 0.
      MESSAGE e000(z_les) WITH text-006." DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM knb1
     WHERE kunnr IN @s_ku_3
       AND bukrs IN @s_bu_3
      INTO @DATA(wa_knb1).

    IF sy-subrc NE 0.
      MESSAGE e000(z_les) WITH text-007." DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0244
     WHERE bukrs      = @wa_knb1-bukrs
       AND id_cliente = @wa_knb1-kunnr
       AND ano        = @s_dt_3-low(4)
      INTO @DATA(wa_zsdt0244).

    IF sy-subrc NE 0.
      MESSAGE e000(z_les) WITH text-013 text-014." DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.


    wa_tela-id_ctr = wa_zsdt0244-id_ctr.
    wa_tela-ano    = wa_zsdt0244-ano.
    wa_tela-cliente_tela = wa_knb1-kunnr.
    wa_tela-bukrs        = wa_knb1-bukrs.
    CALL SCREEN 0500.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_TELA_0500
*&---------------------------------------------------------------------*
FORM f_atualiza_tela_0500 .

  SELECT SINGLE stcd1, stcd2, name1
    FROM kna1
   WHERE kunnr = @wa_tela-id_cliente
    INTO @DATA(wa_kna1).

  IF sy-subrc IS INITIAL.
    wa_tela-cliente_cnpj = wa_kna1-stcd1.
    wa_tela-cliente_cpf  = wa_kna1-stcd2.
    wa_tela-desc_cliente = wa_kna1-name1.
  ENDIF.

  SELECT SINGLE wgbez
    FROM t023t
   WHERE spras = @sy-langu
     AND matkl = @wa_tela-gr_mercadoria
    INTO @DATA(vl_wgbez).

  IF sy-subrc IS INITIAL.
    wa_tela-wgbez = vl_wgbez.
  ENDIF.

  SELECT SINGLE *
    FROM lfa1
   WHERE lifnr = @wa_tela-pc_codigo
    INTO @DATA(wa_lfa1).

  IF sy-subrc IS INITIAL.
    wa_tela-pc_cpf      = wa_lfa1-stcd2.
    wa_tela-pc_cnpj     = wa_lfa1-stcd1.
    wa_tela-desc_coleta = wa_lfa1-name1.
    wa_tela-pc_endereco = wa_lfa1-stras.
    wa_tela-pc_local    = wa_lfa1-ort01.
    wa_tela-pc_zona     = wa_lfa1-lzone.
    wa_tela-pc_uf       = wa_lfa1-regio.
  ENDIF.


  SELECT SINGLE *
    FROM kna1
   WHERE kunnr = @wa_tela-lr_codigo
    INTO @DATA(wa_kna1_2).

  IF sy-subrc IS INITIAL.
    wa_tela-lr_cpf       = wa_kna1_2-stcd2.
    wa_tela-lr_cnpj      = wa_kna1_2-stcd1.
    wa_tela-desc_entrega = wa_kna1_2-name1.
    wa_tela-lr_endereco  = wa_kna1_2-stras.
    wa_tela-lr_local     = wa_kna1_2-ort01.
    wa_tela-lr_zona      = wa_kna1_2-lzone.
    wa_tela-lr_uf        = wa_kna1_2-regio.
  ENDIF.

  SELECT SINGLE *
    FROM trolz
   WHERE aland = 'BR'
     AND azone = @wa_lfa1-lzone
     AND lland = 'BR'
     AND lzone = @wa_kna1_2-lzone
     INTO @DATA(wa_trolz).

  wa_tela-route = wa_trolz-route.

  SELECT SINGLE *
    FROM t052u
   WHERE spras = @sy-langu
     AND zterm = @wa_tela-zterm
    INTO @DATA(wa_t052u).

  wa_tela-desc_zterm = wa_t052u-text1.

ENDFORM.

FORM f_command USING p_ucomm TYPE sy-ucomm.

  CASE v_ucomm.

    WHEN 'EDITAR' OR 'VISUALIZAR'.

      PERFORM f_valida_linhas_editar.

      IF v_valida = 1.

        CLEAR: wa_tela.
        READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) WITH KEY selecionar = abap_true.
        MOVE-CORRESPONDING <fs_saida> TO wa_tela.

        PERFORM zf_buscar_descricao.

        CALL SCREEN 0700.

      ENDIF.

  ENDCASE.

ENDFORM. " F_command

FORM f_toolbar_grid CHANGING p_object TYPE REF TO cl_alv_event_toolbar_set.

  CLEAR wa_toolbar.
  MOVE: 'VISUALIZAR' TO wa_toolbar-function,
  icon_display TO wa_toolbar-icon,
  'Visualizar Programação'(023) TO wa_toolbar-text,
  space TO wa_toolbar-disabled.
  APPEND wa_toolbar TO p_object->mt_toolbar.

  CLEAR wa_toolbar.
  MOVE: 'EDITAR' TO wa_toolbar-function,
  icon_change TO wa_toolbar-icon,
  text-008 TO wa_toolbar-text,
  space TO wa_toolbar-disabled.
  APPEND wa_toolbar TO p_object->mt_toolbar.


  DELETE p_object->mt_toolbar WHERE function <> 'EDITAR'
                                AND function <> 'VISUALIZAR'
                                AND function <> '&MB_EXPORT'
                                AND function <> '&SORT_ASC'
                                AND function <> '&SORT_DSC'
                                AND function <> '&PRINT_BACK'.

ENDFORM. " F_toolbar_grid
*&---------------------------------------------------------------------*
*&      Form  F_EDITAR_ZSDT0169_SEQ
*&---------------------------------------------------------------------*
FORM f_editar_zsdt0169_seq.

  CLEAR: v_salvou.
  IF wa_tela-quantidade IS INITIAL.
    MESSAGE e000(z_les) WITH text-010." DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  wa_tela-mandt = sy-mandt.
  MODIFY zsdt0169_seq FROM wa_tela.
  COMMIT WORK.
  MESSAGE s000(z_les) WITH text-009. " DISPLAY LIKE 'S'.
  v_salvou = abap_true.

  READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) WITH KEY selecionar = abap_true.

  IF sy-subrc IS INITIAL.
    <fs_saida>-quantidade = wa_tela-quantidade.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0800 OUTPUT.
  SET PF-STATUS 'PF_0800'.
  SET TITLEBAR 'TITULO_0800'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0800 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      SET SCREEN 0.

    WHEN 'SALVAR'.

      PERFORM f_salvar_novo.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_SALVAR_NOVO
*&---------------------------------------------------------------------*
FORM f_salvar_novo .

  SELECT SINGLE bukrs
    FROM t001
   WHERE bukrs = @wa_cabec-bukrs
    INTO @DATA(vl_bukrs).

  IF sy-subrc NE 0.
    MESSAGE s000(z_les) WITH text-006 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM knb1
   WHERE kunnr = @wa_cabec-id_cliente
     AND bukrs = @wa_cabec-bukrs
    INTO @DATA(wa_knb1).

  IF sy-subrc NE 0.
    MESSAGE s000(z_les) WITH text-007 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  IF wa_cabec-data_atual IS INITIAL.
    wa_cabec-data_atual = sy-datum.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0244
   WHERE bukrs      = @wa_cabec-bukrs
     AND id_cliente = @wa_cabec-id_cliente
     AND ano        = @wa_cabec-data_atual(4)
    INTO @DATA(wa_zsdt0244).

  IF sy-subrc IS INITIAL.
    MESSAGE s000(z_les) WITH text-011 text-012 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  IF wa_cabec-usnam IS INITIAL.
    wa_cabec-usnam = sy-uname.
  ENDIF.

  IF wa_cabec-id_ctr     IS INITIAL OR
     wa_cabec-bukrs      IS INITIAL OR
     wa_cabec-id_cliente IS INITIAL.
    MESSAGE s000(z_les) WITH text-010 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.


  wa_cabec-ano        = wa_cabec-data_atual(4).
  wa_cabec-mandt      = sy-mandt.
  wa_cabec-hora_atual = sy-uzeit.
  MODIFY zsdt0244 FROM wa_cabec.
  COMMIT WORK.
  MESSAGE s000(z_les) WITH text-009 DISPLAY LIKE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_0800
*&---------------------------------------------------------------------*
FORM f_seleciona_0800 .

  DATA: lv_nr_range_nr TYPE inri-nrrangenr,
        lv_object      TYPE inri-object,
        lv_quantity    TYPE inri-quantity,
        lv_number      TYPE i.

* Novo
  IF s_bu_2[] IS INITIAL OR
     s_dt_2[] IS INITIAL.

    IF s_bu_2[] IS INITIAL.
      MESSAGE e000(z_les) WITH 'Informe código Empresa Prest. Serviço'(V01).
    ENDIF.

    IF s_dt_2[] IS INITIAL.
      MESSAGE e000(z_les) WITH 'Campo data obrigatório'(V03).
    ENDIF.

  ELSE.

    SELECT SINGLE bukrs
      FROM t001
     WHERE bukrs IN @s_bu_2
      INTO @DATA(vl_bukrs).

    IF sy-subrc NE 0.
      MESSAGE s000(z_les) WITH text-006 DISPLAY LIKE 'S'.
      STOP.
    ENDIF.

    wa_cabec-bukrs      = s_bu_2-low.
    wa_cabec-data_atual = s_dt_2-low.
    wa_cabec-usnam      = sy-uname.
    wa_cabec-id_cliente = s_ku_2-low.

    SELECT SINGLE butxt
      FROM t001
     WHERE bukrs = @wa_cabec-bukrs
      INTO @DATA(vl_butxt).

    wa_cabec-desc_emp = vl_butxt.

    IF wa_cabec-id_cliente IS NOT INITIAL.

      SELECT SINGLE name1, stcd1, stcd2
        FROM kna1
       WHERE kunnr = @wa_cabec-id_cliente
        INTO @DATA(wa_kna1).

      wa_cabec-desc_cliente = wa_kna1-name1.

      IF wa_kna1-stcd1 IS NOT INITIAL.
        wa_cabec-cnpj         = wa_kna1-stcd1.
      ELSEIF wa_kna1-stcd2 IS NOT INITIAL.
        wa_cabec-cnpj         = wa_kna1-stcd2.
      ENDIF.

    ENDIF.

    lv_nr_range_nr = '01'.
    lv_object      = 'ZSD_CTR_FP'.
    lv_quantity    = '00000000000000000001'.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = lv_nr_range_nr
        object                  = lv_object
        quantity                = lv_quantity
      IMPORTING
        number                  = lv_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    wa_cabec-id_ctr = lv_number.
    CONDENSE wa_cabec-id_ctr NO-GAPS.

    CALL SCREEN 0800.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  IF lcl_alv IS BOUND.
    CALL METHOD lcl_alv->free.
    CLEAR lcl_alv.
  ENDIF.

  IF lcl_container_alv IS BOUND.
    CALL METHOD lcl_container_alv->free.
    CLEAR lcl_container_alv.
  ENDIF.

  CREATE OBJECT lcl_container_alv
    EXPORTING
      container_name              = 'ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  CREATE OBJECT lcl_alv
    EXPORTING
      i_parent          = lcl_container_alv
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  CREATE OBJECT lcl_event.

* Incluir a referência a o evento TOOLBAR
  SET HANDLER lcl_event->handle_toolbar FOR lcl_alv.

* Incluir a referência a o evento USER_COMMAND
  SET HANDLER lcl_event->handle_command_grid FOR lcl_alv.

  wa_layout2-zebra = abap_true.
  wa_layout2-cwidth_opt = abap_true.

  REFRESH t_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSDS022'
    CHANGING
      ct_fieldcat            = t_fieldcat2
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  READ TABLE t_fieldcat2 ASSIGNING FIELD-SYMBOL(<fs_fieldcat>) INDEX 1.
  <fs_fieldcat>-checkbox = abap_true.
  <fs_fieldcat>-edit = abap_true.
  <fs_fieldcat>-outputlen = 2.

  CALL METHOD lcl_alv->set_table_for_first_display
    EXPORTING
      i_save          = 'A'
      i_default       = 'X'
      is_layout       = wa_layout2
    CHANGING
      it_outtab       = t_saida[]
      it_fieldcatalog = t_fieldcat2[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0900 OUTPUT.

  SET PF-STATUS 'PF_0900'.
  SET TITLEBAR 'TITULO_0900'.

  PERFORM f_exibe_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0900 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      SET SCREEN 0.

    WHEN 'EDITAR'.
      CALL SCREEN 0700.

    WHEN 'VISUALIZAR'.
      CALL SCREEN 0700.
    WHEN OTHERS.

  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_LINHAS_EDITAR
*&---------------------------------------------------------------------*
FORM f_valida_linhas_editar .

  CLEAR: v_valida.
  LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
    IF <fs_saida>-selecionar = abap_true.
      IF v_valida IS INITIAL.
        v_valida = 1.
      ELSE.
        v_valida = 2.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF v_valida = 2.
    MESSAGE s000(z_les) WITH text-019 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  IF v_valida IS INITIAL.
    MESSAGE s000(z_les) WITH text-020 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ALV
*&---------------------------------------------------------------------*
FORM f_atualiza_alv .

  DATA: l_stable       TYPE lvc_s_stbl, "Estrutura para refresh do ALV
        l_soft_refresh TYPE c. "Campo para refresh do ALV

*...Fixa posição da linha no ALV
  l_stable-row = abap_true.
  l_stable-col = abap_true.
  l_soft_refresh = abap_true.

*...Atualiza o ALV
  CALL METHOD lcl_alv->refresh_table_display
    EXPORTING
      is_stable      = l_stable
      i_soft_refresh = l_soft_refresh
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_CLIENTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_cliente INPUT.

  IF wa_cabec-id_cliente IS INITIAL.
    MESSAGE s000(z_les) WITH 'Informe Código do Cliente'(V02)
                      DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE name1, stcd1, stcd2
    FROM kna1
   WHERE kunnr = @wa_cabec-id_cliente
    INTO @DATA(wa_kna1).

  wa_cabec-desc_cliente = wa_kna1-name1.

  IF wa_kna1-stcd1 IS NOT INITIAL.
    wa_cabec-cnpj         = wa_kna1-stcd1.
  ELSEIF wa_kna1-stcd2 IS NOT INITIAL.
    wa_cabec-cnpj         = wa_kna1-stcd2.
  ENDIF.

  SELECT SINGLE *
  FROM knb1
 WHERE kunnr = @wa_cabec-id_cliente
   AND bukrs IN @s_bu_2
  INTO @DATA(wa_knb1).

  IF sy-subrc NE 0.
    MESSAGE s000(z_les) WITH text-007 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_DESCRICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_buscar_descricao .

* Descrição Cliente
  IF wa_tela-id_cliente IS NOT INITIAL.
    SELECT SINGLE name1 FROM kna1
      INTO (wa_tela-desc_cliente)
      WHERE kunnr = wa_tela-id_cliente.
  ENDIF.

* Denominações para grupos de mercadoria
  IF wa_tela-gr_mercadoria IS NOT INITIAL.
    SELECT SINGLE wgbez60 FROM t023t
      INTO ( wa_tela-wgbez )
      WHERE spras = sy-langu
       AND matkl = wa_tela-gr_mercadoria.
  ENDIF.

* Descrição Coleta
  IF wa_tela-pc_codigo IS NOT INITIAL.
    SELECT SINGLE name1 FROM lfa1
      INTO (wa_tela-desc_coleta)
      WHERE kunnr = wa_tela-id_cliente.
  ENDIF.

  IF wa_tela-lr_codigo IS NOT INITIAL.
    SELECT SINGLE name1 FROM kna1
  INTO (wa_tela-desc_entrega)
  WHERE kunnr = wa_tela-id_cliente.
  ENDIF.

  IF wa_tela-desc_zterm IS NOT INITIAL.

* Descrição ZTERM
    SELECT SINGLE text1 FROM t052u
      INTO (wa_tela-desc_zterm)
      WHERE spras = sy-langu
        AND zterm = wa_tela-zterm.

  ENDIF.

ENDFORM.
