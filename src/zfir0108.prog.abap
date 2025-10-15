*&---------------------------------------------------------------------*
*& Report ZFIR0108
*&---------------------------------------------------------------------*
REPORT zfir0108.

TABLES: zfit0170, acdoca.

*&---------------------------------------------------------------------*
*&  ESTRUTURAS
*&---------------------------------------------------------------------*
"US #167060 - MMSILVA - 19.02.2025 - Inicio
TYPES: BEGIN OF ty_bsak,
         belnr TYPE bsak-belnr.
TYPES: END OF ty_bsak.

TYPES: BEGIN OF ty_bsik,
         belnr TYPE bsik-belnr,
         zlspr TYPE bsik-zlspr.
TYPES: END OF ty_bsik.
"US #167060 - MMSILVA - 19.02.2025 - Fim


*&---------------------------------------------------------------------*
*&  TABELAS INTERNAS E WORKAREAS
*&---------------------------------------------------------------------*
DATA: it_zfit0170 TYPE TABLE OF zfit0170,
      it_acdoca   TYPE TABLE OF acdoca,
      it_bkpf     TYPE TABLE OF bkpf,
      it_lfa1     TYPE TABLE OF lfa1,
      it_ekko     TYPE TABLE OF ekko,
      it_t161t    TYPE TABLE OF t161t,
      it_bseg     TYPE TABLE OF bseg,
      it_bsak     TYPE TABLE OF ty_bsak, "US #167060 - MMSILVA - 19.02.2025
      it_bsik     TYPE TABLE OF ty_bsik, "US #167060 - MMSILVA - 19.02.2025
      wa_zfit0170 TYPE          zfit0170,
      wa_acdoca   TYPE          acdoca,
      wa_bkpf     TYPE          bkpf,
      wa_lfa1     TYPE          lfa1,
      wa_ekko     TYPE          ekko,
      wa_t161t    TYPE          t161t,
      wa_bseg     TYPE          bseg,
      wa_bsak     TYPE          ty_bsak, "US #167060 - MMSILVA - 19.02.2025
      wa_bsik     TYPE          ty_bsik. "US #167060 - MMSILVA - 19.02.2025


*&---------------------------------------------------------------------*
*&  SAIDA
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_saida,
         bukrs     TYPE zfit0170-bukrs,
         belnr     TYPE zfit0170-belnr,
         zfbdt     TYPE zfit0170-zfbdt,
         dt_envio  TYPE zfit0170-dt_envio,
         nome_arq  TYPE zfit0170-nome_arq,
         lifnr     TYPE acdoca-lifnr,
         ebeln     TYPE acdoca-ebeln,
         value     TYPE acdoca-hsl,
         budat     TYPE acdoca-budat,
         bldat     TYPE acdoca-bldat,
         blart     TYPE acdoca-blart,
         gjahr     TYPE acdoca-gjahr,
         name1     TYPE lfa1-name1,
         xblnr     TYPE bkpf-xblnr,
*         blart_b   TYPE bkpf-blart,
         bsart     TYPE ekko-bsart,
         zterm     TYPE ekko-zterm,
         zbd1t     TYPE ekko-zbd1t,
         batxt     TYPE t161t-batxt,
*         lifnr_b   TYPE bseg-lifnr,
*         ebeln_b   TYPE bseg-ebeln,
*         wrbtr     TYPE bseg-wrbtr,
*         zfbdt_b   TYPE bseg-zfbdt,
         tipo_mvto TYPE val_text, "US #165501 - MMSILVA - 14.02.2025
         stcd1     TYPE lfa1-stcd1, "US #175176 - MMSILVA - 28.04.2025
         stcd2     TYPE lfa1-stcd2, "BUG # - MMSILVA - 23.05.2025
       END OF ty_saida.

DATA: it_saida TYPE TABLE OF ty_saida,
      wa_saida TYPE          ty_saida.


*&---------------------------------------------------------------------*
*&  ALV
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: dg_splitter_1        TYPE REF TO cl_gui_splitter_container,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      tl_function          TYPE ui_functions,
      wl_function          TYPE ui_func,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      ok_code              TYPE sy-ucomm,
*
      zcl_util             TYPE REF TO zcl_util.

DATA: variante         LIKE disvariant.
DATA: gs_variant_c TYPE disvariant.

DATA: it_return TYPE TABLE OF ddshretval,
      it_t028g  TYPE TABLE OF t028g.


*&---------------------------------------------------------------------*
*&  CLASSES
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.


    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

*    CLASS-METHODS:
*      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
*        IMPORTING e_object.

*    CLASS-METHODS:
*      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
*        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION


CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.

  ENDMETHOD.

  METHOD on_hotspot_click.
    DATA : l_columnid TYPE lvc_s_col.

    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

    CASE e_column_id-fieldname.
      WHEN 'EBELN'.
        SET PARAMETER ID 'BES' FIELD wa_saida-ebeln.

        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

      WHEN 'BELNR'.
        SET PARAMETER ID 'BLN' FIELD wa_saida-belnr.
        SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr.

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.


*&---------------------------------------------------------------------*
*&  PARAMETROS
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  SELECT-OPTIONS: s_bukrs FOR zfit0170-bukrs OBLIGATORY,
                  s_data  FOR zfit0170-dt_envio OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.


*&---------------------------------------------------------------------*
*&  INICIO
*&---------------------------------------------------------------------*
INITIALIZATION.


*&---------------------------------------------------------------------*
*&  START
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_selecao_dados.

  PERFORM f_manipula_dados.

  PERFORM f_exibir_dados.


*&---------------------------------------------------------------------*
*&  SELECIONA DADOS
*&---------------------------------------------------------------------*
FORM f_selecao_dados.

  IF s_data-high IS INITIAL.
    s_data-high = s_data-low.
  ENDIF.

  FREE: it_zfit0170.
  SELECT *
    FROM zfit0170
    INTO TABLE it_zfit0170
    WHERE bukrs IN s_bukrs
    AND dt_envio IN s_data.


  IF it_zfit0170 IS NOT INITIAL.
    FREE: it_bkpf, it_lfa1, it_acdoca.
    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf
      FOR ALL ENTRIES IN it_zfit0170
      WHERE bukrs = it_zfit0170-bukrs
      AND   belnr = it_zfit0170-belnr
      AND   gjahr = it_zfit0170-gjahr.

    SELECT *
      FROM acdoca
      INTO TABLE it_acdoca
      FOR ALL ENTRIES IN it_zfit0170
      WHERE rldnr  = '0L'
      AND   rbukrs = it_zfit0170-bukrs
      AND   belnr  = it_zfit0170-belnr
      AND   gjahr  = it_zfit0170-gjahr
      AND   koart  = 'K'.

    SELECT *
      FROM bseg
      INTO TABLE it_bseg
      FOR ALL ENTRIES IN it_zfit0170
      WHERE belnr = it_zfit0170-belnr
*** Stefanini - IR222229 - 24/03/2025 - LAZAROSR - Início de Alteração
        AND bukrs = it_zfit0170-bukrs
        AND gjahr = it_zfit0170-gjahr.
*** Stefanini - IR222229 - 24/03/2025 - LAZAROSR - Fim de Alteração

    IF it_acdoca IS NOT INITIAL.
      SELECT *
        FROM lfa1
        INTO TABLE it_lfa1
        FOR ALL ENTRIES IN it_acdoca
        WHERE lifnr = it_acdoca-lifnr.

      SELECT *
        FROM ekko
        INTO TABLE it_ekko
        FOR ALL ENTRIES IN it_acdoca
        WHERE ebeln = it_acdoca-ebeln.

      IF it_ekko IS NOT INITIAL.
        SELECT *
          FROM t161t
          INTO TABLE it_t161t
          FOR ALL ENTRIES IN it_ekko
          WHERE bsart = it_ekko-bsart
          AND   spras = 'PT'.
      ENDIF.
    ENDIF.

*** Stefanini - IR222229 - 24/03/2025 - LAZAROSR - Início de Alteração
    IF it_bseg IS NOT INITIAL.

      SELECT DISTINCT bsart
      FROM ekko
      INTO TABLE @DATA(lt_bsart)
      FOR ALL ENTRIES IN @it_bseg
      WHERE ebeln = @it_bseg-ebeln.

      IF sy-subrc IS INITIAL.

        SELECT *
          FROM t161t
          APPENDING TABLE @it_t161t
          FOR ALL ENTRIES IN @lt_bsart
          WHERE bsart       = @lt_bsart-bsart
          AND   t161t~spras = 'PT'.

      ENDIF.

    ENDIF.
*** Stefanini - IR222229 - 24/03/2025 - LAZAROSR - Fim de Alteração

    "US #167060 - MMSILVA - 19.02.2025 - Inicio
    SELECT belnr
      FROM bsak
      INTO TABLE it_bsak
      FOR ALL ENTRIES IN it_zfit0170
      WHERE belnr = it_zfit0170-belnr.

    SELECT belnr zlspr
      FROM bsik
      INTO TABLE it_bsik
      FOR ALL ENTRIES IN it_zfit0170
      WHERE belnr = it_zfit0170-belnr.
    "US #167060 - MMSILVA - 19.02.2025 - Fim
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&  MANIPULA DADOS
*&---------------------------------------------------------------------*
FORM f_manipula_dados.

  SORT it_zfit0170 BY belnr.

*** Stefanini - IR222229 - 24/03/2025 - LAZAROSR - Início de Alteração
  SORT it_bseg BY bukrs belnr gjahr.
*** Stefanini - IR222229 - 24/03/2025 - LAZAROSR - Fim de Alteração

  LOOP AT it_zfit0170 INTO wa_zfit0170 WHERE bukrs IN s_bukrs AND
                                             dt_envio IN s_data.

    wa_saida-bukrs    = wa_zfit0170-bukrs.
    wa_saida-belnr    = wa_zfit0170-belnr.
    wa_saida-zfbdt    = wa_zfit0170-zfbdt + wa_zfit0170-zbd1t.

    "US #165501 - MMSILVA - 14.02.2025 - Inicio
    IF ( wa_zfit0170-tipo_mvto IS NOT INITIAL ) AND ( wa_zfit0170-tipo_mvto NE 'X' ).

      "ISSUE #173814 - MMSILVA - 09.04.2025 - Comentado devido alteração no texto - Inicio comentário
*      SELECT SINGLE ddtext
*        FROM dd07t
*        INTO @DATA(wa_ddtext)
*        WHERE domname    = 'ZTIPO_MVTO'
*        AND   domvalue_l = @wa_zfit0170-tipo_mvto.
*
*      IF wa_ddtext IS NOT INITIAL.
*
*        wa_saida-tipo_mvto = wa_ddtext.
*
*      ENDIF.
      "ISSUE #173814 - MMSILVA - 09.04.2025 - Comentado devido alteração no texto - Fim comentário

*     ISSUE #173814 - MMSILVA - 09.04.2025 - Inicio
      CASE wa_zfit0170-tipo_mvto.
        WHEN '0'.
         wa_saida-tipo_mvto = 'Inclusão'.
       	WHEN '5'.
         wa_saida-tipo_mvto = 'Alteração'.
        WHEN '9'.
         wa_saida-tipo_mvto = 'Exclusão'.
      ENDCASE.
*     ISSUE #173814 - MMSILVA - 09.04.2025 - Final

*    elseif ( wa_zfit0170-tipo_mvto is not initial ) and ( wa_saida-tipo_mvto is initial ) and ( wa_zfit0170-tipo_mvto eq 'X' ).
*      read table it_bsak into wa_bsak with key belnr = wa_zfit0170-belnr.
*
*      if sy-subrc is initial.
*        wa_saida-tipo_mvto = 'Compensado'.
*      else.
*        read table it_bsik into wa_bsik with key belnr = wa_zfit0170-belnr.
*
*        if sy-subrc is initial.
*          if wa_bsik-zlspr is not initial.
*            wa_saida-tipo_mvto = 'Bloqueado'.
*          else.
*            wa_saida-tipo_mvto = 'Pendente'.
*          endif.
*        endif.
*      endif.

    ENDIF.
    "US #165501 - MMSILVA - 14.02.2025 - Fim

    IF wa_saida-zfbdt IS INITIAL.
      SELECT SINGLE *
          FROM bseg
          WHERE belnr = @wa_zfit0170-belnr
          AND   bschl = '31'
          INTO CORRESPONDING FIELDS OF @wa_bseg.

      IF wa_bseg IS NOT INITIAL.
        wa_saida-zfbdt = wa_bseg-zfbdt + wa_bseg-zbd1t.
      ENDIF.
    ENDIF.
    wa_saida-nome_arq = wa_zfit0170-nome_arq.
    wa_saida-dt_envio = wa_zfit0170-dt_envio.

    READ TABLE it_acdoca INTO wa_acdoca WITH KEY rldnr  = '0L'
                                                 rbukrs = wa_zfit0170-bukrs
                                                 belnr  = wa_zfit0170-belnr
                                                 gjahr  = wa_zfit0170-gjahr
                                                 koart  = 'K'.
    " Caso esteja na tabela acdoca.
    IF wa_acdoca IS NOT INITIAL.
      wa_saida-lifnr = wa_acdoca-lifnr.
      wa_saida-ebeln = wa_acdoca-ebeln.
      wa_saida-value = wa_acdoca-hsl * -1.
      wa_saida-budat = wa_acdoca-budat.
      wa_saida-bldat = wa_acdoca-bldat.
      wa_saida-blart = wa_acdoca-blart.
      wa_saida-gjahr = wa_acdoca-gjahr.

      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_acdoca-ebeln.

      IF wa_ekko IS NOT INITIAL.

        wa_saida-bsart = wa_ekko-bsart.
        wa_saida-zterm = wa_ekko-zterm.
        wa_saida-zbd1t = wa_ekko-zbd1t.

        READ TABLE it_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart
                                                   spras = 'PT'.

        IF wa_t161t IS NOT INITIAL.
          wa_saida-batxt = wa_t161t-batxt.
        ENDIF.

      ELSEIF wa_ekko IS INITIAL.

        SELECT SINGLE *
          FROM bseg
          WHERE belnr = @wa_zfit0170-belnr
          AND   bukrs = @wa_zfit0170-bukrs
          AND   gjahr = @wa_zfit0170-gjahr
          AND   bschl = '31'
          INTO CORRESPONDING FIELDS OF @wa_bseg.

        IF wa_bseg IS NOT INITIAL.

          wa_saida-ebeln = wa_bseg-ebeln.

          SELECT SINGLE *
            FROM ekko
            WHERE ebeln = @wa_bseg-ebeln
            INTO CORRESPONDING FIELDS OF @wa_ekko.

          IF wa_ekko IS NOT INITIAL.
            wa_saida-bsart = wa_ekko-bsart.
            wa_saida-zterm = wa_ekko-zterm.
            wa_saida-zbd1t = wa_ekko-zbd1t.

            READ TABLE it_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart
                                                       spras = 'PT'.

            IF wa_t161t IS NOT INITIAL.
              wa_saida-batxt = wa_t161t-batxt.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wa_zfit0170-bukrs
                                               belnr = wa_zfit0170-belnr
                                               gjahr = wa_zfit0170-gjahr.

      IF wa_bkpf IS NOT INITIAL.

        wa_saida-xblnr = wa_bkpf-xblnr.

      ENDIF.

      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_acdoca-lifnr.

      IF wa_lfa1 IS NOT INITIAL AND wa_acdoca IS NOT INITIAL.
        wa_saida-name1 = wa_lfa1-name1.
* ----> BUG #180472 - MMSILVA - 23.05.2025 - Inicio <----
        IF wa_lfa1-stkzn IS INITIAL.
          wa_saida-stcd1 = wa_lfa1-stcd1. "US 175176 - MMSILVA - 28.04.2025
        ELSE.
          wa_saida-stcd2 = wa_lfa1-stcd2.
        ENDIF.
* ----> BUG #180472 - MMSILVA - 23.05.2025 - Fim <----
      ENDIF.

      APPEND wa_saida TO it_saida.
      "Caso não tenha resultados na acdoca
    ELSEIF wa_acdoca IS INITIAL.

*** Stefanini - IR222229 - 24/03/2025 - LAZAROSR - Início de Alteração
*      LOOP AT it_bseg INTO wa_bseg WHERE belnr = wa_zfit0170-belnr.

      READ TABLE it_bseg TRANSPORTING NO FIELDS
                         WITH KEY bukrs = wa_zfit0170-bukrs
                                  belnr = wa_zfit0170-belnr
                                  gjahr = wa_zfit0170-gjahr
                                              BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        LOOP AT it_bseg INTO wa_bseg FROM sy-tabix.

          IF wa_bseg-bukrs NE wa_zfit0170-bukrs
          OR wa_bseg-belnr NE wa_zfit0170-belnr
          OR wa_bseg-gjahr NE wa_zfit0170-gjahr.
            EXIT.
          ENDIF.
*** Stefanini - IR222229 - 24/03/2025 - LAZAROSR - Fim de Alteração

*        if wa_bseg is not initial.
          wa_saida-lifnr = wa_bseg-lifnr.
          wa_saida-ebeln = wa_bseg-ebeln.
          wa_saida-value = wa_bseg-wrbtr.
          wa_saida-zfbdt = wa_bseg-zfbdt.

          SELECT SINGLE *
            FROM ekko
            WHERE ebeln = @wa_bseg-ebeln
            INTO CORRESPONDING FIELDS OF @wa_ekko.

          IF wa_ekko IS NOT INITIAL.
            wa_saida-bsart = wa_ekko-bsart.
            wa_saida-zterm = wa_ekko-zterm.
            wa_saida-zbd1t = wa_ekko-zbd1t.

            READ TABLE it_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart
                                                       spras = 'PT'.

            IF wa_t161t IS NOT INITIAL.
              wa_saida-batxt = wa_t161t-batxt.
            ENDIF.
          ENDIF.

          READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wa_zfit0170-bukrs
                                                   belnr = wa_zfit0170-belnr
                                                   gjahr = wa_zfit0170-gjahr.

          IF wa_bkpf IS NOT INITIAL.
            wa_saida-xblnr = wa_bkpf-xblnr.
            wa_saida-blart = wa_bkpf-blart.
            wa_saida-bldat = wa_bkpf-bldat.
            wa_saida-budat = wa_bkpf-budat.
          ENDIF.

          SELECT SINGLE *
            FROM lfa1
            WHERE lifnr = @wa_bseg-lifnr
            INTO CORRESPONDING FIELDS OF @wa_lfa1.

          IF wa_lfa1 IS NOT INITIAL.
            wa_saida-name1 = wa_lfa1-name1.
* --------> BUG #180472 - MMSILVA - 23.05.2025 - Inicio <--------
            IF wa_lfa1-stkzn IS INITIAL.
              wa_saida-stcd1 = wa_lfa1-stcd1. "US 175176 - MMSILVA - 28.04.2025
            ELSE.
              wa_saida-stcd2 = wa_lfa1-stcd2.
            ENDIF.
* --------> BUG #180472 - MMSILVA - 23.05.2025 - Fim <--------
          ENDIF.
*         endif.
          APPEND wa_saida TO it_saida.
          CLEAR: wa_saida-bsart,
                 wa_saida-zterm,
                 wa_saida-zbd1t,
                 wa_saida-batxt,
                 wa_saida-xblnr,
                 wa_saida-blart,
                 wa_saida-bldat,
                 wa_saida-budat,
                 wa_saida-name1.
          CLEAR: wa_acdoca, wa_bkpf, wa_lfa1, wa_ekko, wa_t161t, wa_bseg.
        ENDLOOP.

*** Stefanini - IR222229 - 24/03/2025 - LAZAROSR - Início de Alteração
      ENDIF.
*** Stefanini - IR222229 - 24/03/2025 - LAZAROSR - Fim de Alteração

    ENDIF.
    CLEAR: wa_zfit0170, wa_saida, wa_acdoca, wa_bkpf, wa_lfa1, wa_ekko, wa_t161t, wa_bseg.
  ENDLOOP.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_exibir_dados
*&---------------------------------------------------------------------*
FORM f_exibir_dados .

  CALL SCREEN 0100.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_imprimir_dados
*&---------------------------------------------------------------------*
FORM f_init_alv .

  DATA: wl_layout TYPE slis_layout_alv.
  DATA:
    p_text      TYPE sdydo_text_element,
    filtros	    TYPE zif_screen_linha_filtro,
    i_filtros	  TYPE zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) TYPE c,
    v_uzeit(10) TYPE c.


  PERFORM f_fieldcatalog.

  variante = VALUE #( report = sy-repid ).



  IF g_grid IS INITIAL.

    CLEAR: i_filtros.
    CONCATENATE sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) INTO v_datum.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) INTO v_uzeit.
    DESCRIBE TABLE it_saida LINES DATA(v_lines).
    APPEND VALUE #( parametro = 'Data:' valor = v_datum ) TO i_filtros.
    APPEND VALUE #( parametro = 'Hora:' valor = v_uzeit ) TO i_filtros.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros.

    p_text = 'Relatório Faturas Enviadas para AL5 Bank'.
  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      EXPORTING
        i_titulo  = CONV #( p_text )
        i_filtros = i_filtros
      CHANGING
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.


    w_layout-sel_mode = 'A'.
    w_layout-col_opt  = abap_true.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      CHANGING
        it_outtab                     = it_saida[]
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


    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ELSE.
    CALL METHOD g_grid->refresh_table_display( is_stable = w_stable ).
  ENDIF.

  wl_layout-colwidth_optimize = 'X'.


ENDFORM.


*&---------------------------------------------------------------------*
*& Form montar_layout
*&---------------------------------------------------------------------*
FORM f_fieldcatalog .

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
 01  ''   ''   'IT_SAIDA'   'BUKRS      '            'Empresa                    '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'IT_SAIDA'   'LIFNR      '            'Fornecedor                 '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'IT_SAIDA'   'NAME1      '            'Nome Fornecedor            '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'IT_SAIDA'   'STCD1      '            'CNPJ                       '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ', "US #175176 - MMSILVA - 28.04.2025
 04  ''   ''   'IT_SAIDA'   'STCD2      '            'CPF                        '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ', "BUG #180472 - MMSILVA - 23.05.2025
 04  ''   ''   'IT_SAIDA'   'BELNR      '            'Doc. Contabil              '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'IT_SAIDA'   'BLDAT      '            'Data do Documento          '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'IT_SAIDA'   'BLART      '            'Tipo do Documento          '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''   ''   'IT_SAIDA'   'XBLNR      '            'Referência                 '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''   ''   'IT_SAIDA'   'EBELN      '            'Pedido                     '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''   ''   'IT_SAIDA'   'BSART      '            'Tipo do Pedido             '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 10  ''   ''   'IT_SAIDA'   'BATXT      '            'Denom. Tipo do Pedido      '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 11  ''   ''   'IT_SAIDA'   'ZTERM      '            'Condição de Pagamento      '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 12  ''   ''   'IT_SAIDA'   'ZBD1T      '            'Pagamento em Dias          '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 13  ''   ''   'IT_SAIDA'   'VALUE      '            'Valor R$                   '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 14  ''   ''   'IT_SAIDA'   'DT_ENVIO   '            'Dt. do Arquivo             '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 15  ''   ''   'IT_SAIDA'   'BUDAT      '            'Dt. do Lançamento          '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 16  ''   ''   'IT_SAIDA'   'ZFBDT      '            'Dt. Vencimento             '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 17  ''   ''   'IT_SAIDA'   'TIPO_MVTO  '            'Tipo de Movimento          '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '. "US #165501 - MMSILVA - 14.02.2025
* 17  ''   ''   'IT_SAIDA'   'NOME_ARQ   '            'Nome Arquivo               '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.





ENDFORM.


*&---------------------------------------------------------------------*
*& Form  f_estrutura_alv
*&---------------------------------------------------------------------*
FORM f_estrutura_alv  USING VALUE(p_col_pos)       TYPE i                    "1
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
                           VALUE(p_fix).                                    "17

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

  IF p_field = 'EBELN'.
    w_fieldcat-hotspot = abap_true.
  ELSEIF p_field = 'BELNR'.
    w_fieldcat-hotspot = abap_true.
  ENDIF.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.


*&---------------------------------------------------------------------*
*& Module STATUS_100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100'.

  PERFORM f_init_alv.
ENDMODULE.


*&---------------------------------------------------------------------*
*&  Module  USER_COMMAND_100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
