*&---------------------------------------------------------------------*
*& Report ZMMR211
*&---------------------------------------------------------------------*
*& Relatório Entrada de NF(ZMM0110) por Romaneio
*& Consultor ABAP: Renato Garcia de Aro
*& Data: 06.05.2025
*&---------------------------------------------------------------------*
REPORT zmmr211.

INCLUDE zmmr211_top_include.

TABLES: j_1bnfdoc, ZIB_NFE_DIST_TER.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_bukrs     FOR zsdt0001-bukrs,
                  s_branch    FOR zsdt0001-branch,
                  s_dtmov     FOR zsdt0001-dt_movimento OBLIGATORY,
                  s_docdat    FOR zsdt0001-docdat,
                  s_parid     FOR zsdt0001-parid,
                  s_chave     FOR zsdt0001-chave_nfe,
                  s_nfnum     FOR zsdt0001-nfnum,
                  s_cnpj      FOR zib_nfe_dist_ter-forne_cnpj.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS: p_sement RADIOBUTTON GROUP grp1,
              p_fertil RADIOBUTTON GROUP grp1.

SELECTION-SCREEN: END OF BLOCK b2.


SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

  PARAMETERS: p_penden RADIOBUTTON GROUP grp2,
              p_determ RADIOBUTTON GROUP grp2,
              p_todosp RADIOBUTTON GROUP grp2.

SELECTION-SCREEN: END OF BLOCK b3.


SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.

  PARAMETERS: p_pendmt RADIOBUTTON GROUP grp3,
              p_geramt RADIOBUTTON GROUP grp3,
              p_todomt RADIOBUTTON GROUP grp3.

SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.

  PARAMETERS: p_pendft RADIOBUTTON GROUP grp4,
              p_geraft RADIOBUTTON GROUP grp4,
              p_todoft RADIOBUTTON GROUP grp4.

SELECTION-SCREEN: END OF BLOCK b5.

CLASS lcl_alv DEFINITION.

  PUBLIC SECTION.

    DATA o_alv TYPE REF TO cl_salv_table.

    DATA it_saida TYPE TABLE OF ty_saida.


    METHODS:

      get_dada,

      generate_output.

  PRIVATE SECTION.
    METHODS:
      ajusta_colunas
        CHANGING
          co_alv TYPE REF TO cl_salv_table.

    METHODS:
      pfstatus
        CHANGING
          co_alv TYPE REF TO cl_salv_table.

    METHODS:
      display_settings
        CHANGING
          co_alv TYPE REF TO cl_salv_table.

    METHODS:
      set_hotspot
        CHANGING
          co_alv    TYPE REF TO cl_salv_table
          co_report TYPE REF TO lcl_alv.

    METHODS:
      on_user_command
        FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
          e_salv_function.

    METHODS:
      on_click
        FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column.

    METHODS:
      refresh_table.

ENDCLASS.


START-OF-SELECTION.

  DATA lo_alv TYPE REF TO lcl_alv.

  CREATE OBJECT lo_alv.

  lo_alv->get_dada( ).

  lo_alv->generate_output( ).

CLASS lcl_alv IMPLEMENTATION.

  METHOD get_dada.

    CONSTANTS c_tp_mov TYPE c VALUE 'E'.

    CONSTANTS c_fert TYPE tvarvc-low VALUE 'Fertilizantes'.
    CONSTANTS c_seme TYPE tvarvc-low VALUE 'Sementes'.

    CONSTANTS c_fert_p TYPE tvarvc-low VALUE 'MAGGI_GR_FERTILIZANTES'.
    CONSTANTS c_seme_p TYPE tvarvc-low VALUE 'MAGGI_GR_SEMENTES'.


    DATA: it_range_fert TYPE RANGE OF char30,
          ls_range_fert LIKE LINE OF it_range_fert,
          it_range_seme TYPE RANGE OF char30,
          ls_range_seme LIKE LINE OF it_range_seme.

    DATA wa_saida LIKE LINE OF it_saida.


    SELECT *
        FROM zsdt0001
        INTO TABLE @DATA(it_zsdt0001)
        WHERE bukrs        IN @s_bukrs  AND
              branch       IN @s_branch AND
              dt_movimento IN @s_dtmov  AND
              docdat       IN @s_docdat AND
              parid        IN @s_parid  AND
              chave_nfe    IN @s_chave  AND
              tp_movimento EQ @c_tp_mov AND
              nfnum        IN @s_nfnum.

    IF sy-subrc NE 0.
      MESSAGE 'Não existem dados para esta seleção!' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.

    ELSE.

      SELECT sign, opti AS option, low, high
         FROM tvarvc
         INTO CORRESPONDING FIELDS OF TABLE @it_range_fert
        WHERE name = @c_fert_p AND type = 'S'.

      SELECT sign, opti AS option, low, high
         FROM tvarvc
         INTO CORRESPONDING FIELDS OF TABLE @it_range_seme
         WHERE name = @c_seme_p AND type = 'S'.


      SELECT ma~matnr, ma~matkl, mt~maktx
        FROM mara AS ma
        LEFT OUTER JOIN makt AS mt
        ON mt~matnr EQ ma~matnr AND spras = @sy-langu
       INTO TABLE @DATA(it_mara)
        FOR ALL ENTRIES IN @it_zsdt0001
        WHERE ma~matnr EQ @it_zsdt0001-matnr.

      SELECT it~chave_nfe, it~prod_item, it~ebeln, it~mblnr, it~belnr_ft, ter~chave_nfe AS chave_ter,
             ter~st_fiscal
        FROM zib_nfe_dist_itm AS it
        LEFT OUTER JOIN zib_nfe_dist_ter AS ter
        ON ter~chave_nfe = it~chave_nfe
        INTO TABLE @DATA(it_itens_nfe)
        FOR ALL ENTRIES IN @it_zsdt0001
        WHERE it~chave_nfe   EQ @it_zsdt0001-chave_nfe AND
              ter~forne_cnpj IN @s_cnpj.

      SORT: it_zsdt0001, it_itens_nfe, it_mara.

      LOOP AT it_zsdt0001 INTO DATA(ls_zsdt0001).

        MOVE-CORRESPONDING ls_zsdt0001 TO wa_saida.

        IF line_exists( it_mara[ matnr = wa_saida-matnr ] ).

          DATA(ls_mara) = it_mara[ matnr = wa_saida-matnr ].

          "Fertilizantes
          wa_saida-tpinsumo = COND string(
            WHEN line_exists( it_range_fert[ low = ls_mara-matkl ] ) THEN c_fert ).


          IF wa_saida-tpinsumo IS INITIAL.
            "Sementes
            wa_saida-tpinsumo = COND string(
             WHEN line_exists( it_range_seme[ low = ls_mara-matkl ] ) THEN c_seme ).
          ENDIF.

        ENDIF.

        IF line_exists( it_itens_nfe[ chave_nfe = wa_saida-chave_nfe ] ).

          DATA(ls_nfe_itens) = it_itens_nfe[ chave_nfe = wa_saida-chave_nfe ].

          IF ls_nfe_itens-ebeln IS NOT INITIAL AND
            ( ls_nfe_itens-st_fiscal EQ '98' OR ls_nfe_itens-st_fiscal EQ '99' ).
            wa_saida-stpedido = 'Determinado'.
          ENDIF.

          IF ls_nfe_itens-mblnr IS NOT INITIAL.
            wa_saida-stmatnr = 'Gerado'.
          ENDIF.

          IF ls_nfe_itens-belnr_ft IS NOT INITIAL.
            wa_saida-stmatnr2 = 'Gerado'.
          ENDIF.

        ENDIF.

        IF line_exists( it_itens_nfe[ chave_ter = wa_saida-chave_nfe ] ).
          wa_saida-xmlreceb = icon_led_green.
        ELSE.
          wa_saida-xmlreceb = icon_led_red.
        ENDIF.

        wa_saida-pedatrib = ls_nfe_itens-ebeln.

        SELECT ebeln, ebelp, matnr
          FROM ekpo INTO TABLE @DATA(it_ekpo)
          WHERE ebeln = @ls_nfe_itens-ebeln.
        IF sy-subrc EQ 0.

          DELETE it_ekpo WHERE ebeln = ls_nfe_itens-ebeln.
          IF it_ekpo[] IS INITIAL.
            wa_saida-maktx = ls_mara-maktx.
          ENDIF.

        ENDIF.

        wa_saida-mblnr = ls_nfe_itens-mblnr.

        wa_saida-belnr_ft = ls_nfe_itens-belnr_ft.

        SELECT SINGLE cpudt FROM
          mkpf INTO wa_saida-cpudt
          WHERE mblnr EQ wa_saida-mblnr.

        SELECT SINGLE cpudt FROM
          rbkp INTO wa_saida-cpudt_rb
          WHERE belnr EQ wa_saida-belnr_ft.

        SELECT SINGLE lgort FROM
         mseg INTO wa_saida-matnr
         WHERE belnr EQ wa_saida-lgort.


        SELECT SINGLE name1
          FROM lfa1
          INTO wa_saida-name1
          WHERE lifnr = wa_saida-parid.

        APPEND wa_saida TO it_saida.

        CLEAR wa_saida.

      ENDLOOP.

      "Radio buttons
      "Insumos
      IF p_sement IS NOT INITIAL.
        DELETE it_saida WHERE tpinsumo = c_fert.
      ELSE.
        DELETE it_saida WHERE tpinsumo = c_seme.
      ENDIF.

      "Status Pedido
      IF p_penden IS NOT INITIAL."pendentes
        DELETE it_saida WHERE stpedido IS NOT INITIAL.
      ELSEIF p_determ IS NOT INITIAL."determinados
        DELETE it_saida WHERE stpedido IS INITIAL.
      ENDIF.

      "Status Doc. Material
      IF p_pendmt IS NOT INITIAL. "pendente
        DELETE it_saida WHERE stmatnr IS NOT INITIAL.
      ELSEIF p_geramt IS NOT INITIAL."gerado
        DELETE it_saida WHERE stmatnr IS INITIAL.
      ENDIF.

      "Status Doc. fatura
      IF p_pendmt IS NOT INITIAL. "pendente
        DELETE it_saida WHERE stmatnr2 IS NOT INITIAL.
      ELSEIF p_geramt IS NOT INITIAL. "gerado
        DELETE it_saida WHERE stmatnr2 IS INITIAL.
      ENDIF.

      SORT it_saida.

    ENDIF.

  ENDMETHOD.


  METHOD generate_output.

    DATA: lx_msg TYPE REF TO cx_salv_msg.
    TRY.
        cl_salv_table=>factory(
         IMPORTING
           r_salv_table = o_alv
         CHANGING
           t_table = it_saida ).

      CATCH cx_salv_msg INTO lx_msg.

    ENDTRY.

    "Chamada do método para ajustar colunas
    CALL METHOD ajusta_colunas
      CHANGING
        co_alv = o_alv.

    "Menu PFStatus
    CALL METHOD pfstatus
      CHANGING
        co_alv = o_alv.

    CALL METHOD display_settings
      CHANGING
        co_alv = o_alv.

    "Set Up the Hotspot & Event Handler
    CALL METHOD set_hotspot
      CHANGING
        co_alv    = o_alv
        co_report = lo_alv.

    o_alv->display( ).

  ENDMETHOD.

  METHOD ajusta_colunas.
    INCLUDE: <icon>.

    DATA lo_cols TYPE REF TO cl_salv_columns.
    DATA lo_column TYPE REF TO cl_salv_column_table.
    DATA lo_functions  TYPE REF TO cl_salv_functions_list.

    DATA: lo_functional_settings  TYPE REF TO cl_salv_functional_settings.
    DATA: lo_tooltips             TYPE REF TO cl_salv_tooltips.


    lo_functions = o_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

    lo_cols = o_alv->get_columns( ).
    lo_cols->set_optimize( 'X' ).


    TRY .


        lo_column ?= lo_cols->get_column( 'CHAVE_NFE' ).
        CALL METHOD lo_column->set_cell_type
          EXPORTING
            value = if_salv_c_cell_type=>hotspot.

        lo_column ?= lo_cols->get_column( 'CHAVE_NFE' ).
        lo_column->set_long_text( 'Chave NFe' ).
        lo_column->set_medium_text( 'Chave NFe' ).
        lo_column->set_short_text( 'Chave NFe' ).
        lo_column->set_output_length( 10 ).

        lo_column ?= lo_cols->get_column( 'TPINSUMO' ).
        lo_column->set_long_text( 'Tipo insumo' ).
        lo_column->set_medium_text( 'Tipo insumo' ).
        lo_column->set_short_text( 'Tp. Insumo' ).
        lo_column->set_output_length( 10 ).

        lo_column ?= lo_cols->get_column( 'STPEDIDO' ).
        lo_column->set_long_text( 'Status Pedido' ).
        lo_column->set_medium_text( 'St. Pedido' ).
        lo_column->set_short_text( 'St. Pedido' ).
        lo_column->set_output_length( 12 ).


        lo_column ?= lo_cols->get_column( 'STPEDIDO' ).
        lo_column->set_long_text( 'Status Pedido' ).
        lo_column->set_medium_text( 'St. Pedido' ).
        lo_column->set_short_text( 'St. Pedido' ).
        lo_column->set_output_length( 12 ).

        lo_column ?= lo_cols->get_column( 'STMATNR' ).
        lo_column->set_long_text( 'Status Físico' ).
        lo_column->set_medium_text( 'St. Físico' ).
        lo_column->set_short_text( 'St. Físico' ).
        lo_column->set_output_length( 14 ).


        lo_column ?= lo_cols->get_column( 'STMATNR2' ).
        lo_column->set_long_text( 'Status Fiscal' ).
        lo_column->set_medium_text( 'St. Fiscal' ).
        lo_column->set_short_text( 'St. Fisc.' ).
        lo_column->set_output_length( 16 ).


        lo_column ?= lo_cols->get_column( 'XMLRECEB' ).
        lo_column->set_long_text( 'XML Recebido' ).
        lo_column->set_medium_text( 'XML Recebido' ).
        lo_column->set_short_text( 'XML Rec.' ).
        lo_column->set_output_length( 14 ).
        lo_column->set_alignment( if_salv_c_alignment=>centered ).
        lo_column->set_icon( if_salv_c_bool_sap=>true ).


        lo_column ?= lo_cols->get_column( 'CPUDT' ).
        lo_column->set_long_text( 'Data Doc. Material' ).
        lo_column->set_medium_text( 'Dt. Doc. Mat.' ).
        lo_column->set_short_text( 'Dt. Mat.' ).
        lo_column->set_output_length( 20 ).

        lo_column ?= lo_cols->get_column( 'CPUDT_RB' ).
        lo_column->set_long_text( 'Data Doc. Fatura' ).
        lo_column->set_medium_text( 'Dt. Doc. Fat.' ).
        lo_column->set_short_text( 'Dt. Fat.' ).
        lo_column->set_output_length( 20 ).

        "Alinha o título da coluna para esquerda
*        lo_column = lo_cols->get_column( 'PSPNR' ).
*        lo_column->set_alignment( if_salv_c_alignment=>left ).

        lo_functional_settings = o_alv->get_functional_settings( ).
        lo_tooltips = lo_functional_settings->get_tooltips( ).

      CATCH cx_salv_not_found.

    ENDTRY.

  ENDMETHOD.

  METHOD display_settings.

    DATA lo_display_settings TYPE REF TO cl_salv_display_settings.

    lo_display_settings = o_alv->get_display_settings( ).
    lo_display_settings->set_striped_pattern( 'X' ).

  ENDMETHOD.


  METHOD set_hotspot.

    "Events
    DATA: lo_events TYPE REF TO cl_salv_events_table.

    "get all events
    lo_events = o_alv->get_event( ).

    "event handler
    SET HANDLER co_report->on_click FOR lo_events.
    SET HANDLER co_report->on_user_command FOR lo_events.


  ENDMETHOD.                    "set_hospot

  METHOD on_click.

    FIELD-SYMBOLS: <fs_saida> LIKE LINE OF it_saida.

    READ TABLE lo_alv->it_saida INTO DATA(ls_saida) INDEX row.

    SUBMIT zmmr115
     WITH chaven INCL ls_saida-chave_nfe
     WITH etomad INCL ls_saida-bukrs
     WITH ftomad INCL ls_saida-branch
           AND RETURN.

  ENDMETHOD.

  METHOD on_user_command.

    CASE e_salv_function.
      WHEN '&ATUALIZA'.

        lo_alv->get_dada( ).
        lo_alv->generate_output( ).

      WHEN '&MARK_T'.

      WHEN '&DESMARC_T'.

      WHEN 'BACK'.
        LEAVE TO SCREEN 0.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "on_user_command

  METHOD refresh_table.
    o_alv->refresh( ).
  ENDMETHOD.                    "refresh_table


  METHOD pfstatus.

    co_alv->set_screen_status(
      EXPORTING
        report        =    sy-repid " ABAP Program: Current Main Program
        pfstatus      =    'ZSTANDARD' " Screens, Current GUI Status
        set_functions =   2 "co_alv->c_functions_all  " ALV: Data Element for Constants
    ).

  ENDMETHOD.                    "pfstatus

ENDCLASS.
