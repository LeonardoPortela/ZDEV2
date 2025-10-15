*&---------------------------------------------------------------------*
*& Report ZFIR0126
*&---------------------------------------------------------------------*
*&Consultor ABAP: Renato Aro
*&Data: 23.06.2025
*&---------------------------------------------------------------------*
REPORT zfir0126.


TABLES: zfit186, vbak, kna1.
*----------------------------------------------------------------------*
* DECLARAÇÕES
*----------------------------------------------------------------------*
" Estrutura para os dados da tabela ZFIT186
TYPES: BEGIN OF ty_saida,
         ov_principal    TYPE zfit186-ov_principal,      " Valor Principal
         cd_sol_isen     TYPE zfit186-cd_sol_isen,       " Código Solicitação Isenção
         data_venc       TYPE zfit186-data_venc,         " Data de Vencimento
         moeda           TYPE zfit186-moeda,             " Moeda
         vl_moeda_doc    TYPE zfit186-vl_moeda_doc,      " Valor na Moeda do Documento
         tipo_negocio    TYPE zfit186-tipo_negocio,      " Tipo de Negócio
         usuario_solicit TYPE zfit186-usuario_solicit,   " Usuário Solicitante
         data            TYPE zfit186-data,              " Data solicitação
         hora            TYPE zfit186-hora,              " Hora solicitação
         usuar_lib       TYPE zfit186-usuar_lib,         " Usuário liberação   BUG IMPEDITIVO 186091*
         data_lib        TYPE zfit186-data_lib,          " Data liberação      BUG IMPEDITIVO 186091*
         hora_lib        TYPE zfit186-hora_lib,          " Hora liberação      BUG IMPEDITIVO 186091*
         justificativa   TYPE zfit186-justificativa,     " Justificativa
         status_solicit  TYPE zfit186-status_solicit,    " Status da Solicitação
         status_txt      TYPE c LENGTH 30,
         dtcan           TYPE zfit186-dtcan,             " Data Cancelamento
         hrcan           TYPE zfit186-hrcan,             " Hora Cancelamento
         juscanc         TYPE zfit186-juscanc,           " Justificativa Cancelamento
         usrcanc         TYPE zfit186-usrcanc,
       END OF ty_saida.


* Estrutura baseada na tabela ZSDT0337
TYPES: BEGIN OF ty_zsdt0337,
         bukrs              TYPE zsdt0337-bukrs,                 " Empresa
         cod_solict_isencao TYPE zsdt0337-cod_solict_isencao,    " Código Solicitação Isenção
         vbeln              TYPE zsdt0337-vbeln,                 " Documento de vendas
         nivel              TYPE zsdt0337-nivel,                 " Nível
         aprovador          TYPE zsdt0337-aprovador,             " Aprovador
         data_atual         TYPE zsdt0337-data_atual,            " Data atual
         hora_atual         TYPE zsdt0337-hora_atual,            " Hora atual
         valor_de           TYPE zsdt0337-valor_de,              " Valor de
         valor_ate          TYPE zsdt0337-valor_ate,             " Valor até
         usuario            TYPE zsdt0337-usuario,               " Usuário
         status_apr         TYPE zsdt0337-status_apr,            " Status aprovação
         status_txt         TYPE c LENGTH 30,
         valor_moeda_doc    TYPE zsdt0337-valor_moeda_doc,       " Valor moeda documento
         moeda_doc          TYPE zsdt0337-moeda_doc,             " Moeda documento
         valor_brl          TYPE zsdt0337-valor_brl,             " Valor em BRL
       END OF ty_zsdt0337.

DATA gt_fi186 TYPE TABLE OF zfit186.

" Variáveis para ALV
DATA: go_alv       TYPE REF TO cl_salv_table,
      go_functions TYPE REF TO cl_salv_functions,
      lo_columns   TYPE REF TO cl_salv_columns_table,
      lo_column    TYPE REF TO cl_salv_column_table,
      go_layout    TYPE REF TO cl_salv_layout,
      gs_key       TYPE salv_s_layout_key.


DATA   go_container   TYPE REF TO cl_gui_custom_container.


DATA go_textedit             TYPE REF TO cl_gui_textedit.
DATA gt_text                 TYPE TABLE OF as4text.

DATA tg_texto     TYPE catsxt_longtext_itab.
DATA gv_display .
DATA wa_saida  TYPE ty_saida.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:

                  s_bukrs  FOR vbak-vkorg,               " Empresa
                  s_vkbur  FOR vbak-vkbur,               " Escritório de Vendas
                  s_ovprin FOR zfit186-ov_principal,     " Valor Principal
                  s_kunnr  FOR kna1-kunnr,               " Cliente
                  s_cdisen FOR zfit186-cd_sol_isen,      " Código Solicitação Isenção
                  s_data   FOR zfit186-data.             " Data
SELECTION-SCREEN END OF BLOCK b1.


CLASS lcl_alv DEFINITION.

  PUBLIC SECTION.

    DATA o_alv  TYPE REF TO cl_salv_table.
    DATA o_alv2 TYPE REF TO cl_salv_table.

    DATA it_saida  TYPE TABLE OF ty_saida.

    DATA it_0337   TYPE TABLE OF ty_zsdt0337.
    DATA: lt_idd07v TYPE TABLE OF  dd07v.

    METHODS:

      get_data,

      get_data2
        IMPORTING
          is_saida TYPE ty_saida,


      generate_output,

      generate_output2.

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
      set_events
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

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " Buscar dados
*  PERFORM buscar_dados.

  " Exibir ALV
*  PERFORM exibir_alv.
  DATA lo_alv TYPE REF TO lcl_alv.

  CREATE OBJECT lo_alv.

  DATA lo_alv2 TYPE REF TO lcl_alv.

  CREATE OBJECT lo_alv2.

  lo_alv->get_data( ).

  lo_alv->generate_output( ).


CLASS lcl_alv IMPLEMENTATION.

  METHOD get_data.

    SELECT *
      FROM zfit186
      INTO TABLE @gt_fi186
      WHERE ov_principal IN @s_ovprin
        AND cd_sol_isen IN @s_cdisen
        AND data IN @s_data.
    IF sy-subrc NE 0.
      MESSAGE |Não existem dados para exibição| TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.


      it_saida = VALUE #( FOR ls_fi186 IN gt_fi186
                        ( CORRESPONDING #( ls_fi186 )

                           ) ).


      LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).



        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname        = 'ZDOFI_TP_NEG'
            text           = 'X'
            langu          = sy-langu
          TABLES
            dd07v_tab      = lt_idd07v
          EXCEPTIONS
            wrong_textflag = 1
            OTHERS         = 2.


        READ TABLE lt_idd07v INTO DATA(ls_idd07v) WITH KEY domvalue_l = <fs_saida>-tipo_negocio.
        IF sy-subrc EQ 0.

          <fs_saida>-tipo_negocio = ls_idd07v-ddtext.

        ENDIF.

        <fs_saida>-justificativa = icon_display_note.

        <fs_saida>-juscanc       = icon_display_note.

        CLEAR lt_idd07v.
        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname        = 'ZDOFI_STAT_SOLIC2'
            text           = 'X'
            langu          = sy-langu
          TABLES
            dd07v_tab      = lt_idd07v
          EXCEPTIONS
            wrong_textflag = 1
            OTHERS         = 2.

        CLEAR ls_idd07v.
        READ TABLE lt_idd07v INTO ls_idd07v WITH KEY domvalue_l = <fs_saida>-status_solicit.
        IF sy-subrc EQ 0.

          <fs_saida>-status_txt = ls_idd07v-ddtext.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD get_data2.


    FREE it_0337.

    SELECT *
      FROM zsdt0337
      INTO CORRESPONDING FIELDS OF TABLE it_0337
      WHERE cod_solict_isencao EQ is_saida-cd_sol_isen
      AND   vbeln              EQ is_saida-ov_principal.


    LOOP AT it_0337 ASSIGNING FIELD-SYMBOL(<fs_0337>).

      CLEAR lt_idd07v.
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'ZDOFI_STAT_SOLIC'
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = lt_idd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.


      READ TABLE lt_idd07v INTO DATA(ls_idd07v) WITH KEY domvalue_l = <fs_0337>-status_apr.
      IF sy-subrc EQ 0.

        <fs_0337>-status_txt = ls_idd07v-ddtext.

      ENDIF.
    ENDLOOP.
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
  METHOD generate_output2.

    DATA: lx_msg TYPE REF TO cx_salv_msg.
    TRY.
        cl_salv_table=>factory(
         IMPORTING
           r_salv_table = o_alv2
         CHANGING
           t_table = it_0337 ).

      CATCH cx_salv_msg INTO lx_msg.

    ENDTRY.
* CS2024000604 - Erros diversos Isenção de Juros #189279 - RU 04-09-2025
    "Menu PFStatus
    CALL METHOD pfstatus
      CHANGING
        co_alv = o_alv2.
* CS2024000604 - Erros diversos Isenção de Juros #189279 - RU 04-09-2025

    "Chamada do método para ajustar colunas
    CALL METHOD ajusta_colunas
      CHANGING
        co_alv = o_alv2.

* CS2024000604 - Erros diversos Isenção de Juros #189279 - RU 04-09-2025
    "Set Up the Hotspot & Event Handler
    CALL METHOD set_events
      CHANGING
        co_alv    = o_alv2
        co_report = lo_alv2.
* CS2024000604 - Erros diversos Isenção de Juros #189279 - RU 04-09-2025
    o_alv2->display( ).


  ENDMETHOD.
  METHOD ajusta_colunas.

    INCLUDE: <icon>.

    DATA lo_cols TYPE REF TO cl_salv_columns.
    DATA lo_column TYPE REF TO cl_salv_column_table.
    DATA lo_functions  TYPE REF TO cl_salv_functions_list.

    DATA: lo_functional_settings  TYPE REF TO cl_salv_functional_settings.
    DATA: lo_tooltips             TYPE REF TO cl_salv_tooltips.


    TRY .

        IF o_alv IS BOUND.

          lo_functions = o_alv->get_functions( ).
          lo_functions->set_all( abap_true ).

          lo_cols = o_alv->get_columns( ).
          lo_cols->set_optimize( 'X' ).

          " Valor Principal
          lo_column ?= lo_cols->get_column( 'OV_PRINCIPAL' ).
          lo_column->set_short_text( 'Ov. Princ.' ).
          lo_column->set_medium_text( 'Ordem V. Principal' ).
          lo_column->set_long_text( 'Ordem de Venda Principal' ).

          " Código Solicitação Isenção
          lo_column ?= lo_cols->get_column( 'CD_SOL_ISEN' ).
          lo_column->set_short_text( 'Cód.Sol.' ).
          lo_column->set_medium_text( 'Cód.Sol.Isenção' ).
          lo_column->set_long_text( 'Código Solicitação Isenção' ).

          " Data Vencimento
          lo_column ?= lo_cols->get_column( 'DATA_VENC' ).
          lo_column->set_short_text( 'Dt.Venc' ).
          lo_column->set_medium_text( 'Data Vencimento' ).
          lo_column->set_long_text( 'Data de Vencimento' ).
          lo_column->set_optimized('X').


          " Moeda
          lo_column ?= lo_cols->get_column( 'MOEDA' ).
          lo_column->set_short_text( 'Moeda' ).
          lo_column->set_medium_text( 'Moeda' ).
          lo_column->set_long_text( 'Moeda do Documento' ).

          " Valor Moeda Documento
          lo_column ?= lo_cols->get_column( 'VL_MOEDA_DOC' ).
          lo_column->set_short_text( 'Valor Doc' ).
          lo_column->set_medium_text( 'Valor Moeda Doc' ).
          lo_column->set_long_text( 'Valor na Moeda do Documento' ).

          " Tipo Negócio
          lo_column ?= lo_cols->get_column( 'TIPO_NEGOCIO' ).
          lo_column->set_short_text( 'Tp.Negóc' ).
          lo_column->set_medium_text( 'Tipo Negócio' ).
          lo_column->set_long_text( 'Tipo de Negócio' ).
          lo_column->set_output_length( 20 ).

          " Usuário Solicitante
          lo_column ?= lo_cols->get_column( 'USUARIO_SOLICIT' ).
          lo_column->set_short_text( 'Usuário S' ).
          lo_column->set_medium_text( 'Usuário Solic' ).
          lo_column->set_long_text( 'Usuário Solicitante' ).

          " Data
          lo_column ?= lo_cols->get_column( 'DATA' ).
          lo_column->set_short_text( 'Data Sol' ).
          lo_column->set_medium_text( 'Data Solicit' ).
          lo_column->set_long_text( 'Data da Solicitação' ).

          " Hora
          lo_column ?= lo_cols->get_column( 'HORA' ).
          lo_column->set_short_text( 'Hora Sol' ).
          lo_column->set_medium_text( 'Hora Solicit' ).
          lo_column->set_long_text( 'Hora da Solicitação' ).

*   BUG IMPEDITIVO 186091*

          " Usuário liberação
          lo_column ?= lo_cols->get_column( 'USUAR_LIB' ).
          lo_column->set_short_text( 'Usuário L' ).
          lo_column->set_medium_text( 'Usuário Liber' ).
          lo_column->set_long_text( 'Usuário Liberação' ).

          " Data
          lo_column ?= lo_cols->get_column( 'DATA_LIB' ).
          lo_column->set_short_text( 'Data Lib' ).
          lo_column->set_medium_text( 'Data Liber' ).
          lo_column->set_long_text( 'Data da Liberação' ).

          " Hora
          lo_column ?= lo_cols->get_column( 'HORA_LIB' ).
          lo_column->set_short_text( 'Hora Lib' ).
          lo_column->set_medium_text( 'Hora Liber' ).
          lo_column->set_long_text( 'Hora da Liberação' ).

*   BUG IMPEDITIVO 186091*


          lo_column ?= lo_cols->get_column( 'JUSTIFICATIVA' ).
          CALL METHOD lo_column->set_cell_type
            EXPORTING
              value = if_salv_c_cell_type=>hotspot.

          " Justificativa
          lo_column ?= lo_cols->get_column( 'JUSTIFICATIVA' ).
          lo_column->set_short_text( 'Justific.' ).
          lo_column->set_medium_text( 'Justificativa' ).
          lo_column->set_long_text( 'Justificativa da Solicitação' ).
          lo_column->set_icon( if_salv_c_bool_sap=>true ).


          lo_column ?= lo_cols->get_column( 'STATUS_TXT' ).
          CALL METHOD lo_column->set_cell_type
            EXPORTING
              value = if_salv_c_cell_type=>hotspot.


          " Status Solicitação
          lo_column ?= lo_cols->get_column( 'STATUS_TXT' ).
          lo_column->set_short_text( 'Status' ).
          lo_column->set_medium_text( 'Status Solicit' ).
          lo_column->set_long_text( 'Status da Solicitação' ).

          lo_column ?= lo_cols->get_column( 'STATUS_SOLICIT' ).
          lo_column->set_visible( abap_false ).

          " Data Cancelamento
          lo_column ?= lo_cols->get_column( 'DTCAN' ).
          lo_column->set_short_text( 'Dt.Cancel' ).
          lo_column->set_medium_text( 'Data Cancel.' ).
          lo_column->set_long_text( 'Data de Cancelamento' ).

          " Hora Cancelamento
          lo_column ?= lo_cols->get_column( 'HRCAN' ).
          lo_column->set_short_text( 'Hr.Cancel' ).
          lo_column->set_medium_text( 'Hora Cancel.' ).
          lo_column->set_long_text( 'Hora de Cancelamento' ).


          lo_column ?= lo_cols->get_column( 'JUSCANC' ).
          CALL METHOD lo_column->set_cell_type
            EXPORTING
              value = if_salv_c_cell_type=>hotspot.

          " Justificativa Cancelamento
          lo_column ?= lo_cols->get_column( 'JUSCANC' ).
          lo_column->set_short_text( 'Just.Canc' ).
          lo_column->set_medium_text( 'Justif. Cancel.' ).
          lo_column->set_long_text( 'Justificativa do Cancelamento' ).


          " Hora Cancelamento
          lo_column ?= lo_cols->get_column( 'USRCANC' ).
          lo_column->set_short_text( 'Usr.Canc' ).
          lo_column->set_medium_text( 'Usr. Cancel.' ).
          lo_column->set_long_text( 'Usuário Cancelamento' ).

          lo_functional_settings = o_alv->get_functional_settings( ).

        ENDIF.

        IF o_alv2 IS BOUND.

          lo_functions = o_alv2->get_functions( ).
          lo_functions->set_all( abap_true ).

          lo_cols = o_alv2->get_columns( ).
          lo_cols->set_optimize( 'X' ).

          " BUKRS - Company Code
          lo_column ?= lo_cols->get_column( 'BUKRS' ).
          lo_column->set_short_text( 'Emp' ).
          lo_column->set_medium_text( 'Empresa' ).
          lo_column->set_long_text( 'Código da Empresa' ).

          " COD_SOLICT_ISENCAO - Exemption Request Code
          lo_column ?= lo_cols->get_column( 'COD_SOLICT_ISENCAO' ).
          lo_column->set_short_text( 'Cód Isen' ).
          lo_column->set_medium_text( 'Cód Isenção' ).
          lo_column->set_long_text( 'Código Solicitação Isenção' ).

          " VBELN - Sales Document Number
          lo_column ?= lo_cols->get_column( 'VBELN' ).
          lo_column->set_short_text( 'OV. Princ.' ).
          lo_column->set_medium_text( 'OV. Principal' ).
          lo_column->set_long_text( 'Ordem de Venda Principal' ).

          " NIVEL - Level
          lo_column ?= lo_cols->get_column( 'NIVEL' ).
          lo_column->set_short_text( 'Nível' ).
          lo_column->set_medium_text( 'Nível' ).
          lo_column->set_long_text( 'Nível de Aprovação' ).

          " APROVADOR - Approver
          lo_column ?= lo_cols->get_column( 'APROVADOR' ).
          lo_column->set_short_text( 'Aprov' ).
          lo_column->set_medium_text( 'Aprovador' ).
          lo_column->set_long_text( 'Usuário Aprovador' ).

          " DATA_ATUAL - Current Date
          lo_column ?= lo_cols->get_column( 'DATA_ATUAL' ).
          lo_column->set_short_text( 'Data' ).
          lo_column->set_medium_text( 'Data Atual' ).
          lo_column->set_long_text( 'Data Atual' ).

          " HORA_ATUAL - Current Time
          lo_column ?= lo_cols->get_column( 'HORA_ATUAL' ).
          lo_column->set_short_text( 'Hora' ).
          lo_column->set_medium_text( 'Hora Atual' ).
          lo_column->set_long_text( 'Hora Atual' ).

          " VALOR_DE - Value From
          lo_column ?= lo_cols->get_column( 'VALOR_DE' ).
          lo_column->set_short_text( 'Vlr De' ).
          lo_column->set_medium_text( 'Valor De' ).
          lo_column->set_long_text( 'Valor Inicial' ).
          lo_column->set_output_length( 15 ).


          " VALOR_ATE - Value To
          lo_column ?= lo_cols->get_column( 'VALOR_ATE' ).
          lo_column->set_short_text( 'Vlr Até' ).
          lo_column->set_medium_text( 'Valor Até' ).
          lo_column->set_long_text( 'Valor Final' ).

          " USUARIO - User
          lo_column ?= lo_cols->get_column( 'USUARIO' ).
          lo_column->set_short_text( 'Usuário' ).
          lo_column->set_medium_text( 'Usuário' ).
          lo_column->set_long_text( 'Código do Usuário' ).

          " STATUS_APR - Approval Status
          lo_column ?= lo_cols->get_column( 'STATUS_TXT' ).
          lo_column->set_short_text( 'Status' ).
          lo_column->set_medium_text( 'Status Aprov' ).
          lo_column->set_long_text( 'Status de Aprovação' ).


          lo_column ?= lo_cols->get_column( 'STATUS_APR' ).
          lo_column->set_visible( abap_false ).

          " VALOR_MOEDA_DOC - Document Currency Value
          lo_column ?= lo_cols->get_column( 'VALOR_MOEDA_DOC' ).
          lo_column->set_short_text( 'Vlr Moeda' ).
          lo_column->set_medium_text( 'Valor Moeda Doc' ).
          lo_column->set_long_text( 'Valor na Moeda do Documento' ).

          " MOEDA_DOC - Document Currency
          lo_column ?= lo_cols->get_column( 'MOEDA_DOC' ).
          lo_column->set_short_text( 'Moeda' ).
          lo_column->set_medium_text( 'Moeda Doc' ).
          lo_column->set_long_text( 'Moeda do Documento' ).

          " VALOR_BRL - Value in BRL
          lo_column ?= lo_cols->get_column( 'VALOR_BRL' ).
          lo_column->set_short_text( 'Vlr BRL' ).
          lo_column->set_medium_text( 'Valor BRL' ).
          lo_column->set_long_text( 'Valor em Reais' ).


          lo_functional_settings = o_alv2->get_functional_settings( ).

        ENDIF.


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
    lo_events = co_alv->get_event( ).

    "event handler
    SET HANDLER co_report->on_click FOR lo_events.

    SET HANDLER co_report->on_user_command FOR lo_events.


  ENDMETHOD.                    "set_hospot
* CS2024000604 - Erros diversos Isenção de Juros #189279 - RU 04-09-2025
  METHOD set_events.
    "Events
    DATA: lo_events TYPE REF TO cl_salv_events_table.

    "get all events
    lo_events = co_alv->get_event( ).

    SET HANDLER co_report->on_user_command FOR lo_events.
  ENDMETHOD.
* CS2024000604 - Erros diversos Isenção de Juros #189279 - RU 04-09-2025
  METHOD on_click.
    DATA: lv_texto_alt TYPE string,
          lv_pos       TYPE i.

    DATA lv_data TYPE c LENGTH 10.
    DATA lv_data_lib TYPE c LENGTH 10.
    DATA lv_hora TYPE c LENGTH 10.
    DATA lv_hora_lib TYPE c LENGTH 10.


    FIELD-SYMBOLS: <fs_saida> LIKE LINE OF it_saida.

*    READ TABLE lo_alv->it_saida INTO DATA(ls_saida) INDEX row.
    READ TABLE lo_alv->it_saida INTO wa_saida INDEX row.
    DATA(ls_saida) = wa_saida.

    CASE column.

      WHEN 'JUSTIFICATIVA'.

        FREE tg_texto.

        READ TABLE gt_fi186 INTO DATA(ls_fi186) WITH KEY ov_principal = ls_saida-ov_principal
                                                         cd_sol_isen  = ls_saida-cd_sol_isen.

        WRITE ls_fi186-data TO lv_data DD/MM/YY.

        lv_hora = ls_fi186-hora(2) && ':' && ls_fi186-hora+2(2) && ':' && ls_fi186-hora+4(2).

        APPEND INITIAL LINE TO tg_texto ASSIGNING FIELD-SYMBOL(<fs_texto>).
        <fs_texto> = |Usuário solicitante: | && ls_fi186-usuario_solicit && ' Data: ' && lv_data && ' Hora: ' && lv_hora.

        CLEAR: lv_texto_alt, lv_pos.

        lv_texto_alt = ls_fi186-justificativa.

        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_texto_alt WITH ''.  "

        " Quebra automaticamente em partes de 72
        DO.
          IF strlen( lv_texto_alt ) <= 72.
            " Última linha
            IF strlen( lv_texto_alt ) > 0.
              APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
              <fs_texto> = lv_texto_alt.
            ENDIF.
            EXIT.
          ELSE.
            " Adiciona parte de 72 caracteres
            APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
            <fs_texto> = lv_texto_alt(72).
            " Remove os 72 primeiros caracteres
            lv_texto_alt = lv_texto_alt+72.
          ENDIF.
        ENDDO.

        APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
        <fs_texto> = '-----------------------------------'.

        "liberação do Gerente
        WRITE ls_fi186-data_lib TO lv_data_lib DD/MM/YY.

        lv_hora_lib = ls_fi186-hora_lib(2) && ':' && ls_fi186-hora_lib+2(2) && ':' && ls_fi186-hora_lib+4(2).

        APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
        <fs_texto> = |Usuário liberação: | && ls_fi186-usuar_lib && ' Data: ' && lv_data_lib && ' Hora: ' && lv_hora_lib.

        CLEAR: lv_texto_alt, lv_pos.

        lv_texto_alt = ls_fi186-justificativa_ger.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_texto_alt WITH ''.  "
        " Quebra automaticamente em partes de 72
        DO.
          IF strlen( lv_texto_alt ) <= 72.
            " Última linha
            IF strlen( lv_texto_alt ) > 0.
              APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
              <fs_texto> = lv_texto_alt.
            ENDIF.
            EXIT.
          ELSE.
            " Adiciona parte de 72 caracteres
            APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
            <fs_texto> = lv_texto_alt(72).
            " Remove os 72 primeiros caracteres
            lv_texto_alt = lv_texto_alt+72.
          ENDIF.
        ENDDO.

        gv_display = 'X'.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Texto para Motivo'
            im_display_mode = gv_display " Somente vizualizar ou inserir
          CHANGING
            ch_text         = tg_texto.

      WHEN 'JUSCANC'.

        FREE tg_texto.
        UNASSIGN <fs_texto>.

        READ TABLE gt_fi186 INTO ls_fi186  WITH KEY ov_principal = ls_saida-ov_principal
                                                    cd_sol_isen  = ls_saida-cd_sol_isen.

*        CALL SCREEN '0100' STARTING AT 1 1 ENDING AT 80 20.
        CLEAR: lv_texto_alt, lv_pos.
        lv_texto_alt = ls_fi186-juscanc.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_texto_alt WITH ''.  "
        " Quebra automaticamente em partes de 72
        DO.
          IF strlen( lv_texto_alt ) <= 72.
            " Última linha
            IF strlen( lv_texto_alt ) > 0.
              APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
              <fs_texto> = lv_texto_alt.
            ENDIF.
            EXIT.
          ELSE.
            " Adiciona parte de 72 caracteres
            APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
            <fs_texto> = lv_texto_alt(72).
            " Remove os 72 primeiros caracteres
            lv_texto_alt = lv_texto_alt+72.
          ENDIF.
        ENDDO.
        gv_display = 'X'.


        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Texto para Motivo'
            im_display_mode = gv_display " Somente vizualizar ou inserir
          CHANGING
            ch_text         = tg_texto.


      WHEN 'STATUS_TXT'.

        lo_alv2->get_data2( ls_saida ).

        IF lo_alv2->it_0337 IS NOT INITIAL.
          lo_alv2->generate_output2( ).
        ENDIF.


      WHEN OTHERS.

    ENDCASE.


  ENDMETHOD.

  METHOD on_user_command.

    CASE e_salv_function.

      WHEN 'REFRESH'.  " Botão Refresh nativo
        " Limpa a tabela atual
        CLEAR: it_saida, gt_fi186.
* CS2024000604 - Erros diversos Isenção de Juros #189279 - RU 04-09-2025
        IF o_alv IS NOT INITIAL.
          " Busca os dados novamente
          get_data( ).

          " Atualiza o ALV
          o_alv->refresh( ).
        ENDIF.

        IF wa_saida IS NOT INITIAL AND o_alv2 IS NOT INITIAL.
          get_data2( wa_saida ).

          o_alv2->refresh( ).
        ENDIF.

        " Exibe mensagem de sucesso
        MESSAGE 'Dados atualizados com sucesso!' TYPE 'S'.
* CS2024000604 - Erros diversos Isenção de Juros #189279 - RU 04-09-2025

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

* "Implementado botao atualizar no alv - SMC - 25-08-2025
    DATA: lo_functions TYPE REF TO cl_salv_functions_list.

*  " Obtém as funções do ALV
*  lo_functions = co_alv->get_functions( ).
*
*  " Habilita todas as funções padrão (inclui Refresh)
*  lo_functions->set_all( abap_true ).

    co_alv->set_screen_status(
       pfstatus      =  'SALV_STANDARD'
       report        =  sy-repid
       set_functions = co_alv->c_functions_all ).

*"Implementado botao atualizar no alv - SMC - 25-08-2025
  ENDMETHOD.                    "pfstatus

ENDCLASS.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
* SET TITLEBAR 'xxx'.


  IF go_container IS NOT BOUND.

*   create control container
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'CC_JUST'
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc = 0.

      CREATE OBJECT go_textedit
        EXPORTING
          max_number_chars           = 255
          parent                     = go_container
          wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position          = 60
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.




      CALL METHOD go_textedit->set_text_as_stream
        EXPORTING
          text            = gt_text
        EXCEPTIONS
          error_dp        = 1
          error_dp_create = 2
          OTHERS          = 3.


    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.

    WHEN 'BT_SAIR' OR 'EXIT'.

      CALL METHOD go_textedit->delete_text.

      SET SCREEN 0.
      LEAVE SCREEN.

*    WHEN 'BT_CONF'.
*
*      PERFORM processa_justificativa.
*
*      SET SCREEN 0.
*      LEAVE SCREEN.

  ENDCASE.
ENDMODULE.
