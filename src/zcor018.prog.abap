*&--------------------------------------------------------------------&*
*&                         CO                                         &*
*&--------------------------------------------------------------------&*
*& Projeto..: Amaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 29/10/2015                                              &*
*& Descrição: Relatório – Relatório de Ordens Estatísticas            &*
*& Transação: ZCOR                                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT  zcor018.


*=============================================================================*
*Estrutura Alv                                                                *
*=============================================================================*
DATA:it_fcat    TYPE TABLE OF lvc_s_fcat,
     it_list    TYPE vrm_values,
     list_value TYPE vrm_values,
     wa_layout  TYPE lvc_s_layo,
     wa_cont    TYPE REF TO cl_gui_custom_container,
     wa_alv     TYPE REF TO  cl_gui_alv_grid.


*----------------------------------------------------------------------*
***INCLUDE Zmmr101_0001 .
*----------------------------------------------------------------------*

DATA: dg_dyndoc_id     TYPE REF TO cl_dd_document.

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: l_graphic_xstr TYPE xstring.
DATA: graphic_size   TYPE i.
DATA: l_graphic_conv TYPE i.
DATA: l_graphic_offs TYPE i.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
*  public section.
*    methods handle_hotspot_click
*      for event hotspot_click of cl_gui_alv_grid
*      importing e_row_id
*                e_column_id
*                es_row_no.
*    METHODS TOP_OF_PAGE
*      FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
*      IMPORTING E_DYNDOC_ID.
ENDCLASS.                    "lcl_event_handler DEFINITION

*=============================================================================*
*Estrutura cabeçalho Alv                                                      *
*=============================================================================*
DATA: picture          TYPE REF TO cl_gui_picture,
      gf_first_display TYPE c VALUE 'X',
      ctl_cccontainer  TYPE REF TO cl_gui_custom_container,
      dg_splitter      TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2    TYPE REF TO cl_gui_splitter_container,
      dg_parent_html   TYPE REF TO cl_gui_container,
      dg_parent_html1  TYPE REF TO cl_gui_container,
      dg_parent_html2  TYPE REF TO cl_gui_container,
      dg_parent_grid   TYPE REF TO cl_gui_container,
      event_handler    TYPE REF TO lcl_event_handler,
      dg_html_cntrl    TYPE REF TO cl_gui_html_viewer,
      ctl_alv_resumo   TYPE REF TO cl_gui_alv_grid,
      gs_scroll_col    TYPE lvc_s_col,
      gs_scroll_row    TYPE lvc_s_roid,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant,
      it_exclude_fcode TYPE ui_functions.

*=============================================================================*
*TABELAS                                                                      *
*=============================================================================*

TABLES: coas, bkpf.

*=============================================================================*
*Estrutura                                                                    *
*=============================================================================*
TYPES:
  BEGIN OF ty_saida,

    aufnr     TYPE coas-aufnr,
    auart     TYPE  coas-auart,
    ktext     TYPE coas-ktext,
    bukrs     TYPE coas-bukrs,
    werks     TYPE coas-werks,
    gsber     TYPE coas-gsber,
    kostv     TYPE coas-kostv,
    cycle     TYPE coas-cycle,
    posnr     TYPE imakz-posnr,
    txt50     TYPE imakt-txt50,
    erdat     TYPE coas-erdat  ,               "Data Abertura
    idat2     TYPE coas-idat2  ,               "Data Encerramento Técnico
    idat3     TYPE coas-idat3  ,               "Data Encerramento Comercial
    status(4)               ,               "Status da Ordem


  END OF ty_saida,

  BEGIN OF ty_sintetico_saida,
    gjahr      TYPE bsis-gjahr,         "Ano
    aufnr      TYPE kaep_coep_x-aufnr,  "Ordem
    budat      TYPE cobk-budat,         "Data     "Selecionar o mes da data
    wogbtr     TYPE kaep_coep_x-wogbtr, "Valor/Mobj
    wkgbtr     TYPE kaep_coep_x-wkgbtr, "Valor/MACC
    bukrs      TYPE bsis-bukrs,         "Empresa
    idat1      TYPE aufk-idat1,          "Data abert
    idat3      TYPE aufk-idat3,          "Data enc
    auart      TYPE aufk-auart,          "Tipo ordem
    status(30) TYPE c,
  END OF ty_sintetico_saida.

*=============================================================================*
*TABELA INTERNA                                                               *
*=============================================================================*
DATA:
  it_coas  TYPE TABLE OF coas,
  it_imakz TYPE TABLE OF imakz,
  it_imak  TYPE TABLE OF imak,
  it_imakt TYPE TABLE OF imakt,
  it_jest  TYPE TABLE OF jest,
  it_jsto  TYPE TABLE OF jsto,
  it_tj02t TYPE TABLE OF tj02t,
  it_saida TYPE TABLE OF ty_saida.

*=============================================================================*
*WORK AREA                                                                    *
*=============================================================================*
DATA:
  wa_coas            TYPE  coas,
  wa_imakz           TYPE  imakz,
  wa_imak            TYPE  imak,
  wa_imakt           TYPE  imakt,
  wa_jest            TYPE  jest,
  wa_jsto            TYPE  jsto,
  wa_tj02t           TYPE  tj02t,
  wa_saida           TYPE  ty_saida,
  wa_sintetico_saida TYPE  ty_sintetico_saida,
  pmes1              TYPE c,
  group_id(3)        TYPE c,
  v_radio(10)        TYPE c,
  v_alv(30)          TYPE c.


*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
DATA: gd_ucomm TYPE sy-ucomm.
*=============================================================================*
*FIELD-SYMBOLS - Get/Prepara Dados                                                              *
*=============================================================================*
FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
               <lt_data_line> TYPE ANY TABLE,
               <ls_data>      TYPE any,
               <ls_data_line> TYPE any.

DATA:
  lr_data                TYPE REF TO data,
  lr_data_line           TYPE REF TO data,
  lr_data_descr          TYPE REF TO cl_abap_datadescr,
  lr_data_line_descr     TYPE REF TO cl_abap_datadescr,
  it_alv_saida           TYPE TABLE OF ty_sintetico_saida,
  it_sintetico_alv_saida TYPE TABLE OF ty_sintetico_saida.


DATA: BEGIN OF  t_dfies OCCURS 0.
        INCLUDE STRUCTURE dfies.
DATA: END OF t_dfies.
DATA: i_dfies TYPE dfies.

*=============================================================================*
SELECTION-SCREEN:  BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_kokrs FOR coas-kokrs OBLIGATORY,                        "Area de custo
                  s_bukrs FOR coas-bukrs,                                   "Empresa
                  s_werks FOR coas-werks,                                   "Centro
                  s_autyp FOR coas-autyp,                                   "Categoria
                  s_auart FOR coas-auart,                                  "Tipo Ordem
                  s_dtfaix  FOR sy-datum OBLIGATORY NO-EXTENSION.       "Periodo  (Obrigatório ) ( BUDAT-COBK ) MODIF ID bl3

  SELECTION-SCREEN: SKIP.

  SELECTION-SCREEN BEGIN OF LINE.

    SELECTION-SCREEN COMMENT (13) TEXT-001.

    PARAMETERS p_abt AS CHECKBOX USER-COMMAND check1.
    SELECTION-SCREEN COMMENT (13) TEXT-003.

    PARAMETERS p_lib AS CHECKBOX USER-COMMAND check1.
    SELECTION-SCREEN COMMENT (15) TEXT-004.

    PARAMETERS p_enc AS CHECKBOX USER-COMMAND check1.
    SELECTION-SCREEN COMMENT (13) TEXT-005.

    PARAMETERS p_tod AS CHECKBOX USER-COMMAND check1.
    SELECTION-SCREEN COMMENT (15) TEXT-006.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN:
  BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS : p_rd1b2 RADIOBUTTON GROUP gp1 DEFAULT 'X' USER-COMMAND check1.
    SELECTION-SCREEN COMMENT 3(20) v_ana.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS : p_rd2b2 RADIOBUTTON GROUP gp1.
    SELECTION-SCREEN COMMENT 3(20) v_sin.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN: END OF BLOCK b1.



*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
START-OF-SELECTION.




  IF v_radio = 'ANALITICO'.
    PERFORM:  f_seleciona_dados_analitico, " Form selecionar dados

              f_organiza_dados_analitico, " Form organiza dados
               f_alv_analitico.

  ELSE.

    IF v_radio = 'SINTETICO' AND s_dtfaix-low = '00000000'.
      MESSAGE 'Escolha uma Data ou Período' TYPE 'S'.
      STOP.
    ENDIF.

    PERFORM:  f_seleciona_dados_analitico,
              f_organiza_dados_analitico.

    DATA: lr_ordem TYPE RANGE OF range_auf.

    FREE:lr_ordem.

    lr_ordem[] = VALUE #( FOR wa_params1 IN it_saida WHERE ( aufnr <> space ) ( option = 'EQ' sign = 'I' low = wa_params1-aufnr ) ).

    SORT lr_ordem BY low.

    DELETE ADJACENT DUPLICATES FROM lr_ordem COMPARING ALL FIELDS.

    PERFORM f_prepare_run_time_info.

    SUBMIT rkaep000  WITH p_tcode   EQ 'KOB1'
                              WITH aufnr IN lr_ordem
                              WITH p_kokrs   EQ 'MAGI'
                              WITH r_budat   IN s_dtfaix
                              WITH p_werks   IN s_werks
                              WITH p_bukrs   IN s_bukrs
                              WITH p_usedb   = 'X'
                              WITH p_usear   = ' '
                              WITH p_maxsel  = 200000
                              WITH p_disvar  = '/CONFERENCIA'
                              AND RETURN.



    PERFORM: f_get_runtime_info,
              f_organiza_dados_sintetico,
              f_alv_sintetico.






  ENDIF.



*END-OF-SELECTION.
  CALL SCREEN 0100.


INITIALIZATION.

************************************************************************
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN.
  gd_ucomm = sy-ucomm.


************************************************************************
*AT SELECTION-SCREEN OUTPUT.
AT SELECTION-SCREEN OUTPUT.


  v_ana = 'Analitico'.
  v_sin = 'Sintético'.

  PERFORM verifica_radio.

  CASE 'X'.
    WHEN p_abt.
      LOOP AT SCREEN.
        IF ( screen-name CS 'P_ABT' OR screen-name CS 'P_LIB' OR screen-name CS 'P_ENC' OR screen-name CS 'KOKRS' OR screen-name CS 'BUKRS' OR screen-name CS 'WERKS'
    OR screen-name CS 'AUTYP' OR screen-name CS 'AUART'  OR screen-name CS 'S_DTFAIX' OR screen-name CS 'p_rd1b2' OR screen-name CS 'p_rd2b2' ) AND ( NOT screen-name CS 'P_TOD' ).
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      PERFORM verifica_radio.
    WHEN p_lib.
      LOOP AT SCREEN.
        IF ( screen-name CS 'P_ABT' OR screen-name CS 'P_LIB' OR screen-name CS 'P_ENC' OR screen-name CS 'KOKRS' OR screen-name CS 'BUKRS' OR screen-name CS 'WERKS'
           OR screen-name CS 'AUTYP' OR screen-name CS 'AUART' OR screen-name CS 'S_DTFAIX' OR screen-name CS 'p_rd1b2' OR screen-name CS 'p_rd2b2' ) AND ( NOT screen-name CS 'P_TOD' ).
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      PERFORM verifica_radio.
    WHEN p_enc.
      LOOP AT SCREEN.
        IF ( screen-name CS 'P_ABT' OR screen-name CS 'P_LIB' OR screen-name CS 'P_ENC' OR screen-name CS 'KOKRS' OR screen-name CS 'BUKRS' OR screen-name CS 'WERKS'
           OR screen-name CS 'AUTYP' OR screen-name CS 'AUART' OR screen-name CS 'S_DTFAIX' OR screen-name CS 'p_rd1b2' OR screen-name CS 'p_rd2b2' ) AND ( NOT screen-name CS 'P_TOD' ).
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      PERFORM verifica_radio.
    WHEN p_tod.
      LOOP AT SCREEN.
        IF (   screen-name CS 'P_TOD' OR screen-name CS 'KOKRS' OR screen-name CS 'BUKRS' OR screen-name CS 'WERKS' OR screen-name CS 'AUTYP' OR screen-name CS 'AUART' OR screen-name CS 'S_DTFAIX' OR screen-name CS 'p_rd1b2' OR screen-name CS 'p_rd2b2' )
           AND ( NOT screen-name CS 'P_ABT' OR screen-name CS 'P_LIB' OR screen-name CS 'P_ENC' ).
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      PERFORM verifica_radio.
  ENDCASE.


*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*

FORM verifica_radio.

  CLEAR: v_radio, v_alv.
  CASE 'X'.
    WHEN p_rd1b2.
      v_radio = 'ANALITICO'.
      v_alv = 'IT_SAIDA'.
    WHEN p_rd2b2.
      v_radio = 'SINTETICO'.
      v_alv = 'IT_SINTETICO_ALV_SAIDA'.
  ENDCASE.


*  LOOP AT SCREEN .
*    IF v_radio = 'SINTETICO' AND screen-group1 = 'BL3' .
*      screen-required = 2.
*    ELSE.
*      screen-required = 0.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.



ENDFORM.




FORM f_seleciona_dados_analitico.

  IF p_abt EQ abap_true AND p_lib NE abap_true AND p_enc NE abap_true AND p_tod NE abap_true.                      "Ordem aberta

    SELECT *
    FROM coas
    INTO TABLE it_coas
    WHERE kokrs IN s_kokrs
      AND bukrs IN s_bukrs
      AND werks IN s_werks
      AND autyp IN s_autyp
      AND auart IN s_auart
      AND erdat IN s_dtfaix
      AND phas0 EQ 'X'.

  ELSEIF p_lib EQ abap_true AND p_enc NE abap_true AND p_abt NE abap_true AND p_tod NE abap_true.                 "ordem liberada
    SELECT *
    FROM coas
    INTO TABLE it_coas
    WHERE kokrs IN s_kokrs
      AND bukrs IN s_bukrs
      AND werks IN s_werks
      AND autyp IN s_autyp
      AND auart IN s_auart
      AND erdat IN s_dtfaix
      AND phas1 EQ 'X'.

  ELSEIF p_enc EQ abap_true AND p_lib NE abap_true  AND p_abt NE abap_true AND p_tod NE abap_true.                "ordem tecnicamente encerrada
    SELECT *
    FROM coas
    INTO TABLE it_coas
    WHERE kokrs IN s_kokrs
      AND bukrs IN s_bukrs
      AND werks IN s_werks
      AND autyp IN s_autyp
      AND auart IN s_auart
      AND erdat IN s_dtfaix
      AND phas2 EQ 'X'
       OR phas3 EQ 'X'.

  ELSEIF p_tod EQ abap_true OR p_abt EQ abap_true AND p_lib EQ abap_true AND p_enc EQ abap_true .                "Todas
    SELECT *
    FROM coas
    INTO TABLE it_coas
    WHERE kokrs IN s_kokrs
      AND bukrs IN s_bukrs
      AND werks IN s_werks
      AND autyp IN s_autyp
      AND auart IN s_auart
      AND erdat IN s_dtfaix.

  ELSEIF p_abt EQ abap_true AND p_lib EQ abap_true AND p_enc NE abap_true AND p_tod NE abap_true.              "aberta e liberada
    SELECT *
    FROM coas
    INTO TABLE it_coas
    WHERE kokrs IN s_kokrs
      AND bukrs IN s_bukrs
      AND werks IN s_werks
      AND autyp IN s_autyp
      AND auart IN s_auart
      AND erdat IN s_dtfaix
      AND phas0 EQ 'X'
       OR phas1 EQ 'X'.

  ELSEIF p_abt EQ abap_true AND p_enc EQ abap_true AND p_lib NE abap_true AND p_tod NE abap_true.             "aberta e encerrada
    SELECT *
    FROM coas
    INTO TABLE it_coas
    WHERE kokrs IN s_kokrs
      AND bukrs IN s_bukrs
      AND werks IN s_werks
      AND autyp IN s_autyp
      AND auart IN s_auart
      AND erdat IN s_dtfaix
      AND phas0 EQ 'X'
       OR phas2 EQ 'X'
       OR phas3 EQ 'X'.

  ELSEIF p_lib EQ abap_true AND p_enc EQ abap_true AND p_abt NE abap_true AND p_tod NE abap_true.            "Liberada e encerrada
    SELECT *
     FROM coas
     INTO TABLE it_coas
     WHERE kokrs IN s_kokrs
       AND bukrs IN s_bukrs
       AND werks IN s_werks
       AND autyp IN s_autyp
       AND auart IN s_auart
      AND erdat IN s_dtfaix
       AND phas1 EQ 'X'
        OR phas2 EQ 'X'
        OR phas3 EQ 'X'.


  ELSEIF s_kokrs = abap_false .
    MESSAGE 'Selecione um Área de Custo!' TYPE 'S'.
    STOP.

  ELSEIF p_abt EQ ' ' AND  p_lib EQ ' ' AND  p_enc EQ  ' ' AND  p_tod EQ ' '.
    MESSAGE 'Marque uma das opções de Status' TYPE 'S'.
    STOP.


  ENDIF.

  SELECT *
  FROM imakz
  INTO TABLE it_imakz
  FOR ALL ENTRIES IN it_coas
  WHERE objnr EQ it_coas-objnr .

  SELECT *
  FROM imak
  INTO TABLE it_imak
  FOR ALL ENTRIES IN it_imakz
  WHERE posnr EQ it_imakz-posnr.

  SELECT *
  FROM imakt
  INTO TABLE it_imakt
  FOR ALL ENTRIES IN it_imak
  WHERE posnr EQ it_imak-posnr.

ENDFORM.                    "F_SELECIONA_DADOS
*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
FORM f_organiza_dados_analitico.

  LOOP AT it_coas INTO wa_coas.

    READ TABLE it_imakz INTO wa_imakz WITH KEY objnr = wa_coas-objnr.
    IF sy-subrc IS INITIAL.
      wa_saida-posnr = wa_imakz-posnr.

      READ TABLE it_imakt INTO wa_imakt WITH KEY posnr = wa_imakz-posnr.
      IF sy-subrc IS INITIAL.
        wa_saida-txt50 = wa_imakt-txt50.
      ENDIF.
    ENDIF.

    wa_saida-aufnr = wa_coas-aufnr.
    wa_saida-auart = wa_coas-auart.
    wa_saida-ktext = wa_coas-ktext.
    wa_saida-bukrs = wa_coas-bukrs.
    wa_saida-werks = wa_coas-werks.
    wa_saida-gsber = wa_coas-gsber.
    wa_saida-kostv = wa_coas-kostv.
    wa_saida-cycle = wa_coas-cycle.
*    WA_SAIDA-POSNR = WA_IMAKZ-POSNR.
    wa_saida-erdat = wa_coas-erdat.
    wa_saida-idat2 = wa_coas-idat2.
    wa_saida-idat3 = wa_coas-idat3.

    APPEND wa_saida TO it_saida.
    CLEAR wa_saida.
  ENDLOOP.

ENDFORM.                    "F_ORGANIZA_DADOS ANALITICO
*=============================================================================*
*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
FORM f_organiza_dados_sintetico.

  IF <lt_data> IS ASSIGNED.
    FREE: it_alv_saida.
    MOVE-CORRESPONDING <lt_data> TO it_alv_saida.

    IF it_alv_saida IS NOT INITIAL.
      SELECT aufnr, idat1,idat2, phas3, auart,phas0,phas1, phas2, idat3 FROM aufk
        INTO TABLE @DATA(it_aufk)
        FOR ALL ENTRIES IN @it_alv_saida
        WHERE aufnr = @it_alv_saida-aufnr.
    ENDIF.
  ENDIF.

  SORT it_alv_saida BY aufnr.
  SORT it_aufk BY aufnr.


  LOOP AT it_alv_saida INTO wa_sintetico_saida.
    READ TABLE it_aufk INTO DATA(aux_aufk) WITH KEY aufnr = wa_sintetico_saida-aufnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_sintetico_saida-idat1 = aux_aufk-idat1.
      wa_sintetico_saida-idat3 = aux_aufk-idat3.
      wa_sintetico_saida-auart = aux_aufk-auart.
      IF aux_aufk-phas0 = 'X'.
        wa_sintetico_saida-status = 'ABERTO'.
      ELSEIF aux_aufk-phas1 = 'X'.
        wa_sintetico_saida-status = 'LIBERADO'.
      ELSEIF aux_aufk-phas3 = 'X'.
        wa_sintetico_saida-status =  'ENCERRADO'.
      ENDIF.


      APPEND wa_sintetico_saida TO it_sintetico_alv_saida.
      CLEAR wa_sintetico_saida.
    ENDIF.
  ENDLOOP.

* LOOP AT it_alv_saida INTO wa_sintetico_saida.
* APPEND wa_saida TO it_saida.
*CLEAR wa_saida.
*ENDLOOP.

ENDFORM.                    "F_ORGANIZA_DADOS ANALITICO
*=============================================================================*

*=============================================================================*
* Get dados                                                                   *
*=============================================================================*

FORM f_prepare_run_time_info .

  IF <lt_data> IS ASSIGNED.
    CLEAR: <lt_data>[].
  ENDIF.

  IF <lt_data_line> IS ASSIGNED.
    CLEAR: <lt_data_line>[].
  ENDIF.

  IF <ls_data> IS ASSIGNED .
    CLEAR: <ls_data>.
  ENDIF.

  IF <ls_data_line> IS ASSIGNED .
    CLEAR: <ls_data_line>.
  ENDIF.

  FREE: lr_data, lr_data_line, lr_data_descr, lr_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                          metadata = abap_false
                                          data     = abap_true ).

ENDFORM.

" Fim - get Dados
*=============================================================================*

*=============================================================================*
* pREPARA dados                                                                   *
*=============================================================================*
FORM f_get_runtime_info .

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING r_data_descr      = lr_data_descr
                    r_data_line_descr = lr_data_line_descr ).

      CHECK ( lr_data_descr IS NOT INITIAL ) OR ( lr_data_line_descr IS NOT INITIAL ).

      CREATE DATA lr_data      TYPE HANDLE lr_data_descr.
      CREATE DATA lr_data_line TYPE HANDLE lr_data_line_descr.

      ASSIGN lr_data->*      TO <lt_data>.
      ASSIGN lr_data_line->* TO <lt_data_line>.

      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data      = <lt_data>
                                                   t_data_line = <lt_data_line> ).

    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN lr_data->*      TO <ls_data>.
  ASSIGN lr_data_line->* TO <ls_data_line>.

ENDFORM.
" Fim - Prepara Dados
*=============================================================================*


*=============================================================================*
*Form F_Alv                                                                   *
*=============================================================================*
FORM f_alv_analitico.

  PERFORM alv_preenche_cat USING:

        'AUFNR'                  'Ordem         '          '20'  '' 'X'   '',
        'AUART'                  'Tp Ordem      '          '10'  '' ''   '',
        'KTEXT'                  'Texto Breve   '          '30'  '' ''   '',
        'BUKRS'                  'Empresa       '          '12'  '' ''   '',
        'WERKS'                  'Centro        '          '10'  '' ''   '',
        'GSBER'                  'Divisão       '          '10'  '' ''   '',
        'KOSTV'                  'Cen Cst Resp  '          '15'  '' ''   '',
        'CYCLE'                  'Cen Cst Lanç  '          '15'  '' ''   '',
        'POSNR'                  'Solicitação   '          '15'  '' ''   '',
        'TXT50'                  'Descrição     '          '30'  '' ''   '',
        'ERDAT'                  'Data abertura '          '15'  '' ''   '',
        'IDAT2'                  'Data enc tecn '          '15'  '' ''   '',
        'IDAT3'                  'Data enc com  '          '15'  '' ''   ''.
ENDFORM.                    "F_ALV


*=============================================================================*
*Form F_Alv                                                                   *
*=============================================================================*
FORM f_alv_sintetico.

  PERFORM alv_preenche_cat USING:

        'GJAHR'                  'Ano           '          '6'  '' 'X'   '',
        'AUFNR'                  'Ordem         '          '10'  '' ''   '',
        'BUDAT'                  'Data          '          '12'  '' ''   '',
        'WOGBTR'                 'Valor/Mobj    '          '10'  '' ''   '',
        'WKGBTR'                 'Valor/MACC    '          '10'  '' ''   '',
        'BUKRS'                  'Empresa       '          '10'  '' ''   '',
        'IDAT1'                  'Data abert    '          '12'  '' ''   '',
        'IDAT3'                  'Data enc      '          '12'  '' ''   '',
        'AUART'                  'Tipo ordem    '          '10'  '' ''   '',
        'STATUS'                 'Status        '          '12'  '' ''   ''.
ENDFORM.                    "F_ALV

"&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT - ANALITICO
*&---------------------------------------------------------------------*
FORM alv_preenche_cat   USING   p_campo TYPE c
                                p_desc  TYPE c
                                p_tam   TYPE c
                                p_hot   TYPE c
                                p_zero  TYPE c
                                p_sum   TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.
  wl_fcat-tabname     = v_alv.
  wl_fcat-fieldname   = p_campo.
  wl_fcat-scrtext_l   = p_desc.
  wl_fcat-scrtext_m   = p_desc.
  wl_fcat-scrtext_s   = p_desc.
  wl_fcat-hotspot     = p_hot.
  wl_fcat-no_zero     = p_zero.
  wl_fcat-outputlen   = p_tam.


  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT


"&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " PAI_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM container_html .

  DATA : dl_length        TYPE i,                           " Length
         dl_background_id TYPE sdydo_key VALUE space. " Background_id

  IF dg_html_cntrl IS INITIAL.
    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = dg_dyndoc_id
      bottom   = space
    IMPORTING
      length   = dl_length.

  CALL METHOD dg_dyndoc_id->merge_document.

  CALL METHOD dg_dyndoc_id->set_document_background
    EXPORTING
      picture_id = dl_background_id.

  dg_dyndoc_id->html_control = dg_html_cntrl.

  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_html1
    EXCEPTIONS
      html_display_error = 1.

ENDFORM.                    " CONTAINER_HTML

*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       To add Text
*----------------------------------------------------------------------*
FORM add_text USING p_text  TYPE sdydo_text_element
                    p_style TYPE sdydo_attribute
                    p_size  TYPE sdydo_attribute
                    p_color TYPE sdydo_attribute.

* Adding text
  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text          = p_text
      sap_style     = p_style
      sap_fontsize  = p_size
      sap_color     = p_color
      sap_fontstyle = cl_dd_area=>sans_serif.

  "SAP_STYLE    = CL_DD_AREA=>HEADING
  "SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
  "SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
ENDFORM.                    " ADD_TEXT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_objects OUTPUT.

  DATA: url(255) TYPE c.

* Create container and ALV objects only once
  IF ctl_cccontainer IS INITIAL.

    IF sy-batch IS INITIAL.

*   Create object for container
      CREATE OBJECT ctl_cccontainer
        EXPORTING
          container_name              = 'TELA_0100'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


      CREATE OBJECT dg_dyndoc_id
        EXPORTING
          style = 'ALV_GRID'.

      CREATE OBJECT dg_splitter
        EXPORTING
          parent  = ctl_cccontainer
          rows    = 2
          columns = 1.

      CALL METHOD dg_splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = dg_parent_html.

      CREATE OBJECT dg_splitter_2
        EXPORTING
          parent  = dg_parent_html
          rows    = 1
          columns = 2.

      CALL METHOD dg_splitter_2->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = dg_parent_html1.

      CALL METHOD dg_splitter_2->set_column_width
        EXPORTING
          id    = 1
          width = 40.

      CALL METHOD dg_splitter_2->get_container
        EXPORTING
          row       = 1
          column    = 2
        RECEIVING
          container = dg_parent_html2.

      CREATE OBJECT picture
        EXPORTING
          parent = dg_parent_html2.

      PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

      CALL METHOD picture->load_picture_from_url
        EXPORTING
          url = url.

      CALL METHOD picture->set_display_mode
        EXPORTING
          display_mode = picture->display_mode_fit_center.

      CALL METHOD dg_splitter->get_container
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = dg_parent_grid.

      CALL METHOD dg_splitter->set_row_height
        EXPORTING
          id     = 1
          height = 15.


*   Create object for ALV grid inside container
      CREATE OBJECT ctl_alv_resumo
        EXPORTING
          i_parent = dg_parent_grid.

*   Fill info for layout variant
      PERFORM fill_gs_variant.

      "GS_LAYOUT-SEL_MODE = 'A'.
      gs_layout-zebra      = 'X'.
      "GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.

*   Create Object for Event Handler
      CREATE OBJECT event_handler.
*      SET HANDLER EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR CTL_ALV_RESUMO.
*    SET HANDLER EVENT_HANDLER->TOP_OF_PAGE          FOR CTL_ALV_RESUMO.
      CREATE OBJECT wa_alv
        EXPORTING
          i_parent          = dg_parent_grid
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

**   Send data to ALV grid
      PERFORM send_data_to_alv_grid.









      PERFORM cria_html_cab.

      CALL METHOD ctl_alv_resumo->list_processing_events
        EXPORTING
          i_event_name = 'TOP_OF_PAGE'
          i_dyndoc_id  = dg_dyndoc_id.

      CLEAR: gf_first_display.
    ELSE.

      CREATE OBJECT ctl_cccontainer
        EXPORTING
          container_name              = 'TELA_0100'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.

      CREATE OBJECT ctl_alv_resumo
        EXPORTING
          i_parent = ctl_cccontainer.

      PERFORM send_resumo_to_alv_grid.









    ENDIF.

  ENDIF.

  CALL METHOD ctl_alv_resumo->refresh_table_display.

  CALL METHOD ctl_alv_resumo->set_scroll_info_via_id
    EXPORTING
      is_col_info = gs_scroll_col
      is_row_no   = gs_scroll_row.

ENDMODULE.                 " CREATE_OBJECTS  OUTPUT

FORM  send_data_to_alv_grid.

  IF v_alv = 'IT_SAIDA'.

    CALL METHOD ctl_alv_resumo->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        is_variant           = gs_variant
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida.

  ELSEIF v_alv = 'IT_SINTETICO_ALV_SAIDA'.

    CALL METHOD ctl_alv_resumo->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        is_variant           = gs_variant
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_sintetico_alv_saida.
  ENDIF.


ENDFORM.


FORM  send_resumo_to_alv_grid.

  IF v_alv = 'IT_SAIDA'.

    CALL METHOD ctl_alv_resumo->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        is_variant           = gs_variant
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida.

  ELSEIF v_alv = 'IT_SINTETICO_ALV_SAIDA'.

    CALL METHOD ctl_alv_resumo->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        is_variant           = gs_variant
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_sintetico_alv_saida.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_html_cab .

  DATA: column                  TYPE REF TO cl_dd_area,
        column_1                TYPE REF TO cl_dd_area,
        column_2                TYPE REF TO cl_dd_area,
        table_element           TYPE REF TO cl_dd_table_element,
        table_element2          TYPE REF TO cl_dd_table_element,
        p_text                  TYPE sdydo_text_element,
        p_text_table            TYPE sdydo_text_table,
        sdydo_text_element(255),
        vg_mes(2), vg_ano(4),
        qtd                     TYPE i.

  CALL METHOD dg_dyndoc_id->initialize_document.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 1
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element.

  CALL METHOD table_element->add_column
    IMPORTING
      column = column.

  CALL METHOD table_element->set_column_style
    EXPORTING
      col_no    = 1
      sap_align = 'CENTER'
      sap_style = cl_dd_document=>heading.

  p_text = 'Relatório de Ordens Estatísticas'.
  CALL METHOD column->add_text
    EXPORTING
      text      = p_text
      sap_style = 'HEADING'.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element2.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_1.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_2.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 1
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 2
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

*  IF S_KOKRS-LOW IS NOT INITIAL.
*    SDYDO_TEXT_ELEMENT = 'Area de contabilidade:'.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*  ENDIF.
*
*IF S_KOKRS-LOW IS NOT INITIAL.
*  SDYDO_TEXT_ELEMENT = 'Empresa:'.
*  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*endif.
*
*IF S_KOKRS-LOW IS NOT INITIAL.
*  SDYDO_TEXT_ELEMENT = 'Centro:'.
*  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*endif.
*
*IF S_KOKRS-LOW IS NOT INITIAL.
*  SDYDO_TEXT_ELEMENT = 'Tipo de ordem: '.
*  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*endif.
*
*  CALL METHOD COLUMN_1->ADD_TEXT
*    EXPORTING
*      TEXT_TABLE = P_TEXT_TABLE
*      FIX_LINES  = 'X'.
*
*  CLEAR: P_TEXT_TABLE, SDYDO_TEXT_ELEMENT.
*
**  "Area de custo *********
*  IF S_KOKRS-LOW IS NOT INITIAL.
*    SDYDO_TEXT_ELEMENT = S_KOKRS-LOW.
*
*    IF S_KOKRS-HIGH IS NOT INITIAL.
*      CONCATENATE SDYDO_TEXT_ELEMENT '-' S_KOKRS-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
*    ENDIF.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*  ENDIF.
*
*
*  "Empresa*********
*  IF S_BUKRS-LOW IS NOT INITIAL.
*    SDYDO_TEXT_ELEMENT = S_BUKRS-LOW.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*  ENDIF.
*
*  "Centro*********
*  IF S_GSBER-LOW IS NOT INITIAL.
*    SDYDO_TEXT_ELEMENT = S_GSBER-LOW.
*
*    IF S_GSBER-HIGH IS NOT INITIAL.
*      CONCATENATE SDYDO_TEXT_ELEMENT '-' S_GSBER-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
*    ENDIF.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*  ENDIF.
*
*  "Area de contabilidade
*  IF S_KOKRS-LOW IS NOT INITIAL.
*    SDYDO_TEXT_ELEMENT = S_KOKRS-LOW.
*  ENDIF.
*    IF S_HKONT-HIGH IS NOT INITIAL.
*      CONCATENATE SDYDO_TEXT_ELEMENT '-' S_HKONT-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
*    ENDIF.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*  ENDIF.

*  "Data de Lançamento*****
*  IF S_BUDAT-LOW IS NOT INITIAL.
*    CONCATENATE S_BUDAT-LOW+6(2) '/' S_BUDAT-LOW+4(2) '/' S_BUDAT-LOW(4) INTO SDYDO_TEXT_ELEMENT.
*    IF S_BUDAT-HIGH IS NOT INITIAL.
*      CONCATENATE SDYDO_TEXT_ELEMENT '-' S_BUDAT-HIGH+6(2) INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
*      CONCATENATE SDYDO_TEXT_ELEMENT '/' S_BUDAT-HIGH+4(2) '/' S_BUDAT-HIGH(4) INTO SDYDO_TEXT_ELEMENT.
*    ENDIF.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*  ENDIF.


  CALL METHOD column_2->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  PERFORM container_html.

ENDFORM.                    " CRIA_HTML_CAB

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
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

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = space.
  gs_variant-log_group   = space.
  gs_variant-username    = space.
  gs_variant-variant     = space.
  gs_variant-text        = space.
  gs_variant-dependvars  = space.

ENDFORM.                    " FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*&      Form  EXPORTAR_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM export_to_xlsx USING t_export_excel TYPE table.

  DATA: p_local TYPE string.

  CONCATENATE 'C:\' sy-uname '_' sy-datum '_' sy-uzeit '.XLS' INTO p_local.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename   = p_local
      filetype   = 'DAT'
      codepage   = '8404'
    TABLES
      data_tab   = t_export_excel
*     DATA_TAB   = <FS_DATA>
      fieldnames = it_fcat.

ENDFORM.                    " EXPORTAR_ARQUIVO
