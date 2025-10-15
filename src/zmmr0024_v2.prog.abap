
REPORT  zmmr0024_v2 MESSAGE-ID zpmmsg.

*----------------------------------------------------------------------*
* TYPE-POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: vrm.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ausp, cabn, mchb, mara, chvw, zmmt0025, zmmt0027.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF ty_mchb,
    werks TYPE mchb-werks,
    lgort TYPE mchb-lgort,
  END OF ty_mchb,

  BEGIN OF ty_mch1,
    matnr        TYPE mch1-matnr,
    charg        TYPE mch1-charg,
    cuobj_bm     TYPE mch1-cuobj_bm,
    ersda        TYPE mch1-ersda,
    cuobj_bm_aux TYPE ausp-objek,
  END OF ty_mch1,

  BEGIN OF ty_ausp,
    objek     TYPE ausp-objek,
    atinn     TYPE ausp-atinn,
    atwrt     TYPE ausp-atwrt,
    atflv     TYPE ausp-atflv,
    objek_aux TYPE mch1-cuobj_bm,
    charg     TYPE ztsafrafardos-charg,
  END OF ty_ausp,

  BEGIN OF ty_mara,
    matnr TYPE mara-matnr,
    normt TYPE mara-normt,
    matkl TYPE mara-matkl,
    mtart TYPE mara-mtart,
  END OF ty_mara,

  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  BEGIN OF ty_zmmt0025,
    atinn TYPE zmmt0025-atinn,
    atnam TYPE zmmt0025-atnam,
  END OF ty_zmmt0025,

  BEGIN OF ty_ztsafrafardos,
    charg       TYPE ztsafrafardos-charg,
    data_inicio TYPE ztsafrafardos-data_inicio,
    data_fim    TYPE ztsafrafardos-data_fim,
    status      TYPE ztsafrafardos-status,
    werks_key   TYPE ztsafrafardos-werks_key,
    werks_from  TYPE ztsafrafardos-werks_from,
    werks_to    TYPE ztsafrafardos-werks_to,
  END OF ty_ztsafrafardos,

  BEGIN OF ty_zppt0014_aux,
    include   TYPE zmmt0027,
    clint     TYPE zppt0014-clint,
    class     TYPE zppt0014-class,
    atinn     TYPE zppt0014-atinn,
    atnam     TYPE zppt0014-atnam,
    klart     TYPE zppt0014-klart,
    valor_de  TYPE zppt0014-valor_de,
    valor_ate TYPE zppt0014-valor_ate,
  END OF ty_zppt0014_aux,

  BEGIN OF ty_mseg,
    mblnr TYPE mseg-mblnr,
    mjahr TYPE mseg-mjahr,
    zeile TYPE mseg-zeile,
    bwart TYPE mseg-bwart,
    matnr TYPE mseg-matnr,
    werks TYPE mseg-werks,
    lgort TYPE mseg-lgort,
    charg TYPE mseg-charg,
    smbln TYPE mseg-smbln,
    menge TYPE mseg-menge,
    budat TYPE mkpf-budat,
    check TYPE c,
  END OF ty_mseg,

  BEGIN OF ty_saida,
    werks             TYPE mchb-werks,
    znumer            TYPE char10,
    normt             TYPE mara-normt,
    charg1            TYPE zppt0002-cd_sai,
    charg             TYPE mchb-charg,
    clabs             TYPE mchb-cspem,
    lgort             TYPE mchb-lgort,
    matnr             TYPE mara-matnr,
    maktx             TYPE makt-maktx,
    uhml              TYPE ausp-atwrt,
    vlr_min_uhml      TYPE ausp-atwrt,
    vlr_max_uhml      TYPE ausp-atwrt,
    st_uhml(20)       TYPE c,
    ui                TYPE ausp-atwrt,
    vlr_min_ui        TYPE ausp-atwrt,
    vlr_max_ui        TYPE ausp-atwrt,
    st_ui(20)         TYPE c,
    str               TYPE ausp-atwrt,
    vlr_min_str       TYPE ausp-atwrt,
    vlr_max_str       TYPE ausp-atwrt,
    st_str(20)        TYPE c,
    elg               TYPE ausp-atwrt,
    vlr_min_elg       TYPE ausp-atwrt,
    vlr_max_elg       TYPE ausp-atwrt,
    st_elg(20)        TYPE c,
    mic               TYPE ausp-atwrt,
    vlr_min_mic       TYPE ausp-atwrt,
    vlr_max_mic       TYPE ausp-atwrt,
    st_mic(20)        TYPE c,
    rd                TYPE ausp-atwrt,
    vlr_min_rd        TYPE ausp-atwrt,
    vlr_max_rd        TYPE ausp-atwrt,
    st_rd(20)         TYPE c,
    _b                TYPE ausp-atwrt,
    vlr_min__b        TYPE ausp-atwrt,
    vlr_max__b        TYPE ausp-atwrt,
    st__b(20)         TYPE c,
    cg                TYPE ausp-atwrt,
    vlr_min_cg        TYPE ausp-atwrt,
    vlr_max_cg        TYPE ausp-atwrt,
    st_cg(20)         TYPE c,
    t_cnt             TYPE ausp-atwrt,
    vlr_min_t_cnt     TYPE ausp-atwrt,
    vlr_max_t_cnt     TYPE ausp-atwrt,
    st_t_cnt(20)      TYPE c,
    t_area            TYPE ausp-atwrt,
    vlr_min_t_area    TYPE ausp-atwrt,
    vlr_max_t_area    TYPE ausp-atwrt,
    st_t_area(20)     TYPE c,
    leaf              TYPE ausp-atwrt,
    vlr_min_leaf      TYPE ausp-atwrt,
    vlr_max_leaf      TYPE ausp-atwrt,
    st_leaf(20)       TYPE c,
    mr                TYPE ausp-atwrt,
    vlr_min_mr        TYPE ausp-atwrt,
    vlr_max_mr        TYPE ausp-atwrt,
    st_mr(20)         TYPE c,
    sfi               TYPE ausp-atwrt,
    vlr_min_sfi       TYPE ausp-atwrt,
    vlr_max_sfi       TYPE ausp-atwrt,
    st_sfi(20)        TYPE c,
    sci               TYPE ausp-atwrt,
    vlr_min_sci       TYPE ausp-atwrt,
    vlr_max_sci       TYPE ausp-atwrt,
    st_sci(20)        TYPE c,
    csp               TYPE ausp-atwrt,
    vlr_min_csp       TYPE ausp-atwrt,
    vlr_max_csp       TYPE ausp-atwrt,
    st_csp(20)        TYPE c,
    fardao            TYPE ausp-atwrt,
    vlr_min_fardao    TYPE ausp-atwrt,
    vlr_max_fardao    TYPE ausp-atwrt,
    st_fardao(20)     TYPE c,
    talhao            TYPE ausp-atwrt,
    vlr_min_talhao    TYPE ausp-atwrt,
    vlr_max_talhao    TYPE ausp-atwrt,
    st_talhao(20)     TYPE c,
    variedade         TYPE ausp-atwrt,
    vlr_min_variedade TYPE ausp-atwrt,
    vlr_max_variedade TYPE ausp-atwrt,
    st_variedade(20)  TYPE c,
    reserv            TYPE c LENGTH 20,
    ersda             TYPE mchb-ersda,
    safra             TYPE ausp-atwrt,
    vlr_min_safra     TYPE ausp-atwrt,
    vlr_max_safra     TYPE ausp-atwrt,
    st_safra(20)      TYPE c,
    periodo           TYPE ausp-atwrt,
    vlr_min_periodo   TYPE ausp-atwrt,
    vlr_max_periodo   TYPE ausp-atwrt,
    st_periodo(20)    TYPE c,
    status            TYPE c LENGTH 10,
    werks_orig        TYPE mchb-werks,
  END OF ty_saida,

  BEGIN OF ty_saida_resu,
    werks      TYPE mchb-werks,
    normt      TYPE mara-normt,
    matnr      TYPE mara-matnr,
    clabs      TYPE mchb-cspem,
    qtd_f      TYPE sy-tabix,
    werks_orig TYPE mchb-werks,
  END OF ty_saida_resu.

TYPES: BEGIN OF ty_zmme_cl.
         INCLUDE TYPE zmme_cl.
TYPES:   check TYPE c,
       END OF ty_zmme_cl.

TYPES: BEGIN OF ty_caracts.
         INCLUDE TYPE zmmt0027.
TYPES:   atnam TYPE c,
       END OF ty_caracts.

DATA t_class TYPE RANGE OF zmmt0025-atnam WITH HEADER LINE.


*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA: git_fardos_trace    TYPE zpps0007_t,
      it_mchb             TYPE TABLE OF ty_mchb,
      it_mchb_aux         TYPE TABLE OF ty_mchb,
      it_mch1             TYPE TABLE OF ty_mch1,
      it_mch1_aux         TYPE TABLE OF ty_mch1,
      it_mch1_obj         TYPE TABLE OF ty_mch1,
      it_ausp             TYPE TABLE OF ty_ausp,
      it_ausp_class       TYPE TABLE OF ty_ausp,
      it_mara             TYPE TABLE OF ty_mara,
      it_zmmt0025         TYPE TABLE OF ty_zmmt0025,
      it_saida            TYPE TABLE OF ty_saida,
      it_saida_aux        TYPE TABLE OF ty_saida,
      it_saida_resu       TYPE TABLE OF ty_saida_resu,
      it_matnr            TYPE TABLE OF zmme_cl,
      it_return           TYPE TABLE OF ty_zmme_cl,
      it_return_aux       TYPE TABLE OF zmme_cl,
      it_zmmt0027         TYPE TABLE OF zmmt0027,
      it_zmmt0008         TYPE TABLE OF zmmt0008,
      it_zmmt0027_aux     TYPE TABLE OF zmmt0027,
      it_zppt0014         TYPE TABLE OF zppt0014,
      it_zppt0014_aux     TYPE TABLE OF ty_zppt0014_aux,
      it_zppt0002         TYPE TABLE OF zppt0002,
      it_zmmt0025_        TYPE TABLE OF zmmt0025,
      it_mseg             TYPE STANDARD TABLE OF ty_mseg,
      it_v_mkm_zu_kls_bez TYPE TABLE OF v_mkm_zu_kls_bez,
      wa_v_mkm_zu_kls_bez TYPE v_mkm_zu_kls_bez,
      it_caracts          TYPE TABLE OF ty_caracts,
      wa_caracts          TYPE ty_caracts.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_mchb         TYPE ty_mchb,
      wa_mch1         TYPE ty_mch1,
      wa_mch1_aux     TYPE ty_mch1,
      wa_ausp         TYPE ty_ausp,
      wa_ausp_class   TYPE ty_ausp,
      wa_mara         TYPE ty_mara,
      wa_zmmt0025     TYPE ty_zmmt0025,
      wa_saida        TYPE ty_saida,
      wa_saida_aux    TYPE ty_saida,
      wa_saida_resu   TYPE ty_saida_resu,
      wa_return       TYPE zmme_cl,
      wa_matnr        TYPE zmme_cl,
      wa_zmmt0027     TYPE zmmt0027,
      wa_zmmt0027_aux TYPE zmmt0027,
      wa_zppt0014     TYPE zppt0014,
      wa_zppt0014_aux TYPE ty_zppt0014_aux,
      wa_zppt0002     TYPE zppt0002,
      wa_zmmt0008     TYPE zmmt0008,
      wa_mseg         TYPE ty_mseg.
"wa_ztsafrafardos TYPE ztsafrafardos.
"wa_ztfardos      TYPE ty_ztsafrafardos.

*----------------------------------------------------------------------*
* ListBox
*----------------------------------------------------------------------*
DATA: name  TYPE vrm_id,
      lista TYPE vrm_values,
      value LIKE LINE OF lista.
*----------------------------------------------------------------------*
* ALV
*----------------------------------------------------------------------*
DATA: cl_container TYPE REF TO cl_gui_custom_container,
      cl_grid      TYPE REF TO cl_gui_alv_grid,
      ls_layout    TYPE lvc_s_layo,
      ls_fcat      TYPE TABLE OF lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.


*----------------------------------------------------------------------*
* SELECT-PARAMETERS
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK bloco_01 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
              p_werks FOR mchb-werks OBLIGATORY,
              p_lgort FOR mchb-lgort OBLIGATORY,
              p_safra FOR mchb-charg NO INTERVALS MODIF ID c1 NO-EXTENSION,
              p_wkori FOR mchb-werks NO INTERVALS MODIF ID c2.

  PARAMETERS: resu_mm  AS CHECKBOX.

SELECTION-SCREEN: END OF BLOCK bloco_01.


SELECTION-SCREEN: BEGIN OF BLOCK bloco_03 WITH FRAME TITLE TEXT-052.
  SELECTION-SCREEN BEGIN OF LINE.

    SELECTION-SCREEN POSITION 01.
    SELECTION-SCREEN COMMENT 01(10) TEXT-054.
    PARAMETERS: rfd_nemb  TYPE char1  RADIOBUTTON GROUP rb03 MODIF ID mm DEFAULT 'X' USER-COMMAND uc1.

    SELECTION-SCREEN POSITION 05.
    SELECTION-SCREEN COMMENT 17(10) TEXT-053.
    PARAMETERS: rfd_emb   TYPE char1  RADIOBUTTON GROUP rb03 MODIF ID mm.

    SELECTION-SCREEN POSITION 10.
    SELECTION-SCREEN COMMENT 34(07) TEXT-055.
    PARAMETERS: rfd_all   TYPE char1  RADIOBUTTON GROUP rb03 MODIF ID mm.

  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK bloco_03.

SELECTION-SCREEN: BEGIN OF BLOCK bloco_02 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS:
              p_hvi FOR ausp-atwrt NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK bloco_02.


SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-013.
  SELECTION-SCREEN BEGIN OF LINE .

    SELECTION-SCREEN POSITION 01.
    SELECTION-SCREEN COMMENT 01(18) TEXT-006.
    PARAMETERS: p_fpdr TYPE char1 RADIOBUTTON GROUP rb04.

    SELECTION-SCREEN POSITION 05.
    SELECTION-SCREEN COMMENT 30(20) TEXT-008.
    PARAMETERS: p_dpdr TYPE char1 RADIOBUTTON GROUP rb04.

    SELECTION-SCREEN POSITION 25.
    SELECTION-SCREEN COMMENT 60(05) TEXT-056.
    PARAMETERS: p_dpdr_t TYPE char1 RADIOBUTTON GROUP rb04 DEFAULT 'X'.

  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b6.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF p_werks[] IS INITIAL OR p_safra[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Preencher todos os campos obrigatórios' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'C2'.
      IF rfd_emb = abap_true.
        screen-output = 1.
        screen-active = 1.
      ELSE.
        screen-output = 0.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name(11) = 'P_WERKS-LOW' OR
       screen-name(11) = 'P_SAFRA-LOW'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  SELECT atinn atnam
    FROM zmmt0025 INTO TABLE it_zmmt0025.

  IF it_zmmt0025[] IS NOT INITIAL.
    SORT it_zmmt0025 BY atinn.

    LOOP AT it_zmmt0025 INTO wa_zmmt0025 WHERE atnam NE 'SAFRA'.
      value-key  = wa_zmmt0025-atinn.
      value-text = wa_zmmt0025-atnam.
      APPEND value TO lista.
    ENDLOOP.

    name = 'P_CLASS'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = lista.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_hvi-low.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ATNAM'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_HVI'
      value_org   = 'S'
    TABLES
      value_tab   = it_zmmt0025.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF ( p_fpdr IS NOT INITIAL  OR  p_dpdr IS NOT INITIAL ) AND p_hvi IS INITIAL.
    MESSAGE TEXT-051 TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    PERFORM: seleciona_dados.
    CALL SCREEN 0100.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados.

  CLEAR: it_saida[], it_saida_resu[].

  DATA: lit_blocos TYPE zpps0008_001_t.

  SELECT werks lgort
    FROM mchb INTO TABLE it_mchb
   WHERE werks IN p_werks
     AND lgort IN p_lgort.

  CHECK it_mchb[] IS NOT INITIAL.

  SORT it_mchb BY werks lgort.
  DELETE ADJACENT DUPLICATES FROM it_mchb COMPARING werks lgort.

  DATA(lit_mchb_wekrs) = it_mchb[].
  SORT lit_mchb_wekrs BY werks.
  DELETE ADJACENT DUPLICATES FROM lit_mchb_wekrs COMPARING werks.

  LOOP AT lit_mchb_wekrs INTO DATA(lwa_mchb_werks).

    "Consultar Fardinhos Trace Cotton
    CLEAR: lit_blocos[].
    LOOP AT it_mchb INTO DATA(lwa_mchb) WHERE werks = lwa_mchb_werks-werks.
      APPEND VALUE #( bloco = lwa_mchb-lgort ) TO lit_blocos.
    ENDLOOP.

    zcl_trace_cotton_utils=>get_fardos_bloco_trace_cotton(
      EXPORTING
        i_safra                     = CONV #( p_safra-low )
        i_filial_algodoeira         = CONV #( lwa_mchb_werks-werks )
        i_blocos                    = lit_blocos
        i_check_embarque_sap        = abap_true
      IMPORTING
        e_msg_error                 = DATA(_msg_error)
        e_fardos_bloco_trace_cotton = DATA(lit_fardos_trace)
    ).

    IF _msg_error IS NOT INITIAL.
      MESSAGE _msg_error TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    APPEND LINES OF lit_fardos_trace TO git_fardos_trace.
  ENDLOOP.

  IF rfd_emb  EQ abap_true OR  "Fardos Embarcados
     rfd_nemb EQ abap_true.    "Fardos não Embarcados
    CASE rfd_emb. "Embarcado?
      WHEN abap_true.
        DELETE git_fardos_trace WHERE embarcado_sap = abap_false.
      WHEN abap_false.
        DELETE git_fardos_trace WHERE embarcado_sap = abap_true.
    ENDCASE.
  ENDIF.

  CHECK git_fardos_trace[] IS NOT INITIAL.

  SELECT matnr normt matkl mtart
    FROM mara INTO TABLE it_mara
    FOR ALL ENTRIES IN git_fardos_trace
   WHERE normt EQ git_fardos_trace-cd_classificacao(18)
     AND mtart EQ 'ZFER'.

  IF rfd_all EQ abap_true OR
     rfd_emb EQ abap_true.

    SELECT *
      FROM zmmt0008
      INTO TABLE it_zmmt0008
       FOR ALL ENTRIES IN git_fardos_trace
     WHERE safra      = git_fardos_trace-safra
       AND lgort      = git_fardos_trace-bloco(4)
       AND charg      = git_fardos_trace-nr_fardo_completo(10)
       AND werks_orig = git_fardos_trace-id_filial_algodoeira.

    DELETE it_zmmt0008 WHERE werks NOT IN p_wkori[].

    SORT it_zmmt0008 BY safra lgort charg werks_orig.
    DELETE ADJACENT DUPLICATES FROM it_zmmt0008 COMPARING safra lgort charg werks_orig.

    IF p_wkori[] IS NOT INITIAL.
      LOOP AT git_fardos_trace INTO DATA(lwa_fardo_trace).
        DATA(l_tabix) = sy-tabix.
        READ TABLE it_zmmt0008 INTO wa_zmmt0008 WITH KEY safra      = lwa_fardo_trace-safra
                                                         lgort      = lwa_fardo_trace-bloco
                                                         charg      = lwa_fardo_trace-nr_fardo_completo
                                                         werks_orig = lwa_fardo_trace-id_filial_algodoeira
                                              BINARY SEARCH.
        IF sy-subrc <> 0.
          DELETE git_fardos_trace INDEX l_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

  PERFORM: organiza_saida.
ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_SAIDA
*&---------------------------------------------------------------------*
FORM organiza_saida.

  CASE resu_mm.
    WHEN abap_true.
      PERFORM f_organiza_saida_resumo.
    WHEN abap_false.
      PERFORM f_organiza_saida_analitico.
  ENDCASE.

ENDFORM.                    " ORGANIZA_SAIDA


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS 'FT0100'.
  SET TITLEBAR  'TB0100'.

  IF ( cl_container IS INITIAL ).
    PERFORM: create_object_alv.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
FORM create_object_alv.

  CREATE OBJECT cl_container
    EXPORTING
      container_name              = 'CONTAINER_PRINCIPAL'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  CREATE OBJECT cl_grid
    EXPORTING
      i_parent          = cl_container
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  gs_layout-cwidth_opt = abap_true.

  IF ( resu_mm EQ 'X' ).
    PERFORM: catalog_alv_resumo.

    CALL METHOD cl_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = it_saida_resu
        it_fieldcatalog               = ls_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.

    PERFORM: catalog_alv.

    CALL METHOD cl_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = it_saida
        it_fieldcatalog               = ls_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  CLEAR: lista.
ENDFORM.                    " CREATE_OBJECT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE pai INPUT.
  CASE sy-ucomm.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CATALOG_ALV
*&---------------------------------------------------------------------*
FORM catalog_alv.

  IF p_hvi-low IS INITIAL.

    PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '20'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'UHML             '  'UHML                 ' '7 '  ''  ''  '' '',
            'UI               '  'UI                   ' '7 '  ''  ''  '' '',
            'STR              '  'STR                  ' '7 '  ''  ''  '' '',
            'ELG              '  'ELG                  ' '7 '  ''  ''  '' '',
            'MIC              '  'MIC                  ' '7 '  ''  ''  '' '',
            'RD               '  'RD                   ' '7 '  ''  ''  '' '',
            '_B               '  '_+B                  ' '7 '  ''  ''  '' '',
            'CG               '  'CG                   ' '7 '  ''  ''  '' '',
            'T_CNT            '  'T.CNT                ' '7 '  ''  ''  '' '',
            'T_AREA           '  'T.AREA               ' '7 '  ''  ''  '' '',
            'LEAF             '  'LEAF                 ' '7 '  ''  ''  '' '',
            'MR               '  'MR                   ' '7 '  ''  ''  '' '',
            'SFI              '  'SFI(W)               ' '7 '  ''  ''  '' '',
            'SCI              '  'SCI                  ' '7 '  ''  ''  '' '',
            'CSP              '  'CSP                  ' '7 '  ''  ''  '' '',
            'SAFRA            '  'Safra                ' '4 '  ''  ''  '' '',
            'STATUS           '  'Status               ' '10'  ''  ''  '' ''.

  ELSEIF p_hvi-low IS NOT INITIAL AND p_fpdr IS INITIAL AND p_dpdr IS INITIAL.

    PERFORM  alv_preenche_cat USING:
           'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
           'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
           'CHARG            '  'Número               ' '10'  ''  ''  '' '',
           'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
           'MATNR            '  'Material             ' '  '  ''  'X' '' '',
           'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
           'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
           'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
           'UHML             '  'UHML                 ' '7 '  ''  ''  '' '',
           'UI               '  'UI                   ' '7 '  ''  ''  '' '',
           'STR              '  'STR                  ' '7 '  ''  ''  '' '',
           'ELG              '  'ELG                  ' '7 '  ''  ''  '' '',
           'MIC              '  'MIC                  ' '7 '  ''  ''  '' '',
           'RD               '  'RD                   ' '7 '  ''  ''  '' '',
           '_B               '  '_+B                  ' '7 '  ''  ''  '' '',
           'CG               '  'CG                   ' '7 '  ''  ''  '' '',
           'T_CNT            '  'T.CNT                ' '7 '  ''  ''  '' '',
           'T_AREA           '  'T.AREA               ' '7 '  ''  ''  '' '',
           'LEAF             '  'LEAF                 ' '7 '  ''  ''  '' '',
           'MR               '  'MR                   ' '7 '  ''  ''  '' '',
           'SFI              '  'SFI(W)               ' '7 '  ''  ''  '' '',
           'SCI              '  'SCI                  ' '7 '  ''  ''  '' '',
           'CSP              '  'CSP                  ' '7 '  ''  ''  '' '',
           'SAFRA            '  'Safra                ' '4 '  ''  ''  '' '',
           'STATUS            '  'Status              ' '10'  ''  ''  '' ''.

  ELSEIF p_fpdr IS NOT INITIAL AND p_dpdr IS NOT INITIAL.

    PERFORM  alv_preenche_cat USING:

     'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
     'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
     'CHARG            '  'Número               ' '10'  ''  ''  '' '',
     'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
     'MATNR            '  'Material             ' '  '  ''  'X' '' '',
     'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
     'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
     'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
     'UHML             '  'UHML                 ' '7 '  ''  ''  '' '',
     'VLR_MIN_UHML     '  'v_min_UHML           ' '20'  ''  ''  '' '',
     'VLR_MAX_UHML     '  'v_max_UHML           ' '20'  ''  ''  '' '',
     'ST_UHML          '  'ST_UHML              ' '20'  ''  ''  '' 'c410',
     'UI               '  'UI                   ' '7 '  ''  ''  '' '',
     'VLR_MIN_UI       '  'v_min_UI             ' '20'  ''  ''  '' '',
     'VLR_MAX_UI       '  'v_max_UI             ' '20'  ''  ''  '' '',
     'ST_UI            '  'st_UI                ' '20'  ''  ''  '' '',
     'STR              '  'STR                  ' '7 '  ''  ''  '' '',
     'VLR_MIN_STR      '  'v_min_STR            ' '20'  ''  ''  '' '',
     'VLR_MAX_STR      '  'v_max_STR            ' '20'  ''  ''  '' '',
     'ST_STR           '  'st_STR               ' '20'  ''  ''  '' '',
     'ELG              '  'ELG                  ' '7 '  ''  ''  '' '',
     'VLR_MIN_ELG      '  'v_min_ELG            ' '20'  ''  ''  '' '',
     'VLR_MAX_ELG      '  'v_max_ELG            ' '20'  ''  ''  '' '',
     'ST_ELG           '  'st_ELG               ' '20'  ''  ''  '' '',
     'MIC              '  'MIC                  ' '7 '  ''  ''  '' '',
     'VLR_MIN_MIC      '  'v_min_MIC            ' '20'  ''  ''  '' '',
     'VLR_MAX_MIC      '  'v_max_MIC            ' '20'  ''  ''  '' '',
     'ST_MIC           '  'st_MIC               ' '20'  ''  ''  '' '',
     'RD               '  'RD                   ' '7 '  ''  ''  '' '',
     'VLR_MIN_RD       '  'v_min_RD             ' '20'  ''  ''  '' '',
     'VLR_MAX_RD       '  'v_max_RD             ' '20'  ''  ''  '' '',
     'ST_RD            '  'st_RD                ' '20'  ''  ''  '' '',
     '_B               '  '_+b                  ' '7 '  ''  ''  '' '',
     'VLR_MIN__B       '  'v_min__B             ' '20'  ''  ''  '' '',
     'VLR_MAX__B       '  'v_max__B             ' '20'  ''  ''  '' '',
     'ST__B            '  'st__B                ' '20'  ''  ''  '' '',
     'CG               '  'CG                   ' '7 '  ''  ''  '' '',
     'VLR_MIN_CG       '  'v_min_CG             ' '20'  ''  ''  '' '',
     'VLR_MAX_CG       '  'v_max_CG             ' '20'  ''  ''  '' '',
     'ST_CG            '  'st_CG                ' '20'  ''  ''  '' '',
     'T_CNT            '  't.cnt                ' '7 '  ''  ''  '' '',
     'VLR_MIN_T_CNT    '  'v_min_t_cnt          ' '20'  ''  ''  '' '',
     'VLR_MAX_T_CNT    '  'v_max_t_cnt          ' '20'  ''  ''  '' '',
     'ST_T_CNT         '  'st_t_cnt             ' '20'  ''  ''  '' '',
     'T_AREA           '  't.area               ' '7 '  ''  ''  '' '',
     'VLR_MIN_T_AREA   '  'v_min_t_area         ' '20'  ''  ''  '' '',
     'VLR_MAX_T_AREA   '  'v_max_t_area         ' '20'  ''  ''  '' '',
     'ST_T_AREA        '  'st_t_area            ' '20'  ''  ''  '' '',
     'LEAF             '  'leaf                 ' '7 '  ''  ''  '' '',
     'VLR_MIN_LEAF     '  'v_min_leaf           ' '20'  ''  ''  '' '',
     'VLR_MAX_LEAF     '  'v_max_leaf           ' '20'  ''  ''  '' '',
     'ST_LEAF          '  'st_leaf              ' '20'  ''  ''  '' '',
     'MR               '  'mr                   ' '7 '  ''  ''  '' '',
     'VLR_MIN_MR       '  'v_min_mr             ' '20'  ''  ''  '' '',
     'VLR_MAX_MR       '  'v_max_mr             ' '20'  ''  ''  '' '',
     'ST_MR            '  'st_mr                ' '20'  ''  ''  '' '',
     'SFI              '  'sfi(w)               ' '7 '  ''  ''  '' '',
     'VLR_MIN_SFI      '  'v_min_sfi            ' '20'  ''  ''  '' '',
     'VLR_MAX_SFI      '  'v_max_sfi            ' '20'  ''  ''  '' '',
     'ST_SFI           '  'st_sfi               ' '20'  ''  ''  '' '',
     'SCI              '  'sci                  ' '7 '  ''  ''  '' '',
     'VLR_MIN_SCI      '  'v_min_sci            ' '20'  ''  ''  '' '',
     'VLR_MAX_SCI      '  'v_max_sci            ' '20'  ''  ''  '' '',
     'ST_SCI           '  'st_sci               ' '20'  ''  ''  '' '',
     'CSP              '  'csp                  ' '7 '  ''  ''  '' '',
     'VLR_MIN_CSP      '  'v_min_csp            ' '20'  ''  ''  '' '',
     'VLR_MAX_CSP      '  'v_max_csp            ' '20'  ''  ''  '' '',
     'ST_CSP           '  'st_sci               ' '20'  ''  ''  '' '',
     'SAFRA            '  'safra                ' '4 '  ''  ''  '' '',
     'STATUS           '  'status               ' '10'  ''  ''  '' ''.


  ELSEIF p_hvi-low IS NOT INITIAL AND p_fpdr IS NOT INITIAL OR p_dpdr IS NOT INITIAL.

    CASE p_hvi-low.
      WHEN 'UHML'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'UHML             '  'UHML                 ' '7 '  ''  ''  '' '',
            'vlr_min_UHML     '  'v_min_UHML           ' '20'  ''  ''  '' '',
            'VLR_MAX_UHML     '  'v_max_UHML           ' '20'  ''  ''  '' '',
            'ST_UHML          '  'ST_UHML              ' '20'  ''  ''  '' 'c410'.

      WHEN 'UI'.
        PERFORM  alv_preenche_cat USING:
           'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
           'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
           'CHARG            '  'Número               ' '10'  ''  ''  '' '',
           'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
           'MATNR            '  'Material             ' '  '  ''  'X' '' '',
           'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
           'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
           'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
           'UI               '  'UI                   ' '7 '  ''  ''  '' '',
           'vlr_min_UI       '  'v_min_UI             ' '20'  ''  ''  '' '',
           'vlr_max_UI       '  'v_max_UI             ' '20'  ''  ''  '' '',
           'st_UI            '  'st_UI                ' '20'  ''  ''  '' 'c410'.

      WHEN 'STR'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'STR              '  'STR                  ' '7 '  ''  ''  '' '',
            'vlr_min_STR      '  'v_min_STR            ' '20'  ''  ''  '' '',
            'vlr_max_STR      '  'v_max_STR            ' '20'  ''  ''  '' '',
            'st_STR           '  'st_STR               ' '20'  ''  ''  '' 'c410'.

      WHEN 'ELG'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ELG              '  'ELG                  ' '7 '  ''  ''  '' '',
            'vlr_min_ELG      '  'v_min_ELG            ' '20'  ''  ''  '' '',
            'vlr_max_ELG      '  'v_max_ELG            ' '20'  ''  ''  '' '',
            'st_ELG           '  'st_ELG               ' '20'  ''  ''  '' 'c410'.

      WHEN 'MIC'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'MIC              '  'MIC                  ' '7 '  ''  ''  '' '',
            'vlr_min_MIC      '  'v_min_MIC            ' '20'  ''  ''  '' '',
            'vlr_max_MIC      '  'v_max_MIC            ' '20'  ''  ''  '' '',
            'st_MIC           '  'st_MIC               ' '20'  ''  ''  '' 'c410'.

      WHEN 'RD'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'RD               '  'RD                   ' '7 '  ''  ''  '' '',
            'vlr_min_RD       '  'v_min_RD             ' '20'  ''  ''  '' '',
            'vlr_max_RD       '  'v_max_RD             ' '20'  ''  ''  '' '',
            'st_RD            '  'st_RD                ' '20'  ''  ''  '' 'c410'.

      WHEN '_B'.
        PERFORM  alv_preenche_cat USING:
           'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
           'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
           'CHARG             '  'Número               ' '10'  ''  ''  '' '',
           'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
           'MATNR             '  'Material             ' '  '  ''  'X' '' '',
           'NORMT             '  'Tipo                 ' '10'  ''  ''  '' '',
           'CLABS             '  'Peso                 ' '10'  ''  ''  '' '',
           'LGORT             '  'Bloco                ' '10'  ''  ''  '' '',
            '_B               '  '_+b                  ' '7 '  ''  ''  '' '',
            'vlr_min__B       '  'v_min__B             ' '20'  ''  ''  '' '',
            'vlr_max__B       '  'v_max__B             ' '20'  ''  ''  '' '',
            'st__B            '  'st__B                ' '20'  ''  ''  '' 'c410'.

      WHEN 'CG'.
        PERFORM  alv_preenche_cat USING:
           'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
           'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
           'CHARG            '  'Número               ' '10'  ''  ''  '' '',
           'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
           'MATNR            '  'Material             ' '  '  ''  'X' '' '',
           'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
           'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
           'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'CG               '  'CG                   ' '7 '  ''  ''  '' '',
            'VLR_MIN_CG       '  'V_MIN_CG             ' '20'  ''  ''  '' '',
            'VLR_MAX_CG       '  'V_MAX_CG             ' '20'  ''  ''  '' '',
            'ST_CG            '  'ST_CG                ' '20'  ''  ''  '' 'C410'.

      WHEN 'T_CNT'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'T_CNT            '  'T.CNT                ' '7 '  ''  ''  '' '',
            'VLR_MIN_T_CNT    '  'V_MIN_T_CNT          ' '20'  ''  ''  '' '',
            'VLR_MAX_T_CNT    '  'V_MAX_T_CNT          ' '20'  ''  ''  '' '',
            'ST_T_CNT         '  'ST_T_CNT             ' '20'  ''  ''  '' 'C410'.

      WHEN 'T_AREA'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'T_AREA           '  'T.AREA               ' '7 '  ''  ''  '' '',
            'VLR_MIN_T_AREA   '  'V_MIN_T_AREA         ' '20'  ''  ''  '' '',
            'VLR_MAX_T_AREA   '  'V_MAX_T_AREA         ' '20'  ''  ''  '' '',
            'ST_T_AREA        '  'ST_T_AREA            ' '20'  ''  ''  '' 'C410'.

      WHEN 'LEAF'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'LEAF             '  'LEAF                 ' '7 '  ''  ''  '' '',
            'VLR_MIN_LEAF     '  'V_MIN_LEAF           ' '20'  ''  ''  '' '',
            'VLR_MAX_LEAF     '  'V_MAX_LEAF           ' '20'  ''  ''  '' '',
            'ST_LEAF          '  'ST_LEAF              ' '20'  ''  ''  '' 'C410'.

      WHEN 'MR'.
        PERFORM  alv_preenche_cat USING:
           'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
           'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
           'CHARG            '  'Número               ' '10'  ''  ''  '' '',
           'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
           'MATNR            '  'Material             ' '  '  ''  'X' '' '',
           'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
           'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
           'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
           'MR               '  'MR                   ' '7 '  ''  ''  '' '',
           'VLR_MIN_MR       '  'V_MIN_MR             ' '20'  ''  ''  '' '',
           'VLR_MAX_MR       '  'V_MAX_MR             ' '20'  ''  ''  '' '',
           'ST_MR            '  'ST_MR                ' '20'  ''  ''  '' 'C410'.

      WHEN 'SFI'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'SFI              '  'SFI(W)               ' '7 '  ''  ''  '' '',
            'VLR_MIN_SFI      '  'V_MIN_SFI            ' '20'  ''  ''  '' '',
            'VLR_MAX_SFI      '  'V_MAX_SFI            ' '20'  ''  ''  '' '',
            'ST_SFI           '  'ST_SFI               ' '20'  ''  ''  '' 'C410'.

      WHEN 'SCI'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'SCI              '  'SCI                  ' '7 '  ''  ''  '' '',
            'VLR_MIN_SCI      '  'V_MIN_SCI            ' '20'  ''  ''  '' '',
            'VLR_MAX_SCI      '  'V_MAX_SCI            ' '20'  ''  ''  '' '',
            'ST_SCI           '  'ST_SCI               ' '20'  ''  ''  '' 'C410'.

      WHEN 'CSP'.
        PERFORM  alv_preenche_cat USING:
            'WERKS            '  'Centro               ' '  '  ''  ''  '' '',
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'CSP              '  'CSP                  ' '7 '  ''  ''  '' '',
            'VLR_MIN_CSP      '  'V_MIN_CSP            ' '20'  ''  ''  '' '',
            'VLR_MAX_CSP      '  'V_MAX_CSP            ' '20'  ''  ''  '' '',
            'ST_CSP           '  'ST_SCI               ' '20'  ''  ''  '' 'C410'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " CATALOG_ALV

*&---------------------------------------------------------------------*
*&      Form  CATALOG_ALV_RESUMO
*&---------------------------------------------------------------------*
FORM catalog_alv_resumo.
  PERFORM  alv_preenche_cat USING:
           'WERKS'    'Centro'     ' '  '' ''  '' '',
           'NORMT'    'Tipo'       '10' '' ''   '' '',
           'MATNR'    'Material'   ' '  '' 'X'  '' '',
           'CLABS'    'Peso'       '25' '' ''   '' '',
           'QTD_F'    'Qtd. Fardo' '10' '' ''   '' ''.
ENDFORM.                    " CATALOG_ALV_RESUMO

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat  USING    p_campo   TYPE c
                                p_desc    TYPE c
                                p_tam     TYPE c
                                p_hot     TYPE c
                                p_zero    TYPE c
                                p_convert TYPE c
                                p_emphasize TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.

  IF ( resu_mm EQ 'X' ).
    wl_fcat-tabname   = 'IT_SAIDA_RESU'.
  ELSE.
    wl_fcat-tabname   = 'IT_SAIDA'.
  ENDIF.

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-convexit  = p_convert.
  wl_fcat-emphasize = p_emphasize.

  gs_layout-excp_conds    = 'X'.
  gs_layout-zebra         = 'X'.
  gs_layout-sel_mode      = 'A'.
  gs_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout-totals_bef    = ''.

  APPEND wl_fcat TO ls_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT


FORM f_organiza_saida_resumo.

  SORT: it_mara  BY normt.

  DATA(lit_fardos_trace) = git_fardos_trace[].

  LOOP AT git_fardos_trace INTO DATA(lwa_fardo_trace).

    CLEAR: wa_saida_resu.

    wa_saida_resu-werks = lwa_fardo_trace-id_filial_algodoeira.
    wa_saida_resu-normt = lwa_fardo_trace-cd_classificacao.

    READ TABLE it_mara INTO wa_mara WITH KEY normt = wa_saida_resu-normt BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida_resu-matnr = wa_mara-matnr.
    ENDIF.

    LOOP AT lit_fardos_trace INTO DATA(lwa_fardo_trace_aux) WHERE id_filial_algodoeira = lwa_fardo_trace-id_filial_algodoeira
                                                              AND cd_classificacao     = lwa_fardo_trace-cd_classificacao.
      ADD lwa_fardo_trace_aux-peso_liquido TO wa_saida_resu-clabs.
      ADD 1 TO wa_saida_resu-qtd_f.
    ENDLOOP.

    APPEND wa_saida_resu TO it_saida_resu.

    DELETE git_fardos_trace WHERE id_filial_algodoeira = lwa_fardo_trace-id_filial_algodoeira
                              AND cd_classificacao     = lwa_fardo_trace-cd_classificacao.
  ENDLOOP.


ENDFORM.

FORM f_organiza_saida_analitico.

  SORT: it_mara     BY normt,
        it_zmmt0025 BY atnam,
        it_zppt0014 BY atinn.

  LOOP AT git_fardos_trace INTO DATA(lwa_fardo_trace).

    CLEAR: wa_saida.

    SEARCH lwa_fardo_trace-nr_fardo_completo FOR './.'.
    IF sy-fdpos > 0.
      wa_saida-znumer = |{ lwa_fardo_trace-nr_fardo_completo(3) }|.
    ENDIF.

    wa_saida-charg      = lwa_fardo_trace-nr_maquina && lwa_fardo_trace-nr_fardo.
    wa_saida-lgort      = lwa_fardo_trace-bloco.
    wa_saida-werks      = lwa_fardo_trace-id_filial_algodoeira.
    wa_saida-normt      = lwa_fardo_trace-cd_classificacao.
    wa_saida-clabs      = lwa_fardo_trace-peso_liquido.

    READ TABLE it_mara INTO wa_mara WITH KEY normt = wa_saida-normt BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-matnr = wa_mara-matnr.
    ENDIF.

    IF lwa_fardo_trace-embarcado_sap EQ abap_true.
      wa_saida-status   = 'EMBARCADO'.
    ELSE.
      wa_saida-status   = 'DISPONÍVEL'.
    ENDIF.

    wa_saida-charg1    = lwa_fardo_trace-cd_sai.

    "UHML
    wa_saida-uhml = lwa_fardo_trace-uhml.
    PERFORM f_set_caracteristica USING 'UHML' wa_saida-uhml CHANGING  wa_saida-vlr_min_uhml wa_saida-vlr_min_uhml wa_saida-st_uhml.

    "UI
    wa_saida-ui = lwa_fardo_trace-ui.
    PERFORM f_set_caracteristica USING 'UI' wa_saida-ui CHANGING  wa_saida-vlr_min_ui wa_saida-vlr_min_ui wa_saida-st_ui.

    "STR
    wa_saida-str = lwa_fardo_trace-str.
    PERFORM f_set_caracteristica USING 'STR' wa_saida-str CHANGING  wa_saida-vlr_min_str wa_saida-vlr_min_str wa_saida-st_str.

    "ELG
    wa_saida-elg = lwa_fardo_trace-elg.
    PERFORM f_set_caracteristica USING 'ELG' wa_saida-elg CHANGING  wa_saida-vlr_min_elg wa_saida-vlr_min_elg wa_saida-st_elg.

    "MIC
    wa_saida-mic = lwa_fardo_trace-mic.
    PERFORM f_set_caracteristica USING 'MIC' wa_saida-mic CHANGING  wa_saida-vlr_min_mic wa_saida-vlr_min_mic wa_saida-st_mic.

    "RD
    wa_saida-rd = lwa_fardo_trace-rd.
    PERFORM f_set_caracteristica USING 'RD' wa_saida-rd CHANGING  wa_saida-vlr_min_rd wa_saida-vlr_min_rd wa_saida-st_rd.

    "+B
    wa_saida-_b = lwa_fardo_trace-_b.
    PERFORM f_set_caracteristica USING '+B' wa_saida-_b CHANGING  wa_saida-vlr_min__b wa_saida-vlr_min__b wa_saida-st__b.

    "CG
    wa_saida-cg = lwa_fardo_trace-cg.
    PERFORM f_set_caracteristica USING 'CG' wa_saida-cg CHANGING  wa_saida-vlr_min_cg wa_saida-vlr_min_cg wa_saida-st_cg.

    "T.CNT
    wa_saida-t_cnt = lwa_fardo_trace-count.
    PERFORM f_set_caracteristica USING 'T.CNT' wa_saida-t_cnt CHANGING  wa_saida-vlr_min_t_cnt wa_saida-vlr_min_t_cnt wa_saida-st_t_cnt.

    "T.AREA
    wa_saida-t_area = lwa_fardo_trace-area.
    PERFORM f_set_caracteristica USING 'T.AREA' wa_saida-t_area CHANGING  wa_saida-vlr_min_t_area wa_saida-vlr_min_t_area wa_saida-st_t_area.

    "LEAF
    wa_saida-leaf = lwa_fardo_trace-leaf.
    PERFORM f_set_caracteristica USING 'LEAF' wa_saida-leaf CHANGING  wa_saida-vlr_min_leaf wa_saida-vlr_min_leaf wa_saida-st_leaf.

    "MR
    wa_saida-mr = lwa_fardo_trace-mr.
    PERFORM f_set_caracteristica USING 'MR' wa_saida-mr CHANGING  wa_saida-vlr_min_mr wa_saida-vlr_min_mr wa_saida-st_mr.

    "SFI(W)
    wa_saida-sfi = lwa_fardo_trace-sfi.
    PERFORM f_set_caracteristica USING 'SFI(W)' wa_saida-sfi CHANGING  wa_saida-vlr_min_sfi wa_saida-vlr_min_sfi wa_saida-st_sfi.

    "SCI
    wa_saida-sci = lwa_fardo_trace-sci.
    PERFORM f_set_caracteristica USING 'SCI' wa_saida-sci CHANGING  wa_saida-vlr_min_sci wa_saida-vlr_min_sci wa_saida-st_sci.

    "CSP
    wa_saida-csp = lwa_fardo_trace-csp.
    PERFORM f_set_caracteristica USING 'CSP' wa_saida-csp CHANGING  wa_saida-vlr_min_csp wa_saida-vlr_min_csp wa_saida-st_csp.

    wa_saida-safra     = lwa_fardo_trace-safra.

    APPEND wa_saida TO it_saida.
  ENDLOOP.

  PERFORM f_aplica_filtro_fora_padrao.
  PERFORM f_aplica_filtro_dentro_padrao.

ENDFORM.


FORM f_set_caracteristica  USING  p_caracteristica
                                  p_valor
                         CHANGING p_valor_minino
                                  p_valor_maximo
                                  p_resultado.

  READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = p_caracteristica BINARY SEARCH.
  CHECK sy-subrc EQ 0.

  READ TABLE it_zppt0014 INTO wa_zppt0014 WITH KEY atinn = wa_zmmt0025-atinn BINARY SEARCH.
  CHECK sy-subrc EQ 0.

  p_valor_minino = wa_zppt0014-valor_de.
  p_valor_maximo = wa_zppt0014-valor_de.

  CHECK wa_zppt0014-valor_de IS NOT INITIAL.

  IF p_valor < wa_zppt0014-valor_de OR p_valor > wa_zppt0014-valor_ate.
    p_resultado = 'F.P'.
  ELSE.
    p_resultado = 'D.P'.
  ENDIF.

ENDFORM.

FORM f_aplica_filtro_fora_padrao .

  CHECK p_fpdr IS NOT INITIAL.

  CASE p_hvi-low.
    WHEN 'UHML'.
      DELETE it_saida WHERE st_uhml       NE 'F.P'
                        AND st_uhml       NE abap_off.
    WHEN 'UI'.
      DELETE it_saida WHERE st_ui         NE 'F.P'
                        AND st_ui         NE abap_off.
    WHEN 'STR'.
      DELETE it_saida WHERE st_str        NE 'F.P'
                        AND st_str        NE abap_off.
    WHEN 'ELG'.
      DELETE it_saida WHERE st_elg        NE 'F.P'
                        AND st_elg        NE abap_off.
    WHEN 'MIC'.
      DELETE it_saida WHERE st_mic        NE 'F.P'
                        AND st_mic        NE abap_off.
    WHEN 'RD'.
      DELETE it_saida WHERE st_rd         NE 'F.P'
                        AND st_rd         NE abap_off.
    WHEN '_B'.
      DELETE it_saida WHERE st__b         NE 'F.P'
                        AND st__b         NE abap_off.
    WHEN 'CG'.
      DELETE it_saida WHERE st_cg         NE 'F.P'
                        AND st_cg         NE abap_off.
    WHEN 'CNT'.
      DELETE it_saida WHERE st_t_cnt      NE 'F.P'
                        AND st_t_cnt      NE abap_off.
    WHEN 'T_AREA'.
      DELETE it_saida WHERE st_t_area     NE 'F.P'
                        AND st_t_area     NE abap_off.
    WHEN 'LEAF'.
      DELETE it_saida WHERE st_leaf       NE 'F.P'
                        AND st_leaf       NE abap_off.
    WHEN  'MR'.
      DELETE it_saida WHERE st_mr         NE 'F.P'
                        AND st_mr         NE abap_off.
    WHEN  'SFI'.
      DELETE it_saida WHERE st_sfi        NE 'F.P'
                        AND st_sfi        NE abap_off.
    WHEN  'SCI'.
      DELETE it_saida WHERE st_sci        NE 'F.P'
                        AND st_sci        NE abap_off.
    WHEN  'CSP'.
      DELETE it_saida WHERE st_csp        NE 'F.P'
                        AND st_csp        NE abap_off.
    WHEN  'FARDAO'.
      DELETE it_saida WHERE st_fardao     NE 'F.P'
                        AND st_fardao     NE abap_off.
    WHEN  'TALHAO'.
      DELETE it_saida WHERE st_talhao     NE 'F.P'
                        AND st_talhao     NE abap_off.
    WHEN  'VARIEDADE'.
      DELETE it_saida WHERE st_variedade  NE 'F.P'
                        AND st_variedade  NE abap_off.
    WHEN  'SAFRA'.
      DELETE it_saida WHERE st_safra      NE 'F.P'
                        AND st_safra      NE abap_off.
    WHEN  'PERIODO'.
      DELETE it_saida WHERE st_periodo    NE 'F.P'
                        AND st_uhml       NE abap_off.
  ENDCASE.

ENDFORM.


FORM f_aplica_filtro_dentro_padrao.

  CHECK p_dpdr IS NOT INITIAL.

  CASE p_hvi-low.
    WHEN 'UHML'.
      DELETE it_saida WHERE st_uhml       NE 'D.P'
                        AND st_uhml       NE abap_off.
    WHEN 'UI'.
      DELETE it_saida WHERE st_ui         NE 'D.P'
                        AND st_ui         NE abap_off.
    WHEN 'STR'.
      DELETE it_saida WHERE st_str        NE 'D.P'
                        AND st_str        NE abap_off.
    WHEN 'ELG'.
      DELETE it_saida WHERE st_elg        NE 'D.P'
                        AND st_elg        NE abap_off.
    WHEN 'MIC'.
      DELETE it_saida WHERE st_mic        NE 'D.P'
                        AND st_mic        NE abap_off.
    WHEN 'RD'.
      DELETE it_saida WHERE st_rd         NE 'D.P'
                        AND st_rd         NE abap_off.
    WHEN '_B'.
      DELETE it_saida WHERE st__b         NE 'D.P'
                        AND st__b         NE abap_off.
    WHEN 'CG'.
      DELETE it_saida WHERE st_cg         NE 'D.P'
                        AND st_cg         NE abap_off.
    WHEN 'CNT'.
      DELETE it_saida WHERE st_t_cnt      NE 'D.P'
                        AND st_t_cnt      NE abap_off.
    WHEN 'T_AREA'.
      DELETE it_saida WHERE st_t_area     NE 'D.P'
                        AND st_t_area     NE abap_off.
    WHEN 'LEAF'.
      DELETE it_saida WHERE st_leaf       NE 'D.P'
                        AND st_leaf       NE abap_off.
    WHEN  'MR'.
      DELETE it_saida WHERE st_mr         NE 'D.P'
                        AND st_mr         NE abap_off.
    WHEN  'SFI'.
      DELETE it_saida WHERE st_sfi        NE 'D.P'
                        AND st_sfi        NE abap_off.
    WHEN  'SCI'.
      DELETE it_saida WHERE st_sci        NE 'D.P'
                        AND st_sci        NE abap_off.
    WHEN  'CSP'.
      DELETE it_saida WHERE st_csp        NE 'D.P'
                        AND st_csp        NE abap_off.
    WHEN  'FARDAO'.
      DELETE it_saida WHERE st_fardao     NE 'D.P'
                        AND st_fardao     NE abap_off.
    WHEN  'TALHAO'.
      DELETE it_saida WHERE st_talhao     NE 'D.P'
                        AND st_talhao     NE abap_off.
    WHEN  'VARIEDADE'.
      DELETE it_saida WHERE st_variedade  NE 'D.P'
                        AND st_variedade  NE abap_off.
    WHEN  'SAFRA'.
      DELETE it_saida WHERE st_safra      NE 'D.P'
                        AND st_safra      NE abap_off.
    WHEN  'PERIODO'.
      DELETE it_saida WHERE st_periodo    NE 'D.P'
                        AND st_periodo    NE abap_off.
  ENDCASE.

ENDFORM.
