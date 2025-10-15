*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZMMI0002                                                *
* Descrição  : Relação de Fardos por Bloco                             *
* Módulo     : MM                                Transação: ZMM0026    *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 01/07/2011 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      : Igor Sobral                            Data: 20/06/2013 *
* Observações: Busca de Safra                                          *
*----------------------------------------------------------------------*
REPORT zmmr0002 NO STANDARD PAGE HEADING MESSAGE-ID sd.

FIELD-SYMBOLS: <f_campo>  TYPE any.

TABLES: mseg, mkpf, coas.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_mkpf,
         mblnr TYPE mkpf-mblnr,
         mjahr TYPE mkpf-mjahr,
         budat TYPE mkpf-budat,
       END OF type_mkpf,

       BEGIN OF type_mkpf_aux,
         mblnr TYPE mkpf-mblnr,
         mjahr TYPE mkpf-mjahr,
*         budat    TYPE mkpf-budat,
       END OF type_mkpf_aux,

       BEGIN OF type_mseg,
         mblnr TYPE mseg-mblnr,
         mjahr TYPE mseg-mjahr,
         zeile TYPE mseg-zeile,
         bwart TYPE mseg-bwart,
         matnr TYPE mseg-matnr,
         werks TYPE mseg-werks,
         lgort TYPE mseg-lgort,
         charg TYPE mseg-charg,
         menge TYPE mseg-menge,
         aufnr TYPE mseg-aufnr,
         bukrs TYPE mseg-bukrs,
         smbln TYPE mseg-smbln,
         shkzg TYPE mseg-shkzg,
       END OF type_mseg,

       BEGIN OF type_aufk,
         aufnr TYPE aufk-aufnr,
         aufex TYPE aufk-aufex,
       END OF type_aufk,

       BEGIN OF type_rmclm,
         klart TYPE tcla-klart,
         cuobj TYPE inob-cuobj,
         clint TYPE kssk-clint,
         class TYPE klah-class,
         vondt TYPE klah-vondt,
         bisdt TYPE klah-bisdt,
       END OF type_rmclm,

       BEGIN OF type_inob,
         cuobj  TYPE inob-cuobj,
         klart  TYPE inob-klart,
         obtab  TYPE inob-obtab,
         objek  TYPE inob-objek,
         objekk TYPE kssk-objek,
       END OF type_inob,

       BEGIN OF type_kssk,
         objek TYPE kssk-objek,
         mafid TYPE kssk-mafid,
         klart TYPE kssk-klart,
         clint TYPE kssk-clint,
         adzhl TYPE kssk-adzhl,
       END OF type_kssk,

       BEGIN OF type_klah,
         clint TYPE klah-clint,
         klart TYPE klah-klart,
         class TYPE klah-class,
         vondt TYPE klah-vondt,
         bisdt TYPE klah-bisdt,
       END OF type_klah,

       BEGIN OF type_mchb,
         matnr             TYPE mchb-matnr,
         werks             TYPE mchb-werks,
         lgort             TYPE mchb-lgort,
         charg             TYPE mchb-charg,
         clabs             TYPE mchb-clabs,
         cspem             TYPE mchb-cspem,
         objek             TYPE inob-objek,
         safra             TYPE numc4,
         nr_fardo_completo TYPE mchb-charg,
         nr_maquina        TYPE aufk-aufex,
       END OF type_mchb,

       BEGIN OF type_t001l,
         werks TYPE t001l-werks,
         lgort TYPE t001l-lgort,
         lgobe TYPE t001l-lgobe,
       END OF type_t001l,

       BEGIN OF type_msg,
         material TYPE mseg-matnr,
         centro   TYPE mseg-werks,
         deposito TYPE mseg-lgort,
         lote     TYPE mseg-charg,
         msg      TYPE char100,
       END OF type_msg,

       BEGIN OF type_mara,
         matnr TYPE mara-matnr,
         normt TYPE mara-normt,
       END OF type_mara,

       BEGIN OF type_chvw,
         werks TYPE chvw-werks,
         matnr TYPE chvw-matnr,
         charg TYPE chvw-charg,
         aufnr TYPE chvw-aufnr,
       END OF type_chvw,

       BEGIN OF type_coas,
         aufnr TYPE coas-aufnr,
         aufex TYPE coas-aufex,
       END OF type_coas,

       BEGIN OF type_alv,
         lgort     TYPE mchb-lgort,  "*-CS2023000189-17.04.2023-#108708-JT
         fardo01   TYPE mseg-charg,
         tipo01    TYPE mara-normt,
         peso01    TYPE mseg-menge,
         maquina01 TYPE aufk-aufex,
         fardo02   TYPE mseg-charg,
         tipo02    TYPE mara-normt,
         peso02    TYPE mseg-menge,
         maquina02 TYPE aufk-aufex,
         fardo03   TYPE mseg-charg,
         tipo03    TYPE mara-normt,
         peso03    TYPE mseg-menge,
         maquina03 TYPE aufk-aufex,
         fardo04   TYPE mseg-charg,
         tipo04    TYPE mara-normt,
         peso04    TYPE mseg-menge,
         maquina04 TYPE aufk-aufex,
       END OF type_alv,

       BEGIN OF type_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END OF type_t001w,

       BEGIN OF ty_det_sai,
         fardo TYPE mseg-charg,
         peso  TYPE mseg-menge,
       END   OF ty_det_sai,

       BEGIN OF type_cabec,
         lgort     TYPE mchb-lgort,  "*-CS2023000189-17.04.2023-#108708-JT
         variedade TYPE char20,
         fardos    TYPE numc4,
         peso      TYPE char20,
         cliente   TYPE char100,
         acts      TYPE char10,
       END OF type_cabec.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_mkpf           TYPE TABLE OF type_mkpf,
      t_mseg           TYPE TABLE OF type_mseg,
      t_312            TYPE TABLE OF type_mseg,
      t_mchb           TYPE TABLE OF type_mchb,
      t_mchb_ger       TYPE TABLE OF type_mchb,
      t_mchb_aux       TYPE TABLE OF type_mchb,
      t_inob           TYPE TABLE OF type_inob,
      t_kssk           TYPE TABLE OF type_kssk,
      t_klah           TYPE TABLE OF type_klah,
      t_msg            TYPE TABLE OF type_msg,
      t_alv            TYPE TABLE OF type_alv,
      t_alv_ger        TYPE TABLE OF type_alv,
      t_fcat           TYPE TABLE OF lvc_s_fcat,
      t_mara           TYPE TABLE OF type_mara,
      t_aufk           TYPE TABLE OF type_aufk,
      t_msegpp         TYPE TABLE OF type_mseg,
      t_mkpfpp         TYPE TABLE OF type_mkpf,
      t_chvw           TYPE TABLE OF type_chvw,
      t_coas           TYPE TABLE OF type_coas,
      t_tool           TYPE ui_functions,
      w_mchb_ger       TYPE type_mchb,
      w_cabec          TYPE zmmt0150_out,
      t_dados          TYPE TABLE OF zmmt0151_out,
      w_dados          TYPE zmmt0151_out,
      t_det_sai        TYPE TABLE OF ty_det_sai,
      w_det_sai        TYPE ty_det_sai,
      w_alv_ger        TYPE type_alv,
      st_cabec         TYPE type_cabec,
      t_cabec_ger      TYPE TABLE OF type_cabec,
      w_cabec_ger      TYPE type_cabec,
      st_rmclm         TYPE type_rmclm,
      st_t001l         TYPE type_t001l,
      st_t001w         TYPE type_t001w,
      st_chvw          TYPE type_chvw,
      st_aufk          TYPE type_aufk,
      st_coas          TYPE type_coas,
      sl_mchb_aux      TYPE type_mchb,
      git_fardos_trace TYPE zpps0007_t,
      t_mkpf_aux       TYPE TABLE OF type_mkpf_aux WITH HEADER LINE.

*-CS2023000189-17.04.2023-#108708-JT-inicio
TYPES: BEGIN OF ty_dados_ger,
         w_cabec TYPE zmmt0150_out,
         t_dados LIKE t_dados,
       END OF ty_dados_ger.
DATA: t_dados_ger   TYPE TABLE OF ty_dados_ger,
      w_dados_ger   TYPE ty_dados_ger,
      wa_stable     TYPE lvc_s_stbl VALUE 'XX',
      l_error       TYPE char1,
      l_lotes       TYPE string,
      l_pdf_xtring  TYPE xstring,
      l_index_frame TYPE i,
      l_total_frame TYPE i,
      t_pdf_files   TYPE zsdt_pdf_files,
      w_pdf_files   TYPE zsde_pdf_files.
*-CS2023000189-17.04.2023-#108708-JT-fim

CLASS: lcl_alv_toolbar   DEFINITION DEFERRED.
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar.

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_311   TYPE mseg-bwart VALUE '311',
           c_312   TYPE mseg-bwart VALUE '312',
           c_131   TYPE mseg-bwart VALUE '131',
           c_ws    TYPE mkpf-vgart VALUE 'WS',
           c_x     TYPE char1      VALUE 'X',
           c_c     TYPE char1      VALUE 'C',
           c_q     TYPE char1      VALUE 'Q',
           c_s     TYPE char1      VALUE 'S',
           c_table TYPE char5      VALUE 'T_ALV',
           c_form  TYPE rs38l_fnam VALUE 'ZMMR023'.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_cont   TYPE REF TO cl_gui_custom_container,
      s_alv    TYPE REF TO cl_gui_alv_grid,
      s_layout TYPE lvc_s_layo,
      l_lin    TYPE numc2,
      l_tabix  TYPE sy-tabix,
      l_col    TYPE numc2,
      l_campox TYPE char40,
      l_campo1 TYPE char40,
      l_campo2 TYPE char40,
      l_campo3 TYPE char40.

DATA: p_table_f TYPE bapi1003_key-objecttable,
      p_num_f   TYPE bapi1003_key-classnum,
      p_type_f  TYPE bapi1003_key-classtype.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:  s_werks  FOR mseg-werks NO INTERVALS NO-EXTENSION OBLIGATORY.

  SELECT-OPTIONS:  s_lgort  FOR mseg-lgort OBLIGATORY. " NO INTERVALS NO-EXTENSION OBLIGATORY,  *-CS2023000189-17.04.2023-#108708-JT
  SELECT-OPTIONS:  s_matnr  FOR mseg-matnr NO INTERVALS NO-EXTENSION.
  "s_budat  for mkpf-budat obligatory,
  SELECT-OPTIONS:  s_aufex  FOR coas-aufex NO INTERVALS NO-EXTENSION.
  PARAMETERS:      p_safra  TYPE zzsafra   OBLIGATORY.

*-CS2023000189-17.04.2023-#108708-JT-inicio
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (31) TEXT-110 FOR FIELD p_kunnr.
    PARAMETERS:      p_kunnr  TYPE kunnr     OBLIGATORY.
    SELECTION-SCREEN POSITION 52.
    SELECTION-SCREEN COMMENT (40) l_name1.
  SELECTION-SCREEN END OF LINE.
*-CS2023000189-17.04.2023-#108708-JT-fim

SELECTION-SCREEN END OF BLOCK a1.

*-CS2023000189-17.04.2023-#108708-JT-inicio
AT SELECTION-SCREEN ON p_kunnr.

  CLEAR l_name1.

  SELECT SINGLE name1
    INTO l_name1
    FROM kna1
   WHERE kunnr = p_kunnr.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Cliente informado Incorreto' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT id, data, hora, lote, kunnr
    INTO TABLE @DATA(t_0166)
    FROM zsdt0166
   WHERE matnr   = @s_matnr-low
     AND werks   = @s_werks-low
     AND lote   IN @s_lgort
     AND safra   = @p_safra
     AND status  = 'A'.

  IF sy-subrc = 0.
    SORT t_0166 BY lote kunnr.
    DELETE ADJACENT DUPLICATES FROM t_0166
                          COMPARING lote kunnr.

    l_error = abap_false.
    l_lotes = abap_false.

    LOOP AT t_0166 INTO DATA(w_0166).
      IF w_0166-kunnr <> p_kunnr.
        l_error = abap_true.
        l_lotes = l_lotes && |{ w_0166-lote } |.
      ENDIF.
    ENDLOOP.

    IF l_error = abap_true.
      MESSAGE i024(sd) WITH 'Atenção! Cliente: ' p_kunnr 'sem TAKE-UP para os lotes: ' l_lotes.
    ENDIF.
  ENDIF.
*-CS2023000189-17.04.2023-#108708-JT-fim

*----------------------------------------------------------------------*
*                           Start of Selection                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  REFRESH: t_msg, t_alv.

* Seleciona Dados
  PERFORM: z_seleciona_dados,

* Processa Dados
           z_processa_dados,

* Monta FieldCat
           z_monta_fieldcat.

  CHECK NOT t_alv_ger[] IS INITIAL.  ""*-CS2023000189-17.04.2023-#108708-JT

  PERFORM f_posicao_tela USING 1.  "*-CS2023000189-17.04.2023-#108708-JT

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                             Seleciona Dados                          *
*----------------------------------------------------------------------*

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  '@0X@'.
    ty_toolbar-function  =  'PRT'.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.
  METHOD handle_user_command.

*    DATA: vl_fm_name           TYPE rs38l_fnam,
*          w_control_parameters TYPE ssfctrlop.

    CASE e_ucomm.

*-CS2021000799 - 05.11.2021 - JT - inicio
      WHEN 'PRT'.
        PERFORM f_imprime_formulario.  "*-CS2023000189-17.04.2023-#108708-JT
































*-CS2021000799 - 05.11.2021 - JT - fim

    ENDCASE.

  ENDMETHOD.
ENDCLASS.

***********************************************************
* imprime formulario
***********************************************************
FORM f_imprime_formulario.

  DATA: vl_fm_name           TYPE rs38l_fnam,
        w_control_parameters TYPE ssfctrlop,
        ls_options           TYPE ssfcompop,
        ls_job_output_info   TYPE ssfcrescl,
        ls_otfdata           TYPE tsfotf,
        ls_bin_fsize         TYPE i,
        ls_xstring_document  TYPE xstring,
        t_lines              TYPE STANDARD TABLE OF tline,
        control              TYPE ssfctrlop.

  FREE: t_pdf_files.

*  Impresora
  ls_options-tddest              = 'LOCL'.     "Disposit. saída
  ls_options-tdimmed             = abap_true.  "Saída Imediata
  ls_options-tdnewid             = abap_true.  "Nova Ordem SPOOL
  w_control_parameters-no_dialog = abap_true.
  w_control_parameters-langu     = sy-langu.
  w_control_parameters-getotf    = abap_true.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = c_form
    IMPORTING
      fm_name            = vl_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*-------------------------------
* gera smartform de cada bloco
*-------------------------------
  LOOP AT t_dados_ger INTO w_dados_ger.

    w_cabec   = w_dados_ger-w_cabec.
    t_dados[] = w_dados_ger-t_dados[].

    CALL FUNCTION vl_fm_name
      EXPORTING
        output_options     = ls_options
        control_parameters = w_control_parameters
        i_cabec            = w_cabec
      IMPORTING
        job_output_info    = ls_job_output_info
      TABLES
        it_saida           = t_dados
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    MOVE ls_job_output_info-otfdata[] TO ls_otfdata[].

*-------------------------------------------
*---format xstring
*-------------------------------------------
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = ls_bin_fsize
        bin_file              = ls_xstring_document
      TABLES
        otf                   = ls_otfdata[]
        lines                 = t_lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

    CLEAR w_pdf_files.
    w_pdf_files-data       =  ls_xstring_document.
    w_pdf_files-len        =  xstrlen( ls_xstring_document ).
    APPEND w_pdf_files    TO t_pdf_files.

  ENDLOOP.

  CHECK t_pdf_files[] IS NOT INITIAL.

*-----------------------------------------
* agrupa documentos
*-----------------------------------------
  TRY.
      l_pdf_xtring = zcl_faturamento=>zif_faturamento~get_instance(
                         )->get_merge_pdf( EXPORTING t_pdf_files = t_pdf_files
                         ).

    CATCH zcx_faturamento.
    CATCH zcx_error.
  ENDTRY.

*-----------------------------------------
* visualiza PDF
*-----------------------------------------
  CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
    EXPORTING
      i_pdf                    = l_pdf_xtring
    EXCEPTIONS
      convert_otf_to_pdf_error = 1
      cntl_error               = 2
      OTHERS                   = 3.

ENDFORM.

FORM z_seleciona_dados.

  "perform z_seleciona_doc.
* Seleciona MKPF
  PERFORM: "z_seleciona_mkpf,

* Seleciona MSEG
           "z_seleciona_mseg,

* Seleciona MCHB
           z_seleciona_mchb,

* Seleciona Dados Características
           z_seleciona_carac,

           z_seleciona_mchb_fill_safra,

           f_rebuild_mchb_fardos_trace TABLES t_mchb, "Remontar MCHB com dados Fardos Trace Cotton

* Seleciona T001L
           z_seleciona_t001l,

* Seleciona Mara
           z_seleciona_mara,

* Seleciona CHVW
           z_seleciona_chvw,

" Seleciona AUFK
           z_seleciona_aufk,

* Dados PP
           z_seleciona_pp.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MKPF                                         *
*&---------------------------------------------------------------------*
*                              Seleciona MKPF                          *
*----------------------------------------------------------------------*
FORM z_seleciona_mkpf.

  REFRESH t_mkpf.

  CHECK NOT t_mkpf_aux[] IS INITIAL.
  SELECT mblnr mjahr budat
  FROM mkpf
    INTO TABLE t_mkpf
    FOR ALL ENTRIES IN t_mkpf_aux
  WHERE mblnr EQ t_mkpf_aux-mblnr
    AND mjahr EQ t_mkpf_aux-mjahr.
  "and budat in s_budat.

  IF t_mkpf[] IS INITIAL.
    MESSAGE i836 WITH TEXT-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_MKPF

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MSEG                                         *
*&---------------------------------------------------------------------*
*                              Seleciona MSEG                          *
*----------------------------------------------------------------------*
FORM z_seleciona_mseg.

  DATA sl_mseg TYPE type_mseg.

  REFRESH: t_mseg, t_312.

  CHECK NOT t_mkpf[] IS INITIAL.

  SELECT mblnr mjahr zeile bwart matnr
         werks lgort charg menge aufnr
         bukrs smbln shkzg
  FROM mseg
    INTO TABLE t_mseg
    FOR ALL ENTRIES IN t_mkpf
  WHERE mblnr EQ t_mkpf-mblnr
    AND mjahr EQ t_mkpf-mjahr
    AND matnr IN s_matnr
    AND werks IN s_werks
    AND lgort IN s_lgort.

  t_312[] = t_mseg[].
  DELETE: t_mseg WHERE bwart NE c_311,
          t_mseg WHERE shkzg NE c_s,
          t_312  WHERE bwart NE c_312,
          t_312  WHERE smbln IS INITIAL.

  SORT t_mseg BY mblnr ASCENDING
                 mjahr ASCENDING.

  IF NOT t_312[] IS INITIAL.
    LOOP AT t_312 INTO sl_mseg.
      READ TABLE t_mseg WITH KEY mblnr = sl_mseg-smbln
                                 mjahr = sl_mseg-mjahr
                        BINARY SEARCH
                        TRANSPORTING NO FIELDS.

      CHECK sy-subrc IS INITIAL.

      DELETE t_mseg INDEX sy-tabix.

      CLEAR sl_mseg.
    ENDLOOP.
  ENDIF.

  IF t_mseg[] IS INITIAL.
    MESSAGE i836 WITH TEXT-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_MSEG

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_CARAC                                        *
*&---------------------------------------------------------------------*
*                     Seleciona Dados Características                  *
*----------------------------------------------------------------------*
FORM z_seleciona_carac.
  DATA: tl_mchb TYPE TABLE OF type_mchb,
        tl_kssk TYPE TABLE OF type_kssk.

  REFRESH: t_inob, t_kssk, t_klah.
  CLEAR st_rmclm.

  CHECK t_mchb[] IS NOT INITIAL.

  tl_mchb[] = t_mchb[].
  SORT tl_mchb BY objek ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_mchb COMPARING objek.

  SELECT SINGLE a~klart
  FROM tcla AS a
    INNER JOIN tclao AS b ON a~klart EQ b~klart
    INTO st_rmclm-klart
  WHERE a~obtab    EQ 'MCHA'
    AND a~intklart EQ space
    AND a~multobj  EQ 'X'
    AND b~obtab    EQ 'MCH1'.

  SELECT cuobj klart obtab objek
  FROM inob
    INTO TABLE t_inob
    FOR ALL ENTRIES IN t_mchb
  WHERE klart EQ st_rmclm-klart
    AND obtab EQ 'MARA'
    AND objek EQ t_mchb-objek.

  CHECK NOT t_inob[] IS INITIAL.
* Move Cuobj p/ Objekk
  PERFORM z_move_cuobj_objekk.

  SELECT objek mafid klart clint adzhl
  FROM kssk
    INTO TABLE t_kssk
    FOR ALL ENTRIES IN t_inob
  WHERE objek EQ t_inob-objekk
    AND mafid EQ 'O'
    AND klart EQ st_rmclm-klart.

  CHECK NOT t_kssk[] IS INITIAL.
  tl_kssk[] = t_kssk[].
  SORT tl_kssk BY clint ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_kssk COMPARING clint.

  SELECT clint klart class vondt bisdt
  FROM klah
    INTO TABLE t_klah
    FOR ALL ENTRIES IN tl_kssk
  WHERE clint EQ tl_kssk-clint.

ENDFORM.                    " Z_SELECIONA_CARAC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MCHB                                         *
*&---------------------------------------------------------------------*
*                            Seleciona MCHB                            *
*----------------------------------------------------------------------*
FORM z_seleciona_mchb.
  DATA tl_mseg TYPE TABLE OF type_mseg.

*  check not t_mseg[] is initial.
*  tl_mseg[] = t_mseg[].
*  sort tl_mseg by matnr werks lgort charg ascending.
*  delete adjacent duplicates from tl_mseg comparing matnr werks lgort charg.
*
*  select matnr werks lgort charg
*  from mchb
*    into table t_mchb
*    for all entries in tl_mseg
*  where matnr eq tl_mseg-matnr
*    and werks eq tl_mseg-werks
*    and lgort eq tl_mseg-lgort
*    and charg eq tl_mseg-charg.
*
*  sort t_mchb by matnr ascending
*                 werks ascending
*                 lgort ascending
*                 charg ascending.

  SELECT matnr werks lgort charg clabs cspem
  FROM mchb
    INTO TABLE t_mchb
  WHERE matnr IN s_matnr
    AND werks IN s_werks
    AND lgort IN s_lgort
    AND ( clabs > 0 OR cspem > 0 ).

* Move Matnr p/ Objek
  PERFORM z_move_matnr_objek.


ENDFORM.                    " Z_SELECIONA_MCHB

FORM f_rebuild_mchb_fardos_trace TABLES i_t_mchb LIKE t_mchb.

  DATA: lwa_mchb TYPE type_mchb.

  DATA(lit_mchb_bkp)   = i_t_mchb[].
  DATA(lit_mchb_group) = lit_mchb_bkp[].

  CLEAR: i_t_mchb[].

  SORT lit_mchb_group BY matnr werks lgort.
  DELETE ADJACENT DUPLICATES FROM lit_mchb_group COMPARING matnr werks lgort.

  "Consultar Fardinhos Trace Cotton
  LOOP AT lit_mchb_group INTO DATA(lwa_mchb_group).

    zcl_trace_cotton_utils=>get_fardos_bloco_trace_cotton(
      EXPORTING
        i_safra                     = CONV #( lwa_mchb_group-safra )
        i_filial_algodoeira         = CONV #( lwa_mchb_group-werks )
        i_bloco                     = CONV #( lwa_mchb_group-lgort )
        i_matnr                     = CONV #( lwa_mchb_group-matnr )
        i_check_embarque_sap        = abap_true
      IMPORTING
        e_msg_error                 = DATA(_msg_error)
        e_fardos_bloco_trace_cotton = DATA(lit_fardos_trace)
    ).

    IF _msg_error IS NOT INITIAL.
      MESSAGE _msg_error TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DELETE lit_fardos_trace WHERE embarcado_sap = abap_true.

    LOOP AT lit_fardos_trace INTO DATA(lwa_fardo_trace).
      APPEND lwa_fardo_trace TO git_fardos_trace.
    ENDLOOP.
  ENDLOOP.


  LOOP AT  git_fardos_trace INTO lwa_fardo_trace.
    CLEAR: lwa_mchb.

    READ TABLE lit_mchb_bkp INTO DATA(lwa_mchb_bkp) WITH KEY werks = lwa_fardo_trace-id_filial_algodoeira
                                                             safra = lwa_fardo_trace-safra
                                                             lgort = lwa_fardo_trace-bloco.
    CHECK sy-subrc EQ 0.

    lwa_mchb-matnr                = lwa_mchb_bkp-matnr.
    lwa_mchb-werks                = lwa_fardo_trace-id_filial_algodoeira.
    lwa_mchb-lgort                = lwa_fardo_trace-bloco.
    lwa_mchb-charg                = lwa_mchb_bkp-charg.
    lwa_mchb-clabs                = lwa_fardo_trace-peso_liquido.
    lwa_mchb-safra                = lwa_mchb_bkp-safra.
    lwa_mchb-nr_fardo_completo    = lwa_fardo_trace-nr_fardo_completo.
    lwa_mchb-nr_maquina           = lwa_fardo_trace-nr_maquina.

    APPEND lwa_mchb TO i_t_mchb.

  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_CUOBJ_OBJEKK                                      *
*&---------------------------------------------------------------------*
*                          Move Cuobj p/ Objekk                        *
*----------------------------------------------------------------------*
FORM z_move_cuobj_objekk.

  DATA: sl_inob  TYPE type_inob,
        vl_index TYPE i.

  LOOP AT t_inob INTO sl_inob.
    vl_index = sy-tabix.

    sl_inob-objekk = sl_inob-cuobj.
    MODIFY t_inob FROM sl_inob INDEX vl_index
      TRANSPORTING objekk.

    CLEAR sl_inob.
  ENDLOOP.

ENDFORM.                    " Z_MOVE_CUOBJ_OBJEKK

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_MATNR_OBJEK                                       *
*&---------------------------------------------------------------------*
*                          Move Matnr p/ Objek                         *
*----------------------------------------------------------------------*
FORM z_move_matnr_objek.

  DATA: sl_mchb  TYPE type_mchb,
        vl_index TYPE i.

  LOOP AT t_mchb INTO sl_mchb.
    vl_index = sy-tabix.

    sl_mchb-objek = sl_mchb-matnr.
    MODIFY t_mchb FROM sl_mchb INDEX vl_index
      TRANSPORTING objek.

    CLEAR sl_mchb.
  ENDLOOP.

ENDFORM.                    " Z_MOVE_MATNR_OBJEK

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS                                         *
*&---------------------------------------------------------------------*
*                           Processa Dados                             *
*----------------------------------------------------------------------*
FORM z_processa_dados.

  DATA: sl_mseg   TYPE type_mseg,
        sl_mchb   TYPE type_mchb,
        sl_inob   TYPE type_inob,
        sl_kssk   TYPE type_kssk,
        sl_klah   TYPE type_klah,
        sl_keys   TYPE bapi1003_object_keys,
        sl_mara   TYPE type_mara,
        sl_aufk   TYPE type_aufk,
        sl_msegpp TYPE type_mseg,
        vl_num    TYPE bapi1003_key-classnum,
        vl_type   TYPE bapi1003_key-classtype,
        vl_table  TYPE bapi1003_key-objecttable,
        vl_key    TYPE bapi1003_key-object,
        vl_talhao TYPE char30,
        vl_safra  TYPE numc4,
        vl_varie  TYPE char30,
        vl_bukrs  TYPE mseg-bukrs,
        sl_alv    TYPE type_alv,
        vl_cont   TYPE numc2,
        vl_cont2  TYPE i,
        vl_ok     TYPE char1,
        "vl_menge  type mseg-menge                         ,
        vl_clabs  TYPE mchb-clabs,
        tl_table  TYPE TABLE OF bapi1003_object_keys,
        tl_ret    TYPE TABLE OF bapiret2,
        l_name1   TYPE kna1-name1,
        l_kunnr   TYPE char10,
        l_acts    TYPE zacts.

*  sort: t_inob   by objek ascending,
*        t_kssk   by objek ascending,
*        t_klah   by clint ascending,
*        t_mseg   by bukrs ascending,
*        t_mara   by matnr ascending,
*        t_aufk   by aufnr ascending,
*        t_mseg   by charg ascending,
*        t_msegpp by matnr ascending
*                    werks ascending
*                    charg ascending.

  SORT: t_mchb   BY charg ASCENDING,
        t_chvw   BY charg ASCENDING,
        t_aufk   BY aufnr ASCENDING,
        t_kssk   BY objek ASCENDING,
        t_klah   BY clint ASCENDING,
        t_mara   BY matnr ASCENDING,
        t_aufk   BY aufnr ASCENDING.

*-CS2023000189-17.04.2023-#108708-JT-inicio
  t_mchb_ger[] = t_mchb[].
  SORT t_mchb_ger BY lgort.
  DELETE ADJACENT DUPLICATES FROM t_mchb_ger
                        COMPARING lgort.
*-CS2023000189-17.04.2023-#108708-JT-fim

  SORT t_mchb BY nr_fardo_completo. "MM - Ordenação Nr. Fardos ZMM0026 - #150194 - WPP

*-CS2023000189-17.04.2023-#108708-JT-inicio
  FREE: t_alv_ger, t_cabec_ger.

  LOOP AT t_mchb_ger INTO w_mchb_ger.
*-CS2023000189-17.04.2023-#108708-JT-fim

    FREE: t_det_sai, t_alv, l_acts.
    CLEAR st_cabec.

    vl_table = 'MCH1'.
    vl_cont  = 1.

    CLEAR: sl_mchb, vl_clabs, l_error.
*---> 05/07/2023 - Migração S4 - DL
    SORT t_inob BY objek.
    SORT t_kssk BY objek.
    SORT t_klah BY clint.
*<--- 05/07/2023 - Migração S4 - DL

    LOOP AT t_mchb  INTO sl_mchb WHERE lgort EQ w_mchb_ger-lgort  "*-CS2023000189-17.04.2023-#108708-JT
                                   AND safra EQ p_safra.

      ""Projeto Reestruturação Algodao 2024
*      READ TABLE t_chvw INTO st_chvw WITH KEY werks = sl_mchb-werks
*                                              matnr = sl_mchb-matnr
*                                              charg = sl_mchb-charg
*                                     BINARY SEARCH.
*
*      READ TABLE t_aufk INTO st_aufk WITH KEY aufnr = st_chvw-aufnr
*                                     BINARY SEARCH.
      ""Projeto Reestruturação Algodao 2024


      sl_keys-key_field = 'MATNR'.
      sl_keys-value_int = sl_mchb-matnr.
      APPEND sl_keys TO tl_table.

      sl_keys-key_field = 'CHARG'.
      sl_keys-value_int = sl_mchb-charg.
      APPEND sl_keys TO tl_table.

      CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
        EXPORTING
          objecttable    = vl_table
        IMPORTING
          objectkey_conc = vl_key
        TABLES
          objectkeytable = tl_table
          return         = tl_ret.

      READ TABLE t_inob INTO sl_inob WITH KEY objek = sl_mchb-matnr
                                     BINARY SEARCH.

      READ TABLE t_kssk INTO sl_kssk WITH KEY objek = sl_inob-objekk
                                     BINARY SEARCH.

      READ TABLE t_klah INTO sl_klah WITH KEY clint = sl_kssk-clint
                                     BINARY SEARCH.

      vl_num  = sl_klah-class.
      vl_type = st_rmclm-klart.

*   Preenche Características
      PERFORM z_preenche_carac USING vl_key
                                     vl_table
                                     vl_num
                                     vl_type
                                     sl_mchb
                            CHANGING vl_varie
                                     vl_ok.

      IF NOT vl_ok IS INITIAL.
        CONTINUE.
      ENDIF.

      ADD 1 TO: vl_cont2,
                st_cabec-fardos.

      IF NOT ( sl_mchb-clabs IS INITIAL ).
        ADD sl_mchb-clabs TO vl_clabs.
      ELSE.
        ADD sl_mchb-cspem TO vl_clabs.
      ENDIF.

      READ TABLE t_mara INTO sl_mara WITH KEY matnr = sl_mchb-matnr
                                     BINARY SEARCH.

      PERFORM z_verifica_index CHANGING vl_cont2
                                        vl_cont.

*-CS2023000189-17.04.2023-#108708-JT-inicio
      CLEAR st_t001l.




      SELECT SINGLE werks lgort lgobe
       FROM t001l
       INTO st_t001l
      WHERE werks  = sl_mchb-werks
        AND lgort  = w_mchb_ger-lgort.

*     SELECT SINGLE acts
*       INTO l_acts
*       FROM zsdt0166







*      WHERE matnr  = sl_mchb-matnr
*        AND werks  = sl_mchb-werks
*        AND kunnr  = p_kunnr
*        AND lote   = w_mchb_ger-lgort
*        AND safra  = sl_mchb-safra
*        AND status = 'A'.

      PERFORM f_verifica_acts    USING sl_mchb-matnr
                                       sl_mchb-werks
                                       w_mchb_ger-lgort
                                       sl_mchb-safra
                                       p_kunnr
                              CHANGING l_acts
                                       l_error.

      IF l_error = abap_true.
        EXIT.
      ENDIF.
*-CS2023000189-17.04.2023-#108708-JT-fim

      PERFORM z_preenche_saida USING vl_cont
                                     vl_cont2
                                     sl_mchb
                                     sl_mara-normt
                                     sl_mchb-nr_maquina. "st_aufk-aufex.  "Projeto Reestruturação Algodao 2024

      CLEAR: sl_mchb, st_chvw, st_aufk, sl_inob, sl_kssk, sl_klah.

    ENDLOOP.
*
*-CS2023000189-17.04.2023-#108708-JT-inicio
    IF l_error = abap_true.
      CONTINUE.
    ENDIF.

    SELECT SINGLE name1
      INTO l_name1
      FROM kna1
     WHERE kunnr = p_kunnr.

    CONDENSE l_name1.


    l_kunnr = p_kunnr.
    PACK l_kunnr TO l_kunnr.
    CONDENSE l_kunnr.

    st_cabec-cliente   = COND #( WHEN p_kunnr IS INITIAL THEN abap_off
                                                         ELSE |{ l_kunnr } - { l_name1 } | ).
    st_cabec-acts      = COND #( WHEN l_acts = abap_off  THEN 'Não'
                                                         ELSE 'Sim' ).
*-CS2023000189-17.04.2023-#108708-JT-fim

    st_cabec-lgort     = st_t001l-lgort.    "*-CS2023000189-17.04.2023-#108708-JT
    st_cabec-variedade = vl_varie.
    WRITE vl_clabs TO st_cabec-peso.
    CONDENSE st_cabec-peso NO-GAPS.

    APPEND st_cabec   TO t_cabec_ger.     "*-CS2023000189-17.04.2023-#108708-JT

    sl_alv-lgort     = st_t001l-lgort.    "*-CS2023000189-17.04.2023-#108708-JT
    sl_alv-fardo01   = st_cabec-fardos.
    sl_alv-peso01    = vl_clabs.
    sl_alv-maquina01 = st_t001l-lgobe.
    sl_alv-fardo02   = s_werks-low.
    SHIFT sl_alv-fardo01 LEFT DELETING LEADING '0'.
    APPEND sl_alv TO t_alv.

    APPEND LINES OF t_alv[] TO t_alv_ger[]. "*-CS2023000189-17.04.2023-#108708-JT

*-CS2021000799 - 05.11.2021 - JT - inicio
    w_det_sai-fardo    = st_cabec-fardos.
    w_det_sai-peso     = vl_clabs.
    APPEND w_det_sai  TO t_det_sai.

    PERFORM f_monta_formulario.
*-CS2021000799 - 05.11.2021 - JT - fim

  ENDLOOP.  "*-CS2023000189-17.04.2023-#108708-JT

ENDFORM.                    " Z_PROCESSA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_CARAC                                         *
*&---------------------------------------------------------------------*
FORM f_monta_formulario.

  DATA: sl_alv  TYPE type_alv,
        sl_mchb TYPE type_mchb,
        l_name1 TYPE kna1-name1,
        l_kunnr TYPE char10,
        l_acts  TYPE zacts.

*-------------------------------------------
*-cabecalho formylario
*-------------------------------------------
  FREE: sl_alv, sl_mchb, w_cabec, t_dados, l_lin, l_col, l_name1, l_acts.

  READ TABLE t_alv  INTO sl_alv  INDEX 1.
  READ TABLE t_mchb INTO sl_mchb WITH KEY safra = p_safra.

*-CS2023000189-17.04.2023-#108708-JT-inicio
  SELECT SINGLE name1
    INTO l_name1
    FROM kna1
   WHERE kunnr = p_kunnr.

* SELECT SINGLE acts
*   INTO l_acts
*   FROM zsdt0166
*  WHERE matnr  = sl_mchb-matnr
*    AND werks  = st_t001w-werks
*    AND lote   = st_t001l-lgort
*    AND safra  = sl_mchb-safra
*    AND status = 'A'.

  PERFORM f_verifica_acts    USING sl_mchb-matnr
                                   st_t001w-werks
                                   st_t001l-lgort
                                   sl_mchb-safra
                                   p_kunnr
                          CHANGING l_acts
                                   l_error.

  CONDENSE l_name1.

  l_kunnr = p_kunnr.
  PACK l_kunnr TO l_kunnr.
  CONDENSE l_kunnr.

  w_cabec-cliente  = COND #( WHEN p_kunnr IS INITIAL THEN abap_off
                                                     ELSE |{ l_kunnr } - { l_name1 } | ).
  w_cabec-acts     = COND #( WHEN l_acts = abap_off  THEN 'Não'
                                                     ELSE 'Sim' ).
*-CS2023000189-17.04.2023-#108708-JT-fim

  w_cabec-tipo     = sl_alv-tipo01.
  w_cabec-data     = sy-datum.
  w_cabec-centro   = st_t001w-werks.
  w_cabec-maquina  = st_t001l-lgobe.
  w_cabec-deposito = st_t001l-lgort.
  w_cabec-material = sl_mchb-matnr.
  w_cabec-safra    = sl_mchb-safra.

  l_lin = 0.
  l_col = 1.

  DESCRIBE TABLE t_det_sai LINES DATA(l_lines_det).

  LOOP AT t_det_sai  INTO w_det_sai.

    DATA(l_tabix_det) = sy-tabix.

    CLEAR w_dados.

    l_lin = l_lin + 1.

    IF l_lin = 33.
      l_col = l_col + 1.
      IF l_col = 5.
        l_col = 1.
      ENDIF.

      l_lin = 1.
    ENDIF.

    l_tabix = l_lin.

    l_campo1 = 'FARDO_'   && l_col.
    l_campox = 'W_DADOS-' && l_campo1.
    ASSIGN (l_campox) TO <f_campo>.
    IF sy-subrc = 0.
      <f_campo> = w_det_sai-fardo.
    ENDIF.

    l_campo2 = 'PESO_' && l_col && 'X'.
    l_campox = 'W_DADOS-' && l_campo2.
    ASSIGN (l_campox) TO <f_campo>.
    IF sy-subrc = 0.
      <f_campo> = w_det_sai-peso.
    ENDIF.

    IF l_tabix_det = l_lines_det.
      l_campo3 = 'LINHA_TOTAL_' && l_col.
      l_campox = 'W_DADOS-' && l_campo3.
      ASSIGN (l_campox) TO <f_campo>.
      IF sy-subrc = 0.
        <f_campo> = abap_true.
      ENDIF.
    ENDIF.

    IF l_col = 1.
      APPEND w_dados   TO t_dados.
    ELSE.
      MODIFY t_dados FROM w_dados INDEX l_tabix
                     TRANSPORTING (l_campo1) (l_campo2) (l_campo3).
    ENDIF.

  ENDLOOP.

  "*-CS2023000189-17.04.2023-#108708-JT-inicio
  w_dados_ger-w_cabec   = w_cabec.
  w_dados_ger-t_dados[] = t_dados[].
  APPEND w_dados_ger   TO t_dados_ger.
  "*-CS2023000189-17.04.2023-#108708-JT-fim

*-------------------------------------------
*-monta linhas formylario
*-------------------------------------------
* LOOP AT t_alv   INTO sl_alv.
*   w_dados-fardo_01 = sl_alv-fardo01.
*   w_dados-peso_01  = sl_alv-peso01.
*   w_dados-fardo_02 = sl_alv-fardo02.
*   w_dados-peso_02  = sl_alv-peso02.
*   w_dados-fardo_03 = sl_alv-fardo03.
*   w_dados-peso_03  = sl_alv-peso03.
*   w_dados-fardo_04 = sl_alv-fardo04.
*   w_dados-peso_04  = sl_alv-peso04.
*   APPEND w_dados  TO t_dados.
* ENDLOOP.

ENDFORM.

******************************************************
* verificar ACTS
******************************************************
FORM f_verifica_acts    USING p_matnr
                              p_werks
                              p_lote
                              p_safra
                              p_kunnr
                     CHANGING p_acts
                              p_erro.

  FREE: p_acts, p_erro.

  SELECT id, data, hora, acts, contrato, safra, empresa
    INTO TABLE @DATA(t_0166)
    FROM zsdt0166
   WHERE matnr   = @p_matnr
     AND werks   = @p_werks
     AND kunnr   = @p_kunnr
     AND lote    = @p_lote
     AND safra   = @p_safra
     AND status  = 'A'.

  IF sy-subrc = 0.
    SORT t_0166 BY id DESCENDING.
    READ TABLE t_0166 INTO DATA(w_0166) INDEX 1.
    p_acts = w_0166-acts.

    SELECT SINGLE *
      FROM zsdt0143
      INTO @DATA(w_0143)
     WHERE contrato     = @w_0166-contrato
       AND safra        = @w_0166-safra
       AND empresa      = @w_0166-empresa
       AND cancelado   <> @abap_true.

    IF sy-subrc = 0.
      p_acts = w_0143-acts.
    ENDIF.
  ELSE.
    p_erro = abap_true.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_CARAC                                         *
*&---------------------------------------------------------------------*
*                      Preenche Características                        *
*----------------------------------------------------------------------*
FORM z_preenche_carac USING p_key    TYPE bapi1003_key-object
                            p_table  TYPE bapi1003_key-objecttable
                            p_num    TYPE bapi1003_key-classnum
                            p_type   TYPE bapi1003_key-classtype
                            "p_mseg   type type_mseg
                            p_mchb   TYPE type_mchb
                   CHANGING p_varie  TYPE char30
                            p_ok     TYPE char1.

  DATA: vl_campo TYPE char30,
        vl_num   TYPE numc10,
        sl_char  TYPE bapi1003_alloc_values_char,
        sl_num   TYPE bapi1003_alloc_values_num,
        tl_num   TYPE TABLE OF bapi1003_alloc_values_num,
        tl_char  TYPE TABLE OF bapi1003_alloc_values_char,
        tl_curr  TYPE TABLE OF bapi1003_alloc_values_curr,
        tl_ret   TYPE TABLE OF bapiret2,
        data     TYPE numc4.

  CLEAR p_ok.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL' "#EC CI_USAGE_OK[2438131] ---> S4 MIGRATION 06/07/2023 - MA
    EXPORTING
      objectkey       = p_key
      objecttable     = p_table
      classnum        = p_num
      classtype       = p_type
    TABLES
      allocvaluesnum  = tl_num
      allocvalueschar = tl_char
      allocvaluescurr = tl_curr
      return          = tl_ret.

  SORT: tl_num  BY charact_descr ASCENDING,
        tl_char BY charact_descr ASCENDING.

* Verifica Safra
** COMMENT - 20.06.2013
**  READ TABLE TL_NUM INTO SL_NUM WITH KEY CHARACT_DESCR = 'Safra'
**                                BINARY SEARCH.
**
**  MOVE SL_NUM-VALUE_FROM TO DATA.

** ADD - 20.06.2013 - Inicio
  READ TABLE tl_char INTO sl_char WITH KEY charact_descr = 'Safra' BINARY SEARCH.

  MOVE sl_char-value_char TO data.
** ADD - 20.06.2013 - Fim

  IF data NE p_safra.
    p_ok = c_x.
  ENDIF.

*  if sl_num-value_from ne p_safra.
*    p_ok = c_x.
*  endif.

* Busca Variedade
  READ TABLE tl_char INTO sl_char WITH KEY charact_descr = 'Variedade'
                                  BINARY SEARCH.

  IF sy-subrc IS INITIAL AND p_varie IS INITIAL.
    p_varie = sl_char-value_char.
  ENDIF.

ENDFORM.                    " Z_PREENCHE_CARAC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_T001L                                        *
*&---------------------------------------------------------------------*
*                            Seleciona T001L                           *
*----------------------------------------------------------------------*
FORM z_seleciona_t001l.

  CLEAR st_t001l.

  SELECT SINGLE werks lgort lgobe
  FROM t001l
    INTO st_t001l
  WHERE werks IN s_werks
    AND lgort IN s_lgort.

ENDFORM.                    " Z_SELECIONA_T001L

*&---------------------------------------------------------------------*
*&      Form  Z_RETORNA_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_retorna_msg USING p_text TYPE c
                         p_mseg TYPE type_mseg.

  DATA sl_msg TYPE type_msg.

  sl_msg-material = p_mseg-matnr.
  sl_msg-centro   = p_mseg-werks.
  sl_msg-deposito = p_mseg-lgort.
  sl_msg-lote     = p_mseg-charg.
  sl_msg-msg      = p_text.

  APPEND sl_msg TO t_msg.

ENDFORM.                    " Z_RETORNA_MSG

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_SAIDA                                         *
*&---------------------------------------------------------------------*
*                              Preenche Saída                          *
*----------------------------------------------------------------------*
FORM z_preenche_saida USING p_cont  TYPE numc2
                            p_index TYPE i
                            "p_mseg  type type_mseg
                            p_mchb  TYPE type_mchb
                            p_normt TYPE mara-normt
                            p_aufex TYPE aufk-aufex.

  DATA: sl_alv   TYPE type_alv,
        vl_lines TYPE i,
        vl_pos   TYPE i,
        vl_tam   TYPE i.

  "Projeto Reestruturação Algodao 2024
  "SEARCH p_mchb-charg FOR './.'.
  SEARCH p_mchb-nr_fardo_completo FOR './.'.
  ""Projeto Reestruturação Algodao 2024
  IF sy-fdpos > 0.
    vl_pos = sy-fdpos + 1.
  ELSE.
    vl_pos = 0.
  ENDIF.

  vl_tam = 10 - vl_pos.

  sl_alv-lgort     = st_t001l-lgort.    "*-CS2023000189-17.04.2023-#108708-JT

  PERFORM z_campo_dinamico USING 'FARDO'
                                 p_cont
                                 p_mchb-nr_fardo_completo+vl_pos(vl_tam) "p_mchb-charg+vl_pos(vl_tam)
                                 c_c
                        CHANGING sl_alv.

  w_det_sai-fardo = p_mchb-nr_fardo_completo+vl_pos(vl_tam).

  PERFORM z_campo_dinamico USING 'TIPO'
                                 p_cont
                                 p_normt
                                 c_c
                        CHANGING sl_alv.

  IF NOT ( p_mchb-clabs IS INITIAL ).
    PERFORM z_campo_dinamico USING 'PESO'
                                    p_cont
                                    p_mchb-clabs
                                    c_q
                           CHANGING sl_alv.
    w_det_sai-peso = p_mchb-clabs.
  ELSE.
    PERFORM z_campo_dinamico USING 'PESO'
                                    p_cont
                                    p_mchb-cspem
                                    c_q
                           CHANGING sl_alv.
    w_det_sai-peso = p_mchb-cspem.
  ENDIF.

  PERFORM z_campo_dinamico USING 'MAQUINA'
                                 p_cont
                                 p_aufex
                                 c_c
                        CHANGING sl_alv.

  DESCRIBE TABLE t_alv LINES vl_lines.

  APPEND w_det_sai   TO t_det_sai.

  IF vl_lines LE 39.
    APPEND sl_alv TO t_alv.
    EXIT.
  ENDIF.

  CASE p_cont.
    WHEN 2.
      MODIFY t_alv FROM sl_alv
        INDEX p_index TRANSPORTING fardo02 tipo02 peso02 maquina02.
    WHEN 3.
      MODIFY t_alv FROM sl_alv
        INDEX p_index TRANSPORTING fardo03 tipo03 peso03 maquina03.
    WHEN 4.
      MODIFY t_alv FROM sl_alv
        INDEX p_index TRANSPORTING fardo04 tipo04 peso04 maquina04.
  ENDCASE.

ENDFORM.                    " Z_PREENCHE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_CAMPO_DINAMICO                                         *
*&---------------------------------------------------------------------*
*                             Campo Dinamico                           *
*----------------------------------------------------------------------*
FORM z_campo_dinamico USING p_campo TYPE c
                            p_cont  TYPE numc2
                            p_valor TYPE any
                            p_tipo  TYPE c
                   CHANGING p_alv   TYPE type_alv.

  DATA vl_campo TYPE char30.

  FIELD-SYMBOLS: <campo>  TYPE any,
                 <campo2> TYPE menge_d.

  CONCATENATE 'P_ALV-'
              p_campo
              p_cont
         INTO vl_campo.

  CASE p_tipo.
    WHEN c_c.
      ASSIGN (vl_campo) TO <campo>.
      CHECK <campo> IS ASSIGNED.
      <campo> = p_valor.
      CONDENSE <campo> NO-GAPS.
      UNASSIGN <campo>.
    WHEN c_q.
      ASSIGN (vl_campo) TO <campo2>.
      CHECK <campo2> IS ASSIGNED.
      <campo2> = p_valor.
      UNASSIGN <campo2>.
  ENDCASE.

ENDFORM.                    " Z_CAMPO_DINAMICO

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MARA                                         *
*&---------------------------------------------------------------------*
*                             Seleciona Mara                           *
*----------------------------------------------------------------------*
FORM z_seleciona_mara.

  DATA tl_mseg TYPE TABLE OF type_mseg.

  REFRESH t_mara.

  CHECK t_mchb[] IS NOT INITIAL.

*  tl_mseg[] = t_mseg[].
*  sort tl_mseg by matnr ascending.
*  delete adjacent duplicates from tl_mseg comparing matnr.
*
*  check not tl_mseg[] is initial.

*  select matnr normt
*  from mara
*    into table t_mara
*    for all entries in tl_mseg
*  where matnr eq tl_mseg-matnr.

  SELECT matnr normt
  FROM mara
    INTO TABLE t_mara
    FOR ALL ENTRIES IN t_mchb
  WHERE matnr EQ t_mchb-matnr.

ENDFORM.                    " Z_SELECIONA_MARA

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

*-CS2023000189-17.04.2023-#108708-JT-inicio
  PERFORM f_posicao_tela   USING l_index_frame.
*-CS2023000189-17.04.2023-#108708-JT-fim

* Instancia Container
  PERFORM: z_inst_cont,
* Instancia Alv
           z_inst_alv,
* Exibe Alv
           z_exibe_alv.

ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

MODULE m_trata_lgort INPUT.

  t_alv[] = t_alv_ger[].
  DELETE t_alv WHERE lgort <> st_t001l-lgort.

  SELECT SINGLE werks lgort lgobe
    FROM t001l
    INTO st_t001l
   WHERE werks  = st_t001w-werks
     AND lgort  = st_t001l-lgort.

  READ TABLE t_cabec_ger INTO st_cabec WITH KEY lgort = st_t001l-lgort.
  l_index_frame = sy-tabix.

ENDMODULE.

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
    MESSAGE i836 WITH TEXT-019.
  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM z_inst_alv.
  CHECK s_alv IS INITIAL.

  CREATE OBJECT s_alv
    EXPORTING
      i_parent          = s_cont
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH TEXT-020.
  ENDIF.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.
  DATA vl_int TYPE int4.

  IF obg_toolbar IS INITIAL.    "*-CS2023000189-17.04.2023-#108708-JT
    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = s_alv.

    SET HANDLER: obg_toolbar->on_toolbar FOR s_alv,
                 obg_toolbar->handle_user_command FOR s_alv.

    CALL METHOD s_alv->set_table_for_first_display
      EXPORTING
        i_default                     = c_x
        is_layout                     = s_layout
        it_toolbar_excluding          = t_tool
      CHANGING
        it_outtab                     = t_alv
        it_fieldcatalog               = t_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CALL METHOD s_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      LEAVE TO SCREEN 0.

*-CS2023000189-17.04.2023-#108708-JT-inicio
    WHEN 'BTN_TOTAL_LEFT'  OR 'BTN_LEFT' OR
         'BTN_TOTAL_RIGHT' OR 'BTN_RIGHT'.
      PERFORM f_navegar_tela    USING sy-ucomm
                             CHANGING l_index_frame.
*-CS2023000189-17.04.2023-#108708-JT-fim
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              Exit Command                            *
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR 'CANC' OR 'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*-CS2023000189-17.04.2023-#108708-JT-inicio
MODULE f_busca_deposito INPUT.

  TYPES: BEGIN OF ty_dep,
           lgort TYPE mchb-lgort,  "*-CS2023000189-17.04.2023-#108708-JT
           lgobe TYPE t001l-lgobe.
  TYPES: END OF ty_dep.

  DATA: t_dep     TYPE TABLE OF ty_dep,
        w_dep     TYPE ty_dep,
        l_lgobe   TYPE t001l-lgobe,
        it_return TYPE TABLE OF ddshretval,
        wa_return LIKE LINE OF it_return.

  FREE: t_dep.

  LOOP AT t_cabec_ger INTO w_cabec_ger.
    SELECT SINGLE lgobe
      FROM t001l
      INTO l_lgobe
     WHERE werks  = st_t001w-werks
       AND lgort  = w_cabec_ger-lgort.

    w_dep-lgort   = w_cabec_ger-lgort.
    w_dep-lgobe   = l_lgobe.
    APPEND w_dep TO t_dep.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'LGORT'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'ST_CABEC-LGORT'
      value_org   = 'S'
    TABLES
      value_tab   = t_dep.

ENDMODULE.
*-CS2023000189-17.04.2023-#108708-JT-fim

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_FIELDCAT                                         *
*&---------------------------------------------------------------------*
*                           Monta FieldCat                             *
*----------------------------------------------------------------------*
FORM z_monta_fieldcat.
  REFRESH: t_fcat, t_tool.

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:
    c_table 'FARDO01'   TEXT-003 10 space,
    c_table 'TIPO01'    TEXT-004 10 space,
    c_table 'PESO01'    TEXT-005 10 space,
    c_table 'MAQUINA01' TEXT-006 18 c_x  ,
    c_table 'FARDO02'   TEXT-003 10 space,
    c_table 'TIPO02'    TEXT-004 10 space,
    c_table 'PESO02'    TEXT-005 10 space,
    c_table 'MAQUINA02' TEXT-006 18 c_x  ,
    c_table 'FARDO03'   TEXT-003 10 space,
    c_table 'TIPO03'    TEXT-004 10 space,
    c_table 'PESO03'    TEXT-005 10 space,
    c_table 'MAQUINA03' TEXT-006 18 c_x  ,
    c_table 'FARDO04'   TEXT-003 10 space,
    c_table 'TIPO04'    TEXT-004 10 space,
    c_table 'PESO04'    TEXT-005 10 space,
    c_table 'MAQUINA04' TEXT-006 18 c_x  .

* Monta Layout
  PERFORM z_layout.

* Exclui Botoes
  PERFORM z_exclui_bots USING: '&SORT'    ,
                               '&SORT_ASC',
                               '&SORT_DSC',
                               '&SUBTOT'  ,
                               '&SUMC'    ,
                               '&PRINT_BACK',
                               '&MAXIMUM' ,
                               '&MINIMUM' ,
                               '&AVERAGE' .

ENDFORM.                    " Z_MONTA_FIELDCAT

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
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.
  CLEAR s_layout.

  s_layout-zebra      = c_x.
  s_layout-cwidth_opt = c_x.
ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_PP                                           *
*&---------------------------------------------------------------------*
*                                Dados PP                              *
*----------------------------------------------------------------------*
FORM z_seleciona_pp.

  IF t_aufk[] IS NOT INITIAL.
    SELECT aufnr aufex
    FROM coas
      INTO TABLE t_coas
      FOR ALL ENTRIES IN t_aufk
    WHERE aufnr EQ t_aufk-aufnr.
  ENDIF.

  SELECT SINGLE werks name1
  FROM t001w
    INTO st_t001w
  WHERE werks IN s_werks.

*  refresh: t_mkpfpp, t_msegpp.
*
*  clear st_t001w.
*
*  select single werks name1
*  from t001w
*    into st_t001w
*  where werks in s_werks.
*
*  if s_budat-low(04) eq s_budat-high(04).
*
*    select mblnr mjahr budat
*    from mkpf
*      into table t_mkpfpp
*    where mjahr eq s_budat-low(04)
*      and vgart eq c_ws.
*  else.
*    select mblnr mjahr budat
*    from mkpf
*      into table t_mkpfpp
*    where ( mjahr eq s_budat-low(04) or
*          mjahr eq s_budat-high(04) )
*      and vgart eq c_ws.
*  endif.
*
*  check not t_mkpfpp[] is initial.
*
*  select mblnr mjahr zeile bwart matnr
*         werks lgort charg menge aufnr
*         bukrs
*  from mseg
*    into table t_msegpp
*    for all entries in t_mkpfpp
*  where mblnr eq t_mkpfpp-mblnr
*    and mjahr eq t_mkpfpp-mjahr
*    and bwart eq c_131
*    and werks in s_werks.

ENDFORM.                    " Z_SELECIONA_PP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_CHVW
*&---------------------------------------------------------------------*
FORM z_seleciona_chvw.

  CHECK t_mchb[] IS NOT INITIAL.

  SELECT werks matnr charg aufnr
  FROM chvw
    INTO TABLE t_chvw
    FOR ALL ENTRIES IN t_mchb
  WHERE werks  EQ t_mchb-werks
    AND matnr  EQ t_mchb-matnr
    AND charg  EQ t_mchb-charg.

ENDFORM.                    " Z_SELECIONA_CHVW

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_AUFK                                         *
*&---------------------------------------------------------------------*
*                            Seleciona AUFK                            *
*----------------------------------------------------------------------*
FORM z_seleciona_aufk.

*  data tl_mseg type table of type_mseg.
*
*  refresh t_aufk.
*
*  tl_mseg[] = t_msegpp[].
*  sort tl_mseg by aufnr ascending.
*  delete adjacent duplicates from tl_mseg comparing aufnr.
*  delete tl_mseg where aufnr is initial.
*
*  check not tl_mseg[] is initial.

*  select aufnr aufex
*  from aufk
*    into table t_aufk
*    for all entries in tl_mseg
*  where aufnr eq tl_mseg-aufnr.

  CHECK t_chvw[] IS NOT INITIAL.

  SELECT aufnr aufex
  FROM aufk
    INTO TABLE t_aufk
    FOR ALL ENTRIES IN t_chvw
  WHERE aufnr EQ t_chvw-aufnr.

ENDFORM.                    " Z_SELECIONA_AUFK

*&---------------------------------------------------------------------*
*&      Form  Z_EXCLUI_BOTS                                            *
*&---------------------------------------------------------------------*
*                               Exclui Botoes                          *
*----------------------------------------------------------------------*
FORM z_exclui_bots USING p_botao TYPE c.

  DATA sl_tool TYPE ui_func.

  sl_tool = p_botao.

  APPEND sl_tool TO t_tool.

ENDFORM.                    " Z_EXCLUI_BOTS

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_INDEX                                         *
*&---------------------------------------------------------------------*
*                             Verifica Index                           *
*----------------------------------------------------------------------*
FORM z_verifica_index CHANGING p_cont  TYPE i
                               p_index TYPE numc2.

  IF p_cont EQ 41.
    ADD 1 TO p_index.
    p_cont = 1.
  ENDIF.

ENDFORM.                    " Z_VERIFICA_INDEX
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM z_seleciona_doc.

*  select mkpf~mblnr mkpf~mjahr "(g_t_fields)
*      into table t_mkpf_aux
*  from mkpf
*    inner join mseg on mkpf~mandt = mseg~mandt
*                   and mkpf~mblnr = mseg~mblnr
*                   and mkpf~mjahr = mseg~mjahr
*  where mkpf~budat in s_budat
**    and mseg~bwart in bwart
**    and mseg~charg in charg
**    and mseg~kunnr in kunnr
*    and mseg~lgort in s_lgort
**    and mseg~lifnr in lifnr
*    and mseg~matnr in s_matnr
**    and mseg~sobkz in sobkz
**    and mkpf~usnam in usnam
**    and mkpf~vgart in vgart
*    and mseg~werks in s_werks.
*    and mkpf~xblnr in xblnr
ENDFORM.                    " Z_SELECIONA_DOC
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MCHB_FILTRO
*&---------------------------------------------------------------------*
FORM z_seleciona_mchb_fill_safra.
  DATA: sl_mchb        TYPE type_mchb,
        tl_num_filtro  TYPE TABLE OF bapi1003_alloc_values_num,
        sl_num_filtro  TYPE bapi1003_alloc_values_num,
        tl_char_filtro TYPE TABLE OF bapi1003_alloc_values_char,
        sl_char_filtro TYPE bapi1003_alloc_values_char,
        tl_curr_filtro TYPE TABLE OF bapi1003_alloc_values_curr,
        tl_ret_filtro  TYPE TABLE OF bapiret2,

        vl_key         TYPE bapi1003_key-object,
        sl_inob_filtro TYPE type_inob,
        sl_kssk_filtro TYPE type_kssk,
        sl_klah_filtro TYPE type_klah,
        vl_num         TYPE bapi1003_key-classnum,
        safra          TYPE numc4,
        vl_index       TYPE i.

  "Projeto Reestruturação Algodao 2024 - Ini
*  SELECT matnr werks lgort charg clabs cspem
*  FROM mchb
*    INTO TABLE t_mchb
*  WHERE matnr IN s_matnr
*    AND werks IN s_werks
*    AND lgort IN s_lgort
*    AND ( clabs > 0 OR cspem > 0 ).
  "Projeto Reestruturação Algodao 2024 - FIM


  LOOP AT t_mchb INTO sl_mchb.
    vl_index = sy-tabix.

    READ TABLE t_inob INTO sl_inob_filtro WITH KEY objek = sl_mchb-matnr
                                          BINARY SEARCH.

    READ TABLE t_kssk INTO sl_kssk_filtro WITH KEY objek = sl_inob_filtro-objekk
                                          BINARY SEARCH.

    READ TABLE t_klah INTO sl_klah_filtro WITH KEY clint = sl_kssk_filtro-clint
                                          BINARY SEARCH.

    vl_num  = sl_klah_filtro-class.

    CONCATENATE sl_mchb-matnr sl_mchb-charg INTO vl_key.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = vl_key
        objecttable     = 'MCH1'
        classnum        = vl_num
        classtype       = st_rmclm-klart
      TABLES
        allocvaluesnum  = tl_num_filtro
        allocvalueschar = tl_char_filtro
        allocvaluescurr = tl_curr_filtro
        return          = tl_ret_filtro.

    IF ( sy-subrc EQ 0 ).
** COMMENT - 20.06.2013
**      READ TABLE tl_num_filtro INTO sl_num_filtro WITH KEY charact_descr = 'Safra'.
**
**      safra = sl_num_filtro-value_from.

** ADD - 20.06.2013 - Inicio
      SORT tl_char_filtro BY charact_descr ASCENDING.
      READ TABLE tl_char_filtro INTO sl_char_filtro WITH KEY charact_descr = 'Safra' BINARY SEARCH.
      safra = sl_char_filtro-value_char.
** ADD - 20.06.2013 - Fim

      IF NOT ( safra IS INITIAL ).
        sl_mchb-safra = safra.
        MODIFY t_mchb FROM sl_mchb INDEX vl_index TRANSPORTING safra.
      ENDIF.
    ENDIF.

    CLEAR: sl_mchb, safra, sl_inob_filtro, sl_kssk_filtro, sl_klah_filtro.
  ENDLOOP.

  DELETE T_MCHB WHERE SAFRA NE P_SAFRA.

ENDFORM.                    " Z_SELECIONA_MCHB_FILTRO

***************************************************
* posiciona tela no 1o bloco
***************************************************
FORM f_posicao_tela  USING p_index.

  LOOP AT SCREEN.
    IF screen-name = 'BTN_LEFT'  OR
       screen-name = 'BTN_TOTAL_LEFT'.
      IF l_index_frame = 1.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
    ENDIF.

    IF screen-name = 'BTN_RIGHT' OR
       screen-name = 'BTN_TOTAL_RIGHT'.
      IF l_index_frame = l_total_frame.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

  l_index_frame = p_index.
  t_alv[]       = t_alv_ger[].

  DESCRIBE TABLE t_dados_ger LINES l_total_frame.

  READ TABLE t_dados_ger INTO w_dados_ger INDEX l_index_frame.

  DELETE t_alv WHERE lgort <> w_dados_ger-w_cabec-deposito.

  SELECT SINGLE werks lgort lgobe
    FROM t001l
    INTO st_t001l
   WHERE werks  = w_dados_ger-w_cabec-centro
     AND lgort  = w_dados_ger-w_cabec-deposito.

  READ TABLE t_cabec_ger INTO st_cabec WITH KEY lgort = w_dados_ger-w_cabec-deposito.

ENDFORM.

***************************************************
* posiciona tela no 1o bloco
***************************************************
FORM f_navegar_tela    USING p_ucomm
                    CHANGING p_index.

  CASE p_ucomm.
    WHEN 'BTN_TOTAL_LEFT'.
      p_index = 1.
    WHEN 'BTN_TOTAL_RIGHT'.
      p_index = l_total_frame.
    WHEN 'BTN_LEFT'.
      CHECK p_index > 1.
      p_index = p_index - 1.
    WHEN 'BTN_RIGHT'.
      CHECK p_index < l_total_frame.
      p_index = p_index + 1.
  ENDCASE.

ENDFORM.
