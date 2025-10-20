*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 17/03/2021                                              &*
*& Descrição: "tempo de faturamento" na Fábrica Comodoro              &*
*& Transação:  ZLES0193                                               &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zlesr0151.

TABLES: zsdt0001.

*---------------------------------------------------------------------*
* TYPES                                                               *
*---------------------------------------------------------------------*
TYPE-POOLS: slis, icon.

TYPES: BEGIN OF ty_zsdt0001,
         bukrs         TYPE zsdt0001-bukrs,
         branch        TYPE zsdt0001-branch,
         dt_movimento  TYPE zsdt0001-dt_movimento,
         nr_romaneio   TYPE zsdt0001-nr_romaneio,
         tp_frete      TYPE zsdt0001-tp_frete,
         placa_cav     TYPE zsdt0001-placa_cav,
         matnr         TYPE zsdt0001-matnr,
         peso_liq      TYPE zsdt0001-peso_liq,
         hr_fechamento TYPE zsdt0001-hr_fechamento,
         doc_transp    TYPE zsdt0001-doc_transp,
         ch_referencia TYPE likp-xblnr,
         fatura_prod   TYPE j_1bnflin-refkey,
         fatura_frete  TYPE zsdt0001-fatura_frete, "j_1bnflin-refkey,
         spart         TYPE vbak-spart,
       END OF ty_zsdt0001.

TYPES: BEGIN OF ty_j_1bnfdoc,
         docnum TYPE j_1bnfdoc-docnum,
         docdat TYPE j_1bnfdoc-docdat,
         cretim TYPE j_1bnfdoc-cretim,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_j_1bnfdoc.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida,
         bukrs         TYPE zsdt0001-bukrs,
         branch        TYPE zsdt0001-branch,
         dt_movimento  TYPE zsdt0001-dt_movimento,
         nr_romaneio   TYPE zsdt0001-nr_romaneio,
         tp_frete      TYPE zsdt0001-tp_frete,
         placa_cav     TYPE zsdt0001-placa_cav,
         matnr(58)     TYPE c,
         mtart         TYPE mara-mtart,
         mtbez         TYPE t134t-mtbez,
         peso_liq      TYPE zsdt0001-peso_liq,
         hr_roma       TYPE zsdt0001-hr_fechamento,
         hr_rem        TYPE zsdt0001-hr_fechamento, "j_1bnfdoc-cretim,
         hr_emi        TYPE zsdt0001-hr_fechamento, "vttk-erzet,
         hr_doc        TYPE zsdt0001-hr_fechamento, "vttk-erzet,
         hr_emidac     TYPE zsdt0001-hr_fechamento, "j_1bnfdoc-cretim,
         hr_fechamento TYPE zsdt0001-hr_fechamento,
         usuario       TYPE zsdt0102-usuario.
TYPES: END OF ty_saida.

*---------------------------------------------------------------------*
* DATA                                                                *
*---------------------------------------------------------------------*
DATA: git_zsdt0001_aux TYPE TABLE OF zsdt0001,
      git_zsdt0001     TYPE TABLE OF ty_zsdt0001 WITH HEADER LINE,
      git_likp         TYPE TABLE OF likp,
      git_makt         TYPE TABLE OF makt,
      git_j_1bnflin    TYPE TABLE OF j_1bnflin,
      git_j_1bnfdoc    TYPE TABLE OF ty_j_1bnfdoc WITH HEADER LINE,
      git_vttk         TYPE TABLE OF vttk,
      git_j_1bnflin_f  TYPE TABLE OF j_1bnflin,
      git_j_1bnfdoc_f  TYPE TABLE OF ty_j_1bnfdoc WITH HEADER LINE,
      git_zsdt0102     TYPE TABLE OF zsdt0102,
      git_zsdt0105     TYPE TABLE OF zsdt0105,
      git_saida        TYPE TABLE OF ty_saida,
      git_filtro       TYPE zif_screen_linha_filtro_t,
      git_fcat         TYPE lvc_t_fcat,
      git_mara         TYPE TABLE OF mara,
      git_t134t        TYPE TABLE OF t134t,
      git_vbak         TYPE TABLE OF vbak.

DATA: gwa_zsdt0001     TYPE ty_zsdt0001,
      gwa_zsdt0001_aux TYPE zsdt0001,
      gwa_likp         TYPE likp,
      gwa_makt         TYPE makt,
      gwa_j_1bnflin    TYPE j_1bnflin,
      gwa_j_1bnfdoc    TYPE ty_j_1bnfdoc,
      gwa_vttk         TYPE vttk,
      gwa_j_1bnflin_f  TYPE j_1bnflin,
      gwa_j_1bnfdoc_f  TYPE ty_j_1bnfdoc,
      gwa_zsdt0102     TYPE zsdt0102,
      gwa_zsdt0105     TYPE zsdt0105,
      gwa_saida        TYPE ty_saida,
      gwa_estrutura    TYPE ty_estrutura,
      gwa_mara         TYPE mara,
      gwa_t134t        TYPE t134t,
      gwa_vbak         TYPE vbak.


DATA: gva_append(1) TYPE c.
*---------------------------------------------------------------------*
* RANGES                                                              *
*---------------------------------------------------------------------*
RANGES: rg_fatura_prod  FOR j_1bnflin-refkey,
        rg_fatura_frete FOR zsdt0001-fatura_frete, "j_1bnflin-refkey,
        rg_doc_transp   FOR zsdt0001-doc_transp.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid.

*----------------------------------------------------------------------*
* Tela de seleção *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: pbukrs   FOR zsdt0001-bukrs OBLIGATORY NO INTERVALS NO-EXTENSION,
                pbranch  FOR zsdt0001-branch OBLIGATORY NO INTERVALS NO-EXTENSION,
                pdtmov   FOR zsdt0001-dt_movimento OBLIGATORY NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.

*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_seleciona_dados .
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM fm_seleciona_dados .

  REFRESH: rg_fatura_prod,
           rg_fatura_frete,
           rg_doc_transp.

  DATA: BEGIN OF lit_tab OCCURS 0,
          bukrs TYPE t001k-bukrs,
          bwkey TYPE t001k-bwkey,
          werks TYPE t001w-werks,
        END OF lit_tab.

  SELECT t001k~bukrs t001k~bwkey t001w~werks
      INTO CORRESPONDING FIELDS OF TABLE lit_tab
         FROM t001w
               INNER JOIN t001k
                   ON t001k~bwkey = t001w~bwkey
    WHERE bukrs IN pbukrs
         AND werks IN pbranch.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s000(z_les) WITH 'Filial não pertence a essa empresa'(004) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SELECT * INTO TABLE git_zsdt0001_aux
    FROM zsdt0001
   WHERE bukrs        IN pbukrs
     AND branch       IN pbranch
     AND dt_movimento IN pdtmov
     and TP_MOVIMENTO EQ 'S'.

  SELECT *
    FROM vbak
    INTO TABLE git_vbak
    FOR ALL ENTRIES IN  git_zsdt0001_aux
   WHERE vbeln EQ  git_zsdt0001_aux-vbeln.


  IF git_zsdt0001_aux IS NOT INITIAL.

    LOOP AT git_zsdt0001_aux INTO gwa_zsdt0001_aux.

      gwa_zsdt0001-bukrs          = gwa_zsdt0001_aux-bukrs.
      gwa_zsdt0001-branch         = gwa_zsdt0001_aux-branch.
      gwa_zsdt0001-dt_movimento   = gwa_zsdt0001_aux-dt_movimento.
      gwa_zsdt0001-nr_romaneio    = gwa_zsdt0001_aux-nr_romaneio.
      gwa_zsdt0001-tp_frete       = gwa_zsdt0001_aux-tp_frete.
      gwa_zsdt0001-placa_cav      = gwa_zsdt0001_aux-placa_cav.
      gwa_zsdt0001-matnr          = gwa_zsdt0001_aux-matnr.
      gwa_zsdt0001-peso_liq       = gwa_zsdt0001_aux-peso_liq.
      gwa_zsdt0001-hr_fechamento  = gwa_zsdt0001_aux-hr_fechamento.
      gwa_zsdt0001-doc_transp     = gwa_zsdt0001_aux-doc_transp.


      MOVE gwa_zsdt0001_aux-ch_referencia TO  gwa_zsdt0001-ch_referencia.
      MOVE gwa_zsdt0001_aux-fatura_prod   TO  gwa_zsdt0001-fatura_prod.


      IF gwa_zsdt0001_aux-fatura_prod IS NOT INITIAL.
        rg_fatura_prod-sign = 'I'.
        rg_fatura_prod-option = 'EQ'.
        rg_fatura_prod-low  =  gwa_zsdt0001_aux-fatura_prod.
        APPEND rg_fatura_prod.
      ENDIF.

      IF gwa_zsdt0001_aux-fatura_frete IS NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gwa_zsdt0001_aux-fatura_frete
          IMPORTING
            output = gwa_zsdt0001-fatura_frete.

        rg_fatura_frete-sign = 'I'.
        rg_fatura_frete-option = 'EQ'.
        rg_fatura_frete-low  =  gwa_zsdt0001-fatura_frete.
        APPEND rg_fatura_frete.
      ENDIF.


      IF gwa_zsdt0001_aux-doc_transp IS NOT INITIAL.
        rg_doc_transp-sign = 'I'.
        rg_doc_transp-option = 'EQ'.
        rg_doc_transp-low  =  gwa_zsdt0001_aux-doc_transp.
        APPEND rg_doc_transp.
      ENDIF.


      READ TABLE git_vbak INTO gwa_vbak WITH KEY vbeln = gwa_zsdt0001_aux-vbeln.
      IF sy-subrc EQ 0.
        gwa_zsdt0001-spart = gwa_vbak-spart.
      ENDIF.

      APPEND gwa_zsdt0001 TO git_zsdt0001.
      CLEAR: gwa_zsdt0001,gwa_vbak.
    ENDLOOP.



    SELECT *
      FROM  likp
      INTO TABLE git_likp
       FOR ALL ENTRIES IN git_zsdt0001
     WHERE xblnr = git_zsdt0001-ch_referencia.


    SELECT *
     FROM  makt
     INTO TABLE git_makt
      FOR ALL ENTRIES IN git_zsdt0001
    WHERE matnr = git_zsdt0001-matnr
      AND spras =  'P'.

*---> 04/07/2023 - Migração S4 - WS
  SORT  git_makt BY  matnr.
*<--- 04/07/2023 - Migração S4 - WS
    DELETE ADJACENT DUPLICATES FROM git_makt COMPARING matnr.


    SELECT b~docnum b~docdat b~cretim c~refkey
      FROM j_1bnfdoc  AS  b
      INNER JOIN j_1bnflin      AS c ON c~docnum = b~docnum
      INTO CORRESPONDING FIELDS OF TABLE git_j_1bnfdoc
     WHERE  b~docnum  EQ c~docnum
      AND c~refkey  IN rg_fatura_prod.

    SELECT *
      FROM  vttk
      INTO TABLE git_vttk
     WHERE tknum  IN rg_doc_transp.

    SELECT b~docnum b~docdat b~cretim c~refkey
      FROM j_1bnfdoc  AS  b
      INNER JOIN j_1bnflin      AS c ON c~docnum = b~docnum
      INTO CORRESPONDING FIELDS OF TABLE git_j_1bnfdoc_f
     WHERE  b~docnum  EQ c~docnum
      AND c~refkey  IN rg_fatura_frete.


    SELECT *
    FROM  zsdt0105
    INTO TABLE git_zsdt0105
     FOR ALL ENTRIES IN git_j_1bnfdoc_f
    WHERE docnum = git_j_1bnfdoc_f-docnum.

    SELECT *
    FROM  zsdt0102
    INTO TABLE git_zsdt0102
     FOR ALL ENTRIES IN git_zsdt0105
    WHERE nmdfe = git_zsdt0105-nmdfe
     AND docnum = git_zsdt0105-docnum_ref .


    SELECT *
      FROM mara
      INTO TABLE git_mara
      FOR ALL ENTRIES IN git_zsdt0001
      WHERE matnr EQ git_zsdt0001-matnr.

    SELECT *
      FROM t134t
      INTO TABLE git_t134t
      FOR ALL ENTRIES IN git_mara
     WHERE spras EQ sy-langu
     AND   mtart EQ git_mara-mtart.


*    SELECT *
*      FROM  zsdt0102
*      INTO TABLE git_zsdt0102
*       FOR ALL ENTRIES IN git_j_1bnfdoc_f
*     WHERE docnum = git_j_1bnfdoc_f-docnum.

    PERFORM fm_organiza_dados.

  ELSE.
    MESSAGE s000(z_les) WITH 'Dados não encontrados'(002) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM fm_organiza_dados .

  LOOP AT git_zsdt0001 INTO gwa_zsdt0001.
    CLEAR: gva_append.
    gwa_saida-bukrs         = gwa_zsdt0001-bukrs.
    gwa_saida-branch        = gwa_zsdt0001-branch.
    gwa_saida-dt_movimento  = gwa_zsdt0001-dt_movimento.
    gwa_saida-nr_romaneio   = gwa_zsdt0001-nr_romaneio.
    gwa_saida-tp_frete      = gwa_zsdt0001-tp_frete.
    gwa_saida-placa_cav     = gwa_zsdt0001-placa_cav.

    READ TABLE git_makt INTO gwa_makt WITH KEY matnr = gwa_zsdt0001-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gwa_makt-matnr
      IMPORTING
        output = gwa_makt-matnr.


    CONCATENATE gwa_makt-matnr '-' gwa_makt-maktx INTO gwa_saida-matnr  SEPARATED BY space.

    gwa_saida-peso_liq      = gwa_zsdt0001-peso_liq.
    gwa_saida-hr_roma       = gwa_zsdt0001-hr_fechamento.


    READ TABLE git_mara INTO gwa_mara WITH KEY matnr = gwa_zsdt0001-matnr.

    READ TABLE git_t134t INTO gwa_t134t WITH KEY mtart = gwa_mara-mtart.
    IF sy-subrc EQ 0.
      gwa_saida-mtart = gwa_t134t-mtart.
      gwa_saida-mtbez = gwa_t134t-mtbez .
      TRANSLATE gwa_saida-mtbez to UPPER CASE.
    ENDIF.

    READ TABLE git_likp INTO gwa_likp WITH KEY xblnr = gwa_zsdt0001-ch_referencia.
    IF sy-subrc = 0.
      IF gwa_zsdt0001-hr_fechamento IS NOT INITIAL.
        "Regra Hora Romaneio x Hora Remessa: ZSDT0001-HR_FECHAMENTO - LIKP-ERZET
        gva_append = 'X'.
        CALL FUNCTION 'SCOV_TIME_DIFF'
          EXPORTING
            im_date1              = gwa_zsdt0001-dt_movimento
            im_date2              = gwa_likp-erdat
            im_time1              = gwa_zsdt0001-hr_fechamento
            im_time2              = gwa_likp-erzet
          IMPORTING
            ex_time               = gwa_saida-hr_roma
          EXCEPTIONS
            start_larger_than_end = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE git_j_1bnfdoc INTO gwa_j_1bnfdoc WITH KEY  refkey  = gwa_zsdt0001-fatura_prod.
    IF sy-subrc = 0.
      IF gwa_likp-erzet IS NOT INITIAL.
        "Regra Hora Remessa x Hora emissão NF:  LIKP-ERZET - J_1BNFDOC-CRETIM
        gva_append = 'X'.
        CALL FUNCTION 'SCOV_TIME_DIFF'
          EXPORTING
            im_date1              = gwa_likp-erdat
            im_date2              = gwa_j_1bnfdoc-docdat
            im_time1              = gwa_likp-erzet
            im_time2              = gwa_j_1bnfdoc-cretim
          IMPORTING
            ex_time               = gwa_saida-hr_rem
          EXCEPTIONS
            start_larger_than_end = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE git_vttk INTO gwa_vttk WITH KEY tknum  = gwa_zsdt0001-doc_transp.
    IF sy-subrc = 0.
      IF gwa_j_1bnfdoc-cretim IS NOT INITIAL.
        "Regra Hora Emissão NF x Hora Doc. Transporte:  J_1BNFDOC-CRETIM -   VTTK-ERZET
        gva_append = 'X'.
        CALL FUNCTION 'SCOV_TIME_DIFF'
          EXPORTING
            im_date1              = gwa_j_1bnfdoc-docdat
            im_date2              = gwa_vttk-erdat
            im_time1              = gwa_j_1bnfdoc-cretim
            im_time2              = gwa_vttk-erzet
          IMPORTING
            ex_time               = gwa_saida-hr_emi
          EXCEPTIONS
            start_larger_than_end = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE git_j_1bnfdoc_f INTO gwa_j_1bnfdoc_f WITH KEY  refkey  = gwa_zsdt0001-fatura_frete.
    IF sy-subrc = 0.
      IF gwa_vttk-erzet IS NOT INITIAL.
        "Regra Hora Doc. Transporte x Hora Emissão DACTE:  VTTK-ERZET -  J_1BNFDOC-CRETIM
        gva_append = 'X'.
        CALL FUNCTION 'SCOV_TIME_DIFF'
          EXPORTING
            im_date1              = gwa_vttk-erdat
            im_date2              = gwa_j_1bnfdoc_f-docdat
            im_time1              = gwa_vttk-erzet
            im_time2              = gwa_j_1bnfdoc_f-cretim
          IMPORTING
            ex_time               = gwa_saida-hr_doc
          EXCEPTIONS
            start_larger_than_end = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE git_zsdt0105 INTO gwa_zsdt0105 WITH KEY docnum = gwa_j_1bnfdoc_f-docnum.
    READ TABLE git_zsdt0102 INTO gwa_zsdt0102 WITH KEY nmdfe  = gwa_zsdt0105-nmdfe
                                                       docnum = gwa_zsdt0105-docnum_ref.
    IF sy-subrc = 0.
      gwa_saida-usuario =  gwa_zsdt0102-usuario.
      IF gwa_j_1bnfdoc-docdat IS NOT INITIAL.
        gva_append = 'X'.
        CALL FUNCTION 'SCOV_TIME_DIFF'
          EXPORTING
            im_date1              = gwa_j_1bnfdoc-docdat
            im_date2              = gwa_zsdt0102-data_emi
            im_time1              = gwa_j_1bnfdoc-cretim
            im_time2              = gwa_zsdt0102-hora_emi
          IMPORTING
            ex_time               = gwa_saida-hr_emidac
          EXCEPTIONS
            start_larger_than_end = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
        ENDIF.
      ENDIF.

      IF gwa_zsdt0001-hr_fechamento IS NOT INITIAL.
        gva_append = 'X'.
        CALL FUNCTION 'SCOV_TIME_DIFF'
          EXPORTING
            im_date1              = gwa_zsdt0001-dt_movimento
            im_date2              = gwa_zsdt0102-data_emi
            im_time1              = gwa_zsdt0001-hr_fechamento
            im_time2              = gwa_zsdt0102-hora_emi
          IMPORTING
            ex_time               = gwa_saida-hr_fechamento
          EXCEPTIONS
            start_larger_than_end = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gva_append = 'X'.
      APPEND gwa_saida TO git_saida.
    ENDIF.

    CLEAR:gwa_saida,
          gwa_likp,
          gwa_j_1bnfdoc,
          gwa_vttk,
          gwa_j_1bnfdoc_f,
          gwa_zsdt0102,
          gwa_zsdt0001,
          gwa_mara,
          gwa_t134t.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_end_of_selection .
  PERFORM fm_filtros.
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
FORM fm_filtros .

  DATA vl_text TYPE TABLE OF textpool.

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    TABLES
      tpool      = vl_text.

  FREE: git_filtro.

  LOOP AT SCREEN.
    git_filtro = VALUE #(
      ( parametro = '' valor = pbukrs )
*      ( PARAMETRO = '' VALOR = P_WERKS )
    ).
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos .
  DATA: lwa_bukrs(50)   TYPE c,
        lwa_butxt       TYPE butxt,
        lwa_space       VALUE '-',
        lwa_layout2(50) TYPE c,
        lwa_layout4(3)  VALUE 'até',
        lwa_low         LIKE pdtmov,
        lwa_high        LIKE pdtmov,
        lwa_data        VALUE '.'.

  PERFORM fm_cria_fieldcat.


  SELECT SINGLE butxt
    INTO lwa_butxt FROM t001
       WHERE bukrs IN pbukrs.


  CONCATENATE pbukrs+3(4) lwa_space lwa_butxt  INTO lwa_bukrs SEPARATED BY space.


  IF pdtmov-low IS NOT INITIAL  AND pdtmov-high IS NOT INITIAL.
    CONCATENATE pdtmov-low+6(2)  lwa_data pdtmov-low+4(2)  lwa_data pdtmov-low(4)  INTO lwa_low.
    CONCATENATE pdtmov-high+6(2) lwa_data pdtmov-high+4(2) lwa_data pdtmov-high(4) INTO lwa_high.
    CONCATENATE   lwa_low lwa_layout4 lwa_high  INTO lwa_layout2 SEPARATED BY space.

  ELSEIF pdtmov-low IS NOT INITIAL.
    CONCATENATE pdtmov-low+6(2)  lwa_data pdtmov-low+4(2)  lwa_data pdtmov-low(4)  INTO lwa_low.
    MOVE  lwa_low TO lwa_layout2.
  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Tempo de Faturamento de Cargas - Fábrica Comodoro'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Empresa' valor = lwa_bukrs )
     ( parametro = 'Período' valor = lwa_layout2 ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      CHANGING
        it_outtab                     = git_saida
        it_fieldcatalog               = git_fcat
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
FORM fm_cria_fieldcat .

  git_fcat =  VALUE lvc_t_fcat(
    ( fieldname = 'BUKRS'              coltext =  'Empresa'                                      outputlen = '08' )
    ( fieldname = 'BRANCH'             coltext =  'Filial'                                       outputlen = '08' )
    ( fieldname = 'DT_MOVIMENTO'       coltext =  'Dt Movimento'                                 outputlen = '12' )
    ( fieldname = 'NR_ROMANEIO'        coltext =  'Romaneio'                                     outputlen = '15' )
    ( fieldname = 'TP_FRETE'           coltext =  'Tipo Frete'                                   outputlen = '10' )
    ( fieldname = 'PLACA_CAV'          coltext =  'Placa'                                        outputlen = '10' )
    ( fieldname = 'MATNR'              coltext =  'Produto'                                      outputlen = '68' )
    ( fieldname = 'MTART'              coltext =  'Tipo Material'                                outputlen = '13' )
    ( fieldname = 'MTBEZ'              coltext =  'Desc.Tipo Material'                           outputlen = '25' )
    ( fieldname = 'PESO_LIQ'           coltext =  'Peso liquido'                                 outputlen = '15' )
    ( fieldname = 'HR_ROMA'            coltext =  'Hora Romaneio x Hora Remessa'                 outputlen = '40' )
    ( fieldname = 'HR_REM'             coltext =  'Hora Remessa x Hora emissão NF'               outputlen = '40' )
    ( fieldname = 'HR_EMI'             coltext =  'Hora Emissão NF x Hora Doc. Transporte'       outputlen = '40' )
    ( fieldname = 'HR_DOC'             coltext =  'Hora Doc. Trans. x Hora Emissão DACTE'        outputlen = '40' )
    ( fieldname = 'HR_EMIDAC'          coltext =  'Hora Emissão DACTE x Hora Emissão MDF-e'      outputlen = '40' )
    ( fieldname = 'HR_FECHAMENTO'      coltext =  'Hora Romaneio x Hora Emissão MDF-e'           outputlen = '40' )
    ( fieldname = 'USUARIO'            coltext =  'Usuário'                                      outputlen = '15' )
      ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  FM_STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fm_status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100' WITH 'Tempo de Faturamento de Cargas'.
  PERFORM fm_criar_objetos.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FM_USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fm_user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FM_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fm_user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
