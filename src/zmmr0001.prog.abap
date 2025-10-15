*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZMMI0001                                                *
* Descrição  : Classificação Kuhlmann                                  *
* Módulo     : MM                                Transação: ZMM0025    *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 28/06/2011 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      : Igor Sobral                            Data: 20/06/2013 *
* Observações: Busca de Safra                                          *
*----------------------------------------------------------------------*
REPORT zmmr0001 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES: mseg, mkpf, coas.

*******************************************************************************************
* classes / btree
*******************************************************************************************
CLASS cl_gui_column_tree     DEFINITION LOAD.
CLASS cl_gui_cfw             DEFINITION LOAD.

DATA: tree1               TYPE REF TO cl_hrpayna_gui_alv_tree, "cl_gui_alv_tree.
      mr_toolbar          TYPE REF TO cl_gui_toolbar,
      g_container         TYPE scrfname VALUE 'CONTAINER',
      g_container2        TYPE scrfname VALUE 'CONTAINER2',
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      g_custom_container2 TYPE REF TO cl_gui_custom_container,
      g_grid              TYPE REF TO cl_gui_alv_grid,
      g_grid2             TYPE REF TO cl_gui_alv_grid,
      w_tool              TYPE stb_button,
      t_fieldcatalog      TYPE lvc_t_fcat, "Fieldcatalog
      t_exctab            TYPE slis_t_extab,
      w_item_layout       TYPE lvc_s_laci,
      w_layout            TYPE lvc_s_layo,
      ls_fieldcatalog     TYPE lvc_s_fcat,
      ls_exclude          TYPE ui_func,
      pt_exclude          TYPE ui_functions,
      pt_exclude2         TYPE ui_functions.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_mkpf,
         mblnr TYPE mkpf-mblnr,
         mjahr TYPE mkpf-mjahr,
         budat TYPE mkpf-budat,
       END OF type_mkpf,

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
         matnr TYPE mchb-matnr,
         werks TYPE mchb-werks,
         lgort TYPE mchb-lgort,
         charg TYPE mchb-charg,
         objek TYPE inob-objek,
       END OF type_mchb,

       BEGIN OF type_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
       END OF type_t001,

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

       BEGIN OF ty_ztsafrafardos,
         mandt       TYPE ztsafrafardos-mandt,
         charg       TYPE ztsafrafardos-charg,
         data_inicio TYPE ztsafrafardos-data_inicio,
         data_fim    TYPE ztsafrafardos-data_fim,
         status      TYPE ztsafrafardos-status,
       END OF ty_ztsafrafardos.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_mkpf           TYPE TABLE OF type_mkpf,
      t_mkpfpp         TYPE TABLE OF type_mkpf,
      t_mseg           TYPE TABLE OF type_mseg WITH HEADER LINE,
      t_mseg_aux       TYPE TABLE OF type_mseg,
      t_mseg_ger       TYPE TABLE OF type_mseg WITH HEADER LINE,
      t_312            TYPE TABLE OF type_mseg,
      t_132            TYPE TABLE OF type_mseg,
      t_310            TYPE TABLE OF type_mseg,

      t_msegpp         TYPE TABLE OF type_mseg WITH HEADER LINE,
      t_mchb           TYPE TABLE OF type_mchb,
      t_aufk           TYPE TABLE OF type_aufk,
      t_inob           TYPE TABLE OF type_inob,
      t_kssk           TYPE TABLE OF type_kssk,
      t_klah           TYPE TABLE OF type_klah,
      t_t001           TYPE TABLE OF type_t001,
      t_msg            TYPE TABLE OF type_msg,
      t_mara           TYPE TABLE OF type_mara,
      it_mchb_atual    TYPE TABLE OF mchb      WITH HEADER LINE,
      it_mseg_atual    TYPE TABLE OF mseg      WITH HEADER LINE,
      st_rmclm         TYPE type_rmclm,
      st_t001l         TYPE type_t001l,
      it_ztsafrafardos TYPE TABLE OF ty_ztsafrafardos,
      wa_ztsafrafardos TYPE ty_ztsafrafardos,

      it_zmmt0027      TYPE TABLE OF zmmt0027,   "ADD - 20.06.2013
      wa_zmmt0027      TYPE zmmt0027,            "ADD - 20.06.2013
      it_zmmt0027_ger  TYPE TABLE OF zmmt0027,   "ADD - 20.06.2013
      wa_zmmt0027_ger  TYPE zmmt0027,            "ADD - 20.06.2013
      tl_saida_tot     TYPE TABLE OF zmmr0001,
      sl_saida_tot     TYPE zmmr0001,
      tl_saida_ger     TYPE TABLE OF zmmr0001,
      sl_saida_ger     TYPE zmmr0001.

DATA: g_excel_sem_popup TYPE c,
      t_saida           TYPE TABLE OF zmmr0001,
      w_saida           TYPE zmmr0001,
      ok_code           TYPE sy-ucomm.

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
DATA: vg_name1      TYPE t001w-name1,
      vg_bukrs      TYPE t001k-bukrs,
      vg_cod        TYPE ztsafrafardos-matnr, "ADD - 25.08.2016
      l_pdf_ant     TYPE c,
      l_exc_ant     TYPE c,
      t_fcat_lvc    TYPE TABLE OF lvc_s_fcat,
      w_fcat_lvc    TYPE lvc_s_fcat,
      t_fcat_kkb    TYPE kkblo_t_fieldcat,
      t_prod_ninter TYPE TABLE OF zmmt0027,
      w_prod_ninter TYPE zmmt0027.

RANGES: r_cpint  FOR lfa1-lifnr.

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_311 TYPE mseg-bwart VALUE '311',
           c_312 TYPE mseg-bwart VALUE '312',
           c_131 TYPE mseg-bwart VALUE '131',
           c_132 TYPE mseg-bwart VALUE '132',
           c_309 TYPE mseg-bwart VALUE '309',
           c_310 TYPE mseg-bwart VALUE '310',
           c_ws  TYPE mkpf-vgart VALUE 'WS',
           c_s   TYPE char1      VALUE 'S',
           c_sh  TYPE char1      VALUE 'H'.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE text-005.
PARAMETERS: c_atual TYPE c RADIOBUTTON GROUP gr DEFAULT 'X' USER-COMMAND us1,
            c_histo TYPE c RADIOBUTTON GROUP gr.
SELECTION-SCREEN END   OF BLOCK a2.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
  s_werks  FOR mseg-werks NO INTERVALS NO-EXTENSION, " OBLIGATORY,
*-CS2021000809 - 08.12.2021 - JT - inicio
  s_lgort  FOR mseg-lgort, " OBLIGATORY, "NO INTERVALS NO-EXTENSION OBLIGATORY,
  s_matnr  FOR mseg-matnr NO INTERVALS NO-EXTENSION. " OBLIGATORY.
*-CS2021000809 - 08.12.2021 - JT - fim
"S_BUDAT  FOR MKPF-BUDAT NO-EXTENSION OBLIGATORY
"  s_aufex  for coas-aufex no intervals no-extension.
PARAMETERS:
  p_safra TYPE mseg-charg, "   OBLIGATORY.
  p_cpint TYPE mseg-lifnr. "   OBLIGATORY.   "*-CS2022000332-#82301-01.08.2022-JT-inicio
SELECTION-SCREEN END   OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME TITLE text-006.
PARAMETERS: p_exc    AS CHECKBOX                USER-COMMAND tipo_said DEFAULT 'X',
            p_pdf    AS CHECKBOX                USER-COMMAND tipo_said,
            p_retrat TYPE c RADIOBUTTON GROUP gr2 USER-COMMAND muda_tela DEFAULT 'X',
            p_paisag TYPE c RADIOBUTTON GROUP gr2.
SELECTION-SCREEN END OF BLOCK a3.

SELECTION-SCREEN BEGIN OF BLOCK a4 WITH FRAME TITLE text-007.
PARAMETERS: ch_elg  TYPE c AS CHECKBOX MODIF ID m1 DEFAULT 'X',
            ch_leaf TYPE c AS CHECKBOX MODIF ID m1 DEFAULT 'X',
            ch_cg   TYPE c AS CHECKBOX MODIF ID m1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK a4.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*

  IF s_werks[] IS INITIAL OR
     s_lgort[] IS INITIAL OR
     p_safra IS INITIAL.
    MESSAGE s024(sd) WITH 'Preencher todos os campos obrigatórios' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*-CS2022000332-#82301-01.08.2022-JT-inicio
  IF c_histo = abap_true AND p_cpint IS NOT INITIAL.
    SELECT SINGLE lifnr
      INTO @DATA(_lifnr)
      FROM lfa1
     WHERE lifnr = @p_cpint.
    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH 'Código do Fornecedor informado não localizado no '
                            'cadastro de dados mestre.' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.
*-CS2022000332-#82301-01.08.2022-JT-fim

*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
  PERFORM modifica_tela.

*----------------------------------------------------------------------*
*                           Start of Selection                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  REFRESH t_msg.

* Seleciona Dados
  PERFORM: z_seleciona_dados,

* Processa Dados
           z_processa_dados.

  IF NOT t_msg[] IS INITIAL.
    CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
      TABLES
        table    = t_msg
      EXCEPTIONS
        fb_error = 1
        OTHERS   = 2.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*-FORM MODIFICA_TELA
*----------------------------------------------------------------------*

FORM modifica_tela.

  LOOP AT SCREEN.

*-CS2022000332-#82301-01.08.2022-JT-inicio
    IF screen-name CS 'P_CPINT'.
      IF c_histo = abap_false.
        screen-invisible = 1.
        screen-input = 0.
        screen-active = 0.
      ELSE.
        screen-invisible = 0.
        screen-input = 1.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
*-CS2022000332-#82301-01.08.2022-JT-fim

*-CS2021000809 - 08.12.2021 - JT - inicio
    IF screen-name CS 'WERKS'       OR
       screen-name  = 'S_LGORT-LOW' OR
       screen-name CS 'SAFRA'.
      screen-required = '2'.
      MODIFY SCREEN.
    ENDIF.

    IF p_pdf = abap_true AND p_exc = abap_true.
      IF l_pdf_ant = abap_true.
        p_exc = abap_true.
        p_pdf = abap_false.
      ELSE.
        p_exc = abap_false.
        p_pdf = abap_true.
      ENDIF.
    ENDIF.

    IF p_pdf = abap_false.
      p_exc = abap_true.
      IF screen-name CS 'P_RETRAT' OR
         screen-name CS 'P_PAISAG'.
        screen-invisible = 1.
        screen-input = 0.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'M1'.
        screen-invisible = 0.
        screen-input = 1.
        screen-active = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ELSE.
      p_exc = abap_false.
      IF screen-name CS 'P_RETRAT' OR
         screen-name CS 'P_PAISAG'.
        screen-invisible = 0.
        screen-input = 1.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
*-CS2021000809 - 08.12.2021 - JT - fim

    IF p_retrat = 'X'.

      IF screen-group1 EQ 'M1'.
        screen-invisible = 1.
        screen-input = 0.
        screen-active = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ELSEIF p_paisag = 'X' OR p_exc = abap_true.

      IF screen-group1 EQ 'M1'.
        screen-invisible = 0.
        screen-input = 1.
        screen-active = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDIF.

  ENDLOOP.

  l_exc_ant = p_exc.
  l_pdf_ant = p_pdf.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                             Seleciona Dados                          *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona MKPF
  IF NOT c_atual IS INITIAL.
    PERFORM: z_seleciona_mchb_atual,
* Seleciona MSEG
             z_seleciona_mseg_atual.
  ELSE.
** COMMENT - 20.06.2013
**    PERFORM: "Z_SELECIONA_MKPF,
*** Seleciona MSEG
**             z_seleciona_mseg,
*** Dados PP
**             z_seleciona_pp.

* Seleciona dados historicos ZMMT0027
    PERFORM: z_seleciona_zmmt0027,
             z_seleciona_t001l.
  ENDIF.

  IF NOT c_atual IS INITIAL.
    PERFORM:
* Seleciona AUFK
             "Z_SELECIONA_AUFK,

* Seleciona MCHB
             z_seleciona_mchb,

* Seleciona Dados Características
             z_seleciona_carac,

* Seleciona T001
             z_seleciona_t001,

* Seleciona T001L
             z_seleciona_t001l,

* Seleciona Mara
             z_seleciona_mara.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MKPF                                         *
*&---------------------------------------------------------------------*
*                              Seleciona MKPF                          *
*----------------------------------------------------------------------*
FORM z_seleciona_mkpf.

  REFRESH t_mkpf.

  SELECT mandt charg data_inicio data_fim status
  FROM ztsafrafardos
    INTO TABLE it_ztsafrafardos
  WHERE charg EQ p_safra
    AND werks_from IN s_werks
    AND status EQ 'L'.


  SELECT mblnr mjahr budat
  FROM mkpf
    INTO TABLE t_mkpf
    FOR ALL ENTRIES IN it_ztsafrafardos
  WHERE ( budat >= it_ztsafrafardos-data_inicio AND budat <= it_ztsafrafardos-data_fim ).

  IF t_mkpf[] IS INITIAL.
    MESSAGE i836 WITH text-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_MKPF

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MSEG                                         *
*&---------------------------------------------------------------------*
*                              Seleciona MSEG                          *
*----------------------------------------------------------------------*
FORM z_seleciona_mseg.

  DATA: sl_mseg     TYPE type_mseg,
        sl_mseg_aux TYPE type_mseg.

  REFRESH: t_mseg, t_312.

  "CHECK NOT T_MKPF[] IS INITIAL.

  SELECT mblnr mjahr zeile bwart matnr
         werks lgort charg menge aufnr
         bukrs smbln shkzg
  FROM mseg
    INTO TABLE t_mseg
  WHERE  matnr IN s_matnr
    AND  werks IN s_werks
    AND  lgort IN s_lgort
    AND  bwart IN ('131','132','311','312','309','310').

  CLEAR: sl_mseg.
  LOOP AT t_mseg INTO sl_mseg.
    IF ( sl_mseg-bwart EQ '131' ).
      DELETE t_mseg WHERE bwart EQ '132'
                      AND charg EQ sl_mseg-charg.

      IF ( sy-subrc EQ 0 ).
        DELETE t_mseg WHERE bwart EQ '131'
                        AND charg EQ sl_mseg-charg.
      ENDIF.
    ENDIF.

    IF ( sl_mseg-bwart EQ '309' ).
      DELETE t_mseg WHERE bwart EQ '310'
                      AND charg EQ sl_mseg-charg.

      IF ( sy-subrc EQ 0 ).
        DELETE t_mseg WHERE bwart EQ '309'
                        AND charg EQ sl_mseg-charg.
      ENDIF.
    ENDIF.

    CLEAR: sl_mseg.
  ENDLOOP.

  t_312[] = t_mseg[].
  t_132[] = t_mseg[].
  t_310[] = t_mseg[].

  "delete  t_mseg where bwart ne c_311 and bwart ne c_131 and bwart ne c_132.
  DELETE  t_312  WHERE bwart NE c_312 AND smbln IS INITIAL.
  DELETE  t_132  WHERE bwart NE c_132 AND smbln IS INITIAL.
  DELETE  t_310  WHERE bwart NE c_310 AND smbln IS INITIAL.

  DELETE: "T_MSEG WHERE BWART NE C_132,
          t_mseg WHERE shkzg NE c_s.

*         T_312 WHERE BWART NE C_312,
*         T_312 WHERE SMBLN IS INITIAL,
*         T_132 WHERE BWART NE C_312,
*         T_132 WHERE SMBLN IS INITIAL.

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

  IF NOT ( t_132[] IS INITIAL ).
    LOOP AT t_132 INTO sl_mseg.
      READ TABLE t_mseg WITH KEY mblnr = sl_mseg-smbln
                                 mjahr = sl_mseg-mjahr
                        BINARY SEARCH
                        TRANSPORTING NO FIELDS.

      CHECK sy-subrc IS INITIAL.

      DELETE t_mseg INDEX sy-tabix.

      CLEAR sl_mseg.
    ENDLOOP.
  ENDIF.

  IF NOT ( t_310[] IS INITIAL ).
    LOOP AT t_310 INTO sl_mseg.
      READ TABLE t_mseg WITH KEY mblnr = sl_mseg-smbln
                                 mjahr = sl_mseg-mjahr
                        BINARY SEARCH
                        TRANSPORTING NO FIELDS.

      CHECK sy-subrc IS INITIAL.

      DELETE t_mseg INDEX sy-tabix.

      CLEAR sl_mseg.
    ENDLOOP.
  ENDIF.

  SORT: t_mseg[] BY charg.

  DELETE ADJACENT DUPLICATES FROM t_mseg COMPARING charg.

  IF t_mseg[] IS INITIAL.
    MESSAGE i836 WITH text-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_MSEG

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_PP                                           *
*&---------------------------------------------------------------------*
*                                Dados PP                              *
*----------------------------------------------------------------------*
FORM z_seleciona_pp.

  REFRESH: t_mkpfpp, t_msegpp.

  DATA: wa_data TYPE ztsafrafardos.

  CLEAR: wa_data.

  SELECT SINGLE * FROM ztsafrafardos INTO wa_data WHERE charg EQ p_safra AND werks_from IN s_werks.

  IF wa_data-data_inicio(04) EQ wa_data-data_fim(04).
    SELECT mblnr mjahr budat
    FROM mkpf
      INTO TABLE t_mkpfpp
    WHERE mjahr EQ wa_data-data_inicio(04)
      AND vgart EQ c_ws.
  ELSE.
    SELECT mblnr mjahr budat
    FROM mkpf
      INTO TABLE t_mkpfpp
    WHERE ( mjahr EQ wa_data-data_inicio(04) OR mjahr EQ wa_data-data_fim(04) )
      AND vgart EQ c_ws.
  ENDIF.

  CHECK NOT t_mkpfpp[] IS INITIAL.

  SELECT mblnr mjahr zeile bwart matnr
         werks lgort charg menge aufnr
         bukrs
  FROM mseg
    INTO TABLE t_msegpp
    FOR ALL ENTRIES IN t_mkpfpp
  WHERE mblnr EQ t_mkpfpp-mblnr
    AND mjahr EQ t_mkpfpp-mjahr
    AND bwart EQ c_131
    AND werks IN s_werks.

ENDFORM.                    " Z_SELECIONA_PP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_AUFK                                         *
*&---------------------------------------------------------------------*
*                            Seleciona AUFK                            *
*----------------------------------------------------------------------*
FORM z_seleciona_aufk.

  DATA tl_mseg TYPE TABLE OF type_mseg.

  REFRESH t_aufk.

  tl_mseg[] = t_msegpp[].
  SORT tl_mseg BY aufnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_mseg COMPARING aufnr.
  DELETE tl_mseg WHERE aufnr IS INITIAL.

  CHECK NOT tl_mseg[] IS INITIAL.

  SELECT aufnr aufex
  FROM aufk
    INTO TABLE t_aufk
    FOR ALL ENTRIES IN tl_mseg
  WHERE aufnr EQ tl_mseg-aufnr.

ENDFORM.                    " Z_SELECIONA_AUFK

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

  CHECK NOT t_mchb[] IS INITIAL.
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
    FOR ALL ENTRIES IN tl_mchb
   WHERE klart EQ st_rmclm-klart
     AND obtab EQ 'MARA'
     AND objek EQ tl_mchb-objek.

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

  CHECK NOT t_mseg[] IS INITIAL.
  tl_mseg[] = t_mseg[].
  SORT tl_mseg BY matnr werks lgort charg ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_mseg COMPARING matnr werks lgort charg.

  SELECT matnr werks lgort charg
  FROM mchb
    INTO TABLE t_mchb
    FOR ALL ENTRIES IN tl_mseg
  WHERE matnr EQ tl_mseg-matnr
    AND werks EQ tl_mseg-werks
    AND lgort EQ tl_mseg-lgort
    AND charg EQ tl_mseg-charg.

  SORT t_mchb BY matnr ASCENDING
                 werks ASCENDING
                 lgort ASCENDING
                 charg ASCENDING.

* Move Matnr p/ Objek
  PERFORM z_move_matnr_objek.

ENDFORM.                    " Z_SELECIONA_MCHB

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
    MODIFY t_inob FROM sl_inob INDEX vl_index TRANSPORTING objekk.

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
    MODIFY t_mchb FROM sl_mchb INDEX vl_index TRANSPORTING objek.

    CLEAR sl_mchb.
  ENDLOOP.

ENDFORM.                    " Z_MOVE_MATNR_OBJEK

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS                                         *
*&---------------------------------------------------------------------*
*                           Processa Dados                             *
*----------------------------------------------------------------------*
FORM z_processa_dados.

  FREE: tl_saida_ger.

  IF NOT c_atual IS INITIAL.
    DATA: sl_mseg     TYPE type_mseg,
          sl_mseg_ger TYPE type_mseg,
          sl_mchb     TYPE type_mchb,
          sl_msegpp   TYPE type_mseg,
          sl_aufk     TYPE type_aufk,
          sl_inob     TYPE type_inob,
          sl_kssk     TYPE type_kssk,
          sl_klah     TYPE type_klah,
          sl_keys     TYPE bapi1003_object_keys,
          sl_0001     TYPE zmmi0001,
          sl_t001     TYPE type_t001,
          sl_saida    TYPE zmmr0001,
          sl_mara     TYPE type_mara,
          vl_num      TYPE bapi1003_key-classnum,
          vl_type     TYPE bapi1003_key-classtype,
          vl_table    TYPE bapi1003_key-objecttable,
          vl_key      TYPE bapi1003_key-object,
          vl_talhao   TYPE char30,
          vl_safra    TYPE numc4,
          vl_varie    TYPE char30,
          vl_bukrs    TYPE mseg-bukrs,
          tl_table    TYPE TABLE OF bapi1003_object_keys,
          tl_saida    TYPE TABLE OF zmmr0001,
          tl_ret      TYPE TABLE OF bapiret2.

    REFRESH tl_saida.

    SORT: t_msegpp BY matnr ASCENDING
                      werks ASCENDING
                      charg ASCENDING,
          t_aufk   BY aufnr ASCENDING,
          t_inob   BY objek ASCENDING,
          t_kssk   BY objek ASCENDING,
          t_klah   BY clint ASCENDING,
          t_t001   BY bukrs ASCENDING,
          t_mseg   BY bukrs ASCENDING,
          t_mara   BY matnr ASCENDING,
          it_mchb_atual BY matnr werks lgort charg.

    vl_table = 'MCH1'.

* Busca Descrição da Filial
    SELECT SINGLE name1 FROM t001w
      INTO vg_name1
    WHERE werks IN s_werks.

    t_mseg_ger[] = t_mseg[].
    SORT t_mseg_ger BY lgort.
    DELETE ADJACENT DUPLICATES FROM t_mseg_ger
                          COMPARING lgort.

    LOOP AT t_mseg_ger INTO sl_mseg_ger.

      FREE: tl_saida.

      LOOP AT t_mseg INTO sl_mseg WHERE lgort = sl_mseg_ger-lgort.

        REFRESH tl_table.

        CLEAR: sl_msegpp, sl_aufk, sl_mchb, sl_saida, sl_t001.

        READ TABLE t_t001 INTO sl_t001 WITH KEY bukrs = sl_mseg-bukrs
                                       BINARY SEARCH.

        IF vl_bukrs IS INITIAL.
          vl_bukrs = sl_mseg-bukrs.
        ELSE.
          IF vl_bukrs NE sl_mseg-bukrs AND NOT tl_saida[] IS INITIAL.
*       Chama SmartForms
            PERFORM z_smart_forms TABLES tl_saida
                                   USING sl_t001
                                         vl_varie.
            REFRESH tl_saida.
            CLEAR vl_varie.
            vl_bukrs = sl_mseg-bukrs.
          ENDIF.
        ENDIF.

        READ TABLE t_msegpp INTO sl_msegpp WITH KEY matnr = sl_mseg-matnr
                                                    werks = sl_mseg-werks
                                                    charg = sl_mseg-charg
                                           BINARY SEARCH.

        READ TABLE t_aufk INTO sl_aufk WITH KEY aufnr = sl_msegpp-aufnr
                                       BINARY SEARCH.

        READ TABLE t_mchb INTO sl_mchb WITH KEY matnr = sl_mseg-matnr
                                                werks = sl_mseg-werks
                                                lgort = sl_mseg-lgort
                                                charg = sl_mseg-charg
                                       BINARY SEARCH.

        sl_keys-key_field = 'MATNR'.
        sl_keys-value_int = sl_mseg-matnr.
        APPEND sl_keys TO tl_table.

        sl_keys-key_field = 'CHARG'.
        sl_keys-value_int = sl_mseg-charg.
        APPEND sl_keys TO tl_table.

        CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
          EXPORTING
            objecttable    = vl_table
          IMPORTING
            objectkey_conc = vl_key
          TABLES
            objectkeytable = tl_table
            return         = tl_ret.

        READ TABLE t_inob INTO sl_inob WITH KEY objek = sl_mchb-objek
                                       BINARY SEARCH.

        READ TABLE t_kssk INTO sl_kssk WITH KEY objek = sl_inob-objekk
                                       BINARY SEARCH.

        READ TABLE t_klah INTO sl_klah WITH KEY clint = sl_kssk-clint
                                       BINARY SEARCH.

        READ TABLE t_mara INTO sl_mara WITH KEY matnr = sl_mseg-matnr
                                       BINARY SEARCH.

        vl_num  = sl_klah-class.
        vl_type = st_rmclm-klart.

*   Preenche Características
        PERFORM z_preenche_carac USING vl_key
                                       vl_table
                                       vl_num
                                       vl_type
                                       sl_mseg
                              CHANGING sl_0001
                                       vl_varie
                                       vl_talhao.
        IF  sl_0001 IS INITIAL. "ALRS CH 113443
          CONTINUE.
        ENDIF.



        READ TABLE it_mchb_atual  WITH KEY matnr = sl_mseg-matnr
                                           werks = sl_mseg-werks
                                           lgort = sl_mseg-lgort
                                           charg = sl_mseg-charg
                           BINARY SEARCH.
        sl_saida =  CORRESPONDING #( sl_0001 ).
*       SL_SAIDA-MENGE  = SL_MSEG-MENGE.
*       SL_SAIDA-PESO   = SL_MSEG-MENGE.
        sl_saida-menge  = it_mchb_atual-clabs + it_mchb_atual-cspem .
        sl_saida-peso   = sl_saida-menge.
        sl_saida-peso_d = sl_saida-menge.
        sl_saida-aufex  = sl_aufk-aufex+10.
        sl_saida-talhao = vl_talhao.

        sl_saida-werks  = sl_mseg-werks.  "*-CS2022000332-#82301-01.08.2022-JT-inicio

*-CS2021000809 - 08.12.2021 - JT - inicio
        sl_saida-lgort      = sl_mseg-lgort.
        sl_saida-tot_fardos = 1.
*-CS2021000809 - 08.12.2021 - JT - fim

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = sl_mara-normt
          IMPORTING
            output = sl_saida-material.

        CONDENSE sl_saida-peso.

        IF NOT sl_saida-peso IS INITIAL.
          APPEND sl_saida TO tl_saida.
        ENDIF.

        CLEAR sl_mseg.
      ENDLOOP.

      SORT tl_saida BY fardos.

      PERFORM z_ajusta_colunas TABLES tl_saida.
      PERFORM z_insere_rodape  TABLES tl_saida
                                USING abap_false.

      APPEND LINES OF tl_saida[]   TO tl_saida_ger[].
    ENDLOOP.

** ADD - 20.06.2013 - Inicio -----------------------------------------------
  ELSE.
    CHECK NOT it_zmmt0027[] IS INITIAL.

    it_zmmt0027_ger[] = it_zmmt0027[].
    SORT it_zmmt0027_ger BY lgort.
    DELETE ADJACENT DUPLICATES FROM it_zmmt0027_ger
                          COMPARING lgort.

    LOOP AT it_zmmt0027_ger INTO wa_zmmt0027_ger.

      FREE: tl_saida.

      LOOP AT it_zmmt0027 INTO wa_zmmt0027 WHERE lgort = wa_zmmt0027_ger-lgort.
        MOVE:
*             wa_zmmt0027-MANDT         TO sl_saida-,
              wa_zmmt0027-charg         TO sl_saida-fardos,
*             wa_zmmt0027-WERKS         TO sl_saida-,
*             wa_zmmt0027-MATKL         TO sl_saida-,
*             WA_ZMMT0027-MATNR         TO SL_SAIDA-MATERIAL, "ADD 25.08.2016
*             wa_zmmt0027-SAFRA         TO sl_saida-,
              wa_zmmt0027-menge         TO sl_saida-menge,
              wa_zmmt0027-menge         TO sl_saida-peso,
*-CS2021000809 - 08.12.2021 - JT - inicio
              wa_zmmt0027-menge        TO sl_saida-peso_d,
              wa_zmmt0027-lgort        TO sl_saida-lgort,
*-CS2021000809 - 08.12.2021 - JT - fim
              wa_zmmt0027-normt         TO sl_saida-material, "ADD 25.08.2016
*             wa_zmmt0027-BUDAT         TO sl_saida-,
*             wa_zmmt0027-CHARG_ORIG    TO sl_saida-,
*             wa_zmmt0027-STATUS        TO sl_saida-,
              wa_zmmt0027-adquirido_terc TO sl_saida-adquirido_terc, "*-CS2022000332-#82301-01.08.2022-JT-inicio
              wa_zmmt0027-werks          TO sl_saida-werks. "*-CS2022000332-#82301-01.08.2022-JT-inicio
*---> 14/06/2023 - Migração S4 - JS
*              wa_zmmt0027-variedade     TO vl_varie,
              vl_varie = CONV #( wa_zmmt0027-variedade ).
*<--- 14/06/2023 - Migração S4 - JS
              MOVE:
              wa_zmmt0027-talhao        TO sl_saida-talhao,
              wa_zmmt0027-far_uhml      TO sl_saida-uhml,
              wa_zmmt0027-far_ui        TO sl_saida-ui,
              wa_zmmt0027-far_str       TO sl_saida-str,
              wa_zmmt0027-far_elg       TO sl_saida-elg,
              wa_zmmt0027-far_mic       TO sl_saida-mic,
              wa_zmmt0027-far_rd        TO sl_saida-rd,
              wa_zmmt0027-far_b         TO sl_saida-plusb,
              wa_zmmt0027-far_cg        TO sl_saida-cg,
              wa_zmmt0027-far_tcnt      TO sl_saida-t_cnt,
              wa_zmmt0027-far_tarea     TO sl_saida-t_area,
              wa_zmmt0027-far_leaf      TO sl_saida-leaf,
              wa_zmmt0027-far_mr        TO sl_saida-mr,
              wa_zmmt0027-far_sfiw      TO sl_saida-sfi_w,
              wa_zmmt0027-far_sci       TO sl_saida-sci,
              wa_zmmt0027-far_csp       TO sl_saida-csp,
              1                         TO sl_saida-tot_fardos.
*            wa_zmmt0027-FAR_PERIODO   TO sl_saida-,
*           to sl_saida-AUFEX   "Em branco

        CONDENSE sl_saida-peso.

        IF NOT sl_saida-peso IS INITIAL.
          APPEND sl_saida TO tl_saida.
        ENDIF.

        CLEAR: wa_zmmt0027.
      ENDLOOP.

      SORT tl_saida BY fardos.

      PERFORM z_ajusta_colunas TABLES tl_saida.
      PERFORM z_insere_rodape  TABLES tl_saida
                                USING abap_false.

      APPEND LINES OF tl_saida[]   TO tl_saida_ger[].
    ENDLOOP.

* Busca Empresa
    SELECT SINGLE bukrs       FROM t001k  INTO vg_bukrs WHERE bwkey IN s_werks.
    SELECT SINGLE bukrs butxt FROM t001   INTO sl_t001  WHERE bukrs EQ vg_bukrs.
* Busca Descrição da Filial
    SELECT SINGLE name1       FROM t001w  INTO vg_name1 WHERE werks IN s_werks.
  ENDIF.
** ADD - 20.06.2013 - Fim --------------------------------------------------

  IF NOT tl_saida_ger[] IS INITIAL.

    tl_saida_tot[] = tl_saida_ger.
    DELETE tl_saida_tot WHERE subtotal = abap_true.

    PERFORM z_insere_rodape  TABLES tl_saida_tot
                              USING abap_true.

    DESCRIBE TABLE tl_saida_tot LINES DATA(l_linhas).
    l_linhas = l_linhas - 1.

    READ TABLE tl_saida_tot INTO sl_saida_tot INDEX l_linhas.
    APPEND sl_saida_tot       TO tl_saida_ger.

*   Chama SmartForms
    PERFORM z_smart_forms TABLES tl_saida_ger
                           USING sl_t001
                                 vl_varie.

  ENDIF.

ENDFORM.                    " Z_PROCESSA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_CARAC                                         *
*&---------------------------------------------------------------------*
*                      Preenche Características                        *
*----------------------------------------------------------------------*
FORM z_preenche_carac USING p_key    TYPE bapi1003_key-object
                            p_table  TYPE bapi1003_key-objecttable
                            p_num    TYPE bapi1003_key-classnum
                            p_type   TYPE bapi1003_key-classtype
                            p_mseg   TYPE type_mseg
                   CHANGING p_0001   TYPE zmmi0001
                            p_varie  TYPE char30
                            p_talhao TYPE char30.

  DATA: tl_cat      TYPE lvc_t_fcat,
        sl_cat      TYPE lvc_s_fcat,
        vl_campo    TYPE char30,
        vl_num      TYPE numc10,
        vl_char     TYPE atwrt,                                "ADD - 20.06.2013
        n_carac0(8) TYPE p DECIMALS 0, "caracteristicas com 0 decimal.
        n_carac1(8) TYPE p DECIMALS 1, "caracteristicas com 1 decimal.
        sl_char     TYPE bapi1003_alloc_values_char,
        sl_num      TYPE bapi1003_alloc_values_num,
        sl_aux      TYPE bapi1003_alloc_values_char,
        tl_aux      TYPE TABLE OF bapi1003_alloc_values_char,
        tl_num      TYPE TABLE OF bapi1003_alloc_values_num,
        tl_char     TYPE TABLE OF bapi1003_alloc_values_char,
        tl_curr     TYPE TABLE OF bapi1003_alloc_values_curr,
        tl_ret      TYPE TABLE OF bapiret2.

  "FIELD-SYMBOLS <campo> TYPE char30.
  FIELD-SYMBOLS <campo> TYPE any.

  CLEAR: p_0001, p_talhao.

  p_0001-fardos = p_mseg-charg.

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

  SORT: tl_num  BY charact ASCENDING,
        tl_char BY charact ASCENDING.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMMI0001'
    CHANGING
      ct_fieldcat            = tl_cat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DELETE tl_cat INDEX 1.

  LOOP AT tl_cat INTO sl_cat.
    CLEAR sl_aux.

    CASE sl_cat-fieldname.
      WHEN 'PLUSB'.
        sl_aux-charact_descr = '+B'.
      WHEN 'T_CNT'.
        sl_aux-charact_descr = 'T.CNT'.
      WHEN 'T_AREA'.
        sl_aux-charact_descr = 'T.AREA'.
      WHEN 'SFI_W'.
        sl_aux-charact_descr = 'SFI(W)'.
      WHEN OTHERS.
        sl_aux-charact_descr = sl_cat-fieldname.
    ENDCASE.

    sl_aux-value_char  = sl_cat-fieldname.

    APPEND sl_aux TO tl_aux.

    CLEAR sl_cat.
  ENDLOOP.

  SORT: tl_aux  BY charact_descr ASCENDING,
        tl_num  BY charact_descr ASCENDING,
        tl_char BY charact_descr ASCENDING.


** ADD - 20.06.2013 - Inicio
  READ TABLE tl_char INTO sl_char WITH KEY charact_descr = 'Safra' BINARY SEARCH.
  IF NOT sy-subrc IS INITIAL.
    PERFORM z_retorna_msg USING text-004 p_mseg.
    EXIT.
  ENDIF.

  IF sl_char-value_char NE p_safra.
    CLEAR: p_0001.
    EXIT.
  ENDIF.
** ADD - 20.06.2013 - Fim

  LOOP AT tl_aux INTO sl_aux.
    CONCATENATE 'P_0001' sl_aux-value_char INTO vl_campo SEPARATED BY '-'.

    ASSIGN (vl_campo) TO <campo>. "Leonardo 07/07 IR:IR065347

    READ TABLE tl_char INTO sl_char WITH KEY charact_descr = sl_aux-charact_descr
                                    BINARY SEARCH.

**    IF NOT SY-SUBRC IS INITIAL.
** COMMENT - 20.06.2013
**      READ TABLE TL_NUM INTO SL_NUM WITH KEY CHARACT_DESCR = SL_AUX-CHARACT_DESCR
**                                    BINARY SEARCH.
**      IF NOT SL_NUM-VALUE_FROM IS INITIAL.
**        MOVE SL_NUM-VALUE_FROM TO VL_NUM.
**        <CAMPO> = VL_NUM.
**        SHIFT <CAMPO> LEFT DELETING LEADING '0'.
**      ENDIF.
**    ELSE.
    IF sy-subrc = 0.
      IF NOT sl_char-value_char IS INITIAL.
        CASE sl_aux-charact_descr.
          WHEN 'RD' OR '+B' OR 'UI' OR 'SFI(W)' OR 'ELG' OR 'STR'.
            n_carac1 = sl_char-value_char.
            WRITE n_carac1 TO sl_char-value_char.
            TRANSLATE sl_char-value_char USING ',.'.
            CONDENSE sl_char-value_char NO-GAPS.
          WHEN 'T.CNT'  .
            n_carac0 = sl_char-value_char.
            WRITE n_carac0 TO sl_char-value_char.
            TRANSLATE sl_char-value_char USING ',.'.
            CONDENSE sl_char-value_char NO-GAPS.
          WHEN 'CSP'.
            TRANSLATE sl_char-value_char USING ',.'.
            CONDENSE sl_char-value_char NO-GAPS.
        ENDCASE.
        MOVE sl_char-value_char TO <campo>. "Leonardo 07/07 IR:IR065347
      ENDIF.
    ENDIF.

    CLEAR: sl_aux, sl_char, sl_num.
  ENDLOOP.

* Busca Variedade
  READ TABLE tl_char INTO sl_char WITH KEY charact_descr = 'Variedade'
                                  BINARY SEARCH.

  IF sy-subrc IS INITIAL AND p_varie IS INITIAL.
    p_varie = sl_char-value_char.
  ENDIF.

* Busca Talhão
  READ TABLE tl_char INTO sl_char WITH KEY charact_descr = 'Talhao'
                                  BINARY SEARCH.

  IF sy-subrc IS INITIAL.
    p_talhao = sl_char-value_char.
  ENDIF.

ENDFORM.                    " Z_PREENCHE_CARAC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_T001                                         *
*&---------------------------------------------------------------------*
*                              Seleciona T001                          *
*----------------------------------------------------------------------*
FORM z_seleciona_t001.
  DATA tl_mseg TYPE TABLE OF type_mseg.

  REFRESH t_t001.

  CHECK NOT t_mseg[] IS INITIAL.
  tl_mseg[] = t_mseg[].
  SORT tl_mseg BY bukrs ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_mseg COMPARING bukrs.

  SELECT bukrs butxt
  FROM t001
    INTO TABLE t_t001
    FOR ALL ENTRIES IN tl_mseg
  WHERE bukrs EQ tl_mseg-bukrs.

ENDFORM.                    " Z_SELECIONA_T001

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
*&      Form  PF_GERA_EXCEL
*&---------------------------------------------------------------------*
*                            Chama SmartForms                          *
*----------------------------------------------------------------------*
FORM pf_gera_excel TABLES p_saida  STRUCTURE zmmr0001
                    USING p_rodape TYPE zmmr0001_rod.

  g_excel_sem_popup  = abap_true.
  EXPORT g_excel_sem_popup FROM g_excel_sem_popup TO MEMORY ID 'ZHCM_EXCEL_SEM_POPUP'.

  t_saida[] = p_saida[].

  FREE: t_fcat_lvc.

  w_fcat_lvc-col_pos   = 1.
  w_fcat_lvc-fieldname = 'LGORT'.
  w_fcat_lvc-intlen    = 15.
  w_fcat_lvc-dd_outlen = 15.
  w_fcat_lvc-coltext   = 'Bloco'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-tabname   = 'T_SAIDA'.
  w_fcat_lvc-emphasize = 'K41'.
  w_fcat_lvc-datatype  = 'CHAR'.
  w_fcat_lvc-inttype   = 'C'.
  w_fcat_lvc-intlen    = 20.
  w_fcat_lvc-dd_outlen = 20.
  w_fcat_lvc-col_pos   = 2.
  w_fcat_lvc-fieldname = 'CD_SAI'.
  w_fcat_lvc-coltext   = 'N.Fardo'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 3.
  w_fcat_lvc-fieldname = 'MATERIAL'.
  w_fcat_lvc-coltext   = 'Tipo'.
  w_fcat_lvc-intlen    = 08.
  w_fcat_lvc-dd_outlen = 08.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  IF ch_cg = abap_true.
    w_fcat_lvc-col_pos   = 4.
    w_fcat_lvc-fieldname = 'CG'.
    w_fcat_lvc-coltext   = 'CG'.
    w_fcat_lvc-edit      = abap_false.
    APPEND w_fcat_lvc   TO t_fcat_lvc.
  ENDIF.
*
  w_fcat_lvc-intlen    = 20.
  w_fcat_lvc-dd_outlen = 20.
*
  w_fcat_lvc-col_pos   = 5.
  w_fcat_lvc-fieldname = 'TOT_FARDOS'.
  w_fcat_lvc-coltext   = 'Fardos'.
  w_fcat_lvc-edit      = abap_false.
  w_fcat_lvc-intlen    = 08.
  w_fcat_lvc-dd_outlen = 08.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-intlen    = 20.
  w_fcat_lvc-dd_outlen = 20.
*
  w_fcat_lvc-col_pos   = 6.
  w_fcat_lvc-fieldname = 'PESO'.
  w_fcat_lvc-coltext   = 'Peso'.
  w_fcat_lvc-edit      = abap_false.
* w_fcat_lvc-domname   = 'MENGE_D'.
* w_fcat_lvc-inttype   = abap_false.
* w_fcat_lvc-datatype  = 'QUAN'.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 7.
  w_fcat_lvc-fieldname = 'RD'.
  w_fcat_lvc-coltext   = 'Rd.'.
  w_fcat_lvc-edit      = abap_false.
  w_fcat_lvc-domname   = abap_false.
  w_fcat_lvc-inttype   = 'C'.
  w_fcat_lvc-datatype  = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 8.
  w_fcat_lvc-fieldname = 'PLUSB'.
  w_fcat_lvc-coltext   = '+b'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 9.
  w_fcat_lvc-fieldname = 'UHML'.
  w_fcat_lvc-coltext   = 'Fibra'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 10.
  w_fcat_lvc-fieldname = 'MIC'.
  w_fcat_lvc-coltext   = 'Mic'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 11.
  w_fcat_lvc-fieldname = 'STR'.
  w_fcat_lvc-coltext   = 'Res'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 12.
  w_fcat_lvc-fieldname = 'UI'.
  w_fcat_lvc-coltext   = 'UI'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 13.
  w_fcat_lvc-fieldname = 'CSP'.
  w_fcat_lvc-coltext   = 'CSP'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 14.
  w_fcat_lvc-fieldname = 'MR'.
  w_fcat_lvc-coltext   = 'Mr'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 15.
  w_fcat_lvc-fieldname = 'SCI'.
  w_fcat_lvc-coltext   = 'SCI'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  w_fcat_lvc-col_pos   = 16.
  w_fcat_lvc-fieldname = 'SFI_W'.
  w_fcat_lvc-coltext   = 'Sfi'.
  w_fcat_lvc-edit      = abap_false.
  APPEND w_fcat_lvc   TO t_fcat_lvc.
*
  IF ch_leaf = abap_true.
    w_fcat_lvc-col_pos   = 17.
    w_fcat_lvc-fieldname = 'LEAF'.
    w_fcat_lvc-coltext   = 'LEAF'.
    w_fcat_lvc-edit      = abap_false.
    APPEND w_fcat_lvc   TO t_fcat_lvc.
  ENDIF.
*
  IF ch_elg = abap_true.
    w_fcat_lvc-col_pos   = 18.
    w_fcat_lvc-fieldname = 'ELG'.
    w_fcat_lvc-coltext   = 'ELG'.
    w_fcat_lvc-edit      = abap_false.
    APPEND w_fcat_lvc   TO t_fcat_lvc.
  ENDIF.

* Transfer to KKBLO format.
*  CALL FUNCTION 'LVC_TRANSFER_TO_KKBLO'
*    EXPORTING
*      it_fieldcat_lvc         = t_fcat_lvc[]
*    IMPORTING
*      et_fieldcat_kkblo       = t_fcat_kkb
*    EXCEPTIONS
*      it_data_missing         = 1
*      it_fieldcat_lvc_missing = 2
*      OTHERS                  = 3.
*
*  CHECK sy-subrc EQ 0.

*-----------------------------------------
* Call XXL.
*-----------------------------------------
*  CALL FUNCTION 'ALV_XXL_CALL'
*    EXPORTING
*      i_tabname           = 'T_SAIDA'
*      it_fieldcat         = t_fcat_kkb
*    TABLES
*      it_outtab           = t_saida[]
*    EXCEPTIONS
*      fatal_error         = 1
*      no_display_possible = 2
*      OTHERS              = 3.
*
*  FREE MEMORY ID 'ZHCM_EXCEL_SEM_POPUP'.

*-----------------------------------------
* excel de saida
*-----------------------------------------
  CALL SCREEN 300.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SMART_FORMS                                            *
*&---------------------------------------------------------------------*
*                            Chama SmartForms                          *
*----------------------------------------------------------------------*
FORM z_insere_rodape TABLES p_saida  STRUCTURE zmmr0001
                      USING p_geral.

  DATA: vl_safra  TYPE char4,
        sl_rodape TYPE zmmr0001_rod,
        vl_pos    TYPE i,
        vl_tam    TYPE i.

  PERFORM z_seleciona_cod. "ADD - 25.08.2016
  PERFORM pf_exec_cd_sai TABLES p_saida.

* SORT p_saida BY fardos ASCENDING.

  vl_safra = p_safra.

* Preenche Rodapé
  PERFORM z_rodape TABLES p_saida
                    USING p_geral
                 CHANGING sl_rodape.

** Igor Vilela - Correcção Kulhman - inicio
  LOOP AT p_saida.

*-CS2022000332-#82301-01.08.2022-JT-inicio
    IF p_saida-adquirido_terc = abap_false.
      SEARCH p_saida-fardos FOR './.'.
      vl_pos = sy-fdpos + 1.
      CLEAR: p_saida-fardos(vl_pos).
    ELSE.
      p_saida-fardos = p_saida-fardos+3(7).  "Fornecedor não intercompany
      IF strlen( p_saida-fardos ) < 7.
        CLEAR p_saida-fardos.
      ENDIF.
    ENDIF.
*-CS2022000332-#82301-01.08.2022-JT-fim

    CONDENSE p_saida-fardos NO-GAPS.

    SEARCH p_saida-material FOR '.-.'.
    IF sy-fdpos GE 3.
      vl_pos = sy-fdpos .
      CLEAR: p_saida-material+vl_pos(3).
    ENDIF.
    CONDENSE p_saida-material NO-GAPS.
    MODIFY p_saida.
  ENDLOOP.
** Igor Vilela - Correcção Kulhman -

  MOVE-CORRESPONDING sl_rodape TO w_saida.
  MOVE sl_rodape-fardos        TO w_saida-cd_sai.
  MOVE abap_true               TO w_saida-subtotal.
  APPEND w_saida               TO p_saida.
  CLEAR w_saida.
  MOVE abap_true               TO w_saida-subtotal.
  APPEND w_saida               TO p_saida.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SMART_FORMS                                            *
*&---------------------------------------------------------------------*
*                            Chama SmartForms                          *
*----------------------------------------------------------------------*
FORM z_smart_forms TABLES p_saida  STRUCTURE zmmr0001
                    USING p_t001   TYPE type_t001
                          vl_varie TYPE char30.

  DATA: vl_form   TYPE tdsfname,
        vl_name   TYPE rs38l_fnam,
        vl_safra  TYPE char4,
        sl_rodape TYPE zmmr0001_rod,
        vl_pos    TYPE i,
        vl_tam    TYPE i.

  vl_safra = p_safra.

*-CS2021000809 - 08.12.2021 - JT - inicio
  IF p_exc = abap_true.
    PERFORM pf_gera_excel TABLES p_saida
                           USING sl_rodape.
    EXIT.
  ENDIF.
*-CS2021000809 - 08.12.2021 - JT - inicio

  IF p_retrat IS NOT INITIAL.
    vl_form = 'ZMMSF0001_AUX'.
  ELSE.
    vl_form = 'ZMMSF0001_AUX3'.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_form
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION vl_name
    EXPORTING
      i_rodape         = sl_rodape
      p_butxt          = p_t001-butxt
      p_lgort          = st_t001l-lgort
      p_lgobe          = st_t001l-lgobe
      p_name1          = vg_name1
      p_safra          = vl_safra
      p_variedade      = vl_varie
      p_codsai         = vg_cod "ADD - 25.08.2016
    TABLES
      t_zmmr0001       = p_saida
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_SMART_FORMS

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
*&      Form  Z_RODAPE
*&---------------------------------------------------------------------*
*                              Preenche Rodapé                         *
*----------------------------------------------------------------------*
FORM z_rodape TABLES p_saida STRUCTURE zmmr0001
               USING p_geral
            CHANGING p_roda  TYPE zmmr0001_rod.

  DATA: vl_peso  TYPE p DECIMALS  3,
        vl_str   TYPE p DECIMALS  2,
        vl_csp   TYPE p DECIMALS  2,
        vl_elg   TYPE p DECIMALS  2,
        vl_ui    TYPE p DECIMALS  2,
        vl_rd    TYPE p DECIMALS  2,
        vl_plusb TYPE p DECIMALS  2,
        vl_uhml  TYPE p DECIMALS  2,
        vl_mic   TYPE p DECIMALS  2,
        vl_mr    TYPE p DECIMALS  2,
        vl_sfi_w TYPE p DECIMALS  2,
        vl_sci   TYPE p DECIMALS  2,
        vl_leaf  TYPE i, " DECIMALS  2,
*        VL_CG    TYPE P DECIMALS  2,
        sl_saida TYPE zmmr0001.

  CLEAR p_roda.

  DESCRIBE TABLE p_saida LINES DATA(l_tot_saida).
  p_roda-fardos     = l_tot_saida.
  p_roda-tot_fardos = l_tot_saida.

  LOOP AT p_saida INTO sl_saida.
    ADD: sl_saida-str   TO vl_str.
    REPLACE ALL OCCURRENCES OF ',' IN sl_saida-csp WITH '.'.
    ADD: sl_saida-csp   TO vl_csp,
         sl_saida-ui    TO vl_ui,
         sl_saida-peso  TO vl_peso,
         sl_saida-elg   TO vl_elg,
         sl_saida-rd    TO vl_rd,
         sl_saida-plusb TO vl_plusb,
         sl_saida-uhml  TO vl_uhml,
         sl_saida-mic   TO vl_mic,
         sl_saida-mr    TO vl_mr,
         sl_saida-leaf  TO vl_leaf,
*         SL_SAIDA-CG    TO VL_CG,
         sl_saida-sfi_w TO vl_sfi_w,
         sl_saida-sci   TO vl_sci.
    CLEAR sl_saida.
  ENDLOOP.

  CHECK NOT p_roda-fardos IS INITIAL.

  IF NOT vl_str IS INITIAL.
    vl_str = vl_str / p_roda-fardos.
  ENDIF.

  IF NOT vl_csp IS INITIAL.
    vl_csp = vl_csp / p_roda-fardos.
  ENDIF.

  IF NOT vl_ui IS INITIAL.
    vl_ui = vl_ui / p_roda-fardos.
  ENDIF.

  IF NOT vl_elg IS INITIAL.
    vl_elg = vl_elg / p_roda-fardos.
  ENDIF.

  IF NOT vl_rd IS INITIAL.
    vl_rd = vl_rd / p_roda-fardos.
  ENDIF.

  IF NOT vl_plusb IS INITIAL.
    vl_plusb = vl_plusb / p_roda-fardos.
  ENDIF.

  IF NOT vl_uhml IS INITIAL.
    vl_uhml = vl_uhml / p_roda-fardos.
  ENDIF.

  IF NOT vl_mic IS INITIAL.
    vl_mic = vl_mic / p_roda-fardos.
  ENDIF.

  IF NOT vl_mr IS INITIAL.
    vl_mr = vl_mr / p_roda-fardos.
  ENDIF.

  IF NOT vl_sfi_w IS INITIAL.
    vl_sfi_w = vl_sfi_w / p_roda-fardos.
  ENDIF.

  IF NOT vl_sci   IS INITIAL.
    vl_sci = vl_sci / p_roda-fardos.
  ENDIF.

  IF NOT vl_leaf IS INITIAL.
    vl_leaf = vl_leaf / p_roda-fardos.
  ENDIF.

  READ TABLE p_saida INTO sl_saida INDEX 1.

  IF p_geral = abap_true.
    IF p_exc = abap_true.
      p_roda-lgort  = 'Total Geral'.
    ELSE.
      p_roda-fardos = 'Total Geral: ' && l_tot_saida.
    ENDIF.
  ELSE.
    IF p_exc = abap_true.
      p_roda-lgort  = sl_saida-lgort && ' Total'.
    ELSE.
      p_roda-fardos = sl_saida-lgort && ' Total: ' && l_tot_saida.
    ENDIF.
  ENDIF.

* p_roda-fardos = p_roda-fardos.
  p_roda-peso   = vl_peso.
  p_roda-str    = vl_str.
  p_roda-csp    = vl_csp.
  p_roda-ui     = vl_ui.
  p_roda-elg    = vl_elg.
  p_roda-rd     = vl_rd.
  p_roda-plusb  = vl_plusb.
  p_roda-uhml   = vl_uhml.
  p_roda-mic    = vl_mic.
  p_roda-mr     = vl_mr.
  p_roda-sfi_w  = vl_sfi_w.
  p_roda-sci    = vl_sci.
  p_roda-leaf   = vl_leaf.
*  P_RODA-CG     = VL_CG.

  CONDENSE: "p_roda-fardos NO-GAPS,
            p_roda-peso   NO-GAPS,
            p_roda-str    NO-GAPS,
            p_roda-csp    NO-GAPS,
            p_roda-ui     NO-GAPS,
            p_roda-elg    NO-GAPS,
            p_roda-rd     NO-GAPS,
            p_roda-plusb  NO-GAPS,
            p_roda-uhml   NO-GAPS,
            p_roda-mic    NO-GAPS,
            p_roda-mr     NO-GAPS,
            p_roda-sfi_w  NO-GAPS,
            p_roda-sci    NO-GAPS,
            p_roda-leaf   NO-GAPS.
*            P_RODA-CG     NO-GAPS.

ENDFORM.                    " Z_RODAPE

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MARA                                         *
*&---------------------------------------------------------------------*
*                             Seleciona Mara                           *
*----------------------------------------------------------------------*
FORM z_seleciona_mara.

  DATA tl_mseg TYPE TABLE OF type_mseg.

  REFRESH t_mara.

  tl_mseg[] = t_mseg[].
  SORT tl_mseg BY matnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_mseg COMPARING matnr.

  CHECK NOT tl_mseg[] IS INITIAL.

  SELECT matnr normt
  FROM mara
    INTO TABLE t_mara
    FOR ALL ENTRIES IN tl_mseg
  WHERE matnr EQ tl_mseg-matnr.

ENDFORM.                    " Z_SELECIONA_MARA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MCHB_ATUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_seleciona_mchb_atual.

  SELECT * FROM mchb AS m
  INTO TABLE it_mchb_atual
WHERE m~matnr IN s_matnr
  AND m~werks IN s_werks
  AND m~lgort IN s_lgort
  AND ( m~clabs GT 0 OR m~cspem GT 0 )
  AND EXISTS ( SELECT *
                 FROM zppt0002 AS z
                WHERE z~acharg EQ m~charg
                  AND z~werks EQ m~werks
                  AND EXISTS ( SELECT *
                                 FROM ztsafrafardos AS s
                                WHERE s~charg EQ p_safra
                                  AND s~werks_from IN s_werks
                                  AND s~data_inicio LE z~budat
                                  AND s~data_fim    GE z~budat ) ).

  IF it_mchb_atual[] IS INITIAL.
    MESSAGE i836 WITH text-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_MCHB_ATUAL

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MSEG_ATUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_seleciona_mseg_atual .

  SELECT * INTO TABLE it_mseg_atual
  FROM mseg
    FOR ALL ENTRIES IN it_mchb_atual
  WHERE matnr EQ it_mchb_atual-matnr
    AND werks EQ it_mchb_atual-werks
    AND lgort EQ it_mchb_atual-lgort
    AND charg EQ it_mchb_atual-charg.
  "AND ( BWART EQ C_131 OR BWART EQ C_311 ).

  SORT: it_mseg_atual BY charg.

  DELETE ADJACENT DUPLICATES FROM it_mseg_atual COMPARING charg.

  LOOP AT it_mseg_atual.
    MOVE-CORRESPONDING it_mseg_atual TO t_msegpp.
    MOVE-CORRESPONDING it_mseg_atual TO t_mseg.
    APPEND t_msegpp.
    APPEND t_mseg.
  ENDLOOP.

ENDFORM.                    " Z_SELECIONA_MSEG_ATUAL

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZMMT0027
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM z_seleciona_zmmt0027.

  FREE: t_prod_ninter.

*-CS2022000332-#82301-01.08.2022-JT-inicio
  FREE: r_cpint.
  IF p_cpint IS NOT INITIAL.
    r_cpint-sign   = 'I'.
    r_cpint-option = 'EQ'.
    r_cpint-low    = p_cpint.
    APPEND r_cpint.
  ENDIF.
*-CS2022000332-#82301-01.08.2022-JT-fim

  SELECT * FROM zmmt0027
    INTO TABLE it_zmmt0027
  WHERE werks IN s_werks
    AND matnr IN s_matnr
    AND safra EQ p_safra
    AND lgort IN s_lgort
    AND lifnr IN r_cpint.  "*-CS2022000332-#82301-01.08.2022-JT-inicio

  "*-CS2022000332-#82301-01.08.2022-JT-inicio
  t_prod_ninter[] = it_zmmt0027[].
  DELETE t_prod_ninter WHERE adquirido_terc IS INITIAL.

  SORT t_prod_ninter BY charg(3).
  DELETE ADJACENT DUPLICATES FROM t_prod_ninter
                        COMPARING charg(3).
  "*-CS2022000332-#82301-01.08.2022-JT-fim

ENDFORM.                    " Z_SELECIONA_ZMMT0027
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_COD
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM z_seleciona_cod . "ADD - 25.08.2016 Seleciona o Código do Beneficiador baseado no centro atual da mala

  CLEAR vg_cod.

  SELECT SINGLE matnr
    FROM ztsafrafardos
    INTO vg_cod
   WHERE werks_to = s_werks-low
     AND charg    = p_safra
     AND status   = 'L'.

*-CS2022000332-#82301-01.08.2022-JT-inicio
  IF sy-subrc <> 0 AND t_prod_ninter[] IS NOT INITIAL.
    LOOP AT t_prod_ninter INTO w_prod_ninter.
      SELECT cod_gs1
        INTO vg_cod
        FROM zppt0033
          UP TO 1 ROWS
       WHERE lifnr  = w_prod_ninter-lifnr
         AND safra  = w_prod_ninter-safra
         AND digito = w_prod_ninter-charg(3).
      ENDSELECT.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
*-CS2022000332-#82301-01.08.2022-JT-fim

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_COLUNAS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM z_ajusta_colunas TABLES t_saida  STRUCTURE zmmr0001.  "ADD - 05.03.2018 - Valida as colunas selecionadas  ( ELG LEAF CG )

  IF ch_elg IS INITIAL.
    LOOP AT t_saida.
      CLEAR t_saida-elg.
      MODIFY t_saida.
    ENDLOOP.
  ENDIF.

  IF ch_leaf IS INITIAL.
    LOOP AT t_saida.
      CLEAR t_saida-leaf.
      MODIFY t_saida.
    ENDLOOP.
  ENDIF.

  IF ch_cg IS INITIAL.
    LOOP AT t_saida.
      CLEAR t_saida-cg.
      MODIFY t_saida.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_EXEC_CD_SAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_exec_cd_sai TABLES p_saida STRUCTURE zmmr0001.

  SELECT acharg, werks, cd_sai
    INTO TABLE @DATA(it_sai)
    FROM zppt0002
      FOR ALL ENTRIES IN @p_saida
    WHERE acharg EQ @p_saida-fardos
      AND werks  EQ @p_saida-werks.  "*-CS2022000332-#82301-01.08.2022-JT-inicio

  LOOP AT p_saida ASSIGNING FIELD-SYMBOL(<f_saida>).
    TRY .
        <f_saida>-cd_sai = it_sai[ acharg = <f_saida>-fardos
                                   werks  = <f_saida>-werks  ]-cd_sai.  ""*-CS2022000332-#82301-01.08.2022-JT-inicio
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDLOOP.

ENDFORM.

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv2.

  IF g_custom_container2 IS INITIAL.
    CREATE OBJECT g_custom_container2 EXPORTING container_name = g_container2.
    CREATE OBJECT g_grid2 EXPORTING i_parent = g_custom_container2.
  ENDIF.

* FREE pt_exclude.

  w_layout-zebra        = abap_false.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.

  t_fieldcatalog[] = t_fcat_lvc[].

* PERFORM toolbar_alv2.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid2->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude2
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
*     it_sort              = lt_sort
      it_outtab            = t_saida.

*  CALL METHOD g_grid2->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

*  IF m_event_handler2 IS INITIAL.
*    CREATE OBJECT m_event_handler2.
*    SET HANDLER : m_event_handler2->toolbar FOR g_grid2.
*    SET HANDLER : m_event_handler2->user_command FOR g_grid2.
*  ENDIF.

* CALL METHOD g_grid->refresh_table_display.

ENDFORM.                    " init_tree

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'ZMMR0001'.
  SET TITLEBAR 'ZMMR0001'.

  PERFORM init_alv2.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
