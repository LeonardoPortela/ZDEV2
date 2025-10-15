*----------------------------------------------------------------------*
* Programa..: ZMMR0170                                                 *
* Tipo......: Report                                                   *
* Transação.: ZMM0170                                                  *
* Descrição.: Relatório de Estoques - Giro, Valor Estoque e Cobertura  *
*             de Estoque para que seja exportado para o B.I., onde     *
*             onde serão efetivamente criados relatórios de análises   *
*             de estoques em nível de depósitos                        *
* Autor.....: Sara Oikawa                                              *
* Data......: 13.08.2020                                               *
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data       | Change     | Autor        | Alteração                   *
*----------------------------------------------------------------------*
* 13.08.20   |DEVK9A0MPT  |Sara Oikawa   | Codificação Inicial         *
*----------------------------------------------------------------------*
REPORT zmmr0170_tst  MESSAGE-ID z01.

*&---------------------------------------------------------------------*
*& Tabelas Transparentes                                               *
*&---------------------------------------------------------------------*
TABLES: mara    , "Dados gerais de material
        makt    , "Textos breves de material
        mardh   , "Segmento de depósito mestre de material - histórico
        s031    , "Estatística: movimento de estoques atuais
        ckmlhd  , "Ledger de material: registro de cabeçalho
        ckmlpp  , "Ledger de material: quantidades de regs.totais do período
        ckmlcr  , "Ledger de material: valores dos regs.totais do período
        zmmt0130. "Tabela Estoques ( exportação B.I. )


*&---------------------------------------------------------------------*
*& Declaração de Tipos                                                 *
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_saida_alv.
         INCLUDE STRUCTURE zmmt0130.
TYPES: END OF ty_saida_alv.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         mtart TYPE mara-mtart,
         meins TYPE mara-meins,
       END OF ty_mara,

       BEGIN OF ty_makt,
         matnr TYPE mara-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt,

       BEGIN OF ty_mseg,
         mblnr      TYPE mseg-mblnr,
         mjahr      TYPE mseg-mjahr,
         zeile      TYPE mseg-zeile,
         matnr      TYPE mseg-matnr,
         werks      TYPE mseg-werks,
         shkzg      TYPE mseg-shkzg,
         budat_mkpf TYPE mseg-budat_mkpf,
         bwart      TYPE mseg-bwart,
       END OF ty_mseg,

       BEGIN OF ty_mseg_sum,
         budat_mkpf TYPE mseg-budat_mkpf,
         mblnr      TYPE mseg-mblnr,
         mjahr      TYPE mseg-mjahr,
         zeile      TYPE mseg-zeile,
         werks      TYPE mseg-werks,
         matnr      TYPE mseg-matnr,
         lgort      TYPE mseg-lgort,
         shkzg      TYPE mseg-shkzg,
         menge      TYPE mseg-menge,
         dmbtr      TYPE mseg-dmbtr,
         spmon      TYPE s031-spmon,
       END OF ty_mseg_sum,

       BEGIN OF ty_mardh,
         matnr TYPE mardh-matnr,
         werks TYPE mardh-werks,
         lgort TYPE mardh-lgort,
         lfgja TYPE mardh-lfgja,
         lfmon TYPE mardh-lfmon,
         labst TYPE mardh-labst,
       END OF ty_mardh,

       BEGIN OF ty_mardh_aux,
         matnr TYPE mardh-matnr,
         werks TYPE mardh-werks,
       END OF ty_mardh_aux,

       BEGIN OF ty_ckmlhd,
         kalnr TYPE ckmlhd-kalnr,
         matnr TYPE ckmlhd-matnr,
         bwkey TYPE ckmlhd-bwkey,
       END OF ty_ckmlhd,

       BEGIN OF ty_ckmlpp,
         kalnr  TYPE ckmlpp-kalnr,
         bdatj  TYPE ckmlpp-bdatj,
         poper  TYPE ckmlpp-poper,
         lbkum  TYPE ckmlpp-lbkum,
         abkumo TYPE ckmlpp-abkumo,
       END OF ty_ckmlpp,

       BEGIN OF ty_ckmlcr,
         kalnr   TYPE ckmlcr-kalnr,
         bdatj   TYPE ckmlcr-bdatj,
         poper   TYPE ckmlcr-poper,
         salk3   TYPE ckmlcr-salk3,
         absalk3 TYPE ckmlcr-absalk3,
       END OF ty_ckmlcr,

       BEGIN OF ty_s031,
         spmon TYPE s031-spmon,
         werks TYPE s031-werks,
         matnr TYPE s031-matnr,
         lgort TYPE s031-lgort,
         mgvbr TYPE s031-mgvbr,
         wgvbr TYPE s031-wgvbr,
         mzubb TYPE s031-mzubb,
         magbb TYPE s031-magbb,
       END OF ty_s031,

       BEGIN OF ty_s039x,
         spmon   TYPE s039-spmon,
         werks   TYPE s039-werks,
         matnr   TYPE s039-matnr,
         dbwbest TYPE s039-dbwbest,
       END OF ty_s039x,

       BEGIN OF ty_s039,
         ssour     TYPE s039-ssour,
         vrsio     TYPE s039-vrsio,
         spmon     TYPE s039-spmon,
         sptag     TYPE s039-sptag,
         spwoc     TYPE s039-spwoc,
         spbup     TYPE s039-spbup,
         werks     TYPE s039-werks,
         matnr     TYPE s039-matnr,
         lgort     TYPE s039-lgort,
         mbwbest   TYPE s039-mbwbest,
         dbwbest   TYPE s039-dbwbest,
         wbwbest   TYPE s039-wbwbest,
         rwbewbest TYPE s039-rwbewbest,
       END OF ty_s039.


*&---------------------------------------------------------------------*
*& Declaração de Tabelas Internas / Estruturas                         *
*&---------------------------------------------------------------------*

DATA: t_mara       TYPE TABLE OF ty_mara,
      t_mard       TYPE TABLE OF mard,
*      t_mard_w     TYPE TABLE OF mard,
      t_makt       TYPE TABLE OF ty_makt,
      t_mardh      TYPE TABLE OF ty_mardh,
      t_mardh_aux  TYPE TABLE OF ty_mardh_aux,
      t_mseg       TYPE TABLE OF ty_mseg,
      t_mseg_cons  TYPE TABLE OF ty_mseg,
      t_mseg_sum   TYPE TABLE OF ty_mseg_sum,
      t_mseg_aux   TYPE TABLE OF ty_mseg_sum,
      t_mseg_tot   TYPE TABLE OF ty_mseg_sum,

      t_ckmlhd     TYPE TABLE OF ty_ckmlhd,
      t_ckmlpp     TYPE TABLE OF ty_ckmlpp,
      t_ckmlcr     TYPE TABLE OF ty_ckmlcr,
      t_s031       TYPE TABLE OF ty_s031,
      t_s039       TYPE TABLE OF ty_s039,
      t_s039_tot   TYPE TABLE OF ty_s039x,
      t_zmmt0130   TYPE TABLE OF zmmt0130,

      wa_mara      TYPE ty_mara,
      wa_makt      TYPE ty_makt,
      wa_mardh     TYPE ty_mardh,
      wa_mardh_aux TYPE ty_mardh_aux,
      wa_ckmlhd    TYPE ty_ckmlhd,
      wa_ckmlpp    TYPE ty_ckmlpp,
      wa_ckmlcr    TYPE ty_ckmlcr,
      wa_s031      TYPE ty_s031,
      wa_s039      TYPE ty_s039,
      wa_s039_tot  TYPE ty_s039x,
      wa_zmmt0130  TYPE zmmt0130.


*&---------------------------------------------------------------------*
*& Declaração de Variáveis                                             *
*&---------------------------------------------------------------------*


DATA: dg_splitter     TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer TYPE REF TO cl_gui_container,
      ctl_alv         TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog TYPE lvc_t_fcat,
      gs_variant      TYPE disvariant,
      gs_layout       TYPE lvc_s_layo,
      t_usermd        TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      it_saida        TYPE TABLE OF ty_saida_alv.

DATA: v_poper    TYPE  poper,
      v_spmon    TYPE  spmon,
      v_qtdias   TYPE  i,
      v_check(1).

"Ranges
DATA: r_lfgja  TYPE RANGE OF lfgja,
      ls_lfgja LIKE LINE  OF r_lfgja,

      r_spmon  TYPE RANGE OF spmon,
      ls_spmon LIKE LINE  OF r_spmon,

      r_poper  TYPE RANGE OF poper,
      ls_poper LIKE LINE  OF r_poper.

*&---------------------------------------------------------------------*
*& Tela de Seleção                                                     *
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-s01.

  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s02.
    SELECT-OPTIONS: s_werks FOR s031-werks OBLIGATORY .
    SELECT-OPTIONS: s_lgort FOR s031-lgort.
    SELECT-OPTIONS: s_matnr FOR mara-matnr.
  SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s03.
    SELECT-OPTIONS: s_mtart FOR mara-mtart.
    SELECT-OPTIONS: s_matkl FOR mara-matkl.
  SELECTION-SCREEN END OF BLOCK b2.


  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s04.
    SELECT-OPTIONS:  s_spmon FOR s031-spmon OBLIGATORY  .
  SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN END OF BLOCK sel.

PARAMETERS: r_s039   LIKE bsid-umskz AS CHECKBOX  DEFAULT ' ' MODIF ID a.
PARAMETERS: r_off    LIKE bsid-umskz AS CHECKBOX  DEFAULT 'X' MODIF ID a.
*&---------------------------------------------------------------------*
*&       P R O C E S S A M E N T O                                     *
*&---------------------------------------------------------------------*
INITIALIZATION.

  "Atribui Periodo Atual default

  s_spmon-low  = sy-datum(06).
  APPEND s_spmon.

  " Usuários que podem mudar o Status
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZMM0170'
    TABLES
      set_values    = t_usermd
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.

  ENDIF.
  v_check = 'S'.
  SORT t_usermd BY from.
  READ TABLE t_usermd WITH KEY from = sy-uname.
  IF sy-subrc NE 0.
    v_check = 'N'.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF v_check = 'S'.
      IF screen-group1 = 'A'.
        screen-active = 1.
      ENDIF.
    ELSE.
      IF screen-group1 = 'A'.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.

  ENDLOOP.

START-OF-SELECTION.

*** Trata Parâmetros

  " Intervalo por Ano
  LOOP AT s_spmon.
    ls_lfgja-sign   = s_spmon-sign.
    ls_lfgja-option = s_spmon-option.
    ls_lfgja-low    = s_spmon-low(4).
    ls_lfgja-high   = s_spmon-high(4).
    APPEND ls_lfgja TO r_lfgja.
    ls_poper-sign   = s_spmon-sign.
    ls_poper-option = s_spmon-option.
    ls_poper-low    = s_spmon-low+4(2).
    ls_poper-high   = s_spmon-high+4(2).
    APPEND ls_poper TO r_poper.
  ENDLOOP.

  "Compor Período
  LOOP AT s_spmon.

    IF ls_spmon-low IS INITIAL.
      ls_spmon-low = s_spmon-low.
    ENDIF.

    IF NOT s_spmon-high IS INITIAL.
      DO.
        ls_spmon-sign   = 'I'.
        ls_spmon-option = 'EQ'.
        ls_spmon-low    = ls_spmon-low.
        APPEND ls_spmon TO r_spmon.

        ADD 1 TO ls_spmon-low.
        IF ls_spmon-low+4(2) > 12.
          ls_spmon-low+4(2) = 01.
          ADD 1 TO ls_spmon-low(4).
        ENDIF.

        IF s_spmon-high IS NOT INITIAL.
          IF ls_spmon-low GT s_spmon-high.
            EXIT.
          ENDIF.
        ENDIF.
      ENDDO.

    ELSE.
      ls_spmon-sign   = 'I'.
      ls_spmon-option = 'EQ'.
      ls_spmon-low    = ls_spmon-low.
      APPEND ls_spmon TO r_spmon.
    ENDIF.

  ENDLOOP.

  IF r_off  = 'X' OR v_check = 'N'.
    SELECT *
    FROM zmmt0130 AS t130
    INNER JOIN mara
    ON t130~matnr  = mara~matnr
    INTO CORRESPONDING FIELDS OF TABLE t_zmmt0130
   WHERE mara~matnr IN s_matnr
     AND mara~matkl IN s_matkl
     AND mara~mtart IN s_mtart
     AND t130~spmon IN s_spmon
     AND t130~werks IN s_werks
    AND t130~lgort IN s_lgort.
  ELSE.
*** Seleção dos dados
    PERFORM f_seleciona_dados.
*** Processa dados obtidos
    PERFORM f_processa_dados.
  ENDIF.

*** Chama tela do relatório
  IF sy-batch IS INITIAL.
    PERFORM f_exibe_relatorio.
  ENDIF.

*&---------------------------------------------------------------------*
*&    FORMS                                                            *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .



  IF NOT s_matnr[] IS INITIAL OR NOT s_matkl[] IS INITIAL
     OR NOT s_mtart[] IS INITIAL.

*.. Obtém dados do material
    SELECT DISTINCT mara~matnr mara~matkl mtart meins
      FROM mara
      INNER JOIN mard
      ON mard~matnr  = mara~matnr
      INTO TABLE t_mara
     WHERE mara~matnr IN s_matnr
       AND mard~werks IN s_werks
       AND mard~lgort IN s_lgort
       AND matkl IN s_matkl
    AND mtart IN s_mtart.
    IF sy-subrc NE 0.
      "Nenhum material encontrado para a seleção
      MESSAGE s012 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    SORT t_mara BY matnr.
*.. obtém dados s039
    PERFORM f_trata_s039.

    "informações mseg.
    PERFORM f_seleciona_mseg.
  ELSE.
    "Informe um dos parâmetros de "Materiais".
    MESSAGE s013 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF t_mara[] IS NOT INITIAL.
    SELECT matnr maktx
      FROM makt
      INTO TABLE t_makt
      FOR ALL ENTRIES IN t_mara
      WHERE matnr EQ t_mara-matnr
    AND spras EQ sy-langu.

    SORT t_makt BY matnr.
  ENDIF.

  "Selecionando dados...
  PERFORM f_progress USING 20 TEXT-m01.

  IF t_mara[] IS NOT INITIAL.
*.. Obtém dados MARDH
    SELECT matnr werks lgort lfgja lfmon labst
      FROM mardh
      INTO TABLE t_mardh
       FOR ALL ENTRIES IN t_mara
           WHERE matnr EQ t_mara-matnr
             AND werks IN s_werks
             AND lgort IN s_lgort
             AND lfgja IN r_lfgja
    AND lfmon BETWEEN s_spmon-low+4(2) AND s_spmon-high+4(2).
*    IF sy-subrc NE 0.
*      "Nenhum material encontrado para a seleção
*      MESSAGE s012 DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.

    SORT t_mardh BY matnr werks lgort lfgja lfmon.
  ENDIF.

  IF t_s039[] IS NOT INITIAL.

    "Selecionando dados...
    PERFORM f_progress USING 40 TEXT-m05.

    SELECT spmon werks matnr lgort mgvbr wgvbr mzubb magbb
      FROM s031
      INTO TABLE t_s031
      FOR ALL ENTRIES IN t_s039
    WHERE ssour EQ t_s039-ssour
      AND vrsio EQ t_s039-vrsio
      AND spmon EQ t_s039-spmon
      AND sptag EQ t_s039-sptag
      AND spwoc EQ t_s039-spwoc
      AND spbup EQ t_s039-spbup
      AND werks EQ t_s039-werks
      AND matnr EQ t_s039-matnr
    AND lgort EQ t_s039-lgort.
  ENDIF.

  SORT t_s031 BY spmon werks matnr lgort.

  "Selecionando dados...
  PERFORM f_progress USING 40 TEXT-m01.

*.. Obtém Ledger de material: registro de cabeçalho
  IF t_mara[] IS NOT INITIAL.
    SELECT kalnr matnr bwkey
      FROM ckmlhd
      INTO TABLE t_ckmlhd
       FOR ALL ENTRIES IN t_mara
     WHERE matnr EQ t_mara-matnr
    AND bwkey IN s_werks.
    IF sy-subrc NE 0.
      MESSAGE s012 DISPLAY LIKE 'E'.
      "Nenhum material encontrado para a seleção
      LEAVE LIST-PROCESSING.
    ELSE.
      SORT t_ckmlhd BY matnr bwkey .
    ENDIF.
  ENDIF.

  "Selecionando dados...
  PERFORM f_progress USING 60 TEXT-m01.

*  IF t_ckmlhd[] IS NOT INITIAL.
*    DATA: wa_kalnr  TYPE ckmv0_matobj_str,
*          lt_kalnr  TYPE ckmv0_matobj_tbl,
*          lt_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE,
*          lt_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE.
*
*    DATA: lv_bdatj_1 TYPE  ckmlpp-bdatj,
*          lv_poper_1 TYPE  ckmlpp-poper,
*          lv_jahrper TYPE mldoc-jahrper.
*
*    LOOP AT t_ckmlhd INTO DATA(wa_ckmlhd).
*      wa_kalnr-kalnr = wa_ckmlhd-kalnr.
*      APPEND wa_kalnr TO lt_kalnr.
*    ENDLOOP.
*
*    IF s_spmon-high IS INITIAL.
*      lv_bdatj_1 = s_spmon-low+0(4).
*      lv_poper_1 = s_spmon-low+4(2).
*    ELSE.
*      lv_bdatj_1 = s_spmon-high+0(4).
*      lv_poper_1 = s_spmon-high+4(2).
*    ENDIF.
*    CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
*      EXPORTING
*        i_bdatj_1               = lv_bdatj_1
*        i_poper_1               = lv_poper_1
*      TABLES
*        t_kalnr                 = lt_kalnr
*        t_ckmlpp                = lt_ckmlpp
*        t_ckmlcr                = lt_ckmlcr
*      EXCEPTIONS
*        no_data_found           = 1
*        input_data_inconsistent = 2
*        buffer_inconsistent     = 3
*        OTHERS                  = 4.
*
*    IF lt_ckmlpp[] IS NOT INITIAL.
*
*      DELETE lt_ckmlpp WHERE  bdatj NOT IN r_lfgja AND poper NOT IN r_poper.
*
*      IF lines( lt_ckmlpp[] ) > 0.
*
*        MOVE-CORRESPONDING lt_ckmlpp[] TO t_ckmlpp[].
*        sy-dbcnt = lines( lt_ckmlpp[] ).
*      ELSE.
*        sy-subrc = 4.
*        sy-dbcnt = 0.
*      ENDIF.
*    ENDIF.
*    SORT t_ckmlpp BY kalnr bdatj poper.
*  ENDIF.


*  IF lt_ckmlcr[] IS NOT INITIAL.
*
*    DELETE lt_ckmlcr WHERE  curtp NE '10'.
*
*    IF lines( lt_ckmlcr[] ) > 0.
*
*      MOVE-CORRESPONDING lt_ckmlcr[] TO t_ckmlcr[].
*      sy-dbcnt = lines( lt_ckmlcr[] ).
*    ELSE.
*      sy-subrc = 4.
*      sy-dbcnt = 0.
*    ENDIF.
*  ENDIF.
*
*  SORT t_ckmlcr BY kalnr bdatj poper.


  IF t_mara[] IS INITIAL OR
     t_s039[] IS INITIAL.
    "Nenhum material encontrado para a seleção
    MESSAGE s012 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.         "F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_PROGRESS
*&---------------------------------------------------------------------*
*       Indicador do processamento
*----------------------------------------------------------------------*
*      -->P_PERCENTAGE - Percentual do processamento
*      -->P_TEXT       - Texto do Processamento
*----------------------------------------------------------------------*
FORM f_progress USING p_percentage TYPE any
                      p_text       TYPE any.

  CHECK sy-batch IS INITIAL.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percentage
      text       = p_text.

ENDFORM.                    " F_PROGRESS

*&---------------------------------------------------------------------*
*&      Form  F_QUANTIDADE_DIAS_MES
*&---------------------------------------------------------------------*
*       Obtém Qtde dias do mês
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_quantidade_dias_mes .

  DATA: lv_mes TYPE  i,
        lv_ano TYPE  i.

*
*  LV_ANO = P_LFGJA.
*  LV_MES = P_LFMON.

  CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
    EXPORTING
      i_date_month = lv_mes
      i_date_year  = lv_ano
    IMPORTING
      e_max_days   = v_qtdias.

*  IF V_QTDIAS  IS INITIAL.
*     V_ERRO = 'X'.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_processa_dados .

  DATA: lv_cont        TYPE sy-tfill,
        lv_tot_estqmed TYPE s039-dbwbest,
        lv_fator       TYPE reexcndfactor,
        lv_butag       TYPE t009b-butag,
        it_datas       TYPE TABLE OF iscal_day,
        v_datai        TYPE sy-datum,
        v_dataf        TYPE sy-datum,
        v_low          TYPE rsparams-low,
        v_low2(2).

  REFRESH: t_zmmt0130.

*  PERFORM F_QUANTIDADE_DIAS_MES .
  SORT t_s039 BY ssour
                 vrsio
                 spmon
                 sptag
                 spwoc
                 spbup
                 werks
                 matnr
                 lgort.

  LOOP AT t_s039 INTO wa_s039.
    IF wa_s039-spmon NOT IN s_spmon.
      CONTINUE.
    ENDIF.
    CLEAR lv_tot_estqmed.

    READ TABLE t_s039_tot INTO wa_s039_tot WITH KEY spmon  = wa_s039-spmon
                                                   werks  = wa_s039-werks
                                                   matnr  = wa_s039-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      lv_tot_estqmed = wa_s039_tot-dbwbest.
    ENDIF.

    CLEAR  wa_mardh.
    READ TABLE t_mardh INTO wa_mardh WITH KEY matnr = wa_s039-matnr
                                              werks = wa_s039-werks
                                              lgort = wa_s039-lgort
                                              lfgja = wa_s039-spmon(04)
                                              lfmon = wa_s039-spmon+4(02).

    IF sy-subrc IS NOT INITIAL.
      wa_mardh-matnr = wa_s039-matnr.
      wa_mardh-werks = wa_s039-werks.
      wa_mardh-lgort = wa_s039-lgort.
      wa_mardh-labst = wa_s039-mbwbest.
    ENDIF.

    CLEAR:   wa_zmmt0130, wa_makt, wa_s031, wa_ckmlhd, wa_ckmlpp, wa_ckmlcr.

*    CONCATENATE wa_mardh-lfgja wa_mardh-lfmon INTO v_spmon.

*    IF v_spmon IN s_spmon.

    READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_mardh-matnr
                                   BINARY SEARCH.

    READ TABLE t_makt INTO wa_makt WITH KEY matnr = wa_mara-matnr
                                   BINARY SEARCH.

    READ TABLE t_ckmlhd INTO wa_ckmlhd WITH KEY matnr = wa_mardh-matnr
                                                bwkey = wa_mardh-werks
                                       BINARY SEARCH.

*    CONCATENATE '0' wa_mardh-lfmon INTO v_poper.
    CONCATENATE '0' wa_s039-spmon+4(02) INTO v_poper.

*    READ TABLE t_ckmlpp INTO wa_ckmlpp WITH KEY kalnr = wa_ckmlhd-kalnr
**                                                bdatj = wa_s039-spmon(4)
**                                                poper = v_poper
*                                       BINARY SEARCH.
*
*    READ TABLE t_ckmlcr INTO wa_ckmlcr WITH KEY kalnr = wa_ckmlpp-kalnr
**                                                bdatj = wa_ckmlpp-bdatj
**                                                poper = wa_ckmlpp-poper
*                                       BINARY SEARCH.

    READ TABLE t_s031 INTO wa_s031 WITH KEY spmon = wa_s039-spmon
                                            werks = wa_mardh-werks
                                            matnr = wa_mardh-matnr
                                            lgort = wa_mardh-lgort
                                   BINARY SEARCH.

    wa_zmmt0130-spmon = wa_s039-spmon.
    wa_zmmt0130-werks = wa_mardh-werks.
    wa_zmmt0130-lgort = wa_mardh-lgort.
    wa_zmmt0130-matnr = wa_mardh-matnr.
    wa_zmmt0130-maktx = wa_makt-maktx.
    wa_zmmt0130-meins = wa_mara-meins.


    "Quantidade de estoque avaliado
    wa_zmmt0130-lbkum = wa_mardh-labst.

    "Valor do estoque avaliado
*    IF NOT wa_ckmlpp-lbkum IS INITIAL.
    wa_zmmt0130-salk3 = wa_s039-wbwbest. "wa_ckmlcr-salk3 * ( wa_mardh-labst / wa_ckmlpp-lbkum ).
*    ENDIF.

    "Estoque médio avaliado
    wa_zmmt0130-estmdavl = wa_s039-dbwbest.

    "Valor médio do estoque (avaliado)
    lv_fator = 0.
    TRY.
        IF lv_tot_estqmed GT 0.
          lv_fator = wa_s039-dbwbest / lv_tot_estqmed.
        ENDIF.
      CATCH cx_sy_conversion_overflow cx_sy_arithmetic_overflow.
        lv_fator = 0.
    ENDTRY.

*    TRY.
*        wa_zmmt0130-vlrmdest = ( ( wa_ckmlcr-absalk3 + wa_ckmlcr-salk3 ) / 2 ) * lv_fator.
*      CATCH cx_sy_conversion_overflow cx_sy_arithmetic_overflow.
*        wa_zmmt0130-vlrmdest = 0.
*    ENDTRY.

    "Mes anterior
    v_low = wa_s039-spmon.
    IF v_low+4(2) = 1.
      v_low+4(2) = 12.
      SUBTRACT 1 FROM v_low(4).
    ELSE.
      SUBTRACT 1 FROM v_low+4(2).
    ENDIF.
    v_low2 = v_low+4(2).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_low2
      IMPORTING
        output = v_low2.
    CONCATENATE v_low(4) v_low2 INTO v_low.

    READ TABLE t_s039 INTO DATA(_s39_ant) WITH KEY ssour  = wa_s039-ssour
                                               vrsio  = wa_s039-vrsio
                                               spmon  = v_low
                                               sptag  = wa_s039-sptag
                                               spwoc  = wa_s039-spwoc
                                               spbup  = wa_s039-spbup
                                               werks  = wa_s039-werks
                                               matnr  = wa_s039-matnr
                                               lgort  = wa_s039-lgort BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR _s39_ant.
    ENDIF.


    TRY.
        wa_zmmt0130-vlrmdest = ( ( _s39_ant-wbwbest +  wa_s039-wbwbest ) / 2 ) .
      CATCH cx_sy_conversion_overflow cx_sy_arithmetic_overflow.
        wa_zmmt0130-vlrmdest = 0.
    ENDTRY.
    CLEAR _s39_ant.


    "Quantidade total de consumo
    wa_zmmt0130-mgvbr = wa_s031-mgvbr.

    "Valor total de consumo
    wa_zmmt0130-wgvbr = wa_s031-wgvbr.

    CLEAR  wa_zmmt0130-rtestavl.
    TRY.
        IF wa_s039-dbwbest GT 0.
          wa_zmmt0130-rtestavl = ( wa_s031-mgvbr /  wa_s039-dbwbest ).
        ENDIF.
      CATCH cx_sy_conversion_overflow cx_sy_arithmetic_overflow.
        wa_zmmt0130-rtestavl = 99999.
    ENDTRY.

    CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
      EXPORTING
        par_month = wa_s039-spmon+4(2)
        par_year  = wa_s039-spmon+0(4)
      IMPORTING
        par_days  = lv_butag.

    v_qtdias = lv_butag.

    CONCATENATE wa_s039-spmon+0(4) wa_s039-spmon+4(2) '01' INTO v_datai.
    v_dataf = v_datai.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = v_dataf
      IMPORTING
        e_date = v_dataf.

    REFRESH it_datas .
    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar = 'MG'
        factory_calendar = 'ZT'
        date_from        = v_datai
        date_to          = v_dataf
      TABLES
        holidays         = it_datas
      EXCEPTIONS
        OTHERS           = 1.

    DELETE it_datas WHERE  holiday NE 'X'.
    DATA(_feriado) =  lines( it_datas ).

    v_qtdias  = v_qtdias  - _feriado.

    IF wa_s031-mgvbr GT 0 AND v_qtdias GT 0.
      "Cobertura do estoque avaliado
      TRY.
          IF wa_s031-mgvbr GT 0 AND v_qtdias GT 0.
            wa_zmmt0130-cbestavl = ( wa_zmmt0130-lbkum / ( wa_s031-mgvbr / v_qtdias ) ).
          ENDIF.
        CATCH cx_sy_conversion_overflow cx_sy_arithmetic_overflow.
          wa_zmmt0130-cbestavl  = 99999.
      ENDTRY.

      "Cobertura média do estoque avaliado
      TRY.
          IF wa_s031-mgvbr GT 0 AND v_qtdias GT 0.
            wa_zmmt0130-cbmdeavl = ( wa_zmmt0130-estmdavl  / ( wa_s031-mgvbr  / v_qtdias ) ).
          ENDIF.
        CATCH cx_sy_conversion_overflow cx_sy_arithmetic_overflow.
          wa_zmmt0130-cbmdeavl = 99999.
      ENDTRY.

      "Tempo Cobertura
      TRY.
          IF wa_s031-mgvbr GT 0 AND v_qtdias GT 0.
            wa_zmmt0130-tmpcober = ( wa_mardh-labst / ( wa_s031-mgvbr / v_qtdias ) ).
          ENDIF.
        CATCH cx_sy_conversion_overflow cx_sy_arithmetic_overflow.
          wa_zmmt0130-tmpcober = 99999.
      ENDTRY.
    ELSE.
      wa_zmmt0130-cbestavl = 99999.
      wa_zmmt0130-cbmdeavl = 99999.
      wa_zmmt0130-tmpcober = 99999.
    ENDIF.

    "Cobertura valor estoque avaliado
    TRY.
        IF wa_s031-wgvbr GT 0 AND v_qtdias GT 0.
          wa_zmmt0130-cbvleavl  = ( wa_zmmt0130-salk3 / ( wa_s031-wgvbr  / v_qtdias ) ).
        ENDIF.
      CATCH cx_sy_conversion_overflow cx_sy_arithmetic_overflow.
        wa_zmmt0130-cbvleavl = 99999.
    ENDTRY.

    wa_zmmt0130-usnam       = sy-uname.
    wa_zmmt0130-data_atual  = sy-datum.
    wa_zmmt0130-hora_atual  = sy-uzeit.


    "Informações MSEG
    "Entrada
    READ TABLE t_mseg INTO DATA(lwa_mseg)
                      WITH KEY matnr      = wa_zmmt0130-matnr
                               werks      = wa_zmmt0130-werks
                               shkzg = 'S'
                               BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_zmmt0130-budat_entrada = lwa_mseg-budat_mkpf.
      wa_zmmt0130-bwart_entrada = lwa_mseg-bwart.
    ENDIF.

    "Saida
    CLEAR lwa_mseg.
    READ TABLE t_mseg INTO lwa_mseg
                      WITH KEY matnr      = wa_zmmt0130-matnr
                               werks      = wa_zmmt0130-werks
                               shkzg = 'H'
                               BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_zmmt0130-budat_saida = lwa_mseg-budat_mkpf.
      wa_zmmt0130-bwart_saida = lwa_mseg-bwart.
    ENDIF.

    "Saida Ultimo consumo
    CLEAR lwa_mseg.
    READ TABLE t_mseg_cons INTO lwa_mseg
                      WITH KEY matnr      = wa_zmmt0130-matnr
                               werks      = wa_zmmt0130-werks
                               shkzg = 'H'
                               BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_zmmt0130-budat_consumo = lwa_mseg-budat_mkpf.
    ENDIF.

    "Valores Entrada
    READ TABLE t_mseg_sum INTO DATA(lwa_mseg_sum)
                          WITH KEY spmon      = wa_zmmt0130-spmon
                                   werks      = wa_zmmt0130-werks
                                   matnr      = wa_zmmt0130-matnr
                                   lgort      = wa_zmmt0130-lgort
                                   shkzg      = 'S'
                                   BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_zmmt0130-quantidade_entrada = lwa_mseg_sum-menge.
      wa_zmmt0130-valor_entrada      = lwa_mseg_sum-dmbtr.
    ENDIF.

    "Valores Saída
    CLEAR lwa_mseg_sum.
    READ TABLE t_mseg_sum INTO lwa_mseg_sum
                          WITH KEY spmon      = wa_zmmt0130-spmon
                                   werks      = wa_zmmt0130-werks
                                   matnr      = wa_zmmt0130-matnr
                                   lgort      = wa_zmmt0130-lgort
                                   shkzg = 'H'
                                   BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_zmmt0130-quantidade_saida = lwa_mseg_sum-menge.
      wa_zmmt0130-valor_saida      = lwa_mseg_sum-dmbtr.
    ENDIF.


    APPEND wa_zmmt0130 TO t_zmmt0130.

  ENDLOOP.

  " Grava registros na tabela ZMMT0130
  DESCRIBE TABLE t_zmmt0130 LINES lv_cont.

  IF lv_cont IS INITIAL.

    " Não foram encontrados registros para a seleção
    WRITE: / TEXT-m02.

  ELSE.

    MODIFY zmmt0130 FROM TABLE t_zmmt0130.
    COMMIT WORK.

    "Processamento OK. Quantidade de Registros inseridos na tabela ZMMT0130 ='
    WRITE: / TEXT-m03 , lv_cont.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_exibe_relatorio .


  CHECK t_zmmt0130 IS NOT INITIAL.

  it_saida[] = t_zmmt0130.

  CALL SCREEN 0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  M_STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_status_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TL0100'.

  IF dg_splitter IS INITIAL.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = 1
        columns = 1.

    ctl_cccontainer = dg_splitter->get_container( row = 1 column = 1 ).

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = ctl_cccontainer.

    PERFORM fill_it_fieldcatalog.
    PERFORM fill_gs_variant.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_saida[].

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_0100_exit INPUT.

  IF ctl_alv IS NOT INITIAL.
    ctl_alv->free( ).
  ENDIF.
  CLEAR: ctl_alv.

  IF ctl_cccontainer IS NOT INITIAL.
    ctl_cccontainer->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer.

  IF dg_splitter IS NOT INITIAL.
    dg_splitter->free( ).
  ENDIF.
  CLEAR: dg_splitter.

  CLEAR it_saida[].

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_0100 INPUT.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZMMT0130'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

  LOOP AT it_fieldcatalog ASSIGNING <fs_cat>.
    <fs_cat>-tabname = 'ZMMT0130'.
    <fs_cat>-col_opt = abap_true.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.
  gs_layout-zebra        = abap_true.
  gs_layout-sel_mode   = 'A'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-stylefname = 'STYLE'.
  gs_layout-ctab_fname = 'COLOR_CELL'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TRATA_S039
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_trata_s039 .

  DATA: lt_rsparams TYPE TABLE OF rsparams,
        ls_rsparams TYPE rsparams,
        v_low       TYPE rsparams-low,
        v_low2(2).

  v_low = s_spmon-low.
  IF v_low+4(2) = 1.
    v_low+4(2) = 12.
    SUBTRACT 1 FROM v_low(4).
  ELSE.
    SUBTRACT 1 FROM v_low+4(2).
  ENDIF.
  v_low2 = v_low+4(2).
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_low2
    IMPORTING
      output = v_low2.
  CONCATENATE v_low(4) v_low2 INTO v_low.


  IF r_s039 = 'X'.
* Criar registros na tabela S039
    REFRESH lt_rsparams.
    LOOP AT s_werks INTO DATA(w_werks).
      ls_rsparams-selname = 'SL_WERKS'.
      ls_rsparams-kind    = 'S'.
      ls_rsparams-sign    = 'I'.
      ls_rsparams-option  =  w_werks-option.
      ls_rsparams-low     = w_werks-low.
      ls_rsparams-high    = w_werks-high.
      CLEAR ls_rsparams-high.
      APPEND ls_rsparams TO lt_rsparams.
    ENDLOOP.

    "Período
    ls_rsparams-selname = 'SL_SPMON'.
    ls_rsparams-kind    = 'S'.
    ls_rsparams-sign    = 'I'.
    ls_rsparams-option  = s_spmon-option.
    ls_rsparams-low     = v_low.
    ls_rsparams-high    = s_spmon-high.
    APPEND ls_rsparams TO lt_rsparams.

    " Msg Executando Programa ZRMCBS039...
    PERFORM f_progress USING 40 TEXT-m04.

    SUBMIT zrmcbs039  WITH SELECTION-TABLE lt_rsparams
                      AND RETURN.

  ENDIF.

  SELECT * FROM mard
    INTO TABLE t_mard
    FOR ALL ENTRIES IN t_mara
  WHERE matnr EQ t_mara-matnr
  AND   werks IN s_werks.

  CHECK t_mard[] IS NOT INITIAL.

  SORT t_mard BY matnr werks.
  DELETE ADJACENT DUPLICATES FROM t_mard COMPARING matnr werks.


*.. Obtém dados S039
  REFRESH t_s039.
  SELECT ssour vrsio spmon sptag spwoc spbup
         werks matnr lgort mbwbest dbwbest wbwbest rwbewbest
    INTO TABLE t_s039
    FROM s039
     FOR ALL ENTRIES IN t_mard
   WHERE vrsio = '000'
    AND spmon IN s_spmon
    AND werks EQ t_mard-werks
    AND matnr EQ t_mard-matnr
    AND lgort IN s_lgort.

  SELECT ssour vrsio spmon sptag spwoc spbup
          werks matnr lgort mbwbest dbwbest wbwbest rwbewbest
     APPENDING TABLE t_s039
     FROM s039
      FOR ALL ENTRIES IN t_mard
    WHERE vrsio = '000'
     AND spmon EQ v_low
     AND werks EQ t_mard-werks
     AND matnr EQ t_mard-matnr
     AND lgort IN s_lgort.

  SORT t_s039 BY ssour vrsio spmon sptag spwoc
                 spbup werks matnr lgort.


  LOOP AT t_s039 INTO wa_s039.
    MOVE-CORRESPONDING wa_s039 TO wa_s039_tot.
    COLLECT wa_s039_tot INTO t_s039_tot.
  ENDLOOP.
  SORT t_s039_tot BY spmon werks matnr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_MSEG
*&---------------------------------------------------------------------*
FORM f_seleciona_mseg .

  DATA: lva_day_in     TYPE sy-datum,
        lva_last_day   TYPE sy-datum,
        lva_day_inicio TYPE sy-datum.

  DATA: lra_spmon TYPE RANGE OF s031-spmon,
        lwa_spmon LIKE LINE OF lra_spmon.


  DATA: lwa_mseg_sum LIKE LINE OF t_mseg_sum,
        lwa_mseg_tot LIKE LINE OF t_mseg_sum,
        lwa_mseg_aux LIKE LINE OF t_mseg_sum.

  lra_spmon[] = s_spmon[].
  SORT: lra_spmon BY high DESCENDING low DESCENDING.

  "Pega o último dia do mês com base na tela de seleção
  READ TABLE lra_spmon INTO lwa_spmon INDEX 1.
  IF sy-subrc IS INITIAL.
    IF lwa_spmon-high IS NOT INITIAL.
      CONCATENATE lwa_spmon-high
                  '01' INTO lva_day_in.
    ELSE.
      CONCATENATE lwa_spmon-low
                  '01' INTO lva_day_in.
    ENDIF.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lva_day_in
      IMPORTING
        last_day_of_month = lva_last_day
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    CONCATENATE lwa_spmon-low
            '01' INTO lva_day_inicio.

  ENDIF.

  IF t_mard[] IS NOT INITIAL.
    SELECT mseg~mblnr  mseg~mjahr mseg~zeile mseg~matnr mseg~werks mseg~shkzg mkpf~budat mseg~bwart
      FROM mseg
      INNER JOIN mkpf
      ON  mkpf~mblnr = mseg~mblnr
      AND mkpf~mjahr = mseg~mjahr
      INTO TABLE t_mseg
      FOR ALL ENTRIES IN t_mard
      WHERE mseg~werks = t_mard-werks
        AND mseg~matnr = t_mard-matnr
        AND mseg~shkzg IN ('S', 'H')
    AND mkpf~budat <= lva_last_day.
    SORT t_mseg BY matnr ASCENDING
                   werks ASCENDING
                   shkzg ASCENDING
                   budat_mkpf DESCENDING.

    t_mseg_cons = t_mseg.
    DELETE t_mseg_cons WHERE bwart NE '201' AND bwart NE '261'.
    SORT t_mseg_cons BY matnr ASCENDING
                        werks ASCENDING
                        shkzg ASCENDING
                        budat_mkpf DESCENDING.


    SELECT mkpf~budat mseg~mblnr  mseg~mjahr mseg~zeile mseg~werks mseg~matnr mseg~lgort mseg~shkzg mseg~menge mseg~dmbtr
      FROM mseg
       INNER JOIN mkpf
        ON  mkpf~mblnr = mseg~mblnr
        AND mkpf~mjahr = mseg~mjahr
     INTO TABLE t_mseg_sum
     FOR ALL ENTRIES IN t_mard
      WHERE mseg~werks = t_mard-werks
        AND mseg~matnr = t_mard-matnr
        AND mseg~shkzg IN ('S', 'H')
        AND mkpf~budat BETWEEN lva_day_inicio AND lva_last_day.



    t_mseg_aux = t_mseg_sum.
    REFRESH: t_mseg_sum.

    LOOP AT t_mseg_aux INTO lwa_mseg_aux.
      CONCATENATE lwa_mseg_aux-budat_mkpf+0(4) lwa_mseg_aux-budat_mkpf+4(2)  INTO lwa_mseg_aux-spmon.
      CONDENSE lwa_mseg_aux-spmon NO-GAPS.
      MODIFY t_mseg_aux FROM  lwa_mseg_aux  INDEX sy-tabix TRANSPORTING spmon.
    ENDLOOP.

    SORT t_mseg_aux BY spmon werks matnr lgort shkzg.
    t_mseg_tot = t_mseg_aux.


    DELETE ADJACENT DUPLICATES FROM t_mseg_aux COMPARING spmon werks matnr lgort shkzg.

    LOOP AT t_mseg_aux INTO lwa_mseg_aux.
      CLEAR: lwa_mseg_sum.
      LOOP AT t_mseg_tot INTO lwa_mseg_tot WHERE spmon = lwa_mseg_aux-spmon
                                                  AND werks = lwa_mseg_aux-werks
                                                  AND matnr = lwa_mseg_aux-matnr
                                                  AND lgort = lwa_mseg_aux-lgort
                                                  AND shkzg = lwa_mseg_aux-shkzg .


        lwa_mseg_sum-menge = lwa_mseg_sum-menge + lwa_mseg_tot-menge.
        lwa_mseg_sum-dmbtr = lwa_mseg_sum-dmbtr + lwa_mseg_tot-dmbtr.


      ENDLOOP.

      lwa_mseg_sum-budat_mkpf = lwa_mseg_tot-budat_mkpf.
      lwa_mseg_sum-werks      = lwa_mseg_tot-werks.
      lwa_mseg_sum-matnr      = lwa_mseg_tot-matnr.
      lwa_mseg_sum-lgort      = lwa_mseg_tot-lgort.
      lwa_mseg_sum-shkzg      = lwa_mseg_tot-shkzg.
      CONCATENATE lwa_mseg_tot-budat_mkpf+0(4) lwa_mseg_tot-budat_mkpf+4(2)  INTO lwa_mseg_sum-spmon.
      CONDENSE lwa_mseg_sum-spmon NO-GAPS.

      APPEND lwa_mseg_sum TO  t_mseg_sum.
      CLEAR: lwa_mseg_sum,
             lwa_mseg_tot,
             lwa_mseg_aux.
    ENDLOOP.

    REFRESH: t_mseg_aux,t_mseg_tot.

    SORT t_mseg_sum BY spmon werks matnr lgort shkzg.

  ENDIF.


ENDFORM.


FORM f_mc9.
  FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
                 <lt_data_line> TYPE ANY TABLE,
                 <ls_data>      TYPE any,
                 <ls_data_line> TYPE any.

  DATA : p_list LIKE abaplist OCCURS 1 WITH HEADER LINE.

  DATA: lr_data            TYPE REF TO data,
        lr_data_line       TYPE REF TO data,
        lr_data_descr      TYPE REF TO cl_abap_datadescr,
        lr_data_line_descr TYPE REF TO cl_abap_datadescr.

  DATA  : it_selection TYPE TABLE OF rsparams,
          wa_selection LIKE LINE OF it_selection.


  DATA: so_data TYPE RANGE OF mkpf-budat,
        wa_data LIKE LINE OF so_data.


  DATA : list TYPE TABLE OF abaplist.

  DATA: BEGIN OF listasci OCCURS 5000,
          default(256) TYPE c,
        END OF listasci.


  cl_salv_bs_runtime_info=>set(
    EXPORTING
      display  = abap_false
      metadata = abap_false
      data     = abap_true ).

  wa_selection-selname = 'SL_WERKS'.
  wa_selection-kind    = 'S'. "S-Select-options P-Parameters
  wa_selection-sign    = 'I'.
  wa_selection-option  = 'EQ'.
  wa_selection-low     = '0101'.
  APPEND wa_selection TO it_selection.

  wa_selection-selname = 'SL_LGORT'.
  wa_selection-kind    = 'S'. "S-Select-options P-Parameters
  wa_selection-sign    = 'I'.
  wa_selection-option  = 'EQ'.
  wa_selection-low     = 'ME01'.
  APPEND wa_selection TO it_selection.

  wa_selection-selname = 'SL_MATNR'.
  wa_selection-kind    = 'S'. "S-Select-options P-Parameters
  wa_selection-sign    = 'I'.
  wa_selection-option  = 'EQ'.
  wa_selection-low     = '000000000000000911'.
  APPEND wa_selection TO it_selection.

  wa_selection-selname = 'SL_SPMON'.
  wa_selection-kind    = 'S'. "S-Select-options P-Parameters
  wa_selection-sign    = 'I'.
  wa_selection-option  = 'EQ'.
  wa_selection-low     = '202312'.
  APPEND wa_selection TO it_selection.



  SUBMIT rmcb0300 WITH SELECTION-TABLE it_selection
  WITH p_tcode = 'MC.9'
  WITH va EQ 'X'
     EXPORTING LIST TO MEMORY
     AND RETURN.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = p_list
*   EXCEPTIONS
*     NOT_FOUND  = 1
*     OTHERS     = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'LIST_TO_ASCI'
* EXPORTING
*   LIST_INDEX               = -1
*   WITH_LINE_BREAK          = ' '
* IMPORTING
*   LIST_STRING_ASCII        =
*   LIST_DYN_ASCII           =
    TABLES
      listasci           = listasci
      listobject         = p_list
    EXCEPTIONS
      empty_list         = 1
      list_index_invalid = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'LIST_FREE_MEMORY'
    TABLES
      listobject = list.


  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data_descr      = lr_data_descr
          r_data_line_descr = lr_data_line_descr ).

      CREATE DATA lr_data TYPE HANDLE lr_data_descr.
      CREATE DATA lr_data_line TYPE HANDLE lr_data_line_descr.

      ASSIGN lr_data->* TO <lt_data>.
      ASSIGN lr_data_line->* TO <lt_data_line>.

      cl_salv_bs_runtime_info=>get_data(
        IMPORTING
          t_data      = <lt_data>
          t_data_line = <lt_data_line> ).

    CATCH cx_salv_bs_sc_runtime_info.
*      MESSAGE 'Não é possível recuperar os dados ALV' TYPE 'E'.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN lr_data->* TO <ls_data>.


ENDFORM.
