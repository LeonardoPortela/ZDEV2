*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 19/08/2013                                              &*
*& Descrição: Relatório de transferencias                             &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*

REPORT  zfis29.

TABLES: j_1bnfdoc.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis, kkblo.
INCLUDE <icon>.


TYPES: BEGIN OF ty_j_1bnfdoc,
         docdat TYPE j_1bnfdoc-docdat,
         bukrs  TYPE j_1bnfdoc-bukrs,
         parid  TYPE j_1bnfdoc-parid,
         parids TYPE j_1bnfdoc-parid,
         docnum TYPE j_1bnfdoc-docnum,
         direct TYPE j_1bnfdoc-direct,
         branch TYPE j_1bnfdoc-branch,
         nfenum TYPE j_1bnfdoc-nfenum,
         cancel TYPE j_1bnfdoc-cancel,
         doctyp TYPE j_1bnfdoc-doctyp,
         model  TYPE j_1bnfdoc-model,
         series TYPE j_1bnfdoc-series,
         filial TYPE j_1bnfdoc-branch,
         del    TYPE char1,
       END OF ty_j_1bnfdoc,

       BEGIN OF ty_j_1bnflin,
         docnum TYPE j_1bnflin-docnum,
         itmnum TYPE j_1bnflin-itmnum,
         bwkey  TYPE j_1bnflin-bwkey,
         cfop   TYPE j_1bnflin-cfop,
         matnr  TYPE j_1bnflin-matnr,
         maktx  TYPE j_1bnflin-maktx,
         menge  TYPE j_1bnflin-menge,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_j_1bnflin,

       BEGIN OF ty_j_1bnfe_active,
         nfnum9 TYPE j_1bnfe_active-nfnum9,
         branch TYPE j_1bnfe_active-branch,
         docnum TYPE j_1bnfe_active-docnum,
         stcd1  TYPE j_1bbranch-stcd1,
       END OF ty_j_1bnfe_active,

       BEGIN OF ty_j_1bbranch,
         stcd1  TYPE j_1bbranch-stcd1,
         branch TYPE j_1bbranch-branch,
       END OF ty_j_1bbranch,

       BEGIN OF ty_doc,
         docnum    TYPE j_1bnfdoc-docnum,
         chave_nfe TYPE zib_nfe_dist_ter-chave_nfe,
       END OF ty_doc,

       BEGIN OF ty_saida,
         bukrs    TYPE j_1bnfdoc-bukrs,
         docdat   TYPE j_1bnfdoc-docdat,
         dias     TYPE i,
         docnum   TYPE j_1bnfdoc-docnum,
         nfenum   TYPE j_1bnfdoc-nfenum,
         branch   TYPE j_1bnfdoc-branch,
         parid    TYPE j_1bnfdoc-parid,
         itmnum   TYPE j_1bnflin-itmnum,
         cfop     TYPE j_1bnflin-cfop,
         matnr    TYPE j_1bnflin-matnr,
         maktx    TYPE j_1bnflin-maktx,
         anln1    TYPE zfiwrt0009-anln1,
         anln2    TYPE zfiwrt0009-anln2,
         menge    TYPE j_1bnflin-menge,
         direct   TYPE j_1bnfdoc-direct,
         seq_lcto TYPE zfiwrt0009-seq_lcto,
       END OF ty_saida.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.
*----------------------------------------------------------------------*
* TABELA INTERNAS
*----------------------------------------------------------------------*
DATA: t_j_1bnfdoc          TYPE TABLE OF ty_j_1bnfdoc,
      t_j_1bnfdoc_ent      TYPE TABLE OF ty_j_1bnfdoc,
      t_j_1bnfe_active     TYPE TABLE OF j_1bnfe_active,
      t_j_1bnflin          TYPE TABLE OF ty_j_1bnflin,
      t_j_1bnfe_active_aux TYPE TABLE OF j_1bnfe_active,
      t_j_1bbranch         TYPE TABLE OF ty_j_1bbranch,
      t_setleaf            TYPE TABLE OF setleaf WITH HEADER LINE,
      t_doc_aux            TYPE TABLE OF ty_doc,
      t_saida              TYPE TABLE OF ty_saida.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_j_1bnfdoc      TYPE ty_j_1bnfdoc,
      wa_j_1bnfdoc_ent  TYPE ty_j_1bnfdoc,
      wa_j_1bnflin      TYPE ty_j_1bnflin,
      wa_j_1bnfe_active TYPE ty_j_1bnfe_active,
      wa_j_1bbranch     TYPE ty_j_1bbranch,
      lv_ind            TYPE sy-tabix,
      wa_saida          TYPE ty_saida.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*
" Seleção de Campos (TextField)
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs  FOR j_1bnfdoc-bukrs OBLIGATORY,
                  s_docdat FOR j_1bnfdoc-docdat OBLIGATORY,
                  s_docnum FOR j_1bnfdoc-docnum,
                  s_parid  FOR j_1bnfdoc-branch OBLIGATORY.

  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN COMMENT /1(50) descr2.
  PARAMETERS:
*              p_mnf1 AS CHECKBOX,  "Sem manifesto
*              p_mnf2 AS CHECKBOX,  "Ciencia da operacao
*              p_mnf3 AS CHECKBOX,  "Confirmação da operacao
              p_mnf4 AS CHECKBOX,  "Operação nao realizada
              p_mnf5 AS CHECKBOX.  "Desconhecimento da Operacao
*              p_mnf6 AS CHECKBOX.  "Desacordo de Entrega de Serviços (CT-e)

  SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  descr2 = 'Desconsiderar Manifesto(s)'.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM iniciar_variaves.
  PERFORM selecionar_dados.
  PERFORM organizacao_dados.
  PERFORM imprimir_dados.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM selecionar_dados .

  DATA: vl_tabix TYPE sy-tabix.

* RJF - ini - CS2023000311 AJUSTE NA TRANSAÇÃO ZFIS30 - notas fiscais de transferencia em transito.


  TYPES: BEGIN OF ty_zsdt0127,
           doc_manifesto TYPE j_1bdocnum,
           cd_operacao   TYPE char10,
         END OF ty_zsdt0127.

  TYPES: BEGIN OF ty_zib_nfe_dist_ter,
           chave_nfe       TYPE zib_nfe_dist_ter-chave_nfe,
           bukrs           TYPE zib_nfe_dist_ter-bukrs,
           branch          TYPE zib_nfe_dist_ter-branch,
           numero          TYPE zib_nfe_dist_ter-numero,
           serie           TYPE zib_nfe_dist_ter-serie,
           dt_emissao      TYPE zib_nfe_dist_ter-dt_emissao,
           forne_cnpj      TYPE zib_nfe_dist_ter-forne_cnpj,
           forne_ie        TYPE zib_nfe_dist_ter-forne_ie,
           destino_cnpj    TYPE zib_nfe_dist_ter-destino_cnpj,
           destino_ie      TYPE zib_nfe_dist_ter-destino_ie,
           model           TYPE zib_nfe_dist_ter-model,
           cancel          TYPE zib_nfe_dist_ter-cancel,
           vl_total        TYPE zib_nfe_dist_ter-vl_total,
           vl_icms_total   TYPE zib_nfe_dist_ter-vl_icms_total,
           stcd1           TYPE lfa1-stcd1,
           numero_aux      TYPE zib_nfe_dist_ter-numero,
           serie_aux       TYPE zib_nfe_dist_ter-serie,
           ref_doc_no      TYPE ekbe-xblnr,
           lifnr           TYPE lfa1-lifnr,
           dt_vencimento   TYPE zib_nfe_dist_ter-dt_vencimento,
           ctr_zterm       TYPE zib_nfe_dist_ter-ctr_zterm,
           vl_total_fatura TYPE zib_nfe_dist_ter-vl_total_fatura,
           cd_tipo_doc     TYPE zib_nfe_dist_ter-cd_tipo_doc,
           cd_fina_emissao TYPE zib_nfe_dist_ter-cd_fina_emissao,
         END OF ty_zib_nfe_dist_ter.

  DATA: tg_zsdt0127_doc     TYPE TABLE OF zsdt0127    WITH HEADER LINE,
        lv_doc_manif(10)    TYPE n,
        tg_zib_nfe_dist_ter TYPE TABLE OF ty_zib_nfe_dist_ter WITH HEADER LINE,
        tg_zib_nfe_forn     TYPE TABLE OF zib_nfe_forn        WITH HEADER LINE,
        tg_zsdt0127         TYPE TABLE OF ty_zsdt0127 WITH HEADER LINE.
  RANGES:
          r_st_mnf   FOR zsdt0127-cd_operacao,
          r_nfenum   FOR j_1bnfdoc-nfenum.
*          r_doc_man  FOR zsdt0127-doc_manifesto.
  CLEAR: tg_zsdt0127_doc[].

*  IF p_mnf1 IS NOT INITIAL.
*    r_st_mnf-sign   = 'I'.
*    r_st_mnf-option = 'EQ'.
*    r_st_mnf-low    = '000000'.
*    r_st_mnf-high   = '000000'.
*    APPEND r_st_mnf.
*  ENDIF.

*  IF p_mnf2 IS NOT INITIAL.
*    r_st_mnf-sign   = 'I'.
*    r_st_mnf-option = 'EQ'.
*    r_st_mnf-low    = '210210'.
*    r_st_mnf-high   = '210210'.
*    APPEND r_st_mnf.
*  ENDIF.

*  IF p_mnf3 IS NOT INITIAL.
*    r_st_mnf-sign   = 'I'.
*    r_st_mnf-option = 'EQ'.
*    r_st_mnf-low    = '210200'.
*    r_st_mnf-high   = '210200'.
*    APPEND r_st_mnf.
*  ENDIF.

  IF p_mnf4 IS NOT INITIAL.
    r_st_mnf-sign   = 'I'.
    r_st_mnf-option = 'EQ'.
    r_st_mnf-low    = '210240'.
    r_st_mnf-high   = '210240'.
    APPEND r_st_mnf.
  ENDIF.

  IF p_mnf5 IS NOT INITIAL.
    r_st_mnf-sign   = 'I'.
    r_st_mnf-option = 'EQ'.
    r_st_mnf-low    = '210220'.
    r_st_mnf-high   = '210220'.
    APPEND r_st_mnf.
  ENDIF.

*  IF p_mnf6 IS NOT INITIAL.
*    r_st_mnf-sign   = 'I'.
*    r_st_mnf-option = 'EQ'.
*    r_st_mnf-low    = '610110'.
*    r_st_mnf-high   = '610110'.
*    APPEND r_st_mnf.
*  ENDIF.


*  SELECT *
*    FROM zib_nfe_dist_ter INTO CORRESPONDING FIELDS OF TABLE tg_zib_nfe_dist_ter
*   WHERE
*         dt_emissao      IN s_docdat
*     AND bukrs           IN s_bukrs
*     AND branch          IN s_parid
*     AND model           EQ '55'.


*  SELECT *
*    FROM zib_nfe_forn INTO TABLE tg_zib_nfe_forn
*   WHERE
*      nu_chave_cnpj   IN p_cnpj
*       AND
*    dt_emissao      IN s_docdat
*       AND nu_chave_numero IN p_nota
*       AND st_nota         IN r_st_xml
*       AND nu_chave_modelo IN r_modelo
*     AND bukrs           IN s_bukrs
*     AND branch          IN s_parid.
*       AND nu_chave        IN p_chave.

*  IF tg_zib_nfe_forn[] IS NOT INITIAL.
*
*  ENDIF.


* RJF - fim - CS2023000311 AJUSTE NA TRANSAÇÃO ZFIS30 - notas fiscais de transferencia em transito.

*  IF s_docnum[] IS INITIAL.
*    s_docnum-sign   = 'I'.
*    s_docnum-option = 'EQ'.
*    s_docnum-low    = abap_false.
*    APPEND s_docnum.
*  ENDIF.

  "Chave de Acesso
*      CONCATENATE
*        wa_j_1bnfe_active-regio
*        wa_j_1bnfe_active-nfyear
*        wa_j_1bnfe_active-nfmonth
*        wa_j_1bnfe_active-stcd1
*        wa_j_1bnfe_active-model
*        wa_j_1bnfe_active-serie
*        wa_j_1bnfe_active-nfnum9
*        wa_j_1bnfe_active-docnum9
*        wa_j_1bnfe_active-cdv    INTO chave_nfe.

  SELECT docdat bukrs parid docnum direct branch nfenum cancel doctyp model series
    FROM j_1bnfdoc
      INTO CORRESPONDING FIELDS OF TABLE t_j_1bnfdoc
       WHERE docnum IN s_docnum
         AND docdat IN s_docdat
         AND bukrs  IN s_bukrs.
*         AND PARID  IN S_PARID.

*  IF t_j_1bnfdoc IS NOT INITIAL.
*    SELECT docnum regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
*    FROM j_1bnfe_active
*      INTO CORRESPONDING FIELDS OF TABLE t_j_1bnfe_active_aux
*      FOR ALL ENTRIES IN t_j_1bnfdoc
*      WHERE docnum EQ t_j_1bnfdoc-docnum.
*
*    t_doc_aux
*    = VALUE #( FOR l IN t_j_1bnfe_active_aux
*    ( docnum = l-docnum chave_nfe = |{ l-regio }{ l-nfyear }{ l-nfmonth }{ l-stcd1 }{ l-model }{ l-serie }{ l-nfnum9 }{ l-docnum9 }{ l-cdv }| ) ).
*  ENDIF.

* RJF - Ini - CS2023000311 AJUSTE NA TRANSAÇÃO ZFIS30 - notas fiscais de transferencia em transito.
  IF r_st_mnf IS NOT INITIAL.

  IF t_j_1bnfdoc IS NOT INITIAL.
    SELECT docnum regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
    FROM j_1bnfe_active
      INTO CORRESPONDING FIELDS OF TABLE t_j_1bnfe_active_aux
      FOR ALL ENTRIES IN t_j_1bnfdoc
      WHERE docnum EQ t_j_1bnfdoc-docnum.

    t_doc_aux
    = VALUE #( FOR l IN t_j_1bnfe_active_aux
    ( docnum = l-docnum chave_nfe = |{ l-regio }{ l-nfyear }{ l-nfmonth }{ l-stcd1 }{ l-model }{ l-serie }{ l-nfnum9 }{ l-docnum9 }{ l-cdv }| ) ).
  ENDIF.

    IF t_doc_aux IS NOT INITIAL.
      SELECT *
        FROM zsdt0127 INTO CORRESPONDING FIELDS OF TABLE tg_zsdt0127_doc
        FOR ALL ENTRIES IN t_doc_aux
       WHERE chave EQ t_doc_aux-chave_nfe
         AND cd_operacao IN r_st_mnf
         AND autorizado EQ abap_true.
      IF sy-subrc IS INITIAL.
        SORT tg_zsdt0127_doc BY doc_manifesto.
      ENDIF.
    ENDIF.

    LOOP AT tg_zsdt0127_doc ASSIGNING FIELD-SYMBOL(<fs_zsdt0127_doc>).
      r_nfenum-sign   = 'I'.
      r_nfenum-option = 'EQ'.
      r_nfenum-low    = <fs_zsdt0127_doc>-doc_manifesto+1(9).
      APPEND r_nfenum.
      CLEAR r_nfenum.
    ENDLOOP.

    IF t_j_1bnfdoc IS NOT INITIAL.
      SORT t_j_1bnfdoc BY nfenum.
      SORT r_nfenum BY low.
    ENDIF.

*LOOP AT r_nfenum into data(lr_nfenum).
*
*READ TABLE t_j_1bnfdoc into data(li_bnfdoc) with key nfenum = lr_nfenum-low(9).
*
*if sy-subrc is not initial.
*      DELETE t_j_1bnfdoc WHERE nfenum eq lr_nfenum-low(9).
*endif.
*ENDLOOP.


    LOOP AT t_j_1bnfdoc[] INTO wa_j_1bnfdoc.
      lv_ind = lv_ind + 1.
      READ TABLE t_doc_aux INTO DATA(ws_doc_aux) WITH KEY docnum = wa_j_1bnfdoc-docnum.

      IF sy-subrc IS INITIAL AND tg_zsdt0127_doc[] IS NOT INITIAL.
        IF ws_doc_aux-chave_nfe IS NOT INITIAL.
          READ TABLE tg_zsdt0127_doc INTO DATA(lr_nfenum) WITH KEY chave = ws_doc_aux-chave_nfe.
          IF sy-subrc IS INITIAL.
            wa_j_1bnfdoc-del = abap_true.
          ENDIF.
        ENDIF.
*      ELSE.
*        wa_j_1bnfdoc-del = abap_true.
      ENDIF.

      IF wa_j_1bnfdoc-del IS NOT INITIAL.
        MODIFY t_j_1bnfdoc[] FROM wa_j_1bnfdoc INDEX lv_ind TRANSPORTING del.
      ENDIF.

    ENDLOOP.
    SORT t_j_1bnfdoc[] BY del.
    DELETE t_j_1bnfdoc[] WHERE del EQ abap_true.

  ENDIF.
* RJF - fim - CS2023000311 AJUSTE NA TRANSAÇÃO ZFIS30 - notas fiscais de transferencia em transito.

  LOOP AT t_j_1bnfdoc[] INTO wa_j_1bnfdoc.

    wa_j_1bnfdoc-filial = wa_j_1bnfdoc-parid+4.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_j_1bnfdoc-series
      IMPORTING
        output = wa_j_1bnfdoc-series.

    wa_j_1bnfdoc-parids = |{ wa_j_1bnfdoc-bukrs }{ wa_j_1bnfdoc-branch }|.

    MODIFY t_j_1bnfdoc[] FROM wa_j_1bnfdoc INDEX sy-tabix TRANSPORTING filial series parids.

    IF ( wa_j_1bnfdoc-parid+4 NOT IN s_parid ) AND ( wa_j_1bnfdoc-branch NOT IN s_parid ).
      DELETE t_j_1bnfdoc INDEX sy-tabix.
    ENDIF.

    IF ( wa_j_1bnfdoc-cancel EQ 'X' ) OR ( wa_j_1bnfdoc-doctyp EQ '5' ).
      DELETE t_j_1bnfdoc INDEX sy-tabix.
    ENDIF.

  ENDLOOP.

  IF ( t_j_1bnfdoc[] IS NOT INITIAL ).

    SELECT  nfnum9 branch docnum stcd1
      FROM j_1bnfe_active
      INTO CORRESPONDING FIELDS OF TABLE t_j_1bnfe_active
      FOR ALL ENTRIES IN t_j_1bnfdoc
      WHERE nfnum9 = t_j_1bnfdoc-nfenum
        AND branch = t_j_1bnfdoc-filial
        AND direct = '1'
        AND model  = t_j_1bnfdoc-model
        AND serie  = t_j_1bnfdoc-series
        AND parid EQ t_j_1bnfdoc-parids.

    IF NOT t_j_1bnfe_active[] IS INITIAL.

      REFRESH: t_j_1bnfdoc_ent[].
      SELECT docdat bukrs parid docnum direct branch nfenum cancel doctyp
        FROM j_1bnfdoc
        INTO CORRESPONDING FIELDS OF TABLE t_j_1bnfdoc_ent
        FOR ALL ENTRIES IN t_j_1bnfe_active
        WHERE docnum = t_j_1bnfe_active-docnum.

      LOOP AT t_j_1bnfe_active[] INTO wa_j_1bnfe_active.

        vl_tabix = sy-tabix.

        READ TABLE t_j_1bnfdoc_ent[] INTO wa_j_1bnfdoc_ent WITH KEY docnum = wa_j_1bnfe_active-docnum.
        IF ( sy-subrc NE 0 ) OR ( wa_j_1bnfdoc_ent-cancel EQ 'X' ) OR ( wa_j_1bnfdoc_ent-doctyp EQ '5' ).
          DELETE t_j_1bnfe_active INDEX vl_tabix.
        ENDIF.

      ENDLOOP.

    ENDIF.

    IF NOT t_j_1bnfe_active[] IS INITIAL.
*      SELECT stcd1 branch
*        FROM j_1bbranch
*        INTO TABLE t_j_1bbranch
*        FOR ALL ENTRIES IN t_j_1bnfe_active
*        WHERE stcd1 = t_j_1bnfe_active-stcd1.
    ENDIF.

    SELECT docnum itmnum bwkey cfop matnr maktx menge refkey
      FROM j_1bnflin
        INTO TABLE t_j_1bnflin
        FOR ALL ENTRIES IN t_j_1bnfdoc
          WHERE docnum EQ t_j_1bnfdoc-docnum
            AND bwkey  EQ t_j_1bnfdoc-branch.
*              and CFOP  eq SET “MAGGI_CFOP_TRANSFERENCIA”


    SELECT *
      FROM setleaf
      INTO TABLE t_setleaf
      WHERE setclass EQ '0000'
        AND setname EQ 'MAGGI_CFOP_TRANSFERENCIA'.

  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
FORM organizacao_dados .
  DATA: wl_parid     TYPE j_1bnfdoc-parid,
        fg_ignora(1).

  SORT: t_j_1bnfe_active[]  BY nfnum9 branch,
        t_j_1bbranch[]      BY stcd1.

  IF ( t_j_1bnflin[] IS NOT INITIAL ).

    SELECT seq_lcto, itmnum, matnr, cfop, bwkey, anln1, anln2
      FROM zfiwrt0009
      INTO TABLE @DATA(tg_zfiwrt0009)
      FOR ALL ENTRIES IN @t_j_1bnflin
      WHERE seq_lcto EQ @t_j_1bnflin-refkey+0(10)
        AND itmnum   EQ @t_j_1bnflin-itmnum.
*        AND MATNR    EQ @T_J_1BNFLIN-MATNR
*        AND CFOP     EQ @T_J_1BNFLIN-CFOP
*        AND BWKEY    EQ @T_J_1BNFLIN-BWKEY.

  ENDIF.

  LOOP AT t_j_1bnfdoc[] INTO wa_j_1bnfdoc.

    IF ( wa_j_1bnfdoc-direct EQ 2 ).

      CLEAR: fg_ignora.

      CONCATENATE wa_j_1bnfdoc-bukrs wa_j_1bnfdoc-branch INTO wl_parid.

      LOOP AT t_j_1bnfe_active[] INTO wa_j_1bnfe_active WHERE nfnum9 = wa_j_1bnfdoc-nfenum
                                                          AND branch = wa_j_1bnfdoc-filial.

        READ TABLE t_j_1bbranch INTO wa_j_1bbranch WITH KEY stcd1 = wa_j_1bnfe_active-stcd1  BINARY SEARCH.

        IF sy-subrc IS INITIAL AND wa_j_1bnfdoc-filial EQ wa_j_1bnfe_active-branch.
          fg_ignora = abap_true.
        ENDIF.

      ENDLOOP.

      IF fg_ignora IS INITIAL.

*        READ TABLE T_J_1BNFLIN INTO WA_J_1BNFLIN
*          WITH KEY  DOCNUM = WA_J_1BNFDOC-DOCNUM
*                    BWKEY  = WA_J_1BNFDOC-BRANCH.

        LOOP AT t_j_1bnflin INTO wa_j_1bnflin WHERE docnum EQ wa_j_1bnfdoc-docnum
                                                AND bwkey  EQ wa_j_1bnfdoc-branch.

          READ TABLE t_setleaf WITH KEY valfrom = wa_j_1bnflin-cfop.

          IF sy-subrc IS INITIAL.

            wa_saida-bukrs  = wa_j_1bnfdoc-bukrs.
            wa_saida-docdat = wa_j_1bnfdoc-docdat.
            wa_saida-docnum = wa_j_1bnfdoc-docnum.
            wa_saida-nfenum = wa_j_1bnfdoc-nfenum.
            wa_saida-branch = wa_j_1bnfdoc-branch.
            wa_saida-parid  = wa_j_1bnfdoc-parid+4(4).
            wa_saida-itmnum = wa_j_1bnflin-itmnum.
            wa_saida-cfop   = wa_j_1bnflin-cfop.
            wa_saida-matnr  = wa_j_1bnflin-matnr.
            wa_saida-maktx  = wa_j_1bnflin-maktx.
            wa_saida-menge  = wa_j_1bnflin-menge.
            wa_saida-direct = wa_j_1bnfdoc-direct.

            CALL FUNCTION 'HR_AUPBS_MONTH_DAY'
              EXPORTING
                beg_da     = wa_j_1bnfdoc-docdat
                end_da     = sy-datum
              IMPORTING
                no_cal_day = wa_saida-dias.

            READ TABLE tg_zfiwrt0009 INTO DATA(wl_imob) WITH KEY seq_lcto = wa_j_1bnflin-refkey+0(10)
                                                                 itmnum = wa_j_1bnflin-itmnum
                                                                 matnr  = wa_j_1bnflin-matnr.

            IF ( wl_imob IS NOT INITIAL ).

              wa_saida-anln1 = wl_imob-anln1.
              IF ( wl_imob-anln2 IS INITIAL ).
                wa_saida-anln2 = '0000'.
              ELSE.
                wa_saida-anln2 = wl_imob-anln2.
              ENDIF.

              wa_saida-seq_lcto = wl_imob-seq_lcto.

            ENDIF.

            APPEND wa_saida TO t_saida[].

          ENDIF.

          CLEAR: wa_saida, wl_imob.

        ENDLOOP.

      ENDIF.
    ENDIF.

    CLEAR: wa_saida, wa_j_1bnfdoc, wa_j_1bnflin, t_setleaf.

  ENDLOOP.

  DELETE t_saida WHERE parid(4) NOT IN s_parid.

ENDFORM.                    " ORGANIZACAO_DADOS

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
FORM imprimir_dados .
  DATA: wl_layout TYPE slis_layout_alv.
  PERFORM definir_eventos.
  PERFORM montar_layout.

  wl_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = 'F_ALV_COMMAND'
      it_fieldcat             = estrutura[]
      is_layout               = wl_layout
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
    TABLES
      t_outtab                = t_saida.



ENDFORM.                    "imprimir_dados

*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
FORM definir_eventos.

  PERFORM f_carregar_eventos USING:
* para tira duplo click          SLIS_EV_USER_COMMAND 'XUSER_COMMAND',
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.


ENDFORM.                    " DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.

ENDFORM.                    " F_CARREGAR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
FORM montar_layout.

  PERFORM montar_estrutura USING:
        1  'J_1BNFDOC'   'BUKRS'      'T_SAIDA' 'BUKRS'     ' '                  ' ' ,
        2  'J_1BNFDOC'   'DOCDAT'     'T_SAIDA' 'DOCDAT'    ' '                  ' ' ,
        3  'J_1BNFDOC'   'DOCNUM'     'T_SAIDA' 'DOCNUM'    ' '                  ' ' ,
        4  'J_1BNFDOC'   'NFENUM'     'T_SAIDA' 'NFENUM'    'Nr.NF'              ' ' ,
        5  'J_1BNFDOC'   'BRANCH'     'T_SAIDA' 'BRANCH'    'Centro fornecedor'  ' ' ,
        6  'J_1BNFDOC'   'PARID'      'T_SAIDA' 'PARID'     'Centro receptor'    ' ' ,
        7  'J_1BNFLIN'   'ITMNUM'     'T_SAIDA' 'ITMNUM'    'Item'               ' ' ,
        8  'J_1BNFLIN'   'CFOP'       'T_SAIDA' 'CFOP'      ' '                  ' ' ,
        9  'J_1BNFLIN'   'MATNR'      'T_SAIDA' 'MATNR'     ' '                  ' ' ,
       10  'J_1BNFLIN'   'MAKTX'      'T_SAIDA' 'MAKTX'     ' '                  ' ' ,
       11  'ZFIWRT0009'  'ANLN1'      'T_SAIDA' 'ANLN1'     ' '                  ' ' ,
       12  'ZFIWRT0009'  'ANLN2'      'T_SAIDA' 'ANLN2'     ' '                  ' ' ,
       13  'J_1BNFLIN'   'MENGE'      'T_SAIDA' 'MENGE'     ' '                  ' ' ,
       14  'ZFIWRT0009'  'SEQ_LCTO'   'T_SAIDA' 'SEQ_LCTO'  ' '                  ' ' ,
       15  'J_1BNFDOC'   'DOCDAT'     'T_SAIDA' 'DIAS'      'Dias em trânsitos'  ' ' .


ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  CLEAR wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-just          = 'C'.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  IF ( wa_estrutura-fieldname EQ 'DOCNUM' ) OR ( wa_estrutura-fieldname EQ 'ANLN1' )
    OR ( wa_estrutura-fieldname EQ 'SEQ_LCTO' ).
    wa_estrutura-hotspot = 'X'.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE

*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
FORM iniciar_variaves .
  v_report = sy-repid.

  PERFORM f_construir_cabecalho USING 'H' TEXT-002.

ENDFORM.                    " INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  F_ALV_COMMAND
*&---------------------------------------------------------------------*
FORM f_alv_command USING l_ucomm
                         l_selfield TYPE slis_selfield.

  DATA: it_dta TYPE STANDARD TABLE OF bdcdata,
        wa_dta TYPE bdcdata,
        opt    TYPE ctu_params.


  IF ( l_selfield-fieldname EQ 'DOCNUM' ).

    READ TABLE t_saida INTO DATA(wl_saida) INDEX l_selfield-tabindex.
    IF ( wl_saida-docnum IS NOT INITIAL ).
      SET PARAMETER ID 'JEF' FIELD wl_saida-docnum.
      CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
    ENDIF.

  ELSEIF ( l_selfield-fieldname EQ 'ANLN1' ).

    READ TABLE t_saida INTO wl_saida INDEX l_selfield-tabindex.

    IF ( wl_saida-docnum IS NOT INITIAL ).
      SET PARAMETER ID 'AN1' FIELD wl_saida-anln1.
      IF ( wl_saida-anln2 IS NOT INITIAL ).
        SET PARAMETER ID 'AN2' FIELD wl_saida-anln2.
      ENDIF.
      SET PARAMETER ID 'BUK' FIELD wl_saida-bukrs.
      CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
    ENDIF.

  ELSEIF ( l_selfield-fieldname EQ 'SEQ_LCTO' ).

    READ TABLE t_saida INTO wl_saida INDEX l_selfield-tabindex.

    IF ( wl_saida-seq_lcto IS NOT INITIAL ).
      CLEAR: it_dta[], wa_dta.

      DEFINE shdb.
        wa_dta-program   = &1.
        wa_dta-dynpro    = &2.
        wa_dta-dynbegin  = &3.
        wa_dta-fnam      = &4.
        wa_dta-fval      = &5.
        APPEND wa_dta TO it_dta.
      END-OF-DEFINITION.

      shdb:
            ''          ''      'T'	  'ZNFW0002'    '',
            'ZWRR0002'  '0100'  'X'   ''            '',
            ''          ''      ''    'BDC_CURSOR'  'P_SEQ_LCTO',
            ''          ''      ''    'BDC_OKCODE'  '=SEARCH',
            ''          ''      ''    'P_SEQ_LCTO'  wl_saida-seq_lcto,
            ''          ''      ''    'BDC_SUBSCR'  'ZWRR0002                                0213TAB_STRIP_NF_SCA'.
*            'ZWRR0002'  '0100'  'X'   ''            '',
*             ''          ''      ''   'BDC_OKCODE'  '/EBACK',
*             ''          ''      ''   'BDC_CURSOR'  'P_SEQ_LCTO',
*             ''          ''      ''   'P_SEQ_LCTO'  '218369',
*             ''          ''      ''   'BDC_SUBSCR'  'ZWRR0002                                0213TAB_STRIP_NF_SCA'.

      opt-dismode = 'E'.
      CALL TRANSACTION 'ZNFW0002' USING it_dta OPTIONS FROM opt.

    ENDIF.

  ENDIF.


ENDFORM.
