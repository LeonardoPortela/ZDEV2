*&---------------------------------------------------------------------*
*& Report  ZLESR0015                                                   *
* Descrição  : Controle de Saldo Ferroviário                           *
* Módulo     : LES                               Transação: ZLES0048   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Paulo Bonetti                          Data: 27/01/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*

REPORT  zlesr0015.


*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zlest0006, zlest0035, makt, vbpa .

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF ty_zlest0035                 ,
*        chave      TYPE ZLEST0035-chave     ,
    nr_nf      TYPE zlest0035-nr_nf,
    serie_nf   TYPE zlest0035-serie_nf,
    docnum     TYPE zlest0035-docnum,
    cnpj       TYPE zlest0035-cnpj,
    werks      TYPE zlest0035-werks,
    qtd_nf     TYPE zlest0035-qtd_nf,
    qtd_cheg   TYPE zlest0035-qtd_cheg,
    saldo      TYPE zlest0035-saldo,
    dtachegada TYPE zlest0035-dtachegada,
  END   OF ty_zlest0035                 ,

  BEGIN OF ty_zlest0035_aux                 ,
    nr_nf       TYPE zlest0035-nr_nf,
    cnpj        TYPE zlest0004-cgc_remetente,
    werks       TYPE zlest0035-werks,
    cgc_cliente TYPE zlest0006-cgc_cliente,
  END   OF ty_zlest0035_aux                 ,

  BEGIN OF ty_zlest0003,
    serie_despacho TYPE zlest0003-serie_despacho,
    nr_despacho    TYPE zlest0003-nr_despacho,
    nr_nf          TYPE zlest0003-nr_nf,
    cgc_remetente  TYPE zlest0035-cnpj,
    cgc_cliente    TYPE zlest0006-cgc_cliente,
  END OF ty_zlest0003,

  BEGIN OF ty_makt                         ,
    matnr TYPE makt-matnr,
    maktx TYPE makt-maktx,
  END   OF ty_makt                         ,

  BEGIN OF ty_j_1bbranch,
    branch TYPE j_1bbranch-branch,
    stcd1  TYPE j_1bbranch-stcd1,
  END OF  ty_j_1bbranch,

  BEGIN OF ty_t001w            ,
    werks TYPE t001w-werks,
    name1 TYPE t001w-name1,
  END   OF ty_t001w            ,

  BEGIN OF ty_parceiro,
    vbeln TYPE vbpa-vbeln,
    parvw TYPE vbpa-parvw,
    lifnr TYPE vbpa-lifnr,
  END OF ty_parceiro,

  BEGIN OF ty_saida                        ,
    nr_nf       TYPE zlest0035-nr_nf,
    serie_nf    TYPE zlest0035-serie_nf,
    docnum      TYPE zlest0035-docnum,
    cnpj        TYPE zlest0035-cnpj,
    cgc_cliente TYPE zlest0006-cgc_cliente,
    werks       TYPE zlest0035-werks,
    qtd_nf      TYPE zlest0035-qtd_nf,
    qtd_cheg    TYPE zlest0035-qtd_cheg,
    qtd_util    TYPE zlest0035-qtd_cheg,
    saldo       TYPE zlest0035-saldo,
    dtachegada  TYPE zlest0035-dtachegada,
    maktx       TYPE makt-maktx,
    name1       TYPE t001w-name1,
    terminal    TYPE lfa1-name1,
    fatura      TYPE c LENGTH 6,
  END OF ty_saida                          ,

  BEGIN OF ty_saida2                       ,
    nr_nf_all      TYPE zlest0006-nr_nf_all,
    nr_fatura      TYPE zlest0006-nr_fatura,
    serie_despacho TYPE zlest0004-serie_despacho,
    nr_despacho    TYPE zlest0004-nr_despacho,
    idvagao        TYPE zlest0019-idvagao,
    peso_vagao     TYPE zlest0019-pesodvagao,
    dtaenvio       TYPE zlest0019-dtaenvio,
    obs            TYPE zlest0019-obs,
    chave          TYPE zlest0019-chave,
    nr_trans       TYPE zlest0006-nr_trans,
    nr_frete       TYPE zlest0006-nr_frete,
  END OF ty_saida2                         .


*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

DATA: it_zlest0006     TYPE TABLE OF zlest0006,
      it_zlest0006_aux TYPE TABLE OF zlest0006,
      it_zlest0004     TYPE TABLE OF zlest0004,
      it_zlest0044     TYPE TABLE OF zlest0044,
      it_zlest0045     TYPE TABLE OF zlest0045,
      it_zlest0004_aux TYPE TABLE OF zlest0004,
      it_zlest0019     TYPE TABLE OF zlest0019,
      it_zlest0019_aux TYPE TABLE OF zlest0019,
      it_zlest0003     TYPE TABLE OF ty_zlest0003,
      it_zlest0003_aux TYPE TABLE OF ty_zlest0003,
      it_zlest0035     TYPE TABLE OF ty_zlest0035,
      tg_0045          TYPE TABLE OF zlest0045 WITH HEADER LINE,
      tg_0044          TYPE TABLE OF zlest0044 WITH HEADER LINE,
      it_t001w         TYPE TABLE OF ty_t001w,
      it_j_1bnfdoc     TYPE TABLE OF j_1bnfdoc,
      it_j_1bnflin     TYPE TABLE OF j_1bnflin,
      it_parceiro      TYPE TABLE OF ty_parceiro,
      it_j_1bbranch    TYPE TABLE OF ty_j_1bbranch,
      it_lfa1          TYPE TABLE OF lfa1,
      it_makt          TYPE TABLE OF ty_makt,
      it_saida         TYPE TABLE OF ty_saida,
      it_saida2        TYPE TABLE OF ty_saida2,
      t_bdc            TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      t_messtab        TYPE TABLE OF bdcmsgcoll.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA: wa_zlest0006     TYPE zlest0006,
      wa_zlest0004     TYPE zlest0004,
      wa_zlest0019     TYPE zlest0019,
      wa_zlest0003     TYPE ty_zlest0003,
      wa_zlest0035     TYPE ty_zlest0035,
      wa_zlest0044     TYPE zlest0044,
      wa_zlest0045     TYPE zlest0045,
      wa_zlest0006_aux TYPE zlest0006,
      wa_zlest0004_aux TYPE zlest0004,
      wa_zlest0003_aux TYPE ty_zlest0003,
      wa_zlest0019_aux TYPE zlest0019,
      wa_t001w         TYPE ty_t001w,
      wa_j_1bnfdoc     TYPE j_1bnfdoc,
      wa_j_1bnflin     TYPE j_1bnflin,
      wa_parceiro      TYPE ty_parceiro,
      wa_j_1bbranch    TYPE ty_j_1bbranch,
      wa_lfa1          TYPE lfa1,
      wa_makt          TYPE ty_makt,
      wa_saida         TYPE ty_saida,
      wa_saida2        TYPE ty_saida2,
      wa_cont          TYPE REF TO cl_gui_custom_container , " Objeto Container
      wa_cont2         TYPE REF TO cl_gui_custom_container , " Objeto Container
      wa_alv           TYPE REF TO cl_gui_alv_grid, " Objeto ALV
      wa_alv2          TYPE REF TO cl_gui_alv_grid, " Objeto ALV
      wa_layout        TYPE lvc_s_layo            . " Layout da Lista / Fim do DATA


*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*

DATA: it_fcat    TYPE TABLE OF lvc_s_fcat,
      it_fcat2   TYPE TABLE OF lvc_s_fcat,
      s_variant  TYPE disvariant,
      s_variant2 TYPE disvariant.              " Tabela Estrutura colunas relatorio

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: p_client FOR vbpa-lifnr         ,
                p_nr_nf  FOR zlest0035-nr_nf    ,
                p_serie  FOR zlest0035-serie_nf ,
                p_docnum FOR zlest0035-docnum   ,
                p_cnpj   FOR zlest0035-cnpj     ,
                p_werks  FOR zlest0035-werks    ,
                p_matnr  FOR makt-matnr         ,
                p_dt     FOR zlest0035-dtachegada.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-016.

PARAMETERS: p_saldo TYPE char1 RADIOBUTTON GROUP rb02,
            p_slzer TYPE char1 RADIOBUTTON GROUP rb02,
            p_ambas TYPE char1 RADIOBUTTON GROUP rb02 DEFAULT 'X'.

SELECTION-SCREEN: END   OF BLOCK b2.
*---------------------------------------------------------------------*
*Início Alteração Ricardo Furst.
*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
DATA: vg_repid   LIKE sy-repid,
      vg_variant TYPE disvariant.

INITIALIZATION.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:  f_seleciona_dados                       , " Form seleciona dados
            f_saida                                 , " Form de saida
            f_alv                                   . " Form ALV

  CALL SCREEN 0100.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Form para selecionar os dados e relacionar as tabelas.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados.

  DATA: tl_zlest0019      TYPE TABLE OF zlest0019,
        sl_zlest0019      TYPE zlest0019,
        sl_zlest0004      TYPE zlest0004,
        sl_zlest0035_aux2 TYPE ty_zlest0035_aux,
        tl_zlest0004      TYPE TABLE OF zlest0004,
        it_j_1bnflin_aux  TYPE TABLE OF j_1bnflin,
        it_zlest0035_aux  TYPE TABLE OF ty_zlest0035,
        it_zlest0035_aux2 TYPE TABLE OF ty_zlest0035_aux.


  REFRESH: it_zlest0006,
           it_zlest0004,
           it_zlest0019,
           it_zlest0003,
           it_zlest0035,
           it_zlest0035_aux2.

  "-------------0035-----------
  IF p_saldo EQ 'X' .

    SELECT nr_nf
           serie_nf
           docnum
           cnpj
           werks
           qtd_nf
           qtd_cheg
           saldo
           dtachegada
     FROM zlest0035
     INTO TABLE it_zlest0035
     WHERE serie_nf   IN p_serie
       AND docnum     IN p_docnum
       AND werks      IN p_werks
       AND dtachegada IN p_dt
       AND nr_nf      IN p_nr_nf
       AND saldo      > 0.

  ELSEIF p_slzer EQ 'X'.

    SELECT nr_nf
           serie_nf
           docnum
           cnpj
           werks
           qtd_nf
           qtd_cheg
           saldo
           dtachegada
      FROM zlest0035
      INTO TABLE it_zlest0035
      WHERE serie_nf   IN p_serie
        AND docnum     IN p_docnum
        AND werks      IN p_werks
        AND dtachegada IN p_dt
        AND nr_nf      IN p_nr_nf
        AND saldo      EQ 0.
  ELSE .

    SELECT nr_nf
           serie_nf
           docnum
           cnpj
           werks
           qtd_nf
           qtd_cheg
           saldo
           dtachegada
      FROM zlest0035
      INTO TABLE it_zlest0035
      WHERE serie_nf   IN p_serie
        AND docnum     IN p_docnum
        AND werks      IN p_werks
        AND dtachegada IN p_dt
        AND nr_nf      IN p_nr_nf.

  ENDIF.

  IF sy-subrc IS INITIAL.



    SELECT *
      FROM zlest0045
      INTO TABLE it_zlest0045
      FOR ALL ENTRIES IN it_zlest0035
     WHERE docnum EQ it_zlest0035-docnum.

    SELECT *
      FROM zlest0044
      INTO TABLE it_zlest0044
      FOR ALL ENTRIES IN it_zlest0045
     WHERE chave_cte EQ it_zlest0045-chave_cte.

    it_zlest0035_aux[] = it_zlest0035[].

*---> 04/07/2023 - Migração S4 - WS
  SORT  it_zlest0035_aux BY werks.
*<--- 04/07/2023 - Migração S4 - WS
    DELETE ADJACENT DUPLICATES FROM it_zlest0035_aux COMPARING werks.

    SORT it_zlest0035_aux BY werks.

    SELECT werks name1
      FROM t001w
      INTO TABLE it_t001w
      FOR ALL ENTRIES IN it_zlest0035
      WHERE werks EQ it_zlest0035-werks.

    "-------NOTA FISCAL
    SELECT *
      FROM j_1bnfdoc
      INTO TABLE it_j_1bnfdoc
      FOR ALL ENTRIES IN it_zlest0035
      WHERE docnum EQ it_zlest0035-docnum.

    IF sy-subrc IS INITIAL .

      "-------ITEM DA NOTA
      SELECT *
        FROM j_1bnflin
        INTO TABLE it_j_1bnflin
        FOR ALL ENTRIES IN it_j_1bnfdoc
        WHERE docnum EQ it_j_1bnfdoc-docnum.

      "------Parceiro da Fatura
      IF sy-subrc IS INITIAL.

        it_j_1bnflin_aux[] = it_j_1bnflin[].

*---> 04/07/2023 - Migração S4 - WS
  SORT  it_j_1bnflin_aux BY refkey.
*<--- 04/07/2023 - Migração S4 - WS
        DELETE ADJACENT DUPLICATES FROM it_j_1bnflin_aux COMPARING refkey.

        SORT it_j_1bnflin_aux BY refkey.

        SELECT vbeln parvw lifnr
          FROM vbpa
          INTO TABLE it_parceiro
          FOR ALL ENTRIES IN it_j_1bnflin_aux
          WHERE vbeln EQ it_j_1bnflin_aux-refkey(10)
            AND parvw EQ 'Z1'.

        SELECT *
          FROM lfa1
          INTO TABLE it_lfa1
           FOR ALL ENTRIES IN it_parceiro
         WHERE lifnr EQ it_parceiro-lifnr.

        "-------Produto

        it_j_1bnflin_aux[] = it_j_1bnflin[].

*---> 04/07/2023 - Migração S4 - WS
  SORT  it_j_1bnflin_aux BY matnr.
*<--- 04/07/2023 - Migração S4 - WS
        DELETE ADJACENT DUPLICATES FROM it_j_1bnflin_aux COMPARING matnr.

        SORT it_j_1bnflin_aux BY matnr.

        SELECT matnr maktx
          FROM makt
          INTO TABLE it_makt
           FOR ALL ENTRIES IN it_j_1bnflin_aux
         WHERE spras EQ sy-langu
           AND matnr EQ it_j_1bnflin_aux-matnr.

        SELECT branch stcd1
          FROM j_1bbranch
          INTO TABLE it_j_1bbranch
           FOR ALL ENTRIES IN it_zlest0035
         WHERE branch EQ it_zlest0035-werks.

        SORT it_j_1bbranch BY branch .

        LOOP AT it_zlest0035 INTO wa_zlest0035 .

          sl_zlest0035_aux2-nr_nf = wa_zlest0035-nr_nf.
          sl_zlest0035_aux2-werks = wa_zlest0035-werks.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_zlest0035-cnpj
            IMPORTING
              output = sl_zlest0035_aux2-cnpj.


          READ TABLE it_j_1bbranch INTO wa_j_1bbranch WITH KEY  branch = sl_zlest0035_aux2-werks BINARY SEARCH.

          sl_zlest0035_aux2-cgc_cliente = wa_j_1bbranch-stcd1.

          APPEND sl_zlest0035_aux2 TO it_zlest0035_aux2.

        ENDLOOP.


        SELECT z~serie_despacho
               z~nr_despacho
               z~nr_nf
               e~cgc_remetente
               d~cgc_cliente
          FROM zlest0003 AS z
         INNER JOIN zlest0004 AS e ON z~serie_despacho EQ e~serie_despacho AND z~nr_despacho EQ  e~nr_despacho
         INNER JOIN zlest0006 AS d ON d~nr_fatura EQ e~nr_fatura
          INTO TABLE it_zlest0003_aux
           FOR ALL ENTRIES IN it_zlest0035_aux2
         WHERE z~nr_nf          EQ it_zlest0035_aux2-nr_nf
           AND e~cgc_remetente  EQ it_zlest0035_aux2-cnpj
           AND d~cgc_cliente    EQ it_zlest0035_aux2-cgc_cliente.

        IF sy-subrc IS INITIAL.
          "-------------0004-----------

          SELECT *
            FROM zlest0004
            INTO TABLE it_zlest0004_aux
            FOR ALL ENTRIES IN it_zlest0003_aux
            WHERE serie_despacho EQ it_zlest0003_aux-serie_despacho
              AND nr_despacho    EQ it_zlest0003_aux-nr_despacho.

          IF sy-subrc IS INITIAL.
            "-------------0006-----------
            SELECT *
              FROM zlest0006
              INTO TABLE it_zlest0006_aux
              FOR ALL ENTRIES IN it_zlest0004_aux
              WHERE nr_fatura   EQ it_zlest0004_aux-nr_fatura.


            REFRESH it_zlest0019_aux.

            LOOP AT it_zlest0004_aux INTO sl_zlest0004.

              sl_zlest0019-seriedcl = sl_zlest0004-serie_despacho.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = sl_zlest0004-nr_despacho
                IMPORTING
                  output = sl_zlest0019-dcl.
              APPEND sl_zlest0019 TO tl_zlest0019.

              CLEAR: sl_zlest0004,
                     sl_zlest0019.
            ENDLOOP.

            SELECT *
              FROM zlest0019
              INTO TABLE it_zlest0019_aux
              FOR ALL ENTRIES IN tl_zlest0019
            WHERE dcl      EQ tl_zlest0019-dcl
              AND seriedcl EQ tl_zlest0019-seriedcl
              AND idinter  EQ 'L2'
              AND tp_reg   EQ '30'
              AND nfenum   IN p_nr_nf.

            DELETE it_zlest0019_aux WHERE NOT nfnum IS INITIAL.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "f_seleciona_dados



*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .

  SORT: it_zlest0006     BY nr_fatura     ,
        it_zlest0004     BY nr_fatura     ,
        it_zlest0003     BY serie_despacho nr_despacho,
        it_zlest0035     BY nr_nf cnpj    ,
        it_t001w         BY werks         ,
        it_j_1bnfdoc     BY docnum        ,
        it_j_1bnflin     BY docnum        ,
        it_makt          BY matnr         ,
        it_parceiro      BY vbeln         ,
        it_lfa1          BY lifnr         ,
        it_zlest0003_aux BY nr_nf cgc_remetente cgc_cliente,
        it_zlest0004_aux BY serie_despacho nr_despacho,
        it_zlest0006_aux BY nr_fatura     ,
        it_zlest0019_aux BY dcl seriedcl nfenum,
        it_j_1bbranch    BY branch        ,
        it_zlest0045     BY docnum        .

  DATA: vl_cnpj  TYPE zlest0004-cgc_remetente,
        wdcl     TYPE zlest0004-nr_despacho,
        wl_chave TYPE zlest0045-chave.

  CLEAR: wl_chave.

  LOOP AT it_zlest0035 INTO wa_zlest0035 ."WHERE nr_nf = wa_zlest0003-nr_nf AND cnpj = wa_zlest0003-cgc_remetente.

    wa_saida-nr_nf          = wa_zlest0035-nr_nf     .
    wa_saida-serie_nf       = wa_zlest0035-serie_nf  .
    wa_saida-docnum         = wa_zlest0035-docnum    .
    wa_saida-cnpj           = wa_zlest0035-cnpj      .
    wa_saida-werks          = wa_zlest0035-werks     .
    wa_saida-qtd_nf         = wa_zlest0035-qtd_nf    .
    wa_saida-qtd_cheg       = wa_zlest0035-qtd_cheg  .
    wa_saida-dtachegada     = wa_zlest0035-dtachegada.
    wa_saida-cgc_cliente    = wa_zlest0035-cnpj      .
    wa_saida-qtd_util       = 0                      .
    CLEAR wa_saida-fatura .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zlest0035-cnpj
      IMPORTING
        output = vl_cnpj.

    READ TABLE it_j_1bbranch INTO wa_j_1bbranch WITH KEY  branch = wa_zlest0035-werks BINARY SEARCH.

*    READ TABLE it_zlest0003_aux INTO wa_zlest0003_aux WITH KEY nr_nf         = wa_zlest0035-nr_nf
*                                                               cgc_remetente = vl_cnpj
*                                                               cgc_cliente   = wa_j_1bbranch-stcd1 BINARY SEARCH.
    LOOP AT it_zlest0003_aux INTO wa_zlest0003_aux WHERE nr_nf         = wa_zlest0035-nr_nf AND
                                                         cgc_remetente = vl_cnpj AND
                                                         cgc_cliente   = wa_j_1bbranch-stcd1 .

      READ TABLE it_zlest0004_aux INTO wa_zlest0004_aux WITH KEY serie_despacho = wa_zlest0003_aux-serie_despacho nr_despacho = wa_zlest0003_aux-nr_despacho BINARY SEARCH.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zlest0004_aux-nr_despacho
        IMPORTING
          output = wdcl.

      READ TABLE it_zlest0006_aux INTO wa_zlest0006_aux WITH KEY nr_fatura = wa_zlest0004_aux-nr_fatura BINARY SEARCH.

      LOOP AT it_zlest0019_aux INTO wa_zlest0019_aux WHERE dcl = wdcl AND seriedcl = wa_zlest0004_aux-serie_despacho AND nfenum = wa_zlest0035-nr_nf.

        IF wa_zlest0006_aux-nr_frete IS NOT INITIAL .
*          WA_SAIDA-QTD_UTIL = WA_SAIDA-QTD_UTIL + WA_ZLEST0019_AUX-PESODVAGAO.

          IF wa_saida-fatura IS INITIAL.
            wa_saida-fatura = 'Fatura'.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zlest0035-werks BINARY SEARCH.
    wa_saida-name1          = wa_t001w-name1.

    READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_zlest0035-docnum BINARY SEARCH.

    READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY docnum = wa_zlest0035-docnum BINARY SEARCH.


    READ TABLE it_parceiro INTO wa_parceiro WITH KEY vbeln = wa_j_1bnflin-refkey(10) BINARY SEARCH.

    IF sy-subrc IS INITIAL .

      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_parceiro-lifnr.
      wa_saida-terminal    = wa_lfa1-name1.

    ELSE.

      IF NOT p_client IS INITIAL.
        CONTINUE.
      ENDIF.

    ENDIF.

    READ TABLE it_makt      INTO wa_makt      WITH KEY matnr = wa_j_1bnflin-matnr BINARY SEARCH.


    IF ( NOT p_matnr IS INITIAL  ) AND ( wa_makt-matnr NE p_matnr ).
      CONTINUE.
    ENDIF.

    wa_saida-maktx = wa_makt-maktx.

    LOOP AT it_zlest0045 INTO wa_zlest0045 WHERE docnum EQ wa_zlest0035-docnum.

      READ TABLE it_zlest0044 INTO wa_zlest0044 WITH KEY chave_cte = wa_zlest0045-chave_cte.

      IF wa_zlest0044-cancelado NE 'X' AND wa_zlest0044-nr_trans IS NOT INITIAL.
*        WA_SAIDA-QTD_UTIL = WA_SAIDA-QTD_UTIL + WA_ZLEST0045-PESO_RATEADO.

        IF wa_saida-fatura IS INITIAL.
          wa_saida-fatura = 'Fatura'.
        ENDIF.

      ENDIF.

    ENDLOOP.

*    WA_SAIDA-SALDO = WA_ZLEST0035-QTD_CHEG - WA_SAIDA-QTD_UTIL .

    CALL FUNCTION 'Z_LES_BUSCA_SALDO_FERROVIARIO'
      EXPORTING
        nr_nf              = wa_saida-nr_nf
        serie_nf           = wa_saida-serie_nf
        cnpj               = wa_saida-cnpj
        werks              = wa_saida-werks
*       docnum             = wa_saida-docnum
      IMPORTING
        saldo              = wa_saida-saldo
        qtd_cheg           = wa_saida-qtd_cheg
        qtd_utilizada      = wa_saida-qtd_util
      EXCEPTIONS
        qtd_cheg_not_found = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      IF wa_saida-qtd_util GT 0.
        wa_saida-fatura = 'Fatura'.
      ELSE.
        CLEAR: wa_saida-fatura.
      ENDIF.
    ENDIF.

    APPEND wa_saida TO it_saida.

    CLEAR: wa_saida        ,
           wa_zlest0035    ,
           wa_zlest0003_aux,
           wa_zlest0004_aux,
           wa_zlest0006_aux.

  ENDLOOP.


ENDFORM.                    " F_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING    VALUE(p_flag)
                           VALUE(p_fnam)
                           VALUE(p_fval).

  CLEAR t_bdc.
  IF NOT p_flag IS INITIAL.
    t_bdc-program  = p_fnam.
    t_bdc-dynpro   = p_fval.
    t_bdc-dynbegin = 'X'.
  ELSE.
    t_bdc-fnam = p_fnam.
    t_bdc-fval = p_fval.
  ENDIF.
  APPEND t_bdc.

ENDFORM.                    " F_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_status OUTPUT.
  SET PF-STATUS 'STATUS_ALV'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_layout .
  wa_layout-zebra = 'X'.
ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_tabname TYPE dd02d-tabname
                               p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c           .
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = p_tabname                         .
  wl_fcat-fieldname = p_campo                           .
  wl_fcat-scrtext_l = p_desc                            .
  wl_fcat-scrtext_m = p_desc                            .
  wl_fcat-scrtext_s = p_desc                            .
  wl_fcat-hotspot   = p_hot                             .
  wl_fcat-no_zero   = p_zero                            .

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT


CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA : wa_event  TYPE REF TO lcl_event_receiver,
       wa_event2 TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                  e_column_id
                  es_row_no                      ,

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object e_interactive                   ,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
    PERFORM z_handle_hotspot USING    e_row_id
                                      e_column_id
                                      es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_exibe_alv OUTPUT.

  s_variant-report = sy-repid.
  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
  IF wa_alv IS INITIAL AND NOT
    wa_cont IS INITIAL.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event IS INITIAL.

    CREATE OBJECT wa_event.
    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.

  ENDIF.

*  wa_layout-GRID_TITLE = 'Controle de Saldo Ferroviário'.
  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      i_default                     = 'X'
      is_variant                    = s_variant      "is_layout = s_layout
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = it_saida
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK NOT wa_alv IS INITIAL.
ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_user_command INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.

    ENDCASE.
  ENDIF.

  IF sy-dynnr EQ '0200'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0 .
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM z_handle_hotspot  USING    p_e_row_id TYPE lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no TYPE  lvc_s_roid.

  DATA: vl_nfobjn TYPE j_1binterf-nfobjn,
        vl_docnum TYPE j_1bnfdoc-docnum.

  IF  p_e_column_id = 'DOCNUM'.


    READ TABLE it_saida INDEX p_e_row_id-index INTO wa_saida.

    vl_docnum = wa_saida-docnum.

    CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
      EXPORTING
        doc_number         = vl_docnum
      IMPORTING
        obj_number         = vl_nfobjn
      EXCEPTIONS
        document_not_found = 1
        docum_lock         = 2
        OTHERS             = 3.

    CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
      EXPORTING
        obj_number         = vl_nfobjn
      EXCEPTIONS
        object_not_found   = 1
        scr_ctrl_not_found = 2
        OTHERS             = 3.

  ENDIF.
  IF  p_e_column_id = 'FATURA'.
    READ TABLE it_saida INDEX p_e_row_id-index INTO wa_saida.

    IF wa_saida-fatura = 'Fatura'.

      PERFORM : f_seleciona_dados2 USING wa_saida-nr_nf
                                         wa_saida-cnpj
                                         wa_saida-cgc_cliente
                                         wa_saida-docnum,
                f_saida_fat ,
                f_alv_fat .

      CALL SCREEN 0200.

    ENDIF.

  ENDIF.


*  case p_e_column_id.
*    when 'VBELN'.
*      read table it_saida into wa_saida index p_es_row_no-row_id.
*      if not wa_saida-vbeln is initial.
*        set parameter id 'AUN' field wa_saida-vbeln. "preenche o campo da tela de pesquisa
*        call transaction 'VA03' and skip first screen. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado
*
*      endif.
*
*
*  endcase.

ENDFORM.                    " Z_HANDLE_HOTSPOT


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM z_handle_toolbar  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.



ENDFORM.                    " Z_HANDLE_TOOLBAR


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM z_handle_command  USING p_ucomm TYPE syucomm       .

  CASE p_ucomm.
    WHEN 'REMESSA'.
*     Gera Remessa
      CALL METHOD wa_alv->refresh_table_display .

  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND


*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv.
  PERFORM alv_preenche_cat USING:
"        'NR_NF_ALL'      text-011   '40'  ' '  ' ',"  DACTE
"        'NR_FATURA'      text-012   '40'  ' '  ' ',"  NR FATURA
"        'SERIE_DESPACHO' text-013   '40'  ' '  ' ',"  Série Despacho
"        'NR_DESPACHO'    text-014   '40'  ' '  ' ',"  Nr Despacho
       'IT_SAIDA' 'FATURA'         ''         '6'   'X'  ' ',"  nr nota
       'IT_SAIDA' 'NR_NF'          text-002   '4'   ' '  ' ',"  nr nota
       'IT_SAIDA' 'SERIE_NF'       text-003   '4'   ' '  ' ',"  serie
       'IT_SAIDA' 'DOCNUM'         text-004   '40'  'X'  ' ',"  Documento
       'IT_SAIDA' 'CNPJ'           text-005   '2'   ' '  ' ',"  Nr id Fiscal
       'IT_SAIDA' 'TERMINAL'       text-018   '40'  ' '  ' ',
       'IT_SAIDA' 'WERKS'          text-006   '10'  ' '  ' ',"  Centro
       'IT_SAIDA' 'NAME1'          text-017   '40'  ' '  ' ',"  Nome Centro
       'IT_SAIDA' 'MAKTX'          text-015   '40'  ' '  ' ',"  Produto
       'IT_SAIDA' 'QTD_NF'         text-007   '10'  ' '  ' ',"  qtde Origem
       'IT_SAIDA' 'QTD_CHEG'       text-008   '40'  ' '  ' ',"  Qtde Chegada
       'IT_SAIDA' 'QTD_UTIL'       text-019   '40'  ' '  ' ',"  Qtde Utilizada
       'IT_SAIDA' 'SALDO'          text-009   '40'  ' '  ' ',"  Saldo
       'IT_SAIDA' 'DTACHEGADA'     text-010   '10'  ' '  ' '.


ENDFORM.                    " F_ALV

INCLUDE zlesr0015_f_seleciona_dadosf01.
