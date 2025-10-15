*&---------------------------------------------------------------------*
*& Report  ZFI_LIVRO_REG_P3
*& Livro Registro da Produção e Estoque - Modelo 3
*&---------------------------------------------------------------------*
* Responsável ...: Marcus Luciano Costa Bárbara                        *
* Data desenv ...: 26.04.2010                                          *
************************************************************************

REPORT  zfi_livro_reg_p3.

TYPES: BEGIN OF tp_tabela,
        budat(8),
        bukrs(4),
        werks(4),
        mblnr(10),
        mjahr(4),
        bwart(3),
        matnr(18),
        menge(13),
        shkzg(1),
        dmbtr(13),
        ebeln(10),
        lbkum(13),
        salk3(13),
        belnr(10),
        rmwwr(13),
        rbkp_budat(8),
        docnum(20),
        nfnum(6),
        series(3),
        maktx(40),
        meins(3),
        steuc(16),
        text1(60),
     END OF tp_tabela.

*----------------------------------------------------------------------*
* Tabelas internas
*----------------------------------------------------------------------*
DATA: BEGIN OF wa_mkpf,
        mjahr LIKE mkpf-mjahr,
        mblnr LIKE mkpf-mblnr,
        budat LIKE mkpf-budat,
      END OF wa_mkpf,

      BEGIN OF wa_mseg,
        bukrs LIKE mseg-bukrs,
        werks LIKE mseg-werks,
        mblnr LIKE mseg-mblnr,
        mjahr LIKE mseg-mjahr,
        bwart LIKE mseg-bwart,
        matnr LIKE mseg-matnr,
        menge LIKE mseg-menge,
        shkzg LIKE mseg-shkzg,
        dmbtr LIKE mseg-dmbtr,
        ebeln LIKE mseg-ebeln,
        ebelp LIKE mseg-ebelp,
        lbkum LIKE mseg-lbkum,
        salk3 LIKE mseg-salk3,
        lfbja LIKE mseg-lfbja,
        lfbnr LIKE mseg-lfbnr,
      END OF wa_mseg,

      BEGIN OF wa_ekbe,
        ebeln LIKE ekbe-ebeln,
        ebelp LIKE ekbe-ebelp,
        lfgja LIKE ekbe-lfgja,
        lfbnr LIKE ekbe-lfbnr,
        gjahr LIKE ekbe-gjahr,
        belnr LIKE ekbe-belnr,
      END OF wa_ekbe,

      BEGIN OF wa_rbkp,
        gjahr LIKE rbkp-gjahr,
        belnr	LIKE rbkp-belnr,
        rmwwr LIKE rbkp-rmwwr,
        budat LIKE rbkp-budat,
      END OF wa_rbkp,

      BEGIN OF wa_j_1bnfdoc,
        belnr  LIKE j_1bnfdoc-belnr,
        pstdat LIKE j_1bnfdoc-pstdat,
        docnum LIKE j_1bnfdoc-docnum,
        nfnum  LIKE j_1bnfdoc-nfnum,
        series LIKE j_1bnfdoc-series,
      END OF wa_j_1bnfdoc,

      BEGIN OF wa_makt,
        matnr LIKE makt-matnr,
        mtart LIKE mara-mtart,
        maktx LIKE makt-maktx,
        meins LIKE mara-meins,
        steuc LIKE marc-steuc,
        text1 LIKE t604n-text1,
      END OF wa_makt,

      wa_mseg2  LIKE wa_mseg,
      it_mkpf  LIKE STANDARD TABLE OF wa_mkpf,
      it_mseg  LIKE STANDARD TABLE OF wa_mseg,
      it_mseg2 LIKE STANDARD TABLE OF wa_mseg,
      it_mseg3 LIKE STANDARD TABLE OF wa_mseg,
      it_ekbe  LIKE STANDARD TABLE OF wa_ekbe,
      it_rbkp  LIKE STANDARD TABLE OF wa_rbkp,
      it_j_1bnfdoc LIKE STANDARD TABLE OF wa_j_1bnfdoc,
      it_makt LIKE STANDARD TABLE OF wa_makt.

DATA: BEGIN OF wa_arqtxt.
        INCLUDE STRUCTURE zmodelofiscalp3.
DATA: END OF wa_arqtxt,

      it_arqtxt LIKE STANDARD TABLE OF zmodelofiscalp3 WITH DEFAULT KEY.

DATA: BEGIN OF wa_ekpo.
        INCLUDE STRUCTURE ekpo.
DATA: END OF wa_ekpo,

      it_ekpo LIKE STANDARD TABLE OF wa_ekpo.

CONSTANTS: c_pathback TYPE c LENGTH 200 VALUE '/usr/interfaces/'.

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
PARAMETERS: s_bukrs LIKE mseg-bukrs,
            s_werks LIKE mseg-werks,
            s_ano   LIKE t009b-bdatj,
            s_mes   LIKE t009b-poper.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s02.
PARAMETERS: p_cami  LIKE rlgrap-filename,  "Arq Excel/TXT
            p_ptchb LIKE rlgrap-filename MODIF ID pat.  "Arq APLIC.SERVER
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Event initialization
*----------------------------------------------------------------------*
INITIALIZATION.

  IF ( sy-mandt EQ 50 ) OR ( sy-mandt EQ 60 ).
    p_cami = '\\sapdev\Interfaces\'.
  ELSEIF ( sy-mandt EQ 160 ).
    p_cami = '\\sapqas\Interfaces\'.
  ELSEIF ( sy-mandt EQ 300 ).
    p_cami = '\\sapprd\Interfaces\'.
  ENDIF.

  p_ptchb = c_pathback.
*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF s_bukrs IS INITIAL.
    MESSAGE 'Deve ser informado a empresa!' TYPE 'I'.
    STOP.
  ENDIF.

  IF s_werks IS INITIAL.
    MESSAGE 'Deve ser informado a filial!' TYPE 'I'.
    STOP.
  ENDIF.

  IF s_ano IS INITIAL.
    MESSAGE 'Deve ser informado o ano!' TYPE 'I'.
    STOP.
  ENDIF.

  IF s_mes IS INITIAL.
    MESSAGE 'Deve ser informado o mês!' TYPE 'I'.
    STOP.
  ENDIF.

*----------------------------------------------------------------------*
* Start or Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: periv      LIKE t009b-periv,
        dt_inicial LIKE sy-datum,
        dt_final   LIKE sy-datum.

  SELECT SINGLE periv FROM t001 INTO periv WHERE bukrs = s_bukrs.

  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = s_ano
      i_periv        = periv
      i_poper        = s_mes
    IMPORTING
      e_date         = dt_inicial
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = s_ano
      i_periv        = periv
      i_poper        = s_mes
    IMPORTING
      e_date         = dt_final
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.

  CLEAR: it_mkpf, it_mseg, it_ekbe, it_rbkp, it_j_1bnfdoc.

  SELECT mjahr mblnr budat
    FROM mkpf
    INTO CORRESPONDING FIELDS OF TABLE it_mkpf
   WHERE mjahr EQ s_ano
     AND budat BETWEEN dt_inicial AND dt_final.

  SELECT a~bukrs a~werks a~mblnr a~mjahr a~bwart a~matnr
         a~menge a~shkzg a~dmbtr a~ebeln a~ebelp a~ebelp
         a~lbkum a~salk3 a~lfbja a~lfbnr
    FROM mseg AS a
    INTO CORRESPONDING FIELDS OF TABLE it_mseg
     FOR ALL ENTRIES IN it_mkpf
   WHERE a~bukrs EQ s_bukrs
     AND a~werks EQ s_werks
     AND a~mblnr EQ it_mkpf-mblnr
     AND a~mjahr EQ it_mkpf-mjahr.

  LOOP AT it_mseg INTO wa_mseg.

    IF ( NOT wa_mseg-ebeln IS INITIAL ) AND ( NOT wa_mseg-ebelp IS INITIAL ).
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_ekpo
        FROM ekpo
       WHERE ebeln EQ wa_mseg-ebeln
         AND ebelp EQ wa_mseg-ebelp
         AND knttp EQ ''.

      IF sy-subrc EQ 0.
        APPEND wa_mseg TO it_mseg2.
      ENDIF.
    ELSE.
      APPEND wa_mseg TO it_mseg2.
    ENDIF.

  ENDLOOP.

  it_mseg = it_mseg2.

  CLEAR: it_mseg2, it_mseg3.

  it_mseg3 = it_mseg.

  LOOP AT it_mseg INTO wa_mseg.
    IF ( wa_mseg-bwart EQ '102' ) AND ( NOT wa_mseg-lfbja IS INITIAL ) AND ( NOT wa_mseg-lfbnr IS INITIAL ).
      READ TABLE it_mseg3 INTO wa_mseg2 WITH KEY mblnr = wa_mseg-lfbnr mjahr = wa_mseg-lfbja.
      IF sy-subrc EQ 0.
        DELETE it_mseg3 WHERE mblnr = wa_mseg2-lfbnr AND mjahr = wa_mseg2-lfbja.
        DELETE it_mseg3 WHERE lfbnr = wa_mseg2-lfbnr AND lfbja = wa_mseg2-lfbja.
      ENDIF.
    ENDIF.
  ENDLOOP.

  it_mseg = it_mseg3.

  SELECT ebeln ebelp lfgja lfbnr gjahr belnr
    FROM ekbe
    INTO CORRESPONDING FIELDS OF TABLE it_ekbe
     FOR ALL ENTRIES IN it_mseg
   WHERE ebeln EQ it_mseg-ebeln
     AND ebelp EQ it_mseg-ebelp
     AND vgabe EQ '2'
     AND werks EQ it_mseg-werks.

  SELECT belnr gjahr rmwwr budat
    FROM rbkp
    INTO CORRESPONDING FIELDS OF TABLE it_rbkp
     FOR ALL ENTRIES IN it_ekbe
   WHERE belnr EQ it_ekbe-belnr
     AND gjahr EQ it_ekbe-gjahr.

  SELECT belnr pstdat docnum nfnum series
    FROM j_1bnfdoc
    INTO CORRESPONDING FIELDS OF TABLE it_j_1bnfdoc
     FOR ALL ENTRIES IN it_rbkp
   WHERE belnr  EQ it_rbkp-belnr
     AND gjahr  EQ it_rbkp-gjahr
     AND pstdat EQ it_rbkp-budat
     AND bukrs  EQ s_bukrs
     AND branch EQ s_werks.

  PERFORM popula_material_info_filial.

  PERFORM popula_arquivo.

  PERFORM grava_arquivo.

*&---------------------------------------------------------------------*
*&      Form  POPULA_MATERIAL_INFO_MATERIAL
*&---------------------------------------------------------------------*
*       Popula informações do Segmento de documento - material
*----------------------------------------------------------------------*
FORM popula_material_info_filial.

  DATA: matnr LIKE wa_mseg-matnr.

  CLEAR: it_makt, matnr.

  SORT it_mseg BY matnr.

  LOOP AT it_mseg INTO wa_mseg.

    matnr = wa_mseg-matnr.

    AT NEW matnr.
      CLEAR: wa_makt.
      wa_makt-matnr = matnr.

      SELECT SINGLE mtart INTO wa_makt-mtart FROM mara
       WHERE matnr EQ wa_makt-matnr.

      SELECT SINGLE maktx INTO wa_makt-maktx FROM makt
       WHERE matnr EQ wa_makt-matnr.

      SELECT SINGLE meins INTO wa_makt-meins FROM mara
       WHERE matnr EQ wa_makt-matnr.

      SELECT SINGLE steuc INTO wa_makt-steuc FROM marc
       WHERE matnr EQ wa_makt-matnr
         AND werks EQ wa_mseg-werks.

      SELECT SINGLE text1 INTO wa_makt-text1 FROM t604n
       WHERE land1 EQ 'BR'
         AND steuc EQ wa_makt-steuc.

      APPEND wa_makt TO it_makt.
    ENDAT.

  ENDLOOP.

ENDFORM.                    " POPULA_MATERIAL


*&---------------------------------------------------------------------*
*&      Form  POPULA_ARQUIVO
*&---------------------------------------------------------------------*
*       Popula tabela interna do aquivo.
*----------------------------------------------------------------------*
FORM popula_arquivo .

  LOOP AT it_mkpf INTO wa_mkpf.

    CLEAR: wa_arqtxt.

    wa_arqtxt-budat = wa_mkpf-budat.

    LOOP AT it_mseg INTO wa_mseg WHERE mblnr EQ wa_mkpf-mblnr
                                   AND mjahr EQ wa_mkpf-mjahr.

      CLEAR: wa_arqtxt-bukrs, wa_arqtxt-werks, wa_arqtxt-mblnr,
             wa_arqtxt-mjahr, wa_arqtxt-bwart, wa_arqtxt-matnr,
             wa_arqtxt-menge, wa_arqtxt-shkzg, wa_arqtxt-dmbtr,
             wa_arqtxt-ebeln, wa_arqtxt-lbkum, wa_arqtxt-salk3,
             wa_arqtxt-belnr, wa_arqtxt-rmwwr, wa_arqtxt-rbkp_budat,
             wa_arqtxt-docnum, wa_arqtxt-nfnum, wa_arqtxt-series,
             wa_arqtxt-maktx, wa_arqtxt-meins, wa_arqtxt-steuc,
             wa_arqtxt-text1.

      wa_arqtxt-bukrs = wa_mseg-bukrs.
      wa_arqtxt-werks = wa_mseg-werks.
      wa_arqtxt-mblnr = wa_mseg-mblnr.
      wa_arqtxt-mjahr = wa_mseg-mjahr.
      wa_arqtxt-bwart = wa_mseg-bwart.
      wa_arqtxt-matnr = wa_mseg-matnr.
      wa_arqtxt-menge = wa_mseg-menge.
      wa_arqtxt-shkzg = wa_mseg-shkzg.
      wa_arqtxt-dmbtr = wa_mseg-dmbtr.
      wa_arqtxt-ebeln = wa_mseg-ebeln.
      wa_arqtxt-lbkum = wa_mseg-lbkum.
      wa_arqtxt-salk3 = wa_mseg-salk3.

      READ TABLE it_ekbe INTO wa_ekbe WITH KEY ebeln = wa_mseg-ebeln
                                               ebelp = wa_mseg-ebelp.
      IF sy-subrc EQ 0.
        wa_arqtxt-belnr = wa_ekbe-belnr.
        READ TABLE it_rbkp INTO wa_rbkp WITH KEY belnr = wa_ekbe-belnr
                                                 gjahr = wa_ekbe-gjahr.
        IF sy-subrc EQ 0.
          wa_arqtxt-rmwwr      = wa_rbkp-rmwwr.
          wa_arqtxt-rbkp_budat = wa_rbkp-budat.
          READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY belnr  = wa_arqtxt-belnr
                                                             pstdat = wa_arqtxt-rbkp_budat.
          IF sy-subrc EQ 0.
            wa_arqtxt-docnum = wa_j_1bnfdoc-docnum.
            wa_arqtxt-nfnum  = wa_j_1bnfdoc-nfnum.
            wa_arqtxt-series = wa_j_1bnfdoc-series.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_arqtxt-matnr.
      IF sy-subrc EQ 0.
        wa_arqtxt-mtart = wa_makt-mtart.
        wa_arqtxt-maktx = wa_makt-maktx.
        wa_arqtxt-meins = wa_makt-meins.
        wa_arqtxt-steuc = wa_makt-steuc.
        wa_arqtxt-text1 = wa_makt-text1.
      ENDIF.

      IF ( wa_arqtxt-mtart NE 'ZHIB' ) AND ( wa_arqtxt-mtart NE 'ZLAG' ).
        APPEND wa_arqtxt TO it_arqtxt.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " POPULA_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  GRAVA_ARQUIVO
*&---------------------------------------------------------------------*
*       Grava arquivo
*----------------------------------------------------------------------*
FORM grava_arquivo .

  DATA: wa_tabela TYPE tp_tabela,
        it_tabela TYPE tp_tabela OCCURS 0 WITH HEADER LINE.

  CLEAR it_tabela[].

  it_tabela-budat = 'C01'.
  it_tabela-bukrs = 'C02'.
  it_tabela-werks = 'C03'.
  it_tabela-mblnr = 'C04'.
  it_tabela-mjahr = 'C05'.
  it_tabela-bwart = 'C06'.
  it_tabela-matnr = 'C07'.
  it_tabela-menge = 'C08'.
  it_tabela-shkzg = 'C09'.
  it_tabela-dmbtr = 'C10'.
  it_tabela-ebeln = 'C11'.
  it_tabela-lbkum = 'C12'.
  it_tabela-salk3 = 'C13'.
  it_tabela-belnr = 'C14'.
  it_tabela-rmwwr = 'C15'.
  it_tabela-rbkp_budat = 'C16'.
  it_tabela-docnum = 'C17'.
  it_tabela-nfnum  = 'C18'.
  it_tabela-series = 'C19'.
  it_tabela-maktx  = 'C20'.
  it_tabela-meins  = 'C21'.
  it_tabela-steuc  = 'C22'.
  it_tabela-text1  = 'C23'.
  APPEND it_tabela.

  LOOP AT it_arqtxt INTO wa_arqtxt.
    it_tabela-budat = wa_arqtxt-budat.
    it_tabela-bukrs = wa_arqtxt-bukrs.
    it_tabela-werks = wa_arqtxt-werks.
    it_tabela-mblnr = wa_arqtxt-mblnr.
    it_tabela-mjahr = wa_arqtxt-mjahr.
    it_tabela-bwart = wa_arqtxt-bwart.
*---> 09/06/2023 - Migração S4 - JS
*     it_tabela-matnr = wa_arqtxt-matnr.
    it_tabela-matnr = CONV #( wa_arqtxt-matnr ).
*<--- 09/06/2023 - Migração S4 - JS
    it_tabela-menge = wa_arqtxt-menge.
    it_tabela-shkzg = wa_arqtxt-shkzg.
    it_tabela-dmbtr = wa_arqtxt-dmbtr.
    it_tabela-ebeln = wa_arqtxt-ebeln.
    it_tabela-lbkum = wa_arqtxt-lbkum.
    it_tabela-salk3 = wa_arqtxt-salk3.
    it_tabela-belnr = wa_arqtxt-belnr.
    it_tabela-rmwwr = wa_arqtxt-rmwwr.
    it_tabela-rbkp_budat = wa_arqtxt-rbkp_budat.
    it_tabela-docnum = wa_arqtxt-docnum.
    it_tabela-nfnum = wa_arqtxt-nfnum.
    it_tabela-series = wa_arqtxt-series.
    it_tabela-maktx = wa_arqtxt-maktx.

    DATA: meins TYPE string,
          lg    TYPE sy-langu.

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = 'PT'
      IMPORTING
        output = lg.

    meins = wa_arqtxt-meins.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input    = meins
        language = lg
      IMPORTING
        output   = meins.

    IF meins EQ 'PEÇ'.
      meins = 'PEC'.
    ENDIF.

    it_tabela-meins = meins.
    it_tabela-steuc = wa_arqtxt-steuc.
    it_tabela-text1 = wa_arqtxt-text1.
    APPEND it_tabela.
  ENDLOOP.

  IF sy-batch EQ space.
    DATA: nome_arq TYPE rlgrap-filename.
    CONCATENATE p_cami s_bukrs s_werks s_ano s_mes '.xls' INTO nome_arq.
    CALL FUNCTION 'SAP_CONVERT_TO_XLS_FORMAT'
      EXPORTING
        i_filename     = nome_arq
      TABLES
        i_tab_sap_data = it_tabela.

    IF sy-subrc EQ 0.
      MESSAGE 'Arquivo gerado!' TYPE 'I'.
    ENDIF.

  ELSEIF sy-batch EQ 'X'.

    DATA: v_arquivo LIKE rlgrap-filename,
          file TYPE c LENGTH 250.

    CONCATENATE p_ptchb s_bukrs s_werks s_ano s_mes '.txt' INTO v_arquivo.

    file = v_arquivo.

    OPEN DATASET v_arquivo IN TEXT MODE ENCODING NON-UNICODE FOR OUTPUT
                              IGNORING CONVERSION ERRORS
                              WITH SMART LINEFEED.

    LOOP AT it_tabela[] INTO wa_tabela.
      TRANSFER wa_tabela TO file.
    ENDLOOP.

    CLOSE DATASET file.

  ENDIF.

ENDFORM.                    " GRAVA_ARQUIVO
