************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 03.06.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Gerar Arquivo de Log de Objetos de custo não        *
*                  incluidos na estrutura da DRE                       *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 03.06.2009    Marcus Barbara       Criação              DEVK905939   *
************************************************************************

REPORT  zgl010.

CONSTANTS: c_path     TYPE c LENGTH 200 VALUE 'C:\'.
CONSTANTS: c_pathback TYPE c LENGTH 200 VALUE '/usr/interfaces/dre/'.

TABLES:
  zgl005_dre_dados,
  t077s,
*Início Alteração Ricardo Furst.
  coas.
*Fim Alteração Ricardo Furst.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: p_bukrs FOR zgl005_dre_dados-bukrs OBLIGATORY,
                p_grupo FOR t077s-ktoks MATCHCODE OBJECT zt077s_ktoks OBLIGATORY.
PARAMETERS:
          p_monat      LIKE zgl005_dre_dados-monat OBLIGATORY,
          p_gjahr      LIKE zgl005_dre_dados-gjahr OBLIGATORY.
*Início Alteração Ricardo Furst.
SELECT-OPTIONS: p_auart FOR coas-auart.
*Fim Alteração Ricardo Furst.
SELECTION-SCREEN END   OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s02.
PARAMETERS:
          p_ptch        LIKE rlgrap-filename MODIF ID pat,  "Arq APLIC.SERVER
          p_ptchb       LIKE rlgrap-filename MODIF ID pat,  "Arq APLIC.SERVER
          p_arq         LIKE rlgrap-filename,               "Arq APLIC.SERVER
          p_lucro       LIKE rlgrap-filename.               "Arq APLIC.SERVER
SELECTION-SCREEN END   OF BLOCK b1.

DATA: BEGIN OF wa_ska1,
        ktopl LIKE ska1-ktopl,
        ktoks LIKE ska1-ktoks,
        saknr LIKE ska1-saknr,
      END OF wa_ska1,

      BEGIN OF wa_bsis,
        bukrs LIKE bsis-bukrs,
        hkont LIKE bsis-hkont,
        kostl LIKE bsis-kostl, "Centro de Custo
        prctr LIKE bsis-prctr, "Centro de Lucro
        belnr LIKE bsis-belnr,
        gsber LIKE bsis-gsber,
        dmbtr LIKE bsis-dmbtr,
        dmbe2 LIKE bsis-dmbe2,
        shkzg LIKE bsis-shkzg,
        budat LIKE bsis-budat,
*Início Alteração Ricardo Furst.
        aufnr LIKE bsis-aufnr,
*Fim Alteração Ricardo Furst.
      END OF wa_bsis,

*Início Alteração Ricardo Furst.
      BEGIN OF wa_coas.
        INCLUDE STRUCTURE coas.
DATA: END OF wa_coas,
*Fim Alteração Ricardo Furst.

      BEGIN OF wa_skat,
        saknr LIKE skat-saknr,
        txt50 LIKE skat-txt50,
      END OF wa_skat,

      BEGIN OF wa_cskt,
        kostl LIKE cskt-kostl,
        ltext LIKE cskt-ltext,
      END OF wa_cskt,

      BEGIN OF wa_csks,
        kostl LIKE csks-kostl,
        ersda LIKE csks-ersda,
        usnam LIKE csks-usnam,
      END OF wa_csks,

      BEGIN OF wa_cepct,
        prctr LIKE cepct-prctr,
        ltext LIKE cepct-ltext,
      END OF wa_cepct,

      BEGIN OF wa_zgl004,
        bukrs LIKE zgl004_dre_est-bukrs,
        saknr LIKE zgl004_dre_est-saknr,
        kostl LIKE zgl004_dre_est-kostl,
        prctr LIKE zgl004_dre_est-prctr,
      END OF wa_zgl004,

      BEGIN OF wa_relatorio,
        bukrs LIKE bsis-bukrs,
        gsber LIKE bsis-gsber,
        hkont LIKE bsis-hkont,
        txt50 LIKE skat-txt50,
        kostl LIKE bsis-kostl,
        prctr LIKE bsis-prctr,
        ltext LIKE cepct-ltext,
        ersda LIKE csks-ersda,
        usnam LIKE csks-usnam,
        belnr LIKE bsis-belnr,
        budat LIKE bsis-budat,
        dmbtr LIKE bsis-dmbtr,
        dmbe2 LIKE bsis-dmbe2,
      END OF wa_relatorio.

TYPES: BEGIN OF ty_custo,
         empresa(8),
         filial(7),
         conta(11),
         desc_conta(51),
         obj_custo(11),
         desc_obj_custo(41),
         datacriacao(10),
         criadopor(13),
         nro_documento(14),
         datalcto(10),
         valorreal(16),
         valordolar(16),
      END OF ty_custo,

      BEGIN OF ty_lucro,
         empresa(8),
         filial(7),
         conta(11),
         desc_conta(51),
         obj_lucro(11),
         desc_obj_lucro(41),
         nro_documento(14),
         datalcto(10),
         valorreal(16),
         valordolar(16),
      END OF ty_lucro.

DATA: it_ska1 LIKE STANDARD TABLE OF wa_ska1,
      it_bsis LIKE STANDARD TABLE OF wa_bsis,
*Início Alteração Ricardo Furst.
      it_coas LIKE STANDARD TABLE OF wa_coas,
*Fim Alteração Ricardo Furst.
      it_skat LIKE STANDARD TABLE OF wa_skat,
      it_cskt LIKE STANDARD TABLE OF wa_cskt,
      it_csks LIKE STANDARD TABLE OF wa_csks,
      it_cepct LIKE STANDARD TABLE OF wa_cepct,
      it_zgl004 LIKE STANDARD TABLE OF wa_zgl004,
      it_relatorio LIKE STANDARD TABLE OF wa_relatorio,
      it_custo TYPE ty_custo OCCURS 0 WITH HEADER LINE,
      it_lucro TYPE ty_lucro OCCURS 0 WITH HEADER LINE.

DATA: wa_custo TYPE ty_custo,
      wa_lucro TYPE ty_lucro.

DATA: vl_dtini TYPE sy-datum,
      vl_dtfim TYPE sy-datum.


INITIALIZATION.

  p_ptch  = c_path.
  p_ptchb = c_pathback.
  p_arq   = 'arq_log_custo'.
  p_lucro = 'arq_log_lucro'.

START-OF-SELECTION.

  CLEAR: it_ska1.

  SELECT ktopl ktoks saknr "#EC CI_DB_OPERATION_OK[2389136]
    INTO CORRESPONDING FIELDS OF TABLE it_ska1 "#EC CI_DB_OPERATION_OK[2431747]
    FROM ska1
   WHERE ktopl EQ '0050'
     AND ktoks IN p_grupo.

  CONCATENATE p_gjahr p_monat '01' INTO vl_dtini.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = vl_dtini
    IMPORTING
      last_day_of_month = vl_dtfim.

  CLEAR: it_bsis, it_skat, it_cskt, it_zgl004, it_relatorio.

  PERFORM psq_centro_custo.

  PERFORM salva_xls USING p_arq 'C'.

  CLEAR: it_bsis, it_skat, it_cepct, it_zgl004, it_relatorio.

  PERFORM psq_centro_lucro.

  PERFORM salva_xls USING p_lucro 'L'.

*&---------------------------------------------------------------------*
*&      Form  PSQ_CENTRO_CUSTO
*&---------------------------------------------------------------------*
*       Relatório de centros de custo não parametrizados
*----------------------------------------------------------------------*
FORM psq_centro_custo .

  SELECT bukrs hkont kostl belnr gsber dmbtr dmbe2 shkzg budat aufnr
    INTO CORRESPONDING FIELDS OF TABLE it_bsis
    FROM bsis
    FOR ALL ENTRIES IN it_ska1
   WHERE bukrs IN p_bukrs
     AND hkont EQ it_ska1-saknr
     AND gjahr EQ p_gjahr
     AND budat BETWEEN vl_dtini AND vl_dtfim
     AND kostl NE ''.

*Início Alteração Ricardo Furst.
  IF NOT it_bsis IS INITIAL.
    SELECT *
      INTO TABLE it_coas
      FROM coas
      FOR ALL ENTRIES IN it_bsis
      WHERE aufnr EQ it_bsis-aufnr
        AND auart IN p_auart.

    IF sy-subrc EQ 0.

      DELETE it_coas WHERE aufnr = ''.

    ENDIF.

  ENDIF.
*Fim Alteração Ricardo Furst.

  SELECT bukrs saknr kostl
    INTO CORRESPONDING FIELDS OF TABLE it_zgl004
    FROM zgl004_dre_est
    FOR ALL ENTRIES IN it_bsis
   WHERE bukrs EQ it_bsis-bukrs
     AND kostl EQ it_bsis-kostl.

  DATA: vl_spras LIKE cskt-spras.

  CLEAR: vl_spras.

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      input  = 'PT'
    IMPORTING
      output = vl_spras.

  SELECT saknr txt50
    FROM skat
    INTO CORRESPONDING FIELDS OF TABLE it_skat
     FOR ALL ENTRIES IN it_bsis
   WHERE ktopl EQ '0050'
     AND saknr EQ it_bsis-hkont
     AND spras EQ vl_spras.

  SELECT kostl ltext
    FROM cskt
    INTO CORRESPONDING FIELDS OF TABLE it_cskt
     FOR ALL ENTRIES IN it_bsis
   WHERE kokrs EQ 'MAGI'
     AND kostl EQ it_bsis-kostl
     AND spras EQ vl_spras.

  SELECT kostl ersda usnam
    FROM csks
    INTO CORRESPONDING FIELDS OF TABLE it_csks
     FOR ALL ENTRIES IN it_cskt
   WHERE kokrs EQ 'MAGI'
     AND kostl EQ it_cskt-kostl.

  SORT: it_bsis   BY bukrs hkont kostl belnr,
        it_zgl004 BY bukrs saknr kostl,
        it_skat   BY saknr,
        it_cskt   BY kostl,
        it_csks   BY kostl.

  LOOP AT it_bsis INTO wa_bsis.

    READ TABLE it_zgl004 INTO wa_zgl004 WITH KEY bukrs = wa_bsis-bukrs
                                                 saknr = wa_bsis-hkont
                                                 kostl = wa_bsis-kostl
                                                 BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR: wa_relatorio.
      READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_bsis-hkont BINARY SEARCH.
      READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_bsis-kostl BINARY SEARCH.
      READ TABLE it_csks INTO wa_csks WITH KEY kostl = wa_bsis-kostl BINARY SEARCH.
      wa_relatorio-bukrs = wa_bsis-bukrs.
      wa_relatorio-gsber = wa_bsis-gsber.
      wa_relatorio-hkont = wa_bsis-hkont.
      wa_relatorio-txt50 = wa_skat-txt50.
      wa_relatorio-kostl = wa_bsis-kostl.
      wa_relatorio-ltext = wa_cskt-ltext.
      wa_relatorio-ersda = wa_csks-ersda.
      wa_relatorio-usnam = wa_csks-usnam.
      wa_relatorio-belnr = wa_bsis-belnr.
      wa_relatorio-budat = wa_bsis-budat.
      IF wa_bsis-shkzg EQ 'H'.
        wa_relatorio-dmbtr = wa_bsis-dmbtr * -1.
        wa_relatorio-dmbe2 = wa_bsis-dmbe2 * -1.
      ELSE.
        wa_relatorio-dmbtr = wa_bsis-dmbtr.
        wa_relatorio-dmbe2 = wa_bsis-dmbe2.
      ENDIF.
      APPEND wa_relatorio TO it_relatorio.
    ENDIF.

  ENDLOOP.

  CLEAR: it_custo[].
  DATA : vl_dmbtr(15),
         vl_dmbe2(15).

  IF NOT it_relatorio IS INITIAL.
    it_custo-empresa = 'Empresa'.
    it_custo-filial  = 'Filial'.
    it_custo-conta   = 'Conta'.
    it_custo-desc_conta = 'Descrição'.
    it_custo-obj_custo  = 'Centro Custo'.
    it_custo-desc_obj_custo = 'Descrição'.
    it_custo-datacriacao = 'Data Criação'.
    it_custo-criadopor   = 'Criado por'.
    it_custo-nro_documento  = 'Nro.Documento'.
    it_custo-datalcto   = 'Data Lcto'.
    it_custo-valorreal  = 'Valor R$'.
    it_custo-valordolar = 'Valor US$'.
    APPEND it_custo.
  ENDIF.

  LOOP AT it_relatorio INTO wa_relatorio.

    vl_dmbtr = wa_relatorio-dmbtr.
    vl_dmbe2 = wa_relatorio-dmbe2.

    it_custo-empresa = wa_relatorio-bukrs.
    it_custo-filial  = wa_relatorio-gsber.
    it_custo-conta   = wa_relatorio-hkont.
    it_custo-desc_conta = wa_relatorio-txt50.
    it_custo-obj_custo  = wa_relatorio-kostl.
    it_custo-desc_obj_custo = wa_relatorio-ltext.
    it_custo-datacriacao    = wa_relatorio-ersda.
    it_custo-criadopor      = wa_relatorio-usnam.
    it_custo-nro_documento  = wa_relatorio-belnr.
    it_custo-datalcto   = wa_relatorio-budat.
    it_custo-valorreal  = vl_dmbtr.
    it_custo-valordolar = vl_dmbe2.

    APPEND it_custo.

  ENDLOOP.


ENDFORM.                    " PSQ_CENTRO_CUSTO

*&---------------------------------------------------------------------*
*&      Form  PSQ_CENTRO_LUCRO
*&---------------------------------------------------------------------*
*       Relatório de centros de lucros não parametrizados
*----------------------------------------------------------------------*
FORM psq_centro_lucro .

  SELECT bukrs hkont prctr belnr gsber dmbtr dmbe2 shkzg budat
    INTO CORRESPONDING FIELDS OF TABLE it_bsis
    FROM bsis
    FOR ALL ENTRIES IN it_ska1
   WHERE bukrs IN p_bukrs
     AND hkont EQ it_ska1-saknr
     AND gjahr EQ p_gjahr
     AND budat BETWEEN vl_dtini AND vl_dtfim
     AND prctr NE ''.

  SELECT bukrs saknr prctr
    INTO CORRESPONDING FIELDS OF TABLE it_zgl004
    FROM zgl004_dre_est
    FOR ALL ENTRIES IN it_bsis
   WHERE bukrs EQ it_bsis-bukrs
     AND prctr EQ it_bsis-prctr.

  DATA: vl_spras LIKE cepct-spras.

  CLEAR: vl_spras.

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      input  = 'PT'
    IMPORTING
      output = vl_spras.

  SELECT saknr txt50
    FROM skat
    INTO CORRESPONDING FIELDS OF TABLE it_skat
     FOR ALL ENTRIES IN it_bsis
   WHERE ktopl EQ '0050'
     AND saknr EQ it_bsis-hkont
     AND spras EQ vl_spras.

  SELECT prctr ltext
    FROM cepct
    INTO CORRESPONDING FIELDS OF TABLE it_cepct
     FOR ALL ENTRIES IN it_bsis
   WHERE spras EQ vl_spras
     AND prctr EQ it_bsis-prctr.

  SORT: it_bsis   BY bukrs hkont prctr belnr,
        it_zgl004 BY bukrs saknr prctr,
        it_skat   BY saknr,
        it_cepct  BY prctr.

  LOOP AT it_bsis INTO wa_bsis.

    READ TABLE it_zgl004 INTO wa_zgl004 WITH KEY bukrs = wa_bsis-bukrs
                                                 saknr = wa_bsis-hkont
                                                 prctr = wa_bsis-prctr
                                                 BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR: wa_relatorio.
      READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_bsis-hkont BINARY SEARCH.
      READ TABLE it_cepct INTO wa_cepct WITH KEY prctr = wa_bsis-prctr BINARY SEARCH.
      wa_relatorio-bukrs = wa_bsis-bukrs.
      wa_relatorio-gsber = wa_bsis-gsber.
      wa_relatorio-hkont = wa_bsis-hkont.
      wa_relatorio-txt50 = wa_skat-txt50.
      wa_relatorio-prctr = wa_bsis-prctr.
      wa_relatorio-usnam = wa_csks-usnam.
      wa_relatorio-belnr = wa_bsis-belnr.
      wa_relatorio-budat = wa_bsis-budat.
      IF wa_bsis-shkzg EQ 'H'.
        wa_relatorio-dmbtr = wa_bsis-dmbtr * -1.
        wa_relatorio-dmbe2 = wa_bsis-dmbe2 * -1.
      ELSE.
        wa_relatorio-dmbtr = wa_bsis-dmbtr.
        wa_relatorio-dmbe2 = wa_bsis-dmbe2.
      ENDIF.
      APPEND wa_relatorio TO it_relatorio.
    ENDIF.

  ENDLOOP.

  CLEAR: it_lucro[].
  DATA : vl_dmbtr(15),
         vl_dmbe2(15).

  IF NOT it_relatorio IS INITIAL.
    it_lucro-empresa = 'Empresa'.
    it_lucro-filial  = 'Filial'.
    it_lucro-conta   = 'Conta'.
    it_lucro-desc_conta = 'Descrição'.
    it_lucro-obj_lucro  = 'Centro Lucro'.
    it_lucro-desc_obj_lucro = 'Descrição'.
    it_lucro-nro_documento  = 'Nro.Documento'.
    it_lucro-datalcto   = 'Data Lcto'.
    it_lucro-valorreal  = 'Valor R$'.
    it_lucro-valordolar = 'Valor US$'.
    APPEND it_lucro.
  ENDIF.

  LOOP AT it_relatorio INTO wa_relatorio.

    vl_dmbtr = wa_relatorio-dmbtr.
    vl_dmbe2 = wa_relatorio-dmbe2.

    it_lucro-empresa = wa_relatorio-bukrs.
    it_lucro-filial  = wa_relatorio-gsber.
    it_lucro-conta   = wa_relatorio-hkont.
    it_lucro-desc_conta = wa_relatorio-txt50.
    it_lucro-obj_lucro  = wa_relatorio-prctr.
    it_lucro-desc_obj_lucro = wa_relatorio-ltext.
    it_lucro-nro_documento  = wa_relatorio-belnr.
    it_lucro-datalcto   = wa_relatorio-budat.
    it_lucro-valorreal  = vl_dmbtr.
    it_lucro-valordolar = vl_dmbe2.

    APPEND it_lucro.
  ENDLOOP.

ENDFORM.                    " PSQ_CENTRO_LUCRO

*&---------------------------------------------------------------------*
*&      Form  SALVA_XLS
*&---------------------------------------------------------------------*
*       Salva Tabela Interna para arquivo XLS
*----------------------------------------------------------------------*
*      -->P_P_ARQ  Nome do Arquivo
*----------------------------------------------------------------------*
FORM salva_xls  USING p_p_arq p_tipo.

  DATA: v_arquivo LIKE rlgrap-filename,
        file TYPE c LENGTH 250.



  IF ( sy-batch EQ 'X' ).

    CONCATENATE p_ptchb p_p_arq '.txt' INTO v_arquivo.

    file = v_arquivo.

    OPEN DATASET v_arquivo IN TEXT MODE ENCODING DEFAULT FOR OUTPUT
                              IGNORING CONVERSION ERRORS
                              WITH SMART LINEFEED.
    IF p_tipo = 'C'.
      LOOP AT it_custo[] INTO wa_custo.
        TRANSFER wa_custo TO file.
      ENDLOOP.
    ELSEIF p_tipo = 'L'.
      LOOP AT it_lucro[] INTO wa_lucro.
        TRANSFER wa_lucro TO file.
      ENDLOOP.
    ENDIF.

    CLOSE DATASET file.

  ELSE.

    CONCATENATE p_ptch p_p_arq '.txt' INTO v_arquivo.

    IF p_tipo = 'C'.

      CALL FUNCTION 'WS_DOWNLOAD'
        EXPORTING
          filename                = v_arquivo
          filetype                = 'ASC'
        TABLES
          data_tab                = it_custo
        EXCEPTIONS
          file_open_error         = 1
          file_write_error        = 2
          invalid_filesize        = 3
          invalid_type            = 4
          no_batch                = 5
          unknown_error           = 6
          invalid_table_width     = 7
          gui_refuse_filetransfer = 8
          customer_error          = 9
          no_authority            = 10
          OTHERS                  = 11.

    ELSEIF p_tipo = 'L'.

      CALL FUNCTION 'WS_DOWNLOAD'
        EXPORTING
          filename                = v_arquivo
          filetype                = 'ASC'
        TABLES
          data_tab                = it_lucro
        EXCEPTIONS
          file_open_error         = 1
          file_write_error        = 2
          invalid_filesize        = 3
          invalid_type            = 4
          no_batch                = 5
          unknown_error           = 6
          invalid_table_width     = 7
          gui_refuse_filetransfer = 8
          customer_error          = 9
          no_authority            = 10
          OTHERS                  = 11.

    ENDIF.

  ENDIF.

ENDFORM. " SALVA_XLS
