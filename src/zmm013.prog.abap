************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 30.06.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Geração de Documento originados do SIGAM            *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 30.06.2009    Marcus Barbara       Criação              DEVK906049   *
************************************************************************

REPORT  zmm013.

*----------------------------------------------------------------------*
* WORK AREAS                                                           *
*----------------------------------------------------------------------*
DATA: BEGIN OF wa_ekbe,
        belnr LIKE ekbe-belnr,
        werks LIKE ekbe-werks,
        ebeln LIKE ekbe-ebeln,
        ebelp LIKE ekbe-ebelp,
        menge LIKE ekbe-menge,
        matnr LIKE ekbe-matnr,
      END OF wa_ekbe,

      BEGIN OF wa_ekes,
        ebeln LIKE ekes-ebeln,
        ebelp LIKE ekes-ebelp,
        vbeln LIKE ekes-vbeln,
        vbelp LIKE ekes-vbelp,
      END OF wa_ekes,

      BEGIN OF wa_lips,
        vbeln LIKE lips-vbeln,
        posnr LIKE lips-posnr,
        lichn LIKE lips-lichn,
      END OF wa_lips,

      BEGIN OF wa_zimp_etiqueta,
        belnr           LIKE zimp_etiqueta_da-belnr,
        werks           LIKE zimp_etiqueta_da-werks,
        matnr           LIKE zimp_etiqueta_da-matnr,
        ebeln           LIKE zimp_etiqueta_da-ebeln,
        ebelp           LIKE zimp_etiqueta_da-ebelp,
        lichn           LIKE zimp_etiqueta_da-lichn,
        status_impr     LIKE zimp_etiqueta_da-status_impr,
        dt_impr         LIKE zimp_etiqueta_da-dt_impr,
        hr_impr         LIKE zimp_etiqueta_da-hr_impr,
        usuario_impr    LIKE zimp_etiqueta_da-usuario_impr,
        dt_reimpressao  LIKE zimp_etiqueta_da-dt_reimpressao,
        hr_reimpressao  LIKE zimp_etiqueta_da-hr_reimpressao,
        usuario_reimpre LIKE zimp_etiqueta_da-usuario_reimpre,
        saldo_ant       LIKE zimp_etiqueta_da-saldo_ant,
        menge           LIKE zimp_etiqueta_da-menge,
        saldo_atual     LIKE zimp_etiqueta_da-saldo_atual,
      END OF wa_zimp_etiqueta.

*----------------------------------------------------------------------*
* TABELAS INTERNAS                                                     *
*----------------------------------------------------------------------*
DATA: it_ekbe LIKE STANDARD TABLE OF wa_ekbe,
      it_ekes LIKE STANDARD TABLE OF wa_ekes,
      it_lips LIKE STANDARD TABLE OF wa_lips,
      it_zimp LIKE STANDARD TABLE OF wa_zimp_etiqueta,
      it_impressao LIKE STANDARD TABLE OF wa_zimp_etiqueta,
      zimp_etiqueta TYPE zimp_etiqueta_da.

*----------------------------------------------------------------------*
* VARIAVEIS                                                            *
*----------------------------------------------------------------------*
DATA: xsdant LIKE zimp_etiqueta_da-saldo_atual.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO                                                      *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-tb1.
PARAMETERS: p_werks  LIKE t001w-werks OBLIGATORY,
            p_matnr  LIKE ekbe-matnr OBLIGATORY,
            p_belnr  LIKE ekbe-belnr OBLIGATORY,
            p_qtde   TYPE i DEFAULT 0 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-tb2.
PARAMETERS: r_impr      RADIOBUTTON GROUP tp DEFAULT 'X',
            r_reim      RADIOBUTTON GROUP tp .
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.
  CLEAR: it_ekbe, it_impressao.

  SELECT belnr werks ebeln ebelp menge matnr
    INTO CORRESPONDING FIELDS OF TABLE it_ekbe
    FROM ekbe
   WHERE belnr EQ p_belnr
     AND werks EQ p_werks
     AND matnr EQ p_matnr.

  IF sy-subrc NE 0.
    MESSAGE 'MIGO não encontrada, analisar o pedido de compras.' TYPE 'I'.
  ELSEIF sy-subrc EQ 0.
    SORT it_ekes BY vbeln vbelp.
    CLEAR: it_ekes.

    SELECT ebeln ebelp vbeln vbelp
      INTO CORRESPONDING FIELDS OF TABLE it_ekes
      FROM ekes
      FOR ALL ENTRIES IN it_ekbe
     WHERE ebeln EQ it_ekbe-ebeln
       AND ebelp EQ it_ekbe-ebelp.

    IF sy-subrc EQ 0.
      SORT it_ekes BY vbeln vbelp.
      CLEAR: it_lips.

      SELECT vbeln posnr lichn
        INTO CORRESPONDING FIELDS OF TABLE it_lips
        FROM lips
        FOR ALL ENTRIES IN it_ekes
       WHERE vbeln EQ it_ekes-vbeln
         AND posnr EQ it_ekes-vbelp.

      SORT it_lips BY lichn.

* Se opção for IMPRESSÃO
      IF r_impr EQ 'X'.
        CLEAR: it_zimp.

        SELECT belnr werks matnr ebeln ebelp lichn status_impr
               dt_impr hr_impr usuario_impr dt_reimpressao
               hr_reimpressao usuario_reimpre saldo_ant menge
               saldo_atual
          INTO CORRESPONDING FIELDS OF TABLE it_zimp
          FROM zimp_etiqueta_da
         WHERE werks EQ p_werks
           AND matnr EQ p_matnr
           AND belnr EQ p_belnr
           AND status_impr NE ''.

        IF sy-subrc EQ 0.
          MESSAGE 'Estes dados para etiqueta já foram impressos!' TYPE 'I'.
        ELSEIF sy-subrc NE 0.
          CLEAR: it_zimp.

          SELECT belnr werks matnr ebeln ebelp lichn status_impr
                 dt_impr hr_impr usuario_impr dt_reimpressao
                 hr_reimpressao usuario_reimpre saldo_ant menge
                 saldo_atual
            INTO CORRESPONDING FIELDS OF TABLE it_zimp
            FROM zimp_etiqueta_da
           WHERE matnr EQ p_matnr
             AND werks EQ p_werks.

          SORT it_zimp BY dt_impr hr_impr.

          xsdant = 0.
          LOOP AT it_zimp INTO wa_zimp_etiqueta.
            xsdant = wa_zimp_etiqueta-saldo_atual.
          ENDLOOP.

          LOOP AT it_ekbe INTO wa_ekbe.
            zimp_etiqueta-belnr = wa_ekbe-belnr.
            zimp_etiqueta-werks = wa_ekbe-werks.
            zimp_etiqueta-matnr = wa_ekbe-matnr.
            zimp_etiqueta-ebeln = wa_ekbe-ebeln.
            zimp_etiqueta-ebelp = wa_ekbe-ebelp.
            zimp_etiqueta-status_impr = 'I'.
            zimp_etiqueta-dt_impr = sy-datum.
            zimp_etiqueta-hr_impr = sy-uzeit.
            zimp_etiqueta-usuario_impr = sy-uname.
            READ TABLE it_ekes INTO wa_ekes WITH KEY ebeln = wa_ekbe-ebeln
                                                     ebelp = wa_ekbe-ebelp
                                                     BINARY SEARCH.

            READ TABLE it_lips INTO wa_lips WITH KEY vbeln = wa_ekes-vbeln
                                                     posnr = wa_ekes-vbelp
                                                     BINARY SEARCH.
            zimp_etiqueta-lichn       = wa_lips-lichn.
            zimp_etiqueta-saldo_ant   = xsdant.
            zimp_etiqueta-menge       = p_qtde.
            zimp_etiqueta-saldo_atual = zimp_etiqueta-saldo_ant + zimp_etiqueta-menge.

            CLEAR: wa_zimp_etiqueta.
            MOVE-CORRESPONDING zimp_etiqueta TO wa_zimp_etiqueta.
            APPEND wa_zimp_etiqueta TO it_impressao.

            INSERT into zimp_etiqueta_da values zimp_etiqueta.

          ENDLOOP.
        ENDIF.

* Se opção for REIMPRESSÃO
      ELSEIF  r_impr NE 'X'.

        CLEAR: it_zimp.

        SELECT belnr werks matnr ebeln ebelp lichn status_impr
               dt_impr hr_impr usuario_impr dt_reimpressao
               hr_reimpressao usuario_reimpre saldo_ant menge
               saldo_atual
          INTO CORRESPONDING FIELDS OF TABLE it_zimp
          FROM zimp_etiqueta_da
         WHERE werks EQ p_werks
           AND matnr EQ p_matnr
           AND belnr EQ p_belnr
           AND status_impr EQ ''.

        IF sy-subrc EQ 0.
          MESSAGE 'Estes dados para etiqueta ainda não foram impressos!' TYPE 'I'.
        ELSEIF sy-subrc NE 0.
          CLEAR: it_zimp.
          SELECT belnr werks matnr ebeln ebelp lichn status_impr
                 dt_impr hr_impr usuario_impr dt_reimpressao
                 hr_reimpressao usuario_reimpre saldo_ant menge
                 saldo_atual
            INTO CORRESPONDING FIELDS OF TABLE it_zimp
            FROM zimp_etiqueta_da
           WHERE matnr EQ p_matnr
             AND werks EQ p_werks.

          SORT it_zimp BY dt_impr hr_impr.

          xsdant = 0.
          LOOP AT it_zimp INTO wa_zimp_etiqueta.
            xsdant = wa_zimp_etiqueta-saldo_atual.
          ENDLOOP.

          LOOP AT it_ekbe INTO wa_ekbe.

            SELECT belnr werks matnr ebeln ebelp lichn status_impr
                   dt_impr hr_impr usuario_impr dt_reimpressao
                   hr_reimpressao usuario_reimpre saldo_ant menge
                   saldo_atual
              INTO TABLE it_impressao
              FROM zimp_etiqueta_da
             WHERE belnr = wa_ekbe-belnr
               AND werks = wa_ekbe-werks
               AND matnr = wa_ekbe-matnr
               AND ebeln = wa_ekbe-ebeln
               AND ebelp = wa_ekbe-ebelp.

            UPDATE zimp_etiqueta_da
               SET status_impr = 'R'
                   dt_reimpressao = sy-datum
                   hr_reimpressao = sy-uzeit
                   usuario_reimpre = sy-uname
             WHERE belnr = wa_ekbe-belnr
               AND werks = wa_ekbe-werks
               AND matnr = wa_ekbe-matnr
               AND ebeln = wa_ekbe-ebeln
               AND ebelp = wa_ekbe-ebelp.

          ENDLOOP.

        ENDIF.

      ENDIF.

      IF NOT it_impressao IS INITIAL.
        PERFORM emissao_etiqueta.
      ENDIF.

    ENDIF.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  EMISSAO_ETIQUETA
*&---------------------------------------------------------------------*
*       Impressão de etiqueta
*----------------------------------------------------------------------*
*      -->P_IT_IMPRESSAO
*----------------------------------------------------------------------*
FORM emissao_etiqueta.

  DATA: BEGIN OF wa_emissao,
          werks  LIKE zimp_etiqueta_da-werks,
          name1  LIKE t001w-name1,
          matnr  LIKE zimp_etiqueta_da-matnr,
          xcont  TYPE i,
          maktx  LIKE makt-maktx,
          lichn  LIKE lips-lichn,
          belnr  LIKE ekbe-belnr,
        END OF wa_emissao,

        BEGIN OF wa_makt,
          matnr LIKE makt-matnr,
          maktx LIKE makt-maktx,
        END OF wa_makt,

        BEGIN OF wa_linhas,
          linha TYPE c LENGTH 150,
        END OF wa_linhas,

        BEGIN OF wa_t001w,
          werks LIKE t001w-werks,
          name1 LIKE t001w-name1,
        END OF wa_t001w.

  DATA: it_emissao LIKE STANDARD TABLE OF wa_emissao,
        it_makt    LIKE STANDARD TABLE OF wa_makt,
        it_linhas  LIKE STANDARD TABLE OF wa_linhas,
        it_t001w   LIKE STANDARD TABLE OF wa_t001w.

  DATA: va_spras LIKE makt-spras,
        contador TYPE i,
        xcont    TYPE c LENGTH 6.

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      input  = 'PT'
    IMPORTING
      output = va_spras.

  SELECT werks name1
    FROM t001w
    INTO TABLE it_t001w
     FOR ALL ENTRIES IN it_impressao
   WHERE werks EQ it_impressao-werks.

  SELECT matnr maktx
    INTO TABLE it_makt
    FROM makt
    FOR ALL ENTRIES IN it_impressao
   WHERE matnr EQ it_impressao-matnr
     AND spras EQ va_spras.

  SORT: it_makt BY matnr,
        it_t001w BY werks.

  CLEAR: it_emissao.

  LOOP AT it_impressao INTO wa_zimp_etiqueta.
    CLEAR: wa_emissao.
    wa_emissao-werks  = wa_zimp_etiqueta-werks.
    READ TABLE it_t001w INTO wa_t001w
    WITH KEY werks = wa_zimp_etiqueta-werks BINARY SEARCH.
    wa_emissao-name1  = wa_t001w-name1.
    wa_emissao-matnr  = wa_zimp_etiqueta-matnr.
    wa_emissao-lichn  = wa_zimp_etiqueta-lichn.
    wa_emissao-belnr  = wa_zimp_etiqueta-belnr.
    READ TABLE it_makt INTO wa_makt
    WITH KEY matnr = wa_zimp_etiqueta-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_emissao-maktx  = wa_makt-maktx.
    ENDIF.

    contador = 1.

    WHILE contador LE wa_zimp_etiqueta-menge.
      wa_emissao-xcont  = wa_zimp_etiqueta-saldo_ant + contador.
      APPEND wa_emissao TO it_emissao.
      contador = contador + 1.
    ENDWHILE.

  ENDLOOP.

  CLEAR: it_linhas.


  LOOP AT it_emissao INTO wa_emissao.

    CLEAR: wa_linhas.

    PERFORM padl USING wa_emissao-xcont xcont 6.

    wa_linhas-linha = 'N'.
    APPEND wa_linhas TO it_linhas.
    wa_linhas-linha = 'O'.
    APPEND wa_linhas TO it_linhas.
    wa_linhas-linha = 'S2'.
    APPEND wa_linhas TO it_linhas.
    wa_linhas-linha = 'Q614,27'.
    APPEND wa_linhas TO it_linhas.
    wa_linhas-linha = 'q784'.
    APPEND wa_linhas TO it_linhas.

    CONCATENATE 'B50,40,0,3,2,7,150,B,"' wa_emissao-werks wa_emissao-matnr+10(8) xcont
                '"' INTO wa_linhas-linha.
    APPEND wa_linhas TO it_linhas.

    CONCATENATE 'A50,280,0,3,1,1,N,"Centro: ' wa_emissao-werks '-' wa_emissao-name1 '"' INTO wa_linhas-linha.
    APPEND wa_linhas TO it_linhas.

    CONCATENATE 'A50,320,0,3,1,1,N,"Material: ' wa_emissao-matnr+10(8) '-' wa_emissao-maktx(32) '"' INTO wa_linhas-linha.
    APPEND wa_linhas TO it_linhas.

    CONCATENATE 'A50,360,0,3,1,1,N,"Lote Fornecedor: ' wa_emissao-lichn '"' INTO wa_linhas-linha.
    APPEND wa_linhas TO it_linhas.

    CONCATENATE 'A50,400,0,3,1,1,N,"Nro.Recebimento: ' wa_emissao-belnr '"' INTO wa_linhas-linha.
    APPEND wa_linhas TO it_linhas.

    wa_linhas-linha = 'P1'.
    APPEND wa_linhas TO it_linhas.

  ENDLOOP.

  IF NOT it_linhas IS INITIAL.
    CALL FUNCTION 'WS_DOWNLOAD'
      EXPORTING
        filename = 'LPT1:'
        filetype = 'ASC'
      TABLES
        data_tab = it_linhas
      EXCEPTIONS
        OTHERS   = 4.
  ENDIF.

ENDFORM.                    " EMISSAO_ETIQUETA

*&---------------------------------------------------------------------*
*&      Form  PADL
*&---------------------------------------------------------------------*
*       Padroniza string
*----------------------------------------------------------------------*
*      -->P_XCONT  text
*      -->qtd      text
*----------------------------------------------------------------------*
FORM padl  USING v_xcont p_xcont qtd.

  DATA: qtd_xcont TYPE i.

  p_xcont = v_xcont.
  SHIFT p_xcont LEFT DELETING LEADING ' '.

  CALL FUNCTION 'STRING_LENGTH'
    EXPORTING
      string = p_xcont
    IMPORTING
      length = qtd_xcont.

  qtd_xcont = qtd - qtd_xcont.

  DO qtd_xcont TIMES.
    CONCATENATE '0' p_xcont INTO p_xcont.
  ENDDO.

ENDFORM.                    " PADL
