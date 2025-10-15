*&---------------------------------------------------------------------*
*& Report  ZMMR130
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR130 MESSAGE-ID ZCARGA.

DATA: WA_ACTTAB   TYPE J_1BNFE_ACTIVE,
      LC_MIGO     TYPE REF TO ZCL_MIGO,
      I_CABECALHO TYPE ZDE_MIGO_CABECALHO,
      I_ITENS     TYPE ZDE_MIGO_ITENS_T,
      WA_ITENS    TYPE LINE OF ZDE_MIGO_ITENS_T,
      E_RETORNO	  TYPE BAPIRET2_T,
      MAT_DOC	    TYPE MBLNR,
      DOC_YEAR    TYPE MJAHR,
      E_J_1BNFDOC	TYPE J_1BNFDOC,
      LC_BKTXT    TYPE BKTXT,
      LC_XBLNR    TYPE XBLNR.

PARAMETERS: POBJKEY  TYPE ZMMT_EE_ZGR-OBJ_KEY  NO-DISPLAY,
            PQUANTI  TYPE ZDE_NM_PESO_DESCONTOS NO-DISPLAY,
            PESTORN  TYPE CHAR01 DEFAULT ABAP_FALSE  NO-DISPLAY,
            DESTORNO TYPE ZDE_DT_MOVIMENTO NO-DISPLAY,
            DTENTRAD TYPE SY-DATUM DEFAULT SY-DATLO NO-DISPLAY.

START-OF-SELECTION.

  CHECK POBJKEY IS NOT INITIAL.

  "Procura Registro de Entrada de Mercadoria
  SELECT SINGLE * INTO @DATA(WA_ZMMT_EE_ZGR)
    FROM ZMMT_EE_ZGR
   WHERE OBJ_KEY EQ @POBJKEY.

  CHECK SY-SUBRC IS INITIAL.

  "Procura Documentos Gerados
  SELECT SINGLE * INTO @DATA(WA_ZMMT_EE_ZGR_DOCS)
    FROM ZMMT_EE_ZGR_DOCS
   WHERE OBJ_KEY EQ @POBJKEY.

  CHECK SY-SUBRC IS INITIAL.

  CASE WA_ZMMT_EE_ZGR-TP_OPERACAO.
    WHEN '06' OR '07'.

      CHECK WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(WA_MKPF)
        FROM MKPF
       WHERE MJAHR EQ @WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR
         AND MBLNR EQ @WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR.

      CHECK SY-SUBRC IS INITIAL.

    WHEN OTHERS.

      CHECK WA_ZMMT_EE_ZGR_DOCS-DOCNUM IS NOT INITIAL.

      "Verifica se Documento Existe
      SELECT SINGLE * INTO @DATA(WA_J_1BNFDOC)
        FROM J_1BNFDOC
       WHERE DOCNUM EQ @WA_ZMMT_EE_ZGR_DOCS-DOCNUM.

      CHECK SY-SUBRC IS INITIAL.

  ENDCASE.

  CASE PESTORN.
    WHEN ABAP_FALSE.

      CASE WA_ZMMT_EE_ZGR-TP_OPERACAO.
        WHEN '06' OR '07'.

          SELECT SINGLE * INTO @DATA(WA_MSEG)
            FROM MSEG
           WHERE SMBLN EQ @WA_MKPF-MBLNR
             AND SJAHR EQ @WA_MKPF-MJAHR.

          CHECK SY-SUBRC IS NOT INITIAL.

          SELECT SINGLE * INTO @WA_MSEG
            FROM MSEG
           WHERE MJAHR EQ @WA_MKPF-MJAHR
             AND MBLNR EQ @WA_MKPF-MBLNR.

          CHECK SY-SUBRC IS INITIAL.

          DATA(LC_MATNR)  = WA_MSEG-MATNR.
          DATA(LC_BRANCH) = WA_MSEG-WERKS.
          DATA(LC_LGORT)  = WA_MSEG-LGORT.
          DATA(LC_CHARG)  = WA_MSEG-CHARG.
          DATA(LC_MEINS)  = WA_MSEG-MEINS.
          LC_BKTXT  = WA_MKPF-MBLNR && WA_MKPF-MJAHR.

        WHEN '11'.

          "Verifica se Não está cancelado
          CHECK WA_J_1BNFDOC-CANCEL EQ ABAP_FALSE.

          IF WA_J_1BNFDOC-FORM IS NOT INITIAL.

            "Verificar NF-e Autorizada
            CALL FUNCTION 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
              EXPORTING
                I_DOCNUM = WA_J_1BNFDOC-DOCNUM
              IMPORTING
                E_ACTTAB = WA_ACTTAB
              EXCEPTIONS
                NO_ENTRY = 1
                OTHERS   = 2.

            CHECK SY-SUBRC IS INITIAL.

            CHECK WA_ACTTAB-NFNUM9 IS NOT INITIAL.

            CHECK WA_ACTTAB-DOCSTA EQ '1'.

          ENDIF.

          "Busca Item da Nota de Entrada
          SELECT SINGLE * INTO @DATA(WA_J_1BNFLIN)
            FROM J_1BNFLIN
           WHERE DOCNUM EQ @WA_ZMMT_EE_ZGR_DOCS-DOCNUM.

          CHECK SY-SUBRC IS INITIAL.

          DATA: LC_MBLNR TYPE MBLNR,
                LC_MJAHR TYPE MJAHR,
                LC_ZEILE TYPE MBLPO.

          LC_MBLNR = WA_J_1BNFLIN-REFKEY(10).
          LC_MJAHR = WA_J_1BNFLIN-REFKEY+10(04).
          LC_ZEILE = WA_J_1BNFLIN-REFITM.

          SELECT SINGLE * INTO @WA_MSEG
            FROM MSEG
           WHERE MBLNR EQ @LC_MBLNR
             AND MJAHR EQ @LC_MJAHR
             AND ZEILE EQ @LC_ZEILE.

          CHECK SY-SUBRC IS INITIAL.

          LC_MATNR  = WA_MSEG-MATNR.
          LC_BRANCH = WA_J_1BNFDOC-BRANCH.
          LC_BKTXT  = WA_J_1BNFLIN-DOCNUM.
          LC_LGORT  = WA_MSEG-LGORT.
          LC_CHARG  = WA_MSEG-CHARG.
          LC_MEINS  = WA_MSEG-MEINS.

        WHEN OTHERS.

          "Verifica se Não está cancelado
          CHECK WA_J_1BNFDOC-CANCEL EQ ABAP_FALSE.

          IF WA_J_1BNFDOC-FORM IS NOT INITIAL.

            "Verificar NF-e Autorizada
            CALL FUNCTION 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
              EXPORTING
                I_DOCNUM = WA_J_1BNFDOC-DOCNUM
              IMPORTING
                E_ACTTAB = WA_ACTTAB
              EXCEPTIONS
                NO_ENTRY = 1
                OTHERS   = 2.

            CHECK SY-SUBRC IS INITIAL.

            CHECK WA_ACTTAB-NFNUM9 IS NOT INITIAL.

            CHECK WA_ACTTAB-DOCSTA EQ '1'.

          ENDIF.

          "Busca Item da Nota de Entrada
          SELECT SINGLE * INTO @WA_J_1BNFLIN
            FROM J_1BNFLIN
           WHERE DOCNUM EQ @WA_ZMMT_EE_ZGR_DOCS-DOCNUM.

          CHECK SY-SUBRC IS INITIAL.

          "Busca Item do Pedido de Compra
          SELECT SINGLE * INTO @DATA(WA_EKPO)
            FROM EKPO
           WHERE EBELN EQ @WA_J_1BNFLIN-XPED
             AND EBELP EQ @WA_J_1BNFLIN-NITEMPED.

          CHECK SY-SUBRC IS INITIAL.

          "Busca Safra do Pedido de Compra
          SELECT SINGLE * INTO @DATA(WA_EKET)
            FROM EKET
           WHERE EBELN EQ @WA_J_1BNFLIN-XPED
             AND EBELP EQ @WA_J_1BNFLIN-NITEMPED.

          CHECK SY-SUBRC IS INITIAL.

          LC_MATNR  = WA_J_1BNFLIN-MATNR.
          LC_BRANCH = WA_J_1BNFDOC-BRANCH.
          LC_BKTXT  = WA_J_1BNFLIN-DOCNUM.
          LC_LGORT  = WA_EKPO-LGORT.
          LC_CHARG  = WA_EKET-CHARG.
          LC_MEINS  = WA_J_1BNFLIN-MEINS.

      ENDCASE.

      SELECT SINGLE * INTO @DATA(NOTA_FISCAL_ETRADA)
        FROM ZSDT0001NT
       WHERE ID_CARGA EQ @WA_ZMMT_EE_ZGR-ID_CARGA
         AND ID_NOTA  EQ @WA_ZMMT_EE_ZGR-ID_NOTA.

      CHECK SY-SUBRC IS INITIAL.

      SELECT SINGLE * INTO @DATA(WA_ZSDT0001CG)
        FROM ZSDT0001CG
       WHERE ID_CARGA EQ @WA_ZMMT_EE_ZGR-ID_CARGA.

      CHECK SY-SUBRC IS INITIAL.

      "Verificar se é entrada FOB
      CHECK WA_ZSDT0001CG-TP_FRETE EQ ZIF_CARGA=>ST_TP_FRETE_FOB.

      DATA(QTD_SOBRA) = NOTA_FISCAL_ETRADA-NM_PESO_SUBTOTAL - NOTA_FISCAL_ETRADA-NM_PESO_LIQUIDO.

      IF ABS( QTD_SOBRA ) IS INITIAL.
        IF PQUANTI IS INITIAL.
          MESSAGE E217 WITH NOTA_FISCAL_ETRADA-NR_NOTA NOTA_FISCAL_ETRADA-NM_SERIE.
        ELSE.
          QTD_SOBRA = PQUANTI.
        ENDIF.
      ENDIF.

      SELECT SINGLE * INTO @DATA(WA_ZMMT0074)
        FROM ZMMT0074
       WHERE WERKS EQ @LC_BRANCH
         AND MATNR EQ @LC_MATNR
         AND ENTRADA_ROM EQ 'S'.

      CHECK SY-SUBRC IS INITIAL.

      I_CABECALHO-DATA_DOCUMENTO  = DTENTRAD.
      I_CABECALHO-DATA_LANCAMENTO = DTENTRAD.
      I_CABECALHO-DESCRICAO       = LC_BKTXT.
      I_CABECALHO-VER_GR_GI_SLIP  = '1'.
      I_CABECALHO-GOODSMVT_CODE   = '06'.
      I_CABECALHO-DOC_REFERENCIA  = WA_ZMMT_EE_ZGR-NT_REMESSA.

*      CASE WA_J_1BNFDOC-MODEL.
*        WHEN '55'.
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*            EXPORTING
*              INPUT  = WA_J_1BNFDOC-NFENUM
*            IMPORTING
*              OUTPUT = I_CABECALHO-DOC_REFERENCIA.
*        WHEN OTHERS.
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*            EXPORTING
*              INPUT  = WA_J_1BNFDOC-NFNUM
*            IMPORTING
*              OUTPUT = I_CABECALHO-DOC_REFERENCIA.
*      ENDCASE.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          INPUT  = WA_J_1BNFDOC-SERIES
*        IMPORTING
*          OUTPUT = WA_J_1BNFDOC-SERIES.
*
*      CONCATENATE I_CABECALHO-DOC_REFERENCIA WA_J_1BNFDOC-SERIES INTO I_CABECALHO-DOC_REFERENCIA SEPARATED BY '-'.


      I_ITENS =
              VALUE #( ( MATERIAL        = LC_MATNR
                         LOCAL_EXPEDICAO = LC_BRANCH
                         DEPOSITO        = COND STRING( WHEN WA_ZMMT_EE_ZGR-LGORT IS NOT INITIAL THEN WA_ZMMT_EE_ZGR-LGORT ELSE LC_LGORT )
                         LOTE            =  LC_CHARG(4)
                         TIPO_MOVIMENTO  = WA_ZMMT0074-BWART
                         PESO            = QTD_SOBRA
                         UNIDADE         = LC_MEINS ) ).

      CREATE OBJECT LC_MIGO.

      TRY .

          DATA(CK_CONTINUAR) = ABAP_TRUE.

          WHILE CK_CONTINUAR EQ ABAP_TRUE.

            DATA(LC_GEROU) =
            LC_MIGO->CRIAR(
              EXPORTING
                I_CABECALHO  = I_CABECALHO    " Estrutura de Cabeçalho do Movimento de Mercadoria
                I_ITENS      = I_ITENS        " Tabela de Itens do Movimento de Mercadoria
                I_BAPI_WAIT  = ABAP_FALSE     " Utilizar o comando 'COMMIT AND WAIT'
              IMPORTING
                E_RETORNO    = E_RETORNO      " Tabela de retorno
                MAT_DOC      = MAT_DOC        " Nº documento de material
                DOC_YEAR     = DOC_YEAR       " Ano do documento do material
                E_J_1BNFDOC  = E_J_1BNFDOC    " Cabeçalho da nota fiscal
            ).

            IF MAT_DOC IS INITIAL.
              DATA(_BLOQ) = ABAP_FALSE.
              DATA: WA_MSG TYPE BAPIRET2.
              PERFORM F_CHECK_MSG_BLOQ TABLES E_RETORNO CHANGING _BLOQ WA_MSG.
              IF _BLOQ EQ ABAP_FALSE.
                CK_CONTINUAR = ABAP_FALSE.
              ELSE.
                MESSAGE ID WA_MSG-ID TYPE 'S' NUMBER WA_MSG-NUMBER WITH WA_MSG-MESSAGE_V1 WA_MSG-MESSAGE_V2 WA_MSG-MESSAGE_V3 WA_MSG-MESSAGE_V4
                DISPLAY LIKE 'W'.
                WAIT UP TO 4 SECONDS.
                CLEAR: E_RETORNO[].
              ENDIF.
            ELSE.
              CK_CONTINUAR = ABAP_FALSE.
            ENDIF.

          ENDWHILE.

          IF LC_GEROU EQ ABAP_TRUE.
            WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR_SOBRA = MAT_DOC.
            WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR_SOBRA = DOC_YEAR.
            MODIFY ZMMT_EE_ZGR_DOCS FROM WA_ZMMT_EE_ZGR_DOCS.
            COMMIT WORK.
          ENDIF.

        CATCH ZCX_MIGO_EXCEPTION.    "
        CATCH ZCX_PEDIDO_COMPRA_EXCEPTION.    "
      ENDTRY.

      CLEAR: LC_MIGO.

    WHEN ABAP_TRUE.

      SELECT * INTO TABLE @DATA(IT_MSEG)
        FROM MSEG
       WHERE MBLNR EQ @WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR_SOBRA
         AND MJAHR EQ @WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR_SOBRA.

      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_MSEG INDEX 1 INTO WA_MSEG.
      ENDIF.

*      IF WA_MSEG-BWART EQ 'ZX1'.
*
*        SELECT SINGLE * INTO @DATA(WA_MKPF)
*          FROM MKPF
*         WHERE MBLNR EQ @WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR_SOBRA
*           AND MJAHR EQ @WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR_SOBRA.
*
*        IF DESTORNO IS INITIAL.
*          I_CABECALHO-DATA_DOCUMENTO  = SY-DATLO.
*          I_CABECALHO-DATA_LANCAMENTO = SY-DATLO.
*        ELSE.
*          I_CABECALHO-DATA_DOCUMENTO  = DESTORNO.
*          I_CABECALHO-DATA_LANCAMENTO = DESTORNO.
*        ENDIF.
*        I_CABECALHO-DESCRICAO       = WA_MKPF-BKTXT.
*        I_CABECALHO-VER_GR_GI_SLIP  = '1'.
*        I_CABECALHO-GOODSMVT_CODE   = '05'.
*        I_CABECALHO-DOC_REFERENCIA  = WA_MKPF-XBLNR.
*
*        LOOP AT IT_MSEG INTO WA_MSEG.
*          CLEAR: WA_ITENS.
*          WA_ITENS-MATERIAL        = WA_MSEG-MATNR.
*          WA_ITENS-LOCAL_EXPEDICAO = WA_J_1BNFDOC-BRANCH.
*          WA_ITENS-DEPOSITO        = COND STRING( WHEN WA_MSEG-LGORT IS NOT INITIAL THEN WA_MSEG-LGORT ELSE WA_EKPO-LGORT  ).
*          WA_ITENS-LOTE            = WA_MSEG-CHARG.
*          WA_ITENS-TIPO_MOVIMENTO  = 'ZX5'.
*          WA_ITENS-PESO            = WA_MSEG-MENGE.
*          WA_ITENS-UNIDADE         = WA_MSEG-MEINS.
*          APPEND WA_ITENS TO I_ITENS.
*        ENDLOOP.
*
*        CREATE OBJECT LC_MIGO.
*
*        TRY .
*            LC_GEROU =
*            LC_MIGO->CRIAR(
*              EXPORTING
*                I_CABECALHO  = I_CABECALHO    " Estrutura de Cabeçalho do Movimento de Mercadoria
*                I_ITENS      = I_ITENS        " Tabela de Itens do Movimento de Mercadoria
*                I_BAPI_WAIT  = ABAP_FALSE     " Utilizar o comando 'COMMIT AND WAIT'
*              IMPORTING
*                E_RETORNO    = E_RETORNO      " Tabela de retorno
*                MAT_DOC      = MAT_DOC        " Nº documento de material
*                DOC_YEAR     = DOC_YEAR       " Ano do documento do material
*                E_J_1BNFDOC  = E_J_1BNFDOC    " Cabeçalho da nota fiscal
*            ).
*
*            IF LC_GEROU EQ ABAP_TRUE.
*              CLEAR:
*              WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR_SOBRA,
*              WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR_SOBRA.
*              MODIFY ZMMT_EE_ZGR_DOCS FROM WA_ZMMT_EE_ZGR_DOCS.
*              COMMIT WORK.
*            ENDIF.
*
*          CATCH ZCX_MIGO_EXCEPTION.    "
*          CATCH ZCX_PEDIDO_COMPRA_EXCEPTION.    "
*        ENDTRY.
*
*        CLEAR: LC_MIGO.
*
*      ELSE.

      IF DESTORNO IS INITIAL.
        DATA(LC_DESTORNO) = SY-DATLO.
      ELSE.
        LC_DESTORNO = DESTORNO.
      ENDIF.

      CREATE OBJECT LC_MIGO.
      TRY .
          DATA(LC_ESTORNOU) =
          LC_MIGO->ESTORNAR(
            EXPORTING
              I_BAPI_WAIT = ABAP_FALSE    " Utilizar o comando 'COMMIT AND WAIT'
            CHANGING
              I_DOC_YEAR  = WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR_SOBRA    " Ano do documento do material
              I_MAT_DOC   = WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR_SOBRA    " Nº documento de material
              I_BUDAT     =	LC_DESTORNO ).

          CLEAR: LC_MIGO.
          IF LC_ESTORNOU EQ ABAP_TRUE.
            CLEAR:
            WA_ZMMT_EE_ZGR_DOCS-MM_MBLNR_SOBRA,
            WA_ZMMT_EE_ZGR_DOCS-MM_MJAHR_SOBRA.
            MODIFY ZMMT_EE_ZGR_DOCS FROM WA_ZMMT_EE_ZGR_DOCS.
            COMMIT WORK.
          ENDIF.
        CATCH ZCX_MIGO_EXCEPTION.
      ENDTRY.
      CLEAR: LC_MIGO.
*      ENDIF.

  ENDCASE.


FORM F_CHECK_MSG_BLOQ TABLES P_RETURN STRUCTURE BAPIRET2
                    CHANGING P_BLOQ   TYPE C
                             P_MSG TYPE BAPIRET2.

  DATA: LC_WAIT_INT_M3_897 TYPE I.

  P_BLOQ = ABAP_FALSE.

  READ TABLE P_RETURN INTO P_MSG WITH KEY TYPE = 'E' ID = 'ME' NUMBER = '006'.
  IF SY-SUBRC EQ 0.
    P_BLOQ = ABAP_TRUE.
    EXIT.
  ENDIF.

  READ TABLE P_RETURN INTO P_MSG WITH KEY TYPE = 'E' ID = 'M3' NUMBER = '024'.
  IF SY-SUBRC EQ 0.
    P_BLOQ = ABAP_TRUE.
    EXIT.
  ENDIF.

  READ TABLE P_RETURN INTO P_MSG WITH KEY TYPE = 'E' ID = 'M3' NUMBER = '682'.
  IF SY-SUBRC EQ 0.
    P_BLOQ = ABAP_TRUE.
    EXIT.
  ENDIF.

  READ TABLE P_RETURN INTO P_MSG WITH KEY TYPE = 'E' ID = 'M3' NUMBER = '897'.
  IF SY-SUBRC EQ 0.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_SET_INTERVAL_M3_897)
     WHERE SETNAME = 'ENT_EST_INTERVAL_M3_897'.

    IF ( SY-SUBRC EQ 0 ) AND ( _WL_SET_INTERVAL_M3_897-VALFROM > 0 ).
      LC_WAIT_INT_M3_897 = _WL_SET_INTERVAL_M3_897-VALFROM.
      WAIT UP TO LC_WAIT_INT_M3_897 SECONDS.
    ENDIF.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_SET_EXIT_M3_897)
     WHERE SETNAME = 'ENT_EST_EXIT_M3_897'
       AND VALFROM = 'X'.

    IF SY-SUBRC NE 0.
      P_BLOQ = ABAP_TRUE.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.
