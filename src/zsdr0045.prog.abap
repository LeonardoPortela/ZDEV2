*&---------------------------------------------------------------------*
*& Report  ZSDR0045
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZSDR0045
NO STANDARD PAGE HEADING    "Não exibe cabeçalho standard
LINE-SIZE 076               "Comprimento da Linha
LINE-COUNT 65.              "Número de Linhas

*&--------------------------------------------------------------------&*
*&  ESTRUTURA
*&--------------------------------------------------------------------&*
DATA: WA_RETURN LIKE ZMME_RETURN_ORDEM_SERVICO,
      IT_RETURN LIKE STANDARD TABLE OF WA_RETURN.

*&---------------------------------------------------------------------*
*& Tipos
*&---------------------------------------------------------------------*
"TYPES:.
TYPES: BEGIN OF TY_OV_ALTERADA ,
         NRO_SOL_OV	TYPE ZSDT0051-NRO_SOL_OV,
       END OF TY_OV_ALTERADA .

*&---------------------------------------------------------------------*
*& Tabelas Internas
*&---------------------------------------------------------------------*
DATA : IT_ZSDT0051    TYPE TABLE OF ZSDT0051 WITH HEADER LINE,
       IT_ZSDT0059    TYPE TABLE OF ZSDT0059 WITH HEADER LINE,
       IT_ZSDT0059_PR TYPE TABLE OF ZSDT0059 WITH HEADER LINE,
       IT_ZSDT0059_SP TYPE TABLE OF ZSDT0059 WITH HEADER LINE,
       IT_ZSDT0053    TYPE TABLE OF ZSDT0053 WITH HEADER LINE,
       IT_ZSDT0097    TYPE TABLE OF ZSDT0097 WITH HEADER LINE,
       IT_VBFA        TYPE TABLE OF VBFA     WITH HEADER LINE,
       "IT_VBAK        TYPE TABLE OF VBAK     WITH HEADER LINE,
       IT_SAIDA       TYPE TABLE OF ZSD_PNL_PREMIO WITH HEADER LINE,
       IT_ZSDT0094    TYPE TABLE OF ZSDT0094 WITH HEADER LINE,
       WA_ZSDT0094    TYPE ZSDT0094.
"IT_OV_ALTERADA TYPE TABLE OF TY_OV_ALTERADA WITH HEADER LINE,

*&---------------------------------------------------------------------*
*& WORKAREAS
*&---------------------------------------------------------------------*
DATA : WA_SAIDA       TYPE ZSD_PNL_PREMIO,
       WA_ZSDT0059    TYPE ZSDT0059,
       WA_ZSDT0059_PR TYPE ZSDT0059,
       WA_ZSDT0059_SP TYPE ZSDT0059,
       WA_OV_ALTERADA TYPE TY_OV_ALTERADA,
       WA_ZSDT0051    TYPE ZSDT0051,
       WA_ZSDT0053    TYPE ZSDT0053,
       WA_ZSDT0097    TYPE ZSDT0097,
       WA_VBFA        TYPE VBFA,
       WA_VBAK        TYPE VBAK,
       VL_UKURS       LIKE TCURR-UKURS,
       LC_MOEDA       LIKE TCURR-FCURR.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF SY-BATCH EQ ABAP_TRUE.
    TRY .
        ZCL_JOB=>GET_CK_PROGRAM_EXECUCAO( EXPORTING I_NOME_PROGRAM = SY-CPROG IMPORTING E_QTD = DATA(E_QTD) ).
      CATCH ZCX_JOB.
        E_QTD = 1.
    ENDTRY.

    IF E_QTD GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.


  PERFORM SELECIONAR_DADOS.
  PERFORM ORGANIZACAO_DADOS.
  PERFORM SELECIONAR_DADOS_SPREAD.
  PERFORM SELECIONAR_DADOS_EXCLUIDOS.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .
  REFRESH : IT_ZSDT0051, IT_ZSDT0053, IT_ZSDT0059.

  DATA: VL_DATA_INI  TYPE ZSDT0059-DATA_ATUAL,
        VL_DATA_FIM  TYPE ZSDT0059-DATA_ATUAL,
        VL_NR_SOL_OV TYPE ZSDT0051-NRO_SOL_OV.

  VL_DATA_INI = SY-DATUM.
  VL_DATA_FIM = SY-DATUM.

  CLEAR VL_NR_SOL_OV.

********COMPRAS ALTERADAS NO DIA - Alterado
*  SELECT *
*    INTO TABLE IT_ZSDT0059  "IT_ZSDT0059_AUX
*    FROM ZSDT0059
*   WHERE DATA_ATUAL >= VL_DATA_INI
*     AND DATA_ATUAL <= VL_DATA_FIM.
*
*  LOOP AT IT_ZSDT0059 INTO WA_ZSDT0059.
*    WA_OV_ALTERADA-NRO_SOL_OV  = WA_ZSDT0059-NRO_SOL_OV.
*    APPEND  WA_OV_ALTERADA TO IT_OV_ALTERADA .
*  ENDLOOP.
*
*  SELECT *
*    INTO TABLE IT_ZSDT0053
*    FROM ZSDT0053
*   WHERE DATA_ATUAL >= VL_DATA_INI
*     AND DATA_ATUAL <= VL_DATA_FIM.
*
*  LOOP AT IT_ZSDT0053 INTO WA_ZSDT0053.
*    WA_OV_ALTERADA-NRO_SOL_OV  = WA_ZSDT0053-NRO_SOL_OV.
*    APPEND  WA_OV_ALTERADA TO IT_OV_ALTERADA .
*  ENDLOOP.
*
********COMPRAS ALTERADAS NO DIA
*
*  IF IT_OV_ALTERADA[] IS NOT INITIAL.
*    SELECT *
*      INTO TABLE IT_ZSDT0051
*      FROM ZSDT0051
*       FOR ALL ENTRIES IN IT_OV_ALTERADA
*     WHERE NRO_SOL_OV = IT_OV_ALTERADA-NRO_SOL_OV
*       AND VTWEG      = '10' "Canal de distribuição = mercado interno
*       AND SPART      = '01'
*       AND STATUS     = 'L'."Setor de atividade = trading
*
*    SELECT *
*      INTO TABLE IT_ZSDT0053
*      FROM ZSDT0053
*       FOR ALL ENTRIES IN IT_OV_ALTERADA
*     WHERE NRO_SOL_OV = IT_OV_ALTERADA-NRO_SOL_OV .
*
*    SELECT *
*      INTO TABLE IT_ZSDT0059
*      FROM ZSDT0059
*       FOR ALL ENTRIES IN IT_OV_ALTERADA
*     WHERE NRO_SOL_OV = IT_OV_ALTERADA-NRO_SOL_OV .
*
*  ENDIF.
  SELECT *
    INTO TABLE IT_ZSDT0051
    FROM ZSDT0051
   WHERE VTWEG      = '10' "Canal de distribuição = mercado interno
     AND SPART      = '01'."Setor de atividade = trading

  IF 1 = 2 ."Caso precise debugar uma sv especifica.
    SELECT *
      INTO TABLE IT_ZSDT0051
      FROM ZSDT0051
     WHERE VTWEG      = '10' "Canal de distribuição = mercado interno
       AND SPART      = '01'"Setor de atividade = trading
       AND NRO_SOL_OV = VL_NR_SOL_OV.
  ENDIF.

  SELECT *
    INTO TABLE IT_ZSDT0053
    FROM ZSDT0053
     FOR ALL ENTRIES IN IT_ZSDT0051
   WHERE NRO_SOL_OV = IT_ZSDT0051-NRO_SOL_OV .

  SELECT *
    INTO TABLE IT_ZSDT0059
    FROM ZSDT0059
     FOR ALL ENTRIES IN IT_ZSDT0051
   WHERE NRO_SOL_OV = IT_ZSDT0051-NRO_SOL_OV .

  SELECT *
    INTO TABLE IT_ZSDT0059_PR
    FROM ZSDT0059
     FOR ALL ENTRIES IN IT_ZSDT0051
   WHERE NRO_SOL_OV = IT_ZSDT0051-NRO_SOL_OV
     AND FIELD      = 'PRECO'.

  SELECT *
    INTO TABLE IT_ZSDT0059_SP
    FROM ZSDT0059
     FOR ALL ENTRIES IN IT_ZSDT0051
   WHERE NRO_SOL_OV = IT_ZSDT0051-NRO_SOL_OV
     AND FIELD      = 'PRECO'.

*pegar o campo RFMNG
  SELECT *
    INTO TABLE IT_VBFA
    FROM VBFA
     FOR ALL ENTRIES IN IT_ZSDT0053
   WHERE VBELV = IT_ZSDT0053-VBELN
     AND VBTYP_N = 'H'
     AND VBTYP_V = 'C'.
*  IF SY-SUBRC IS INITIAL .
*    SELECT *
*      INTO TABLE IT_VBAK
*      FROM VBAK
*       FOR ALL ENTRIES IN IT_VBFA
*     WHERE VBELN = IT_VBFA-VBELN
*       AND AUART = 'ZROB'."Tipo recusa
*  ENDIF.

  SELECT *
    FROM ZSDT0097
    INTO TABLE IT_ZSDT0097
   WHERE TP_REGISTRO NE 'SP'.

  SELECT *
    INTO TABLE IT_ZSDT0094
    FROM ZSDT0094
     FOR ALL ENTRIES IN IT_ZSDT0051
   WHERE NRO_SOL_OV EQ IT_ZSDT0051-NRO_SOL_OV.

  IF 1 = 2.
    DELETE FROM ZSDT0097 WHERE MATERIAL EQ '000000000000119920'.
  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZACAO_DADOS .

  SORT : IT_ZSDT0051    BY NRO_SOL_OV,
         IT_ZSDT0053    BY NRO_SOL_OV FIXACAO POSNR,
         IT_ZSDT0059    BY NRO_SOL_OV BEZEI FIELD TIPO_CALC,
         IT_ZSDT0059_PR BY NRO_SOL_OV POSNR COD_FP BEZEI FIELD TIPO_CALC ,
         IT_ZSDT0059_SP BY NRO_SOL_OV POSNR BEZEI FIELD TIPO_CALC ,
         IT_VBFA        BY VBELV,
         "IT_VBAK        BY VBELN,
         IT_ZSDT0097    BY NR_SOL_OV NR_ORDEM FILIAL TP_VENDA POSNR DOC_DEV.
  "IT_ZSDT0097    BY NR_SOL_OV NR_ORDEM FILIAL TP_VENDA POSNR DOC_DEV SAFRA MATERIAL DT_INI_EMBARQUE DT_FIM_EMBARQUE
  "                  DT_SOLICITACAO DT_VENDA QUANTIDADE VR_BASE_PREMIO UN_MEDIDA VR_BASE_SPREAD DT_FIXACAO_PREMI
  "                  ANO_PREMIO MES_PREMIO VR_ACIMA_EXPORTA STATUS .

  "Solicitações Liberadas
  LOOP AT IT_ZSDT0051 INTO WA_ZSDT0051.
    CLEAR: WA_SAIDA.
    WA_SAIDA-NR_SOL_OV       = WA_ZSDT0051-NRO_SOL_OV.
    WA_SAIDA-TP_VENDA	       = WA_ZSDT0051-TP_VENDA.
    WA_SAIDA-MATERIAL        = WA_ZSDT0051-MATNR.
    WA_SAIDA-DT_INI_EMBARQUE = WA_ZSDT0051-DTDE_LOGIST.
    WA_SAIDA-DT_FIM_EMBARQUE = WA_ZSDT0051-DTATE_LOGIST.
    WA_SAIDA-DT_VENDA	       = WA_ZSDT0051-DATA_VENDA.
    WA_SAIDA-DT_SOLICITACAO  = WA_ZSDT0051-DATA_VENDA.

    "LOOP AT IT_ZSDT0053 INTO WA_ZSDT0053 WHERE NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV  .
    "Vendas Frame 00002
    IF WA_ZSDT0051-TP_VENDA = '00091' OR WA_ZSDT0051-TP_VENDA = '00027'.

      LOOP AT IT_ZSDT0059 INTO WA_ZSDT0059 WHERE NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV
                                             AND BEZEI(1)   = 'P'
                                             AND FIELD      = 'QTDFIXADA'
                                             AND TIPO_CALC  = 'V'
                                             AND POSNR1    <> '000000' .

        "Premio
        CLEAR WA_ZSDT0059_PR.
        READ TABLE IT_ZSDT0059_PR INTO WA_ZSDT0059_PR WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV
                                                               POSNR     = WA_ZSDT0059-POSNR
                                                               COD_FP    = WA_ZSDT0059-COD_FP
                                                               BEZEI(1)  = 'P'
                                                               FIELD     = 'PRECO'
                                                               TIPO_CALC = 'V' BINARY SEARCH.


        WA_SAIDA-STATUS             = WA_ZSDT0051-STATUS.
        WA_SAIDA-VR_BASE_PREMIO	    = WA_ZSDT0059_PR-FORMULA2.
        WA_SAIDA-DT_FIXACAO_PREMIO  = WA_ZSDT0059_PR-VALDT.

        IF WA_ZSDT0059_PR-MONAT = '00'.
          WA_SAIDA-ANO_PREMIO       = WA_ZSDT0059_PR-DATA_ATUAL(4).
          WA_SAIDA-MES_PREMIO       = WA_ZSDT0059_PR-DATA_ATUAL+4(2).
        ELSE.
          CONCATENATE '201' WA_ZSDT0059_PR-CBOT+1(1) INTO WA_SAIDA-ANO_PREMIO.
          WA_SAIDA-MES_PREMIO	= WA_ZSDT0059_PR-MONAT.
        ENDIF.

        WA_SAIDA-QUANTIDADE	        = WA_ZSDT0059-FORMULA2.
        WA_SAIDA-COD_FP             = WA_ZSDT0059-COD_FP.

        "---VLR ACIMA DA EXPORT
        CLEAR WA_ZSDT0059_SP.
        READ TABLE IT_ZSDT0059_SP INTO WA_ZSDT0059_SP WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV
                                                               POSNR     = WA_ZSDT0059-POSNR
                                                               "COD_FP    = WA_ZSDT0059-COD_FP
                                                               BEZEI     = 'VLR ACIMA DA EXPORT'
                                                               FIELD     = 'PRECO'
                                                               TIPO_CALC = 'C' BINARY SEARCH.

        WA_SAIDA-VR_ACIMA_EXPORTACAO = WA_ZSDT0059_SP-FORMULA2 .
        "---

        "---SPRED
        CLEAR WA_ZSDT0059_SP.
        READ TABLE IT_ZSDT0059_SP INTO WA_ZSDT0059_SP WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV
                                                               POSNR     = WA_ZSDT0059-POSNR
                                                               "COD_FP    = WA_ZSDT0059-COD_FP
                                                               BEZEI     = 'SPRED'
                                                               FIELD     = 'PRECO'
                                                               TIPO_CALC = 'V' BINARY SEARCH.

        WA_SAIDA-VR_BASE_SPREAD	 = WA_ZSDT0059_SP-FORMULA2.
        "---

        READ TABLE IT_ZSDT0053 INTO WA_ZSDT0053 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV
                                                         FIXACAO    = WA_ZSDT0059-POSNR
                                                         POSNR      = WA_ZSDT0059-POSNR1 BINARY SEARCH.

        IF WA_ZSDT0053-STATUS EQ 'Y' OR WA_ZSDT0053-STATUS EQ 'C'.
          WA_SAIDA-STATUS = WA_ZSDT0053-STATUS.
        ENDIF.

        WA_SAIDA-FILIAL          = WA_ZSDT0053-WERKS.
        WA_SAIDA-SAFRA           = WA_ZSDT0053-CHARG.
        WA_SAIDA-TP_REGISTRO     = WA_ZSDT0053-VOLEH.
        WA_SAIDA-UN_MEDIDA       = WA_ZSDT0053-PMEIN.
        WA_SAIDA-NR_ORDEM	       = WA_ZSDT0053-VBELN.
        WA_SAIDA-POSNR           = WA_ZSDT0053-POSNR.
        WA_SAIDA-DOC_DEV         = '0000000001'.

        IF WA_SAIDA-DT_FIXACAO_PREMIO <> '00000000' AND NOT WA_SAIDA-DT_FIXACAO_PREMIO IS INITIAL.
          PERFORM GRAVA_TABELA_INTERFACE USING WA_SAIDA.
        ENDIF.

      ENDLOOP.

      "---Devoluções
      LOOP AT IT_ZSDT0053 INTO WA_ZSDT0053 WHERE NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV.
        WA_SAIDA-FILIAL             = WA_ZSDT0053-WERKS.
        WA_SAIDA-SAFRA              = WA_ZSDT0053-CHARG.
        WA_SAIDA-TP_REGISTRO        = WA_ZSDT0053-VOLEH.
        WA_SAIDA-UN_MEDIDA          = WA_ZSDT0053-PMEIN.
        WA_SAIDA-NR_ORDEM	          = WA_ZSDT0053-VBELN.
        WA_SAIDA-POSNR              = WA_ZSDT0053-POSNR.


        "---VLR ACIMA DA EXPORT
        CLEAR WA_ZSDT0059_SP.
        READ TABLE IT_ZSDT0059_SP INTO WA_ZSDT0059_SP WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV
                                                               POSNR     = WA_ZSDT0053-FIXACAO
                                                               "COD_FP    = WA_ZSDT0059-COD_FP
                                                               BEZEI     = 'VLR ACIMA DA EXPORT'
                                                               FIELD     = 'PRECO'
                                                               TIPO_CALC = 'C' BINARY SEARCH.

        WA_SAIDA-VR_ACIMA_EXPORTACAO = WA_ZSDT0059_SP-FORMULA2 .
        "---
        "---SPRED
        CLEAR WA_ZSDT0059_SP.
        READ TABLE IT_ZSDT0059_SP INTO WA_ZSDT0059_SP WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV
                                                               POSNR     = WA_ZSDT0053-FIXACAO
                                                               BEZEI     = 'SPRED'
                                                               FIELD     = 'PRECO'
                                                               TIPO_CALC = 'V' BINARY SEARCH.

        WA_SAIDA-VR_BASE_SPREAD	 = WA_ZSDT0059_SP-FORMULA2.
        "---
        "---Premio Frame
        READ TABLE IT_ZSDT0059_SP INTO WA_ZSDT0059_SP WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV
                                                               POSNR     = WA_ZSDT0053-FIXACAO
                                                               BEZEI     = 'PREMIO FRAME'
                                                               FIELD     = 'PRECO'
                                                               TIPO_CALC = 'C' BINARY SEARCH.


        WA_SAIDA-VR_BASE_PREMIO	    = WA_ZSDT0059_SP-FORMULA2.
        WA_SAIDA-COD_FP             = WA_ZSDT0059-COD_FP.

        CLEAR WA_ZSDT0059.
        READ TABLE IT_ZSDT0059 INTO WA_ZSDT0059 WITH KEY NRO_SOL_OV      = WA_ZSDT0051-NRO_SOL_OV
                                                               POSNR     = WA_ZSDT0053-FIXACAO
                                                               BEZEI(1)  = 'P'
                                                               FIELD     = 'QTDFIXADA'
                                                               TIPO_CALC = 'V' .
        IF WA_ZSDT0059-MONAT = '00'.
          WA_SAIDA-ANO_PREMIO       = WA_ZSDT0059-DATA_ATUAL(4).
          WA_SAIDA-MES_PREMIO       = WA_ZSDT0059-DATA_ATUAL+4(2).
        ELSE.
          CONCATENATE '201' WA_ZSDT0059-CBOT+1(1) INTO WA_SAIDA-ANO_PREMIO.
          WA_SAIDA-MES_PREMIO	      = WA_ZSDT0059-MONAT.
          "WA_SAIDA-ANO_PREMIO          = WA_ZSDT0059-CBOT .
        ENDIF.

        "-------

        LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV = WA_ZSDT0053-VBELN.
          "READ TABLE IT_VBAK INTO WA_VBAK WITH KEY VBELN = WA_VBFA-VBELN BINARY SEARCH.
          "IF NOT SY-SUBRC IS INITIAL.
          WA_SAIDA-QUANTIDADE	       = WA_VBFA-RFMNG.
          WA_SAIDA-DOC_DEV           = WA_VBFA-VBELN.
          WA_SAIDA-DT_FIXACAO_PREMIO = WA_VBFA-ERDAT.
          WA_SAIDA-STATUS            = 'V'.
          PERFORM GRAVA_TABELA_INTERFACE USING WA_SAIDA.
          "ENDIF.
        ENDLOOP.

      ENDLOOP.
    ELSE.
      "Venda Normal
      "WA_SAIDA-QUANTIDADE = 0.
      LOOP AT IT_ZSDT0053 INTO WA_ZSDT0053 WHERE NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV  .

        WA_SAIDA-STATUS          = WA_ZSDT0051-STATUS.
        WA_SAIDA-FILIAL          = WA_ZSDT0053-WERKS.
        WA_SAIDA-QUANTIDADE	     = WA_ZSDT0053-ZMENG.
        WA_SAIDA-SAFRA           = WA_ZSDT0053-CHARG.
        WA_SAIDA-TP_REGISTRO     = WA_ZSDT0053-VOLEH.
        WA_SAIDA-UN_MEDIDA       = WA_ZSDT0053-PMEIN.
        WA_SAIDA-NR_ORDEM	       = WA_ZSDT0053-VBELN.
        WA_SAIDA-POSNR           = WA_ZSDT0053-POSNR.

        CLEAR: WA_ZSDT0059.
        READ TABLE IT_ZSDT0059 INTO WA_ZSDT0059 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV BEZEI = 'SPRED' BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WA_SAIDA-VR_BASE_SPREAD	 = WA_ZSDT0059-FORMULA2.
          WA_SAIDA-COD_FP          = WA_ZSDT0059-COD_FP.
        ELSE.
          READ TABLE IT_ZSDT0059 INTO WA_ZSDT0059 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV BEZEI = 'SPREAD' BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WA_SAIDA-VR_BASE_SPREAD	 = WA_ZSDT0059-FORMULA2.
            WA_SAIDA-COD_FP          = WA_ZSDT0059-COD_FP.
          ENDIF.
        ENDIF.

        IF WA_ZSDT0051-MATNR NE '000000000000119920'.
          IF WA_SAIDA-VR_BASE_SPREAD IS INITIAL.
            CLEAR: WA_ZSDT0059.
            READ TABLE IT_ZSDT0059 INTO WA_ZSDT0059 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV BEZEI = 'SPREAD' BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              WA_SAIDA-VR_BASE_SPREAD	 = WA_ZSDT0059-FORMULA2.
            ENDIF.
          ENDIF.
        ENDIF.

        "Casca de Soja - Não possui premio - campo premio foi utilizado para valor de venda """""""""""""""""""""""""""""""""
        "Casca de Soja - Não possui premio - campo SPREAD foi utilizado para cotacao """"""""""""""""""""""""""""""""""""""""
        CLEAR: WA_ZSDT0059.
        IF WA_ZSDT0051-MATNR EQ '000000000000119920'.
          READ TABLE IT_ZSDT0059 INTO WA_ZSDT0059 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV BEZEI = 'PREÇO VENDA BRUTO'  BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.

            CLEAR: VL_UKURS.

            "Indice Dólar Venda
            IF WA_ZSDT0059-WAERS NE 'USD'.
              LC_MOEDA = 'USD'.
              CLEAR: WA_ZSDT0094.
              VL_UKURS = 0.
              LOOP AT IT_ZSDT0094 WHERE NRO_SOL_OV EQ WA_ZSDT0051-NRO_SOL_OV.
                IF ( WA_ZSDT0094-DATA_REGISTRO < IT_ZSDT0094-DATA_REGISTRO ) OR
                   ( WA_ZSDT0094-DATA_REGISTRO = IT_ZSDT0094-DATA_REGISTRO AND WA_ZSDT0094-HORA_REGISTRO < IT_ZSDT0094-HORA_REGISTRO ).
                  VL_UKURS = IT_ZSDT0094-TAXA_CURVA.
                  MOVE IT_ZSDT0094 TO WA_ZSDT0094.
                ENDIF.
              ENDLOOP.
            ELSE.
              VL_UKURS = 1.
            ENDIF.

            WA_SAIDA-VR_BASE_PREMIO	    = WA_ZSDT0059-FORMULA2.
            WA_SAIDA-VR_BASE_SPREAD     = VL_UKURS.
            WA_SAIDA-DT_FIXACAO_PREMIO  = WA_ZSDT0051-DATA_VENDA.
          ENDIF.
        ELSE.
          READ TABLE IT_ZSDT0059 INTO WA_ZSDT0059 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV BEZEI = 'PREMIO'  BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WA_SAIDA-VR_BASE_PREMIO	    = WA_ZSDT0059-FORMULA2.
            WA_SAIDA-DT_FIXACAO_PREMIO  = WA_ZSDT0059-VALDT.
          ENDIF.
        ENDIF.

        WA_SAIDA-ANO_PREMIO	        = WA_ZSDT0051-DTDE_LOGIST(4).

        IF ( WA_ZSDT0059-MONAT = '00' ) OR ( WA_ZSDT0059-MONAT IS INITIAL ).
          WA_SAIDA-MES_PREMIO       = WA_ZSDT0051-DTDE_LOGIST+4(2).
        ELSE.
          WA_SAIDA-MES_PREMIO	      = WA_ZSDT0059-MONAT.
        ENDIF.

        CLEAR: WA_ZSDT0059.
        READ TABLE IT_ZSDT0059 INTO WA_ZSDT0059 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV BEZEI = 'VLR ACIMA DA EXPORT' BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WA_SAIDA-VR_ACIMA_EXPORTACAO = WA_ZSDT0059-FORMULA2 .
        ENDIF.

        IF WA_ZSDT0053-STATUS EQ 'Y' OR WA_ZSDT0053-STATUS EQ 'C'.
          WA_SAIDA-STATUS = WA_ZSDT0053-STATUS.
        ENDIF.

        WA_SAIDA-TP_REGISTRO         = 'PR'.
        WA_SAIDA-DOC_DEV             = '0000000001'.
        PERFORM GRAVA_TABELA_INTERFACE USING WA_SAIDA.

        "Devoluções
        LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV = WA_ZSDT0053-VBELN.
          WA_SAIDA-QUANTIDADE	       = WA_VBFA-RFMNG.
          WA_SAIDA-DOC_DEV           = WA_VBFA-VBELN.
          WA_SAIDA-DT_FIXACAO_PREMIO = WA_VBFA-ERDAT.
          WA_SAIDA-STATUS            = 'V'.
          PERFORM GRAVA_TABELA_INTERFACE USING WA_SAIDA.
        ENDLOOP.

      ENDLOOP.

    ENDIF.

  ENDLOOP.

  IF NOT IT_SAIDA[] IS INITIAL.
    CALL FUNCTION 'Z_SD_OUTBOUND_PNL_PREMIO' IN BACKGROUND TASK
      DESTINATION 'XI_PNL_PREMIO'
      TABLES
        T_ZSD_PNL_PREMIO = IT_SAIDA[].

    COMMIT WORK.
  ENDIF.


ENDFORM.                    " ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_INTERFACE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM GRAVA_TABELA_INTERFACE  USING P_WA_SAIDA TYPE ZSD_PNL_PREMIO.

  CLEAR WA_ZSDT0097.

  READ TABLE IT_ZSDT0097 INTO WA_ZSDT0097 WITH KEY NR_SOL_OV        = P_WA_SAIDA-NR_SOL_OV
                                                   NR_ORDEM         = P_WA_SAIDA-NR_ORDEM
                                                   FILIAL           = P_WA_SAIDA-FILIAL
                                                   TP_VENDA         = P_WA_SAIDA-TP_VENDA
                                                   POSNR            = P_WA_SAIDA-POSNR
                                                   DOC_DEV          = P_WA_SAIDA-DOC_DEV
                                                   COD_FP           = P_WA_SAIDA-COD_FP.

  "Só envia o que não esta na interface, ou que teve alguma alteração
  IF NOT SY-SUBRC IS INITIAL OR
    ( SY-SUBRC IS INITIAL AND
      ( WA_ZSDT0097-QUANTIDADE       <> P_WA_SAIDA-QUANTIDADE      OR
        WA_ZSDT0097-ANO_PREMIO       <> P_WA_SAIDA-ANO_PREMIO      OR
        WA_ZSDT0097-MES_PREMIO       <> P_WA_SAIDA-MES_PREMIO      OR
        WA_ZSDT0097-VR_ACIMA_EXPORTA <> P_WA_SAIDA-VR_ACIMA_EXPORTACAO OR
        WA_ZSDT0097-VR_BASE_PREMIO   <> P_WA_SAIDA-VR_BASE_PREMIO  OR
        WA_ZSDT0097-VR_BASE_SPREAD   <> P_WA_SAIDA-VR_BASE_SPREAD  OR
        WA_ZSDT0097-SAFRA            <> P_WA_SAIDA-SAFRA  OR
        WA_ZSDT0097-DT_FIXACAO_PREMI <> P_WA_SAIDA-DT_FIXACAO_PREMIO OR
        WA_ZSDT0097-STATUS           <> P_WA_SAIDA-STATUS  OR
        WA_ZSDT0097-NR_ORDEM         <> P_WA_SAIDA-NR_ORDEM ) ).

    CLEAR WA_ZSDT0097.

    WA_ZSDT0097-NR_SOL_OV        = P_WA_SAIDA-NR_SOL_OV       .
    WA_ZSDT0097-NR_ORDEM         = P_WA_SAIDA-NR_ORDEM        .
    WA_ZSDT0097-FILIAL           = P_WA_SAIDA-FILIAL          .
    WA_ZSDT0097-DATA_ATUAL       = SY-DATUM                   .
    WA_ZSDT0097-HORA_ATUAL       = SY-UZEIT                   .
    WA_ZSDT0097-POSNR            = P_WA_SAIDA-POSNR           .
    WA_ZSDT0097-DOC_DEV          = P_WA_SAIDA-DOC_DEV         .
    WA_ZSDT0097-COD_FP           = P_WA_SAIDA-COD_FP          .
    WA_ZSDT0097-TP_VENDA         = P_WA_SAIDA-TP_VENDA        .
    WA_ZSDT0097-SAFRA            = P_WA_SAIDA-SAFRA           .
    WA_ZSDT0097-MATERIAL         = P_WA_SAIDA-MATERIAL        .
    WA_ZSDT0097-DT_INI_EMBARQUE  = P_WA_SAIDA-DT_INI_EMBARQUE .
    WA_ZSDT0097-DT_FIM_EMBARQUE  = P_WA_SAIDA-DT_FIM_EMBARQUE .
    WA_ZSDT0097-DT_SOLICITACAO   = P_WA_SAIDA-DT_SOLICITACAO  .
    WA_ZSDT0097-DT_VENDA         = P_WA_SAIDA-DT_VENDA        .
    WA_ZSDT0097-QUANTIDADE       = P_WA_SAIDA-QUANTIDADE      .
    WA_ZSDT0097-VR_BASE_PREMIO   = P_WA_SAIDA-VR_BASE_PREMIO  .
    WA_ZSDT0097-UN_MEDIDA        = P_WA_SAIDA-UN_MEDIDA       .
    WA_ZSDT0097-VR_BASE_SPREAD   = P_WA_SAIDA-VR_BASE_SPREAD  .
    WA_ZSDT0097-DT_FIXACAO_PREMI = P_WA_SAIDA-DT_FIXACAO_PREMIO.
    WA_ZSDT0097-ANO_PREMIO       = P_WA_SAIDA-ANO_PREMIO      .
    WA_ZSDT0097-MES_PREMIO       = P_WA_SAIDA-MES_PREMIO      .
    WA_ZSDT0097-VR_ACIMA_EXPORTA = P_WA_SAIDA-VR_ACIMA_EXPORTACAO.
    "Somente utilizado para Premio
    WA_ZSDT0097-TP_REGISTRO      = 'PR'.
    WA_ZSDT0097-STATUS           = P_WA_SAIDA-STATUS          .

    MODIFY ZSDT0097 FROM WA_ZSDT0097.
    WA_SAIDA-TP_REGISTRO = 'PR'.
    APPEND WA_SAIDA TO IT_SAIDA.
  ENDIF.

ENDFORM.                    " GRAVA_INTERFACE

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_SPREAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS_SPREAD .

  TYPES: BEGIN OF TY_REGISTRO.
  TYPES: NRO_SOL_OV	       TYPE ZSDED013,
         NR_ORDEM          TYPE VBELN_VA,
         FILIAL            TYPE WERKS_D,
         DATA_ATUAL        TYPE DATS,
         HORA_ATUAL        TYPE TIMS,
         POSNR             TYPE POSNR_VA,
         DOC_DEV           TYPE VBELN_NACH,
         COD_FP            TYPE ZSDED019,
         TP_VENDA          TYPE ZSDED012,
         SAFRA             TYPE CHARG_D,
         MATERIAL          TYPE MATNR,
         DT_INI_EMBARQUE   TYPE VALDT,
         DT_FIM_EMBARQUE   TYPE VALDT,
         DT_SOLICITACAO    TYPE VALDT,
         DT_VENDA          TYPE VALDT,
         QUANTIDADE        TYPE BRGEW_AP,
         UN_MEDIDA         TYPE VOLEH,
         VR_BASE_PREMIO    TYPE DMBTR,
         VR_BASE_SPREAD    TYPE DMBTR,
         FORMULA2          TYPE BEZEI60,
         DT_FIXACAO_SPREAD TYPE VALDT,
         ANO_PREMIO        TYPE CHAR4,
         MES_PREMIO        TYPE CHAR2,
         STATUS            TYPE CHAR1,
         TP_REGISTRO       TYPE CHAR2.
  TYPES: END OF TY_REGISTRO.

  DATA: WA_REGISTRO TYPE TY_REGISTRO,
        IT_REGISTRO TYPE TABLE OF TY_REGISTRO WITH HEADER LINE.

  "Conversão S4 Hana - Amaggi - WPP - Ini
   EXEC SQL.
    OPEN SPREADS FOR

 SELECT IT.NRO_SOL_OV,
         TO_CHAR(IT.VBELN) AS NR_ORDEM,
         IT.WERKS AS FILIAL,
         IT.POSNR,
         '0000000001' AS DOC_DEV,
         PC.COD_FP,
         CB.TP_VENDA,
         IT.CHARG AS SAFRA,
         CB.MATNR AS MATERIAL,
         CB.DTDE_LOGIST  AS DT_INI_EMBARQUE,
         CB.DTATE_LOGIST AS DT_FIM_EMBARQUE,
         CB.DATA_VENDA   AS DT_SOLICITACAO,
         CB.DATA_VENDA   AS DATA_VENDA,
         IT.ZMENG        AS QUANTIDADE,
         IT.PMEIN        AS UN_MEDIDA,

         PC.FORMULA2     AS FORMULA2,
         CB.DATA_VENDA   AS DT_FIXACAO_SPREAD,

         CASE WHEN CB.TP_VENDA IN ('00091','00027') THEN
                CASE WHEN COALESCE(ANO_MES.MONAT,'00') = '00' THEN SUBSTR(PC.DATA_ATUAL,1,4) ELSE '201'||SUBSTR(ANO_MES.CBOT,2,1) END
              ELSE
                SUBSTR(CB.DTDE_LOGIST,1,4)
         END AS ANO_PREMIO,

         CASE WHEN CB.TP_VENDA IN ('00091','00027') THEN
                CASE WHEN COALESCE(ANO_MES.MONAT,'00') = '00' THEN SUBSTR(PC.DATA_ATUAL,5,2) ELSE ANO_MES.MONAT END
              ELSE
                CASE WHEN (COALESCE(MES_SPREAD.MONAT,'00') = '00') AND (COALESCE(MES_PREMIO.MONAT,'00') = '00') THEN
                    SUBSTR(CB.DTDE_LOGIST,5,2)
                ELSE
                   CASE WHEN COALESCE(MES_SPREAD.MONAT,'00') = '00' THEN
                          MES_PREMIO.MONAT
                        ELSE
                          MES_SPREAD.MONAT
                   END
                END
         END AS MES_PREMIO,

         CASE WHEN IT.STATUS IN ('Y','C') THEN IT.STATUS ELSE CB.STATUS END AS STATUS,
         'SP'       AS TP_REGISTRO

    FROM SAPHANADB.ZSDT0051 CB,
         SAPHANADB.ZSDT0053 IT LEFT JOIN ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT, O.CBOT
                       FROM SAPHANADB.ZSDT0059 O
                      WHERE O.MONAT      > '00'
                        AND O.CBOT      <> ' ' ) ANO_MES

                         ON IT.NRO_SOL_OV = ANO_MES.NRO_SOL_OV
                          AND IT.FIXACAO    = ANO_MES.POSNR

                 LEFT JOIN ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT
                       FROM SAPHANADB.ZSDT0059 O
                      WHERE O.BEZEI IN ('SPRED','SPREAD') ) MES_SPREAD

                        ON IT.NRO_SOL_OV = MES_SPREAD.NRO_SOL_OV
                                             AND IT.FIXACAO    = MES_SPREAD.POSNR

                     LEFT JOIN ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT
                       FROM SAPHANADB.ZSDT0059 O
                      WHERE O.BEZEI IN ('PREMIO') ) MES_PREMIO

                       ON IT.NRO_SOL_OV = MES_PREMIO.NRO_SOL_OV
                                            AND IT.FIXACAO    = MES_PREMIO.POSNR,

         SAPHANADB.ZSDT0059 PC


   WHERE CB.VTWEG      = '10'
     AND CB.SPART      = '01'
     AND CB.NRO_SOL_OV = IT.NRO_SOL_OV
     AND IT.NRO_SOL_OV = PC.NRO_SOL_OV
     AND IT.FIXACAO    = PC.POSNR
     AND PC.BEZEI      IN ('SPRED','SPREAD')
  UNION ALL

  SELECT IT.NRO_SOL_OV,
         TO_CHAR(IT.VBELN) AS NR_ORDEM,
         IT.WERKS AS FILIAL,
         IT.POSNR,
         FA.VBELN AS DOC_DEV,
         PC.COD_FP,
         CB.TP_VENDA,
         IT.CHARG AS SAFRA,
         CB.MATNR AS MATERIAL,
         CB.DTDE_LOGIST  AS DT_INI_EMBARQUE,
         CB.DTATE_LOGIST AS DT_FIM_EMBARQUE,
         CB.DATA_VENDA   AS DT_SOLICITACAO,
         CB.DATA_VENDA   AS DATA_VENDA,
         FA.RFMNG        AS QUANTIDADE,
         IT.PMEIN        AS UN_MEDIDA,
         PC.FORMULA2     AS FORMULA2,
         FA.ERDAT        AS DT_FIXACAO_SPREAD,

         CASE WHEN CB.TP_VENDA IN ('00091','00027') THEN
                CASE WHEN COALESCE(ANO_MES.MONAT,'00') = '00' THEN SUBSTR(PC.DATA_ATUAL,1,4) ELSE '201'||SUBSTR(ANO_MES.CBOT,2,1) END
              ELSE
                SUBSTR(CB.DTDE_LOGIST,1,4)
         END AS ANO_PREMIO,

         CASE WHEN CB.TP_VENDA IN ('00091','00027') THEN
                CASE WHEN COALESCE(ANO_MES.MONAT,'00') = '00' THEN SUBSTR(PC.DATA_ATUAL,5,2) ELSE ANO_MES.MONAT END
              ELSE
                CASE WHEN (COALESCE(MES_SPREAD.MONAT,'00') = '00') AND (COALESCE(MES_PREMIO.MONAT,'00') = '00') THEN
                    SUBSTR(CB.DTDE_LOGIST,5,2)
                ELSE
                   CASE WHEN COALESCE(MES_SPREAD.MONAT,'00') = '00' THEN
                          MES_PREMIO.MONAT
                        ELSE
                          MES_SPREAD.MONAT
                   END
                END
         END AS MES_PREMIO,

         'V'  AS STATUS,
         'SP' AS TP_REGISTRO
    FROM SAPHANADB.ZSDT0051 CB,
         SAPHANADB.ZSDT0053 IT LEFT JOIN ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT, O.CBOT
                       FROM SAPHANADB.ZSDT0059 O
                      WHERE O.MONAT      > '00'
                        AND O.CBOT      <> ' '  ) ANO_MES

                                    ON IT.NRO_SOL_OV = ANO_MES.NRO_SOL_OV
                                   AND IT.FIXACAO    = ANO_MES.POSNR


                               LEFT JOIN ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT
                       FROM SAPHANADB.ZSDT0059 O
                      WHERE O.BEZEI IN ('SPRED','SPREAD') ) MES_SPREAD

                  ON IT.NRO_SOL_OV = MES_SPREAD.NRO_SOL_OV
                   AND IT.FIXACAO    = MES_SPREAD.POSNR


                               LEFT JOIN ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT
                       FROM SAPHANADB.ZSDT0059 O
                      WHERE O.BEZEI IN ('PREMIO') ) MES_PREMIO

                   ON IT.NRO_SOL_OV = MES_PREMIO.NRO_SOL_OV
                   AND IT.FIXACAO    = MES_PREMIO.POSNR ,


         SAPHANADB.ZSDT0059 PC,
         SAPHANADB.VBFA     FA
   WHERE CB.VTWEG      = '10'
     AND CB.SPART      = '01'
     AND CB.NRO_SOL_OV = IT.NRO_SOL_OV
     AND PC.NRO_SOL_OV = IT.NRO_SOL_OV
     AND IT.FIXACAO    = PC.POSNR
     AND PC.BEZEI      IN ('SPRED','SPREAD')
   AND IT.VBELN      <> ''
     AND IT.VBELN      = FA.VBELV
     AND FA.VBTYP_N    = 'H'
     AND FA.VBTYP_V    = 'C'

  ENDEXEC.



*  EXEC SQL.
*    OPEN SPREADS FOR
*  SELECT IT.NRO_SOL_OV,
*         TO_CHAR(IT.VBELN) AS NR_ORDEM,
*         IT.WERKS AS FILIAL,
*         IT.POSNR,
*         '0000000001' AS DOC_DEV,
*         PC.COD_FP,
*         CB.TP_VENDA,
*         IT.CHARG AS SAFRA,
*         CB.MATNR AS MATERIAL,
*         CB.DTDE_LOGIST  AS DT_INI_EMBARQUE,
*         CB.DTATE_LOGIST AS DT_FIM_EMBARQUE,
*         CB.DATA_VENDA   AS DT_SOLICITACAO,
*         CB.DATA_VENDA   AS DATA_VENDA,
*         IT.ZMENG        AS QUANTIDADE,
*         IT.PMEIN        AS UN_MEDIDA,
*
*         PC.FORMULA2     AS FORMULA2,
*         CB.DATA_VENDA   AS DT_FIXACAO_SPREAD,
*
*         CASE WHEN CB.TP_VENDA IN ('00091','00027') THEN
*                CASE WHEN COALESCE(ANO_MES.MONAT,'00') = '00' THEN SUBSTR(PC.DATA_ATUAL,1,4) ELSE '201'||SUBSTR(ANO_MES.CBOT,2,1) END
*              ELSE
*                SUBSTR(CB.DTDE_LOGIST,1,4)
*         END AS ANO_PREMIO,
*
*         CASE WHEN CB.TP_VENDA IN ('00091','00027') THEN
*                CASE WHEN COALESCE(ANO_MES.MONAT,'00') = '00' THEN SUBSTR(PC.DATA_ATUAL,5,2) ELSE ANO_MES.MONAT END
*              ELSE
*                CASE WHEN (COALESCE(MES_SPREAD.MONAT,'00') = '00') AND (COALESCE(MES_PREMIO.MONAT,'00') = '00') THEN
*                    SUBSTR(CB.DTDE_LOGIST,5,2)
*                ELSE
*                   CASE WHEN COALESCE(MES_SPREAD.MONAT,'00') = '00' THEN
*                          MES_PREMIO.MONAT
*                        ELSE
*                          MES_SPREAD.MONAT
*                   END
*                END
*         END AS MES_PREMIO,
*
*         CASE WHEN IT.STATUS IN ('Y','C') THEN IT.STATUS ELSE CB.STATUS END AS STATUS,
*         'SP'       AS TP_REGISTRO
*
*    FROM SAPHANADB.ZSDT0051 CB,
*         SAPHANADB.ZSDT0053 IT,
*         SAPHANADB.ZSDT0059 PC,
*         ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT, O.CBOT
*             FROM SAPHANADB.ZSDT0059 O
*            WHERE O.MONAT      > '00'
*              AND O.CBOT      <> ' ' ) ANO_MES,
*         ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT
*             FROM SAPHANADB.ZSDT0059 O
*            WHERE O.BEZEI IN ('SPRED','SPREAD') ) MES_SPREAD,
*         ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT
*             FROM SAPHANADB.ZSDT0059 O
*            WHERE O.BEZEI IN ('PREMIO') ) MES_PREMIO
*   WHERE CB.VTWEG      = '10'
*     AND CB.SPART      = '01'
*     AND CB.NRO_SOL_OV = IT.NRO_SOL_OV
*     AND IT.NRO_SOL_OV = PC.NRO_SOL_OV
*     AND IT.FIXACAO    = PC.POSNR
*     AND PC.BEZEI      IN ('SPRED','SPREAD')
*     AND IT.NRO_SOL_OV = ANO_MES.NRO_SOL_OV(+)
*     AND IT.FIXACAO    = ANO_MES.POSNR(+)
*     AND IT.NRO_SOL_OV = MES_SPREAD.NRO_SOL_OV(+)
*     AND IT.FIXACAO    = MES_SPREAD.POSNR(+)
*     AND IT.NRO_SOL_OV = MES_PREMIO.NRO_SOL_OV(+)
*     AND IT.FIXACAO    = MES_PREMIO.POSNR(+)
*  UNION ALL
*  SELECT IT.NRO_SOL_OV,
*         TO_CHAR(IT.VBELN) AS NR_ORDEM,
*         IT.WERKS AS FILIAL,
*         IT.POSNR,
*         FA.VBELN AS DOC_DEV,
*         PC.COD_FP,
*         CB.TP_VENDA,
*         IT.CHARG AS SAFRA,
*         CB.MATNR AS MATERIAL,
*         CB.DTDE_LOGIST  AS DT_INI_EMBARQUE,
*         CB.DTATE_LOGIST AS DT_FIM_EMBARQUE,
*         CB.DATA_VENDA   AS DT_SOLICITACAO,
*         CB.DATA_VENDA   AS DATA_VENDA,
*         FA.RFMNG        AS QUANTIDADE,
*         IT.PMEIN        AS UN_MEDIDA,
*         PC.FORMULA2     AS FORMULA2,
*         FA.ERDAT        AS DT_FIXACAO_SPREAD,
*
*         CASE WHEN CB.TP_VENDA IN ('00091','00027') THEN
*                CASE WHEN COALESCE(ANO_MES.MONAT,'00') = '00' THEN SUBSTR(PC.DATA_ATUAL,1,4) ELSE '201'||SUBSTR(ANO_MES.CBOT,2,1) END
*              ELSE
*                SUBSTR(CB.DTDE_LOGIST,1,4)
*         END AS ANO_PREMIO,
*
*         CASE WHEN CB.TP_VENDA IN ('00091','00027') THEN
*                CASE WHEN COALESCE(ANO_MES.MONAT,'00') = '00' THEN SUBSTR(PC.DATA_ATUAL,5,2) ELSE ANO_MES.MONAT END
*              ELSE
*                CASE WHEN (COALESCE(MES_SPREAD.MONAT,'00') = '00') AND (COALESCE(MES_PREMIO.MONAT,'00') = '00') THEN
*                    SUBSTR(CB.DTDE_LOGIST,5,2)
*                ELSE
*                   CASE WHEN COALESCE(MES_SPREAD.MONAT,'00') = '00' THEN
*                          MES_PREMIO.MONAT
*                        ELSE
*                          MES_SPREAD.MONAT
*                   END
*                END
*         END AS MES_PREMIO,
*
*         'V'  AS STATUS,
*         'SP' AS TP_REGISTRO
*    FROM SAPHANADB.ZSDT0051 CB,
*         SAPHANADB.ZSDT0053 IT,
*         SAPHANADB.ZSDT0059 PC,
*         SAPHANADB.VBFA     FA,
*         ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT, O.CBOT
*             FROM SAPHANADB.ZSDT0059 O
*            WHERE O.MONAT      > '00'
*              AND O.CBOT      <> ' '  ) ANO_MES,
*         ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT
*             FROM SAPHANADB.ZSDT0059 O
*            WHERE O.BEZEI IN ('SPRED','SPREAD') ) MES_SPREAD,
*         ( SELECT DISTINCT O.NRO_SOL_OV, O.POSNR, O.MONAT
*             FROM SAPHANADB.ZSDT0059 O
*            WHERE O.BEZEI IN ('PREMIO') ) MES_PREMIO
*   WHERE CB.VTWEG      = '10'
*     AND CB.SPART      = '01'
*     AND CB.NRO_SOL_OV = IT.NRO_SOL_OV
*     AND PC.NRO_SOL_OV = IT.NRO_SOL_OV
*     AND IT.FIXACAO    = PC.POSNR
*     AND PC.BEZEI      IN ('SPRED','SPREAD')
*     AND IT.NRO_SOL_OV = ANO_MES.NRO_SOL_OV(+)
*     AND IT.FIXACAO    = ANO_MES.POSNR(+)
*     AND IT.NRO_SOL_OV = MES_SPREAD.NRO_SOL_OV(+)
*     AND IT.FIXACAO    = MES_SPREAD.POSNR(+)
*     AND IT.NRO_SOL_OV = MES_PREMIO.NRO_SOL_OV(+)
*     AND IT.FIXACAO    = MES_PREMIO.POSNR(+)
*     AND IT.VBELN      IS NOT NULL
*     AND IT.VBELN      = FA.VBELV
*     AND FA.VBTYP_N    = 'H'
*     AND FA.VBTYP_V    = 'C'
*  ENDEXEC.
  "Conversão S4 Hana - Amaggi - WPP - Fim

  DO.
    EXEC SQL.
      FETCH NEXT SPREADS INTO :WA_REGISTRO-NRO_SOL_OV,
                              :WA_REGISTRO-NR_ORDEM,
                              :WA_REGISTRO-FILIAL,
                              :WA_REGISTRO-POSNR,
                              :WA_REGISTRO-DOC_DEV,
                              :WA_REGISTRO-COD_FP,
                              :WA_REGISTRO-TP_VENDA,
                              :WA_REGISTRO-SAFRA,
                              :WA_REGISTRO-MATERIAL,
                              :WA_REGISTRO-DT_INI_EMBARQUE,
                              :WA_REGISTRO-DT_FIM_EMBARQUE,
                              :WA_REGISTRO-DT_SOLICITACAO,
                              :WA_REGISTRO-DT_VENDA,
                              :WA_REGISTRO-QUANTIDADE,
                              :WA_REGISTRO-UN_MEDIDA,
                              :WA_REGISTRO-FORMULA2,
                              :WA_REGISTRO-DT_FIXACAO_SPREAD,
                              :WA_REGISTRO-ANO_PREMIO,
                              :WA_REGISTRO-MES_PREMIO,
                              :WA_REGISTRO-STATUS,
                              :WA_REGISTRO-TP_REGISTRO
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      WA_REGISTRO-DATA_ATUAL = SY-DATUM.
      WA_REGISTRO-HORA_ATUAL = SY-UZEIT.
      MOVE WA_REGISTRO-FORMULA2 TO WA_REGISTRO-VR_BASE_SPREAD.
      APPEND WA_REGISTRO TO IT_REGISTRO.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE SPREADS
  ENDEXEC.

  CLEAR: IT_ZSDT0097[].

  EXEC SQL.
    OPEN LOGS FOR
      SELECT CB.NR_SOL_OV,
             CB.NR_ORDEM,
             CB.FILIAL,
             CB.POSNR,
             CB.DOC_DEV,
             CB.COD_FP,
             CB.TP_VENDA,
             CB.SAFRA,
             CB.MATERIAL,
             CB.DT_INI_EMBARQUE,
             CB.DT_FIM_EMBARQUE,
             CB.DT_SOLICITACAO,
             CB.DT_VENDA,
             CB.QUANTIDADE,
             CB.VR_BASE_PREMIO,
             CB.UN_MEDIDA,
             CB.VR_BASE_SPREAD,
             CB.DT_FIXACAO_PREMI,
             CB.ANO_PREMIO,
             CB.MES_PREMIO,
             CB.VR_ACIMA_EXPORTA,
             CB.TP_REGISTRO,
             CB.STATUS,
             CB.DATA_ATUAL,
             CB.HORA_ATUAL
        FROM ZSDT0097 CB
       WHERE CB.TP_REGISTRO = 'SP'
         AND TO_DATE( CB.DATA_ATUAL||' '||CB.HORA_ATUAL, 'YYYYMMDD HH24MISS' ) =
            ( SELECT MAX( TO_DATE( CB2.DATA_ATUAL||' '||CB2.HORA_ATUAL, 'YYYYMMDD HH24MISS' ) )
                FROM SAPHANADB.ZSDT0097 CB2
               WHERE CB2.NR_SOL_OV = CB.NR_SOL_OV
                 AND CB2.NR_ORDEM  = CB.NR_ORDEM
                 AND CB2.FILIAL    = CB.FILIAL
                 AND CB2.TP_VENDA  = CB.TP_VENDA
                 AND CB2.POSNR     = CB.POSNR
                 AND CB2.DOC_DEV   = CB.DOC_DEV
                 AND CB2.COD_FP    = CB.COD_FP )
  ENDEXEC.

  DO.
    EXEC SQL.
      FETCH NEXT LOGS INTO :WA_ZSDT0097-NR_SOL_OV,
                           :WA_ZSDT0097-NR_ORDEM,
                           :WA_ZSDT0097-FILIAL,
                           :WA_ZSDT0097-POSNR,
                           :WA_ZSDT0097-DOC_DEV,
                           :WA_ZSDT0097-COD_FP,
                           :WA_ZSDT0097-TP_VENDA,
                           :WA_ZSDT0097-SAFRA,
                           :WA_ZSDT0097-MATERIAL,
                           :WA_ZSDT0097-DT_INI_EMBARQUE,
                           :WA_ZSDT0097-DT_FIM_EMBARQUE,
                           :WA_ZSDT0097-DT_SOLICITACAO,
                           :WA_ZSDT0097-DT_VENDA,
                           :WA_ZSDT0097-QUANTIDADE,
                           :WA_ZSDT0097-VR_BASE_PREMIO,
                           :WA_ZSDT0097-UN_MEDIDA,
                           :WA_ZSDT0097-VR_BASE_SPREAD,
                           :WA_ZSDT0097-DT_FIXACAO_PREMI,
                           :WA_ZSDT0097-ANO_PREMIO,
                           :WA_ZSDT0097-MES_PREMIO,
                           :WA_ZSDT0097-VR_ACIMA_EXPORTA,
                           :WA_ZSDT0097-TP_REGISTRO,
                           :WA_ZSDT0097-STATUS,
                           :WA_ZSDT0097-DATA_ATUAL,
                           :WA_ZSDT0097-HORA_ATUAL
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      APPEND WA_ZSDT0097 TO IT_ZSDT0097.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE LOGS
  ENDEXEC.

  CLEAR: IT_SAIDA[].

  LOOP AT IT_REGISTRO.
    CLEAR WA_ZSDT0097.
    READ TABLE IT_ZSDT0097 INTO WA_ZSDT0097 WITH KEY NR_SOL_OV = IT_REGISTRO-NRO_SOL_OV
                                                     NR_ORDEM  = IT_REGISTRO-NR_ORDEM
                                                     FILIAL    = IT_REGISTRO-FILIAL
                                                     TP_VENDA  = IT_REGISTRO-TP_VENDA
                                                     POSNR     = IT_REGISTRO-POSNR
                                                     DOC_DEV   = IT_REGISTRO-DOC_DEV
                                                     COD_FP    = IT_REGISTRO-COD_FP.
    "Só envia o que não esta na interface, ou que teve alguma alteração
    IF NOT SY-SUBRC IS INITIAL OR
      ( SY-SUBRC IS INITIAL AND
        ( WA_ZSDT0097-QUANTIDADE       <> IT_REGISTRO-QUANTIDADE      OR
          WA_ZSDT0097-ANO_PREMIO       <> IT_REGISTRO-ANO_PREMIO      OR
          WA_ZSDT0097-MES_PREMIO       <> IT_REGISTRO-MES_PREMIO      OR
          WA_ZSDT0097-VR_BASE_SPREAD   <> IT_REGISTRO-VR_BASE_SPREAD  OR
          WA_ZSDT0097-SAFRA            <> IT_REGISTRO-SAFRA OR
          WA_ZSDT0097-DT_FIXACAO_PREMI <> IT_REGISTRO-DT_FIXACAO_SPREAD OR
          WA_ZSDT0097-STATUS           <> IT_REGISTRO-STATUS  OR
          WA_ZSDT0097-NR_ORDEM         <> IT_REGISTRO-NR_ORDEM ) ).

      CLEAR WA_ZSDT0097.

      WA_ZSDT0097-NR_SOL_OV        = IT_REGISTRO-NRO_SOL_OV.
      WA_ZSDT0097-NR_ORDEM         = IT_REGISTRO-NR_ORDEM.
      WA_ZSDT0097-FILIAL           = IT_REGISTRO-FILIAL.
      WA_ZSDT0097-DATA_ATUAL       = SY-DATUM.
      WA_ZSDT0097-HORA_ATUAL       = SY-UZEIT.
      WA_ZSDT0097-POSNR            = IT_REGISTRO-POSNR.
      WA_ZSDT0097-DOC_DEV          = IT_REGISTRO-DOC_DEV.
      WA_ZSDT0097-COD_FP           = IT_REGISTRO-COD_FP.
      WA_ZSDT0097-TP_VENDA         = IT_REGISTRO-TP_VENDA.
      WA_ZSDT0097-SAFRA            = IT_REGISTRO-SAFRA.
      WA_ZSDT0097-MATERIAL         = IT_REGISTRO-MATERIAL.
      WA_ZSDT0097-DT_INI_EMBARQUE  = IT_REGISTRO-DT_INI_EMBARQUE.
      WA_ZSDT0097-DT_FIM_EMBARQUE  = IT_REGISTRO-DT_FIM_EMBARQUE.
      WA_ZSDT0097-DT_SOLICITACAO   = IT_REGISTRO-DT_SOLICITACAO.
      WA_ZSDT0097-DT_VENDA         = IT_REGISTRO-DT_VENDA.
      WA_ZSDT0097-QUANTIDADE       = IT_REGISTRO-QUANTIDADE.
      WA_ZSDT0097-VR_BASE_SPREAD   = IT_REGISTRO-VR_BASE_SPREAD.
      WA_ZSDT0097-UN_MEDIDA        = IT_REGISTRO-UN_MEDIDA.
      WA_ZSDT0097-DT_FIXACAO_PREMI = IT_REGISTRO-DT_FIXACAO_SPREAD.
      WA_ZSDT0097-ANO_PREMIO       = IT_REGISTRO-ANO_PREMIO.
      WA_ZSDT0097-MES_PREMIO       = IT_REGISTRO-MES_PREMIO.
      WA_ZSDT0097-TP_REGISTRO      = IT_REGISTRO-TP_REGISTRO.
      WA_ZSDT0097-STATUS           = IT_REGISTRO-STATUS.
      MODIFY ZSDT0097 FROM WA_ZSDT0097.

      MOVE-CORRESPONDING IT_REGISTRO TO WA_SAIDA.
      WA_SAIDA-NR_SOL_OV         = IT_REGISTRO-NRO_SOL_OV.
      WA_SAIDA-DT_FIXACAO_PREMIO = IT_REGISTRO-DT_FIXACAO_SPREAD.
      APPEND WA_SAIDA TO IT_SAIDA.
    ENDIF.
  ENDLOOP.

  IF NOT IT_SAIDA[] IS INITIAL.
    CALL FUNCTION 'Z_SD_OUTBOUND_PNL_PREMIO' IN BACKGROUND TASK
      DESTINATION 'XI_PNL_PREMIO'
      TABLES
        T_ZSD_PNL_PREMIO = IT_SAIDA[].

    COMMIT WORK.
  ENDIF.

ENDFORM.                    " SELECIONAS_DADOS_SPREAD


*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_EXCLUIDOS
*&---------------------------------------------------------------------*
*       Selecionar dados recusas/devoluções excluidos
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS_EXCLUIDOS .

  TYPES: BEGIN OF TY_REGISTRO.
  TYPES: NRO_SOL_OV	       TYPE ZSDED013,
         NR_ORDEM          TYPE VBELN_VA,
         FILIAL            TYPE WERKS_D,
         DATA_ATUAL        TYPE DATS,
         HORA_ATUAL        TYPE TIMS,
         POSNR             TYPE POSNR_VA,
         DOC_DEV           TYPE VBELN_NACH,
         COD_FP            TYPE ZSDED019,
         TP_VENDA          TYPE ZSDED012,
         SAFRA             TYPE CHARG_D,
         MATERIAL          TYPE MATNR,
         DT_INI_EMBARQUE   TYPE VALDT,
         DT_FIM_EMBARQUE   TYPE VALDT,
         DT_SOLICITACAO    TYPE VALDT,
         DT_VENDA          TYPE VALDT,
         QUANTIDADE        TYPE BRGEW_AP,
         UN_MEDIDA         TYPE VOLEH,
         VR_BASE_PREMIO    TYPE DMBTR,
         VR_BASE_SPREAD    TYPE DMBTR,
         DT_FIXACAO_PREMIO TYPE VALDT,
         ANO_PREMIO        TYPE CHAR4,
         MES_PREMIO        TYPE CHAR2,
         STATUS            TYPE CHAR1,
         TP_REGISTRO       TYPE CHAR2.
  TYPES: END OF TY_REGISTRO.

  DATA: WA_REGISTRO TYPE TY_REGISTRO,
        IT_REGISTRO TYPE TABLE OF TY_REGISTRO WITH HEADER LINE.

  EXEC SQL.
    OPEN EXCLUIDOS FOR
  SELECT LG.NR_SOL_OV,
         LG.NR_ORDEM,
         LG.FILIAL,
         LG.POSNR,
         LG.DOC_DEV,
         LG.COD_FP,
         LG.TP_VENDA,
         LG.SAFRA,
         LG.MATERIAL,
         LG.DT_INI_EMBARQUE,
         LG.DT_FIM_EMBARQUE,
         LG.DT_SOLICITACAO,
         LG.DT_VENDA,
         LG.QUANTIDADE,
         LG.UN_MEDIDA,
         LG.VR_BASE_PREMIO,
         LG.VR_BASE_SPREAD,
         LG.DT_FIXACAO_PREMI,
         LG.ANO_PREMIO,
         LG.MES_PREMIO,
         LG.STATUS,
         LG.TP_REGISTRO AS TP_REGISTRO
    FROM ZSDT0097 LG
   WHERE LG.DOC_DEV    > 1
     AND LG.TP_REGISTRO = 'PR'
     AND LG.REVERTIDO   <> 'X'
     AND NOT EXISTS ( SELECT *
                        FROM VBFA FF
                       WHERE FF.VBELN   = LG.DOC_DEV
                         AND FF.VBTYP_N = 'H'
                         AND FF.VBTYP_V = 'C' )
UNION ALL
  SELECT LG.NR_SOL_OV,
         LG.NR_ORDEM,
         LG.FILIAL,
         LG.POSNR,
         LG.DOC_DEV,
         LG.COD_FP,
         LG.TP_VENDA,
         LG.SAFRA,
         LG.MATERIAL,
         LG.DT_INI_EMBARQUE,
         LG.DT_FIM_EMBARQUE,
         LG.DT_SOLICITACAO,
         LG.DT_VENDA,
         LG.QUANTIDADE,
         LG.UN_MEDIDA,
         LG.VR_BASE_PREMIO,
         LG.VR_BASE_SPREAD,
         LG.DT_FIXACAO_PREMI,
         LG.ANO_PREMIO,
         LG.MES_PREMIO,
         LG.STATUS AS STATUS,
         LG.TP_REGISTRO AS TP_REGISTRO
    FROM ZSDT0097 LG
   WHERE LG.DOC_DEV     > 1
     AND LG.TP_REGISTRO  = 'SP'
     AND LG.REVERTIDO   <> 'X'
     AND NOT EXISTS ( SELECT *
                        FROM VBFA FF
                       WHERE FF.VBELN   = LG.DOC_DEV
                         AND FF.VBTYP_N = 'H'
                         AND FF.VBTYP_V = 'C' )
  ENDEXEC.

  DO.
    EXEC SQL.
      FETCH NEXT EXCLUIDOS INTO :WA_REGISTRO-NRO_SOL_OV,
                                :WA_REGISTRO-NR_ORDEM,
                                :WA_REGISTRO-FILIAL,
                                :WA_REGISTRO-POSNR,
                                :WA_REGISTRO-DOC_DEV,
                                :WA_REGISTRO-COD_FP,
                                :WA_REGISTRO-TP_VENDA,
                                :WA_REGISTRO-SAFRA,
                                :WA_REGISTRO-MATERIAL,
                                :WA_REGISTRO-DT_INI_EMBARQUE,
                                :WA_REGISTRO-DT_FIM_EMBARQUE,
                                :WA_REGISTRO-DT_SOLICITACAO,
                                :WA_REGISTRO-DT_VENDA,
                                :WA_REGISTRO-QUANTIDADE,
                                :WA_REGISTRO-UN_MEDIDA,
                                :WA_REGISTRO-VR_BASE_PREMIO,
                                :WA_REGISTRO-VR_BASE_SPREAD,
                                :WA_REGISTRO-DT_FIXACAO_PREMIO,
                                :WA_REGISTRO-ANO_PREMIO,
                                :WA_REGISTRO-MES_PREMIO,
                                :WA_REGISTRO-STATUS,
                                :WA_REGISTRO-TP_REGISTRO
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      WA_REGISTRO-DATA_ATUAL = SY-DATUM.
      WA_REGISTRO-HORA_ATUAL = SY-UZEIT.
      WA_REGISTRO-STATUS     = '#'.
      APPEND WA_REGISTRO TO IT_REGISTRO.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE EXCLUIDOS
  ENDEXEC.

  CLEAR: IT_SAIDA[].

  LOOP AT IT_REGISTRO.
    MOVE-CORRESPONDING IT_REGISTRO TO WA_SAIDA.
    WA_SAIDA-NR_SOL_OV         = IT_REGISTRO-NRO_SOL_OV.

    UPDATE ZSDT0097
       SET REVERTIDO   = 'X'
     WHERE NR_SOL_OV   = IT_REGISTRO-NRO_SOL_OV
       AND NR_ORDEM    = IT_REGISTRO-NR_ORDEM
       AND FILIAL      = IT_REGISTRO-FILIAL
       AND POSNR       = IT_REGISTRO-POSNR
       AND DOC_DEV     = IT_REGISTRO-DOC_DEV
       AND COD_FP      = IT_REGISTRO-COD_FP
       AND TP_REGISTRO = IT_REGISTRO-TP_REGISTRO.

    APPEND WA_SAIDA TO IT_SAIDA.
  ENDLOOP.

  IF NOT IT_SAIDA[] IS INITIAL.
    CALL FUNCTION 'Z_SD_OUTBOUND_PNL_PREMIO' IN BACKGROUND TASK
      DESTINATION 'XI_PNL_PREMIO'
      TABLES
        T_ZSD_PNL_PREMIO = IT_SAIDA[].

    COMMIT WORK.
  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS_EXCLUIDOS
