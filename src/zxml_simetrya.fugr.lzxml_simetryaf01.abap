*----------------------------------------------------------------------*
***INCLUDE LZXML_SIMETRYAF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  MONTA_XML_NFE
*&---------------------------------------------------------------------*
*       Monta XML NFe
*----------------------------------------------------------------------*
FORM MONTA_XML_NFE  TABLES   XML_HEAD_TAB TYPE  J1B_NF_XML_HEADER_TAB
                             XML_ITEM_TAB TYPE  J1B_NF_XML_ITEM_TAB
                             XML_BATCH TYPE  J1B_NF_XML_J_TAB
                             XML_REF TYPE  J1B_NF_XML_B12_TAB
                             XML_DUP TYPE  J1B_NF_XML_U2_TAB
                             XML_VOL TYPE  J1B_NF_XML_T4_TAB
                             XML_IMP TYPE  J1B_NF_XML_H4_TAB
                             XML_EXT1 TYPE  J1B_NF_XML_EXTENSION1_TAB
                             XML_EXT2 TYPE  J1B_NF_XML_EXTENSION2_TAB
                             IT_XML STRUCTURE ZEXML
                    USING    XML_IN TYPE  J1B_NF_XML_HEADER.

  DATA: BEGIN OF WA_XML.
          INCLUDE STRUCTURE ZEXML.
        DATA: END OF WA_XML.

  DATA: BEGIN OF WA_NOTA.
          INCLUDE STRUCTURE ZOB_NOTA_FISCAL_SAP.
        DATA: END OF WA_NOTA.

  DATA: BEGIN OF WA_J_1BNFDOC.
          INCLUDE STRUCTURE J_1BNFDOC.
        DATA: END OF WA_J_1BNFDOC.

  DATA: NFE     TYPE ZXML,
        IT_NOTA LIKE STANDARD TABLE OF WA_NOTA.

  "XML NFe
  DATA: NFE01 TYPE ZXML,
        NFE02 TYPE ZXML.

  "XML Ref. NFe
  DATA: XML_REF_NFE01 TYPE ZXML,
        XML_REF_NFE02 TYPE ZXML.

  "XML Det. Export
  DATA: XML_DET_EXP01 TYPE ZXML,
        XML_DET_EXP02 TYPE ZXML,
        XML_DET_EXP03 TYPE ZXML.

  DATA: BEGIN OF WA_REMESSA,
          CH_REFERENCIA LIKE ZSDT0001-CH_REFERENCIA,
          DOC_REM       LIKE ZSDT0001-DOC_REM,
        END OF WA_REMESSA.

  DATA: BEGIN OF WA_LIN,
          REFKEY       TYPE J_1BNFLIN-REFKEY,
          REFITM       TYPE J_1BNFLIN-REFITM,
          REFTYP       TYPE J_1BNFLIN-REFTYP,
          VBELN        TYPE C LENGTH 10,
          REFKEY_VBELN TYPE VBRK-VBELN,
        END OF WA_LIN.

  DATA: IT_LIN LIKE STANDARD TABLE OF WA_LIN.

  DATA: WA_SETLEAF TYPE  SETLEAF.

  SELECT REFKEY REFITM REFTYP
    INTO CORRESPONDING FIELDS OF TABLE IT_LIN
    FROM J_1BNFLIN
   WHERE DOCNUM EQ XML_IN-DOCNUM.

  LOOP AT IT_LIN INTO WA_LIN.
    WA_LIN-VBELN = WA_LIN-REFKEY(10).
    CLEAR WA_REMESSA.
    CASE WA_LIN-REFTYP.
      WHEN 'BI'.
        SELECT SINGLE RE~CH_REFERENCIA L~VBELN
          INTO WA_REMESSA
          FROM VBFA AS V
         INNER JOIN LIPS AS L ON L~VBELN = V~VBELV
         INNER JOIN ZSDT0001 AS RE ON RE~DOC_REM = L~VBELN
         WHERE V~VBELN         EQ WA_LIN-VBELN
           AND RE~TP_MOVIMENTO EQ 'S'
           AND V~VBTYP_N       EQ 'M'
           AND V~VBTYP_V       EQ 'J'. "Entrega via Faturamento
      WHEN 'MD'.
        SELECT SINGLE RE~CH_REFERENCIA L~VBELN
          INTO WA_REMESSA
          FROM VBFA AS V
         INNER JOIN LIPS AS L ON L~VBELN = V~VBELV
         INNER JOIN ZSDT0001 AS RE ON RE~DOC_REM = L~VBELN
         WHERE V~VBELN         EQ WA_LIN-VBELN
           AND RE~TP_MOVIMENTO EQ 'S'
           AND V~VBTYP_N       EQ 'R'
           AND V~VBTYP_V       EQ 'J'.  "Entrega via documento de material
    ENDCASE.
  ENDLOOP.

  CLEAR: VG_ZONA_FRANCA. ", VAR_SUFRAMA.

  CALL FUNCTION 'Z_XML_VERSAO_2'
    IMPORTING
      XML_VERSAO = VG_XML_VERSAO.

  "Autorização de Nota Fiscal
  IF XML_EXT2[] IS INITIAL.

    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF WA_J_1BNFDOC
      FROM J_1BNFDOC
     WHERE DOCNUM EQ XML_IN-DOCNUM.

    PERFORM ABREXML  USING NFE.
    "Identificação
    PERFORM IDE      TABLES XML_REF XML_ITEM_TAB USING NFE XML_IN WA_LIN-VBELN WA_LIN-REFKEY XML_REF_NFE01 XML_REF_NFE02 .
    "Emitente
    PERFORM EMIT     USING NFE XML_IN.
    "Destinatário
    PERFORM DEST     TABLES XML_ITEM_TAB USING NFE XML_IN WA_J_1BNFDOC .
    "Retirada
    PERFORM RETIRADA USING NFE XML_IN.
    "Entrega
    PERFORM ENTREGA USING NFE XML_IN.
    "Item da Nota fiscal
    PERFORM DET TABLES XML_ITEM_TAB XML_IMP USING NFE WA_LIN-REFKEY XML_IN XML_DET_EXP01 XML_DET_EXP02 XML_DET_EXP03.
    "Totais
    PERFORM TOTAIS USING NFE XML_IN.
    "Informações da Transportadora
    PERFORM TRANSP TABLES XML_VOL XML_ITEM_TAB USING NFE XML_IN .
    "Informações da Cobrança
    PERFORM COBR TABLES XML_DUP USING NFE XML_IN.
    "Informações de Pagamento
    PERFORM PAG USING NFE XML_IN.
    "Informações da Cobrança
    PERFORM INFADIC USING NFE XML_IN.
    "Informações de exportação
    PERFORM EXPORTA USING NFE XML_IN.
    "Informações de Compra
    PERFORM COMPRA USING NFE XML_IN.

    PERFORM FECHAXML USING NFE.

    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN NFE WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN NFE WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'í'     IN NFE WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN NFE WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN NFE WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN NFE WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'     IN NFE WITH '&#38;'.
    REPLACE ALL OCCURRENCES OF        ''''    IN NFE WITH '&#39;'.
    REPLACE ALL OCCURRENCES OF        'º'     IN NFE WITH 'o' IGNORING CASE.

    WA_XML-XML = NFE.
    APPEND WA_XML TO IT_XML.

    WA_NOTA-TP_AUTHCOD       = '1'.
    WA_NOTA-NU_DOCUMENTO_SAP = XML_IN-DOCNUM.
    WA_NOTA-ID_EMPRESA       = WA_J_1BNFDOC-BUKRS.
    WA_NOTA-ID_FILIAL        = WA_J_1BNFDOC-BRANCH.
    WA_NOTA-TB_DIRECAO       = WA_J_1BNFDOC-DIRECT.
    WA_NOTA-NR_NFE           = XML_IN-NNF.
    WA_NOTA-SR_NFE           = XML_IN-SERIE.
    WA_NOTA-DT_EMISSAO       = WA_J_1BNFDOC-DOCDAT.
    WA_NOTA-ID_DESTINATARIO  = WA_J_1BNFDOC-PARID.
    WA_NOTA-NU_DOCUMENTO_REM = WA_REMESSA-DOC_REM.
    WA_NOTA-CH_REFERENCIA    = WA_REMESSA-CH_REFERENCIA.

    "Cancelamento de Nota Fiscal
  ELSE.

    DATA: MSGCANCELAMENTO TYPE J1B_NF_XML_EXTENSION2.

    READ TABLE XML_EXT2[] INTO MSGCANCELAMENTO INDEX 1.

    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF WA_J_1BNFDOC
      FROM J_1BNFDOC
     WHERE DOCNUM EQ MSGCANCELAMENTO-DOCNUM.

    PERFORM CANCELARNFE TABLES XML_EXT2 USING XML_IN NFE WA_J_1BNFDOC.

    WA_NOTA-TP_AUTHCOD       = '2'.
    WA_NOTA-NU_DOCUMENTO_SAP = WA_J_1BNFDOC-DOCNUM.
    WA_NOTA-ID_EMPRESA       = WA_J_1BNFDOC-BUKRS.
    WA_NOTA-ID_FILIAL        = WA_J_1BNFDOC-BRANCH.
    WA_NOTA-TB_DIRECAO       = WA_J_1BNFDOC-DIRECT.
    WA_NOTA-NR_NFE           = WA_J_1BNFDOC-NFENUM.
    WA_NOTA-SR_NFE           = WA_J_1BNFDOC-SERIES.
    WA_NOTA-DT_EMISSAO       = WA_J_1BNFDOC-DOCDAT.
    WA_NOTA-ID_DESTINATARIO  = WA_J_1BNFDOC-PARID.
    WA_NOTA-NU_DOCUMENTO_REM = WA_REMESSA-DOC_REM.
    WA_NOTA-CH_REFERENCIA    = WA_REMESSA-CH_REFERENCIA.

    WA_XML-XML = NFE.
    APPEND WA_XML TO IT_XML.

  ENDIF.

  SELECT SINGLE * FROM SETLEAF INTO WA_SETLEAF WHERE SETNAME EQ 'ZSALVA_XML_STATUS'
                                                 AND VALFROM EQ '1'.
  IF ( SY-SUBRC EQ 0 ) .

    DATA: XML_VAR TYPE ZEXML-XML.

    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN WA_XML WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN WA_XML WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'í'     IN WA_XML WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN WA_XML WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN WA_XML WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN WA_XML WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'     IN WA_XML WITH '&#38;'.
    REPLACE ALL OCCURRENCES OF        ''''    IN WA_XML WITH '&#39;'.
    REPLACE ALL OCCURRENCES OF        'º'     IN WA_XML WITH 'o' IGNORING CASE.
    XML_VAR = WA_XML.

    " Função para salvar arquivo XML de NFe
    CALL FUNCTION 'Z_SALVA_XML'
      EXPORTING
        P_XML    = XML_VAR
        P_DOCNUM = WA_J_1BNFDOC-DOCNUM
        P_TIPO   = '1'.

  ENDIF.

  CALL FUNCTION 'EFG_GEN_GET_USER_EMAIL'
    EXPORTING
      I_UNAME         = SY-UNAME
    IMPORTING
      E_EMAIL_ADDRESS = WA_NOTA-DS_EMAIL.

  IF WA_NOTA-DS_EMAIL IS INITIAL.
    WA_NOTA-DS_EMAIL = 'indiretos.fiscal@grupomaggi.com.br'.
  ENDIF.


  IF ( XML_REF_NFE01 IS NOT INITIAL ).

    PERFORM CONC_XML_REF_DET USING NFE
                                   WA_NOTA
                                   NFE01 NFE02
                                   XML_REF_NFE01 XML_REF_NFE02
                                   XML_DET_EXP01 XML_DET_EXP02.

  ELSE.

    WA_NOTA-TX_XML    = NFE(4000).
    WA_NOTA-TX_XML2   = NFE+04000(4000).
    WA_NOTA-TX_XML3   = NFE+08000(4000).
    WA_NOTA-TX_XML4   = NFE+12000(4000).
    WA_NOTA-TX_XML5   = NFE+16000(4000).
    WA_NOTA-TX_XML6   = NFE+20000(4000).
    WA_NOTA-TX_XML7   = NFE+24000(4000).
    WA_NOTA-TX_XML8   = NFE+28000(2000).

  ENDIF.

  APPEND WA_NOTA TO IT_NOTA.

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(WL_SETLEAF)
   WHERE SETNAME EQ 'MAGGI_CTG_NFE_SAP'.

  IF ( SY-SUBRC = 0 ) AND ( WL_SETLEAF-VALFROM IS NOT INITIAL ).

    DATA: WL_ZOB_NFE_SAP TYPE ZOB_NFE_SAP.

    MOVE-CORRESPONDING WA_NOTA TO WL_ZOB_NFE_SAP.

    IF ( XML_REF_NFE01 IS NOT INITIAL ).
      CALL FUNCTION 'Z_GRAVAR_XML_NFE_CTE'
        EXPORTING
          I_XML01       = NFE01
          I_XML02       = NFE02
          I_TIPO        = '1' "NF-e
        CHANGING
          I_ZOB_NFE_SAP = WL_ZOB_NFE_SAP.
    ELSE.
      CALL FUNCTION 'Z_GRAVAR_XML_NFE_CTE'
        EXPORTING
          I_XML01       = NFE
          I_TIPO        = '1' "NF-e
        CHANGING
          I_ZOB_NFE_SAP = WL_ZOB_NFE_SAP.
    ENDIF.

  ELSE.

    CALL FUNCTION 'Z_SD_OUTBOUND_NFE_XML' IN BACKGROUND TASK
      DESTINATION 'XI_XML'
      AS SEPARATE UNIT
      TABLES
        IT_SAIDA = IT_NOTA.

  ENDIF.

  COMMIT WORK.

ENDFORM.                    " MONTA_XML_NFE

*&---------------------------------------------------------------------*
*&      Form  MONTA_XML_CTE
*&---------------------------------------------------------------------*
*       Monta XML CTe
*----------------------------------------------------------------------*
FORM MONTA_XML_CTE  TABLES   XML_HEAD_TAB TYPE  J1B_NF_XML_HEADER_TAB
                             XML_ITEM_TAB TYPE  J1B_NF_XML_ITEM_TAB
                             XML_BATCH TYPE  J1B_NF_XML_J_TAB
                             XML_REF TYPE  J1B_NF_XML_B12_TAB
                             XML_DUP TYPE  J1B_NF_XML_U2_TAB
                             XML_VOL TYPE  J1B_NF_XML_T4_TAB
                             XML_IMP TYPE  J1B_NF_XML_H4_TAB
                             XML_EXT1 TYPE  J1B_NF_XML_EXTENSION1_TAB
                             XML_EXT2 TYPE  J1B_NF_XML_EXTENSION2_TAB
                             IT_XML STRUCTURE ZEXML
                    USING    XML_IN TYPE  J1B_NF_XML_HEADER.

  DATA: BEGIN OF WA_XML.
          INCLUDE STRUCTURE ZEXML.
        DATA: END OF WA_XML.

  DATA: BEGIN OF WA_CONHEC.
          INCLUDE STRUCTURE ZOB_CONHECIMENTO_SAP.
        DATA: END OF WA_CONHEC.

  DATA: BEGIN OF WA_J_1BNFDOC.
          INCLUDE STRUCTURE J_1BNFDOC.
        DATA: END OF WA_J_1BNFDOC.

  DATA: CTE       TYPE ZXML,
        IT_CONHEC LIKE STANDARD TABLE OF WA_CONHEC.

  CALL FUNCTION 'Z_XML_CTE_1_04'
    IMPORTING
      P_INICIADA = XML_CTE_1_04.

  "Autorização de Nota Fiscal
  IF XML_EXT2[] IS INITIAL.

    "Obter os dados da nota fiscal
    PERFORM Z_OBTER_DADOS USING XML_IN-DOCNUM.

    " Implementação das Tag da CTE
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    PERFORM CTEABREXML    USING CTE.

    "Identificação CTe
    PERFORM CTEIDE        USING CTE.
    "Textos fiscais
    PERFORM CTECOMPL      USING CTE XML_IN.
    "Textos complementares
    PERFORM CTEOBSCONT    USING CTE.
    "Emitente do CT-e
    PERFORM CTEEMIT       USING CTE.
    "Remetente das mercadorias transportadas pelo CT-e
    PERFORM CTEREM        USING CTE.
    "Endereço do remetente das mercadorias transportadas pelo CT-eq
    PERFORM CTEENDREM     USING CTE.
    "Informações da NFe
    PERFORM CTENFE        USING CTE.
    "Informações do destinatário das mercadorias transportadas pelo CT-e
    PERFORM CTEDESTMER    USING CTE.
    "Informações do endereçodo destinatário das mercadorias transportadas pelo CT-e
    PERFORM CTEENDDESTM   USING CTE.
    "Informações de valores da prestação de serviço
    PERFORM CTEVLPRESTSRV USING CTE.
    "Informações de componentes dos valores
*  PERFORM ctecomp       USING cte.
    "Informações de impostos
    PERFORM CTEIMP        USING CTE.
    "Informações do conhecimento de transporte
    PERFORM CTEINFCT      USING CTE.
    "Informações de Seguro
    PERFORM CTESEG        USING CTE ' '.
    "Informações da carga
    PERFORM CTECARGA      USING CTE.
    "Informações de quantidades da carga do CT-e
*  PERFORM cteqtde       USING cte.
    "Informações do modal rodoviário
    PERFORM CTEMODAL      USING CTE.
* Sempre montar tag de lotação (dados do veículo e seu proprietário)
*  IF st_vttk-sdabw = c_0002.
    "Informações dos veículos
    PERFORM CTEVEICULOS        USING CTE.
    "Informações de proprietários dos veículos
*    PERFORM cteproprietarios   USING cte.
*  ENDIF.
    "Informações do motorista
    PERFORM CTEMOTORISTA       USING CTE.
    "Informações de Vale Pedágio
    PERFORM CTEVALEPED         USING CTE.
    "Informações do modal rodoviário (fechar a tag)
    PERFORM CTEMODAL_FECHA       USING CTE.
    "Informações do conhecimento de transporte (fechar a tag)
    PERFORM CTEINFCT_FECHA       USING CTE.

    PERFORM CTEFECHAXML          USING CTE.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

* Retirar acentuação
    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]'  IN CTE WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éêè]'   IN CTE WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[íì]'    IN CTE WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óòô]'   IN CTE WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üúù]'   IN CTE WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'ç'      IN CTE WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'ñ'      IN CTE WITH 'n' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'      IN CTE WITH '&amp;'.
    "REPLACE ALL OCCURRENCES OF        '"'      IN cte WITH '&quot;'.
    REPLACE ALL OCCURRENCES OF        ''''     IN CTE WITH '&#39;'.
    REPLACE ALL OCCURRENCES OF        'º'      IN CTE WITH 'o' IGNORING CASE.

    CLEAR WA_CONHEC.
    WA_CONHEC-TP_AUTHCOD       = '1'.
    WA_CONHEC-NU_DOCUMENTO_SAP = XML_IN-DOCNUM.
    WA_CONHEC-ID_EMPRESA       = WK_HEADER-BUKRS.
    WA_CONHEC-ID_FILIAL        = WK_HEADER-BRANCH.
    WA_CONHEC-TB_DIRECAO       = WK_HEADER-DIRECT.
    WA_CONHEC-NR_NFE           = WK_HEADER-NFENUM.
    WA_CONHEC-SR_NFE           = WK_HEADER-SERIES.
    WA_CONHEC-DATA_HORA        = VG_DT_HORA.
    WA_CONHEC-ID_DESTINATARIO  = WK_HEADER-PARID.

    CALL FUNCTION 'EFG_GEN_GET_USER_EMAIL'
      EXPORTING
        I_UNAME         = SY-UNAME
      IMPORTING
        E_EMAIL_ADDRESS = WA_CONHEC-DS_EMAIL.

    IF WA_CONHEC-DS_EMAIL IS INITIAL.
      WA_CONHEC-DS_EMAIL = 'indiretos.fiscal@grupomaggi.com.br'.
    ENDIF.

    WA_XML-XML = CTE.
    APPEND WA_XML TO IT_XML.

  ELSE.
    DATA ST_XML_EXT2 TYPE J1B_NF_XML_EXTENSION2.
    READ TABLE XML_EXT2 INTO ST_XML_EXT2 INDEX 1.

    "Obter os dados da nota fiscal
    PERFORM Z_OBTER_DADOS USING ST_XML_EXT2-DOCNUM.

    "Cancelamento
    DATA: MSGCANCELAMENTO TYPE J1B_NF_XML_EXTENSION2,
          XML_IN1         TYPE J1B_NF_XML_HEADER.

    XML_IN1-C_CNPJ  = ST_TDLNR-STCD1.
    XML_IN1-C1_IE   = ST_TDLNR-STCD3.
    XML_IN1-NNF     = WK_HEADER-NFENUM.
    XML_IN1-SERIE   = WK_HEADER-SERIES.

    CONCATENATE WK_HEADER-CREDAT(4)       "ano
                '-'
                WK_HEADER-CREDAT+4(2)     "mês
                '-'
                WK_HEADER-CREDAT+6(2)     "dia
                '*'
                WK_HEADER-CRETIM(2)       "hora
                ':'
                WK_HEADER-CRETIM+2(2)     "minuto
                ':'
                WK_HEADER-CRETIM+4(2)     "segundo
                INTO VG_DT_HORA.
    REPLACE '*' WITH SPACE INTO VG_DT_HORA.

    READ TABLE XML_EXT2[] INTO MSGCANCELAMENTO INDEX 1.

    PERFORM CANCELARCTE TABLES XML_EXT2 USING XML_IN1 CTE VG_DT_HORA.

    WA_CONHEC-TP_AUTHCOD       = '2'.
    WA_CONHEC-NU_DOCUMENTO_SAP = ST_XML_EXT2-DOCNUM.
    WA_CONHEC-ID_EMPRESA       = WK_HEADER-BUKRS.
    WA_CONHEC-ID_FILIAL        = WK_HEADER-BRANCH.
    WA_CONHEC-TB_DIRECAO       = WK_HEADER-DIRECT.
    WA_CONHEC-NR_NFE           = WK_HEADER-NFENUM.
    WA_CONHEC-SR_NFE           = WK_HEADER-SERIES.
    WA_CONHEC-DATA_HORA        = VG_DT_HORA.
    WA_CONHEC-ID_DESTINATARIO  = WK_HEADER-PARID.

    WA_XML-XML = CTE.
    APPEND WA_XML TO IT_XML.

  ENDIF.

  WA_CONHEC-TX_XML  = CTE(4000).
  WA_CONHEC-TX_XML2 = CTE+4000(4000).
  WA_CONHEC-TX_XML3 = CTE+8000(4000).

  APPEND WA_CONHEC TO IT_CONHEC.

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(WL_SETLEAF)
   WHERE SETNAME EQ 'MAGGI_CTG_CTE_SAP'.

  IF ( SY-SUBRC = 0 ) AND ( WL_SETLEAF-VALFROM IS NOT INITIAL ).

    DATA: WL_ZOB_CTE_SAP TYPE ZOB_CTE_SAP.

    MOVE-CORRESPONDING WA_CONHEC TO WL_ZOB_CTE_SAP.

    CALL FUNCTION 'Z_GRAVAR_XML_NFE_CTE'
      EXPORTING
        I_XML01       = CTE
        I_TIPO        = '2' "CT-e
      CHANGING
        I_ZOB_CTE_SAP = WL_ZOB_CTE_SAP.

  ELSE.

    CALL FUNCTION 'Z_SD_OUTBOUND_CTE_XML' IN BACKGROUND TASK
      DESTINATION 'XI_XML_CTE'
      AS SEPARATE UNIT
      TABLES
        IT_SAIDA = IT_CONHEC.

  ENDIF.

  COMMIT WORK.


ENDFORM.                    " MONTA_XML_CTE


*&---------------------------------------------------------------------*
*&      Form  CTNAB
*&---------------------------------------------------------------------*
*       function de abertura de tag
*----------------------------------------------------------------------*
FORM CTNAB  USING VALUE(TAG) XML.
  CONCATENATE XML '<' TAG '>' SPACE INTO XML.
ENDFORM. " CTNAB

*&---------------------------------------------------------------------*
*&      Form  CTNAB2
*&---------------------------------------------------------------------*
*       function de abertura de tag
*----------------------------------------------------------------------*
FORM CTNAB2  USING VALUE(TAG) VALUE(TAG2) VALUE(CONTEUDO) XML.
  DATA: XTAG1 TYPE STRING,
        XTAG2 TYPE STRING.
  CONCATENATE '<' TAG INTO XTAG1.
  CONCATENATE TAG2 '=' '"' CONTEUDO '"' '>' INTO XTAG2.
  CONCATENATE XML XTAG1 INTO XML.
  CONCATENATE XML XTAG2 INTO XML SEPARATED BY SPACE.
ENDFORM.                                                    " CTNAB2

*&---------------------------------------------------------------------*
*&      Form  CTNAF
*&---------------------------------------------------------------------*
*       function de abertura e fechamento
*----------------------------------------------------------------------*
FORM CTNAF  USING  VALUE(TAG) VALUE(CONTEUDO) XML.
  IF CONTEUDO IS NOT INITIAL.
    CONCATENATE XML '<' TAG '>' CONTEUDO '</' TAG '>' INTO XML.
  ENDIF.
ENDFORM. " CTNAF

*&---------------------------------------------------------------------*
*&      Form  CTNAFV
*&---------------------------------------------------------------------*
*       function de abertura e fechamento valor
*----------------------------------------------------------------------*
FORM CTNAFV  USING   VALUE(TAG) VALUE(VALOR) XML.
  DATA: XVALOR TYPE STRING.
  IF VALOR IS NOT INITIAL.
    XVALOR = VALOR.
    CONCATENATE XML '<' TAG '>' XVALOR '</' TAG '>' INTO XML.
  ELSEIF ( VALOR IS INITIAL  ) AND TAG EQ 'vTotTrib'.
    XVALOR = VALOR.
    CONCATENATE XML '<' TAG '>' '0' '</' TAG '>' INTO XML.
  ENDIF.
ENDFORM.                    " CTNAFV


*&---------------------------------------------------------------------*
*&      Form  CTNFE
*&---------------------------------------------------------------------*
*       function de fechamento de tag
*----------------------------------------------------------------------*
FORM CTNFE  USING VALUE(TAG) XML.
  CONCATENATE XML '</' TAG '>' INTO XML.
ENDFORM. " CTNFE

*&---------------------------------------------------------------------*
*&      Form  CTNAV
*&---------------------------------------------------------------------*
*       function de fechamento de campo obrigatorio
*----------------------------------------------------------------------*
FORM CTNAV  USING  VALUE(TAG) VALUE(CONTEUDO) XML.
  CONCATENATE XML '<' TAG '>' CONTEUDO '</' TAG '>' INTO XML.
ENDFORM.                    " CTNAV

*&---------------------------------------------------------------------*
*&      Form  CTNAVN
*&---------------------------------------------------------------------*
*       Function de fechamento d campo numérico obrigatorio
*----------------------------------------------------------------------*
FORM CTNAVN  USING VALUE(TAG) VALOR XML.
  DATA: XVALOR TYPE STRING.
  IF VALOR IS NOT INITIAL.
    XVALOR = VALOR.
    CONCATENATE XML '<' TAG '>' XVALOR '</' TAG '>' INTO XML.
  ELSE.
    CONCATENATE XML '<' TAG '>' '0' '</' TAG '>' INTO XML.
  ENDIF.
ENDFORM.                    " CTNAVN

*&---------------------------------------------------------------------*
*&      Form  CTNAO
*&---------------------------------------------------------------------*
*       Acrescentar somente o texto na xml, sem a tag.
*----------------------------------------------------------------------*
FORM CTNAO  USING    VALUE(CONTEUDO) XML.
  CONCATENATE XML CONTEUDO INTO XML SEPARATED BY SPACE.
ENDFORM.                    " CTNAO

*&---------------------------------------------------------------------*
*&      Form  CTNAOS
*&---------------------------------------------------------------------*
*       Acrescentar somente o texto na xml, sem a tag.
*----------------------------------------------------------------------*
FORM CTNAOS  USING    VALUE(CONTEUDO) XML.
  CONCATENATE XML CONTEUDO INTO XML.
ENDFORM.                    " CTNAO

*&---------------------------------------------------------------------*
*&      Form  ABREXML
*&---------------------------------------------------------------------*
FORM ABREXML  USING    P_NFE.

  PERFORM CTNAB USING PADRAO  P_NFE.
  PERFORM CTNAB USING INTGNFE P_NFE.

ENDFORM.                    " ABREXML

*&---------------------------------------------------------------------*
*&      Form  FECHAXML
*&---------------------------------------------------------------------*
FORM FECHAXML  USING    P_NFE.

  PERFORM CTNFE USING INTGNFE P_NFE.

ENDFORM.                    " FECHAXML

*&---------------------------------------------------------------------*
*&      Form  IDE
*&---------------------------------------------------------------------*
FORM IDE  TABLES TBREF   TYPE J1B_NF_XML_B12_TAB
                 TBITENS TYPE J1B_NF_XML_ITEM_TAB
          USING  P_NFE
                 TB        TYPE J1B_NF_XML_HEADER
                 VBELN     TYPE VBELN_VF
                 P_REFKEY  TYPE J_1BREFKEY
                 P_XML_REF_NFE01 TYPE ZXML
                 P_XML_REF_NFE02 TYPE ZXML.

  DATA: XDEMI     TYPE C LENGTH 30,
        XDEMI_AUX TYPE C LENGTH 30,
        XDSAI     TYPE C LENGTH 30,
        XHSAI     TYPE C LENGTH 08.

  DATA: WA_ITNS         TYPE J1B_NF_XML_ITEM.


  "Recuperar qual é o CFOP Atribuido no ITEM da Nota Fiscal Eletrônica.
  READ TABLE TBITENS INTO WA_ITNS INDEX 1.

  DATA: OBJ_UTIL TYPE REF TO ZCL_UTIL,
        DATA_BR  TYPE C LENGTH 10,
        DATA_US  TYPE C LENGTH 10,
        HORA     TYPE C LENGTH 10,
        TZD      TYPE C LENGTH 10.

  DATA: LW_TTZZ TYPE TTZZ.
  DATA: LW_TTZR TYPE TTZR.

  CREATE OBJECT OBJ_UTIL.

  OBJ_UTIL->CONV_DATA_US_BR( EXPORTING I_DATA = SY-DATLO
                             RECEIVING E_DATA = DATA_BR ).

  OBJ_UTIL->CONV_DATA_BR_US( EXPORTING   I_DATA  = DATA_BR
                                         I_OPCAO = '-'
                             RECEIVING   E_DATA = DATA_US ).

  SELECT SINGLE * FROM TTZZ INTO LW_TTZZ WHERE TZONE    EQ SY-ZONLO.
  SELECT SINGLE * FROM TTZR INTO LW_TTZR WHERE ZONERULE EQ LW_TTZZ-ZONERULE.

  CONCATENATE SY-TIMLO(2) ':' SY-TIMLO+2(2) ':' SY-TIMLO+4(2)  INTO HORA.

  "Ini. CS2017002043 04.10.2017
  SELECT SINGLE *
    FROM J_1BNFDOC INTO @DATA(_WL_DOC)
   WHERE DOCNUM = @TB-DOCNUM.

  IF ( SY-SUBRC = 0 ) AND ( TB-DOCNUM IS NOT INITIAL ).
    DATA: V_TIME_BR TYPE ERZET.
    CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
      EXPORTING
        I_BUKRS  = _WL_DOC-BUKRS
        I_BRANCH = _WL_DOC-BRANCH
      IMPORTING
        E_TIME   = V_TIME_BR.
    IF V_TIME_BR IS NOT INITIAL.
      CONCATENATE V_TIME_BR(2) ':' V_TIME_BR+2(2) ':' V_TIME_BR+4(2)  INTO HORA.
    ENDIF.
  ENDIF.
  "Fim. CS2017002043 04.10.2017

  CONCATENATE LW_TTZR-UTCDIFF(2) ':' LW_TTZR-UTCDIFF+2(2)      INTO TZD.

  CONCATENATE TB-DEMI(4) '-' TB-DEMI+4(2) '-' TB-DEMI+6(2) INTO XDEMI_AUX.
  CONCATENATE XDEMI_AUX HORA INTO XDEMI SEPARATED BY SPACE.
  CONCATENATE XDEMI_AUX HORA INTO XDSAI SEPARATED BY SPACE.


  PERFORM CTNAB USING IDE P_NFE.
  PERFORM CTNAV USING NATOP   TB-NATOP  P_NFE.
  IF VG_VERSAO_NFE < 4.
    PERFORM CTNAV USING INDPAG  TB-INDPAG P_NFE. "NF-e 4.0 - Comentado 13.04.2018
  ENDIF.
  PERFORM CTNAV USING SERIE   TB-SERIE  P_NFE.
  PERFORM CTNAV USING NNF     TB-NNF    P_NFE.
  PERFORM CTNAV USING DEMI    XDEMI     P_NFE.
  PERFORM CTNAV USING TPNF    TB-TPNF   P_NFE.

  "TAG - idDest - Indetificador de local de destino da operação.

  " 1 - Operação Interna
  " 2 - Operação Interestadual
  " 3 - Operação com Exterior.
  CASE WA_ITNS-CFOP(1).
    WHEN: '1' OR '5'.
      PERFORM CTNAF USING DESTIDDEST '1' P_NFE.
    WHEN: '2' OR '6'.
      PERFORM CTNAF USING DESTIDDEST '2' P_NFE.
    WHEN: '3' OR '7'.
      PERFORM CTNAF USING DESTIDDEST '3' P_NFE.
  ENDCASE.

  PERFORM CTNAV USING CMUNFG  TB-CMUNFG P_NFE.
  PERFORM CTNAF USING FINNFE  TB-FINNFE P_NFE.
  PERFORM CTNAF USING DSAI    XDSAI     P_NFE.
  "PERFORM CTNAF USING HSAIENT XHSAI     P_NFE.

  IF ( TB-E1_IE IS INITIAL ).
    PERFORM CTNAF USING INDFINAL '1'   P_NFE. " Não Contribuinte, que pod eou não possuir inscrição estadual no cadastro de Contribuinte do ICMS
  ELSE.
    "Atribuir o valor 0 - Normal (Layout da Nf-e 3.1)
    PERFORM CTNAF USING INDFINAL '0'   P_NFE.
  ENDIF.

  "Atribuir o valor 9 - Operação não presencial, outros. (Layout da Nf-e 3.1)
  PERFORM CTNAF USING INDPRES '9'   P_NFE.

  IF TB-FINNFE NE '1'.
    PERFORM REFERENCIADAS TABLES TBREF USING P_NFE TB P_REFKEY P_XML_REF_NFE01 P_XML_REF_NFE02.
  ELSE.

    PERFORM REFERENCIADAS TABLES TBREF USING P_NFE TB P_REFKEY P_XML_REF_NFE01 P_XML_REF_NFE02.

    "Auteração para XML versão 2.0
    IF ( NOT VBELN IS INITIAL ) AND ( VG_XML_VERSAO EQ 2 ).
      PERFORM NOTAS_PRODUTOR USING P_NFE VBELN.
    ENDIF.

  ENDIF.


  PERFORM CTNFE USING IDE P_NFE.
  PERFORM AUTORIZACAO_DOWN_XML USING TB-C_CNPJ P_NFE.

ENDFORM.                    " IDE

*&---------------------------------------------------------------------*
*&      Form  REFERENCIADAS
*&---------------------------------------------------------------------*
FORM REFERENCIADAS TABLES TBREF TYPE J1B_NF_XML_B12_TAB
                   USING  P_NFE
                          TB        TYPE J1B_NF_XML_HEADER
                          P_REFKEY  TYPE J_1BREFKEY
                          P_XML_REF_NFE01 TYPE ZXML
                          P_XML_REF_NFE02 TYPE ZXML.

  DATA WA_REF TYPE J1B_NF_XML_B12.

  DATA: LT_ZSDT_RETLOTE TYPE TABLE OF ZSDT_RETLOTE,
        LW_ZSDT_RETLOTE TYPE ZSDT_RETLOTE.
  DATA: LW_CHAVE TYPE C LENGTH 44.

  DATA: LT_ZDOC_EXP TYPE TABLE OF ZDOC_EXP,
        LW_ZDOC_EXP TYPE ZDOC_EXP.

  DATA: LT_VBRK TYPE TABLE OF VBRK,
        LT_VBFA TYPE TABLE OF VBFA.

  DATA: WA_INFO_V1  TYPE LFA1,
        WA_INFO_C1  TYPE KNA1,
        WA_DOC_PROD TYPE J_1BNFDOC,
        WA_LIN_REF  TYPE J_1BNFLIN.

  DATA: LT_ZNOM_REME_NOTAS TYPE TABLE OF ZNOM_REME_NOTAS,
        LW_ZNOM_REME_NOTAS TYPE ZNOM_REME_NOTAS.

  DATA: XML_REF_NFE  TYPE ZXML.

  DATA: LOBJ_UTIL TYPE REF TO ZCL_UTIL.

  CLEAR XML_REF_NFE.

  CASE TB-FINNFE. "Finalidade da Emissão da NF-e.

    WHEN: '1'. "NF-e normal

      PERFORM  NOTAS_REFERENCIADAS USING P_REFKEY TB-FINNFE TB-DOCNUM P_XML_REF_NFE01 P_XML_REF_NFE02.

    WHEN OTHERS.

      SELECT * FROM ZSDT_RETLOTE
          INTO TABLE LT_ZSDT_RETLOTE
        WHERE DOCNUM_RET EQ TB-DOCNUM.

      IF ( SY-SUBRC EQ 0 ).
        PERFORM NOTAS_REFERENCIADAS USING P_REFKEY TB-FINNFE TB-DOCNUM P_XML_REF_NFE01 P_XML_REF_NFE02.
      ELSE.

        LOOP AT TBREF INTO WA_REF.
          PERFORM CTNAB USING NFREF P_NFE.
          IF WA_REF-B12_REFNFE IS NOT INITIAL.
            "PERFORM CTNAB USING REFNF P_NFE.
            PERFORM CTNAF USING REFNFE WA_REF-B12_REFNFE P_NFE.
            "PERFORM CTNFE USING REFNF P_NFE.
          ELSE.

            CLEAR: WA_INFO_V1, WA_INFO_C1, WA_DOC_PROD, WA_LIN_REF.

            PERFORM CTNAB USING REFNFP P_NFE.

            SELECT SINGLE *
              FROM J_1BNFLIN INTO WA_LIN_REF
             WHERE DOCNUM = TB-DOCNUM
               AND DOCREF NE '0000000000'.

            IF ( SY-SUBRC = 0 ) AND ( WA_LIN_REF-DOCREF IS NOT INITIAL ).
              SELECT SINGLE * INTO WA_DOC_PROD
                FROM J_1BNFDOC
               WHERE DOCNUM EQ WA_LIN_REF-DOCREF.

              IF SY-SUBRC = 0.
                CALL FUNCTION 'Z_PARCEIRO_INFO'
                  EXPORTING
                    P_PARCEIRO   = WA_DOC_PROD-PARID
                    P_PARTYPE    = WA_DOC_PROD-PARTYP
                  CHANGING
                    WA_INFO_PART = WA_INFO_V1
                    WA_INFO_C    = WA_INFO_C1.
              ENDIF.
            ENDIF.

            PERFORM CTNAF USING CUF       WA_REF-B12_CUF   P_NFE.
            PERFORM CTNAF USING REFAAMM   WA_REF-B12_AAMM  P_NFE.

            IF WA_DOC_PROD-PARTYP EQ 'V'.

              IF WA_INFO_V1-STKZN IS INITIAL.
                PERFORM CTNAF USING REFNFPCNPJ   WA_INFO_V1-STCD1  P_NFE.
              ELSE.
                PERFORM CTNAF USING REFNFPCPF    WA_INFO_V1-STCD2  P_NFE.
              ENDIF.

              PERFORM CTNAF USING REFNFPIE       WA_INFO_V1-STCD3  P_NFE.

            ELSEIF WA_DOC_PROD-PARTYP EQ 'C'.

              IF WA_INFO_C1-STKZN IS INITIAL.
                PERFORM CTNAF USING REFNFPCNPJ   WA_INFO_C1-STCD1  P_NFE.
              ELSE.
                PERFORM CTNAF USING REFNFPCPF    WA_INFO_C1-STCD2  P_NFE.
              ENDIF.
              PERFORM CTNAF USING REFNFPIE       WA_INFO_C1-STCD3  P_NFE.

            ENDIF.

            PERFORM CTNAF USING REFNFPMOD    WA_REF-B12_MOD   P_NFE.
            PERFORM CTNAF USING REFNFPSERIE  WA_REF-B12_SERIE P_NFE.
            PERFORM CTNAF USING REFNNF       WA_REF-B12_NNF   P_NFE.

            PERFORM CTNFE USING REFNFP P_NFE.
          ENDIF.
          PERFORM CTNFE USING NFREF P_NFE.
        ENDLOOP.


      ENDIF.
  ENDCASE.

  "Adiciona NF Referenciadas
  "IF NOT ( XML_REF_NFE IS INITIAL ).
  "  CONCATENATE P_NFE XML_REF_NFE INTO P_NFE.
  "ENDIF.


ENDFORM.                    " REFERENCIADAS

*&---------------------------------------------------------------------*
*&      Form  EMIT
*&---------------------------------------------------------------------*
FORM EMIT  USING  P_NFE  TB TYPE J1B_NF_XML_HEADER .

  DATA: VG_TAXGRP    TYPE J_1BTAXGRP,
        WA_J_1BSTAST TYPE J_1BSTAST,
        WA_J_1BNDDOC TYPE J_1BNFDOC.

  PERFORM CTNAB  USING EMIT P_NFE.
  PERFORM CTNAV  USING CNPJ TB-C_CNPJ P_NFE.

  "Auteração para XML versão 2.0
  IF VG_XML_VERSAO EQ 2.
    PERFORM LIMPA_NUMERO USING TB-C1_IE.
    PERFORM CTNAV  USING IE   VG_LIMPO  P_NFE.
  ENDIF.

*  "  ICST  Sub.trib.
*  SELECT SINGLE tp~taxgrp INTO vg_taxgrp
*    FROM j_1bnfstx AS im
*   INNER JOIN j_1baj AS tp ON tp~taxtyp EQ im~taxtyp
*   WHERE im~docnum EQ tb-docnum
*     AND tp~taxgrp EQ 'ICST'.
*
*  IF tb-c1_iest IS NOT INITIAL.
*
*  ELSEIF  ( sy-subrc IS INITIAL ) AND ( tb-e1_uf IS NOT INITIAL ).
*
*    SELECT SINGLE * INTO wa_j_1bnddoc
*      FROM j_1bnfdoc
*     WHERE docnum EQ tb-docnum.
*
*    IF sy-subrc IS INITIAL.
*
*      SELECT SINGLE * INTO wa_j_1bstast
*        FROM j_1bstast
*       WHERE bukrs  EQ wa_j_1bnddoc-bukrs
*         AND branch EQ wa_j_1bnddoc-branch
*         AND txreg  EQ tb-e1_uf.
*
*      IF sy-subrc IS INITIAL.
*        tb-c1_iest = wa_j_1bstast-state_insc.
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.

  PERFORM LIMPA_NUMERO USING TB-C1_IEST.
  PERFORM CTNAF  USING IEST  VG_LIMPO P_NFE.

  PERFORM CTNFE  USING EMIT P_NFE.

ENDFORM.                    " EMIT

*&---------------------------------------------------------------------*
*&      Form  DEST
*&---------------------------------------------------------------------*
FORM DEST  TABLES  TBITENS  TYPE J1B_NF_XML_ITEM_TAB
            USING   P_NFE
                    TB TYPE J1B_NF_XML_HEADER
                    J1 TYPE J_1BNFDOC.


  DATA: IP_ADDRESS(69)  TYPE C,
        WA_ZAMB_HOMOLOG TYPE ZAMB_HOMOLOG.

  DATA: WA_ITNS         TYPE J1B_NF_XML_ITEM.

  DATA: WL_LFA1 TYPE LFA1,
        WL_KNA1 TYPE KNA1,
        WL_ADRC TYPE ADRC.


  READ TABLE TBITENS INTO WA_ITNS INDEX 1.

  PERFORM CTNAB USING DEST P_NFE.


  CALL FUNCTION 'ZGET_IP_ADDRESS'
    IMPORTING
      ZIP_ADDRESS = IP_ADDRESS.

  SELECT SINGLE *
    INTO WA_ZAMB_HOMOLOG
    FROM ZAMB_HOMOLOG
   WHERE IP_SERVIDOR = IP_ADDRESS.


  CASE WA_ZAMB_HOMOLOG-AMBIENTE.

    WHEN 'QAS'.
      PERFORM CTNAV USING DESTXNOME 'NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL' P_NFE.
*      IF ( TB-E1_CPAIS IS INITIAL ).
*        PERFORM CTNAF USING DESTCNPJ  '99999999000191'  P_NFE.
*      ELSE.
*        PERFORM CTNAF USING DESTCNPJ  ''  P_NFE.
*      ENDIF.
*      PERFORM CTNAV USING DESTIE ''  P_NFE.
*    WHEN OTHERS.
*      PERFORM ctnav USING destxnome 'ESTA FALTANDO PARAMETRIZAÇÃ0 DO AMBIENTE - TRANSAÇÃO: ZAMBQAS' p_nfe.
**      IF ( TB-E1_CPAIS IS INITIAL ).
**        PERFORM CTNAF USING DESTCNPJ  '99999999000191'  P_NFE.
**      ELSE.
**        PERFORM CTNAF USING DESTCNPJ  ''  P_NFE.
**      ENDIF.
**      PERFORM CTNAV USING DESTIE ''  P_NFE.
    WHEN OTHERS.
      PERFORM LCT USING TB-E_XNOME.
      PERFORM CTNAV USING DESTXNOME VG_LIMPO P_NFE.
  ENDCASE.

  IF TB-E_CNPJ NE '00000000000000'.
    PERFORM CTNAF USING DESTCNPJ  TB-E_CNPJ  P_NFE.
  ENDIF.
  IF TB-E_CPF NE '00000000000'.
    PERFORM CTNAF USING DESTCPF   TB-E_CPF   P_NFE.
  ENDIF.

  "TAG - idEstrangeiro - Identificação do destinatário no caso de comprador estrangeiro.
  "Sempre atribuir o valor "Nulo"(00000).
  CASE WA_ITNS-CFOP(1).
    WHEN: 7 OR 3.
      PERFORM CTNAF USING DESTIDESTRANGEIRO  '00000' P_NFE.
  ENDCASE.

  PERFORM LIMPA_NUMERO USING TB-E1_IE.
  PERFORM CTNAV USING DESTIE VG_LIMPO  P_NFE.

  " TAG - indIEDest - indicador da IE do Destinatário.
  IF (  TB-E1_IE EQ 'ISENTO' ).
    PERFORM CTNAV USING DESTINDIEDEST '2'  P_NFE. "Contribuinte isento de Inscrição no cadastro de contribuinte do ICMS.
  ELSEIF (  TB-E1_IE IS INITIAL ).
    PERFORM CTNAV USING DESTINDIEDEST '9'  P_NFE. " Não Contribuinte, que pod eou não possuir inscrição estadual no cadastro de Contribuinte do ICMS
  ELSEIF NOT ( TB-E1_IE IS INITIAL AND TB-E1_IE NE 'ISENTO').
    PERFORM CTNAV USING DESTINDIEDEST '1'  P_NFE. " Contribuinte ICMS (informar a IE do Destinatário).
  ENDIF.

  "Endereço do Destinatário
  PERFORM CTNAB USING DESTENDERDEST P_NFE.
  PERFORM LCT USING TB-E1_XLGR.
  PERFORM CTNAV USING DESTXLGR VG_LIMPO P_NFE.

  IF NOT TB-E1_NRO IS INITIAL.
    PERFORM CTNAV USING DESTNRO TB-E1_NRO P_NFE.
  ELSE.
    PERFORM CTNAV USING DESTNRO C_SN      P_NFE.
  ENDIF.

  PERFORM LCT USING TB-E1_XCPL.
  PERFORM CTNAF USING DESTXCPL    VG_LIMPO P_NFE.
  PERFORM LCT USING TB-E1_XBAIRRO.
  PERFORM CTNAV USING DESTXBAIRRO VG_LIMPO P_NFE.
  IF ZCL_STRING=>UPPER( CONV #( TB-E1_XPAIS ) ) NE 'BRASIL'.
    PERFORM CTNAV USING DESTCMUN     '9999999'     P_NFE.
  ELSE.
    PERFORM CTNAV USING DESTCMUN     TB-E1_CMUN    P_NFE.
  ENDIF.

  IF ( TB-E1_CEP IS INITIAL ).
    IF ( ( TB-E_CNPJ IS NOT INITIAL ) AND ( TB-E_CNPJ NE '00000000000000' ) ).
      SELECT SINGLE * FROM KNA1 INTO WL_KNA1 WHERE STCD1 EQ TB-E_CNPJ.
      IF ( SY-SUBRC EQ 0 ).
        SELECT SINGLE * FROM ADRC INTO WL_ADRC WHERE ADDRNUMBER EQ WL_KNA1-ADRNR.
        REPLACE REGEX '-' IN WL_ADRC-POST_CODE1 WITH ''.
        PERFORM CTNAF USING DESTCEP WL_ADRC-POST_CODE1 P_NFE.
      ELSE.
        SELECT SINGLE * FROM LFA1 INTO WL_LFA1 WHERE STCD1 EQ TB-E_CNPJ.
        SELECT SINGLE * FROM ADRC INTO WL_ADRC WHERE ADDRNUMBER EQ WL_KNA1-ADRNR.
        REPLACE REGEX '-' IN WL_ADRC-POST_CODE1 WITH ''.
        PERFORM CTNAF USING DESTCEP WL_ADRC-POST_CODE1 P_NFE.
      ENDIF.
    ELSEIF ( ( TB-E_CPF IS NOT INITIAL ) AND ( TB-E_CPF NE '00000000000' ) ).
      "IF NOT ( TB-E_CNPJ IS INITIAL ).
      SELECT SINGLE * FROM KNA1 INTO WL_KNA1 WHERE STCD2 EQ TB-E_CPF.
      IF ( SY-SUBRC EQ 0 ).
        SELECT SINGLE * FROM ADRC INTO WL_ADRC WHERE ADDRNUMBER EQ WL_KNA1-ADRNR.
        REPLACE REGEX '-' IN WL_ADRC-POST_CODE1 WITH ''.
        PERFORM CTNAF USING DESTCEP WL_ADRC-POST_CODE1 P_NFE.
      ELSE.
        SELECT SINGLE * FROM LFA1 INTO WL_LFA1 WHERE STCD2 EQ TB-E_CPF.
        SELECT SINGLE * FROM ADRC INTO WL_ADRC WHERE ADDRNUMBER EQ WL_KNA1-ADRNR.
        REPLACE REGEX '-' IN WL_ADRC-POST_CODE1 WITH ''.
        PERFORM CTNAF USING DESTCEP WL_ADRC-POST_CODE1 P_NFE.
      ENDIF.
      "ENDIF.
    ENDIF.

  ELSE.
    PERFORM CTNAF USING DESTCEP      TB-E1_CEP     P_NFE.
  ENDIF.
  PERFORM CTNAF USING DESTCPAIS    TB-E1_CPAIS   P_NFE.

  PERFORM LIMPA_NUMERO USING TB-E1_FONE.
  PERFORM CTNAF USING DESTFONE VG_LIMPO P_NFE.

  PERFORM E_MAIL_DESTINATARIO USING P_NFE TB J1.

  PERFORM CTNFE USING DESTENDERDEST P_NFE.
  "Campo SUFRAMA
  PERFORM LIMPA_NUMERO USING TB-E1_ISUF.
  PERFORM CTNAF USING DESTISUF VG_LIMPO P_NFE.
  IF NOT VG_LIMPO IS INITIAL.
    VG_ZONA_FRANCA = C_X.
  ENDIF.


  PERFORM CTNFE USING DEST P_NFE.

ENDFORM.                    " DEST

*&---------------------------------------------------------------------*
*&      Form  RETIRADA
*&---------------------------------------------------------------------*
FORM RETIRADA  USING    P_NFE  TB TYPE J1B_NF_XML_HEADER .

  IF TB-F_RETIRADA IS NOT INITIAL.
    PERFORM CTNAB USING RETIRADA P_NFE.
    IF ( TB-F_CNPJ IS NOT INITIAL ) AND ( TB-F_CNPJ NE '00000000000000' ).
      PERFORM CTNAV USING RETIRADACNPJ    TB-F_CNPJ    P_NFE.
    ELSE.
      PERFORM CTNAV USING RETIRADACPF     TB-F_CPF     P_NFE.
    ENDIF.
    PERFORM CTNAV USING RETIRADAXLGR    TB-F_XLGR    P_NFE.
    IF NOT TB-F_NRO IS INITIAL.
      PERFORM CTNAV USING RETIRADANRO     TB-F_NRO  P_NFE.
    ELSE.
      PERFORM CTNAV USING RETIRADANRO     C_SN      P_NFE.
    ENDIF.
    PERFORM LCT USING TB-F_XCPL.
    PERFORM CTNAF USING RETIRADAXCPL    VG_LIMPO     P_NFE.
    PERFORM LCT USING TB-F_XBAIRRO.
    PERFORM CTNAV USING RETIRADAXBAIRRO VG_LIMPO     P_NFE.
    PERFORM CTNAV USING RETIRADACMUN    TB-F_CMUN    P_NFE.
    PERFORM CTNFE USING RETIRADA P_NFE.
  ENDIF.

ENDFORM.                    " RETIRADA

*&---------------------------------------------------------------------*
*&      Form  ENTREGA
*&---------------------------------------------------------------------*
FORM ENTREGA  USING    P_NFE  TB TYPE J1B_NF_XML_HEADER .

  IF TB-ENTREGA IS NOT INITIAL.
    PERFORM CTNAB USING ENTREGA P_NFE.
    IF ( TB-G_CNPJ IS NOT INITIAL ) AND ( TB-G_CNPJ NE '00000000000000' ).
      PERFORM CTNAV USING ENTREGACNPJ    TB-G_CNPJ    P_NFE.
    ELSE.
      PERFORM CTNAV USING ENTREGACPF     TB-G_CPF     P_NFE.
    ENDIF.
    PERFORM CTNAV USING ENTREGAXLGR    TB-G_XLGR    P_NFE.
    IF NOT TB-G_NRO IS INITIAL.
      PERFORM CTNAV USING ENTREGANRO     TB-G_NRO     P_NFE.
    ELSE.
      PERFORM CTNAV USING ENTREGANRO     C_SN         P_NFE.
    ENDIF.
    PERFORM LCT USING TB-G_XCPL.
    PERFORM CTNAF USING ENTREGAXCPL    VG_LIMPO     P_NFE.
    PERFORM LCT USING TB-G_XBAIRRO.
    PERFORM CTNAV USING ENTREGAXBAIRRO VG_LIMPO     P_NFE.
    PERFORM CTNAV USING ENTREGACMUN    TB-G_CMUN    P_NFE.
    PERFORM CTNFE USING ENTREGA P_NFE.
  ENDIF.

ENDFORM.                    " ENTREGA

*&---------------------------------------------------------------------*
*&      Form  TOTAIS
*&---------------------------------------------------------------------*
FORM TOTAIS  USING    P_NFE  TB TYPE J1B_NF_XML_HEADER .

  PERFORM CTNAB USING TOTAL   P_NFE.

  "Tag de Grupo de valores totais referente a ICMS
  PERFORM CTNAB USING ICMSTOT P_NFE.
  IF VAR_TOTAL_VBICM_51 IS NOT INITIAL.
    PERFORM CTNAVN USING VBC     VAR_TOTAL_VBICM_51  P_NFE.
  ELSE.
    PERFORM CTNAVN USING VBC     TB-S1_VBC     P_NFE.
  ENDIF.

  PERFORM CTNAVN USING VICMS   TB-S1_VICMS   P_NFE.
  PERFORM CTNAVN USING VBCST   TB-S1_VBCST   P_NFE.
  PERFORM CTNAVN USING VST     TB-S1_VST     P_NFE.
  PERFORM CTNAVN USING VPROD   TB-S1_VPROD   P_NFE.
  PERFORM CTNAVN USING VFRETE  TB-S1_VFRETE  P_NFE.
  PERFORM CTNAVN USING VSEG    TB-S1_VSEG    P_NFE.

  "PERFORM CTNAVN USING VDESC   TB-S1_VDESC   P_NFE.

  IF NOT ( VAR_TOTAL_DESC IS INITIAL ).
    PERFORM CTNAVN USING VDESC   VAR_TOTAL_DESC   P_NFE.
  ELSE.
    PERFORM CTNAVN USING VDESC   0   P_NFE.
  ENDIF.

  PERFORM CTNAVN USING VII     TB-S1_VII     P_NFE.
  PERFORM CTNAVN USING VIPI    TB-S1_VIPI    P_NFE.
  PERFORM CTNAVN USING VPIS    TB-S1_VPIS    P_NFE.
  PERFORM CTNAVN USING VCOFINS TB-S1_VCOFINS P_NFE.

  IF NOT ( VAR_TOTAL_OUTROS IS INITIAL ).
    "PERFORM CTNAVN USING VOUTRO  TB-S1_VOUTRO  P_NFE.
    PERFORM CTNAVN USING VOUTRO  VAR_TOTAL_OUTROS  P_NFE.
  ELSE.
    PERFORM CTNAVN USING VOUTRO  0 P_NFE.
  ENDIF.


  PERFORM CTNAVN USING VNF     TB-S1_VNF     P_NFE.

  VAR_TOTAL_TRIB = VAR_TOTAL_TRIB2.

  PERFORM CTNAVN USING VTOTTRIB      VAR_TOTAL_TRIB P_NFE.

  IF NOT ( VAR_TOTAL_DESON IS INITIAL ).
    PERFORM CTNAVN USING IMPVICMSDESON VAR_TOTAL_DESON P_NFE.
  ELSE.
    PERFORM CTNAVN USING IMPVICMSDESON 0 P_NFE.
  ENDIF.

  "PERFORM CTNAVN USING IMPVICMSDESON TB-S1_VDESC P_NFE.

  IF ( VAR_TOT_ICMS_UF_DEST > 0 ) OR ( VAR_TOT_ICMS_UF_REMET > 0 ).
    PERFORM CTNAVN USING IMPVFCPUFDEST   0 P_NFE.
    PERFORM CTNAVN USING IMPVICMSUFDEST  VAR_TOT_ICMS_UF_DEST P_NFE.
    PERFORM CTNAVN USING IMPVICMSUFREMET VAR_TOT_ICMS_UF_REMET P_NFE.
  ENDIF.

  IF VG_VERSAO_NFE >= 4.
    PERFORM CTNAVN USING VFCP        0   P_NFE.
    PERFORM CTNAVN USING VFCPST      0   P_NFE.
    PERFORM CTNAVN USING VFCPSTRET   0   P_NFE.
    PERFORM CTNAVN USING VIPIDEVOL   0   P_NFE.
  ENDIF.

  PERFORM CTNFE USING ICMSTOT P_NFE.

  "Tag de Grupo de Retenções de Tributos
  IF ( TB-S3_VRETPIS IS NOT INITIAL ) OR ( TB-S3_VRETCOFINS IS NOT INITIAL ) OR
     ( TB-S3_VRETCSLL IS NOT INITIAL ) OR ( TB-S3_VBCIRRF IS NOT INITIAL ) OR
     ( TB-S3_VIRRF IS NOT INITIAL ) OR ( TB-S3_VBCRETPREV IS NOT INITIAL ) OR
     ( TB-S3_VRETPREV IS NOT INITIAL ).
    PERFORM CTNAB  USING RETTRIB P_NFE.
    PERFORM CTNAFV USING VRETPIS    TB-S3_VRETPIS    P_NFE.
    PERFORM CTNAFV USING VRETCOFINS TB-S3_VRETCOFINS P_NFE.
    PERFORM CTNAFV USING VRETCSLL   TB-S3_VRETCSLL   P_NFE.
    PERFORM CTNAFV USING VBCIRRF    TB-S3_VBCIRRF    P_NFE.
    PERFORM CTNAFV USING VIRRF      TB-S3_VIRRF      P_NFE.
    PERFORM CTNAFV USING VBCRETPREV TB-S3_VBCRETPREV P_NFE.
    PERFORM CTNAFV USING VRETPREV   TB-S3_VRETPREV   P_NFE.
    PERFORM CTNFE USING RETTRIB P_NFE.
  ENDIF.

  PERFORM CTNFE USING TOTAL P_NFE.

  CLEAR: VAR_TOTAL_DESC, VAR_TOTAL_TRIB2, VAR_TOTAL_DESON, VAR_TOTAL_OUTROS.

  CLEAR: VAR_TOT_ICMS_UF_DEST , VAR_TOT_ICMS_UF_REMET.


ENDFORM.                    " TOTAIS

*&---------------------------------------------------------------------*
*&      Form  PAG
*&---------------------------------------------------------------------*
FORM PAG  USING  P_NFE  TB TYPE J1B_NF_XML_HEADER.

  DATA: BEGIN OF TG_LIN OCCURS 0,
          REFKEY       TYPE J_1BNFLIN-REFKEY,
          REFITM       TYPE J_1BNFLIN-REFITM,
          REFTYP       TYPE J_1BNFLIN-REFTYP,
          VBELN        TYPE C LENGTH 10,
          REFKEY_VBELN TYPE VBRK-VBELN,
        END OF TG_LIN.

  DATA: V_ZLSCH TYPE VBRK-ZLSCH,
        V_TPAG  TYPE C LENGTH 2,
        V_VPAG  TYPE J_1BNFLIN-NETWR.

  CLEAR: TG_LIN[], V_ZLSCH, V_TPAG, V_VPAG.

  SELECT REFKEY REFITM REFTYP
    FROM J_1BNFLIN INTO CORRESPONDING FIELDS OF TABLE TG_LIN
   WHERE DOCNUM EQ TB-DOCNUM.

  CHECK TG_LIN[] IS NOT INITIAL.

  LOOP AT TG_LIN.
    CHECK ( TG_LIN-REFKEY IS NOT INITIAL ) AND ( STRLEN( TG_LIN-REFKEY ) >= 10 ).

    TG_LIN-VBELN = TG_LIN-REFKEY(10).

    CASE TG_LIN-REFTYP.
      WHEN 'BI'.
        SELECT SINGLE *
          FROM VBRK INTO @DATA(_WL_VBRK)
         WHERE VBELN EQ @TG_LIN-VBELN.

        IF ( SY-SUBRC = 0 ) AND ( _WL_VBRK-ZLSCH IS NOT INITIAL ).
          V_ZLSCH = _WL_VBRK-ZLSCH.
          EXIT.
        ENDIF.
      WHEN 'MD'.

    ENDCASE.
  ENDLOOP.

  IF V_ZLSCH IS NOT INITIAL.
    CASE V_ZLSCH.
      WHEN 'A'. "Duplicata
        V_TPAG = '99'.
      WHEN 'C'. "Cheque
        V_TPAG = '02'.
      WHEN 'D'. "Boleto
        V_TPAG = '15'.
      WHEN 'E'. "Pagamento Direto
        V_TPAG = '99'.
      WHEN 'H'. "Transf.Bancaria - Folha Pgto
        V_TPAG = '99'.
      WHEN 'M'. "Doc.(Comp)
        V_TPAG = '99'.
      WHEN 'P'. "
        V_TPAG = '99'.
      WHEN 'S'. "TED STR
        V_TPAG = '99'.
      WHEN 'T'. "TED CIP
        V_TPAG = '99'.
      WHEN 'U'. " Transf. Bancaria
        V_TPAG = '99'.
      WHEN 'V'. "Vendor Operation payment
        V_TPAG = '99'.
      WHEN OTHERS.
        V_TPAG = '99'.
    ENDCASE.
  ELSE.
    V_TPAG = '90'. "Sem Pagamento
  ENDIF.

  IF ( TB-FINNFE EQ '3' ) OR
     ( TB-FINNFE EQ '4' ).
    V_TPAG = '90'. "Sem Pagamento
  ENDIF.

  V_VPAG = TB-S1_VNF.

  IF V_TPAG = '90'.
    V_VPAG = 0.
  ENDIF.

  CHECK VG_VERSAO_NFE >= 4.

  PERFORM CTNAB USING PAG P_NFE.
  PERFORM CTNAB USING DETPAG P_NFE.

  PERFORM CTNAV  USING TPAG  V_TPAG  P_NFE.
  PERFORM CTNAVN USING VPAG  V_VPAG  P_NFE.

  PERFORM CTNFE USING DETPAG P_NFE.
  PERFORM CTNFE USING PAG P_NFE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TRANSP
*&---------------------------------------------------------------------*
FORM TRANSP TABLES TBVOL    TYPE J1B_NF_XML_T4_TAB
                   TBITENS  TYPE J1B_NF_XML_ITEM_TAB
             USING P_NFE TYPE ZXML
                   TB    TYPE J1B_NF_XML_HEADER.

  DATA: WA_TBVOL TYPE J1B_NF_XML_T6.
  DATA: T_MONTA  TYPE C LENGTH 1.
  DATA: VJ_1BNFDOC TYPE J_1BNFDOC,
        LV_NTGEW   TYPE NTGEW_15,
        LV_BRGEW   TYPE BRGEW_15.

  DATA: WA_ITNS  TYPE J1B_NF_XML_ITEM,
        V_IDDEST TYPE C.

  DATA: VJ_1BNFLIN   TYPE J_1BNFLIN,
        WL_1BNFLIN   TYPE J_1BNFLIN,
        WL_ZSDT0119  TYPE ZSDT0119,
        WL_ZSDT0121  TYPE ZSDT0121,
        WL_ZSDT0001  TYPE ZSDT0001,
        WL_J_1BNFNAD TYPE J_1BNFNAD,
        WL_MARM      TYPE MARM,
        VVBFA        TYPE VBFA,
        VZSDT0001    TYPE ZSDT0001,
        VZLEST0002   TYPE ZLEST0002,
        VLFA1        TYPE LFA1,
        VL_PARID_TMP TYPE J_1BNFDOC-PARID,
        T_SADRVB     TYPE TABLE OF SADRVB WITH HEADER LINE INITIAL SIZE 0,
        T_VBPAVB     TYPE TABLE OF VBPAVB WITH HEADER LINE INITIAL SIZE 0,
        SL_XVBPA     TYPE VBPAVB,
        S_LFA1       TYPE LFA1.

  DATA: LW_ZFIWRT0019 TYPE ZFIWRT0019.

  DATA: VALOR_D    TYPE P DECIMALS 0,
        VL_SHPUNT  TYPE STRING,
        VALOR_S    TYPE STRING,
        VL_UMREZ_S TYPE STRING.

  CLEAR: WA_ITNS, V_IDDEST.
  "Recuperar qual é o CFOP Atribuido no ITEM da Nota Fiscal Eletrônica.
  READ TABLE TBITENS INTO WA_ITNS INDEX 1.

  CASE WA_ITNS-CFOP(1).
    WHEN: '1' OR '5'.
      V_IDDEST = '1'.
    WHEN: '2' OR '6'.
      V_IDDEST = '2'.
    WHEN: '3' OR '7'.
      V_IDDEST = '3'.
  ENDCASE.

  "Tag de informação de transporte

  DATA(_SEM_FRETE) = ''.

  SELECT SINGLE * INTO VJ_1BNFDOC
    FROM J_1BNFDOC
   WHERE DOCNUM EQ TB-DOCNUM.

  IF SY-SUBRC = 0.
    CLEAR: WL_ZSDT0121, VJ_1BNFLIN.

    SELECT SINGLE *
      FROM J_1BNFLIN INTO VJ_1BNFLIN
     WHERE DOCNUM EQ TB-DOCNUM.

    IF SY-SUBRC = 0.
      CASE VJ_1BNFDOC-INCO1.
        WHEN 'FOB'." OR 'CPT'. "//ISSUE-188371 WBARBOSA 19/08/25
          "Ajuste Tipo de Frete para fatura agrupada a partir da ZLEST0106/ZLEST0136
          SELECT SINGLE *
            FROM J_1BNFNAD INTO WL_J_1BNFNAD
           WHERE DOCNUM = TB-DOCNUM
             AND PARVW  = 'LR'.
          IF SY-SUBRC = 0.
            VL_PARID_TMP = VJ_1BNFDOC-PARID.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = VL_PARID_TMP
              IMPORTING
                OUTPUT = VL_PARID_TMP.

            SELECT SINGLE *
              FROM ZSDT0121 INTO WL_ZSDT0121
             WHERE WERKS EQ VL_PARID_TMP+6(4)
               AND MATNR EQ VJ_1BNFLIN-MATNR
               AND KUNNR EQ WL_J_1BNFNAD-PARID.

            IF ( SY-SUBRC = 0 ) AND ( VJ_1BNFLIN-REFKEY(10) IS NOT INITIAL ).
              SELECT SINGLE *
                FROM ZSDT0001 INTO WL_ZSDT0001
               WHERE FATURA_PROD = VJ_1BNFLIN-REFKEY(10).
              IF SY-SUBRC EQ 0.
                _SEM_FRETE = 'X'.
              ENDIF.
            ENDIF.
          ENDIF.

        WHEN 'CFR'.
          IF VJ_1BNFLIN-REFKEY(10) IS NOT INITIAL.
            SELECT SINGLE *
              FROM ZSDT0001 INTO WL_ZSDT0001
             WHERE FATURA_PROD = VJ_1BNFLIN-REFKEY(10).

            IF ( SY-SUBRC EQ 0 ) AND
               ( WL_ZSDT0001-ID_INTERFACE = '48' OR
                 WL_ZSDT0001-ID_INTERFACE = '51' OR
                 WL_ZSDT0001-ID_INTERFACE = '52' ).
              _SEM_FRETE = 'X'.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDIF.

  PERFORM CTNAB USING TRANSP  P_NFE.

  IF _SEM_FRETE IS NOT INITIAL.
    PERFORM CTNAV USING MODFRETE '9' P_NFE.
    PERFORM CTNFE USING TRANSP  P_NFE.
    CHECK 1 = 2.
  ENDIF.

  IF TB-FINNFE EQ C_2.
    IF VG_XML_VERSAO EQ 2.
      PERFORM CTNAV USING MODFRETE '9' P_NFE.
    ELSE.
      IF VJ_1BNFDOC-INCO1 EQ 'FOB'.
        PERFORM CTNAV USING MODFRETE '1' P_NFE.
      ELSE.
        PERFORM CTNAV USING MODFRETE '0' P_NFE.
      ENDIF.
    ENDIF.
    PERFORM CTNFE USING TRANSP  P_NFE.
    CHECK TB-FINNFE NE C_2.
  ENDIF.


  " 0 = Por conta do emitente (CIF/CPT);
  " 1 = Por conta do destinatário/remetente(FOB);
  " 9 = Sem frete (FCA)

  CASE VJ_1BNFDOC-INCO1.
    WHEN: 'CIF' OR 'CPT'.
      PERFORM CTNAV USING MODFRETE '0' P_NFE.
    WHEN: 'FOB'.
      PERFORM CTNAV USING MODFRETE '1' P_NFE.
    WHEN: 'FCA'.
      PERFORM CTNAV USING MODFRETE '9' P_NFE.
    WHEN OTHERS.
      PERFORM CTNAV USING MODFRETE '1' P_NFE.
  ENDCASE.


*  IF VJ_1BNFDOC-INCO1 EQ 'FOB'.
*    PERFORM CTNAV USING MODFRETE '1' P_NFE.
*  ELSE.
*    IF VJ_1BNFDOC-PARTYP EQ 'B'.
*      PERFORM CTNAV USING MODFRETE '1' P_NFE.
*    ELSE.
*      PERFORM CTNAV USING MODFRETE '0' P_NFE.
*    ENDIF.
*  ENDIF.
*
*  IF VJ_1BNFDOC-INCO1 EQ 'CIF'.
*    PERFORM CTNAV USING MODFRETE '0' P_NFE.
*  ENDIF.
*



*  IF tb-t_modfrete IS INITIAL.
*    PERFORM ctnav USING modfrete '0' p_nfe.
*  ELSE.
*    PERFORM ctnav USING modfrete tb-t_modfrete p_nfe.
*  ENDIF.

  "Tag de informações da trabsportadora
  IF ( TB-T1_CNPJ IS NOT INITIAL ) OR ( TB-T1_CPF IS NOT INITIAL ).
    PERFORM CTNAB USING TRANSPORTA P_NFE.

    IF NOT ( TB-T1_CNPJ IS INITIAL ).
      PERFORM CTNAF USING TRANSPORTACNPJ   TB-T1_CNPJ  P_NFE.
    ELSE.
      PERFORM CTNAF USING TRANSPORTACPF    TB-T1_CPF   P_NFE.
    ENDIF.

    PERFORM CTNAF USING TRANSPORTAXNOME  TB-T1_XNOME P_NFE.
    PERFORM LIMPA_NUMERO USING TB-T1_IE.
    PERFORM CTNAF USING TRANSPORTAIE     VG_LIMPO    P_NFE.
    PERFORM CTNAF USING TRANSPORTAXENDER TB-T1_XEND  P_NFE.
    PERFORM CTNAF USING TRANSPORTAXMUN   TB-T1_XMUN  P_NFE.

    IF NOT TB-T1_IE IS INITIAL.
      IF ( NOT TB-T1_CNPJ IS INITIAL ) AND ( TB-T1_CNPJ NE '00000000000000' ).
        SELECT SINGLE * INTO S_LFA1
          FROM LFA1
         WHERE STCD1 EQ TB-T1_CNPJ
           AND STCD3 EQ TB-T1_IE.
        IF SY-SUBRC IS INITIAL.
          PERFORM CTNAF USING TRANSPORTACMUN S_LFA1-TXJCD+3(7) P_NFE.
        ENDIF.
      ELSEIF ( NOT TB-T1_CPF IS INITIAL ) AND ( TB-T1_CPF NE '00000000000' ).
        SELECT SINGLE * INTO S_LFA1
          FROM LFA1
         WHERE STCD2 EQ TB-T1_CPF
           AND STCD3 EQ TB-T1_IE.
        IF SY-SUBRC IS INITIAL.
          PERFORM CTNAF USING TRANSPORTACMUN S_LFA1-TXJCD+3(7) P_NFE.
        ENDIF.

      ENDIF.
      PERFORM CTNAF USING TRANSPORTAUF     TB-T1_UF    P_NFE.
    ENDIF.

    CLEAR S_LFA1.

    PERFORM CTNFE USING TRANSPORTA P_NFE.
  ELSE.

    CLEAR: T_MONTA.

    SELECT SINGLE *
      INTO VJ_1BNFLIN
      FROM J_1BNFLIN
     WHERE DOCNUM EQ TB-DOCNUM.

    IF SY-SUBRC EQ 0.

      SELECT SINGLE *
         INTO VVBFA
         FROM VBFA
        WHERE VBELN EQ VJ_1BNFLIN-REFKEY(10)
          AND POSNN EQ VJ_1BNFLIN-REFITM
          AND VBTYP_V EQ 'J'.

      IF SY-SUBRC EQ 0.

        CALL FUNCTION 'SD_PARTNER_READ'
          EXPORTING
            F_VBELN  = VVBFA-VBELV
            OBJECT   = 'VBPA'
          TABLES
            I_XVBADR = T_SADRVB
            I_XVBPA  = T_VBPAVB.

        DELETE T_VBPAVB WHERE PARVW NE 'SP'.

        IF NOT T_VBPAVB[] IS INITIAL.

          READ TABLE T_VBPAVB INTO SL_XVBPA INDEX 1.
          SELECT SINGLE *
            FROM LFA1
            INTO S_LFA1
           WHERE LIFNR EQ SL_XVBPA-LIFNR.
          IF SY-SUBRC EQ 0.
            PERFORM CTNAB USING TRANSPORTA P_NFE.
            PERFORM CTNAF USING TRANSPORTACNPJ   S_LFA1-STCD1      P_NFE.
            IF NOT S_LFA1-STKZN IS INITIAL.
              PERFORM CTNAF USING TRANSPORTACPF  S_LFA1-STCD2      P_NFE.
            ENDIF.
            PERFORM CTNAF USING TRANSPORTAXNOME  S_LFA1-NAME1      P_NFE.
            PERFORM LIMPA_NUMERO USING S_LFA1-STCD3.
            PERFORM CTNAF USING TRANSPORTAIE     VG_LIMPO          P_NFE.
            PERFORM CTNAF USING TRANSPORTAXENDER S_LFA1-STRAS      P_NFE.
            PERFORM CTNAF USING TRANSPORTACMUN   S_LFA1-TXJCD+3(7) P_NFE.
            PERFORM CTNAF USING TRANSPORTAUF     S_LFA1-TXJCD(2)   P_NFE.
            PERFORM CTNFE USING TRANSPORTA P_NFE.
            T_MONTA = 'X'.
          ENDIF.
        ENDIF.
      ELSE.

        SELECT SINGLE * FROM ZFIWRT0019 INTO LW_ZFIWRT0019
              WHERE SEQ_LCTO EQ VJ_1BNFLIN-REFKEY(10).

        IF ( SY-SUBRC EQ 0 ).


          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = LW_ZFIWRT0019-LIFNR
            IMPORTING
              OUTPUT = LW_ZFIWRT0019-LIFNR.

          SELECT SINGLE *
              FROM LFA1
              INTO S_LFA1
              WHERE LIFNR EQ LW_ZFIWRT0019-LIFNR.

          PERFORM CTNAB USING TRANSPORTA P_NFE.
          PERFORM CTNAF USING TRANSPORTACNPJ   S_LFA1-STCD1      P_NFE.
          IF NOT S_LFA1-STKZN IS INITIAL.
            PERFORM CTNAF USING TRANSPORTACPF  S_LFA1-STCD2      P_NFE.
          ENDIF.
          PERFORM CTNAF USING TRANSPORTAXNOME  S_LFA1-NAME1      P_NFE.
          PERFORM LIMPA_NUMERO USING S_LFA1-STCD3.
          PERFORM CTNAF USING TRANSPORTAIE     VG_LIMPO          P_NFE.
          PERFORM CTNAF USING TRANSPORTAXENDER S_LFA1-ORT01      P_NFE.
          PERFORM CTNAF USING TRANSPORTACMUN   S_LFA1-TXJCD+3(7) P_NFE.
          PERFORM CTNAF USING TRANSPORTAUF     S_LFA1-TXJCD(2)   P_NFE.
          PERFORM CTNFE USING TRANSPORTA P_NFE.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  "Tag de Retenção de ICMS do Transportador
*  IF ( TB-T5_VSERV IS NOT INITIAL ) OR ( TB-T5_VBCRET IS NOT INITIAL ) OR
*     ( TB-T5_PICMSRET IS NOT INITIAL ) OR ( TB-T5_VICMSRET IS NOT INITIAL ) OR
*     ( TB-T5_CFOP IS NOT INITIAL ) OR ( TB-T5_CMUNFG IS NOT INITIAL ).
*    PERFORM CTNAB USING RETTRANSP P_NFE.
*    PERFORM CTNAVN USING RETTRANSPVSERV    TB-T5_VSERV    P_NFE.
*    PERFORM CTNAV USING RETTRANSPVBCRET   TB-T5_VBCRET   P_NFE.
*    PERFORM CTNAV USING RETTRANSPPICMSRET TB-T5_PICMSRET P_NFE.
*    PERFORM CTNAV USING RETTRANSPVICMSRET TB-T5_VICMSRET P_NFE.
*    PERFORM CTNAV USING RETTRANSPCFOP     TB-T5_CFOP     P_NFE.
*    PERFORM CTNAV USING RETTRANSPCMUN     TB-T5_CMUNFG   P_NFE.
*    PERFORM CTNFE USING RETTRANSP P_NFE.
*  ENDIF.

  "Tag de Veículo
  IF "( V_IDDEST      NE '2' ) OR  "Interestadual - NF-e 4.0 13.04.2018
     ( VG_VERSAO_NFE < 4    ).

    IF ( TB-T2_PLACA IS NOT INITIAL ).
      PERFORM CTNAB USING VEICTRANSP  P_NFE.
      PERFORM CTNAV USING VEICTRANSPPLACA   TB-T2_PLACA P_NFE.
      PERFORM CTNAV USING VEICTRANSPUF      TB-T2_UF1   P_NFE.
      PERFORM CTNAF USING VEICTRANSPRNTC    TB-T2_RNTC  P_NFE.
      PERFORM CTNFE USING VEICTRANSP  P_NFE.
    ELSE.

      IF NOT (  LW_ZFIWRT0019 IS INITIAL ) AND ( LW_ZFIWRT0019-PLACA IS NOT INITIAL ) .

        PERFORM CTNAB USING VEICTRANSP                            P_NFE.
        PERFORM CTNAV USING VEICTRANSPPLACA LW_ZFIWRT0019-PLACA   P_NFE.
        PERFORM CTNAV USING VEICTRANSPUF    LW_ZFIWRT0019-UFPLACA P_NFE.
        PERFORM CTNAF USING VEICTRANSPRNTC  S_LFA1-BAHNS          P_NFE.
        PERFORM CTNFE USING VEICTRANSP                            P_NFE.
      ELSE.

        SELECT SINGLE *
          INTO VJ_1BNFLIN
          FROM J_1BNFLIN
         WHERE DOCNUM EQ TB-DOCNUM.
        IF SY-SUBRC EQ 0.
          SELECT SINGLE *
             INTO VVBFA
             FROM VBFA
            WHERE VBELN EQ VJ_1BNFLIN-REFKEY(10)
              AND POSNN EQ VJ_1BNFLIN-REFITM
              AND VBTYP_V EQ 'J'.
          IF SY-SUBRC EQ 0.
            CLEAR VZSDT0001.
            SELECT SINGLE * INTO VZSDT0001
              FROM ZSDT0001
             WHERE DOC_REM EQ VVBFA-VBELV
               AND TP_MOVIMENTO  EQ 'S'.
            IF SY-SUBRC EQ 0.
              SELECT SINGLE * INTO VZLEST0002
                FROM ZLEST0002
               WHERE PC_VEICULO EQ VZSDT0001-PLACA_CAV.
              IF SY-SUBRC EQ 0 OR VZSDT0001-REGION IS NOT INITIAL.
                SELECT SINGLE * INTO VLFA1
                  FROM LFA1
                 WHERE LIFNR EQ VZLEST0002-PROPRIETARIO.
                IF SY-SUBRC EQ 0 OR VZSDT0001-REGION IS NOT INITIAL.
                  IF VZSDT0001-REGION IS NOT INITIAL.
                    VZLEST0002-CD_UF = VZSDT0001-REGION.
                  ENDIF.
                  PERFORM CTNAB USING VEICTRANSP                          P_NFE.
                  PERFORM CTNAV USING VEICTRANSPPLACA VZSDT0001-PLACA_CAV P_NFE.
                  PERFORM CTNAV USING VEICTRANSPUF    VZLEST0002-CD_UF    P_NFE.
                  PERFORM CTNAF USING VEICTRANSPRNTC  VLFA1-BAHNS         P_NFE.
                  PERFORM CTNFE USING VEICTRANSP                          P_NFE.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  "Tag de Reboque

  IF "( V_IDDEST      NE '2' ) OR  "Interestadual
     ( VG_VERSAO_NFE < 4    ).    "NF-e 4.0 13.04.2018
    IF ( TB-T3_PLACA IS NOT INITIAL ).
      PERFORM CTNAB USING REBOQUE  P_NFE.
      PERFORM CTNAV USING REBOQUEPLACA TB-T3_PLACA P_NFE.
      PERFORM CTNAV USING REBOQUEUF    TB-T3_UF1   P_NFE.
      PERFORM CTNAF USING REBOQUERNTC  TB-T3_RNTC  P_NFE.
      PERFORM CTNFE USING REBOQUE  P_NFE.
    ENDIF.
  ENDIF.

  "Tag de Volumes
  LOOP AT TBVOL INTO WA_TBVOL.
    IF ( WA_TBVOL-T4_QVOL   IS NOT INITIAL ) OR ( WA_TBVOL-T4_ESP   IS NOT INITIAL ) OR
       ( WA_TBVOL-T4_MARCA  IS NOT INITIAL ) OR ( WA_TBVOL-T4_NVOL  IS NOT INITIAL ) OR
       ( WA_TBVOL-T4_PESOL  IS NOT INITIAL ) OR ( WA_TBVOL-T4_PESOB IS NOT INITIAL ) OR
       ( WA_TBVOL-T4_NLACRE IS NOT INITIAL ).


      CLEAR: VALOR_S, VALOR_D.

      VALOR_D = WA_TBVOL-T4_PESOL.
      VALOR_S = VALOR_D.

      REPLACE '.' IN VALOR_S WITH ' '.

      PERFORM CTNAB  USING VOL P_NFE.
      PERFORM CTNAFV USING VOLQVOL    WA_TBVOL-T4_QVOL  P_NFE.
      PERFORM CTNAF  USING VOLESP     WA_TBVOL-T4_ESP   P_NFE.
      PERFORM CTNAF  USING VOLMARCA   WA_TBVOL-T4_MARCA P_NFE.
      PERFORM CTNAF  USING VOLNVOL    WA_TBVOL-T4_NVOL  P_NFE.
      PERFORM CTNAFV USING VOLPESOL   WA_TBVOL-T4_PESOL P_NFE.
      PERFORM CTNAFV USING VOLPESOB   WA_TBVOL-T4_PESOB P_NFE.

      "Tag de Lacres dos volumes
      IF ( TB-T4_NLACRE IS NOT INITIAL ).
        PERFORM CTNAB  USING LACRES P_NFE.
        PERFORM CTNAF  USING LACRESNLACRES WA_TBVOL-T4_NLACRE P_NFE.
        PERFORM CTNFE  USING LACRES P_NFE.
      ENDIF.

      PERFORM CTNAFV USING VOLQVOL  VALOR_S P_NFE.
      PERFORM CTNAF  USING VOLESP   VJ_1BNFDOC-SHPUNT     P_NFE.
      PERFORM CTNFE  USING VOL P_NFE.
    ENDIF.
  ENDLOOP.

  IF NOT  VZSDT0001 IS INITIAL .

    IF ( TBVOL IS INITIAL ) AND ( TBVOL[] IS INITIAL ).

      CLEAR: VALOR_S, VALOR_D.
      VALOR_D   = VZSDT0001-PESO_LIQ.
      VL_SHPUNT = VJ_1BNFDOC-SHPUNT.

      "Conversão de Qtde. Vol. - Especie.
      CLEAR: WL_1BNFLIN.
      SELECT SINGLE *
        FROM J_1BNFLIN INTO WL_1BNFLIN
       WHERE DOCNUM EQ VJ_1BNFDOC-DOCNUM.
      IF SY-SUBRC = 0.

        SELECT SINGLE *
          FROM MARC INTO @DATA(WL_MARC)
         WHERE MATNR EQ @WL_1BNFLIN-MATNR
           AND WERKS EQ @WL_1BNFLIN-WERKS.

        IF ( SY-SUBRC = 0 ) AND ( WL_MARC-SCHGT IS NOT INITIAL ).
          VL_SHPUNT = 'A GRANEL'.
        ELSE.
          SELECT SINGLE *
            FROM ZSDT0119 INTO WL_ZSDT0119
           WHERE BUKRS = VJ_1BNFDOC-BUKRS
             AND WERKS = VJ_1BNFDOC-BRANCH
             AND MATKL = WL_1BNFLIN-MATKL.

          IF ( SY-SUBRC = 0 ) AND ( WL_ZSDT0119-MEINH IS NOT INITIAL ).
            SELECT SINGLE *
              FROM MARM INTO WL_MARM
             WHERE MATNR = WL_1BNFLIN-MATNR
               AND MEINH = WL_ZSDT0119-MEINH.

            IF SY-SUBRC = 0.
              IF ( VZSDT0001-PESO_LIQ > 0 ) AND (  WL_MARM-UMREZ > 0 ).
                VALOR_D    = VZSDT0001-PESO_LIQ / WL_MARM-UMREZ.
                VL_UMREZ_S = WL_MARM-UMREZ.
                CONDENSE WL_MARM-MEINH NO-GAPS.
                CONDENSE VL_UMREZ_S NO-GAPS.
                CLEAR: VL_SHPUNT.
                CONCATENATE WL_MARM-MEINH VL_UMREZ_S 'KG' INTO VL_SHPUNT SEPARATED BY SPACE.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.

      PERFORM CTNAB  USING VOL P_NFE.
      PERFORM CTNAFV USING VOLPESOL VZSDT0001-PESO_FISCAL P_NFE.
      PERFORM CTNAFV USING VOLPESOB VZSDT0001-PESO_FISCAL P_NFE.
      PERFORM CTNAFV USING VOLQVOL  VALOR_D P_NFE.
      PERFORM CTNAF  USING VOLESP   VL_SHPUNT     P_NFE.
      PERFORM CTNFE  USING VOL P_NFE.



    ENDIF.
  ELSEIF TBVOL[] IS INITIAL.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        INPUT                = VJ_1BNFDOC-NTGEW
        UNIT_IN              = VJ_1BNFDOC-GEWEI
        UNIT_OUT             = 'KG'
      IMPORTING
        OUTPUT               = LV_NTGEW
      EXCEPTIONS
        CONVERSION_NOT_FOUND = 1
        DIVISION_BY_ZERO     = 2
        INPUT_INVALID        = 3
        OUTPUT_INVALID       = 4
        OVERFLOW             = 5
        TYPE_INVALID         = 6
        UNITS_MISSING        = 7
        UNIT_IN_NOT_FOUND    = 8
        UNIT_OUT_NOT_FOUND   = 9
        OTHERS               = 10.
    IF SY-SUBRC EQ 0.
      WA_TBVOL-T4_PESOL = LV_NTGEW.
    ENDIF.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        INPUT                = VJ_1BNFDOC-BRGEW
        UNIT_IN              = VJ_1BNFDOC-GEWEI
        UNIT_OUT             = 'KG'
      IMPORTING
        OUTPUT               = LV_BRGEW
      EXCEPTIONS
        CONVERSION_NOT_FOUND = 1
        DIVISION_BY_ZERO     = 2
        INPUT_INVALID        = 3
        OUTPUT_INVALID       = 4
        OVERFLOW             = 5
        TYPE_INVALID         = 6
        UNITS_MISSING        = 7
        UNIT_IN_NOT_FOUND    = 8
        UNIT_OUT_NOT_FOUND   = 9
        OTHERS               = 10.

    IF SY-SUBRC EQ 0.
      WA_TBVOL-T4_PESOB = LV_BRGEW.
    ENDIF.

    IF NOT ( VJ_1BNFDOC-BRGEW IS INITIAL ).

      CLEAR: VALOR_S, VALOR_D.

      VALOR_D = LV_BRGEW.
      VALOR_S = VALOR_D.

      REPLACE '.' IN VALOR_S WITH ' '.


    ENDIF.

    IF ( NOT WA_TBVOL-T4_PESOL IS INITIAL ) OR ( NOT WA_TBVOL-T4_PESOB IS INITIAL ).
      PERFORM CTNAB  USING VOL P_NFE.
      PERFORM CTNAFV USING VOLPESOL   WA_TBVOL-T4_PESOL P_NFE.
      PERFORM CTNAFV USING VOLPESOB   WA_TBVOL-T4_PESOB P_NFE.
      PERFORM CTNAFV USING VOLQVOL    VALOR_S P_NFE.
      PERFORM CTNAF  USING VOLESP     WA_TBVOL-T4_ESP   P_NFE.
      PERFORM CTNFE  USING VOL P_NFE.
    ENDIF.

  ENDIF.

  PERFORM CTNFE USING TRANSP  P_NFE.

ENDFORM.                    " TRANSP

*&---------------------------------------------------------------------*
*&      Form  COBR
*&---------------------------------------------------------------------*
FORM COBR TABLES TBDUB TYPE J1B_NF_XML_U2_TAB
           USING P_NFE TYPE ZXML
                 TB    TYPE J1B_NF_XML_HEADER.

  DATA: XDVENC TYPE C LENGTH 10.
  DATA: WA_DUB TYPE J1B_NF_XML_U3.

  IF ( TB-COBR IS NOT INITIAL ).
    PERFORM CTNAB USING COBR  P_NFE.

    PERFORM CTNAB USING FAT   P_NFE.
    PERFORM CTNAFV USING FATNFAT  TB-NFAT  P_NFE.
    PERFORM CTNAFV USING FATVORIG TB-VORIG P_NFE.
    PERFORM CTNAFV USING FATVDESC TB-VDESC P_NFE.
    PERFORM CTNAFV USING FATVLIQ  TB-VLIQ  P_NFE.

    LOOP AT TBDUB INTO WA_DUB.
      IF ( WA_DUB-NDUP IS NOT INITIAL ) OR ( WA_DUB-DVENC IS NOT INITIAL ) OR ( WA_DUB-VDUP IS NOT INITIAL ).
        PERFORM CTNAB  USING FATDUP P_NFE.
        PERFORM CTNAFV USING FATNDUP WA_DUB-NDUP P_NFE.
        IF WA_DUB-DVENC IS NOT INITIAL.
          CONCATENATE  WA_DUB-DVENC(4) '-' WA_DUB-DVENC+4(2) '-' WA_DUB-DVENC+6(2) INTO XDVENC.
          PERFORM CTNAF  USING FATDVENC XDVENC P_NFE.
        ENDIF.
        PERFORM CTNAFV USING FATVDUP WA_DUB-VDUP P_NFE.
        PERFORM CTNFE  USING FATDUP P_NFE.
      ENDIF.
    ENDLOOP.

    PERFORM CTNFE USING FAT   P_NFE.
    PERFORM CTNFE USING COBR  P_NFE.
  ENDIF.

ENDFORM.                    " COBR

*&---------------------------------------------------------------------*
*&      Form  INFADIC
*&---------------------------------------------------------------------*
FORM INFADIC USING P_NFE  TB TYPE J1B_NF_XML_HEADER .

  DATA: WL_PC_VEICULO TYPE Y_PC_VEICULO,
        VL_TEXTO      TYPE STRING.

  IF ( TB-INFADFISCO_V2 IS NOT INITIAL ) OR ( TB-INFCOMP IS NOT INITIAL ).
    PERFORM CTNAB  USING INFADIC    P_NFE.
    VG_LIMPO = TB-INFADFISCO_V2.
    VG_LIMPO = ZCL_STRING=>CONVERT_TO_UTF8( ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = VG_LIMPO ) ).

    REPLACE ALL OCCURRENCES OF '<' IN VG_LIMPO WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN VG_LIMPO WITH '&gt;'.
    PERFORM CTNAF  USING INFADFISCO VG_LIMPO P_NFE.
    VG_LIMPO = TB-INFCOMP.
    VG_LIMPO = ZCL_STRING=>CONVERT_TO_UTF8( ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = VG_LIMPO ) ).
    REPLACE ALL OCCURRENCES OF '<' IN VG_LIMPO WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN VG_LIMPO WITH '&gt;'.
    PERFORM CTNAF  USING INFCPI     VG_LIMPO    P_NFE.

    IF ( VG_VERSAO_NFE >= 4    ).    "NF-e 4.0 13.04.2018
      PERFORM GET_PLACA_VEIC_NFE USING TB
                              CHANGING WL_PC_VEICULO.

      IF WL_PC_VEICULO IS NOT INITIAL.
        PERFORM CTNAB USING OBSCONT P_NFE.

        PERFORM CTNAV  USING XCAMPO 'Placa Veiculo' P_NFE.

        VL_TEXTO =  WL_PC_VEICULO-PLACA && '-' && WL_PC_VEICULO-UF.
        IF WL_PC_VEICULO-RNTC  IS NOT INITIAL.
          VL_TEXTO = VL_TEXTO && '/ RNTC:' && WL_PC_VEICULO-RNTC.
        ENDIF.

        PERFORM CTNAV  USING XTEXTO VL_TEXTO  P_NFE.
        PERFORM CTNFE  USING OBSCONT    P_NFE.
      ENDIF.

      IF WL_PC_VEICULO-PLACA_CAR1 IS NOT INITIAL.
        PERFORM CTNAB USING OBSCONT P_NFE.

        PERFORM CTNAV  USING XCAMPO 'Placa Reboque 1' P_NFE.

        VL_TEXTO =  WL_PC_VEICULO-PLACA_CAR1 && '-' && WL_PC_VEICULO-UF_CAR1.

        PERFORM CTNAV  USING XTEXTO VL_TEXTO  P_NFE.
        PERFORM CTNFE  USING OBSCONT    P_NFE.
      ENDIF.

      IF WL_PC_VEICULO-PLACA_CAR2 IS NOT INITIAL.
        PERFORM CTNAB USING OBSCONT P_NFE.

        PERFORM CTNAV  USING XCAMPO 'Placa Reboque 2' P_NFE.

        VL_TEXTO =  WL_PC_VEICULO-PLACA_CAR2 && '-' && WL_PC_VEICULO-UF_CAR2.

        PERFORM CTNAV  USING XTEXTO VL_TEXTO  P_NFE.
        PERFORM CTNFE  USING OBSCONT    P_NFE.
      ENDIF.

      IF WL_PC_VEICULO-PLACA_CAR3 IS NOT INITIAL.
        PERFORM CTNAB USING OBSCONT P_NFE.

        PERFORM CTNAV  USING XCAMPO 'Placa Reboque 3' P_NFE.

        VL_TEXTO =  WL_PC_VEICULO-PLACA_CAR3 && '-' && WL_PC_VEICULO-UF_CAR3.

        PERFORM CTNAV  USING XTEXTO VL_TEXTO  P_NFE.
        PERFORM CTNFE  USING OBSCONT    P_NFE.
      ENDIF.

      IF WL_PC_VEICULO-CPF_MOT IS NOT INITIAL.
        PERFORM CTNAB USING OBSCONT P_NFE.

        PERFORM CTNAV  USING XCAMPO 'Motorista' P_NFE.

        CONCATENATE WL_PC_VEICULO-CPF_MOT '-' WL_PC_VEICULO-NOME_MOT INTO VL_TEXTO.

        CONCATENATE 'CPF:' VL_TEXTO INTO VL_TEXTO  SEPARATED BY SPACE.

        PERFORM CTNAV  USING XTEXTO VL_TEXTO  P_NFE.
        PERFORM CTNFE  USING OBSCONT    P_NFE.
      ENDIF.

      IF ( TB-T3_PLACA IS NOT INITIAL ).
        PERFORM CTNAB USING OBSCONT P_NFE.

        PERFORM CTNAV USING XCAMPO 'Placa Reboque' P_NFE.

        VL_TEXTO =  TB-T3_PLACA && '-' && TB-T3_UF1.
        IF TB-T3_RNTC  IS NOT INITIAL.
          VL_TEXTO = VL_TEXTO && '/ RNTC:' && TB-T3_RNTC.
        ENDIF.

        PERFORM CTNAV  USING XTEXTO VL_TEXTO  P_NFE.
        PERFORM CTNFE  USING OBSCONT    P_NFE.
      ENDIF.
    ENDIF.

    PERFORM CTNFE  USING INFADIC    P_NFE.
  ENDIF.

ENDFORM.                    " INFADIC

*&---------------------------------------------------------------------*
*&      Form  EXPORTA
*&---------------------------------------------------------------------*
FORM EXPORTA  USING    P_NFE  TB TYPE J1B_NF_XML_HEADER .

  DATA:  LW_ZNFECOMEX TYPE ZNFECOMEX.


  IF ( TB-COMEX IS NOT INITIAL ).

    "Ajuste para NF-e 3.1
    CALL FUNCTION 'Z_SD_INFO_NFE_EXPORTACAO'
      EXPORTING
        P_DOCNUM       = TB-DOCNUM
      IMPORTING
        E_ZNFECOMEX    = LW_ZNFECOMEX
      EXCEPTIONS
        NAO_LOCALIZADO = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM CTNAB  USING EXPORTA P_NFE.

*    PERFORM CTNAV  USING UFEMBARQ   TB-UFEMBARQ   P_NFE.
*    PERFORM CTNAV  USING XLOCEMBARQ TB-XLOCEMBARQ P_NFE.

    "Alterar o nome das TAGS acima para as que estão abaixo.
    PERFORM CTNAV  USING UFSAIDAPAIS  LW_ZNFECOMEX-UFEMBARQ   P_NFE.
    PERFORM CTNAV  USING XLOCEXPORTA  LW_ZNFECOMEX-XLOCEMBARQ P_NFE.
    PERFORM CTNAV  USING XLOCDESPACHO LW_ZNFECOMEX-NAME1 P_NFE.

    PERFORM CTNFE  USING EXPORTA P_NFE.

    CLEAR: LW_ZNFECOMEX.

  ENDIF.
ENDFORM.                    " EXPORTA

*&---------------------------------------------------------------------*
*&      Form  COMPRA
*&---------------------------------------------------------------------*
FORM COMPRA  USING    P_NFE  TB TYPE J1B_NF_XML_HEADER .
  IF ( TB-XPED IS NOT INITIAL ) OR ( TB-XCONT IS NOT INITIAL ).
    PERFORM CTNAB  USING COMPRA P_NFE.
    PERFORM CTNAF  USING XPED  TB-XPED P_NFE.
    PERFORM CTNAF  USING XCONT TB-XCONT P_NFE.
    PERFORM CTNFE  USING COMPRA P_NFE.
  ENDIF.
ENDFORM.                    " COMPRA

*&---------------------------------------------------------------------*
*&      Form  DET
*&---------------------------------------------------------------------*
FORM DET  TABLES   TBITENS  TYPE J1B_NF_XML_ITEM_TAB
                   TBIMPORT TYPE J1B_NF_XML_H4_TAB
          USING    P_NFE
                   P_REFKEY  TYPE J_1BREFKEY
                   TB        TYPE J1B_NF_XML_HEADER
                   P_XML_DET_EXP01 TYPE ZXML
                   P_XML_DET_EXP02 TYPE ZXML
                   P_XML_DET_EXP03 TYPE ZXML.

  TYPES: BEGIN OF TY_MARA,
           MATNR   TYPE MARA-MATNR,
           MATKL   TYPE MARA-MATKL,
           VALFROM TYPE SETLEAF-VALFROM,
         END OF TY_MARA.

  DATA: WA_ITNS    TYPE J1B_NF_XML_ITEM,
        XDIDDI     TYPE C LENGTH 10,
        XDDESEMB   TYPE C LENGTH 10,
        WA_IMP     TYPE J1B_NF_XML_H4,
        WA_IMP_AUX TYPE J1B_NF_XML_H4.

  DATA: LW_J_1BNFLIN TYPE J_1BNFLIN.

  DATA: WA_ZSDT0098 TYPE ZSDT0098.

  DATA: BEGIN OF WA_IMPORT.
          INCLUDE TYPE J1B_NF_XML_H4.
        DATA: END OF WA_IMPORT.

  DATA: IT_IMPORT     LIKE STANDARD TABLE OF WA_IMPORT,
        IT_IMPORT_AUX LIKE STANDARD TABLE OF WA_IMPORT,
        VG_TAXLW1     TYPE J_1BTAXLW1,
        WA_J_1BNFDOC  TYPE J_1BNFDOC.

  DATA: P_PAIS_DEF  TYPE  LAND1,
        P_REGIO_DEF TYPE  REGIO.

  DATA: V_UNCOM(16)   TYPE   P DECIMALS 10,
        V_VUNTRIB(16) TYPE   P DECIMALS 10.

  DATA: V_QTRIB TYPE P DECIMALS 4.

  DATA: COD_PRODUDO TYPE I.
  CLEAR: IT_IMPORT.

  DATA: IT_VBRK TYPE TABLE OF VBRK,
        WA_VBRK TYPE VBRK.

  DATA: IT_VBFA TYPE TABLE OF VBFA,
        WA_VBFA TYPE VBFA.

  DATA: IT_ZDOC_EXP TYPE TABLE OF ZDOC_EXP,
        WA_ZDOC_EXP TYPE ZDOC_EXP.

  DATA: IT_ZDOC_NF_PRODUTOR TYPE TABLE OF ZDOC_NF_PRODUTOR,
        WA_ZDOC_NF_PRODUTOR TYPE ZDOC_NF_PRODUTOR.


  DATA: IT_MARA TYPE TABLE OF TY_MARA,
        WA_MARA TYPE TY_MARA.

  FIELD-SYMBOLS: <FS_MARA> TYPE TY_MARA.


  DATA: VAR_CHAVE TYPE C LENGTH 44.

  DATA: LOBJ_UTIL TYPE REF TO ZCL_UTIL.

  DATA: IT_SETLEAF TYPE  TABLE OF SETLEAF.

  LOOP AT TBIMPORT INTO WA_IMPORT.
    APPEND WA_IMPORT TO IT_IMPORT.
  ENDLOOP.

  VAR_TOTAL_VBICM_51 = 0.

  SORT IT_IMPORT BY DOCNUM ITMNUM NDI.

  LOOP AT TBITENS INTO WA_ITNS.

    "Inicio da Tag de Produtos
    PERFORM CTNAB2 USING DET NITEM WA_ITNS-NITEM P_NFE.
    "Tag de Informações Adicionais do Produto

    PERFORM CTNAF  USING INFADPROD   WA_ITNS-INFADPROD P_NFE.
    COD_PRODUDO = WA_ITNS-CPROD.
    "Inicio da Tag de Produto
    PERFORM CTNAB  USING DETPROD P_NFE.
    PERFORM CTNAVN USING DETCPROD   COD_PRODUDO      P_NFE.
    "PERFORM ctnaf  USING detcean     wa_itns-cean     p_nfe.

    PERFORM CTNAF  USING DETCEAN     'SEM GTIN'      P_NFE. "CS2018001871 10.07.2018

    PERFORM CTNAV  USING DETXPROD    WA_ITNS-XPROD    P_NFE.
    REPLACE ALL OCCURRENCES OF '.' IN WA_ITNS-NCM WITH '' IGNORING CASE.
    PERFORM CTNAF  USING DETNCM      WA_ITNS-NCM      P_NFE.
    "PERFORM ctnaf  USING detextipi   wa_itns-extipi   p_nfe.
    "PERFORM ctnaf  USING detgenero   wa_itns-genero   p_nfe.
    PERFORM CTNAV  USING DETCFOP     WA_ITNS-CFOP     P_NFE.
    PERFORM CTNAF  USING DETUCOM     WA_ITNS-UCOM     P_NFE.
    PERFORM CTNAVN USING DETQCOM     WA_ITNS-QCOM     P_NFE.

    CLEAR: WA_ZSDT0098.

    SELECT SINGLE *
      FROM J_1BNFLIN INTO @DATA(WL_LIN)
     WHERE DOCNUM EQ @WA_ITNS-DOCNUM
       AND ITMNUM EQ @WA_ITNS-ITMNUM.

    IF SY-SUBRC EQ 0.
      SELECT SINGLE *
        FROM ZSDT0098 INTO WA_ZSDT0098
       WHERE TAXLW1 EQ WL_LIN-TAXLW1
         AND CFOP   EQ WL_LIN-CFOP(4).

      IF SY-SUBRC IS NOT INITIAL.
        SELECT SINGLE *
          FROM ZSDT0098 INTO WA_ZSDT0098
         WHERE TAXLW1 EQ WL_LIN-TAXLW1.
      ENDIF.

      IF SY-SUBRC EQ 0 AND WA_ZSDT0098-CBENF IS NOT INITIAL.
        PERFORM CTNAVN USING 'cBenef'  WA_ZSDT0098-CBENF  P_NFE.
      ENDIF.
    ENDIF.

    CLEAR: V_QTRIB.

    IF NOT ( IT_SETLEAF[] IS INITIAL ) AND ( WA_ITNS-UTRIB EQ 'SAC' ).

      WA_ITNS-UTRIB = 'KG'.
      PERFORM CTNAF  USING DETUTRIB    WA_ITNS-UTRIB    P_NFE.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          I_MATNR              = WA_ITNS-CPROD
          I_IN_ME              = 'BAG'
          I_OUT_ME             = 'KG'
          I_MENGE              = WA_ITNS-QTRIB
        IMPORTING
          E_MENGE              = WA_ITNS-QTRIB
        EXCEPTIONS
          ERROR_IN_APPLICATION = 1
          ERROR                = 2
          OTHERS               = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      PERFORM CTNAVN USING DETQTRIB    WA_ITNS-QTRIB    P_NFE.
    ELSE.

      V_QTRIB = WA_ITNS-QTRIB.

      IF ( WA_ITNS-UCOM = 'KG' ).
        IF ( WA_ITNS-UTRIB = 'TON' ) AND ( WA_ITNS-QCOM > 0 ).
          V_QTRIB = WA_ITNS-QCOM / 1000.
        ENDIF.
      ENDIF.

      PERFORM CTNAF  USING DETUTRIB    WA_ITNS-UTRIB    P_NFE.
      PERFORM CTNAVN USING DETQTRIB    V_QTRIB          P_NFE.
    ENDIF.

    IF WA_ITNS-QCOM GT 0.
      V_UNCOM =  WA_ITNS-VPROD / WA_ITNS-QCOM.
    ENDIF.

    PERFORM CTNAVN USING DETVUNCOM   V_UNCOM   P_NFE.

    PERFORM CEST USING P_NFE COD_PRODUDO.

    PERFORM CTNAVN USING DETVPROD    WA_ITNS-VPROD    P_NFE.

    PERFORM CTNAF  USING DETCEANTRIB 'SEM GTIN'      P_NFE. "CS2018001871 10.07.2018


    IF NOT ( IT_SETLEAF[] IS INITIAL ) AND ( WA_ITNS-UTRIB EQ 'SAC' ).
      V_VUNTRIB  =   WA_ITNS-VPROD / WA_ITNS-QTRIB.
    ELSE.
      IF V_QTRIB GT 0.
        V_VUNTRIB  = WA_ITNS-VPROD / V_QTRIB.
      ELSE.
        IF WA_ITNS-QTRIB GT 0.
          V_VUNTRIB  = WA_ITNS-VPROD / WA_ITNS-QTRIB.
        ENDIF.
      ENDIF.
    ENDIF.

    PERFORM CTNAVN USING DETVUNTRIB  V_VUNTRIB   P_NFE.
    PERFORM CTNAFV USING DETVFRETE   WA_ITNS-VFRETE   P_NFE.

    IF VG_XML_VERSAO EQ 2.
      PERFORM CTNAV  USING DETINDTOT  C_1  P_NFE.
    ENDIF.

    "Notas de Complemento para Fim Especifico.
    IF ( WA_ITNS-CFOP = '7501' ) OR ( WA_ITNS-CFOP = '3503' ).

      IF ( TB-FINNFE EQ '1' ).
        PERFORM DADOS_COMP USING P_REFKEY
                                 P_NFE
                                 P_XML_DET_EXP01
                                 P_XML_DET_EXP02
                                 P_XML_DET_EXP03.
      ENDIF.

    ENDIF.

    PERFORM CTNAFV USING DETVSEG     WA_ITNS-VSEG   P_NFE.

    "Ajuste para adicionar na tag vDesc o valor do desconto por item.
    IF NOT ( WA_ITNS-VDESC IS INITIAL ).
      PERFORM CTNAFV USING DETVDESC WA_ITNS-VDESC P_NFE.
      ADD WA_ITNS-VDESC TO VAR_TOTAL_DESC.
    ENDIF.

    "Ajuste para adicionar na tag vOutro por item.
    SELECT SINGLE * INTO LW_J_1BNFLIN FROM J_1BNFLIN
       WHERE DOCNUM EQ WA_ITNS-DOCNUM
         AND ITMNUM EQ WA_ITNS-ITMNUM.

    IF NOT ( LW_J_1BNFLIN-NETOTH IS INITIAL ).
      PERFORM CTNAFV USING VOUTRO   LW_J_1BNFLIN-NETOTH P_NFE.
      VAR_TOTAL_OUTROS = VAR_TOTAL_OUTROS + LW_J_1BNFLIN-NETOTH.
    ENDIF.

    MOVE IT_IMPORT[] TO IT_IMPORT_AUX[].

    LOOP AT IT_IMPORT INTO WA_IMP WHERE ITMNUM = WA_ITNS-ITMNUM.

      "Inicio da Tag de Declarações de Importação
      PERFORM CTNAB  USING DI P_NFE.
      PERFORM CTNAF  USING DINDI         WA_IMP-NDI         P_NFE.
      CONCATENATE  WA_IMP-DDI(4)     '-' WA_IMP-DDI+4(2) '-' WA_IMP-DDI+6(2) INTO XDIDDI.
      PERFORM CTNAF  USING DIDDI         XDIDDI             P_NFE.
      PERFORM CTNAF  USING DIXLOCDESEMB  WA_IMP-XLOCDESEMB  P_NFE.
      PERFORM CTNAF  USING DIUFDESEMB    WA_IMP-UFDESEMB    P_NFE.
      CONCATENATE  WA_IMP-DDESEMB(4) '-' WA_IMP-DDESEMB+4(2) '-' WA_IMP-DDESEMB+6(2) INTO XDDESEMB.
      PERFORM CTNAF  USING DIDDESEMB     XDDESEMB           P_NFE.
      PERFORM CTNAF  USING DICEXPORTADOR WA_IMP-CEXPORTADOR P_NFE.
      PERFORM DADOS_DI_COMPLEMENTO USING WA_ITNS-DOCNUM  WA_ITNS-ITMNUM  WA_IMP-NDI  P_NFE.

      LOOP AT IT_IMPORT_AUX INTO WA_IMP_AUX WHERE ITMNUM EQ WA_ITNS-ITMNUM
                                              AND NDI    EQ WA_IMP-NDI.
        "Inicio da Tag de Adições da Declarações de Importação
        PERFORM CTNAB  USING ADI P_NFE.
        PERFORM CTNAFV USING NADICAO     WA_IMP_AUX-NADICAO     P_NFE.
        PERFORM CTNAFV USING NSEQADIC    WA_IMP_AUX-NSEQADIC    P_NFE.
        PERFORM CTNAF  USING CFABRICANTE WA_IMP_AUX-CFABRICANTE P_NFE.
        PERFORM CTNAFV USING VDESCDI     WA_IMP_AUX-VDESCDI     P_NFE.
        "Fim da Tag de Adições da Declarações de Importação
        PERFORM CTNFE  USING ADI P_NFE.
      ENDLOOP.

    ENDLOOP.

    FREE: LOBJ_UTIL.

    READ TABLE IT_IMPORT INTO WA_IMP WITH KEY ITMNUM = WA_ITNS-ITMNUM.
    IF SY-SUBRC IS INITIAL.
      PERFORM CTNFE  USING DI P_NFE.
    ENDIF.

    IF WA_ITNS-XPED IS NOT INITIAL.
      PERFORM CTNAV  USING DETXPED    WA_ITNS-XPED   P_NFE.
    ENDIF.

    "Fim da tag do Produto
    PERFORM CTNFE  USING DETPROD P_NFE.

    CLEAR: VG_VENDA_MERC_RO,
           VG_VENDA_MERC_MA,
           VG_VENDA_MERC_TO,
           VG_VENDA_MERC_PI,
           VG_VENDA_MERC_MT.

    SELECT SINGLE * INTO WA_J_1BNFDOC
      FROM J_1BNFDOC
     WHERE DOCNUM EQ WA_ITNS-DOCNUM.

    IF ( WA_J_1BNFDOC-PARTYP EQ C_C ).

      SELECT SINGLE TAXLW1 INTO VG_TAXLW1
        FROM J_1BNFLIN
       WHERE DOCNUM EQ WA_ITNS-DOCNUM
         AND ITMNUM EQ WA_ITNS-ITMNUM.

      IF ( SY-SUBRC IS INITIAL ).

        CASE VG_TAXLW1.
          WHEN C_R58.

            P_PAIS_DEF  = C_BR.
            P_REGIO_DEF = C_RO.

            CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
              EXPORTING
                P_BUKRS      = WA_J_1BNFDOC-BUKRS
                P_BRANCH     = WA_J_1BNFDOC-BRANCH
                P_PARTYP_1   = WA_J_1BNFDOC-PARTYP
                P_PARID_1    = WA_J_1BNFDOC-PARID
                P_PAIS_DEF   = P_PAIS_DEF
                P_REGIO_DEF  = P_REGIO_DEF
              IMPORTING
                P_DENTRO_DEF = VG_VENDA_MERC_RO.

          WHEN C_O46 OR C_O48 OR C_O50.

            P_PAIS_DEF  = C_BR.
            P_REGIO_DEF = C_MA.

            CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
              EXPORTING
                P_BUKRS      = WA_J_1BNFDOC-BUKRS
                P_BRANCH     = WA_J_1BNFDOC-BRANCH
                P_PARTYP_1   = WA_J_1BNFDOC-PARTYP
                P_PARID_1    = WA_J_1BNFDOC-PARID
                P_PAIS_DEF   = P_PAIS_DEF
                P_REGIO_DEF  = P_REGIO_DEF
              IMPORTING
                P_DENTRO_DEF = VG_VENDA_MERC_MA.

          WHEN C_O47 OR C_O49 OR C_O51 .

            P_PAIS_DEF  = C_BR.
            P_REGIO_DEF = C_MA.

            CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
              EXPORTING
                P_BUKRS             = WA_J_1BNFDOC-BUKRS
                P_BRANCH            = WA_J_1BNFDOC-BRANCH
                P_PARTYP_1          = WA_J_1BNFDOC-PARTYP
                P_PARID_1           = WA_J_1BNFDOC-PARID
                P_PAIS_DEF          = P_PAIS_DEF
                P_REGIO_DEF         = P_REGIO_DEF
              IMPORTING
                P_INTERESTADUAL_DEF = VG_VENDA_MERC_MA.

          WHEN C_T45 OR C_T47 OR C_T49.

            P_PAIS_DEF  = C_BR.
            P_REGIO_DEF = C_TO.

            CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
              EXPORTING
                P_BUKRS      = WA_J_1BNFDOC-BUKRS
                P_BRANCH     = WA_J_1BNFDOC-BRANCH
                P_PARTYP_1   = WA_J_1BNFDOC-PARTYP
                P_PARID_1    = WA_J_1BNFDOC-PARID
                P_PAIS_DEF   = P_PAIS_DEF
                P_REGIO_DEF  = P_REGIO_DEF
              IMPORTING
                P_DENTRO_DEF = VG_VENDA_MERC_TO.

          WHEN C_T46 OR C_T48 OR C_T50.

            P_PAIS_DEF  = C_BR.
            P_REGIO_DEF = C_TO.

            CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
              EXPORTING
                P_BUKRS             = WA_J_1BNFDOC-BUKRS
                P_BRANCH            = WA_J_1BNFDOC-BRANCH
                P_PARTYP_1          = WA_J_1BNFDOC-PARTYP
                P_PARID_1           = WA_J_1BNFDOC-PARID
                P_PAIS_DEF          = P_PAIS_DEF
                P_REGIO_DEF         = P_REGIO_DEF
              IMPORTING
                P_INTERESTADUAL_DEF = VG_VENDA_MERC_TO.

          WHEN C_PI4 OR C_PI7.

            P_PAIS_DEF  = C_BR.
            P_REGIO_DEF = C_PI.

            CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
              EXPORTING
                P_BUKRS      = WA_J_1BNFDOC-BUKRS
                P_BRANCH     = WA_J_1BNFDOC-BRANCH
                P_PARTYP_1   = WA_J_1BNFDOC-PARTYP
                P_PARID_1    = WA_J_1BNFDOC-PARID
                P_PAIS_DEF   = P_PAIS_DEF
                P_REGIO_DEF  = P_REGIO_DEF
              IMPORTING
                P_DENTRO_DEF = VG_VENDA_MERC_PI.

          WHEN C_PI5 OR C_PI6 OR C_PI8.

            P_PAIS_DEF  = C_BR.
            P_REGIO_DEF = C_PI.

            CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
              EXPORTING
                P_BUKRS             = WA_J_1BNFDOC-BUKRS
                P_BRANCH            = WA_J_1BNFDOC-BRANCH
                P_PARTYP_1          = WA_J_1BNFDOC-PARTYP
                P_PARID_1           = WA_J_1BNFDOC-PARID
                P_PAIS_DEF          = P_PAIS_DEF
                P_REGIO_DEF         = P_REGIO_DEF
              IMPORTING
                P_INTERESTADUAL_DEF = VG_VENDA_MERC_PI.

          WHEN C_M86.

            P_PAIS_DEF  = C_BR.
            P_REGIO_DEF = C_RO.

            VG_VENDA_MERC_RO = 'X'.

        ENDCASE.

      ENDIF.

    ENDIF.

    "Impostos do item
    PERFORM IMPOSTO USING WA_ITNS P_NFE.

    "Fim da Tag de Produtos
    PERFORM CTNFE  USING DET   P_NFE.

    CLEAR: LW_J_1BNFLIN.
  ENDLOOP.

ENDFORM.                    " DET

*&---------------------------------------------------------------------*
*&      Form  IMPOSTO
*&---------------------------------------------------------------------*
FORM IMPOSTO  USING    WA_ITNS  TYPE J1B_NF_XML_ITEM  P_NFE.

  DATA: VAR_EXCBAS TYPE J_1BNFSTX-EXCBAS.

  "Fim da Tag de ICMS
  PERFORM CTNAB  USING IMPOSTO   P_NFE.
  "Tag do Grupo de informações de ICMS
  PERFORM IMPOSTOICMS USING WA_ITNS P_NFE.
  "Tag do Grupo de Informação do ICMS Interestadual
  PERFORM IMPOSTOICMSINTER USING WA_ITNS P_NFE.
  "Tag do Grupo de informações de IPI
  PERFORM IMPOSTOIPI USING WA_ITNS P_NFE.
  "Tag do Grupo de informações de II
  PERFORM IMPOSTOII USING WA_ITNS P_NFE.
  "Tag do Grupo de informações de PIS
  PERFORM IMPOSTOPIS USING WA_ITNS P_NFE.
  "Tag do Grupo de informações de COFINS
  PERFORM IMPOSTOCOFINS USING WA_ITNS P_NFE.
  "Tag do Grupo de informações de ISSQN
  PERFORM IMPOSTOISSQN USING WA_ITNS P_NFE.
  "Valor Total dos Tributos
  PERFORM: VLRTOTTRIB USING WA_ITNS P_NFE.
  "Fim da Tag de ICMS
  PERFORM CTNFE  USING IMPOSTO   P_NFE.

ENDFORM.                    " IMPOSTO

*&---------------------------------------------------------------------*
*&      Form  IMPOSTOICMS
*&---------------------------------------------------------------------*
*       Impostos Sobre Circulação de Mercadorias e Serviços
*----------------------------------------------------------------------*
FORM IMPOSTOICMS  USING   WA_ITNS  TYPE J1B_NF_XML_ITEM  P_NFE.

  DATA: VG_TAXLW1 TYPE J_1BNFLIN-TAXLW1,
        VG_MOTIVO TYPE ZSDT0098-MOTIVO.

  DATA: WL_ITEM_BADI TYPE J1B_NF_XML_BADI_ITEM.

  CLEAR: WL_ITEM_BADI.

  READ TABLE IT_J1B_NF_XML_BADI_ITEM INTO WL_ITEM_BADI WITH KEY DOCNUM = WA_ITNS-DOCNUM
                                                                ITMNUM = WA_ITNS-ITMNUM.

  "-------------------------
  " Ajsute para NF-e 3.1 - INICIO.
  "-------------------------
  "Recuperar a lei fiscal.
  SELECT SINGLE TAXLW1 INTO VG_TAXLW1
    FROM J_1BNFLIN
   WHERE DOCNUM EQ WA_ITNS-DOCNUM
     AND ITMNUM EQ WA_ITNS-ITMNUM.

  "DE-PARA Para Leis Fiscais e Motivo de Desoneração.
  SELECT SINGLE MOTIVO INTO VG_MOTIVO
    FROM ZSDT0098
   WHERE TAXLW1 EQ VG_TAXLW1.
*  " Ajsute para NF-e 3.1 - FIM
*  "-------------------------

  "Fim da Tag de ICMS
  PERFORM CTNAB  USING IMICMS P_NFE.

  IF WA_ITNS-L1_00_ORIG IS NOT INITIAL.
    PERFORM CTNAV  USING IMORIG        WA_ITNS-L1_00_ORIG  P_NFE.
    PERFORM CTNAV  USING IMCST         WA_ITNS-L1_00_CST   P_NFE.
    PERFORM CTNAV  USING IMMODBC       WA_ITNS-L1_00_MODBC P_NFE.
    PERFORM CTNAVN USING IMVBC         WA_ITNS-L1_00_VBC   P_NFE.
    PERFORM CTNAVN USING IMPICMS       WA_ITNS-L1_00_PICMS P_NFE.
    PERFORM CTNAVN USING IMVICMS       WA_ITNS-L1_00_VICMS P_NFE.
  ENDIF.

  IF WA_ITNS-L1_10_ORIG IS NOT INITIAL.
    PERFORM CTNAV  USING IMORIG     WA_ITNS-L1_10_ORIG     P_NFE.
    PERFORM CTNAV  USING IMCST      WA_ITNS-L1_10_CST      P_NFE.
    PERFORM CTNAV  USING IMMODBC    WA_ITNS-L1_10_MODBC    P_NFE.
    PERFORM CTNAVN USING IMVBC      WA_ITNS-L1_10_VBC      P_NFE.
    PERFORM CTNAVN USING IMPICMS    WA_ITNS-L1_10_PICMS    P_NFE.
    PERFORM CTNAVN USING IMVICMS    WA_ITNS-L1_10_VICMS    P_NFE.
    PERFORM CTNAV  USING IMMODBCST  WA_ITNS-L1_10_MODBCST  P_NFE.
    PERFORM CTNAVN USING IMPMVAST   WA_ITNS-L1_10_PMVAST   P_NFE.
    PERFORM CTNAVN USING IMPREDBCST WA_ITNS-L1_10_PREDBCST P_NFE.
    PERFORM CTNAVN USING IMVBCST    WA_ITNS-L1_10_VBCST    P_NFE.
    PERFORM CTNAVN USING IMPICMSST  WA_ITNS-L1_10_PICMSST  P_NFE.
    PERFORM CTNAVN USING IMVICMSST  WA_ITNS-L1_10_VICMSST  P_NFE.
  ENDIF.

  IF WA_ITNS-L1_20_ORIG IS NOT INITIAL.
    PERFORM CTNAV  USING IMORIG   WA_ITNS-L1_20_ORIG   P_NFE.
    PERFORM CTNAV  USING IMCST    WA_ITNS-L1_20_CST    P_NFE.
    PERFORM CTNAV  USING IMMODBC  WA_ITNS-L1_20_MODBC  P_NFE.
    PERFORM CTNAVN USING IMPREDBC WA_ITNS-L1_20_PREDBC P_NFE.
    PERFORM CTNAVN USING IMVBC    WA_ITNS-L1_20_VBC    P_NFE.
    PERFORM CTNAVN USING IMPICMS  WA_ITNS-L1_20_PICMS  P_NFE.
    PERFORM CTNAVN USING IMVICMS  WA_ITNS-L1_20_VICMS  P_NFE.

    IF ( WL_ITEM_BADI-VICMSDESON IS NOT INITIAL ) AND ( VG_MOTIVO IS NOT INITIAL ).
      PERFORM CTNAV  USING IMPMOTDESICMS VG_MOTIVO               P_NFE.
      PERFORM CTNAVN USING IMPVICMSDESON WL_ITEM_BADI-VICMSDESON P_NFE.

      ADD WL_ITEM_BADI-VICMSDESON TO VAR_TOTAL_DESON.
    ENDIF.

  ENDIF.

  IF WA_ITNS-L1_30_ORIG IS NOT INITIAL.
    PERFORM CTNAV  USING IMORIG     WA_ITNS-L1_30_ORIG     P_NFE.
    PERFORM CTNAV  USING IMCST      WA_ITNS-L1_30_CST      P_NFE.
    PERFORM CTNAV  USING IMMODBCST  WA_ITNS-L1_30_MODBCST  P_NFE.
    PERFORM CTNAVN USING IMPMVAST   WA_ITNS-L1_30_PMVAST   P_NFE.
    PERFORM CTNAVN USING IMPREDBCST WA_ITNS-L1_30_PREDBCST P_NFE.
    PERFORM CTNAVN USING IMVBCST    WA_ITNS-L1_30_VBCST    P_NFE.

    IF ( WL_ITEM_BADI-VICMSDESON IS NOT INITIAL ) AND ( VG_MOTIVO IS NOT INITIAL ).
      PERFORM CTNAV  USING IMPMOTDESICMS VG_MOTIVO               P_NFE.
      PERFORM CTNAVN USING IMPVICMSDESON WL_ITEM_BADI-VICMSDESON P_NFE.

      ADD WL_ITEM_BADI-VICMSDESON TO VAR_TOTAL_DESON.
    ENDIF.

    PERFORM CTNAVN USING IMPICMSST  WA_ITNS-L1_30_PICMSST  P_NFE.
    PERFORM CTNAVN USING IMVICMSST  WA_ITNS-L1_30_VICMSST  P_NFE.
  ENDIF.

  IF WA_ITNS-L1_40_ORIG IS NOT INITIAL.
    PERFORM CTNAV  USING IMORIG WA_ITNS-L1_40_ORIG P_NFE.
    PERFORM CTNAV  USING IMCST  WA_ITNS-L1_40_CST  P_NFE.

    IF ( WL_ITEM_BADI-VICMSDESON IS NOT INITIAL ) AND ( VG_MOTIVO IS NOT INITIAL ).
      PERFORM CTNAV  USING IMPMOTDESICMS VG_MOTIVO               P_NFE.
      PERFORM CTNAVN USING IMPVICMSDESON WL_ITEM_BADI-VICMSDESON P_NFE.

      ADD WL_ITEM_BADI-VICMSDESON TO VAR_TOTAL_DESON.
    ENDIF.

  ENDIF.

  IF WA_ITNS-L1_51_ORIG IS NOT INITIAL.
    PERFORM CTNAV  USING IMORIG   WA_ITNS-L1_51_ORIG   P_NFE.
    PERFORM CTNAV  USING IMCST    WA_ITNS-L1_51_CST    P_NFE.
    IF WA_ITNS-L1_51_VICMS GT 0.
      ADD WA_ITNS-L1_51_VBC TO VAR_TOTAL_VBICM_51.
      PERFORM CTNAV  USING IMMODBC  WA_ITNS-L1_51_MODBC  P_NFE.
      PERFORM CTNAVN USING IMPREDBC WA_ITNS-L1_51_PREDBC P_NFE.
      PERFORM CTNAVN USING IMVBC    WA_ITNS-L1_51_VBC    P_NFE.

      PERFORM CTNAVN USING IMPICMS  WA_ITNS-L1_51_PICMS  P_NFE.
      PERFORM CTNAVN USING IMVICMS  0  P_NFE.

      PERFORM CTNAVN USING IMVICMSOP  WA_ITNS-L1_51_VICMS  P_NFE.
      PERFORM CTNAVN USING IMPDIF     100                  P_NFE.
      PERFORM CTNAVN USING IMVICMSDIF WA_ITNS-L1_51_VICMS  P_NFE.
    ENDIF.
  ENDIF.
  IF WA_ITNS-L1_60_ORIG IS NOT INITIAL.
    PERFORM CTNAV  USING IMORIG     WA_ITNS-L1_60_ORIG     P_NFE.
    PERFORM CTNAV  USING IMCST      WA_ITNS-L1_60_CST      P_NFE.
    PERFORM CTNAVN USING IMVBCST    WA_ITNS-L1_60_VBCST    P_NFE.
    PERFORM CTNAVN USING IMVICMSST  WA_ITNS-L1_60_VICMSST  P_NFE.
  ENDIF.

  IF WA_ITNS-L1_70_ORIG IS NOT INITIAL.
    PERFORM CTNAV  USING IMORIG     WA_ITNS-L1_70_ORIG     P_NFE.
    PERFORM CTNAV  USING IMCST      WA_ITNS-L1_70_CST      P_NFE.
    PERFORM CTNAV  USING IMMODBC    WA_ITNS-L1_70_MODBC    P_NFE.
    PERFORM CTNAVN USING IMPREDBC   WA_ITNS-L1_70_PREDBC   P_NFE.
    PERFORM CTNAVN USING IMVBC      WA_ITNS-L1_70_VBC      P_NFE.
    PERFORM CTNAVN USING IMPICMS    WA_ITNS-L1_70_PICMS    P_NFE.
    PERFORM CTNAVN USING IMVICMS    WA_ITNS-L1_70_VICMS    P_NFE.

    IF ( WL_ITEM_BADI-VICMSDESON IS NOT INITIAL ) AND ( VG_MOTIVO IS NOT INITIAL ).
      PERFORM CTNAV  USING IMPMOTDESICMS VG_MOTIVO               P_NFE.
      PERFORM CTNAVN USING IMPVICMSDESON WL_ITEM_BADI-VICMSDESON P_NFE.

      ADD WL_ITEM_BADI-VICMSDESON TO VAR_TOTAL_DESON.
    ENDIF.

    PERFORM CTNAV  USING IMMODBCST  WA_ITNS-L1_70_MODBCST  P_NFE.
    PERFORM CTNAVN USING IMPMVAST   WA_ITNS-L1_70_PMVAST   P_NFE.
    PERFORM CTNAVN USING IMPREDBCST WA_ITNS-L1_70_PREDBCST P_NFE.
    PERFORM CTNAVN USING IMVBCST    WA_ITNS-L1_70_VBCST    P_NFE.
    PERFORM CTNAVN USING IMPICMSST  WA_ITNS-L1_70_PICMSST  P_NFE.
    PERFORM CTNAVN USING IMVICMSST  WA_ITNS-L1_70_VICMSST  P_NFE.
  ENDIF.


  IF WA_ITNS-L1_90_ORIG IS NOT INITIAL.
    PERFORM CTNAV  USING IMORIG     WA_ITNS-L1_90_ORIG     P_NFE.
    PERFORM CTNAV  USING IMCST      WA_ITNS-L1_90_CST      P_NFE.
    PERFORM CTNAV  USING IMMODBC    WA_ITNS-L1_90_MODBC    P_NFE.
    PERFORM CTNAVN USING IMPREDBC   WA_ITNS-L1_90_PREDBC   P_NFE.
    PERFORM CTNAVN USING IMVBC      WA_ITNS-L1_90_VBC      P_NFE.
    PERFORM CTNAVN USING IMPICMS    WA_ITNS-L1_90_PICMS    P_NFE.
    PERFORM CTNAVN USING IMVICMS    WA_ITNS-L1_90_VICMS    P_NFE.

    IF ( WL_ITEM_BADI-VICMSDESON IS NOT INITIAL ) AND ( VG_MOTIVO IS NOT INITIAL ).
      PERFORM CTNAV  USING IMPMOTDESICMS VG_MOTIVO               P_NFE.
      PERFORM CTNAVN USING IMPVICMSDESON WL_ITEM_BADI-VICMSDESON P_NFE.

      ADD WL_ITEM_BADI-VICMSDESON TO VAR_TOTAL_DESON.
    ENDIF.

    PERFORM CTNAV  USING IMMODBCST  WA_ITNS-L1_90_MODBCST  P_NFE.
    PERFORM CTNAVN USING IMPMVAST   WA_ITNS-L1_90_PMVAST   P_NFE.
    PERFORM CTNAVN USING IMPREDBCST WA_ITNS-L1_90_PREDBCST P_NFE.
    PERFORM CTNAVN USING IMVBCST    WA_ITNS-L1_90_VBCST    P_NFE.
    PERFORM CTNAVN USING IMPICMSST  WA_ITNS-L1_90_PICMSST  P_NFE.
    PERFORM CTNAVN USING IMVICMSST  WA_ITNS-L1_90_VICMSST  P_NFE.
  ENDIF.
  "Fim da Tag de ICMS
  PERFORM CTNFE  USING IMICMS P_NFE.

  CLEAR: VG_MOTIVO.

ENDFORM.                    " IMPOSTOICMS

*&---------------------------------------------------------------------*
*&      Form  IMPOSTOIPI
*&---------------------------------------------------------------------*
*       Impostos sobre produtos industrializados
*----------------------------------------------------------------------*
FORM IMPOSTOIPI  USING    WA_ITNS  TYPE J1B_NF_XML_ITEM  P_NFE.

  "Ajuste até para o CENQ até aplicar a nota do SAP EHP 7
  DATA: LW_ZFIT0092  TYPE ZFIT0092.
  DATA: LW_J_1BNFLIN TYPE J_1BNFLIN.
  DATA: IT_J_1BNFSTX TYPE TABLE OF J_1BNFSTX.
  DATA: WA_J_1BNFSTX TYPE J_1BNFSTX.
  DATA: IT_J_1BAJ    TYPE TABLE OF J_1BAJ.
  DATA: WA_J_1BAJ    TYPE J_1BAJ.

  IF WA_ITNS-N_IPI IS NOT INITIAL AND ( WA_ITNS-N2_CST IS NOT INITIAL OR WA_ITNS-N1_CST IS NOT INITIAL ).

    REFRESH: IT_J_1BAJ, IT_J_1BNFSTX.
    CLEAR: WA_J_1BNFSTX.

    PERFORM CTNAB  USING IPI  P_NFE.
    PERFORM CTNAF  USING IPICIENQ    WA_ITNS-N_CLENQ    P_NFE.
    PERFORM CTNAF  USING IPICNPJPROD WA_ITNS-N_CNPJPROD P_NFE.
    PERFORM CTNAF  USING IPICSELO    WA_ITNS-N_CSELO    P_NFE.
    PERFORM CTNAF  USING IPIQSELO    WA_ITNS-N_QSELO    P_NFE.

    "Ajuste até para o CENQ até aplicar a nota do SAP EHP 7
    SELECT SINGLE * FROM J_1BNFLIN INTO LW_J_1BNFLIN WHERE DOCNUM = WA_ITNS-DOCNUM
                                                       AND ITMNUM = WA_ITNS-ITMNUM.

    SELECT SINGLE * FROM ZFIT0092 INTO LW_ZFIT0092 WHERE CST = LW_J_1BNFLIN-TAXLW2.

    IF ( SY-SUBRC EQ 0 ) AND ( WA_ITNS-N2_CST IS NOT INITIAL OR WA_ITNS-N1_CST IS NOT INITIAL ).
      PERFORM CTNAV  USING IPICENQ  LW_ZFIT0092-CENQ   P_NFE.
    ENDIF.

    SELECT *
      FROM J_1BAJ
      INTO TABLE IT_J_1BAJ
     WHERE TAXGRP = 'IPI'.

    SELECT *
      FROM J_1BNFSTX INTO TABLE IT_J_1BNFSTX
       FOR ALL ENTRIES IN IT_J_1BAJ
      WHERE TAXTYP = IT_J_1BAJ-TAXTYP
        AND DOCNUM = WA_ITNS-DOCNUM
        AND ITMNUM = WA_ITNS-ITMNUM.

    READ TABLE IT_J_1BNFSTX INTO WA_J_1BNFSTX INDEX 1.

    IF ( SY-SUBRC EQ 0 ).
      PERFORM CTNAVN  USING IPIVBC  WA_J_1BNFSTX-BASE   P_NFE.
      PERFORM CTNAVN  USING IPIPIPI WA_J_1BNFSTX-RATE   P_NFE.
      PERFORM CTNAVN  USING IPIVIPI WA_J_1BNFSTX-TAXVAL P_NFE.
    ENDIF.

    IF WA_ITNS-N2_CST IS NOT INITIAL.
      PERFORM CTNAV  USING IPICST      WA_ITNS-N2_CST  P_NFE.
    ELSE.
      PERFORM CTNAV  USING IPICST      WA_ITNS-N1_CST  P_NFE.
    ENDIF.

    PERFORM CTNFE  USING IPI  P_NFE.
  ENDIF.

ENDFORM.                    " IMPOSTOIPI

*&---------------------------------------------------------------------*
*&      Form  IMPOSTOII
*&---------------------------------------------------------------------*
*       Impostos de Importação
*----------------------------------------------------------------------*
FORM IMPOSTOII  USING    WA_ITNS  TYPE J1B_NF_XML_ITEM  P_NFE.

  IF WA_ITNS-O_II IS NOT INITIAL.
    PERFORM CTNAB  USING II P_NFE.
    PERFORM CTNAVN USING IIVBC      WA_ITNS-O_VBC      P_NFE.
    PERFORM CTNAVN USING IIVDESPADU WA_ITNS-O_VDESPADU P_NFE.
    PERFORM CTNAVN USING IIVII      WA_ITNS-O_VII      P_NFE.
    PERFORM CTNAVN USING IIVIOF     WA_ITNS-O_VIOF     P_NFE.
    PERFORM CTNFE  USING II P_NFE.
  ENDIF.

ENDFORM.                    " IMPOSTOII

*&---------------------------------------------------------------------*
*&      Form  IMPOSTOPIS
*&---------------------------------------------------------------------*
*       Impostos de Programa de Integração Social
*----------------------------------------------------------------------*
FORM IMPOSTOPIS  USING    WA_ITNS  TYPE J1B_NF_XML_ITEM  P_NFE.

  IF WA_ITNS-P1_CST IS NOT INITIAL.
    PERFORM CTNAB  USING PIS P_NFE.
    PERFORM CTNAFV USING PISCST       WA_ITNS-P1_CST   P_NFE.
    PERFORM CTNAFV USING PISVBC       WA_ITNS-P1_VBC   P_NFE.
    PERFORM CTNAFV USING PISPPIS      WA_ITNS-P1_PPIS  P_NFE.
    PERFORM CTNAFV USING PISVPIS      WA_ITNS-P1_VPIS  P_NFE.
    PERFORM CTNFE  USING PIS P_NFE.
  ELSEIF WA_ITNS-P2_CST IS NOT INITIAL.
    PERFORM CTNAB  USING PIS P_NFE.
    PERFORM CTNAFV USING PISCST       WA_ITNS-P2_CST       P_NFE.
    PERFORM CTNAFV USING PISQBCPROD   WA_ITNS-P2_QBCPROD   P_NFE.
    PERFORM CTNAFV USING PISVALIQPROD WA_ITNS-P2_VALIQPROD P_NFE.
    PERFORM CTNAFV USING PISVPIS      WA_ITNS-P2_VPIS      P_NFE.
    PERFORM CTNFE  USING PIS P_NFE.
  ELSEIF WA_ITNS-P3_CST IS NOT INITIAL.
    PERFORM CTNAB  USING PIS P_NFE.
    PERFORM CTNAFV USING PISCST       WA_ITNS-P3_CST       P_NFE.
    PERFORM CTNFE  USING PIS P_NFE.
  ELSEIF WA_ITNS-P4_CST IS NOT INITIAL.
    PERFORM CTNAB  USING PIS P_NFE.
    PERFORM CTNAFV USING PISCST       WA_ITNS-P4_CST       P_NFE.
    PERFORM CTNAFV USING PISVBC       WA_ITNS-P4_VBC       P_NFE.
    PERFORM CTNAFV USING PISPPIS      WA_ITNS-P4_PPIS      P_NFE.
    PERFORM CTNAFV USING PISQBCPROD   WA_ITNS-P4_QBCPROD   P_NFE.
    PERFORM CTNAFV USING PISVALIQPROD WA_ITNS-P4_VALIQPROD P_NFE.
    PERFORM CTNAFV USING PISVPIS      WA_ITNS-P4_VPIS      P_NFE.
    PERFORM CTNFE  USING PIS P_NFE.
  ELSEIF WA_ITNS-P5_VBC IS NOT INITIAL.
    PERFORM CTNAB  USING PISST P_NFE.
    PERFORM CTNAFV USING PISVBC       WA_ITNS-P5_VBC       P_NFE.
    PERFORM CTNAFV USING PISPPIS      WA_ITNS-P5_PPIS      P_NFE.
    PERFORM CTNAFV USING PISQBCPROD   WA_ITNS-P5_QBCPROD   P_NFE.
    PERFORM CTNAFV USING PISVALIQPROD WA_ITNS-P5_VALIQPROD P_NFE.
    PERFORM CTNAFV USING PISVPIS      WA_ITNS-P5_VPIS      P_NFE.
    PERFORM CTNFE  USING PISST P_NFE.
  ENDIF.

ENDFORM.                    " IMPOSTOPIS

*&---------------------------------------------------------------------*
*&      Form  IMPOSTOCOFINS
*&---------------------------------------------------------------------*
*       Impostos de Contribuição para o Financiamento da Seguridade Social
*----------------------------------------------------------------------*
FORM IMPOSTOCOFINS  USING    WA_ITNS  TYPE J1B_NF_XML_ITEM  P_NFE.

  IF WA_ITNS-Q1_CST IS NOT INITIAL.
    PERFORM CTNAB  USING COFINS P_NFE.
    PERFORM CTNAFV USING COFINSCST       WA_ITNS-Q1_CST     P_NFE.
    PERFORM CTNAFV USING COFINSVBC       WA_ITNS-Q1_VBC     P_NFE.
    PERFORM CTNAFV USING COFINSPCOFINS   WA_ITNS-Q1_PCOFINS P_NFE.
    PERFORM CTNAFV USING COFINSVCOFINS   WA_ITNS-Q1_VCOFINS P_NFE.
    PERFORM CTNFE  USING COFINS P_NFE.
  ELSEIF WA_ITNS-Q2_CST IS NOT INITIAL.
    PERFORM CTNAB  USING COFINS P_NFE.
    PERFORM CTNAFV USING COFINSCST       WA_ITNS-Q2_CST       P_NFE.
    PERFORM CTNAFV USING COFINSQBCPROD   WA_ITNS-Q2_QBCPROD   P_NFE.
    PERFORM CTNAFV USING COFINSVALIQPROD WA_ITNS-Q2_VALIQPROD P_NFE.
    PERFORM CTNAFV USING COFINSVCOFINS   WA_ITNS-Q2_VCOFINS   P_NFE.
    PERFORM CTNFE  USING COFINS P_NFE.
  ELSEIF WA_ITNS-Q3_CST IS NOT INITIAL.
    PERFORM CTNAB  USING COFINS P_NFE.
    PERFORM CTNAFV USING COFINSCST       WA_ITNS-Q3_CST       P_NFE.
    PERFORM CTNFE  USING COFINS P_NFE.
  ELSEIF WA_ITNS-Q4_CST IS NOT INITIAL.
    PERFORM CTNAB  USING COFINS P_NFE.
    PERFORM CTNAFV USING COFINSCST       WA_ITNS-Q4_CST       P_NFE.
    PERFORM CTNAFV USING COFINSVBC       WA_ITNS-Q4_VBC       P_NFE.
    PERFORM CTNAFV USING COFINSPCOFINS   WA_ITNS-Q4_PCOFINS   P_NFE.
    PERFORM CTNAFV USING COFINSQBCPROD   WA_ITNS-Q4_QBCPROD   P_NFE.
    PERFORM CTNAFV USING COFINSVALIQPROD WA_ITNS-Q4_VALIQPROD P_NFE.
    PERFORM CTNAFV USING COFINSVCOFINS   WA_ITNS-Q4_VCOFINS   P_NFE.
    PERFORM CTNFE  USING COFINS P_NFE.
  ELSEIF WA_ITNS-Q5_VBC IS NOT INITIAL.
    PERFORM CTNAB  USING COFINSST P_NFE.
    PERFORM CTNAFV USING COFINSVBC       WA_ITNS-Q5_VBC       P_NFE.
    PERFORM CTNAFV USING COFINSPCOFINS   WA_ITNS-Q5_PCOFINS   P_NFE.
    PERFORM CTNAFV USING COFINSQBCPROD   WA_ITNS-Q5_QBCPROD   P_NFE.
    PERFORM CTNAFV USING COFINSVALIQPROD WA_ITNS-Q5_VALIQPROD P_NFE.
    PERFORM CTNAFV USING COFINSVCOFINS   WA_ITNS-Q5_VCOFINS   P_NFE.
    PERFORM CTNFE  USING COFINSST P_NFE.
  ENDIF.

ENDFORM.                    " IMPOSTOCOFINS

*&---------------------------------------------------------------------*
*&      Form  IMPOSTOISSQN
*&---------------------------------------------------------------------*
*       Impostos Sobre Serviços de Qualquer Natureza
*----------------------------------------------------------------------*
FORM IMPOSTOISSQN  USING    WA_ITNS  TYPE J1B_NF_XML_ITEM  P_NFE.

  IF WA_ITNS-X_VBC IS NOT INITIAL.
    PERFORM CTNAB  USING ISSQN P_NFE.
    PERFORM CTNAFV USING ISSQNVBC       WA_ITNS-X_VBC       P_NFE.
    PERFORM CTNAFV USING ISSQNVALIQ     WA_ITNS-X_VALIQ     P_NFE.
    PERFORM CTNAFV USING ISSQNVISSQN    WA_ITNS-X_VISSQN    P_NFE.
    PERFORM CTNAFV USING ISSQNCMUNFG    WA_ITNS-X_CMUNFG    P_NFE.
    PERFORM CTNAFV USING ISSQNCLISTSERV WA_ITNS-X_CLISTSERV P_NFE.
    PERFORM CTNFE  USING ISSQN P_NFE.
  ENDIF.

ENDFORM.                    " IMPOSTOISSQN

*&---------------------------------------------------------------------*
*&      Form  CTEABREXML
*&---------------------------------------------------------------------*
FORM CTEABREXML  USING  P_CTE.
  PERFORM CTNAB USING CTEPADRAO P_CTE.
  PERFORM CTNAB USING INTGCTE   P_CTE.
ENDFORM.                    " CTEABREXML

*&---------------------------------------------------------------------*
*&      Form  CTEFECHAXML
*&---------------------------------------------------------------------*
FORM CTEFECHAXML  USING    P_CTE.
  PERFORM CTNFE USING INTGCTE P_CTE.
ENDFORM.                    " CTEFECHAXML

*&---------------------------------------------------------------------*
*&      Form  CTEIDE
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item B – Identificação do Conhecimento de Transporte Eletrônico
* B01 - ide
*----------------------------------------------------------------------*

FORM CTEIDE USING  P_CTE.

* Tipo
  TYPES: Y_ZLEST0030 TYPE ZLEST0030.

* Estrutura
  DATA: ST_ZLEST0030 TYPE ZLEST0030.

* Campos
  DATA: VL_DTROFORA  TYPE ZLEST0030-DSTCAT,
        VL_INDUSTRY  TYPE J_1BBRANCH-INDUSTRY,
        VL_REGIO_PC  TYPE LFA1-REGIO,
        VL_REGIO_LR  TYPE LFA1-REGIO,
        VL_KUNNR_AG  TYPE J_1BNFNAD-PARID,
        VL_KUNNR_RG  TYPE J_1BNFNAD-PARID,
        VL_CFOTXT    TYPE J_1BAGT-CFOTXT,
        VL_TXJCD(15) TYPE C.

  PERFORM CTNAB USING CTEIDE     P_CTE.

* Obter os parceiros PC e LR e suas respectivas regiões.
* Classificar a tabela pelo número de documento e tipo de parceiro.
  SORT TI_PARTNER_P BY DOCNUM PARVW.

  LOOP AT TI_HEADER_P INTO WK_HEADER_P.

* Parceiro PC
    CLEAR: ST_PARTNER_P,
           VL_REGIO_PC.
    READ TABLE TI_PARTNER_P INTO ST_PARTNER_P WITH KEY DOCNUM = WK_HEADER_P-DOCNUM
                                                       PARVW  = C_PC
                                                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE REGIO INTO VL_REGIO_PC
                          FROM LFA1
                         WHERE LIFNR = ST_PARTNER_P-PARID.
    ENDIF.

* Parceiro LR
    CLEAR: ST_PARTNER_P,
           VL_REGIO_LR.
    READ TABLE TI_PARTNER_P INTO ST_PARTNER_P WITH KEY DOCNUM = WK_HEADER_P-DOCNUM
                                                       PARVW  = C_LR
                                                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE REGIO INTO VL_REGIO_LR
                          FROM LFA1
                         WHERE LIFNR = ST_PARTNER_P-PARID.
    ENDIF.

    IF VL_REGIO_PC = VL_REGIO_LR.
      VL_DTROFORA = C_0.
    ELSE.
      VL_DTROFORA = C_1.
    ENDIF.

* Obter o cfop
*    SELECT SINGLE * INTO st_zlest0030
*                    FROM zlest0030
*                   WHERE direct   = wk_header_p-direct
*                     AND dstcat   = vl_dtrofora
*                     AND industry = st_j_1bbranch-industry.

*    PERFORM ctnav USING ctecfop    st_zlest0030-cfop(4) p_cte.
*    PERFORM ctnav USING ctenatop   st_zlest0030-cfotxt  p_cte.

  ENDLOOP.

* Obter o cfop e sua descrição diretamente da nota de serviço.
  PERFORM CTNAV USING CTECFOP    ST_VBRP-J_1BCFOP(4) P_CTE.
* Obger a descrição do cfop.
  CLEAR VL_CFOTXT.
  SELECT SINGLE CFOTXT INTO VL_CFOTXT
                       FROM J_1BAGT
                      WHERE SPRAS = SY-LANGU
                        AND CFOP  = ST_VBRP-J_1BCFOP.
  PERFORM CTNAV USING CTENATOP   VL_CFOTXT  P_CTE.


  IF ST_VTTK-SHTYP = C_Z020.         "Transferência
    VG_CPGTO = C_1.
  ENDIF.
  IF VG_VERSAO_CTE < 3. "CS2017002143 CT-e 3.0 Ini
    PERFORM CTNAV USING CTEFORPAG  VG_CPGTO         P_CTE.
  ENDIF. "CS2017002143 CT-e 3.0 - Fim
  PERFORM CTNAV USING CTESERIE   WK_HEADER-SERIES   P_CTE.
  PERFORM CTNAV USING CTENCT     WK_HEADER-NFENUM   P_CTE.

  CONCATENATE WK_HEADER-CREDAT(4)       "ano
              '-'
              WK_HEADER-CREDAT+4(2)     "mês
              '-'
              WK_HEADER-CREDAT+6(2)     "dia
              '*'
              WK_HEADER-CRETIM(2)       "hora
              ':'
              WK_HEADER-CRETIM+2(2)     "minuto
              ':'
              WK_HEADER-CRETIM+4(2)     "segundo
              INTO VG_DT_HORA.
  REPLACE '*' WITH SPACE INTO VG_DT_HORA.

  "Ini. CS2017002043 04.10.2017
  IF ( WK_HEADER-DOCNUM IS NOT INITIAL ).
    DATA: V_TIME_BR TYPE ERZET.
    CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
      EXPORTING
        I_BUKRS  = WK_HEADER-BUKRS
        I_BRANCH = WK_HEADER-BRANCH
      IMPORTING
        E_TIME   = V_TIME_BR.
    IF V_TIME_BR IS NOT INITIAL.
      CONCATENATE WK_HEADER-CREDAT(4)       "ano
                  '-'
                  WK_HEADER-CREDAT+4(2)     "mês
                  '-'
                  WK_HEADER-CREDAT+6(2)     "dia
                  '*'
                  V_TIME_BR(2)       "hora
                  ':'
                  V_TIME_BR+2(2)     "minuto
                  ':'
                  V_TIME_BR+4(2)     "segundo
                  INTO VG_DT_HORA.
      REPLACE '*' WITH SPACE INTO VG_DT_HORA.
    ENDIF.
  ENDIF.
  "Fim. CS2017002043 04.10.2017

  PERFORM CTNAV USING CTEDHEMI   VG_DT_HORA     P_CTE.
  PERFORM CTNAV USING CTETPCTE   C_0            P_CTE.
  PERFORM CTNAF USING CTEREFCTE  ''             P_CTE.
  PERFORM CTNAV USING CTEMODAL   ST_VTTK-VSART  P_CTE.
  "Tipo de Serviço sempre Normal ( 0 – Normal; 1 – Subcontratação; 2 – Redespacho; 3 – Redespacho Intermediário. )
  PERFORM CTNAV USING CTETPSERV  C_0            P_CTE.

* Todas as notas devem ter os mesmos dados de início e fim da prestação de serviço
* e tomador, logo obter os dados do primeiro registro da tabela.
  READ TABLE TI_HEADER_P INTO WK_HEADER_P INDEX 1.
*  LOOP AT ti_header_p INTO wk_header_p.
  CLEAR: ST_PARTNER_P.

* Início da Prestação do serviço
  IF ST_VTTK-ABFER = C_1 OR
     ST_VTTK-ABFER = C_3.
* Parceiro PC
    READ TABLE TI_PARTNER_P INTO ST_PARTNER_P WITH KEY DOCNUM = WK_HEADER_P-DOCNUM
                                                       PARVW  = C_PC
                                                       BINARY SEARCH.
  ELSEIF ST_VTTK-ABFER = C_2 OR
         ST_VTTK-ABFER = C_4.
* Parceiro LF
    READ TABLE TI_PARTNER_P INTO ST_PARTNER_P WITH KEY DOCNUM = WK_HEADER_P-DOCNUM
                                                       PARVW  = C_LF
                                                       BINARY SEARCH.
  ENDIF.

  IF NOT XML_CTE_1_04 IS INITIAL.
    PERFORM CIDADE_EMISSAO_CTE USING WK_HEADER P_CTE.
  ENDIF.

* Obter o domicílio fiscal que está no cadatro do fornecedor (lfa1) pois o que está
* na tabela de parceiros da nota está errado.
  SELECT SINGLE TXJCD INTO VL_TXJCD
                      FROM LFA1
                     WHERE LIFNR = ST_PARTNER_P-PARID.

  IF VL_TXJCD IS NOT INITIAL.
    PERFORM CTNAV USING CTECMUNINI VL_TXJCD+3(7)  P_CTE.
  ELSE.
    REFRESH TI_J_1BTREG_CITY.
    SELECT TAXJURCODE INTO TABLE TI_J_1BTREG_CITY
                      FROM J_1BTREG_CITY
                     WHERE COUNTRY     = C_BR
                       AND REGION      = ST_PARTNER_P-REGIO
                       AND PSTCD_FROM <= ST_PARTNER_P-PSTLZ
                       AND PSTCD_TO   >= ST_PARTNER_P-PSTLZ.
* Obter sempre o cep mais alto.
    SORT TI_J_1BTREG_CITY BY TAXJURCODE DESCENDING.
    CLEAR ST_J_1BTREG_CITY.
    READ TABLE TI_J_1BTREG_CITY INTO ST_J_1BTREG_CITY INDEX 1.
    PERFORM CTNAV USING CTECMUNINI ST_J_1BTREG_CITY-TAXJURCODE+3(7)  P_CTE.
  ENDIF.

* Fim da Prestação do serviço
* Frete de saída
  CLEAR VL_TXJCD.
  IF ST_VTTK-ABFER = C_1 OR
     ST_VTTK-ABFER = C_3.
* Parceiro LR
    CLEAR ST_PARTNER_P.
    READ TABLE TI_PARTNER_P INTO ST_PARTNER_P WITH KEY DOCNUM = WK_HEADER_P-DOCNUM
                                                       PARVW  = C_LR
                                                       BINARY SEARCH.
* Obter o domicílio fiscal que está no cadatro do cliente (kna1) pois o que está
* na tabela de parceiros da nota está errado.
    CLEAR VL_TXJCD.
    SELECT SINGLE TXJCD INTO VL_TXJCD
                        FROM KNA1
                       WHERE KUNNR = ST_PARTNER_P-PARID.
    IF VL_TXJCD IS INITIAL.
      REFRESH TI_J_1BTREG_CITY.
      SELECT TAXJURCODE INTO TABLE TI_J_1BTREG_CITY
                        FROM J_1BTREG_CITY
                       WHERE COUNTRY     = C_BR
                         AND REGION      = ST_PARTNER_P-REGIO
                         AND PSTCD_FROM <= ST_PARTNER_P-PSTLZ
                         AND PSTCD_TO   >= ST_PARTNER_P-PSTLZ.
    ENDIF.
  ELSEIF ST_VTTK-ABFER = C_2 OR
         ST_VTTK-ABFER = C_4.
    READ TABLE TI_T001W INTO ST_T001W INDEX 1.
    IF ST_T001W-TXJCD IS NOT INITIAL.
      VL_TXJCD = ST_T001W-TXJCD.
    ELSE.
      REFRESH TI_J_1BTREG_CITY.
      SELECT TAXJURCODE INTO TABLE TI_J_1BTREG_CITY
                        FROM J_1BTREG_CITY
                       WHERE COUNTRY     = C_BR
                         AND REGION      = ST_T001W-REGIO
                         AND PSTCD_FROM <= ST_T001W-PSTLZ
                         AND PSTCD_TO   >= ST_T001W-PSTLZ.
    ENDIF.
  ENDIF.
  IF VL_TXJCD IS NOT INITIAL.
    PERFORM CTNAV USING CTECMUNFIM VL_TXJCD+3(7) P_CTE.
  ELSE.
* Obter sempre o cep mais alto.
    SORT TI_J_1BTREG_CITY BY TAXJURCODE DESCENDING.
    CLEAR ST_J_1BTREG_CITY.
    READ TABLE TI_J_1BTREG_CITY INTO ST_J_1BTREG_CITY INDEX 1.
    PERFORM CTNAV USING CTECMUNFIM ST_J_1BTREG_CITY-TAXJURCODE+3(7) P_CTE.
  ENDIF.

  "Não retira em aeroporto.
  PERFORM CTNAF USING CTERETIRA     C_1  P_CTE.
  PERFORM CTNAF USING CTEXDETRETIRA ''   P_CTE.

* Parceiro AG
  CLEAR: ST_PARTNER_P,
         VL_KUNNR_AG.
  READ TABLE TI_PARTNER_P INTO ST_PARTNER_P WITH KEY DOCNUM = WK_HEADER_P-DOCNUM
                                                     PARVW  = C_AG
                                                     BINARY SEARCH.
  IF SY-SUBRC = 0.
    VL_KUNNR_AG = ST_PARTNER_P-PARID.
  ENDIF.
* Parceiro RG
  CLEAR: ST_PARTNER_P,
         VL_KUNNR_RG.
  READ TABLE TI_PARTNER_P INTO ST_PARTNER_P WITH KEY DOCNUM = WK_HEADER_P-DOCNUM
                                                     PARVW  = C_RG
                                                     BINARY SEARCH.
  IF SY-SUBRC = 0.
    VL_KUNNR_RG = ST_PARTNER_P-PARID.
  ENDIF.

  IF ST_VTTK-SHTYP = C_Z020.         "Transferência

    IF VG_VERSAO_CTE >= 3. "CS2017002143 CT-e 3.0 Ini
      PERFORM CTNAF USING INDIETOMA  '1'  P_CTE.
    ENDIF. "CS2017002143 CT-e 3.0 - Fim

    PERFORM CTNAV USING CTETOMA    C_3  P_CTE.
  ELSEIF ST_VTTK-SHTYP = C_Z018.     "Venda Triangular

    IF VG_VERSAO_CTE >= 3. "CS2017002143 CT-e 3.0 Ini
      PERFORM CTNAF USING INDIETOMA  '1'  P_CTE.
    ENDIF. "CS2017002143 CT-e 3.0 - Fim

    PERFORM CTNAV USING CTETOMA    C_0  P_CTE.
  ELSEIF VL_KUNNR_AG = VL_KUNNR_RG.

    IF VG_VERSAO_CTE >= 3. "CS2017002143 CT-e 3.0 Ini
      PERFORM CTNAF USING INDIETOMA  '1'  P_CTE.
    ENDIF. "CS2017002143 CT-e 3.0 - Fim

    PERFORM CTNAV USING CTETOMA    C_0  P_CTE.
  ELSE.

    IF VG_VERSAO_CTE >= 3. "CS2017002143 CT-e 3.0 Ini
      PERFORM CTNAF USING INDIETOMA  '1'  P_CTE. "CS2017002143 CT-e 3.0 - Ini
    ENDIF. "CS2017002143 CT-e 3.0 - Fim

    PERFORM CTNAV USING CTETOMA    C_4  P_CTE.
    "Toma Outros
    PERFORM CTETOMADOR    USING P_CTE.
  ENDIF.
*  ENDLOOP.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEIDE     P_CTE.

ENDFORM.                    " CTEIDE

*&---------------------------------------------------------------------*
*&      Form  CTECOMPL
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item C – Dados complementares do CT-e, para fins Operacionais ou
* Comerciais
* C01 - compl
*----------------------------------------------------------------------*
FORM CTECOMPL  USING    P_CTE TB TYPE J1B_NF_XML_HEADER.

  TYPES: BEGIN OF Y_TRANSBORDO,
           NAME1  TYPE ADRC-NAME1,
           CITY1  TYPE ADRC-CITY1,
           REGION TYPE ADRC-REGION,
         END OF Y_TRANSBORDO.

  DATA: VL_TEXTO(14)  TYPE C,
        VL_CNPJ(18)   TYPE C,
        VL_CNPJ_PROP  TYPE STRING,
        VL_LINHA(500) TYPE C,
        VL_UF         TYPE ZLEST0002-CD_UF,
        VL_NOME       TYPE LFA1-NAME1,
        TL_TRANSBORDO TYPE TABLE OF Y_TRANSBORDO,
        ST_TRANSBORDO TYPE Y_TRANSBORDO,
        WA_PARID      TYPE Y_VTPA,
        CIDADE        TYPE STRING,
        VL_TEM        TYPE C LENGTH 1.

  DATA: IT_VFKP       TYPE TABLE OF VFKP INITIAL SIZE 0 WITH HEADER LINE,
        IT_KONV       TYPE TABLE OF KONV INITIAL SIZE 0 WITH HEADER LINE,
        WA_KONV       TYPE KONV,
        VG_KINAK      TYPE KINAK,
        VG_KWERT      TYPE KWERT,
        VALOR_PEDAGIO TYPE C LENGTH 20,
        TEXTO_PEDAGIO TYPE STRING.

  VG_KWERT = 0.

  PERFORM CTNAB USING CTECOMPL     P_CTE.
  PERFORM CTNAB USING CTEXOBS      P_CTE.
* MC
  IF NOT TB-INFADFISCO_V2 IS INITIAL.
    CONCATENATE  P_CTE TB-INFADFISCO_V2 INTO P_CTE.
    VL_TEM = C_X.
  ENDIF.

  IF NOT TB-INFCOMP IS INITIAL.
    IF TB-INFADFISCO_V2 IS INITIAL.
      CONCATENATE  P_CTE TB-INFCOMP INTO P_CTE.
    ELSE.
      CONCATENATE  P_CTE TB-INFCOMP INTO P_CTE SEPARATED BY SPACE.
    ENDIF.
    VL_TEM = C_X.
  ENDIF.

* Observações do cabeçalho e do item da ordem ZTRO.
  IF TI_LINE[] IS NOT INITIAL.
    LOOP AT TI_LINE INTO ST_LINE.
      IF VL_TEM IS INITIAL.
        PERFORM CTNAOS USING ST_LINE-TDLINE   P_CTE.
        VL_TEM = C_X.
      ELSE.
        PERFORM CTNAO  USING ST_LINE-TDLINE   P_CTE.
      ENDIF.
    ENDLOOP.
  ENDIF.
* Demais textos

* Documento de Transporte
  CONCATENATE 'Número do Transporte:' ST_VTTK-TKNUM INTO VL_LINHA.
  PERFORM CTNAO USING VL_LINHA         P_CTE.
  CLEAR VL_LINHA.

* Documento de Faturamento
  IF ST_VBRP_P-VBELN IS NOT INITIAL.
    CONCATENATE 'Número do Faturamento:' ST_VBRP_P-VBELN INTO VL_LINHA.
    PERFORM CTNAO USING VL_LINHA         P_CTE.
    CLEAR VL_LINHA.
  ENDIF.

  LOOP AT TI_VTPA INTO ST_VTPA.
    IF ST_VTPA-PARVW = C_PV.
* Nome do subcontratado
      PERFORM CTNAO USING C_SUBCONTRATADO   P_CTE.
      PERFORM CTNAO USING ST_VTPA-NAME1     P_CTE.
* CNPJ do subcontratado
      IF ST_VTPA-STCD1 IS NOT INITIAL.
        PERFORM CTNAO  USING C_CNPJ           P_CTE.
        PERFORM CTNAOS USING C_HIFEN          P_CTE.
        PERFORM CTNAOS USING ST_VTPA-STCD1    P_CTE.
      ELSEIF ST_VTPA-STCD2 IS NOT INITIAL.
* CPF do subcontratado
        PERFORM CTNAO  USING C_CPF            P_CTE.
        PERFORM CTNAOS USING C_HIFEN          P_CTE.
        PERFORM CTNAOS USING ST_VTPA-STCD2    P_CTE.
      ENDIF.
* IE do subcontratado
      IF ST_VTPA-STCD3 IS NOT INITIAL.
        PERFORM LIMPA_NUMERO USING ST_VTPA-STCD3.
        PERFORM CTNAO  USING C_IE      P_CTE.
        PERFORM CTNAOS USING C_HIFEN   P_CTE.
        PERFORM CTNAOS USING VG_LIMPO  P_CTE.
      ENDIF.
* Placa do cavalo
      IF ST_VTTK-TEXT1 IS NOT INITIAL.
        PERFORM Z_LER_PLACA USING ST_VTTK-TEXT1
                            CHANGING VL_UF
                                     VL_NOME
                                     VL_CNPJ_PROP.
        PERFORM CTNAO  USING C_PLACA_CAVALO   P_CTE.
        PERFORM CTNAOS USING C_HIFEN          P_CTE.
        PERFORM CTNAOS USING ST_VTTK-TEXT1    P_CTE.
        IF VL_UF IS NOT INITIAL.
          PERFORM CTNAOS USING C_BARRA          P_CTE.
          PERFORM CTNAOS USING VL_UF            P_CTE.
          PERFORM CTNAO  USING VL_NOME          P_CTE.
          PERFORM CTNAO  USING VL_CNPJ_PROP     P_CTE.
        ENDIF.
      ENDIF.
* Placa da carreta 1
      IF ST_VTTK-TEXT2 IS NOT INITIAL.
        CLEAR VL_UF.
        PERFORM Z_LER_PLACA USING ST_VTTK-TEXT2
                            CHANGING VL_UF
                                     VL_NOME
                                     VL_CNPJ_PROP.
        CONCATENATE C_PLACA_CARRETA C_1 INTO VL_TEXTO SEPARATED BY SPACE.
        PERFORM CTNAO  USING VL_TEXTO         P_CTE.
        PERFORM CTNAOS USING C_HIFEN          P_CTE.
        PERFORM CTNAOS USING ST_VTTK-TEXT2    P_CTE.
        IF VL_UF IS NOT INITIAL.
          PERFORM CTNAOS USING C_BARRA          P_CTE.
          PERFORM CTNAOS USING VL_UF            P_CTE.
          PERFORM CTNAO  USING VL_NOME          P_CTE.
          PERFORM CTNAO  USING VL_CNPJ_PROP     P_CTE.
        ENDIF.
      ENDIF.
* Placa da carreta 2
      IF ST_VTTK-TEXT3 IS NOT INITIAL.
        CLEAR VL_UF.
        PERFORM Z_LER_PLACA USING ST_VTTK-TEXT3
                            CHANGING VL_UF
                                     VL_NOME
                                     VL_CNPJ_PROP.
        CONCATENATE C_PLACA_CARRETA C_2 INTO VL_TEXTO SEPARATED BY SPACE.
        PERFORM CTNAO  USING VL_TEXTO         P_CTE.
        PERFORM CTNAOS USING C_HIFEN          P_CTE.
        PERFORM CTNAOS USING ST_VTTK-TEXT3    P_CTE.
        IF VL_UF IS NOT INITIAL.
          PERFORM CTNAOS USING C_BARRA          P_CTE.
          PERFORM CTNAOS USING VL_UF            P_CTE.
          PERFORM CTNAO  USING VL_NOME          P_CTE.
          PERFORM CTNAO  USING VL_CNPJ_PROP     P_CTE.
        ENDIF.
      ENDIF.
* Placa da carreta 3
      IF ST_VTTK-TEXT4 IS NOT INITIAL.
        CLEAR VL_UF.
        PERFORM Z_LER_PLACA USING ST_VTTK-TEXT4
                            CHANGING VL_UF
                                     VL_NOME
                                     VL_CNPJ_PROP.
        CONCATENATE C_PLACA_CARRETA C_3 INTO VL_TEXTO SEPARATED BY SPACE.
        PERFORM CTNAO  USING VL_TEXTO         P_CTE.
        PERFORM CTNAOS USING C_HIFEN          P_CTE.
        PERFORM CTNAOS USING ST_VTTK-TEXT3    P_CTE.
        IF VL_UF IS NOT INITIAL.
          PERFORM CTNAOS USING C_BARRA          P_CTE.
          PERFORM CTNAOS USING VL_UF            P_CTE.
          PERFORM CTNAO  USING VL_NOME          P_CTE.
          PERFORM CTNAO  USING VL_CNPJ_PROP     P_CTE.
        ENDIF.
      ENDIF.
    ELSEIF ST_VTPA-PARVW = 'LR'.
      "Transbordo somente quando o tipo de documento de transporte
      "estiver entre Z001 e Z004.
      CHECK ST_VTTK-SHTYP >= C_Z001 AND
            ST_VTTK-SHTYP <= C_Z004.

      "Obter os dados do local de destino
      REFRESH TL_TRANSBORDO.
      SELECT A~NAME1 A~CITY1 A~REGION
             INTO TABLE TL_TRANSBORDO
             FROM VTTS AS V
             INNER JOIN TVKN AS T ON T~KNOTE = V~KNOTZ
             INNER JOIN ADRC AS A ON A~ADDRNUMBER = T~ADRNR
             WHERE V~TKNUM = ST_VTTK-TKNUM.
      READ TABLE TL_TRANSBORDO INTO ST_TRANSBORDO INDEX 1.
      WRITE ST_VTPA-STCD1 USING EDIT MASK '__.___.___/____-__' TO VL_CNPJ.

      CIDADE = ST_TRANSBORDO-CITY1.

      CONCATENATE ST_TRANSBORDO-NAME1
                  C_HIFEN
                  ST_TRANSBORDO-CITY1
                  INTO VL_LINHA SEPARATED BY SPACE.

      CONCATENATE VL_LINHA
                  C_BARRA
                  ST_TRANSBORDO-REGION
                  INTO VL_LINHA.

      CONCATENATE VL_LINHA
                  C_CNPJ
                  INTO VL_LINHA SEPARATED BY SPACE.

      CONCATENATE VL_LINHA
                  C_HIFEN
                  VL_CNPJ
                  INTO VL_LINHA.

      CONCATENATE VL_LINHA
                  C_IE
                  INTO VL_LINHA SEPARATED BY SPACE.

      CONCATENATE VL_LINHA
                  C_HIFEN
                  ST_VTPA-STCD3
                  INTO VL_LINHA.

      PERFORM CTNAO USING C_TRANSBORDO     P_CTE.
      PERFORM CTNAO USING VL_LINHA         P_CTE.
    ENDIF.

  ENDLOOP.

  SELECT *
    FROM VFKP
    INTO CORRESPONDING FIELDS OF TABLE IT_VFKP
   WHERE REFTY EQ C_8
     AND REBEL EQ ST_VTTK-TKNUM.

  IF NOT IT_VFKP[] IS INITIAL.

* ---> S4 Migration - 07/07/2023 - JP
*
*    SELECT *
*      FROM KONV
*      INTO CORRESPONDING FIELDS OF TABLE IT_KONV
*      FOR ALL ENTRIES IN IT_VFKP
*     WHERE KNUMV EQ IT_VFKP-KNUMV
*       AND KSCHL EQ C_ZPED
*       AND KINAK EQ VG_KINAK
*       AND KWERT GT 0.


    SELECT *
      FROM V_KONV
      INTO table @data(IT_KONV_AUX)
      FOR ALL ENTRIES IN @IT_VFKP
     WHERE KNUMV EQ @IT_VFKP-KNUMV
       AND KSCHL EQ @C_ZSEG
       AND KINAK EQ @VG_KINAK
       AND KWERT GT 0.

    move-corresponding IT_KONV_AUX[] to IT_KONV[].

    READ TABLE IT_konv TRANSPORTING NO FIELDS INDEX 1.
* <--- S4 Migration - 07/07/2023 - JP

    IF SY-SUBRC EQ 0.
      LOOP AT IT_KONV INTO WA_KONV.
        IF WA_KONV-KWERT GT 0.
          VG_KWERT = VG_KWERT + WA_KONV-KWERT.
        ENDIF.
      ENDLOOP.
      IF VG_KWERT GT 0.
        WRITE VG_KWERT TO VALOR_PEDAGIO.
        SHIFT VALOR_PEDAGIO LEFT DELETING LEADING SPACE.
        CONCATENATE 'Valor do pedágio:' VALOR_PEDAGIO INTO TEXTO_PEDAGIO SEPARATED BY SPACE.
        PERFORM CTNAO USING TEXTO_PEDAGIO P_CTE.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM CTESEG  USING  P_CTE 'X'.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEXOBS      P_CTE.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTECOMPL     P_CTE.

ENDFORM.                    " CTECOMPL
*&---------------------------------------------------------------------*
*&      Form  CTEOBSCONT
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item C – Dados complementares do CT-e, para fins Operacionais ou
* Comerciais
* C22 - ObsCont
*----------------------------------------------------------------------*
FORM CTEOBSCONT  USING    P_CTE.
  DATA CIDADE TYPE STRING.
* Os textos que foram montados para serem gravados na tag "ObsCont" devem ser gravados
* na tag "compl".
  CHECK 1 = 2.

  TYPES: BEGIN OF Y_TRANSBORDO,
           NAME1  TYPE ADRC-NAME1,
           CITY1  TYPE ADRC-CITY1,
           REGION TYPE ADRC-REGION,
         END OF Y_TRANSBORDO.

  DATA: VL_TEXTO(14)  TYPE C,
        VL_IE(12)     TYPE C,
        VL_CNPJ(18)   TYPE C,
        VL_LINHA      TYPE STRING,
        TL_TRANSBORDO TYPE TABLE OF Y_TRANSBORDO,
        ST_TRANSBORDO TYPE Y_TRANSBORDO.

  PERFORM CTNAB USING CTEOBSCONT    P_CTE.

  LOOP AT TI_VTPA INTO ST_VTPA.
    IF ST_VTPA-PARVW = C_PV.
* Nome do subcontratado
      PERFORM CTNAV USING CTEXCAMPO     C_SUBCONTRATADO  P_CTE.
      PERFORM CTNAV USING CTEXTEXTO     ST_VTPA-NAME1    P_CTE.
* CNPJ do subcontratado
      PERFORM CTNAV USING CTEXCAMPO     C_CNPJ           P_CTE.
      PERFORM CTNAV USING CTEXTEXTO     ST_VTPA-STCD1    P_CTE.
* IE do subcontratado
      PERFORM LIMPA_NUMERO USING ST_VTPA-STCD3.
      PERFORM CTNAV USING CTEXCAMPO     C_IE             P_CTE.
      PERFORM CTNAV USING CTEXTEXTO     VG_LIMPO         P_CTE.
* Placa do cavalo
      PERFORM CTNAV USING CTEXCAMPO     C_PLACA_CAVALO   P_CTE.
      PERFORM CTNAV USING CTEXTEXTO     ST_VTTK-TEXT1    P_CTE.
* Placa da carreta 1
      IF ST_VTTK-TEXT2 IS NOT INITIAL.
        CONCATENATE C_PLACA_CARRETA C_1 INTO VL_TEXTO SEPARATED BY SPACE.
        PERFORM CTNAV USING CTEXCAMPO   VL_TEXTO         P_CTE.
        PERFORM CTNAV USING CTEXTEXTO   ST_VTTK-TEXT2    P_CTE.
      ENDIF.
* Placa da carreta 2
      IF ST_VTTK-TEXT3 IS NOT INITIAL.
        CONCATENATE C_PLACA_CARRETA C_2 INTO VL_TEXTO SEPARATED BY SPACE.
        PERFORM CTNAV USING CTEXCAMPO   VL_TEXTO         P_CTE.
        PERFORM CTNAV USING CTEXTEXTO   ST_VTTK-TEXT3    P_CTE.
      ENDIF.
* Placa da carreta 3
      IF ST_VTTK-TEXT4 IS NOT INITIAL.
        CONCATENATE C_PLACA_CARRETA C_3 INTO VL_TEXTO SEPARATED BY SPACE.
        PERFORM CTNAV USING CTEXCAMPO   VL_TEXTO         P_CTE.
        PERFORM CTNAV USING CTEXTEXTO   ST_VTTK-TEXT4    P_CTE.
      ENDIF.
    ELSEIF ST_VTPA-PARVW = C_LR.
* Transbordo somente quando o tipo de documento de transporte
* estiver entre Z001 e Z004.
      CHECK ST_VTTK-SHTYP >= C_Z001 AND
            ST_VTTK-SHTYP <= C_Z004.

* Obter os dados do local de destino
      REFRESH TL_TRANSBORDO.
      SELECT A~NAME1 A~CITY1 A~REGION
             INTO TABLE TL_TRANSBORDO
             FROM VTTS AS V
             INNER JOIN TVKN AS T ON T~KNOTE = V~KNOTZ
             INNER JOIN ADRC AS A ON A~ADDRNUMBER = T~ADRNR
             WHERE V~TKNUM = ST_VTTK-TKNUM.
      WRITE ST_VTPA-STCD1 USING EDIT MASK '__.___.___/____-__' TO VL_CNPJ.
      WRITE ST_VTPA-STCD3 USING EDIT MASK '___________-_' TO VL_IE.

      CIDADE = ST_TRANSBORDO-CITY1.

      CONCATENATE ST_TRANSBORDO-NAME1
                  C_HIFEN
                  CIDADE
                  INTO VL_LINHA SEPARATED BY SPACE.

      CONCATENATE VL_LINHA
                  C_BARRA
                  ST_TRANSBORDO-REGION
                  INTO VL_LINHA.

      CONCATENATE VL_LINHA
                  C_CNPJ
                  INTO VL_LINHA SEPARATED BY SPACE.

      CONCATENATE VL_LINHA
                  C_HIFEN
                  VL_CNPJ
                  INTO VL_LINHA.

      CONCATENATE VL_LINHA
                  C_IE
                  INTO VL_LINHA SEPARATED BY SPACE.

      CONCATENATE VL_LINHA
                  C_HIFEN
                  VL_IE
                  INTO VL_LINHA.

      PERFORM CTNAV USING CTEXCAMPO   C_TRANSBORDO     P_CTE.
      PERFORM CTNAV USING CTEXTEXTO   VL_LINHA         P_CTE.
    ENDIF.

  ENDLOOP.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEOBSCONT    P_CTE.

ENDFORM.                    " CTEOBSCONT

*&---------------------------------------------------------------------*
*&      Form  CTETOMADOR
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Considerar os parceiros da nota fiscal de serviço.
* Item B – Identificação do Conhecimento de Transporte Eletrônico
* B17 - tomaOutros
*----------------------------------------------------------------------*
FORM CTETOMADOR  USING    P_CTE.

  IF ST_VTTK-SHTYP = C_Z020.         "Transferência
    READ TABLE TI_LIN INTO ST_LIN INDEX 1.
* Obter os dados do parceiro WE
    CLEAR: ST_PARTNER_P.
    READ TABLE TI_PARTNER_P INTO ST_PARTNER_P WITH KEY DOCNUM = ST_LIN-DOCNUM
                                                       PARVW  = C_WE
                                                       BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.
  ELSE.
* Parceiro AG
    CLEAR: ST_PARTNER,
           VG_PARID_AG.
    READ TABLE TI_PARTNER INTO ST_PARTNER WITH KEY DOCNUM = WK_HEADER-DOCNUM
                                                   PARVW  = C_AG
                                                   BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      VG_PARID_AG = ST_PARTNER_P-PARID.
    ENDIF.
* Parceiro RG
    CLEAR: ST_PARTNER,
           VG_PARID_RG.
    READ TABLE TI_PARTNER INTO ST_PARTNER WITH KEY DOCNUM = WK_HEADER-DOCNUM
                                                   PARVW  = C_RG
                                                   BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      VG_PARID_RG = ST_PARTNER_P-PARID.
    ENDIF.
* Quando os parceiros RG e AG são diferentes, há consignatário e este será o
* o parceiro RG.
    CHECK VG_PARID_RG <> VG_PARID_AG.
  ENDIF.

  PERFORM CTNAB USING CTETOMAOUTROS P_CTE.
  IF ST_PARTNER-LAND1 = C_BR.
    IF ST_PARTNER-CGC IS NOT INITIAL.
      PERFORM CTNAV USING CTETOMACNPJ ST_PARTNER-CGC   P_CTE.
    ELSE.
      PERFORM CTNAV USING CTETOMACPF  ST_PARTNER-CPF   P_CTE.
    ENDIF.
  ENDIF.
  PERFORM CTNAV USING CTETOMAIE     ST_PARTNER-STAINS  P_CTE.
  PERFORM CTNAV USING CTETOMAXNOME  ST_PARTNER-NAME1   P_CTE.
  PERFORM CTNAF USING CTETOMAXFAN   ''                 P_CTE.
  PERFORM CTNAF USING CTETOMAFONE   ''                 P_CTE.

  "Endereço Tomador
  PERFORM CTEENDERTOMA  USING P_CTE.

  PERFORM CTNFE USING CTETOMAOUTROS  P_CTE.

ENDFORM.                    " CTETOMADOR

*&---------------------------------------------------------------------*
*&      Form  CTEENDERTOMA
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item B – Identificação do Conhecimento de Transporte Eletrônico
* B24 - enderToma
*----------------------------------------------------------------------*
FORM CTEENDERTOMA  USING    P_CTE.

  DATA VL_TXJCD TYPE KNA1-TXJCD.

* Obter os dados de endereço caso algum esteja faltando na tabela de parceiros da nota
  IF ST_PARTNER-STRAS        IS INITIAL OR
     ST_PARTNER-STRAS+25(10) IS INITIAL OR
     ST_PARTNER-ORT02        IS INITIAL OR
     ST_PARTNER-PSTLZ        IS INITIAL.
    PERFORM Z_LER_CLIENTE USING ST_PARTNER-PARID.
  ENDIF.

  PERFORM CTNAB USING CTETOMAENDERTOMA  P_CTE.
* Obter os dados do parceiro consignatário - parceiro RG
  PERFORM CTNAV USING CTETOMAXLGR     ST_PARTNER-STRAS(25)      P_CTE.
  PERFORM CTNAV USING CTETOMAXNUM     ST_PARTNER-STRAS+25(10)   P_CTE.
  PERFORM CTNAF USING CTETOMAXCPL     ''                        P_CTE.
  PERFORM CTNAV USING CTETOMAXBAIRRO  ST_PARTNER-ORT02          P_CTE.

*  REFRESH ti_j_1btreg_city.
*  SELECT taxjurcode INTO TABLE ti_j_1btreg_city
*                    FROM j_1btreg_city
*                   WHERE country     = c_br
*                     AND region      = st_partner-regio
*                     AND pstcd_from <= st_partner-pstlz
*                     AND pstcd_to   >= st_partner-pstlz.
** Obter sempre o cep mais alto.
*  SORT ti_j_1btreg_city BY taxjurcode DESCENDING.
*  CLEAR st_j_1btreg_city.
*  READ TABLE ti_j_1btreg_city INTO st_j_1btreg_city INDEX 1.
*  PERFORM ctnav USING ctetomacmun     st_j_1btreg_city-taxjurcode+3(7) p_cte.

* Obter o domicílio fiscal que está no cadatro do cliente (kna1) pois o que está
* na tabela de parceiros da nota está errado.
  CLEAR VL_TXJCD.
  SELECT SINGLE TXJCD INTO VL_TXJCD
                      FROM KNA1
                     WHERE KUNNR = ST_PARTNER_P-PARID.

  PERFORM CTNAV USING CTETOMACMUN     VL_TXJCD+3(7)                    P_CTE.
  PERFORM Z_CONVERTER_CEP USING ST_PARTNER-PSTLZ.
  PERFORM CTNAV USING CTETOMACEP      VG_PSTLZ                         P_CTE.
  PERFORM CTNAF USING CTETOMAPAIS     ''                               P_CTE.
* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTETOMAENDERTOMA P_CTE.
ENDFORM.                    " CTEENDERTOMA

*&---------------------------------------------------------------------*
*&      Form  CTEEMIT
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item D – Emitente do CT-e
* D01 - emit
*&---------------------------------------------------------------------*
FORM CTEEMIT  USING   P_CTE.

  PERFORM CTNAB USING CTEEMIT     P_CTE.
  PERFORM CTNAV USING CTEEMITCNPJ ST_TDLNR-STCD1 P_CTE.
  PERFORM LIMPA_NUMERO USING ST_TDLNR-STCD3.
  PERFORM CTNAV USING CTEEMITIE VG_LIMPO P_CTE.
* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEEMIT     P_CTE.

ENDFORM.                    " CTEEMIT

*&---------------------------------------------------------------------*
*&      Form  CTEREM
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item E – Remetente das Mercadorias Transportadas pelo CT-e
* E01 - rem
*&---------------------------------------------------------------------*
FORM CTEREM  USING  P_CTE.

  DATA: VL_ADRNR TYPE J_1BBRANCH-ADRNR,
        VL_NAME1 TYPE ADRC-NAME1.

  PERFORM CTNAB USING CTEREM     P_CTE.

* Identificar os documentos de transportes.
*  LOOP AT ti_lin INTO st_lin.

* Obter os dados do parceiro parceiro AG
*    CLEAR: st_partner_p.
*    READ TABLE ti_partner_p INTO st_partner_p WITH KEY docnum = st_lin-docnum
*                                                       parvw  = c_ag
*                                                       BINARY SEARCH.
*    CHECK sy-subrc EQ 0.

*    IF st_partner_p-land1 = c_br.
*      IF st_partner_p-cgc IS NOT INITIAL.
*        PERFORM ctnav USING cteremcnpj st_partner_p-cgc     p_cte.
*      ELSE.
*        PERFORM ctnav USING cteremcpf  st_partner_p-cpf     p_cte.
*      ENDIF.
*    ENDIF.
*    PERFORM ctnav USING cteremie     st_partner_p-stains  p_cte.
*    PERFORM ctnav USING cteremxnome  st_partner_p-name1   p_cte.

*  ENDLOOP.

* Obter o centro do primeiro registro, pois ele é o mesmo em todos.
  READ TABLE TI_VBAP_P INTO ST_VBAP INDEX 1.

*  CLEAR st_doctos.
*  SELECT SINGLE name state_insc stcd1 stcd2
*         INTO st_doctos
*         FROM j_1bbranch
*       WHERE bukrs = wk_header-bukrs
*         AND branch = st_vbap-werks.

  IF ST_VTTK-SHTYP = C_Z020.         "Transferência
    ST_VBAP-WERKS = VG_WERKS.
  ENDIF.
  SELECT SINGLE A~NAME1 J~STATE_INSC J~STCD1 J~STCD2
         INTO ST_DOCTOS
         FROM J_1BBRANCH AS J
        INNER JOIN ADRC AS A ON A~ADDRNUMBER = J~ADRNR
       WHERE BUKRS  = WK_HEADER-BUKRS
         AND BRANCH = ST_VBAP-WERKS.

  IF ST_DOCTOS-STCD1 IS NOT INITIAL.
    PERFORM LIMPA_NUMERO USING ST_DOCTOS-STCD1.
    PERFORM CTNAV USING CTEREMCNPJ VG_LIMPO  P_CTE.
  ELSE.
    PERFORM LIMPA_NUMERO USING ST_DOCTOS-STCD2.
    PERFORM CTNAV USING CTEREMCPF  VG_LIMPO  P_CTE.
  ENDIF.
  PERFORM LIMPA_NUMERO USING ST_DOCTOS-STATE_INSC.
  PERFORM CTNAV USING CTEREMIE     VG_LIMPO             P_CTE.
*  PERFORM ctnav USING cteremxnome  st_doctos-name        p_cte.
  PERFORM LCT USING ST_DOCTOS-NAME.

  IF VG_AMBIENTE = 'QAS'.
    PERFORM LCT USING 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL'.
    PERFORM CTNAV USING CTEREMXNOME  VG_LIMPO       P_CTE.
  ELSE.
    PERFORM CTNAV USING CTEREMXNOME  VG_LIMPO       P_CTE.
  ENDIF.

* Para fechar os dados, utilizar a rotina criada para nfe
*  PERFORM ctnfe USING cterem     p_cte.

ENDFORM.                    " CTEREM

*&---------------------------------------------------------------------*
*&      Form  CTEENDREM
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item E – Remetente das Mercadorias Transportadas pelo CT-e
* E08 - enderReme
*&---------------------------------------------------------------------*
FORM CTEENDREM  USING    P_CTE.

  DATA: VL_ADRNR TYPE TVST-ADRNR.

  PERFORM CTNAB USING CTEREMENDERREME     P_CTE.

* Identificar os documentos de transportes.
*  LOOP AT ti_lin INTO st_lin.

* Obter os dados do parceiro consignatário - parceiro AG
*    CLEAR: st_partner_p.
*    READ TABLE ti_partner_p INTO st_partner_p WITH KEY docnum = st_lin-docnum
*                                                       parvw  = c_ag
*                                                       BINARY SEARCH.
*    CHECK sy-subrc EQ 0.

* Obter os dados de endereço caso algum esteja faltando na tabela de parceiros da nota
*    IF st_partner_p-stras        IS INITIAL OR
*       st_partner_p-stras+25(10) IS INITIAL OR
*       st_partner_p-ort02        IS INITIAL OR
*       st_partner_p-pstlz        IS INITIAL.
*      PERFORM z_ler_cliente USING st_partner_p-parid.
*    ENDIF.

* O bter o local de expedição do primeiro registro, pois ele é o mesmo em todos.
  READ TABLE TI_VBAP INTO ST_VBAP INDEX 1.

  IF ST_VTTK-SHTYP = C_Z020.         "Transferência
    ST_VBAP-WERKS = VG_WERKS.
  ENDIF.
  CLEAR ST_END.
  SELECT SINGLE T~ADRNR A~CITY2 A~POST_CODE1 A~STREET A~HOUSE_NUM1 A~REGION
         INTO ST_END
         FROM TVST AS T
        INNER JOIN ADRC AS A
         ON T~ADRNR = A~ADDRNUMBER
       WHERE VSTEL = ST_VBAP-WERKS.
  PERFORM CTNAV USING CTEREMXLGR    ST_END-STREET(25)           P_CTE.
  IF NOT ST_END-HOUSE_NUM1 IS INITIAL.
    PERFORM CTNAV USING CTEREMNRO     ST_END-HOUSE_NUM1           P_CTE.
  ELSE.
    PERFORM CTNAV USING CTEREMNRO     C_SN           P_CTE.
  ENDIF.
  PERFORM CTNAF USING CTEREMXCPL    ''               P_CTE.
  PERFORM LCT USING ST_END-CITY2.
  PERFORM CTNAV USING CTEREMXBAIRRO VG_LIMPO P_CTE.

  REFRESH TI_J_1BTREG_CITY.
  SELECT TAXJURCODE INTO TABLE TI_J_1BTREG_CITY
                    FROM J_1BTREG_CITY
                   WHERE COUNTRY     = C_BR
                     AND REGION      = ST_END-REGION
                     AND PSTCD_FROM <= ST_END-POST_CODE1
                     AND PSTCD_TO   >= ST_END-POST_CODE1.
* Obter sempre o cep mais alto.
  SORT TI_J_1BTREG_CITY BY TAXJURCODE DESCENDING.
  CLEAR ST_J_1BTREG_CITY.
  READ TABLE TI_J_1BTREG_CITY INTO ST_J_1BTREG_CITY INDEX 1.
  PERFORM CTNAV USING CTEREMCMUN    ST_J_1BTREG_CITY-TAXJURCODE+3(7) P_CTE.
  PERFORM Z_CONVERTER_CEP USING ST_PARTNER-PSTLZ.
  PERFORM CTNAF USING CTEREMCEP     VG_PSTLZ                   P_CTE.

*  ENDLOOP.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEREMENDERREME     P_CTE.

ENDFORM.                    " CTEENDREM

*&---------------------------------------------------------------------*
*&      Form  CTENFE
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item E – Remetente das Mercadorias Transportadas pelo CT-e
* E40 - infNFe
*&---------------------------------------------------------------------*
FORM CTENFE  USING    P_CTE.

  DATA: VL_NFE       TYPE J_1BAA-NFE,
        VL_CHAVE(44) TYPE C,
        VL_DOCNUM    TYPE J_1BNFDOC-DOCNUM.

  SORT TI_HEADER_P BY DOCNUM.
  CLEAR VL_DOCNUM.

  LOOP AT TI_HEADER_P INTO WK_HEADER_P.

    IF VL_DOCNUM IS INITIAL.
      VL_DOCNUM = WK_HEADER_P-DOCNUM.
    ELSEIF VL_DOCNUM = WK_HEADER_P-DOCNUM.
      CONTINUE.
    ENDIF.

* Verificar se a nota é eletrônica.
    SELECT SINGLE NFE INTO VL_NFE
                      FROM J_1BAA
                     WHERE NFTYPE = WK_HEADER_P-NFTYPE.
    CHECK SY-SUBRC EQ 0 AND VL_NFE IS NOT INITIAL.
    PERFORM CTNAB USING CTEINFNFE      P_CTE.
*    PERFORM ctnav USING cteinfchave    wk_header_p-authcod  p_cte.
* Obter os campos que compõe a chave da nota fiscal eletronica

    CALL METHOD ZCL_UTIL=>MONTA_CHAVE_NFE
      EXPORTING
        I_DOCNUM = WK_HEADER_P-DOCNUM
        I_VALIDA = 'X'
      RECEIVING
        E_CHAVE  = VL_CHAVE.

*    SELECT SINGLE REGIO  NFYEAR  NFMONTH
*                  STCD1  MODEL   SERIE
*                  NFNUM9 DOCNUM9 CDV     INTO ST_J_1BNFE_ACTIVE
*                  FROM J_1BNFE_ACTIVE
*                 WHERE DOCNUM = WK_HEADER_P-DOCNUM.
*    CONCATENATE ST_J_1BNFE_ACTIVE-REGIO
*                ST_J_1BNFE_ACTIVE-NFYEAR
*                ST_J_1BNFE_ACTIVE-NFMONTH
*                ST_J_1BNFE_ACTIVE-STCD1
*                ST_J_1BNFE_ACTIVE-MODEL
*                ST_J_1BNFE_ACTIVE-SERIE
*                ST_J_1BNFE_ACTIVE-NFNUM9
*                ST_J_1BNFE_ACTIVE-DOCNUM9
*                ST_J_1BNFE_ACTIVE-CDV
*                INTO VL_CHAVE.

    PERFORM CTNAV USING CTEINFCHAVE    VL_CHAVE  P_CTE.
* Para fechar os dados, utilizar a rotina criada para nfe
    PERFORM CTNFE USING CTEINFNFE     P_CTE.
  ENDLOOP.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEREM     P_CTE.

ENDFORM.                    " CTENFE

*&---------------------------------------------------------------------*
*&      Form  CTEDESTMER
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item H – Destinatário das Mercadorias Transportadas pelo CT-e
* H01 - dest
*&---------------------------------------------------------------------*
FORM CTEDESTMER  USING    P_CTE.

  PERFORM CTNAB USING CTEDESTREMDEST     P_CTE.

* Identificar os documentos de transportes.
  LOOP AT TI_LIN INTO ST_LIN.

* Obter os dados do parceiro consignatário - parceiro WE
    CLEAR: ST_PARTNER_P.
    READ TABLE TI_PARTNER_P INTO ST_PARTNER_P WITH KEY DOCNUM = ST_LIN-DOCNUM
                                                       PARVW  = C_WE
                                                       BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.

    IF ST_PARTNER_P-LAND1 = C_BR.
      IF ST_PARTNER_P-CGC IS NOT INITIAL.
        PERFORM CTNAV USING CTEDESTREMCNPJ ST_PARTNER_P-CGC     P_CTE.
      ELSE.
        PERFORM CTNAV USING CTEDESTREMCPF  ST_PARTNER_P-CPF     P_CTE.
      ENDIF.
    ENDIF.
    PERFORM CTNAV USING CTEDESTREMIE     ST_PARTNER_P-STAINS  P_CTE.
    PERFORM LCT USING ST_PARTNER_P-NAME1.

    IF VG_AMBIENTE = 'QAS'.
      PERFORM CTNAV USING CTEDESTREMXNOME  'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL'   P_CTE.
    ELSE.
      PERFORM CTNAV USING CTEDESTREMXNOME  VG_LIMPO   P_CTE.
    ENDIF.

    PERFORM CTNAF USING CTEDESTREMFONE   ''         P_CTE.
    PERFORM CTNAF USING CTEDESTREMISUF   ''         P_CTE.

  ENDLOOP.

ENDFORM.                    " CTEDESTMER

*&---------------------------------------------------------------------*
*&      Form  CTEENDDESTM
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item H – Destinatário das Mercadorias Transportadas pelo CT-e
* H08 - enderDest
*&---------------------------------------------------------------------*
FORM CTEENDDESTM  USING    P_CTE.

  DATA: VL_PSTLZ(8) TYPE N,
        VL_TXJCD    TYPE KNA1-TXJCD.

  PERFORM CTNAB USING CTEENDDESTENDERDEST     P_CTE.

* Identificar os documentos de transportes.
  LOOP AT TI_LIN INTO ST_LIN.

* Obter os dados do parceiro consignatário - parceiro WE
    CLEAR: ST_PARTNER_P.
    READ TABLE TI_PARTNER_P INTO ST_PARTNER_P WITH KEY DOCNUM = ST_LIN-DOCNUM
                                                       PARVW  = C_WE
                                                       BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.

* Obter os dados de endereço caso algum esteja faltando na tabela de parceiros da nota
*    IF st_partner_p-stras        IS INITIAL OR
*       st_partner_p-stras+25(10) IS INITIAL OR
*       st_partner_p-ort02        IS INITIAL OR
*       st_partner_p-pstlz        IS INITIAL.
*      PERFORM z_ler_cliente USING st_partner_p-parid.
*    ENDIF.
*    PERFORM ctnav USING cteenddestxlgr    st_partner_p-stras(25)     p_cte.
*    PERFORM ctnav USING cteenddestnro     st_partner_p-stras+25(10)  p_cte.
*    PERFORM ctnaf USING cteenddestxcpl    ''                         p_cte.
*    PERFORM ctnav USING cteenddestxbairro st_partner_p-ort02         p_cte.
    CLEAR ST_END.
    SELECT SINGLE K~ADRNR A~CITY2 A~POST_CODE1 A~STREET A~HOUSE_NUM1 A~REGION
           INTO ST_END
           FROM KNA1 AS K
          INNER JOIN ADRC AS A
           ON K~ADRNR = A~ADDRNUMBER
         WHERE KUNNR = ST_PARTNER_P-PARID.
    PERFORM CTNAV USING CTEREMXLGR    ST_END-STREET(25)           P_CTE.
    IF NOT ST_END-HOUSE_NUM1 IS INITIAL.
      PERFORM CTNAV USING CTEREMNRO     ST_END-HOUSE_NUM1           P_CTE.
    ELSE.
      PERFORM CTNAV USING CTEREMNRO     C_SN                      P_CTE.
    ENDIF.
    PERFORM CTNAF USING CTEREMXCPL    ''        P_CTE.
    PERFORM LCT USING ST_END-CITY2.
    PERFORM CTNAV USING CTEREMXBAIRRO VG_LIMPO  P_CTE.

*    REFRESH ti_j_1btreg_city.
*    SELECT taxjurcode INTO TABLE ti_j_1btreg_city
*                      FROM j_1btreg_city
*                     WHERE country     = c_br
*                       AND region      = st_partner_p-regio
*                       AND pstcd_from <= st_partner_p-pstlz
*                       AND pstcd_to   >= st_partner_p-pstlz.
** Obter sempre o cep mais alto.
*    SORT ti_j_1btreg_city BY taxjurcode DESCENDING.
*    CLEAR st_j_1btreg_city.
*    READ TABLE ti_j_1btreg_city INTO st_j_1btreg_city INDEX 1.
*    PERFORM ctnav USING cteenddestcmun    st_j_1btreg_city-taxjurcode+3(7) p_cte.

* Obter o domicílio fiscal que está no cadatro do cliente (kna1) pois o que está
* na tabela de parceiros da nota está errado.
    CLEAR VL_TXJCD.
    SELECT SINGLE TXJCD INTO VL_TXJCD
                        FROM KNA1
                       WHERE KUNNR = ST_PARTNER_P-PARID.

    PERFORM CTNAV USING CTEENDDESTCMUN    VL_TXJCD+3(7)              P_CTE.
    PERFORM Z_CONVERTER_CEP USING ST_PARTNER_P-PSTLZ.
    PERFORM CTNAF USING CTEENDDESTCEP     VG_PSTLZ                   P_CTE.

  ENDLOOP.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEENDDESTENDERDEST     P_CTE.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEDESTREMDEST     P_CTE.

ENDFORM.                    " CTEENDDESTM

*&---------------------------------------------------------------------*
*&      Form  CTEVLPRESTSRV
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item I – Valores da Prestação de Serviço
* I01 - vPrest
*&---------------------------------------------------------------------*
FORM CTEVLPRESTSRV  USING    P_CTE.

  DATA VL_VALOR TYPE VFKP-KZWI1.

  PERFORM CTNAB USING  CTEVLRPSERV           P_CTE.
  CLEAR VL_VALOR.
  LOOP AT TI_VFKP INTO ST_VFKP.
    VL_VALOR = VL_VALOR + ST_VFKP-NETWR. "Valor Líquido
*    vl_valor = vl_valor      +
*               st_vfkp-kzwi1 +  "Subtotal-condição 1 do esquema de cálculo
*               st_vfkp-kzwi6.   "Subtotal-condição 6 do esquema de cálculo
  ENDLOOP.
  PERFORM CTNAVN USING CTEVLRPSERVVTPREST    VL_VALOR   P_CTE.
  PERFORM CTNAVN USING CTEVLRPSERVVREC       VL_VALOR   P_CTE.

* Item I – Valores da Prestação de Serviço
* I04 - comp
  PERFORM CTNAB  USING CTECOMPCOMP           P_CTE.
  PERFORM CTNAV  USING CTECOMPXNOME          C_FRETE_PESO   P_CTE.
  PERFORM CTNAVN USING CTECOMPVCOMP          VL_VALOR       P_CTE.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE  USING CTECOMPCOMP           P_CTE.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE  USING CTEVLRPSERV           P_CTE.
ENDFORM.                    " CTEVLPRESTSRV

*&---------------------------------------------------------------------*
*&      Form  CTECOMP
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item I – Valores da Prestação de Serviço
* I04 - comp
*&---------------------------------------------------------------------*
FORM CTECOMP  USING    P_CTE.

  DATA VL_VALOR TYPE VFKP-KZWI1.

  PERFORM CTNAB  USING CTECOMPCOMP           P_CTE.
  CLEAR VL_VALOR.
  LOOP AT TI_VFKP INTO ST_VFKP.
    VL_VALOR = ST_VFKP-KZWI1 +  "Subtotal-condição 1 do esquema de cálculo
               ST_VFKP-KZWI6.   "Subtotal-condição 6 do esquema de cálculo
  ENDLOOP.
  PERFORM CTNAV  USING CTECOMPXNOME          C_FRETE_PESO   P_CTE.
  PERFORM CTNAVN USING CTECOMPVCOMP          VL_VALOR       P_CTE.
* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE  USING CTECOMPCOMP           P_CTE.

ENDFORM.                    " CTECOMP

*&---------------------------------------------------------------------*
*&      Form  CTEIMP
*&---------------------------------------------------------------------*
* Monta o arquivo XML
* Item J – Informações Relativas aos Impostos
* J01 - imp
* J02 - ICMS
*&---------------------------------------------------------------------*
FORM CTEIMP  USING    P_CTE.

  DATA: VL_BICM  TYPE KONV-KAWRT,
        VL_ALIQ  TYPE KONV-KBETR,
        VL_VICMS TYPE VFKP-KZWI6.

  PERFORM CTNAB USING CTEIMPIMP           P_CTE.
  PERFORM CTNAB USING CTEIMPICMS          P_CTE.
*  PERFORM ctnav USING cteimpcst           c_0       p_cte.
  CLEAR: VL_BICM,
         VL_ALIQ.
  LOOP AT TI_KONV INTO ST_KONV
                  WHERE KSCHL <> C_ZICM.
    ADD ST_KONV-KAWRT TO VL_BICM.
    IF VL_ALIQ IS INITIAL.
      VL_ALIQ = ST_KONV-KBETR / 10.
    ENDIF.
  ENDLOOP.

  IF VL_BICM IS NOT INITIAL.
    PERFORM CTNAV USING CTEIMPCST           C_0       P_CTE.
    PERFORM CTNAVN USING CTEIMPVBC          VL_BICM   P_CTE.
  ELSE.
* Quando não há base de icms o código deve ser 40 (Isento)
    PERFORM CTNAV USING CTEIMPCST           C_40      P_CTE.
  ENDIF.
  IF VL_ALIQ IS NOT INITIAL.
    PERFORM CTNAVN USING CTEIMPPICMS        VL_ALIQ   P_CTE.
  ENDIF.
  CLEAR VL_VICMS.
  LOOP AT TI_VFKP INTO ST_VFKP.
    ADD ST_VFKP-KZWI6 TO VL_VICMS.
  ENDLOOP.
  IF VL_VICMS IS NOT INITIAL.
    PERFORM CTNAVN USING CTEIMPVICMS       VL_VICMS  P_CTE.
  ENDIF.
  PERFORM CTNAF  USING CTEIMPPREDBC       ''        P_CTE.
  PERFORM CTNAF  USING CTEIMPVCRED        ''        P_CTE.
  PERFORM CTNAF  USING CTEIMPINFADFISCO   ''        P_CTE.
* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEIMPICMS          P_CTE.
  PERFORM CTNFE USING CTEIMPIMP           P_CTE.

ENDFORM.                    " CTEIMP
*&---------------------------------------------------------------------*
*&      Form  CTEINFCT
*&---------------------------------------------------------------------*
* Item K – Informações de CT-e normal ou CT-e emitido em hipótese de
* anulação de débito
* K01 - infCTeNorm
*&---------------------------------------------------------------------*
FORM CTEINFCT  USING    P_CTE.

  PERFORM CTNAB USING CTEINFINFCTENORM           P_CTE.

ENDFORM.                    " CTEINFCT_FECHA

*&---------------------------------------------------------------------*
* Item K – Informações de CT-e normal ou CT-e emitido em hipótese de
* anulação de débito
* K01 - infCTeNorm
*&---------------------------------------------------------------------*
FORM CTEINFCT_FECHA  USING    P_CTE.

  PERFORM CTNFE USING CTEINFINFCTENORM           P_CTE.

ENDFORM.                    " CTEINFCT_FECHA
*&---------------------------------------------------------------------*
*&      Form  CTECARGA
*&---------------------------------------------------------------------*
* Item L - Informações da Carga Transportada
* L01 - infCarga
*&---------------------------------------------------------------------*
FORM CTECARGA  USING    P_CTE.

  TYPES: BEGIN OF Y_MATERIAL,
           MATNR TYPE J_1BNFLIN-MATNR,
           MENGE TYPE J_1BNFLIN-MENGE,
         END OF Y_MATERIAL,

         BEGIN OF Y_UNIDADE,
           MEINS TYPE J_1BNFLIN-MEINS,
           MENGE TYPE J_1BNFLIN-MENGE,
         END OF Y_UNIDADE.

  DATA: VL_TOT_MERC TYPE J_1BNFLIN-NETWR,
        VL_MAKTX    TYPE MAKT-MAKTX,
        TL_MATERIAL TYPE TABLE OF Y_MATERIAL,
        TL_UNIDADE  TYPE TABLE OF Y_UNIDADE,
        ST_MATERIAL TYPE Y_MATERIAL,
        ST_UNIDADE  TYPE Y_UNIDADE.

  PERFORM CTNAB USING CTECARGAINFCARGA           P_CTE.
  CLEAR VL_TOT_MERC.
  LOOP AT TI_ITEM_P INTO ST_ITEM_P.
*    vl_tot_merc = st_item_p-menge * st_item_p-netpr.
    ADD ST_ITEM_P-NETWR TO VL_TOT_MERC.
    ST_MATERIAL-MATNR = ST_ITEM_P-MATNR.
    ST_MATERIAL-MENGE = ST_ITEM_P-MENGE.
    COLLECT ST_MATERIAL INTO TL_MATERIAL.
* Acumular as quantidades por unidade
    ST_UNIDADE-MEINS = ST_ITEM_P-MEINS.
    ST_UNIDADE-MENGE = ST_ITEM_P-MENGE.
    COLLECT ST_UNIDADE INTO TL_UNIDADE.
  ENDLOOP.

  IF NOT XML_CTE_1_04 IS INITIAL.
    PERFORM CTNAVN USING CTECARGAVCARGA VL_TOT_MERC   P_CTE.
  ELSE.
    PERFORM CTNAVN USING CTECARGAVMERC  VL_TOT_MERC   P_CTE.
  ENDIF.

  SORT TL_MATERIAL BY MENGE DESCENDING.
  READ TABLE TL_MATERIAL INTO ST_MATERIAL INDEX 1.
  SELECT SINGLE MAKTX INTO VL_MAKTX
                      FROM MAKT
                     WHERE MATNR = ST_MATERIAL-MATNR
                       AND SPRAS = SY-LANGU.
  PERFORM CTNAVN USING CTECARGAPROPRED  VL_MAKTX   P_CTE.

* Item L - Informações da Carga Transportada
* L05 - infQ
  PERFORM CTNAB USING CTEQTDEINFQ         P_CTE.

* Obter as quantidades acumuladas por unidade
*  LOOP AT ti_item_p INTO st_item_p.
*    IF  st_item_p-meins = c_kg.
*      PERFORM ctnav USING cteqtdecunid    c_01              p_cte.
*      PERFORM ctnav USING cteqtdetpmed    c_peso_bruto      p_cte.
*    ELSEIF st_item_p-meins = c_to.
*      PERFORM ctnav USING cteqtdecunid    c_02              p_cte.
*      PERFORM ctnav USING cteqtdetpmed    c_peso_bruto      p_cte.
*    ELSEIF st_item_p-meins = c_un.
*      PERFORM ctnav USING cteqtdecunid    c_03              p_cte.
*      PERFORM ctnav USING cteqtdetpmed    c_caixa           p_cte.
*    ELSEIF st_item_p-meins = c_lt.
*      PERFORM ctnav USING cteqtdecunid    c_04              p_cte.
*      PERFORM ctnav USING cteqtdetpmed    c_litragem        p_cte.
*    ENDIF.
  LOOP AT TL_UNIDADE INTO ST_UNIDADE.
    IF  ST_UNIDADE-MEINS = C_KG.
      PERFORM CTNAV USING CTEQTDECUNID    C_01              P_CTE.
      PERFORM CTNAV USING CTEQTDETPMED    C_PESO_BRUTO      P_CTE.
    ELSEIF ST_UNIDADE-MEINS = C_TO.
      PERFORM CTNAV USING CTEQTDECUNID    C_02              P_CTE.
      PERFORM CTNAV USING CTEQTDETPMED    C_PESO_BRUTO      P_CTE.
    ELSEIF ST_UNIDADE-MEINS = C_UN.
      PERFORM CTNAV USING CTEQTDECUNID    C_03              P_CTE.
      PERFORM CTNAV USING CTEQTDETPMED    C_CAIXA           P_CTE.
    ELSEIF ST_UNIDADE-MEINS = C_LT.
      PERFORM CTNAV USING CTEQTDECUNID    C_04              P_CTE.
      PERFORM CTNAV USING CTEQTDETPMED    C_LITRAGEM        P_CTE.
    ENDIF.
    PERFORM CTNAVN USING CTEQTDEQCARGA    ST_UNIDADE-MENGE   P_CTE.
  ENDLOOP.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEQTDEINFQ         P_CTE.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTECARGAINFCARGA           P_CTE.

ENDFORM.                    " CTECARGA

*&---------------------------------------------------------------------*
*&      Form  CTEQTDE
*&---------------------------------------------------------------------*
* Item L - Informações da Carga Transportada
* L05 - infQ
*&---------------------------------------------------------------------*
FORM CTEQTDE  USING    P_CTE.

  PERFORM CTNAB USING CTEQTDEINFQ         P_CTE.

  LOOP AT TI_ITEM_P INTO ST_ITEM_P.
    IF  ST_ITEM_P-MEINS = C_KG.
      PERFORM CTNAV USING CTEQTDECUNID    C_01              P_CTE.
      PERFORM CTNAV USING CTEQTDETPMED    C_PESO_BRUTO      P_CTE.
    ELSEIF ST_ITEM_P-MEINS = C_TO.
      PERFORM CTNAV USING CTEQTDECUNID    C_02              P_CTE.
      PERFORM CTNAV USING CTEQTDETPMED    C_PESO_BRUTO      P_CTE.
    ELSEIF ST_ITEM_P-MEINS = C_UN.
      PERFORM CTNAV USING CTEQTDECUNID    C_03              P_CTE.
      PERFORM CTNAV USING CTEQTDETPMED    C_CAIXA           P_CTE.
    ELSEIF ST_ITEM_P-MEINS = C_LT.
      PERFORM CTNAV USING CTEQTDECUNID    C_04              P_CTE.
      PERFORM CTNAV USING CTEQTDETPMED    C_LITRAGEM        P_CTE.
    ENDIF.
    PERFORM CTNAVN USING CTEQTDEQCARGA    ST_ITEM_P-MENGE   P_CTE.

  ENDLOOP.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEQTDEINFQ         P_CTE.

ENDFORM.                    " CTEQTDE

*&---------------------------------------------------------------------*
*&      Form  CTEMODAL
*&---------------------------------------------------------------------*
* Item O – Dados específicos do Modal RODOVIÁRIO
* O01 - rodo
*&---------------------------------------------------------------------*
FORM CTEMODAL  USING    P_CTE.

  DATA VL_DATA(10) TYPE C.

  PERFORM CTNAB USING CTEMODALRODO       P_CTE.
  PERFORM CTNAV USING CTEMODALRNTRC      ST_TDLNR-BAHNS      P_CTE.
  CONCATENATE VG_DTA_ENT(4)       "ano
              '-'
              VG_DTA_ENT+4(2)     "mês
              '-'
              VG_DTA_ENT+6(2)     "dia
              INTO VL_DATA.

  IF VG_VERSAO_CTE < 3. "CS2017002143 CT-e 3.0 Ini
    PERFORM CTNAV USING CTEMODALDPREV      VL_DATA             P_CTE.
*   Na Maggi a lotação deve ser sempre Sim (1)
*    IF st_vttk-sdabw = c_0002.
    PERFORM CTNAV USING CTEMODALLOTA    C_1                 P_CTE.
*    ELSE.
*      PERFORM ctnav USING ctemodallota    c_0                 p_cte.
*    ENDIF.
  ENDIF. "CS2017002143 CT-e 3.0 - Fim

ENDFORM.                    " CTEMODAL

*&---------------------------------------------------------------------*
*&      Form  CTEMODAL_FECHA
*&---------------------------------------------------------------------*
* Item O – Dados específicos do Modal RODOVIÁRIO
* O01 - rodo
*&---------------------------------------------------------------------*
FORM CTEMODAL_FECHA  USING    P_CTE.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE USING CTEMODALRODO       P_CTE.

ENDFORM.                    " CTEMODAL_FECHA

*&---------------------------------------------------------------------*
*&      Form  CTEVEICULOS
*&---------------------------------------------------------------------*
* Item O – Dados específicos do Modal RODOVIÁRIO
* O28 - veic
*&---------------------------------------------------------------------*
FORM CTEVEICULOS  USING    P_CTE.

  DATA VL_PLACA(7) TYPE C.

  CHECK VG_VERSAO_CTE < 3. "CS2017002143 CT-e 3.0

  DO 4 TIMES.

    CASE SY-INDEX.
      WHEN 1.
        VL_PLACA = ST_VTTK-TEXT1(7).
      WHEN 2.
        VL_PLACA = ST_VTTK-TEXT2(7).
      WHEN 3.
        VL_PLACA = ST_VTTK-TEXT3(7).
      WHEN 4.
        VL_PLACA = ST_VTTK-TEXT4(7).
    ENDCASE.
    CHECK VL_PLACA IS NOT INITIAL.

* Selecionar os dados dos veículos.
    SELECT SINGLE * INTO ST_ZLEST0002 FROM ZLEST0002 WHERE PC_VEICULO = VL_PLACA.

    CHECK SY-SUBRC EQ 0.

    PERFORM CTNAB  USING CTEVEICVEIC       P_CTE.
    PERFORM CTNAV  USING CTEVEICRENAVAM    ST_ZLEST0002-CD_RENAVAM     P_CTE.
    PERFORM CTNAV  USING CTEVEICPLACA      ST_ZLEST0002-PC_VEICULO     P_CTE.
    PERFORM CTNAVN USING CTEVEICTARA       ST_ZLEST0002-TARA           P_CTE.
    PERFORM CTNAV  USING CTEVEICCAPKG      ST_ZLEST0002-CAP_KG         P_CTE.
    PERFORM CTNAV  USING CTEVEICCAPM3      ST_ZLEST0002-CAP_M3         P_CTE.
    IF ST_VTTK-ADD04 = C_0000000001.
      PERFORM CTNAV  USING CTEVEICTPPROP  C_P  P_CTE.
    ELSE.
      PERFORM CTNAV  USING CTEVEICTPPROP  C_T  P_CTE.
    ENDIF.
    PERFORM CTNAV  USING CTEVEICTPVEIC     ST_ZLEST0002-TP_VEICULO     P_CTE.
    PERFORM CTNAV  USING CTEVEICTPROD      ST_ZLEST0002-TP_RODADO      P_CTE.
    PERFORM CTNAV  USING CTEVEICTPCAR      ST_ZLEST0002-TP_CARROCERIA2 P_CTE.
    PERFORM CTNAV  USING CTEVEICUF         ST_ZLEST0002-CD_UF          P_CTE.

    "Informações de proprietários dos veículos
    PERFORM CTEPROPRIETARIOS  USING P_CTE.

* Para fechar os dados, utilizar a rotina criada para nfe
    PERFORM CTNFE USING CTEVEICVEIC        P_CTE.

  ENDDO.
ENDFORM.                    " CTEVEICULOS

*&---------------------------------------------------------------------*
*&      Form  CTEVEICULOS_FECHA
*&---------------------------------------------------------------------*
* Item O – Dados específicos do Modal RODOVIÁRIO
* O28 - veic
*----------------------------------------------------------------------*
FORM CTEVEICULOS_FECHA  USING    P_CTE.

* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE  USING CTEVEICVEIC       P_CTE.

ENDFORM.                    " CTEVEICULOS_FECHA

*&---------------------------------------------------------------------*
*&      Form  CTEPROPRIETARIOS
*&---------------------------------------------------------------------*
* Item O – Dados específicos do Modal RODOVIÁRIO
* O40 - prop
*&---------------------------------------------------------------------*
FORM CTEPROPRIETARIOS  USING    P_CTE.

  DATA: E_MAIL    TYPE AD_SMTPADR.

* Buscar os dados do parceiro PV (proprietário do veículo).
  CLEAR ST_LFA1.
  SELECT SINGLE L~NAME1 L~STCD1 L~STCD2 L~STCD3 L~BAHNS A~REGION L~ADRNR
         INTO ST_LFA1
         FROM VTPA AS V
        INNER JOIN LFA1 AS L ON V~LIFNR = L~LIFNR
        INNER JOIN ADRC AS A ON V~ADRNR = A~ADDRNUMBER
       WHERE VBELN = ST_VTTK-TKNUM
         AND PARVW = C_PV.
  CHECK SY-SUBRC = 0.

*  LOOP AT ti_header_p INTO wk_header_p.

  DATA: VL_GRAVA_SN  TYPE C,
        VL_11POS(11) TYPE C,
        VL_14POS(14) TYPE C,
        VL_BAHNS     TYPE LFA1-BAHNS.

  PERFORM CTNAB  USING CTEPROPPROP       P_CTE.

* CPF
  CLEAR VL_11POS.
  IF ST_LFA1-STCD2 IS NOT INITIAL.
    VL_11POS = ST_LFA1-STCD2.
    SHIFT VL_11POS RIGHT DELETING TRAILING ' '.
    OVERLAY VL_11POS WITH '00000000000'.
    PERFORM CTNAV  USING CTEPROPCPF        VL_11POS         P_CTE.
  ENDIF.

* CNPJ
* Informar o CNPJ se o CPF estiver em branco.
  CLEAR VL_14POS.
  IF ST_LFA1-STCD1 IS NOT INITIAL AND
     ST_LFA1-STCD2 IS INITIAL.
    VL_14POS = ST_LFA1-STCD1.
    SHIFT VL_14POS RIGHT DELETING TRAILING ' '.
    OVERLAY VL_14POS WITH '00000000000000'.
    PERFORM CTNAV  USING CTEPROPCNPJ       VL_14POS         P_CTE.
  ENDIF.

  PERFORM CTNAV  USING CTEPROPCRNTRC     ST_LFA1-BAHNS    P_CTE.
  PERFORM CTNAV  USING CTEPROPXNOME      ST_LFA1-NAME1    P_CTE.

  IF ( ST_LFA1-STCD3 IS NOT INITIAL ) AND ( ST_LFA1-STCD3 NE 'ISENTO' ).
    PERFORM CTNAF  USING CTEPROPIE       ST_LFA1-STCD3    P_CTE.
    IF ( ST_LFA1-REGION IS NOT INITIAL ).
      PERFORM CTNAV  USING CTEPROPUF       ST_LFA1-REGION   P_CTE.
    ENDIF.
  ENDIF.

  PERFORM CTNAV  USING CTEPROPTPPROP     C_0              P_CTE.

  IF NOT ST_LFA1-ADRNR IS INITIAL.
    CLEAR: E_MAIL.
    SELECT SINGLE SMTP_ADDR INTO E_MAIL
      FROM ADR6
     WHERE ADDRNUMBER EQ ST_LFA1-ADRNR.
    PERFORM CTNAFV USING CTEEMAIL          E_MAIL           P_CTE.
  ENDIF.
* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE  USING CTEPROPPROP       P_CTE.

*  ENDLOOP.

ENDFORM.                    " CTEPROPRIETARIOS

*&---------------------------------------------------------------------*
*&      Form  CTEMOTORISTA
*&---------------------------------------------------------------------*
* Item O – Dados específicos do Modal RODOVIÁRIO
* O50 - moto
*----------------------------------------------------------------------*
FORM CTEMOTORISTA  USING    P_CTE.

  DATA: VL_NAME1 TYPE LFA1-NAME1,
        VL_STCD2 TYPE LFA1-STCD2.

  CHECK VG_VERSAO_CTE < 3. "CS2017002143 CT-e 3.0

* Buscar os dados do parceiro MT (motorista).
  CLEAR ST_LFA1.
  SELECT SINGLE L~NAME1 L~STCD2
         INTO (VL_NAME1, VL_STCD2)
         FROM VTPA AS V
        INNER JOIN LFA1 AS L ON V~LIFNR = L~LIFNR
       WHERE VBELN = ST_VTTK-TKNUM
         AND PARVW = C_MT.
  CHECK SY-SUBRC = 0.

  PERFORM CTNAB  USING CTEMOTOMOTO       P_CTE.
  PERFORM LCT USING VL_NAME1.
  PERFORM CTNAV  USING CTEMOTOXNOME      VG_LIMPO    P_CTE.
  PERFORM CTNAV  USING CTEMOTOCPF        VL_STCD2    P_CTE.
* Para fechar os dados, utilizar a rotina criada para nfe
  PERFORM CTNFE  USING CTEMOTOMOTO       P_CTE.

ENDFORM.                    " CTEMOTORISTA

*&---------------------------------------------------------------------*
*&      Form  CANCABREXML
*&---------------------------------------------------------------------*
FORM CANCABREXML  USING    P_NFE.

  PERFORM CTNAB USING PADRAO  P_NFE.
  PERFORM CTNAB USING CANCNFE P_NFE.

ENDFORM.                    " CANCABREXML

*&---------------------------------------------------------------------*
*&      Form  CANCABREXML
*&---------------------------------------------------------------------*
FORM CANCABREXML2  USING    P_NFE.

  PERFORM CTNAB USING PADRAO  P_NFE.
  PERFORM CTNAB USING CANCCTE P_NFE.

ENDFORM.                    " CANCABREXML

*&---------------------------------------------------------------------*
*&      Form  CANCFECHAXML
*&---------------------------------------------------------------------*
FORM CANCFECHAXML  USING    P_NFE.

  PERFORM CTNFE USING CANCNFE P_NFE.

ENDFORM.                    " CANCFECHAXML

*&---------------------------------------------------------------------*
*&      Form  CANCFECHAXML
*&---------------------------------------------------------------------*
FORM CANCFECHAXML2  USING    P_NFE.

  PERFORM CTNFE USING CANCCTE P_NFE.

ENDFORM.                    " CANCFECHAXML

*&---------------------------------------------------------------------*
*&      Form  CANCELARNFE
*&---------------------------------------------------------------------*
FORM CANCELARNFE  TABLES   XML_EXT2 TYPE J1B_NF_XML_EXTENSION2_TAB
                  USING    XML_IN   TYPE J1B_NF_XML_HEADER
                           NFE
                           WA_J_1BNFDOC TYPE J_1BNFDOC.

  DATA: XDEMI         TYPE C LENGTH 10,
        LOCAL_NEGOCIO TYPE J_1BBRANCH.

  CONCATENATE  WA_J_1BNFDOC-DOCDAT(4) '-' WA_J_1BNFDOC-DOCDAT+4(2) '-' WA_J_1BNFDOC-DOCDAT+6(2) INTO XDEMI.

  DATA: JUSTIFICATIVA TYPE STRING.

  LOOP AT XML_EXT2.
    CONCATENATE JUSTIFICATIVA XML_EXT2-VALUE INTO JUSTIFICATIVA.
  ENDLOOP.

  PERFORM CANCABREXML USING NFE.
  PERFORM CTNAV USING CANCNFECNPJ     XML_IN-C_CNPJ NFE.

  IF VG_XML_VERSAO EQ 2.
    SELECT SINGLE * INTO LOCAL_NEGOCIO
      FROM J_1BBRANCH
     WHERE BUKRS  EQ WA_J_1BNFDOC-BUKRS
       AND BRANCH EQ WA_J_1BNFDOC-BRANCH.

    PERFORM CTNAV  USING CANCNFEIE    LOCAL_NEGOCIO-STATE_INSC  NFE.
  ENDIF.

  PERFORM CTNAV USING CANCNFENNOTA    XML_IN-NNF    NFE.
  PERFORM CTNAV USING CANCNFENSERIE   XML_IN-SERIE  NFE.
  PERFORM CTNAV USING CANCNFEXJUST    JUSTIFICATIVA NFE.
  PERFORM CTNAV USING CANCNFEDATAEMIS XDEMI         NFE.
  PERFORM CANCFECHAXML USING NFE.

ENDFORM.                    " CANCELARNFE
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_DADOS_NF
*&---------------------------------------------------------------------*
* Obtenção dos dados da nota fiscal.
*----------------------------------------------------------------------*
FORM Z_OBTER_DADOS USING P_DOCNUM TYPE J1B_NF_XML_HEADER-DOCNUM.

  DATA: VL_CANCEL TYPE J_1BNFE_ACTIVE-CANCEL,
        VL_TABIX  TYPE SY-TABIX,
        VL_MBLNR  TYPE MSEG-MBLNR.

* Limpar tabelas internas
  REFRESH: TI_PARTNER,
           TI_ITEM,
           TI_ITEM_TAX,
           TI_HEADER_MSG,
           TI_REFER_MSG,
           TI_HEADER_P,
           TI_PARTNER_P,
           TI_ITEM_P,
           TI_ITEM_TAX_P,
           TI_HEADER_MSG_P,
           TI_REFER_MSG_P,
           TI_VBRP,
           TI_VBAK,
           TI_VTPA,
           TI_VTTP.

  CLEAR: WK_HEADER, ST_VBRP, ST_VBRK, ST_VTTK, ST_TDLNR, ST_TTDS.

  CALL FUNCTION 'Z_SD_NFES_DA_CTE'
    EXPORTING
      MP_DOCNUM          = P_DOCNUM
    TABLES
      MT_PARTNER         = TI_PARTNER
      MT_ITEM            = TI_ITEM
      MT_ITEM_TAX        = TI_ITEM_TAX
      MT_HEADER_MSG      = TI_HEADER_MSG
      MT_REFER_MSG       = TI_REFER_MSG
      MT_VBRP            = TI_VBRP
      MT_VBAK            = TI_VBAK
      MT_VTTP            = TI_VTTP
      MT_HEADRE_NOTA_NFE = TI_HEADER_P
      MT_ITEM_NOTA       = TI_ITEM_P
      MT_PARTNER_P       = TI_PARTNER_P
      MT_ITEM_TAX_P      = TI_ITEM_TAX_P
      MT_HEADER_MSG_P    = TI_HEADER_MSG_P
      MT_REFER_MSG_P     = TI_REFER_MSG_P
    CHANGING
      MR_VTTK            = ST_VTTK
      MR_HEADER_NOTA_CTE = WK_HEADER
      MR_VBAK            = ST_VBAK
      MR_VBRK            = ST_VBRK
      MR_LFA1            = ST_TDLNR
      MR_TTDS            = ST_TTDS.

  CHECK TI_VBRP  IS NOT INITIAL.
  CHECK ST_VBRK  IS NOT INITIAL.
  CHECK TI_VBAK  IS NOT INITIAL.
  CHECK ST_VTTK  IS NOT INITIAL.
  CHECK ST_TDLNR IS NOT INITIAL.
  CHECK ST_TTDS  IS NOT INITIAL.
  CHECK TI_VTTP  IS NOT INITIAL.
  CHECK ST_VBAK  IS NOT INITIAL.


  LOOP AT TI_ITEM_P INTO ST_ITEM_P.
    ST_LIN-DOCNUM = ST_ITEM_P-DOCNUM.
    APPEND ST_LIN TO TI_LIN.
  ENDLOOP.

  READ TABLE TI_VBRP INTO ST_VBRP INDEX 1.

* Buscar os dados de controle do parceiro diretamente nas tabelas
* de dados mestres, isto porque em caso de recusa do sefaz por erro
* de cadastro na nota, apoós a atualização do cadastro a nova nota
* não está trazendo os dados atualizados.
  PERFORM Z_OBTER_DADOS_PARCEIROS_CTRL TABLES TI_PARTNER.

* Obter os parceiros cadastrados (fornecedores e clientes)
* Fornecedores
  SELECT P~PARVW P~LIFNR L~NAME1 L~STCD1 L~STCD2 L~STCD3 INTO TABLE TI_VTPA
                     FROM VBAK AS K
                INNER JOIN VTPA AS P ON P~VBELN = K~TKNUM
                INNER JOIN LFA1 AS L ON L~LIFNR = P~LIFNR
               FOR ALL ENTRIES IN TI_VBRP
                    WHERE K~VBELN = TI_VBRP-AUBEL.
* Clientes
  SELECT P~PARVW P~KUNNR X~NAME1 X~STCD1 X~STCD2 X~STCD3 APPENDING TABLE TI_VTPA
                     FROM VBAK AS K
                INNER JOIN VTPA AS P ON P~VBELN = K~TKNUM
                INNER JOIN KNA1 AS X ON X~KUNNR = P~KUNNR
               FOR ALL ENTRIES IN TI_VBRP
                    WHERE K~VBELN = TI_VBRP-AUBEL.

* Obter o texto (modal)
  CLEAR ST_T173T.
  SELECT SINGLE VSART BEZEI INTO ST_T173T FROM T173T WHERE SPRAS = C_PT AND VSART = ST_VTTK-VSART.

* Documentos de remessa referente ao documento de transporte de produtos
  REFRESH TI_LIPS.
  SELECT VBELN VGBEL VGPOS WERKS VBELN INTO TABLE TI_LIPS FROM LIPS FOR ALL ENTRIES IN TI_VTTP WHERE VBELN = TI_VTTP-VBELN.
  CHECK SY-SUBRC EQ 0.

* Determinar o centro referente as remessas /avisos de recebimento do
* documento de transporte de produto.
  REFRESH TI_T001W.
  SELECT * INTO TABLE TI_T001W FROM T001W FOR ALL ENTRIES IN TI_LIPS WHERE WERKS = TI_LIPS-WERKS.
  CHECK SY-SUBRC EQ 0.

*------------------------------------------------------------------------------
* Obter as ordens de vendas dos produtos.

  IF ST_VTTK-SHTYP = C_Z020.                          "Transferência
    READ TABLE TI_HEADER_P INTO WK_HEADER_P INDEX 1.
    CLEAR VG_PARID.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WK_HEADER_P-PARID
      IMPORTING
        OUTPUT = VG_PARID.
    PERFORM Z_CALCULAR_DATA_PREVISTA.
    CLEAR VG_WERKS.
    READ TABLE TI_ITEM_P INTO ST_ITEM_P INDEX 1.
    VG_WERKS = ST_ITEM_P-WERKS.
    PERFORM Z_OBTER_VALORES_CUSTO_FRETE.
  ELSE.
* Obter os dados da fatura (item) a partir da nota fiscal de serviço
    REFRESH TI_VBRP_P.
    SELECT *
      FROM VBRP INTO TABLE TI_VBRP_P
       FOR ALL ENTRIES IN TI_ITEM_P
     WHERE VBELN = TI_ITEM_P-REFKEY(10)
       AND POSNR = TI_ITEM_P-REFITM.
    CHECK SY-SUBRC EQ 0.
  ENDIF.

* Obter os dados da fatura (cabeçalho)
  READ TABLE TI_VBRP_P INTO ST_VBRP_P INDEX 1.

  CLEAR ST_VBRK_P.
  SELECT SINGLE * INTO ST_VBRK_P FROM VBRK WHERE VBELN = ST_VBRP_P-VBELN.
  CHECK SY-SUBRC EQ 0.

* Obter os dados da ordem referentes a venda do produto
  REFRESH TI_VBAK.
  SELECT * INTO TABLE TI_VBAK FROM VBAK FOR ALL ENTRIES IN TI_VBRP_P WHERE VBELN = TI_VBRP_P-AUBEL.
  CHECK SY-SUBRC EQ 0.

* Obter a chave de expedição das ordens
  REFRESH TI_VBAP_P.
  SELECT VBELN WERKS VSTEL INTO TABLE TI_VBAP_P FROM VBAP FOR ALL ENTRIES IN TI_VBAK WHERE VBELN = TI_VBAK-VBELN.
  SORT TI_VBAP_P.
  DELETE ADJACENT DUPLICATES FROM TI_VBAP_P.

*------------------------------------------------------------------------------
* Classificar a tabela de parceiros por número de documento e tipo de parceiro.
  SORT TI_PARTNER_P BY DOCNUM PARVW.

* Buscar os dados de controle do parceiro diretamente nas tabelas
* de dados mestres, isto porque em caso de recusa do sefaz por erro
* de cadastro na nota, apoós a atualização do cadastro a nova nota
* não está trazendo os dados atualizados.
  PERFORM Z_OBTER_DADOS_PARCEIROS_CTRL TABLES TI_PARTNER_P.

* Determinar a categoria de CFOP por filial .
  CLEAR ST_J_1BBRANCH.
  SELECT SINGLE BUKRS BRANCH INDUSTRY INTO ST_J_1BBRANCH FROM J_1BBRANCH WHERE BUKRS  = ST_TTDS-BUKRS AND BRANCH = ST_VTTK-TPLST.

* Determinar a condição de pagamento  fatura referente ao serviço.
  REFRESH TI_T052.
  SELECT ZTERM INTO TABLE TI_T052 FROM T052 WHERE ZTERM = ST_VBRK-ZTERM AND ZSTG1 <> SPACE.
  IF SY-SUBRC EQ 0.
    VG_CPGTO = C_1.
  ELSE.
    VG_CPGTO = C_0.
  ENDIF.

  IF ST_VTTK-SHTYP <> C_Z020.         "Transferência.
    PERFORM Z_OBTER_VALORES_CUSTO_FRETE.
    PERFORM Z_CALCULAR_DATA_PREVISTA.
  ENDIF.

* Obter os textos que devem ser impressos na CTE
  REFRESH TI_LINE.
* Textos da ordem ZTRO
  PERFORM Z_OBTER_TEXTOS USING ST_VBAK-VBELN.

ENDFORM.                    " Z_OBTER_DADOS_NF
*&---------------------------------------------------------------------*
*&      Form  Z_CONVERTER_CEP
*&--------------------------------------------------------------------*
* Tirar o hifen do cep e move-lo para um campo numérico.
*----------------------------------------------------------------------*
FORM Z_CONVERTER_CEP  USING    P_PSTLZ.

  REPLACE '-' WITH SPACE INTO P_PSTLZ.
  VG_PSTLZ = ST_PARTNER_P-PSTLZ.

ENDFORM.                    " Z_CONVERTER_CEP
*&---------------------------------------------------------------------*
*&      Form  Z_LER_CLIENTE
*&---------------------------------------------------------------------*
* Obter os dados do parceiro caso o mesmo não esteja na tabela de
* parceiro da nota fiscal (J1BNFNAD).
*----------------------------------------------------------------------*
FORM Z_LER_CLIENTE  USING    P_PARID.

  CLEAR ST_END.
  SELECT SINGLE K~ADRNR A~CITY2 A~POST_CODE1 A~STREET A~HOUSE_NUM1
         INTO ST_END
         FROM KNA1 AS K
        INNER JOIN ADRC AS A
         ON K~ADRNR = A~ADDRNUMBER
       WHERE KUNNR = P_PARID.

  CHECK SY-SUBRC = 0.
  IF ST_PARTNER_P-STRAS        IS INITIAL.
    ST_PARTNER_P-STRAS = ST_END-STREET.
  ENDIF.
  IF ST_PARTNER_P-STRAS+25(10) IS INITIAL.
    ST_PARTNER_P-STRAS+25(10) = ST_END-HOUSE_NUM1.
  ENDIF.
  IF ST_PARTNER_P-ORT02        IS INITIAL.
    ST_PARTNER_P-ORT02 = ST_END-CITY2.
  ENDIF.
  IF ST_PARTNER_P-PSTLZ        IS INITIAL.
    ST_PARTNER_P-PSTLZ = ST_END-POST_CODE1.
  ENDIF.

ENDFORM.                    " Z_LER_CLIENTE
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_DADOS_PARCEIROS_CTRL
*&---------------------------------------------------------------------*
* Obter os dados dos de controle dos parceiros diretamente nas
* tabelas de dados mestre.
*----------------------------------------------------------------------*
FORM Z_OBTER_DADOS_PARCEIROS_CTRL TABLES P_TI_PARTNER.

  DATA VL_TABIX TYPE SY-TABIX.

  CLEAR ST_PARTNER.
  LOOP AT P_TI_PARTNER INTO ST_PARTNER.

    CHECK: ( ST_PARTNER-CGC IS INITIAL   AND
             ST_PARTNER-CPF IS INITIAL ) OR
             ST_PARTNER-NAME1 IS INITIAL OR
             ST_PARTNER-STAINS IS INITIAL.
    VL_TABIX = SY-TABIX.
    CLEAR ST_KNA1.
    SELECT SINGLE NAME1 STCD1 STCD2 STCD3 INTO ST_KNA1
                                          FROM KNA1
                                         WHERE KUNNR = ST_PARTNER-PARID.
    IF ST_PARTNER-NAME1 IS INITIAL.
      ST_PARTNER-NAME1 = ST_KNA1-NAME1.
    ENDIF.
    IF ST_PARTNER-CGC IS INITIAL.
      ST_PARTNER-CGC = ST_KNA1-STCD1.
    ENDIF.
    IF ST_PARTNER-CPF IS INITIAL.
      ST_PARTNER-CPF = ST_KNA1-STCD2.
    ENDIF.
    IF ST_PARTNER-STAINS IS INITIAL.
      ST_PARTNER-STAINS = ST_KNA1-STCD3.
    ENDIF.

    MODIFY P_TI_PARTNER INDEX VL_TABIX FROM ST_PARTNER.

  ENDLOOP.

ENDFORM.                    " Z_OBTER_DADOS_PARCEIROS_CTRL
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_TEXTOS
*&---------------------------------------------------------------------*
* Obter textos da ordem de venda de serviço de transporte
*----------------------------------------------------------------------*
FORM Z_OBTER_TEXTOS  USING    P_VBELN.

  DATA: VL_NAME   TYPE THEAD-TDNAME,
        VL_LANGU  TYPE THEAD-TDSPRAS,
        VL_OBJETO TYPE THEAD-TDOBJECT.

  DATA: BEGIN OF TL_LINE OCCURS 0.
          INCLUDE STRUCTURE TLINE.
        DATA: END OF TL_LINE.

  VL_LANGU = SY-LANGU.

  DO 2 TIMES.

    IF SY-TABIX = 1.
* Só o número da ordem de venda (cabeçalho)
      VL_NAME = P_VBELN.
      VL_OBJETO = C_VBBK.
    ELSE.
* Ordem de venda e item (item)
      CONCATENATE P_VBELN C_000010 INTO VL_NAME.
      VL_OBJETO = C_VBBP.
    ENDIF.

    REFRESH TL_LINE.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        CLIENT                  = SY-MANDT
        ID                      = C_0001
        LANGUAGE                = VL_LANGU
        NAME                    = VL_NAME
        OBJECT                  = VL_OBJETO
      TABLES
        LINES                   = TL_LINE
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.

    IF SY-SUBRC = 0.
      APPEND LINES OF TL_LINE TO TI_LINE.
    ENDIF.

  ENDDO.

* Apagar linhas em branco
  DELETE TI_LINE WHERE TDLINE IS INITIAL.

ENDFORM.                    " Z_OBTER_TEXTOS
*&---------------------------------------------------------------------*
*&      Form  Z_LER_PLACA
*&---------------------------------------------------------------------*
* Obter o estado correspondente a placa do veículo.
*----------------------------------------------------------------------*
FORM Z_LER_PLACA  USING    P_PLACA  TYPE  VTTK-TEXT1
                  CHANGING P_UF     TYPE  ZLEST0002-CD_UF
                           P_NOME   TYPE  LFA1-NAME1
                           P_CNPJ.

  DATA: VL_NAME1    TYPE LFA1-NAME1,
        VL_CNPJ(21) TYPE C,
        VL_CPF      TYPE LFA1-STCD2.

  SELECT SINGLE * INTO ST_ZLEST0002
                  FROM ZLEST0002
                 WHERE PC_VEICULO = P_PLACA(7).

  CHECK SY-SUBRC EQ 0.
  P_UF = ST_ZLEST0002-CD_UF.

* Obter o nome, cnpj ou cpf do parceiro
  SELECT SINGLE NAME1 STCD1 STCD2 INTO (VL_NAME1, VL_CNPJ, VL_CPF)
                                  FROM LFA1
                                 WHERE LIFNR = ST_ZLEST0002-PROPRIETARIO.

  CHECK SY-SUBRC EQ 0.
  P_NOME = VL_NAME1.
  IF VL_CNPJ IS INITIAL.
    CONCATENATE C_CPF C_HIFEN VL_CPF INTO P_CNPJ.
  ELSE.
    CONCATENATE C_CNPJ C_HIFEN VL_CNPJ INTO P_CNPJ.
  ENDIF.

ENDFORM.                    " Z_LER_PLACA
*&---------------------------------------------------------------------*
*&      Form  CANCELARCTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XML_EXT2  text
*      -->P_XML_IN  text
*      -->P_CTE  text
*      -->P_VG_DT_HORA  text
*----------------------------------------------------------------------*
FORM CANCELARCTE  TABLES   XML_EXT2 TYPE J1B_NF_XML_EXTENSION2_TAB
                  USING    XML_IN   TYPE J1B_NF_XML_HEADER
                           CTE
                           VG_DT_HORA.

  DATA: JUSTIFICATIVA TYPE STRING.

  LOOP AT XML_EXT2.
    CONCATENATE JUSTIFICATIVA XML_EXT2-VALUE INTO JUSTIFICATIVA.
  ENDLOOP.

  PERFORM CANCABREXML2 USING CTE.
  PERFORM CTNAV USING CANCCTECNPJ     XML_IN-C_CNPJ CTE.
  PERFORM CTNAV USING CANCCTEIE       XML_IN-C1_IE  CTE.
  PERFORM CTNAV USING CANCCTENCT      XML_IN-NNF    CTE.
  PERFORM CTNAV USING CANCCTENSERIE   XML_IN-SERIE  CTE.
  PERFORM CTNAV USING CANCCTEXJUST    JUSTIFICATIVA CTE.
  PERFORM CTNAV USING CANCCTEDATAEMIS VG_DT_HORA  CTE.
  PERFORM CANCFECHAXML2 USING CTE.

  "<?xml version="1.0" encoding="UTF-8"?>
  "  <CancCTe>
  "     <CNPJ>11338257000174</CNPJ>
  "     <IE>133808777</IE>
  "     <nCT>2</nCT>
  "     <nSerie>0</nSerie>
  "     <xJust>CONHECIMENTO EMITIDO COM DADOS ADICIONAIS INCORRETOS</xJust>
  "     <dataEmis>2010-03-19 18:12:13</dataEmis>
  "  </CancCTe>

ENDFORM.                    " CANCELARNFE

*&---------------------------------------------------------------------*
*&      Form  Z_LIMPA_GLOBAIS                                          *
*&---------------------------------------------------------------------*
*                          Limpa Tabelas Globais                       *
*----------------------------------------------------------------------*
FORM Z_LIMPA_GLOBAIS.

  REFRESH: TI_PARTNER,
           TI_ITEM,
           TI_ITEM_TAX,
           TI_HEADER_MSG,
           TI_REFER_MSG,
           TI_HEADER_P,
           TI_PARTNER_P,
           TI_ITEM_P,
           TI_ITEM_TAX_P,
           TI_HEADER_MSG_P,
           TI_REFER_MSG_P,
           TI_VBRP,
           TI_VBRP_P,
           TI_VBAK,
           TI_VTPA,
           TI_VBAP,
           TI_VBAP_P,
           TI_VTTP,
           TI_LIPS,
           TI_VBFAP,
           TI_LIN,
           TI_EKBE,
           TI_RBKP,
           TI_REFLIN,
           TI_T001W,
           TI_VFKP,
           TI_KONV,
           TI_T052,
           TI_J_1BTREG_CITY.

  CLEAR: WK_HEADER,
         WK_HEADER_P,
         ST_PARTNER,
         ST_PARTNER_P,
         ST_ITEM_P,
         ST_VBRP,
         ST_VBRP_P,
         ST_VBRK,
         ST_VBRK_P,
         ST_VBAK,
         ST_VTPA,
         ST_VBAP,
         ST_VTTK,
         ST_T173T,
         ST_TDLNR,
         ST_TTDS,
         ST_T001W,
         ST_J_1BBRANCH,
         ST_RBKP,
         ST_REFLIN,
         ST_LIN,
         ST_TVRO,
         ST_VFKP,
         ST_KONV,
         ST_ZLEST0002,
         ST_J_1BTREG_CITY,
         ST_J_1BNFE_ACTIVE,
         ST_END,
         ST_KNA1,
         ST_LFA1,
         ST_DOCTOS,
         ST_LINE,
         VG_CPGTO,
         VG_CONSIGNATARIO,
         VG_DTA_ENT,
         VG_DT_HORA,
         VG_PSTLZ(8),
         VG_PARID_AG,
         VG_PARID_RG,
         VG_XML_VERSAO,
         VG_VERSAO_NFE,
         VG_VERSAO_CTE.

ENDFORM.                    " Z_LIMPA_GLOBAIS
*&---------------------------------------------------------------------*
*&      Form  Z_CALCULAR_DATA_PREVISTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_CALCULAR_DATA_PREVISTA .

* Data prevista de entrega.
  CLEAR ST_TVRO.
  SELECT SINGLE * INTO ST_TVRO
                  FROM TVRO
                 WHERE ROUTE = ST_VTTK-ROUTE.

* Calcular a data de entrega
  DATA: VL_QUOCIENTE  TYPE I,
        VL_DIAS(10)   TYPE C,
        VL_DIAS_N(10) TYPE N.
  CALL FUNCTION 'CONVERSION_EXIT_TSTRG_OUTPUT'
    EXPORTING
      INPUT  = ST_TVRO-TRAZTD
    IMPORTING
      OUTPUT = VL_DIAS.
  VL_DIAS_N = VL_DIAS.
  VL_DIAS_N = VL_DIAS_N / 100.
  VL_QUOCIENTE = VL_DIAS_N DIV 24.
  VG_DTA_ENT = ST_VTTK-DATBG + VL_QUOCIENTE.

ENDFORM.                    " Z_CALCULAR_DATA_PREVISTA
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_VALORES_CUSTO_FRETE
*&---------------------------------------------------------------------*
*  Obter valores do custo do frete
*----------------------------------------------------------------------*
FORM Z_OBTER_VALORES_CUSTO_FRETE .

* Valor do frete referente ao documento de transporte de produto.
  DATA: VG_KINAK LIKE KONV-KINAK.

  REFRESH TI_VFKP.
  SELECT * INTO TABLE TI_VFKP
           FROM VFKP
          WHERE REBEL = ST_VTTK-TKNUM
            AND REFTY = C_8
            AND FKPTY = C_Z001.

* Ttipos de condição de frete
  IF TI_VFKP[] IS NOT INITIAL.
    REFRESH TI_KONV.

* ---> S4 Migration - 07/07/2023 - JP
*    SELECT * INTO TABLE TI_KONV
*             FROM KONV
*          FOR ALL ENTRIES IN TI_VFKP
*            WHERE KNUMV = TI_VFKP-KNUMV
*              AND KAPPL = C_F
*              AND KSCHL IN (C_ZICM,C_ZICC,C_ZIPT)
*              "AND kinak <> c_x
*              AND KINAK EQ VG_KINAK.

    SELECT * INTO TABLE @data(TI_KONV_aux)
             FROM v_KONV
          FOR ALL ENTRIES IN @TI_VFKP
            WHERE KNUMV = @TI_VFKP-KNUMV
              AND KAPPL = @C_F
              AND KSCHL IN (@C_ZICM,@C_ZICC,@C_ZIPT)
              "AND kinak <> c_x
              AND KINAK EQ @VG_KINAK.

    move-corresponding TI_KONV_aux[] to TI_KONV[].

* <--- S4 Migration - 07/07/2023 - JP

    SORT TI_KONV BY KSCHL.
  ENDIF.

ENDFORM.                    " Z_OBTER_VALORES_CUSTO_FRETE

*&---------------------------------------------------------------------*
*&      Form  CTEVALEPED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CTEVALEPED  USING    P_CTE.

  DATA: IT_VFKP  TYPE TABLE OF VFKP INITIAL SIZE 0 WITH HEADER LINE,
        IT_KONV  TYPE TABLE OF KONV INITIAL SIZE 0 WITH HEADER LINE,
        WA_KONV  TYPE KONV,
        VG_KINAK TYPE KINAK,
        VG_KWERT TYPE KWERT.

  CHECK VG_VERSAO_CTE < 3. "CS2017002143 CT-e 3.0

  VG_KWERT = 0.

  IF XML_CTE_1_04 IS INITIAL.

    SELECT *
      FROM VFKP
      INTO CORRESPONDING FIELDS OF TABLE IT_VFKP
     WHERE REFTY EQ C_8
       AND REBEL EQ ST_VTTK-TKNUM.

* ---> S4 Migration - 07/07/2023 - JP
*  SELECT *
*    FROM KONV
*    INTO CORRESPONDING FIELDS OF TABLE IT_KONV
*    FOR ALL ENTRIES IN IT_VFKP
*   WHERE KNUMV EQ IT_VFKP-KNUMV
*     AND KSCHL EQ C_ZPED
*     AND KINAK EQ VG_KINAK
*     AND KWERT GT 0.

  SELECT *
    FROM V_KONV
    INTO table @data(IT_KONV_AUX)
    FOR ALL ENTRIES IN @IT_VFKP
   WHERE KNUMV EQ @IT_VFKP-KNUMV
     AND KSCHL EQ @C_ZPED
     AND KINAK EQ @VG_KINAK
     AND KWERT GT 0.

   move-corresponding IT_KONV_AUX[] to IT_KONV[].
   READ TABLE it_KONV TRANSPORTING NO FIELDS index 1.
* <--- S4 Migration - 07/07/2023 - JP

    IF SY-SUBRC EQ 0.
      LOOP AT IT_KONV INTO WA_KONV.
        IF WA_KONV-KWERT GT 0.
          VG_KWERT = VG_KWERT + WA_KONV-KWERT.
        ENDIF.
      ENDLOOP.
      IF VG_KWERT GT 0.
        PERFORM CTNAB  USING CTEVALEPED   P_CTE.
        PERFORM CTNAVN USING CTEVTVALEPED VG_KWERT     P_CTE.
        PERFORM CTNAV  USING CTERESPPG    C_TOMA_PED5  P_CTE.
        PERFORM CTNFE  USING CTEVALEPED   P_CTE.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " CTEVALEPED

*&---------------------------------------------------------------------*
*&      Form  E_MAIL_DESTINATARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM E_MAIL_DESTINATARIO  USING  XML
                                 TB TYPE J1B_NF_XML_HEADER
                                 J1 TYPE J_1BNFDOC.

  DATA: E_MAIL_PARC  TYPE AD_SMTPADR,
        E_MAIL       TYPE AD_SMTPADR,
        IT_MAIL      TYPE TABLE OF AD_SMTPADR,
        IT_MAIL_RM   TYPE TABLE OF ADRT-REMARK WITH HEADER LINE,
        E_TEXTO      TYPE STRING,
        E_TEXTO_SI   TYPE STRING,
        E_AUX        TYPE STRING,
        VG_ADRNR     TYPE ADRNR,
        VG_BUKRS     TYPE BUKRS,
        VG_BRANCH    TYPE J_1BBRANC_,
        IT_J_1BNFNAD TYPE TABLE OF J_1BNFNAD WITH HEADER LINE.

  RANGES RPARVW FOR J_1BNFNAD-PARVW.

  CLEAR: E_MAIL, E_TEXTO, E_TEXTO_SI, E_AUX , E_MAIL_PARC.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "E-mail do Parceiro da Nota Fiscal """""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  CASE J1-PARTYP.
    WHEN 'V'.
      SELECT SINGLE ADRNR INTO VG_ADRNR FROM LFA1
       WHERE LIFNR EQ J1-PARID.
    WHEN 'C'.
      SELECT SINGLE ADRNR INTO VG_ADRNR FROM KNA1
       WHERE KUNNR EQ J1-PARID.
    WHEN 'B'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = J1-PARID
        IMPORTING
          OUTPUT = J1-PARID.

      VG_BUKRS  = J1-PARID+2(4).
      VG_BRANCH = J1-PARID+6(4).

      SELECT SINGLE ADRNR INTO VG_ADRNR FROM J_1BBRANCH
       WHERE BUKRS  EQ VG_BUKRS
         AND BRANCH EQ VG_BRANCH.
  ENDCASE.

  IF NOT VG_ADRNR IS INITIAL.
    SELECT SINGLE SMTP_ADDR INTO E_MAIL_PARC
      FROM ADR6
     WHERE ADDRNUMBER EQ VG_ADRNR.

    IF ( SY-SUBRC IS INITIAL ) AND ( E_MAIL_PARC IS NOT INITIAL ).
      E_TEXTO = E_MAIL_PARC.
    ENDIF.
  ENDIF.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "E-mail do Parceiro Terminal/Local de Entrega """"""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  RPARVW-SIGN   = 'I'.
  RPARVW-OPTION = 'EQ'.
  RPARVW-LOW    = 'Z1'. "Terminal
  RPARVW-HIGH   = 'Z1'. "Terminal
  APPEND RPARVW.
  RPARVW-LOW    = 'LR'. "Local de Entrega
  RPARVW-HIGH   = 'LR'. "Local de Entrega
  APPEND RPARVW.

  SELECT * INTO TABLE IT_J_1BNFNAD
    FROM J_1BNFNAD
   WHERE DOCNUM EQ J1-DOCNUM
     AND PARVW  IN RPARVW.

  LOOP AT IT_J_1BNFNAD.

    CASE IT_J_1BNFNAD-PARTYP.
      WHEN 'C'.
        "Endereço Clente
        SELECT SINGLE ADRNR INTO VG_ADRNR FROM KNA1
         WHERE KUNNR EQ IT_J_1BNFNAD-PARID.
      WHEN 'V'.
        "Endereço Forncedor
        SELECT SINGLE ADRNR INTO VG_ADRNR FROM LFA1
         WHERE LIFNR EQ IT_J_1BNFNAD-PARID.
    ENDCASE.

    IF ( SY-SUBRC IS INITIAL ) AND ( NOT VG_ADRNR IS INITIAL ).
      CLEAR: E_MAIL.
      REFRESH: IT_MAIL, IT_MAIL_RM.

      SELECT SMTP_ADDR INTO TABLE IT_MAIL
        FROM ADR6
       WHERE ADDRNUMBER EQ VG_ADRNR
         AND SMTP_ADDR  NE ''.

      SELECT REMARK INTO TABLE IT_MAIL_RM
        FROM ADRT
       WHERE ADDRNUMBER EQ VG_ADRNR
         AND REMARK     NE ''
         AND COMM_TYPE  EQ 'INT'.

      LOOP AT IT_MAIL_RM.
        E_MAIL = IT_MAIL_RM.
        APPEND E_MAIL TO IT_MAIL.
      ENDLOOP.

      LOOP AT IT_MAIL INTO E_MAIL.

        "IF ( SY-SUBRC IS INITIAL ) AND ( E_MAIL IS NOT INITIAL ) AND ( E_MAIL_PARC NE E_MAIL ).
        IF ( E_MAIL_PARC NE E_MAIL ).

          IF E_TEXTO IS INITIAL.
            E_TEXTO = E_MAIL.
          ELSE.
            CLEAR: E_AUX.
            CONCATENATE E_TEXTO ';' E_MAIL INTO E_AUX.

            IF STRLEN( E_AUX ) >= 200. "Verifica se excedeu limite da Tag. Caso exceda, adiciona os emails da tag Email do Simetrya.

              CLEAR: E_AUX.
              CONCATENATE E_TEXTO_SI ';' E_MAIL INTO E_AUX.

              IF STRLEN( E_AUX ) <= 60. "Verifica se excedeu limite da Tag. Caso exceda, adiciona os emails da tag Email do Simetrya.

                IF E_TEXTO_SI IS INITIAL.
                  E_TEXTO_SI = E_MAIL.
                ELSE.
                  CONCATENATE E_TEXTO_SI ';' E_MAIL INTO E_TEXTO_SI.
                ENDIF.

              ENDIF.

            ELSE.
              CONCATENATE E_TEXTO ';' E_MAIL INTO E_TEXTO.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDLOOP.

  IF ( NOT E_TEXTO IS INITIAL ).
    "    E_TEXTO = E_TEXTO(150).
    PERFORM CTNAF USING DESTEMAILXML E_TEXTO XML.
    PERFORM CTNAF USING DESTEMAILDANFE E_TEXTO XML.
  ENDIF.

  IF ( NOT E_TEXTO_SI IS INITIAL ).
    PERFORM CTNAF USING DESTEMAILXMLSI E_TEXTO_SI XML.
  ENDIF.

ENDFORM.                    " E_MAIL_DESTINATARIO

*&---------------------------------------------------------------------*
*&      Form  CTESEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CTESEG  USING    P_CTE P_OBS.

  DATA: WA_PARID TYPE Y_VTPA,
        IT_VFKP  TYPE TABLE OF VFKP INITIAL SIZE 0 WITH HEADER LINE,
        IT_KONV  TYPE TABLE OF KONV INITIAL SIZE 0 WITH HEADER LINE,
        WA_KONV  TYPE KONV,
        VG_KINAK TYPE KINAK,
        VG_KWERT TYPE KWERT.

  CHECK VG_VERSAO_CTE < 3. "CS2017002143 CT-e 3.0

  VG_KWERT = 0.

  SELECT *
    FROM VFKP
    INTO CORRESPONDING FIELDS OF TABLE IT_VFKP
   WHERE REFTY EQ C_8
     AND REBEL EQ ST_VTTK-TKNUM.

* ---> S4 Migration - 07/07/2023 - JP
*  SELECT *
*    FROM KONV
*    INTO CORRESPONDING FIELDS OF TABLE IT_KONV
*    FOR ALL ENTRIES IN IT_VFKP
*   WHERE KNUMV EQ IT_VFKP-KNUMV
*     AND KSCHL EQ C_ZSEG
*     AND KINAK EQ VG_KINAK
*     AND KWERT GT 0.

  SELECT *
    FROM V_KONV
    INTO table @data(IT_KONV_AUX)
    FOR ALL ENTRIES IN @IT_VFKP
   WHERE KNUMV EQ @IT_VFKP-KNUMV
     AND KSCHL EQ @C_ZSEG
     AND KINAK EQ @VG_KINAK
     AND KWERT GT 0.

   move-corresponding IT_KONV_AUX[] to IT_KONV[].

* <--- S4 Migration - 07/07/2023 - JP

  IF P_OBS IS INITIAL.
    PERFORM CTNAB USING CTESEG     P_CTE.
    PERFORM CTNAV USING CTERESPSEG C_4 P_CTE.
  ENDIF.

  IF SY-SUBRC EQ 0.
    LOOP AT IT_KONV INTO WA_KONV.
      IF WA_KONV-KWERT GT 0.
        VG_KWERT = VG_KWERT + WA_KONV-KWERT.
      ENDIF.
    ENDLOOP.
    IF VG_KWERT GT 0.
      READ TABLE TI_VTPA INTO WA_PARID WITH KEY PARVW = C_SG.
      IF SY-SUBRC EQ 0.
        IF P_OBS IS INITIAL.
          PERFORM CTNAV USING CTEXSEG WA_PARID-NAME1(30) P_CTE.
        ELSE.
          CONCATENATE 'Seguradora:' WA_PARID-NAME1 INTO VG_LIMPO SEPARATED BY SPACE.
          PERFORM CTNAO USING VG_LIMPO  P_CTE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF P_OBS IS INITIAL.
    PERFORM CTNFE USING CTESEG P_CTE.
  ENDIF.

ENDFORM.                    " CTESEG

*&---------------------------------------------------------------------*
*&      Form  NOTAS_PRODUTOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM NOTAS_PRODUTOR  USING    P_NFE
                              VBELN TYPE VBELN_VF.

  DATA: IT_PROD         TYPE TABLE OF ZDOC_NF_PRODUTOR INITIAL SIZE 0 WITH HEADER LINE,
        IT_NFDOC        TYPE TABLE OF J_1BNFDOC        INITIAL SIZE 0 WITH HEADER LINE,
        IT_ZSDT_RETLOTE TYPE TABLE OF ZSDT_RETLOTE,
        GW_ZSDT_RETLOTE TYPE ZSDT_RETLOTE,
        WA_ACTIVE       TYPE J_1BNFE_ACTIVE,
        WA_NFDOC        TYPE J_1BNFDOC,
        WA_CHAVE        TYPE CHAR44,
        WA_AAMM         TYPE C LENGTH 4,
        WA_VBRP         TYPE VBRP,
        WA_INFO_PART    TYPE LFA1,
        WA_INFO_C       TYPE KNA1.

  CHECK 1 = 2.

  CHECK NOT VBELN IS INITIAL.

  SELECT SINGLE *
    INTO WA_VBRP
    FROM VBRP
   WHERE VBELN EQ VBELN.

  CHECK SY-SUBRC IS INITIAL.

  SELECT *
     INTO TABLE IT_PROD
     FROM ZDOC_NF_PRODUTOR
    WHERE VBELN EQ WA_VBRP-VGBEL.

  CHECK SY-SUBRC IS INITIAL.

  SELECT *
    INTO TABLE IT_NFDOC
    FROM J_1BNFDOC
     FOR ALL ENTRIES IN IT_PROD
   WHERE DOCNUM EQ IT_PROD-DOCNUM_PROD.

  CHECK SY-SUBRC IS INITIAL.

  LOOP AT IT_NFDOC INTO WA_NFDOC.
    PERFORM ADD_REF_NFE_NF USING P_NFE WA_NFDOC-DOCNUM.
  ENDLOOP.

ENDFORM.                    " NOTAS_PRODUTOR

FORM ADD_REF_NFE_NF USING P_NFE
                          P_DOCNUM.

  DATA: WA_ACTIVE    TYPE J_1BNFE_ACTIVE,
        WA_NFDOC     TYPE J_1BNFDOC,
        WA_CHAVE     TYPE CHAR44,
        WA_AAMM      TYPE C LENGTH 4,
        WA_INFO_PART TYPE LFA1,
        WA_INFO_C    TYPE KNA1.

  SELECT SINGLE *
    FROM J_1BNFDOC INTO WA_NFDOC
   WHERE DOCNUM EQ P_DOCNUM.

  CHECK SY-SUBRC EQ 0.

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(_WL_SETLEAF)
   WHERE SETNAME = 'XML_NFE_REF_NF_NFE'.

  CHECK ( SY-SUBRC EQ 0 ) AND ( _WL_SETLEAF-VALFROM IS NOT INITIAL ).

  CONCATENATE WA_NFDOC-DOCDAT+2(2) WA_NFDOC-DOCDAT+4(2) INTO WA_AAMM.

  IF WA_NFDOC-NFE IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_NFDOC-NFNUM
      IMPORTING
        OUTPUT = WA_NFDOC-NFENUM.
  ENDIF.

  IF WA_NFDOC-NFE IS INITIAL.

    IF ( WA_NFDOC-REGIO IS INITIAL ) OR ( WA_NFDOC-STKZN IS INITIAL AND WA_NFDOC-CGC IS INITIAL ) OR ( NOT WA_NFDOC-STKZN IS INITIAL AND WA_NFDOC-CPF IS INITIAL ) OR
       ( WA_NFDOC-STAINS IS INITIAL ).

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          P_PARCEIRO   = WA_NFDOC-PARID
          P_PARTYPE    = WA_NFDOC-PARTYP
        CHANGING
          WA_INFO_PART = WA_INFO_PART
          WA_INFO_C    = WA_INFO_C.

      PERFORM CTNAB USING NFREF  P_NFE.
      PERFORM CTNAB USING REFNFP P_NFE.

      IF WA_NFDOC-PARTYP EQ 'V'.
        PERFORM CTNAF USING REFNFPSIGLAUF  WA_INFO_PART-REGIO  P_NFE.
        IF WA_INFO_PART-STKZN IS INITIAL.
          PERFORM CTNAF USING REFNFPCNPJ   WA_INFO_PART-STCD1  P_NFE.
        ELSE.
          PERFORM CTNAF USING REFNFPCPF    WA_INFO_PART-STCD2  P_NFE.
        ENDIF.
        PERFORM CTNAF USING REFNFPIE       WA_INFO_PART-STCD3  P_NFE.
      ELSEIF WA_NFDOC-PARTYP EQ 'C'.
        PERFORM CTNAF USING REFNFPSIGLAUF  WA_INFO_C-REGIO  P_NFE.
        IF WA_INFO_C-STKZN IS INITIAL.
          PERFORM CTNAF USING REFNFPCNPJ   WA_INFO_C-STCD1  P_NFE.
        ELSE.
          PERFORM CTNAF USING REFNFPCPF    WA_INFO_C-STCD2  P_NFE.
        ENDIF.
        PERFORM CTNAF USING REFNFPIE       WA_INFO_C-STCD3  P_NFE.
      ENDIF.
      PERFORM CTNAF USING REFNFPAAMM     WA_AAMM  P_NFE.
      PERFORM CTNAF USING REFNFPMOD      WA_NFDOC-MODEL  P_NFE.
      PERFORM CTNAF USING REFNFPSERIE    WA_NFDOC-SERIES P_NFE.
      PERFORM CTNAF USING REFNFPNNF      WA_NFDOC-NFENUM P_NFE.
      PERFORM CTNFE USING REFNFP P_NFE.
      PERFORM CTNFE USING NFREF P_NFE.
    ELSE.
      PERFORM CTNAB USING NFREF          P_NFE.
      PERFORM CTNAB USING REFNFP         P_NFE.
      PERFORM CTNAF USING REFNFPSIGLAUF  WA_NFDOC-REGIO  P_NFE.
      PERFORM CTNAF USING REFNFPAAMM     WA_AAMM  P_NFE.
      IF WA_NFDOC-STKZN IS INITIAL.
        PERFORM CTNAF USING REFNFPCNPJ   WA_NFDOC-CGC  P_NFE.
      ELSE.
        PERFORM CTNAF USING REFNFPCPF    WA_NFDOC-CPF  P_NFE.
      ENDIF.
      PERFORM CTNAF USING REFNFPIE       WA_NFDOC-STAINS  P_NFE.
      PERFORM CTNAF USING REFNFPMOD      WA_NFDOC-MODEL  P_NFE.
      PERFORM CTNAF USING REFNFPSERIE    WA_NFDOC-SERIES P_NFE.
      PERFORM CTNAF USING REFNFPNNF      WA_NFDOC-NFENUM P_NFE.
      PERFORM CTNFE USING REFNFP         P_NFE.
      PERFORM CTNFE USING NFREF          P_NFE.
    ENDIF.
  ELSE.
    SELECT SINGLE * INTO WA_ACTIVE
      FROM J_1BNFE_ACTIVE
     WHERE DOCNUM EQ WA_NFDOC-DOCNUM.

    IF SY-SUBRC IS INITIAL.
      CALL METHOD ZCL_UTIL=>MONTA_CHAVE_NFE
        EXPORTING
          I_DOCNUM = WA_NFDOC-DOCNUM
          I_VALIDA = 'X'
        RECEIVING
          E_CHAVE  = WA_CHAVE.

      PERFORM CTNAB USING NFREF  P_NFE.
      PERFORM CTNAF USING REFNFE WA_CHAVE P_NFE.
      PERFORM CTNFE USING NFREF  P_NFE.
    ENDIF.
  ENDIF.

ENDFORM.

FORM ADD_REF_NFE_NF_CCT USING P_NFE
                              P_ZLEST0147 TYPE ZLEST0147.

  DATA: WA_ACTIVE TYPE J_1BNFE_ACTIVE,
        WA_NFDOC  TYPE J_1BNFDOC,
        WA_CHAVE  TYPE CHAR44,
        WA_AAMM   TYPE C LENGTH 4,
        WA_INFO_V TYPE LFA1,
        WA_INFO_C TYPE KNA1.

  SELECT SINGLE *
    FROM J_1BNFDOC INTO WA_NFDOC
   WHERE DOCNUM EQ P_ZLEST0147-DOCNUM.

  IF SY-SUBRC NE 0.
    MESSAGE E899(FI) WITH 'Documento' P_ZLEST0147-DOCNUM 'não encontrado.' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  CONCATENATE P_ZLEST0147-DT_EMISSAO+2(2) P_ZLEST0147-DT_EMISSAO+4(2) INTO WA_AAMM.

  IF P_ZLEST0147-MODEL = '55'.

    PERFORM CTNAB USING NFREF  P_NFE.
    PERFORM CTNAF USING REFNFE P_ZLEST0147-CHAVE_NFE P_NFE.
    PERFORM CTNFE USING NFREF  P_NFE.

*    SELECT SINGLE * INTO WA_ACTIVE
*      FROM J_1BNFE_ACTIVE
*     WHERE DOCNUM EQ WA_NFDOC-DOCNUM.
*
*    IF SY-SUBRC IS INITIAL.
*      CALL METHOD ZCL_UTIL=>MONTA_CHAVE_NFE
*        EXPORTING
*          I_DOCNUM = WA_NFDOC-DOCNUM
*          I_VALIDA = 'X'
*        RECEIVING
*          E_CHAVE  = WA_CHAVE.
*
*      PERFORM CTNAB USING NFREF  P_NFE.
*      PERFORM CTNAF USING REFNFE WA_CHAVE P_NFE.
*      PERFORM CTNFE USING NFREF  P_NFE.
*    ENDIF.

  ELSE.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        P_PARCEIRO   = WA_NFDOC-PARID
        P_PARTYPE    = WA_NFDOC-PARTYP
      CHANGING
        WA_INFO_PART = WA_INFO_V
        WA_INFO_C    = WA_INFO_C.

    PERFORM CTNAB USING NFREF  P_NFE.
    PERFORM CTNAB USING REFNFP P_NFE.

    IF WA_NFDOC-PARTYP EQ 'V'.
      PERFORM CTNAF USING REFNFPSIGLAUF  WA_INFO_V-REGIO  P_NFE.
      IF WA_INFO_V-STKZN IS INITIAL.
        PERFORM CTNAF USING REFNFPCNPJ   WA_INFO_V-STCD1  P_NFE.
      ELSE.
        PERFORM CTNAF USING REFNFPCPF    WA_INFO_V-STCD2  P_NFE.
      ENDIF.
      PERFORM CTNAF USING REFNFPIE       WA_INFO_V-STCD3  P_NFE.
    ELSEIF WA_NFDOC-PARTYP EQ 'C'.
      PERFORM CTNAF USING REFNFPSIGLAUF  WA_INFO_C-REGIO  P_NFE.
      IF WA_INFO_C-STKZN IS INITIAL.
        PERFORM CTNAF USING REFNFPCNPJ   WA_INFO_C-STCD1  P_NFE.
      ELSE.
        PERFORM CTNAF USING REFNFPCPF    WA_INFO_C-STCD2  P_NFE.
      ENDIF.
      PERFORM CTNAF USING REFNFPIE       WA_INFO_C-STCD3  P_NFE.
    ENDIF.
    PERFORM CTNAF USING REFNFPAAMM     WA_AAMM  P_NFE.
    PERFORM CTNAF USING REFNFPMOD      P_ZLEST0147-MODEL P_NFE.
    PERFORM CTNAF USING REFNFPSERIE    P_ZLEST0147-SERIE P_NFE.
    PERFORM CTNAF USING REFNFPNNF      P_ZLEST0147-NFNUM P_NFE.
    PERFORM CTNFE USING REFNFP P_NFE.
    PERFORM CTNFE USING NFREF P_NFE.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CIDADE_EMISSAO_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CIDADE_EMISSAO_CTE  USING P_DOCNUM TYPE J_1BNFDOC
                               XML.

  DATA: TAXJURCODE TYPE  J_1BTXJCD.

  CALL FUNCTION 'Z_SD_INFO_CIDADE_EMITE'
    EXPORTING
      WA_J_1BNFDOC = P_DOCNUM
    IMPORTING
      TAXJURCODE   = TAXJURCODE.

  PERFORM CTNAV USING CTECMUNENV TAXJURCODE+3(7) XML.

ENDFORM.                    " CIDADE_EMISSAO_CTE


*&---------------------------------------------------------------------*
*&      Form  get_ambiente
*&---------------------------------------------------------------------*
FORM GET_AMBIENTE CHANGING VL_AMBIENTE.
  DATA: IP_ADDRESS      TYPE C LENGTH 69,
        WA_ZAMB_HOMOLOG TYPE ZAMB_HOMOLOG.

  CALL FUNCTION 'ZGET_IP_ADDRESS'
    IMPORTING
      ZIP_ADDRESS = IP_ADDRESS.

  SELECT SINGLE *
    INTO WA_ZAMB_HOMOLOG
    FROM ZAMB_HOMOLOG
   WHERE IP_SERVIDOR = IP_ADDRESS.

  VL_AMBIENTE = WA_ZAMB_HOMOLOG-AMBIENTE.


ENDFORM.                    "get_ambiente
*&---------------------------------------------------------------------*
*&      Form  VLRTOTTRIB
*&---------------------------------------------------------------------*
FORM VLRTOTTRIB  USING WA_ITNS TYPE J1B_NF_XML_ITEM  P_NFE.


  "  DATA: VAR_DESON      TYPE J_1BTAXVAL.
  CLEAR: VAR_TOTAL_TRIB.
  VAR_TOTAL_TRIB = WA_ITNS-L1_00_VICMS + WA_ITNS-L1_10_VICMS + WA_ITNS-L1_20_VICMS + WA_ITNS-L1_30_VICMSST + WA_ITNS-L1_51_VICMS +
                   WA_ITNS-L1_60_VICMSST + WA_ITNS-L1_70_VICMS + WA_ITNS-L1_90_VICMS + WA_ITNS-O_VII + WA_ITNS-P1_VPIS +
                   WA_ITNS-P2_VPIS + WA_ITNS-P4_VPIS + WA_ITNS-P5_VPIS + WA_ITNS-Q1_VCOFINS + WA_ITNS-Q2_VCOFINS + WA_ITNS-Q4_VCOFINS + WA_ITNS-Q5_VCOFINS.

  "VAR_DESON = WA_ITNS-L1_00_VICMS.

  ADD VAR_TOTAL_TRIB TO VAR_TOTAL_TRIB2.
  "ADD VAR_TOTAL_TRIB TO VAR_TOTAL_DESON.

  PERFORM CTNAFV USING VTOTTRIB  VAR_TOTAL_TRIB P_NFE.
ENDFORM.                    " VLRTOTTRIB

*---------------------------------------------------------------------*
*      Form  AUTORIZACAO_DOWN_XML
*---------------------------------------------------------------------*
FORM AUTORIZACAO_DOWN_XML USING VALUE(P_CNPJ_CPJ) P_NFE.

  "-- Tabelas Internas e Work Area
  DATA: IT_KNA1     TYPE TABLE OF KNA1,
        WA_KNA1     TYPE KNA1,
        IT_ZSDT0103 TYPE TABLE OF ZSDT0103,
        WA_ZSDT0103 TYPE ZSDT0103.

  "--Variables
  DATA : COUNT    TYPE I,
         MSG_TEXT TYPE C.

  "Abre Tag Autorização Xml

  SELECT *
    FROM KNA1 INTO TABLE IT_KNA1
   WHERE ( STCD1 = P_CNPJ_CPJ OR STCD2 = P_CNPJ_CPJ ).

  SELECT *
    FROM ZSDT0103 INTO TABLE IT_ZSDT0103
  FOR ALL ENTRIES IN IT_KNA1
  WHERE WERKS = IT_KNA1-KUNNR.

  IF ( SY-SUBRC EQ 0 ).

    COUNT = LINES( IT_ZSDT0103 ).

    MSG_TEXT = 'Não foi possível adicionar a Autorização de Download no Arquivo xml.'.
    CONCATENATE MSG_TEXT  SPACE 'Número de Filiais excedido(Máx. 10) para o CNPJ/CPF' INTO MSG_TEXT.

    IF COUNT > 10.
      MESSAGE E899(FI) WITH MSG_TEXT.
    ENDIF.


    PERFORM CTNAB USING AUTXML P_NFE.

    LOOP AT IT_ZSDT0103 INTO WA_ZSDT0103.

      IF STRLEN( WA_ZSDT0103-STCD1 ) = '11'. "CPF
        PERFORM CTNAV USING REFNFPCPF WA_ZSDT0103-STCD1 P_NFE.
      ELSE. "CNPJ
        PERFORM CTNAV USING CNPJ WA_ZSDT0103-STCD1 P_NFE.
      ENDIF.

    ENDLOOP.

    PERFORM CTNFE USING AUTXML P_NFE. " Fecha Tag Autorização XML"

  ENDIF.

ENDFORM.                    " AUTORIZACAO_DOWN_XML

FORM DADOS_DI_COMPLEMENTO  USING    P_DOCNUM
                                    P_ITMNUM
                                    P_NDI
                                    P_NFE.

  SELECT SINGLE * INTO WA_ZNOTA_IMPORT
     FROM ZNOTA_IMPORT
    WHERE DOCNUM = P_DOCNUM
      AND ITMNUM = P_ITMNUM
      AND NDI    = P_NDI.

  IF SY-SUBRC = 0.

    IF ( NOT WA_ZNOTA_IMPORT-TPVIATRANSP IS INITIAL ).
      PERFORM CTNAF  USING DITPVIATRANSP WA_ZNOTA_IMPORT-TPVIATRANSP  P_NFE.
    ENDIF.

    IF ( NOT WA_ZNOTA_IMPORT-VAFRMM IS INITIAL ).
      PERFORM CTNAFV  USING DIVAFRMM WA_ZNOTA_IMPORT-VAFRMM  P_NFE.
    ENDIF.

    IF ( NOT WA_ZNOTA_IMPORT-TPINTERMEDIO IS INITIAL ).
      PERFORM CTNAF  USING DITPINTERMEDIO WA_ZNOTA_IMPORT-TPINTERMEDIO  P_NFE.
    ENDIF.

  ENDIF.


ENDFORM.                    " DADOS_DI_COMPLEMENTO
*&---------------------------------------------------------------------*
*&      Form  DADOS_COMP
*&---------------------------------------------------------------------*

FORM DADOS_COMP  USING    P_REFKEY TYPE J_1BREFKEY
                          P_NFE
                          P_XML_DET_EXP01 TYPE ZXML
                          P_XML_DET_EXP02 TYPE ZXML
                          P_XML_DET_EXP03 TYPE ZXML.

  "Internal Table and WorkArea
  DATA: LT_VBRK             TYPE TABLE OF VBRK,
        LW_VBRK             TYPE VBRK,
        LT_VBFA             TYPE TABLE OF VBFA,
        LW_VBFA             TYPE VBFA,
        LT_ZDOC_EXP         TYPE TABLE OF ZDOC_EXP,
        LW_ZDOC_EXP         TYPE ZDOC_EXP,
        LT_ZDOC_NF_PRODUTOR TYPE TABLE OF ZDOC_NF_PRODUTOR,
        LW_ZDOC_NF_PRODUTOR TYPE ZDOC_NF_PRODUTOR.


  "Objeto
  DATA: LOBJ_UTIL TYPE REF TO ZCL_UTIL.

  "Variables
  DATA: LW_CHAVE  TYPE C LENGTH 44.
  DATA: VL_LEN_XML TYPE I.
  DATA: VL_XML_DET TYPE C.

  "===========================================================================================
  " Notas de Complemento para Fim Especifico.
  "===========================================================================================
  SELECT *
    FROM VBRK
    INTO TABLE LT_VBRK
   WHERE VBELN EQ P_REFKEY(10).

  IF ( SY-SUBRC EQ 0 ).

    SELECT * FROM VBFA
      INTO TABLE LT_VBFA
      FOR ALL ENTRIES IN LT_VBRK
    WHERE VBELN EQ LT_VBRK-VBELN
      AND VBTYP_N EQ 'M'
      AND VBTYP_V EQ 'J'.

    IF ( SY-SUBRC EQ 0 ).

      SELECT *  FROM ZDOC_EXP
        INTO TABLE LT_ZDOC_EXP
        FOR ALL ENTRIES IN LT_VBFA
      WHERE VBELN EQ LT_VBFA-VBELV.

      IF ( SY-SUBRC EQ 0 ).

        SELECT * FROM ZDOC_NF_PRODUTOR
            INTO TABLE LT_ZDOC_NF_PRODUTOR
            FOR ALL ENTRIES IN LT_ZDOC_EXP
         WHERE VBELN EQ LT_ZDOC_EXP-VBELN.

        IF ( SY-SUBRC EQ 0 ).

          READ TABLE LT_VBRK INTO LW_VBRK INDEX 1.

          IF ( SY-SUBRC EQ 0 ).

            CASE LW_VBRK-FKART.

              WHEN: 'ZEXI'.

                READ TABLE LT_VBFA     INTO LW_VBFA     WITH KEY VBELN = LW_VBRK-VBELN.
                READ TABLE LT_ZDOC_EXP INTO LW_ZDOC_EXP WITH KEY VBELN = LW_VBFA-VBELV.

                IF ( SY-SUBRC EQ 0 ).

                  CREATE OBJECT LOBJ_UTIL.

                  LOOP AT LT_ZDOC_NF_PRODUTOR INTO LW_ZDOC_NF_PRODUTOR.

                    VL_LEN_XML = STRLEN( P_XML_DET_EXP01 ).
                    ADD 200 TO VL_LEN_XML.

                    IF ( VL_LEN_XML > 30000 ).
                      PERFORM ADD_DET_EXP  USING P_XML_DET_EXP02 LW_ZDOC_NF_PRODUTOR
                                                 LW_ZDOC_EXP     LOBJ_UTIL.
                    ELSE.
                      PERFORM ADD_DET_EXP  USING P_XML_DET_EXP01 LW_ZDOC_NF_PRODUTOR
                                                 LW_ZDOC_EXP     LOBJ_UTIL.
                    ENDIF.

                    CLEAR: LW_ZDOC_NF_PRODUTOR, LW_CHAVE.
                  ENDLOOP.

                ENDIF.

            ENDCASE.

          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " DADOS_COMP
*&---------------------------------------------------------------------*
*&      Form  CEST
*&---------------------------------------------------------------------*

FORM CEST  USING  P_NFE P_COD_PRODUDO.

  DATA: WA_ZSDT0106 TYPE ZSDT0106,
        VL_MATNR    TYPE ZSDT0106-MATNR,
        VL_COD_PROD TYPE STRING.

  CLEAR: VL_MATNR,VL_COD_PROD.

  VL_COD_PROD = P_COD_PRODUDO.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = VL_COD_PROD
    IMPORTING
      OUTPUT = VL_MATNR.

  SELECT SINGLE *
    INTO WA_ZSDT0106
    FROM ZSDT0106
   WHERE MATNR = VL_MATNR.

  IF SY-SUBRC = 0.
    PERFORM CTNAVN USING DETCEST WA_ZSDT0106-CEST   P_NFE.
  ENDIF.

ENDFORM.                    " CEST

*&---------------------------------------------------------------------*
*&      Form  ADD_DET_EXP
*&---------------------------------------------------------------------*
FORM ADD_DET_EXP  USING    P_XML_DET  TYPE ZXML
                           P_ZDOC_NF_PRODUTOR TYPE ZDOC_NF_PRODUTOR
                           P_ZDOC_EXP         TYPE ZDOC_EXP
                           P_OBJ_UTIL TYPE REF TO ZCL_UTIL.

  DATA: LW_ZLEST0147 TYPE ZLEST0147.

  DATA: LW_CHAVE  TYPE C LENGTH 44.

  CHECK P_ZDOC_NF_PRODUTOR-DOCNUM_PROD IS NOT INITIAL.

  SELECT SINGLE * FROM J_1BNFE_ACTIVE INTO @DATA(_WL_ACTIVE) WHERE DOCNUM EQ @P_ZDOC_NF_PRODUTOR-DOCNUM_PROD.
  CHECK SY-SUBRC EQ 0.

  CLEAR: LW_CHAVE.

  PERFORM CTNAB USING DETEXPORT P_XML_DET.
  PERFORM CTNAB USING EXPORTIND P_XML_DET.

  REPLACE REGEX '[/]' IN P_ZDOC_EXP-NR_REGISTRO_EXPO WITH ''.
  REPLACE REGEX '[-]' IN P_ZDOC_EXP-NR_REGISTRO_EXPO WITH ''.

  IF P_ZDOC_EXP-NR_REGISTRO_EXPO IS INITIAL.
    P_ZDOC_EXP-NR_REGISTRO_EXPO = '000000000000'.
  ENDIF.

  PERFORM CTNAV USING NRE P_ZDOC_EXP-NR_REGISTRO_EXPO P_XML_DET.

  "Check se é uma entrada Propria
  CLEAR: LW_ZLEST0147.
  SELECT SINGLE * INTO LW_ZLEST0147
    FROM ZLEST0147 AS A
   WHERE A~DOCNUM          EQ P_ZDOC_NF_PRODUTOR-DOCNUM_PROD
     AND A~ENTRADA_PROPRIA EQ ABAP_TRUE
     AND EXISTS ( SELECT ID_RECEPCAO
                    FROM ZLEST0146 AS B
                   WHERE B~ID_RECEPCAO EQ A~ID_RECEPCAO
                     AND B~CANCEL      EQ ABAP_FALSE ).

  IF ( SY-SUBRC EQ 0 ) AND ( LW_ZLEST0147-MODEL EQ '55' ).
    LW_CHAVE = LW_ZLEST0147-CHAVE_NFE.
  ELSE.
    CALL METHOD P_OBJ_UTIL->MONTA_CHAVE_NFE
      EXPORTING
        I_DOCNUM   = P_ZDOC_NF_PRODUTOR-DOCNUM_PROD
        I_VALIDA   = 'X'
      RECEIVING
        E_CHAVE    = LW_CHAVE
      EXCEPTIONS
        ERRO_CHAVE = 1
        OTHERS     = 2.
  ENDIF.

  IF SY-SUBRC IS INITIAL.
    PERFORM CTNAV  USING CC_CHNFE LW_CHAVE P_XML_DET.
    PERFORM CTNAVN USING QEXPORT P_ZDOC_NF_PRODUTOR-MENGE P_XML_DET.
  ENDIF.

  PERFORM CTNFE USING EXPORTIND P_XML_DET.
  PERFORM CTNFE USING DETEXPORT P_XML_DET.

ENDFORM.                    " ADD_REF_NFE

*&---------------------------------------------------------------------*
*&      Form  ADD_XML_REF_TABLE
*&---------------------------------------------------------------------*
FORM ADD_XML_TABLE TABLES P_IT_XML_NFE
                   USING  P_XML_NFE.


  DATA: WA_XML_NFE TYPE ZXML4000.

  CLEAR WA_XML_NFE.
  WA_XML_NFE   = P_XML_NFE(4000).
  CHECK WA_XML_NFE IS NOT INITIAL.
  APPEND WA_XML_NFE TO P_IT_XML_NFE.

  CLEAR WA_XML_NFE.
  WA_XML_NFE   = P_XML_NFE+04000(4000).
  CHECK WA_XML_NFE IS NOT INITIAL.
  APPEND WA_XML_NFE TO P_IT_XML_NFE.

  CLEAR WA_XML_NFE.
  WA_XML_NFE   = P_XML_NFE+08000(4000).
  CHECK WA_XML_NFE IS NOT INITIAL.
  APPEND WA_XML_NFE TO P_IT_XML_NFE.

  CLEAR WA_XML_NFE.
  WA_XML_NFE   = P_XML_NFE+12000(4000).
  CHECK WA_XML_NFE IS NOT INITIAL.
  APPEND WA_XML_NFE TO P_IT_XML_NFE.

  CLEAR WA_XML_NFE.
  WA_XML_NFE   = P_XML_NFE+16000(4000).
  CHECK WA_XML_NFE IS NOT INITIAL.
  APPEND WA_XML_NFE TO P_IT_XML_NFE.

  CLEAR WA_XML_NFE.
  WA_XML_NFE   = P_XML_NFE+20000(4000).
  CHECK WA_XML_NFE IS NOT INITIAL.
  APPEND WA_XML_NFE TO P_IT_XML_NFE.

  CLEAR WA_XML_NFE.
  WA_XML_NFE   = P_XML_NFE+24000(4000).
  CHECK WA_XML_NFE IS NOT INITIAL.
  APPEND WA_XML_NFE TO P_IT_XML_NFE.

  CLEAR WA_XML_NFE.
  WA_XML_NFE   = P_XML_NFE+28000(2000).
  CHECK WA_XML_NFE IS NOT INITIAL.
  APPEND WA_XML_NFE TO P_IT_XML_NFE.

ENDFORM.                    " ADD_XML_TABLE
*&---------------------------------------------------------------------*
*&      Form  CONC_XML
*&---------------------------------------------------------------------*
FORM CONC_XML  USING    P_XML_VALOR TYPE ZXML4000
                        P_NFE01
                        P_NFE02.

  DATA: VL_TAM_NFE     TYPE I,
        VL_TAM_VLR     TYPE I,
        VL_TAM_RES_NFE TYPE I,
        VL_TAM_RES_VLR TYPE I.

  "NF-e 01
  IF ( STRLEN( P_NFE01 ) < 30000 ).

    VL_TAM_RES_NFE = 30000 - STRLEN( P_NFE01 ).

    IF VL_TAM_RES_NFE < 4000.
      CONCATENATE P_NFE01 P_XML_VALOR(VL_TAM_RES_NFE) INTO P_NFE01.
      VL_TAM_RES_VLR =  STRLEN( P_XML_VALOR ) - VL_TAM_RES_NFE.
      IF VL_TAM_RES_VLR > 0.
        CONCATENATE P_NFE02 P_XML_VALOR+VL_TAM_RES_NFE(VL_TAM_RES_VLR) INTO P_NFE02.
      ENDIF.
      EXIT.
    ELSE.
      CONCATENATE P_NFE01 P_XML_VALOR INTO P_NFE01.
      EXIT.
    ENDIF.

  ENDIF.

  "NF-e 02
  IF ( STRLEN( P_NFE02 ) < 30000 ).

    VL_TAM_RES_NFE = 30000 - STRLEN( P_NFE02 ).

    IF VL_TAM_RES_NFE < 4000.
      CONCATENATE P_NFE02 P_XML_VALOR(VL_TAM_RES_NFE) INTO P_NFE02.
      EXIT.
    ELSE.
      CONCATENATE P_NFE02 P_XML_VALOR INTO P_NFE02.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.                    " CONC_XML
*&---------------------------------------------------------------------*
*&      Form  CONC_XML_REF_DET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CONC_XML_REF_DET  USING  P_NFE           TYPE ZXML
                              P_WA_NOTA       TYPE ZOB_NOTA_FISCAL_SAP
                              P_NFE01         TYPE ZXML
                              P_NFE02         TYPE ZXML
                              P_XML_REF_NFE01 TYPE ZXML
                              P_XML_REF_NFE02 TYPE ZXML
                              P_XML_DET_EXP01 TYPE ZXML
                              P_XML_DET_EXP02 TYPE ZXML.

  DATA: NFE_AUX    TYPE ZXML,
        IT_XML_NFE TYPE TABLE OF ZXML4000,
        WA_XML_NFE TYPE ZXML4000.

  DATA: VL_INI_COPY      TYPE I,
        VL_FIM_COPY      TYPE I,
        VL_INTERVAL_COPY TYPE I.

  CLEAR: VL_INI_COPY.
  FIND '</indPres>' IN P_NFE MATCH OFFSET VL_INI_COPY.
  ADD 10 TO VL_INI_COPY.

  IF ( VL_INI_COPY > 10 ).

    CLEAR: NFE_AUX.
    NFE_AUX = P_NFE(VL_INI_COPY).

    "Transfere XML Inicial p/ Tabela Interna(Até tag </indPres> ).
    PERFORM ADD_XML_TABLE TABLES IT_XML_NFE USING NFE_AUX.

    "Transfere XML Ref NF-e. p/ Tabela Interna
    "(Tags <NFref><refNFe>22160410XXXXXX</refNFe></NFref>).
    PERFORM ADD_XML_TABLE TABLES IT_XML_NFE USING P_XML_REF_NFE01.
    PERFORM ADD_XML_TABLE TABLES IT_XML_NFE USING P_XML_REF_NFE02.

    CLEAR: VL_FIM_COPY.
    FIND '</indTot>' IN P_NFE MATCH OFFSET VL_FIM_COPY.
    ADD 9 TO VL_FIM_COPY.
    IF ( P_XML_DET_EXP01 IS NOT INITIAL ) AND
       ( VL_FIM_COPY > 9 ).

      VL_INTERVAL_COPY = VL_FIM_COPY - VL_INI_COPY.

      CLEAR: NFE_AUX.
      NFE_AUX = P_NFE+VL_INI_COPY(VL_INTERVAL_COPY).

      "Transfere XML intermediário p/ Tabela Interna
      "(Da Tag </indPres> até '</indTot>').
      PERFORM ADD_XML_TABLE TABLES IT_XML_NFE USING NFE_AUX.

      "Transfere XML Detalhes Exportação p/ Tabela Interna.
      "(Tags <detExport><exportInd><nRE>16XX</nRE><chNFe>221XXX</chNFe><qExport>47540.000 </qExport></exportInd></detExport>)
      PERFORM ADD_XML_TABLE TABLES IT_XML_NFE USING P_XML_DET_EXP01.
      PERFORM ADD_XML_TABLE TABLES IT_XML_NFE USING P_XML_DET_EXP02.

      "Transfere XML Final p/ Tabela Interna.
      "(Da Tag </indTot> até Final do XML)
      VL_INI_COPY = VL_FIM_COPY.

      VL_FIM_COPY = STRLEN( P_NFE ) - VL_INI_COPY.
      CLEAR: NFE_AUX.
      NFE_AUX = P_NFE+VL_INI_COPY(VL_FIM_COPY).

      PERFORM ADD_XML_TABLE TABLES IT_XML_NFE USING NFE_AUX.

    ELSE.
      "Transfere XML Final p/ Tabela Interna.
      "(Da Tag </indPres> Até Final do XML)
      VL_FIM_COPY = STRLEN( P_NFE ) - VL_INI_COPY.

      CLEAR: NFE_AUX.
      NFE_AUX = P_NFE+VL_INI_COPY(VL_FIM_COPY).

      PERFORM ADD_XML_TABLE TABLES IT_XML_NFE USING NFE_AUX.
    ENDIF.

    LOOP AT IT_XML_NFE INTO WA_XML_NFE.
      PERFORM CONC_XML USING WA_XML_NFE P_NFE01 P_NFE02.
      CLEAR: WA_XML_NFE.
    ENDLOOP.

  ENDIF. "IF ( VL_INI_COPY > 10 ).

  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN P_NFE01 WITH 'a' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN P_NFE01 WITH 'e' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'í'     IN P_NFE01 WITH 'i' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN P_NFE01 WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN P_NFE01 WITH 'u' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN P_NFE01 WITH 'c' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        '&'     IN P_NFE01 WITH '&#38;'.
  REPLACE ALL OCCURRENCES OF        ''''    IN P_NFE01 WITH '&#39;'.
  REPLACE ALL OCCURRENCES OF        'º'     IN P_NFE01 WITH 'o' IGNORING CASE.

  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN P_NFE02 WITH 'a' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN P_NFE02 WITH 'e' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'í'     IN P_NFE02 WITH 'i' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN P_NFE02 WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN P_NFE02 WITH 'u' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN P_NFE02 WITH 'c' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        '&'     IN P_NFE02 WITH '&#38;'.
  REPLACE ALL OCCURRENCES OF        ''''    IN P_NFE02 WITH '&#39;'.
  REPLACE ALL OCCURRENCES OF        'º'     IN P_NFE02 WITH 'o' IGNORING CASE.

  P_WA_NOTA-TX_XML    = P_NFE01(4000).
  P_WA_NOTA-TX_XML2   = P_NFE01+04000(4000).
  P_WA_NOTA-TX_XML3   = P_NFE01+08000(4000).
  P_WA_NOTA-TX_XML4   = P_NFE01+12000(4000).
  P_WA_NOTA-TX_XML5   = P_NFE01+16000(4000).
  P_WA_NOTA-TX_XML6   = P_NFE01+20000(4000).
  P_WA_NOTA-TX_XML7   = P_NFE01+24000(4000).
  P_WA_NOTA-TX_XML8   = P_NFE01+28000(2000).
  P_WA_NOTA-TX_XML9   = P_NFE02(4000).
  P_WA_NOTA-TX_XML10  = P_NFE02+04000(4000).
  P_WA_NOTA-TX_XML11  = P_NFE02+08000(4000).
  P_WA_NOTA-TX_XML12  = P_NFE02+12000(4000).
  P_WA_NOTA-TX_XML13  = P_NFE02+16000(4000).
  P_WA_NOTA-TX_XML14  = P_NFE02+20000(4000).
  P_WA_NOTA-TX_XML15  = P_NFE02+24000(4000).
  P_WA_NOTA-TX_XML16  = P_NFE02+28000(2000).


ENDFORM.                    " CONC_XML_REF_DET


FORM IMPOSTOICMSINTER  USING   WA_ITNS  TYPE J1B_NF_XML_ITEM  P_NFE.


  TYPES: BEGIN OF TY_ICMS_INTER,
           VBCUFDEST      TYPE J_1BTAXVAL,
           PFCPUFDEST     TYPE J_1BTAXVAL,
           PICMSUFDEST    TYPE J_1BTAXVAL,
           PICMSINTER     TYPE J_1BTAXVAL,
           PICMSINTERPART TYPE J_1BTAXVAL,
           VFCPUFDEST     TYPE J_1BTAXVAL,
           VICMSUFDEST    TYPE J_1BTAXVAL,
           VICMSUFREMET   TYPE J_1BTAXVAL,
         END OF TY_ICMS_INTER.

  DATA: WL_ICMS_INTER TYPE TY_ICMS_INTER,
        VL_CALCULADO  TYPE C.

  CLEAR: WL_ICMS_INTER.

  CALL FUNCTION 'ZCALC_ICMS_VENDA_INTERESTADUAL'
    EXPORTING
      I_DOCNUM         = WA_ITNS-DOCNUM
      I_ITMNUM         = WA_ITNS-ITMNUM
      I_CFOP           = WA_ITNS-CFOP
    IMPORTING
      E_CALCULADO      = VL_CALCULADO
    CHANGING
      C_VBCUFDEST      = WL_ICMS_INTER-VBCUFDEST
      C_PFCPUFDEST     = WL_ICMS_INTER-PFCPUFDEST
      C_PICMSUFDEST    = WL_ICMS_INTER-PICMSUFDEST
      C_PICMSINTER     = WL_ICMS_INTER-PICMSINTER
      C_PICMSINTERPART = WL_ICMS_INTER-PICMSINTERPART
      C_VFCPUFDEST     = WL_ICMS_INTER-VFCPUFDEST
      C_VICMSUFDEST    = WL_ICMS_INTER-VICMSUFDEST
      C_VICMSUFREMET   = WL_ICMS_INTER-VICMSUFREMET.

  CHECK VL_CALCULADO IS NOT INITIAL.

  PERFORM CTNAB  USING IMPICMSUFDEST P_NFE.

  PERFORM CTNAVN USING IMPVBCUFDEST        WL_ICMS_INTER-VBCUFDEST      P_NFE.
  PERFORM CTNAVN USING IMPPFCPUFDEST       WL_ICMS_INTER-PFCPUFDEST     P_NFE.
  PERFORM CTNAVN USING IMPPICMSUFDEST      WL_ICMS_INTER-PICMSUFDEST    P_NFE.
  PERFORM CTNAVN USING IMPPICMSINTER       WL_ICMS_INTER-PICMSINTER     P_NFE.
  PERFORM CTNAVN USING IMPPICMSINTERPART   WL_ICMS_INTER-PICMSINTERPART P_NFE.
  PERFORM CTNAVN USING IMPVFCPUFDEST       WL_ICMS_INTER-VFCPUFDEST     P_NFE.
  PERFORM CTNAVN USING IMPVICMSUFDEST      WL_ICMS_INTER-VICMSUFDEST    P_NFE.
  PERFORM CTNAVN USING IMPVICMSUFREMET     WL_ICMS_INTER-VICMSUFREMET   P_NFE.

  PERFORM CTNFE  USING IMPICMSUFDEST P_NFE.

  ADD WL_ICMS_INTER-VICMSUFDEST  TO VAR_TOT_ICMS_UF_DEST.
  ADD WL_ICMS_INTER-VICMSUFREMET TO VAR_TOT_ICMS_UF_REMET.

ENDFORM.

FORM GET_VERSAO_XML .

  CLEAR: VG_VERSAO_NFE, VG_VERSAO_CTE.

  "Get Versão CT-e
  SELECT SINGLE * FROM SETLEAF INTO @DATA(_WL_SETLEAF) WHERE SETNAME = 'MAGGI_VERSAO_CTE'.
  IF ( SY-SUBRC = 0 ) AND ( _WL_SETLEAF-VALFROM IS NOT INITIAL ).
    VG_VERSAO_CTE = _WL_SETLEAF-VALFROM.
  ENDIF.

  IF VG_VERSAO_CTE IS INITIAL.
    VG_VERSAO_CTE = '2.00'.
  ENDIF.

  "Get Versão NF-e
  SELECT SINGLE * FROM SETLEAF INTO _WL_SETLEAF WHERE SETNAME = 'MAGGI_VERSAO_NFE'.
  IF ( SY-SUBRC = 0 ) AND ( _WL_SETLEAF-VALFROM IS NOT INITIAL ).
    VG_VERSAO_NFE = _WL_SETLEAF-VALFROM.
  ENDIF.

  IF VG_VERSAO_NFE IS INITIAL.
    VG_VERSAO_NFE = '3.10'.
  ENDIF.

ENDFORM.

FORM GET_PLACA_VEIC_NFE USING TB           TYPE J1B_NF_XML_HEADER
                     CHANGING C_PC_VEICULO TYPE Y_PC_VEICULO.

  DATA: WL_ZFIWRT0019 TYPE ZFIWRT0019,
        WL_LFA1       TYPE LFA1.

  CLEAR: C_PC_VEICULO.

  SELECT SINGLE *
    FROM J_1BNFLIN INTO @DATA(_WL_LIN)
   WHERE DOCNUM EQ @TB-DOCNUM.

  CHECK ( SY-SUBRC = 0 ).

  IF ( TB-T2_PLACA IS NOT INITIAL ).
    C_PC_VEICULO-PLACA = TB-T2_PLACA.
    C_PC_VEICULO-UF    = TB-T2_UF1.
    C_PC_VEICULO-RNTC  = TB-T2_RNTC.
  ELSE.

    CLEAR: WL_ZFIWRT0019, WL_LFA1.

    IF _WL_LIN-REFKEY(10) IS NOT INITIAL.
      SELECT SINGLE *
        FROM ZFIWRT0019 INTO WL_ZFIWRT0019
       WHERE SEQ_LCTO EQ _WL_LIN-REFKEY(10).

      IF SY-SUBRC = 0.
        SELECT SINGLE *
          FROM LFA1 INTO WL_LFA1
         WHERE LIFNR EQ WL_ZFIWRT0019-LIFNR.
      ENDIF.
    ENDIF.

    IF ( WL_ZFIWRT0019 IS NOT INITIAL ) AND ( WL_ZFIWRT0019-PLACA IS NOT INITIAL ).
      C_PC_VEICULO-PLACA = WL_ZFIWRT0019-PLACA.
      C_PC_VEICULO-UF    = WL_ZFIWRT0019-UFPLACA.
      C_PC_VEICULO-RNTC  = WL_LFA1-BAHNS.
    ELSE.

      IF _WL_LIN-REFKEY(10) IS NOT INITIAL.
        SELECT SINGLE *
          FROM VBFA INTO @DATA(_WL_VBFA)
         WHERE VBELN   EQ @_WL_LIN-REFKEY(10)
           AND POSNN   EQ @_WL_LIN-REFITM
           AND VBTYP_V EQ 'J'.
        IF SY-SUBRC EQ 0.
          SELECT SINGLE *
            FROM ZSDT0001 INTO @DATA(_WL_ZSDT0001)
           WHERE DOC_REM      EQ @_WL_VBFA-VBELV
             AND TP_MOVIMENTO EQ 'S'.
          IF ( SY-SUBRC EQ 0 ) AND ( _WL_ZSDT0001-PLACA_CAV IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ZLEST0002 INTO @DATA(_WL_ZLEST0002)
             WHERE PC_VEICULO EQ @_WL_ZSDT0001-PLACA_CAV.
            IF ( SY-SUBRC EQ 0 ) OR ( _WL_ZSDT0001-REGION IS NOT INITIAL ).
              CLEAR: WL_LFA1.
              IF _WL_ZLEST0002-PROPRIETARIO IS NOT INITIAL.
                SELECT SINGLE *
                  FROM LFA1 INTO WL_LFA1
                 WHERE LIFNR EQ _WL_ZLEST0002-PROPRIETARIO.
              ENDIF.
              IF ( SY-SUBRC EQ 0 ) OR ( _WL_ZSDT0001-REGION IS NOT INITIAL ).
                IF _WL_ZSDT0001-REGION IS NOT INITIAL.
                  _WL_ZLEST0002-CD_UF = _WL_ZSDT0001-REGION.
                ENDIF.

                "Placa Cavalo |----------------------------------------------|

                C_PC_VEICULO-PLACA      = _WL_ZSDT0001-PLACA_CAV.
                C_PC_VEICULO-UF         = _WL_ZLEST0002-CD_UF.
                C_PC_VEICULO-RNTC       = WL_LFA1-BAHNS.

                "Placa Reboque 1 |-------------------------------------------|
                C_PC_VEICULO-PLACA_CAR1 = _WL_ZSDT0001-PLACA_CAR1.

                SELECT SINGLE *
                  FROM ZLEST0002 INTO @DATA(_WL_ZLEST0002_TMP)
                 WHERE PC_VEICULO EQ @C_PC_VEICULO-PLACA_CAR1.

                IF ( SY-SUBRC EQ 0 ) AND ( C_PC_VEICULO-PLACA_CAR1 IS NOT INITIAL ).
                  C_PC_VEICULO-UF_CAR1 = _WL_ZLEST0002_TMP-CD_UF.
                ENDIF.

                "Placa Reboque 2 |-------------------------------------------|
                C_PC_VEICULO-PLACA_CAR2 = _WL_ZSDT0001-PLACA_CAR2.

                SELECT SINGLE *
                  FROM ZLEST0002 INTO _WL_ZLEST0002_TMP
                 WHERE PC_VEICULO EQ C_PC_VEICULO-PLACA_CAR2.

                IF ( SY-SUBRC EQ 0 ) AND ( C_PC_VEICULO-PLACA_CAR2 IS NOT INITIAL ).
                  C_PC_VEICULO-UF_CAR2 = _WL_ZLEST0002_TMP-CD_UF.
                ENDIF.
                "Placa Reboque 3 |-------------------------------------------|

                C_PC_VEICULO-PLACA_CAR3 = _WL_ZSDT0001-PLACA_CAR3.

                SELECT SINGLE *
                  FROM ZLEST0002 INTO _WL_ZLEST0002_TMP
                 WHERE PC_VEICULO EQ C_PC_VEICULO-PLACA_CAR3.

                IF ( SY-SUBRC EQ 0 ) AND ( C_PC_VEICULO-PLACA_CAR3 IS NOT INITIAL ).
                  C_PC_VEICULO-UF_CAR3 = _WL_ZLEST0002_TMP-CD_UF.
                ENDIF.

                C_PC_VEICULO-CODIGO_MOT = |{ _WL_ZSDT0001-MOTORISTA ALPHA = IN }|.

                SELECT SINGLE *
                  FROM LFA1 INTO @DATA(_LFA1_MOT)
                 WHERE LIFNR EQ @C_PC_VEICULO-CODIGO_MOT.

                IF ( SY-SUBRC EQ 0 ) AND ( C_PC_VEICULO-CODIGO_MOT IS NOT INITIAL ).
                  C_PC_VEICULO-CPF_MOT  = _LFA1_MOT-STCD2.
                  C_PC_VEICULO-NOME_MOT = _LFA1_MOT-NAME1.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF. " IF _WL_LIN-REFKEY(10) IS NOT INITIAL.
    ENDIF.
  ENDIF.



ENDFORM.
