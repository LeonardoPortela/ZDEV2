*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Autor......: Rogério Filipsick                                       *
* Data.......: 07/08/2019                                              *
* Descrição  : Relatório de Fretes Aquaviários                         *
* Transação..: ZLES0148                                                *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição *
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
REPORT ZLESR0110_AQUAV.

*----------------------------------------------------------------------*
* Tabelas -------------------------------------------------------------*
*----------------------------------------------------------------------*
TABLES: ZLEST0061, T001, T001W, ZIB_CTE_DIST_N55.

*----------------------------------------------------------------------*
* Tipos ---------------------------------------------------------------*
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_ZLEST0061,
         BUKRS        TYPE ZLEST0061-BUKRS,
         WERKS        TYPE ZLEST0061-WERKS,
         ANO_VIAGEM   TYPE ZLEST0061-ANO_VIAGEM,
         NR_VIAGEM    TYPE ZLEST0061-NR_VIAGEM,
         EMBARCACAO   TYPE ZLEST0061-EMBARCACAO,
         NOME_EMB     TYPE ZLEST0061-NOME_EMB,
         COD_MATERIAL TYPE ZLEST0061-COD_MATERIAL,
         TP_CLASS     TYPE ZLEST0061-TP_CLASS,
         DT_MOVIMENTO TYPE ZLEST0061-DT_MOVIMENTO,
         CL_CODIGO    TYPE ZLEST0061-CL_CODIGO,
         RM_CODIGO    TYPE ZLEST0061-RM_CODIGO,
         AUART        TYPE ZLEST0061-AUART,
         NR_DCO       TYPE ZLEST0061-NR_DCO,
         SAFRA        TYPE ZLEST0061-SAFRA,
         DT_FATURA    TYPE ZLEST0061-DT_FATURA,
         DOCNUM       TYPE ZLEST0061-DOCNUM,
       END OF TY_ZLEST0061,

       BEGIN OF TY_J_1BNFDOC,
         DOCNUM TYPE J_1BNFDOC-DOCNUM,
         CANCEL TYPE J_1BNFDOC-CANCEL,
         NFENUM TYPE J_1BNFDOC-NFENUM,
         SERIES TYPE J_1BNFDOC-SERIES,
         BRGEW  TYPE J_1BNFDOC-BRGEW,
         NFTOT  TYPE J_1BNFDOC-NFTOT,
         PARID  TYPE J_1BNFDOC-PARID,
         CGC    TYPE J_1BNFDOC-CGC,
         PSTDAT TYPE J_1BNFDOC-PSTDAT,
         NAME1  TYPE J_1BNFDOC-NAME1,
       END OF TY_J_1BNFDOC,

       BEGIN OF TY_J_1BNFLIN,
         DOCNUM TYPE J_1BNFLIN-DOCNUM,
         ITMNUM TYPE J_1BNFLIN-ITMNUM,
         NETWR  TYPE J_1BNFLIN-NETWR,
       END OF TY_J_1BNFLIN,

       BEGIN OF TY_MAKT,
         MATNR TYPE MAKT-MATNR,
         SPRAS TYPE MAKT-SPRAS,
         MAKTX TYPE MAKT-MAKTX,
       END OF TY_MAKT,

       BEGIN OF TY_MARA,
         MATNR TYPE MARA-MATNR,
         MATKL TYPE MARA-MATKL,
       END OF TY_MARA,

       BEGIN OF TY_T023T,
         MATKL   TYPE T023T-MATKL,
         WGBEZ60 TYPE T023T-WGBEZ60,
       END OF TY_T023T,

       BEGIN OF TY_ZLEST0056,
         BUKRS       TYPE ZLEST0056-BUKRS,
         WERKS       TYPE ZLEST0056-WERKS,
         NR_VIAGEM   TYPE ZLEST0056-NR_VIAGEM,
         ANO_VIAGEM  TYPE ZLEST0056-ANO_VIAGEM,
         PO_EMBARQUE TYPE ZLEST0056-PO_EMBARQUE,
         PO_DESTINO  TYPE ZLEST0056-PO_DESTINO,
       END OF TY_ZLEST0056,

       BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         LAND1 TYPE LFA1-LAND1,
         NAME1 TYPE LFA1-NAME1,
         ORT01 TYPE LFA1-ORT01,
         REGIO TYPE LFA1-REGIO,
       END OF TY_LFA1,

       BEGIN OF TY_KNA1,
         KUNNR TYPE KNA1-KUNNR,
         LAND1 TYPE KNA1-LAND1,
         NAME1 TYPE KNA1-NAME1,
         ORT01 TYPE KNA1-ORT01,
         REGIO TYPE KNA1-REGIO,
       END OF TY_KNA1,

       BEGIN OF TY_ZLEST0063,
         BUKRS      TYPE ZLEST0063-BUKRS,
         WERKS      TYPE ZLEST0063-WERKS,
         NR_VIAGEM  TYPE ZLEST0063-NR_VIAGEM,
         ANO_VIAGEM TYPE ZLEST0063-ANO_VIAGEM,
         EMBARCACAO TYPE ZLEST0063-EMBARCACAO,
         NOME_EMB   TYPE ZLEST0063-NOME_EMB,
       END OF TY_ZLEST0063.

TYPES: BEGIN OF TY_REPORT.
         INCLUDE TYPE ZHRST_FRETE.
       TYPES: END OF TY_REPORT .

*----------------------------------------------------------------------*
* Tabelas internas ----------------------------------------------------*
*----------------------------------------------------------------------*
DATA: T_ZLEST0061 TYPE TABLE OF TY_ZLEST0061,
      T_J_1BNFDOC TYPE TABLE OF TY_J_1BNFDOC,
      T_J_1BNFLIN TYPE TABLE OF TY_J_1BNFLIN,
      T_MAKT      TYPE TABLE OF TY_MAKT,
      T_MARA      TYPE TABLE OF TY_MARA,
      T_T023T     TYPE TABLE OF TY_T023T,
      T_ZLEST0056 TYPE TABLE OF TY_ZLEST0056,
      T_LFA1      TYPE TABLE OF TY_LFA1,
      T_KNA1      TYPE TABLE OF TY_KNA1,
      T_ZLEST0063 TYPE TABLE OF TY_ZLEST0063,
      T_REPORT    TYPE TABLE OF TY_REPORT,
      T_FIELDCAT  TYPE   SLIS_T_FIELDCAT_ALV.

*----------------------------------------------------------------------*
* Estruturas ----------------------------------------------------------*
*----------------------------------------------------------------------*
DATA: WA_LAYOUT TYPE SLIS_LAYOUT_ALV.

*----------------------------------------------------------------------*
* Constantes ----------------------------------------------------------*
*----------------------------------------------------------------------*
CONSTANTS: C_S TYPE C VALUE 'S',
           C_E TYPE C VALUE 'E'.

*----------------------------------------------------------------------*
* Tela de seleção -----------------------------------------------------*
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_DF FOR ZLEST0061-DT_FATURA OBLIGATORY,
                S_BUKRS FOR T001-BUKRS,
                S_WERKS FOR T001W-WERKS,
                S_DOCNUM FOR ZLEST0061-DOCNUM.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* START-OF-SELECTION --------------------------------------------------*
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM F_SELECIONA_DADOS.

  PERFORM F_MONTA_DADOS.

*----------------------------------------------------------------------*
* END-OF-SELECTION ----------------------------------------------------*
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF T_REPORT[] IS NOT INITIAL.
    PERFORM F_MONTA_SAIDA.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*      Seleciona dados do Report
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.

  SELECT BUKRS WERKS ANO_VIAGEM NR_VIAGEM EMBARCACAO
         NOME_EMB COD_MATERIAL TP_CLASS DT_MOVIMENTO
         CL_CODIGO RM_CODIGO AUART NR_DCO SAFRA
         DT_FATURA DOCNUM
    FROM ZLEST0061
    INTO TABLE T_ZLEST0061
   WHERE DT_FATURA IN S_DF
     AND BUKRS     IN S_BUKRS
     AND WERKS     IN S_WERKS
     AND DOCNUM    IN S_DOCNUM.

  IF SY-SUBRC IS INITIAL.

    SELECT BUKRS WERKS NR_VIAGEM ANO_VIAGEM
           EMBARCACAO NOME_EMB
      FROM ZLEST0063
      INTO TABLE T_ZLEST0063
       FOR ALL ENTRIES IN T_ZLEST0061
     WHERE BUKRS      = T_ZLEST0061-BUKRS
       AND WERKS      = T_ZLEST0061-WERKS
       AND NR_VIAGEM  = T_ZLEST0061-NR_VIAGEM
       AND ANO_VIAGEM = T_ZLEST0061-ANO_VIAGEM
       AND EMBARCACAO = C_E.

    SELECT DOCNUM CANCEL NFENUM SERIES
           BRGEW NFTOT PARID CGC PSTDAT
           NAME1
      FROM J_1BNFDOC
      INTO TABLE T_J_1BNFDOC
       FOR ALL ENTRIES IN T_ZLEST0061
     WHERE DOCNUM = T_ZLEST0061-DOCNUM
       AND CANCEL = SPACE.

    IF SY-SUBRC IS INITIAL.

      SELECT DOCNUM ITMNUM NETWR
        FROM J_1BNFLIN
        INTO TABLE T_J_1BNFLIN
         FOR ALL ENTRIES IN T_J_1BNFDOC
       WHERE DOCNUM = T_J_1BNFDOC-DOCNUM
         AND ITMNUM = '00010'.

    ENDIF.

    SELECT MATNR SPRAS MAKTX
      FROM MAKT
      INTO TABLE T_MAKT
       FOR ALL ENTRIES IN T_ZLEST0061
     WHERE MATNR = T_ZLEST0061-COD_MATERIAL
       AND SPRAS = SY-LANGU.

    SELECT MATNR MATKL
      FROM MARA
      INTO TABLE T_MARA
       FOR ALL ENTRIES IN T_ZLEST0061
     WHERE MATNR = T_ZLEST0061-COD_MATERIAL.

    IF SY-SUBRC IS INITIAL.
      SELECT MATKL WGBEZ60
        FROM T023T
        INTO TABLE T_T023T
         FOR ALL ENTRIES IN T_MARA
       WHERE MATKL = T_MARA-MATKL.
    ENDIF.


    SELECT BUKRS WERKS NR_VIAGEM
           ANO_VIAGEM PO_EMBARQUE PO_DESTINO
      FROM ZLEST0056
      INTO TABLE T_ZLEST0056
       FOR ALL ENTRIES IN T_ZLEST0061
     WHERE BUKRS      = T_ZLEST0061-BUKRS
       AND WERKS      = T_ZLEST0061-WERKS
       AND NR_VIAGEM  = T_ZLEST0061-NR_VIAGEM
       AND ANO_VIAGEM = T_ZLEST0061-ANO_VIAGEM.

    IF SY-SUBRC IS INITIAL.

      SELECT LIFNR LAND1 NAME1 ORT01 REGIO
        FROM LFA1
        INTO TABLE T_LFA1
         FOR ALL ENTRIES IN T_ZLEST0056
       WHERE LIFNR = T_ZLEST0056-PO_EMBARQUE
         AND LAND1 = 'BR'.

      SELECT KUNNR LAND1 NAME1 ORT01 REGIO
        FROM KNA1
        INTO TABLE T_KNA1
         FOR ALL ENTRIES IN T_ZLEST0056
       WHERE KUNNR = T_ZLEST0056-PO_DESTINO
         AND LAND1 = 'BR'.


    ENDIF.

  ELSE.
    MESSAGE S000(Z_LES) WITH TEXT-002 DISPLAY LIKE C_S.
    STOP.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS
*&---------------------------------------------------------------------*
*       Monta dados do report
*----------------------------------------------------------------------*
FORM F_MONTA_DADOS.

  DATA: LW_CHAVE  TYPE C LENGTH 44.

  LOOP AT T_ZLEST0061 ASSIGNING FIELD-SYMBOL(<FS_ZLEST0061>).


    READ TABLE T_ZLEST0063 ASSIGNING FIELD-SYMBOL(<ZLEST0063>)
                                         WITH KEY BUKRS      = <FS_ZLEST0061>-BUKRS
                                                  WERKS      = <FS_ZLEST0061>-WERKS
                                                  NR_VIAGEM  = <FS_ZLEST0061>-NR_VIAGEM
                                                  ANO_VIAGEM = <FS_ZLEST0061>-ANO_VIAGEM.

    READ TABLE T_J_1BNFDOC ASSIGNING FIELD-SYMBOL(<FS_J_1BNFDOC>)
                                         WITH KEY DOCNUM = <FS_ZLEST0061>-DOCNUM.

    IF SY-SUBRC IS INITIAL.

      READ TABLE T_J_1BNFLIN ASSIGNING FIELD-SYMBOL(<FS_J_1BNFLIN>)
                                         WITH KEY DOCNUM = <FS_J_1BNFDOC>-DOCNUM.

    ENDIF.

    READ TABLE T_MAKT ASSIGNING FIELD-SYMBOL(<FS_MAKT>)
                                    WITH KEY MATNR = <FS_ZLEST0061>-COD_MATERIAL.


    READ TABLE T_MARA ASSIGNING FIELD-SYMBOL(<FS_MARA>)
                                    WITH KEY MATNR = <FS_ZLEST0061>-COD_MATERIAL.

    IF SY-SUBRC IS INITIAL.

      READ TABLE T_T023T ASSIGNING FIELD-SYMBOL(<FS_T023T>)
                                       WITH KEY MATKL = <FS_MARA>-MATKL.

    ENDIF.

    READ TABLE T_ZLEST0056 ASSIGNING FIELD-SYMBOL(<FS_ZLEST0056>)
                                         WITH KEY BUKRS      = <FS_ZLEST0061>-BUKRS
                                                  WERKS      = <FS_ZLEST0061>-WERKS
                                                  NR_VIAGEM  = <FS_ZLEST0061>-NR_VIAGEM
                                                  ANO_VIAGEM = <FS_ZLEST0061>-ANO_VIAGEM.
    IF SY-SUBRC IS INITIAL.

      READ TABLE T_LFA1 ASSIGNING FIELD-SYMBOL(<FS_LFA1>)
                                     WITH KEY LIFNR = <FS_ZLEST0056>-PO_EMBARQUE.

      READ TABLE T_KNA1 ASSIGNING FIELD-SYMBOL(<FS_KNA1>)
                                      WITH KEY KUNNR = <FS_ZLEST0056>-PO_DESTINO.

    ENDIF.

    APPEND INITIAL LINE TO T_REPORT ASSIGNING FIELD-SYMBOL(<FS_REPORT>).

    CLEAR: LW_CHAVE.
    CALL METHOD ZCL_UTIL=>MONTA_CHAVE_NFE
      EXPORTING
        I_DOCNUM = <FS_ZLEST0061>-DOCNUM
        I_VALIDA = 'X'
      RECEIVING
        E_CHAVE  = LW_CHAVE.

    IF <FS_J_1BNFDOC> IS ASSIGNED.
      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          INPUT  = <FS_J_1BNFDOC>-CGC
        IMPORTING
          OUTPUT = <FS_REPORT>-CNPJ.
    ENDIF.

    <FS_REPORT>-DT_FATURA        = <FS_ZLEST0061>-DT_FATURA.
    <FS_REPORT>-COD_MAT          = <FS_ZLEST0061>-COD_MATERIAL.
    <FS_REPORT>-DOCNUM_CTE       = <FS_ZLEST0061>-DOCNUM.
    <FS_REPORT>-EMPRESA_CTE      = <FS_ZLEST0061>-BUKRS.
    <FS_REPORT>-FILIAL_CTE       = <FS_ZLEST0061>-WERKS.
    <FS_REPORT>-CHAVE_CTE        = LW_CHAVE.
    <FS_REPORT>-NR_VIAGEM        = <FS_ZLEST0061>-NR_VIAGEM.
    <FS_REPORT>-ANO_VIAGEM       = <FS_ZLEST0061>-ANO_VIAGEM.
    <FS_REPORT>-BARCACA          = <FS_ZLEST0061>-NOME_EMB.

    IF <ZLEST0063> IS ASSIGNED.
      <FS_REPORT>-EMPURADOR = <ZLEST0063>-NOME_EMB.
    ENDIF.

    IF <FS_MAKT> IS ASSIGNED.
      <FS_REPORT>-DESCR_MAT        = <FS_MAKT>-MAKTX.
    ENDIF.

    IF <FS_MARA> IS ASSIGNED.
      <FS_REPORT>-GRUPO_MAT        = <FS_MARA>-MATKL.
    ENDIF.

    IF <FS_T023T> IS ASSIGNED.
      <FS_REPORT>-DESC_GRUPO       = <FS_T023T>-WGBEZ60.
    ENDIF.

    IF <FS_J_1BNFDOC> IS ASSIGNED.
      <FS_REPORT>-NR_CTE           = <FS_J_1BNFDOC>-NFENUM.
      <FS_REPORT>-SERIE_CTE        = <FS_J_1BNFDOC>-SERIES.
      <FS_REPORT>-DATA_CTE         = <FS_J_1BNFDOC>-PSTDAT.
      <FS_REPORT>-PESO_BRUTO       = <FS_J_1BNFDOC>-BRGEW.
      <FS_REPORT>-COD_CLIENTE      = <FS_J_1BNFDOC>-PARID.
      <FS_REPORT>-NOME_CLIENTE     = <FS_J_1BNFDOC>-NAME1.
    ENDIF.

    IF <FS_J_1BNFLIN> IS ASSIGNED.
      <FS_REPORT>-VL_BRL           = <FS_J_1BNFLIN>-NETWR.
    ENDIF.

    IF <FS_ZLEST0056> IS ASSIGNED.
      <FS_REPORT>-PORTO_EMBARQUE   = <FS_ZLEST0056>-PO_EMBARQUE.
      <FS_REPORT>-PO_DESTINO       = <FS_ZLEST0056>-PO_DESTINO.
    ENDIF.

    IF <FS_LFA1> IS ASSIGNED.
      <FS_REPORT>-DESCR_PO_EMB     = <FS_LFA1>-NAME1.
      <FS_REPORT>-UF_PO_EMB        = <FS_LFA1>-REGIO.
    ENDIF.

    IF <FS_KNA1> IS ASSIGNED.
      <FS_REPORT>-DESCR_PO_DESTINO = <FS_KNA1>-NAME1.
      <FS_REPORT>-UF_PO_DESTINO    = <FS_KNA1>-REGIO.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_SAIDA
*&---------------------------------------------------------------------*
*       Monta saída Report ALV
*----------------------------------------------------------------------*
FORM F_MONTA_SAIDA .

  DATA: LV_PROGRAM TYPE SY-REPID.


  LV_PROGRAM  = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = LV_PROGRAM
      I_STRUCTURE_NAME       = 'ZHRST_FRETE'
    CHANGING
      CT_FIELDCAT            = T_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.


  WA_LAYOUT-EXPAND_ALL = ABAP_TRUE.
  WA_LAYOUT-COLWIDTH_OPTIMIZE = ABAP_TRUE.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = LV_PROGRAM
      IS_LAYOUT          = WA_LAYOUT
      IT_FIELDCAT        = T_FIELDCAT
    TABLES
      T_OUTTAB           = T_REPORT
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.


ENDFORM.
