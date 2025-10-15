*&---------------------------------------------------------------------*
*& Report
*&
*&---------------------------------------------------------------------*
*&TITULO: Relação de Notas Fiscais por viagem ( Aquaviário )
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 04.11.2013
*TRANSACAO: ZLES0082
*&---------------------------------------------------------------------*

REPORT  ZLES0084.


*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZLEST0061, ZLEST0060.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES:
  BEGIN OF TY_MAKT,
    MATNR TYPE MAKT-MATNR,
    MAKTX TYPE MAKT-MAKTX,
  END OF TY_MAKT,

  BEGIN OF TY_J_1BNFDOC,
    DOCNUM TYPE J_1BNFDOC-DOCNUM,
    NFENUM TYPE J_1BNFDOC-NFENUM,
  END OF TY_J_1BNFDOC,

  BEGIN OF TY_KNA1,
    KUNNR TYPE KNA1-KUNNR,
    STCD1 TYPE KNA1-STCD1,
  END OF TY_KNA1,

  BEGIN OF TY_LFA1,
    LIFNR TYPE LFA1-LIFNR,
    STCD1 TYPE LFA1-STCD1,
  END OF TY_LFA1,

  BEGIN OF TY_SAIDA,
    BUKRS         TYPE ZLEST0060-BUKRS, "EMPRESA
    WERKS         TYPE ZLEST0060-WERKS , "CENTRO
    ANO_VIAGEM    TYPE ZLEST0060-ANO_VIAGEM, "ANO
    NR_VIAGEM     TYPE ZLEST0060-NR_VIAGEM, "VIAGEM
    CL_CODIGO     TYPE ZLEST0061-CL_CODIGO, " COD. CLIENTE
    STCD1         TYPE KNA1-STCD1, " CNPJ CLIENTE
    DT_FATURA     TYPE ZLEST0061-DT_FATURA, "DATA
    DOCNUM        TYPE ZLEST0060-DOCNUM,
    NFENUM        TYPE J_1BNFDOC-NFENUM, " DACTE
    NOME_EMB      TYPE ZLEST0061-NOME_EMB, "BARCAÇA
    CK_ANULADO    TYPE ZLEST0061-CK_ANULADO, "Documento Anulado
    NFNUM         TYPE ZLEST0060-NFNUM, "NR. NF
    DOCDAT        TYPE ZLEST0060-DOCDAT, "DATA NF
    DOC_REM       TYPE ZLEST0060-DOC_REM,
    PESO_FISCAL   TYPE ZLEST0060-PESO_FISCAL, "PESO FISCAL
    NETWR         TYPE ZLEST0060-NETWR, "VALOR NF
    PESO_LIQ_RET  TYPE ZLEST0060-PESO_LIQ_RET, " PESO LIQ_RET
    VLR_LIQ_RET   TYPE ZLEST0060-VLR_LIQ_RET, " VALOR LIQ RET.
    NR_ROMANEIO   TYPE ZLEST0060-NR_ROMANEIO, "ROMANEIO
    MAKTX         TYPE MAKT-MAKTX, "PRODUTO
    SAFRA         TYPE ZLEST0060-SAFRA,
    NR_DCO        TYPE ZLEST0060-NR_DCO,
    PESO_SUBTOTAL TYPE ZSDT0001-PESO_SUBTOTAL,
    PESO_LIQ      TYPE ZSDT0001-PESO_LIQ,
    CHAVE_NFE     TYPE ZLEST0060-CHAVE_NFE,
    SERIES        TYPE ZLEST0060-SERIES,
    RM_CODIGO     TYPE ZLEST0060-RM_CODIGO,
    STCD1_RM      TYPE LFA1-STCD1,
    NETPR         TYPE J_1BNETPRI,
    PERC_VINC     TYPE P DECIMALS 10,
    TP_MAT        TYPE C LENGTH 3,
    OPERACAO      TYPE ZLEST0060-OPERACAO,
    EUDR(12), "ZLES0082 - Adicionar Coluna Atende EUDR - BG #156128
  END OF TY_SAIDA.

TYPES: BEGIN OF TY_SAIDA_0120,
         MATNR       TYPE ZDADOS_COMBOIO_AQUAV-MATNR,
         MAKTX       TYPE ZDADOS_COMBOIO_AQUAV-MAKTX,
         DS_TP_CLASS TYPE ZDADOS_COMBOIO_AQUAV-DS_TP_CLASS.
         INCLUDE STRUCTURE ZDADOS_NF_VINC_AQUAV.
TYPES: END OF TY_SAIDA_0120.


TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.


DATA: OBJ_ALV_0120       TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_0120 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

* ALV excluded functions
DATA: IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE.

* ALV layout variant
DATA: GS_VARIANT       TYPE DISVARIANT.

* ALV layout
DATA: GS_LAYOUT        TYPE LVC_S_LAYO.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: IT_ZLEST0061  TYPE TABLE OF ZLEST0061,
      IT_ZLEST0060  TYPE TABLE OF ZLEST0060,
      IT_ZLEST0063  TYPE TABLE OF ZLEST0063,
      IT_ZSDT0001   TYPE TABLE OF ZSDT0001,
      IT_ZLEST0104  TYPE TABLE OF ZLEST0104,
      IT_MAKT       TYPE TABLE OF TY_MAKT,
      IT_J_1BNFDOC  TYPE TABLE OF TY_J_1BNFDOC,
      IT_KNA1       TYPE TABLE OF TY_KNA1,
      IT_LFA1       TYPE TABLE OF TY_LFA1,
      IT_SAIDA_0120 TYPE TABLE OF TY_SAIDA_0120,
      IT_SAIDA      TYPE TABLE OF TY_SAIDA.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*

DATA: WA_ZLEST0061  TYPE ZLEST0061,
      WA_ZLEST0060  TYPE ZLEST0060,
      WA_ZLEST0063  TYPE ZLEST0063,
      WA_ZSDT0001   TYPE ZSDT0001,
      WA_ZLEST0104  TYPE ZLEST0104,
      WA_MAKT       TYPE TY_MAKT,
      WA_J_1BNFDOC  TYPE TY_J_1BNFDOC,
      WA_KNA1       TYPE TY_KNA1,
      WA_LFA1       TYPE TY_LFA1,
      WA_SAIDA_0120 TYPE TY_SAIDA_0120,
      WA_SAIDA      TYPE TY_SAIDA.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  IT_FCAT         TYPE TABLE OF TY_ESTRUTURA,
  S_VARIANT       TYPE DISVARIANT           , " Tabela Estrutura co
  T_TOP           TYPE SLIS_T_LISTHEADER,
  XS_EVENTS       TYPE SLIS_ALV_EVENT,
  EVENTS          TYPE SLIS_T_EVENT,
  GD_LAYOUT       TYPE SLIS_LAYOUT_ALV,
  T_PRINT         TYPE SLIS_PRINT_ALV,
  V_REPORT        LIKE SY-REPID,
  T_SORT          TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
  IT_SETLEAF      LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
  ESTRUTURA       TYPE TABLE OF TY_ESTRUTURA,
  VG_I            TYPE I,
  WG_MENSAGEM(30).

DATA: IT_SEL_ROWS TYPE LVC_T_ROW,
      WA_SEL_ROWS TYPE LVC_S_ROW.

DATA: OK-CODE TYPE SY-UCOMM,
      VL_FORM TYPE TDSFNAME,
      VL_NAME TYPE RS38L_FNAM.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: EDITCONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EDITOR          TYPE REF TO CL_GUI_TEXTEDIT,
      CL_CONTAINER_95 TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      CL_CONTAINER_05 TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID   TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID         TYPE REF TO CL_GUI_ALV_GRID,
      WA_STABLE       TYPE LVC_S_STBL,
      WA_AFIELD       TYPE LVC_S_FCAT,
      IT_FIELDCAT     TYPE LVC_T_FCAT,
      W_FIELDCAT      TYPE LVC_S_FCAT,
      I_SORT          TYPE LVC_T_SORT,
      WA_LAYOUT       TYPE LVC_S_LAYO,
      IS_STABLE       TYPE LVC_S_STBL VALUE 'XX',
      WG_REPNAME      LIKE SY-REPID,
      WG_X_VARIANT    LIKE DISVARIANT,
      WG_EXIT(1)      TYPE C,
      WG_SAVE(1)      TYPE C,
      WG_VARIANT      LIKE DISVARIANT,
      GT_F4           TYPE LVC_T_F4 WITH HEADER LINE.

DEFINE MC_PREENCHE_CLASS.
  VG_I = VG_I + 1.
  CLEAR T_SORT.
  T_SORT-SPOS      = VG_I.
  T_SORT-FIELDNAME = &1.
  T_SORT-GROUP     = &2.
  IF &3 = 'D'.
    T_SORT-DOWN        = 'X'.
  ELSE.
    T_SORT-UP          = &3.
  ENDIF.
  T_SORT-SUBTOT    = &4.
  APPEND T_SORT.
END-OF-DEFINITION.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_0               TYPE C VALUE '0',
           C_1               TYPE C VALUE '1',
           C_2               TYPE C VALUE '2',
           C_B               TYPE C VALUE 'B',
           C_S               TYPE C VALUE 'S',
           C_L               TYPE C VALUE 'L',
           C_X               TYPE C VALUE 'X',
           C_D               TYPE C VALUE 'D',
           C_K               TYPE C VALUE 'K',
           C_W               TYPE C VALUE 'W',
           C_F               TYPE C VALUE 'F',
           C_T               TYPE C VALUE 'T',
           C_I               TYPE C VALUE 'I',
           C_N               TYPE C VALUE 'N',
           C_H               TYPE C VALUE 'H',
           C_AG(2)           TYPE C VALUE 'AG',
           C_NE(2)           TYPE C VALUE 'NE',
           C_01(2)           TYPE C VALUE '01',
           C_30(2)           TYPE C VALUE '30',
           C_40(2)           TYPE C VALUE '40',
           C_50(4)           TYPE C VALUE '0050',
           C_76(2)           TYPE C VALUE '76',
           C_71(2)           TYPE C VALUE '71',
           C_72(2)           TYPE C VALUE '72',
           C_BR(2)           TYPE C VALUE 'BR',
           C_LF(2)           TYPE C VALUE 'LF',
           C_LR(2)           TYPE C VALUE 'LR',
           C_Z1(2)           TYPE C VALUE 'Z1',
           C_ADD(3)          TYPE C VALUE 'ADD',
           C_DEL(3)          TYPE C VALUE 'DEL',
           C_DG1(3)          TYPE C VALUE 'DG1',
           C_DG2(3)          TYPE C VALUE 'DG2',
           C_DUMMY_HEADER(3) TYPE C VALUE '099',
           C_DUMMY_ITENS(3)  TYPE C VALUE '098',
           C_EXIT(4)         TYPE C VALUE 'EXIT',
           C_ROOT(4)         TYPE C VALUE 'ROOT',
           C_MINIMIZAR(4)    TYPE C VALUE '@K2@',
           C_MAXIMIZAR(4)    TYPE C VALUE '@K1@',
           C_BACK(4)         TYPE C VALUE 'BACK',
           C_SAVE(4)         TYPE C VALUE 'SAVE',
           C_DESAT(5)        TYPE C VALUE 'DESAT',
           C_DMBTR(5)        TYPE C VALUE 'DMBTR',
           C_MODIF(5)        TYPE C VALUE 'MODIF',
           C_CANCEL(6)       TYPE C VALUE 'CANCEL',
           C_DELDOC(6)       TYPE C VALUE 'DELDOC',
           C_DCLICK(6)       TYPE C VALUE 'DCLICK',
           C_SEARCH(6)       TYPE C VALUE 'SEARCH',
           C_ATUALI(6)       TYPE C VALUE 'ATUALI',
           C_ADD_MSG(7)      TYPE C VALUE 'ADD_MSG',
           C_DEL_MSG(7)      TYPE C VALUE 'DEL_MSG',
           C_CLOS_MSG(8)     TYPE C VALUE 'CLOS_MSG',
           C_SAVE_MSG(8)     TYPE C VALUE 'SAVE_MSG',
           C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:  P_BUKRS  FOR ZLEST0061-BUKRS       NO INTERVALS NO-EXTENSION,
                   P_WERKS  FOR ZLEST0061-WERKS       NO INTERVALS NO-EXTENSION,
                   P_ANO    FOR ZLEST0061-ANO_VIAGEM  NO INTERVALS NO-EXTENSION,
                   P_VIAG   FOR ZLEST0061-NR_VIAGEM   NO INTERVALS NO-EXTENSION,
                   P_SAFRA  FOR ZLEST0061-SAFRA,
                   P_DT_FT  FOR ZLEST0061-DT_FATURA,
                   P_DT_NF  FOR ZLEST0060-DOCDAT,
                   P_CLI    FOR ZLEST0061-CL_CODIGO,
                   P_EMB    FOR ZLEST0061-NOME_EMB  NO INTERVALS.

  PARAMETER: P_C_OV AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN: END OF BLOCK B1.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF ( P_DT_FT-LOW IS INITIAL ) AND ( P_DT_NF-LOW IS INITIAL ).

    IF ( P_BUKRS-LOW IS INITIAL ) OR
       ( P_WERKS-LOW IS INITIAL ) OR
       ( P_ANO-LOW   IS INITIAL ) OR
       ( P_VIAG-LOW  IS INITIAL ).
      MESSAGE 'Informe a Data CTe ou Data NF ou Empresa/Centro Emissor/Ano/Viagem!' TYPE 'S'.
      EXIT.
    ENDIF.

  ENDIF.

  PERFORM:
            F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA, " Form de saida
            F_IMPRIME_DADOS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  REFRESH: IT_ZLEST0060[] , IT_ZLEST0063[], IT_ZLEST0061[], IT_ZLEST0104[].

  "NF's Vinculadas
  SELECT *
    FROM ZLEST0060
    INTO TABLE IT_ZLEST0060
   WHERE BUKRS       IN P_BUKRS
    AND  WERKS       IN P_WERKS
    AND  ANO_VIAGEM  IN P_ANO
    AND  NR_VIAGEM   IN P_VIAG
    AND  NOME_EMB    IN P_EMB
    AND  CL_CODIGO   IN P_CLI
    AND  SAFRA       IN P_SAFRA
    AND  DOCDAT      IN P_DT_NF.

  CHECK IT_ZLEST0060[] IS NOT INITIAL.

  "Dados Comboio
  SELECT *
    FROM ZLEST0063
    INTO TABLE IT_ZLEST0063
     FOR ALL ENTRIES IN IT_ZLEST0060
   WHERE BUKRS       = IT_ZLEST0060-BUKRS
    AND  WERKS       = IT_ZLEST0060-WERKS
    AND  ANO_VIAGEM  = IT_ZLEST0060-ANO_VIAGEM
    AND  NR_VIAGEM   = IT_ZLEST0060-NR_VIAGEM
    AND  NOME_EMB    = IT_ZLEST0060-NOME_EMB.

  CHECK IT_ZLEST0063[] IS NOT INITIAL.

  "OV's geradas
  SELECT *
    FROM ZLEST0061
    INTO TABLE IT_ZLEST0061
      FOR ALL ENTRIES IN IT_ZLEST0060
    WHERE BUKRS         = IT_ZLEST0060-BUKRS
    AND   WERKS         = IT_ZLEST0060-WERKS
    AND   ANO_VIAGEM    = IT_ZLEST0060-ANO_VIAGEM
    AND   NR_VIAGEM     = IT_ZLEST0060-NR_VIAGEM
    AND   SAFRA         = IT_ZLEST0060-SAFRA
    AND   DT_FATURA     IN P_DT_FT
    AND   CL_CODIGO     = IT_ZLEST0060-CL_CODIGO
    AND   AUART         IN ('ZTAG','ZTAB', 'ZTAM', 'ZTAF' )
    AND   NOME_EMB      = IT_ZLEST0060-NOME_EMB
    AND   NR_DCO        = IT_ZLEST0060-NR_DCO
    AND   ID_FRETE_AQUA = IT_ZLEST0060-ID_FRETE_AQUA.

  SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_ZLEST0063
    WHERE SPRAS EQ SY-LANGU
      AND MATNR = IT_ZLEST0063-COD_MATERIAL.

  IF IT_ZLEST0061[] IS NOT INITIAL.

    SELECT DOCNUM NFENUM
      FROM J_1BNFDOC
      INTO TABLE IT_J_1BNFDOC
      FOR ALL ENTRIES IN IT_ZLEST0061
      WHERE DOCNUM = IT_ZLEST0061-DOCNUM.

  ENDIF.

  LOOP AT IT_ZLEST0060 INTO WA_ZLEST0060.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_ZLEST0060-CL_CODIGO
      IMPORTING
        OUTPUT = WA_ZLEST0060-CL_CODIGO.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_ZLEST0060-DT_CODIGO
      IMPORTING
        OUTPUT = WA_ZLEST0060-DT_CODIGO.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_ZLEST0060-RM_CODIGO
      IMPORTING
        OUTPUT = WA_ZLEST0060-RM_CODIGO.

    MODIFY IT_ZLEST0060 FROM WA_ZLEST0060 TRANSPORTING CL_CODIGO.
  ENDLOOP.

  SELECT  KUNNR STCD1
    FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_ZLEST0060
    WHERE KUNNR	=	 IT_ZLEST0060-CL_CODIGO.

  SELECT  LIFNR STCD1
    FROM LFA1
    INTO TABLE IT_LFA1
    FOR ALL ENTRIES IN IT_ZLEST0060
    WHERE LIFNR	=	 IT_ZLEST0060-CL_CODIGO.

  SELECT  KUNNR STCD1
    FROM KNA1 APPENDING TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_ZLEST0060
    WHERE KUNNR	=	 IT_ZLEST0060-DT_CODIGO.

  SELECT  LIFNR STCD1
    FROM LFA1 APPENDING TABLE IT_LFA1
    FOR ALL ENTRIES IN IT_ZLEST0060
    WHERE LIFNR	=	 IT_ZLEST0060-RM_CODIGO.

  SELECT  *
    FROM ZLEST0104 INTO TABLE IT_ZLEST0104
    FOR ALL ENTRIES IN IT_ZLEST0060
    WHERE EMISSOR	=	 IT_ZLEST0060-WERKS.

  SORT IT_ZLEST0104 BY EMISSOR.

  LOOP AT IT_ZLEST0060 INTO WA_ZLEST0060.
    DATA(_TABIX) = SY-TABIX.

    CHECK ( WA_ZLEST0060-BUKRS_ROM  IS INITIAL ) OR
          ( WA_ZLEST0060-BRANCH_ROM IS INITIAL ).

    READ TABLE IT_ZLEST0104 INTO WA_ZLEST0104 WITH KEY EMISSOR = WA_ZLEST0060-WERKS BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_ZLEST0060-BUKRS_ROM   = WA_ZLEST0104-BUKRS.
      WA_ZLEST0060-BRANCH_ROM  = WA_ZLEST0104-BRANCH.
      MODIFY IT_ZLEST0060 FROM WA_ZLEST0060 INDEX _TABIX.
    ENDIF.
  ENDLOOP.

  SELECT * FROM ZSDT0001
    INTO TABLE IT_ZSDT0001
    FOR ALL ENTRIES IN IT_ZLEST0060
  WHERE BUKRS       EQ IT_ZLEST0060-BUKRS_ROM
    AND NR_SAFRA    EQ IT_ZLEST0060-SAFRA
    AND NR_ROMANEIO EQ IT_ZLEST0060-NR_ROMANEIO
    AND NFNUM       EQ IT_ZLEST0060-NFNUM.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .

  SORT:  IT_MAKT        BY MATNR,
         IT_J_1BNFDOC   BY DOCNUM,
         IT_KNA1        BY KUNNR,
         IT_LFA1        BY LIFNR,
         IT_ZLEST0104   BY EMISSOR,
         IT_ZLEST0060   BY BUKRS WERKS ANO_VIAGEM NR_VIAGEM NOME_EMB CL_CODIGO SAFRA NR_DCO.

  LOOP AT IT_ZLEST0060 INTO WA_ZLEST0060.

    CLEAR: WA_SAIDA, WA_ZLEST0061, WA_ZLEST0063, WA_J_1BNFDOC, WA_MAKT, WA_ZSDT0001, WA_ZLEST0104.

    READ TABLE IT_ZLEST0061 INTO WA_ZLEST0061 WITH KEY BUKRS         = WA_ZLEST0060-BUKRS
                                                       WERKS         = WA_ZLEST0060-WERKS
                                                       ANO_VIAGEM    = WA_ZLEST0060-ANO_VIAGEM
                                                       NR_VIAGEM     = WA_ZLEST0060-NR_VIAGEM
                                                       NOME_EMB      = WA_ZLEST0060-NOME_EMB
                                                       CL_CODIGO     = WA_ZLEST0060-CL_CODIGO
                                                       SAFRA         = WA_ZLEST0060-SAFRA
                                                       NR_DCO        = WA_ZLEST0060-NR_DCO
                                                       OPERACAO      = WA_ZLEST0060-OPERACAO
                                                       ID_FRETE_AQUA = WA_ZLEST0060-ID_FRETE_AQUA.


    IF ( SY-SUBRC NE 0 ).
      IF P_C_OV IS NOT INITIAL.  "Somente NF's com CT-e.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE IT_ZLEST0063 INTO WA_ZLEST0063 WITH KEY BUKRS       = WA_ZLEST0060-BUKRS
                                                       WERKS       = WA_ZLEST0060-WERKS
                                                       ANO_VIAGEM  = WA_ZLEST0060-ANO_VIAGEM
                                                       NR_VIAGEM   = WA_ZLEST0060-NR_VIAGEM
                                                       NOME_EMB    = WA_ZLEST0060-NOME_EMB.

    CHECK SY-SUBRC = 0.

    WA_SAIDA-BUKRS            = WA_ZLEST0060-BUKRS. "EMPRESA
    WA_SAIDA-WERKS            = WA_ZLEST0060-WERKS. "CENTRO
    WA_SAIDA-ANO_VIAGEM       = WA_ZLEST0060-ANO_VIAGEM. "ANO
    WA_SAIDA-NR_VIAGEM        = WA_ZLEST0060-NR_VIAGEM. "VIAGEM
    WA_SAIDA-CL_CODIGO        = WA_ZLEST0060-CL_CODIGO. " COD. CLIENTE
    WA_SAIDA-RM_CODIGO        = WA_ZLEST0060-RM_CODIGO. " COD. CLIENTE
    WA_SAIDA-OPERACAO         = WA_ZLEST0060-OPERACAO.
    "ZLES0082 - Adicionar Coluna Atende EUDR - BG #156128 - INCIO
    CASE WA_ZLEST0060-EUDR.
      WHEN 'S'.
        WA_SAIDA-EUDR = 'S-Atende'.
      WHEN 'N'.
        WA_SAIDA-EUDR = 'N-Não Atende'.
      WHEN OTHERS.
    ENDCASE.
    "ZLES0082 - Adicionar Coluna Atende EUDR - BG #156128 - FIM
    CASE WA_ZLEST0060-TOMADOR_SERV.
      WHEN: 'R'.
        READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_ZLEST0060-RM_CODIGO BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-STCD1            = WA_LFA1-STCD1. " CNPJ FORNECEDOR
        ENDIF.
      WHEN: 'D'.
        READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_ZLEST0060-DT_CODIGO BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-STCD1            = WA_KNA1-STCD1. " CNPJ CLIENTE
        ENDIF.
    ENDCASE.

    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_ZLEST0060-RM_CODIGO BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA-STCD1_RM       = WA_LFA1-STCD1. " CNPJ FORNECEDOR
    ENDIF.

    WA_SAIDA-DT_FATURA        = WA_ZLEST0061-DT_FATURA. "DATA
    WA_SAIDA-DOCNUM           = WA_ZLEST0061-DOCNUM. "Doc.Num CT-e.
    WA_SAIDA-CK_ANULADO       = WA_ZLEST0061-CK_ANULADO. "Documento Anulado.

    READ TABLE IT_J_1BNFDOC INTO WA_J_1BNFDOC WITH KEY DOCNUM = WA_ZLEST0061-DOCNUM BINARY SEARCH.
    WA_SAIDA-NFENUM           = WA_J_1BNFDOC-NFENUM. " DACTE

    WA_SAIDA-NOME_EMB         = WA_ZLEST0063-NOME_EMB. "BARCAÇA
    WA_SAIDA-NFNUM            = WA_ZLEST0060-NFNUM. "NR. NF
    WA_SAIDA-DOCDAT           = WA_ZLEST0060-DOCDAT. "DATA NF
    WA_SAIDA-DOC_REM          = WA_ZLEST0060-DOC_REM. "Remessa
    WA_SAIDA-PESO_FISCAL      = WA_ZLEST0060-PESO_FISCAL. "PESO FISCAL
    WA_SAIDA-NETWR            = WA_ZLEST0060-NETWR. "VALOR NF
    WA_SAIDA-PESO_LIQ_RET     = WA_ZLEST0060-PESO_LIQ_RET.
    WA_SAIDA-VLR_LIQ_RET      = WA_ZLEST0060-VLR_LIQ_RET.
    WA_SAIDA-NR_ROMANEIO      = WA_ZLEST0060-NR_ROMANEIO. "ROMANEIO

    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_ZLEST0063-COD_MATERIAL BINARY SEARCH.
    WA_SAIDA-MAKTX            = WA_MAKT-MAKTX. "PRODUTO

    WA_SAIDA-SAFRA            = WA_ZLEST0060-SAFRA.
    WA_SAIDA-NR_DCO           = WA_ZLEST0060-NR_DCO.

    READ TABLE IT_ZSDT0001 INTO WA_ZSDT0001 WITH KEY  BUKRS       = WA_ZLEST0060-BUKRS_ROM
                                                      BRANCH      = WA_ZLEST0060-BRANCH_ROM
                                                      NR_SAFRA    = WA_ZLEST0060-SAFRA
                                                      NR_ROMANEIO = WA_ZLEST0060-NR_ROMANEIO
                                                      NFNUM       = WA_ZLEST0060-NFNUM.

    WA_SAIDA-PESO_SUBTOTAL = WA_ZSDT0001-PESO_SUBTOTAL.
    WA_SAIDA-PESO_LIQ      = WA_ZSDT0001-PESO_LIQ.

    IF ( WA_ZLEST0060-PESO_LIQ_RET IS INITIAL ) AND ( WA_ZSDT0001-PESO_LIQRET_EST IS NOT INITIAL ) AND
       ( WA_ZSDT0001-PESO_FISCAL > 0 ).

      WA_SAIDA-NETPR          = WA_ZSDT0001-NETWR / WA_ZSDT0001-PESO_FISCAL.
      WA_SAIDA-PERC_VINC      = WA_ZLEST0060-PESO_FISCAL / WA_ZSDT0001-PESO_FISCAL.
      WA_SAIDA-PESO_LIQ_RET   = WA_SAIDA-PERC_VINC * WA_ZSDT0001-PESO_LIQRET_EST.
      WA_SAIDA-VLR_LIQ_RET    = WA_SAIDA-PESO_LIQ_RET * WA_SAIDA-NETPR.
    ENDIF.

    WA_SAIDA-CHAVE_NFE     = WA_ZLEST0060-CHAVE_NFE.
    WA_SAIDA-SERIES        = WA_ZLEST0060-SERIES.

    CASE WA_ZSDT0001-TP_TRANSGENIA.
      WHEN 'CO'.
        WA_SAIDA-TP_MAT = 'CO'.
      WHEN 'R1' OR 'R2'.
        WA_SAIDA-TP_MAT = 'RR'.
      WHEN OTHERS.
    ENDCASE.

    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR WA_SAIDA.

  ENDLOOP.

ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .
  PERFORM F_ALV_FIELDCAT.
  PERFORM F_ALV_SORT.

  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.
  WA_LAYOUT-GRID_TITLE = 'Notas Fiscais'.
  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT   = 'X'.

  CALL SCREEN 0100.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .

  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NR_VIAGEM'.
  WA_AFIELD-SCRTEXT_S = 'VIAGEM'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CL_CODIGO'.
  WA_AFIELD-SCRTEXT_S = 'COD. CLIENTE'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'STCD1'.
  WA_AFIELD-SCRTEXT_S = 'CNPJ CLIENTE'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RM_CODIGO'.
  WA_AFIELD-SCRTEXT_S = 'COD. REMETENTE'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'STCD1_RM'.
  WA_AFIELD-SCRTEXT_S = 'CNPJ REMETENTE'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_FATURA'.
  WA_AFIELD-SCRTEXT_S = 'DATA'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DOCNUM'.
  WA_AFIELD-SCRTEXT_S     = 'DOCNUM.CTE'.
  WA_AFIELD-SCRTEXT_L     = 'DOCNUM.CTE'.
  WA_AFIELD-SCRTEXT_M     = 'DOCNUM.CTE'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NFENUM'.
  WA_AFIELD-SCRTEXT_S = 'DACTE'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NOME_EMB'.
  WA_AFIELD-SCRTEXT_S = 'BARCAÇA'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NFNUM'.
  WA_AFIELD-SCRTEXT_S = 'NR. NF'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DOCDAT'.
  WA_AFIELD-SCRTEXT_S = 'DATA NF'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DOC_REM'.
  WA_AFIELD-SCRTEXT_S     = 'REMESSA'.
  WA_AFIELD-SCRTEXT_L     = 'REMESSA'.
  WA_AFIELD-SCRTEXT_M     = 'REMESSA'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'PESO_FISCAL'.
  WA_AFIELD-SCRTEXT_S = 'PESO FISCAL'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NETWR'.
  WA_AFIELD-SCRTEXT_S = 'VALOR NF'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'PESO_LIQ_RET'.
  WA_AFIELD-SCRTEXT_S     = 'PESO LIQ.RET.'.
  WA_AFIELD-SCRTEXT_L     = 'PESO LIQ.RET.'.
  WA_AFIELD-SCRTEXT_M     = 'PESO LIQ.RET.'.
  WA_AFIELD-OUTPUTLEN     = 13.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_LIQ_RET'.
  WA_AFIELD-SCRTEXT_S     = 'VLR.LIQ.RET'.
  WA_AFIELD-SCRTEXT_L     = 'VLR.LIQ.RET'.
  WA_AFIELD-SCRTEXT_M     = 'VLR.LIQ.RET'.
  WA_AFIELD-OUTPUTLEN     = 13.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NR_ROMANEIO'.
  WA_AFIELD-SCRTEXT_S = 'ROMANEIO'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MAKTX'.
  WA_AFIELD-SCRTEXT_S = 'PRODUTO'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SAFRA'.
  WA_AFIELD-SCRTEXT_S = 'SAFRA'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NR_DCO'.
  WA_AFIELD-SCRTEXT_S = 'DCO'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'PESO_SUBTOTAL'.
  WA_AFIELD-SCRTEXT_S = 'Peso SubTotal'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'PESO_LIQ'.
  WA_AFIELD-SCRTEXT_S = 'Peso Liq.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'CK_ANULADO'.
  WA_AFIELD-SCRTEXT_S = 'Anulado'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN     = 10.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'CHAVE_NFE'.
  WA_AFIELD-SCRTEXT_S = 'Chave NFe'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN     = 45.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'SERIES'.
  WA_AFIELD-SCRTEXT_S = 'Serie'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN     = 05.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'TP_MAT'.
  WA_AFIELD-SCRTEXT_S = 'Tp.Mat.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 05.
  WA_AFIELD-EDIT      = ''.
  WA_AFIELD-KEY       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'OPERACAO'.
  WA_AFIELD-SCRTEXT_S = 'Operação'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 08.
  WA_AFIELD-EDIT      = ''.
  WA_AFIELD-KEY       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.
"ZLES0082 - Adicionar Coluna Atende EUDR - BG #156128 - INICIO
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'EUDR'.
  WA_AFIELD-SCRTEXT_S = 'EUDR'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 12.
  WA_AFIELD-EDIT      = ''.
  WA_AFIELD-KEY       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.
"ZLES0082 - Adicionar Coluna Atende EUDR - BG #156128  - FIM

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.

  SET PF-STATUS 'F_SET_PF' EXCLUDING FCODE.
  SET TITLEBAR  'ZFTITLE'.


  IF CL_CONTAINER_95 IS INITIAL.
    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        SIDE  = '4'
        RATIO = '80'.
  ENDIF.

  IF NOT CL_GRID IS INITIAL.

    PERFORM ZF_ALV_HEADER.
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
        NO_MARGINS = 'X'.

    PERFORM ZF_ALV_HEADER .


    IF EDITCONTAINER IS INITIAL .
      CREATE OBJECT EDITCONTAINER
        EXPORTING
          CONTAINER_NAME = 'HEADER'.
    ENDIF .

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.


    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT = CL_CONTAINER_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    WG_SAVE = 'X'.
    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    WA_STABLE-ROW        = C_X.
    WG_X_VARIANT-REPORT  = SY-REPID.
    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = WG_X_VARIANT
        IS_LAYOUT       = WA_LAYOUT
        I_SAVE          = WG_SAVE
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
        IT_SORT         = I_SORT[]
        IT_OUTTAB       = IT_SAIDA[].

  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH IT_SAIDA.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'RELA'.
      PERFORM F_IMPRIME_SMART USING ''.
    WHEN 'RELA2'.
      PERFORM F_IMPRIME_SMART USING '1'.
    WHEN 'RESUMO_EXP'.
      PERFORM F_RESUMO_EXPEDICAO.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALV_HEADER .

  DATA:   WL_DATA(10),
               WL_HORA(8),
               WL_LINHA(60),
               WL_TEXT TYPE SDYDO_TEXT_ELEMENT.


  IF P_BUKRS   IS NOT INITIAL.
    CONCATENATE 'Empresa  :' P_BUKRS-LOW
      INTO WL_LINHA SEPARATED BY SPACE.

    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_WERKS   IS NOT INITIAL.
    CONCATENATE 'Centro Emissor :' P_WERKS-LOW
      INTO WL_LINHA SEPARATED BY SPACE.

    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_ANO   IS NOT INITIAL.
    CONCATENATE 'Ano :' P_ANO-LOW
      INTO WL_LINHA SEPARATED BY SPACE.

    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.


  IF P_VIAG  IS NOT INITIAL.
    CONCATENATE 'Viagem :' P_VIAG-LOW
      INTO WL_LINHA SEPARATED BY SPACE.

    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.


  IF P_CLI  IS NOT INITIAL.
    IF P_CLI-HIGH IS INITIAL.
      CONCATENATE 'Cliente:' P_CLI-LOW
        INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Cliente :' P_CLI-LOW 'à' P_CLI-HIGH
  INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.

    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.


ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_SORT .
  CLEAR VG_I.
  REFRESH T_SORT.
  MC_PREENCHE_CLASS:  'NR_VIAGEM'        '' 'X' 'X',
                      'CL_CODIGO'        '' 'X' 'X',
                      'STCD1'            '' 'X' ' ',
                      'DT_FATURA'        '' 'X' ' ',
                      'NFENUM'           '' 'X' ' ',
                      'NOME_EMB'         '' 'X' ' '.
ENDFORM.                    " F_ALV_SORT


*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_SMART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALV      text
*----------------------------------------------------------------------*
FORM  F_IMPRIME_SMART USING P_TIPO TYPE C.
  DATA: LS_CONTROL        TYPE SSFCTRLOP,
        LS_OPTIONS        TYPE SSFCOMPOP,
        JOB_OUTPUT_INFO   TYPE SSFCRESCL,
        LS_XSFPARAM_LINE  TYPE SSFXSFP,
        V_BIN_FILESIZE    TYPE I,
        IT_DOCS           TYPE STANDARD TABLE OF DOCS,
        IT_LINES          TYPE STANDARD TABLE OF TLINE,
        LV_FNAME          TYPE RS38L_FNAM,
        LV_MAIL_RECIPIENT TYPE SWOTOBJID,
        LV_MAIL_SENDER    TYPE SWOTOBJID,
        LV_CONTROL        TYPE SSFCTRLOP,
        LV_NAME           TYPE SO_NAME,
        LV_OUTPUT         TYPE SSFCOMPOP,
        WL_ZMENG(20),
        WL_DMBTR(20),
        WL_VLRTOT(20).

  DATA: I_OTF       TYPE ITCOO OCCURS 0 WITH HEADER LINE,
        I_TLINE     TYPE TABLE OF TLINE WITH HEADER LINE,
        I_RECEIVERS TYPE TABLE OF SOMLRECI1 WITH HEADER LINE,
        I_RECORD    LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        I_OBJPACK   LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
        I_OBJTXT    LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        I_OBJBIN    LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        I_RECLIST   LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
        WA_OBJHEAD  TYPE SOLI_TAB,
        W_CTRLOP    TYPE SSFCTRLOP,
        W_COMPOP    TYPE SSFCOMPOP,
        W_RETURN    TYPE SSFCRESCL,
        WA_DOC_CHNG TYPE SODOCCHGI1,
        W_DATA      TYPE SODOCCHGI1,
        WA_BUFFER   TYPE STRING, "To convert from 132 to 255
* Variables declarations
        V_FORM_NAME TYPE RS38L_FNAM,
        V_LEN_IN    LIKE SOOD-OBJLEN,
        V_LEN_OUT   LIKE SOOD-OBJLEN,
        V_LEN_OUTN  TYPE I,
        V_LINES_TXT TYPE I,
        V_LINES_BIN TYPE I.

  VL_FORM = 'ZLES0001'.
*
  DATA: W_BUKRS  TYPE T001-BUKRS,  "Empresa
        W_WERKS  TYPE T001W-WERKS, "Centro Emissor
        W_ANO    TYPE ZLEST0056-ANO_VIAGEM, "Ano da Viagem
        W_VIAGEM TYPE ZLEST0058-NR_VIAGEM.  "Número da Viagem

  CALL METHOD CL_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  IF IT_SEL_ROWS[] IS INITIAL.
    MESSAGE S836(SD) WITH 'Selecione uma linha!' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  IF LINES( IT_SEL_ROWS ) NE 1.
    MESSAGE S836(SD) WITH 'selecione somente uma linha!' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WA_SEL_ROWS-INDEX.

  CHECK SY-SUBRC = 0.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = VL_FORM
    IMPORTING
      FM_NAME            = VL_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  Impresora
  LS_CONTROL-NO_DIALOG = ' '. "Evita la pantalla de opciones de salida del formulario
  LS_OPTIONS-TDDEST   = 'LOCL'.
  LS_OPTIONS-TDIMMED  = C_X.
  LS_OPTIONS-TDNEWID  = C_X.
  LS_OPTIONS-TDNOARCH = C_X.

  LS_CONTROL-PREVIEW = SPACE.
  LS_CONTROL-DEVICE  = 'PRINTER'.
  LS_CONTROL-GETOTF  = ' '.

  W_BUKRS     = WA_SAIDA-BUKRS.
  W_WERKS     = WA_SAIDA-WERKS.
  W_ANO       = WA_SAIDA-ANO_VIAGEM.
  W_VIAGEM    = WA_SAIDA-NR_VIAGEM.

  CLEAR:JOB_OUTPUT_INFO.
  CALL FUNCTION VL_NAME
    EXPORTING
      USER_SETTINGS      = ' '
      CONTROL_PARAMETERS = LS_CONTROL
      OUTPUT_OPTIONS     = LS_OPTIONS
      I_BUKRS            = W_BUKRS
      I_WERKS            = W_WERKS
      I_ANO_VIAGEM       = W_ANO
      I_NR_VIAGEM        = W_VIAGEM
      I_TP_AGRUP         = P_TIPO
    IMPORTING
      JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " F_IMPRIME_SMART4

FORM F_RESUMO_EXPEDICAO.

  DATA: T_DADOS_COMBOIO TYPE TABLE OF ZDADOS_COMBOIO_AQUAV WITH HEADER LINE,
        T_DADOS_NF_VINC TYPE TABLE OF ZDADOS_NF_VINC_AQUAV WITH HEADER LINE.

  DATA: W_BUKRS  TYPE T001-BUKRS,  "Empresa
        W_WERKS  TYPE T001W-WERKS, "Centro Emissor
        W_ANO    TYPE ZLEST0056-ANO_VIAGEM, "Ano da Viagem
        W_VIAGEM TYPE ZLEST0058-NR_VIAGEM.  "Número da Viagem

  CALL METHOD CL_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  IF IT_SEL_ROWS[] IS INITIAL.
    MESSAGE S836(SD) WITH 'Selecione uma linha!' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  IF LINES( IT_SEL_ROWS ) NE 1.
    MESSAGE S836(SD) WITH 'selecione somente uma linha!' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WA_SEL_ROWS-INDEX.

  CHECK SY-SUBRC = 0.

  W_BUKRS     = WA_SAIDA-BUKRS.
  W_WERKS     = WA_SAIDA-WERKS.
  W_ANO       = WA_SAIDA-ANO_VIAGEM.
  W_VIAGEM    = WA_SAIDA-NR_VIAGEM.

  CALL FUNCTION 'ZLES_RESUMO_VIAGEM_AQUAVIARIO'
    EXPORTING
      I_BUKRS         = W_BUKRS
      I_WERKS         = W_WERKS
      I_ANO_VIAGEM    = W_ANO
      I_NR_VIAGEM     = W_VIAGEM
    TABLES
      T_DADOS_COMBOIO = T_DADOS_COMBOIO
      T_DADOS_NF_VINC = T_DADOS_NF_VINC.

  CHECK T_DADOS_NF_VINC[] IS NOT INITIAL.

  LOOP AT T_DADOS_NF_VINC.
    CLEAR: WA_SAIDA_0120.
    READ TABLE T_DADOS_COMBOIO WITH KEY BUKRS       = T_DADOS_NF_VINC-BUKRS
                                        WERKS       = T_DADOS_NF_VINC-WERKS
                                        ANO_VIAGEM  = T_DADOS_NF_VINC-ANO_VIAGEM
                                        NR_VIAGEM   = T_DADOS_NF_VINC-NR_VIAGEM
                                        NOME_EMB    = T_DADOS_NF_VINC-NOME_EMB.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING T_DADOS_COMBOIO TO WA_SAIDA_0120.
    ENDIF.

    MOVE-CORRESPONDING T_DADOS_NF_VINC TO WA_SAIDA_0120.

    APPEND WA_SAIDA_0120 TO IT_SAIDA_0120.
  ENDLOOP.

  CALL SCREEN 0120 STARTING AT 2 2 ENDING AT 178 25.

ENDFORM.

MODULE USER_COMMAND_0120 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE STATUS_0120 OUTPUT.
  SET PF-STATUS 'PF0120'.
  SET TITLEBAR 'T0120'.
ENDMODULE.


MODULE PBO_0120 OUTPUT.

  IF OBJ_CONTAINER_0120 IS INITIAL.

    PERFORM F_REFRESH_OBJETOS.
    PERFORM F_CRIAR_CATALOG USING '0120'.

    CREATE OBJECT OBJ_CONTAINER_0120
      EXPORTING
        CONTAINER_NAME = 'CC_ALV_0120'.

    CREATE OBJECT OBJ_ALV_0120
      EXPORTING
        I_PARENT = OBJ_CONTAINER_0120.

*    CREATE OBJECT OBJ_TOOLBAR_0120
*      EXPORTING
*        IO_ALV_GRID = OBJ_ALV_0120.

    GS_LAYOUT-SEL_MODE   = 'A'.
    "GS_VARIANT-REPORT  = SY-REPID.
    WA_STABLE-ROW         = 'X'.
    WA_STABLE-COL         = 'X'.

    PERFORM F_EXCLUDE_FCODE USING '0120'.

    CALL METHOD OBJ_ALV_0120->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        IS_VARIANT           = GS_VARIANT
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCAT
        IT_OUTTAB            = IT_SAIDA_0120.


    CALL METHOD OBJ_ALV_0120->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD OBJ_ALV_0120->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  ELSE.
    CALL METHOD OBJ_ALV_0120->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDMODULE.

FORM F_REFRESH_OBJETOS .

  CLEAR: GS_LAYOUT,
         GS_VARIANT.

  REFRESH: IT_EXCLUDE_FCODE.

ENDFORM.



FORM F_CRIAR_CATALOG USING P_SCREEN.

  FREE: W_FIELDCAT, IT_FIELDCAT.

  CASE P_SCREEN.
    WHEN '0120'.

      PERFORM F_ESTRUTURA_ALV USING:

         01  'ZDADOS_COMBOIO_AQUAV'   'NOME_EMB'       'IT_SAIDA_0120' 'NOME_EMB'       'Nome.Emb.'       '20'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         02  'ZDADOS_COMBOIO_AQUAV'   'MATNR'          'IT_SAIDA_0120' 'MATNR'          'Material'        '10'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         03  'ZDADOS_COMBOIO_AQUAV'   'MAKTX'          'IT_SAIDA_0120' 'MAKTX'          'Ds.Material'     '20'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         04  'ZDADOS_COMBOIO_AQUAV'   'DS_TP_CLASS'    'IT_SAIDA_0120' 'DS_TP_CLASS'    'Class.Prod.'     '15'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         05  'ZDADOS_NF_VINC_AQUAV'   'DT_MOVIMENTO'   'IT_SAIDA_0120' 'DT_MOVIMENTO'   'Dt.Movimento'    '12'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         06  'ZDADOS_NF_VINC_AQUAV'   'PESO_VINCULADO' 'IT_SAIDA_0120' 'PESO_VINCULADO' 'Peso Vinc.'      '13'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         07  'ZDADOS_NF_VINC_AQUAV'   'NR_DCO'         'IT_SAIDA_0120' 'NR_DCO'         'Nr.DCO'          '08'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         08  'ZDADOS_NF_VINC_AQUAV'   'SAFRA'          'IT_SAIDA_0120' 'SAFRA'          'Safra'           '05'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         09  'ZDADOS_NF_VINC_AQUAV'   'RM_CODIGO'      'IT_SAIDA_0120' 'RM_CODIGO'      'Código Remet.'   '07'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         10  'ZDADOS_NF_VINC_AQUAV'   'NAME1_REME'     'IT_SAIDA_0120' 'NAME1_REME'     'Desc. Remet.'    '25'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         11  'ZDADOS_NF_VINC_AQUAV'   'STCD1_REME'     'IT_SAIDA_0120' 'STCD1_REME'     'CNPJ Remet.'     '15'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         12  'ZDADOS_NF_VINC_AQUAV'   'DT_CODIGO'      'IT_SAIDA_0120' 'DT_CODIGO'      'Código Dest.'    '07'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         13  'ZDADOS_NF_VINC_AQUAV'   'NAME1_DEST'     'IT_SAIDA_0120' 'NAME1_DEST'     'Desc. Dest.'     '25'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         14  'ZDADOS_NF_VINC_AQUAV'   'STCD1_DEST'     'IT_SAIDA_0120' 'STCD1_DEST'     'CNPJ Dest.'      '15'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         15  'ZDADOS_NF_VINC_AQUAV'   'STATUS_OV'      'IT_SAIDA_0120' 'STATUS_OV'      'O.V Ger.'        '04'   ' '    '' ' ' ' ' ' ' ' ' ' ',
         16  'ZDADOS_NF_VINC_AQUAV'   'DOCNUM'         'IT_SAIDA_0120' 'DOCNUM'         'Docnum CT-e'     '11'   ' '    '' ' ' ' ' ' ' ' ' ' '.
  ENDCASE.

ENDFORM.


FORM F_ESTRUTURA_ALV USING VALUE(P_COL_POS)       TYPE I
                           VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                           VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                           VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                           VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                           VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                           VALUE(P_OUTPUTLEN)
                           VALUE(P_EDIT)
                           VALUE(P_SUM)
                           VALUE(P_EMPHASIZE)
                           VALUE(P_JUST)
                           VALUE(P_HOTSPOT)
                           VALUE(P_F4)
                           VALUE(P_CHECKBOX).

  CLEAR W_FIELDCAT.

  W_FIELDCAT-FIELDNAME   = P_FIELD.
  W_FIELDCAT-TABNAME     = P_TABNAME.
  W_FIELDCAT-REF_TABLE   = P_REF_TABNAME.
  W_FIELDCAT-REF_FIELD   = P_REF_FIELDNAME.
  W_FIELDCAT-KEY         = ' '.
  W_FIELDCAT-EDIT        = P_EDIT.
  W_FIELDCAT-COL_POS     = P_COL_POS.
  W_FIELDCAT-OUTPUTLEN   = P_OUTPUTLEN.
  W_FIELDCAT-NO_OUT      = ' '.
  W_FIELDCAT-REPTEXT     = P_SCRTEXT_L.
  W_FIELDCAT-SCRTEXT_S   = P_SCRTEXT_L.
  W_FIELDCAT-SCRTEXT_M   = P_SCRTEXT_L.
  W_FIELDCAT-SCRTEXT_L   = P_SCRTEXT_L.
  W_FIELDCAT-EMPHASIZE   = P_EMPHASIZE.
  W_FIELDCAT-STYLE       =
  W_FIELDCAT-JUST        = P_JUST.
  W_FIELDCAT-HOTSPOT     = P_HOTSPOT.
  W_FIELDCAT-F4AVAILABL  = P_F4.
  W_FIELDCAT-CHECKBOX    = P_CHECKBOX.

  APPEND W_FIELDCAT TO IT_FIELDCAT.

ENDFORM.


FORM F_EXCLUDE_FCODE USING P_SCREEN.

  APPEND CL_GUI_ALV_GRID=>MC_FC_REFRESH           TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_CHECK             TO IT_EXCLUDE_FCODE.

ENDFORM.
