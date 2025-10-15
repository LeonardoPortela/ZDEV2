*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*| Descrição: Taxa de Elevação                                               |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Victor Hugo Souza Nunes ( victor.hugo@grupomaggi.com.br )            |*
*|                                                                           |*
*|  Tester:                                                                  |*
*|    + Marcos Santos ( marcos.santos@grupomaggi.com.br )                    |*
*|  Changelog:                                                               |*
*|                                                                           |*
*/===========================================================================\*

REPORT  ZFIR0059.

TABLES: ZIB_CONTABIL. "Tabela de Registros de Geração de Documentos Z_FI_MANYDOCUME

TYPES: BEGIN OF TY_SAIDA,
         OBJ_KEY   TYPE ZIB_CONTABIL-OBJ_KEY,   "Chave referência
         BELNR     TYPE ZIB_CONTABIL_CHV-BELNR, "Nº documento de um documento contábil
         INVOICE   TYPE ZFIT0036-INVOICE,       "Invoice
         BUDAT     TYPE ZIB_CONTABIL-BUDAT,     "Data de lançamento no documento
         ZFBDT     TYPE ZIB_CONTABIL-ZFBDT,     "Data base para cálculo do vencimento
         DMBTR     TYPE ZIB_CONTABIL-DMBTR,     "Montante em moeda interna
         TX_CAMBIO TYPE ZIB_CONTABIL-DMBTR,     "Montante em moeda interna
         TX_CURVA  TYPE KURRF,                  "Taxa Curva
         HKONT     TYPE ZIB_CONTABIL-HKONT,     "Conta do Razão da contabilidade geral
         NAME1     TYPE LFA1-NAME1,             "Descrição/Nome do fornecedor.
       END OF TY_SAIDA.

**********************************
*  TABELAS INTERNAS.
**********************************
DATA: GT_ZFIT0036         TYPE TABLE OF ZFIT0036,         "INVOICES - Contas a Pagar
      GT_ZIB_CONTABIL     TYPE TABLE OF ZIB_CONTABIL,     "Tabela de Registros de Geração de Documentos Z_FI_MANYDOCUME
      GT_ZIB_CONTABIL_CHV TYPE TABLE OF ZIB_CONTABIL_CHV, "Tabela de controle de Chave de referência
      GT_LFA1             TYPE TABLE OF LFA1,             "Mestre de fornecedores (parte geral)
      GT_SAIDA            TYPE TABLE OF TY_SAIDA,         "Estrutura de Saída,
      GT_FIELDCATALOG     TYPE LVC_T_FCAT.                "Catálogo de campos para controle visor de listas

**********************************
* WORK AREA.
**********************************
DATA: GW_ZFIT0036         TYPE ZFIT0036,        "INVOICES - Contas a Pagar
      GW_ZIB_CONTABIL     TYPE ZIB_CONTABIL,    "Tabela de Registros de Geração de Documentos Z_FI_MANYDOCUME
      GW_ZIB_CONTABIL_CHV TYPE ZIB_CONTABIL_CHV,"Tabela de controle de Chave de referência
      GW_LFA1             TYPE LFA1,            "Mestre de fornecedores (parte geral)
      GW_SAIDA            TYPE TY_SAIDA,        "Estrutura de Saída.
      GW_FIELDCATALOG     TYPE LVC_S_FCAT.      "Controle VLA: catálogo de campos



**********************************
* CLASSES
**********************************
DATA: GOBJ_ZCL_WEBSERVICE_TX_CURVA TYPE REF TO ZCL_WEBSERVICE_TX_CURVA. "Classe-Webservice para recuperar a taxa curva.

DATA: OBJ_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "Container fuer Custom Controls in der Dynpro Area
      OBJ_ALV       TYPE REF TO CL_GUI_ALV_GRID.         "ALV List Viewer


**********************************
* CONSTANTES
**********************************
CONSTANTS: C_61 TYPE C LENGTH 2 VALUE '61', "Invoice 61
           C_31 TYPE C LENGTH 2 VALUE '31', "Chave de Lançamento
           C_A  TYPE C LENGTH 1 VALUE 'A'. "I_SAVE = 'A' (Habilitado).

CONSTANTS: TELA_0100  TYPE SY-DYNNR VALUE '0100',  "Tela 0100 para apresentação do ALV.
           C_BACK     TYPE SY-UCOMM VALUE 'BACK',  "Botão de Voltar para tela anterior.
           C_CANCEL   TYPE SY-UCOMM VALUE 'CANC',  "Botão de Cancelar e voltar uma tela anterior.
           C_EXIT     TYPE SY-UCOMM VALUE 'EXIT'.  "Botão para sair do programa.
**********************************
* VARIAVEIS
**********************************
DATA: VAR_DATA TYPE DATUM. "Data para o WebService

**********************************
* Tela de Seleção.
**********************************
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS  FOR ZIB_CONTABIL-BUKRS OBLIGATORY NO-EXTENSION, "Empresa
                P_BUDAT  FOR ZIB_CONTABIL-BUDAT OBLIGATORY NO-EXTENSION. "Data de Lançamento
SELECTION-SCREEN: END OF BLOCK B1.

**********************************
*  Seleção de Dados.
**********************************
START-OF-SELECTION.

  ""INVOICES - Contas a Pagar
  SELECT * FROM ZFIT0036
   INTO TABLE GT_ZFIT0036
  WHERE ID_TIPO_INVOICE EQ C_61.

  CHECK NOT GT_ZFIT0036[] IS INITIAL.

  "Tabela de Registros de Geração de Documentos Z_FI_MANYDOCUME
  SELECT * FROM ZIB_CONTABIL
    INTO TABLE GT_ZIB_CONTABIL
    FOR ALL ENTRIES IN GT_ZFIT0036
 WHERE OBJ_KEY EQ GT_ZFIT0036-OBJ_KEY
   AND BSCHL   EQ C_31.

  "Tabela de controle de Chave de referência
  SELECT * FROM ZIB_CONTABIL_CHV
    INTO TABLE GT_ZIB_CONTABIL_CHV
    FOR ALL ENTRIES IN GT_ZFIT0036
   WHERE OBJ_KEY EQ GT_ZFIT0036-OBJ_KEY.

  "Mestre de fornecedores (parte geral)
  SELECT * FROM LFA1
    INTO TABLE GT_LFA1
    FOR ALL ENTRIES IN GT_ZIB_CONTABIL
  WHERE LIFNR EQ GT_ZIB_CONTABIL-HKONT.


  "Loop para agrupar as informações selecionadas acima e mostrar no ALV
  "Dentro do mesmo existe uma chamada para o WebService da taxa curva.

  LOOP AT GT_ZFIT0036 INTO GW_ZFIT0036.

    GW_SAIDA-INVOICE   = GW_ZFIT0036-INVOICE.

    READ TABLE GT_ZIB_CONTABIL INTO GW_ZIB_CONTABIL WITH KEY OBJ_KEY = GW_ZFIT0036-OBJ_KEY.
    GW_SAIDA-OBJ_KEY   = GW_ZIB_CONTABIL-OBJ_KEY.
    GW_SAIDA-BUDAT     = GW_ZIB_CONTABIL-BUDAT.
    GW_SAIDA-ZFBDT     = GW_ZIB_CONTABIL-ZFBDT.
    GW_SAIDA-DMBTR     = GW_ZIB_CONTABIL-DMBTR.
    GW_SAIDA-TX_CAMBIO = GW_ZIB_CONTABIL-DMBTR / GW_ZIB_CONTABIL-DMBE2.
    GW_SAIDA-HKONT     = GW_ZIB_CONTABIL-HKONT.

    READ TABLE GT_ZIB_CONTABIL_CHV INTO GW_ZIB_CONTABIL_CHV WITH KEY OBJ_KEY = GW_ZFIT0036-OBJ_KEY.
    GW_SAIDA-BELNR     = GW_ZIB_CONTABIL_CHV-BELNR.

    READ TABLE GT_LFA1 INTO GW_LFA1 WITH KEY LIFNR = GW_ZIB_CONTABIL-HKONT.
    GW_SAIDA-NAME1 = GW_LFA1-NAME1.

    FREE: GOBJ_ZCL_WEBSERVICE_TX_CURVA. "Limpar o objeto do WebService da Taxa Curva
    CREATE OBJECT GOBJ_ZCL_WEBSERVICE_TX_CURVA. "Criar o Objeto do WebService da Taxa Curva.

    "Atribuir a data da zib_contabil que é CHAR para o tipo DATUM, para passagem de parametro no método
    "de busca da taxa curva no webservice.
    VAR_DATA = GW_ZIB_CONTABIL-BUDAT.

    "Chamada do métod busca_taxa do webservice da taxa curva, esperando retorno do valor da mesma.
    GW_SAIDA-TX_CURVA = GOBJ_ZCL_WEBSERVICE_TX_CURVA->BUSCAR_TAXA( I_DATA = VAR_DATA ).

    APPEND GW_SAIDA TO GT_SAIDA.

    CLEAR: GW_SAIDA, GW_ZIB_CONTABIL, GW_ZIB_CONTABIL_CHV, GW_LFA1, VAR_DATA.

  ENDLOOP.

END-OF-SELECTION.

  IF ( GT_SAIDA[] IS INITIAL ).
    MESSAGE 'Dados não encontrados.' TYPE 'S' DISPLAY LIKE 'W'.
  ELSE.
    PERFORM: CRIAR_ALV.
    CALL SCREEN TELA_0100.   "Chamar a Tela para apresentação do ALV.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.
  CASE SY-UCOMM.
    WHEN: C_BACK OR C_CANCEL.
      LEAVE TO SCREEN 0. "Voltar uma tela antes.
    WHEN: C_EXIT.
      LEAVE PROGRAM. "Sair do programa.
  ENDCASE.
ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV
*&---------------------------------------------------------------------*
FORM CRIAR_ALV .

  DATA: WL_LAYOUT  TYPE LVC_S_LAYO. "Controle VLA: estrutura layout

  WL_LAYOUT-ZEBRA      = 'X'.

  CREATE OBJECT OBJ_CONTAINER
    EXPORTING
      CONTAINER_NAME              = 'CONTAINER'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  PERFORM: MONTAR_CATALOG.

  CREATE OBJECT OBJ_ALV
    EXPORTING
      I_PARENT          = OBJ_CONTAINER
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

  CALL METHOD OBJ_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WL_LAYOUT
      I_SAVE                        = C_A
    CHANGING
      IT_OUTTAB                     = GT_SAIDA[]
      IT_FIELDCATALOG               = GT_FIELDCATALOG[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " CRIAR_ALV
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG
*&---------------------------------------------------------------------*
FORM MONTAR_CATALOG.

  PERFORM FIELDCATALOG USING:
  'OBJ_KEY'     'ID'                 '20'    ' ' '' ' ' '' ' ',
  'BUKRS'       'Empresa'            '10'    ' ' '' ' ' '' ' ',
  'BELNR'       'Nr. Documento'      '15'    ' ' '' ' ' '' ' ',
  'INVOICE'     'Invoice'            '10'    ' ' '' ' ' '' ' ',
  'BUDAT'       'Dt. Lçto.'          '10'    ' ' '' ' ' '' ' ',
  'ZFBDT'       'Dt. Vcto.'          '10'    ' ' '' ' ' '' ' ',
  'DMBTR'       'Valor R$'           '13'    ' ' '' ' ' '' ' ',
  'TX_CAMBIO'   'Tx. Câmbio'         '9'     ' ' '' ' ' '' ' ',
  'TX_CURVA'    'Tx. Curva'          '9'     ' ' '' ' ' '' ' ',
  'HKONT'       'Fornecedor'         '10'    ' ' '' ' ' '' ' ',
  'NAME1'       'Nome do Fornecedor' '25'    ' ' '' ' ' '' ' '.

ENDFORM.                    " MONTAR_CATALOG
*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FIELDCATALOG  USING    VALUE(P_FIELDNAME)
                            VALUE(P_DESC)
                            VALUE(P_TAM)
                            VALUE(P_NO_ZERO)
                            VALUE(P_HOTSPOT)
                            VALUE(P_COR)
                            VALUE(P_JUST)
                            VALUE(P_SUM).

  GW_FIELDCATALOG-FIELDNAME = P_FIELDNAME.
  GW_FIELDCATALOG-SCRTEXT_L = P_DESC.
  GW_FIELDCATALOG-SCRTEXT_M = P_DESC.
  GW_FIELDCATALOG-SCRTEXT_S = P_DESC.
  GW_FIELDCATALOG-OUTPUTLEN = P_TAM.
  GW_FIELDCATALOG-NO_ZERO   = P_NO_ZERO.
  GW_FIELDCATALOG-HOTSPOT   = P_HOTSPOT.
  GW_FIELDCATALOG-EMPHASIZE = P_COR.
  GW_FIELDCATALOG-JUST      = P_JUST.
  GW_FIELDCATALOG-DO_SUM    = P_SUM.

  APPEND GW_FIELDCATALOG TO GT_FIELDCATALOG.

  CLEAR: GW_FIELDCATALOG.

ENDFORM.                    " FIELDCATALOG
