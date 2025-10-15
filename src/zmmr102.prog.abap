************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 23.04.2015                                          *
* Objetivo    ...: Relatorio de Estoque Negativo                       *
* Transação   ...: ZMM0075                                             *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 27.04.2015  Welgem Barbosa   Criação      10:27          DEVK945781  *
************************************************************************


REPORT  ZMMR102.

*----------------------------------------------------------------------*
*       TABELAS UTILIZADAS
*----------------------------------------------------------------------*
TABLES: MARC, MARD, MAKT.

TYPE-POOLS: SLIS.

*----------------------------------------------------------------------*
*       ESTRUTURA DA TABELA TY_SAIDA
*----------------------------------------------------------------------*
TYPES:
      BEGIN OF TY_SAIDA,
          MATNR TYPE MARC-MATNR, " Material
          WERKS TYPE MARC-WERKS, " Centro de Custo
          XMCNG TYPE C LENGTH 3, " 'X' sim,  '' nao
          LGORT TYPE MARD-LGORT, " Deposito
          LABST TYPE MARD-LABST, " Quantidade
          MAKTX TYPE MAKT-MAKTX, " Descrição do Material
      END OF TY_SAIDA.


*----------------------------------------------------------------------*
*       DECLARAÇÃO DA TABELA INTERNAS E WORK AREAS
*----------------------------------------------------------------------*
DATA: IT_SAIDA          TYPE TABLE OF TY_SAIDA,
      WA_SAIDA          TYPE TY_SAIDA,
      WA_FCAT           TYPE LVC_S_FCAT,
      IT_FCAT           TYPE LVC_T_FCAT,
      WA_CONT           TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV            TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT         TYPE LVC_S_LAYO,
      LT_SORT           TYPE LVC_T_SORT,
      WA_SORT           LIKE LINE OF LT_SORT.

FIELD-SYMBOLS <FS_SAIDA> TYPE TY_SAIDA.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLE DEFINITION
*----------------------------------------------------------------------*
*       DEFININDO CLASSE PARA AÇÃO DE CHAMADA DE TRANSAÇÃO
*----------------------------------------------------------------------*
CLASS LCL_EV_HOT DEFINITION.
  PUBLIC SECTION.
    METHODS HOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID.
ENDCLASS.                    "LCL_EV_HOT DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EV_HOT IMPLEMENTATION
*----------------------------------------------------------------------*
*       IMPLEMENTANDO A CHAMADA DA TRANSAÇÃO
*----------------------------------------------------------------------*
CLASS LCL_EV_HOT IMPLEMENTATION.
  METHOD HOT_CLICK.
    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX E_ROW_ID.

      SET PARAMETER ID 'MAT' FIELD WA_SAIDA-MATNR.     "PARAMETRO DA TRANSAÇÃO MM03
      SET PARAMETER ID 'WRK' FIELD WA_SAIDA-WERKS.
      SET PARAMETER ID 'LAG' FIELD WA_SAIDA-LGORT.
      CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.

  ENDMETHOD.                    "HOT_CLICK
ENDCLASS.                    "LCL_EV_HOT IMPLEMENTATION



*&---------------------------------------------------------------------*
*&      Form  SELECIONE_DADOS
*&---------------------------------------------------------------------*
*       ENTRADA DO USUÁRIO PARA BUSCA
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_CEN  FOR MARC-WERKS NO-EXTENSION OBLIGATORY,
                P_MAT  FOR MARC-MATNR NO-EXTENSION,
                P_DEP  FOR MARD-LGORT NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM: SELECIONE_DADOS,
   F_ALV.", F_FCAT.   "chamada dos forms

  CALL SCREEN 0100.

SORT IT_SAIDA BY MATNR DESCENDING.

*&---------------------------------------------------------------------*
*&      Form  SELECIONE_DADOS
*&---------------------------------------------------------------------*
*       SELECT PARA UNIR AS 3 TABELAS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONE_DADOS .

  SELECT
          MRC~MATNR
          MRC~WERKS
          MRC~XMCNG
          MRD~LGORT
          MRD~LABST
          MKT~MAKTX

  FROM MARC AS MRC
  INNER JOIN MARD AS MRD ON  MRD~MATNR EQ MRC~MATNR " uni a marc e mard pelo codigo do material matnr
                        AND  MRD~WERKS EQ MRC~WERKS " uni a marc e mard pelo codigo do centro de custo werk
  INNER JOIN MAKT AS MKT ON  MKT~MATNR EQ MRC~MATNR " uni a marc e makt pelo codigo do material matnr
                        AND  SPRAS     EQ SY-LANGU  " informa a mesma linguagem do usuário logado
  INTO TABLE IT_SAIDA
  WHERE MRC~MATNR IN P_MAT                      " Codigo do material inserido pelo usuário
    AND MRC~WERKS IN P_CEN                      " Centro de custo inserido pelo usuário
    AND ( MRC~XMCNG EQ 'X' OR MRD~LABST < 0 )   " busca todos os dados com XMCNG com o valor 'X' ou o campo LABST com valores negativos.
    AND MRD~LGORT IN P_DEP.                     " Codigo do Deposito inserido pelo usuário

  LOOP AT IT_SAIDA ASSIGNING <FS_SAIDA>.

    IF <FS_SAIDA>-XMCNG NE 'X'.
      <FS_SAIDA>-XMCNG = 'NÃO'.
    ELSE.
      <FS_SAIDA>-XMCNG = 'SIM'.
    ENDIF.
  ENDLOOP.
  UNASSIGN <FS_SAIDA>.

ENDFORM.                    " SELECIONE_DADOS


*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       Modelando a ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV .
  PERFORM ALV_PREENCHE_CAT USING:
" Sequencia da ALV para impressão
          "Campo"  "Descrição"     "qtd de string" "no-0"
          'MATNR'  'Material'          '18'         'X'     'X' '',
          'MAKTX'  'Desc.Material'     '40'         ''      '' '',
          'WERKS'  'Centro'            '7'          ''      '' '',
          'LGORT'  'Deposito'          '9'          ''      '' '',
          'LABST'  'Quantidade'        '15'         ''      '' '',
          'XMCNG'  'Est.Negativo'      '4'          ''      '' ''.
ENDFORM.                    " F_ALV


*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       Preenchendo a modelagem
*----------------------------------------------------------------------*
*      -->P_CAMPO  text
*      -->P_DESC   text
*      -->P_TAM    text
*      -->P_ZERO   text
*      -->P_HOT    text
*      -->P_SUM    text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING:   P_CAMPO TYPE C
                                P_DESC  TYPE C
                                P_TAM   TYPE C
                                P_ZERO  TYPE C
                                P_HOT   TYPE C
                                P_SUM   TYPE C.

  WA_FCAT-FIELDNAME = P_CAMPO.
  "WA_FCAT-SELTEXT_M = P_DESC.
  WA_FCAT-SCRTEXT_M = P_DESC.
  WA_FCAT-OUTPUTLEN = P_TAM.
  WA_FCAT-NO_ZERO   = P_ZERO.
  WA_FCAT-HOTSPOT   = P_HOT.
  WA_FCAT-DO_SUM    = P_SUM.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Module  B_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE B_0100 OUTPUT.
  SET PF-STATUS 'S_0100'.
  SET TITLEBAR  'T_0100'.

  DATA: HANDLE_HOTSPOT TYPE REF TO LCL_EV_HOT.  "CRIANDO A VARIAVEL PARA REFERENCIAR MINHA CLASSE
  CREATE OBJECT HANDLE_HOTSPOT.

  CREATE OBJECT WA_CONT
    EXPORTING
      CONTAINER_NAME              = 'T_0100'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  CREATE OBJECT WA_ALV
    EXPORTING
      I_PARENT                    = WA_CONT
    EXCEPTIONS
      ERROR_CNTL_CREATE           = 1
      ERROR_CNTL_INIT             = 2
      ERROR_CNTL_LINK             = 3
      ERROR_DP_CREATE             = 4
      OTHERS                      = 5.

*     SET HANDLER-> CHAMA O OBJETO HANDLE_HOTSPOT E O METODO INSERINDO PARA O WA-ALV
  SET HANDLER HANDLE_HOTSPOT->HOT_CLICK FOR WA_ALV.

  CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
    CHANGING
      IT_OUTTAB                     = IT_SAIDA
      IT_FIELDCATALOG               = IT_FCAT
      IT_SORT                       = LT_SORT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

  IF SY-SUBRC NE 0 .
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDMODULE.                 " B_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  A_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE A_0100 INPUT.
  CASE SY-UCOMM.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " A_0100  INPUT
