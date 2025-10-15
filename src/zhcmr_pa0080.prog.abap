************************************************************************
* PROJETO            : Amaggi x TGG                                    *
* PROGRAMA           : ZHCMR_PA0080                                    *
* TRANSACAO          : xxxxxxxx                                        *
* DESCRICAO          : Acordo Individual Banco Horas                   *
* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *
* AUTOR              : Henrique Martins                                *
* DATA               : 29/08/2022                                      *
*----------------------------------------------------------------------*
*                      HISTORICO DE MUDANÇAS                           *
*----------------------------------------------------------------------*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*----------------------------------------------------------------------*
REPORT ZHCMR_PA0080.

SELECTION-SCREEN BEGIN OF BLOCK 1 .
PARAMETERS: P_CHAVE TYPE CHAR30 NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK 1.

***********************************************
* Initialization
***********************************************
INITIALIZATION.
***********************************************
* Start of selection
***********************************************

*======================================================================*
*** Nodes BDL
*======================================================================*
  NODES: PERAS.

*======================================================================*
*** Tabelas
*======================================================================*
  TABLES: PERNR.

  DATA: VL_FORMNAME      TYPE TDSFNAME,
        WA_SAIDA         TYPE ZHCMS_REG_EMPREGADO.


*======================================================================*
*** Infotipos
*======================================================================*
  INFOTYPES: 0001 NAME P0001.
  INFOTYPES: 0002 NAME P0002.
  INFOTYPES: 0465 NAME P0465.
  INFOTYPES: 0030 NAME P0030.
  INFOTYPES: 0007 NAME P0007.


*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.
* Get objects
  GET PERAS.

  RP_PROVIDE_FROM_LAST P0001 SPACE PN-BEGDA PN-ENDDA.
  RP_PROVIDE_FROM_LAST P0002 SPACE PN-BEGDA PN-ENDDA.
  RP_PROVIDE_FROM_LAST P0465 SPACE PN-BEGDA PN-ENDDA.
  RP_PROVIDE_FROM_LAST P0030 '10'  PN-BEGDA PN-ENDDA.

  PERFORM DADOS_EMPRESA.
  PERFORM: FORM_IMPRIMIR.

*&---------------------------------------------------------------------*
*&      Form  FORM_IMPRIMIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FORM_IMPRIMIR.

  DATA: VL_FORMNAME      TYPE TDSFNAME,
        VL_NAME          TYPE RS38L_FNAM,
        V_BUKRS_TEXT     TYPE T001-BUTXT,
        V_BRUNCH         TYPE T7BRAP-FILIA,
        WA_BRANCH        TYPE BAPIBRANCH-BRANCH,
        G_BRANCHLIST     LIKE BAPIBRANCH OCCURS 0 WITH HEADER LINE,
        RETURN           LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE,
        V_CGC_NUMBER(18) TYPE C,
        V_ACCOUNTNO      LIKE BAPIP0009-BANKN,
        V_NAMEOFBANK     LIKE BAPIP0009-NAMEOFBANKL,
        V_CTPS_NR        TYPE PA0465-CTPS_NR,
        V_CTPS_SERIE     TYPE PA0465-CTPS_SERIE,
        V_DT_EMIS        TYPE PA0465-DT_EMIS,
        V_ES_EMIS        TYPE PA0465-ES_EMIS,
        V_MOTIVO         TYPE STRING,
        V_DIAS           TYPE STRING,
        V_DT_RETORNO     TYPE DATUM,
        V_DIASNUM        TYPE I.

  DATA: WA_CONTROL_PARAMETERS TYPE SSFCTRLOP,
        WA_OUTPUT_OPTIONS     TYPE SSFCOMPOP.


  VL_FORMNAME = 'ZHCMS_PA0030'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = VL_FORMNAME
    IMPORTING
      FM_NAME            = VL_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.


  SELECT SINGLE CTPS_NR CTPS_SERIE DT_EMIS ES_EMIS
    FROM PA0465
    INTO (V_CTPS_NR, V_CTPS_SERIE, V_DT_EMIS, V_ES_EMIS)
    WHERE PERNR = P0001-PERNR
    AND  SUBTY = '0003'. "CTPS

  SELECT SINGLE * FROM T513S  INTO @DATA(WA_T513S)
    WHERE SPRSL EQ 'P'
    AND   STELL EQ @P0001-STELL
    AND   BEGDA <= @PN-ENDDA
    AND   ENDDA >= @PN-BEGDA.

    data: l_pskey type pskey,
          l_tb_text type HRPAD_TEXT_TAB.

    MOVE-CORRESPONDING p0030 to l_pskey.

    CL_HRPA_TEXT_CLUSTER=>read( EXPORTING tclas  = 'A'
                                          pskey  = l_pskey
                                          no_auth_check = abap_true
                                IMPORTING text_tab = l_tb_text ).


    loop at l_tb_text into data(l_text).

      V_MOTIVO = |{ V_MOTIVO } { l_text }|.

    endloop.

  V_DIASNUM = P0030-ENDDA - P0030-BEGDA.
  ADD 1 TO V_DIASNUM.
  V_DIAS = V_DIASNUM.
  V_DT_RETORNO = P0030-ENDDA + 1.

  IF P_CHAVE IS NOT INITIAL.
    WA_OUTPUT_OPTIONS-TDCOVTITLE    = P_CHAVE.
    WA_OUTPUT_OPTIONS-TDIMMED       = SPACE.
    WA_CONTROL_PARAMETERS-NO_DIALOG = 'X'.
  ENDIF.

  CALL FUNCTION VL_NAME
    EXPORTING
      NAME               = P0002-CNAME
      CTPS_NR            = V_CTPS_NR
      CTPS_SERIE         = V_CTPS_SERIE
      DT_EMIS            = V_DT_EMIS
      ES_EMIS            = V_ES_EMIS
      DESC_CARGO         = WA_T513S-STLTX
      DADOS_EMPRESA      = WA_SAIDA
      MOTIVO             = V_MOTIVO
      DT_INI             = P0030-BEGDA
      DT_FIM             = P0030-ENDDA
      DIAS               = V_DIAS
      DT_RETORNO         = V_DT_RETORNO
      CONTROL_PARAMETERS = WA_CONTROL_PARAMETERS
      OUTPUT_OPTIONS     = WA_OUTPUT_OPTIONS
      USER_SETTINGS      = ' '
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.

  IF SY-SUBRC <> 0.
  ENDIF.



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DADOS_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DADOS_EMPRESA.

  DATA: IT_ADDR  TYPE ADDR1_VAL.

  DATA: BUKRS        LIKE BAPIBRANCH-BUKRS,
        BRANCH       LIKE BAPIBRANCH-BRANCH,
        NAME         LIKE BAPIBRANCH-NAME,
        CGC_NR       LIKE BAPIBRANCH-CGC_NUMBER,
        DESC_SECAO   LIKE P1000-STEXT,
        DESC_FUNCAO  LIKE P1000-STEXT,
        SALARIO      LIKE P0008-BET01,
        SALARIO_HORA LIKE P0008-BET01,
        ENTRY_DATE   TYPE DATS,
        CBO_NR       TYPE T7BRCB-CBO,
        DTDEMISSAO   TYPE DATS.

  DATA: VL_IMAGE_NAME TYPE TDOBNAME,
        VL_TEM_IMG    TYPE CHAR1.

  CLEAR: VL_IMAGE_NAME, VL_TEM_IMG.

  RP_PROVIDE_FROM_LAST P0001 SPACE PN-BEGDA PN-ENDDA.

  "Empresa e Filial
  CALL FUNCTION 'HR_BR_GET_FILIAL_PER_AREA'
    EXPORTING
      P_WERKS        = P0001-WERKS
      P_BTRTL        = P0001-BTRTL
    IMPORTING
      BUKRS          = BUKRS
      BRANCH         = BRANCH
    EXCEPTIONS
      NO_LINK_AREAS  = 1
      NO_GROUP_FOUND = 2.

  "Endereço e CGC
  CALL FUNCTION 'HR_BR_LER_FILIAL_GERAL'
    EXPORTING
      COMPANY_CODE      = BUKRS
      BRANCH            = BRANCH
      DATE              = PN-ENDDA
    IMPORTING
      CGC               = CGC_NR
      COMP_NAME         = NAME
      COMP_ADDR         = IT_ADDR
    EXCEPTIONS
      BRANCH_NOT_FOUND  = 1
      ADDRESS_NOT_FOUND = 2
      COMPANY_NOT_FOUND = 3
      OTHERS            = 4.

  "Descrição da Posição
  CALL FUNCTION 'HR_READ_FOREIGN_OBJECT_TEXT'
    EXPORTING
      OTYPE         = 'S'
      OBJID         = P0001-PLANS
      BEGDA         = P0001-BEGDA
      ENDDA         = P0001-ENDDA
    IMPORTING
      OBJECT_TEXT   = DESC_FUNCAO
    EXCEPTIONS
      NOTHING_FOUND = 1.

  "Descrição do Setor
  CALL FUNCTION 'HR_READ_FOREIGN_OBJECT_TEXT'
    EXPORTING
      OTYPE                   = 'O'
      OBJID                   = P0001-ORGEH
      BEGDA                   = P0001-BEGDA
      ENDDA                   = P0001-ENDDA
    IMPORTING
      OBJECT_TEXT             = DESC_SECAO
    EXCEPTIONS
      NOTHING_FOUND           = 1
      WRONG_OBJECTTYPE        = 2
      MISSING_COSTCENTER_DATA = 3
      MISSING_OBJECT_ID       = 4.

  "Salário
  CALL FUNCTION 'HRGPBS_TPS_PERIOD_SALARY_PERNR'
    EXPORTING
      F_PERNR         = PERNR-PERNR
      F_DATE          = PN-ENDDA
    IMPORTING
      F_PERIOD_SALARY = SALARIO
    EXCEPTIONS
      INTERNAL_ERROR  = 1
      OTHERS          = 2.

*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2.
*    EXIT.
*  ENDIF.

  "Data de Contratação
  CALL FUNCTION 'HR_ENTRY_DATE'
    EXPORTING
      PERSNR    = PERNR-PERNR
      BEGDA     = '18000101'
      ENDDA     = '99991231'
    IMPORTING
      ENTRYDATE = ENTRY_DATE.

  " Data de Demissão
  CALL FUNCTION 'RP_GET_FIRE_DATE'
    EXPORTING
      PERSNR   = PERNR-PERNR
      STATUS2  = '0'
    IMPORTING
      FIREDATE = DTDEMISSAO.

  DATA DTADEMISSAOCHAR TYPE CHAR14.

  DTADEMISSAOCHAR = '___/___/_____'.

  IF DTDEMISSAO IS NOT INITIAL.
    DTADEMISSAOCHAR = DTDEMISSAO.
    DTADEMISSAOCHAR =  |{ DTDEMISSAO+6(2) } / { DTDEMISSAO+4(2) } / {  DTDEMISSAO(4) }|.
    CONDENSE DTADEMISSAOCHAR.
  ENDIF.

  "Inscrição Estadual
  SELECT SINGLE STATE_INSC
    FROM J_1BBRANCH
    INTO WA_SAIDA-INSCE_NR
    WHERE BUKRS EQ BUKRS
    AND BRANCH = BRANCH.

  "CBO
  SELECT SINGLE CBO
    FROM T7BRCB
    INTO CBO_NR
    WHERE PLANS = P0001-PLANS
    AND ENDDA GE PN-ENDDA.

  RP_PROVIDE_FROM_LAST P0007 SPACE PN-BEGDA PN-ENDDA.

  "Horário de Trabalho
  ZCL_HRST_COMMONS=>GET_DESCR_HOR_TRABALHO(
     EXPORTING
      IW_P0001 = P0001
      IV_SCHKZ = P0007-SCHKZ
     RECEIVING
      RE_RTEXT = WA_SAIDA-HORA_DESC ).

  "Salário por Hora
  IF P0007-MOSTD IS NOT INITIAL.
    SALARIO_HORA = SALARIO / P0007-MOSTD.
  ELSE.
    SALARIO_HORA = 0.
  ENDIF.

  WA_SAIDA-BUKRS = P0001-BUKRS.
  WA_SAIDA-NAME1 = IT_ADDR-NAME1.
  WA_SAIDA-STREET = IT_ADDR-STREET.
  WA_SAIDA-HOUSE_NUM1 = IT_ADDR-HOUSE_NUM1.
  WA_SAIDA-HOUSE_NUM2 = IT_ADDR-HOUSE_NUM2.
  WA_SAIDA-CITY2 = IT_ADDR-CITY2.
  WA_SAIDA-CITY1 = IT_ADDR-CITY1.
  WA_SAIDA-POST_CODE1 = IT_ADDR-POST_CODE1.
  WA_SAIDA-REGION = IT_ADDR-REGION.
  WA_SAIDA-CGC_NR = CGC_NR.
  WA_SAIDA-PERNR = P0001-PERNR.
  WA_SAIDA-ENTRY_DATE = ENTRY_DATE.
  WA_SAIDA-DTDEMISSAOCHAR = DTADEMISSAOCHAR.
  WA_SAIDA-DESC_SECAO = DESC_SECAO.
  WA_SAIDA-DESC_FUNCAO = DESC_FUNCAO.
  WA_SAIDA-CBO_NR = CBO_NR.
  WA_SAIDA-SALARIO = SALARIO.
  WA_SAIDA-SALARIO_HORA = SALARIO_HORA.
  "wa_saida-hora_trab ???


  MOVE P0001-PERNR TO VL_IMAGE_NAME. "'IMG_'

  "Verifica se possui foto
*  CALL METHOD ZCL_IMAGE_HELPER->CHECK_GRAPHIC_EXIST
*    EXPORTING
*      ID    = ZCL_IMAGE_HELPER=>ID_REPOSITORY-HR_PHOTO
*      NAME  = VL_IMAGE_NAME
*    RECEIVING
*      VALUE = VL_TEM_IMG.

  IF VL_TEM_IMG IS NOT INITIAL.
    CONCATENATE 'IMG_' VL_IMAGE_NAME INTO WA_SAIDA-FOTO.
  ENDIF.


ENDFORM.
