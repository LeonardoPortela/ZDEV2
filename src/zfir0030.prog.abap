************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 19.07.2013                                          *
* Objetivo    ...: Relatório para consulta de dados enviados para o BW *
* Imobilizado Vida Util                                                *
* Transação   ...:                                                     *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 19.07.2013   Camila Brand    Criação                                 *
************************************************************************

REPORT  ZFIR0030.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZFIT0044.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:

   " Documento de vendas: dados de cabeçalho
 BEGIN OF TY_ZFIT0044,
   EMPRESA          TYPE ZFIT0044-EMPRESA,
   NU_IMOBILIZADO   TYPE ZFIT0044-NU_IMOBILIZADO,
   SU_IMOBILIZADO   TYPE ZFIT0044-SU_IMOBILIZADO,
   DATA_AQUI        TYPE ZFIT0044-DATA_AQUI,
   CLASSE_IMOB      TYPE ZFIT0044-CLASSE_IMOB,
   DET_CONTA        TYPE ZFIT0044-DET_CONTA,
   AREA_AVAL        TYPE ZFIT0044-AREA_AVAL,
   VIDA_PLAN_ANOS   TYPE ZFIT0044-VIDA_PLAN_ANOS,
   VIDA_PLAN_PER    TYPE ZFIT0044-VIDA_PLAN_PER,
   VALOR_AQUI_ACU   TYPE ZFIT0044-VALOR_AQUI_ACU,
   CONTA_RAZAO      TYPE ZFIT0044-CONTA_RAZAO,
   EXERCICIO        TYPE ZFIT0044-EXERCICIO,
   MOEDA            TYPE ZFIT0044-MOEDA,
   VALOR_AQUI_PER   TYPE ZFIT0044-VALOR_AQUI_PER,
   VIDA_PLAN_ANOS_F TYPE ZFIT0044-VIDA_PLAN_ANOS_F,
 END OF TY_ZFIT0044.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.


*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: T_BDC     TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      T_MESSTAB TYPE TABLE OF BDCMSGCOLL,


      IT_ZFIT0044      TYPE TABLE OF TY_ZFIT0044,
      T_SAIDA          TYPE TABLE OF TY_ZFIT0044.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
      WA_CONT          TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV           TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT        TYPE LVC_S_LAYO,
      WA_ZFIT0044      TYPE TY_ZFIT0044,
      WA_SAIDA         TYPE TY_ZFIT0044.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*

DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER,
      T_SORT       TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETER: P_BUKRS  TYPE ZFIT0044-EMPRESA OBLIGATORY,
           P_DTPROC TYPE ZFIT0044-DATA_AQUI OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK B1.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
PERFORM F_INICIAR_VARIAVES.
PERFORM F_SELECIONA_DADOS.
PERFORM F_ORGANIZA_DADOS.
PERFORM F_IMPRIME_DADOS.


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  SELECT
    EMPRESA
    NU_IMOBILIZADO
    SU_IMOBILIZADO
    DATA_AQUI
    CLASSE_IMOB
    DET_CONTA
    AREA_AVAL
    VIDA_PLAN_ANOS
    VIDA_PLAN_PER
    VALOR_AQUI_ACU
    CONTA_RAZAO
    EXERCICIO
    MOEDA
    VALOR_AQUI_PER
    VIDA_PLAN_ANOS_F

 FROM ZFIT0044
    INTO TABLE IT_ZFIT0044
     WHERE DATA_AQUI EQ  P_DTPROC
     AND EMPRESA EQ  P_BUKRS .


ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM F_ORGANIZA_DADOS .

  LOOP AT IT_ZFIT0044 INTO WA_ZFIT0044.
    WA_SAIDA-EMPRESA           = WA_ZFIT0044-EMPRESA.
    WA_SAIDA-NU_IMOBILIZADO    = WA_ZFIT0044-NU_IMOBILIZADO.
    WA_SAIDA-SU_IMOBILIZADO    = WA_ZFIT0044-SU_IMOBILIZADO.
    WA_SAIDA-DATA_AQUI         = WA_ZFIT0044-DATA_AQUI.
    WA_SAIDA-CLASSE_IMOB       = WA_ZFIT0044-CLASSE_IMOB.
    WA_SAIDA-DET_CONTA         = WA_ZFIT0044-DET_CONTA.
    WA_SAIDA-AREA_AVAL         = WA_ZFIT0044-AREA_AVAL.
    WA_SAIDA-VIDA_PLAN_ANOS    = WA_ZFIT0044-VIDA_PLAN_ANOS.
    WA_SAIDA-VIDA_PLAN_PER     = WA_ZFIT0044-VIDA_PLAN_PER.
    WA_SAIDA-VALOR_AQUI_ACU    = WA_ZFIT0044-VALOR_AQUI_ACU.
    WA_SAIDA-CONTA_RAZAO       = WA_ZFIT0044-CONTA_RAZAO.
    WA_SAIDA-EXERCICIO         = WA_ZFIT0044-EXERCICIO.
    WA_SAIDA-MOEDA             = WA_ZFIT0044-MOEDA.
    WA_SAIDA-VALOR_AQUI_PER    = WA_ZFIT0044-VALOR_AQUI_PER.
    WA_SAIDA-VIDA_PLAN_ANOS_F  = WA_ZFIT0044-VIDA_PLAN_ANOS_F.


    APPEND WA_SAIDA TO T_SAIDA.
    CLEAR: WA_SAIDA.
  ENDLOOP.

ENDFORM.                    " F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*

FORM F_IMPRIME_DADOS .
  IF T_SAIDA[] IS INITIAL.
    MESSAGE I000(Z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.
  PERFORM F_DEFINIR_EVENTOS.
  PERFORM F_ALV_SORT.
  PERFORM F_MONTAR_LAYOUT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      I_CALLBACK_USER_COMMAND = 'F_USER_COMMAND'
      IT_FIELDCAT             = ESTRUTURA[]
      IT_SORT                 = T_SORT[]
      I_SAVE                  = 'A'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
    TABLES
      T_OUTTAB                = T_SAIDA.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*

FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*

FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*

FORM F_MONTAR_LAYOUT.
  PERFORM F_MONTAR_ESTRUTURA USING:

    'EMPRESA'             'Empresa' ' ',
    'NU_IMOBILIZADO'      'Imobilizado' ' ',
    'SU_IMOBILIZADO'      'Sub Numero' ' ',
    'DATA_AQUI'           'Data Processamento' ' ',
    'CLASSE_IMOB'         'Classe Imobilizado' ' ',
    'VIDA_PLAN_ANOS'      'Vida Util Anos' ' ',
    'VIDA_PLAN_PER'       'Vida Util Periodo' ' ',
    'VALOR_AQUI_ACU'      'Valor de Aquisição' ' ',
    'CONTA_RAZAO'         'Conta Razão' ' ',
    'EXERCICIO'           'Exercicio' ' ',
    'VALOR_AQUI_PER'      'Valor Aquisição p/ período' ' ',
    'VIDA_PLAN_ANOS_F'    'Valor Aquisição / Ano' ' '.


ENDFORM.                    " F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM F_MONTAR_ESTRUTURA USING VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                              VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                              VALUE(P_ZERO)          TYPE C.

  DATA: X_CONTADOR TYPE STRING.
  CLEAR: WA_ESTRUTURA, X_CONTADOR.

  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = 'T_SAIDA'.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-NO_ZERO       = P_ZERO                            .
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.
  WA_ESTRUTURA-OUTPUTLEN     = X_CONTADOR.


*  IF P_FIELD EQ 'DOCNUM'.
*    WA_ESTRUTURA-HOTSPOT = 'X'.
*  ELSE.
*    CLEAR WA_ESTRUTURA-HOTSPOT.
*  ENDIF.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " F_montar_estrutura

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.
*            I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INICIAR_VARIAVES.

  DATA: W_TEXTO1(40).
  DATA: W_TEXTO2(20).

  V_REPORT = SY-REPID.

*** Nome do Report
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.

  SELECT SINGLE BUTXT FROM T001 INTO W_TEXTO2
    WHERE BUKRS EQ P_BUKRS.

  CONCATENATE 'Empresa:' P_BUKRS '-' W_TEXTO2 INTO W_TEXTO1 SEPARATED BY SPACE.
*** Nome da empresa
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO1.

  WRITE: SY-DATUM TO W_TEXTO2.
  CONCATENATE 'Data:' W_TEXTO2 INTO W_TEXTO1 SEPARATED BY SPACE.

  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.
  WRITE: SY-UZEIT TO W_TEXTO2.

  CONCATENATE 'Hora:' W_TEXTO2 INTO W_TEXTO1 SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.

  CONCATENATE 'Usuário:' SY-UNAME INTO W_TEXTO1 SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.
ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_SORT.

*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'BUKRS'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 1.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'BUTXT'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 2.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'GSBER'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 3.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'NAME'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 4.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'DATA'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 5.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'USER'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 6.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM F_USER_COMMAND USING L_UCOMM
                          L_SELFIELD TYPE SLIS_SELFIELD.

*  IF L_SELFIELD-FIELDNAME = 'DOCNUM'.
*    READ TABLE T_SAIDA INDEX L_SELFIELD-TABINDEX INTO WA_SAIDA.
*
*    SET PARAMETER ID 'JEF' FIELD L_SELFIELD-VALUE.
*
*    CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
*    "CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

* * ENDIF.

ENDFORM.                    "f_user_command
