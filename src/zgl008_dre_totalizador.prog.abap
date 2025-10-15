************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 11.12.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Relatório de aprensantação da DRE                   *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 11.12.2008    Marcus Barbara       Criação              DEVK905322   *
* 19.12.2008    Marcus Barbara       Alteração            DEVK905363   *
* 23.03.2009    Marcus Barbara       Alteração            DEVK905717   *
* 26.03.2009    Marcus Barbara       Alteração            DEVK905719   *
************************************************************************

REPORT  ZGL008_DRE_TOTALIZADOR NO STANDARD PAGE HEADING    "Não exibe cabeçalho standard
LINE-SIZE 076               "Comprimento da Linha
LINE-COUNT 65.              "Número de Linhas

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS.

*----------------------------------------------------------------------*
* Tabelas Internas (ALV)                                               *
*----------------------------------------------------------------------*
DATA: IT_FIELDCAT        TYPE SLIS_T_FIELDCAT_ALV,  "Estrutura de saida
      IT_EVENT           TYPE SLIS_T_EVENT  WITH HEADER LINE,   "Eventos
      VG_LAYOUT          TYPE SLIS_LAYOUT_ALV.   "Layout do alv

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*

DATA: BEGIN OF WA_DRE_001.
        INCLUDE STRUCTURE ZGL001_DRE_EST.
DATA: END   OF WA_DRE_001.

DATA: BEGIN OF WA_DRE_002.
        INCLUDE STRUCTURE ZGL002_DRE_EST.
DATA: END   OF WA_DRE_002.

DATA: BEGIN OF WA_DRE_008.
        INCLUDE STRUCTURE ZGL008_DRE_TOTAL.
DATA: END   OF WA_DRE_008.

DATA: BEGIN OF WA_DRE_008_2,
        BUKRS  LIKE ZGL008_DRE_TOTAL-BUKRS,
        VERSN  LIKE ZGL008_DRE_TOTAL-VERSN,
        NIVEL  LIKE ZGL008_DRE_TOTAL-NIVEL,
        EQUAC  LIKE ZGL008_DRE_TOTAL-EQUAC,
        DESNVL LIKE ZGL002_DRE_EST-DESNVL,
      END OF WA_DRE_008_2.

DATA: IT_DRE_001   LIKE STANDARD TABLE OF WA_DRE_001,
      IT_DRE_002   LIKE STANDARD TABLE OF WA_DRE_002,
      IT_DRE_008   LIKE STANDARD TABLE OF WA_DRE_008,
      IT_DRE_008_2 LIKE STANDARD TABLE OF WA_DRE_008_2.

*----------------------------------------------------------------------*
* Telas de Inclusão / Alteração / Exclusão
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 100 AS WINDOW. "Criar estrutura
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S10.
PARAMETERS: P_EQUAC     LIKE ZGL008_DRE_TOTAL-EQUAC OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK B1.
SELECTION-SCREEN END   OF SCREEN 100.

*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
*
START-OF-SELECTION.
  PERFORM F_ALV_ESTRUTURA.

*&---------------------------------------------------------------------*
*&      Form  F_ALV_ESTRUTURA
*&---------------------------------------------------------------------*
*   Em ALV Grid exibir o primerio nivel da estrutura de DRE
*----------------------------------------------------------------------*
FORM F_ALV_ESTRUTURA .
* Busco dados do primeiro nivel da estrutura de DRE
  PERFORM F_ALV_SEL_DADOS.
* Montar estruduta de ALV
  PERFORM F_ALV_EST.
* Executa ALV
  PERFORM F_ALV_EXECUTA.
ENDFORM.                    " F_ALV_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  F_ALV_SEL_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV_SEL_DADOS .
  SELECT *
    FROM ZGL001_DRE_EST
    INTO TABLE IT_DRE_001.
ENDFORM.                    " F_ALV_SEL_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_ALV_EST
*&---------------------------------------------------------------------*
*       Monta estrutura de ALV
*----------------------------------------------------------------------*
FORM F_ALV_EST.

  CLEAR: IT_FIELDCAT.

  PERFORM F_FIELDCAT USING:
        '0' '' 'IT_DRE_001' 'BUKRS' 'Empresa'
        07  ''  ''             '' ''  CHANGING IT_FIELDCAT,
        '1' '' 'IT_DRE_001' 'VERSN' 'Estrutura'
        09  ''  ''             '' 'X' CHANGING IT_FIELDCAT,
        '2' '' 'IT_DRE_001' 'VSTXT' 'Denominação da Estrutura'
        20  ''  ''             '' ''  CHANGING IT_FIELDCAT,
        '3' '' 'IT_DRE_001' 'WAERS' 'Moeda'
        05  ''  ''             '' ''  CHANGING IT_FIELDCAT.
ENDFORM.                    " F_ALV_EST
*&---------------------------------------------------------------------*
*&      Form  F_ALV_EXECUTA
*&---------------------------------------------------------------------*
*       Executa ALV
*----------------------------------------------------------------------*
FORM F_ALV_EXECUTA .
* Variavel Local
  DATA: VL_REPID LIKE SY-REPID.

  VL_REPID = SY-REPID.

  IT_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  IT_EVENT-FORM = SLIS_EV_TOP_OF_PAGE.
  APPEND IT_EVENT.

* Determinar a tabela de cores
  VG_LAYOUT-ZEBRA               = 'X'.

* Função para exibir o ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    I_CALLBACK_PROGRAM       = VL_REPID
    I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
    I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*    i_callback_top_of_page   = 'TOP_OF_PAGE'
    IS_LAYOUT                = VG_LAYOUT
*    i_background_id          = c_enjoy
    IT_FIELDCAT              = IT_FIELDCAT[]
    I_DEFAULT                = 'A'
    I_SAVE                   = 'A'
*    it_events                = it_event[]
  TABLES
    T_OUTTAB                 = IT_DRE_001
  EXCEPTIONS
    PROGRAM_ERROR            = 1
    OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " F_ALV_EXECUTA
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_FIELDCAT  USING P_CONT P_KEY  P_TAB  P_FIELD P_DESC
                      P_TAM  P_QTDE P_FIX  P_JUST P_HOT
             CHANGING P_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

* Tabela interna local
  DATA: TL_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

  TL_FIELDCAT-COL_POS    = P_CONT. "Posição
  TL_FIELDCAT-KEY        = P_KEY.  "
  TL_FIELDCAT-TABNAME    = P_TAB.  "Tabela interna
  TL_FIELDCAT-FIELDNAME  = P_FIELD."Campo
  TL_FIELDCAT-SELTEXT_L  = P_DESC. "Descrição longa
  TL_FIELDCAT-SELTEXT_M  = P_DESC. "Descrição media
  TL_FIELDCAT-SELTEXT_S  = P_DESC. "Descrição pequena
  TL_FIELDCAT-OUTPUTLEN  = P_TAM.  "Tamanho
  TL_FIELDCAT-QUANTITY   = P_QTDE. "Campo quantidade
  TL_FIELDCAT-FIX_COLUMN = P_FIX.  "Fixar coluna
  TL_FIELDCAT-JUST       = P_JUST. "Alinhar
  TL_FIELDCAT-HOTSPOT    = P_HOT.  "Clique chama evento
  APPEND TL_FIELDCAT TO P_FIELDCAT.

ENDFORM.                    " F_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  F_ALV_ESTRUTURA_NIVEL
*&---------------------------------------------------------------------*
*   Em ALV Grid exibir o primerio nivel da estrutura de DRE
*----------------------------------------------------------------------*
FORM F_ALV_ESTRUTURA_NIVEL USING WA_DRE_001_BUKRS WA_DRE_001_VERSN.
* Busco dados do primeiro nivel da estrutura de DRE
  PERFORM F_ALV_SEL_DADOS_NIVEL USING WA_DRE_001_BUKRS WA_DRE_001_VERSN.
* Montar estruduta de ALV
  PERFORM F_ALV_EST_NIVEL.
* Executa ALV
  PERFORM F_ALV_EXECUTA_NIVEL.
ENDFORM.                    "F_ALV_ESTRUTURA_NIVEL

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Ao clicar na estrutura vai listar todos os niveis
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING P_UCOMM LIKE SY-UCOMM
      P_FIELD TYPE SLIS_SELFIELD.

  READ TABLE IT_DRE_001 INTO WA_DRE_001 INDEX P_FIELD-TABINDEX.
  IF P_UCOMM EQ '&REF'.
    REFRESH: IT_DRE_001,
             IT_FIELDCAT.
    LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
    PERFORM F_ALV_ESTRUTURA.
  ENDIF.

  IF P_FIELD-FIELDNAME = 'VERSN'.
    SET TITLEBAR  '001'.
    PERFORM F_ALV_ESTRUTURA_NIVEL USING WA_DRE_001-BUKRS WA_DRE_001-VERSN.
  ENDIF.

ENDFORM.                    "user_command

*&--------------------------------------------------------------------*
*&      Form  set_pf_status
*&--------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'PF_STATUS_ALV'.
ENDFORM.                    "set_pf_status

*&---------------------------------------------------------------------*
*&      Form  F_ALV_SEL_DADOS_NIVEL
*&---------------------------------------------------------------------*
*       Seleção de dados de níveis.
*----------------------------------------------------------------------*
FORM F_ALV_SEL_DADOS_NIVEL USING WA_DRE_001_BUKRS WA_DRE_001_VERSN .
  SELECT *
    FROM ZGL002_DRE_EST
    INTO TABLE IT_DRE_002
   WHERE TLEVEL EQ 2
     AND BUKRS EQ WA_DRE_001_BUKRS
     AND VERSN EQ WA_DRE_001_VERSN.
ENDFORM.                    " F_ALV_SEL_DADOS_NIVEL

*&---------------------------------------------------------------------*
*&      Form  F_ALV_EST_NIVEL
*&---------------------------------------------------------------------*
*       Estrutura de ALV de
*----------------------------------------------------------------------*
FORM F_ALV_EST_NIVEL .
  CLEAR: IT_FIELDCAT.
  PERFORM F_FIELDCAT USING:
        '0' '' 'IT_DRE_002' 'NIVEL' 'Nível'
        07  ''  ''             '' 'X'  CHANGING IT_FIELDCAT,
        '1' '' 'IT_DRE_002' 'DESNVL' 'Descrição'
        40  ''  ''             '' ' ' CHANGING IT_FIELDCAT.
ENDFORM.                    " F_ALV_EST_NIVEL

*&---------------------------------------------------------------------*
*&      Form  F_ALV_EXECUTA_NIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV_EXECUTA_NIVEL .
* Variavel Local
  DATA: VL_REPID LIKE SY-REPID.

  VL_REPID = SY-REPID.

  IT_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  IT_EVENT-FORM = SLIS_EV_TOP_OF_PAGE.
  APPEND IT_EVENT.

* Determinar a tabela de cores
  VG_LAYOUT-ZEBRA               = 'X'.

* Função para exibir o ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    I_CALLBACK_PROGRAM       = VL_REPID
    I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS_NIVEL'
    I_CALLBACK_USER_COMMAND  = 'USER_COMMAND_NIVEL'
*    i_callback_top_of_page   = 'TOP_OF_PAGE'
    IS_LAYOUT                = VG_LAYOUT
*    i_background_id          = c_enjoy
    IT_FIELDCAT              = IT_FIELDCAT[]
    I_DEFAULT                = 'A'
    I_SAVE                   = 'A'
*    it_events                = it_event[]
  TABLES
    T_OUTTAB                 = IT_DRE_002
  EXCEPTIONS
    PROGRAM_ERROR            = 1
    OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " F_ALV_EXECUTA_NIVEL


*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_NIVEL
*&---------------------------------------------------------------------*
*       Ao clicar na estrutura vai listar todos os niveis
*----------------------------------------------------------------------*
FORM USER_COMMAND_NIVEL  USING P_UCOMM LIKE SY-UCOMM
      P_FIELD TYPE SLIS_SELFIELD.

  READ TABLE IT_DRE_002 INTO WA_DRE_002 INDEX P_FIELD-TABINDEX.

  IF P_UCOMM EQ '&REF'.
    REFRESH: IT_DRE_001,
             IT_FIELDCAT.
    LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
    PERFORM F_ALV_ESTRUTURA.
  ENDIF.

  IF P_FIELD-FIELDNAME = 'NIVEL'.
    SET TITLEBAR '001'.
    PERFORM F_ALV_ESTRUTURA_NIVEL_EQ USING WA_DRE_002-BUKRS WA_DRE_002-VERSN WA_DRE_002-NIVEL ' '.
  ENDIF.

ENDFORM.                    "USER_COMMAND_NIVEL

*&--------------------------------------------------------------------*
*&      Form  set_pf_status_nivel
*&--------------------------------------------------------------------*
FORM SET_PF_STATUS_NIVEL USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'PF_STATUS_ALV'.
ENDFORM.                    "set_pf_status_nivel

*&---------------------------------------------------------------------*
*&      Form  F_ALV_SEL_DADOS_NIVEL_EQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV_SEL_DADOS_NIVEL_EQ  USING WA_DRE_002_BUKRS WA_DRE_002_VERSN WA_DRE_002_NIVEL.

  CLEAR: IT_DRE_008.

  SELECT *
    FROM ZGL008_DRE_TOTAL
    INTO TABLE IT_DRE_008
   WHERE BUKRS EQ WA_DRE_002_BUKRS
     AND VERSN EQ WA_DRE_002_VERSN
     AND NIVEL EQ WA_DRE_002_NIVEL.

  CLEAR: IT_DRE_008_2, WA_DRE_008_2.

  LOOP AT IT_DRE_008 INTO WA_DRE_008.
    CLEAR: WA_DRE_008_2.
    WA_DRE_008_2-BUKRS  = WA_DRE_008-BUKRS.
    WA_DRE_008_2-VERSN  = WA_DRE_008-VERSN.
    WA_DRE_008_2-NIVEL  = WA_DRE_008-NIVEL.
    WA_DRE_008_2-EQUAC  = WA_DRE_008-EQUAC.
    WA_DRE_008_2-DESNVL = WA_DRE_002-DESNVL.
    APPEND WA_DRE_008_2 TO IT_DRE_008_2.
  ENDLOOP.
ENDFORM.                    " F_ALV_SEL_DADOS_NIVEL_EQ

*&---------------------------------------------------------------------*
*&      Form  F_ALV_EST_NIVEL_EQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV_EST_NIVEL_EQ .
  CLEAR: IT_FIELDCAT.
  PERFORM F_FIELDCAT USING:
        '0' '' 'IT_DRE_008_2' 'DESNVL' 'Nível'
        40  ''  ''             '' ' '  CHANGING IT_FIELDCAT,
        '1' '' 'IT_DRE_008_2' 'EQUAC' 'Equação'
        15  ''  ''             '' ' '  CHANGING IT_FIELDCAT.
ENDFORM.  " F_ALV_EST_NIVEL_EQ

*&---------------------------------------------------------------------*
*&      Form  F_ALV_EXECUTA_NIVEL_EQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV_EXECUTA_NIVEL_EQ.
* Variavel Local
  DATA: VL_REPID LIKE SY-REPID.

  VL_REPID = SY-REPID.

  IT_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  IT_EVENT-FORM = SLIS_EV_TOP_OF_PAGE.
  APPEND IT_EVENT.

* Determinar a tabela de cores
  VG_LAYOUT-ZEBRA               = 'X'.

* Função para exibir o ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    I_CALLBACK_PROGRAM       = VL_REPID
    I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS_NIVEL_EQ'
    I_CALLBACK_USER_COMMAND  = 'USER_COMMAND_NIVEL_EQ'
*    i_callback_top_of_page   = 'TOP_OF_PAGE'
    IS_LAYOUT                = VG_LAYOUT
*    i_background_id          = c_enjoy
    IT_FIELDCAT              = IT_FIELDCAT[]
    I_DEFAULT                = 'A'
    I_SAVE                   = 'A'
*    it_events                = it_event[]
  TABLES
    T_OUTTAB                 = IT_DRE_008_2
  EXCEPTIONS
    PROGRAM_ERROR            = 1
    OTHERS                   = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_ALV_EXECUTA_NIVEL_EQ


*&--------------------------------------------------------------------*
*&      Form  SET_PF_STATUS_NIVEL_EQ
*&--------------------------------------------------------------------*
FORM SET_PF_STATUS_NIVEL_EQ USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'TELA'.
ENDFORM.                    "SET_PF_STATUS_NIVEL_EQ

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_NIVEL_EQ
*&---------------------------------------------------------------------*
*       Ao clicar na estrutura vai listar todos os niveis
*----------------------------------------------------------------------*
FORM USER_COMMAND_NIVEL_EQ  USING P_UCOMM LIKE SY-UCOMM
      P_FIELD TYPE SLIS_SELFIELD.

  DATA: VG_MSG(1).

  READ TABLE IT_DRE_008_2 INTO WA_DRE_008_2 INDEX P_FIELD-TABINDEX.

  WA_DRE_008-BUKRS  = WA_DRE_008_2-BUKRS.
  WA_DRE_008-VERSN  = WA_DRE_008_2-VERSN.
  WA_DRE_008-NIVEL  = WA_DRE_008_2-NIVEL.
  WA_DRE_008-EQUAC  = WA_DRE_008_2-EQUAC.

  IF P_UCOMM EQ '&REF'.
    REFRESH: IT_DRE_002,
             IT_FIELDCAT.
    LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
    PERFORM F_ALV_ESTRUTURA_NIVEL USING WA_DRE_008-BUKRS WA_DRE_008-VERSN.
  ENDIF.

  CASE P_UCOMM.
    WHEN 'ATUAL'.
      LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
      PERFORM F_ALV_ESTRUTURA_NIVEL_EQ USING WA_DRE_002-BUKRS WA_DRE_002-VERSN WA_DRE_002-NIVEL ' '.
    WHEN 'CRNVL'.
      SELECT SINGLE *
        INTO WA_DRE_008
        FROM ZGL008_DRE_TOTAL
       WHERE BUKRS EQ WA_DRE_008-BUKRS
         AND VERSN EQ WA_DRE_008-VERSN
         AND NIVEL EQ WA_DRE_002-NIVEL.

      IF SY-SUBRC EQ 0.
        SET TITLEBAR '001'.
        MESSAGE I000(Z01) WITH 'Não pode ser cadastrado mais de uma equação!'.
      ELSE.
        PERFORM F_NOVA_EQUACAO USING VG_MSG.
      ENDIF.
    WHEN 'ATNVL'.
      SELECT SINGLE *
        INTO WA_DRE_008
        FROM ZGL008_DRE_TOTAL
       WHERE BUKRS EQ WA_DRE_008-BUKRS
         AND VERSN EQ WA_DRE_008-VERSN
         AND NIVEL EQ WA_DRE_008-NIVEL.

      IF SY-SUBRC EQ 0.
        PERFORM F_ALTERAR_EQUACAO USING VG_MSG.
      ELSE.
        MESSAGE I000(Z01) WITH 'Deve ser selecionado uma equação!'.
      ENDIF.
    WHEN 'DTNLV'.
      PERFORM F_EXCLUIR_EQUACAO USING VG_MSG.
  ENDCASE.

  IF VG_MSG IS NOT INITIAL.
    CASE VG_MSG.
      WHEN 'I'.
        MESSAGE S000(Z01) WITH 'Equação ' P_EQUAC ' cadastrada com sucesso.'.
      WHEN 'A'.
        MESSAGE S000(Z01) WITH 'Equação ' P_EQUAC ' alterada com sucesso.'.
      WHEN 'E'.
        MESSAGE S000(Z01) WITH 'Equação excluida com sucesso.'.
    ENDCASE.
  ENDIF.

ENDFORM.                    "USER_COMMAND_NIVEL_EQ

*&---------------------------------------------------------------------*
*&      Form  F_ALV_ESTRUTURA_NIVEL_EQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV_ESTRUTURA_NIVEL_EQ  USING    WA_DRE_002_BUKRS
                                        WA_DRE_002_VERSN
                                        WA_DRE_002_NIVEL
                                        P_MSG.

* Busco dados do primeiro nivel da estrutura de DRE
  PERFORM F_ALV_SEL_DADOS_NIVEL_EQ USING WA_DRE_002-BUKRS WA_DRE_002-VERSN WA_DRE_002-NIVEL.
* Montar estruduta de ALV
  PERFORM F_ALV_EST_NIVEL_EQ.
* Executa ALV
  PERFORM F_ALV_EXECUTA_NIVEL_EQ.

ENDFORM.                    " F_ALV_ESTRUTURA_NIVEL_EQ

*&---------------------------------------------------------------------*
*&      Form  F_NOVA_EQUACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_NOVA_EQUACAO USING P_VG_MSG.
  CLEAR: P_EQUAC.
  CALL SELECTION-SCREEN 100 STARTING AT 10 3.
  IF SY-SUBRC EQ 0.
    WA_DRE_008-BUKRS = WA_DRE_002-BUKRS.
    WA_DRE_008-VERSN = WA_DRE_002-VERSN.
    WA_DRE_008-NIVEL = WA_DRE_002-NIVEL.
    WA_DRE_008-EQUAC = P_EQUAC.
    IF P_EQUAC IS NOT INITIAL.
      INSERT INTO ZGL008_DRE_TOTAL VALUES WA_DRE_008.
      P_VG_MSG = 'I'.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_NOVA_EQUACAO

*&---------------------------------------------------------------------*
*&      Form  F_EXCLUIR_EQUACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_EXCLUIR_EQUACAO USING P_VG_MSG.
  DATA: VL_RES             TYPE C LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      TEXTLINE1 = 'Deseja excluir a equação?'
      TITEL     = 'Atenção!'
    IMPORTING
      ANSWER    = VL_RES.

  IF VL_RES EQ 'J'.
    DELETE FROM ZGL008_DRE_TOTAL WHERE BUKRS = WA_DRE_008-BUKRS
                                   AND VERSN = WA_DRE_008-VERSN
                                   AND NIVEL = WA_DRE_008-NIVEL.
    P_VG_MSG = 'E'.
  ENDIF.

ENDFORM.                    " F_EXCLUIR_EQUACAO

*&---------------------------------------------------------------------*
*&      Form  F_ALTERAR_EQUACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALTERAR_EQUACAO USING P_VG_MSG.

  P_EQUAC = WA_DRE_008-EQUAC.

  CALL SELECTION-SCREEN 100 STARTING AT 10 3.
  IF SY-SUBRC EQ 0.
    UPDATE ZGL008_DRE_TOTAL SET EQUAC = P_EQUAC
     WHERE BUKRS = WA_DRE_008-BUKRS
       AND VERSN = WA_DRE_008-VERSN
       AND NIVEL = WA_DRE_008-NIVEL.

    P_VG_MSG = 'A'.
  ENDIF.

ENDFORM.                    " F_ALTERAR_EQUACAO
