******************************************************************************
*                             AMAGGI EXPORTAÇÃO                            ***
******************************************************************************
*** Programa  : ZPM00016                                                   ***
***                                                                        ***
*** Descrição : Consumo de óleo lubrificante por compartimento             ***
**              Especificação feita pelo analista Cleudo                   ***
**                                                                         ***
*** Objetivo  : Controlar de óleo lubrificante por equipamento             ***
***                                                                        ***
*** Versão    Autor               Data          Observações                ***
*** ------    ----------          ----------    --------------             ***
*                                                                          ***
***                                                                        ***
******************************************************************************

REPORT ZPMR0004.

DEFINE MC_PREENCHE_FIELDCAT_FILTER.

  CLEAR GS_FIELDCAT.
  ADD 1 TO GV_POS.
  GS_FIELDCAT-COL_POS       = GV_POS.
  GS_FIELDCAT-FIELDNAME     = &1.
  GS_FIELDCAT-DATATYPE      = &2.
  GS_FIELDCAT-REPTEXT       = &3.
  GS_FIELDCAT-DO_SUM        = &4.
  GS_FIELDCAT-KEY           = &5.
  GS_FIELDCAT-INTTYPE       = &6.

  APPEND GS_FIELDCAT TO GT_FIELDCAT.

END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION .
  PUBLIC SECTION .
    METHODS:
*Double-click Controle
      HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK
                           OF        CL_GUI_ALV_GRID
                           IMPORTING E_ROW_ID
                                     E_COLUMN_ID
                                     ES_ROW_NO,
*Para ser acionado antes dos comandos do usuário
      HANDLE_BEFORE_USER_COMMAND  FOR EVENT BEFORE_USER_COMMAND
                                  OF        CL_GUI_ALV_GRID
                                  IMPORTING E_UCOMM .
  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION .

  METHOD HANDLE_BEFORE_USER_COMMAND .
    PERFORM HANDLE_BEFORE_USER_COMMAND USING E_UCOMM .
  ENDMETHOD .                    "handle_before_user_command

  METHOD HANDLE_HOTSPOT_CLICK .
    PERFORM HANDLE_HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID ES_ROW_NO .
  ENDMETHOD .                    "handle_double_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*** Declaração de constantes
***********************************************************
CONSTANTS: CC_X          TYPE C VALUE 'X',
           CC_M          TYPE C VALUE 'M',
           CC_V          TYPE C VALUE 'V',
           CON_EXIT      TYPE SY-UCOMM VALUE 'EXIT',
           CON_CANC      TYPE SY-UCOMM VALUE 'CANCEL',
           CON_BACK      TYPE SY-UCOMM VALUE 'BACK',
           CC_TT_ANALIT  TYPE C LENGTH 40 VALUE 'Analítico',
           CC_TT_ATV_MOT TYPE C LENGTH 40 VALUE 'Atividade X Motivo',
           CC_TT_REM_CAT TYPE C LENGTH 40 VALUE 'Remonta X Categoria',
           CC_TT_REM_COM TYPE C LENGTH 40 VALUE 'Remonta X Compartimento X Tipo Obj.',
           CC_TT_COR_PRG TYPE C LENGTH 40 VALUE 'Corretiva X Programada'.


*-- Definição global para a ALV
*--- ALV Grid instance reference
DATA GR_ALVGRID              TYPE REF TO CL_GUI_ALV_GRID .
*--- Nome do custom control
DATA GC_CUSTOM_CONTROL_NAME  TYPE SCRFNAME VALUE 'CC_ALV' .
*--- Custom container instance reference
DATA GR_CCONTAINER           TYPE REF TO CL_GUI_CUSTOM_CONTAINER .
*--- Set eventos
DATA GR_EVENT_HANDLER        TYPE REF TO LCL_EVENT_HANDLER .
*--- Field catalog table
DATA: GT_FIELDCAT            TYPE LVC_T_FCAT,
      GS_FIELDCAT            TYPE LVC_S_FCAT.
*--- Layout Estrutura
DATA GS_LAYOUT               TYPE LVC_S_LAYO.

DATA: GT_EXCLUDE             TYPE UI_FUNCTIONS,
      OK_CODE                LIKE SY-UCOMM.

*----------------------------------------------------------*
*           Definição de variáveis globais                 *
*----------------------------------------------------------*

DATA:
        GV_POS      TYPE I,
        V_TABIX     TYPE SY-TABIX        ,  " guardar o indice
        V_PERMISSAO TYPE C               .
*----------------------------------------------------*
*                ALV GRID                            *
*----------------------------------------------------*
DATA: DEF_VARIANT TYPE DISVARIANT,
      VARIANT     TYPE DISVARIANT,
      V_SAVE(1)   TYPE C VALUE 'A'.

*----------------------------------------------------*
*               Types                                *
*----------------------------------------------------*
TYPES:

      BEGIN OF TY_EQUI ,         " Equipamento
          OBJNR TYPE EQUI-OBJNR, " OBJETO
          EQUNR TYPE EQUI-EQUNR, " Codigo do equipamento
          HERST TYPE EQUI-HERST, " marca
          TYPBZ TYPE EQUI-TYPBZ, " Modelo
          BAUJJ TYPE EQUI-BAUJJ, " Ano de construção
          EQART TYPE EQUI-EQART, " Tipo do objeto técnico
          EQTYP TYPE EQUI-EQTYP, " Categoria de equipamento
          IWERK TYPE EQUZ-IWERK, " CENTRO
          TIDNR TYPE EQUZ-TIDNR, " Nº identificação técnica
          ILOAN TYPE EQUZ-ILOAN, " Localização e classificação contábil para o objeto técnico
          KOSTL TYPE ILOA-KOSTL, " Centro de custo

      END OF TY_EQUI,

      BEGIN OF TY_IMRG              , " Ordem
          POINT   TYPE IMRG-POINT   , " Ponto medição
          IDATE   TYPE IMRG-IDATE   , " Data da medição
          DOCAF   TYPE IMRG-DOCAF   , " Indicador: doc.med.foi incluído após medidas correspondentes
          READG   TYPE IMRG-READG   , " Valor medido/posição total do contador em unidade SI
          READGI  TYPE IMRG-READGI  , " Indicador: o campo de números correspondente contém um valor
          RECDV   TYPE IMRG-RECDV   , " Valor medido na unidade de entrada
          RECDVI  TYPE IMRG-RECDVI  , " Indicador: o campo de números correspondente contém um valor
          RECDU   TYPE IMRG-RECDU   , " Unidade de medida ao entrar documento
          CNTRR   TYPE IMRG-CNTRR   , " Posição do contador em unidade SI
          CNTRRI  TYPE IMRG-CNTRRI  , " Indicador: o campo de números correspondente contém um valor
          CDIFF   TYPE IMRG-CDIFF   , " Diferença de posições de numerador em unidade SI
          CDIFFI  TYPE IMRG-CDIFFI  , " Indicador: o campo de números correspondente contém um valor
          CANCL   TYPE IMRG-CANCL   , " Cancelado
          IDIFF   TYPE IMRG-IDIFF   , " IDENTIFICAÇAO DE TRANSFERENCIA
          CODCT   TYPE IMRG-CODCT   , " SUBST. OU REMONTA
          CODGR   TYPE IMRG-CODGR   , " SUBST. OU REMONTA
          VLCOD   TYPE IMRG-VLCOD   , " SUBST. OU REMONTA
          MDTXT   TYPE IMRG-MDTXT   , " Motivo
          PSORT   TYPE IMPTT-LOCAS  , " Compartimento
          MPTYP   TYPE IMPTT-MPTYP  , " Categoria do ponto de medição
          MPOBJ   TYPE IMPTT-MPOBJ  , " Nº objeto do objeto do ponto de medição
          EXPON   LIKE IMPTT-EXPON,       "Exp.10ª potência repres.vírg.flutuante
          DECIM   LIKE IMPTT-DECIM,       "Nº de casas decimais na representação de números
          ATINN   LIKE IMPTT-ATINN, " caracteristica
          INDTR   TYPE IMPTT-INDTR, " Indicador vida util
      END OF TY_IMRG,

      BEGIN OF TY_IMRG_ANT              , " Ordem
          POINT   TYPE IMRG-POINT   , " Ponto medição
          IDATE   TYPE IMRG-IDATE   , " Data da medição
          CNTRR   TYPE IMRG-CNTRR   , " Posição do contador em unidade SI
          RECDV   TYPE IMRG-RECDV   , " valor
          RECDU   TYPE IMRG-RECDU   , " Unidade de medida ao entrar documento
          INDTR   TYPE IMPTT-INDTR   , " IDENTIFICAÇAO DE TRANSFERENCIA
          READG   TYPE IMRG-READG   , " Valor medido/posição total do contador em unidade SI
          CANCL   TYPE IMRG-CANCL   , " doc estornados
          CODCT   TYPE IMRG-CODCT   , " SUBST. OU REMONTA
          CODGR   TYPE IMRG-CODGR   , " SUBST. OU REMONTA
          VLCOD   TYPE IMRG-VLCOD   , " SUBST. OU REMONTA
          PSORT   TYPE IMPTT-LOCAS  , " Compartimento
          MPTYP   TYPE IMPTT-MPTYP  , " Categoria do ponto de medição
          MPOBJ   TYPE IMPTT-MPOBJ  , " Nº objeto do objeto do ponto de medição
          EXPON   LIKE IMPTT-EXPON,       "Exp.10ª potência repres.vírg.flutuante
          DECIM   LIKE IMPTT-DECIM,       "Nº de casas decimais na representação de números
      END OF TY_IMRG_ANT,

      BEGIN OF TY_IMRG_AUX,
          MPOBJ   TYPE IMPTT-MPOBJ,   " Nº objeto do objeto do ponto de medição
      END OF  TY_IMRG_AUX,

      BEGIN OF TY_T001W ,         " Centro
          WERKS TYPE T001W-WERKS, " codigo
          NAME1 TYPE T001W-NAME1, " descrição
      END OF TY_T001W,

      BEGIN OF  TY_CABN, " Característica
        ATINN TYPE CABN-ATINN , " codigo
        ATNAM TYPE CABN-ATNAM , " descrição
      END OF TY_CABN,

      BEGIN OF  TY_T370U, " Descrição do tipo proprietario
        EQTYP TYPE T370U-EQTYP , " codigo
        TYPTX TYPE T370U-TYPTX , " descrição
      END OF TY_T370U,

      BEGIN OF TY_T370K_T, " Descrição do tipo do equipamento
        EQART TYPE T370K_T-EQART , " código
        EARTX TYPE T370K_T-EARTX , " descrição
      END OF TY_T370K_T,

      BEGIN OF TY_QPCT, " Descrição remonta ou substituição
        KATALOGART  TYPE QPCT-KATALOGART ,
        CODEGRUPPE  TYPE QPCT-CODEGRUPPE ,
        CODE        TYPE QPCT-CODE ,
        KURZTEXT    TYPE QPCT-KURZTEXT ,
      END OF TY_QPCT,

      BEGIN OF TY_RELATORIO,      " Dados do relatorio
          IWERK   TYPE EQUZ-IWERK,  " CENTRO
          EQUNR   TYPE EQUI-EQUNR,  " Codigo do equipamento
          OBJNR   TYPE EQUI-OBJNR,  " OBJETO
          POINT   TYPE IMRG-POINT,  " Ponto medição
          ATNAM   TYPE CABN-ATNAM,  " caracteristica
          PSORT   TYPE IMPTT-LOCAS, " Compartimento
          CODCT   TYPE IMRG-CODCT,  " SUBST. OU REMONTA
          CODGR   TYPE IMRG-CODGR,  " SUBST. OU REMONTA
          VLCOD   TYPE IMRG-VLCOD,  " SUBST. OU REMONTA
          LOCAS   TYPE IMPTT-LOCAS,  "
          RECDU   TYPE IMRG-RECDU,  " Unidade de medida ao entrar documento
          SUBS_REM(15) TYPE C,    " DESCRIÇÃO SUBST. OU REMONTA
          HERST   TYPE EQUI-HERST,  " marca
          TYPBZ   TYPE EQUI-TYPBZ,  " Modelo
          BAUJJ   TYPE EQUI-BAUJJ,  " Ano de construção
          EQART(30) TYPE C,       " Tipo do objeto técnico
          EQTYP(30) TYPE C,       " Categoria de equipamento
          TIDNR   TYPE ITOB-TIDNR,  " Identificação tecnica
          KOSTL   TYPE ITOB-KOSTL,  " Centro de Custo
          CONSUMO TYPE MSEG-MENGE," Quantidade de consumo
          HORINI  TYPE MSEG-MENGE, " Horimetro inicial
          DATINI  TYPE SY-DATUM,   " Data inicial
          HORFIM  TYPE MSEG-MENGE, " Horimetro final
          DATFIM  TYPE SY-DATUM,   " Data final
          HORROD  TYPE MSEG-MENGE, " Horas rodadas
          MEDIA   TYPE MSEG-MENGE, " Media consumo
          DIESEL  TYPE MSEG-MENGE, " Quantidade diesel consumido
          ZMDIE   TYPE MSEG-MENGE, " Consumo / Diesel
          MOTIVO  TYPE C LENGTH 30," Motivo
          QUANT   TYPE I,          " Quantidade
      END OF TY_RELATORIO .


" mseg-aufnr, mkpf-mblnr
*----------------------------------------------------*
*                Tabelas Internas                    *
*----------------------------------------------------*
DATA: T_EQUI       TYPE SORTED TABLE OF TY_EQUI
                   WITH HEADER LINE
                   WITH NON-UNIQUE KEY OBJNR ,

      T_IMRG       TYPE STANDARD TABLE OF TY_IMRG
                   WITH NON-UNIQUE KEY MPOBJ ,

      T_IMRG_AUX   TYPE SORTED TABLE OF TY_IMRG_AUX
                   WITH HEADER LINE
                   WITH NON-UNIQUE KEY MPOBJ,
*      t_imrg_ant   type table of ty_imrg_ant  with header line,
      T_CABN       TYPE TABLE OF TY_CABN      WITH HEADER LINE ,
      T_T001W      TYPE TABLE OF TY_T001W     WITH HEADER LINE,
      T_T370U      TYPE TABLE OF TY_T370U     WITH HEADER LINE,
      T_T370K_T    TYPE TABLE OF TY_T370K_T   WITH HEADER LINE,
      T_QPCT       TYPE TABLE OF TY_QPCT      WITH HEADER LINE,
      T_RELATORIO  TYPE TABLE OF TY_RELATORIO,
      W_RELATORIO  TYPE TY_RELATORIO,
      W_IMRG       TYPE TY_IMRG.

*data: w_equi  type equi,
*      w_eqkt  type eqkt,
*      w_equz  type equz,
*      w_iloa  type iloa,
*      w_eqbs  type eqbs,
*      w_fleet type fleet,
*      w_efhm  type efhm.

* Variáveis
DATA: LV_INVISIBLE TYPE I VALUE 0.

*----------------------------------------------------*
*                Parâmetros de Seleção               *
*----------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK BLOCK1
                        WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: S_IWERK FOR T_EQUI-IWERK OBLIGATORY,
                S_IDATE FOR W_IMRG-IDATE OBLIGATORY,
*                S_TIDNR FOR T_EQUI-TIDNR ,
                S_EQUNR FOR T_EQUI-EQUNR ,
                S_EQART FOR T_EQUI-EQART ,
                S_LOCAS FOR W_IMRG-PSORT .

PARAMETERS: P_MPTYP TYPE IMPTT-MPTYP." OBLIGATORY.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN: BEGIN OF BLOCK A2 WITH FRAME TITLE TEXT-002.
PARAMETERS: R_ANALIT RADIOBUTTON GROUP B1 USER-COMMAND F_MUDA_TELA DEFAULT 'X',
            R_AT_MT  RADIOBUTTON GROUP B1,
            R_RE_TP  RADIOBUTTON GROUP B1,
            R_RE_CP  RADIOBUTTON GROUP B1,
            R_CO_PR  RADIOBUTTON GROUP B1.
SELECTION-SCREEN: END OF BLOCK A2.

SELECTION-SCREEN ULINE.

PARAMETERS: P_VARI  TYPE DISVARIANT-VARIANT.

SELECTION-SCREEN: END OF BLOCK BLOCK1.

*---------------------------------------------------*
*               Evento de inicialização             *
*---------------------------------------------------*
INITIALIZATION.

  VARIANT-REPORT = 'ZPM0016'.
  DEF_VARIANT-REPORT = 'ZPM0016'.

* Verificar se existe uma variante default
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = 'A'
    CHANGING
      CS_VARIANT = DEF_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.

  IF SY-SUBRC = 0.
    P_VARI = DEF_VARIANT-VARIANT.
  ENDIF.

*---------------------------------------------------*
*          Evento de seleção de variant             *
*---------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F_F4_VARIANT.

* Selecione para o campo "Óleo/Filtro"
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MPTYP.
  PERFORM F_BUSCA_OLEO_FILTRO.

*---------------------------------------------------*
*Evento para verificar se existe a variant informada*
*---------------------------------------------------*
AT SELECTION-SCREEN.
  IF R_ANALIT IS NOT INITIAL
  OR R_AT_MT  IS NOT INITIAL
  OR R_CO_PR  IS NOT INITIAL.
    IF  P_MPTYP IS INITIAL
    AND LV_INVISIBLE = 0.
      MESSAGE 'Campo "Óleo/Filtro" obrigatório.' TYPE 'E'.
    ENDIF.
  ELSE.
    PERFORM F_VARIANT.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  PERFORM F_MUDA_TELA.

*----------------------------------------------------*
*        Evento: Start-of-Selection                  *
*----------------------------------------------------*
START-OF-SELECTION.

  PERFORM SELECIONA_DADOS.
  CHECK T_EQUI[] IS NOT INITIAL.
  IF T_IMRG[] IS NOT INITIAL.
    PERFORM PREPARA_RELATORIO.
  ELSE.
    IF V_PERMISSAO = 'S'.
      MESSAGE 'Sem permissão para os critérios selecionados.' TYPE 'I'.
    ELSE.
      MESSAGE 'Não encontrados registros.' TYPE 'I'.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

  IF T_RELATORIO[] IS NOT INITIAL.
    CALL SCREEN 100.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  seleciona_dados
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS.
  DATA: L_KOSTL LIKE T_EQUI-KOSTL.

  REFRESH:  T_EQUI      ,
            T_T001W     ,
            T_IMRG      ,
            T_CABN      ,
            T_T370U     ,
            T_T370K_T   ,
            T_QPCT      ,
            T_RELATORIO .

  CLEAR:    V_PERMISSAO .

  PERFORM F_LUPA USING 'Selecionando dados de equipamento. Aguarde...' SPACE.

  " Dados do equipamento
  SELECT OBJNR EQUNR HERST TYPBZ BAUJJ EQART EQTYP SWERK TIDNR ILOAN KOSTL
    INTO TABLE T_EQUI
    FROM V_EQUI
   WHERE EQUNR IN S_EQUNR
     AND EQART IN S_EQART
     AND SWERK IN S_IWERK
     AND EQTYP IN ('V','F').

  FREE T_IMRG_AUX.

  IF T_EQUI[] IS  INITIAL.
    MESSAGE 'Nenhuma informação foi encontrada com o criterio de seleção.' TYPE 'I'.
    EXIT.
  ENDIF.

  " Tipo de proprietário do Equipamento
  SELECT EQTYP TYPTX
    INTO TABLE T_T370U
    FROM T370U
     FOR ALL ENTRIES IN T_EQUI
   WHERE SPRAS   EQ  SY-LANGU
     AND EQTYP  EQ T_EQUI-EQTYP.

  SORT T_T370U BY EQTYP.

  " Tipo de equipamento
  SELECT EQART EARTX
    INTO TABLE T_T370K_T
    FROM T370K_T
     FOR ALL ENTRIES IN T_EQUI
   WHERE SPRAS   EQ  SY-LANGU
     AND EQART   EQ  T_EQUI-EQART.

  SORT  T_T370K_T BY EQART.

  PERFORM F_LUPA USING 'Selecionando movimentação de consumo de lubrificante. Aguarde...' SPACE.

  " Tabela de movimentação do consumo de lubrificante
  SELECT  IMRG~POINT  IMRG~IDATE IMRG~DOCAF IMRG~READG IMRG~READGI IMRG~RECDV
          IMRG~RECDVI IMRG~RECDU IMRG~CNTRR IMRG~CNTRRI IMRG~CDIFF IMRG~CDIFFI
          IMRG~CANCL  IMRG~IDIFF IMRG~CODCT IMRG~CODGR IMRG~VLCOD IMRG~MDTXT
          IMPTT~LOCAS IMPTT~MPTYP IMPTT~MPOBJ IMPTT~EXPON IMPTT~DECIM IMPTT~ATINN
          IMPTT~INDTR
  INTO TABLE T_IMRG
  FROM IMPTT
 INNER JOIN IMRG ON IMRG~POINT =  IMPTT~POINT
   FOR ALL ENTRIES IN T_EQUI
 WHERE IMRG~IDATE  IN S_IDATE
   AND IMPTT~MPOBJ EQ T_EQUI-OBJNR
   AND IMPTT~LOCAS IN S_LOCAS
   AND NOT ( CANCL EQ 'X' OR INDTR EQ 'X' ).

  IF T_IMRG[] IS  INITIAL.
    MESSAGE 'Nenhuma informação foi encontrada com o PERIODO informado.' TYPE 'I'.
    EXIT.

  ENDIF.

  " Textos codes
  SELECT DISTINCT KATALOGART CODEGRUPPE CODE KURZTEXT
    INTO TABLE T_QPCT
    FROM QPCT
     FOR ALL ENTRIES IN T_IMRG
   WHERE KATALOGART = T_IMRG-CODCT
     AND CODEGRUPPE = T_IMRG-CODGR
     AND CODE       = T_IMRG-VLCOD.

  SORT T_QPCT BY KATALOGART CODEGRUPPE CODE.

  " Característica
  SELECT DISTINCT ATINN ATNAM
    INTO TABLE T_CABN
    FROM CABN
     FOR ALL ENTRIES IN T_IMRG
   WHERE ATINN EQ T_IMRG-ATINN.

  SORT T_CABN BY ATINN.

  " Deleta equipamentos que não houve abastecimento de oleo hidraulico
  LOOP AT T_EQUI.
    V_TABIX = SY-TABIX.

    READ TABLE T_IMRG INTO W_IMRG WITH TABLE KEY MPOBJ = T_EQUI-OBJNR.
    IF SY-SUBRC NE 0.
      DELETE T_EQUI INDEX V_TABIX.
* Centro de custo.
    ELSE.
      SELECT SINGLE KOSTL
        FROM ILOA
        INTO (L_KOSTL)
       WHERE ILOAN = T_EQUI-ILOAN.

      T_EQUI-KOSTL = L_KOSTL.

      MODIFY T_EQUI INDEX V_TABIX TRANSPORTING KOSTL.       "Centro de custo.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  prepara_relatorio
*&---------------------------------------------------------------------*
FORM PREPARA_RELATORIO.
  DATA: L_IDATE_AUX  TYPE IMRG-IDATE,
        L_FLSTR      LIKE RIHIMRG-PYEAC,
        L_IMRG       LIKE W_IMRG,
        L_MPOBJ      LIKE W_IMRG-MPOBJ,
        L_OBJNR      LIKE W_RELATORIO-OBJNR,
        ON1          TYPE C VALUE '0',
        L_TABIX_HIGH TYPE SY-TABIX,
        L_TABIX      TYPE SY-TABIX,

        TL_REL_AUX   TYPE TABLE OF TY_RELATORIO WITH HEADER LINE,
        LV_QUANT     TYPE I,
        LV_MOTIVO    TYPE IMRG-MDTXT.

  FIELD-SYMBOLS: <W_IMRG>       TYPE TY_IMRG,
                 <FS_RELATORIO> TYPE TY_RELATORIO.

  ASSIGN W_IMRG TO <W_IMRG> .

  PERFORM F_LUPA USING 'Preparando relatório PRIMEIRO processo. Aguarde...' SPACE.

  IF R_ANALIT IS NOT INITIAL.
* Ordenar tabela
    SORT T_IMRG BY MPOBJ PSORT CODCT CODGR VLCOD MPTYP IDATE.

    LOOP AT T_IMRG INTO <W_IMRG> WHERE MPTYP EQ P_MPTYP
                                  AND  IDATE >= S_IDATE-LOW
                                  AND  IDATE <= S_IDATE-HIGH.
      CLEAR W_RELATORIO.
      CLEAR T_EQUI.
      READ TABLE T_EQUI WITH TABLE KEY OBJNR = <W_IMRG>-MPOBJ.

      W_RELATORIO-EQUNR   = T_EQUI-EQUNR.   " Codigo do equipamento
      W_RELATORIO-OBJNR   = T_EQUI-OBJNR.   " Objeto
      W_RELATORIO-TIDNR   = T_EQUI-TIDNR.   " Objeto técnico.
      W_RELATORIO-KOSTL   = T_EQUI-KOSTL.   " Centro de custo.
      W_RELATORIO-IWERK   = T_EQUI-IWERK.   " Centro de custo.
      W_RELATORIO-POINT   = <W_IMRG>-POINT. " Ponto de medição
      W_RELATORIO-LOCAS   = <W_IMRG>-PSORT. " locas.
      W_RELATORIO-CODCT   = <W_IMRG>-CODCT. " remonta ou substituição
      W_RELATORIO-CODGR   = <W_IMRG>-CODGR. " remonta ou substituição
      W_RELATORIO-VLCOD   = <W_IMRG>-VLCOD. " remonta ou substituição
      W_RELATORIO-RECDU   = <W_IMRG>-RECDU.

      " descrição da característica
      CLEAR T_CABN.
      READ TABLE T_CABN WITH KEY ATINN = <W_IMRG>-ATINN BINARY SEARCH.
      W_RELATORIO-ATNAM   = T_CABN-ATNAM.

      " Busca descrição da remonta ou substituição
      CLEAR T_QPCT.
      READ TABLE T_QPCT WITH KEY  KATALOGART = <W_IMRG>-CODCT
                                  CODEGRUPPE = <W_IMRG>-CODGR
                                  CODE       = <W_IMRG>-VLCOD
                        BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        W_RELATORIO-SUBS_REM = T_QPCT-KURZTEXT .
      ENDIF.

      TRANSLATE W_RELATORIO-SUBS_REM TO UPPER CASE.
      W_RELATORIO-HERST   = T_EQUI-HERST. " marca
      W_RELATORIO-TYPBZ   = T_EQUI-TYPBZ. " Modelo
      W_RELATORIO-BAUJJ   = T_EQUI-BAUJJ. " Ano de construção

      CLEAR T_T370K_T.
      READ TABLE T_T370K_T WITH KEY EQART = T_EQUI-EQART BINARY SEARCH.
      W_RELATORIO-EQART   = T_T370K_T-EARTX. " Tipo do objeto técnico

      CLEAR T_T370U.
      READ TABLE T_T370U WITH KEY EQTYP = T_EQUI-EQTYP BINARY SEARCH.
      W_RELATORIO-EQTYP   = T_T370U-TYPTX. " Categoria de equipamento

      CLEAR:  L_FLSTR.

*   Converter valores para o padrão.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          CHAR_UNIT       = <W_IMRG>-RECDU
          DECIMALS        = <W_IMRG>-DECIM
          EXPONENT        = <W_IMRG>-EXPON
          FLTP_VALUE_SI   = <W_IMRG>-RECDV
          INDICATOR_VALUE = CC_X
        IMPORTING
          CHAR_VALUE      = L_FLSTR
        EXCEPTIONS
          NO_UNIT_GIVEN   = 01.

      TRANSLATE L_FLSTR USING ',.'.
      IF P_MPTYP = 'H'.
        L_FLSTR = L_FLSTR / 1000.
      ENDIF.

      ADD L_FLSTR TO W_RELATORIO-CONSUMO." Consumo
      COLLECT W_RELATORIO INTO T_RELATORIO.
      CLEAR W_RELATORIO.
    ENDLOOP.

*--------------------------------------------------------------------------
    PERFORM F_LUPA USING 'Preparando relatório SEGUNDO processo. Aguarde...' SPACE.

    CLEAR <W_IMRG>.

    LOOP AT T_RELATORIO INTO W_RELATORIO.
* Loop para localizar a indice do proximo registro para limitar a pesquisa
* nos proximos loops.
* localiza primeiro indice do equipamento na tablela t_imrg para iniciar pesquisa.
      IF L_OBJNR NE W_RELATORIO-OBJNR.
        L_OBJNR = W_RELATORIO-OBJNR.
        READ TABLE T_IMRG INTO L_IMRG   WITH KEY MPOBJ = W_RELATORIO-OBJNR
                                        BINARY SEARCH.
        IF SY-SUBRC = 0.
          L_TABIX = SY-TABIX.
        ENDIF.
** localiza ultimo índice do equipamento na tabela interna t_imrg para limitar pesquisa.
        CLEAR: ON1.
        LOOP AT T_IMRG INTO L_IMRG FROM  L_TABIX .
          IF ON1 = 0.
            L_MPOBJ = L_IMRG-MPOBJ.
            ON1     = 1.
          ENDIF.

          IF L_MPOBJ EQ L_IMRG-MPOBJ.
            CONTINUE.
          ELSE.
            L_TABIX_HIGH = SY-TABIX.
            EXIT.

          ENDIF.

        ENDLOOP.
        IF L_TABIX_HIGH IS INITIAL.
          DESCRIBE TABLE T_IMRG LINES L_TABIX_HIGH.
        ENDIF.

      ENDIF.
*   Localizar remonta anterior
      CLEAR: L_IDATE_AUX.

      LOOP AT T_IMRG INTO <W_IMRG> FROM L_TABIX TO L_TABIX_HIGH WHERE MPOBJ = W_RELATORIO-OBJNR
                                                                  AND PSORT = W_RELATORIO-LOCAS
                                                                  AND IDATE < S_IDATE-LOW
                                                                  AND MPTYP = P_MPTYP.
      ENDLOOP.

      IF <W_IMRG> IS INITIAL.
        READ TABLE T_IMRG INTO <W_IMRG> WITH KEY MPOBJ = W_RELATORIO-OBJNR
                                                 PSORT = W_RELATORIO-LOCAS
                                                 VLCOD = W_RELATORIO-VLCOD
                                                 MPTYP = P_MPTYP
                                        BINARY SEARCH.
      ENDIF.
      L_IDATE_AUX = <W_IMRG>-IDATE.

*   Localizar horimetro inicial
      LOOP AT T_IMRG INTO <W_IMRG> FROM L_TABIX TO L_TABIX_HIGH
                                   WHERE MPOBJ  = W_RELATORIO-OBJNR
                                   AND   MPTYP  = CC_M
                                   AND   IDATE >= L_IDATE_AUX.

        CLEAR:  L_FLSTR.
*     Converter valores para o padrão.
        CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
          EXPORTING
            CHAR_UNIT       = <W_IMRG>-RECDU
            DECIMALS        = <W_IMRG>-DECIM
            EXPONENT        = <W_IMRG>-EXPON
            FLTP_VALUE_SI   = <W_IMRG>-READG "t_IMRG-CNTRR
            INDICATOR_VALUE = CC_X
          IMPORTING
            CHAR_VALUE      = L_FLSTR
          EXCEPTIONS
            NO_UNIT_GIVEN   = 01.

        TRANSLATE L_FLSTR USING ',.'.

        MOVE    L_FLSTR        TO W_RELATORIO-HORINI.
        MOVE    <W_IMRG>-IDATE TO W_RELATORIO-DATINI.
        W_RELATORIO-RECDU = <W_IMRG>-RECDU.
        EXIT.

      ENDLOOP.

*   Localizar horimetro final
      IF S_IDATE-HIGH IS NOT INITIAL.
        L_IDATE_AUX = S_IDATE-HIGH.
      ELSE.
        L_IDATE_AUX = S_IDATE-LOW.
      ENDIF.

      LOOP AT T_IMRG INTO <W_IMRG>   FROM L_TABIX TO L_TABIX_HIGH
                                     WHERE MPOBJ  = W_RELATORIO-OBJNR
                                     AND   MPTYP  = CC_M
                                     AND   IDATE <= L_IDATE_AUX.

      ENDLOOP.

      CLEAR:  L_FLSTR.

*     Converter valores para o padrão.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          CHAR_UNIT       = <W_IMRG>-RECDU
          DECIMALS        = <W_IMRG>-DECIM
          EXPONENT        = <W_IMRG>-EXPON
          FLTP_VALUE_SI   = <W_IMRG>-READG "t_IMRG-CNTRR
          INDICATOR_VALUE = CC_X
        IMPORTING
          CHAR_VALUE      = L_FLSTR
        EXCEPTIONS
          NO_UNIT_GIVEN   = 01.

      TRANSLATE L_FLSTR USING ',.'.

      MOVE: L_FLSTR          TO W_RELATORIO-HORFIM,
            <W_IMRG>-IDATE   TO W_RELATORIO-DATFIM.

      IF W_RELATORIO-HORFIM IS NOT INITIAL.
        W_RELATORIO-HORROD = W_RELATORIO-HORFIM - W_RELATORIO-HORINI .

        IF W_RELATORIO-RECDU EQ 'H'.
          IF W_RELATORIO-HORROD EQ '0.00'.
            W_RELATORIO-MEDIA =  '0.00'.
          ELSE.
            W_RELATORIO-MEDIA =  W_RELATORIO-CONSUMO / W_RELATORIO-HORROD.
          ENDIF.

        ELSE.
          IF W_RELATORIO-CONSUMO EQ '0.00'.
            W_RELATORIO-MEDIA =  '0.00'.
          ELSE.
            W_RELATORIO-MEDIA   =  W_RELATORIO-HORROD / W_RELATORIO-CONSUMO.
          ENDIF.

        ENDIF.

      ENDIF.

*   Calcular quantidade de diesel
      LOOP AT T_IMRG INTO <W_IMRG> FROM L_TABIX TO L_TABIX_HIGH
                                   WHERE MPOBJ  = W_RELATORIO-OBJNR
                                   AND   MPTYP  = CC_V
                                   AND   IDATE >= S_IDATE-LOW
                                   AND   IDATE <= S_IDATE-HIGH.

        CLEAR:  L_FLSTR.
*     Converter valores para o padrão.
        CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
          EXPORTING
            CHAR_UNIT       = <W_IMRG>-RECDU
            DECIMALS        = <W_IMRG>-DECIM
            EXPONENT        = <W_IMRG>-EXPON
            FLTP_VALUE_SI   = <W_IMRG>-RECDV
            INDICATOR_VALUE = CC_X
          IMPORTING
            CHAR_VALUE      = L_FLSTR
          EXCEPTIONS
            NO_UNIT_GIVEN   = 01.

        TRANSLATE L_FLSTR USING ',.'.

        IF P_MPTYP = 'H'.
          L_FLSTR = L_FLSTR / 1000.
        ENDIF.

        TRY.
            ADD: L_FLSTR TO W_RELATORIO-DIESEL.
          CATCH CX_ROOT.

        ENDTRY.
      ENDLOOP.

*   Calcular consumo / diesel
      IF W_RELATORIO-DIESEL > 0
      AND W_RELATORIO-LOCAS = 'F100'.
        W_RELATORIO-ZMDIE = W_RELATORIO-CONSUMO * 100 / W_RELATORIO-DIESEL.
      ENDIF.

      MODIFY T_RELATORIO FROM W_RELATORIO.
      CLEAR <W_IMRG>.

    ENDLOOP.

    SORT T_RELATORIO BY IWERK EQUNR HORINI.

  ELSEIF R_AT_MT IS NOT INITIAL .
**  Visão sintética (Atividade X Motivo - Filtro/Óleo)
    LOOP AT T_QPCT.
      LOOP AT T_IMRG ASSIGNING <W_IMRG> WHERE CODCT = T_QPCT-KATALOGART
                                         AND  CODGR = T_QPCT-CODEGRUPPE
                                         AND  VLCOD = T_QPCT-CODE
                                         AND  MPTYP = P_MPTYP.

        READ TABLE TL_REL_AUX WITH KEY  SUBS_REM = T_QPCT-KURZTEXT(15)
                                        MOTIVO   = <W_IMRG>-MDTXT.
        IF SY-SUBRC IS INITIAL.
*  *     Buscar se ja existe também o motivo
          ADD 1 TO TL_REL_AUX-QUANT.
          MODIFY TL_REL_AUX TRANSPORTING QUANT WHERE SUBS_REM = T_QPCT-KURZTEXT(15)
                                                AND  MOTIVO   = <W_IMRG>-MDTXT.
          CONTINUE.
        ENDIF.

        TL_REL_AUX-QUANT    = 1.
        TL_REL_AUX-MOTIVO   = <W_IMRG>-MDTXT.
        TL_REL_AUX-SUBS_REM = T_QPCT-KURZTEXT(15).
        APPEND TL_REL_AUX.

      ENDLOOP.

    ENDLOOP.

    T_RELATORIO[] = TL_REL_AUX[].

    SORT T_RELATORIO BY SUBS_REM MOTIVO.

  ELSEIF R_RE_TP IS NOT INITIAL.
**  Visão sintética (Remonta X Categoria)

    LOOP AT T_QPCT WHERE CODE EQ '0020'.
      LOOP AT T_IMRG ASSIGNING <W_IMRG> WHERE CODCT = T_QPCT-KATALOGART
                                         AND  CODGR = T_QPCT-CODEGRUPPE
                                         AND  VLCOD = T_QPCT-CODE
                                         AND  MPTYP = 'H'.

        LOOP AT T_EQUI WHERE OBJNR = <W_IMRG>-MPOBJ.

          READ TABLE T_T370K_T WITH KEY EQART = T_EQUI-EQART BINARY SEARCH.
          W_RELATORIO-EQART   = T_T370K_T-EARTX.

          READ TABLE TL_REL_AUX WITH KEY  SUBS_REM = T_QPCT-KURZTEXT(15)
                                          EQART    = T_T370K_T-EARTX.
          IF SY-SUBRC IS INITIAL.
*  *     Buscar se ja existe também o motivo
            ADD 1 TO TL_REL_AUX-QUANT.
            MODIFY TL_REL_AUX TRANSPORTING QUANT WHERE SUBS_REM = T_QPCT-KURZTEXT(15)
                                                  AND  EQART    = T_T370K_T-EARTX.
            CONTINUE.
          ENDIF.

          TL_REL_AUX-QUANT    = 1.
          TL_REL_AUX-EQART    = T_T370K_T-EARTX.
          TL_REL_AUX-SUBS_REM = T_QPCT-KURZTEXT(15).
          APPEND TL_REL_AUX.

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    T_RELATORIO[] = TL_REL_AUX[].

    SORT T_RELATORIO BY SUBS_REM EQART.

  ELSEIF R_RE_CP IS NOT INITIAL.
**  Visão sintética (Remonta X Compartimento X Tipo Obj.)

    LOOP AT T_QPCT WHERE CODE EQ '0020'.
      LOOP AT T_IMRG ASSIGNING <W_IMRG> WHERE CODCT = T_QPCT-KATALOGART
                                         AND  CODGR = T_QPCT-CODEGRUPPE
                                         AND  VLCOD = T_QPCT-CODE
                                         AND  MPTYP = 'H'.

        LOOP AT T_EQUI WHERE OBJNR = <W_IMRG>-MPOBJ.

          READ TABLE T_T370K_T WITH KEY EQART = T_EQUI-EQART BINARY SEARCH.
          W_RELATORIO-EQART   = T_T370K_T-EARTX.

          READ TABLE TL_REL_AUX WITH KEY  SUBS_REM = T_QPCT-KURZTEXT(15)
                                          EQART    = T_T370K_T-EARTX
                                          LOCAS    = <W_IMRG>-PSORT.
          IF SY-SUBRC IS INITIAL.
            ADD 1 TO TL_REL_AUX-QUANT.
            MODIFY TL_REL_AUX TRANSPORTING QUANT WHERE SUBS_REM = T_QPCT-KURZTEXT(15)
                                                  AND  EQART    = T_T370K_T-EARTX
                                                  AND  LOCAS    = <W_IMRG>-PSORT.
            CONTINUE.

          ENDIF.

          TL_REL_AUX-QUANT    = 1.
          TL_REL_AUX-EQART    = T_T370K_T-EARTX.
          TL_REL_AUX-LOCAS    = <W_IMRG>-PSORT.
          TL_REL_AUX-SUBS_REM = T_QPCT-KURZTEXT(15).
          APPEND TL_REL_AUX.

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    T_RELATORIO[] = TL_REL_AUX[].

    SORT T_RELATORIO BY SUBS_REM EQART LOCAS.

  ELSEIF R_CO_PR IS NOT INITIAL.
**  Visão sintética (Corretiva X Programada)

    LOOP AT T_QPCT.
      LOOP AT T_IMRG ASSIGNING <W_IMRG> WHERE CODCT = T_QPCT-KATALOGART
                                         AND  CODGR = T_QPCT-CODEGRUPPE
                                         AND  VLCOD = T_QPCT-CODE
                                         AND  MPTYP = P_MPTYP.

        IF <W_IMRG>-VLCOD = '0010'
        OR <W_IMRG>-VLCOD = '0020'.
          LV_MOTIVO = 'Programadas'.
          READ TABLE TL_REL_AUX WITH KEY LOCAS  = <W_IMRG>-PSORT
                                         MOTIVO = 'Programadas'.
          IF SY-SUBRC IS INITIAL.
            ADD 1 TO TL_REL_AUX-QUANT.
            MODIFY TL_REL_AUX TRANSPORTING QUANT WHERE LOCAS  = <W_IMRG>-PSORT
                                                  AND  MOTIVO = 'Programadas'.
            CONTINUE.
          ENDIF.

        ELSEIF <W_IMRG>-VLCOD = '0030'
        OR     <W_IMRG>-VLCOD = '0040'
        OR     <W_IMRG>-VLCOD = '0050'
        OR     <W_IMRG>-VLCOD = '0060'
        OR     <W_IMRG>-VLCOD = '0070'.
          LV_MOTIVO = 'Corretivas'.
          READ TABLE TL_REL_AUX WITH KEY LOCAS  = <W_IMRG>-PSORT
                                         MOTIVO = LV_MOTIVO.
          IF SY-SUBRC IS INITIAL.
            ADD 1 TO TL_REL_AUX-QUANT.
            MODIFY TL_REL_AUX TRANSPORTING QUANT WHERE LOCAS  = <W_IMRG>-PSORT
                                                  AND  MOTIVO = LV_MOTIVO.
            CONTINUE.

          ENDIF.

        ENDIF.

        TL_REL_AUX-MOTIVO = LV_MOTIVO.
        TL_REL_AUX-QUANT  = 1.
        TL_REL_AUX-LOCAS  = <W_IMRG>-PSORT.
        APPEND TL_REL_AUX.

      ENDLOOP.
    ENDLOOP.
    T_RELATORIO[] = TL_REL_AUX[].

    SORT T_RELATORIO BY PSORT MOTIVO.

  ENDIF.

  IF T_RELATORIO[] IS  INITIAL.
    MESSAGE 'Nenhuma informação foi encontrada com o PERIODO informado.' TYPE 'I'.
    EXIT.

  ENDIF.

  FREE:  T_EQUI[]    ,
         T_T001W[]   ,
         T_IMRG      ,
         T_CABN[]    ,
         T_T370U[]   ,
         T_T370K_T[] ,
         T_QPCT[]    .

ENDFORM.                    " prepara_relatorio

*&---------------------------------------------------------------------*
*&      Form  F_F4_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_F4_VARIANT .

  DATA: VARIANT_EXIT(1) TYPE C.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT 	= VARIANT
            I_SAVE      = 'A'
       IMPORTING
            E_EXIT      = VARIANT_EXIT
            ES_VARIANT 	= DEF_VARIANT
       EXCEPTIONS
            NOT_FOUND  	= 2.

  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF VARIANT_EXIT = SPACE.
      P_VARI = DEF_VARIANT-VARIANT.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_F4_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VARIANT .

  IF NOT P_VARI IS INITIAL.

    MOVE VARIANT TO DEF_VARIANT.
    MOVE P_VARI TO DEF_VARIANT-VARIANT.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        I_SAVE     = 'A'
      CHANGING
        CS_VARIANT = DEF_VARIANT.

    VARIANT = DEF_VARIANT.

  ELSE.
    CLEAR VARIANT.
    VARIANT-REPORT = SY-REPID.
  ENDIF.

ENDFORM.                    " F_VARIANT


*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV .
  IF GR_ALVGRID IS INITIAL .
*----Criando o custom container instance
    CREATE OBJECT GR_CCONTAINER
      EXPORTING
        CONTAINER_NAME              = GC_CUSTOM_CONTROL_NAME
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

*----Criando alv grid instance
    CREATE OBJECT GR_ALVGRID
      EXPORTING
        I_PARENT          = GR_CCONTAINER
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

*--Criando instance para event handler
    CREATE OBJECT GR_EVENT_HANDLER .
*--Preparando  field catalog.
    PERFORM PREPARE_FIELD_CATALOG  .
*--Preparando layout structure
    PERFORM PREPARE_LAYOUT CHANGING GS_LAYOUT .
*--Exclui Bottons
    PERFORM EXCLUDE_TB_FUNCTIONS CHANGING GT_EXCLUDE.
*--Registrando methods para para manipular eventos da ALV Grid
    SET HANDLER GR_EVENT_HANDLER->HANDLE_BEFORE_USER_COMMAND FOR GR_ALVGRID .
    SET HANDLER GR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK       FOR GR_ALVGRID .
*-- Exibe ALV
    CALL METHOD GR_ALVGRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT                    = VARIANT
        I_SAVE                        = V_SAVE
        I_DEFAULT                     = 'X'
        IS_LAYOUT                     = GS_LAYOUT
        IT_TOOLBAR_EXCLUDING          = GT_EXCLUDE
      CHANGING
        IT_OUTTAB                     = T_RELATORIO
        IT_FIELDCATALOG               = GT_FIELDCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

  ELSE .
    CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY
      EXCEPTIONS
        FINISHED = 1
        OTHERS   = 2.

  ENDIF .
ENDFORM .                    "display_alv


*----------------------------------------------------------------------*
*  MODULE display_alv OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE D100_DISPLAY_ALV OUTPUT.
  SET PF-STATUS 'S100'.
  SET TITLEBAR  'T100'.
  PERFORM DISPLAY_ALV .

ENDMODULE.                    "display_alv OUTPUT

*----------------------------------------------------------------------*
*  MODULE d100_exit INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE D100_EXIT INPUT.
  CASE  SY-UCOMM.
    WHEN CON_EXIT OR CON_BACK.
      CALL METHOD GR_ALVGRID->FREE.
      CALL METHOD CL_GUI_CFW=>FLUSH.
      CLEAR GR_CCONTAINER.
      CLEAR GR_EVENT_HANDLER.
      REFRESH: T_RELATORIO.
      LEAVE TO SCREEN 0.
    WHEN CON_CANC .
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT


*&---------------------------------------------------------------------*
*&      Form  prepare_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_LAYOUT  text
*----------------------------------------------------------------------*
FORM PREPARE_LAYOUT CHANGING PS_LAYOUT TYPE LVC_S_LAYO.
  DATA: L_CABEC(220)  TYPE C,
*        L_CABEC1(70) TYPE C,
*        L_CABEC2(70) TYPE C,
        LS_T001W     TYPE TY_T001W.

  IF R_ANALIT IS NOT INITIAL.
    L_CABEC = CC_TT_ANALIT .
  ELSEIF R_AT_MT IS NOT INITIAL .
    L_CABEC = CC_TT_ATV_MOT .
  ELSEIF R_RE_TP IS NOT INITIAL.
    L_CABEC = CC_TT_REM_CAT .
  ELSEIF R_RE_CP IS NOT INITIAL.
    L_CABEC = CC_TT_REM_COM .
  ELSEIF R_CO_PR IS NOT INITIAL.
    L_CABEC = CC_TT_COR_PRG .
  ENDIF.

  PS_LAYOUT-ZEBRA        = 'X' .  " Impressão Zebra
  PS_LAYOUT-GRID_TITLE   = L_CABEC .
  PS_LAYOUT-CWIDTH_OPT   = 'X'.   " otimiza a largura da coluna
  PS_LAYOUT-SEL_MODE     = 'A'.   " Seleção de Coluna

ENDFORM.                    "prepare_layout


*&---------------------------------------------------------------------*
*&      Form  handle_hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_ROW      text
*      -->I_COLUMN   text
*      -->IS_ROW_NO  text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK USING I_ROW     TYPE LVC_S_ROW
                               I_COLUMN  TYPE LVC_S_COL
                               IS_ROW_NO TYPE LVC_S_ROID.

  READ TABLE T_RELATORIO INTO W_RELATORIO INDEX IS_ROW_NO-ROW_ID  .
  IF SY-SUBRC = 0 AND I_COLUMN-FIELDNAME = 'EQUNR' .
    PERFORM F_LUPA USING 'Exibe equipamento. Aguarde...' SPACE.
    SET PARAMETER ID 'EQN' FIELD W_RELATORIO-EQUNR.
    CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .
  ENDIF.

  IF I_COLUMN-FIELDNAME = 'POINT' OR
     I_COLUMN-FIELDNAME = 'CONSUMO'.
    DATA:
      T_RSPARAMS TYPE RSPARAMS_TT,
      W_RSPARAMS TYPE RSPARAMS.

    CLEAR W_RSPARAMS.

    W_RSPARAMS-SELNAME = 'POINT'.
    W_RSPARAMS-KIND    = 'S'.
    W_RSPARAMS-SIGN    = 'I'.
    W_RSPARAMS-OPTION  = 'EQ'.
    W_RSPARAMS-LOW     = W_RELATORIO-POINT. " selfield-value.
    APPEND W_RSPARAMS TO T_RSPARAMS.
    CLEAR W_RSPARAMS.

    W_RSPARAMS-SELNAME = 'IDATEV'.
    W_RSPARAMS-KIND    = 'S'.
    W_RSPARAMS-SIGN    = 'I'.
    W_RSPARAMS-OPTION  = 'BT'.
    W_RSPARAMS-LOW     = S_IDATE-LOW.
    W_RSPARAMS-HIGH    = S_IDATE-HIGH.
    APPEND W_RSPARAMS TO T_RSPARAMS.
    CLEAR W_RSPARAMS.

    W_RSPARAMS-SELNAME = 'VLCOD'.
    W_RSPARAMS-KIND    = 'S'.
    W_RSPARAMS-SIGN    = 'I'.
    W_RSPARAMS-OPTION  = 'EQ'.
    W_RSPARAMS-LOW     = W_RELATORIO-VLCOD.
    APPEND W_RSPARAMS TO T_RSPARAMS.
    CLEAR W_RSPARAMS.

    W_RSPARAMS-SELNAME = 'CODCT'.
    W_RSPARAMS-KIND    = 'S'.
    W_RSPARAMS-SIGN    = 'I'.
    W_RSPARAMS-OPTION  = 'EQ'.
    W_RSPARAMS-LOW     = W_RELATORIO-CODCT.
    APPEND W_RSPARAMS TO T_RSPARAMS.
    CLEAR W_RSPARAMS.

    W_RSPARAMS-SELNAME = 'CODGR'.
    W_RSPARAMS-KIND    = 'S'.
    W_RSPARAMS-SIGN    = 'I'.
    W_RSPARAMS-OPTION  = 'EQ'.
    W_RSPARAMS-LOW     = W_RELATORIO-CODGR.
    APPEND W_RSPARAMS TO T_RSPARAMS.
    CLEAR W_RSPARAMS.

    W_RSPARAMS-SELNAME = 'VLCOD'.
    W_RSPARAMS-KIND    = 'S'.
    W_RSPARAMS-SIGN    = 'I'.
    W_RSPARAMS-OPTION  = 'EQ'.
    W_RSPARAMS-LOW     = W_RELATORIO-VLCOD.
    APPEND W_RSPARAMS TO T_RSPARAMS.
    CLEAR W_RSPARAMS.

    W_RSPARAMS-SELNAME = 'SELCOUNT'.
    W_RSPARAMS-KIND    = 'S'.
    W_RSPARAMS-SIGN    = 'I'.
    W_RSPARAMS-OPTION  = 'EQ'.
    W_RSPARAMS-LOW     = '99999'.
    APPEND W_RSPARAMS TO T_RSPARAMS.
    CLEAR W_RSPARAMS.

    SUBMIT RIIMR020 WITH SELECTION-TABLE T_RSPARAMS AND RETURN.

  ENDIF.

ENDFORM .                    "handle_hotspot_click


*&---------------------------------------------------------------------*
*&      Form  handle_before_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_UCOMM    text
*----------------------------------------------------------------------*
FORM HANDLE_BEFORE_USER_COMMAND USING I_UCOMM TYPE SYUCOMM .
  DATA: LT_FILTER            TYPE LVC_T_COL  WITH HEADER LINE ,
        LT_FCAT              TYPE LVC_T_FCAT WITH HEADER LINE ,
        LT_FILTER_CRIT       TYPE LVC_T_FILT WITH HEADER LINE ,
        LT_FILTERED_ENTRIES  TYPE LVC_T_FIDX.

  CASE I_UCOMM .
    WHEN '&MB_FILTER' .
      GR_ALVGRID->GET_FRONTEND_FIELDCATALOG( IMPORTING ET_FIELDCATALOG = LT_FCAT[] ).

      LOOP AT LT_FCAT.
        CASE LT_FCAT-FIELDNAME.
          WHEN 'TYPBZ'.
          WHEN 'LOCAS'.
          WHEN 'SUBS_REM'.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
        MOVE-CORRESPONDING LT_FCAT TO LT_FILTER_CRIT.
        APPEND LT_FILTER_CRIT.
      ENDLOOP.

      GR_ALVGRID->SET_FILTER_CRITERIA( EXPORTING IT_FILTER  = LT_FILTER_CRIT[] ) .

  ENDCASE .

ENDFORM .                    "handle_before_user_command

*&---------------------------------------------------------------------*
*&      Form  prepare_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PREPARE_FIELD_CATALOG  .
  DATA: ALV_STYLE_FONT_BOLD(4)    TYPE X VALUE '00000020'.

  IF R_ANALIT IS NOT INITIAL.
    MC_PREENCHE_FIELDCAT_FILTER:
      'IWERK'            'CHAR' 'Centro'                    ' ' 'X' 'C' ,
      'EQUNR'            'CHAR' 'Equipamento'               ' ' 'X' 'C' ,
      'POINT'            'CHAR' 'Ponto'                     ' ' 'X' 'C' ,
      'LOCAS'            'CHAR' 'Compartimento'             ' ' ' ' 'C' ,
      'ATNAM'            'CHAR' 'Característica'            ' ' ' ' 'C' ,
      'SUBS_REM'         'CHAR' 'Tipo catálogo'             ' ' ' ' 'C' ,
      'EQART'            'CHAR' 'Tipo objeto'               ' ' ' ' 'C' ,
      'HERST'            'CHAR' 'Fabricante'                ' ' ' ' 'C' ,
      'TYPBZ'            'CHAR' 'Modelo'                    ' ' ' ' 'C' ,
*      'KOSTL'            'CHAR' 'Centro de Custo'           ' ' ' ' 'C' ,
      'BAUJJ'            'CHAR' 'Ano'                       ' ' ' ' 'C' ,
*      'EQTYP'            'CHAR' 'Categoria'                 ' ' ' ' 'C' ,
      'CONSUMO'          'CURR' 'Consumo'                   ' ' ' ' 'P' ,
      'DATINI'           'DATS' 'Data inicio'               ' ' ' ' 'D' ,
      'HORINI'           'CURR' 'Início Hod/Hor'            ' ' ' ' 'P' ,
      'DATFIM'           'DATS' 'Data Final'                ' ' ' ' 'D' ,
      'HORFIM'           'CURR' 'Final  Hod/Hor'            ' ' ' ' 'P' ,
      'HORROD'           'CURR' 'Hod/Hor Rodados'           ' ' ' ' 'P' ,
      'MEDIA'            'CURR' 'Média consumo'             ' ' ' ' 'P' ,
      'DIESEL'           'CURR' 'Consumo Diesel'            ' ' ' ' 'P' ,
      'ZMDIE'            'CURR' '% Média diesel'            ' ' ' ' 'P' .

    READ TABLE GT_FIELDCAT INTO GS_FIELDCAT WITH KEY FIELDNAME = 'TIDNR' .
    IF SY-SUBRC = 0.
      GS_FIELDCAT-JUST        = 'C'.
      MODIFY  GT_FIELDCAT FROM GS_FIELDCAT INDEX SY-TABIX TRANSPORTING JUST .
    ENDIF.

    READ TABLE GT_FIELDCAT INTO GS_FIELDCAT WITH KEY FIELDNAME = 'POINT' .
    IF SY-SUBRC = 0.
      GS_FIELDCAT-NO_ZERO     = 'X'.
      GS_FIELDCAT-JUST        = 'C'.
      GS_FIELDCAT-HOTSPOT     = 'X'.
      MODIFY  GT_FIELDCAT FROM GS_FIELDCAT INDEX SY-TABIX TRANSPORTING JUST NO_ZERO HOTSPOT .
    ENDIF.

    READ TABLE GT_FIELDCAT INTO GS_FIELDCAT WITH KEY FIELDNAME = 'EQUNR' .
    IF SY-SUBRC = 0.
      GS_FIELDCAT-OUTPUTLEN   =  13.
      GS_FIELDCAT-NO_ZERO     = 'X'.
      GS_FIELDCAT-JUST        = 'C'.
      GS_FIELDCAT-HOTSPOT     = 'X'.
      MODIFY  GT_FIELDCAT FROM GS_FIELDCAT INDEX SY-TABIX TRANSPORTING OUTPUTLEN JUST NO_ZERO HOTSPOT .
    ENDIF.

  ELSEIF R_AT_MT IS NOT INITIAL .
**  Visão sintética (Atividade X Motivo)
    MC_PREENCHE_FIELDCAT_FILTER:
      'SUBS_REM'         'CHAR' 'Tipo catálogo'             ' ' ' ' 'C' ,
      'MOTIVO'           'CHAR' 'Motivo'                    ' ' ' ' 'C' ,
      'QUANT'            'INT'  'Quantidade'                ' ' ' ' 'I' .
  ELSEIF R_RE_TP IS NOT INITIAL.
    MC_PREENCHE_FIELDCAT_FILTER:
      'SUBS_REM'         'CHAR' 'Tipo catálogo'             ' ' ' ' 'C' ,
      'EQART'            'CHAR' 'Tipo objeto'               ' ' ' ' 'C' ,
      'QUANT'            'INT'  'Quantidade'                ' ' ' ' 'I' .
  ELSEIF R_RE_CP IS NOT INITIAL.
    MC_PREENCHE_FIELDCAT_FILTER:
     'SUBS_REM'         'CHAR' 'Tipo catálogo'             ' ' ' ' 'C' ,
     'EQART'            'CHAR' 'Tipo objeto'               ' ' ' ' 'C' ,
     'LOCAS'            'CHAR' 'Compartimento'             ' ' ' ' 'C' ,
     'QUANT'            'INT'  'Quantidade'                ' ' ' ' 'I' .
  ELSEIF R_CO_PR IS NOT INITIAL.
    MC_PREENCHE_FIELDCAT_FILTER:
      'LOCAS'            'CHAR' 'Compartimento'             ' ' ' ' 'C' ,
      'MOTIVO'           'CHAR' 'Motivo'                    ' ' ' ' 'C' ,
      'QUANT'            'INT'  'Quantidade'                ' ' ' ' 'I' .
  ENDIF.

ENDFORM.                    "prepare_field_catalog

*&---------------------------------------------------------------------*
*&      Form  d03_exclude_tb_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_EXCLUDE  text
*----------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS  CHANGING P_GT_EXCLUDE TYPE UI_FUNCTIONS.
  APPEND CL_GUI_ALV_GRID=>MC_FC_PRINT           TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_MB_VIEW            TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_INFO            TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW  TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY        TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW    TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO        TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_FIND            TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_MB_PASTE           TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT         TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW  TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW  TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_MB_VIEW            TO P_GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_CHECK           TO P_GT_EXCLUDE. " Botão Verificar
  APPEND CL_GUI_ALV_GRID=>MC_FC_REFRESH         TO P_GT_EXCLUDE. " Botão Renovar

ENDFORM.                    "d03_exclude_tb_functions


*&---------------------------------------------------------------------*
*&      Form  F_LUPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MSG1     text
*      -->P_MSG2     text
*----------------------------------------------------------------------*
FORM F_LUPA USING P_MSG1 P_MSG2.
  DATA: VL_MESSAGE(150) TYPE C.
  CLEAR VL_MESSAGE.

  CONCATENATE P_MSG1 P_MSG2 INTO VL_MESSAGE SEPARATED BY SPACE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = 99
      TEXT       = VL_MESSAGE.

ENDFORM. "f_lupa
*&---------------------------------------------------------------------*
*&      Form  F_MUDA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MUDA_TELA .
*  IF R_ANALIT IS NOT INITIAL
*  OR R_AT_MT  IS NOT INITIAL
*  OR R_RE_TP  IS NOT INITIAL.
*    IF P_MPTYP IS INITIAL.
*      MESSAGE 'Campo "Óleo/Filtro" obrigatório.' TYPE 'E'.
*    ENDIF.
*  ELSE.
*    P_MPTYP = 'H'.
  LOOP AT SCREEN.
    IF R_RE_TP EQ 'X'
    OR R_RE_CP EQ 'X'.
      CASE SCREEN-NAME.
        WHEN: 'P_MPTYP' OR '%_P_MPTYP_%_APP_%-TEXT'.
          LV_INVISIBLE = 1.
          SCREEN-INVISIBLE = 1.
          SCREEN-OUTPUT    = 0.
          SCREEN-INPUT     = 0.
          MODIFY SCREEN.
          CONTINUE.
      ENDCASE.
    ELSE.
      CLEAR LV_INVISIBLE.
      SCREEN-INVISIBLE = 0.
      SCREEN-OUTPUT    = 1.
      SCREEN-INPUT     = 1.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.
*  ENDIF.
ENDFORM.                    " F_MUDA_TELA

*&---------------------------------------------------------------------*
*&      Form  f_busca_oleo_filtro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_BUSCA_OLEO_FILTRO.
  DATA: BEGIN OF TL_TEMP OCCURS 0,
         MPTYP TYPE IMPTT-MPTYP,
         MDTXT TYPE IMRG-MDTXT,
       END OF TL_TEMP.

  DATA: TL_RETURN TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC  TYPE TABLE OF DSELC      WITH HEADER LINE.

  CLEAR TL_TEMP.
  TL_TEMP-MPTYP = 'H'.
  TL_TEMP-MDTXT = 'Óleo'.
  APPEND TL_TEMP.

  CLEAR TL_TEMP.
  TL_TEMP-MPTYP = 'F'.
  TL_TEMP-MDTXT = 'Filtro'.
  APPEND TL_TEMP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'MPTYP'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'MPTYP'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_TEMP
      RETURN_TAB      = TL_RETURN
      DYNPFLD_MAPPING = TL_DSELC.

ENDFORM.                    "f_busca_oleo_filtro
