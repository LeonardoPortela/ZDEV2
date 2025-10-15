*--------------------------------------------------------------------------------------------------------*
*&                          AMAGGI
*--------------------------------------------------------------------------------------------------------*
*& REPORT ZPMR0030.                                                                                      *
*& Data           : 03/12/2017                                                                           *
*& Especificado   : Anderson Oenning                                                                     *
*& Desenvolvimento: Anderson Oenning                                                                     *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*--------------------------------------------------------------------------------------------------------*
REPORT ZPMR0030.

*** Macros
***********************************************************
DEFINE MC_PREENCHE_FIELDCAT.

  clear st_fieldcat.
  st_fieldcat-fieldname     = &1.
  st_fieldcat-datatype      = &2.
  st_fieldcat-reptext_ddic  = &3.
  st_fieldcat-outputlen     = &4.
  st_fieldcat-icon          = &5.
  st_fieldcat-key           = &6.
  st_fieldcat-HOTSPOT       = &7.

  append st_fieldcat to it_fieldcat.

END-OF-DEFINITION.

DEFINE MC_PREENCHE_CLASS.

  vg_i = vg_i + 1.
  clear it_sort.
  it_sort-spos      = vg_i.
  it_sort-fieldname = &1.
  it_sort-group     = &2.
  it_sort-up        = &3.
  it_sort-subtot    = &4.
  append it_sort.

END-OF-DEFINITION.

DEFINE MC_PREENCHE_CABEC.

  clear st_header.
  st_header-typ  = &1.
  st_header-key  = &2.
  st_header-info = &3.
  append st_header to it_header.

END-OF-DEFINITION.

*** Declaração de constantes
***********************************************************
CONSTANTS: CC_A        TYPE C VALUE 'A',
           CC_X        TYPE C VALUE 'X',
           CC_I        TYPE C VALUE 'I',
           CC_1        TYPE C VALUE '1',
           CC_2        TYPE C VALUE '2',
           CC_SPRAS(2) TYPE C VALUE 'PT',
           CC_M        TYPE C VALUE 'M'.
* Definições para ALV
TYPE-POOLS: KKBLO.  "Tipos globais para ALV

*----------------------------------------------------------*
*           Definição de variáveis globais                 *
*----------------------------------------------------------*

DATA:   V_TABIX     TYPE SY-TABIX        ,  " guardar o índice
        L_FLSTR     LIKE RIHIMRG-PYEAC,
*        l_flstr_aux LIKE rihimrg-pyeac   ,
        V_PERMISSAO TYPE C.

DATA: VG_I              TYPE I,
      ST_FIELDCAT       TYPE KKBLO_FIELDCAT,
      IT_FIELDCAT       TYPE KKBLO_T_FIELDCAT,
      ST_COLINFO        TYPE KKBLO_SPECIALCOL,
      ST_LAYOUT         TYPE KKBLO_LAYOUT,
      ST_PRINT          TYPE SLIS_PRINT_ALV,
      ST_HEADER         TYPE KKBLO_LISTHEADER,
      IT_FIELDCAT_ALV   TYPE SLIS_T_FIELDCAT_ALV,
      IT_SPECIAL_GROUPS TYPE SLIS_T_SP_GROUP_ALV,
      IT_LAYOUT_ALV     TYPE SLIS_LAYOUT_ALV,
      IT_HEADER         TYPE KKBLO_T_LISTHEADER,
      IT_SORT           TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      ST_GRID_SETTINGS  TYPE LVC_S_GLAY.

INCLUDE <ICON>.

*----------------------------------------------------*
*                ALV GRID                            *
*----------------------------------------------------*
DATA: O_ALV       TYPE REF TO CL_GUI_ALV_GRID,
      O_CONT      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DEF_VARIANT TYPE DISVARIANT,
      VARIANT     TYPE DISVARIANT,
      V_SAVE(1)   TYPE C VALUE 'A'.

*----------------------------------------------------*
*               Tables                               *
*----------------------------------------------------*

TABLES: MARA, EQUI, EQUZ.

*----------------------------------------------------*
*               Types                                *
*----------------------------------------------------*
TYPES:

  BEGIN OF TY_EQUI ,         " Equipamento
    OBJNR TYPE EQUI-OBJNR, " OBJETO
    EQUNR TYPE EQUI-EQUNR, " Código do equipamento
    HERST TYPE EQUI-HERST, " Marca
    TYPBZ TYPE EQUI-TYPBZ, " Modelo
    BAUJJ TYPE EQUI-BAUJJ, " Ano de construção
    EQART TYPE EQUI-EQART, " Tipo do objeto técnico
    EQTYP TYPE EQUI-EQTYP, " Categoria de equipamento
    IWERK TYPE EQUZ-IWERK, " CENTRO
  END OF TY_EQUI,

  BEGIN OF TY_EQUZ ,         " log de modificações
    EQUNR TYPE EQUZ-EQUNR, " Equipamento
    HEQUI TYPE EQUZ-HEQUI, " Equipamento SUPERIOR
    DATBI TYPE EQUZ-DATBI, " Data de validade final
    DATAB TYPE EQUZ-DATAB, " VALIDO DESDE
    IWERK TYPE EQUZ-IWERK, " Centro
    GEWRK TYPE EQUZ-GEWRK, " Centro Trabalho
    TIDNR TYPE EQUZ-TIDNR, " Identificação técnica
    ILOAN TYPE EQUZ-ILOAN, " Local/Classe
    MAPAR TYPE EQUZ-MAPAR, " Vida útil
    HEQNR TYPE EQUZ-HEQNR, " Posição
  END OF TY_EQUZ,

  " Para pegar a descrição
  BEGIN OF TY_EQKT ,
    EQUNR TYPE EQKT-EQUNR, " Equipamento
    EQKTX TYPE EQKT-EQKTX, " Descrição
  END OF TY_EQKT  ,

  BEGIN OF TY_EQUZF ,         " identificação técnica da frota
    HEQUI TYPE EQUZ-HEQUI, " Equipamento
    TIDNR TYPE EQUZ-TIDNR, " Identificação técnica
  END OF TY_EQUZF,

  BEGIN OF TY_IMRG              , " Ordem
    POINT  TYPE IMRG-POINT   , " Ponto medição
    IDATE  TYPE IMRG-IDATE   , " Data da medição
    DOCAF  TYPE IMRG-DOCAF   , " Indicador: doc.med.foi incluído após medidas correspondentes
    READG  TYPE IMRG-READG   , " Valor medido/posição total do contador em unidade SI
    READGI TYPE IMRG-READGI  , " Indicador: o campo de números correspondente contém um valor
    RECDV  TYPE IMRG-RECDV   , " Valor medido na unidade de entrada
    RECDVI TYPE IMRG-RECDVI  , " Indicador: o campo de números correspondente contém um valor
    RECDU  TYPE IMRG-RECDU   , " Unidade de medida ao entrar documento
    CNTRR  TYPE IMRG-CNTRR   , " Posição do contador em unidade SI
    CNTRRI TYPE IMRG-CNTRRI  , " Indicador: o campo de números correspondente contém um valor
    CDIFF  TYPE IMRG-CDIFF   , " Diferença de posições de numerador em unidade SI
    CDIFFI TYPE IMRG-CDIFFI  , " Indicador: o campo de números correspondente contém um valor
    CANCL  TYPE IMRG-CANCL   , " Cancelado
    IDIFF  TYPE IMRG-IDIFF   , " IDENTIFICAÇAO DE TRANSFERENCIA
    MPTYP  TYPE IMPTT-MPTYP  , " Categoria do ponto de medição
    MPOBJ  TYPE IMPTT-MPOBJ  , " Nº objeto do objeto do ponto de medição
    EXPON  LIKE IMPTT-EXPON,       "Exp.10ª potência repres.vírg.flutuante
    DECIM  LIKE IMPTT-DECIM,       "Nº de casas decimais na representação de números
  END OF TY_IMRG,

  BEGIN OF TY_T001W ,         " Centro
    WERKS TYPE T001W-WERKS, " código
    NAME1 TYPE T001W-NAME1, " descrição
  END OF TY_T001W,

  BEGIN OF TY_RELATORIO,      " Dados do relatório
    IWERK(30)   TYPE C           ,       "EQUZ-IWERK, " CENTRO
    A_EQUNR     TYPE EQUI-EQUNR, " Código do equipamento agregado
    A_TIDNR     TYPE EQUZ-TIDNR, " Identificação técnica
    A_DESC(50)  TYPE C     , " Descrição do equipamento
    A_EQTYP(20) TYPE C     , " Tipo de equipamento
    F_EQUNR     TYPE EQUI-EQUNR, " Código do equipamento frota
    F_TIDNR     TYPE EQUZ-TIDNR, " Identificação técnica
    F_DESC(50)  TYPE C      , " Descrição do equipamento
    DATAB       TYPE EQUZ-DATAB, " Data de criação da movimentação
    DATBI       TYPE EQUZ-DATBI, " Data de validade
    HORINI      TYPE MSEG-MENGE , "P LENGTH 8 DECIMALS 3  , "IMRG-READG, " Odômetro e Horímetro
    DATINI      TYPE SY-DATUM,
    HORFIM      TYPE MSEG-MENGE , " P LENGTH 8 DECIMALS 3  , "IMRG-READG, " Odômetro e Horímetro
    DATFIM      TYPE SY-DATUM,
    HORROD      TYPE MSEG-MENGE , " P LENGTH 8 DECIMALS 3  , "IMRG-READG, " Odômetro e Horímetro
    DIAS        TYPE MSEG-MENGE , " Quantidade de dias
    MEDIA       TYPE MSEG-MENGE , " Média
    MAPAR       TYPE EQUZ-MAPAR , " Vida útil
    HEQNR       TYPE EQUZ-HEQNR , " Posição
    ICONE       TYPE ICON-ID,      "Icone.
  END OF TY_RELATORIO .

TYPES: BEGIN OF TY_PARAM,
         PARAM TYPE ZTPARAM-PARAM,
         ZVAL  TYPE ZTPARAM-ZVAL,
       END OF TY_PARAM.

TYPES: BEGIN OF TY_ZVAL,
         PARAM TYPE ZTPARAM-PARAM,
         ZVAL  TYPE EQUI-EQART,
       END OF TY_ZVAL.


" mseg-aufnr, mkpf-mblnr
*----------------------------------------------------*
*                Tabelas Internas                    *
*----------------------------------------------------*
DATA: T_EQUI      TYPE TABLE OF TY_EQUI      WITH HEADER LINE,
      T_EQUZ      TYPE TABLE OF TY_EQUZ      WITH HEADER LINE,
      T_EQUZF     TYPE TABLE OF TY_EQUZF     WITH HEADER LINE,
      T_EQKT      TYPE TABLE OF TY_EQKT      WITH HEADER LINE,
      T_IMRG      TYPE TABLE OF TY_IMRG      WITH HEADER LINE,
      T_T001W     TYPE TABLE OF TY_T001W     WITH HEADER LINE,
*      t_ITOB       TYPE TABLE OF ty_ITOB      WITH HEADER LINE,
      T_RELATORIO TYPE TABLE OF TY_RELATORIO,
      T_PARAM     TYPE TABLE OF TY_PARAM WITH HEADER LINE,
      T_ZVAL      TYPE TABLE OF TY_ZVAL WITH HEADER LINE,
      T_RANG      TYPE RANGE OF EQUI-EQART WITH HEADER LINE,
      W_RELATORIO TYPE TY_RELATORIO.

DATA: TI_LINECOLOR  TYPE SLIS_SPECIALCOL_ALV   OCCURS 0 WITH HEADER LINE,
      TI_LISTHEADER TYPE SLIS_T_LISTHEADER,
      TI_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV            WITH HEADER LINE,
      TI_SORT       TYPE SLIS_SORTINFO_ALV     OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------*
*               Variaveis Globais                    *
*----------------------------------------------------*
DATA: V_LISTHEADER TYPE SLIS_LISTHEADER,    "Cabeçalho
      V_LAYOUT     TYPE SLIS_LAYOUT_ALV,    "layout para saída
      V_PRINT      TYPE SLIS_PRINT_ALV,     "Ctrl de impressão
      V_VARIANTE   LIKE DISVARIANT.         "Variante de exibiçã

DATA: W_EVENTS     TYPE SLIS_T_EVENT WITH HEADER LINE, " Work-area eventos
      W_ES_VARIANT LIKE DISVARIANT,      " variants.
      W_LAYOUT     TYPE SLIS_LAYOUT_ALV,      " Para layout tipo zebra
      I_EVENTS     TYPE SLIS_T_EVENT,         " Para os eventos
      V_REPID      LIKE SY-REPID,
      V_FLAG,
      V_LNUMT(25)  TYPE C.

CONSTANTS:
  T_IMPLEM(9) VALUE 'TP_IMPLEM',
  T_OBJ(9)    VALUE 'TP_OBJ'.


*----------------------------------------------------*
*                Parâmetros de Seleção               *
*----------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK BLOCK1
                        WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:   S_IWERK FOR T_EQUI-IWERK ,
                  S_IDATE FOR T_IMRG-IDATE OBLIGATORY,
                  S_EQUNR FOR T_EQUI-EQUNR ,
                  S_HEQUI FOR T_EQUZ-HEQUI.

*SELECTION-SCREEN ULINE.

*SELECTION-SCREEN ULINE.

SELECTION-SCREEN: END OF BLOCK BLOCK1.

*SELECTION-SCREEN: BEGIN OF BLOCK BLOCK2.

*" tipo de relatório
SELECTION-SCREEN BEGIN OF BLOCK B03 WITH FRAME TITLE TEXT-008.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF LINE .
SELECTION-SCREEN COMMENT 1(10) TEXT-004.
SELECTION-SCREEN POSITION 11.
PARAMETERS: P_RELAGR RADIOBUTTON GROUP REL .

SELECTION-SCREEN POSITION 17.
SELECTION-SCREEN COMMENT 17(05) TEXT-002.
PARAMETERS: P_RELPNE RADIOBUTTON GROUP REL.

SELECTION-SCREEN POSITION 48.
SELECTION-SCREEN COMMENT 48(05) TEXT-003.
PARAMETERS: P_TODOS RADIOBUTTON GROUP REL DEFAULT 'X'.


SELECTION-SCREEN POSITION 29.
SELECTION-SCREEN COMMENT 29(11) TEXT-007.
PARAMETERS: P_IMPLE RADIOBUTTON GROUP REL.

SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK b03.



SELECTION-SCREEN ULINE.
*selection-screen begin of block b2 with frame title text-002.
*PARAMETERS:     P_VARI  TYPE DISVARIANT-VARIANT.
*selection-screen end of block b2.

SELECTION-SCREEN: END OF BLOCK B03.

*---------------------------------------------------*
*               Evento de inicialização             *
*---------------------------------------------------*
INITIALIZATION.

  VARIANT-REPORT = 'ZPMR0030'.
  DEF_VARIANT-REPORT = 'ZPMR0030'.

*    variant-report = 'ZUIRPM011'.
*  def_variant-report = 'ZUIRPM011'.

* Verificar se existe uma variante default
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = 'A'
    CHANGING
      CS_VARIANT = DEF_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 0.
*    P_VARI = DEF_VARIANT-VARIANT.
  ENDIF.

*---------------------------------------------------*
*          Evento de seleção de variant             *
*---------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
*  PERFORM F_F4_VARIANT.

*---------------------------------------------------*
*Evento para verificar se existe a variant informada*
*---------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM F_VARIANT.

*----------------------------------------------------*
*        Evento: Start-of-Selection                  *
*----------------------------------------------------*
START-OF-SELECTION.

  PERFORM SELECIONA_DADOS.

  IF T_IMRG[] IS NOT INITIAL.
    PERFORM PREPARA_RELATORIO.
  ELSE.
*    IF V_PERMISSAO = 'S'.
*      MESSAGE 'Sem permissão para os critérios selecionados. ' TYPE 'I'.
*    ELSE.

    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
*    MESSAGE 'Não encontrados registros. ' TYPE 'I'.
*    ENDIF.
  ENDIF.

  IF T_RELATORIO[] IS NOT INITIAL.
    PERFORM F_CABECALHO.
    IF P_RELPNE IS NOT INITIAL.
      PERFORM F_CATALOGO_P.
    ELSEIF
      P_TODOS IS NOT INITIAL.
      PERFORM F_CATALOGO_P.
    ELSE.
      PERFORM F_CATALOGO.
    ENDIF.
    PERFORM F_CLASSIFICACAO.
    PERFORM F_LAYOUT.
    PERFORM EVENTOS.
    PERFORM F_RELATORIO TABLES T_RELATORIO.

  ELSE.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  seleciona_dados
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS.

  REFRESH:  T_EQUI      ,
            T_EQUZ      ,
            T_T001W     ,
            T_IMRG      ,
            T_RELATORIO .
  CLEAR:    V_PERMISSAO .

*  BREAK 12676136.

  IF S_HEQUI IS NOT INITIAL AND S_EQUNR IS NOT INITIAL.
    MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF S_HEQUI IS NOT INITIAL.

*    " Dados do equipamento
*    SELECT EQUI~OBJNR EQUI~EQUNR EQUI~HERST EQUI~TYPBZ EQUI~BAUJJ EQUI~EQART EQUI~EQTYP EQUZ~IWERK
*    INTO TABLE T_EQUI
*    FROM    EQUI
*    INNER JOIN EQUZ ON EQUZ~EQUNR = EQUI~EQUNR
*    WHERE   EQUI~EQUNR   IN S_EQUNR
*    AND     HEQUI        IN S_HEQUI
*    AND     EQUZ~IWERK   IN S_IWERK .
*    SORT T_EQUI[] ASCENDING BY EQUNR.

    "Dados do equipamento
    SELECT OBJNR EQUNR HERST TYPBZ BAUJJ EQART EQTYP "AOENNING
     FROM EQUI
     INTO TABLE T_EQUI
     WHERE   EQTYP IN ('U', 'T', 'A', 'V').
*     WHERE EQUNR EQ T_EQUI-EQUNR.
    SORT T_EQUI[] ASCENDING BY EQUNR.

    " Deleta equipamento que não forem agregado, pneu ou frota.
    IF P_RELAGR EQ 'X'. " Agregados
      DELETE T_EQUI WHERE  EQTYP NE 'U' .
    ELSE.
      IF P_RELPNE EQ 'X'. " Pneu
        DELETE T_EQUI WHERE  EQTYP NE 'T' .
      ELSEIF P_IMPLE EQ 'X'. " Frota
        DELETE T_EQUI WHERE  EQTYP NE 'A' AND EQTYP NE 'V'.
      ELSEIF P_TODOS EQ 'X'. " Todos (agregado e pneu)
        DELETE T_EQUI WHERE  EQTYP NE 'U' AND
                             EQTYP NE 'T' AND
                             EQTYP NE 'A' AND
                             EQTYP NE 'V' .
      ENDIF.
    ENDIF.


*   Verificar direitos de acesso ao centro
*  PERFORM f_permissao_centro.

    CHECK T_EQUI[] IS NOT INITIAL.

    " Log de modificações
    SELECT EQUNR HEQUI DATBI DATAB IWERK GEWRK TIDNR ILOAN MAPAR HEQNR
    INTO TABLE T_EQUZ
    FROM EQUZ
    FOR ALL ENTRIES IN T_EQUI
    WHERE EQUNR EQ T_EQUI-EQUNR
    AND   HEQUI IN S_HEQUI
    AND   IWERK IN S_IWERK "AOENNING
    AND   DATAB IN S_IDATE .

    " Busca Centro
    SELECT DISTINCT WERKS NAME1
    INTO TABLE T_T001W
    FROM T001W
    FOR ALL ENTRIES IN T_EQUZ
    WHERE WERKS = T_EQUZ-IWERK. "AOENNING

*BREAK-POINT.

    DELETE T_EQUZ WHERE EQUNR IS INITIAL.

    " identificação técnica da frota
    IF T_EQUZ[] IS NOT INITIAL.
      SELECT DISTINCT EQUNR TIDNR
      INTO TABLE T_EQUZF
      FROM EQUZ
      FOR ALL ENTRIES IN T_EQUZ
      WHERE EQUNR EQ T_EQUZ-HEQUI.
    ENDIF.


    " Busca equipamento frota
    IF T_EQUZF[] IS NOT INITIAL.
      SELECT OBJNR EQUNR HERST TYPBZ BAUJJ EQART EQTYP
      APPENDING CORRESPONDING FIELDS OF TABLE T_EQUI
      FROM    EQUI
      FOR ALL ENTRIES IN T_EQUZF
      WHERE   EQUNR   EQ  T_EQUZF-HEQUI.
    ENDIF.

    " Busca descrição do agregado
    SELECT DISTINCT EQUNR EQKTX
    INTO TABLE T_EQKT
    FROM EQKT
    FOR ALL ENTRIES IN T_EQUI
    WHERE EQUNR   EQ T_EQUI-EQUNR
    AND   SPRAS   EQ CC_SPRAS.

*  " Tabela de movimentação do consumo de combustível
    SELECT  IMRG~POINT IMRG~IDATE IMRG~DOCAF IMRG~READG IMRG~READGI IMRG~RECDV IMRG~RECDVI IMRG~RECDU IMRG~CNTRR IMRG~CNTRRI IMRG~CDIFF IMRG~CDIFFI IMRG~CANCL IMRG~IDIFF IMPTT~MPTYP IMPTT~MPOBJ IMPTT~EXPON IMPTT~DECIM
    INTO TABLE T_IMRG
    FROM    IMPTT
    INNER JOIN IMRG ON IMRG~POINT =  IMPTT~POINT
    FOR ALL ENTRIES IN T_EQUI
    WHERE IMPTT~MPOBJ   EQ T_EQUI-OBJNR.

    DELETE T_IMRG WHERE IDIFF EQ 'X'.  " Delete vida util para ficar somente horimetro.
    DELETE T_IMRG WHERE CANCL EQ 'X'.  " Delete os estornados



  ELSE."AOENNING
    " Seleciona implementos cadastrado na tabela ZTPARAM (ZPM0002).
    IF P_IMPLE IS NOT INITIAL.

      FREE: T_ZVAL[].
      SELECT PARAM ZVAL
        FROM ZTPARAM
        INTO CORRESPONDING FIELDS OF TABLE T_PARAM
        WHERE PARAM EQ T_IMPLEM.

      IF NOT T_PARAM[] IS INITIAL.
        LOOP AT T_PARAM.
          T_ZVAL-PARAM = T_PARAM-PARAM.
          T_ZVAL-ZVAL = T_PARAM-ZVAL.
          APPEND T_ZVAL.
        ENDLOOP.
      ENDIF.

      SELECT OBJNR EQUNR HERST TYPBZ BAUJJ EQART EQTYP "AOENNING
      FROM EQUI
      INTO TABLE T_EQUI
      FOR ALL ENTRIES IN T_ZVAL
      WHERE EQART EQ T_ZVAL-ZVAL
      AND   EQUNR IN S_EQUNR.
      SORT T_EQUI[] ASCENDING BY EQUNR.

      "Seleciona todos (Pneus, Agregados e Implementos).
    ELSEIF P_TODOS IS NOT INITIAL.
      SELECT PARAM ZVAL
      FROM ZTPARAM
      INTO CORRESPONDING FIELDS OF TABLE T_PARAM
      WHERE PARAM IN ( T_IMPLEM ).

      IF NOT T_PARAM[] IS INITIAL.
        LOOP AT T_PARAM.
          T_RANG-SIGN = 'I'.
          T_RANG-OPTION = 'EQ'.
          T_RANG-LOW = T_PARAM-ZVAL.
          APPEND T_RANG.
        ENDLOOP.
      ENDIF.

      SORT T_RANG[] ASCENDING BY LOW.

      SELECT OBJNR EQUNR HERST TYPBZ BAUJJ EQART EQTYP "AOENNING
      FROM EQUI
      INTO TABLE T_EQUI
      WHERE EQUNR IN S_EQUNR.
*      AND   EQTYP IN ('U', 'T', 'A', 'V').
      SORT T_EQUI[] ASCENDING BY EQUNR.

      "Deletar todos da tabela EQUI, diferente de Implemento cadastrado na tabela ZTPARAM (ZPM0002).
      DELETE T_EQUI WHERE EQTYP EQ 'V' AND EQART NOT IN T_RANG.
      DELETE T_EQUI WHERE EQTYP EQ 'A' AND EQART NOT IN T_RANG.

*      BREAK-POINT.
    ELSE.
      "Seleciona todos equipamentos.
      SELECT OBJNR EQUNR HERST TYPBZ BAUJJ EQART EQTYP "AOENNING
      FROM EQUI
      INTO TABLE T_EQUI
      WHERE EQUNR IN S_EQUNR
      AND   EQTYP IN ('U', 'T', 'A', 'V').
      SORT T_EQUI[] ASCENDING BY EQUNR.
    ENDIF.



    CHECK T_EQUI[] IS NOT INITIAL.

    " Deleta equipamento que não forem agregado, pneu ou frota.
    IF P_RELAGR EQ 'X'. " Agregados
      DELETE T_EQUI WHERE  EQTYP NE 'U' .
    ELSE.
      IF P_RELPNE EQ 'X'. " Pneu
        DELETE T_EQUI WHERE  EQTYP NE 'T' .
      ELSEIF P_IMPLE EQ 'X'. " Frota
        DELETE T_EQUI WHERE  EQTYP NE 'A' AND EQTYP NE 'V'.
      ELSEIF P_TODOS EQ 'X'. " Todos (agregado e pneu)
        DELETE T_EQUI WHERE  EQTYP NE 'U' AND
                             EQTYP NE 'T' AND
                             EQTYP NE 'A' AND
                             EQTYP NE 'V' .
      ENDIF.
    ENDIF.


*   Verificar direitos de acesso ao centro
*  PERFORM f_permissao_centro.

    CHECK T_EQUI[] IS NOT INITIAL.

    " Log de modificações
    SELECT EQUNR HEQUI DATBI DATAB IWERK GEWRK TIDNR ILOAN MAPAR HEQNR
    INTO TABLE T_EQUZ
    FROM EQUZ
    FOR ALL ENTRIES IN T_EQUI
    WHERE EQUNR EQ T_EQUI-EQUNR
    AND   IWERK IN S_IWERK "AOENNING
    AND   DATAB IN S_IDATE .

    " Busca Centro
    SELECT DISTINCT WERKS NAME1
    INTO TABLE T_T001W
    FROM T001W
    FOR ALL ENTRIES IN T_EQUZ
    WHERE WERKS = T_EQUZ-IWERK. "AOENNING

*BREAK-POINT.
    " Deleta agregados que não tiver em nenhum equipamento.
    DELETE T_EQUZ WHERE HEQUI IS INITIAL.

    " identificação técnica da frota
    IF T_EQUZ[] IS NOT INITIAL.
      SELECT DISTINCT EQUNR TIDNR
      INTO TABLE T_EQUZF
      FROM EQUZ
      FOR ALL ENTRIES IN T_EQUZ
      WHERE EQUNR EQ T_EQUZ-HEQUI.

    ENDIF.


    " Busca equipamento frota
    IF T_EQUZF[] IS NOT INITIAL.
      SELECT OBJNR EQUNR HERST TYPBZ BAUJJ EQART EQTYP
      APPENDING CORRESPONDING FIELDS OF TABLE T_EQUI
      FROM    EQUI
      FOR ALL ENTRIES IN T_EQUZF
      WHERE   EQUNR   EQ  T_EQUZF-HEQUI.


    ENDIF.

    " Busca descrição do agregado
    SELECT DISTINCT EQUNR EQKTX
    INTO TABLE T_EQKT
    FROM EQKT
    FOR ALL ENTRIES IN T_EQUI
    WHERE EQUNR   EQ T_EQUI-EQUNR
    AND   SPRAS   EQ CC_SPRAS.




*  " Tabela de movimentação do consumo de combustível
    SELECT  IMRG~POINT IMRG~IDATE IMRG~DOCAF IMRG~READG IMRG~READGI IMRG~RECDV IMRG~RECDVI IMRG~RECDU IMRG~CNTRR IMRG~CNTRRI IMRG~CDIFF IMRG~CDIFFI IMRG~CANCL IMRG~IDIFF IMPTT~MPTYP IMPTT~MPOBJ IMPTT~EXPON IMPTT~DECIM
    INTO TABLE T_IMRG
    FROM    IMPTT
    INNER JOIN IMRG ON IMRG~POINT =  IMPTT~POINT
    FOR ALL ENTRIES IN T_EQUI
    WHERE IMPTT~MPOBJ   EQ T_EQUI-OBJNR.



    DELETE T_IMRG WHERE IDIFF EQ 'X'.  " Delete vida util para ficar somente horimetro.
    DELETE T_IMRG WHERE CANCL EQ 'X'.  " Delete os estornados

  ENDIF.


ENDFORM.                    " seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  prepara_relatorio
*&---------------------------------------------------------------------*
FORM PREPARA_RELATORIO.

  DATA V_OBJNR TYPE EQUI-OBJNR.

  SORT T_IMRG BY MPOBJ IDATE.


  LOOP AT T_EQUZ.
    CLEAR W_RELATORIO.


    W_RELATORIO-DATAB = T_EQUZ-DATAB. " Data de validade desde
    W_RELATORIO-DATBI = T_EQUZ-DATBI. " Data de validade até
    W_RELATORIO-MAPAR = T_EQUZ-MAPAR. " vida útil
    W_RELATORIO-HEQNR = T_EQUZ-HEQNR. " Posição

    "   Quando agregado
*    esta no momento no equipamento joga ano 9999
    IF T_EQUZ-DATBI > SY-DATUM.
      T_EQUZ-DATBI = SY-DATUM.
    ENDIF.



    "   Equipamento - Agregado
    CLEAR T_EQUI.
    READ TABLE T_EQUI WITH KEY EQUNR = T_EQUZ-EQUNR.
    W_RELATORIO-A_EQUNR   = T_EQUI-EQUNR. " Codigo do equipamento agregado
    W_RELATORIO-A_TIDNR   = T_EQUZ-TIDNR. " Identif. Tecnica agregado
*    CONCATENATE t_equi-HERST '-' t_equi-TYPBZ '-' t_equi-BAUJJ INTO w_relatorio-A_desc  SEPARATED BY space.

    " Busca descrição do equipamento
    CLEAR T_EQKT.
    READ TABLE T_EQKT WITH KEY EQUNR = T_EQUZ-EQUNR.
    W_RELATORIO-A_DESC = T_EQKT-EQKTX.

    IF T_EQUI-EQTYP EQ 'U'.
      W_RELATORIO-A_EQTYP = 'U-Agregado'.
      W_RELATORIO-ICONE = ICON_OPERATION.
    ENDIF.

    IF T_EQUI-EQTYP EQ 'T'.
      W_RELATORIO-A_EQTYP = 'T-Pneu'.
      W_RELATORIO-ICONE = ICON_SYSTEMS.
    ENDIF.

    IF T_EQUI-EQTYP EQ 'V'.
      W_RELATORIO-A_EQTYP = 'V-Frota'.
      W_RELATORIO-ICONE = ICON_TRANSPORTATION_MODE.
    ENDIF.

    IF T_EQUI-EQTYP EQ 'A'.
      W_RELATORIO-A_EQTYP = 'A-Frota'.
      W_RELATORIO-ICONE = ICON_TRANSPORTATION_MODE.
    ENDIF.



    "   Busca o centro do agregado
    CLEAR T_T001W.
    READ TABLE T_T001W WITH KEY WERKS = T_EQUZ-IWERK. "AOENNING
    CONCATENATE T_T001W-WERKS '-' T_T001W-NAME1 INTO W_RELATORIO-IWERK.

    "   Equipamento - Frota
    CLEAR T_EQUI.
    READ TABLE T_EQUI WITH KEY EQUNR = T_EQUZ-HEQUI.
    W_RELATORIO-F_EQUNR   = T_EQUI-EQUNR. " Codigo do equipamento frota
*    CONCATENATE t_equi-HERST '-' t_equi-TYPBZ '-' t_equi-BAUJJ INTO w_relatorio-F_desc  SEPARATED BY space.

    " Busca descrição do equipamento
    CLEAR T_EQKT.
    READ TABLE T_EQKT WITH KEY EQUNR = T_EQUZ-HEQUI.
    W_RELATORIO-F_DESC = T_EQKT-EQKTX.

    CLEAR T_EQUZF.
    READ TABLE T_EQUZF WITH KEY HEQUI = T_EQUZ-HEQUI.
    W_RELATORIO-F_TIDNR   = T_EQUZF-TIDNR. " Identif. Tecnica frota

    " Busca horímetro inicio
    LOOP AT T_IMRG WHERE  MPOBJ   =  T_EQUI-OBJNR
                   AND    IDATE   <= T_EQUZ-DATAB.

      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          CHAR_UNIT       = T_IMRG-RECDU
          DECIMALS        = T_IMRG-DECIM
          EXPONENT        = T_IMRG-EXPON
          FLTP_VALUE_SI   = T_IMRG-CNTRR
          INDICATOR_VALUE = CC_X
        IMPORTING
          CHAR_VALUE      = L_FLSTR
        EXCEPTIONS
          NO_UNIT_GIVEN   = 01.

      TRANSLATE L_FLSTR USING ',.'.

      MOVE    L_FLSTR TO W_RELATORIO-HORINI.
      MOVE    T_IMRG-IDATE TO W_RELATORIO-DATINI.
    ENDLOOP.
    " Caso não achar nenhum abastecimento antes da criação do agregado pega a primeira posterior
    IF W_RELATORIO-HORINI IS INITIAL.
      LOOP AT T_IMRG WHERE  MPOBJ   =  T_EQUI-OBJNR
                     AND    IDATE   >= T_EQUZ-DATAB.

        CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
          EXPORTING
            CHAR_UNIT       = T_IMRG-RECDU
            DECIMALS        = T_IMRG-DECIM
            EXPONENT        = T_IMRG-EXPON
            FLTP_VALUE_SI   = T_IMRG-CNTRR
            INDICATOR_VALUE = CC_X
          IMPORTING
            CHAR_VALUE      = L_FLSTR
          EXCEPTIONS
            NO_UNIT_GIVEN   = 01.

        TRANSLATE L_FLSTR USING ',.'.

        MOVE    L_FLSTR TO W_RELATORIO-HORINI.
        MOVE    T_IMRG-IDATE TO W_RELATORIO-DATINI.
        EXIT.
      ENDLOOP.
    ENDIF.
    " Busca horímetro Final
    CLEAR L_FLSTR.
    LOOP AT T_IMRG WHERE  MPOBJ   =  T_EQUI-OBJNR
                   AND    IDATE   >= T_EQUZ-DATBI.

      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          CHAR_UNIT       = T_IMRG-RECDU
          DECIMALS        = T_IMRG-DECIM
          EXPONENT        = T_IMRG-EXPON
          FLTP_VALUE_SI   = T_IMRG-CNTRR
          INDICATOR_VALUE = CC_X
        IMPORTING
          CHAR_VALUE      = L_FLSTR
        EXCEPTIONS
          NO_UNIT_GIVEN   = 01.

      TRANSLATE L_FLSTR USING ',.'.

      MOVE    L_FLSTR TO W_RELATORIO-HORFIM.
      MOVE    T_IMRG-IDATE TO W_RELATORIO-DATFIM.
      EXIT.
    ENDLOOP.
    " caso não achar nenhum abastecimento depois da movimentação do agregado (t_EQUZ-DATBI) pega o ultimo dento do período
    IF W_RELATORIO-HORFIM IS INITIAL.
      LOOP AT T_IMRG WHERE  MPOBJ   =  T_EQUI-OBJNR
                     AND    IDATE   <= T_EQUZ-DATBI.

        CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
          EXPORTING
            CHAR_UNIT       = T_IMRG-RECDU
            DECIMALS        = T_IMRG-DECIM
            EXPONENT        = T_IMRG-EXPON
            FLTP_VALUE_SI   = T_IMRG-CNTRR
            INDICATOR_VALUE = CC_X
          IMPORTING
            CHAR_VALUE      = L_FLSTR
          EXCEPTIONS
            NO_UNIT_GIVEN   = 01.

        TRANSLATE L_FLSTR USING ',.'.

        MOVE    L_FLSTR TO W_RELATORIO-HORFIM.
        MOVE    T_IMRG-IDATE TO W_RELATORIO-DATFIM.
      ENDLOOP.
    ENDIF.

    W_RELATORIO-HORROD    = W_RELATORIO-HORFIM - W_RELATORIO-HORINI.
    W_RELATORIO-DIAS      = T_EQUZ-DATBI - T_EQUZ-DATAB.
    IF W_RELATORIO-DIAS IS NOT INITIAL.
      W_RELATORIO-MEDIA     = W_RELATORIO-HORROD / W_RELATORIO-DIAS.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = W_RELATORIO-A_EQUNR
      IMPORTING
        OUTPUT = W_RELATORIO-A_EQUNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = W_RELATORIO-F_EQUNR
      IMPORTING
        OUTPUT = W_RELATORIO-F_EQUNR.


    APPEND W_RELATORIO TO T_RELATORIO.
    CLEAR W_RELATORIO.
  ENDLOOP.

  SORT T_RELATORIO BY IWERK A_EQUNR F_EQUNR DATAB DATBI.

ENDFORM.                    " prepara_relatorio

************************************************************************
***                        Definições ALV                            ***
************************************************************************
*&--------------------------------------------------------------------*
*&      Form  top_of_page
*&--------------------------------------------------------------------*
*       Imprime o cabeçalho
*---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING UCOMM LIKE SY-UCOMM
      SELFIELD TYPE SLIS_SELFIELD.
*  BREAK-POINT.
  CASE UCOMM.
*    WHEN 'PRINT'.

    WHEN '&IC1'.
      IF SELFIELD-FIELDNAME = 'A_EQUNR'.
        SET PARAMETER ID 'EQN' FIELD SELFIELD-VALUE.
        CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .
      ENDIF.

      IF SELFIELD-FIELDNAME = 'F_EQUNR'.
        SET PARAMETER ID 'EQN' FIELD SELFIELD-VALUE.
        CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .
      ENDIF.

    WHEN OTHERS.
  ENDCASE.
  SELFIELD-REFRESH = 'X'.
ENDFORM.                    " user_command

*&---------------------------------------------------------------------*
*&      Form  pf_status_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS '900'.

ENDFORM.                    "pf_status_set
*&---------------------------------------------------------------------*
*&      Form  F_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CABECALHO.

  DATA: L_CABEC(100)  TYPE C,
        L_CABEC1(100) TYPE C,
        L_CABEC2(100) TYPE C.

  MC_PREENCHE_CABEC: 'H' ' ' 'Dados de Seleção',  " Título do relatório
                     'S' ' ' ' '.                 " Para não exibir barra de rolagem

  IF S_IWERK IS NOT INITIAL.
* Centro
    IF ( S_IWERK-LOW IS NOT INITIAL ) AND ( S_IWERK-HIGH IS NOT INITIAL ).
      CONCATENATE S_IWERK-LOW 'á' S_IWERK-HIGH INTO L_CABEC SEPARATED BY SPACE.
    ELSEIF ( S_IWERK-LOW IS NOT INITIAL ).
      L_CABEC = S_IWERK-LOW.
    ELSE.
*    L_CABEC = 'Todos'.
    ENDIF.
    MC_PREENCHE_CABEC:  'S' 'Centro                 ' L_CABEC.
  ENDIF.

  IF S_EQUNR IS NOT INITIAL.
* Equipamento
    IF ( S_EQUNR-LOW IS NOT INITIAL ) AND ( S_EQUNR-HIGH IS NOT INITIAL ).
      CONCATENATE S_EQUNR-LOW 'á' S_EQUNR-HIGH INTO L_CABEC SEPARATED BY SPACE.
    ELSEIF ( S_EQUNR-LOW IS NOT INITIAL ).
      L_CABEC = S_EQUNR-LOW.
    ELSE.
*    L_CABEC = 'Todos'.
    ENDIF.
    MC_PREENCHE_CABEC:  'S' 'Equipamento            ' L_CABEC.
  ENDIF.

  IF S_HEQUI IS NOT INITIAL.
* Equipamento Superior.
    IF ( S_HEQUI-LOW IS NOT INITIAL ) AND ( S_HEQUI-HIGH IS NOT INITIAL ).
      CONCATENATE S_HEQUI-LOW 'á' S_EQUNR-HIGH INTO L_CABEC SEPARATED BY SPACE.
    ELSEIF ( S_HEQUI-LOW IS NOT INITIAL ).
      L_CABEC = S_HEQUI-LOW.
    ELSE.
*    L_CABEC = 'Todos'.
    ENDIF.
    MC_PREENCHE_CABEC:  'S' 'Equipamento            ' L_CABEC.
  ENDIF.

  IF S_IDATE IS NOT INITIAL.
* Periodo de seleção
    WRITE: S_IDATE-LOW  TO L_CABEC1,
           S_IDATE-HIGH TO L_CABEC2.
    CONCATENATE L_CABEC1 'á' L_CABEC2 INTO L_CABEC SEPARATED BY SPACE.
    MC_PREENCHE_CABEC:  'S' 'Período                  ' L_CABEC.
  ENDIF.

*  DO 7 TIMES.
*    MC_PREENCHE_CABEC:  'S' '' ''.
*  ENDDO.

ENDFORM.                    " F_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CATALOGO.

  IF S_HEQUI IS NOT INITIAL.

    MC_PREENCHE_FIELDCAT:
  "                                                      Tam  Sum ID
    'IWERK'            'CHAR' 'Centro'                   '25' ' ' ' ' ' ',
     'F_EQUNR'          'CHAR' 'Equipamento (Frota)'      '18' ' ' ' ' 'X',
*  'F_TIDNR'          'CHAR' 'Ident. Téc. (Frota)'     '15' ' ' ' ' ' ',
    'F_DESC'           'CHAR' 'Descrição Frota'          '40' ' ' ' ' ' ',
     'ICONE'            'CHAR' ' '                   '10' 'X' ' ' ' ',
    'A_EQTYP'          'CHAR' 'Tipo'                     '10' ' ' ' ' ' ',
    'A_EQUNR'          'CHAR' 'Equipamento (agreg.)'     '18' ' ' ' ' 'X',
*  'A_TIDNR'          'CHAR' 'Ident. Téc. (agreg.)'    '15' ' ' ' ' ' ',
    'A_DESC'           'CHAR' 'Descrição Agregado'       '37' ' ' ' ' ' ',
    'DATAB'            'DATS' 'Válido Desde'             '13' ' ' ' ' ' ',
    'DATBI'            'DATS' 'Válido Até'               '10' ' ' ' ' ' ',
*  'DATINI'           'DATS' 'Data Abast. inicio'      '13' ' ' ' ' ' ',
    'HORINI'           'CURR' 'Início Hod/Hor'           '15' ' ' ' ' ' ',
*  'DATFIM'           'DATS' 'Data Abast. Final'       '13' ' ' ' ' ' ',
    'HORFIM'           'CURR' 'Final  Hod/Hor'           '15' ' ' ' ' ' ',
    'HORROD'           'CURR' 'Qtdade Horas/Km'          '15' ' ' ' ' ' ',
    'DIAS'             'CURR' 'Dias'                     '08' ' ' ' ' ' ',
    'MEDIA'            'CURR' 'Média'                    '10' ' ' ' ' ' ',
    'MAPAR'            'CHAR' 'Nr. vida'                 '09' ' ' ' ' ' '.
*    'HEQNR'            'CHAR' 'Posição'                 '07' ' ' ' ' ' '.

  ELSE.
    MC_PREENCHE_FIELDCAT:

 "                                                      Tam  Sum ID
   'IWERK'            'CHAR' 'Centro'                   '25' ' ' ' ' ' ',
   'A_EQUNR'          'CHAR' 'Equipamento (agreg.)'     '18' ' ' ' ' 'X',
*  'A_TIDNR'          'CHAR' 'Ident. Téc. (agreg.)'    '15' ' ' ' ' ' ',
   'A_DESC'           'CHAR' 'Descrição Agregado'       '37' ' ' ' ' ' ',
   'ICONE'            'CHAR' ' '                   '10' 'X' ' ' ' ',
   'A_EQTYP'          'CHAR' 'Tipo'                     '10' ' ' ' ' ' ',
   'F_EQUNR'          'CHAR' 'Equipamento (Frota)'      '18' ' ' ' ' 'X',
*  'F_TIDNR'          'CHAR' 'Ident. Téc. (Frota)'     '15' ' ' ' ' ' ',
   'F_DESC'           'CHAR' 'Descrição Frota'          '40' ' ' ' ' ' ',
   'DATAB'            'DATS' 'Válido Desde'             '13' ' ' ' ' ' ',
   'DATBI'            'DATS' 'Válido Até'               '10' ' ' ' ' ' ',
*  'DATINI'           'DATS' 'Data Abast. inicio'      '13' ' ' ' ' ' ',
   'HORINI'           'CURR' 'Início Hod/Hor'           '15' ' ' ' ' ' ',
*  'DATFIM'           'DATS' 'Data Abast. Final'       '13' ' ' ' ' ' ',
   'HORFIM'           'CURR' 'Final  Hod/Hor'           '15' ' ' ' ' ' ',
   'HORROD'           'CURR' 'Qtdade Horas/Km'          '15' ' ' ' ' ' ',
   'DIAS'             'CURR' 'Dias'                     '08' ' ' ' ' ' ',
   'MEDIA'            'CURR' 'Média'                    '10' ' ' ' ' ' ',
   'MAPAR'            'CHAR' 'Nr. vida'                 '09' ' ' ' ' ' '.
*   'HEQNR'            'CHAR' 'Posição'                 '07' ' ' ' ' ' '.

  ENDIF.
ENDFORM.                    " F_CATALOGO

FORM F_CATALOGO_P.

  IF S_HEQUI IS NOT INITIAL.

    MC_PREENCHE_FIELDCAT:
  "                                                      Tam  Sum ID

    'IWERK'            'CHAR' 'Centro'                   '25' ' ' ' ' ' ',
     'F_EQUNR'          'CHAR' 'Equipamento (Frota)'      '18' ' ' ' ' 'X',
*  'F_TIDNR'          'CHAR' 'Ident. Téc. (Frota)'     '15' ' ' ' ' ' ',
    'F_DESC'           'CHAR' 'Descrição Frota'          '40' ' ' ' ' ' ',
     'ICONE'            'CHAR' ' '                   '10' 'X' ' ' ' ',
    'A_EQTYP'          'CHAR' 'Tipo'                     '10' ' ' ' ' ' ',
    'A_EQUNR'          'CHAR' 'Equipamento (agreg.)'     '18' ' ' ' ' 'X',
*  'A_TIDNR'          'CHAR' 'Ident. Téc. (agreg.)'    '15' ' ' ' ' ' ',
    'A_DESC'           'CHAR' 'Descrição Agregado'       '37' ' ' ' ' ' ',
    'DATAB'            'DATS' 'Válido Desde'             '13' ' ' ' ' ' ',
    'DATBI'            'DATS' 'Válido Até'               '10' ' ' ' ' ' ',
*  'DATINI'           'DATS' 'Data Abast. inicio'      '13' ' ' ' ' ' ',
    'HORINI'           'CURR' 'Início Hod/Hor'           '15' ' ' ' ' ' ',
*  'DATFIM'           'DATS' 'Data Abast. Final'       '13' ' ' ' ' ' ',
    'HORFIM'           'CURR' 'Final  Hod/Hor'           '15' ' ' ' ' ' ',
    'HORROD'           'CURR' 'Qtdade Horas/Km'          '15' ' ' ' ' ' ',
    'DIAS'             'CURR' 'Dias'                     '08' ' ' ' ' ' ',
    'MEDIA'            'CURR' 'Média'                    '10' ' ' ' ' ' ',
    'MAPAR'            'CHAR' 'Nr. vida'                 '09' ' ' ' ' ' ',
    'HEQNR'            'CHAR' 'Posição'                 '07' ' ' ' ' ' '.

  ELSE.
    MC_PREENCHE_FIELDCAT:

 "                                                      Tam  Sum ID

   'IWERK'            'CHAR' 'Centro'                   '25' ' ' ' ' ' ',
   'A_EQUNR'          'CHAR' 'Equipamento (agreg.)'     '18' ' ' ' ' 'X',
*  'A_TIDNR'          'CHAR' 'Ident. Téc. (agreg.)'    '15' ' ' ' ' ' ',
   'A_DESC'           'CHAR' 'Descrição Agregado'       '37' ' ' ' ' ' ',
   'ICONE'            'CHAR' ' '                   '10' 'X' ' ' ' ',
   'A_EQTYP'          'CHAR' 'Tipo'                     '10' ' ' ' ' ' ',
   'F_EQUNR'          'CHAR' 'Equipamento (Frota)'      '18' ' ' ' ' 'X',
*  'F_TIDNR'          'CHAR' 'Ident. Téc. (Frota)'     '15' ' ' ' ' ' ',
   'F_DESC'           'CHAR' 'Descrição Frota'          '40' ' ' ' ' ' ',
   'DATAB'            'DATS' 'Válido Desde'             '13' ' ' ' ' ' ',
   'DATBI'            'DATS' 'Válido Até'               '10' ' ' ' ' ' ',
*  'DATINI'           'DATS' 'Data Abast. inicio'      '13' ' ' ' ' ' ',
   'HORINI'           'CURR' 'Início Hod/Hor'           '15' ' ' ' ' ' ',
*  'DATFIM'           'DATS' 'Data Abast. Final'       '13' ' ' ' ' ' ',
   'HORFIM'           'CURR' 'Final  Hod/Hor'           '15' ' ' ' ' ' ',
   'HORROD'           'CURR' 'Qtdade Horas/Km'          '15' ' ' ' ' ' ',
   'DIAS'             'CURR' 'Dias'                     '08' ' ' ' ' ' ',
   'MEDIA'            'CURR' 'Média'                    '10' ' ' ' ' ' ',
   'MAPAR'            'CHAR' 'Nr. vida'                 '09' ' ' ' ' ' ',
   'HEQNR'            'CHAR' 'Posição'                 '07' ' ' ' ' ' '.

  ENDIF.
ENDFORM.                    " F_CATALOGO
*&---------------------------------------------------------------------*
*&      Form  F_CLASSIFICACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CLASSIFICACAO .

  CLEAR VG_I.
  REFRESH IT_SORT.

*  mc_preenche_class:
*                     'IWERK'       'X' 'X' 'X',
*                     'A_EQUNR'     'X' 'X' 'X'.
ENDFORM.                    " F_CLASSIFICACAO
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_LAYOUT .
*BREAK-POINT.
  ST_LAYOUT-NO_ZEBRA            = ' '.
  ST_LAYOUT-GROUP_BUTTONS       = ' '.
  ST_LAYOUT-TOTALS_ONLY         = ' '.

* Otimizar colunas na tela
  ST_LAYOUT-COLWIDTH_OPTIMIZE   = 'X'.

  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA_BACK'
    EXPORTING
      IT_FIELDCAT       = IT_FIELDCAT
      IS_LAYOUT         = ST_LAYOUT
    IMPORTING
      ET_FIELDCAT       = IT_FIELDCAT_ALV
      ES_LAYOUT         = IT_LAYOUT_ALV
      ET_SPECIAL_GROUPS = IT_SPECIAL_GROUPS.

ENDFORM.                    " F_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EVENTOS .
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = I_EVENTS.

  READ TABLE I_EVENTS
       WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
       INTO W_EVENTS.
  IF SY-SUBRC = 0.
    MOVE 'ENCABEZADO' TO W_EVENTS-FORM.
    MODIFY I_EVENTS FROM W_EVENTS INDEX SY-TABIX.
  ENDIF.


ENDFORM.                    " EVENTOS



*&---------------------------------------------------------------------*
*&      Form  F_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_RELATORIO TABLES PT_OUTTAB TYPE TABLE.

*  Não otimizar colunas no preview e nem na impressão
  ST_GRID_SETTINGS-NO_COLWOPT = CC_X.

* Não imprimir o relatório estatístico antes do relatório
  ST_PRINT-NO_PRINT_SELINFOS  = CC_X.
  ST_PRINT-NO_PRINT_LISTINFOS = CC_X.

  V_REPID = SY-REPID.
  CALL FUNCTION 'K_KKB_SAVE_MODE_GET'
    IMPORTING
      E_SAVE = V_SAVE.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPID
      IS_LAYOUT               = IT_LAYOUT_ALV
      IT_FIELDCAT             = IT_FIELDCAT_ALV
      IT_SPECIAL_GROUPS       = IT_SPECIAL_GROUPS
      I_CALLBACK_TOP_OF_PAGE  = 'TOP_OF_PAGE'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*     i_callback_pf_status_set = 'PF_STATUS_SET'
      I_GRID_SETTINGS         = ST_GRID_SETTINGS
      IT_SORT                 = IT_SORT[]
      I_DEFAULT               = CC_X
      IS_PRINT                = ST_PRINT
      I_SAVE                  = V_SAVE
      IS_VARIANT              = VARIANT
    TABLES
      T_OUTTAB                = PT_OUTTAB
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE CC_I NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.

*  FREE: it_relatorio.

ENDFORM.                    "f_relatorio

FORM TOP_OF_PAGE.                                           "#EC CALLED
* Para criar um logotipo, deve-se entrar na transação 0FPM002 e
* preencher:
* - Classe = PICTURES
* - Objeto = OT
* - Item   = Nome do ID da figura

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER
      I_LOGO             = 'LOGO_NOVO'.

ENDFORM.                    "top_of_page
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
      IS_VARIANT = VARIANT
      I_SAVE     = 'A'
    IMPORTING
      E_EXIT     = VARIANT_EXIT
      ES_VARIANT = DEF_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF VARIANT_EXIT = SPACE.
*      P_VARI = DEF_VARIANT-VARIANT.
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
*FORM F_VARIANT .
*
*  IF NOT P_VARI IS INITIAL.
*
*    MOVE VARIANT TO DEF_VARIANT.
*    MOVE P_VARI TO DEF_VARIANT-VARIANT.
*
*    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
*      EXPORTING
*        I_SAVE     = 'A'
*      CHANGING
*        CS_VARIANT = DEF_VARIANT.
*
*    VARIANT = DEF_VARIANT.
*
*  ELSE.
*    CLEAR VARIANT.
*    VARIANT-REPORT = V_REPID.
*  ENDIF.
*
*ENDFORM.                    " F_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_PERMISSAO_CENTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PERMISSAO_CENTRO .

  LOOP AT T_EQUI.
    V_PERMISSAO = 'S'.

    AUTHORITY-CHECK OBJECT 'ZUIRPM011'
      ID 'IWERK'  FIELD T_EQUI-IWERK.

    IF SY-SUBRC NE 0.
      DELETE T_EQUI INDEX SY-TABIX.
    ENDIF.

  ENDLOOP.

ENDFORM.
