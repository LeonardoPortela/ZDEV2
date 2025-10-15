*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 19/02/2013                                              &*
*& Descrição: PGTO IMPOSTOS – Liberação de Lote Pagamento	            &*
*& Transação: ZIMP54                                                  &*
*---------------------------------------------------------------------&*

REPORT  ZIMP54.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: T001.

TYPES:
  BEGIN OF TY_ZIMP_CAD_LOTE,
    LOTE        TYPE ZIMP_CAD_LOTE-LOTE,
    BUKRS       TYPE ZIMP_CAD_LOTE-BUKRS,
    LOEKZ       TYPE ZIMP_CAD_LOTE-LOEKZ,
    STATUS_LOTE TYPE ZIMP_CAD_LOTE-STATUS_LOTE,
    USUARIO     TYPE ZIMP_CAD_LOTE-USUARIO,
    DT_VENC     TYPE ZIMP_CAD_LOTE-DT_VENC,
    DESCR_LOTE  TYPE ZIMP_CAD_LOTE-DESCR_LOTE,
    DEP_RESP    TYPE ZIMP_CAD_LOTE-DEP_RESP,
    DATA_ATUAL  TYPE ZIMP_CAD_LOTE-DATA_ATUAL,
    HORA_ATUAL  TYPE ZIMP_CAD_LOTE-HORA_ATUAL,
  END OF TY_ZIMP_CAD_LOTE,

  BEGIN OF TY_TEXTO,
    NAME_EMPRESA TYPE T001-BUTXT,
    DESCR_LOTE   TYPE ZIMP_CAD_LOTE-DESCR_LOTE,
  END OF TY_TEXTO,

  BEGIN OF TY_ZIMP_APROVADOR,
    BUKRS      TYPE ZIMP_APROVADOR-BUKRS,
    BUKRS_ATE  TYPE ZIMP_APROVADOR-BUKRS_ATE,
    DEP_RESP   TYPE ZIMP_APROVADOR-DEP_RESP,
    WAERS      TYPE ZIMP_APROVADOR-WAERS,
    NIVEL      TYPE ZIMP_APROVADOR-NIVEL,
    APROVADOR  TYPE ZIMP_APROVADOR-APROVADOR,
    VALOR_DE   TYPE ZIMP_APROVADOR-VALOR_DE,
    VALOR_ATE  TYPE ZIMP_APROVADOR-VALOR_ATE,
    DT_VAL_DE  TYPE ZIMP_APROVADOR-DT_VAL_DE,
    DT_VAL_ATE TYPE ZIMP_APROVADOR-DT_VAL_ATE,
    HR_VAL_DE  TYPE ZIMP_APROVADOR-HR_VAL_DE,
    HR_VAL_ATE TYPE ZIMP_APROVADOR-HR_VAL_ATE,
  END OF TY_ZIMP_APROVADOR,

  BEGIN OF TY_ESTRA ,
    BUKRS     TYPE ZIMP_LOTES_APROV-BUKRS,
    LOTE      TYPE ZIMP_LOTES_APROV-LOTE,
    VALOR_DE  TYPE ZIMP_APROVADOR-VALOR_DE,
    VALOR_ATE TYPE ZIMP_APROVADOR-VALOR_ATE,
    APROVADOR TYPE ZIMP_APROVADOR-APROVADOR,
    NIVEL     TYPE ZIMP_APROVADOR-NIVEL,
    ESTADO(4),
    OPCOES(4),
  END OF TY_ESTRA,

  " Lançamento de Impostos
  BEGIN OF TY_ZIMP_LANC_IMPOST,
    DOC_IMPOSTO  TYPE  ZIMP_LANC_IMPOST-DOC_IMPOSTO,
    BUKRS        TYPE  ZIMP_LANC_IMPOST-BUKRS,
    LOTE         TYPE  ZIMP_LANC_IMPOST-LOTE,
    DT_VENC      TYPE  ZIMP_LANC_IMPOST-DT_VENC,
    DT_APURACAO  TYPE  ZIMP_LANC_IMPOST-DT_APURACAO,
    MES_APURACAO TYPE  ZIMP_LANC_IMPOST-MES_APURACAO,
    ANO_APURACAO TYPE  ZIMP_LANC_IMPOST-ANO_APURACAO,
    OBSERVACAO   TYPE  ZIMP_LANC_IMPOST-OBSERVACAO,
    COD_IMPOSTO  TYPE  ZIMP_LANC_IMPOST-COD_IMPOSTO,
    REF_IMPOSTO  TYPE  ZIMP_LANC_IMPOST-REF_IMPOSTO,
    TP_IMPOSTO   TYPE  ZIMP_LANC_IMPOST-TP_IMPOSTO,
    COD_PGTO     TYPE  ZIMP_LANC_IMPOST-COD_PGTO,
    CONV_BANCO   TYPE  ZIMP_LANC_IMPOST-CONV_BANCO,
    HBKID        TYPE  ZIMP_LANC_IMPOST-HBKID,
    DATA_ATUAL   TYPE  ZIMP_LANC_IMPOST-DATA_ATUAL,
    HORA_ATUAL   TYPE  ZIMP_LANC_IMPOST-HORA_ATUAL,
    USUARIO      TYPE  ZIMP_LANC_IMPOST-USUARIO,
    LOEKZ        TYPE  ZIMP_LANC_IMPOST-LOEKZ,
  END OF TY_ZIMP_LANC_IMPOST,

  BEGIN OF TY_ZIMP_LANC_IMP_CT,
    BUKRS        TYPE ZIMP_LANC_IMP_CT-BUKRS,
    DOC_IMPOSTO  TYPE ZIMP_LANC_IMP_CT-DOC_IMPOSTO,
    COD_IMPOSTO  TYPE ZIMP_LANC_IMP_CT-COD_IMPOSTO,
    COD_ABERTURA TYPE ZIMP_LANC_IMP_CT-COD_ABERTURA,
    BSCHL        TYPE ZIMP_LANC_IMP_CT-BSCHL,
    HKONT        TYPE ZIMP_LANC_IMP_CT-HKONT,
    LIFNR        TYPE ZIMP_LANC_IMP_CT-LIFNR,
    KOSTL        TYPE ZIMP_LANC_IMP_CT-KOSTL,
    GSBER        TYPE ZIMP_LANC_IMP_CT-GSBER,
    VALOR_IMP    TYPE ZIMP_LANC_IMP_CT-VALOR_IMP,
    DATA_ATUAL   TYPE ZIMP_LANC_IMP_CT-DATA_ATUAL,
    HORA_ATUAL   TYPE ZIMP_LANC_IMP_CT-HORA_ATUAL,
    USUARIO      TYPE ZIMP_LANC_IMP_CT-USUARIO,
  END OF TY_ZIMP_LANC_IMP_CT,

  BEGIN OF TY_ZIMP_CAD_IMPOSTO,
    COD_IMPOSTO   TYPE ZIMP_CAD_IMPOSTO-COD_IMPOSTO,
    DESCR_IMPOSTO TYPE ZIMP_CAD_IMPOSTO-DESCR_IMPOSTO,
  END OF TY_ZIMP_CAD_IMPOSTO,

  BEGIN OF TY_LFA1,
    LIFNR TYPE LFA1-LIFNR,
    NAME1 TYPE LFA1-NAME1,
    SPERQ TYPE LFA1-SPERQ,
    SPERR TYPE LFA1-SPERR,
    LOEVM TYPE LFA1-LOEVM,
    NODEL TYPE LFA1-NODEL,
  END OF TY_LFA1,

  BEGIN OF TY_LFB1,
    LIFNR TYPE LFB1-LIFNR,
    SPERR TYPE LFB1-SPERR,
    LOEVM TYPE LFB1-LOEVM,
    NODEL TYPE LFB1-NODEL,
  END OF TY_LFB1,

  BEGIN OF TY_T001,
    BUKRS TYPE T001-BUKRS,
    BUTXT TYPE T001-BUTXT,
  END OF TY_T001,

  BEGIN OF TY_ZIMP_TIPOS_IMPOS,
    TP_ARREC    TYPE ZIMP_TIPOS_IMPOS-TP_ARREC,
    ARRECADACAO TYPE ZIMP_TIPOS_IMPOS-ARRECADACAO,
  END OF TY_ZIMP_TIPOS_IMPOS,

  BEGIN OF TY_ZIMP_CAMPOS_GUIA,
    COD_ABERTURA    TYPE ZIMP_CAMPOS_GUIA-COD_CAMP_GUIA,
    DESCR_CAMP_GUIA TYPE ZIMP_CAMPOS_GUIA-DESCR_CAMP_GUIA,
  END OF TY_ZIMP_CAMPOS_GUIA,

  BEGIN OF TY_SAIDA,
    ICON(4)        TYPE C,
    DOC_IMPOSTO    TYPE ZIMP_LANC_IMPOST-DOC_IMPOSTO, " Doc.Imp.
    COD_IMPOSTO    TYPE ZIMP_LANC_IMPOST-COD_IMPOSTO, "Cod.Imp.
    DESCR_IMPOSTO  TYPE ZIMP_CAD_IMPOSTO-DESCR_IMPOSTO, "Descr.Imposto
    TP_IMPOSTO(50) TYPE C, "Tp.Imposto
    COD_PGTO       TYPE ZIMP_LANC_IMPOST-COD_PGTO, "Cond.Pgto
    CONV_BANCO     TYPE ZIMP_LANC_IMPOST-CONV_BANCO, "Conv.Banco
    HBKID(20)      TYPE C, "Bco.Empresa
    DT_APURACAO    TYPE ZIMP_LANC_IMPOST-DT_APURACAO, "Per.Apuração
    MES_ANO(7)     TYPE C, "Mês\ano Apuração
    DT_VENC        TYPE ZIMP_LANC_IMPOST-DT_VENC, "Dt.Vencimento
    LIFNR(50)      TYPE C, "Fornecedor
    VLR_TOTAL      TYPE ZIMP_LANC_IMP_CT-VALOR_IMP, " Valor total do fornecedor
    ZAHLS          TYPE LFB1-ZAHLS,
  END OF TY_SAIDA.


TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
       TYPES: END OF TY_ESTRUTURA.


*----------------------------------------------------------------------*
* Field-symbols
*----------------------------------------------------------------------*
* <fs_data> p/ser a tabela dinâmica onde constaram os dados de exibição
FIELD-SYMBOLS: <FS_DATA>  TYPE ANY TABLE,

*work-área p/ trabalhar os dados antes de inclui <fs_data>
               <WA_DATA>  TYPE ANY,

*campo que recebera dados e apontara p/ os campos dinâmicos da wa.
               <FS_CAMPO> TYPE ANY.

*----------------------------------------------------------------------*
* Tabelas Interna
*----------------------------------------------------------------------*
*Tabela dinâmica de exibição do ALV
DATA: T_DATA TYPE REF TO DATA.

*----------------------------------------------------------------------*
* Estrutura de dados
*----------------------------------------------------------------------*
* Work-Área p/ montar dados dos campos
DATA: WA_FCAT_LVC TYPE LVC_S_FCAT,

* Tabela sem cabeçalho p/ receber dados da wa acima e passar informações
* de campos p/ gerar a tabela dinâmica
      LT_FCAT_LVC TYPE LVC_T_FCAT.



*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: T_BDC               TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      T_MESSTAB           TYPE TABLE OF BDCMSGCOLL,

      IT_ZIMP_LANC_IMPOST TYPE TABLE OF TY_ZIMP_LANC_IMPOST,
      IT_ZIMP_LANC_IMP_CT TYPE TABLE OF TY_ZIMP_LANC_IMP_CT,
      IT_ZIMP_LANC_IMP_AB TYPE TABLE OF TY_ZIMP_LANC_IMP_CT,
      IT_ZIMP_CAD_IMPOSTO TYPE TABLE OF TY_ZIMP_CAD_IMPOSTO,
      IT_ZIMP_CAMPOS_GUIA TYPE TABLE OF TY_ZIMP_CAMPOS_GUIA,
      IT_ZIMP_TIPOS_IMPOS TYPE TABLE OF TY_ZIMP_TIPOS_IMPOS,
      IT_ZIMP_APROVADOR   TYPE TABLE OF TY_ZIMP_APROVADOR,
      IT_ESTRA            TYPE TABLE OF TY_ESTRA,
      IT_LFA1             TYPE TABLE OF TY_LFA1,
      IT_LFB1             TYPE TABLE OF TY_LFB1,
      IT_SAIDA            TYPE TABLE OF TY_SAIDA,

      IT_COLOR            TYPE TABLE OF LVC_S_SCOL.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  WA_CONT             TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_ALV              TYPE REF TO CL_GUI_ALV_GRID,
  WA_LAYOUT           TYPE LVC_S_LAYO,

  WA_ZIMP_LANC_IMPOST TYPE TY_ZIMP_LANC_IMPOST,
  WA_ZIMP_LANC_IMP_CT TYPE TY_ZIMP_LANC_IMP_CT,
  WA_ZIMP_LANC_IMP_AB TYPE TY_ZIMP_LANC_IMP_CT,
  WA_ZIMP_CAD_IMPOSTO TYPE TY_ZIMP_CAD_IMPOSTO,
  WA_ZIMP_CAMPOS_GUIA TYPE TY_ZIMP_CAMPOS_GUIA,
  WA_ZIMP_TIPOS_IMPOS TYPE TY_ZIMP_TIPOS_IMPOS,
  WA_ZIMP_CAD_LOTE    TYPE TY_ZIMP_CAD_LOTE,
  WA_ZIMP_APROVADOR   TYPE TY_ZIMP_APROVADOR,
  WA_LFA1             TYPE TY_LFA1,
  WA_LFB1             TYPE TY_LFB1,
  WA_T001             TYPE TY_T001,
  WA_ESTRA            TYPE TY_ESTRA,
  WA_SAIDA            TYPE TY_SAIDA,

  WA_COLOR            TYPE LVC_S_SCOL.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  IT_FCAT       TYPE TABLE OF TY_ESTRUTURA,
  S_VARIANT     TYPE DISVARIANT           , " Tabela Estrutura co
  T_TOP         TYPE SLIS_T_LISTHEADER,
  XS_EVENTS     TYPE SLIS_ALV_EVENT,
  EVENTS        TYPE SLIS_T_EVENT,
  GD_LAYOUT     TYPE SLIS_LAYOUT_ALV,
  T_PRINT       TYPE SLIS_PRINT_ALV,
  V_REPORT      LIKE SY-REPID,
  T_SORT        TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
  IT_SETLEAF    LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
  ESTRUTURA     TYPE TABLE OF TY_ESTRUTURA,
  VG_I          TYPE I,
  V_REPID       LIKE SY-REPID,
  V_CAMP(7),      " variável p/ montar campo dinâmico
  V_TEXT(100),    " variável p/ montar texto dinâmico
  V_CONTINUA(1),
  VTOTAL        TYPE ZIMP_LANC_IMP_CT-VALOR_IMP VALUE 0.

DATA: REPID            LIKE SY-REPID.
DATA: S_FIELDCAT       TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.
DATA: S_LAYOUT         TYPE SLIS_LAYOUT_ALV.
DATA: S_PRINT          TYPE SLIS_PRINT_ALV.
DATA: S_SORT           TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.
DATA: VARIANTE         LIKE DISVARIANT.
DATA: DEF_VARIANTE     LIKE DISVARIANT.
DATA: S_SELFIELD       TYPE SLIS_SELFIELD.
DATA: LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

DATA WG_TEXTO TYPE TY_TEXTO.

DEFINE MC_PREENCHE_CLASS.
  VG_I = VG_I + 1.
  CLEAR T_SORT.
  T_SORT-SPOS      = VG_I.
  T_SORT-FIELDNAME = &1.
  T_SORT-GROUP     = &2.
  T_SORT-UP        = &3.
  T_SORT-SUBTOT    = &4.
  APPEND T_SORT.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
"DATA   DYFIELDS LIKE DYNPREAD OCCURS 1 WITH HEADER LINE.
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS FOR T001-BUKRS.
PARAMETERS: P_LOTE TYPE ZIMP_CAD_LOTE-LOTE,
            P_USU  TYPE ZIMP_LANC_IMPOST-USUARIO,
            P_DAT  TYPE ZIMP_CAD_LOTE-DATA_ATUAL,
            P_HOR  TYPE ZIMP_CAD_LOTE-HORA_ATUAL.
*            P_LOT1 RADIOBUTTON GROUP RAD1,
*            P_LOT2 RADIOBUTTON GROUP RAD1 DEFAULT 'X',
*            P_LOT3 RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN: END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.
  CLEAR WA_ZIMP_CAD_LOTE.
  SELECT SINGLE LOTE BUKRS LOEKZ STATUS_LOTE USUARIO DT_VENC DESCR_LOTE DEP_RESP DATA_ATUAL HORA_ATUAL
        FROM ZIMP_CAD_LOTE
        INTO WA_ZIMP_CAD_LOTE
        WHERE LOTE = P_LOTE.
  LOOP AT SCREEN.
    IF P_LOTE IS NOT INITIAL.
      IF SCREEN-NAME = 'P_USU' OR
         SCREEN-NAME = 'P_DAT' OR
         SCREEN-NAME = 'P_HOR' .
        SCREEN-INPUT = '0'.
        P_USU =  WA_ZIMP_CAD_LOTE-USUARIO.
        IF WA_ZIMP_CAD_LOTE-STATUS_LOTE = 'A' OR
           WA_ZIMP_CAD_LOTE-STATUS_LOTE = 'L'.
          P_DAT =  WA_ZIMP_CAD_LOTE-DATA_ATUAL.
          P_HOR =  WA_ZIMP_CAD_LOTE-HORA_ATUAL.
        ENDIF.
        WG_TEXTO-DESCR_LOTE = WA_ZIMP_CAD_LOTE-DESCR_LOTE.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF P_BUKRS-LOW IS NOT INITIAL.
      IF SCREEN-NAME = 'P_BUKRS'.
        CLEAR WA_T001.
        SELECT SINGLE BUKRS BUTXT
         FROM T001
         INTO WA_T001
         WHERE BUKRS = P_BUKRS-LOW.
        WG_TEXTO-NAME_EMPRESA = WA_T001-BUTXT.
        MODIFY SCREEN.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  V_CONTINUA = 'S'.

  IF P_BUKRS[] IS INITIAL .
    MESSAGE 'Informe a Empresa!' TYPE 'I'.
  ELSEIF P_LOTE IS INITIAL .
    MESSAGE 'Informe o lote!' TYPE 'I'.
  ELSE.

    SELECT * INTO TABLE @DATA(IT_EMPRESA)
      FROM T001
      WHERE BUKRS IN @P_BUKRS.

    LOOP AT IT_EMPRESA INTO DATA(WA_EMPRESA).

      AUTHORITY-CHECK OBJECT 'F_SKA1_BUK'
             ID 'ACTVT' FIELD '03'       "display
             ID 'BUKRS' FIELD  WA_EMPRESA-BUKRS.
      IF SY-SUBRC <> 0.
        DELETE IT_EMPRESA INDEX SY-TABIX.
      ENDIF.

    ENDLOOP.

    IF IT_EMPRESA[] IS NOT INITIAL.

      PERFORM:
                F_INICIAR_VARIAVES, " Cabeçalho
                F_SELECIONA_DADOS, " Form seleciona dados
                F_SAIDA, " Form de saida
                F_IMPRIME_DADOS.
    ELSE.
      MESSAGE S000(Z_FI) WITH 'Sem autorização para esta Empresa ' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDIF.

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

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Preparando dados'.

  RANGES: RG_STATUS_LOTE FOR ZIMP_CAD_LOTE-STATUS_LOTE.
  RANGES: RG_LOEKZ FOR ZIMP_CAD_LOTE-LOEKZ.

  CLEAR: RG_STATUS_LOTE[], RG_LOEKZ[].

*  CASE ABAP_TRUE.
*    WHEN P_LOT1. "Todos
*    WHEN P_LOT2. "Lotes Não Liberados
*      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = SPACE ) TO RG_STATUS_LOTE.
*      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = SPACE ) TO RG_LOEKZ.
*    WHEN P_LOT3. "Lotes Não Aprovados
*      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = 'L' ) TO RG_STATUS_LOTE.
*      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = SPACE ) TO RG_LOEKZ.
*  ENDCASE.

  SELECT SINGLE LOTE BUKRS LOEKZ STATUS_LOTE USUARIO DT_VENC DESCR_LOTE DEP_RESP
    FROM ZIMP_CAD_LOTE
    INTO WA_ZIMP_CAD_LOTE
    WHERE LOTE = P_LOTE
    AND   BUKRS IN P_BUKRS
    AND   LOEKZ = ' '
    AND   STATUS_LOTE NE 'A'
    AND   STATUS_LOTE IN RG_STATUS_LOTE
    AND   LOEKZ       IN RG_LOEKZ.

  IF  SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  SELECT DOC_IMPOSTO BUKRS  LOTE DT_VENC DT_APURACAO MES_APURACAO ANO_APURACAO OBSERVACAO COD_IMPOSTO REF_IMPOSTO TP_IMPOSTO COD_PGTO CONV_BANCO
    HBKID  DATA_ATUAL HORA_ATUAL USUARIO LOEKZ
    FROM ZIMP_LANC_IMPOST
    INTO TABLE IT_ZIMP_LANC_IMPOST
    WHERE BUKRS IN P_BUKRS
    AND   LOTE  = P_LOTE
    AND   LOEKZ = ''.

  CHECK IT_ZIMP_LANC_IMPOST[] IS NOT INITIAL.

  SELECT BUKRS DOC_IMPOSTO COD_IMPOSTO COD_ABERTURA BSCHL HKONT LIFNR KOSTL GSBER VALOR_IMP DATA_ATUAL HORA_ATUAL USUARIO
    FROM ZIMP_LANC_IMP_CT
    INTO TABLE IT_ZIMP_LANC_IMP_CT
    FOR ALL ENTRIES IN IT_ZIMP_LANC_IMPOST
    WHERE DOC_IMPOSTO = IT_ZIMP_LANC_IMPOST-DOC_IMPOSTO
    AND   BUKRS       = IT_ZIMP_LANC_IMPOST-BUKRS.

  SELECT COD_IMPOSTO DESCR_IMPOSTO
    FROM ZIMP_CAD_IMPOSTO
    INTO TABLE IT_ZIMP_CAD_IMPOSTO
    FOR ALL ENTRIES IN IT_ZIMP_LANC_IMPOST
    WHERE COD_IMPOSTO = IT_ZIMP_LANC_IMPOST-COD_IMPOSTO ORDER BY PRIMARY KEY .

  SELECT TP_ARREC ARRECADACAO
    FROM ZIMP_TIPOS_IMPOS
    INTO TABLE IT_ZIMP_TIPOS_IMPOS
    FOR ALL ENTRIES IN IT_ZIMP_LANC_IMPOST
    WHERE TP_ARREC = IT_ZIMP_LANC_IMPOST-TP_IMPOSTO.

  SELECT LIFNR NAME1 SPERQ SPERR LOEVM NODEL
    FROM LFA1
    INTO TABLE IT_LFA1
    FOR ALL ENTRIES IN  IT_ZIMP_LANC_IMP_CT
    WHERE LIFNR = IT_ZIMP_LANC_IMP_CT-LIFNR.

  IF IT_LFA1[] IS NOT INITIAL.
    SELECT LIFNR SPERR LOEVM NODEL
        FROM LFB1
        INTO TABLE IT_LFB1
        FOR ALL ENTRIES IN  IT_LFA1
        WHERE LIFNR = IT_LFA1-LIFNR
        AND   BUKRS IN P_BUKRS.
  ENDIF.
  DELETE IT_ZIMP_LANC_IMP_CT WHERE VALOR_IMP EQ 0.
  IT_ZIMP_LANC_IMP_AB[] = IT_ZIMP_LANC_IMP_CT[].
  SORT IT_ZIMP_LANC_IMP_AB BY COD_ABERTURA.
  DELETE ADJACENT DUPLICATES FROM IT_ZIMP_LANC_IMP_AB COMPARING COD_ABERTURA.

  SELECT COD_CAMP_GUIA DESCR_CAMP_GUIA
    FROM ZIMP_CAMPOS_GUIA
    INTO TABLE IT_ZIMP_CAMPOS_GUIA
    FOR ALL ENTRIES IN IT_ZIMP_LANC_IMP_AB
    WHERE COD_CAMP_GUIA EQ IT_ZIMP_LANC_IMP_AB-COD_ABERTURA.


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

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Gerando relatório'.

  DATA VLIFNR TYPE ZIMP_LANC_IMP_CT-LIFNR.
  SORT: IT_ZIMP_LANC_IMPOST BY DOC_IMPOSTO,
        IT_ZIMP_LANC_IMP_CT BY DOC_IMPOSTO BUKRS ,
        IT_ZIMP_LANC_IMPOST BY COD_IMPOSTO,
        IT_ZIMP_TIPOS_IMPOS BY TP_ARREC,
        IT_LFA1             BY LIFNR,
        IT_LFB1             BY LIFNR.

  LOOP AT IT_ZIMP_LANC_IMPOST INTO WA_ZIMP_LANC_IMPOST.

    IF WA_ZIMP_CAD_LOTE-STATUS_LOTE = ' '.
      WA_SAIDA-ICON =  ICON_MESSAGE_WARNING.
    ELSE.
      WA_SAIDA-ICON =  ICON_CHECKED.
    ENDIF.
    WA_SAIDA-DOC_IMPOSTO    = WA_ZIMP_LANC_IMPOST-DOC_IMPOSTO.
    WA_SAIDA-COD_IMPOSTO    = WA_ZIMP_LANC_IMPOST-COD_IMPOSTO.

    READ TABLE IT_ZIMP_CAD_IMPOSTO INTO WA_ZIMP_CAD_IMPOSTO WITH KEY COD_IMPOSTO = WA_ZIMP_LANC_IMPOST-COD_IMPOSTO BINARY SEARCH.
    WA_SAIDA-DESCR_IMPOSTO  = WA_ZIMP_CAD_IMPOSTO-DESCR_IMPOSTO.
    CLEAR VLIFNR .
    LOOP AT IT_ZIMP_LANC_IMP_CT INTO WA_ZIMP_LANC_IMP_CT WHERE DOC_IMPOSTO = WA_ZIMP_LANC_IMPOST-DOC_IMPOSTO
                                                         AND   BUKRS       = WA_ZIMP_LANC_IMPOST-BUKRS.
      IF WA_ZIMP_LANC_IMP_CT-LIFNR IS NOT INITIAL.
        VLIFNR          = WA_ZIMP_LANC_IMP_CT-LIFNR.
        EXIT.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = VLIFNR
      IMPORTING
        OUTPUT = VLIFNR.

    CLEAR WA_SAIDA-ZAHLS.
    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_ZIMP_LANC_IMP_CT-LIFNR BINARY SEARCH.
    IF SY-SUBRC  = 0.
      CONCATENATE  VLIFNR '-' WA_LFA1-NAME1 INTO WA_SAIDA-LIFNR.

      IF WA_LFA1-SPERR IS NOT INITIAL OR
         WA_LFA1-SPERQ IS NOT INITIAL OR
         WA_LFA1-LOEVM IS NOT INITIAL OR
         WA_LFA1-NODEL IS NOT INITIAL.
        WA_SAIDA-ZAHLS = 'X'.
      ENDIF.
    ENDIF.

    "Bloqueio?
    IF VLIFNR IS NOT INITIAL.
      READ TABLE IT_LFB1 INTO WA_LFB1 WITH KEY LIFNR = WA_ZIMP_LANC_IMP_CT-LIFNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        IF WA_LFB1-SPERR IS NOT INITIAL OR
           WA_LFB1-LOEVM IS NOT INITIAL OR
           WA_LFB1-NODEL IS NOT INITIAL.
          WA_SAIDA-ZAHLS = 'X'.
        ENDIF.
      ELSE.
        WA_SAIDA-ZAHLS = 'E'.
      ENDIF.
    ENDIF.

    READ TABLE IT_ZIMP_TIPOS_IMPOS INTO WA_ZIMP_TIPOS_IMPOS WITH KEY TP_ARREC = WA_ZIMP_LANC_IMPOST-TP_IMPOSTO BINARY SEARCH.
    CONCATENATE WA_ZIMP_LANC_IMPOST-TP_IMPOSTO '-' WA_ZIMP_TIPOS_IMPOS-ARRECADACAO  INTO WA_SAIDA-TP_IMPOSTO .

    WA_SAIDA-COD_PGTO       = WA_ZIMP_LANC_IMPOST-COD_PGTO.
    WA_SAIDA-CONV_BANCO     = WA_ZIMP_LANC_IMPOST-CONV_BANCO.
    IF WA_ZIMP_LANC_IMPOST-HBKID = 'BBD'.
      CONCATENATE WA_ZIMP_LANC_IMPOST-HBKID '-Bradesco' INTO WA_SAIDA-HBKID.
    ELSE.
      CONCATENATE WA_ZIMP_LANC_IMPOST-HBKID '-Banco do Brasil' INTO WA_SAIDA-HBKID.
    ENDIF.
    WA_SAIDA-DT_APURACAO    = WA_ZIMP_LANC_IMPOST-DT_APURACAO.
    CONCATENATE WA_ZIMP_LANC_IMPOST-MES_APURACAO '/' WA_ZIMP_LANC_IMPOST-ANO_APURACAO INTO WA_SAIDA-MES_ANO.
    WA_SAIDA-DT_VENC        = WA_ZIMP_LANC_IMPOST-DT_VENC.

    APPEND WA_SAIDA TO  IT_SAIDA.
    CLEAR: WA_SAIDA, VLIFNR.

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
  PERFORM F_DEFINIR_EVENTOS.
  PERFORM F_ALV_SORT.
  PERFORM F_ALV.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING:
                                     SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*----------------------------------------------------------------------*
*       Form  f_monta_top_of_page
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.

* Cabeçalho Logo
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIST_TOP_OF_PAGE[].
  "I_LOGO             = 'WELLA_LOGO'.

ENDFORM.        " top_of_page.

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_SORT .

ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV .

  PERFORM ALV_PREENCHE_CAT USING:
               'ICON'          ' '             '04'       ' '     ' '    ' ' , "Icon
               'DOC_IMPOSTO'   TEXT-002        '15'       ' '     ' '    ' ' , "Doc.Imp.
               'COD_IMPOSTO'   TEXT-003        '10'       ' '     ' '    ' ' , "Cod.Imp.
               'DESCR_IMPOSTO' TEXT-004        '20'       ' '     ' '    ' ' , "Descr.Imposto
               'TP_IMPOSTO'    TEXT-005        '40'       ' '     ' '    ' ' , "Tp.Imposto
               'COD_PGTO'      TEXT-006        '15'       ' '     ' '    ' ' , "Cond.Pgto
               'CONV_BANCO'    TEXT-007        '05'       ' '     ' '    ' ' , "Conv.Banco
               'HBKID'         TEXT-008        '15'       ' '     ' '    ' ' , "Bco.Empresa
               'DT_APURACAO'   TEXT-009        '15'       ' '     ' '    ' ' , "Per.Apuração
               'MES_ANO'       TEXT-010        '15'       ' '     ' '    ' ' , "Mês\ano Apuração
               'DT_VENC'       TEXT-011        '15'       ' '     ' '    ' ' , "Dt.Vencimento
               'LIFNR'         TEXT-012        '50'       ' '     ' '    ' ' , "Fornecedor
               'ZAHLS'         TEXT-014        '06'       ' '     ' '    ' ' ,
               'VLR_TOTAL'     TEXT-013        '15'       ' '     ' '    ' ' .

  PERFORM MONTA_FIELDCAT USING:
               'ICON'          ' '                ' '             '04'       ' ' , "Icon
               'DOC_IMPOSTO'   'ZIMP_LANC_IMPOST' TEXT-002        '15'       'DOC_IMPOSTO' , "Doc.Imp.
               'COD_IMPOSTO'   'ZIMP_LANC_IMPOST' TEXT-003        '10'       'COD_IMPOSTO' , "Cod.Imp.
               'DESCR_IMPOSTO' 'ZIMP_CAD_IMPOSTO' TEXT-004        '20'       'DESCR_IMPOSTO' , "Descr.Imposto
               'TP_IMPOSTO'    ' '                TEXT-005        '40'       ' ' , "Tp.Imposto
               'COD_PGTO'      'ZIMP_LANC_IMPOST' TEXT-006        '15'       'COD_PGTO' , "Cond.Pgto
               'CONV_BANCO'    'ZIMP_LANC_IMPOST' TEXT-007        '05'       'CONV_BANCO' , "Conv.Banco
               'HBKID'         ' '                TEXT-008        '20'       ' ' , "Bco.Empresa
               'DT_APURACAO'   'ZIMP_LANC_IMPOST' TEXT-009        '15'       'DT_APURACAO' , "Per.Apuração
               'MES_ANO'       ' '                TEXT-010        '15'       ' ' , "Mês\ano Apuração
               'DT_VENC'       'ZIMP_LANC_IMPOST' TEXT-011        '15'       'DT_VENC' , "Dt.Vencimento
               'LIFNR'         ' '                TEXT-012        '50'       ' ' , "Fornecedor
               'ZAHLS'         ' '                TEXT-014        '06'       ' ' ,
               'VLR_TOTAL'     'ZIMP_LANC_IMP_CT' TEXT-013        '15'       'VALOR_IMP' .


  SORT IT_ZIMP_CAMPOS_GUIA BY COD_ABERTURA.
  LOOP AT IT_ZIMP_LANC_IMP_AB INTO WA_ZIMP_LANC_IMP_AB.
    CLEAR: V_CAMP,
         V_TEXT.
    IF WA_ZIMP_LANC_IMP_AB-LIFNR IS INITIAL.
      READ TABLE IT_ZIMP_CAMPOS_GUIA INTO WA_ZIMP_CAMPOS_GUIA WITH KEY COD_ABERTURA = WA_ZIMP_LANC_IMP_AB-COD_ABERTURA BINARY SEARCH.
      CONCATENATE  'VAL'
                   WA_ZIMP_LANC_IMP_AB-COD_ABERTURA
                   INTO V_CAMP.
      V_TEXT = WA_ZIMP_CAMPOS_GUIA-DESCR_CAMP_GUIA.
      "CONDENSE v_text NO-GAPS.
      PERFORM MONTA_FIELDCAT USING
             V_CAMP   'ZIMP_LANC_IMP_CT' V_TEXT            '15' 'VALOR_IMP'.
      PERFORM ALV_PREENCHE_CAT USING:
              V_CAMP   V_TEXT       '15'       ' '     ' '    ' ' .
    ENDIF.
  ENDLOOP.

*  TABELA DINAMICA
  DATA: T_ALVDATA TYPE REF TO DATA.

* Monta tabela dinâmica
  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      I_STYLE_TABLE   = ' '
*                     tab com as informações de campo
      IT_FIELDCATALOG = LT_FCAT_LVC
    IMPORTING
*                     retorna tab dinâmica com campos informados
      EP_TABLE        = T_DATA.

* Carrega <fs_data> com a estrutura dos campos passados para o metodo
  ASSIGN T_DATA->* TO <FS_DATA>.
  CREATE DATA T_ALVDATA LIKE LINE OF <FS_DATA>.
  ASSIGN T_ALVDATA->* TO <WA_DATA>.

  REFRESH <FS_DATA>.


  LOOP AT IT_SAIDA INTO WA_SAIDA.

* Campos fixos
    PERFORM F_CARREGA_DADOS USING:
        WA_SAIDA-ICON          'ICON'   ,
        WA_SAIDA-DOC_IMPOSTO   'DOC_IMPOSTO'   ,
        WA_SAIDA-COD_IMPOSTO   'COD_IMPOSTO'   ,
        WA_SAIDA-DESCR_IMPOSTO 'DESCR_IMPOSTO' ,
        WA_SAIDA-TP_IMPOSTO   'TP_IMPOSTO'    ,
        WA_SAIDA-COD_PGTO     'COD_PGTO'      ,
        WA_SAIDA-CONV_BANCO   'CONV_BANCO'    ,
        WA_SAIDA-HBKID        'HBKID'         ,
        WA_SAIDA-DT_APURACAO  'DT_APURACAO'   ,
        WA_SAIDA-MES_ANO      'MES_ANO'       ,
        WA_SAIDA-DT_VENC      'DT_VENC'       ,
        WA_SAIDA-LIFNR        'LIFNR'         ,
        WA_SAIDA-ZAHLS        'ZAHLS'         .

    CLEAR WA_SAIDA-VLR_TOTAL.
    LOOP AT IT_ZIMP_LANC_IMP_CT INTO WA_ZIMP_LANC_IMP_CT WHERE DOC_IMPOSTO = WA_SAIDA-DOC_IMPOSTO.
      IF WA_ZIMP_LANC_IMP_CT-LIFNR IS INITIAL.
        CLEAR V_CAMP.
        CONCATENATE 'VAL'
                     WA_ZIMP_LANC_IMP_CT-COD_ABERTURA
                     INTO V_CAMP.
        PERFORM F_CARREGA_DADOS USING:
            WA_ZIMP_LANC_IMP_CT-VALOR_IMP     V_CAMP.
        ADD WA_ZIMP_LANC_IMP_CT-VALOR_IMP TO WA_SAIDA-VLR_TOTAL.
      ENDIF.
    ENDLOOP.

    PERFORM F_CARREGA_DADOS USING:
           WA_SAIDA-VLR_TOTAL    'VLR_TOTAL'.
*  Inclui dados da work-área dinâmica na tabela dinâmica
    PERFORM F_CARREGA_ALV USING <FS_DATA>
                                <WA_DATA>.
    CLEAR           <WA_DATA>..
  ENDLOOP.

* Impressão do ALV passando tabela dinâmica
  PERFORM F_IMPRIME_DADOS_ALV USING <FS_DATA>.
ENDFORM.                    " F_AL

*&---------------------------------------------------------------------*
*&      Form  f_carrega_alv
*&---------------------------------------------------------------------*
*      -->P_TAB  tabela
*      -->P_WA   work-área
*----------------------------------------------------------------------*
FORM F_CARREGA_ALV USING    P_TAB TYPE TABLE
                            P_WA.
*  Inclui dados da work-área dinâmica na tabela dinâmica
  APPEND P_WA TO P_TAB.

ENDFORM.                    " f_carrega_alv
*&---------------------------------------------------------------------*
*&      Form  f_imprime_dados
*&---------------------------------------------------------------------*
FORM F_IMPRIME_DADOS_ALV USING P_ITAB_OUTPUT TYPE TABLE.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = V_REPORT
      IS_LAYOUT                = GD_LAYOUT
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IT_FIELDCAT              = IT_FCAT[]
      IT_SORT                  = T_SORT[]
      I_SAVE                   = 'X'
      IT_EVENTS                = EVENTS
      IS_PRINT                 = T_PRINT
*     IS_VARIANT               = VG_VARIANT
    TABLES
      T_OUTTAB                 = P_ITAB_OUTPUT.

ENDFORM.                    " f_imprime_dados


*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*

FORM USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM           "#EC CALLED
                        RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA: LV_FILE     TYPE STRING.

  DATA:
    BEGIN OF LS_DATE,
      YEAR(4)  TYPE N,
      MONTH(2) TYPE N,
      DAY(2)   TYPE N,
    END OF LS_DATE.


  IF R_UCOMM EQ '&LIBERAR'.
    PERFORM LIBERA_LOTE USING <FS_DATA>.
    RS_SELFIELD-REFRESH = 'X'.
  ELSEIF R_UCOMM EQ '&REINICIA'.
    PERFORM REINICIA_LOTE USING <FS_DATA>.
    RS_SELFIELD-REFRESH = 'X'.
  ENDIF.
ENDFORM.  "User_command

*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.        "#EC CALLED
  DESCRIBE TABLE RT_EXTAB. "Avoid Extended Check Warning
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM. "Set_pf_status

*----------------------------------------------------------------------*
*       Form  f_monta_top_of_page
*----------------------------------------------------------------------*
FORM F_MONTA_TOP_OF_PAGE USING P_LIST_TOP_OF_PAGE TYPE
                               SLIS_T_LISTHEADER.

  DATA: T_HEADER   TYPE SLIS_LISTHEADER,
        V_DATA(10) TYPE C.

  T_HEADER-TYP  = 'H'.
  T_HEADER-INFO = 'Liberação de Lote Pagamento  '(T01).
  APPEND T_HEADER TO P_LIST_TOP_OF_PAGE.
  CLEAR T_HEADER.
  WRITE SY-DATUM USING EDIT  MASK '__.__.____' TO V_DATA.
  CONCATENATE 'Data : '(023)  V_DATA INTO T_HEADER-KEY SEPARATED BY
  SPACE.
  T_HEADER-TYP  = 'S'.
  APPEND T_HEADER TO P_LIST_TOP_OF_PAGE.

ENDFORM.                    " f_monta_top_of_page


*----------------------------------------------------------------------*
*       Form  f_carrega_dados
*----------------------------------------------------------------------*
*   Carrega dados para work-área dinâmica
*----------------------------------------------------------------------*
*      -->P_valor   valor
*      -->P_campo   campo
*----------------------------------------------------------------------*
FORM F_CARREGA_DADOS USING    P_VALOR
                              P_CAMPO.

*Aponta <fs_campo> para <wa_data>-campo montado
  ASSIGN COMPONENT P_CAMPO  OF STRUCTURE <WA_DATA> TO <FS_CAMPO>.

*Move valor para <fs_campo> que esta apontando p/<wa_data>-campo montado
  MOVE P_VALOR TO <FS_CAMPO>.

ENDFORM.                    " f_carrega_dados
*&---------------------------------------------------------------------*
*&      Form  MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD      text
*      -->P_TABREF     text
*      -->P_TEXT       text
*      -->P_OUT        text
*      -->P_REF_FIELD  text
*----------------------------------------------------------------------*
FORM MONTA_FIELDCAT USING P_FIELD
*                          p_tab
                          P_TABREF
                          P_TEXT
                          P_OUT
                          P_REF_FIELD.
**** Se o programa for um ALV, pode aproveitar p/ carregar fieldcat com
* os atributos necessários, caso não se trate de um ALV basta informar o
* campo de referencia, a tabela de referência, o campo  e a tabela.

  CLEAR: S_FIELDCAT, WA_FCAT_LVC.
  WA_FCAT_LVC-FIELDNAME   = S_FIELDCAT-FIELDNAME   = P_FIELD.
  WA_FCAT_LVC-TABNAME     = S_FIELDCAT-TABNAME     = '<FS_DATA>'.
  WA_FCAT_LVC-REF_TABLE   = S_FIELDCAT-REF_TABNAME = P_TABREF.
  WA_FCAT_LVC-SELTEXT     = S_FIELDCAT-SELTEXT_L   = P_TEXT.

  S_FIELDCAT-SELTEXT_M    = P_TEXT.
  S_FIELDCAT-SELTEXT_L    = P_TEXT.
  S_FIELDCAT-SELTEXT_S    = P_TEXT.

  WA_FCAT_LVC-OUTPUTLEN   = S_FIELDCAT-OUTPUTLEN   = P_OUT.
  WA_FCAT_LVC-REF_FIELD   = S_FIELDCAT-REF_FIELDNAME   = P_REF_FIELD.

* carrega fieldcat do alv
  APPEND S_FIELDCAT.

*inclui dados da work-área p/ tabela sem cab.
  APPEND WA_FCAT_LVC TO LT_FCAT_LVC.

ENDFORM.                    " monta_fieldcat
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0649   text
*      -->P_TEXT_003  text
*      -->P_0651   text
*      -->P_0652   text
*      -->P_0653   text
*      -->P_0654   text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING   P_CAMPO  TYPE C
                               P_DESC   TYPE C
                               P_TAM    TYPE C
                               P_HOT    TYPE C
                               P_ZERO   TYPE C
                               P_SOMA   TYPE C.


  DATA: WL_FCAT TYPE TY_ESTRUTURA.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SELTEXT_S = P_DESC.
  WL_FCAT-SELTEXT_M = P_DESC.
  WL_FCAT-SELTEXT_L = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-DO_SUM    = P_SOMA.
  IF P_CAMPO = 'ICON'.
    WL_FCAT-ICON      = 'X'.
  ENDIF.


  APPEND WL_FCAT TO IT_FCAT.
ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INICIAR_VARIAVES .
  DATA:
    W_TEXTO1(10),
    W_TEXTO2(10),
    W_TEXTO3(40),

    W_EMPRESA_TEXTO(40),
    W_EXER_TEXTO(40),
    W_PER_TEXTO(40),

    EMPRESA             TYPE C LENGTH 99,
    LOTE                TYPE C LENGTH 99,
    USUARIO             TYPE C LENGTH 50.


  V_REPORT = SY-REPID.

  W_TEXTO3 = 'Liberação de Lote de Pagamento Impostos'.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO3.

  IF P_BUKRS[] IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(IT_T001)
      FROM T001
     WHERE BUKRS IN @P_BUKRS.

    DESCRIBE TABLE IT_T001 LINES DATA(LINHAS_EMPRESA).

    CASE LINHAS_EMPRESA.
      WHEN 1.
        READ TABLE IT_T001 INDEX 1 INTO DATA(WA_T001_).
        W_EMPRESA_TEXTO = 'Empresa    :'.
        CONCATENATE W_EMPRESA_TEXTO WA_T001_-BUKRS '-' WA_T001_-BUTXT INTO EMPRESA SEPARATED BY SPACE.
      WHEN OTHERS.
        CLEAR: EMPRESA.
        LOOP AT IT_T001 INTO WA_T001_.
          IF EMPRESA IS NOT INITIAL.
            EMPRESA = EMPRESA && ','.
          ENDIF.
          EMPRESA = |{ EMPRESA } { WA_T001_-BUKRS }|.
        ENDLOOP.
        EMPRESA = |Empresas    :{ EMPRESA }|.
    ENDCASE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' EMPRESA.
  ENDIF.

  IF P_LOTE IS NOT INITIAL.
    W_EXER_TEXTO = 'Lote  :'.
    CONCATENATE W_EXER_TEXTO P_LOTE '-' WA_ZIMP_CAD_LOTE-DESCR_LOTE  INTO LOTE SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' LOTE.
  ENDIF.

*  CASE ABAP_TRUE.
*    WHEN P_LOT1. "Todos
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' 'Status      : Todos'.
*    WHEN P_LOT2. "Lotes Não Liberados
*      PERFORM F_CONSTRUIR_CABECALHO USING 'S' 'Status      : Lotes Não Liberados'.
*    WHEN P_LOT3. "Lotes Não Aprovados
*      PERFORM F_CONSTRUIR_CABECALHO USING 'S' 'Status      : Lotes Não Aprovados'.
*  ENDCASE.

  IF P_USU IS NOT INITIAL.
    W_EXER_TEXTO = 'Usuário  :'.
    CONCATENATE W_EXER_TEXTO P_USU  INTO USUARIO SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' USUARIO.
  ENDIF.


ENDFORM.                    " F_INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  f_construir_cabecalho
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TYP        text
*      -->TEXT       text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO    USING TYP TEXT.


  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  LIBERA_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIBERA_LOTE USING GT_OUTTAB TYPE TABLE.

  DATA LFIELD(10) VALUE   'ICON'.
  DATA LFIELDV(10) VALUE   'VLR_TOTAL'.
  DATA LFIELDD(10) VALUE   'DT_VENC'.
  DATA LFIELDB(10) VALUE   'ZAHLS'.
  DATA WERRO(1).
  FIELD-SYMBOLS <WA> TYPE ANY.
  ASSIGN LOCAL COPY OF INITIAL LINE OF GT_OUTTAB TO <WA>.
  VTOTAL = 0.
  CLEAR WERRO.
  LOOP AT GT_OUTTAB ASSIGNING  <WA>.
    ASSIGN COMPONENT LFIELDD  OF STRUCTURE <WA> TO <FS_CAMPO>.
    IF <FS_CAMPO> LT SY-DATUM.
      MESSAGE 'Data de vencimento menor que a data atual' TYPE 'I'.
      WERRO = 'X'.
      EXIT.
    ENDIF.
    ASSIGN COMPONENT LFIELDB  OF STRUCTURE <WA> TO <FS_CAMPO>.
    IF <FS_CAMPO> NE ''.
      IF <FS_CAMPO> = 'E'.
        MESSAGE 'Fornecedor não expandido para esta empresa!' TYPE 'I'.
      ELSE.
        MESSAGE 'Fornecedor bloqueado' TYPE 'I'.
      ENDIF.
      WERRO = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF  WERRO IS INITIAL.
    LOOP AT GT_OUTTAB ASSIGNING  <WA>.

      ASSIGN COMPONENT LFIELD  OF STRUCTURE <WA> TO <FS_CAMPO>.
      MOVE  ICON_CHECKED TO <FS_CAMPO>.

      MODIFY GT_OUTTAB FROM <WA>.

      ASSIGN COMPONENT LFIELDV  OF STRUCTURE <WA> TO <FS_CAMPO>.
      ADD <FS_CAMPO> TO VTOTAL.
    ENDLOOP.

    PERFORM ENVIA_EMAIL USING WA_ZIMP_CAD_LOTE-BUKRS.
    UPDATE ZIMP_CAD_LOTE SET STATUS_LOTE = 'L'
                             USUARIO     = SY-UNAME
                             DATA_ATUAL  = SY-DATUM
                             HORA_ATUAL  = SY-UZEIT "09.08.2013
    WHERE LOTE = WA_ZIMP_CAD_LOTE-LOTE.
  ENDIF.

ENDFORM.                    " LIBERA_LOTE
*&---------------------------------------------------------------------*
*&      Form  REINICIA_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REINICIA_LOTE USING GT_OUTTAB TYPE TABLE..
  DATA LFIELD(10) VALUE   'ICON'.
  FIELD-SYMBOLS <WA> TYPE ANY.
  ASSIGN LOCAL COPY OF INITIAL LINE OF GT_OUTTAB TO <WA>.

  LOOP AT GT_OUTTAB ASSIGNING  <WA>.
    LFIELD = 'ICON'.
    ASSIGN COMPONENT LFIELD  OF STRUCTURE <WA> TO <FS_CAMPO>.
    MOVE  ICON_SYSTEM_UNDO TO <FS_CAMPO>.
    MODIFY GT_OUTTAB FROM <WA>.
  ENDLOOP.

  UPDATE ZIMP_CAD_LOTE SET STATUS_LOTE = ' '
  WHERE LOTE = WA_ZIMP_CAD_LOTE-LOTE.

ENDFORM.                    " REINICIA_LOTE
*&---------------------------------------------------------------------*
*&      Module  SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_LOTE INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_LOTE OCCURS 0,
          LOTE       TYPE ZIMP_CAD_LOTE-LOTE,
          DESCR_LOTE TYPE ZIMP_CAD_LOTE-DESCR_LOTE,
          BUKRS      TYPE ZIMP_CAD_LOTE-BUKRS,
        END OF TL_LOTE.
  DATA: L_DYNPFIELDS LIKE DYNPREAD OCCURS 0 WITH HEADER LINE.
  REFRESH L_DYNPFIELDS.
  CLEAR   L_DYNPFIELDS.
  IF P_BUKRS IS  INITIAL.
    L_DYNPFIELDS-FIELDNAME  = 'P_BUKRS'.
    APPEND L_DYNPFIELDS.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME     = SY-REPID
        DYNUMB     = SY-DYNNR
      TABLES
        DYNPFIELDS = L_DYNPFIELDS.
    READ TABLE L_DYNPFIELDS INDEX 1.
    MOVE L_DYNPFIELDS-FIELDVALUE TO P_BUKRS.
  ENDIF.

  SELECT LOTE DESCR_LOTE BUKRS
    FROM ZIMP_CAD_LOTE
    INTO TABLE TL_LOTE
    WHERE LOEKZ = ''
    AND STATUS_LOTE NE 'A'
    AND BUKRS IN P_BUKRS.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'LOTE'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZIMP_CAD_LOTE-LOTE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_LOTE
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ENVIA_EMAIL USING P_EMPRESA TYPE BUKRS.


  FIELD-SYMBOLS: <FS_SOLIX> TYPE SOLIX.

* Objetos para enviar email
  DATA: OBJPACK     LIKE SOPCKLSTI1 OCCURS  2 WITH HEADER LINE.
  DATA: OBJHEAD     LIKE SOLISTI1   OCCURS  1 WITH HEADER LINE.
  DATA: OBJBIN_ORD  LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.
  DATA: OBJBIN_LOG  LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.
  DATA: OBJBIN_ANN  TYPE SOLISTI1.
  DATA: OBJBIN    LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE,
        OBJBIN1   TYPE SOLI_TAB, "   OCCURS 10 WITH HEADER LINE.
        WA_OBJBIN LIKE LINE OF OBJBIN.
  DATA: CONTENT_HEX TYPE STANDARD TABLE OF SOLIX WITH HEADER LINE.
  DATA: OBJTXT      LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.
  DATA: RECLIST     LIKE SOMLRECI1  OCCURS  5 WITH HEADER LINE.
  DATA: DOC_CHNG    LIKE SODOCCHGI1.
  DATA: TAB_LINES   LIKE SY-TABIX.
  DATA: L_ANEX      TYPE STRING.
  DATA: L_LENG      TYPE I.
  DATA: L_ARQ       TYPE STRING.
  DATA: L_TAM       TYPE I.
  DATA: L_TAM_ORD   TYPE I.
  DATA: L_TAM_LOG   TYPE I.
  DATA: L_EMAIL(300) TYPE C.
  DATA: VUSER       TYPE SY-UNAME.

  DATA: VDEP_RESP(2),
        VVALOR_ATE TYPE ZIMP_LOTES_APROV-VALOR_ATE.

  DATA: IT_SHORTCUT_PARAM LIKE ZST_SHORTCUT_PAR OCCURS 0 WITH HEADER LINE.
  DATA: CONTENT TYPE STRING.

*  ** Pass the required parameters and create the shortcut
  CLEAR IT_SHORTCUT_PARAM.
  REFRESH IT_SHORTCUT_PARAM.

  " Pegar o primeiro e-mail da lista de aprovadores
  CLEAR VDEP_RESP.
  VVALOR_ATE = 0.

  SELECT SINGLE * INTO @DATA(WA_EMPRESAS)
    FROM T001
   WHERE BUKRS EQ @P_EMPRESA.

  SELECT  BUKRS BUKRS_ATE DEP_RESP WAERS NIVEL APROVADOR VALOR_DE VALOR_ATE DT_VAL_DE DT_VAL_ATE HR_VAL_DE HR_VAL_ATE
    FROM ZIMP_APROVADOR
    INTO TABLE IT_ZIMP_APROVADOR
    WHERE BUKRS     LE WA_EMPRESAS-BUKRS
      AND BUKRS_ATE GE WA_EMPRESAS-BUKRS.

  SORT IT_ZIMP_APROVADOR BY BUKRS BUKRS_ATE DEP_RESP NIVEL.

  LOOP AT IT_ZIMP_APROVADOR INTO WA_ZIMP_APROVADOR.
    IF  WA_ZIMP_APROVADOR-BUKRS_ATE IS INITIAL.
      IF  WA_ZIMP_APROVADOR-BUKRS NE WA_EMPRESAS-BUKRS.
        CONTINUE.
      ENDIF.
    ELSEIF WA_ZIMP_APROVADOR-BUKRS     GT WA_EMPRESAS-BUKRS OR
           WA_ZIMP_APROVADOR-BUKRS_ATE LT WA_EMPRESAS-BUKRS.
      CONTINUE.
    ENDIF.
    IF WA_ZIMP_CAD_LOTE-DEP_RESP = WA_ZIMP_APROVADOR-DEP_RESP.
      IF ( WA_ZIMP_APROVADOR-DT_VAL_DE LT SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE GT SY-DATUM ) OR
         ( WA_ZIMP_APROVADOR-DT_VAL_DE EQ SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE GT SY-DATUM
           AND WA_ZIMP_APROVADOR-HR_VAL_DE LE SY-UZEIT )  OR
         ( WA_ZIMP_APROVADOR-DT_VAL_DE LT SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE EQ SY-DATUM
           AND WA_ZIMP_APROVADOR-HR_VAL_ATE GE SY-UZEIT ) OR
         ( WA_ZIMP_APROVADOR-DT_VAL_DE EQ SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE EQ SY-DATUM
           AND WA_ZIMP_APROVADOR-HR_VAL_DE LE SY-UZEIT AND WA_ZIMP_APROVADOR-HR_VAL_ATE GE SY-UZEIT ).
        IF VTOTAL > VVALOR_ATE.
          VVALOR_ATE = WA_ZIMP_APROVADOR-VALOR_ATE.
          VDEP_RESP = WA_ZIMP_APROVADOR-DEP_RESP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF VDEP_RESP IS INITIAL.
    LOOP AT IT_ZIMP_APROVADOR INTO WA_ZIMP_APROVADOR.
      IF  WA_ZIMP_APROVADOR-BUKRS_ATE IS INITIAL.
        IF  WA_ZIMP_APROVADOR-BUKRS NE WA_EMPRESAS-BUKRS.
          CONTINUE.
        ENDIF.
      ELSEIF WA_ZIMP_APROVADOR-BUKRS     GT WA_EMPRESAS-BUKRS OR
             WA_ZIMP_APROVADOR-BUKRS_ATE LT WA_EMPRESAS-BUKRS.
        CONTINUE.
      ENDIF.
      IF WA_ZIMP_APROVADOR-DEP_RESP IS INITIAL.
        IF ( WA_ZIMP_APROVADOR-DT_VAL_DE LT SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE GT SY-DATUM ) OR
           ( WA_ZIMP_APROVADOR-DT_VAL_DE EQ SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE GT SY-DATUM
             AND WA_ZIMP_APROVADOR-HR_VAL_DE LE SY-UZEIT )  OR
           ( WA_ZIMP_APROVADOR-DT_VAL_DE LT SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE EQ SY-DATUM
             AND WA_ZIMP_APROVADOR-HR_VAL_ATE GE SY-UZEIT ) OR
           ( WA_ZIMP_APROVADOR-DT_VAL_DE EQ SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE EQ SY-DATUM
             AND WA_ZIMP_APROVADOR-HR_VAL_DE LE SY-UZEIT AND WA_ZIMP_APROVADOR-HR_VAL_ATE GE SY-UZEIT ).
          IF VTOTAL > VVALOR_ATE.
            VVALOR_ATE = WA_ZIMP_APROVADOR-VALOR_ATE.
            VDEP_RESP = WA_ZIMP_APROVADOR-DEP_RESP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  REFRESH IT_ESTRA.
  LOOP AT IT_ZIMP_APROVADOR INTO WA_ZIMP_APROVADOR.
    IF  WA_ZIMP_APROVADOR-BUKRS_ATE IS INITIAL.
      IF  WA_ZIMP_APROVADOR-BUKRS NE WA_EMPRESAS-BUKRS.
        CONTINUE.
      ENDIF.
    ELSEIF WA_ZIMP_APROVADOR-BUKRS     GT WA_EMPRESAS-BUKRS OR
           WA_ZIMP_APROVADOR-BUKRS_ATE LT WA_EMPRESAS-BUKRS.
      CONTINUE.
    ENDIF.
    IF WA_ZIMP_APROVADOR-VALOR_ATE <= VVALOR_ATE.
      WA_ESTRA-BUKRS        = WA_EMPRESAS-BUKRS.
      WA_ESTRA-LOTE         = WA_ZIMP_CAD_LOTE-LOTE.
      WA_ESTRA-VALOR_DE     = WA_ZIMP_APROVADOR-VALOR_DE.
      WA_ESTRA-VALOR_ATE    = WA_ZIMP_APROVADOR-VALOR_ATE.
      WA_ESTRA-APROVADOR    = WA_ZIMP_APROVADOR-APROVADOR.
      WA_ESTRA-NIVEL        = WA_ZIMP_APROVADOR-NIVEL.
      IF VDEP_RESP IS INITIAL.
        APPEND WA_ESTRA TO IT_ESTRA.
      ELSEIF VDEP_RESP = WA_ZIMP_APROVADOR-DEP_RESP.
        IF ( WA_ZIMP_APROVADOR-DT_VAL_DE LT SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE GT SY-DATUM ) OR
           ( WA_ZIMP_APROVADOR-DT_VAL_DE EQ SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE GT SY-DATUM
             AND WA_ZIMP_APROVADOR-HR_VAL_DE LE SY-UZEIT )  OR
           ( WA_ZIMP_APROVADOR-DT_VAL_DE LT SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE EQ SY-DATUM
             AND WA_ZIMP_APROVADOR-HR_VAL_ATE GE SY-UZEIT ) OR
           ( WA_ZIMP_APROVADOR-DT_VAL_DE EQ SY-DATUM AND WA_ZIMP_APROVADOR-DT_VAL_ATE EQ SY-DATUM
             AND WA_ZIMP_APROVADOR-HR_VAL_DE LE SY-UZEIT AND WA_ZIMP_APROVADOR-HR_VAL_ATE GE SY-UZEIT ).
          APPEND WA_ESTRA TO IT_ESTRA.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  SORT IT_ESTRA BY NIVEL.
  READ TABLE IT_ESTRA INTO WA_ESTRA INDEX 1.

  DATA: BSMTP_ADDR TYPE ADR6-SMTP_ADDR.

  SELECT SINGLE ADR6~SMTP_ADDR INTO BSMTP_ADDR
    FROM USR21
      INNER JOIN ADR6
         ON  USR21~ADDRNUMBER = ADR6~ADDRNUMBER
        AND USR21~PERSNUMBER = ADR6~PERSNUMBER
            WHERE USR21~BNAME = WA_ESTRA-APROVADOR.

  CLEAR: DOC_CHNG, OBJTXT[], OBJPACK[], OBJBIN[], OBJHEAD[], RECLIST[].

* Criação do documento de Email
  DOC_CHNG-OBJ_NAME = 'LOG_ESTRA'.

* Assunto do Email
  DOC_CHNG-OBJ_DESCR = 'Aprovação Pagamento Imposto - Vencimento'.

* Texto
  OBJTXT-LINE = 'Está disponível para aprovação no sistema SAP, o lote de impostos abaixo.'.
  APPEND OBJTXT.
  CLEAR OBJTXT.
  APPEND OBJTXT.

  OBJTXT-LINE = 'Para aprovar clique no link "Estratégia" em anexo.' .
  APPEND OBJTXT.
  CLEAR OBJTXT.

  OBJTXT-LINE = '------------------------------------------------------------------------------------------------' .
  APPEND OBJTXT.
  CLEAR OBJTXT.

  DATA: CTOTAL(20),
        VDATA(10).
  CONCATENATE WA_ZIMP_CAD_LOTE-DT_VENC+6(2) WA_ZIMP_CAD_LOTE-DT_VENC+4(2) WA_ZIMP_CAD_LOTE-DT_VENC+0(4) INTO VDATA SEPARATED BY '.'.

  WRITE VTOTAL TO CTOTAL CURRENCY 'USD'.

  CONDENSE CTOTAL NO-GAPS.
  CONCATENATE 'Empresa:' WA_EMPRESAS-BUKRS '-' WA_EMPRESAS-BUTXT 'Lote:' P_LOTE ' R$' CTOTAL ' Venc.'  VDATA INTO OBJTXT SEPARATED BY SPACE.
  APPEND OBJTXT.
  CLEAR OBJTXT.

* Setar tamanho da mensagem
  DESCRIBE TABLE OBJTXT LINES TAB_LINES.
  READ TABLE OBJTXT INDEX TAB_LINES.
  DOC_CHNG-DOC_SIZE = ( TAB_LINES - 1 ) * 255 + STRLEN( OBJTXT ).

* Criar entrada de documento comprimido
  CLEAR OBJPACK-TRANSF_BIN.
  "OBJPACK-TRANSF_BIN = 'X'.
  OBJPACK-HEAD_START = 1.
  OBJPACK-HEAD_NUM   = 0.
  OBJPACK-BODY_START = 1.
  OBJPACK-BODY_NUM   = TAB_LINES.
  OBJPACK-DOC_TYPE   = 'RAW'.
  APPEND OBJPACK.


  CALL FUNCTION 'ZFM_CREATE_SHORTCUT'
    EXPORTING
      RECIPIENT_USER_ID = WA_ESTRA-APROVADOR
      TRANSACTION       = 'ZIMP56'
    IMPORTING
      CONTENT           = CONTENT
    TABLES
      SHORTCUT_PARAM    = IT_SHORTCUT_PARAM.

  CLEAR : TAB_LINES, OBJBIN.
  CONCATENATE CONTENT WA_OBJBIN-LINE INTO WA_OBJBIN-LINE.
  APPEND  WA_OBJBIN TO OBJBIN.

  DESCRIBE TABLE OBJBIN LINES TAB_LINES.
  OBJHEAD = 'ESTRATEGIA.SAP'.
  APPEND OBJHEAD.

** Creation of the entry for the compressed attachment
  OBJPACK-TRANSF_BIN = 'X'.
  OBJPACK-HEAD_START = 1.
  OBJPACK-HEAD_NUM   = 1.
  OBJPACK-BODY_START = 1.
  OBJPACK-BODY_NUM   = TAB_LINES.
  OBJPACK-DOC_TYPE   = 'EXT'." SAP
  OBJPACK-OBJ_NAME   = 'SAPSHORTCUTMAIL'.
  OBJPACK-OBJ_DESCR  = 'ESTRATEGIA.SAP'.
  OBJPACK-DOC_SIZE   = TAB_LINES * 255.
  APPEND OBJPACK.

* Alimentar destinatários do email
  IF BSMTP_ADDR IS INITIAL.
    MESSAGE 'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.' TYPE 'I'.
    EXIT.
  ENDIF.

  RECLIST-RECEIVER = BSMTP_ADDR.
  RECLIST-REC_TYPE = 'U'.                    "Define email externo
  APPEND RECLIST.

* Enviar email
  VUSER = SY-UNAME.
  SY-UNAME = 'R3JOB'.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = DOC_CHNG
      PUT_IN_OUTBOX              = 'X'
      COMMIT_WORK                = 'X'
    TABLES
      PACKING_LIST               = OBJPACK
      OBJECT_HEADER              = OBJHEAD
      CONTENTS_BIN               = OBJBIN
      CONTENTS_TXT               = OBJTXT      "CONTENTS_HEX = CONTENT_HEX
      RECEIVERS                  = RECLIST
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      OPERATION_NO_AUTHORIZATION = 4
      OTHERS                     = 99.

  SY-UNAME = VUSER.

ENDFORM.                    " ENVIA_EMAIL
