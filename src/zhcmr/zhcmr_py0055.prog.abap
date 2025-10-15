
*&---------------------------------------------------------------------*
*& Report ZHCMR_PY0055
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZHCMR_PY0055.

TABLES: PA0001, PA0000, PERNR, ZHCMT_PY_0025.

TYPES: BEGIN OF TY_saida,
         Matricula           TYPE  PA0001-PERNR,
         Nome_EMP            TYPE  PA0002-CNAME,
         Empresa             TYPE  PA0001-BUKRS,
         Desc_Empresa        TYPE  T001-BUTXT,
         Filial              TYPE  PA0001-WERKS,
         Desc_Filial         TYPE  T001W-NAME1,
         Centro_Custo        TYPE  PA0001-KOSTL,
         Desc_CCusto         TYPE  CSKT-LTEXT,
         Unid_Org            TYPE  PA0001-ORGEH,
         Desc_UOrg           TYPE  HRP1000-STEXT,
         Cargo               TYPE  PA0001-STELL,
         Desc_Cargo          TYPE  HRP1000-STEXT,
         Cat_Empregado       TYPE  CHAR20,
         DT_adm              TYPE  BEGDA,
         DT_desl             TYPE  ENDDA,
         Base_INSS           TYPE  BETRG,
         Base_INSS_13        TYPE  BETRG,
         Dedu_Maternidade    TYPE  BETRG,
         Dedu_Maternidade_13 TYPE  BETRG,
         Dedu_Faltas         TYPE  BETRG,
         Liminar_Amaggi      TYPE  BETRG,
         Liminar_Hermasa     TYPE  BETRG,
         Result_base_INSS    TYPE  BETRG,
         Result_base_INSS_13 TYPE  BETRG,
         INSS_Empregado_314  TYPE  BETRG,
         INSS_Empregador     TYPE  BETRG,
         PERC_Empresa        TYPE  T7BRB2-PCONT,
         Contrib_RAT         TYPE  BETRG,
         PERC_RAT_AJUSTADO   TYPE  UKURS_CURR,
         PERC_RAT            TYPE  T7BRB2-PSAT,
         PERC_FAP            TYPE  T7BRB2-PFAP,
         Cont_Terceiros      TYPE  BETRG,
         PERC_Terceiros      TYPE  T7BRB2-THDRT,
         Dedu_Sal_FamIlia    TYPE  BETRG,
         Total_INSS_Empresa  TYPE  BETR28_KK,
         Total_INSS_Geral    TYPE  BETR28_KK,
       END OF  TY_saida,

       BEGIN OF TY_pa0002,
         PERNR TYPE PA0001-PERNR,
         CNAME TYPE PA0002-CNAME,
       END OF TY_pa0002,

       BEGIN OF TY_t001,
         BUKRS TYPE PA0001-BUKRS,
         BUTXT TYPE  T001-BUTXT,
       END OF TY_t001,

       BEGIN OF TY_t001W,
         WERKS TYPE PA0001-WERKS,
         NAME1 TYPE  t001W-NAME1,
       END OF TY_t001W,

       BEGIN OF TY_cskt,
         KOSTL TYPE  PA0001-KOSTL,
         KOKRS TYPE  PA0001-KOKRS,
         KTEXT TYPE  KTEXT,
       END OF TY_cskt,

       BEGIN OF TY_hrp1000 ,
         OBJID TYPE PA0001-ORGEH,
         STEXT TYPE HRP1000-STEXT,
       END OF TY_hrp1000.

DATA: V_begda           TYPE BEGDA,
      V_DATA            TYPE ENDDA,
      V_ENDDA           TYPE ENDDA,
      V_POSICAO         TYPE PLANS,
      V_DT_ADMISSAO     TYPE DATS,
      V_DT_DESLIGAMENTO TYPE DATS,
      V_perc_empresa    TYPE I.

DATA:
  CL_READ_PAYROLL TYPE REF TO CL_HR_BR_READ_PAYROLL,
  TL_RGDIR        TYPE TABLE OF PC261,
  WL_RGDIR        TYPE PC261,
  TL_PAYROLL      TYPE TABLE OF PAYBR_RESULT,
  WL_PAYROLL      TYPE          PAYBR_RESULT,
  TL_RT           TYPE TABLE OF PC207,
  WL_RT           TYPE PC207.

DATA: IT_PA0001     TYPE TABLE OF PA0001,
      WA_PA0001     TYPE PA0001,
      IT_pa0000     TYPE TABLE OF PA0000,
      WA_pa0000     TYPE PA0000,
      IT_pa0002     TYPE TABLE OF ty_PA0002,
      WA_pa0002     TYPE ty_PA0002,
      IT_t001       TYPE TABLE OF TY_t001,
      WA_t001       TYPE TY_t001,
      it_t001W      TYPE TABLE OF TY_t001W,
      WA_t001W      TYPE TY_t001W,
      IT_cskt       TYPE TABLE OF TY_cskt,
      WA_cskt       TYPE TY_cskt,
      IT_UNID_ORG   TYPE TABLE OF TY_hrp1000,
      WA_UNID_ORG   TYPE TY_hrp1000,
      IT_DESC_CARGO TYPE TABLE OF TY_hrp1000,
      WA_DESC_CARGO TYPE TY_hrp1000,
      IT_t7brb2     TYPE TABLE OF T7BRB2,
      WA_t7brb2     TYPE T7BRB2,
      IT_pa0398     TYPE TABLE OF PA0398,
      WA_pa0398     TYPE     PA0398,
      IT_SAIDA      TYPE TABLE OF TY_SAIDA,
      WA_SAIDA      TYPE TY_SAIDA.
RANGES: RG_AFAST FOR PA2001-SUBTY,
          RG_DSR   FOR T512W-LGART,
          RG_ADNOT FOR T512W-LGART,
          RG_BH    FOR T512W-LGART,
          RG_100   FOR T512W-LGART,
          RG_50    FOR T512W-LGART,
          RG_PREAS FOR PA0008-PREAS,
          RG_GRAT  FOR T512W-LGART,
          RG_OCRSN FOR PC261-OCRSN.

DATA: G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_1        TYPE REF TO CL_GUI_CONTAINER,
      DG_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_2        TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_2A       TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV      TYPE REF TO CL_GUI_CONTAINER,
      PICTURE            TYPE REF TO CL_GUI_PICTURE,
      GS_LAYOUT          TYPE LVC_S_LAYO,
      GS_VARIANT         TYPE DISVARIANT,
      CTL_ALV            TYPE REF TO CL_GUI_ALV_GRID,
      IT_FIELDCATALOG    TYPE LVC_T_FCAT,
      WA_FIELDCATALOG    TYPE LVC_S_FCAT,
      GS_SCROLL_COL      TYPE LVC_S_COL,
      GS_SCROLL_ROW      TYPE LVC_S_ROID,
      IT_EXCLUDE_FCODE   TYPE UI_FUNCTIONS,
      DG_DYNDOC_ID       TYPE REF TO CL_DD_DOCUMENT,
      TABLE_ELEMENT      TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN             TYPE REF TO CL_DD_AREA,
      TABLE_ELEMENT2     TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN_1           TYPE REF TO CL_DD_AREA,
      COLUMN_2           TYPE REF TO CL_DD_AREA,
      DG_HTML_CNTRL      TYPE REF TO CL_GUI_HTML_VIEWER.


**********************************************************************
* PARAMETROS DE SELECAO
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK TP000 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
       P_mes FOR ZHCMT_PY_0025-MESPR NO-EXTENSION NO INTERVALS OBLIGATORY,
       P_ano FOR ZHCMT_PY_0025-ANOPR NO-EXTENSION NO INTERVALS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK TP000 .

SELECTION-SCREEN BEGIN OF BLOCK TP001 WITH FRAME TITLE TEXT-004.

  SELECT-OPTIONS:
        P_PERNR FOR PA0001-PERNR MATCHCODE OBJECT PREM NO INTERVALS, "N pessoal
        P_BUKRS FOR PERNR-BUKRS NO INTERVALS, "Empresa
        P_WERKS FOR PERNR-WERKS NO INTERVALS , "AreaRecursos Humanos
        P_KOSTL FOR PERNR-KOSTL NO INTERVALS , "Centro de Custo
        P_UNIOR FOR PERNR-ORGEH NO INTERVALS , "Unid.Organizacional
        P_POSI FOR PERNR-PLANS NO INTERVALS , "Posição
        P_CARGO FOR PERNR-STELL NO INTERVALS , "Cargo
        P_STATUS FOR PA0000-STAT2 NO INTERVALS . "Status ocupação
SELECTION-SCREEN END OF BLOCK TP001 .

START-OF-SELECTION.





  PERFORM FM_START_OF_SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FM_START_OF_SELECTION .
  PERFORM FM_DADOS_SELECIONA.
  PERFORM FM_DADOS_PROCESSA.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FM_DADOS_SELECIONA.

  RG_OCRSN-SIGN   = 'I'.
  RG_OCRSN-OPTION = 'EQ'.
  RG_OCRSN-LOW    = ''.
  APPEND RG_OCRSN.
  CLEAR RG_OCRSN.

  RG_OCRSN-SIGN   = 'I'.
  RG_OCRSN-OPTION = 'EQ'.
  RG_OCRSN-LOW    = 'RESC'.
  APPEND RG_OCRSN.
  CLEAR RG_OCRSN.

  CONCATENATE P_ANO-LOW P_MES-LOW '01' INTO V_ENDDA.

  CONCATENATE '01' P_MES-LOW  P_ANO-LOW INTO V_DATA.

  CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
    EXPORTING
      I_DATE = V_ENDDA
    IMPORTING
      E_DATE = V_BEGDA.
  .

  IF P_POSI-LOW IS INITIAL.

    SELECT *
  FROM PA0001
  INTO TABLE IT_PA0001
  WHERE PERNR IN P_PERNR AND
        BUKRS IN P_BUKRS AND
        WERKS IN P_WERKS AND
        KOSTL IN P_KOSTL AND
        ORGEH IN P_UNIOR AND
        PLANS NE '99999999' AND",  mas tem que ser <> 99999999 caso o parâmetro venha vazio
        STELL IN P_CARGO AND
        BEGDA <= V_BEGDA AND
        ENDDA >= V_ENDDA.
  ELSE.
    SELECT *
      FROM PA0001
      INTO TABLE IT_PA0001
      WHERE PERNR IN P_PERNR AND
            BUKRS IN P_BUKRS AND
            WERKS IN P_WERKS AND
            KOSTL IN P_KOSTL AND
            ORGEH IN P_UNIOR AND
            PLANS IN P_POSI AND",  mas tem que ser <> 99999999 caso o parâmetro venha vazio
            STELL IN P_CARGO AND
            BEGDA <= V_BEGDA AND
            ENDDA >= V_ENDDA.
  ENDIF.



  SELECT *
  FROM  PA0000
  INTO TABLE IT_pa0000
  WHERE STAT2 IN P_STATUS AND
        BEGDA <= V_BEGDA AND
        ENDDA >= V_ENDDA.

  CHECK IT_PA0001 IS NOT INITIAL.

  SELECT PERNR CNAME
  FROM  PA0002
  INTO TABLE IT_pa0002
  FOR ALL ENTRIES IN IT_PA0001
  WHERE PERNR EQ IT_PA0001-PERNR AND
        ENDDA >= SY-DATUM.

  SELECT BUKRS BUTXT
  FROM  T001
  INTO TABLE IT_t001
  FOR ALL ENTRIES IN IT_PA0001
  WHERE BUKRS EQ IT_PA0001-BUKRS.

  SELECT WERKS NAME1
  FROM   T001W
  INTO TABLE IT_t001W
  FOR ALL ENTRIES IN IT_PA0001
  WHERE WERKS EQ IT_PA0001-WERKS.


  SELECT KOSTL KOKRS KTEXT
   FROM   CSKT
   INTO TABLE IT_cskt
   FOR ALL ENTRIES IN IT_PA0001
   WHERE SPRAS = SY-LANGU AND
         KOSTL = IT_PA0001-KOSTL AND
         KOKRS = IT_PA0001-KOKRS AND
         DATBI >= SY-DATUM.


  SELECT OBJID STEXT
  FROM   HRP1000
  INTO TABLE IT_UNID_ORG
  FOR ALL ENTRIES IN IT_PA0001
  WHERE PLVAR = '01' AND
         OTYPE = 'O' AND
         OBJID = IT_PA0001-ORGEH AND
         LANGU = SY-LANGU AND
         BEGDA <= V_BEGDA AND
         ENDDA >= V_ENDDA.


  SELECT OBJID STEXT
  FROM   HRP1000
  INTO TABLE IT_DESC_CARGO
  FOR ALL ENTRIES IN IT_PA0001
  WHERE PLVAR = '01' AND
        OTYPE = 'C' AND
        OBJID = IT_PA0001-STELL AND
        LANGU = SY-LANGU AND
        BEGDA <= V_BEGDA AND
        ENDDA >= V_ENDDA.

  SELECT *
  FROM   T7BRB2
  INTO TABLE IT_t7brb2
  FOR ALL ENTRIES IN IT_PA0001
  WHERE BUKRS = IT_PA0001-BUKRS  AND
        FILIA = IT_PA0001-WERKS  AND
        BEGDA <= V_BEGDA AND
        ENDDA >= V_ENDDA.


  SELECT *
  FROM   PA0398
  INTO TABLE IT_pa0398
  FOR ALL ENTRIES IN IT_PA0001
  WHERE PERNR = IT_PA0001-PERNR AND
        BEGDA <= V_BEGDA AND
        ENDDA >= V_ENDDA.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FM_DADOS_PROCESSA .

  LOOP AT IT_PA0001[] INTO WA_PA0001.
    WA_SAIDA-MATRICULA = WA_PA0001-PERNR.
    WA_SAIDA-EMPRESA = WA_PA0001-BUKRS.
    WA_SAIDA-FILIAL =  WA_PA0001-WERKS.
    WA_SAIDA-CENTRO_CUSTO = WA_pa0001-KOSTL.
    WA_SAIDA-UNID_ORG = WA_pa0001-ORGEH.
    WA_SAIDA-CARGO = WA_pa0001-STELL.

    READ TABLE IT_PA0002 INTO WA_PA0002 WITH KEY PERNR = WA_PA0001-PERNR.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-NOME_EMP =  WA_PA0002-CNAME.
    ENDIF.

    READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = WA_PA0001-BUKRS.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-DESC_EMPRESA =  WA_T001-BUTXT.
    ENDIF.

    READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_PA0001-WERKS.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-DESC_FILIAL =  WA_T001W-NAME1.
    ENDIF.

    READ TABLE IT_CSKT INTO WA_cskt WITH KEY  KOSTL = WA_PA0001-KOSTL
                                              KOKRS = WA_PA0001-KOKRS.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-DESC_CCUSTO =  WA_cskt-KTEXT.
    ENDIF.

    READ TABLE IT_UNID_ORG INTO WA_UNID_ORG WITH KEY OBJID = WA_PA0001-ORGEH.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-DESC_UORG =  WA_UNID_ORG-STEXT.
    ENDIF.

    READ TABLE IT_DESC_CARGO INTO WA_DESC_CARGO WITH KEY OBJID = WA_PA0001-STELL.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-DESC_CARGO =  WA_DESC_CARGO-STEXT.
    ENDIF.

    CALL FUNCTION 'HR_ENTRY_DATE'
      EXPORTING
        PERSNR               = WA_PA0001-PERNR
*       RCLAS                =
        BEGDA                = '18000101'
        ENDDA                = '99991231'
*       VARKY                =
        INITIALIZE_PS_BUFFER = 'X'
      IMPORTING
        ENTRYDATE            = V_DT_ADMISSAO.

    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.


    CALL FUNCTION 'RP_GET_FIRE_DATE'
      EXPORTING
        PERSNR   = WA_PA0001-PERNR
        STATUS2  = '0'
      IMPORTING
        FIREDATE = V_DT_DESLIGAMENTO.


    WA_SAIDA-DT_ADM = V_DT_ADMISSAO.
    WA_SAIDA-DT_DESL = V_DT_DESLIGAMENTO.

**======================================================================*
**** CLUSTER FOLHA
**======================================================================*
* Objeto
    CLEAR: CL_READ_PAYROLL.
    CREATE OBJECT CL_READ_PAYROLL
      EXPORTING
        IV_PERNR = wa_PA0001-PERNR.

* Lista de Clusters
    REFRESH: TL_RGDIR.
    CALL METHOD CL_READ_PAYROLL->GET_RGDIR
      EXPORTING
        IV_BEGDA = V_ENDDA
        IV_ENDDA = V_begda
      IMPORTING
        ET_RGDIR = TL_RGDIR.

    CLEAR: WL_RGDIR.

    DELETE TL_RGDIR WHERE OCRSN NOT IN RG_OCRSN.

    REFRESH TL_PAYROLL.
    CALL METHOD CL_READ_PAYROLL->GET_PAY_RESULT_TABLE
      EXPORTING
        IT_RGDIR        = TL_RGDIR
      IMPORTING
        ET_PAYBR_RESULT = TL_PAYROLL.

    REFRESH: TL_RT.

    CLEAR: WL_PAYROLL.
    LOOP AT TL_PAYROLL INTO WL_PAYROLL.
      APPEND LINES OF WL_PAYROLL-INTER-RT TO TL_RT.
    ENDLOOP.
    LOOP AT TL_RT INTO WL_RT.
      IF WL_RT-LGART EQ '/121'.
        WA_SAIDA-BASE_INSS    =   WA_SAIDA-BASE_INSS     +  WL_RT-BETRG.
      ENDIF.

      IF WL_RT-LGART EQ '/123'.
        WA_SAIDA-BASE_INSS_13    =   WA_SAIDA-BASE_INSS_13     +  WL_RT-BETRG.
      ENDIF.

      IF WL_RT-LGART EQ '/T85' OR WL_RT-LGART EQ '/T80' OR WL_RT-LGART EQ '/T90'.
        WA_SAIDA-DEDU_MATERNIDADE    =   WA_SAIDA-DEDU_MATERNIDADE     +  WL_RT-BETRG.
      ENDIF.

      IF WL_RT-LGART EQ '/353'.
        WA_SAIDA-DEDU_MATERNIDADE_13    =   WA_SAIDA-DEDU_MATERNIDADE_13     +  WL_RT-BETRG.
      ENDIF.

      IF WL_RT-LGART EQ '/117'.
        WA_SAIDA-DEDU_FALTAS    =   WA_SAIDA-DEDU_FALTAS     +  WL_RT-BETRG.
      ENDIF.

      IF WL_RT-LGART EQ '/125'.
        WA_SAIDA-LIMINAR_AMAGGI   =   WA_SAIDA-LIMINAR_AMAGGI     +  WL_RT-BETRG.
      ENDIF.

      IF WL_RT-LGART EQ '/125'.
        WA_SAIDA-LIMINAR_AMAGGI   =   WA_SAIDA-LIMINAR_AMAGGI     +  WL_RT-BETRG.
      ENDIF.

      IF WL_RT-LGART EQ '/128'.
        WA_SAIDA-LIMINAR_HERMASA   =   WA_SAIDA-LIMINAR_HERMASA     +  WL_RT-BETRG.
      ENDIF.

      IF WL_RT-LGART EQ '/314' OR  WL_RT-LGART EQ '/303'.
        WA_SAIDA-INSS_EMPREGADO_314   =   WA_SAIDA-INSS_EMPREGADO_314     +  WL_RT-BETRG.
      ENDIF.

      IF WL_RT-LGART EQ '/320' OR  WL_RT-LGART EQ '1102'.
        WA_SAIDA-DEDU_SAL_FAMILIA   =   WA_SAIDA-DEDU_SAL_FAMILIA     +  WL_RT-BETRG.
      ENDIF.
    ENDLOOP.

    READ TABLE IT_PA0398 INTO WA_PA0398 WITH KEY PERNR = WA_pa0001-PERNR.
    IF SY-SUBRC IS INITIAL.
      IF WA_PA0398-EMPID = '06' .
        WA_SAIDA-CAT_EMPREGADO = 'Acionista'.
      ELSEIF WA_PA0398-AGNOC = '04'.
        WA_SAIDA-CAT_EMPREGADO = 'Aposentado especial'.
      ELSE.
        WA_SAIDA-CAT_EMPREGADO = 'Empregado'.
      ENDIF.
    ENDIF.

    READ TABLE IT_T7BRB2 INTO WA_T7BRB2 WITH KEY BUKRS = WA_pa0001-BUKRS
                                                 FILIA = WA_pa0001-WERKS .
    IF SY-SUBRC IS INITIAL.
      IF WA_T7BRB2-PCONT IS NOT INITIAL.
        IF WA_SAIDA-CAT_EMPREGADO = 'Acionista'.
          WA_SAIDA-PERC_EMPRESA = 11.
        ELSEIF WA_SAIDA-CAT_EMPREGADO = 'Empregado'.
          WA_SAIDA-PERC_EMPRESA = WA_T7BRB2-PCONT.
        ELSE.
          WA_SAIDA-PERC_EMPRESA = 26.
        ENDIF.
      ENDIF.

        WA_SAIDA-PERC_RAT = WA_T7BRB2-PSAT.
        WA_SAIDA-PERC_FAP = WA_T7BRB2-PFAP.
        WA_SAIDA-PERC_TERCEIROS = WA_T7BRB2-THDRT.
    ENDIF.

    WA_SAIDA-RESULT_BASE_INSS = WA_SAIDA-BASE_INSS -  WA_SAIDA-DEDU_MATERNIDADE -  WA_SAIDA-DEDU_FALTAS -  WA_SAIDA-LIMINAR_AMAGGI -  WA_SAIDA-LIMINAR_HERMASA.
    WA_SAIDA-RESULT_BASE_INSS_13 =  WA_SAIDA-BASE_INSS_13 -  WA_SAIDA-DEDU_MATERNIDADE_13.
    WA_SAIDA-INSS_EMPREGADOR = ( WA_SAIDA-RESULT_BASE_INSS + WA_SAIDA-RESULT_BASE_INSS_13 ) *  ( WA_SAIDA-PERC_EMPRESA / 100 ).
    WA_SAIDA-PERC_RAT_AJUSTADO = WA_SAIDA-PERC_RAT * WA_SAIDA-PERC_FAP  / 100.
    WA_SAIDA-CONTRIB_RAT = ( WA_SAIDA-RESULT_BASE_INSS + WA_SAIDA-RESULT_BASE_INSS_13 ) *      WA_SAIDA-PERC_RAT_AJUSTADO.
    WA_SAIDA-CONT_TERCEIROS = ( WA_SAIDA-RESULT_BASE_INSS + WA_SAIDA-RESULT_BASE_INSS_13 ) * ( WA_SAIDA-PERC_TERCEIROS / 100 ).
    WA_SAIDA-TOTAL_INSS_EMPRESA = WA_SAIDA-INSS_EMPREGADOR  + WA_SAIDA-CONTRIB_RAT + WA_SAIDA-CONT_TERCEIROS.
    WA_SAIDA-TOTAL_INSS_GERAL = WA_SAIDA-INSS_EMPREGADO_314 + WA_SAIDA-TOTAL_INSS_EMPRESA.


    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR WA_SAIDA.
  ENDLOOP.
  PERFORM  FIELDCATALOG.
ENDFORM.
FORM  FIELDCATALOG.
  IF IT_SAIDA IS NOT INITIAL.

    CLEAR: IT_FIELDCATALOG[].

    PERFORM ALV_PREENCHE_CAT USING:


      'pa0001'        'PERNR'      'MATRICULA'             'Matricula'                                 '09'     ''      '' '' '',
      'pa0002'        'CNAME'      'NOME_EMP'              'Nome Empregado'                            '30'     ''      '' '' '',
      'pa0001'        'BUKRS'      'EMPRESA'               'Empresa'                                   '04'     ''      '' '' '',
      't001'          'BUTXT'      'DESC_EMPRESA'          'Descrição Empresa'                         '30'     ''      '' '' '',
      'pa0001'        'WERKS'      'FILIAL'                'Filial'                                    '12'     ''      '' '' '',
      't001w'         'NAME1'      'DESC_FILIAL'           'Desc. Filial'                              '40'     ''      '' '' '',
      'pa0001'        'KOSTL'      'CENTRO_CUSTO'          'Centro de Custo'                           '10'     ''      '' '' '',
      'cskt'          'LTEXT'      'DESC_CCUSTO'           'Desc. Centro Custo'                        '10'     ''      '' '' '',
      'pa0001'        'ORGEH'      'UNID_ORG'              'Unid Org'                                  '30'     ''      '' '' '',
      'hrp1000'       'STEXT'      'DESC_UORG'             'Desc. UnidOrg'                             '07'     ''      '' '' '',
      'pa0001'        'STELL'      'CARGO'                 'Cargo'                                     '20'     ''      '' '' '',
      'hrp1000'       'STEXT'      'DESC_CARGO'            'Desc. Cargo'                               '20'     ''      '' '' '',
      ''              ''           'CAT_EMPREGADO'         'Categ. Empregado'                          '20'     ''      '' '' '',
      ''              'BEGDA'      'DT_ADM'                'Dt Admissão'                               '10'     ''      '' '' '',
      ''              'ENDDA'      'DT_DESL'               'Dt Desligamento'                           '10'     ''      '' '' '',
      ''              'BETRG'      'BASE_INSS '            'Base INSS (/121)'                          '10'     ''      '' '' '',
      ''              'BET01'      'BASE_INSS_13'          'Base INSS 13º (/123)'                      '04'     ''      '' '' '',
      'PA0014'        'BETRG'      'DEDU_MATERNIDADE'      'Deduç. Maternidade (/T85, /T80, /T90)'     '14'     ''      '' '' '',
      'HRP9666'       'ZPERC'      'DEDU_MATERNIDADE_13'   'Deduç. Maternidade 13º(/353)'              '20'     ''      '' '' '',
      'P0008'         'BET01'      'DEDU_FALTAS'           'Dedução Faltas (/117)'                     '20'     ''      '' '' '',
      ''              ''           'LIMINAR_AMAGGI'        'Liminar Amaggi (/125)'                     '40'     ''      '' '' '',
      'P0008'         'BET01'      'LIMINAR_HERMASA'       'Liminar Hermasa (/128)'                    '40'     ''      '' '' '',
      'P0008'         'BET01'      'RESULT_BASE_INSS'      'Resultado base INSS'                       '20'     ''      '' '' '',
      'P0008'         'BET01'      'RESULT_BASE_INSS_13'   'Resultado base INSS 13º'                   '20'     ''      '' '' '',
      'P0008'         'BET01'      'INSS_EMPREGADO_314'    'INSS Empregado (/314, /303)'               '20'     ''      '' '' '',
      'P0008'         'BET01'      'INSS_EMPREGADOR'       'INSS Empregador'                           '20'     ''      '' '' '',
      'P0008'         'BET01'      'PERC_EMPRESA'          '% Empresa'                                 '20'     ''      '' '' '',
      'P0008'         'BET01'      'CONTRIB_RAT'           'Contribuição RAT'                          '20'     ''      '' '' '',
      'P0008'         'BET01'      'PERC_RAT_AJUSTADO'     '% RAT AJUSTADO'                            '20'     ''      '' '' '',
      'P0008'         'BET01'      'PERC_RAT'              '% RAT'                                     '20'     ''      '' '' '',
      'P0008'         'BET01'      'PERC_FAP'              '% FAP'                                     '20'     ''      '' '' '',
      'ZHCMT_BN_0001' 'VA_P_MVRA'  'CONT_TERCEIROS'        'Cont. Terceiros'                           '16'     ''      '' '' '',
      'ZHCMT_BN_0001' 'VR_P_MVR0'  'PERC_TERCEIROS'        '% Terceiros'                               '20'     ''      '' '' '',
      'T5UCA'         'LTEXT'      'DEDU_SAL_FAMILIA'      'Deduç. Sal. Família (/320, 1102)'          '11'     ''      '' '' '',
      'PA0167'        'DEPCV'      'TOTAL_INSS_EMPRESA'    'Total INSS Empresa'                        '17'     ''      '' '' '',
      'Q0167'         'EECST'      'TOTAL_INSS_GERAL'      'Total INSS Geral'                          '20'     ''      '' '' ''.

    PERFORM SAIDA_TELA.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING    VALUE(P_REF_TABLE)
                                VALUE(P_REF_FIELD)
                                VALUE(P_CAMPO)
                                VALUE(P_DESC)
                                VALUE(P_TAM)
                                VALUE(P_ZERO)
                                VALUE(P_HOT)
                                VALUE(P_SUM)
                                VALUE(P_JUST).

  WA_FIELDCATALOG-FIELDNAME  = P_CAMPO.
  WA_FIELDCATALOG-COLTEXT    = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_L  = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_M  = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_S  = P_DESC.

  WA_FIELDCATALOG-OUTPUTLEN  = P_TAM.
  WA_FIELDCATALOG-HOTSPOT    = P_HOT.
  WA_FIELDCATALOG-NO_ZERO    = P_ZERO.
  WA_FIELDCATALOG-DO_SUM     = P_SUM.
  WA_FIELDCATALOG-JUST       = 'C'."P_JUST.

  IF WA_FIELDCATALOG-FIELDNAME = 'TOTAL_SB'  OR
     WA_FIELDCATALOG-FIELDNAME = '80_FX'     OR
     WA_FIELDCATALOG-FIELDNAME = '90_FX'     OR
     WA_FIELDCATALOG-FIELDNAME = '100_FX'    OR
     WA_FIELDCATALOG-FIELDNAME = '110_FX'    OR
     WA_FIELDCATALOG-FIELDNAME = '120_FX'    OR
     WA_FIELDCATALOG-FIELDNAME = 'STATUS_FX' OR
     WA_FIELDCATALOG-FIELDNAME = 'MOEDAS'.
    WA_FIELDCATALOG-COL_OPT    = ' '.
  ELSE.
    WA_FIELDCATALOG-COL_OPT    = 'X'.
  ENDIF.

  WA_FIELDCATALOG-REF_FIELD = P_REF_FIELD.
  WA_FIELDCATALOG-REF_TABLE = P_REF_TABLE.


  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.
  CLEAR: WA_FIELDCATALOG.


ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA: URL(255)                TYPE C,
        DATA_INI(10)            TYPE C,
        P_TEXT                  TYPE SDYDO_TEXT_ELEMENT,
        SDYDO_TEXT_ELEMENT(255),
        P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TL0100'.
  IF G_CUSTOM_CONTAINER IS INITIAL.
* create a container for the tree control
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV.

    CREATE OBJECT DG_SPLITTER_2
      EXPORTING
        PARENT  = DG_PARENT_1
        ROWS    = 1
        COLUMNS = 2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 2
      RECEIVING
        CONTAINER = DG_PARENT_2A.

    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 16.

    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
      EXPORTING
        ID    = 1
        WIDTH = 40.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = DG_PARENT_2A.

    PERFORM F_PEGA_IMAGEM USING 'LOGO_NOVO' CHANGING URL.

    CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
      EXPORTING
        URL = URL.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_FIT_CENTER.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.

    GS_LAYOUT-SEL_MODE   = 'A'.
    CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_SAIDA[].

    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.

    CALL METHOD DG_DYNDOC_ID->INITIALIZE_DOCUMENT.

    CALL METHOD DG_DYNDOC_ID->ADD_TABLE
      EXPORTING
        NO_OF_COLUMNS = 1
        BORDER        = '0'
        WIDTH         = '100%'
      IMPORTING
        TABLE         = TABLE_ELEMENT.

    CALL METHOD TABLE_ELEMENT->ADD_COLUMN
      IMPORTING
        COLUMN = COLUMN.

    CALL METHOD TABLE_ELEMENT->SET_COLUMN_STYLE
      EXPORTING
        COL_NO    = 1
        SAP_ALIGN = 'CENTER'
        SAP_STYLE = CL_DD_DOCUMENT=>HEADING.

    P_TEXT = TEXT-002.

    CALL METHOD COLUMN->ADD_TEXT
      EXPORTING
        TEXT      = P_TEXT
        SAP_STYLE = 'HEADING'.

    CALL METHOD DG_DYNDOC_ID->ADD_TABLE
      EXPORTING
        NO_OF_COLUMNS = 2
        BORDER        = '0'
        WIDTH         = '100%'
      IMPORTING
        TABLE         = TABLE_ELEMENT2.

    CALL METHOD TABLE_ELEMENT2->ADD_COLUMN
      EXPORTING
        SAP_STYLE   = 'SAP_BOLD'
        STYLE_CLASS = 'SAP_BOLD'
      IMPORTING
        COLUMN      = COLUMN_1.

    CLEAR: P_TEXT_TABLE.
    "SDYDO_TEXT_ELEMENT = 'Escalas: '.
    "APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

*    sdydo_text_element = 'Resultado Despesas INSS Folha de Pagamento'.
*    APPEND sdydo_text_element TO p_text_table.
*
*    CALL METHOD column_1->add_text
*      EXPORTING
*        text_table = p_text_table
*        fix_lines  = 'X'.
*
*    CALL METHOD table_element2->add_column
*      IMPORTING
*        column = column_2.
*
*    CALL METHOD table_element2->set_column_style
*      EXPORTING
*        col_no       = 2
*        sap_align    = 'LEFT'
*        sap_fontsize = cl_dd_document=>medium.
*
*    CLEAR: p_text_table.

    CONCATENATE  SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM(4)  INTO DATA_INI.
    "CONCATENATE  P_BEGDA-HIGH+6(2) '.' P_BEGDA-HIGH+4(2) '.' P_BEGDA-HIGH(4) INTO DATA_FIM.


    CONCATENATE 'Data de Processamento:' DATA_INI INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

    CALL METHOD COLUMN_1->ADD_TEXT
      EXPORTING
        TEXT_TABLE = P_TEXT_TABLE
        FIX_LINES  = 'X'.

    CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT.

    CREATE OBJECT DG_HTML_CNTRL
      EXPORTING
        PARENT = DG_PARENT_2.

    DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

    CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = DG_PARENT_2
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.

  ENDIF.

  CALL METHOD CTL_ALV->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL
      ES_ROW_NO   = GS_SCROLL_ROW.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT .

  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '0100'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0190   text
*      <--P_URL  text
*----------------------------------------------------------------------*
FORM F_PEGA_IMAGEM  USING    NOME_LOGO
                    CHANGING URL.

  DATA: BEGIN OF GRAPHIC_TABLE OCCURS 0,
          LINE(255) TYPE X,
        END OF GRAPHIC_TABLE.
  DATA: L_GRAPHIC_XSTR TYPE XSTRING.
  DATA: GRAPHIC_SIZE   TYPE I.
  DATA: L_GRAPHIC_CONV TYPE I.
  DATA: L_GRAPHIC_OFFS TYPE I.

  REFRESH GRAPHIC_TABLE.
  CALL METHOD CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP
    EXPORTING
      P_OBJECT = 'GRAPHICS'
      P_NAME   = NOME_LOGO
      P_ID     = 'BMAP'
      P_BTYPE  = 'BCOL'
    RECEIVING
      P_BMP    = L_GRAPHIC_XSTR.

  GRAPHIC_SIZE = XSTRLEN( L_GRAPHIC_XSTR ).
  L_GRAPHIC_CONV = GRAPHIC_SIZE.
  L_GRAPHIC_OFFS = 0.
  WHILE L_GRAPHIC_CONV > 255.
    GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(255).
    APPEND GRAPHIC_TABLE.
    L_GRAPHIC_OFFS = L_GRAPHIC_OFFS + 255.
    L_GRAPHIC_CONV = L_GRAPHIC_CONV - 255.
  ENDWHILE.
  GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(L_GRAPHIC_CONV).
  APPEND GRAPHIC_TABLE.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE     = 'IMAGE'
      SUBTYPE  = 'X-UNKNOWN'
      SIZE     = GRAPHIC_SIZE
      LIFETIME = 'T'
    TABLES
      DATA     = GRAPHIC_TABLE
    CHANGING
      URL      = URL.

ENDFORM.                    " F_PEGA_IMAGEM

FORM SAIDA_TELA .
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
