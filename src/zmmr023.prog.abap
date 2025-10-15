*----------------------------------------------------------------------*
* Report  ZMMR023                                                      *
* Descrição  : Comprovante de Retirada de Materiais do Estoque         *
* Módulo     : MM SD                            Transação:             *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Camila Brand                            Data: 06/05/2011*
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*

REPORT  ZMMR023.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON, ZMMR.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  RESB,     " Reserva/necessidade dependente
  MSEG,     " Segmento de documento - material
  MKPF,     " Cabeçalho do documento do material
  ZMMT0003, " Tabela de Aprovadores X C.Custo / Almoxarifes - Reservas
  RKPF,     " Cabeçalho do documento da reserva
  T001,     " Empresas
  T001W,    " Centros/filiais
  USREFUS,  " Usuário de referência para aplicações de Internet
  MAKT,     " Textos breves de material
  MARD,     " Dados de depósito para material
  T156T,    " Tipo movimento  Texto
  CSKT,     " Registro mestre de centros de custo
  SKAT.     " Mestre de contas do Razão (plano de contas: denominação)

*----------------------------------------------------------------------*
* ESTRUTURA
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF TY_RESB,
    RSNUM TYPE RESB-RSNUM, " Nº reserva
    RSPOS TYPE RESB-RSPOS, " Nº item da reserva / das necessidades dependentes
    XWAOK TYPE RESB-XWAOK, " Movimento de mercadoria para reserva é permitido
    KZEAR TYPE RESB-KZEAR, " Registro final da reserva
    WERKS TYPE RESB-WERKS, " Centro
    BDTER TYPE RESB-BDTER, " Data necessidade do componente
    BDMNG TYPE RESB-BDMNG, " Quantidade necessária
    ENMNG TYPE RESB-ENMNG,
    BWART TYPE RESB-BWART, " Tipo de movimento (administração de estoques)
    SAKNR TYPE RESB-SAKNR, " Nº conta do Razão
    MATNR TYPE RESB-MATNR, " Nº do material
    SGTXT TYPE RESB-SGTXT, " Texto do item
    KOSTL TYPE RKPF-KOSTL,
    LGORT TYPE RESB-LGORT,
  END OF TY_RESB,

  BEGIN OF TY_T156T,
    BWART TYPE T156T-BWART, " Tipo de movimento (administração de estoques)
    BTEXT TYPE T156T-BTEXT, " Texto para tipo de movimento (gestão de estoques)
    SPRAS TYPE T156T-SPRAS, " Código de idioma
    SOBKZ TYPE T156T-SOBKZ, " Código de estoque especial
  END OF TY_T156T,

  BEGIN OF TY_MSEG,
    MJAHR      TYPE  MSEG-MJAHR, " Ano do documento do material
    RSNUM      TYPE  MSEG-RSNUM, " Nº reserva / necessidades dependentes
    KOSTL      TYPE  MSEG-KOSTL, " Centro de custo
    AUFNR      TYPE  MSEG-AUFNR, " Nº ordem
    ZEILE      TYPE  MSEG-ZEILE, " Item no documento do material
    MATNR      TYPE  MSEG-MATNR, " Nº do material
    ERFME      TYPE  MSEG-ERFME, " Unidade de medida do registro
    LGORT      TYPE  MSEG-LGORT, " Depósito
    MENGE      TYPE  MSEG-MENGE, " Quantidade
    SGTXT      TYPE  MSEG-SGTXT, " Texto do item
    MBLNR      TYPE  MSEG-MBLNR, " Nº documento de material
    WERKS      TYPE  MSEG-WERKS, " Centro
    WEMPF      TYPE  MSEG-WEMPF, " Recebedor da mercadoria
    RSPOS      TYPE  MSEG-RSPOS, " Nº item da reserva
    LINE_ID    TYPE  MSEG-LINE_ID, " Nº item da reserva
    BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
  END OF TY_MSEG,

  BEGIN OF TY_MSEG_AUX,
    MJAHR      TYPE  MSEG-MJAHR, " Ano do documento do material
    RSNUM      TYPE  MSEG-RSNUM, " Nº reserva / necessidades dependentes
    KOSTL      TYPE  MSEG-KOSTL, " Centro de custo
    AUFNR      TYPE  MSEG-AUFNR, " Nº ordem
    ZEILE      TYPE  MSEG-ZEILE, " Item no documento do material
    MATNR      TYPE  MSEG-MATNR, " Nº do material
    ERFME      TYPE  MSEG-ERFME, " Unidade de medida do registro
    LGORT      TYPE  MSEG-LGORT, " Depósito
    MENGE      TYPE  MSEG-MENGE, " Quantidade
    SGTXT      TYPE  MSEG-SGTXT, " Texto do item
    MBLNR      TYPE  MSEG-MBLNR, " Nº documento de material
    WERKS      TYPE  MSEG-WERKS, " Centro
    WEMPF      TYPE  MSEG-WEMPF, " Recebedor da mercadoria
    RSPOS      TYPE  MSEG-RSPOS, " Nº item da reserva
    LINE_ID    TYPE  MSEG-LINE_ID, " Nº item da reserva
    BUDAT_MKPF TYPE  MSEG-BUDAT_MKPF, " Nº item da reserva
  END OF TY_MSEG_AUX,

  BEGIN OF TY_MSEG_AUX2,
    MJAHR      TYPE  MSEG-MJAHR, " Ano do documento do material
    RSNUM      TYPE  MSEG-RSNUM, " Nº reserva / necessidades dependentes
    KOSTL      TYPE  MSEG-KOSTL, " Centro de custo
    AUFNR      TYPE  MSEG-AUFNR, " Nº ordem
    ZEILE      TYPE  MSEG-ZEILE, " Item no documento do material
    MATNR      TYPE  MSEG-MATNR, " Nº do material
    ERFME      TYPE  MSEG-ERFME, " Unidade de medida do registro
    LGORT      TYPE  MSEG-LGORT, " Depósito
    MENGE      TYPE  MSEG-MENGE, " Quantidade
    SGTXT      TYPE  MSEG-SGTXT, " Texto do item
    MBLNR      TYPE  MSEG-MBLNR, " Nº documento de material
    WERKS      TYPE  MSEG-WERKS, " Centro
    SMBLN      TYPE  MSEG-SMBLN, " Nº documento de material
    WEMPF      TYPE  MSEG-WEMPF, " Recebedor da mercadoria
    RSPOS      TYPE  MSEG-RSPOS, " Nº item da reserva
    LINE_ID    TYPE  MSEG-LINE_ID, " Nº item da reserva
    BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
  END OF TY_MSEG_AUX2,

  BEGIN OF TY_MSEG_DIF,
    RSNUM TYPE  MSEG-RSNUM, " Nº reserva / necessidades dependentes
    MBLNR TYPE  MSEG-MBLNR, " Nº documento de material
    MENGE TYPE  MSEG-MENGE, " Quantidade
  END OF TY_MSEG_DIF,

  BEGIN OF TY_MKPF,
    MBLNR TYPE  MKPF-MBLNR, " Nº documento de material
    USNAM TYPE  MKPF-USNAM, " Nome do usuário
    CPUDT TYPE  MKPF-CPUDT, " Data da entrada do documento contábil
    CPUTM TYPE  MKPF-CPUTM, " Hora da entrada
    BKTXT TYPE  MKPF-BKTXT, " Texto de cabeçalho de documento
  END OF TY_MKPF,

  BEGIN OF TY_ZMMT0003,
    KOSTL TYPE ZMMT0003-KOSTL, " Centro de custo
    UNAME TYPE ZMMT0003-UNAME, " Nome do usuário
  END OF TY_ZMMT0003,

  BEGIN OF TY_RKPF,
    RSNUM TYPE RKPF-RSNUM, " Nº reserva
    PARBU TYPE RKPF-PARBU, " Empresa de compensação
    USNAM TYPE RKPF-USNAM, " Nome do usuário
    RSDAT TYPE RKPF-RSDAT, " Data base da reserva
  END OF TY_RKPF,

  BEGIN OF TY_T001,
    BUKRS TYPE T001-BUKRS, " Empresa
    BUTXT TYPE T001-BUTXT, " Denominação da firma ou empresa
  END OF TY_T001,

  BEGIN OF TY_T001W,
    WERKS TYPE T001W-WERKS, " Centro
    NAME1 TYPE T001W-NAME1, " Nome
  END OF TY_T001W,

  BEGIN OF TY_USREFUS,
    BNAME     TYPE USREFUS-BNAME,     " Nome do usuário no mestre de usuários
    USERALIAS TYPE USREFUS-USERALIAS, " Alias para usuário da Internet
  END OF TY_USREFUS,

  BEGIN OF TY_USREFUSBX,
    BNAME     TYPE USREFUS-BNAME,     " Nome do usuário no mestre de usuários
    USERALIAS TYPE USREFUS-USERALIAS, " Alias para usuário da Internet
  END OF TY_USREFUSBX,

  BEGIN OF TY_USREFUSCR,
    BNAME     TYPE USREFUS-BNAME,     " Nome do usuário no mestre de usuários
    USERALIAS TYPE USREFUS-USERALIAS, " Alias para usuário da Internet
  END OF TY_USREFUSCR,

  BEGIN OF TY_MAKT,
    MATNR TYPE MAKT-MATNR, " Nº do material
    MAKTX TYPE MAKT-MAKTX, " Texto breve de material
  END OF TY_MAKT,

  BEGIN OF TY_MARD,
    MATNR TYPE MARD-MATNR, " Nº do material
    WERKS TYPE MARD-WERKS, " Centro
    LGORT TYPE MARD-LGORT, " Depósito
    LGPBE TYPE MARD-LGPBE, " Posição no depósito
  END OF TY_MARD,

  BEGIN OF TY_CSKT,
    KOSTL TYPE CSKT-KOSTL, " Centro de custo
    KOKRS TYPE CSKT-KOKRS, " Área de contabilidade de custos
    LTEXT TYPE CSKT-LTEXT, " Descrição
  END OF TY_CSKT,

  BEGIN OF TY_SKAT,
    SAKNR TYPE SKAT-SAKNR, " Nº conta do Razão
    SPRAS TYPE SKAT-SPRAS, " Código de idioma
    KTOPL TYPE SKAT-KTOPL, " Plano de contas
    TXT50 TYPE SKAT-TXT50, " Texto das contas do Razão
  END OF TY_SKAT,

  BEGIN OF TY_ZMMT0009,
    RSNUM        TYPE ZMMT0009-RSNUM,        " Nº reserva
    RSPOS        TYPE ZMMT0009-RSPOS,        " Nº item da reserva
    KOSTL        TYPE ZMMT0009-KOSTL,        " Centro de custo
    UNAME        TYPE ZMMT0009-UNAME,        " Nome do usuário
    DT_APROVACAO TYPE ZMMT0009-DT_APROVACAO, " Data de criação do registro
    HR_APROVACAO TYPE ZMMT0009-HR_APROVACAO, " Hora do registro
  END OF TY_ZMMT0009.


*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA:
  T_BDC        TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
  IT_SAIDA     TYPE TABLE OF ZMMR_SAIDA,
  IT_SAIDA_CAB TYPE TABLE OF ZMMR_SAIDA,
  IT_RESB      TYPE TABLE OF TY_RESB,
  IT_MSEG      TYPE TABLE OF TY_MSEG,
  IT_MSEG_AUX  TYPE TABLE OF TY_MSEG_AUX,
  IT_MSEG_AUX2 TYPE TABLE OF TY_MSEG_AUX2,
  IT_MSEG_DIF  TYPE TABLE OF TY_MSEG_DIF,
  IT_MKPF      TYPE TABLE OF TY_MKPF,
  IT_ZMMT0003  TYPE TABLE OF TY_ZMMT0003,
  IT_RKPF      TYPE TABLE OF TY_RKPF,
  IT_T001      TYPE TABLE OF TY_T001,
  IT_T001W     TYPE TABLE OF TY_T001W,
  IT_USREFUS   TYPE TABLE OF TY_USREFUS,
  IT_USREFUSBX TYPE TABLE OF TY_USREFUSBX,
  IT_USREFUSCR TYPE TABLE OF TY_USREFUSCR,
  IT_MAKT      TYPE TABLE OF TY_MAKT,
  IT_MARD      TYPE TABLE OF TY_MARD,
  IT_T156T     TYPE TABLE OF TY_T156T,
  IT_CSKT      TYPE TABLE OF TY_CSKT,
  IT_SKAT      TYPE TABLE OF TY_SKAT,
  IT_ZMMT0009  TYPE TABLE OF TY_ZMMT0009.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  WA_CONT      TYPE REF TO CL_GUI_CUSTOM_CONTAINER        , " Objeto Container
  WA_ALV       TYPE REF TO CL_GUI_ALV_GRID                , " Objeto ALV
  WA_LAYOUT    TYPE LVC_S_LAYO                            , " Layout da Lista / Fim do DATA
  WA_SAIDA     TYPE ZMMR_SAIDA,
  WA_SAIDA_CAB TYPE ZMMR_SAIDA,
  WA_RESB      TYPE TY_RESB,
  WA_MSEG      TYPE TY_MSEG,
  WA_MSEG_AUX  TYPE TY_MSEG_AUX,
  WA_MSEG_AUX2 TYPE TY_MSEG_AUX2,
  WA_MSEG_DIF  TYPE TY_MSEG_DIF,
  WA_MKPF      TYPE TY_MKPF,
  WA_ZMMT0003  TYPE TY_ZMMT0003,
  WA_RKPF      TYPE TY_RKPF,
  WA_T001      TYPE TY_T001,
  WA_T001W     TYPE TY_T001W,
  WA_USREFUS   TYPE TY_USREFUS,
  WA_USREFUSBX TYPE TY_USREFUSBX,
  WA_USREFUSCR TYPE TY_USREFUSCR,
  WA_MAKT      TYPE TY_MAKT,
  WA_MARD      TYPE TY_MARD,
  WA_T156T     TYPE TY_T156T,
  WA_CSKT      TYPE TY_CSKT,
  WA_SKAT      TYPE TY_SKAT,
  WA_ZMMT0009  TYPE TY_ZMMT0009.



*----------------------------------------------------------------------*
* ESTRUTURA ALV  - Tabela Estrutura Colunas do Relatório
*----------------------------------------------------------------------*
DATA:
  IT_FCAT   TYPE TABLE OF LVC_S_FCAT,
  S_VARIANT TYPE DISVARIANT.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:  N_RSV   FOR MSEG-RSNUM NO INTERVALS  NO-EXTENSION,
                 P_DATR  FOR MKPF-CPUDT NO INTERVALS  NO-EXTENSION.


PARAMETERS : P_MBLNR TYPE MSEG-MBLNR.

PARAMETERS: R_ABER RADIOBUTTON GROUP RAD1,
            R_BAIX RADIOBUTTON GROUP RAD1 DEFAULT 'X'.

SELECTION-SCREEN: END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MBLNR.

  DATA: T_DYNPFIELDS  TYPE STANDARD TABLE OF DYNPREAD INITIAL SIZE 1 WITH HEADER LINE,
        LT_DYNPFIELDS LIKE STANDARD TABLE OF DYNPREAD INITIAL SIZE 1 WITH HEADER LINE,
        VG_RSNUM      TYPE RSNUM,
        AUX_NUM       TYPE C LENGTH 10.

  LT_DYNPFIELDS-FIELDNAME = 'N_RSV-LOW'.
  APPEND LT_DYNPFIELDS.

  LT_DYNPFIELDS-FIELDNAME = 'P_DATR-LOW'.
  APPEND LT_DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME             = SY-REPID
      DYNUMB             = SY-DYNNR
      TRANSLATE_TO_UPPER = 'X'
    TABLES
      DYNPFIELDS         = LT_DYNPFIELDS
    EXCEPTIONS
      OTHERS             = 0.

  READ TABLE LT_DYNPFIELDS INDEX 1.

  IF  LT_DYNPFIELDS-FIELDVALUE IS INITIAL .
    MESSAGE I000(Z01) WITH 'É Obrigatório informar o número da reserva!'.
    EXIT.
  ELSE.

    DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
          TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

    DATA: BEGIN OF T_FIELDTAB OCCURS 3.
            INCLUDE STRUCTURE DYNPREAD.
    DATA: END OF T_FIELDTAB.

    DATA: BEGIN OF TL_MATERIAL OCCURS 0,
            MBLNR TYPE MBLNR, " Nº documento de material
            CPUDT TYPE CPUDT, " Doc. Retirada.
            ZEILE TYPE MBLPO, " Item no documento do material
            MAKTG TYPE MAKTG, " Texto breve de material
          END OF TL_MATERIAL.

    DATA: WA_MATERIAL LIKE TL_MATERIAL.



    REFRESH: TL_MATERIAL, T_FIELDTAB.
    CLEAR:   TL_MATERIAL, T_FIELDTAB.

    WRITE LT_DYNPFIELDS-FIELDVALUE TO AUX_NUM.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = AUX_NUM
      IMPORTING
        OUTPUT = AUX_NUM.

    WRITE AUX_NUM TO VG_RSNUM.

    READ TABLE LT_DYNPFIELDS INDEX 2.
    IF NOT LT_DYNPFIELDS-FIELDVALUE IS INITIAL .
      CLEAR: P_DATR[].

      CALL FUNCTION 'CONVERSION_EXIT_PDATE_INPUT'
        EXPORTING
          INPUT  = LT_DYNPFIELDS-FIELDVALUE
        IMPORTING
          OUTPUT = P_DATR-LOW.

      P_DATR-SIGN   = 'I'.
      P_DATR-OPTION = 'EQ'.
      APPEND P_DATR.
    ELSE.
      CLEAR: P_DATR[].
    ENDIF.

    SELECT M~MBLNR S~CPUDT M~ZEILE T~MAKTG
      INTO CORRESPONDING FIELDS OF TABLE TL_MATERIAL
      FROM MSEG AS M
     INNER JOIN MAKT AS T ON T~MATNR EQ M~MATNR
     INNER JOIN MKPF AS S ON S~MBLNR EQ M~MBLNR
     WHERE M~RSNUM EQ VG_RSNUM
       AND M~RSPOS NE SPACE
       AND T~SPRAS EQ SY-LANGU
       AND S~CPUDT IN P_DATR.

    TL_DSELC-FLDNAME    = 'MBLNR'.
    TL_DSELC-DYFLDNAME = 'MSEG-MBLNR'.
    APPEND TL_DSELC.

    TL_DSELC-FLDNAME    = 'MJAHR'.
    TL_DSELC-DYFLDNAME = 'MSEG-MJAHR'.
    APPEND TL_DSELC.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD        = 'MBLNR'
        DYNPPROG        = SY-REPID                          "'ZFINR018'
        DYNPNR          = SY-DYNNR
        VALUE_ORG       = 'S'      "ynprofield = 'ZFIWRT0001-OPERACAO'
      TABLES
        VALUE_TAB       = TL_MATERIAL
        RETURN_TAB      = TL_RETURN_TAB
        DYNPFLD_MAPPING = TL_DSELC.

    READ TABLE TL_RETURN_TAB INDEX 1.
    READ TABLE TL_MATERIAL INTO WA_MATERIAL WITH KEY MBLNR = TL_RETURN_TAB-FIELDVAL.

    P_DATR-LOW = WA_MATERIAL-CPUDT.
    P_MBLNR    = WA_MATERIAL-MBLNR.

    IF P_DATR-LOW IS NOT INITIAL.

      MOVE: 'P_DATR-LOW'       TO T_DYNPFIELDS-FIELDNAME.
      WRITE: WA_MATERIAL-CPUDT TO T_DYNPFIELDS-FIELDVALUE.
      APPEND T_DYNPFIELDS.

    ENDIF.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME     = SY-REPID
        DYNUMB     = SY-DYNNR
      TABLES
        DYNPFIELDS = T_DYNPFIELDS.

  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: FORM_SELECIONA,            " Seleção de Dados - Formulário SD
           FORM_SAIDA,                " Saída SD
           FORM_IMPRIMIR.             " Impressão


END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FORM_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM FORM_SELECIONA.

  DATA: MBLNR1 TYPE   MSEG-MBLNR,
        ZEILE1 TYPE   MSEG-ZEILE,
        MBLNR2 TYPE   MSEG-MBLNR,
        ZEILE2 TYPE   MSEG-ZEILE,
        SMBLN2 TYPE   MSEG-SMBLN.


  " Busca reservas
  IF R_BAIX = 'X'.
    IF P_MBLNR IS INITIAL .
      MESSAGE I000(Z01) WITH 'Informe o Documento de Retirada !.'.
      STOP.
    ENDIF.

    SELECT RSNUM RSPOS XWAOK KZEAR WERKS BDTER BDMNG ENMNG BWART SAKNR MATNR SGTXT
    FROM RESB
    INTO TABLE IT_RESB
    WHERE RSNUM IN N_RSV
    AND XWAOK EQ 'X'
    AND ENMNG NE 0.

    IF IT_RESB IS INITIAL .
      MESSAGE I000(Z01) WITH 'Reserva não encontrada ou não baixada!.'.
      STOP.
    ENDIF.
  ELSE.
    SELECT RESB~RSNUM RESB~RSPOS RESB~XWAOK RESB~KZEAR RESB~WERKS RESB~BDTER RESB~BDMNG RESB~ENMNG RESB~BWART RESB~SAKNR RESB~MATNR RESB~SGTXT RKPF~KOSTL
           RESB~LGORT
      FROM RESB
      INNER JOIN RKPF ON RKPF~RSNUM = RESB~RSNUM
      INTO TABLE IT_RESB
      WHERE RESB~RSNUM IN N_RSV
      AND XWAOK EQ 'X'
      AND   RESB~ENMNG EQ 0.

    IF IT_RESB IS INITIAL .
      MESSAGE I000(Z01) WITH 'Reserva informada não aprovada ou já baixada'.
      STOP.
    ENDIF.
  ENDIF.



  " Descrição Tipo de Movimento
  SELECT BWART BTEXT SPRAS SOBKZ
  FROM T156T
  INTO TABLE IT_T156T
  FOR ALL ENTRIES IN IT_RESB
  WHERE BWART EQ IT_RESB-BWART
  AND SPRAS EQ 'PT'
  AND SOBKZ = ''.

  " Descricao Classificação Contabil
  SELECT SAKNR SPRAS KTOPL TXT50
  FROM SKAT
  INTO TABLE IT_SKAT
  FOR ALL ENTRIES IN IT_RESB
  WHERE SAKNR EQ IT_RESB-SAKNR
  AND SPRAS EQ 'PT'
  AND KTOPL = '0050'.

  " Nome Centro
  SELECT WERKS NAME1
  FROM T001W
  INTO TABLE IT_T001W
  FOR ALL ENTRIES IN IT_RESB
  WHERE WERKS EQ IT_RESB-WERKS.

  IF R_BAIX = 'X'.
    IF NOT ( P_DATR-LOW(4) IS INITIAL ).
      " Segmento de documento - material
      SELECT MJAHR RSNUM KOSTL AUFNR ZEILE MATNR ERFME LGORT MENGE SGTXT MBLNR WERKS WEMPF RSPOS LINE_ID BUDAT_MKPF
        FROM MSEG
        INTO TABLE IT_MSEG_AUX
        FOR ALL ENTRIES IN IT_RESB
      "where mjahr eq it_resb-bdter(4)
      WHERE MJAHR EQ P_DATR-LOW(4)
        AND RSNUM EQ IT_RESB-RSNUM
        AND RSPOS EQ IT_RESB-RSPOS
        AND MBLNR EQ P_MBLNR
        AND BWART IN ('201','Z91','261','311','202','Z92','262','312').


      " Segmento de documento - material
      SELECT MJAHR RSNUM KOSTL AUFNR ZEILE MATNR ERFME LGORT MENGE SGTXT MBLNR WERKS SMBLN WEMPF RSPOS LINE_ID BUDAT_MKPF
        FROM MSEG
        INTO TABLE IT_MSEG_AUX2
        FOR ALL ENTRIES IN IT_RESB
      "where mjahr eq it_resb-bdter(4)
      WHERE MJAHR EQ P_DATR-LOW(4)
        AND RSNUM EQ IT_RESB-RSNUM
        AND RSPOS EQ IT_RESB-RSPOS
        AND MBLNR EQ P_MBLNR
        AND BWART IN ('202','Z92','262','312').

    ELSE.

      " Segmento de documento - material
      SELECT MJAHR RSNUM KOSTL AUFNR ZEILE MATNR ERFME LGORT MENGE SGTXT MBLNR WERKS WEMPF RSPOS LINE_ID BUDAT_MKPF
        FROM MSEG
        INTO TABLE IT_MSEG_AUX
        FOR ALL ENTRIES IN IT_RESB
      "where mjahr eq it_resb-bdter(4)
      WHERE RSNUM EQ IT_RESB-RSNUM
        AND RSPOS EQ IT_RESB-RSPOS
        AND MBLNR EQ P_MBLNR
        AND BWART IN ('201','Z91','261','311','202','Z92','262','312').


      " Segmento de documento - material
      SELECT MJAHR RSNUM KOSTL AUFNR ZEILE MATNR ERFME LGORT MENGE SGTXT MBLNR WERKS SMBLN WEMPF RSPOS LINE_ID BUDAT_MKPF
        FROM MSEG
        INTO TABLE IT_MSEG_AUX2
        FOR ALL ENTRIES IN IT_RESB
      "where mjahr eq it_resb-bdter(4)
      WHERE RSNUM EQ IT_RESB-RSNUM
        AND RSPOS EQ IT_RESB-RSPOS
        AND MBLNR EQ P_MBLNR
        AND BWART IN ('202','Z92','262','312').


    ENDIF.

    " Material sem Extorno
    SORT IT_MSEG_AUX BY MBLNR.

    LOOP AT IT_MSEG_AUX INTO WA_MSEG_AUX.

      MBLNR1 = WA_MSEG_AUX-MBLNR.
      ZEILE1 = WA_MSEG_AUX-ZEILE.

      LOOP AT IT_MSEG_AUX2 INTO WA_MSEG_AUX2 .
        MBLNR2 = WA_MSEG_AUX2-MBLNR.
        ZEILE2 = WA_MSEG_AUX2-ZEILE.
        SMBLN2 = WA_MSEG_AUX2-SMBLN.


        IF  ZEILE2 = ZEILE1 AND SMBLN2 = MBLNR1   .
          CONTINUE.
        ELSE.

          WA_MSEG-MJAHR = WA_MSEG_AUX-MJAHR.
          WA_MSEG-RSNUM = WA_MSEG_AUX-RSNUM.
          WA_MSEG-KOSTL = WA_MSEG_AUX-KOSTL.
          WA_MSEG-AUFNR = WA_MSEG_AUX-AUFNR.
          WA_MSEG-ZEILE = WA_MSEG_AUX-ZEILE.
          WA_MSEG-MATNR = WA_MSEG_AUX-MATNR.
          WA_MSEG-ERFME = WA_MSEG_AUX-ERFME.
          WA_MSEG-LGORT = WA_MSEG_AUX-LGORT.
          WA_MSEG-MENGE = WA_MSEG_AUX-MENGE.
          WA_MSEG-SGTXT = WA_MSEG_AUX-SGTXT.
          WA_MSEG-MBLNR = WA_MSEG_AUX-MBLNR.
          WA_MSEG-WERKS = WA_MSEG_AUX-WERKS.
          WA_MSEG-WEMPF = WA_MSEG_AUX-WEMPF.
          WA_MSEG-RSPOS = WA_MSEG_AUX-RSPOS.
          WA_MSEG-LINE_ID  = WA_MSEG_AUX-LINE_ID.

          APPEND WA_MSEG TO IT_MSEG.

        ENDIF.
      ENDLOOP.
      WA_MSEG-MJAHR = WA_MSEG_AUX-MJAHR.
      WA_MSEG-RSNUM = WA_MSEG_AUX-RSNUM.
      WA_MSEG-KOSTL = WA_MSEG_AUX-KOSTL.
      WA_MSEG-AUFNR = WA_MSEG_AUX-AUFNR.
      WA_MSEG-ZEILE = WA_MSEG_AUX-ZEILE.
      WA_MSEG-MATNR = WA_MSEG_AUX-MATNR.
      WA_MSEG-ERFME = WA_MSEG_AUX-ERFME.
      WA_MSEG-LGORT = WA_MSEG_AUX-LGORT.
      WA_MSEG-MENGE = WA_MSEG_AUX-MENGE.
      WA_MSEG-SGTXT = WA_MSEG_AUX-SGTXT.
      WA_MSEG-MBLNR = WA_MSEG_AUX-MBLNR.
      WA_MSEG-WERKS = WA_MSEG_AUX-WERKS.
      WA_MSEG-WEMPF = WA_MSEG_AUX-WEMPF.
      WA_MSEG-RSPOS = WA_MSEG_AUX-RSPOS.
      WA_MSEG-LINE_ID = WA_MSEG_AUX-LINE_ID.

      APPEND WA_MSEG TO IT_MSEG.


    ENDLOOP.
  ELSE.
    LOOP AT IT_RESB INTO WA_RESB.
      WA_MSEG-RSNUM = WA_RESB-RSNUM. " Nº reserva
      WA_MSEG-RSPOS = WA_RESB-RSPOS. " Nº item da reserva / das necessidades dependentes
      WA_MSEG-WERKS = WA_RESB-WERKS. " Centro
      WA_MSEG-MENGE = WA_RESB-BDMNG. " Quantidade necessária
      WA_MSEG-MATNR = WA_RESB-MATNR. " Nº do material
      WA_MSEG-SGTXT = WA_RESB-SGTXT. " Texto do item
      WA_MSEG-KOSTL = WA_RESB-KOSTL. " Centro de custo
      WA_MSEG-LGORT = WA_RESB-LGORT. " deposito
      APPEND WA_MSEG TO IT_MSEG.
    ENDLOOP.
  ENDIF.

  IF IT_MSEG IS NOT INITIAL.

    "Descricao Centro de Custos
    SELECT KOSTL KOKRS LTEXT
    FROM CSKT
    INTO TABLE IT_CSKT
    FOR ALL ENTRIES IN IT_MSEG
    WHERE KOSTL EQ IT_MSEG-KOSTL
    AND KOKRS IN ('MAGI' , 'MGLD').


    " Descrição do Material
    SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_MSEG
    WHERE MATNR EQ  IT_MSEG-MATNR.

    " Local Material
    SELECT  MATNR WERKS LGORT LGPBE
    FROM MARD
    INTO TABLE IT_MARD
    FOR ALL ENTRIES IN IT_MSEG
    WHERE MATNR EQ  IT_MSEG-MATNR
    AND WERKS  EQ  IT_MSEG-WERKS
    AND LGORT EQ IT_MSEG-LGORT.

    " Cabeçalho do documento da reserva
    SELECT RSNUM PARBU USNAM RSDAT
    FROM RKPF
    INTO TABLE IT_RKPF
    FOR ALL ENTRIES IN IT_MSEG
    WHERE RSNUM EQ IT_MSEG-RSNUM.

    " Nome Empresa - Complemento Cabeçalho reserva
    SELECT BUKRS BUTXT
    FROM T001
    INTO TABLE IT_T001
    FOR ALL ENTRIES IN IT_RKPF
    WHERE BUKRS EQ IT_RKPF-PARBU.

    " Usuário criação reserva - Complemento Cabeçalho reserva
    SELECT BNAME USERALIAS
    FROM USREFUS
    INTO TABLE IT_USREFUSCR
    FOR ALL ENTRIES IN IT_RKPF
    WHERE BNAME EQ IT_RKPF-USNAM.


    " Cabeçalho do documento do material
    SELECT MBLNR USNAM CPUDT CPUTM BKTXT
    FROM MKPF
    INTO TABLE IT_MKPF
    FOR ALL ENTRIES IN IT_MSEG
    WHERE MBLNR EQ IT_MSEG-MBLNR.

    " Usuário Baixa Estoque
    SELECT BNAME USERALIAS
    FROM USREFUS
    INTO TABLE IT_USREFUSBX
    FOR ALL ENTRIES IN IT_MKPF
    WHERE BNAME EQ IT_MKPF-USNAM.

    " Usuário Aprovação Reserva
    SELECT RSNUM RSPOS KOSTL UNAME DT_APROVACAO HR_APROVACAO
    FROM ZMMT0009
    INTO TABLE IT_ZMMT0009
    FOR ALL ENTRIES IN IT_MSEG
    WHERE RSNUM EQ IT_MSEG-RSNUM
    AND   RSPOS EQ IT_MSEG-RSPOS.
    "AND   KOSTL EQ IT_MSEG-KOSTL.

    " Usuário Aprovação Reserva
    SELECT BNAME USERALIAS
    FROM USREFUS
    INTO TABLE IT_USREFUS
    FOR ALL ENTRIES IN IT_ZMMT0009
    WHERE BNAME EQ IT_ZMMT0009-UNAME.

  ENDIF.




ENDFORM.                    "form_seleciona_sd

*&---------------------------------------------------------------------*
*&      Form  FORM_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FORM_SAIDA.

  SORT:
    IT_T001W     BY WERKS,
    IT_MSEG      BY RSNUM  RSPOS LINE_ID,
    IT_RESB      BY RSNUM  MATNR,
    IT_MAKT      BY MATNR,
    IT_MARD      BY MATNR WERKS  LGORT,
    IT_RKPF      BY RSNUM,
    IT_USREFUSCR BY BNAME,
    IT_T001      BY BUKRS,
    IT_MKPF      BY MBLNR,
    IT_USREFUSBX BY BNAME,
    IT_ZMMT0003  BY KOSTL,
    IT_USREFUS   BY BNAME,
    IT_T156T     BY BWART,
    IT_SKAT      BY SAKNR,
    IT_CSKT      BY KOSTL,
    IT_ZMMT0009  BY RSNUM RSPOS KOSTL.

  DATA: AUX(100) TYPE C,
        X_SALDO  TYPE I,
        RSNUMAUX TYPE RESB-RSNUM,
        DIF      TYPE MSEG-MENGE.


  LOOP AT IT_RESB INTO WA_RESB.
    CLEAR: WA_MSEG, WA_MAKT, WA_MARD.
    READ TABLE IT_MSEG INTO WA_MSEG  WITH KEY RSNUM = WA_RESB-RSNUM RSPOS = WA_RESB-RSPOS   BINARY SEARCH.

    IF NOT SY-SUBRC IS INITIAL.
      CONTINUE.
    ENDIF.


    READ TABLE IT_MAKT INTO WA_MAKT  WITH KEY MATNR = WA_MSEG-MATNR  BINARY SEARCH.
    READ TABLE IT_MARD INTO WA_MARD   WITH KEY MATNR = WA_MSEG-MATNR WERKS = WA_MSEG-WERKS LGORT = WA_MSEG-LGORT   BINARY SEARCH.
    " Saida Itens.
    WA_SAIDA-RSPOS = WA_MSEG-RSPOS.
    WA_SAIDA-BDTER = WA_RESB-BDTER.
    WA_SAIDA-MATNR = WA_MSEG-MATNR.
    WA_SAIDA-MAKTX = WA_MAKT-MAKTX.
    WA_SAIDA-ERFME = WA_MSEG-ERFME.
    WA_SAIDA-LGORT = WA_MSEG-LGORT.
    WA_SAIDA-BDMNG = WA_RESB-BDMNG.
    WA_SAIDA-MENGE = WA_RESB-ENMNG.    "WA_MSEG-MENGE.


    " Calculo diferença.
    X_SALDO = 0.
    DIF     = 0.

    REFRESH: IT_MSEG_DIF.

    SELECT RSNUM MBLNR MENGE
    FROM MSEG
    INTO TABLE IT_MSEG_DIF
    WHERE RSNUM EQ WA_RESB-RSNUM
      AND ZEILE  EQ WA_MSEG-ZEILE
      AND MBLNR  EQ WA_MSEG-MBLNR.



*    LOOP AT IT_MSEG_DIF INTO WA_MSEG_DIF.
*      DIF = DIF + WA_MSEG_DIF-MENGE.
*    ENDLOOP.



    X_SALDO = WA_RESB-BDMNG - WA_RESB-ENMNG.

    WA_SAIDA-DIF   = X_SALDO.

    " Fim calculo diferença.



    WA_SAIDA-LGPBE = WA_MARD-LGPBE.
    WA_SAIDA-SGTXT = WA_RESB-SGTXT.


    APPEND WA_SAIDA TO IT_SAIDA.

    IF WA_RESB-RSNUM <> RSNUMAUX.

      RSNUMAUX =  WA_RESB-RSNUM.

      READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_RESB-WERKS    BINARY SEARCH.
      READ TABLE IT_RKPF INTO WA_RKPF   WITH KEY RSNUM = WA_MSEG-RSNUM  BINARY SEARCH.
      READ TABLE IT_T156T INTO WA_T156T WITH KEY BWART = WA_RESB-BWART  BINARY SEARCH.
      READ TABLE IT_SKAT  INTO WA_SKAT  WITH KEY SAKNR = WA_RESB-SAKNR  BINARY SEARCH.
      READ TABLE IT_CSKT  INTO WA_CSKT  WITH KEY KOSTL = WA_MSEG-KOSTL  BINARY SEARCH.
      READ TABLE IT_USREFUSCR INTO WA_USREFUSCR   WITH KEY BNAME = WA_RKPF-USNAM  BINARY SEARCH.
      READ TABLE IT_T001 INTO WA_T001   WITH KEY BUKRS = WA_RKPF-PARBU  BINARY SEARCH.
      READ TABLE IT_MKPF INTO WA_MKPF   WITH KEY MBLNR = WA_MSEG-MBLNR   BINARY SEARCH.
      READ TABLE IT_USREFUSBX INTO WA_USREFUSBX   WITH KEY BNAME = WA_MKPF-USNAM   BINARY SEARCH.
      "READ TABLE it_zmmt0003 INTO wa_zmmt0003   WITH KEY kostl = wa_mseg-kostl   BINARY SEARCH.
      READ TABLE IT_ZMMT0009 INTO WA_ZMMT0009   WITH KEY RSNUM  = WA_MSEG-RSNUM  RSPOS =  WA_MSEG-RSPOS BINARY SEARCH.
      READ TABLE IT_USREFUS INTO WA_USREFUS   WITH KEY BNAME = WA_ZMMT0009-UNAME   BINARY SEARCH.



      " Informações Cabeçalho
      WA_SAIDA_CAB-RSNUM = WA_RESB-RSNUM.
      WA_SAIDA_CAB-BWART = WA_RESB-BWART.
      WA_SAIDA_CAB-KOSTL = WA_MSEG-KOSTL.
      WA_SAIDA_CAB-SAKNR = WA_RESB-SAKNR.
      WA_SAIDA_CAB-AUFNR = WA_MSEG-AUFNR.
      WA_SAIDA_CAB-BKTXT = WA_MKPF-BKTXT.
      WA_SAIDA_CAB-WEMPF = WA_MSEG-WEMPF.

      WA_SAIDA_CAB-BTEXT = WA_T156T-BTEXT.
      WA_SAIDA_CAB-LTEXT = WA_CSKT-LTEXT.
      WA_SAIDA_CAB-TXT50 = WA_SKAT-TXT50.

      AUX = ''.

      CONCATENATE  WA_RKPF-PARBU '-' WA_T001-BUTXT INTO AUX SEPARATED BY SPACE.

      WA_SAIDA_CAB-EMP = AUX. " Empresa

      AUX = ''.

      CONCATENATE  WA_RESB-WERKS '-' WA_T001W-NAME1 INTO AUX SEPARATED BY SPACE.

      WA_SAIDA_CAB-CENTRO = AUX.

      WA_SAIDA_CAB-USNAM         =  WA_RKPF-USNAM.
      WA_SAIDA_CAB-USERALIASCR   =  WA_USREFUSCR-USERALIAS.
      WA_SAIDA_CAB-RSDAT         =  WA_RKPF-RSDAT.
      "wa_saida_cab-uname        =  wa_zmmt0003-uname.
      "wa_saida_cab-useralias    =  wa_usrefus-useralias.

      WA_SAIDA_CAB-UNAME         =  WA_ZMMT0009-UNAME.
      WA_SAIDA_CAB-USERALIAS     =  WA_USREFUS-USERALIAS.
      WA_SAIDA_CAB-DT_APROVACAO  =  WA_ZMMT0009-DT_APROVACAO.
      WA_SAIDA_CAB-HR_APROVACAO  =  WA_ZMMT0009-HR_APROVACAO.

      WA_SAIDA_CAB-USNAMBX       =  WA_MKPF-USNAM.
      WA_SAIDA_CAB-USERALIASBX   =  WA_USREFUSBX-USERALIAS.
      WA_SAIDA_CAB-CPUDT         =  WA_MKPF-CPUDT.
      WA_SAIDA_CAB-CPUTM         =  WA_MKPF-CPUTM.

    ENDIF.

  ENDLOOP.

ENDFORM.                    "form_saida


*&---------------------------------------------------------------------*
*&      Form  FORM_IMPRIMIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FORM_IMPRIMIR  .

  DATA: VL_FORMNAME TYPE TDSFNAME,
        VL_NAME     TYPE RS38L_FNAM.

  VL_FORMNAME = 'ZMMR022_NOVO'.


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

  CALL FUNCTION VL_NAME
    EXPORTING
      P_CAB            = WA_SAIDA_CAB
      P_RES            = WA_SAIDA
    TABLES
      IT_SAIDA         = IT_SAIDA
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      USER_CANCELED    = 4
      OTHERS           = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "form_imprimir
