*----------------------------------------------------------------------*
***INCLUDE LZGF_PEDIDO_COMPRAF01.
*----------------------------------------------------------------------*

DATA: OK_CODE  TYPE SY-UCOMM.

TABLES: ZDE_PEDIDO_CRIAR.

DATA: SPLITTER_HTML1        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CCCONTAINER_HTML1 TYPE REF TO CL_GUI_CONTAINER,
      HTML_CONTROL_HTML1    TYPE REF TO CL_GUI_HTML_VIEWER.

*&---------------------------------------------------------------------*
*&      Form  PEDIDO_COMPRA_LANCAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PEDIDO_COMPRA_LANCAMENTO USING P_FUNDO TYPE C.

  IF P_FUNDO EQ ABAP_TRUE.
    OK_CODE = 'OPEN'.
    CALL SCREEN 0102.
  ELSE.
    CALL SCREEN 0100 STARTING AT 5 3.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0102 OUTPUT.

*  SET PF-STATUS 'xxxxxxxx'.
  SET TITLEBAR 'TL0102'.

  SUPPRESS DIALOG.

  IF SPLITTER_HTML1 IS INITIAL.

    CREATE OBJECT SPLITTER_HTML1
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD SPLITTER_HTML1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CCCONTAINER_HTML1.

    CREATE OBJECT HTML_CONTROL_HTML1
      EXPORTING
        PARENT = CTL_CCCONTAINER_HTML1.

    DATA: DATA_TABLE TYPE STANDARD TABLE OF TEXT255,
          I_URL      TYPE C LENGTH 200.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        I_STRING         = ZCL_UTIL=>GET_HTML_FUNDO( )
        I_TABLINE_LENGTH = 255
      TABLES
        ET_TABLE         = DATA_TABLE.

    HTML_CONTROL_HTML1->LOAD_DATA(
      IMPORTING
        ASSIGNED_URL           = I_URL
      CHANGING
        DATA_TABLE             = DATA_TABLE
      EXCEPTIONS
        DP_INVALID_PARAMETER   = 1
        DP_ERROR_GENERAL       = 2
        CNTL_ERROR             = 3
        HTML_SYNTAX_NOTCORRECT = 4
        OTHERS                 = 5
    ).

    HTML_CONTROL_HTML1->SHOW_URL(
      EXPORTING
        URL                    = I_URL
      EXCEPTIONS
        CNTL_ERROR             = 1
        CNHT_ERROR_NOT_ALLOWED = 2
        CNHT_ERROR_PARAMETER   = 3
        DP_ERROR_GENERAL       = 4
        OTHERS                 = 5
    ).
  ENDIF.

  LEAVE TO LIST-PROCESSING.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102 INPUT.

  CALL SCREEN 0100 STARTING AT 5 3.
  PERFORM LIMPAR_TELA.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE OK_CODE.
    WHEN 'CRIAR'.
      CLEAR: OK_CODE.
      PERFORM CRIAR_PEDIDO_COMPRA.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CRIAR_PEDIDO_COMPRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIAR_PEDIDO_COMPRA .

  DATA: LC_PEDIDO TYPE EBELN.

  PERFORM GET_PEDIDO_COMPRA CHANGING LC_PEDIDO.

  IF LC_PEDIDO IS INITIAL.

  ENDIF.

  CHECK LC_PEDIDO IS NOT INITIAL.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_PEDIDO_COMPRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LC_PEDIDO  text
*----------------------------------------------------------------------*
FORM GET_PEDIDO_COMPRA  CHANGING P_LC_PEDIDO.

  DATA: LC_FILTRO TYPE ZDE_FILTRO_PEDIDO_COMPRA,
        LC_PEDIDO TYPE REF TO ZCL_PEDIDO_COMPRA,
        IT_EKKO   TYPE ZDE_EKKO_T.

  DATA(LC_SAFRA_FILIAL) = |{ ZDE_PEDIDO_CRIAR-SAFRA }_{ ZDE_PEDIDO_CRIAR-BRANCH }|.

  TRY .

      ZCL_CENTRO=>ZIF_CENTRO~GET_INSTANCE(
        )->GET_CENTRO_AFIXAR( EXPORTING I_BUKRS = ZDE_PEDIDO_CRIAR-BUKRS I_BRANCH = ZDE_PEDIDO_CRIAR-BRANCH IMPORTING R_WERKS_AFIXAR = DATA(R_WERKS_AFIXAR)    " Centro
        ).

      ZCL_DEPOSITO=>ZIF_DEPOSITO~GET_INSTANCE(
        )->GET_DEPOSITO_MATERIAL_FILIAL(
        EXPORTING
          I_MATNR          = ZDE_PEDIDO_CRIAR-MATNR
          I_TP_PRODUTO     = SPACE
          I_BUKRS          = ZDE_PEDIDO_CRIAR-BUKRS
          I_BRANCH         = ZDE_PEDIDO_CRIAR-BRANCH
          I_CENTRO_A_FIXAR = R_WERKS_AFIXAR    " Centro
        IMPORTING
          E_LGORT          = DATA(E_LGORT)    " Depósito
      ).

      ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
        )->SET_PARCEIRO( I_PARCEIRO = ZDE_PEDIDO_CRIAR-LIFNR
        )->CK_ATIVO(
        )->CK_ATIVO_EMPRESA( I_EMPRESA = ZDE_PEDIDO_CRIAR-BUKRS
        )->CK_RESTRICAO_EMBARGO(
        )->GET_TIPO_PARCEIRO( IMPORTING E_TIPO = DATA(E_TIPO)
        )->GET_REGIO( IMPORTING E_REGIO	= DATA(E_REGIO)
        ).

      SELECT SINGLE * INTO @DATA(WA_ZSDT0001TELN)
        FROM ZSDT0001TELN
       WHERE ID_ENTRADA EQ @ZDE_PEDIDO_CRIAR-ID_ENTRADA
         AND NR_SAFRA   EQ @ZDE_PEDIDO_CRIAR-SAFRA
         AND ID_BUKRS   EQ @ZDE_PEDIDO_CRIAR-BUKRS
         AND ID_BRANCH  EQ @ZDE_PEDIDO_CRIAR-BRANCH.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_TP_ENTRADA_NAO_PERMITIDO-MSGID MSGNO = ZCX_CARGA=>ZCX_TP_ENTRADA_NAO_PERMITIDO-MSGNO
                              ATTR1 = CONV #( ZDE_PEDIDO_CRIAR-ID_ENTRADA )
                              ATTR2 = CONV #( ZDE_PEDIDO_CRIAR-SAFRA )
                              ATTR3 = CONV #( ZDE_PEDIDO_CRIAR-BRANCH ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_TP_ENTRADA_NAO_PERMITIDO-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_TP_ENTRADA_NAO_PERMITIDO-MSGID
            MSGV1  = CONV #( ZDE_PEDIDO_CRIAR-ID_ENTRADA )
            MSGV2  = CONV #( ZDE_PEDIDO_CRIAR-SAFRA )
            MSGV3  = CONV #( ZDE_PEDIDO_CRIAR-BRANCH ).
      ENDIF.

      SELECT SINGLE * INTO @DATA(WA_TIPO_ENTRADA)
        FROM ZSDT0001TE
       WHERE ID_ENTRADA EQ @ZDE_PEDIDO_CRIAR-ID_ENTRADA
         AND ID_EMPRESA EQ @ZDE_PEDIDO_CRIAR-BUKRS
         AND CK_NFE     EQ @ZDE_PEDIDO_CRIAR-EMITE_NFE.

      DATA(ID_MOD_FISCAL) = COND STRING( WHEN ZDE_PEDIDO_CRIAR-EMITE_NFE EQ ABAP_TRUE THEN ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO ELSE ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL ).

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_TE_MD_FISCAL_SEM_PARAM-MSGID
                              MSGNO = ZCX_CARGA=>ZCX_TE_MD_FISCAL_SEM_PARAM-MSGNO
                              ATTR1 = ID_MOD_FISCAL )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_TE_MD_FISCAL_SEM_PARAM-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_TE_MD_FISCAL_SEM_PARAM-MSGID
            MSGV1  = CONV #( ID_MOD_FISCAL ).
      ENDIF.

      IF WA_TIPO_ENTRADA-ID_IVA IS INITIAL.
        RAISE EXCEPTION TYPE ZCX_CARGA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_TE_SEM_IVA-MSGID MSGNO = ZCX_CARGA=>ZCX_TE_SEM_IVA-MSGNO )
            MSGTY  = 'E'
            MSGNO  = ZCX_CARGA=>ZCX_TE_SEM_IVA-MSGNO
            MSGID  = ZCX_CARGA=>ZCX_TE_SEM_IVA-MSGID.
      ENDIF.

      CASE WA_TIPO_ENTRADA-TP_PESSOA.
        WHEN ZIF_PARCEIROS=>ST_PESSOA_FISICA.
          IF E_TIPO NE WA_TIPO_ENTRADA-TP_PESSOA.
            RAISE EXCEPTION TYPE ZCX_CARGA
              EXPORTING
                TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_TE_SOMENTE_FISICA-MSGID
                                  MSGNO = ZCX_CARGA=>ZCX_TE_SOMENTE_FISICA-MSGNO
                                  ATTR1 = CONV #( WA_TIPO_ENTRADA-ID_ENTRADA ) )
                MSGTY  = 'E'
                MSGNO  = ZCX_CARGA=>ZCX_TE_SOMENTE_FISICA-MSGNO
                MSGID  = ZCX_CARGA=>ZCX_TE_SOMENTE_FISICA-MSGID
                MSGV1  = CONV #( WA_TIPO_ENTRADA-ID_ENTRADA ).
          ENDIF.
        WHEN ZIF_PARCEIROS=>ST_PESSOA_JURIDICA.
          IF E_TIPO NE WA_TIPO_ENTRADA-TP_PESSOA.
            RAISE EXCEPTION TYPE ZCX_CARGA
              EXPORTING
                TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_TE_SOMENTE_JURIDICA-MSGID
                                  MSGNO = ZCX_CARGA=>ZCX_TE_SOMENTE_JURIDICA-MSGNO
                                  ATTR1 = CONV #( WA_TIPO_ENTRADA-ID_ENTRADA ) )
                MSGTY  = 'E'
                MSGNO  = ZCX_CARGA=>ZCX_TE_SOMENTE_JURIDICA-MSGNO
                MSGID  = ZCX_CARGA=>ZCX_TE_SOMENTE_JURIDICA-MSGID
                MSGV1  = CONV #( WA_TIPO_ENTRADA-ID_ENTRADA ).
          ENDIF.
      ENDCASE.

      CREATE OBJECT LC_PEDIDO.
      LC_FILTRO-IBUKRS = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = ZDE_PEDIDO_CRIAR-BUKRS  HIGH = ZDE_PEDIDO_CRIAR-BUKRS  ) ).
      LC_FILTRO-ILIFNR = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = ZDE_PEDIDO_CRIAR-LIFNR  HIGH = ZDE_PEDIDO_CRIAR-LIFNR  ) ).
      LC_FILTRO-IMATNR = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = ZDE_PEDIDO_CRIAR-MATNR  HIGH = ZDE_PEDIDO_CRIAR-MATNR  ) ).
      LC_FILTRO-ICHARG = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = ZDE_PEDIDO_CRIAR-SAFRA  HIGH = ZDE_PEDIDO_CRIAR-SAFRA  ) ( LOW = LC_SAFRA_FILIAL HIGH = LC_SAFRA_FILIAL ) ).
      LC_FILTRO-IWERKS = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = R_WERKS_AFIXAR   HIGH = R_WERKS_AFIXAR ) ).
      LC_FILTRO-IBSTYP = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = 'F'     HIGH = 'F'     ) ).
      LC_FILTRO-IBSART = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = 'ZGR'   HIGH = 'ZGR'   ) ).
      LC_FILTRO-IEKORG = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = 'OC01'  HIGH = 'OC01'  ) ).
      LC_FILTRO-IEKGRP = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = 'G01'   HIGH = 'G01'   ) ).
      LC_FILTRO-IFRGRL = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = SPACE   HIGH = SPACE   ) ).
      LC_FILTRO-IEBELP = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = '00010' HIGH = '00010' ) ).
      LC_FILTRO-IBSTAE = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = '0004'  HIGH = '0004'  ) ).
      LC_FILTRO-IMWSKZ = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = WA_TIPO_ENTRADA-ID_IVA  HIGH = WA_TIPO_ENTRADA-ID_IVA  ) ).
      LC_FILTRO-ILGORT = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = E_LGORT HIGH = E_LGORT ) ).

      IF LC_PEDIDO->ZIF_PESQUISA~PESQUISAR( EXPORTING I_FILTROS  = LC_FILTRO IMPORTING E_REGISTROS = IT_EKKO ) NE ABAP_TRUE.

        ZCL_PEDIDO_COMPRA=>SET_CRIAR_PEDIDO_COMPRA(
              EXPORTING
                I_BEDAT      = SY-DATUM  " Data do documento de compra
                I_EKORG      = 'OC01'    " Organização de compras
                I_EKGRP      = 'G01'    " Grupo de compradores
                I_WAERS      = 'BRL'    " Código da moeda
                I_BSART      = 'ZGR '   " Tipo de documento de compras
                I_ZTERM      = 'Z001'    " Chave de condições de pagamento
                I_LIFNR      = ZDE_PEDIDO_CRIAR-LIFNR " Nº conta do fornecedor
                I_BUKRS      = ZDE_PEDIDO_CRIAR-BUKRS " Empresa
                I_CHARG      = CONV #( ZDE_PEDIDO_CRIAR-SAFRA )    " Número do lote
                I_EINDT      = SY-DATUM    " Data de remessa do item
                I_MWSKZ      = WA_TIPO_ENTRADA-ID_IVA " Código do IVA
                I_MENGE      = 400000000    " Quantidade do pedido
                I_MATNR      = ZDE_PEDIDO_CRIAR-MATNR " Nº do material
                I_MEINS      = 'KG'    " Unidade de medida do pedido
                I_WERKS      = ZDE_PEDIDO_CRIAR-BRANCH    " Centro
                I_TP_CENTRO  = ZCL_PEDIDO_COMPRA=>ST_TP_CENTRO_A_FIXAR
            ).
        WAIT UP TO 5 SECONDS.

        LC_PEDIDO->ZIF_PESQUISA~PESQUISAR( EXPORTING I_FILTROS  = LC_FILTRO IMPORTING E_REGISTROS = IT_EKKO ).

      ENDIF.

      READ TABLE IT_EKKO INDEX 1 INTO DATA(WA_EKKO).

      CHECK SY-SUBRC IS INITIAL.

      ZCL_PEDIDO_COMPRA=>SHOW_PEDIDO( I_EBELN = WA_EKKO-EBELN ).

    CATCH ZCX_PARCEIROS INTO DATA(EX_FORNECEDOR).    " .
      EX_FORNECEDOR->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_CARGA INTO DATA(EX_CARGA).    " .
      EX_CARGA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_DEPOSITO INTO DATA(EX_DEPOSITO).    " .
      EX_DEPOSITO->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_NFE_INBOUND_EXCEPTION INTO DATA(EX_NFE).
      EX_NFE->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_PEDIDO_COMPRA_EXCEPTION INTO DATA(EX_PEDIDO).
      EX_PEDIDO->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_CADASTRO INTO DATA(EX_CADASTRO).
      EX_CADASTRO->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_JOB INTO DATA(EX_JOB).
      EX_JOB->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDAR_PSAFRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDAR_PSAFRA INPUT.

  DATA: PNO_INI TYPE I,
        PNO_FIM TYPE I.

  PNO_INI = SY-DATUM(4).
  PNO_FIM = SY-DATUM(4).
  ADD -1 TO PNO_INI.
  ADD  1 TO PNO_FIM.

  IF ZDE_PEDIDO_CRIAR-SAFRA LT PNO_INI OR
     ZDE_PEDIDO_CRIAR-SAFRA GT PNO_FIM.
    MESSAGE E002 WITH ZDE_PEDIDO_CRIAR-SAFRA.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDAR_PFILIA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDAR_PFILIA INPUT.

  SELECT SINGLE * INTO @DATA(WA_J_1BBRANCH)
    FROM J_1BBRANCH
   WHERE BRANCH EQ @ZDE_PEDIDO_CRIAR-BRANCH.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE E003 WITH ZDE_PEDIDO_CRIAR-BRANCH.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
    ID 'WERKS' FIELD  ZDE_PEDIDO_CRIAR-BRANCH
    ID 'ACTVT' FIELD '03'.    "Alteração

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E004.
    WHEN 12.
      MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    WHEN OTHERS.
  ENDCASE.

  CLEAR: WA_J_1BBRANCH.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TELA .

  IF HTML_CONTROL_HTML1 IS NOT INITIAL.
    HTML_CONTROL_HTML1->FREE( ).
  ENDIF.
  CLEAR: HTML_CONTROL_HTML1.

  IF CTL_CCCONTAINER_HTML1 IS NOT INITIAL.
    CTL_CCCONTAINER_HTML1->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER_HTML1.

  IF SPLITTER_HTML1 IS NOT INITIAL.
    SPLITTER_HTML1->FREE( ).
  ENDIF.
  CLEAR: SPLITTER_HTML1.

ENDFORM.
