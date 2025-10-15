class ZCL_FI_COMPENSACAO definition
  public
  final
  create public .

public section.

  interfaces ZIF_FI_COMPENSACAO .

  aliases ADD_PARTIDA
    for ZIF_FI_COMPENSACAO~ADD_PARTIDA .
  aliases COMPENSAR
    for ZIF_FI_COMPENSACAO~COMPENSAR .
  aliases SET_AUGLV
    for ZIF_FI_COMPENSACAO~SET_AUGLV .
  aliases SET_BUKRS
    for ZIF_FI_COMPENSACAO~SET_BUKRS .
  aliases SET_DT_COMPENSACAO
    for ZIF_FI_COMPENSACAO~SET_DT_COMPENSACAO .
  aliases SET_DT_CONVERSAO
    for ZIF_FI_COMPENSACAO~SET_DT_CONVERSAO .
  aliases SET_DT_DOCUMENTO
    for ZIF_FI_COMPENSACAO~SET_DT_DOCUMENTO .
  aliases SET_DT_LCTO
    for ZIF_FI_COMPENSACAO~SET_DT_LCTO .
  aliases SET_MODE
    for ZIF_FI_COMPENSACAO~SET_MODE .
  aliases SET_MOEDA
    for ZIF_FI_COMPENSACAO~SET_MOEDA .
  aliases SET_PERIODO
    for ZIF_FI_COMPENSACAO~SET_PERIODO .
  aliases SET_REFERENCIA
    for ZIF_FI_COMPENSACAO~SET_REFERENCIA .
  aliases SET_SGFUNCT
    for ZIF_FI_COMPENSACAO~SET_SGFUNCT .
  aliases SET_SIMULACAO
    for ZIF_FI_COMPENSACAO~SET_SIMULACAO .
  aliases SET_TAXA_CAMBIO
    for ZIF_FI_COMPENSACAO~SET_TAXA_CAMBIO .
  aliases SET_TCODE
    for ZIF_FI_COMPENSACAO~SET_TCODE .
  aliases SET_TEXTO_CAB_DOC
    for ZIF_FI_COMPENSACAO~SET_TEXTO_CAB_DOC .
  aliases SET_TEXTO_COMPENSACAO
    for ZIF_FI_COMPENSACAO~SET_TEXTO_COMPENSACAO .
  aliases SET_TP_DOCUMENTO
    for ZIF_FI_COMPENSACAO~SET_TP_DOCUMENTO .
protected section.
private section.

  aliases AT_AUGLV
    for ZIF_FI_COMPENSACAO~AT_AUGLV .
  aliases AT_AUGTX
    for ZIF_FI_COMPENSACAO~AT_AUGTX .
  aliases AT_BKTXT
    for ZIF_FI_COMPENSACAO~AT_BKTXT .
  aliases AT_BLART
    for ZIF_FI_COMPENSACAO~AT_BLART .
  aliases AT_BLDAT
    for ZIF_FI_COMPENSACAO~AT_BLDAT .
  aliases AT_BUDAT
    for ZIF_FI_COMPENSACAO~AT_BUDAT .
  aliases AT_BUKRS
    for ZIF_FI_COMPENSACAO~AT_BUKRS .
  aliases AT_HWAE2_EMP
    for ZIF_FI_COMPENSACAO~AT_HWAE2_EMP .
  aliases AT_HWAE3_EMP
    for ZIF_FI_COMPENSACAO~AT_HWAE3_EMP .
  aliases AT_KURSF
    for ZIF_FI_COMPENSACAO~AT_KURSF .
  aliases AT_MODE
    for ZIF_FI_COMPENSACAO~AT_MODE .
  aliases AT_MONAT
    for ZIF_FI_COMPENSACAO~AT_MONAT .
  aliases AT_PARTIDAS_COMP
    for ZIF_FI_COMPENSACAO~AT_PARTIDAS_COMP .
  aliases AT_SGFUNCT
    for ZIF_FI_COMPENSACAO~AT_SGFUNCT .
  aliases AT_SIMULACAO
    for ZIF_FI_COMPENSACAO~AT_SIMULACAO .
  aliases AT_TCODE
    for ZIF_FI_COMPENSACAO~AT_TCODE .
  aliases AT_WAERS
    for ZIF_FI_COMPENSACAO~AT_WAERS .
  aliases AT_WAERS_EMP
    for ZIF_FI_COMPENSACAO~AT_WAERS_EMP .
  aliases AT_WWERT
    for ZIF_FI_COMPENSACAO~AT_WWERT .
  aliases AT_XBLNR
    for ZIF_FI_COMPENSACAO~AT_XBLNR .
  aliases ADD_PARTIDA_RESIDUAL
    for ZIF_FI_COMPENSACAO~ADD_PARTIDA_RESIDUAL .
  aliases CHECK_PARTIDA_OPEN
    for ZIF_FI_COMPENSACAO~CHECK_PARTIDA_OPEN .
  aliases PREPARAR_DADOS_COMPENSACAO
    for ZIF_FI_COMPENSACAO~PREPARAR_DADOS_COMPENSACAO .
  aliases SET_TAXA_PARTIDA
    for ZIF_FI_COMPENSACAO~SET_TAXA_PARTIDA .
  aliases TRANSF_DADOS_PARTIDA_D
    for ZIF_FI_COMPENSACAO~TRANSF_DADOS_PARTIDA_D .
  aliases TRANSF_DADOS_PARTIDA_K
    for ZIF_FI_COMPENSACAO~TRANSF_DADOS_PARTIDA_K .
  aliases TRANSF_DADOS_PARTIDA_S
    for ZIF_FI_COMPENSACAO~TRANSF_DADOS_PARTIDA_S .
  aliases VALIDAR_COMPENSACAO
    for ZIF_FI_COMPENSACAO~VALIDAR_COMPENSACAO .
ENDCLASS.



CLASS ZCL_FI_COMPENSACAO IMPLEMENTATION.


  method ZIF_FI_COMPENSACAO~ADD_PARTIDA.

    CHECK I_PARTIDA_COMP IS NOT INITIAL.

    APPEND I_PARTIDA_COMP TO ME->AT_PARTIDAS_COMP.

  endmethod.


  method ZIF_FI_COMPENSACAO~ADD_PARTIDA_RESIDUAL.

    DATA: V_ZFBDT_OUT(10),
          V_VLR_OUT(16).

    DATA: V_VLR TYPE BSIK-WRBTR.

    DATA: WL_FTPOST TYPE FTPOST.

    IF I_PARTIDA_ORIGINAL-VLR_RESIDUAL <= 0.
      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_SLD_RESIDUAL_INVALID-MSGID
                             MSGNO = ZCX_FI_COMPENSACAO=>ZCX_SLD_RESIDUAL_INVALID-MSGNO
                             ATTR1 = CONV #( I_PARTIDA_ORIGINAL-BELNR )
                             ATTR2 = CONV #( I_PARTIDA_ORIGINAL-BUZEI )
                             ATTR3 = CONV #( I_PARTIDA_ORIGINAL-BUKRS )
                             ATTR4 = CONV #( I_PARTIDA_ORIGINAL-GJAHR ) )
             MSGTY  = 'E'
             MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_SLD_RESIDUAL_INVALID-MSGNO
             MSGID  = ZCX_FI_COMPENSACAO=>ZCX_SLD_RESIDUAL_INVALID-MSGID
             MSGV1  = CONV #( I_PARTIDA_ORIGINAL-BELNR )
             MSGV2  = CONV #( I_PARTIDA_ORIGINAL-BUZEI )
             MSGV3  = CONV #( I_PARTIDA_ORIGINAL-BUKRS )
             MSGV4  = CONV #( I_PARTIDA_ORIGINAL-GJAHR ).
    ENDIF.

    IF I_PARTIDA_ORIGINAL-KURSF <= 0.
      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_TX_GER_RESID_NOT_FOUND-MSGID
                             MSGNO = ZCX_FI_COMPENSACAO=>ZCX_TX_GER_RESID_NOT_FOUND-MSGNO
                             ATTR1 = CONV #( I_PARTIDA_ORIGINAL-BELNR )
                             ATTR2 = CONV #( I_PARTIDA_ORIGINAL-BUZEI )
                             ATTR3 = CONV #( I_PARTIDA_ORIGINAL-GJAHR ) )
             MSGTY  = 'E'
             MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_TX_GER_RESID_NOT_FOUND-MSGNO
             MSGID  = ZCX_FI_COMPENSACAO=>ZCX_TX_GER_RESID_NOT_FOUND-MSGID
             MSGV1  = CONV #( I_PARTIDA_ORIGINAL-BELNR )
             MSGV2  = CONV #( I_PARTIDA_ORIGINAL-BUZEI )
             MSGV3  = CONV #( I_PARTIDA_ORIGINAL-GJAHR ).
    ENDIF.

    DATA(_ERRO_CHAVE) = ABAP_FALSE.

    CASE I_PARTIDA_ORIGINAL-KOART.
      WHEN 'D'. "Cliente
         IF ( I_PARTIDA_ORIGINAL-BSCHL NE '01' ) AND
            ( I_PARTIDA_ORIGINAL-BSCHL NE '11' ) AND
            ( I_PARTIDA_ORIGINAL-BSCHL NE '09' ) AND
            ( I_PARTIDA_ORIGINAL-BSCHL NE '19' ).
           _ERRO_CHAVE = ABAP_TRUE.
         ENDIF.
      WHEN 'K'. "Fornecedor
        IF ( I_PARTIDA_ORIGINAL-BSCHL NE '21' ) AND
           ( I_PARTIDA_ORIGINAL-BSCHL NE '31' ) AND
           ( I_PARTIDA_ORIGINAL-BSCHL NE '29' ) AND
           ( I_PARTIDA_ORIGINAL-BSCHL NE '39' ).
          _ERRO_CHAVE  = ABAP_TRUE.
        ENDIF.
    ENDCASE.

    IF _ERRO_CHAVE EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_ERRO_BSCHL-MSGID
                             MSGNO = ZCX_FI_COMPENSACAO=>ZCX_ERRO_BSCHL-MSGNO
                             ATTR1 = CONV #( I_PARTIDA_ORIGINAL-BSCHL ) )
             MSGTY  = 'E'
             MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_ERRO_BSCHL-MSGNO
             MSGID  = ZCX_FI_COMPENSACAO=>ZCX_ERRO_BSCHL-MSGID
             MSGV1  = CONV #( I_PARTIDA_ORIGINAL-BSCHL ).
    ENDIF.

    ADD 1 TO C_COUNT_FT.

    WL_FTPOST-STYPE = 'P'.
    WL_FTPOST-COUNT = C_COUNT_FT .

    "Chave Lançamento
    IF I_PARTIDA_ORIGINAL-BSCHL IS NOT INITIAL.
      WL_FTPOST-FNAM = 'RF05A-NEWBS'.
      WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-BSCHL.
      APPEND WL_FTPOST TO C_FTPOST_T.
    ENDIF.

    "Nº Conta
    IF I_PARTIDA_ORIGINAL-AGKON IS NOT INITIAL.
      WL_FTPOST-FNAM = 'BSEG-HKONT'.
      WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-AGKON.
      APPEND WL_FTPOST TO C_FTPOST_T.
    ENDIF.

    "Divisão
    IF I_PARTIDA_ORIGINAL-GSBER IS NOT INITIAL.
      WL_FTPOST-FNAM = 'BSEG-GSBER'.
      WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-GSBER.
      APPEND WL_FTPOST TO C_FTPOST_T.
    ENDIF.

    "Texto Item
    IF I_PARTIDA_ORIGINAL-SGTXT_RESIDUAL IS NOT INITIAL.
      WL_FTPOST-FNAM = 'BSEG-SGTXT'.
      WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-SGTXT_RESIDUAL.
      APPEND WL_FTPOST TO C_FTPOST_T.
    ENDIF.

    "Atribuição
    IF I_PARTIDA_ORIGINAL-ZUONR IS NOT INITIAL.
      WL_FTPOST-FNAM = 'BSEG-ZUONR'.
      WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-ZUONR.
      APPEND WL_FTPOST TO C_FTPOST_T.
    ENDIF.

    "OV/Pedido
    IF I_PARTIDA_ORIGINAL-OVPED IS NOT INITIAL.
      WL_FTPOST-FNAM = 'BSEG-HZUON'.
      WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-OVPED.

      IF I_PARTIDA_ORIGINAL-EBELP IS NOT INITIAL.
        WL_FTPOST-FVAL = WL_FTPOST-FVAL && I_PARTIDA_ORIGINAL-EBELP.
      ENDIF.
      APPEND WL_FTPOST TO C_FTPOST_T.
    ENDIF.

    "Data Vencimento
    IF I_PARTIDA_ORIGINAL-ZFBDT IS NOT INITIAL.
      CONCATENATE I_PARTIDA_ORIGINAL-ZFBDT+6(2) I_PARTIDA_ORIGINAL-ZFBDT+4(2) I_PARTIDA_ORIGINAL-ZFBDT(4)
             INTO V_ZFBDT_OUT SEPARATED BY '.'.

      WL_FTPOST-FNAM = 'BSEG-ZFBDT'.
      WL_FTPOST-FVAL = V_ZFBDT_OUT.
      APPEND WL_FTPOST TO C_FTPOST_T.

      IF ( I_PARTIDA_ORIGINAL-UMSKS IS INITIAL ).
        WL_FTPOST-FNAM = 'BSEG-ZBD1T'.
        WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-ZBD1T.
        CONDENSE WL_FTPOST-FVAL NO-GAPS.

        APPEND WL_FTPOST TO C_FTPOST_T.
      ENDIF.
    ENDIF.

    "Classe de operação de Razão Especial
    IF I_PARTIDA_ORIGINAL-UMSKS IS NOT INITIAL. "Adiantamento
      WL_FTPOST-FNAM = 'RF05A-NEWUM'.
      WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-UMSKZ.
      APPEND WL_FTPOST TO C_FTPOST_T.

      IF ( I_PARTIDA_ORIGINAL-ANLN1 IS NOT INITIAL ).
        WL_FTPOST-FNAM = 'BSEG-ANLN1'.
        WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-ANLN1.
        APPEND WL_FTPOST TO C_FTPOST_T.

        IF I_PARTIDA_ORIGINAL-ANLN2 IS NOT INITIAL.
          WL_FTPOST-FNAM = 'BSEG-ANLN2'.
          WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-ANLN2.
          APPEND WL_FTPOST TO C_FTPOST_T.
        ENDIF.
      ENDIF.
    ENDIF.

    "Valor Documento
    IF I_PARTIDA_ORIGINAL-VLR_RESIDUAL IS NOT INITIAL.
      WRITE: I_PARTIDA_ORIGINAL-VLR_RESIDUAL TO V_VLR_OUT.
      CONDENSE V_VLR_OUT NO-GAPS.

      WL_FTPOST-FNAM = 'BSEG-WRBTR'.
      WL_FTPOST-FVAL =  V_VLR_OUT.
      APPEND WL_FTPOST TO C_FTPOST_T.
    ENDIF.

*   CS2017000399 - Baixa Adto - Saldo Residual - Copiar informação do documento origem  US 74720 - BG - Inicio

    "ref de pgto
     IF I_PARTIDA_ORIGINAL-KIDNO IS NOT INITIAL.
      WL_FTPOST-FNAM = 'BSEG-KIDNO'.
      WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-KIDNO.
      APPEND WL_FTPOST TO C_FTPOST_T.
    ENDIF.

*   Chave ref 1
    IF I_PARTIDA_ORIGINAL-xref1 IS NOT INITIAL.
      WL_FTPOST-FNAM = 'BSEG-XREF1'.
      WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-xref1.
      APPEND WL_FTPOST TO C_FTPOST_T.
    ENDIF.


**   Chavve ref 3
*    IF I_PARTIDA_ORIGINAL-xref3 IS NOT INITIAL.
*      WL_FTPOST-FNAM = 'BSEG-XREF3'.
*      WL_FTPOST-FVAL = I_PARTIDA_ORIGINAL-xref3.
*      APPEND WL_FTPOST TO C_FTPOST_T.
*    ENDIF.


*   CS2017000399 - Baixa Adto - Saldo Residual - Copiar informação do documento origem  US 74720 - BG - FIM

    "Tratamento por Moedas da Empresa
    IF ( ME->AT_WAERS_EMP EQ 'BRL' ) AND  "Primeira Moeda Empresa
       ( ME->AT_HWAE2_EMP EQ 'USD' ).     "Segunda  Moeda Empresa

      IF I_PARTIDA_ORIGINAL-WAERS EQ ME->AT_WAERS_EMP.

        V_VLR = I_PARTIDA_ORIGINAL-VLR_RESIDUAL / ABS( I_PARTIDA_ORIGINAL-KURSF ).
        WRITE: V_VLR TO V_VLR_OUT.
        CONDENSE V_VLR_OUT NO-GAPS.

        WL_FTPOST-FNAM = 'BSEG-DMBE2'.
        WL_FTPOST-FVAL =  V_VLR_OUT.
        APPEND WL_FTPOST TO C_FTPOST_T.

      ELSE.

        V_VLR = I_PARTIDA_ORIGINAL-VLR_RESIDUAL * ABS( I_PARTIDA_ORIGINAL-KURSF ).

        WRITE: V_VLR TO V_VLR_OUT.
        CONDENSE V_VLR_OUT NO-GAPS.

        WL_FTPOST-FNAM = 'BSEG-DMBTR'.
        WL_FTPOST-FVAL =  V_VLR_OUT.
        APPEND WL_FTPOST TO C_FTPOST_T.

      ENDIF.

    ELSE.

      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_CENARIO_NAO_PREVISTO-MSGID
                            MSGNO = ZCX_FI_COMPENSACAO=>ZCX_CENARIO_NAO_PREVISTO-MSGNO
                            ATTR1 = CONV #( '(Definição Valores Residual)' )
                            ATTR2 = CONV #( ME->AT_WAERS_EMP              )
                            ATTR3 = CONV #( ME->AT_HWAE2_EMP              ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_CENARIO_NAO_PREVISTO-MSGNO
            MSGID  = ZCX_FI_COMPENSACAO=>ZCX_CENARIO_NAO_PREVISTO-MSGID
            MSGV1  = CONV #(  '(Definição Valores Residual)' )
            MSGV2  = CONV #(  ME->AT_WAERS_EMP              )
            MSGV3  = CONV #(  ME->AT_HWAE2_EMP              ).

    ENDIF.


  endmethod.


  method ZIF_FI_COMPENSACAO~CHECK_PARTIDA_OPEN.

    CLEAR: E_BSIK, E_BSID, E_BSIS, E_BSEG.

    DATA ETL5C4R9486 TYPE TABLE OF BSEG.
DATA RLDNR_L5C4R8811 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L5C4R8811
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L5C4R8811
    I_BUKRS = I_BUKRS
    I_BELNR = I_BELNR
    I_GJAHR = I_GJAHR
    I_BUZEI = I_BUZEI
  IMPORTING
    ET_BSEG = ETL5C4R9486
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC = 0 AND LINES( ETL5C4R9486 ) = 1.
  E_BSEG = ETL5C4R9486[ 1 ].
  SY-DBCNT = 1.
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


    IF SY-SUBRC NE 0.
      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_FOUND-MSGID
                             MSGNO = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_FOUND-MSGNO
                             ATTR1 = CONV #( I_BELNR )
                             ATTR2 = CONV #( I_BUZEI )
                             ATTR3 = CONV #( I_BUKRS )
                             ATTR4 = CONV #( I_GJAHR ) )
             MSGTY  = 'E'
             MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_FOUND-MSGNO
             MSGID  = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_FOUND-MSGID
             MSGV1  = CONV #( I_BELNR )
             MSGV2  = CONV #( I_BUZEI )
             MSGV3  = CONV #( I_BUKRS )
             MSGV4  = CONV #( I_GJAHR ).
    ENDIF.

    CASE E_BSEG-KOART.
      WHEN 'D'. "Cliente

        SELECT SINGLE *
          FROM BSID INTO E_BSID
         WHERE BUKRS EQ E_BSEG-BUKRS
           AND BELNR EQ E_BSEG-BELNR
           AND GJAHR EQ E_BSEG-GJAHR
           AND BUZEI EQ E_BSEG-BUZEI.

        IF SY-SUBRC NE 0.
          RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
            EXPORTING
               TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGID
                                 MSGNO = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGNO
                                 ATTR1 = CONV #( E_BSEG-BELNR )
                                 ATTR2 = CONV #( E_BSEG-BUZEI )
                                 ATTR3 = CONV #( E_BSEG-BUKRS )
                                 ATTR4 = CONV #( E_BSEG-GJAHR ) )
                 MSGTY  = 'E'
                 MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGNO
                 MSGID  = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGID
                 MSGV1  = CONV #( E_BSEG-BELNR )
                 MSGV2  = CONV #( E_BSEG-BUZEI )
                 MSGV3  = CONV #( E_BSEG-BUKRS )
                 MSGV4  = CONV #( E_BSEG-GJAHR ).
        ENDIF.

      WHEN 'K'. "Fornecedores

        SELECT SINGLE *
          FROM BSIK INTO E_BSIK
         WHERE BUKRS EQ E_BSEG-BUKRS
           AND BELNR EQ E_BSEG-BELNR
           AND GJAHR EQ E_BSEG-GJAHR
           AND BUZEI EQ E_BSEG-BUZEI.

        IF SY-SUBRC NE 0.
          RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
            EXPORTING
               TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGID
                                 MSGNO = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGNO
                                 ATTR1 = CONV #( E_BSEG-BELNR )
                                 ATTR2 = CONV #( E_BSEG-BUZEI )
                                 ATTR3 = CONV #( E_BSEG-BUKRS )
                                 ATTR4 = CONV #( E_BSEG-GJAHR ) )
                 MSGTY  = 'E'
                 MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGNO
                 MSGID  = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGID
                 MSGV1  = CONV #( E_BSEG-BELNR )
                 MSGV2  = CONV #( E_BSEG-BUZEI )
                 MSGV3  = CONV #( E_BSEG-BUKRS )
                 MSGV4  = CONV #( E_BSEG-GJAHR ).
        ENDIF.

      WHEN 'S'. "Contas do Razão

        SELECT SINGLE *
          FROM BSIS INTO E_BSIS
         WHERE BUKRS EQ E_BSEG-BUKRS
           AND BELNR EQ E_BSEG-BELNR
           AND GJAHR EQ E_BSEG-GJAHR
           AND BUZEI EQ E_BSEG-BUZEI.

        IF SY-SUBRC NE 0.
          RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
            EXPORTING
               TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGID
                                 MSGNO = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGNO
                                 ATTR1 = CONV #( E_BSEG-BELNR )
                                 ATTR2 = CONV #( E_BSEG-BUZEI )
                                 ATTR3 = CONV #( E_BSEG-BUKRS )
                                 ATTR4 = CONV #( E_BSEG-GJAHR ) )
                 MSGTY  = 'E'
                 MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGNO
                 MSGID  = ZCX_FI_COMPENSACAO=>ZCX_DOCUMENT_NOT_OPEN-MSGID
                 MSGV1  = CONV #( E_BSEG-BELNR )
                 MSGV2  = CONV #( E_BSEG-BUZEI )
                 MSGV3  = CONV #( E_BSEG-BUKRS )
                 MSGV4  = CONV #( E_BSEG-GJAHR ).
        ENDIF.

    ENDCASE.


  endmethod.


  METHOD zif_fi_compensacao~compensar.


    DATA: lt_bkdf  TYPE TABLE OF bkdf,
          lt_bkpf  TYPE TABLE OF bkpf,
          wa_bkpf  TYPE bkpf,
          lt_bsec  TYPE TABLE OF bsec,
          wa_bseg  TYPE bseg,
          lt_bsed  TYPE TABLE OF bsed,
          lt_bseg  TYPE TABLE OF bseg,
          lt_bset  TYPE TABLE OF bset,
          lit_bseg_atribui TYPE TABLE OF bseg,
          msg_text TYPE string.

*------------------------------------------------------------------------------------*
*   Variaveis Data
*------------------------------------------------------------------------------------*

    DATA: v_bldat_out TYPE c LENGTH 10,
          v_budat_out TYPE c LENGTH 10,
          v_kursf_out TYPE c LENGTH 16,
          v_wwert_out TYPE c LENGTH 10,
          v_zfbdt_out TYPE c LENGTH 10,
          v_vlr_out   TYPE c LENGTH 16.

*------------------------------------------------------------------------------------*
*   Variaveis Controle BAPI
*------------------------------------------------------------------------------------*

    DATA: it_ftpost  TYPE re_t_ex_ftpost,
          it_blntab  TYPE re_t_ex_blntab,
          it_ftclear TYPE re_t_ex_ftclear,
          it_fttax   TYPE re_t_ex_fttax.

    DATA: wl_blntab      TYPE blntab,
          wl_ftclear     TYPE ftclear,
          wl_ftpost      TYPE ftpost,
          wl_fttax       TYPE fttax,
          wl_return      TYPE bapiret2,
          wl_partida_tmp TYPE zde_partida_comp_fi.

    DATA: v_count_ft TYPE ftpost-count,
          v_vlr      TYPE bsik-dmbtr.

    CLEAR: it_blntab,   it_blntab[],
           it_ftclear,  it_ftclear[],
           it_ftpost,   it_ftpost[],
           it_fttax,    it_fttax[],
           wl_return.

    r_compensado = abap_false.

    CLEAR: e_belnr. "Documento Compensação

    me->preparar_dados_compensacao( ).

    DATA(_validado) = me->validar_compensacao( ).

    CHECK _validado EQ abap_true.

    "Start Compensação
    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_client           = sy-mandt
        i_function         = 'C'
        i_mode             = me->at_mode
        i_update           = 'S'
        i_user             = sy-uname
      EXCEPTIONS
        client_incorrect   = 1
        function_invalid   = 2
        group_name_missing = 3
        mode_invalid       = 4
        update_invalid     = 5
        OTHERS             = 6.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_fi_compensacao
        EXPORTING
          textid = VALUE #( msgid = zcx_fi_compensacao=>zcx_error_ini_comp-msgid
                            msgno = zcx_fi_compensacao=>zcx_error_ini_comp-msgno )
          msgty  = 'E'
          msgno  = zcx_fi_compensacao=>zcx_error_ini_comp-msgno
          msgid  = zcx_fi_compensacao=>zcx_error_ini_comp-msgid.
    ENDIF.

*----------------------------------------------------------------------------------------------------*
*  Preparar Dados Cabeçalho
*----------------------------------------------------------------------------------------------------*

    v_count_ft = 1.

    wl_ftpost-stype = 'K'."Header
    wl_ftpost-count = v_count_ft.  "Number of Dynpro

    "Empresa
    IF me->at_bukrs IS NOT INITIAL.
      wl_ftpost-fnam = 'BKPF-BUKRS'.
      wl_ftpost-fval = me->at_bukrs.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    "Moeda
    IF me->at_waers IS NOT INITIAL.
      wl_ftpost-fnam = 'BKPF-WAERS'.
      wl_ftpost-fval = me->at_waers.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    "Taxa Cambio
    IF me->at_kursf IS NOT INITIAL.
      WRITE me->at_kursf TO v_kursf_out.
      CONDENSE v_kursf_out NO-GAPS.

      wl_ftpost-fnam = 'BKPF-KURSF'.
      wl_ftpost-fval = v_kursf_out.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    "Data Conversão
    IF me->at_wwert IS NOT INITIAL.
      CONCATENATE  me->at_wwert+6(2) me->at_wwert+4(2) me->at_wwert(4) INTO v_wwert_out SEPARATED BY '.'.
      wl_ftpost-fnam = 'BKPF-WWERT'.
      wl_ftpost-fval = v_wwert_out.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    "Data Documento
    IF me->at_bldat IS NOT INITIAL.
      CONCATENATE  me->at_bldat+6(2) me->at_bldat+4(2) me->at_bldat(4) INTO v_bldat_out SEPARATED BY '.'.
      wl_ftpost-fnam = 'BKPF-BLDAT'.
      wl_ftpost-fval = v_bldat_out.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    "Data Lançamento
    IF me->at_budat IS NOT INITIAL.
      CONCATENATE  me->at_budat+6(2) me->at_budat+4(2) me->at_budat(4) INTO v_budat_out SEPARATED BY '.'.
      wl_ftpost-fnam = 'BKPF-BUDAT'.
      wl_ftpost-fval = v_budat_out.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    "Periodo
    IF me->at_monat IS NOT INITIAL.
      wl_ftpost-fnam = 'BKPF-MONAT'.
      wl_ftpost-fval =  me->at_monat.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    "Tipo Documento
    IF me->at_blart IS NOT INITIAL.
      wl_ftpost-fnam = 'BKPF-BLART'.
      wl_ftpost-fval = me->at_blart.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    "Referencia
    IF me->at_xblnr IS NOT INITIAL.
      wl_ftpost-fnam = 'BKPF-XBLNR'.
      wl_ftpost-fval = me->at_xblnr.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    "Texto Cabeçalho
    IF me->at_bktxt IS NOT INITIAL.
      wl_ftpost-fnam = 'BKPF-BKTXT'.
      wl_ftpost-fval = me->at_bktxt.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    "Texto Compensação
    IF me->at_augtx IS NOT INITIAL.
      wl_ftpost-fnam = 'RF05A-AUGTX'.
      wl_ftpost-fval = me->at_augtx.
      APPEND wl_ftpost TO it_ftpost.
    ENDIF.

    LOOP AT me->at_partidas_comp INTO DATA(wl_partida_comp) WHERE belnr IS NOT INITIAL.

      CLEAR: wl_ftclear.

      wl_ftclear-agkoa  = wl_partida_comp-koart.
      wl_ftclear-agkon  = wl_partida_comp-agkon.
      wl_ftclear-agums  = wl_partida_comp-umskz.
      wl_ftclear-agbuk  = wl_partida_comp-bukrs.
      wl_ftclear-xnops  = wl_partida_comp-xnops.
      wl_ftclear-selfd  = 'BELNR'.

      CONCATENATE wl_partida_comp-belnr wl_partida_comp-budat(4) wl_partida_comp-buzei INTO wl_ftclear-selvon.

      APPEND wl_ftclear TO it_ftclear.

      IF ( wl_partida_comp-vlr_residual > 0 ). "Deixar Residual na Compensação
        CLEAR: wl_partida_tmp.
        MOVE-CORRESPONDING wl_partida_comp TO wl_partida_tmp.

        me->add_partida_residual( EXPORTING i_partida_original = wl_partida_tmp
                                   CHANGING c_ftpost_t         = it_ftpost
                                            c_count_ft         = v_count_ft ).
      ENDIF.

    ENDLOOP.

    "Itens Manuais
    LOOP AT me->at_partidas_comp INTO wl_partida_comp WHERE belnr IS INITIAL.

      IF ( wl_partida_comp-dmbtr = 0 ) AND ( wl_partida_comp-dmbe2 = 0 ).
        RAISE EXCEPTION TYPE zcx_fi_compensacao
          EXPORTING
            textid = VALUE #( msgid = zcx_fi_compensacao=>zcx_lcto_valor_zerado-msgid
                              msgno = zcx_fi_compensacao=>zcx_lcto_valor_zerado-msgno )
            msgty  = 'E'
            msgno  = zcx_fi_compensacao=>zcx_lcto_valor_zerado-msgno
            msgid  = zcx_fi_compensacao=>zcx_lcto_valor_zerado-msgid.
      ENDIF.

      SELECT SINGLE koart
        FROM tbsl INTO wl_partida_comp-koart
       WHERE bschl = wl_partida_comp-bschl.

      IF ( sy-subrc NE 0 ) OR ( wl_partida_comp-bschl IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_fi_compensacao
          EXPORTING
            textid = VALUE #( msgid = zcx_fi_compensacao=>zcx_lcto_sem_chv_lcto-msgid
                              msgno = zcx_fi_compensacao=>zcx_lcto_sem_chv_lcto-msgno )
            msgty  = 'E'
            msgno  = zcx_fi_compensacao=>zcx_lcto_sem_chv_lcto-msgno
            msgid  = zcx_fi_compensacao=>zcx_lcto_sem_chv_lcto-msgid.
      ENDIF.

      IF ( wl_partida_comp-koart IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_fi_compensacao
          EXPORTING
            textid = VALUE #( msgid = zcx_fi_compensacao=>zcx_lcto_sem_tipo_conta-msgid
                              msgno = zcx_fi_compensacao=>zcx_lcto_sem_tipo_conta-msgno )
            msgty  = 'E'
            msgno  = zcx_fi_compensacao=>zcx_lcto_sem_tipo_conta-msgno
            msgid  = zcx_fi_compensacao=>zcx_lcto_sem_tipo_conta-msgid.
      ENDIF.

      ADD 1 TO v_count_ft.

      wl_ftpost-stype = 'P'.
      wl_ftpost-count = v_count_ft .

      "Chave Lançamento
      IF wl_partida_comp-bschl IS NOT INITIAL.
        wl_ftpost-fnam = 'RF05A-NEWBS'.
        wl_ftpost-fval =  wl_partida_comp-bschl.
        APPEND wl_ftpost TO it_ftpost.
      ENDIF.

      "Nr Conta
      IF wl_partida_comp-agkon IS NOT INITIAL.
        wl_ftpost-fnam = 'BSEG-HKONT'.
        wl_ftpost-fval = wl_partida_comp-agkon.
        APPEND wl_ftpost TO it_ftpost.
      ENDIF.

      "Tratamento por Moedas da Empresa
      IF ( me->at_waers_emp EQ 'BRL' ) AND  "Primeira Moeda Empresa
         ( me->at_hwae2_emp EQ 'USD' ).     "Segunda  Moeda Empresa

        IF wl_partida_comp-waers = me->at_waers_emp.
          v_vlr = abs( wl_partida_comp-dmbtr ).
        ELSE.
          v_vlr = abs( wl_partida_comp-dmbe2 ).
        ENDIF.

        WRITE: v_vlr TO v_vlr_out.
        CONDENSE v_vlr_out NO-GAPS.

        wl_ftpost-fnam = 'BSEG-WRBTR'.
        wl_ftpost-fval =  v_vlr_out.
        APPEND wl_ftpost TO it_ftpost.

        IF wl_partida_comp-waers EQ me->at_waers_emp.

          v_vlr = abs( wl_partida_comp-dmbe2 ).
          WRITE: v_vlr TO v_vlr_out.
          CONDENSE v_vlr_out NO-GAPS.

          wl_ftpost-fnam = 'BSEG-DMBE2'.
          wl_ftpost-fval =  v_vlr_out.
          APPEND wl_ftpost TO it_ftpost.

        ELSE.

          v_vlr  = abs( wl_partida_comp-dmbtr ).
          WRITE: v_vlr TO v_vlr_out.
          CONDENSE v_vlr_out NO-GAPS.

          wl_ftpost-fnam = 'BSEG-DMBTR'.
          wl_ftpost-fval =  v_vlr_out.
          APPEND wl_ftpost TO it_ftpost.

        ENDIF.

      ELSE.

        RAISE EXCEPTION TYPE zcx_fi_compensacao
          EXPORTING
            textid = VALUE #( msgid = zcx_fi_compensacao=>zcx_cenario_nao_previsto-msgid
                              msgno = zcx_fi_compensacao=>zcx_cenario_nao_previsto-msgno
                              attr1 = CONV #( '(Definição Valores Item Manual)' )
                              attr2 = CONV #( me->at_waers_emp              )
                              attr3 = CONV #( me->at_hwae2_emp              ) )
            msgty  = 'E'
            msgno  = zcx_fi_compensacao=>zcx_cenario_nao_previsto-msgno
            msgid  = zcx_fi_compensacao=>zcx_cenario_nao_previsto-msgid
            msgv1  = CONV #( '(Definição Valores Item Manual)' )
            msgv2  = CONV #( me->at_waers_emp )
            msgv3  = CONV #( me->at_hwae2_emp ).

      ENDIF.

      "Classe operação Razão Especial
      IF wl_partida_comp-umsks IS NOT INITIAL.
        wl_ftpost-fnam = 'RF05A-NEWUM'.
        wl_ftpost-fval = wl_partida_comp-umsks.
        APPEND wl_ftpost TO it_ftpost.
      ENDIF.

      CASE wl_partida_comp-koart.
        WHEN 'D' OR 'K'. "Cliente ou Fornecedor

          "Data Vencimento
          IF wl_partida_comp-zfbdt IS NOT INITIAL.

            CONCATENATE wl_partida_comp-zfbdt+6(2) wl_partida_comp-zfbdt+4(2) wl_partida_comp-zfbdt(4)
                   INTO v_zfbdt_out SEPARATED BY '.'.

            wl_ftpost-fnam = 'BSEG-ZFBDT'.
            wl_ftpost-fval = v_zfbdt_out.
            APPEND wl_ftpost TO it_ftpost.

          ENDIF.

          "Divisão
          IF wl_partida_comp-gsber IS NOT INITIAL.
            wl_ftpost-fnam = 'BSEG-GSBER'.
            wl_ftpost-fval =  wl_partida_comp-gsber.
            APPEND wl_ftpost TO it_ftpost.
          ENDIF.

          "OV/Pedido
          IF wl_partida_comp-ovped IS NOT INITIAL.
            wl_ftpost-fnam = 'BSEG-HZUON'.
            wl_ftpost-fval =  wl_partida_comp-ovped.
            APPEND wl_ftpost TO it_ftpost.
          ENDIF.

        WHEN 'S'. "Razão

          "Divisão
          IF wl_partida_comp-gsber IS NOT INITIAL.
            wl_ftpost-fnam = 'BSEG-BUPLA'.
            wl_ftpost-fval =  wl_partida_comp-gsber.
            APPEND wl_ftpost TO it_ftpost.
          ENDIF.

      ENDCASE.

      "Texto Item
      IF wl_partida_comp-sgtxt IS NOT INITIAL.
        wl_ftpost-fnam = 'BSEG-SGTXT'.
        wl_ftpost-fval = wl_partida_comp-sgtxt.
        APPEND wl_ftpost TO it_ftpost.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv                    = me->at_auglv
        i_tcode                    = me->at_tcode
        i_sgfunct                  = me->at_sgfunct
        i_no_auth                  = 'X'
        i_xsimu                    = me->at_simulacao
      IMPORTING
        e_msgid                    = wl_return-id
        e_msgno                    = wl_return-number
        e_msgty                    = wl_return-type
        e_msgv1                    = wl_return-message_v1
        e_msgv2                    = wl_return-message_v2
        e_msgv3                    = wl_return-message_v3
        e_msgv4                    = wl_return-message_v4
      TABLES
        t_blntab                   = it_blntab
        t_ftclear                  = it_ftclear
        t_ftpost                   = it_ftpost
        t_fttax                    = it_fttax
      EXCEPTIONS
        clearing_procedure_invalid = 1
        clearing_procedure_missing = 2
        table_t041a_empty          = 3
        transaction_code_invalid   = 4
        amount_format_error        = 5
        too_many_line_items        = 6
        company_code_invalid       = 7
        screen_not_found           = 8
        no_authorization           = 9
        OTHERS                     = 10.

    IF it_blntab[] IS INITIAL.

      RAISE EXCEPTION TYPE zcx_fi_compensacao
        EXPORTING
          textid = VALUE #( msgid = CONV #( wl_return-id         )
                            msgno = CONV #( wl_return-number     )
                            attr1 = CONV #( wl_return-message_v1 )
                            attr2 = CONV #( wl_return-message_v2 )
                            attr3 = CONV #( wl_return-message_v3 )
                            attr4 = CONV #( wl_return-message_v4 ) )
          msgid  = wl_return-id
          msgno  = wl_return-number
          msgty  = 'E'
          msgv1  = wl_return-message_v1
          msgv2  = wl_return-message_v2
          msgv3  = wl_return-message_v3
          msgv4  = wl_return-message_v4.

    ELSE.

      READ TABLE it_blntab INTO wl_blntab INDEX 1.

      e_belnr  = wl_blntab-belnr. "Documento Compensação

      IF e_belnr IS NOT INITIAL.
        r_compensado = abap_true.

*   CS2017000399 - Baixa Adto - Saldo Residual - Copiar informação do documento origem  US 74720 - BG - Inicio
        CLEAR wl_partida_comp.

        LOOP AT me->at_partidas_comp INTO wl_partida_comp WHERE vlr_residual > 0.


          CASE wl_partida_comp-koart.
            WHEN 'D'.
              SELECT *
                INTO TABLE @DATA(it_bsid)
                FROM bsid
                WHERE bukrs = @wl_blntab-bukrs
                AND belnr = @wl_blntab-belnr
                AND gjahr = @wl_blntab-gjahr.

              IF it_bsid[] IS NOT INITIAL.

                CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_BSID
              I_WHERE_CLAUSE = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND BUZEI = IT_FOR_ALL_ENTRIES-BUZEI|
    IMPORTING ET_BSEG = LIT_BSEG_ATRIBUI
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC <> 0 OR LINES( LIT_BSEG_ATRIBUI ) = 0.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ELSE.
  SY-DBCNT = LINES( LIT_BSEG_ATRIBUI ).
ENDIF.


              ENDIF.

            WHEN 'K'.

              SELECT *
               INTO TABLE @DATA(it_bsik)
               FROM bsik
               WHERE bukrs = @wl_blntab-bukrs
               AND belnr = @wl_blntab-belnr
               AND gjahr = @wl_blntab-gjahr.

              IF it_bsiK[] IS NOT INITIAL.

                CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_BSIK
              I_WHERE_CLAUSE = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND BUZEI = IT_FOR_ALL_ENTRIES-BUZEI|
    IMPORTING ET_BSEG = LIT_BSEG_ATRIBUI
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC <> 0 OR LINES( LIT_BSEG_ATRIBUI ) = 0.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ELSE.
  SY-DBCNT = LINES( LIT_BSEG_ATRIBUI ).
ENDIF.


              ENDIF.
            WHEN 'S'.

              SELECT *
               INTO TABLE @DATA(it_bsis)
               FROM bsis
               WHERE bukrs = @wl_blntab-bukrs
               AND belnr = @wl_blntab-belnr
               AND gjahr = @wl_blntab-gjahr.

              IF it_bsis[] IS NOT INITIAL.

                CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_BSIS
              I_WHERE_CLAUSE = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND BUZEI = IT_FOR_ALL_ENTRIES-BUZEI|
    IMPORTING ET_BSEG = LIT_BSEG_ATRIBUI
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC <> 0 OR LINES( LIT_BSEG_ATRIBUI ) = 0.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ELSE.
  SY-DBCNT = LINES( LIT_BSEG_ATRIBUI ).
ENDIF.


              ENDIF.

          ENDCASE.

          LOOP AT lit_bseg_atribui[] INTO wa_bseg.
            CLEAR wa_bkpf.
            REFRESH lt_bkpf.
            SELECT SINGLE *
              FROM bkpf
              INTO wa_bkpf
            WHERE bukrs = wa_bseg-bukrs
            AND   belnr = wa_bseg-belnr
            AND   gjahr = wa_bseg-gjahr.

            CHECK  sy-subrc EQ 0.

            APPEND wa_bkpf TO lt_bkpf.
            "CLEAR wa_bseg.
            REFRESH  lt_bseg.
           " MOVE-CORRESPONDING wa_bseg TO wa_bseg.
            wa_bseg-xref3 = wl_partida_comp-xref3.
            APPEND wa_bseg TO lt_bseg.

            CALL FUNCTION 'CHANGE_DOCUMENT'
              TABLES
                t_bkdf = lt_bkdf
                t_bkpf = lt_bkpf
                t_bsec = lt_bsec
                t_bsed = lt_bsed
                t_bseg = lt_bseg
                t_bset = lt_bset.

          ENDLOOP.


        ENDLOOP.

*   CS2017000399 - Baixa Adto - Saldo Residual - Copiar informação do documento origem  US 74720 - BG - fim


      ENDIF.

    ENDIF.


    "Fim
    CALL FUNCTION 'POSTING_INTERFACE_END'
      EXPORTING
        i_bdcimmed              = 'X'
      EXCEPTIONS
        session_not_processable = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

  ENDMETHOD.


  method ZIF_FI_COMPENSACAO~PREPARAR_DADOS_COMPENSACAO.

    DATA: WL_BSEG TYPE BSEG,
          WL_BSIK TYPE BSIK,
          WL_BSID TYPE BSID,
          WL_BSIS TYPE BSIS.

*-------------------------------------------------------------------------------------------*
*   Configurações de Compensação
*-------------------------------------------------------------------------------------------*

    "Operação de compensação
    IF ME->AT_AUGLV IS INITIAL.
      ME->AT_AUGLV =  'UMBUCHNG'.
    ENDIF.

    "Código Transação
    IF ME->AT_TCODE IS INITIAL.
      ME->AT_TCODE = 'FB05'.
    ENDIF.

    "Função alternativa para interface de contabilização FI
    IF ME->AT_SGFUNCT IS INITIAL.
      ME->AT_SGFUNCT = 'C'. "Post imediatamente.
    ENDIF.

    "Modo
    IF ME->AT_MODE IS INITIAL.
      ME->AT_MODE = 'N'.
    ENDIF.

*-------------------------------------------------------------------------------------------*
*   Partidas Compensação
*-------------------------------------------------------------------------------------------*
    LOOP AT ME->AT_PARTIDAS_COMP ASSIGNING FIELD-SYMBOL(<FS_PARTIDA_COMP>).

      IF <FS_PARTIDA_COMP>-BELNR IS NOT INITIAL.

        "Checa se Partida esta em aberto
        ME->CHECK_PARTIDA_OPEN( EXPORTING I_BUKRS = <FS_PARTIDA_COMP>-BUKRS
                                          I_BELNR = <FS_PARTIDA_COMP>-BELNR
                                          I_GJAHR = <FS_PARTIDA_COMP>-GJAHR
                                          I_BUZEI = <FS_PARTIDA_COMP>-BUZEI
                                IMPORTING E_BSEG  = WL_BSEG
                                          E_BSIK  = WL_BSIK
                                          E_BSID  = WL_BSID
                                          E_BSIS  = WL_BSIS ).

        CASE WL_BSEG-KOART.
          WHEN 'D'. "Cliente

            ME->TRANSF_DADOS_PARTIDA_D( EXPORTING I_BSID    = WL_BSID
                                         CHANGING C_PARTIDA = <FS_PARTIDA_COMP> ).
          WHEN 'K'. "Fornecedores

            ME->TRANSF_DADOS_PARTIDA_K( EXPORTING I_BSIK    = WL_BSIK
                                         CHANGING C_PARTIDA = <FS_PARTIDA_COMP> ).
          WHEN 'S'. "Contas do Razão

            ME->TRANSF_DADOS_PARTIDA_S( EXPORTING I_BSIS    = WL_BSIS
                                         CHANGING C_PARTIDA = <FS_PARTIDA_COMP> ).
        ENDCASE.

        IF <FS_PARTIDA_COMP>-TX_AUTO EQ ABAP_TRUE. "Seta Taxa Cambio Automaticamente
          ME->SET_TAXA_PARTIDA( CHANGING C_PARTIDA = <FS_PARTIDA_COMP> ).
        ENDIF.

      ENDIF.

    ENDLOOP.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_AUGLV.

    ME->AT_AUGLV = I_AUGLV.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_BUKRS.

    DATA: WL_X001 TYPE X001.

    CHECK I_BUKRS IS NOT INITIAL.

    ME->AT_BUKRS = I_BUKRS.

*-------------------------------------------------------------------------------------------*
*   Definir Moedas da Empresa
*-------------------------------------------------------------------------------------------*

    SELECT SINGLE *
      FROM T001 INTO @DATA(WL_T001)
     WHERE BUKRS EQ @ME->AT_BUKRS.

    IF ( SY-SUBRC EQ 0 ).

      CLEAR: WL_X001.

      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          I_BUKRS  = ME->AT_BUKRS
        IMPORTING
          E_X001   = WL_X001.

      ME->AT_WAERS_EMP = WL_T001-WAERS.
      ME->AT_HWAE2_EMP = WL_X001-HWAE2.
      ME->AT_HWAE3_EMP = WL_X001-HWAE3.

    ENDIF.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_DT_COMPENSACAO.

    ME->AT_BLDAT  = I_AUGDT.
    ME->AT_BUDAT  = I_AUGDT.
    ME->AT_MONAT  = I_AUGDT+4(2).

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_DT_CONVERSAO.

    ME->AT_WWERT = I_WWERT.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_DT_DOCUMENTO.

    ME->AT_BLDAT = I_BLDAT.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_DT_LCTO.

    ME->AT_BUDAT = I_BUDAT.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_MODE.

    ME->AT_MODE = I_MODE.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_MOEDA.

    ME->AT_WAERS = I_WAERS.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_PERIODO.

    ME->AT_MONAT = I_MONAT.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_REFERENCIA.

    ME->AT_XBLNR = I_XBLNR.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_SGFUNCT.

    ME->AT_SGFUNCT = I_SGFUNCT.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_SIMULACAO.

    ME->AT_SIMULACAO = I_SIMULACAO.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_TAXA_CAMBIO.

    ME->AT_KURSF = I_KURSF.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_TAXA_PARTIDA.

    TYPES: BEGIN OF TY_BSIS_CBANCO,
             BUKRS     TYPE BSIS-BUKRS,
             BELNR     TYPE BSIS-BELNR,
             GJAHR     TYPE BSIS-GJAHR,
             HKONT     TYPE BSIS-HKONT,
             DMBTR     TYPE BSIS-DMBTR,
             DMBE2     TYPE BSIS-DMBE2,
             WAERS     TYPE BSIS-WAERS,
             WRBTR     TYPE BSIS-WRBTR,
           END OF TY_BSIS_CBANCO.


    DATA: R_FDLEV_BANCO  TYPE RANGE OF SKB1-FDLEV,
          WL_FDLEV_BANCO LIKE LINE  OF R_FDLEV_BANCO.

    DATA: TG_BSIS_CBANCO TYPE TABLE OF TY_BSIS_CBANCO.

    "Monta Range Nivel Adiministração Tesouraria
    CLEAR: R_FDLEV_BANCO[].
    WL_FDLEV_BANCO-SIGN   = 'I'.
    WL_FDLEV_BANCO-OPTION = 'EQ'.
    WL_FDLEV_BANCO-LOW    = 'F0'. "Lançamento Conta bancária
    APPEND WL_FDLEV_BANCO TO R_FDLEV_BANCO.

    IF C_PARTIDA-KURSF IS INITIAL.

      "Tratamento por Moedas da Empresa
      IF ( ME->AT_WAERS_EMP EQ 'BRL' ) AND  "Primeira Moeda Empresa
         ( ME->AT_HWAE2_EMP EQ 'USD' ).     "Segunda  Moeda Empresa

        IF ( C_PARTIDA-DMBTR > 0 ) AND  ( C_PARTIDA-DMBE2 > 0 ).
          TRY.
            C_PARTIDA-KURSF = C_PARTIDA-DMBTR / C_PARTIDA-DMBE2.
          CATCH CX_SY_ARITHMETIC_OVERFLOW.
          ENDTRY.
        ENDIF.

        "Checa se Possui Partida de Banco no Lançamento.
        CLEAR: TG_BSIS_CBANCO[].

        SELECT BSIS~BUKRS BSIS~BELNR BSIS~GJAHR BSIS~HKONT BSIS~DMBTR BSIS~DMBE2
               BSIS~WAERS BSIS~WRBTR
               APPENDING CORRESPONDING FIELDS OF TABLE TG_BSIS_CBANCO
          FROM BSIS INNER JOIN SKB1 ON BSIS~BUKRS = SKB1~BUKRS "#EC CI_DB_OPERATION_OK[2431747]
                                   AND BSIS~HKONT = SKB1~SAKNR
         WHERE BSIS~BUKRS EQ C_PARTIDA-BUKRS
           AND BSIS~GJAHR EQ C_PARTIDA-GJAHR
           AND BSIS~BELNR EQ C_PARTIDA-BELNR
           AND SKB1~FDLEV IN R_FDLEV_BANCO.

        LOOP AT TG_BSIS_CBANCO INTO DATA(WL_BSIS_BANCO) WHERE DMBTR > 0
                                                          AND DMBE2 > 0.
          TRY.
            C_PARTIDA-KURSF = WL_BSIS_BANCO-DMBTR / WL_BSIS_BANCO-DMBE2.
          CATCH CX_SY_ARITHMETIC_OVERFLOW.
          ENDTRY.
        ENDLOOP.

      ELSE.

        RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_CENARIO_NAO_PREVISTO-MSGID
                              MSGNO = ZCX_FI_COMPENSACAO=>ZCX_CENARIO_NAO_PREVISTO-MSGNO
                              ATTR1 = CONV #( '(Definição Taxa Automática)' )
                              ATTR2 = CONV #( ME->AT_WAERS_EMP              )
                              ATTR3 = CONV #( ME->AT_HWAE2_EMP              ) )
              MSGTY  = 'E'
              MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_CENARIO_NAO_PREVISTO-MSGNO
              MSGID  = ZCX_FI_COMPENSACAO=>ZCX_CENARIO_NAO_PREVISTO-MSGID
              MSGV1  = CONV #(  '(Definição Taxa Automática)' )
              MSGV2  = CONV #(  ME->AT_WAERS_EMP              )
              MSGV3  = CONV #(  ME->AT_HWAE2_EMP              ).

      ENDIF.

    ENDIF.


  endmethod.


  method ZIF_FI_COMPENSACAO~SET_TCODE.

    ME->AT_TCODE = I_TCODE.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_TEXTO_CAB_DOC.

    ME->AT_BKTXT = I_BKTXT.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_TEXTO_COMPENSACAO.

     ME->AT_AUGTX = I_AUGTX.

  endmethod.


  method ZIF_FI_COMPENSACAO~SET_TP_DOCUMENTO.

    ME->AT_BLART = I_BLART.

  endmethod.


  METHOD zif_fi_compensacao~transf_dados_partida_d.

    c_partida-koart           = 'D'.
    c_partida-agkon           = i_bsid-kunnr.
    c_partida-bldat           = i_bsid-bldat.
    c_partida-budat           = i_bsid-budat.
    c_partida-waers           = i_bsid-waers.
    c_partida-dmbtr           = i_bsid-dmbtr.
    c_partida-dmbe2           = i_bsid-dmbe2.
    c_partida-dmbe3           = i_bsid-dmbe3.
    c_partida-bschl           = i_bsid-bschl.
    c_partida-umsks           = i_bsid-umsks.
    c_partida-umskz           = i_bsid-umskz.
    c_partida-shkzg           = i_bsid-shkzg.
    c_partida-gsber           = i_bsid-gsber.
    c_partida-sgtxt           = i_bsid-sgtxt.
    c_partida-sgtxt_residual  = i_bsid-sgtxt.
    c_partida-zfbdt           = i_bsid-zfbdt.
    c_partida-zbd1t           = i_bsid-zbd1t.
    c_partida-zbd2t           = i_bsid-zbd2t.
    c_partida-zbd3t           = i_bsid-zbd3t.
    c_partida-kidno           = i_bsid-kidno.
    c_partida-zuonr           = i_bsid-zuonr.
    c_partida-xblnr           = i_bsid-xblnr.
    c_partida-blart           = i_bsid-blart.
    c_partida-ovped           = i_bsid-vbel2.
    c_partida-itmop           = i_bsid-posn2.
    c_partida-posn2           = i_bsid-posn2.
    c_partida-zterm           = i_bsid-zterm.
    c_partida-anln1           = i_bsid-anln1.
    c_partida-anln2           = i_bsid-anln2.

  ENDMETHOD.


  METHOD zif_fi_compensacao~transf_dados_partida_k.

    c_partida-koart           = 'K'.
    c_partida-agkon           = i_bsik-lifnr.
    c_partida-bldat           = i_bsik-bldat.
    c_partida-budat           = i_bsik-budat.
    c_partida-waers           = i_bsik-waers.
    c_partida-dmbtr           = i_bsik-dmbtr.
    c_partida-dmbe2           = i_bsik-dmbe2.
    c_partida-dmbe3           = i_bsik-dmbe3.
    c_partida-bschl           = i_bsik-bschl.
    c_partida-umsks           = i_bsik-umsks.
    c_partida-umskz           = i_bsik-umskz.
    c_partida-shkzg           = i_bsik-shkzg.
    c_partida-gsber           = i_bsik-gsber.
    c_partida-sgtxt           = i_bsik-sgtxt.
    c_partida-sgtxt_residual  = i_bsik-sgtxt.
    c_partida-zfbdt           = i_bsik-zfbdt.
    c_partida-zbd1t           = i_bsik-zbd1t.
    c_partida-zbd2t           = i_bsik-zbd2t.
    c_partida-zbd3t           = i_bsik-zbd3t.
    c_partida-kidno           = i_bsik-kidno.
    c_partida-zuonr           = i_bsik-zuonr.
    c_partida-xblnr           = i_bsik-xblnr.
    c_partida-blart           = i_bsik-blart.
    c_partida-ovped           = i_bsik-ebeln.
    c_partida-itmop           = i_bsik-ebelp.
    c_partida-ebelp           = i_bsik-ebelp.
    c_partida-zterm           = i_bsik-zterm.
    c_partida-anln1           = i_bsik-anln1.
    c_partida-anln2           = i_bsik-anln2.

  ENDMETHOD.


  METHOD zif_fi_compensacao~transf_dados_partida_s.

    c_partida-koart           = 'S'.
    c_partida-agkon           = i_bsis-hkont.
    c_partida-bldat           = i_bsis-bldat.
    c_partida-budat           = i_bsis-budat.
    c_partida-waers           = i_bsis-waers.
    c_partida-dmbtr           = i_bsis-dmbtr.
    c_partida-dmbe2           = i_bsis-dmbe2.
    c_partida-dmbe3           = i_bsis-dmbe3.
    c_partida-bschl           = i_bsis-bschl.
    c_partida-shkzg           = i_bsis-shkzg.
    c_partida-gsber           = i_bsis-gsber.
    c_partida-sgtxt           = i_bsis-sgtxt.
    c_partida-sgtxt_residual  = i_bsis-sgtxt.
    c_partida-zfbdt           = i_bsis-zfbdt.
    c_partida-kidno           = i_bsis-kidno.
    c_partida-zuonr           = i_bsis-zuonr.
    c_partida-xblnr           = i_bsis-xblnr.
    c_partida-blart           = i_bsis-blart.

  ENDMETHOD.


  method ZIF_FI_COMPENSACAO~VALIDAR_COMPENSACAO.

    R_VALIDADO = ABAP_FALSE.

*-------------------------------------------------------------------------------------------*
*   Validação Cabeçalho Compensação
*-------------------------------------------------------------------------------------------*

    IF ME->AT_BUKRS IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BUKRS-MSGID
                            MSGNO = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BUKRS-MSGNO )
          MSGTY  = 'E'
          MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BUKRS-MSGNO
          MSGID  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BUKRS-MSGID.
    ENDIF.

    IF ME->AT_BUDAT IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BUDAT-MSGID
                            MSGNO = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BUDAT-MSGNO )
          MSGTY  = 'E'
          MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BUDAT-MSGNO
          MSGID  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BUDAT-MSGID.
    ENDIF.

    IF ME->AT_BLDAT IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BLDAT-MSGID
                            MSGNO = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BLDAT-MSGNO )
          MSGTY  = 'E'
          MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BLDAT-MSGNO
          MSGID  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BLDAT-MSGID.
    ENDIF.

    IF ME->AT_WAERS IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_WAERS-MSGID
                            MSGNO = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_WAERS-MSGNO )
          MSGTY  = 'E'
          MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_WAERS-MSGNO
          MSGID  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_WAERS-MSGID.
    ENDIF.

*    IF ME->AT_KURSF IS INITIAL.
*      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_KURSF-MSGID
*                            MSGNO = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_KURSF-MSGNO )
*          MSGTY  = 'E'
*          MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_KURSF-MSGNO
*          MSGID  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_KURSF-MSGID.
*    ENDIF.

    IF ME->AT_BLART IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BLART-MSGID
                            MSGNO = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BLART-MSGNO )
          MSGTY  = 'E'
          MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BLART-MSGNO
          MSGID  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_BLART-MSGID.
    ENDIF.

    IF ME->AT_MONAT IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_FI_COMPENSACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_MONAT-MSGID
                            MSGNO = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_MONAT-MSGNO )
          MSGTY  = 'E'
          MSGNO  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_MONAT-MSGNO
          MSGID  = ZCX_FI_COMPENSACAO=>ZCX_OBG_INF_MONAT-MSGID.
    ENDIF.

*-------------------------------------------------------------------------------------------*
*   Validação Partidas Compensação
*-------------------------------------------------------------------------------------------*
    LOOP AT ME->AT_PARTIDAS_COMP INTO DATA(_WL_PARTIDAS_COMP).



    ENDLOOP.

    R_VALIDADO = ABAP_TRUE.

  endmethod.
ENDCLASS.
