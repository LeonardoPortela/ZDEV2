class ZCL_DOC_FISCAL_FT_ENTRADA definition
  public
  final
  create public .

public section.

  interfaces ZIF_DOC_FISCAL_FT_ENTRADA .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: CK_ESTORNANDO TYPE CHAR01.

ENDCLASS.



CLASS ZCL_DOC_FISCAL_FT_ENTRADA IMPLEMENTATION.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_AVISO_RECEBIMENTO.

    DATA: LC_XBLNR TYPE XBLNR1,
          L_SERIE  TYPE C LENGTH 3.

    DATA: RVGBEL TYPE RANGE OF VGBEL.
    IF I_EBELN IS NOT INITIAL.
      RVGBEL = VALUE #( OPTION = 'EQ' SIGN = 'I' ( LOW = I_EBELN ) ).
    ENDIF.

    DATA: RVGPOS TYPE RANGE OF VGPOS.
    IF I_EBELP IS NOT INITIAL.
      RVGPOS = VALUE #( OPTION = 'EQ' SIGN = 'I' ( LOW = I_EBELP ) ).
    ENDIF.

    R_INSTANCIA = ME.

    CLEAR: E_LIKP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = I_NUMERO
      IMPORTING
        OUTPUT = LC_XBLNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = I_SERIE
      IMPORTING
        OUTPUT = L_SERIE.

    CONCATENATE LC_XBLNR L_SERIE INTO LC_XBLNR SEPARATED BY '-'.

    SELECT * INTO TABLE @DATA(IT_LIKP)
      FROM LIKP AS M
     WHERE LIFNR EQ @I_EMISSOR
       AND XBLNR EQ @LC_XBLNR
       AND EXISTS ( SELECT * FROM LIPS AS L WHERE L~VBELN EQ M~VBELN
                                              AND L~VGBEL IN @RVGBEL
                                              AND L~VGPOS IN @RVGPOS ).

    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_LIKP INDEX 1 INTO E_LIKP.
    ELSE.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_AVISO_RECEBIMENTO-MSGID
                            MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_AVISO_RECEBIMENTO-MSGNO
                            ATTR1 = CONV #( I_EMISSOR )
                            ATTR2 = CONV #( LC_XBLNR  ) )
          MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_AVISO_RECEBIMENTO-MSGID
          MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_AVISO_RECEBIMENTO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_EMISSOR )
          MSGV2  = CONV #( LC_XBLNR ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_CK_ADIANTAMENTO.

    R_INSTANCIA = ME.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0103)
      FROM ZLEST0103
     WHERE BUKRS  EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BUKRS
       AND BRANCH EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BRANCH.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_MARGEM_ADIANTAMENTO-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_MARGEM_ADIANTAMENTO-MSGNO
                          ATTR1 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BUKRS  )
                          ATTR2 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BRANCH ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_MARGEM_ADIANTAMENTO-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_MARGEM_ADIANTAMENTO-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BUKRS )
        MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BRANCH ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_CK_CONDICAO_ZIOF.

    R_INSTANCIA = ME.

    TRY .

        ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
          )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE
          )->CK_PARCEIRO_LOCAL_NEGOCIO(
          ).

      CATCH ZCX_PARCEIROS.    " .
        EXIT.
    ENDTRY.

    SELECT SINGLE *
      FROM A917
      INTO @DATA(WL_A917)
     WHERE KAPPL EQ 'F'
       AND KSCHL EQ 'ZIOF'
       AND MATNR EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL
       AND TDLNR EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE
       AND KFRST EQ @SPACE
       AND DATBI GE @SY-DATUM.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_IOF-MSGID
                            MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_IOF-MSGNO
                            ATTR1 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL )
                            ATTR2 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE ) )
          MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_IOF-MSGID
          MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_IOF-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL )
          MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE ).
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_KONP)
      FROM KONP
     WHERE KNUMH    EQ @WL_A917-KNUMH
       AND LOEVM_KO EQ @ABAP_FALSE.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGID
                            MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGNO
                            ATTR1 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL )
                            ATTR2 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE ) )
          MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGID
          MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL )
          MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE ).
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_CK_CONDICAO_ZSEG.

    R_INSTANCIA = ME.

    TRY .

        ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
          )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE
          )->CK_PARCEIRO_LOCAL_NEGOCIO(
          ).

      CATCH ZCX_PARCEIROS.    " .
        EXIT.
    ENDTRY.

    SELECT SINGLE *
      FROM A917
      INTO @DATA(WL_A917)
     WHERE KAPPL EQ 'F'
       AND KSCHL EQ 'ZSEG'
       AND MATNR EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL
       AND TDLNR EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE
       AND KFRST EQ @SPACE
       AND DATBI GE @SY-DATUM.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGID
                            MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGNO
                            ATTR1 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL )
                            ATTR2 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE ) )
          MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGID
          MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL )
          MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE ).
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_KONP)
      FROM KONP
     WHERE KNUMH    EQ @WL_A917-KNUMH
       AND LOEVM_KO EQ @ABAP_FALSE.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGID
                            MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGNO
                            ATTR1 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL )
                            ATTR2 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE ) )
          MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGID
          MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_SEGURO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL )
          MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE ).
    ENDIF.


  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_CK_FINALIZADO.

    R_INSTANCIA = ME.

    IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC NE '99'.



    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_CK_ITINERARIO_PEDAGIO.

    R_INSTANCIA = ME.

    SELECT SINGLE * INTO @DATA(WA_LIKP)
      FROM LIKP
     WHERE VBELN EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0027)
      FROM ZLEST0027
     WHERE ROUTE EQ @WA_LIKP-ROUTE.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DETERMINACAO_PEDAGIO-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DETERMINACAO_PEDAGIO-MSGNO
                          ATTR1 = CONV #( WA_LIKP-ROUTE  ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DETERMINACAO_PEDAGIO-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DETERMINACAO_PEDAGIO-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( WA_LIKP-ROUTE ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_CK_ITINERARIO_RELEVANTE.

    R_INSTANCIA = ME.

    TRY .

        ZCL_ITINERARIO=>ZIF_ITINERARIO~GET_INSTANCE(
          )->GET_ITINERARIO_RELEVANTE(
          EXPORTING
            I_COD_LOC_COLETA  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA    " Nº conta do fornecedor
            I_COD_LOC_ENTREGA = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA    " Nº cliente
          ).

      CATCH ZCX_ITINERARIO.    " .
        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ITINERARIO_IRRELEVANTE-MSGID
                              MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ITINERARIO_IRRELEVANTE-MSGNO )
            MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ITINERARIO_IRRELEVANTE-MSGID
            MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ITINERARIO_IRRELEVANTE-MSGNO
            MSGTY  = 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_DOC_FISCAL_FT_ENTRADA.

    R_INSTANCIA = ME.

    E_AVISO_RECEBIMENTO  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO.
    E_AVISO_RECEB_ITEMS  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS.
    E_ORDEM_CARREGAMENTO = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO.
    E_NOTA_FISCAL        = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL.
    E_DACTE              = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_DACTE.
    E_DECLARACAO         = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_DECLARACAO.
    E_CONTRATO_VIAGEM    = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_CONTRATO_VIAGEM.
    E_MDFE               = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_MDFE.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_ENTRONCAMENTO_LC_ENTREGA.

    SELECT SINGLE * INTO E_TVKN
      FROM TVKN
     WHERE KUNNR EQ ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_LC_SEM_ENTRONCAMENTO-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_LC_SEM_ENTRONCAMENTO-MSGNO
                          ATTR1 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_LC_SEM_ENTRONCAMENTO-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_LC_SEM_ENTRONCAMENTO-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = SY-MSGID
                          MSGNO = SY-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = SY-MSGID
        MSGNO  = SY-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL_STRING.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_INFO_TIPO_TRANSPORTE.

    R_INSTANCIA = ME.

    SELECT SINGLE * INTO E_TVTK
      FROM TVTK
     WHERE SHTYP EQ I_SHTYP.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_TIPO_TRANSPORTE-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_TIPO_TRANSPORTE-MSGNO
                          ATTR1 = CONV #( I_SHTYP ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_TIPO_TRANSPORTE-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_TIPO_TRANSPORTE-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( I_SHTYP ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_INFO_VEICULO.

    SELECT SINGLE * INTO E_VEICULO
      FROM ZLEST0002
     WHERE PC_VEICULO EQ I_PLACA.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_VEICULO_SEM_CADASTRO-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_VEICULO_SEM_CADASTRO-MSGNO
                          ATTR1 = CONV #( I_PLACA ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_VEICULO_SEM_CADASTRO-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_VEICULO_SEM_CADASTRO-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( I_PLACA ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_INSTANCE.

    IF ZIF_DOC_FISCAL_FT_ENTRADA~INSTANCE IS NOT BOUND.
      CREATE OBJECT ZIF_DOC_FISCAL_FT_ENTRADA~INSTANCE TYPE ZCL_DOC_FISCAL_FT_ENTRADA.
      R_INSTANCIA = ZIF_DOC_FISCAL_FT_ENTRADA~INSTANCE.
    ELSE.
      R_INSTANCIA = ZIF_DOC_FISCAL_FT_ENTRADA~INSTANCE.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_ITINERARIO.

    R_INSTANCIA = ME.

    SELECT SINGLE * INTO @DATA(WA_LIKP)
      FROM LIKP
     WHERE VBELN EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN.

    SELECT SINGLE * INTO E_TVRO
      FROM TVRO
     WHERE ROUTE = WA_LIKP-ROUTE.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_ITINERARIO-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_ITINERARIO-MSGNO
                          ATTR1 = CONV #( WA_LIKP-ROUTE ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_ITINERARIO-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_ITINERARIO-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( WA_LIKP-ROUTE ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_LOCAL_COLETA.

    R_INSTANCIA = ME.

    SELECT SINGLE * INTO E_LFA1
      FROM LFA1
     WHERE LIFNR EQ ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_LOCAL_COLETA-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_LOCAL_COLETA-MSGNO
                          ATTR1 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_LOCAL_COLETA-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_LOCAL_COLETA-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_LOCAL_ENTREGA_FORNECEDOR.

    SELECT SINGLE STCD1 INTO @DATA(LC_STCD1)
      FROM KNA1
     WHERE KUNNR EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA.

    SELECT SINGLE LIFNR FROM LFA1 INTO E_LIFNR WHERE STCD1 = LC_STCD1 AND STCD1 NE SPACE.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_LC_SEM_FORNECDOR-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_LC_SEM_FORNECDOR-MSGNO
                          ATTR1 = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_LC_SEM_FORNECDOR-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_LC_SEM_FORNECDOR-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_NEW_ID_DOC_AGRUPADO.

    R_INSTANCIA = ME.

    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        OBJECT           = 'ZFRETEENAG'
      EXCEPTIONS
        FOREIGN_LOCK     = 1
        OBJECT_NOT_FOUND = 2
        SYSTEM_FAILURE   = 3
        OTHERS           = 4.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZFRETEENAG'
        QUANTITY                = '00000000000000000001'
        IGNORE_BUFFER           = 'X'
      IMPORTING
        NUMBER                  = E_ID_AGRUPA
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

* Desbloqueia o objeto de numeração
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        OBJECT           = 'ZFRETEENAG'
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_ORDEM_CARREGAMENTO.

    DATA: LC_FILTRO_ORD_CARREGA TYPE ZDE_FILTRO_ZSDT0001OD,
          E_MESSAGEM_ERRO       TYPE STRING.

    R_INSTANCIA = ME.

    LC_FILTRO_ORD_CARREGA-IIDORDEM = VALUE #( SIGN = 'I' OPTION = 'EQ' ( HIGH = I_ID_ORDEM LOW = I_ID_ORDEM ) ).

    TRY .
        E_ORDEM_CARREGAMENTO = ZCL_ORDEM_CARREGAMENTO=>GET_ORDEM_CARREGAMENTO( I_FILTRO = LC_FILTRO_ORD_CARREGA ).
      CATCH ZCX_ORDEM_CARREGAMENTO INTO DATA(EX_ORD_CARREGAMENTO).    "
        MESSAGE ID EX_ORD_CARREGAMENTO->MSGID TYPE 'S' NUMBER EX_ORD_CARREGAMENTO->MSGNO
           WITH EX_ORD_CARREGAMENTO->MSGV1 EX_ORD_CARREGAMENTO->MSGV2 EX_ORD_CARREGAMENTO->MSGV3 EX_ORD_CARREGAMENTO->MSGV4.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_PEDIDO_COMPRA.

    DATA: LC_PEDIDO TYPE REF TO ZCL_PEDIDO_COMPRA,
          LC_FILTRO TYPE ZDE_FILTRO_PEDIDO_COMPRA,
          IT_EKKO   TYPE ZDE_EKKO_T.

    R_INSTANCIA = ME.

    CLEAR: E_INFO_PEDIDO.

    "Buscar Centro a Fixar
    SELECT SINGLE * INTO @DATA(WA_AFIXAR)
      FROM ZSDT_DEPARA_CEN
     WHERE CENTRO_REAL       EQ @I_WERKS
       AND TP_CENTRO_VIRTUAL EQ '1'.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID    = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_CENTRO_AFIXAR-MSGID
                               MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_CENTRO_AFIXAR-MSGNO
                               ATTR1 = CONV #( I_WERKS )
                               ATTR2 = 'ZSDT0036' )
          MSGID     = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_CENTRO_AFIXAR-MSGID
          MSGNO     = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_CENTRO_AFIXAR-MSGNO
          MSGTY     = 'E'
          MSGV1     = CONV #( I_WERKS )
          MSGV2     = 'ZSDT0036'
          TRANSACAO = 'ZSDT0036'.
    ENDIF.

    CREATE OBJECT LC_PEDIDO.
    LC_FILTRO-IBUKRS = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = I_BUKRS   HIGH = I_BUKRS  ) ).
    LC_FILTRO-ILIFNR = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = I_LIFNR   HIGH = I_LIFNR  ) ).
    LC_FILTRO-IMATNR = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = I_MATNR   HIGH = I_MATNR  ) ).
    LC_FILTRO-ICHARG = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = I_CHARG   HIGH = I_CHARG  ) ).
    LC_FILTRO-IMWSKZ = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = I_ID_IVA  HIGH = I_ID_IVA ) ).
    LC_FILTRO-IWERKS = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = WA_AFIXAR-CENTROV_1 HIGH = WA_AFIXAR-CENTROV_1 ) ).
    LC_FILTRO-IBSTYP = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = 'F'     HIGH = 'F'      ) ).
    LC_FILTRO-IBSART = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = 'ZGR'   HIGH = 'ZGR'    ) ).
    LC_FILTRO-IEKORG = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = 'OC01'  HIGH = 'OC01'   ) ).
    LC_FILTRO-IEKGRP = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = 'G01'   HIGH = 'G01'    ) ).
    LC_FILTRO-IFRGRL = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = SPACE   HIGH = SPACE    ) ).
    LC_FILTRO-IEBELP = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = '00010' HIGH = '00010'  ) ).
    LC_FILTRO-IBSTAE = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = '0004'  HIGH = '0004'   ) ).
    LC_FILTRO-ILGORT = VALUE #( SIGN = 'I'  OPTION = 'EQ' ( LOW = I_LGORT  HIGH = I_LGORT ) ).

    IF LC_PEDIDO->ZIF_PESQUISA~PESQUISAR( EXPORTING I_FILTROS  = LC_FILTRO IMPORTING E_REGISTROS = IT_EKKO ) NE ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_PEDIDO_COMPRA-MSGID
                            MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_PEDIDO_COMPRA-MSGNO
                            ATTR1 = CONV #( I_LIFNR )
                            ATTR2 = CONV #( I_CHARG )
                            ATTR3 = CONV #( I_MATNR )
                            ATTR4 = 'ZGR' )
          MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_PEDIDO_COMPRA-MSGID
          MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_PEDIDO_COMPRA-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_LIFNR )
          MSGV2  = CONV #( I_CHARG )
          MSGV3  = CONV #( I_MATNR )
          MSGV4  = 'ZGR'.
    ENDIF.

    READ TABLE IT_EKKO INDEX 1 INTO E_INFO_PEDIDO-EKKO.

    SELECT SINGLE * INTO E_INFO_PEDIDO-EKPO
      FROM EKPO
     WHERE EBELN EQ E_INFO_PEDIDO-EKKO-EBELN
       AND EBELP EQ '00010'.

    SELECT SINGLE * INTO E_INFO_PEDIDO-EKET
      FROM EKET
     WHERE EBELN EQ E_INFO_PEDIDO-EKKO-EBELN
       AND EBELP EQ '00010'.

  ENDMETHOD.


  METHOD zif_doc_fiscal_ft_entrada~get_preco_frete_entrada.

    DATA: lc_data    TYPE c LENGTH 10,
          l_add01(4) TYPE n.

    e_qtd_precos_encontrados = 0.
    e_tarifa = 0.

    SELECT SINGLE * INTO @DATA(wa_lfa1)
      FROM lfa1
     WHERE lifnr EQ @i_cod_loc_coleta.

    SELECT SINGLE * INTO @DATA(wa_kna1)
      FROM kna1
     WHERE kunnr EQ @i_cod_loc_entrega.

    SELECT SINGLE * INTO @DATA(wa_likp)
      FROM likp
     WHERE vbeln EQ @i_aviso.

    SELECT SINGLE * INTO @DATA(wa_ekko)
      FROM ekko
     WHERE ebeln EQ @i_ebeln.

    SELECT SINGLE shtyp INTO @DATA(lc_shtyp)
      FROM zsdt0011
     WHERE bsart EQ @wa_ekko-bsart
       AND bsart NE @space.

    IF sy-subrc IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
        EXPORTING
          textid = VALUE #( msgid = zcx_doc_fiscal_ft_entrada=>zcx_sem_tipo_pedido_transp-msgid
                            msgno = zcx_doc_fiscal_ft_entrada=>zcx_sem_tipo_pedido_transp-msgno
                            attr1 = CONV #( wa_ekko-bsart ) )
          msgid  = zcx_doc_fiscal_ft_entrada=>zcx_sem_tipo_pedido_transp-msgid
          msgno  = zcx_doc_fiscal_ft_entrada=>zcx_sem_tipo_pedido_transp-msgno
          msgty  = 'E'
          msgv1  = CONV #( wa_ekko-bsart ).

    ENDIF.

    DATA(lc_data_referecia) = COND #( LET clet = wa_likp-lddat IN WHEN clet IS INITIAL THEN i_dt_referecia ELSE wa_likp-lddat ).
    IF lc_data_referecia IS INITIAL.
      lc_data_referecia = sy-datum.
    ENDIF.

    SELECT SINGLE * INTO @DATA(lc_veiculo)
      FROM zlest0002
     WHERE pc_veiculo EQ @i_placa_cav.

    IF sy-subrc IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
        EXPORTING
          textid = VALUE #( msgid = zcx_doc_fiscal_ft_entrada=>zcx_veiculo_sem_cadastro-msgid
                            msgno = zcx_doc_fiscal_ft_entrada=>zcx_veiculo_sem_cadastro-msgno
                            attr1 = CONV #( i_placa_cav ) )
          msgid  = zcx_doc_fiscal_ft_entrada=>zcx_veiculo_sem_cadastro-msgid
          msgno  = zcx_doc_fiscal_ft_entrada=>zcx_veiculo_sem_cadastro-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_placa_cav ).
    ENDIF.

    IF lc_veiculo-agregado EQ 1.
      DATA(lc_add01) = '0000000001'.
    ELSE.
      lc_add01 = '0000000002'.
    ENDIF.

    WRITE lc_data_referecia TO lc_data.

    e_message = |Dt. Referência: { lc_data }, Tipo Transporte: { lc_shtyp }, Agente frete: { i_agente_frete }, Itinerário: { wa_likp-route }|.
    e_message = |{ e_message }, Suplem.1: { lc_add01 }, Zona de partida: { wa_lfa1-lzone }, Zona de chegada: { wa_kna1-lzone }|.
    e_message = |{ e_message }, Material: { i_material }|.

    "Vlr Frete
    SELECT SINGLE * INTO @DATA(wa_a900)
      FROM a900
     WHERE shtyp EQ @lc_shtyp
       AND tdlnr EQ @i_agente_frete
       AND route EQ @wa_likp-route
       AND add01 EQ @lc_add01
       AND datab LE @lc_data_referecia
       AND datbi GE @lc_data_referecia.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE * INTO @DATA(wa_konp)
        FROM konp
       WHERE knumh    EQ @wa_a900-knumh
         AND loevm_ko EQ @abap_false.
      IF sy-subrc IS INITIAL.
        e_tarifa = wa_konp-kbetr.
        ADD 1 TO e_qtd_precos_encontrados.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_a910)
      FROM a910
     WHERE shtyp  EQ @lc_shtyp
       AND tdlnr  EQ @i_agente_frete
       AND lzonea EQ @wa_lfa1-lzone
       AND lzonez EQ @wa_kna1-lzone
       AND datab  LE @lc_data_referecia
       AND datbi  GE @lc_data_referecia.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh EQ wa_a910-knumh AND loevm_ko EQ abap_false.
      IF sy-subrc IS INITIAL.
        e_tarifa = wa_konp-kbetr.
        ADD 1 TO e_qtd_precos_encontrados.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_a911)
      FROM a911
     WHERE shtyp EQ @lc_shtyp
       AND tdlnr EQ @i_agente_frete
       AND route EQ @wa_likp-route
       AND datab LE @lc_data_referecia
       AND datbi GE @lc_data_referecia.

    IF sy-subrc IS INITIAL.
      CLEAR: wa_konp.
      SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh EQ wa_a911-knumh AND loevm_ko EQ abap_false.
      IF sy-subrc IS INITIAL.
        e_tarifa = wa_konp-kbetr.
        ADD 1 TO e_qtd_precos_encontrados.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_a915)
      FROM a915
     WHERE shtyp  EQ @lc_shtyp
       AND tdlnr  EQ @i_agente_frete
       AND lzonea EQ @wa_lfa1-lzone
       AND lzonez EQ @wa_kna1-lzone
       AND add01  EQ @lc_add01
       AND datab  LE @lc_data_referecia
       AND datbi  GE @lc_data_referecia.

    IF sy-subrc IS INITIAL.
      CLEAR: wa_konp.
      SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh EQ wa_a915-knumh AND loevm_ko EQ abap_false.
      IF sy-subrc IS INITIAL.
        e_tarifa = wa_konp-kbetr.
        ADD 1 TO e_qtd_precos_encontrados.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_a918)
      FROM a918
     WHERE shtyp  EQ @lc_shtyp
       AND tdlnr  EQ @i_agente_frete
       AND matnr  EQ @i_material
       AND lzonea EQ @wa_lfa1-lzone
       AND lzonez EQ @wa_kna1-lzone
       AND add01  EQ @lc_add01
       AND datab  LE @lc_data_referecia
       AND datbi  GE @lc_data_referecia.

    IF sy-subrc IS INITIAL.
      CLEAR: wa_konp.
      SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh EQ wa_a918-knumh AND loevm_ko EQ abap_false.
      IF sy-subrc IS INITIAL.
        e_tarifa = wa_konp-kbetr.
        ADD 1 TO e_qtd_precos_encontrados.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_a919)
      FROM a919
     WHERE shtyp  EQ @lc_shtyp
       AND tdlnr  EQ @i_agente_frete
       AND matnr  EQ @i_material
       AND lzonea EQ @wa_lfa1-lzone
       AND lzonez EQ @wa_kna1-lzone
       AND datab  LE @lc_data_referecia
       AND datbi  GE @lc_data_referecia.

    IF sy-subrc IS INITIAL.
      CLEAR: wa_konp.
      SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh EQ wa_a919-knumh AND loevm_ko EQ abap_false.
      IF sy-subrc IS INITIAL.
        e_tarifa = wa_konp-kbetr.
        ADD 1 TO e_qtd_precos_encontrados.
      ENDIF.
    ENDIF.

*-CS2019001158 - Jaime Tassoni - 19.11.2020 - inicio
    IF i_viagem_id IS NOT INITIAL.
      l_add01 = lc_add01.

      SELECT SINGLE * INTO @DATA(wa_a942)
        FROM a942
       WHERE shtyp     EQ @lc_shtyp
         AND sdabw     EQ @l_add01
         AND id_viagem EQ @i_viagem_id
         AND kfrst     EQ @space
         AND datab     LE @lc_data_referecia
         AND datbi     GE @lc_data_referecia.

      IF sy-subrc IS INITIAL.
        CLEAR: wa_konp.
        SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh EQ wa_a942-knumh AND loevm_ko EQ abap_false.
        IF sy-subrc IS INITIAL.
          e_tarifa = wa_konp-kbetr.
          MOVE 1 TO e_qtd_precos_encontrados.
        ENDIF.
      ENDIF.
    ENDIF.
*-CS2019001158 - Jaime Tassoni - 19.11.2020 - inicio

  ENDMETHOD.


  METHOD zif_doc_fiscal_ft_entrada~get_preco_tarifa_frete.

    DATA: lc_qtd_precos_encontrados TYPE i.

    r_instancia = me.

*-CS2019001158 - Jaime Tassoni - 19.11.2020 - inicio
    SELECT *
      FROM zlest0185
      INTO @DATA(wa_zlest0185)
        UP TO 1 ROWS
     WHERE id_ordem = @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-id_ordem.
    ENDSELECT.
*-CS2019001158 - Jaime Tassoni - 19.11.2020 - inicio

    e_tarifa = 0.
    lc_qtd_precos_encontrados = 0.

    zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~get_preco_frete_entrada(
      EXPORTING
        i_aviso                   = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln
        i_cod_loc_coleta          = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-cod_loc_coleta
        i_cod_loc_entrega         = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-cod_loc_entrega
        i_ebeln                   = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebeln
        i_placa_cav               = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-placa_cav
        i_dt_referecia            = sy-datum
        i_agente_frete            = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-agente_frete
        i_material                = me->zif_doc_fiscal_ft_entrada~at_nota_fiscal-material
        i_viagem_id               = wa_zlest0185-viagem_id
      IMPORTING
        e_tarifa                  = e_tarifa
        e_qtd_precos_encontrados  = lc_qtd_precos_encontrados
    ).

    IF lc_qtd_precos_encontrados GT 1.
      RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
        EXPORTING
          textid = VALUE #( msgid = zcx_doc_fiscal_ft_entrada=>zcx_varios_preco_frete-msgid
                            msgno = zcx_doc_fiscal_ft_entrada=>zcx_varios_preco_frete-msgno )
          msgid  = zcx_doc_fiscal_ft_entrada=>zcx_varios_preco_frete-msgid
          msgno  = zcx_doc_fiscal_ft_entrada=>zcx_varios_preco_frete-msgno
          msgty  = 'E'.
    ENDIF.

    CHECK e_tarifa LE 0.

    "ZCX_VARIOS_PRECO_FRETE	Varios Preço de Frete. Solicite regularização à transportadora da região!
    RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
      EXPORTING
        textid = VALUE #( msgid = zcx_doc_fiscal_ft_entrada=>zcx_sem_preco_frete-msgid
                          msgno = zcx_doc_fiscal_ft_entrada=>zcx_sem_preco_frete-msgno )
        msgid  = zcx_doc_fiscal_ft_entrada=>zcx_sem_preco_frete-msgid
        msgno  = zcx_doc_fiscal_ft_entrada=>zcx_sem_preco_frete-msgno
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_TIPO_ENTRADA.

    R_INSTANCIA = ME.

    SELECT SINGLE * INTO E_TIPO_ENTRADA
      FROM ZSDT0001TE
     WHERE ID_ENTRADA EQ I_ID_ENTRADA
       AND ID_EMPRESA EQ I_ID_EMPRESA
       AND CK_NFE     EQ I_CK_NFE.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_TIPO_ENTRADA-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_TIPO_ENTRADA-MSGNO
                          ATTR1 = CONV #( I_ID_ENTRADA )
                          ATTR2 = CONV #( I_ID_EMPRESA )
                          ATTR3 = CONV #( I_CK_NFE ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_TIPO_ENTRADA-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_TIPO_ENTRADA-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( I_ID_ENTRADA )
        MSGV2  = CONV #( I_ID_EMPRESA )
        MSGV3  = CONV #( I_CK_NFE ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~GET_TIPO_TRANSPORTE.

    R_INSTANCIA = ME.

    SELECT SINGLE * INTO @DATA(WA_EKKO)
      FROM EKKO
     WHERE EBELN EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELN.

    SELECT SINGLE SHTYP INTO E_SHTYP
      FROM ZSDT0011
     WHERE BSART EQ WA_EKKO-BSART.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_TIPO_PEDIDO_TRANSP-MSGID
                          MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_TIPO_PEDIDO_TRANSP-MSGNO
                          ATTR1 = CONV #( WA_EKKO-BSART ) )
        MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_TIPO_PEDIDO_TRANSP-MSGID
        MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_TIPO_PEDIDO_TRANSP-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( WA_EKKO-BSART ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_ATUALIZA_CONHECIMENTO.

    DATA: LC_CICLOS	TYPE ZDE_QTD_CICLOS.
    DATA: LC_REFKEY	TYPE J_1BREFKEY.

    R_INSTANCIA = ME.

    CLEAR: E_NRO_NF_FRETE.

    TRY .
        ZCL_FATURAMENTO=>ZIF_FATURAMENTO~GET_INSTANCE(
          )->GET_PROCESSO_EMISSAO_DOCS(
            EXPORTING
              I_TKNUM       = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP
            IMPORTING
              E_CONHECIMENTO = DATA(E_CONHECIMENTO)
          ).

      CATCH ZCX_FATURAMENTO INTO DATA(EX_FATURAMENTO).

        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID  = EX_FATURAMENTO->MSGID
                              MSGNO  = EX_FATURAMENTO->MSGNO
                              ATTR1  = EX_FATURAMENTO->MSGV1
                              ATTR2  = EX_FATURAMENTO->MSGV2
                              ATTR3  = EX_FATURAMENTO->MSGV3
                              ATTR4  = EX_FATURAMENTO->MSGV4 )
            MSGID  = EX_FATURAMENTO->MSGID
            MSGNO  = EX_FATURAMENTO->MSGNO
            MSGV1  = EX_FATURAMENTO->MSGV1
            MSGV2  = EX_FATURAMENTO->MSGV2
            MSGV3  = EX_FATURAMENTO->MSGV3
            MSGV4  = EX_FATURAMENTO->MSGV4
            MSGTY  = 'E'.

      CATCH ZCX_ERROR INTO DATA(EX_ERROR).

        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID  = EX_ERROR->MSGID
                              MSGNO  = EX_ERROR->MSGNO
                              ATTR1  = EX_ERROR->MSGV1
                              ATTR2  = EX_ERROR->MSGV2
                              ATTR3  = EX_ERROR->MSGV3
                              ATTR4  = EX_ERROR->MSGV4 )
            MSGID  = EX_ERROR->MSGID
            MSGNO  = EX_ERROR->MSGNO
            MSGV1  = EX_ERROR->MSGV1
            MSGV2  = EX_ERROR->MSGV2
            MSGV3  = EX_ERROR->MSGV3
            MSGV4  = EX_ERROR->MSGV4
            MSGTY  = 'E'.

    ENDTRY.


    IF E_CONHECIMENTO EQ ABAP_TRUE.

      LC_REFKEY = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE.

      SELECT SINGLE IT~DOCNUM
        INTO ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE
        FROM J_1BNFLIN AS IT
        INNER JOIN J_1BNFDOC AS NF ON NF~DOCNUM EQ IT~DOCNUM
       WHERE IT~REFTYP  EQ 'BI'
         AND IT~REFKEY EQ LC_REFKEY
         AND NF~CANCEL NE ABAP_TRUE.

      LC_CICLOS = 5.

      WHILE LC_CICLOS IS NOT INITIAL AND ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE IS INITIAL.

        "Tempo de Cadas Ciclo
        WAIT UP TO 10 SECONDS.

        SELECT SINGLE IT~DOCNUM
          INTO ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE
          FROM J_1BNFLIN AS IT
          INNER JOIN J_1BNFDOC AS NF ON NF~DOCNUM EQ IT~DOCNUM
         WHERE IT~REFTYP  EQ 'BI'
           AND IT~REFKEY EQ LC_REFKEY
           AND NF~CANCEL NE ABAP_TRUE.

        SUBTRACT 1 FROM LC_CICLOS.

      ENDWHILE.

      IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE
          IMPORTING
            OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE.
        E_NRO_NF_FRETE = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '08'.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '09'.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).
      ELSE.
        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DOCUMENO_FATURA-MSGID
                              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DOCUMENO_FATURA-MSGNO
                              ATTR1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE ) )
            MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DOCUMENO_FATURA-MSGID
            MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DOCUMENO_FATURA-MSGNO
            MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE )
            MSGTY  = 'E'.
      ENDIF.

    ELSE.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '08'.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_AUTORIZAR_DOCUMENTO.

    R_INSTANCIA = ME.

    TRY .
        ZCL_FATURAMENTO=>ZIF_FATURAMENTO~GET_INSTANCE(
          )->GET_PROCESSO_EMISSAO_DOCS(
            EXPORTING
              I_TKNUM       = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP
            IMPORTING
              E_CONHECIMENTO = DATA(E_CONHECIMENTO)
              E_MANIFESTO    = DATA(E_MANIFESTO)
          ).

      CATCH ZCX_FATURAMENTO INTO DATA(EX_FATURAMENTO).

        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID  = EX_FATURAMENTO->MSGID
                              MSGNO  = EX_FATURAMENTO->MSGNO
                              ATTR1  = EX_FATURAMENTO->MSGV1
                              ATTR2  = EX_FATURAMENTO->MSGV2
                              ATTR3  = EX_FATURAMENTO->MSGV3
                              ATTR4  = EX_FATURAMENTO->MSGV4 )
            MSGID  = EX_FATURAMENTO->MSGID
            MSGNO  = EX_FATURAMENTO->MSGNO
            MSGV1  = EX_FATURAMENTO->MSGV1
            MSGV2  = EX_FATURAMENTO->MSGV2
            MSGV3  = EX_FATURAMENTO->MSGV3
            MSGV4  = EX_FATURAMENTO->MSGV4
            MSGTY  = 'E'.

      CATCH ZCX_ERROR INTO DATA(EX_ERROR).

        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID  = EX_ERROR->MSGID
                              MSGNO  = EX_ERROR->MSGNO
                              ATTR1  = EX_ERROR->MSGV1
                              ATTR2  = EX_ERROR->MSGV2
                              ATTR3  = EX_ERROR->MSGV3
                              ATTR4  = EX_ERROR->MSGV4 )
            MSGID  = EX_ERROR->MSGID
            MSGNO  = EX_ERROR->MSGNO
            MSGV1  = EX_ERROR->MSGV1
            MSGV2  = EX_ERROR->MSGV2
            MSGV3  = EX_ERROR->MSGV3
            MSGV4  = EX_ERROR->MSGV4
            MSGTY  = 'E'.

    ENDTRY.

    IF E_CONHECIMENTO EQ ABAP_TRUE.

      TRY .
          "Singleton
          DATA(LC_CTE) =
          "Autorizar Documento Fiscal Eletrônico
          ZCL_CTE=>ZIF_DOC_ELETRONICO~GET_INSTANCE(
             EXPORTING
               I_DOCNUM  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE
           )->SET_BLOQUEAR(
             EXPORTING
               I_BLOQUEAR = ABAP_FALSE
           )->SET_REGISTRO(
             EXPORTING
               I_DOCNUM = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE

           )->SET_AUTORIZAR(
             EXPORTING
               I_AGUARDAR         = ABAP_TRUE  " Aguardar Resposta
               I_CICLOS           = 120   " Quantidade de Ciclos
               I_SEGUNDOS         = 03   " Quantidade de Segundos do Ciclo

           )->GET_CK_AUTORIZADO_USO(

           )->SET_AUTORIZA_MDFE(
             EXPORTING
               I_ACAO     = ZIF_DOC_ELETRONICO=>AT_ACAO_MDFE_AUTORIZA
               I_AGUARDAR = ABAP_TRUE  " Aguardar Resposta
               I_CICLOS   = 120   " Quantidade de Ciclos
               I_SEGUNDOS = 03   " Quantidade de Segundos do Ciclo

           )->SET_AUTORIZA_VIAGEM_TIP_FRETE(
             EXPORTING
               I_ACAO = ZIF_DOC_ELETRONICO=>AT_ACAO_VIAGEM_FRETE_AUTORIZAR

           )->SET_CLEAR(
           ).

          ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-CTE_AUTORIZADO = ABAP_TRUE.
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC        = '99'.
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).
          "ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_CLEAR( ).

        CATCH ZCX_DOC_ELETRONICO INTO DATA(EX_DOC_ELETRONICO).
          IF LC_CTE IS NOT INITIAL.
            LC_CTE->SET_CLEAR( ).
            CLEAR: LC_CTE.
          ENDIF.
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-CTE_AUTORIZADO = ABAP_FALSE.
          EX_DOC_ELETRONICO->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
      ENDTRY.

    ENDIF.


  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_CLEAR.

    R_INSTANCIA = ME.

    CLEAR: ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO,
           ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO,
           ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS,
           ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL,
           ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_DACTE,
           ME->CK_ESTORNANDO.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_CRIAR_AVISO_NOTA_CARGA.

    DATA: I_ITEM   TYPE ZDE_BAPI_REMESSA_ITEM,
          LC_AVISO TYPE REF TO ZCL_AVISO_RECEBIMENTO,
          I_XBLNR  TYPE XBLNR_V1,
          L_SERIE  TYPE C LENGTH 3.

    DATA: FT_ENTRADA TYPE REF TO ZCL_DOC_FISCAL_FT_ENTRADA.

    CLEAR: E_VBELN.
    R_GEROU = ABAP_FALSE.

    SELECT SINGLE * INTO @DATA(WA_ZSDT0001CG)
      FROM ZSDT0001CG
     WHERE ID_CARGA EQ @I_NOTA-ID_CARGA.

    SELECT SINGLE * INTO @DATA(WA_EKKO)
      FROM EKKO
     WHERE EBELN EQ @I_NOTA-PO_NUMBER.

    SELECT SINGLE * INTO @DATA(WA_EKPO)
      FROM EKPO
     WHERE EBELN EQ @I_NOTA-PO_NUMBER
       AND EBELP EQ @I_NOTA-PO_ITEM.

    SELECT *
      INTO TABLE @DATA(IT_EKET)
      FROM EKET
     WHERE EBELN EQ @I_NOTA-PO_NUMBER
       AND EBELP EQ @I_NOTA-PO_ITEM.

    READ TABLE IT_EKET INTO DATA(WA_EKET) WITH KEY EBELN = WA_EKPO-EBELN EBELP = WA_EKPO-EBELP.

    CREATE OBJECT LC_AVISO.
    LC_AVISO->SET_FORNECEDOR( I_LIFNR = I_NOTA-ID_FORNECEDOR ).

    DATA(LC_CK_NFE) = COND #( LET CLET = I_NOTA-ID_MOD_FISCAL IN WHEN CLET EQ ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO THEN ABAP_TRUE ELSE ABAP_FALSE ).

    SELECT SINGLE * INTO @DATA(WA_ZSDT0001TE)
      FROM ZSDT0001TE
     WHERE ID_ENTRADA EQ @I_NOTA-ID_ENTRADA
       AND ID_EMPRESA EQ @WA_ZSDT0001CG-ID_BUKRS
       AND CK_NFE     EQ @LC_CK_NFE.

    I_ITEM-EBELN        = I_NOTA-PO_NUMBER.
    I_ITEM-EBELP        = WA_EKPO-EBELP.
    I_ITEM-VGTYP        = 'V'.
    I_ITEM-QUANTIDADE   = I_NOTA-NR_QUANTIDADE.
    I_ITEM-UNIDADE      = 'KG'.
    I_ITEM-MATERIAL     = WA_EKPO-MATNR.
    I_ITEM-TRATY        = '0001'.
    I_ITEM-TRAGR        = '0001'.
    I_ITEM-LADGR        = '0003'.
    I_ITEM-MFRGR        = '00000001'.
    I_ITEM-KZBEW        = 'B'.
    I_ITEM-PLANT        = WA_EKPO-WERKS.
    I_ITEM-STGE_LOC     = WA_EKPO-LGORT.
    I_ITEM-MOVE_TYPE    = WA_ZSDT0001TE-TP_MOV_MERCADORIA.
    I_ITEM-BATCH        = WA_EKET-CHARG.
    I_ITEM-LICHA        = WA_EKET-CHARG.
    LC_AVISO->SET_ITEM( I_ITEM = I_ITEM ).

    IF I_NOTA-ID_ENTREGUE_POR IS NOT INITIAL.
      LC_AVISO->SET_LC_COLETA_PARID( I_PARID = I_NOTA-ID_ENTREGUE_POR ).
      LC_AVISO->SET_LC_COLETA_PARTYP( I_PARTYP = 'V' ).
    ELSE.
      LC_AVISO->SET_LC_COLETA_PARID( I_PARID = I_NOTA-ID_FORNECEDOR ).
      LC_AVISO->SET_LC_COLETA_PARTYP( I_PARTYP = 'V' ).
    ENDIF.

    LC_AVISO->SET_SP_FRETE_PARID( I_PARID = I_AGENTE_FRETE ).
    LC_AVISO->SET_SP_FRETE_PARTYP( I_PARTYP = 'V' ).

    LC_AVISO->SET_SHIP_POINT( I_SHIP_POINT = WA_ZSDT0001CG-ID_BRANCH ).

    LC_AVISO->SET_LC_ENTREGA_PARID( I_PARID = I_COD_LOC_ENTREGA ).
    LC_AVISO->SET_LC_ENTREGA_PARTYP( I_PARTYP = 'C' ).

    LC_AVISO->SET_VALOR_NOTA( I_VALOR_NOTA = I_NOTA-NR_VALOR ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = I_NOTA-NR_NOTA
      IMPORTING
        OUTPUT = I_XBLNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = I_NOTA-NM_SERIE
      IMPORTING
        OUTPUT = L_SERIE.

    CONCATENATE I_XBLNR L_SERIE INTO I_XBLNR SEPARATED BY '-'.

    LC_AVISO->SET_XBLNR( I_XBLNR = I_XBLNR ).
    LC_AVISO->SET_CK_ROUTE_VALIDAR( I_CK_ROUTE_VALIDAR = ABAP_TRUE ).

    "Procurar se já existe aviso de recebimento gerado """""""""""""""""""""""""""""""""""""""""
    FT_ENTRADA = NEW #(  ).

    TRY.

        FT_ENTRADA->ZIF_DOC_FISCAL_FT_ENTRADA~GET_AVISO_RECEBIMENTO(
          EXPORTING
            I_NUMERO    = CONV #( I_NOTA-NR_NOTA )
            I_SERIE     = I_NOTA-NM_SERIE
            I_EMISSOR   = I_NOTA-ID_FORNECEDOR
            I_EBELN     = WA_EKPO-EBELN
            I_EBELP     = WA_EKPO-EBELP
          IMPORTING
           E_LIKP      = DATA(E_LIKP) ).

        R_GEROU = ABAP_TRUE.
        E_VBELN = E_LIKP-VBELN.

        UPDATE ZSDT0001NT
           SET AV_VBELN  = E_LIKP-VBELN
         WHERE ID_CARGA EQ I_NOTA-ID_CARGA
           AND ID_NOTA  EQ I_NOTA-ID_NOTA.

        COMMIT WORK AND WAIT.

        MESSAGE S289 WITH E_VBELN I_NOTA-NR_NOTA I_NOTA-NM_SERIE.

      CATCH ZCX_DOC_FISCAL_FT_ENTRADA.
    ENDTRY.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK R_GEROU IS INITIAL.

    TRY .
        R_GEROU = LC_AVISO->CRIAR_AVISO_RECEBIMENTO( I_PARTICAO_LOTE = ABAP_FALSE ).
      CATCH ZCX_DELIVERY INTO DATA(EX_DELIVERY).
        EX_DELIVERY->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
        ZCL_DOC_FISCAL_FT_ENTRADA=>ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
    ENDTRY.

    IF R_GEROU EQ ABAP_TRUE.
      E_VBELN = LC_AVISO->GET_NR_REMESSA( ).

      MESSAGE S288 WITH E_VBELN I_NOTA-NR_NOTA I_NOTA-NM_SERIE.

      UPDATE ZSDT0001NT
         SET AV_VBELN  = E_VBELN
       WHERE ID_CARGA EQ I_NOTA-ID_CARGA
         AND ID_NOTA  EQ I_NOTA-ID_NOTA.

      COMMIT WORK AND WAIT.
      WAIT UP TO 2 SECONDS.

      CLEAR: LC_AVISO.
    ELSE.
      DATA(R_RETORNO) = LC_AVISO->GET_RETORNO( ).
      CLEAR: LC_AVISO.
      LOOP AT R_RETORNO INTO DATA(WA_RETORNO) WHERE TYPE EQ 'E'.
        MESSAGE ID WA_RETORNO-ID TYPE 'S'
         NUMBER WA_RETORNO-NUMBER
           WITH WA_RETORNO-MESSAGE_V1
                WA_RETORNO-MESSAGE_V2
                WA_RETORNO-MESSAGE_V3
                WA_RETORNO-MESSAGE_V4.
        ZCL_DOC_FISCAL_FT_ENTRADA=>ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
      ENDLOOP.
      READ TABLE R_RETORNO INDEX 1 INTO WA_RETORNO.
      MESSAGE ID WA_RETORNO-ID TYPE 'S'
       NUMBER WA_RETORNO-NUMBER
         WITH WA_RETORNO-MESSAGE_V1
              WA_RETORNO-MESSAGE_V2
              WA_RETORNO-MESSAGE_V3
              WA_RETORNO-MESSAGE_V4.
      ZCL_DOC_FISCAL_FT_ENTRADA=>ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_CRIAR_AVISO_RECEBIMENTO.

    DATA: I_ITEM   TYPE ZDE_BAPI_REMESSA_ITEM,
          LC_AVISO TYPE REF TO ZCL_AVISO_RECEBIMENTO,
          I_XBLNR	 TYPE XBLNR_V1,
          L_SERIE  TYPE C LENGTH 3.

    R_INSTANCIA = ME.

    SELECT SINGLE * INTO @DATA(WA_EKPO)
      FROM EKPO
     WHERE EBELN EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELN
       AND EBELP EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELP.

    SELECT *
      INTO TABLE @DATA(IT_EKET)
      FROM EKET
     WHERE EBELN EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELN
       AND EBELP EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELP.

    READ TABLE IT_EKET INTO DATA(WA_EKET) WITH KEY EBELN = WA_EKPO-EBELN EBELP = WA_EKPO-EBELP.

    CREATE OBJECT LC_AVISO.
    LC_AVISO->SET_FORNECEDOR( I_LIFNR = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_REMETENTE ).

    I_ITEM-EBELN        = WA_EKPO-EBELN.
    I_ITEM-EBELP        = WA_EKPO-EBELP.
    I_ITEM-VGTYP        = 'V'.
    I_ITEM-QUANTIDADE   = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PESO_LIQ.
    I_ITEM-UNIDADE      = 'KG'.
    I_ITEM-MATERIAL     = WA_EKPO-MATNR.
    I_ITEM-TRATY        = '0001'.
    I_ITEM-TRAGR        = '0001'.
    I_ITEM-LADGR        = '0003'.
    I_ITEM-MFRGR        = '00000001'.
    I_ITEM-KZBEW        = 'B'.
    I_ITEM-PLANT        = WA_EKPO-WERKS.
    I_ITEM-STGE_LOC     = WA_EKPO-LGORT.
    I_ITEM-MOVE_TYPE    = '101'.
    I_ITEM-BATCH        = WA_EKET-CHARG.
    I_ITEM-LICHA        = WA_EKET-CHARG.
    LC_AVISO->SET_ITEM( I_ITEM = I_ITEM ).

    IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA IS NOT INITIAL.
      LC_AVISO->SET_LC_COLETA_PARID( I_PARID = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA ).
      LC_AVISO->SET_LC_COLETA_PARTYP( I_PARTYP = 'V' ).
    ELSE.
      LC_AVISO->SET_LC_COLETA_PARID( I_PARID = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_REMETENTE ).
      LC_AVISO->SET_LC_COLETA_PARTYP( I_PARTYP = 'V' ).
    ENDIF.

    LC_AVISO->SET_SP_FRETE_PARID( I_PARID = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE ).
    LC_AVISO->SET_SP_FRETE_PARTYP( I_PARTYP = 'V' ).

    LC_AVISO->SET_SHIP_POINT( I_SHIP_POINT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BRANCH ).

    IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA IS NOT INITIAL.
      LC_AVISO->SET_LC_ENTREGA_PARID( I_PARID = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA ).
      LC_AVISO->SET_LC_ENTREGA_PARTYP( I_PARTYP = 'V' ).
    ELSE.
      LC_AVISO->SET_LC_ENTREGA_PARID( I_PARID = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_DEST_MERC ).
      LC_AVISO->SET_LC_ENTREGA_PARTYP( I_PARTYP = 'C' ).
    ENDIF.

    LC_AVISO->SET_VALOR_NOTA( I_VALOR_NOTA = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VLR_TOTAL ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NUMERO
      IMPORTING
        OUTPUT = I_XBLNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-SERIE
      IMPORTING
        OUTPUT = L_SERIE.

    CONCATENATE I_XBLNR L_SERIE INTO I_XBLNR SEPARATED BY '-'.

    LC_AVISO->SET_XBLNR( I_XBLNR = I_XBLNR ).
    LC_AVISO->SET_CK_ROUTE_VALIDAR( I_CK_ROUTE_VALIDAR = ABAP_TRUE ).

    TRY .
        DATA(R_GEROU) = LC_AVISO->CRIAR_AVISO_RECEBIMENTO( I_PARTICAO_LOTE = ABAP_FALSE ).
      CATCH ZCX_DELIVERY INTO DATA(EX_DELIVERY).
        EX_DELIVERY->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
    ENDTRY.

    IF R_GEROU EQ ABAP_TRUE.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN = LC_AVISO->GET_NR_REMESSA( ).
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VBELN       = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-VBELN = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN.
      WAIT UP TO 5 SECONDS.
      CLEAR: LC_AVISO.
    ELSE.
      DATA(R_RETORNO) = LC_AVISO->GET_RETORNO( ).
      CLEAR: LC_AVISO.
      LOOP AT R_RETORNO INTO DATA(WA_RETORNO) WHERE TYPE EQ 'E'.
        MESSAGE ID WA_RETORNO-ID TYPE 'S'
         NUMBER WA_RETORNO-NUMBER
           WITH WA_RETORNO-MESSAGE_V1
                WA_RETORNO-MESSAGE_V2
                WA_RETORNO-MESSAGE_V3
                WA_RETORNO-MESSAGE_V4.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
      ENDLOOP.
      READ TABLE R_RETORNO INDEX 1 INTO WA_RETORNO.
      MESSAGE ID WA_RETORNO-ID TYPE 'S'
       NUMBER WA_RETORNO-NUMBER
         WITH WA_RETORNO-MESSAGE_V1
              WA_RETORNO-MESSAGE_V2
              WA_RETORNO-MESSAGE_V3
              WA_RETORNO-MESSAGE_V4.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_CRIAR_DOC_CUSTO.

    R_INSTANCIA = ME.

    CLEAR: E_FKNUM,
           E_OV_FRETE,
           E_FATURA_FRETE.

    TRY .
        ZCL_FATURAMENTO=>ZIF_FATURAMENTO~GET_INSTANCE(
          )->GET_PROCESSO_EMISSAO_DOCS(
            EXPORTING
              I_TKNUM       = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP
            IMPORTING
              E_DOC_CUSTO    = DATA(E_DOC_CUSTO)
              E_CONHECIMENTO = DATA(E_CONHECIMENTO)
              E_TIPO_VEICULO = DATA(E_TIPO_VEICULO)
              E_TP_FRETE     = DATA(E_TP_FRETE)
          ).

      CATCH ZCX_FATURAMENTO INTO DATA(EX_FATURAMENTO).

        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID  = EX_FATURAMENTO->MSGID
                              MSGNO  = EX_FATURAMENTO->MSGNO
                              ATTR1  = EX_FATURAMENTO->MSGV1
                              ATTR2  = EX_FATURAMENTO->MSGV2
                              ATTR3  = EX_FATURAMENTO->MSGV3
                              ATTR4  = EX_FATURAMENTO->MSGV4 )
            MSGID  = EX_FATURAMENTO->MSGID
            MSGNO  = EX_FATURAMENTO->MSGNO
            MSGV1  = EX_FATURAMENTO->MSGV1
            MSGV2  = EX_FATURAMENTO->MSGV2
            MSGV3  = EX_FATURAMENTO->MSGV3
            MSGV4  = EX_FATURAMENTO->MSGV4
            MSGTY  = 'E'.

      CATCH ZCX_ERROR INTO DATA(EX_ERROR).

        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID  = EX_ERROR->MSGID
                              MSGNO  = EX_ERROR->MSGNO
                              ATTR1  = EX_ERROR->MSGV1
                              ATTR2  = EX_ERROR->MSGV2
                              ATTR3  = EX_ERROR->MSGV3
                              ATTR4  = EX_ERROR->MSGV4 )
            MSGID  = EX_ERROR->MSGID
            MSGNO  = EX_ERROR->MSGNO
            MSGV1  = EX_ERROR->MSGV1
            MSGV2  = EX_ERROR->MSGV2
            MSGV3  = EX_ERROR->MSGV3
            MSGV4  = EX_ERROR->MSGV4
            MSGTY  = 'E'.

    ENDTRY.

    CASE E_DOC_CUSTO.
      WHEN ABAP_TRUE.

        SELECT SINGLE * INTO @DATA(WA_VFKP)
          FROM VFKP
         WHERE REBEL EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP.

        IF SY-SUBRC IS NOT INITIAL.

*RB_IN  Documento de custo de frete
*RB_OUT	Ordem / Fatura Serviço
          IF E_TIPO_VEICULO EQ ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_PROPRIO.
            DATA(CKFPROP) = ABAP_TRUE.
          ELSE.
            CKFPROP = ABAP_FALSE.
          ENDIF.

          TRY .
              ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
                )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE
                )->CK_PARCEIRO_LOCAL_NEGOCIO(
                ).

              DATA(RB_CUS) = ABAP_FALSE.

            CATCH ZCX_PARCEIROS.    " .
              IF CKFPROP EQ ABAP_TRUE.
                RB_CUS = ABAP_TRUE.
              ELSEIF CKFPROP EQ ABAP_FALSE AND E_TP_FRETE EQ ZIF_CARGA=>ST_TP_FRETE_CPT.
                RB_CUS = ABAP_TRUE.
              ENDIF.
          ENDTRY.

          SUBMIT ZLESR0013 WITH SO_TKNUM = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP
                           WITH P_VBELN  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                           WITH P_EBELN  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELN
                           WITH CKSETAP  = ABAP_TRUE
                           WITH RB_IN    = E_DOC_CUSTO    "Documento de custo de frete
                           WITH RB_OUT   = E_CONHECIMENTO "Ordem / Fatura Serviço
                           WITH CKFPROP  = CKFPROP
                           WITH RB_CUS   = RB_CUS
                           WITH RB_DTFAT = SY-DATUM AND RETURN.

          GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM.
          GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE.
          GET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM
            IMPORTING
              OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE
            IMPORTING
              OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE
            IMPORTING
              OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE.

          E_FKNUM = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM.
          E_OV_FRETE = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE.
          E_FATURA_FRETE = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE.

        ELSEIF E_CONHECIMENTO EQ ABAP_TRUE.

          ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM = WA_VFKP-FKNUM.
          E_FKNUM = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM.

          SELECT SINGLE * INTO @DATA(WA_VBAK)
            FROM VBAK
           WHERE TKNUM EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP.

          IF SY-SUBRC IS NOT INITIAL.
            SUBMIT ZLESR0013 WITH SO_TKNUM = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP
                             WITH P_VBELN  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                             WITH P_EBELN  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELN
                             WITH CKSETAP  = ABAP_TRUE
                             WITH RB_IN    = ABAP_FALSE  "Documento de custo de frete
                             WITH RB_OUT   = E_CONHECIMENTO   "Ordem / Fatura Serviço
                             WITH RB_DTFAT = SY-DATUM AND RETURN.

            GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE.
            GET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE
              IMPORTING
                OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE
              IMPORTING
                OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE.

            E_OV_FRETE = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE.
            E_FATURA_FRETE = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE.

          ELSE.
            "Ordem de Venda do Serviço
            ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE = WA_VBAK-VBELN.
            E_OV_FRETE = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE.

            SELECT SINGLE * INTO @DATA(WA_VBRP)
              FROM VBRP
             WHERE AUBEL EQ @WA_VBAK-VBELN AND DRAFT = @SPACE .

            IF SY-SUBRC IS INITIAL.
              "Fatura da Ordem de Venda do Frete
              ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE = WA_VBRP-VBELN.
              E_FATURA_FRETE = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE.
            ENDIF.
          ENDIF.
        ENDIF.

        IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM
            IMPORTING
              OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM.
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '05'.
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).
        ELSE.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DOC_CUSTO-MSGID
                                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DOC_CUSTO-MSGNO
                                ATTR1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP ) )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DOC_CUSTO-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_DOC_CUSTO-MSGNO
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP )
              MSGTY  = 'E'.
        ENDIF.

        IF E_CONHECIMENTO EQ ABAP_TRUE.
          IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE
              IMPORTING
                OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE.
            ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '06'.
            ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).
          ELSE.
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_ORDEM_VENDA_FRETE-MSGID
                                  MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_ORDEM_VENDA_FRETE-MSGNO
                                  ATTR1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM ) )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_ORDEM_VENDA_FRETE-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_ORDEM_VENDA_FRETE-MSGNO
                MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM )
                MSGTY  = 'E'.
          ENDIF.

          IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE
              IMPORTING
                OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE.
            ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '07'.
            ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).
          ELSE.
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_FATURA_FRETE-MSGID
                                  MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_FATURA_FRETE-MSGNO
                                  ATTR1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE ) )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_FATURA_FRETE-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_SEM_FATURA_FRETE-MSGNO
                MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE )
                MSGTY  = 'E'.
          ENDIF.
        ELSE.
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '07'.
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).
        ENDIF.
      WHEN ABAP_FALSE.

        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '07'.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_doc_fiscal_ft_entrada~set_criar_doc_transporte.

    DATA: st_headerdata       TYPE bapishipmentheader,
          st_stagedata        TYPE bapishipmentstage,
          it_stagedata        TYPE TABLE OF bapishipmentstage,
          st_itemdata         TYPE bapishipmentitem,
          st_headerdataaction TYPE bapishipmentheaderaction,
          it_itemdata         TYPE TABLE OF bapishipmentitem,
          lc_traztd	          TYPE traztd,
          lc_viagem_id        TYPE zlest0185-viagem_id,
          lc_bukrs            TYPE zlest0185-bukrs,
          lc_branch           TYPE zlest0185-branch,
          lc_adrnr            TYPE j_1bbranch-adrnr,
          lc_region           TYPE adrc-region,
          lc_tdlnr            TYPE zlest0207-tdlnr,
          it_return	          TYPE TABLE OF bapiret2.

    DATA: handle TYPE REF TO zcl_memory_ft_entrada,
          root   TYPE REF TO zcl_memory_ft_entrada_handle,
          oref   TYPE REF TO zcl_memory_ft_entrada_handle.

    r_instancia = me.

    CLEAR: e_doc_transp.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-id_agrupa_frete IS NOT INITIAL.

      SELECT * INTO TABLE @DATA(it_zlest0108)
        FROM zlest0108
       WHERE id_agrupa_frete EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-id_agrupa_frete.

      LOOP AT it_zlest0108 ASSIGNING FIELD-SYMBOL(<fs_zlest0108>).

        CLEAR: lc_viagem_id,
               lc_bukrs,
               lc_branch,
               lc_adrnr,
               lc_region,
               lc_tdlnr.

        TRY .
            <fs_zlest0108>-doc_transp = zcl_delivery=>get_doc_transporte_rodo( EXPORTING i_vbeln = <fs_zlest0108>-vbeln ).
          CATCH zcx_delivery.    "
        ENDTRY.

        "Verificar Pedido de Compra e Ordem de Venda """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF <fs_zlest0108>-id_carga IS INITIAL.
          CONTINUE.
        ENDIF.

*-------CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
        SELECT SINGLE      bukrs     branch     viagem_id
                 INTO ( lc_bukrs, lc_branch, lc_viagem_id )
          FROM zlest0185
         WHERE id_ordem = <fs_zlest0108>-id_ordem.

*-------Agente de Frete para Z021
        IF i_shtyp = 'Z021'.
          SELECT SINGLE    adrnr
                   INTO lc_adrnr
                   FROM j_1bbranch
                  WHERE bukrs  = lc_bukrs
                    AND branch = lc_branch.

          SELECT SINGLE    region
                   INTO lc_region
                   FROM adrc
                  WHERE addrnumber = lc_adrnr
                    AND date_from <= sy-datum
                    AND date_to   >= sy-datum.

          SELECT SINGLE    tdlnr
                   INTO lc_tdlnr
                   FROM zlest0207
                  WHERE bukrs  = lc_bukrs
                    AND branch = lc_branch
                    AND regio  = lc_region.

          IF lc_tdlnr IS NOT INITIAL.
            me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-agente_frete = lc_tdlnr.
          ELSE.
            SELECT SINGLE    tdlnr
                     INTO lc_tdlnr
                     FROM zlest0207
                    WHERE bukrs  = lc_bukrs
                      AND branch = ''
                      AND regio  = lc_region.
            IF lc_tdlnr IS NOT INITIAL.
              me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-agente_frete = lc_tdlnr.
            ENDIF.
          ENDIF.
        ENDIF.
*-------CS2019001158 - Jaime Tassoni - 16.11.2020 - fim

        SELECT SINGLE * INTO @DATA(wa_zsdt0001nt)
          FROM zsdt0001nt
         WHERE id_carga EQ @<fs_zlest0108>-id_carga
           AND id_nota  EQ @<fs_zlest0108>-id_nota.

        IF wa_zsdt0001nt-po_number IS NOT INITIAL.

          SELECT SINGLE * INTO @DATA(wa_ekko)
            FROM ekko
           WHERE ebeln EQ @wa_zsdt0001nt-po_number
             AND bsart NE 'ZGR'.

          "Qualquer outro tipo de pedido deve ser parametrizado
          IF sy-subrc IS INITIAL.
            SELECT SINGLE * INTO @DATA(wa_ekpo)
              FROM ekpo
             WHERE ebeln EQ @wa_zsdt0001nt-po_number
               AND ebelp EQ @wa_zsdt0001nt-po_item.

            IF wa_ekpo-zckfreteent IS INITIAL.
              "Pedido de Compra &1 não configurado para Frete de Entrada (ME22N)
              RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
                EXPORTING
                  textid = VALUE #( msgid = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_pedido-msgid
                                    msgno = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_pedido-msgno
                                    attr1 = wa_zsdt0001nt-po_number )
                  msgid  = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_pedido-msgid
                  msgno  = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_pedido-msgno
                  msgty  = 'E'
                  msgv1  = CONV #( wa_zsdt0001nt-po_number ).
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.

        ENDIF.

        SELECT SINGLE * INTO @DATA(wa_zsdt0001ov)
          FROM zsdt0001ov
         WHERE id_carga EQ @<fs_zlest0108>-id_carga.

        IF sy-subrc IS INITIAL.

          SELECT SINGLE * INTO @DATA(wa_vbak)
            FROM vbak
           WHERE vbeln EQ @wa_zsdt0001ov-nr_ordem_venda.

          IF sy-subrc IS INITIAL.
            IF wa_vbak-kvgr5 NE '002'.
              "Ordem de Venda &1 não configurada para Frete de Entrada (VA02)
              RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
                EXPORTING
                  textid = VALUE #( msgid = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_ordem-msgid
                                    msgno = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_ordem-msgno
                                    attr1 = wa_zsdt0001ov-nr_ordem_venda )
                  msgid  = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_ordem-msgid
                  msgno  = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_ordem-msgno
                  msgty  = 'E'
                  msgv1  = CONV #( wa_zsdt0001ov-nr_ordem_venda ).
            ENDIF.

          ELSE.

            SELECT SINGLE * INTO @wa_ekpo
              FROM ekpo
             WHERE ebeln EQ @wa_zsdt0001ov-nr_ordem_venda
               AND ebelp EQ '00010'.

            IF wa_ekpo-zckfreteent IS INITIAL AND sy-subrc IS INITIAL.
              "Pedido de Compra &1 não configurado para Frete de Entrada (ME22N)
              RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
                EXPORTING
                  textid = VALUE #( msgid = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_pedido-msgid
                                    msgno = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_pedido-msgno
                                    attr1 = wa_ekpo-ebeln )
                  msgid  = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_pedido-msgid
                  msgno  = zcx_doc_fiscal_ft_entrada=>zcx_frete_entrada_pedido-msgno
                  msgty  = 'E'
                  msgv1  = CONV #( wa_ekpo-ebeln ).
            ENDIF.
          ENDIF.

        ENDIF.

      ENDLOOP.

      DELETE it_zlest0108 WHERE vbeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln.

    ENDIF.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp IS INITIAL.
      TRY .
          me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp = zcl_delivery=>get_doc_transporte_rodo( EXPORTING i_vbeln = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln ).
        CATCH zcx_delivery.    "
      ENDTRY.
    ENDIF.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp
        IMPORTING
          output = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.

      e_doc_transp = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.

      SELECT SINGLE * INTO @DATA(lc_vttk)
        FROM vttk
       WHERE tknum EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.

      IF sy-subrc IS INITIAL AND lc_vttk-sttbg EQ abap_true.
        EXIT.
      ELSEIF sy-subrc IS INITIAL.

        st_headerdata-shipment_num           = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.
        st_headerdataaction-shipment_num     = 'D'.
        st_headerdataaction-service_agent_id = 'D'.

        CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
          EXPORTING
            headerdata       = st_headerdata
            headerdataaction = st_headerdataaction
          TABLES
            return           = it_return.

        LOOP AT it_return INTO DATA(wa_retorno) WHERE type = 'E'.
          MESSAGE ID wa_retorno-id TYPE 'S' NUMBER wa_retorno-number WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.
          me->zif_doc_fiscal_ft_entrada~get_erro_geral( ).
        ENDLOOP.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.

    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_likp)
      FROM likp
     WHERE vbeln EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln.

    SELECT SINGLE * INTO @DATA(wa_tvro)
      FROM tvro
     WHERE route EQ @wa_likp-route.

    SELECT SINGLE * INTO @DATA(wa_tvtk)
      FROM tvtk
     WHERE shtyp EQ @i_shtyp.

    IF wa_tvro-traztd GT 24.
      lc_traztd = wa_tvro-traztd / 24.
    ELSE.
      lc_traztd = wa_tvro-traztd.
    ENDIF.

    CLEAR: st_headerdata, st_headerdataaction.
    st_headerdata-service_agent_id      = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-agente_frete.
    st_headerdata-service_level         = '1'.
    st_headerdata-shipping_type         = '01'.
    st_headerdata-status_plan           = 'X'.
    st_headerdata-status_checkin        = 'X'.
    st_headerdata-status_load_start     = 'X'.
    st_headerdata-special_procedure_id  = '0001'.
    st_headerdata-shpmnt_cost_rel       = 'X'.
    st_headerdata-shipment_type         = i_shtyp.
    st_headerdata-zid_viagem            = lc_viagem_id.

    SELECT SINGLE j_1bbranch INTO @st_headerdata-trans_plan_pt
      FROM t001w
     WHERE werks EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-werks.

    SELECT SINGLE * INTO @DATA(wa_j_1bbranch)
      FROM j_1bbranch
     WHERE branch EQ @st_headerdata-trans_plan_pt.

    st_headerdata-shipment_route        = wa_likp-route.
    st_headerdata-zid_carga             = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-id_carga.

    st_headerdata-distance              = wa_tvro-distz.
    st_headerdata-distance_unit         = wa_tvro-medst.
    st_headerdata-time_travel           = lc_traztd.
    st_headerdata-time_unit             = 'H'.

    "Etapas
    CLEAR st_stagedata.
    REFRESH it_stagedata.
    st_stagedata-stage_cat      = '1'.
    st_stagedata-stage_seq 	    = '0001'.
    st_stagedata-shipping_type  = '01'.
    st_stagedata-service_agent  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-agente_frete.

    DATA(i_coleta) = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance(  )->set_parceiro( me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-cod_loc_coleta ) )->at_lfa1.

    CASE i_coleta-ktokk.
      WHEN 'ZFIC'.
        st_stagedata-org_shipp_dpmnt = i_coleta-lifnr+6(4).
      WHEN OTHERS.
        st_stagedata-org_suppl  = i_coleta-lifnr.
    ENDCASE.

    st_stagedata-leg_indicator = wa_tvtk-laufk.

    CASE wa_tvtk-laufk.
      WHEN 1. "Se Código de Percurso  igual a 1:  Percurso preliminar

        TRY.
            me->zif_doc_fiscal_ft_entrada~get_entroncamento_lc_entrega( IMPORTING e_tvkn = DATA(e_tvkn) ).
            st_stagedata-dest_point   = e_tvkn-knote.
          CATCH zcx_doc_fiscal_ft_entrada .
            st_stagedata-dest_point   = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-cod_loc_entrega.
        ENDTRY.

      WHEN 4. "Se Código de Percurso  igual a 4:  Percurso direto

        CASE i_shtyp.
          WHEN 'Z004'.
            me->zif_doc_fiscal_ft_entrada~get_local_entrega_fornecedor(
             IMPORTING
               e_lifnr = st_stagedata-dest_suppl ).
          WHEN OTHERS.
            st_stagedata-dest_cust = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-cod_loc_entrega.
        ENDCASE.

      WHEN OTHERS.
    ENDCASE.

    APPEND st_stagedata TO it_stagedata.

    " DADOS ITENS
    REFRESH it_itemdata.
    CLEAR st_itemdata.
    st_itemdata-delivery    =  me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln.
    st_itemdata-itenerary   =  10.
    APPEND st_itemdata TO it_itemdata.

    LOOP AT it_zlest0108 INTO DATA(wa_zlest0108).
      CLEAR st_itemdata.
      st_itemdata-delivery    =  wa_zlest0108-vbeln.
      st_itemdata-itenerary   =  10 * ( sy-tabix + 1 ).
      APPEND st_itemdata TO it_itemdata.
    ENDLOOP.

    TRY .
        zcl_miro=>verificar_criar( i_data = sy-datum i_bukrs = wa_j_1bbranch-bukrs ).
      CATCH zcx_miro_exception INTO DATA(ex_miro).
        RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
          EXPORTING
            textid = VALUE #( msgid = ex_miro->msgid
                              msgno = ex_miro->msgno
                              attr1 = CONV #( ex_miro->msgv1 )
                              attr2 = CONV #( ex_miro->msgv2 )
                              attr3 = CONV #( ex_miro->msgv3 )
                              attr4 = CONV #( ex_miro->msgv4 ) )
            msgid  = ex_miro->msgid
            msgno  = ex_miro->msgno
            msgty  = 'E'
            msgv1  = ex_miro->msgv1
            msgv2  = ex_miro->msgv2
            msgv3  = ex_miro->msgv3
            msgv4  = ex_miro->msgv4.
    ENDTRY.

    TRY.
        handle = zcl_memory_ft_entrada=>attach_for_write( inst_name = CONV #( me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln ) ).
        CREATE OBJECT root AREA HANDLE handle.
        root->at_zlest0108 = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento.
        handle->set_root( root ).
        handle->detach_commit( ).
      CATCH cx_shm_exclusive_lock_active.  "
      CATCH cx_shm_version_limit_exceeded.  "
      CATCH cx_shm_change_lock_active.  "
      CATCH cx_shm_parameter_error.  "
      CATCH cx_shm_pending_lock_removed.  "
      CATCH cx_shm_attach_error.
    ENDTRY.

    CALL FUNCTION 'BAPI_SHIPMENT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        headerdata = st_headerdata
      IMPORTING
        transport  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp
      TABLES
        itemdata   = it_itemdata
        stagedata  = it_stagedata
        return     = it_return.

    LOOP AT it_return INTO wa_retorno WHERE type = 'E'.
      TRY.
          handle = zcl_memory_ft_entrada=>attach_for_read( inst_name = CONV #( me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln ) ).
          oref ?= handle->root.
          CLEAR oref.
          handle->detach( ).
          handle->free_area( ).
          handle->free_instance( ).
        CATCH cx_shm_inconsistent.  "
        CATCH cx_shm_no_active_version.  "
        CATCH cx_shm_read_lock_active.  "
        CATCH cx_shm_exclusive_lock_active.  "
        CATCH cx_shm_parameter_error.  "
        CATCH cx_shm_change_lock_active.  "
        CATCH cx_shm_attach_error.
      ENDTRY.
      MESSAGE ID wa_retorno-id TYPE 'S' NUMBER wa_retorno-number WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.
      me->zif_doc_fiscal_ft_entrada~get_erro_geral( ).
    ENDLOOP.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp IS NOT INITIAL.

      MESSAGE s290 WITH me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      CLEAR: st_headerdata, st_headerdataaction.
      st_headerdata-shipment_num           = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.
      st_headerdata-status_load_end        = 'X'.
      st_headerdataaction-status_load_end  = 'C'.

      CLEAR: it_return.

      CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
        EXPORTING
          headerdata       = st_headerdata
          headerdataaction = st_headerdataaction
        TABLES
          return           = it_return.

      LOOP AT it_return INTO wa_retorno WHERE type = 'E'.

        TRY.
            handle = zcl_memory_ft_entrada=>attach_for_read( inst_name = CONV #( me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln ) ).
            oref ?= handle->root.
            CLEAR oref.
            handle->detach( ).
            handle->free_area( ).
            handle->free_instance( ).
          CATCH cx_shm_inconsistent.  "
          CATCH cx_shm_no_active_version.  "
          CATCH cx_shm_read_lock_active.  "
          CATCH cx_shm_exclusive_lock_active.  "
          CATCH cx_shm_parameter_error.  "
          CATCH cx_shm_change_lock_active.  "
          CATCH cx_shm_attach_error.
        ENDTRY.

        MESSAGE ID wa_retorno-id TYPE 'S' NUMBER wa_retorno-number WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.
        me->zif_doc_fiscal_ft_entrada~get_erro_geral( ).
      ENDLOOP.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      CLEAR: st_headerdata, st_headerdataaction.
      st_headerdata-shipment_num              = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.
      st_headerdata-status_compl              = 'X'.
      st_headerdata-status_shpmnt_start       = 'X'.
      st_headerdataaction-status_compl        = 'C'.
      st_headerdataaction-status_shpmnt_start = 'C'.

      CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
        EXPORTING
          headerdata       = st_headerdata
          headerdataaction = st_headerdataaction
        TABLES
          return           = it_return.
*
      TRY.
          handle = zcl_memory_ft_entrada=>attach_for_read( inst_name = CONV #( me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln ) ).
          oref ?= handle->root.
          CLEAR oref.
          handle->detach( ).
          handle->free_area( ).
          handle->free_instance( ).
        CATCH cx_shm_inconsistent.  "
        CATCH cx_shm_no_active_version.  "
        CATCH cx_shm_read_lock_active.  "
        CATCH cx_shm_exclusive_lock_active.  "
        CATCH cx_shm_parameter_error.  "
        CATCH cx_shm_change_lock_active.  "
        CATCH cx_shm_attach_error.
      ENDTRY.

      LOOP AT it_return INTO wa_retorno WHERE type = 'E'.
        MESSAGE ID wa_retorno-id TYPE 'S' NUMBER wa_retorno-number WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.
        me->zif_doc_fiscal_ft_entrada~get_erro_geral( ).
      ENDLOOP.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-st_proc = '04'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp
        IMPORTING
          output = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.

      e_doc_transp = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.
      me->zif_doc_fiscal_ft_entrada~set_gravar_registro( ).

      "WAIT UP TO 5 SECONDS.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
        EXPORTING
          textid = VALUE #( msgid = zcx_doc_fiscal_ft_entrada=>zcx_documento_transporte-msgid
                            msgno = zcx_doc_fiscal_ft_entrada=>zcx_documento_transporte-msgno )
          msgid  = zcx_doc_fiscal_ft_entrada=>zcx_documento_transporte-msgid
          msgno  = zcx_doc_fiscal_ft_entrada=>zcx_documento_transporte-msgno
          msgty  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_DELE_AVISO_FRETE.

    R_INSTANCIA = ME.

    SELECT SINGLE * INTO @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO
      FROM ZLEST0108
     WHERE VBELN EQ @I_VBELN.

    CHECK SY-SUBRC IS INITIAL.

    IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP IS NOT INITIAL OR
       ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM IS NOT INITIAL OR
       ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE IS NOT INITIAL OR
       ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE IS NOT INITIAL OR
       ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGRUPA_EST-MSGID
                            MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGRUPA_EST-MSGNO )
          MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGRUPA_EST-MSGID
          MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGRUPA_EST-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    DELETE FROM ZLEST0108 WHERE VBELN EQ @I_VBELN.
    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_DELE_AVISO_NOTA_CARGA.

    DATA: LV_AVISO TYPE REF TO ZCL_AVISO_RECEBIMENTO.

    CHECK I_NOTA-AV_VBELN IS NOT INITIAL.

    TRY.
        LV_AVISO = NEW #(  ).
        LV_AVISO->SET_NR_REMESSA( I_REMESSA = I_NOTA-AV_VBELN ).
        DATA(R_ESTORNOU_AVISO) = LV_AVISO->ELIMINAR( ).

      CATCH ZCX_DELIVERY INTO DATA(EX_DELIVERY).

        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID = EX_DELIVERY->MSGID
                              MSGNO = EX_DELIVERY->MSGNO
                              ATTR1 = EX_DELIVERY->MSGV1
                              ATTR2 = EX_DELIVERY->MSGV2
                              ATTR3 = EX_DELIVERY->MSGV3
                              ATTR4 = EX_DELIVERY->MSGV4 )
            MSGTY  = 'E'
            MSGID  = EX_DELIVERY->MSGID
            MSGNO  = EX_DELIVERY->MSGNO
            MSGV1  = EX_DELIVERY->MSGV1
            MSGV2  = EX_DELIVERY->MSGV2
            MSGV3  = EX_DELIVERY->MSGV3
            MSGV4  = EX_DELIVERY->MSGV4.

    ENDTRY.

    DATA(R_RETORNO_AVISO) = LV_AVISO->GET_RETORNO_ELIMINAR( ).
    CLEAR: LV_AVISO.

    IF R_ESTORNOU_AVISO EQ ABAP_TRUE.

      UPDATE ZSDT0001NT
         SET AV_VBELN  = SPACE
       WHERE ID_CARGA EQ I_NOTA-ID_CARGA
         AND ID_NOTA  EQ I_NOTA-ID_NOTA.

      COMMIT WORK AND WAIT.

    ELSE.

      READ TABLE R_RETORNO_AVISO INTO DATA(WA_RETORNO_AVISO) WITH KEY TYPE = 'E'.

      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = WA_RETORNO_AVISO-ID
                            MSGNO = WA_RETORNO_AVISO-NUMBER
                            ATTR1 = WA_RETORNO_AVISO-MESSAGE_V1
                            ATTR2 = WA_RETORNO_AVISO-MESSAGE_V2
                            ATTR3 = WA_RETORNO_AVISO-MESSAGE_V3
                            ATTR4 = WA_RETORNO_AVISO-MESSAGE_V4 )
          MSGTY  = 'E'
          MSGID  = WA_RETORNO_AVISO-ID
          MSGNO  = WA_RETORNO_AVISO-NUMBER
          MSGV1  = WA_RETORNO_AVISO-MESSAGE_V1
          MSGV2  = WA_RETORNO_AVISO-MESSAGE_V2
          MSGV3  = WA_RETORNO_AVISO-MESSAGE_V3
          MSGV4  = WA_RETORNO_AVISO-MESSAGE_V4.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_ESTORNAR_AVISO.

    DATA: LC_AVISO TYPE REF TO ZCL_AVISO_RECEBIMENTO.

    R_INSTANCIA = ME.

    IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN IS NOT INITIAL.

      CREATE OBJECT LC_AVISO.
      LC_AVISO->SET_NR_REMESSA( I_REMESSA = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN ).

      DATA(LC_CK_GEROU) = LC_AVISO->ELIMINAR( ).
      DATA(IT_RETORNO)  = LC_AVISO->GET_RETORNO_ELIMINAR( ).

      IF LC_CK_GEROU EQ ABAP_FALSE.

        READ TABLE IT_RETORNO INTO DATA(WA_RETORNO) WITH KEY TYPE = 'E'.
        IF SY-SUBRC IS INITIAL.

          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = WA_RETORNO-ID
                                MSGNO = WA_RETORNO-NUMBER
                                ATTR1 = CONV #( WA_RETORNO-MESSAGE_V1 )
                                ATTR2 = CONV #( WA_RETORNO-MESSAGE_V2 )
                                ATTR3 = CONV #( WA_RETORNO-MESSAGE_V3 )
                                ATTR4 = CONV #( WA_RETORNO-MESSAGE_V4 ) )
              MSGID  = WA_RETORNO-ID
              MSGNO  = WA_RETORNO-NUMBER
              MSGTY  = 'E'
              MSGV1  = WA_RETORNO-MESSAGE_V1
              MSGV2  = WA_RETORNO-MESSAGE_V2
              MSGV3  = WA_RETORNO-MESSAGE_V3
              MSGV4  = WA_RETORNO-MESSAGE_V4.
        ELSE.

        ENDIF.
      ELSE.
        CLEAR: ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_doc_fiscal_ft_entrada~set_estornar_documentos.

    DATA: it_return              TYPE TABLE OF bapireturn1,
          it_success             TYPE TABLE OF bapivbrksuccess,
          wa_orderheaderin       TYPE bapisdh1,
          wa_orderheaderinx      TYPE bapisdh1x,
          it_return_ret2         TYPE TABLE OF bapiret2,
          it_bapiparex           TYPE TABLE OF bapiparex,
          wa_bapiparex           TYPE bapiparex,
          wa_bape_vbak           TYPE bape_vbak,
          wa_bape_vbakx          TYPE bape_vbakx,
          e_justificativa_cancel TYPE string.

    r_instancia = me.

    me->ck_estornando = abap_true.

    "Verificar pontos de estorno

    TRY .

        "Recuperar Registros: de Documento de Transporte
        IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp IS INITIAL AND me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln IS NOT INITIAL.
          SELECT SINGLE v~tknum INTO @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp
            FROM vttk AS v
           INNER JOIN vttp AS p ON p~tknum EQ v~tknum AND p~vbeln EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln
           WHERE v~shtyp EQ 'Z021' "Frete de Entrada
             AND v~vsart EQ '01'.  "Rodoviário
        ENDIF.

        "Recuperar Registros: de Documento de Custo
        IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fknum IS INITIAL AND me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp IS NOT INITIAL.
          SELECT SINGLE fknum INTO @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fknum
            FROM vfkp
           WHERE rebel EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.
        ENDIF.

        IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete IS INITIAL AND me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp IS NOT INITIAL.
          SELECT SINGLE vbeln INTO @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete
            FROM vbak
           WHERE tknum EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.
        ENDIF.

        IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete IS INITIAL AND me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete IS NOT INITIAL.
          SELECT SINGLE vbeln INTO @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete
            FROM vbrp AS p
           WHERE p~aubel EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete
             AND p~shkzg EQ @space
             AND NOT EXISTS ( SELECT * FROM vbrk AS k WHERE k~sfakn EQ p~vbeln ) AND draft = @space .
        ENDIF.

        IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete IS INITIAL AND me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete IS NOT INITIAL.
          SELECT SINGLE j~docnum INTO @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete
            FROM j_1bnflin AS j
            INNER JOIN j_1bnfdoc AS d ON d~docnum = j~docnum AND d~cancel EQ @space
            WHERE j~reftyp EQ 'BI'
              AND j~refkey EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete.
        ENDIF.

        IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete IS NOT INITIAL.
          "Estorna Documentos Eletrônicos
          DATA(lc_cte) =
          zcl_cte=>zif_doc_eletronico~get_instance( i_docnum = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete
            )->set_registro( i_docnum = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete

            "Verifica Possivel estorno completo
            )->get_ck_estorno(

            "Rescindir Contrato Tip
            )->set_autoriza_viagem_tip_frete(
               EXPORTING
                 i_acao          = zif_doc_eletronico=>at_acao_viagem_frete_cancelar
                 i_motivo_cancel = '01'
               IMPORTING
                 e_justificativa_cancel = e_justificativa_cancel

            "Cancelar MDF-e
            )->set_autoriza_mdfe(
               EXPORTING
                 i_acao                 = zif_doc_eletronico=>at_acao_mdfe_cancelar
                 i_justificativa_cancel = e_justificativa_cancel

            "Cancelar CT-e
            )->set_cancelar(
              EXPORTING
                i_motivo    = '01'    " Motivo para estorno/não utilização
                i_aguardar  = abap_true  " Aguardar Cancelamento
            ) .

          TRY .
              lc_cte->get_ck_doc_cancel( ).
            CATCH zcx_doc_eletronico.    "
              TRY .
                  lc_cte->get_ck_numero_nao_determinado( ).
                CATCH zcx_doc_eletronico.    "
                  TRY .
                      lc_cte->get_ck_autorizado_cancel(
                        )->set_clear(
                        ).
                      CLEAR: me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete.
                      me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-cte_autorizado = abap_false.
                      me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-st_proc = '07'.
                      me->zif_doc_fiscal_ft_entrada~set_gravar_registro( ).
                    CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).    "
                      lc_cte->set_clear( ).
                      CLEAR: lc_cte.
                      ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
                      me->zif_doc_fiscal_ft_entrada~get_erro_geral( ).
                  ENDTRY.
              ENDTRY.
          ENDTRY.

          lc_cte->set_clear( ).

        ELSEIF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_mdfe IS NOT INITIAL.

          SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
            FROM j_1bnfdoc
           WHERE docnum EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_mdfe.

          IF sy-subrc IS INITIAL AND wa_j_1bnfdoc-cancel EQ abap_false.
            RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
              EXPORTING
                textid    = VALUE #( msgid = zcx_doc_fiscal_ft_entrada=>zcx_frete_mdfe-msgid
                                     msgno = zcx_doc_fiscal_ft_entrada=>zcx_frete_mdfe-msgno
                                     attr1 = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_mdfe )
                msgid     = zcx_doc_fiscal_ft_entrada=>zcx_frete_mdfe-msgid
                msgno     = zcx_doc_fiscal_ft_entrada=>zcx_frete_mdfe-msgno
                msgty     = 'E'
                msgv1     = CONV #( me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_mdfe )
                transacao = 'ZMDFE'.
          ELSE.

            CLEAR: me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_mdfe.
            me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-st_proc = '05'.
            me->zif_doc_fiscal_ft_entrada~set_gravar_registro( ).

          ENDIF.

        ENDIF.

        IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete IS NOT INITIAL.

          SELECT SINGLE * INTO @DATA(wa_vbrk)
            FROM vbrk
           WHERE vbeln EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete.

          "Não está estornado
          IF wa_vbrk-fksto NE abap_true.

            "Estorna Faturamento da CT-e
            CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
              EXPORTING
                billingdocument = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete
              TABLES
                return          = it_return
                success         = it_success.

            IF it_success IS NOT INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = abap_true.
            ELSE.
              READ TABLE it_return WITH KEY type = 'E' INTO DATA(wa_return).
              IF sy-subrc IS INITIAL.
                sy-msgid = wa_return-id.
                sy-msgno = wa_return-number.
                sy-msgv1 = wa_return-message_v1.
                sy-msgv2 = wa_return-message_v2.
                sy-msgv3 = wa_return-message_v3.
                sy-msgv4 = wa_return-message_v4.
                me->zif_doc_fiscal_ft_entrada~get_erro_geral( ).
              ENDIF.
            ENDIF.
          ENDIF.

          CLEAR: me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete,
                 me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete.
          me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-st_proc = '06'.
          me->zif_doc_fiscal_ft_entrada~set_gravar_registro( ).

        ENDIF.

        "Bloqueia Ordem de Venda de Frete
        IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete IS NOT INITIAL.
          CLEAR: wa_orderheaderin, wa_orderheaderinx, it_bapiparex, it_return_ret2.

          wa_bape_vbak-vbeln      = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete.
          wa_bape_vbak-tknum      = ''.
          wa_bapiparex-structure  = 'BAPE_VBAK'.
          wa_bapiparex-valuepart1 = wa_bape_vbak.
          APPEND wa_bapiparex TO it_bapiparex.

          CLEAR wa_bapiparex.
          wa_bape_vbakx-vbeln     = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete.
          wa_bape_vbakx-tknum     = 'X'.
          wa_bapiparex-structure  = 'BAPE_VBAKX'.
          wa_bapiparex-valuepart1 = wa_bape_vbakx.
          APPEND wa_bapiparex TO it_bapiparex.

          wa_orderheaderin-bill_block  = '10'.
          wa_orderheaderinx-updateflag = 'U'.
          wa_orderheaderinx-bill_block = 'X'.

          DATA(ck_tentar_cancelar) = abap_true.

          WHILE ck_tentar_cancelar EQ abap_true.
            CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                salesdocument    = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete
                order_header_in  = wa_orderheaderin
                order_header_inx = wa_orderheaderinx
              TABLES
                return           = it_return_ret2
                extensionin      = it_bapiparex.

            READ TABLE it_return_ret2 WITH KEY type = 'E' INTO DATA(wa_return_ret2).
            IF sy-subrc IS NOT INITIAL.
              ck_tentar_cancelar = abap_false.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = abap_true.

              CLEAR: me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete.
              me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-st_proc  = '05'.
              me->zif_doc_fiscal_ft_entrada~set_gravar_registro( ).
            ELSE.
              IF NOT ( wa_return_ret2-id EQ 'V1' AND wa_return_ret2-number EQ '042' ).
                sy-msgid = wa_return_ret2-id.
                sy-msgno = wa_return_ret2-number.
                sy-msgv1 = wa_return_ret2-message_v1.
                sy-msgv2 = wa_return_ret2-message_v2.
                sy-msgv3 = wa_return_ret2-message_v3.
                sy-msgv4 = wa_return_ret2-message_v4.
                me->zif_doc_fiscal_ft_entrada~get_erro_geral( ).
              ENDIF.
              "Aguardar
              WAIT UP TO 2 SECONDS.
            ENDIF.
          ENDWHILE.

        ENDIF.

        "Estornar Custo """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        me->zif_doc_fiscal_ft_entrada~set_estornar_doc_custo( ).
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        "Estorna Documento de Transporte """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        me->zif_doc_fiscal_ft_entrada~set_estornar_doc_transporte( ).
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        IF i_estornar_aviso EQ abap_true.

          "Estorna Documento de Aviso """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          me->zif_doc_fiscal_ft_entrada~set_estornar_aviso( ).
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          DELETE FROM zlest0110
           WHERE ebeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebeln
             AND ebelp EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebelp
             AND werks EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-werks
             AND lgort EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-lgort
             AND charg EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-charg
             AND vbeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln.

          DELETE FROM zlest0109
           WHERE ebeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebeln
             AND ebelp EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebelp
             AND werks EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-werks
             AND lgort EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-lgort
             AND charg EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-charg
             AND vbeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln.

          DELETE FROM zlest0108
           WHERE ebeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebeln
             AND ebelp EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebelp
             AND werks EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-werks
             AND lgort EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-lgort
             AND charg EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-charg
             AND vbeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln.

          COMMIT WORK.

          me->zif_doc_fiscal_ft_entrada~set_clear( ).

        ELSE.


        ENDIF.

      CATCH zcx_doc_eletronico INTO ex_doc_eletronico.
        ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
        me->zif_doc_fiscal_ft_entrada~get_erro_geral( ).
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_ESTORNAR_DOC_CUSTO.

    DATA: LC_DATA(10),
          LC_SHDB     TYPE REF TO ZCL_SHDB.

    CONCATENATE SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM+0(4) INTO LC_DATA.

    R_INSTANCIA = ME.

    IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(VFKK)
        FROM VFKK
       WHERE FKNUM EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM.

      IF SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO @DATA(WA_VFKP)
          FROM VFKP
         WHERE FKNUM EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM
           AND FKSTO EQ @ABAP_FALSE.

        IF SY-SUBRC IS INITIAL.

          TRY .
              SELECT SINGLE * INTO @DATA(WA_VTTK)
                FROM VTTK
               WHERE TKNUM EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP.

              SELECT SINGLE * INTO @DATA(WA_VTTP)
                FROM VTTP
               WHERE TKNUM EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP.

              SELECT SINGLE * INTO @DATA(WA_LIKP)
                FROM LIKP
               WHERE VBELN EQ @WA_VTTP-VBELN.

              DATA(LC_ESTORNAR) = ABAP_TRUE.
              WHILE LC_ESTORNAR = ABAP_TRUE.

                "ZLESR0092_FORM
                "ZLESR0092_FORM
                CASE WA_LIKP-INCO1.
                  WHEN ZIF_CARGA=>ST_TP_FRETE_CPT.
                    CASE WA_VTTK-SHTYP.
                      WHEN 'Z001'.
                        LC_SHDB = CAST #(
                        ZCL_SHDB=>ZIF_SHDB~GET_INSTANCE(
                         )->SET_CLEAR(
                         )->SET_TRANSACTION( I_TCODE = 'VI02'
                         )->SET_MODE( I_MODE = ZIF_SHDB=>ST_TIPO_MODE_SEM_TELA_SEM_DEBU
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0020' DYNBEGIN = 'X' FNAM = ''               FVAL = '' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'     FVAL = 'VFKK-FKNUM' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=UEBP' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKK-FKNUM'     FVAL = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0030' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'     FVAL = 'VFKP-FKPOS(02)' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=PLOE' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VIM_MARKED(02)' FVAL = 'X' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPLSPO1' DYNPRO = '0100' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=YES' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0030' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'     FVAL = 'VFKP-FKPOS(01)' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=PDET' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'     FVAL = 'VFKP-POSTX' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=PABR' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=SICH' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKPD-SLSTOR'   FVAL = 'X' )
                         )->SET_EXECUTAR(
                         ) ).

                      WHEN OTHERS.
                        LC_SHDB = CAST #(
                        ZCL_SHDB=>ZIF_SHDB~GET_INSTANCE(
                         )->SET_CLEAR(
                         )->SET_TRANSACTION( I_TCODE = 'VI02'
                         )->SET_MODE( I_MODE = ZIF_SHDB=>ST_TIPO_MODE_SEM_TELA_SEM_DEBU
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0020' DYNBEGIN = 'X' FNAM = ''             FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'   FVAL = 'VFKK-FKNUM' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'   FVAL = '=UEBP' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKK-FKNUM'   FVAL = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0030' DYNBEGIN = 'X' FNAM = ''             FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'   FVAL = 'VFKP-FKPOS(01)' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'   FVAL = '=PDET' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''             FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'   FVAL = 'VFKP-POSTX' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'   FVAL = '=PABR' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''             FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'   FVAL = '=SICH' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKPD-SLSTOR' FVAL = 'X' )
                         )->SET_EXECUTAR(
                         ) ).

                    ENDCASE.
                  WHEN OTHERS.
                    CASE WA_VTTK-SHTYP.
                      WHEN 'Z001'.
                        LC_SHDB = CAST #(
                        ZCL_SHDB=>ZIF_SHDB~GET_INSTANCE(
                         )->SET_CLEAR(
                         )->SET_TRANSACTION( I_TCODE = 'VI02'
                         )->SET_MODE( I_MODE = ZIF_SHDB=>ST_TIPO_MODE_SEM_TELA_SEM_DEBU
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0020' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'     FVAL = 'VFKK-FKNUM' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=UEBP' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKK-FKNUM'     FVAL = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0030' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'     FVAL = 'VFKP-FKPOS(02)' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=PLOE' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VIM_MARKED(02)' FVAL = 'X' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPLSPO1' DYNPRO = '0100' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=YES' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0030' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'     FVAL = 'VFKP-FKPOS(01)' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=PDET' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=PABR' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=KLAC' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKPD-SLSTOR'   FVAL = 'X' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '/00' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKPD-STDAT'    FVAL = LC_DATA )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''               FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'     FVAL = '=SICH' )
                         )->SET_EXECUTAR(
                         ) ).

                      WHEN OTHERS.
                        LC_SHDB = CAST #(
                        ZCL_SHDB=>ZIF_SHDB~GET_INSTANCE(
                         )->SET_CLEAR(
                         )->SET_TRANSACTION( I_TCODE = 'VI02'
                         )->SET_MODE( I_MODE = ZIF_SHDB=>ST_TIPO_MODE_SEM_TELA_SEM_DEBU
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0020' DYNBEGIN = 'X' FNAM = ''             FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'   FVAL = 'VFKK-FKNUM' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'   FVAL = '=UEBP' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKK-FKNUM'   FVAL = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0030' DYNBEGIN = 'X' FNAM = ''             FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR'   FVAL = 'VFKP-FKPOS(01)' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'   FVAL = '=PDET' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''             FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'   FVAL = '=PABR' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''             FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'   FVAL = '=KLAC' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKPD-SLSTOR' FVAL = 'X' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''             FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'   FVAL = '/00' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKPD-STDAT'  FVAL = LC_DATA )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0040' DYNBEGIN = 'X' FNAM = ''             FVAL = ' ' )
                         )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE'   FVAL = '=SICH' )
                         )->SET_EXECUTAR(
                         ) ).
                    ENDCASE.
                ENDCASE.

                TRY .
                    LC_SHDB->ZIF_SHDB~GET_CK_EXISTE_MSG_ERRO( IMPORTING E_MSG = DATA(E_MSG) ).
                    IF NOT ( E_MSG-MSGID EQ 'VY' AND E_MSG-MSGNR EQ '011' ).
                      LC_ESTORNAR = ABAP_FALSE.
                      MESSAGE ID E_MSG-MSGID TYPE 'S' NUMBER E_MSG-MSGNR WITH E_MSG-MSGV1 E_MSG-MSGV2 E_MSG-MSGV3 E_MSG-MSGV4.
                      ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
                    ENDIF.
                  CATCH ZCX_SHDB INTO DATA(EX_SHDB).
                    LC_ESTORNAR = ABAP_FALSE.
                    COMMIT WORK.
                    WAIT UP TO 2 SECONDS.
                ENDTRY.
              ENDWHILE.

            CATCH ZCX_SHDB INTO EX_SHDB.
              EX_SHDB->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
              ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
          ENDTRY.

        ENDIF.

        TRY .
            LC_ESTORNAR = ABAP_TRUE.
            WHILE LC_ESTORNAR = ABAP_TRUE.
              LC_SHDB = CAST #(
                      ZCL_SHDB=>ZIF_SHDB~GET_INSTANCE(
                       )->SET_CLEAR(
                       )->SET_TRANSACTION( I_TCODE = 'VI02'
                       )->SET_MODE( I_MODE = ZIF_SHDB=>ST_TIPO_MODE_SEM_TELA_SEM_DEBU
                       )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0020' DYNBEGIN = 'X' FNAM = ''           FVAL = ' ' )
                       )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_CURSOR' FVAL = 'VFKK-FKNUM' )
                       )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE' FVAL = '=UEBP' )
                       )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'VFKK-FKNUM' FVAL = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM )
                       )->SET_ADD_BDCDATA( VALUE #( PROGRAM = 'SAPMV54A' DYNPRO = '0030' DYNBEGIN = 'X' FNAM = ''           FVAL = ' ' )
                       )->SET_ADD_BDCDATA( VALUE #( PROGRAM = ''         DYNPRO = ''     DYNBEGIN = ''  FNAM = 'BDC_OKCODE' FVAL = '/ELOES' )
                       )->SET_EXECUTAR(
                       ) ).

              TRY .
                  LC_SHDB->ZIF_SHDB~GET_CK_EXISTE_MSG_ERRO( IMPORTING E_MSG = E_MSG ).
                  IF NOT ( E_MSG-MSGID EQ 'VY' AND E_MSG-MSGNR EQ '011' ).
                    LC_ESTORNAR = ABAP_FALSE.
                    MESSAGE ID E_MSG-MSGID TYPE 'S' NUMBER E_MSG-MSGNR WITH E_MSG-MSGV1 E_MSG-MSGV2 E_MSG-MSGV3 E_MSG-MSGV4.
                    ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
                  ENDIF.
                CATCH ZCX_SHDB INTO EX_SHDB.
                  LC_ESTORNAR = ABAP_FALSE.
                  COMMIT WORK.
                  WAIT UP TO 2 SECONDS.
              ENDTRY.

            ENDWHILE.

            CLEAR: ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM.
            ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '04'.
            ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).

          CATCH ZCX_SHDB INTO EX_SHDB.
            EX_SHDB->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
            ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
        ENDTRY.
      ELSE.
        CLEAR: ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '04'.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_ESTORNAR_DOC_TRANSPORTE.

    DATA: WA_HEADERDATA       TYPE BAPISHIPMENTHEADER,
          WA_HEADERDATAACTION TYPE BAPISHIPMENTHEADERACTION,
          WA_ITEMDATA         TYPE BAPISHIPMENTITEM,
          IT_ITEMDATA         TYPE TABLE OF BAPISHIPMENTITEM,
          WA_VTTP             TYPE VTTP,
          WA_ITEMDATAACTION   TYPE BAPISHIPMENTITEMACTION,
          IT_ITEMDATAACTION   TYPE TABLE OF BAPISHIPMENTITEMACTION,
          IT_RETURN           TYPE TABLE OF BAPIRET2.

    R_INSTANCIA = ME.

    IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(WA_VTTK)
        FROM VTTK
       WHERE TKNUM EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP.

      IF SY-SUBRC IS INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP
          IMPORTING
            OUTPUT = WA_HEADERDATA-SHIPMENT_NUM.

        WA_HEADERDATAACTION-SHIPMENT_NUM     = 'D'.
        WA_HEADERDATAACTION-SERVICE_AGENT_ID = 'D'.

        SELECT * INTO WA_VTTP
          FROM VTTP
         WHERE TKNUM EQ ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP.

          WA_ITEMDATA-DELIVERY  = WA_VTTP-VBELN.
          WA_ITEMDATA-ITENERARY = WA_VTTP-TPNUM.
          APPEND WA_ITEMDATA TO IT_ITEMDATA.

          WA_ITEMDATAACTION-DELIVERY  = 'D'.
          WA_ITEMDATAACTION-ITENERARY = 'D'.
          APPEND WA_ITEMDATAACTION TO IT_ITEMDATAACTION.
        ENDSELECT.

        CLEAR: IT_RETURN, IT_RETURN[].

        CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
          EXPORTING
            HEADERDATA       = WA_HEADERDATA
            HEADERDATAACTION = WA_HEADERDATAACTION
          TABLES
            ITEMDATA         = IT_ITEMDATA
            ITEMDATAACTION   = IT_ITEMDATAACTION
            RETURN           = IT_RETURN.

        READ TABLE IT_RETURN WITH KEY TYPE = 'E' INTO DATA(WA_RETURN).
        IF SY-SUBRC IS INITIAL.
          MESSAGE ID WA_RETURN-ID TYPE 'S' NUMBER WA_RETURN-NUMBER WITH WA_RETURN-MESSAGE_V1 WA_RETURN-MESSAGE_V2 WA_RETURN-MESSAGE_V3 WA_RETURN-MESSAGE_V4.
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
        ENDIF.

        IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_CARGA IS NOT INITIAL.
          DELETE FROM ZSDT0001FEUFS WHERE ID_CARGA EQ ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_CARGA.
          DELETE FROM ZSDT0001FE WHERE ID_CARGA EQ ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_CARGA.
        ENDIF.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

      ENDIF.

      CLEAR: ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP,
             ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-KBETR,
             ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC.

      ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_GERAR_ADIANTAMENTO_PEDAGIO.

    DATA: E_ID_PROC_CLIENTE TYPE ZDE_ID_PROC_CLIENTE,
          E_GRAVOU          TYPE CHAR01.

    DATA: OBJ_REPOM    TYPE REF TO ZCL_REPOM_VIAGEM_VPR,
          E_ERROS	     TYPE ZDE_REPOM_ERROS_T,
          WA_ZLEST0123 TYPE ZLEST0123.

    TRY .
        E_ID_PROC_CLIENTE =
            ZCL_REPOM_VIAGEM_VPR=>CRIAR_VIAGEM(
              EXPORTING
                I_TKNUM             = I_TKNUM
                I_CD_CID_ORIGEM     = I_CD_CID_ORIGEM
                I_CD_CID_DESTINO    = I_CD_CID_DESTINO
                I_NR_CARTAO         = I_NR_CARTAO
                I_ID_ROTA_REPOM     = I_ID_ROTA_REPOM
                I_ID_PERCURSO_REPOM = I_ID_PERCURSO_REPOM
              IMPORTING
                E_GRAVOU = E_GRAVOU ).

      CATCH CX_ROOT INTO DATA(EX_ROOT).
        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGID
                              MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGNO
                              ATTR1 = CONV #( I_CD_CID_ORIGEM )
                              ATTR2 = CONV #( I_CD_CID_DESTINO ) )
            MSGTY  = 'E'
            MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGID
            MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGNO
            MSGV1  = CONV #( I_CD_CID_ORIGEM )
            MSGV2  = CONV #( I_CD_CID_DESTINO ).
    ENDTRY.

    "Autorizar
    CREATE OBJECT OBJ_REPOM
      EXPORTING
        I_ID_PROC_CLIENTE = E_ID_PROC_CLIENTE.

    CALL METHOD OBJ_REPOM->SOLICITAR
      IMPORTING
        E_ERROS                    = E_ERROS
      EXCEPTIONS
        SERVICO_NAO_ENCONTRADO     = 1
        HTTP_COMMUNICATION_FAILURE = 2
        HTTP_INVALID_STATE         = 3
        HTTP_PROCESSING_FAILED     = 4
        HTTP_INVALID_TIMEOUT       = 5
        ERRO                       = 6
        OTHERS                     = 7.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
    ENDIF.

    DATA(E_AUTORIZADO) = ZCL_REPOM_VIAGEM_VPR=>GET_AUTORIZADO( EXPORTING I_ID_PROC_CLIENTE = E_ID_PROC_CLIENTE ).

    OBJ_REPOM->GET_REGISTRO( IMPORTING E_REGISTRO = E_ZLEST0123 ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_GERAR_DOCUMENTO_TRANSPORTE.

    R_INSTANCIA = ME.

    "Busca/Valida Tarifa de Frete
    "Valida Tarifa de Seguro
    "Valida Tarifa de IOF
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_PRECO_TARIFA_FRETE(
          IMPORTING
            E_TARIFA = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-KBETR
     )->GET_ITINERARIO(
          IMPORTING
            E_TVRO = DATA(E_TVRO)
     )->GET_CK_CONDICAO_ZSEG(
     )->GET_CK_CONDICAO_ZIOF(
     )->GET_TIPO_TRANSPORTE(
          IMPORTING
            E_SHTYP = DATA(E_SHTYP)
     )->GET_LOCAL_COLETA(
          IMPORTING
            E_LFA1  = DATA(LC_COLETA)
     )->GET_INFO_TIPO_TRANSPORTE(
          EXPORTING
            I_SHTYP = E_SHTYP
          IMPORTING
            E_TVTK  = DATA(E_TVTK)
     )->GET_CK_ADIANTAMENTO(
     )->GET_CK_ITINERARIO_PEDAGIO(
     ).

    ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_CRIAR_DOC_TRANSPORTE(
           EXPORTING
             I_TVRO   = E_TVRO
             I_SHTYP  = E_SHTYP
             I_TVTK   = E_TVTK
           IMPORTING
             E_DOC_TRANSP = DATA(E_DOC_TRANSP)
      )->SET_CRIAR_DOC_CUSTO(
           IMPORTING
             E_FKNUM = DATA(E_FKNUM)
             E_OV_FRETE = DATA(E_OV_FRETE)
             E_FATURA_FRETE = DATA(E_FATURA_FRETE)
      )->SET_ATUALIZA_CONHECIMENTO(
           IMPORTING
             E_NRO_NF_FRETE = DATA(E_NRO_NF_FRETE)
      )->SET_AUTORIZAR_DOCUMENTO(
      )->SET_GERAR_MDFE_AVULSA(
      ).

      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '99'.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_GERAR_MDFE_AVULSA.

    R_INSTANCIA = ME.

    TRY .
        ZCL_FATURAMENTO=>ZIF_FATURAMENTO~GET_INSTANCE(
          )->GET_PROCESSO_EMISSAO_DOCS(
            EXPORTING
              I_TKNUM       = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP
            IMPORTING
              E_CONHECIMENTO = DATA(E_CONHECIMENTO)
              E_MANIFESTO    = DATA(E_MANIFESTO)
          ).

      CATCH ZCX_FATURAMENTO INTO DATA(EX_FATURAMENTO).

        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID  = EX_FATURAMENTO->MSGID
                              MSGNO  = EX_FATURAMENTO->MSGNO
                              ATTR1  = EX_FATURAMENTO->MSGV1
                              ATTR2  = EX_FATURAMENTO->MSGV2
                              ATTR3  = EX_FATURAMENTO->MSGV3
                              ATTR4  = EX_FATURAMENTO->MSGV4 )
            MSGID  = EX_FATURAMENTO->MSGID
            MSGNO  = EX_FATURAMENTO->MSGNO
            MSGV1  = EX_FATURAMENTO->MSGV1
            MSGV2  = EX_FATURAMENTO->MSGV2
            MSGV3  = EX_FATURAMENTO->MSGV3
            MSGV4  = EX_FATURAMENTO->MSGV4
            MSGTY  = 'E'.

      CATCH ZCX_ERROR INTO DATA(EX_ERROR).

        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID  = EX_ERROR->MSGID
                              MSGNO  = EX_ERROR->MSGNO
                              ATTR1  = EX_ERROR->MSGV1
                              ATTR2  = EX_ERROR->MSGV2
                              ATTR3  = EX_ERROR->MSGV3
                              ATTR4  = EX_ERROR->MSGV4 )
            MSGID  = EX_ERROR->MSGID
            MSGNO  = EX_ERROR->MSGNO
            MSGV1  = EX_ERROR->MSGV1
            MSGV2  = EX_ERROR->MSGV2
            MSGV3  = EX_ERROR->MSGV3
            MSGV4  = EX_ERROR->MSGV4
            MSGTY  = 'E'.

    ENDTRY.

    "Verificar se Nota Eletrônica """"""""""""""""""""""""""""""""""""""""""""""""""
    IF E_MANIFESTO EQ ABAP_TRUE.
      SELECT * INTO TABLE @DATA(IT_ZLEST0110)
        FROM ZLEST0110
       WHERE VBELN EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
         AND CHAVE NE @SPACE.

      IF SY-SUBRC IS NOT INITIAL.
        E_MANIFESTO = ABAP_FALSE.
      ENDIF.
    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF E_CONHECIMENTO EQ ABAP_FALSE AND E_MANIFESTO EQ ABAP_TRUE.


      IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_MDFE IS NOT INITIAL.
        SELECT SINGLE * INTO @DATA(WA_MDFE)
          FROM J_1BNFDOC
         WHERE DOCNUM EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_MDFE.

        IF ( SY-SUBRC IS NOT INITIAL ) OR ( SY-SUBRC IS INITIAL AND WA_MDFE-CANCEL EQ ABAP_TRUE ).
          CLEAR: ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_MDFE.
        ENDIF.

      ENDIF.

      CHECK ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_MDFE IS INITIAL.

      DATA(LOCAL_DESCARGA) = CAST ZCL_CLIENTES( ZCL_CLIENTES=>ZIF_PARCEIROS~GET_INSTANCE(
                                                  )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA
                                                  )->GET_NAME( ) )->AT_KNA1.

      DATA(LOCAL_COLETA)   = CAST ZCL_FORNECEDORES( ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
                                                      )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA
                                                      )->GET_NAME( ) )->AT_LFA1.

      SELECT SINGLE * INTO @DATA(WA_T001W)
        FROM T001W
       WHERE WERKS EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-WERKS.

      SELECT SINGLE * INTO @DATA(WA_J_1BBRANCH)
        FROM J_1BBRANCH
       WHERE BRANCH EQ @WA_T001W-J_1BBRANCH.

      DATA: WA_ZSDT0237 TYPE ZSDT0237,
            WA_ZSDT0243 TYPE ZSDT0243,
            WA_ZSDT0238 TYPE ZSDT0238,
            WA_ZSDT0239 TYPE ZSDT0239,
            WA_ZSDT0240 TYPE ZSDT0240,
            WA_ZSDT0241 TYPE ZSDT0241,
            WA_ZSDT0242 TYPE ZSDT0242,
            GT_ZSDT0238 TYPE TABLE OF ZSDT0238,
            GT_ZSDT0239 TYPE TABLE OF ZSDT0239,
            GT_ZSDT0243 TYPE TABLE OF ZSDT0243,
            GT_ZSDT0240 TYPE TABLE OF ZSDT0240,
            GT_ZSDT0241 TYPE TABLE OF ZSDT0241,
            GT_ZSDT0242 TYPE TABLE OF ZSDT0242.

      WA_ZSDT0237-BUKRS         = WA_J_1BBRANCH-BUKRS.
      WA_ZSDT0237-BRANCH        = WA_J_1BBRANCH-BRANCH.
      WA_ZSDT0237-TPEMIT        = '2'. "Transportador de Carga Própria
      WA_ZSDT0237-COUNTRY       = 'BR'.
      WA_ZSDT0237-CMUNINI       = LOCAL_COLETA-TXJCD.

      SELECT SINGLE * INTO @DATA(WA_ADRC)
        FROM ADRC
       WHERE ADDRNUMBER EQ @LOCAL_COLETA-ADRNR.

      SELECT SINGLE * INTO @DATA(WA_TZONE)
        FROM TZONE
       WHERE LAND1 EQ @LOCAL_COLETA-LAND1
         AND ZONE1 EQ @LOCAL_COLETA-LZONE.

      DATA(LC_CEP) = COND STRING( WHEN WA_TZONE-ZCEP IS NOT INITIAL THEN WA_TZONE-ZCEP ELSE WA_ADRC-POST_CODE1 ).
      WA_ZSDT0237-LOCC_CEP      = ZCL_STRING=>REPLACE( I_STR = LC_CEP I_CHAR_OLD   = '-' I_CHAR_NEW   = '' ).

      WA_ZSDT0237-CMUNFIM       = LOCAL_DESCARGA-TXJCD.

      CLEAR: LC_CEP, WA_ADRC, WA_TZONE.

      SELECT SINGLE * INTO @WA_ADRC
        FROM ADRC
       WHERE ADDRNUMBER EQ @LOCAL_DESCARGA-ADRNR.

      SELECT SINGLE * INTO @WA_TZONE
        FROM TZONE
       WHERE LAND1 EQ @LOCAL_DESCARGA-LAND1
         AND ZONE1 EQ @LOCAL_DESCARGA-LZONE.

      LC_CEP = COND STRING( WHEN WA_TZONE-ZCEP IS NOT INITIAL THEN WA_TZONE-ZCEP ELSE WA_ADRC-POST_CODE1 ).

      WA_ZSDT0237-LOCD_CEP      = ZCL_STRING=>REPLACE( I_STR = LC_CEP I_CHAR_OLD   = '-' I_CHAR_NEW   = '' ).
      WA_ZSDT0237-CUNID         = '01'.
      WA_ZSDT0237-MODAL         = '01'.
      WA_ZSDT0237-INDCANALVERDE = '1'.

      "Município de Carregamento """""""""""""""""""""""""""""""""
      WA_ZSDT0238-SEQUENCIA     = 1.
      WA_ZSDT0238-COUNTRY       = WA_ZSDT0237-COUNTRY.
      WA_ZSDT0238-CMUNCAR       = WA_ZSDT0237-CMUNINI.
      APPEND WA_ZSDT0238 TO GT_ZSDT0238.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "Município de Descarregamentp """"""""""""""""""""""""""""""
      WA_ZSDT0239-SEQUENCIA     = 1.
      WA_ZSDT0239-COUNTRY       = WA_ZSDT0237-COUNTRY.
      WA_ZSDT0239-CMUNDESC      = WA_ZSDT0237-CMUNFIM.
      APPEND WA_ZSDT0239 TO GT_ZSDT0239.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "UF's do Percurso """"""""""""""""""""""""""""""""""""""""""
      SELECT * INTO TABLE @DATA(IT_ZSDT0001FEUFS)
        FROM ZSDT0001FEUFS
       WHERE ID_CARGA EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_CARGA
         AND ID_CARGA NE @SPACE.

      LOOP AT IT_ZSDT0001FEUFS INTO DATA(WA_ZSDT0001FEUFS).
        CLEAR: WA_ZSDT0240.
        WA_ZSDT0240-SEQUENCIA = WA_ZSDT0001FEUFS-NM_SEQUENCIA.
        WA_ZSDT0240-LAND1     = WA_ZSDT0001FEUFS-LAND1.
        WA_ZSDT0240-BLAND     = WA_ZSDT0001FEUFS-BLAND.
        APPEND WA_ZSDT0240 TO GT_ZSDT0240.
      ENDLOOP.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_AGRUPA_FRETE IS NOT INITIAL AND ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_CARGA IS NOT INITIAL.

        SELECT * INTO TABLE @DATA(IT_ZLEST0108)
          FROM ZLEST0108
         WHERE ID_AGRUPA_FRETE EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_AGRUPA_FRETE
           AND ID_AGRUPA_FRETE NE @SPACE.

        LOOP AT IT_ZLEST0108 ASSIGNING FIELD-SYMBOL(<FS_ZLEST0108>).
          IF <FS_ZLEST0108>-ID_CARGA IS INITIAL.
            CONTINUE.
          ENDIF.

          SELECT SINGLE * INTO @DATA(WA_ZSDT0001CG)
            FROM ZSDT0001CG
           WHERE ID_CARGA EQ @<FS_ZLEST0108>-ID_CARGA.

          IF SY-SUBRC IS INITIAL.

            WA_ZSDT0237-MATNR = WA_ZSDT0001CG-ID_PRODUTO.

            CLEAR: WA_ZSDT0237-TPCARGA,
                   WA_ZSDT0237-XPROD.

            IF WA_ZSDT0237-MATNR IS NOT INITIAL.

              SELECT SINGLE MATKL INTO @WA_ZSDT0237-MATKL
                FROM MARA
               WHERE MATNR EQ @WA_ZSDT0237-MATNR.

              IF SY-SUBRC IS INITIAL.
                SELECT SINGLE * INTO @DATA(WA_ZLEST0193)
                  FROM ZLEST0193
                 WHERE MATKL EQ @WA_ZSDT0237-MATKL.

                IF SY-SUBRC IS INITIAL.
                  WA_ZSDT0237-TPCARGA = ZCL_STRING=>LPAD( I_STR  = CONV #( WA_ZLEST0193-TIPO_CARGA ) I_QTD  = 2 I_CHAR = '0' ).
                ENDIF.

                SELECT SINGLE MAKTX INTO @WA_ZSDT0237-XPROD
                  FROM MAKT
                 WHERE MATNR EQ @WA_ZSDT0237-MATNR
                   AND SPRAS EQ @SY-LANGU.
              ENDIF.
            ENDIF.
          ENDIF.

          SELECT SINGLE * INTO @DATA(WA_ZSDT0001NT)
            FROM ZSDT0001NT
           WHERE ID_CARGA EQ @<FS_ZLEST0108>-ID_CARGA
             AND ID_NOTA  EQ @<FS_ZLEST0108>-ID_NOTA.

          IF SY-SUBRC IS INITIAL.

            "Soma dos Valores e Volumes do Agrupamento """""""""""""""""
            ADD WA_ZSDT0001NT-NR_QUANTIDADE TO WA_ZSDT0237-QCARGA.
            ADD WA_ZSDT0001NT-NR_VALOR TO WA_ZSDT0237-VCARGA.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            "Documentos Referenciados """"""""""""""""""""""""""""""""""
            IF WA_ZSDT0001NT-NR_CHAVE_NFE IS NOT INITIAL.
              CLEAR: WA_ZSDT0241.
              WA_ZSDT0241-CHAVE = WA_ZSDT0001NT-NR_CHAVE_NFE.
              WA_ZSDT0241-SEQUENCIA = WA_ZSDT0239-SEQUENCIA.
              APPEND WA_ZSDT0241 TO GT_ZSDT0241.
            ENDIF.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          ENDIF.

        ENDLOOP.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

        "Motorista """""""""""""""""""""""""""""""""""""""""""""""
        IF WA_ZSDT0001CG-ID_MOTORISTA IS NOT INITIAL.
          CLEAR: WA_ZSDT0242.
          WA_ZSDT0242-LIFNR = WA_ZSDT0001CG-ID_MOTORISTA.
          APPEND WA_ZSDT0242 TO GT_ZSDT0242.
        ENDIF.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

        "Placa Veículos """"""""""""""""""""""""""""""""""""""""""
        IF WA_ZSDT0001CG-DS_PLACA_TRATOR IS NOT INITIAL.

          SELECT SINGLE * INTO @DATA(WA_ZLEST0002)
            FROM ZLEST0002
           WHERE PC_VEICULO EQ @WA_ZSDT0001CG-DS_PLACA_TRATOR.

          IF SY-SUBRC IS NOT INITIAL.
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                                  MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                                  ATTR1  = WA_ZSDT0001CG-DS_PLACA_TRATOR )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                MSGV1  = CONV #( WA_ZSDT0001CG-DS_PLACA_TRATOR )
                MSGTY  = 'E'.
          ENDIF.

          SELECT SINGLE *
            FROM LFA1 INTO @DATA(WL_LFA1_PROP)
           WHERE LIFNR = @WA_ZLEST0002-PROPRIETARIO.

          IF ( WL_LFA1_PROP IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ADRC INTO @WA_ADRC
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.

            SELECT SINGLE *
              FROM ADR6 INTO @DATA(WA_ADR6)
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.
          ENDIF.

          WA_ZSDT0243-PC_VEICULO = WA_ZSDT0001CG-DS_PLACA_TRATOR.
          WA_ZSDT0243-CPF        = WL_LFA1_PROP-STCD2.
          WA_ZSDT0243-CNPJ       = WL_LFA1_PROP-STCD1.
          WA_ZSDT0243-RNTRC      = WL_LFA1_PROP-BAHNS.
          WA_ZSDT0243-X_NOME     = WL_LFA1_PROP-NAME1.
          WA_ZSDT0243-IE         = WL_LFA1_PROP-STCD3.
          WA_ZSDT0243-UF_PROP    = WA_ADRC-REGION.
          WA_ZSDT0243-TP_PROP    = '0'.
          WA_ZSDT0243-C_INT      = WA_ZLEST0002-PROPRIETARIO.
          WA_ZSDT0243-RENAVAM    = WA_ZLEST0002-CD_RENAVAM.
          WA_ZSDT0243-TARA       = WA_ZLEST0002-TARA.
          WA_ZSDT0243-CAP_KG     = WA_ZLEST0002-CAP_KG.
          WA_ZSDT0243-CAP_M3     = WA_ZLEST0002-CAP_M3.
          WA_ZSDT0243-TP_ROD     = WA_ZLEST0002-TP_RODADO.
          WA_ZSDT0243-TP_CAR     = WA_ZLEST0002-TP_CARROCERIA2.
          WA_ZSDT0243-UF_VEIC    = WA_ZLEST0002-CD_UF.

          "Tipo de Emissão Igual a 2 não informa tipo do transporte
          "IF WL_LFA1_PROP-STKZN EQ ABAP_FALSE.
          "  WA_ZSDT0237-TPTRANSP = '2'. "ETC
          "ELSE.
          "  WA_ZSDT0237-TPTRANSP = '1'. "TAC
          "ELSE.

          CASE WA_ZLEST0002-TP_VEICULO.
            WHEN '0'.
              WA_ZSDT0243-TP_ROD = '03'.
            WHEN '1'.
              IF WA_ZSDT0243-TP_ROD = '0' OR WA_ZSDT0243-TP_ROD = '00'.
                WA_ZSDT0243-TP_ROD = '03'.
              ENDIF.
          ENDCASE.

          APPEND WA_ZSDT0243 TO GT_ZSDT0243.

        ENDIF.

        IF WA_ZSDT0001CG-DS_PLACA_REBOQ_1 IS NOT INITIAL.

          SELECT SINGLE * INTO @WA_ZLEST0002
            FROM ZLEST0002
           WHERE PC_VEICULO EQ @WA_ZSDT0001CG-DS_PLACA_REBOQ_1.

          IF SY-SUBRC IS NOT INITIAL.
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                                  MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                                  ATTR1  = WA_ZSDT0001CG-DS_PLACA_REBOQ_1 )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                MSGV1  = CONV #( WA_ZSDT0001CG-DS_PLACA_REBOQ_1 )
                MSGTY  = 'E'.
          ENDIF.

          SELECT SINGLE *
            FROM LFA1 INTO @WL_LFA1_PROP
           WHERE LIFNR = @WA_ZLEST0002-PROPRIETARIO.

          IF ( WL_LFA1_PROP IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ADRC INTO @WA_ADRC
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.

            SELECT SINGLE *
              FROM ADR6 INTO @WA_ADR6
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.
          ENDIF.

          WA_ZSDT0243-PC_VEICULO = WA_ZSDT0001CG-DS_PLACA_REBOQ_1.
          WA_ZSDT0243-CPF     = WL_LFA1_PROP-STCD2.
          WA_ZSDT0243-CNPJ    = WL_LFA1_PROP-STCD1.
          WA_ZSDT0243-RNTRC   = WL_LFA1_PROP-BAHNS.
          WA_ZSDT0243-X_NOME  = WL_LFA1_PROP-NAME1.
          WA_ZSDT0243-IE      = WL_LFA1_PROP-STCD3.
          WA_ZSDT0243-UF_PROP = WA_ADRC-REGION.
          WA_ZSDT0243-TP_PROP = '0'.

          WA_ZSDT0243-C_INT    = WA_ZLEST0002-PROPRIETARIO.
          WA_ZSDT0243-RENAVAM  = WA_ZLEST0002-CD_RENAVAM.
          WA_ZSDT0243-TARA     = WA_ZLEST0002-TARA.
          WA_ZSDT0243-CAP_KG   = WA_ZLEST0002-CAP_KG.
          WA_ZSDT0243-CAP_M3   = WA_ZLEST0002-CAP_M3.
          WA_ZSDT0243-TP_ROD   = WA_ZLEST0002-TP_RODADO.
          WA_ZSDT0243-TP_CAR   = WA_ZLEST0002-TP_CARROCERIA2.
          WA_ZSDT0243-UF_VEIC  = WA_ZLEST0002-CD_UF.

          CASE WA_ZLEST0002-TP_VEICULO.
            WHEN '0'.
              WA_ZSDT0243-TP_ROD = '03'.
            WHEN '1'.
              IF WA_ZSDT0243-TP_ROD = '0' OR WA_ZSDT0243-TP_ROD = '00'.
                WA_ZSDT0243-TP_ROD = '03'.
              ENDIF.
          ENDCASE.

          APPEND WA_ZSDT0243 TO GT_ZSDT0243.

        ENDIF.

        IF WA_ZSDT0001CG-DS_PLACA_REBOQ_2 IS NOT INITIAL.

          SELECT SINGLE * INTO @WA_ZLEST0002
            FROM ZLEST0002
           WHERE PC_VEICULO EQ @WA_ZSDT0001CG-DS_PLACA_REBOQ_2.

          IF SY-SUBRC IS NOT INITIAL.
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                                  MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                                  ATTR1  = WA_ZSDT0001CG-DS_PLACA_REBOQ_2 )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                MSGV1  = CONV #( WA_ZSDT0001CG-DS_PLACA_REBOQ_2 )
                MSGTY  = 'E'.
          ENDIF.

          SELECT SINGLE *
            FROM LFA1 INTO @WL_LFA1_PROP
           WHERE LIFNR = @WA_ZLEST0002-PROPRIETARIO.

          IF ( WL_LFA1_PROP IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ADRC INTO @WA_ADRC
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.

            SELECT SINGLE *
              FROM ADR6 INTO @WA_ADR6
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.
          ENDIF.

          WA_ZSDT0243-PC_VEICULO = WA_ZSDT0001CG-DS_PLACA_REBOQ_2.
          WA_ZSDT0243-CPF     = WL_LFA1_PROP-STCD2.
          WA_ZSDT0243-CNPJ    = WL_LFA1_PROP-STCD1.
          WA_ZSDT0243-RNTRC   = WL_LFA1_PROP-BAHNS.
          WA_ZSDT0243-X_NOME  = WL_LFA1_PROP-NAME1.
          WA_ZSDT0243-IE      = WL_LFA1_PROP-STCD3.
          WA_ZSDT0243-UF_PROP = WA_ADRC-REGION.
          WA_ZSDT0243-TP_PROP = '0'.

          WA_ZSDT0243-C_INT    = WA_ZLEST0002-PROPRIETARIO.
          WA_ZSDT0243-RENAVAM  = WA_ZLEST0002-CD_RENAVAM.
          WA_ZSDT0243-TARA     = WA_ZLEST0002-TARA.
          WA_ZSDT0243-CAP_KG   = WA_ZLEST0002-CAP_KG.
          WA_ZSDT0243-CAP_M3   = WA_ZLEST0002-CAP_M3.
          WA_ZSDT0243-TP_ROD   = WA_ZLEST0002-TP_RODADO.
          WA_ZSDT0243-TP_CAR   = WA_ZLEST0002-TP_CARROCERIA2.
          WA_ZSDT0243-UF_VEIC  = WA_ZLEST0002-CD_UF.

          CASE WA_ZLEST0002-TP_VEICULO.
            WHEN '0'.
              WA_ZSDT0243-TP_ROD = '03'.
            WHEN '1'.
              IF WA_ZSDT0243-TP_ROD = '0' OR WA_ZSDT0243-TP_ROD = '00'.
                WA_ZSDT0243-TP_ROD = '03'.
              ENDIF.
          ENDCASE.

          APPEND WA_ZSDT0243 TO GT_ZSDT0243.

        ENDIF.

        IF WA_ZSDT0001CG-DS_PLACA_REBOQ_3 IS NOT INITIAL.

          SELECT SINGLE * INTO @WA_ZLEST0002
            FROM ZLEST0002
           WHERE PC_VEICULO EQ @WA_ZSDT0001CG-DS_PLACA_REBOQ_3.

          IF SY-SUBRC IS NOT INITIAL.
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                                  MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                                  ATTR1  = WA_ZSDT0001CG-DS_PLACA_REBOQ_3 )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                MSGV1  = CONV #( WA_ZSDT0001CG-DS_PLACA_REBOQ_3 )
                MSGTY  = 'E'.
          ENDIF.

          SELECT SINGLE *
            FROM LFA1 INTO @WL_LFA1_PROP
           WHERE LIFNR = @WA_ZLEST0002-PROPRIETARIO.

          IF ( WL_LFA1_PROP IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ADRC INTO @WA_ADRC
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.

            SELECT SINGLE *
              FROM ADR6 INTO @WA_ADR6
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.
          ENDIF.

          WA_ZSDT0243-PC_VEICULO = WA_ZSDT0001CG-DS_PLACA_REBOQ_3.
          WA_ZSDT0243-CPF     = WL_LFA1_PROP-STCD2.
          WA_ZSDT0243-CNPJ    = WL_LFA1_PROP-STCD1.
          WA_ZSDT0243-RNTRC   = WL_LFA1_PROP-BAHNS.
          WA_ZSDT0243-X_NOME  = WL_LFA1_PROP-NAME1.
          WA_ZSDT0243-IE      = WL_LFA1_PROP-STCD3.
          WA_ZSDT0243-UF_PROP = WA_ADRC-REGION.
          WA_ZSDT0243-TP_PROP = '0'.

          WA_ZSDT0243-C_INT    = WA_ZLEST0002-PROPRIETARIO.
          WA_ZSDT0243-RENAVAM  = WA_ZLEST0002-CD_RENAVAM.
          WA_ZSDT0243-TARA     = WA_ZLEST0002-TARA.
          WA_ZSDT0243-CAP_KG   = WA_ZLEST0002-CAP_KG.
          WA_ZSDT0243-CAP_M3   = WA_ZLEST0002-CAP_M3.
          WA_ZSDT0243-TP_ROD   = WA_ZLEST0002-TP_RODADO.
          WA_ZSDT0243-TP_CAR   = WA_ZLEST0002-TP_CARROCERIA2.
          WA_ZSDT0243-UF_VEIC  = WA_ZLEST0002-CD_UF.

          CASE WA_ZLEST0002-TP_VEICULO.
            WHEN '0'.
              WA_ZSDT0243-TP_ROD = '03'.
            WHEN '1'.
              IF WA_ZSDT0243-TP_ROD = '0' OR WA_ZSDT0243-TP_ROD = '00'.
                WA_ZSDT0243-TP_ROD = '03'.
              ENDIF.
          ENDCASE.

          APPEND WA_ZSDT0243 TO GT_ZSDT0243.

        ENDIF.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

      ELSE.

        LOOP AT IT_ZLEST0110 INTO DATA(WA_ZLEST0110).

          SELECT SINGLE * INTO @DATA(WA_LIKP)
            FROM LIKP
           WHERE VBELN EQ @WA_ZLEST0110-VBELN.

          SELECT SINGLE * INTO @DATA(WA_LIPS)
            FROM LIPS
           WHERE VBELN EQ @WA_ZLEST0110-VBELN.

          IF SY-SUBRC IS INITIAL.

            WA_ZSDT0237-MATNR = WA_LIPS-MATNR.

            CLEAR: WA_ZSDT0237-TPCARGA,
                   WA_ZSDT0237-XPROD.

            IF WA_ZSDT0237-MATNR IS NOT INITIAL.
              SELECT SINGLE MATKL INTO @WA_ZSDT0237-MATKL
                FROM MARA
               WHERE MATNR EQ @WA_ZSDT0237-MATNR.

              IF SY-SUBRC IS INITIAL.
                SELECT SINGLE * INTO @WA_ZLEST0193
                  FROM ZLEST0193
                 WHERE MATKL EQ @WA_ZSDT0237-MATKL.

                IF SY-SUBRC IS INITIAL.
                  WA_ZSDT0237-TPCARGA = ZCL_STRING=>LPAD( I_STR  = CONV #( WA_ZLEST0193-TIPO_CARGA ) I_QTD  = 2 I_CHAR = '0' ).
                ENDIF.

                SELECT SINGLE MAKTX INTO @WA_ZSDT0237-XPROD
                  FROM MAKT
                 WHERE MATNR EQ @WA_ZSDT0237-MATNR
                   AND SPRAS EQ @SY-LANGU.
              ENDIF.
            ENDIF.

            DATA: VALOR_CARGA TYPE ZXNFE_VCARGA.

            REPLACE ALL OCCURRENCES OF ',' IN WA_LIKP-BOLNR WITH '.'.

            VALOR_CARGA = WA_LIKP-BOLNR.

            "Soma dos Valores e Volumes do Agrupamento """""""""""""""""
            ADD WA_LIPS-NTGEW TO WA_ZSDT0237-QCARGA.
            ADD VALOR_CARGA   TO WA_ZSDT0237-VCARGA.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            "Documentos Referenciados """"""""""""""""""""""""""""""""""
            IF WA_ZLEST0110-CHAVE IS NOT INITIAL.
              CLEAR: WA_ZSDT0241.
              WA_ZSDT0241-CHAVE = WA_ZLEST0110-CHAVE.
              WA_ZSDT0241-SEQUENCIA = WA_ZSDT0239-SEQUENCIA.
              APPEND WA_ZSDT0241 TO GT_ZSDT0241.
            ENDIF.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          ENDIF.

        ENDLOOP.

        "Motorista """""""""""""""""""""""""""""""""""""""""""""""
        IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-MOTORISTA IS NOT INITIAL.
          CLEAR: WA_ZSDT0242.
          WA_ZSDT0242-LIFNR = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-MOTORISTA.
          APPEND WA_ZSDT0242 TO GT_ZSDT0242.
        ENDIF.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

        "Placa Veículos """"""""""""""""""""""""""""""""""""""""""
        IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAV IS NOT INITIAL.

          SELECT SINGLE * INTO @WA_ZLEST0002
            FROM ZLEST0002
           WHERE PC_VEICULO EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAV.

          IF SY-SUBRC IS NOT INITIAL.
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                                  MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                                  ATTR1  = WA_ZSDT0001CG-DS_PLACA_TRATOR )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                MSGV1  = CONV #( WA_ZSDT0001CG-DS_PLACA_TRATOR )
                MSGTY  = 'E'.
          ENDIF.

          SELECT SINGLE *
            FROM LFA1 INTO @WL_LFA1_PROP
           WHERE LIFNR = @WA_ZLEST0002-PROPRIETARIO.

          IF ( WL_LFA1_PROP IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ADRC INTO @WA_ADRC
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.

            SELECT SINGLE *
              FROM ADR6 INTO @WA_ADR6
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.
          ENDIF.

          WA_ZSDT0243-PC_VEICULO = WA_ZLEST0002-PC_VEICULO.
          WA_ZSDT0243-CPF        = WL_LFA1_PROP-STCD2.
          WA_ZSDT0243-CNPJ       = WL_LFA1_PROP-STCD1.
          WA_ZSDT0243-RNTRC      = WL_LFA1_PROP-BAHNS.
          WA_ZSDT0243-X_NOME     = WL_LFA1_PROP-NAME1.
          WA_ZSDT0243-IE         = WL_LFA1_PROP-STCD3.
          WA_ZSDT0243-UF_PROP    = WA_ADRC-REGION.
          WA_ZSDT0243-TP_PROP    = '0'.
          WA_ZSDT0243-C_INT      = WA_ZLEST0002-PROPRIETARIO.
          WA_ZSDT0243-RENAVAM    = WA_ZLEST0002-CD_RENAVAM.
          WA_ZSDT0243-TARA       = WA_ZLEST0002-TARA.
          WA_ZSDT0243-CAP_KG     = WA_ZLEST0002-CAP_KG.
          WA_ZSDT0243-CAP_M3     = WA_ZLEST0002-CAP_M3.
          WA_ZSDT0243-TP_ROD     = WA_ZLEST0002-TP_RODADO.
          WA_ZSDT0243-TP_CAR     = WA_ZLEST0002-TP_CARROCERIA2.
          WA_ZSDT0243-UF_VEIC    = WA_ZLEST0002-CD_UF.

          CASE WA_ZLEST0002-TP_VEICULO.
            WHEN '0'.
              WA_ZSDT0243-TP_ROD = '03'.
            WHEN '1'.
              IF WA_ZSDT0243-TP_ROD = '0' OR WA_ZSDT0243-TP_ROD = '00'.
                WA_ZSDT0243-TP_ROD = '03'.
              ENDIF.
          ENDCASE.

          APPEND WA_ZSDT0243 TO GT_ZSDT0243.

        ENDIF.

        IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR1 IS NOT INITIAL.

          SELECT SINGLE * INTO @WA_ZLEST0002
            FROM ZLEST0002
           WHERE PC_VEICULO EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR1.

          IF SY-SUBRC IS NOT INITIAL.
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                                  MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                                  ATTR1  = WA_ZSDT0001CG-DS_PLACA_REBOQ_1 )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                MSGV1  = CONV #( WA_ZSDT0001CG-DS_PLACA_REBOQ_1 )
                MSGTY  = 'E'.
          ENDIF.

          SELECT SINGLE *
            FROM LFA1 INTO @WL_LFA1_PROP
           WHERE LIFNR = @WA_ZLEST0002-PROPRIETARIO.

          IF ( WL_LFA1_PROP IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ADRC INTO @WA_ADRC
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.

            SELECT SINGLE *
              FROM ADR6 INTO @WA_ADR6
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.
          ENDIF.

          WA_ZSDT0243-PC_VEICULO = WA_ZLEST0002-PC_VEICULO.
          WA_ZSDT0243-CPF     = WL_LFA1_PROP-STCD2.
          WA_ZSDT0243-CNPJ    = WL_LFA1_PROP-STCD1.
          WA_ZSDT0243-RNTRC   = WL_LFA1_PROP-BAHNS.
          WA_ZSDT0243-X_NOME  = WL_LFA1_PROP-NAME1.
          WA_ZSDT0243-IE      = WL_LFA1_PROP-STCD3.
          WA_ZSDT0243-UF_PROP = WA_ADRC-REGION.
          WA_ZSDT0243-TP_PROP = '0'.

          WA_ZSDT0243-C_INT    = WA_ZLEST0002-PROPRIETARIO.
          WA_ZSDT0243-RENAVAM  = WA_ZLEST0002-CD_RENAVAM.
          WA_ZSDT0243-TARA     = WA_ZLEST0002-TARA.
          WA_ZSDT0243-CAP_KG   = WA_ZLEST0002-CAP_KG.
          WA_ZSDT0243-CAP_M3   = WA_ZLEST0002-CAP_M3.
          WA_ZSDT0243-TP_ROD   = WA_ZLEST0002-TP_RODADO.
          WA_ZSDT0243-TP_CAR   = WA_ZLEST0002-TP_CARROCERIA2.
          WA_ZSDT0243-UF_VEIC  = WA_ZLEST0002-CD_UF.

          CASE WA_ZLEST0002-TP_VEICULO.
            WHEN '0'.
              WA_ZSDT0243-TP_ROD = '03'.
            WHEN '1'.
              IF WA_ZSDT0243-TP_ROD = '0' OR WA_ZSDT0243-TP_ROD = '00'.
                WA_ZSDT0243-TP_ROD = '03'.
              ENDIF.
          ENDCASE.

          APPEND WA_ZSDT0243 TO GT_ZSDT0243.

        ENDIF.

        IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR2 IS NOT INITIAL.

          SELECT SINGLE * INTO @WA_ZLEST0002
            FROM ZLEST0002
           WHERE PC_VEICULO EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR2.

          IF SY-SUBRC IS NOT INITIAL.
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                                  MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                                  ATTR1  = WA_ZSDT0001CG-DS_PLACA_REBOQ_2 )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                MSGV1  = CONV #( WA_ZSDT0001CG-DS_PLACA_REBOQ_2 )
                MSGTY  = 'E'.
          ENDIF.

          SELECT SINGLE *
            FROM LFA1 INTO @WL_LFA1_PROP
           WHERE LIFNR = @WA_ZLEST0002-PROPRIETARIO.

          IF ( WL_LFA1_PROP IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ADRC INTO @WA_ADRC
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.

            SELECT SINGLE *
              FROM ADR6 INTO @WA_ADR6
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.
          ENDIF.

          WA_ZSDT0243-PC_VEICULO = WA_ZLEST0002-PC_VEICULO.
          WA_ZSDT0243-CPF     = WL_LFA1_PROP-STCD2.
          WA_ZSDT0243-CNPJ    = WL_LFA1_PROP-STCD1.
          WA_ZSDT0243-RNTRC   = WL_LFA1_PROP-BAHNS.
          WA_ZSDT0243-X_NOME  = WL_LFA1_PROP-NAME1.
          WA_ZSDT0243-IE      = WL_LFA1_PROP-STCD3.
          WA_ZSDT0243-UF_PROP = WA_ADRC-REGION.
          WA_ZSDT0243-TP_PROP = '0'.

          WA_ZSDT0243-C_INT    = WA_ZLEST0002-PROPRIETARIO.
          WA_ZSDT0243-RENAVAM  = WA_ZLEST0002-CD_RENAVAM.
          WA_ZSDT0243-TARA     = WA_ZLEST0002-TARA.
          WA_ZSDT0243-CAP_KG   = WA_ZLEST0002-CAP_KG.
          WA_ZSDT0243-CAP_M3   = WA_ZLEST0002-CAP_M3.
          WA_ZSDT0243-TP_ROD   = WA_ZLEST0002-TP_RODADO.
          WA_ZSDT0243-TP_CAR   = WA_ZLEST0002-TP_CARROCERIA2.
          WA_ZSDT0243-UF_VEIC  = WA_ZLEST0002-CD_UF.

          CASE WA_ZLEST0002-TP_VEICULO.
            WHEN '0'.
              WA_ZSDT0243-TP_ROD = '03'.
            WHEN '1'.
              IF WA_ZSDT0243-TP_ROD = '0' OR WA_ZSDT0243-TP_ROD = '00'.
                WA_ZSDT0243-TP_ROD = '03'.
              ENDIF.
          ENDCASE.

          APPEND WA_ZSDT0243 TO GT_ZSDT0243.

        ENDIF.

        IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR3 IS NOT INITIAL.

          SELECT SINGLE * INTO @WA_ZLEST0002
            FROM ZLEST0002
           WHERE PC_VEICULO EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR3.

          IF SY-SUBRC IS NOT INITIAL.
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                                  MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                                  ATTR1  = WA_ZSDT0001CG-DS_PLACA_REBOQ_3 )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_FRETE_PLACA-MSGNO
                MSGV1  = CONV #( WA_ZSDT0001CG-DS_PLACA_REBOQ_3 )
                MSGTY  = 'E'.
          ENDIF.

          SELECT SINGLE *
            FROM LFA1 INTO @WL_LFA1_PROP
           WHERE LIFNR = @WA_ZLEST0002-PROPRIETARIO.

          IF ( WL_LFA1_PROP IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ADRC INTO @WA_ADRC
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.

            SELECT SINGLE *
              FROM ADR6 INTO @WA_ADR6
             WHERE ADDRNUMBER EQ @WL_LFA1_PROP-ADRNR.
          ENDIF.

          WA_ZSDT0243-PC_VEICULO = WA_ZLEST0002-PC_VEICULO.
          WA_ZSDT0243-CPF     = WL_LFA1_PROP-STCD2.
          WA_ZSDT0243-CNPJ    = WL_LFA1_PROP-STCD1.
          WA_ZSDT0243-RNTRC   = WL_LFA1_PROP-BAHNS.
          WA_ZSDT0243-X_NOME  = WL_LFA1_PROP-NAME1.
          WA_ZSDT0243-IE      = WL_LFA1_PROP-STCD3.
          WA_ZSDT0243-UF_PROP = WA_ADRC-REGION.
          WA_ZSDT0243-TP_PROP = '0'.

          WA_ZSDT0243-C_INT    = WA_ZLEST0002-PROPRIETARIO.
          WA_ZSDT0243-RENAVAM  = WA_ZLEST0002-CD_RENAVAM.
          WA_ZSDT0243-TARA     = WA_ZLEST0002-TARA.
          WA_ZSDT0243-CAP_KG   = WA_ZLEST0002-CAP_KG.
          WA_ZSDT0243-CAP_M3   = WA_ZLEST0002-CAP_M3.
          WA_ZSDT0243-TP_ROD   = WA_ZLEST0002-TP_RODADO.
          WA_ZSDT0243-TP_CAR   = WA_ZLEST0002-TP_CARROCERIA2.
          WA_ZSDT0243-UF_VEIC  = WA_ZLEST0002-CD_UF.

          CASE WA_ZLEST0002-TP_VEICULO.
            WHEN '0'.
              WA_ZSDT0243-TP_ROD = '03'.
            WHEN '1'.
              IF WA_ZSDT0243-TP_ROD = '0' OR WA_ZSDT0243-TP_ROD = '00'.
                WA_ZSDT0243-TP_ROD = '03'.
              ENDIF.
          ENDCASE.

          APPEND WA_ZSDT0243 TO GT_ZSDT0243.

        ENDIF.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

      ENDIF.

      DATA: MDFE TYPE REF TO ZCL_MDFE.

      CREATE OBJECT MDFE.
      MDFE->SET_DATA_EMI( SY-DATLO ).
      MDFE->SET_HORA_EMI( SY-TIMLO ).

      IF ( SY-SUBRC = 0 ) AND ( WA_ZSDT0237-DOCNUM IS NOT INITIAL ).
        DATA: V_TIME_BR TYPE ERZET.
        CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
          EXPORTING
            I_BUKRS  = WA_ZSDT0237-BUKRS
            I_BRANCH = WA_ZSDT0237-BRANCH
          IMPORTING
            E_TIME   = V_TIME_BR.
        IF V_TIME_BR IS NOT INITIAL.
          MDFE->SET_HORA_EMI( V_TIME_BR ).
        ENDIF.
      ENDIF.

      "Adiciona UFs de Percurso.
      LOOP AT GT_ZSDT0240 INTO WA_ZSDT0240 WHERE NOT ( BLAND IS INITIAL ).
        MDFE->ADD_UF_PERC( CONV #( WA_ZSDT0240-BLAND ) ).
      ENDLOOP.

      IF GT_ZSDT0243[] IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(IT_ZLEST0003)
          FROM ZLEST0002
           FOR ALL ENTRIES IN @GT_ZSDT0243
         WHERE PC_VEICULO EQ @GT_ZSDT0243-PC_VEICULO.
      ENDIF.

      READ TABLE IT_ZLEST0003 INTO DATA(PC_TRACAO) WITH KEY TP_VEICULO = '0'.
      IF SY-SUBRC IS INITIAL.
        MDFE->SET_PLACA_CAV( I_PLACA_CAV = PC_TRACAO-PC_VEICULO ).
      ENDIF.

      MDFE->SET_PLACA_CAR1( I_PLACA_CAR1 = '' ).
      MDFE->SET_PLACA_CAR2( I_PLACA_CAR2 = '' ).
      MDFE->SET_PLACA_CAR3( I_PLACA_CAR3 = '' ).
      "MDFE->SET_PLACA_CAR4( I_PLACA_CAR1 = '' ).

      LOOP AT GT_ZSDT0243 INTO WA_ZSDT0243 WHERE PC_VEICULO NE PC_TRACAO-PC_VEICULO.
        CASE SY-TABIX.
          WHEN 1.
            MDFE->SET_PLACA_CAR1( I_PLACA_CAR1 = WA_ZSDT0243-PC_VEICULO ).
          WHEN 2.
            MDFE->SET_PLACA_CAR2( I_PLACA_CAR2 = WA_ZSDT0243-PC_VEICULO ).
          WHEN 3.
            MDFE->SET_PLACA_CAR3( I_PLACA_CAR3 = WA_ZSDT0243-PC_VEICULO ).
          WHEN 4.
            "MDFE->SET_PLACA_CAR4( I_PLACA_CAR3 = WA_ZSDT0243-PC_VEICULO ).
        ENDCASE.
      ENDLOOP.

      READ TABLE GT_ZSDT0242 INDEX 1 INTO WA_ZSDT0242.
      IF SY-SUBRC IS INITIAL.
        MDFE->SET_MOTORISTA( I_MOTORISTA  = WA_ZSDT0242-LIFNR ).
      ENDIF.

      "Adiciona Documentos ao MDF-e
      LOOP AT GT_ZSDT0241 INTO WA_ZSDT0241 WHERE DOCNUM_REF IS NOT INITIAL.
        MDFE->ADD_DOCUMENTO( WA_ZSDT0241-DOCNUM_REF ).
      ENDLOOP.

      "01	Rodoviário
      "02	Aéreo
      "03	Hidroviário
      "04	Ferroviário

      MDFE->AT_MODAL   = WA_ZSDT0237-MODAL.
      MDFE->AT_UFINI   = WA_ZSDT0237-CMUNINI(2).
      MDFE->AT_CMUNINI = WA_ZSDT0237-CMUNINI+3(7).

      SELECT SINGLE TEXT INTO @MDFE->AT_NMUNINI
        FROM J_1BTXJURT
       WHERE SPRAS      EQ @SY-LANGU
         AND COUNTRY    EQ @WA_ZSDT0237-COUNTRY
         AND TAXJURCODE EQ @WA_ZSDT0237-CMUNINI.

      MDFE->AT_UFFIM   = WA_ZSDT0237-CMUNFIM(2).
      MDFE->AT_CMUNFIM = WA_ZSDT0237-CMUNFIM+3(7).

      SELECT SINGLE TEXT INTO @MDFE->AT_NMUNFIM
        FROM J_1BTXJURT
       WHERE SPRAS      EQ @SY-LANGU
         AND COUNTRY    EQ @WA_ZSDT0237-COUNTRY
         AND TAXJURCODE EQ @WA_ZSDT0237-CMUNFIM.

      MDFE->AT_QCARGA    = WA_ZSDT0237-QCARGA.
      MDFE->AT_VCARGA    = WA_ZSDT0237-VCARGA.
      MDFE->AT_CUNID     = WA_ZSDT0237-CUNID.
      CASE WA_ZSDT0237-CUNID.
        WHEN '01'.
          MDFE->AT_CUNID_SAP = 'KG'.
        WHEN '02'.
          MDFE->AT_CUNID_SAP = 'TO'.
      ENDCASE.

      DATA: LC_ZSDT0237 TYPE ZSDT0237.
      MOVE-CORRESPONDING WA_ZSDT0237 TO LC_ZSDT0237.

      CLEAR: MDFE->AT_IT_UF_PERC[].

      LOOP AT GT_ZSDT0240 INTO WA_ZSDT0240.
        APPEND VALUE #( UF = WA_ZSDT0240-BLAND ) TO MDFE->AT_IT_UF_PERC.
      ENDLOOP.

      WA_ZSDT0237-DOCNUM = MDFE->GRAVAR_MDFE( I_ZSDT0237 = LC_ZSDT0237 ).

      CHECK WA_ZSDT0237-DOCNUM IS NOT INITIAL.

      LOOP AT GT_ZSDT0238 ASSIGNING FIELD-SYMBOL(<FS238>).
        <FS238>-DOCNUM = WA_ZSDT0237-DOCNUM.
      ENDLOOP.

      LOOP AT GT_ZSDT0239 ASSIGNING FIELD-SYMBOL(<FS239>).
        <FS239>-DOCNUM = WA_ZSDT0237-DOCNUM.
      ENDLOOP.

      LOOP AT GT_ZSDT0240 ASSIGNING FIELD-SYMBOL(<FS240>).
        <FS240>-DOCNUM = WA_ZSDT0237-DOCNUM.
      ENDLOOP.

      LOOP AT GT_ZSDT0242 ASSIGNING FIELD-SYMBOL(<FS242>).
        <FS242>-DOCNUM = WA_ZSDT0237-DOCNUM.
      ENDLOOP.

      LOOP AT GT_ZSDT0241 ASSIGNING FIELD-SYMBOL(<FS241>).
        <FS241>-DOCNUM = WA_ZSDT0237-DOCNUM.
      ENDLOOP.

      LOOP AT GT_ZSDT0243 ASSIGNING FIELD-SYMBOL(<FS243>).
        <FS243>-DOCNUM = WA_ZSDT0237-DOCNUM.
      ENDLOOP.

      MODIFY ZSDT0237 FROM WA_ZSDT0237.
      MODIFY ZSDT0238 FROM TABLE GT_ZSDT0238.
      MODIFY ZSDT0240 FROM TABLE GT_ZSDT0240.
      MODIFY ZSDT0239 FROM TABLE GT_ZSDT0239.
      MODIFY ZSDT0241 FROM TABLE GT_ZSDT0241.
      MODIFY ZSDT0243 FROM TABLE GT_ZSDT0243.
      MODIFY ZSDT0242 FROM TABLE GT_ZSDT0242.
      COMMIT WORK AND WAIT.

      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_MDFE = WA_ZSDT0237-DOCNUM.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '09'.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ST_PROC = '99'.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO( ).


      TRY .

          SELECT SINGLE * into @data(WA_ZSDT0102)
            FROM ZSDT0102
           WHERE DOCNUM EQ @WA_ZSDT0237-DOCNUM.

          DATA(VL_DOCNUM_MDFE) = WA_ZSDT0237-DOCNUM.

          IF MDFE->GET_CK_AUTORIZADO( ) EQ ABAP_FALSE.

            "Encerramento """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            "Encerramento """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            "A exceção ZCX_AVERBACAO_SEGURO não será interceptada nem é declarada na cáusula RAISING de "SET_AUTORIZA_MDFE".
            TRY .
                MESSAGE S048(ZMDFE) WITH WA_ZSDT0237-DOCNUM.
                DATA(VL_DOCNUM_SOL_ENC) = MDFE->ENVIAR_MDFE( I_SEM_CONFIRMACAO = ABAP_TRUE
                      I_AGUARDAR = ABAP_TRUE
                      I_CICLOS = 120
                      I_SEGUNDOS = 3
                      I_RAISE = ABAP_TRUE
                 ).
              CATCH ZCX_AVERBACAO_SEGURO.
            ENDTRY.

            IF VL_DOCNUM_SOL_ENC IS NOT INITIAL.
              MESSAGE S049(ZMDFE) WITH VL_DOCNUM_SOL_ENC.
              "Aguardar Encerramento
              CLEAR: WA_ZSDT0102.
              WAIT UP TO 5 SECONDS.
              DATA(LC_CICLOS) = 120.
              WHILE LC_CICLOS IS NOT INITIAL AND WA_ZSDT0102-ENCERRADO EQ ABAP_FALSE AND WA_ZSDT0102-STATUS NE '3'.
                "Tempo de Cadas Ciclo
                WAIT UP TO 3 SECONDS.
                "Busca Vinculo do CT-e com a
                SELECT SINGLE * INTO WA_ZSDT0102
                  FROM ZSDT0102
                 WHERE DOCNUM EQ VL_DOCNUM_SOL_ENC.
                SUBTRACT 1 FROM LC_CICLOS.
              ENDWHILE.

              VL_DOCNUM_SOL_ENC = MDFE->ENVIAR_MDFE( I_SEM_CONFIRMACAO = ABAP_TRUE ).
              "Existe MDF-e a Ser Encerrada """""""""""""""""""""""""""""""
              IF VL_DOCNUM_SOL_ENC IS NOT INITIAL.
                ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
              ENDIF.
            ENDIF.
            """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            CLEAR: MDFE.

            "Busca Vinculo do CT-e com a
            SELECT SINGLE * INTO WA_ZSDT0102
              FROM ZSDT0102
             WHERE DOCNUM EQ VL_DOCNUM_MDFE.

            WAIT UP TO 5 SECONDS.

            LC_CICLOS = 120.

            WHILE LC_CICLOS IS NOT INITIAL AND WA_ZSDT0102-AUTORIZADO EQ ABAP_FALSE AND WA_ZSDT0102-STATUS NE '3'.
              "Tempo de Cadas Ciclo
              WAIT UP TO 3 SECONDS.
              "Busca Vinculo do CT-e com a
              SELECT SINGLE *
                INTO WA_ZSDT0102
                FROM ZSDT0102
               WHERE DOCNUM EQ VL_DOCNUM_MDFE.
              SUBTRACT 1 FROM LC_CICLOS.
            ENDWHILE.
          ELSE.
            SELECT SINGLE * INTO WA_ZSDT0102
              FROM ZSDT0102
             WHERE NMDFE  EQ WA_ZSDT0102-NMDFE
               AND DOCNUM EQ WA_ZSDT0102-DOCNUM.
          ENDIF.

          IF WA_ZSDT0102-AUTORIZADO EQ ABAP_TRUE.
            "Autorizado
          ELSE.
            IF WA_ZSDT0102-STATUS EQ '3'.
              "pegar texto para exceção
              ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL_STRING( I_TEXTO = CONV #( WA_ZSDT0102-MSG ) ).
            ELSE.
              "Ainda não foi autorizado

            ENDIF.
          ENDIF.

        CATCH ZCX_MDFE INTO DATA(EX_MDFE).
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL_STRING( I_TEXTO = EX_MDFE->GET_LONGTEXT( ) ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_GERAR_PEDAGIO.

    DATA: E_ID_PROC_CLIENTE TYPE ZDE_ID_PROC_CLIENTE,
          E_GRAVOU          TYPE CHAR01.

    DATA: OBJ_REPOM    TYPE REF TO ZCL_REPOM_VIAGEM_VPR,
          E_ERROS	     TYPE ZDE_REPOM_ERROS_T,
          WA_ZLEST0123 TYPE ZLEST0123.

    TRY .
        E_ID_PROC_CLIENTE =
            ZCL_REPOM_VIAGEM_VPR=>CRIAR_VIAGEM(
              EXPORTING
                I_TKNUM             = I_TKNUM
                I_CD_CID_ORIGEM     = I_CD_CID_ORIGEM
                I_CD_CID_DESTINO    = I_CD_CID_DESTINO
                I_NR_CARTAO         = I_NR_CARTAO
                I_ID_ROTA_REPOM     = I_ID_ROTA_REPOM
                I_ID_PERCURSO_REPOM = I_ID_PERCURSO_REPOM
              IMPORTING
                E_GRAVOU = E_GRAVOU ).

      CATCH CX_ROOT INTO DATA(EX_ROOT).
        RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGID
                              MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGNO
                              ATTR1 = CONV #( I_CD_CID_ORIGEM )
                              ATTR2 = CONV #( I_CD_CID_DESTINO ) )
            MSGTY  = 'E'
            MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGID
            MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGNO
            MSGV1  = CONV #( I_CD_CID_ORIGEM )
            MSGV2  = CONV #( I_CD_CID_DESTINO ).
    ENDTRY.

    "Autorizar
    CREATE OBJECT OBJ_REPOM
      EXPORTING
        I_ID_PROC_CLIENTE = E_ID_PROC_CLIENTE.

    CALL METHOD OBJ_REPOM->SOLICITAR
      IMPORTING
        E_ERROS                    = E_ERROS
      EXCEPTIONS
        SERVICO_NAO_ENCONTRADO     = 1
        HTTP_COMMUNICATION_FAILURE = 2
        HTTP_INVALID_STATE         = 3
        HTTP_PROCESSING_FAILED     = 4
        HTTP_INVALID_TIMEOUT       = 5
        ERRO                       = 6
        OTHERS                     = 7.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
    ENDIF.

    IF E_ERROS IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGID
                            MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGNO
                            ATTR1 = CONV #( I_CD_CID_ORIGEM )
                            ATTR2 = CONV #( I_CD_CID_DESTINO ) )
          MSGTY  = 'E'
          MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGID
          MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ERRO_PEDAGIO-MSGNO
          MSGV1  = CONV #( I_CD_CID_ORIGEM )
          MSGV2  = CONV #( I_CD_CID_DESTINO ).
    ENDIF.

    DATA(E_AUTORIZADO) = ZCL_REPOM_VIAGEM_VPR=>GET_AUTORIZADO( EXPORTING I_ID_PROC_CLIENTE = E_ID_PROC_CLIENTE ).

    OBJ_REPOM->GET_REGISTRO( IMPORTING E_REGISTRO = E_ZLEST0123 ).

    CLEAR: OBJ_REPOM.

  ENDMETHOD.


  METHOD zif_doc_fiscal_ft_entrada~set_gravar_registro.

    r_instancia = me.

    me->zif_doc_fiscal_ft_entrada~set_validar_aviso(  ).

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-id_agrupa_frete IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(it_zlest0108)
        FROM zlest0108
       WHERE id_agrupa_frete EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-id_agrupa_frete
         AND vbeln NE @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln.
    ENDIF.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp
        IMPORTING
          output = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.
    ENDIF.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fknum IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fknum
        IMPORTING
          output = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fknum.
    ENDIF.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete
        IMPORTING
          output = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete.
    ENDIF.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete
        IMPORTING
          output = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete.
    ENDIF.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete
        IMPORTING
          output = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete.
    ENDIF.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_mdfe IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_mdfe
        IMPORTING
          output = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_mdfe.
    ENDIF.

    MODIFY zlest0108 FROM me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento.
    MODIFY zlest0110 FROM me->zif_doc_fiscal_ft_entrada~at_nota_fiscal.
    MODIFY zlest0109 FROM me->zif_doc_fiscal_ft_entrada~at_aviso_receb_items.

    LOOP AT it_zlest0108 ASSIGNING FIELD-SYMBOL(<fs0108>).
      <fs0108>-doc_transp   = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-doc_transp.
      <fs0108>-fknum        = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fknum.
      <fs0108>-ov_frete     = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ov_frete.
      <fs0108>-fatura_frete = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-fatura_frete.
      <fs0108>-nro_nf_frete = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete.
      <fs0108>-nro_nf_mdfe  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_mdfe.
      <fs0108>-kbetr        = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-kbetr.
      <fs0108>-st_proc      = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-st_proc.
    ENDLOOP.

    IF it_zlest0108[] IS NOT INITIAL.
      MODIFY zlest0108 FROM TABLE it_zlest0108.
    ENDIF.

    LOOP AT it_zlest0108 INTO DATA(wa_zlest0108) WHERE id_carga IS NOT INITIAL AND id_nota IS NOT INITIAL.
      UPDATE zsdt0001nt
         SET fte_tknum       = wa_zlest0108-doc_transp
             fte_fknum       = wa_zlest0108-fknum
             fte_vbeln_va    = wa_zlest0108-ov_frete
             fte_vbeln_vf    = wa_zlest0108-fatura_frete
             fte_docnum      = wa_zlest0108-nro_nf_frete
             fte_docnum_mdfe = wa_zlest0108-nro_nf_mdfe
       WHERE id_carga EQ wa_zlest0108-id_carga
         AND id_nota  EQ wa_zlest0108-id_nota.
    ENDLOOP.

    COMMIT WORK AND WAIT.

    me->zif_doc_fiscal_ft_entrada~set_registro(
      EXPORTING
        i_ebeln     = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebeln
        i_ebelp     = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebelp
        i_werks     = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-werks
        i_lgort     = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-lgort
        i_charg     = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-charg
        i_vbeln     = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln
      ).

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_NEW_DOCUMENTO_AGRUPADO.

    DATA: LC_ID_AGRUPA TYPE ZDE_ID_AGRUPA_FRETE.

    R_INSTANCIA = ME.

    CHECK I_AVISOS[] IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(IT_NOTAS_CARGA)
      FROM ZSDT0001NT
       FOR ALL ENTRIES IN @I_AVISOS
     WHERE AV_VBELN EQ @I_AVISOS-VBELN.

    CHECK SY-SUBRC IS INITIAL.

    SELECT * INTO TABLE @DATA(IT_CARGA)
      FROM ZSDT0001CG
      FOR ALL ENTRIES IN @IT_NOTAS_CARGA
     WHERE ID_CARGA EQ @IT_NOTAS_CARGA-ID_CARGA.

    SELECT * INTO TABLE @DATA(IT_LIPS)
      FROM LIPS
       FOR ALL ENTRIES IN @I_AVISOS
     WHERE VBELN EQ @I_AVISOS-VBELN.

    SELECT * INTO TABLE @DATA(IT_ZLEST0108)
      FROM ZLEST0108
       FOR ALL ENTRIES IN @I_AVISOS
     WHERE VBELN EQ @I_AVISOS-VBELN.

    SELECT * INTO TABLE @DATA(IT_VBPA)
      FROM VBPA
       FOR ALL ENTRIES IN @I_AVISOS
     WHERE VBELN EQ @I_AVISOS-VBELN.

    SORT IT_ZLEST0108 BY VBELN.
    SORT IT_LIPS BY VBELN.
    SORT IT_VBPA BY VBELN PARVW.
    SORT IT_CARGA BY ID_CARGA.

    LOOP AT IT_NOTAS_CARGA INTO DATA(WA_NOTAS_CARGA).

      READ TABLE IT_LIPS WITH KEY VBELN = WA_NOTAS_CARGA-AV_VBELN INTO DATA(WA_LIPS).
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE IT_ZLEST0108 WITH KEY VBELN = WA_NOTAS_CARGA-AV_VBELN TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      IF I_ID_AGRUPA_FRETE IS NOT INITIAL.
        LC_ID_AGRUPA = I_ID_AGRUPA_FRETE.
      ELSEIF LC_ID_AGRUPA IS INITIAL.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_NEW_ID_DOC_AGRUPADO( IMPORTING E_ID_AGRUPA = LC_ID_AGRUPA ).
      ENDIF.

      " Criar Linha de Frete """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      CLEAR: ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO.
      "Ordem de Venda
      "Pedido de Compra
      "Tipo de Entrada
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_AGRUPA_FRETE   = LC_ID_AGRUPA.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_CARGA          = WA_NOTAS_CARGA-ID_CARGA.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_NOTA           = WA_NOTAS_CARGA-ID_NOTA.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VLR_TOTAL         = WA_NOTAS_CARGA-NR_VALOR.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-CK_NFE            = COND STRING( WHEN WA_NOTAS_CARGA-ID_MOD_FISCAL EQ ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE THEN ABAP_TRUE ELSE ABAP_FALSE ).
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NUMERO            = WA_NOTAS_CARGA-NR_NOTA.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-SERIE             = WA_NOTAS_CARGA-NM_SERIE.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELN             = WA_NOTAS_CARGA-PO_NUMBER.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELP             = WA_NOTAS_CARGA-PO_ITEM.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-WERKS             = WA_LIPS-WERKS.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-LGORT             = WA_LIPS-LGORT.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-CHARG             = WA_LIPS-CHARG.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AUTOMATICO        = ABAP_TRUE.
      "ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_ROTA_REPOM     = I_ID_ROTA_REPOM.
      "ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_PERCURSO_REPOM = I_ID_PERCURSO_REPOM.
      "ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NR_CARTAO_REPOM   = I_NR_CARTAO_REPOM.


      "Fornecedor SAP
      READ TABLE IT_VBPA WITH KEY VBELN = WA_NOTAS_CARGA-AV_VBELN PARVW = 'LF' INTO DATA(WA_VBPA) BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_REMETENTE   = WA_VBPA-LIFNR.
      ENDIF.

      "Fornecedor SAP
      READ TABLE IT_VBPA WITH KEY VBELN = WA_NOTAS_CARGA-AV_VBELN PARVW = 'PC' INTO WA_VBPA BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA  = WA_VBPA-LIFNR.
      ENDIF.

      "Cliente SAP
      READ TABLE IT_VBPA WITH KEY VBELN = WA_NOTAS_CARGA-AV_VBELN PARVW = 'LR' INTO WA_VBPA BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA = WA_VBPA-KUNNR.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_DEST_MERC   = WA_VBPA-KUNNR.
      ENDIF.

      READ TABLE IT_CARGA INTO DATA(WA_CARGA) WITH KEY ID_CARGA = WA_NOTAS_CARGA-ID_CARGA BINARY SEARCH.

      "Fornecedor SAP
      READ TABLE IT_VBPA WITH KEY VBELN = WA_NOTAS_CARGA-AV_VBELN PARVW = 'SP' INTO WA_VBPA BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE = WA_VBPA-LIFNR.
      ELSE.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE = WA_CARGA-ID_AGENT_FRETE.
      ENDIF.

      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PESO_LIQ        = WA_LIPS-NTGEW.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-SAFRA_ORDEM_CAR = WA_CARGA-NR_SAFRA.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_ORDEM        = WA_CARGA-ID_ORDEM.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ZDT_MOV         = SY-DATUM.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAV       = WA_CARGA-DS_PLACA_TRATOR.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR1      = WA_CARGA-DS_PLACA_REBOQ_1.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR2      = WA_CARGA-DS_PLACA_REBOQ_2.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR3      = WA_CARGA-DS_PLACA_REBOQ_3.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-MOTORISTA       = WA_CARGA-ID_MOTORISTA.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN           = WA_LIPS-VBELN.

      IF WA_CARGA-ID_ORDEM IS NOT INITIAL.

        SELECT SINGLE * INTO @DATA(WA_ZSDT0001OD)
         FROM ZSDT0001OD
        WHERE ID_ORDEM EQ @WA_CARGA-ID_ORDEM.

        IF SY-SUBRC IS INITIAL.
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_ORDEM_CAR = WA_ZSDT0001OD-NR_ORDEM.
        ENDIF.

      ENDIF.

      "Notas Fiscal
      CLEAR: ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-EBELN          = WA_NOTAS_CARGA-PO_NUMBER.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-EBELP          = WA_NOTAS_CARGA-PO_ITEM.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-WERKS          = WA_LIPS-WERKS.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-LGORT          = WA_LIPS-LGORT.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHARG          = WA_LIPS-CHARG.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-NFE            = COND STRING( WHEN WA_NOTAS_CARGA-ID_MOD_FISCAL EQ ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE THEN ABAP_TRUE ELSE ABAP_FALSE ).
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MODELO         = WA_NOTAS_CARGA-ID_MOD_FISCAL.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-SERIE          = WA_NOTAS_CARGA-NM_SERIE.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-NUMERO         = WA_NOTAS_CARGA-NR_NOTA.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CLIENTE        = WA_NOTAS_CARGA-ID_FORNECEDOR.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-DTEMISSAO      = WA_NOTAS_CARGA-DT_EMISSAO.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_BC          = WA_NOTAS_CARGA-NR_VALOR.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_ICMS        = 0.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_BC_ST       = 0.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_ST          = 0.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_PRODUTOS    = WA_NOTAS_CARGA-NR_VALOR.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_NOTA_FISCAL = WA_NOTAS_CARGA-NR_VALOR.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL       = WA_LIPS-MATNR.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-QUANTIDADE     = WA_LIPS-NTGEW.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-UNIDADE        = WA_LIPS-GEWEI.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CFOP           = WA_NOTAS_CARGA-CFOP.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-LC_RETIRADA    = WA_NOTAS_CARGA-ID_ENTREGUE_POR.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-PIN_SUFRAMA    = SPACE.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHAVE          = WA_NOTAS_CARGA-NR_CHAVE_NFE.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-ID_CARGA       = WA_NOTAS_CARGA-ID_CARGA.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-ID_NOTA        = WA_NOTAS_CARGA-ID_NOTA.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VBELN          = WA_LIPS-VBELN.

      DATA(FORNECEDOR) = CAST ZCL_FORNECEDORES( ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE( )->SET_PARCEIRO( I_PARCEIRO = WA_NOTAS_CARGA-ID_FORNECEDOR ) )->AT_LFA1.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-NAME1          = FORNECEDOR-NAME1.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-STCD1          = FORNECEDOR-STCD1.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-STCD2          = FORNECEDOR-STCD2.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-PFISICA        = FORNECEDOR-STKZN.

      IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHAVE IS NOT INITIAL.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-DOCNUM9 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHAVE+34(9).
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CDV     = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHAVE+43(1).
      ENDIF.

      IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MODELO EQ ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE.

        ZCL_NFE_XML=>ZIF_NFE_XML~GET_INSTANCE(
          )->SET_REGISTRO( I_CHAVE = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHAVE
          )->GET_VALIDAR(
           EXPORTING
             I_MATERIAL       =  WA_LIPS-MATNR
          IMPORTING
            E_VALIDACAO      = DATA(E_VALIDACAO)
          ).

        IF E_VALIDACAO-CK_ERRO EQ ABAP_TRUE.
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL_STRING( I_TEXTO = E_VALIDACAO-DS_MESSAGEM ).
        ELSEIF E_VALIDACAO-NM_QTD_ITENS GT 1.
          MESSAGE ID ZCX_CARGA=>ZCX_NFE_MANY_ROWS-MSGID TYPE 'S' NUMBER ZCX_CARGA=>ZCX_NFE_MANY_ROWS-MSGNO INTO DATA(MTEXT) .
          ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL_STRING( I_TEXTO = MTEXT ).
        ENDIF.

      ENDIF.

      "Item do Aviso de Recebimento
      CLEAR: ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-EBELN      = WA_NOTAS_CARGA-PO_NUMBER.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-EBELP      = WA_NOTAS_CARGA-PO_ITEM.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-WERKS      = WA_LIPS-WERKS.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-LGORT      = WA_LIPS-LGORT.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-CHARG      = WA_LIPS-CHARG.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-LIFNR      = WA_NOTAS_CARGA-ID_FORNECEDOR.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-MATNR      = WA_LIPS-MATNR.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-NFNUM      = WA_NOTAS_CARGA-NR_NOTA.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-SERIE      = WA_NOTAS_CARGA-NM_SERIE.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-QTDE_AVISO = WA_LIPS-NTGEW.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-UNIDADE    = WA_LIPS-GEWEI.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-VBELN      = WA_LIPS-VBELN.

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      CLEAR: WA_CARGA.

      ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_GRAVAR_REGISTRO(
        )->SET_CLEAR(
        ).

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_NEW_DOCUMENTO_IF_CARGA.

    DATA: I_COD_LOC_ENTREGA TYPE KUNNR.
    DATA: I_AVISOS TYPE T_VBELN.

    R_INSTANCIA = ME.

    TRY .
        I_IF_CARGA->GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = DATA(E_APRESENTACAO) ).
      CATCH ZCX_ORDEM_CARREGAMENTO INTO DATA(EX_ORDEM).
        EX_ORDEM->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
        RETURN.
      CATCH ZCX_CARGA INTO DATA(EX_CARGA).
        EX_CARGA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
        RETURN.
    ENDTRY.

    LOOP AT E_APRESENTACAO-NOTAS INTO DATA(WA_DOCUMENTO_FISCAL).

      I_COD_LOC_ENTREGA = I_IF_CARGA->CARGA-ID_BRANCH.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = I_COD_LOC_ENTREGA
        IMPORTING
          OUTPUT = I_COD_LOC_ENTREGA.

      TRY.
          ZCL_DOC_FISCAL_FT_ENTRADA=>ZIF_DOC_FISCAL_FT_ENTRADA~SET_CRIAR_AVISO_NOTA_CARGA(
            EXPORTING
              I_AGENTE_FRETE            = I_IF_CARGA->CARGA-ID_AGENT_FRETE
              I_COD_LOC_ENTREGA         = I_COD_LOC_ENTREGA
              I_NOTA                    = WA_DOCUMENTO_FISCAL
            IMPORTING
              E_VBELN                   = E_VBELN ).

          IF E_VBELN IS NOT INITIAL.
            APPEND VALUE #( VBELN = E_VBELN ) TO I_AVISOS.
          ENDIF.

        CATCH ZCX_DOC_FISCAL_FT_ENTRADA INTO DATA(EX_ENTRADA).
          EX_ENTRADA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
          RETURN.
      ENDTRY.

    ENDLOOP.

    CHECK I_AVISOS[] IS NOT INITIAL.

    TRY.
        ZCL_DOC_FISCAL_FT_ENTRADA=>ZIF_DOC_FISCAL_FT_ENTRADA~GET_INSTANCE(
           )->SET_NEW_DOCUMENTO_AGRUPADO( EXPORTING I_AVISOS = I_AVISOS
           ).
      CATCH ZCX_DOC_FISCAL_FT_ENTRADA INTO EX_ENTRADA.
        EX_ENTRADA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
        RETURN.
    ENDTRY.

    TRY.
        READ TABLE I_AVISOS INTO DATA(WA_AVISO) INDEX 1.
        ZCL_DOC_FISCAL_FT_ENTRADA=>ZIF_DOC_FISCAL_FT_ENTRADA~GET_INSTANCE(
          )->SET_REGISTRO( EXPORTING I_VBELN = WA_AVISO-VBELN
          )->SET_GERAR_DOCUMENTO_TRANSPORTE(
          ).
      CATCH ZCX_DOC_FISCAL_FT_ENTRADA INTO EX_ENTRADA.
        EX_ENTRADA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
        RETURN.
    ENDTRY.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0108)
       FROM ZLEST0108
     WHERE VBELN EQ @WA_AVISO-VBELN.

    CHECK SY-SUBRC IS INITIAL.

    E_EBELN        = WA_ZLEST0108-EBELN.
    E_EBELP        = WA_ZLEST0108-EBELP.
    E_VBELN        = WA_ZLEST0108-VBELN.
    E_DOC_TRANSP   = WA_ZLEST0108-DOC_TRANSP.
    E_FKNUM        = WA_ZLEST0108-FKNUM.
    E_OV_FRETE     = WA_ZLEST0108-OV_FRETE.
    E_FATURA_FRETE = WA_ZLEST0108-FATURA_FRETE.
    E_NRO_NF_FRETE = WA_ZLEST0108-NRO_NF_FRETE.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_NEW_DOCUMENTO_ORDEM_CARREG.

    DATA: LC_NOTA_FISCAL  TYPE ZDE_INFO_NOTA.

    MOVE-CORRESPONDING I_NOTA_FISCAL TO LC_NOTA_FISCAL.

    DATA: WA_INFO_C TYPE KNA1,
          WA_INFO_K TYPE LFA1.

    R_INSTANCIA = ME.

    CLEAR: E_EBELN,
           E_EBELP,
           E_VBELN,
           E_DOC_TRANSP,
           E_FKNUM,
           E_OV_FRETE,
           E_FATURA_FRETE,
           E_NRO_NF_FRETE.

    ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_CLEAR( ).

    ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ORDEM_CARREGAMENTO(
      EXPORTING
        I_ID_ORDEM  = I_ID_ORDEM
      IMPORTING
       E_ORDEM_CARREGAMENTO = DATA(E_ORDEM_CARREGAMENTO)
      ).

    MOVE-CORRESPONDING E_ORDEM_CARREGAMENTO TO ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO.

    TRY .
        IF LC_NOTA_FISCAL-FORNECEDOR IS INITIAL.
          ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
            )->SET_PARCEIRO_CNPJ_CPF_IE(
            EXPORTING
              I_CNPJ          = I_NOTA_FISCAL-CNPJ_FORNECEDOR   " Code CGC
              I_CPF           = I_NOTA_FISCAL-CPF_FORNECEDOR    " NºCPF
              I_INSC_ESTATUAL = I_NOTA_FISCAL-INSC_FORNECEDOR   " Nº identificação fiscal 3
            )->CK_ATIVO(
            )->CK_ATIVO_EMPRESA( I_EMPRESA = E_ORDEM_CARREGAMENTO-ID_BUKRS_AG
            )->CK_ATIVO_EMPRESA( I_EMPRESA = E_ORDEM_CARREGAMENTO-ID_BUKRS
            )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = DATA(E_PARCEIRO)
            ).
        ELSE.
          ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
            )->SET_PARCEIRO( I_PARCEIRO = LC_NOTA_FISCAL-FORNECEDOR
            )->CK_ATIVO(
            )->CK_ATIVO_EMPRESA( I_EMPRESA = E_ORDEM_CARREGAMENTO-ID_BUKRS_AG
            )->CK_ATIVO_EMPRESA( I_EMPRESA = E_ORDEM_CARREGAMENTO-ID_BUKRS
            )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = E_PARCEIRO
            ).
        ENDIF.

      CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).
        EX_PARCEIROS->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
    ENDTRY.

    TRY .
        IF I_NOTA_FISCAL-TP_PRODUTO IS NOT INITIAL.
          ZCL_DEPOSITO=>ZIF_DEPOSITO~GET_INSTANCE(
            )->GET_DEPOSITO_MATERIAL_FILIAL(
            EXPORTING
              I_MATNR          = I_NOTA_FISCAL-MATERIAL    " Nº do material
              I_TP_PRODUTO     = I_NOTA_FISCAL-TP_PRODUTO    " Tipo de Produto
              I_BUKRS          = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BUKRS    " Empresa
              I_BRANCH         = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BRANCH    " Local de negócios
            IMPORTING
              E_LGORT          = DATA(E_LGORT)  ).
        ELSE.
          E_LGORT = 'ARMZ'.
        ENDIF.
      CATCH ZCX_DEPOSITO.    "
        E_LGORT = 'ARMZ'.
    ENDTRY.

    TRY.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_PEDIDO_COMPRA(
          EXPORTING
            I_BUKRS     = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BUKRS
            I_LIFNR     = E_PARCEIRO
            I_MATNR     = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_PRODUTO
            I_CHARG     = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-NR_SAFRA )
            I_WERKS     = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BRANCH
            I_ID_IVA    = 'M7'
            I_LGORT     = E_LGORT
           IMPORTING
            E_INFO_PEDIDO = DATA(E_INFO_PEDIDO) ).

      CATCH ZCX_DOC_FISCAL_FT_ENTRADA.
        TRY .
            ZCL_PEDIDO_COMPRA=>SET_CRIAR_PEDIDO_COMPRA(
              EXPORTING
                I_BEDAT      = SY-DATUM " Data do documento de compra
                I_EKORG      = 'OC01'    " Organização de compras
                I_EKGRP      = 'G01'    " Grupo de compradores
                I_WAERS      = 'BRL'    " Código da moeda
                I_BSART      = 'ZGR '   " Tipo de documento de compras
                I_ZTERM      = 'Z001'    " Chave de condições de pagamento
                I_LIFNR      = E_PARCEIRO   " Nº conta do fornecedor
                I_BUKRS      = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BUKRS " Empresa
                I_LGORT      = E_LGORT   " Depósito
                I_CHARG      = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-NR_SAFRA )    " Número do lote
                I_EINDT      = SY-DATUM    " Data de remessa do item
                I_MWSKZ      = 'M7'    " Código do IVA
                I_MENGE      = 400000000    " Quantidade do pedido
                I_MATNR      = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_PRODUTO   " Nº do material
                I_MEINS      = 'KG'    " Unidade de medida do pedido
                I_WERKS      = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BRANCH " Centro
                I_TP_CENTRO  = ZCL_PEDIDO_COMPRA=>ST_TP_CENTRO_A_FIXAR
            ).
          CATCH ZCX_CADASTRO INTO DATA(EX_CADASTRO).
            EX_CADASTRO->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
            ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
          CATCH ZCX_PEDIDO_COMPRA_EXCEPTION INTO DATA(EX_PEDIDO_COMPRA_EXCEPTION).
            EX_PEDIDO_COMPRA_EXCEPTION->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
            ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL( ).
        ENDTRY.

        WAIT UP TO 5 SECONDS.

        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_PEDIDO_COMPRA(
           EXPORTING
            I_BUKRS     = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BUKRS
            I_LIFNR     = E_PARCEIRO
            I_MATNR     = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_PRODUTO
            I_CHARG     = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-NR_SAFRA )
            I_WERKS     = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BRANCH
            I_ID_IVA    = 'M7'
            I_LGORT     = E_LGORT
           IMPORTING
            E_INFO_PEDIDO = E_INFO_PEDIDO ).

    ENDTRY.

    "Ordem de Venda
    "Pedido de Compra
    "Tipo de Entrada
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_CARGA          = LC_NOTA_FISCAL-ID_CARGA.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_NOTA           = LC_NOTA_FISCAL-ID_NOTA.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VLR_TOTAL         = LC_NOTA_FISCAL-VL_NOTA_FISCAL.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-CK_NFE            = LC_NOTA_FISCAL-NFE.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NUMERO            = LC_NOTA_FISCAL-NUMERO.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-SERIE             = LC_NOTA_FISCAL-SERIE.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELN             = E_INFO_PEDIDO-EKPO-EBELN.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELP             = E_INFO_PEDIDO-EKPO-EBELP.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-WERKS             = E_INFO_PEDIDO-EKPO-WERKS.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-LGORT             = E_INFO_PEDIDO-EKPO-LGORT.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-CHARG             = E_INFO_PEDIDO-EKET-CHARG.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AUTOMATICO        = ABAP_TRUE.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_ROTA_REPOM     = I_ID_ROTA_REPOM.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_PERCURSO_REPOM = I_ID_PERCURSO_REPOM.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NR_CARTAO_REPOM   = I_NR_CARTAO_REPOM.

    "Fornecedor SAP
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_REMETENTE   = E_INFO_PEDIDO-EKKO-LIFNR.
    "Fornecedor SAP
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA  = I_ID_LOCAL_COLETA.
    "Cliente SAP
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BRANCH.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA
      IMPORTING
        OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA.

    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_DEST_MERC   = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PESO_LIQ        = I_PESO_LIQ.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_ORDEM_CAR   = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-NR_ORDEM.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-SAFRA_ORDEM_CAR = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-NR_SAFRA.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_ORDEM        = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_ORDEM.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ZDT_MOV         = SY-DATUM.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAV       = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-DS_PLACA_TRATOR.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR1      = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-DS_PLACA_REBOQ_1.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR2      = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-DS_PLACA_REBOQ_2.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAR3      = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-DS_PLACA_REBOQ_3.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-MOTORISTA       = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_MOTORISTA.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE    = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_ORDEM_CARREGAMENTO-ID_BRANCH_AG.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE
      IMPORTING
        OUTPUT = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE.

    "Notas Fiscal
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-EBELN          = E_INFO_PEDIDO-EKPO-EBELN.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-EBELP          = E_INFO_PEDIDO-EKPO-EBELP.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-WERKS          = E_INFO_PEDIDO-EKPO-WERKS.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-LGORT          = E_INFO_PEDIDO-EKPO-LGORT.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHARG          = E_INFO_PEDIDO-EKET-CHARG.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-NFE            = LC_NOTA_FISCAL-NFE.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MODELO         = LC_NOTA_FISCAL-MODELO.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-SERIE          = LC_NOTA_FISCAL-SERIE.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-NUMERO         = LC_NOTA_FISCAL-NUMERO.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CLIENTE        = E_PARCEIRO.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-DTEMISSAO      = LC_NOTA_FISCAL-DTEMISSAO.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_BC          = LC_NOTA_FISCAL-VL_BC.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_ICMS        = LC_NOTA_FISCAL-VL_ICMS.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_BC_ST       = LC_NOTA_FISCAL-VL_BC_ST.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_ST          = LC_NOTA_FISCAL-VL_ST.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_PRODUTOS    = LC_NOTA_FISCAL-VL_PRODUTOS.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VL_NOTA_FISCAL = LC_NOTA_FISCAL-VL_NOTA_FISCAL.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL       = LC_NOTA_FISCAL-MATERIAL.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-QUANTIDADE     = LC_NOTA_FISCAL-QUANTIDADE.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-UNIDADE        = LC_NOTA_FISCAL-UNIDADE.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CFOP           = LC_NOTA_FISCAL-CFOP.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-LC_RETIRADA    = LC_NOTA_FISCAL-LC_RETIRADA.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-PIN_SUFRAMA    = LC_NOTA_FISCAL-PIN_SUFRAMA.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-NAME1          = LC_NOTA_FISCAL-NAME1.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-STCD1          = LC_NOTA_FISCAL-STCD1.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-STCD2          = LC_NOTA_FISCAL-STCD2.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHAVE          = LC_NOTA_FISCAL-CHAVE.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-ID_CARGA       = LC_NOTA_FISCAL-ID_CARGA.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-ID_NOTA        = LC_NOTA_FISCAL-ID_NOTA.

    IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHAVE IS NOT INITIAL.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-DOCNUM9 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHAVE+34(9).
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CDV     = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHAVE+43(1).
    ENDIF.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        P_PARCEIRO   = E_PARCEIRO
        P_PARTYPE    = 'V'
      CHANGING
        WA_INFO_PART = WA_INFO_K
        WA_INFO_C    = WA_INFO_C.

    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-STCD1   = WA_INFO_K-STCD1.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-STCD2   = WA_INFO_K-STCD2.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-NAME1   = WA_INFO_K-NAME1.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-PFISICA = WA_INFO_K-STKZN.

    CLEAR: ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-NFE.
    IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MODELO EQ '55'.
      ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-NFE = 'X'.

      ZCL_NFE_XML=>ZIF_NFE_XML~GET_INSTANCE(
        )->SET_REGISTRO( I_CHAVE = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CHAVE
        )->GET_VALIDAR(
         EXPORTING
           I_MATERIAL       =  LC_NOTA_FISCAL-MATERIAL   " Nº do material
        IMPORTING
          E_VALIDACAO      = DATA(E_VALIDACAO)
        ).

      IF E_VALIDACAO-CK_ERRO EQ ABAP_TRUE.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL_STRING( I_TEXTO = E_VALIDACAO-DS_MESSAGEM ).
      ELSEIF E_VALIDACAO-NM_QTD_ITENS GT 1.
        MESSAGE ID ZCX_CARGA=>ZCX_NFE_MANY_ROWS-MSGID TYPE 'S' NUMBER ZCX_CARGA=>ZCX_NFE_MANY_ROWS-MSGNO INTO DATA(MTEXT) .
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_ERRO_GERAL_STRING( I_TEXTO = MTEXT ).
      ENDIF.

    ENDIF.

    "Item do Aviso de Recebimento
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-EBELN      = E_INFO_PEDIDO-EKPO-EBELN.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-EBELP      = E_INFO_PEDIDO-EKPO-EBELP.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-WERKS      = E_INFO_PEDIDO-EKPO-WERKS.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-LGORT      = E_INFO_PEDIDO-EKPO-LGORT.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-CHARG      = E_INFO_PEDIDO-EKET-CHARG.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-LIFNR      = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-CLIENTE.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-MATNR      = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-NFNUM      = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-NUMERO.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-SERIE      = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-SERIE.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-QTDE_AVISO = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PESO_LIQ.
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-UNIDADE    = 'KG'.

    "Aviso de Recebimento
    TRY.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_AVISO_RECEBIMENTO(
          EXPORTING
            I_NUMERO    = LC_NOTA_FISCAL-NUMERO
            I_SERIE     = LC_NOTA_FISCAL-SERIE
            I_EMISSOR   = E_PARCEIRO
          IMPORTING
           E_LIKP      = DATA(E_LIKP) ).

        SELECT SINGLE * INTO @DATA(WA_LIPS)
          FROM LIPS
         WHERE VBELN EQ @E_LIKP-VBELN.

        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-MATERIAL      = WA_LIPS-MATNR.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-UNIDADE       = 'KG'.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-UNIDADE = 'KG'.

        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_NOTA_FISCAL-VBELN       = E_LIKP-VBELN.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN = E_LIKP-VBELN.
        ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEB_ITEMS-VBELN = E_LIKP-VBELN.

      CATCH ZCX_DOC_FISCAL_FT_ENTRADA .

        ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_CK_ITINERARIO_RELEVANTE(
         )->SET_CRIAR_AVISO_RECEBIMENTO(
         )->SET_GRAVAR_REGISTRO(
         ).
    ENDTRY.

    E_EBELN = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELN.
    E_EBELP = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-EBELP.
    E_VBELN = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN.

    "Busca/Valida Tarifa de Frete
    "Valida Tarifa de Seguro
    "Valida Tarifa de IOF
    ME->ZIF_DOC_FISCAL_FT_ENTRADA~GET_PRECO_TARIFA_FRETE(
          IMPORTING
            E_TARIFA = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-KBETR
     )->GET_ITINERARIO(
          IMPORTING
            E_TVRO = DATA(E_TVRO)
     )->GET_CK_CONDICAO_ZSEG(
     )->GET_CK_CONDICAO_ZIOF(
     )->GET_TIPO_TRANSPORTE(
          IMPORTING
            E_SHTYP = DATA(E_SHTYP)
     )->GET_LOCAL_COLETA(
          IMPORTING
            E_LFA1  = DATA(LC_COLETA)
     )->GET_INFO_TIPO_TRANSPORTE(
          EXPORTING
            I_SHTYP = E_SHTYP
          IMPORTING
            E_TVTK  = DATA(E_TVTK)
     )->GET_CK_ADIANTAMENTO(
     )->GET_CK_ITINERARIO_PEDAGIO(
     ).

    "Gerar Documento de Transporte
    "Pedídágio Automático
    "Adiantamento Automático
    "Gerar Documento de Custo
    "Gerar Ordem de Venda
    "Gerar Faturamento

*           E_DOC_TRANSP
*           E_FKNUM
*           E_OV_FRETE
*           E_FATURA_FRETE
*           E_NRO_NF_FRETE

    ME->ZIF_DOC_FISCAL_FT_ENTRADA~SET_CRIAR_DOC_TRANSPORTE(
           EXPORTING
             I_TVRO   = E_TVRO
             I_SHTYP  = E_SHTYP
             I_TVTK   = E_TVTK
           IMPORTING
             E_DOC_TRANSP = E_DOC_TRANSP
      )->SET_CRIAR_DOC_CUSTO(
           IMPORTING
             E_FKNUM = E_FKNUM
             E_OV_FRETE = E_OV_FRETE
             E_FATURA_FRETE = E_FATURA_FRETE
      )->SET_ATUALIZA_CONHECIMENTO(
           IMPORTING
             E_NRO_NF_FRETE = E_NRO_NF_FRETE
      )->SET_AUTORIZAR_DOCUMENTO(
      ).

  ENDMETHOD.


  METHOD zif_doc_fiscal_ft_entrada~set_registro.

    DATA: lc_ds_url_danfe_out TYPE char100,
          arquivo             TYPE REF TO zcl_arquivo,
          lc_viagem           TYPE REF TO zcl_ciot_viagem,
          lc_mdfe             TYPE REF TO zcl_mdfe,
          obj_ciot            TYPE REF TO zcl_ciot.  "*-#130491-04.04.2024-JT

    DATA:
      rg_ebeln TYPE RANGE OF ebeln,
      rg_ebelp TYPE RANGE OF ebelp,
      rg_werks TYPE RANGE OF ewerk,
      rg_lgort TYPE RANGE OF lgort_d,
      rg_charg TYPE RANGE OF charg_d.

    r_instancia = me.

    IF i_ebeln IS NOT INITIAL.
      rg_ebeln = VALUE #( option = 'EQ' sign = 'I' ( high = i_ebeln low = i_ebeln ) ).
    ENDIF.

    IF i_ebelp IS NOT INITIAL.
      rg_ebelp = VALUE #( option = 'EQ' sign = 'I' ( high = i_ebelp low = i_ebelp ) ).
    ENDIF.

    IF i_werks IS NOT INITIAL.
      rg_werks = VALUE #( option = 'EQ' sign = 'I' ( high = i_werks low = i_werks ) ).
    ENDIF.

    IF i_lgort IS NOT INITIAL.
      rg_lgort = VALUE #( option = 'EQ' sign = 'I' ( high = i_lgort low = i_lgort ) ).
    ENDIF.

    IF i_charg IS NOT INITIAL.
      rg_charg = VALUE #( option = 'EQ' sign = 'I' ( high = i_charg low = i_charg ) ).
    ENDIF.

    SELECT SINGLE * INTO me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento
      FROM zlest0108
     WHERE ebeln IN rg_ebeln
       AND ebelp IN rg_ebelp
       AND werks IN rg_werks
       AND lgort IN rg_lgort
       AND charg IN rg_charg
       AND vbeln EQ i_vbeln.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_doc_fiscal_ft_entrada
        EXPORTING
          textid = VALUE #( msgid = zcx_doc_fiscal_ft_entrada=>zcx_ent_frete_nao_encontrada-msgid
                            msgno = zcx_doc_fiscal_ft_entrada=>zcx_ent_frete_nao_encontrada-msgno )
          msgid  = zcx_doc_fiscal_ft_entrada=>zcx_ent_frete_nao_encontrada-msgid
          msgno  = zcx_doc_fiscal_ft_entrada=>zcx_ent_frete_nao_encontrada-msgno
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE * INTO me->zif_doc_fiscal_ft_entrada~at_aviso_receb_items
      FROM zlest0109
     WHERE ebeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebeln
       AND ebelp EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebelp
       AND werks EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-werks
       AND lgort EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-lgort
       AND charg EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-charg
       AND vbeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln.

    SELECT SINGLE * INTO me->zif_doc_fiscal_ft_entrada~at_nota_fiscal
      FROM zlest0110
     WHERE ebeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebeln
       AND ebelp EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-ebelp
       AND werks EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-werks
       AND lgort EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-lgort
       AND charg EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-charg
       AND vbeln EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-vbeln.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-id_ordem IS NOT INITIAL.
      SELECT SINGLE * INTO me->zif_doc_fiscal_ft_entrada~at_ordem_carregamento
        FROM zsdt0001od
       WHERE id_ordem EQ me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-id_ordem.
    ENDIF.

    CHECK i_nao_carregar_arquivos EQ abap_false.

    IF me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete IS NOT INITIAL.

      CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
        EXPORTING
          doc_numero       = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete
          imprimir         = abap_false
        CHANGING
          ds_url_danfe_out = lc_ds_url_danfe_out
        EXCEPTIONS
          nao_localizado   = 1
          OTHERS           = 2.

      IF sy-subrc IS INITIAL AND lc_ds_url_danfe_out IS NOT INITIAL.
        CREATE OBJECT arquivo.
        TRY   .
            arquivo->get_file_uri_get(
               EXPORTING
                 i_uri = CONV #( lc_ds_url_danfe_out )
               IMPORTING
                 e_texto_2 = me->zif_doc_fiscal_ft_entrada~at_dacte ).
          CATCH zcx_arquivo.
        ENDTRY.
        CLEAR: arquivo.
      ENDIF.

      CALL FUNCTION 'Z_SD_PRINT_DECLARA'
        EXPORTING
          doc_numero   = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete
          imprimir     = abap_false
        IMPORTING
          e_declaracao = me->zif_doc_fiscal_ft_entrada~at_declaracao.

      CREATE OBJECT lc_viagem.
      lc_viagem->imprimir_contrato(
        EXPORTING
          cte_docnum  = me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete    " Nº documento
          imprimir    = abap_false
        IMPORTING
          e_url       = DATA(e_url_viagem)
        EXCEPTIONS
          nao_ciot    = 1
          erro_status = 2
          OTHERS      = 3
      ).

      IF sy-subrc IS INITIAL.
        IF e_url_viagem IS NOT INITIAL AND e_url_viagem(4) = 'http'.  "*-#130491-04.04.2024-JT
          CREATE OBJECT arquivo.
          TRY   .
              arquivo->get_file_uri_get(
                 EXPORTING
                   i_uri = e_url_viagem
                 IMPORTING
                   e_texto_2 = me->zif_doc_fiscal_ft_entrada~at_contrato_viagem ).
            CATCH zcx_arquivo.
          ENDTRY.
          CLEAR: arquivo.
        ELSE.
*-#130491-04.04.2024-JT-inicio
          SELECT SINGLE cd_ciot
            INTO @DATA(_cd_ciot)
            FROM zcte_ciot
           WHERE docnum = @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete.

          IF sy-subrc = 0.
            CREATE OBJECT obj_ciot.
            me->zif_doc_fiscal_ft_entrada~at_contrato_viagem = obj_ciot->get_pdf_arquivos_viagem( i_cd_ciot = _cd_ciot i_tipoarq = 'CONTRATO' ).
          ENDIF.
*-#130491-04.04.2024-JT-fim
        ENDIF.
      ENDIF.

      SELECT *
        INTO TABLE @DATA(it_zsdt0105)
        FROM zsdt0105
       WHERE docnum_ref NE 0
         AND nmdfe      NE @abap_false
         AND docnum     EQ @me->zif_doc_fiscal_ft_entrada~at_aviso_recebimento-nro_nf_frete.

      LOOP AT it_zsdt0105 INTO DATA(wa_zsdt0105).
        SELECT SINGLE *
          INTO @DATA(wa_zsdt0102)
          FROM zsdt0102
         WHERE docnum    EQ @wa_zsdt0105-docnum_ref
           AND nmdfe     EQ @wa_zsdt0105-nmdfe
           AND estornado NE @abap_true
           AND cancel    NE @abap_true
           AND encerrado NE @abap_true.

        IF sy-subrc IS INITIAL.
          CREATE OBJECT lc_mdfe
            EXPORTING
              i_nmdfe  = wa_zsdt0102-nmdfe
              i_docnum = wa_zsdt0102-docnum.

          lc_mdfe->print_mdfe( EXPORTING i_imprimir = abap_false IMPORTING e_url = DATA(e_url_mdfe) ).
          IF e_url_mdfe IS NOT INITIAL.
            CREATE OBJECT arquivo.
            TRY   .
                arquivo->get_file_uri_get(
                   EXPORTING
                     i_uri = e_url_mdfe
                   IMPORTING
                     e_texto_2 = me->zif_doc_fiscal_ft_entrada~at_mdfe ).
              CATCH zcx_arquivo.
            ENDTRY.
            CLEAR: arquivo.
          ENDIF.
          CLEAR: lc_mdfe.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DOC_FISCAL_FT_ENTRADA~SET_VALIDAR_AVISO.

    R_INSTANCIA = ME.

    "Verificar Igualdade
    IF ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_AGRUPA_FRETE IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(WA_ANTERIOR)
        FROM ZLEST0108
       WHERE ID_AGRUPA_FRETE EQ @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-ID_AGRUPA_FRETE
         AND VBELN NE @ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN.

      IF SY-SUBRC IS INITIAL.
        IF ME->CK_ESTORNANDO EQ ABAP_FALSE.
          IF ( WA_ANTERIOR-DOC_TRANSP NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-DOC_TRANSP AND WA_ANTERIOR-DOC_TRANSP IS NOT INITIAL ) OR
             ( WA_ANTERIOR-FKNUM NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FKNUM AND WA_ANTERIOR-FKNUM IS NOT INITIAL )  OR
             ( WA_ANTERIOR-OV_FRETE NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-OV_FRETE AND WA_ANTERIOR-OV_FRETE IS NOT INITIAL )  OR
             ( WA_ANTERIOR-FATURA_FRETE NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-FATURA_FRETE AND WA_ANTERIOR-FATURA_FRETE IS NOT INITIAL )  OR
             ( WA_ANTERIOR-NRO_NF_FRETE NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-NRO_NF_FRETE AND WA_ANTERIOR-NRO_NF_FRETE IS NOT INITIAL ) .
            RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
              EXPORTING
                TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGRUPA_EST-MSGID
                                  MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGRUPA_EST-MSGNO )
                MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGRUPA_EST-MSGID
                MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGRUPA_EST-MSGNO
                MSGTY  = 'E'.
          ENDIF.
        ENDIF.

        "Filial
        IF WA_ANTERIOR-WERKS NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-WERKS.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_FILIAL-MSGID
                                MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_FILIAL-MSGNO
                                ATTR1 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                                ATTR2 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-WERKS
                                ATTR3 = WA_ANTERIOR-WERKS )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_FILIAL-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_FILIAL-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN )
              MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-WERKS )
              MSGV3  = CONV #( WA_ANTERIOR-WERKS ).
        ENDIF.

        "Local de Estoque
        IF WA_ANTERIOR-LGORT NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-LGORT.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_ESTOQUE-MSGID
                                MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_ESTOQUE-MSGNO
                                ATTR1 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                                ATTR2 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-LGORT
                                ATTR3 = WA_ANTERIOR-LGORT )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_ESTOQUE-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_ESTOQUE-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN )
              MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-LGORT )
              MSGV3  = CONV #( WA_ANTERIOR-LGORT ).
        ENDIF.

        "Safra
        IF WA_ANTERIOR-CHARG NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-CHARG.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_SAFRA-MSGID
                                MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_SAFRA-MSGNO
                                ATTR1 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                                ATTR2 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-CHARG
                                ATTR3 = WA_ANTERIOR-CHARG )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_SAFRA-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_SAFRA-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN )
              MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-CHARG )
              MSGV3  = CONV #( WA_ANTERIOR-CHARG ).
        ENDIF.

        "Agente de Frete Diferente
        IF WA_ANTERIOR-AGENTE_FRETE NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGENTE-MSGID
                                MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGENTE-MSGNO
                                ATTR1 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                                ATTR2 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE
                                ATTR3 = WA_ANTERIOR-AGENTE_FRETE )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGENTE-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_AGENTE-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN )
              MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-AGENTE_FRETE )
              MSGV3  = CONV #( WA_ANTERIOR-AGENTE_FRETE ).
        ENDIF.

        "Remetente
        IF WA_ANTERIOR-COD_REMETENTE NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_REMETENTE.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_REME-MSGID
                                MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_REME-MSGNO
                                ATTR1 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                                ATTR2 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_REMETENTE
                                ATTR3 = WA_ANTERIOR-COD_REMETENTE )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_REME-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_REME-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN )
              MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_REMETENTE )
              MSGV3  = CONV #( WA_ANTERIOR-COD_REMETENTE ).
        ENDIF.

        "Destinatário
        IF WA_ANTERIOR-COD_DEST_MERC NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_DEST_MERC.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_DEST-MSGID
                                MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_DEST-MSGNO
                                ATTR1 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                                ATTR2 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_DEST_MERC
                                ATTR3 = WA_ANTERIOR-COD_DEST_MERC )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_DEST-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_DEST-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN )
              MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_DEST_MERC )
              MSGV3  = CONV #( WA_ANTERIOR-COD_DEST_MERC ).
        ENDIF.

        "Ponto de Coleta
        IF WA_ANTERIOR-COD_LOC_COLETA NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_COLE-MSGID
                                MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_COLE-MSGNO
                                ATTR1 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                                ATTR2 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA
                                ATTR3 = WA_ANTERIOR-COD_LOC_COLETA )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_COLE-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_COLE-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN )
              MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_COLETA )
              MSGV3  = CONV #( WA_ANTERIOR-COD_LOC_COLETA ).
        ENDIF.

        "Ponto de Entrega
        IF WA_ANTERIOR-COD_LOC_ENTREGA NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_ENTRE-MSGID
                                MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_ENTRE-MSGNO
                                ATTR1 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                                ATTR2 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA
                                ATTR3 = WA_ANTERIOR-COD_LOC_ENTREGA )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_ENTRE-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_ENTRE-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN )
              MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-COD_LOC_ENTREGA )
              MSGV3  = CONV #( WA_ANTERIOR-COD_LOC_ENTREGA ).
        ENDIF.

        "Veículo de Tração
        IF WA_ANTERIOR-PLACA_CAV NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAV.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_PLACA-MSGID
                                MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_PLACA-MSGNO
                                ATTR1 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                                ATTR2 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAV
                                ATTR3 = WA_ANTERIOR-PLACA_CAV )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_PLACA-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_PLACA-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN )
              MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-PLACA_CAV )
              MSGV3  = CONV #( WA_ANTERIOR-PLACA_CAV ).
        ENDIF.

        "Motorista
        IF WA_ANTERIOR-MOTORISTA NE ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-MOTORISTA.
          RAISE EXCEPTION TYPE ZCX_DOC_FISCAL_FT_ENTRADA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_MOTO-MSGID
                                MSGNO = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_MOTO-MSGNO
                                ATTR1 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN
                                ATTR2 = ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-MOTORISTA
                                ATTR3 = WA_ANTERIOR-MOTORISTA )
              MSGID  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_MOTO-MSGID
              MSGNO  = ZCX_DOC_FISCAL_FT_ENTRADA=>ZCX_ENT_FRETE_MOTO-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-VBELN )
              MSGV2  = CONV #( ME->ZIF_DOC_FISCAL_FT_ENTRADA~AT_AVISO_RECEBIMENTO-MOTORISTA )
              MSGV3  = CONV #( WA_ANTERIOR-MOTORISTA ).
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
