class ZCL_MATERIAL_DESTINACAO definition
  public
  create public .

public section.

  interfaces ZIF_MATERIAL_DESTINACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MATERIAL_DESTINACAO IMPLEMENTATION.


  METHOD ZIF_MATERIAL_DESTINACAO~FREE.

    R_IF_MATERIAL_DESTINACAO = ME.

    IF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO IS NOT INITIAL.
      ME->ZIF_MATERIAL_DESTINACAO~SET_DENQUEUE( I_ID_DESTINACAO = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~GET_INSTANCE.

    IF ZIF_MATERIAL_DESTINACAO~IF_MATERIAL_DESTINACAO IS NOT BOUND.
      CREATE OBJECT ZIF_MATERIAL_DESTINACAO~IF_MATERIAL_DESTINACAO TYPE ZCL_MATERIAL_DESTINACAO.
    ENDIF.
    R_IF_MATERIAL_DESTINACAO = ZIF_MATERIAL_DESTINACAO~IF_MATERIAL_DESTINACAO.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~GET_NEW_ID.

    R_IF_MATERIAL_DESTINACAO = ME.

    CLEAR: E_ID_DESTINACAO.

    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        OBJECT           = 'ZDESTINA'
      EXCEPTIONS
        FOREIGN_LOCK     = 1
        OBJECT_NOT_FOUND = 2
        SYSTEM_FAILURE   = 3
        OTHERS           = 4.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
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
        OBJECT                  = 'ZDESTINA'
        QUANTITY                = '0000000001'
        IGNORE_BUFFER           = 'X'
      IMPORTING
        NUMBER                  = E_ID_DESTINACAO
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
      RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
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
        OBJECT           = 'ZDESTINA'
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
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


  METHOD ZIF_MATERIAL_DESTINACAO~GET_REGISTRO.

    IF_MATERIAL_DESTINACAO = ME.

    E_ZMMT0114 = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~GET_TEXTO_NOTA_FISCAL.

    IF_MATERIAL_DESTINACAO = ME.

    CALL FUNCTION 'ZMF_DESTINACAO_MERC_OBS_FISCAL'
      EXPORTING
        I_ID_DESTINACAO = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO
        I_INFORMAR      = ABAP_FALSE
      IMPORTING
        E_TEXTO         = E_STRING.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~GET_TEXTO_NOTA_FISCAL_DOCNUM.

    CLEAR: R_TEXTO .

    SELECT SINGLE * INTO @DATA(WA_ZMMT0114)
      FROM ZMMT0114
     WHERE DOCNUM EQ @I_DOCNUM.

    IF SY-SUBRC IS NOT INITIAL.
      SELECT SINGLE * INTO @WA_ZMMT0114
        FROM ZMMT0114
       WHERE DOCNUM_DEV EQ @I_DOCNUM.
    ENDIF.

    CHECK SY-SUBRC IS INITIAL.

    ZCL_FACTORY_MAT_DESTINACAO=>ZIF_FACTORY_MAT_DESTINACAO~GET_INSTANCE(
      )->SET_FACTORY_OBJETO( EXPORTING I_ID_DESTINACAO = WA_ZMMT0114-ID_DESTINACAO
      )->GET_FACTORY_OBJETO(
      )->SET_REGISTRO( I_ID_DESTINACAO = WA_ZMMT0114-ID_DESTINACAO
      )->GET_TEXTO_NOTA_FISCAL( IMPORTING E_STRING = R_TEXTO
      ).

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~GET_TEXTO_NOTA_FISCAL_PADRAO.

    IF_MATERIAL_DESTINACAO = ME.

    CLEAR: E_STRING.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_ADD_DOC_MATERIAL_ORIGEM.

    DATA: VL_LIFNR TYPE LIFNR,
          WA_LFA1  TYPE LFA1.

    R_IF_MATERIAL_DESTINACAO = ME.

    DATA(LC_TP_ORIGEM) = COND STRING(
                                  WHEN I_ORIG_NFE IS NOT INITIAL THEN ZIF_MATERIAL_DESTINACAO=>ST_TP_ORIGEM_NFE
                                  WHEN I_ORIG_ROMANEIO IS NOT INITIAL THEN ZIF_MATERIAL_DESTINACAO=>ST_TP_ORIGEM_ROMANEIO ).

    IF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_ORIGEM IS INITIAL AND LC_TP_ORIGEM IS NOT INITIAL.

      ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_ORIGEM = LC_TP_ORIGEM.

    ELSEIF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_ORIGEM NE LC_TP_ORIGEM.

      RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_NAO_PERMITIDA-MSGID
                            MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_NAO_PERMITIDA-MSGNO
                            ATTR1 = LC_TP_ORIGEM )
          MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_NAO_PERMITIDA-MSGID
          MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_NAO_PERMITIDA-MSGNO
          MSGTY  = SY-MSGTY
          MSGV1  = 'E'.

    ELSEIF LC_TP_ORIGEM IS NOT INITIAL AND LC_TP_ORIGEM NE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_ORIGEM.

      RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_INFORMAR-MSGID
                            MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_INFORMAR-MSGNO )
          MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_INFORMAR-MSGID
          MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_INFORMAR-MSGNO
          MSGTY  = 'E'.

    ENDIF.

    CASE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_ORIGEM.
      WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_ORIGEM_NFE.

        SELECT SINGLE * INTO @DATA(W_NFE)
          FROM ZIB_NFE_DIST_TER
         WHERE CHAVE_NFE EQ @I_ORIG_NFE.

        CASE ME->ZIF_MATERIAL_DESTINACAO~AT_TP_DESTINACAO.
          WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_ARMAZENAR.

            CASE W_NFE-CK_TRANS_NF_PROPRI.
              WHEN ABAP_TRUE.
                "Saída para Armazenagem com Frete
                ME->ZIF_MATERIAL_DESTINACAO~AT_TIPO_MOVIMENTO = 'ZQ1'.
              WHEN ABAP_FALSE.
                "Saída para Armazenagem sem Frete
                ME->ZIF_MATERIAL_DESTINACAO~AT_TIPO_MOVIMENTO = 'Z41'.
            ENDCASE.

          WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_RETORNO.

            READ TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0116 INDEX 1 INTO DATA(WA_ZMMT0116).

            SELECT SINGLE * INTO @DATA(WA_NFE_SAIDA)
              FROM ZIB_NFE_DIST_TER
             WHERE MBLNR EQ @WA_ZMMT0116-ORIG_MBLNR
               AND MJAHR EQ @WA_ZMMT0116-ORIG_MJAHR.

            CASE W_NFE-CK_TRANS_NF_PROPRI.
              WHEN ABAP_TRUE.
                "Retorno de Armazenagem com frete
                ME->ZIF_MATERIAL_DESTINACAO~AT_TIPO_MOVIMENTO = 'ZQ2'.
              WHEN ABAP_FALSE.
                "Retorno de Armazenagem sem frete
                ME->ZIF_MATERIAL_DESTINACAO~AT_TIPO_MOVIMENTO = 'Z42'.
            ENDCASE.

        ENDCASE.

        SELECT SINGLE * INTO @DATA(WA_MSEG)
          FROM MSEG
         WHERE MBLNR EQ @I_ORIG_MBLNR
           AND MJAHR EQ @I_ORIG_MJAHR
           AND ZEILE EQ @I_ORIG_ZEILE.

        IF WA_MSEG-XAUTO NE ABAP_FALSE.
          RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_ESTOQUE_ESPECIAL-MSGID
                                MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_ESTOQUE_ESPECIAL-MSGNO
                                ATTR1 = WA_MSEG-MBLNR
                                ATTR2 = WA_MSEG-MJAHR
                                ATTR3 = WA_MSEG-ZEILE )
              MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_ESTOQUE_ESPECIAL-MSGID
              MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_ESTOQUE_ESPECIAL-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( WA_MSEG-MBLNR )
              MSGV2  = CONV #( WA_MSEG-MJAHR )
              MSGV3  = CONV #( WA_MSEG-ZEILE ).
        ENDIF.

        IF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BUKRS IS INITIAL.
          ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BUKRS = WA_MSEG-BUKRS.
        ELSEIF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BUKRS NE WA_MSEG-BUKRS.
          RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_EMPRESA-MSGID
                                MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_EMPRESA-MSGNO
                                ATTR1 = WA_MSEG-BUKRS )
              MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_EMPRESA-MSGID
              MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_EMPRESA-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( WA_MSEG-BUKRS ).
        ENDIF.

        READ TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115 WITH KEY ORIG_MBLNR = I_ORIG_MBLNR ORIG_MJAHR = I_ORIG_MJAHR TRANSPORTING NO FIELDS.
        IF SY-SUBRC IS NOT INITIAL.
          APPEND VALUE #( ORIG_MBLNR      = I_ORIG_MBLNR
                          ORIG_MJAHR      = I_ORIG_MJAHR
                          ORIG_NFE        = I_ORIG_NFE
                          ORIG_ROMANEIO   = I_ORIG_ROMANEIO
                          CK_TOTAL_ORIGEM = I_CK_TOTAL_ORIGEM ) TO ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115.
        ENDIF.

        READ TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0116 WITH KEY ORIG_MBLNR = I_ORIG_MBLNR ORIG_MJAHR = I_ORIG_MJAHR ORIG_ZEILE = I_ORIG_ZEILE
        ASSIGNING FIELD-SYMBOL(<FS_ZMMT0016>).

        IF SY-SUBRC IS INITIAL.
          ADD I_MENGE TO <FS_ZMMT0016>-MENGE.
          ADD I_VALOR TO <FS_ZMMT0016>-VALOR.
        ELSE.
          APPEND VALUE #( ORIG_MBLNR    = I_ORIG_MBLNR
                          ORIG_MJAHR    = I_ORIG_MJAHR
                          ORIG_ZEILE    = I_ORIG_ZEILE
                          MENGE         = I_MENGE
                          MEINS         = I_MEINS
                          VALOR         = I_VALOR
                          FORNE         = I_FORNE
                          MATNR         = WA_MSEG-MATNR
                          LGORT         = WA_MSEG-LGORT
                          CHARG         = WA_MSEG-CHARG
                          WERKS         = WA_MSEG-WERKS
                          MWSKZ         = 'ZE'
                          EBELN         = WA_MSEG-EBELN
                          EBELP         = WA_MSEG-EBELP ) TO ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0116.
        ENDIF.

      WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_ORIGEM_ROMANEIO.

        SELECT SINGLE * INTO @DATA(WA_ZSDT0001)
          FROM ZSDT0001
         WHERE CH_REFERENCIA EQ @I_ORIG_ROMANEIO.

        IF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BUKRS IS INITIAL.
          ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BUKRS = WA_ZSDT0001-BUKRS.
        ELSEIF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BUKRS NE WA_ZSDT0001-BUKRS.
          RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_EMPRESA-MSGID
                                MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_EMPRESA-MSGNO
                                ATTR1 = WA_ZSDT0001-BUKRS )
              MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_EMPRESA-MSGID
              MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_EMPRESA-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( WA_ZSDT0001-BUKRS ).
        ENDIF.

        SELECT SINGLE * INTO @DATA(WA_EKKO) FROM EKKO WHERE EBELN EQ @WA_ZSDT0001-VBELN.

        SELECT SINGLE * INTO @DATA(WA_EKPO) FROM EKPO WHERE EBELN EQ @WA_ZSDT0001-VBELN.

        SELECT SINGLE * INTO @DATA(WA_EKET) FROM EKET WHERE EBELN EQ @WA_EKPO-EBELN AND EBELP EQ @WA_EKPO-EBELP.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_EKPO-WERKS
          IMPORTING
            OUTPUT = VL_LIFNR.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            P_PARCEIRO   = VL_LIFNR
            P_PARTYPE    = 'B'
          CHANGING
            WA_INFO_PART = WA_LFA1.

        CASE ME->ZIF_MATERIAL_DESTINACAO~AT_TP_DESTINACAO.
          WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_ARMAZENAR.
            SELECT SINGLE T~DESCRIPT
              FROM SETLEAF AS S
             INNER JOIN SETLINET AS T ON T~SETNAME EQ S~SETNAME AND T~LINEID EQ S~LINEID
              INTO @DATA(VL_DESCRIPT)
             WHERE S~SETNAME = 'MAGGI_ZMM0019_IVA_SAIDA'
               AND S~VALFROM = @WA_LFA1-REGIO.
          WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_RETORNO.
            SELECT SINGLE T~DESCRIPT
              FROM SETLEAF AS S
             INNER JOIN SETLINET AS T ON T~SETNAME EQ S~SETNAME AND T~LINEID EQ S~LINEID
              INTO @VL_DESCRIPT
             WHERE S~SETNAME = 'MAGGI_ZMM0019_IVA_ENTRAD'
               AND S~VALFROM = @WA_LFA1-REGIO.
        ENDCASE.

        TRY .
            ZCL_DEPOSITO=>ZIF_DEPOSITO~GET_INSTANCE(
              )->GET_DEPOSITO_MATERIAL_FILIAL(
                    EXPORTING
                      I_MATNR = WA_EKPO-MATNR
                      I_TP_PRODUTO = CONV #( COND STRING( WHEN WA_ZSDT0001-TP_TRANSGENIA(1) EQ 'C' THEN ZIF_CARGA=>ST_TP_TRANSGENIASE_CO ELSE 'RR' ) ) " Tipo de Produto
                      I_BUKRS = WA_EKKO-BUKRS
                      I_BRANCH = WA_EKPO-WERKS
                    IMPORTING
                      E_LGORT          = WA_EKPO-LGORT
                      E_CENTRO_A_FIXAR = DATA(E_CENTRO_A_FIXAR)
              ).
          CATCH ZCX_DEPOSITO INTO DATA(EX_DEPOSITO).    "
            EX_DEPOSITO->ZIF_ERROR~PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
            EXIT.
        ENDTRY.

        READ TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115 WITH KEY ORIG_ROMANEIO = WA_ZSDT0001-CH_REFERENCIA TRANSPORTING NO FIELDS.
        IF SY-SUBRC IS NOT INITIAL.
          APPEND VALUE #( ORIG_ROMANEIO   = I_ORIG_ROMANEIO
                          CK_TOTAL_ORIGEM = I_CK_TOTAL_ORIGEM ) TO ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115.
        ENDIF.

        READ TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0118 WITH KEY CH_REFERENCIA = WA_ZSDT0001-CH_REFERENCIA
        ASSIGNING FIELD-SYMBOL(<FS_ZMMT0018>).

        IF SY-SUBRC IS INITIAL.
          ADD I_MENGE TO <FS_ZMMT0018>-MENGE.
          ADD I_VALOR TO <FS_ZMMT0018>-VALOR.
        ELSE.
          APPEND VALUE #( CH_REFERENCIA = WA_ZSDT0001-CH_REFERENCIA
                          MENGE         = I_MENGE
                          MEINS         = I_MEINS
                          VALOR         = I_VALOR
                          FORNE         = I_FORNE
                          MATNR         = WA_EKPO-MATNR
                          LGORT         = WA_EKPO-LGORT
                          CHARG         = WA_MSEG-CHARG
                          WERKS         = WA_EKPO-WERKS
                          MWSKZ         = VL_DESCRIPT ) TO ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0118.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_CLEAR.

    R_IF_MATERIAL_DESTINACAO = ME.

    CLEAR: ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114,
           ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115,
           ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0116,
           ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0118,
           ME->ZIF_MATERIAL_DESTINACAO~AT_TP_DESTINACAO.

    ME->ZIF_MATERIAL_DESTINACAO~SET_DEFAULT_CONFIG( ).

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_DEFAULT_CONFIG.
    R_IF_MATERIAL_DESTINACAO = ME.
  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_DENQUEUE.

    DATA: BEGIN OF TY_A_DESTINACAO,
            MANDT	        TYPE SY-MANDT,
            ID_DESTINACAO TYPE ZDE_DESTINACAO,
          END OF TY_A_DESTINACAO.

    R_IF_MATERIAL_DESTINACAO = ME.

    CHECK ME->ZIF_MATERIAL_DESTINACAO~AT_NAO_GERAR_BLOQUEIO EQ ABAP_FALSE.

    DATA: __SEQTA_TAB TYPE TABLE OF SEQTA,
          __SCOPE     TYPE DDENQSCOPE VALUE '3',
          __SYNCHRON  TYPE DDENQSYNC  VALUE SPACE.

    CALL 'C_ENQ_WILDCARD' ID 'HEX0' FIELD TY_A_DESTINACAO.

    TY_A_DESTINACAO-MANDT = SY-MANDT.
    TY_A_DESTINACAO-ID_DESTINACAO = COND STRING( WHEN I_ID_DESTINACAO IS NOT INITIAL THEN I_ID_DESTINACAO ELSE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO )..

    APPEND VALUE #( GNAME = 'ZMMT0114' GMODE = 'X' GARG  = TY_A_DESTINACAO ) TO __SEQTA_TAB.

    PERFORM SEND_ENQUEUE IN PROGRAM SAPLSENA TABLES __SEQTA_TAB USING '2' __SCOPE ' ' __SYNCHRON 'EZMMT0114' ' '.

  ENDMETHOD.


  METHOD zif_material_destinacao~set_enqueue.

    DATA: BEGIN OF ty_a_destinacao,
            mandt	        TYPE sy-mandt,
            id_destinacao TYPE zde_destinacao,
          END OF ty_a_destinacao.

    r_if_material_destinacao = me.

    CHECK i_id_destinacao IS NOT INITIAL OR me->zif_material_destinacao~at_zmmt0114-id_destinacao IS NOT INITIAL.

    TRY .

        CHECK me->zif_material_destinacao~at_nao_gerar_bloqueio EQ abap_false.

        DATA: __seqta_tab TYPE TABLE OF seqta,
              __scope     TYPE ddenqscope VALUE '3'.

        CALL 'C_ENQ_WILDCARD' ID 'HEX0' FIELD ty_a_destinacao.

        ty_a_destinacao-mandt = sy-mandt.
        ty_a_destinacao-id_destinacao = COND string( WHEN i_id_destinacao IS NOT INITIAL THEN i_id_destinacao ELSE me->zif_material_destinacao~at_zmmt0114-id_destinacao ).

        APPEND VALUE #( gname = 'ZMMT0114' gmode = 'X' garg  = ty_a_destinacao ) TO __seqta_tab.

        PERFORM send_enqueue IN PROGRAM saplsena TABLES __seqta_tab USING '1' __scope space space 'EZMMT001' space.

      CATCH cx_root INTO DATA(lc_root).

        RAISE EXCEPTION TYPE zcx_material_destinacao
          EXPORTING
            textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgty  = sy-msgty
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_ESTORNAR_MOVIMENTO.

    R_IF_MATERIAL_DESTINACAO = ME->ZIF_MATERIAL_DESTINACAO~SET_ESTORNAR_NOTA_DEVOLUCAO( ).

    CLEAR: E_RETORNO.

    CHECK NOT (
        ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_DESTINACAO EQ ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_DEVOLUCAO AND
        ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BELNR_ESTORNO IS INITIAL
     ).

    CLEAR: E_ESTORNOU.

    CHECK ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-MBLNR IS NOT INITIAL.

    DATA: LC_MIGO TYPE REF TO ZCL_MIGO.

    CREATE OBJECT LC_MIGO.

    TRY.

        LC_MIGO->ESTORNAR(
          EXPORTING
            I_BAPI_WAIT        = ABAP_TRUE
          IMPORTING
            I_MAT_DOC_ESTORNO  = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-MBLNR_ESTORNO
            I_DOC_YEAR_ESTORNO = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-MJAHR_ESTORNO
          CHANGING
            I_MAT_DOC          = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-MBLNR
            I_DOC_YEAR         = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-MJAHR
          RECEIVING
            R_GEROU            = E_ESTORNOU
        ).

        E_RETORNO = LC_MIGO->GET_RETORNO_ESTORNO( ).

      CATCH ZCX_MIGO_EXCEPTION INTO DATA(EX_MIGO_ERRO).
        IF EX_MIGO_ERRO->MSGID = ZCX_MIGO_EXCEPTION=>ZCX_ESTORNADO-MSGID AND
           EX_MIGO_ERRO->MSGNO = ZCX_MIGO_EXCEPTION=>ZCX_ESTORNADO-MSGNO.
          E_ESTORNOU = ABAP_TRUE.
        ELSE.

          CLEAR: LC_MIGO.

          RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = EX_MIGO_ERRO->MSGID
                                MSGNO = EX_MIGO_ERRO->MSGNO
                                ATTR1 = EX_MIGO_ERRO->MSGV1
                                ATTR2 = EX_MIGO_ERRO->MSGV2
                                ATTR3 = EX_MIGO_ERRO->MSGV3
                                ATTR4 = EX_MIGO_ERRO->MSGV4 )
              MSGID  = EX_MIGO_ERRO->MSGID
              MSGNO  = EX_MIGO_ERRO->MSGNO
              MSGTY  = 'E'
              MSGV1  = EX_MIGO_ERRO->MSGV1
              MSGV2  = EX_MIGO_ERRO->MSGV2
              MSGV3  = EX_MIGO_ERRO->MSGV3
              MSGV4  = EX_MIGO_ERRO->MSGV4.
        ENDIF.
    ENDTRY.

    CLEAR: LC_MIGO.

    IF E_ESTORNOU EQ ABAP_TRUE.
      ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-DT_ESTORNO = SY-DATUM.
      ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-HR_ESTORNO = SY-UZEIT.
      ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-US_ESTORNO = SY-UNAME.
      ME->ZIF_MATERIAL_DESTINACAO~SET_GRAVAR( ).

      CASE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_ORIGEM.
        WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_ORIGEM_NFE.

          CASE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_DESTINACAO.

            WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_DEVOLUCAO.

              READ TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115 INDEX 1 INTO DATA(WA_0115).

              UPDATE ZIB_NFE_DIST_TER
                 SET MBLNR_DEV = SPACE
                     MJAHR_DEV  = SPACE
               WHERE CHAVE_NFE EQ WA_0115-ORIG_NFE.

              COMMIT WORK AND WAIT.

            WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_ARMAZENAR.

              READ TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115 INDEX 1 INTO WA_0115.

              UPDATE ZIB_NFE_DIST_TER
                 SET MBLNR_ARM  = SPACE
                     MJAHR_ARM  = SPACE
                     DOCNUM_ARM = SPACE
               WHERE CHAVE_NFE EQ WA_0115-ORIG_NFE.

              COMMIT WORK AND WAIT.

          ENDCASE.

        WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_ORIGEM_ROMANEIO.
      ENDCASE.

    ELSE.

      READ TABLE E_RETORNO INTO DATA(WA_RETORNO) WITH KEY TYPE = 'E'.

      RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = WA_RETORNO-ID
                            MSGNO = WA_RETORNO-NUMBER
                            ATTR1 = WA_RETORNO-MESSAGE_V1
                            ATTR2 = WA_RETORNO-MESSAGE_V2
                            ATTR3 = WA_RETORNO-MESSAGE_V3
                            ATTR4 = WA_RETORNO-MESSAGE_V4 )
          MSGID  = WA_RETORNO-ID
          MSGNO  = WA_RETORNO-NUMBER
          MSGTY  = 'E'
          MSGV1  = WA_RETORNO-MESSAGE_V1
          MSGV2  = WA_RETORNO-MESSAGE_V2
          MSGV3  = WA_RETORNO-MESSAGE_V3
          MSGV4  = WA_RETORNO-MESSAGE_V4.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_ESTORNAR_NOTA_DEVOLUCAO.

    DATA: R_GEROU   TYPE CHAR01,
          E_RETORNO	TYPE BAPIRET2_T.

    IF_MATERIAL_DESTINACAO = ME.

    CHECK ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_DESTINACAO = ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_DEVOLUCAO.

    CHECK ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BELNR_DEV IS NOT INITIAL.

    DATA: MIRO TYPE REF TO ZCL_MIRO.

    CREATE OBJECT MIRO.

    TRY .

        ME->ZIF_MATERIAL_DESTINACAO~SET_VALIDAR( ).

        SELECT SINGLE * INTO @DATA(WA_RBKP)
          FROM RBKP
         WHERE BELNR EQ @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BELNR_DEV
           AND GJAHR EQ @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-GJAHR_DEV.

        IF WA_RBKP-STBLG IS NOT INITIAL.
          ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BELNR_ESTORNO = WA_RBKP-STBLG.
          ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-GJAHR_ESTORNO = WA_RBKP-STJAH.
          R_GEROU = ABAP_TRUE.
        ELSE.
          MIRO->ESTORNAR(
            EXPORTING
              I_BAPI_WAIT                = ABAP_TRUE
            IMPORTING
              E_INVOICEDOCNUMBER_ESTORNO = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BELNR_ESTORNO
              E_FISCALYEAR_ESTORNO       = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-GJAHR_ESTORNO
              E_RETORNO                  = E_RETORNO
            CHANGING
              I_INVOICEDOCNUMBER         = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BELNR_DEV
              I_FISCALYEAR               = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-GJAHR_DEV
              I_POSTINGDATE              = SY-DATUM
            RECEIVING
              R_GEROU                    = R_GEROU
          ).
        ENDIF.

        IF R_GEROU EQ ABAP_FALSE.

          READ TABLE E_RETORNO INTO DATA(WA_RETORNO) WITH KEY TYPE = 'E'.

          IF SY-SUBRC IS INITIAL.
            RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
              EXPORTING
                TEXTID = VALUE #( MSGID = WA_RETORNO-ID
                                  MSGNO = WA_RETORNO-NUMBER
                                  ATTR1 = WA_RETORNO-MESSAGE_V1
                                  ATTR2 = WA_RETORNO-MESSAGE_V2
                                  ATTR3 = WA_RETORNO-MESSAGE_V3
                                  ATTR4 = WA_RETORNO-MESSAGE_V4 )
                MSGID  = WA_RETORNO-ID
                MSGNO  = WA_RETORNO-NUMBER
                MSGTY  = 'E'
                MSGV1  = WA_RETORNO-MESSAGE_V1
                MSGV2  = WA_RETORNO-MESSAGE_V2
                MSGV3  = WA_RETORNO-MESSAGE_V3
                MSGV4  = WA_RETORNO-MESSAGE_V4.
          ENDIF.

          READ TABLE E_RETORNO INTO WA_RETORNO WITH KEY TYPE = 'A'.

          IF SY-SUBRC IS INITIAL.
            RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
              EXPORTING
                TEXTID = VALUE #( MSGID = WA_RETORNO-ID
                                  MSGNO = WA_RETORNO-NUMBER
                                  ATTR1 = WA_RETORNO-MESSAGE_V1
                                  ATTR2 = WA_RETORNO-MESSAGE_V2
                                  ATTR3 = WA_RETORNO-MESSAGE_V3
                                  ATTR4 = WA_RETORNO-MESSAGE_V4 )
                MSGID  = WA_RETORNO-ID
                MSGNO  = WA_RETORNO-NUMBER
                MSGTY  = 'E'
                MSGV1  = WA_RETORNO-MESSAGE_V1
                MSGV2  = WA_RETORNO-MESSAGE_V2
                MSGV3  = WA_RETORNO-MESSAGE_V3
                MSGV4  = WA_RETORNO-MESSAGE_V4.
          ENDIF.

        ENDIF.

        ME->ZIF_MATERIAL_DESTINACAO~SET_GRAVAR( ).

        IF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BELNR_ESTORNO IS NOT INITIAL.

          CASE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_ORIGEM.
            WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_ORIGEM_NFE.

              CASE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_DESTINACAO.

                WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_DEVOLUCAO.

                  READ TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115 INDEX 1 INTO DATA(WA_0115).

                  UPDATE ZIB_NFE_DIST_TER
                     SET DOCNUM_DEV = SPACE
                         BELNR_DEV  = SPACE
                         GJAHR_DEV  = SPACE
                   WHERE CHAVE_NFE EQ WA_0115-ORIG_NFE.

                  COMMIT WORK AND WAIT.

                WHEN OTHERS.

              ENDCASE.

            WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_ORIGEM_ROMANEIO.
          ENDCASE.

        ENDIF.

      CATCH ZCX_MIRO_EXCEPTION INTO DATA(EX_MIRO).

        RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = EX_MIRO->MSGID
                              MSGNO = EX_MIRO->MSGNO
                              ATTR1 = EX_MIRO->MSGV1
                              ATTR2 = EX_MIRO->MSGV2
                              ATTR3 = EX_MIRO->MSGV3
                              ATTR4 = EX_MIRO->MSGV4 )
            MSGID  = EX_MIRO->MSGID
            MSGNO  = EX_MIRO->MSGNO
            MSGTY  = 'E'
            MSGV1  = EX_MIRO->MSGV1
            MSGV2  = EX_MIRO->MSGV2
            MSGV3  = EX_MIRO->MSGV3
            MSGV4  = EX_MIRO->MSGV4.

    ENDTRY.

  ENDMETHOD.


  METHOD zif_material_destinacao~set_gerar_movimento.

    DATA: handle TYPE REF TO zcl_memory_nfe_inbound,
          root   TYPE REF TO zcl_memory_nfe_inbound_handle,
          oref   TYPE REF TO zcl_memory_nfe_inbound_handle.

    DATA: i_cabecalho    TYPE zde_migo_cabecalho,
          wa_items       TYPE zde_migo_itens,
          i_itens        TYPE zde_migo_itens_t,
          p_data_val     TYPE datum,
          vl_lifnr       TYPE lfa1-lifnr,
          wa_lfa1        TYPE lfa1,
          e_status(1),
          e_messa(64),
          lc_nfobjn      TYPE j_1binterf-nfobjn,
          obj_header     TYPE j_1bnfdoc,
          obj_partner    TYPE TABLE OF j_1bnfnad,
          obj_item       TYPE TABLE OF j_1bnflin,
          obj_item_tax   TYPE TABLE OF j_1bnfstx,
          obj_header_msg TYPE TABLE OF j_1bnfftx,
          obj_refer_msg  TYPE TABLE OF j_1bnfref.

    CLEAR: e_gerou, e_retorno.

    r_if_material_destinacao = me.

    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        p_data_ent     = i_dt_movimento
        p_bukrs        = me->zif_material_destinacao~at_zmmt0114-bukrs
        p_val_fi       = 'X'
        p_val_mm       = 'X'
      IMPORTING
        p_data_val     = p_data_val
      EXCEPTIONS
        data_fi_mm_nao = 1
        OTHERS         = 2.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_material_destinacao
        EXPORTING
          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = sy-msgty
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        i_bukrs  = me->zif_material_destinacao~at_zmmt0114-bukrs
        i_data   = p_data_val
      IMPORTING
        e_status = e_status
        e_messa  = e_messa
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_material_destinacao
        EXPORTING
          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = sy-msgty
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

    IF e_status = 'E'.
      MESSAGE s000(z01) WITH e_messa.
      RAISE EXCEPTION TYPE zcx_material_destinacao
        EXPORTING
          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = 'E'
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

    READ TABLE me->zif_material_destinacao~at_zmmt0115 INDEX 1 INTO DATA(wa_0115).

    DATA(lc_doc_data) = sy-datum.

    CASE me->zif_material_destinacao~at_zmmt0114-tp_origem.
      WHEN zif_material_destinacao=>st_tp_origem_nfe.

        SELECT SINGLE * INTO @DATA(wa_ib_nfe)
          FROM zib_nfe_dist_ter
         WHERE chave_nfe EQ @wa_0115-orig_nfe.

        i_cabecalho-descricao  = 'NFe Inbound'.
        i_cabecalho-zchave_nfe = wa_0115-orig_nfe.
        "lc_doc_data = wa_ib_nfe-dt_emissao.

      WHEN zif_material_destinacao=>st_tp_origem_romaneio.

        CASE me->zif_material_destinacao~at_zmmt0114-tp_destinacao.
          WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.

            SELECT SINGLE * INTO @DATA(wa_romaneio)
              FROM zsdt0001
             WHERE ch_referencia EQ @wa_0115-orig_romaneio.

            lc_doc_data = wa_romaneio-docdat.
            i_cabecalho-zch_referencia = wa_0115-orig_romaneio.

          WHEN OTHERS.
        ENDCASE.

        i_cabecalho-zchave_nfe = wa_0115-orig_romaneio.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_material_destinacao
          EXPORTING
            textid = VALUE #( msgid = zcx_material_destinacao=>zcx_origem_valida-msgid
                              msgno = zcx_material_destinacao=>zcx_origem_valida-msgno )
            msgid  = zcx_material_destinacao=>zcx_origem_valida-msgid
            msgno  = zcx_material_destinacao=>zcx_origem_valida-msgno
            msgty  = 'E'.
    ENDCASE.

    "Verificar Saldo """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    i_cabecalho-data_documento  = lc_doc_data.
    i_cabecalho-data_lancamento = p_data_val.
    i_cabecalho-ver_gr_gi_slip  = '2'.
    i_cabecalho-ver_gr_gi_slip  = '2'.
    i_cabecalho-zid_destinacao  = me->zif_material_destinacao~at_zmmt0114-id_destinacao.


    CASE me->zif_material_destinacao~at_zmmt0114-tp_destinacao.
      WHEN zif_material_destinacao=>st_tp_destinacao_retorno.

        i_cabecalho-goodsmvt_code   = '04'.

        SELECT SINGLE * INTO @DATA(wa_t156)
          FROM t156
         WHERE bwart EQ @me->zif_material_destinacao~at_zmmt0114-bwart.

        IF sy-subrc IS INITIAL AND wa_t156-j_1bnftype IS NOT INITIAL.
          SELECT SINGLE * INTO @DATA(wa_j_1baa)
            FROM j_1baa
           WHERE nftype EQ @wa_t156-j_1bnftype.

          IF sy-subrc IS INITIAL AND wa_j_1baa-form IS INITIAL.

            DATA: wa_cabecalho TYPE zde_miro_cabecalho.

            CASE me->zif_material_destinacao~at_zmmt0114-tp_origem.
              WHEN zif_material_destinacao=>st_tp_origem_nfe.
                wa_cabecalho-lifnr      = wa_ib_nfe-p_emissor.
                wa_cabecalho-series     = wa_ib_nfe-serie.
                wa_cabecalho-nf_number9 = wa_ib_nfe-numero.
              WHEN OTHERS.
            ENDCASE.

            i_cabecalho-doc_referencia = zcl_miro=>get_chave_referencia(
              i_nf_number  = wa_cabecalho-nf_number
              i_series     = wa_cabecalho-series
              i_subseries  = wa_cabecalho-subseries
              i_nf_number9 = wa_cabecalho-nf_number9 ).

            DATA(lc_referencia) = wa_cabecalho-lifnr && i_cabecalho-doc_referencia.

            IF lc_referencia IS NOT INITIAL.
              TRY.
                  handle = zcl_memory_nfe_inbound=>attach_for_write( inst_name = CONV #( lc_referencia ) ).
                  CREATE OBJECT root AREA HANDLE handle.
                  root->at_nfe_inbound = wa_ib_nfe.
                  handle->set_root( root ).
                  handle->detach_commit( ).
                CATCH cx_shm_exclusive_lock_active.  "
                CATCH cx_shm_version_limit_exceeded.  "
                CATCH cx_shm_change_lock_active.  "
                CATCH cx_shm_parameter_error.  "
                CATCH cx_shm_pending_lock_removed.  "
                CATCH cx_shm_attach_error.
              ENDTRY.
            ENDIF.

          ENDIF.
        ENDIF.

      WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.
        i_cabecalho-goodsmvt_code   = '04'.
      WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.
        i_cabecalho-goodsmvt_code   = '01'.

        SELECT SINGLE * INTO @DATA(wa_rbkp)
          FROM rbkp
         WHERE belnr EQ @me->zif_material_destinacao~at_zmmt0114-belnr_dev
           AND gjahr EQ @me->zif_material_destinacao~at_zmmt0114-gjahr_dev.

        i_cabecalho-doc_referencia = wa_rbkp-xblnr.

    ENDCASE.

    i_cabecalho-valor_total =
     REDUCE j_1bbase( INIT i TYPE j_1bbase
      FOR ls IN me->zif_material_destinacao~at_zmmt0116
        NEXT i = i + ls-valor ).

    IF me->zif_material_destinacao~at_zmmt0116[] IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(it_mseg)
        FROM mseg
         FOR ALL ENTRIES IN @me->zif_material_destinacao~at_zmmt0116
       WHERE mblnr EQ @me->zif_material_destinacao~at_zmmt0116-orig_mblnr
         AND mjahr EQ @me->zif_material_destinacao~at_zmmt0116-orig_mjahr
         AND zeile EQ @me->zif_material_destinacao~at_zmmt0116-orig_zeile.
    ENDIF.

    SORT it_mseg BY mblnr mjahr zeile.

    IF me->zif_material_destinacao~at_zmmt0118[] IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(it_zsdt0001)
        FROM zsdt0001
         FOR ALL ENTRIES IN @me->zif_material_destinacao~at_zmmt0118
       WHERE ch_referencia EQ @me->zif_material_destinacao~at_zmmt0118-ch_referencia.
    ENDIF.

    SORT it_zsdt0001 BY ch_referencia.

    LOOP AT me->zif_material_destinacao~at_zmmt0116 INTO DATA(wa_0116).
      READ TABLE it_mseg WITH KEY mblnr = wa_0116-orig_mblnr mjahr = wa_0116-orig_mjahr zeile = wa_0116-orig_zeile
      INTO DATA(wa_mseg).
      CLEAR: wa_items.
      wa_items-tipo_movimento  = me->zif_material_destinacao~at_zmmt0114-bwart.
      wa_items-tax_code        = 'ZE'.
      wa_items-material        = wa_mseg-matnr.
      wa_items-deposito        = wa_mseg-lgort.
      wa_items-lote            = wa_mseg-charg.
      wa_items-peso            = wa_0116-menge.
      wa_items-unidade         = wa_0116-meins.

      IF me->zif_material_destinacao~at_zmmt0114-tp_destinacao NE zif_material_destinacao=>st_tp_destinacao_armazenar.

        wa_items-po_number  = wa_0116-ebeln.
        wa_items-po_item    = wa_0116-ebelp.
        IF wa_0116-ebeln IS NOT INITIAL.
          SELECT SINGLE * INTO @DATA(wa_ekko) FROM ekko WHERE ebeln EQ @wa_0116-ebeln.
          IF sy-subrc IS INITIAL.
            wa_items-fornecedor = wa_ekko-lifnr.
          ELSE.
            wa_items-fornecedor = wa_0116-forne.
          ENDIF.
        ELSE.
          wa_items-fornecedor = wa_0116-forne.
        ENDIF.

      ELSE.
        wa_items-fornecedor = wa_0116-forne.
      ENDIF.

      SELECT SINGLE *
        INTO wa_lfa1
        FROM lfa1
        WHERE lifnr = wa_items-fornecedor.

***********IR160111************************************************
      CASE me->zif_material_destinacao~at_zmmt0114-tp_destinacao.
        WHEN zif_material_destinacao=>st_tp_destinacao_retorno.

          SELECT SINGLE t~descript
            FROM setleaf AS s
           INNER JOIN setlinet AS t ON t~setname EQ s~setname AND t~lineid EQ s~lineid
            INTO @DATA(vl_descript2)
           WHERE s~setname = 'MAGGI_ZMM0019_IVA_ENTRAD'
             AND s~valfrom = @wa_lfa1-regio.
          IF sy-subrc = 0.
            wa_items-tax_code        = vl_descript2.
          ENDIF.

        WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.

          SELECT SINGLE t~descript
            FROM setleaf AS s
           INNER JOIN setlinet AS t ON t~setname EQ s~setname AND t~lineid EQ s~lineid
            INTO @vl_descript2
           WHERE s~setname = 'MAGGI_ZMM0019_IVA_SAIDA'
             AND s~valfrom = @wa_lfa1-regio.
          IF sy-subrc = 0.
            wa_items-tax_code        = vl_descript2.
          ENDIF.

        WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.
      ENDCASE.
***********IR160111************************************************

      wa_items-ext_base_amont  = wa_0116-valor.
      wa_items-local_expedicao = wa_mseg-werks.
      wa_items-grund           = me->zif_material_destinacao~at_zmmt0114-grund.
      APPEND wa_items TO i_itens.
    ENDLOOP.

    LOOP AT me->zif_material_destinacao~at_zmmt0118 INTO DATA(wa_0118).

      READ TABLE it_zsdt0001 INTO DATA(wa_zsdt0001) WITH KEY ch_referencia = wa_0118-ch_referencia BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(wa_ekpo) FROM ekpo WHERE ebeln EQ @wa_zsdt0001-vbeln.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(wa_eket) FROM eket WHERE ebeln EQ @wa_ekpo-ebeln AND ebelp EQ @wa_ekpo-ebelp.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ekpo-werks
        IMPORTING
          output = vl_lifnr.

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          p_parceiro   = vl_lifnr
          p_partype    = 'B'
        CHANGING
          wa_info_part = wa_lfa1.

      CASE me->zif_material_destinacao~at_zmmt0114-tp_destinacao.
        WHEN zif_material_destinacao=>st_tp_destinacao_retorno.

          SELECT SINGLE t~descript
            FROM setleaf AS s
           INNER JOIN setlinet AS t ON t~setname EQ s~setname AND t~lineid EQ s~lineid
            INTO @DATA(vl_descript)
           WHERE s~setname = 'MAGGI_ZMM0019_IVA_ENTRAD'
             AND s~valfrom = @wa_lfa1-regio.

        WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.

          SELECT SINGLE t~descript
            FROM setleaf AS s
           INNER JOIN setlinet AS t ON t~setname EQ s~setname AND t~lineid EQ s~lineid
            INTO @vl_descript
           WHERE s~setname = 'MAGGI_ZMM0019_IVA_SAIDA'
             AND s~valfrom = @wa_lfa1-regio.

        WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.
      ENDCASE.

      wa_items-tipo_movimento  = me->zif_material_destinacao~at_zmmt0114-bwart.
      wa_items-tax_code        = vl_descript.
      wa_items-material        = wa_ekpo-matnr.
      wa_items-deposito        = wa_ekpo-lgort.
      wa_items-lote            = wa_eket-charg.
      wa_items-peso            = wa_0118-menge.
      wa_items-unidade         = wa_0118-meins.
      wa_items-ext_base_amont  = wa_0118-valor.
      wa_items-local_expedicao = wa_ekpo-werks.
      wa_items-grund           = me->zif_material_destinacao~at_zmmt0114-grund.

      IF wa_0118-ebeln IS NOT INITIAL.
        SELECT SINGLE * INTO @wa_ekko FROM ekko WHERE ebeln EQ @wa_0118-ebeln.
        IF sy-subrc IS INITIAL.
          wa_items-fornecedor = wa_ekko-lifnr.
        ELSE.
          wa_items-fornecedor = wa_0118-forne.
        ENDIF.
      ELSE.
        wa_items-fornecedor = wa_0118-forne.
      ENDIF.

      APPEND wa_items TO i_itens.

    ENDLOOP.

    "Agrupa Itens.
    DATA(lc_itens) = i_itens[].
    SORT lc_itens BY material local_expedicao deposito lote tipo_movimento unidade ordem fornecedor po_number po_item suppl_vend deliv_numb deliv_item expirydate prod_date tax_code grund.

    DELETE ADJACENT DUPLICATES FROM lc_itens
    COMPARING material local_expedicao deposito lote tipo_movimento unidade ordem fornecedor po_number po_item suppl_vend deliv_numb deliv_item expirydate prod_date tax_code grund.

    LOOP AT lc_itens ASSIGNING FIELD-SYMBOL(<fs_itens>).
      <fs_itens>-peso = 0.
      <fs_itens>-ext_base_amont = 0.
      LOOP AT i_itens INTO wa_items
        WHERE material EQ <fs_itens>-material
          AND local_expedicao EQ <fs_itens>-local_expedicao
          AND deposito EQ <fs_itens>-deposito
          AND lote EQ <fs_itens>-lote
          AND tipo_movimento EQ <fs_itens>-tipo_movimento
          AND unidade EQ <fs_itens>-unidade
          AND ordem EQ <fs_itens>-ordem
          AND fornecedor EQ <fs_itens>-fornecedor
          AND po_number EQ <fs_itens>-po_number
          AND po_item EQ <fs_itens>-po_item
          AND suppl_vend EQ <fs_itens>-suppl_vend
          AND deliv_numb EQ <fs_itens>-deliv_numb
          AND deliv_item EQ <fs_itens>-deliv_item
          AND expirydate EQ <fs_itens>-expirydate
          AND prod_date EQ <fs_itens>-prod_date
          AND tax_code EQ <fs_itens>-tax_code
          AND grund EQ <fs_itens>-grund.
        ADD wa_items-peso TO <fs_itens>-peso.
        ADD wa_items-ext_base_amont TO <fs_itens>-ext_base_amont.
      ENDLOOP.
    ENDLOOP.

    i_itens[] = lc_itens[].

    DATA: lc_migo TYPE REF TO zcl_migo.

    me->zif_material_destinacao~set_validar(
       )->set_gerar_nota_devolucao(
       ).

    CREATE OBJECT lc_migo.

    TRY .

        lc_migo->criar(
          EXPORTING
            i_cabecalho = i_cabecalho
            i_itens     = i_itens
            i_bapi_wait = abap_true
          IMPORTING
            e_retorno   = e_retorno
            e_j_1bnfdoc = DATA(e_j_1bnfdoc)
            mat_doc     = DATA(mat_doc)
            doc_year    = DATA(doc_year)
          RECEIVING
            r_gerou     = DATA(r_gerou_migo)
        ).

        IF lc_referencia IS NOT INITIAL.
          TRY.
              handle = zcl_memory_nfe_inbound=>attach_for_read( inst_name = CONV #( lc_referencia ) ).
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
        ENDIF.

      CATCH zcx_migo_exception INTO DATA(ex_migo).

        IF lc_referencia IS NOT INITIAL.
          TRY.
              handle = zcl_memory_nfe_inbound=>attach_for_read( inst_name = CONV #( lc_referencia ) ).
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
        ENDIF.

        RAISE EXCEPTION TYPE zcx_material_destinacao
          EXPORTING
            textid = VALUE #( msgid = ex_migo->msgid
                              msgno = ex_migo->msgno
                              attr1 = CONV #( ex_migo->msgv1 )
                              attr2 = CONV #( ex_migo->msgv2 )
                              attr3 = CONV #( ex_migo->msgv3 )
                              attr4 = CONV #( ex_migo->msgv4 ) )
            msgid  = ex_migo->msgid
            msgno  = ex_migo->msgno
            msgty  = ex_migo->msgty
            msgv1  = ex_migo->msgv1
            msgv2  = ex_migo->msgv2
            msgv3  = ex_migo->msgv3
            msgv4  = ex_migo->msgv4.
      CATCH zcx_pedido_compra_exception INTO DATA(ex_compra).

        IF lc_referencia IS NOT INITIAL.
          TRY.
              handle = zcl_memory_nfe_inbound=>attach_for_read( inst_name = CONV #( lc_referencia ) ).
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
        ENDIF.

        RAISE EXCEPTION TYPE zcx_material_destinacao
          EXPORTING
            textid = VALUE #( msgid = ex_compra->msgid
                              msgno = ex_compra->msgno
                              attr1 = CONV #( ex_compra->msgv1 )
                              attr2 = CONV #( ex_compra->msgv2 )
                              attr3 = CONV #( ex_compra->msgv3 )
                              attr4 = CONV #( ex_compra->msgv4 ) )
            msgid  = ex_compra->msgid
            msgno  = ex_compra->msgno
            msgty  = ex_compra->msgty
            msgv1  = ex_compra->msgv1
            msgv2  = ex_compra->msgv2
            msgv3  = ex_compra->msgv3
            msgv4  = ex_compra->msgv4.
    ENDTRY.

    IF r_gerou_migo EQ abap_true.
      me->zif_material_destinacao~at_zmmt0114-mblnr  = mat_doc.
      me->zif_material_destinacao~at_zmmt0114-mjahr  = doc_year.
      me->zif_material_destinacao~at_zmmt0114-docnum = e_j_1bnfdoc-docnum.
      me->zif_material_destinacao~set_gravar( ).
      e_mblnr  = mat_doc.
      e_mjahr  = doc_year.
      e_docnum = e_j_1bnfdoc-docnum.
      e_gerou  = abap_true.

      CASE me->zif_material_destinacao~at_zmmt0114-tp_origem.
        WHEN zif_material_destinacao=>st_tp_origem_nfe.

          CASE me->zif_material_destinacao~at_zmmt0114-tp_destinacao.
            WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.

              UPDATE zib_nfe_dist_ter
                 SET mblnr_arm  = e_mblnr
                     mjahr_arm  = e_mjahr
                     docnum_arm = e_docnum
               WHERE chave_nfe EQ wa_0115-orig_nfe.

              COMMIT WORK AND WAIT.

*-CS2025000249-12.05.2025-#174358-JT-inicio
*--------------------------------------------
*------------ ajustar CFOP e Leis Fiscais NF
*--------------------------------------------
              me->zif_material_destinacao~set_ajustar_cfop_nf( i_tp_destinacao = me->zif_material_destinacao~at_zmmt0114-tp_destinacao
                                                               i_chave_nfe     = wa_0115-orig_nfe ).
              COMMIT WORK AND WAIT.
*-CS2025000249-12.05.2025-#174358-JT-fim

              IF wa_ib_nfe-ck_trans_nf_propri EQ abap_true AND e_docnum IS NOT INITIAL.

                SELECT SINGLE * INTO @DATA(wa_zib_nfe_dist_frt)
                  FROM zib_nfe_dist_frt
                 WHERE chave_nfe EQ @wa_ib_nfe-chave_nfe.

                IF sy-subrc IS INITIAL AND wa_zib_nfe_dist_frt-id_agent_frete IS NOT INITIAL.

                  CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
                    EXPORTING
                      doc_number         = e_docnum
                    IMPORTING
                      obj_number         = lc_nfobjn
                    EXCEPTIONS
                      document_not_found = 1
                      docum_lock         = 2
                      OTHERS             = 3.

                  CALL FUNCTION 'J_1B_NF_OBJECT_READ'
                    EXPORTING
                      obj_number       = lc_nfobjn
                    IMPORTING
                      obj_header       = obj_header
                    TABLES
                      obj_partner      = obj_partner
                      obj_item         = obj_item
                      obj_item_tax     = obj_item_tax
                      obj_header_msg   = obj_header_msg
                      obj_refer_msg    = obj_refer_msg
                    EXCEPTIONS
                      object_not_found = 1
                      OTHERS           = 2.

                  IF sy-subrc IS INITIAL.

                    READ TABLE obj_partner INTO DATA(wa_patner) WITH KEY parvw = 'SP'.

                    IF sy-subrc IS NOT INITIAL.

                      wa_patner-docnum = obj_header-docnum.
                      wa_patner-parvw  = 'SP'.
                      wa_patner-partyp = 'V'.
                      wa_patner-parid  = wa_zib_nfe_dist_frt-id_agent_frete.
                      APPEND wa_patner TO obj_partner.

                      CALL FUNCTION 'J_1B_NF_OBJECT_UPDATE'
                        EXPORTING
                          obj_number       = lc_nfobjn
                          obj_header       = obj_header
                        TABLES
                          obj_partner      = obj_partner
                          obj_item         = obj_item
                          obj_item_tax     = obj_item_tax
                        EXCEPTIONS
                          object_not_found = 1
                          OTHERS           = 2.

                      IF sy-subrc IS INITIAL.
                        CALL FUNCTION 'J_1B_NF_DOC_UPDATE_FROM_OBJECT'
                          EXPORTING
                            obj_number         = lc_nfobjn
                          EXCEPTIONS
                            object_not_found   = 1
                            document_not_found = 2
                            update_problem     = 3
                            docum_lock         = 4
                            OTHERS             = 5.

                        CALL FUNCTION 'J_1B_NF_OBJECT_DROP'
                          EXPORTING
                            obj_number       = lc_nfobjn
                          EXCEPTIONS
                            object_not_found = 1
                            OTHERS           = 2.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                ENDIF.
              ENDIF.

            WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.

              UPDATE zib_nfe_dist_ter
                 SET mblnr_dev  = e_mblnr
                     mjahr_dev  = e_mjahr
               WHERE chave_nfe EQ wa_0115-orig_nfe.

              COMMIT WORK AND WAIT.

            WHEN OTHERS.
          ENDCASE.

        WHEN zif_material_destinacao=>st_tp_origem_romaneio.
      ENDCASE.

    ELSE.
      READ TABLE e_retorno WITH KEY type = 'E' INTO DATA(wa_retorno).
      RAISE EXCEPTION TYPE zcx_material_destinacao
        EXPORTING
          textid = VALUE #( msgid = wa_retorno-id
                            msgno = wa_retorno-number
                            attr1 = CONV #( wa_retorno-message_v1 )
                            attr2 = CONV #( wa_retorno-message_v2 )
                            attr3 = CONV #( wa_retorno-message_v3 )
                            attr4 = CONV #( wa_retorno-message_v4 ) )
          msgid  = wa_retorno-id
          msgno  = wa_retorno-number
          msgty  = 'E'
          msgv1  = wa_retorno-message_v1
          msgv2  = wa_retorno-message_v2
          msgv3  = wa_retorno-message_v3
          msgv4  = wa_retorno-message_v4.
    ENDIF.

    CLEAR: lc_migo.

  ENDMETHOD.


  METHOD zif_material_destinacao~set_gerar_nota_devolucao.

    TYPES: BEGIN OF ty_po_cfop,                             "1493219
             ebeln TYPE ebeln,                              "1493219
             ebelp TYPE ebelp,                              "1493219
             rblgp TYPE rblgp,                              "1502482
             cfop  TYPE j_1bcfop,                           "1493219
           END OF ty_po_cfop.                               "1493219

    TYPES BEGIN OF ty_rseg.
    TYPES : ebeln  TYPE rseg-ebeln.
    TYPES : ebelp  TYPE rseg-ebelp.
    TYPES : wrbtr  TYPE rseg-wrbtr.
    TYPES : menge  TYPE rseg-menge.
    TYPES : matnr  TYPE rseg-matnr.
    TYPES : lfbnr  TYPE rseg-lfbnr.
    TYPES : lfgja  TYPE rseg-lfgja.
    TYPES : lfpos  TYPE rseg-lfpos.
    TYPES END OF ty_rseg.

    TYPES BEGIN OF ty_vinc.
    TYPES : ebeln      TYPE rseg-ebeln.
    TYPES : ebelp      TYPE rseg-ebelp.
    TYPES : menge      TYPE rseg-menge.
    TYPES : matnr      TYPE rseg-matnr.
    TYPES : wrbtr      TYPE rseg-wrbtr.
    TYPES : wmwst1     TYPE rbkp-wmwst1.
    TYPES : lfbnr      TYPE rseg-lfbnr.
    TYPES : lfgja      TYPE rseg-lfgja.
    TYPES : lfpos      TYPE rseg-lfpos.
    TYPES : lfbnr_ori  TYPE rseg-lfbnr.  "*-CS2025000249-27.05.2025-#175255-JT
    TYPES : lfgja_ori  TYPE rseg-lfgja.  "*-CS2025000249-27.05.2025-#175255-JT
    TYPES : lfpos_ori  TYPE rseg-lfpos.  "*-CS2025000249-27.05.2025-#175255-JT
    TYPES END OF ty_vinc.

    DATA: lc_total_item     TYPE rseg-wrbtr,
          lc_total_impostos TYPE rbkp-wmwst1,
          lc_refkey         TYPE j_1brefkey.

    DATA: it_ty_rseg TYPE TABLE OF ty_rseg.
    DATA: it_ty_vinc TYPE TABLE OF ty_vinc.
    DATA: it_ty_vinc_aux TYPE TABLE OF ty_vinc. "*-CS2025000249-27.05.2025-#175255-JT
    DATA: gt_cfop TYPE TABLE OF ty_po_cfop.
    DATA: lc_headerdata TYPE bapi_incinv_create_header,
          lc_itemdata	  TYPE bapi_incinv_create_item,
          it_itemdata   TYPE TABLE OF bapi_incinv_create_item,
          it_bapiret2   TYPE TABLE OF bapiret2,
          lc_rblgp      TYPE rblgp.

    CLEAR: e_belnr_dev,
           e_gjahr_dev,
           e_docnum_dev.

    if_material_destinacao = me.

    e_docnum_dev = me->zif_material_destinacao~at_zmmt0114-docnum_dev.
    e_belnr_dev  = me->zif_material_destinacao~at_zmmt0114-belnr_dev.
    e_gjahr_dev  = me->zif_material_destinacao~at_zmmt0114-gjahr_dev.

    CHECK me->zif_material_destinacao~at_tp_destinacao EQ zif_material_destinacao=>st_tp_destinacao_devolucao.

    CHECK me->zif_material_destinacao~at_zmmt0114-belnr_dev IS INITIAL.


    READ TABLE me->zif_material_destinacao~at_zmmt0115 INTO DATA(wa_zmmt0115) INDEX 1.

    DATA(lc_tipo) = COND string( WHEN wa_zmmt0115-ck_total_origem EQ abap_true THEN 'TOTAL' ELSE 'PARCIAL' ).

    CASE me->zif_material_destinacao~at_zmmt0114-tp_origem.
      WHEN zif_material_destinacao=>st_tp_origem_nfe.

        TRY .

            DATA: nfe_inbound TYPE REF TO zcl_nfe_inbound.
            CREATE OBJECT nfe_inbound EXPORTING i_sem_bloqueio = abap_true.
            nfe_inbound->zif_cadastro~set_registro( i_id_registro = wa_zmmt0115-orig_nfe ).
            DATA(lc_nfe) = nfe_inbound->get_cabecalho_nota( ).
            DATA(nfe_base) = nfe_inbound->get_info_nota( ).

            nfe_inbound->free( ).
            CLEAR: nfe_inbound.

            "0202
            READ TABLE nfe_base-nfe_base-itens INDEX 1 INTO DATA(wa_item).

          CATCH zcx_cadastro INTO DATA(ex_cadastro).

            RAISE EXCEPTION TYPE zcx_material_destinacao
              EXPORTING
                textid = VALUE #( msgid = ex_cadastro->msgid
                                  msgno = ex_cadastro->msgno
                                  attr1 = ex_cadastro->msgv1
                                  attr2 = ex_cadastro->msgv2
                                  attr3 = ex_cadastro->msgv3
                                  attr4 = ex_cadastro->msgv4 )
                msgid  = ex_cadastro->msgid
                msgno  = ex_cadastro->msgno
                msgty  = 'E'
                msgv1  = ex_cadastro->msgv1
                msgv2  = ex_cadastro->msgv2
                msgv3  = ex_cadastro->msgv3
                msgv4  = ex_cadastro->msgv4.

          CATCH zcx_nfe_inbound_exception INTO DATA(ex_nfe).

            RAISE EXCEPTION TYPE zcx_material_destinacao
              EXPORTING
                textid = VALUE #( msgid = ex_nfe->msgid
                                  msgno = ex_nfe->msgno
                                  attr1 = ex_nfe->msgv1
                                  attr2 = ex_nfe->msgv2
                                  attr3 = ex_nfe->msgv3
                                  attr4 = ex_nfe->msgv4 )
                msgid  = ex_nfe->msgid
                msgno  = ex_nfe->msgno
                msgty  = 'E'
                msgv1  = ex_nfe->msgv1
                msgv2  = ex_nfe->msgv2
                msgv3  = ex_nfe->msgv3
                msgv4  = ex_nfe->msgv4.

        ENDTRY.

        SELECT SINGLE * INTO @DATA(wa_rbkp)
          FROM rbkp
         WHERE belnr EQ @lc_nfe-belnr
           AND gjahr EQ @lc_nfe-gjahr.

        SELECT * INTO TABLE @DATA(it_rseg)
          FROM rseg
         WHERE belnr EQ @lc_nfe-belnr
           AND gjahr EQ @lc_nfe-gjahr.

*-CS2025000249-27.05.2025-#175255-JT-inicio
        SELECT * INTO TABLE @DATA(it_mseg)
          FROM mseg
         WHERE mblnr EQ @lc_nfe-mblnr
           AND mjahr EQ @lc_nfe-mjahr.
*-CS2025000249-27.05.2025-#175255-JT-fim

        lc_headerdata-doc_type       = wa_rbkp-blart.
        lc_headerdata-doc_date       = sy-datum.
        lc_headerdata-pstng_date     = sy-datum.
        lc_headerdata-comp_code      = wa_rbkp-bukrs.
        lc_headerdata-diff_inv       = wa_rbkp-lifnr.
        lc_headerdata-currency       = wa_rbkp-waers.
        lc_headerdata-calc_tax_ind   = abap_true.
        lc_headerdata-header_txt     = wa_rbkp-xblnr.
        lc_headerdata-pmnt_block     = 'O'.
        lc_headerdata-del_costs_taxc = wa_rbkp-mwskz1.
        lc_headerdata-pymt_meth      = 'E'.
        lc_headerdata-housebankid    = wa_rbkp-hbkid.
        lc_headerdata-alloc_nmbr     = wa_rbkp-zuonr.
        lc_headerdata-bus_area       = wa_rbkp-gsber.
        lc_headerdata-item_text      = |DEV. { lc_tipo } NFe { wa_rbkp-xblnr }|.
        lc_headerdata-j_1bnftype     = 'ZR'.

        "Total por Material da Devolução
        LOOP AT me->zif_material_destinacao~at_zmmt0116 INTO DATA(wa_zmmt0116).
          READ TABLE it_ty_vinc ASSIGNING FIELD-SYMBOL(<fs_vinc>)
          WITH KEY matnr = wa_zmmt0116-matnr
                   ebeln = wa_zmmt0116-ebeln
                   ebelp = wa_zmmt0116-ebelp
                   lfbnr = wa_zmmt0116-orig_mblnr
                   lfgja = wa_zmmt0116-orig_mjahr
                   lfpos = wa_zmmt0116-orig_zeile.
          IF sy-subrc IS INITIAL.
            <fs_vinc>-menge  = <fs_vinc>-menge + wa_zmmt0116-menge.
            <fs_vinc>-wrbtr  = 0.
            <fs_vinc>-wmwst1 = 0.
          ELSE.
            APPEND VALUE #(
                menge     = wa_zmmt0116-menge
                matnr     = wa_zmmt0116-matnr
                ebeln     = wa_zmmt0116-ebeln
                ebelp     = wa_zmmt0116-ebelp
                lfbnr     = wa_zmmt0116-orig_mblnr
                lfgja     = wa_zmmt0116-orig_mjahr
                lfpos     = wa_zmmt0116-orig_zeile
                lfbnr_ori = wa_zmmt0116-orig_mblnr  "*-CS2025000249-27.05.2025-#175255-JT
                lfgja_ori = wa_zmmt0116-orig_mjahr  "*-CS2025000249-27.05.2025-#175255-JT
                lfpos_ori = wa_zmmt0116-orig_zeile  "*-CS2025000249-27.05.2025-#175255-JT
                wrbtr     = 0 wmwst1 = 0  ) TO it_ty_vinc.
          ENDIF.
        ENDLOOP.

        lc_total_item = 0.
        lc_total_impostos = wa_rbkp-wmwst1.

        "Totalizar MIRO:
        LOOP AT it_rseg INTO DATA(wa_rseg).
          "Valor Total dos Itens
          ADD wa_rseg-wrbtr TO lc_total_item.
          "Valor Total dos Itens por Material
          READ TABLE it_ty_rseg ASSIGNING FIELD-SYMBOL(<fs_rseg>)
            WITH KEY matnr = wa_rseg-matnr
                     ebeln = wa_rseg-ebeln
                     ebelp = wa_rseg-ebelp
                     lfbnr = wa_rseg-lfbnr
                     lfgja = wa_rseg-lfgja
                     lfpos = wa_rseg-lfpos.
          IF sy-subrc IS INITIAL.
            <fs_rseg>-menge  = <fs_rseg>-menge + wa_rseg-menge.
            <fs_rseg>-wrbtr  = <fs_rseg>-wrbtr + wa_rseg-wrbtr.
          ELSE.
            APPEND VALUE #(
              wrbtr = wa_rseg-wrbtr
              menge = wa_rseg-menge
              matnr = wa_rseg-matnr
              ebeln = wa_rseg-ebeln
              ebelp = wa_rseg-ebelp
              lfbnr = wa_rseg-lfbnr
              lfgja = wa_rseg-lfgja
              lfpos = wa_rseg-lfpos
               ) TO it_ty_rseg.
          ENDIF.
        ENDLOOP.

        LOOP AT it_ty_vinc ASSIGNING <fs_vinc>.

          DATA(ck_achou_referencia) = abap_false.

          LOOP AT it_ty_rseg INTO DATA(wa_ty_rseg)
              WHERE matnr = <fs_vinc>-matnr
                AND ebeln = <fs_vinc>-ebeln
                AND ebelp = <fs_vinc>-ebelp
                AND lfbnr = <fs_vinc>-lfbnr
                AND lfgja = <fs_vinc>-lfgja
                AND lfpos = <fs_vinc>-lfpos.
            <fs_vinc>-wrbtr  = <fs_vinc>-menge * ( wa_ty_rseg-wrbtr / wa_ty_rseg-menge ).
            <fs_vinc>-wmwst1 = <fs_vinc>-wrbtr * ( lc_total_impostos / lc_total_item ).
            "Achou MIRO com Referência
            ck_achou_referencia = abap_true.
          ENDLOOP.

          "Não Achou MIRO com Referência, soma sem referência
          IF ck_achou_referencia EQ abap_false.
            CLEAR: <fs_vinc>-lfbnr, <fs_vinc>-lfgja, <fs_vinc>-lfpos.
            LOOP AT it_ty_rseg INTO wa_ty_rseg
                WHERE matnr = <fs_vinc>-matnr
                  AND ebeln = <fs_vinc>-ebeln
                  AND ebelp = <fs_vinc>-ebelp.
              <fs_vinc>-wrbtr  = <fs_vinc>-menge * ( wa_ty_rseg-wrbtr / wa_ty_rseg-menge ).
              <fs_vinc>-wmwst1 = <fs_vinc>-wrbtr * ( lc_total_impostos / lc_total_item ).
            ENDLOOP.
          ENDIF.
        ENDLOOP.

*-CS2025000249-27.05.2025-#175255-JT-inicio
*-------Ajusta documentos referncias
        LOOP AT it_ty_vinc    INTO DATA(_ty_vinc).
          DATA(l_tabix) = sy-tabix.
          READ TABLE it_mseg  INTO DATA(_mseg) WITH KEY mblnr = _ty_vinc-lfbnr_ori
                                                        mjahr = _ty_vinc-lfgja_ori
                                                        zeile = _ty_vinc-lfpos_ori.
          IF sy-subrc = 0.
            _ty_vinc-lfbnr       = _mseg-mblnr.
            _ty_vinc-lfgja       = _mseg-mjahr.
            _ty_vinc-lfpos       = _mseg-zeile.
            MODIFY it_ty_vinc FROM _ty_vinc INDEX l_tabix.
          ENDIF.
        ENDLOOP.
*-CS2025000249-27.05.2025-#175255-JT-fim

        SORT it_ty_vinc BY matnr ebeln ebelp lfbnr lfgja lfpos.

*-CS2025000249-27.05.2025-#175255-JT-inicio
        LOOP AT it_ty_vinc          INTO DATA(w_ty_vinc).
          READ TABLE it_ty_vinc_aux INTO DATA(w_ty_vinc_aux) WITH KEY matnr = w_ty_vinc-matnr
                                                                      ebeln = w_ty_vinc-ebeln
                                                                      ebelp = w_ty_vinc-ebelp
                                                                      lfbnr = w_ty_vinc-lfbnr
                                                                      lfgja = w_ty_vinc-lfgja
                                                                      lfpos = w_ty_vinc-lfpos.
          IF sy-subrc = 0.
            w_ty_vinc_aux-menge      = w_ty_vinc_aux-menge + w_ty_vinc-menge.
            w_ty_vinc_aux-wrbtr      = w_ty_vinc_aux-wrbtr + w_ty_vinc-wrbtr.
            MODIFY it_ty_vinc_aux FROM w_ty_vinc_aux INDEX sy-tabix.
          ELSE.
            APPEND w_ty_vinc        TO it_ty_vinc_aux.
          ENDIF.
        ENDLOOP.

        it_ty_vinc[]                 = it_ty_vinc_aux[].
*       DELETE ADJACENT DUPLICATES FROM it_ty_vinc COMPARING matnr ebeln ebelp lfbnr lfgja lfpos.
*-CS2025000249-27.05.2025-#175255-JT-fim

        CASE lc_tipo.
          WHEN 'TOTAL'.

            lc_headerdata-gross_amount   = wa_rbkp-rmwwr.

          WHEN OTHERS.

            "Total dos Itens mais Impostos
            lc_headerdata-gross_amount = 0.
            LOOP AT it_ty_vinc INTO DATA(wa_ty_vinc).
              ADD wa_ty_vinc-wrbtr TO lc_headerdata-gross_amount.
              ADD wa_ty_vinc-wmwst1 TO lc_headerdata-gross_amount.
            ENDLOOP.

        ENDCASE.

        "Gerar Itens
        LOOP AT it_ty_vinc INTO wa_ty_vinc.

          lc_rblgp = sy-tabix.

*-CS2025000249-27.05.2025-#175255-JT-inicio
          SELECT SINGLE * INTO @DATA(_ekpo)
            FROM ekpo
           WHERE ebeln EQ @wa_ty_vinc-ebeln
             AND ebelp EQ @wa_ty_vinc-ebelp.

          IF sy-subrc = 0 AND _ekpo-webre = abap_false.
            CLEAR: wa_ty_vinc-lfbnr, wa_ty_vinc-lfgja, wa_ty_vinc-lfpos.
          ENDIF.
*-CS2025000249-27.05.2025-#175255-JT-fim

          CLEAR: lc_itemdata, wa_rseg.
          lc_itemdata-invoice_doc_item = sy-tabix.
          READ TABLE it_rseg
          WITH KEY ebeln = wa_ty_vinc-ebeln
                   ebelp = wa_ty_vinc-ebelp
                   matnr = wa_ty_vinc-matnr
                   lfbnr = wa_ty_vinc-lfbnr
                   lfgja = wa_ty_vinc-lfgja
*                  lfpos = wa_ty_vinc-lfpos  "*-CS2025000249-27.05.2025-#175255-JT
                 INTO wa_rseg.

*-CS2025000249-27.05.2025-#175255-JT-inicio
          IF sy-subrc <> 0.
            READ TABLE it_rseg
            WITH KEY ebeln = wa_ty_vinc-ebeln
                     ebelp = wa_ty_vinc-ebelp
                     matnr = wa_ty_vinc-matnr
*                    lfbnr = wa_ty_vinc-lfbnr
*                    lfgja = wa_ty_vinc-lfgja
*                    lfpos = wa_ty_vinc-lfpos  "*-CS2025000249-27.05.2025-#175255-JT
                     INTO wa_rseg.
          ENDIF.

          IF sy-subrc <> 0.
            READ TABLE it_mseg  INTO _mseg WITH KEY mblnr = wa_ty_vinc-lfbnr_ori
                                                    mjahr = wa_ty_vinc-lfgja_ori
                                                    zeile = wa_ty_vinc-lfpos_ori.
            IF sy-subrc = 0.
              wa_rseg-mwskz = _mseg-mwskz.
              wa_rseg-txjcd = _mseg-txjcd.
              wa_rseg-bstme = _mseg-bstme.
            ENDIF.
          ENDIF.
*-CS2025000249-27.05.2025-#175255-JT-fim

          lc_itemdata-po_number        = wa_ty_vinc-ebeln.
          lc_itemdata-po_item          = wa_ty_vinc-ebelp.
          lc_itemdata-tax_code         = wa_rseg-mwskz.
          lc_itemdata-taxjurcode       = wa_rseg-txjcd.
          lc_itemdata-quantity         = wa_ty_vinc-menge.
          lc_itemdata-po_unit          = wa_rseg-bstme.
          lc_itemdata-item_amount      = wa_ty_vinc-wrbtr.
          lc_itemdata-ref_doc          = wa_ty_vinc-lfbnr.
          lc_itemdata-ref_doc_year     = wa_ty_vinc-lfgja.
          lc_itemdata-ref_doc_it       = wa_ty_vinc-lfpos.
          APPEND lc_itemdata TO it_itemdata.

          CLEAR: gt_cfop[].

          SELECT SINGLE * INTO @DATA(j_1bbranch)
            FROM j_1bbranch
           WHERE bukrs  EQ @nfe_base-nfe_base-e_tomadora
             AND branch EQ @nfe_base-nfe_base-f_tomadora.

          CASE j_1bbranch-industry.
            WHEN '07'.
              DATA(lc_cfop) = wa_item-prod_cfop(1) && '202AA'.
            WHEN OTHERS.

          ENDCASE.

          IF lc_cfop IS NOT INITIAL.
            "Devolução de compra para comercialização
            APPEND VALUE #( ebeln = wa_ty_vinc-ebeln ebelp = wa_ty_vinc-ebelp rblgp = lc_rblgp cfop  = lc_cfop ) TO gt_cfop.
          ENDIF.

        ENDLOOP.

      WHEN zif_material_destinacao=>st_tp_origem_romaneio.

    ENDCASE.

    me->zif_material_destinacao~set_validar(  ).

    IF gt_cfop[] IS NOT INITIAL.
      EXPORT gt_cfop FROM gt_cfop TO MEMORY ID 'CFOPXML'.
    ENDIF.

*-CS2025000249-27.05.2025-#175255-JT-inicio
    IF i_gerar_via_job = abap_true.
      me->zif_material_destinacao~set_gerar_nota_devolucao_job( EXPORTING i_chave_nfe  = wa_zmmt0115-orig_nfe
                                                                          i_headerdata = lc_headerdata
                                                                          i_itemdata   = it_itemdata
                                                                          i_cfop       = gt_cfop
                                                                IMPORTING e_belnr_dev  = me->zif_material_destinacao~at_zmmt0114-belnr_dev
                                                                          e_gjahr_dev  = me->zif_material_destinacao~at_zmmt0114-gjahr_dev ).
    ELSE.
      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          headerdata       = lc_headerdata
        IMPORTING
          invoicedocnumber = me->zif_material_destinacao~at_zmmt0114-belnr_dev
          fiscalyear       = me->zif_material_destinacao~at_zmmt0114-gjahr_dev
        TABLES
          itemdata         = it_itemdata
          return           = it_bapiret2.
    ENDIF.
*-CS2025000249-27.05.2025-#175255-JT-fim

    IF me->zif_material_destinacao~at_zmmt0114-belnr_dev IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.

      e_belnr_dev = me->zif_material_destinacao~at_zmmt0114-belnr_dev.
      e_gjahr_dev = me->zif_material_destinacao~at_zmmt0114-gjahr_dev.

      CONCATENATE me->zif_material_destinacao~at_zmmt0114-belnr_dev me->zif_material_destinacao~at_zmmt0114-gjahr_dev INTO lc_refkey.

      SELECT SINGLE * INTO @DATA(wa_j_1bnflin)
        FROM j_1bnflin
       WHERE refkey EQ @lc_refkey
         AND reftyp EQ 'LI'.

      IF sy-subrc IS NOT INITIAL.
        WAIT UP TO 2 SECONDS.
        SELECT SINGLE * INTO @wa_j_1bnflin
          FROM j_1bnflin
         WHERE refkey EQ @lc_refkey
           AND reftyp EQ 'LI'.
      ENDIF.

      IF sy-subrc IS INITIAL.
        me->zif_material_destinacao~at_zmmt0114-docnum_dev = wa_j_1bnflin-docnum.
        e_docnum_dev = me->zif_material_destinacao~at_zmmt0114-docnum_dev.
      ENDIF.

      me->zif_material_destinacao~set_gravar( ).

      CASE me->zif_material_destinacao~at_zmmt0114-tp_origem.
        WHEN zif_material_destinacao=>st_tp_origem_nfe.

          CASE me->zif_material_destinacao~at_zmmt0114-tp_destinacao.

            WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.

              READ TABLE me->zif_material_destinacao~at_zmmt0115 INDEX 1 INTO DATA(wa_0115).

              UPDATE zib_nfe_dist_ter
                 SET docnum_dev = me->zif_material_destinacao~at_zmmt0114-docnum_dev
                     belnr_dev  = me->zif_material_destinacao~at_zmmt0114-belnr_dev
                     gjahr_dev  = me->zif_material_destinacao~at_zmmt0114-gjahr_dev
               WHERE chave_nfe EQ wa_0115-orig_nfe.

              COMMIT WORK AND WAIT.

*-CS2025000249-27.05.2025-#175255-JT-inicio
*--------------------------------------------
*------------ ajustar CFOP e Leis Fiscais NF
*--------------------------------------------
              me->zif_material_destinacao~set_ajustar_cfop_nf( i_tp_destinacao = me->zif_material_destinacao~at_zmmt0114-tp_destinacao
                                                               i_chave_nfe     = wa_0115-orig_nfe ).
              COMMIT WORK AND WAIT.
*-CS2025000249-27.05.2025-#175255-JT-fim

            WHEN OTHERS.

          ENDCASE.

        WHEN zif_material_destinacao=>st_tp_origem_romaneio.
      ENDCASE.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      READ TABLE it_bapiret2 INTO DATA(wa_bapiret2)
      WITH KEY  type = 'E'.

*-CS2025000249-27.05.2025-#175255-JT-inicio
      IF sy-subrc <> 0.
        wa_bapiret2-id         = 'SD'.
        wa_bapiret2-number     = '024'.
        wa_bapiret2-message_v1 = 'Não foi possível gerar DEVOLUÇÃO!'.
        wa_bapiret2-message_v2 = 'Verifique Log!'.
      ENDIF.
*-CS2025000249-27.05.2025-#175255-JT-fim

      RAISE EXCEPTION TYPE zcx_material_destinacao
        EXPORTING
          textid = VALUE #( msgid = wa_bapiret2-id
                            msgno = wa_bapiret2-number
                            attr1 = wa_bapiret2-message_v1
                            attr2 = wa_bapiret2-message_v2
                            attr3 = wa_bapiret2-message_v3
                            attr4 = wa_bapiret2-message_v4 )
          msgid  = wa_bapiret2-id
          msgno  = wa_bapiret2-number
          msgty  = 'E'
          msgv1  = wa_bapiret2-message_v1
          msgv2  = wa_bapiret2-message_v2
          msgv3  = wa_bapiret2-message_v3
          msgv4  = wa_bapiret2-message_v4.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_GRAVAR.

    R_IF_MATERIAL_DESTINACAO = ME.

    CLEAR: E_GRAVOU.

    ME->ZIF_MATERIAL_DESTINACAO~SET_VALIDAR( IMPORTING E_VALIDOU = DATA(E_VALIDOU) ).

    CHECK E_VALIDOU EQ ABAP_TRUE.

    IF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO IS INITIAL.
      ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BWART = ME->ZIF_MATERIAL_DESTINACAO~AT_TIPO_MOVIMENTO.
      ME->ZIF_MATERIAL_DESTINACAO~GET_NEW_ID( IMPORTING E_ID_DESTINACAO = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO ).
      ME->ZIF_MATERIAL_DESTINACAO~SET_ENQUEUE( I_ID_DESTINACAO = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO ).
    ENDIF.

    LOOP AT ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115 ASSIGNING FIELD-SYMBOL(<FS_0115>).
      <FS_0115>-ID_DESTINACAO = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO.
    ENDLOOP.

    LOOP AT ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0116 ASSIGNING FIELD-SYMBOL(<FS_0116>).
      <FS_0116>-ID_DESTINACAO = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO.
    ENDLOOP.

    ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_DESTINACAO = ME->ZIF_MATERIAL_DESTINACAO~AT_TP_DESTINACAO.
    ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BWART         = ME->ZIF_MATERIAL_DESTINACAO~AT_TIPO_MOVIMENTO.
    ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-GRUND         = ME->ZIF_MATERIAL_DESTINACAO~AT_MOTIVO_MOVIMENTO.
    ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-DT_LANCAMENTO = SY-DATUM.
    ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-HR_LANCAMENTO = SY-UZEIT.
    ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-US_LANCAMENTO = SY-UNAME.

    DELETE FROM ZMMT0115 WHERE ID_DESTINACAO EQ ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO.
    DELETE FROM ZMMT0116 WHERE ID_DESTINACAO EQ ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO.
    DELETE FROM ZMMT0118 WHERE ID_DESTINACAO EQ ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO.

    MODIFY ZMMT0114 FROM ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114.
    MODIFY ZMMT0115 FROM TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115.
    MODIFY ZMMT0116 FROM TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0116.
    MODIFY ZMMT0118 FROM TABLE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0118.
    COMMIT WORK.

    E_GRAVOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_NEW.

    R_IF_MATERIAL_DESTINACAO = ME.

    ME->ZIF_MATERIAL_DESTINACAO~SET_CLEAR( ).

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_REGISTRO.

    R_IF_MATERIAL_DESTINACAO = ME->ZIF_MATERIAL_DESTINACAO~SET_CLEAR( ).

    SELECT SINGLE * INTO @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114
      FROM ZMMT0114
     WHERE ID_DESTINACAO EQ @I_ID_DESTINACAO.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_NOT_FOUND_DEST-MSGID
                            MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_NOT_FOUND_DEST-MSGNO
                            ATTR1 = CONV #( I_ID_DESTINACAO ) )
          MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_NOT_FOUND_DEST-MSGID
          MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_NOT_FOUND_DEST-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_ID_DESTINACAO ).
    ENDIF.

    SELECT * INTO TABLE @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115
      FROM ZMMT0115
     WHERE ID_DESTINACAO EQ @I_ID_DESTINACAO.

    SELECT * INTO TABLE @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0116
      FROM ZMMT0116
     WHERE ID_DESTINACAO EQ @I_ID_DESTINACAO.

    SELECT * INTO TABLE @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0118
      FROM ZMMT0118
     WHERE ID_DESTINACAO EQ @I_ID_DESTINACAO.

    ME->ZIF_MATERIAL_DESTINACAO~AT_TP_DESTINACAO  = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_DESTINACAO.
    ME->ZIF_MATERIAL_DESTINACAO~AT_TIPO_MOVIMENTO = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BWART.
    ME->ZIF_MATERIAL_DESTINACAO~AT_MOTIVO_MOVIMENTO = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-GRUND.

  ENDMETHOD.


  METHOD zif_material_destinacao~set_registro_doc_destinacao.

    r_if_material_destinacao = me.

    SELECT SINGLE * INTO @DATA(wa_zmmt0114)
      FROM zmmt0114
     WHERE mblnr EQ @i_mblnr
       AND mjahr EQ @i_mjahr.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_material_destinacao
        EXPORTING
          textid = VALUE #( msgid = zcx_material_destinacao=>zcx_not_found_dest_mat-msgid
                            msgno = zcx_material_destinacao=>zcx_not_found_dest_mat-msgno
                            attr1 = CONV #( i_mblnr )
                            attr2 = CONV #( i_mjahr ) )
          msgid  = zcx_material_destinacao=>zcx_not_found_dest_mat-msgid
          msgno  = zcx_material_destinacao=>zcx_not_found_dest_mat-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_mblnr )
          msgv2  = CONV #( i_mjahr ).
    ENDIF.

    me->zif_material_destinacao~set_registro( i_id_destinacao = wa_zmmt0114-id_destinacao ).

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_SALDO_ITEM_MSEG.

    DATA: LC_TIPO_MOVIMENTO TYPE SHKZG.

    CLEAR: E_VALOR, R_MENGE.

    "Buscar Documento de Material
    SELECT SINGLE * INTO @DATA(WA_MSEG)
      FROM MSEG
     WHERE MBLNR EQ @I_MBLNR
       AND MJAHR EQ @I_MJAHR
       AND ZEILE EQ @I_ZEILE.

    CHECK SY-SUBRC IS INITIAL.

    R_MENGE = WA_MSEG-MENGE.

    "Somente reduz saldo documentos com sentido inverso
    "Exe.: Movmento de Entrada
    LC_TIPO_MOVIMENTO = WA_MSEG-SHKZG.

    "Buscar Utilização
    SELECT * INTO TABLE @DATA(IT_ZMMT0116)
      FROM ZMMT0116
     WHERE ORIG_MBLNR EQ @I_MBLNR
       AND ORIG_MJAHR EQ @I_MJAHR
       AND ORIG_ZEILE EQ @I_ZEILE.

    IF SY-SUBRC IS INITIAL.
      "Buscar Cabeçalho da Utilização
      SELECT * INTO TABLE @DATA(IT_ZMMT0114)
        FROM ZMMT0114
         FOR ALL ENTRIES IN @IT_ZMMT0116
       WHERE ID_DESTINACAO EQ @IT_ZMMT0116-ID_DESTINACAO
         AND MBLNR     NE @SPACE
         AND BELNR_DEV EQ @SPACE.

      IF SY-SUBRC IS INITIAL.
        "Exe.: Movimento de Saída
        SELECT * INTO TABLE @DATA(IT_MSEG_DETINACAO)
          FROM MSEG
           FOR ALL ENTRIES IN @IT_ZMMT0114
         WHERE MBLNR EQ @IT_ZMMT0114-MBLNR
           AND MJAHR EQ @IT_ZMMT0114-MJAHR
           AND SHKZG NE @LC_TIPO_MOVIMENTO.

        "Buscar Estorno
        IF SY-SUBRC IS INITIAL.
          SELECT * INTO TABLE @DATA(IT_MSEG_DETINACAO_ESTORNADO)
            FROM MSEG
             FOR ALL ENTRIES IN @IT_MSEG_DETINACAO
           WHERE SMBLN EQ @IT_MSEG_DETINACAO-MBLNR
             AND SJAHR EQ @IT_MSEG_DETINACAO-MJAHR
             AND SMBLP EQ @IT_MSEG_DETINACAO-ZEILE.
        ENDIF.
      ENDIF.

      SORT IT_ZMMT0114 BY ID_DESTINACAO.
      SORT IT_MSEG_DETINACAO BY MBLNR MJAHR.
      SORT IT_MSEG_DETINACAO_ESTORNADO BY SMBLN SJAHR.
    ENDIF.

    LOOP AT IT_ZMMT0116 INTO DATA(WA_ZMMT0116).

      READ TABLE IT_ZMMT0114 WITH KEY ID_DESTINACAO = WA_ZMMT0116-ID_DESTINACAO BINARY SEARCH INTO DATA(WA_ZMMT0114).
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE IT_MSEG_DETINACAO_ESTORNADO WITH KEY SMBLN = WA_ZMMT0114-MBLNR SJAHR = WA_ZMMT0114-MJAHR BINARY SEARCH INTO DATA(WA_MSEG_DETINACAO).
      IF SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      "Saldo utilizado anterior
      R_MENGE = R_MENGE - WA_ZMMT0116-MENGE.

    ENDLOOP.

    "Devolução """""""""""""""""""""""""""""""""""""""""""
    IF IT_ZMMT0116[] IS NOT INITIAL.

      SELECT * INTO TABLE @DATA(IT_ZMMT0114_EST)
        FROM ZMMT0114
         FOR ALL ENTRIES IN @IT_ZMMT0116
       WHERE ID_DESTINACAO EQ @IT_ZMMT0116-ID_DESTINACAO
         AND MBLNR     EQ @SPACE
         AND BELNR_DEV NE @SPACE.

      IF SY-SUBRC IS INITIAL.

        SORT IT_ZMMT0114_EST BY ID_DESTINACAO.

        SELECT * INTO TABLE @DATA(IT_RBKP)
          FROM RBKP
           FOR ALL ENTRIES IN @IT_ZMMT0114_EST
         WHERE BELNR EQ @IT_ZMMT0114_EST-BELNR_DEV
           AND GJAHR EQ @IT_ZMMT0114_EST-GJAHR_DEV
           AND STBLG EQ @SPACE.

        SORT IT_RBKP BY BELNR GJAHR.
      ENDIF.

      LOOP AT IT_ZMMT0116 INTO WA_ZMMT0116.

        READ TABLE IT_ZMMT0114_EST WITH KEY ID_DESTINACAO = WA_ZMMT0116-ID_DESTINACAO BINARY SEARCH INTO WA_ZMMT0114.
        IF SY-SUBRC IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE IT_RBKP WITH KEY BELNR = WA_ZMMT0114-BELNR_DEV GJAHR = WA_ZMMT0114-GJAHR_DEV BINARY SEARCH TRANSPORTING NO FIELDS.
        IF SY-SUBRC IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        "Saldo utilizado anterior
        R_MENGE = R_MENGE - WA_ZMMT0116-MENGE.

      ENDLOOP.

    ENDIF.

    IF R_MENGE GT 0.

      "Verificar documento de material de origem NF-e InBound
      SELECT SINGLE * INTO @DATA(WA_NFE_INBOUND)
        FROM ZIB_NFE_DIST_TER
       WHERE MBLNR EQ @I_MBLNR
         AND MJAHR EQ @I_MJAHR.

      IF SY-SUBRC IS INITIAL.

        SELECT * INTO TABLE @DATA(IT_ZIB_NFE_DIST_ITM)
          FROM ZIB_NFE_DIST_ITM
         WHERE CHAVE_NFE EQ @WA_NFE_INBOUND-CHAVE_NFE
           AND MATNR     EQ @WA_MSEG-MATNR.

        "Buscar Valor do Item com Baso no Valor do Item da Nota

        DATA(VALOR_SOMA_ITEM) =
         REDUCE J_1BBASE( INIT I TYPE J_1BBASE
          FOR LS IN IT_ZIB_NFE_DIST_ITM WHERE ( MATNR EQ WA_MSEG-MATNR )
            NEXT I = I + LS-PROD_VLR_TOTAL_B ).

        DATA(QUANT_SOMA_ITEM) =
         REDUCE J_1BBASE( INIT I TYPE J_1BBASE
          FOR LS IN IT_ZIB_NFE_DIST_ITM WHERE ( MATNR EQ WA_MSEG-MATNR )
            NEXT I = I + LS-MENGE ).

        E_VALOR = ( WA_MSEG-MENGE * VALOR_SOMA_ITEM ) / QUANT_SOMA_ITEM.

        CHECK 1 EQ 2.
      ENDIF.

      "Romaneio de Saída nem sempre tem vinculo por isso não deu para fazer

      "Pega valor do documento de mercadoria
      E_VALOR = ( R_MENGE * WA_MSEG-DMBTR ) / WA_MSEG-MENGE.

    ELSE.
      R_MENGE = 0.
      E_VALOR = 0.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_SALDO_ITEM_ROM.

    CLEAR: E_VALOR, R_MENGE.

    SELECT SINGLE * INTO @DATA(WA_ZSDT0001)
      FROM ZSDT0001
     WHERE CH_REFERENCIA EQ @I_CH_REFERENCIA.

    CHECK SY-SUBRC IS INITIAL AND WA_ZSDT0001-EBELN IS NOT INITIAL.

    "Buscar Documento do Romaneio
    "Somente permitir romaneio de saída com pedido de compra ZARM
    SELECT SINGLE * INTO @DATA(WA_EKKO)
      FROM EKKO
     WHERE EBELN EQ @WA_ZSDT0001-EBELN.

    CHECK WA_EKKO-BSART EQ 'ZARM' AND WA_EKKO-BSTYP EQ 'F'.

    SELECT SINGLE * INTO @DATA(WA_EKPO)
      FROM EKPO
     WHERE EBELN EQ @WA_ZSDT0001-EBELN.

    R_MENGE = WA_ZSDT0001-PESO_LIQ.

    SELECT * INTO TABLE @DATA(IT_ZMMT0118)
      FROM ZMMT0118
     WHERE CH_REFERENCIA EQ @I_CH_REFERENCIA.

    IF SY-SUBRC IS INITIAL.
      SELECT * INTO TABLE @DATA(IT_ZMMT0114)
        FROM ZMMT0114
         FOR ALL ENTRIES IN @IT_ZMMT0118
       WHERE ID_DESTINACAO EQ @IT_ZMMT0118-ID_DESTINACAO.

      IF SY-SUBRC IS INITIAL.
        SELECT * INTO TABLE @DATA(IT_MSEG)
          FROM MSEG
           FOR ALL ENTRIES IN @IT_ZMMT0114
         WHERE MBLNR EQ @IT_ZMMT0114-MBLNR
           AND MJAHR EQ @IT_ZMMT0114-MJAHR.

        IF SY-SUBRC IS INITIAL.
          "Estornos
          SELECT * INTO TABLE @DATA(IT_MSEG_ESTORNO)
            FROM MSEG
             FOR ALL ENTRIES IN @IT_MSEG
           WHERE MBLNR EQ @IT_MSEG-MBLNR
             AND MJAHR EQ @IT_MSEG-MJAHR.
        ENDIF.
      ENDIF.

      SORT IT_ZMMT0114 BY ID_DESTINACAO.
      SORT IT_MSEG BY MBLNR MJAHR.
      SORT IT_MSEG_ESTORNO BY SMBLN SJAHR.

    ENDIF.

    LOOP AT IT_ZMMT0118 INTO DATA(WA_ZMMT0118).

      READ TABLE IT_ZMMT0114 WITH KEY ID_DESTINACAO = WA_ZMMT0118-ID_DESTINACAO BINARY SEARCH INTO DATA(WA_ZMMT0114).
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE IT_MSEG_ESTORNO WITH KEY SMBLN = WA_ZMMT0114-MBLNR SJAHR = WA_ZMMT0114-MJAHR BINARY SEARCH INTO DATA(WA_MSEG_ESTORNO).
      IF SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      R_MENGE = R_MENGE - WA_ZMMT0118-MENGE.

    ENDLOOP.

    E_VALOR = R_MENGE * WA_EKPO-NETPR.

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_VALIDAR.

    CLEAR: E_VALIDOU.

    R_IF_MATERIAL_DESTINACAO = ME.

    CASE ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-TP_DESTINACAO.
      WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_ARMAZENAR.

        IF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BWART NE 'Z41' AND ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BWART NE 'ZQ1'.
          RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_VALIDA-MSGID
                                MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_VALIDA-MSGNO
                                ATTR1 = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BWART )
              MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_VALIDA-MSGID
              MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_VALIDA-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BWART ).
        ENDIF.

      WHEN ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_DEVOLUCAO.

        IF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BWART NE '122'.
          RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_VALIDA-MSGID
                                MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_VALIDA-MSGNO
                                ATTR1 = ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BWART )
              MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_VALIDA-MSGID
              MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_VALIDA-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-BWART ).
        ENDIF.

      WHEN OTHERS.

    ENDCASE.

    IF ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115[] IS NOT INITIAL.

      SELECT * INTO TABLE @DATA(IT_MSEG)
        FROM MSEG
         FOR ALL ENTRIES IN @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115
       WHERE MBLNR EQ @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115-ORIG_MBLNR
         AND MJAHR EQ @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115-ORIG_MJAHR.

      SORT IT_MSEG BY MBLNR MJAHR ZEILE.

      "Varias Entradas para o Mesmo Documento de Material
      SELECT * INTO TABLE @DATA(IT_ZMMT0116)
        FROM ZMMT0116
         FOR ALL ENTRIES IN @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115
       WHERE ORIG_MBLNR EQ @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115-ORIG_MBLNR
         AND ORIG_MJAHR EQ @ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115-ORIG_MJAHR.

      SORT IT_ZMMT0116 BY ORIG_MBLNR ORIG_MJAHR ORIG_ZEILE.

      IF IT_ZMMT0116[] IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(IT_ZMMT0114)
          FROM ZMMT0114
           FOR ALL ENTRIES IN @IT_ZMMT0116
         WHERE ID_DESTINACAO EQ @IT_ZMMT0116-ID_DESTINACAO.

        SORT IT_ZMMT0114 BY ID_DESTINACAO.
      ENDIF.

      IF IT_ZMMT0114[] IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(IT_MSEG_DESTINADO)
          FROM MSEG
           FOR ALL ENTRIES IN @IT_ZMMT0114
         WHERE MBLNR EQ @IT_ZMMT0114-MBLNR
           AND MJAHR EQ @IT_ZMMT0114-MJAHR.

        SORT IT_MSEG_DESTINADO BY MBLNR MJAHR.
      ENDIF.

      IF IT_MSEG_DESTINADO[] IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(IT_MSEG_DESTINADO_ESTORNO)
          FROM MSEG
           FOR ALL ENTRIES IN @IT_MSEG_DESTINADO
         WHERE SMBLN EQ @IT_MSEG_DESTINADO-MBLNR
           AND SJAHR EQ @IT_MSEG_DESTINADO-MJAHR
           AND SMBLP EQ @IT_MSEG_DESTINADO-ZEILE.

        SORT IT_MSEG_DESTINADO_ESTORNO BY SMBLN SJAHR SMBLP.
      ENDIF.

    ENDIF.

    "Somente Permitir Documentos de Material de Entrada
    LOOP AT ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115 INTO DATA(WA_ZMMT0115).
      LOOP AT IT_MSEG INTO DATA(WA_MSEG).
        IF WA_MSEG-SMBLN IS NOT INITIAL.
          RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_DOC_SAIDA_NAO-MSGID
                                MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_DOC_SAIDA_NAO-MSGNO )
              MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_DOC_SAIDA_NAO-MSGID
              MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_DOC_SAIDA_NAO-MSGNO
              MSGTY  = 'E'.
        ENDIF.
      ENDLOOP.
    ENDLOOP.


    "Verificar Saldo do Documento de Material de Origem """"""""""""""""""""""""""""""""""""""""""""""
    LOOP AT ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0116 INTO DATA(WA_ZMMT0116).

      "Buscar Registro Orignal """"""""""""""""""""""""""""""""""""""""""""
      READ TABLE IT_MSEG ASSIGNING FIELD-SYMBOL(<FS_MSEG>)
        WITH KEY
           MBLNR = WA_ZMMT0116-ORIG_MBLNR
           MJAHR = WA_ZMMT0116-ORIG_MJAHR
           ZEILE = WA_ZMMT0116-ORIG_ZEILE
        BINARY SEARCH.

      "Saldo Utilizado Anterior
      LOOP AT IT_ZMMT0116 INTO DATA(WA_ZMMT0116_UTILIZADO)
        WHERE ORIG_MBLNR EQ WA_ZMMT0116-ORIG_MBLNR
          AND ORIG_MJAHR EQ WA_ZMMT0116-ORIG_MJAHR
          AND ORIG_ZEILE EQ WA_ZMMT0116-ORIG_ZEILE.

        IF WA_ZMMT0116_UTILIZADO-ID_DESTINACAO EQ ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0114-ID_DESTINACAO.
          CONTINUE.
        ENDIF.

        "Verificar se Documento Anterior não está estornado
        READ TABLE IT_ZMMT0114 INTO DATA(WA_ZMMT0114) WITH KEY ID_DESTINACAO = WA_ZMMT0116_UTILIZADO-ID_DESTINACAO BINARY SEARCH.
        IF SY-SUBRC IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE IT_MSEG_DESTINADO INTO DATA(WA_MSEG_DESTINADO) WITH KEY MBLNR = WA_ZMMT0114-MBLNR MJAHR = WA_ZMMT0114-MJAHR BINARY SEARCH.
        IF SY-SUBRC IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE IT_MSEG_DESTINADO_ESTORNO WITH KEY SMBLN = WA_MSEG_DESTINADO-MBLNR
           SJAHR = WA_MSEG_DESTINADO-MJAHR
           SMBLP = WA_MSEG_DESTINADO-ZEILE
           TRANSPORTING NO FIELDS BINARY SEARCH.

        IF SY-SUBRC IS INITIAL.
          CONTINUE.
        ENDIF.

        <FS_MSEG>-MENGE = <FS_MSEG>-MENGE - WA_ZMMT0116_UTILIZADO-MENGE.

      ENDLOOP.

      "Saldo Utilizado Agora
      <FS_MSEG>-MENGE = <FS_MSEG>-MENGE - WA_ZMMT0116-MENGE.

      IF <FS_MSEG>-MENGE LT 0.
        RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_SEM_SALDO-MSGID
                              MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_SEM_SALDO-MSGNO
                              ATTR1 = <FS_MSEG>-MBLNR
                              ATTR2 = <FS_MSEG>-MJAHR
                              ATTR3 = <FS_MSEG>-ZEILE )
            MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_SEM_SALDO-MSGID
            MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_SEM_SALDO-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( <FS_MSEG>-MBLNR )
            MSGV2  = CONV #( <FS_MSEG>-MJAHR )
            MSGV3  = CONV #( <FS_MSEG>-ZEILE ).
      ENDIF.

    ENDLOOP.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "*-US191846-01.10.2025-#191846-WPP-inicio
    READ TABLE me->zif_material_destinacao~at_zmmt0115 INTO DATA(wa_zmmt0115_check) INDEX 1. "*-CS2025000249-27.05.2025-#175255-WPP

    me->zif_material_destinacao~check_parametros_zmm0185( i_tp_destinacao = me->zif_material_destinacao~at_zmmt0114-tp_destinacao
                                                          i_chave_nfe     = wa_zmmt0115_check-orig_nfe ).
    "*-US191846-01.10.2025-#191846-WPP-Fim

    E_VALIDOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_material_destinacao~set_ajustar_cfop_nf.

    DATA: lv_bsart          TYPE esart,
          lc_nfobjn         TYPE j_1binterf-nfobjn,
          lv_docnum         TYPE j_1bdocnum,
          obj_header        TYPE j_1bnfdoc,
          obj_partner       TYPE TABLE OF j_1bnfnad,
          obj_item          TYPE TABLE OF j_1bnflin,
          obj_item_tax      TYPE TABLE OF j_1bnfstx,
          obj_header_msg    TYPE TABLE OF j_1bnfftx,
          obj_refer_msg     TYPE TABLE OF j_1bnfref,
          obj_ot_partner    TYPE TABLE OF j_1bnfcpd,
          obj_import_di     TYPE TABLE OF j_1bnfimport_di,
          obj_import_adi    TYPE TABLE OF j_1bnfimport_adi,
          obj_cte_res       TYPE TABLE OF j_1bcte_d_res,
          obj_cte_docref    TYPE TABLE OF j_1bcte_d_docref,
          obj_trans_volumes TYPE TABLE OF j_1bnftransvol,
          obj_trailer_info  TYPE TABLE OF j_1bnftrailer,
          obj_trade_notes   TYPE TABLE OF j_1bnftradenotes,
          obj_add_info      TYPE TABLE OF j_1bnfadd_info,
          obj_ref_proc      TYPE TABLE OF j_1bnfrefproc,
          obj_sugar_suppl   TYPE TABLE OF j_1bnfsugarsuppl,
          obj_sugar_deduc   TYPE TABLE OF j_1bnfsugardeduc,
          obj_vehicle       TYPE TABLE OF j_1bnfvehicle,
          obj_pharmaceut    TYPE TABLE OF j_1bnfpharmaceut,
          obj_fuel          TYPE TABLE OF j_1bnffuel,
          obj_export        TYPE TABLE OF j_1bnfe_export,
          obj_nve           TYPE TABLE OF j_1bnfnve,
          lc_dados          TYPE zsde0185,    "*-US191846-01.10.2025-#191846-JT-inicio
          lc_retorno        TYPE zmmt0154_t,  "*-US191846-01.10.2025-#191846-JT-inicio
          wc_retorno        TYPE zmmt0154.    "*-US191846-01.10.2025-#191846-JT-inicio

    r_if_material_destinacao = me.

    SELECT SINGLE *
      INTO @DATA(_zib_nfe_dist_ter)
      FROM zib_nfe_dist_ter
     WHERE chave_nfe = @i_chave_nfe.

    CHECK sy-subrc = 0.

    CASE i_tp_destinacao.
      WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.
        CHECK _zib_nfe_dist_ter-mblnr_arm  IS NOT INITIAL AND
              _zib_nfe_dist_ter-docnum_arm IS NOT INITIAL.

        "*-US191846-01.10.2025-#191846-WPP-inicio
        lv_bsart = COND #( WHEN _zib_nfe_dist_ter-ck_trans_nf_propri = abap_true THEN 'ZARM' ELSE 'ZARS' ).
        "*-US191846-01.10.2025-#191846-WPP-Fim

      WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.

        CHECK _zib_nfe_dist_ter-belnr_dev  IS NOT INITIAL AND
              _zib_nfe_dist_ter-docnum_dev IS NOT INITIAL.

        "*-US191846-01.10.2025-#191846-WPP-inicio
        SELECT SINGLE *
          FROM zib_nfe_dist_itm INTO @DATA(lwa_zib_nfe_dist_itm)
         WHERE chave_nfe EQ @i_chave_nfe
           AND ebeln NE @space.

        IF sy-subrc EQ 0.
          SELECT SINGLE *
            FROM ekko INTO @DATA(lwa_ekko_nfe)
           WHERE ebeln EQ @lwa_zib_nfe_dist_itm-ebeln.
        ENDIF.

        lv_bsart = lwa_ekko_nfe-bsart.
        "*-US191846-01.10.2025-#191846-WPP-Fim

    ENDCASE.

    "*-US191846-01.10.2025-#191846-WPP-inicio
*    lv_bsart = COND #( WHEN _zib_nfe_dist_ter-ck_trans_nf_propri = abap_true THEN 'ZARM'
*                                                                             ELSE 'ZARS' ).
    "*-US191846-01.10.2025-#191846-WPP-Fim

    SELECT chave_nfe, matnr
      INTO TABLE @DATA(t_zib_nfe_dist_itm)
      FROM zib_nfe_dist_itm
     WHERE chave_nfe = @i_chave_nfe.

    CHECK sy-subrc = 0.



    SELECT SINGLE regio
      INTO @DATA(_regio_toma)
      FROM t001w
     WHERE werks = @_zib_nfe_dist_ter-f_tomadora.

    CHECK sy-subrc = 0.

    IF _zib_nfe_dist_ter-f_armazem IS INITIAL.
      SELECT SINGLE regio
        INTO @DATA(_regio_armz)
        FROM lfa1
       WHERE lifnr = @_zib_nfe_dist_ter-p_emissor.
    ELSE.
      SELECT SINGLE regio
        INTO @_regio_armz
        FROM lfa1
       WHERE lifnr = @_zib_nfe_dist_ter-f_armazem.
    ENDIF.

    CHECK sy-subrc = 0.

    SELECT matnr, matkl
      INTO TABLE @DATA(t_mara)
      FROM mara
       FOR ALL ENTRIES IN @t_zib_nfe_dist_itm
     WHERE matnr = @t_zib_nfe_dist_itm-matnr.

    CHECK sy-subrc = 0.

    CASE i_tp_destinacao.
      WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.
        lv_docnum = _zib_nfe_dist_ter-docnum_arm.

        SELECT SINGLE mwskz
          INTO @DATA(_mwskz)
          FROM mseg
         WHERE mblnr  = @_zib_nfe_dist_ter-mblnr_arm
           AND mjahr  = @_zib_nfe_dist_ter-mjahr_arm.

      WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.
        lv_docnum = _zib_nfe_dist_ter-docnum_dev.

        SELECT SINGLE mwskz
          INTO @_mwskz
          FROM j_1bnflin
         WHERE docnum = @_zib_nfe_dist_ter-docnum_dev
           AND reftyp = 'LI'.
    ENDCASE.

    CHECK sy-subrc = 0.

*------------------------------------
*-- read NF
*------------------------------------
    CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
      EXPORTING
        doc_number         = lv_docnum
      IMPORTING
        obj_number         = lc_nfobjn
      EXCEPTIONS
        document_not_found = 1
        docum_lock         = 2
        OTHERS             = 3.

    CALL FUNCTION 'J_1B_NF_OBJECT_READ'
      EXPORTING
        obj_number        = lc_nfobjn
      IMPORTING
        obj_header        = obj_header
      TABLES
        obj_partner       = obj_partner
        obj_item          = obj_item
        obj_item_tax      = obj_item_tax
        obj_header_msg    = obj_header_msg
        obj_refer_msg     = obj_refer_msg
        obj_ot_partner    = obj_ot_partner
        obj_import_di     = obj_import_di
        obj_import_adi    = obj_import_adi
        obj_cte_res       = obj_cte_res
        obj_cte_docref    = obj_cte_docref
        obj_trans_volumes = obj_trans_volumes
        obj_trailer_info  = obj_trailer_info
        obj_trade_notes   = obj_trade_notes
        obj_add_info      = obj_add_info
        obj_ref_proc      = obj_ref_proc
        obj_sugar_suppl   = obj_sugar_suppl
        obj_sugar_deduc   = obj_sugar_deduc
        obj_vehicle       = obj_vehicle
        obj_pharmaceut    = obj_pharmaceut
        obj_fuel          = obj_fuel
        obj_export        = obj_export
        obj_nve           = obj_nve
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.

    CHECK sy-subrc IS INITIAL.

*------------------------------------
*-- Ajustar CFOP - j_1bnflin
*------------------------------------
    SORT t_zib_nfe_dist_itm BY matnr.
    DELETE ADJACENT DUPLICATES FROM t_zib_nfe_dist_itm COMPARING matnr.

    LOOP AT t_zib_nfe_dist_itm INTO DATA(_zib_nfe_dist_itm).

      READ TABLE t_mara INTO DATA(_mara) WITH KEY matnr = _zib_nfe_dist_itm-matnr.
      CHECK sy-subrc = 0.

*-US191846-01.10.2025-#191846-JT-inicio
*      SELECT SINGLE *
*        FROM zmmt0154
*        INTO @DATA(_zmmt0154)
*       WHERE bsart   = @lv_bsart
*         AND uf_orig = @_regio_toma
*         AND uf_dest = @_regio_armz
*         AND matnr   = @_zib_nfe_dist_itm-matnr
*         AND bukrs   = @_zib_nfe_dist_ter-e_tomadora.
*
*      IF sy-subrc <> 0.
*        SELECT SINGLE *
*          FROM   zmmt0154
*          INTO @_zmmt0154
*         WHERE bsart   = @lv_bsart
*           AND uf_orig = @_regio_toma
*           AND uf_dest = @_regio_armz
*           AND matkl   = @_mara-matkl
*           AND bukrs   = @_zib_nfe_dist_ter-e_tomadora.
*      ENDIF.

      CLEAR: lc_dados.
      lc_dados-bsart-valor   = lv_bsart.
      lc_dados-uf_orig-valor = _regio_toma.
      lc_dados-uf_dest-valor = _regio_armz.
      lc_dados-matnr-valor   = _mara-matnr.
      lc_dados-bukrs-valor   = _zib_nfe_dist_ter-e_tomadora.
      lc_dados-direcao-valor = '2'. "saida

      lc_retorno = zcl_leis_fiscais=>get_impostos( i_dados = lc_dados i_todos = abap_false ).

      READ TABLE lc_retorno INTO DATA(_zmmt0154) INDEX 1.

      IF sy-subrc NE 0.
        DATA: lva_msg_error TYPE c LENGTH 200.

        lva_msg_error = |Não encontrado parametros com IVA na ZMM0185 para o tipo Pedido: { lv_bsart } Origem: { _regio_toma }|.
        lva_msg_error = |{ lva_msg_error } Destino: { _regio_armz } Material: { _mara-matnr } Empresa: { _zib_nfe_dist_ter-e_tomadora } Direção: Saída!|.
        lva_msg_error = |{ lva_msg_error } Criar FI para Departamento Fiscal|.

        RAISE EXCEPTION TYPE zcx_material_destinacao
          EXPORTING
            textid = VALUE #( msgid = zcx_material_destinacao=>zcx_erro_geral-msgid
                              msgno = zcx_material_destinacao=>zcx_erro_geral-msgno
                              attr1 = CONV #( lva_msg_error+000(050) )
                              attr2 = CONV #( lva_msg_error+050(050) )
                              attr3 = CONV #( lva_msg_error+100(050) )
                              attr4 = CONV #( lva_msg_error+150(050) )
                               )
            msgid  = zcx_material_destinacao=>zcx_erro_geral-msgid
            msgno  = zcx_material_destinacao=>zcx_erro_geral-msgno
            msgv1  = CONV #( lva_msg_error+000(050) )
            msgv2  = CONV #( lva_msg_error+050(050) )
            msgv3  = CONV #( lva_msg_error+100(050) )
            msgv4  = CONV #( lva_msg_error+150(050) )
            msgty  = 'E'.
      ENDIF.

*-US191846-01.10.2025-#191846-JT-fim

      CHECK sy-subrc = 0.

*------------------------------------
*---- ajuste CFOP / Leis Fiscais
*------------------------------------
      LOOP AT obj_item ASSIGNING FIELD-SYMBOL(<w_item_nota>) WHERE matnr = _zib_nfe_dist_itm-matnr.
        <w_item_nota>-mwskz  = _zmmt0154-mwskz.
        <w_item_nota>-cfop   = _zmmt0154-cfop.
        <w_item_nota>-taxlw1 = _zmmt0154-j_1btaxlw1.
        <w_item_nota>-taxlw2 = _zmmt0154-j_1btaxlw2.
        <w_item_nota>-taxlw4 = _zmmt0154-j_1btaxlw4.
        <w_item_nota>-taxlw5 = _zmmt0154-j_1btaxlw5.
      ENDLOOP.
    ENDLOOP.

*------------------------------------
*-- update NF
*------------------------------------
    CALL FUNCTION 'J_1B_NF_OBJECT_UPDATE'
      EXPORTING
        obj_number        = lc_nfobjn
        obj_header        = obj_header
      TABLES
        obj_partner       = obj_partner
        obj_item          = obj_item
        obj_item_tax      = obj_item_tax
        obj_header_msg    = obj_header_msg
        obj_refer_msg     = obj_refer_msg
        obj_ot_partner    = obj_ot_partner
        obj_import_di     = obj_import_di
        obj_import_adi    = obj_import_adi
        obj_cte_res       = obj_cte_res
        obj_cte_docref    = obj_cte_docref
        obj_trans_volumes = obj_trans_volumes
        obj_trailer_info  = obj_trailer_info
        obj_trade_notes   = obj_trade_notes
        obj_add_info      = obj_add_info
        obj_ref_proc      = obj_ref_proc
        obj_sugar_suppl   = obj_sugar_suppl
        obj_sugar_deduc   = obj_sugar_deduc
        obj_vehicle       = obj_vehicle
        obj_pharmaceut    = obj_pharmaceut
        obj_fuel          = obj_fuel
        obj_export        = obj_export
        obj_nve           = obj_nve
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.

    CHECK sy-subrc IS INITIAL.

    CALL FUNCTION 'J_1B_NF_DOC_UPDATE_FROM_OBJECT'
      EXPORTING
        obj_number         = lc_nfobjn
      EXCEPTIONS
        object_not_found   = 1
        document_not_found = 2
        update_problem     = 3
        docum_lock         = 4
        OTHERS             = 5.

    CALL FUNCTION 'J_1B_NF_OBJECT_DROP'
      EXPORTING
        obj_number       = lc_nfobjn
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

  ENDMETHOD.


  METHOD zif_material_destinacao~set_ajustar_incoterms_nf.

    DATA: lc_nfobjn         TYPE j_1binterf-nfobjn,
          lv_docnum         TYPE j_1bdocnum,
          obj_header        TYPE j_1bnfdoc,
          obj_partner       TYPE TABLE OF j_1bnfnad,
          obj_item          TYPE TABLE OF j_1bnflin,
          obj_item_tax      TYPE TABLE OF j_1bnfstx,
          obj_header_msg    TYPE TABLE OF j_1bnfftx,
          obj_refer_msg     TYPE TABLE OF j_1bnfref,
          obj_ot_partner    TYPE TABLE OF j_1bnfcpd,
          obj_import_di     TYPE TABLE OF j_1bnfimport_di,
          obj_import_adi    TYPE TABLE OF j_1bnfimport_adi,
          obj_cte_res       TYPE TABLE OF j_1bcte_d_res,
          obj_cte_docref    TYPE TABLE OF j_1bcte_d_docref,
          obj_trans_volumes TYPE TABLE OF j_1bnftransvol,
          obj_trailer_info  TYPE TABLE OF j_1bnftrailer,
          obj_trade_notes   TYPE TABLE OF j_1bnftradenotes,
          obj_add_info      TYPE TABLE OF j_1bnfadd_info,
          obj_ref_proc      TYPE TABLE OF j_1bnfrefproc,
          obj_sugar_suppl   TYPE TABLE OF j_1bnfsugarsuppl,
          obj_sugar_deduc   TYPE TABLE OF j_1bnfsugardeduc,
          obj_vehicle       TYPE TABLE OF j_1bnfvehicle,
          obj_pharmaceut    TYPE TABLE OF j_1bnfpharmaceut,
          obj_fuel          TYPE TABLE OF j_1bnffuel,
          obj_export        TYPE TABLE OF j_1bnfe_export,
          obj_nve           TYPE TABLE OF j_1bnfnve.

    r_if_material_destinacao = me.

*------------------------------------
*-- read NF
*------------------------------------
    CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
      EXPORTING
        doc_number         = i_docnum
      IMPORTING
        obj_number         = lc_nfobjn
      EXCEPTIONS
        document_not_found = 1
        docum_lock         = 2
        OTHERS             = 3.

    CALL FUNCTION 'J_1B_NF_OBJECT_READ'
      EXPORTING
        obj_number        = lc_nfobjn
      IMPORTING
        obj_header        = obj_header
      TABLES
        obj_partner       = obj_partner
        obj_item          = obj_item
        obj_item_tax      = obj_item_tax
        obj_header_msg    = obj_header_msg
        obj_refer_msg     = obj_refer_msg
        obj_ot_partner    = obj_ot_partner
        obj_import_di     = obj_import_di
        obj_import_adi    = obj_import_adi
        obj_cte_res       = obj_cte_res
        obj_cte_docref    = obj_cte_docref
        obj_trans_volumes = obj_trans_volumes
        obj_trailer_info  = obj_trailer_info
        obj_trade_notes   = obj_trade_notes
        obj_add_info      = obj_add_info
        obj_ref_proc      = obj_ref_proc
        obj_sugar_suppl   = obj_sugar_suppl
        obj_sugar_deduc   = obj_sugar_deduc
        obj_vehicle       = obj_vehicle
        obj_pharmaceut    = obj_pharmaceut
        obj_fuel          = obj_fuel
        obj_export        = obj_export
        obj_nve           = obj_nve
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.

    CHECK sy-subrc IS INITIAL.

*------------------------------------
*---- ajuste Inco1/Inco2
*------------------------------------
    obj_header-inco1 = i_inco1.
    obj_header-inco2 = i_inco2.

*------------------------------------
*-- update NF
*------------------------------------
    CALL FUNCTION 'J_1B_NF_OBJECT_UPDATE'
      EXPORTING
        obj_number        = lc_nfobjn
        obj_header        = obj_header
      TABLES
        obj_partner       = obj_partner
        obj_item          = obj_item
        obj_item_tax      = obj_item_tax
        obj_header_msg    = obj_header_msg
        obj_refer_msg     = obj_refer_msg
        obj_ot_partner    = obj_ot_partner
        obj_import_di     = obj_import_di
        obj_import_adi    = obj_import_adi
        obj_cte_res       = obj_cte_res
        obj_cte_docref    = obj_cte_docref
        obj_trans_volumes = obj_trans_volumes
        obj_trailer_info  = obj_trailer_info
        obj_trade_notes   = obj_trade_notes
        obj_add_info      = obj_add_info
        obj_ref_proc      = obj_ref_proc
        obj_sugar_suppl   = obj_sugar_suppl
        obj_sugar_deduc   = obj_sugar_deduc
        obj_vehicle       = obj_vehicle
        obj_pharmaceut    = obj_pharmaceut
        obj_fuel          = obj_fuel
        obj_export        = obj_export
        obj_nve           = obj_nve
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.

    CHECK sy-subrc IS INITIAL.

    CALL FUNCTION 'J_1B_NF_DOC_UPDATE_FROM_OBJECT'
      EXPORTING
        obj_number         = lc_nfobjn
      EXCEPTIONS
        object_not_found   = 1
        document_not_found = 2
        update_problem     = 3
        docum_lock         = 4
        OTHERS             = 5.

    CALL FUNCTION 'J_1B_NF_OBJECT_DROP'
      EXPORTING
        obj_number       = lc_nfobjn
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

  ENDMETHOD.


  METHOD zif_material_destinacao~set_gerar_nota_devolucao_job.

    DATA: lv_number    TYPE tbtcjob-jobcount,
          lv_name      TYPE tbtcjob-jobname,
          w_j_1bnflin  TYPE j_1bnflin,
          lv_json_head TYPE string,
          lv_json_item TYPE string,
          lv_json_cfop TYPE string.

    FREE: e_belnr_dev,
          e_gjahr_dev.

    lv_json_head = /ui2/cl_json=>serialize( data = i_headerdata ).
    lv_json_item = /ui2/cl_json=>serialize( data = i_itemdata ).
    lv_json_cfop = /ui2/cl_json=>serialize( data = i_cfop ).

*--------------------------------------------------
*-- for debug
*--------------------------------------------------
    IF 1 = 2.
      SUBMIT zmmr0047 WITH p_chave = i_chave_nfe
                      WITH pjsonhd = lv_json_head
                      WITH pjsonit = lv_json_item
                      WITH pjsoncf = lv_json_cfop
                       AND RETURN.
    ENDIF.

*--------------------------------------------------
*-- criar JOB
*--------------------------------------------------
    DATA(lc_user_job) = zcl_job=>get_user_job( ).

    lv_name = 'JOB_DEVOLUCAO_' && i_chave_nfe.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_name
      IMPORTING
        jobcount         = lv_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc IS INITIAL.
      SUBMIT zmmr0047 VIA JOB lv_name
                       NUMBER lv_number
                         WITH p_chave = i_chave_nfe
                         WITH pjsonhd = lv_json_head
                         WITH pjsonit = lv_json_item
                         WITH pjsoncf = lv_json_cfop
                         USER lc_user_job
                          AND RETURN.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = lv_number
            jobname              = lv_name
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.

        IF sy-subrc IS NOT INITIAL.
          CALL FUNCTION 'BP_JOB_DELETE'
            EXPORTING
              jobcount                 = lv_number
              jobname                  = lv_name
            EXCEPTIONS
              cant_delete_event_entry  = 1
              cant_delete_job          = 2
              cant_delete_joblog       = 3
              cant_delete_steps        = 4
              cant_delete_time_entry   = 5
              cant_derelease_successor = 6
              cant_enq_predecessor     = 7
              cant_enq_successor       = 8
              cant_enq_tbtco_entry     = 9
              cant_update_predecessor  = 10
              cant_update_successor    = 11
              commit_failed            = 12
              jobcount_missing         = 13
              jobname_missing          = 14
              job_does_not_exist       = 15
              job_is_already_running   = 16
              no_delete_authority      = 17
              OTHERS                   = 18.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BP_JOB_DELETE'
          EXPORTING
            jobcount                 = lv_number
            jobname                  = lv_name
          EXCEPTIONS
            cant_delete_event_entry  = 1
            cant_delete_job          = 2
            cant_delete_joblog       = 3
            cant_delete_steps        = 4
            cant_delete_time_entry   = 5
            cant_derelease_successor = 6
            cant_enq_predecessor     = 7
            cant_enq_successor       = 8
            cant_enq_tbtco_entry     = 9
            cant_update_predecessor  = 10
            cant_update_successor    = 11
            commit_failed            = 12
            jobcount_missing         = 13
            jobname_missing          = 14
            job_does_not_exist       = 15
            job_is_already_running   = 16
            no_delete_authority      = 17
            OTHERS                   = 18.
      ENDIF.
    ENDIF.

*--------------------------------------------------
*-- aguardar execução do job
*--------------------------------------------------
    zcl_job=>get_instance(
      )->set_key_job( i_jobname = lv_name i_jobcount = lv_number
      )->get_wait_job_exec(
      ).

*--------------------------------------------------
*-- recuperar Numero doc estorno
*--------------------------------------------------
    DO 5 TIMES.
      SELECT SINGLE belnr_dev, gjahr_dev
        INTO @DATA(_documento)
        FROM zib_nfe_dist_ter
       WHERE chave_nfe = @i_chave_nfe.

      IF sy-subrc = 0 AND _documento-belnr_dev IS NOT INITIAL.
        e_belnr_dev = _documento-belnr_dev.
        e_gjahr_dev = _documento-gjahr_dev.
        RETURN.
      ELSE.
        WAIT UP TO 3 SECONDS.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD zif_material_destinacao~check_parametros_zmm0185.

    DATA: lv_bsart   TYPE esart,
          lc_dados   TYPE zsde0185,
          lc_retorno TYPE zmmt0154_t,
          wc_retorno TYPE zmmt0154.

    r_if_material_destinacao = me.

    SELECT SINGLE *
      INTO @DATA(_zib_nfe_dist_ter)
      FROM zib_nfe_dist_ter
     WHERE chave_nfe = @i_chave_nfe.

    CHECK sy-subrc = 0.

    CASE i_tp_destinacao.
      WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.

        lv_bsart = COND #( WHEN _zib_nfe_dist_ter-ck_trans_nf_propri = abap_true THEN 'ZARM' ELSE 'ZARS' ).

      WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.

        SELECT SINGLE *
          FROM zib_nfe_dist_itm INTO @DATA(lwa_zib_nfe_dist_itm)
         WHERE chave_nfe EQ @i_chave_nfe
           AND ebeln NE @space.

        IF sy-subrc EQ 0.
          SELECT SINGLE *
            FROM ekko INTO @DATA(lwa_ekko_nfe)
           WHERE ebeln EQ @lwa_zib_nfe_dist_itm-ebeln.
        ENDIF.

        lv_bsart = lwa_ekko_nfe-bsart.

      WHEN OTHERS.
        EXIT.

    ENDCASE.

    SELECT chave_nfe, matnr
      INTO TABLE @DATA(t_zib_nfe_dist_itm)
      FROM zib_nfe_dist_itm
     WHERE chave_nfe = @i_chave_nfe.

    CHECK sy-subrc = 0.

    SELECT SINGLE regio
      INTO @DATA(_regio_toma)
      FROM t001w
     WHERE werks = @_zib_nfe_dist_ter-f_tomadora.

    CHECK sy-subrc = 0.

    IF _zib_nfe_dist_ter-f_armazem IS INITIAL.
      SELECT SINGLE regio
        INTO @DATA(_regio_armz)
        FROM lfa1
       WHERE lifnr = @_zib_nfe_dist_ter-p_emissor.
    ELSE.
      SELECT SINGLE regio
        INTO @_regio_armz
        FROM lfa1
       WHERE lifnr = @_zib_nfe_dist_ter-f_armazem.
    ENDIF.

    CHECK sy-subrc = 0.

    SELECT matnr, matkl
      INTO TABLE @DATA(t_mara)
      FROM mara
       FOR ALL ENTRIES IN @t_zib_nfe_dist_itm
     WHERE matnr = @t_zib_nfe_dist_itm-matnr.

    CHECK sy-subrc = 0.

    SORT t_zib_nfe_dist_itm BY matnr.
    DELETE ADJACENT DUPLICATES FROM t_zib_nfe_dist_itm COMPARING matnr.

    LOOP AT t_zib_nfe_dist_itm INTO DATA(_zib_nfe_dist_itm).

      READ TABLE t_mara INTO DATA(_mara) WITH KEY matnr = _zib_nfe_dist_itm-matnr.
      CHECK sy-subrc = 0.

      CLEAR: lc_dados.
      lc_dados-bsart-valor   = lv_bsart.
      lc_dados-uf_orig-valor = _regio_toma.
      lc_dados-uf_dest-valor = _regio_armz.
      lc_dados-matnr-valor   = _mara-matnr.
      lc_dados-bukrs-valor   = _zib_nfe_dist_ter-e_tomadora.
      lc_dados-direcao-valor = '2'. "saida

      lc_retorno = zcl_leis_fiscais=>get_impostos( i_dados = lc_dados i_todos = abap_false ).

      READ TABLE lc_retorno INTO DATA(_zmmt0154) INDEX 1.

      IF sy-subrc NE 0.
        DATA: lva_msg_error TYPE c LENGTH 200.

        lva_msg_error = |Não encontrado parametros com IVA na ZMM0185 para o tipo Pedido: { lv_bsart } Origem: { _regio_toma }|.
        lva_msg_error = |{ lva_msg_error } Destino: { _regio_armz } Material: { _mara-matnr } Empresa: { _zib_nfe_dist_ter-e_tomadora } Direção: Saída!|.
        lva_msg_error = |{ lva_msg_error } Criar FI para Departamento Fiscal|.

        RAISE EXCEPTION TYPE zcx_material_destinacao
          EXPORTING
            textid = VALUE #( msgid = zcx_material_destinacao=>zcx_erro_geral-msgid
                              msgno = zcx_material_destinacao=>zcx_erro_geral-msgno
                              attr1 = CONV #( lva_msg_error+000(050) )
                              attr2 = CONV #( lva_msg_error+050(050) )
                              attr3 = CONV #( lva_msg_error+100(050) )
                              attr4 = CONV #( lva_msg_error+150(050) )
                               )
            msgid  = zcx_material_destinacao=>zcx_erro_geral-msgid
            msgno  = zcx_material_destinacao=>zcx_erro_geral-msgno
            msgv1  = CONV #( lva_msg_error+000(050) )
            msgv2  = CONV #( lva_msg_error+050(050) )
            msgv3  = CONV #( lva_msg_error+100(050) )
            msgv4  = CONV #( lva_msg_error+150(050) )
            msgty  = 'E'.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
