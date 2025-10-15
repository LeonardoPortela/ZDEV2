class ZCL_NFE_XML definition
  public
  final
  create public .

public section.

  interfaces ZIF_NFE_XML .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NFE_XML IMPLEMENTATION.


  METHOD ZIF_NFE_XML~GET_CK_CFOP_EXPORTACAO.

    R_IF_NFE_XML = ME.

    DATA: LC_CK_CFOP_EXPORTACAO TYPE CHAR01.
    LC_CK_CFOP_EXPORTACAO = ABAP_FALSE.

    LOOP AT ME->ZIF_NFE_XML~IB_NFE_ITM INTO DATA(WA_ITEM).

      SELECT SINGLE * INTO @DATA(WA_ZMMT0108)
        FROM ZMMT0108
       WHERE CFOP EQ @WA_ITEM-PROD_CFOP.

      IF SY-SUBRC IS INITIAL.
        LC_CK_CFOP_EXPORTACAO = ABAP_TRUE.
      ENDIF.

    ENDLOOP.

    IF LC_CK_CFOP_EXPORTACAO EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_NFE_XML
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_XML=>ZCX_NFE_NAO_EXPORTACAO-MSGID
                            MSGNO = ZCX_NFE_XML=>ZCX_NFE_NAO_EXPORTACAO-MSGNO )
          MSGID  = ZCX_NFE_XML=>ZCX_NFE_NAO_EXPORTACAO-MSGID
          MSGNO  = ZCX_NFE_XML=>ZCX_NFE_NAO_EXPORTACAO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_nfe_xml~get_ck_ncm_exportacao.

    r_if_nfe_xml = me.

    DATA: lc_ck_ncm_exportacao TYPE char01,
          lc_ncm               TYPE char10.

    lc_ck_ncm_exportacao = abap_false.

    LOOP AT me->zif_nfe_xml~ib_nfe_itm INTO DATA(wa_item).

      CLEAR: lc_ncm.

      IF zcl_string=>length( text = CONV #( wa_item-prod_ncm ) ) LT 8.
        CONTINUE.
      ENDIF.

      CONCATENATE wa_item-prod_ncm(4) '.' wa_item-prod_ncm+4(2) '.' wa_item-prod_ncm+6(2) INTO lc_ncm.

      IF i_grupo_material IS NOT INITIAL.

        SELECT SINGLE * INTO @DATA(wa_zmmt0108)
          FROM zmmt0107
         WHERE matkl EQ @i_grupo_material
           AND direcao EQ 'E' " User Story 151256 - AOENNING
           AND nbm   EQ @lc_ncm.

      ELSEIF i_material IS NOT INITIAL.

        SELECT SINGLE matkl INTO @DATA(lc_grupo_material)
          FROM mara
         WHERE matnr EQ @i_material.

        IF sy-subrc IS INITIAL.
          SELECT SINGLE * INTO @wa_zmmt0108
            FROM zmmt0107
           WHERE matkl EQ @lc_grupo_material
             AND direcao EQ 'E' " User Story 151256 - AOENNING
             AND nbm   EQ @lc_ncm.
        ENDIF.

      ELSE.
        SELECT SINGLE * INTO @wa_zmmt0108
          FROM zmmt0107
         WHERE nbm EQ @lc_ncm
           AND direcao EQ 'E'. " User Story 151256 - AOENNING
      ENDIF.

      IF sy-subrc IS INITIAL.
        lc_ck_ncm_exportacao = abap_true.

        TRANSLATE wa_item-prod_und_trib TO UPPER CASE.

        IF wa_zmmt0108-und_trib NE wa_item-prod_und_trib.
          RAISE EXCEPTION TYPE zcx_nfe_xml
            EXPORTING
              textid = VALUE #( msgid = zcx_nfe_xml=>zcx_nfe_und_exp_errada-msgid
                                msgno = zcx_nfe_xml=>zcx_nfe_und_exp_errada-msgno
                                attr1 = wa_item-prod_und_trib
                                attr2 = wa_zmmt0108-und_trib )
              msgid  = zcx_nfe_xml=>zcx_nfe_und_exp_errada-msgid
              msgno  = zcx_nfe_xml=>zcx_nfe_und_exp_errada-msgno
              msgty  = 'E'
              msgv1  = CONV #( wa_item-prod_und_trib )
              msgv2  = CONV #( wa_zmmt0108-und_trib ).
        ENDIF.

      ENDIF.

    ENDLOOP.

    IF lc_ck_ncm_exportacao EQ abap_false.
      RAISE EXCEPTION TYPE zcx_nfe_xml
        EXPORTING
          textid = VALUE #( msgid = zcx_nfe_xml=>zcx_nfe_nao_exportacao-msgid
                            msgno = zcx_nfe_xml=>zcx_nfe_nao_exportacao-msgno )
          msgid  = zcx_nfe_xml=>zcx_nfe_nao_exportacao-msgid
          msgno  = zcx_nfe_xml=>zcx_nfe_nao_exportacao-msgno
          msgty  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_NFE_XML~GET_CK_NFE_AUTORIZADA.

    R_IF_NFE_XML = ME.

    IF ME->ZIF_NFE_XML~IB_NFE-CANCEL EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_NFE_XML
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_XML=>ZCX_NFE_CANCELADA-MSGID
                            MSGNO = ZCX_NFE_XML=>ZCX_NFE_CANCELADA-MSGNO )
          MSGID  = ZCX_NFE_XML=>ZCX_NFE_CANCELADA-MSGID
          MSGNO  = ZCX_NFE_XML=>ZCX_NFE_CANCELADA-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ME->ZIF_NFE_XML~IB_NFE-CD_MSG_SEFAZ NE '100'.
      RAISE EXCEPTION TYPE ZCX_NFE_XML
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_XML=>ZCX_NFE_NAO_AUTORIZADA_USO-MSGID
                            MSGNO = ZCX_NFE_XML=>ZCX_NFE_NAO_AUTORIZADA_USO-MSGNO )
          MSGID  = ZCX_NFE_XML=>ZCX_NFE_NAO_AUTORIZADA_USO-MSGID
          MSGNO  = ZCX_NFE_XML=>ZCX_NFE_NAO_AUTORIZADA_USO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_NFE_XML~GET_CK_NFE_LOCALIZADO.

    R_IF_NFE_XML = ME.

    IF ME->ZIF_NFE_XML~IB_NFE IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_NFE_XML
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_XML=>ZCX_NFE_NAO_LOCALIZADO-MSGID
                            MSGNO = ZCX_NFE_XML=>ZCX_NFE_NAO_LOCALIZADO-MSGNO )
          MSGID  = ZCX_NFE_XML=>ZCX_NFE_NAO_LOCALIZADO-MSGID
          MSGNO  = ZCX_NFE_XML=>ZCX_NFE_NAO_LOCALIZADO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_NFE_XML~GET_INSTANCE.

    IF ZIF_NFE_XML~AT_NFE_XML IS NOT BOUND.
      CREATE OBJECT ZIF_NFE_XML~AT_NFE_XML TYPE ZCL_NFE_XML.
    ENDIF.
    R_IF_NFE_XML = ZIF_NFE_XML~AT_NFE_XML.

  ENDMETHOD.


  METHOD ZIF_NFE_XML~GET_VALIDAR.

    R_IF_NFE_XML = ME.

    CLEAR: E_VALIDACAO.

    "Valida se Documento Está na Base do SAP
    TRY .
        ME->ZIF_NFE_XML~GET_CK_NFE_LOCALIZADO( ).
        E_VALIDACAO-CK_LOCALIZADO = ABAP_TRUE.
      CATCH ZCX_NFE_XML INTO DATA(EX_NFE_XML).
        E_VALIDACAO-CK_ERRO = ABAP_TRUE.
        E_VALIDACAO-DS_MESSAGEM = EX_NFE_XML->ZIF_ERROR~GET_MSG_ERRO( ).
    ENDTRY.

    CHECK E_VALIDACAO-CK_ERRO EQ ABAP_FALSE.

    DESCRIBE TABLE ME->ZIF_NFE_XML~IB_NFE_ITM LINES E_VALIDACAO-NM_QTD_ITENS.

    "Valida de Documento está autorizado
    TRY.
        ME->ZIF_NFE_XML~GET_CK_NFE_AUTORIZADA( ).
        E_VALIDACAO-CK_AUTORIZADO = ABAP_TRUE.
      CATCH ZCX_NFE_XML INTO EX_NFE_XML.
        E_VALIDACAO-CK_ERRO = ABAP_TRUE.
        E_VALIDACAO-DS_MESSAGEM = EX_NFE_XML->ZIF_ERROR~GET_MSG_ERRO( ).
    ENDTRY.

    CHECK E_VALIDACAO-CK_ERRO EQ ABAP_FALSE.

    "Valida se Documento é para Exportação Conforme CFOP's
    "Valida de NCM da Nota Fiscal é para Exportação
    TRY.
        ME->ZIF_NFE_XML~GET_CK_CFOP_EXPORTACAO(  ).
        E_VALIDACAO-CK_EXPORTACAO = ABAP_TRUE.

        TRY.
            ME->ZIF_NFE_XML~GET_CK_NCM_EXPORTACAO( I_MATERIAL = I_MATERIAL I_GRUPO_MATERIAL = I_GRUPO_MATERIAL ).
            E_VALIDACAO-CK_NCM_EXPORTACAO = ABAP_TRUE.
          CATCH ZCX_NFE_XML INTO EX_NFE_XML.
            E_VALIDACAO-DS_MESSAGEM = EX_NFE_XML->ZIF_ERROR~GET_MSG_ERRO( ).
            E_VALIDACAO-CK_ERRO = ABAP_TRUE.
        ENDTRY.

        CHECK E_VALIDACAO-CK_ERRO EQ ABAP_FALSE.
      CATCH ZCX_NFE_XML .
    ENDTRY.

    CHECK E_VALIDACAO-CK_ERRO EQ ABAP_FALSE.


  ENDMETHOD.


  METHOD ZIF_NFE_XML~GET_WEB_SERVICE_VALIDAR.

    TYPES BEGIN OF TY_SOLICITACAO.
    TYPES: CHAVE TYPE STRING.
    TYPES: MATNR TYPE STRING.
    TYPES: MATKL TYPE STRING.
    TYPES END OF TY_SOLICITACAO.

    DATA: LC_SOLICITACAO TYPE TY_SOLICITACAO.

    R_IF_NFE_XML = ME.

    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_JSON CHANGING DATA = LC_SOLICITACAO ).

    ME->ZIF_NFE_XML~SET_REGISTRO( I_CHAVE = CONV #( LC_SOLICITACAO-CHAVE )
      )->GET_VALIDAR(
      EXPORTING
        I_MATERIAL       = CONV #( ZCL_STRING=>LPAD( I_STR = LC_SOLICITACAO-MATNR I_QTD  = 18 I_CHAR = '0' ) )   " Nº do material
        I_GRUPO_MATERIAL = CONV #( ZCL_STRING=>LPAD( I_STR = LC_SOLICITACAO-MATKL I_QTD  = 09 I_CHAR = '0' ) )   " Grupo de mercadorias
      IMPORTING
        E_VALIDACAO      = DATA(E_VALIDACAO)    " Estrutura de Retorno de Validação de XML de NF-e
      ).

    E_JSON = ZCL_FMCALL_BASE=>ABAP2JSON( ABAP_DATA = E_VALIDACAO ).

  ENDMETHOD.


  METHOD ZIF_NFE_XML~SET_CLEAR.

    CLEAR: ME->ZIF_NFE_XML~IB_NFE,
           ME->ZIF_NFE_XML~IB_NFE_ITM,
           ME->ZIF_NFE_XML~IB_NFE_ITM[].

    R_IF_NFE_XML = ME.

  ENDMETHOD.


  METHOD ZIF_NFE_XML~SET_REGISTRO.

    R_IF_NFE_XML = ME->ZIF_NFE_XML~SET_CLEAR( ).

    SELECT SINGLE * INTO @ME->ZIF_NFE_XML~IB_NFE
      FROM ZIB_NFE_DIST_TER
     WHERE CHAVE_NFE EQ @I_CHAVE.

    IF SY-SUBRC IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(WA_ZIB_NFE_ERRO)
        FROM ZIB_DFE_ERRO
       WHERE CHAVE EQ @I_CHAVE.

      IF SY-SUBRC IS INITIAL.
        MESSAGE S398(00)
        WITH 'XML recebido com erro!' WA_ZIB_NFE_ERRO-DS_ERRO+000(050) WA_ZIB_NFE_ERRO-DS_ERRO+050(050) WA_ZIB_NFE_ERRO-DS_ERRO+100(150).
      ELSE.
        MESSAGE S103(ZNFE_DISTRI) WITH I_CHAVE.
      ENDIF.

      RAISE EXCEPTION TYPE ZCX_NFE_XML
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1.

    ENDIF.

    SELECT * INTO TABLE @ME->ZIF_NFE_XML~IB_NFE_ITM
      FROM ZIB_NFE_DIST_ITM
     WHERE CHAVE_NFE EQ @I_CHAVE.

  ENDMETHOD.
ENDCLASS.
