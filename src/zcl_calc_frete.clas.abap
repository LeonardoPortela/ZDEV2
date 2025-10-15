class ZCL_CALC_FRETE definition
  public
  final
  create public .

public section.

  class-methods CALCULAR_VALOR_NOTA_CONTAINER
    importing
      !QT_CONTAINER type I
      !QT_PROD_CONTAINER type ZDE_QUANT_CONTAINE
      !QT_PROD_NOTA type ZDE_QUANT_CONTAINE
      !VL_CONTAINER type KBETR
    returning
      value(R_VALOR_FRETE_NOTA) type KBETR .
  class-methods GET_VALOR_ADIANTAMENTO
    importing
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
      !I_LIFNR type TDLNR optional
    returning
      value(R_MARGADTO) type ZMARGADTO
    raising
      ZCX_CALC_FRETE .
  class-methods GET_VALOR_FRETE
    importing
      !I_KAPPL type KAPPL default 'F'
      !I_KSCHL type KSCHA default 'ZFRE'
      !I_ROUTE type ROUTE optional
      !I_TDLNR type TDLNR
      !I_SHTYP type SHTYP
      !I_LZONEA type LZONEA
      !I_LZONEZ type LZONEZ
      !I_VEGR5 type VEGR5 optional
      !I_ADD01 type VTTK_ADD01 optional
      !I_MATNR type MATNR optional
      !I_ORDEM_VENDA type VBELN optional
      !I_ID_ORDEM type ZDE_ID_ORDEM optional
      !I_PLACA_TRATOR type ZDE_PLACA_TRATOR optional
      !I_ID_CIDADE_BASE type ZDE_CIDADE_BASE optional
      !I_DATA_REFERENCIA type DATUM default SY-DATUM
      !I_FRETE_ENTRADA type CHAR01 default ABAP_FALSE
      !I_VSTEL type VSTEL optional
      !I_ID_CARGUERO type ZDE_ID_CARGUERO optional
      !I_VIAGEM_ID type ZLEST0185-VIAGEM_ID optional
    exporting
      !E_KBETR type KBETR_KOND
      !E_KONWA type KONWA
      !E_KRECH type KRECH
      !E_TEXTO type STRING
      !E_LZONEA type LZONEA
      !E_LZONEZ type LZONEZ
      !E_ROUTE type ROUTE
    raising
      ZCX_CALC_FRETE .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_CALC_FRETE .
  class-methods GET_CIDADE_TABELA_MESORREGIAO
    importing
      !I_AGENTE_FRETE type LIFNR
      !I_PONTO_COLETA type LIFNR
    exporting
      !E_DS_CIDADE_BASE type ZDE_DS_MESORREGIAO
    returning
      value(R_ID_CIDADE_BASE) type ZDE_CIDADE_BASE
    raising
      ZCX_CALC_FRETE .
  class-methods GET_CONDICAO_ADICIONAL
    importing
      !I_KAPPL type KAPPL default 'F'
      !I_KSCHL type KSCHA default 'ZFRE'
      !I_TDLNR type TDLNR
      !I_SHTYP type SHTYP
      !I_TPLST type TPLST
      !I_DATA_REFERENCIA type DATUM default SY-DATUM
    exporting
      !E_KBETR type KBETR_KOND
      !E_KONWA type KONWA
      !E_KRECH type KRECH
    raising
      ZCX_CALC_FRETE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CALC_FRETE IMPLEMENTATION.


  METHOD CALCULAR_VALOR_NOTA_CONTAINER.

    "Valor do Container vezes a quantidade de containers pelo percentual da nota fiscal na carga ( 5700 * 1 ) * (   1.406,7464 / 27.485,3427  ) =  R$ 291,74
    "Peso Total: 27.485,3427 (Peso do Container)  R$ 5.700,00 (Valor Container)
    "Peso Nota:   1.406,7464    0,0512    R$   291,74
    "             5.458,1639    0,1986    R$ 1.131,93
    "             5.343,0000    0,1944    R$ 1.108,05
    "------------------------------------------------
    "            12.207,9103    0,4442    R$ 2.531,72

    R_VALOR_FRETE_NOTA = ( QT_CONTAINER * VL_CONTAINER ) * ( QT_PROD_NOTA / QT_PROD_CONTAINER ).

  ENDMETHOD.


  METHOD GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_CALC_FRETE
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_CALC_FRETE=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_CALC_FRETE=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_CALC_FRETE=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_CALC_FRETE=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD GET_CIDADE_TABELA_MESORREGIAO.

    CLEAR: R_ID_CIDADE_BASE, E_DS_CIDADE_BASE.

    "01	Logística - Tabelamento de Preço Ferroviário
    SELECT SINGLE * INTO @DATA(WA_ZLEST0187)
      FROM ZLEST0187 AS B
     WHERE B~TP_REGIAO = '01'
       AND EXISTS ( SELECT * FROM ZLEST0191 AS A WHERE A~LIFNR EQ @I_AGENTE_FRETE AND A~ID_REGIAO EQ B~ID_REGIAO ).

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CALC_FRETE
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CALC_FRETE=>ZCX_FORN_SEM_TAB_PRECO-MSGID
                            MSGNO = ZCX_CALC_FRETE=>ZCX_FORN_SEM_TAB_PRECO-MSGNO
                            ATTR1 = CONV #( I_AGENTE_FRETE ) )
          MSGID  = ZCX_CALC_FRETE=>ZCX_FORN_SEM_TAB_PRECO-MSGID
          MSGNO  = ZCX_CALC_FRETE=>ZCX_FORN_SEM_TAB_PRECO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_AGENTE_FRETE ).
    ENDIF.

    SELECT SINGLE TXJCD INTO @DATA(LC_TXJCD)
      FROM LFA1
     WHERE LIFNR EQ @I_PONTO_COLETA.

    "Procura Domicilio como Base
    SELECT SINGLE * INTO @DATA(WA_ZLEST0189)
      FROM ZLEST0189
     WHERE ID_CIDADE_BASE EQ @LC_TXJCD
       AND ID_REGIAO      EQ @WA_ZLEST0187-ID_REGIAO.

    IF SY-SUBRC IS NOT INITIAL.

      "Procura Domicilio como Base
      SELECT SINGLE * INTO @DATA(WA_ZLEST0190)
        FROM ZLEST0190
       WHERE ID_CIDADE EQ @LC_TXJCD
         AND ID_REGIAO EQ @WA_ZLEST0187-ID_REGIAO.

      IF SY-SUBRC IS INITIAL.
        R_ID_CIDADE_BASE = WA_ZLEST0190-ID_CIDADE_BASE.

        SELECT SINGLE * INTO @WA_ZLEST0189
          FROM ZLEST0189
         WHERE ID_REGIAO      EQ @WA_ZLEST0190-ID_REGIAO
           AND COUNTRY_BASE   EQ @WA_ZLEST0190-COUNTRY_BASE
           AND ID_CIDADE_BASE EQ @WA_ZLEST0190-ID_CIDADE_BASE.

        IF SY-SUBRC IS INITIAL.
          E_DS_CIDADE_BASE = WA_ZLEST0189-DS_MESORREGIAO.
        ENDIF.

      ENDIF.

    ELSE.

      R_ID_CIDADE_BASE = WA_ZLEST0189-ID_CIDADE_BASE.
      E_DS_CIDADE_BASE = WA_ZLEST0189-DS_MESORREGIAO.

    ENDIF.

    CHECK R_ID_CIDADE_BASE IS INITIAL OR E_DS_CIDADE_BASE IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_CALC_FRETE
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_CALC_FRETE=>ZCX_FORN_PC_SEM_TAB_PRECO-MSGID
                          MSGNO = ZCX_CALC_FRETE=>ZCX_FORN_PC_SEM_TAB_PRECO-MSGNO
                          ATTR1 = CONV #( I_AGENTE_FRETE )
                          ATTR2 = CONV #( I_PONTO_COLETA ) )
        MSGID  = ZCX_CALC_FRETE=>ZCX_FORN_PC_SEM_TAB_PRECO-MSGID
        MSGNO  = ZCX_CALC_FRETE=>ZCX_FORN_PC_SEM_TAB_PRECO-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( I_AGENTE_FRETE )
        MSGV2  = CONV #( I_PONTO_COLETA ).

  ENDMETHOD.


  METHOD GET_VALOR_ADIANTAMENTO.

    R_MARGADTO = 0.

    IF I_LIFNR IS NOT INITIAL.

      TRY .

          DATA(WA_LFA1_FORNCEDOR) =
          CAST ZCL_FORNECEDORES(
          ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
            )->SET_PARCEIRO( I_PARCEIRO = I_LIFNR
            ) )->AT_LFA1.

          IF WA_LFA1_FORNCEDOR-KTOKK EQ 'ZFIC'.
            R_MARGADTO = 0.
            EXIT.
          ENDIF.

        CATCH ZCX_PARCEIROS.
      ENDTRY.

      SELECT SINGLE MARGADTO INTO @R_MARGADTO
        FROM ZLEST0103
       WHERE BUKRS  EQ @I_BUKRS
         AND BRANCH EQ @I_BRANCH
         AND TDLNR  EQ @I_LIFNR.
    ENDIF.

    CHECK SY-SUBRC IS NOT INITIAL OR I_LIFNR IS INITIAL.

    SELECT SINGLE MARGADTO INTO @R_MARGADTO
      FROM ZLEST0103
     WHERE BUKRS  EQ @I_BUKRS
       AND BRANCH EQ @I_BRANCH
       AND TDLNR  EQ @SPACE.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_CALC_FRETE
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_CALC_FRETE=>ZCX_SEM_MARGEM_ADIANTAMENTO-MSGID
                          MSGNO = ZCX_CALC_FRETE=>ZCX_SEM_MARGEM_ADIANTAMENTO-MSGNO
                          ATTR1 = CONV #( I_BUKRS )
                          ATTR2 = CONV #( I_BRANCH ) )
        MSGID  = ZCX_CALC_FRETE=>ZCX_SEM_MARGEM_ADIANTAMENTO-MSGID
        MSGNO  = ZCX_CALC_FRETE=>ZCX_SEM_MARGEM_ADIANTAMENTO-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( I_BUKRS )
        MSGV2  = CONV #( I_BRANCH ).

  ENDMETHOD.


  METHOD get_valor_frete.

    "DATA: LC_LZONEA  TYPE LZONEA,
    "      LC_LZONEZ  TYPE LZONEZ,
    "      LC_ROUTE  TYPE ROUTE.

    DATA: v_cont_fre      TYPE i,
          e_vlr_frete_neg TYPE  zvalor_frete,
          e_data_text     TYPE c LENGTH 10,
          l_add01(4)      TYPE n.

    e_route  = i_route.
    e_lzonea = i_lzonea.
    e_lzonez = i_lzonez.

    "Reatribuição das zonas de transporte provocada por implementações "Z"
    IF i_ordem_venda IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(lc_vbak)
        FROM vbak
       WHERE vbeln EQ @i_ordem_venda.

      IF sy-subrc IS INITIAL.
        e_lzonea = COND string( WHEN lc_vbak-zlzone_pc IS NOT INITIAL THEN lc_vbak-zlzone_pc ELSE e_lzonea ).
        IF i_frete_entrada EQ abap_false.
          e_lzonez = COND string( WHEN lc_vbak-zlzone_lr IS NOT INITIAL THEN lc_vbak-zlzone_lr ELSE e_lzonez ).
        ENDIF.
      ENDIF.
    ENDIF.

    "Reatribuição de Itinerário provocado por implementações "Z"
    IF e_route IS NOT INITIAL AND ( e_lzonea NE i_lzonea OR e_lzonez NE i_lzonez ).
      TRY .
          zcl_itinerario=>zif_itinerario~get_instance(
            )->get_itinerario_zonas( EXPORTING i_azone = e_lzonea i_lzone = e_lzonez IMPORTING e_tvro = DATA(e_tvro)
            ).

          e_route = e_tvro-route.
        CATCH zcx_itinerario INTO DATA(ex_itinerario).
          RAISE EXCEPTION TYPE zcx_calc_frete
            EXPORTING
              textid = VALUE #( msgno = ex_itinerario->msgno
                                msgid = ex_itinerario->msgid
                                attr1 = ex_itinerario->msgv1
                                attr2 = ex_itinerario->msgv2
                                attr3 = ex_itinerario->msgv3
                                attr4 = ex_itinerario->msgv4 )
              msgty  = 'E'
              msgno  = ex_itinerario->msgno
              msgv1  = ex_itinerario->msgv1
              msgv2  = ex_itinerario->msgv2
              msgv3  = ex_itinerario->msgv3
              msgv4  = ex_itinerario->msgv4
              msgid  = ex_itinerario->msgid.
      ENDTRY.

    ENDIF.

    WRITE i_data_referencia TO e_data_text.
    e_texto = zcl_string=>concat( s1 = 'DtRef' s2 = CONV #( e_data_text ) sp = ':' ).

    IF e_route IS NOT INITIAL.
      e_texto = zcl_string=>concat( s1 = e_texto s2 = 'Itinerário' sp = '/' ).
      e_texto = zcl_string=>concat( s1 = e_texto s2 = CONV #( e_route ) sp = ':' ).
    ENDIF.

    IF i_tdlnr IS NOT INITIAL.
      DATA: lc_tdlnr TYPE tdlnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = i_tdlnr
        IMPORTING
          output = lc_tdlnr.

      e_texto = zcl_string=>concat( s1 = e_texto s2 = 'ForncServ' sp = '/' ).
      e_texto = zcl_string=>concat( s1 = e_texto s2 = CONV #( lc_tdlnr ) sp = ':' ).
    ENDIF.

    IF i_shtyp IS NOT INITIAL.
      e_texto = zcl_string=>concat( s1 = e_texto s2 = 'TpTransp' sp = '/' ).
      e_texto = zcl_string=>concat( s1 = e_texto s2 = CONV #( i_shtyp ) sp = ':' ).
    ENDIF.

    IF e_lzonea IS NOT INITIAL.
      e_texto = zcl_string=>concat( s1 = e_texto s2 = 'ZPatida' sp = '/' ).
      e_texto = zcl_string=>concat( s1 = e_texto s2 = CONV #( e_lzonea ) sp = ':' ).
    ENDIF.

    IF e_lzonez IS NOT INITIAL.
      e_texto = zcl_string=>concat( s1 = e_texto s2 = 'ZChegada' sp = '/' ).
      e_texto = zcl_string=>concat( s1 = e_texto s2 = CONV #( e_lzonez ) sp = ':' ).
    ENDIF.

    IF i_add01 IS NOT INITIAL.
      DATA: lc_add01 TYPE vttk_add01.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = i_add01
        IMPORTING
          output = lc_add01.
      e_texto = zcl_string=>concat( s1 = e_texto s2 = 'Suplem1' sp = '/' ).
      e_texto = zcl_string=>concat( s1 = e_texto s2 = CONV #( lc_add01 ) sp = ':' ).
    ENDIF.

    IF i_matnr IS NOT INITIAL.
      DATA: lc_matnr  TYPE matnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = i_matnr
        IMPORTING
          output = lc_matnr.
      e_texto = zcl_string=>concat( s1 = e_texto s2 = 'Mat' sp = '/' ).
      e_texto = zcl_string=>concat( s1 = e_texto s2 = CONV #( lc_matnr ) sp = ':' ).
    ENDIF.

    IF i_ordem_venda IS NOT INITIAL.
      e_texto = zcl_string=>concat( s1 = e_texto s2 = 'OV' sp = '/' ).
      e_texto = zcl_string=>concat( s1 = e_texto s2 = CONV #( i_ordem_venda ) sp = ':' ).
    ENDIF.

    IF i_placa_trator IS NOT INITIAL.
      e_texto = zcl_string=>concat( s1 = e_texto s2 = 'Placa' sp = '/' ).
      e_texto = zcl_string=>concat( s1 = e_texto s2 = CONV #( i_placa_trator ) sp = ':' ).
    ENDIF.

    IF i_id_cidade_base IS NOT INITIAL.
      e_texto = zcl_string=>concat( s1 = e_texto s2 = 'Cidade' sp = '/' ).
      e_texto = zcl_string=>concat( s1 = e_texto s2 = CONV #( i_id_cidade_base ) sp = ':' ).
    ENDIF.

    " Tipo de transporte/ ForncServ./ Itinerário/ Suplem.
    IF e_route IS NOT INITIAL AND i_add01 IS NOT INITIAL.
      SELECT *
        FROM a900
        INTO TABLE @DATA(it_a900)
       WHERE kappl EQ @i_kappl
         AND kschl EQ @i_kschl
         AND shtyp EQ @i_shtyp
         AND tdlnr EQ @i_tdlnr
         AND route EQ @e_route
         AND add01 EQ @i_add01
         AND kfrst EQ @space
         AND datab LE @i_data_referencia
         AND datbi GE @i_data_referencia.

      IF it_a900[] IS NOT INITIAL.
        SELECT *
          FROM konp INTO TABLE @DATA(it_konp)
           FOR ALL ENTRIES IN @it_a900
         WHERE knumh    EQ @it_a900-knumh
           AND loevm_ko EQ @space.
      ENDIF.
    ENDIF.

*-CS2020000700 - jtassoni - 21.09.2020 - inicio
    IF i_vegr5 IS NOT INITIAL.
      SELECT *
       FROM a939 INTO TABLE @DATA(it_a939)
      WHERE kappl  EQ @i_kappl
        AND kschl  EQ @i_kschl
        AND shtyp  EQ @i_shtyp
        AND tdlnr  EQ @i_tdlnr
        AND lzonea EQ @e_lzonea
        AND lzonez EQ @e_lzonez
        AND vegr5  EQ @i_vegr5
        AND kfrst  EQ @space
        AND datab  LE @i_data_referencia
        AND datbi  GE @i_data_referencia.

      IF it_a939[] IS NOT INITIAL.
        SELECT *
          FROM konp APPENDING TABLE @it_konp
           FOR ALL ENTRIES IN @it_a939
         WHERE knumh    EQ @it_a939-knumh
           AND loevm_ko EQ @space.
      ENDIF.
    ENDIF.

    IF i_vegr5 IS NOT INITIAL AND
       e_route IS NOT INITIAL.
      SELECT *
        FROM a940 INTO TABLE @DATA(it_a940)
       WHERE kappl EQ @i_kappl
         AND kschl EQ @i_kschl
         AND shtyp EQ @i_shtyp
         AND tdlnr EQ @i_tdlnr
         AND route EQ @e_route
         AND vegr5 EQ @i_vegr5
         AND kfrst EQ @space
         AND datab LE @i_data_referencia
         AND datbi GE @i_data_referencia.

      IF it_a940[] IS NOT INITIAL.
        SELECT *
          FROM konp APPENDING TABLE @it_konp
           FOR ALL ENTRIES IN @it_a940
         WHERE knumh    EQ @it_a940-knumh
           AND loevm_ko EQ @space.
      ENDIF.
    ENDIF.

    IF it_a939[] IS INITIAL AND it_a940[] IS INITIAL.
      " Tp.transp./ForncServ./Zona part./Zona cheg.
      SELECT *
       FROM a910 INTO TABLE @DATA(it_a910)
      WHERE kappl  EQ @i_kappl
        AND kschl  EQ @i_kschl
        AND shtyp  EQ @i_shtyp
        AND tdlnr  EQ @i_tdlnr
        AND lzonea EQ @e_lzonea
        AND lzonez EQ @e_lzonez
        AND kfrst  EQ @space
        AND datab LE @i_data_referencia
        AND datbi GE @i_data_referencia.

      IF it_a910[] IS NOT INITIAL.
        SELECT *
          FROM konp APPENDING TABLE @it_konp
           FOR ALL ENTRIES IN @it_a910
         WHERE knumh    EQ @it_a910-knumh
           AND loevm_ko EQ @space.
      ENDIF.
    ENDIF.
*-CS2020000700 - jtassoni - 21.09.2020 - inicio

    IF ( e_route IS NOT INITIAL ) AND ( it_a940[] IS INITIAL ).
      "Tp.transp./ForncServ./ItinTransp/Contrato
      SELECT *
        FROM a911 INTO TABLE @DATA(it_a911)
       WHERE kappl EQ @i_kappl
         AND kschl EQ @i_kschl
         AND shtyp EQ @i_shtyp
         AND tdlnr EQ @i_tdlnr
         AND route EQ @e_route
         AND kfrst EQ @space
         AND datab LE @i_data_referencia
         AND datbi GE @i_data_referencia.

      IF it_a911[] IS NOT INITIAL.
        SELECT *
          FROM konp APPENDING TABLE @it_konp
           FOR ALL ENTRIES IN @it_a911
         WHERE knumh    EQ @it_a911-knumh
           AND loevm_ko EQ @space.
      ENDIF.
    ENDIF.

    IF i_add01 IS NOT INITIAL.
      " Tp.transp./ForncServ./Zona part./Zona cheg./Agregado
      SELECT *
       FROM a915 INTO TABLE @DATA(it_a915)
      WHERE kappl  EQ @i_kappl
        AND kschl  EQ @i_kschl
        AND shtyp  EQ @i_shtyp
        AND tdlnr  EQ @i_tdlnr
        AND lzonea EQ @e_lzonea
        AND lzonez EQ @e_lzonez
        AND add01  EQ @i_add01
        AND kfrst  EQ @space
        AND datab  LE @i_data_referencia
        AND datbi  GE @i_data_referencia.

      IF it_a915[] IS NOT INITIAL.
        SELECT *
          FROM konp APPENDING TABLE @it_konp
           FOR ALL ENTRIES IN @it_a915
         WHERE knumh    EQ @it_a915-knumh
           AND loevm_ko EQ @space.
      ENDIF.
    ENDIF.

    IF i_add01 IS NOT INITIAL AND i_matnr IS NOT INITIAL.
      " Tp.transp./ForncServ./Material/Zona part./Zona cheg./Suplem.
      SELECT *
       FROM a918 INTO TABLE @DATA(it_a918)
      WHERE kappl  EQ @i_kappl
        AND kschl  EQ @i_kschl
        AND shtyp  EQ @i_shtyp
        AND tdlnr  EQ @i_tdlnr
        AND matnr  EQ @i_matnr
        AND lzonea EQ @e_lzonea
        AND lzonez EQ @e_lzonez
        AND add01  EQ @i_add01
        AND kfrst  EQ @space
        AND datab  LE @i_data_referencia
        AND datbi  GE @i_data_referencia.

      IF it_a918[] IS NOT INITIAL.
        SELECT *
          FROM konp APPENDING TABLE @it_konp
           FOR ALL ENTRIES IN @it_a918
         WHERE knumh    EQ @it_a918-knumh
           AND loevm_ko EQ @space.
      ENDIF.
    ENDIF.

    IF i_matnr IS NOT INITIAL.
      "Tp.transp./ForncServ./Material/Zona part./Zona cheg.
      SELECT *
        FROM a919 INTO TABLE @DATA(it_a919)
       WHERE kappl  EQ @i_kappl
         AND kschl  EQ @i_kschl
         AND shtyp  EQ @i_shtyp
         AND tdlnr  EQ @i_tdlnr
         AND matnr  EQ @i_matnr
         AND lzonea EQ @e_lzonea
         AND lzonez EQ @e_lzonez
         AND kfrst  EQ @space
         AND datab  LE @i_data_referencia
         AND datbi  GE @i_data_referencia.

      IF it_a919[] IS NOT INITIAL.
        SELECT *
          FROM konp APPENDING TABLE @it_konp
           FOR ALL ENTRIES IN @it_a919
         WHERE knumh    EQ @it_a919-knumh
           AND loevm_ko EQ @space.
      ENDIF.
    ENDIF.

    "Tp.transp./ForncServ./Zona part./ Zona cheg./Domicilio(Mesorregião)
    SELECT *
     FROM a934 INTO TABLE @DATA(it_a934)
    WHERE kappl  EQ @i_kappl
      AND kschl  EQ @i_kschl
      AND shtyp  EQ @i_shtyp
      AND tdlnr  EQ @i_tdlnr
      AND lzonea EQ @e_lzonea
      AND lzonez EQ @e_lzonez
      AND pstlza EQ @i_id_cidade_base
      AND kfrst  EQ @space
      AND datab  LE @i_data_referencia
      AND datbi  GE @i_data_referencia.

    IF it_a934[] IS NOT INITIAL.
      SELECT *
        FROM konp APPENDING TABLE @it_konp
         FOR ALL ENTRIES IN @it_a934
       WHERE knumh    EQ @it_a934-knumh
         AND loevm_ko EQ @space.
    ENDIF.

    IF i_matnr IS NOT INITIAL.

      "Tp.transp./ForncServ./Zona part./ Zona cheg./Material/Domicilio(Mesorregião)
      SELECT *
       FROM a938 INTO TABLE @DATA(it_a938)
      WHERE kappl  EQ @i_kappl
        AND kschl  EQ @i_kschl
        AND shtyp  EQ @i_shtyp
        AND tdlnr  EQ @i_tdlnr
        AND lzonea EQ @e_lzonea
        AND lzonez EQ @e_lzonez
        AND matnr  EQ @i_matnr
        AND pstlza EQ @i_id_cidade_base
        AND kfrst  EQ @space
        AND datab  LE @i_data_referencia
        AND datbi  GE @i_data_referencia.

      IF it_a938[] IS NOT INITIAL.
        SELECT *
          FROM konp APPENDING TABLE @it_konp
           FOR ALL ENTRIES IN @it_a938
         WHERE knumh    EQ @it_a938-knumh
           AND loevm_ko EQ @space.
      ENDIF.

    ENDIF.

    IF i_vstel IS NOT INITIAL.

      "Tp.transp./ForncServ./Zona part./ Zona cheg./Local Exped./Domicilio(Mesorregião)
      SELECT *
       FROM a933 INTO TABLE @DATA(it_a933)
    WHERE kappl  EQ @i_kappl
      AND kschl  EQ @i_kschl
      AND shtyp  EQ @i_shtyp
      AND tdlnr  EQ @i_tdlnr
      AND lzonea EQ @e_lzonea
      AND lzonez EQ @e_lzonez
      AND vstel  EQ @i_vstel
      AND kfrst  EQ @space
      AND datab  LE @i_data_referencia
      AND datbi  GE @i_data_referencia.

      IF it_a933[] IS NOT INITIAL.
        SELECT *
          FROM konp APPENDING TABLE @it_konp
           FOR ALL ENTRIES IN @it_a933
         WHERE knumh    EQ @it_a933-knumh
           AND loevm_ko EQ @space.
      ENDIF.

    ENDIF.

*---CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
    IF i_viagem_id IS NOT INITIAL.

      l_add01 = lc_add01.

*-------Tp.transp./Contrato/Id.Viagem
      SELECT *
        FROM a942 INTO TABLE @DATA(it_a942)
       WHERE kappl     EQ @i_kappl
         AND kschl     EQ @i_kschl
         AND shtyp     EQ @i_shtyp
         "AND sdabw     EQ @l_add01  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
         AND id_viagem EQ @i_viagem_id
         AND kfrst     EQ @space
         AND datab     LE @i_data_referencia
         AND datbi     GE @i_data_referencia.

      IF it_a942[] IS NOT INITIAL.
        SELECT *
          FROM konp INTO TABLE @DATA(it_konp_942)
           FOR ALL ENTRIES IN @it_a942
         WHERE knumh    EQ @it_a942-knumh
           AND loevm_ko EQ @space.

        IF sy-subrc = 0.
          DELETE it_a900[] WHERE kappl IS NOT INITIAL.
          DELETE it_a910[] WHERE kappl IS NOT INITIAL.
          DELETE it_a911[] WHERE kappl IS NOT INITIAL.
          DELETE it_a915[] WHERE kappl IS NOT INITIAL.
          DELETE it_a918[] WHERE kappl IS NOT INITIAL.
          DELETE it_a919[] WHERE kappl IS NOT INITIAL.
          DELETE it_a934[] WHERE kappl IS NOT INITIAL.
          DELETE it_a933[] WHERE kappl IS NOT INITIAL.
          DELETE it_a938[] WHERE kappl IS NOT INITIAL.
          DELETE it_a939[] WHERE kappl IS NOT INITIAL.
          DELETE it_a940[] WHERE kappl IS NOT INITIAL.
          DELETE it_konp[] WHERE knumh IS NOT INITIAL.

          SELECT *
            FROM konp APPENDING TABLE @it_konp
             FOR ALL ENTRIES IN @it_a942
           WHERE knumh    EQ @it_a942-knumh
             AND loevm_ko EQ @space.
        ENDIF.
      ENDIF.
    ENDIF.
*---CS2019001158 - Jaime Tassoni - 16.11.2020 - fim

    SORT: it_a900 BY shtyp tdlnr route add01,
          it_a910 BY shtyp tdlnr lzonea lzonez,
          it_a911 BY shtyp tdlnr route,
          it_a915 BY shtyp tdlnr lzonea lzonez add01,
          it_a918 BY shtyp tdlnr matnr lzonea lzonez add01,
          it_a919 BY shtyp tdlnr matnr lzonea lzonez,
          it_a934 BY shtyp tdlnr lzonea lzonez pstlza,
          it_a933 BY shtyp tdlnr lzonea lzonez vstel,
          it_a938 BY shtyp tdlnr lzonea lzonez matnr pstlza,
          it_a939 BY shtyp tdlnr lzonea lzonez vegr5,
          it_a940 BY shtyp tdlnr route vegr5,
*-----CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
          it_a942 BY shtyp sdabw id_viagem,
*-----CS2019001158 - Jaime Tassoni - 16.11.2020 - fim
          it_konp BY knumh.

    IF e_route IS NOT INITIAL AND i_add01 IS NOT INITIAL.
      LOOP AT it_a900 INTO DATA(wa_a900) WHERE shtyp = i_shtyp AND tdlnr = i_tdlnr AND route = e_route AND add01 = i_add01.
        READ TABLE it_konp INTO DATA(wa_konp) WITH KEY knumh = wa_a900-knumh BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_kbetr = wa_konp-kbetr.
          e_konwa = wa_konp-konwa.
          e_krech = wa_konp-krech.
          ADD 1 TO v_cont_fre.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT it_a910 INTO DATA(wa_a910) WHERE shtyp  = i_shtyp AND tdlnr  = i_tdlnr AND lzonea = e_lzonea AND lzonez = e_lzonez.
      READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a910-knumh BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        e_kbetr = wa_konp-kbetr.
        e_konwa = wa_konp-konwa.
        e_krech = wa_konp-krech.
        ADD 1 TO v_cont_fre.
      ENDIF.
    ENDLOOP.

*-CS2020000700 - jtassoni - 21.09.2020 - inicio
    IF i_vegr5 IS NOT INITIAL.
      LOOP AT it_a939 INTO DATA(wa_a939) WHERE shtyp  = i_shtyp
                                           AND tdlnr  = i_tdlnr
                                           AND lzonea = e_lzonea
                                           AND lzonez = e_lzonez
                                           AND vegr5  = i_vegr5.
        READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a939-knumh BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_kbetr = wa_konp-kbetr.
          e_konwa = wa_konp-konwa.
          e_krech = wa_konp-krech.
          ADD 1 TO v_cont_fre.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF i_vegr5 IS NOT INITIAL.
      LOOP AT it_a940 INTO DATA(wa_a940) WHERE shtyp = i_shtyp
                                           AND tdlnr = i_tdlnr
                                           AND route = e_route
                                           AND vegr5 = i_vegr5.
        READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a940-knumh BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_kbetr = wa_konp-kbetr.
          e_konwa = wa_konp-konwa.
          e_krech = wa_konp-krech.
          ADD 1 TO v_cont_fre.
        ENDIF.
      ENDLOOP.
    ENDIF.
*-CS2020000700 - jtassoni - 21.09.2020 - fim

*-CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
    IF i_viagem_id IS NOT INITIAL.
      LOOP AT it_a942 INTO DATA(wa_a942) WHERE shtyp     = i_shtyp
                                           "AND sdabw     = l_add01 "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
                                           AND id_viagem = i_viagem_id.
        READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a942-knumh BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_kbetr = wa_konp-kbetr.
          e_konwa = wa_konp-konwa.
          e_krech = wa_konp-krech.
          ADD 1 TO v_cont_fre.
        ENDIF.
      ENDLOOP.
    ENDIF.
*-CS2019001158 - Jaime Tassoni - 16.11.2020 - fim

    IF e_route IS NOT INITIAL.
      LOOP AT it_a911 INTO DATA(wa_a911) WHERE shtyp = i_shtyp AND tdlnr = i_tdlnr AND route = e_route.
        READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a911-knumh BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_kbetr = wa_konp-kbetr.
          e_konwa = wa_konp-konwa.
          e_krech = wa_konp-krech.
          ADD 1 TO v_cont_fre.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF i_add01 IS NOT INITIAL.
      LOOP AT it_a915 INTO DATA(wa_a915) WHERE shtyp  = i_shtyp AND tdlnr  = i_tdlnr AND lzonea = e_lzonea AND lzonez = e_lzonez AND add01  = i_add01.
        READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a915-knumh BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_kbetr = wa_konp-kbetr.
          e_konwa = wa_konp-konwa.
          e_krech = wa_konp-krech.
          ADD 1 TO v_cont_fre.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF i_add01 IS NOT INITIAL AND i_matnr IS NOT INITIAL.
      LOOP AT it_a918 INTO DATA(wa_a918) WHERE shtyp  = i_shtyp AND tdlnr  = i_tdlnr AND matnr  = i_matnr AND lzonea = e_lzonea AND lzonez = e_lzonez AND add01  = i_add01.
        READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a918-knumh BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_kbetr = wa_konp-kbetr.
          e_konwa = wa_konp-konwa.
          e_krech = wa_konp-krech.
          ADD 1 TO v_cont_fre.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF i_matnr IS NOT INITIAL.
      LOOP AT it_a919 INTO DATA(wa_a919) WHERE shtyp  = i_shtyp AND tdlnr  = i_tdlnr AND matnr  = i_matnr AND lzonea = e_lzonea AND lzonez = e_lzonez.
        READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a919-knumh BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_kbetr = wa_konp-kbetr.
          e_konwa = wa_konp-konwa.
          e_krech = wa_konp-krech.
          ADD 1 TO v_cont_fre.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT it_a934 INTO DATA(wa_a934) WHERE shtyp  = i_shtyp AND tdlnr  = i_tdlnr AND lzonea = e_lzonea AND lzonez = e_lzonez AND pstlza = i_id_cidade_base.
      READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a934-knumh BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        e_kbetr = wa_konp-kbetr.
        e_konwa = wa_konp-konwa.
        e_krech = wa_konp-krech.
        ADD 1 TO v_cont_fre.
      ENDIF.
    ENDLOOP.

    IF i_matnr IS NOT INITIAL.
      LOOP AT it_a938 INTO DATA(wa_a938) WHERE shtyp  = i_shtyp AND tdlnr  = i_tdlnr AND lzonea = e_lzonea AND lzonez = e_lzonez AND pstlza = i_id_cidade_base AND matnr = i_matnr.
        READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a938-knumh BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_kbetr = wa_konp-kbetr.
          e_konwa = wa_konp-konwa.
          e_krech = wa_konp-krech.
          ADD 1 TO v_cont_fre.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF i_vstel IS NOT INITIAL.
      LOOP AT it_a933 INTO DATA(wa_a933) WHERE shtyp  = i_shtyp AND tdlnr  = i_tdlnr AND lzonea = e_lzonea AND lzonez = e_lzonez AND vstel = i_vstel.
        READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a933-knumh BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_kbetr = wa_konp-kbetr.
          e_konwa = wa_konp-konwa.
          e_krech = wa_konp-krech.
          ADD 1 TO v_cont_fre.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF e_krech = 'A'. "Percentual
      e_kbetr = e_kbetr / 10.
    ENDIF.

    IF i_ordem_venda IS NOT INITIAL AND i_placa_trator IS NOT INITIAL AND i_id_ordem IS NOT INITIAL.
      CALL FUNCTION 'ZLES_VALOR_FRETE_ORDEM_CAR'
        EXPORTING
          i_id_ordem      = i_id_ordem
          i_vbeln         = i_ordem_venda
          i_placa_cav     = i_placa_trator
          i_shtyp         = i_shtyp
        IMPORTING
          e_vlr_frete_neg = e_vlr_frete_neg.

      IF e_vlr_frete_neg IS NOT INITIAL.
        e_kbetr = e_vlr_frete_neg.
      ENDIF.
    ENDIF.

    IF e_kbetr IS INITIAL.
      zcl_calc_frete=>gera_erro_geral( i_texto = 'Não existe valor de frete cadastrado. Solicite à transportadora da sua região' ).
    ENDIF.

    IF v_cont_fre GT 1.
      zcl_calc_frete=>gera_erro_geral( i_texto = 'Existe mais de um valor de frete cadastrado. Solicite a regularização à transportadora da sua região' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_condicao_adicional.

    DATA: t_a920 TYPE TABLE OF a920,
          t_konp TYPE TABLE OF konp.

    FREE: e_kbetr, e_konwa, e_krech.

*------------------------------
*-- busca condicao A920
*------------------------------
    SELECT *
      FROM a920
      INTO TABLE t_a920
     WHERE kappl  = i_kappl
       AND kschl  = i_kschl
       AND tdlnr  = i_tdlnr
       AND shtyp  = i_shtyp
       AND tplst  = i_tplst
       AND kfrst  = space
       AND datab <= i_data_referencia
       AND datbi >= i_data_referencia.

    IF t_a920[] IS NOT INITIAL.
      SELECT *
        FROM konp
        INTO TABLE t_konp
         FOR ALL ENTRIES IN t_a920
       WHERE knumh    = t_a920-knumh
         AND loevm_ko = space.
    ENDIF.

*------------------------------
*-- valores condicao
*------------------------------
    LOOP AT t_a920 INTO DATA(w_a920).
      READ TABLE t_konp INTO DATA(w_konp) WITH KEY knumh = w_a920-knumh
                        BINARY SEARCH.
      IF sy-subrc = 0.
        e_kbetr = w_konp-kbetr.
        e_konwa = w_konp-konwa.
        e_krech = w_konp-krech.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
