class ZCL_ITINERARIO definition
  public
  final
  create public .

public section.

  interfaces ZIF_ITINERARIO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ITINERARIO IMPLEMENTATION.


  METHOD ZIF_ITINERARIO~GET_DISTANCIA.

    IF_ITINERARIO = ME.

    E_DISTANCIA = ME->ZIF_ITINERARIO~AT_TVRO-DISTZ.

    "Distância
    CHECK ME->ZIF_ITINERARIO~AT_TVRO-DISTZ IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_ITINERARIO
      EXPORTING
        TEXTID    = VALUE #( MSGID = ZCX_ITINERARIO=>ZCX_ITINERARIO_SEM_DIST-MSGID
                             MSGNO = ZCX_ITINERARIO=>ZCX_ITINERARIO_SEM_DIST-MSGNO
                             ATTR1 = ME->ZIF_ITINERARIO~AT_TVRO-ROUTE )
        MSGID     = ZCX_ITINERARIO=>ZCX_ITINERARIO_SEM_DIST-MSGID
        MSGNO     = ZCX_ITINERARIO=>ZCX_ITINERARIO_SEM_DIST-MSGNO
        MSGTY     = 'E'
        MSGV1     = CONV #( ME->ZIF_ITINERARIO~AT_TVRO-ROUTE )
        TRANSACAO = '0VTC'.

  ENDMETHOD.


  METHOD ZIF_ITINERARIO~GET_INSTANCE.

    IF ZIF_ITINERARIO~AT_IF_ITINERARIO IS NOT BOUND.
      CREATE OBJECT ZIF_ITINERARIO~AT_IF_ITINERARIO TYPE ZCL_ITINERARIO.
    ENDIF.
    R_IF_ITINERARIO = ZIF_ITINERARIO~AT_IF_ITINERARIO.

  ENDMETHOD.


  METHOD zif_itinerario~get_itinerario_relevante.

    DATA: l_msgv1 TYPE char50,
          l_msgv2 TYPE char50.

    r_instance = me.

    CLEAR: e_tvro.

    SELECT SINGLE * INTO @DATA(wa_lfa1)
      FROM lfa1
     WHERE lifnr EQ @i_cod_loc_coleta.

    SELECT SINGLE * INTO @DATA(wa_kna1)
      FROM kna1
     WHERE kunnr EQ @i_cod_loc_entrega.

    SELECT SINGLE *
      INTO @DATA(wa_trolz)
      FROM trolz
     WHERE aland EQ 'BR'
       AND azone EQ @wa_lfa1-lzone
       AND lland EQ 'BR'
       AND lzone EQ @wa_kna1-lzone.

    SELECT SINGLE * INTO @e_tvro
      FROM tvro
     WHERE route EQ @wa_trolz-route.

    IF ( e_tvro-tdiix IS INITIAL ) OR ( e_tvro-route IS INITIAL ).

      l_msgv1 = 'Itinerário não foi localizado: '.
      CONCATENATE i_cod_loc_coleta '/' i_cod_loc_entrega INTO l_msgv2.

      RAISE EXCEPTION TYPE zcx_itinerario
        EXPORTING
          textid = VALUE #( msgid = zcx_itinerario=>zcx_itinerario_irrelevante-msgid
                            msgno = zcx_itinerario=>zcx_itinerario_irrelevante-msgno
                            attr1 = CONV #( i_cod_loc_coleta )
                            attr2 = CONV #( i_cod_loc_entrega ) )
          msgid  = zcx_itinerario=>zcx_itinerario_irrelevante-msgid
          msgno  = zcx_itinerario=>zcx_itinerario_irrelevante-msgno
          msgty  = 'E'
          msgv1  = l_msgv1
          msgv2  = l_msgv2.
*         msgv1  = CONV #( i_cod_loc_coleta )
*         msgv2  = CONV #( i_cod_loc_entrega ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ITINERARIO~GET_ITINERARIO_ZONAS.

    R_INSTANCE = ME.

    SELECT SINGLE *
      INTO @DATA(WA_TROLZ)
      FROM TROLZ
     WHERE ALAND EQ 'BR'
       AND AZONE EQ @I_AZONE
       AND LLAND EQ 'BR'
       AND LZONE EQ @I_LZONE.

    SELECT SINGLE * INTO @E_TVRO
      FROM TVRO
     WHERE ROUTE EQ @WA_TROLZ-ROUTE.

    IF ( E_TVRO-TDIIX IS INITIAL ) OR ( E_TVRO-ROUTE IS INITIAL ).
      RAISE EXCEPTION TYPE ZCX_ITINERARIO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_ITINERARIO=>ZCX_ITINERARIO_IRRELEVANTE-MSGID
                            MSGNO = ZCX_ITINERARIO=>ZCX_ITINERARIO_IRRELEVANTE-MSGNO
                            ATTR1 = CONV #( I_AZONE )
                            ATTR2 = CONV #( I_LZONE ) )
          MSGID  = ZCX_ITINERARIO=>ZCX_ITINERARIO_IRRELEVANTE-MSGID
          MSGNO  = ZCX_ITINERARIO=>ZCX_ITINERARIO_IRRELEVANTE-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_AZONE )
          MSGV2  = CONV #( I_LZONE ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ITINERARIO~SET_ITINERARIO.

    IF_ITINERARIO = ME.

    SELECT SINGLE * INTO @ME->ZIF_ITINERARIO~AT_TVRO
      FROM TVRO
     WHERE ROUTE EQ @I_ROUTE.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_ITINERARIO
      EXPORTING
        TEXTID    = VALUE #( MSGID = ZCX_ITINERARIO=>ZCX_ITINERARIO_NOT_FOUND-MSGID
                             MSGNO = ZCX_ITINERARIO=>ZCX_ITINERARIO_NOT_FOUND-MSGNO
                             ATTR1 = I_ROUTE )
        MSGID     = ZCX_ITINERARIO=>ZCX_ITINERARIO_NOT_FOUND-MSGID
        MSGNO     = ZCX_ITINERARIO=>ZCX_ITINERARIO_NOT_FOUND-MSGNO
        MSGTY     = 'E'
        MSGV1     = CONV #( I_ROUTE )
        TRANSACAO = '0VTC'.

  ENDMETHOD.
ENDCLASS.
