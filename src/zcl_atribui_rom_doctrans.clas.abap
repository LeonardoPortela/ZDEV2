class ZCL_ATRIBUI_ROM_DOCTRANS definition
  public
  final
  create public .

public section.

  class-methods SET_INFO_ROM_DOC_TRANS
    importing
      !I_ROM type ZDE_LES_ZSDT0001
    changing
      !I_DOCTRANS type BAPISHIPMENTHEADER .
  class-methods GET_CK_OV_OC
    importing
      !I_NR_ORDEM_VENDA type VBELN_VA
      !I_ID_ORDEM type ZDE_ID_ORDEM
    raising
      ZCX_ORDEM_VENDA .
  class-methods SET_ID_ORDEM_ROMANEIO
    importing
      !I_ID_ORDEM type ZDE_ID_ORDEM
    changing
      !I_ZSDT0001 type ZSDT0001
    returning
      value(R_CK_ALTEROU) type CHAR01 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ATRIBUI_ROM_DOCTRANS IMPLEMENTATION.


  METHOD GET_CK_OV_OC.

    "Pesquisar Ordem de Venda no Carguero
    SELECT SINGLE * INTO @DATA(WA_ZLEST0181)
      FROM ZLEST0181
     WHERE EBELN EQ @I_NR_ORDEM_VENDA.

    CHECK SY-SUBRC IS INITIAL.

    IF I_ID_ORDEM IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_ORDEM_VENDA=>ZCX_OBRIG_ORDEM_CARREGA-MSGID
                            MSGNO = ZCX_ORDEM_VENDA=>ZCX_OBRIG_ORDEM_CARREGA-MSGNO  )
          MSGID  = ZCX_ORDEM_VENDA=>ZCX_OBRIG_ORDEM_CARREGA-MSGID
          MSGNO  = ZCX_ORDEM_VENDA=>ZCX_OBRIG_ORDEM_CARREGA-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0185)
      FROM ZLEST0185
     WHERE ID_ORDEM EQ @I_ID_ORDEM.

    IF SY-SUBRC IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(WA_ZSDT0001OD)
        FROM ZSDT0001OD
       WHERE ID_ORDEM  EQ @I_ID_ORDEM.

      RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_ORDEM_VENDA=>ZCX_ORD_VENDA_ORD_CARREGA-MSGID
                            MSGNO = ZCX_ORDEM_VENDA=>ZCX_ORD_VENDA_ORD_CARREGA-MSGNO
                            ATTR1 = WA_ZSDT0001OD-NR_ORDEM
                            ATTR2 = I_NR_ORDEM_VENDA  )
          MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORD_VENDA_ORD_CARREGA-MSGID
          MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORD_VENDA_ORD_CARREGA-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( WA_ZSDT0001OD-NR_ORDEM )
          MSGV2  = CONV #( I_NR_ORDEM_VENDA ).
    ENDIF.

    IF WA_ZLEST0185-EBELN NE I_NR_ORDEM_VENDA.

      SELECT SINGLE * INTO @WA_ZSDT0001OD
        FROM ZSDT0001OD
       WHERE ID_ORDEM  EQ @I_ID_ORDEM.

      RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_ORDEM_VENDA=>ZCX_ORD_VENDA_ORD_CARREGA-MSGID
                            MSGNO = ZCX_ORDEM_VENDA=>ZCX_ORD_VENDA_ORD_CARREGA-MSGNO
                            ATTR1 = WA_ZSDT0001OD-NR_ORDEM
                            ATTR2 = I_NR_ORDEM_VENDA  )
          MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORD_VENDA_ORD_CARREGA-MSGID
          MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORD_VENDA_ORD_CARREGA-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( WA_ZSDT0001OD-NR_ORDEM )
          MSGV2  = CONV #( I_NR_ORDEM_VENDA ).

    ENDIF.

  ENDMETHOD.


  METHOD SET_ID_ORDEM_ROMANEIO.

    IF I_ZSDT0001-ID_ORDEM NE I_ID_ORDEM.
      R_CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    I_ZSDT0001-ID_ORDEM = I_ID_ORDEM.

  ENDMETHOD.


  METHOD set_info_rom_doc_trans.

    i_doctrans-zid_carga               = i_rom-id_carga.
    i_doctrans-zid_romaneio            = i_rom-ch_referencia.
    i_doctrans-zid_ordem               = i_rom-id_ordem.

    IF i_rom-id_ordem IS NOT INITIAL.
      SELECT SINGLE viagem_id INTO i_doctrans-zid_viagem
        FROM zlest0185
       WHERE id_ordem EQ i_rom-id_ordem.
    ENDIF.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    IF i_rom-id_interface = '48'. "Sementes
      SELECT SINGLE viagem_id
       INTO i_doctrans-zid_viagem
       FROM zsdt0133
      WHERE nro_cg EQ I_ROM-nro_cg.
    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

  ENDMETHOD.
ENDCLASS.
