CLASS ZCL_DELIVERY DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS CONSTRUCTOR .
    METHODS CRIAR
      IMPORTING
        !I_TIPO_DELIVERY   TYPE CHAR01 DEFAULT 'E'
        !I_CABECALHO       TYPE ZDE_BAPI_REMESSA_CAB
        !I_ITEMS           TYPE ZDE_BAPI_REMESSA_ITEM_T
        !I_PARCEIROS       TYPE ZDE_BAPI_REMESSA_PARCEIROS_T
        !I_GERAR_MOVIMENTO TYPE CHAR01 DEFAULT 'X'
        !I_PARTICAO_LOTE   TYPE CHAR01 DEFAULT ' '
      RETURNING
        VALUE(R_GEROU)     TYPE CHAR01
      RAISING
        ZCX_DELIVERY .
    METHODS GET_NR_REMESSA
      RETURNING
        VALUE(R_REMESSA) TYPE VBELN_VL .
    METHODS ELIMINAR
      RETURNING
        VALUE(R_GEROU) TYPE CHAR01
      RAISING
        ZCX_DELIVERY .
    METHODS SET_NR_REMESSA
      IMPORTING
        !I_REMESSA        TYPE VBELN_VL
      RETURNING
        VALUE(R_DELIVERY) TYPE REF TO ZCL_DELIVERY .
    METHODS GET_RETORNO
      RETURNING
        VALUE(R_RETORNO) TYPE BAPIRET2_T .
    METHODS GET_RETORNO_ELIMINAR
      RETURNING
        VALUE(R_RETORNO) TYPE BAPIRET2_T .
    METHODS PIKING
      RETURNING
        VALUE(R_GEROU) TYPE CHAR01
      RAISING
        ZCX_DELIVERY .
    METHODS ELIMINAR_MOVIMENTO_MERCADORIA
      RETURNING
        VALUE(R_GEROU) TYPE CHAR01 .
    CLASS-METHODS GET_DOC_TRANSPORTE_RODO
      IMPORTING
        !I_VBELN       TYPE VBELN_VL
      RETURNING
        VALUE(R_TKNUM) TYPE TKNUM
      RAISING
        ZCX_DELIVERY .
    METHODS SET_DATA_VENCIMENTO
      IMPORTING
        !I_VFDAT          TYPE VFDAT
      RETURNING
        VALUE(R_DELIVERY) TYPE REF TO ZCL_DELIVERY .
    CLASS-METHODS GERA_ERRO_GERAL
      IMPORTING
        !I_TEXTO TYPE STRING
      RAISING
        ZCX_DELIVERY .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA RETORNO TYPE BAPIRET2_T .
    DATA RETORNO_PIKING TYPE BAPIRET2_T .
    DATA RETORNO_ELIMINAR TYPE BAPIRET2_T .
    DATA CABECALHO TYPE ZDE_BAPI_REMESSA_CAB .
    DATA ITEMS TYPE ZDE_BAPI_REMESSA_ITEM_T .
    DATA PARCEIROS TYPE ZDE_BAPI_REMESSA_PARCEIROS_T .
    DATA PARTICAO_LOTE TYPE CHAR01 VALUE ' ' ##NO_TEXT.
    DATA GERAR_MOVIMENTO TYPE CHAR01 .
    DATA AV_VBELN TYPE VBELN_VL .
    DATA AT_VFDAT TYPE VFDAT .

    METHODS LIMPAR .
    METHODS SET_RETORNO
      IMPORTING
        !I_RETORNO TYPE BAPIRET2_T .
    METHODS SET_RETORNO_PIKING
      IMPORTING
        !I_RETORNO TYPE BAPIRET2_T .
    METHODS SET_RETORNO_ELIMINAR
      IMPORTING
        !I_RETORNO TYPE BAPIRET2_T .
    METHODS CRIAR_ENTRADA
      RETURNING
        VALUE(R_GEROU) TYPE CHAR01
      RAISING
        ZCX_DELIVERY .
    METHODS MONTA_SHDB
      IMPORTING
        !P_DYNBEGIN     TYPE ANY
        !P_NAME         TYPE ANY
        !P_VALUE        TYPE ANY
      CHANGING
        VALUE(E_TABELA) TYPE BDCDATA_TAB .
ENDCLASS.



CLASS ZCL_DELIVERY IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    ME->CABECALHO-CK_ROUTE_VALIDAR = ABAP_TRUE.

  ENDMETHOD.


  METHOD CRIAR.

    ME->LIMPAR( ).
    ME->CABECALHO       = I_CABECALHO.
    ME->ITEMS           = I_ITEMS.
    ME->PARCEIROS       = I_PARCEIROS.
    ME->GERAR_MOVIMENTO = I_GERAR_MOVIMENTO.
    ME->PARTICAO_LOTE   = I_PARTICAO_LOTE.

    CASE I_TIPO_DELIVERY.
      WHEN 'E'.
        R_GEROU = ME->CRIAR_ENTRADA( ).
      WHEN 'S'.

        "Implementar

    ENDCASE.

    IF R_GEROU EQ ABAP_TRUE.
      MESSAGE S000 WITH ME->AV_VBELN.
    ENDIF.

  ENDMETHOD.


  METHOD criar_entrada.

    CONSTANTS: lc_delivery TYPE vbeln VALUE '$        1'.

    DATA: lc_remessa        TYPE vbeln_vl,
          lc_retorno        TYPE TABLE OF bapiret2,
          wa_retorno        TYPE bapiret2,
          lc_sender_system  TYPE tbdls-logsys,
          wa_parceiros      TYPE zde_bapi_remessa_parceiros,
          lc_a_lzone        TYPE azone,
          lc_l_lzone        TYPE lzone,
          vl_vlr_nf(20)     TYPE c,
          vl_lfimg(20)      TYPE c,
          vl_kcmeng         TYPE komdlgn-kcmeng,
          lc_item_number    TYPE bapiibdlvitem-itm_number,
          items_particionar	TYPE zde_bapi_remessa_item_t.

    DATA: i_vbsk        TYPE vbsk,
          it_xkomdlgn   TYPE TABLE OF komdlgn,
          wa_xkomdlgn   TYPE komdlgn,
          it_xvbfs      TYPE TABLE OF vbfs,
          it_xvbls      TYPE TABLE OF vbls,
          it_gn_partner TYPE TABLE OF partner_gn,
          wa_gn_partner TYPE partner_gn.


    IF me->cabecalho-route IS INITIAL AND me->cabecalho-ck_route_validar EQ abap_true.

      CASE me->cabecalho-lc_coleta_partyp.
        WHEN 'C'.
          SELECT SINGLE lzone INTO lc_a_lzone
            FROM kna1
           WHERE kunnr EQ me->cabecalho-lc_coleta_parid.
        WHEN 'V' OR 'B'.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = me->cabecalho-lc_coleta_parid
            IMPORTING
              output = me->cabecalho-lc_coleta_parid.

          SELECT SINGLE lzone INTO lc_a_lzone
            FROM lfa1
           WHERE lifnr EQ me->cabecalho-lc_coleta_parid.
      ENDCASE.

      CASE me->cabecalho-lc_entrega_partyp.
        WHEN 'C'.

          SELECT SINGLE lzone INTO lc_l_lzone
            FROM kna1
           WHERE kunnr EQ me->cabecalho-lc_entrega_parid.

        WHEN 'V' OR 'B'.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = me->cabecalho-lc_entrega_parid
            IMPORTING
              output = me->cabecalho-lc_entrega_parid.

          SELECT SINGLE lzone INTO lc_l_lzone
            FROM lfa1
           WHERE lifnr EQ me->cabecalho-lc_entrega_parid.
      ENDCASE.

      SELECT SINGLE *
        INTO @DATA(wa_trolz)
        FROM trolz
       WHERE aland = 'BR'
         AND azone = @lc_a_lzone
         AND lland = 'BR'
         AND lzone = @lc_l_lzone.

      IF sy-subrc IS INITIAL.
        me->cabecalho-route = wa_trolz-route.
      ELSE.
        "ZCX_SEM_ITINERARIO	Não Enc.Itinerário p/ Coleta &MSGV1& (Zona: &MSGV2&) Entrega &MSGV3& (Zona: &MSGV4&)!
        RAISE EXCEPTION TYPE zcx_delivery
          EXPORTING
            textid = VALUE #( msgid = zcx_delivery=>zcx_sem_itinerario-msgid
                              msgno = zcx_delivery=>zcx_sem_itinerario-msgno
                              attr1 = CONV #( me->cabecalho-lc_coleta_parid )
                              attr2 = CONV #( lc_a_lzone )
                              attr3 = CONV #( me->cabecalho-lc_entrega_parid )
                              attr4 = CONV #( lc_l_lzone )  )
            msgty  = 'E'
            msgno  = zcx_delivery=>zcx_sem_itinerario-msgno
            msgv1  = CONV #( me->cabecalho-lc_coleta_parid )
            msgv2  = CONV #( lc_a_lzone )
            msgv3  = CONV #( me->cabecalho-lc_entrega_parid )
            msgv4  = CONV #( lc_l_lzone )
            msgid  = zcx_delivery=>zcx_sem_itinerario-msgid.
      ENDIF.
    ENDIF.

    "Fornecedor da Mercadoria
    IF me->cabecalho-lifnr IS NOT INITIAL.

      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.

      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'LF'
        IMPORTING
          output = wa_gn_partner-parvw.

      wa_gn_partner-lifnr = me->cabecalho-lifnr.
      APPEND wa_gn_partner TO it_gn_partner.

    ENDIF.

    "Local de Coleta
    IF me->cabecalho-lc_coleta_parid IS NOT INITIAL.

      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.
      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'PC'
        IMPORTING
          output = wa_gn_partner-parvw.
      wa_gn_partner-lifnr = me->cabecalho-lc_coleta_parid.
      APPEND wa_gn_partner TO it_gn_partner.

    ENDIF.

    "Local de Entrega
    IF me->cabecalho-lc_entrega_parid IS NOT INITIAL.

      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.
      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'LR'
        IMPORTING
          output = wa_gn_partner-parvw.
      wa_gn_partner-kunnr = me->cabecalho-lc_entrega_parid.
      APPEND wa_gn_partner TO it_gn_partner.

    ENDIF.

    "Agente de Frete
    IF me->cabecalho-sp_frete_parid IS NOT INITIAL.
      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.
      wa_gn_partner-parvw = 'SP'.
      wa_gn_partner-lifnr = me->cabecalho-sp_frete_parid.
      APPEND wa_gn_partner TO it_gn_partner.
    ENDIF.

    "Fornecedor Mercadoria
    IF me->cabecalho-wl_forn_merc_parid IS NOT INITIAL.
      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.
      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'WL'
        IMPORTING
          output = wa_gn_partner-parvw.
      wa_gn_partner-lifnr = me->cabecalho-wl_forn_merc_parid.
      APPEND wa_gn_partner TO it_gn_partner.

*-IR 207623-02.12.2024-#159511-JT-inicio
      "Fornecedor Mercadoria
    ELSEIF me->cabecalho-lc_coleta_parid IS NOT INITIAL.
      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.
      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'WL'
        IMPORTING
          output = wa_gn_partner-parvw.
      wa_gn_partner-lifnr = me->cabecalho-lc_coleta_parid.
      APPEND wa_gn_partner TO it_gn_partner.
    ENDIF.
*-IR 207623-02.12.2024-#159511-JT-fim

    lc_item_number = 10.

    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = lc_sender_system
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.

    "ITEMS_PARTICIONAR = ME->ITEMS.

    "    IF ME->PARTICAO_LOTE EQ ABAP_TRUE.

    LOOP AT me->items INTO DATA(wa_itm).
      IF wa_itm-item_do_lote IS INITIAL.
        APPEND wa_itm TO items_particionar.
      ELSE.
        READ TABLE items_particionar WITH KEY item_do_lote = wa_itm-item_do_lote TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          APPEND wa_itm TO items_particionar.
        ENDIF.
      ENDIF.
    ENDLOOP.

*      SORT ITEMS_PARTICIONAR BY ITEM_DO_LOTE.
*      DELETE ADJACENT DUPLICATES FROM ITEMS_PARTICIONAR COMPARING ITEM_DO_LOTE.

*      READ TABLE ITEMS_PARTICIONAR WITH KEY ITEM_DO_LOTE = SPACE TRANSPORTING NO FIELDS.
*      IF SY-SUBRC IS INITIAL.
*        RAISE EXCEPTION TYPE ZCX_DELIVERY
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_DELIVERY=>ZCX_ITEM_DO_LOTE-MSGID
*                              MSGNO = ZCX_DELIVERY=>ZCX_ITEM_DO_LOTE-MSGNO )
*            MSGTY  = 'E'
*            MSGNO  = ZCX_DELIVERY=>ZCX_ITEM_DO_LOTE-MSGNO
*            MSGID  = ZCX_DELIVERY=>ZCX_ITEM_DO_LOTE-MSGID.
*      ENDIF.
    "    ENDIF.

    LOOP AT items_particionar INTO DATA(wa_itens).

      CLEAR wa_xkomdlgn.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lc_item_number
        IMPORTING
          output = lc_item_number.

      wa_xkomdlgn-rfbel    = lc_delivery.
      "WA_XKOMDLGN-VSTEL    = WA_ITENS-PLANT.
      wa_xkomdlgn-lfart    = 'EL'.
      wa_xkomdlgn-dlvtp    = '7'.
      wa_xkomdlgn-matnr    = wa_itens-material.
      wa_xkomdlgn-werks    = wa_itens-plant.
      wa_xkomdlgn-lfdat    = sy-datlo.
      wa_xkomdlgn-lfuhr    = sy-timlo.
      wa_xkomdlgn-umvkz    = 1.
      wa_xkomdlgn-umvkn    = 1.
      wa_xkomdlgn-vrkme    = wa_itens-unidade.
      wa_xkomdlgn-meins    = wa_itens-unidade.
      wa_xkomdlgn-vgbel    = wa_itens-ebeln.
      wa_xkomdlgn-vgpos    = wa_itens-ebelp.
      wa_xkomdlgn-vgtyp    = wa_itens-vgtyp.
      wa_xkomdlgn-verursys = lc_sender_system.
      wa_xkomdlgn-kunag    = me->cabecalho-sp_frete_parid.
      wa_xkomdlgn-vfdat    = me->cabecalho-vfdat.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = me->cabecalho-sp_frete_parid
        IMPORTING
          output = me->cabecalho-sp_frete_parid.

      DATA(str_qtd) = strlen( me->cabecalho-sp_frete_parid ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->cabecalho-sp_frete_parid
        IMPORTING
          output = me->cabecalho-sp_frete_parid.

      IF str_qtd LE 4.

        DATA: lc_j_1bbranch TYPE j_1bbranch.
        MOVE me->cabecalho-sp_frete_parid TO lc_j_1bbranch-branch.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lc_j_1bbranch-branch
          IMPORTING
            output = lc_j_1bbranch-branch.

        SELECT SINGLE * INTO lc_j_1bbranch FROM j_1bbranch
          WHERE branch = lc_j_1bbranch-branch .

        IF sy-subrc IS INITIAL.
          wa_xkomdlgn-inco1   = 'CIF'.
          wa_xkomdlgn-inco2   = 'CIF'.
        ELSE.
          wa_xkomdlgn-inco1   = 'CPT'.
          wa_xkomdlgn-inco2   = 'CPT'.
        ENDIF.

      ELSE.
        wa_xkomdlgn-inco1   = 'CPT'.
        wa_xkomdlgn-inco2   = 'CPT'.
      ENDIF.

      wa_xkomdlgn-lgort    = wa_itens-stge_loc.
      wa_xkomdlgn-gewei    = wa_itens-unidade.
      wa_xkomdlgn-lifnr    = me->cabecalho-lifnr.
      wa_xkomdlgn-traty    = wa_itens-traty.
      wa_xkomdlgn-tragr    = wa_itens-tragr.
      wa_xkomdlgn-ladgr    = wa_itens-ladgr.
      wa_xkomdlgn-mfrgr    = wa_itens-mfrgr.

      vl_lfimg = me->cabecalho-vl_total_fatura.
      SHIFT vl_lfimg LEFT DELETING LEADING space.
      TRANSLATE vl_lfimg USING '.,'.

      wa_xkomdlgn-bolnr    = vl_lfimg.
      wa_xkomdlgn-kzbew    = wa_itens-kzbew.
      wa_xkomdlgn-noatp    = abap_true.
      wa_xkomdlgn-xchar    = abap_true.
      wa_xkomdlgn-xchpf    = abap_true.
      wa_xkomdlgn-lifex    = me->cabecalho-xblnr.
      wa_xkomdlgn-route    = me->cabecalho-route.

      "Verificar se Pedido de Compra é para Movimentar Mercadoria
      TRY .
          DATA(r_bstae) = zcl_pedido_compra=>get_chave_controle_conf_item( EXPORTING i_ebeln = wa_itens-ebeln i_ebelp = wa_itens-ebelp ).
          SELECT SINGLE * INTO @DATA(wa_t163g)
            FROM t163g
           WHERE bstae EQ @r_bstae
             AND blfdn EQ '1'
             AND ebtyp EQ 'LA'.

          IF ( sy-subrc IS INITIAL ) AND ( wa_t163g-werel EQ abap_true OR wa_t163g-wezuo EQ abap_true ).
            wa_xkomdlgn-nowab = abap_false.
          ELSEIF sy-subrc IS INITIAL.
            wa_xkomdlgn-nowab = abap_true.
          ENDIF.

        CATCH zcx_pedido_compra_exception.
      ENDTRY.

      SELECT SINGLE maktx INTO wa_xkomdlgn-arktx
        FROM makt
       WHERE spras EQ sy-langu
         AND matnr EQ wa_itens-material.

      IF ( me->particao_lote EQ abap_false ) OR ( wa_itens-item_do_lote IS INITIAL ).
        wa_xkomdlgn-rfpos    = lc_item_number.
        wa_xkomdlgn-charg    = wa_itens-batch.
        wa_xkomdlgn-lichn    = wa_itens-licha.
        wa_xkomdlgn-lfimg    = wa_itens-quantidade.
        wa_xkomdlgn-lgmng    = wa_itens-quantidade.
        wa_xkomdlgn-ntgew    = wa_itens-quantidade.
        wa_xkomdlgn-brgew    = wa_itens-quantidade.

        "ALRS buscar conversao un medida 09/12/2024
        TRANSLATE wa_itens-unidade TO UPPER CASE.
        IF  wa_itens-unidade = 'L' OR   wa_itens-unidade = 'BAG'.
          wa_xkomdlgn-kcgewei    = 'KG'.
        ENDIF.

        SELECT SINGLE *
          FROM marm
          INTO @DATA(wmarm2)
        WHERE matnr = @wa_itens-material
        AND   meinh = @wa_itens-unidade
        AND   gewei = @wa_xkomdlgn-kcgewei.

        SELECT SINGLE *
           INTO @DATA(wzib_nfe_dist_lot2)
           FROM zib_nfe_dist_lot
           WHERE chave_nfe = @me->cabecalho-chave_nfe
           AND   matnr     = @wa_itens-material
           AND   charg     = @wa_itens-batch.
        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM zib_nfe_dist_lca
            INTO @DATA(wzib_nfe_dist_lca2)
            WHERE cd_lote_item = @wzib_nfe_dist_lot2-cd_lote_item
            AND   atnam        = 'PESO_BAG'.
          IF sy-subrc = 0.
            REPLACE ',' IN wzib_nfe_dist_lca2-atwrt WITH  '.'.
            wzib_nfe_dist_lca2-atwrt = zcl_util=>get_string_numeric( CONV #( wzib_nfe_dist_lca2-atwrt ) ). "*-CS2025000249-07.05.2025-#174157-JT
            wmarm2-brgew = wzib_nfe_dist_lca2-atwrt.
          ENDIF.
          wa_xkomdlgn-ntgew    = wmarm2-brgew.
          wa_xkomdlgn-brgew    = wmarm2-brgew.
        ENDIF.
        "ALRS buscar conversao un medida 09/12/2024


        APPEND wa_xkomdlgn TO it_xkomdlgn.
      ELSE.
        wa_xkomdlgn-rfpos    = lc_item_number.
        wa_xkomdlgn-charg    = ''.
        wa_xkomdlgn-lichn    = ''.
        wa_xkomdlgn-lfimg    = 0.
        wa_xkomdlgn-lgmng    = 0.
        wa_xkomdlgn-ntgew    = 0.
        wa_xkomdlgn-brgew    = 0.
        wa_xkomdlgn-kzazu    = 'X'.
        wa_xkomdlgn-vsbed    = '01'.
        wa_xkomdlgn-traty    = ''.
        wa_xkomdlgn-noatp    = abap_false.
        wa_xkomdlgn-posar    = abap_off. "'B'.  "IR 207623-02.12.2024-#159511-JT

        wa_xkomdlgn-kcgewei    = wa_itens-unidade.
        TRANSLATE wa_itens-unidade TO UPPER CASE.
        IF  wa_itens-unidade = 'L' OR   wa_itens-unidade = 'BAG'.
          wa_xkomdlgn-kcgewei    = 'KG'.
        ENDIF.
        wa_xkomdlgn-ematn    = wa_itens-material.
*        WA_XKOMDLGN-KCGEWEI  = WA_ITENS-UNIDADE.
        APPEND wa_xkomdlgn TO it_xkomdlgn.

        READ TABLE it_xkomdlgn ASSIGNING FIELD-SYMBOL(<fs_item>) WITH KEY rfpos = lc_item_number.
        SELECT SINGLE *
          FROM marm
          INTO @DATA(wmarm)
        WHERE matnr = @wa_itens-material
        AND   meinh = @wa_itens-unidade
        AND   gewei = @wa_xkomdlgn-kcgewei.

        LOOP AT me->items INTO DATA(wa_lote) WHERE item_do_lote EQ wa_itens-item_do_lote.
          SELECT SINGLE *
            INTO @DATA(wzib_nfe_dist_lot)
            FROM zib_nfe_dist_lot
            WHERE chave_nfe = @me->cabecalho-chave_nfe
            AND   matnr     = @wa_itens-material
            AND   charg     = @wa_lote-batch.
          IF sy-subrc = 0.
            SELECT SINGLE *
              FROM zib_nfe_dist_lca
              INTO @DATA(wzib_nfe_dist_lca)
              WHERE cd_lote_item = @wzib_nfe_dist_lot-cd_lote_item
              AND   atnam        = 'PESO_BAG'.
            IF sy-subrc = 0.
              REPLACE ',' IN wzib_nfe_dist_lca-atwrt WITH  '.'.
              wzib_nfe_dist_lca-atwrt = zcl_util=>get_string_numeric( CONV #( wzib_nfe_dist_lca-atwrt ) ). "*-CS2025000249-07.05.2025-#174157-JT
              wmarm-brgew = wzib_nfe_dist_lca-atwrt.
            ENDIF.
          ENDIF.

          ADD wa_lote-quantidade TO <fs_item>-kcmeng.
          vl_kcmeng = wa_lote-quantidade.
          IF wmarm-brgew GT 0.
            vl_kcmeng = ( wa_lote-quantidade * wmarm-brgew ).
            ADD vl_kcmeng TO <fs_item>-kcbrgew.
            ADD vl_kcmeng TO <fs_item>-kcntgew.
            vl_kcmeng =  wmarm-brgew.
          ELSE.
            ADD wa_lote-quantidade TO <fs_item>-kcbrgew.
            ADD wa_lote-quantidade TO <fs_item>-kcntgew..
          ENDIF.

          wa_xkomdlgn-uecha = wa_xkomdlgn-rfpos.
          wa_xkomdlgn-charg = wa_lote-batch.
          wa_xkomdlgn-lichn = wa_lote-licha.
          wa_xkomdlgn-lfimg = wa_lote-quantidade.
          wa_xkomdlgn-lgmng = wa_lote-quantidade.
          IF wa_itens-unidade = 'BAG'.
            wa_xkomdlgn-gewei = 'KG'.
          ENDIF.
          wa_xkomdlgn-ntgew = vl_kcmeng.
          wa_xkomdlgn-brgew = vl_kcmeng.
          wa_xkomdlgn-noatp    = abap_false.
          APPEND wa_xkomdlgn TO it_xkomdlgn.
        ENDLOOP.

      ENDIF.

      ADD 10 TO lc_item_number.
    ENDLOOP.

    IF 1 = 1.

      CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
        IMPORTING
          own_logical_system             = lc_sender_system
        EXCEPTIONS
          own_logical_system_not_defined = 1
          OTHERS                         = 2.

      IF sy-subrc IS NOT INITIAL.

        RAISE EXCEPTION TYPE zcx_delivery
          EXPORTING
            textid = VALUE #( msgid = sy-msgid
                              msgno = sy-msgno
                              attr1 = CONV #( sy-msgv1 )
                              attr2 = CONV #( sy-msgv2 )
                              attr3 = CONV #( sy-msgv3 )
                              attr4 = CONV #( sy-msgv4 ) )
            msgty  = 'E'
            msgno  = sy-msgno
            msgid  = sy-msgid
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4.
      ENDIF.

      CALL FUNCTION 'GN_DELIVERY_CREATE'
        EXPORTING
          vbsk_i        = i_vbsk
        TABLES
          xkomdlgn      = it_xkomdlgn
          xvbfs         = it_xvbfs
          xvbls         = it_xvbls
          it_gn_partner = it_gn_partner.

      IF sy-subrc IS INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        DELETE it_xvbls WHERE vbeln_lif IS INITIAL.
        SORT it_xvbls BY vbeln_lif.
        DELETE ADJACENT DUPLICATES FROM it_xvbls COMPARING vbeln_lif.
        DESCRIBE TABLE it_xvbls LINES DATA(qtd_linhas).

        IF qtd_linhas GE 1.
          r_gerou = abap_true.
          READ TABLE it_xvbls INDEX 1 INTO DATA(wa_xvbls).
          me->set_nr_remessa( i_remessa = wa_xvbls-vbeln_lif ).
          COMMIT WORK.

          CALL FUNCTION 'UPDATE_XBLNR_IN_LIKP'
            EXPORTING
              i_vbeln           = wa_xvbls-vbeln_lif
              i_xblnr           = me->cabecalho-xblnr
            EXCEPTIONS
              document_blocked  = 1
              update_no_success = 2
              OTHERS            = 3.

          IF sy-subrc IS NOT INITIAL.
            WAIT UP TO 2 SECONDS.
            CALL FUNCTION 'UPDATE_XBLNR_IN_LIKP'
              EXPORTING
                i_vbeln           = wa_xvbls-vbeln_lif
                i_xblnr           = me->cabecalho-xblnr
              EXCEPTIONS
                document_blocked  = 1
                update_no_success = 2
                OTHERS            = 3.

            IF sy-subrc IS NOT INITIAL.
              WAIT UP TO 2 SECONDS.
              CALL FUNCTION 'UPDATE_XBLNR_IN_LIKP'
                EXPORTING
                  i_vbeln           = wa_xvbls-vbeln_lif
                  i_xblnr           = me->cabecalho-xblnr
                EXCEPTIONS
                  document_blocked  = 1
                  update_no_success = 2
                  OTHERS            = 3.
            ENDIF.
          ENDIF.

          IF me->cabecalho-ch_referencia IS NOT INITIAL.

            DO 3 TIMES.
              CALL FUNCTION 'ZUPDATE_CH_REF_IN_LIKP'
                EXPORTING
                  i_vbeln           = wa_xvbls-vbeln_lif
                  i_ch_ref          = me->cabecalho-ch_referencia
                EXCEPTIONS
                  document_blocked  = 1
                  update_no_success = 2
                  OTHERS            = 3.

              IF sy-subrc IS NOT INITIAL.
                WAIT UP TO 3 SECONDS.
              ELSE.
                COMMIT WORK.
                EXIT.
              ENDIF.
            ENDDO.

          ENDIF.

        ELSE.
          r_gerou = abap_false.
        ENDIF.
      ELSE.
        r_gerou = abap_false.
      ENDIF.

      LOOP AT it_xvbfs INTO DATA(wa_xvbfs).
        MESSAGE ID     wa_xvbfs-msgid
                TYPE   wa_xvbfs-msgty
                NUMBER wa_xvbfs-msgno
                WITH   wa_xvbfs-msgv1 wa_xvbfs-msgv2 wa_xvbfs-msgv3 wa_xvbfs-msgv4
                INTO   wa_retorno-message.

        wa_retorno-type        = sy-msgty.
        wa_retorno-id          = sy-msgid.
        wa_retorno-number      = sy-msgno.
        wa_retorno-message_v1  = sy-msgv1.
        wa_retorno-message_v2  = sy-msgv2.
        wa_retorno-message_v3  = sy-msgv3.
        wa_retorno-message_v4  = sy-msgv4.
        APPEND wa_retorno TO lc_retorno.
      ENDLOOP.
      me->set_retorno( i_retorno = lc_retorno[] ).


*      CALL FUNCTION 'BAPI_INB_DELIVERY_SAVEREPLICA'
*        EXPORTING
*          HEADER_DATA             = LC_HEADER_DATA
*          SENDER_SYSTEM           = LC_SENDER_SYSTEM
*          HEADER_ORG              = LC_HEADER_ORG
*        IMPORTING
*          DELIVERY                = LC_REMESSA
*        TABLES
*          HEADER_PARTNER          = LC_HEADER_PARTNER
*          HEADER_DEADLINES        = LC_HEADER_DEADLINES
*          ITEM_DATA               = LC_ITEM_DATA
*          ITEM_ORG                = LC_ITEM_ORG
*          ITEM_REF_PURCHASE_ORDER = LC_ITEM_REF_PURCHASE_ORDER
*          RETURN                  = LC_RETORNO.
*
*      ME->SET_RETORNO( I_RETORNO = LC_RETORNO[] ).

*      IF LC_REMESSA IS NOT INITIAL.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
**        IF ME->CABECALHO-XBLNR IS NOT INITIAL.
**          CALL FUNCTION 'UPDATE_XBLNR_IN_LIKP'
**            EXPORTING
**              I_VBELN = LC_REMESSA
**              I_XBLNR = ME->CABECALHO-XBLNR.
**        ENDIF.
*        R_GEROU = ABAP_TRUE.
*        ME->SET_NR_REMESSA( I_REMESSA = LC_REMESSA ).
*
*        IF ME->GERAR_MOVIMENTO EQ ABAP_TRUE.
*
*          R_GEROU = ME->PIKING( ).
*          IF R_GEROU EQ ABAP_FALSE.
*            ME->ELIMINAR( ).
*          ENDIF.
*
*        ENDIF.
*
*      ELSE.
*        R_GEROU = ABAP_FALSE.
*      ENDIF.
    ELSE.
      DATA: it_bdcdata TYPE bdcdata_tab,
            wa_bdcdata TYPE LINE OF bdcdata_tab,
            vl_data    TYPE dats,
            lc_total   TYPE lfimg,
            vl_mode    TYPE c,
            it_msg     TYPE TABLE OF bdcmsgcoll,
            wa_msg     TYPE bdcmsgcoll.

      LOOP AT me->items INTO DATA(wa_item).
        ADD wa_item-quantidade TO lc_total.
      ENDLOOP.
      vl_lfimg = lc_total.
      SHIFT vl_lfimg LEFT DELETING LEADING space.
      TRANSLATE vl_lfimg USING '.,'.

      CLEAR: it_bdcdata.

      CONCATENATE me->cabecalho-lfdat+6(2)   me->cabecalho-lfdat+4(2)   me->cabecalho-lfdat(4)  INTO vl_data.

      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = 'X' p_name = 'SAPMV50A' p_value = '4007' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'BDC_OKCODE' p_value = '/00' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'LV50C-BSTNR' p_value = me->cabecalho-ebeln CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'RV50A-LFDAT_LA' p_value = vl_data CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'RV50A-VERUR_LA' p_value = me->cabecalho-xblnr CHANGING e_tabela = it_bdcdata.

      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = 'X' p_name = 'SAPMV50A' p_value = '1000' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'BDC_OKCODE' p_value = '/00' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'LIKP-BLDAT' p_value = vl_data CHANGING e_tabela = it_bdcdata.
      "CALL METHOD ME->MONTA_SHDB EXPORTING P_DYNBEGIN = ' ' P_NAME = 'RV50A-LFDAT_LA' P_VALUE = VL_DATA CHANGING E_TABELA = IT_BDCDATA.
      "CALL METHOD ME->MONTA_SHDB EXPORTING P_DYNBEGIN = ' ' P_NAME = 'RV50A-WADAT_IST_LA' P_VALUE = VL_DATA CHANGING E_TABELA = IT_BDCDATA.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'LIPSD-G_LFIMG(01)' p_value = vl_lfimg CHANGING e_tabela = it_bdcdata.

      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = 'X' p_name = 'SAPMV50A' p_value = '1000' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'BDC_OKCODE' p_value = '=HDET_T' CHANGING e_tabela = it_bdcdata.

      "Informações da Cabeçalho """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = 'X' p_name = 'SAPMV50A' p_value = '2000' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'BDC_OKCODE' p_value = '=T\03' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = 'X' p_name = 'SAPMV50A' p_value = '2000' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'BDC_OKCODE' p_value = '/00' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'LIKP-ROUTE' p_value = me->cabecalho-route CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'LIKP-BOLNR' p_value = vl_vlr_nf CHANGING e_tabela = it_bdcdata.

      "Colocar parceiro """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = 'X' p_name = 'SAPMV50A' p_value = '2000' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'BDC_OKCODE' p_value = '=T\07' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = 'X' p_name = 'SAPMV50A' p_value = '2000' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'BDC_OKCODE' p_value = '/00' CHANGING e_tabela = it_bdcdata.

      "Fornecedor
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'GVS_TC_DATA-REC-PARTNER(01)' p_value = me->cabecalho-lifnr CHANGING e_tabela = it_bdcdata.

      "Local de Coleta
      IF me->cabecalho-lc_coleta_parid IS NOT INITIAL.
        CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'GVS_TC_DATA-REC-PARVW(02)' p_value = 'PC' CHANGING e_tabela = it_bdcdata.
        CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'GVS_TC_DATA-REC-PARTNER(02)' p_value = me->cabecalho-lc_coleta_parid CHANGING e_tabela = it_bdcdata.
      ENDIF.
      "Local de Entrega
      IF me->cabecalho-lc_entrega_parid IS NOT INITIAL.
        CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'GVS_TC_DATA-REC-PARVW(03)' p_value = 'LR' CHANGING e_tabela = it_bdcdata.
        CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'GVS_TC_DATA-REC-PARTNER(03)' p_value = me->cabecalho-lc_entrega_parid CHANGING e_tabela = it_bdcdata.
      ENDIF.
      "Agente de Frete
      IF me->cabecalho-sp_frete_parid IS NOT INITIAL.
        CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'GVS_TC_DATA-REC-PARVW(04)' p_value = 'SP' CHANGING e_tabela = it_bdcdata.
        CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'GVS_TC_DATA-REC-PARTNER(04)' p_value = me->cabecalho-sp_frete_parid CHANGING e_tabela = it_bdcdata.
      ENDIF.

      "Fornecedor Mercadoria
      IF me->cabecalho-wl_forn_merc_parid IS NOT INITIAL.
        CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'GVS_TC_DATA-REC-PARVW(05)' p_value = 'WL' CHANGING e_tabela = it_bdcdata.
        CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'GVS_TC_DATA-REC-PARTNER(05)' p_value = me->cabecalho-wl_forn_merc_parid CHANGING e_tabela = it_bdcdata.
      ENDIF.

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = 'X' p_name = 'SAPMV50A' p_value = '2000' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'BDC_OKCODE' p_value = '=BACK_T' CHANGING e_tabela = it_bdcdata.

      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = 'X' p_name = 'SAPMV50A' p_value = '1000' CHANGING e_tabela = it_bdcdata.
      CALL METHOD me->monta_shdb EXPORTING p_dynbegin = ' ' p_name = 'BDC_OKCODE' p_value = '=SICH_T' CHANGING e_tabela = it_bdcdata.

      vl_mode = 'N'.
      CALL TRANSACTION 'VL31N' USING it_bdcdata MODE vl_mode UPDATE 'S' MESSAGES INTO it_msg.

      READ TABLE it_msg WITH KEY msgtyp = 'E' INTO wa_msg.
      IF sy-subrc IS INITIAL.
      ELSE.
        READ TABLE it_msg WITH KEY msgtyp = 'S' msgnr = '311' INTO wa_msg.
        MOVE wa_msg-msgv2 TO me->av_vbeln.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = me->av_vbeln
          IMPORTING
            output = me->av_vbeln.

        SELECT SINGLE * INTO @DATA(wa_likp)
          FROM likp
         WHERE vbeln EQ @me->av_vbeln.

        IF sy-subrc IS INITIAL.

          IF me->cabecalho-xblnr IS NOT INITIAL.
            CALL FUNCTION 'UPDATE_XBLNR_IN_LIKP'
              EXPORTING
                i_vbeln = me->av_vbeln
                i_xblnr = me->cabecalho-xblnr.
          ENDIF.

          r_gerou = abap_true.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD eliminar.

    DATA: it_bdcdata TYPE bdcdata_tab,
          it_msg     TYPE TABLE OF bdcmsgcoll,
          vl_mode    TYPE c.

    DATA: lc_retorno        TYPE TABLE OF bapiret2.

    IF me->av_vbeln IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_zlest0108)
        FROM zlest0108
       WHERE vbeln EQ @me->av_vbeln.

      IF sy-subrc IS INITIAL.

        IF wa_zlest0108-doc_transp IS INITIAL.

          DELETE FROM zlest0108 WHERE vbeln = me->av_vbeln.
          DELETE FROM zlest0109 WHERE vbeln = me->av_vbeln.

        ELSE.

          RAISE EXCEPTION TYPE zcx_delivery
            EXPORTING
              textid = VALUE #( msgid = zcx_delivery=>zcx_doc_frete_entrada-msgid
                                msgno = zcx_delivery=>zcx_doc_frete_entrada-msgno )
              msgid  = zcx_delivery=>zcx_doc_frete_entrada-msgid
              msgno  = zcx_delivery=>zcx_doc_frete_entrada-msgno
              msgty  = 'E'.

        ENDIF.
      ENDIF.

    ENDIF.

    SELECT SINGLE vbtyp INTO @DATA(lc_vbtyp)
      FROM likp
     WHERE vbeln EQ @me->av_vbeln.

    IF sy-subrc IS NOT INITIAL.
      r_gerou = abap_true.
      EXIT.
    ENDIF.

    CASE lc_vbtyp.
      WHEN 'J'.

        DATA: lc_delivery_j       TYPE bapiobdlvhdrchg-deliv_numb,
              lc_header_data_j    TYPE bapiobdlvhdrchg,
              lc_header_control_j TYPE bapiobdlvhdrctrlchg.

        lc_delivery_j                  = me->av_vbeln.
        lc_header_data_j-deliv_numb    = lc_delivery_j.
        lc_header_control_j-deliv_numb = lc_delivery_j.
        lc_header_control_j-dlv_del    = abap_true.
        lc_header_control_j-no_lock    = abap_true.

        CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
          EXPORTING
            header_data    = lc_header_data_j
            header_control = lc_header_control_j
            delivery       = lc_delivery_j
          TABLES
            return         = lc_retorno.

        me->set_retorno_eliminar( i_retorno = lc_retorno[] ).

        READ TABLE lc_retorno WITH KEY type = 'E' TRANSPORTING NO FIELDS.
        IF sy-subrc IS INITIAL.
          r_gerou = abap_false.
        ELSE.
          r_gerou = abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.

      WHEN '7'.

        DATA: lc_delivery_7       TYPE bapiibdlvhdrchg-deliv_numb,
              lc_header_data_7    TYPE bapiibdlvhdrchg,
              lc_header_control_7	TYPE bapiibdlvhdrctrlchg.

        lc_delivery_7                  = me->av_vbeln.
        lc_header_data_7-deliv_numb    = lc_delivery_7.
        lc_header_control_7-deliv_numb = lc_delivery_7.
        lc_header_control_7-dlv_del    = abap_true.
        lc_header_control_7-no_lock    = abap_true.

        CALL FUNCTION 'BAPI_INB_DELIVERY_CHANGE'
          EXPORTING
            header_data    = lc_header_data_7
            header_control = lc_header_control_7
            delivery       = lc_delivery_7
          TABLES
            return         = lc_retorno.

        me->set_retorno_eliminar( i_retorno = lc_retorno[] ).

        READ TABLE lc_retorno WITH KEY type = 'E' TRANSPORTING NO FIELDS.
        IF sy-subrc IS INITIAL.
          r_gerou = abap_false.
        ELSE.
          r_gerou = abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.


    ENDCASE.


  ENDMETHOD.


  METHOD ELIMINAR_MOVIMENTO_MERCADORIA.

    DATA: IT_RETORNO TYPE TABLE OF BAPIRET2,
          WA_RETORNO TYPE BAPIRET2.

    DATA: NUMBER           TYPE TBTCJOB-JOBCOUNT,
          NAME             TYPE TBTCJOB-JOBNAME,
          PRINT_PARAMETERS TYPE PRI_PARAMS.

    R_GEROU = ABAP_FALSE.

    SELECT SINGLE * INTO @DATA(WA_LIKP)
      FROM LIKP
     WHERE VBELN EQ @ME->AV_VBELN.

    IF WA_LIKP-WADAT_IST IS INITIAL.
      R_GEROU = ABAP_TRUE.
      EXIT.
    ENDIF.

    CHECK ME->AV_VBELN IS NOT INITIAL.

    IF 1 EQ 2.
      SUBMIT ZMMR147 WITH FP_VBELN = ME->AV_VBELN WITH FP_BUDAT = SY-DATLO AND RETURN.
    ELSE.
      CONCATENATE 'JOB_REVERSE_PICKING' ME->AV_VBELN INTO NAME SEPARATED BY '_'.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          JOBNAME          = NAME
        IMPORTING
          JOBCOUNT         = NUMBER
        EXCEPTIONS
          CANT_CREATE_JOB  = 1
          INVALID_JOB_DATA = 2
          JOBNAME_MISSING  = 3
          OTHERS           = 4.

      IF SY-SUBRC IS INITIAL.
        SUBMIT ZMMR147  TO SAP-SPOOL SPOOL PARAMETERS PRINT_PARAMETERS WITHOUT SPOOL DYNPRO VIA JOB NAME NUMBER NUMBER
          WITH FP_VBELN = ME->AV_VBELN WITH FP_BUDAT = SY-DATLO
          USER SY-UNAME
           AND RETURN.

        IF SY-SUBRC IS INITIAL.
          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              JOBCOUNT             = NUMBER
              JOBNAME              = NAME
              STRTIMMED            = 'X'
            EXCEPTIONS
              CANT_START_IMMEDIATE = 1
              INVALID_STARTDATE    = 2
              JOBNAME_MISSING      = 3
              JOB_CLOSE_FAILED     = 4
              JOB_NOSTEPS          = 5
              JOB_NOTEX            = 6
              LOCK_FAILED          = 7
              OTHERS               = 8.

          IF SY-SUBRC IS NOT INITIAL.
            CALL FUNCTION 'BP_JOB_DELETE'
              EXPORTING
                JOBCOUNT                 = NUMBER
                JOBNAME                  = NAME
              EXCEPTIONS
                CANT_DELETE_EVENT_ENTRY  = 1
                CANT_DELETE_JOB          = 2
                CANT_DELETE_JOBLOG       = 3
                CANT_DELETE_STEPS        = 4
                CANT_DELETE_TIME_ENTRY   = 5
                CANT_DERELEASE_SUCCESSOR = 6
                CANT_ENQ_PREDECESSOR     = 7
                CANT_ENQ_SUCCESSOR       = 8
                CANT_ENQ_TBTCO_ENTRY     = 9
                CANT_UPDATE_PREDECESSOR  = 10
                CANT_UPDATE_SUCCESSOR    = 11
                COMMIT_FAILED            = 12
                JOBCOUNT_MISSING         = 13
                JOBNAME_MISSING          = 14
                JOB_DOES_NOT_EXIST       = 15
                JOB_IS_ALREADY_RUNNING   = 16
                NO_DELETE_AUTHORITY      = 17
                OTHERS                   = 18.
          ENDIF.
        ELSE.
          CALL FUNCTION 'BP_JOB_DELETE'
            EXPORTING
              JOBCOUNT                 = NUMBER
              JOBNAME                  = NAME
            EXCEPTIONS
              CANT_DELETE_EVENT_ENTRY  = 1
              CANT_DELETE_JOB          = 2
              CANT_DELETE_JOBLOG       = 3
              CANT_DELETE_STEPS        = 4
              CANT_DELETE_TIME_ENTRY   = 5
              CANT_DERELEASE_SUCCESSOR = 6
              CANT_ENQ_PREDECESSOR     = 7
              CANT_ENQ_SUCCESSOR       = 8
              CANT_ENQ_TBTCO_ENTRY     = 9
              CANT_UPDATE_PREDECESSOR  = 10
              CANT_UPDATE_SUCCESSOR    = 11
              COMMIT_FAILED            = 12
              JOBCOUNT_MISSING         = 13
              JOBNAME_MISSING          = 14
              JOB_DOES_NOT_EXIST       = 15
              JOB_IS_ALREADY_RUNNING   = 16
              NO_DELETE_AUTHORITY      = 17
              OTHERS                   = 18.
        ENDIF.
      ENDIF.

      "Aguardar execução do job
      ZCL_JOB=>GET_INSTANCE(
       )->SET_KEY_JOB( I_JOBNAME = NAME I_JOBCOUNT = NUMBER
       )->GET_WAIT_JOB_EXEC(
       )->GET_LOG_JOB( IMPORTING E_LOGS = DATA(E_LOGS) ).

    ENDIF.

    SELECT SINGLE * INTO @WA_LIKP
      FROM LIKP
     WHERE VBELN EQ @ME->AV_VBELN.

    IF WA_LIKP-WADAT_IST IS INITIAL.
      R_GEROU = ABAP_TRUE.
    ENDIF.

    LOOP AT E_LOGS INTO DATA(WA_MESG).
      CLEAR: WA_RETORNO.
      WA_RETORNO-ID         = WA_MESG-MSGID.
      WA_RETORNO-TYPE       = WA_MESG-MSGTYPE.
      WA_RETORNO-NUMBER     = WA_MESG-MSGNO.
      WA_RETORNO-MESSAGE_V1 = WA_MESG-MSGV1.
      WA_RETORNO-MESSAGE_V2 = WA_MESG-MSGV2.
      WA_RETORNO-MESSAGE_V3 = WA_MESG-MSGV3.
      WA_RETORNO-MESSAGE_V4 = WA_MESG-MSGV4.

      MESSAGE ID     WA_RETORNO-ID
              TYPE   WA_RETORNO-TYPE
              NUMBER WA_RETORNO-NUMBER
              WITH   WA_MESG-MSGV1 WA_MESG-MSGV2 WA_MESG-MSGV3 WA_MESG-MSGV4
              INTO   WA_RETORNO-MESSAGE.

      APPEND WA_RETORNO TO IT_RETORNO.
    ENDLOOP.

    ME->SET_RETORNO_ELIMINAR( I_RETORNO = IT_RETORNO[] ).

  ENDMETHOD.


  METHOD GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_DELIVERY
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_DELIVERY=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_DELIVERY=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_DELIVERY=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_DELIVERY=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD GET_DOC_TRANSPORTE_RODO.

    SELECT * INTO TABLE @DATA(IT_VTTP)
      FROM VTTP
     WHERE VBELN EQ @I_VBELN.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DELIVERY
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_DELIVERY=>ZCX_DOC_TRANSPORTE-MSGID
                            MSGNO  = ZCX_DELIVERY=>ZCX_DOC_TRANSPORTE-MSGNO )
          MSGID  = ZCX_DELIVERY=>ZCX_DOC_TRANSPORTE-MSGID
          MSGNO  = ZCX_DELIVERY=>ZCX_DOC_TRANSPORTE-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    SELECT * INTO TABLE @DATA(IT_VTTK)
      FROM VTTK
       FOR ALL ENTRIES IN @IT_VTTP
     WHERE TKNUM EQ @IT_VTTP-TKNUM.

    READ TABLE IT_VTTK INTO DATA(WA_VTTK) WITH KEY VSART = '01'.

    IF SY-SUBRC IS INITIAL.
      R_TKNUM = WA_VTTK-TKNUM.
    ELSE.
      RAISE EXCEPTION TYPE ZCX_DELIVERY
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_DELIVERY=>ZCX_DOC_TRANSPORTE_RODO-MSGID
                            MSGNO  = ZCX_DELIVERY=>ZCX_DOC_TRANSPORTE_RODO-MSGNO )
          MSGID  = ZCX_DELIVERY=>ZCX_DOC_TRANSPORTE_RODO-MSGID
          MSGNO  = ZCX_DELIVERY=>ZCX_DOC_TRANSPORTE_RODO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD GET_NR_REMESSA.
    R_REMESSA = ME->AV_VBELN.
  ENDMETHOD.


  METHOD GET_RETORNO.
    R_RETORNO = ME->RETORNO.
  ENDMETHOD.


  METHOD GET_RETORNO_ELIMINAR.
    R_RETORNO = ME->RETORNO_ELIMINAR.
  ENDMETHOD.


  METHOD LIMPAR.

    CLEAR: ME->AV_VBELN,
           ME->RETORNO,
           ME->RETORNO_PIKING,
           ME->RETORNO_ELIMINAR,
           ME->CABECALHO,
           ME->ITEMS,
           ME->PARCEIROS.

  ENDMETHOD.


  METHOD MONTA_SHDB.

    DATA: WA_BDCDATA TYPE BDCDATA.

    IF P_DYNBEGIN = 'X'.
      MOVE: P_NAME      TO WA_BDCDATA-PROGRAM,
            P_VALUE     TO WA_BDCDATA-DYNPRO,
            P_DYNBEGIN  TO WA_BDCDATA-DYNBEGIN.
      APPEND WA_BDCDATA TO E_TABELA.
    ELSE.
      MOVE: P_NAME      TO WA_BDCDATA-FNAM,
            P_VALUE     TO WA_BDCDATA-FVAL.
      APPEND WA_BDCDATA TO E_TABELA.
    ENDIF.

  ENDMETHOD.


  METHOD PIKING.

    DATA: LC_VBKOK_WA  TYPE VBKOK,
          IT_VBPOK_TAB TYPE TABLE OF VBPOK,
          WA_VBPOK_TAB TYPE VBPOK,
          IT_PROT      TYPE TABLE OF PROTT,
          LC_RETORNO   TYPE TABLE OF BAPIRET2,
          WA_RETORNO   TYPE BAPIRET2.

    SELECT SINGLE * INTO @DATA(WA_LIKP)
      FROM LIKP
     WHERE VBELN EQ @ME->AV_VBELN.

    IF WA_LIKP-WADAT_IST IS NOT INITIAL.
      R_GEROU = ABAP_TRUE.
      EXIT.
    ENDIF.

    LC_VBKOK_WA-VBELN_VL = ME->AV_VBELN.
    LC_VBKOK_WA-VBELN    = ME->AV_VBELN.
    LC_VBKOK_WA-WABUC    = ABAP_TRUE.
    LC_VBKOK_WA-KODAT    = SY-DATLO.

    SELECT * INTO TABLE @DATA(IT_LIPS)
      FROM LIPS
     WHERE VBELN EQ @ME->AV_VBELN.

    LOOP AT IT_LIPS INTO DATA(WA_LIPS).
      WA_VBPOK_TAB-VBELN_VL = WA_LIPS-VBELN.
      WA_VBPOK_TAB-POSNR_VL = WA_LIPS-POSNR.
      WA_VBPOK_TAB-VBELN    = WA_LIPS-VBELN.
      WA_VBPOK_TAB-POSNN    = WA_LIPS-POSNR.
      WA_VBPOK_TAB-VFDAT    = ME->AT_VFDAT.
      APPEND WA_VBPOK_TAB TO IT_VBPOK_TAB.
    ENDLOOP.

    CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
      EXPORTING
        VBKOK_WA  = LC_VBKOK_WA
        SYNCHRON  = ABAP_TRUE
      TABLES
        VBPOK_TAB = IT_VBPOK_TAB
        PROT      = IT_PROT.

    LOOP AT IT_PROT INTO DATA(WA_PROT).
      WA_RETORNO-ID         = WA_PROT-MSGID.
      WA_RETORNO-NUMBER     = WA_PROT-MSGNO.
      WA_RETORNO-TYPE       = WA_PROT-MSGTY.
      WA_RETORNO-MESSAGE_V1 = WA_PROT-MSGV1.
      WA_RETORNO-MESSAGE_V2 = WA_PROT-MSGV2.
      WA_RETORNO-MESSAGE_V3 = WA_PROT-MSGV3.
      WA_RETORNO-MESSAGE_V4 = WA_PROT-MSGV4.

      MESSAGE ID     WA_PROT-MSGID
              TYPE   WA_PROT-MSGTY
              NUMBER WA_PROT-MSGNO
              WITH   WA_PROT-MSGV1 WA_PROT-MSGV2 WA_PROT-MSGV3 WA_PROT-MSGV4
              INTO   WA_RETORNO-MESSAGE.

      APPEND WA_RETORNO TO LC_RETORNO.
    ENDLOOP.

    ME->SET_RETORNO_PIKING( I_RETORNO = LC_RETORNO[] ).

    READ TABLE LC_RETORNO WITH KEY TYPE = 'E' INTO WA_RETORNO.
    IF SY-SUBRC IS INITIAL.
      R_GEROU = ABAP_FALSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ZCL_DELIVERY=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( WA_RETORNO-MESSAGE ) ).
    ELSE.
      R_GEROU = ABAP_TRUE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.

  ENDMETHOD.


  METHOD SET_DATA_VENCIMENTO.

    R_DELIVERY = ME.

    ME->AT_VFDAT = I_VFDAT.

  ENDMETHOD.


  METHOD SET_NR_REMESSA.

    R_DELIVERY = ME.

    ME->AV_VBELN = I_REMESSA.

  ENDMETHOD.


  METHOD SET_RETORNO.
    ME->RETORNO = I_RETORNO.
  ENDMETHOD.


  METHOD SET_RETORNO_ELIMINAR.
    ME->RETORNO_ELIMINAR = I_RETORNO.
  ENDMETHOD.


  METHOD SET_RETORNO_PIKING.
    ME->RETORNO_PIKING = I_RETORNO.
  ENDMETHOD.
ENDCLASS.
